/*
 * Demonstration code for alternative RDD IO API which uses own
 * very simple TCP/IP file server with RPC support
 * All files which names starts 'net:' are redirected to this API.
 * This is server code giving the following .prg functions:
 *    netio_Listen( [<nPort>], [<cAddress>], [<cRootDir>], [<lRPC>] )
 *                                           --> <pListenSocket> | NIL
 *    netio_Accept( <pListenSocket>, [<nTimeOut>],
 *                  [<cPass>], [<nCompressionLevel>], [<nStrategy>] )
 *                                           --> <pConnectionSocket> | NIL
 *    netio_Compress( <pConnectionSocket>,
 *                    [<cPass>], [<nCompressionLevel>], [<nStrategy>] )
 *                                           --> NIL
 *    netio_Server( <pConnectionSocket> ) --> NIL
 *    netio_ServerStop( <pListenSocket> | <pConnectionSocket> [, <lStop>] )
 *                                           --> NIL
 *    netio_ServerTimeOut( <pConnectionSocket> [, <nTimeOut>] ) --> [<nTimeOut>]
 *    netio_RPC(<pListenSocket> | <pConnectionSocket> [, <lEnable>])
 *                                           --> <lPrev>
 *    netio_RPCFilter( <pConnectionSocket>,
 *                     <sFuncSym> | <hValue> | NIL ) --> NIL
 *
 *    netio_SrvStatus( <pConnectionSocket> [, <nStreamID>] ) --> <nStatus>
 *    netio_SrvSendItem(<pConnectionSocket>, <nStreamID>, <xData>)
 *          --> <lSent>
 *    netio_SrvSendData(<pConnectionSocket>, <nStreamID>, <cData>)
 *          --> <lSent>
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

// $HB_BEGIN_LICENSE$
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file LICENSE.txt.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
//
// As a special exception, the Harbour Project gives permission for
// additional uses of the text contained in its release of Harbour.
//
// The exception is that, if you link the Harbour libraries with other
// files to produce an executable, this does not by itself cause the
// resulting executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of
// linking the Harbour library code into it.
//
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.
//
// This exception applies only to the code released by the Harbour
// Project under the name Harbour.  If you copy code from other
// Harbour Project or Free Software Foundation releases into a copy of
// Harbour, as the General Public License permits, the exception does
// not apply to the code that you add in this way.  To avoid misleading
// anyone as to the status of such modified files, you must delete
// this exception notice from them.
//
// If you write modifications of your own for Harbour, it is your choice
// whether to permit this exception to apply to your modifications.
// If you do not wish that, delete this exception notice.
// $HB_END_LICENSE$

#include <hbapi.hpp>
#include <hbapiitm.hpp>
#include <hbapifs.hpp>
#include <hbapierr.hpp>
#include "hbsocket.hpp"
#include "hbznet.h"
#include "hbzlib.ch"
#include <hbinit.hpp>
#include <hbvm.hpp>
#include <hbstack.hpp>
#include <hbthread.hpp>
#include <hbdate.hpp>
#include "netio.hpp"
#include "hbserial.ch"

/*
 * server code
 */

struct _HB_CONSTREAM
{
   int id;
   int type;
   struct _HB_CONSTREAM * next;
};

using HB_CONSTREAM = _HB_CONSTREAM;
using PHB_CONSTREAM = _HB_CONSTREAM *;

struct _HB_CONSRV
{
   PHB_SOCKEX     sock;
   PHB_FILE       fileTable[NETIO_FILES_MAX];
   int            filesCount;
   int            firstFree;
   int            timeout;
   HB_BOOL        stop;
   HB_BOOL        rpc;
   HB_BOOL        login;
   PHB_SYMB       rpcFunc;
   PHB_ITEM       rpcFilter;
   PHB_ITEM       mutex;
   PHB_CONSTREAM  streams;
   HB_MAXUINT     wr_count;
   HB_MAXUINT     rd_count;
   int            rootPathLen;
   char           rootPath[HB_PATH_MAX];
};

using HB_CONSRV = _HB_CONSRV;
using PHB_CONSRV = _HB_CONSRV *;

struct _HB_LISTENSD
{
   HB_SOCKET sd;
   HB_BOOL   stop;
   HB_BOOL   rpc;
   char      rootPath[HB_PATH_MAX];
};

using HB_LISTENSD = _HB_LISTENSD;
using PHB_LISTENSD = _HB_LISTENSD *;

static HB_BOOL s_isDirSep(char c)
{
   /* intentionally used explicit values instead of Harbour macros
    * because client can use different OS
    */
   return c == '/' || c == '\\';
}

static const char * s_consrvFilePath(char * pszFileName, PHB_CONSRV conn, HB_BOOL fLink)
{
   int iPos = 0, iLevel = 0;
   char ch = HB_OS_PATH_DELIM_CHR;

   if( conn->rootPathLen )
   {
      while( s_isDirSep(pszFileName[0]) )
      {
         ++pszFileName;
      }
   }

   while( pszFileName[iPos] && iLevel >= 0 )
   {
      if( conn->rootPathLen && s_isDirSep(ch) )
      {
         if( pszFileName[iPos] == '.' && pszFileName[iPos + 1] == '.' && (pszFileName[iPos + 2] == '\0' || s_isDirSep(pszFileName[iPos + 2])) )
         {
            --iLevel;
         }
         else if( pszFileName[iPos] && !s_isDirSep(pszFileName[iPos]) && !(pszFileName[iPos] == '.' && s_isDirSep(pszFileName[iPos + 1])) )
         {
            ++iLevel;
         }
      }
      ch = pszFileName[iPos];
      if( s_isDirSep(ch) )
      {
         pszFileName[iPos] = HB_OS_PATH_DELIM_CHR;
      }
      ++iPos;
   }

   if( iLevel < 0 )
   {
      pszFileName = nullptr;
   }
   else if( conn->rootPathLen && !fLink )
   {
      memmove(pszFileName + conn->rootPathLen, pszFileName, iPos + 1);
      memcpy(pszFileName, conn->rootPath, conn->rootPathLen);
   }

   return pszFileName;
}

static HB_ERRCODE s_srvFsError(void)
{
   HB_ERRCODE errCode = hb_fsError();

   if( errCode == 0 )
   {
      errCode = NETIO_ERR_FILE_IO;
   }
   return errCode;
}

static int s_srvFileNew(PHB_CONSRV conn, PHB_FILE pFile)
{
   if( conn->filesCount < NETIO_FILES_MAX )
   {
      while( conn->firstFree < NETIO_FILES_MAX )
      {
         if( conn->fileTable[conn->firstFree] == nullptr )
         {
            conn->filesCount++;
            conn->fileTable[conn->firstFree] = pFile;
            return conn->firstFree++;
         }
         conn->firstFree++;
      }
   }
   return -1;
}

static PHB_FILE s_srvFileFree(PHB_CONSRV conn, int iFile)
{
   if( iFile < NETIO_FILES_MAX && conn->filesCount > 0 )
   {
      PHB_FILE pFile = conn->fileTable[iFile];
      if( pFile )
      {
         conn->fileTable[iFile] = nullptr;
         conn->filesCount--;
         if( conn->firstFree > iFile )
         {
            conn->firstFree = iFile;
         }
         return pFile;
      }
   }

   return nullptr;
}

static PHB_FILE s_srvFileGet(PHB_CONSRV conn, int iFile)
{
   if( iFile < NETIO_FILES_MAX && conn->filesCount > 0 )
   {
      return conn->fileTable[iFile];
   }
   else
   {
      return nullptr;
   }
}

static void s_consrv_disconnect(PHB_CONSRV conn)
{
   if( conn->sock )
   {
      hb_sockexClose(conn->sock, true);
      conn->sock = nullptr;
   }
}

static void s_consrv_close(PHB_CONSRV conn)
{
   int i = 0;

   if( conn->rpcFilter )
   {
      hb_itemRelease(conn->rpcFilter);
   }

   while( conn->streams )
   {
      PHB_CONSTREAM stream = conn->streams;
      conn->streams = stream->next;
      hb_xfree(stream);
   }

   if( conn->mutex )
   {
      hb_itemRelease(conn->mutex);
   }

   if( conn->sock )
   {
      hb_sockexClose(conn->sock, true);
   }

   while( conn->filesCount > 0 )
   {
      if( i >= NETIO_FILES_MAX )
      {
         break;   /* internal error, it should not happen */
      }

      if( conn->fileTable[i] )
      {
         hb_fileClose(conn->fileTable[i]);
         conn->filesCount--;
      }
      ++i;
   }

   hb_xfree(conn);
}

static HB_GARBAGE_FUNC(s_consrv_destructor)
{
   auto conn_ptr = static_cast<PHB_CONSRV*>(Cargo);

   if( *conn_ptr )
   {
      PHB_CONSRV conn = *conn_ptr;
      *conn_ptr = nullptr;
      s_consrv_close(conn);
   }
}

static HB_GARBAGE_FUNC(s_consrv_mark)
{
   auto conn_ptr = static_cast<PHB_CONSRV*>(Cargo);

   if( *conn_ptr && (*conn_ptr)->rpcFilter )
   {
      hb_gcMark((*conn_ptr)->rpcFilter);
   }
}

static const HB_GC_FUNCS s_gcConSrvFuncs =
{
   s_consrv_destructor,
   s_consrv_mark
};

static PHB_CONSRV s_consrvParam(int iParam)
{
   auto conn_ptr = static_cast<PHB_CONSRV*>(hb_parptrGC(&s_gcConSrvFuncs, iParam));

   if( conn_ptr && *conn_ptr )
   {
      return *conn_ptr;
   }

   hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   return nullptr;
}

static void s_consrvRet(PHB_CONSRV conn)
{
   if( conn )
   {
      auto conn_ptr = static_cast<PHB_CONSRV*>(hb_gcAllocate(sizeof(PHB_CONSRV), &s_gcConSrvFuncs));
      *conn_ptr = conn;
      hb_retptrGC(conn_ptr);
   }
   else
   {
      hb_ret();
   }   
}

static PHB_CONSRV s_consrvNew(PHB_SOCKEX sock, const char * szRootPath, HB_BOOL rpc)
{
   auto conn = static_cast<PHB_CONSRV>(memset(hb_xgrab(sizeof(HB_CONSRV)), 0, sizeof(HB_CONSRV)));

   conn->sock = sock;
   conn->rpc = rpc;
   conn->timeout = -1;
   if( szRootPath )
   {
      hb_strncpy(conn->rootPath, szRootPath, sizeof(conn->rootPath) - 1);
      conn->rootPathLen = static_cast<int>(strlen(conn->rootPath));
   }

   return conn;
}

static HB_BOOL s_srvRecvAll(PHB_CONSRV conn, void * buffer, long len)
{
   auto ptr = static_cast<HB_BYTE*>(buffer);
   long lRead = 0, l;
   HB_MAXINT timeout = conn->timeout;
   HB_MAXUINT timer = hb_timerInit(timeout);

   while( lRead < len && !conn->stop )
   {
      l = hb_sockexRead(conn->sock, ptr + lRead, len - lRead, 1000);
      if( l > 0 )
      {
         lRead += l;
         conn->rd_count += l;
      }
      else if( l == 0 || hb_socketGetError() != HB_SOCKET_ERR_TIMEOUT || (timeout = hb_timerTest(timeout, &timer)) == 0 || hb_vmRequestQuery() != 0 )
      {
         break;
      }
   }

   return lRead == len;
}

static HB_BOOL s_srvSendAll(PHB_CONSRV conn, void * buffer, long len)
{
   auto ptr = static_cast<HB_BYTE*>(buffer);
   long lSent = 0;

   if( !conn->mutex || hb_threadMutexLock(conn->mutex) )
   {
      HB_MAXINT timeout = conn->timeout;
      HB_MAXUINT timer = hb_timerInit(timeout);

      while( lSent < len && !conn->stop )
      {
         long l = hb_sockexWrite(conn->sock, ptr + lSent, len - lSent, 1000);
         if( l > 0 )
         {
            lSent += l;
            conn->wr_count += l;
         }
         else if( hb_socketGetError() != HB_SOCKET_ERR_TIMEOUT || (timeout = hb_timerTest(timeout, &timer)) == 0 || hb_vmRequestQuery() != 0 )
         {
            break;
         }
      }
      if( lSent == len && !conn->stop )
      {
         if( hb_sockexFlush(conn->sock, conn->timeout > 0 ? conn->timeout : -1, HB_FALSE) != 0 )
         {
            lSent = -1;
         }
      }

      if( conn->mutex )
      {
         hb_threadMutexUnlock(conn->mutex);
      }
   }
   return lSent == len;
}

static HB_GARBAGE_FUNC(s_listensd_destructor)
{
   auto lsd_ptr = static_cast<PHB_LISTENSD*>(Cargo);

   if( *lsd_ptr )
   {
      PHB_LISTENSD lsd = *lsd_ptr;
      *lsd_ptr = nullptr;
      if( lsd->sd != HB_NO_SOCKET )
      {
         hb_socketClose(lsd->sd);
         lsd->sd = HB_NO_SOCKET;
      }
      hb_xfree(lsd);
   }
}

static const HB_GC_FUNCS s_gcListensdFuncs =
{
   s_listensd_destructor,
   hb_gcDummyMark
};

static PHB_LISTENSD s_listenParam(int iParam, HB_BOOL fError)
{
   auto lsd_ptr = static_cast<PHB_LISTENSD*>(hb_parptrGC(&s_gcListensdFuncs, iParam));

   if( lsd_ptr && *lsd_ptr )
   {
      return *lsd_ptr;
   }

   if( fError )
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }

   return nullptr;
}

static void s_listenRet(HB_SOCKET sd, const char * szRootPath, HB_BOOL rpc)
{
   if( sd != HB_NO_SOCKET )
   {
      auto lsd = static_cast<PHB_LISTENSD>(memset(hb_xgrab(sizeof(HB_LISTENSD)), 0, sizeof(HB_LISTENSD)));
      lsd->sd = sd;
      lsd->rpc = rpc;
      if( szRootPath )
      {
         hb_strncpy(lsd->rootPath, szRootPath, sizeof(lsd->rootPath) - 1);
      }
      else
      {
         hb_fsBaseDirBuff(lsd->rootPath);
      }
      auto iLen = static_cast<int>(strlen(lsd->rootPath));
      if( iLen > 0 )
      {
         if( !s_isDirSep(lsd->rootPath[iLen - 1]) )
         {
            if( iLen == static_cast<int>(sizeof(lsd->rootPath)) - 1 )
            {
               --iLen;
            }
            lsd->rootPath[iLen] = HB_OS_PATH_DELIM_CHR;
         }
      }
      auto lsd_ptr = static_cast<PHB_LISTENSD*>(hb_gcAllocate(sizeof(PHB_LISTENSD), &s_gcListensdFuncs));
      *lsd_ptr = lsd;
      hb_retptrGC(lsd_ptr);
   }
   else
   {
      hb_ret();
   }
}


/* netio_RPC(<pListenSocket> | <pConnectionSocket> [, <lEnable>]) --> <lPrev>
 */
HB_FUNC(NETIO_RPC)
{
   PHB_LISTENSD lsd = s_listenParam(1, false);
   bool fRPC = false;

   if( lsd )
   {
      fRPC = lsd->rpc;
      if( HB_ISLOG(2) )
      {
         lsd->rpc = hb_parl(2);
      }
   }
   else
   {
      PHB_CONSRV conn = s_consrvParam(1);
      if( conn )
      {
         fRPC = conn->rpc;
         if( HB_ISLOG(2) )
         {
            conn->rpc = hb_parl(2);
         }
      }
   }
   hb_retl(fRPC);
}

/* netio_RPCFilter( <pConnectionSocket>,
 *                  <sFuncSym> | <hValue> | NIL ) --> NIL
 */
HB_FUNC(NETIO_RPCFILTER)
{
   PHB_CONSRV conn = s_consrvParam(1);

   if( conn )
   {
      if( conn->rpcFilter )
      {
         hb_itemRelease(conn->rpcFilter);
         conn->rpcFilter = nullptr;
      }
      conn->rpcFunc = hb_itemGetSymbol(hb_param(2, Harbour::Item::SYMBOL));
      if( !conn->rpcFunc )
      {
         auto pHash = hb_param(2, Harbour::Item::HASH);
         if( pHash )
         {
            conn->rpcFilter = hb_itemNew(pHash);
            hb_gcUnlock(conn->rpcFilter);
         }
      }
   }
}

/* netio_ServerStop( <pListenSocket> | <pConnectionSocket> [, <lStop>] ) --> NIL
 */
HB_FUNC(NETIO_SERVERSTOP)
{
   PHB_LISTENSD lsd = s_listenParam(1, false);
   bool fStop = hb_parldef(2, true);

   if( lsd )
   {
      lsd->stop = fStop;
   }
   else
   {
      PHB_CONSRV conn = s_consrvParam(1);
      if( conn )
      {
         conn->stop = fStop;
      }
   }
}

/* netio_ServerTimeOut( <pConnectionSocket> [, <nTimeOut>] ) --> [<nTimeOut>]
 */
HB_FUNC(NETIO_SERVERTIMEOUT)
{
   PHB_CONSRV conn = s_consrvParam(1);

   if( conn )
   {
      hb_retni(conn->timeout);
      if( HB_ISNUM(2) )
      {
         conn->timeout = hb_parni(2);
      }
   }
}

/* netio_Listen( [<nPort>], [<cIfAddr>], [<cRootDir>], [<lRPC>] )
 *    --> <pListenSocket> | NIL
 */
HB_FUNC(NETIO_LISTEN)
{
   static bool s_fInit = true;

   auto iPort = hb_parnidef(1, NETIO_DEFAULT_PORT);
   auto szAddress = hb_parc(2);
   auto szRootPath = hb_parc(3);
   HB_BOOL fRPC = hb_parl(4);
   void * pSockAddr;
   unsigned uiLen;
   HB_SOCKET sd = HB_NO_SOCKET;

   if( s_fInit )
   {
      hb_socketInit();
      s_fInit = false;
   }

   if( hb_socketInetAddr(&pSockAddr, &uiLen, szAddress, iPort) )
   {
      sd = hb_socketOpen(HB_SOCKET_PF_INET, HB_SOCKET_PT_STREAM, 0);
      if( sd != HB_NO_SOCKET )
      {
         if( hb_socketBind(sd, pSockAddr, uiLen) != 0 || hb_socketListen(sd, 10) != 0 )
         {
            hb_socketClose(sd);
            sd = HB_NO_SOCKET;
         }
      }
      hb_xfree(pSockAddr);
   }

   s_listenRet(sd, szRootPath, fRPC);
}

/* netio_Accept( <pListenSocket>, [<nTimeOut>],
 *               [<cPass>], [<nCompressionLevel>], [<nStrategy>] )
 *    --> <pConnectionSocket> | NIL
 */
HB_FUNC(NETIO_ACCEPT)
{
   PHB_LISTENSD lsd = s_listenParam(1, true);
   PHB_CONSRV conn = nullptr;

   if( lsd && lsd->sd != HB_NO_SOCKET && !lsd->stop )
   {
      HB_MAXINT timeout = hb_parnintdef(2, -1);
      HB_SOCKET connsd;
      auto keylen = static_cast<int>(hb_parclen(3));

      if( keylen > NETIO_PASSWD_MAX )
      {
         keylen = NETIO_PASSWD_MAX;
      }
      auto iLevel = hb_parnidef(4, keylen ? HB_ZLIB_COMPRESSION_DEFAULT : HB_ZLIB_COMPRESSION_DISABLE);
      auto iStrategy = hb_parnidef(5, HB_ZLIB_STRATEGY_DEFAULT);

      do
      {
         connsd = hb_socketAccept(lsd->sd, nullptr, nullptr, timeout < 0 ? 1000 : timeout);
      }
      while( connsd == HB_NO_SOCKET && !lsd->stop && timeout < 0 && hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT && hb_vmRequestQuery() == 0 );

      if( connsd != HB_NO_SOCKET )
      {
         PHB_SOCKEX sock;

         hb_socketSetKeepAlive(connsd, true);
         hb_socketSetNoDelay(connsd, true);

         if( iLevel == HB_ZLIB_COMPRESSION_DISABLE )
         {
            sock = hb_sockexNew(connsd, nullptr, nullptr);
         }
         else
         {
            sock = hb_sockexNewZNet(connsd, hb_parc(3), keylen, iLevel, iStrategy);
         }
         
         if( sock != nullptr )
         {
            conn = s_consrvNew(sock, lsd->rootPath, lsd->rpc);
         }
         else
         {
            hb_socketClose(connsd);
         }
      }
   }

   s_consrvRet(conn);
}

/* netio_Compress( <pConnectionSocket>,
 *                 [<cPass>], [<nCompressionLevel>], [<nStrategy>] ) --> NIL
 */
HB_FUNC(NETIO_COMPRESS)
{
   PHB_CONSRV conn = s_consrvParam(1);

   if( conn && conn->sock && !conn->stop )
   {
      auto keylen = static_cast<int>(hb_parclen(2));
      PHB_SOCKEX sock;

      if( keylen > NETIO_PASSWD_MAX )
      {
         keylen = NETIO_PASSWD_MAX;
      }

      auto iLevel = hb_parnidef(3, keylen ? HB_ZLIB_COMPRESSION_DEFAULT : HB_ZLIB_COMPRESSION_DISABLE);
      auto iStrategy = hb_parnidef(4, HB_ZLIB_STRATEGY_DEFAULT);

      if( iLevel == HB_ZLIB_COMPRESSION_DISABLE )
      {
         sock = hb_sockexNew(hb_sockexGetHandle(conn->sock), nullptr, nullptr);
      }
      else
      {
         sock = hb_sockexNewZNet(hb_sockexGetHandle(conn->sock), hb_parc(2), keylen, iLevel, iStrategy);
      }

      if( sock )
      {
         hb_sockexClose(conn->sock, false);
         conn->sock = sock;
      }
   }
}

static HB_BOOL s_netio_login_accept(PHB_CONSRV conn)
{
   if( conn && conn->sock && !conn->stop && !conn->login )
   {
      HB_BYTE msgbuf[NETIO_MSGLEN];

      if( s_srvRecvAll(conn, msgbuf, NETIO_MSGLEN) && HB_GET_LE_INT32(msgbuf) == NETIO_LOGIN )
      {
         long len = HB_GET_LE_INT16(&msgbuf[4]);

         if( len < static_cast<long>(sizeof(msgbuf)) && len == static_cast<long>(strlen(NETIO_LOGINSTRID)) && s_srvRecvAll(conn, msgbuf, len) )
         {
            if( memcmp(NETIO_LOGINSTRID, msgbuf, len) == 0 )
            {
               HB_PUT_LE_UINT32(&msgbuf[0], NETIO_LOGIN);
               HB_PUT_LE_UINT32(&msgbuf[4], NETIO_CONNECTED);
               memset(msgbuf + 8, '\0', NETIO_MSGLEN - 8);
               if( s_srvSendAll(conn, msgbuf, NETIO_MSGLEN) )
               {
                  conn->login = true;
               }
            }
         }
      }
      if( !conn->login )
      {
         s_consrv_disconnect(conn);
      }
   }

   return conn && conn->login;
}

/* netio_VerifyClient( <pConnectionSocket> ) --> <lAccepted>
 */
HB_FUNC(NETIO_VERIFYCLIENT)
{
   PHB_CONSRV conn = s_consrvParam(1);

   if( conn )
   {
      hb_retl(s_netio_login_accept(conn));
   }
}

/* netio_Server( <pConnectionSocket> ) --> NIL
 */
HB_FUNC(NETIO_SERVER)
{
   PHB_CONSRV conn = s_consrvParam(1);

   if( s_netio_login_accept(conn) )
   {
      /* clear return value if any */
      hb_ret();

      for( ;; )
      {
         HB_BYTE buffer[2048], * ptr = nullptr, * msg;
         HB_BYTE msgbuf[NETIO_MSGLEN];
         bool fNoAnswer = false;
         HB_ERRCODE errCode = 0, errFsCode;
         long len = 0, size, size2;
         long lJulian, lMillisec;
         int iFileNo, iStreamID, iIndex, iResult;
         HB_FATTR ulAttr;
         HB_U32 uiMsg;
         HB_FATTR nFlags;
         HB_USHORT uiFlags;
         char * szExt;
         PHB_FILE pFile;
         HB_FOFFSET llOffset, llSize;
         HB_MAXINT nTimeout;

         msg = buffer;

         if( !s_srvRecvAll(conn, msgbuf, NETIO_MSGLEN) )
         {
            break;
         }

         uiMsg = HB_GET_LE_UINT32(msgbuf);
         switch( uiMsg )
         {
            case NETIO_EXISTS:
            case NETIO_DELETE:
            case NETIO_DIREXISTS:
            case NETIO_DIRMAKE:
            case NETIO_DIRREMOVE:
               size = HB_GET_LE_UINT16(&msgbuf[4]);
               if( size <= 0 )
               {
                  errCode = NETIO_ERR_WRONG_PARAM;
               }
               else
               {
                  if( size + conn->rootPathLen >= static_cast<long>(sizeof(buffer)) )
                  {
                     ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(size + conn->rootPathLen + 1));
                  }
                  msg[size] = '\0';
                  if( !s_srvRecvAll(conn, msg, size) )
                  {
                     errCode = NETIO_ERR_READ;
                  }
                  else
                  {
                     const char * pszName = s_consrvFilePath(reinterpret_cast<char*>(msg), conn, false);

                     if( !pszName )
                     {
                        errCode = NETIO_ERR_WRONG_FILE_PATH;
                     }
                     else if( !(uiMsg == NETIO_DELETE    ? hb_fileDelete(pszName) :
                               (uiMsg == NETIO_DIREXISTS ? hb_fileDirExists(pszName) :
                               (uiMsg == NETIO_DIRMAKE   ? hb_fileDirMake(pszName) :
                               (uiMsg == NETIO_DIRREMOVE ? hb_fileDirRemove(pszName) :
                                                           hb_fileExists(pszName, nullptr))))) )
                     {
                        errCode = s_srvFsError();
                     }
                     else
                     {
                        HB_PUT_LE_UINT32(&msg[0], uiMsg);
                        memset(msg + 4, '\0', NETIO_MSGLEN - 4);
                     }
                  }
               }
               break;

            case NETIO_DIRSPACE:
               size = HB_GET_LE_UINT16(&msgbuf[4]);
               uiFlags = HB_GET_LE_UINT16(&msgbuf[6]);
               if( size <= 0 )
               {
                  errCode = NETIO_ERR_WRONG_PARAM;
               }
               else
               {
                  if( size + conn->rootPathLen >= static_cast<long>(sizeof(buffer)) )
                  {
                     ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(size + conn->rootPathLen + 1));
                  }
                  msg[size] = '\0';
                  if( !s_srvRecvAll(conn, msg, size) )
                  {
                     errCode = NETIO_ERR_READ;
                  }
                  else
                  {
                     const char * pszName = s_consrvFilePath(reinterpret_cast<char*>(msg), conn, false);

                     if( !pszName )
                     {
                        errCode = NETIO_ERR_WRONG_FILE_PATH;
                     }
                     else
                     {
                        auto nSize = static_cast<HB_MAXINT>(hb_fileDirSpace(pszName, uiFlags));
                        errFsCode = hb_fsError();
                        HB_PUT_LE_UINT32(&msg[0], uiMsg);
                        HB_PUT_LE_UINT64(&msg[4], nSize);
                        HB_PUT_LE_UINT32(&msg[12], errFsCode);
                        memset(msg + 16, '\0', NETIO_MSGLEN - 16);
                     }
                  }
               }
               break;

            case NETIO_DIRECTORY:
               size  = HB_GET_LE_UINT16(&msgbuf[4]);
               size2 = HB_GET_LE_UINT16(&msgbuf[6]);
               if( size < 0 || size2 < 0 )
               {
                  errCode = NETIO_ERR_WRONG_PARAM;
               }
               else
               {
                  if( HB_MAX(size, size2) + conn->rootPathLen >= static_cast<long>(sizeof(buffer)) )
                  {
                     ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(HB_MAX(size, size2) + conn->rootPathLen + 1));
                  }
                  msg[size] = '\0';
                  if( !s_srvRecvAll(conn, msg, size) )
                  {
                     errCode = NETIO_ERR_READ;
                  }
                  else
                  {
                     const char * pszName = s_consrvFilePath(reinterpret_cast<char*>(msg), conn, false);
                     char * pszDirSpec;

                     pszDirSpec = pszName ? hb_strdup(pszName) : nullptr;

                     msg[size2] = '\0';
                     if( size2 && !s_srvRecvAll(conn, msg, size2) )
                     {
                        errCode = NETIO_ERR_READ;
                     }
                     else if( !pszName )
                     {
                        errCode = NETIO_ERR_WRONG_FILE_PATH;
                     }
                     else
                     {
                        HB_SIZE itmSize = 0;
                        char * itmData = nullptr;
                        const char * pszAttr = size2 ? reinterpret_cast<const char*>(msg) : nullptr;
                        PHB_ITEM pResult = hb_fileDirectory(pszDirSpec, pszAttr);

                        errFsCode = hb_fsError();

                        if( pResult )
                        {
                           itmData = hb_itemSerialize(pResult, HB_SERIALIZE_NUMSIZE, &itmSize);
                           hb_itemRelease(pResult);
                        }

                        if( itmSize <= sizeof(buffer) - NETIO_MSGLEN )
                        {
                           msg = buffer;
                        }
                        else if( !ptr || itmSize > static_cast<HB_SIZE>(HB_MAX(size, size2)) + conn->rootPathLen + 1 - NETIO_MSGLEN )
                        {
                           if( ptr )
                           {
                              hb_xfree(ptr);
                           }
                           ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(itmSize + NETIO_MSGLEN));
                        }
                        if( itmData )
                        {
                           memcpy(msg + NETIO_MSGLEN, itmData, itmSize);
                           hb_xfree(itmData);
                        }
                        len = static_cast<long>(itmSize);

                        HB_PUT_LE_UINT32(&msg[0], uiMsg);
                        HB_PUT_LE_UINT32(&msg[4], len);
                        HB_PUT_LE_UINT32(&msg[8], errFsCode);
                        memset(msg + 12, '\0', NETIO_MSGLEN - 12);
                     }
                     if( pszDirSpec )
                     {
                        hb_xfree(pszDirSpec);
                     }
                  }
               }
               break;

            case NETIO_LINKREAD:
               size = HB_GET_LE_UINT16(&msgbuf[4]);
               if( size < 0 )
               {
                  errCode = NETIO_ERR_WRONG_PARAM;
               }
               else
               {
                  if( size + conn->rootPathLen >= static_cast<long>(sizeof(buffer)) )
                  {
                     ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(size + conn->rootPathLen + 1));
                  }
                  msg[size] = '\0';
                  if( !s_srvRecvAll(conn, msg, size) )
                  {
                     errCode = NETIO_ERR_READ;
                  }
                  else
                  {
                     const char * pszName = s_consrvFilePath(reinterpret_cast<char*>(msg), conn, false);

                     if( !pszName )
                     {
                        errCode = NETIO_ERR_WRONG_FILE_PATH;
                     }
                     else
                     {
                        char * pszTarget = hb_fileLinkRead(pszName);

                        errFsCode = hb_fsError();
                        if( pszTarget )
                        {
                           len = static_cast<long>(strlen(pszTarget));
                           if( len <= static_cast<long>(sizeof(buffer) - NETIO_MSGLEN) )
                           {
                              msg = buffer;
                           }
                           else if( !ptr || len > static_cast<long>(size + conn->rootPathLen + 1 - NETIO_MSGLEN) )
                           {
                              if( ptr )
                              {
                                 hb_xfree(ptr);
                              }
                              ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(len + NETIO_MSGLEN));
                           }
                           memcpy(msg + NETIO_MSGLEN, pszTarget, len);
                           hb_xfree(pszTarget);
                        }
                        else
                        {
                           len = 0;
                        }
                        HB_PUT_LE_UINT32(&msg[0], uiMsg);
                        HB_PUT_LE_UINT32(&msg[4], len);
                        HB_PUT_LE_UINT32(&msg[8], errFsCode);
                        memset(msg + 12, '\0', NETIO_MSGLEN - 12);
                     }
                  }
               }
               break;

            case NETIO_RENAME:
            case NETIO_COPY:
            case NETIO_LINK:
            case NETIO_LINKSYM:
               size  = HB_GET_LE_UINT16(&msgbuf[4]);
               size2 = HB_GET_LE_UINT16(&msgbuf[6]);
               if( size <= 0 || size2 <= 0 )
               {
                  errCode = NETIO_ERR_WRONG_PARAM;
               }
               else
               {
                  if( HB_MAX(size, size2) + conn->rootPathLen >= static_cast<long>(sizeof(buffer)) )
                  {
                     ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(HB_MAX(size, size2) + conn->rootPathLen + 1));
                  }
                  msg[size] = '\0';
                  if( !s_srvRecvAll(conn, msg, size) )
                  {
                     errCode = NETIO_ERR_READ;
                  }
                  else
                  {
                     const char * szFile = s_consrvFilePath(reinterpret_cast<char*>(msg), conn, uiMsg == NETIO_LINKSYM);
                     char * szOldName = szFile ? hb_strdup(szFile) : nullptr;

                     msg[size2] = '\0';
                     if( !s_srvRecvAll(conn, msg, size2) )
                     {
                        errCode = NETIO_ERR_READ;
                     }
                     else if( !szOldName )
                     {
                        errCode = NETIO_ERR_WRONG_FILE_PATH;
                     }
                     else
                     {
                        szFile = s_consrvFilePath(reinterpret_cast<char*>(msg), conn, false);
                        if( !szFile )
                        {
                           errCode = NETIO_ERR_WRONG_FILE_PATH;
                        }
                        else if( !(uiMsg == NETIO_RENAME ? hb_fileRename(szOldName, szFile) :
                                  (uiMsg == NETIO_COPY   ? hb_fileCopy(szOldName, szFile) :
                                  (uiMsg == NETIO_LINK   ? hb_fileLink(szOldName, szFile) :
                                                           hb_fileLinkSym(szOldName, szFile)))) )
                        {
                           errCode = s_srvFsError();
                        }
                        else
                        {
                           HB_PUT_LE_UINT32(&msg[0], uiMsg);
                           memset(msg + 4, '\0', NETIO_MSGLEN - 4);
                        }
                     }
                     if( szOldName )
                     {
                        hb_xfree(szOldName);
                     }
                  }
               }
               break;

            case NETIO_ATTRSET:
            case NETIO_ATTRGET:
               size = HB_GET_LE_UINT16(&msgbuf[4]);
               ulAttr = HB_GET_LE_UINT32(&msgbuf[6]);
               if( size <= 0 )
               {
                  errCode = NETIO_ERR_WRONG_PARAM;
               }
               else
               {
                  if( size + conn->rootPathLen >= static_cast<long>(sizeof(buffer)) )
                  {
                     ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(size + conn->rootPathLen + 1));
                  }
                  msg[size] = '\0';
                  if( !s_srvRecvAll(conn, msg, size) )
                  {
                     errCode = NETIO_ERR_READ;
                  }
                  else
                  {
                     const char * pszName = s_consrvFilePath(reinterpret_cast<char*>(msg), conn, false);

                     if( !pszName )
                     {
                        errCode = NETIO_ERR_WRONG_FILE_PATH;
                     }
                     else if( !(uiMsg == NETIO_ATTRSET ? hb_fileAttrSet(pszName, ulAttr) : hb_fileAttrGet(pszName, &ulAttr)) )
                     {
                        errCode = s_srvFsError();
                     }
                     else
                     {
                        HB_PUT_LE_UINT32(&msg[0], uiMsg);
                        HB_PUT_LE_UINT32(&msg[4], ulAttr);
                        memset(msg + 8, '\0', NETIO_MSGLEN - 8);
                     }
                  }
               }
               break;

            case NETIO_FTIMESET:
            case NETIO_FTIMEGET:
               size = HB_GET_LE_UINT16(&msgbuf[4]);
               lJulian = HB_GET_LE_UINT32(&msgbuf[6]);
               lMillisec = HB_GET_LE_UINT32(&msgbuf[10]);
               if( size <= 0 )
               {
                  errCode = NETIO_ERR_WRONG_PARAM;
               }
               else
               {
                  if( size + conn->rootPathLen >= static_cast<long>(sizeof(buffer)) )
                  {
                     ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(size + conn->rootPathLen + 1));
                  }
                  msg[size] = '\0';
                  if( !s_srvRecvAll(conn, msg, size) )
                  {
                     errCode = NETIO_ERR_READ;
                  }
                  else
                  {
                     const char * pszName = s_consrvFilePath(reinterpret_cast<char*>(msg), conn, false);

                     if( !pszName )
                     {
                        errCode = NETIO_ERR_WRONG_FILE_PATH;
                     }
                     else if( !(uiMsg == NETIO_FTIMESET ? hb_fileTimeSet(pszName, lJulian, lMillisec) : hb_fileTimeGet(pszName, &lJulian, &lMillisec)) )
                     {
                        errCode = s_srvFsError();
                     }
                     else
                     {
                        HB_PUT_LE_UINT32(&msg[0], uiMsg);
                        HB_PUT_LE_UINT32(&msg[4], lJulian);
                        HB_PUT_LE_UINT32(&msg[8], lMillisec);
                        memset(msg + 12, '\0', NETIO_MSGLEN - 12);
                     }
                  }
               }
               break;

            case NETIO_OPEN:
            case NETIO_OPEN2:
               size = HB_GET_LE_UINT16(&msgbuf[4]);
               if( uiMsg == NETIO_OPEN2 )
               {
                  nFlags = HB_GET_LE_UINT32(&msgbuf[6]);
                  szExt = msgbuf[10] ? hb_strndup(reinterpret_cast<const char*>(&msgbuf[10]), NETIO_MSGLEN - 10) : nullptr;
               }
               else
               {
                  nFlags = HB_GET_LE_UINT16(&msgbuf[6]);
                  szExt = msgbuf[8] ? hb_strndup(reinterpret_cast<const char*>(&msgbuf[8]), NETIO_MSGLEN - 8) : nullptr;
               }
               if( size <= 0 )
               {
                  errCode = NETIO_ERR_WRONG_PARAM;
               }
               else
               {
                  if( size + conn->rootPathLen >= static_cast<long>(sizeof(buffer)) )
                  {
                     ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(size + conn->rootPathLen + 1));
                  }
                  msg[size] = '\0';
                  if( !s_srvRecvAll(conn, msg, size) )
                  {
                     errCode = NETIO_ERR_READ;
                  }
                  else if( conn->filesCount >= NETIO_FILES_MAX )
                  {
                     errCode = NETIO_ERR_FILES_MAX;
                  }
                  else
                  {
                     const char * szFile = s_consrvFilePath(reinterpret_cast<char*>(msg), conn, false);

                     if( !szFile )
                     {
                        errCode = NETIO_ERR_WRONG_FILE_PATH;
                     }
                     else
                     {
                        nFlags &= ~ static_cast<HB_FATTR>(FXO_COPYNAME);
                        pFile = hb_fileExtOpen(szFile, szExt, nFlags, nullptr, nullptr);
                        if( !pFile )
                        {
                           errCode = s_srvFsError();
                        }
                        else
                        {
                           iFileNo = s_srvFileNew(conn, pFile);
                           if( iFileNo < 0 )
                           {
                              errCode = NETIO_ERR_FILES_MAX;
                              hb_fileClose(pFile);
                           }
                           else
                           {
                              HB_PUT_LE_UINT32(&msg[0], NETIO_OPEN);
                              HB_PUT_LE_UINT16(&msg[4], iFileNo);
                              memset(msg + 6, '\0', NETIO_MSGLEN - 6);
                           }
                        }
                     }
                  }
               }
               if( szExt )
               {
                  hb_xfree(szExt);
               }
               break;

            case NETIO_CONFIGURE:
               iFileNo = HB_GET_LE_UINT16(&msgbuf[4]);
               size = HB_GET_LE_UINT32(&msgbuf[6]);
               iIndex = HB_GET_LE_INT32(&msgbuf[10]);
               if( size < 0 )
               {
                  errCode = NETIO_ERR_WRONG_PARAM;
               }
               else
               {
                  if( size > static_cast<long>(sizeof(buffer) - NETIO_MSGLEN) )
                  {
                     ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(size + NETIO_MSGLEN));
                  }
                  if( !s_srvRecvAll(conn, msg, size) )
                  {
                     errCode = NETIO_ERR_READ;
                  }
                  else
                  {
                     pFile = s_srvFileGet(conn, iFileNo);
                     if( pFile == nullptr )
                     {
                        errCode = NETIO_ERR_WRONG_FILE_HANDLE;
                     }
                     else
                     {
                        PHB_ITEM pValue = nullptr;

                        if( size > 0 )
                        {
                           auto data = reinterpret_cast<const char*>(msg);
                           HB_SIZE nSize = size;

                           pValue = hb_itemDeserialize(&data, &nSize);
                           if( !pValue || nSize != 0 )
                           {
                              errCode = NETIO_ERR_WRONG_PARAM;
                           }
                        }
                        if( errCode == 0 )
                        {
                           char * itmData = nullptr;
                           HB_SIZE itmSize = 0;

                           iResult = hb_fileConfigure(pFile, iIndex, pValue) ? 1 : 0;
                           if( pValue )
                           {
                              itmData = hb_itemSerialize(pValue, HB_SERIALIZE_NUMSIZE, &itmSize);
                           }

                           if( itmSize <= sizeof(buffer) - NETIO_MSGLEN )
                           {
                              msg = buffer;
                           }
                           else if( !ptr || itmSize > static_cast<HB_SIZE>(size) - NETIO_MSGLEN )
                           {
                              if( ptr )
                              {
                                 hb_xfree(ptr);
                              }
                              ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(itmSize + NETIO_MSGLEN));
                           }
                           else
                           {
                              msg = ptr;
                           }

                           if( itmData )
                           {
                              memcpy(msg + NETIO_MSGLEN, itmData, itmSize);
                              hb_xfree(itmData);
                           }
                           len = static_cast<long>(itmSize);

                           errFsCode = hb_fsError();
                           HB_PUT_LE_UINT32(&msg[0], uiMsg);
                           HB_PUT_LE_UINT32(&msg[4], len);
                           HB_PUT_LE_UINT32(&msg[8], iResult);
                           HB_PUT_LE_UINT32(&msg[12], errFsCode);
                           memset(msg + 12, '\0', NETIO_MSGLEN - 12);
                        }
                        if( pValue )
                        {
                           hb_itemRelease(pValue);
                        }
                     }
                  }
               }
               break;

            case NETIO_READ:
            case NETIO_READAT:
               iFileNo = HB_GET_LE_UINT16(&msgbuf[4]);
               size = HB_GET_LE_UINT32(&msgbuf[6]);
               if( uiMsg == NETIO_READ )
               {
                  nTimeout = HB_GET_LE_INT64(&msgbuf[10]);
                  llOffset = 0;
               }
               else
               {
                  nTimeout = 0;
                  llOffset = HB_GET_LE_INT64(&msgbuf[10]);
               }
               if( size < 0 )
               {
                  errCode = NETIO_ERR_WRONG_PARAM;
               }
               else
               {
                  if( size > static_cast<long>(sizeof(buffer) - NETIO_MSGLEN) )
                  {
                     ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(size + NETIO_MSGLEN));
                  }
                  pFile = s_srvFileGet(conn, iFileNo);
                  if( pFile == nullptr )
                  {
                     errCode = NETIO_ERR_WRONG_FILE_HANDLE;
                  }
                  else
                  {
                     if( uiMsg == NETIO_READ )
                     {
                        len = static_cast<long>(hb_fileRead(pFile, msg + NETIO_MSGLEN, size, nTimeout));
                     }
                     else
                     {
                        len = static_cast<long>(hb_fileReadAt(pFile, msg + NETIO_MSGLEN, size, llOffset));
                     }
                     errFsCode = hb_fsError();
                     HB_PUT_LE_UINT32(&msg[0], uiMsg);
                     HB_PUT_LE_UINT32(&msg[4], len);
                     HB_PUT_LE_UINT32(&msg[8], errFsCode);
                     memset(msg + 12, '\0', NETIO_MSGLEN - 12);
                     if( len == FS_ERROR )
                     {
                        len = 0;
                     }
                  }
               }
               break;

            case NETIO_WRITE:
            case NETIO_WRITEAT:
               iFileNo = HB_GET_LE_UINT16(&msgbuf[4]);
               size = HB_GET_LE_UINT32(&msgbuf[6]);
               if( uiMsg == NETIO_WRITE )
               {
                  nTimeout = HB_GET_LE_INT64(&msgbuf[10]);
                  llOffset = 0;
               }
               else
               {
                  nTimeout = 0;
                  llOffset = HB_GET_LE_INT64(&msgbuf[10]);
               }
               if( size < 0 )
               {
                  errCode = NETIO_ERR_WRONG_PARAM;
               }
               else
               {
                  if( size > static_cast<long>(sizeof(buffer)) )
                  {
                     ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(size));
                  }
                  if( !s_srvRecvAll(conn, msg, size) )
                  {
                     errCode = NETIO_ERR_READ;
                  }
                  else
                  {
                     pFile = s_srvFileGet(conn, iFileNo);
                     if( pFile == nullptr )
                     {
                        errCode = NETIO_ERR_WRONG_FILE_HANDLE;
                     }
                     else
                     {
                        if( uiMsg == NETIO_WRITE )
                        {
                           size = static_cast<long>(hb_fileWrite(pFile, msg, size, nTimeout));
                        }
                        else
                        {
                           size = static_cast<long>(hb_fileWriteAt(pFile, msg, size, llOffset));
                        }
                        errFsCode = hb_fsError();
                        HB_PUT_LE_UINT32(&msg[0], uiMsg);
                        HB_PUT_LE_UINT32(&msg[4], size);
                        HB_PUT_LE_UINT32(&msg[8], errFsCode);
                        memset(msg + 12, '\0', NETIO_MSGLEN - 12);
                     }
                  }
               }
               break;

            case NETIO_UNLOCK:
               fNoAnswer = true;
               /* fallthrough */
            case NETIO_LOCK:
            case NETIO_TESTLOCK:
               iFileNo = HB_GET_LE_UINT16(&msgbuf[4]);
               llOffset = HB_GET_LE_INT64(&msgbuf[6]);
               llSize = HB_GET_LE_INT64(&msgbuf[14]);
               uiFlags = HB_GET_LE_UINT16(&msgbuf[22]);
               pFile = s_srvFileGet(conn, iFileNo);
               if( pFile == nullptr )
               {
                  errCode = NETIO_ERR_WRONG_FILE_HANDLE;
               }
               else if( uiMsg == NETIO_TESTLOCK )
               {
                  iResult = hb_fileLockTest(pFile, llOffset, llSize, uiFlags);
                  errFsCode = hb_fsError();
                  HB_PUT_LE_UINT32(&msg[0], uiMsg);
                  HB_PUT_LE_UINT32(&msg[4], iResult);
                  HB_PUT_LE_UINT32(&msg[8], errFsCode);
                  memset(msg + 12, '\0', NETIO_MSGLEN - 4);
               }
               else if( !hb_fileLock(pFile, llOffset, llSize, uiFlags) )
               {
                  errCode = s_srvFsError();
               }
               else if( !fNoAnswer )
               {
                  HB_PUT_LE_UINT32(&msg[0], uiMsg);
                  memset(msg + 4, '\0', NETIO_MSGLEN - 4);
               }
               break;

            case NETIO_TRUNC:
               iFileNo = HB_GET_LE_UINT16(&msgbuf[4]);
               llOffset = HB_GET_LE_INT64(&msgbuf[6]);
               pFile = s_srvFileGet(conn, iFileNo);
               if( pFile == nullptr )
               {
                  errCode = NETIO_ERR_WRONG_FILE_HANDLE;
               }
               else if( !hb_fileTruncAt(pFile, llOffset) )
               {
                  errCode = s_srvFsError();
               }
               else
               {
                  HB_PUT_LE_UINT32(&msg[0], NETIO_TRUNC);
                  memset(msg + 4, '\0', NETIO_MSGLEN - 4);
               }
               break;

            case NETIO_SEEK:
               iFileNo = HB_GET_LE_UINT16(&msgbuf[4]);
               llOffset = HB_GET_LE_INT64(&msgbuf[6]);
               uiFlags = HB_GET_LE_UINT16(&msgbuf[14]);
               pFile = s_srvFileGet(conn, iFileNo);
               if( pFile == nullptr )
               {
                  errCode = NETIO_ERR_WRONG_FILE_HANDLE;
               }
               else
               {
                  llOffset = hb_fileSeek(pFile, llOffset, uiFlags);
                  errFsCode = hb_fsError();
                  HB_PUT_LE_UINT32(&msg[0], NETIO_SEEK);
                  HB_PUT_LE_UINT64(&msg[4], llOffset);
                  HB_PUT_LE_UINT32(&msg[12], errFsCode);
                  memset(msg + 16, '\0', NETIO_MSGLEN - 16);
               }
               break;

            case NETIO_SIZE:
               iFileNo = HB_GET_LE_UINT16(&msgbuf[4]);
               pFile = s_srvFileGet(conn, iFileNo);
               if( pFile == nullptr )
               {
                  errCode = NETIO_ERR_WRONG_FILE_HANDLE;
               }
               else
               {
                  llOffset = hb_fileSize(pFile);
                  errFsCode = hb_fsError();
                  HB_PUT_LE_UINT32(&msg[0], NETIO_SIZE);
                  HB_PUT_LE_UINT64(&msg[4], llOffset);
                  HB_PUT_LE_UINT32(&msg[12], errFsCode);
                  memset(msg + 16, '\0', NETIO_MSGLEN - 16);
               }
               break;

            case NETIO_EOF:
               iFileNo = HB_GET_LE_UINT16(&msgbuf[4]);
               pFile = s_srvFileGet(conn, iFileNo);
               if( pFile == nullptr )
               {
                  errCode = NETIO_ERR_WRONG_FILE_HANDLE;
               }
               else
               {
                  iResult = hb_fileEof(pFile) ? 1 : 0;
                  errFsCode = hb_fsError();
                  HB_PUT_LE_UINT32(&msg[0], NETIO_EOF);
                  HB_PUT_LE_UINT32(&msg[4], iResult);
                  HB_PUT_LE_UINT32(&msg[8], errFsCode);
                  memset(msg + 12, '\0', NETIO_MSGLEN - 12);
               }
               break;

            case NETIO_COMMIT:
               iFileNo = HB_GET_LE_UINT16(&msgbuf[4]);
               pFile = s_srvFileGet(conn, iFileNo);
               if( pFile )
               {
                  hb_fileCommit(pFile);
               }
               fNoAnswer = true;
               break;

            case NETIO_CLOSE:
               iFileNo = HB_GET_LE_UINT16(&msgbuf[4]);
               pFile = s_srvFileFree(conn, iFileNo);
               if( pFile == nullptr )
               {
                  errCode = NETIO_ERR_WRONG_FILE_HANDLE;
               }
               else
               {
                  hb_fileClose(pFile);
                  HB_PUT_LE_UINT32(&msg[0], NETIO_CLOSE);
                  memset(msg + 4, '\0', NETIO_MSGLEN - 4);
               }
               break;

            case NETIO_SRVCLOSE:
               iStreamID = HB_GET_LE_INT32(&msgbuf[4]);
               if( iStreamID && conn->mutex && hb_threadMutexLock(conn->mutex) )
               {
                  PHB_CONSTREAM * pStreamPtr = &conn->streams;
                  while( *pStreamPtr )
                  {
                     if( (*pStreamPtr)->id == iStreamID )
                     {
                        break;
                     }
                     pStreamPtr = &(*pStreamPtr)->next;
                  }
                  if( *pStreamPtr != nullptr )
                  {
                     PHB_CONSTREAM stream = *pStreamPtr;
                     *pStreamPtr = stream->next;
                     hb_xfree(stream);
                  }
                  else
                  {
                     iStreamID = 0;
                  }
                  hb_threadMutexUnlock(conn->mutex);
               }
               else
               {
                  iStreamID = 0;
               }

               if( iStreamID == 0 )
               {
                  errCode = NETIO_ERR_WRONG_STREAMID;
               }
               else
               {
                  HB_PUT_LE_UINT32(&msg[0], NETIO_SRVCLOSE);
                  memset(msg + 4, '\0', NETIO_MSGLEN - 4);
               }
               break;

            case NETIO_PROC:
               fNoAnswer = true;
               /* fallthrough */
            case NETIO_PROCIS:
            case NETIO_PROCW:
            case NETIO_FUNC:
            case NETIO_FUNCCTRL:
               size = HB_GET_LE_UINT32(&msgbuf[4]);
               if( size < 2 )
               {
                  errCode = NETIO_ERR_WRONG_PARAM;
               }
               else
               {
                  if( size > static_cast<long>(sizeof(buffer)) )
                  {
                     ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(size));
                  }
                  if( !s_srvRecvAll(conn, msg, size) )
                  {
                     errCode = NETIO_ERR_READ;
                  }
                  else if( !conn->rpc )
                  {
                     errCode = NETIO_ERR_UNSUPPORTED;
                  }
                  else
                  {
                     auto data = reinterpret_cast<const char*>(msg);
                     size2 = static_cast<long>(hb_strnlen(data, size)) + 1;
                     if( size2 > size )
                     {
                        errCode = NETIO_ERR_WRONG_PARAM;
                     }
                     else
                     {
                        PHB_DYNS pDynSym = nullptr;
                        PHB_ITEM pItem = nullptr;

                        if( conn->rpcFilter )
                        {
                           pItem = hb_hashGetCItemPtr(conn->rpcFilter, data);
                           if( !pItem )
                           {
                              errCode = NETIO_ERR_NOT_EXISTS;
                           }
                        }
                        else
                        {
                           pDynSym = hb_dynsymFindName(data);
                           if( !pDynSym || !hb_dynsymIsFunction(pDynSym) )
                           {
                              errCode = NETIO_ERR_NOT_EXISTS;
                           }
                        }

                        if( uiMsg != NETIO_PROCIS && errCode == 0 )
                        {
                           if( hb_vmRequestReenter() )
                           {
                              HB_SIZE nSize = size - size2;
                              HB_USHORT uiPCount = 0;
                              HB_BOOL fSend = false;

                              iStreamID = 0;
                              data += size2;
                              if( pItem != nullptr )
                              {
                                 fSend = true;
                                 hb_vmPushEvalSym();
                                 hb_vmPush(pItem);
                              }
                              else if( conn->rpcFunc )
                              {
                                 hb_vmPushSymbol(conn->rpcFunc);
                                 hb_vmPushNil();
                                 hb_vmPushDynSym(pDynSym);
                                 ++uiPCount;
                              }
                              else
                              {
                                 hb_vmPushDynSym(pDynSym);
                                 hb_vmPushNil();
                              }
                              if( uiMsg == NETIO_FUNCCTRL )
                              {
                                 int iStreamType;

                                 iStreamID = HB_GET_LE_INT32(&msgbuf[8]);
                                 iStreamType = HB_GET_LE_INT32(&msgbuf[12]);
                                 hb_vmPush(hb_param(1, Harbour::Item::ANY));
                                 hb_vmPushInteger(iStreamID);
                                 uiPCount += 2;
                                 if( iStreamType != NETIO_SRVDATA && iStreamType != NETIO_SRVITEM )
                                 {
                                    iStreamID = 0;
                                 }
                                 if( iStreamID )
                                 {
                                    if( conn->mutex == nullptr )
                                    {
                                       conn->mutex = hb_threadMutexCreate();
                                    }
                                    if( hb_threadMutexLock(conn->mutex) )
                                    {
                                       auto stream = static_cast<PHB_CONSTREAM>(hb_xgrab(sizeof(HB_CONSTREAM)));
                                       stream->id = iStreamID;
                                       stream->type = iStreamType;
                                       stream->next = conn->streams;
                                       conn->streams = stream;
                                    }
                                    else
                                    {
                                       errCode = NETIO_ERR_REFUSED;
                                       iStreamID = 0;
                                    }
                                 }
                                 else
                                 {
                                    errCode = NETIO_ERR_WRONG_PARAM;
                                 }
                              }
                              while( nSize > 0 && errCode == 0 )
                              {
                                 pItem = hb_itemDeserialize(&data, &nSize);
                                 if( !pItem )
                                 {
                                    errCode = NETIO_ERR_WRONG_PARAM;
                                    break;
                                 }
                                 ++uiPCount;
                                 hb_vmPush(pItem);
                                 hb_itemRelease(pItem);
                              }
                              if( errCode != 0 )
                              {
                                 uiPCount += 2;
                                 do
                                 {
                                    hb_stackPop();
                                 }
                                 while( --uiPCount );
                              }
                              else
                              {
                                 if( fSend )
                                 {
                                    hb_vmSend(uiPCount);
                                 }
                                 else
                                 {
                                    hb_vmProc(uiPCount);
                                 }
                                 if( uiMsg == NETIO_FUNC || uiMsg == NETIO_FUNCCTRL )
                                 {
                                    HB_SIZE itmSize;
                                    PHB_ITEM pResult = hb_stackReturnItem();
                                    char * itmData = hb_itemSerialize(pResult, HB_SERIALIZE_NUMSIZE, &itmSize);
                                    if( itmSize <= sizeof(buffer) - NETIO_MSGLEN )
                                    {
                                       msg = buffer;
                                    }
                                    else if( !ptr || itmSize > static_cast<HB_SIZE>(size) - NETIO_MSGLEN )
                                    {
                                       if( ptr )
                                       {
                                          hb_xfree(ptr);
                                       }
                                       ptr = msg = static_cast<HB_BYTE*>(hb_xgrab(itmSize + NETIO_MSGLEN));
                                    }
                                    memcpy(msg + NETIO_MSGLEN, itmData, itmSize);
                                    hb_xfree(itmData);
                                    len = static_cast<long>(itmSize);
                                    if( iStreamID && hb_itemGetNI(pResult) == iStreamID )
                                    {
                                       hb_threadMutexUnlock(conn->mutex);
                                       iStreamID = 0;
                                    }
                                 }
                              }
                              hb_vmRequestRestore();
                              if( iStreamID )
                              {
                                 PHB_CONSTREAM stream = conn->streams;

                                 if( stream->id == iStreamID )
                                 {
                                    conn->streams = stream->next;
                                    hb_xfree(stream);
                                 }
                                 hb_threadMutexUnlock(conn->mutex);
                              }
                           }
                           else
                           {
                              errCode = NETIO_ERR_REFUSED;
                           }
                        }
                     }
                     if( errCode == 0 && !fNoAnswer )
                     {
                        HB_PUT_LE_UINT32(&msg[0], uiMsg);
                        HB_PUT_LE_UINT32(&msg[4], len);
                        memset(msg + 8, '\0', NETIO_MSGLEN - 8);
                     }
                  }
               }
               break;

            case NETIO_SYNC:
               continue;

            default: /* unrecognized message */
               errCode = NETIO_ERR_UNKNOWN_COMMAND;
               break;
         }

         if( fNoAnswer )
         {
            #if 0
            continue; /* do not send dummy record */
            #endif
            HB_PUT_LE_UINT32(&msg[0], NETIO_SYNC);
            memset(msg + 4, '\0', NETIO_MSGLEN - 4);
            len = NETIO_MSGLEN;
         }
         else if( errCode != 0 )
         {
            HB_PUT_LE_UINT32(&msg[0], NETIO_ERROR);
            HB_PUT_LE_UINT32(&msg[4], errCode);
            memset(msg + 8, '\0', NETIO_MSGLEN - 8);
            len = NETIO_MSGLEN;
         }
         else
         {
            len += NETIO_MSGLEN;
         }

         errCode = !s_srvSendAll(conn, msg, len);

         if( ptr )
         {
            hb_xfree(ptr);
         }

         if( errCode )
         {
            break;
         }
      }
   }
}

/* netio_ServedConnection() --> <pConnectionSocket>
 */
HB_FUNC(NETIO_SERVEDCONNECTION)
{
   static PHB_DYNS s_pDyns_netio_server = nullptr;

   if( s_pDyns_netio_server == nullptr )
   {
      s_pDyns_netio_server = hb_dynsymGetCase("NETIO_SERVER");
   }

   if( s_pDyns_netio_server != nullptr )
   {
      HB_ISIZ nOffset = hb_stackBaseSymbolOffset(hb_dynsymSymbol(s_pDyns_netio_server));

      if( nOffset > 0 )
      {
         PHB_ITEM pItem = hb_stackItem(nOffset + 2);

         if( hb_itemGetPtrGC(pItem, &s_gcConSrvFuncs) != nullptr )
         {
            hb_itemReturn(pItem);
         }
      }
   }
}

/* netio_SrvSendItem(<pConnectionSocket>, <nStreamID>, <xData>) --> <lSent>
 */
HB_FUNC(NETIO_SRVSENDITEM)
{
   PHB_CONSRV conn = s_consrvParam(1);
   auto iStreamID = hb_parni(2);
   auto pItem = hb_param(3, Harbour::Item::ANY);
   bool fResult = false;

   if( conn && conn->sock && !conn->stop && conn->mutex && iStreamID && pItem )
   {
      HB_SIZE nLen;

      char * itmData = hb_itemSerialize(pItem, HB_SERIALIZE_NUMSIZE, &nLen);
      auto lLen = static_cast<long>(nLen);
      auto msg = static_cast<char*>(hb_xgrab(lLen + NETIO_MSGLEN));
      HB_PUT_LE_UINT32(&msg[0], NETIO_SRVITEM);
      HB_PUT_LE_UINT32(&msg[4], iStreamID);
      HB_PUT_LE_UINT32(&msg[8], lLen);
      memset(msg + 12, '\0', NETIO_MSGLEN - 12);
      memcpy(msg + NETIO_MSGLEN, itmData, lLen);
      hb_xfree(itmData);

      lLen += NETIO_MSGLEN;
      if( hb_threadMutexLock(conn->mutex) )
      {
         PHB_CONSTREAM stream = conn->streams;
         while( stream )
         {
            if( stream->id == iStreamID )
            {
               break;
            }
            stream = stream->next;
         }
         if( stream && stream->type == NETIO_SRVITEM )
         {
            fResult = s_srvSendAll(conn, msg, lLen);
         }
         hb_threadMutexUnlock(conn->mutex);
      }
      hb_xfree(msg);
   }
   hb_retl(fResult);
}

/* netio_SrvSendData(<pConnectionSocket>, <nStreamID>, <cData>) --> <lSent>
 */
HB_FUNC(NETIO_SRVSENDDATA)
{
   PHB_CONSRV conn = s_consrvParam(1);
   auto iStreamID = hb_parni(2);
   auto lLen = static_cast<long>(hb_parclen(3));
   bool fResult = false;

   if( conn && conn->sock && !conn->stop && conn->mutex && iStreamID && lLen > 0 )
   {
      auto msg = static_cast<char*>(hb_xgrab(lLen + NETIO_MSGLEN));
      HB_PUT_LE_UINT32(&msg[0], NETIO_SRVDATA);
      HB_PUT_LE_UINT32(&msg[4], iStreamID);
      HB_PUT_LE_UINT32(&msg[8], lLen);
      memset(msg + 12, '\0', NETIO_MSGLEN - 12);
      memcpy(msg + NETIO_MSGLEN, hb_parc(3), lLen);

      lLen += NETIO_MSGLEN;
      if( hb_threadMutexLock(conn->mutex) )
      {
         PHB_CONSTREAM stream = conn->streams;
         while( stream )
         {
            if( stream->id == iStreamID )
            {
               break;
            }
            stream = stream->next;
         }
         if( stream && stream->type == NETIO_SRVDATA )
         {
            fResult = s_srvSendAll(conn, msg, lLen);
         }
         hb_threadMutexUnlock(conn->mutex);
      }
      hb_xfree(msg);
   }
   hb_retl(fResult);
}

/* netio_SrvStatus( <pConnectionSocket>
 *                  [, <nStreamID> | <nSrvInfo>, @<xData>] ) --> <nStatus>
 */
HB_FUNC(NETIO_SRVSTATUS)
{
   PHB_CONSRV conn = s_consrvParam(1);
   auto iStreamID = hb_parni(2);
   int iSrvInfo = 0;
   int iStatus = NETIO_SRVSTAT_RUNNING;

   if( iStreamID < 0 )
   {
      iSrvInfo = iStreamID;
      iStreamID = 0;
   }

   if( !conn )
   {
      iStatus = NETIO_SRVSTAT_WRONGHANDLE;
   }
   else if( conn->sock == nullptr )
   {
      iStatus = NETIO_SRVSTAT_CLOSED;
   }
   else if( conn->stop )
   {
      iStatus = NETIO_SRVSTAT_STOPPED;
   }
   else if( iStreamID != 0 && conn->mutex )
   {
      if( hb_threadMutexLock(conn->mutex) )
      {
         PHB_CONSTREAM stream = conn->streams;
         while( stream )
         {
            if( stream->id == iStreamID )
            {
               iStatus = stream->type == NETIO_SRVDATA ? NETIO_SRVSTAT_DATASTREAM : NETIO_SRVSTAT_ITEMSTREAM;
               break;
            }
            stream = stream->next;
         }
         hb_threadMutexUnlock(conn->mutex);
      }
   }
   else
   {
      switch( iSrvInfo )
      {
         case NETIO_SRVINFO_FILESCOUNT:
            hb_storni(conn->filesCount, 3);
            break;
         case NETIO_SRVINFO_BYTESSENT:
            hb_stornint(conn->wr_count, 3);
            break;
         case NETIO_SRVINFO_BYTESRECEIVED:
            hb_stornint(conn->rd_count, 3);
            break;
         case NETIO_SRVINFO_PEERADDRESS:
         {
            void * addr;
            unsigned int len;
            PHB_ITEM pItem = nullptr;

            if( hb_socketGetPeerName(hb_sockexGetHandle(conn->sock), &addr, &len) == 0 )
            {
               pItem = hb_socketAddrToItem(addr, len);
               if( addr )
               {
                  hb_xfree(addr);
               }
            }
            if( !hb_itemParamStoreRelease(3, pItem) && pItem )
            {
               hb_itemRelease(pItem);
            }
            break;
         }
      }
   }

   hb_retni(iStatus);
}
