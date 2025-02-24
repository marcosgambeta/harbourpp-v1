/*
 * Demonstration code for alternative RDD IO API which uses own
 * very simple TCP/IP file server with RPC support
 * All files which names starts 'net:' are redirected to this API.
 * This is client code with
 *    netio_Connect( [<cServer>], [<nPort>], [<nTimeOut>],
 *                   [<cPasswd>], [<nCompressionLevel>], [<nStrategy>] )
 *          --> <lOK>
 * function which register alternative RDD IO API, sets server
 * address and port and connection timeout parameter.
 * Then it tries to connect to the server and returns .T. on success.
 * This code also provides the following .prg functions:
 *    netio_Disconnect( [<cServer>], [<nPort>] ) --> <lOK>
 *    netio_Decode([@]<cFullName>, [@<cServer>], [@<nPort>], [@<nTimeOut>],
 *                  [@<cPasswd>], [@<nCompressionLevel>], [@<nStrategy>])
 *          --> <lDecoded>
 *    netio_ProcExists( <cProcName> ) --> <lExists>
 *    netio_ProcExec(<cProcName> [, <params,...>]) --> <lSent>
 *    netio_ProcExecW( <cProcName> [, <params,...>] ) --> <lExecuted>
 *    netio_FuncExec(<cFuncName> [, <params,...>]) --> <xFuncRetVal>
 *
 *    netio_OpenDataStream( <cStreamFuncName> [, <params,...>] )
 *          --> <nStreamID>
 *    netio_OpenItemStream( <cStreamFuncName> [, <params,...>] )
 *          --> <nStreamID>
 *    netio_CloseStream( <nStreamID>, [<cServer>], [<nPort>] )
 *          --> <lOK>
 *    netio_GetData(<nStreamID>, [<cServer>], [<nPort>])
 *          --> <aData> | <cData> | NIL
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

/* this has to be declared before hbapifs.h is included */
#define _HB_FILE_IMPLEMENTATION_

#include <hbapi.hpp>
#include <hbapifs.hpp>
#include <hbapiitm.hpp>
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
 * client code
 */

struct _HB_SRVDATA
{
   int      id;
   int      type;
   PHB_ITEM array;
   char *   data;
   HB_SIZE  size;
   HB_SIZE  bufsize;
   HB_SIZE  maxsize;
   struct _HB_SRVDATA * next;
};

using HB_SRVDATA = _HB_SRVDATA;
using PHB_SRVDATA = _HB_SRVDATA *;

struct _HB_CONCLI
{
   HB_COUNTER          used;
   HB_COUNTER          usrcount;
   PHB_ITEM            mutex;
   HB_ERRCODE          errcode;
   int                 timeout;
   int                 port;
   PHB_SOCKEX          sock;
   PHB_SRVDATA         srvdata;
   struct _HB_CONCLI * next;
   char *              path;
   int                 level;
   int                 strategy;
   int                 passlen;
   char                passwd[NETIO_PASSWD_MAX];
   char                server[1];
};

using HB_CONCLI = _HB_CONCLI;
using PHB_CONCLI = _HB_CONCLI *;

struct _HB_FILE
{
   const HB_FILE_FUNCS * pFuncs;
   PHB_CONCLI conn;
   HB_USHORT  fd;
};

using HB_FILE = _HB_FILE;

struct HB_CONDATA
{
   int  timeout;
   int  port;
   int  level;
   int  strategy;
   int  passlen;
   char server[NETIO_SERVERNAME_MAX];
   char passwd[NETIO_PASSWD_MAX];
};

using PHB_CONDATA = HB_CONDATA *;

/* MT macros */
#define HB_NETIO_LOCK()    hb_threadEnterCriticalSection(&s_netioMtx)
#define HB_NETIO_UNLOCK()  hb_threadLeaveCriticalSection(&s_netioMtx)
static HB_CRITICAL_NEW(s_netioMtx);

static HB_TSD_NEW(s_conData, sizeof(HB_CONDATA), nullptr, nullptr);

static PHB_CONCLI s_connections = nullptr;

static HB_COUNTER s_pathCount = 0;

static bool s_defaultInit = true;
static HB_CONDATA s_defaultConn =
{
   NETIO_DEFAULT_TIMEOUT,
   NETIO_DEFAULT_PORT,
   HB_ZLIB_COMPRESSION_DISABLE,
   HB_ZLIB_STRATEGY_DEFAULT,
   0,
   NETIO_DEFAULT_SERVER,
   ""
};

static bool s_fInit = true;

static const HB_FILE_FUNCS * s_fileMethods(void);

static void hb_errRT_NETIO(HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, HB_ERRCODE errOsCode, const char * szDescription, const char * szOperation)
{
   auto pError = hb_errRT_New(ES_ERROR, "NETIO", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE);
   hb_errLaunch(pError);
   hb_itemRelease(pError);
}

static long s_fileRecvAll(PHB_CONCLI conn, void * buffer, long len)
{
   auto ptr = static_cast<HB_BYTE*>(buffer);
   long lRead = 0, l;
   HB_MAXINT timeout = conn->timeout;
   HB_MAXUINT timer = hb_timerInit(timeout);

   while( lRead < len )
   {
      l = hb_sockexRead(conn->sock, ptr + lRead, len - lRead, 1000);
      if( l > 0 )
      {
         lRead += l;
      }
      else if( l == 0 || hb_socketGetError() != HB_SOCKET_ERR_TIMEOUT || (timeout = hb_timerTest(timeout, &timer)) == 0 || hb_vmRequestQuery() != 0 )
      {
         break;
      }
   }

   return lRead;
}

static long s_fileRecvTest(PHB_CONCLI conn, void * buffer, long len)
{
   auto ptr = static_cast<HB_BYTE*>(buffer);
   long lRead = 0, l;
   HB_MAXINT timeout = 0;

   while( lRead < len )
   {
      l = hb_sockexRead(conn->sock, ptr + lRead, len - lRead, timeout);
      if( l <= 0 )
      {
         break;
      }
      lRead += l;
      timeout = conn->timeout;
   }

   return lRead;
}

static int s_fileGenSrvDataID(PHB_CONCLI conn)
{
   PHB_SRVDATA pSrvData = conn->srvdata;
   static int s_iStreamID = 0;

   if( ++s_iStreamID <= 0 )
   {
      s_iStreamID = 1;
   }

   while( pSrvData )
   {
      if( pSrvData->id == s_iStreamID )
      {
         if( ++s_iStreamID <= 0 )
         {
            s_iStreamID = 1;
         }
         pSrvData = conn->srvdata;
      }
      else
      {
         pSrvData = pSrvData->next;
      }
   }

   return s_iStreamID;
}

static PHB_SRVDATA s_fileFindSrvData(PHB_CONCLI conn, int iStreamID, int iType)
{
   PHB_SRVDATA pSrvData = conn->srvdata;

   while( pSrvData )
   {
      if( pSrvData->id == iStreamID )
      {
         return (iType == 0 || pSrvData->type == iType) ? pSrvData : nullptr;
      }
      pSrvData = pSrvData->next;
   }

   return nullptr;
}

static int s_fileNewSrvData(PHB_CONCLI conn, int iType)
{
   auto pSrvData = static_cast<PHB_SRVDATA>(hb_xgrabz(sizeof(HB_SRVDATA)));
   pSrvData->id = s_fileGenSrvDataID(conn);
   pSrvData->type = iType;

   if( iType == NETIO_SRVITEM )
   {
      pSrvData->maxsize = 4096;
   }
   else if( iType == NETIO_SRVDATA )
   {
      pSrvData->maxsize = 0x10000;
   }

   pSrvData->next = conn->srvdata;

   if( !conn->srvdata )
   {
      hb_atomic_inc(&conn->used);
   }

   conn->srvdata = pSrvData;

   return pSrvData->id;
}

static HB_BOOL s_fileCloseSrvData(PHB_CONCLI conn, int iStreamID)
{
   PHB_SRVDATA * pSrvDataPtr = &conn->srvdata;

   while( *pSrvDataPtr )
   {
      if( (*pSrvDataPtr)->id == iStreamID )
      {
         PHB_SRVDATA pSrvData = *pSrvDataPtr;
         *pSrvDataPtr = pSrvData->next;
         if( pSrvData->array )
         {
            hb_itemRelease(pSrvData->array);
         }
         if( pSrvData->data )
         {
            hb_xfree(pSrvData->data);
         }
         hb_xfree(pSrvData);
         if( !conn->srvdata )
         {
            hb_atomic_dec(&conn->used);
         }
         return true;
      }
      pSrvDataPtr = &(*pSrvDataPtr)->next;
   }

   return false;
}

static HB_BOOL s_fileRecvSrvData(PHB_CONCLI conn, long len, int iStreamID, int iType)
{
   auto buffer = static_cast<char*>(hb_xgrab(len));
   bool fResult = false;

   if( s_fileRecvAll(conn, buffer, len) == len )
   {
      PHB_SRVDATA pSrvData = s_fileFindSrvData(conn, iStreamID, iType);

      if( pSrvData )
      {
         if( pSrvData->size < pSrvData->maxsize )
         {
            if( iType == NETIO_SRVITEM )
            {
               HB_SIZE nSize = len;
               const char * data = buffer;
               PHB_ITEM pItem = hb_itemDeserialize(&data, &nSize);

               if( pItem != nullptr )
               {
                  if( nSize == 0 )
                  {
                     if( pSrvData->array == nullptr )
                     {
                        pSrvData->array = hb_itemArrayNew(0);
                     }
                     if( hb_arrayLen(pSrvData->array) < pSrvData->maxsize )
                     {
                        hb_arrayAddForward(pSrvData->array, pItem);
                     }
                  }
                  hb_itemRelease(pItem);
               }
            }
            else if( iType == NETIO_SRVDATA )
            {
               auto lmax = static_cast<long>(pSrvData->maxsize - pSrvData->size);

               if( len > lmax )
               {
                  len = lmax;
               }
               if( pSrvData->size + len > pSrvData->bufsize )
               {
                  pSrvData->bufsize = (pSrvData->size + len) << 1;
                  if( pSrvData->bufsize > pSrvData->maxsize )
                  {
                     pSrvData->bufsize = pSrvData->maxsize;
                  }
                  pSrvData->data = static_cast<char*>(hb_xrealloc(pSrvData->data, pSrvData->bufsize));
               }
               memcpy(pSrvData->data + pSrvData->size, buffer, len);
               pSrvData->size += len;
            }
         }
      }
      fResult = true;
   }
   else
   {
      conn->errcode = hb_socketGetError();
      hb_errRT_NETIO(EG_READ, 1001, conn->errcode, nullptr, HB_ERR_FUNCNAME);
   }
   hb_xfree(buffer);

   return fResult;
}

static HB_BOOL s_fileSendMsg(PHB_CONCLI conn, HB_BYTE * msgbuf, const void * data, long len, HB_BOOL fWait, HB_BOOL fNoError)
{
   HB_BYTE buffer[2048];
   HB_BYTE * msg, * ptr = nullptr;
   HB_LONG lSent = 0, l;
   bool fResult = false;

   if( len == 0 )
   {
      msg = msgbuf;
      len = NETIO_MSGLEN;
   }
   else
   {
      len += NETIO_MSGLEN;
      if( len > static_cast<long>(sizeof(buffer)) )
      {
         msg = ptr = static_cast<HB_BYTE*>(hb_xgrab(len));
      }
      else
      {
         msg = buffer;
      }
      memcpy(msg, msgbuf, NETIO_MSGLEN);
      memcpy(msg + NETIO_MSGLEN, data, len - NETIO_MSGLEN);
   }

   while( lSent < len )
   {
      l = hb_sockexWrite(conn->sock, msg + lSent, len - lSent, conn->timeout);
      if( l <= 0 )
      {
         break;
      }
      lSent += l;
   }

   if( ptr )
   {
      hb_xfree(ptr);
   }

   if( lSent == len )
   {
      if( hb_sockexFlush(conn->sock, conn->timeout, false) != 0 )
      {
         conn->errcode = hb_socketGetError();
         if( !fNoError )
         {
            hb_errRT_NETIO(EG_WRITE, 1002, conn->errcode, nullptr, HB_ERR_FUNCNAME);
         }
      }
      else if( fWait )
      {
         int iMsg = HB_GET_LE_INT32(msgbuf);

         for( ;; )
         {
            int iResult;

            if( s_fileRecvAll(conn, msgbuf, NETIO_MSGLEN) != NETIO_MSGLEN )
            {
               conn->errcode = hb_socketGetError();
               if( !fNoError )
               {
                  hb_errRT_NETIO(EG_READ, 1003, conn->errcode, nullptr, HB_ERR_FUNCNAME);
               }
               break;
            }

            iResult = HB_GET_LE_INT32(msgbuf);

            if( iResult == NETIO_SRVITEM || iResult == NETIO_SRVDATA )
            {
               int iStreamID = HB_GET_LE_UINT32(&msgbuf[4]);

               len = HB_GET_LE_INT32(&msgbuf[8]);
               if( len > 0 )
               {
                  if( !s_fileRecvSrvData(conn, len, iStreamID, iResult) )
                  {
                     break;
                  }
               }
            }
            else if( iResult != NETIO_SYNC )
            {
               if( iResult == NETIO_ERROR )
               {
                  conn->errcode = static_cast<HB_ERRCODE>(HB_GET_LE_UINT32(&msgbuf[4]));
                  hb_fsSetError(conn->errcode);
               }
               else if( iResult != iMsg )
               {
                  conn->errcode = NETIO_ERR_UNKNOWN_COMMAND;
                  if( !fNoError )
                  {
                     hb_errRT_NETIO(EG_UNSUPPORTED, 1004, 0, nullptr, HB_ERR_FUNCNAME);
                  }
               }
               else
               {
                  fResult = true;
               }
               break;
            }
         }
      }
      else
      {
         fResult = true;
      }
   }
   else
   {
      conn->errcode = hb_socketGetError();
      if( !fNoError )
      {
         hb_errRT_NETIO(EG_WRITE, 1005, conn->errcode, nullptr, HB_ERR_FUNCNAME);
      }
   }

   return fResult;
}

static HB_BOOL s_fileProcessData(PHB_CONCLI conn)
{
   HB_BYTE msgbuf[NETIO_MSGLEN];
   bool fResult = true;
   int iMsg, iStreamID;

   for( ;; )
   {
      long len = s_fileRecvTest(conn, msgbuf, NETIO_MSGLEN);

      if( len == NETIO_MSGLEN )
      {
         iMsg = HB_GET_LE_INT32(msgbuf);
         if( iMsg == NETIO_SRVITEM || iMsg == NETIO_SRVDATA )
         {
            iStreamID = HB_GET_LE_INT32(&msgbuf[4]);
            len = HB_GET_LE_INT32(&msgbuf[8]);
            if( len > 0 )
            {
               if( !s_fileRecvSrvData(conn, len, iStreamID, iMsg) )
               {
                  fResult = false;
                  break;
               }
            }
         }
         else if( iMsg == NETIO_ERROR )
         {
            conn->errcode = static_cast<HB_ERRCODE>(HB_GET_LE_UINT32(&msgbuf[4]));
            hb_fsSetError(conn->errcode);
         }
         else if( iMsg != NETIO_SYNC )
         {
            fResult = false;
            conn->errcode = NETIO_ERR_UNKNOWN_COMMAND;
            hb_errRT_NETIO(EG_UNSUPPORTED, 1006, 0, nullptr, HB_ERR_FUNCNAME);
            break;
         }
      }
      else
      {
         if( len != 0 )
         {
            fResult = false;
            conn->errcode = hb_socketGetError();
            hb_errRT_NETIO(EG_READ, 1007, conn->errcode, nullptr, HB_ERR_FUNCNAME);
         }
         break;
      }
   }

   return fResult;
}

static void s_fileConFree(PHB_CONCLI conn)
{
   if( conn->sock )
   {
      hb_sockexClose(conn->sock, true);
   }
   while( conn->srvdata )
   {
      PHB_SRVDATA pSrvData = conn->srvdata;
      conn->srvdata = pSrvData->next;
      if( pSrvData->array )
      {
         hb_itemRelease(pSrvData->array);
      }
      if( pSrvData->data )
      {
         hb_xfree(pSrvData->data);
      }
      hb_xfree(pSrvData);
   }
   if( conn->mutex )
   {
      hb_itemRelease(conn->mutex);
   }
   if( conn->path )
   {
      hb_atomic_dec(&s_pathCount);
      hb_xfree(conn->path);
   }
   hb_xfree(conn);
}

static PHB_CONCLI s_fileConNew(HB_SOCKET sd, const char * pszServer, int iPort, int iTimeOut, const char * pszPasswd, int iPassLen, int iLevel, int iStrategy)
{
   auto iLen = static_cast<int>(strlen(pszServer));
   auto conn = static_cast<PHB_CONCLI>(hb_xgrab(sizeof(HB_CONCLI) + iLen));
   hb_atomic_set(&conn->used, 1);
   hb_atomic_set(&conn->usrcount, 0);
   conn->mutex = hb_threadMutexCreate();
   conn->errcode = 0;
   conn->srvdata = nullptr;
   conn->next = nullptr;
   conn->path = nullptr;
   conn->timeout = iTimeOut;
   conn->port = iPort;
   memcpy(conn->server, pszServer, iLen + 1);
   conn->level = iLevel;
   conn->strategy = iStrategy;
   conn->passlen = iPassLen;
   if( iPassLen )
   {
      memcpy(conn->passwd, pszPasswd, iPassLen);
   }

   if( iLevel == HB_ZLIB_COMPRESSION_DISABLE )
   {
      conn->sock = hb_sockexNew(sd, nullptr, nullptr);
   }
   else
   {
      conn->sock = hb_sockexNewZNet(sd, pszPasswd, iPassLen, iLevel, iStrategy);
   }

   if( conn->sock == nullptr )
   {
      s_fileConFree(conn);
      conn = nullptr;
   }
   else
   {
      hb_sockexSetShutDown(conn->sock, true);
   }

   return conn;
}

static void s_fileConRegister(PHB_CONCLI conn)
{
   PHB_CONCLI * connPtr;

   HB_NETIO_LOCK();
   connPtr = &s_connections;
   while( *connPtr )
   {
      connPtr = &(*connPtr)->next;
   }
   *connPtr = conn;
   HB_NETIO_UNLOCK();
}

static void s_fileConClose(PHB_CONCLI conn)
{
   if( hb_atomic_dec(&conn->used) )
   {
      HB_NETIO_LOCK();
      if( hb_atomic_get(&conn->used) == 0 )
      {
         PHB_CONCLI * connPtr = &s_connections;
         while( *connPtr )
         {
            if( *connPtr == conn )
            {
               *connPtr = conn->next;
               break;
            }
            connPtr = &(*connPtr)->next;
         }
      }
      else
      {
         conn = nullptr;   /* reused by other thread */
      }
      HB_NETIO_UNLOCK();

      if( conn )
      {
         s_fileConFree(conn);
      }
   }
}

static PHB_CONCLI s_fileConFind(const char * pszServer, int iPort)
{
   PHB_CONCLI conn;

   HB_NETIO_LOCK();
   conn = s_connections;
   while( conn )
   {
      if( conn->port == iPort && hb_stricmp(conn->server, pszServer) == 0 )
      {
         hb_atomic_inc(&conn->used);
         break;
      }
      conn = conn->next;
   }
   HB_NETIO_UNLOCK();

   return conn;
}

static HB_BOOL s_fileUsrDisconnect(const char * pszServer, int iPort)
{
   PHB_CONCLI conn, connClose = nullptr;

   HB_NETIO_LOCK();
   conn = s_connections;
   while( conn )
   {
      if( conn->port == iPort && hb_stricmp(conn->server, pszServer) == 0 )
      {
         if( hb_atomic_get(&conn->usrcount) )
         {
            if( hb_atomic_dec(&conn->usrcount) )
            {
               connClose = conn;
            }
         }
         break;
      }
      conn = conn->next;
   }
   HB_NETIO_UNLOCK();

   if( connClose )
   {
      s_fileConClose(connClose);
   }

   return conn != nullptr;
}

static void s_fileUsrConnect(PHB_CONCLI conn)
{
   hb_atomic_inc(&conn->usrcount);
}

static HB_BOOL s_fileConLock(PHB_CONCLI conn)
{
   return !conn->mutex || hb_threadMutexLock(conn->mutex);
}

static void s_fileConUnlock(PHB_CONCLI conn)
{
   if( conn->mutex )
   {
      hb_threadMutexUnlock(conn->mutex);
   }
}

static PHB_CONCLI s_fileNameConFind(const char ** pFileName, HB_BOOL fLock)
{
   PHB_CONCLI conn = nullptr;

   if( s_pathCount > 0 )
   {
      HB_NETIO_LOCK();
      conn = s_connections;
      while( conn )
      {
         if( conn->path )
         {
            auto iLen = static_cast<int>(strlen(conn->path));
#ifdef HB_OS_UNIX
            if( strncmp(*pFileName, conn->path, iLen) == 0 )
#else
            if( hb_strnicmp(*pFileName, conn->path, iLen) == 0 )
#endif
            {
               if( fLock )
               {
                  hb_atomic_inc(&conn->used);
               }
               *pFileName += iLen;
               break;
            }
         }
         conn = conn->next;
      }
      HB_NETIO_UNLOCK();
   }

   return conn;
}


static void s_fileGetConnParam(const char ** pszServer, int * piPort, int * piTimeOut, const char ** pszPasswd, int * piPassLen)
{
   auto pConData = static_cast<PHB_CONDATA>(hb_stackTestTSD(&s_conData));

   if( pConData == nullptr )
   {
      pConData = &s_defaultConn;
   }

   if( *pszServer == nullptr )
   {
      *pszServer = pConData->server;
   }
   if( *piPort == 0 )
   {
      *piPort = pConData->port;
   }
   if( piTimeOut && (*piTimeOut == 0 || *piTimeOut < -1) )
   {
      *piTimeOut = pConData->timeout;
   }
   if( piPassLen && *piPassLen == 0 && pConData->passlen != 0 )
   {
      *piPassLen = pConData->passlen;
      *pszPasswd = pConData->passwd;
   }
}

static const char * s_fileDecode(const char * pszFileName, char * buffer, const char ** pServer, int * piPort, int * piTimeOut,
                                 const char ** pPasswd, int * piPassLen, int * piLevel, int * piStrategy)
{
   HB_SYMBOL_UNUSED(piTimeOut);
   HB_SYMBOL_UNUSED(piLevel);
   HB_SYMBOL_UNUSED(piStrategy);

   if( pszFileName )
   {
      /* decode server address and port if given as part of file name
       * in format like:
       *          "example.org:2941:path/to/file"
       * or:
       *          "example.org:2941:passwd:path/to/file"
       * or:
       *          "//example.org:2941/path/to/file"
       */
      const char * psz, * pth = nullptr;

      if( (pszFileName[0] == '/' || pszFileName[0] == '\\') && pszFileName[0] == pszFileName[1] )
      {
         pszFileName += 2;
         pth = strchr(pszFileName, '/');
         psz = strchr(pszFileName, '\\');
         if( !pth || (psz && psz < pth) )
         {
            pth = psz;
            if( !pth )
            {
               pth = pszFileName + strlen(pszFileName);
            }
         }
      }

      psz = strchr(pszFileName, ':');
      if( pth && (!psz || pth < psz) )
      {
         psz = pth;
      }

      if( psz )
      {
         auto iLen = static_cast<int>(psz - pszFileName);

         if( pth || iLen == 0 || iLen > 1 )
         {
            if( iLen >= NETIO_SERVERNAME_MAX )
            {
               iLen = NETIO_SERVERNAME_MAX - 1;
            }
            if( iLen > 0 )
            {
               hb_strncpy(buffer, pszFileName, iLen);
               *pServer = buffer;
            }
            pszFileName = psz + 1;
            if( !pth || psz < pth )
            {
               char port_buf[10], c;

               iLen = 0;
               while( HB_ISDIGIT(pszFileName[iLen]) && iLen < static_cast<int>(sizeof(port_buf)) - 1 )
               {
                  port_buf[iLen] = pszFileName[iLen];
                  ++iLen;
               }
               c = pszFileName[iLen];
               if( c == ':' || c == '/' || c == '\\' )
               {
                  if( iLen > 0 )
                  {
                     int iOverflow;
                     HB_MAXINT llPort;

                     port_buf[iLen] = '\0';
                     llPort = hb_strValInt(port_buf, &iOverflow);

                     if( !iOverflow && llPort > 0 && llPort < 0x10000 )
                     {
                        pszFileName += iLen;
                        *piPort = static_cast<int>(llPort);
                     }
                  }
                  if( c == ':' )
                  {
                     ++pszFileName;
                     iLen = 0;
                     while( pszFileName[iLen] && pszFileName[iLen] != ':' )
                     {
                        ++iLen;
                     }
                     if( pszFileName[iLen] == ':' )
                     {
                        if( pPasswd )
                        {
                           *pPasswd = pszFileName;
                        }
                        pszFileName += iLen + 1;
                        if( iLen > NETIO_PASSWD_MAX )
                        {
                           iLen = NETIO_PASSWD_MAX;
                        }
                        if( piPassLen )
                        {
                           *piPassLen = iLen;
                        }   
                     }
                  }
               }
            }
         }
      }
   }

   return pszFileName;
}

static PHB_CONCLI s_fileConnCheck(PHB_CONCLI conn, const char ** pFileName, HB_BOOL fDefault, HB_BOOL * pfResult)
{
   const char * pszFileName = *pFileName;

   *pfResult = HB_TRUE;
   if( hb_strnicmp(pszFileName, NETIO_FILE_PREFIX, NETIO_FILE_PREFIX_LEN) == 0 )
   {
      char server[NETIO_SERVERNAME_MAX];
      const char * pszServer = nullptr;
      int iPort = 0;

      if( !fDefault )
      {
         pszServer = conn->server;
         iPort = conn->port;
      }
      else
      {
         s_fileGetConnParam(&pszServer, &iPort, nullptr, nullptr, nullptr);
      }

      pszFileName = s_fileDecode(pszFileName + NETIO_FILE_PREFIX_LEN, server, &pszServer, &iPort, nullptr, nullptr, nullptr, nullptr, nullptr);
      if( iPort != conn->port )
      {
         *pfResult = HB_FALSE;
      }
      else
      {
         if( pszServer != conn->server && hb_stricmp(pszServer, conn->server) != 0 )
         {
            char * pszIpAddres = hb_socketResolveAddr(pszServer, HB_SOCKET_AF_INET);
            if( pszIpAddres == nullptr || hb_stricmp(pszIpAddres, conn->server) != 0 )
            {
               *pfResult = HB_FALSE;
               conn = nullptr;
            }
            if( pszIpAddres )
            {
               hb_xfree(pszIpAddres);
            }
         }
         if( conn )
         {
            *pFileName = pszFileName;
         }
         return conn;
      }
   }
   else if( s_pathCount > 0 && conn->path )
   {
      HB_NETIO_LOCK();
      if( conn->path )
      {
         auto iLen = static_cast<int>(strlen(conn->path));
#ifdef HB_OS_UNIX
         if( strncmp(*pFileName, conn->path, iLen) == 0 )
#else
         if( hb_strnicmp(*pFileName, conn->path, iLen) == 0 )
#endif
         {
            *pFileName += iLen;
         }
      }
      HB_NETIO_LOCK();
      if( pszFileName != *pFileName )
      {
         return conn;
      }
   }

   return nullptr;
}

static PHB_CONCLI s_fileConnect(const char ** pFileName, const char * pszServer, int iPort, int iTimeOut, HB_BOOL fNoError,
                                const char * pszPasswd, int iPassLen, int iLevel, int iStrategy)
{
   PHB_CONCLI conn = nullptr;

   if( pFileName )
   {
      if( hb_strnicmp(*pFileName, NETIO_FILE_PREFIX, NETIO_FILE_PREFIX_LEN) == 0 )
      {
         *pFileName += NETIO_FILE_PREFIX_LEN;
      }
      else
      {
         conn = s_fileNameConFind(pFileName, true);
      }
   }

   if( conn == nullptr )
   {
      char server[NETIO_SERVERNAME_MAX];
      char * pszIpAddres;

      s_fileGetConnParam(&pszServer, &iPort, &iTimeOut, &pszPasswd, &iPassLen);

      if( pFileName )
      {
         *pFileName = s_fileDecode(*pFileName, server, &pszServer, &iPort, &iTimeOut, &pszPasswd, &iPassLen, &iLevel, &iStrategy);
      }

      if( iLevel == HB_ZLIB_COMPRESSION_DISABLE && iPassLen )
      {
         iLevel = HB_ZLIB_COMPRESSION_DEFAULT;
      }

      pszIpAddres = hb_socketResolveAddr(pszServer, HB_SOCKET_AF_INET);
      if( pszIpAddres == nullptr )
      {
         return nullptr;
      }

      conn = s_fileConFind(pszIpAddres, iPort);
      if( conn == nullptr )
      {
         HB_SOCKET sd = hb_socketOpen(HB_SOCKET_PF_INET, HB_SOCKET_PT_STREAM, 0);
         if( sd != HB_NO_SOCKET )
         {
            void * pSockAddr;
            unsigned uiLen;

            if( hb_socketInetAddr(&pSockAddr, &uiLen, pszIpAddres, iPort) )
            {
               hb_socketSetKeepAlive(sd, true);
               if( hb_socketConnect(sd, pSockAddr, uiLen, iTimeOut) == 0 )
               {
                  hb_socketSetNoDelay(sd, true);
                  conn = s_fileConNew(sd, pszIpAddres, iPort, iTimeOut, pszPasswd, iPassLen, iLevel, iStrategy);
                  if( conn )
                  {
                     HB_BYTE msgbuf[NETIO_MSGLEN];
                     auto len = static_cast<HB_U16>(strlen(NETIO_LOGINSTRID));

                     HB_PUT_LE_UINT32(&msgbuf[0], NETIO_LOGIN);
                     HB_PUT_LE_UINT16(&msgbuf[4], len);
                     memset(msgbuf + 6, '\0', sizeof(msgbuf) - 6);

                     if( !s_fileSendMsg(conn, msgbuf, NETIO_LOGINSTRID, len, true, fNoError) || HB_GET_LE_UINT32(&msgbuf[4]) != NETIO_CONNECTED )
                     {
                        s_fileConFree(conn);
                        conn = nullptr;
                     }
                     else
                     {
                        s_fileConRegister(conn);
                     }

                     sd = HB_NO_SOCKET;
                  }
               }
               hb_xfree(pSockAddr);
            }
            if( sd != HB_NO_SOCKET )
            {
               hb_socketClose(sd);
            }
         }
      }
      hb_xfree(pszIpAddres);
   }

   if( conn != nullptr && s_defaultInit )
   {
      HB_NETIO_LOCK();
      if( s_defaultInit )
      {
         hb_strncpy(s_defaultConn.server, conn->server, sizeof(s_defaultConn.server) - 1);
         s_defaultConn.port = iPort;
         s_defaultConn.timeout = iTimeOut;
         s_defaultConn.level = iLevel;
         s_defaultConn.strategy = iStrategy;
         s_defaultConn.passlen = iPassLen;
         if( iPassLen )
         {
            memcpy(s_defaultConn.passwd, pszPasswd, iPassLen);
         }
         s_defaultInit = false;
      }
      HB_NETIO_UNLOCK();
   }

   return conn;
}

static void s_netio_exit(void * cargo)
{
   HB_SYMBOL_UNUSED(cargo);

   while( s_connections )
   {
      PHB_CONCLI conn = s_connections;
      s_connections = conn->next;
      s_fileConFree(conn);
   }

   if( !s_fInit )
   {
      hb_socketCleanup();
      s_fInit = true;
   }
}

static void s_netio_init(void * cargo)
{
   HB_SYMBOL_UNUSED(cargo);

   if( s_fInit )
   {
      hb_socketInit();
      hb_fileRegisterFull(s_fileMethods());
      hb_vmAtQuit(s_netio_exit, nullptr);
      s_fInit = false;
   }
}

/* netio_Decode([@]<cFullName>, [@<cServer>], [@<nPort>], [@<nTimeOut>], ;
 *              [@<cPasswd>], [@<nCompressionLevel>], [@<nStrategy>]) --> <lOK>
 */
HB_FUNC(NETIO_DECODE)
{
   char server[NETIO_SERVERNAME_MAX];
   auto pszFullName = hb_parc(1);
   const char * pszFile;
   auto pszServer = hb_parc(2);
   auto iPort = hb_parni(3);
   auto iTimeOut = hb_parni(4);
   auto pszPasswd = hb_parc(5);
   auto iPassLen = static_cast<int>(hb_parclen(5));
   auto iLevel = hb_parnidef(6, HB_ZLIB_COMPRESSION_DISABLE);
   auto iStrategy = hb_parnidef(7, HB_ZLIB_STRATEGY_DEFAULT);

   s_fileGetConnParam(&pszServer, &iPort, &iTimeOut, &pszPasswd, &iPassLen);

   if( pszFullName )
   {
      if( hb_strnicmp(pszFullName, NETIO_FILE_PREFIX, NETIO_FILE_PREFIX_LEN) == 0 )
      {
         pszFullName += NETIO_FILE_PREFIX_LEN;
      }
      pszFile = s_fileDecode(pszFullName, server, &pszServer, &iPort, &iTimeOut, &pszPasswd, &iPassLen, &iLevel, &iStrategy);
   }
   else
   {
      pszFile = nullptr;
   }

   if( iLevel == HB_ZLIB_COMPRESSION_DISABLE && iPassLen )
   {
      iLevel = HB_ZLIB_COMPRESSION_DEFAULT;
   }

   hb_storc(pszServer, 2);
   hb_storni(iPort, 3);
   hb_storni(iTimeOut, 4);
   hb_storclen(pszPasswd, iPassLen, 5);
   hb_storni(iLevel, 6);
   hb_storni(iStrategy, 7);
   if( pszFile != pszFullName )
   {
      /* the order is important and 1st parameter
       * should be assigned at the end
       */
      hb_storc(pszFile, 1);
   }

   hb_retl(pszFile != pszFullName);
}

/* netio_Connect([<cServer>], [<nPort>], [<nTimeOut>], ;
 *               [<cPasswd>], [<nCompressionLevel>], [<nStrategy>]) --> <lOK>
 */
HB_FUNC(NETIO_CONNECT)
{
   auto pszServer = hb_parc(1);
   auto pszPasswd = hb_parc(4);
   auto iPort = hb_parni(2);
   auto iTimeOut = hb_parni(3);
   auto iPassLen = static_cast<int>(hb_parclen(4));
   auto iLevel = hb_parnidef(5, HB_ZLIB_COMPRESSION_DISABLE);
   auto iStrategy = hb_parnidef(6, HB_ZLIB_STRATEGY_DEFAULT);

   if( iPassLen > NETIO_PASSWD_MAX )
   {
      iPassLen = NETIO_PASSWD_MAX;
   }

   s_netio_init(nullptr);

   PHB_CONCLI conn = s_fileConnect(nullptr, pszServer, iPort, iTimeOut, true, pszPasswd, iPassLen, iLevel, iStrategy);
   if( conn )
   {
      auto pConData = static_cast<PHB_CONDATA>(hb_stackGetTSD(&s_conData));

      pConData->timeout = conn->timeout;
      pConData->port = conn->port;
      hb_strncpy(pConData->server, conn->server, sizeof(pConData->server) - 1);
      pConData->level = conn->level;
      pConData->strategy = conn->strategy;
      pConData->passlen = conn->passlen;
      if( conn->passlen )
      {
         memcpy(pConData->passwd, conn->passwd, conn->passlen);
      }

      s_fileUsrConnect(conn);
   }

   hb_retl(conn != nullptr);
}

static HB_GARBAGE_FUNC(s_concli_destructor)
{
   auto conn_ptr = static_cast<PHB_CONCLI*>(Cargo);

   if( *conn_ptr )
   {
      s_fileConClose(*conn_ptr);
      *conn_ptr = nullptr;
   }
}

static const HB_GC_FUNCS s_gcConCliFuncs =
{
   s_concli_destructor,
   hb_gcDummyMark
};

static PHB_CONCLI s_connParam(int iParam)
{
   auto conn_ptr = static_cast<PHB_CONCLI*>(hb_parptrGC(&s_gcConCliFuncs, iParam));

   if( conn_ptr )
   {
      PHB_CONCLI conn = *conn_ptr;
      if( conn )
      {
         hb_atomic_inc(&conn->used);
         return conn;
      }
   }
   return nullptr;
}

/* netio_GetConnection([<cServer>], [<nPort>], [<nTimeOut>], ;
 *                     [<cPasswd>], [<nCompressionLevel>], [<nStrategy>])
 *       --> <pConnection> | NIL
 */
HB_FUNC(NETIO_GETCONNECTION)
{
   auto pszServer = hb_parc(1);
   auto pszPasswd = hb_parc(4);
   auto iPort = hb_parni(2);
   auto iTimeOut = hb_parni(3);
   auto iPassLen = static_cast<int>(hb_parclen(4));
   auto iLevel = hb_parnidef(5, HB_ZLIB_COMPRESSION_DISABLE);
   auto iStrategy = hb_parnidef(6, HB_ZLIB_STRATEGY_DEFAULT);

   if( iPassLen > NETIO_PASSWD_MAX )
   {
      iPassLen = NETIO_PASSWD_MAX;
   }

   s_netio_init(nullptr);

   PHB_CONCLI conn = s_fileConnect(nullptr, pszServer, iPort, iTimeOut, true, pszPasswd, iPassLen, iLevel, iStrategy);
   if( conn )
   {
      auto conn_ptr = static_cast<PHB_CONCLI*>(hb_gcAllocate(sizeof(PHB_CONCLI), &s_gcConCliFuncs));
      *conn_ptr = conn;
      hb_retptrGC(conn_ptr);
   }
}

/* netio_Disconnect([<cServer>], [<nPort>]) --> <lOK>
 */
HB_FUNC(NETIO_DISCONNECT)
{
   auto pszServer = hb_parc(1);
   char * pszIpAddres;
   auto iPort = hb_parni(2);
   bool fDisconnected = false;

   s_fileGetConnParam(&pszServer, &iPort, nullptr, nullptr, nullptr);
   pszIpAddres = hb_socketResolveAddr(pszServer, HB_SOCKET_AF_INET);
   if( pszIpAddres != nullptr )
   {
      fDisconnected = s_fileUsrDisconnect(pszIpAddres, iPort);
      hb_xfree(pszIpAddres);
   }
   hb_retl(fDisconnected);
}

/* netio_TimeOut(<pConnection> [, <nTimeOut>]) --> [<nTimeOut>]
 */
HB_FUNC(NETIO_TIMEOUT)
{
   PHB_CONCLI conn = s_connParam(1);

   if( conn )
   {
      if( s_fileConLock(conn) )
      {
         hb_retni(conn->timeout);
         if( HB_ISNUM(2) )
         {
            conn->timeout = hb_parni(2);
         }
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC(NETIO_SETPATH)
{
   PHB_CONCLI conn = s_connParam(1);

   if( conn )
   {
      if( s_fileConLock(conn) )
      {
         auto pszNewPath = hb_parc(2);
         char * pszSetPath = nullptr, * pszOldPath = nullptr;

         if( pszNewPath && *pszNewPath )
         {
            pszSetPath = hb_strdup(pszNewPath);
         }

         HB_NETIO_LOCK();
         if( pszNewPath )
         {
            if( conn->path )
            {
               hb_atomic_dec(&s_pathCount);
               pszOldPath = conn->path;
               conn->path = nullptr;
            }
            if( pszSetPath )
            {
               conn->path = pszSetPath;
               hb_atomic_inc(&s_pathCount);
            }
         }
         else if( conn->path )
         {
            pszOldPath = hb_strdup(conn->path);
         }
         HB_NETIO_UNLOCK();
         s_fileConUnlock(conn);
         hb_retc_buffer(pszOldPath);
      }
      s_fileConClose(conn);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

static const char * s_netio_params(int iParam, int iMsg, const char * pszName, HB_U32 * pSize, char ** pFree)
{
   int iPCount = iMsg == NETIO_PROCIS ? 0 : hb_pcount();
   char * data = nullptr;
   HB_SIZE itmSize;

   HB_SIZE size = strlen(pszName) + 1;

   while( ++iParam <= iPCount )
   {
      char * itmData = hb_itemSerialize(hb_param(iParam, Harbour::Item::ANY), HB_SERIALIZE_NUMSIZE, &itmSize);
      if( data == nullptr )
      {
         data = static_cast<char*>(memcpy(hb_xgrab(size + itmSize), pszName, size));
      }
      else
      {
         data = static_cast<char*>(hb_xrealloc(data, size + itmSize));
      }
      memcpy(data + size, itmData, itmSize);
      size += itmSize;
      hb_xfree(itmData);
   }

   *pFree = data;
   *pSize = static_cast<HB_U32>(size);

   return data ? data : pszName;
}

static HB_BOOL s_netio_procexec(int iMsg, int iType)
{
   bool fResult = false;
   int iParam = 1;

   PHB_CONCLI conn = s_connParam(1);
   if( conn )
   {
      ++iParam;
   }
   auto pszProcName = hb_parc(iParam);
   if( pszProcName )
   {
      if( !conn )
      {
         conn = s_fileConnect(&pszProcName, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
      }
      if( conn )
      {
         if( s_fileConLock(conn) )
         {
            HB_BYTE msgbuf[NETIO_MSGLEN];
            char * buffer;
            HB_U32 size;
            int iStreamID = 0;

            const char * data = s_netio_params(iParam, iMsg, pszProcName, &size, &buffer);
            HB_PUT_LE_UINT32(&msgbuf[0], iMsg);
            HB_PUT_LE_UINT32(&msgbuf[4], size);
            if( iMsg == NETIO_FUNCCTRL )
            {
               iStreamID = s_fileNewSrvData(conn, iType);
               HB_PUT_LE_UINT32(&msgbuf[8], iStreamID);
               HB_PUT_LE_UINT32(&msgbuf[12], iType);
               memset(msgbuf + 16, '\0', sizeof(msgbuf) - 16);
            }
            else
            {
               memset(msgbuf + 8, '\0', sizeof(msgbuf) - 8);
            }
            fResult = s_fileSendMsg(conn, msgbuf, data, size, iMsg != NETIO_PROC, false);
            if( fResult && (iMsg == NETIO_FUNC || iMsg == NETIO_FUNCCTRL) )
            {
               HB_SIZE nResult = HB_GET_LE_UINT32(&msgbuf[4]);

               if( nResult > 0 )
               {
                  PHB_ITEM pItem = nullptr;
                  HB_SIZE nRecv;

                  if( nResult > size && buffer )
                  {
                     hb_xfree(buffer);
                     buffer = nullptr;
                  }
                  if( buffer == nullptr )
                  {
                     buffer = static_cast<char*>(hb_xgrab(nResult));
                  }
                  nRecv = s_fileRecvAll(conn, buffer, static_cast<long>(nResult));
                  if( nResult == nRecv )
                  {
                     data = buffer;
                     pItem = hb_itemDeserialize(&data, &nResult);
                  }
                  if( pItem != nullptr )
                  {
                     if( iMsg == NETIO_FUNCCTRL )
                     {
                        if( iStreamID == hb_itemGetNI(pItem) )
                        {
                           iStreamID = 0;
                        }
                        else
                        {
                           hb_itemPutNI(pItem, -1);
                        }
                     }
                     hb_itemReturnRelease(pItem);
                  }
                  else
                  {
                     HB_ERRCODE errOsCode = 0;

                     if( nResult != nRecv )
                     {
                        conn->errcode = errOsCode = hb_socketGetError();
                     }
                     else
                     {
                        conn->errcode = NETIO_ERR_WRONG_PARAM;
                     }

                     hb_errRT_NETIO(EG_CORRUPTION, 1008, errOsCode, nullptr, HB_ERR_FUNCNAME);
                  }
               }
            }
            if( iStreamID != 0 )
            {
               s_fileCloseSrvData(conn, iStreamID);
            }
            if( buffer )
            {
               hb_xfree(buffer);
            }
            s_fileConUnlock(conn);
         }
      }
   }

   if( conn )
   {
      s_fileConClose(conn);
   }

   return fResult;
}

/* check if function/procedure exists on the server side:
 *
 * netio_ProcExists( <cProcName> ) --> <lExists>
 */
HB_FUNC(NETIO_PROCEXISTS)
{
   hb_retl(s_netio_procexec(NETIO_PROCIS, 0));
}

/* execute function/procedure on server the side,
 * do not wait for confirmation:
 *
 * netio_ProcExec(<cProcName> [, <params,...>]) --> <lSent>
 */
HB_FUNC(NETIO_PROCEXEC)
{
   hb_retl(s_netio_procexec(NETIO_PROC, 0));
}

/* execute function/procedure on the server side and wait for
 * confirmation:
 *
 * netio_ProcExecW( <cProcName> [, <params,...>] ) --> <lExecuted>
 */
HB_FUNC(NETIO_PROCEXECW)
{
   hb_retl(s_netio_procexec(NETIO_PROCW, 0));
}

/* execute function on the server side and wait for its return value:
 *
 * netio_FuncExec(<cFuncName> [, <params,...>]) --> <xFuncRetVal>
 */
HB_FUNC(NETIO_FUNCEXEC)
{
   s_netio_procexec(NETIO_FUNC, 0);
}

/* open communication stream/channel which allow to send data
 * asynchronously from server to client:
 *
 * netio_OpenDataStream( <cStreamFuncName> [, <params,...>] ) --> <nStreamID>
 *
 * it executes on the server side:
 *    <cStreamFuncName>( <pConnSock>, <nStreamID> [, <params,...>] )
 * and then check value returned by above function. If it's equal to
 * <nStreamID> then the communication stream is opened and <nStreamID>
 * is returned to the client.
 * The function returns new stream ID or -1 if the communication stream
 * cannot be set.
 */
HB_FUNC(NETIO_OPENDATASTREAM)
{
   s_netio_procexec(NETIO_FUNCCTRL, NETIO_SRVDATA);
}

/* open communication stream/channel which allow to send data
 * asynchronously from server to client:
 *
 * netio_OpenItemStream( <cStreamFuncName> [, <params,...>] ) --> <nStreamID>
 *
 * it executes on the server side:
 *    <cStreamFuncName>( <pConnSock>, <nStreamID> [, <params,...>] )
 * and then check value returned by above function. If it's equal to
 * <nStreamID> then the communication stream is opened and <nStreamID>
 * is returned to the client.
 * The function returns new stream ID or -1 if the communication stream
 * cannot be set.
 */
HB_FUNC(NETIO_OPENITEMSTREAM)
{
   s_netio_procexec(NETIO_FUNCCTRL, NETIO_SRVITEM);
}

static PHB_CONCLI s_netio_getConn(void)
{
   PHB_CONCLI conn = s_connParam(2);

   if( !conn )
   {
      auto pszServer = hb_parc(2);
      auto iPort = hb_parni(3);

      s_fileGetConnParam(&pszServer, &iPort, nullptr, nullptr, nullptr);
      char * pszIpAddres = hb_socketResolveAddr(pszServer, HB_SOCKET_AF_INET);
      if( pszIpAddres != nullptr )
      {
         conn = s_fileConFind(pszIpAddres, iPort);
         hb_xfree(pszIpAddres);
      }
   }

   return conn;
}

/* close communication stream/channel:
 *
 * netio_CloseStream( <nStreamID>, [<pConnection>] | [[<cServer>], [<nPort>]] )
 *    --> <lOK>
 */
HB_FUNC(NETIO_CLOSESTREAM)
{
   auto iStreamID = hb_parni(1);
   bool fResult = false;

   if( iStreamID )
   {
      PHB_CONCLI conn = s_netio_getConn();

      if( conn )
      {
         if( s_fileConLock(conn) )
         {
            fResult = s_fileCloseSrvData(conn, iStreamID);
            if( fResult )
            {
               HB_BYTE msgbuf[NETIO_MSGLEN];

               HB_PUT_LE_UINT32(&msgbuf[0], NETIO_SRVCLOSE);
               HB_PUT_LE_UINT32(&msgbuf[4], iStreamID);
               memset(msgbuf + 8, '\0', sizeof(msgbuf) - 8);
               s_fileSendMsg(conn, msgbuf, nullptr, 0, true, false);
            }
            s_fileConUnlock(conn);
         }
         s_fileConClose(conn);
      }
   }
   hb_retl(fResult);
}

/* retrieve data sent from the server by cominication stream
 *
 * netio_GetData(<nStreamID>, [<pConnection>] | [[<cServer>], [<nPort>]])
 *    --> <aData> | <cData> | NIL
 */
HB_FUNC(NETIO_GETDATA)
{
   auto iStreamID = hb_parni(1);

   if( iStreamID )
   {
      PHB_CONCLI conn = s_netio_getConn();

      if( conn )
      {
         if( s_fileConLock(conn) )
         {
            if( s_fileProcessData(conn) )
            {
               PHB_SRVDATA pSrvData = s_fileFindSrvData(conn, iStreamID, 0);
               if( pSrvData )
               {
                  if( pSrvData->type == NETIO_SRVITEM )
                  {
                     if( pSrvData->array )
                     {
                        hb_itemReturnForward(pSrvData->array);
                        hb_arrayNew(pSrvData->array, 0);
                     }
                     else
                     {
                        hb_reta(0);
                     }
                  }
                  else if( pSrvData->type == NETIO_SRVDATA )
                  {
                     hb_retclen(pSrvData->data, pSrvData->size);
                     pSrvData->size = 0;
                  }
               }
            }
            s_fileConUnlock(conn);
         }
         s_fileConClose(conn);
      }
   }
}

/* Client methods
 */
static HB_BOOL s_fileAccept(PHB_FILE_FUNCS pFuncs, const char * pszFileName)
{
   HB_SYMBOL_UNUSED(pFuncs);

   return hb_strnicmp(pszFileName, NETIO_FILE_PREFIX, NETIO_FILE_PREFIX_LEN) == 0 || s_fileNameConFind(&pszFileName, false) != nullptr;
}

static HB_BOOL s_fileDirExists(PHB_FILE_FUNCS pFuncs, const char * pszDirName)
{
   bool fResult = false;

   HB_SYMBOL_UNUSED(pFuncs);

   PHB_CONCLI conn = s_fileConnect(&pszDirName, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len = static_cast<HB_U16>(strlen(pszDirName));

         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_DIREXISTS);
         HB_PUT_LE_UINT16(&msgbuf[4], len);
         memset(msgbuf + 6, '\0', sizeof(msgbuf) - 6);
         fResult = s_fileSendMsg(conn, msgbuf, pszDirName, len, true, false);
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   return fResult;
}

static HB_BOOL s_fileDirMake(PHB_FILE_FUNCS pFuncs, const char * pszDirName)
{
   bool fResult = false;

   HB_SYMBOL_UNUSED(pFuncs);

   PHB_CONCLI conn = s_fileConnect(&pszDirName, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len = static_cast<HB_U16>(strlen(pszDirName));

         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_DIRMAKE);
         HB_PUT_LE_UINT16(&msgbuf[4], len);
         memset(msgbuf + 6, '\0', sizeof(msgbuf) - 6);
         fResult = s_fileSendMsg(conn, msgbuf, pszDirName, len, true, false);
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   return fResult;
}

static HB_BOOL s_fileDirRemove(PHB_FILE_FUNCS pFuncs, const char * pszDirName)
{
   bool fResult = false;

   HB_SYMBOL_UNUSED(pFuncs);

   PHB_CONCLI conn = s_fileConnect(&pszDirName, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len = static_cast<HB_U16>(strlen(pszDirName));

         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_DIRREMOVE);
         HB_PUT_LE_UINT16(&msgbuf[4], len);
         memset(msgbuf + 6, '\0', sizeof(msgbuf) - 6);
         fResult = s_fileSendMsg(conn, msgbuf, pszDirName, len, true, false);
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   return fResult;
}

static double s_fileDirSpace(PHB_FILE_FUNCS pFuncs, const char * pszDirName, HB_USHORT uiType)
{
   double dResult = 0.0;

   HB_SYMBOL_UNUSED(pFuncs);

   PHB_CONCLI conn = s_fileConnect(&pszDirName, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len = static_cast<HB_U16>(strlen(pszDirName));

         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_DIRSPACE);
         HB_PUT_LE_UINT16(&msgbuf[4], len);
         HB_PUT_LE_UINT16(&msgbuf[6], uiType);
         memset(msgbuf + 8, '\0', sizeof(msgbuf) - 8);
         if( s_fileSendMsg(conn, msgbuf, pszDirName, len, true, false) )
         {
            dResult = static_cast<double>(HB_GET_LE_UINT64(&msgbuf[4]));
            hb_fsSetError(static_cast<HB_ERRCODE>(HB_GET_LE_UINT32(&msgbuf[12])));
         }
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   return dResult;
}

static PHB_ITEM s_fileDirectory(PHB_FILE_FUNCS pFuncs, const char * pszDirSpec, const char * pszAttr)
{
   PHB_ITEM pDirArray = nullptr;

   HB_SYMBOL_UNUSED(pFuncs);

   PHB_CONCLI conn = s_fileConnect(&pszDirSpec, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len1 = static_cast<HB_U16>(pszDirSpec ? strlen(pszDirSpec) : 0);
         auto len2 = static_cast<HB_U16>(pszAttr ? strlen(pszAttr) : 0);
         HB_BYTE * pBuffer = nullptr;

         if( len1 + len2 > 0 )
         {
            pBuffer = static_cast<HB_BYTE*>(hb_xgrab(len1 + len2));
            if( len1 )
            {
               memcpy(pBuffer, pszDirSpec, len1);
            }
            if( len2 )
            {
               memcpy(pBuffer + len1, pszAttr, len2);
            }
         }
         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_DIRECTORY);
         HB_PUT_LE_UINT16(&msgbuf[4], len1);
         HB_PUT_LE_UINT16(&msgbuf[6], len2);
         memset(msgbuf + 8, '\0', sizeof(msgbuf) - 8);
         if( s_fileSendMsg(conn, msgbuf, pBuffer, len1 + len2, true, false) )
         {
            auto errCode = static_cast<HB_ERRCODE>(HB_GET_LE_UINT32(&msgbuf[8]));
            HB_SIZE nResult = HB_GET_LE_UINT32(&msgbuf[4]), nRecv = 0;

            if( nResult > 0 )
            {
               auto buffer = static_cast<char*>(hb_xgrab(nResult));
               const char * data = buffer;

               nRecv = s_fileRecvAll(conn, buffer, static_cast<long>(nResult));
               if( nRecv == nResult )
               {
                  pDirArray = hb_itemDeserialize(&data, &nResult);
               }
               hb_xfree(buffer);
            }
            hb_fsSetError(errCode);
            if( pDirArray == nullptr )
            {
               HB_ERRCODE errOsCode = 0;

               if( nResult != nRecv )
               {
                  conn->errcode = errOsCode = hb_socketGetError();
               }
               else
               {
                  conn->errcode = NETIO_ERR_WRONG_PARAM;
               }

               hb_errRT_NETIO(EG_CORRUPTION, 1013, errOsCode, nullptr, HB_ERR_FUNCNAME);
            }
         }
         if( pBuffer )
         {
            hb_xfree(pBuffer);
         }
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   if( pDirArray == nullptr )
   {
      pDirArray = hb_itemArrayNew(0);
   }

   return pDirArray;
}

static HB_BOOL s_fileExists(PHB_FILE_FUNCS pFuncs, const char * pszFileName, char * pRetPath)
{
   bool fResult = false;

   HB_SYMBOL_UNUSED(pFuncs);

   if( pRetPath )
   {
      hb_strncpy(pRetPath, pszFileName, HB_PATH_MAX - 1);
   }

   PHB_CONCLI conn = s_fileConnect(&pszFileName, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len = static_cast<HB_U16>(strlen(pszFileName));

         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_EXISTS);
         HB_PUT_LE_UINT16(&msgbuf[4], len);
         memset(msgbuf + 6, '\0', sizeof(msgbuf) - 6);
         fResult = s_fileSendMsg(conn, msgbuf, pszFileName, len, true, false);
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   return fResult;
}

static HB_BOOL s_fileDelete(PHB_FILE_FUNCS pFuncs, const char * pszFileName)
{
   bool fResult = false;

   HB_SYMBOL_UNUSED(pFuncs);

   PHB_CONCLI conn = s_fileConnect(&pszFileName, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len = static_cast<HB_U16>(strlen(pszFileName));

         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_DELETE);
         HB_PUT_LE_UINT16(&msgbuf[4], len);
         memset(msgbuf + 6, '\0', sizeof(msgbuf) - 6);
         fResult = s_fileSendMsg(conn, msgbuf, pszFileName, len, true, false);
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   return fResult;
}

static HB_BOOL s_fileRename(PHB_FILE_FUNCS pFuncs, const char * pszFileName, const char * pszNewName)
{
   HB_BOOL fResult = false;

   HB_SYMBOL_UNUSED(pFuncs);

   PHB_CONCLI conn = s_fileConnect(&pszFileName, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      s_fileConnCheck(conn, &pszNewName, false, &fResult);
      if( !fResult )
      {
         conn->errcode = 2;
         hb_fsSetError(conn->errcode);
      }
      else if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len1 = static_cast<HB_U16>(strlen(pszFileName));
         auto len2 = static_cast<HB_U16>(strlen(pszNewName));
         auto pBuffer = static_cast<HB_BYTE*>(hb_xgrab(len1 + len2));

         memcpy(pBuffer, pszFileName, len1);
         memcpy(pBuffer + len1, pszNewName, len2);
         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_RENAME);
         HB_PUT_LE_UINT16(&msgbuf[4], len1);
         HB_PUT_LE_UINT16(&msgbuf[6], len2);
         memset(msgbuf + 8, '\0', sizeof(msgbuf) - 8);
         fResult = s_fileSendMsg(conn, msgbuf, pBuffer, len1 + len2, true, false);
         hb_xfree(pBuffer);
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   return fResult;
}

static HB_BOOL s_fileCopy(PHB_FILE_FUNCS pFuncs, const char * pszSrcFile, const char * pszDstFile)
{
   HB_BOOL fResult = false;
   const char * pszSource = pszSrcFile;

   if( !s_fileAccept(pFuncs, pszDstFile) )
   {
      return hb_fsCopy(pszSrcFile, pszDstFile);
   }

   PHB_CONCLI conn = s_fileConnect(&pszSource, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      if( s_fileConnCheck(conn, &pszDstFile, false, &fResult) == nullptr )
      {
         fResult = hb_fsCopy(pszSrcFile, pszDstFile);
      }
      else if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len1 = static_cast<HB_U16>(strlen(pszSource));
         auto len2 = static_cast<HB_U16>(strlen(pszDstFile));
         auto pBuffer = static_cast<HB_BYTE*>(hb_xgrab(len1 + len2));

         memcpy(pBuffer, pszSource, len1);
         memcpy(pBuffer + len1, pszDstFile, len2);
         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_COPY);
         HB_PUT_LE_UINT16(&msgbuf[4], len1);
         HB_PUT_LE_UINT16(&msgbuf[6], len2);
         memset(msgbuf + 8, '\0', sizeof(msgbuf) - 8);
         fResult = s_fileSendMsg(conn, msgbuf, pBuffer, len1 + len2, true, false);
         hb_xfree(pBuffer);
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   return fResult;
}

static HB_BOOL s_fileAttrGet(PHB_FILE_FUNCS pFuncs, const char * pszFileName, HB_FATTR * pulAttr)
{
   bool fResult = false;

   HB_SYMBOL_UNUSED(pFuncs);

   PHB_CONCLI conn = s_fileConnect(&pszFileName, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len = static_cast<HB_U16>(strlen(pszFileName));

         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_ATTRGET);
         HB_PUT_LE_UINT16(&msgbuf[4], len);
         memset(msgbuf + 6, '\0', sizeof(msgbuf) - 6);
         if( s_fileSendMsg(conn, msgbuf, pszFileName, len, true, false) )
         {
            * pulAttr = HB_GET_LE_UINT32(&msgbuf[4]);
            fResult = true;
         }
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   return fResult;
}

static HB_BOOL s_fileAttrSet(PHB_FILE_FUNCS pFuncs, const char * pszFileName, HB_FATTR ulAttr)
{
   bool fResult = false;

   HB_SYMBOL_UNUSED(pFuncs);

   PHB_CONCLI conn = s_fileConnect(&pszFileName, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len = static_cast<HB_U16>(strlen(pszFileName));

         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_ATTRSET);
         HB_PUT_LE_UINT16(&msgbuf[4], len);
         HB_PUT_LE_UINT32(&msgbuf[6], ulAttr);
         memset(msgbuf + 10, '\0', sizeof(msgbuf) - 10);
         fResult = s_fileSendMsg(conn, msgbuf, pszFileName, len, true, false);
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   return fResult;
}

static HB_BOOL s_fileTimeGet(PHB_FILE_FUNCS pFuncs, const char * pszFileName, long * plJulian, long * plMillisec)
{
   bool fResult = false;

   HB_SYMBOL_UNUSED(pFuncs);

   PHB_CONCLI conn = s_fileConnect(&pszFileName, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len = static_cast<HB_U16>(strlen(pszFileName));

         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_FTIMEGET);
         HB_PUT_LE_UINT16(&msgbuf[4], len);
         memset(msgbuf + 6, '\0', sizeof(msgbuf) - 6);
         if( s_fileSendMsg(conn, msgbuf, pszFileName, len, true, false) )
         {
            * plJulian   = HB_GET_LE_UINT32(&msgbuf[4]);
            * plMillisec = HB_GET_LE_UINT32(&msgbuf[8]);
            fResult = true;
         }
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   return fResult;
}

static HB_BOOL s_fileTimeSet(PHB_FILE_FUNCS pFuncs, const char * pszFileName, long lJulian, long lMillisec)
{
   bool fResult = false;

   HB_SYMBOL_UNUSED(pFuncs);

   PHB_CONCLI conn = s_fileConnect(&pszFileName, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len = static_cast<HB_U16>(strlen(pszFileName));

         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_FTIMESET);
         HB_PUT_LE_UINT16(&msgbuf[4], len);
         HB_PUT_LE_UINT32(&msgbuf[6], lJulian);
         HB_PUT_LE_UINT32(&msgbuf[10], lMillisec);
         memset(msgbuf + 14, '\0', sizeof(msgbuf) - 14);
         fResult = s_fileSendMsg(conn, msgbuf, pszFileName, len, true, false);
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   return fResult;
}

static HB_BOOL s_fileLink(PHB_FILE_FUNCS pFuncs, const char * pszExisting, const char * pszNewName)
{
   HB_BOOL fResult = false;

   HB_SYMBOL_UNUSED(pFuncs);

   PHB_CONCLI conn = s_fileConnect(&pszExisting, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      s_fileConnCheck(conn, &pszNewName, false, &fResult);
      if( !fResult )
      {
         conn->errcode = 2;
         hb_fsSetError(conn->errcode);
      }
      else if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len1 = static_cast<HB_U16>(strlen(pszExisting));
         auto len2 = static_cast<HB_U16>(strlen(pszNewName));
         auto pBuffer = static_cast<HB_BYTE*>(hb_xgrab(len1 + len2));

         memcpy(pBuffer, pszExisting, len1);
         memcpy(pBuffer + len1, pszNewName, len2);
         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_LINK);
         HB_PUT_LE_UINT16(&msgbuf[4], len1);
         HB_PUT_LE_UINT16(&msgbuf[6], len2);
         memset(msgbuf + 8, '\0', sizeof(msgbuf) - 8);
         fResult = s_fileSendMsg(conn, msgbuf, pBuffer, len1 + len2, true, false);
         hb_xfree(pBuffer);
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   return fResult;
}

static HB_BOOL s_fileLinkSym(PHB_FILE_FUNCS pFuncs, const char * pszTarget, const char * pszNewName)
{
   HB_BOOL fResult = false;

   HB_SYMBOL_UNUSED(pFuncs);

   PHB_CONCLI conn = s_fileConnect(&pszTarget, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      s_fileConnCheck(conn, &pszNewName, false, &fResult);
      if( !fResult )
      {
         conn->errcode = 2;
         hb_fsSetError(conn->errcode);
      }
      else if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len1 = static_cast<HB_U16>(strlen(pszTarget));
         auto len2 = static_cast<HB_U16>(strlen(pszNewName));
         auto pBuffer = static_cast<HB_BYTE*>(hb_xgrab(len1 + len2));

         memcpy(pBuffer, pszTarget, len1);
         memcpy(pBuffer + len1, pszNewName, len2);
         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_LINKSYM);
         HB_PUT_LE_UINT16(&msgbuf[4], len1);
         HB_PUT_LE_UINT16(&msgbuf[6], len2);
         memset(msgbuf + 8, '\0', sizeof(msgbuf) - 8);
         fResult = s_fileSendMsg(conn, msgbuf, pBuffer, len1 + len2, true, false);
         hb_xfree(pBuffer);
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   return fResult;
}

static char * s_fileLinkRead(PHB_FILE_FUNCS pFuncs, const char * pszFileName)
{
   char * pszResult = nullptr;

   HB_SYMBOL_UNUSED(pFuncs);

   PHB_CONCLI conn = s_fileConnect(&pszFileName, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len = static_cast<HB_U16>(strlen(pszFileName));

         HB_PUT_LE_UINT32(&msgbuf[0], NETIO_LINKREAD);
         HB_PUT_LE_UINT16(&msgbuf[4], len);
         memset(msgbuf + 6, '\0', sizeof(msgbuf) - 6);
         if( s_fileSendMsg(conn, msgbuf, pszFileName, len, true, false) )
         {
            HB_SIZE nResult = HB_GET_LE_UINT32(&msgbuf[4]), nRecv = 0;
            auto errCode = static_cast<HB_ERRCODE>(HB_GET_LE_UINT32(&msgbuf[8]));

            if( nResult > 0 )
            {
               pszResult = static_cast<char*>(hb_xgrab(nResult + 1));
               nRecv = s_fileRecvAll(conn, pszResult, static_cast<long>(nResult));
               if( nRecv != nResult )
               {
                  hb_xfree(pszResult);
                  pszResult = nullptr;
               }
               else
               {
                  pszResult[nResult] = '\0';
               }
            }
            if( nRecv != nResult )
            {
               conn->errcode = hb_socketGetError();
               hb_errRT_NETIO(EG_CORRUPTION, 1014, conn->errcode, nullptr, HB_ERR_FUNCNAME);
            }
            hb_fsSetError(errCode);
         }
         s_fileConUnlock(conn);
      }
      s_fileConClose(conn);
   }

   return pszResult;
}

static PHB_FILE s_fileOpen(PHB_FILE_FUNCS pFuncs, const char * pszFileName, const char * pDefExt, HB_FATTR nExFlags, const char * pPaths, PHB_ITEM pError)
{
   PHB_FILE pFile = nullptr;
   const char * pszFile = pszFileName;

   HB_SYMBOL_UNUSED(pFuncs);
   HB_SYMBOL_UNUSED(pPaths);

   PHB_CONCLI conn = s_fileConnect(&pszFile, nullptr, 0, 0, false, nullptr, 0, HB_ZLIB_COMPRESSION_DISABLE, 0);
   if( conn )
   {
      if( s_fileConLock(conn) )
      {
         HB_BYTE msgbuf[NETIO_MSGLEN];
         auto len = static_cast<HB_U16>(strlen(pszFile));

         if( nExFlags & 0xFFFF0000 )
         {
            HB_PUT_LE_UINT32(&msgbuf[0], NETIO_OPEN2);
            HB_PUT_LE_UINT16(&msgbuf[4], len);
            HB_PUT_LE_UINT32(&msgbuf[6], nExFlags);
            memset(msgbuf + 10, '\0', sizeof(msgbuf) - 10);
            if( pDefExt )
            {
               hb_strncpy(reinterpret_cast<char*>(&msgbuf[10]), static_cast<const char*>(pDefExt), sizeof(msgbuf) - 11);
            }
         }
         else
         {
            HB_PUT_LE_UINT32(&msgbuf[0], NETIO_OPEN);
            HB_PUT_LE_UINT16(&msgbuf[4], len);
            HB_PUT_LE_UINT16(&msgbuf[6], static_cast<HB_U16>(nExFlags));
            memset(msgbuf + 8, '\0', sizeof(msgbuf) - 8);
            if( pDefExt )
            {
               hb_strncpy(reinterpret_cast<char*>(&msgbuf[8]), static_cast<const char*>(pDefExt), sizeof(msgbuf) - 9);
            }
         }

         if( s_fileSendMsg(conn, msgbuf, pszFile, len, true, false) )
         {
            pFile = static_cast<PHB_FILE>(hb_xgrab(sizeof(HB_FILE)));
            pFile->pFuncs = s_fileMethods();
            pFile->conn = conn;
            pFile->fd = HB_GET_LE_UINT16(&msgbuf[4]);
         }
         s_fileConUnlock(conn);
      }

      if( !pFile )
      {
         s_fileConClose(conn);
      }
   }

   if( pError )
   {
      hb_errPutFileName(pError, pszFileName);
      if( pFile == nullptr )
      {
         hb_errPutOsCode(pError, hb_fsError());
         hb_errPutGenCode(pError, static_cast<HB_ERRCODE>((nExFlags & FXO_TRUNCATE) ? EG_CREATE : EG_OPEN));
      }
   }

   return pFile;
}

static void s_fileClose(PHB_FILE pFile)
{
   if( s_fileConLock(pFile->conn) )
   {
      HB_BYTE msgbuf[NETIO_MSGLEN];

      HB_PUT_LE_UINT32(&msgbuf[0], NETIO_CLOSE);
      HB_PUT_LE_UINT16(&msgbuf[4], pFile->fd);
      memset(msgbuf + 6, '\0', sizeof(msgbuf) - 6);
      s_fileSendMsg(pFile->conn, msgbuf, nullptr, 0, true, false);
      s_fileConUnlock(pFile->conn);
   }
   s_fileConClose(pFile->conn);
   hb_xfree(pFile);
}

static HB_BOOL s_fileLock(PHB_FILE pFile, HB_FOFFSET ulStart, HB_FOFFSET ulLen, int iType)
{
   bool fResult = false;

   if( s_fileConLock(pFile->conn) )
   {
      HB_BYTE msgbuf[NETIO_MSGLEN];
      HB_BOOL fUnLock = (iType & FL_MASK) == FL_UNLOCK;

      HB_PUT_LE_UINT32(&msgbuf[0], fUnLock ? NETIO_UNLOCK : NETIO_LOCK);
      HB_PUT_LE_UINT16(&msgbuf[4], pFile->fd);
      HB_PUT_LE_UINT64(&msgbuf[6], ulStart);
      HB_PUT_LE_UINT64(&msgbuf[14], ulLen);
      HB_PUT_LE_UINT16(&msgbuf[22], static_cast<HB_USHORT>(iType));
#if NETIO_MSGLEN > 24
      memset(msgbuf + 24, '\0', sizeof(msgbuf) - 24);
#endif

      fResult = s_fileSendMsg(pFile->conn, msgbuf, nullptr, 0, !fUnLock, false);
      s_fileConUnlock(pFile->conn);
   }

   return fResult;
}

static int s_fileLockTest(PHB_FILE pFile, HB_FOFFSET ulStart, HB_FOFFSET ulLen, int iType)
{
   int iResult = -1;

   if( s_fileConLock(pFile->conn) )
   {
      HB_BYTE msgbuf[NETIO_MSGLEN];

      HB_PUT_LE_UINT32(&msgbuf[0], NETIO_TESTLOCK);
      HB_PUT_LE_UINT16(&msgbuf[4], pFile->fd);
      HB_PUT_LE_UINT64(&msgbuf[6], ulStart);
      HB_PUT_LE_UINT64(&msgbuf[14], ulLen);
      HB_PUT_LE_UINT16(&msgbuf[22], static_cast<HB_USHORT>(iType));
#if NETIO_MSGLEN > 24
      memset(msgbuf + 24, '\0', sizeof(msgbuf) - 24);
#endif

      if( s_fileSendMsg(pFile->conn, msgbuf, nullptr, 0, true, false) )
      {
         iResult = HB_GET_LE_INT32(&msgbuf[4]);
      }

      s_fileConUnlock(pFile->conn);
   }

   return iResult;
}

static HB_SIZE s_fileRead(PHB_FILE pFile, void * data, HB_SIZE nSize, HB_MAXINT timeout)
{
   HB_SIZE nResult = 0;

   if( s_fileConLock(pFile->conn) )
   {
      HB_BYTE msgbuf[NETIO_MSGLEN];

      HB_PUT_LE_UINT32(&msgbuf[0], NETIO_READ);
      HB_PUT_LE_UINT16(&msgbuf[4], pFile->fd);
      HB_PUT_LE_UINT32(&msgbuf[6], nSize);
      HB_PUT_LE_UINT64(&msgbuf[10], timeout);
      memset(msgbuf + 18, '\0', sizeof(msgbuf) - 18);

      if( s_fileSendMsg(pFile->conn, msgbuf, nullptr, 0, true, false) )
      {
         auto errCode = static_cast<HB_ERRCODE>(HB_GET_LE_UINT32(&msgbuf[8]));
         nResult = HB_GET_LE_UINT32(&msgbuf[4]);
         if( nResult > 0 && nResult != static_cast<HB_SIZE>(FS_ERROR) )
         {
            if( nResult > nSize ) /* error, it should not happen, enemy attack? */
            {
               pFile->conn->errcode = errCode = NETIO_ERR_WRONG_FILE_SIZE;
               hb_errRT_NETIO(EG_DATAWIDTH, 1011, 0, nullptr, HB_ERR_FUNCNAME);
               nResult = 0;
            }
            else if( s_fileRecvAll(pFile->conn, data, static_cast<long>(nResult)) != static_cast<long>(nResult) )
            {
               pFile->conn->errcode = hb_socketGetError();
               errCode = NETIO_ERR_READ;
               hb_errRT_NETIO(EG_READ, 1012, pFile->conn->errcode, nullptr, HB_ERR_FUNCNAME);
            }
         }
         hb_fsSetError(errCode);
      }
      s_fileConUnlock(pFile->conn);
   }

   return nResult;
}

static HB_SIZE s_fileWrite(PHB_FILE pFile, const void * data, HB_SIZE nSize, HB_MAXINT timeout)
{
   HB_SIZE nResult = 0;

   if( s_fileConLock(pFile->conn) )
   {
      HB_BYTE msgbuf[NETIO_MSGLEN];

      HB_PUT_LE_UINT32(&msgbuf[0], NETIO_WRITE);
      HB_PUT_LE_UINT16(&msgbuf[4], pFile->fd);
      HB_PUT_LE_UINT32(&msgbuf[6], static_cast<long>(nSize));
      HB_PUT_LE_UINT64(&msgbuf[10], timeout);
      memset(msgbuf + 18, '\0', sizeof(msgbuf) - 18);

      if( s_fileSendMsg(pFile->conn, msgbuf, data, static_cast<long>(nSize), true, false) )
      {
         nResult = HB_GET_LE_UINT32(&msgbuf[4]);
         hb_fsSetError(static_cast<HB_ERRCODE>(HB_GET_LE_UINT32(&msgbuf[8])));
      }
      s_fileConUnlock(pFile->conn);
   }

   return nResult;
}

static HB_SIZE s_fileReadAt(PHB_FILE pFile, void * data, HB_SIZE nSize, HB_FOFFSET llOffset)
{
   HB_SIZE nResult = 0;

   if( s_fileConLock(pFile->conn) )
   {
      HB_BYTE msgbuf[NETIO_MSGLEN];

      HB_PUT_LE_UINT32(&msgbuf[0], NETIO_READAT);
      HB_PUT_LE_UINT16(&msgbuf[4], pFile->fd);
      HB_PUT_LE_UINT32(&msgbuf[6], nSize);
      HB_PUT_LE_UINT64(&msgbuf[10], llOffset);
      memset(msgbuf + 18, '\0', sizeof(msgbuf) - 18);

      if( s_fileSendMsg(pFile->conn, msgbuf, nullptr, 0, true, false) )
      {
         auto errCode = static_cast<HB_ERRCODE>(HB_GET_LE_UINT32(&msgbuf[8]));
         nResult = HB_GET_LE_UINT32(&msgbuf[4]);
         if( nResult > 0 && nResult != static_cast<HB_SIZE>(FS_ERROR) )
         {
            if( nResult > nSize ) /* error, it should not happen, enemy attack? */
            {
               pFile->conn->errcode = errCode = NETIO_ERR_WRONG_FILE_SIZE;
               hb_errRT_NETIO(EG_DATAWIDTH, 1009, 0, nullptr, HB_ERR_FUNCNAME);
               nResult = 0;
            }
            else if( s_fileRecvAll(pFile->conn, data, static_cast<long>(nResult)) != static_cast<long>(nResult) )
            {
               pFile->conn->errcode = hb_socketGetError();
               errCode = NETIO_ERR_READ;
               hb_errRT_NETIO(EG_READ, 1010, pFile->conn->errcode, nullptr, HB_ERR_FUNCNAME);
            }
         }
         hb_fsSetError(errCode);
      }
      s_fileConUnlock(pFile->conn);
   }

   return nResult;
}

static HB_SIZE s_fileWriteAt(PHB_FILE pFile, const void * data, HB_SIZE nSize, HB_FOFFSET llOffset)
{
   HB_SIZE nResult = 0;

   if( s_fileConLock(pFile->conn) )
   {
      HB_BYTE msgbuf[NETIO_MSGLEN];

      HB_PUT_LE_UINT32(&msgbuf[0], NETIO_WRITEAT);
      HB_PUT_LE_UINT16(&msgbuf[4], pFile->fd);
      HB_PUT_LE_UINT32(&msgbuf[6], static_cast<long>(nSize));
      HB_PUT_LE_UINT64(&msgbuf[10], llOffset);
      memset(msgbuf + 18, '\0', sizeof(msgbuf) - 18);

      if( s_fileSendMsg(pFile->conn, msgbuf, data, static_cast<long>(nSize), true, false) )
      {
         nResult = HB_GET_LE_UINT32(&msgbuf[4]);
         hb_fsSetError(static_cast<HB_ERRCODE>(HB_GET_LE_UINT32(&msgbuf[8])));
      }
      s_fileConUnlock(pFile->conn);
   }

   return nResult;
}

static HB_BOOL s_fileTruncAt(PHB_FILE pFile, HB_FOFFSET llOffset)
{
   bool fResult = false;

   if( s_fileConLock(pFile->conn) )
   {
      HB_BYTE msgbuf[NETIO_MSGLEN];

      HB_PUT_LE_UINT32(&msgbuf[0], NETIO_TRUNC);
      HB_PUT_LE_UINT16(&msgbuf[4], pFile->fd);
      HB_PUT_LE_UINT64(&msgbuf[6], llOffset);
      memset(msgbuf + 14, '\0', sizeof(msgbuf) - 14);

      fResult = s_fileSendMsg(pFile->conn, msgbuf, nullptr, 0, true, false);
      s_fileConUnlock(pFile->conn);
   }

   return fResult;
}

static HB_FOFFSET s_fileSeek(PHB_FILE pFile, HB_FOFFSET llOffset, HB_USHORT uiFlags)
{
   HB_FOFFSET llResult = 0;

   if( s_fileConLock(pFile->conn) )
   {
      HB_BYTE msgbuf[NETIO_MSGLEN];

      HB_PUT_LE_UINT32(&msgbuf[0], NETIO_SEEK);
      HB_PUT_LE_UINT16(&msgbuf[4], pFile->fd);
      HB_PUT_LE_UINT64(&msgbuf[6], llOffset);
      HB_PUT_LE_UINT16(&msgbuf[14], uiFlags);
      memset(msgbuf + 16, '\0', sizeof(msgbuf) - 16);

      if( s_fileSendMsg(pFile->conn, msgbuf, nullptr, 0, true, false) )
      {
         llResult = HB_GET_LE_UINT64(&msgbuf[4]);
         hb_fsSetError(static_cast<HB_ERRCODE>(HB_GET_LE_UINT32(&msgbuf[12])));
      }
      s_fileConUnlock(pFile->conn);
   }

   return llResult;
}

static HB_FOFFSET s_fileSize(PHB_FILE pFile)
{
   HB_FOFFSET llOffset = 0;

   if( s_fileConLock(pFile->conn) )
   {
      HB_BYTE msgbuf[NETIO_MSGLEN];

      HB_PUT_LE_UINT32(msgbuf, NETIO_SIZE);
      HB_PUT_LE_UINT16(&msgbuf[4], pFile->fd);
      memset(msgbuf + 6, '\0', sizeof(msgbuf) - 6);

      if( s_fileSendMsg(pFile->conn, msgbuf, nullptr, 0, true, false) )
      {
         llOffset = HB_GET_LE_UINT64(&msgbuf[4]);
         hb_fsSetError(static_cast<HB_ERRCODE>(HB_GET_LE_UINT32(&msgbuf[12])));
      }
      s_fileConUnlock(pFile->conn);
   }

   return llOffset;
}

static HB_BOOL s_fileEof(PHB_FILE pFile)
{
   bool fEof = true;

   if( s_fileConLock(pFile->conn) )
   {
      HB_BYTE msgbuf[NETIO_MSGLEN];

      HB_PUT_LE_UINT32(msgbuf, NETIO_EOF);
      HB_PUT_LE_UINT16(&msgbuf[4], pFile->fd);
      memset(msgbuf + 6, '\0', sizeof(msgbuf) - 6);

      if( s_fileSendMsg(pFile->conn, msgbuf, nullptr, 0, true, false) )
      {
         fEof = HB_GET_LE_UINT16(&msgbuf[4]) != 0;
         hb_fsSetError(static_cast<HB_ERRCODE>(HB_GET_LE_UINT32(&msgbuf[8])));
      }
      s_fileConUnlock(pFile->conn);
   }

   return fEof;
}

static void s_fileFlush(PHB_FILE pFile, HB_BOOL fDirty)
{
   HB_SYMBOL_UNUSED(pFile);
   HB_SYMBOL_UNUSED(fDirty);
}

static void s_fileCommit(PHB_FILE pFile)
{
   if( s_fileConLock(pFile->conn) )
   {
      HB_BYTE msgbuf[NETIO_MSGLEN];

      HB_PUT_LE_UINT32(msgbuf, NETIO_COMMIT);
      HB_PUT_LE_UINT16(&msgbuf[4], pFile->fd);
      memset(msgbuf + 6, '\0', sizeof(msgbuf) - 6);

      s_fileSendMsg(pFile->conn, msgbuf, nullptr, 0, false, false);
      s_fileConUnlock(pFile->conn);
   }
}

static HB_BOOL s_fileConfigure(PHB_FILE pFile, int iIndex, PHB_ITEM pValue)
{
   bool fResult = false;

   if( s_fileConLock(pFile->conn) )
   {
      HB_BYTE msgbuf[NETIO_MSGLEN];
      HB_SIZE itmSize = 0;
      char * itmData = nullptr;

      if( pValue )
      {
         itmData = hb_itemSerialize(pValue, HB_SERIALIZE_NUMSIZE, &itmSize);
      }

      HB_PUT_LE_UINT32(&msgbuf[0], NETIO_CONFIGURE);
      HB_PUT_LE_UINT16(&msgbuf[4], pFile->fd);
      HB_PUT_LE_UINT32(&msgbuf[6], itmSize);
      HB_PUT_LE_UINT32(&msgbuf[10], iIndex);
      memset(msgbuf + 14, '\0', sizeof(msgbuf) - 14);

      hb_itemClear(pValue);

      if( s_fileSendMsg(pFile->conn, msgbuf, itmData, static_cast<long>(itmSize), true, false) )
      {
         auto errCode = static_cast<HB_ERRCODE>(HB_GET_LE_UINT32(&msgbuf[12]));
         HB_SIZE nResult = HB_GET_LE_UINT32(&msgbuf[4]), nRecv = 0;

         fResult = HB_GET_LE_UINT32(&msgbuf[8]) != 0;
         if( nResult > 0 )
         {
            auto buffer = static_cast<char*>(hb_xgrab(nResult));
            const char * data = buffer;

            nRecv = s_fileRecvAll(pFile->conn, buffer, static_cast<long>(nResult));
            if( nRecv == nResult && pValue )
            {
               PHB_ITEM pResult = hb_itemDeserialize(&data, &nResult);

               if( pResult )
               {
                  hb_itemMove(pValue, pResult);
                  hb_itemRelease(pResult);
                  if( iIndex == HB_VF_IONAME )
                  {
                     hb_itemPutCPtr(pValue, hb_xstrcpy(nullptr, NETIO_FILE_PREFIX, hb_itemGetCPtr(pValue), nullptr));
                  }
               }
            }
            hb_xfree(buffer);
         }
         hb_fsSetError(errCode);
         if( nRecv != nResult )
         {
            pFile->conn->errcode = hb_socketGetError();
            hb_errRT_NETIO(EG_CORRUPTION, 1015, pFile->conn->errcode, nullptr, HB_ERR_FUNCNAME);
            fResult = false;
         }
      }
      if( itmData )
      {
         hb_xfree(itmData);
      }
      s_fileConUnlock(pFile->conn);
   }

   return fResult;
}

static HB_FHANDLE s_fileHandle(PHB_FILE pFile)
{
   return pFile ? hb_sockexGetHandle(pFile->conn->sock) : HB_NO_SOCKET;
}

static const HB_FILE_FUNCS * s_fileMethods(void)
{
   static const HB_FILE_FUNCS s_fileFuncs =
   {
      s_fileAccept,

      s_fileExists,
      s_fileDelete,
      s_fileRename,
      s_fileCopy,

      s_fileDirExists,
      s_fileDirMake,
      s_fileDirRemove,
      s_fileDirSpace,
      s_fileDirectory,

      s_fileTimeGet,
      s_fileTimeSet,
      s_fileAttrGet,
      s_fileAttrSet,

      s_fileLink,
      s_fileLinkSym,
      s_fileLinkRead,

      s_fileOpen,
      s_fileClose,
      s_fileLock,
      s_fileLockTest,
      s_fileRead,
      s_fileWrite,
      s_fileReadAt,
      s_fileWriteAt,
      s_fileTruncAt,
      s_fileSeek,
      s_fileSize,
      s_fileEof,
      s_fileFlush,
      s_fileCommit,
      s_fileConfigure,
      s_fileHandle
   };

   return &s_fileFuncs;
}

/* set HB_NETIO_STARTUP_INIT macro if you want to register NETIO client
 * automatically at application startup
 */
#if defined(HB_NETIO_STARTUP_INIT)

HB_CALL_ON_STARTUP_BEGIN(_hb_file_netio_init_)
   hb_vmAtInit(s_netio_init, nullptr);
HB_CALL_ON_STARTUP_END(_hb_file_netio_init_)

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup _hb_file_netio_init_
#elif defined(HB_DATASEG_STARTUP)
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC(_hb_file_netio_init_)
   #include "hbiniseg.hpp"
#endif

#endif // HB_NETIO_STARTUP_INIT
