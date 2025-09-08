//
// The internet protocol / TCP support
//
// Copyright 2002 Giancarlo Niccolai <gian@niccolai.ws>
//                Ron Pinkas [Ron@RonPinkas.com]
//                Marcelo Lombardo [marcelo.lombardo@newage-software.com.br]
// Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
//    updated and ported to Harbour
// Copyright 2008 Miguel Angel marchuet <miguelangel@marchuet.net>
//    added dynamic system buffer
//

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

#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbsocket.hpp"
#include "hbapierr.hpp"
#include "hbvm.hpp"
#include "hbthread.hpp"
#include "hbznet.h"

struct HB_SOCKET_STRUCT
{
  HB_SOCKET sd;
  void *remote;
  unsigned remotelen;
  char *buffer;
  long inbuffer;
  long posbuffer;
  long readahead;
  int iError;
  int iCount;
  int iTimeout;
  int iTimeLimit;
  PHB_ITEM pPeriodicBlock;
  PHB_ZNETSTREAM stream;
  HB_INET_RDFUNC recvFunc;
  HB_INET_WRFUNC sendFunc;
  HB_INET_FLFUNC flushFunc;
  HB_INET_CLFUNC cleanFunc;
  HB_INET_ERFUNC errorFunc;
  HB_INET_ESFUNC errstrFunc;
};

using PHB_SOCKET_STRUCT = HB_SOCKET_STRUCT *;

#define HB_INET_BUFFER_LEN 1500

#define HB_INET_INITIALIZE()                                                                                           \
  if (s_initialize)                                                                                                    \
  hb_inetAutoInit()

#define HB_PARSOCKET(n) (static_cast<PHB_SOCKET_STRUCT>(hb_parptrGC(&s_gcInetFuncs, n)))

#define HB_SOCKET_INIT(s, p)                                                                                           \
  do {                                                                                                                 \
    HB_INET_INITIALIZE();                                                                                              \
    s = static_cast<PHB_SOCKET_STRUCT>(hb_gcAllocate(sizeof(*s), &s_gcInetFuncs));                                     \
    memset(s, 0, sizeof(*s));                                                                                          \
    s->sd = HB_NO_SOCKET;                                                                                              \
    s->readahead = HB_INET_BUFFER_LEN;                                                                                 \
    s->iTimeout = -1;                                                                                                  \
    s->iTimeLimit = -1;                                                                                                \
    s->iError = HB_INET_ERR_OK;                                                                                        \
    p = hb_itemPutPtrGC(p, s);                                                                                         \
  } while (false)

static const char *const s_inetCRLF = "\r\n";

static HB_COUNTER s_initialize = 1;

#if defined(HB_OS_LINUX)
// #define HB_INET_LINUX_INTERRUPT     SIGUSR1 + 90
#ifdef HB_INET_LINUX_INTERRUPT
#include <signal.h>

static void hb_inetLinuxSigusrHandle(int sig)
{
  // nothing to do
  HB_SYMBOL_UNUSED(sig);
}
#endif
#endif

static void hb_inetErrRT(void)
{
  hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

static bool hb_inetIsOpen(PHB_SOCKET_STRUCT socket)
{
  if (socket->sd == HB_NO_SOCKET) {
    socket->iError = HB_INET_ERR_CLOSEDSOCKET;
    return false;
  }
  return true;
}

static int s_inetGetError(PHB_SOCKET_STRUCT socket)
{
  int iError = socket->errorFunc ? socket->errorFunc(socket->stream) : hb_socketGetError();

  if (iError == HB_SOCKET_ERR_TIMEOUT) {
    iError = HB_INET_ERR_TIMEOUT;
  }

  return iError;
}

static bool s_inetIsTimeout(PHB_SOCKET_STRUCT socket)
{
  return s_inetGetError(socket) == HB_INET_ERR_TIMEOUT;
}

static void hb_inetGetError(PHB_SOCKET_STRUCT socket)
{
  socket->iError = s_inetGetError(socket);
}

static void hb_inetCloseStream(PHB_SOCKET_STRUCT socket)
{
  if (socket->flushFunc && socket->sd != HB_NO_SOCKET) {
    socket->flushFunc(socket->stream, socket->sd, HB_MAX(socket->iTimeout, 10000), true);
  }

  if (socket->cleanFunc) {
    socket->cleanFunc(socket->stream);
  }

  socket->recvFunc = nullptr;
  socket->sendFunc = nullptr;
  socket->flushFunc = nullptr;
  socket->cleanFunc = nullptr;
  socket->stream = nullptr;
}

static int hb_inetCloseSocket(PHB_SOCKET_STRUCT socket, HB_BOOL fShutDown)
{
  hb_inetCloseStream(socket);

  if (fShutDown) {
    hb_socketShutdown(socket->sd, HB_SOCKET_SHUT_RDWR);
  }

  int ret = hb_socketClose(socket->sd);

  socket->sd = HB_NO_SOCKET;
  socket->inbuffer = 0;
  return ret;
}

static HB_GARBAGE_FUNC(hb_inetSocketFinalize)
{
  auto socket = static_cast<PHB_SOCKET_STRUCT>(Cargo);

  if (socket->sd != HB_NO_SOCKET) {
    hb_inetCloseSocket(socket, true);
  } else {
    hb_inetCloseStream(socket);
  }

  if (socket->pPeriodicBlock) {
    hb_itemRelease(socket->pPeriodicBlock);
    socket->pPeriodicBlock = nullptr;
  }
  if (socket->remote) {
    hb_xfree(socket->remote);
    socket->remote = nullptr;
  }
  if (socket->buffer) {
    hb_xfree(socket->buffer);
    socket->buffer = nullptr;
  }
}

static HB_GARBAGE_FUNC(hb_inetSocketMark)
{
  auto socket = static_cast<PHB_SOCKET_STRUCT>(Cargo);

  if (socket->pPeriodicBlock) {
    hb_gcMark(socket->pPeriodicBlock);
  }
}

static const HB_GC_FUNCS s_gcInetFuncs = {hb_inetSocketFinalize, hb_inetSocketMark};

// Socket Initialization

static void hb_inetAutoInit(void)
{
  if (s_initialize) {
    if (hb_atomic_dec(&s_initialize)) {
      hb_socketInit();
#if defined(HB_INET_LINUX_INTERRUPT)
      signal(HB_INET_LINUX_INTERRUPT, hb_inetLinuxSigusrHandle);
#endif
    }
  }
}

HB_SOCKET hb_znetInetFD(PHB_ITEM pItem, HB_BOOL fError)
{
  auto socket = static_cast<PHB_SOCKET_STRUCT>(hb_itemGetPtrGC(pItem, &s_gcInetFuncs));

  if (socket) {
    return socket->sd;
  } else if (fError) {
    hb_inetErrRT();
  }

  return HB_NO_SOCKET;
}

HB_MAXINT hb_znetInetTimeout(PHB_ITEM pItem, HB_BOOL fError)
{
  auto socket = static_cast<PHB_SOCKET_STRUCT>(hb_itemGetPtrGC(pItem, &s_gcInetFuncs));

  if (socket) {
    return socket->iTimeout; // socket->pPeriodicBlock ? socket->iTimeLimit
  } else if (fError) {
    hb_inetErrRT();
  }

  return -1;
}

HB_BOOL hb_znetInetInitialize(PHB_ITEM pItem, PHB_ZNETSTREAM pStream, HB_INET_RDFUNC recvFunc, HB_INET_WRFUNC sendFunc,
                              HB_INET_FLFUNC flushFunc, HB_INET_CLFUNC cleanFunc, HB_INET_ERFUNC errorFunc,
                              HB_INET_ESFUNC errstrFunc)
{
  auto socket = static_cast<PHB_SOCKET_STRUCT>(hb_itemGetPtrGC(pItem, &s_gcInetFuncs));

  if (socket) {
    hb_inetCloseStream(socket);
    socket->recvFunc = recvFunc;
    socket->sendFunc = sendFunc;
    socket->flushFunc = flushFunc;
    socket->cleanFunc = cleanFunc;
    socket->errorFunc = errorFunc;
    socket->errstrFunc = errstrFunc;
    socket->stream = pStream;
    return true;
  }

  hb_inetErrRT();
  return false;
}

HB_FUNC(HB_INETINIT)
{
  hb_atomic_set(&s_initialize, 0);
  int ret = hb_socketInit();
  if (ret == 0) {
#if defined(HB_INET_LINUX_INTERRUPT)
    signal(HB_INET_LINUX_INTERRUPT, hb_inetLinuxSigusrHandle);
#endif
  }
  hb_retl(ret == 0);
}

HB_FUNC(HB_INETCLEANUP)
{
  hb_socketCleanup();
}

// Socket Creation and destruction

HB_FUNC(HB_INETCREATE)
{
  PHB_ITEM pSocket = nullptr;
  PHB_SOCKET_STRUCT socket;

  HB_SOCKET_INIT(socket, pSocket);

  if (HB_ISNUM(1)) {
    socket->iTimeout = hb_parni(1);
  }

  hb_itemReturnRelease(pSocket);
}

HB_FUNC(HB_INETCLOSE)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    if (socket->sd != HB_NO_SOCKET) {
      hb_retni(hb_inetCloseSocket(socket, true));
#ifdef HB_INET_LINUX_INTERRUPT
      kill(0, HB_INET_LINUX_INTERRUPT);
#endif
    } else {
      hb_retni(-1);
    }
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETFD)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    hb_retnint(socket->sd);

    if (hb_parl(2)) {
      socket->sd = HB_NO_SOCKET;
    }
  } else {
    hb_inetErrRT();
  }
}

// Socket data access & management

HB_FUNC(HB_INETSTATUS)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    hb_retni(socket->sd == HB_NO_SOCKET ? -1 : 1); // TODO: hb_retni(socket->status);
  } else {
    hb_inetErrRT();
  }
}

// Prepared, but still not used; being in wait for comments
#if 0
HB_FUNC( HB_INETSTATUSDESC )
{
   PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

   if( socket ) {
      switch( socket->status ) {
         case 0:  hb_retc_const("Connection not opened"); return;
         case 1:  hb_retc_const("Connection alive"); return;
         case 2:  hb_retc_const("Last operation error"); return;
         case 3:  hb_retc_const("Last operation timeout"); return;
         default: hb_retc_const("unknown");
      }
   } else {
      hb_inetErrRT();
   }
}
#endif

HB_FUNC(HB_INETERRORCODE)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    hb_retni(socket->iError);
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETERRORDESC)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    switch (socket->iError) {
    case HB_INET_ERR_OK:
      hb_retc_null();
      return;
    case HB_INET_ERR_TIMEOUT:
      hb_retc_const("Timeout");
      return;
    case HB_INET_ERR_CLOSEDCONN:
      hb_retc_const("Connection closed");
      return;
    case HB_INET_ERR_CLOSEDSOCKET:
      hb_retc_const("Closed socket");
      return;
    case HB_INET_ERR_BUFFOVERRUN:
      hb_retc_const("Buffer overrun");
      return;
    default:
      if (socket->errstrFunc) {
        hb_retc(socket->errstrFunc(socket->stream, socket->iError));
      } else {
        hb_retc(hb_socketErrorStr(socket->iError));
      }
    }
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETCLEARERROR)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    socket->iError = HB_INET_ERR_OK;
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETCOUNT)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    hb_retni(socket->iCount);
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETADDRESS)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    char *szAddr = socket->remote ? hb_socketAddrGetName(socket->remote, socket->remotelen) : nullptr;
    if (szAddr != nullptr) {
      hb_retc_buffer(szAddr);
    } else {
      hb_retc_null();
    }
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETPORT)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    hb_retni(socket->remote ? hb_socketAddrGetPort(socket->remote, socket->remotelen) : 0);
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETTIMEOUT)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    hb_retni(socket->iTimeout);

    if (HB_ISNUM(2)) {
      socket->iTimeout = hb_parni(2);
    }
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETCLEARTIMEOUT)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    socket->iTimeout = -1;
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETTIMELIMIT)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    hb_retni(socket->iTimeLimit);

    if (HB_ISNUM(2)) {
      socket->iTimeLimit = hb_parni(2);
    }
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETCLEARTIMELIMIT)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    socket->iTimeLimit = -1;
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETPERIODCALLBACK)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    auto pExec = hb_param(2, Harbour::Item::ARRAY | Harbour::Item::EVALITEM);

    if (socket->pPeriodicBlock) {
      hb_itemReturn(socket->pPeriodicBlock);
    }

    if (pExec) {
      if (socket->pPeriodicBlock) {
        hb_itemRelease(socket->pPeriodicBlock);
      }
      socket->pPeriodicBlock = hb_itemClone(pExec);
      hb_gcUnlock(socket->pPeriodicBlock);
    }
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETCLEARPERIODCALLBACK)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    if (socket->pPeriodicBlock) {
      hb_itemRelease(socket->pPeriodicBlock);
      socket->pPeriodicBlock = nullptr;
    }
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETGETSNDBUFSIZE)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    int iSize = -1;
    if (hb_inetIsOpen(socket)) {
      if (hb_socketGetSndBufSize(socket->sd, &iSize) != 0) {
        iSize = -1;
      }
    }
    hb_retni(iSize);
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETGETRCVBUFSIZE)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    int iSize = -1;
    if (hb_inetIsOpen(socket)) {
      if (hb_socketGetRcvBufSize(socket->sd, &iSize) != 0) {
        iSize = -1;
      }
    }
    hb_retni(iSize);
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETSETSNDBUFSIZE)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    int iSize = -1;
    if (hb_inetIsOpen(socket)) {
      iSize = hb_parni(2);
      hb_socketSetSndBufSize(socket->sd, iSize);
    }
    hb_retni(iSize);
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETSETRCVBUFSIZE)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket) {
    int iSize = -1;
    if (hb_inetIsOpen(socket)) {
      iSize = hb_parni(2);
      hb_socketSetRcvBufSize(socket->sd, iSize);
    }
    hb_retni(iSize);
  } else {
    hb_inetErrRT();
  }
}

// TCP receive and send functions

static long s_inetRecv(PHB_SOCKET_STRUCT socket, char *buffer, long size, HB_BOOL readahead, HB_MAXINT timeout)
{
  long rec = 0;

  if (readahead && socket->inbuffer == 0 && socket->readahead > size) {
    if (socket->buffer == nullptr) {
      socket->buffer = static_cast<char *>(hb_xgrab(socket->readahead));
    }
    socket->posbuffer = 0;
    if (socket->recvFunc) {
      rec = socket->recvFunc(socket->stream, socket->sd, socket->buffer, socket->readahead, timeout);
    } else {
      rec = hb_socketRecv(socket->sd, socket->buffer, socket->readahead, 0, timeout);
    }
    socket->inbuffer = HB_MAX(0, rec);
  } else {
    readahead = false;
  }

  if (socket->inbuffer > 0) {
    rec = HB_MIN(size, socket->inbuffer);
    memcpy(buffer, socket->buffer + socket->posbuffer, rec);
    socket->posbuffer += rec;
    socket->inbuffer -= rec;
  }

  if (size > rec && !readahead) {
    if (socket->recvFunc) {
      size = socket->recvFunc(socket->stream, socket->sd, buffer + rec, size - rec, rec ? 0 : timeout);
    } else {
      size = hb_socketRecv(socket->sd, buffer + rec, size - rec, 0, rec ? 0 : timeout);
    }

    if (rec == 0) {
      rec = size;
    } else if (size > 0) {
      rec += size;
    }
  }

  return rec;
}

static void s_inetRecvInternal(int iMode)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);
  auto pBuffer = hb_param(2, Harbour::Item::STRING);

  if (socket == nullptr || pBuffer == nullptr || !HB_ISBYREF(2)) {
    hb_inetErrRT();
  } else if (!hb_inetIsOpen(socket)) {
    hb_retni(-1);
  } else {
    int iLen, iMaxLen, iReceived, iTimeElapsed;
    char *buffer;
    HB_SIZE nLen;

    if (hb_itemGetWriteCL(pBuffer, &buffer, &nLen)) {
      iLen = static_cast<int>(nLen);
    } else {
      iLen = 0;
      buffer = nullptr;
    }

    if (HB_ISNUM(3)) {
      iMaxLen = hb_parni(3);
      if (iMaxLen < 0) {
        iMaxLen = 0;
      } else if (iLen < iMaxLen) {
        iMaxLen = iLen;
      }
    } else {
      iMaxLen = iLen;
    }

    iReceived = iTimeElapsed = 0;
    socket->iError = HB_INET_ERR_OK;
    do {
      iLen = s_inetRecv(socket, buffer + iReceived, iMaxLen - iReceived, false, socket->iTimeout);
      if (iLen >= 0) {
        iReceived += iLen;
        if (iMode == 0) { // Called from hb_inetRecv()?
          break;
        }
      } else if (iLen == -1 && s_inetIsTimeout(socket)) {
        // if we have a pPeriodicBlock, timeLimit is our REAL timeout
        if (socket->pPeriodicBlock) {
          // timed out; let's see if we have to run a cb routine
          iTimeElapsed += socket->iTimeout;
          hb_execFromArray(socket->pPeriodicBlock);
          // do we continue?
          if (hb_parl(-1) && hb_vmRequestQuery() == 0 &&
              (socket->iTimeLimit == -1 || iTimeElapsed < socket->iTimeLimit)) {
            iLen = 1; // Declare success to continue loop
          }
        }
      }
    } while (iReceived < iMaxLen && iLen > 0);

    socket->iCount = iReceived;

    if (iLen == 0) {
      socket->iError = HB_INET_ERR_CLOSEDCONN;
    } else if (iLen < 0) {
      hb_inetGetError(socket);
    }

    hb_retni(iReceived > 0 ? iReceived : iLen);
  }
}

HB_FUNC(HB_INETRECV)
{
  s_inetRecvInternal(0);
}

HB_FUNC(HB_INETRECVALL)
{
  s_inetRecvInternal(1);
}

static void s_inetRecvPattern(const char *const *patterns, int *patternsizes, int iPatternsCount, int iParam)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);
  auto pResult = hb_param(iParam, Harbour::Item::BYREF);
  auto pMaxSize = hb_param(iParam + 1, Harbour::Item::NUMERIC);
  auto pBufferSize = hb_param(iParam + 2, Harbour::Item::NUMERIC);

  char cChar = '\0';
  int iPaternFound = 0;
  int iTimeElapsed = 0;
  int iPos = 0;
  int iLen;
  int iAllocated, iBufferSize, iMax;
  int i;

  if (socket == nullptr) {
    hb_inetErrRT();
    return;
  } else if (!hb_inetIsOpen(socket)) {
    if (pResult) {
      hb_itemPutNI(pResult, -1);
    }
    hb_retc_null();
    return;
  }

  iBufferSize = pBufferSize ? pBufferSize->getNI() : 80;
  iMax = pMaxSize ? pMaxSize->getNI() : 0;

  socket->iError = HB_INET_ERR_OK;

  auto buffer = static_cast<char *>(hb_xgrab(iBufferSize));
  iAllocated = iBufferSize;

  do {
    if (iPos == iAllocated - 1) {
      iAllocated += iBufferSize;
      buffer = static_cast<char *>(hb_xrealloc(buffer, iAllocated));
    }

    iLen = s_inetRecv(socket, &cChar, 1, true, socket->iTimeout);
    if (iLen == -1 && s_inetIsTimeout(socket)) {
      iLen = -2; // this signals timeout
      if (socket->pPeriodicBlock) {
        HB_BOOL fResult;

        iTimeElapsed += socket->iTimeout;
        hb_execFromArray(socket->pPeriodicBlock);
        fResult = hb_parl(-1) && hb_vmRequestQuery() == 0;

        if (fResult && (socket->iTimeLimit == -1 || iTimeElapsed < socket->iTimeLimit)) {
          iLen = 1;
        }
      }
    } else if (iLen > 0) {
      buffer[iPos++] = cChar;
      for (i = 0; i < iPatternsCount; ++i) {
        if (patternsizes[i] <= iPos && cChar == patterns[i][patternsizes[i] - 1]) {
          if (memcmp(buffer + iPos - patternsizes[i], patterns[i], patternsizes[i]) == 0) {
            iPaternFound = i + 1;
            break;
          }
        }
      }
    }
  } while (iLen > 0 && iPaternFound == 0 && (iMax == 0 || iPos < iMax));

  if (iPaternFound) {
    socket->iCount = iPos;
    if (pResult) {
      hb_itemPutNI(pResult, iPos);
    }
    hb_retclen_buffer(buffer, iPos - patternsizes[iPaternFound - 1]);
  } else {
    if (iLen == 0) {
      socket->iError = HB_INET_ERR_CLOSEDCONN;
    } else if (iLen < 0) {
      hb_inetGetError(socket);
    } else {
      socket->iError = HB_INET_ERR_BUFFOVERRUN;
      iLen = -1;
    }
    if (pResult) {
      hb_itemPutNI(pResult, iLen);
    }
    hb_xfree(buffer);
    hb_retc_null();
  }
}

HB_FUNC(HB_INETRECVLINE)
{
  auto iEolLen = static_cast<int>(strlen(s_inetCRLF));

  s_inetRecvPattern(&s_inetCRLF, &iEolLen, 1, 2);
}

#define HB_PATERN_BUF_SIZE 16

HB_FUNC(HB_INETRECVENDBLOCK)
{
  auto pProto = hb_param(2, Harbour::Item::ARRAY | Harbour::Item::STRING);
  const char *patterns_buf[HB_PATERN_BUF_SIZE];
  const char **patterns = patterns_buf;
  int patternsizes_buf[HB_PATERN_BUF_SIZE];
  int *patternsizes = patternsizes_buf;
  int iPatternsCount = 0;
  int iLen;

  if (pProto && pProto->isArray()) {
    auto iPatternsMax = static_cast<int>(hb_arrayLen(pProto));
    int i;

    for (i = 1; i <= iPatternsMax; i++) {
      iLen = static_cast<int>(hb_arrayGetCLen(pProto, i));
      if (iLen > 0) {
        ++iPatternsCount;
      }
    }
    if (iPatternsCount > 0) {
      if (iPatternsCount > HB_PATERN_BUF_SIZE) {
        patterns = static_cast<const char **>(hb_xgrab(sizeof(char *) * iPatternsCount));
        patternsizes = static_cast<int *>(hb_xgrab(sizeof(int) * iPatternsCount));
      }
      iPatternsCount = 0;
      for (i = 1; i <= iPatternsMax; i++) {
        iLen = static_cast<int>(hb_arrayGetCLen(pProto, i));
        if (iLen > 0) {
          patterns[iPatternsCount] = hb_arrayGetCPtr(pProto, i);
          patternsizes[iPatternsCount] = iLen;
          ++iPatternsCount;
        }
      }
    }
  }

  if (iPatternsCount == 0) {
    iLen = static_cast<int>(hb_itemGetCLen(pProto));
    if (iLen > 0) {
      patterns[0] = pProto->getCPtr();
      patternsizes[0] = iLen;
    } else {
      patterns[0] = s_inetCRLF;
      patternsizes[0] = static_cast<int>(strlen(s_inetCRLF));
    }
    iPatternsCount = 1;
  }

  s_inetRecvPattern(patterns, patternsizes, iPatternsCount, 3);

  if (iPatternsCount > HB_PATERN_BUF_SIZE) {
    hb_xfree(static_cast<void *>(patterns));
    hb_xfree(patternsizes);
  }
}

HB_FUNC(HB_INETDATAREADY)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket == nullptr || (hb_pcount() >= 2 && !HB_ISNUM(2))) {
    hb_inetErrRT();
  } else if (!hb_inetIsOpen(socket)) {
    hb_retni(-1);
  } else {
    int iVal;

    socket->iError = HB_INET_ERR_OK;
    if (socket->inbuffer > 0) {
      iVal = 1;
    } else {
      HB_MAXINT timeout = hb_parnint(2); // default to 0

      if (socket->readahead > 0 && socket->recvFunc) {
        char buffer[1];

        iVal = static_cast<int>(s_inetRecv(socket, buffer, 1, true, timeout));
        if (iVal == 1) {
          socket->posbuffer--;
          socket->inbuffer++;
        }
      } else {
        iVal = hb_socketSelectRead(socket->sd, timeout);
      }

      if (iVal < 0) {
        hb_inetGetError(socket);
      }
    }
    hb_retni(iVal);
  }
}

static void s_inetSendInternal(HB_BOOL lAll)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);
  auto pBuffer = hb_param(2, Harbour::Item::STRING);
  const char *buffer;
  int iLen, iSent, iSend;
  long lLastSnd = 1;

  if (socket == nullptr || pBuffer == nullptr) {
    hb_inetErrRT();
  } else if (!hb_inetIsOpen(socket)) {
    hb_retni(-1);
  } else {
    buffer = pBuffer->getCPtr();
    iSend = static_cast<int>(pBuffer->getCLen());
    if (HB_ISNUM(3)) {
      iLen = hb_parni(3);
      if (iLen < iSend) {
        iSend = iLen;
      }
    }

    socket->iError = HB_INET_ERR_OK;

    iSent = iLen = 0;
    while (iSent < iSend) {
      if (socket->sendFunc) {
        iLen = socket->sendFunc(socket->stream, socket->sd, buffer + iSent, iSend - iSent, socket->iTimeout, &lLastSnd);
        if (lLastSnd <= 0 && iLen > 0) {
          iSent += iLen;
          iLen = static_cast<int>(lLastSnd);
        }
      } else {
        iLen = hb_socketSend(socket->sd, buffer + iSent, iSend - iSent, 0, socket->iTimeout);
      }
      if (iLen > 0) {
        iSent += iLen;
        if (!lAll) {
          break;
        }
      } else {
        hb_inetGetError(socket);
        break;
      }
    }
    socket->iCount = iSent;

    if (socket->flushFunc && (lLastSnd > 0 || (lLastSnd == -1 && socket->iTimeout >= 0 && socket->iTimeout < 10000 &&
                                               s_inetIsTimeout(socket)))) {
      // TODO: safe information about unflushed data and try to call
      //       flush before entering receive wait sate
      socket->flushFunc(socket->stream, socket->sd,
                        socket->iTimeout < 0 ? socket->iTimeout : HB_MAX(socket->iTimeout, 10000), false);
    }

    hb_retni(iSent > 0 ? iSent : iLen);
  }
}

HB_FUNC(HB_INETSEND)
{
  s_inetSendInternal(false);
}

HB_FUNC(HB_INETSENDALL)
{
  s_inetSendInternal(true);
}

// Name resolution interface functions

HB_FUNC(HB_INETGETHOSTS)
{
  auto szHost = hb_parc(1);

  if (szHost != nullptr) {
    HB_INET_INITIALIZE();
    PHB_ITEM pHosts = hb_socketGetHosts(szHost, HB_SOCKET_PF_INET);
    if (pHosts) {
      hb_itemReturnRelease(pHosts);
    } else {
      hb_reta(0);
    }
  } else {
    hb_inetErrRT();
  }
}

HB_FUNC(HB_INETGETALIAS)
{
  auto szHost = hb_parc(1);

  if (szHost != nullptr) {
    HB_INET_INITIALIZE();
    PHB_ITEM pHosts = hb_socketGetAliases(szHost, HB_SOCKET_PF_INET);
    if (pHosts) {
      hb_itemReturnRelease(pHosts);
    } else {
      hb_reta(0);
    }
  } else {
    hb_inetErrRT();
  }
}

// Interface information function

HB_FUNC(HB_INETIFINFO)
{
  HB_INET_INITIALIZE();
  PHB_ITEM pInfo = hb_socketGetIFaces(hb_parnidef(2, HB_SOCKET_PF_INET), hb_parl(1));
  if (pInfo) {
    hb_itemReturnRelease(pInfo);
  } else {
    hb_reta(0);
  }
}

// Server Specific functions

static int s_inetBind(PHB_SOCKET_STRUCT socket, const void *pSockAddr, unsigned uiLen)
{
#if !defined(HB_OS_WIN)
  hb_socketSetReuseAddr(socket->sd, true);
#endif
  return hb_socketBind(socket->sd, pSockAddr, uiLen);
}

HB_FUNC(HB_INETSERVER)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(2);
  PHB_ITEM pSocket = nullptr;

  if (!HB_ISNUM(1) || (socket == nullptr && !HB_ISNIL(2))) {
    hb_inetErrRT();
    return;
  }

  if (!socket) {
    HB_SOCKET_INIT(socket, pSocket);
  } else if (socket->sd != HB_NO_SOCKET) {
    hb_inetCloseSocket(socket, false);
  }
  socket->sd = hb_socketOpen(HB_SOCKET_PF_INET, HB_SOCKET_PT_STREAM, 0);
  if (socket->sd == HB_NO_SOCKET) {
    hb_inetGetError(socket);
  } else {
    auto iPort = hb_parni(1);
    auto szAddress = hb_parc(3);
    auto iListen = hb_parnidef(4, 10);

    if (socket->remote) {
      hb_xfree(socket->remote);
    }
    if (!hb_socketInetAddr(&socket->remote, &socket->remotelen, szAddress, iPort) ||
        s_inetBind(socket, socket->remote, socket->remotelen) != 0 || hb_socketListen(socket->sd, iListen) != 0) {
      hb_inetGetError(socket);
      hb_inetCloseSocket(socket, false);
    } else {
      socket->iError = HB_INET_ERR_OK;
    }
  }
  if (pSocket) {
    hb_itemReturnRelease(pSocket);
  } else {
    hb_itemReturn(hb_param(2, Harbour::Item::ANY));
  }
}

HB_FUNC(HB_INETACCEPT)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);

  if (socket == nullptr) {
    hb_inetErrRT();
  } else if (hb_inetIsOpen(socket)) {
    do {
      void *sa;
      unsigned len;
      HB_SOCKET incoming = hb_socketAccept(socket->sd, &sa, &len, socket->iTimeout);

      if (incoming == HB_NO_SOCKET) {
        hb_inetGetError(socket);
      } else {
        PHB_SOCKET_STRUCT new_socket;
        PHB_ITEM pSocket = nullptr;
        HB_SOCKET_INIT(new_socket, pSocket);
        new_socket->remote = sa;
        new_socket->remotelen = len;
        new_socket->sd = incoming;
        hb_itemReturnRelease(pSocket);
        socket->iError = HB_INET_ERR_OK;
        break;
      }
    } while (socket->iError == HB_SOCKET_ERR_AGAIN && hb_vmRequestQuery() == 0);
  }
}

// Client specific (connection functions)

static void hb_inetConnectInternal(HB_BOOL fResolve)
{
  auto szHost = hb_parc(1);
  char *szAddr = nullptr;
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(3);
  auto iPort = hb_parni(2);

  if (szHost == nullptr || iPort == 0 || (socket == nullptr && !HB_ISNIL(3))) {
    hb_inetErrRT();
  } else {
    PHB_ITEM pSocket = nullptr;

    if (!socket) {
      HB_SOCKET_INIT(socket, pSocket);
    } else if (socket->sd != HB_NO_SOCKET) {
      hb_inetCloseSocket(socket, false);
    }

    if (fResolve) {
      szHost = szAddr = hb_socketResolveAddr(szHost, HB_SOCKET_AF_INET);
    }

    if (fResolve && !szAddr) {
      hb_inetGetError(socket);
      if (socket->iError == 0) {
        socket->iError = HB_SOCKET_ERR_WRONGADDR;
      }
    } else {
      // Creates comm socket
      socket->sd = hb_socketOpen(HB_SOCKET_PF_INET, HB_SOCKET_PT_STREAM, 0);
      if (socket->sd == HB_NO_SOCKET) {
        hb_inetGetError(socket);
      } else {
        if (socket->remote) {
          hb_xfree(socket->remote);
        }
        if (hb_socketInetAddr(&socket->remote, &socket->remotelen, szHost, iPort)) {
          hb_socketSetKeepAlive(socket->sd, true);
          if (hb_socketConnect(socket->sd, socket->remote, socket->remotelen, socket->iTimeout) != 0) {
            hb_inetGetError(socket);
          } else {
            socket->iError = HB_INET_ERR_OK;
          }
        } else {
          hb_inetGetError(socket);
        }
      }
      if (szAddr != nullptr) {
        hb_xfree(szAddr);
      }
    }
    if (pSocket) {
      hb_itemReturnRelease(pSocket);
    } else {
      hb_itemReturn(hb_param(3, Harbour::Item::ANY));
    }
  }
}

HB_FUNC(HB_INETCONNECT)
{
  hb_inetConnectInternal(true);
}

HB_FUNC(HB_INETCONNECTIP)
{
  hb_inetConnectInternal(false);
}

// Datagram functions

HB_FUNC(HB_INETDGRAMBIND)
{
  PHB_SOCKET_STRUCT socket;
  PHB_ITEM pSocket = nullptr;
  auto iPort = hb_parni(1);
  const char *szAddress;

  // Parameter error checking
  if (iPort == 0 || (hb_pcount() >= 4 && !HB_ISCHAR(4))) {
    hb_inetErrRT();
    return;
  }

  HB_SOCKET_INIT(socket, pSocket);

  // Creates comm socket
  socket->sd = hb_socketOpen(HB_SOCKET_PF_INET, HB_SOCKET_PT_DGRAM, HB_SOCKET_IPPROTO_UDP);
  if (socket->sd == HB_NO_SOCKET) {
    hb_inetGetError(socket);
    hb_itemReturnRelease(pSocket);
    return;
  }

  // Setting broadcast if needed.
  if (hb_parl(3)) {
    hb_socketSetBroadcast(socket->sd, true);
  }

  szAddress = hb_parc(2);
  if (socket->remote) {
    hb_xfree(socket->remote);
  }
  if (!hb_socketInetAddr(&socket->remote, &socket->remotelen, szAddress, iPort) ||
      s_inetBind(socket, socket->remote, socket->remotelen) != 0) {
    hb_inetGetError(socket);
    hb_inetCloseSocket(socket, false);
  } else if (hb_pcount() >= 4) {
    if (hb_socketSetMulticast(socket->sd, HB_SOCKET_PF_INET, hb_parc(4)) != 0) {
      hb_inetGetError(socket);
    }
  }

  hb_itemReturnRelease(pSocket);
}

HB_FUNC(HB_INETDGRAM)
{
  PHB_SOCKET_STRUCT socket;
  PHB_ITEM pSocket = nullptr;

  HB_SOCKET_INIT(socket, pSocket);

  // Creates comm socket
  socket->sd = hb_socketOpen(HB_SOCKET_PF_INET, HB_SOCKET_PT_DGRAM, HB_SOCKET_IPPROTO_UDP);
  if (socket->sd == HB_NO_SOCKET) {
    hb_inetGetError(socket);
    hb_itemReturnRelease(pSocket);
    return;
  }

  // Setting broadcast if needed.
  if (hb_parl(1)) {
    hb_socketSetBroadcast(socket->sd, true);
  }

  hb_itemReturnRelease(pSocket);
}

HB_FUNC(HB_INETDGRAMSEND)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);
  auto szAddress = hb_parc(2);
  auto iPort = hb_parni(3);
  auto pBuffer = hb_param(4, Harbour::Item::STRING);
  int iLen;
  const char *szBuffer;

  if (socket == nullptr || szAddress == nullptr || iPort == 0 || pBuffer == nullptr) {
    hb_inetErrRT();
  } else if (!hb_inetIsOpen(socket)) {
    socket->iCount = 0;
    hb_retni(-1);
  } else {
    socket->iCount = 0;
    if (socket->remote) {
      hb_xfree(socket->remote);
    }
    if (!hb_socketInetAddr(&socket->remote, &socket->remotelen, szAddress, iPort)) {
      hb_inetGetError(socket);
      iLen = -1;
    } else {
      szBuffer = pBuffer->getCPtr();
      iLen = static_cast<int>(pBuffer->getCLen());
      if (HB_ISNUM(5)) {
        auto iMaxLen = hb_parni(5);
        if (iMaxLen < iLen) {
          iLen = HB_MAX(iMaxLen, 0);
        }
      }
      iLen = hb_socketSendTo(socket->sd, szBuffer, iLen, 0, socket->remote, socket->remotelen, socket->iTimeout);
      if (iLen == -1) {
        hb_inetGetError(socket);
      } else {
        socket->iError = HB_INET_ERR_OK;
        socket->iCount = iLen;
      }
    }
    hb_retni(iLen);
  }
}

HB_FUNC(HB_INETDGRAMRECV)
{
  PHB_SOCKET_STRUCT socket = HB_PARSOCKET(1);
  auto pBuffer = hb_param(2, Harbour::Item::STRING);
  int iTimeElapsed = 0;
  int iLen = 0, iMax;
  char *buffer = nullptr;
  HB_SIZE nLen;
  HB_BOOL fRepeat;

  if (socket == nullptr || pBuffer == nullptr || !HB_ISBYREF(2)) {
    hb_inetErrRT();
  } else if (!hb_inetIsOpen(socket)) {
    socket->iCount = 0;
    hb_retni(-1);
  } else {
    socket->iCount = 0;
    if (hb_itemGetWriteCL(pBuffer, &buffer, &nLen)) {
      iLen = static_cast<int>(nLen);
    }
    if (HB_ISNUM(3)) {
      iMax = hb_parni(3);
      if (iMax < iLen) {
        iLen = HB_MAX(iMax, 0);
      }
    }

    do {
      fRepeat = false;
      if (socket->remote) {
        hb_xfree(socket->remote); // FIXME: double free
      }
      iMax = hb_socketRecvFrom(socket->sd, buffer, iLen, 0, &socket->remote, &socket->remotelen, socket->iTimeout);
      if (socket->pPeriodicBlock) {
        iTimeElapsed += socket->iTimeout;
        hb_execFromArray(socket->pPeriodicBlock);
        // do we continue?
        fRepeat =
            hb_parl(-1) && hb_vmRequestQuery() == 0 && (socket->iTimeLimit == -1 || iTimeElapsed < socket->iTimeLimit);
      }
    } while (fRepeat);

    if (iMax < 0) {
      hb_inetGetError(socket);
    } else {
      socket->iError = iMax == 0 ? HB_INET_ERR_CLOSEDCONN : HB_INET_ERR_OK;
    }

    hb_retni(iMax);
  }
}

// Generic utility(?) functions

HB_FUNC(HB_INETCRLF)
{
  hb_retc_const(s_inetCRLF);
}

HB_FUNC(HB_INETISSOCKET)
{
  hb_retl(HB_PARSOCKET(1) != nullptr);
}
