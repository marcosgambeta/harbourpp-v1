//
// Length Prefix Protocol
//
// Copyright 2010 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>
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

// Idea and protocol
// =================
// Very often it is required to accept the whole data message from
// TCP connection. Because of stream nature of TCP, this requires
// additional steps from application like start/end marker, or sending
// length of structure before the structure. The latter simple approach
// was used in Length Prefix Protocol (LPP). Protocol can easily be
// described by simple Harbour expression:
//  Bin2L(hb_BLen(cData)) + cData
//
// Future extensions: Protocol is limited to 4 GiB size for a single LPP
// message. This can be extended in future to use highest bit of length
// (or some highest length values 2^32-1, etc) as a special marker for
// 64-bit or similar length encoding.
//
// Public functions and procedures
// ===============================
// hb_lppCreate(hSocket) --> hLPP
// hb_lppDestroy(hNSTP)
//  Destroys only LPP related structures. Socket remains open and
//  it is possible to continue data transfers using hb_socket*()
//  functions.
// hb_lppError(hLPP) --> nError
//  nError value is compatible with Harbour socket error API,
//  the only new error code (until now) is HB_LPP_ERROR_TOOLARGE
// hb_lppSetLimit(hLPP, nLimit)
//  Sets size limit for receiving data. Sending 4 bytes containing
//  large 32-bit value makes receiving application to allocate a
//  large memory block for storage of data to be received. It is very
//  easy to crash  application (or system) using such protocol and
//  logic. hb_lppSetLimit() helps to protect against such attacks.
//  On hb_lppCreate() limit is set to 1024 bytes. This is enough
//  for server/client authentication. After successful
//  authentication server can increase size limit and large LPP
//  packets can be used.
// hb_lppSend(hLPP, cBuf [, nTimeout = FOREVER ]) --> lSuccess
// hb_lppRecv(hLPP, @cBuf [, nTimeout = FOREVER ]) --> lSuccess
// hb_lppSendLen(hLPP) --> nBytesSent
//  Useful for drawing progress bars, etc.
// hb_lppRecvLen(hLPP) --> nBytesReceived
//  Useful for drawing progress bars, etc.
//
// Sample code
// ===========
// // send sample
// hLPP := hb_lppCreate(hSocket)
// DO WHILE !( lI := hb_lppSend(hLPP, cData, nTimeout) ) .AND. ;
//        hb_lppError(hLPP) == HB_SOCKET_ERR_TIMEOUT )
//    // draw progress bar using hb_lppSendLen(hLPP)
// ENDDO
// IF lI  // or hb_lppError(hLPP) == 0
//    // Sent OK
// ELSE
//    // error
// ENDIF
// hb_hsctpDestroy(hLPP)
//
//
// // recv sample
// DO WHILE !(lI := hb_lppRecv(hLPP, @cData, nTimeout)) .AND. ;
//        hb_lppError(hLPP) == HB_SOCKET_ERR_TIMEOUT)
//    // draw progress bar using hb_lppRecvLen(hLPP)
// ENDDO
// IF lI
//    // Rcvd OK, data in cData
// ELSEIF hb_lppError(hLPP) == 0
//    // remote side shutdown connection
// ELSE
//    // error
// ENDIF

#include "hbapiitm.hpp"
#include "hbapierr.hpp"
#include "hblpp.h"

struct HB_LPP_GC
{
  PHB_LPP pSocket;
  PHB_ITEM pItemSocket;
};

using PHB_LPP_GC = HB_LPP_GC *;

static HB_GARBAGE_FUNC(hb_lpp_destructor)
{
  auto pGC = static_cast<PHB_LPP_GC>(Cargo);

  if (pGC->pSocket)
  {
    hb_lppDestroy(pGC->pSocket);
    pGC->pSocket = nullptr;
  }
  if (pGC->pItemSocket)
  {
    hb_itemRelease(pGC->pItemSocket);
    pGC->pItemSocket = nullptr;
  }
}

static HB_GARBAGE_FUNC(hb_lpp_mark)
{
  auto pGC = static_cast<PHB_LPP_GC>(Cargo);

  if (pGC->pItemSocket)
  {
    hb_gcMark(pGC->pItemSocket);
  }
}

static const HB_GC_FUNCS s_gcPSocketFuncs = {hb_lpp_destructor, hb_lpp_mark};

HB_FUNC(HB_LPPCREATE)
{
  HB_SOCKET sd;

  auto pItem = hb_param(1, Harbour::Item::POINTER);
  if (!pItem || (sd = hb_socketItemGet(pItem)) == HB_NO_SOCKET)
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    return;
  }

  auto pGC = static_cast<PHB_LPP_GC>(hb_gcAllocate(sizeof(HB_LPP_GC), &s_gcPSocketFuncs));
  pGC->pSocket = hb_lppCreate(sd);
  pGC->pItemSocket = hb_itemNew(pItem);
  hb_gcUnlock(pGC->pItemSocket);
  hb_retptrGC(pGC);
}

HB_FUNC(HB_LPPDESTROY)
{
  auto pGC = static_cast<PHB_LPP_GC>(hb_parptrGC(&s_gcPSocketFuncs, 1));
  if (!pGC || !pGC->pSocket)
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    return;
  }
  hb_lppDestroy(pGC->pSocket);
  pGC->pSocket = nullptr;
  hb_itemRelease(pGC->pItemSocket);
  pGC->pItemSocket = nullptr;
}

HB_FUNC(HB_LPPERROR)
{
  auto pGC = static_cast<PHB_LPP_GC>(hb_parptrGC(&s_gcPSocketFuncs, 1));
  if (!pGC || !pGC->pSocket)
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    return;
  }
  hb_retni(hb_lppError(pGC->pSocket));
}

HB_FUNC(HB_LPPSETLIMIT)
{
  auto pGC = static_cast<PHB_LPP_GC>(hb_parptrGC(&s_gcPSocketFuncs, 1));
  if (!pGC || !pGC->pSocket)
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    return;
  }
  hb_lppSetLimit(pGC->pSocket, hb_parns(2));
}

HB_FUNC(HB_LPPSEND)
{
  auto pGC = static_cast<PHB_LPP_GC>(hb_parptrGC(&s_gcPSocketFuncs, 1));
  if (!pGC || !pGC->pSocket || hb_socketItemGet(pGC->pItemSocket) == HB_NO_SOCKET)
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    return;
  }

  auto pData = hb_param(2, Harbour::Item::STRING);
  hb_retl(hb_lppSend(pGC->pSocket, pData ? pData->getCPtr() : "", hb_itemGetCLen(pData), hb_parnintdef(3, -1)));
}

HB_FUNC(HB_LPPRECV)
{
  auto pGC = static_cast<PHB_LPP_GC>(hb_parptrGC(&s_gcPSocketFuncs, 1));
  if (!pGC || !pGC->pSocket || hb_socketItemGet(pGC->pItemSocket) == HB_NO_SOCKET)
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    return;
  }

  void *data;
  HB_SIZE len;
  bool bRet = hb_lppRecv(pGC->pSocket, &data, &len, hb_parnintdef(3, -1));
  if (bRet)
  {
    if (HB_ISBYREF(2))
    {
      hb_storclen(static_cast<char *>(data), len, 2);
    }
    hb_xfree(data);
  }
  hb_retl(bRet);
}

HB_FUNC(HB_LPPSENDLEN)
{
  auto pGC = static_cast<PHB_LPP_GC>(hb_parptrGC(&s_gcPSocketFuncs, 1));
  if (!pGC || !pGC->pSocket)
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    return;
  }
  hb_retns(hb_lppSendLen(pGC->pSocket));
}

HB_FUNC(HB_LPPRECVLEN)
{
  auto pGC = static_cast<PHB_LPP_GC>(hb_parptrGC(&s_gcPSocketFuncs, 1));
  if (!pGC || !pGC->pSocket)
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    return;
  }
  hb_retns(hb_lppRecvLen(pGC->pSocket));
}
