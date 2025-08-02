//
// Socket API wrapper functions
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

/*
 * hb_socketGetError() --> nSocketError
 * hb_socketGetOSError() --> nOSError
 * hb_socketErrorString([ nSocketErrror = hb_socketGetError() ], [ hSocket ]) --> cError
 * hb_socketGetSockName( hSocket ) --> aAddr | NIL
 * hb_socketGetPeerName( hSocket ) --> aAddr | NIL
 * hb_socketOpen([ nDomain = HB_SOCKET_AF_INET ], [ nType = HB_SOCKET_PT_STREAM ], [ nProtocol = 0 ]) --> hSocket
 * hb_socketClose(hSocket) --> lSuccess
 * hb_socketShutdown(hSocket, [ nMode = HB_SOCKET_SHUT_RDWR ]) --> lSuccess
 * hb_socketBind(hSocket, aAddr) --> lSuccess
 * hb_socketListen(hSocket, [ iQueueLen = 10 ]) --> lSuccess
 * hb_socketAccept(hSocket, [ @aAddr ], [ nTimeout = FOREVER ]) --> hConnectionSocket
 * hb_socketConnect(hSocket, aAddr, [ nTimeout = FOREVER ]) --> lSuccess
 * hb_socketSend(hSocket, cBuffer, [ nLen = Len(cBuffer) ], [ nFlags = 0 ], [ nTimeout = FOREVER ]) --> nBytesSent
 * hb_socketSendTo(hSocket, cBuffer, [ nLen = Len(cBuffer) ], [ nFlags = 0 ], aAddr, [ nTimeout = FOREVER ]) -->
 * nBytesSent hb_socketRecv(hSocket, @cBuffer, [ nLen = Len(cBuffer) ], [ nFlags = 0 ], [ nTimeout = FOREVER ]) -->
 * nBytesRecv hb_socketRecvFrom(hSocket, @cBuffer, [ nLen = Len(cBuffer) ], [ nFlags = 0 ], @aAddr, [ nTimeout = FOREVER
 * ]) --> nBytesRecv hb_socketSetBlockingIO(hSocket, lValue) --> nSuccess hb_socketSetNoDelay(hSocket, lValue) -->
 * lSuccess hb_socketSetNoSigPipe( hSocket, lValue ) --> lSuccess hb_socketSetExclusiveAddr(hSocket, lValue) -->
 * lSuccess hb_socketSetReuseAddr(hSocket, lValue) --> lSuccess hb_socketSetKeepAlive( hSocket, lValue ) --> lSuccess
 * hb_socketSetBroadcast(hSocket, lValue) --> lSuccess
 * hb_socketSetSndBufSize(hSocket, nValue) --> lSuccess
 * hb_socketSetRcvBufSize(hSocket, nValue) --> lSuccess
 * hb_socketGetSndBufSize(hSocket, @nValue) --> lSuccess
 * hb_socketGetRcvBufSize(hSocket, @nValue) --> lSuccess
 * hb_socketSetMulticast(hSocket, [ nFamily = HB_SOCKET_AF_INET ], cAddr) --> lSuccess
 * hb_socketSelectRead(hSocket, [ nTimeout = FOREVER ]) --> nRet
 * hb_socketSelectWrite(hSocket, [ nTimeout = FOREVER ]) --> nRet
 * hb_socketSelectWriteEx(hSocket, [ nTimeout = FOREVER ]) --> nRet
 * hb_socketSelect(aRead, lSetRead, aWrite, lSetWrite, aExcep, lSetExcep, [ nTimeout = FOREVER ]) --> nRet
 * hb_socketResolveINetAddr(cAddr, nPort) --> aAddr | NIL
 * hb_socketResolveAddr(cAddr, [ nFamily = HB_SOCKET_AF_INET ]) --> cResolved
 * hb_socketGetHostName( aAddr ) --> cHostName
 * hb_socketGetHosts(cAddr, [ nFamily = HB_SOCKET_AF_INET ]) --> aHosts
 * hb_socketGetIFaces([ nFamily ], [ lNoAliases ]) --> aIfaces
 * hb_socketGetFD(hSocket) --> nFD
 * hb_socketSetFilter(hSocket, cFilterName, [ hSockParams ]) --> hSocket
 * hb_socketGetFilter(hSocket) --> cFilterName
 * hb_socketRead(hSocket, @cBuffer, [ nLen = Len(cBuffer) ], [ nTimeout = FOREVER ]) --> nBytesRead
 * hb_socketWrite(hSocket, cBuffer, [ nLen = Len(cBuffer) ], [ nTimeout = FOREVER ]) --> nBytesWritten
 * hb_socketFlush(hSocket, [ nTimeout = FOREVER ], [ lSync ]) --> nBytesLeft
 * hb_socketAutoFlush(hSocket, [ nNewSetting ]) --> nPrevSetting
 * hb_socketAutoShutdown(hSocket, [ lNewSetting ]) --> lPrevSetting
 */

/* this has to be declared before hbsocket.h is included */
#define _HB_SOCKEX_IMPLEMENTATION_

#include "hbapiitm.hpp"
#include "hbapierr.hpp"
#include "hbvm.hpp"
#include "hbstack.hpp"
#include "hbsocket.hpp"

static bool s_fInit = false;

/* create new extended socket structure */
static PHB_SOCKEX s_sockexNew(HB_SOCKET sd, PHB_ITEM pParams);

/* destroy extended socket structure */
static int s_sockexClose(PHB_SOCKEX pSock, HB_BOOL fClose)
{
  int iResult = hb_sockexRawClear(pSock, fClose);

  hb_xfree(pSock);

  return iResult;
}

/* read data from extended socket, check internal read-ahead
   buffer first */
static long s_sockexRead(PHB_SOCKEX pSock, void *data, long len, HB_MAXINT timeout)
{
  long lRead = HB_MIN(pSock->inbuffer, len);

  if (lRead > 0)
  {
    memcpy(data, pSock->buffer + pSock->posbuffer, lRead);
    pSock->inbuffer -= lRead;
    if (pSock->inbuffer)
    {
      pSock->posbuffer += lRead;
    }
    else
    {
      pSock->posbuffer = 0;
    }
    return lRead;
  }
  if (pSock->sd == HB_NO_SOCKET)
  {
    hb_socketSetError(HB_SOCKET_ERR_INVALIDHANDLE);
    return -1;
  }
  return hb_socketRecv(pSock->sd, data, len, 0, timeout);
}

/* write data to extended socket */
static long s_sockexWrite(PHB_SOCKEX pSock, const void *data, long len, HB_MAXINT timeout)
{
  if (pSock->sd == HB_NO_SOCKET)
  {
    hb_socketSetError(HB_SOCKET_ERR_INVALIDHANDLE);
    return -1;
  }
  return hb_socketSend(pSock->sd, data, len, 0, timeout);
}

/* flush data written to extended socket which may not be sent so far,
   return number of bytes not flushed (still in sent buffer), parameter
   fSync is set to TRUE if this function should use special flush method
   which allows to synchronize with new filter initialized from scratch,
   i.e. it's necessary to inform peer that we replace ZLIB compressor,
   core code set it to true only just before closing the filter. */
static long s_sockexFlush(PHB_SOCKEX pSock, HB_MAXINT timeout, HB_BOOL fSync)
{
  HB_SYMBOL_UNUSED(pSock);
  HB_SYMBOL_UNUSED(timeout);
  HB_SYMBOL_UNUSED(fSync);

  return 0;
}

/* check if data can be read from extended socket,
   return 1, 0 or -1 to indicate error.
   If fBuffer parameter is set to true then only data already read
   from socket and stored in memory buffer should be checked without
   any timeout. Such call is executed just before inside
   hb_sockexSelect() just before call to low-level socket select()
   function. */
static int s_sockexCanRead(PHB_SOCKEX pSock, HB_BOOL fBuffer, HB_MAXINT timeout)
{
  if (pSock->inbuffer > 0)
  {
    return 1;
  }
  else if (pSock->sd == HB_NO_SOCKET)
  {
    hb_socketSetError(HB_SOCKET_ERR_INVALIDHANDLE);
    return -1;
  }
  return fBuffer ? 0 : hb_socketSelectRead(pSock->sd, timeout);
}

/* check if data can be written to extended socket without any delay,
   return 1, 0 or -1 to indicate error.
   If fBuffer parameter is set to true then only this function should
   check only free place in internal sent buffer if such buffer exists.
   Real socket device should be checked only when fBuffer is false.
   If extended socket does not use any sent buffers and fBuffer is true
   then this functions should return 0. In most of implementations 0
   can be returned in all cases if fBuffer is true. Such behavior will
   reduce number of data buffered and not flushed. */
static int s_sockexCanWrite(PHB_SOCKEX pSock, HB_BOOL fBuffer, HB_MAXINT timeout)
{
  if (pSock->sd == HB_NO_SOCKET)
  {
    hb_socketSetError(HB_SOCKET_ERR_INVALIDHANDLE);
    return -1;
  }
  return fBuffer ? 0 : hb_socketSelectWrite(pSock->sd, timeout);
}

/* return socket name, caller is responsible to free the buffer
   it's intentionally non static buffer to allow nested filters
   create complex filter name dynamically, i.e. "zlib|blowfish"
 */
static char *s_sockexName(PHB_SOCKEX pSock)
{
  return hb_strdup(pSock->pFilter->pszName);
}

/* convert error code into short string description */
static const char *s_sockexErrorStr(PHB_SOCKEX pSock, int iError)
{
  HB_SYMBOL_UNUSED(pSock);

  return hb_socketErrorStr(iError);
}

/* this is basic wrapper which does not support multilevel filtering
   so it destroys previous wrappers if any and restore new raw socket,
   some other wrappers may allow to join filters encapsulating existing
   ones, i.e.: "ZLIB" | "BlowFish" */
static PHB_SOCKEX s_sockexNext(PHB_SOCKEX pSock, PHB_ITEM pParams)
{
  PHB_SOCKEX pSockNew = nullptr;

  if (pSock)
  {
    pSockNew = s_sockexNew(pSock->sd, pParams);
    if (pSockNew)
    {
      hb_sockexClose(pSock, false);
    }
  }

  return pSockNew;
}

static const HB_SOCKET_FILTER s_sockFilter = {"socket",         s_sockexNew,   s_sockexNext,    s_sockexClose,
                                              s_sockexRead,     s_sockexWrite, s_sockexFlush,   s_sockexCanRead,
                                              s_sockexCanWrite, s_sockexName,  s_sockexErrorStr};

/* create new extended socket structure */
static PHB_SOCKEX s_sockexNew(HB_SOCKET sd, PHB_ITEM pParams)
{
  auto pSock = static_cast<PHB_SOCKEX>(hb_xgrabz(sizeof(HB_SOCKEX)));
  pSock->sd = sd;
  pSock->pFilter = &s_sockFilter;

  hb_socekxParamsInit(pSock, pParams);

  return pSock;
}

static const HB_SOCKET_FILTER *s_socketFilters[HB_SOCKET_FILTER_MAX];
static int s_iFilterCount = 0;

int hb_sockexRegister(const HB_SOCKET_FILTER *pFilter)
{
  if (s_iFilterCount == 0)
  {
    s_socketFilters[s_iFilterCount++] = &s_sockFilter;
  }

  if (pFilter)
  {
    for (auto i = 0; i < s_iFilterCount; ++i)
    {
      if (s_socketFilters[i] == pFilter)
      {
        return 1;
      }
      if (hb_stricmp(s_socketFilters[i]->pszName, pFilter->pszName) == 0)
      {
        return 2;
      }
    }

    if (s_iFilterCount >= HB_SOCKET_FILTER_MAX)
    {
      return 3;
    }

    s_socketFilters[s_iFilterCount++] = pFilter;
  }

  return 0;
}

/* helper functions */

static bool s_socketaddrParam(int iParam, void **pAddr, unsigned int *puiLen)
{
  auto pItem = hb_param(iParam, Harbour::Item::ARRAY);

  if (pItem && hb_socketAddrFromItem(pAddr, puiLen, pItem))
  {
    return true;
  }

  hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  return false;
}

static HB_SOCKET s_socketSelectCallback(PHB_ITEM pItem)
{
  HB_SOCKET socket = hb_socketItemGet(pItem);

  if (socket != HB_NO_SOCKET)
  {
    return socket;
  }

  hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  return HB_NO_SOCKET;
}

static void s_socket_exit(void *cargo)
{
  HB_SYMBOL_UNUSED(cargo);

  if (s_fInit)
  {
    hb_socketCleanup();
    s_iFilterCount = 0;
    s_fInit = false;
  }
}

static void s_socket_init(void)
{
  if (!s_fInit)
  {
    hb_sockexRegister(nullptr);
    hb_socketInit();
    hb_vmAtQuit(s_socket_exit, nullptr);
    s_fInit = true;
  }
}

void hb_socketAutoInit(void)
{
  s_socket_init();
}

/* Collectable pointer support */

static HB_GARBAGE_FUNC(hb_socket_destructor)
{
  auto pSockPtr = static_cast<PHB_SOCKEX *>(Cargo);

  if (*pSockPtr)
  {
    hb_sockexClose(*pSockPtr, true);
    *pSockPtr = nullptr;
  }
}

static const HB_GC_FUNCS s_gcSocketFuncs = {hb_socket_destructor, hb_gcDummyMark};

HB_SOCKET hb_socketParam(int iParam)
{
  auto pSockPtr = static_cast<PHB_SOCKEX *>(hb_parptrGC(&s_gcSocketFuncs, iParam));

  if (pSockPtr && *pSockPtr)
  {
    return (*pSockPtr)->sd;
  }

  hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  return HB_NO_SOCKET;
}

HB_SOCKET hb_socketItemGet(PHB_ITEM pItem)
{
  auto pSockPtr = static_cast<PHB_SOCKEX *>(hb_itemGetPtrGC(pItem, &s_gcSocketFuncs));

  return pSockPtr && *pSockPtr ? (*pSockPtr)->sd : HB_NO_SOCKET;
}

PHB_ITEM hb_socketItemPut(PHB_ITEM pItem, HB_SOCKET sd)
{
  auto pSockPtr = static_cast<PHB_SOCKEX *>(hb_gcAllocate(sizeof(PHB_SOCKEX), &s_gcSocketFuncs));

  *pSockPtr = hb_sockexNew(sd, nullptr, nullptr);

  return hb_itemPutPtrGC(pItem, pSockPtr);
}

void hb_socketItemClear(PHB_ITEM pItem)
{
  auto pSockPtr = static_cast<PHB_SOCKEX *>(hb_itemGetPtrGC(pItem, &s_gcSocketFuncs));

  if (pSockPtr && *pSockPtr)
  {
    hb_sockexClose(*pSockPtr, false);
    *pSockPtr = nullptr;
  }
}

/* extended socket functions hb_sockex* */

PHB_SOCKEX hb_sockexParam(int iParam)
{
  auto pSockPtr = static_cast<PHB_SOCKEX *>(hb_parptrGC(&s_gcSocketFuncs, iParam));

  if (pSockPtr && *pSockPtr)
  {
    return *pSockPtr;
  }

  hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  return nullptr;
}

PHB_SOCKEX hb_sockexItemGet(PHB_ITEM pItem)
{
  auto pSockPtr = static_cast<PHB_SOCKEX *>(hb_itemGetPtrGC(pItem, &s_gcSocketFuncs));

  return pSockPtr ? *pSockPtr : nullptr;
}

PHB_ITEM hb_sockexItemPut(PHB_ITEM pItem, PHB_SOCKEX pSock)
{
  auto pSockPtr = static_cast<PHB_SOCKEX *>(hb_gcAllocate(sizeof(PHB_SOCKEX), &s_gcSocketFuncs));

  *pSockPtr = pSock;

  return hb_itemPutPtrGC(pItem, pSockPtr);
}

void hb_sockexItemClear(PHB_ITEM pItem)
{
  auto pSockPtr = static_cast<PHB_SOCKEX *>(hb_itemGetPtrGC(pItem, &s_gcSocketFuncs));

  if (pSockPtr && *pSockPtr)
  {
    *pSockPtr = nullptr;
  }
}

HB_BOOL hb_sockexItemReplace(PHB_ITEM pItem, PHB_SOCKEX pSock)
{
  auto pSockPtr = static_cast<PHB_SOCKEX *>(hb_itemGetPtrGC(pItem, &s_gcSocketFuncs));

  if (pSockPtr)
  {
    if (*pSockPtr)
    {
      hb_sockexClose(*pSockPtr, false);
    }
    *pSockPtr = pSock;
    return true;
  }
  return false;
}

HB_BOOL hb_sockexItemSetFilter(PHB_ITEM pItem, const char *pszFilter, PHB_ITEM pParams)
{
  auto pSockPtr = static_cast<PHB_SOCKEX *>(hb_itemGetPtrGC(pItem, &s_gcSocketFuncs));

  if (pSockPtr && *pSockPtr)
  {
    PHB_SOCKEX pSock = *pSockPtr;

    if (pszFilter == nullptr ? pSock->pFilter == &s_sockFilter : !hb_stricmp(pSock->pFilter->pszName, pszFilter))
    {
      return true;
    }
    else
    {
      PHB_SOCKEX pSockNew = hb_sockexNext(pSock, pszFilter, pParams);

      if (pSockNew)
      {
        *pSockPtr = pSockNew;
        return true;
      }
    }
  }
  return false;
}

static int s_socket_filter_find(const char *pszFilter)
{
  for (auto i = 0; i < s_iFilterCount; ++i)
  {
    if (hb_stricmp(s_socketFilters[i]->pszName, pszFilter) == 0)
    {
      return i;
    }
  }
  return -1;
}

static const HB_SOCKET_FILTER **s_socket_getfilters(const char *pszFilter, const HB_SOCKET_FILTER **pFilters,
                                                    int *piCount)
{
  int iCount = 0, iMax = *piCount;

  if (pszFilter == nullptr)
  {
    pFilters[iCount++] = &s_sockFilter;
  }
  else
  {
    int i = s_socket_filter_find(pszFilter);

    if (i >= 0)
    {
      pFilters[iCount++] = s_socketFilters[i];
    }
    else if (strchr(pszFilter, '|') != nullptr)
    {
      char *pszFilterList, *ptr;

      pszFilterList = hb_strdup(pszFilter);
      ptr = pszFilterList + strlen(pszFilterList);

      while (ptr-- > pszFilterList)
      {
        i = -2;
        if (*ptr == '|')
        {
          *ptr = '\0';
          if (ptr[1])
          {
            i = s_socket_filter_find(ptr + 1);
          }
        }
        else if (ptr == pszFilterList && ptr[0])
        {
          i = s_socket_filter_find(ptr);
        }

        if (i >= 0)
        {
          if (iCount == iMax)
          {
            if (iMax == *piCount)
            {
              pFilters = static_cast<const HB_SOCKET_FILTER **>(hb_xmemdup(pFilters, sizeof(*pFilters) * iMax));
            }
            iMax += 16;
            pFilters = static_cast<const HB_SOCKET_FILTER **>(hb_xrealloc(pFilters, sizeof(*pFilters) * iMax));
          }
          pFilters[iCount++] = s_socketFilters[i];
        }
        else if (i == -1)
        {
          iCount = 0;
          break;
        }
      }
      hb_xfree(pszFilterList);
    }
  }
  if (iCount == 0)
  {
    if (iMax > *piCount)
    {
      hb_xfree(pFilters);
    }
    pFilters = nullptr;
  }
  *piCount = iCount;

  return pFilters;
}

PHB_SOCKEX hb_sockexNew(HB_SOCKET sd, const char *pszFilter, PHB_ITEM pParams)
{
  const HB_SOCKET_FILTER *pBuffer[16];
  const HB_SOCKET_FILTER **pFilters;
  int iCount = HB_SIZEOFARRAY(pBuffer);
  PHB_SOCKEX pSock = nullptr;

  pFilters = s_socket_getfilters(pszFilter, pBuffer, &iCount);
  if (pFilters)
  {
    for (auto i = 0; i < iCount; ++i)
    {
      PHB_SOCKEX pSockNew = pSock == nullptr ? pFilters[i]->New(sd, pParams) : pFilters[i]->Next(pSock, pParams);
      if (pSockNew == nullptr)
      {
        if (pSock)
        {
          hb_sockexClose(pSock, false);
          pSock = nullptr;
        }
        break;
      }
      pSock = pSockNew;
    }
    if (pFilters != pBuffer)
    {
      hb_xfree(pFilters);
    }
  }
  return pSock;
}

PHB_SOCKEX hb_sockexNext(PHB_SOCKEX pSock, const char *pszFilter, PHB_ITEM pParams)
{
  const HB_SOCKET_FILTER *pBuffer[16];
  const HB_SOCKET_FILTER **pFilters;
  int iCount = HB_SIZEOFARRAY(pBuffer);

  pFilters = s_socket_getfilters(pszFilter, pBuffer, &iCount);
  if (pFilters)
  {
    for (auto i = 0; pSock && i < iCount; ++i)
    {
      PHB_SOCKEX pSockNew = pFilters[i]->Next(pSock, pParams);
      if (pSockNew != nullptr)
      {
        pSock = pSockNew;
      }
      else if (i == 0)
      {
        pSock = nullptr;
      }
    }
    if (pFilters != pBuffer)
    {
      hb_xfree(pFilters);
    }
  }
  return pSock;
}

int hb_sockexClose(PHB_SOCKEX pSock, HB_BOOL fClose)
{
  return pSock->pFilter->Close(pSock, fClose);
}

long hb_sockexRead(PHB_SOCKEX pSock, void *data, long len, HB_MAXINT timeout)
{
  return pSock->pFilter->Read(pSock, data, len, timeout);
}

long hb_sockexWrite(PHB_SOCKEX pSock, const void *data, long len, HB_MAXINT timeout)
{
  len = pSock->pFilter->Write(pSock, data, len, timeout);
  if (len >= 0 && pSock->iAutoFlush > 0)
  {
    if (timeout >= 0)
    {
      timeout = HB_MAX(timeout, pSock->iAutoFlush);
    }
    hb_sockexFlush(pSock, timeout, false);
  }
  return len;
}

long hb_sockexFlush(PHB_SOCKEX pSock, HB_MAXINT timeout, HB_BOOL fSync)
{
  return pSock->pFilter->Flush(pSock, timeout, fSync);
}

int hb_sockexCanRead(PHB_SOCKEX pSock, HB_BOOL fBuffer, HB_MAXINT timeout)
{
  return pSock->pFilter->CanRead(pSock, fBuffer, timeout);
}

int hb_sockexCanWrite(PHB_SOCKEX pSock, HB_BOOL fBuffer, HB_MAXINT timeout)
{
  return pSock->pFilter->CanWrite(pSock, fBuffer, timeout);
}

char *hb_sockexName(PHB_SOCKEX pSock)
{
  return pSock->pFilter->Name(pSock);
}

const char *hb_sockexErrorStr(PHB_SOCKEX pSock, int iError)
{
  return pSock->pFilter->ErrorStr(pSock, iError);
}

int hb_sockexSelect(PHB_ITEM pArrayRD, HB_BOOL fSetRD, PHB_ITEM pArrayWR, HB_BOOL fSetWR, PHB_ITEM pArrayEX,
                    HB_BOOL fSetEX, HB_MAXINT timeout, HB_SOCKET_FUNC pFunc)
{
  int iResult;
  HB_SIZE nRead = 0, nWrite = 0, nLen, nPos;
  PHB_SOCKEX pSock;

  if (pArrayRD)
  {
    nLen = hb_arrayLen(pArrayRD);
    for (nPos = 1; nPos <= nLen; ++nPos)
    {
      pSock = hb_sockexItemGet(hb_arrayGetItemPtr(pArrayRD, nPos));
      if (pSock && pSock->pFilter->CanRead(pSock, true, 0) > 0)
      {
        ++nRead;
        if (fSetRD && nRead != nPos)
        {
          hb_itemMove(hb_arrayGetItemPtr(pArrayRD, nRead), hb_arrayGetItemPtr(pArrayRD, nPos));
        }
      }
    }
  }
  if (pArrayWR)
  {
    nLen = hb_arrayLen(pArrayWR);
    for (nPos = 1; nPos <= nLen; ++nPos)
    {
      pSock = hb_sockexItemGet(hb_arrayGetItemPtr(pArrayWR, nPos));
      if (pSock && pSock->pFilter->CanWrite(pSock, true, 0) > 0)
      {
        ++nWrite;
        if (fSetWR && nWrite != nPos)
        {
          hb_itemMove(hb_arrayGetItemPtr(pArrayWR, nWrite), hb_arrayGetItemPtr(pArrayWR, nPos));
        }
      }
    }
  }

  if (nRead > 0 || nWrite > 0)
  {
    if (fSetRD && pArrayRD)
    {
      hb_arraySize(pArrayRD, nRead);
    }
    if (fSetWR && pArrayWR)
    {
      hb_arraySize(pArrayWR, nWrite);
    }
    if (fSetEX && pArrayEX)
    {
      hb_arraySize(pArrayEX, 0);
    }
    iResult = static_cast<int>(nRead + nWrite);
  }
  else
  {
    if (pFunc == nullptr)
    {
      pFunc = s_socketSelectCallback;
    }
    iResult = hb_socketSelect(pArrayRD, fSetRD, pArrayWR, fSetWR, pArrayEX, fSetEX, timeout, pFunc);
  }

  return iResult;
}

int hb_sockexRawClear(PHB_SOCKEX pSock, HB_BOOL fClose)
{
  int iResult = 0;

  if (fClose && pSock->sd != HB_NO_SOCKET)
  {
    if (pSock->fShutDown)
    {
      hb_socketShutdown(pSock->sd, HB_SOCKET_SHUT_RDWR);
    }
    iResult = hb_socketClose(pSock->sd);
  }
  if (pSock->buffer)
  {
    hb_xfree(pSock->buffer);
  }

  memset(pSock, 0, sizeof(*pSock));
  pSock->sd = HB_NO_SOCKET;

  return iResult;
}

HB_BOOL hb_sockexIsRaw(PHB_SOCKEX pSock)
{
  return pSock && pSock->pFilter == &s_sockFilter;
}

HB_SOCKET hb_sockexGetHandle(PHB_SOCKEX pSock)
{
  return pSock ? pSock->sd : HB_NO_SOCKET;
}

void hb_sockexClearHandle(PHB_SOCKEX pSock)
{
  if (pSock)
  {
    pSock->sd = HB_NO_SOCKET;
  }
}

HB_BOOL hb_sockexGetShutDown(PHB_SOCKEX pSock)
{
  return pSock && pSock->fShutDown;
}

void hb_sockexSetShutDown(PHB_SOCKEX pSock, HB_BOOL fShutDown)
{
  if (pSock)
  {
    pSock->fShutDown = fShutDown;
  }
}

int hb_sockexGetAutoFlush(PHB_SOCKEX pSock)
{
  return pSock ? pSock->iAutoFlush : 0;
}

void hb_sockexSetAutoFlush(PHB_SOCKEX pSock, int iAutoFlush)
{
  if (pSock)
  {
    pSock->iAutoFlush = iAutoFlush;
  }
}

void hb_socekxParamsGetStd(PHB_ITEM pParams, const void **pKeydata, int *pKeylen, const void **pIV, int *pIVlen,
                           int *pLevel, int *pStrategy)
{
  if (pParams && pParams->isHash())
  {
    PHB_ITEM pItem;

    if (pKeydata && pKeylen && (pItem = hb_hashGetCItemPtr(pParams, "key")) != nullptr && pItem->isString())
    {
      *pKeydata = pItem->getCPtr();
      *pKeylen = static_cast<int>(pItem->getCLen());
    }
    else if (pKeydata && pKeylen && (pItem = hb_hashGetCItemPtr(pParams, "pass")) != nullptr && pItem->isString())
    {
      *pKeydata = pItem->getCPtr();
      *pKeylen = static_cast<int>(pItem->getCLen());
    }
    if (pIV && pIVlen && (pItem = hb_hashGetCItemPtr(pParams, "iv")) != nullptr && pItem->isString())
    {
      *pIV = pItem->getCPtr();
      *pIVlen = static_cast<int>(pItem->getCLen());
    }
    if (pLevel && (pItem = hb_hashGetCItemPtr(pParams, "zlib")) != nullptr && pItem->isNumeric())
    {
      *pLevel = pItem->getNI();
    }
    if (pStrategy && (pItem = hb_hashGetCItemPtr(pParams, "zs")) != nullptr && pItem->isNumeric())
    {
      *pStrategy = pItem->getNI();
    }
  }
}

void hb_socekxParamsInit(PHB_SOCKEX pSock, PHB_ITEM pParams)
{
  if (pParams && pParams->isHash())
  {
    PHB_ITEM pItem;

    if ((pItem = hb_hashGetCItemPtr(pParams, "readahead")) != nullptr && pItem->isNumeric())
    {
      if (pSock->buffer == nullptr)
      {
        pSock->readahead = pItem->getNL();
      }
    }
    if ((pItem = hb_hashGetCItemPtr(pParams, "flush")) != nullptr && pItem->isNumeric())
    {
      pSock->iAutoFlush = pItem->getNI();
    }
    if ((pItem = hb_hashGetCItemPtr(pParams, "redir")) != nullptr && pItem->isLogical())
    {
      pSock->fRedirAll = pItem->getL();
    }
  }
}

/* PRG functions */

HB_FUNC(HB_SOCKETGETERROR)
{
  hb_retni(hb_socketGetError());
}

HB_FUNC(HB_SOCKETGETOSERROR)
{
  hb_retni(hb_socketGetOsError());
}

HB_FUNC(HB_SOCKETERRORSTRING)
{
  PHB_SOCKEX pSock;
  int iError = 1;

  if (HB_ISPOINTER(1))
  {
    pSock = hb_sockexParam(1);
  }
  else if (HB_ISPOINTER(2))
  {
    pSock = hb_sockexParam(2);
  }
  else
  {
    pSock = nullptr;
    iError = 0;
  }

  if (pSock || iError == 0)
  {
    if (HB_ISNUM(1))
    {
      iError = hb_parni(1);
    }
    else if (HB_ISNUM(2))
    {
      iError = hb_parni(2);
    }
    else
    {
      iError = hb_socketGetError();
    }

    hb_retc(pSock ? hb_sockexErrorStr(pSock, iError) : hb_socketErrorStr(iError));
  }
}

HB_FUNC(HB_SOCKETGETSOCKNAME)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    void *addr;
    unsigned int len;

    if (hb_socketGetSockName(socket, &addr, &len) == 0)
    {
      PHB_ITEM pItem = hb_socketAddrToItem(addr, len);

      if (addr)
      {
        hb_xfree(addr);
      }

      if (pItem != nullptr)
      {
        hb_itemReturnRelease(pItem);
        return;
      }
    }
    hb_ret();
  }
}

HB_FUNC(HB_SOCKETGETPEERNAME)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    void *addr;
    unsigned int len;

    if (hb_socketGetPeerName(socket, &addr, &len) == 0)
    {
      PHB_ITEM pItem = hb_socketAddrToItem(addr, len);

      if (addr)
      {
        hb_xfree(addr);
      }

      if (pItem != nullptr)
      {
        hb_itemReturnRelease(pItem);
        return;
      }
    }
    hb_ret();
  }
}

HB_FUNC(HB_SOCKETOPEN)
{
  HB_SOCKET socket;
  auto iDomain = hb_parnidef(1, HB_SOCKET_AF_INET);
  auto iType = hb_parnidef(2, HB_SOCKET_PT_STREAM);
  auto iProtocol = hb_parni(3);

  s_socket_init();
  if ((socket = hb_socketOpen(iDomain, iType, iProtocol)) != HB_NO_SOCKET)
  {
    hb_socketItemPut(hb_stackReturnItem(), socket);
  }
  else
  {
    hb_retptr(nullptr);
  }
}

HB_FUNC(HB_SOCKETCLOSE)
{
  auto pSock = hb_sockexParam(1);

  if (pSock)
  {
    hb_sockexItemClear(hb_param(1, Harbour::Item::POINTER));
    hb_retl(hb_sockexClose(pSock, true) == 0);
  }
}

HB_FUNC(HB_SOCKETSHUTDOWN)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    hb_retl(hb_socketShutdown(socket, hb_parnidef(2, HB_SOCKET_SHUT_RDWR)) == 0);
  }
}

HB_FUNC(HB_SOCKETBIND)
{
  HB_SOCKET socket = hb_socketParam(1);
  void *addr;
  unsigned int len;

  if (socket != HB_NO_SOCKET && s_socketaddrParam(2, &addr, &len))
  {
    hb_retl(hb_socketBind(socket, addr, len) == 0);
    hb_xfree(addr);
  }
}

HB_FUNC(HB_SOCKETLISTEN)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    hb_retl(hb_socketListen(socket, hb_parnidef(2, 10)) == 0);
  }
}

HB_FUNC(HB_SOCKETACCEPT)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    HB_SOCKET socketaccept;
    void *addr = nullptr;
    unsigned int len;

    socketaccept = hb_socketAccept(socket, &addr, &len, hb_parnintdef(3, -1));

    if (socketaccept != HB_NO_SOCKET)
    {
      hb_sockexSetShutDown(hb_sockexItemGet(hb_socketItemPut(hb_stackReturnItem(), socketaccept)), true);
    }
    else
    {
      hb_retptr(nullptr);
    }

    if (HB_ISBYREF(2))
    {
      PHB_ITEM pItem;
      if (socketaccept != HB_NO_SOCKET && (pItem = hb_socketAddrToItem(addr, len)) != nullptr)
      {
        hb_itemParamStoreForward(2, pItem);
        hb_itemRelease(pItem);
      }
      else
      {
        hb_stor(2);
      }
    }

    if (addr)
    {
      hb_xfree(addr);
    }
  }
}

HB_FUNC(HB_SOCKETCONNECT)
{
  HB_SOCKET socket = hb_socketParam(1);
  void *addr;
  unsigned int len;

  if (socket != HB_NO_SOCKET && s_socketaddrParam(2, &addr, &len))
  {
    HB_BOOL fResult = hb_socketConnect(socket, addr, len, hb_parnintdef(3, -1)) == 0;

    if (fResult)
    {
      hb_sockexSetShutDown(hb_sockexItemGet(hb_param(1, Harbour::Item::POINTER)), true);
    }
    hb_retl(fResult);
    hb_xfree(addr);
  }
}

HB_FUNC(HB_SOCKETSEND)
{
  auto pSock = hb_sockexParam(1);

  if (pSock)
  {
    auto lLen = static_cast<long>(hb_parclen(2));
    auto data = hb_parc(2);
    HB_MAXINT timeout = hb_parnintdef(5, -1);

    if (HB_ISNUM(3))
    {
      long lParam = hb_parnl(3);

      if (lParam >= 0 && lParam < lLen)
      {
        lLen = lParam;
      }
    }
    if (pSock->fRedirAll)
    {
      int iAutoFlush = pSock->iAutoFlush;
      if (iAutoFlush <= 0)
      {
        pSock->iAutoFlush = 15000;
      }
      lLen = hb_sockexWrite(pSock, hb_parc(2), lLen, timeout);
      pSock->iAutoFlush = iAutoFlush;
    }
    else
    {
      lLen = hb_socketSend(pSock->sd, data, lLen, hb_parni(4), timeout);
    }
    hb_retnl(lLen);
  }
}

HB_FUNC(HB_SOCKETSENDTO)
{
  HB_SOCKET socket = hb_socketParam(1);
  void *addr;
  unsigned int len;

  if (socket != HB_NO_SOCKET && s_socketaddrParam(5, &addr, &len))
  {
    auto lLen = static_cast<long>(hb_parclen(2));

    if (HB_ISNUM(3))
    {
      long lParam = hb_parnl(3);

      if (lParam >= 0 && lParam < lLen)
      {
        lLen = lParam;
      }
    }
    hb_retnl(hb_socketSendTo(socket, hb_parc(2), lLen, hb_parni(4), addr, len, hb_parnintdef(6, -1)));
    hb_xfree(addr);
  }
}

HB_FUNC(HB_SOCKETRECV)
{
  auto pSock = hb_sockexParam(1);

  if (pSock)
  {
    auto pItem = hb_param(2, Harbour::Item::STRING);
    char *pBuffer;
    HB_SIZE nLen;

    if (pItem && HB_ISBYREF(2) && hb_itemGetWriteCL(pItem, &pBuffer, &nLen))
    {
      if (HB_ISNUM(3))
      {
        long lRead = hb_parnl(3);
        if (lRead >= 0 && lRead < static_cast<long>(nLen))
        {
          nLen = lRead;
        }
      }
      hb_retnl(pSock->fRedirAll
                   ? hb_sockexRead(pSock, pBuffer, static_cast<long>(nLen), hb_parnintdef(5, -1))
                   : hb_socketRecv(pSock->sd, pBuffer, static_cast<long>(nLen), hb_parni(4), hb_parnintdef(5, -1)));
    }
    else
    {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    }
  }
}

HB_FUNC(HB_SOCKETRECVFROM)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    auto pItem = hb_param(2, Harbour::Item::STRING);
    char *pBuffer;
    HB_SIZE nLen;

    if (pItem && HB_ISBYREF(2) && hb_itemGetWriteCL(pItem, &pBuffer, &nLen))
    {
      void *addr = nullptr;
      unsigned int len;
      long lRet;

      if (HB_ISNUM(3))
      {
        long lRead = hb_parnl(3);
        if (lRead >= 0 && lRead < static_cast<long>(nLen))
        {
          nLen = lRead;
        }
      }
      hb_retnl(lRet = hb_socketRecvFrom(socket, pBuffer, static_cast<long>(nLen), hb_parni(4), &addr, &len,
                                        hb_parnintdef(6, -1)));
      if (HB_ISBYREF(5))
      {
        PHB_ITEM pAddr;

        if (lRet != -1 && (pAddr = hb_socketAddrToItem(addr, len)) != nullptr)
        {
          hb_itemParamStoreForward(5, pAddr);
          hb_itemRelease(pAddr);
        }
        else
        {
          hb_stor(5);
        }
      }

      if (addr)
      {
        hb_xfree(addr);
      }
      return;
    }
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_SOCKETSETBLOCKINGIO)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    hb_retni(hb_socketSetBlockingIO(socket, hb_parl(2)));
  }
}

HB_FUNC(HB_SOCKETSETNODELAY)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    hb_retl(hb_socketSetNoDelay(socket, hb_parl(2)) == 0);
  }
}

HB_FUNC(HB_SOCKETSETNOSIGPIPE)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    hb_retl(hb_socketSetNoSigPipe(socket, hb_parl(2)) == 0);
  }
}

HB_FUNC(HB_SOCKETSETEXCLUSIVEADDR)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    hb_retl(hb_socketSetExclusiveAddr(socket, hb_parl(2)) == 0);
  }
}

HB_FUNC(HB_SOCKETSETREUSEADDR)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    hb_retl(hb_socketSetReuseAddr(socket, hb_parl(2)) == 0);
  }
}

HB_FUNC(HB_SOCKETSETKEEPALIVE)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    hb_retl(hb_socketSetKeepAlive(socket, hb_parl(2)) == 0);
  }
}

HB_FUNC(HB_SOCKETSETBROADCAST)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    hb_retl(hb_socketSetBroadcast(socket, hb_parl(2)) == 0);
  }
}

HB_FUNC(HB_SOCKETSETSNDBUFSIZE)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    hb_retl(hb_socketSetSndBufSize(socket, hb_parni(2)) == 0);
  }
}

HB_FUNC(HB_SOCKETSETRCVBUFSIZE)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    hb_retl(hb_socketSetRcvBufSize(socket, hb_parni(2)) == 0);
  }
}

HB_FUNC(HB_SOCKETGETSNDBUFSIZE)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    int size;
    hb_retl(hb_socketGetSndBufSize(socket, &size) == 0);
    hb_storni(size, 2);
  }
}

HB_FUNC(HB_SOCKETGETRCVBUFSIZE)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    int size;
    hb_retl(hb_socketGetRcvBufSize(socket, &size) == 0);
    hb_storni(size, 2);
  }
}

HB_FUNC(HB_SOCKETSETMULTICAST)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    hb_retl(hb_socketSetMulticast(socket, hb_parnidef(2, HB_SOCKET_AF_INET), hb_parc(3)) == 0);
  }
}

HB_FUNC(HB_SOCKETSELECTREAD)
{
  auto pSock = hb_sockexParam(1);

  if (pSock)
  {
    hb_retni(hb_sockexCanRead(pSock, false, hb_parnintdef(2, -1)));
  }
}

HB_FUNC(HB_SOCKETSELECTWRITE)
{
  auto pSock = hb_sockexParam(1);

  if (pSock)
  {
    hb_retni(hb_sockexCanWrite(pSock, false, hb_parnintdef(2, -1)));
  }
}

HB_FUNC(HB_SOCKETSELECTWRITEEX)
{
  HB_SOCKET socket = hb_socketParam(1);

  if (socket != HB_NO_SOCKET)
  {
    hb_retni(hb_socketSelectWriteEx(socket, hb_parnintdef(2, -1)));
  }
}

HB_FUNC(HB_SOCKETSELECT)
{
  s_socket_init();
  hb_retni(hb_sockexSelect(hb_param(1, Harbour::Item::ARRAY), hb_parl(2), hb_param(3, Harbour::Item::ARRAY), hb_parl(4),
                           hb_param(5, Harbour::Item::ARRAY), hb_parl(6), hb_parnintdef(7, -1),
                           s_socketSelectCallback));
}

HB_FUNC(HB_SOCKETRESOLVEINETADDR)
{
  void *addr;
  unsigned int len;

  s_socket_init();
  if (hb_socketResolveInetAddr(&addr, &len, hb_parc(1), hb_parni(2)))
  {
    PHB_ITEM pItem = hb_socketAddrToItem(addr, len);

    if (addr)
    {
      hb_xfree(addr);
    }

    if (pItem != nullptr)
    {
      hb_itemReturnRelease(pItem);
      return;
    }
  }
  hb_ret();
}

HB_FUNC(HB_SOCKETRESOLVEADDR)
{
  char *szAddr;

  s_socket_init();
  szAddr = hb_socketResolveAddr(hb_parc(1), hb_parnidef(2, HB_SOCKET_AF_INET));
  if (szAddr != nullptr)
  {
    hb_retc_buffer(szAddr);
  }
  else
  {
    hb_retc_null();
  }
}

HB_FUNC(HB_SOCKETGETHOSTNAME)
{
  void *addr;
  unsigned int len;

  if (s_socketaddrParam(1, &addr, &len))
  {
    char *szHostName = hb_socketGetHostName(addr, len);

    if (addr)
    {
      hb_xfree(addr);
    }
    if (szHostName != nullptr)
    {
      hb_retc_buffer(szHostName);
    }
    else
    {
      hb_retc_null();
    }
  }
}

HB_FUNC(HB_SOCKETGETHOSTS)
{
  auto szAddr = hb_parc(1);

  if (szAddr != nullptr)
  {
    PHB_ITEM pItem;

    s_socket_init();
    pItem = hb_socketGetHosts(szAddr, hb_parnidef(2, HB_SOCKET_AF_INET));
    if (pItem != nullptr)
    {
      hb_itemReturnRelease(pItem);
    }
    else
    {
      hb_reta(0);
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

#if 0
/* This function is not implemented at C level, yet [Mindaugas] */
HB_FUNC( HB_SOCKETGETALIASES )
{
   auto szAddr = hb_parc(1);

   if( szAddr ) {
      PHB_ITEM pItem;

      s_socket_init();
      pItem = hb_socketGetAliases(szAddr, hb_parnidef(2, HB_SOCKET_AF_INET));
      if( pItem != nullptr ) {
         hb_itemReturnRelease(pItem);
      } else {
         hb_reta(0);
      }
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}
#endif

HB_FUNC(HB_SOCKETGETIFACES)
{
  PHB_ITEM pItem;

  s_socket_init();
  pItem = hb_socketGetIFaces(hb_parni(1), hb_parl(2));
  if (pItem != nullptr)
  {
    hb_itemReturnRelease(pItem);
  }
  else
  {
    hb_reta(0);
  }
}

HB_FUNC(HB_SOCKETGETFD)
{
  hb_retnint(hb_socketParam(1));
}

HB_FUNC(HB_SOCKETSETFILTER)
{
  auto pItem = hb_param(1, Harbour::Item::POINTER);

  if (hb_sockexItemSetFilter(pItem, hb_parc(2), hb_param(3, Harbour::Item::ANY)))
  {
    hb_itemReturn(pItem);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_SOCKETGETFILTER)
{
  auto pSock = hb_sockexParam(1);

  if (pSock)
  {
    hb_retc_buffer(hb_sockexName(pSock));
  }
}

HB_FUNC(HB_SOCKETREAD)
{
  auto pSock = hb_sockexParam(1);

  if (pSock)
  {
    auto pItem = hb_param(2, Harbour::Item::STRING);
    char *pBuffer;
    HB_SIZE nLen;

    if (pItem && HB_ISBYREF(2) && hb_itemGetWriteCL(pItem, &pBuffer, &nLen))
    {
      if (HB_ISNUM(3))
      {
        long lRead = hb_parnl(3);
        if (lRead >= 0 && lRead < static_cast<long>(nLen))
        {
          nLen = lRead;
        }
      }
      hb_retnl(hb_sockexRead(pSock, pBuffer, static_cast<long>(nLen), hb_parnintdef(4, -1)));
    }
    else
    {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    }
  }
}

HB_FUNC(HB_SOCKETWRITE)
{
  auto pSock = hb_sockexParam(1);

  if (pSock)
  {
    HB_MAXINT timeout = hb_parnintdef(4, -1);
    auto lLen = static_cast<long>(hb_parclen(2));

    if (HB_ISNUM(3))
    {
      long lWrite = hb_parnl(3);

      if (lWrite >= 0 && lWrite < lLen)
      {
        lLen = lWrite;
      }
    }
    hb_retnl(hb_sockexWrite(pSock, hb_parc(2), lLen, timeout));
  }
}

HB_FUNC(HB_SOCKETFLUSH)
{
  auto pSock = hb_sockexParam(1);

  if (pSock)
  {
    hb_retnl(hb_sockexFlush(pSock, hb_parnintdef(2, -1), hb_parl(3)));
  }
}

HB_FUNC(HB_SOCKETAUTOFLUSH)
{
  auto pSock = hb_sockexParam(1);

  if (pSock)
  {
    hb_retni(hb_sockexGetAutoFlush(pSock));
    if (HB_ISNUM(2))
    {
      hb_sockexSetAutoFlush(pSock, hb_parni(2));
    }
  }
}

HB_FUNC(HB_SOCKETAUTOSHUTDOWN)
{
  auto pSock = hb_sockexParam(1);

  if (pSock)
  {
    hb_retl(hb_sockexGetShutDown(pSock));
    if (HB_ISLOG(2))
    {
      hb_sockexSetShutDown(pSock, hb_parl(2));
    }
  }
}
