//
// The Item API
//
// Copyright 1999 Antonio Linares <alinares@fivetech.com>
// Copyright 1999-2007 Viktor Szakats (vszakats.net/harbour)
//    hb_itemPCount(), hb_itemParamPtr(), hb_itemReturnPtr()
//    hb_itemPutDL(), hb_itemPutNI(), hb_itemGetDL(), hb_itemGetNI(),
//    hb_itemGetCPtr(), hb_itemPutCLPtr(), hb_itemGetCLen(), hb_itemGetNLen()
//    hb_itemPutCConst(), hb_itemPutCLConst()
//    hb_itemPutNLen(), hb_itemPutNDLen(), hb_itemPutNILen(), hb_itemPutNLLen()
//    hb_itemPutD(), hb_itemSetCMemo()
// Copyright 1999 Eddie Runia <eddie@runia.com> (hb_itemStrCmp())
// Copyright 1999 David G. Holm <dholm@jsd-llc.com> (hb_itemStr(), hb_itemString(), hb_itemValToStr())
//

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

#include "hbvmopt.hpp"
// hbfloat.h have to be included before other header files
#include "hbfloat.hpp"

#include "hbvm.hpp"
#include "hbstack.hpp"
#include "hbapicls.hpp"
#include "hbapiitm.hpp"
#include "hbapilng.hpp"
#include "hbapierr.hpp"
#include "hbdate.hpp"
#include "hbset.hpp"
#include "hbapicdp.hpp"

PHB_ITEM hb_itemNew(PHB_ITEM pNull)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemNew(%p)", static_cast<void*>(pNull)));
#endif

  return hb_gcGripGet(pNull);
}

PHB_ITEM hb_itemParam(HB_USHORT uiParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemParam(%hu)", uiParam));
#endif

#if 0
   return hb_itemNew(hb_param(uiParam, Harbour::Item::ANY));
#endif
  return hb_gcGripGet(hb_param(uiParam, Harbour::Item::ANY));
}

// Internal Item API. Use this with care.

PHB_ITEM hb_itemParamPtr(HB_USHORT uiParam, long lMask)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemParamPtr(%hu, %ld)", uiParam, lMask));
#endif

  return hb_param(static_cast<int>(uiParam), lMask);
}

HB_BOOL hb_itemParamStore(HB_USHORT uiParam, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemParamStore(%hu, %p)", uiParam, static_cast<void*>(pItem)));
#endif

  if (hb_param(uiParam, Harbour::Item::BYREF))
  {
    HB_STACK_TLS_PRELOAD
    auto pDest = hb_stackItemFromBase(uiParam);

    if (pItem != nullptr)
    {
      hb_itemCopyToRef(pDest, pItem);
    }
    else
    {
      hb_itemSetNil(hb_itemUnRef(pDest));
    }
    return true;
  }

  return false;
}

HB_BOOL hb_itemParamStoreForward(HB_USHORT uiParam, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemParamStoreForward(%hu, %p)", uiParam, static_cast<void*>(pItem)));
#endif

  if (hb_param(uiParam, Harbour::Item::BYREF))
  {
    HB_STACK_TLS_PRELOAD
    auto pDest = hb_stackItemFromBase(uiParam);

    if (pItem != nullptr)
    {
      hb_itemMoveToRef(pDest, pItem);
    }
    else
    {
      hb_itemSetNil(hb_itemUnRef(pDest));
    }
    return true;
  }

  return false;
}

HB_BOOL hb_itemParamStoreRelease(HB_USHORT uiParam, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemParamStoreRelease(%hu, %p)", uiParam, static_cast<void*>(pItem)));
#endif

  if (hb_param(uiParam, Harbour::Item::BYREF))
  {
    HB_STACK_TLS_PRELOAD
    auto pDest = hb_stackItemFromBase(uiParam);

    if (pItem != nullptr)
    {
      hb_itemMoveToRef(pDest, pItem);
      hb_itemRelease(pItem);
    }
    else
    {
      hb_itemSetNil(hb_itemUnRef(pDest));
    }
    return true;
  }

  return false;
}

HB_USHORT hb_itemPCount(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPCount()"));
#endif

  HB_STACK_TLS_PRELOAD
  return static_cast<HB_USHORT>(hb_pcount());
}

HB_BOOL hb_itemRelease(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemRelease(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    hb_gcGripDrop(pItem);
    return true;
  }
  else
  {
    return false;
  }
}

PHB_ITEM hb_itemArrayNew(HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemArrayNew(%" HB_PFS "u)", nLen));
#endif

  auto pItem = hb_itemNew(nullptr);
  hb_arrayNew(pItem, nLen);
  return pItem;
}

PHB_ITEM hb_itemArrayGet(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemArrayGet(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  auto pItem = hb_itemNew(nullptr);

  if (pArray)
  {
    hb_arrayGet(pArray, nIndex, pItem);
  }

  return pItem;
}

PHB_ITEM hb_itemArrayPut(PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemArrayPut(%p, %" HB_PFS "u, %p)", static_cast<void*>(pArray), nIndex, static_cast<void*>(pItem)));
#endif

  if (pArray)
  {
    hb_arraySet(pArray, nIndex, pItem);
  }

  return pArray;
}

PHB_ITEM hb_itemPutNil(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNil(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    hb_itemSetNil(pItem);
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  return pItem;
}

PHB_ITEM hb_itemPutC(PHB_ITEM pItem, const char *szText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutC(%p, %s)", static_cast<void*>(pItem), szText));
#endif

  HB_SIZE nLen = szText ? strlen(szText) : 0;
  HB_SIZE nAlloc;
  if (nLen <= 1)
  {
    nAlloc = 0;
    szText = hb_szAscii[nLen ? static_cast<unsigned char>(szText[0]) : 0];
  }
  else
  {
    nAlloc = nLen + 1;
    szText = static_cast<char *>(hb_xmemcpy(hb_xgrab(nAlloc), szText, nAlloc));
  }

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::STRING);
  pItem->item.asString.value = const_cast<char *>(szText);
  pItem->item.asString.length = nLen;
  pItem->item.asString.allocated = nAlloc;

  return pItem;
}

PHB_ITEM hb_itemPutCL(PHB_ITEM pItem, const char *szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCL(%p, %.*s, %" HB_PFS "u)", static_cast<void*>(pItem), static_cast<int>(nLen), szText, nLen));
#endif

  HB_SIZE nAlloc;
  char *szValue;

  if (nLen <= 1)
  {
    nAlloc = 0;
    szValue = const_cast<char *>(hb_szAscii[nLen ? static_cast<unsigned char>(szText[0]) : 0]);
  }
  else
  {
    nAlloc = nLen + 1;
    szValue = static_cast<char *>(hb_xmemcpy(hb_xgrab(nAlloc), szText, nLen));
    szValue[nLen] = '\0';
  }

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  // NOTE: CA-Cl*pper seems to be buggy here, it will return nLen bytes of
  //       trash if the szText buffer is nullptr, at least with hb_retclen().
  //       [vszakats]

  pItem->setType(Harbour::Item::STRING);
  pItem->item.asString.value = szValue;
  pItem->item.asString.length = nLen;
  pItem->item.asString.allocated = nAlloc;

  return pItem;
}

PHB_ITEM hb_itemPutCConst(PHB_ITEM pItem, const char *szText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCConst(%p, %s)", static_cast<void*>(pItem), szText));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  HB_SIZE nLen = szText ? strlen(szText) : 0;

  pItem->setType(Harbour::Item::STRING);
  pItem->item.asString.length = nLen;
  pItem->item.asString.allocated = 0;
  pItem->item.asString.value =
      const_cast<char *>((nLen > 1 ? szText : hb_szAscii[nLen ? static_cast<unsigned char>(szText[0]) : 0]));

  return pItem;
}

PHB_ITEM hb_itemPutCLConst(PHB_ITEM pItem, const char *szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCLConst(%p, %.*s, %" HB_PFS "u)", static_cast<void*>(pItem), static_cast<int>(nLen), szText, nLen));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::STRING);
  pItem->item.asString.length = nLen;
  pItem->item.asString.allocated = 0;

  if (nLen <= 1)
  {
    pItem->item.asString.value = const_cast<char *>(hb_szAscii[nLen ? static_cast<unsigned char>(szText[0]) : 0]);
  }
  else if (szText[nLen] == '\0')
  {
    pItem->item.asString.value = const_cast<char *>(szText);
  }
  else
  {
    hb_errInternal(6003, "Internal error: hb_itemPutCLConst() missing termination character", nullptr, nullptr);
  }

  return pItem;
}

PHB_ITEM hb_itemPutCPtr(PHB_ITEM pItem, char *szText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCPtr(%p, %s)", static_cast<void*>(pItem), szText));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  HB_SIZE nLen = szText ? strlen(szText) : 0;

  pItem->setType(Harbour::Item::STRING);
  pItem->item.asString.length = nLen;
  if (nLen <= 1)
  {
    pItem->item.asString.allocated = 0;
    pItem->item.asString.value = const_cast<char *>(hb_szAscii[nLen ? static_cast<unsigned char>(szText[0]) : 0]);
    if (szText != nullptr)
    {
      hb_xfree(szText);
    }
  }
  else
  {
    pItem->item.asString.allocated = nLen + 1;
    pItem->item.asString.value = szText;
  }

  return pItem;
}

PHB_ITEM hb_itemPutCLPtr(PHB_ITEM pItem, char *szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutCLPtr(%p, %.*s, %" HB_PFS "u)", static_cast<void*>(pItem), static_cast<int>(nLen), szText, nLen));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::STRING);
  pItem->item.asString.length = nLen;
  if (nLen <= 1)
  {
    pItem->item.asString.allocated = 0;
    pItem->item.asString.value = const_cast<char *>(hb_szAscii[nLen ? static_cast<unsigned char>(szText[0]) : 0]);
    hb_xfree(szText);
  }
  else
  {
    szText[nLen] = '\0';
    pItem->item.asString.allocated = nLen + 1;
    pItem->item.asString.value = szText;
  }

  return pItem;
}

void hb_itemSetCMemo(PHB_ITEM pItem)
{
  if (pItem && pItem->isString())
  {
    pItem->type |= Harbour::Item::MEMOFLAG;
  }
}

// NOTE: The caller should free the pointer if it's not nullptr. [vszakats]

char *hb_itemGetC(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetC(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem && pItem->isString())
  {
    auto szResult = static_cast<char *>(hb_xgrab(pItem->item.asString.length + 1));
    hb_xmemcpy(szResult, pItem->item.asString.value, pItem->item.asString.length);
    szResult[pItem->item.asString.length] = '\0';

    return szResult;
  }
  else
  {
    return nullptr;
  }
}

char *_HB_ITEM::getC() // equivalent to hb_itemGetC
{
  if (this->isString())
  {
    auto szResult = static_cast<char *>(hb_xgrab(this->item.asString.length + 1));
    hb_xmemcpy(szResult, this->item.asString.value, this->item.asString.length);
    szResult[this->item.asString.length] = '\0';
    return szResult;
  }
  else
  {
    return nullptr;
  }
}

// NOTE: Caller should not modify the buffer returned by this function.
//       [vszakats]

const char *hb_itemGetCPtr(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetCPtr(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem && pItem->isString())
  {
    return pItem->item.asString.value;
  }
  else
  {
    return "";
  }
}

const char *_HB_ITEM::getCPtr() // equivalent to hb_itemGetCPtr
{
  if (this->isString())
  {
    return this->item.asString.value;
  }
  else
  {
    return "";
  }
}

HB_SIZE hb_itemGetCLen(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetCLen(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem && pItem->isString())
  {
    return pItem->item.asString.length;
  }
  else
  {
    return 0;
  }
}

HB_SIZE _HB_ITEM::getCLen() // equivalent to hb_itemGetCLen
{
  if (this->isString())
  {
    return this->item.asString.length;
  }
  else
  {
    return 0;
  }
}

HB_SIZE hb_itemCopyC(PHB_ITEM pItem, char *szBuffer, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyC(%p, %s, %" HB_PFS "u)", static_cast<void*>(pItem), szBuffer, nLen));
#endif

  if (pItem && pItem->isString())
  {
    if (nLen == 0 || nLen > pItem->item.asString.length)
    {
      nLen = pItem->item.asString.length;
    }
    hb_xmemcpy(szBuffer, pItem->item.asString.value, nLen);
    return nLen;
  }
  else
  {
    return 0;
  }
}

HB_BOOL hb_itemFreeC(char *szText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemFreeC(%s)", szText));
#endif

  if (szText != nullptr)
  {
    hb_xfree(szText);
    return true;
  }
  else
  {
    return false;
  }
}

const char *hb_itemGetCRef(PHB_ITEM pItem, void **phRef, HB_SIZE *pnLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetCRef(%p, %p, %p)", static_cast<void*>(pItem), static_cast<void*>(phRef), static_cast<void*>(pnLen)));
#endif

  *phRef = nullptr;

  if (pItem && pItem->isString())
  {
    if (pnLen)
    {
      *pnLen = pItem->item.asString.length;
    }

    if (pItem->item.asString.allocated)
    {
      *phRef = static_cast<void *>(pItem->item.asString.value);
      hb_xRefInc(pItem->item.asString.value);
    }

    return pItem->item.asString.value;
  }

  if (pnLen)
  {
    *pnLen = 0;
  }

  return nullptr;
}

void hb_itemFreeCRef(void *hRef)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemFreeCRef(%p)", hRef));
#endif

  if (hRef)
  {
    hb_xRefFree(hRef);
  }
}

// NOTE: Clipper is buggy and will not append a trailing zero, although
//       the NG says that it will. Check your buffers, since what may have
//       worked with Clipper could overrun the buffer with Harbour.
//       The correct buffer size is 9 bytes: char szDate[9]
//       [vszakats]

char *hb_itemGetDS(PHB_ITEM pItem, char *szDate)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetDS(%p, %p)", static_cast<void*>(pItem), static_cast<void*>(szDate)));
#endif

  if (pItem && pItem->isDateTime())
  {
    return hb_dateDecStr(szDate, pItem->item.asDateTime.julian);
  }
  else
  {
    return hb_dateDecStr(szDate, 0);
  }
}

long hb_itemGetDL(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetDL(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem && pItem->isDateTime())
  {
    return pItem->item.asDateTime.julian;
  }
  else
  {
    return 0;
  }
}

// This function always closes the time with a zero byte, so it needs a
// 18 character long buffer to store time in format "YYYYMMDDhhmmssfff"
// with trailing 0 byte.
char *hb_itemGetTS(PHB_ITEM pItem, char *szDateTime)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetTS(%p, %s)", static_cast<void*>(pItem), szDateTime));
#endif

  if (pItem && pItem->isDateTime())
  {
    return hb_timeStampStrRawPut(szDateTime, pItem->item.asDateTime.julian, pItem->item.asDateTime.time);
  }
  else
  {
    return hb_timeStampStrRawPut(szDateTime, 0, 0);
  }
}

double hb_itemGetTD(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetTD(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem && pItem->isDateTime())
  {
    return hb_timeStampPackDT(pItem->item.asDateTime.julian, pItem->item.asDateTime.time);
  }
  else
  {
    return 0;
  }
}

HB_BOOL hb_itemGetTDT(PHB_ITEM pItem, long *plJulian, long *plMilliSec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetTDT(%p,%p,%p)", static_cast<void*>(pItem), static_cast<void*>(plJulian), static_cast<void*>(plMilliSec)));
#endif

  if (pItem && pItem->isDateTime())
  {
    *plJulian = pItem->item.asDateTime.julian;
    *plMilliSec = pItem->item.asDateTime.time;
    return true;
  }
  else
  {
    *plJulian = *plMilliSec = 0;
    return false;
  }
}

HB_BOOL hb_itemGetL(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetL(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isLogical())
    {
      return pItem->logicalValue();
    }
    else if (pItem->isInteger())
    {
      return pItem->item.asInteger.value != 0;
    }
    else if (pItem->isLong())
    {
      return pItem->item.asLong.value != 0;
    }
    else if (pItem->isDouble())
    {
      return pItem->item.asDouble.value != 0.0;
    }
  }

  return false;
}

HB_BOOL _HB_ITEM::getL() // equivalent to hb_itemGetL
{
  if (this->isLogical())
  {
    return this->logicalValue();
  }
  else if (this->isInteger())
  {
    return this->item.asInteger.value != 0;
  }
  else if (this->isLong())
  {
    return this->item.asLong.value != 0;
  }
  else if (this->isDouble())
  {
    return this->item.asDouble.value != 0.0;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_itemGetLX(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetLX(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isLogical())
    {
      return pItem->logicalValue();
    }
    else if (pItem->isInteger())
    {
      return pItem->item.asInteger.value != 0;
    }
    else if (pItem->isLong())
    {
      return pItem->item.asLong.value != 0;
    }
    else if (pItem->isDouble())
    {
      return pItem->item.asDouble.value != 0.0;
    }
    else if (pItem->isDateTime())
    {
      return pItem->item.asDateTime.julian != 0 || pItem->item.asDateTime.time != 0;
    }
    else
    {
      return true;
    }
  }

  return false;
}

double hb_itemGetND(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetND(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isDouble())
    {
      return pItem->item.asDouble.value;
    }
    else if (pItem->isInteger())
    {
      return static_cast<double>(pItem->item.asInteger.value);
    }
    else if (pItem->isLong())
    {
      return static_cast<double>(pItem->item.asLong.value);
    }
  }

  return 0;
}

double _HB_ITEM::getND() // equivalent to hb_itemGetND
{
  if (this->isDouble())
  {
    return this->item.asDouble.value;
  }
  else if (this->isInteger())
  {
    return static_cast<double>(this->item.asInteger.value);
  }
  else if (this->isLong())
  {
    return static_cast<double>(this->item.asLong.value);
  }
  else
  {
    return 0;
  }
}

int hb_itemGetNI(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNI(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isInteger())
    {
      return pItem->item.asInteger.value;
    }
    else if (pItem->isLong())
    {
      return static_cast<int>(pItem->item.asLong.value);
    }
    else if (pItem->isDouble())
    {
      return HB_CAST_INT(pItem->item.asDouble.value);
    }
  }

  return 0;
}

int _HB_ITEM::getNI() // equivalent to hb_itemGetNI
{
  if (this->isInteger())
  {
    return this->item.asInteger.value;
  }
  else if (this->isLong())
  {
    return static_cast<int>(this->item.asLong.value);
  }
  else if (this->isDouble())
  {
    return HB_CAST_INT(this->item.asDouble.value);
  }
  else
  {
    return 0;
  }
}

long hb_itemGetNL(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNL(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isLong())
    {
      return static_cast<long>(pItem->item.asLong.value);
    }
    else if (pItem->isInteger())
    {
      return static_cast<long>(pItem->item.asInteger.value);
    }
    else if (pItem->isDouble())
    {
      return HB_CAST_LONG(pItem->item.asDouble.value);
    }
  }

  return 0;
}

long _HB_ITEM::getNL() // equivalent to hb_itemGetNL
{
  if (this->isLong())
  {
    return static_cast<long>(this->item.asLong.value);
  }
  else if (this->isInteger())
  {
    return static_cast<long>(this->item.asInteger.value);
  }
  else if (this->isDouble())
  {
    return HB_CAST_LONG(this->item.asDouble.value);
  }
  else
  {
    return 0;
  }
}

HB_ISIZ hb_itemGetNS(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNS(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isLong())
    {
      return static_cast<HB_ISIZ>(pItem->item.asLong.value);
    }
    else if (pItem->isInteger())
    {
      return static_cast<HB_ISIZ>(pItem->item.asInteger.value);
    }
    else if (pItem->isDouble())
    {
      return HB_CAST_ISIZ(pItem->item.asDouble.value);
    }
  }

  return 0;
}

HB_MAXINT hb_itemGetNInt(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNL(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isLong())
    {
      return static_cast<HB_MAXINT>(pItem->item.asLong.value);
    }
    else if (pItem->isInteger())
    {
      return static_cast<HB_MAXINT>(pItem->item.asInteger.value);
    }
    else if (pItem->isDouble())
    {
      return HB_CAST_MAXINT(pItem->item.asDouble.value);
    }
  }

  return 0;
}

HB_MAXINT _HB_ITEM::getNInt() // equivalent to hb_itemGetNInt
{
  if (this->isLong())
  {
    return static_cast<HB_MAXINT>(this->item.asLong.value);
  }
  else if (this->isInteger())
  {
    return static_cast<HB_MAXINT>(this->item.asInteger.value);
  }
  else if (this->isDouble())
  {
    return HB_CAST_MAXINT(this->item.asDouble.value);
  }
  else
  {
    return 0;
  }
}

#ifndef HB_LONG_LONG_OFF
HB_LONGLONG hb_itemGetNLL(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNL(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isLong())
    {
      return static_cast<HB_LONGLONG>(pItem->item.asLong.value);
    }
    else if (pItem->isInteger())
    {
      return static_cast<HB_LONGLONG>(pItem->item.asInteger.value);
    }
    else if (pItem->isDouble())
    {
      return HB_CAST_LONGLONG(pItem->item.asDouble.value);
    }
  }

  return 0;
}
#endif

void *hb_itemGetPtr(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetPtr(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem && pItem->isPointer())
  {
    return pItem->item.asPointer.value;
  }
  else
  {
    return nullptr;
  }
}

void *hb_itemGetPtrGC(PHB_ITEM pItem, const HB_GC_FUNCS *pFuncs)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetPtrGC(%p,%p)", static_cast<void*>(pItem), static_cast<const void*>(pFuncs)));
#endif

  if (pItem && pItem->isPointer() && pItem->item.asPointer.collect && hb_gcFuncs(pItem->item.asPointer.value) == pFuncs)
  {
    return pItem->item.asPointer.value;
  }
  else
  {
    return nullptr;
  }
}

PHB_SYMB hb_itemGetSymbol(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetSymbol(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem && pItem->isSymbol())
  {
    return pItem->item.asSymbol.value;
  }
  else
  {
    return nullptr;
  }
}

PHB_ITEM hb_itemReturn(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemReturn(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    HB_STACK_TLS_PRELOAD
    hb_itemCopy(hb_stackReturnItem(), pItem);
  }

  return pItem;
}

PHB_ITEM hb_itemReturnForward(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemReturnForward(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    HB_STACK_TLS_PRELOAD
    hb_itemMove(hb_stackReturnItem(), pItem);
  }

  return pItem;
}

void hb_itemReturnRelease(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemReturnRelease(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    HB_STACK_TLS_PRELOAD
    hb_itemMove(hb_stackReturnItem(), pItem);
    hb_itemRelease(pItem);
  }
}

PHB_ITEM hb_itemPutDS(PHB_ITEM pItem, const char *szDate)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutDS(%p, %.8s)", static_cast<void*>(pItem), szDate));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::DATE);
  pItem->item.asDateTime.julian = hb_dateEncStr(szDate);
  pItem->item.asDateTime.time = 0;

  return pItem;
}

PHB_ITEM hb_itemPutD(PHB_ITEM pItem, int iYear, int iMonth, int iDay)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutD(%p, %04i, %02i, %02i)", static_cast<void*>(pItem), iYear, iMonth, iDay));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::DATE);
  pItem->item.asDateTime.julian = hb_dateEncode(iYear, iMonth, iDay);
  pItem->item.asDateTime.time = 0;

  return pItem;
}

PHB_ITEM hb_itemPutDL(PHB_ITEM pItem, long lJulian)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutDL(%p, %ld)", static_cast<void*>(pItem), lJulian));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::DATE);
  pItem->item.asDateTime.julian = lJulian;
  pItem->item.asDateTime.time = 0;

  return pItem;
}

PHB_ITEM hb_itemPutTS(PHB_ITEM pItem, const char *szDateTime)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutTS(%p, %s)", static_cast<void*>(pItem), szDateTime));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::TIMESTAMP);
  hb_timeStampStrRawGet(szDateTime, &pItem->item.asDateTime.julian, &pItem->item.asDateTime.time);

  return pItem;
}

PHB_ITEM hb_itemPutTD(PHB_ITEM pItem, double dTimeStamp)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutTD(%p, %lf)", static_cast<void*>(pItem), dTimeStamp));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  long lJulian, lMilliSec;
  hb_timeStampUnpackDT(dTimeStamp, &lJulian, &lMilliSec);
  pItem->setType(Harbour::Item::TIMESTAMP);
  pItem->item.asDateTime.julian = lJulian;
  pItem->item.asDateTime.time = lMilliSec;

  return pItem;
}

PHB_ITEM hb_itemPutTDT(PHB_ITEM pItem, long lJulian, long lMilliSec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutTDT(%p, %ld, %ld)", static_cast<void*>(pItem), lJulian, lMilliSec));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::TIMESTAMP);
  pItem->item.asDateTime.julian = lJulian;
  pItem->item.asDateTime.time = lMilliSec;

  return pItem;
}

PHB_ITEM hb_itemPutL(PHB_ITEM pItem, HB_BOOL bValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutL(%p, %d)", static_cast<void*>(pItem), static_cast<int>(bValue)));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::LOGICAL);
  pItem->setLogicalValue(bValue);

  return pItem;
}

PHB_ITEM _HB_ITEM::putL(HB_BOOL bValue) // equivalent to hb_itemPutL
{
  if (this->isComplex())
  {
    hb_itemClear(this);
  }

  this->setType(Harbour::Item::LOGICAL);
  this->setLogicalValue(bValue);

  return this;
}

PHB_ITEM hb_itemPutND(PHB_ITEM pItem, double dNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutND(%p, %lf)", static_cast<void*>(pItem), dNumber));
#endif

  HB_STACK_TLS_PRELOAD

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::DOUBLE);
  pItem->item.asDouble.length = HB_DBL_LENGTH(dNumber);
  pItem->item.asDouble.decimal = static_cast<HB_USHORT>(hb_stackSetStruct()->HB_SET_DECIMALS);
  pItem->item.asDouble.value = dNumber;

  return pItem;
}

PHB_ITEM hb_itemPutNI(PHB_ITEM pItem, int iNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNI(%p, %d)", static_cast<void*>(pItem), iNumber));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::INTEGER);
  pItem->item.asInteger.value = iNumber;
  pItem->item.asInteger.length = HB_INT_LENGTH(iNumber);

  return pItem;
}

PHB_ITEM _HB_ITEM::putNI(int iNumber) // equivalent to hb_itemPutNI
{
  if (this->isComplex())
  {
    hb_itemClear(this);
  }

  this->setType(Harbour::Item::INTEGER);
  this->item.asInteger.value = iNumber;
  this->item.asInteger.length = HB_INT_LENGTH(iNumber);

  return this;
}

PHB_ITEM hb_itemPutNL(PHB_ITEM pItem, long lNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNL(%p, %ld)", static_cast<void*>(pItem), lNumber));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  HB_ITEM_PUT_LONGRAW(pItem, lNumber);

  return pItem;
}

PHB_ITEM hb_itemPutNS(PHB_ITEM pItem, HB_ISIZ nNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNS(%p, %" HB_PFS "d)", static_cast<void*>(pItem), nNumber));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

#if HB_SIZE_MAX <= HB_VMUINT_MAX
  pItem->setType(Harbour::Item::INTEGER);
  pItem->item.asInteger.value = nNumber;
  // EXP limit used intentionally
  pItem->item.asInteger.length = HB_INT_EXPLENGTH(nNumber);
#else
  if (HB_LIM_INT(nNumber))
  {
    pItem->setType(Harbour::Item::INTEGER);
    pItem->item.asInteger.value = static_cast<int>(nNumber);
    // EXP limit used intentionally
    pItem->item.asInteger.length = HB_INT_EXPLENGTH(nNumber);
  }
  else
  {
    pItem->setType(Harbour::Item::LONG);
    pItem->item.asLong.value = nNumber;
    pItem->item.asLong.length = HB_LONG_LENGTH(nNumber);
  }
#endif

  return pItem;
}

#ifndef HB_LONG_LONG_OFF
PHB_ITEM hb_itemPutNLL(PHB_ITEM pItem, HB_LONGLONG llNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNL(%p, %" PFLL "d)", static_cast<void*>(pItem), llNumber));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

#if HB_VMLONG_MAX >= LONGLONG_MAX
  pItem->setType(Harbour::Item::LONG);
  pItem->item.asLong.value = static_cast<HB_MAXINT>(llNumber);
  pItem->item.asLong.length = HB_LONG_LENGTH(llNumber);
#else
  pItem->setType(Harbour::Item::DOUBLE);
  pItem->item.asDouble.value = static_cast<double>(llNumber);
  pItem->item.asDouble.length = HB_DBL_LENGTH(pItem->item.asDouble.value);
  pItem->item.asDouble.decimal = 0;
#endif
  return pItem;
}
#endif

PHB_ITEM hb_itemPutNInt(PHB_ITEM pItem, HB_MAXINT nNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNInt(%p, %" PFHL "d)", static_cast<void*>(pItem), nNumber));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  if (HB_LIM_INT(nNumber))
  {
    pItem->setType(Harbour::Item::INTEGER);
    pItem->item.asInteger.value = static_cast<int>(nNumber);
    // EXP limit used intentionally
    pItem->item.asInteger.length = HB_INT_EXPLENGTH(nNumber);
  }
  else
  {
    pItem->setType(Harbour::Item::LONG);
    pItem->item.asLong.value = nNumber;
    pItem->item.asLong.length = HB_LONG_LENGTH(nNumber);
  }

  return pItem;
}

PHB_ITEM hb_itemPutNIntLen(PHB_ITEM pItem, HB_MAXINT nNumber, int iWidth)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNIntLen(%p, %" PFHL "d, %d)", static_cast<void*>(pItem), nNumber, iWidth));
#endif

  if (HB_LIM_INT(nNumber))
  {
    return hb_itemPutNILen(pItem, static_cast<int>(nNumber), iWidth);
  }
  else
  {
#ifdef HB_LONG_LONG_OFF
    return hb_itemPutNLLen(pItem, static_cast<long>(nNumber), iWidth);
#else
    return hb_itemPutNLLLen(pItem, static_cast<HB_LONGLONG>(nNumber), iWidth);
#endif
  }
}

PHB_ITEM hb_itemPutNLen(PHB_ITEM pItem, double dNumber, int iWidth, int iDec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNLen(%p, %lf, %d, %d)", static_cast<void*>(pItem), dNumber, iWidth, iDec));
#endif

  if (iDec < 0)
  {
    HB_STACK_TLS_PRELOAD
    iDec = hb_stackSetStruct()->HB_SET_DECIMALS;
  }

  if (iDec == 0)
  {
    auto nNumber = static_cast<HB_MAXINT>(dNumber);

    if (static_cast<double>(nNumber) == dNumber)
    {
      if (iWidth <= 0 || iWidth >= HB_DEFAULT_WIDTH)
      {
        iWidth = HB_DBL_LENGTH(dNumber);
      }

      return hb_itemPutNIntLen(pItem, nNumber, iWidth);
    }
  }

  return hb_itemPutNDLen(pItem, dNumber, iWidth, iDec);
}

PHB_ITEM hb_itemPutNDLen(PHB_ITEM pItem, double dNumber, int iWidth, int iDec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNDLen(%p, %lf, %d, %d)", static_cast<void*>(pItem), dNumber, iWidth, iDec));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  if (iWidth <= 0 || iWidth >= HB_DEFAULT_WIDTH)
  {
    iWidth = HB_DBL_LENGTH(dNumber);
  }

  if (iDec < 0)
  {
    HB_STACK_TLS_PRELOAD
    iDec = hb_stackSetStruct()->HB_SET_DECIMALS;
  }

  pItem->setType(Harbour::Item::DOUBLE);
  pItem->item.asDouble.length = static_cast<HB_USHORT>(iWidth);
  pItem->item.asDouble.decimal = static_cast<HB_USHORT>(iDec);
  pItem->item.asDouble.value = dNumber;

  return pItem;
}

PHB_ITEM hb_itemPutNDDec(PHB_ITEM pItem, double dNumber, int iDec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNDDec(%p, %lf, %i)", static_cast<void*>(pItem), dNumber, iDec));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::DOUBLE);
  pItem->item.asDouble.length = HB_DBL_LENGTH(dNumber);

  if (iDec == HB_DEFAULT_DECIMALS)
  {
    HB_STACK_TLS_PRELOAD
    pItem->item.asDouble.decimal = static_cast<HB_USHORT>(hb_stackSetStruct()->HB_SET_DECIMALS);
  }
  else
  {
    pItem->item.asDouble.decimal = static_cast<HB_USHORT>(iDec);
  }

  pItem->item.asDouble.value = dNumber;

  return pItem;
}

double hb_itemGetNDDec(PHB_ITEM pItem, int *piDec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNDDec(%p,%p)", static_cast<void*>(pItem), static_cast<void*>(piDec)));
#endif

  if (pItem->isInteger())
  {
    *piDec = 0;
    return static_cast<double>(pItem->item.asInteger.value);
  }
  else if (pItem->isLong())
  {
    *piDec = 0;
    return static_cast<double>(pItem->item.asLong.value);
  }
  else if (pItem->isDouble())
  {
    *piDec = pItem->item.asDouble.decimal;
    return pItem->item.asDouble.value;
  }

  *piDec = 0;
  return 0.0;
}

PHB_ITEM hb_itemPutNILen(PHB_ITEM pItem, int iNumber, int iWidth)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNILen(%p, %d, %d)", static_cast<void*>(pItem), iNumber, iWidth));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  if (iWidth <= 0 || iWidth >= HB_DEFAULT_WIDTH)
  {
    iWidth = HB_INT_LENGTH(iNumber);
  }

  pItem->setType(Harbour::Item::INTEGER);
  pItem->item.asInteger.length = static_cast<HB_USHORT>(iWidth);
  pItem->item.asInteger.value = iNumber;

  return pItem;
}

PHB_ITEM hb_itemPutNLLen(PHB_ITEM pItem, long lNumber, int iWidth)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNLLen(%p, %ld, %d)", static_cast<void*>(pItem), lNumber, iWidth));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

#if HB_VMINT_MAX == LONG_MAX
  if (iWidth <= 0 || iWidth >= HB_DEFAULT_WIDTH)
  {
    iWidth = HB_INT_LENGTH(lNumber);
  }

  pItem->setType(Harbour::Item::INTEGER);
  pItem->item.asInteger.value = static_cast<int>(lNumber);
  pItem->item.asInteger.length = static_cast<HB_USHORT>(iWidth);
#else
  if (iWidth <= 0 || iWidth >= HB_DEFAULT_WIDTH)
  {
    iWidth = HB_LONG_LENGTH(lNumber);
  }

  pItem->setType(Harbour::Item::LONG);
  pItem->item.asLong.value = static_cast<HB_MAXINT>(lNumber);
  pItem->item.asLong.length = iWidth;
#endif

  return pItem;
}

#ifndef HB_LONG_LONG_OFF
PHB_ITEM hb_itemPutNLLLen(PHB_ITEM pItem, HB_LONGLONG llNumber, int iWidth)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNLLLen(%p, %" PFLL "d, %d)", static_cast<void*>(pItem), llNumber, iWidth));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

#if HB_VMLONG_MAX >= LONGLONG_MAX
  if (iWidth <= 0 || iWidth >= HB_DEFAULT_WIDTH)
  {
    iWidth = HB_LONG_LENGTH(llNumber);
  }

  pItem->setType(Harbour::Item::LONG);
  pItem->item.asLong.value = static_cast<HB_MAXINT>(llNumber);
  pItem->item.asLong.length = static_cast<HB_USHORT>(iWidth);
#else
  pItem->setType(Harbour::Item::DOUBLE);
  pItem->item.asDouble.value = static_cast<double>(llNumber);
  if (iWidth <= 0 || iWidth >= HB_DEFAULT_WIDTH)
  {
    iWidth = HB_LONG_LENGTH(pItem->item.asDouble.value);
  }
  pItem->item.asDouble.length = iWidth;
  pItem->item.asDouble.decimal = 0;
#endif

  return pItem;
}
#endif

PHB_ITEM hb_itemPutNumType(PHB_ITEM pItem, double dNumber, int iDec, int iType1, int iType2)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutNumType(%p, %lf, %d, %i, %i)", static_cast<void*>(pItem), dNumber, iDec, iType1, iType2));
#endif

  if (iDec || iType1 & Harbour::Item::DOUBLE || iType2 & Harbour::Item::DOUBLE)
  {
    return hb_itemPutNDDec(pItem, dNumber, iDec);
  }
  else if (HB_DBL_LIM_INT(dNumber))
  {
    return hb_itemPutNI(pItem, static_cast<int>(dNumber));
  }
  else if (HB_DBL_LIM_LONG(dNumber))
  {
#ifdef HB_LONG_LONG_OFF
    return hb_itemPutNL(pItem, static_cast<long>(static_cast<unsigned long>(dNumber)));
#else
    return hb_itemPutNLL(pItem, static_cast<HB_LONGLONG>(dNumber));
#endif
  }
  else
  {
    return hb_itemPutND(pItem, dNumber);
  }
}

PHB_ITEM hb_itemPutPtr(PHB_ITEM pItem, void *pValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutPtr(%p, %p)", static_cast<void*>(pItem), pValue));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::POINTER);
  pItem->item.asPointer.value = pValue;
  pItem->item.asPointer.collect = pItem->item.asPointer.single = false;

  return pItem;
}

PHB_ITEM hb_itemPutPtrGC(PHB_ITEM pItem, void *pValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutPtrGC(%p, %p)", static_cast<void*>(pItem), pValue));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::POINTER);
  pItem->item.asPointer.value = pValue;
  pItem->item.asPointer.collect = true;
  pItem->item.asPointer.single = false;

  hb_gcAttach(pValue);

  return pItem;
}

PHB_ITEM hb_itemPutPtrRawGC(PHB_ITEM pItem, void *pValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutPtrRawGC(%p, %p)", static_cast<void*>(pItem), pValue));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::POINTER);
  pItem->item.asPointer.value = pValue;
  pItem->item.asPointer.collect = true;
  pItem->item.asPointer.single = false;

  return pItem;
}

PHB_ITEM hb_itemPutSymbol(PHB_ITEM pItem, PHB_SYMB pSym)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutSymbol(%p,%p)", static_cast<void*>(pItem), static_cast<void*>(pSym)));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  pItem->setType(Harbour::Item::SYMBOL);
  pItem->item.asSymbol.value = pSym;
  pItem->item.asSymbol.stackstate = nullptr;
  pItem->item.asSymbol.paramcnt = pItem->item.asSymbol.paramdeclcnt = 0;

  return pItem;
}

void hb_itemGetNLen(PHB_ITEM pItem, int *piWidth, int *piDecimal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetNLen(%p, %p, %p)", static_cast<void*>(pItem), static_cast<void*>(piWidth), static_cast<void*>(piDecimal)));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isDouble())
    {
      if (piWidth)
      {
        *piWidth = static_cast<int>(pItem->item.asDouble.length);
      }
      if (piDecimal)
      {
        *piDecimal = static_cast<int>(pItem->item.asDouble.decimal);
      }
    }
    else if (pItem->isInteger())
    {
      if (piWidth)
      {
        *piWidth = static_cast<int>(pItem->item.asInteger.length);
      }
      if (piDecimal)
      {
        *piDecimal = 0;
      }
    }
    else if (pItem->isLong())
    {
      if (piWidth)
      {
        *piWidth = static_cast<int>(pItem->item.asLong.length);
      }
      if (piDecimal)
      {
        *piDecimal = 0;
      }
    }
    else
    {
      if (piWidth)
      {
        *piWidth = 0;
      }
      if (piDecimal)
      {
        *piDecimal = 0;
      }
    }
  }
}

HB_SIZE hb_itemSize(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemSize(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isString())
    {
      return pItem->item.asString.length;
    }
    else if (pItem->isArray())
    {
      return hb_arrayLen(pItem);
    }
    else if (pItem->isHash())
    {
      return hb_hashLen(pItem);
    }
  }

  return 0;
}

HB_TYPE hb_itemType(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemType(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    return static_cast<HB_TYPE>(HB_ITEM_TYPE(pItem));
  }
  else
  {
    return Harbour::Item::NIL;
  }
}

const char *hb_itemTypeStr(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemTypeStr(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    switch (HB_ITEM_TYPE(pItem))
    {
    case Harbour::Item::ARRAY:
      return hb_arrayIsObject(pItem) ? "O" : "A";

    case Harbour::Item::BLOCK:
      return "B";

    case Harbour::Item::DATE:
      return "D";

    case Harbour::Item::TIMESTAMP:
      return "T";

    case Harbour::Item::LOGICAL:
      return "L";

    case Harbour::Item::INTEGER:
    case Harbour::Item::LONG:
    case Harbour::Item::DOUBLE:
      return "N";

    case Harbour::Item::STRING:
      return "C";

    case Harbour::Item::MEMO:
      return "M";

    case Harbour::Item::HASH:
      return "H";

    case Harbour::Item::POINTER:
      return "P";

    case Harbour::Item::SYMBOL:
      return "S";
    }
  }

  return "U";
}

enum HB_IT_BASIC
{
  HB_IT_U,
  HB_IT_N,
  HB_IT_C,
  HB_IT_L,
  HB_IT_T,
  HB_IT_B,
  HB_IT_H,
  HB_IT_A,
  HB_IT_O,
  HB_IT_P,
  HB_IT_S
};

static HB_IT_BASIC s_hb_itemTypeBasic(PHB_ITEM pItem)
{
  switch (HB_ITEM_TYPE(pItem))
  {
  case Harbour::Item::ARRAY:
    return hb_arrayIsObject(pItem) ? HB_IT_O : HB_IT_A;

  case Harbour::Item::BLOCK:
    return HB_IT_B;

  case Harbour::Item::DATE:
  case Harbour::Item::TIMESTAMP:
    return HB_IT_T;

  case Harbour::Item::LOGICAL:
    return HB_IT_L;

  case Harbour::Item::INTEGER:
  case Harbour::Item::LONG:
  case Harbour::Item::DOUBLE:
    return HB_IT_N;

  case Harbour::Item::STRING:
  case Harbour::Item::MEMO:
    return HB_IT_C;

  case Harbour::Item::HASH:
    return HB_IT_H;

  case Harbour::Item::POINTER:
    return HB_IT_P;

  case Harbour::Item::SYMBOL:
    return HB_IT_S;
  }

  return HB_IT_U;
}

HB_BOOL hb_itemTypeCmp(PHB_ITEM pItem1, PHB_ITEM pItem2)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemTypeCmp(%p, %p)", static_cast<void*>(pItem1), static_cast<void*>(pItem2)));
#endif

  return s_hb_itemTypeBasic(pItem1) == s_hb_itemTypeBasic(pItem2);
}

// Internal API, not standard Clipper

void hb_itemInit(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemInit(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem != nullptr)
  {
    pItem->setType(Harbour::Item::NIL);
  }
}

void hb_itemClear(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemClear(%p)", static_cast<void*>(pItem)));
#endif

  HB_TYPE type = HB_ITEM_TYPERAW(pItem);
  pItem->setType(Harbour::Item::NIL);

  // GCLOCK enter
  if (type & Harbour::Item::STRING)
  {
    if (pItem->item.asString.allocated)
    {
      hb_xRefFree(pItem->item.asString.value);
    }
  }
  else if (type & Harbour::Item::ARRAY)
  {
    hb_gcRefFree(pItem->item.asArray.value);
  }
  else if (type & Harbour::Item::BLOCK)
  {
    hb_gcRefFree(pItem->item.asBlock.value);
  }
  else if (type & Harbour::Item::HASH)
  {
    hb_gcRefFree(pItem->item.asHash.value);
  }
  else if (type & Harbour::Item::BYREF)
  {
    if (type & Harbour::Item::MEMVAR)
    {
      hb_memvarValueDecRef(pItem->item.asMemvar.value);
    }
    else if (type & Harbour::Item::ENUM)
    { // FOR EACH control variable
      hb_vmEnumRelease(pItem->item.asEnum.basePtr, pItem->item.asEnum.valuePtr);
    }
    else if (type & Harbour::Item::EXTREF)
    {
      pItem->item.asExtRef.func->clear(pItem->item.asExtRef.value);
    }
    else if (pItem->item.asRefer.offset == 0 && pItem->item.asRefer.value >= 0)
    {
      hb_gcRefFree(pItem->item.asRefer.BasePtr.array);
    }
  }
  else if (type & Harbour::Item::POINTER)
  {
    if (pItem->item.asPointer.collect)
    {
      hb_gcRefFree(pItem->item.asPointer.value);
    }
  }
  // GCLOCK leave
}

// Internal API, not standard Clipper

void hb_itemCopy(PHB_ITEM pDest, PHB_ITEM pSource)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopy(%p, %p)", static_cast<void*>(pDest), static_cast<void*>(pSource)));
#endif

  if (pDest == pSource)
  {
    hb_errInternal(HB_EI_ITEMBADCOPY, nullptr, "hb_itemCopy()", nullptr);
  }

  if (pDest->isComplex())
  {
    hb_itemClear(pDest);
  }

  hb_itemRawCpy(pDest, pSource);
  pDest->type &= ~Harbour::Item::DEFAULT;

  if (pSource->isComplex())
  {
    // GCLOCK enter
    if (pSource->isString())
    {
      if (pSource->item.asString.allocated)
      {
        hb_xRefInc(pSource->item.asString.value);
      }
    }
    else if (pSource->isArray())
    {
      hb_gcRefInc(pSource->item.asArray.value);
    }
    else if (pSource->isBlock())
    {
      hb_gcRefInc(pSource->item.asBlock.value);
    }
    else if (pSource->isHash())
    {
      hb_gcRefInc(pSource->item.asHash.value);
    }
    else if (pSource->isByRef())
    {
      if (pSource->isMemVar())
      {
        hb_memvarValueIncRef(pSource->item.asMemvar.value);
      }
      else if (pSource->isEnum())
      { // enumerators cannot be copied
        pDest->setType(Harbour::Item::NIL);
      }
      else if (pSource->isExtRef())
      {
        pSource->item.asExtRef.func->copy(pDest);
      }
      else if (pSource->item.asRefer.offset == 0 && pSource->item.asRefer.value >= 0)
      {
        hb_gcRefInc(pSource->item.asRefer.BasePtr.array);
      }
    }
    else if (pSource->isPointer())
    {
      if (pSource->item.asPointer.collect)
      {
        if (pSource->item.asPointer.single)
        {
          pDest->item.asPointer.collect = false;
        }
        else
        {
          hb_gcRefInc(pSource->item.asPointer.value);
        }
      }
    }
    // GCLOCK leave
  }
}

// Internal API, not standard Clipper

void hb_itemCopyToRef(PHB_ITEM pDest, PHB_ITEM pSource)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyToRef(%p, %p)", static_cast<void*>(pDest), static_cast<void*>(pSource)));
#endif

  if (pDest->isByRef())
  {
    pDest = hb_itemUnRefWrite(pDest, pSource);
    if (!pDest || pDest == pSource)
    {
      // extended reference or pDest is a reference to pSource
      // - do not copy
      return;
    }
  }

  if (pSource->isByRef())
  {
    if (hb_itemUnRef(pSource) == pDest)
    {
      // assign will create cyclic reference
      // pSource and pDest reference to the same item
      // we can simply drop coping
      return;
    }
  }

  if (pDest->isObject() && hb_objOperatorCall(HB_OO_OP_ASSIGN, pDest, pDest, pSource, nullptr))
  {
    return;
  }

  hb_itemCopy(pDest, pSource);
}

// Internal API, not standard Clipper

void hb_itemCopyFromRef(PHB_ITEM pDest, PHB_ITEM pSource)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyFromRef(%p, %p)", static_cast<void*>(pDest), static_cast<void*>(pSource)));
#endif

  if (pSource->isByRef())
  {
    pSource = hb_itemUnRef(pSource);
    if (pDest == pSource)
    {
      // pSource is a reference to pDest - do not copy
      return;
    }
  }

  hb_itemCopy(pDest, pSource);
}

// copy (transfer) the value of item without increasing
// a reference counters, the pSource item is cleared
void hb_itemMove(PHB_ITEM pDest, PHB_ITEM pSource)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemMove(%p, %p)", static_cast<void*>(pDest), static_cast<void*>(pSource)));
#endif

  if (pDest == pSource)
  {
    hb_errInternal(HB_EI_ITEMBADCOPY, nullptr, "hb_itemMove()", nullptr);
  }

  if (pDest->isComplex())
  {
    hb_itemClear(pDest);
  }

  // GCLOCK enter
  hb_itemRawCpy(pDest, pSource);
  pDest->type &= ~Harbour::Item::DEFAULT;
  pSource->setType(Harbour::Item::NIL);
  // GCLOCK leave
}

// Internal API, not standard Clipper

void hb_itemMoveRef(PHB_ITEM pDest, PHB_ITEM pSource)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemMoveRef(%p, %p)", static_cast<void*>(pDest), static_cast<void*>(pSource)));
#endif

  if (pSource->isByRef())
  {
    if (hb_itemUnRef(pSource) == (pDest->isByRef() ? hb_itemUnRef(pDest) : pDest))
    {
      // assign will create cyclic reference
      // pSource is a reference to pDest
      // we can simply drop coping
      hb_itemSetNil(pSource);
      return;
    }
  }

  if (pDest->isComplex())
  {
    hb_itemClear(pDest);
  }

  // GCLOCK enter
  hb_itemRawCpy(pDest, pSource);
  pDest->type &= ~Harbour::Item::DEFAULT;
  pSource->setType(Harbour::Item::NIL);
  // GCLOCK leave
}

// Internal API, not standard Clipper

void hb_itemMoveToRef(PHB_ITEM pDest, PHB_ITEM pSource)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemMoveToRef(%p, %p)", static_cast<void*>(pDest), static_cast<void*>(pSource)));
#endif

  if (pDest->isByRef())
  {
    pDest = hb_itemUnRefWrite(pDest, pSource);
    if (!pDest || pDest == pSource)
    {
      // extended reference or pDest is a reference to pSource
      // - do not copy
      hb_itemSetNil(pSource);
      return;
    }
  }

  if (pSource->isByRef())
  {
    if (hb_itemUnRef(pSource) == pDest)
    {
      // assign will create cyclic reference
      // pSource and pDest reference to the same item
      // we can simply drop coping
      hb_itemSetNil(pSource);
      return;
    }
  }

  if (pDest->isObject() && hb_objOperatorCall(HB_OO_OP_ASSIGN, pDest, pDest, pSource, nullptr))
  {
    hb_itemSetNil(pSource);
    return;
  }

  if (pDest->isComplex())
  {
    hb_itemClear(pDest);
  }

  // GCLOCK enter
  hb_itemRawCpy(pDest, pSource);
  pDest->type &= ~Harbour::Item::DEFAULT;
  pSource->setType(Harbour::Item::NIL);
  // GCLOCK leave
}

void hb_itemMoveFromRef(PHB_ITEM pDest, PHB_ITEM pSource)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemMoveFromRef(%p, %p)", static_cast<void*>(pDest), static_cast<void*>(pSource)));
#endif

  if (pSource->isByRef())
  {
    auto pUnRef = hb_itemUnRef(pSource);
    if (pDest != pUnRef)
    {
      // pSource is not a reference to pDest - make copy
      hb_itemCopy(pDest, pUnRef);
    }
    hb_itemClear(pSource);
  }
  else
  {
    hb_itemMove(pDest, pSource);
  }
}

// Internal API, not standard Clipper

void hb_itemSwap(PHB_ITEM pItem1, PHB_ITEM pItem2)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemSwap(%p, %p)", static_cast<void*>(pItem1), static_cast<void*>(pItem2)));
#endif

  HB_ITEM temp;

  // It's safe to use this version because our GC cannot be
  // activated inside memcpy()

  // GCLOCK enter
  hb_itemRawCpy(&temp, pItem2);
  hb_itemRawCpy(pItem2, pItem1);
  hb_itemRawCpy(pItem1, &temp);
  pItem1->type &= ~Harbour::Item::DEFAULT;
  pItem2->type &= ~Harbour::Item::DEFAULT;
  // GCLOCK leave
}

// Internal API, not standard Clipper
// De-references item passed by the reference

PHB_ITEM hb_itemUnRefOnce(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnRefOnce(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem->isByRef())
  {
    if (pItem->isMemVar())
    {
      pItem = pItem->item.asMemvar.value;
    }
    else if (pItem->isEnum())
    { // FOR EACH control variable
      // enumerator variable
      if (pItem->item.asEnum.valuePtr)
      {
        return pItem->item.asEnum.valuePtr;
      }
      else
      {
        PHB_ITEM pBase = pItem->item.asEnum.basePtr->isByRef() ? hb_itemUnRef(pItem->item.asEnum.basePtr)
                                                               : pItem->item.asEnum.basePtr;
        if (pBase->isArray())
        {
          pBase = hb_arrayGetItemPtr(pBase, pItem->item.asEnum.offset);
          if (pBase)
          {
            return pBase;
          }
        }
        else if (pBase->isHash())
        {
          pBase = hb_hashGetValueAt(pBase, pItem->item.asEnum.offset);
          if (pBase)
          {
            return pBase;
          }
        }
        else if (pBase->isString())
        {
          if (pItem->item.asEnum.offset > 0 &&
              static_cast<HB_SIZE>(pItem->item.asEnum.offset) <= pBase->item.asString.length)
          {
            pItem->item.asEnum.valuePtr =
                hb_itemPutCL(nullptr, pBase->item.asString.value + pItem->item.asEnum.offset - 1, 1);
            return pItem->item.asEnum.valuePtr;
          }
        }

        // put it here to avoid recursive RT error generation
        pItem->item.asEnum.valuePtr = hb_itemNew(nullptr);

        if (hb_vmRequestQuery() == 0)
        {
          HB_STACK_TLS_PRELOAD
          hb_itemPutNS(hb_stackAllocItem(), pItem->item.asEnum.offset);
          hb_errRT_BASE(EG_BOUND, 1132, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, pItem->item.asEnum.basePtr,
                        hb_stackItemFromTop(-1));
          hb_stackPop();
        }
        return pItem->item.asEnum.valuePtr;
      }
    }
    else if (pItem->isExtRef())
    {
      pItem = pItem->item.asExtRef.func->read(pItem);
    }
    else
    {
      if (pItem->item.asRefer.value >= 0)
      {
        if (pItem->item.asRefer.offset == 0)
        {
          // a reference to a static variable or array item
          if (static_cast<HB_SIZE>(pItem->item.asRefer.value) < pItem->item.asRefer.BasePtr.array->nLen)
          {
            pItem = pItem->item.asRefer.BasePtr.array->pItems + pItem->item.asRefer.value;
          }
          else if (hb_vmRequestQuery() == 0)
          {
            HB_STACK_TLS_PRELOAD
            hb_arrayPushBase(pItem->item.asRefer.BasePtr.array);
            hb_itemPutNS(hb_stackAllocItem(), pItem->item.asRefer.value + 1);
            hb_errRT_BASE(EG_BOUND, 1132, nullptr, hb_langDGetErrorDesc(EG_ARRACCESS), 2, hb_stackItemFromTop(-2),
                          hb_stackItemFromTop(-1));
            hb_stackPop();
            hb_stackPop();

            // check it again - user error handler can resize the array
            if (static_cast<HB_SIZE>(pItem->item.asRefer.value) < pItem->item.asRefer.BasePtr.array->nLen)
            {
              pItem = pItem->item.asRefer.BasePtr.array->pItems + pItem->item.asRefer.value;
            }
            else
            {
              // It's safe to clear the item - if we are here then
              // the reference chain to this item does not start in
              // one of the pItem->item.asRefer.BasePtr.array items
              // or more then one reference to this array exists
              // so it will not be freed [druzus]
              hb_itemClear(pItem);
            }
          }
        }
        else
        {
          // a reference to a local variable
          PHB_ITEM *pLocal;

          pLocal = *(pItem->item.asRefer.BasePtr.itemsbasePtr) + pItem->item.asRefer.offset + pItem->item.asRefer.value;
          pItem = *pLocal;
        }
      }
      else
      {
        // local variable referenced in a codeblock
        pItem = hb_codeblockGetRef(pItem->item.asRefer.BasePtr.block, static_cast<int>(pItem->item.asRefer.value));
      }
    }
  }

  return pItem;
}

// Internal API, not standard Clipper
// De-references item passed by the reference

PHB_ITEM hb_itemUnRef(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnRef(%p)", static_cast<void*>(pItem)));
#endif

  do
  {
    pItem = hb_itemUnRefOnce(pItem);
  } while (pItem->isByRef());

  return pItem;
}

// Unreference passed variable for writing
// Do not unreference string enumerators
PHB_ITEM hb_itemUnRefWrite(PHB_ITEM pItem, PHB_ITEM pSource)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnRefWrite(%p,%p)", static_cast<void*>(pItem), static_cast<void*>(pSource)));
#endif

  if (pItem->isExtRef())
  {
    pItem = pItem->item.asExtRef.func->write(pItem, pSource);
  }
  else if (pSource->isString() && pSource->item.asString.length == 1)
  {
    do
    {
      if (pItem->isEnum() && pItem->item.asEnum.basePtr->isByRef() && pItem->item.asEnum.offset >= 1)
      {
        auto pBase = hb_itemUnRef(pItem->item.asEnum.basePtr);
        if (pBase->isString() && static_cast<HB_SIZE>(pItem->item.asEnum.offset) <= pBase->item.asString.length)
        {
          hb_itemUnShareString(pBase);
          pBase->item.asString.value[pItem->item.asEnum.offset - 1] = pSource->item.asString.value[0];
          return pItem->item.asEnum.valuePtr;
        }
      }
      pItem = hb_itemUnRefOnce(pItem);
    } while (pItem->isByRef());
  }
  else
  {
    pItem = hb_itemUnRef(pItem);
  }

  return pItem;
}

// Unreference passed variable
// Do not unreference the last reference stored
PHB_ITEM hb_itemUnRefRefer(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnRefRefer(%p)", static_cast<void*>(pItem)));
#endif

  PHB_ITEM pLast;

  do
  {
    pLast = pItem;
    pItem = hb_itemUnRefOnce(pItem);
  } while (pItem->isByRef());

  return pLast;
}

// Internal API, not standard Clipper
// Resize string buffer of given string item

PHB_ITEM hb_itemReSizeString(PHB_ITEM pItem, HB_SIZE nSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemReSizeString(%p,%" HB_PFS "u)", static_cast<void*>(pItem), nSize));
#endif

  if (pItem->item.asString.allocated == 0)
  {
    auto szText = static_cast<char *>(hb_xgrab(nSize + 1));
    hb_xmemcpy(szText, pItem->item.asString.value, pItem->item.asString.length);
    szText[nSize] = '\0';
    pItem->item.asString.value = szText;
    pItem->item.asString.length = nSize;
    pItem->item.asString.allocated = nSize + 1;
  }
  else
  {
    HB_SIZE nAlloc = nSize + 1 + (pItem->item.asString.allocated <= nSize ? nSize : 0);
    pItem->item.asString.value = static_cast<char *>(hb_xRefResize(
        pItem->item.asString.value, pItem->item.asString.length, nAlloc, &pItem->item.asString.allocated));
    pItem->item.asString.length = nSize;
    pItem->item.asString.value[nSize] = '\0';
  }
  pItem->type &= ~Harbour::Item::DEFAULT;

  return pItem;
}

// Internal API, not standard Clipper
// UnShare string buffer of given string item

PHB_ITEM hb_itemUnShareString(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnShareString(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem->item.asString.allocated == 0 || hb_xRefCount(pItem->item.asString.value) > 1)
  {
    HB_SIZE nLen = pItem->item.asString.length + 1;
    auto szText = static_cast<char *>(hb_xmemcpy(hb_xgrab(nLen), pItem->item.asString.value, nLen));
    if (pItem->item.asString.allocated)
    {
      // GCLOCK enter
      hb_xRefFree(pItem->item.asString.value);
      // GCLOCK leave
    }
    pItem->item.asString.value = szText;
    pItem->item.asString.allocated = nLen;
  }
  pItem->type &= ~Harbour::Item::DEFAULT;

  return pItem;
}

PHB_ITEM hb_itemUnShare(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemUnShare(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem->isByRef())
  {
    pItem = hb_itemUnRef(pItem);
  }

  if (pItem->isString())
  {
    return hb_itemUnShareString(pItem);
  }
  else
  {
    return pItem;
  }
}

HB_BOOL hb_itemGetWriteCL(PHB_ITEM pItem, char **pszValue, HB_SIZE *pnLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetWriteCL(%p,%p,%p)", static_cast<void*>(pItem), static_cast<void*>(pszValue), static_cast<void*>(pnLen)));
#endif

  if (pItem != nullptr)
  {
    if (pItem->isByRef())
    {
      pItem = hb_itemUnRef(pItem);
    }

    if (pItem->isString())
    {
      hb_itemUnShareString(pItem);
      *pnLen = pItem->item.asString.length;
      *pszValue = pItem->item.asString.value;
      return true;
    }
  }
  return false;
}

// Internal API, not standard Clipper
// clone the given item
PHB_ITEM hb_itemClone(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemClone(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem->isArray())
  {
    if (pItem->isObject())
    {
      return hb_objCloneTo(hb_itemNew(nullptr), pItem);
    }
    else
    {
      return hb_arrayClone(pItem);
    }
  }
  else if (pItem->isHash())
  {
    return hb_hashClone(pItem);
  }
  else
  {
    return hb_itemNew(pItem);
  }
}

void hb_itemCloneTo(PHB_ITEM pDest, PHB_ITEM pSource)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCloneTo(%p,%p)", static_cast<void*>(pDest), static_cast<void*>(pSource)));
#endif

  if (pSource->isArray())
  {
    if (pSource->isObject())
    {
      hb_objCloneTo(pDest, pSource);
    }
    else
    {
      hb_arrayCloneTo(pDest, pSource);
    }
  }
  else if (pSource->isHash())
  {
    hb_hashCloneTo(pDest, pSource);
  }
  else
  {
    hb_itemCopy(pDest, pSource);
  }
}

// Check whether two items are exactly equal
HB_BOOL hb_itemEqual(PHB_ITEM pItem1, PHB_ITEM pItem2)
{
  auto fResult = false;

  if (pItem1->isNumeric())
  {
    if (pItem1->isNumInt() && pItem2->isNumInt())
    {
      fResult = HB_ITEM_GET_NUMINTRAW(pItem1) == HB_ITEM_GET_NUMINTRAW(pItem2);
    }
    else
    {
      fResult = pItem2->isNumeric() && pItem1->getND() == pItem2->getND();
    }
  }
  else if (pItem1->isString())
  {
    fResult = pItem2->isString() && pItem1->item.asString.length == pItem2->item.asString.length &&
              memcmp(pItem1->item.asString.value, pItem2->item.asString.value, pItem1->item.asString.length) == 0;
  }
  else if (pItem1->isNil())
  {
    fResult = pItem2->isNil();
  }
  else if (pItem1->isDateTime())
  {
    fResult = pItem2->isDateTime() && pItem1->item.asDateTime.julian == pItem2->item.asDateTime.julian &&
              pItem1->item.asDateTime.time == pItem2->item.asDateTime.time;
  }
  else if (pItem1->isLogical())
  {
    fResult = pItem2->isLogical() &&
              (pItem1->logicalValue() ? pItem2->logicalValue() : !pItem2->logicalValue());
  }
  else if (pItem1->isArray())
  {
    fResult = pItem2->isArray() && pItem1->item.asArray.value == pItem2->item.asArray.value;
  }
  else if (pItem1->isHash())
  {
    fResult = pItem2->isHash() && pItem1->item.asHash.value == pItem2->item.asHash.value;
  }
  else if (pItem1->isPointer())
  {
    fResult = pItem2->isPointer() && pItem1->item.asPointer.value == pItem2->item.asPointer.value;
  }
  else if (pItem1->isBlock())
  {
    fResult = pItem2->isBlock() && pItem1->item.asBlock.value == pItem2->item.asBlock.value;
  }
  else if (pItem1->isSymbol())
  {
    fResult = pItem2->isSymbol() && (pItem1->item.asSymbol.value == pItem2->item.asSymbol.value ||
                                     (pItem1->item.asSymbol.value->pDynSym != nullptr &&
                                      pItem1->item.asSymbol.value->pDynSym == pItem2->item.asSymbol.value->pDynSym));
  }
  return fResult;
}

// For compatible types compare pItem1 with pItem2 setting piResult
// to -1, 0 or 1 if pItem1 is <, == or > then pItem2 and return true
// otherwise return false.
HB_BOOL hb_itemCompare(PHB_ITEM pItem1, PHB_ITEM pItem2, HB_BOOL bForceExact, int *piResult)
{
  auto fResult = false;

  if (pItem1->isNumeric())
  {
    if (pItem1->isNumInt() && pItem2->isNumInt())
    {
      HB_MAXINT n1 = HB_ITEM_GET_NUMINTRAW(pItem1), n2 = HB_ITEM_GET_NUMINTRAW(pItem2);
      *piResult = n1 < n2 ? -1 : (n1 > n2 ? 1 : 0);
      fResult = true;
    }
    else if (pItem2->isNumeric())
    {
      auto d1 = pItem1->getND();
      auto d2 = pItem2->getND();
      *piResult = d1 < d2 ? -1 : (d1 > d2 ? 1 : 0);
      fResult = true;
    }
  }
  else if (pItem1->isString())
  {
    if (pItem2->isString())
    {
      *piResult = hb_itemStrCmp(pItem1, pItem2, bForceExact);
      fResult = true;
    }
  }
  else if (pItem1->isNil())
  {
    if (pItem2->isNil())
    {
      *piResult = 0;
      fResult = true;
    }
  }
  else if (pItem1->isDateTime())
  {
    if (pItem2->isDateTime())
    {
      *piResult = pItem1->item.asDateTime.julian < pItem2->item.asDateTime.julian
                      ? -1
                      : (pItem1->item.asDateTime.julian > pItem2->item.asDateTime.julian
                             ? 1
                             : (pItem1->item.asDateTime.time < pItem2->item.asDateTime.time
                                    ? -1
                                    : (pItem1->item.asDateTime.time > pItem2->item.asDateTime.time ? 1 : 0)));
      fResult = true;
    }
  }
  else if (pItem1->isLogical())
  {
    if (pItem2->isLogical())
    {
      *piResult = pItem1->logicalValue() ? (pItem2->logicalValue() ? 0 : 1)
                                               : (pItem2->logicalValue() ? -1 : 0);
      fResult = true;
    }
  }
  else if (pItem1->isArray())
  {
    if (pItem2->isArray())
    {
      *piResult = pItem1->item.asArray.value < pItem2->item.asArray.value
                      ? -1
                      : (pItem1->item.asArray.value > pItem2->item.asArray.value ? 1 : 0);
      fResult = true;
    }
  }
  else if (pItem1->isHash())
  {
    if (pItem2->isHash())
    {
      *piResult = pItem1->item.asHash.value < pItem2->item.asHash.value
                      ? -1
                      : (pItem1->item.asHash.value > pItem2->item.asHash.value ? 1 : 0);
      fResult = true;
    }
  }
  else if (pItem1->isPointer())
  {
    if (pItem2->isPointer())
    {
      *piResult = pItem1->item.asPointer.value < pItem2->item.asPointer.value
                      ? -1
                      : (pItem1->item.asPointer.value > pItem2->item.asPointer.value ? 1 : 0);
      fResult = true;
    }
  }
  else if (pItem1->isBlock())
  {
    if (pItem2->isBlock())
    {
      *piResult = pItem1->item.asBlock.value < pItem2->item.asBlock.value
                      ? -1
                      : (pItem1->item.asBlock.value > pItem2->item.asBlock.value ? 1 : 0);
      fResult = true;
    }
  }
  else if (pItem1->isSymbol())
  {
    if (pItem2->isSymbol())
    {
      *piResult = (pItem1->item.asSymbol.value == pItem2->item.asSymbol.value ||
                   (pItem1->item.asSymbol.value->pDynSym != nullptr &&
                    pItem1->item.asSymbol.value->pDynSym == pItem2->item.asSymbol.value->pDynSym))
                      ? 0
                      : (pItem1->item.asSymbol.value < pItem2->item.asSymbol.value ? -1 : 1);
      fResult = true;
    }
  }
  return fResult;
}

// Internal API, not standard Clipper

// Check whether two strings are equal (0), smaller (-1), or greater (1)
int hb_itemStrCmp(PHB_ITEM pFirst, PHB_ITEM pSecond, HB_BOOL bForceExact)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemStrCmp(%p, %p, %d)", static_cast<void*>(pFirst), static_cast<void*>(pSecond), static_cast<int>(bForceExact)));
#endif

  HB_STACK_TLS_PRELOAD

  const char *szFirst = pFirst->item.asString.value;
  const char *szSecond = pSecond->item.asString.value;
  HB_SIZE nLenFirst = pFirst->item.asString.length;
  HB_SIZE nLenSecond = pSecond->item.asString.length;

  if (szFirst == szSecond && nLenFirst == nLenSecond)
  {
    return 0;
  }

  if (!bForceExact && hb_stackSetStruct()->HB_SET_EXACT)
  {
    // SET EXACT ON and not using ==
    // Don't include trailing spaces
    while (nLenFirst > nLenSecond && szFirst[nLenFirst - 1] == ' ')
    {
      nLenFirst--;
    }
    while (nLenSecond > nLenFirst && szSecond[nLenSecond - 1] == ' ')
    {
      nLenSecond--;
    }
    bForceExact = true;
  }

  HB_SIZE nMinLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;

  int iRet = 0; // Current status

  // Both strings not empty
  if (nMinLen)
  {
    auto cdp = hb_vmCDP();
    if (cdp && !HB_CDP_ISBINSORT(cdp))
    {
      iRet = hb_cdpcmp(szFirst, nLenFirst, szSecond, nLenSecond, cdp, bForceExact);
    }
    else
    {
      do
      {
        if (*szFirst != *szSecond)
        {
          iRet = (static_cast<HB_UCHAR>(*szFirst) < static_cast<HB_UCHAR>(*szSecond)) ? -1 : 1;
          break;
        }
        szFirst++;
        szSecond++;
      } while (--nMinLen);

      // If equal and length is different !
      if (!iRet && nLenFirst != nLenSecond)
      {
        // Force an exact comparison?
        if (bForceExact || nLenSecond > nLenFirst)
        {
          iRet = (nLenFirst < nLenSecond) ? -1 : 1;
        }
      }
    }
  }
  else
  {
    // Both empty ?
    if (nLenFirst != nLenSecond)
    {
      if (bForceExact)
      {
        iRet = (nLenFirst < nLenSecond) ? -1 : 1;
      }
      else
      {
        iRet = (nLenSecond == 0) ? 0 : -1;
      }
    }
    else
    {
      // Both empty => Equal !
      iRet = 0;
    }
  }

  return iRet;
}

// Check whether two strings are equal (0), smaller (-1), or greater (1), ignore case
int hb_itemStrICmp(PHB_ITEM pFirst, PHB_ITEM pSecond, HB_BOOL bForceExact)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemStrICmp(%p, %p, %d)", static_cast<void*>(pFirst), static_cast<void*>(pSecond), static_cast<int>(bForceExact)));
#endif

  HB_STACK_TLS_PRELOAD

  const char *szFirst = pFirst->item.asString.value;
  const char *szSecond = pSecond->item.asString.value;
  HB_SIZE nLenFirst = pFirst->item.asString.length;
  HB_SIZE nLenSecond = pSecond->item.asString.length;

  if (!bForceExact && hb_stackSetStruct()->HB_SET_EXACT)
  {
    // SET EXACT ON and not using ==
    // Don't include trailing spaces
    while (nLenFirst > nLenSecond && szFirst[nLenFirst - 1] == ' ')
    {
      nLenFirst--;
    }
    while (nLenSecond > nLenFirst && szSecond[nLenSecond - 1] == ' ')
    {
      nLenSecond--;
    }
    bForceExact = true;
  }

  HB_SIZE nMinLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;

  int iRet = 0; // Current status

  // Both strings not empty
  if (nMinLen)
  {
    auto cdp = hb_vmCDP();
    if (cdp && !HB_CDP_ISBINSORT(cdp))
    {
      iRet = hb_cdpicmp(szFirst, nLenFirst, szSecond, nLenSecond, cdp, bForceExact);
    }
    else
    {
      do
      {
        int i1 = HB_TOUPPER(static_cast<HB_UCHAR>(*szFirst));
        int i2 = HB_TOUPPER(static_cast<HB_UCHAR>(*szSecond));
        if (i1 != i2)
        {
          iRet = (i1 < i2) ? -1 : 1;
          break;
        }
        szFirst++;
        szSecond++;
      } while (--nMinLen);

      // If equal and length is different !
      if (!iRet && nLenFirst != nLenSecond)
      {
        // Force an exact comparison?
        if (bForceExact || nLenSecond > nLenFirst)
        {
          iRet = (nLenFirst < nLenSecond) ? -1 : 1;
        }
      }
    }
  }
  else
  {
    // Both empty ?
    if (nLenFirst != nLenSecond)
    {
      if (bForceExact)
      {
        iRet = (nLenFirst < nLenSecond) ? -1 : 1;
      }
      else
      {
        iRet = (nLenSecond == 0) ? 0 : -1;
      }
    }
    else
    {
      // Both empty => Equal !
      iRet = 0;
    }
  }

  return iRet;
}

// converts a numeric to a string with optional width & precision.

HB_BOOL hb_itemStrBuf(char *szResult, PHB_ITEM pNumber, int iSize, int iDec)
{
  if (iDec < 0)
  {
    iDec = 0;
  }

  int iPos, iDot;

  if (iDec > 0)
  {
    iPos = iDot = iSize - iDec - 1;
  }
  else
  {
    iPos = iSize;
    iDot = 0;
  }

  auto fNeg = false;

  if (pNumber->isDouble())
  {
    auto dNumber = pNumber->getND();

    if (!hb_isfinite(dNumber))
    {
      // Numeric overflow
      iPos = -1;
    }
    else
    {
      double dInt, dFract, dDig, doBase = 10.0;
      int iPrec, iFirst = -1;

#if 0
         dNumber = hb_numRound(dNumber, iDec);
#endif

#ifdef HB_NUM_PRECISION
      iPrec = HB_NUM_PRECISION;
#else
      iPrec = 16;
#endif

      if (dNumber < 0)
      {
        fNeg = true;
        dFract = modf(-dNumber, &dInt);
      }
      else
      {
        fNeg = false;
        dFract = modf(dNumber, &dInt);
      }

      while (iPos-- > 0)
      {
        dDig = modf(dInt / doBase + 0.01, &dInt) * doBase;
        szResult[iPos] = '0' + static_cast<char>(dDig + 0.01);
        if (szResult[iPos] != '0')
        {
          iFirst = iPos;
        }
        if (dInt < 1)
        {
          break;
        }
      }

      if (iPos > 0)
      {
        memset(szResult, ' ', iPos);
      }

      if (iDec > 0 && iPos >= 0)
      {
        for (iPos = iDot + 1; iPos < iSize; iPos++)
        {
          dFract = modf(dFract * doBase, &dDig);
          szResult[iPos] = '0' + static_cast<char>(dDig + 0.01);
          if (iFirst < 0)
          {
            if (szResult[iPos] != '0')
            {
              iFirst = iPos - 1;
            }
          }
          else if (iPos - iFirst >= iPrec)
          {
            break;
          }
        }
      }

      // now try to round the results and set 0 in places over defined
      // precision, the same is done by Clipper
      if (iPos >= 0)
      {
        int iZer;

        if (iFirst < 0)
        {
          iZer = 0;
        }
        else
        {
          iZer = iSize - iFirst - iPrec - (iDec > 0 ? 1 : 0);
        }

        dFract = modf(dFract * doBase, &dDig);
        auto iLast = static_cast<int>(dDig + 0.01);

        // hack for x.xxxx4999999999, f.e. 8.995 ~FL 8.994999999999999218..
        if (iLast == 4 && iZer < 0)
        {
          for (iPos = -iZer; iPos > 0; --iPos)
          {
            dFract = modf(dFract * doBase, &dDig);
            if (dDig + 0.01 < 9 && (iPos != 1 || dDig < 2))
            {
              break;
            }
          }
          if (iPos == 0)
          {
            iLast = 5;
          }
        }
        iLast = iLast >= 5 ? 1 : 0;

        iPos = iSize;
        while (iPos-- > 0)
        {
          if (iDec == 0 || iPos != iDot)
          {
            if (iZer > 0)
            {
              if (iDec == 0 || iPos <= iDot + 1)
              {
                iLast = szResult[iPos] >= '5' ? 1 : 0;
              }

              szResult[iPos] = '0';
              --iZer;
            }
            else if (iLast > 0)
            {
              if (szResult[iPos] == '9')
              {
                szResult[iPos] = '0';
              }
              else
              {
                if (szResult[iPos] < '0')
                { // '-' or ' '
                  szResult[iPos] = '1';
                  iFirst = iPos;
                }
                else
                {
                  szResult[iPos]++;
                  if (iFirst < 0)
                  {
                    iFirst = iPos;
                  }
                }
                break;
              }
            }
            else
            {
              break;
            }
          }
        }

        if (fNeg && iFirst >= 0 && iPos >= 0)
        {
          iPos = (iDot > 0 && iFirst >= iDot) ? iDot - 2 : iFirst - 1;
          if (iPos >= 0)
          {
            szResult[iPos] = '-';
          }
        }
      }
    }
  }
  else
  {
    HB_MAXINT nNumber;

    if (pNumber->isInteger())
    {
      nNumber = pNumber->item.asInteger.value;
    }
    else if (pNumber->isLong())
    {
      nNumber = pNumber->item.asLong.value;
    }
    else
    {
      nNumber = 0;
      iPos = -1;
    }

    fNeg = (nNumber < 0);
    while (iPos-- > 0)
    {
      szResult[iPos] = '0' + static_cast<char>(fNeg ? -(nNumber % 10) : (nNumber % 10));
      nNumber /= 10;
      if (nNumber == 0)
      {
        break;
      }
    }
    if (fNeg && iPos-- > 0)
    {
      szResult[iPos] = '-';
    }

    if (iPos > 0)
    {
      memset(szResult, ' ', iPos);
    }

    if (iDec > 0 && iPos >= 0)
    {
      memset(&szResult[iSize - iDec], '0', iDec);
    }
  }

  szResult[iSize] = '\0';
  // Set to asterisks in case of overflow
  if (iPos < 0)
  {
    memset(szResult, '*', iSize);
    return false;
  }
  else if (iDot > 0)
  {
    szResult[iDot] = '.';
  }

  return true;
}

// converts a numeric to a string with optional width & precision.
// This function should be used by any function that wants to format numeric
// data for displaying, printing, or putting in a database.
//
// Note: The caller is responsible for calling hb_xfree() to free the results
//       buffer, but ONLY if the return value is not a nullptr pointer! (If a nullptr
//       pointer is returned, then there was a conversion error.)
char *hb_itemStr(PHB_ITEM pNumber, PHB_ITEM pWidth, PHB_ITEM pDec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemStr(%p, %p, %p)", static_cast<void*>(pNumber), static_cast<void*>(pWidth), static_cast<void*>(pDec)));
#endif

  char *szResult = nullptr;

  if (pNumber)
  {
    // Default to the width and number of decimals specified by the item,
    // with a limit of 90 integer places, plus one space for the sign.
    int iWidth, iDec;

    hb_itemGetNLen(pNumber, &iWidth, &iDec);

    if (iWidth > 90)
    {
      iWidth = 90;
    }

    if (pWidth && pWidth->isNumeric())
    {
      // If the width parameter is specified, override the default value
      // and set the number of decimals to zero
      iWidth = pWidth->getNI();

      if (iWidth < 1)
      {
        iWidth = 10; // If 0 or negative, use default
      }
      iDec = 0;
    }

    // Clipper ignores decimal places when iWidth is 1
    if (iWidth > 1 && pDec && pDec->isNumeric())
    {
      // This function does not include the decimal places in the width,
      // so the width must be adjusted downwards, if the decimal places
      // parameter is greater than 0
      iDec = pDec->getNI();

      if (iDec <= 0)
      {
        iDec = 0;
      }
      else if (pWidth)
      {
        iWidth -= (iDec + 1);
      }
    }

    int iSize = (iDec > 0 ? iWidth + 1 + iDec : iWidth);

    if (iSize > 0)
    {
      szResult = static_cast<char *>(hb_xgrab(iSize + 1));
      hb_itemStrBuf(szResult, pNumber, iSize, iDec);
    }
  }

  return szResult;
}

// NOTE: The caller must free the pointer if the bFreeReq param gets set to
//       HB_TRUE, this trick is required to stay thread safe, while minimize
//       memory allocation and buffer copying.
//       As a side effect the caller should never modify the returned buffer
//       since it may point to a constant value. [vszakats]

char *hb_itemString(PHB_ITEM pItem, HB_SIZE *nLen, HB_BOOL *bFreeReq)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemString(%p, %p, %p)", static_cast<void*>(pItem), static_cast<void*>(nLen), static_cast<void*>(bFreeReq)));
#endif

  char *buffer;

  switch (HB_ITEM_TYPE(pItem))
  {
  case Harbour::Item::STRING:
  case Harbour::Item::MEMO:
    buffer = const_cast<char *>(pItem->getCPtr());
    *nLen = pItem->getCLen();
    *bFreeReq = false;
    break;

  case Harbour::Item::DATE: {
    HB_STACK_TLS_PRELOAD
    char szDate[9];

    hb_dateDecStr(szDate, pItem->item.asDateTime.julian);

    buffer = static_cast<char *>(hb_xgrab(11));
    hb_dateFormat(szDate, buffer, hb_stackSetStruct()->HB_SET_DATEFORMAT);
    *nLen = strlen(buffer);
    *bFreeReq = true;
    break;
  }

  case Harbour::Item::TIMESTAMP: {
    HB_STACK_TLS_PRELOAD
    char szDateTime[27];

    hb_timeStampFormat(szDateTime, hb_stackSetStruct()->HB_SET_DATEFORMAT, hb_stackSetStruct()->HB_SET_TIMEFORMAT,
                       pItem->item.asDateTime.julian, pItem->item.asDateTime.time);

    buffer = hb_strdup(szDateTime);
    *nLen = strlen(buffer);
    *bFreeReq = true;
    break;
  }

  case Harbour::Item::DOUBLE:
  case Harbour::Item::INTEGER:
  case Harbour::Item::LONG: {
    HB_STACK_TLS_PRELOAD
    if (hb_stackSetStruct()->HB_SET_FIXED)
    {
      // If fixed mode is enabled, use the default number of decimal places.
      hb_stackAllocItem()->putNI(hb_stackSetStruct()->HB_SET_DECIMALS);
      buffer = hb_itemStr(pItem, nullptr, hb_stackItemFromTop(-1));
      hb_stackPop();
    }
    else
    {
      buffer = hb_itemStr(pItem, nullptr, nullptr);
    }
    if (buffer)
    {
      *nLen = strlen(buffer);
      *bFreeReq = true;
    }
    else
    {
      buffer = const_cast<char *>("");
      *nLen = 0;
      *bFreeReq = false;
    }
    break;
  }
  case Harbour::Item::NIL:
    buffer = const_cast<char *>("NIL");
    *nLen = 3;
    *bFreeReq = false;
    break;

  case Harbour::Item::LOGICAL:
    buffer = const_cast<char *>(pItem->getL() ? ".T." : ".F.");
    *nLen = 3;
    *bFreeReq = false;
    break;

  case Harbour::Item::SYMBOL:
    *bFreeReq = true;
    *nLen = strlen(hb_itemGetSymbol(pItem)->szName) + 3;
    buffer = static_cast<char *>(hb_xgrab(*nLen + 1));
    buffer[0] = '@';
    memcpy(buffer + 1, hb_itemGetSymbol(pItem)->szName, *nLen - 3);
    buffer[*nLen - 2] = '(';
    buffer[*nLen - 1] = ')';
    buffer[*nLen] = '\0';
    break;

  case Harbour::Item::POINTER: {
    int size = (sizeof(void *) << 1) + 3; // n bytes for address + 0x + \0
    auto addr = reinterpret_cast<HB_PTRUINT>(hb_itemGetPtr(pItem));

    *nLen = size - 1;
    *bFreeReq = true;
    buffer = static_cast<char *>(hb_xgrab(size));
    buffer[0] = '0';
    buffer[1] = 'x';
    buffer[--size] = '\0';
    do
    {
      auto uc = static_cast<HB_UCHAR>(addr & 0xf);
      buffer[--size] = static_cast<char>(uc + (uc < 10 ? '0' : 'A' - 10));
      addr >>= 4;
    } while (size > 2);
    break;
  }
  default:
    buffer = const_cast<char *>("");
    *nLen = 0;
    *bFreeReq = false;
  }

  return buffer;
}

// This function is used by all of the PAD functions to prepare the argument
// being padded. If date, convert to string using hb_dateFormat(). If numeric,
// convert to unpadded string. Return pointer to string and set string length

char *hb_itemPadConv(PHB_ITEM pItem, HB_SIZE *pnSize, HB_BOOL *bFreeReq)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPadConv(%p, %p, %p)", static_cast<void*>(pItem), static_cast<void*>(pnSize), static_cast<void*>(bFreeReq)));
#endif

  if (pItem != nullptr)
  {
    switch (HB_ITEM_TYPE(pItem))
    {
    case Harbour::Item::STRING:
    case Harbour::Item::MEMO:
    case Harbour::Item::DATE:
    case Harbour::Item::TIMESTAMP:
      return hb_itemString(pItem, pnSize, bFreeReq);

    case Harbour::Item::DOUBLE:
    case Harbour::Item::INTEGER:
    case Harbour::Item::LONG: {
      int i;
      char *buffer = hb_itemString(pItem, pnSize, bFreeReq);

      // remove leading spaces if any, a little bit redundant but
      // I don't want to complicate the API interface more. Druzus
      for (i = 0; buffer[i] == ' '; i++)
      {
        ;
      }

      if (i > 0)
      {
        int j = 0;
        *pnSize -= i;
        do
        {
          buffer[j++] = buffer[i];
        } while (buffer[i++]);
      }
      return buffer;
    }
    default:
      break;
    }
  }
  return nullptr;
}

PHB_ITEM hb_itemValToStr(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemValToStr(%p)", static_cast<void*>(pItem)));
#endif

  HB_SIZE nLen;
  HB_BOOL bFreeReq;
  char *buffer = hb_itemString(pItem, &nLen, &bFreeReq);
  PHB_ITEM pResult;
  if (bFreeReq)
  {
    pResult = hb_itemPutCLPtr(nullptr, buffer, nLen);
  }
  else
  {
    pResult = hb_itemPutCL(nullptr, buffer, nLen);
  }

  return pResult;
}
