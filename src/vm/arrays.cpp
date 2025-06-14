//
// The Array API (C level)
//
// Copyright 1999 Antonio Linares <alinares@fivetech.com>
// Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
//   (hb_arrayIsObject(), hb_arrayCopyC(), hb_arrayGetC())
// Copyright 2001 Ron Pinkas <ron@profit-master.com>
//   (hb_arrayClone(), hb_arrayFromStack(), hb_arrayFromParams())
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

#include "hbvmopt.hpp"
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbapierr.hpp"
#include "hbapilng.hpp"
#include "hbvm.hpp"
#include "hbstack.hpp"

static void hb_arrayReleaseItems(PHB_BASEARRAY pBaseArray)
{
  if (pBaseArray->nLen)
  {
    do
    {
      pBaseArray->nLen--;
      if ((pBaseArray->pItems + pBaseArray->nLen)->isComplex())
      {
        (pBaseArray->pItems + pBaseArray->nLen)->clear();
      }
    } while (pBaseArray->nLen);

    // protection against possible base array resizing in user destructors
    if (pBaseArray->pItems)
    {
      hb_xfree(pBaseArray->pItems);
      pBaseArray->pItems = nullptr;
    }
  }
}

void hb_arrayPushBase(PHB_BASEARRAY pBaseArray)
{
  HB_STACK_TLS_PRELOAD
  auto pItem = hb_stackAllocItem();
  pItem->setType(Harbour::Item::ARRAY);
  pItem->setArrayValue(pBaseArray);
  hb_gcRefInc(pBaseArray);
}

// This releases array when called from the garbage collector
static HB_GARBAGE_FUNC(hb_arrayGarbageRelease)
{
  auto pBaseArray = static_cast<PHB_BASEARRAY>(Cargo);

  if (pBaseArray->uiClass)
  {
    // do not execute destructor for supercasted objects [druzus]
    if (pBaseArray->uiPrevCls == 0 && hb_clsHasDestructor(pBaseArray->uiClass))
    {
      HB_STACK_TLS_PRELOAD
      hb_arrayPushBase(pBaseArray);
      hb_objDestructorCall(hb_stackItemFromTop(-1));
      hb_stackPop();
    }

    // This is only some additional protection for buggy code
    // which can store reference to this object in other class
    // destructor when executed from GC and it will only cause
    // RT error when user will try to send any message to this
    // object [druzus]
    pBaseArray->uiClass = 0;
  }

  hb_arrayReleaseItems(pBaseArray);
}

static HB_GARBAGE_FUNC(hb_arrayGarbageMark)
{
  auto pBaseArray = static_cast<PHB_BASEARRAY>(Cargo);

  if (pBaseArray->nLen)
  {
    HB_SIZE nLen = pBaseArray->nLen;
    PHB_ITEM pItems = pBaseArray->pItems;

    while (nLen--)
    {
      if ((pItems + nLen)->isGCItem())
      {
        hb_gcItemRef(pItems + nLen);
      }
    }
  }
}

static const HB_GC_FUNCS s_gcArrayFuncs = {hb_arrayGarbageRelease, hb_arrayGarbageMark};

HB_BOOL hb_arrayNew(PHB_ITEM pItem, HB_SIZE nLen) // creates a new array
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayNew(%p, %" HB_PFS "u)", static_cast<void*>(pItem), nLen));
#endif

  if (pItem->isComplex())
  {
    pItem->clear();
  }

  PHB_ITEM pItems;

  // allocate memory for items before hb_gcAllocRaw() to be
  // safe for automatic GC activation in hb_xgrab() without
  // calling hb_gcLock()/hb_gcUnlock(). [druzus]
  if (nLen > 0)
  {
    pItems = static_cast<PHB_ITEM>(hb_xgrab(sizeof(HB_ITEM) * nLen));
    for (HB_SIZE nPos = 0; nPos < nLen; ++nPos)
    {
      (pItems + nPos)->setType(Harbour::Item::NIL);
    }
  }
  else
  {
    pItems = nullptr;
  }

  auto pBaseArray = static_cast<PHB_BASEARRAY>(hb_gcAllocRaw(sizeof(HB_BASEARRAY), &s_gcArrayFuncs));
  pBaseArray->pItems = pItems;
  pBaseArray->nLen = nLen;
  pBaseArray->uiClass = 0;
  pBaseArray->uiPrevCls = 0;
  pBaseArray->nAllocated = nLen;
  pItem->setType(Harbour::Item::ARRAY);
  pItem->setArrayValue(pBaseArray);

  return true;
}

void hb_arraySwap(PHB_ITEM pArray1, PHB_ITEM pArray2)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySwap(%p, %p)", static_cast<void*>(pArray1), static_cast<void*>(pArray2)));
#endif

  if (pArray1->isArray() && pArray2->isArray())
  {
    HB_BASEARRAY tmpBaseArray;

    tmpBaseArray = *pArray1->item.asArray.value;
    *pArray1->item.asArray.value = *pArray2->item.asArray.value;
    *pArray2->item.asArray.value = tmpBaseArray;
  }
}

HB_BOOL hb_arraySize(PHB_ITEM pArray, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySize(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nLen));
#endif

  if (pArray->isArray())
  {
    PHB_BASEARRAY pBaseArray = pArray->arrayValue();

    if (nLen != pBaseArray->nLen)
    {
      HB_SIZE nPos;

      if (pBaseArray->nLen == 0)
      {
        pBaseArray->pItems = static_cast<PHB_ITEM>(hb_xgrab(nLen * sizeof(HB_ITEM)));
        pBaseArray->nAllocated = nLen;

        for (nPos = 0; nPos < nLen; nPos++)
        {
          (pBaseArray->pItems + nPos)->setType(Harbour::Item::NIL);
        }
      }
      else
      {
        if (pBaseArray->nLen < nLen)
        {
          if (pBaseArray->nAllocated < nLen)
          {
            // A common practice is to double allocation buffer size. Thus, making
            // reallocation count logarithmic to total number of added numbers.
            // I've used here a little different formula. ulAllocated is divided by
            // factor 2 ( >> 1 ) and 1 is added to requested size. This algorithm
            // has properties:
            //   - reallocation count remains asymptotically logarithmic;
            //   - saves memory for large arrays, because reallocation buffer
            //     size is not doubled, but multiplied by 1.5;
            //   - adding of 1, allows reduce reallocation count for small arrays.
            pBaseArray->nAllocated = (pBaseArray->nAllocated >> 1) + 1 + nLen;
            pBaseArray->pItems =
                static_cast<PHB_ITEM>(hb_xrealloc(pBaseArray->pItems, sizeof(HB_ITEM) * pBaseArray->nAllocated));
          }

          // set value for new items
          for (nPos = pBaseArray->nLen; nPos < nLen; nPos++)
          {
            (pBaseArray->pItems + nPos)->setType(Harbour::Item::NIL);
          }
        }
        else if (pBaseArray->nLen > nLen)
        {
          // release old items
          for (nPos = nLen; nPos < pBaseArray->nLen; nPos++)
          {
            if ((pBaseArray->pItems + nPos)->isComplex())
            {
              (pBaseArray->pItems + nPos)->clear();
            }
          }

          if (nLen == 0)
          {
            hb_xfree(pBaseArray->pItems);
            pBaseArray->pItems = nullptr;
          }
          else if (nLen < (pBaseArray->nAllocated >> 1))
          {
            pBaseArray->pItems = static_cast<PHB_ITEM>(hb_xrealloc(pBaseArray->pItems, sizeof(HB_ITEM) * nLen));
            pBaseArray->nAllocated = nLen;
          }
        }
      }

      pBaseArray->nLen = nLen;
    }

    return true;
  }
  else
  {
    return false;
  }
}

HB_SIZE hb_arrayLen(PHB_ITEM pArray)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayLen(%p)", static_cast<void*>(pArray)));
#endif

  return pArray->isArray() ? pArray->arrayLen() : 0;
}

HB_BOOL hb_arrayIsObject(PHB_ITEM pArray)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayIsObject(%p)", static_cast<void*>(pArray)));
#endif

  return pArray->isArray() ? pArray->arrayValue()->uiClass != 0 : false;
}

// retrieves the array unique ID
void *hb_arrayId(PHB_ITEM pArray)
{
  return pArray != nullptr && pArray->isArray() ? static_cast<void *>(pArray->arrayValue()) : nullptr;
}

// retrieves numer of references to the array
HB_COUNTER hb_arrayRefs(PHB_ITEM pArray)
{
  return pArray != nullptr && pArray->isArray() ? hb_gcRefCount(pArray->arrayValue()) : 0;
}

PHB_ITEM hb_arrayFromId(PHB_ITEM pItem, void *pArrayId)
{
  HB_STACK_TLS_PRELOAD

  hb_arrayPushBase(static_cast<PHB_BASEARRAY>(pArrayId));
  if (pItem == nullptr)
  {
    pItem = hb_itemNew(nullptr);
  }
  hb_itemMove(pItem, hb_stackItemFromTop(-1));
  hb_stackPop();

  return pItem;
}

HB_BOOL hb_arrayAdd(PHB_ITEM pArray, PHB_ITEM pValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayAdd(%p, %p)", static_cast<void*>(pArray), static_cast<void*>(pValue)));
#endif

  if (pArray->isArray())
  {
    auto pBaseArray = static_cast<PHB_BASEARRAY>(pArray->arrayValue());

    if (pBaseArray->nLen < HB_SIZE_MAX)
    {
      hb_arraySize(pArray, pBaseArray->nLen + 1);
      pBaseArray = static_cast<PHB_BASEARRAY>(pArray->arrayValue());
      hb_itemCopy(pBaseArray->pItems + (pBaseArray->nLen - 1), pValue);

      return true;
    }
  }

  return false;
}

HB_BOOL hb_arrayAddForward(PHB_ITEM pArray, PHB_ITEM pValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayAddForward(%p, %p)", static_cast<void*>(pArray), static_cast<void*>(pValue)));
#endif

  if (pArray->isArray())
  {
    auto pBaseArray = static_cast<PHB_BASEARRAY>(pArray->arrayValue());

    if (pBaseArray->nLen < HB_SIZE_MAX)
    {
      hb_arraySize(pArray, pBaseArray->nLen + 1);
      pBaseArray = static_cast<PHB_BASEARRAY>(pArray->arrayValue());
      hb_itemMove(pBaseArray->pItems + (pBaseArray->nLen - 1), pValue);

      return true;
    }
  }

  return false;
}

HB_BOOL hb_arrayDel(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayDel(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  if (pArray->isArray())
  {
    HB_SIZE nLen = pArray->arrayLen();

    if (nIndex > 0 && nIndex <= nLen)
    {
      PHB_BASEARRAY pBaseArray = pArray->arrayValue();

      if (nIndex == nLen)
      {
        hb_itemSetNil(pBaseArray->pItems + nIndex - 1);
      }
      else
      {
        for (; nIndex < nLen; ++nIndex)
        { // move items
          hb_itemMoveRef(pBaseArray->pItems + nIndex - 1, pBaseArray->pItems + nIndex);
        }
      }

      return true;
    }
  }

  return false;
}

HB_BOOL hb_arrayIns(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayIns(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  if (pArray->isArray())
  {
    HB_SIZE nLen = pArray->arrayLen();

    if (nIndex > 0 && nIndex <= nLen)
    {
      PHB_BASEARRAY pBaseArray = pArray->arrayValue();

      if (nIndex == nLen)
      {
        hb_itemSetNil(pBaseArray->pItems + nIndex - 1);
      }
      else
      {
        while (--nLen >= nIndex)
        { // move items
          hb_itemMoveRef(pBaseArray->pItems + nLen, pBaseArray->pItems + nLen - 1);
        }
      }

      return true;
    }
  }

  return false;
}

HB_BOOL hb_arraySet(PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySet(%p, %" HB_PFS "u, %p)", static_cast<void*>(pArray), nIndex, static_cast<void*>(pItem)));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemCopy(pArray->arrayItem(nIndex), pItem);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetForward(PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetForward(%p, %" HB_PFS "u, %p)", static_cast<void*>(pArray), nIndex, static_cast<void*>(pItem)));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemMove(pArray->arrayItem(nIndex), pItem);
    return true;
  }
  else
  {
    pItem->clear();
    return false;
  }
}

HB_BOOL hb_arrayGet(PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGet(%p, %" HB_PFS "u, %p)", static_cast<void*>(pArray), nIndex, static_cast<void*>(pItem)));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemCopy(pItem, pArray->arrayItem(nIndex));
    return true;
  }
  else
  {
    hb_itemSetNil(pItem);
    return false;
  }
}

HB_BOOL hb_arrayGetItemRef(PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetItemRef(%p, %" HB_PFS "u, %p)", static_cast<void*>(pArray), nIndex, static_cast<void*>(pItem)));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    if (pArray != pItem)
    {
      if (pItem->isComplex())
      {
        pItem->clear();
      }
      hb_gcRefInc(pArray->arrayValue());
    }
    pItem->setType(Harbour::Item::BYREF);
    pItem->item.asRefer.BasePtr.array = pArray->arrayValue();
    pItem->item.asRefer.value = nIndex - 1;
    pItem->item.asRefer.offset = 0;
    return true;
  }
  else
  {
    hb_itemSetNil(pItem);
    return false;
  }
}

// This function returns a pointer to an item occupied by the specified
// array element - it doesn't return an item's value
PHB_ITEM hb_arrayGetItemPtr(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetItemPtr(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? pArray->arrayItem(nIndex) : nullptr;
}

char *hb_arrayGetDS(PHB_ITEM pArray, HB_SIZE nIndex, char *szDate)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetDS(%p, %" HB_PFS "u, %s)", static_cast<void*>(pArray), nIndex, szDate));
#endif

  // NOTE: Intentionally calling it with a bad parameter in order to get
  //       the default value from hb_itemGetDS(). [vszakats]
  return pArray->isArray() && pArray->isValidIndex(nIndex) ? (pArray->arrayItem(nIndex))->getDS(szDate)
                                                           : hb_itemGetDS(nullptr, szDate);
}

long hb_arrayGetDL(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetDL(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  // NOTE: Intentionally calling it with a bad parameter in order to get
  //       the default value from hb_itemGetDL(). [vszakats]
  return pArray->isArray() && pArray->isValidIndex(nIndex) ? (pArray->arrayItem(nIndex))->getDL()
                                                           : hb_itemGetDL(nullptr);
}

double hb_arrayGetTD(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetTD(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? (pArray->arrayItem(nIndex))->getTD() : 0;
}

HB_BOOL hb_arrayGetTDT(PHB_ITEM pArray, HB_SIZE nIndex, long *plJulian, long *plMilliSec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetTDT(%p, %" HB_PFS "u, %p, %p)", static_cast<void*>(pArray), nIndex, static_cast<void*>(plJulian), static_cast<void*>(plMilliSec)));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    return (pArray->arrayItem(nIndex))->getTDT(plJulian, plMilliSec);
  }
  else
  {
    *plJulian = *plMilliSec = 0;
    return false;
  }
}

HB_BOOL hb_arrayGetL(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetL(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? (pArray->arrayItem(nIndex))->getL() : false;
}

int hb_arrayGetNI(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetNI(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? (pArray->arrayItem(nIndex))->getNI() : 0;
}

long hb_arrayGetNL(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetNL(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? (pArray->arrayItem(nIndex))->getNL() : 0;
}

HB_ISIZ hb_arrayGetNS(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetNS(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? hb_itemGetNS(pArray->arrayItem(nIndex)) : 0;
}

#ifndef HB_LONG_LONG_OFF
HB_LONGLONG hb_arrayGetNLL(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetNLL(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? hb_itemGetNLL(pArray->arrayItem(nIndex)) : 0;
}
#endif

HB_MAXINT hb_arrayGetNInt(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetNInt(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? (pArray->arrayItem(nIndex))->getNInt() : 0;
}

double hb_arrayGetND(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetND(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? (pArray->arrayItem(nIndex))->getND() : 0;
}

HB_SIZE hb_arrayCopyC(PHB_ITEM pArray, HB_SIZE nIndex, char *szBuffer, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayCopyC(%p, %" HB_PFS "u, %s, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex, szBuffer, nLen));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? hb_itemCopyC(pArray->arrayItem(nIndex), szBuffer, nLen)
                                                           : 0;
}

char *hb_arrayGetC(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetC(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? (pArray->arrayItem(nIndex))->getC() : nullptr;
}

const char *hb_arrayGetCPtr(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetCPtr(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? (pArray->arrayItem(nIndex))->getCPtr() : "";
}

HB_SIZE hb_arrayGetCLen(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetCLen(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? (pArray->arrayItem(nIndex))->getCLen() : 0;
}

void *hb_arrayGetPtr(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetPtr(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? (pArray->arrayItem(nIndex))->getPtr() : nullptr;
}

void *hb_arrayGetPtrGC(PHB_ITEM pArray, HB_SIZE nIndex, const HB_GC_FUNCS *pFuncs)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetPtrGC(%p, %" HB_PFS "u, %p)", static_cast<void*>(pArray), nIndex, static_cast<const void*>(pFuncs)));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? hb_itemGetPtrGC(pArray->arrayItem(nIndex), pFuncs)
                                                           : nullptr;
}

PHB_SYMB hb_arrayGetSymbol(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetSymbol(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? (pArray->arrayItem(nIndex))->getSymbol() : nullptr;
}

HB_TYPE hb_arrayGetType(PHB_ITEM pArray, HB_SIZE nIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetType(%p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex));
#endif

  return pArray->isArray() && pArray->isValidIndex(nIndex) ? hb_itemType(pArray->arrayItem(nIndex)) : 0;
}

HB_BOOL hb_arraySetDS(PHB_ITEM pArray, HB_SIZE nIndex, const char *szDate)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetDS(%p, %" HB_PFS "u, %s)", static_cast<void*>(pArray), nIndex, szDate));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutDS(pArray->arrayItem(nIndex), szDate);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetDL(PHB_ITEM pArray, HB_SIZE nIndex, long lDate)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetDL(%p, %" HB_PFS "u, %ld)", static_cast<void*>(pArray), nIndex, lDate));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutDL(pArray->arrayItem(nIndex), lDate);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetTD(PHB_ITEM pArray, HB_SIZE nIndex, double dTimeStamp)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetTD(%p, %" HB_PFS "u, %lf)", static_cast<void*>(pArray), nIndex, dTimeStamp));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutTD(pArray->arrayItem(nIndex), dTimeStamp);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetTDT(PHB_ITEM pArray, HB_SIZE nIndex, long lJulian, long lMilliSec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetTDT(%p, %" HB_PFS "u, %lu, %lu)", static_cast<void*>(pArray), nIndex, lJulian, lMilliSec));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutTDT(pArray->arrayItem(nIndex), lJulian, lMilliSec);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetL(PHB_ITEM pArray, HB_SIZE nIndex, HB_BOOL fValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetL(%p, %" HB_PFS "u, %d)", static_cast<void*>(pArray), nIndex, fValue));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    (pArray->arrayItem(nIndex))->putL(fValue);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetNI(PHB_ITEM pArray, HB_SIZE nIndex, int iNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetNI(%p, %" HB_PFS "u, %d)", static_cast<void*>(pArray), nIndex, iNumber));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    (pArray->arrayItem(nIndex))->putNI(iNumber);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetNL(PHB_ITEM pArray, HB_SIZE nIndex, long lNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetNL(%p, %" HB_PFS "u, %lu)", static_cast<void*>(pArray), nIndex, lNumber));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutNL(pArray->arrayItem(nIndex), lNumber);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetNS(PHB_ITEM pArray, HB_SIZE nIndex, HB_ISIZ nNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetNS(%p, %" HB_PFS "u, %" HB_PFS "d)", static_cast<void*>(pArray), nIndex, nNumber));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutNS(pArray->arrayItem(nIndex), nNumber);
    return true;
  }
  else
  {
    return false;
  }
}

#ifndef HB_LONG_LONG_OFF
HB_BOOL hb_arraySetNLL(PHB_ITEM pArray, HB_SIZE nIndex, HB_LONGLONG llNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetNLL(%p, %" HB_PFS "u, %" PFLL "d)", static_cast<void*>(pArray), nIndex, llNumber));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutNLL(pArray->arrayItem(nIndex), llNumber);
    return true;
  }
  else
  {
    return false;
  }
}
#endif

HB_BOOL hb_arraySetNInt(PHB_ITEM pArray, HB_SIZE nIndex, HB_MAXINT nNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetNInt(%p, %" HB_PFS "u, %" PFHL "d)", static_cast<void*>(pArray), nIndex, nNumber));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutNInt(pArray->arrayItem(nIndex), nNumber);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetND(PHB_ITEM pArray, HB_SIZE nIndex, double dNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetND(%p, %" HB_PFS "u, %lf)", static_cast<void*>(pArray), nIndex, dNumber));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutND(pArray->arrayItem(nIndex), dNumber);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetC(PHB_ITEM pArray, HB_SIZE nIndex, const char *szText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetC(%p, %" HB_PFS "u, %p)", static_cast<void*>(pArray), nIndex, static_cast<const void*>(szText)));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutC(pArray->arrayItem(nIndex), szText);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetCL(PHB_ITEM pArray, HB_SIZE nIndex, const char *szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetC(%p, %" HB_PFS "u, %p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex, static_cast<const void*>(szText), nLen));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutCL(pArray->arrayItem(nIndex), szText, nLen);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetCPtr(PHB_ITEM pArray, HB_SIZE nIndex, char *szText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetCPtr(%p, %" HB_PFS "u, %p)", static_cast<void*>(pArray), nIndex, static_cast<void*>(szText)));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutCPtr(pArray->arrayItem(nIndex), szText);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetCLPtr(PHB_ITEM pArray, HB_SIZE nIndex, char *szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetCLPtr(%p, %" HB_PFS "u, %p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex, static_cast<void*>(szText), nLen));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutCLPtr(pArray->arrayItem(nIndex), szText, nLen);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetCConst(PHB_ITEM pArray, HB_SIZE nIndex, const char *szText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetCConst(%p, %" HB_PFS "u, %p)", static_cast<void*>(pArray), nIndex, static_cast<const void*>(szText)));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutCConst(pArray->arrayItem(nIndex), szText);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetPtr(PHB_ITEM pArray, HB_SIZE nIndex, void *pValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetPtr(%p, %" HB_PFS "u, %p)", static_cast<void*>(pArray), nIndex, pValue));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutPtr(pArray->arrayItem(nIndex), pValue);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetPtrGC(PHB_ITEM pArray, HB_SIZE nIndex, void *pValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetPtrGC(%p, %" HB_PFS "u, %p)", static_cast<void*>(pArray), nIndex, pValue));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutPtrGC(pArray->arrayItem(nIndex), pValue);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arraySetSymbol(PHB_ITEM pArray, HB_SIZE nIndex, PHB_SYMB pSymbol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetSymbol(%p, %" HB_PFS "u, %p)", static_cast<void*>(pArray), nIndex, static_cast<void*>(pSymbol)));
#endif

  if (pArray->isArray() && pArray->isValidIndex(nIndex))
  {
    hb_itemPutSymbol(pArray->arrayItem(nIndex), pSymbol);
    return true;
  }
  else
  {
    return false;
  }
}

HB_BOOL hb_arrayLast(PHB_ITEM pArray, PHB_ITEM pResult)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayLast(%p, %p)", static_cast<void*>(pArray), static_cast<void*>(pResult)));
#endif

  if (pArray->isArray())
  {
    if (pArray->arrayLen() > 0)
    {
      hb_itemCopy(pResult, pArray->arrayItem(pArray->arrayLen()));
    }
    else
    {
      hb_itemSetNil(pResult);
    }

    return true;
  }

  hb_itemSetNil(pResult);

  return false;
}

HB_BOOL hb_arrayFill(PHB_ITEM pArray, PHB_ITEM pValue, HB_SIZE *pnStart, HB_SIZE *pnCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayFill(%p, %p, %p, %p)", static_cast<void*>(pArray), static_cast<void*>(pValue), static_cast<void*>(pnStart), static_cast<void*>(pnCount)));
#endif

  if (pArray->isArray())
  {
    PHB_BASEARRAY pBaseArray = pArray->arrayValue();
    HB_SIZE nLen = pBaseArray->nLen;
    HB_SIZE nStart;

    if (pnStart && *pnStart)
    {
      nStart = *pnStart - 1;
    }
    else
    {
      nStart = 0;
    }

    if (nStart < nLen)
    {
      HB_SIZE nCount = nLen - nStart;
      if (pnCount && *pnCount < nCount)
      {
        nCount = *pnCount;
      }

      if (nCount > 0)
      {
        do
        {
          hb_itemCopy(pBaseArray->pItems + nStart++, pValue);
        } while (--nCount > 0);
      }
    }

    return true;
  }
  else
  {
    return false;
  }
}

HB_SIZE hb_arrayScan(PHB_ITEM pArray, PHB_ITEM pValue, HB_SIZE *pnStart, HB_SIZE *pnCount, HB_BOOL fExact)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayScan(%p, %p, %p, %p, %d)", pArray, pValue, pnStart, pnCount, static_cast<int>(fExact)));
#endif

  if (pArray->isArray())
  {
    PHB_BASEARRAY pBaseArray = pArray->arrayValue();
    HB_SIZE nLen = pBaseArray->nLen;
    HB_SIZE nStart;

    if (pnStart && *pnStart)
    {
      nStart = *pnStart - 1;
    }
    else
    {
      nStart = 0;
    }

    if (nStart < nLen)
    {
      HB_SIZE nCount = nLen - nStart;
      if (pnCount && *pnCount < nCount)
      {
        nCount = *pnCount;
      }

      if (nCount > 0)
      {
        // Make separate search loops for different types to find, so that
        // the loop can be faster.

        if (pValue->isBlock())
        {
          HB_STACK_TLS_PRELOAD
          do
          {
            hb_vmPushEvalSym();
            hb_vmPush(pValue);
            hb_vmPush(pBaseArray->pItems + nStart);
            hb_vmPushSize(++nStart);
            hb_vmEval(2);

            if (hb_stackReturnItem()->isLogical() && hb_stackReturnItem()->logicalValue())
            {
              return nStart;
            }
          } while (--nCount > 0 && nStart < pBaseArray->nLen);
        }
        else if (pValue->isString())
        {
          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart++;

            // NOTE: The order of the pItem and pValue parameters passed to
            //       hb_itemStrCmp() is significant, please don't change it. [vszakats]
            if (pItem->isString() && hb_itemStrCmp(pItem, pValue, fExact) == 0)
            {
              return nStart;
            }
          } while (--nCount > 0);
        }
        else if (pValue->isNumInt())
        {
          HB_MAXINT nValue = pValue->getNInt();

          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart++;

            if (pItem->isNumeric() && pItem->getNInt() == nValue && pItem->getND() == static_cast<double>(nValue))
            {
              return nStart;
            }
          } while (--nCount > 0);
        }
        else if (pValue->isNumeric())
        {
          auto dValue = pValue->getND();

          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart++;

            if (pItem->isNumeric() && pItem->getND() == dValue)
            {
              return nStart;
            }
          } while (--nCount > 0);
        }
        else if (pValue->isDateTime())
        {
          if (fExact)
          {
            do
            {
              PHB_ITEM pItem = pBaseArray->pItems + nStart++;

              if (pItem->isDateTime() && pItem->dateTimeJulian() == pValue->dateTimeJulian() &&
                  pItem->dateTimeTime() == pValue->dateTimeTime())
              {
                return nStart;
              }
            } while (--nCount > 0);
          }
          else
          {
            do
            {
              PHB_ITEM pItem = pBaseArray->pItems + nStart++;

              if (pItem->isDateTime() && pItem->dateTimeJulian() == pValue->dateTimeJulian())
              {
                return nStart;
              }
            } while (--nCount > 0);
          }
        }
        else if (pValue->isLogical())
        {
          HB_BOOL bValue = pValue->getL();

          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart++;

            if (pItem->isLogical() && pItem->getL() == bValue)
            {
              return nStart;
            }
          } while (--nCount > 0);
        }
        else if (pValue->isNil())
        {
          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart++;

            if (pItem->isNil())
            {
              return nStart;
            }
          } while (--nCount > 0);
        }
        else if (pValue->isPointer())
        {
          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart++;

            if (pItem->isPointer() && pItem->pointerValue() == pValue->pointerValue())
            {
              return nStart;
            }
          } while (--nCount > 0);
        }
        else if (fExact && pValue->isArray())
        {
          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart++;

            if (pItem->isArray() && pItem->arrayValue() == pValue->arrayValue())
            {
              return nStart;
            }
          } while (--nCount > 0);
        }
        else if (fExact && pValue->isHash())
        {
          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart++;

            if (pItem->isHash() && pItem->hashValue() == pValue->hashValue())
            {
              return nStart;
            }
          } while (--nCount > 0);
        }
      }
    }
  }

  return 0;
}

HB_SIZE hb_arrayRevScan(PHB_ITEM pArray, PHB_ITEM pValue, HB_SIZE *pnStart, HB_SIZE *pnCount, HB_BOOL fExact)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayRevScan(%p, %p, %p, %p, %d)", static_cast<void*>(pArray), static_cast<void*>(pValue), static_cast<void*>(pnStart), static_cast<void*>(pnCount), static_cast<int>(fExact)));
#endif

  if (pArray->isArray())
  {
    PHB_BASEARRAY pBaseArray = pArray->arrayValue();
    HB_SIZE nLen = pBaseArray->nLen;
    HB_SIZE nStart;

    if (pnStart && *pnStart)
    {
      nStart = *pnStart - 1;
    }
    else
    {
      nStart = nLen - 1;
    }

    if (nStart < nLen)
    {
      HB_SIZE nCount = nStart + 1;
      if (pnCount && *pnCount < nCount)
      {
        nCount = *pnCount;
      }

      if (nCount > 0)
      {
        // Make separate search loops for different types to find, so that
        // the loop can be faster.

        if (pValue->isBlock())
        {
          HB_STACK_TLS_PRELOAD
          do
          {
            hb_vmPushEvalSym();
            hb_vmPush(pValue);
            if (nStart < pBaseArray->nLen)
            {
              hb_vmPush(pBaseArray->pItems + nStart);
            }
            else
            {
              hb_vmPushNil();
            }
            hb_vmPushSize(nStart + 1);
            hb_vmEval(2);

            if (hb_stackReturnItem()->isLogical() && hb_stackReturnItem()->logicalValue())
            {
              return nStart + 1;
            }
          } while (--nCount && nStart--);
        }
        else if (pValue->isString())
        {
          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart;

            // NOTE: The order of the pItem and pValue parameters passed to
            //       hb_itemStrCmp() is significant, please don't change it. [vszakats]
            if (pItem->isString() && hb_itemStrCmp(pItem, pValue, fExact) == 0)
            {
              return nStart + 1;
            }
          } while (--nCount && nStart--);
        }
        else if (pValue->isNumInt())
        {
          HB_MAXINT nValue = pValue->getNInt();

          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart;

            if (pItem->isNumeric() && pItem->getNInt() == nValue && pItem->getND() == static_cast<double>(nValue))
            {
              return nStart + 1;
            }
          } while (--nCount && nStart--);
        }
        else if (pValue->isNumeric())
        {
          auto dValue = pValue->getND();

          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart;

            if (pItem->isNumeric() && pItem->getND() == dValue)
            {
              return nStart + 1;
            }
          } while (--nCount && nStart--);
        }
        else if (pValue->isDateTime())
        {
          if (fExact)
          {
            do
            {
              PHB_ITEM pItem = pBaseArray->pItems + nStart;

              if (pItem->isDateTime() && pItem->dateTimeJulian() == pValue->dateTimeJulian() &&
                  pItem->dateTimeTime() == pValue->dateTimeTime())
              {
                return nStart + 1;
              }
            } while (--nCount && nStart--);
          }
          else
          {
            do
            {
              PHB_ITEM pItem = pBaseArray->pItems + nStart;

              if (pItem->isDateTime() && pItem->dateTimeJulian() == pValue->dateTimeJulian())
              {
                return nStart + 1;
              }
            } while (--nCount && nStart--);
          }
        }
        else if (pValue->isLogical())
        {
          HB_BOOL bValue = pValue->getL();

          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart;

            if (pItem->isLogical() && pItem->getL() == bValue)
            {
              return nStart + 1;
            }
          } while (--nCount && nStart--);
        }
        else if (pValue->isNil())
        {
          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart;

            if (pItem->isNil())
            {
              return nStart + 1;
            }
          } while (--nCount && nStart--);
        }
        else if (pValue->isPointer())
        {
          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart;

            if (pItem->isPointer() && pItem->pointerValue() == pValue->pointerValue())
            {
              return nStart + 1;
            }
          } while (--nCount && nStart--);
        }
        else if (fExact && pValue->isArray())
        {
          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart;

            if (pItem->isArray() && pItem->arrayValue() == pValue->arrayValue())
            {
              return nStart + 1;
            }
          } while (--nCount && nStart--);
        }
        else if (fExact && pValue->isHash())
        {
          do
          {
            PHB_ITEM pItem = pBaseArray->pItems + nStart;

            if (pItem->isHash() && pItem->hashValue() == pValue->hashValue())
            {
              return nStart + 1;
            }
          } while (--nCount && nStart--);
        }
      }
    }
  }

  return 0;
}

HB_BOOL hb_arrayEval(PHB_ITEM pArray, PHB_ITEM bBlock, HB_SIZE *pnStart, HB_SIZE *pnCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayEval(%p, %p, %p, %p)", static_cast<void*>(pArray), static_cast<void*>(bBlock), static_cast<void*>(pnStart), static_cast<void*>(pnCount)));
#endif

  if (pArray->isArray() && bBlock->isBlock())
  {
    PHB_BASEARRAY pBaseArray = pArray->arrayValue();
    HB_SIZE nLen = pBaseArray->nLen;
    HB_SIZE nStart;

    if (pnStart && *pnStart)
    {
      nStart = *pnStart - 1;
    }
    else
    {
      nStart = 0;
    }

    if (nStart < nLen)
    {
      HB_SIZE nCount = nLen - nStart;
      if (pnCount && *pnCount < nCount)
      {
        nCount = *pnCount;
      }

      if (nCount > 0)
      {
        do
        {
          hb_vmPushEvalSym();
          hb_vmPush(bBlock);
          hb_vmPush(pBaseArray->pItems + nStart);
          hb_vmPushSize(nStart + 1);
          hb_vmEval(2);
        } while (--nCount > 0 && ++nStart < pBaseArray->nLen);
        // checking for nStart < pBaseArray->nLen is fix for
        // possible GPF when codeblock decrease array size
      }
    }

    return true;
  }
  else
  {
    return false;
  }
}

// NOTE: CA-Cl*pper 5.3a has a fix for the case when the starting position
//       is greater than the length of the array. [vszakats]

HB_BOOL hb_arrayCopy(PHB_ITEM pSrcArray, PHB_ITEM pDstArray, HB_SIZE *pnStart, HB_SIZE *pnCount, HB_SIZE *pnTarget)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayCopy(%p, %p, %p, %p, %p)", static_cast<void*>(pSrcArray), static_cast<void*>(pDstArray), static_cast<void*>(pnStart), static_cast<void*>(pnCount), static_cast<void*>(pnTarget)));
#endif

  if (pSrcArray->isArray() && pDstArray->isArray())
  {
    PHB_BASEARRAY pSrcBaseArray = pSrcArray->arrayValue();
    PHB_BASEARRAY pDstBaseArray = pDstArray->arrayValue();
    HB_SIZE nSrcLen = pSrcBaseArray->nLen;
    HB_SIZE nDstLen = pDstBaseArray->nLen;
    HB_SIZE nStart;
    HB_SIZE nTarget;

    if (pnStart && (*pnStart >= 1))
    {
      nStart = *pnStart;
    }
    else
    {
      nStart = 1;
    }

    if (pnTarget && (*pnTarget >= 1))
    {
      nTarget = *pnTarget;
    }
    else
    {
      nTarget = 1;
    }

#ifdef HB_COMPAT_C53 // From CA-Cl*pper 5.3a
    if (nStart <= nSrcLen)
    {
#else
    if (nSrcLen > 0)
    {
#endif
      HB_SIZE nCount;
#ifndef HB_COMPAT_C53 // From CA-Cl*pper 5.3a
      if (nStart > nSrcLen)
      {
        nStart = nSrcLen;
      }
#endif
      if (pnCount && (*pnCount <= nSrcLen - nStart))
      {
        nCount = *pnCount;
      }
      else
      {
        nCount = nSrcLen - nStart + 1;
      }

// This is probably a bug, present in all versions of CA-Cl*pper.
#if defined(HB_CLP_STRICT) || 1
      if (nDstLen > 0)
      {
        if (nTarget > nDstLen)
        {
          nTarget = nDstLen;
        }
#else
      if (nTarget <= nDstLen)
      {
#endif
        if (pDstBaseArray->pItems + nTarget != pSrcBaseArray->pItems + nStart)
        {
          if (nCount > nDstLen - nTarget)
          {
            nCount = nDstLen - nTarget + 1;
          }

          for (nTarget--, nStart--; nCount > 0; nCount--, nStart++, nTarget++)
          {
            hb_itemCopy(pDstBaseArray->pItems + nTarget, pSrcBaseArray->pItems + nStart);
          }
        }
      }
    }

    return true;
  }
  else
  {
    return false;
  }
}

static void hb_arrayCloneBody(PHB_ITEM pDest, PHB_ITEM pArray, PHB_NESTED_CLONED pClonedList)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayCloneBody(%p, %p, %p)", static_cast<void*>(pDest), static_cast<void*>(pArray), static_cast<void*>(pClonedList)));
#endif

  HB_SIZE nLen = pArray->arrayLen();
  hb_arrayNew(pDest, nLen);
  pDest->arrayValue()->uiClass = pArray->arrayValue()->uiClass;
  PHB_ITEM pSrcItem = pArray->arrayItems();
  PHB_ITEM pDstItem = pDest->arrayItems();

  while (nLen--)
  {
    hb_nestedCloneDo(pDstItem++, pSrcItem++, pClonedList);
  }
}

void hb_nestedCloneInit(PHB_NESTED_CLONED pClonedList, void *pValue, PHB_ITEM pDest)
{
  if (hb_gcRefCount(pValue) > 1)
  {
    pClonedList->nCount = 1;
    pClonedList->nSize = 16;
    pClonedList->pRefs = static_cast<PHB_NESTED_REF>(hb_xgrab(pClonedList->nSize * sizeof(HB_NESTED_REF)));
    pClonedList->pRefs[0].value = pValue;
    pClonedList->pRefs[0].pDest = pDest;
  }
  else
  {
    pClonedList->nCount = pClonedList->nSize = 0;
    pClonedList->pRefs = nullptr;
  }
}

void hb_nestedCloneFree(PHB_NESTED_CLONED pClonedList)
{
  if (pClonedList->pRefs)
  {
    hb_xfree(pClonedList->pRefs);
  }
}

static bool hb_nestedCloneFind(PHB_NESTED_CLONED pClonedList, void *pValue, PHB_ITEM pDest)
{
  if (hb_gcRefCount(pValue) <= 1)
  {
    return false;
  }

  HB_SIZE nFirst = 0;
  HB_SIZE nLast = pClonedList->nCount;
  HB_SIZE nMiddle = (nFirst + nLast) >> 1;
  PHB_NESTED_REF pRef = pClonedList->pRefs;

  while (nFirst < nLast)
  {
    if (reinterpret_cast<HB_PTRUINT>(pRef[nMiddle].value) < reinterpret_cast<HB_PTRUINT>(pValue))
    {
      nFirst = nMiddle + 1;
    }
    else if (reinterpret_cast<HB_PTRUINT>(pRef[nMiddle].value) > reinterpret_cast<HB_PTRUINT>(pValue))
    {
      nLast = nMiddle;
    }
    else
    {
      hb_itemCopy(pDest, pRef[nMiddle].pDest);
      return true;
    }
    nMiddle = (nFirst + nLast) >> 1;
  }

  if (pClonedList->nCount >= pClonedList->nSize)
  {
    pClonedList->nSize += pClonedList->nSize ? pClonedList->nSize >> 1 : 16;
    pClonedList->pRefs =
        static_cast<PHB_NESTED_REF>(hb_xrealloc(pClonedList->pRefs, pClonedList->nSize * sizeof(HB_NESTED_REF)));
  }

  pRef = &pClonedList->pRefs[nMiddle];
  if (nMiddle < pClonedList->nCount)
  {
    memmove(pRef + 1, pRef, (pClonedList->nCount - nMiddle) * sizeof(HB_NESTED_REF));
  }
  pClonedList->nCount++;

  pRef->value = pValue;
  pRef->pDest = pDest;

  return false;
}

void hb_nestedCloneDo(PHB_ITEM pDstItem, PHB_ITEM pSrcItem, PHB_NESTED_CLONED pClonedList)
{
  // Clipper clones nested array ONLY if NOT an Object!!!
  if (pSrcItem->isArray())
  {
    if (!hb_nestedCloneFind(pClonedList, static_cast<void *>(pSrcItem->arrayValue()), pDstItem))
    {
      if (pSrcItem->arrayValue()->uiClass != 0)
      {
        hb_objCloneBody(pDstItem, pSrcItem, pClonedList);
      }
      else
      {
        hb_arrayCloneBody(pDstItem, pSrcItem, pClonedList);
      }
    }
  }
  else if (pSrcItem->isHash())
  {
    if (!hb_nestedCloneFind(pClonedList, static_cast<void *>(pSrcItem->hashValue()), pDstItem))
    {
      hb_hashCloneBody(pDstItem, pSrcItem, pClonedList);
    }
  }
  else
  {
    hb_itemCopy(pDstItem, pSrcItem);
  }
}

PHB_ITEM hb_arrayCloneTo(PHB_ITEM pDest, PHB_ITEM pArray)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayCloneTo(%p,%p)", static_cast<void*>(pDest), static_cast<void*>(pArray)));
#endif

  if (pArray->isArray())
  {
    HB_NESTED_CLONED clonedList;
    hb_nestedCloneInit(&clonedList, static_cast<void *>(pArray->arrayValue()), pDest);
    hb_arrayCloneBody(pDest, pArray, &clonedList);
    hb_nestedCloneFree(&clonedList);
  }
  return pDest;
}

PHB_ITEM hb_arrayClone(PHB_ITEM pArray)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayClone(%p)", static_cast<void*>(pArray)));
#endif

  return hb_arrayCloneTo(hb_itemNew(nullptr), pArray);
}

PHB_ITEM hb_arrayFromStack(HB_USHORT uiLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayFromStack(%hu)", uiLen));
#endif

  HB_STACK_TLS_PRELOAD
  auto pArray = hb_itemNew(nullptr);

  hb_arrayNew(pArray, uiLen);

  for (HB_USHORT uiPos = 1; uiPos <= uiLen; uiPos++)
  {
    hb_arraySet(pArray, uiPos, hb_stackItemFromTop(uiPos - uiLen - 1));
  }

  return pArray;
}

PHB_ITEM hb_arrayFromParams(int iLevel)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayFromParams(%d)", iLevel));
#endif

  HB_STACK_TLS_PRELOAD

  HB_ISIZ nBaseOffset = hb_stackBaseProcOffset(iLevel);
  HB_USHORT uiPCount = (nBaseOffset > 0) ? hb_stackItem(nBaseOffset)->symbolParamCnt() : 0;

  auto pArray = hb_itemArrayNew(uiPCount);
  for (HB_USHORT uiPos = 1; uiPos <= uiPCount; uiPos++)
  {
    hb_arraySet(pArray, uiPos, hb_stackItem(nBaseOffset + uiPos + 1));
  }

  return pArray;
}

PHB_ITEM hb_arrayBaseParams(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayBaseParams()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pArray = hb_itemNew(nullptr);
  HB_USHORT uiPCount = hb_stackBaseItem()->symbolParamCnt();

  hb_arrayNew(pArray, uiPCount);

  for (HB_USHORT uiPos = 1; uiPos <= uiPCount; uiPos++)
  {
    hb_arraySet(pArray, uiPos, hb_stackItemFromBase(uiPos));
  }

  return pArray;
}

PHB_ITEM hb_arraySelfParams(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySelfParams()"));
#endif

  HB_STACK_TLS_PRELOAD

  auto pArray = hb_itemNew(nullptr);
  HB_USHORT uiPCount = hb_stackBaseItem()->symbolParamCnt();

  hb_arrayNew(pArray, uiPCount + 1);

  for (HB_USHORT uiPos = 0; uiPos <= uiPCount; uiPos++)
  {
    hb_arraySet(pArray, uiPos + 1, hb_stackItemFromBase(uiPos));
  }

  return pArray;
}
