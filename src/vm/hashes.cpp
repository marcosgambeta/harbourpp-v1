//
// The Hash tables API (C level)
//
// Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef _HB_HASH_INTERNAL_
#define _HB_HASH_INTERNAL_
#endif

#include "hbvmopt.hpp"
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapierr.hpp"
#include "hbapilng.hpp"
#include "hbvm.hpp"
#include "hbxvm.hpp"
#include "hbstack.hpp"

#define HB_HASH_ITEM_ALLOC 16

// internal structures for hashes
struct _HB_HASHPAIR
{
  HB_ITEM key;
  HB_ITEM value;
};

using HB_HASHPAIR = _HB_HASHPAIR;
using PHB_HASHPAIR = HB_HASHPAIR *;

struct _HB_BASEHASH
{
  PHB_HASHPAIR pPairs; // pointer to the array of key/value pairs
  PHB_ITEM pDefault;   // default autoadd value
  HB_SIZE *pnPos;      // the sort order for HB_HASH_KEEPORDER
  HB_SIZE nSize;       // size of allocated pair array
  HB_SIZE nLen;        // number of used items in pair array
  int iFlags;          // hash item flags
};

using HB_BASEHASH = _HB_BASEHASH;
using PHB_BASEHASH = HB_BASEHASH *;

// This releases hash when called from the garbage collector
static HB_GARBAGE_FUNC(hb_hashGarbageRelease)
{
  auto pBaseHash = static_cast<PHB_BASEHASH>(Cargo);

#if 0
   HB_TRACE(HB_TR_INFO, ("hb_hashGarbageRelease(%p)", static_cast<void*>(pBaseHash)));
#endif

  if (pBaseHash->nSize > 0)
  {
    while (pBaseHash->nLen)
    {
      pBaseHash->nLen--;
      PHB_ITEM pKey = &pBaseHash->pPairs[pBaseHash->nLen].key;
      PHB_ITEM pVal = &pBaseHash->pPairs[pBaseHash->nLen].value;

      // small hack for buggy destructors in hash items
      pBaseHash->iFlags |= HB_HASH_RESORT;

      if (pKey->isGCItem() && pVal->isGCItem())
      {
        HB_STACK_TLS_PRELOAD
        hb_itemRawMove(hb_stackAllocItem(), pVal);
        pKey->clear();
        hb_stackPop();
      }
      else
      {
        if (pKey->isComplex())
        {
          pKey->clear();
        }
        if (pVal->isComplex())
        {
          pVal->clear();
        }
      }
    }

    if (pBaseHash->pnPos)
    {
      hb_xfree(pBaseHash->pnPos);
      pBaseHash->pnPos = nullptr;
    }

    if (pBaseHash->pPairs)
    {
      hb_xfree(pBaseHash->pPairs);
      pBaseHash->pPairs = nullptr;
    }
  }

  if (pBaseHash->pDefault)
  {
    PHB_ITEM pDefault = pBaseHash->pDefault;
    pBaseHash->pDefault = nullptr;
    hb_itemRelease(pDefault);
  }
}

static HB_GARBAGE_FUNC(hb_hashGarbageMark)
{
  auto pBaseHash = static_cast<PHB_BASEHASH>(Cargo);

#if 0
   HB_TRACE(HB_TR_INFO, ("hb_hashMarkGarbage(%p)", static_cast<void*>(pBaseHash)));
#endif

  if (pBaseHash->nLen > 0)
  {
    PHB_HASHPAIR pPairs = pBaseHash->pPairs;
    HB_SIZE nLen = pBaseHash->nLen;

    while (nLen--)
    {
      if ((&pPairs[nLen].key)->isGCItem())
      {
        hb_gcItemRef(&pPairs[nLen].key);
      }
      if ((&pPairs[nLen].value)->isGCItem())
      {
        hb_gcItemRef(&pPairs[nLen].value);
      }
    }
  }
  if (pBaseHash->pDefault)
  {
    hb_gcMark(pBaseHash->pDefault);
  }
}

static const HB_GC_FUNCS s_gcHashFuncs = {hb_hashGarbageRelease, hb_hashGarbageMark};

static int hb_hashItemCmp(PHB_ITEM pKey1, PHB_ITEM pKey2, int iFlags)
{
  if (pKey1->isString())
  {
    if (pKey2->isString())
    {
      if (iFlags & HB_HASH_BINARY)
      {
        return pKey1->stringLength() < pKey2->stringLength()
                   ? -1
                   : (pKey1->stringLength() > pKey2->stringLength()
                          ? 1
                          : memcmp(pKey1->stringValue(), pKey2->stringValue(), pKey1->stringLength()));
      }
      else if (iFlags & HB_HASH_IGNORECASE)
      {
        return hb_itemStrICmp(pKey1, pKey2, true);
      }
      else
      {
        return hb_itemStrCmp(pKey1, pKey2, true);
      }
    }
    else
    {
      return 1;
    }
  }
  else if (pKey1->isDateTime())
  {
    if (pKey2->isDateTime())
    {
      return pKey1->dateTimeJulian() < pKey2->dateTimeJulian()
                 ? -1
                 : (pKey1->dateTimeJulian() > pKey2->dateTimeJulian()
                        ? 1
                        : (pKey1->dateTimeTime() < pKey2->dateTimeTime()
                               ? -1
                               : (pKey1->dateTimeTime() > pKey2->dateTimeTime() ? 1 : 0)));
    }
    else if (pKey2->isString())
    {
      return -1;
    }
    else
    {
      return 1;
    }
  }
  else if (pKey1->isPointer())
  {
    if (pKey2->isPointer())
    {
      return pKey1->pointerValue() < pKey2->pointerValue() ? -1
                                                           : (pKey1->pointerValue() > pKey2->pointerValue() ? 1 : 0);
    }
    else if (pKey2->isString() || pKey2->isDateTime())
    {
      return -1;
    }
    else
    {
      return 1;
    }
  }
  else if (pKey1->isNumInt() && pKey2->isNumInt())
  {
    HB_MAXINT n1 = HB_ITEM_GET_NUMINTRAW(pKey1), n2 = HB_ITEM_GET_NUMINTRAW(pKey2);
    return n1 < n2 ? -1 : (n1 > n2 ? 1 : 0);
  }
  else if (pKey2->isNumeric())
  {
    auto d1 = pKey1->getND();
    auto d2 = pKey2->getND();
    return d1 < d2 ? -1 : (d1 > d2 ? 1 : 0);
  }
  return -1;
}

static void hb_hashResort(PHB_BASEHASH pBaseHash)
{
  auto pPairs = static_cast<PHB_HASHPAIR>(hb_xgrab(pBaseHash->nLen * sizeof(HB_HASHPAIR)));
  for (HB_SIZE nPos = 0; nPos < pBaseHash->nLen; ++nPos)
  {
    memcpy(pPairs + nPos, pBaseHash->pPairs + pBaseHash->pnPos[nPos], sizeof(HB_HASHPAIR));
    pBaseHash->pnPos[nPos] = nPos;
  }

  hb_xfree(pBaseHash->pPairs);
  pBaseHash->pPairs = pPairs;
  pBaseHash->nSize = pBaseHash->nLen;
  pBaseHash->pnPos = static_cast<HB_SIZE *>(hb_xrealloc(pBaseHash->pnPos, pBaseHash->nSize * sizeof(HB_SIZE)));
}

static void hb_hashSortDo(PHB_BASEHASH pBaseHash)
{
  int iFlags = pBaseHash->iFlags;

  if (pBaseHash->pnPos)
  {
    HB_SIZE *pnPos = pBaseHash->pnPos;

    pnPos[0] = 0;
    for (HB_SIZE nFrom = 1; nFrom < pBaseHash->nLen; ++nFrom)
    {
      PHB_ITEM pKey = &pBaseHash->pPairs[nFrom].key;
      HB_SIZE nLeft = 0, nRight = nFrom;

      while (nLeft < nRight)
      {
        HB_SIZE nMiddle = (nLeft + nRight) >> 1;
        int i = hb_hashItemCmp(&pBaseHash->pPairs[pnPos[nMiddle]].key, pKey, iFlags);
        if (i > 0)
        {
          nRight = nMiddle;
        }
        else
        {
          nLeft = nMiddle + 1;
        }
      }
      if (nLeft < nFrom)
      {
        nRight = nFrom;
        do
        {
          pnPos[nRight] = pnPos[nRight - 1];
        } while (--nRight > nLeft);
      }
      pnPos[nLeft] = nFrom;
    }
  }
  else
  {
    // The hash array is probably quite well sorted so this trivial
    // algorithm is the most efficient one [druzus]

    for (HB_SIZE nFrom = 1; nFrom < pBaseHash->nLen; ++nFrom)
    {
      HB_SIZE nPos = nFrom;
      while (nPos > 0 && hb_hashItemCmp(&pBaseHash->pPairs[nPos - 1].key, &pBaseHash->pPairs[nPos].key, iFlags) > 0)
      {
        HB_HASHPAIR pair;
        memcpy(&pair, pBaseHash->pPairs + nPos - 1, sizeof(HB_HASHPAIR));
        memcpy(pBaseHash->pPairs + nPos - 1, pBaseHash->pPairs + nPos, sizeof(HB_HASHPAIR));
        memcpy(pBaseHash->pPairs + nPos, &pair, sizeof(HB_HASHPAIR));
        --nPos;
      }
    }
  }

  pBaseHash->iFlags &= ~HB_HASH_RESORT;
}

static bool hb_hashFind(PHB_BASEHASH pBaseHash, PHB_ITEM pKey, HB_SIZE *pnPos)
{
  int iFlags = pBaseHash->iFlags;

  if (iFlags & HB_HASH_RESORT)
  {
    hb_hashSortDo(pBaseHash);
  }

  HB_SIZE nLeft = 0;
  HB_SIZE nRight = pBaseHash->nLen;

  while (nLeft < nRight)
  {
    HB_SIZE nMiddle = (nLeft + nRight) >> 1;
    int i =
        hb_hashItemCmp(&pBaseHash->pPairs[pBaseHash->pnPos ? pBaseHash->pnPos[nMiddle] : nMiddle].key, pKey, iFlags);
    if (i == 0)
    {
      *pnPos = pBaseHash->pnPos ? pBaseHash->pnPos[nMiddle] : nMiddle;
      return true;
    }
    else if (i < 0)
    {
      nLeft = nMiddle + 1;
    }
    else
    {
      nRight = nMiddle;
    }
  }

  *pnPos = nLeft;
  return false;
}

static void hb_hashResize(PHB_BASEHASH pBaseHash, HB_SIZE nNewSize)
{
  if (pBaseHash->nSize < nNewSize)
  {
    if (pBaseHash->nSize)
    {
      pBaseHash->pPairs = static_cast<PHB_HASHPAIR>(hb_xrealloc(pBaseHash->pPairs, nNewSize * sizeof(HB_HASHPAIR)));
      if (pBaseHash->pnPos)
      {
        pBaseHash->pnPos = static_cast<HB_SIZE *>(hb_xrealloc(pBaseHash->pnPos, nNewSize * sizeof(HB_SIZE)));
      }
    }
    else
    {
      pBaseHash->pPairs = static_cast<PHB_HASHPAIR>(hb_xgrab(nNewSize * sizeof(HB_HASHPAIR)));
      if (pBaseHash->iFlags & HB_HASH_KEEPORDER)
      {
        pBaseHash->pnPos = static_cast<HB_SIZE *>(hb_xgrab(nNewSize * sizeof(HB_SIZE)));
      }
    }

    do
    {
      pBaseHash->pPairs[pBaseHash->nSize].key.type = Harbour::Item::NIL;
      pBaseHash->pPairs[pBaseHash->nSize].value.type = Harbour::Item::NIL;
    } while (++pBaseHash->nSize < nNewSize);
  }
  else if (pBaseHash->nSize > nNewSize && pBaseHash->nLen <= nNewSize)
  {
    pBaseHash->nSize = nNewSize;
    if (nNewSize)
    {
      pBaseHash->pPairs = static_cast<PHB_HASHPAIR>(hb_xrealloc(pBaseHash->pPairs, nNewSize * sizeof(HB_HASHPAIR)));
      if (pBaseHash->pnPos)
      {
        pBaseHash->pnPos = static_cast<HB_SIZE *>(hb_xrealloc(pBaseHash->pnPos, nNewSize * sizeof(HB_SIZE)));
      }
    }
    else
    {
      hb_xfree(pBaseHash->pPairs);
      pBaseHash->pPairs = nullptr;
      if (pBaseHash->pnPos)
      {
        hb_xfree(pBaseHash->pnPos);
        pBaseHash->pnPos = nullptr;
      }
    }
  }
}

static PHB_ITEM hb_hashValuePtr(PHB_BASEHASH pBaseHash, PHB_ITEM pKey, bool fAdd)
{
  HB_SIZE nPos;

  if (!hb_hashFind(pBaseHash, pKey, &nPos))
  {
    if (!fAdd)
    {
      return nullptr;
    }

    if (pBaseHash->nSize == pBaseHash->nLen)
    {
      hb_hashResize(pBaseHash, pBaseHash->nSize + HB_HASH_ITEM_ALLOC);
    }

    if (pBaseHash->pnPos)
    {
      memmove(pBaseHash->pnPos + nPos + 1, pBaseHash->pnPos + nPos, (pBaseHash->nLen - nPos) * sizeof(HB_SIZE));
      nPos = (pBaseHash->pnPos[nPos] = pBaseHash->nLen);
    }
    else if (nPos < pBaseHash->nLen)
    {
      memmove(pBaseHash->pPairs + nPos + 1, pBaseHash->pPairs + nPos, (pBaseHash->nLen - nPos) * sizeof(HB_HASHPAIR));
      pBaseHash->pPairs[nPos].key.type = Harbour::Item::NIL;
      pBaseHash->pPairs[nPos].value.type = Harbour::Item::NIL;
    }

    pBaseHash->nLen++;
    hb_itemCopy(&pBaseHash->pPairs[nPos].key, pKey);
    if (pBaseHash->pDefault)
    {
      hb_itemCloneTo(&pBaseHash->pPairs[nPos].value, pBaseHash->pDefault);
    }
  }

  return &pBaseHash->pPairs[nPos].value;
}

static bool hb_hashNewValue(PHB_BASEHASH pBaseHash, PHB_ITEM pKey, PHB_ITEM pValue)
{
  HB_SIZE nPos;

  if (!hb_hashFind(pBaseHash, pKey, &nPos))
  {
    if (pBaseHash->nSize == pBaseHash->nLen)
    {
      hb_hashResize(pBaseHash, pBaseHash->nSize + HB_HASH_ITEM_ALLOC);
    }

    if (pBaseHash->pnPos)
    {
      memmove(pBaseHash->pnPos + nPos + 1, pBaseHash->pnPos + nPos, (pBaseHash->nLen - nPos) * sizeof(HB_SIZE));
      nPos = (pBaseHash->pnPos[nPos] = pBaseHash->nLen);
    }
    else if (nPos < pBaseHash->nLen)
    {
      memmove(pBaseHash->pPairs + nPos + 1, pBaseHash->pPairs + nPos, (pBaseHash->nLen - nPos) * sizeof(HB_HASHPAIR));
      pBaseHash->pPairs[nPos].key.type = Harbour::Item::NIL;
      pBaseHash->pPairs[nPos].value.type = Harbour::Item::NIL;
    }

    pBaseHash->nLen++;
    hb_itemCopy(&pBaseHash->pPairs[nPos].key, pKey);
    hb_itemCopyFromRef(&pBaseHash->pPairs[nPos].value, pValue);

    return true;
  }

  return false;
}

static void hb_hashNewPair(PHB_BASEHASH pBaseHash, PHB_ITEM *pKeyPtr, PHB_ITEM *pValPtr)
{
  if (pBaseHash->nSize == pBaseHash->nLen)
  {
    hb_hashResize(pBaseHash, pBaseHash->nSize + HB_HASH_ITEM_ALLOC);
  }

  if (pBaseHash->pnPos)
  {
    pBaseHash->pnPos[pBaseHash->nLen] = pBaseHash->nLen;
  }

  *pKeyPtr = &pBaseHash->pPairs[pBaseHash->nLen].key;
  *pValPtr = &pBaseHash->pPairs[pBaseHash->nLen].value;

  pBaseHash->nLen++;
}

static void hb_hashDelPair(PHB_BASEHASH pBaseHash, HB_SIZE nPos)
{
  if (--pBaseHash->nLen == 0)
  {
    PHB_HASHPAIR pPairs = pBaseHash->pPairs;
    pBaseHash->pPairs = nullptr;
    pBaseHash->nSize = 0;
    if (pBaseHash->pnPos)
    {
      hb_xfree(pBaseHash->pnPos);
      pBaseHash->pnPos = nullptr;
    }
    if ((&pPairs->key)->isComplex())
    {
      (&pPairs->key)->clear();
    }
    if ((&pPairs->value)->isComplex())
    {
      (&pPairs->value)->clear();
    }
    hb_xfree(pPairs);
  }
  else
  {
    if (pBaseHash->pnPos && (pBaseHash->iFlags & HB_HASH_RESORT) == 0)
    {
#ifdef HB_FAST_HASH_DEL
      HB_SIZE *pnLast;

      HB_SIZE *pnPos = pBaseHash->pnPos + pBaseHash->nLen;
      HB_SIZE *pnDel = pnLast = nullptr;
      for (;;)
      {
        if (*pnPos == nPos)
        {
          pnDel = pnPos;
          if (pnLast != nullptr)
          {
            break;
          }
        }
        if (*pnPos == pBaseHash->nLen)
        {
          pnLast = pnPos;
          if (pnDel != nullptr)
          {
            break;
          }
        }
        if (pnPos-- == pBaseHash->pnPos)
        {
          hb_errInternal(HB_EI_ERRUNRECOV, "HB_HDEL(): corrupted hash index", nullptr, nullptr);
        }
      }
      *pnLast = *pnDel;
      if (pnDel < pBaseHash->pnPos + pBaseHash->nLen)
      {
        memmove(pnDel, pnDel + 1, (pBaseHash->pnPos + pBaseHash->nLen - pnDel) * sizeof(HB_SIZE));
      }
      if (nPos != pBaseHash->nLen)
      {
        HB_HASHPAIR pair;
        memcpy(&pair, pBaseHash->pPairs + nPos, sizeof(HB_HASHPAIR));
        memcpy(pBaseHash->pPairs + nPos, pBaseHash->pPairs + pBaseHash->nLen, sizeof(HB_HASHPAIR));
        nPos = pBaseHash->nLen;
        memcpy(pBaseHash->pPairs + nPos, &pair, sizeof(HB_HASHPAIR));
      }
#else
      HB_SIZE n = 0;
      while (n < pBaseHash->nLen)
      {
        if (pBaseHash->pnPos[n] > nPos)
        {
          pBaseHash->pnPos[n++]--;
        }
        else if (pBaseHash->pnPos[n] == nPos)
        {
          memmove(&pBaseHash->pnPos[n], &pBaseHash->pnPos[n + 1], (pBaseHash->nLen - n) * sizeof(HB_SIZE));
        }
        else
        {
          ++n;
        }
      }
#endif
    }

    if (nPos != pBaseHash->nLen)
    {
      HB_HASHPAIR pair;
      memcpy(&pair, pBaseHash->pPairs + nPos, sizeof(HB_HASHPAIR));
      memmove(pBaseHash->pPairs + nPos, pBaseHash->pPairs + nPos + 1, (pBaseHash->nLen - nPos) * sizeof(HB_HASHPAIR));
      nPos = pBaseHash->nLen;
      memcpy(pBaseHash->pPairs + nPos, &pair, sizeof(HB_HASHPAIR));
    }

    hb_itemSetNil(&pBaseHash->pPairs[nPos].key);
    hb_itemSetNil(&pBaseHash->pPairs[nPos].value);
    if (pBaseHash->nSize - pBaseHash->nLen > (HB_HASH_ITEM_ALLOC << 1))
    {
      pBaseHash->nSize -= HB_HASH_ITEM_ALLOC;
      pBaseHash->pPairs =
          static_cast<PHB_HASHPAIR>(hb_xrealloc(pBaseHash->pPairs, pBaseHash->nSize * sizeof(HB_HASHPAIR)));
      if (pBaseHash->pnPos)
      {
        pBaseHash->pnPos = static_cast<HB_SIZE *>(hb_xrealloc(pBaseHash->pnPos, pBaseHash->nSize * sizeof(HB_SIZE)));
      }
    }
  }
}

PHB_ITEM hb_hashNew(PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashNew(%p)", static_cast<void*>(pItem)));
#endif

  if (pItem == nullptr)
  {
    pItem = hb_itemNew(nullptr);
  }
  else if (pItem->isComplex())
  {
    pItem->clear();
  }

  auto pBaseHash = static_cast<PHB_BASEHASH>(hb_gcAllocRaw(sizeof(HB_BASEHASH), &s_gcHashFuncs));
  pBaseHash->pPairs = nullptr;
  pBaseHash->pnPos = nullptr;
  pBaseHash->nSize = 0;
  pBaseHash->nLen = 0;
  pBaseHash->iFlags = HB_HASH_FLAG_DEFAULT;
  pBaseHash->pDefault = nullptr;

  pItem->setType(Harbour::Item::HASH);
  pItem->setHashValue(pBaseHash);

  return pItem;
}

HB_SIZE hb_hashLen(PHB_ITEM pHash)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashLen(%p)", static_cast<void*>(pHash)));
#endif

  if (pHash->isHash())
  {
    return pHash->hashValue()->nLen;
  }
  else
  {
    return 0;
  }
}

void hb_hashPreallocate(PHB_ITEM pHash, HB_SIZE nNewSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashPreallocate(%p,%" HB_PFS "u)", static_cast<void*>(pHash), nNewSize));
#endif

  if (pHash->isHash())
  {
    hb_hashResize(pHash->hashValue(), nNewSize);
  }
}

HB_BOOL hb_hashAllocNewPair(PHB_ITEM pHash, PHB_ITEM *pKeyPtr, PHB_ITEM *pValPtr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashAllocNewPair(%p,%p,%p)", static_cast<void*>(pHash), static_cast<void*>(pKeyPtr), static_cast<void*>(pValPtr)));
#endif

  if (pHash->isHash())
  {
    hb_hashNewPair(pHash->hashValue(), pKeyPtr, pValPtr);
    return true;
  }
  else
  {
    return false;
  }
}

void hb_hashSort(PHB_ITEM pHash)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashSort(%p)", static_cast<void*>(pHash)));
#endif

  if (pHash->isHash())
  {
    PHB_BASEHASH pBaseHash = pHash->hashValue();

    if (pBaseHash->iFlags & HB_HASH_RESORT)
    {
      hb_hashSortDo(pBaseHash);
    }

    if (pBaseHash->pnPos)
    {
      hb_hashResort(pBaseHash);
    }
  }
}

PHB_ITEM hb_hashGetItemPtr(PHB_ITEM pHash, PHB_ITEM pKey, int iFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetItemPtr(%p,%p,%d)", static_cast<void*>(pHash), static_cast<void*>(pKey), iFlags));
#endif

  if (pHash->isHash() && pKey->isHashKey())
  {
    PHB_ITEM pDest =
        hb_hashValuePtr(pHash->hashValue(), pKey, iFlags && (pHash->hashValue()->iFlags & iFlags) == iFlags);
    if (pDest)
    {
      return pDest->isByRef() ? hb_itemUnRef(pDest) : pDest;
    }
  }

  return nullptr;
}

PHB_ITEM hb_hashGetCItemPtr(PHB_ITEM pHash, const char *pszKey)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetCItemPtr(%p,%s)", static_cast<void*>(pHash), pszKey));
#endif

  if (pHash->isHash())
  {
    HB_STACK_TLS_PRELOAD
    // we will not make any copy of pKey (autoadd is disabled) so it's
    // safe to use hb_itemPutCConst()
    auto pKey = hb_itemPutCConst(hb_stackAllocItem(), pszKey);
    PHB_ITEM pDest = hb_hashValuePtr(pHash->hashValue(), pKey, false);
    hb_stackPop();
    if (pDest)
    {
      return pDest->isByRef() ? hb_itemUnRef(pDest) : pDest;
    }
  }

  return nullptr;
}

HB_SIZE hb_hashGetCItemPos(PHB_ITEM pHash, const char *pszKey)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetCItemPos(%p,%s)", static_cast<void*>(pHash), pszKey));
#endif

  HB_SIZE nPos = 0;

  if (pHash->isHash())
  {
    HB_STACK_TLS_PRELOAD
    // we will not make any copy of pKey (autoadd is disabled) so it's
    // safe to use hb_itemPutCConst()
    auto pKey = hb_itemPutCConst(hb_stackAllocItem(), pszKey);

    if (hb_hashFind(pHash->hashValue(), pKey, &nPos))
    {
      nPos++;
    }
    else
    {
      nPos = 0;
    }
    hb_stackPop();
  }

  return nPos;
}

PHB_ITEM hb_hashGetItemRefPtr(PHB_ITEM pHash, PHB_ITEM pKey)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetItemRefPtr(%p,%p)", static_cast<void*>(pHash), static_cast<void*>(pKey)));
#endif

  if (pHash->isHash() && pKey->isHashKey())
  {
    PHB_ITEM pDest =
        hb_hashValuePtr(pHash->hashValue(), pKey,
                        (pHash->hashValue()->iFlags & HB_HASH_AUTOADD_REFERENCE) == HB_HASH_AUTOADD_REFERENCE);
    if (pDest)
    {
      if (!pDest->isByRef())
      {
        pDest = hb_memvarDetachLocal(pDest);
      }
      return pDest;
    }
  }

  return nullptr;
}

HB_BOOL hb_hashScan(PHB_ITEM pHash, PHB_ITEM pKey, HB_SIZE *pnPos)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashScan(%p,%p,%p)", static_cast<void*>(pHash), static_cast<void*>(pKey), static_cast<void*>(pnPos)));
#endif

  if (pHash->isHash())
  {
    HB_SIZE nPos;
    if (pKey->isHashKey())
    {
      if (hb_hashFind(pHash->hashValue(), pKey, &nPos))
      {
        if (pnPos)
        {
          *pnPos = nPos + 1;
        }
        return true;
      }
    }
    else if (pKey->isHash() && pKey->hashValue()->nLen == 1)
    {
      if (hb_hashFind(pHash->hashValue(), &pKey->hashValue()->pPairs[0].key, &nPos))
      {
        PHB_ITEM pVal1 = &pHash->hashValue()->pPairs[nPos].value;
        PHB_ITEM pVal2 = &pKey->hashValue()->pPairs[0].value;

        if (hb_itemEqual(pVal1, pVal2))
        {
          if (pnPos)
          {
            *pnPos = nPos + 1;
          }
          return true;
        }
      }
    }
  }
  if (pnPos)
  {
    *pnPos = 0;
  }
  return false;
}

HB_BOOL hb_hashScanSoft(PHB_ITEM pHash, PHB_ITEM pKey, HB_SIZE *pnPos)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashScanSoft(%p,%p,%p)", static_cast<void*>(pHash), static_cast<void*>(pKey), static_cast<void*>(pnPos)));
#endif

  if (pHash->isHash() && pKey->isHashKey())
  {
    HB_SIZE nPos;
    if (hb_hashFind(pHash->hashValue(), pKey, &nPos))
    {
      if (pnPos)
      {
        *pnPos = nPos + 1;
      }
      return true;
    }
    else
    {
      if (pnPos)
      {
        if (nPos != 0 && pHash->hashValue()->pnPos)
        {
          nPos = pHash->hashValue()->pnPos[nPos - 1] + 1;
        }
        *pnPos = nPos;
      }
      return false;
    }
  }
  if (pnPos)
  {
    *pnPos = 0;
  }
  return false;
}

HB_BOOL hb_hashClear(PHB_ITEM pHash)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashClear(%p)", static_cast<void*>(pHash)));
#endif

  if (pHash->isHash())
  {
    if (pHash->hashValue()->nSize)
    {
      while (pHash->hashValue()->nLen)
      {
        pHash->hashValue()->nLen--;
        if ((&pHash->hashValue()->pPairs[pHash->hashValue()->nLen].key)->isComplex())
        {
          (&pHash->hashValue()->pPairs[pHash->hashValue()->nLen].key)->clear();
        }
        if ((&pHash->hashValue()->pPairs[pHash->hashValue()->nLen].value)->isComplex())
        {
          (&pHash->hashValue()->pPairs[pHash->hashValue()->nLen].value)->clear();
        }
      }
      // This condition is a protection against recursive call
      // from .prg object destructor [druzus]
      if (pHash->hashValue()->nSize)
      {
        hb_xfree(pHash->hashValue()->pPairs);
        pHash->hashValue()->pPairs = nullptr;
        pHash->hashValue()->nSize = 0;
        if (pHash->hashValue()->pnPos)
        {
          hb_xfree(pHash->hashValue()->pnPos);
          pHash->hashValue()->pnPos = nullptr;
        }
      }
    }
    return true;
  }

  return false;
}

HB_BOOL hb_hashDel(PHB_ITEM pHash, PHB_ITEM pKey)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashDel(%p,%p)", static_cast<void*>(pHash), static_cast<void*>(pKey)));
#endif

  if (pHash->isHash() && pKey->isHashKey())
  {
    PHB_BASEHASH pBaseHash = pHash->hashValue();
    HB_SIZE nPos;

    if (hb_hashFind(pBaseHash, pKey, &nPos))
    {
      hb_hashDelPair(pBaseHash, nPos);
      return true;
    }
  }

  return false;
}

HB_BOOL hb_hashRemove(PHB_ITEM pHash, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashRemove(%p,%p)", static_cast<void*>(pHash), static_cast<void*>(pItem)));
#endif

  if (pHash->isHash())
  {
    if (pItem->isHashKey())
    {
      hb_hashDel(pHash, pItem);
      return true;
    }
    else if (pItem->isArray())
    {
      HB_SIZE n = 0;
      PHB_ITEM pKey;
      while ((pKey = hb_arrayGetItemPtr(pItem, ++n)) != nullptr)
      {
        hb_hashDel(pHash, pKey);
      }
      return true;
    }
    else if (pItem->isHash())
    {
      if (pHash->hashValue() == pItem->hashValue())
      {
        hb_hashClear(pHash);
      }
      else
      {
        HB_SIZE nLen = 0;
        while (nLen < pItem->hashValue()->nLen)
        {
          hb_hashDel(pHash, &pItem->hashValue()->pPairs[nLen++].key);
        }
      }
      return true;
    }
  }
  return false;
}

HB_BOOL hb_hashAdd(PHB_ITEM pHash, PHB_ITEM pKey, PHB_ITEM pValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashAdd(%p,%p,%p)", static_cast<void*>(pHash), static_cast<void*>(pKey), static_cast<void*>(pValue)));
#endif

  if (pHash->isHash() && pKey->isHashKey())
  {
    PHB_ITEM pDest = hb_hashValuePtr(pHash->hashValue(), pKey, true);
    if (pDest)
    {
      if (pDest->isByRef())
      {
        pDest = hb_itemUnRef(pDest);
      }
      if (pValue)
      {
        hb_itemCopyFromRef(pDest, pValue);
      }
      else
      {
        hb_itemSetNil(pDest);
      }
      return true;
    }
  }

  return false;
}

HB_BOOL hb_hashAddNew(PHB_ITEM pHash, PHB_ITEM pKey, PHB_ITEM pValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashAddNew(%p,%p,%p)", static_cast<void*>(pHash), static_cast<void*>(pKey), static_cast<void*>(pValue)));
#endif

  if (pHash->isHash() && pKey->isHashKey())
  {
    return hb_hashNewValue(pHash->hashValue(), pKey, pValue);
  }
  else
  {
    return false;
  }
}

PHB_ITEM hb_hashGetKeyAt(PHB_ITEM pHash, HB_SIZE nPos)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetKeyAt(%p,%" HB_PFS "u)", static_cast<void*>(pHash), nPos));
#endif

  if (pHash->isHash() && nPos > 0 && nPos <= pHash->hashValue()->nLen)
  {
    return &pHash->hashValue()->pPairs[nPos - 1].key;
  }
  else
  {
    return nullptr;
  }
}

PHB_ITEM hb_hashGetValueAt(PHB_ITEM pHash, HB_SIZE nPos)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetValueAt(%p,%" HB_PFS "u)", static_cast<void*>(pHash), nPos));
#endif

  if (pHash->isHash() && nPos > 0 && nPos <= pHash->hashValue()->nLen)
  {
    PHB_ITEM pValue = &pHash->hashValue()->pPairs[nPos - 1].value;
    return pValue->isByRef() ? hb_itemUnRef(pValue) : pValue;
  }
  else
  {
    return nullptr;
  }
}

HB_BOOL hb_hashDelAt(PHB_ITEM pHash, HB_SIZE nPos)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashDelAt(%p,%" HB_PFS "u)", static_cast<void*>(pHash), nPos));
#endif

  if (pHash->isHash() && nPos > 0 && nPos <= pHash->hashValue()->nLen)
  {
    hb_hashDelPair(pHash->hashValue(), nPos - 1);
    return true;
  }
  else
  {
    return false;
  }
}

// retrieves the hash unique ID
void *hb_hashId(PHB_ITEM pHash)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashId(%p)", static_cast<void*>(pHash)));
#endif

  if (pHash->isHash())
  {
    return static_cast<void *>(pHash->hashValue());
  }
  else
  {
    return nullptr;
  }
}

// retrieves numer of references to the hash
HB_COUNTER hb_hashRefs(PHB_ITEM pHash)
{
  if (pHash->isHash())
  {
    return hb_gcRefCount(pHash->hashValue());
  }
  else
  {
    return 0;
  }
}

void hb_hashCloneBody(PHB_ITEM pDest, PHB_ITEM pHash, PHB_NESTED_CLONED pClonedList)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashCloneBody(%p,%p,%p)", static_cast<void*>(pDest), static_cast<void*>(pHash), static_cast<void*>(pClonedList)));
#endif

  hb_hashNew(pDest);
  pDest->hashValue()->iFlags = pHash->hashValue()->iFlags;
  hb_hashResize(pDest->hashValue(), pHash->hashValue()->nLen);
  if (pHash->hashValue()->pDefault)
  {
    pDest->hashValue()->pDefault = hb_itemNew(pHash->hashValue()->pDefault);
    hb_gcUnlock(pDest->hashValue()->pDefault);
  }
  if (pHash->hashValue()->pnPos)
  {
    memcpy(pDest->hashValue()->pnPos, pHash->hashValue()->pnPos, pHash->hashValue()->nLen * sizeof(HB_SIZE));
  }
  for (HB_SIZE nPos = 0; nPos < pHash->hashValue()->nLen; ++nPos)
  {
    PHB_ITEM pValue = &pHash->hashValue()->pPairs[nPos].value;
    if (pValue->isByRef())
    {
      pValue = hb_itemUnRef(pValue);
    }
    hb_itemCopy(&pDest->hashValue()->pPairs[nPos].key, &pHash->hashValue()->pPairs[nPos].key);
    pDest->hashValue()->nLen++;
    hb_nestedCloneDo(&pDest->hashValue()->pPairs[nPos].value, pValue, pClonedList);
  }
}

PHB_ITEM hb_hashCloneTo(PHB_ITEM pDest, PHB_ITEM pHash)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashCloneTo(%p,%p)", static_cast<void*>(pDest), static_cast<void*>(pHash)));
#endif

  if (pHash->isHash())
  {
    HB_NESTED_CLONED clonedList;

    hb_nestedCloneInit(&clonedList, static_cast<void *>(pHash->hashValue()), pDest);
    hb_hashCloneBody(pDest, pHash, &clonedList);
    hb_nestedCloneFree(&clonedList);
  }

  return pDest;
}

PHB_ITEM hb_hashClone(PHB_ITEM pHash)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashClone(%p)", static_cast<void*>(pHash)));
#endif

  return hb_hashCloneTo(hb_itemNew(nullptr), pHash);
}

void hb_hashJoin(PHB_ITEM pDest, PHB_ITEM pSource, int iType)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashJoin(%p,%p,%d)", static_cast<void*>(pDest), static_cast<void*>(pSource), iType));
#endif

  if (pDest->isHash() && pSource->isHash())
  {
    PHB_BASEHASH pBaseHash;
    HB_SIZE nPos;

    switch (iType)
    {
    case HB_HASH_UNION: // OR
      pBaseHash = pSource->hashValue();
      if (pBaseHash != pDest->hashValue())
      {
        for (nPos = 0; nPos < pBaseHash->nLen; ++nPos)
        {
          PHB_ITEM pVal = &pBaseHash->pPairs[nPos].value;
          if (pVal->isByRef())
          {
            pVal = hb_itemUnRef(pVal);
          }
          hb_hashAdd(pDest, &pBaseHash->pPairs[nPos].key, pVal);
        }
      }
      break;

    case HB_HASH_INTERSECT: // AND
      pBaseHash = pDest->hashValue();
      if (pBaseHash != pSource->hashValue())
      {
        for (nPos = 0; nPos < pBaseHash->nLen;)
        {
          HB_SIZE nSrcPos;
          if (hb_hashFind(pSource->hashValue(), &pBaseHash->pPairs[nPos].key, &nSrcPos))
          {
            PHB_ITEM pDestVal = &pBaseHash->pPairs[nPos].value;
            if (pDestVal->isByRef())
            {
              pDestVal = hb_itemUnRef(pDestVal);
            }
            hb_itemCopyFromRef(pDestVal, &pSource->hashValue()->pPairs[nSrcPos].value);
            ++nPos;
          }
          else
          {
            hb_hashDelPair(pBaseHash, nPos);
          }
        }
      }
      break;

    case HB_HASH_DIFFERENCE: // XOR
      pBaseHash = pSource->hashValue();
      if (pBaseHash == pDest->hashValue())
      {
        hb_hashClear(pDest);
      }
      else
      {
        for (nPos = 0; nPos < pBaseHash->nLen; ++nPos)
        {
          if (!hb_hashDel(pDest, &pBaseHash->pPairs[nPos].key))
          {
            PHB_ITEM pVal = &pBaseHash->pPairs[nPos].value;
            if (pVal->isByRef())
            {
              pVal = hb_itemUnRef(pVal);
            }
            hb_hashAdd(pDest, &pBaseHash->pPairs[nPos].key, pVal);
          }
        }
      }
      break;

    case HB_HASH_REMOVE: // NOT -> h1 AND (h1 XOR h2)
      pBaseHash = pSource->hashValue();
      if (pBaseHash == pDest->hashValue())
      {
        hb_hashClear(pDest);
      }
      else
      {
        for (nPos = 0; nPos < pBaseHash->nLen; ++nPos)
        {
          hb_hashDel(pDest, &pBaseHash->pPairs[nPos].key);
        }
      }
      break;
    }
  }
}

PHB_ITEM hb_hashGetKeys(PHB_ITEM pHash)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetKeys(%p)", static_cast<void*>(pHash)));
#endif

  if (pHash->isHash())
  {
    auto pKeys = hb_itemArrayNew(hb_hashLen(pHash));
    PHB_ITEM pKey;
    HB_SIZE nPos = 0;

    while ((pKey = hb_hashGetKeyAt(pHash, ++nPos)) != nullptr)
    {
      auto pDest = hb_arrayGetItemPtr(pKeys, nPos);
      if (!pDest)
      {
        break;
      }
      hb_itemCopy(pDest, pKey);
    }
    return pKeys;
  }

  return nullptr;
}

PHB_ITEM hb_hashGetValues(PHB_ITEM pHash)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetValues(%p)", static_cast<void*>(pHash)));
#endif

  if (pHash->isHash())
  {
    auto pValues = hb_itemArrayNew(hb_hashLen(pHash));
    PHB_ITEM pVal;
    HB_SIZE nPos = 0;

    while ((pVal = hb_hashGetValueAt(pHash, ++nPos)) != nullptr)
    {
      auto pDest = hb_arrayGetItemPtr(pValues, nPos);
      if (!pDest)
      {
        break;
      }
      hb_itemCopy(pDest, pVal);
    }
    return pValues;
  }

  return nullptr;
}

void hb_hashSetDefault(PHB_ITEM pHash, PHB_ITEM pValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashSetDefault(%p,%p)", static_cast<void*>(pHash), static_cast<void*>(pValue)));
#endif

  if (pHash->isHash())
  {
    if (pHash->hashValue()->pDefault)
    {
      hb_itemRelease(pHash->hashValue()->pDefault);
      pHash->hashValue()->pDefault = nullptr;
    }
    if (pValue && !pValue->isNil() && (!pValue->isHash() || pHash->hashValue() != pValue->hashValue()))
    {
      pHash->hashValue()->pDefault = hb_itemClone(pValue);
      hb_gcUnlock(pHash->hashValue()->pDefault);
    }
  }
}

PHB_ITEM hb_hashGetDefault(PHB_ITEM pHash)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetDefault(%p)", static_cast<void*>(pHash)));
#endif

  return pHash->isHash() ? pHash->hashValue()->pDefault : nullptr;
}

void hb_hashSetFlags(PHB_ITEM pHash, int iFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashSetFlags(%p,%d)", static_cast<void*>(pHash), iFlags));
#endif

  if (pHash->isHash())
  {
    pHash->hashValue()->iFlags |= iFlags;
    if (pHash->hashValue()->pnPos == nullptr && pHash->hashValue()->nSize &&
        (pHash->hashValue()->iFlags & HB_HASH_KEEPORDER) != 0)
    {
      HB_SIZE n = pHash->hashValue()->nSize;

      pHash->hashValue()->pnPos = static_cast<HB_SIZE *>(hb_xgrab(n * sizeof(HB_SIZE)));
      do
      {
        --n;
        pHash->hashValue()->pnPos[n] = n;
      } while (n);
    }
  }
}

void hb_hashClearFlags(PHB_ITEM pHash, int iFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashClearFlags(%p,%d)", static_cast<void*>(pHash), iFlags));
#endif

  if (pHash->isHash())
  {
    pHash->hashValue()->iFlags &= ~iFlags;
    if (pHash->hashValue()->pnPos != nullptr && (pHash->hashValue()->iFlags & HB_HASH_KEEPORDER) == 0)
    {
      hb_hashResort(pHash->hashValue());
      hb_xfree(pHash->hashValue()->pnPos);
      pHash->hashValue()->pnPos = nullptr;
    }
  }
}

int hb_hashGetFlags(PHB_ITEM pHash)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetFlags(%p)", static_cast<void*>(pHash)));
#endif

  return pHash->isHash() ? pHash->hashValue()->iFlags : 0;
}
