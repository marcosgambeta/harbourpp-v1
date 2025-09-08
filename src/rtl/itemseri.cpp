//
// Item serialization code
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

#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbapicdp.hpp"
#include "hbapierr.hpp"
#include "hbzlib.h"
#include "hbvm.hpp"
#include "hbstack.hpp"
#include "hbserial.ch"

/*
HB_UCHAR [1] - item type
   0. NIL               0
   1. TRUE              0
   2. FALSE             0
   3. ZERO              0
   4. INT8              1
   5. INT16             2
   6. INT24             3
   7. INT32             4
   8. INT64             8
   9. DOUBLE IEEE754 LE 8
  10. DATE              3
  11. STRING8           1+n
  12. STRING16          2+n
  13. STRING32          4+n
  14. ARRAY8            1+n
  15. ARRAY16           2+n
  16. ARRAY32           4+n
  17. ARRAYREF8         1+n
  18. ARRAYREF16        2+n
  19. ARRAYREF32        4+n
  20. HASH8             1+n
  21. HASH16            2+n
  22. HASH32            4+n
  23. HASHREF8          1+n
  24. HASHREF16         2+n
  25. HASHREF32         4+n
  26. SYMBOL            1+n
  27. CYCLIC REFERENCE  4
  28. OBJECT MARKER     n+1+m+1
  29. STRNUL            0
  30. STRPAD8           1+1+n
  31. STRPAD16          2+2+n
  32. STRPAD32          4+4+n
  33. INT8NUM           1+1
  34. INT16NUM          2+2
  35. INT24NUM          3+1
  36. INT32NUM          4+1
  37. INT64NUM          8+1
  38. DBLNUM            8+1+1
  39. TIMESTAMP         8
  40. HASHFLAGS         2
  41. HASHDEFAULT VALUE 0
  42. ZCOMPRESS         4+4+n

xHarbour types HB_SERIAL_XHB_*:
  67. 'C' <BE64:n><str> 8+n
  76. 'L' 'T'|'F'       1
  78. 'N' 'I'<BE64>     1+8
  78. 'N' 'L'<BE64>     1+8
  78. 'N' 'X'<BE64>     1+8
  78. 'N' 'D'<IEEE754LE>1+8
  68. 'D' <BE64>        8
  84. 'T' <IEEE754LE>   8
  90. 'Z'               0
complex ones:
  65. 'A' <BE64>        8+n   <val>,...
  66. 'B'               1+n   <val>       (HB_SaveBlock())
  72. 'H' <BE64>        8+n   <key,val>,...
  79. 'O' <BE64>        8+n   <clsname>,<msg,val>,...   (__ClsGetPropertiesAndValues())
  81. 'Q' <BE64:n>      8+n   <clsname>,HBPersistent:SaveToText(raw)
  82. 'R' 'A' <BE64>    1+8   (index to array of arrays)
  82. 'R' 'O' <BE64>    1+8   (index to array of objects)
  82. 'R' 'H' <BE64>    1+8   (index to array of hashes)
  82. 'R' 'B' <BE64>    1+8   (index to array of codeblock)
*/

#define HB_SERIAL_NIL 0
#define HB_SERIAL_TRUE 1
#define HB_SERIAL_FALSE 2
#define HB_SERIAL_ZERO 3
#define HB_SERIAL_INT8 4
#define HB_SERIAL_INT16 5
#define HB_SERIAL_INT24 6
#define HB_SERIAL_INT32 7
#define HB_SERIAL_INT64 8
#define HB_SERIAL_DOUBLE 9
#define HB_SERIAL_DATE 10
#define HB_SERIAL_STRING8 11
#define HB_SERIAL_STRING16 12
#define HB_SERIAL_STRING32 13
#define HB_SERIAL_ARRAY8 14
#define HB_SERIAL_ARRAY16 15
#define HB_SERIAL_ARRAY32 16
#define HB_SERIAL_ARRAYREF8 17
#define HB_SERIAL_ARRAYREF16 18
#define HB_SERIAL_ARRAYREF32 19
#define HB_SERIAL_HASH8 20
#define HB_SERIAL_HASH16 21
#define HB_SERIAL_HASH32 22
#define HB_SERIAL_HASHREF8 23
#define HB_SERIAL_HASHREF16 24
#define HB_SERIAL_HASHREF32 25
#define HB_SERIAL_SYMBOL 26
#define HB_SERIAL_REF 27
#define HB_SERIAL_OBJ 28
#define HB_SERIAL_STRNUL 29
#define HB_SERIAL_STRPAD8 30
#define HB_SERIAL_STRPAD16 31
#define HB_SERIAL_STRPAD32 32
#define HB_SERIAL_INT8NUM 33
#define HB_SERIAL_INT16NUM 34
#define HB_SERIAL_INT24NUM 35
#define HB_SERIAL_INT32NUM 36
#define HB_SERIAL_INT64NUM 37
#define HB_SERIAL_DBLNUM 38
#define HB_SERIAL_TIMESTAMP 39
#define HB_SERIAL_HASHFLAGS 40
#define HB_SERIAL_HASHDEFVAL 41
#define HB_SERIAL_ZCOMPRESS 42
/* xHarbour types */
#define HB_SERIAL_XHB_A 65
#define HB_SERIAL_XHB_B 66
#define HB_SERIAL_XHB_C 67
#define HB_SERIAL_XHB_D 68
#define HB_SERIAL_XHB_H 72
#define HB_SERIAL_XHB_L 76
#define HB_SERIAL_XHB_O 79
#define HB_SERIAL_XHB_Q 81
#define HB_SERIAL_XHB_R 82
#define HB_SERIAL_XHB_N 78
#define HB_SERIAL_XHB_T 84
#define HB_SERIAL_XHB_Z 90

#define HB_SERIAL_DUMMYOFFSET (static_cast<HB_SIZE>(-1))

#define HB_SERIAL_REFLSTINIT 16

struct HB_REF_ITEM
{
  void *value;
  HB_SIZE nOffset;
  int iRefs;
  int iType;
};

using PHB_REF_ITEM = HB_REF_ITEM *;

struct HB_REF_LIST
{
  HB_SIZE nSize;
  HB_SIZE nCount;
  PHB_REF_ITEM pRefs;
};

using PHB_REF_LIST = HB_REF_LIST *;

static HB_SIZE hb_deserializeItem(PHB_ITEM pItem, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut, const HB_UCHAR *pBuffer,
                                  HB_SIZE nOffset, PHB_REF_LIST pRefList);

#if 0
static void hb_itemSerialRefListShow(PHB_REF_LIST pRefList)
{
   printf("\n================================\n");
   printf("pRefList->nSize=%ld, pRefList->nCount=%ld\n", pRefList->nSize, pRefList->nCount);

   for( HB_SIZE nPos = 0; nPos < pRefList->nCount; ++nPos ) {
      printf("\t%ld] value=0x%p, nOffset=%ld, iRefs=%d, iType=%d\n", nPos,
             pRefList->pRefs[nPos].value,
             pRefList->pRefs[nPos].nOffset,
             pRefList->pRefs[nPos].iRefs,
             pRefList->pRefs[nPos].iType);
   }
   printf("================================\n");
   fflush(stdout);
}
#endif

static void hb_itemSerialRefListInit(PHB_REF_LIST pRefList)
{
  memset(pRefList, 0, sizeof(HB_REF_LIST));
}

static void hb_itemSerialRefListFree(PHB_REF_LIST pRefList)
{
  if (pRefList->nSize) {
    hb_xfree(pRefList->pRefs);
  }
}

static PHB_REF_ITEM hb_itemSerialValueFind(PHB_REF_LIST pRefList, void *value, HB_SIZE *pnPos)
{
  HB_SIZE nFirst = 0;
  HB_SIZE nLast = pRefList->nCount;
  HB_SIZE nMiddle = (nFirst + nLast) >> 1;

  while (nFirst < nLast) {
    if (reinterpret_cast<HB_PTRUINT>(pRefList->pRefs[nMiddle].value) < reinterpret_cast<HB_PTRUINT>(value)) {
      nFirst = nMiddle + 1;
    } else if (reinterpret_cast<HB_PTRUINT>(pRefList->pRefs[nMiddle].value) > reinterpret_cast<HB_PTRUINT>(value)) {
      nLast = nMiddle;
    } else {
      *pnPos = nMiddle;
      return &pRefList->pRefs[nMiddle];
    }
    nMiddle = (nFirst + nLast) >> 1;
  }

  *pnPos = nMiddle;

  return nullptr;
}

static PHB_REF_ITEM hb_itemSerialOffsetFind(PHB_REF_LIST pRefList, HB_SIZE nOffset, int iType, HB_SIZE *pnPos)
{
  HB_SIZE nFirst = 0;
  HB_SIZE nLast = pRefList->nCount;
  HB_SIZE nMiddle = (nFirst + nLast) >> 1;

  while (nFirst < nLast) {
    if (pRefList->pRefs[nMiddle].nOffset < nOffset) {
      nFirst = nMiddle + 1;
    } else if (pRefList->pRefs[nMiddle].nOffset > nOffset) {
      nLast = nMiddle;
    } else if (pRefList->pRefs[nMiddle].iType < iType) {
      nFirst = nMiddle + 1;
    } else if (pRefList->pRefs[nMiddle].iType > iType) {
      nLast = nMiddle;
    } else {
      *pnPos = nMiddle;
      return &pRefList->pRefs[nMiddle];
    }
    nMiddle = (nFirst + nLast) >> 1;
  }

  *pnPos = nMiddle;

  return nullptr;
}

static PHB_REF_ITEM hb_itemSerialRefNew(PHB_REF_LIST pRefList, HB_SIZE nPos)
{
  if (pRefList->nCount >= pRefList->nSize) {
    if (pRefList->nSize == 0) {
      pRefList->nSize = HB_SERIAL_REFLSTINIT;
    } else {
      pRefList->nSize += pRefList->nSize >> 1;
    }
    pRefList->pRefs = static_cast<PHB_REF_ITEM>(hb_xrealloc(pRefList->pRefs, pRefList->nSize * sizeof(HB_REF_ITEM)));
  }

  HB_SIZE nMove = pRefList->nCount - nPos;
  PHB_REF_ITEM pRef = &pRefList->pRefs[pRefList->nCount++];
  while (nMove-- > 0) {
    *pRef = *(pRef - 1);
    pRef--;
  }

  return pRef;
}

/* used by hb_itemSerialSize() for Harbour::Item::ARRAY and Harbour::Item::HASH */
static bool hb_itemSerialValueRef(PHB_REF_LIST pRefList, void *value, HB_SIZE nOffset)
{
  PHB_REF_ITEM pRef;
  HB_SIZE nPos;

  if ((pRef = hb_itemSerialValueFind(pRefList, value, &nPos)) != nullptr) {
    pRef->iRefs = 1;
    return true;
  }

  pRef = hb_itemSerialRefNew(pRefList, nPos);

  pRef->value = value;
  pRef->nOffset = nOffset;
  pRef->iRefs = 0;
  pRef->iType = 0;

  return false;
}

/* used between hb_itemSerialSize() and hb_serializeItem() */
static void hb_itemSerialUnusedFree(PHB_REF_LIST pRefList)
{
  if (pRefList->nSize) {
    HB_SIZE nCount;

    for (HB_SIZE nPos = nCount = 0; nPos < pRefList->nCount; ++nPos) {
      if (pRefList->pRefs[nPos].iRefs != 0) {
        if (nCount != nPos) {
          memcpy(&pRefList->pRefs[nCount], &pRefList->pRefs[nPos], sizeof(HB_REF_ITEM));
        }
        ++nCount;
      }
    }
    pRefList->nSize = pRefList->nCount = nCount;
    pRefList->pRefs = static_cast<PHB_REF_ITEM>(hb_xrealloc(pRefList->pRefs, nCount * sizeof(HB_REF_ITEM)));
  }
}

/* used by hb_serializeItem() for Harbour::Item::ARRAY and Harbour::Item::HASH */
static bool hb_itemSerialValueOffset(PHB_REF_LIST pRefList, void *value, HB_SIZE nOffset, HB_SIZE *pnRef)
{
  PHB_REF_ITEM pRef;
  HB_SIZE nPos;

  if ((pRef = hb_itemSerialValueFind(pRefList, value, &nPos)) != nullptr) {
    *pnRef = pRef->nOffset;
    return pRef->nOffset < nOffset;
  }

  *pnRef = HB_SERIAL_DUMMYOFFSET;
  return false;
}

/* used by hb_deserializeTest()
   for HB_SERIAL_ARRAYREF*, HB_SERIAL_HASHREF*, HB_SERIAL_REF */
static bool hb_itemSerialOffsetRef(PHB_REF_LIST pRefList, HB_SIZE nOffset)
{
  HB_SIZE nPos;

  if (hb_itemSerialOffsetFind(pRefList, nOffset, 0, &nPos) != nullptr) {
    return true;
  }

  PHB_REF_ITEM pRef = hb_itemSerialRefNew(pRefList, nPos);

  pRef->value = nullptr;
  pRef->nOffset = nOffset;
  pRef->iRefs = 0;
  pRef->iType = 0;

  return false;
}

/* used by hb_deserializeTest() for HB_SERIAL_XHB_R */
static void hb_itemSerialTypedRef(PHB_REF_LIST pRefList, int iType, HB_SIZE nIndex)
{
  HB_SIZE nPos;

  if (hb_itemSerialOffsetFind(pRefList, nIndex, iType, &nPos) == nullptr) {
    PHB_REF_ITEM pRef = hb_itemSerialRefNew(pRefList, nPos);

    pRef->value = nullptr;
    pRef->nOffset = nIndex;
    pRef->iRefs = 0;
    pRef->iType = iType;
  }
}

/* used by hb_deserializeItem()
   for HB_SERIAL_ARRAYREF* and HB_SERIAL_HASHREF* */
static void hb_itemSerialOffsetSet(PHB_REF_LIST pRefList, PHB_ITEM pItem, HB_SIZE nOffset)
{
  PHB_REF_ITEM pRef;
  HB_SIZE nPos;

  if ((pRef = hb_itemSerialOffsetFind(pRefList, nOffset, 0, &nPos)) != nullptr) {
    pRef->value = static_cast<void *>(pItem);
  }
}

/* used by hb_deserializeItem() for HB_SERIAL_REF */
static void hb_itemSerialOffsetGet(PHB_REF_LIST pRefList, PHB_ITEM pItem, HB_SIZE nOffset)
{
  PHB_REF_ITEM pRef;
  HB_SIZE nPos;

  if ((pRef = hb_itemSerialOffsetFind(pRefList, nOffset, 0, &nPos)) != nullptr) {
    hb_itemCopy(pItem, static_cast<PHB_ITEM>(pRef->value));
  }
}

/* used by hb_deserializeItem() for
   HB_SERIAL_XHB_A, HB_SERIAL_XHB_H, HB_SERIAL_XHB_Q, HB_SERIAL_XHB_O */
static void hb_itemSerialTypedSet(PHB_REF_LIST pRefList, PHB_ITEM pItem, int iType)
{
  HB_SIZE nPos = pRefList->nCount;

  while (nPos--) {
    PHB_REF_ITEM pRef = &pRefList->pRefs[nPos];

    if (pRef->iType == iType && pRef->value == nullptr) {
      if (static_cast<HB_SIZE>(++pRef->iRefs) == pRef->nOffset) {
        pRef->value = static_cast<void *>(pItem);
      }
    }
  }
}

/* used by hb_deserializeItem() for HB_SERIAL_XHB_R */
static void hb_itemSerialTypedGet(PHB_REF_LIST pRefList, PHB_ITEM pItem, int iType, HB_SIZE nIndex)
{
  PHB_REF_ITEM pRef;
  HB_SIZE nPos;

  if ((pRef = hb_itemSerialOffsetFind(pRefList, nIndex, iType, &nPos)) != nullptr) {
    if (pRef->value) {
      hb_itemCopy(pItem, static_cast<PHB_ITEM>(pRef->value));
    }
  }
}

static HB_SIZE hb_itemSerialSize(PHB_ITEM pItem, int iFlags, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                                 PHB_REF_LIST pRefList, HB_SIZE nOffset)
{
  HB_SIZE nSize, nLen, u;
  HB_MAXINT lVal;
  HB_USHORT uiClass;
  const char *szVal;

  if (pItem->isByRef()) {
    pItem = hb_itemUnRef(pItem);
  }

  switch (hb_itemType(pItem)) {
  case Harbour::Item::NIL:
  case Harbour::Item::LOGICAL:
    nSize = 1;
    break;

  case Harbour::Item::DATE:
    nSize = 4;
    break;

  case Harbour::Item::TIMESTAMP:
    nSize = 9;
    break;

  case Harbour::Item::INTEGER:
  case Harbour::Item::LONG:
    lVal = pItem->getNInt();
    if (lVal == 0) {
      nSize = (iFlags & HB_SERIALIZE_NUMSIZE) ? 2 : 1;
    } else if (HB_LIM_INT8(lVal)) {
      nSize = 2;
    } else if (HB_LIM_INT16(lVal)) {
      nSize = 3;
    } else if (HB_LIM_INT24(lVal)) {
      nSize = 4;
    } else if (HB_LIM_INT32(lVal)) {
      nSize = 5;
    } else {
      nSize = 9;
    }
    if (iFlags & HB_SERIALIZE_NUMSIZE) {
      nSize++;
    }
    break;

  case Harbour::Item::DOUBLE:
    if (iFlags & HB_SERIALIZE_NUMSIZE) {
      nSize = 11;
    } else {
      nSize = (pItem->getND() == 0.0) ? 1 : 9;
    }
    break;

  case Harbour::Item::SYMBOL:
    nSize = 2 + strlen(pItem->getSymbol()->szName);
    break;

  case Harbour::Item::STRING:
  case Harbour::Item::MEMO:
    szVal = pItem->getCPtr();
    nLen = pItem->getCLen();
    if (nLen == 0) {
      nSize = 1;
    } else {
      u = nLen;
      while (u && szVal[u - 1] == ' ') {
        --u;
      }
      u = nLen - u;
      nLen = hb_cdpnDupLen(szVal, nLen, cdpIn, cdpOut);
      if (nLen <= 255) {
        nSize = u > 1 ? nLen - u + 3 : nLen + 2;
      } else if (nLen <= UINT16_MAX) {
        nSize = u > 2 ? nLen - u + 5 : nLen + 3;
      } else {
        nSize = u > 4 ? nLen - u + 9 : nLen + 5;
      }
    }
    break;

  case Harbour::Item::ARRAY:
    nSize = 0;
    uiClass = hb_objGetClass(pItem);
    if (uiClass) {
      const char *szClass = hb_clsName(uiClass), *szFunc = hb_clsFuncName(uiClass);
      if (szClass != nullptr && szFunc != nullptr) {
        nSize += strlen(szClass) + strlen(szFunc) + 3;
      }
    }
    if ((iFlags & HB_SERIALIZE_IGNOREREF) == 0 && hb_arrayRefs(pItem) > 1 &&
        hb_itemSerialValueRef(pRefList, hb_arrayId(pItem), nOffset + nSize)) {
      nSize = 5;
    } else {
      nLen = hb_arrayLen(pItem);
      if (nLen <= 255) {
        nSize += 2;
      } else if (nLen <= UINT16_MAX) {
        nSize += 3;
      } else {
        nSize += 5;
      }
      for (u = 1; u <= nLen; u++) {
        nSize += hb_itemSerialSize(hb_arrayGetItemPtr(pItem, u), iFlags, cdpIn, cdpOut, pRefList, nOffset + nSize);
      }
    }
    break;

  case Harbour::Item::HASH:
    if ((iFlags & HB_SERIALIZE_IGNOREREF) == 0 && hb_hashRefs(pItem) > 1 &&
        hb_itemSerialValueRef(pRefList, hb_hashId(pItem), nOffset)) {
      nSize = 5;
    } else {
      PHB_ITEM pDefVal;

      if ((hb_hashGetFlags(pItem) & ~HB_HASH_RESORT) != HB_HASH_FLAG_DEFAULT) {
        nSize = 3;
      } else {
        nSize = 0;
      }
      pDefVal = hb_hashGetDefault(pItem);
      if (pDefVal) {
        nSize++;
        nSize += hb_itemSerialSize(pDefVal, iFlags, cdpIn, cdpOut, pRefList, nOffset + nSize);
      }
      nLen = hb_hashLen(pItem);
      if (nLen <= 255) {
        nSize += 2;
      } else if (nLen <= UINT16_MAX) {
        nSize += 3;
      } else {
        nSize += 5;
      }
      for (u = 1; u <= nLen; u++) {
        nSize += hb_itemSerialSize(hb_hashGetKeyAt(pItem, u), iFlags, cdpIn, cdpOut, pRefList, nOffset + nSize);
        nSize += hb_itemSerialSize(hb_hashGetValueAt(pItem, u), iFlags, cdpIn, cdpOut, pRefList, nOffset + nSize);
      }
    }
    break;

  default:
    /* map to NIL */
    nSize = 1;
  }

  return nSize;
}

static HB_SIZE hb_serializeItem(PHB_ITEM pItem, HB_BOOL iFlags, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut,
                                HB_UCHAR *pBuffer, HB_SIZE nOffset, PHB_REF_LIST pRefList)
{
  HB_MAXINT lVal;
  double d;
  int iWidth, iDecimal;
  long l, l2;
  const char *szVal;
  HB_SIZE nRef, nLen, n;

  if (pItem->isByRef()) {
    pItem = hb_itemUnRef(pItem);
  }

  switch (hb_itemType(pItem)) {
  case Harbour::Item::NIL:
    pBuffer[nOffset++] = HB_SERIAL_NIL;
    break;

  case Harbour::Item::LOGICAL:
    pBuffer[nOffset++] = pItem->getL() ? HB_SERIAL_TRUE : HB_SERIAL_FALSE;
    break;

  case Harbour::Item::DATE:
    pBuffer[nOffset++] = HB_SERIAL_DATE;
    l = pItem->getDL();
    HB_PUT_LE_UINT24(&pBuffer[nOffset], l);
    nOffset += 3;
    break;

  case Harbour::Item::TIMESTAMP:
    pBuffer[nOffset++] = HB_SERIAL_TIMESTAMP;
    pItem->getTDT(&l, &l2);
    HB_PUT_LE_UINT32(&pBuffer[nOffset], l);
    nOffset += 4;
    HB_PUT_LE_UINT32(&pBuffer[nOffset], l2);
    nOffset += 4;
    break;

  case Harbour::Item::INTEGER:
  case Harbour::Item::LONG:
    lVal = pItem->getNInt();
    if (iFlags & HB_SERIALIZE_NUMSIZE) {
      hb_itemGetNLen(pItem, &iWidth, nullptr);
      if (HB_LIM_INT8(lVal)) {
        pBuffer[nOffset++] = HB_SERIAL_INT8NUM;
        pBuffer[nOffset++] = static_cast<HB_UCHAR>(lVal);
      } else if (HB_LIM_INT16(lVal)) {
        pBuffer[nOffset++] = HB_SERIAL_INT16NUM;
        HB_PUT_LE_UINT16(&pBuffer[nOffset], lVal);
        nOffset += 2;
      } else if (HB_LIM_INT24(lVal)) {
        pBuffer[nOffset++] = HB_SERIAL_INT24NUM;
        HB_PUT_LE_UINT24(&pBuffer[nOffset], lVal);
        nOffset += 3;
      } else if (HB_LIM_INT32(lVal)) {
        pBuffer[nOffset++] = HB_SERIAL_INT32NUM;
        HB_PUT_LE_UINT32(&pBuffer[nOffset], lVal);
        nOffset += 4;
      } else {
        pBuffer[nOffset++] = HB_SERIAL_INT64NUM;
        HB_PUT_LE_UINT64(&pBuffer[nOffset], lVal);
        nOffset += 8;
      }
      pBuffer[nOffset++] = static_cast<HB_UCHAR>(iWidth);
    } else if (lVal == 0) {
      pBuffer[nOffset++] = HB_SERIAL_ZERO;
    } else if (HB_LIM_INT8(lVal)) {
      pBuffer[nOffset++] = HB_SERIAL_INT8;
      pBuffer[nOffset++] = static_cast<HB_UCHAR>(lVal);
    } else if (HB_LIM_INT16(lVal)) {
      pBuffer[nOffset++] = HB_SERIAL_INT16;
      HB_PUT_LE_UINT16(&pBuffer[nOffset], lVal);
      nOffset += 2;
    } else if (HB_LIM_INT24(lVal)) {
      pBuffer[nOffset++] = HB_SERIAL_INT24;
      HB_PUT_LE_UINT24(&pBuffer[nOffset], lVal);
      nOffset += 3;
    } else if (HB_LIM_INT32(lVal)) {
      pBuffer[nOffset++] = HB_SERIAL_INT32;
      HB_PUT_LE_UINT32(&pBuffer[nOffset], lVal);
      nOffset += 4;
    } else {
      pBuffer[nOffset++] = HB_SERIAL_INT64;
      HB_PUT_LE_UINT64(&pBuffer[nOffset], lVal);
      nOffset += 8;
    }
    break;

  case Harbour::Item::DOUBLE:
    d = pItem->getND();
    if (iFlags & HB_SERIALIZE_NUMSIZE) {
      hb_itemGetNLen(pItem, &iWidth, &iDecimal);
      pBuffer[nOffset++] = HB_SERIAL_DBLNUM;
      HB_PUT_LE_DOUBLE(&pBuffer[nOffset], d);
      nOffset += 8;
      pBuffer[nOffset++] = static_cast<HB_UCHAR>(iWidth);
      pBuffer[nOffset++] = static_cast<HB_UCHAR>(iDecimal);
    } else if (d == 0.0) {
      pBuffer[nOffset++] = HB_SERIAL_ZERO;
    } else {
      pBuffer[nOffset++] = HB_SERIAL_DOUBLE;
      HB_PUT_LE_DOUBLE(&pBuffer[nOffset], d);
      nOffset += 8;
    }
    break;

  case Harbour::Item::SYMBOL:
    szVal = pItem->getSymbol()->szName;
    nLen = strlen(szVal);
    if (nLen > 0xFF) {
      nLen = 0xFF;
    }
    pBuffer[nOffset++] = HB_SERIAL_SYMBOL;
    pBuffer[nOffset++] = static_cast<HB_UCHAR>(nLen);
    memcpy(&pBuffer[nOffset], szVal, nLen);
    nOffset += nLen;
    break;

  case Harbour::Item::STRING:
  case Harbour::Item::MEMO:
    szVal = pItem->getCPtr();
    nLen = pItem->getCLen();
    if (nLen == 0) {
      pBuffer[nOffset++] = HB_SERIAL_STRNUL;
    } else {
      HB_SIZE nSize = n = nLen;
      while (n && szVal[n - 1] == ' ') {
        --n;
      }
      n = nLen - n;
      nLen = hb_cdpnDupLen(szVal, nLen, cdpIn, cdpOut);
      if (nLen <= 255) {
        if (n > 1) {
          nLen -= n;
          nSize -= n;
          pBuffer[nOffset++] = HB_SERIAL_STRPAD8;
          pBuffer[nOffset++] = static_cast<HB_UCHAR>(nLen);
          pBuffer[nOffset++] = static_cast<HB_UCHAR>(n);
        } else {
          pBuffer[nOffset++] = HB_SERIAL_STRING8;
          pBuffer[nOffset++] = static_cast<HB_UCHAR>(nLen);
        }
      } else if (nLen <= UINT16_MAX) {
        if (n > 2) {
          nLen -= n;
          nSize -= n;
          pBuffer[nOffset++] = HB_SERIAL_STRPAD16;
          HB_PUT_LE_UINT16(&pBuffer[nOffset], nLen);
          nOffset += 2;
          HB_PUT_LE_UINT16(&pBuffer[nOffset], n);
          nOffset += 2;
        } else {
          pBuffer[nOffset++] = HB_SERIAL_STRING16;
          HB_PUT_LE_UINT16(&pBuffer[nOffset], nLen);
          nOffset += 2;
        }
      } else {
        if (n > 4) {
          nLen -= n;
          nSize -= n;
          pBuffer[nOffset++] = HB_SERIAL_STRPAD32;
          HB_PUT_LE_UINT32(&pBuffer[nOffset], nLen);
          nOffset += 4;
          HB_PUT_LE_UINT32(&pBuffer[nOffset], n);
          nOffset += 4;
        } else {
          pBuffer[nOffset++] = HB_SERIAL_STRING32;
          HB_PUT_LE_UINT32(&pBuffer[nOffset], nLen);
          nOffset += 4;
        }
      }
      n = nLen;
      hb_cdpnDup2(szVal, nSize, reinterpret_cast<char *>(&pBuffer[nOffset]), &n, cdpIn, cdpOut);
      nOffset += nLen;
    }
    break;

  case Harbour::Item::ARRAY:
    nRef = HB_SERIAL_DUMMYOFFSET;
    if (hb_arrayRefs(pItem) > 1 && hb_itemSerialValueOffset(pRefList, hb_arrayId(pItem), nOffset, &nRef)) {
      pBuffer[nOffset++] = HB_SERIAL_REF;
      HB_PUT_LE_UINT32(&pBuffer[nOffset], nRef);
      nOffset += 4;
    } else {
      HB_USHORT uiClass = hb_objGetClass(pItem);
      if (uiClass) {
        const char *szClass = hb_clsName(uiClass), *szFunc = hb_clsFuncName(uiClass);
        if (szClass != nullptr && szFunc != nullptr) {
          pBuffer[nOffset++] = HB_SERIAL_OBJ;
          nLen = strlen(szClass) + 1;
          memcpy(&pBuffer[nOffset], szClass, nLen);
          nOffset += nLen;
          nLen = strlen(szFunc) + 1;
          memcpy(&pBuffer[nOffset], szFunc, nLen);
          nOffset += nLen;
        }
      }
      nLen = hb_arrayLen(pItem);
      if (nLen <= 255) {
        pBuffer[nOffset++] = nRef == HB_SERIAL_DUMMYOFFSET ? HB_SERIAL_ARRAY8 : HB_SERIAL_ARRAYREF8;
        pBuffer[nOffset++] = static_cast<HB_UCHAR>(nLen);
      } else if (nLen <= UINT16_MAX) {
        pBuffer[nOffset++] = nRef == HB_SERIAL_DUMMYOFFSET ? HB_SERIAL_ARRAY16 : HB_SERIAL_ARRAYREF16;
        HB_PUT_LE_UINT16(&pBuffer[nOffset], nLen);
        nOffset += 2;
      } else {
        pBuffer[nOffset++] = nRef == HB_SERIAL_DUMMYOFFSET ? HB_SERIAL_ARRAY32 : HB_SERIAL_ARRAYREF32;
        HB_PUT_LE_UINT32(&pBuffer[nOffset], nLen);
        nOffset += 4;
      }
      for (n = 1; n <= nLen; n++) {
        nOffset = hb_serializeItem(hb_arrayGetItemPtr(pItem, n), iFlags, cdpIn, cdpOut, pBuffer, nOffset, pRefList);
      }
    }
    break;

  case Harbour::Item::HASH:
    nRef = HB_SERIAL_DUMMYOFFSET;
    if (hb_hashRefs(pItem) > 1 && hb_itemSerialValueOffset(pRefList, hb_hashId(pItem), nOffset, &nRef)) {
      pBuffer[nOffset++] = HB_SERIAL_REF;
      HB_PUT_LE_UINT32(&pBuffer[nOffset], nRef);
      nOffset += 4;
    } else {
      int iHashFlags = hb_hashGetFlags(pItem);
      auto pDefVal = hb_hashGetDefault(pItem);

      if ((iHashFlags & ~HB_HASH_RESORT) != HB_HASH_FLAG_DEFAULT) {
        pBuffer[nOffset++] = HB_SERIAL_HASHFLAGS;
        HB_PUT_LE_UINT16(&pBuffer[nOffset], iHashFlags);
        nOffset += 2;
      }
      if (pDefVal) {
        pBuffer[nOffset++] = HB_SERIAL_HASHDEFVAL;
        nOffset = hb_serializeItem(pDefVal, iFlags, cdpIn, cdpOut, pBuffer, nOffset, pRefList);
      }
      nLen = hb_hashLen(pItem);
      if (nLen <= 255) {
        pBuffer[nOffset++] = nRef == HB_SERIAL_DUMMYOFFSET ? HB_SERIAL_HASH8 : HB_SERIAL_HASHREF8;
        pBuffer[nOffset++] = static_cast<HB_UCHAR>(nLen);
      } else if (nLen <= UINT16_MAX) {
        pBuffer[nOffset++] = nRef == HB_SERIAL_DUMMYOFFSET ? HB_SERIAL_HASH16 : HB_SERIAL_HASHREF16;
        HB_PUT_LE_UINT16(&pBuffer[nOffset], nLen);
        nOffset += 2;
      } else {
        pBuffer[nOffset++] = nRef == HB_SERIAL_DUMMYOFFSET ? HB_SERIAL_HASH32 : HB_SERIAL_HASHREF32;
        HB_PUT_LE_UINT32(&pBuffer[nOffset], nLen);
        nOffset += 4;
      }
      for (n = 1; n <= nLen; n++) {
        nOffset = hb_serializeItem(hb_hashGetKeyAt(pItem, n), iFlags, cdpIn, cdpOut, pBuffer, nOffset, pRefList);
        nOffset = hb_serializeItem(hb_hashGetValueAt(pItem, n), iFlags, cdpIn, cdpOut, pBuffer, nOffset, pRefList);
      }
    }
    break;

  default:
    /* map to NIL */
    pBuffer[nOffset++] = HB_SERIAL_NIL;
    break;
  }

  return nOffset;
}

static bool hb_deserializeTest(const HB_UCHAR **pBufferPtr, HB_SIZE *pnSize, HB_SIZE nOffset, PHB_REF_LIST pRefList)
{
  const HB_UCHAR *pBuffer = *pBufferPtr;
  HB_SIZE nSize = *pnSize, nLen = 0;

  if (nSize == 0) {
    return false;
  }

  switch (*pBuffer++) {
  case HB_SERIAL_NIL:
  case HB_SERIAL_TRUE:
  case HB_SERIAL_FALSE:
  case HB_SERIAL_ZERO:
  case HB_SERIAL_STRNUL:
    nSize = 1;
    break;
  case HB_SERIAL_INT8:
    nSize = 2;
    break;
  case HB_SERIAL_INT8NUM:
  case HB_SERIAL_INT16:
    nSize = 3;
    break;
  case HB_SERIAL_INT16NUM:
  case HB_SERIAL_INT24:
  case HB_SERIAL_DATE:
    nSize = 4;
    break;
  case HB_SERIAL_INT24NUM:
  case HB_SERIAL_INT32:
    nSize = 5;
    break;
  case HB_SERIAL_INT32NUM:
    nSize = 6;
    break;
  case HB_SERIAL_INT64:
  case HB_SERIAL_DOUBLE:
  case HB_SERIAL_TIMESTAMP:
    nSize = 9;
    break;
  case HB_SERIAL_INT64NUM:
    nSize = 10;
    break;
  case HB_SERIAL_DBLNUM:
    nSize = 11;
    break;
  case HB_SERIAL_SYMBOL:
  case HB_SERIAL_STRING8:
    nSize = 2 + (nSize >= 2 ? *pBuffer : nSize);
    break;
  case HB_SERIAL_STRING16:
    nSize = 3 + (nSize >= 3 ? HB_GET_LE_UINT16(pBuffer) : nSize);
    break;
  case HB_SERIAL_STRING32:
    nSize = 5 + (nSize >= 5 ? HB_GET_LE_UINT32(pBuffer) : nSize);
    break;
  case HB_SERIAL_STRPAD8:
    nSize = 3 + (nSize >= 3 ? *pBuffer : nSize);
    break;
  case HB_SERIAL_STRPAD16:
    nSize = 5 + (nSize >= 5 ? HB_GET_LE_UINT16(pBuffer) : nSize);
    break;
  case HB_SERIAL_STRPAD32:
    nSize = 9 + (nSize >= 9 ? HB_GET_LE_UINT32(pBuffer) : nSize);
    break;
  case HB_SERIAL_ARRAYREF8:
    if (hb_itemSerialOffsetRef(pRefList, nOffset)) {
      return false;
    }
    /* fallthrough */
  case HB_SERIAL_ARRAY8:
    if (nSize >= 2) {
      nSize = 2;
      nLen = *pBuffer;
    } else {
      nSize++;
    }
    break;
  case HB_SERIAL_ARRAYREF16:
    if (hb_itemSerialOffsetRef(pRefList, nOffset)) {
      return false;
    }
    /* fallthrough */
  case HB_SERIAL_ARRAY16:
    if (nSize >= 3) {
      nSize = 3;
      nLen = HB_GET_LE_UINT16(pBuffer);
    } else {
      nSize++;
    }
    break;
  case HB_SERIAL_ARRAYREF32:
    if (hb_itemSerialOffsetRef(pRefList, nOffset)) {
      return false;
    }
    /* fallthrough */
  case HB_SERIAL_ARRAY32:
    if (nSize >= 5) {
      nSize = 5;
      nLen = HB_GET_LE_UINT32(pBuffer);
    } else {
      nSize++;
    }
    break;
  case HB_SERIAL_HASHREF8:
    if (hb_itemSerialOffsetRef(pRefList, nOffset)) {
      return false;
    }
    /* fallthrough */
  case HB_SERIAL_HASH8:
    if (nSize >= 2) {
      nSize = 2;
      nLen = *pBuffer << 1;
    } else {
      nSize++;
    }
    break;
  case HB_SERIAL_HASHREF16:
    if (hb_itemSerialOffsetRef(pRefList, nOffset)) {
      return false;
    }
    /* fallthrough */
  case HB_SERIAL_HASH16:
    if (nSize >= 3) {
      nSize = 3;
      nLen = HB_GET_LE_UINT16(pBuffer) << 1;
    } else {
      nSize++;
    }
    break;
  case HB_SERIAL_HASHREF32:
    if (hb_itemSerialOffsetRef(pRefList, nOffset)) {
      return false;
    }
    /* fallthrough */
  case HB_SERIAL_HASH32:
    if (nSize >= 5) {
      nSize = 5;
      nLen = HB_GET_LE_UINT32(pBuffer) << 1;
    } else {
      nSize++;
    }
    break;
  case HB_SERIAL_REF:
    if (!hb_itemSerialOffsetRef(pRefList, HB_GET_LE_UINT32(pBuffer))) {
      return false;
    }
    nSize = 5;
    break;
  case HB_SERIAL_OBJ:
    nLen = hb_strnlen(reinterpret_cast<const char *>(pBuffer), nSize - 1) + 1;
    if (nLen >= nSize) {
      nSize++;
    } else {
      nLen += hb_strnlen(reinterpret_cast<const char *>(pBuffer) + nLen, nSize - nLen - 1) + 2;
      if (nLen >= nSize) {
        nSize++;
      } else {
        nSize = nLen;
      }
    }
    nLen = 1;
    break;
  case HB_SERIAL_HASHFLAGS:
    nSize = 3;
    nLen = 1;
    break;
  case HB_SERIAL_HASHDEFVAL:
    nSize = 1;
    nLen = 2;
    break;
  case HB_SERIAL_ZCOMPRESS:
    nSize = 9 + (nSize >= 9 ? HB_GET_LE_UINT32(pBuffer) : nSize);
    break;

  /* xHarbour types */
  case HB_SERIAL_XHB_C:
    nSize = 9 + (nSize >= 9 ? static_cast<HB_SIZE>(HB_GET_BE_UINT64(pBuffer)) : nSize);
    break;
  case HB_SERIAL_XHB_L:
    nSize = 2;
    break;
  case HB_SERIAL_XHB_N:
    if (nSize >= 2 && *pBuffer == 'X') {
      /* this is workaround for bug in xHarbour serialization code */
      nSize = 20;
    } else {
      nSize = 10;
    }
    break;
  case HB_SERIAL_XHB_D:
  case HB_SERIAL_XHB_T:
    nSize = 9;
    break;
  case HB_SERIAL_XHB_Z:
    nSize = 1;
    break;
  case HB_SERIAL_XHB_A:
    if (nSize >= 9) {
      nSize = 9;
      nLen = static_cast<HB_SIZE>(HB_GET_BE_UINT64(pBuffer));
    } else {
      nSize++;
    }
    break;
  case HB_SERIAL_XHB_B:
    nSize = 1;
    nLen = 1;
    break;
  case HB_SERIAL_XHB_H:
    if (nSize >= 9) {
      nSize = 9;
      nLen = static_cast<HB_SIZE>(HB_GET_BE_UINT64(pBuffer)) << 1;
    } else {
      nSize++;
    }
    break;
  case HB_SERIAL_XHB_O:
    if (nSize >= 9) {
      nSize = 9;
      nLen = (static_cast<HB_SIZE>(HB_GET_BE_UINT64(pBuffer)) << 1) + 1;
    } else {
      nSize++;
    }
    break;
  case HB_SERIAL_XHB_Q:
    if (nSize >= 18 && pBuffer[8] == HB_SERIAL_XHB_C) {
      auto nData = static_cast<HB_SIZE>(HB_GET_BE_UINT64(pBuffer));
      if (nData >= 9 && nData - 9 >= static_cast<HB_SIZE>(HB_GET_BE_UINT64(&pBuffer[9]))) {
        nSize = 9 + nData;
      } else {
        nSize++;
      }
    } else {
      nSize++;
    }
    nSize = 9 + (nSize >= 9 ? static_cast<HB_SIZE>(HB_GET_BE_UINT64(pBuffer)) : nSize);
    break;
  case HB_SERIAL_XHB_R:
    if (nSize++ >= 10) {
      switch (pBuffer[0]) {
      case HB_SERIAL_XHB_A:
      case HB_SERIAL_XHB_H:
      case HB_SERIAL_XHB_O:
        hb_itemSerialTypedRef(pRefList, pBuffer[0], static_cast<HB_SIZE>(HB_GET_BE_UINT64(&pBuffer[1])));
        /* fallthrough */
      case HB_SERIAL_XHB_B:
        /* we do not support xHarbour codeblock deserialization: HB_RestoreBlock(pItem) */
        nSize = 10;
        break;
      }
    }
    break;

  default:
    nSize = 1;
    break;
  }

  if (nSize > *pnSize) {
    return false;
  }

  *pnSize -= nSize;
  *pBufferPtr += nSize;

  while (nLen) {
    nOffset += nSize;
    nSize = *pnSize;
    if (!hb_deserializeTest(pBufferPtr, pnSize, nOffset, pRefList)) {
      return false;
    }
    nSize -= *pnSize;
    --nLen;
  }

  return true;
}

static HB_SIZE hb_deserializeHash(PHB_ITEM pItem, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut, const HB_UCHAR *pBuffer,
                                  HB_SIZE nOffset, HB_SIZE nLen, PHB_REF_LIST pRefList)
{
  hb_hashNew(pItem);

  if (nLen) {
#if 0
      auto pKey = hb_itemNew(nullptr);
      auto pVal = hb_itemNew(nullptr);

      hb_hashPreallocate(pItem, nLen);
      while( nLen-- ) {
         nOffset = hb_deserializeItem(pKey, cdpIn, cdpOut, pBuffer, nOffset, pRefList);
         nOffset = hb_deserializeItem(pVal, cdpIn, cdpOut, pBuffer, nOffset, pRefList);
         hb_hashAdd(pItem, pKey, pVal);
      }
      hb_itemRelease(pKey);
      hb_itemRelease(pVal);
#else
    PHB_ITEM pKey, pVal;

    hb_hashSetFlags(pItem, HB_HASH_BINARY | HB_HASH_RESORT);
    hb_hashPreallocate(pItem, nLen);
    while (nLen--) {
      if (hb_hashAllocNewPair(pItem, &pKey, &pVal)) {
        nOffset = hb_deserializeItem(pKey, cdpIn, cdpOut, pBuffer, nOffset, pRefList);
        nOffset = hb_deserializeItem(pVal, cdpIn, cdpOut, pBuffer, nOffset, pRefList);
      }
    }
#endif
  }

  return nOffset;
}

static HB_SIZE hb_deserializeArray(PHB_ITEM pItem, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut, const HB_UCHAR *pBuffer,
                                   HB_SIZE nOffset, HB_SIZE nLen, PHB_REF_LIST pRefList)
{
  hb_arrayNew(pItem, nLen);

  for (HB_SIZE u = 1; u <= nLen; u++) {
    nOffset = hb_deserializeItem(hb_arrayGetItemPtr(pItem, u), cdpIn, cdpOut, pBuffer, nOffset, pRefList);
  }

  return nOffset;
}

static HB_SIZE hb_deserializeItem(PHB_ITEM pItem, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut, const HB_UCHAR *pBuffer,
                                  HB_SIZE nOffset, PHB_REF_LIST pRefList)
{
  HB_SIZE nLen, nPad, nSize;
  char *szVal;

  switch (pBuffer[nOffset++]) {
  case HB_SERIAL_NIL:
    hb_itemClear(pItem);
    break;

  case HB_SERIAL_TRUE:
    hb_itemPutL(pItem, true);
    break;

  case HB_SERIAL_FALSE:
    hb_itemPutL(pItem, false);
    break;

  case HB_SERIAL_ZERO:
    hb_itemPutNI(pItem, 0);
    break;

  case HB_SERIAL_INT8:
    hb_itemPutNI(pItem, static_cast<signed char>(pBuffer[nOffset++]));
    break;

  case HB_SERIAL_INT16:
    hb_itemPutNI(pItem, HB_GET_LE_INT16(&pBuffer[nOffset]));
    nOffset += 2;
    break;

  case HB_SERIAL_INT24:
    hb_itemPutNInt(pItem, HB_GET_LE_INT24(&pBuffer[nOffset]));
    nOffset += 3;
    break;

  case HB_SERIAL_INT32:
    hb_itemPutNInt(pItem, HB_GET_LE_INT32(&pBuffer[nOffset]));
    nOffset += 4;
    break;

  case HB_SERIAL_INT64:
    hb_itemPutNInt(pItem, HB_GET_LE_INT64(&pBuffer[nOffset]));
    nOffset += 8;
    break;

  case HB_SERIAL_INT8NUM:
    hb_itemPutNILen(pItem, static_cast<signed char>(pBuffer[nOffset]), pBuffer[nOffset + 1]);
    nOffset += 2;
    break;

  case HB_SERIAL_INT16NUM:
    hb_itemPutNILen(pItem, HB_GET_LE_INT16(&pBuffer[nOffset]), pBuffer[nOffset + 2]);
    nOffset += 3;
    break;

  case HB_SERIAL_INT24NUM:
    hb_itemPutNIntLen(pItem, HB_GET_LE_INT24(&pBuffer[nOffset]), pBuffer[nOffset + 3]);
    nOffset += 4;
    break;

  case HB_SERIAL_INT32NUM:
    hb_itemPutNIntLen(pItem, HB_GET_LE_INT32(&pBuffer[nOffset]), pBuffer[nOffset + 4]);
    nOffset += 5;
    break;

  case HB_SERIAL_INT64NUM:
    hb_itemPutNIntLen(pItem, HB_GET_LE_INT64(&pBuffer[nOffset]), pBuffer[nOffset + 8]);
    nOffset += 9;
    break;

  case HB_SERIAL_DOUBLE:
    hb_itemPutND(pItem, HB_GET_LE_DOUBLE(&pBuffer[nOffset]));
    nOffset += 8;
    break;

  case HB_SERIAL_DBLNUM:
    hb_itemPutNDLen(pItem, HB_GET_LE_DOUBLE(&pBuffer[nOffset]), pBuffer[nOffset + 8], pBuffer[nOffset + 9]);
    nOffset += 10;
    break;

  case HB_SERIAL_DATE:
    hb_itemPutDL(pItem, HB_GET_LE_UINT24(&pBuffer[nOffset]));
    nOffset += 3;
    break;

  case HB_SERIAL_TIMESTAMP:
    hb_itemPutTDT(pItem, HB_GET_LE_UINT32(&pBuffer[nOffset]), HB_GET_LE_UINT32(&pBuffer[nOffset + 4]));
    nOffset += 8;
    break;

  case HB_SERIAL_SYMBOL:
    nLen = pBuffer[nOffset++];
    szVal = hb_strndup(reinterpret_cast<const char *>(&pBuffer[nOffset]), nLen);
    hb_itemPutSymbol(pItem, hb_dynsymGetSymbol(szVal));
    hb_xfree(szVal);
    nOffset += nLen;
    break;

  case HB_SERIAL_STRNUL:
    hb_itemPutCL(pItem, nullptr, 0);
    break;
  case HB_SERIAL_STRING8:
    nSize = nLen = pBuffer[nOffset++];
    szVal = hb_cdpnDup(reinterpret_cast<const char *>(&pBuffer[nOffset]), &nLen, cdpIn, cdpOut);
    hb_itemPutCLPtr(pItem, szVal, nLen);
    nOffset += nSize;
    break;
  case HB_SERIAL_STRING16:
    nSize = nLen = HB_GET_LE_UINT16(&pBuffer[nOffset]);
    nOffset += 2;
    szVal = hb_cdpnDup(reinterpret_cast<const char *>(&pBuffer[nOffset]), &nLen, cdpIn, cdpOut);
    hb_itemPutCLPtr(pItem, szVal, nLen);
    nOffset += nSize;
    break;
  case HB_SERIAL_STRING32:
    nSize = nLen = HB_GET_LE_UINT32(&pBuffer[nOffset]);
    nOffset += 4;
    szVal = hb_cdpnDup(reinterpret_cast<const char *>(&pBuffer[nOffset]), &nLen, cdpIn, cdpOut);
    hb_itemPutCLPtr(pItem, szVal, nLen);
    nOffset += nSize;
    break;
  case HB_SERIAL_STRPAD8:
    nSize = pBuffer[nOffset++];
    nPad = pBuffer[nOffset++];
    nLen = hb_cdpnDupLen(reinterpret_cast<const char *>(&pBuffer[nOffset]), nSize, cdpIn, cdpOut);
    szVal = static_cast<char *>(hb_xgrab(nLen + nPad + 1));
    hb_cdpnDup2(reinterpret_cast<const char *>(&pBuffer[nOffset]), nSize, szVal, &nLen, cdpIn, cdpOut);
    memset(szVal + nLen, ' ', nPad);
    hb_itemPutCLPtr(pItem, szVal, nLen + nPad);
    nOffset += nSize;
    break;
  case HB_SERIAL_STRPAD16:
    nSize = HB_GET_LE_UINT16(&pBuffer[nOffset]);
    nOffset += 2;
    nPad = HB_GET_LE_UINT16(&pBuffer[nOffset]);
    nOffset += 2;
    nLen = hb_cdpnDupLen(reinterpret_cast<const char *>(&pBuffer[nOffset]), nSize, cdpIn, cdpOut);
    szVal = static_cast<char *>(hb_xgrab(nLen + nPad + 1));
    hb_cdpnDup2(reinterpret_cast<const char *>(&pBuffer[nOffset]), nSize, szVal, &nLen, cdpIn, cdpOut);
    memset(szVal + nLen, ' ', nPad);
    hb_itemPutCLPtr(pItem, szVal, nLen + nPad);
    nOffset += nSize;
    break;
  case HB_SERIAL_STRPAD32:
    nSize = HB_GET_LE_UINT32(&pBuffer[nOffset]);
    nOffset += 4;
    nPad = HB_GET_LE_UINT32(&pBuffer[nOffset]);
    nOffset += 4;
    nLen = hb_cdpnDupLen(reinterpret_cast<const char *>(&pBuffer[nOffset]), nSize, cdpIn, cdpOut);
    szVal = static_cast<char *>(hb_xgrab(nLen + nPad + 1));
    hb_cdpnDup2(reinterpret_cast<const char *>(&pBuffer[nOffset]), nSize, szVal, &nLen, cdpIn, cdpOut);
    hb_xmemset(szVal + nLen, ' ', nPad);
    hb_itemPutCLPtr(pItem, szVal, nLen + nPad);
    nOffset += nSize;
    break;

  case HB_SERIAL_ARRAYREF8:
    hb_itemSerialOffsetSet(pRefList, pItem, nOffset - 1);
    /* fallthrough */
  case HB_SERIAL_ARRAY8:
    nLen = pBuffer[nOffset++];
    nOffset = hb_deserializeArray(pItem, cdpIn, cdpOut, pBuffer, nOffset, nLen, pRefList);
    break;
  case HB_SERIAL_ARRAYREF16:
    hb_itemSerialOffsetSet(pRefList, pItem, nOffset - 1);
    /* fallthrough */
  case HB_SERIAL_ARRAY16:
    nLen = HB_GET_LE_UINT16(&pBuffer[nOffset]);
    nOffset = hb_deserializeArray(pItem, cdpIn, cdpOut, pBuffer, nOffset + 2, nLen, pRefList);
    break;
  case HB_SERIAL_ARRAYREF32:
    hb_itemSerialOffsetSet(pRefList, pItem, nOffset - 1);
    /* fallthrough */
  case HB_SERIAL_ARRAY32:
    nLen = HB_GET_LE_UINT32(&pBuffer[nOffset]);
    nOffset = hb_deserializeArray(pItem, cdpIn, cdpOut, pBuffer, nOffset + 4, nLen, pRefList);
    break;

  case HB_SERIAL_HASHREF8:
    hb_itemSerialOffsetSet(pRefList, pItem, nOffset - 1);
    /* fallthrough */
  case HB_SERIAL_HASH8:
    nLen = pBuffer[nOffset++];
    nOffset = hb_deserializeHash(pItem, cdpIn, cdpOut, pBuffer, nOffset, nLen, pRefList);
    break;
  case HB_SERIAL_HASHREF16:
    hb_itemSerialOffsetSet(pRefList, pItem, nOffset - 1);
    /* fallthrough */
  case HB_SERIAL_HASH16:
    nLen = HB_GET_LE_UINT16(&pBuffer[nOffset]);
    nOffset = hb_deserializeHash(pItem, cdpIn, cdpOut, pBuffer, nOffset + 2, nLen, pRefList);
    break;
  case HB_SERIAL_HASHREF32:
    hb_itemSerialOffsetSet(pRefList, pItem, nOffset - 1);
    /* fallthrough */
  case HB_SERIAL_HASH32:
    nLen = HB_GET_LE_UINT32(&pBuffer[nOffset]);
    nOffset = hb_deserializeHash(pItem, cdpIn, cdpOut, pBuffer, nOffset + 4, nLen, pRefList);
    break;

  case HB_SERIAL_REF:
    hb_itemSerialOffsetGet(pRefList, pItem, HB_GET_LE_UINT32(&pBuffer[nOffset]));
    nOffset += 4;
    break;

  case HB_SERIAL_OBJ: {
    const char *szFunc;
    auto szClass = reinterpret_cast<const char *>(&pBuffer[nOffset]);
    nLen = strlen(szClass);
    szFunc = szClass + nLen + 1;
    nOffset = hb_deserializeItem(pItem, cdpIn, cdpOut, pBuffer, nOffset + nLen + strlen(szFunc) + 2, pRefList);
    hb_objSetClass(pItem, szClass, szFunc);
    break;
  }

  case HB_SERIAL_HASHFLAGS: {
    int iHashFlags = HB_GET_LE_UINT16(&pBuffer[nOffset]);
    nOffset = hb_deserializeItem(pItem, cdpIn, cdpOut, pBuffer, nOffset + 2, pRefList);
    hb_hashClearFlags(pItem, HB_HASH_FLAG_MASK);
    if ((iHashFlags & (HB_HASH_KEEPORDER | HB_HASH_BINARY)) != HB_HASH_BINARY) {
      iHashFlags |= HB_HASH_RESORT;
    }
    hb_hashSetFlags(pItem, iHashFlags);
    break;
  }

  case HB_SERIAL_HASHDEFVAL: {
    auto pDefVal = hb_itemNew(nullptr);
    nOffset = hb_deserializeItem(pDefVal, cdpIn, cdpOut, pBuffer, nOffset, pRefList);
    nOffset = hb_deserializeItem(pItem, cdpIn, cdpOut, pBuffer, nOffset, pRefList);
    hb_hashSetDefault(pItem, pDefVal);
    hb_itemRelease(pDefVal);
    break;
  }

  case HB_SERIAL_ZCOMPRESS:
    nSize = HB_GET_LE_UINT32(&pBuffer[nOffset]);
    nOffset += 4;
    nLen = HB_GET_LE_UINT32(&pBuffer[nOffset]);
    nOffset += 4;
    szVal = static_cast<char *>(hb_xgrab(nLen + 1));
    switch (hb_zlibUncompress(szVal, &nLen, reinterpret_cast<const char *>(&pBuffer[nOffset]), nSize)) {
    case HB_ZLIB_RES_OK: {
      HB_REF_LIST refListZ;

      hb_itemSerialRefListInit(&refListZ);
      pBuffer = reinterpret_cast<const HB_UCHAR *>(szVal);
      if (hb_deserializeTest(&pBuffer, &nLen, 0, &refListZ)) {
        hb_deserializeItem(pItem, cdpIn, cdpOut, reinterpret_cast<const HB_UCHAR *>(szVal), 0, &refListZ);
      } else {
        hb_itemClear(pItem);
      }
      hb_itemSerialRefListFree(&refListZ);
      break;
    }
    case HB_ZLIB_RES_UNSUPPORTED:
      if (hb_vmRequestQuery() == 0) {
        hb_itemPutCLPtr(pItem, szVal, nLen);
        hb_errRT_BASE_Ext1(EG_ARG, 3016, nullptr, HB_ERR_FUNCNAME, 0, EF_CANDEFAULT, 1, pItem);
        szVal = nullptr;
      }
      /* fallthrough */

    default:
      hb_itemClear(pItem);
    }

    if (szVal != nullptr) {
      hb_xfree(szVal);
    }
    nOffset += nSize;
    break;

  /* xHarbour types */
  case HB_SERIAL_XHB_C:
    nSize = nLen = static_cast<HB_SIZE>(HB_GET_BE_UINT64(&pBuffer[nOffset]));
    nOffset += 8;
    szVal = hb_cdpnDup(reinterpret_cast<const char *>(&pBuffer[nOffset]), &nLen, cdpIn, cdpOut);
    hb_itemPutCLPtr(pItem, szVal, nLen);
    nOffset += nSize;
    break;
  case HB_SERIAL_XHB_L:
    hb_itemPutL(pItem, pBuffer[nOffset++] == 'T');
    break;
  case HB_SERIAL_XHB_N:
    switch (pBuffer[nOffset++]) {
    case 'I':
      hb_itemPutNI(pItem, static_cast<int>(HB_GET_BE_UINT64(&pBuffer[nOffset])));
      break;
    case 'L':
      hb_itemPutNL(pItem, static_cast<long>(HB_GET_BE_UINT64(&pBuffer[nOffset])));
      break;
    case 'X':
      hb_itemPutNInt(pItem, static_cast<HB_MAXINT>(HB_GET_BE_UINT64(&pBuffer[nOffset])));
      /* this is workaround for bug in xHarbour serialization code */
      nOffset += 10;
      break;
    case 'D':
      hb_itemPutND(pItem, HB_GET_LE_DOUBLE(&pBuffer[nOffset]));
      break;
    default:
      hb_itemClear(pItem);
      break;
    }
    nOffset += 8;
    break;
  case HB_SERIAL_XHB_D:
    hb_itemPutDL(pItem, static_cast<long>(HB_GET_BE_UINT64(&pBuffer[nOffset])));
    nOffset += 8;
    break;
  case HB_SERIAL_XHB_T:
    hb_itemPutTD(pItem, HB_GET_LE_DOUBLE(&pBuffer[nOffset]));
    nOffset += 8;
    break;
  case HB_SERIAL_XHB_Z:
    hb_itemClear(pItem);
    break;
  case HB_SERIAL_XHB_A:
    nLen = static_cast<HB_SIZE>(HB_GET_BE_UINT64(&pBuffer[nOffset]));
    hb_itemSerialTypedSet(pRefList, pItem, HB_SERIAL_XHB_A);
    nOffset = hb_deserializeArray(pItem, cdpIn, cdpOut, pBuffer, nOffset + 8, nLen, pRefList);
    break;
  case HB_SERIAL_XHB_B:
    nOffset = hb_deserializeItem(pItem, cdpIn, cdpOut, pBuffer, nOffset, pRefList);
/* we do not support xHarbour codeblock deserialization: HB_RestoreBlock(pItem) */
#if 0
         hb_itemSerialTypedSet(pRefList, pItem, HB_SERIAL_XHB_B);
#endif
    hb_itemClear(pItem);
    break;
  case HB_SERIAL_XHB_H:
    nLen = static_cast<HB_SIZE>(HB_GET_BE_UINT64(&pBuffer[nOffset]));
    hb_itemSerialTypedSet(pRefList, pItem, HB_SERIAL_XHB_H);
    nOffset = hb_deserializeHash(pItem, cdpIn, cdpOut, pBuffer, nOffset + 8, nLen, pRefList);
    hb_hashSetFlags(pItem, HB_HASH_KEEPORDER | HB_HASH_RESORT);
    break;
  case HB_SERIAL_XHB_O: {
    HB_USHORT uiClass;

    nLen = static_cast<HB_SIZE>(HB_GET_BE_UINT64(&pBuffer[nOffset]));
    /* deserialize :className */
    nOffset = hb_deserializeItem(pItem, cdpIn, cdpOut, pBuffer, nOffset + 8, pRefList);
    /* find class handle */
    uiClass = hb_clsFindClass(hb_itemGetCPtr(pItem), nullptr);
    if (uiClass && hb_vmRequestReenter()) {
      auto pMsg = hb_stackAllocItem();
      auto pVal = hb_stackAllocItem();

      hb_clsAssociate(uiClass);
      hb_itemMove(pItem, hb_stackReturnItem());
      hb_itemSerialTypedSet(pRefList, pItem, HB_SERIAL_XHB_O);

      while (nLen--) {
        nOffset = hb_deserializeItem(pMsg, cdpIn, cdpOut, pBuffer, nOffset, pRefList);
        nOffset = hb_deserializeItem(pVal, cdpIn, cdpOut, pBuffer, nOffset, pRefList);
        if (hb_vmRequestQuery() == 0) {
          char szMsg[HB_SYMBOL_NAME_LEN];
          hb_snprintf(szMsg, sizeof(szMsg), "_%s", hb_itemGetCPtr(pMsg));
          hb_objSendMsg(pItem, szMsg, 1, pVal);
        }
      }
      hb_stackPop();
      hb_stackPop();
      hb_vmRequestRestore();
    } else {
      hb_itemSerialTypedSet(pRefList, pItem, HB_SERIAL_XHB_O);
      while (nLen--) {
        nOffset = hb_deserializeItem(pItem, cdpIn, cdpOut, pBuffer, nOffset, pRefList);
        nOffset = hb_deserializeItem(pItem, cdpIn, cdpOut, pBuffer, nOffset, pRefList);
      }
      hb_itemClear(pItem);
    }
    break;
  }
  case HB_SERIAL_XHB_Q: {
    HB_USHORT uiClass;

    nPad = static_cast<HB_SIZE>(HB_GET_BE_UINT64(&pBuffer[nOffset])) + nOffset + 8;
    /* deserialize :className */
    nOffset = hb_deserializeItem(pItem, cdpIn, cdpOut, pBuffer, nOffset + 8, pRefList);
    nLen = nPad - nOffset;
    /* get serialized HBPERSISTENT text */
    szVal = hb_cdpnDup(reinterpret_cast<const char *>(&pBuffer[nOffset]), &nLen, cdpIn, cdpOut);
    nOffset = nPad;
    /* find class handle */
    uiClass = hb_clsFindClass(hb_itemGetCPtr(pItem), nullptr);
    hb_itemPutCLPtr(pItem, szVal, nLen);
    if (uiClass && hb_vmRequestReenter()) {
      hb_clsAssociate(uiClass);
      hb_vmPushDynSym(hb_dynsymGetCase("LOADFROMTEXT"));
      hb_vmPush(hb_stackReturnItem());
      hb_vmPush(pItem);
      hb_vmPushLogical(true);
      hb_itemMove(pItem, hb_stackReturnItem());
      hb_vmSend(2);
      hb_vmRequestRestore();
    } else {
      hb_itemClear(pItem);
    }
    hb_itemSerialTypedSet(pRefList, pItem, HB_SERIAL_XHB_O);
    break;
  }
  case HB_SERIAL_XHB_R:
    hb_itemSerialTypedGet(pRefList, pItem, pBuffer[nOffset],
                          static_cast<HB_SIZE>(HB_GET_BE_UINT64(&pBuffer[nOffset + 1])));
    nOffset += 9;
    break;

  default:
    hb_itemClear(pItem);
    break;
  }

  return nOffset;
}

/*
 * public API functions
 */
char *hb_itemSerializeCP(PHB_ITEM pItem, int iFlags, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut, HB_SIZE *pnSize)
{
  HB_REF_LIST refList;

  hb_itemSerialRefListInit(&refList);
  HB_SIZE nSize = hb_itemSerialSize(pItem, iFlags, cdpIn, cdpOut, &refList, 0);
  auto pBuffer = static_cast<HB_UCHAR *>(hb_xgrab(nSize + 1));
  hb_itemSerialUnusedFree(&refList);
  hb_serializeItem(pItem, iFlags, cdpIn, cdpOut, pBuffer, 0, &refList);
  hb_itemSerialRefListFree(&refList);

  if ((iFlags & HB_SERIALIZE_COMPRESS) != 0 && nSize > 20) {
    HB_SIZE nDest = hb_zlibCompressBound(nSize);
    if (nDest > 0) {
      auto pDest = static_cast<char *>(hb_xgrab(nDest));
      if (hb_zlibCompress(pDest, &nDest, reinterpret_cast<const char *>(pBuffer), nSize, HB_ZLIB_COMPRESSION_DEFAULT) ==
          HB_ZLIB_RES_OK) {
        if (nDest + 9 < nSize) {
          pBuffer[0] = HB_SERIAL_ZCOMPRESS;
          HB_PUT_LE_UINT32(&pBuffer[1], nDest);
          HB_PUT_LE_UINT32(&pBuffer[5], nSize);
          memcpy(&pBuffer[9], pDest, nDest);
          nSize = nDest + 9;
          pBuffer = static_cast<HB_UCHAR *>(hb_xrealloc(pBuffer, nSize + 1));
        }
      }
      hb_xfree(pDest);
    }
  }

  pBuffer[nSize] = '\0';
  if (pnSize) {
    *pnSize = nSize;
  }

  return reinterpret_cast<char *>(pBuffer);
}

char *hb_itemSerialize(PHB_ITEM pItem, int iFlags, HB_SIZE *pnSize)
{
  return hb_itemSerializeCP(pItem, iFlags, nullptr, nullptr, pnSize);
}

PHB_ITEM hb_itemDeserializeCP(const char **pBufferPtr, HB_SIZE *pnSize, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut)
{
  auto pBuffer = reinterpret_cast<const HB_UCHAR *>(*pBufferPtr);
  PHB_ITEM pItem = nullptr;
  HB_REF_LIST refList;

  hb_itemSerialRefListInit(&refList);
  if (!pnSize || hb_deserializeTest(reinterpret_cast<const HB_UCHAR **>(pBufferPtr), pnSize, 0, &refList)) {
    pItem = hb_itemNew(nullptr);
    hb_deserializeItem(pItem, cdpIn, cdpOut, pBuffer, 0, &refList);
  }
  hb_itemSerialRefListFree(&refList);

  return pItem;
}

PHB_ITEM hb_itemDeserialize(const char **pBufferPtr, HB_SIZE *pnSize)
{
  return hb_itemDeserializeCP(pBufferPtr, pnSize, nullptr, nullptr);
}

HB_FUNC(HB_SERIALIZE)
{
  auto pItem = hb_param(1, Harbour::Item::ANY);

  if (pItem != nullptr) {
    auto pszCdpIn = hb_parc(3);
    auto pszCdpOut = hb_parc(4);

    PHB_CODEPAGE cdpIn = pszCdpIn ? hb_cdpFindExt(pszCdpIn) : hb_vmCDP();
    PHB_CODEPAGE cdpOut = pszCdpOut ? hb_cdpFindExt(pszCdpOut) : hb_vmCDP();

    int iFlags;
    if (HB_ISNUM(2)) {
      iFlags = hb_parni(2);
    } else {
      iFlags = hb_parl(2) ? HB_SERIALIZE_NUMSIZE : 0;
    }

    HB_SIZE nSize;
    char *pBuffer = hb_itemSerializeCP(pItem, iFlags, cdpIn, cdpOut, &nSize);
    hb_retclen_buffer(pBuffer, nSize);
  }
}

HB_FUNC(HB_DESERIALIZE)
{
  auto pParam = hb_param(1, Harbour::Item::BYREF);
  auto nSize = hb_parclen(1);

  if (nSize) {
    auto pBuffer = hb_parc(1);
    auto pszCdpIn = hb_parc(2);
    auto pszCdpOut = hb_parc(3);

    PHB_CODEPAGE cdpIn = pszCdpIn ? hb_cdpFindExt(pszCdpIn) : hb_vmCDP();
    PHB_CODEPAGE cdpOut = pszCdpOut ? hb_cdpFindExt(pszCdpOut) : hb_vmCDP();

    PHB_ITEM pItem = hb_itemDeserializeCP(&pBuffer, &nSize, cdpIn, cdpOut);
    if (pItem != nullptr) {
      hb_itemReturn(pItem);
      if (pParam) {
        hb_itemPutCL(pItem, pBuffer, nSize);
        hb_itemMove(pParam, pItem);
      }
      hb_itemRelease(pItem);
    } else if (pParam) {
      hb_itemClear(pParam);
    }
  } else if (pParam) {
    hb_itemClear(pParam);
  }
}
