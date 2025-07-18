// ADS memory index driver
// Copyright 2007 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>

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

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

#include "rddads.hpp"

#include <hbinit.hpp>
#include <hbapierr.hpp>
#include <hbdbferr.hpp>
#include <hbapilng.hpp>
#include <hbdate.hpp>
#include <hbset.hpp>
#include <hbvm.hpp>

#include "rddsys.ch"

#ifndef SUPER_ORDDESTROY
#define SUPER_ORDDESTROY(w, ip) ((*(SUPERTABLE)->orderDestroy)(w, ip))
#endif

#define SUPERTABLE (&adsxSuper)

#define MIX_MAXKEYLEN 250
#define MIX_MAXTAGNAMELEN 16

#define MIX_KEYPOOLFIRST 256
#define MIX_KEYPOOLRESIZE 1024

typedef struct _MIXKEY
{
  HB_ULONG rec;
  HB_BYTE val[1];
} MIXKEY, *PMIXKEY;

typedef struct _MIXTAG
{
  struct _MIXTAG *pNext;

  char *szName;
  char *szKeyExpr;
  char *szForExpr;
  PHB_ITEM pKeyItem;
  PHB_ITEM pForItem;
  HB_BYTE bType;
  HB_USHORT uiLen;

  PMIXKEY *pKeys;
  HB_ULONG ulRecMax;
  HB_ULONG ulRecCount;

  PHB_CODEPAGE pCodepage; // National sort table for character key tags, nullptr otherwise

  HB_ULONG ulKeyNo;
} MIXTAG, *PMIXTAG;

typedef HB_ULONG MIXUPDATE, *PMIXUPDATE;

typedef struct _ADSXAREA_
{
  ADSAREA adsarea;

  // Additional fields for ADSX RDD

  PMIXTAG pTagList;
  PMIXTAG pTagCurrent;
} ADSXAREA, *ADSXAREAP;

static HB_USHORT s_uiRddIdADSX = static_cast<HB_USHORT>(-1);
static HB_USHORT s_uiRddIdADSNTXX = static_cast<HB_USHORT>(-1);
static HB_USHORT s_uiRddIdADSCDXX = static_cast<HB_USHORT>(-1);
#if ADS_LIB_VERSION >= 900
static HB_USHORT s_uiRddIdADSVFPX = static_cast<HB_USHORT>(-1);
#endif
static HB_USHORT s_uiRddIdADSADTX = static_cast<HB_USHORT>(-1);
static RDDFUNCS adsxSuper;

// Misc functions

static HB_ERRCODE hb_mixErrorRT(ADSXAREAP pArea, HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char *filename,
                                HB_ERRCODE errOsCode, HB_USHORT uiFlags)
{
  HB_ERRCODE iRet = Harbour::FAILURE;

  if (hb_vmRequestQuery() == 0)
  {
    auto pError = hb_errNew();
    hb_errPutGenCode(pError, errGenCode);
    hb_errPutSubCode(pError, errSubCode);
    hb_errPutOsCode(pError, errOsCode);
    hb_errPutDescription(pError, hb_langDGetErrorDesc(errGenCode));
    if (filename)
    {
      hb_errPutFileName(pError, filename);
    }
    if (uiFlags)
    {
      hb_errPutFlags(pError, uiFlags);
    }
    iRet = SELF_ERROR(&pArea->adsarea.area, pError);
    hb_errRelease(pError);
  }
  return iRet;
}

// Clone of ADS RDD function
static HB_ERRCODE hb_adsUpdateAreaFlags(ADSXAREAP pArea)
{
  UNSIGNED16 u16Bof, u16Eof, u16Found;

  AdsAtBOF(pArea->adsarea.hTable, &u16Bof);
  AdsAtEOF(pArea->adsarea.hTable, &u16Eof);
  AdsIsFound(pArea->adsarea.hTable, &u16Found);

  pArea->adsarea.area.fBof = u16Bof != 0;
  pArea->adsarea.area.fEof = u16Eof != 0;
  pArea->adsarea.area.fFound = u16Found != 0;

  pArea->adsarea.fPositioned = !pArea->adsarea.area.fBof && !pArea->adsarea.area.fEof;

  return Harbour::SUCCESS;
}

// Memory Index

static PMIXKEY mixKeyNew(PHB_ITEM pItem, HB_ULONG ulRecNo, HB_BYTE bType, HB_USHORT uiLen)
{
  double dbl;
  HB_BYTE buf[8];

  auto pKey = static_cast<PMIXKEY>(hb_xgrab(sizeof(HB_ULONG) + uiLen));
  pKey->rec = ulRecNo;

  switch (bType)
  {
  case 'C': {
    auto nLen = hb_itemGetCLen(pItem);
    if (nLen > static_cast<HB_SIZE>(uiLen))
    {
      nLen = uiLen;
    }
    memcpy(pKey->val, hb_itemGetCPtr(pItem), nLen);
    if (nLen < static_cast<HB_SIZE>(uiLen))
    {
      memset(pKey->val + nLen, ' ', static_cast<HB_SIZE>(uiLen) - nLen);
    }
    break;
  }
  case 'N':
    dbl = hb_itemGetND(pItem);
    HB_DBL2ORD(&dbl, buf);
    memcpy(pKey->val, buf, 8);
    break;

  case 'D':
    dbl = static_cast<double>(hb_itemGetDL(pItem));
    HB_DBL2ORD(&dbl, buf);
    memcpy(pKey->val, buf, 8);
    break;

  case 'L':
    pKey->val[0] = static_cast<HB_BYTE>(hb_itemGetL(pItem) ? 'T' : 'F');
    break;

  default:
    memset(pKey->val, ' ', uiLen);
  }
  return pKey;
}

static PMIXKEY mixKeyEval(PMIXTAG pTag, ADSXAREAP pArea)
{
  PMIXKEY pKey;
  auto iCurrArea = hb_rddGetCurrentWorkAreaNumber();
  auto pCodepage = hb_cdpSelect(pArea->adsarea.area.cdPage);

  if (iCurrArea != pArea->adsarea.area.uiArea)
  {
    hb_rddSelectWorkAreaNumber(pArea->adsarea.area.uiArea);
  }
  else
  {
    iCurrArea = 0;
  }

  auto pItem = hb_vmEvalBlockOrMacro(pTag->pKeyItem);

  pKey = mixKeyNew(pItem, pArea->adsarea.ulRecNo, pTag->bType, pTag->uiLen);

  if (iCurrArea)
  {
    hb_rddSelectWorkAreaNumber(iCurrArea);
  }

  hb_cdpSelect(pCodepage);

  return pKey;
}

static bool mixEvalCond(PHB_ITEM pCondItem, ADSXAREAP pArea)
{
  int iCurrArea = 0;
  bool fRet;

  if (pArea != nullptr)
  {
    iCurrArea = hb_rddGetCurrentWorkAreaNumber();

    if (iCurrArea != pArea->adsarea.area.uiArea)
    {
      hb_rddSelectWorkAreaNumber(pArea->adsarea.area.uiArea);
    }
    else
    {
      iCurrArea = 0;
    }
  }

  fRet = hb_itemGetL(hb_vmEvalBlockOrMacro(pCondItem));

  if (iCurrArea)
  {
    hb_rddSelectWorkAreaNumber(iCurrArea);
  }

  return fRet;
}

static void mixKeyFree(PMIXKEY pKey)
{
  hb_xfree(pKey);
}

static int mixQSortCompare(PMIXKEY p1, PMIXKEY p2, HB_USHORT uiLen, PHB_CODEPAGE pCodepage)
{
  int i;

  if (pCodepage)
  {
    i = hb_cdpcmp(reinterpret_cast<const char *>(p1->val), static_cast<HB_ULONG>(uiLen),
                  reinterpret_cast<const char *>(p2->val), static_cast<HB_ULONG>(uiLen), pCodepage, 0);
  }
  else
  {
    i = memcmp(p1->val, p2->val, uiLen);
  }

  if (i == 0)
  {
    if (p1->rec < p2->rec)
    {
      return -1;
    }
    else if (p1->rec > p2->rec)
    {
      return 1;
    }
    else
    {
      return 0;
    }
  }
  else
  { // This is used to compare keys excluding recno
    if (i < 0)
    {
      i = -2;
    }
    else
    {
      i = 2;
    }
  }
  return i;
}

static void mixQSort(PMIXKEY *pKeys, HB_ULONG left, HB_ULONG right, HB_USHORT uiLen, PHB_CODEPAGE pCodepage)
{
  HB_ULONG l, r;
  PMIXKEY x, h;

  l = left;
  r = right;

  x = pKeys[(l + r) / 2];

  do
  {
    while (mixQSortCompare(x, pKeys[l], uiLen, pCodepage) > 0)
    {
      l++;
    }
    while (mixQSortCompare(pKeys[r], x, uiLen, pCodepage) > 0)
    {
      r--;
    }

    if (l < r)
    {
      h = pKeys[l];
      pKeys[l] = pKeys[r];
      pKeys[r] = h;
      l++;
      r--;
    }
  } while (l < r);

  if (left < r && (r != right))
  {
    mixQSort(pKeys, left, r, uiLen, pCodepage);
  }

  if (l < right && (l != left))
  {
    mixQSort(pKeys, l, right, uiLen, pCodepage);
  }
}

static PMIXKEY mixFindKeyLen(PMIXTAG pTag, PMIXKEY pKey, HB_USHORT uiLen, HB_ULONG *ulKeyPos)
{
  HB_ULONG l, r;
  int i = 1;

  if (!pTag->ulRecCount)
  {
    if (ulKeyPos)
    {
      *ulKeyPos = 0;
    }
    return nullptr;
  }

  l = 0;
  r = pTag->ulRecCount - 1;

  while (l < r)
  {
    i = mixQSortCompare(pTag->pKeys[(l + r) / 2], pKey, uiLen, pTag->pCodepage);

    if (i < 0)
    {
      l = (l + r) / 2 + 1;
    }
    else if (i > 0)
    {
      r = (l + r) / 2;
    }
    else
    {
      l = r = (l + r) / 2;
    }
  }

  if (i)
  {
    i = mixQSortCompare(pTag->pKeys[l], pKey, uiLen, pTag->pCodepage);
    if (i < 0)
    {
      l++;
    }
  }

  if (ulKeyPos)
  {
    *ulKeyPos = l;
  }

  return i ? nullptr : pTag->pKeys[l];
}

static PMIXKEY mixFindKey(PMIXTAG pTag, PMIXKEY pKey, HB_ULONG *ulKeyPos)
{
  return mixFindKeyLen(pTag, pKey, pTag->uiLen, ulKeyPos);
}

static int mixCompareKey(PMIXTAG pTag, HB_ULONG ulKeyPos, PMIXKEY pKey, HB_USHORT uiLen)
{
  return mixQSortCompare(pTag->pKeys[ulKeyPos], pKey, uiLen, pTag->pCodepage);
}

static PMIXTAG mixTagCreate(const char *szTagName, PHB_ITEM pKeyExpr, PHB_ITEM pKeyItem, PHB_ITEM pForItem,
                            PHB_ITEM pWhileItem, HB_BYTE bType, HB_USHORT uiLen, ADSXAREAP pArea)
{
  PMIXKEY pKey;
  LPDBORDERCONDINFO pOrdCondInfo = pArea->adsarea.area.lpdbOrdCondInfo;
  ADSHANDLE hOrder;
  HB_ULONG ulRec, ulStartRec, ulNextCount = 0;
  HB_LONG lStep = 0;
  PHB_ITEM pItem, pEvalItem = nullptr;

  auto pTag = static_cast<PMIXTAG>(hb_xgrabz(sizeof(MIXTAG)));

  pTag->szName = static_cast<char *>(hb_xgrab(MIX_MAXTAGNAMELEN + 1));
  hb_strncpyUpperTrim(pTag->szName, szTagName, MIX_MAXTAGNAMELEN);

  pTag->szKeyExpr = static_cast<char *>(hb_xgrab(hb_itemGetCLen(pKeyExpr) + 1));
  hb_strncpyTrim(pTag->szKeyExpr, hb_itemGetCPtr(pKeyExpr), hb_itemGetCLen(pKeyExpr));

  // TODO: for expression
  pTag->szForExpr = nullptr;

  pTag->pKeyItem = pKeyItem;
  pTag->pForItem = pForItem;
  pTag->bType = bType;
  pTag->uiLen = uiLen;

  // Use national support
  if (bType == 'C' && pArea->adsarea.area.cdPage && !HB_CDP_ISBINSORT(pArea->adsarea.area.cdPage))
  {
    pTag->pCodepage = pArea->adsarea.area.cdPage;
  }

  pTag->pKeys = static_cast<PMIXKEY *>(hb_xgrab(sizeof(PMIXKEY) * MIX_KEYPOOLFIRST));
  pTag->ulRecMax = MIX_KEYPOOLFIRST;

  ulStartRec = 0;

  if (pOrdCondInfo)
  {
    pEvalItem = pOrdCondInfo->itmCobEval;
    lStep = pOrdCondInfo->lStep;
  }

  if (!pOrdCondInfo || pOrdCondInfo->fAll)
  {
    pArea->adsarea.hOrdCurrent = 0;
  }
  else
  {
    if (pOrdCondInfo->itmRecID)
    {
      ulStartRec = hb_itemGetNL(pOrdCondInfo->itmRecID);
    }

    if (ulStartRec)
    {
      ulNextCount = 1;
    }
    else if (pOrdCondInfo->fRest || pOrdCondInfo->lNextCount > 0)
    {
      if (pOrdCondInfo->itmStartRecID)
      {
        ulStartRec = hb_itemGetNL(pOrdCondInfo->itmStartRecID);
      }

      if (!ulStartRec)
      {
        ulStartRec = pArea->adsarea.ulRecNo;
      }

      if (pArea->adsarea.area.lpdbOrdCondInfo->lNextCount > 0)
      {
        ulNextCount = pOrdCondInfo->lNextCount;
      }
    }
    else if (!pOrdCondInfo->fUseCurrent)
    {
      pArea->adsarea.hOrdCurrent = 0;
    }
  }

  hOrder = pArea->adsarea.hOrdCurrent ? pArea->adsarea.hOrdCurrent : pArea->adsarea.hTable;

  if (ulStartRec)
  {
    AdsGotoRecord(pArea->adsarea.hTable, ulStartRec);
  }
  else
  {
    AdsGotoTop(hOrder);
  }
  hb_adsUpdateAreaFlags(pArea);

  while (!pArea->adsarea.area.fEof)
  {
    SELF_RECNO(&pArea->adsarea.area, &ulRec);
    SELF_GOTO(&pArea->adsarea.area, ulRec);

    if (pEvalItem)
    {
      if (lStep >= pOrdCondInfo->lStep)
      {
        lStep = 0;
        if (!mixEvalCond(pEvalItem, nullptr))
        {
          break;
        }
      }
      ++lStep;
    }

    if (pWhileItem && !mixEvalCond(pWhileItem, nullptr))
    {
      break;
    }

    if (pForItem == nullptr || mixEvalCond(pForItem, nullptr))
    {
      pItem = hb_vmEvalBlockOrMacro(pKeyItem);

      pKey = mixKeyNew(pItem, ulRec, bType, uiLen);

      if (pTag->ulRecCount == pTag->ulRecMax)
      {
        pTag->pKeys =
            static_cast<PMIXKEY *>(hb_xrealloc(pTag->pKeys, sizeof(PMIXKEY) * (pTag->ulRecMax + MIX_KEYPOOLRESIZE)));
        pTag->ulRecMax += MIX_KEYPOOLRESIZE;
      }

      pTag->pKeys[pTag->ulRecCount] = pKey;
      pTag->ulRecCount++;
    }

    if (ulNextCount)
    {
      ulNextCount--;
      if (!ulNextCount)
      {
        break;
      }
    }

    AdsSkip(hOrder, 1);
    hb_adsUpdateAreaFlags(pArea);
  }

  // QuickSort
  if (pTag->ulRecCount >= 2)
  {
    mixQSort(pTag->pKeys, 0, pTag->ulRecCount - 1, uiLen, pTag->pCodepage);
  }

  return pTag;
}

static void mixTagDestroy(PMIXTAG pTag)
{
  hb_xfree(pTag->szName);
  hb_xfree(pTag->szKeyExpr);
  if (pTag->szForExpr)
  {
    hb_xfree(pTag->szForExpr);
  }

  if (pTag->pKeyItem)
  {
    hb_vmDestroyBlockOrMacro(pTag->pKeyItem);
  }

  if (pTag->pForItem)
  {
    hb_vmDestroyBlockOrMacro(pTag->pForItem);
  }

  for (HB_ULONG ul = 0; ul < pTag->ulRecCount; ul++)
  {
    mixKeyFree(pTag->pKeys[ul]);
  }

  hb_xfree(pTag->pKeys);

  hb_xfree(pTag);
}

static PMIXTAG mixFindTag(ADSXAREAP pArea, PHB_ITEM pOrder)
{
  PMIXTAG pTag;

  if (pOrder->isNumber())
  {
    UNSIGNED16 usOrder = 0, usFind;

    usFind = static_cast<UNSIGNED16>(hb_itemGetNI(pOrder));

    AdsGetNumIndexes(pArea->adsarea.hTable, &usOrder);
    usOrder++;
    pTag = pArea->pTagList;
    while (pTag && usOrder != usFind)
    {
      pTag = pTag->pNext;
    }
  }
  else
  {
    char szTag[MIX_MAXTAGNAMELEN + 1];

    hb_strncpyUpperTrim(szTag, hb_itemGetCPtr(pOrder), MIX_MAXTAGNAMELEN);
    pTag = pArea->pTagList;
    while (pTag && hb_stricmp(szTag, pTag->szName))
    {
      pTag = pTag->pNext;
    }
  }

  return pTag;
}

static PMIXUPDATE mixUpdateCreate(ADSXAREAP pArea)
{
  PMIXTAG pTag;
  int iTag;

  pTag = pArea->pTagList;
  if (!pArea->adsarea.fPositioned || !pTag)
  {
    return nullptr;
  }

  iTag = 0;
  while (pTag)
  {
    pTag = pTag->pNext;
    iTag++;
  }

  auto pUpdate = static_cast<PMIXUPDATE>(hb_xgrab(sizeof(MIXUPDATE) * iTag));
  pTag = pArea->pTagList;
  iTag = 0;
  while (pTag)
  {
    PMIXKEY pKey = mixKeyEval(pTag, pArea);
    HB_ULONG ulKeyPos;

    pUpdate[iTag] = mixFindKey(pTag, pKey, &ulKeyPos) ? ulKeyPos : static_cast<HB_ULONG>(-1);
    mixKeyFree(pKey);

    pTag = pTag->pNext;
    iTag++;
  }

  return pUpdate;
}

static void mixUpdateDestroy(ADSXAREAP pArea, PMIXUPDATE pUpdate, int fUpdate)
{
  PMIXTAG pTag = pArea->pTagList;
  int iTag;

  if (!pUpdate)
  {
    return;
  }

  if (!fUpdate)
  {
    hb_xfree(pUpdate);
    return;
  }

  iTag = 0;
  while (pTag)
  {
    bool bFor = pTag->pForItem == nullptr || mixEvalCond(pTag->pForItem, pArea);
    if (pUpdate[iTag] == static_cast<HB_ULONG>(-1))
    {
      if (bFor)
      {
        HB_ULONG ulKeyPos;
        PMIXKEY pKey = mixKeyEval(pTag, pArea);
        mixFindKey(pTag, pKey, &ulKeyPos);

        // insert key into index
        if (pTag->ulRecCount == pTag->ulRecMax)
        {
          pTag->pKeys =
              static_cast<PMIXKEY *>(hb_xrealloc(pTag->pKeys, sizeof(PMIXKEY) * (pTag->ulRecMax + MIX_KEYPOOLRESIZE)));
          pTag->ulRecMax += MIX_KEYPOOLRESIZE;
        }
        if (ulKeyPos < pTag->ulRecCount)
        {
          memmove(pTag->pKeys + ulKeyPos + 1, pTag->pKeys + ulKeyPos, (pTag->ulRecCount - ulKeyPos) * sizeof(PMIXKEY));
        }
        pTag->pKeys[ulKeyPos] = pKey;
        pTag->ulRecCount++;
      }
    }
    else
    {
      PMIXKEY pKey = mixKeyEval(pTag, pArea);
      if (bFor)
      {
        if (mixCompareKey(pTag, pUpdate[iTag], pKey, pTag->uiLen) != 0)
        {
          HB_ULONG ulKeyPos;
          mixKeyFree(pTag->pKeys[pUpdate[iTag]]);
          mixFindKey(pTag, pKey, &ulKeyPos);
          if (ulKeyPos == pUpdate[iTag] || ulKeyPos == pUpdate[iTag] + 1)
          {
            // assign new key in same position
            pTag->pKeys[pUpdate[iTag]] = pKey;
          }
          else
          {
            // move keys and assign new key to new position
            if (ulKeyPos < pUpdate[iTag])
            {
              memmove(pTag->pKeys + ulKeyPos + 1, pTag->pKeys + ulKeyPos, (pUpdate[iTag] - ulKeyPos) * sizeof(PMIXKEY));
              pTag->pKeys[ulKeyPos] = pKey;
            }
            else
            {
              memmove(pTag->pKeys + pUpdate[iTag], pTag->pKeys + pUpdate[iTag] + 1,
                      (ulKeyPos - pUpdate[iTag] - 1) * sizeof(PMIXKEY));
              pTag->pKeys[ulKeyPos - 1] = pKey;
            }
          }
        }
        else
        {
          mixKeyFree(pKey);
        }
      }
      else
      {
        // delete key
        mixKeyFree(pKey);
        memmove(pTag->pKeys + pUpdate[iTag], pTag->pKeys + pUpdate[iTag] + 1,
                (pTag->ulRecCount - pUpdate[iTag]) * sizeof(PMIXKEY));
        pTag->ulRecCount--;
      }
    }
    pTag = pTag->pNext;
    iTag++;
  }
  hb_xfree(pUpdate);
}

// ADSX RDD METHODS

static HB_ERRCODE adsxGoBottom(ADSXAREAP pArea)
{
  PMIXTAG pTag;
  HB_ULONG ulRecNo;

  pTag = pArea->pTagCurrent;

  if (!pTag)
  {
    return SUPER_GOBOTTOM(&pArea->adsarea.area);
  }

  if (pTag->ulRecCount > 0)
  {
    ulRecNo = pTag->pKeys[pTag->ulRecCount - 1]->rec;
  }
  else
  {
    ulRecNo = 0;
  }
  if (SUPER_GOTO(&pArea->adsarea.area, ulRecNo) == Harbour::SUCCESS)
  {
    pTag->ulKeyNo = ulRecNo ? pTag->ulRecCount : 0;
    return Harbour::SUCCESS;
  }
  pTag->ulKeyNo = 0;
  return Harbour::FAILURE;
}

static HB_ERRCODE adsxGoTop(ADSXAREAP pArea)
{
  PMIXTAG pTag;
  HB_ULONG ulRecNo;

  pTag = pArea->pTagCurrent;

  if (!pTag)
  {
    return SUPER_GOTOP(&pArea->adsarea.area);
  }

  if (pTag->ulRecCount > 0)
  {
    ulRecNo = pTag->pKeys[0]->rec;
  }
  else
  {
    ulRecNo = 0;
  }
  if (SUPER_GOTO(&pArea->adsarea.area, ulRecNo) == Harbour::SUCCESS)
  {
    pTag->ulKeyNo = ulRecNo ? 1 : 0;
    return Harbour::SUCCESS;
  }
  pTag->ulKeyNo = 0;
  return Harbour::FAILURE;
}

static HB_ERRCODE adsxSeek(ADSXAREAP pArea, HB_BOOL bSoftSeek, PHB_ITEM pKey, HB_BOOL bFindLast)
{
  PMIXKEY pMixKey;
  HB_USHORT uiLen;
  HB_ULONG ulKeyPos, ulRecNo;
  HB_ERRCODE errCode;
  bool fFound = false;

  if (!pArea->pTagCurrent)
  {
    return SUPER_SEEK(&pArea->adsarea.area, bSoftSeek, pKey, bFindLast);
  }

  // TODO: pKey type validation, EG_DATATYPE runtime error
  uiLen = pArea->pTagCurrent->uiLen;
  pMixKey = mixKeyNew(pKey, bFindLast ? static_cast<HB_ULONG>(-1) : 0, pArea->pTagCurrent->bType, uiLen);

  if (pArea->pTagCurrent->bType == 'C')
  {
    auto nLen = hb_itemGetCLen(pKey);
    if (nLen < static_cast<HB_SIZE>(uiLen))
    {
      uiLen = static_cast<HB_USHORT>(nLen);
    }
  }

  // reset any pending relations - I hope ACE make the same and the problem
  // reported in GOTO() does not exist here
  SELF_RESETREL(&pArea->adsarea);

  mixFindKeyLen(pArea->pTagCurrent, pMixKey, uiLen, &ulKeyPos);

  ulRecNo = 0;
  if (bFindLast)
  {
    if (ulKeyPos > 0 && mixCompareKey(pArea->pTagCurrent, ulKeyPos - 1, pMixKey, uiLen) == -1)
    {
      ulRecNo = pArea->pTagCurrent->pKeys[ulKeyPos - 1]->rec;
      fFound = true;
    }
    else if (ulKeyPos < pArea->pTagCurrent->ulRecCount)
    {
      ulRecNo = pArea->pTagCurrent->pKeys[ulKeyPos]->rec;
      fFound = false;
    }
  }
  else
  {
    if (ulKeyPos < pArea->pTagCurrent->ulRecCount)
    {
      ulRecNo = pArea->pTagCurrent->pKeys[ulKeyPos]->rec;
      fFound = mixCompareKey(pArea->pTagCurrent, ulKeyPos, pMixKey, uiLen) == 1;
    }
  }

  mixKeyFree(pMixKey);

  ulRecNo = (bSoftSeek || fFound) ? ulRecNo : 0;

  errCode = SELF_GOTO(&pArea->adsarea.area, ulRecNo);

  pArea->adsarea.area.fEof = ulRecNo == 0;
  pArea->adsarea.area.fBof = false;
  pArea->adsarea.area.fFound = fFound;
  pArea->adsarea.fPositioned = !pArea->adsarea.area.fEof;
  return errCode;
}

static HB_ERRCODE adsxSkip(ADSXAREAP pArea, HB_LONG lToSkip)
{
  PMIXKEY pKey;
  HB_ULONG ulKeyPos;
  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (!pArea->pTagCurrent || lToSkip == 0)
  {
    return SUPER_SKIP(&pArea->adsarea.area, lToSkip);
  }

  // resolve any pending relations
  if (pArea->adsarea.lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->adsarea.area);
  }

  pArea->adsarea.area.fTop = pArea->adsarea.area.fBottom = false;

  if (lToSkip > 0)
  {
    if (!pArea->adsarea.fPositioned)
    {
      errCode = SELF_GOTO(&pArea->adsarea.area, pArea->adsarea.ulRecNo);
    }
    else
    {
      pArea->adsarea.area.fEof = false;
    }

    pKey = mixKeyEval(pArea->pTagCurrent, pArea);

    if (mixFindKey(pArea->pTagCurrent, pKey, &ulKeyPos) &&
        pArea->pTagCurrent->ulRecCount > static_cast<HB_ULONG>(lToSkip) &&
        ulKeyPos < pArea->pTagCurrent->ulRecCount - static_cast<HB_ULONG>(lToSkip))
    {
      if (SELF_GOTO(&pArea->adsarea.area, pArea->pTagCurrent->pKeys[ulKeyPos + lToSkip]->rec) == Harbour::FAILURE)
      {
        errCode = Harbour::FAILURE;
      }

      pArea->adsarea.fPositioned = true;
      pArea->adsarea.area.fEof = false;
    }
    else
    {
      SELF_GOTO(&pArea->adsarea.area, 0);
      pArea->adsarea.fPositioned = false;
      pArea->adsarea.area.fEof = true;
    }

    mixKeyFree(pKey);
    pArea->adsarea.area.fBof = false;
  }
  else
  {
    if (!pArea->adsarea.fPositioned)
    {
      errCode = SELF_GOBOTTOM(&pArea->adsarea.area);
      pArea->adsarea.area.fBottom = false;
      ++lToSkip;
    }
    else
    {
      pArea->adsarea.area.fBof = false;
    }

    pKey = mixKeyEval(pArea->pTagCurrent, pArea);

    if (mixFindKey(pArea->pTagCurrent, pKey, &ulKeyPos) &&
        pArea->pTagCurrent->ulRecCount >= static_cast<HB_ULONG>(-lToSkip) &&
        ulKeyPos >= static_cast<HB_ULONG>(-lToSkip))
    {
      if (SELF_GOTO(&pArea->adsarea.area, pArea->pTagCurrent->pKeys[ulKeyPos + lToSkip]->rec) == Harbour::FAILURE)
      {
        errCode = Harbour::FAILURE;
      }
      pArea->adsarea.area.fBof = false;
    }
    else
    {
      SELF_GOTOP(&pArea->adsarea.area);
      pArea->adsarea.area.fBof = true;
    }

    mixKeyFree(pKey);
    pArea->adsarea.area.fEof = false;
  }

  // Force relational movement in child WorkAreas
  if (pArea->adsarea.area.lpdbRelations)
  {
    SELF_SYNCCHILDREN(&pArea->adsarea.area);
  }

  return errCode;
}

static HB_ERRCODE adsxPutValue(ADSXAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
  PMIXUPDATE pUpdate;
  HB_ERRCODE errCode;

  pUpdate = mixUpdateCreate(pArea);
  errCode = SUPER_PUTVALUE(&pArea->adsarea.area, uiIndex, pItem);
  mixUpdateDestroy(pArea, pUpdate, errCode == Harbour::SUCCESS);
  return errCode;
}

static HB_ERRCODE adsxClose(ADSXAREAP pArea)
{
  pArea->pTagCurrent = nullptr;
  while (pArea->pTagList)
  {
    PMIXTAG pTag = pArea->pTagList;
    pArea->pTagList = pArea->pTagList->pNext;
    mixTagDestroy(pTag);
  }
  return SUPER_CLOSE(&pArea->adsarea.area);
}

static HB_ERRCODE adsxCreate(ADSXAREAP pArea, LPDBOPENINFO pCreateInfo)
{
  if (SUPER_CREATE(&pArea->adsarea.area, pCreateInfo) == Harbour::SUCCESS)
  {
    if (pCreateInfo->cdpId)
    {
      pArea->adsarea.area.cdPage = hb_cdpFind(pCreateInfo->cdpId);
      if (!pArea->adsarea.area.cdPage)
      {
        pArea->adsarea.area.cdPage = hb_vmCDP();
      }
    }
    else
    {
      pArea->adsarea.area.cdPage = hb_vmCDP();
    }
    return Harbour::SUCCESS;
  }
  return Harbour::FAILURE;
}

static HB_ERRCODE adsxNewArea(ADSXAREAP pArea)
{
  HB_ERRCODE errCode;

  errCode = SUPER_NEW(&pArea->adsarea.area);
  if (errCode == Harbour::SUCCESS)
  {
    if (pArea->adsarea.area.rddID == s_uiRddIdADSADTX)
    {
      pArea->adsarea.iFileType = ADS_ADT;
    }
    else if (pArea->adsarea.area.rddID == s_uiRddIdADSNTXX)
    {
      pArea->adsarea.iFileType = ADS_NTX;
    }
    else if (pArea->adsarea.area.rddID == s_uiRddIdADSCDXX)
    {
      pArea->adsarea.iFileType = ADS_CDX;
    }
#if ADS_LIB_VERSION >= 900
    else if (pArea->adsarea.area.rddID == s_uiRddIdADSVFPX)
    {
      pArea->adsarea.iFileType = ADS_VFP;
    }
#endif
    pArea->adsarea.area.uiMaxFieldNameLength =
        (pArea->adsarea.iFileType == ADS_ADT) ? ADS_MAX_FIELD_NAME : ADS_MAX_DBF_FIELD_NAME;
  }
  return errCode;
}

static HB_ERRCODE adsxOpen(ADSXAREAP pArea, LPDBOPENINFO pOpenInfo)
{
  if (SUPER_OPEN(&pArea->adsarea.area, pOpenInfo) == Harbour::SUCCESS)
  {
    if (pOpenInfo->cdpId)
    {
      pArea->adsarea.area.cdPage = hb_cdpFind(pOpenInfo->cdpId);
      if (!pArea->adsarea.area.cdPage)
      {
        pArea->adsarea.area.cdPage = hb_vmCDP();
      }
    }
    else
    {
      pArea->adsarea.area.cdPage = hb_vmCDP();
    }
    return Harbour::SUCCESS;
  }
  return Harbour::FAILURE;
}

static HB_ERRCODE adsxStructSize(ADSXAREAP pArea, HB_USHORT *StructSize)
{
  HB_SYMBOL_UNUSED(pArea);

  *StructSize = sizeof(ADSXAREA);
  return Harbour::SUCCESS;
}

static HB_ERRCODE adsxSysName(ADSXAREAP pArea, HB_BYTE *pBuffer)
{
  UNSIGNED16 u16TableType;
  UNSIGNED32 u32RetVal;

  if (pArea->adsarea.hTable)
  {
    u32RetVal = AdsGetTableType(pArea->adsarea.hTable, &u16TableType);
    if (u32RetVal != AE_SUCCESS)
    {
#if 0
         HB_TRACE( HB_TR_DEBUG, ( "Error in adsxSysName: %lu  pArea->adsarea.hTable %p", static_cast<HB_ULONG>(u32RetVal), static_cast<void*>(static_cast<HB_PTRUINT>(pArea->adsarea.hTable)) ) );
#endif
      u16TableType = static_cast<UNSIGNED16>(pArea->adsarea.iFileType);
    }
  }
  else
  {
    u16TableType = static_cast<UNSIGNED16>(pArea->adsarea.iFileType);
  }

  switch (u16TableType)
  {
  case ADS_NTX:
    hb_strncpy(reinterpret_cast<char *>(pBuffer), "ADSNTXX", HB_RDD_MAX_DRIVERNAME_LEN);
    break;
  case ADS_CDX:
    hb_strncpy(reinterpret_cast<char *>(pBuffer), "ADSCDXX", HB_RDD_MAX_DRIVERNAME_LEN);
    break;
#if ADS_LIB_VERSION >= 900
  case ADS_VFP:
    hb_strncpy(reinterpret_cast<char *>(pBuffer), "ADSVFPX", HB_RDD_MAX_DRIVERNAME_LEN);
    break;
#endif
  case ADS_ADT:
    hb_strncpy(reinterpret_cast<char *>(pBuffer), "ADSADTX", HB_RDD_MAX_DRIVERNAME_LEN);
    break;
  }

  return Harbour::SUCCESS;
}

static HB_ERRCODE adsxOrderListFocus(ADSXAREAP pArea, LPDBORDERINFO pOrderInfo)
{
  if (SUPER_ORDLSTFOCUS(&pArea->adsarea.area, pOrderInfo) == Harbour::SUCCESS)
  {
    if (pArea->pTagCurrent)
    {
      pOrderInfo->itmResult = hb_itemPutC(pOrderInfo->itmResult, pArea->pTagCurrent->szName);
      if (pOrderInfo->itmOrder)
      {
        pArea->pTagCurrent = nullptr;
      }
    }
    return Harbour::SUCCESS;
  }

  if (pArea->pTagCurrent)
  {
    pOrderInfo->itmResult = hb_itemPutC(pOrderInfo->itmResult, pArea->pTagCurrent->szName);
  }

  pArea->pTagCurrent = mixFindTag(pArea, pOrderInfo->itmOrder);
  pArea->adsarea.hOrdCurrent = 0;

  if (pArea->pTagCurrent)
  {
    return Harbour::SUCCESS;
  }
  else
  {
    return Harbour::FAILURE;
  }
}

static HB_ERRCODE adsxOrderCreate(ADSXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo)
{
  PMIXTAG pTagNew, pTag;
  PHB_ITEM pKeyItem, pForItem = nullptr, pWhileItem = nullptr, pResult;
  HB_ULONG ulRecNo;
  HB_USHORT uiLen;
  HB_BYTE bType;
  UNSIGNED16 bValidExpr;
  bool bKeyADS, bForADS, bWhileADS;
  ADSHANDLE hIndex = (ADSHANDLE)0;

  bForADS = bWhileADS = true;

  // Test key expression
  bValidExpr = 0;
  AdsIsExprValid(pArea->adsarea.hTable,
                 reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(pOrderInfo->abExpr))), &bValidExpr);
  bKeyADS = bValidExpr;

  if (pArea->adsarea.area.lpdbOrdCondInfo)
  {
    // Test FOR expression
    if (pArea->adsarea.area.lpdbOrdCondInfo->abFor)
    {
      bValidExpr = 0;
      AdsIsExprValid(pArea->adsarea.hTable, reinterpret_cast<UNSIGNED8 *>(pArea->adsarea.area.lpdbOrdCondInfo->abFor),
                     &bValidExpr);
      bForADS = bValidExpr;
    }
    else if (pArea->adsarea.area.lpdbOrdCondInfo->itmCobFor)
    {
      bForADS = false;
    }

    // Test WHILE expression
    if (pArea->adsarea.area.lpdbOrdCondInfo->abWhile)
    {
      bValidExpr = 0;
      AdsIsExprValid(pArea->adsarea.hTable, reinterpret_cast<UNSIGNED8 *>(pArea->adsarea.area.lpdbOrdCondInfo->abWhile),
                     &bValidExpr);
      bWhileADS = bValidExpr;
    }
    else if (pArea->adsarea.area.lpdbOrdCondInfo->itmCobWhile)
    {
      bWhileADS = false;
    }
  }

  if (bKeyADS && bForADS && bWhileADS)
  {
    return SUPER_ORDCREATE(&pArea->adsarea.area, pOrderInfo);
  }

  if (pArea->adsarea.area.lpdbOrdCondInfo && ((bForADS && pArea->adsarea.area.lpdbOrdCondInfo->abFor) ||
                                              (bWhileADS && pArea->adsarea.area.lpdbOrdCondInfo->abWhile)))
  {
    // We can use server side indexing to filter records. This improves speed
    UNSIGNED32 u32RetVal;
    UNSIGNED8 szKeyExpr[1024];
    UNSIGNED16 usLen = sizeof(szKeyExpr);

    if (pArea->adsarea.area.lpdbOrdCondInfo->fUseCurrent && pArea->adsarea.hOrdCurrent)
    {
      AdsGetIndexExpr(pArea->adsarea.hOrdCurrent, szKeyExpr, &usLen);
    }
    else
    {
      szKeyExpr[0] = '\0';
    }

#if ADS_LIB_VERSION >= 610
    u32RetVal = AdsCreateIndex61(
        pArea->adsarea.area.lpdbOrdCondInfo->fUseCurrent ? pArea->adsarea.hOrdCurrent : pArea->adsarea.hTable,
        static_cast<UNSIGNED8 *>(HB_UNCONST(pOrderInfo->abBagName)),
        static_cast<UNSIGNED8 *>(HB_UNCONST(pOrderInfo->atomBagName)),
        szKeyExpr[0] ? szKeyExpr : reinterpret_cast<UNSIGNED8 *>(const_cast<char *>("1")),
        bForADS ? reinterpret_cast<UNSIGNED8 *>(pArea->adsarea.area.lpdbOrdCondInfo->abFor) : nullptr,
        bWhileADS ? reinterpret_cast<UNSIGNED8 *>(pArea->adsarea.area.lpdbOrdCondInfo->abWhile) : nullptr, ADS_COMPOUND,
        ADS_DEFAULT, &hIndex);
#else
    u32RetVal = AdsCreateIndex(
        pArea->adsarea.area.lpdbOrdCondInfo->fUseCurrent ? pArea->adsarea.hOrdCurrent : pArea->adsarea.hTable,
        static_cast<UNSIGNED8 *>(HB_UNCONST(pOrderInfo->abBagName)),
        static_cast<UNSIGNED8 *>(HB_UNCONST(pOrderInfo->atomBagName)),
        szKeyExpr[0] ? szKeyExpr : reinterpret_cast<UNSIGNED8 *>(const_cast<char *>("1")),
        bForADS ? static_cast<UNSIGNED8 *>(pArea->adsarea.area.lpdbOrdCondInfo->abFor) : nullptr,
        bWhileADS ? static_cast<UNSIGNED8 *>(pArea->adsarea.area.lpdbOrdCondInfo->abWhile) : nullptr, ADS_COMPOUND,
        &hIndex);
#endif

    if (u32RetVal != AE_SUCCESS)
    {
      hb_mixErrorRT(pArea, EG_CREATE, static_cast<HB_ERRCODE>(u32RetVal), pOrderInfo->atomBagName, 0, 0);
      return Harbour::FAILURE;
    }

    pArea->adsarea.area.lpdbOrdCondInfo->fUseCurrent = true;

    // If while condition is already used, remove it from OrdCondInfo
    if (bWhileADS && pArea->adsarea.area.lpdbOrdCondInfo->abWhile)
    {
      hb_xfree(pArea->adsarea.area.lpdbOrdCondInfo->abWhile);
      pArea->adsarea.area.lpdbOrdCondInfo->abWhile = nullptr;
      if (pArea->adsarea.area.lpdbOrdCondInfo->itmCobWhile)
      {
        hb_itemRelease(pArea->adsarea.area.lpdbOrdCondInfo->itmCobWhile);
        pArea->adsarea.area.lpdbOrdCondInfo->itmCobWhile = nullptr;
      }
      if (pArea->adsarea.area.lpdbOrdCondInfo->itmStartRecID)
      {
        hb_itemRelease(pArea->adsarea.area.lpdbOrdCondInfo->itmStartRecID);
        pArea->adsarea.area.lpdbOrdCondInfo->itmStartRecID = nullptr;
      }
      pArea->adsarea.area.lpdbOrdCondInfo->fRest = false;
    }
  }

  // Obtain key codeblock
  if (pOrderInfo->itmCobExpr)
  {
    pKeyItem = hb_itemNew(pOrderInfo->itmCobExpr);
  }
  else
  {
    if (SELF_COMPILE(&pArea->adsarea.area, hb_itemGetCPtr(pOrderInfo->abExpr)) == Harbour::FAILURE)
    {
      if (hIndex)
      {
        AdsDeleteIndex(hIndex);
      }
      return Harbour::FAILURE;
    }
    pKeyItem = pArea->adsarea.area.valResult;
    pArea->adsarea.area.valResult = nullptr;
  }

  // Test key codeblock on EOF
  ulRecNo = pArea->adsarea.ulRecNo;
  SELF_GOTO(&pArea->adsarea.area, 0);
  if (SELF_EVALBLOCK(&pArea->adsarea.area, pKeyItem) == Harbour::FAILURE)
  {
    if (hIndex)
    {
      AdsDeleteIndex(hIndex);
    }
    hb_vmDestroyBlockOrMacro(pKeyItem);
    SELF_GOTO(&pArea->adsarea.area, ulRecNo);
    return Harbour::FAILURE;
  }

  pResult = pArea->adsarea.area.valResult;
  pArea->adsarea.area.valResult = nullptr;

  switch (hb_itemType(pResult))
  {
  case Harbour::Item::STRING:
  case Harbour::Item::MEMO:
    bType = 'C';
    uiLen = static_cast<HB_USHORT>(hb_itemGetCLen(pResult));
    if (uiLen > MIX_MAXKEYLEN)
    {
      uiLen = MIX_MAXKEYLEN;
    }
    break;

  case Harbour::Item::INTEGER:
  case Harbour::Item::LONG:
  case Harbour::Item::DOUBLE:
    bType = 'N';
    uiLen = 8;
    break;

  case Harbour::Item::DATE:
    bType = 'D';
    uiLen = 8;
    break;

  case Harbour::Item::LOGICAL:
    bType = 'L';
    uiLen = 1;
    break;

  default:
    bType = 'U';
    uiLen = 0;
  }
  hb_itemRelease(pResult);

  if (bType == 'U' || uiLen == 0)
  {
    if (hIndex)
    {
      AdsDeleteIndex(hIndex);
    }
    hb_vmDestroyBlockOrMacro(pKeyItem);
    SELF_GOTO(&pArea->adsarea.area, ulRecNo);
    hb_mixErrorRT(pArea, bType == 'U' ? EG_DATATYPE : EG_DATAWIDTH, 1026, nullptr, 0, 0);
    return Harbour::FAILURE;
  }

  if (pArea->adsarea.area.lpdbOrdCondInfo)
  {
    // Obtain FOR codeblock
    if (pArea->adsarea.area.lpdbOrdCondInfo->itmCobFor)
    {
      pForItem = hb_itemNew(pArea->adsarea.area.lpdbOrdCondInfo->itmCobFor);
    }
    else if (pArea->adsarea.area.lpdbOrdCondInfo->abFor)
    {
      if (SELF_COMPILE(&pArea->adsarea.area, pArea->adsarea.area.lpdbOrdCondInfo->abFor) == Harbour::FAILURE)
      {
        if (hIndex)
        {
          AdsDeleteIndex(hIndex);
        }
        hb_vmDestroyBlockOrMacro(pKeyItem);
        SELF_GOTO(&pArea->adsarea.area, ulRecNo);
        return Harbour::FAILURE;
      }
      pForItem = pArea->adsarea.area.valResult;
      pArea->adsarea.area.valResult = nullptr;
    }

    // Obtain WHILE codeblock
    if (pArea->adsarea.area.lpdbOrdCondInfo->itmCobWhile)
    {
      pWhileItem = hb_itemNew(pArea->adsarea.area.lpdbOrdCondInfo->itmCobWhile);
    }
    else if (pArea->adsarea.area.lpdbOrdCondInfo->abWhile)
    {
      if (SELF_COMPILE(&pArea->adsarea.area, pArea->adsarea.area.lpdbOrdCondInfo->abWhile) == Harbour::FAILURE)
      {
        if (hIndex)
        {
          AdsDeleteIndex(hIndex);
        }
        hb_vmDestroyBlockOrMacro(pKeyItem);
        if (pForItem)
        {
          hb_vmDestroyBlockOrMacro(pForItem);
        }
        SELF_GOTO(&pArea->adsarea.area, ulRecNo);
        return Harbour::FAILURE;
      }
      pWhileItem = pArea->adsarea.area.valResult;
      pArea->adsarea.area.valResult = nullptr;
    }
  }

  // Test FOR codeblock on EOF
  if (pForItem)
  {
    if (SELF_EVALBLOCK(&pArea->adsarea.area, pForItem) == Harbour::FAILURE)
    {
      if (hIndex)
      {
        AdsDeleteIndex(hIndex);
      }
      hb_vmDestroyBlockOrMacro(pKeyItem);
      hb_vmDestroyBlockOrMacro(pForItem);
      if (pWhileItem)
      {
        hb_vmDestroyBlockOrMacro(pWhileItem);
      }
      SELF_GOTO(&pArea->adsarea.area, ulRecNo);
      return Harbour::FAILURE;
    }
    if (hb_itemType(pArea->adsarea.area.valResult) != Harbour::Item::LOGICAL)
    {
      if (hIndex)
      {
        AdsDeleteIndex(hIndex);
      }
      hb_itemRelease(pArea->adsarea.area.valResult);
      pArea->adsarea.area.valResult = 0;
      hb_vmDestroyBlockOrMacro(pKeyItem);
      hb_vmDestroyBlockOrMacro(pForItem);
      if (pWhileItem)
      {
        hb_vmDestroyBlockOrMacro(pWhileItem);
      }
      SELF_GOTO(&pArea->adsarea.area, ulRecNo);
      hb_mixErrorRT(pArea, EG_DATATYPE, EDBF_INVALIDFOR, nullptr, 0, 0);
      return Harbour::FAILURE;
    }
    hb_itemRelease(pArea->adsarea.area.valResult);
    pArea->adsarea.area.valResult = nullptr;
  }

  // TODO: WHILE condition is not tested, like in DBFCDX. Why? Compatibility with Clipper?

  SELF_GOTO(&pArea->adsarea.area, ulRecNo);

  // Set auxiliary index as current for subindexing
  if (hIndex)
  {
    pArea->adsarea.hOrdCurrent = hIndex;
  }

  pTagNew =
      mixTagCreate(pOrderInfo->atomBagName, pOrderInfo->abExpr, pKeyItem, pForItem, pWhileItem, bType, uiLen, pArea);

  pArea->adsarea.hOrdCurrent = 0;
  if (hIndex)
  {
    AdsDeleteIndex(hIndex);
  }

  if (pWhileItem)
  {
    hb_vmDestroyBlockOrMacro(pWhileItem);
  }

  // Append the tag to the end of list
  if (pArea->pTagList)
  {
    pTag = pArea->pTagList;
    while (pTag->pNext)
    {
      pTag = pTag->pNext;
    }
    pTag->pNext = pTagNew;
  }
  else
  {
    pArea->pTagList = pTagNew;
  }
  pArea->pTagCurrent = pTagNew;
  return Harbour::SUCCESS;
}

static HB_ERRCODE adsxOrderDestroy(ADSXAREAP pArea, LPDBORDERINFO pOrderInfo)
{
  PMIXTAG pTag, pTag2;

  // TODO: ADS RDD missing implementation of ordDestroy( nOrder )
  if (SUPER_ORDDESTROY(&pArea->adsarea.area, pOrderInfo) == Harbour::SUCCESS)
  {
    return Harbour::SUCCESS;
  }

  pTag = mixFindTag(pArea, pOrderInfo->itmOrder);

  if (pTag)
  {
    if (pTag == pArea->pTagList)
    {
      pArea->pTagList = pTag->pNext;
    }
    else
    {
      pTag2 = pArea->pTagList;
      while (pTag2->pNext != pTag)
      {
        pTag2 = pTag2->pNext;
      }
      pTag2->pNext = pTag->pNext;
    }

    if (pTag == pArea->pTagCurrent)
    {
      pArea->pTagCurrent = nullptr;
    }

    mixTagDestroy(pTag);
    return Harbour::SUCCESS;
  }
  else
  {
    pArea->adsarea.hOrdCurrent = 0;
    return Harbour::FAILURE;
  }
}

static HB_ERRCODE adsxOrderInfo(ADSXAREAP pArea, HB_USHORT uiIndex, LPDBORDERINFO pOrderInfo)
{
  PMIXTAG pTag = pArea->pTagCurrent;

  // resolve any pending relations
  if (pArea->adsarea.lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->adsarea.area);
  }

  // all others need an index handle
  if (uiIndex != DBOI_ORDERCOUNT)
  {
    if (pOrderInfo->itmOrder)
    {
      if (pOrderInfo->itmOrder->isString())
      {
        pTag = pArea->pTagList;
        while (pTag)
        {
          if (!hb_stricmp(hb_itemGetCPtr(pOrderInfo->itmOrder), pTag->szName))
          {
            break;
          }

          pTag = pTag->pNext;
        }
      }
      else if (pOrderInfo->itmOrder->isNumeric())
      {
        UNSIGNED16 usOrder = 0, usSearch = static_cast<UNSIGNED16>(hb_itemGetNI(pOrderInfo->itmOrder));

        AdsGetNumIndexes(pArea->adsarea.hTable, &usOrder);

        pTag = usSearch <= usOrder ? nullptr : pArea->pTagList;
        while (pTag)
        {
          if (++usOrder == usSearch)
          {
            break;
          }

          pTag = pTag->pNext;
        }
      }
    }

    if (!pTag)
    {
      return SUPER_ORDINFO(&pArea->adsarea.area, uiIndex, pOrderInfo);
    }
  }

  switch (uiIndex)
  {
  case DBOI_CONDITION:
    pOrderInfo->itmResult = hb_itemPutC(pOrderInfo->itmResult, pTag->szForExpr);
    break;

  case DBOI_EXPRESSION:
    pOrderInfo->itmResult = hb_itemPutC(pOrderInfo->itmResult, pTag->szKeyExpr);
    break;

  case DBOI_ISCOND:
    pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, pTag->pForItem != nullptr);
    break;

  case DBOI_ISDESC:
    pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, false);
    break;

  case DBOI_UNIQUE:
    pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, false);
    break;

  case DBOI_KEYTYPE:
    pOrderInfo->itmResult = hb_itemPutCL(pOrderInfo->itmResult, reinterpret_cast<char *>(&pTag->bType), 1);
    break;

  case DBOI_KEYSIZE:
    pOrderInfo->itmResult = hb_itemPutNI(pOrderInfo->itmResult, pTag->uiLen);
    break;

  case DBOI_KEYVAL: {
    auto pCodepage = hb_cdpSelect(pArea->adsarea.area.cdPage);

    auto pItem = hb_vmEvalBlockOrMacro(pTag->pKeyItem);
    hb_cdpSelect(pCodepage);
    if (!pOrderInfo->itmResult)
    {
      pOrderInfo->itmResult = hb_itemNew(nullptr);
    }
    hb_itemMove(pOrderInfo->itmResult, pItem);
    break;
  }
  case DBOI_KEYCOUNT:
  case DBOI_KEYCOUNTRAW: // ignore filter but RESPECT SCOPE
    pOrderInfo->itmResult = hb_itemPutNL(pOrderInfo->itmResult, pTag->ulRecCount);
    break;

  case DBOI_POSITION:
  case DBOI_RECNO:
  case DBOI_KEYNORAW:
    if (uiIndex == DBOI_POSITION && pOrderInfo->itmNewVal && pOrderInfo->itmNewVal->isNumeric())
    {
      HB_ULONG ulPos;

      ulPos = hb_itemGetNL(pOrderInfo->itmNewVal);

      if (ulPos > 0 && ulPos <= pTag->ulRecCount)
      {
        SELF_GOTO(&pArea->adsarea.area, pTag->pKeys[ulPos - 1]->rec);
      }

      pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, !pArea->adsarea.area.fEof);
    }
    else
    {
      PMIXKEY pKey;
      HB_ULONG ulKeyPos;

      if (!pArea->adsarea.fPositioned)
      {
        SELF_GOTO(&pArea->adsarea.area, pArea->adsarea.ulRecNo);
      }
      else
      {
        pArea->adsarea.area.fEof = false;
      }

      pKey = mixKeyEval(pTag, pArea);

      pOrderInfo->itmResult =
          hb_itemPutNL(pOrderInfo->itmResult, mixFindKey(pTag, pKey, &ulKeyPos) ? (ulKeyPos + 1) : 0);
      mixKeyFree(pKey);
    }
    break;

  case DBOI_RELKEYPOS:
    if (pOrderInfo->itmNewVal && pOrderInfo->itmNewVal->isNumeric())
    {
      auto ulPos = static_cast<HB_ULONG>(hb_itemGetND(pOrderInfo->itmNewVal) * static_cast<double>(pTag->ulRecCount));

      if (ulPos > 0 && ulPos <= pTag->ulRecCount)
      {
        SELF_GOTO(&pArea->adsarea.area, pTag->pKeys[ulPos - 1]->rec);
      }

      pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, !pArea->adsarea.area.fEof);
    }
    else
    {
      PMIXKEY pKey;
      HB_ULONG ulKeyPos;

      if (!pArea->adsarea.fPositioned)
      {
        SELF_GOTO(&pArea->adsarea.area, pArea->adsarea.ulRecNo);
      }
      else
      {
        pArea->adsarea.area.fEof = false;
      }

      pKey = mixKeyEval(pTag, pArea);

      if (!mixFindKey(pTag, pKey, &ulKeyPos))
      {
        ulKeyPos = 0;
      }

      mixKeyFree(pKey);

      pOrderInfo->itmResult =
          hb_itemPutND(pOrderInfo->itmResult, static_cast<double>(ulKeyPos) / static_cast<double>(pTag->ulRecCount));
    }
    break;

  case DBOI_NAME:
    pOrderInfo->itmResult = hb_itemPutC(pOrderInfo->itmResult, pTag->szName);
    break;

  case DBOI_BAGNAME:
    pOrderInfo->itmResult = hb_itemPutC(pOrderInfo->itmResult, nullptr);
    break;

  case DBOI_FULLPATH:
    pOrderInfo->itmResult = hb_itemPutC(pOrderInfo->itmResult, nullptr);
    break;

  case DBOI_BAGEXT:
    pOrderInfo->itmResult = hb_itemPutC(pOrderInfo->itmResult, "mix");
    break;

  case DBOI_ORDERCOUNT: {
    UNSIGNED16 usOrder = 0;

    AdsGetNumIndexes(pArea->adsarea.hTable, &usOrder);
    pTag = pArea->pTagList;
    while (pTag)
    {
      pTag = pTag->pNext;
      usOrder++;
    }
    pOrderInfo->itmResult = hb_itemPutNI(pOrderInfo->itmResult, static_cast<int>(usOrder));
    break;
  }

  case DBOI_NUMBER: {
    PMIXTAG pTag2;
    UNSIGNED16 usOrder = 0;

    AdsGetNumIndexes(pArea->adsarea.hTable, &usOrder);
    pTag2 = pArea->pTagList;
    usOrder++;
    while (pTag2 && pTag != pTag2)
    {
      pTag2 = pTag2->pNext;
      usOrder++;
    }
    pOrderInfo->itmResult = hb_itemPutNI(pOrderInfo->itmResult, static_cast<int>(usOrder));
    break;
  }

  case DBOI_CUSTOM:
    pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, false);
    break;

  case DBOI_OPTLEVEL:
    pOrderInfo->itmResult = hb_itemPutNI(pOrderInfo->itmResult, DBOI_OPTIMIZED_NONE);
    break;

  case DBOI_KEYADD:
    pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, false);
    break;

  case DBOI_KEYDELETE:
    pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, false);
    break;

  case DBOI_AUTOOPEN:
    pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, false);
    break;

  default:
    return SUPER_ORDINFO(&pArea->adsarea.area, uiIndex, pOrderInfo);
  }
  return Harbour::SUCCESS;
}

// clang-format off
static RDDFUNCS adsxTable = { nullptr,
                              nullptr,
                              nullptr,
                              ( DBENTRYP_V ) adsxGoBottom,
                              nullptr,
                              nullptr,
                              ( DBENTRYP_V ) adsxGoTop,
                              ( DBENTRYP_BIB ) adsxSeek,
                              ( DBENTRYP_L ) adsxSkip,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              ( DBENTRYP_SI ) adsxPutValue,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              ( DBENTRYP_V ) adsxClose,
                              ( DBENTRYP_VO ) adsxCreate,
                              nullptr,
                              ( DBENTRYP_V ) adsxNewArea,
                              ( DBENTRYP_VO ) adsxOpen,
                              nullptr,
                              ( DBENTRYP_SP ) adsxStructSize,
                              ( DBENTRYP_CP ) adsxSysName,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              ( DBENTRYP_VOI ) adsxOrderListFocus,
                              nullptr,
                              nullptr,
                              ( DBENTRYP_VOC ) adsxOrderCreate,
                              ( DBENTRYP_VOI ) adsxOrderDestroy,
                              ( DBENTRYP_SVOI ) adsxOrderInfo,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr,
                              nullptr };
// clang-format on

static void adsxRegisterRDD(HB_USHORT *pusRddId, const char *szRddName)
{
  auto puiCount = static_cast<HB_USHORT *>(hb_parptr(1));
  auto pTable = static_cast<RDDFUNCS *>(hb_parptr(2));
  auto uiRddId = static_cast<HB_USHORT>(hb_parni(4));
  auto puiSuperRddId = static_cast<HB_USHORT *>(hb_parptr(5));

  if (pTable)
  {
    HB_ERRCODE errCode;

    if (puiCount)
    {
      *puiCount = RDDFUNCSCOUNT;
    }

    errCode = hb_rddInheritEx(pTable, &adsxTable, &adsxSuper, szRddName, puiSuperRddId);
    if (errCode == Harbour::SUCCESS)
    {
      *pusRddId = uiRddId;
    }
    hb_retni(errCode);
  }
  else
  {
    hb_retni(Harbour::FAILURE);
  }
}

HB_FUNC_STATIC(ADSX_GETFUNCTABLE)
{
  adsxRegisterRDD(&s_uiRddIdADSX, "ADS");
}

HB_FUNC_STATIC(ADSNTXX_GETFUNCTABLE)
{
  adsxRegisterRDD(&s_uiRddIdADSNTXX, "ADSNTX");
}

HB_FUNC_STATIC(ADSCDXX_GETFUNCTABLE)
{
  adsxRegisterRDD(&s_uiRddIdADSCDXX, "ADSCDX");
}

#if ADS_LIB_VERSION >= 900
HB_FUNC_STATIC(ADSVFPX_GETFUNCTABLE)
{
  adsxRegisterRDD(&s_uiRddIdADSVFPX, "ADSVFP");
}
#endif

HB_FUNC_STATIC(ADSADTX_GETFUNCTABLE)
{
  adsxRegisterRDD(&s_uiRddIdADSADTX, "ADSADT");
}

HB_FUNC(ADSX)
{
  ;
}
HB_FUNC(ADSNTXX)
{
  ;
}
HB_FUNC(ADSCDXX)
{
  ;
}
HB_FUNC(ADSVFPX)
{
  ;
}
HB_FUNC(ADSADTX)
{
  ;
}

HB_FUNC_EXTERN(ADSCDX);

// clang-format off
static void hb_adsxRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED(cargo);

   if( hb_rddRegister("ADSX",    RDT_FULL) > 1 ||
       hb_rddRegister("ADSNTXX", RDT_FULL) > 1 ||
       hb_rddRegister("ADSCDXX", RDT_FULL) > 1 ||
#if ADS_LIB_VERSION >= 900
       hb_rddRegister("ADSVFPX", RDT_FULL) > 1 ||
#endif
       hb_rddRegister("ADSADTX", RDT_FULL) > 1 ) {
      // try different RDD register order
      hb_rddRegister("ADS",    RDT_FULL);
      hb_rddRegister("ADSNTX", RDT_FULL);
      hb_rddRegister("ADSCDX", RDT_FULL);
#if ADS_LIB_VERSION >= 900
      hb_rddRegister("ADSVFP", RDT_FULL);
#endif
      hb_rddRegister("ADSADT", RDT_FULL);

      if( hb_rddRegister("ADSX",    RDT_FULL) > 1 ||
          hb_rddRegister("ADSNTXX", RDT_FULL) > 1 ||
          hb_rddRegister("ADSCDXX", RDT_FULL) > 1 ||
#if ADS_LIB_VERSION >= 900
          hb_rddRegister("ADSVFPX", RDT_FULL) > 1 ||
#endif
          hb_rddRegister("ADSADTX", RDT_FULL) > 1 ) {
         hb_errInternal(HB_EI_RDDINVALID, nullptr, nullptr, nullptr);
         // not executed, only to force linking ADS RDD
         HB_FUNC_EXEC(ADSCDX);
      }
   }
}
// clang-format on

// clang-format off
HB_INIT_SYMBOLS_BEGIN( adsx1__InitSymbols )
{ "ADSX",                 {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSX )}, nullptr },
{ "ADSX_GETFUNCTABLE",    {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSX_GETFUNCTABLE )}, nullptr },
{ "ADSNTXX",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSNTXX )}, nullptr },
{ "ADSNTXX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSNTXX_GETFUNCTABLE )}, nullptr },
{ "ADSCDXX",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSCDXX )}, nullptr },
{ "ADSCDXX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSCDXX_GETFUNCTABLE )}, nullptr },
#if ADS_LIB_VERSION >= 900
{ "ADSVFPX",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSVFPX )}, nullptr },
{ "ADSVFPX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSVFPX_GETFUNCTABLE )}, nullptr },
#endif
{ "ADSADTX",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSADTX )}, nullptr },
{ "ADSADTX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSADTX_GETFUNCTABLE )}, nullptr }
HB_INIT_SYMBOLS_END( adsx1__InitSymbols )
// clang-format on

// clang-format off
HB_CALL_ON_STARTUP_BEGIN(_hb_adsx_rdd_init_)
  hb_vmAtInit(hb_adsxRddInit, nullptr);
HB_CALL_ON_STARTUP_END(_hb_adsx_rdd_init_)
// clang-format on

// clang-format off
#if defined(HB_PRAGMA_STARTUP)
   #pragma startup adsx1__InitSymbols
   #pragma startup _hb_adsx_rdd_init_
#elif defined(HB_DATASEG_STARTUP)
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC(adsx1__InitSymbols) \
                              HB_DATASEG_FUNC(_hb_adsx_rdd_init_)
   #include "hbiniseg.hpp"
#endif
// clang-format on
