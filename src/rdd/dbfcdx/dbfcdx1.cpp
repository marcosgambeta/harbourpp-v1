//
// DBFCDX RDD (ver.2)
//
// Copyright 1999-2002 Bruno Cantero <bruno@issnet.net>
// Copyright 2000-2003 Horacio Roldan <harbour_ar@yahoo.com.ar> (portions)
// Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl> - all code except
// hb_cdxTagDoIndex and related hb_cdxSort* rewritten.
// Copyright 2004 Przemyslaw Czerpak <druzus@priv.onet.pl> - rest of code rewritten
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

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

#define HB_CDX_NEW_SORT

#if !defined(HB_SIXCDX)
#define HB_CDX_PACKTRAIL
#endif

#define HB_CDX_DBGCODE
#if 0
#define HB_CDX_DBGCODE_EXT
#define HB_CDX_DSPDBG_INFO
#define HB_CDX_DBGTIME
#define HB_CDX_DBGUPDT
#endif

#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbinit.hpp"
#include "hbapierr.hpp"
#include "hbapilng.hpp"
#include "hbvm.hpp"
#include "hbset.hpp"
#include "hbstack.hpp"
#include "hbrddcdx.hpp"
#include "hbmath.hpp"
#include "rddsys.ch"
#include "hbregex.hpp"
#include "hbapicdp.hpp"

#define hb_cdxFilePageOffset(I, B) (static_cast<HB_FOFFSET>(B) << ((I)->fLargeFile ? (I)->uiPageBits : 0))
#define hb_cdxFilePageNum(I, O) (static_cast<HB_ULONG>((O) >> ((I)->fLargeFile ? (I)->uiPageBits : 0)))
#define hb_cdxFilePageNext(I, C) ((C) << ((I)->fLargeFile ? 0 : (I)->uiPageBits))
#define hb_cdxFilePageRootValid(I, B) ((I)->fLargeFile ? (B) != CDX_DUMMYNODE : ((B) % (I)->uiPageLen == 0))
#define hb_cdxPageKeyBufPtr(P) ((reinterpret_cast<HB_BYTE *>(&(P)->node)) + (P)->TagParent->pIndex->uiPageLen)
#define hb_cdxPageIntKeyPool(P) ((reinterpret_cast<HB_BYTE *>(&(P)->node)) + CDX_INT_HEADSIZE)
#define hb_cdxPageExtKeyPool(P) ((reinterpret_cast<HB_BYTE *>(&(P)->node)) + CDX_EXT_HEADSIZE)

// Tag->fRePos = HB_TRUE means that rootPage->...->childLeafPage path is
// bad and has to be reloaded
// CurKey->rec == 0 means that there is no correct CurKey

// create a new Tag (make index)
static void hb_cdxTagDoIndex(LPCDXTAG pTag, bool fReindex);

// Close Tag
static void hb_cdxTagClose(LPCDXTAG pTag);

// free Tag pages from cache
static void hb_cdxTagPoolFree(LPCDXTAG pTag, int nPagesLeft);

// Store tag header to index files
static void hb_cdxTagHeaderStore(LPCDXTAG pTag);

// write all changed pages in tag cache
static void hb_cdxTagPoolFlush(LPCDXTAG pTag);

// Discard all pages in cache (TagClose and TagPoolFree for all Tags)
static void hb_cdxIndexDiscardBuffers(LPCDXINDEX pIndex);

// write all changed pages in cache (pagePool and Tag Header)
static void hb_cdxIndexFlushBuffers(LPCDXINDEX pIndex);

// free cached pages of index file
static void hb_cdxIndexPoolFree(LPCDXINDEX pIndex, int nPagesLeft);

// split Root Page
static int hb_cdxPageRootSplit(LPCDXPAGE pPage);

// free create index structure
static void hb_cdxSortFree(LPCDXSORTINFO pSort);

static HB_USHORT s_uiRddId = static_cast<HB_USHORT>(-1);

static RDDFUNCS cdxSuper;

#ifdef HB_CDX_DSPDBG_INFO
static void hb_cdxDspTags(LPCDXINDEX pIndex)
{
  LPCDXTAG pTag = nullptr;

  fprintf(stderr, "\r\n*TAGS*");
  while (pIndex)
  {
    fprintf(stderr, "\r\nBAG: [%s] ->", pIndex->szFileName);
    pTag = pIndex->TagList;
    while (pTag != nullptr)
    {
      fprintf(stderr, " {%s}", pTag->szName);
      pTag = pTag->pNext;
    }
    pIndex = pIndex->pNext;
  }
  fprintf(stderr, "\r\n*END*\r\n");
  fflush(stderr);
}
#endif

#ifdef HB_CDX_DBGTIME
#include <sys/time.h>
using CDXDBGTIME = HB_LONGLONG;

static CDXDBGTIME cdxTimeIntBld = 0;
static CDXDBGTIME cdxTimeExtBld = 0;
static CDXDBGTIME cdxTimeIntBlc = 0;
static CDXDBGTIME cdxTimeExtBlc = 0;
static CDXDBGTIME cdxTimeGetKey = 0;
static CDXDBGTIME cdxTimeFreeKey = 0;
static CDXDBGTIME cdxTimeIdxBld = 0;

static CDXDBGTIME hb_cdxGetTime()
{
  struct timeval tv;

  gettimeofday(&tv, nullptr);
  return static_cast<CDXDBGTIME>(tv.tv_sec) * 1000000 + static_cast<CDXDBGTIME>(tv.tv_usec);
}
#endif
#ifdef HB_CDX_DBGUPDT
static HB_ULONG cdxWriteNO = 0;
static HB_ULONG cdxReadNO = 0;
static HB_SHORT cdxStackSize = 0;
static HB_SHORT cdxTmpStackSize = 0;
#endif

// internal DBFCDX function

// generate internal error
static void hb_cdxErrInternal(const char *szMsg)
{
  hb_errInternal(9201, szMsg ? szMsg : "hb_cdxErrInternal: data integrity error.", nullptr, nullptr);
}

// generate Run-Time error
static HB_ERRCODE hb_cdxErrorRT(CDXAREAP pArea, HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char *filename,
                                HB_ERRCODE errOsCode, HB_USHORT uiFlags, PHB_ITEM *pErrorPtr)
{
  HB_ERRCODE iRet = Harbour::FAILURE;

  if (hb_vmRequestQuery() == 0)
  {
    PHB_ITEM pError;
    if (pErrorPtr)
    {
      if (!*pErrorPtr)
      {
        *pErrorPtr = hb_errNew();
      }
      pError = *pErrorPtr;
    }
    else
    {
      pError = hb_errNew();
    }
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
    iRet = SELF_ERROR(&pArea->dbfarea.area, pError);
    if (!pErrorPtr)
    {
      hb_errRelease(pError);
    }
  }
  return iRet;
}

// create index sort table
static void hb_cdxMakeSortTab(CDXAREAP pArea)
{
  if (pArea->dbfarea.area.cdPage && !HB_CDP_ISBINSORT(pArea->dbfarea.area.cdPage) &&
      !(pArea->fSortCDP || pArea->sortTab))
  {
    pArea->sortTab = hb_cdpGetSortTab(pArea->dbfarea.area.cdPage);
    if (!pArea->sortTab)
    {
      pArea->fSortCDP = true;
    }
  }
}

// create new index key
static LPCDXKEY hb_cdxKeyNew(HB_USHORT uiLen)
{
  auto pKey = static_cast<LPCDXKEY>(hb_xgrabz(sizeof(CDXKEY) + uiLen));
  pKey->len = uiLen;

  return pKey;
}

// Free index key
static void hb_cdxKeyFree(LPCDXKEY pKey)
{
  hb_xfree(pKey);
}

// copy index key, if dst is null create new dst key else destroy dst
static LPCDXKEY hb_cdxKeyCopy(LPCDXKEY pKeyDest, LPCDXKEY pKey)
{
  if (!pKeyDest)
  {
    pKeyDest = hb_cdxKeyNew(pKey->len);
  }
  else if (pKeyDest->len != pKey->len)
  {
    pKeyDest = static_cast<LPCDXKEY>(hb_xrealloc(pKeyDest, sizeof(CDXKEY) + pKey->len));
  }

  return static_cast<LPCDXKEY>(memcpy(pKeyDest, pKey, sizeof(CDXKEY) + pKey->len));
}

// store bytes value in index key
static LPCDXKEY hb_cdxKeyPut(LPCDXKEY pKey, const HB_BYTE *pbVal, HB_USHORT uiLen, HB_ULONG ulRec)
{
  if (pbVal == nullptr)
  {
    uiLen = 0;
  }

  if (!pKey)
  {
    pKey = hb_cdxKeyNew(uiLen);
  }
  else if (pKey->len != uiLen)
  {
    pKey = static_cast<LPCDXKEY>(hb_xrealloc(pKey, sizeof(CDXKEY) + uiLen));
    pKey->len = uiLen;
  }

  if (uiLen)
  {
    memcpy(pKey->val, pbVal, uiLen);
  }
  pKey->val[uiLen] = '\0';

  pKey->mode = CDX_CMP_EXACT;
  pKey->rec = ulRec;

  return pKey;
}

// store string value in index key
static LPCDXKEY hb_cdxKeyPutCL(LPCDXKEY pKey, const char *pText, HB_SIZE nLen, HB_ULONG ulRec, HB_USHORT uiKeyLen,
                               int iMode)
{
  if (!pKey)
  {
    pKey = hb_cdxKeyNew(uiKeyLen);
  }
  else if (pKey->len != uiKeyLen)
  {
    pKey = static_cast<LPCDXKEY>(hb_xrealloc(pKey, sizeof(CDXKEY) + uiKeyLen));
    pKey->len = uiKeyLen;
  }

  if (nLen > static_cast<HB_SIZE>(uiKeyLen))
  {
    nLen = uiKeyLen;
  }
  else if (nLen < static_cast<HB_SIZE>(uiKeyLen))
  {
    memset(&pKey->val[nLen], ' ', static_cast<HB_SIZE>(uiKeyLen) - nLen);
  }
  if (nLen)
  {
    memcpy(pKey->val, pText, nLen);
  }
  pKey->val[uiKeyLen] = '\0';

  pKey->mode = static_cast<HB_USHORT>(iMode);
  pKey->rec = ulRec;

  return pKey;
}

// compare two values using Tag conditions (len & type)
static int hb_cdxValCompare(LPCDXTAG pTag, const HB_BYTE *val1, int len1, const HB_BYTE *val2, int len2, int iMode)
{
  int iLimit, iResult = 0;

  iLimit = (len1 > len2) ? len2 : len1;

  if (pTag->uiType == 'C')
  {
    if (iLimit > 0)
    {
      if (pTag->pIndex->pArea->sortTab)
      {
        const HB_UCHAR *sortTab = pTag->pIndex->pArea->sortTab;
        int iPos = 0;
        while (iPos < iLimit)
        {
          iResult = sortTab[val1[iPos]] - sortTab[val2[iPos]];
          if (iResult != 0)
          {
            break;
          }
          iPos++;
        }
      }
      else if (pTag->pIndex->pArea->fSortCDP)
      {
        return -hb_cdpcmp(reinterpret_cast<const char *>(val2), static_cast<HB_SIZE>(len2),
                          reinterpret_cast<const char *>(val1), static_cast<HB_SIZE>(len1),
                          pTag->pIndex->pArea->dbfarea.area.cdPage, 0);
      }
      else
      {
        iResult = memcmp(val1, val2, iLimit);
      }
    }

    if (iResult == 0)
    {
      if (len1 > len2)
      {
        iResult = 1;
      }
      else if (len1 < len2 && iMode == CDX_CMP_EXACT)
      {
        iResult = -1;
      }
    }
    else if (iResult > 0)
    {
      iResult = 1;
    }
    else
    {
      iResult = -1;
    }
  }
  else if (iMode == CDX_CMP_DATE && iLimit == 8)
  {
    double d1, d2;
    long l;

    HB_ORD2DBL(val1, &d1);
    HB_ORD2DBL(val2, &d2);
    l = static_cast<long>(d1) - static_cast<long>(d2);
    if (l < 0)
    {
      iResult = -1;
    }
    else if (l > 0)
    {
      iResult = 1;
    }
  }
  else
  {
    if (iLimit == 0 || (iResult = memcmp(val1, val2, iLimit)) == 0)
    {
      if (len1 > len2)
      {
        iResult = 1;
      }
      else if (len1 < len2)
      {
        iResult = -1;
      }
    }
    else if (iResult > 0)
    {
      iResult = 1;
    }
    else
    {
      iResult = -1;
    }
  }
  return iResult;
}

// get CDX key type for given item
static HB_BYTE hb_cdxItemType(PHB_ITEM pItem)
{
  switch (hb_itemType(pItem))
  {
  case Harbour::Item::STRING:
  case Harbour::Item::MEMO:
    return 'C';

  case Harbour::Item::INTEGER:
  case Harbour::Item::LONG:
  case Harbour::Item::DOUBLE:
    return 'N';

  case Harbour::Item::DATE:
    return 'D';

  case Harbour::Item::TIMESTAMP:
    return 'T';

  case Harbour::Item::LOGICAL:
    return 'L';

  default:
    return 'U';
  }
}

// convert internal type of key expression to comparable type
static HB_BYTE hb_cdxItemTypeCmp(HB_BYTE bType)
{
  return bType == 'T' ? 'D' : bType;
}

// store Item in index key
// TODO: uiType check and generate RT error if necessary
static LPCDXKEY hb_cdxKeyPutItem(LPCDXKEY pKey, PHB_ITEM pItem, HB_ULONG ulRec, LPCDXTAG pTag, int iMode)
{
  HB_BYTE buf[CDX_MAXKEY];
  const HB_BYTE *ptr;
  HB_SIZE nLen;
  double d;

  ptr = &buf[0];

  switch (hb_cdxItemType(pItem))
  {
  case 'C':
  {
    HB_SIZE nDestLen = pTag->uiLen;
    char *pFree = nullptr, *pDest;
    auto cdpVM = hb_vmCDP();

    auto pText = pItem->getCPtr();
    nLen = pItem->getCLen();

    if (cdpVM != pTag->pIndex->pArea->dbfarea.area.cdPage)
    {
      if (nDestLen <= sizeof(buf))
      {
        pDest = reinterpret_cast<char *>(buf);
      }
      else
      {
        pDest = pFree = static_cast<char *>(hb_xgrab(nDestLen));
      }
      hb_cdpnDup2(pText, nLen, pDest, &nDestLen, cdpVM, pTag->pIndex->pArea->dbfarea.area.cdPage);
      pText = pDest;
      nLen = nDestLen;
      nDestLen = pTag->uiLen;
    }

    if (pTag->IgnoreCase)
    {
      if (pText != reinterpret_cast<char *>(buf) && nDestLen <= sizeof(buf))
      {
        pDest = reinterpret_cast<char *>(buf);
      }
      else
      {
        pDest = static_cast<char *>(hb_xgrab(nDestLen));
      }

      nLen = hb_cdpnDup2Upper(pTag->pIndex->pArea->dbfarea.area.cdPage, pText, nLen, pDest, nDestLen);
      pText = pDest;
      if (pDest != reinterpret_cast<char *>(buf))
      {
        if (pFree)
        {
          hb_xfree(pFree);
        }
        pFree = pDest;
      }
    }

    if (iMode != CDX_CMP_EXACT && nLen < nDestLen)
    {
      nDestLen = nLen;
    }
    pKey = hb_cdxKeyPutCL(pKey, pText, nLen, ulRec, static_cast<HB_USHORT>(nDestLen), iMode);
    if (pFree)
    {
      hb_xfree(pFree);
    }
    return pKey;
  }
  case 'N':
    if (pTag->uiLen == 4)
    {
      HB_U32 uiVal = static_cast<HB_U32>(pItem->getNI()) + 0x80000000;
      HB_PUT_BE_UINT32(buf, uiVal);
      nLen = 4;
    }
    else
    {
      d = pItem->getND();
      HB_DBL2ORD(&d, buf);
      nLen = 8;
    }
    break;
  case 'D':
    d = static_cast<double>(pItem->getDL());
    HB_DBL2ORD(&d, buf);
    nLen = 8;
    if (iMode == CDX_CMP_PREFIX && pTag->uiType == 'T')
    {
      iMode = CDX_CMP_DATE;
    }
    break;
  case 'T':
    if (pTag->uiType == 'D')
    {
      d = static_cast<double>(pItem->getDL());
    }
    else
    {
      d = pItem->getTD();
    }
    HB_DBL2ORD(&d, buf);
    nLen = 8;
    break;
  case 'L':
    *buf = static_cast<HB_BYTE>(pItem->getL() ? 'T' : 'F');
    nLen = 1;
    break;
  default:
    ptr = nullptr;
    nLen = 0;
    hb_cdxErrorRT(pTag->pIndex->pArea, EG_DATATYPE, EDBF_INVALIDKEY, nullptr, 0, 0, nullptr);
    break;
  }

  pKey = hb_cdxKeyPut(pKey, ptr, static_cast<HB_USHORT>(nLen), ulRec);
  pKey->mode = static_cast<HB_USHORT>(iMode);

  return pKey;
}

// get Item from index key
static PHB_ITEM hb_cdxKeyGetItem(LPCDXKEY pKey, PHB_ITEM pItem, LPCDXTAG pTag)
{
  double d;

  if (pKey)
  {
    switch (pTag->uiType)
    {
    case 'C':
    {
      HB_SIZE nLen = pKey->len;
      char *pszVal = hb_cdpnDup(reinterpret_cast<const char *>(pKey->val), &nLen,
                                pTag->pIndex->pArea->dbfarea.area.cdPage, hb_vmCDP());
      pItem = hb_itemPutCLPtr(pItem, pszVal, nLen);
      break;
    }
    case 'N':
      if (pKey->len == 4)
      {
        HB_I32 iVal = static_cast<HB_I32>(HB_GET_BE_UINT32(pKey->val)) - 0x80000000;
        pItem = hb_itemPutNI(pItem, iVal);
      }
      else
      {
        HB_ORD2DBL(pKey->val, &d);
        pItem = hb_itemPutND(pItem, d);
      }
      break;
    case 'D':
      HB_ORD2DBL(pKey->val, &d);
      pItem = hb_itemPutDL(pItem, static_cast<long>(d));
      break;
    case 'T':
      HB_ORD2DBL(pKey->val, &d);
      pItem = hb_itemPutTD(pItem, d);
      break;
    case 'L':
      pItem = hb_itemPutL(pItem, pKey->val[0] == 'T');
      break;
    default:
      if (pItem != nullptr)
      {
        hb_itemClear(pItem);
      }
      else
      {
        pItem = hb_itemNew(nullptr);
      }
    }
  }
  else if (pItem != nullptr)
  {
    hb_itemClear(pItem);
  }
  else
  {
    pItem = hb_itemNew(nullptr);
  }

  return pItem;
}

// evaluate key expression and create new Key from the result
static LPCDXKEY hb_cdxKeyEval(LPCDXKEY pKey, LPCDXTAG pTag)
{
  CDXAREAP pArea = pTag->pIndex->pArea;
  PHB_ITEM pItem;
  auto cdpTmp = hb_cdpSelect(pArea->dbfarea.area.cdPage);

  if (pTag->nField)
  {
    pItem = hb_stackReturnItem();
    SELF_GETVALUE(&pArea->dbfarea.area, pTag->nField, pItem);
    pKey = hb_cdxKeyPutItem(pKey, pItem, pArea->dbfarea.ulRecNo, pTag, CDX_CMP_EXACT);
  }
  else
  {
    auto iCurrArea = hb_rddGetCurrentWorkAreaNumber();

    if (iCurrArea != pArea->dbfarea.area.uiArea)
    {
      hb_rddSelectWorkAreaNumber(pArea->dbfarea.area.uiArea);
    }
    else
    {
      iCurrArea = 0;
    }

    pItem = hb_vmEvalBlockOrMacro(pTag->pKeyItem);
    pKey = hb_cdxKeyPutItem(pKey, pItem, pArea->dbfarea.ulRecNo, pTag, CDX_CMP_EXACT);

    if (iCurrArea)
    {
      hb_rddSelectWorkAreaNumber(iCurrArea);
    }
  }

  hb_cdpSelect(cdpTmp);

  return pKey;
}

// evaluate conditional expression and return the result
static bool hb_cdxEvalCond(CDXAREAP pArea, PHB_ITEM pCondItem, bool fSetWA)
{
  auto iCurrArea = 0;
  auto fRet = false;

  if (fSetWA)
  {
    iCurrArea = hb_rddGetCurrentWorkAreaNumber();
    if (iCurrArea != pArea->dbfarea.area.uiArea)
    {
      hb_rddSelectWorkAreaNumber(pArea->dbfarea.area.uiArea);
    }
    else
    {
      iCurrArea = 0;
    }
  }

  fRet = hb_vmEvalBlockOrMacro(pCondItem)->getL();

  if (iCurrArea)
  {
    hb_rddSelectWorkAreaNumber(iCurrArea);
  }

  return fRet;
}

// evaluate seek/skip block: {| key, rec | ... }
static bool hb_cdxEvalSeekCond(LPCDXTAG pTag, PHB_ITEM pCondItem)
{
  auto fRet = false;
  PHB_ITEM pKeyVal, pKeyRec;

  pKeyVal = hb_cdxKeyGetItem(pTag->CurKey, nullptr, pTag);
  pKeyRec = hb_itemPutNInt(nullptr, pTag->CurKey->rec);

  fRet = hb_vmEvalBlockV(pCondItem, 2, pKeyVal, pKeyRec)->getL();

  hb_itemRelease(pKeyVal);
  hb_itemRelease(pKeyRec);

  return fRet;
}

// check if Key is in top scope
static bool hb_cdxTopScope(LPCDXTAG pTag)
{
  LPCDXKEY pKey;

  if (pTag->UsrAscend)
  {
    pKey = pTag->topScopeKey;
    return !pKey || !pKey->len ||
           hb_cdxValCompare(pTag, pKey->val, pKey->len, pTag->CurKey->val, pTag->CurKey->len, pKey->mode) <= 0;
  }
  else
  {
    pKey = pTag->bottomScopeKey;
    return !pKey || !pKey->len ||
           hb_cdxValCompare(pTag, pKey->val, pKey->len, pTag->CurKey->val, pTag->CurKey->len, pKey->mode) >= 0;
  }
}

// check if Key is in bottom scope
static bool hb_cdxBottomScope(LPCDXTAG pTag)
{
  LPCDXKEY pKey;

  if (pTag->UsrAscend)
  {
    pKey = pTag->bottomScopeKey;
    return !pKey || !pKey->len ||
           hb_cdxValCompare(pTag, pKey->val, pKey->len, pTag->CurKey->val, pTag->CurKey->len, pKey->mode) >= 0;
  }
  else
  {
    pKey = pTag->topScopeKey;
    return !pKey || !pKey->len ||
           hb_cdxValCompare(pTag, pKey->val, pKey->len, pTag->CurKey->val, pTag->CurKey->len, pKey->mode) <= 0;
  }
}

// clear top or bottom scope
static void hb_cdxTagClearScope(LPCDXTAG pTag, HB_USHORT nScope)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxTagClearScope(%p, %hu)", static_cast<void*>(pTag), nScope));
#endif

  CDXAREAP pArea = pTag->pIndex->pArea;
  LPCDXKEY *pScopeKey;
  PHB_ITEM *pScope;

  // resolve any pending scope relations first
  if (pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped)
  {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  if (pTag->UsrAscend ? nScope == 0 : nScope != 0)
  {
    pScope = &pTag->topScope;
    pScopeKey = &pTag->topScopeKey;
  }
  else
  {
    pScope = &pTag->bottomScope;
    pScopeKey = &pTag->bottomScopeKey;
  }
  if (*pScope)
  {
    hb_itemRelease(*pScope);
    *pScope = nullptr;
  }
  if (*pScopeKey)
  {
    hb_cdxKeyFree(*pScopeKey);
    *pScopeKey = nullptr;
    pTag->curKeyState &= ~(CDX_CURKEY_RAWCNT | CDX_CURKEY_LOGCNT);
    if (nScope == 0)
    {
      pTag->curKeyState &= ~(CDX_CURKEY_RAWPOS | CDX_CURKEY_LOGPOS);
    }
  }
}

// set top or bottom scope
static void hb_cdxTagSetScope(LPCDXTAG pTag, HB_USHORT nScope, PHB_ITEM pItem)
{
  CDXAREAP pArea = pTag->pIndex->pArea;
  PHB_ITEM pScopeVal;

  // resolve any pending scope relations first
  if (pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped)
  {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  pScopeVal = (hb_itemType(pItem) & Harbour::Item::BLOCK) ? hb_vmEvalBlock(pItem) : pItem;

  if (hb_cdxItemTypeCmp(static_cast<HB_BYTE>(pTag->uiType)) == hb_cdxItemTypeCmp(hb_cdxItemType(pScopeVal)))
  {
    PHB_ITEM *pScope;
    LPCDXKEY *pScopeKey;
    HB_ULONG ulRec;

    if (pTag->UsrAscend ? nScope == 0 : nScope != 0)
    {
      pScope = &(pTag->topScope);
      pScopeKey = &(pTag->topScopeKey);
      ulRec = CDX_IGNORE_REC_NUM;
    }
    else
    {
      pScope = &(pTag->bottomScope);
      pScopeKey = &(pTag->bottomScopeKey);
      ulRec = CDX_MAX_REC_NUM;
    }

    if (*pScope == nullptr)
    {
      *pScope = hb_itemNew(nullptr);
    }
    hb_itemCopy(*pScope, pItem);
    *pScopeKey = hb_cdxKeyPutItem(*pScopeKey, pScopeVal, ulRec, pTag, CDX_CMP_PREFIX);
    pTag->curKeyState &= ~(CDX_CURKEY_RAWCNT | CDX_CURKEY_LOGCNT);
    if (nScope == 0)
    {
      pTag->curKeyState &= ~(CDX_CURKEY_RAWPOS | CDX_CURKEY_LOGPOS);
    }
  }
  else
  {
    // TODO: !!!
    // RT error: DBFCDX/1051  Scope Type Mismatch
    // hb_cdxErrorRT
  }
}

static void hb_cdxTagGetScope(LPCDXTAG pTag, HB_USHORT nScope, PHB_ITEM pItem)
{
  CDXAREAP pArea = pTag->pIndex->pArea;
  PHB_ITEM *pScope;

  // resolve any pending scoped relations first
  if (pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped)
  {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  pScope = (pTag->UsrAscend ? nScope == 0 : nScope != 0) ? &(pTag->topScope) : &(pTag->bottomScope);
  if (*pScope)
  {
    hb_itemCopy(pItem, *pScope);
  }
  else
  {
    hb_itemClear(pItem);
  }
}

// refresh top and bottom scope value if set as codeblock
static void hb_cdxTagRefreshScope(LPCDXTAG pTag)
{
  PHB_ITEM pItem;

  if (pTag->pIndex->pArea->dbfarea.lpdbPendingRel && pTag->pIndex->pArea->dbfarea.lpdbPendingRel->isScoped)
  {
    SELF_FORCEREL(&pTag->pIndex->pArea->dbfarea.area);
  }

  if (hb_itemType(pTag->topScope) & Harbour::Item::BLOCK)
  {
    pItem = hb_vmEvalBlock(pTag->topScope);
    pTag->topScopeKey = hb_cdxKeyPutItem(pTag->topScopeKey, pItem, pTag->topScopeKey->rec, pTag, CDX_CMP_PREFIX);
  }
  if (hb_itemType(pTag->bottomScope) & Harbour::Item::BLOCK)
  {
    pItem = hb_vmEvalBlock(pTag->bottomScope);
    pTag->bottomScopeKey =
        hb_cdxKeyPutItem(pTag->bottomScopeKey, pItem, pTag->bottomScopeKey->rec, pTag, CDX_CMP_PREFIX);
  }
}

#ifdef HB_CDX_DBGCODE_EXT
// check internal integrity of page pool
static void hb_cdxTagPoolCheck(LPCDXTAG pTag)
{
  LPCDXPAGE pPage, pPrevPage;

  pPage = pTag->pagePool;
  pPrevPage = nullptr;
  while (pPage)
  {
    if (pPage->pPoolPrev != pPrevPage || pPage->TagParent != pTag)
    {
      hb_cdxErrInternal("hb_cdxTagPoolCheck: data integrity error.");
    }
    pPrevPage = pPage;
    pPage = pPage->pPoolNext;
  }
}

// check if the Tag buffers was not changed without write lock
static void hb_cdxTagCheckBuffers(LPCDXTAG pTag)
{
  auto fChanged = false;

  hb_cdxTagPoolCheck(pTag);
  if (pTag->TagChanged)
  {
    fChanged = true;
  }
  else
  {
    LPCDXPAGE pPage = pTag->pagePool;
    while (pPage && !fChanged)
    {
      fChanged = pPage->fChanged;
      pPage = pPage->pPoolNext;
    }
  }
  if (fChanged)
  {
    hb_cdxErrInternal("hb_cdxTagCheckBuffers: modification without write lock.");
  }
}

// check if the Index buffers was not changed without write lock
static void hb_cdxIndexCheckBuffers(LPCDXINDEX pIndex)
{
  LPCDXTAG pTag;

  if (pIndex->fChanged || (pIndex->freeLst && pIndex->freeLst->fStat))
  {
    hb_cdxErrInternal("hb_cdxIndexCheckBuffers: modification without write lock.");
  }

  if (pIndex->pCompound)
  {
    hb_cdxTagCheckBuffers(pIndex->pCompound);
  }
  pTag = pIndex->TagList;
  while (pTag != nullptr)
  {
    hb_cdxTagCheckBuffers(pTag);
    pTag = pTag->pNext;
  }
}
#endif

// lock index for flushing data after (exclusive lock)
static void hb_cdxIndexLockFlush(LPCDXINDEX pIndex)
{
  if (!hb_dbfLockIdxWrite(&pIndex->pArea->dbfarea, pIndex->pFile, &pIndex->lockData))
  {
    hb_errInternal(9109, "hb_cdxIndexLockFlush: flush lock failed.", nullptr, nullptr);
  }
}

// get free index page
static HB_ULONG hb_cdxIndexGetAvailPage(LPCDXINDEX pIndex, bool fHeader)
{
  PHB_FILE pFile = pIndex->pFile;
  HB_ULONG ulPage;

  if (pIndex->fReadonly)
  {
    hb_errInternal(9101, "hb_cdxIndexGetAvailPage on readonly database.", nullptr, nullptr);
  }

  if (pIndex->fShared && !pIndex->lockWrite)
  {
    hb_errInternal(9102, "hb_cdxIndexGetAvailPage on not locked index file.", nullptr, nullptr);
  }

  if (pIndex->freePage != 0 && pIndex->freePage != CDX_DUMMYNODE && !fHeader)
  {
    ulPage = pIndex->freePage;
    if (pIndex->freeLst != nullptr)
    {
      LPCDXLIST pLst = pIndex->freeLst;
      pIndex->freePage = pLst->nextPage;
      pIndex->freeLst = pLst->pNext;
      hb_xfree(pLst);
    }
    else
    {
      HB_BYTE byBuf[4];
      if (hb_fileReadAt(pFile, byBuf, 4, hb_cdxFilePageOffset(pIndex, ulPage)) != 4)
      {
        hb_errInternal(EDBF_READ, "hb_cdxIndexGetAvailPage: Read index page failed.", nullptr, nullptr);
      }
#ifdef HB_CDX_DBGUPDT
      cdxReadNO++;
#endif
      pIndex->freePage = HB_GET_LE_UINT32(byBuf);
    }
  }
  else
  {
    HB_SIZE nSize = fHeader ? pIndex->uiHeaderLen : pIndex->uiPageLen, nLen = 0;

    if (pIndex->nextAvail == CDX_DUMMYNODE)
    {
      pIndex->nextAvail = hb_cdxFilePageNum(pIndex, hb_fileSize(pFile));
    }

    ulPage = pIndex->nextAvail;
    do
    {
      pIndex->nextAvail += hb_cdxFilePageNext(pIndex, 1);
      nLen += pIndex->uiPageLen;
    } while (nLen < nSize);

    // TODO: ###
    if (fHeader)
    {
      if (nSize < static_cast<HB_SIZE>(pIndex->uiPageLen))
      {
        nSize = pIndex->uiPageLen;
      }
      auto byPageBuf = static_cast<HB_BYTE *>(hb_xgrabz(nSize));

      hb_cdxIndexLockFlush(pIndex);
      if (hb_fileWriteAt(pFile, byPageBuf, nSize, hb_cdxFilePageOffset(pIndex, ulPage)) != nSize)
      {
        hb_errInternal(EDBF_WRITE, "Write in index page failed.", nullptr, nullptr);
      }
#ifdef HB_CDX_DBGUPDT
      cdxWriteNO++;
#endif
      pIndex->fChanged = true;
      hb_xfree(byPageBuf);
    }
  }
  return ulPage;
}

// free index page
static void hb_cdxIndexPutAvailPage(LPCDXINDEX pIndex, HB_ULONG ulPage, bool fHeader)
{
  if (ulPage != 0 && ulPage != CDX_DUMMYNODE)
  {
    HB_SIZE nSize = fHeader ? pIndex->uiHeaderLen : pIndex->uiPageLen, nLen = 0;
    LPCDXLIST pLst;

    if (pIndex->fReadonly)
    {
      hb_errInternal(9101, "hb_cdxIndexPutAvailPage on readonly database.", nullptr, nullptr);
    }
    if (pIndex->fShared && !pIndex->lockWrite)
    {
      hb_errInternal(9102, "hb_cdxIndexPutAvailPage on not locked index file.", nullptr, nullptr);
    }

    do
    {
      pLst = static_cast<LPCDXLIST>(hb_xgrab(sizeof(CDXLIST)));
      pLst->nextPage = pIndex->freePage;
      pIndex->freePage = ulPage;
      pLst->fStat = true;
      pLst->pNext = pIndex->freeLst;
      pIndex->freeLst = pLst;
      ulPage += hb_cdxFilePageNext(pIndex, 1);
      nLen += pIndex->uiPageLen;
    } while (nLen < nSize);
  }
}

// flush list of free pages into index file
static void hb_cdxIndexFlushAvailPage(LPCDXINDEX pIndex)
{
  LPCDXLIST pLst = pIndex->freeLst;
  HB_ULONG ulPage;

  if (pIndex->fReadonly)
  {
    hb_errInternal(9101, "hb_cdxIndexFlushAvailPage on readonly database.", nullptr, nullptr);
  }
  if (pIndex->fShared && !pIndex->lockWrite)
  {
    hb_errInternal(9102, "hb_cdxIndexFlushAvailPage on not locked index file.", nullptr, nullptr);
  }
  hb_cdxIndexLockFlush(pIndex);

  ulPage = pIndex->freePage;
  if (pLst && pLst->fStat)
  {
    auto byPageBuf = static_cast<HB_BYTE *>(hb_xgrabz(pIndex->uiPageLen));

    do
    {
      HB_PUT_LE_UINT32(byPageBuf, pLst->nextPage);
      if (hb_fileWriteAt(pIndex->pFile, byPageBuf, pIndex->uiPageLen, hb_cdxFilePageOffset(pIndex, ulPage)) !=
          static_cast<HB_SIZE>(pIndex->uiPageLen))
      {
        hb_errInternal(EDBF_WRITE, "Write in index page failed.", nullptr, nullptr);
      }
#ifdef HB_CDX_DBGUPDT
      cdxWriteNO++;
#endif
      pIndex->fChanged = true;
      ulPage = pLst->nextPage;
      pLst->fStat = false;
      pLst = pLst->pNext;
    } while (pLst && pLst->fStat);
    hb_xfree(byPageBuf);
  }
}

// drop list of free pages in index file
static void hb_cdxIndexDropAvailPage(LPCDXINDEX pIndex)
{
  LPCDXLIST pLst;

  while (pIndex->freeLst)
  {
    pLst = pIndex->freeLst->pNext;
    hb_xfree(pIndex->freeLst);
    pIndex->freeLst = pLst;
  }
}

// write index page
static void hb_cdxIndexPageWrite(LPCDXINDEX pIndex, HB_ULONG ulPage, const HB_BYTE *pBuffer, HB_SIZE nSize)
{
  if (pIndex->fReadonly)
  {
    hb_errInternal(9101, "hb_cdxIndexPageWrite on readonly database.", nullptr, nullptr);
  }
  if (pIndex->fShared && !pIndex->lockWrite)
  {
    hb_errInternal(9102, "hb_cdxIndexPageWrite on not locked index file.", nullptr, nullptr);
  }
  hb_cdxIndexLockFlush(pIndex);

  if (hb_fileWriteAt(pIndex->pFile, pBuffer, nSize, hb_cdxFilePageOffset(pIndex, ulPage)) != nSize)
  {
    hb_errInternal(EDBF_WRITE, "Write in index page failed.", nullptr, nullptr);
  }
  pIndex->fChanged = true;
#ifdef HB_CDX_DBGUPDT
  cdxWriteNO++;
#endif
}

// read index page
static void hb_cdxIndexPageRead(LPCDXINDEX pIndex, HB_ULONG ulPage, HB_BYTE *pBuffer, HB_SIZE nSize)
{
  if (pIndex->fShared && !(pIndex->lockRead || pIndex->lockWrite))
  {
    hb_errInternal(9103, "hb_cdxIndexPageRead on not locked index file.", nullptr, nullptr);
  }

  if (hb_fileReadAt(pIndex->pFile, pBuffer, nSize, hb_cdxFilePageOffset(pIndex, ulPage)) != nSize)
  {
    hb_errInternal(EDBF_READ, "hb_cdxIndexPageRead: Read index page failed.", nullptr, nullptr);
  }
#ifdef HB_CDX_DBGUPDT
  cdxReadNO++;
#endif
}

// check if index was updated by other process and if it was discard buffers
static void hb_cdxIndexCheckVersion(LPCDXINDEX pIndex)
{
  HB_BYTE byBuf[8];
  HB_ULONG ulVer, ulFree;

  if (hb_fileReadAt(pIndex->pFile, byBuf, 8, 0x04) != 8)
  {
    if (pIndex->lockWrite > 0 && hb_fileSize(pIndex->pFile) == 0)
    {
      memset(byBuf, 0, 8);
    }
    else
    {
      hb_errInternal(2155, "hb_cdxIndexCheckVersion: Read error on index heading page.", nullptr, nullptr);
    }
  }
#ifdef HB_CDX_DBGUPDT
  cdxReadNO++;
#endif
  ulFree = HB_GET_LE_UINT32(&byBuf[0]);
  ulVer = HB_GET_BE_UINT32(&byBuf[4]);
  if (!pIndex->fShared)
  {
    pIndex->ulVersion = pIndex->freePage;
  }
  else if (ulVer != pIndex->ulVersion || ulFree != pIndex->freePage)
  {
    pIndex->nextAvail = CDX_DUMMYNODE;
    pIndex->ulVersion = ulVer;
    pIndex->freePage = ulFree;
    hb_cdxIndexDiscardBuffers(pIndex);
  }
#if 0
   hb_cdxIndexDiscardBuffers(pIndex);  // TODO: !!! ## remove it it's for test only
#endif
}

// lock index for reading (shared lock)
static bool hb_cdxIndexLockRead(LPCDXINDEX pIndex)
{
  auto ret = false;

  if (pIndex->lockRead > 0 || pIndex->lockWrite > 0 || !pIndex->pArea->dbfarea.fShared || !pIndex->fShared ||
      HB_DIRTYREAD(&pIndex->pArea->dbfarea))
  {
    pIndex->lockRead++;
    return true;
  }
#ifdef HB_CDX_DBGCODE
  if (pIndex->lockRead != 0)
  {
    hb_errInternal(9105, "hb_cdxIndexLockRead: bad count of locks.", nullptr, nullptr);
  }

  if (pIndex->WrLck || pIndex->RdLck)
  {
    hb_errInternal(9107, "hb_cdxIndexLockRead: lock failure (*)", nullptr, nullptr);
  }
  pIndex->RdLck = true;
#endif

  ret = hb_dbfLockIdxFile(&pIndex->pArea->dbfarea, pIndex->pFile, FL_LOCK | FLX_SHARED | FLX_WAIT, true,
                          &pIndex->lockData);
  if (!ret)
  {
    hb_cdxErrorRT(pIndex->pArea, EG_LOCK, EDBF_LOCK, pIndex->szFileName, hb_fsError(), 0, nullptr);
  }

  if (ret)
  {
    pIndex->lockRead++;
    hb_cdxIndexCheckVersion(pIndex);
  }
  return ret;
}

// lock index for writing (exclusive lock)
static bool hb_cdxIndexLockWrite(LPCDXINDEX pIndex)
{
  auto ret = false;

  if (pIndex->fReadonly)
  {
    hb_errInternal(9101, "hb_cdxIndexLockWrite: readonly index.", nullptr, nullptr);
  }
  if (pIndex->lockRead)
  {
    hb_errInternal(9105, "hb_cdxIndexLockWrite: writeLock after readLock.", nullptr, nullptr);
  }
  if (pIndex->lockWrite > 0)
  {
    pIndex->lockWrite++;
    return true;
  }
  if (pIndex->lockWrite != 0)
  {
    hb_errInternal(9105, "hb_cdxIndexLockWrite: bad count of locks.", nullptr, nullptr);
  }

  if (!pIndex->pArea->dbfarea.fShared || !pIndex->fShared)
  {
    ret = true;
  }
  else
  {
#ifdef HB_CDX_DBGCODE
    if (pIndex->WrLck || pIndex->RdLck)
    {
      hb_errInternal(9107, "hb_cdxIndexLockWrite: lock failure (*)", nullptr, nullptr);
    }
    pIndex->WrLck = true;
#endif
    ret = hb_dbfLockIdxFile(&pIndex->pArea->dbfarea, pIndex->pFile, FL_LOCK | FLX_EXCLUSIVE | FLX_WAIT, true,
                            &pIndex->lockData);
  }
  if (!ret)
  {
    hb_cdxErrorRT(pIndex->pArea, EG_LOCK, EDBF_LOCK, pIndex->szFileName, hb_fsError(), 0, nullptr);
  }

  if (ret)
  {
    pIndex->lockWrite++;
    if (pIndex->fShared || pIndex->nextAvail == CDX_DUMMYNODE)
    {
      hb_cdxIndexCheckVersion(pIndex);
    }
  }
  return ret;
}

// remove index read lock (shared lock)
static bool hb_cdxIndexUnLockRead(LPCDXINDEX pIndex)
{
  pIndex->lockRead--;
  if (pIndex->lockRead < 0)
  {
    hb_errInternal(9106, "hb_cdxIndexUnLockRead: bad count of locks.", nullptr, nullptr);
  }
  if (pIndex->lockRead || pIndex->lockWrite)
  {
    return true;
  }
#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxIndexCheckBuffers(pIndex);
#endif

  hb_cdxIndexPoolFree(pIndex, CDX_PAGECACHESIZE);

  if (pIndex->pArea->dbfarea.fShared && pIndex->fShared && !HB_DIRTYREAD(&pIndex->pArea->dbfarea))
  {
#ifdef HB_CDX_DBGCODE
    if (pIndex->WrLck || !pIndex->RdLck)
    {
      hb_errInternal(9108, "hb_cdxIndexUnLockRead: unlock error (*)", nullptr, nullptr);
    }
    pIndex->RdLck = false;
#endif
    if (!hb_dbfLockIdxFile(&pIndex->pArea->dbfarea, pIndex->pFile, FL_UNLOCK, true, &pIndex->lockData))
    {
      hb_errInternal(9108, "hb_cdxIndexUnLockRead: unlock error.", nullptr, nullptr);
    }
  }
  return true;
}

// remove index write lock (exclusive lock)
static bool hb_cdxIndexUnLockWrite(LPCDXINDEX pIndex)
{
  if (pIndex->lockWrite > 1)
  {
    pIndex->lockWrite--;
    return true;
  }

  if (pIndex->lockWrite < 1)
  {
    hb_errInternal(9106, "hb_cdxIndexUnLockWrite: bad count of locks.", nullptr, nullptr);
  }
  if (pIndex->lockRead)
  {
    hb_errInternal(9105, "hb_cdxIndexUnLockWrite: writeUnLock before readUnLock.", nullptr, nullptr);
  }

  hb_cdxIndexFlushBuffers(pIndex);
  hb_cdxIndexPoolFree(pIndex, CDX_PAGECACHESIZE);

  pIndex->lockWrite--;
  if (pIndex->pArea->dbfarea.fShared && pIndex->fShared)
  {
    if (pIndex->fChanged)
    {
      HB_BYTE byBuf[8];
      (pIndex->ulVersion)++;
      HB_PUT_LE_UINT32(&byBuf[0], pIndex->freePage);
      HB_PUT_BE_UINT32(&byBuf[4], pIndex->ulVersion);
      if (hb_fileWriteAt(pIndex->pFile, byBuf, 8, 0x04) != 8)
      {
        hb_errInternal(EDBF_WRITE, "Write in index page failed (ver)", nullptr, nullptr);
      }
      pIndex->fFlush = true;
      pIndex->fChanged = false;
    }
    hb_fileFlush(pIndex->pFile, true);
#ifdef HB_CDX_DBGCODE
    if (!pIndex->WrLck || pIndex->RdLck)
    {
      hb_errInternal(9108, "hb_cdxIndexUnLockWrite: unlock error (*)", nullptr, nullptr);
    }
    pIndex->WrLck = false;
#endif
    if (!hb_dbfLockIdxFile(&pIndex->pArea->dbfarea, pIndex->pFile, FL_UNLOCK, true, &pIndex->lockData))
    {
      hb_errInternal(9108, "hb_cdxIndexUnLockWrite: unlock error.", nullptr, nullptr);
    }
  }
  else
  {
    if (pIndex->ulVersion != pIndex->freePage)
    {
      HB_BYTE byBuf[4];
      HB_PUT_LE_UINT32(&byBuf[0], pIndex->freePage);
      if (hb_fileWriteAt(pIndex->pFile, byBuf, 4, 0x04) != 4)
      {
        hb_errInternal(EDBF_WRITE, "Write in index page failed (ver.ex)", nullptr, nullptr);
      }
      pIndex->ulVersion = pIndex->freePage;
      pIndex->fFlush = true;
#ifdef HB_CDX_DBGUPDT
      cdxWriteNO++;
#endif
    }
    else if (pIndex->fChanged)
    {
      pIndex->fFlush = true;
    }
    pIndex->fChanged = false;
  }
  return true;
}

// discard all pages in cache (TagClose and TagPoolFree for all Tags)
static void hb_cdxIndexDiscardBuffers(LPCDXINDEX pIndex)
{
  LPCDXTAG pTag;

#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxIndexCheckBuffers(pIndex);
#endif

  hb_cdxIndexDropAvailPage(pIndex);
  if (pIndex->pCompound)
  {
    hb_cdxTagClose(pIndex->pCompound);
    hb_cdxTagPoolFree(pIndex->pCompound, 0);
    pIndex->pCompound->fRePos = true;
    pIndex->pCompound->curKeyState = 0;
    if (pIndex->pCompound->CurKey)
    {
      pIndex->pCompound->CurKey->rec = 0;
    }
  }
  pTag = pIndex->TagList;
  while (pTag != nullptr)
  {
    hb_cdxTagClose(pTag);
    hb_cdxTagPoolFree(pTag, 0);
    pTag->fRePos = true;
    pTag->curKeyState = 0;
    if (pTag->CurKey && !pTag->Custom)
    {
      pTag->CurKey->rec = 0;
    }
    pTag = pTag->pNext;
  }
  hb_fileFlush(pIndex->pFile, false);
}

// write all changed pages in cache (pagePool, pages in Tags and Tag Header)
static void hb_cdxIndexFlushBuffers(LPCDXINDEX pIndex)
{
  LPCDXTAG pTag;

  if (pIndex->pCompound)
  {
    hb_cdxTagPoolFlush(pIndex->pCompound);
    if (pIndex->pCompound->TagChanged)
    {
      hb_cdxTagHeaderStore(pIndex->pCompound);
    }
  }
  pTag = pIndex->TagList;
  while (pTag != nullptr)
  {
    hb_cdxTagPoolFlush(pTag);
    if (pTag->TagChanged)
    {
      hb_cdxTagHeaderStore(pTag);
    }
    pTag = pTag->pNext;
  }
  hb_cdxIndexFlushAvailPage(pIndex);
}

// free cached pages of index file
static void hb_cdxIndexPoolFree(LPCDXINDEX pIndex, int nPagesLeft)
{
  LPCDXTAG pTag;

  if (pIndex->pCompound)
  {
    hb_cdxTagPoolFree(pIndex->pCompound, nPagesLeft);
  }
  pTag = pIndex->TagList;
  while (pTag != nullptr)
  {
    hb_cdxTagPoolFree(pTag, nPagesLeft);
    pTag = pTag->pNext;
  }
}

// get key value ptr from index page
static HB_BYTE *hb_cdxPageGetKeyVal(LPCDXPAGE pPage, int iKey)
{
#ifdef HB_CDX_DBGCODE
  if (iKey < 0 || iKey >= pPage->iKeys)
  {
    hb_cdxErrInternal("hb_cdxPageGetKeyVal: wrong iKey index.");
  }
#endif
  if (pPage->pKeyBuf)
  {
    return &pPage->pKeyBuf[iKey * (pPage->TagParent->uiLen + 8)];
  }
  else if (pPage->PageType & CDX_NODE_LEAF)
  {
    int iLen;
    HB_BYTE bTrail, *pKeyVal;

    pKeyVal = hb_cdxPageKeyBufPtr(pPage);
    iLen = pPage->TagParent->uiLen;
    bTrail = pPage->TagParent->bTrail;
    if (iKey < pPage->bufKeyNum - 1)
    {
      pPage->bufKeyNum = 0;
    }
    if (pPage->bufKeyNum == 0)
    {
      pPage->bufKeyPos = static_cast<HB_SHORT>(pPage->TagParent->pIndex->uiPageLen - CDX_EXT_HEADSIZE);
      pPage->bufKeyLen = static_cast<HB_SHORT>(iLen);
    }
    while (pPage->bufKeyNum <= iKey)
    {
      int iPos, iTmp, iTrl, iDup;

      iPos = pPage->bufKeyNum * pPage->ReqByte;
      iTmp = HB_GET_LE_UINT32(&hb_cdxPageExtKeyPool(pPage)[iPos + pPage->ReqByte - 4]) >>
             (32 - pPage->TCBits - pPage->DCBits);
      iDup = (pPage->bufKeyNum == 0) ? 0 : (iTmp & pPage->DCMask);
      iTrl = (iTmp >> pPage->DCBits) & pPage->TCMask;
      if ((iTmp = iLen - iDup - iTrl) > 0)
      {
        pPage->bufKeyPos -= static_cast<HB_SHORT>(iTmp);
        memcpy(&pKeyVal[iDup], &hb_cdxPageExtKeyPool(pPage)[pPage->bufKeyPos], iTmp);
      }
#ifdef HB_CDX_DBGCODE
      else if (iTmp < 0)
      {
        fprintf(stderr, "\r\npPage->Page=%lx, iLen=%d, iDup=%d, iTrl=%d", pPage->Page, iLen, iDup, iTrl);
        fflush(stderr);
        hb_cdxErrInternal("hb_cdxPageGetKeyVal: index corrupted.");
      }
#endif
      if (iTrl > 0 && (iTmp = pPage->bufKeyLen - iLen + iTrl) > 0)
      {
        memset(&pKeyVal[iLen - iTrl], bTrail, iTmp);
      }
      pPage->bufKeyLen = static_cast<HB_SHORT>(iLen - iTrl);
      pPage->bufKeyNum++;
#if 0
         fprintf(stderr, "\r\npPage->Page=%lx, iKey=%d, iLen=%d, iDup=%d, iTrl=%d, ulRec=%d, val[%s]", pPage->Page, pPage->bufKeyNum - 1, iLen, iDup, iTrl, HB_GET_LE_UINT32(&hb_cdxPageExtKeyPool(pPage)[iPos]), pKeyVal);
         fflush(stderr);
#endif
    }
    return pKeyVal;
  }
  else
  {
    return &hb_cdxPageIntKeyPool(pPage)[iKey * (pPage->TagParent->uiLen + 8)];
  }
}

// get record number from index page
static HB_ULONG hb_cdxPageGetKeyRec(LPCDXPAGE pPage, int iKey)
{
#ifdef HB_CDX_DBGCODE
  if (iKey < 0 || iKey >= pPage->iKeys)
  {
    hb_cdxErrInternal("hb_cdxPageGetKeyRec: wrong iKey index.");
  }
#endif
  if (pPage->pKeyBuf)
  {
    HB_BYTE *ptr = &pPage->pKeyBuf[(iKey + 1) * (pPage->TagParent->uiLen + 8) - 8];
    return HB_GET_LE_UINT32(ptr);
  }
  else if (pPage->PageType & CDX_NODE_LEAF)
  {
    HB_BYTE *ptr = &hb_cdxPageExtKeyPool(pPage)[iKey * pPage->ReqByte];
    return HB_GET_LE_UINT32(ptr) & pPage->RNMask;
  }
  else
  {
    HB_BYTE *ptr = &hb_cdxPageIntKeyPool(pPage)[(iKey + 1) * (pPage->TagParent->uiLen + 8) - 8];
    return HB_GET_BE_UINT32(ptr);
  }
}

// get child page number from interior index page
static HB_ULONG hb_cdxPageGetKeyPage(LPCDXPAGE pPage, int iKey)
{
  HB_BYTE *ptr;

#ifdef HB_CDX_DBGCODE
  if (iKey < 0 || iKey >= pPage->iKeys)
  {
    hb_cdxErrInternal("hb_cdxPageGetKeyPage: wrong iKey index.");
  }
  if (pPage->PageType & CDX_NODE_LEAF)
  {
    hb_cdxErrInternal("hb_cdxPageGetKeyPage: page is a leaf.");
  }
#endif
  ptr = &hb_cdxPageIntKeyPool(pPage)[(iKey + 1) * (pPage->TagParent->uiLen + 8) - 4];
  return HB_GET_BE_UINT32(ptr);
}

// get number of duplicated keys from key in leaf index page
static int hb_cdxPageGetKeyTrl(LPCDXPAGE pPage, int iKey)
{
#ifdef HB_CDX_DBGCODE_EXT
  if (iKey < 0 || iKey >= pPage->iKeys)
  {
    hb_cdxErrInternal("hb_cdxPageGetKeyTrl: wrong iKey index.");
  }
  if ((pPage->PageType & CDX_NODE_LEAF) == 0)
  {
    hb_cdxErrInternal("hb_cdxPageGetKeyTrl: page is not a leaf.");
  }
#endif
  if (pPage->pKeyBuf)
  {
    HB_BYTE *ptr = &pPage->pKeyBuf[(iKey + 1) * (pPage->TagParent->uiLen + 8) - 2];
    return HB_GET_LE_UINT16(ptr);
  }
  else
  {
    HB_BYTE *ptr = &hb_cdxPageExtKeyPool(pPage)[(iKey + 1) * pPage->ReqByte - 4];
    return (HB_GET_LE_UINT32(ptr) >> (32 - pPage->TCBits)) & pPage->TCMask;
  }
}

#ifdef HB_CDX_DBGCODE_EXT
// check if keys are sorted in proper order
static void hb_cdxPageCheckKeys(LPCDXPAGE pPage)
{
  if (pPage->iKeys > 1)
  {
    int K, iLen = pPage->TagParent->uiLen;
    HB_ULONG ulRec, ulRecPrev;
    HB_BYTE *pbVal;
    auto pbValPrev = static_cast<HB_BYTE *>(hb_xgrab(iLen));

    pPage->bufKeyNum = 0;
    pbVal = hb_cdxPageGetKeyVal(pPage, 0);
    ulRec = hb_cdxPageGetKeyRec(pPage, 0);
    for (auto i = 1; i < pPage->iKeys; i++)
    {
      memcpy(pbValPrev, pbVal, iLen);
      ulRecPrev = ulRec;
      pbVal = hb_cdxPageGetKeyVal(pPage, i);
      ulRec = hb_cdxPageGetKeyRec(pPage, i);
      K = hb_cdxValCompare(pPage->TagParent, pbValPrev, iLen, pbVal, iLen, CDX_CMP_EXACT);
      if (K > 0 || (K == 0 && ulRecPrev >= ulRec))
      {
        if (pPage->PageType & CDX_NODE_LEAF)
        {
          fprintf(stderr, "\r\niFree=%d, ReqByte=%d, RNBits=%d, DCBits=%d, TCBits=%d", pPage->iFree, pPage->ReqByte,
                  pPage->RNBits, pPage->DCBits, pPage->TCBits);
        }
        fprintf(stderr, "\r\nikey=%d, pPage->iKeys=%d, K=%d, ulRecPrev=%ld, ulRec=%ld", i, pPage->iKeys, K, ulRecPrev,
                ulRec);
        fprintf(stderr, "\r\npbValPrev=[%s] pbVal=[%s], [%d], pPage->pKeyBuf=%p, pPage->iCurKey=%d", pbValPrev, pbVal,
                memcmp(pbValPrev, pbVal, iLen), pPage->pKeyBuf, pPage->iCurKey);
        fflush(stderr);
        hb_cdxErrInternal("hb_cdxPageCheckKeys: index corrupted.");
      }
    }
    hb_xfree(pbValPrev);
  }
}

// Check decoded leaf page if all trailing and duplicate characters are set
static void hb_cdxPageCheckDupTrl(LPCDXPAGE pPage, HB_BYTE *pKeyBuf, int iKeys, bool fSpc)
{
  int iNum = pPage->TagParent->uiLen, iPos, iDup, iTrl, iFree = pPage->TagParent->pIndex->uiPageLen - CDX_EXT_HEADSIZE;
  int iLen = iNum + 8;
  HB_BYTE bTrail = pPage->TagParent->bTrail;
  auto bErr = false;

  for (auto iKey = 0; iKey < iKeys; iKey++)
  {
    iPos = iKey * iLen;
    iTrl = iDup = 0;
    while (iTrl < iNum && pKeyBuf[iPos + iNum - iTrl - 1] == bTrail)
    {
      ++iTrl;
    }
    if (iKey > 0)
    {
      int iMax;
#ifdef HB_CDX_PACKTRAIL
      iMax = iNum - iTrl;
#else
      iMax = HB_GET_LE_UINT16(&pKeyBuf[iPos - 2]);
      iMax = iNum - HB_MAX(iMax, iTrl);
#endif
      while (iDup < iMax && pKeyBuf[iPos + iDup] == pKeyBuf[iPos - iLen + iDup])
      {
        ++iDup;
      }
    }
    if (iTrl != HB_GET_LE_UINT16(&pKeyBuf[iPos + iNum + 6]))
    {
      fprintf(stderr, "\r\niTrl=%d, keybuf->iTrl=%d, iKey=%d/%d\r\n", iTrl, HB_GET_LE_UINT16(&pKeyBuf[iPos + iNum + 6]),
              iKey, iKeys);
      fflush(stderr);
      bErr = true;
    }
    if (iDup != (iKey == 0 ? 0 : HB_GET_LE_UINT16(&pKeyBuf[iPos + iNum + 4])))
    {
      fprintf(stderr, "\r\niDup=%d, keybuf->iDup=%d (iTrl=%d), iKey=%d/%d\r\n", iDup,
              HB_GET_LE_UINT16(&pKeyBuf[iPos + iNum + 4]), iTrl, iKey, iKeys);
      fflush(stderr);
      bErr = true;
    }
    if (iKey > 0)
    {
      int K;
      K = hb_cdxValCompare(pPage->TagParent, &pKeyBuf[iPos - iLen], iNum, &pKeyBuf[iPos], iNum, CDX_CMP_EXACT);
      if (K > 0 ||
          (K == 0 && HB_GET_LE_UINT32(&pKeyBuf[iPos + iNum - iLen]) >= HB_GET_LE_UINT32(&pKeyBuf[iPos + iNum])))
      {
        fprintf(stderr, "\r\nikey=%d, iKeys=%d, K=%d, ulRecPrev=%ld, ulRec=%ld", iKey, iKeys, K,
                static_cast<HB_ULONG>(HB_GET_LE_UINT32(&pKeyBuf[iPos + iNum - iLen])),
                static_cast<HB_ULONG>(HB_GET_LE_UINT32(&pKeyBuf[iPos + iNum])));
        fprintf(stderr, "\r\npbValPrev=[%s] pbVal=[%s], [%d], pKeyBuf=%p", &pKeyBuf[iPos - iLen], &pKeyBuf[iPos],
                memcmp(&pKeyBuf[iPos - iLen], &pKeyBuf[iPos], iNum), pKeyBuf);
        fflush(stderr);
        bErr = true;
      }
    }
    iFree -= iNum + pPage->ReqByte - iDup - iTrl;
  }
  if (fSpc && (iFree != pPage->iFree /* || iFree < 0 */))
  {
    fprintf(stderr,
            "\r\nFreeSpace calculated wrong! iFree=%d, pPage->iFree=%d, ReqByte=%d, RNBits=%d, DCBits=%d, TCBits=%d",
            iFree, pPage->iFree, pPage->ReqByte, pPage->RNBits, pPage->DCBits, pPage->TCBits);
    fflush(stderr);
    bErr = true;
  }
  if (bErr)
  {
    fprintf(stderr, "\r\nPage=%lx, Page->iFree=%d, iLen=%d\r\n", pPage->Page, pPage->iFree, iNum);
    fflush(stderr);
    hb_cdxErrInternal("hb_cdxPageCheckDupTrl: index corrupted.");
  }
}

static void hb_cdxPageLeafDecode(LPCDXPAGE pPage, HB_BYTE *pKeyBuf);
static void hb_cdxPageCheckDupTrlRaw(LPCDXPAGE pPage)
{
  auto pKeyBuf = static_cast<HB_BYTE *>(hb_xgrab(pPage->iKeys * (pPage->TagParent->uiLen + 8)));

  hb_cdxPageLeafDecode(pPage, pKeyBuf);
  hb_cdxPageCheckDupTrl(pPage, pKeyBuf, pPage->iKeys, true);
  hb_xfree(pKeyBuf);
}

static void hb_cdxChkLeafRecord(const HB_BYTE *pSrc, HB_ULONG ulRec, int iDup, int iTrl, LPCDXPAGE pPage)
{
  int iTmp = HB_GET_LE_UINT32(pSrc + pPage->ReqByte - 4) >> (32 - pPage->TCBits - pPage->DCBits);
  HB_ULONG ulRec2 = HB_GET_LE_UINT32(pSrc) & pPage->RNMask;
  int iDup2 = iTmp & pPage->DCMask, iTrl2 = (iTmp >> pPage->DCBits) & pPage->TCMask;

  if (ulRec != ulRec2 || iDup != iDup2 || iTrl != iTrl2)
  {
    fprintf(stderr, "\r\nDCBits=%d[%X], TCBits=%d[%X]  ", pPage->DCBits, pPage->DCMask, pPage->TCBits, pPage->TCMask);
    for (iTmp = 0; iTmp < pPage->ReqByte; ++iTmp)
    {
      fprintf(stderr, "%02X ", pSrc[iTmp]);
    }
    iTmp = ((iTrl << pPage->DCBits) | iDup) << (24 - pPage->TCBits - pPage->DCBits);
    fprintf(stderr, "  %6X", iTmp);
    fprintf(stderr, "\r\nhb_cdxChkLeafRecord: ReqByte=%d, ulRec[%lu=>%lu], iDup[%d=>%d], iTrl[%d=>%d]\r\n",
            pPage->ReqByte, ulRec, ulRec2, iDup, iDup2, iTrl, iTrl2);
    fflush(stderr);
    hb_cdxErrInternal("hb_cdxChkLeafRecord: wrong leaf record.");
  }
}
#endif

// put record and duplicate + trailing counters into leaf page
static void hb_cdxSetLeafRecord(HB_BYTE *pDst, HB_ULONG ulRec, int iDup, int iTrl, int iReq, int iDCbits, int iTCbits)
{
  int iBits, iFrom;

  iFrom = (iTCbits + iDCbits + 7) >> 3;
  iBits = ((iTrl << iDCbits) | iDup) << ((iFrom << 3) - iTCbits - iDCbits);
  iFrom = iReq - iFrom;
  for (auto i = 0; i < iReq; i++, ulRec >>= 8)
  {
    pDst[i] = static_cast<HB_BYTE>(ulRec & 0xff);
    if (i >= iFrom)
    {
      pDst[i] |= static_cast<HB_BYTE>(iBits & 0xff);
      iBits >>= 8;
    }
  }
}

// encode keys in buffer into cdx leaf node
static void hb_cdxPageLeafEncode(LPCDXPAGE pPage, HB_BYTE *pKeyBuf, int iKeys)
{
  int iReq, iNum, iLen;
  HB_BYTE *pKeyPos, *pRecPos, *pSrc;

#ifdef HB_CDX_DBGCODE
  if ((pPage->PageType & CDX_NODE_LEAF) == 0)
  {
    fprintf(stderr, "\r\npPage->Page=%lx. left=%lx, right=%lx", pPage->Page, pPage->Left, pPage->Right);
    fflush(stderr);
    hb_cdxErrInternal("hb_cdxPageLeafEncode: page is not a leaf.");
  }
#endif
#ifdef HB_CDX_DBGCODE_EXT
  if (!pKeyBuf)
  {
    hb_cdxErrInternal("hb_cdxPageLeafEncode: page has no buffer.");
  }
  hb_cdxPageCheckDupTrl(pPage, pKeyBuf, iKeys, true);
#endif
  iNum = pPage->TagParent->uiLen;
  iLen = iNum + 8;
  iReq = pPage->ReqByte;
  pRecPos = hb_cdxPageExtKeyPool(pPage);
  pKeyPos = reinterpret_cast<HB_BYTE *>(&pPage->node.extNode) + pPage->TagParent->pIndex->uiPageLen;
  pSrc = &pKeyBuf[0];
  for (auto iKey = 0; iKey < iKeys; iKey++, pSrc += iLen, pRecPos += iReq)
  {
    int iTrl, iDup, iTmp;
    HB_ULONG ulRec;

    ulRec = HB_GET_LE_UINT32(&pSrc[iNum]);
    iDup = HB_GET_LE_UINT16(&pSrc[iNum + 4]);
    iTrl = HB_GET_LE_UINT16(&pSrc[iNum + 6]);
    iTmp = iNum - iTrl - iDup;
#if 0
      fprintf(stderr, "\r\nKEY=%d, REC=%ld, DUP=%d, TRL=%d, VAL[%s]", iKey, ulRec, iDup, iTrl, pSrc);
#endif
    hb_cdxSetLeafRecord(pRecPos, ulRec, iDup, iTrl, iReq, pPage->DCBits, pPage->TCBits);
#ifdef HB_CDX_DBGCODE_EXT
    hb_cdxChkLeafRecord(pRecPos, ulRec, iDup, iTrl, pPage);
#endif
    if (iTmp > 0)
    {
      pKeyPos -= iTmp;
      memcpy(pKeyPos, &pSrc[iDup], iTmp);
    }
#ifdef HB_CDX_DBGCODE
    else if (iTmp < 0)
    {
      fprintf(stderr, "\r\n[%s][%s]", pSrc - iLen, pSrc);
      fprintf(stderr, "\r\npPage->Page=0x%lx, iKey=%d, iNum=%d, iDup=%d, iTrl=%d", pPage->Page, iKey, iNum, iDup, iTrl);
      fflush(stderr);
      hb_cdxErrInternal("hb_cdxPageLeafEncode: index corrupted.");
    }
#endif
  }
  if (pRecPos < pKeyPos)
  {
    memset(pRecPos, 0, pKeyPos - pRecPos);
  }
#ifdef HB_CDX_DBGCODE
  if (pKeyPos - pRecPos != pPage->iFree)
  {
    fprintf(stderr, "\r\nPage=0x%lx, calc=%d, iFree=%d, req=%u, keys=%d, keyLen=%d\r\n", pPage->Page,
            static_cast<int>(pKeyPos - pRecPos), pPage->iFree, pPage->ReqByte, iKeys, iNum);
    fflush(stderr);
    hb_cdxErrInternal("hb_cdxPageLeafEncode: FreeSpace calculated wrong!");
  }
  if (pPage->iFree < 0)
  {
    hb_cdxErrInternal("hb_cdxPageLeafEncode: FreeSpace calculated wrong!!");
  }
#endif
  pPage->iKeys = iKeys;
  pPage->fChanged = true;
  pPage->bufKeyNum = 0;
#ifdef HB_CDX_DBGCODE_EXT
  {
    HB_BYTE *pKeyBf = pPage->pKeyBuf;
    pPage->pKeyBuf = nullptr;
#if 0
      fprintf(stderr, "\r\nhb_cdxPageLeafEncode: check keys");
      if( iKeys > 0 ) {
         pPage->bufKeyNum = 0;
         hb_cdxPageGetKeyVal(pPage, iKeys - 1);
      }
#endif
    hb_cdxPageCheckKeys(pPage);
    pPage->pKeyBuf = pKeyBf;
  }
  hb_cdxPageCheckKeys(pPage);
  hb_cdxPageCheckDupTrl(pPage, pKeyBuf, pPage->iKeys, true);
#endif
}

// decode keys in page into buffer
static void hb_cdxPageLeafDecode(LPCDXPAGE pPage, HB_BYTE *pKeyBuf)
{
  int iBits, iReq, iLen = pPage->TagParent->uiLen;
  HB_BYTE *pDst, *pSrc, *pRec, bTrail = pPage->TagParent->bTrail;

#ifdef HB_CDX_DBGCODE
  if ((pPage->PageType & CDX_NODE_LEAF) == 0)
  {
    fprintf(stderr, "\r\npPage->Page=%lx", pPage->Page);
    fflush(stderr);
    hb_cdxErrInternal("hb_cdxPageLeafDecode: page is not a leaf.");
  }
#endif
  iBits = (32 - pPage->TCBits - pPage->DCBits);
  pDst = pKeyBuf;
  pRec = hb_cdxPageExtKeyPool(pPage);
  pSrc = reinterpret_cast<HB_BYTE *>(&pPage->node.extNode) + pPage->TagParent->pIndex->uiPageLen;
  iReq = pPage->ReqByte;
  for (auto iKey = 0; iKey < pPage->iKeys; iKey++, pRec += iReq)
  {
    int iTmp, iDup, iTrl, iNew;
    HB_ULONG ulRec;
    HB_BYTE *pTmp;

    pTmp = &pRec[iReq - 4];
    iTmp = HB_GET_LE_UINT32(pTmp) >> iBits;
    iDup = (iKey == 0) ? 0 : (iTmp & pPage->DCMask);
    iTrl = (iTmp >> pPage->DCBits) & pPage->TCMask;
    iNew = iLen - iDup - iTrl;
    if (iDup > 0)
    {
      memcpy(pDst, pDst - iLen - 8, iDup);
      pDst += iDup;
    }
    if (iNew > 0)
    {
      pSrc -= iNew;
      memcpy(pDst, pSrc, iNew);
      pDst += iNew;
    }
#ifdef HB_CDX_DBGCODE
    else if (iNew < 0)
    {
      fprintf(stderr, "\r\npPage->Page=%lx, iLen=%d, iDup=%d, iTrl=%d", pPage->Page, iLen, iDup, iTrl);
      fflush(stderr);
      hb_cdxErrInternal("hb_cdxPageLeafDecode: index corrupted.");
    }
#endif
    if (iTrl > 0)
    {
      memset(pDst, bTrail, iTrl);
      pDst += iTrl;
    }
    ulRec = HB_GET_LE_UINT32(pRec) & pPage->RNMask;
    HB_PUT_LE_UINT32(pDst, ulRec);
    pDst += 4;
    HB_PUT_LE_UINT16(pDst, iDup);
    pDst += 2;
    HB_PUT_LE_UINT16(pDst, iTrl);
    pDst += 2;
  }
#ifdef HB_CDX_DBGCODE_EXT
  {
    bool fChg = pPage->fChanged;
    hb_cdxPageLeafEncode(pPage, pKeyBuf, pPage->iKeys);
    pPage->fChanged = fChg;
  }
#endif
}

// init space leaf page
static void hb_cdxPageLeafInitSpace(LPCDXPAGE pPage)
{
  int iLen = pPage->TagParent->uiLen;
  HB_BYTE bBits;

  for (bBits = 0; iLen; bBits++, iLen >>= 1)
  {
    ;
  }

  pPage->ReqByte = bBits > 12 ? 5 : (bBits > 8 ? 4 : 3);
  pPage->RNBits = (pPage->ReqByte << 3) - (bBits << 1);
  pPage->DCBits = pPage->TCBits = bBits;
  pPage->DCMask = pPage->TCMask = static_cast<HB_USHORT>(HB_CDXBITMASK(bBits));
  pPage->RNMask = HB_CDXBITMASK(pPage->RNBits);
  pPage->iFree = pPage->TagParent->pIndex->uiPageLen - CDX_EXT_HEADSIZE;
}

// calculate the size of keys stored in buffer, return
// the number of keys which can be stored in the page
static void hb_cdxPageCalcLeafSpace(LPCDXPAGE pPage, HB_BYTE *pKeyBuf, int iKeys)
{
  int iNum = pPage->TagParent->uiLen, iSize;
  int iLen = iNum + 8;
  HB_BYTE ReqByte;
  HB_ULONG ulRec, RNMask;

  hb_cdxPageLeafInitSpace(pPage);
  pPage->iKeys = 0;
  RNMask = pPage->RNMask;
  ReqByte = pPage->ReqByte;
#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxPageCheckDupTrl(pPage, pKeyBuf, iKeys, false);
#endif
  // clear duplicate counter in 1st key
  HB_PUT_LE_UINT16(&pKeyBuf[iNum + 4], 0);
  for (auto iKey = 0; iKey < iKeys; iKey++)
  {
    HB_BYTE *bPtr = &pKeyBuf[iKey * iLen + iNum];
    ulRec = HB_GET_LE_UINT32(bPtr);
    iSize = ReqByte + iNum - HB_GET_LE_UINT16(&bPtr[4]) - HB_GET_LE_UINT16(&bPtr[6]);
    if (ulRec > RNMask)
    {
      HB_BYTE RNBits = pPage->RNBits;
      while (ulRec > RNMask)
      {
        ReqByte++;
        RNBits += 8;
        RNMask = (RNMask << 8) | 0xFF;
        iSize += (iKey + 1);
      }
      if (iSize > pPage->iFree)
      {
        break;
      }
#ifdef HB_CDX_DSPDBG_INFO_X
      fprintf(stderr, "\r\npPage->Page=%lx, ulRec=%lx, RNMask=%lx/%lx, RNBits=%d/%d, DCB=%d, TCB=%d (%lx), iKey=%d/%d",
              pPage->Page, ulRec, RNMask, pPage->RNMask, RNBits, pPage->RNBits, pPage->DCBits, pPage->TCBits,
              HB_CDXBITMASK(RNBits), iKey, iKeys);
      fflush(stderr);
#endif
      pPage->RNMask = RNMask;
      pPage->RNBits = RNBits;
      pPage->ReqByte = ReqByte;
    }
    else if (iSize > pPage->iFree)
    {
      break;
    }
    pPage->iFree -= static_cast<HB_SHORT>(iSize);
    pPage->iKeys++;
  }
}

// remove key from page
static int hb_cdxPageLeafDelKey(LPCDXPAGE pPage)
{
  int iKey = pPage->iCurKey, iLen = pPage->TagParent->uiLen + 8, iSpc;
  int iRet = 0;

#ifdef HB_CDX_DBGCODE
  if ((pPage->PageType & CDX_NODE_LEAF) == 0)
  {
    hb_cdxErrInternal("hb_cdxPageLeafDelKey: page is not a leaf.");
  }
  if (iKey < 0 || iKey >= pPage->iKeys)
  {
    hb_cdxErrInternal("hb_cdxPageLeafDelKey: wrong iKey index.");
  }
#endif
  if (!pPage->pKeyBuf)
  {
    auto pKeyBuf = static_cast<HB_BYTE *>(hb_xgrab((pPage->iKeys) * iLen));
    hb_cdxPageLeafDecode(pPage, pKeyBuf);
    pPage->pKeyBuf = pKeyBuf;
  }
#ifdef HB_CDX_DSPDBG_INFO
  fprintf(stderr, "\r\ndelkey: Page=%lx, iKey=%d/%d, rec=%ld, iFree=%d", pPage->Page, iKey, pPage->iKeys,
          static_cast<HB_ULONG>(HB_GET_LE_UINT32(&pPage->pKeyBuf[(iKey + 1) * iLen - 8])), pPage->iFree);
  fflush(stderr);
#endif
  iSpc = pPage->ReqByte + pPage->TagParent->uiLen - HB_GET_LE_UINT16(&pPage->pKeyBuf[(iKey + 1) * iLen - 4]) -
         HB_GET_LE_UINT16(&pPage->pKeyBuf[(iKey + 1) * iLen - 2]);
  if (iKey < pPage->iKeys - 1)
  {
    int iPos = (iKey + 2) * iLen - 4, iDup = 0, iDupNext;
    iDupNext = HB_GET_LE_UINT16(&pPage->pKeyBuf[iPos]);
    iSpc -= iDupNext;
    if (iKey > 0)
    {
      int iPrev = (iKey - 1) * iLen, iNext = (iKey + 1) * iLen, iNum = pPage->TagParent->uiLen, iTrlNext, iDupCurr;
      iTrlNext = HB_GET_LE_UINT16(&pPage->pKeyBuf[iNext + iLen - 2]);
      iDupCurr = HB_GET_LE_UINT16(&pPage->pKeyBuf[iNext - 4]);
#ifdef HB_CDX_PACKTRAIL
      iNum -= iTrlNext;
#else
      {
        int iTrlPrev = HB_GET_LE_UINT16(&pPage->pKeyBuf[iPrev + iLen - 2]);
        iNum -= HB_MAX(iTrlNext, iTrlPrev);
      }
#endif
      iDup = HB_MIN(iDupNext, iDupCurr);
      if (iDup > iNum)
      {
        iDup = iNum;
      }
      else
      {
        while (iDup < iNum && pPage->pKeyBuf[iPrev + iDup] == pPage->pKeyBuf[iNext + iDup])
        {
          ++iDup;
        }
      }
#ifdef HB_CDX_DSPDBG_INFO
      fprintf(stderr, "+%d=%d", iSpc + iDup, pPage->iFree + iSpc + iDup);
      if (iSpc + iDup < 0)
      {
        fprintf(stderr, " iLen=%d, iDup=%d, iNum=%d pd=%d pt=%d cd=%d ct=%d nd=%d nt=%d", iLen - 8, iDup, iNum,
                HB_GET_LE_UINT16(&pPage->pKeyBuf[iPrev + iLen - 4]),
                HB_GET_LE_UINT16(&pPage->pKeyBuf[iPrev + iLen - 2]), iDupCurr,
                HB_GET_LE_UINT16(&pPage->pKeyBuf[iNext - 2]), iDupNext, iTrlNext);
      }
      fflush(stderr);
#endif
    }
    HB_PUT_LE_UINT16(&pPage->pKeyBuf[iPos], iDup);
    iSpc += iDup;
  }
  pPage->iFree += static_cast<HB_SHORT>(iSpc);
  if (--pPage->iKeys > iKey)
  {
    memmove(&pPage->pKeyBuf[iKey * iLen], &pPage->pKeyBuf[(iKey + 1) * iLen], (pPage->iKeys - iKey) * iLen);
  }
  pPage->fBufChanged = pPage->fChanged = true;
#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxPageCheckKeys(pPage);
  hb_cdxPageCheckDupTrl(pPage, pPage->pKeyBuf, pPage->iKeys, true);
#endif
  if (iKey >= pPage->iKeys)
  {
    iRet |= NODE_NEWLASTKEY;
  }
  if (pPage->iKeys == 0)
  {
    iRet |= NODE_JOIN;
  }
  else if (pPage->iFree < 0)
  {
    iRet |= NODE_SPLIT;
  }
  if (pPage->iFree >= pPage->ReqByte)
  {
    iRet |= NODE_BALANCE;
  }
  return iRet;
}

// add key to page at current position
static int hb_cdxPageLeafAddKey(LPCDXPAGE pPage, LPCDXKEY pKey)
{
  int iKey, iNum = pPage->TagParent->uiLen;
  int iLen = iNum + 8, iSpc, iTrl, iDup, iMax, iPos;
  HB_BYTE bTrail = pPage->TagParent->bTrail;
  int iRet = 0;

#ifdef HB_CDX_DSPDBG_INFO
  fprintf(stderr, "\r\naddkey: Page=%lx, iKey=%d/%d, rec=%ld, iFree=%d", pPage->Page, pPage->iCurKey, pPage->iKeys,
          pKey->rec, pPage->iFree);
  fflush(stderr);
#endif
#ifdef HB_CDX_DBGCODE
  if ((pPage->PageType & CDX_NODE_LEAF) == 0)
  {
    hb_cdxErrInternal("hb_cdxPageLeafAddKey: page is not a leaf.");
  }
  if (pPage->iCurKey < 0 || pPage->iCurKey > pPage->iKeys)
  {
    hb_cdxErrInternal("hb_cdxPageLeafAddKey: wrong iKey index.");
  }
#endif
  if (!pPage->pKeyBuf)
  {
    auto pKeyBuf = static_cast<HB_BYTE *>(hb_xgrab((pPage->iKeys + 1) * iLen));
    hb_cdxPageLeafDecode(pPage, pKeyBuf);
    pPage->pKeyBuf = pKeyBuf;
  }
  else
  {
    pPage->pKeyBuf = static_cast<HB_BYTE *>(hb_xrealloc(pPage->pKeyBuf, (pPage->iKeys + 1) * iLen));
  }

#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxPageCheckKeys(pPage);
  hb_cdxPageCheckDupTrl(pPage, pPage->pKeyBuf, pPage->iKeys, true);
#endif

  iTrl = iDup = 0;
  iKey = pPage->iCurKey;
  iPos = iKey * iLen;
  if (iKey < pPage->iKeys)
  {
    if (!pPage->TagParent->pIndex->pArea->fSortCDP)
    {
      iDup = HB_GET_LE_UINT16(&pPage->pKeyBuf[iPos + iNum + 4]);
    }
    memmove(&pPage->pKeyBuf[iPos + iLen], &pPage->pKeyBuf[iPos], iLen * (pPage->iKeys - iKey));
  }
  if (pKey->len >= iNum)
  {
    memcpy(&pPage->pKeyBuf[iPos], pKey->val, iNum);
  }
  else
  {
    memcpy(&pPage->pKeyBuf[iPos], pKey->val, pKey->len);
    memset(&pPage->pKeyBuf[iPos + pKey->len], bTrail, iNum - pKey->len);
  }
  HB_PUT_LE_UINT32(&pPage->pKeyBuf[iPos + iNum], pKey->rec);
  while (iTrl < iNum && pPage->pKeyBuf[iPos + iNum - iTrl - 1] == bTrail)
  {
    ++iTrl;
  }
  if (iKey > 0)
  {
#ifdef HB_CDX_PACKTRAIL
    iMax = iNum - iTrl;
#else
    iMax = HB_GET_LE_UINT16(&pPage->pKeyBuf[iPos - 2]);
    iMax = iNum - HB_MAX(iTrl, iMax);
#endif
    if (iDup > iMax)
    {
      iDup = iMax;
    }
    else
    {
      while (iDup < iMax && pPage->pKeyBuf[iPos + iDup] == pPage->pKeyBuf[iPos + iDup - iLen])
      {
        ++iDup;
      }
    }
  }
  HB_PUT_LE_UINT16(&pPage->pKeyBuf[iPos + iNum + 4], iDup);
  HB_PUT_LE_UINT16(&pPage->pKeyBuf[iPos + iNum + 6], iTrl);
  iSpc = pPage->ReqByte + iNum - iTrl - iDup;
  if (iKey < pPage->iKeys)
  {
#ifdef HB_CDX_PACKTRAIL
    iMax = iNum - HB_GET_LE_UINT16(&pPage->pKeyBuf[iPos + iLen + iLen - 2]);
#else
    iMax = HB_GET_LE_UINT16(&pPage->pKeyBuf[iPos + iLen + iLen - 2]);
    iMax = iNum - HB_MAX(iTrl, iMax);
#endif
    iSpc += HB_GET_LE_UINT16(&pPage->pKeyBuf[iPos + iLen + iLen - 4]);
    iDup = 0;
    while (iDup < iMax && pPage->pKeyBuf[iPos + iDup] == pPage->pKeyBuf[iPos + iDup + iLen])
    {
      ++iDup;
    }
    HB_PUT_LE_UINT16(&pPage->pKeyBuf[iPos + iLen + iLen - 4], iDup);
    iSpc -= iDup;
  }
  pPage->iKeys++;
  while (pKey->rec > pPage->RNMask)
  {
    pPage->RNMask = (pPage->RNMask << 8) | 0xFF;
    pPage->ReqByte++;
    pPage->RNBits += 8;
    iSpc += pPage->iKeys;
  }
  pPage->iFree -= static_cast<HB_SHORT>(iSpc);
  pPage->fBufChanged = pPage->fChanged = true;
#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxPageCheckKeys(pPage);
  hb_cdxPageCheckDupTrl(pPage, pPage->pKeyBuf, pPage->iKeys, true);
#endif
  if (iKey >= pPage->iKeys - 1)
  {
    iRet |= NODE_NEWLASTKEY;
  }
  if (pPage->iFree < 0)
  {
    iRet |= NODE_SPLIT;
  }
  if (pPage->iFree >= pPage->ReqByte && pPage->Left != CDX_DUMMYNODE && pPage->Right != CDX_DUMMYNODE)
  {
    iRet |= NODE_BALANCE;
  }
  return iRet;
}

// set (insert) key in interior node record to (with) given value
static void hb_cdxPageIntSetKey(LPCDXPAGE pPage, int iKey, bool fIns, HB_BYTE *pbVal, HB_ULONG ulRec, HB_ULONG ulPag)
{
  int iLen = pPage->TagParent->uiLen;
  int iPos = iKey * (iLen + 8);
  HB_BYTE *pKeyPool = hb_cdxPageIntKeyPool(pPage);

#ifdef HB_CDX_DSPDBG_INFO
  fprintf(stderr, "\r\nintSetKey (%s): Page=%lx, iKey=%d/%d, ulPag=%lx", fIns ? "ins" : "set", pPage->Page, iKey,
          pPage->iKeys, ulPag);
  fflush(stderr);
#endif
#ifdef HB_CDX_DBGCODE
  if ((pPage->PageType & CDX_NODE_LEAF) != 0)
  {
    hb_cdxErrInternal("hb_cdxPageIntSetKey: page is a leaf!");
  }
  if (iKey < 0 || iKey >= pPage->iKeys + (fIns ? 1 : 0))
  {
    hb_cdxErrInternal("hb_cdxPageIntSetKey: wrong iKey index.");
  }
#endif
  if (fIns)
  {
    if (iKey < pPage->iKeys)
    {
      memmove(&pKeyPool[iPos + iLen + 8], &pKeyPool[iPos], (iLen + 8) * (pPage->iKeys - iKey));
    }
    pPage->iKeys++;
  }
  if (pbVal)
  {
    memcpy(&pKeyPool[iPos], pbVal, iLen);
  }
  else if (fIns)
  {
    memset(&pKeyPool[iPos], pPage->TagParent->bTrail, iLen);
  }
  if (ulRec)
  {
    HB_PUT_BE_UINT32(&pKeyPool[iPos + iLen], ulRec);
  }
  HB_PUT_BE_UINT32(&pKeyPool[iPos + iLen + 4], ulPag);
  pPage->fChanged = true;
}

// delete key in interior node record
static void hb_cdxPageIntDelKey(LPCDXPAGE pPage, int iKey)
{
  int iLen = pPage->TagParent->uiLen + 8;
  HB_BYTE *pKeyPool = hb_cdxPageIntKeyPool(pPage);

#ifdef HB_CDX_DSPDBG_INFO
  fprintf(stderr, "\r\nintDelKey: Page=%lx, iKey=%d/%d, ulPag=%lx", pPage->Page, iKey, pPage->iKeys,
          static_cast<HB_ULONG>(HB_GET_BE_UINT32(&pKeyPool[(iKey + 1) * iLen - 4])));
  fflush(stderr);
#endif
#ifdef HB_CDX_DBGCODE
  if ((pPage->PageType & CDX_NODE_LEAF) != 0)
  {
    hb_cdxErrInternal("hb_cdxPageIntDelKey: page is a leaf!");
  }
  if (iKey < 0 || iKey >= pPage->iKeys)
  {
    hb_cdxErrInternal("hb_cdxPageIntDelKey: wrong iKey index.");
  }
#endif
  pPage->iKeys--;
  if (pPage->iKeys > iKey)
  {
    memmove(&pKeyPool[iKey * iLen], &pKeyPool[(iKey + 1) * iLen], (pPage->iKeys - iKey) * iLen);
  }
  memset(&pKeyPool[pPage->iKeys * iLen], 0, iLen);
  pPage->fChanged = true;
}

// (re)load CDX page from index file
static void hb_cdxPageLoad(LPCDXPAGE pPage)
{
  if (pPage->pKeyBuf)
  {
    hb_xfree(pPage->pKeyBuf);
    pPage->pKeyBuf = nullptr;
    pPage->fBufChanged = false;
  }
  hb_cdxIndexPageRead(pPage->TagParent->pIndex, pPage->Page, reinterpret_cast<HB_BYTE *>(&pPage->node),
                      pPage->TagParent->pIndex->uiPageLen);
  pPage->PageType = static_cast<HB_BYTE>(HB_GET_LE_UINT16(pPage->node.intNode.attr));
  pPage->Left = HB_GET_LE_UINT32(pPage->node.intNode.leftPtr);
  pPage->Right = HB_GET_LE_UINT32(pPage->node.intNode.rightPtr);
  pPage->iKeys = HB_GET_LE_UINT16(pPage->node.intNode.nKeys);
  pPage->fChanged = false;

  if ((pPage->PageType & CDX_NODE_LEAF) != 0)
  {
    pPage->RNBits = pPage->node.extNode.recBits;
    pPage->DCBits = pPage->node.extNode.dupBits;
    pPage->TCBits = pPage->node.extNode.trlBits;
    if (pPage->DCBits <= 8)
    {
      pPage->DCMask = pPage->node.extNode.dupMask;
      pPage->TCMask = pPage->node.extNode.trlMask;
    }
    else
    {
      pPage->DCMask = static_cast<HB_USHORT>(HB_CDXBITMASK(pPage->DCBits));
      pPage->TCMask = static_cast<HB_USHORT>(HB_CDXBITMASK(pPage->TCBits));
    }
    pPage->RNMask = HB_GET_LE_UINT32(pPage->node.extNode.recMask);
    pPage->ReqByte = pPage->node.extNode.keyBytes;
    pPage->iFree = HB_GET_LE_UINT16(pPage->node.extNode.freeSpc);
    pPage->bufKeyNum = 0;
#if 0
      if( !pPage->pKeyBuf ) {
         auto pKeyBuf = static_cast<HB_BYTE*>(hb_xgrab((pPage->iKeys + 1) * ( pPage->TagParent->uiLen + 6 )));
         hb_cdxPageLeafDecode(pPage, pKeyBuf);
         pPage->pKeyBuf = pKeyBuf;
      }
#endif
  }
#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxPageCheckKeys(pPage);
#endif
}

// store page into index file
static void hb_cdxPageStore(LPCDXPAGE pPage)
{
#ifdef HB_CDX_DBGCODE
  if (pPage->Page == 0 || pPage->Page == CDX_DUMMYNODE)
  {
    hb_cdxErrInternal("hb_cdxPageStore: Page number wrong!");
  }
  if (pPage->PageType & CDX_NODE_LEAF)
  {
    if (pPage->iFree < 0)
    {
      hb_cdxErrInternal("hb_cdxPageStore: FreeSpace calculated wrong!");
    }
  }
  else if (pPage->iKeys > pPage->TagParent->MaxKeys)
  {
    hb_cdxErrInternal("hb_cdxPageStore: number of keys exceed!");
  }
#endif
  HB_PUT_LE_UINT16(pPage->node.intNode.attr, static_cast<HB_U16>(pPage->PageType));
  HB_PUT_LE_UINT16(pPage->node.intNode.nKeys, pPage->iKeys);
  HB_PUT_LE_UINT32(pPage->node.intNode.leftPtr, pPage->Left);
  HB_PUT_LE_UINT32(pPage->node.intNode.rightPtr, pPage->Right);

  if ((pPage->PageType & CDX_NODE_LEAF) != 0)
  {
    HB_PUT_LE_UINT16(pPage->node.extNode.freeSpc, pPage->iFree);
    HB_PUT_LE_UINT32(pPage->node.extNode.recMask, pPage->RNMask);
    if (pPage->DCBits <= 8)
    {
      pPage->node.extNode.dupMask = static_cast<HB_BYTE>(pPage->DCMask);
      pPage->node.extNode.trlMask = static_cast<HB_BYTE>(pPage->TCMask);
    }
    else
    {
      HB_PUT_LE_UINT16(&pPage->node.extNode.dupMask, pPage->DCMask);
    }
    pPage->node.extNode.recBits = pPage->RNBits;
    pPage->node.extNode.dupBits = pPage->DCBits;
    pPage->node.extNode.trlBits = pPage->TCBits;
    pPage->node.extNode.keyBytes = pPage->ReqByte;

    if (pPage->pKeyBuf && pPage->fBufChanged)
    {
      hb_cdxPageLeafEncode(pPage, pPage->pKeyBuf, pPage->iKeys);
      pPage->fBufChanged = false;
    }
#ifdef HB_CDX_DBGCODE_EXT
    if (pPage->pKeyBuf)
    {
      hb_xfree(pPage->pKeyBuf);
      pPage->pKeyBuf = nullptr;
    }
#endif
  }
  hb_cdxIndexPageWrite(pPage->TagParent->pIndex, pPage->Page, reinterpret_cast<const HB_BYTE *>(&pPage->node),
                       pPage->TagParent->pIndex->uiPageLen);
#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxPageCheckKeys(pPage);
#endif
  pPage->fChanged = false;
}

// create new empty page and allocate space for it in index file if ulPage == 0
// or load it from index file if ulPage != CDX_DUMMYNODE
static LPCDXPAGE hb_cdxPageNew(LPCDXTAG pTag, LPCDXPAGE pOwnerPage, HB_ULONG ulPage)
{
  LPCDXPAGE pPage = nullptr;

#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxTagPoolCheck(pTag);
#endif
  if (ulPage && ulPage != CDX_DUMMYNODE && pTag->pagePool)
  {
    pPage = pTag->pagePool;
    while (pPage && pPage->Page != ulPage)
    {
      pPage = pPage->pPoolNext;
    }
  }
  if (pPage)
  {
    if (pPage->pPoolPrev)
    {
      pPage->pPoolPrev->pPoolNext = pPage->pPoolNext;
      if (pPage->pPoolNext)
      {
        pPage->pPoolNext->pPoolPrev = pPage->pPoolPrev;
      }
      pPage->pPoolPrev = nullptr;
      pPage->pPoolNext = pTag->pagePool;
      pPage->pPoolNext->pPoolPrev = pPage;
      pTag->pagePool = pPage;
    }
  }
  else
  {
    HB_SIZE nSize = sizeof(CDXPAGE);

    nSize += pTag->uiLen + 8 + pTag->pIndex->uiPageLen - sizeof(pPage->node);
    pPage = static_cast<LPCDXPAGE>(hb_xgrabz(nSize));

    pPage->PageType = CDX_NODE_UNUSED;
    pPage->Left = pPage->Right = CDX_DUMMYNODE;
    pPage->TagParent = pTag;

    if (ulPage && ulPage != CDX_DUMMYNODE)
    {
      pPage->Page = ulPage;
      hb_cdxPageLoad(pPage);
    }
    else if (!ulPage)
    {
      pPage->Page = hb_cdxIndexGetAvailPage(pTag->pIndex, false);
      pPage->fChanged = true;
    }
    pPage->pPoolPrev = nullptr;
    pPage->pPoolNext = pTag->pagePool;
    pTag->pagePool = pPage;
    if (pPage->pPoolNext)
    {
      pPage->pPoolNext->pPoolPrev = pPage;
    }
  }
  pPage->Owner = pOwnerPage;
  pPage->iCurKey = -1;
  pPage->bUsed = 1;
#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxTagPoolCheck(pTag);
#endif
  return pPage;
}

// free single page
static void hb_cdxPageFree(LPCDXPAGE pPage, bool fReal)
{
#ifdef HB_CDX_DBGCODE_EXT
  LPCDXTAG pTag = pPage->TagParent;
  hb_cdxTagPoolCheck(pTag);
#endif
  if (pPage->Child != nullptr)
  {
    hb_cdxPageFree(pPage->Child, fReal);
    pPage->Child = nullptr;
  }

  if (pPage->PageType == CDX_NODE_UNUSED)
  {
    fReal = true;
    pPage->fChanged = false;
  }

  if (fReal)
  {
    if (pPage->fChanged)
    {
      hb_cdxPageStore(pPage);
    }

#ifdef HB_CDX_DBGCODE_EXT
    hb_cdxTagPoolCheck(pTag);
#endif
    if (pPage->pPoolPrev)
    {
      pPage->pPoolPrev->pPoolNext = pPage->pPoolNext;
      if (pPage->pPoolNext)
      {
        pPage->pPoolNext->pPoolPrev = pPage->pPoolPrev;
      }
    }
    else
    {
      pPage->TagParent->pagePool = pPage->pPoolNext;
      if (pPage->pPoolNext)
      {
        pPage->pPoolNext->pPoolPrev = nullptr;
      }
    }
#ifdef HB_CDX_DBGCODE_EXT
    hb_cdxTagPoolCheck(pTag);
#endif
  }

  if (pPage->Owner != nullptr && pPage->Owner->Child == pPage)
  {
    pPage->Owner->Child = nullptr;
  }
  pPage->Owner = nullptr;
  pPage->bUsed = 0;

  if (fReal)
  {
    if (pPage->PageType == CDX_NODE_UNUSED)
    {
      hb_cdxIndexPutAvailPage(pPage->TagParent->pIndex, pPage->Page, false);
    }
    if (pPage->pKeyBuf)
    {
      hb_xfree(pPage->pKeyBuf);
    }
    hb_xfree(pPage);
  }
#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxTagPoolCheck(pTag);
#endif
}

// read child page
static void hb_cdxPageGetChild(LPCDXPAGE pPage)
{
  HB_ULONG ulPage;

#ifdef HB_CDX_DBGCODE
  if ((pPage->PageType & CDX_NODE_LEAF) != 0)
  {
    hb_cdxErrInternal("hb_cdxPageGetChild: index corrupted.");
  }
#endif

  ulPage = hb_cdxPageGetKeyPage(pPage, pPage->iCurKey);
  if (pPage->Child != nullptr)
  {
    if (pPage->Child->Page != ulPage)
    {
      hb_cdxPageFree(pPage->Child, false);
      pPage->Child = nullptr;
    }
  }
#ifdef HB_CDX_DSPDBG_INFO
  fprintf(stderr, "\r\nGetChild: Parent=%lx, Child=%lx", pPage->Page, ulPage);
  fflush(stderr);
#endif
  if (pPage->Child == nullptr)
  {
    pPage->Child = hb_cdxPageNew(pPage->TagParent, pPage, ulPage);
  }
}

static int hb_cdxPageKeyLeafBalance(LPCDXPAGE pPage, int iChildRet)
{
  LPCDXPAGE childs[CDX_BALANCE_LEAFPAGES + 2], lpTmpPage;
  int iChKeys[CDX_BALANCE_LEAFPAGES + 2], iChFree[CDX_BALANCE_LEAFPAGES + 2];
  int iFirstKey, iBlncKeys = CDX_BALANCE_LEAFPAGES;
  int iLen = pPage->TagParent->uiLen + 8, iKeys = 0, iFree = 0, iSkip = 0, iBufSize = 0;
  HB_BYTE *pKeyPool = nullptr, *pPtr;
  auto fIns = false;
  int iRet = 0, iDup, iMax, i;
#ifndef HB_CDX_PACKTRAIL
  int iTmp;
#endif

#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxPageCheckKeys(pPage);
#endif

  if (pPage->iCurKey > 0)
  {
    iFirstKey = pPage->iCurKey - 1;
  }
  else
  {
    iFirstKey = 0;
    --iBlncKeys;
    if (pPage->Left != CDX_DUMMYNODE)
    {
      iRet |= NODE_BALANCE;
    }
  }
  if (iBlncKeys > pPage->iKeys - iFirstKey)
  {
    iBlncKeys = pPage->iKeys - iFirstKey;
    if (pPage->Right != CDX_DUMMYNODE)
    {
      iRet |= NODE_BALANCE;
    }
  }

#ifdef HB_CDX_DSPDBG_INFO
  fprintf(stderr, "\r\nleaf balance: Page=%lx (%d/%d)", pPage->Page, iFirstKey, iBlncKeys);
  fflush(stderr);
#endif

  if ((iChildRet & (NODE_SPLIT | NODE_JOIN)) == 0 && (iBlncKeys < 2 || (iChildRet & NODE_BALANCE) == 0))
  {
    return iRet;
  }

  for (i = 0; i < iBlncKeys; i++)
  {
    HB_ULONG ulPage = hb_cdxPageGetKeyPage(pPage, iFirstKey + i);
    if (pPage->Child && pPage->Child->Page == ulPage)
    {
      childs[i] = pPage->Child;
      pPage->Child = nullptr;
    }
    else
    {
      childs[i] = hb_cdxPageNew(pPage->TagParent, pPage, ulPage);
    }

#ifdef HB_CDX_DBGCODE
    if (i > 0 && (childs[i]->Page != childs[i - 1]->Right || childs[i]->Left != childs[i - 1]->Page))
    {
      fprintf(stderr, "\r\nchilds[%d]->Page=%lx, childs[%d]->Right=%lx, childs[%d]->Page=%lx, childs[%d]->Left=%lx",
              i - 1, childs[i - 1]->Page, i - 1, childs[i - 1]->Right, i, childs[i]->Page, i, childs[i]->Left);
      fflush(stderr);
      hb_cdxErrInternal("hb_cdxPageKeyLeafBalance: index corrupted.");
    }
#endif
    iChKeys[i] = childs[i]->iKeys;
    iChFree[i] = childs[i]->iFree;
    if (childs[i]->iFree >= childs[i]->ReqByte) // TODO: increase limit for last page
    {
      iFree += childs[i]->iFree;
    }
    else if (childs[i]->iFree >= 0)
    {
      if (i == iSkip)
      {
        ++iSkip;
      }
#if 1
      else if (i + 1 == iBlncKeys && (iChildRet & NODE_SPLIT) == 0)
      {
        iBlncKeys--;
        hb_cdxPageFree(childs[i], false);
      }
#endif
    }
    if (i >= iSkip && i < iBlncKeys)
    {
      iKeys += childs[i]->iKeys;
    }

#ifdef HB_CDX_DSPDBG_INFO
    fprintf(stderr, ", childs[%d]->Page=%lx(%d/%d)", i, childs[i]->Page, childs[i]->iKeys, childs[i]->iFree);
    fprintf(stderr, "(%d/%d/%d:%d,%lx)", i, iSkip, iBlncKeys, iKeys, childs[i]->Right);
    fflush(stderr);
#endif
  }
  if ((iChildRet & NODE_SPLIT) == 0)
  {
    for (i = iBlncKeys - 1; i > iSkip && childs[i]->iFree >= 0 && childs[i]->iFree < childs[i]->ReqByte; i--)
    {
      iKeys -= childs[i]->iKeys;
      hb_cdxPageFree(childs[i], false);
      iBlncKeys--;
    }
  }
  if ((iChildRet & (NODE_SPLIT | NODE_JOIN)) == 0 &&
      (iBlncKeys < 2 || iFree < pPage->TagParent->pIndex->uiPageLen - CDX_EXT_HEADSIZE))
  {
    for (i = 0; i < iBlncKeys; i++)
    {
      hb_cdxPageFree(childs[i], false);
    }
    return iRet;
  }
#ifdef HB_CDX_DSPDBG_INFO
  fprintf(stderr, "\r\nleaf balance: Page=%lx iKeys=%d", pPage->Page, iKeys);
  fflush(stderr);
#endif
  if (iKeys > 0)
  {
    iBufSize = iKeys;
    pPtr = pKeyPool = static_cast<HB_BYTE *>(hb_xgrab(iBufSize * iLen));
    for (i = iSkip; i < iBlncKeys && iKeys > 0; i++)
    {
      if (childs[i]->iKeys > 0)
      {
        if (childs[i]->pKeyBuf)
        {
          memcpy(pPtr, childs[i]->pKeyBuf, childs[i]->iKeys * iLen);
        }
        else
        {
          hb_cdxPageLeafDecode(childs[i], pPtr);
        }
        // update number of duplicate characters when join pages
        if (pPtr > pKeyPool)
        {
#ifdef HB_CDX_PACKTRAIL
          iMax = iLen - 8 - HB_GET_LE_UINT16(&pPtr[iLen - 2]);
#else
          iTmp = HB_GET_LE_UINT16(&pPtr[-2]);
          iMax = HB_GET_LE_UINT16(&pPtr[iLen - 2]);
          iMax = iLen - 8 - HB_MAX(iMax, iTmp);
#endif
          iDup = 0;
          while (iDup < iMax && pPtr[iDup] == pPtr[iDup - iLen])
          {
            ++iDup;
          }
          HB_PUT_LE_UINT16(&pPtr[iLen - 4], iDup);
          if (iSkip == i - 1 && childs[iSkip]->iFree >= 0 &&
              iLen - 8 - iDup - HB_GET_LE_UINT16(&pPtr[iLen - 2]) > childs[iSkip]->iFree - childs[iSkip]->ReqByte)
          {
            memmove(pKeyPool, pPtr, childs[i]->iKeys * iLen);
            pPtr = pKeyPool;
            iKeys -= childs[i - 1]->iKeys;
            iSkip++;
#ifdef HB_CDX_DSPDBG_INFO
            fprintf(stderr, "\r\niSkip=%d, iBlncKeys=%d", iSkip, iBlncKeys);
            fflush(stderr);
#endif
          }
        }
        pPtr += childs[i]->iKeys * iLen;
#ifdef HB_CDX_DSPDBG_INFO
        fprintf(stderr, ", childs[%d]->iKeys=%d", i, childs[i]->iKeys);
        fflush(stderr);
#endif
      }
    }
  }

#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxPageCheckDupTrl(pPage, pKeyPool, iKeys, false);
#endif
  pPtr = pKeyPool;
  fIns = false;
  i = iSkip;
  while (iKeys > 0)
  {
    if (i == iBlncKeys)
    {
      if (childs[i - 1]->Right != CDX_DUMMYNODE)
      {
        lpTmpPage = hb_cdxPageNew(pPage->TagParent, pPage, childs[i - 1]->Right);
      }
      else
      {
        lpTmpPage = nullptr;
      }

#if 1
      if (!fIns && lpTmpPage != nullptr)
      {
        int j, iSize = 0, iMaxReq;
        HB_ULONG ulMaxRec = 0, ul;
        HB_BYTE *pbKey;

        for (j = 0; j < iKeys; j++)
        {
          if (ulMaxRec < (ul = HB_GET_LE_UINT32(&pPtr[(j + 1) * iLen - 8])))
          {
            ulMaxRec = ul;
          }
          iSize += iLen - 8 - (j == 0 ? 0 : HB_GET_LE_UINT16(&pPtr[(j + 1) * iLen - 4])) -
                   HB_GET_LE_UINT16(&pPtr[(j + 1) * iLen - 2]);
        }
        pbKey = hb_cdxPageGetKeyVal(lpTmpPage, 0);
        iMax = hb_cdxPageGetKeyTrl(lpTmpPage, 0);
#ifdef HB_CDX_PACKTRAIL
        iMax = iLen - 8 - iMax;
#else
        iTmp = HB_GET_LE_UINT16(&pPtr[iKeys * iLen - 2]);
        iMax = iLen - 8 - HB_MAX(iTmp, iMax);
#endif
        for (j = 0; j < iMax && pPtr[(iKeys - 1) * iLen + j] == pbKey[j]; j++)
        {
        }
#ifdef HB_CDX_DSPDBG_INFO
        fprintf(stderr, "\r\niDup=%d, iTrl=%d ", j, iLen - 8 - iMax);
        fflush(stderr);
#endif
        iSize -= j;
        iMaxReq = lpTmpPage->ReqByte;
        ul = lpTmpPage->RNMask;
        while (ulMaxRec > ul)
        {
          ++iMaxReq;
          ul = (ul << 8) | 0xFF;
        }
        iSize += iKeys * iMaxReq;
        iSize = lpTmpPage->iFree - iSize - (iMaxReq - lpTmpPage->ReqByte) * lpTmpPage->iKeys;
        if (iSize < 0)
        {
          fIns = true;
        }
        else
        {
#ifdef HB_CDX_DSPDBG_INFO
          fprintf(stderr, "\r\ninserting iDup=%d #keys=%d/%d (%d) parent=%lx, child=%lx (%d), rec=%ld", j, iKeys,
                  lpTmpPage->iKeys, i, pPage->Page, lpTmpPage->Page, iSize,
                  static_cast<HB_ULONG>(HB_GET_LE_UINT32(pPtr + iLen - 8)));
          fflush(stderr);
#endif
          if (iBufSize >= iKeys + lpTmpPage->iKeys)
          {
            memmove(pKeyPool, pPtr, iKeys * iLen);
          }
          else
          {
            iBufSize = iKeys + lpTmpPage->iKeys;
            auto pTmp = static_cast<HB_BYTE *>(hb_xgrab(iBufSize * iLen));
            memcpy(pTmp, pPtr, iKeys * iLen);
            hb_xfree(pKeyPool);
            pKeyPool = pTmp;
          }
          if (lpTmpPage->iKeys > 0)
          {
            pPtr = &pKeyPool[iKeys * iLen];
            if (lpTmpPage->pKeyBuf)
            {
              memcpy(pPtr, lpTmpPage->pKeyBuf, lpTmpPage->iKeys * iLen);
            }
            else
            {
              hb_cdxPageLeafDecode(lpTmpPage, pPtr);
            }
#ifdef HB_CDX_PACKTRAIL
            iMax = iLen - 8 - HB_GET_LE_UINT16(&pPtr[iLen - 2]);
#else
            iTmp = HB_GET_LE_UINT16(&pPtr[-2]);
            iMax = HB_GET_LE_UINT16(&pPtr[iLen - 2]);
            iMax = iLen - 8 - HB_MAX(iMax, iTmp);
#endif
            iDup = 0;
            while (iDup < iMax && pPtr[iDup] == pPtr[iDup - iLen])
            {
              ++iDup;
            }
            HB_PUT_LE_UINT16(&pPtr[iLen - 4], iDup);
            iKeys += lpTmpPage->iKeys;
#ifdef HB_CDX_DSPDBG_INFO
            fprintf(stderr, " iDup2=%d, iTrl2=%d ", iDup, HB_GET_LE_UINT16(&pPtr[iLen - 2]));
            fflush(stderr);
#endif
          }
          pPtr = pKeyPool;
          childs[i] = lpTmpPage;
          if (iFirstKey + i >= pPage->iKeys)
          {
            iRet |= NODE_NEWLASTKEY;
          }
#ifdef HB_CDX_DBGCODE_EXT
          childs[i]->iKeys = 0;
          if (childs[i]->pKeyBuf)
          {
            hb_xfree(childs[i]->pKeyBuf);
            childs[i]->pKeyBuf = nullptr;
            childs[i]->fBufChanged = false;
          }
          hb_cdxPageCalcLeafSpace(childs[i], pPtr, iKeys);
          hb_cdxPageLeafEncode(childs[i], pPtr, childs[i]->iKeys);
          iSize += (iMaxReq - childs[i]->ReqByte) * childs[i]->iKeys;
          if (iSize != childs[i]->iFree)
          {
            fprintf(stderr, "\r\ninserting, iSize=%d, childs[i]->iFree=%d", iSize, childs[i]->iFree);
            fflush(stderr);
            fprintf(stderr, "\r\niKeys=%d, iMaxReq=%d", iKeys, iMaxReq);
            fflush(stderr);
            hb_cdxErrInternal("hb_cdxPageGetChild: index corrupted.");
          }
#endif
        }
#endif
      }
      else
      {
        fIns = true;
      }

      if (fIns)
      {
        childs[i] = hb_cdxPageNew(pPage->TagParent, pPage, 0);
        childs[i]->PageType = CDX_NODE_LEAF;
        // Update siblings links
        childs[i]->Left = childs[i - 1]->Page;
        childs[i]->Right = childs[i - 1]->Right;
        childs[i - 1]->Right = childs[i]->Page;
        childs[i - 1]->fChanged = true;
        if (lpTmpPage != nullptr)
        {
          lpTmpPage->Left = childs[i]->Page;
          lpTmpPage->fChanged = true;
          hb_cdxPageFree(lpTmpPage, false);
        }
        iBlncKeys++;
        iRet |= NODE_BALANCE;
#ifdef HB_CDX_DSPDBG_INFO
        fprintf(stderr, "\r\nleaf balance: new child[%d]->Page=%lx", i, childs[i]->Page);
        fflush(stderr);
#endif
      }
    }
    childs[i]->iKeys = 0;
    if (childs[i]->pKeyBuf)
    {
      hb_xfree(childs[i]->pKeyBuf);
      childs[i]->pKeyBuf = nullptr;
      childs[i]->fBufChanged = false;
    }
    hb_cdxPageCalcLeafSpace(childs[i], pPtr, iKeys);
    if (i == iSkip && i < iBlncKeys && !childs[i]->fChanged && childs[i]->iKeys == iChKeys[i] &&
        childs[i]->iFree == iChFree[i])
    {
#ifdef HB_CDX_DSPDBG_INFO
      fprintf(stderr, "\r\niskip++\r\n");
      fflush(stderr);
#endif
      iSkip++;
    }
    else
    {
      hb_cdxPageLeafEncode(childs[i], pPtr, childs[i]->iKeys);
    }
    pPtr += childs[i]->iKeys * iLen;
    iKeys -= childs[i]->iKeys;
    // update parent key
    if (i < iBlncKeys)
    {
      hb_cdxPageIntSetKey(pPage, iFirstKey + i, fIns, pPtr - iLen, HB_GET_LE_UINT32(pPtr - 8), childs[i]->Page);
    }
    else
    {
      iBlncKeys++;
    }
#ifdef HB_CDX_DSPDBG_INFO
    fprintf(stderr, " (%d/%d)", childs[i]->iKeys, childs[i]->iFree);
    fflush(stderr);
#endif
#ifdef HB_CDX_DBGCODE_EXT
    hb_cdxPageCheckKeys(childs[i]);
#endif
    i++;
  }
  if (i < iBlncKeys)
  {
    // Update siblings links
#if 1
    if (childs[iBlncKeys - 1]->Right != CDX_DUMMYNODE && (i > 1 || (i == 1 && childs[0]->Left == CDX_DUMMYNODE)))
    {
      HB_ULONG Page;
      Page = childs[iBlncKeys - 1]->Page;
      childs[iBlncKeys - 1]->Page = childs[i - 1]->Page;
      childs[i - 1]->Page = Page;
      hb_cdxPageIntSetKey(pPage, iFirstKey + i - 1, false, nullptr, 0, Page);
      childs[i - 1]->Right = childs[iBlncKeys - 1]->Right;
      childs[i - 1]->fChanged = true;
      if (i > 1)
      {
        childs[i - 2]->Right = Page;
        childs[i - 2]->fChanged = true;
      }
    }
    else
#endif
    {
      HB_ULONG Left, Right;
      Right = childs[iBlncKeys - 1]->Right;
      if (i > 0)
      {
        Left = childs[i - 1]->Page;
        childs[i - 1]->Right = Right;
        childs[i - 1]->fChanged = true;
      }
      else
      {
        Left = childs[0]->Left;
        if (Left != CDX_DUMMYNODE)
        {
          lpTmpPage = hb_cdxPageNew(pPage->TagParent, pPage, Left);
          lpTmpPage->Right = Right;
          lpTmpPage->fChanged = true;
          hb_cdxPageFree(lpTmpPage, false);
        }
      }
      if (Right != CDX_DUMMYNODE)
      {
        lpTmpPage = hb_cdxPageNew(pPage->TagParent, pPage, Right);
        lpTmpPage->Left = Left;
        lpTmpPage->fChanged = true;
        hb_cdxPageFree(lpTmpPage, false);
      }
    }
    // Unlink empty pages from parent
    while (i < iBlncKeys)
    {
      // Delete parent key
      iBlncKeys--;
#ifdef HB_CDX_DSPDBG_INFO
      fprintf(stderr, "\r\nleaf balance: free child[%d]->Page=%lx", iBlncKeys, childs[iBlncKeys]->Page);
      fflush(stderr);
#endif
      if (childs[iBlncKeys]->pKeyBuf)
      {
        hb_xfree(childs[iBlncKeys]->pKeyBuf);
        childs[iBlncKeys]->pKeyBuf = nullptr;
        childs[iBlncKeys]->fBufChanged = false;
      }
      hb_cdxPageIntDelKey(pPage, iFirstKey + iBlncKeys);
      childs[iBlncKeys]->Owner = nullptr;
      childs[iBlncKeys]->fChanged = false;
      childs[iBlncKeys]->PageType = CDX_NODE_UNUSED;
      childs[iBlncKeys]->Left = CDX_DUMMYNODE;
      childs[iBlncKeys]->Right = CDX_DUMMYNODE;
      hb_cdxPageFree(childs[iBlncKeys], false);
    }
    iRet |= NODE_BALANCE;
  }
  for (i = 0; i < iBlncKeys; i++)
  {
    hb_cdxPageFree(childs[i], false);
  }

  if (pKeyPool)
  {
    hb_xfree(pKeyPool);
  }
  pPage->fChanged = true;
#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxPageCheckKeys(pPage);
#endif
  if (pPage->iKeys > pPage->TagParent->MaxKeys)
  {
    iRet |= NODE_SPLIT;
  }
  return iRet;
}

static int hb_cdxPageKeyIntBalance(LPCDXPAGE pPage, int iChildRet)
{
  LPCDXPAGE childs[CDX_BALANCE_INTPAGES + 2], lpTmpPage;
  int iFirstKey, iBlncKeys = CDX_BALANCE_INTPAGES;
  int iLen = pPage->TagParent->uiLen + 8, iKeys = 0, iNeedKeys, iMin = pPage->TagParent->MaxKeys, iMax = 0, iDiv;
  HB_BYTE *pKeyPool = nullptr, *pPtr;
  bool fForce = (iChildRet & (NODE_SPLIT | NODE_JOIN)) != 0;
  int iRet = 0, i;

  if (!fForce && (iChildRet & NODE_BALANCE) == 0)
  {
    return iRet;
  }

  if (pPage->Child && pPage->Child->Child)
  {
    hb_cdxPageFree(pPage->Child->Child, false);
  }

#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxPageCheckKeys(pPage);
#endif

  if (pPage->iKeys <= iBlncKeys || pPage->iCurKey <= iBlncKeys / 2)
  {
    iFirstKey = 0;
  }
  else if (pPage->iCurKey + (iBlncKeys >> 1) >= pPage->iKeys)
  {
    iFirstKey = pPage->iKeys - iBlncKeys;
  }
  else
  {
    iFirstKey = pPage->iCurKey - (iBlncKeys >> 1);
  }
  if (iBlncKeys > pPage->iKeys - iFirstKey)
  {
    iBlncKeys = pPage->iKeys - iFirstKey;
    iRet |= NODE_BALANCE;
  }

#ifdef HB_CDX_DSPDBG_INFO
  fprintf(stderr, "\r\nbalance: Page=%lx(%d) (%d/%d)", pPage->Page, pPage->iKeys, iFirstKey, iBlncKeys);
  fflush(stderr);
#endif

  if (!fForce && iBlncKeys < 2)
  {
    return iRet;
  }

  for (i = 0; i < iBlncKeys; i++)
  {
    HB_ULONG ulPage = hb_cdxPageGetKeyPage(pPage, iFirstKey + i);
    if (pPage->Child && pPage->Child->Page == ulPage)
    {
      childs[i] = pPage->Child;
      pPage->Child = nullptr;
    }
    else
    {
      childs[i] = hb_cdxPageNew(pPage->TagParent, pPage, ulPage);
    }

#ifdef HB_CDX_DBGCODE
    if (i > 0 && (childs[i]->Page != childs[i - 1]->Right || childs[i]->Left != childs[i - 1]->Page))
    {
      fprintf(stderr, "\r\nchilds[%d]->Page=%lx, childs[%d]->Right=%lx, childs[%d]->Page=%lx, childs[%d]->Left=%lx",
              i - 1, childs[i - 1]->Page, i - 1, childs[i - 1]->Right, i, childs[i]->Page, i, childs[i]->Left);
      fflush(stderr);
      hb_cdxErrInternal("hb_cdxPageKeyIntBalance: index corrupted.");
    }
#endif
    iKeys += childs[i]->iKeys;

    if (childs[i]->iKeys > iMax)
    {
      iMax = childs[i]->iKeys;
    }
    if (childs[i]->iKeys < iMin)
    {
      iMin = childs[i]->iKeys;
    }
#ifdef HB_CDX_DSPDBG_INFO
    fprintf(stderr, ", childs[%d]->Page=%lx(%d)", i, childs[i]->Page, childs[i]->iKeys);
    fflush(stderr);
#endif
  }
  iNeedKeys = (iKeys + pPage->TagParent->MaxKeys - 1) / pPage->TagParent->MaxKeys;
#if 1
  if (iNeedKeys == 1 && iBlncKeys > 1 && childs[0]->Left != CDX_DUMMYNODE &&
      childs[iBlncKeys - 1]->Right != CDX_DUMMYNODE && iKeys >= (CDX_BALANCE_INTPAGES << 1) &&
      iKeys > ((pPage->TagParent->MaxKeys * 3) >> 1))
  {
    iNeedKeys = 2;
  }
#endif
#if 1
  iDiv = HB_MAX(iMax - iMin - (pPage->TagParent->MaxKeys >> 1) + 1, iBlncKeys - iNeedKeys);
#else
  iDiv = iMax - iMin;
#endif
  if (iKeys > 0 && (iDiv >= 2 || fForce))
  {
#if 1
    if (iBlncKeys == 1 && iKeys > pPage->TagParent->MaxKeys && childs[0]->Right != CDX_DUMMYNODE)
    {
      lpTmpPage = hb_cdxPageNew(pPage->TagParent, pPage, childs[0]->Right);
      iKeys += lpTmpPage->iKeys;
      childs[iBlncKeys++] = lpTmpPage;
      if (iFirstKey + iBlncKeys > pPage->iKeys)
      {
        iRet |= NODE_NEWLASTKEY;
      }
      iNeedKeys = (iKeys + pPage->TagParent->MaxKeys - 1) / pPage->TagParent->MaxKeys;
    }
    else
#endif
    {
      iMin = HB_MAX(iKeys / iNeedKeys, 2);
      iMax = HB_MAX((iKeys + iNeedKeys - 1) / iNeedKeys, iMin);
      for (i = iBlncKeys - 1; i > 1 && childs[i]->iKeys >= iMin && childs[i]->iKeys <= iMax; i--)
      {
        iKeys -= childs[i]->iKeys;
        hb_cdxPageFree(childs[i], false);
        iBlncKeys--;
        iMin = HB_MAX(iKeys / iNeedKeys, 2);
        iMax = HB_MAX((iKeys + iNeedKeys - 1) / iNeedKeys, iMin);
      }
      while (iBlncKeys > 2 && childs[0]->iKeys >= iMin && childs[0]->iKeys <= iMax)
      {
        iKeys -= childs[0]->iKeys;
        hb_cdxPageFree(childs[0], false);
        iBlncKeys--;
        iFirstKey++;
        for (i = 0; i < iBlncKeys; i++)
        {
          childs[i] = childs[i + 1];
        }
        iMin = HB_MAX(iKeys / iNeedKeys, 2);
        iMax = HB_MAX((iKeys + iNeedKeys - 1) / iNeedKeys, iMin);
      }
    }
  }
  if (!fForce && (iBlncKeys < 2 || iDiv < 2))
  {
    for (i = 0; i < iBlncKeys; i++)
    {
      hb_cdxPageFree(childs[i], false);
    }
    return iRet;
  }

  if (iKeys > 0)
  {
    pPtr = pKeyPool = static_cast<HB_BYTE *>(hb_xgrab(iKeys * iLen));
    for (i = 0; i < iBlncKeys; i++)
    {
      if (childs[i]->iKeys > 0)
      {
        memcpy(pPtr, hb_cdxPageIntKeyPool(childs[i]), childs[i]->iKeys * iLen);
        pPtr += childs[i]->iKeys * iLen;
      }
    }
  }

  if (iNeedKeys > iBlncKeys)
  {
    if (iBlncKeys < 2)
    {
      i = iBlncKeys;
    }
    else
    {
      i = iBlncKeys - 1;
      childs[iBlncKeys] = childs[i];
    }
    childs[i] = hb_cdxPageNew(pPage->TagParent, pPage, 0);
    childs[i]->PageType = CDX_NODE_BRANCH;
    childs[i]->iKeys = 0;
    childs[i]->fChanged = true;
    // Add new parent key
    hb_cdxPageIntSetKey(pPage, iFirstKey + i, true, nullptr, 0, childs[iBlncKeys]->Page);
    // Update siblings links
    childs[i]->Left = childs[i - 1]->Page;
    childs[i]->Right = childs[i - 1]->Right;
    childs[i - 1]->Right = childs[i]->Page;
    if (i < iBlncKeys)
    {
      childs[i + 1]->Left = childs[i]->Page;
    }
    else if (childs[i]->Right != CDX_DUMMYNODE)
    {
      lpTmpPage = hb_cdxPageNew(pPage->TagParent, pPage, childs[iBlncKeys]->Right);
      lpTmpPage->Left = childs[i]->Page;
      lpTmpPage->fChanged = true;
      hb_cdxPageFree(lpTmpPage, false);
    }
#ifdef HB_CDX_DSPDBG_INFO
    fprintf(stderr, "\r\nint balance: new child[%d]->Page=%lx", iBlncKeys, childs[iBlncKeys]->Page);
    fflush(stderr);
#endif
    iBlncKeys++;
    iRet |= NODE_BALANCE;
  }
  else if (iNeedKeys < iBlncKeys)
  {
    HB_ULONG Left, Right;

    // Update siblings links
    if (iNeedKeys > 1)
    {
      childs[iNeedKeys - 2]->Right = childs[iBlncKeys - 1]->Page;
      childs[iBlncKeys - 1]->Left = childs[iNeedKeys - 2]->Page;
      lpTmpPage = childs[iBlncKeys - 1];
      childs[iBlncKeys - 1] = childs[iNeedKeys - 1];
      childs[iNeedKeys - 1] = lpTmpPage;
    }
    else if (iNeedKeys > 0 && childs[0]->Left == CDX_DUMMYNODE)
    {
      lpTmpPage = childs[iBlncKeys - 1];
      childs[iBlncKeys - 1] = childs[0];
      childs[0] = lpTmpPage;
      childs[0]->Left = CDX_DUMMYNODE;
    }
    else
    {
      Right = childs[iBlncKeys - 1]->Right;
      if (iNeedKeys > 0)
      {
        Left = childs[iNeedKeys - 1]->Page;
        childs[iNeedKeys - 1]->Right = Right;
      }
      else
      {
        Left = childs[0]->Left;
        if (Left != CDX_DUMMYNODE)
        {
          lpTmpPage = hb_cdxPageNew(pPage->TagParent, pPage, Left);
          lpTmpPage->Right = Right;
          lpTmpPage->fChanged = true;
          hb_cdxPageFree(lpTmpPage, false);
        }
      }
      if (Right != CDX_DUMMYNODE)
      {
        lpTmpPage = hb_cdxPageNew(pPage->TagParent, pPage, Right);
        lpTmpPage->Left = Left;
        lpTmpPage->fChanged = true;
        hb_cdxPageFree(lpTmpPage, false);
      }
    }
    // Unlink empty pages from parent
    for (i = iBlncKeys - 1; i >= iNeedKeys; i--)
    {
      // Delete parent key
#ifdef HB_CDX_DSPDBG_INFO
      fprintf(stderr, "\r\nbalance: free child[%d]->Page=%lx", i, childs[i]->Page);
      fflush(stderr);
#endif
      hb_cdxPageIntDelKey(pPage, iFirstKey + i);
      childs[i]->Owner = nullptr;
      childs[i]->fChanged = false;
      childs[i]->PageType = CDX_NODE_UNUSED;
      childs[i]->Left = CDX_DUMMYNODE;
      childs[i]->Right = CDX_DUMMYNODE;
      childs[i]->iKeys = 0;
      hb_cdxPageFree(childs[i], false);
    }
    iBlncKeys = iNeedKeys;
    iRet |= NODE_BALANCE;
  }

  // Redistribute childs internal node's keys and update parent keys
  if (iKeys > 0)
  {
    fForce = pPage->TagParent->MaxKeys == 2 && iBlncKeys > 2 && iKeys == (iBlncKeys << 1) - 1;
    pPtr = pKeyPool;
    for (i = 0; i < iBlncKeys; i++)
    {
      int iNodeKeys = (fForce && i == 1) ? 1 : ((iKeys + iBlncKeys - i - 1) / (iBlncKeys - i));
#ifdef HB_CDX_DBGCODE
      if (iNodeKeys > pPage->TagParent->MaxKeys)
      {
        hb_cdxErrInternal("hb_cdxPageKeyIntBalance: iNodeKeys calculated wrong!");
      }
#endif
      // TODO: do nothing if iNodeKeys == childs[i]->iKeys && i == iSkip
      memcpy(hb_cdxPageIntKeyPool(childs[i]), pPtr, iNodeKeys * iLen);
      childs[i]->iKeys = iNodeKeys;
      childs[i]->fChanged = true;
      pPtr += iNodeKeys * iLen;
      iKeys -= iNodeKeys;
      // update parent key
      if (iFirstKey + i < pPage->iKeys)
      {
        hb_cdxPageIntSetKey(pPage, iFirstKey + i, false, pPtr - iLen, HB_GET_BE_UINT32(pPtr - 8), childs[i]->Page);
      }
#ifdef HB_CDX_DSPDBG_INFO
      fprintf(stderr, " (%d)", childs[i]->iKeys);
#endif
#ifdef HB_CDX_DBGCODE_EXT
      hb_cdxPageCheckKeys(childs[i]);
#endif
      hb_cdxPageFree(childs[i], false);
    }
    hb_xfree(pKeyPool);
  }
  pPage->fChanged = true;
#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxPageCheckKeys(pPage);
#endif
  if (pPage->iKeys > pPage->TagParent->MaxKeys)
  {
    iRet |= NODE_SPLIT;
  }
  return iRet;
}

// balance keys in child pages
static int hb_cdxPageBalance(LPCDXPAGE pPage, int iChildRet)
{
  int iRet = 0;

  if ((pPage->PageType & CDX_NODE_LEAF) != 0)
  {
    iRet = iChildRet;
  }
  else
  {
    if (iChildRet & NODE_NEWLASTKEY)
    {
      if (pPage->Child->iKeys == 0)
      {
        iChildRet |= NODE_JOIN;
        iRet |= NODE_NEWLASTKEY;
      }
      else
      {
        hb_cdxPageIntSetKey(pPage, pPage->iCurKey, false, hb_cdxPageGetKeyVal(pPage->Child, pPage->Child->iKeys - 1),
                            hb_cdxPageGetKeyRec(pPage->Child, pPage->Child->iKeys - 1), pPage->Child->Page);
#ifdef HB_CDX_DBGCODE_EXT
        hb_cdxPageCheckKeys(pPage);
#endif
        pPage->fChanged = true;
        if (pPage->iCurKey >= pPage->iKeys - 1)
        {
          iRet |= NODE_NEWLASTKEY;
        }
      }
    }
    if ((pPage->Child->PageType & CDX_NODE_LEAF) != 0)
    {
      iRet |= hb_cdxPageKeyLeafBalance(pPage, iChildRet);
    }
    else
    {
      iRet |= hb_cdxPageKeyIntBalance(pPage, iChildRet);
    }
  }
  if (!pPage->Owner)
  {
    if (pPage->iKeys == 0)
    {
      pPage->PageType |= CDX_NODE_LEAF;
      hb_cdxPageLeafInitSpace(pPage);
    }
    else if (iRet & NODE_SPLIT)
    {
      iRet = hb_cdxPageRootSplit(pPage);
    }
  }
  return iRet;
}

// split Root Page
static int hb_cdxPageRootSplit(LPCDXPAGE pPage)
{
  LPCDXPAGE pNewRoot;
  HB_ULONG ulPage;

  pNewRoot = hb_cdxPageNew(pPage->TagParent, nullptr, 0);
  // do not change root page address if it's unnecessary
  // so we don't have to update Tag header
  pPage->TagParent->RootPage = pNewRoot;
  ulPage = pNewRoot->Page;
  pNewRoot->Page = pPage->Page;
  pPage->Page = ulPage;

  pPage->Owner = pNewRoot;
  pPage->PageType &= ~CDX_NODE_ROOT;
  pNewRoot->PageType = CDX_NODE_ROOT | CDX_NODE_BRANCH;
  pNewRoot->fChanged = true;
  pNewRoot->Child = pPage;
  pNewRoot->iCurKey = 0;
  hb_cdxPageIntSetKey(pNewRoot, 0, true, hb_cdxPageGetKeyVal(pPage, pPage->iKeys - 1),
                      hb_cdxPageGetKeyRec(pPage, pPage->iKeys - 1), pPage->Page);
#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxPageCheckKeys(pNewRoot);
  hb_cdxTagPoolCheck(pPage->TagParent);
#endif
  hb_cdxPageBalance(pNewRoot, NODE_SPLIT);
  return 0;
}

// remove current Key from Tag
static int hb_cdxPageKeyRemove(LPCDXPAGE pPage)
{
  int iChildRet;

  if (pPage->PageType & CDX_NODE_LEAF)
  {
    iChildRet = hb_cdxPageLeafDelKey(pPage);
  }
  else
  { // interior node
    iChildRet = hb_cdxPageKeyRemove(pPage->Child);
  }
  return hb_cdxPageBalance(pPage, iChildRet);
}

// add Key to Tag at current position
static int hb_cdxPageKeyInsert(LPCDXPAGE pPage, LPCDXKEY pKey)
{
  int iChildRet;

  if (pPage->PageType & CDX_NODE_LEAF)
  {
    iChildRet = hb_cdxPageLeafAddKey(pPage, pKey);
  }
  else
  { // interior node
    iChildRet = hb_cdxPageKeyInsert(pPage->Child, pKey);
  }
#ifdef HB_CDX_DBGUPDT
  cdxTmpStackSize++;
#endif
  return hb_cdxPageBalance(pPage, iChildRet);
}

// Store Tag header to index files
static void hb_cdxTagHeaderStore(LPCDXTAG pTag)
{
  HB_USHORT uiKeyLen, uiForLen;
  CDXTAGHEADER tagHeader;

  if (!pTag->TagChanged)
  {
    return;
  }

  // TODO: !!! read the following field from the index file,
  //       at least freePtr has to be read for pTag->TagBlock == 0
  // tagHeader.freePtr  [4]      offset of list of free pages or -1
  // tagHeader.reserved1[4]      Version number ???
  // tagHeader.reserved2[486]

  pTag->TagChanged = false;
  pTag->OptFlags &= ~(CDX_TYPE_UNIQUE | CDX_TYPE_FORFILTER | CDX_TYPE_PARTIAL | CDX_TYPE_CUSTOM);
  if (pTag->UniqueKey)
  {
    pTag->OptFlags |= CDX_TYPE_UNIQUE;
  }
  if (pTag->pForItem != nullptr)
  {
    pTag->OptFlags |= CDX_TYPE_FORFILTER;
  }
#if defined(HB_SIXCDX)
  if (pTag->Custom)
  {
    pTag->OptFlags |= CDX_TYPE_PARTIAL | CDX_TYPE_CUSTOM;
  }
  else if (pTag->ChgOnly)
  {
    pTag->OptFlags |= CDX_TYPE_CUSTOM;
  }
  else if (pTag->Partial)
  {
    pTag->OptFlags |= CDX_TYPE_PARTIAL;
  }
#else
  if (pTag->Custom)
  {
    pTag->OptFlags |= CDX_TYPE_CUSTOM;
  }
  if (pTag->Partial)
  {
    pTag->OptFlags |= CDX_TYPE_PARTIAL;
  }
#endif

  memset(&tagHeader, 0, sizeof(tagHeader));
  HB_PUT_LE_UINT32(tagHeader.rootPtr, pTag->RootBlock);
  HB_PUT_LE_UINT16(tagHeader.keySize, pTag->uiLen);
  HB_PUT_LE_UINT16(tagHeader.headerLen, pTag->pIndex->uiHeaderLen);
  HB_PUT_LE_UINT16(tagHeader.pageLen, pTag->pIndex->uiPageLen);
  tagHeader.indexOpt = pTag->OptFlags;
  if (pTag->TagBlock == 0)
  {
    HB_PUT_BE_UINT32(tagHeader.signature, CDX_HARBOUR_SIGNATURE);
    tagHeader.indexSig = pTag->pIndex->fLargeFile ? 0x21 : 0x01;
  }
  else
  {
    tagHeader.indexSig = 0x01;
  }
  if (!pTag->AscendKey)
  {
    HB_PUT_LE_UINT16(tagHeader.ascendFlg, 1);
  }
  if (pTag->IgnoreCase)
  {
    tagHeader.ignoreCase = 1;
  }

  uiKeyLen = pTag->KeyExpr == nullptr ? 0 : static_cast<HB_USHORT>(strlen(pTag->KeyExpr));
  uiForLen = pTag->ForExpr == nullptr ? 0 : static_cast<HB_USHORT>(strlen(pTag->ForExpr));

  if (uiKeyLen + uiForLen > CDX_HEADEREXPLEN - 2)
  {
    hb_cdxErrorRT(pTag->pIndex->pArea, EG_DATAWIDTH, EDBF_KEYLENGTH, nullptr, 0, 0, nullptr);
  }
  else
  {
    HB_PUT_LE_UINT16(tagHeader.keyExpPos, 0);
    HB_PUT_LE_UINT16(tagHeader.keyExpLen, uiKeyLen + 1);
    HB_PUT_LE_UINT16(tagHeader.forExpPos, uiKeyLen + 1);
    HB_PUT_LE_UINT16(tagHeader.forExpLen, uiForLen + 1);
    if (uiKeyLen > 0)
    {
      memcpy(tagHeader.keyExpPool, pTag->KeyExpr, uiKeyLen);
    }
    if (uiForLen > 0)
    {
      memcpy(tagHeader.keyExpPool + uiKeyLen + 1, pTag->ForExpr, uiForLen);
    }
  }
  hb_cdxIndexPageWrite(pTag->pIndex, pTag->TagBlock, reinterpret_cast<const HB_BYTE *>(&tagHeader), sizeof(tagHeader));
}

#if defined(HB_SIXCDX)
static bool hb_cdxIsTemplateFunc(const char *szKeyExpr)
{
  // For CDX format SIx3 really makes something like that
  return hb_strnicmp(szKeyExpr, "sxChar(", 7) == 0 || hb_strnicmp(szKeyExpr, "sxDate(", 7) == 0 ||
         hb_strnicmp(szKeyExpr, "sxNum(", 6) == 0 || hb_strnicmp(szKeyExpr, "sxLog(", 6) == 0;
}
#endif

static bool hb_cdxSetPageSize(LPCDXINDEX pIndex, bool fLargeFile, HB_USHORT uiPageSize, HB_USHORT uiHeaderSize)
{
  if (uiPageSize >= CDX_PAGELEN && uiPageSize <= CDX_PAGELEN_MAX && ((uiPageSize - 1) & uiPageSize) == 0 &&
      uiHeaderSize == CDX_HEADERLEN)
  {
    pIndex->fLargeFile = fLargeFile;
    pIndex->uiHeaderLen = uiHeaderSize;
    pIndex->uiPageLen = uiPageSize;
    pIndex->uiPageBits = CDX_PAGELEN_BITS;
    pIndex->uiMaxKeyLen = CDX_MAXKEY;

    if (uiPageSize > CDX_PAGELEN)
    {
      while (static_cast<HB_INT>(1 << pIndex->uiPageBits) < uiPageSize)
      {
        ++pIndex->uiPageBits;
      }
      pIndex->uiMaxKeyLen = ((uiPageSize - CDX_INT_HEADSIZE) >> 1) - 8;
    }

    pIndex->nextAvail = CDX_DUMMYNODE;

    return true;
  }
  return false;
}

// Read a tag definition from the index file
static void hb_cdxTagLoad(LPCDXTAG pTag)
{
  CDXTAGHEADER tagHeader;
  HB_USHORT uiForPos, uiForLen, uiKeyPos, uiKeyLen;
  HB_ULONG ulRecNo;

  // read the page from a file
  hb_cdxIndexPageRead(pTag->pIndex, pTag->TagBlock, reinterpret_cast<HB_BYTE *>(&tagHeader), sizeof(tagHeader));

  uiForPos = HB_GET_LE_UINT16(tagHeader.forExpPos);
  uiForLen = HB_GET_LE_UINT16(tagHeader.forExpLen);
  uiKeyPos = HB_GET_LE_UINT16(tagHeader.keyExpPos);
  uiKeyLen = HB_GET_LE_UINT16(tagHeader.keyExpLen);

  pTag->RootBlock = HB_GET_LE_UINT32(tagHeader.rootPtr);

  if (pTag->TagBlock == 0)
  {
    auto fLargeFile = false;
    HB_USHORT uiPageLen = CDX_PAGELEN, uiHeaderLen = CDX_HEADERLEN;
    HB_U32 u32Sig = HB_GET_BE_UINT32(tagHeader.signature);

    if (u32Sig == CDX_HARBOUR_SIGNATURE || u32Sig == HB_SWAP_UINT32(CDX_HARBOUR_SIGNATURE))
    {
      fLargeFile = tagHeader.indexSig == 0x21;
      uiHeaderLen = HB_GET_LE_UINT16(tagHeader.headerLen);
      uiPageLen = HB_GET_LE_UINT16(tagHeader.pageLen);
    }

    if (!hb_cdxSetPageSize(pTag->pIndex, fLargeFile, uiPageLen, uiHeaderLen))
    {
      pTag->RootBlock = 0;
    }
  }

  // Return if:
  // no root page allocated
  // invalid root page offset (position inside an index file)
  // invalid key value length
  if (pTag->RootBlock == 0 || !hb_cdxFilePageRootValid(pTag->pIndex, pTag->RootBlock) ||
      hb_cdxFilePageOffset(pTag->pIndex, pTag->RootBlock) >= hb_fileSize(pTag->pIndex->pFile) ||
      HB_GET_LE_UINT16(tagHeader.keySize) > pTag->pIndex->uiMaxKeyLen || uiKeyLen + uiForLen > CDX_HEADEREXPLEN ||
      uiForPos + uiForLen > CDX_HEADEREXPLEN || uiKeyPos + uiKeyLen > CDX_HEADEREXPLEN ||
      (uiKeyPos < uiForPos ? (uiKeyPos + uiKeyLen > uiForPos && tagHeader.keyExpPool[uiForPos])
                           : (uiForPos + uiForLen > uiKeyPos && tagHeader.keyExpPool[uiForPos])))
  {
    pTag->RootBlock = 0; // To force RT error - index corrupted
    return;
  }

  // some wrong RDDs do not set expression length this is workaround for them
  if (uiKeyPos == 0 && uiKeyLen != 0 && uiForPos == 0 && uiForLen != 0)
  {
    uiForPos = uiKeyLen;
  }
  if (!uiKeyLen)
  {
    uiKeyLen = (uiForPos >= uiKeyPos ? uiForPos : CDX_HEADEREXPLEN) - uiKeyPos;
  }
  if (!uiForLen)
  {
    uiForLen = (uiForPos <= uiKeyPos ? uiKeyPos : CDX_HEADEREXPLEN) - uiForPos;
  }

  pTag->KeyExpr = static_cast<char *>(hb_xgrab(uiKeyLen + 1));
  hb_strncpyTrim(pTag->KeyExpr, reinterpret_cast<const char *>(tagHeader.keyExpPool), uiKeyLen);

  pTag->uiLen = HB_GET_LE_UINT16(tagHeader.keySize);
  pTag->MaxKeys = (pTag->pIndex->uiPageLen - CDX_INT_HEADSIZE) / (pTag->uiLen + 8);

  pTag->OptFlags = tagHeader.indexOpt;
  pTag->UniqueKey = (pTag->OptFlags & CDX_TYPE_UNIQUE) != 0;
#if defined(HB_SIXCDX)
  pTag->Custom = (pTag->OptFlags & CDX_TYPE_CUSTOM) != 0 && (pTag->OptFlags & CDX_TYPE_PARTIAL) != 0;
  pTag->ChgOnly = (pTag->OptFlags & CDX_TYPE_CUSTOM) != 0 && (pTag->OptFlags & CDX_TYPE_PARTIAL) == 0;
  pTag->Partial = (pTag->OptFlags & CDX_TYPE_CUSTOM) != 0 || (pTag->OptFlags & CDX_TYPE_PARTIAL) != 0;

  pTag->Template = pTag->Custom && hb_cdxIsTemplateFunc(pTag->KeyExpr);
  // SIx3 does not support repeated key value for the same record
  pTag->MultiKey = false;
#else
  pTag->Partial = (pTag->OptFlags & CDX_TYPE_PARTIAL) != 0;
  pTag->Custom = (pTag->OptFlags & CDX_TYPE_CUSTOM) != 0;
  pTag->ChgOnly = false;
  pTag->Template = pTag->MultiKey = pTag->Custom;
#endif

  pTag->AscendKey = pTag->UsrAscend = (HB_GET_LE_UINT16(tagHeader.ascendFlg) == 0);
  pTag->UsrUnique = false;

  if (tagHeader.indexSig == 0x01 || tagHeader.indexSig == 0x21)
  {
    pTag->IgnoreCase = tagHeader.ignoreCase == 1;
  }
  else
  {
    pTag->IgnoreCase = false;
  }

  if (pTag->OptFlags & CDX_TYPE_STRUCTURE)
  {
    return;
  }

  if (!*pTag->KeyExpr || SELF_COMPILE(&pTag->pIndex->pArea->dbfarea.area, pTag->KeyExpr) == Harbour::FAILURE)
  {
    pTag->RootBlock = 0; // To force RT error - index corrupted
    return;
  }
  pTag->pKeyItem = pTag->pIndex->pArea->dbfarea.area.valResult;
  pTag->pIndex->pArea->dbfarea.area.valResult = nullptr;

  // go to a blank record before testing expression
  ulRecNo = pTag->pIndex->pArea->dbfarea.ulRecNo;
  SELF_GOTO(&pTag->pIndex->pArea->dbfarea.area, 0);

  pTag->uiType = hb_cdxItemType(hb_vmEvalBlockOrMacro(pTag->pKeyItem));
  pTag->bTrail = (pTag->uiType == 'C') ? ' ' : '\0';
  if (pTag->uiType == 'C')
  {
    hb_cdxMakeSortTab(pTag->pIndex->pArea);
  }
  else
  {
    pTag->IgnoreCase = false;
  }

  pTag->nField = hb_rddFieldExpIndex(&pTag->pIndex->pArea->dbfarea.area, pTag->KeyExpr);

  // Check if there is a FOR expression: pTag->OptFlags & CDX_TYPE_FORFILTER
  if (tagHeader.keyExpPool[uiForPos] != 0)
  {
    pTag->ForExpr = static_cast<char *>(hb_xgrab(uiForLen + 1));
    hb_strncpyTrim(pTag->ForExpr, reinterpret_cast<const char *>(tagHeader.keyExpPool) + uiForPos, uiForLen);
    if (SELF_COMPILE(&pTag->pIndex->pArea->dbfarea.area, pTag->ForExpr) == Harbour::FAILURE)
    {
      pTag->RootBlock = 0; // To force RT error - index corrupted
    }
    else
    {
      pTag->pForItem = pTag->pIndex->pArea->dbfarea.area.valResult;
      pTag->pIndex->pArea->dbfarea.area.valResult = nullptr;

      // CL52 / SIXCDX when index is open evaluates only KEY expression
      // and do not check the FOR one.
      // CL53 / COMIX evaluates both KEY and FOR expressions.
#if !defined(HB_SIXCDX)
      if (hb_cdxItemType(hb_vmEvalBlockOrMacro(pTag->pForItem)) != 'L')
      {
        hb_cdxErrorRT(pTag->pIndex->pArea, EG_DATATYPE, EDBF_INVALIDFOR, nullptr, 0, 0, nullptr);
        pTag->RootBlock = 0; // To force RT error - index corrupted
      }
#endif
    }
  }
  SELF_GOTO(&pTag->pIndex->pArea->dbfarea.area, ulRecNo);

  if (pTag->uiLen > pTag->pIndex->uiMaxKeyLen || pTag->uiType == 'U' ||
      (pTag->uiType == 'N' && pTag->uiLen != 8 && pTag->uiLen != 4) || (pTag->uiType == 'D' && pTag->uiLen != 8) ||
      (pTag->uiType == 'T' && pTag->uiLen != 8) || (pTag->uiType == 'L' && pTag->uiLen != 1))
  {
    hb_cdxErrorRT(pTag->pIndex->pArea, pTag->uiType == 'U' ? EG_DATATYPE : EG_DATAWIDTH, EDBF_INVALIDKEY, nullptr, 0, 0,
                  nullptr);
    pTag->RootBlock = 0; // To force RT error - index corrupted
  }
}

// release structure with a tag information from memory
static void hb_cdxTagFree(LPCDXTAG pTag)
{
  if (pTag->RootPage != nullptr)
  {
    hb_cdxPageFree(pTag->RootPage, false);
    pTag->RootPage = nullptr;
  }
  hb_cdxTagPoolFlush(pTag);
  hb_cdxTagPoolFree(pTag, 0);
  if (pTag->TagChanged)
  {
    hb_cdxTagHeaderStore(pTag);
  }
  if (pTag->szName != nullptr)
  {
    hb_xfree(pTag->szName);
  }
  if (pTag->KeyExpr != nullptr)
  {
    hb_xfree(pTag->KeyExpr);
  }
  if (pTag->pKeyItem != nullptr)
  {
    hb_vmDestroyBlockOrMacro(pTag->pKeyItem);
  }
  if (pTag->ForExpr != nullptr)
  {
    hb_xfree(pTag->ForExpr);
  }
  if (pTag->pForItem != nullptr)
  {
    hb_vmDestroyBlockOrMacro(pTag->pForItem);
  }
  hb_cdxKeyFree(pTag->CurKey);
  if (pTag->HotKey)
  {
    hb_cdxKeyFree(pTag->HotKey);
  }
  hb_cdxTagClearScope(pTag, 0);
  hb_cdxTagClearScope(pTag, 1);
  hb_xfree(pTag);
}

// Creates a new structure with a tag information
// TagHdr = offset of index page where a tag header is stored
//            if CDX_DUMMYNODE then allocate space for a new tag header
static LPCDXTAG hb_cdxTagNew(LPCDXINDEX pIndex, const char *szTagName, HB_ULONG TagHdr)
{
  char szName[CDX_MAXTAGNAMELEN + 1];

  auto pTag = static_cast<LPCDXTAG>(hb_xgrab(sizeof(CDXTAG)));
  memset(pTag, 0, sizeof(CDXTAG));
  hb_strncpyUpperTrim(szName, szTagName, sizeof(szName) - 1);
  pTag->szName = hb_strdup(szName);
  pTag->pIndex = pIndex;
  pTag->AscendKey = pTag->UsrAscend = true;
  pTag->UsrUnique = pTag->IgnoreCase = false;
  pTag->uiType = 'C';
  pTag->bTrail = ' ';
  pTag->CurKey = hb_cdxKeyNew(0);
  if (TagHdr == CDX_DUMMYNODE)
  {
    pTag->TagBlock = hb_cdxIndexGetAvailPage(pIndex, true);
    pTag->TagChanged = true;
    pTag->OptFlags = CDX_TYPE_COMPACT | CDX_TYPE_COMPOUND;
  }
  else
  {
    pTag->TagBlock = TagHdr;
    hb_cdxTagLoad(pTag);
    if (pTag->RootBlock == 0)
    {
      // index file is corrupted
      hb_cdxTagFree(pTag);
      pTag = nullptr;
    }
  }
  return pTag;
}

// close Tag (free used pages into page pool)
static void hb_cdxTagClose(LPCDXTAG pTag)
{
  if (pTag->RootPage != nullptr)
  {
    hb_cdxPageFree(pTag->RootPage, false);
    pTag->RootPage = nullptr;
  }
  if (pTag->TagChanged)
  {
    hb_cdxTagHeaderStore(pTag);
  }

  pTag->fRePos = true;
}

// (re)open Tag
static void hb_cdxTagOpen(LPCDXTAG pTag)
{
  CDXTAGHEADER tagHeader;

  if (!pTag->RootPage)
  {
    hb_cdxIndexPageRead(pTag->pIndex, pTag->TagBlock, reinterpret_cast<HB_BYTE *>(&tagHeader), sizeof(tagHeader));
    pTag->RootBlock = HB_GET_LE_UINT32(tagHeader.rootPtr);
    if (pTag->RootBlock && pTag->RootBlock != CDX_DUMMYNODE)
    {
      pTag->RootPage = hb_cdxPageNew(pTag, nullptr, pTag->RootBlock);
    }
    if (!pTag->RootPage)
    {
      hb_cdxErrInternal("hb_cdxTagOpen: index corrupted");
    }
  }
}

// free Tag pages from cache
static void hb_cdxTagPoolFree(LPCDXTAG pTag, int nPagesLeft)
{
  LPCDXPAGE pPage, pPageNext;

#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxTagPoolCheck(pTag);
#endif
  pPage = pTag->pagePool;
  while (nPagesLeft && pPage)
  {
    pPage = pPage->pPoolNext;
    nPagesLeft--;
  }
  while (pPage)
  {
    pPageNext = pPage->pPoolNext;
    if (!pPage->bUsed)
    {
      hb_cdxPageFree(pPage, true);
    }
    pPage = pPageNext;
  }
#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxTagPoolCheck(pTag);
#endif
}

// write all changed pages in tag cache
static void hb_cdxTagPoolFlush(LPCDXTAG pTag)
{
  LPCDXPAGE pPage;

  pPage = pTag->pagePool;
  while (pPage)
  {
    if (pPage->fChanged)
    {
      hb_cdxPageStore(pPage);
    }
    pPage = pPage->pPoolNext;
  }
#ifdef HB_CDX_DBGCODE_EXT
  hb_cdxTagPoolCheck(pTag);
#endif
}

// retrieve CurKey from current Tag position
static void hb_cdxSetCurKey(LPCDXPAGE pPage)
{
  while (pPage->Child)
  {
    pPage = pPage->Child;
  }

  pPage->TagParent->CurKey = hb_cdxKeyPut(pPage->TagParent->CurKey, hb_cdxPageGetKeyVal(pPage, pPage->iCurKey),
                                          pPage->TagParent->uiLen, hb_cdxPageGetKeyRec(pPage, pPage->iCurKey));
}

// seek given Key in the Page or in its children
static int hb_cdxPageSeekKey(LPCDXPAGE pPage, LPCDXKEY pKey, HB_ULONG ulKeyRec)
{
  int l, r, n, k;
  bool fLeaf = (pPage->PageType & CDX_NODE_LEAF) != 0;

  if (fLeaf && !pPage->pKeyBuf && pPage->iKeys > 0)
  {
    int iLen = pPage->TagParent->uiLen + 8;
    auto pKeyBuf = static_cast<HB_BYTE *>(hb_xgrab(pPage->iKeys * iLen));
    hb_cdxPageLeafDecode(pPage, pKeyBuf);
    pPage->pKeyBuf = pKeyBuf;
  }

  k = (ulKeyRec == CDX_MAX_REC_NUM) ? -1 : 1;
  n = -1;
  l = 0;
  r = pPage->iKeys - 1;
  while (l < r)
  {
    n = (l + r) >> 1;
    k = hb_cdxValCompare(pPage->TagParent, pKey->val, pKey->len, hb_cdxPageGetKeyVal(pPage, n), pPage->TagParent->uiLen,
                         pKey->mode);
    if (k == 0)
    {
      if (ulKeyRec == CDX_MAX_REC_NUM)
      {
        k = 1;
      }
      else if (ulKeyRec != CDX_IGNORE_REC_NUM)
      {
        HB_ULONG ulRec = hb_cdxPageGetKeyRec(pPage, n);
        if (ulKeyRec > ulRec)
        {
          k = 1;
        }
        else if (ulKeyRec < ulRec)
        {
          k = -1;
        }
      }
    }
    if (k > 0)
    {
      l = n + 1;
    }
    else
    {
      r = n;
    }
  }
  pPage->iCurKey = l;
  if (r < 0)
  {
    return k;
  }

  if (!fLeaf)
  {
    hb_cdxPageGetChild(pPage);
#ifdef HB_CDX_DBGCODE
    if (memcmp(hb_cdxPageGetKeyVal(pPage, pPage->iCurKey), hb_cdxPageGetKeyVal(pPage->Child, pPage->Child->iKeys - 1),
               pPage->TagParent->uiLen) != 0 ||
        hb_cdxPageGetKeyRec(pPage, pPage->iCurKey) != hb_cdxPageGetKeyRec(pPage->Child, pPage->Child->iKeys - 1))
    {
      fprintf(stderr, "\r\nkeyLen=%u", pPage->TagParent->uiLen);
      fprintf(stderr, "\r\nparent=%lx, iKey=%d, rec=%lu", pPage->Page, pPage->iCurKey,
              hb_cdxPageGetKeyRec(pPage, pPage->iCurKey));
      fprintf(stderr, "\r\n child=%lx, iKey=%d, rec=%lu", pPage->Child->Page, pPage->Child->iKeys - 1,
              hb_cdxPageGetKeyRec(pPage->Child, pPage->Child->iKeys - 1));
      fprintf(stderr, "\r\nparent val=[%s]", hb_cdxPageGetKeyVal(pPage, pPage->iCurKey));
      fprintf(stderr, "\r\n child val=[%s]", hb_cdxPageGetKeyVal(pPage->Child, pPage->Child->iKeys - 1));
      fflush(stderr);
      hb_cdxErrInternal("hb_cdxPageSeekKey: wrong parent key.");
    }
#endif
    k = hb_cdxPageSeekKey(pPage->Child, pKey, ulKeyRec);
  }
  else if (l != n || ulKeyRec == CDX_MAX_REC_NUM)
  {
    k = hb_cdxValCompare(pPage->TagParent, pKey->val, pKey->len, hb_cdxPageGetKeyVal(pPage, pPage->iCurKey),
                         pPage->TagParent->uiLen, pKey->mode);
    if (k == 0 && ulKeyRec != CDX_MAX_REC_NUM && ulKeyRec != CDX_IGNORE_REC_NUM)
    {
      HB_ULONG ulRec = hb_cdxPageGetKeyRec(pPage, pPage->iCurKey);
      if (ulKeyRec > ulRec)
      {
        k = 1;
      }
      else if (ulKeyRec < ulRec)
      {
        k = -1;
      }
    }
  }
  if (ulKeyRec == CDX_MAX_REC_NUM)
  {
    if (pPage->iCurKey > 0 && k < 0)
    {
      pPage->iCurKey--;
      if (!fLeaf)
      {
        hb_cdxPageGetChild(pPage);
        k = hb_cdxPageSeekKey(pPage->Child, pKey, ulKeyRec);
      }
      else
      {
        k = hb_cdxValCompare(pPage->TagParent, pKey->val, pKey->len, hb_cdxPageGetKeyVal(pPage, pPage->iCurKey),
                             pPage->TagParent->uiLen, pKey->mode);
      }
    }
  }
  else if (k > 0 && fLeaf)
  {
    pPage->iCurKey++;
  }
  return k;
}

// an interface for fast check record number in record filter
static bool hb_cdxCheckRecordScope(CDXAREAP pArea, HB_ULONG ulRec)
{
  HB_LONG lRecNo = static_cast<HB_LONG>(ulRec);

  if (SELF_COUNTSCOPE(&pArea->dbfarea.area, nullptr, &lRecNo) == Harbour::SUCCESS && lRecNo == 0)
  {
    return false;
  }
  return true;
}

// check and evaluate record filter
static bool hb_cdxCheckRecordFilter(CDXAREAP pArea, HB_ULONG ulRecNo)
{
  HB_BOOL lResult = false;
  bool fDeleted = hb_setGetDeleted();

  if (pArea->dbfarea.area.dbfi.itmCobExpr || fDeleted)
  {
    if (pArea->dbfarea.ulRecNo != ulRecNo || pArea->dbfarea.lpdbPendingRel)
    {
      SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
    }

    if (fDeleted)
    {
      SELF_DELETED(&pArea->dbfarea.area, &lResult);
    }

    if (!lResult && pArea->dbfarea.area.dbfi.itmCobExpr)
    {
      auto pResult = hb_vmEvalBlock(pArea->dbfarea.area.dbfi.itmCobExpr);
      lResult = pResult->isLogical() && !pResult->getL();
    }
  }
  return !lResult;
}

// read Top Key from Page or its children
static bool hb_cdxPageReadTopKey(LPCDXPAGE pPage)
{
  while ((pPage->PageType & CDX_NODE_LEAF) == 0 && pPage->iKeys > 0)
  {
    pPage->iCurKey = 0;
    hb_cdxPageGetChild(pPage);
    pPage = pPage->Child;
  }
  if (pPage->iKeys == 0)
  {
    return false;
  }
  pPage->iCurKey = 0;

  hb_cdxSetCurKey(pPage);
  return true;
}

// read Bottom Key from Page or its children
static bool hb_cdxPageReadBottomKey(LPCDXPAGE pPage)
{
  while ((pPage->PageType & CDX_NODE_LEAF) == 0 && pPage->iKeys > 0)
  {
    pPage->iCurKey = pPage->iKeys - 1;
    hb_cdxPageGetChild(pPage);
    pPage = pPage->Child;
  }
  if (pPage->iKeys == 0)
  {
    return false;
  }
  pPage->iCurKey = pPage->iKeys - 1;

  hb_cdxSetCurKey(pPage);
  return true;
}

// read Previous Key from Page or its children
static bool hb_cdxPageReadPrevKey(LPCDXPAGE pPage)
{
  LPCDXPAGE pOwnerPage = nullptr;

  while (pPage->Child)
  {
    pOwnerPage = pPage;
    pPage = pPage->Child;
  }

  do
  {
    pPage->iCurKey--;
    while (pPage->iCurKey < 0)
    {
      if (pPage->Left == CDX_DUMMYNODE || !pOwnerPage)
      {
        pPage->iCurKey = 0;
        if (pPage->iKeys > 0)
        {
          hb_cdxSetCurKey(pPage);
        }
        return false;
      }
      pOwnerPage->Child = hb_cdxPageNew(pPage->TagParent, pPage->Owner, pPage->Left);
      hb_cdxPageFree(pPage, !pPage->fChanged);
      pPage = pOwnerPage->Child;
      pPage->iCurKey = pPage->iKeys - 1;
    }
    if (pPage->iCurKey == 0)
    {
      hb_cdxSetCurKey(pPage);
      if (!hb_cdxTopScope(pPage->TagParent) || !hb_cdxBottomScope(pPage->TagParent))
      {
        break;
      }
    }
  } while ((pPage->TagParent->OptFlags & CDX_TYPE_STRUCTURE) == 0 &&
           !hb_cdxCheckRecordScope(pPage->TagParent->pIndex->pArea, hb_cdxPageGetKeyRec(pPage, pPage->iCurKey)));
  if (pPage->iCurKey != 0)
  {
    hb_cdxSetCurKey(pPage);
  }
  return true;
}

// read Next Key from Page or its children
static bool hb_cdxPageReadNextKey(LPCDXPAGE pPage)
{
  LPCDXPAGE pOwnerPage = nullptr;

  while (pPage->Child)
  {
    pOwnerPage = pPage;
    pPage = pPage->Child;
  }

  do
  {
    pPage->iCurKey++;
    while (pPage->iCurKey >= pPage->iKeys)
    {
      if (pPage->Right == CDX_DUMMYNODE || !pOwnerPage)
      {
        pPage->iCurKey = pPage->iKeys;
        return false;
      }
      pOwnerPage->Child = hb_cdxPageNew(pPage->TagParent, pPage->Owner, pPage->Right);
      hb_cdxPageFree(pPage, !pPage->fChanged);
      pPage = pOwnerPage->Child;
      pPage->iCurKey = 0;
    }
    if (pPage->iCurKey == 0)
    {
      hb_cdxSetCurKey(pPage);
      if (!hb_cdxTopScope(pPage->TagParent) || !hb_cdxBottomScope(pPage->TagParent))
      {
        break;
      }
    }
  } while ((pPage->TagParent->OptFlags & CDX_TYPE_STRUCTURE) == 0 &&
           !hb_cdxCheckRecordScope(pPage->TagParent->pIndex->pArea, hb_cdxPageGetKeyRec(pPage, pPage->iCurKey)));
  if (pPage->iCurKey != 0)
  {
    hb_cdxSetCurKey(pPage);
  }
  return true;
}

// read Previous Unique Key from Page or its children
static bool hb_cdxPageReadPrevUniqKey(LPCDXPAGE pPage)
{
  LPCDXPAGE pOwnerPage = nullptr;

  while (pPage->Child)
  {
    pOwnerPage = pPage;
    pPage = pPage->Child;
  }
  while (pPage->iCurKey < 0 || memcmp(pPage->TagParent->CurKey->val, hb_cdxPageGetKeyVal(pPage, pPage->iCurKey),
                                      pPage->TagParent->uiLen) == 0)
  {
    if (pPage->iCurKey > 0)
    {
      pPage->iCurKey--;
    }
    else
    {
      if (pPage->Left == CDX_DUMMYNODE || !pOwnerPage)
      {
        pPage->iCurKey = 0;
        if (pPage->iKeys > 0)
        {
          hb_cdxSetCurKey(pPage);
        }
        return false;
      }
      pOwnerPage->Child = hb_cdxPageNew(pPage->TagParent, pPage->Owner, pPage->Left);
      hb_cdxPageFree(pPage, !pPage->fChanged);
      pPage = pOwnerPage->Child;
      pPage->iCurKey = pPage->iKeys - 1;
    }
  }

  hb_cdxSetCurKey(pPage);
  return true;
}

// read Next Unique Key from Page or its children
static bool hb_cdxPageReadNextUniqKey(LPCDXPAGE pPage)
{
  LPCDXPAGE pOwnerPage = nullptr;

  while (pPage->Child)
  {
    pOwnerPage = pPage;
    pPage = pPage->Child;
  }

  while (pPage->iCurKey >= pPage->iKeys ||
         memcmp(pPage->TagParent->CurKey->val, hb_cdxPageGetKeyVal(pPage, pPage->iCurKey), pPage->TagParent->uiLen) ==
             0)
  {
    if (pPage->iCurKey < pPage->iKeys - 1)
    {
      pPage->iCurKey++;
    }
    else
    {
      if (pPage->Right == CDX_DUMMYNODE || !pOwnerPage)
      {
        pPage->iCurKey = pPage->iKeys - 1;
        if (pPage->iKeys > 0)
        {
          hb_cdxSetCurKey(pPage);
        }
        return false;
      }
      pOwnerPage->Child = hb_cdxPageNew(pPage->TagParent, pPage->Owner, pPage->Right);
      hb_cdxPageFree(pPage, !pPage->fChanged);
      pPage = pOwnerPage->Child;
      pPage->iCurKey = 0;
    }
  }
  hb_cdxSetCurKey(pPage);
  return true;
}

// read the TOP/BOTTOM/NEXT/PREVIOUS Key from Tag
static void hb_cdxTagKeyRead(LPCDXTAG pTag, HB_BYTE bTypRead)
{
  auto fAfter = false;
  auto fBof = false;
  auto fEof = false;

  pTag->CurKey->rec = 0;
  pTag->fRePos = false;
  hb_cdxTagOpen(pTag);

  if (pTag->UsrUnique)
  {
    switch (bTypRead)
    {
    case NEXT_RECORD:
      bTypRead = NXTU_RECORD;
      break;

    case PREV_RECORD:
      bTypRead = PRVU_RECORD;
      // fallthrough
    case BTTM_RECORD:
      fAfter = true;
      break;
    }
  }
  if (pTag->UsrAscend)
  {
    fBof = pTag->TagBOF;
    fEof = pTag->TagEOF;
  }
  else
  {
    fBof = pTag->TagEOF;
    fEof = pTag->TagBOF;
    switch (bTypRead)
    {
    case TOP_RECORD:
      bTypRead = BTTM_RECORD;
      break;

    case BTTM_RECORD:
      bTypRead = TOP_RECORD;
      break;

    case PREV_RECORD:
      bTypRead = NEXT_RECORD;
      break;

    case NEXT_RECORD:
      bTypRead = PREV_RECORD;
      break;

    case PRVU_RECORD:
      bTypRead = NXTU_RECORD;
      break;

    case NXTU_RECORD:
      bTypRead = PRVU_RECORD;
      break;
    }
  }
  switch (bTypRead)
  {
  case TOP_RECORD:
    fBof = fEof = !hb_cdxPageReadTopKey(pTag->RootPage);
    break;

  case BTTM_RECORD:
    fBof = fEof = !hb_cdxPageReadBottomKey(pTag->RootPage);
    break;

  case PREV_RECORD:
    if (!fBof)
    {
      fBof = !hb_cdxPageReadPrevKey(pTag->RootPage);
    }
    break;

  case NEXT_RECORD:
    if (!fEof)
    {
      fEof = !hb_cdxPageReadNextKey(pTag->RootPage);
    }
    break;

  case PRVU_RECORD:
    if (!fBof)
    {
      fBof = !hb_cdxPageReadPrevUniqKey(pTag->RootPage);
    }
    break;

  case NXTU_RECORD:
    if (!fEof)
    {
      fEof = !hb_cdxPageReadNextUniqKey(pTag->RootPage);
    }
    break;
  }

  if (fEof)
  {
    pTag->CurKey->rec = 0;
  }
  else if (fAfter && !fBof)
  {
    if (pTag->UsrAscend)
    {
      if (hb_cdxPageReadPrevUniqKey(pTag->RootPage))
      {
        hb_cdxPageReadNextKey(pTag->RootPage);
      }
    }
    else
    {
      if (hb_cdxPageReadNextUniqKey(pTag->RootPage))
      {
        hb_cdxPageReadPrevKey(pTag->RootPage);
      }
    }
  }

  if (pTag->UsrAscend)
  {
    pTag->TagBOF = fBof;
    pTag->TagEOF = fEof;
  }
  else
  {
    pTag->TagBOF = fEof;
    pTag->TagEOF = fBof;
  }
}

// find pKey in pTag return 0 or TagNO
static HB_ULONG hb_cdxTagKeyFind(LPCDXTAG pTag, LPCDXKEY pKey)
{
  int K;
  HB_ULONG ulKeyRec = pKey->rec;

  pTag->fRePos = false;
  hb_cdxTagOpen(pTag);

  pTag->TagBOF = pTag->TagEOF = false;
  K = hb_cdxPageSeekKey(pTag->RootPage, pKey, ulKeyRec);
  if (ulKeyRec == CDX_MAX_REC_NUM)
  {
    K = -K;
  }

  if (K > 0)
  {
    pTag->CurKey->rec = 0;
    pTag->TagEOF = true;
  }
  else
  {
    hb_cdxSetCurKey(pTag->RootPage);
    if (K == 0)
    {
      return pTag->CurKey->rec;
    }
  }
  return 0;
}

#if 0
// find pKey in pTag return 0 or record number, respect descend/unique flags
static HB_ULONG hb_cdxTagKeySeek(LPCDXTAG pTag, LPCDXKEY pKey)
{
   int K;
   HB_ULONG ulKeyRec = pKey->rec;

   if( pTag->UsrUnique ) {
      if( pTag->UsrAscend ) {
         if( ulKeyRec == CDX_MAX_REC_NUM ) {
            ulKeyRec = CDX_IGNORE_REC_NUM;
         }
      } else if( ulKeyRec == CDX_IGNORE_REC_NUM ) {
         ulKeyRec = CDX_MAX_REC_NUM;
      }
   } else if( !pTag->UsrAscend ) {
      if( ulKeyRec == CDX_MAX_REC_NUM ) {
         ulKeyRec = CDX_IGNORE_REC_NUM;
      } else if( ulKeyRec == CDX_IGNORE_REC_NUM ) {
         ulKeyRec = CDX_MAX_REC_NUM;
      }
   }

   pTag->CurKey->rec = 0;
   pTag->fRePos = false;
   hb_cdxTagOpen(pTag);

   pTag->TagBOF = pTag->TagEOF = false;
   K = hb_cdxPageSeekKey(pTag->RootPage, pKey, ulKeyRec);
   if( ulKeyRec == CDX_MAX_REC_NUM ) {
      K = -K;
   }

   if( K > 0 ) {
      pTag->TagEOF = true;
   } else {
      hb_cdxSetCurKey(pTag->RootPage);
      if( K == 0 ) {
         return pTag->CurKey->rec;
      }
   }
   return 0;
}
#endif

// add the Key into the Tag
static bool hb_cdxTagKeyAdd(LPCDXTAG pTag, LPCDXKEY pKey)
{
  hb_cdxTagOpen(pTag);
  if (hb_cdxPageSeekKey(pTag->RootPage, pKey, pTag->UniqueKey ? CDX_IGNORE_REC_NUM : pKey->rec) != 0 ||
      (pTag->Custom && pTag->MultiKey && !pTag->UniqueKey))
  {
    hb_cdxPageKeyInsert(pTag->RootPage, pKey);
    pTag->curKeyState &= ~(CDX_CURKEY_RAWPOS | CDX_CURKEY_LOGPOS | CDX_CURKEY_RAWCNT | CDX_CURKEY_LOGCNT);
    pTag->fRePos = true;
    // TODO: !!! remove when page leaf balance can save CurKey
    hb_cdxTagKeyFind(pTag, pKey);
    return true;
  }
  return false;
}

// delete the Key from the Tag
static bool hb_cdxTagKeyDel(LPCDXTAG pTag, LPCDXKEY pKey)
{
  if (hb_cdxTagKeyFind(pTag, pKey) != 0)
  {
    hb_cdxPageKeyRemove(pTag->RootPage);
    pTag->curKeyState &= ~(CDX_CURKEY_RAWPOS | CDX_CURKEY_LOGPOS | CDX_CURKEY_RAWCNT | CDX_CURKEY_LOGCNT);
    pTag->CurKey->rec = 0;
    return true;
  }
  return false;
}

// Go to the first visible record in Tag
static void hb_cdxTagGoTop(LPCDXTAG pTag)
{
  LPCDXKEY pKey = pTag->UsrAscend ? pTag->topScopeKey : pTag->bottomScopeKey;
  HB_ULONG ulPos = 1;

  if (pKey)
  {
    hb_cdxTagKeyFind(pTag, pKey);
  }
  else
  {
    hb_cdxTagKeyRead(pTag, TOP_RECORD);
  }

  for (;;)
  {
    if (pTag->CurKey->rec == 0 || pTag->TagEOF || !hb_cdxBottomScope(pTag))
    {
      pTag->TagBOF = pTag->TagEOF = true;
      pTag->CurKey->rec = 0;
      break;
    }
    else if ((pTag->OptFlags & CDX_TYPE_STRUCTURE) != 0 ||
             hb_cdxCheckRecordScope(pTag->pIndex->pArea, pTag->CurKey->rec))
    {
      pTag->rawKeyPos = ulPos;
      CURKEY_SETRAWPOS(pTag);
      break;
    }
    hb_cdxTagKeyRead(pTag, NEXT_RECORD);
    ulPos++;
  }
}

// Go to the last visible record in Tag
static void hb_cdxTagGoBottom(LPCDXTAG pTag)
{
  LPCDXKEY pKey = pTag->UsrAscend ? pTag->bottomScopeKey : pTag->topScopeKey;
  HB_ULONG ulPos = 0;

  if (pKey)
  {
    hb_cdxTagKeyFind(pTag, pKey);
  }
  else
  {
    hb_cdxTagKeyRead(pTag, BTTM_RECORD);
  }

  for (;;)
  {
    if (pTag->CurKey->rec == 0 || pTag->TagBOF || !hb_cdxTopScope(pTag))
    {
      pTag->TagBOF = pTag->TagEOF = true;
      pTag->CurKey->rec = 0;
      break;
    }
    else if ((pTag->OptFlags & CDX_TYPE_STRUCTURE) != 0 ||
             hb_cdxCheckRecordScope(pTag->pIndex->pArea, pTag->CurKey->rec))
    {
      if (CURKEY_RAWCNT(pTag))
      {
        pTag->rawKeyPos = pTag->rawKeyCount - ulPos;
        CURKEY_SETRAWPOS(pTag);
      }
      break;
    }
    hb_cdxTagKeyRead(pTag, PREV_RECORD);
    ulPos++;
  }
}

// skip to Next Key in the Tag
static void hb_cdxTagSkipNext(LPCDXTAG pTag)
{
  bool fPos = CURKEY_RAWPOS(pTag);
  auto fEof = false;
  HB_ULONG ulSkip = 1;

  if (pTag->CurKey->rec != 0)
  {
    if (!hb_cdxTopScope(pTag))
    {
      ulSkip = 0;
      hb_cdxTagGoTop(pTag);
    }
    else
    {
      hb_cdxTagKeyRead(pTag, NEXT_RECORD);
    }
  }

  while (!fEof)
  {
    if (pTag->TagEOF || pTag->CurKey->rec == 0 || !hb_cdxBottomScope(pTag) || !hb_cdxTopScope(pTag))
    {
      fEof = true;
    }
    else if ((pTag->OptFlags & CDX_TYPE_STRUCTURE) != 0 ||
             hb_cdxCheckRecordScope(pTag->pIndex->pArea, pTag->CurKey->rec))
    {
      break;
    }
    hb_cdxTagKeyRead(pTag, NEXT_RECORD);
    ulSkip++;
  }

  if (fEof)
  {
    pTag->CurKey->rec = 0;
    pTag->TagEOF = true;
  }
  else if (fPos)
  {
    pTag->rawKeyPos += ulSkip;
    CURKEY_SETRAWPOS(pTag);
  }
}

// skip to Previous Key in the Tag
static void hb_cdxTagSkipPrev(LPCDXTAG pTag)
{
  bool fPos = CURKEY_RAWPOS(pTag);
  auto fBof = false;
  HB_ULONG ulSkip = 1;

  if (pTag->CurKey->rec == 0)
  {
    ulSkip = 0;
    hb_cdxTagGoBottom(pTag);
  }
  else
  {
    hb_cdxTagKeyRead(pTag, PREV_RECORD);
  }

  while (!fBof)
  {
    if (pTag->TagBOF || pTag->CurKey->rec == 0 || !hb_cdxBottomScope(pTag) || !hb_cdxTopScope(pTag))
    {
      fBof = true;
    }
    else if ((pTag->OptFlags & CDX_TYPE_STRUCTURE) != 0 ||
             hb_cdxCheckRecordScope(pTag->pIndex->pArea, pTag->CurKey->rec))
    {
      break;
    }
    hb_cdxTagKeyRead(pTag, PREV_RECORD);
    ulSkip++;
  }

  if (fBof)
  {
    hb_cdxTagGoTop(pTag);
    pTag->TagBOF = true;
  }
  else if (fPos)
  {
    pTag->rawKeyPos -= ulSkip;
    CURKEY_SETRAWPOS(pTag);
  }
}

// Reorder the Tag list by their position in index file (not names)
// to be Clipper compatible
static void hb_cdxReorderTagList(LPCDXTAG *TagListPtr)
{
  auto fRepeat = true;

  while (fRepeat)
  {
    LPCDXTAG *pTagPtr, pTagTmp;

    fRepeat = false;
    pTagPtr = TagListPtr;
    while (*pTagPtr && (*pTagPtr)->pNext)
    {
      if ((*pTagPtr)->TagBlock > (*pTagPtr)->pNext->TagBlock)
      {
        pTagTmp = (*pTagPtr);
        (*pTagPtr) = (*pTagPtr)->pNext;
        pTagTmp->pNext = (*pTagPtr)->pNext;
        (*pTagPtr)->pNext = pTagTmp;
        fRepeat = true;
      }
      pTagPtr = &(*pTagPtr)->pNext;
    }
  }
}

// create new order header, store it and then make an order
static LPCDXTAG hb_cdxIndexCreateTag(bool fStruct, LPCDXINDEX pIndex, const char *szTagName, const char *szKeyExp,
                                     PHB_ITEM pKeyItem, HB_BYTE bType, HB_USHORT uiLen, const char *szForExp,
                                     PHB_ITEM pForItem, bool fAscnd, bool fUniq, bool fNoCase, bool fCustom,
                                     bool fReindex)
{
  LPCDXTAG pTag;

  pTag = hb_cdxTagNew(pIndex, szTagName, CDX_DUMMYNODE);

  if (fStruct)
  {
    pTag->OptFlags |= CDX_TYPE_STRUCTURE;
  }

  if (bType == 'C')
  {
    hb_cdxMakeSortTab(pTag->pIndex->pArea);
  }

  if (szKeyExp != nullptr)
  {
    pTag->KeyExpr = hb_strduptrim(szKeyExp);
    pTag->nField = hb_rddFieldExpIndex(&pTag->pIndex->pArea->dbfarea.area, pTag->KeyExpr);
  }
  pTag->pKeyItem = pKeyItem;
  if (szForExp != nullptr)
  {
    pTag->ForExpr = hb_strduptrim(szForExp);
  }

  pTag->pForItem = pForItem;
  pTag->AscendKey = pTag->UsrAscend = fAscnd;
  pTag->UniqueKey = fUniq;
  pTag->UsrUnique = false;
  pTag->IgnoreCase = fNoCase && bType == 'C';
  pTag->Custom = fCustom;

#if defined(HB_SIXCDX)
  pTag->Template = pTag->KeyExpr && hb_cdxIsTemplateFunc(pTag->KeyExpr);
  if (pTag->Template)
  {
    pTag->Custom = true;
  }
  // SIx3 does not support repeated key value for the same record
  pTag->MultiKey = false;
#else
  pTag->Template = pTag->MultiKey = pTag->Custom;
#endif
  pTag->Partial = pTag->ChgOnly = false;
  pTag->uiType = bType;
  pTag->bTrail = (bType == 'C') ? ' ' : '\0';
  pTag->uiLen = uiLen;
  pTag->MaxKeys = (pIndex->uiPageLen - CDX_INT_HEADSIZE) / (uiLen + 8);
  pTag->TagChanged = true;
  hb_cdxTagDoIndex(pTag, fReindex);

  return pTag;
}

// create structural (compound) tag
static void hb_cdxIndexCreateStruct(LPCDXINDEX pIndex, char *szTagName)
{
  // here we can change default tag name
  pIndex->pCompound = hb_cdxIndexCreateTag(true, pIndex, szTagName, nullptr, nullptr, 'C', CDX_MAXTAGNAMELEN, nullptr,
                                           nullptr, true, false, false, false, false);
}

// free page and all child pages
static void hb_cdxIndexFreePages(LPCDXPAGE pPage)
{
  if ((pPage->PageType & CDX_NODE_LEAF) == 0)
  {
    for (auto iKey = 0; iKey < pPage->iKeys; iKey++)
    {
      LPCDXPAGE pChildPage = hb_cdxPageNew(pPage->TagParent, nullptr, hb_cdxPageGetKeyPage(pPage, iKey));
      if (pChildPage)
      {
        hb_cdxIndexFreePages(pChildPage);
      }
    }
  }
  pPage->PageType = CDX_NODE_UNUSED;
  hb_cdxPageFree(pPage, false);
}

// remove Tag from Bag
static void hb_cdxIndexDelTag(LPCDXINDEX pIndex, const char *szTagName)
{
  LPCDXTAG *pTagPtr = &pIndex->TagList;

  while (*pTagPtr && hb_stricmp((*pTagPtr)->szName, szTagName) != 0)
  {
    pTagPtr = &(*pTagPtr)->pNext;
  }

  if (*pTagPtr)
  {
    LPCDXTAG pTag = *pTagPtr;
    LPCDXKEY pKey = hb_cdxKeyPutCL(nullptr, pTag->szName, strlen(pTag->szName), pTag->TagBlock,
                                   pIndex->pCompound->uiLen, CDX_CMP_EXACT);
    if (hb_cdxTagKeyDel(pIndex->pCompound, pKey))
    {
      if (pTag != pIndex->TagList || pTag->pNext != nullptr)
      {
        LPCDXPAGE pPage;

        hb_cdxTagOpen(pTag);
        pPage = pTag->RootPage;
        hb_cdxTagClose(pTag);
        if (!pIndex->fShared)
        {
          if (pPage)
          {
            hb_cdxIndexFreePages(pPage);
          }
          hb_cdxIndexPutAvailPage(pIndex, pTag->TagBlock, true);
        }
        pTag->TagChanged = false;
      }
    }
    *pTagPtr = pTag->pNext;
    hb_cdxTagFree(pTag);
    hb_cdxKeyFree(pKey);
  }
}

// add tag to order bag
static LPCDXTAG hb_cdxIndexAddTag(LPCDXINDEX pIndex, const char *szTagName, const char *szKeyExp, PHB_ITEM pKeyItem,
                                  HB_BYTE bType, HB_USHORT uiLen, const char *szForExp, PHB_ITEM pForItem, bool fAscend,
                                  bool fUnique, bool fNoCase, bool fCustom, bool fReindex)
{
  LPCDXTAG pTag, *pTagPtr;
  LPCDXKEY pKey;

  // Delete previous tag first to free the place for new one
  // its redundant Tag should be already deleted
  hb_cdxIndexDelTag(pIndex, szTagName);

  // Create new tag an add to tag list
  pTag = hb_cdxIndexCreateTag(false, pIndex, szTagName, szKeyExp, pKeyItem, bType, uiLen, szForExp, pForItem, fAscend,
                              fUnique, fNoCase, fCustom, fReindex);
  pTagPtr = &pIndex->TagList;
  while (*pTagPtr)
  {
    pTagPtr = &(*pTagPtr)->pNext;
  }
  *pTagPtr = pTag;
  pKey = hb_cdxKeyPutCL(nullptr, szTagName, strlen(szTagName), pTag->TagBlock, pIndex->pCompound->uiLen, CDX_CMP_EXACT);
  hb_cdxTagKeyAdd(pIndex->pCompound, pKey);
  hb_cdxKeyFree(pKey);
  return pTag;
}

// rebuild from scratch all orders in index file
static void hb_cdxIndexReindex(LPCDXINDEX pIndex)
{
  LPCDXTAG pCompound, pTagList, pTag;

  hb_cdxIndexLockWrite(pIndex);
  hb_cdxIndexLockFlush(pIndex);
  hb_cdxIndexDiscardBuffers(pIndex);

  pCompound = pIndex->pCompound;
  pTagList = pIndex->TagList;
  pIndex->pCompound = nullptr;
  pIndex->TagList = nullptr;

  pIndex->ulVersion = 0;
  pIndex->nextAvail = 0;
  pIndex->freePage = 0;
  hb_fileTruncAt(pIndex->pFile, 0);
  pIndex->fChanged = true;

  // Rebuild the compound (master) tag
  if (pCompound)
  {
    hb_cdxIndexCreateStruct(pIndex, pCompound->szName);
    hb_cdxTagFree(pCompound);
  }

  // Rebuild each tag
  while (pTagList)
  {
    pTag = pTagList;
    hb_cdxIndexAddTag(pIndex, pTag->szName, pTag->KeyExpr, pTag->pKeyItem, static_cast<HB_BYTE>(pTag->uiType),
                      pTag->uiLen, pTag->ForExpr, pTag->pForItem, pTag->AscendKey, pTag->UniqueKey, pTag->IgnoreCase,
                      pTag->Custom, true);
    pTagList = pTag->pNext;
    pTag->pKeyItem = pTag->pForItem = nullptr;
    hb_cdxTagFree(pTag);
  }
  hb_cdxIndexUnLockWrite(pIndex);
}

static void hb_cdxIndexInit(LPCDXINDEX pIndex)
{
  HB_USHORT uiPageSize = DBFAREA_DATA(&pIndex->pArea->dbfarea)->uiIndexPageSize;
  auto fLargeFile = false;

  if (uiPageSize < CDX_PAGELEN)
  {
    uiPageSize = CDX_PAGELEN;
  }
  else if (uiPageSize > CDX_PAGELEN_MAX)
  {
    uiPageSize = CDX_PAGELEN_MAX;
  }

  fLargeFile = uiPageSize > CDX_PAGELEN || pIndex->pArea->dbfarea.bLockType == DB_DBFLOCK_HB64;
  hb_cdxSetPageSize(pIndex, fLargeFile, uiPageSize, CDX_HEADERLEN);
}

// create new index structure
static LPCDXINDEX hb_cdxIndexNew(CDXAREAP pArea)
{
  auto pIndex = static_cast<LPCDXINDEX>(hb_xgrab(sizeof(CDXINDEX)));
  memset(pIndex, 0, sizeof(CDXINDEX));
  pIndex->pArea = pArea;
  hb_cdxIndexInit(pIndex);

  return pIndex;
}

// free (close) all tag in index file
static void hb_cdxIndexFreeTags(LPCDXINDEX pIndex)
{
  LPCDXTAG pTag;

  // Free Compound tag
  if (pIndex->pCompound != nullptr)
  {
    hb_cdxTagFree(pIndex->pCompound);
    pIndex->pCompound = nullptr;
  }

  while (pIndex->TagList)
  {
    pTag = pIndex->TagList;
    pIndex->TagList = pTag->pNext;
    hb_cdxTagFree(pTag);
  }
}

// free (close) index and all tags in it
static void hb_cdxIndexFree(LPCDXINDEX pIndex)
{
  // Free List of Free Pages
  hb_cdxIndexDropAvailPage(pIndex);

  // free all tags
  hb_cdxIndexFreeTags(pIndex);

  // Close file
  if (pIndex->pFile)
  {
    hb_fileClose(pIndex->pFile);
    if (pIndex->fDelete)
    {
      hb_fileDelete(pIndex->szRealName ? pIndex->szRealName : pIndex->szFileName);
    }
  }

#ifdef HB_CDX_DBGCODE
  if (pIndex->fShared && (pIndex->lockWrite || pIndex->lockRead) && hb_vmRequestQuery() == 0)
  {
    hb_errInternal(9104, "hb_cdxIndexFree: index file still locked.", nullptr, nullptr);
  }

  if ((pIndex->WrLck || pIndex->RdLck) && hb_vmRequestQuery() == 0)
  {
    hb_errInternal(9104, "hb_cdxIndexFree: index file still locked (*)", nullptr, nullptr);
  }
#endif

  if (pIndex->szFileName != nullptr)
  {
    hb_xfree(pIndex->szFileName);
  }
  if (pIndex->szRealName)
  {
    hb_xfree(pIndex->szRealName);
  }

  hb_xfree(pIndex);
}

// load orders from index file
static bool hb_cdxIndexLoad(LPCDXINDEX pIndex, char *szBaseName)
{
  LPCDXTAG TagList, *pTagPtr;
  auto fResult = false;

  TagList = nullptr;
  pTagPtr = &TagList;

  hb_cdxIndexLockRead(pIndex);
  // load the tags
  pIndex->pCompound = hb_cdxTagNew(pIndex, szBaseName, 0L);

  // check if index is not corrupted
  if (pIndex->pCompound)
  {
    pIndex->pCompound->OptFlags = CDX_TYPE_COMPACT | CDX_TYPE_COMPOUND | CDX_TYPE_STRUCTURE;
    hb_cdxTagGoTop(pIndex->pCompound);
    while (!pIndex->pCompound->TagEOF)
    {
      *pTagPtr = hb_cdxTagNew(pIndex, reinterpret_cast<char *>(pIndex->pCompound->CurKey->val),
                              pIndex->pCompound->CurKey->rec);
      // tag is corrupted - break tags loading
      if (*pTagPtr == nullptr)
      {
        fResult = false;
        break;
      }
      fResult = true;
      pTagPtr = &(*pTagPtr)->pNext;
      hb_cdxTagSkipNext(pIndex->pCompound);
    }
  }

  hb_cdxIndexUnLockRead(pIndex);
  hb_cdxReorderTagList(&TagList);
  pTagPtr = &pIndex->TagList;
  while (*pTagPtr != nullptr)
  {
    pTagPtr = &(*pTagPtr)->pNext;
  }
  (*pTagPtr) = TagList;

#ifdef HB_CDX_DSPDBG_INFO
  hb_cdxDspTags(pIndex);
#endif

  return fResult;
}

// create index file name
static void hb_cdxCreateFName(CDXAREAP pArea, const char *szBagName, bool *fProd, char *szFileName, char *szBaseName)
{
  PHB_FNAME pFileName;
  PHB_ITEM pExt = nullptr;
  bool fName = szBagName && *szBagName;

  pFileName = hb_fsFNameSplit(fName ? szBagName : pArea->dbfarea.szDataFileName);

  if (szBaseName != nullptr)
  {
    if (pFileName->szName)
    {
      hb_strncpyUpperTrim(szBaseName, pFileName->szName, CDX_MAXTAGNAMELEN);
    }
    else
    {
      szBaseName[0] = '\0';
    }
  }

  if (!fName || (!pFileName->szExtension && hb_setGetDefExtension()))
  {
    DBORDERINFO pExtInfo{};
    pExt = pExtInfo.itmResult = hb_itemPutC(nullptr, nullptr);
    if (SELF_ORDINFO(&pArea->dbfarea.area, DBOI_BAGEXT, &pExtInfo) == Harbour::SUCCESS && hb_itemGetCLen(pExt) > 0)
    {
      pFileName->szExtension = hb_itemGetCPtr(pExt);
    }
  }
  hb_fsFNameMerge(szFileName, pFileName);

  if (fProd)
  {
    if (!pFileName->szName)
    {
      *fProd = false;
    }
    else if (!fName)
    {
      *fProd = true;
    }
    else
    {
      PHB_FNAME pTableFileName = hb_fsFNameSplit(pArea->dbfarea.szDataFileName);

      *fProd = pTableFileName->szName && hb_stricmp(pTableFileName->szName, pFileName->szName) == 0;
      if (*fProd && pFileName->szExtension && !pExt)
      {
        DBORDERINFO pExtInfo{};
        pExt = pExtInfo.itmResult = hb_itemPutC(nullptr, nullptr);
        if (SELF_ORDINFO(&pArea->dbfarea.area, DBOI_BAGEXT, &pExtInfo) == Harbour::SUCCESS)
        {
          *fProd = hb_stricmp(pFileName->szExtension, hb_itemGetCPtr(pExt)) == 0;
        }
      }
      hb_xfree(pTableFileName);
    }
  }
  hb_xfree(pFileName);
  if (pExt)
  {
    hb_itemRelease(pExt);
  }
}

// free (close) used indexes, if not fAll then keep structure index
static void hb_cdxOrdListClear(CDXAREAP pArea, bool fAll, LPCDXINDEX pKeepInd)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOrdListClear(%p, %d)", static_cast<void*>(pArea), static_cast<int>(fAll)));
#endif

  if (pArea->lpIndexes)
  {
    LPCDXINDEX pIndex, *pIndexPtr;

    if (!fAll)
    {
      // TODO: we have to control this on open
      PHB_FNAME pFileNameDbf, pFileNameCdx;
      pFileNameDbf = hb_fsFNameSplit(pArea->dbfarea.szDataFileName);
      pFileNameCdx = hb_fsFNameSplit(pArea->lpIndexes->szFileName);
      fAll = hb_stricmp(pFileNameDbf->szName ? pFileNameDbf->szName : "",
                        pFileNameCdx->szName ? pFileNameCdx->szName : "") != 0;
      if (!fAll)
      {
        DBORDERINFO pExtInfo{};
        PHB_ITEM pExt;

        pExt = pExtInfo.itmResult = hb_itemPutC(nullptr, nullptr);
        if (SELF_ORDINFO(&pArea->dbfarea.area, DBOI_BAGEXT, &pExtInfo) == Harbour::SUCCESS)
        {
          fAll = hb_stricmp(pFileNameCdx->szExtension, hb_itemGetCPtr(pExt)) != 0;
        }
        hb_itemRelease(pExt);
      }
      hb_xfree(pFileNameDbf);
      hb_xfree(pFileNameCdx);
    }
    pIndexPtr = fAll ? &pArea->lpIndexes : &pArea->lpIndexes->pNext;
    while (*pIndexPtr)
    {
      pIndex = *pIndexPtr;
      if (pKeepInd == pIndex)
      {
        pIndexPtr = &pIndex->pNext;
      }
      else
      {
        *pIndexPtr = pIndex->pNext;
        hb_cdxIndexFree(pIndex);
      }
    }
  }
}

// find order bag by its name
static LPCDXINDEX hb_cdxFindBag(CDXAREAP pArea, const char *szBagName)
{
  LPCDXINDEX pIndex;
  PHB_FNAME pFileName;

  pFileName = hb_fsFNameSplit(szBagName);
  pIndex = pArea->lpIndexes;
  while (pIndex)
  {
    PHB_FNAME pIndexName = hb_fsFNameSplit(pIndex->szFileName);
    bool fFound = (pFileName->szName ? pIndexName->szName && !hb_stricmp(pIndexName->szName, pFileName->szName)
                                     : !pIndexName->szName) &&
                  (!pFileName->szPath || (pIndexName->szPath && !hb_stricmp(pIndexName->szPath, pFileName->szPath))) &&
                  (!pFileName->szExtension ||
                   (pIndexName->szExtension && !hb_stricmp(pIndexName->szExtension, pFileName->szExtension)));
    hb_xfree(pIndexName);
    if (fFound)
    {
      break;
    }
    pIndex = pIndex->pNext;
  }
  hb_xfree(pFileName);
  return pIndex;
}

// get Tag by number
static LPCDXTAG hb_cdxGetTagByNumber(CDXAREAP pArea, HB_USHORT uiTag)
{
  LPCDXTAG pTag = nullptr;
  LPCDXINDEX pIndex = pArea->lpIndexes;

  while (uiTag && pIndex)
  {
    pTag = pIndex->TagList;
    while (uiTag && pTag)
    {
      if (--uiTag)
      {
        pTag = pTag->pNext;
      }
    }
    pIndex = pIndex->pNext;
  }
  return pTag;
}

// get Tag number
static HB_USHORT hb_cdxGetTagNumber(CDXAREAP pArea, LPCDXTAG pFindTag)
{
  HB_USHORT uiTag = 0;
  LPCDXTAG pTag = nullptr;
  LPCDXINDEX pIndex = pArea->lpIndexes;

  if (pFindTag)
  {
    while (pIndex && (pTag != pFindTag))
    {
      pTag = pIndex->TagList;
      while (pTag != nullptr)
      {
        uiTag++;
        if (pTag == pFindTag)
        {
          break;
        }
        pTag = pTag->pNext;
      }
      pIndex = pIndex->pNext;
    }
    if (!pTag)
    {
      uiTag = 0;
    }
  }
  return uiTag;
}

// find Tag in tag list
static LPCDXTAG hb_cdxFindTag(CDXAREAP pArea, PHB_ITEM pTagItem, PHB_ITEM pBagItem, HB_USHORT *puiTag)
{
  LPCDXTAG pTag = nullptr;
  int iTag = 0, iFind = 0;
  char szTag[CDX_MAXTAGNAMELEN + 1];
  LPCDXINDEX pIndex = pArea->lpIndexes;
  auto fBag = false;

  hb_strncpyUpperTrim(szTag, hb_itemGetCPtr(pTagItem), sizeof(szTag) - 1);
  if (!szTag[0])
  {
    iFind = hb_itemGetNI(pTagItem);
  }

  fBag = szTag[0] && hb_itemGetCLen(pBagItem) > 0;
  if (fBag)
  {
    pIndex = hb_cdxFindBag(pArea, hb_itemGetCPtr(pBagItem));
  }
  else
  {
    int iBag = hb_itemGetNI(pBagItem);

    if (iBag > 0)
    {
      fBag = true;
      while (pIndex)
      {
        if (--iBag == 0)
        {
          break;
        }
        pIndex = pIndex->pNext;
      }
    }
    else if (iBag < 0)
    {
      pIndex = nullptr;
    }
  }

  if (pIndex && (iFind > 0 || szTag[0]))
  {
    do
    {
      pTag = pIndex->TagList;
      while (pTag != nullptr)
      {
        iTag++;
        if ((iFind != 0 ? iTag == iFind : !hb_stricmp(pTag->szName, szTag)))
        {
          break;
        }
        pTag = pTag->pNext;
      }
      if (pTag || fBag)
      {
        break;
      }
      pIndex = pIndex->pNext;
    } while (pIndex);
  }

  if (puiTag)
  {
    if (!pTag)
    {
      *puiTag = 0;
    }
    else if (fBag)
    {
      *puiTag = hb_cdxGetTagNumber(pArea, pTag);
    }
    else
    {
      *puiTag = static_cast<HB_USHORT>(iTag);
    }
  }

  return pTag;
}

// get current active Tag
static LPCDXTAG hb_cdxGetActiveTag(CDXAREAP pArea)
{
  LPCDXTAG pTag;

  if (!pArea->uiTag)
  {
    return nullptr;
  }
  pTag = hb_cdxGetTagByNumber(pArea, pArea->uiTag);
  if (!pTag)
  {
    pArea->uiTag = 0;
  }
  return pTag;
}

// refresh CurKey value and set proper path from RootPage to LeafPage
static bool hb_cdxCurKeyRefresh(CDXAREAP pArea, LPCDXTAG pTag)
{
  if (pArea->dbfarea.lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  if (!pArea->dbfarea.fPositioned)
  {
    pTag->TagEOF = true;
    pTag->fRePos = false;
    pTag->CurKey->rec = 0;
    return false;
  }
  else if (pTag->fRePos || pTag->CurKey->rec != pArea->dbfarea.ulRecNo)
  {
    LPCDXKEY pKey = nullptr, pKey2 = nullptr;

    // Try to find previous if it's key for the same record
    if (pTag->CurKey->rec == pArea->dbfarea.ulRecNo)
    {
      pKey = hb_cdxKeyCopy(pKey, pTag->CurKey);
      hb_cdxTagKeyFind(pTag, pKey);
    }
    if (pTag->CurKey->rec != pArea->dbfarea.ulRecNo)
    {
      bool fValidBuf = pArea->dbfarea.fValidBuffer;
      // not found, create new key from DBF and if differs seek again
      pKey2 = hb_cdxKeyEval(pKey2, pTag);
      if (pKey == nullptr || memcmp(pKey2->val, pKey->val, pKey->len) != 0)
      {
        hb_cdxTagKeyFind(pTag, pKey2);
      }

      // not found, if key was generated from DBF buffer then force to
      // update it, create the new key and if differs seek again
      if (pTag->CurKey->rec != pArea->dbfarea.ulRecNo && fValidBuf)
      {
        SELF_GOTO(&pArea->dbfarea.area, pArea->dbfarea.ulRecNo);
        pKey = hb_cdxKeyEval(pKey, pTag);
        if (memcmp(pKey2->val, pKey->val, pKey->len) != 0)
        {
          hb_cdxTagKeyFind(pTag, pKey);
        }
      }
      if (pTag->CurKey->rec != pArea->dbfarea.ulRecNo && pTag->Template)
      {
        hb_cdxTagGoTop(pTag);
        while (!pTag->TagBOF && !pTag->TagEOF && hb_cdxBottomScope(pTag))
        {
          if (pTag->CurKey->rec == pArea->dbfarea.ulRecNo)
          {
            break;
          }
          hb_cdxTagSkipNext(pTag);
        }
      }
    }
    if (pKey)
    {
      hb_cdxKeyFree(pKey);
    }
    if (pKey2)
    {
      hb_cdxKeyFree(pKey2);
    }
    return pTag->CurKey->rec != 0 && pTag->CurKey->rec == pArea->dbfarea.ulRecNo;
  }
  return true;
}

// skip to next/previous unique key
static HB_ERRCODE hb_cdxDBOISkipUnique(CDXAREAP pArea, LPCDXTAG pTag, HB_LONG lToSkip)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxDBOISkipUnique(%p, %p, %ld)", static_cast<void*>(pArea), static_cast<void*>(pTag), lToSkip));
#endif

  HB_ERRCODE retval;
  auto fForward = false;

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  if (!pTag)
  {
    return SELF_SKIP(&pArea->dbfarea.area, lToSkip);
  }

  if (pArea->dbfarea.lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = false;

  // CL53 DBFCDX when index is active use this parameter
  // only to chose forward or backward skipping
  fForward = lToSkip >= 0;

  if (!pArea->dbfarea.fPositioned)
  {
    if (fForward)
    {
      retval = SELF_GOTO(&pArea->dbfarea.area, 0);
    }
    else
    {
      retval = SELF_GOBOTTOM(&pArea->dbfarea.area);
    }
  }
  else
  {
    LPCDXKEY pKey = nullptr;
    auto fOut = false;

    hb_cdxIndexLockRead(pTag->pIndex);
    hb_cdxTagRefreshScope(pTag);
    if (!hb_cdxCurKeyRefresh(pArea, pTag))
    {
      if (pTag->TagEOF || (fForward ? !hb_cdxBottomScope(pTag) : !hb_cdxTopScope(pTag)))
      {
        fOut = true;
      }
      else if ((fForward ? pTag->UsrAscend && hb_cdxTopScope(pTag) : !pTag->UsrAscend && hb_cdxBottomScope(pTag)) &&
               pTag->CurKey->rec != 0)
      {
        pKey = hb_cdxKeyEval(pKey, pTag);
      }
    }
    if (fForward)
    {
      if (pArea->dbfarea.fPositioned && !pTag->TagEOF)
      {
        if (!pKey)
        {
          pKey = hb_cdxKeyCopy(nullptr, pTag->CurKey);
          hb_cdxTagSkipNext(pTag);
        }
        while (!pTag->TagEOF)
        {
          if (hb_cdxValCompare(pTag, pKey->val, pKey->len, pTag->CurKey->val, pTag->CurKey->len, CDX_CMP_EXACT) != 0)
          {
            SELF_GOTO(&pArea->dbfarea.area, pTag->CurKey->rec);
            SELF_SKIPFILTER(&pArea->dbfarea.area, 1);
            break;
          }
          hb_cdxTagSkipNext(pTag);
        }
      }
      retval = SELF_GOTO(&pArea->dbfarea.area, (!pArea->dbfarea.fPositioned || pTag->TagEOF) ? 0 : pTag->CurKey->rec);
    }
    else
    {
      if (!fOut && !pTag->TagBOF)
      {
        if (!pKey)
        {
          pKey = hb_cdxKeyCopy(nullptr, pTag->CurKey);
          hb_cdxTagSkipPrev(pTag);
        }
        while (!pTag->TagBOF)
        {
          if (hb_cdxValCompare(pTag, pKey->val, pKey->len, pTag->CurKey->val, pTag->CurKey->len, CDX_CMP_EXACT) != 0)
          {
            SELF_GOTO(&pArea->dbfarea.area, pTag->CurKey->rec);
            SELF_SKIPFILTER(&pArea->dbfarea.area, -1);
            break;
          }
          hb_cdxTagSkipPrev(pTag);
        }
      }

      if (fOut || pTag->TagBOF)
      {
        retval = SELF_GOTOP(&pArea->dbfarea.area);
        pArea->dbfarea.area.fBof = true;
      }
      else
      {
        retval = SELF_GOTO(&pArea->dbfarea.area, pTag->CurKey->rec);
      }
    }
    hb_cdxIndexUnLockRead(pTag->pIndex);
    if (pKey)
    {
      hb_cdxKeyFree(pKey);
    }
  }
  // Update Bof and Eof flags
  if (fForward)
  {
    pArea->dbfarea.area.fBof = false;
  }
  else
  {
    pArea->dbfarea.area.fEof = false;
  }

  return retval;
}

// skip while code block doesn't return true
static bool hb_cdxDBOISkipEval(CDXAREAP pArea, LPCDXTAG pTag, bool fForward, PHB_ITEM pEval)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxDBOISkipEval(%p, %p, %i, %p)", static_cast<void*>(pArea), static_cast<void*>(pTag), fForward, static_cast<void*>(pEval)));
#endif

  auto fFound = false;
  auto fFirst = true;

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return false;
  }

  if (!pTag || (hb_itemType(pEval) & Harbour::Item::BLOCK) == 0)
  {
    if (SELF_SKIP(&pArea->dbfarea.area, fForward ? 1 : -1) == Harbour::FAILURE)
    {
      return false;
    }
    return fForward ? !pArea->dbfarea.area.fEof : !pArea->dbfarea.area.fBof;
  }

  if (pArea->dbfarea.lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = false;

  hb_cdxIndexLockRead(pTag->pIndex);
  hb_cdxTagRefreshScope(pTag);
  if (!hb_cdxCurKeyRefresh(pArea, pTag))
  {
    if (!pTag->TagEOF && pTag->CurKey->rec != 0 && (fForward ? pTag->UsrAscend : !pTag->UsrAscend) &&
        hb_cdxTopScope(pTag) && hb_cdxBottomScope(pTag))
    {
      fFirst = false;
    }
  }
  if (fForward)
  {
    if (fFirst)
    {
      hb_cdxTagSkipNext(pTag);
    }
    while (!pTag->TagEOF)
    {
      if (SELF_GOTO(&pArea->dbfarea.area, pTag->CurKey->rec) == Harbour::FAILURE)
      {
        break;
      }
      if (hb_cdxEvalSeekCond(pTag, pEval))
      {
        HB_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
        SELF_SKIPFILTER(&pArea->dbfarea.area, 1);
        if (pArea->dbfarea.ulRecNo == ulRecNo || hb_cdxEvalSeekCond(pTag, pEval))
        {
          fFound = true;
          break;
        }
      }
      hb_cdxTagSkipNext(pTag);
    }
    if (!fFound)
    {
      SELF_GOTO(&pArea->dbfarea.area, 0);
    }
  }
  else
  {
    if (fFirst)
    {
      hb_cdxTagSkipPrev(pTag);
    }
    while (!pTag->TagBOF)
    {
      if (SELF_GOTO(&pArea->dbfarea.area, pTag->CurKey->rec) == Harbour::FAILURE)
      {
        break;
      }
      if (hb_cdxEvalSeekCond(pTag, pEval))
      {
        HB_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
        SELF_SKIPFILTER(&pArea->dbfarea.area, -1);
        if (pArea->dbfarea.ulRecNo == ulRecNo || hb_cdxEvalSeekCond(pTag, pEval))
        {
          fFound = true;
          break;
        }
      }
      hb_cdxTagSkipPrev(pTag);
    }
    if (!fFound)
    {
      SELF_GOTOP(&pArea->dbfarea.area);
      pArea->dbfarea.area.fBof = true;
    }
  }
  hb_cdxIndexUnLockRead(pTag->pIndex);

  // Update Bof and Eof flags
  if (fForward)
  {
    pArea->dbfarea.area.fBof = false;
  }
  else
  {
    pArea->dbfarea.area.fEof = false;
  }

  return fFound;
}

// skip while comparison with given pattern with wildcards doesn't return true
static bool hb_cdxDBOISkipWild(CDXAREAP pArea, LPCDXTAG pTag, bool fForward, PHB_ITEM pWildItm)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxDBOISkipWild(%p, %p, %i, %p)", static_cast<void*>(pArea), static_cast<void*>(pTag), fForward, static_cast<void*>(pWildItm)));
#endif

  auto fFound = false;
  auto fFirst = true;
  char *szFree = nullptr;
  int iFixed = 0, iStop;

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return false;
  }

  auto szPattern = hb_itemGetCPtr(pWildItm);

  if (!pTag || pTag->uiType != 'C' || !szPattern || !*szPattern)
  {
    if (SELF_SKIP(&pArea->dbfarea.area, fForward ? 1 : -1) == Harbour::FAILURE)
    {
      return false;
    }
    return fForward ? pArea->dbfarea.fPositioned : !pArea->dbfarea.area.fBof;
  }

  if (pArea->dbfarea.area.cdPage != hb_vmCDP())
  {
    szPattern = szFree = hb_cdpDup(szPattern, hb_vmCDP(), pArea->dbfarea.area.cdPage);
  }

  while (iFixed < pTag->uiLen && szPattern[iFixed] && szPattern[iFixed] != '*' && szPattern[iFixed] != '?')
  {
    ++iFixed;
  }

  if (pArea->dbfarea.lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = false;

  hb_cdxIndexLockRead(pTag->pIndex);
  hb_cdxTagRefreshScope(pTag);
  if (!hb_cdxCurKeyRefresh(pArea, pTag))
  {
    if (!pTag->TagEOF && pTag->CurKey->rec != 0 && (fForward ? pTag->UsrAscend : !pTag->UsrAscend) &&
        hb_cdxTopScope(pTag) && hb_cdxBottomScope(pTag))
    {
      fFirst = false;
    }
  }

  iStop = pTag->UsrAscend ? -1 : 1;
  if (!fForward)
  {
    iStop = -iStop;
  }

  if (iFixed && !pTag->TagEOF && pTag->CurKey->rec != 0 &&
      hb_cdxValCompare(pTag, reinterpret_cast<const HB_BYTE *>(szPattern), iFixed, pTag->CurKey->val, iFixed,
                       CDX_CMP_PREFIX) == -iStop)
  {
    LPCDXKEY pKey;

    pKey = hb_cdxKeyPut(nullptr, reinterpret_cast<const HB_BYTE *>(szPattern), static_cast<HB_USHORT>(iFixed),
                        pTag->UsrAscend ? CDX_IGNORE_REC_NUM : CDX_MAX_REC_NUM);
    pKey->mode = CDX_CMP_PREFIX;
    if (!hb_cdxTagKeyFind(pTag, pKey))
    {
      if (fForward)
      {
        pTag->TagEOF = true;
      }
      else
      {
        pTag->TagBOF = true;
      }
    }
    hb_cdxKeyFree(pKey);
    fFirst = false;
  }

  if (fForward)
  {
    if (fFirst)
    {
      hb_cdxTagSkipNext(pTag);
    }
    while (!pTag->TagEOF)
    {
      if (hb_strMatchWild(reinterpret_cast<const char *>(pTag->CurKey->val), szPattern))
      {
        HB_ULONG ulRecNo = pTag->CurKey->rec;
        if (SELF_GOTO(&pArea->dbfarea.area, ulRecNo) != Harbour::SUCCESS)
        {
          break;
        }
        SELF_SKIPFILTER(&pArea->dbfarea.area, 1);
        if (pArea->dbfarea.ulRecNo == ulRecNo ||
            hb_strMatchWild(reinterpret_cast<const char *>(pTag->CurKey->val), szPattern))
        {
          fFound = true;
          break;
        }
      }
      if (iFixed && hb_cdxValCompare(pTag, reinterpret_cast<const HB_BYTE *>(szPattern), iFixed, pTag->CurKey->val,
                                     iFixed, CDX_CMP_PREFIX) == iStop)
      {
        break;
      }
      hb_cdxTagSkipNext(pTag);
    }
    if (!fFound)
    {
      SELF_GOTO(&pArea->dbfarea.area, 0);
    }
  }
  else
  {
    if (fFirst)
    {
      hb_cdxTagSkipPrev(pTag);
    }
    while (!pTag->TagBOF)
    {
      if (hb_strMatchWild(reinterpret_cast<const char *>(pTag->CurKey->val), szPattern))
      {
        HB_ULONG ulRecNo = pTag->CurKey->rec;
        if (SELF_GOTO(&pArea->dbfarea.area, ulRecNo) != Harbour::SUCCESS)
        {
          break;
        }
        SELF_SKIPFILTER(&pArea->dbfarea.area, -1);
        if (pArea->dbfarea.ulRecNo == ulRecNo ||
            hb_strMatchWild(reinterpret_cast<const char *>(pTag->CurKey->val), szPattern))
        {
          fFound = true;
          break;
        }
      }
      if (iFixed && hb_cdxValCompare(pTag, reinterpret_cast<const HB_BYTE *>(szPattern), iFixed, pTag->CurKey->val,
                                     iFixed, CDX_CMP_PREFIX) == iStop)
      {
        break;
      }
      hb_cdxTagSkipPrev(pTag);
    }
    if (!fFound)
    {
      SELF_GOTOP(&pArea->dbfarea.area);
      pArea->dbfarea.area.fBof = true;
    }
  }
  hb_cdxIndexUnLockRead(pTag->pIndex);

  // Update Bof and Eof flags
  if (fForward)
  {
    pArea->dbfarea.area.fBof = false;
  }
  else
  {
    pArea->dbfarea.area.fEof = false;
  }

  if (szFree != nullptr)
  {
    hb_xfree(szFree);
  }

  return fFound;
}

static bool hb_cdxRegexMatch(CDXAREAP pArea, PHB_REGEX pRegEx, LPCDXKEY pKey)
{
  const char *szKey = reinterpret_cast<const char *>(pKey->val);
  HB_SIZE nLen = pKey->len;
  char *pszBuff = nullptr;
  auto fResult = false;

  if (pArea->dbfarea.area.cdPage != hb_vmCDP())
  {
    szKey = pszBuff = hb_cdpnDup(szKey, &nLen, pArea->dbfarea.area.cdPage, hb_vmCDP());
  }
  fResult = hb_regexMatch(pRegEx, szKey, nLen, false);
  if (pszBuff)
  {
    hb_xfree(pszBuff);
  }

  return fResult;
}

// skip while regular expression on index key val doesn't return true
static bool hb_cdxDBOISkipRegEx(CDXAREAP pArea, LPCDXTAG pTag, bool fForward, PHB_ITEM pRegExItm)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxDBOISkipRegEx(%p, %p, %i, %p)", static_cast<void*>(pArea), static_cast<void*>(pTag), fForward, static_cast<void*>(pRegExItm)));
#endif

  auto fFound = false;
  auto fFirst = true;
  PHB_REGEX pRegEx;

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return false;
  }

  if (!pTag || pTag->uiType != 'C' || (pRegEx = hb_regexGet(pRegExItm, 0)) == nullptr)
  {
    if (SELF_SKIP(&pArea->dbfarea.area, fForward ? 1 : -1) == Harbour::FAILURE)
    {
      return false;
    }
    return fForward ? pArea->dbfarea.fPositioned : !pArea->dbfarea.area.fBof;
  }

  if (pArea->dbfarea.lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = false;

  hb_cdxIndexLockRead(pTag->pIndex);
  hb_cdxTagRefreshScope(pTag);
  if (!hb_cdxCurKeyRefresh(pArea, pTag))
  {
    if (!pTag->TagEOF && pTag->CurKey->rec != 0 && (fForward ? pTag->UsrAscend : !pTag->UsrAscend) &&
        hb_cdxTopScope(pTag) && hb_cdxBottomScope(pTag))
    {
      fFirst = false;
    }
  }
  if (fForward)
  {
    if (fFirst)
    {
      hb_cdxTagSkipNext(pTag);
    }
    while (!pTag->TagEOF)
    {
      if (hb_cdxRegexMatch(pArea, pRegEx, pTag->CurKey))
      {
        HB_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
        SELF_SKIPFILTER(&pArea->dbfarea.area, 1);
        if (pArea->dbfarea.ulRecNo == ulRecNo || hb_cdxRegexMatch(pArea, pRegEx, pTag->CurKey))
        {
          fFound = true;
          break;
        }
      }
      hb_cdxTagSkipNext(pTag);
    }
    SELF_GOTO(&pArea->dbfarea.area, fFound ? pTag->CurKey->rec : 0);
  }
  else
  {
    if (fFirst)
    {
      hb_cdxTagSkipPrev(pTag);
    }
    while (!pTag->TagBOF)
    {
      if (hb_cdxRegexMatch(pArea, pRegEx, pTag->CurKey))
      {
        HB_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
        SELF_SKIPFILTER(&pArea->dbfarea.area, -1);
        if (pArea->dbfarea.ulRecNo == ulRecNo || hb_cdxRegexMatch(pArea, pRegEx, pTag->CurKey))
        {
          fFound = true;
          break;
        }
      }
      hb_cdxTagSkipPrev(pTag);
    }
    if (fFound)
    {
      SELF_GOTO(&pArea->dbfarea.area, pTag->CurKey->rec);
    }
    else
    {
      SELF_GOTOP(&pArea->dbfarea.area);
      pArea->dbfarea.area.fBof = true;
    }
  }
  hb_cdxIndexUnLockRead(pTag->pIndex);

  // Update Bof and Eof flags
  if (fForward)
  {
    pArea->dbfarea.area.fBof = false;
  }
  else
  {
    pArea->dbfarea.area.fEof = false;
  }

  hb_regexFree(pRegEx);

  return fFound;
}

// evaluate given C function in given scope
static HB_ULONG hb_cdxDBOIScopeEval(LPCDXTAG pTag, HB_EVALSCOPE_FUNC pFunc, void *pParam, PHB_ITEM pItemLo,
                                    PHB_ITEM pItemHi)
{
  HB_ULONG ulCount = 0, ulLen = static_cast<HB_ULONG>(pTag->uiLen);
  LPCDXKEY pCurKey = hb_cdxKeyCopy(nullptr, pTag->CurKey), pTopScopeKey = pTag->topScopeKey,
           pBtmScopeKey = pTag->bottomScopeKey;

  // TODO: RT error when item type differ then Tag type
  if (!pItemLo || pItemLo->isNil())
  {
    pTag->topScopeKey = nullptr;
  }
  else
  {
    pTag->topScopeKey = hb_cdxKeyPutItem(nullptr, pItemLo, CDX_IGNORE_REC_NUM, pTag, CDX_CMP_PREFIX);
  }

  if (!pItemHi || pItemHi->isNil())
  {
    pTag->bottomScopeKey = nullptr;
  }
  else
  {
    pTag->bottomScopeKey = hb_cdxKeyPutItem(nullptr, pItemHi, CDX_MAX_REC_NUM, pTag, CDX_CMP_PREFIX);
  }

  hb_cdxIndexLockRead(pTag->pIndex);
  hb_cdxTagGoTop(pTag);
  while (!pTag->TagEOF)
  {
    pFunc(pTag->CurKey->rec, pTag->CurKey->val, ulLen, pParam);
    ulCount++;
    hb_cdxTagSkipNext(pTag);
  }
  hb_cdxIndexUnLockRead(pTag->pIndex);

  if (pTag->topScopeKey)
  {
    hb_cdxKeyFree(pTag->topScopeKey);
  }
  pTag->topScopeKey = pTopScopeKey;
  if (pTag->bottomScopeKey)
  {
    hb_cdxKeyFree(pTag->bottomScopeKey);
  }
  pTag->bottomScopeKey = pBtmScopeKey;
  pTag->curKeyState &= ~(CDX_CURKEY_RAWPOS | CDX_CURKEY_LOGPOS);

  pTag->fRePos = true;
  hb_cdxKeyFree(pTag->CurKey);
  pTag->CurKey = pCurKey;

  return ulCount;
}

// return number of keys in order
static HB_LONG hb_cdxDBOIKeyCount(CDXAREAP pArea, LPCDXTAG pTag, bool fFilters)
{
  HB_ULONG ulKeyCount = 0;
  bool fLogOpt = pArea->dbfarea.area.dbfi.itmCobExpr || !pArea->dbfarea.area.dbfi.fFilter;

  if (pTag != nullptr)
  {
    bool fCheckFilter = (fLogOpt && fFilters && pArea->dbfarea.area.dbfi.itmCobExpr);
    HB_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
    LPCDXKEY pCurKey;
    hb_cdxIndexLockRead(pTag->pIndex);
    hb_cdxTagRefreshScope(pTag);

    if (pTag && (fFilters ? fLogOpt && CURKEY_LOGCNT(pTag) : CURKEY_RAWCNT(pTag)))
    {
      ulKeyCount = fFilters ? pTag->logKeyCount : pTag->rawKeyCount;
    }
    else
    {
      if (pTag->topScopeKey || pTag->bottomScopeKey || pTag->UsrUnique || pArea->dbfarea.area.dbfi.fFilter)
      {
        pCurKey = hb_cdxKeyCopy(nullptr, pTag->CurKey);
        hb_cdxTagGoTop(pTag);
        while (!pTag->TagEOF)
        {
          if (!fCheckFilter || hb_cdxCheckRecordFilter(pArea, pTag->CurKey->rec))
          {
            ulKeyCount++;
          }
          hb_cdxTagSkipNext(pTag);
        }
        pTag->fRePos = true;
        hb_cdxKeyFree(pTag->CurKey);
        pTag->CurKey = pCurKey;
        if (fCheckFilter)
        {
          SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
        }
      }
      else
      {
        LPCDXPAGE pPage;
        pCurKey = hb_cdxKeyCopy(nullptr, pTag->CurKey);
        if (pTag->UsrAscend)
        {
          hb_cdxTagGoTop(pTag);
        }
        else
        {
          hb_cdxTagGoBottom(pTag);
        }
        pPage = pTag->RootPage;
        while (pPage->Child)
        {
          pPage = pPage->Child;
        }
        ulKeyCount = pPage->iKeys;
        if (pPage->Right != CDX_DUMMYNODE)
        {
          HB_ULONG ulPage = pPage->Right;
          pPage = hb_cdxPageNew(pTag, nullptr, CDX_DUMMYNODE);
          pPage->Page = ulPage;
          while (pPage->Page != CDX_DUMMYNODE)
          {
            hb_cdxPageLoad(pPage);
            ulKeyCount += pPage->iKeys;
            pPage->Page = pPage->Right;
          }
          hb_cdxPageFree(pPage, true);
        }
        pTag->fRePos = true;
        hb_cdxKeyFree(pTag->CurKey);
        pTag->CurKey = pCurKey;
      }
      if (!fFilters)
      {
        pTag->rawKeyCount = ulKeyCount;
        pTag->curKeyState |= CDX_CURKEY_RAWCNT;
      }
      else if (fLogOpt)
      {
        pTag->logKeyCount = ulKeyCount;
        pTag->curKeyState |= CDX_CURKEY_LOGCNT;
      }
    }
    hb_cdxIndexUnLockRead(pTag->pIndex);
  }
  else
  { // no filter, no order
    if (fLogOpt && fFilters && pArea->dbfarea.area.dbfi.itmCobExpr)
    {
      HB_ULONG ulRecNo = pArea->dbfarea.ulRecNo;

      if (SELF_GOTOP(&pArea->dbfarea.area) == Harbour::SUCCESS)
      {
        while (!pArea->dbfarea.area.fEof)
        {
          ulKeyCount++;
          if (SELF_SKIP(&pArea->dbfarea.area, 1) != Harbour::SUCCESS)
          {
            break;
          }
        }
        SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
      }
    }
    else
    {
      SELF_RECCOUNT(&pArea->dbfarea.area, &ulKeyCount);
    }
  }
  return ulKeyCount;
}

// return logical key position in order
static HB_LONG hb_cdxDBOIKeyNo(CDXAREAP pArea, LPCDXTAG pTag, bool fFilters)
{
  HB_ULONG ulKeyNo = 0;
  bool fLogOpt = pArea->dbfarea.area.dbfi.itmCobExpr || !pArea->dbfarea.area.dbfi.fFilter;

  if (pArea->dbfarea.lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  if (!pArea->dbfarea.fPositioned)
  {
    ulKeyNo = 0;
  }
  else if (pTag != nullptr)
  {
    bool fCheckFilter = (fLogOpt && fFilters && pArea->dbfarea.area.dbfi.itmCobExpr);
    HB_ULONG ulRecNo = pArea->dbfarea.ulRecNo;

    hb_cdxIndexLockRead(pTag->pIndex);
    hb_cdxTagRefreshScope(pTag);

    if (fFilters ? (fLogOpt && CURKEY_LOGPOS(pTag))
                 : (CURKEY_RAWPOS(pTag) && pTag->rawKeyRec == pArea->dbfarea.ulRecNo))
    {
      ulKeyNo = fFilters ? pTag->logKeyPos : pTag->rawKeyPos;
    }
    else
    {
      hb_cdxTagOpen(pTag);
      if (hb_cdxCurKeyRefresh(pArea, pTag))
      {
        if (pTag->topScopeKey || pTag->bottomScopeKey || pTag->UsrUnique || pArea->dbfarea.area.dbfi.fFilter)
        {
          if (hb_cdxBottomScope(pTag) && hb_cdxTopScope(pTag) &&
              (!fCheckFilter || hb_cdxCheckRecordFilter(pArea, ulRecNo)))
          {
            LPCDXKEY pCurKey = hb_cdxKeyCopy(nullptr, pTag->CurKey);
            if (!hb_cdxCheckRecordScope(pArea, pTag->CurKey->rec))
            {
              hb_cdxTagSkipPrev(pTag);
            }
            while (!pTag->TagBOF)
            {
              if (!fCheckFilter || hb_cdxCheckRecordFilter(pArea, pTag->CurKey->rec))
              {
                ulKeyNo++;
              }
              hb_cdxTagSkipPrev(pTag);
            }
            pTag->fRePos = true;
            hb_cdxKeyFree(pTag->CurKey);
            pTag->CurKey = pCurKey;
            if (fCheckFilter)
            {
              SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
            }
          }
        }
        else
        {
          LPCDXPAGE pPage = pTag->RootPage;
          while (pPage->Child)
          {
            pPage = pPage->Child;
          }
          if (pTag->UsrAscend)
          {
            ulKeyNo = pPage->iCurKey + 1;
            if (pPage->Left != CDX_DUMMYNODE)
            {
              HB_ULONG ulPage = pPage->Left;
              pPage = hb_cdxPageNew(pTag, nullptr, CDX_DUMMYNODE);
              pPage->Page = ulPage;
              while (pPage->Page != CDX_DUMMYNODE)
              {
                hb_cdxPageLoad(pPage);
                ulKeyNo += pPage->iKeys;
                pPage->Page = pPage->Left;
              }
              hb_cdxPageFree(pPage, true);
            }
          }
          else
          {
            ulKeyNo = pPage->iKeys - pPage->iCurKey;
            if (pPage->Right != CDX_DUMMYNODE)
            {
              HB_ULONG ulPage = pPage->Right;
              pPage = hb_cdxPageNew(pTag, nullptr, CDX_DUMMYNODE);
              pPage->Page = ulPage;
              while (pPage->Page != CDX_DUMMYNODE)
              {
                hb_cdxPageLoad(pPage);
                ulKeyNo += pPage->iKeys;
                pPage->Page = pPage->Right;
              }
              hb_cdxPageFree(pPage, true);
            }
          }
        }
        if (ulKeyNo != 0)
        {
          if (!fFilters)
          {
            pTag->rawKeyPos = ulKeyNo;
            CURKEY_SETRAWPOS(pTag);
          }
          else if (fLogOpt)
          {
            pTag->logKeyPos = ulKeyNo;
            CURKEY_SETLOGPOS(pTag);
          }
        }
      }
    }
    hb_cdxIndexUnLockRead(pTag->pIndex);
  }
  else
  {
    HB_ULONG ulRecNo = pArea->dbfarea.ulRecNo;

    if (fLogOpt && fFilters && pArea->dbfarea.area.dbfi.itmCobExpr)
    {
      if (hb_cdxCheckRecordFilter(pArea, ulRecNo))
      {
        do
        {
          ulKeyNo++;
          if (SELF_SKIP(&pArea->dbfarea.area, -1) != Harbour::SUCCESS)
          {
            break;
          }
        } while (!(&pArea->dbfarea.area)->fBof);
        SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
      }
    }
    else
    {
      ulKeyNo = ulRecNo;
    }
  }
  return ulKeyNo;
}

// DBOI_KEYGOTO goto specific logical record in the index file
static HB_ERRCODE hb_cdxDBOIKeyGoto(CDXAREAP pArea, LPCDXTAG pTag, HB_ULONG ulKeyNo, bool fFilters)
{
  HB_ERRCODE retval;
  HB_ULONG ulKeyCnt = ulKeyNo;
  bool fLogOpt = pArea->dbfarea.area.dbfi.itmCobExpr || !pArea->dbfarea.area.dbfi.fFilter;

  if (ulKeyNo == 0)
  {
    retval = SELF_GOTO(&pArea->dbfarea.area, 0);
  }
  else if (pTag != nullptr)
  {
    bool fCheckFilter = (fLogOpt && fFilters && pArea->dbfarea.area.dbfi.itmCobExpr);
    hb_cdxIndexLockRead(pTag->pIndex);
    hb_cdxTagRefreshScope(pTag);
    if (!pArea->dbfarea.lpdbPendingRel && (fFilters ? fLogOpt && CURKEY_LOGPOS(pTag) && pTag->logKeyPos == ulKeyNo
                                                    : (CURKEY_RAWPOS(pTag) && pTag->rawKeyPos == ulKeyNo)))
    {
      retval = SELF_GOTO(&pArea->dbfarea.area, fFilters ? pTag->logKeyRec : pTag->rawKeyRec);
    }
    else
    {
      if (pTag->topScopeKey || pTag->bottomScopeKey || pTag->UsrUnique || pArea->dbfarea.area.dbfi.fFilter)
      {
        hb_cdxTagGoTop(pTag);
        if (fCheckFilter)
        {
          while (!pTag->TagEOF)
          {
            if (hb_cdxCheckRecordFilter(pArea, pTag->CurKey->rec))
            {
              if (!--ulKeyCnt)
              {
                break;
              }
            }
            hb_cdxTagSkipNext(pTag);
          }
        }
        else
        {
          while (!pTag->TagEOF && --ulKeyCnt)
          {
            hb_cdxTagSkipNext(pTag);
          }
        }
      }
      else
      {
        LPCDXPAGE pPage, pOwnerPage = nullptr;
        HB_ULONG ulNextPg;
        hb_cdxTagGoTop(pTag);
        pPage = pTag->RootPage;
        while (pPage->Child)
        {
          pOwnerPage = pPage;
          pPage = pPage->Child;
        }
        while (static_cast<HB_ULONG>(pPage->iKeys) < ulKeyCnt && pOwnerPage &&
               (ulNextPg = pTag->UsrAscend ? pPage->Right : pPage->Left) != CDX_DUMMYNODE)
        {
          ulKeyCnt -= pPage->iKeys;
          pOwnerPage->Child = hb_cdxPageNew(pPage->TagParent, pPage->Owner, ulNextPg);
          hb_cdxPageFree(pPage, false);
          pPage = pOwnerPage->Child;
        }
        if (static_cast<HB_ULONG>(pPage->iKeys) >= ulKeyCnt)
        {
          pPage->iCurKey = pTag->UsrAscend ? static_cast<int>(ulKeyCnt) - 1 : pPage->iKeys - static_cast<int>(ulKeyCnt);
          hb_cdxSetCurKey(pPage);
        }
        else
        {
          pTag->CurKey->rec = 0;
        }
      }
      retval = SELF_GOTO(&pArea->dbfarea.area, pTag->CurKey->rec);
      if (pArea->dbfarea.fPositioned)
      {
        if (!fFilters)
        {
          pTag->rawKeyPos = ulKeyNo;
          CURKEY_SETRAWPOS(pTag);
        }
        else if (fLogOpt)
        {
          pTag->logKeyPos = ulKeyNo;
          CURKEY_SETLOGPOS(pTag);
        }
      }
    }
    hb_cdxIndexUnLockRead(pTag->pIndex);
  }
  else
  {
    if (fLogOpt && fFilters && pArea->dbfarea.area.dbfi.itmCobExpr)
    {
      retval = SELF_GOTOP(&pArea->dbfarea.area);
      if (retval == Harbour::SUCCESS && --ulKeyCnt)
      {
        retval = SELF_SKIP(&pArea->dbfarea.area, ulKeyCnt);
      }
    }
    else
    {
      retval = SELF_GOTO(&pArea->dbfarea.area, ulKeyNo);
    }
  }

  return retval;
}

static double hb_cdxCountRelKeyPos(LPCDXPAGE pPage)
{
  return ((pPage->Child ? hb_cdxCountRelKeyPos(pPage->Child) : 0.5) + pPage->iCurKey) / pPage->iKeys;
}

static bool hb_cdxGoToRelKeyPos(LPCDXPAGE pPage, double dPos)
{
  do
  {
    if (pPage->iKeys == 0)
    {
      return false;
    }

    pPage->iCurKey = static_cast<int>(dPos * pPage->iKeys);
    if (pPage->iCurKey >= pPage->iKeys)
    {
      pPage->iCurKey = pPage->iKeys - 1;
    }

    if ((pPage->PageType & CDX_NODE_LEAF) != 0)
    {
      break;
    }

    dPos = dPos * pPage->iKeys - pPage->iCurKey;
    if (dPos < 0.0)
    {
      dPos = 0.0;
    }
    else if (dPos >= 1.0)
    {
      dPos = 1.0;
    }

    hb_cdxPageGetChild(pPage);
    pPage = pPage->Child;
  } while (pPage);

  return true;
}

static double hb_cdxDBOIGetRelKeyPos(CDXAREAP pArea, LPCDXTAG pTag)
{
  HB_ULONG ulRecNo = 0, ulRecCount = 0;
  double dPos = 0.0;

  // resolve any pending relations
  SELF_RECNO(&pArea->dbfarea.area, &ulRecNo);

  if (!pArea->dbfarea.fPositioned)
  {
    if (ulRecNo > 1)
    {
      dPos = 1.0;
    }
  }
  else if (!pTag)
  {
    SELF_RECCOUNT(&pArea->dbfarea.area, &ulRecCount);
    if (ulRecCount != 0)
    {
      dPos = (0.5 + ulRecNo) / ulRecCount;
    }
  }
  else
  {
    LPCDXKEY pKey;
    double dStart, dStop, dFact = 0.0000000000001;
    auto fOK = true;

    if (pTag->UsrAscend)
    {
      dStart = 0.0;
      dStop = 1.0;
    }
    else
    {
      dStart = 1.0;
      dStop = 0.0;
    }

    hb_cdxIndexLockRead(pTag->pIndex);
    hb_cdxTagRefreshScope(pTag);

    pKey = pTag->UsrAscend ? pTag->topScopeKey : pTag->bottomScopeKey;
    if (pKey)
    {
      hb_cdxTagKeyFind(pTag, pKey);
      if (pTag->CurKey->rec == 0 || pTag->TagEOF || !hb_cdxBottomScope(pTag))
      {
        fOK = false;
      }
      else
      {
        dStart = hb_cdxCountRelKeyPos(pTag->RootPage);
      }
    }
    pKey = pTag->UsrAscend ? pTag->bottomScopeKey : pTag->topScopeKey;
    if (pKey && fOK)
    {
      hb_cdxTagKeyFind(pTag, pKey);
      if (pTag->CurKey->rec == 0 || pTag->TagBOF || !hb_cdxTopScope(pTag))
      {
        fOK = false;
      }
      else
      {
        dStop = hb_cdxCountRelKeyPos(pTag->RootPage);
      }
    }
    if (fOK)
    {
      if (!pTag->UsrAscend)
      {
        double dTmp = dStart;
        dStart = dStop;
        dStop = dTmp;
      }
      pTag->fRePos = true;
      if (hb_cdxCurKeyRefresh(pArea, pTag) && hb_cdxTopScope(pTag) && hb_cdxBottomScope(pTag))
      {
        if (dStart >= dStop - dFact)
        {
          dPos = 0.5;
        }
        else
        {
          dPos = hb_cdxCountRelKeyPos(pTag->RootPage);
          dPos = (dPos - dStart) / (dStop - dStart);
          if (!pTag->UsrAscend)
          {
            dPos = 1.0 - dPos;
          }
          // fix possible differences in FL representation
          if (dPos <= 0.0)
          {
            dPos = 0.0;
          }
          else if (dPos >= 1.0)
          {
            dPos = 1.0;
          }
        }
      }
    }
    hb_cdxIndexUnLockRead(pTag->pIndex);
  }

  return dPos;
}

static void hb_cdxDBOISetRelKeyPos(CDXAREAP pArea, LPCDXTAG pTag, double dPos)
{
  if (!pTag)
  {
    if (dPos >= 1.0)
    {
      SELF_GOBOTTOM(&pArea->dbfarea.area);
    }
    else if (dPos <= 0.0)
    {
      SELF_GOTOP(&pArea->dbfarea.area);
    }
    else
    {
      HB_ULONG ulRecCount, ulRecNo;
      SELF_RECCOUNT(&pArea->dbfarea.area, &ulRecCount);
      ulRecNo = static_cast<HB_ULONG>(dPos) * ulRecCount + 1;
      if (ulRecNo >= ulRecCount)
      {
        ulRecNo = ulRecCount;
      }
      SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
      SELF_SKIPFILTER(&pArea->dbfarea.area, 1);
      if (pArea->dbfarea.area.fEof)
      {
        SELF_GOTOP(&pArea->dbfarea.area);
      }
    }
  }
  else
  {
    auto fForward = true;
    auto fOK = true;
    auto fTop = false;
    hb_cdxIndexLockRead(pTag->pIndex);
    hb_cdxTagRefreshScope(pTag);

    if (dPos >= 1.0)
    {
      fForward = false;
    }
    else if (dPos <= 0.0)
    {
      fTop = true;
    }
    else
    {
      LPCDXKEY pKey;
      double dStart, dStop, dFact = 0.0000000000001;

      if (pTag->UsrAscend)
      {
        dStart = 0.0;
        dStop = 1.0;
      }
      else
      {
        dStart = 1.0;
        dStop = 0.0;
      }

      pKey = pTag->UsrAscend ? pTag->topScopeKey : pTag->bottomScopeKey;
      if (pKey)
      {
        hb_cdxTagKeyFind(pTag, pKey);
        if (pTag->CurKey->rec == 0 || pTag->TagEOF || !hb_cdxBottomScope(pTag))
        {
          fOK = false;
        }
        else
        {
          dStart = hb_cdxCountRelKeyPos(pTag->RootPage);
        }
      }
      pKey = pTag->UsrAscend ? pTag->bottomScopeKey : pTag->topScopeKey;
      if (pKey && fOK)
      {
        hb_cdxTagKeyFind(pTag, pKey);
        if (pTag->CurKey->rec == 0 || pTag->TagBOF || !hb_cdxTopScope(pTag))
        {
          fOK = false;
        }
        else
        {
          dStop = hb_cdxCountRelKeyPos(pTag->RootPage);
        }
      }
      if (fOK)
      {
        if (!pTag->UsrAscend)
        {
          double dTmp = dStart;
          dStart = dStop;
          dStop = dTmp;
          dPos = 1.0 - dPos;
        }
        if (dStart >= dStop - dFact)
        {
          fTop = true;
        }
        else
        {
          dPos = dPos * (dStop - dStart) + dStart;
          pTag->fRePos = false;
          hb_cdxTagOpen(pTag);
          pTag->TagBOF = pTag->TagEOF = false;
          if (!hb_cdxGoToRelKeyPos(pTag->RootPage, dPos))
          {
            fTop = true;
          }
          else
          {
            hb_cdxSetCurKey(pTag->RootPage);
            if (!hb_cdxTopScope(pTag))
            {
              fTop = true;
            }
            else if (!hb_cdxBottomScope(pTag))
            {
              fForward = false;
            }
          }
        }
      }
    }
    if (!fOK)
    {
      SELF_GOTO(&pArea->dbfarea.area, 0);
    }
    else
    {
      if (fForward)
      {
        if (fTop)
        {
          hb_cdxTagGoTop(pTag);
        }
        while (!pTag->TagEOF)
        {
          if (hb_cdxCheckRecordFilter(pArea, pTag->CurKey->rec))
          {
            break;
          }
          hb_cdxTagSkipNext(pTag);
        }
        if (pTag->TagEOF && !fTop)
        {
          fForward = false;
        }
      }
      if (!fForward)
      {
        hb_cdxTagGoBottom(pTag);
        while (!pTag->TagBOF)
        {
          if (hb_cdxCheckRecordFilter(pArea, pTag->CurKey->rec))
          {
            break;
          }
          hb_cdxTagSkipPrev(pTag);
        }
        if (pTag->TagBOF)
        {
          pTag->CurKey->rec = 0;
        }
      }
      SELF_GOTO(&pArea->dbfarea.area, pTag->CurKey->rec);
    }
    hb_cdxIndexUnLockRead(pTag->pIndex);
  }
}

// DBOI_FINDREC find a specific record in the tag - it's useful for
// custom indexes when the same record can be stored more then once
// or when the used index key is unknown
static bool hb_cdxDBOIFindRec(CDXAREAP pArea, LPCDXTAG pTag, HB_ULONG ulRecNo, bool fCont)
{
  auto fFound = false;

  if (pTag && ulRecNo)
  {
    if (pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped)
    {
      SELF_FORCEREL(&pArea->dbfarea.area);
    }

    hb_cdxIndexLockRead(pTag->pIndex);
    hb_cdxTagRefreshScope(pTag);
    if (fCont)
    {
      if (!hb_cdxCurKeyRefresh(pArea, pTag))
      {
        ulRecNo = 0;
      }
      else
      {
        hb_cdxTagSkipNext(pTag);
      }
    }
    else
    {
      hb_cdxTagGoTop(pTag);
    }
    if (ulRecNo)
    {
      while (!pTag->TagBOF && !pTag->TagEOF && hb_cdxBottomScope(pTag))
      {
        if (pTag->CurKey->rec == ulRecNo)
        {
          fFound = true;
          break;
        }
        hb_cdxTagSkipNext(pTag);
      }
    }
    hb_cdxIndexUnLockRead(pTag->pIndex);
  }
  SELF_GOTO(&pArea->dbfarea.area, fFound ? ulRecNo : 0);
  return fFound;
}

static void hb_cdxClearLogPosInfo(CDXAREAP pArea)
{
  LPCDXINDEX pIndex = pArea->lpIndexes;
  LPCDXTAG pTag;

  while (pIndex)
  {
    pTag = pIndex->TagList;
    while (pTag != nullptr)
    {
      pTag->curKeyState &= ~(CDX_CURKEY_LOGPOS | CDX_CURKEY_LOGCNT);
      pTag = pTag->pNext;
    }
    pIndex = pIndex->pNext;
  }
}

static void hb_cdxClearPosInfo(CDXAREAP pArea)
{
  LPCDXINDEX pIndex = pArea->lpIndexes;
  LPCDXTAG pTag;

  while (pIndex)
  {
    pTag = pIndex->TagList;
    while (pTag != nullptr)
    {
      pTag->curKeyState &= ~(CDX_CURKEY_LOGPOS | CDX_CURKEY_LOGCNT | CDX_CURKEY_RAWPOS | CDX_CURKEY_RAWCNT);
      pTag = pTag->pNext;
    }
    pIndex = pIndex->pNext;
  }
}

// -- DBFCDX METHODS --

// ( DBENTRYP_BP )    hb_cdxBof     : nullptr
// ( DBENTRYP_BP )    hb_cdxEof     : nullptr
// ( DBENTRYP_BP )    hb_cdxFound   : nullptr

// ( DBENTRYP_V )     hb_cdxGoBottom
static HB_ERRCODE hb_cdxGoBottom(CDXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGoBottom(%p)", static_cast<void*>(pArea)));
#endif

  LPCDXTAG pTag;

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  pTag = hb_cdxGetActiveTag(pArea);
  if (!pTag)
  {
    return SUPER_GOBOTTOM(&pArea->dbfarea.area);
  }

  if (pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped)
  {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  hb_cdxIndexLockRead(pTag->pIndex);
  hb_cdxTagRefreshScope(pTag);

  hb_cdxTagGoBottom(pTag);

  pArea->dbfarea.area.fTop = false;
  pArea->dbfarea.area.fBottom = true;

  HB_ERRCODE retval = SELF_GOTO(&pArea->dbfarea.area, pTag->CurKey->rec);

  if (retval != Harbour::FAILURE && pArea->dbfarea.fPositioned)
  {
    retval = SELF_SKIPFILTER(&pArea->dbfarea.area, -1);

    if (pArea->dbfarea.fPositioned && CURKEY_LOGCNT(pTag))
    {
      pTag->logKeyPos = pTag->logKeyCount;
      CURKEY_SETLOGPOS(pTag);
    }
  }
  hb_cdxIndexUnLockRead(pTag->pIndex);

  return retval;
}

// ( DBENTRYP_UL )    hb_cdxGoTo    : nullptr
// ( DBENTRYP_I )     hb_cdxGoToId  : nullptr

// ( DBENTRYP_V )     hb_cdxGoTop
static HB_ERRCODE hb_cdxGoTop(CDXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGoTop(%p)", static_cast<void*>(pArea)));
#endif

  LPCDXTAG pTag;

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  pTag = hb_cdxGetActiveTag(pArea);
  if (!pTag)
  {
    return SUPER_GOTOP(&pArea->dbfarea.area);
  }

  if (pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped)
  {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  hb_cdxIndexLockRead(pTag->pIndex);
  hb_cdxTagRefreshScope(pTag);

  hb_cdxTagGoTop(pTag);

  pArea->dbfarea.area.fTop = true;
  pArea->dbfarea.area.fBottom = false;

  HB_ERRCODE retval = SELF_GOTO(&pArea->dbfarea.area, pTag->CurKey->rec);

  if (retval != Harbour::FAILURE && pArea->dbfarea.fPositioned)
  {
    retval = SELF_SKIPFILTER(&pArea->dbfarea.area, 1);
  }

  if (retval != Harbour::FAILURE && pArea->dbfarea.fPositioned)
  {
    pTag->logKeyPos = 1;
    CURKEY_SETLOGPOS(pTag);
  }

  hb_cdxIndexUnLockRead(pTag->pIndex);
  return retval;
}

// ( DBENTRYP_BIB )   hb_cdxSeek
static HB_ERRCODE hb_cdxSeek(CDXAREAP pArea, HB_BOOL fSoftSeek, PHB_ITEM pKeyItm, HB_BOOL fFindLast)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxSeek(%p, %d, %p, %d)", static_cast<void*>(pArea), fSoftSeek, static_cast<void*>(pKeyItm), fFindLast));
#endif

  LPCDXTAG pTag;

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  pTag = hb_cdxGetActiveTag(pArea);

  if (!pTag)
  {
    hb_cdxErrorRT(pArea, EG_NOORDER, EDBF_NOTINDEXED, nullptr, 0, EF_CANDEFAULT, nullptr);
    return Harbour::FAILURE;
  }
  else
  {
    LPCDXKEY pKey;
    HB_ERRCODE retval = Harbour::SUCCESS;
    auto fEOF = false;
    auto fLast = false;
    HB_ULONG ulRec;

    if (pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped)
    {
      SELF_FORCEREL(&pArea->dbfarea.area);
    }

    pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = false;
    pArea->dbfarea.area.fEof = false;

    if (pTag->UsrUnique)
    {
      fLast = !pTag->UsrAscend;
    }
    else
    {
      fLast = pTag->UsrAscend ? fFindLast : !fFindLast;
    }

    // TODO: runtime error if ValType(pKeyItm) != pTag->Type
    pKey = hb_cdxKeyPutItem(nullptr, pKeyItm, fLast ? CDX_MAX_REC_NUM : CDX_IGNORE_REC_NUM, pTag, CDX_CMP_PREFIX);

    hb_cdxIndexLockRead(pTag->pIndex);
    hb_cdxTagRefreshScope(pTag);
    ulRec = hb_cdxTagKeyFind(pTag, pKey);
    if ((ulRec == 0 && !fSoftSeek) || pTag->TagEOF)
    {
      fEOF = true;
    }
    else
    { // if( fSoftSeek )
      if (!hb_cdxBottomScope(pTag))
      {
        fEOF = true;
      }
      else if (!hb_cdxTopScope(pTag))
      {
        hb_cdxTagGoTop(pTag);
        if (pTag->CurKey->rec == 0)
        {
          fEOF = true;
        }
      }
    }
    hb_cdxIndexUnLockRead(pTag->pIndex);
    if (!fEOF)
    {
      retval = SELF_GOTO(&pArea->dbfarea.area, pTag->CurKey->rec);
      if (retval != Harbour::FAILURE && pArea->dbfarea.fPositioned)
      {
        retval = SELF_SKIPFILTER(&pArea->dbfarea.area, fFindLast ? -1 : 1);
        if (retval != Harbour::FAILURE && ulRec && pArea->dbfarea.fPositioned)
        {
          pArea->dbfarea.area.fFound =
              (ulRec == pArea->dbfarea.ulRecNo ||
               hb_cdxValCompare(pTag, pKey->val, pKey->len, pTag->CurKey->val, pTag->CurKey->len, pKey->mode) == 0);
          if (!pArea->dbfarea.area.fFound && !fSoftSeek)
          {
            fEOF = true;
          }
        }
      }
    }
    if (retval != Harbour::FAILURE && (fEOF || !hb_cdxTopScope(pTag) || !hb_cdxBottomScope(pTag)))
    {
      retval = SELF_GOTO(&pArea->dbfarea.area, 0);
    }
    pArea->dbfarea.area.fBof = false;
    hb_cdxKeyFree(pKey);
    return retval;
  }
}

// ( DBENTRYP_L )     hb_cdxSkip        : nullptr
static HB_ERRCODE hb_cdxSkip(CDXAREAP pArea, HB_LONG lToSkip)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxSkip(%p, %ld)", static_cast<void*>(pArea), lToSkip));
#endif

  LPCDXTAG pTag;
  HB_ULONG ulPos, ulRec;

  if (pArea->dbfarea.lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  pTag = lToSkip == 0 ? nullptr : hb_cdxGetActiveTag(pArea);
  if (pTag && pArea->dbfarea.fPositioned && CURKEY_LOGPOS(pTag))
  {
    ulPos = pTag->logKeyPos;
    ulRec = pTag->logKeyRec;
  }
  else
  {
    ulPos = ulRec = 0;
  }

  if (SUPER_SKIP(&pArea->dbfarea.area, lToSkip) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  if (pTag != nullptr)
  {
    if (ulPos && (pTag->logKeyPos != ulPos || pTag->logKeyRec != ulRec || (pTag->curKeyState & CDX_CURKEY_LOGPOS) == 0))
    {
      ulPos = 0;
    }

    if (lToSkip > 0)
    {
      if (pArea->dbfarea.area.fEof)
      {
        if (lToSkip == 1 && ulPos && !CURKEY_LOGCNT(pTag))
        {
          pTag->logKeyCount = ulPos;
          pTag->curKeyState |= CDX_CURKEY_LOGCNT;
        }
      }
      else if (ulPos)
      {
        pTag->logKeyPos += lToSkip;
        pTag->logKeyRec = pArea->dbfarea.ulRecNo;
      }
    }
    else if (pArea->dbfarea.area.fBof)
    {
      if (pArea->dbfarea.fPositioned)
      {
        pTag->logKeyPos = 1;
        CURKEY_SETLOGPOS(pTag);
      }
    }
    else if (ulPos)
    {
      pTag->logKeyPos += lToSkip;
      pTag->logKeyRec = pArea->dbfarea.ulRecNo;
    }
  }
  return Harbour::SUCCESS;
}

// ( DBENTRYP_L )     hb_cdxSkipFilter  : nullptr

// ( DBENTRYP_L )     hb_cdxSkipRaw
static HB_ERRCODE hb_cdxSkipRaw(CDXAREAP pArea, HB_LONG lToSkip)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxSkipRaw(%p, %ld)", static_cast<void*>(pArea), lToSkip));
#endif

  LPCDXTAG pTag;
  HB_ERRCODE retval;
  auto fOut = false;
  auto fForward = false;

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  pTag = hb_cdxGetActiveTag(pArea);

  if (!pTag || lToSkip == 0)
  {
    return SUPER_SKIPRAW(&pArea->dbfarea.area, lToSkip);
  }

  if (pArea->dbfarea.lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  fForward = (lToSkip > 0);

  hb_cdxIndexLockRead(pTag->pIndex);
  hb_cdxTagRefreshScope(pTag);
  if (!hb_cdxCurKeyRefresh(pArea, pTag))
  {
    if (fForward)
    {
      if (pTag->TagEOF || !hb_cdxBottomScope(pTag))
      {
        fOut = true;
      }
      else if (pTag->UsrAscend && hb_cdxTopScope(pTag))
      {
        lToSkip--;
      }
    }
    else if (pArea->dbfarea.fPositioned)
    {
      if (pTag->TagEOF || !hb_cdxTopScope(pTag))
      {
        fOut = true;
      }
      else if (!pTag->UsrAscend && hb_cdxBottomScope(pTag))
      {
        lToSkip++;
      }
    }
  }
  if (fForward)
  {
    if (!fOut)
    {
      while (lToSkip-- > 0)
      {
        hb_cdxTagSkipNext(pTag);
        if (pTag->TagEOF)
        {
          fOut = true;
          break;
        }
      }
    }
    retval = SELF_GOTO(&pArea->dbfarea.area, (pTag->TagEOF || fOut) ? 0 : pTag->CurKey->rec);
  }
  else // if( lToSkip < 0 )
  {
    if (fOut)
    {
      hb_cdxTagGoTop(pTag);
    }
    else
    {
      while (lToSkip++ < 0)
      {
        hb_cdxTagSkipPrev(pTag);
        if (pTag->TagBOF)
        {
          fOut = true;
          break;
        }
      }
    }
    retval = SELF_GOTO(&pArea->dbfarea.area, pTag->CurKey->rec);
    pArea->dbfarea.area.fBof = fOut;
  }
  hb_cdxIndexUnLockRead(pTag->pIndex);
  // Update Bof and Eof flags
#if 0
   if( fForward ) {
      pArea->dbfarea.area.fBof = false;
   } else {
      pArea->dbfarea.area.fEof = false;
   }
#endif
  return retval;
}

// ( DBENTRYP_VF )    hb_cdxAddField        : nullptr
// ( DBENTRYP_B )     hb_cdxAppend          : nullptr
// ( DBENTRYP_I )     hb_cdxCreateFields    : nullptr
// ( DBENTRYP_V )     hb_cdxDeleteRec       : nullptr
// ( DBENTRYP_BP )    hb_cdxDeleted         : nullptr
// ( DBENTRYP_SP )    hb_cdxFieldCount      : nullptr
// ( DBENTRYP_VF )    hb_cdxFieldDisplay    : nullptr
// ( DBENTRYP_SSI )   hb_cdxFieldInfo       : nullptr
// ( DBENTRYP_SCP )   hb_cdxFieldName       : nullptr

// ( DBENTRYP_V )     hb_cdxFlush           : nullptr
// Flush _system_ buffers to disk
static HB_ERRCODE hb_cdxFlush(CDXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxFlush(%p)", static_cast<void*>(pArea)));
#endif

  LPCDXINDEX pIndex;

  if (SELF_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  HB_ERRCODE errCode = SUPER_FLUSH(&pArea->dbfarea.area);

  if (hb_setGetHardCommit())
  {
    pIndex = pArea->lpIndexes;
    while (pIndex)
    {
      if (pIndex->pFile && pIndex->fFlush)
      {
        hb_fileCommit(pIndex->pFile);
        pIndex->fFlush = false;
      }
      pIndex = pIndex->pNext;
    }
  }

  return errCode;
}

// ( DBENTRYP_PP )    hb_cdxGetRec          : nullptr
// ( DBENTRYP_SI )    hb_cdxGetValue        : nullptr
// ( DBENTRYP_SVL )   hb_cdxGetVarLen       : nullptr

// ( DBENTRYP_V )     hb_cdxGoCold
// Perform a write of WorkArea memory to the data store.
static HB_ERRCODE hb_cdxGoCold(CDXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGoCold(%p)", static_cast<void*>(pArea)));
#endif

  bool fRecordChanged = pArea->dbfarea.fRecordChanged;
  bool fAppend = pArea->dbfarea.fAppend;

  if (SUPER_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  if ((fRecordChanged || pArea->fCdxAppend) && pArea->lpIndexes)
  {
    LPCDXTAG pTag = pArea->lpIndexes->TagList;
    LPCDXKEY pKey = nullptr;
    auto fAdd = false;
    auto fDel = false;
    auto fLck = false;
    LPDBRELINFO lpdbPendingRel;

    if (pArea->dbfarea.fShared)
    {
      if (fAppend)
      {
        if (pArea->fCdxAppend)
        {
          hb_cdxErrInternal("hb_cdxGoCold: multiple appending without GOCOLD.");
        }
        pArea->fCdxAppend = true;
        return Harbour::SUCCESS;
      }
      else
      {
        fAppend = pArea->fCdxAppend;
        pArea->fCdxAppend = false;
      }
    }

    // The pending relation may move the record pointer so we should
    // disable them for KEY/FOR evaluation
    lpdbPendingRel = pArea->dbfarea.lpdbPendingRel;
    pArea->dbfarea.lpdbPendingRel = nullptr;

    // TODO:
    // There is possible race condition here but not very dangerous.
    // To avoid it we should Lock all index file before SUPER_GOCOLD
    // but it makes other problem if two stations open the database index
    // files in a different order then they can block each other.
    // Without changes in locking scheme we can do only one thing which
    // is enough if there is only one index file: lock first index only
    // before SUPER_GOCOLD
    // Druzus, 2003-10-05 10:27:52 CEST

    while (pTag != nullptr)
    {
      if (!pTag->Custom)
      {
        pKey = hb_cdxKeyEval(pKey, pTag);

        if (pTag->pForItem != nullptr)
        {
          fAdd = hb_cdxEvalCond(pArea, pTag->pForItem, true);
        }
        else
        {
          fAdd = true;
        }

        if (fAppend)
        {
          fDel = false;
        }
        else
        {
          if (hb_cdxValCompare(pTag, pKey->val, pKey->len, pTag->HotKey->val, pTag->HotKey->len, CDX_CMP_EXACT) == 0)
          {
            fDel = !fAdd && pTag->HotFor;
            fAdd = fAdd && !pTag->HotFor;
          }
          else
          {
            fDel = pTag->HotFor;
          }
        }
        if (fDel || fAdd)
        {
          if (!fLck)
          {
            hb_cdxIndexLockWrite(pTag->pIndex);
            fLck = true;
          }
          if (fDel)
          {
            hb_cdxTagKeyDel(pTag, pTag->HotKey);
          }
          if (fAdd)
          {
            hb_cdxTagKeyAdd(pTag, pKey);
          }
        }
#if 0
            if( pTag->HotKey ) {
               hb_cdxKeyFree(pTag->HotKey);
               pTag->HotKey = nullptr;
            }
#endif
      }
      if (pTag->pNext)
      {
        pTag = pTag->pNext;
      }
      else
      {
        if (fLck)
        {
          hb_cdxIndexUnLockWrite(pTag->pIndex);
          fLck = false;
        }
        if (pTag->pIndex->pNext)
        {
          pTag = pTag->pIndex->pNext->TagList;
        }
        else
        {
          pTag = nullptr;
        }
      }
    }

    if (pKey)
    {
      hb_cdxKeyFree(pKey);
    }

    // Restore disabled pending relation
    pArea->dbfarea.lpdbPendingRel = lpdbPendingRel;
  }

  return Harbour::SUCCESS;
}

// ( DBENTRYP_V )     hb_cdxGoHot
// Mark the WorkArea data buffer as hot.
static HB_ERRCODE hb_cdxGoHot(CDXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGoHot(%p)", static_cast<void*>(pArea)));
#endif

  if (pArea->dbfarea.fRecordChanged)
  {
    hb_cdxErrInternal("hb_cdxGoHot: multiple marking buffer as hot.");
  }

  if (SUPER_GOHOT(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  if (pArea->lpIndexes && !pArea->fCdxAppend)
  {
    LPCDXTAG pTag = pArea->lpIndexes->TagList;
    while (pTag != nullptr)
    {
      if (!pTag->Custom)
      {
        pTag->HotKey = hb_cdxKeyEval(pTag->HotKey, pTag);
        pTag->HotFor = pTag->pForItem == nullptr || hb_cdxEvalCond(pArea, pTag->pForItem, true);
      }

      if (pTag->pNext)
      {
        pTag = pTag->pNext;
      }
      else
      {
        if (pTag->pIndex->pNext)
        {
          pTag = pTag->pIndex->pNext->TagList;
        }
        else
        {
          pTag = nullptr;
        }
      }
    }
  }
  return Harbour::SUCCESS;
}

// ( DBENTRYP_P )     hb_cdxPutRec          : nullptr
// ( DBENTRYP_SI )    hb_cdxPutValue        : nullptr
// ( DBENTRYP_V )     hb_cdxRecall          : nullptr
// ( DBENTRYP_ULP )   hb_cdxRecCount        : nullptr
// ( DBENTRYP_ISI )   hb_cdxRecInfo         : nullptr
// ( DBENTRYP_ULP )   hb_cdxRecNo           : nullptr
// ( DBENTRYP_I )     hb_cdxRecId           : nullptr
// ( DBENTRYP_S )     hb_cdxSetFieldExtent  : nullptr
// ( DBENTRYP_CP )    hb_cdxAlias           : nullptr

// ( DBENTRYP_V )     hb_cdxClose
// Close the table in the WorkArea.
static HB_ERRCODE hb_cdxClose(CDXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxClose(%p)", static_cast<void*>(pArea)));
#endif

  if (SELF_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }
  HB_ERRCODE errCode = SUPER_CLOSE(&pArea->dbfarea.area);

  if (errCode == Harbour::SUCCESS)
  {
    if (pArea->pSort)
    {
      hb_cdxSortFree(pArea->pSort);
      pArea->pSort = nullptr;
    }

    hb_cdxOrdListClear(pArea, true, nullptr);
#ifdef HB_CDX_DBGTIME
    fprintf(stderr,
            "\r\ncdxTimeIntBld=%f, cdxTimeExtBld=%f, cdxTimeBld=%f\r\n"
            "cdxTimeGetKey=%f, cdxTimeFreeKey=%f\r\n"
            "cdxTimeExtBlc=%f, cdxTimeIntBlc=%f\r\n"
            "cdxTimeIdxBld=%f\r\n"
            "cdxTimeTotal=%f\r\n",
            static_cast<double>(cdxTimeIntBld) / 1000000, static_cast<double>(cdxTimeExtBld) / 1000000,
            static_cast<double>(cdxTimeIntBld + cdxTimeExtBld) / 1000000, static_cast<double>(cdxTimeGetKey) / 1000000,
            static_cast<double>(cdxTimeFreeKey) / 1000000, static_cast<double>(cdxTimeIntBlc) / 1000000,
            static_cast<double>(cdxTimeExtBlc) / 1000000, static_cast<double>(cdxTimeIdxBld) / 1000000,
            static_cast<double>(cdxTimeIntBld + cdxTimeExtBld + cdxTimeIdxBld + cdxTimeGetKey + cdxTimeFreeKey +
                                cdxTimeExtBlc + cdxTimeIntBlc) /
                1000000);
    fflush(stderr);
    cdxTimeIntBld = cdxTimeExtBld = 0;
#endif
#ifdef HB_CDX_DBGUPDT
    fprintf(stderr, "\r\n#reads=%ld, #writes=%ld, stacksize=%d\r\n", cdxReadNO, cdxWriteNO, cdxStackSize);
    fflush(stderr);
    cdxReadNO = cdxWriteNO = 0;
#endif
  }

  return errCode;
}

// ( DBENTRYP_VO )    hb_cdxCreate          : nullptr
// ( DBENTRYP_SI )    hb_cdxInfo            : nullptr
// ( DBENTRYP_V )     hb_cdxNewArea         : nullptr

// ( DBENTRYP_VO )    hb_cdxOpen
// Open a data store in the WorkArea.
static HB_ERRCODE hb_cdxOpen(CDXAREAP pArea, LPDBOPENINFO pOpenInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOpen(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOpenInfo)));
#endif

  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (!pArea->dbfarea.bLockType)
  {
    auto pItem = hb_itemNew(nullptr);
    if (SELF_INFO(&pArea->dbfarea.area, DBI_LOCKSCHEME, pItem) != Harbour::SUCCESS)
    {
      hb_itemRelease(pItem);
      return Harbour::FAILURE;
    }
    pArea->dbfarea.bLockType = static_cast<HB_BYTE>(pItem->getNI());
    hb_itemRelease(pItem);
    if (pArea->dbfarea.bLockType == 0)
    {
      pArea->dbfarea.bLockType = DB_DBFLOCK_VFP;
    }
  }
  if (SUPER_OPEN(&pArea->dbfarea.area, pOpenInfo) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  // open (production) structural index
  if (DBFAREA_DATA(&pArea->dbfarea)->fStrictStruct ? pArea->dbfarea.fHasTags : hb_setGetAutOpen())
  {
    char szFileName[HB_PATH_MAX];

    pArea->dbfarea.fHasTags = false;
    hb_cdxCreateFName(pArea, nullptr, nullptr, szFileName, nullptr);
    // CL5.3/COMIX CDX RDDs looks for production indexes
    // only in the directory where DBF file is located but
    // CL5.2/SIXCDX RDDs also respects SET PATH [druzus]
    if (DBFAREA_DATA(&pArea->dbfarea)->fStrictStruct ||
#if defined(HB_SIXCDX)
        hb_fileExists(szFileName, szFileName))
#else
        hb_fileExists(szFileName, nullptr))
#endif
    {
      DBORDERINFO pOrderInfo;

      pOrderInfo.itmResult = hb_itemPutNI(nullptr, 0);
      pOrderInfo.atomBagName = hb_itemPutC(nullptr, szFileName);
      pOrderInfo.itmNewVal = nullptr;
      pOrderInfo.itmOrder = nullptr;
      errCode = SELF_ORDLSTADD(&pArea->dbfarea.area, &pOrderInfo);
      if (errCode == Harbour::SUCCESS)
      {
        pOrderInfo.itmOrder = hb_itemPutNI(nullptr, hb_setGetAutOrder());
        errCode = SELF_ORDLSTFOCUS(&pArea->dbfarea.area, &pOrderInfo);
        hb_itemRelease(pOrderInfo.itmOrder);
        if (errCode == Harbour::SUCCESS)
        {
          errCode = SELF_GOTOP(&pArea->dbfarea.area);
        }
      }
      hb_itemRelease(pOrderInfo.atomBagName);
      hb_itemRelease(pOrderInfo.itmResult);
    }
  }
  else
  {
    pArea->dbfarea.fHasTags = false;
  }

  return errCode;
}

// ( DBENTRYP_V )     hb_cdxRelease         : nullptr

// ( DBENTRYP_SP )    hb_cdxStructSize
// Retrieve the size of the WorkArea structure.
static HB_ERRCODE hb_cdxStructSize(CDXAREAP pArea, HB_USHORT *uiSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxStrucSize(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(uiSize)));
#endif
  HB_SYMBOL_UNUSED(pArea);

  *uiSize = sizeof(CDXAREA);
  return Harbour::SUCCESS;
}

// ( DBENTRYP_CP )    hb_cdxSysName         : nullptr

// ( DBENTRYP_VEI )   hb_cdxEval            : nullptr

// ( DBENTRYP_V )     hb_cdxPack
static HB_ERRCODE hb_cdxPack(CDXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxPack(%p)", static_cast<void*>(pArea)));
#endif

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  if (SUPER_PACK(&pArea->dbfarea.area) == Harbour::SUCCESS)
  {
    return SELF_ORDLSTREBUILD(&pArea->dbfarea.area);
  }
  else
  {
    return Harbour::FAILURE;
  }
}

// ( DBENTRYP_LSP )   hb_cdxPackRec         : nullptr
// ( DBENTRYP_VS )    hb_cdxSort            : nullptr
// ( DBENTRYP_VT )    hb_cdxTrans           : nullptr
// ( DBENTRYP_VT )    hb_cdxTransRec        : nullptr

// ( DBENTRYP_V )     hb_cdxZap
static HB_ERRCODE hb_cdxZap(CDXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("nb_cdxZap(%p)", static_cast<void*>(pArea)));
#endif

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  if (SUPER_ZAP(&pArea->dbfarea.area) == Harbour::SUCCESS)
  {
    return SELF_ORDLSTREBUILD(&pArea->dbfarea.area);
  }
  else
  {
    return Harbour::FAILURE;
  }
}

// ( DBENTRYP_VR )    hb_cdxChildEnd        : nullptr
// ( DBENTRYP_VR )    hb_cdxChildStart      : nullptr
// ( DBENTRYP_VR )    hb_cdxChildSync       : nullptr
// ( DBENTRYP_V )     hb_cdxSyncChildren    : nullptr
// ( DBENTRYP_V )     hb_cdxClearRel        : nullptr
// ( DBENTRYP_V )     hb_cdxForceRel        : nullptr
// ( DBENTRYP_SSP )   hb_cdxRelArea         : nullptr
// ( DBENTRYP_VR )    hb_cdxRelEval         : nullptr
// ( DBENTRYP_SI )    hb_cdxRelText         : nullptr
// ( DBENTRYP_VR )    hb_cdxSetRel          : nullptr

// ( DBENTRYP_VOI )   hb_cdxOrderListAdd
static HB_ERRCODE hb_cdxOrderListAdd(CDXAREAP pArea, LPDBORDERINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOrderListAdd(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOrderInfo)));
#endif

  HB_FATTR nFlags;
  PHB_FILE pFile;
  char szBaseName[CDX_MAXTAGNAMELEN + 1];
  char szFileName[HB_PATH_MAX];
  LPCDXINDEX pIndex, *pIndexPtr;
  auto fProd = false;
  auto bRetry = false;
  PHB_ITEM pError = nullptr;

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  if (hb_itemGetCLen(pOrderInfo->atomBagName) == 0)
  {
    return Harbour::FAILURE;
  }

  hb_cdxCreateFName(pArea, hb_itemGetCPtr(pOrderInfo->atomBagName), &fProd, szFileName, szBaseName);

#if 0
   if( !szBaseName[0] ) {
      return Harbour::FAILURE;
   }
#endif
  pIndex = hb_cdxFindBag(pArea, szFileName);

  if (pIndex)
  {
    // index already open, do nothing
    if (!pArea->uiTag)
    {
      pArea->uiTag = hb_cdxGetTagNumber(pArea, pIndex->TagList);
      SELF_GOTOP(&pArea->dbfarea.area);
    }
    return Harbour::SUCCESS;
  }

  nFlags = (pArea->dbfarea.fReadonly ? FO_READ : FO_READWRITE) | (pArea->dbfarea.fShared ? FO_DENYNONE : FO_EXCLUSIVE) |
           FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME | FXO_NOSEEKPOS;
  do
  {
    pFile = hb_fileExtOpen(szFileName, nullptr, nFlags, nullptr, pError);
    if (!pFile)
    {
      bRetry = hb_cdxErrorRT(pArea, EG_OPEN, EDBF_OPEN_INDEX, szFileName, hb_fsError(), EF_CANRETRY | EF_CANDEFAULT,
                             &pError) == E_RETRY;
    }
    else
    {
      if (hb_fileSize(pFile) <= static_cast<HB_FOFFSET>(sizeof(CDXTAGHEADER)))
      {
        hb_fileClose(pFile);
        pFile = nullptr;
        hb_cdxErrorRT(pArea, EG_CORRUPTION, EDBF_CORRUPT, szFileName, hb_fsError(), EF_CANDEFAULT, nullptr);
      }
      bRetry = false;
    }

  } while (bRetry);

  if (pError)
  {
    hb_errRelease(pError);
  }

  if (!pFile)
  {
    return Harbour::FAILURE;
  }

  pIndex = hb_cdxIndexNew(pArea);
  pIndex->pFile = pFile;
  pIndex->fShared = pArea->dbfarea.fShared;
  pIndex->fReadonly = pArea->dbfarea.fReadonly;
  pIndex->szFileName = hb_strdup(szFileName);

  pIndexPtr = &pArea->lpIndexes;
  while (*pIndexPtr != nullptr)
  {
    pIndexPtr = &(*pIndexPtr)->pNext;
  }
  *pIndexPtr = pIndex;

  if (!hb_cdxIndexLoad(pIndex, szBaseName))
  {
    // index file is corrupted
    *pIndexPtr = nullptr;
    hb_cdxIndexFree(pIndex);
    hb_cdxErrorRT(pArea, EG_CORRUPTION, EDBF_CORRUPT, szFileName, hb_fsError(), EF_CANDEFAULT, nullptr);
    return Harbour::FAILURE;
  }

  if (fProd)
  {
    pArea->dbfarea.fHasTags = true;
  }

  // dbfcdx specific: If there was no controlling order, set this one.
  // This is the behaviour of Clipper's dbfcdx, although
  // Clipper doc says a different rule
  if (!pArea->uiTag)
  {
    pArea->uiTag = hb_cdxGetTagNumber(pArea, pIndex->TagList);
    SELF_GOTOP(&pArea->dbfarea.area);
  }
  return Harbour::SUCCESS;
}

// ( DBENTRYP_V )     hb_cdxOrderListClear
// Clear the current order list.
static HB_ERRCODE hb_cdxOrderListClear(CDXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOrderListClear(%p)", static_cast<void*>(pArea)));
#endif

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  hb_cdxOrdListClear(
      pArea, !(DBFAREA_DATA(&pArea->dbfarea)->fStrictStruct ? pArea->dbfarea.fHasTags : hb_setGetAutOpen()), nullptr);
  pArea->uiTag = 0;

  return Harbour::SUCCESS;
}

// ( DBENTRYP_VOI )   hb_cdxOrderListDelete
static HB_ERRCODE hb_cdxOrderListDelete(CDXAREAP pArea, LPDBORDERINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOrderListDelete(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOrderInfo)));
#endif

  char szTagName[CDX_MAXTAGNAMELEN + 1];
  char szFileName[HB_PATH_MAX];
  LPCDXINDEX pIndex, *pIndexPtr;
  auto fProd = false;

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  hb_cdxCreateFName(pArea, hb_itemGetCPtr(pOrderInfo->atomBagName), &fProd, szFileName, szTagName);

  if (fProd && (DBFAREA_DATA(&pArea->dbfarea)->fStrictStruct ? pArea->dbfarea.fHasTags : hb_setGetAutOpen()))
  {
    pIndex = nullptr;
  }
  else
  {
    pIndex = hb_cdxFindBag(pArea, szFileName);
  }

  if (pIndex)
  {
    LPCDXTAG pTag = hb_cdxGetActiveTag(pArea);
    if (pTag && pTag->pIndex == pIndex)
    {
      pArea->uiTag = 0;
    }
    pIndexPtr = &pArea->lpIndexes;
    while (*pIndexPtr)
    {
      if (pIndex == *pIndexPtr)
      {
        *pIndexPtr = pIndex->pNext;
        hb_cdxIndexFree(pIndex);
        break;
      }
      pIndexPtr = &(*pIndexPtr)->pNext;
    }
  }
  return Harbour::SUCCESS;
}

// ( DBENTRYP_VOI )   hb_cdxOrderListFocus
static HB_ERRCODE hb_cdxOrderListFocus(CDXAREAP pArea, LPDBORDERINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOrderListFocus(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOrderInfo)));
#endif

  LPCDXTAG pTag;

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  if (!pArea->lpIndexes)
  {
    return Harbour::SUCCESS;
  }

  pTag = hb_cdxGetActiveTag(pArea);
  if (pTag != nullptr)
  {
    pOrderInfo->itmResult = hb_itemPutC(pOrderInfo->itmResult, pTag->szName);
  }

  if (pOrderInfo->itmOrder)
  {
    hb_cdxFindTag(pArea, pOrderInfo->itmOrder, pOrderInfo->atomBagName, &pArea->uiTag);
  }

  return Harbour::SUCCESS;
}

// ( DBENTRYP_V )     hb_cdxOrderListRebuild
static HB_ERRCODE hb_cdxOrderListRebuild(CDXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxPack(%p)", static_cast<void*>(pArea)));
#endif

  LPCDXINDEX pIndex, *pIndexPtr;
  HB_USHORT uiPrevTag;

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  if (pArea->dbfarea.fShared)
  {
    hb_cdxErrorRT(pArea, EG_SHARED, EDBF_SHARED, pArea->dbfarea.szDataFileName, 0, 0, nullptr);
    return Harbour::FAILURE;
  }
  if (pArea->dbfarea.fReadonly)
  {
    hb_cdxErrorRT(pArea, EG_READONLY, EDBF_READONLY, pArea->dbfarea.szDataFileName, 0, 0, nullptr);
    return Harbour::FAILURE;
  }

  if (!pArea->lpIndexes)
  {
    return Harbour::SUCCESS;
  }

  uiPrevTag = pArea->uiTag;
  pArea->uiTag = 0;

  pIndex = pArea->lpIndexes;
  pArea->lpIndexes = nullptr;
  pIndexPtr = &pArea->lpIndexes;
  while (pIndex)
  {
    (*pIndexPtr) = pIndex;
    pIndex = pIndex->pNext;
    (*pIndexPtr)->pNext = nullptr;
    hb_cdxIndexReindex(*pIndexPtr);
    pIndexPtr = &(*pIndexPtr)->pNext;
  }

  pArea->uiTag = uiPrevTag;
  // Clear pArea->dbfarea.area.lpdbOrdCondInfo
  SELF_ORDSETCOND(&pArea->dbfarea.area, nullptr);

  return SELF_GOTOP(&pArea->dbfarea.area);
}

// ( DBENTRYP_VOO )   hb_cdxOrderCondition  : nullptr

// ( DBENTRYP_VOC )   hb_cdxOrderCreate
// create new order
static HB_ERRCODE hb_cdxOrderCreate(CDXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOrderCreate(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOrderInfo)));
#endif

  HB_ULONG ulRecNo;
  auto fProd = false;
  auto fNewFile = false;
  auto fOpenedIndex = false;
  auto fAscend = true;
  auto fNoCase = false;
  auto fCustom = false;
  auto fTemporary = false;
  auto fExclusive = false;
  PHB_ITEM pKeyExp, pForExp = nullptr, pResult;
  char szCpndTagName[CDX_MAXTAGNAMELEN + 1], szTagName[CDX_MAXTAGNAMELEN + 1];
  char szFileName[HB_PATH_MAX];
  const char *szFor = nullptr;
  LPCDXINDEX pIndex;
  LPCDXTAG pTag;
  HB_USHORT uiLen;
  HB_BYTE bType;

  if (SELF_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  if (pArea->dbfarea.lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  if (hb_strlentrim(hb_itemGetCPtr(pOrderInfo->abExpr)) +
          (pArea->dbfarea.area.lpdbOrdCondInfo && pArea->dbfarea.area.lpdbOrdCondInfo->abFor
               ? hb_strlentrim(pArea->dbfarea.area.lpdbOrdCondInfo->abFor)
               : 0) >
      CDX_HEADEREXPLEN - 2)
  {
    hb_cdxErrorRT(pArea, EG_DATAWIDTH, EDBF_KEYLENGTH, nullptr, 0, 0, nullptr);
    return Harbour::FAILURE;
  }

  if (SELF_COMPILE(&pArea->dbfarea.area, hb_itemGetCPtr(pOrderInfo->abExpr)) == Harbour::FAILURE)
  {
    if (pOrderInfo->itmCobExpr)
    {
      pKeyExp = hb_itemNew(pOrderInfo->itmCobExpr);
    }
    else
    {
      hb_cdxErrorRT(pArea, EG_DATATYPE, EDBF_INVALIDKEY, nullptr, 0, 0, nullptr);
      return Harbour::FAILURE;
    }
  }
  else
  {
    pKeyExp = pArea->dbfarea.area.valResult;
    pArea->dbfarea.area.valResult = nullptr;
    // If we have a codeblock for the expression, use it
    if (pOrderInfo->itmCobExpr)
    {
      hb_vmDestroyBlockOrMacro(pKeyExp);
      pKeyExp = hb_itemNew(pOrderInfo->itmCobExpr);
    }
  }

  // Get a blank record before testing expression
  ulRecNo = pArea->dbfarea.ulRecNo;
  SELF_GOTO(&pArea->dbfarea.area, 0);
  if (SELF_EVALBLOCK(&pArea->dbfarea.area, pKeyExp) == Harbour::FAILURE)
  {
    hb_vmDestroyBlockOrMacro(pKeyExp);
    SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
    return Harbour::FAILURE;
  }
  pResult = pArea->dbfarea.area.valResult;
  pArea->dbfarea.area.valResult = nullptr;

  bType = hb_cdxItemType(pResult);
  switch (bType)
  {
  case 'N':
  case 'D':
  case 'T':
    uiLen = 8;
    break;
  case 'L':
    uiLen = 1;
    break;
  case 'C':
  {
    auto nLen = pResult->getCLen();
    if (nLen > USHRT_MAX)
    {
      nLen = USHRT_MAX;
    }
    uiLen = static_cast<HB_USHORT>(nLen);
    break;
  }
  default:
    bType = 'U';
    uiLen = 0;
  }
  hb_itemRelease(pResult);

  // Make sure KEY has proper type and length
  if (bType == 'U' || uiLen == 0)
  {
    hb_vmDestroyBlockOrMacro(pKeyExp);
    SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
    hb_cdxErrorRT(pArea, bType == 'U' ? EG_DATATYPE : EG_DATAWIDTH, EDBF_INVALIDKEY, nullptr, 0, 0, nullptr);
    return Harbour::FAILURE;
  }
#if defined(HB_COMPAT_C53) && defined(HB_CLP_STRICT)
  else if (bType == 'C' && uiLen > CDX_MAXKEY)
  {
    if (hb_cdxErrorRT(pArea, EG_DATAWIDTH, EDBF_INVALIDKEY, nullptr, 0, EF_CANDEFAULT, nullptr) == E_DEFAULT)
    {
      uiLen = CDX_MAXKEY;
    }
    else
    {
      hb_vmDestroyBlockOrMacro(pKeyExp);
      SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
      return Harbour::FAILURE;
    }
  }
#endif
  if (pArea->dbfarea.area.lpdbOrdCondInfo)
  {
    fAscend = !pArea->dbfarea.area.lpdbOrdCondInfo->fDescending;
    fCustom = pArea->dbfarea.area.lpdbOrdCondInfo->fCustom;
    fTemporary = pArea->dbfarea.area.lpdbOrdCondInfo->fTemporary;
    fExclusive = pArea->dbfarea.area.lpdbOrdCondInfo->fExclusive;

    // Check conditional expression
    szFor = pArea->dbfarea.area.lpdbOrdCondInfo->abFor;
    if (szFor != nullptr)
    {
      if (SELF_COMPILE(&pArea->dbfarea.area, szFor) == Harbour::FAILURE)
      {
        hb_vmDestroyBlockOrMacro(pKeyExp);
        SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
        hb_cdxErrorRT(pArea, EG_DATATYPE, EDBF_INVALIDFOR, nullptr, 0, 0, nullptr);
        return Harbour::FAILURE;
      }
      pForExp = pArea->dbfarea.area.valResult;
      pArea->dbfarea.area.valResult = nullptr;
    }
    // If we have a codeblock for the conditional expression, use it
    if (pArea->dbfarea.area.lpdbOrdCondInfo->itmCobFor)
    {
      if (pForExp)
      {
        hb_vmDestroyBlockOrMacro(pForExp);
      }
      pForExp = hb_itemNew(pArea->dbfarea.area.lpdbOrdCondInfo->itmCobFor);
    }
  }

  if (pArea->dbfarea.fTemporary)
  {
    fTemporary = true;
  }

  // Test conditional expression
  if (pForExp)
  {
    auto fOK = false;

    if (SELF_EVALBLOCK(&pArea->dbfarea.area, pForExp) == Harbour::FAILURE)
    {
      hb_vmDestroyBlockOrMacro(pKeyExp);
      hb_vmDestroyBlockOrMacro(pForExp);
      SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
      return Harbour::FAILURE;
    }
    fOK = hb_itemType(pArea->dbfarea.area.valResult) & Harbour::Item::LOGICAL;
    hb_itemRelease(pArea->dbfarea.area.valResult);
    pArea->dbfarea.area.valResult = nullptr;
    if (!fOK)
    {
      hb_vmDestroyBlockOrMacro(pKeyExp);
      hb_vmDestroyBlockOrMacro(pForExp);
      SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
      hb_cdxErrorRT(pArea, EG_DATATYPE, EDBF_INVALIDFOR, nullptr, 0, 0, nullptr);
      return Harbour::FAILURE;
    }
  }

  SELF_GOTO(&pArea->dbfarea.area, ulRecNo);

  // abBagName -> cBag, atomBagName -> cTag
  // The following scheme implemented:
  // 1. abBagName == nullptr   -> add the Tag to the structural index
  // 2. atomBagName == nullptr -> overwrite any index file of abBagName
  // 3. add the Tag to index file

  hb_cdxCreateFName(pArea, pOrderInfo->abBagName, &fProd, szFileName, szCpndTagName);

  if (pOrderInfo->atomBagName && pOrderInfo->atomBagName[0])
  {
    hb_strncpyUpperTrim(szTagName, pOrderInfo->atomBagName, sizeof(szTagName) - 1);
    fNewFile = false;
  }
  else
  {
    hb_strncpy(szTagName, szCpndTagName, sizeof(szTagName) - 1);
    fNewFile = true;
  }

  if (!pArea->dbfarea.area.lpdbOrdCondInfo ||
      (pArea->dbfarea.area.lpdbOrdCondInfo->fAll && !pArea->dbfarea.area.lpdbOrdCondInfo->fAdditive))
  {
    hb_cdxOrdListClear(
        pArea, !(DBFAREA_DATA(&pArea->dbfarea)->fStrictStruct ? pArea->dbfarea.fHasTags : hb_setGetAutOpen()), nullptr);
  }

  pIndex = hb_cdxFindBag(pArea, szFileName);

  if (fNewFile && pIndex != nullptr)
  {
    LPCDXINDEX *pIndexPtr = &pArea->lpIndexes;
    while (*pIndexPtr)
    {
      if (pIndex == *pIndexPtr)
      {
        *pIndexPtr = pIndex->pNext;
        break;
      }
      pIndexPtr = &(*pIndexPtr)->pNext;
    }
    hb_cdxIndexFree(pIndex);
    pIndex = nullptr;
  }
  fOpenedIndex = (pIndex != nullptr);

  if (!fOpenedIndex)
  {
    char szTempFile[HB_PATH_MAX];
    PHB_FILE pFile;
    auto bRetry = false;
    bool fShared = pArea->dbfarea.fShared && !fTemporary && !fExclusive;
    PHB_ITEM pError = nullptr;

    do
    {
      if (fTemporary)
      {
        pFile = hb_fileCreateTemp(nullptr, nullptr, FC_NORMAL, szTempFile);
        fNewFile = true;
      }
      else
      {
        pFile = hb_fileExtOpen(szFileName, nullptr,
                               FO_READWRITE | (fShared ? FO_DENYNONE : FO_EXCLUSIVE) |
                                   (fNewFile ? FXO_TRUNCATE : FXO_APPEND) | FXO_DEFAULTS | FXO_SHARELOCK |
                                   FXO_COPYNAME | FXO_NOSEEKPOS,
                               nullptr, pError);
      }
      if (!pFile)
      {
        bRetry = hb_cdxErrorRT(pArea, EG_CREATE, EDBF_CREATE_INDEX, szFileName, hb_fsError(),
                               EF_CANRETRY | EF_CANDEFAULT, &pError) == E_RETRY;
      }
      else
      {
        bRetry = false;
        if (!fNewFile)
        {
          fNewFile = (hb_fileSize(pFile) == 0);
        }
      }
    } while (bRetry);

    if (pError)
    {
      hb_errRelease(pError);
    }

    if (pFile)
    {
      pIndex = hb_cdxIndexNew(pArea);
      pIndex->pFile = pFile;
      pIndex->fShared = fShared;
      pIndex->fReadonly = false;
      pIndex->szFileName = hb_strdup(szFileName);
      pIndex->fDelete = fTemporary;
      if (fTemporary)
      {
        pIndex->szRealName = hb_strdup(szTempFile);
      }

      if (!fNewFile)
      {
        // index file is corrupted?
        if (!hb_cdxIndexLoad(pIndex, szCpndTagName))
        {
          // TODO: What should be default?
#if 0
               hb_cdxIndexFree(pIndex);
               hb_fileClose(pFile);
               pFile = nullptr;
               hb_cdxErrorRT(pArea, EG_CORRUPTION, EDBF_CORRUPT, szFileName, hb_fsError(), EF_CANDEFAULT, nullptr);
#endif
          hb_cdxIndexFreeTags(pIndex);
          fNewFile = true;
        }
      }
    }

    if (!pFile)
    {
      hb_vmDestroyBlockOrMacro(pKeyExp);
      if (pForExp != nullptr)
      {
        hb_vmDestroyBlockOrMacro(pForExp);
      }
      return Harbour::FAILURE;
    }
  }
  else if (pIndex->fReadonly)
  {
    hb_cdxErrorRT(pArea, EG_READONLY, EDBF_READONLY, pIndex->szFileName, 0, 0, nullptr);
    return Harbour::FAILURE;
  }

  hb_cdxIndexLockWrite(pIndex);
  hb_cdxIndexLockFlush(pIndex);
  if (!fNewFile)
  {
    pTag = hb_cdxGetTagByNumber(pArea, pArea->uiTag);
    // Delete new tag if exist
    hb_cdxIndexDelTag(pIndex, szTagName);
    pArea->uiTag = hb_cdxGetTagNumber(pArea, pTag);
    fNewFile = (pIndex->TagList == nullptr);
  }

  if (fNewFile)
  {
    hb_fileTruncAt(pIndex->pFile, 0);
    pIndex->fChanged = true;
    hb_cdxIndexDropAvailPage(pIndex);
    if (pIndex->pCompound != nullptr)
    {
      hb_cdxTagFree(pIndex->pCompound);
    }
    hb_cdxIndexInit(pIndex);
    pIndex->nextAvail = pIndex->freePage = 0;
    hb_cdxIndexCreateStruct(pIndex, szCpndTagName);
  }

  if (uiLen > pIndex->uiMaxKeyLen)
  {
    uiLen = pIndex->uiMaxKeyLen;
  }

  pTag = hb_cdxIndexAddTag(pIndex, szTagName, hb_itemGetCPtr(pOrderInfo->abExpr), pKeyExp, bType, uiLen, szFor, pForExp,
                           fAscend, pOrderInfo->fUnique, fNoCase, fCustom, false);

  if (pArea->dbfarea.area.lpdbOrdCondInfo &&
      (!pArea->dbfarea.area.lpdbOrdCondInfo->fAll && !pArea->dbfarea.area.lpdbOrdCondInfo->fAdditive))
  {
    hb_cdxOrdListClear(
        pArea, !(DBFAREA_DATA(&pArea->dbfarea)->fStrictStruct ? pArea->dbfarea.fHasTags : hb_setGetAutOpen()), pIndex);
  }
  hb_cdxIndexUnLockWrite(pIndex);
  // Update DBF header
  if (!pArea->dbfarea.fHasTags && !fOpenedIndex && !pIndex->fDelete && fProd)
  {
    pArea->dbfarea.fHasTags = true;
    if (!pArea->dbfarea.fReadonly && (pArea->dbfarea.dbfHeader.bHasTags & 0x01) == 0 &&
        (hb_setGetAutOpen() || DBFAREA_DATA(&pArea->dbfarea)->fStrictStruct))
    {
      SELF_WRITEDBHEADER(&pArea->dbfarea.area);
    }
  }
  else
  {
    fProd = false;
  }

  if (!fOpenedIndex)
  {
    if (fProd || pArea->lpIndexes == nullptr)
    {
      pIndex->pNext = pArea->lpIndexes;
      pArea->lpIndexes = pIndex;
    }
    else
    {
      LPCDXINDEX pIndexTmp = pArea->lpIndexes;
      while (pIndexTmp->pNext)
      {
        pIndexTmp = pIndexTmp->pNext;
      }
      pIndexTmp->pNext = pIndex;
    }
  }

  pArea->uiTag = hb_cdxGetTagNumber(pArea, pTag);

  // Clear pArea->dbfarea.area.lpdbOrdCondInfo
  SELF_ORDSETCOND(&pArea->dbfarea.area, nullptr);

  return SELF_GOTOP(&pArea->dbfarea.area);
}

// ( DBENTRYP_VOI )   hb_cdxOrderDestroy
static HB_ERRCODE hb_cdxOrderDestroy(CDXAREAP pArea, LPDBORDERINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOrderDestroy(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOrderInfo)));
#endif

  LPCDXINDEX pIndex, pIndexTmp;
  LPCDXTAG pTag;
  HB_USHORT uiTag;

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  if (!pArea->lpIndexes)
  {
    return Harbour::SUCCESS;
  }

  if (pOrderInfo->itmOrder)
  {
    pTag = hb_cdxFindTag(pArea, pOrderInfo->itmOrder, pOrderInfo->atomBagName, &uiTag);
    if (pTag != nullptr)
    {
      pIndex = pTag->pIndex;
      if (/* !pIndex->fShared && */ !pIndex->fReadonly)
      {
        hb_cdxIndexLockWrite(pIndex);
        hb_cdxIndexDelTag(pIndex, pTag->szName);
        hb_cdxIndexUnLockWrite(pIndex);
        if (!pIndex->TagList)
        {
          if (pArea->lpIndexes == pIndex)
          {
            pArea->lpIndexes = pIndex->pNext;
            if (pArea->dbfarea.fHasTags)
            {
              pArea->dbfarea.fHasTags = false;
              if (!pArea->dbfarea.fReadonly && (pArea->dbfarea.dbfHeader.bHasTags & 0x01) != 0 &&
                  (hb_setGetAutOpen() || DBFAREA_DATA(&pArea->dbfarea)->fStrictStruct))
              {
                SELF_WRITEDBHEADER(&pArea->dbfarea.area);
              }
            }
          }
          else
          {
            pIndexTmp = pArea->lpIndexes;
            while (pIndexTmp->pNext && (pIndexTmp->pNext != pIndex))
            {
              pIndexTmp = pIndexTmp->pNext;
            }
            if (pIndexTmp->pNext == pIndex)
            {
              pIndexTmp->pNext = pIndex->pNext;
            }
          }
          pIndex->fDelete = true;
          hb_cdxIndexFree(pIndex);
        }
        if (uiTag < pArea->uiTag)
        {
          pArea->uiTag--;
        }
        else if (uiTag == pArea->uiTag)
        {
          pArea->uiTag = 0;
        }
      }
      else
      {
        // TODO: allow this operation for shared mode?
        hb_errInternal(1023, "hb_cdxOrderDestroy: exclusive required.", nullptr, nullptr);
      }
    }
  }
  return Harbour::SUCCESS;
}

// ( DBENTRYP_SVOI )  hb_cdxOrderInfo
// Provides information about order management.
static HB_ERRCODE hb_cdxOrderInfo(CDXAREAP pArea, HB_USHORT uiIndex, LPDBORDERINFO pInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOrderInfo(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pInfo)));
#endif

  LPCDXTAG pTag;
  HB_USHORT uiTag = 0;

  switch (uiIndex)
  {
  case DBOI_STRICTREAD:
    pInfo->itmResult = hb_itemPutNil(pInfo->itmResult);
    return SELF_RDDINFO(SELF_RDDNODE(&pArea->dbfarea.area), RDDI_STRICTREAD, 0, pInfo->itmResult);

  case DBOI_OPTIMIZE:
    pInfo->itmResult = hb_itemPutNil(pInfo->itmResult);
    return SELF_RDDINFO(SELF_RDDNODE(&pArea->dbfarea.area), RDDI_OPTIMIZE, 0, pInfo->itmResult);

  case DBOI_AUTOOPEN:
    pInfo->itmResult = hb_itemPutNil(pInfo->itmResult);
    return SELF_RDDINFO(SELF_RDDNODE(&pArea->dbfarea.area), RDDI_AUTOOPEN, 0, pInfo->itmResult);

  case DBOI_AUTOORDER:
    pInfo->itmResult = hb_itemPutNil(pInfo->itmResult);
    return SELF_RDDINFO(SELF_RDDNODE(&pArea->dbfarea.area), RDDI_AUTOORDER, 0, pInfo->itmResult);

  case DBOI_AUTOSHARE:
    pInfo->itmResult = hb_itemPutNil(pInfo->itmResult);
    return SELF_RDDINFO(SELF_RDDNODE(&pArea->dbfarea.area), RDDI_AUTOSHARE, 0, pInfo->itmResult);

  case DBOI_BAGEXT:
    pInfo->itmResult = hb_itemPutNil(pInfo->itmResult);
    return SELF_RDDINFO(SELF_RDDNODE(&pArea->dbfarea.area), RDDI_ORDBAGEXT, 0, pInfo->itmResult);

  case DBOI_EVALSTEP:
    pInfo->itmResult = hb_itemPutNL(
        pInfo->itmResult, pArea->dbfarea.area.lpdbOrdCondInfo ? pArea->dbfarea.area.lpdbOrdCondInfo->lStep : 0);
    return Harbour::SUCCESS;

  case DBOI_KEYSINCLUDED:
    pInfo->itmResult = hb_itemPutNL(pInfo->itmResult, pArea->pSort ? pArea->pSort->ulTotKeys : 0);
    return Harbour::SUCCESS;

  case DBOI_I_TAGNAME:
    pInfo->itmResult = hb_itemPutC(pInfo->itmResult, pArea->pSort ? pArea->pSort->pTag->szName : nullptr);
    return Harbour::SUCCESS;

  case DBOI_I_BAGNAME:
    pInfo->itmResult = hb_itemPutC(pInfo->itmResult, pArea->pSort ? pArea->pSort->pTag->pIndex->szFileName : nullptr);
    return Harbour::SUCCESS;

  case DBOI_ISREINDEX:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pArea->pSort ? pArea->pSort->fReindex : false);
    return Harbour::SUCCESS;

  case DBOI_LOCKOFFSET:
  case DBOI_HPLOCKING:
  {
    HB_DBFLOCKDATA lockData;

    hb_dbfLockIdxGetData(pArea->dbfarea.bLockType, &lockData);
    if (uiIndex == DBOI_LOCKOFFSET)
    {
      pInfo->itmResult = hb_itemPutNInt(pInfo->itmResult, lockData.offset);
    }
    else
    {
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, lockData.size > 0);
    }
    return Harbour::SUCCESS;
  }

  case DBOI_ORDERCOUNT:
  {
    LPCDXINDEX pIndex;
    const char *pszBag = hb_itemGetCLen(pInfo->atomBagName) > 0 ? pInfo->atomBagName->getCPtr() : nullptr;
    pIndex = pszBag ? hb_cdxFindBag(pArea, pszBag) : pArea->lpIndexes;
    while (pIndex)
    {
      pTag = pIndex->TagList;
      while (pTag != nullptr)
      {
        ++uiTag;
        pTag = pTag->pNext;
      }
      pIndex = pszBag ? nullptr : pIndex->pNext;
    }
    pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, uiTag);
    return Harbour::SUCCESS;
  }

  case DBOI_BAGCOUNT:
  {
    LPCDXINDEX pIndex = pArea->lpIndexes;
    while (pIndex)
    {
      ++uiTag;
      pIndex = pIndex->pNext;
    }
    pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, uiTag);
    return Harbour::SUCCESS;
  }

  case DBOI_BAGNUMBER:
  {
    LPCDXINDEX pIndex = pArea->lpIndexes, pIndexSeek;

    if (hb_itemGetCLen(pInfo->atomBagName) > 0)
    {
      pIndexSeek = hb_cdxFindBag(pArea, pInfo->atomBagName->getCPtr());
    }
    else
    {
      pTag = hb_cdxGetTagByNumber(pArea, pArea->uiTag);
      pIndexSeek = pTag ? pTag->pIndex : nullptr;
    }

    if (pIndexSeek)
    {
      do
      {
        ++uiTag;
        if (pIndex == pIndexSeek)
        {
          break;
        }
        pIndex = pIndex->pNext;
      } while (pIndex);
    }
    pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, pIndex ? uiTag : 0);
    return Harbour::SUCCESS;
  }

  case DBOI_BAGORDER:
  {
    LPCDXINDEX pIndex = pArea->lpIndexes, pIndexSeek;

    if (hb_itemGetCLen(pInfo->atomBagName) > 0)
    {
      pIndexSeek = hb_cdxFindBag(pArea, pInfo->atomBagName->getCPtr());
    }
    else
    {
      pTag = hb_cdxGetTagByNumber(pArea, pArea->uiTag);
      pIndexSeek = pTag ? pTag->pIndex : nullptr;
    }

    if (pIndexSeek)
    {
      ++uiTag;
      do
      {
        if (pIndex == pIndexSeek)
        {
          break;
        }
        pTag = pIndex->TagList;
        while (pTag != nullptr)
        {
          ++uiTag;
          pTag = pTag->pNext;
        }
        pIndex = pIndex->pNext;
      } while (pIndex);
    }
    pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, pIndex ? uiTag : 0);
    return Harbour::SUCCESS;
  }

  case DBOI_RESETPOS:
    hb_cdxClearPosInfo(pArea);
    return Harbour::SUCCESS;
  }

  if (FAST_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE)
  {
    return Harbour::FAILURE;
  }

  if (pInfo->itmOrder)
  {
    pTag = hb_cdxFindTag(pArea, pInfo->itmOrder, pInfo->atomBagName, &uiTag);
  }
  else
  {
    uiTag = pArea->uiTag;
    pTag = hb_cdxGetTagByNumber(pArea, uiTag);
  }

  switch (uiIndex)
  {
  case DBOI_CONDITION:
    pInfo->itmResult = hb_itemPutC(pInfo->itmResult, pTag ? pTag->ForExpr : nullptr);
    if (pTag && pInfo->itmNewVal && pInfo->itmNewVal->isString())
    {
      if (pTag->ForExpr != nullptr)
      {
        hb_xfree(pTag->ForExpr);
        pTag->ForExpr = nullptr;
      }
      if (pTag->pForItem != nullptr)
      {
        hb_vmDestroyBlockOrMacro(pTag->pForItem);
        pTag->pForItem = nullptr;
      }
      if (hb_itemGetCLen(pInfo->itmNewVal) > 0)
      {
        auto pForExpr = pInfo->itmNewVal->getCPtr();

        if (SELF_COMPILE(&pArea->dbfarea.area, pForExpr) == Harbour::SUCCESS)
        {
          PHB_ITEM pForItem = pArea->dbfarea.area.valResult;
          pArea->dbfarea.area.valResult = nullptr;
          if (SELF_EVALBLOCK(&pArea->dbfarea.area, pForItem) == Harbour::SUCCESS)
          {
            if (hb_itemType(pArea->dbfarea.area.valResult) & Harbour::Item::LOGICAL)
            {
              pTag->pForItem = pForItem;
              pForItem = nullptr;
            }
            hb_itemRelease(pArea->dbfarea.area.valResult);
            pArea->dbfarea.area.valResult = nullptr;
          }
          if (pForItem)
          {
            hb_vmDestroyBlockOrMacro(pForItem);
          }
        }
      }
    }
    break;

  case DBOI_EXPRESSION:
    pInfo->itmResult = hb_itemPutC(pInfo->itmResult, pTag ? pTag->KeyExpr : nullptr);
    break;

  case DBOI_POSITION:
    if (pInfo->itmNewVal && pInfo->itmNewVal->isNumeric())
    {
      pInfo->itmResult = hb_itemPutL(
          pInfo->itmResult, hb_cdxDBOIKeyGoto(pArea, pTag, pInfo->itmNewVal->getNL(), true) == Harbour::SUCCESS);
    }
    else
    {
      pInfo->itmResult = hb_itemPutNL(pInfo->itmResult, hb_cdxDBOIKeyNo(pArea, pTag, true));
    }
    break;

  // TODO: is this ok?  DBOI_RECNO == DBOI_KEYNORAW ? No, it isn't.
  // case DBOI_RECNO:
  case DBOI_KEYNORAW:
    if (pInfo->itmNewVal && pInfo->itmNewVal->isNumeric())
    {
      pInfo->itmResult = hb_itemPutL(
          pInfo->itmResult, hb_cdxDBOIKeyGoto(pArea, pTag, pInfo->itmNewVal->getNL(), false) == Harbour::SUCCESS);
    }
    else
    {
      pInfo->itmResult = hb_itemPutNL(pInfo->itmResult, hb_cdxDBOIKeyNo(pArea, pTag, false));
    }
    break;

  case DBOI_KEYCOUNT:
    pInfo->itmResult = hb_itemPutNL(pInfo->itmResult, hb_cdxDBOIKeyCount(pArea, pTag, true));
    break;

  case DBOI_KEYCOUNTRAW:
    pInfo->itmResult = hb_itemPutNL(pInfo->itmResult, hb_cdxDBOIKeyCount(pArea, pTag, false));
    break;

  case DBOI_RELKEYPOS:
    if (pInfo->itmNewVal && pInfo->itmNewVal->isNumeric())
    {
      hb_cdxDBOISetRelKeyPos(pArea, pTag, pInfo->itmNewVal->getND());
    }
    else
    {
      pInfo->itmResult = hb_itemPutND(pInfo->itmResult, hb_cdxDBOIGetRelKeyPos(pArea, pTag));
    }
    break;

  case DBOI_FINDREC:
    pInfo->itmResult =
        hb_itemPutL(pInfo->itmResult, hb_cdxDBOIFindRec(pArea, pTag, hb_itemGetNL(pInfo->itmNewVal), false));
    break;

  case DBOI_FINDRECCONT:
    pInfo->itmResult =
        hb_itemPutL(pInfo->itmResult, hb_cdxDBOIFindRec(pArea, pTag, hb_itemGetNL(pInfo->itmNewVal), true));
    break;

  case DBOI_SKIPUNIQUE:
    pInfo->itmResult =
        hb_itemPutL(pInfo->itmResult,
                    hb_cdxDBOISkipUnique(pArea, pTag,
                                         pInfo->itmNewVal && pInfo->itmNewVal->isNumeric() ? pInfo->itmNewVal->getNL()
                                                                                           : 1) == Harbour::SUCCESS);
    break;

  case DBOI_SKIPEVAL:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, hb_cdxDBOISkipEval(pArea, pTag, true, pInfo->itmNewVal));
    break;

  case DBOI_SKIPEVALBACK:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, hb_cdxDBOISkipEval(pArea, pTag, false, pInfo->itmNewVal));
    break;

  case DBOI_SKIPWILD:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, hb_cdxDBOISkipWild(pArea, pTag, true, pInfo->itmNewVal));
    break;

  case DBOI_SKIPWILDBACK:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, hb_cdxDBOISkipWild(pArea, pTag, false, pInfo->itmNewVal));
    break;

  case DBOI_SKIPREGEX:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, hb_cdxDBOISkipRegEx(pArea, pTag, true, pInfo->itmNewVal));
    break;

  case DBOI_SKIPREGEXBACK:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, hb_cdxDBOISkipRegEx(pArea, pTag, false, pInfo->itmNewVal));
    break;

  case DBOI_SCOPEEVAL:
    if (pTag && pInfo->itmNewVal && hb_arrayLen(pInfo->itmNewVal) == DBRMI_SIZE &&
        hb_arrayGetPtr(pInfo->itmNewVal, DBRMI_FUNCTION) != nullptr)
    {
      pInfo->itmResult = hb_itemPutNL(
          pInfo->itmResult,
          hb_cdxDBOIScopeEval(
              pTag, reinterpret_cast<HB_EVALSCOPE_FUNC>(hb_arrayGetPtr(pInfo->itmNewVal, DBRMI_FUNCTION)),
              hb_arrayGetPtr(pInfo->itmNewVal, DBRMI_PARAM), hb_arrayGetItemPtr(pInfo->itmNewVal, DBRMI_LOVAL),
              hb_arrayGetItemPtr(pInfo->itmNewVal, DBRMI_HIVAL)));
    }
    else
    {
      // TODO: RT error
      ;
    }
    break;

  case DBOI_NAME:
    pInfo->itmResult = hb_itemPutC(pInfo->itmResult, pTag ? pTag->szName : nullptr);
    break;

  case DBOI_NUMBER:
    pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, uiTag);
    break;

  case DBOI_BAGNAME:
    if (pTag != nullptr)
    {
      PHB_FNAME pFileName = hb_fsFNameSplit(pTag->pIndex->szFileName);
      pInfo->itmResult = hb_itemPutC(pInfo->itmResult, pFileName->szName);
      hb_xfree(pFileName);
    }
    else
    {
      pInfo->itmResult = hb_itemPutC(pInfo->itmResult, nullptr);
    }
    break;

  case DBOI_FULLPATH:
    pInfo->itmResult = hb_itemPutC(pInfo->itmResult, pTag ? pTag->pIndex->szFileName : nullptr);
    break;

  case DBOI_FILEHANDLE:
    pInfo->itmResult =
        hb_itemPutNInt(pInfo->itmResult, static_cast<HB_NHANDLE>(pTag ? hb_fileHandle(pTag->pIndex->pFile) : FS_ERROR));
    break;

  case DBOI_ISCOND:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag && pTag->ForExpr != nullptr);
    break;

  case DBOI_ISDESC:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag && !pTag->UsrAscend);
    if (pTag && pInfo->itmNewVal && pInfo->itmNewVal->isLogical())
    {
      pTag->UsrAscend = !pInfo->itmNewVal->getL();
      pTag->curKeyState &= ~(CDX_CURKEY_RAWPOS | CDX_CURKEY_LOGPOS);
    }
    break;

  case DBOI_UNIQUE:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, (pTag ? pTag->UniqueKey || pTag->UsrUnique : false));
    if (pTag && pInfo->itmNewVal && pInfo->itmNewVal->isLogical() && !pTag->UniqueKey)
    {
      pTag->UsrUnique = pInfo->itmNewVal->getL();
      pTag->curKeyState &= ~(CDX_CURKEY_RAWPOS | CDX_CURKEY_LOGPOS | CDX_CURKEY_RAWCNT | CDX_CURKEY_LOGCNT);
    }
    break;

  case DBOI_KEYTYPE:
    if (pTag != nullptr)
    {
      char szType[2];
      szType[0] = static_cast<char>(pTag->uiType);
      szType[1] = 0;
      pInfo->itmResult = hb_itemPutC(pInfo->itmResult, szType);
    }
    else
    {
      pInfo->itmResult = hb_itemPutC(pInfo->itmResult, nullptr);
    }
    break;

  case DBOI_KEYSIZE:
    pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, pTag ? pTag->uiLen : 0);
    break;

  case DBOI_KEYDEC:
    // there is no fixed number of decimal places for numeric keys
    // in CDX format
    pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, 0);
    break;

  case DBOI_KEYVAL:
    hb_itemClear(pInfo->itmResult);
    if (pArea->dbfarea.lpdbPendingRel)
    {
      SELF_FORCEREL(&pArea->dbfarea.area);
    }
    if (pTag && pArea->dbfarea.fPositioned)
    {
      if (pTag->CurKey->rec != pArea->dbfarea.ulRecNo)
      {
        hb_cdxIndexLockRead(pTag->pIndex);
        hb_cdxCurKeyRefresh(pArea, pTag);
        hb_cdxIndexUnLockRead(pTag->pIndex);
      }
      if (pTag->CurKey->rec == pArea->dbfarea.ulRecNo)
      {
        pInfo->itmResult = hb_cdxKeyGetItem(pTag->CurKey, pInfo->itmResult, pTag);
      }
    }
    break;

  case DBOI_SCOPETOP:
    if (pTag != nullptr)
    {
      if (pInfo->itmResult)
      {
        hb_cdxTagGetScope(pTag, 0, pInfo->itmResult);
      }
      if (pInfo->itmNewVal)
      {
        hb_cdxTagSetScope(pTag, 0, pInfo->itmNewVal);
      }
    }
    else if (pInfo->itmResult)
    {
      hb_itemClear(pInfo->itmResult);
    }
    break;

  case DBOI_SCOPEBOTTOM:
    if (pTag != nullptr)
    {
      if (pInfo->itmResult)
      {
        hb_cdxTagGetScope(pTag, 1, pInfo->itmResult);
      }
      if (pInfo->itmNewVal)
      {
        hb_cdxTagSetScope(pTag, 1, pInfo->itmNewVal);
      }
    }
    else if (pInfo->itmResult)
    {
      hb_itemClear(pInfo->itmResult);
    }
    break;

  case DBOI_SCOPESET:
    if (pTag != nullptr)
    {
      if (pInfo->itmNewVal)
      {
        hb_cdxTagSetScope(pTag, 0, pInfo->itmNewVal);
        hb_cdxTagSetScope(pTag, 1, pInfo->itmNewVal);
      }
    }
    if (pInfo->itmResult)
    {
      hb_itemClear(pInfo->itmResult);
    }
    break;

  case DBOI_SCOPETOPCLEAR:
    if (pTag != nullptr)
    {
      if (pInfo->itmResult)
      {
        hb_cdxTagGetScope(pTag, 0, pInfo->itmResult);
      }
      hb_cdxTagClearScope(pTag, 0);
    }
    else if (pInfo->itmResult)
    {
      hb_itemClear(pInfo->itmResult);
    }
    break;

  case DBOI_SCOPEBOTTOMCLEAR:
    if (pTag != nullptr)
    {
      if (pInfo->itmResult)
      {
        hb_cdxTagGetScope(pTag, 1, pInfo->itmResult);
      }
      hb_cdxTagClearScope(pTag, 1);
    }
    else if (pInfo->itmResult)
    {
      hb_itemClear(pInfo->itmResult);
    }
    break;

  case DBOI_SCOPECLEAR:
    if (pTag != nullptr)
    {
      hb_cdxTagClearScope(pTag, 0);
      hb_cdxTagClearScope(pTag, 1);
    }
    if (pInfo->itmResult)
    {
      hb_itemClear(pInfo->itmResult);
    }
    break;

  case DBOI_CUSTOM:
    if (pTag && !pTag->Template && (hb_itemType(pInfo->itmNewVal) & Harbour::Item::LOGICAL))
    {
      bool fNewVal = pInfo->itmNewVal->getL();
      if (pTag->Custom ? !fNewVal : fNewVal)
      {
        if (hb_cdxIndexLockWrite(pTag->pIndex))
        {
          if (!pTag->Template && (pTag->Custom ? !fNewVal : fNewVal))
          {
            pTag->Custom = fNewVal;
            pTag->Partial = true;
            pTag->ChgOnly = false;
            pTag->TagChanged = true;
            // This is a hacks to emulate both SIX3 and COMIX behavior
            // which should be cleaned. I intentionally not used
            // HB_SIXCDX macro here [druzus]
            if (pTag->Custom)
            {
              pTag->Template = pTag->MultiKey = true;
            }
          }
          hb_cdxIndexUnLockWrite(pTag->pIndex);
        }
      }
    }
    // Warning: it's not CL53 compatible. CL53 returns previous
    // CUSTOM flag value not current one. [druzus]
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag && pTag->Custom);
    break;

  case DBOI_CHGONLY:
    if (pTag && !pTag->Custom && (hb_itemType(pInfo->itmNewVal) & Harbour::Item::LOGICAL))
    {
      bool fNewVal = pInfo->itmNewVal->getL();
      if (pTag->ChgOnly ? !fNewVal : fNewVal)
      {
        if (hb_cdxIndexLockWrite(pTag->pIndex))
        {
          if (!pTag->Custom && (pTag->ChgOnly ? !fNewVal : fNewVal))
          {
            pTag->ChgOnly = fNewVal;
            pTag->Partial = true;
            pTag->TagChanged = true;
          }
          hb_cdxIndexUnLockWrite(pTag->pIndex);
        }
      }
    }
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag && pTag->ChgOnly);
    break;

  case DBOI_TEMPLATE:
    if (pTag && pTag->Custom && !pTag->Template && hb_itemGetL(pInfo->itmNewVal))
    {
      if (hb_cdxIndexLockWrite(pTag->pIndex))
      {
        if (pTag->Custom && !pTag->Template)
        {
          pTag->Template = true;
          pTag->TagChanged = true;
        }
        hb_cdxIndexUnLockWrite(pTag->pIndex);
      }
    }
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag && pTag->Template);
    break;

  case DBOI_MULTIKEY:
    if (pTag && pTag->Custom && !pTag->MultiKey && hb_itemGetL(pInfo->itmNewVal))
    {
      if (hb_cdxIndexLockWrite(pTag->pIndex))
      {
        if (pTag->Custom && !pTag->MultiKey)
        {
          pTag->MultiKey = true;
          pTag->TagChanged = true;
        }
        hb_cdxIndexUnLockWrite(pTag->pIndex);
      }
    }
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag && pTag->MultiKey);
    break;

  case DBOI_PARTIAL:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag && pTag->Partial);
    break;

  case DBOI_KEYADD:
  {
    auto fResult = false;
    if (pTag != nullptr)
    {
      if (pTag->Custom)
      {
        if (pArea->dbfarea.lpdbPendingRel)
        {
          SELF_FORCEREL(&pArea->dbfarea.area);
        }

        if (pArea->dbfarea.fPositioned && (!pTag->pForItem || hb_cdxEvalCond(pArea, pTag->pForItem, true)))
        {
          LPCDXKEY pKey;
#if defined(HB_SIXCDX)
          if (pTag->Template)
          {
            if (pTag->uiType == hb_cdxItemType(pInfo->itmNewVal))
            {
              pKey = hb_cdxKeyPutItem(nullptr, pInfo->itmNewVal, pArea->dbfarea.ulRecNo, pTag, CDX_CMP_EXACT);
            }
            else
            {
              pKey = nullptr;
            }
          }
#else
          if (pInfo->itmNewVal && !pInfo->itmNewVal->isNil() && pTag->Template)
          {
            pKey = hb_cdxKeyPutItem(nullptr, pInfo->itmNewVal, pArea->dbfarea.ulRecNo, pTag, CDX_CMP_EXACT);
          }
#endif
          else
          {
            pKey = hb_cdxKeyEval(nullptr, pTag);
          }

          if (pKey)
          {
            if (hb_cdxIndexLockWrite(pTag->pIndex))
            {
              fResult = hb_cdxTagKeyAdd(pTag, pKey);
              hb_cdxIndexUnLockWrite(pTag->pIndex);
            }
            hb_cdxKeyFree(pKey);
          }
        }
      }
#if !defined(HB_SIXCDX)
      else
      {
        hb_cdxErrorRT(pArea, EG_ARG, EDBF_NOTCUSTOM, nullptr, 0, 0, nullptr);
      }
#endif
    }
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, fResult);
    break;
  }
  case DBOI_KEYDELETE:
  {
    auto fResult = false;
    if (pTag != nullptr)
    {
      if (pTag->Custom)
      {
        if (pArea->dbfarea.lpdbPendingRel)
        {
          SELF_FORCEREL(&pArea->dbfarea.area);
        }

        if (pArea->dbfarea.fPositioned && (!pTag->pForItem || hb_cdxEvalCond(pArea, pTag->pForItem, true)))
        {
          auto fLck = false;
          LPCDXKEY pKey;
#if defined(HB_SIXCDX)
          if (pTag->Template)
          {
            if (pTag->uiType == hb_cdxItemType(pInfo->itmNewVal))
            {
              pKey = hb_cdxKeyPutItem(nullptr, pInfo->itmNewVal, pArea->dbfarea.ulRecNo, pTag, CDX_CMP_EXACT);
            }
            else
            {
              pKey = nullptr;
            }
          }
#else
          if (pInfo->itmNewVal && !pInfo->itmNewVal->isNil() && pTag->Template)
          {
            pKey = hb_cdxKeyPutItem(nullptr, pInfo->itmNewVal, pArea->dbfarea.ulRecNo, pTag, CDX_CMP_EXACT);
          }
#endif
          else
          {
            if (pTag->CurKey->rec != pArea->dbfarea.ulRecNo)
            {
              if (hb_cdxIndexLockWrite(pTag->pIndex))
              {
                fLck = true;
                hb_cdxCurKeyRefresh(pArea, pTag);
              }
            }

            if (pTag->CurKey->rec == pArea->dbfarea.ulRecNo)
            {
              pKey = hb_cdxKeyCopy(nullptr, pTag->CurKey);
            }
            else
            {
              pKey = hb_cdxKeyEval(nullptr, pTag);
            }
          }
          if (pKey)
          {
            if (!fLck)
            {
              fLck = hb_cdxIndexLockWrite(pTag->pIndex);
            }
            if (fLck)
            {
              fResult = hb_cdxTagKeyDel(pTag, pKey);
            }
            hb_cdxKeyFree(pKey);
          }
          if (fLck)
          {
            hb_cdxIndexUnLockWrite(pTag->pIndex);
          }
        }
      }
#if !defined(HB_SIXCDX)
      else
      {
        hb_cdxErrorRT(pArea, EG_ARG, EDBF_NOTCUSTOM, nullptr, 0, 0, nullptr);
      }
#endif
    }
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, fResult);
    break;
  }
  case DBOI_READLOCK:
    if (pTag != nullptr)
    {
      if (hb_itemType(pInfo->itmNewVal) & Harbour::Item::LOGICAL)
      {
        if (pInfo->itmNewVal->getL())
        {
          hb_cdxIndexLockRead(pTag->pIndex);
        }
        else
        {
          hb_cdxIndexUnLockRead(pTag->pIndex);
        }
      }
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->pIndex->lockRead > 0);
    }
    else
    {
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, false);
    }
    break;

  case DBOI_WRITELOCK:
    if (pTag != nullptr)
    {
      if (hb_itemType(pInfo->itmNewVal) & Harbour::Item::LOGICAL)
      {
        if (pInfo->itmNewVal->getL())
        {
          hb_cdxIndexLockWrite(pTag->pIndex);
        }
        else
        {
          hb_cdxIndexUnLockWrite(pTag->pIndex);
        }
      }
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->pIndex->lockWrite > 0);
    }
    else
    {
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, false);
    }
    break;

  case DBOI_UPDATECOUNTER:
    if (pTag != nullptr)
    {
      // refresh update counter
      if (hb_cdxIndexLockRead(pTag->pIndex))
      {
        hb_cdxIndexUnLockRead(pTag->pIndex);
      }
      pInfo->itmResult = hb_itemPutNInt(pInfo->itmResult, pTag->pIndex->ulVersion);
    }
    else
    {
      pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, 0);
    }
    break;

  case DBOI_SHARED:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag && pTag->pIndex->fShared);
    break;

  case DBOI_ISREADONLY:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag && pTag->pIndex->fReadonly);
    break;

  case DBOI_ISMULTITAG:
  case DBOI_ISSORTRECNO:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag != nullptr);
    break;

  case DBOI_LARGEFILE:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag && pTag->pIndex->fLargeFile);
    break;

  case DBOI_INDEXTYPE:
    pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, pTag ? DBOI_TYPE_COMPOUND : DBOI_TYPE_UNDEF);
    break;

  case DBOI_INDEXPAGESIZE:
    pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, pTag ? pTag->pIndex->uiPageLen : 0);
    break;

  default:
    return SUPER_ORDINFO(&pArea->dbfarea.area, uiIndex, pInfo);
  }
  return Harbour::SUCCESS;
}

// ( DBENTRYP_V )     hb_cdxClearFilter
static HB_ERRCODE hb_cdxClearFilter(CDXAREAP pArea)
{
  HB_ERRCODE errCode = SUPER_CLEARFILTER(&pArea->dbfarea.area);

  hb_cdxClearLogPosInfo(pArea);
  return errCode;
}

// ( DBENTRYP_V )     hb_cdxClearLocate     : nullptr
// ( DBENTRYP_V )     hb_cdxClearScope      : nullptr

// ( DBENTRYP_VPLP )  hb_cdxCountScope
static HB_ERRCODE hb_cdxCountScope(CDXAREAP pArea, void *pPtr, HB_LONG *plRec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxCountScope(%p, %p, %p)", static_cast<void*>(pArea), static_cast<void*>(pPtr), static_cast<void*>(plRec)));
#endif

  if (pPtr == nullptr)
  {
    return Harbour::SUCCESS;
  }
  return SUPER_COUNTSCOPE(&pArea->dbfarea.area, pPtr, plRec);
}

// ( DBENTRYP_I )     hb_cdxFilterText      : nullptr
// ( DBENTRYP_SI )    hb_cdxScopeInfo       : nullptr

// ( DBENTRYP_VFI )   hb_cdxSetFilter
static HB_ERRCODE hb_cdxSetFilter(CDXAREAP pArea, LPDBFILTERINFO pFilterInfo)
{
  HB_ERRCODE errCode = SUPER_SETFILTER(&pArea->dbfarea.area, pFilterInfo);

  hb_cdxClearLogPosInfo(pArea);
  return errCode;
}

// ( DBENTRYP_VLO )   hb_cdxSetLocate       : nullptr
// ( DBENTRYP_VOS )   hb_cdxSetScope        : nullptr
// ( DBENTRYP_VPL )   hb_cdxSkipScope       : nullptr
// ( DBENTRYP_B )     hb_cdxLocate          : nullptr

// ( DBENTRYP_CC )    hb_cdxCompile         : nullptr
// ( DBENTRYP_I )     hb_cdxError           : nullptr
// ( DBENTRYP_I )     hb_cdxEvalBlock       : nullptr

// ( DBENTRYP_VSP )   hb_cdxRawLock         : nullptr
// ( DBENTRYP_VL )    hb_cdxLock            : nullptr
// ( DBENTRYP_UL )    hb_cdxUnLock          : nullptr

// ( DBENTRYP_V )     hb_cdxCloseMemFile    : nullptr
// ( DBENTRYP_VO )    hb_cdxCreateMemFile   : nullptr
// ( DBENTRYP_SCCS )  hb_cdxGetValueFile    : nullptr
// ( DBENTRYP_VO )    hb_cdxOpenMemFile     : nullptr
// ( DBENTRYP_SCCS )  hb_cdxPutValueFile    : nullptr

// ( DBENTRYP_V )     hb_cdxReadDBHeader    : nullptr
// ( DBENTRYP_V )     hb_cdxWriteDBHeader   : nullptr
// ( DBENTRYP_SVP )   hb_cdxWhoCares        : nullptr

// Retrieve (set) information about RDD
// ( DBENTRYP_RSLV )   hb_fptFieldInfo
static HB_ERRCODE hb_cdxRddInfo(LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxRddInfo(%p, %hu, %lu, %p)", static_cast<void*>(pRDD), uiIndex, ulConnect, static_cast<void*>(pItem)));
#endif

  LPDBFDATA pData;

  pData = DBFNODE_DATA(pRDD);

  switch (uiIndex)
  {
  case RDDI_ORDBAGEXT:
  case RDDI_ORDEREXT:
  case RDDI_ORDSTRUCTEXT:
  {
    auto szExt = hb_itemGetCPtr(pItem);
    char *szNewVal;

    szNewVal = szExt[0] == '.' && szExt[1] ? hb_strdup(szExt) : nullptr;
    hb_itemPutC(pItem, pData->szIndexExt[0] ? pData->szIndexExt : CDX_INDEXEXT);
    if (szNewVal != nullptr)
    {
      hb_strncpy(pData->szIndexExt, szNewVal, sizeof(pData->szIndexExt) - 1);
      hb_xfree(szNewVal);
    }
    break;
  }

  case RDDI_MULTIKEY:
  case RDDI_MULTITAG:
  case RDDI_SORTRECNO:
  case RDDI_STRUCTORD:
    hb_itemPutL(pItem, true);
    break;

  case RDDI_STRICTSTRUCT:
  {
    bool fStrictStruct = pData->fStrictStruct;
    if (hb_itemType(pItem) & Harbour::Item::LOGICAL)
    {
      pData->fStrictStruct = pItem->getL();
    }
    hb_itemPutL(pItem, fStrictStruct);
    break;
  }

  default:
    return SUPER_RDDINFO(pRDD, uiIndex, ulConnect, pItem);
  }

  return Harbour::SUCCESS;
}

// #########################################################################

static int hb_cdxQuickSortCompare(LPCDXSORTINFO pSort, HB_BYTE *pKey1, HB_BYTE *pKey2)
{
  int i, iLen = pSort->keyLen;

  i = hb_cdxValCompare(pSort->pTag, pKey1, iLen, pKey2, iLen, CDX_CMP_EXACT);

  if (i == 0)
  {
    i = (HB_GET_LE_UINT32(pKey1 + iLen) < HB_GET_LE_UINT32(pKey2 + iLen)) ? -1 : 1;
  }

  return i;
}

static bool hb_cdxQSort(LPCDXSORTINFO pSort, HB_BYTE *pSrc, HB_BYTE *pBuf, HB_LONG lKeys)
{
  if (lKeys > 1)
  {
    int iLen = pSort->keyLen + 4;
    HB_LONG l1, l2;
    HB_BYTE *pPtr1, *pPtr2, *pDst;
    auto f1 = false;
    auto f2 = false;

    l1 = lKeys >> 1;
    l2 = lKeys - l1;
    pPtr1 = &pSrc[0];
    pPtr2 = &pSrc[l1 * iLen];

    f1 = hb_cdxQSort(pSort, pPtr1, &pBuf[0], l1);
    f2 = hb_cdxQSort(pSort, pPtr2, &pBuf[l1 * iLen], l2);
    if (f1)
    {
      pDst = pBuf;
    }
    else
    {
      pDst = pSrc;
      pPtr1 = &pBuf[0];
    }
    if (!f2)
    {
      pPtr2 = &pBuf[l1 * iLen];
    }
    while (l1 > 0 && l2 > 0)
    {
      if (hb_cdxQuickSortCompare(pSort, pPtr1, pPtr2) <= 0)
      {
        memcpy(pDst, pPtr1, iLen);
        pPtr1 += iLen;
        l1--;
      }
      else
      {
        memcpy(pDst, pPtr2, iLen);
        pPtr2 += iLen;
        l2--;
      }
      pDst += iLen;
    }
    if (l1 > 0)
    {
      memcpy(pDst, pPtr1, iLen * l1);
    }
    else if (l2 > 0 && f1 == f2)
    {
      memcpy(pDst, pPtr2, iLen * l2);
    }
    return !f1;
  }
  return true;
}

static void hb_cdxSortSortPage(LPCDXSORTINFO pSort)
{
  HB_SIZE nSize = static_cast<HB_SIZE>(pSort->ulKeys) * (pSort->keyLen + 4);

#ifdef HB_CDX_DBGTIME
  cdxTimeIdxBld -= hb_cdxGetTime();
#endif
  if (!hb_cdxQSort(pSort, pSort->pKeyPool, &pSort->pKeyPool[nSize], pSort->ulKeys))
  {
    pSort->pStartKey = &pSort->pKeyPool[nSize];
  }
  else
  {
    pSort->pStartKey = pSort->pKeyPool;
  }
#ifdef HB_CDX_DBGTIME
  cdxTimeIdxBld += hb_cdxGetTime();
#endif
}

static void hb_cdxSortAddNodeKey(LPCDXSORTINFO pSort, int iLevel, HB_BYTE *pKeyVal, HB_ULONG ulRec, HB_ULONG ulPage)
{
  LPCDXPAGE pPage;
  auto fNew = false;
  int iLen = pSort->keyLen, iDup = 0, iTrl = 0, iTmp, iPos;
  HB_BYTE *pTmp;

  pPage = pSort->NodeList[iLevel];
  if (iLevel == 0)
  {
    while (iTrl < iLen && pKeyVal[iLen - iTrl - 1] == pSort->bTrl)
    {
      iTrl++;
    }
    if (pPage != nullptr && pPage->iKeys > 0)
    {
#ifdef HB_CDX_PACKTRAIL
      int iMax = iLen - iTrl;
#else
      int iMax = iLen - HB_MAX(iTrl, pSort->iLastTrl);
#endif
      while (pKeyVal[iDup] == pSort->pLastKey[iDup] && iDup < iMax)
      {
        iDup++;
      }
    }
#ifndef HB_CDX_PACKTRAIL
    pSort->iLastTrl = iTrl;
#endif
  }
  if (pPage == nullptr)
  {
    fNew = true;
  }
  else
  {
    if (iLevel == 0)
    {
      fNew = (pPage->iFree - (iLen - iDup - iTrl) - pPage->ReqByte) < 0;
    }
    else
    {
      fNew = (pSort->NodeList[iLevel]->iKeys >= pSort->pTag->MaxKeys);
    }
  }

  if (fNew)
  {
    pPage = hb_cdxPageNew(pSort->pTag, nullptr, 0);
    pPage->PageType = (iLevel == 0) ? CDX_NODE_LEAF : CDX_NODE_BRANCH;
    if (iLevel == 0)
    {
      hb_cdxPageLeafInitSpace(pPage);
      iDup = 0;
      while (pSort->ulMaxRec > pPage->RNMask)
      {
        pPage->ReqByte++;
        pPage->RNBits += 8;
        pPage->RNMask = (pPage->RNMask << 8) | 0xFF;
      }
    }
    if (pSort->NodeList[iLevel] != nullptr)
    {
      pSort->NodeList[iLevel]->Right = pPage->Page;
      pPage->Left = pSort->NodeList[iLevel]->Page;
      if (iLevel == 0)
      {
#ifdef HB_CDX_DBGCODE_EXT
        hb_cdxPageCheckDupTrlRaw(pSort->NodeList[iLevel]);
#endif
        hb_cdxSortAddNodeKey(pSort, iLevel + 1, pSort->pLastKey, pSort->ulLastRec, pSort->NodeList[iLevel]->Page);
      }
      else
      {
        iPos = (pSort->NodeList[iLevel]->iKeys - 1) * (iLen + 8);
        pTmp = &hb_cdxPageIntKeyPool(pSort->NodeList[iLevel])[iPos];
        hb_cdxSortAddNodeKey(pSort, iLevel + 1, pTmp, HB_GET_BE_UINT32(&pTmp[iLen]), pSort->NodeList[iLevel]->Page);
      }
      hb_cdxPageFree(pSort->NodeList[iLevel], true);
    }
    pSort->NodeList[iLevel] = pPage;
  }
  if (iLevel == 0)
  {
    iPos = pPage->iKeys * pPage->ReqByte;
    hb_cdxSetLeafRecord(&hb_cdxPageExtKeyPool(pPage)[iPos], ulRec, iDup, iTrl, pPage->ReqByte, pPage->DCBits,
                        pPage->TCBits);
#ifdef HB_CDX_DBGCODE_EXT
    hb_cdxChkLeafRecord(&hb_cdxPageExtKeyPool(pPage)[iPos], ulRec, iDup, iTrl, pPage);
#endif
    iTmp = iLen - iDup - iTrl;
    if (iTmp > 0)
    {
      memcpy(&hb_cdxPageExtKeyPool(pPage)[pPage->iFree + iPos - iTmp], &pKeyVal[iDup], iTmp);
    }
    pPage->iFree -= static_cast<HB_SHORT>(iTmp + pPage->ReqByte);
    pPage->iKeys++;
#ifdef HB_CDX_DBGCODE_EXT
    hb_cdxPageCheckDupTrlRaw(pSort->NodeList[iLevel]);
#endif
  }
  else
  {
    pPage = pSort->NodeList[iLevel];
    iPos = pPage->iKeys * (iLen + 8);
    pTmp = &hb_cdxPageIntKeyPool(pPage)[iPos];
    memcpy(pTmp, pKeyVal, iLen);
    HB_PUT_BE_UINT32(&pTmp[iLen], ulRec);
    HB_PUT_BE_UINT32(&pTmp[iLen + 4], ulPage);
    pPage->iKeys++;
  }
}

static void hb_cdxSortWritePage(LPCDXSORTINFO pSort)
{
  HB_SIZE nSize = static_cast<HB_SIZE>(pSort->ulKeys) * (pSort->keyLen + 4);

  hb_cdxSortSortPage(pSort);

  if (pSort->pTempFile == nullptr)
  {
    char szName[HB_PATH_MAX];
    pSort->pTempFile = hb_fileCreateTemp(nullptr, nullptr, FC_NORMAL, szName);
    if (pSort->pTempFile == nullptr)
    {
      hb_errInternal(9301, "hb_cdxSortWritePage: Could not create temporary file.", nullptr, nullptr);
    }
    pSort->szTempFileName = hb_strdup(szName);
  }
  pSort->pSwapPage[pSort->ulCurPage].ulKeys = pSort->ulKeys;
  pSort->pSwapPage[pSort->ulCurPage].nOffset = hb_fileSize(pSort->pTempFile);
  if (hb_fileWriteAt(pSort->pTempFile, pSort->pStartKey, nSize, pSort->pSwapPage[pSort->ulCurPage].nOffset) != nSize)
  {
    hb_errInternal(9302, "hb_cdxSortWritePage: Write error in temporary file.", nullptr, nullptr);
  }
  pSort->ulKeys = 0;
  pSort->ulCurPage++;
}

static void hb_cdxSortGetPageKey(LPCDXSORTINFO pSort, HB_ULONG ulPage, HB_BYTE **pKeyVal, HB_ULONG *pulRec)
{
  int iLen = pSort->keyLen;

  if (pSort->pSwapPage[ulPage].ulKeyBuf == 0)
  {
    HB_ULONG ulKeys = HB_MIN(pSort->ulPgKeys, pSort->pSwapPage[ulPage].ulKeys);
    HB_SIZE nSize = static_cast<HB_SIZE>(ulKeys) * (iLen + 4);

    if (hb_fileReadAt(pSort->pTempFile, pSort->pSwapPage[ulPage].pKeyPool, nSize, pSort->pSwapPage[ulPage].nOffset) !=
        nSize)
    {
      hb_errInternal(9303, "hb_cdxSortGetPageKey: Read error from temporary file.", nullptr, nullptr);
    }
    pSort->pSwapPage[ulPage].nOffset += nSize;
    pSort->pSwapPage[ulPage].ulKeyBuf = ulKeys;
    pSort->pSwapPage[ulPage].ulCurKey = 0;
  }
  *pKeyVal = &pSort->pSwapPage[ulPage].pKeyPool[pSort->pSwapPage[ulPage].ulCurKey * (iLen + 4)];
  *pulRec = HB_GET_LE_UINT32(*pKeyVal + iLen);
}

#ifdef HB_CDX_NEW_SORT
static void hb_cdxSortOrderPages(LPCDXSORTINFO pSort)
{
  pSort->ulFirst = 0;
  pSort->pSortedPages = static_cast<HB_ULONG *>(hb_xgrab(pSort->ulPages * sizeof(HB_ULONG)));
  pSort->pSortedPages[0] = 0;

  if (pSort->ulTotKeys > 0)
  {
    int iLen = pSort->keyLen;
    HB_BYTE *pKey = nullptr;

    for (HB_ULONG n = 0; n < pSort->ulPages; n++)
    {
      HB_ULONG ulRec;
      HB_LONG l, r;

      hb_cdxSortGetPageKey(pSort, n, &pKey, &ulRec);
      l = 0;
      r = n - 1;
      while (l <= r)
      {
        int i;
        HB_ULONG ulPage;
        HB_LONG m;
        HB_BYTE *pTmp;

        m = (l + r) >> 1;
        ulPage = pSort->pSortedPages[m];
        pTmp = &pSort->pSwapPage[ulPage].pKeyPool[pSort->pSwapPage[ulPage].ulCurKey * (iLen + 4)];
        i = hb_cdxValCompare(pSort->pTag, pKey, iLen, pTmp, iLen, CDX_CMP_EXACT);
        if (i == 0)
        {
          i = (ulRec < HB_GET_LE_UINT32(&pTmp[iLen])) ? -1 : 1;
        }
        if (i > 0)
        {
          l = m + 1;
        }
        else
        {
          r = m - 1;
        }
      }
      for (r = n; r > l; r--)
      {
        pSort->pSortedPages[r] = pSort->pSortedPages[r - 1];
      }
      pSort->pSortedPages[l] = n;
    }
  }
}

static bool hb_cdxSortKeyGet(LPCDXSORTINFO pSort, HB_BYTE **pKeyVal, HB_ULONG *pulRec)
{
  HB_ULONG ulPage = pSort->pSortedPages[pSort->ulFirst];

  // check if first page has some keys yet
  if (pSort->pSwapPage[ulPage].ulKeys > 0)
  {
    int iLen = pSort->keyLen;
    HB_BYTE *pKey;
    HB_ULONG ulRec;
    HB_LONG l, r;

    // last key was taken from this page - we have to resort it.
    // This is done intentionally here to be sure that the key
    // value return by this function will not be overwritten by
    // next keys in page read from temporary file in function
    // hb_cdxSortGetPageKey() - please do not move this part down
    // even it seems to be correct
    hb_cdxSortGetPageKey(pSort, ulPage, &pKey, &ulRec);

    l = pSort->ulFirst + 1;
    r = pSort->ulPages - 1;
    while (l <= r)
    {
      int i;
      HB_LONG m;
      HB_BYTE *pTmp;

      m = (l + r) >> 1;
      ulPage = pSort->pSortedPages[m];
      pTmp = &pSort->pSwapPage[ulPage].pKeyPool[pSort->pSwapPage[ulPage].ulCurKey * (iLen + 4)];
      i = hb_cdxValCompare(pSort->pTag, pKey, iLen, pTmp, iLen, CDX_CMP_EXACT);
      if (i == 0)
      {
        i = (ulRec < HB_GET_LE_UINT32(&pTmp[iLen])) ? -1 : 1;
      }

      if (i > 0)
      {
        l = m + 1;
      }
      else
      {
        r = m - 1;
      }
    }
    if (l > static_cast<HB_LONG>(pSort->ulFirst) + 1)
    {
      ulPage = pSort->pSortedPages[pSort->ulFirst];
      for (r = pSort->ulFirst + 1; r < l; r++)
      {
        pSort->pSortedPages[r - 1] = pSort->pSortedPages[r];
      }
      pSort->pSortedPages[l - 1] = ulPage;
    }
  }
  else
  {
    pSort->ulFirst++;
  }

  if (pSort->ulFirst < pSort->ulPages)
  {
    ulPage = pSort->pSortedPages[pSort->ulFirst];
    hb_cdxSortGetPageKey(pSort, ulPage, pKeyVal, pulRec);
    pSort->pSwapPage[ulPage].ulCurKey++;
    pSort->pSwapPage[ulPage].ulKeys--;
    pSort->pSwapPage[ulPage].ulKeyBuf--;
    return true;
  }
  *pulRec = 0;
  *pKeyVal = nullptr;
  return false;
}

#else

static bool hb_cdxSortKeyGet(LPCDXSORTINFO pSort, HB_BYTE **pKeyVal, HB_ULONG *pulRec)
{
  int i, iLen = pSort->keyLen;
  HB_ULONG ulKeyPage = 0, ulRec = 0, ulRecTmp;
  HB_BYTE *pKey = nullptr, *pTmp;

  for (HB_ULONG ulPage = 0; ulPage < pSort->ulPages; ulPage++)
  {
    if (pSort->pSwapPage[ulPage].ulKeys > 0)
    {
      hb_cdxSortGetPageKey(pSort, ulPage, &pTmp, &ulRecTmp);
      if (!pKey)
      {
        i = 1;
      }
      else
      {
        i = hb_cdxValCompare(pSort->pTag, pKey, iLen, pTmp, iLen, CDX_CMP_EXACT);
        if (i == 0)
        {
          i = (ulRec < ulRecTmp) ? -1 : 1;
        }
      }
      if (i > 0)
      {
        pKey = pTmp;
        ulRec = ulRecTmp;
        ulKeyPage = ulPage;
      }
    }
  }
  if (pKey)
  {
    pSort->pSwapPage[ulKeyPage].ulCurKey++;
    pSort->pSwapPage[ulKeyPage].ulKeys--;
    pSort->pSwapPage[ulKeyPage].ulKeyBuf--;
    *pulRec = ulRec;
    *pKeyVal = pKey;
    return true;
  }
  *pulRec = 0;
  *pKeyVal = nullptr;
  return false;
}

#endif

static void hb_cdxSortKeyAdd(LPCDXSORTINFO pSort, HB_ULONG ulRec, const HB_BYTE *pKeyVal, int iKeyLen)
{
  int iLen = pSort->keyLen;
  HB_BYTE *pDst;

  if (pSort->ulKeys >= pSort->ulPgKeys)
  {
    hb_cdxSortWritePage(pSort);
  }
  pDst = &pSort->pKeyPool[pSort->ulKeys * (iLen + 4)];

  if (pSort->pTag->IgnoreCase)
  {
    iKeyLen = static_cast<int>(hb_cdpnDup2Upper(pSort->pTag->pIndex->pArea->dbfarea.area.cdPage,
                                                reinterpret_cast<const char *>(pKeyVal), iKeyLen,
                                                reinterpret_cast<char *>(pDst), iLen));
    if (iLen > iKeyLen)
    {
      memset(&pDst[iKeyLen], pSort->bTrl, iLen - iKeyLen);
    }
  }
  else if (iLen > iKeyLen)
  {
    memcpy(pDst, pKeyVal, iKeyLen);
    memset(&pDst[iKeyLen], pSort->bTrl, iLen - iKeyLen);
  }
  else
  {
    memcpy(pDst, pKeyVal, iLen);
  }

  HB_PUT_LE_UINT32(&pDst[iLen], ulRec);
  pSort->ulKeys++;
  pSort->ulTotKeys++;
}

static LPCDXSORTINFO hb_cdxSortNew(LPCDXTAG pTag, HB_ULONG ulRecCount)
{
  HB_BYTE *pBuf;
  int iLen = pTag->uiLen;
  HB_ULONG ulSize, ulMax, ulMin;

  if (ulRecCount == 0)
  {
    ulRecCount = 1;
  }

  auto pSort = static_cast<LPCDXSORTINFO>(hb_xgrab(sizeof(CDXSORTINFO)));
  memset(pSort, 0, sizeof(CDXSORTINFO));
  ulMax = ulMin = static_cast<HB_ULONG>(ceil(sqrt(static_cast<double>(ulRecCount))));
  ulSize = (1L << 20) / (iLen + 4);
  while (ulMax < ulSize)
  {
    ulMax <<= 1;
  }
  if (ulMax > ulRecCount)
  {
    ulMax = ulRecCount;
  }

  do
  {
    ulSize = ulMax * (iLen + 4);
    pBuf = static_cast<HB_BYTE *>(hb_xalloc(ulSize << 2));
    if (pBuf)
    {
      hb_xfree(pBuf);
      pBuf = static_cast<HB_BYTE *>(hb_xalloc(ulSize << 1));
    }
    else
    {
      ulMax >>= 1;
    }
  } while (!pBuf && ulMax >= ulMin);

  if (!pBuf)
  {
    // call hb_xgrab() to force out of memory error,
    // though in multi process environment this call may return
    // with success when other process free some memory
    // (also the size of buf is reduced to absolute minimum).
    // Sorry but I'm to lazy to implement indexing with smaller
    // memory though it's possible - just simply I can even create
    // index on-line by key adding like in normal update process.
    // The memory necessary to index file is now ~
    //    ~ (keySize+4+sizeof(CDXSWAPPAGE)) * sqrt(ulRecCount) * 2
    // so the maximum is for DBF with 2^32 records and keySize 240 ~
    // ~ 2^17 * 268 ~=~ 35 MB
    // this is not a problem for current computers and I do not see
    // any way to use DBFs with four billions records and indexes with
    // such long (240 bytes) keys on the old ones - they will be simply
    // to slow. IMHO it's also better to signal out of memory here and
    // force some system upgrades then run process which will have to
    // take many hours, Druzus.
    ulMax = ulMin;
    pBuf = static_cast<HB_BYTE *>(hb_xgrab((ulMax << 1) * (iLen + 4)));
  }

  pSort->pTag = pTag;
  pSort->pTempFile = nullptr;
  pSort->keyLen = iLen;
  pSort->bTrl = pTag->bTrail;
  pSort->fUnique = pTag->UniqueKey;
  pSort->ulMaxKey = ulMax << 1;
  pSort->ulPgKeys = ulMax;
  pSort->ulMaxRec = ulRecCount;
  pSort->pKeyPool = pBuf;
  pSort->ulPages = (ulRecCount + pSort->ulPgKeys - 1) / pSort->ulPgKeys;
  pSort->pSwapPage = static_cast<LPCDXSWAPPAGE>(hb_xgrab(sizeof(CDXSWAPPAGE) * pSort->ulPages));
  memset(pSort->pSwapPage, 0, sizeof(CDXSWAPPAGE) * pSort->ulPages);
  pSort->pLastKey = static_cast<HB_BYTE *>(hb_xgrabz(iLen + 1));

  return pSort;
}

static void hb_cdxSortFree(LPCDXSORTINFO pSort)
{
  if (pSort->pTempFile != nullptr)
  {
    hb_fileClose(pSort->pTempFile);
  }
  if (pSort->szTempFileName)
  {
    hb_fileDelete(pSort->szTempFileName);
    hb_xfree(pSort->szTempFileName);
  }
  if (pSort->pLastKey)
  {
    hb_xfree(pSort->pLastKey);
  }
  if (pSort->pKeyPool)
  {
    hb_xfree(pSort->pKeyPool);
  }
  if (pSort->pSwapPage)
  {
    hb_xfree(pSort->pSwapPage);
  }
  if (pSort->pRecBuff)
  {
    hb_xfree(pSort->pRecBuff);
  }
  if (pSort->pSortedPages)
  {
    hb_xfree(pSort->pSortedPages);
  }
  hb_xfree(pSort);
}

static void hb_cdxSortOut(LPCDXSORTINFO pSort)
{
  bool fUnique = pSort->fUnique;
  auto fNext = false;
  HB_ULONG ulRec;
  HB_BYTE *pKeyVal;
  int iLen = pSort->keyLen, iLevel;

  pSort->ulPages = pSort->ulCurPage + 1;
  pSort->ulPgKeys = pSort->ulMaxKey / pSort->ulPages;
#if 0
  fprintf(stderr, "\r\npSort->ulMaxKey=%ld, pSort->ulPages=%ld, pSort->ulPgKeys=%ld, size=%ld\r\n",
          pSort->ulMaxKey, pSort->ulPages, pSort->ulPgKeys,
          pSort->ulMaxKey * (pSort->keyLen + 4));
          fflush(stderr);
#endif
  if (pSort->ulPages > 1)
  {
    HB_BYTE *pBuf = pSort->pKeyPool;
    hb_cdxSortWritePage(pSort);
    for (HB_ULONG ulPage = 0; ulPage < pSort->ulPages; ulPage++)
    {
      pSort->pSwapPage[ulPage].ulKeyBuf = 0;
      pSort->pSwapPage[ulPage].ulCurKey = 0;
      pSort->pSwapPage[ulPage].pKeyPool = pBuf;
      pBuf += pSort->ulPgKeys * (pSort->keyLen + 4);
    }
  }
  else
  {
    hb_cdxSortSortPage(pSort);
    pSort->pSwapPage[0].ulKeys = pSort->ulKeys;
    pSort->pSwapPage[0].ulKeyBuf = pSort->ulKeys;
    pSort->pSwapPage[0].ulCurKey = 0;
    pSort->pSwapPage[0].pKeyPool = pSort->pStartKey;
  }

#ifdef HB_CDX_NEW_SORT
  hb_cdxSortOrderPages(pSort);
#endif

  for (HB_ULONG ulKey = 0; ulKey < pSort->ulTotKeys; ulKey++)
  {
    if (!hb_cdxSortKeyGet(pSort, &pKeyVal, &ulRec))
    {
      hb_errInternal(9304, "hb_cdxSortOut: memory structure corrupted.", nullptr, nullptr);
    }

    if (fUnique)
    {
      if (ulKey != 0 && hb_cdxValCompare(pSort->pTag, pSort->pLastKey, iLen, pKeyVal, iLen, CDX_CMP_EXACT) == 0)
      {
        continue;
      }
    }
#ifdef HB_CDX_DBGCODE_EXT
    if (ulKey != 0)
    {
      int i = hb_cdxValCompare(pSort->pTag, pSort->pLastKey, iLen, pKeyVal, iLen, CDX_CMP_EXACT);
      if (i == 0)
      {
        i = (pSort->ulLastRec < ulRec) ? -1 : 1;
      }
      if (i > 0)
      {
        fprintf(stderr, "\r\nulKey=%ld, pKeyVal=[%s][%ld], pKeyLast=[%s][%ld]\r\n", ulKey, pKeyVal, ulRec,
                pSort->pLastKey, pSort->ulLastRec);
        fflush(stderr);
        hb_errInternal(9305, "hb_cdxSortOut: sorting fails.", nullptr, nullptr);
      }
    }
#endif
    hb_cdxSortAddNodeKey(pSort, 0, pKeyVal, ulRec, 0);
    memcpy(pSort->pLastKey, pKeyVal, iLen);
    pSort->ulLastRec = ulRec;
  }

#ifdef HB_CDX_DBGCODE
  if (hb_cdxSortKeyGet(pSort, &pKeyVal, &ulRec))
  {
    hb_errInternal(9306, "hb_cdxSortOut: memory structure corrupted(2).", nullptr, nullptr);
  }
#endif

  if (pSort->NodeList[0] == nullptr)
  {
    pSort->NodeList[0] = hb_cdxPageNew(pSort->pTag, nullptr, 0);
    pSort->NodeList[0]->PageType = CDX_NODE_LEAF;
    hb_cdxPageLeafInitSpace(pSort->NodeList[0]);
  }

  iLevel = 0;
  fNext = true;
  do
  {
    if (iLevel + 1 == CDX_STACKSIZE || pSort->NodeList[iLevel + 1] == nullptr)
    {
      pSort->NodeList[iLevel]->PageType |= CDX_NODE_ROOT;
      pSort->pTag->RootBlock = pSort->NodeList[iLevel]->Page;
      fNext = false;
    }
    else
    {
      hb_cdxSortAddNodeKey(pSort, iLevel + 1, pSort->pLastKey, pSort->ulLastRec, pSort->NodeList[iLevel]->Page);
    }
    hb_cdxPageFree(pSort->NodeList[iLevel], true);
    iLevel++;
  } while (fNext);
}

static void hb_cdxTagEmptyIndex(LPCDXTAG pTag)
{
  pTag->RootPage = hb_cdxPageNew(pTag, nullptr, 0);
  pTag->RootBlock = pTag->RootPage->Page;
  pTag->RootPage->PageType = CDX_NODE_ROOT | CDX_NODE_LEAF;
  hb_cdxPageLeafInitSpace(pTag->RootPage);
}

static void hb_cdxTagDoIndex(LPCDXTAG pTag, bool fReindex)
{
  LPCDXAREA pArea = pTag->pIndex->pArea;
  LPCDXSORTINFO pSort;
  PHB_ITEM pWhileItem = nullptr, pEvalItem = nullptr;
  HB_ULONG ulRecCount, ulRecNo = pArea->dbfarea.ulRecNo;
  HB_LONG lStep = 0;
  auto cdpTmp = hb_cdpSelect(pArea->dbfarea.area.cdPage);

  if (pArea->dbfarea.area.lpdbOrdCondInfo)
  {
    pEvalItem = pArea->dbfarea.area.lpdbOrdCondInfo->itmCobEval;
    pWhileItem = pArea->dbfarea.area.lpdbOrdCondInfo->itmCobWhile;
    lStep = pArea->dbfarea.area.lpdbOrdCondInfo->lStep;
    if (pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount || pArea->dbfarea.area.lpdbOrdCondInfo->itmRecID ||
        pArea->dbfarea.area.lpdbOrdCondInfo->fRest || pArea->dbfarea.area.lpdbOrdCondInfo->fUseCurrent ||
        pArea->dbfarea.area.lpdbOrdCondInfo->fUseFilter)
    {
      pTag->Partial = true;
    }
  }

  if (pTag->Custom || (pTag->OptFlags & CDX_TYPE_STRUCTURE))
  {
    ulRecCount = 0;
  }
  else if (SELF_RECCOUNT(&pArea->dbfarea.area, &ulRecCount) != Harbour::SUCCESS)
  {
    return;
  }

  pArea->pSort = pSort = hb_cdxSortNew(pTag, ulRecCount);
  pSort->fReindex = fReindex;

#if defined(HB_SIXCDX)
  if ((pTag->OptFlags & CDX_TYPE_STRUCTURE) == 0 && pEvalItem)
  {
    SELF_GOTO(&pArea->dbfarea.area, 0);
    if (!hb_cdxEvalCond(pArea, pEvalItem, false))
    {
      hb_cdxSortFree(pSort);
      pArea->pSort = nullptr;
      SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
      return;
    }
  }
#endif
  if (ulRecCount == 0)
  {
    hb_cdxTagEmptyIndex(pTag);
  }
  else
  {
    HB_USHORT uiSaveTag = pArea->uiTag;
    HB_ULONG ulStartRec = 0, ulNextCount = 0;
    auto fDirectRead = false;
    auto fUseFilter = false;
    HB_BYTE *pSaveRecBuff = pArea->dbfarea.pRecord, cTemp[8];
    int iRecBuff = 0, iRecBufSize, iRec;
    PHB_ITEM pForItem, pItem = nullptr;

    pForItem = pTag->pForItem;
    if (pTag->nField)
    {
      pItem = hb_itemNew(nullptr);
    }

    if (!pArea->dbfarea.area.lpdbOrdCondInfo || pArea->dbfarea.area.lpdbOrdCondInfo->fAll)
    {
      pArea->uiTag = 0;
    }
    else
    {
      if (pArea->dbfarea.area.lpdbOrdCondInfo->itmRecID)
      {
        ulStartRec = hb_itemGetNL(pArea->dbfarea.area.lpdbOrdCondInfo->itmRecID);
      }
      if (ulStartRec)
      {
        ulNextCount = 1;
      }
      else if (pArea->dbfarea.area.lpdbOrdCondInfo->fRest || pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount > 0)
      {
        if (pArea->dbfarea.area.lpdbOrdCondInfo->itmStartRecID)
        {
          ulStartRec = hb_itemGetNL(pArea->dbfarea.area.lpdbOrdCondInfo->itmStartRecID);
        }
        if (!ulStartRec)
        {
          ulStartRec = ulRecNo;
        }
        if (pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount > 0)
        {
          ulNextCount = pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount;
        }
      }
      else if (pArea->dbfarea.area.lpdbOrdCondInfo->fUseFilter)
      {
        fUseFilter = true;
      }
      else if (!pArea->dbfarea.area.lpdbOrdCondInfo->fUseCurrent)
      {
        pArea->uiTag = 0;
      }
      else if (pArea->uiTag != 0)
      {
        LPCDXTAG pCurrTag = hb_cdxGetActiveTag(pArea);
        if (pCurrTag)
        {
          hb_cdxIndexLockRead(pCurrTag->pIndex);
          hb_cdxTagRefreshScope(pCurrTag);
          hb_cdxTagGoTop(pCurrTag);
          ulStartRec = pCurrTag->CurKey->rec;
          hb_cdxIndexUnLockRead(pCurrTag->pIndex);
        }
      }
    }

    iRecBufSize = (USHRT_MAX + 1) / pArea->dbfarea.uiRecordLen;
    fDirectRead = !hb_setGetStrictRead() && iRecBufSize > 1 && // !pArea->dbfarea.area.lpdbRelations &&
                  (!pArea->dbfarea.area.lpdbOrdCondInfo || pArea->dbfarea.area.lpdbOrdCondInfo->fAll ||
                   (pArea->uiTag == 0 && !fUseFilter));

    if (fDirectRead)
    {
      pSort->pRecBuff = static_cast<HB_BYTE *>(hb_xgrab(pArea->dbfarea.uiRecordLen * iRecBufSize));
    }

    if (ulStartRec == 0 && pArea->uiTag == 0)
    {
      ulStartRec = 1;
    }

    if (ulStartRec == 0)
    {
      SELF_GOTOP(&pArea->dbfarea.area);
    }
    else
    {
      SELF_GOTO(&pArea->dbfarea.area, ulStartRec);
      if (fUseFilter)
      {
        SELF_SKIPFILTER(&pArea->dbfarea.area, 1);
      }
    }

    ulRecNo = pArea->dbfarea.ulRecNo;

    while (!pArea->dbfarea.area.fEof)
    {
      if (fDirectRead)
      {
        if (ulRecNo > ulRecCount)
        {
          break;
        }
        if (iRecBuff == 0 || iRecBuff >= iRecBufSize)
        {
          HB_SIZE nSize;

          if (ulRecCount - ulRecNo >= static_cast<HB_ULONG>(iRecBufSize))
          {
            iRec = iRecBufSize;
          }
          else
          {
            iRec = ulRecCount - ulRecNo + 1;
          }
          if (ulNextCount > 0 && ulNextCount < static_cast<HB_ULONG>(iRec))
          {
            iRec = static_cast<int>(ulNextCount);
          }
          nSize = static_cast<HB_SIZE>(iRec) * pArea->dbfarea.uiRecordLen;
          if (hb_fileReadAt(pArea->dbfarea.pDataFile, pSort->pRecBuff, nSize,
                            static_cast<HB_FOFFSET>(pArea->dbfarea.uiHeaderLen) +
                                static_cast<HB_FOFFSET>(ulRecNo - 1) *
                                    static_cast<HB_FOFFSET>(pArea->dbfarea.uiRecordLen)) != nSize)
          {
            hb_cdxErrorRT(pTag->pIndex->pArea, EG_READ, EDBF_READ, pTag->pIndex->szFileName, hb_fsError(), 0, nullptr);
            break;
          }
          iRecBuff = 0;
        }
        pArea->dbfarea.pRecord = pSort->pRecBuff + iRecBuff * pArea->dbfarea.uiRecordLen;
        pArea->dbfarea.ulRecNo = ulRecNo;
        if (SELF_GETREC(&pArea->dbfarea.area, nullptr) == Harbour::FAILURE)
        {
          break;
        }
        pArea->dbfarea.fValidBuffer = pArea->dbfarea.fPositioned = true;
        pArea->dbfarea.fDeleted = pArea->dbfarea.pRecord[0] == '*';
        // Force relational movement in child WorkAreas
        if (pArea->dbfarea.area.lpdbRelations)
        {
          if (SELF_SYNCCHILDREN(&pArea->dbfarea.area) == Harbour::FAILURE)
          {
            break;
          }
        }
        iRecBuff++;
      }

#if !defined(HB_SIXCDX)
      if (pEvalItem)
      {
        if (lStep >= pArea->dbfarea.area.lpdbOrdCondInfo->lStep)
        {
          lStep = 0;
          if (!hb_cdxEvalCond(pArea, pEvalItem, false))
          {
            break;
          }
        }
        ++lStep;
      }
#endif

      if (pWhileItem && !hb_cdxEvalCond(nullptr, pWhileItem, false))
      {
        break;
      }

      if (ulRecNo <= ulRecCount && (pForItem == nullptr || hb_cdxEvalCond(pArea, pForItem, false)))
      {
        double d;

        if (pTag->nField)
        {
          SELF_GETVALUE(&pArea->dbfarea.area, pTag->nField, pItem);
        }
        else
        {
          pItem = hb_vmEvalBlockOrMacro(pTag->pKeyItem);
        }

        switch (hb_itemType(pItem))
        {
        case Harbour::Item::STRING:
        case Harbour::Item::MEMO:
          hb_cdxSortKeyAdd(pSort, pArea->dbfarea.ulRecNo, reinterpret_cast<const HB_BYTE *>(pItem->getCPtr()),
                           static_cast<int>(pItem->getCLen()));
          break;

        case Harbour::Item::INTEGER:
        case Harbour::Item::LONG:
        case Harbour::Item::DOUBLE:
          if (pTag->uiLen == 4)
          {
            HB_U32 uiVal = static_cast<HB_U32>(pItem->getNI()) + 0x80000000;
            HB_PUT_BE_UINT32(&cTemp[0], uiVal);
            hb_cdxSortKeyAdd(pSort, pArea->dbfarea.ulRecNo, cTemp, 4);
          }
          else
          {
            d = pItem->getND();
            HB_DBL2ORD(&d, &cTemp[0]);
            hb_cdxSortKeyAdd(pSort, pArea->dbfarea.ulRecNo, cTemp, 8);
          }
          break;

        case Harbour::Item::DATE:
          d = static_cast<double>(pItem->getDL());
          HB_DBL2ORD(&d, &cTemp[0]);
          hb_cdxSortKeyAdd(pSort, pArea->dbfarea.ulRecNo, cTemp, 8);
          break;

        case Harbour::Item::TIMESTAMP:
          d = pItem->getTD();
          HB_DBL2ORD(&d, &cTemp[0]);
          hb_cdxSortKeyAdd(pSort, pArea->dbfarea.ulRecNo, cTemp, 8);
          break;

        case Harbour::Item::LOGICAL:
          cTemp[0] = static_cast<HB_BYTE>(pItem->getL() ? 'T' : 'F');
          hb_cdxSortKeyAdd(pSort, pArea->dbfarea.ulRecNo, cTemp, 1);
          break;

        default:
          hb_cdxErrorRT(pArea, EG_DATATYPE, EDBF_INVALIDKEY, nullptr, 0, 0, nullptr);
          pEvalItem = nullptr;
          ulNextCount = 1;
          break;
        }
      }

      if (ulNextCount > 0)
      {
        if (--ulNextCount == 0)
        {
          break;
        }
      }

#if defined(HB_SIXCDX)
      if (pEvalItem)
      {
        if (lStep >= pArea->dbfarea.area.lpdbOrdCondInfo->lStep)
        {
          lStep = 0;
          if (!hb_cdxEvalCond(pArea, pEvalItem, false))
          {
            break;
          }
        }
        ++lStep;
      }
#endif

      if (fDirectRead)
      {
        ulRecNo++;
      }
      else
      {
        if (SELF_SKIPRAW(&pArea->dbfarea.area, 1) == Harbour::FAILURE)
        {
          break;
        }
        if (fUseFilter && SELF_SKIPFILTER(&pArea->dbfarea.area, 1) == Harbour::FAILURE)
        {
          break;
        }
        ulRecNo = pArea->dbfarea.ulRecNo;
      }
    }

    hb_cdxSortOut(pSort);
    if (pTag->nField)
    {
      hb_itemRelease(pItem);
    }

    if (fDirectRead)
    {
      pArea->dbfarea.pRecord = pSaveRecBuff;
      SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
    }
    pArea->uiTag = uiSaveTag;

#if !defined(HB_SIXCDX)
    if (pEvalItem && lStep)
    {
#if 0
         pArea->dbfarea.area.fEof = true;
#endif
      hb_cdxEvalCond(pArea, pEvalItem, false);
    }
#endif
  }

#if defined(HB_SIXCDX)
  if (pEvalItem)
  {
    SELF_GOTO(&pArea->dbfarea.area, 0);
    pArea->dbfarea.area.fBof = false;
    hb_cdxEvalCond(pArea, pEvalItem, false);
  }
#endif

  hb_cdxSortFree(pSort);
  pArea->pSort = nullptr;

  hb_cdpSelect(cdpTmp);
}

static const RDDFUNCS cdxTable = {
    // Movement and positioning methods

    (DBENTRYP_BP) nullptr,                             // hb_cdxBof
    (DBENTRYP_BP) nullptr,                             // hb_cdxEof
    (DBENTRYP_BP) nullptr,                             // hb_cdxFound
    (DBENTRYP_V)hb_cdxGoBottom, (DBENTRYP_UL) nullptr, // hb_cdxGoTo
    (DBENTRYP_I) nullptr,                              // hb_cdxGoToId
    (DBENTRYP_V)hb_cdxGoTop, (DBENTRYP_BIB)hb_cdxSeek, (DBENTRYP_L)hb_cdxSkip,
    (DBENTRYP_L) nullptr, // hb_cdxSkipFilter
    (DBENTRYP_L)hb_cdxSkipRaw,

    // Data management

    (DBENTRYP_VF) nullptr,                                                   // hb_cdxAddField
    (DBENTRYP_B) nullptr,                                                    // hb_cdxAppend
    (DBENTRYP_I) nullptr,                                                    // hb_cdxCreateFields
    (DBENTRYP_V) nullptr,                                                    // hb_cdxDeleteRec
    (DBENTRYP_BP) nullptr,                                                   // hb_cdxDeleted
    (DBENTRYP_SP) nullptr,                                                   // hb_cdxFieldCount
    (DBENTRYP_VF) nullptr,                                                   // hb_cdxFieldDisplay
    (DBENTRYP_SSI) nullptr,                                                  // hb_cdxFieldInfo
    (DBENTRYP_SCP) nullptr,                                                  // hb_cdxFieldName
    (DBENTRYP_V)hb_cdxFlush, (DBENTRYP_PP) nullptr,                          // hb_cdxGetRec
    (DBENTRYP_SI) nullptr,                                                   // hb_cdxGetValue
    (DBENTRYP_SVL) nullptr,                                                  // hb_cdxGetVarLen
    (DBENTRYP_V)hb_cdxGoCold, (DBENTRYP_V)hb_cdxGoHot, (DBENTRYP_P) nullptr, // hb_cdxPutRec
    (DBENTRYP_SI) nullptr,                                                   // hb_cdxPutValue
    (DBENTRYP_V) nullptr,                                                    // hb_cdxRecall
    (DBENTRYP_ULP) nullptr,                                                  // hb_cdxRecCount
    (DBENTRYP_ISI) nullptr,                                                  // hb_cdxRecInfo
    (DBENTRYP_ULP) nullptr,                                                  // hb_cdxRecNo
    (DBENTRYP_I) nullptr,                                                    // hb_cdxRecId
    (DBENTRYP_S) nullptr,                                                    // hb_cdxSetFieldExtent

    // WorkArea/Database management

    (DBENTRYP_CP) nullptr,                                // hb_cdxAlias
    (DBENTRYP_V)hb_cdxClose, (DBENTRYP_VO) nullptr,       // hb_cdxCreate
    (DBENTRYP_SI) nullptr,                                // hb_cdxInfo
    (DBENTRYP_V) nullptr,                                 // hb_cdxNewArea
    (DBENTRYP_VO)hb_cdxOpen, (DBENTRYP_V) nullptr,        // hb_cdxRelease
    (DBENTRYP_SP)hb_cdxStructSize, (DBENTRYP_CP) nullptr, // hb_cdxSysName
    (DBENTRYP_VEI) nullptr,                               // hb_cdxEval
    (DBENTRYP_V)hb_cdxPack, (DBENTRYP_LSP) nullptr,       // hb_cdxPackRec
    (DBENTRYP_VS) nullptr,                                // hb_cdxSort
    (DBENTRYP_VT) nullptr,                                // hb_cdxTrans
    (DBENTRYP_VT) nullptr,                                // hb_cdxTransRec
    (DBENTRYP_V)hb_cdxZap,

    // Relational Methods

    (DBENTRYP_VR) nullptr,  // hb_cdxChildEnd
    (DBENTRYP_VR) nullptr,  // hb_cdxChildStart
    (DBENTRYP_VR) nullptr,  // hb_cdxChildSync
    (DBENTRYP_V) nullptr,   // hb_cdxSyncChildren
    (DBENTRYP_V) nullptr,   // hb_cdxClearRel
    (DBENTRYP_V) nullptr,   // hb_cdxForceRel
    (DBENTRYP_SSP) nullptr, // hb_cdxRelArea
    (DBENTRYP_VR) nullptr,  // hb_cdxRelEval
    (DBENTRYP_SI) nullptr,  // hb_cdxRelText
    (DBENTRYP_VR) nullptr,  // hb_cdxSetRel

    // Order Management

    (DBENTRYP_VOI)hb_cdxOrderListAdd, (DBENTRYP_V)hb_cdxOrderListClear, (DBENTRYP_VOI)hb_cdxOrderListDelete,
    (DBENTRYP_VOI)hb_cdxOrderListFocus, (DBENTRYP_V)hb_cdxOrderListRebuild,
    (DBENTRYP_VOO) nullptr, // hb_cdxOrderCondition
    (DBENTRYP_VOC)hb_cdxOrderCreate, (DBENTRYP_VOI)hb_cdxOrderDestroy, (DBENTRYP_SVOI)hb_cdxOrderInfo,

    // Filters and Scope Settings

    (DBENTRYP_V)hb_cdxClearFilter, (DBENTRYP_V) nullptr,   // hb_cdxClearLocate
    (DBENTRYP_V) nullptr,                                  // hb_cdxClearScope
    (DBENTRYP_VPLP)hb_cdxCountScope, (DBENTRYP_I) nullptr, // hb_cdxFilterText
    (DBENTRYP_SI) nullptr,                                 // hb_cdxScopeInfo
    (DBENTRYP_VFI)hb_cdxSetFilter, (DBENTRYP_VLO) nullptr, // hb_cdxSetLocate
    (DBENTRYP_VOS) nullptr,                                // hb_cdxSetScope
    (DBENTRYP_VPL) nullptr,                                // hb_cdxSkipScope
    (DBENTRYP_B) nullptr,                                  // hb_cdxLocate

    // Miscellaneous

    (DBENTRYP_CC) nullptr, // hb_cdxCompile
    (DBENTRYP_I) nullptr,  // hb_cdxError
    (DBENTRYP_I) nullptr,  // hb_cdxEvalBlock

    // Network operations

    (DBENTRYP_VSP) nullptr, // hb_cdxRawLock
    (DBENTRYP_VL) nullptr,  // hb_cdxLock
    (DBENTRYP_I) nullptr,   // hb_cdxUnLock

    // Memofile functions

    (DBENTRYP_V) nullptr,    // hb_cdxCloseMemFile
    (DBENTRYP_VO) nullptr,   // hb_cdxCreateMemFile
    (DBENTRYP_SCCS) nullptr, // hb_cdxGetValueFile
    (DBENTRYP_VO) nullptr,   // hb_cdxOpenMemFile
    (DBENTRYP_SCCS) nullptr, // hb_cdxPutValueFile

    // Database file header handling

    (DBENTRYP_V) nullptr, // hb_cdxReadDBHeader
    (DBENTRYP_V) nullptr, // hb_cdxWriteDBHeader

    // non WorkArea functions

    (DBENTRYP_R) nullptr,     // hb_cdxInit
    (DBENTRYP_R) nullptr,     // hb_cdxExit
    (DBENTRYP_RVVL) nullptr,  // hb_cdxDrop
    (DBENTRYP_RVVL) nullptr,  // hb_cdxExists
    (DBENTRYP_RVVVL) nullptr, // hb_cdxRename
    (DBENTRYP_RSLV)hb_cdxRddInfo,

    // Special and reserved methods

    (DBENTRYP_SVP) nullptr // hb_cdxWhoCares
};

#if defined(HB_SIXCDX)
#define HB_CDXRDD "SIXCDX"
#else
#define HB_CDXRDD "DBFCDX"
#endif

HB_FUNC_STATIC(_GETFUNCTABLE)
{
  auto puiCount = static_cast<HB_USHORT *>(hb_parptr(1));
  auto pTable = static_cast<RDDFUNCS *>(hb_parptr(2));
  auto uiRddId = static_cast<HB_USHORT>(hb_parni(4));
  auto puiSuperRddId = static_cast<HB_USHORT *>(hb_parptr(5));

#if 0
   HB_TRACE(HB_TR_DEBUG, (HB_CDXRDD "_GETFUNCTABLE(%p, %p)", static_cast<void*>(puiCount), static_cast<void*>(pTable)));
#endif

  if (pTable)
  {
    if (puiCount)
    {
      *puiCount = RDDFUNCSCOUNT;
    }
    HB_ERRCODE errCode = hb_rddInheritEx(pTable, &cdxTable, &cdxSuper, "DBFFPT", puiSuperRddId);
    if (errCode != Harbour::SUCCESS)
    {
      errCode = hb_rddInheritEx(pTable, &cdxTable, &cdxSuper, "DBFDBT", puiSuperRddId);
    }
    if (errCode != Harbour::SUCCESS)
    {
      errCode = hb_rddInheritEx(pTable, &cdxTable, &cdxSuper, "DBF", puiSuperRddId);
    }
    if (errCode == Harbour::SUCCESS)
    {
      // we successfully register our RDD so now we can initialize it
      // You may think that this place is RDD init statement, Druzus
      s_uiRddId = uiRddId;
    }
    hb_retni(errCode);
  }
  else
  {
    hb_retni(Harbour::FAILURE);
  }
}

static void hb_cdxRddInit(void *cargo)
{
  HB_SYMBOL_UNUSED(cargo);

  if (hb_rddRegister("DBF", RDT_FULL) <= 1)
  {
    hb_rddRegister("DBFFPT", RDT_FULL);
    if (hb_rddRegister(HB_CDXRDD, RDT_FULL) <= 1)
    {
      return;
    }
  }

  hb_errInternal(HB_EI_RDDINVALID, nullptr, nullptr, nullptr);
}

#if defined(HB_SIXCDX)

HB_FUNC_TRANSLATE(SIXCDX, _DBF)

HB_INIT_SYMBOLS_BEGIN(_hb_sixcdx1_InitSymbols_){"SIXCDX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME(SIXCDX)}, nullptr},
    {"SIXCDX_GETFUNCTABLE",
     {HB_FS_PUBLIC | HB_FS_LOCAL},
     {HB_FUNCNAME(_GETFUNCTABLE)},
     nullptr} HB_INIT_SYMBOLS_END(_hb_sixcdx1_InitSymbols_)

        HB_CALL_ON_STARTUP_BEGIN(_hb_sixcdx_rdd_init_) hb_vmAtInit(hb_cdxRddInit, nullptr);
HB_CALL_ON_STARTUP_END(_hb_sixcdx_rdd_init_)

#if defined(HB_PRAGMA_STARTUP)
#pragma startup _hb_sixcdx1_InitSymbols_
#pragma startup _hb_sixcdx_rdd_init_
#elif defined(HB_DATASEG_STARTUP)
#define HB_DATASEG_BODY                                                                                                \
  HB_DATASEG_FUNC(_hb_sixcdx1_InitSymbols_)                                                                            \
  HB_DATASEG_FUNC(_hb_sixcdx_rdd_init_)
#include "hbiniseg.h"
#endif

#else

HB_FUNC_TRANSLATE(DBFCDX, _DBF)

HB_INIT_SYMBOLS_BEGIN(_hb_dbfcdx1_InitSymbols_){"DBFCDX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME(DBFCDX)}, nullptr},
    {"DBFCDX_GETFUNCTABLE",
     {HB_FS_PUBLIC | HB_FS_LOCAL},
     {HB_FUNCNAME(_GETFUNCTABLE)},
     nullptr} HB_INIT_SYMBOLS_END(_hb_dbfcdx1_InitSymbols_)

        HB_CALL_ON_STARTUP_BEGIN(_hb_dbfcdx_rdd_init_) hb_vmAtInit(hb_cdxRddInit, nullptr);
HB_CALL_ON_STARTUP_END(_hb_dbfcdx_rdd_init_)

#if defined(HB_PRAGMA_STARTUP)
#pragma startup _hb_dbfcdx1_InitSymbols_
#pragma startup _hb_dbfcdx_rdd_init_
#elif defined(HB_DATASEG_STARTUP)
#define HB_DATASEG_BODY                                                                                                \
  HB_DATASEG_FUNC(_hb_dbfcdx1_InitSymbols_)                                                                            \
  HB_DATASEG_FUNC(_hb_dbfcdx_rdd_init_)
#include "hbiniseg.hpp"
#endif

#endif
