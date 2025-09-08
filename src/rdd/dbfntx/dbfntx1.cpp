//
// DBFNTX RDD
//
// Copyright 1999 Bruno Cantero <bruno@issnet.net>
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
 * The following functions are added by
 *       Alexander Kresin <alex@belacy.belgorod.su>
 *
 * commonError()
 * hb_IncString()
 * ntxNumToStr()
 * checkLogicalExpr()
 * hb__ntxTagKeyCount()
 * hb_ntxInTopScope()
 * hb_ntxInBottomScope()
 * hb_ntxTagKeyNo()
 * hb_ntxTagKeyCount()
 * hb_ntxClearScope()
 * hb_ntxGoEof()
 * hb_ntxGetKeyType()
 * hb_ntxTagKeyFind()
 * hb_ntxPageKeySearch()
 * hb_ntxTagFindCurrentKey()
 * hb_ntxIsRecBad()
 * hb_ntxPageFindCurrentKey()
 * hb_ntxGetCurrentKey()
 * hb_ntxTagGoToNextKey()
 * hb_ntxTagGoToPrevKey()
 * hb_ntxTagGoToTopKey()
 * hb_ntxTagGoToBottomKey()
 * hb_ntxTagKeyGoTo()
 * hb_ntxPageRelease()
 * hb_ntxKeysMove()
 * hb_ntxPageSplit()
 * hb_ntxPageJoin()
 * hb_ntxPageBalance()
 * hb_ntxTagBalance()
 * hb_ntxPageKeyDel()
 * hb_ntxTagKeyAdd()
 * hb_ntxSwapPageSave()
 * hb_ntxKeysSort()
 * hb_ntxSortKeyAdd()
 * hb_ntxSortKeyEnd()
 * hb_ntxWritePage()
 * hb_ntxRootPage()
 * hb_ntxGetSortedKey()
 * hb_ntxBufferSave()
 * hb_ntxReadBuf()
 * hb_ntxPageFind()
 * ntxFindIndex()
 * hb_ntxOrdKeyAdd()
 * hb_ntxOrdKeyDel()
 * ntxGoBottom()
 * ntxGoTo()
 * ntxGoTop()
 * ntxSeek()
 * ntxSkipRaw()
 * ntxGoCold()
 * ntxGoHot()
 * ntxSysName()
 * ntxPack()
 * ntxZap()
 * ntxClearScope()
 * ntxScopeInfo()
 * ntxOrderListAdd()
 * ntxOrderListClear()
 * ntxOrderListFocus()
 * ntxOrderListRebuild()
 * ntxSetScope()
 */

/*
 * Copyright 2005 Przemyslaw Czerpak <druzus@priv.onet.pl>
 * in practice most of the code rewritten
 */

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

/* #define HB_NTX_NOMULTITAG */

/* #define HB_NTX_EXTERNAL_PAGEBUFFER */

#define HB_NTX_STRONG_BALANCE

/*
#define HB_NTX_DEBUG
#define HB_NTX_DEBUG_EXT
#define HB_NTX_DEBUG_DISP
*/

#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbinit.hpp"
#include "hbapierr.hpp"
#include "hbapilng.hpp"
#include "hbvm.hpp"
#include "hbset.hpp"
#include "hbstack.hpp"
#include "hbmath.hpp"
#include "hbrddntx.hpp"
#include "rddsys.ch"
#include "hbregex.hpp"
#include "hbapicdp.hpp"

#ifdef HB_NTX_DEBUG_DISP
static HB_ULONG s_rdNO = 0;
static HB_ULONG s_wrNO = 0;
#endif

static RDDFUNCS ntxSuper;
static HB_USHORT s_uiRddId;

/* temporary casts to suppress 32/64-bit Windows warnings */
#define HB_SHORTCAST HB_SHORT
#define HB_USHORTCAST HB_USHORT
#define HB_INTCAST int

#define hb_ntxKeyFree(K) hb_xfree(K)
#define hb_ntxFileOffset(I, B) (static_cast<HB_FOFFSET>(B) << ((I)->LargeFile ? NTXBLOCKBITS : 0))
#define hb_ntxPageBuffer(p) ((p)->buffer)

/*
 * The helper functions (endian dependent) - on big endian machines
 * or RISC with strict alignment it's much better to use functions
 * then macros to inform compiler that can count complex parameters
 * only once.
 * On other machines it should not cause noticeable differences because
 * most of modern C compilers auto inline small functions
 */
#if defined(HB_LITTLE_ENDIAN) && !defined(HB_STRICT_ALIGNMENT)

#define hb_ntxGetKeyCount(p) HB_GET_LE_UINT16(hb_ntxPageBuffer(p))
#define hb_ntxSetKeyCount(p, n) HB_PUT_LE_UINT16(hb_ntxPageBuffer(p), (n))

#define hb_ntxGetKeyOffset(p, n) HB_GET_LE_UINT16(hb_ntxPageBuffer(p) + 2 + ((n) << 1))
#define hb_ntxGetKeyPtr(p, n) (hb_ntxPageBuffer(p) + hb_ntxGetKeyOffset(p, n))
#define hb_ntxGetKeyPage(p, n) HB_GET_LE_UINT32(hb_ntxGetKeyPtr(p, n))
#define hb_ntxGetKeyRec(p, n) HB_GET_LE_UINT32(hb_ntxGetKeyPtr(p, n) + 4)
#define hb_ntxGetKeyVal(p, n) (hb_ntxGetKeyPtr(p, n) + 8)

#define hb_ntxSetKeyOffset(p, n, u) HB_PUT_LE_UINT16(hb_ntxPageBuffer(p) + 2 + ((n) << 1), u)
#define hb_ntxSetKeyPage(p, n, l) HB_PUT_LE_UINT32(hb_ntxGetKeyPtr(p, n), l)
#define hb_ntxSetKeyRec(p, n, l) HB_PUT_LE_UINT32(hb_ntxGetKeyPtr(p, n) + 4, l)

#else

static HB_USHORT hb_ntxGetKeyCount(LPPAGEINFO pPage)
{
  const char *ptr = hb_ntxPageBuffer(pPage);

  return HB_GET_LE_UINT16(ptr);
}

static void hb_ntxSetKeyCount(LPPAGEINFO pPage, HB_USHORT uiKeys)
{
  char *ptr = hb_ntxPageBuffer(pPage);

  HB_PUT_LE_UINT16(ptr, uiKeys);
}

static HB_USHORT hb_ntxGetKeyOffset(LPPAGEINFO pPage, HB_SHORT iKey)
{
  const char *ptr = hb_ntxPageBuffer(pPage) + 2 + (iKey << 1);

  return HB_GET_LE_UINT16(ptr);
}

static void hb_ntxSetKeyOffset(LPPAGEINFO pPage, HB_SHORT iKey, HB_USHORT uiOffset)
{
  char *ptr = hb_ntxPageBuffer(pPage) + 2 + (iKey << 1);

  HB_PUT_LE_UINT16(ptr, uiOffset);
}

static char *hb_ntxGetKeyPtr(LPPAGEINFO pPage, HB_SHORT iKey)
{
  return hb_ntxPageBuffer(pPage) + hb_ntxGetKeyOffset(pPage, iKey);
}

static HB_ULONG hb_ntxGetKeyPage(LPPAGEINFO pPage, HB_SHORT iKey)
{
  const char *ptr = hb_ntxGetKeyPtr(pPage, iKey);

  return HB_GET_LE_UINT32(ptr);
}

static void hb_ntxSetKeyPage(LPPAGEINFO pPage, HB_SHORT iKey, HB_ULONG ulPage)
{
  char *ptr = hb_ntxGetKeyPtr(pPage, iKey);

  HB_PUT_LE_UINT32(ptr, ulPage);
}

static char *hb_ntxGetKeyVal(LPPAGEINFO pPage, HB_SHORT iKey)
{
  return hb_ntxGetKeyPtr(pPage, iKey) + 8;
}

static void hb_ntxSetKeyRec(LPPAGEINFO pPage, HB_SHORT iKey, HB_ULONG ulRec)
{
  char *ptr = hb_ntxGetKeyPtr(pPage, iKey) + 4;

  HB_PUT_LE_UINT32(ptr, ulRec);
}

static HB_ULONG hb_ntxGetKeyRec(LPPAGEINFO pPage, HB_SHORT iKey)
{
  const char *ptr = hb_ntxGetKeyPtr(pPage, iKey) + 4;

  return HB_GET_LE_UINT32(ptr);
}

#endif

/*
 * generate Run-Time error
 */
static HB_ERRCODE hb_ntxErrorRT(NTXAREAP pArea, HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char *szFileName,
                                HB_ERRCODE errOsCode, HB_USHORT uiFlags, PHB_ITEM *pErrorPtr)
{
  HB_ERRCODE iRet = Harbour::FAILURE;

  if (hb_vmRequestQuery() == 0) {
    PHB_ITEM pError;
    if (pErrorPtr) {
      if (!*pErrorPtr) {
        *pErrorPtr = hb_errNew();
      }
      pError = *pErrorPtr;
    } else {
      pError = hb_errNew();
    }
    hb_errPutGenCode(pError, errGenCode);
    hb_errPutSubCode(pError, errSubCode);
    hb_errPutOsCode(pError, errOsCode);
    hb_errPutDescription(pError, hb_langDGetErrorDesc(errGenCode));
    if (szFileName != nullptr) {
      hb_errPutFileName(pError, szFileName);
    }
    if (uiFlags) {
      hb_errPutFlags(pError, uiFlags);
    }
    iRet = SELF_ERROR(&pArea->dbfarea.area, pError);
    if (!pErrorPtr) {
      hb_errRelease(pError);
    }
  }
  return iRet;
}

/*
 * convert numeric item into NTX key value
 */
static char *hb_ntxNumToStr(PHB_ITEM pItem, char *szBuffer, HB_USHORT length, HB_USHORT dec)
{
  char *ptr = szBuffer;

  hb_itemStrBuf(szBuffer, pItem, length, dec);

  while (*ptr == ' ') {
    *ptr++ = '0';
  }

  if (*ptr == '-') {
    *ptr = '0';
    for (ptr = &szBuffer[0]; *ptr; ptr++) {
      if (*ptr >= '0' && *ptr <= '9') {
        *ptr = static_cast<char>('0' - (*ptr - '0') - 4);
        /*
         * I intentionally used the above formula to avoid problems on
         * non ASCII machines though many of other xHarbour codes is
         * hard coded to ASCII values and should be fixed. Druzus.
         */
      }
    }
  }

  return szBuffer;
}

/*
 * convert numeric NTX key value into item
 */
static PHB_ITEM hb_ntxStrToNum(PHB_ITEM pItem, const char *szKeyVal, HB_USHORT length, HB_USHORT dec)
{
  char szBuffer[NTX_MAX_KEY + 1];
  const char *ptr = szKeyVal;
  int iLen, iDec;
  HB_MAXINT lValue;
  double dValue;

  HB_SYMBOL_UNUSED(dec);

  if (*ptr == '0' - 4) { /* negative number */
    char *ptr2, c;
    ptr2 = szBuffer;
    while ((c = *ptr++) != 0) {
      if (c != '.') {
        c = '0' - (c - '0' + 4);
      }
      *ptr2++ = c;
    }
    szBuffer[0] = '-';
    *ptr2 = '\0';
    ptr = szBuffer;
  }
  if (hb_valStrnToNum(ptr, length, &lValue, &dValue, &iDec, &iLen)) {
    return hb_itemPutNDLen(pItem, dValue, iLen, iDec);
  } else {
    return hb_itemPutNIntLen(pItem, lValue, length);
  }
}

/*
 * create new index key
 */
static LPKEYINFO hb_ntxKeyNew(LPKEYINFO pKeyFrom, int keylen)
{
  auto pKey = static_cast<LPKEYINFO>(hb_xgrab(sizeof(KEYINFO) + keylen));
  if (pKeyFrom) {
    memcpy(pKey->key, pKeyFrom->key, keylen + 1);
    pKey->Tag = pKeyFrom->Tag;
    pKey->Xtra = pKeyFrom->Xtra;
  } else {
    pKey->key[keylen] = '\0';
    pKey->Tag = pKey->Xtra = 0;
  }
  return pKey;
}

/*
 * copy index key, if dst is null create new dst key else destroy dst
 */
static LPKEYINFO hb_ntxKeyCopy(LPKEYINFO pKeyDest, LPKEYINFO pKey, int keylen)
{
  if (!pKeyDest) {
    pKeyDest = hb_ntxKeyNew(nullptr, keylen);
  }

  memcpy(pKeyDest->key, pKey->key, keylen + 1);
  pKeyDest->Tag = pKey->Tag;
  pKeyDest->Xtra = pKey->Xtra;

  return pKeyDest;
}

/*
 * get ntx key type for given item
 */
static HB_BYTE hb_ntxItemType(PHB_ITEM pItem)
{
  switch (hb_itemType(pItem)) {
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

/*
 * convert key type to comparable type
 */
static HB_BYTE hb_ntxItemTypeCmp(HB_BYTE bType)
{
  return bType == 'T' ? 'D' : bType;
}

/*
 * store Item in index key
 * TODO: uiType check and generate RT error if necessary
 *       probably not here or we will have to add parameter
 *       for scope key evaluation
 */
static LPKEYINFO hb_ntxKeyPutItem(LPKEYINFO pKey, PHB_ITEM pItem, HB_ULONG ulRecNo, LPTAGINFO pTag, bool fTrans,
                                  HB_USHORT *puiLen)
{
  HB_SIZE len;

  if (!pKey) {
    pKey = hb_ntxKeyNew(nullptr, pTag->KeyLength);
  }

  if (puiLen) {
    *puiLen = pTag->KeyLength;
  }

  switch (hb_ntxItemType(pItem)) {
  case 'C':
    if (fTrans) {
      len = pTag->KeyLength;
      hb_cdpnDup2(pItem->getCPtr(), pItem->getCLen(), pKey->key, &len, hb_vmCDP(),
                  pTag->pIndex->pArea->dbfarea.area.cdPage);
    } else {
      len = pItem->getCLen();
      if (len > static_cast<HB_SIZE>(pTag->KeyLength)) {
        len = pTag->KeyLength;
      }
      memcpy(pKey->key, pItem->getCPtr(), len);
    }
    if (len < static_cast<HB_SIZE>(pTag->KeyLength)) {
      memset(pKey->key + len, ' ', pTag->KeyLength - len);
      if (puiLen) {
        *puiLen = static_cast<HB_USHORT>(len);
      }
    }
    pKey->key[pTag->KeyLength] = '\0';
    break;
  case 'N':
    hb_ntxNumToStr(pItem, pKey->key, pTag->KeyLength, pTag->KeyDec);
    break;
  case 'T':
    if (pTag->KeyType == 'T') {
      hb_itemGetTS(pItem, pKey->key);
      break;
    }
    /* fallthrough */
  case 'D':
    if (pTag->KeyLength < 8) {
      char szDate[9];
      pItem->getDS(szDate);
      memcpy(pKey->key, szDate, pTag->KeyLength);
    } else {
      pItem->getDS(pKey->key);
      if (pTag->KeyLength > 8) {
        memset(pKey->key + 8, '\0', pTag->KeyLength - 8);
        if (puiLen) {
          *puiLen = 8;
        }
      }
    }
    pKey->key[pTag->KeyLength] = '\0';
    break;
  case 'L':
    pKey->key[0] = (pItem->getL() ? 'T' : 'F');
    if (pTag->KeyLength > 1) {
      memset(pKey->key + 1, '\0', pTag->KeyLength - 1);
    }
    pKey->key[pTag->KeyLength] = '\0';
    break;
  default:
    memset(pKey->key, '\0', pTag->KeyLength + 1);
  }
  pKey->Xtra = ulRecNo;
  pKey->Tag = 0;

  return pKey;
}

/*
 * get Item from index key
 */
static PHB_ITEM hb_ntxKeyGetItem(PHB_ITEM pItem, LPKEYINFO pKey, LPTAGINFO pTag, bool fTrans)
{
  if (pKey) {
    switch (pTag->KeyType) {
    case 'C':
      if (fTrans) {
        HB_SIZE nLen = pTag->KeyLength;
        char *pszVal = hb_cdpnDup(pKey->key, &nLen, pTag->pIndex->pArea->dbfarea.area.cdPage, hb_vmCDP());
        pItem = hb_itemPutCLPtr(pItem, pszVal, nLen);
      } else {
        pItem = hb_itemPutCL(pItem, pKey->key, pTag->KeyLength);
      }
      break;
    case 'N':
      pItem = hb_ntxStrToNum(pItem, pKey->key, pTag->KeyLength, pTag->KeyDec);
      break;
    case 'D':
      pItem = hb_itemPutDS(pItem, pKey->key);
      break;
    case 'T':
      pItem = hb_itemPutTS(pItem, pKey->key);
      break;
    case 'L':
      pItem = hb_itemPutL(pItem, pKey->key[0] == 'T');
      break;
    default:
      if (pItem != nullptr) {
        hb_itemClear(pItem);
      } else {
        pItem = hb_itemNew(nullptr);
      }
    }
  } else if (pItem != nullptr) {
    hb_itemClear(pItem);
  } else {
    pItem = hb_itemNew(nullptr);
  }

  return pItem;
}

/*
 * evaluate conditional expression and return the logical result
 */
static bool hb_ntxEvalCond(NTXAREAP pArea, PHB_ITEM pCondItem, bool fSetWA)
{
  auto iCurrArea = 0;
  auto fRet = false;

  if (fSetWA) {
    iCurrArea = hb_rddGetCurrentWorkAreaNumber();
    if (iCurrArea != pArea->dbfarea.area.uiArea) {
      hb_rddSelectWorkAreaNumber(pArea->dbfarea.area.uiArea);
    } else {
      iCurrArea = 0;
    }
  }

  fRet = hb_vmEvalBlockOrMacro(pCondItem)->getL();

  if (iCurrArea) {
    hb_rddSelectWorkAreaNumber(iCurrArea);
  }

  return fRet;
}

/*
 * evaluate seek/skip block: {| key, rec | ... }
 */
static bool hb_ntxEvalSeekCond(LPTAGINFO pTag, PHB_ITEM pCondItem)
{
  auto fRet = false;
  PHB_ITEM pKeyVal;

  pKeyVal = hb_ntxKeyGetItem(nullptr, pTag->CurKeyInfo, pTag, true);
  auto pKeyRec = hb_itemPutNInt(nullptr, pTag->CurKeyInfo->Xtra);

  fRet = hb_vmEvalBlockV(pCondItem, 2, pKeyVal, pKeyRec)->getL();

  hb_itemRelease(pKeyVal);
  hb_itemRelease(pKeyRec);

  return fRet;
}

/*
 * get ITEM type of key expression
 */
static HB_BYTE hb_ntxGetKeyType(LPTAGINFO pTag)
{
  HB_BYTE bType;

  if (pTag->nField) {
    auto pItem = hb_itemNew(nullptr);
    SELF_GETVALUE(&pTag->pIndex->pArea->dbfarea.area, pTag->nField, pItem);
    bType = hb_ntxItemType(pItem);
    hb_itemRelease(pItem);
  } else {
    auto iCurrArea = hb_rddGetCurrentWorkAreaNumber();

    if (iCurrArea != pTag->pIndex->pArea->dbfarea.area.uiArea) {
      hb_rddSelectWorkAreaNumber(pTag->pIndex->pArea->dbfarea.area.uiArea);
    } else {
      iCurrArea = 0;
    }

    bType = hb_ntxItemType(hb_vmEvalBlockOrMacro(pTag->pKeyItem));

    if (iCurrArea) {
      hb_rddSelectWorkAreaNumber(iCurrArea);
    }
  }
  return bType;
}

/*
 * evaluate key expression and create new Key from the result
 */
static LPKEYINFO hb_ntxEvalKey(LPKEYINFO pKey, LPTAGINFO pTag)
{
  NTXAREAP pArea = pTag->pIndex->pArea;
  PHB_ITEM pItem;
  auto cdpTmp = hb_cdpSelect(pArea->dbfarea.area.cdPage);

  if (pTag->nField) {
    pItem = hb_itemNew(nullptr);
    SELF_GETVALUE(&pArea->dbfarea.area, pTag->nField, pItem);
    pKey = hb_ntxKeyPutItem(pKey, pItem, pArea->dbfarea.ulRecNo, pTag, false, nullptr);
    hb_itemRelease(pItem);
  } else {
    auto iCurrArea = hb_rddGetCurrentWorkAreaNumber();

    if (iCurrArea != pArea->dbfarea.area.uiArea) {
      hb_rddSelectWorkAreaNumber(pArea->dbfarea.area.uiArea);
    } else {
      iCurrArea = 0;
    }

    pItem = hb_vmEvalBlockOrMacro(pTag->pKeyItem);
    pKey = hb_ntxKeyPutItem(pKey, pItem, pArea->dbfarea.ulRecNo, pTag, false, nullptr);

    if (iCurrArea) {
      hb_rddSelectWorkAreaNumber(iCurrArea);
    }
  }

  hb_cdpSelect(cdpTmp);

  return pKey;
}

/*
 * compare two values using Tag conditions (len & type)
 */
static int hb_ntxValCompare(LPTAGINFO pTag, const char *val1, int len1, const char *val2, int len2, bool fExact)
{
  int iLimit, iResult = 0;

  iLimit = (len1 > len2) ? len2 : len1;

  if (pTag->KeyType == 'C') {
    if (iLimit > 0) {
      if (HB_CDP_ISBINSORT(pTag->pIndex->pArea->dbfarea.area.cdPage)) {
        iResult = memcmp(val1, val2, iLimit);
      } else {
        return -hb_cdpcmp(val2, static_cast<HB_SIZE>(len2), val1, static_cast<HB_SIZE>(len1),
                          pTag->pIndex->pArea->dbfarea.area.cdPage, 0);
      }
    }

    if (iResult == 0) {
      if (len1 > len2) {
        iResult = 1;
      } else if (len1 < len2 && fExact) {
        iResult = -1;
      }
    } else if (iResult > 0) {
      iResult = 1;
    } else {
      iResult = -1;
    }
  } else {
    if (iLimit <= 0 || (iResult = memcmp(val1, val2, iLimit)) == 0) {
      if (len1 > len2) {
        iResult = 1;
      } else if (len1 < len2 && fExact) {
        iResult = -1;
      }
    } else if (iResult > 0) {
      iResult = 1;
    } else {
      iResult = -1;
    }
  }
  return iResult;
}

/*
 * check if a given key is in top scope
 */
static bool hb_ntxInTopScope(LPTAGINFO pTag, const char *key)
{
  PHB_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->bottom : &pTag->top;

  if (pScope->scopeKeyLen) {
    int i = hb_ntxValCompare(pTag, pScope->scopeKey->key, pScope->scopeKeyLen, key, pTag->KeyLength, false);
    return pTag->fUsrDescend ? i >= 0 : i <= 0;
  } else {
    return true;
  }
}

/*
 * check if a given key is in bottom scope
 */
static bool hb_ntxInBottomScope(LPTAGINFO pTag, const char *key)
{
  PHB_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->top : &pTag->bottom;

  if (pScope->scopeKeyLen) {
    int i = hb_ntxValCompare(pTag, pScope->scopeKey->key, pScope->scopeKeyLen, key, pTag->KeyLength, false);
    return pTag->fUsrDescend ? i <= 0 : i >= 0;
  } else {
    return true;
  }
}

/*
 * check if a given key is in current scope
 */
static bool hb_ntxKeyInScope(LPTAGINFO pTag, LPKEYINFO pKey)
{
  return hb_ntxInTopScope(pTag, pKey->key) && hb_ntxInBottomScope(pTag, pKey->key);
}

/*
 * clear top or bottom scope
 */
static void hb_ntxTagClearScope(LPTAGINFO pTag, HB_USHORT nScope)
{
  NTXAREAP pArea = pTag->pIndex->pArea;
  PHB_NTXSCOPE pScope;

  /* resolve any pending scope relations first */
  if (pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped) {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  if (pTag->fUsrDescend) {
    nScope = (nScope == 0) ? 1 : 0;
  }

  pScope = (nScope == 0) ? &pTag->top : &pTag->bottom;

  if (pScope->scopeKey) {
    hb_ntxKeyFree(pScope->scopeKey);
    pScope->scopeKey = nullptr;
  }
  if (pScope->scopeItem) {
    hb_itemRelease(pScope->scopeItem);
    pScope->scopeItem = nullptr;
  }
  pScope->scopeKeyLen = 0;

  pTag->keyCount = 0;
}

/*
 * set top or bottom scope
 */
static void hb_ntxTagSetScope(LPTAGINFO pTag, HB_USHORT nScope, PHB_ITEM pItem)
{
  NTXAREAP pArea = pTag->pIndex->pArea;
  PHB_ITEM pScopeVal;

  /* resolve any pending scope relations first */
  if (pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped) {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  pScopeVal = (hb_itemType(pItem) & Harbour::Item::BLOCK) ? hb_vmEvalBlock(pItem) : pItem;

  if (hb_ntxItemTypeCmp(pTag->KeyType) == hb_ntxItemTypeCmp(hb_ntxItemType(pScopeVal))) {
    PHB_NTXSCOPE pScope;
    bool fTop = (nScope == 0);

    if (pTag->fUsrDescend) {
      fTop = !fTop;
    }

    pScope = fTop ? &pTag->top : &pTag->bottom;

    pScope->scopeKey =
        hb_ntxKeyPutItem(pScope->scopeKey, pScopeVal,
                         (fTop == static_cast<bool>(pTag->AscendKey)) ? NTX_IGNORE_REC_NUM : NTX_MAX_REC_NUM, pTag,
                         true, &pScope->scopeKeyLen);

    if (pScope->scopeItem == nullptr) {
      pScope->scopeItem = hb_itemNew(nullptr);
    }
    hb_itemCopy(pScope->scopeItem, pItem);

    pTag->keyCount = 0;
  } else {
    hb_ntxTagClearScope(pTag, nScope);
  }
}

/*
 * get top or bottom scope item
 */
static void hb_ntxTagGetScope(LPTAGINFO pTag, HB_USHORT nScope, PHB_ITEM pItem)
{
  NTXAREAP pArea = pTag->pIndex->pArea;
  PHB_NTXSCOPE pScope;

  /* resolve any pending scope relations first */
  if (pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped) {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  if (pTag->fUsrDescend) {
    nScope = (nScope == 0) ? 1 : 0;
  }

  pScope = (nScope == 0) ? &pTag->top : &pTag->bottom;

  if (pScope->scopeItem) {
    hb_itemCopy(pItem, pScope->scopeItem);
  } else {
    hb_itemClear(pItem);
  }
}

/*
 * refresh top and bottom scope value if set as codeblock
 */
static void hb_ntxTagRefreshScope(LPTAGINFO pTag)
{
  PHB_ITEM pItem;

  /* resolve any pending scope relations first */
  if (pTag->pIndex->pArea->dbfarea.lpdbPendingRel && pTag->pIndex->pArea->dbfarea.lpdbPendingRel->isScoped) {
    SELF_FORCEREL(&pTag->pIndex->pArea->dbfarea.area);
  }

  if (hb_itemType(pTag->top.scopeItem) & Harbour::Item::BLOCK) {
    pItem = hb_vmEvalBlock(pTag->top.scopeItem);
    pTag->top.scopeKey =
        hb_ntxKeyPutItem(pTag->top.scopeKey, pItem, pTag->top.scopeKey->Xtra, pTag, true, &pTag->top.scopeKeyLen);
  }
  if (hb_itemType(pTag->bottom.scopeItem) & Harbour::Item::BLOCK) {
    pItem = hb_vmEvalBlock(pTag->bottom.scopeItem);
    pTag->bottom.scopeKey = hb_ntxKeyPutItem(pTag->bottom.scopeKey, pItem, pTag->bottom.scopeKey->Xtra, pTag, true,
                                             &pTag->bottom.scopeKeyLen);
  }
}

/*
 * an interface for fast check record number in record filter
 */
static bool hb_ntxCheckRecordScope(NTXAREAP pArea, HB_ULONG ulRec)
{
  HB_LONG lRecNo = static_cast<HB_LONG>(ulRec);

  if (SELF_COUNTSCOPE(&pArea->dbfarea.area, nullptr, &lRecNo) == Harbour::SUCCESS && lRecNo == 0) {
    return false;
  }
  return true;
}

#ifdef HB_NTX_DEBUG
static void hb_ntxTagCheckBuffers(LPTAGINFO pTag)
{
  LPPAGEINFO pPage;

  if ((pTag->HdrChanged || pTag->pIndex->Changed) && !pTag->pIndex->lockWrite) {
    hb_errInternal(9301, "hb_ntxTagCheckBuffers: tag modified in unlocked index", nullptr, nullptr);
  }

  for (HB_ULONG i = 0; i < pTag->pIndex->ulPages; i++) {
    pPage = pTag->pIndex->pages[i];
    if (pPage->Changed && !pTag->pIndex->lockWrite) {
      hb_errInternal(9302, "hb_ntxTagCheckBuffers: page modified in unlocked index", nullptr, nullptr);
    }
    if (pPage->iUsed) {
      hb_errInternal(9303, "hb_ntxTagCheckBuffers: page still allocated", nullptr, nullptr);
    }
  }
}

static void hb_ntxPageCheckKeys(LPPAGEINFO pPage, LPTAGINFO pTag, int iPos, int iType)
{
  int i;

  for (HB_USHORT u = 1; u < pPage->uiKeys; u++) {
    i = hb_ntxValCompare(pTag, hb_ntxGetKeyVal(pPage, u - 1), pTag->KeyLength, hb_ntxGetKeyVal(pPage, u),
                         pTag->KeyLength, true);
    if (!pTag->AscendKey) {
      i = -i;
    }
    if (i > 0) {
      printf("\r\nuiKeys=%d(%d/%d), (%d)[%.*s]>(%d)[%.*s]", pPage->uiKeys, iPos, iType, u - 1, pTag->KeyLength,
             hb_ntxGetKeyVal(pPage, u - 1), u, pTag->KeyLength, hb_ntxGetKeyVal(pPage, u));
      fflush(stdout);
      hb_errInternal(9304, "hb_ntxPageCheckKeys: keys sorted wrong.", nullptr, nullptr);
    }
  }
}
#endif

/*
 * read a given block from index file
 */
static bool hb_ntxBlockRead(LPNTXINDEX pIndex, HB_ULONG ulBlock, void *buffer, int iSize)
{
  if (!pIndex->lockRead && !pIndex->lockWrite) {
    hb_errInternal(9103, "hb_ntxBlockRead on not locked index file.", nullptr, nullptr);
  }

#ifdef HB_NTX_DEBUG_DISP
  s_rdNO++;
#endif
  if (hb_fileReadAt(pIndex->DiskFile, buffer, iSize, hb_ntxFileOffset(pIndex, ulBlock)) !=
      static_cast<HB_SIZE>(iSize)) {
    hb_ntxErrorRT(pIndex->pArea, EG_READ, EDBF_READ, pIndex->IndexName, hb_fsError(), 0, nullptr);
    return false;
  }
  return true;
}

/*
 * write a given block into index file
 */
static bool hb_ntxBlockWrite(LPNTXINDEX pIndex, HB_ULONG ulBlock, const void *buffer, int iSize)
{
  if (!pIndex->lockWrite) {
    hb_errInternal(9102, "hb_ntxBlockWrite on not locked index file.", nullptr, nullptr);
  }

#ifdef HB_NTX_DEBUG_DISP
  s_wrNO++;
#endif
  if (hb_fileWriteAt(pIndex->DiskFile, buffer, iSize, hb_ntxFileOffset(pIndex, ulBlock)) !=
      static_cast<HB_SIZE>(iSize)) {
    hb_ntxErrorRT(pIndex->pArea, EG_WRITE, EDBF_WRITE, pIndex->IndexName, hb_fsError(), 0, nullptr);
    return false;
  }
  return true;
}

/*
 * write a given tag page to file
 */
static bool hb_ntxPageSave(LPNTXINDEX pIndex, LPPAGEINFO pPage)
{
  hb_ntxSetKeyCount(pPage, pPage->uiKeys);
  if (!hb_ntxBlockWrite(pIndex, pPage->Page, hb_ntxPageBuffer(pPage), NTXBLOCKSIZE)) {
    return false;
  }
  pPage->Changed = false;
  pIndex->fFlush = true;
  /* In shared mode we have to update counter in version field of
     NTXHEADER to signal for other stations that their index buffers
     has to be discarded */
  if (pIndex->fShared) {
    pIndex->Changed = true;
  }
  return true;
}

/*
 * discard all index buffers due to concurrent access
 */
static void hb_ntxDiscardBuffers(LPNTXINDEX pIndex)
{
  pIndex->ulPages = pIndex->ulPageLast = 0;
  pIndex->pChanged = pIndex->pFirst = pIndex->pLast = nullptr;
  if (pIndex->Compound) {
    for (auto i = 0; i < pIndex->iTags; i++) {
      pIndex->lpTags[i]->RootBlock = 0;
      pIndex->lpTags[i]->stackLevel = 0;
    }
  } else {
    pIndex->TagBlock = 0;
    if (pIndex->iTags) {
      pIndex->lpTags[0]->stackLevel = 0;
    }
  }
  hb_fileFlush(pIndex->DiskFile, false);
}

/*
 * update tag flags
 */
static void hb_ntxTagUpdateFlags(LPTAGINFO pTag)
{
  HB_USHORT uiSignature = pTag->Signature;

  pTag->Custom = (uiSignature & NTX_FLAG_CUSTOM) != 0;
  pTag->ChgOnly = (uiSignature & NTX_FLAG_CHGONLY) != 0;
  pTag->Partial = (uiSignature & NTX_FLAG_PARTIAL) != 0;
  pTag->Template = (uiSignature & NTX_FLAG_TEMPLATE) != 0;
  pTag->MultiKey = (uiSignature & NTX_FLAG_MULTIKEY) != 0;
  pTag->fSortRec = (uiSignature & NTX_FLAG_SORTRECNO) != 0;
}

/*
 * check tag header in compound index
 */
static bool hb_ntxTagHeaderCheck(LPTAGINFO pTag)
{
  if (!pTag->RootBlock) {
    if (pTag->HeadBlock) {
      NTXHEADERUPDT header;
      if (hb_ntxBlockRead(pTag->pIndex, pTag->HeadBlock, &header, sizeof(header))) {
        pTag->Signature = HB_GET_LE_UINT16(header.type);
        pTag->RootBlock = HB_GET_LE_UINT32(header.root);
        hb_ntxTagUpdateFlags(pTag);
      }
    }
  }
  return pTag->RootBlock != 0;
}

/*
 * free buffers for pages in the tag
 */
static void hb_ntxFreePageBuffer(LPNTXINDEX pIndex)
{
  HB_ULONG ulMax = pIndex->ulPagesDepth;

  if (ulMax) {
    LPPAGEINFO *pPagePtr = pIndex->pages;

    for (HB_ULONG ul = 0; ul < ulMax; ul++, pPagePtr++) {
      if (*pPagePtr) {
#ifdef HB_NTX_EXTERNAL_PAGEBUFFER
        if (hb_ntxPageBuffer(*pPagePtr)) {
          hb_xfree(hb_ntxPageBuffer(*pPagePtr));
        }
#endif
        hb_xfree(*pPagePtr);
      }
    }
    hb_xfree(pIndex->pages);
    pIndex->pages = nullptr;
    pIndex->ulPages = pIndex->ulPageLast = pIndex->ulPagesDepth = 0;
    pIndex->pFirst = pIndex->pLast = pIndex->pChanged = nullptr;
  }
}

/*
 * trunc index file, left only space for header
 */
static void hb_ntxIndexTrunc(LPNTXINDEX pIndex)
{
  if (!pIndex->lockWrite) {
    hb_errInternal(9102, "hb_ntxIndexTrunc on not locked index file.", nullptr, nullptr);
  }

  hb_ntxFreePageBuffer(pIndex);
  pIndex->Update = pIndex->Changed = pIndex->fFlush = true;
  pIndex->TagBlock = pIndex->NextAvail = 0;
  pIndex->Version = 0;
  hb_fileTruncAt(pIndex->DiskFile, NTXBLOCKSIZE);
}

/*
 * try to find given tag page in the buffer
 */
static LPPAGEINFO hb_ntxPageFind(LPTAGINFO pTag, HB_ULONG ulPage)
{
  LPPAGEINFO *pPagePtr = pTag->pIndex->pages;

  for (HB_ULONG u = pTag->pIndex->ulPages; u; u--, pPagePtr++) {
    if (*pPagePtr && (*pPagePtr)->Page == ulPage) {
      return *pPagePtr;
    }
  }
  return nullptr;
}

/*
 * try to find free space in buffer
 */
static LPPAGEINFO hb_ntxPageGetBuffer(LPTAGINFO pTag, HB_ULONG ulPage)
{
  LPNTXINDEX pIndex = pTag->pIndex;
  LPPAGEINFO *pPagePtr;

  if (pIndex->ulPages < pIndex->ulPagesDepth) {
    pPagePtr = &pIndex->pages[pIndex->ulPages++];
  } else if (pIndex->pFirst) {
    LPPAGEINFO pPage = pIndex->pFirst;

    if (pPage->iUsed) {
      hb_errInternal(9305, "hb_ntxPageGetBuffer: page used.", nullptr, nullptr);
    }
    if (pPage->Changed) {
      hb_errInternal(9306, "hb_ntxPageGetBuffer: page changed.", nullptr, nullptr);
    }

    pIndex->pFirst = pPage->pNext;
    if (pIndex->pFirst) {
      pIndex->pFirst->pPrev = nullptr;
    } else {
      pIndex->pLast = nullptr;
    }
    pPage->pPrev = nullptr;
    pPage->Page = ulPage;
    pPage->iUsed = 1;

    return pPage;
  } else if (pIndex->ulPagesDepth == 0) {
    pIndex->ulPages = 1;
    pIndex->ulPageLast = 0;
    pIndex->ulPagesDepth = NTX_PAGES_PER_TAG;
    pIndex->pages = static_cast<LPPAGEINFO *>(hb_xgrabz(sizeof(LPPAGEINFO) * NTX_PAGES_PER_TAG));
    pPagePtr = &pIndex->pages[0];
  } else {
    HB_ULONG ul = pIndex->ulPageLast;
    for (;;) {
      if (++ul >= pIndex->ulPagesDepth) {
        ul = 0;
      }
      pPagePtr = &pIndex->pages[ul];
      if (!(*pPagePtr)->iUsed && !(*pPagePtr)->Changed) {
        pIndex->ulPageLast = ul;
        break;
      }
      if (ul == pIndex->ulPageLast) {
        ul = pIndex->ulPagesDepth;
        pIndex->ulPagesDepth += NTX_PAGES_PER_TAG >> 1;
        pIndex->pages =
            static_cast<LPPAGEINFO *>(hb_xrealloc(pIndex->pages, sizeof(LPPAGEINFO) * pIndex->ulPagesDepth));
        memset(pIndex->pages + ul, 0, (NTX_PAGES_PER_TAG >> 1) * sizeof(LPPAGEINFO));
        pIndex->ulPages++;
        pPagePtr = &pIndex->pages[ul];
        pIndex->ulPageLast = 0;
        break;
      }
    }
  }

  if (!*pPagePtr) {
    *pPagePtr = static_cast<LPPAGEINFO>(hb_xgrabz(sizeof(HB_PAGEINFO)));
  }
#ifdef HB_NTX_EXTERNAL_PAGEBUFFER
  if (!hb_ntxPageBuffer(*pPagePtr)) {
    hb_ntxPageBuffer(*pPagePtr) = static_cast<char *>(hb_xgrabz(NTXBLOCKSIZE));
  }
#endif
  (*pPagePtr)->pPrev = nullptr;
  (*pPagePtr)->Page = ulPage;
  (*pPagePtr)->iUsed = 1;
  return *pPagePtr;
}

/*
 * free the index page for future reuse
 */
static void hb_ntxPageFree(LPTAGINFO pTag, LPPAGEINFO pPage)
{
  hb_ntxSetKeyPage(pPage, 0, pTag->pIndex->NextAvail);
  pTag->pIndex->NextAvail = pPage->Page;
  pTag->pIndex->Changed = pPage->Changed = true;
}

/*
 * mark used page as free
 */
static void hb_ntxPageRelease(LPTAGINFO pTag, LPPAGEINFO pPage)
{
  LPNTXINDEX pIndex = pTag->pIndex;

  if (--pPage->iUsed == 0) {
    if (pPage->Changed) {
      if (!pPage->pPrev) {
        pPage->pPrev = pPage;
        pPage->pNext = pIndex->pChanged;
        pIndex->pChanged = pPage;
      }
    } else if (pIndex->pLast) {
      pIndex->pLast->pNext = pPage;
      pPage->pPrev = pIndex->pLast;
      pPage->pNext = nullptr;
      pIndex->pLast = pPage;
    } else {
      pPage->pNext = pPage->pPrev = nullptr;
      pIndex->pFirst = pIndex->pLast = pPage;
    }
  } else if (pPage->iUsed < 0) {
    hb_errInternal(9307, "hb_ntxPageRelease: unused page freed.", nullptr, nullptr);
  }
}

/*
 * load page from index file or the buffer
 */
static LPPAGEINFO hb_ntxPageLoad(LPTAGINFO pTag, HB_ULONG ulPage)
{
  LPPAGEINFO pPage;

  if (!ulPage) {
    if (hb_ntxTagHeaderCheck(pTag)) {
      ulPage = pTag->RootBlock;
    }
    if (!ulPage) {
      hb_ntxErrorRT(pTag->pIndex->pArea, EG_CORRUPTION, EDBF_CORRUPT, pTag->pIndex->IndexName, 0, 0, nullptr);
      return nullptr;
    }
  }
  pPage = hb_ntxPageFind(pTag, ulPage);
  if (pPage) {
    if (!pPage->Changed && !pPage->iUsed) {
      if (pPage->pNext) {
        pPage->pNext->pPrev = pPage->pPrev;
      } else {
        pTag->pIndex->pLast = pPage->pPrev;
      }
      if (pPage->pPrev) {
        pPage->pPrev->pNext = pPage->pNext;
        pPage->pPrev = nullptr;
      } else {
        pTag->pIndex->pFirst = pPage->pNext;
      }
    }
    pPage->iUsed++;
  } else {
    pPage = hb_ntxPageGetBuffer(pTag, ulPage);
    pPage->Changed = false;
    if (!hb_ntxBlockRead(pTag->pIndex, ulPage, hb_ntxPageBuffer(pPage), NTXBLOCKSIZE)) {
      hb_ntxPageRelease(pTag, pPage);
      return nullptr;
    }
    pPage->uiKeys = hb_ntxGetKeyCount(pPage);
  }
  return pPage;
}

/*
 * initialize empty page structure
 */
static void hb_ntxPageInit(LPTAGINFO pTag, LPPAGEINFO pPage)
{
  HB_USHORT o = (pTag->MaxKeys + 2) << 1;

  for (HB_USHORT u = 0; u <= pTag->MaxKeys; u++, o += pTag->KeyLength + 8) {
    hb_ntxSetKeyOffset(pPage, u, o);
  }
  hb_ntxSetKeyPage(pPage, 0, 0);
  pPage->uiKeys = 0;
}

/*
 * allocate new page address
 */
static HB_ULONG hb_ntxPageAlloc(LPNTXINDEX pIndex)
{
  HB_ULONG ulPage;

  if (!pIndex->TagBlock) {
    HB_FOFFSET fOffset;
    fOffset = hb_fileSize(pIndex->DiskFile);
    pIndex->TagBlock = static_cast<HB_ULONG>(fOffset >> (pIndex->LargeFile ? NTXBLOCKBITS : 0));
  }
  ulPage = pIndex->TagBlock;
  pIndex->TagBlock += pIndex->LargeFile ? 1 : NTXBLOCKSIZE;
  return ulPage;
}

/*
 * allocate new page in index file - reuse freed one or increase file
 */
static LPPAGEINFO hb_ntxPageNew(LPTAGINFO pTag, bool fNull)
{
  LPPAGEINFO pPage;

  if (pTag->pIndex->NextAvail != 0) {
    /*
       Handling of a pool of empty pages.
       Some sources says that this address is in the first 4 bytes of
       a page ( https://www.clicketyclick.dk/databases/xbase/format/ ).
       But as I understood, studying dumps of Clipper ntx'es, address of the
       next available page is in the address field of a first key item
       in the page - it is done here now in such a way.
       = Alexander Kresin =
     */
    pPage = hb_ntxPageLoad(pTag, pTag->pIndex->NextAvail);
    if (!pPage) {
      return nullptr;
    }
    /*
       Unfortunately Clipper does not left unused index pages clean and
       the key counter can be set to non zero value so to make possible
       concurrent index access from Clipper and xHarbour it's necessary
       to disable the check code below. [druzus]
     */
#if 0
      if( pPage->uiKeys != 0 ) {
         hb_ntxErrorRT(pTag->pIndex->pArea, EG_CORRUPTION, EDBF_CORRUPT, pTag->pIndex->IndexName, 0, 0, nullptr);
         return nullptr;
      }
#endif
    pTag->pIndex->NextAvail = hb_ntxGetKeyPage(pPage, 0);
#if defined(HB_NTX_NOMULTITAG)
    hb_ntxSetKeyPage(pPage, 0, 0);
    pPage->uiKeys = 0;
#else
    hb_ntxPageInit(pTag, pPage);
#endif
  } else {
    pPage = hb_ntxPageGetBuffer(pTag, fNull ? 0 : hb_ntxPageAlloc(pTag->pIndex));
    hb_ntxPageInit(pTag, pPage);
  }
  pTag->pIndex->Changed = pPage->Changed = true;

  return pPage;
}

/*
 * add given page to list of free pages
 */
static void hb_ntxPageAddFree(LPTAGINFO pTag, HB_ULONG ulPage)
{
  LPPAGEINFO pPage = hb_ntxPageGetBuffer(pTag, ulPage);

  pPage->Changed = true;
  hb_ntxPageInit(pTag, pPage);
  hb_ntxPageFree(pTag, pPage);
  hb_ntxPageSave(pTag->pIndex, pPage);
  hb_ntxPageRelease(pTag, pPage);
}

/*
 * get free page in index file
 */
static HB_ULONG hb_ntxPageGetFree(LPTAGINFO pTag)
{
  LPPAGEINFO pPage = hb_ntxPageNew(pTag, false);
  HB_ULONG ulPage = 0;

  if (pPage) {
    ulPage = pPage->Page;
    pPage->Changed = false;
    hb_ntxPageRelease(pTag, pPage);
  }
  return ulPage;
}

/*
 * create the new tag structure
 */
static LPTAGINFO hb_ntxTagNew(LPNTXINDEX pIndex, const char *szTagName, bool fTagName, const char *szKeyExpr,
                              PHB_ITEM pKeyExpr, HB_BYTE bKeyType, HB_USHORT uiKeyLen, HB_USHORT uiKeyDec,
                              const char *szForExpr, PHB_ITEM pForExpr, bool fAscendKey, bool fUnique, bool fCustom,
                              bool fSortRec)
{
  auto pTag = static_cast<LPTAGINFO>(hb_xgrabz(sizeof(TAGINFO)));
  pTag->TagName = hb_strndup(szTagName, NTX_MAX_TAGNAME);
  pTag->fTagName = fTagName;
  pTag->pIndex = pIndex;
  if (szKeyExpr != nullptr) {
    pTag->KeyExpr = hb_strndup(szKeyExpr, NTX_MAX_EXP);
  }
  if (pForExpr && szForExpr) {
    pTag->ForExpr = hb_strndup(szForExpr, NTX_MAX_EXP);
  }
  pTag->nField = hb_rddFieldExpIndex(&pIndex->pArea->dbfarea.area, pTag->KeyExpr);
  pTag->pKeyItem = pKeyExpr;
  pTag->pForItem = pForExpr;
  pTag->AscendKey = fAscendKey;
  pTag->fUsrDescend = !pTag->AscendKey;
  pTag->UniqueKey = fUnique;
  pTag->Custom = fCustom;
  pTag->MultiKey = fCustom && DBFAREA_DATA(&pIndex->pArea->dbfarea)->fMultiKey;
  pTag->KeyType = bKeyType;
  pTag->KeyLength = uiKeyLen;
  pTag->KeyDec = uiKeyDec;
  pTag->fSortRec = fSortRec;
  /*
   * TODO?: keep during page update the offset to 'MaxKeys' key fixed
   * so we will be able to store 1 key more in the page
   */
  pTag->MaxKeys = (NTXBLOCKSIZE - 2) / (uiKeyLen + 10) - 1;

  /* TODO?: is it necessary? It should not interact with well implemented
     algorithm */
  if (pTag->MaxKeys & 0x01 && pTag->MaxKeys > 2) {
    pTag->MaxKeys--;
  }

  pTag->CurKeyInfo = hb_ntxKeyNew(nullptr, pTag->KeyLength);

  return pTag;
}

/*
 * free from memory tag structure
 */
static void hb_ntxTagFree(LPTAGINFO pTag)
{
  if (pTag == pTag->pIndex->pArea->lpCurTag) {
    pTag->pIndex->pArea->lpCurTag = nullptr;
  }
  hb_xfree(pTag->TagName);
  if (pTag->KeyExpr) {
    hb_xfree(pTag->KeyExpr);
  }
  if (pTag->ForExpr) {
    hb_xfree(pTag->ForExpr);
  }
  if (pTag->pKeyItem) {
    hb_vmDestroyBlockOrMacro(pTag->pKeyItem);
  }
  if (pTag->pForItem) {
    hb_vmDestroyBlockOrMacro(pTag->pForItem);
  }
  if (pTag->HotKeyInfo) {
    hb_ntxKeyFree(pTag->HotKeyInfo);
  }
  hb_ntxKeyFree(pTag->CurKeyInfo);
  hb_ntxTagClearScope(pTag, 0);
  hb_ntxTagClearScope(pTag, 1);
  if (pTag->stack) {
    hb_xfree(pTag->stack);
  }
  hb_xfree(pTag);
}

/*
 * delete tag from compound index
 */
static void hb_ntxTagDelete(LPTAGINFO pTag)
{
  LPNTXINDEX pIndex = pTag->pIndex;

  for (auto i = 0; i < pIndex->iTags; i++) {
    if (pTag == pIndex->lpTags[i]) {
      while (++i < pIndex->iTags) {
        pIndex->lpTags[i - 1] = pIndex->lpTags[i];
      }
      if (--pIndex->iTags) {
        pIndex->lpTags = static_cast<LPTAGINFO *>(hb_xrealloc(pIndex->lpTags, sizeof(LPTAGINFO) * pIndex->iTags));
      } else {
        hb_xfree(pIndex->lpTags);
      }
      break;
    }
  }
  hb_ntxTagFree(pTag);
  pIndex->pArea->fSetTagNumbers = true;
}

/*
 * add tag to compound index
 */
static HB_ERRCODE hb_ntxTagAdd(LPNTXINDEX pIndex, LPTAGINFO pTag)
{
  if (pIndex->iTags >= CTX_MAX_TAGS) {
    return Harbour::FAILURE;
  }

  if (pIndex->iTags) {
    pIndex->lpTags = static_cast<LPTAGINFO *>(hb_xrealloc(pIndex->lpTags, sizeof(LPTAGINFO) * (pIndex->iTags + 1)));
  } else {
    pIndex->lpTags = static_cast<LPTAGINFO *>(hb_xgrab(sizeof(LPTAGINFO)));
  }

  pIndex->lpTags[pIndex->iTags++] = pTag;
  pIndex->pArea->fSetTagNumbers = true;
  return Harbour::SUCCESS;
}

/*
 * create new tag and load it from index file
 */
static LPTAGINFO hb_ntxTagLoad(LPNTXINDEX pIndex, HB_ULONG ulBlock, const char *szTagName, HB_BYTE *buffer)
{
  LPNTXHEADER lpNTX = reinterpret_cast<LPNTXHEADER>(buffer);
  LPTAGINFO pTag;
  PHB_ITEM pKeyExp, pForExp = nullptr;
  HB_USHORT usType;
  auto fName = false;

  usType = HB_GET_LE_UINT16(lpNTX->type);

  if ((usType & ~NTX_FLAG_MASK) || ((usType & NTX_FLAG_DEFALUT) != NTX_FLAG_DEFALUT && usType != NTX_FLAG_OLDDEFALUT) ||
      lpNTX->key_expr[0] < 0x20) {
    return nullptr;
  }

  if (SELF_COMPILE(&pIndex->pArea->dbfarea.area, reinterpret_cast<const char *>(lpNTX->key_expr)) == Harbour::FAILURE) {
    return nullptr;
  }
  pKeyExp = pIndex->pArea->dbfarea.area.valResult;
  pIndex->pArea->dbfarea.area.valResult = nullptr;

  if (usType & NTX_FLAG_FORITEM && lpNTX->for_expr[0] >= 0x20) {
    if (SELF_COMPILE(&pIndex->pArea->dbfarea.area, reinterpret_cast<const char *>(lpNTX->for_expr)) ==
        Harbour::FAILURE) {
      hb_vmDestroyBlockOrMacro(pKeyExp);
      return nullptr;
    }
    pForExp = pIndex->pArea->dbfarea.area.valResult;
    pIndex->pArea->dbfarea.area.valResult = nullptr;
  }
  fName = !pIndex->Compound && lpNTX->tag_name[0] >= 0x20;
  pTag = hb_ntxTagNew(pIndex, fName ? reinterpret_cast<const char *>(lpNTX->tag_name) : szTagName, fName,
                      reinterpret_cast<const char *>(lpNTX->key_expr), pKeyExp, '\0', HB_GET_LE_UINT16(lpNTX->key_size),
                      HB_GET_LE_UINT16(lpNTX->key_dec), reinterpret_cast<const char *>(lpNTX->for_expr), pForExp,
                      lpNTX->descend[0] == 0, lpNTX->unique[0] != 0,
                      (usType & NTX_FLAG_CUSTOM) != 0 || lpNTX->custom[0] != 0, (usType & NTX_FLAG_SORTRECNO) != 0);

  pTag->Signature = usType;
  hb_ntxTagUpdateFlags(pTag);
  pTag->HeadBlock = ulBlock;
  pTag->RootBlock = HB_GET_LE_UINT32(lpNTX->root);
  pTag->MaxKeys = HB_GET_LE_UINT16(lpNTX->max_item);
  pTag->KeyType = hb_ntxGetKeyType(pTag);

  pIndex->LargeFile = (usType & NTX_FLAG_LARGEFILE) != 0;

  if (!pIndex->Compound) {
    pIndex->Version = HB_GET_LE_UINT16(lpNTX->version);
    pIndex->NextAvail = HB_GET_LE_UINT32(lpNTX->next_page);
    pIndex->TagBlock = 0;

    /* TODO: this breaks unlocking !!! */
    if (usType & NTX_FLAG_LARGEFILE) {
      pIndex->pArea->dbfarea.bLockType = DB_DBFLOCK_HB64;
    } else if (usType & NTX_FLAG_EXTLOCK) {
      pIndex->pArea->dbfarea.bLockType = DB_DBFLOCK_CLIPPER2;
    } else if (!pIndex->pArea->dbfarea.bLockType) {
      pIndex->pArea->dbfarea.bLockType = (usType & NTX_FLAG_EXTLOCK) ? DB_DBFLOCK_CLIPPER2 : DB_DBFLOCK_CLIPPER;
    }
  }
  return pTag;
}

/*
 * add tag into CTX header
 */
static void hb_ntxIndexTagAdd(LPNTXINDEX pIndex, LPTAGINFO pTag)
{
  LPCTXHEADER lpCTX = reinterpret_cast<LPCTXHEADER>(pIndex->HeaderBuff);
  int iTags = HB_GET_LE_UINT16(lpCTX->ntags), iLen, i;
  LPCTXTAGITEM pTagItem = lpCTX->tags;

  for (i = 0; i < iTags; pTagItem++, i++) {
    if (!hb_strnicmp(reinterpret_cast<const char *>(pTagItem->tag_name), pTag->TagName, NTX_MAX_TAGNAME)) {
      break;
    }
  }
  if (i == iTags) {
    ++iTags;
    HB_PUT_LE_UINT16(lpCTX->ntags, iTags);
    iLen = static_cast<int>(strlen(pTag->TagName));
    if (iLen > NTX_MAX_TAGNAME) {
      iLen = NTX_MAX_TAGNAME;
    }
    memcpy(pTagItem->tag_name, pTag->TagName, iLen);
    memset(pTagItem->tag_name + iLen, 0, sizeof(pTagItem->tag_name) - iLen);
  }
  HB_PUT_LE_UINT32(pTagItem->tag_header, pTag->HeadBlock);
  pIndex->Update = true;
}

/*
 * delete tag from CTX header
 */
static void hb_ntxIndexTagDel(LPNTXINDEX pIndex, const char *szTagName)
{
  LPCTXHEADER lpCTX = reinterpret_cast<LPCTXHEADER>(pIndex->HeaderBuff);
  int iTags = HB_GET_LE_UINT16(lpCTX->ntags);
  LPCTXTAGITEM pTagItem = lpCTX->tags;

  for (auto i = 0; i < iTags; pTagItem++, i++) {
    if (!hb_strnicmp(reinterpret_cast<const char *>(pTagItem->tag_name), szTagName, NTX_MAX_TAGNAME)) {
      memmove(pTagItem, pTagItem + 1, (iTags - i) * sizeof(CTXTAGITEM));
      memset(pTagItem + iTags - 1, 0, sizeof(CTXTAGITEM));
      --iTags;
      HB_PUT_LE_UINT16(lpCTX->ntags, iTags);
      pIndex->Update = true;
      break;
    }
  }
}

/*
 * find tag header block in CTX header
 */
static HB_ULONG hb_ntxIndexTagFind(LPCTXHEADER lpCTX, const char *szTagName)
{
  int iTags = HB_GET_LE_UINT16(lpCTX->ntags);
  LPCTXTAGITEM pTagItem = lpCTX->tags;

  for (auto i = 0; i < iTags; pTagItem++, i++) {
    if (!hb_strnicmp(reinterpret_cast<const char *>(pTagItem->tag_name), szTagName, NTX_MAX_TAGNAME)) {
      return HB_GET_LE_UINT32(pTagItem->tag_header);
    }
  }
  return NTX_DUMMYNODE;
}

/*
 * Write tag header
 */
static HB_ERRCODE hb_ntxTagHeaderSave(LPTAGINFO pTag)
{
  LPNTXINDEX pIndex = pTag->pIndex;
  NTXHEADER Header;
  int iSize = NTX_ROOTHEAD_HEADSIZE, type, version = 0, iLen;
  HB_ULONG next = 0;

  if (pIndex->Compound) {
    if (!pTag->HeadBlock) {
      pTag->HeadBlock = hb_ntxPageGetFree(pTag);
      if (!pTag->HeadBlock) {
        return Harbour::FAILURE;
      }
      hb_ntxIndexTagAdd(pIndex, pTag);
    }
  } else {
    if (pTag->HeadBlock) {
      hb_ntxPageAddFree(pTag, pTag->HeadBlock);
      pTag->HeadBlock = 0;
      pIndex->Update = true;
    }
    pIndex->Version++;
    version = pIndex->Version &= 0xffff;
    next = pIndex->NextAvail;
  }

  type = NTX_FLAG_DEFALUT | (pTag->ForExpr ? NTX_FLAG_FORITEM : 0) |
         (pTag->Partial ? NTX_FLAG_PARTIAL | NTX_FLAG_FORITEM : 0) |
         (pIndex->pArea->dbfarea.bLockType == DB_DBFLOCK_CLIPPER2 ? NTX_FLAG_EXTLOCK : 0) |
         (pTag->Partial ? NTX_FLAG_PARTIAL | NTX_FLAG_FORITEM : 0) |
         /* non Clipper flags */
         (pTag->Custom ? NTX_FLAG_CUSTOM : 0) | (pTag->ChgOnly ? NTX_FLAG_CHGONLY : 0) |
         (pTag->Template ? NTX_FLAG_TEMPLATE : 0) | (pTag->MultiKey ? NTX_FLAG_MULTIKEY : 0) |
         (pTag->fSortRec ? NTX_FLAG_SORTRECNO : 0) | (pIndex->LargeFile ? NTX_FLAG_LARGEFILE : 0);

  HB_PUT_LE_UINT16(Header.type, type);
  HB_PUT_LE_UINT16(Header.version, version);
  HB_PUT_LE_UINT32(Header.root, pTag->RootBlock);
  HB_PUT_LE_UINT32(Header.next_page, next);

  if (pIndex->Update) {
    memset(reinterpret_cast<HB_BYTE *>(&Header) + NTX_ROOTHEAD_HEADSIZE, 0, sizeof(NTXHEADER) - NTX_ROOTHEAD_HEADSIZE);

    HB_PUT_LE_UINT16(Header.item_size, pTag->KeyLength + 8);
    HB_PUT_LE_UINT16(Header.key_size, pTag->KeyLength);
    HB_PUT_LE_UINT16(Header.key_dec, pTag->KeyDec);
    HB_PUT_LE_UINT16(Header.max_item, pTag->MaxKeys);
    HB_PUT_LE_UINT16(Header.half_page, pTag->MaxKeys >> 1);
    Header.unique[0] = pTag->UniqueKey ? 1 : 0;
    Header.descend[0] = pTag->AscendKey ? 0 : 1;
    Header.custom[0] = pTag->Custom ? 1 : 0;
    iLen = static_cast<int>(strlen(pTag->KeyExpr));
    if (iLen > NTX_MAX_EXP) {
      iLen = NTX_MAX_EXP;
    }
    memcpy(Header.key_expr, pTag->KeyExpr, iLen);
    if (pTag->ForExpr) {
      iLen = static_cast<int>(strlen(pTag->ForExpr));
      if (iLen > NTX_MAX_EXP) {
        iLen = NTX_MAX_EXP;
      }
      memcpy(Header.for_expr, pTag->ForExpr, iLen);
    }
    if (pTag->fTagName) {
      iLen = static_cast<int>(strlen(pTag->TagName));
      if (iLen > NTX_MAX_TAGNAME) {
        iLen = NTX_MAX_TAGNAME;
      }
      memcpy(Header.tag_name, pTag->TagName, iLen);
    }
    iSize = sizeof(NTXHEADER);
  }

  if (!hb_ntxBlockWrite(pIndex, pTag->HeadBlock, &Header, iSize)) {
    return Harbour::FAILURE;
  }
  pTag->HdrChanged = false;
  pIndex->Changed = pIndex->Compound;
  pIndex->fFlush = true;
  return Harbour::SUCCESS;
}

/*
 * create new index structure
 */
static LPNTXINDEX hb_ntxIndexNew(NTXAREAP pArea)
{
  auto pIndex = static_cast<LPNTXINDEX>(hb_xgrabz(sizeof(NTXINDEX)));

  pIndex->DiskFile = nullptr;
  pIndex->pArea = pArea;
  return pIndex;
}

/*
 * close the index file and free from memory index and tag structures
 */
static void hb_ntxIndexFree(LPNTXINDEX pIndex)
{
  hb_ntxFreePageBuffer(pIndex);
  if (pIndex->iTags) {
    for (auto i = 0; i < pIndex->iTags; i++) {
      hb_ntxTagFree(pIndex->lpTags[i]);
    }
    hb_xfree(pIndex->lpTags);
  }
  if (pIndex->HeaderBuff) {
    hb_xfree(pIndex->HeaderBuff);
  }
  if (pIndex->DiskFile) {
    hb_fileClose(pIndex->DiskFile);
    if (pIndex->fDelete) {
      hb_fileDelete(pIndex->RealName ? pIndex->RealName : pIndex->IndexName);
    }
  }
  if (pIndex->IndexName) {
    hb_xfree(pIndex->IndexName);
  }
  if (pIndex->RealName) {
    hb_xfree(pIndex->RealName);
  }
  pIndex->pArea->fSetTagNumbers = true;
  hb_xfree(pIndex);
}

/*
 * Write tag header
 */
static HB_ERRCODE hb_ntxIndexHeaderSave(LPNTXINDEX pIndex)
{
  if (pIndex->Compound) {
    LPCTXHEADER lpCTX = reinterpret_cast<LPCTXHEADER>(pIndex->HeaderBuff);
    int iSize = pIndex->Update ? NTXBLOCKSIZE : 16;
    HB_USHORT type;

    type = NTX_FLAG_COMPOUND | (pIndex->LargeFile ? NTX_FLAG_LARGEFILE : 0);

    pIndex->Version++;
    HB_PUT_LE_UINT16(lpCTX->type, type);
    HB_PUT_LE_UINT16(lpCTX->ntags, pIndex->iTags);
    HB_PUT_LE_UINT32(lpCTX->version, pIndex->Version);
    HB_PUT_LE_UINT32(lpCTX->freepage, pIndex->NextAvail);
    HB_PUT_LE_UINT32(lpCTX->filesize, pIndex->TagBlock);

    if (!hb_ntxBlockWrite(pIndex, 0, lpCTX, iSize)) {
      return Harbour::FAILURE;
    }
  }
  pIndex->Changed = pIndex->Update = false;
  return Harbour::SUCCESS;
}

/*
 * load new tags from index file
 */
static HB_ERRCODE hb_ntxIndexLoad(LPNTXINDEX pIndex, const char *szTagName)
{
  LPTAGINFO pTag;
  HB_USHORT type;

  if (!pIndex->fValidHeader) {
    if (!pIndex->HeaderBuff) {
      pIndex->HeaderBuff = static_cast<HB_BYTE *>(hb_xgrab(NTXBLOCKSIZE));
    }
    if (!hb_ntxBlockRead(pIndex, 0, pIndex->HeaderBuff, NTXBLOCKSIZE)) {
      return Harbour::FAILURE;
    }
    pIndex->fValidHeader = true;
  }

  type = HB_GET_LE_UINT16(pIndex->HeaderBuff);
#if !defined(HB_NTX_NOMULTITAG)
  pIndex->Compound = (type & NTX_FLAG_COMPOUND) != 0;
  if (pIndex->Compound) {
    HB_BYTE tagbuffer[NTXBLOCKSIZE];
    LPCTXHEADER lpCTX = reinterpret_cast<LPCTXHEADER>(pIndex->HeaderBuff);
    LPCTXTAGITEM pTagItem = lpCTX->tags;
    int iTags;

    iTags = HB_GET_LE_UINT16(lpCTX->ntags);
    if (iTags > CTX_MAX_TAGS) {
      return Harbour::FAILURE;
    }
    pIndex->Version = HB_GET_LE_UINT32(lpCTX->version);
    pIndex->NextAvail = HB_GET_LE_UINT32(lpCTX->freepage);
    pIndex->TagBlock = HB_GET_LE_UINT32(lpCTX->filesize);
    pIndex->LargeFile = (type & NTX_FLAG_LARGEFILE) != 0;

    for (pIndex->iTags = 0; pIndex->iTags < iTags; pTagItem++) {
      HB_ULONG ulBlock = HB_GET_LE_UINT32(pTagItem->tag_header);
      if (ulBlock == 0 || pTagItem->tag_name[0] <= 0x20) {
        return Harbour::FAILURE;
      }
      if (!hb_ntxBlockRead(pIndex, ulBlock, tagbuffer, NTXBLOCKSIZE)) {
        return Harbour::FAILURE;
      }
      pTag = hb_ntxTagLoad(pIndex, ulBlock, reinterpret_cast<const char *>(pTagItem->tag_name), tagbuffer);
      if (!pTag) {
        return Harbour::FAILURE;
      }
      hb_ntxTagAdd(pIndex, pTag);
    }
  } else
#endif
  {
    pTag = hb_ntxTagLoad(pIndex, 0, szTagName, pIndex->HeaderBuff);
    if (!pTag) {
      return Harbour::FAILURE;
    }
    hb_ntxTagAdd(pIndex, pTag);
  }

  return Harbour::SUCCESS;
}

/*
 * read index header and check for concurrent access
 */
static HB_ERRCODE hb_ntxIndexHeaderRead(LPNTXINDEX pIndex)
{
  HB_USHORT type;

  if (!pIndex->HeaderBuff) {
    pIndex->HeaderBuff = static_cast<HB_BYTE *>(hb_xgrab(NTXBLOCKSIZE));
  }

  if (!hb_ntxBlockRead(pIndex, 0, pIndex->HeaderBuff, NTXBLOCKSIZE)) {
    return Harbour::FAILURE;
  }

  type = HB_GET_LE_UINT16(pIndex->HeaderBuff);
  if ((type & NTX_FLAG_COMPOUND) != 0) {
#if defined(HB_NTX_NOMULTITAG)
    hb_ntxErrorRT(pIndex->pArea, EG_CORRUPTION, EDBF_CORRUPT, pIndex->IndexName, 0, 0, nullptr);
    return Harbour::FAILURE;
#else
    LPCTXHEADER lpCTX = reinterpret_cast<LPCTXHEADER>(pIndex->HeaderBuff);
    HB_ULONG ulVersion, ulNext;
#if 0
      HB_USHORT usTags = HB_GET_LE_UINT16(lpCTX->ntags);
#endif

    ulVersion = HB_GET_LE_UINT32(lpCTX->version);
    ulNext = HB_GET_LE_UINT32(lpCTX->freepage);
    pIndex->TagBlock = HB_GET_LE_UINT32(lpCTX->filesize);

    if (pIndex->Version != ulVersion || pIndex->NextAvail != ulNext || !pIndex->Compound) {
      hb_ntxDiscardBuffers(pIndex);
      pIndex->Version = ulVersion;
      pIndex->NextAvail = ulNext;
      pIndex->Compound = true;
      for (auto i = 1; i < pIndex->iTags; i++) {
        pIndex->lpTags[i]->HeadBlock = hb_ntxIndexTagFind(lpCTX, pIndex->lpTags[i]->TagName);
        if (!pIndex->lpTags[i]->HeadBlock) {
          pIndex->lpTags[i]->RootBlock = 0;
        }
      }
    }
#endif
  } else {
    LPNTXHEADER lpNTX = reinterpret_cast<LPNTXHEADER>(pIndex->HeaderBuff);
    HB_ULONG ulRootPage, ulVersion, ulNext;
    LPTAGINFO pTag;

    if (pIndex->Compound) {
      hb_ntxErrorRT(pIndex->pArea, EG_CORRUPTION, EDBF_CORRUPT, pIndex->IndexName, 0, 0, nullptr);
      return Harbour::FAILURE;
    }
    pTag = pIndex->iTags ? pIndex->lpTags[0] : nullptr;

    ulVersion = HB_GET_LE_UINT16(lpNTX->version);
    ulRootPage = HB_GET_LE_UINT32(lpNTX->root);
    ulNext = HB_GET_LE_UINT32(lpNTX->next_page);
    if (pIndex->Version != ulVersion || pIndex->NextAvail != ulNext ||
        (pTag && (pTag->Signature != type || ulRootPage != pTag->RootBlock))) {
      hb_ntxDiscardBuffers(pIndex);
      pIndex->Version = ulVersion;
      pIndex->NextAvail = ulNext;
      if (pTag != nullptr) {
        pTag->RootBlock = ulRootPage;
        pTag->Signature = type;
        hb_ntxTagUpdateFlags(pTag);
      }
    }
  }
  return Harbour::SUCCESS;
}

/*
 * write modified pages to index file
 */
static void hb_ntxIndexFlush(LPNTXINDEX pIndex)
{
  while (pIndex->pChanged) {
    LPPAGEINFO pPage = pIndex->pChanged;
    pIndex->pChanged = pPage->pNext;
    if (pPage->Changed) {
      hb_ntxPageSave(pIndex, pPage);
      ++pPage->iUsed;
      /* hack */
      hb_ntxPageRelease(pIndex->lpTags[0], pPage);
    } else {
      hb_errInternal(9308, "hb_ntxIndexFlush: unchaged page in the list.", nullptr, nullptr);
    }
  }

  if (pIndex->Compound) {
    for (auto i = 0; i < pIndex->iTags; i++) {
      if (pIndex->lpTags[i]->HdrChanged) {
        hb_ntxTagHeaderSave(pIndex->lpTags[i]);
      }
    }
    if (pIndex->Changed) {
      hb_ntxIndexHeaderSave(pIndex);
    }
  } else if (pIndex->iTags) {
    if (pIndex->Changed || pIndex->lpTags[0]->HdrChanged) {
      hb_ntxTagHeaderSave(pIndex->lpTags[0]);
    }
  }
}

/*
 * lock index for reading (shared lock)
 */
static bool hb_ntxIndexLockRead(LPNTXINDEX pIndex)
{
  auto fOK = false;

  if (pIndex->lockRead > 0 || pIndex->lockWrite > 0 || !pIndex->fShared || HB_DIRTYREAD(&pIndex->pArea->dbfarea)) {
    fOK = true;
    pIndex->lockRead++;
  } else {
    fOK = hb_dbfLockIdxFile(&pIndex->pArea->dbfarea, pIndex->DiskFile, FL_LOCK | FLX_SHARED | FLX_WAIT, false,
                            &pIndex->lockData);
    /* if fOK then check VERSION field in NTXHEADER and
     * if it has been changed then discard all page buffers
     */
    if (fOK) {
      pIndex->lockRead++;
      if (hb_ntxIndexHeaderRead(pIndex) != Harbour::SUCCESS) {
        pIndex->lockRead--;
        hb_dbfLockIdxFile(&pIndex->pArea->dbfarea, pIndex->DiskFile, FL_UNLOCK, false, &pIndex->lockData);
        return false;
      }
    }
  }
  if (!fOK) {
    hb_ntxErrorRT(pIndex->pArea, EG_LOCK, EDBF_LOCK, pIndex->IndexName, hb_fsError(), 0, nullptr);
  }

  return fOK;
}

/*
 * lock index for writing (exclusive lock)
 */
static bool hb_ntxIndexLockWrite(LPNTXINDEX pIndex, bool fCheck)
{
  auto fOK = false;

  if (pIndex->fReadonly) {
    hb_errInternal(9101, "hb_ntxIndexLockWrite: readonly index.", nullptr, nullptr);
  }

  if (pIndex->lockRead) {
    hb_errInternal(9105, "hb_ntxIndexLockWrite: writeLock after readLock.", nullptr, nullptr);
  }

  if (pIndex->lockWrite > 0 || !pIndex->fShared) {
    fOK = true;
    pIndex->lockWrite++;
  } else {
    fOK = hb_dbfLockIdxFile(&pIndex->pArea->dbfarea, pIndex->DiskFile, FL_LOCK | FLX_EXCLUSIVE | FLX_WAIT, false,
                            &pIndex->lockData);
    /* if fOK then check VERSION field in NTXHEADER and
     * if it has been changed then discard all page buffers
     */
    if (fOK) {
      pIndex->lockWrite++;
      if (fCheck && hb_ntxIndexHeaderRead(pIndex) != Harbour::SUCCESS) {
        pIndex->lockWrite--;
        hb_dbfLockIdxFile(&pIndex->pArea->dbfarea, pIndex->DiskFile, FL_UNLOCK, false, &pIndex->lockData);
        return false;
      }
    }
  }
  if (!fOK) {
    hb_ntxErrorRT(pIndex->pArea, EG_LOCK, EDBF_LOCK, pIndex->IndexName, hb_fsError(), 0, nullptr);
  }

  return fOK;
}

/*
 * remove index read lock (shared lock)
 */
static bool hb_ntxIndexUnLockRead(LPNTXINDEX pIndex)
{
  auto fOK = false;

#ifdef HB_NTX_DEBUG
  for (auto i = 0; i < pIndex->iTags; i++) {
    hb_ntxTagCheckBuffers(pIndex->lpTags[i]);
  }
#endif

  pIndex->lockRead--;
  if (pIndex->lockRead < 0) {
    hb_errInternal(9106, "hb_ntxIndexUnLockRead: bad count of locks.", nullptr, nullptr);
  }

  if (pIndex->lockRead || pIndex->lockWrite || !pIndex->fShared || HB_DIRTYREAD(&pIndex->pArea->dbfarea)) {
    fOK = true;
  } else {
    pIndex->fValidHeader = false;
    fOK = hb_dbfLockIdxFile(&pIndex->pArea->dbfarea, pIndex->DiskFile, FL_UNLOCK, false, &pIndex->lockData);
  }
  if (!fOK) {
    hb_errInternal(9108, "hb_ntxIndexUnLockRead: unlock error.", nullptr, nullptr);
  }

  return fOK;
}

/*
 * remove index write lock (exclusive lock)
 */
static bool hb_ntxIndexUnLockWrite(LPNTXINDEX pIndex)
{
  auto fOK = false;

#ifdef HB_NTX_DEBUG
  for (auto i = 0; i < pIndex->iTags; i++) {
    hb_ntxTagCheckBuffers(pIndex->lpTags[i]);
  }
#endif

  if (pIndex->lockWrite <= 0) {
    hb_errInternal(9106, "hb_ntxIndexUnLockWrite: bad count of locks.", nullptr, nullptr);
  }
  if (pIndex->lockRead) {
    hb_errInternal(9105, "hb_ntxIndexUnLockWrite: writeUnLock before readUnLock.", nullptr, nullptr);
  }

  hb_ntxIndexFlush(pIndex);
  pIndex->lockWrite--;

  if (pIndex->lockWrite || !pIndex->fShared) {
    fOK = true;
  } else {
    hb_fileFlush(pIndex->DiskFile, true);
    pIndex->fValidHeader = false;
    fOK = hb_dbfLockIdxFile(&pIndex->pArea->dbfarea, pIndex->DiskFile, FL_UNLOCK, false, &pIndex->lockData);
  }
  if (!fOK) {
    hb_errInternal(9108, "hb_ntxIndexUnLockWrite: unlock error.", nullptr, nullptr);
  }

  return fOK;
}

/*
 * lock tag for reading (shared lock)
 */
static bool hb_ntxTagLockRead(LPTAGINFO pTag)
{
  auto fOK = false;

  if (hb_ntxIndexLockRead(pTag->pIndex)) {
    fOK = hb_ntxTagHeaderCheck(pTag);
    if (!fOK) {
      hb_ntxIndexUnLockRead(pTag->pIndex);
      hb_ntxErrorRT(pTag->pIndex->pArea, EG_CORRUPTION, EDBF_CORRUPT, pTag->pIndex->IndexName, 0, 0, nullptr);
    }
  }
  return fOK;
}

/*
 * lock tag for writing (exclusive lock)
 */
static bool hb_ntxTagLockWrite(LPTAGINFO pTag)
{
  auto fOK = false;

  if (hb_ntxIndexLockWrite(pTag->pIndex, true)) {
    fOK = hb_ntxTagHeaderCheck(pTag);
    if (!fOK) {
      hb_ntxIndexUnLockWrite(pTag->pIndex);
      hb_ntxErrorRT(pTag->pIndex->pArea, EG_CORRUPTION, EDBF_CORRUPT, pTag->pIndex->IndexName, 0, 0, nullptr);
    }
  }
  return fOK;
}

/*
 * remove tag read lock (shared lock)
 */
static bool hb_ntxTagUnLockRead(LPTAGINFO pTag)
{
  return hb_ntxIndexUnLockRead(pTag->pIndex);
}

/*
 * remove tag write lock (exclusive lock)
 */
static bool hb_ntxTagUnLockWrite(LPTAGINFO pTag)
{
  return hb_ntxIndexUnLockWrite(pTag->pIndex);
}

/*
 * retrive key from page
 */
static void hb_ntxPageGetKey(LPPAGEINFO pPage, HB_USHORT uiKey, LPKEYINFO pKey, HB_USHORT uiLen)
{
  if (uiKey < pPage->uiKeys) {
    memcpy(pKey->key, hb_ntxGetKeyVal(pPage, uiKey), uiLen);
    pKey->Xtra = hb_ntxGetKeyRec(pPage, uiKey);
    pKey->Tag = pPage->Page;
  } else {
    pKey->Xtra = pKey->Tag = 0;
  }
}

/*
 * set next page and key in page path
 */
static void hb_ntxTagSetPageStack(LPTAGINFO pTag, HB_ULONG ulPage, HB_USHORT uiKey)
{
  if (pTag->stackLevel == pTag->stackSize) {
    if (pTag->stackSize == 0) {
      pTag->stackSize = NTX_STACKSIZE;
      pTag->stack = static_cast<LPTREESTACK>(hb_xgrab(sizeof(TREE_STACK) * NTX_STACKSIZE));
    } else {
      pTag->stackSize += NTX_STACKSIZE;
      pTag->stack = static_cast<LPTREESTACK>(hb_xrealloc(pTag->stack, sizeof(TREE_STACK) * pTag->stackSize));
    }
  }
  pTag->stack[pTag->stackLevel].page = ulPage;
  pTag->stack[pTag->stackLevel++].ikey = uiKey;
}

/*
 * go down from the given index page to the first key
 */
static LPPAGEINFO hb_ntxPageTopMove(LPTAGINFO pTag, HB_ULONG ulPage)
{
  LPPAGEINFO pPage = nullptr;

  do {
    if (pPage) {
      hb_ntxPageRelease(pTag, pPage);
    }
    pPage = hb_ntxPageLoad(pTag, ulPage);
    if (!pPage) {
      return nullptr;
    }
#ifdef HB_NTX_DEBUG_EXT
    if (pPage->uiKeys == 0 && pTag->stackLevel > 0) {
      hb_errInternal(9201, "hb_ntxPageTopMove: index corrupted.", nullptr, nullptr);
    }
#endif
    ulPage = hb_ntxGetKeyPage(pPage, 0);
    hb_ntxTagSetPageStack(pTag, pPage->Page, 0);
  } while (ulPage);

  return pPage;
}

/*
 * go down from the given index page to the last key
 */
static LPPAGEINFO hb_ntxPageBottomMove(LPTAGINFO pTag, HB_ULONG ulPage)
{
  LPPAGEINFO pPage = nullptr;

  do {
    if (pPage) {
      hb_ntxPageRelease(pTag, pPage);
    }
    pPage = hb_ntxPageLoad(pTag, ulPage);
    if (!pPage) {
      return nullptr;
    }
#ifdef HB_NTX_DEBUG_EXT
    if (pPage->uiKeys == 0 && pTag->stackLevel > 0) {
      hb_errInternal(9201, "hb_ntxPageBottomMove: index corrupted.", nullptr, nullptr);
    }
#endif
    ulPage = hb_ntxGetKeyPage(pPage, pPage->uiKeys);
    hb_ntxTagSetPageStack(pTag, pPage->Page, pPage->uiKeys - (ulPage || pPage->uiKeys == 0 ? 0 : 1));
  } while (ulPage);

  return pPage;
}

/*
 * set page path to the first key in tag
 */
static bool hb_ntxTagTopKey(LPTAGINFO pTag)
{
  LPPAGEINFO pPage;
  int iKeys;

  pTag->stackLevel = 0;
  pPage = hb_ntxPageTopMove(pTag, 0);
  if (!pPage) {
    return false;
  }
  hb_ntxPageGetKey(pPage, 0, pTag->CurKeyInfo, pTag->KeyLength);
  iKeys = pPage->uiKeys;
  hb_ntxPageRelease(pTag, pPage);
  return iKeys != 0;
}

/*
 * set page path to the last key in tag
 */
static bool hb_ntxTagBottomKey(LPTAGINFO pTag)
{
  LPPAGEINFO pPage;
  int iKeys;

  pTag->stackLevel = 0;
  pPage = hb_ntxPageBottomMove(pTag, 0);
  if (!pPage) {
    return false;
  }
  hb_ntxPageGetKey(pPage, pTag->stack[pTag->stackLevel - 1].ikey, pTag->CurKeyInfo, pTag->KeyLength);
  iKeys = pPage->uiKeys;
  hb_ntxPageRelease(pTag, pPage);
  return iKeys != 0;
}

/*
 * update page path to the next key in tag
 */
static bool hb_ntxTagNextKey(LPTAGINFO pTag)
{
  int iLevel = pTag->stackLevel - 1;

  if (iLevel >= 0) {
    LPPAGEINFO pPage;
    HB_ULONG ulPage = 0;

    pPage = hb_ntxPageLoad(pTag, pTag->stack[iLevel].page);
    if (!pPage) {
      return false;
    }
    if (pTag->stack[iLevel].ikey < static_cast<HB_SHORT>(pPage->uiKeys)) {
      ulPage = hb_ntxGetKeyPage(pPage, pTag->stack[iLevel].ikey + 1);
    }
    if (ulPage || pTag->stack[iLevel].ikey + 1 < pPage->uiKeys) {
      pTag->stack[iLevel].ikey++;
      if (ulPage) {
        hb_ntxPageRelease(pTag, pPage);
        pPage = hb_ntxPageTopMove(pTag, ulPage);
        if (!pPage) {
          return false;
        }
      }
    } else {
      while (--iLevel >= 0) {
        hb_ntxPageRelease(pTag, pPage);
        pPage = hb_ntxPageLoad(pTag, pTag->stack[iLevel].page);
        if (!pPage) {
          return false;
        }
        if (pTag->stack[iLevel].ikey < static_cast<HB_SHORT>(pPage->uiKeys)) {
          break;
        }
      }
      if (iLevel < 0) {
        hb_ntxPageRelease(pTag, pPage);
        return false;
      }
      pTag->stackLevel = static_cast<HB_USHORTCAST>(iLevel + 1);
    }
    hb_ntxPageGetKey(pPage, pTag->stack[pTag->stackLevel - 1].ikey, pTag->CurKeyInfo, pTag->KeyLength);
    hb_ntxPageRelease(pTag, pPage);
    return true;
  }
  return false;
}

/*
 * update page path to the previous key in tag
 */
static bool hb_ntxTagPrevKey(LPTAGINFO pTag)
{
  int iLevel = pTag->stackLevel - 1;

  if (iLevel >= 0) {
    LPPAGEINFO pPage;
    HB_ULONG ulPage;

    pPage = hb_ntxPageLoad(pTag, pTag->stack[iLevel].page);
    if (!pPage) {
      return false;
    }
    ulPage = hb_ntxGetKeyPage(pPage, pTag->stack[iLevel].ikey);
    if (ulPage) {
      hb_ntxPageRelease(pTag, pPage);
      pPage = hb_ntxPageBottomMove(pTag, ulPage);
      if (!pPage) {
        return false;
      }
    } else if (pTag->stack[iLevel].ikey) {
      pTag->stack[iLevel].ikey--;
    } else {
      while (--iLevel >= 0) {
        hb_ntxPageRelease(pTag, pPage);
        pPage = hb_ntxPageLoad(pTag, pTag->stack[iLevel].page);
        if (!pPage) {
          return false;
        }
        if (pTag->stack[iLevel].ikey) {
          pTag->stack[iLevel].ikey--;
          break;
        }
      }
      if (iLevel < 0) {
        hb_ntxPageRelease(pTag, pPage);
        return false;
      }
      pTag->stackLevel = static_cast<HB_USHORTCAST>(iLevel + 1);
    }
    hb_ntxPageGetKey(pPage, pTag->stack[pTag->stackLevel - 1].ikey, pTag->CurKeyInfo, pTag->KeyLength);
    hb_ntxPageRelease(pTag, pPage);
    return true;
  }
  return false;
}

/*
 * find a key value in page
 */
static int hb_ntxPageKeyFind(LPTAGINFO pTag, LPPAGEINFO pPage, const char *key, HB_SHORT keylen, bool fNext,
                             HB_ULONG ulRecNo, bool *fStop)
{
  int iLast = -1, iBegin = 0, iEnd = pPage->uiKeys - 1;

  *fStop = false;
  while (iBegin <= iEnd) {
    int i, k;

    i = (iBegin + iEnd) >> 1;
    k = hb_ntxValCompare(pTag, key, keylen, hb_ntxGetKeyVal(pPage, i), pTag->KeyLength, false);
    if (k == 0) {
      if (ulRecNo != 0 && pTag->fSortRec) {
        HB_ULONG ulRec = hb_ntxGetKeyRec(pPage, i);
        if (ulRecNo < ulRec) {
          k = -1;
        } else if (ulRecNo > ulRec) {
          k = 1;
        } else {
          *fStop = true;
          return i;
        }
      }
    } else if (!pTag->AscendKey) {
      k = -k;
    }
    if (fNext ? k >= 0 : k > 0) {
      iBegin = i + 1;
    } else {
      if (k == 0 && !ulRecNo) {
        *fStop = true;
      }
      iLast = i;
      iEnd = i - 1;
    }
  }
  return iLast >= 0 ? iLast : static_cast<int>(pPage->uiKeys);
}

/*
 * find a record in page starting from given key
 */
static bool hb_ntxPageFindRecNo(LPPAGEINFO pPage, int *iStart, HB_ULONG ulRecno)
{
  int iKey = *iStart;

  while (iKey < pPage->uiKeys) {
    if (hb_ntxGetKeyRec(pPage, iKey) == ulRecno) {
      *iStart = iKey;
      return true;
    }
    iKey++;
  }
  return false;
}

/*
 * set page path to given key in tag
 */
static bool hb_ntxTagKeyFind(LPTAGINFO pTag, LPKEYINFO pKey, HB_USHORT uiLen)
{
  LPPAGEINFO pPage = nullptr;
  HB_ULONG ulPage = 0, ulRecNo = 0;
  int iKey;
  auto fStop = false;
  auto fNext = false;
  auto fPrev = false;
  auto fOut = false;

  if (pKey->Tag == NTX_MAX_REC_NUM) { /* for key add */
    if (pTag->fSortRec) {
      ulRecNo = pKey->Xtra;
    } else {
      fNext = true;
    }
  } else if (pKey->Xtra == NTX_MAX_REC_NUM) { /* for seek last */
    fNext = fPrev = true;
  } else if (pKey->Xtra != NTX_IGNORE_REC_NUM) { /* for key del and current key */
    ulRecNo = pKey->Xtra;
  }
  /* else -> normal seek */

  pTag->stackLevel = 0;
  do {
    if (pPage) {
      hb_ntxPageRelease(pTag, pPage);
    }
    pPage = hb_ntxPageLoad(pTag, ulPage);
    if (!pPage) {
      return false;
    }
    iKey = hb_ntxPageKeyFind(pTag, pPage, pKey->key, uiLen, fNext, ulRecNo, &fStop);
    hb_ntxTagSetPageStack(pTag, pPage->Page, static_cast<HB_USHORTCAST>(iKey));
    if (fStop && ulRecNo && pTag->fSortRec) {
      break;
    }
    ulPage = hb_ntxGetKeyPage(pPage, iKey);
  } while (ulPage != 0);

  if (ulRecNo && !pTag->fSortRec) { /* small hack - should speedup in some cases */
    if (hb_ntxPageFindRecNo(pPage, &iKey, ulRecNo)) {
      pTag->stack[pTag->stackLevel - 1].ikey = static_cast<HB_SHORTCAST>(iKey);
    }
  }

  hb_ntxPageGetKey(pPage, static_cast<HB_USHORTCAST>(iKey), pTag->CurKeyInfo, pTag->KeyLength);
  hb_ntxPageRelease(pTag, pPage);

  if (ulRecNo) {
    if (!pTag->fSortRec) {
      fStop = true;
      while (fStop && ulRecNo != pTag->CurKeyInfo->Xtra) {
        if (!hb_ntxTagNextKey(pTag)) { /* Tag EOF */
          fOut = true;
          fStop = false;
        } else {
          fStop = hb_ntxValCompare(pTag, pKey->key, uiLen, pTag->CurKeyInfo->key, pTag->KeyLength, false) == 0;
        }
      }
    }
  } else if (fPrev) {
    if (!hb_ntxTagPrevKey(pTag)) {
      fOut = true;
      fStop = false;
    } else {
      fStop = hb_ntxValCompare(pTag, pKey->key, uiLen, pTag->CurKeyInfo->key, pTag->KeyLength, false) == 0;
    }
  } else if (!fNext && !fStop && pTag->CurKeyInfo->Xtra == 0) {
    if (!hb_ntxTagNextKey(pTag)) { /* Tag EOF */
      fOut = true;
      fStop = false;
    } else {
      fStop = hb_ntxValCompare(pTag, pKey->key, uiLen, pTag->CurKeyInfo->key, pTag->KeyLength, false) == 0;
    }
  }

  pTag->TagBOF = pTag->TagEOF = fOut || pTag->CurKeyInfo->Xtra == 0;

  return fStop;
}

/*
 * set key in the given tag page
 */
static void hb_ntxPageKeySet(LPTAGINFO pTag, LPPAGEINFO pPage, HB_USHORT uiPos, HB_ULONG ulPage, HB_ULONG ulRec,
                             const char *keyVal)
{
  hb_ntxSetKeyPage(pPage, uiPos, ulPage);
  hb_ntxSetKeyRec(pPage, uiPos, ulRec);
  memcpy(hb_ntxGetKeyVal(pPage, uiPos), keyVal, pTag->KeyLength);
  pPage->Changed = true;
}

/*
 * add key to tag page
 */
static void hb_ntxPageKeyAdd(LPTAGINFO pTag, LPPAGEINFO pPage, HB_USHORT uiPos, HB_ULONG ulPage, HB_ULONG ulRec,
                             const char *keyVal)
{
  HB_USHORT ntmp = hb_ntxGetKeyOffset(pPage, pPage->uiKeys + 1);

  /* TODO?: update to keep last key pointer fixed */
  for (HB_USHORT u = pPage->uiKeys + 1; u > uiPos; u--) {
    hb_ntxSetKeyOffset(pPage, u, hb_ntxGetKeyOffset(pPage, u - 1));
  }
  hb_ntxSetKeyOffset(pPage, uiPos, ntmp);
  pPage->uiKeys++;

  hb_ntxPageKeySet(pTag, pPage, uiPos, ulPage, ulRec, keyVal);
#ifdef HB_NTX_DEBUG
  hb_ntxPageCheckKeys(pPage, pTag, uiPos, 41);
#endif
}

/*
 * del key from the page
 */
static void hb_ntxPageKeyDel(LPPAGEINFO pPage, HB_USHORT uiPos)
{
  HB_USHORT ntmp = hb_ntxGetKeyOffset(pPage, uiPos);

  /* TODO?: update to keep last key pointer fixed */
  for (HB_USHORT u = uiPos; u < pPage->uiKeys; u++) {
    hb_ntxSetKeyOffset(pPage, u, hb_ntxGetKeyOffset(pPage, u + 1));
  }
  hb_ntxSetKeyOffset(pPage, pPage->uiKeys, ntmp);

  pPage->uiKeys--;
  pPage->Changed = true;
}

/*
 * split single page into two and return key to the new one
 */
static LPKEYINFO hb_ntxPageSplit(LPTAGINFO pTag, LPPAGEINFO pPage, LPKEYINFO pKey, HB_USHORT uiPos)
{
  LPPAGEINFO pNewPage = hb_ntxPageNew(pTag, false);
  LPKEYINFO pKeyNew;
  HB_USHORT uiKeys = pPage->uiKeys + 1, uiLen = pTag->KeyLength + 8, i, j, u, uiHalf;
  HB_ULONG ulPage;

  if (!pNewPage) {
    return nullptr;
  }
  pKeyNew = hb_ntxKeyNew(nullptr, pTag->KeyLength);

  uiHalf = uiKeys >> 1;

  j = 0;
  while (pNewPage->uiKeys < uiHalf) {
    if (pNewPage->uiKeys == uiPos) {
      hb_ntxSetKeyPage(pNewPage, pNewPage->uiKeys, pKey->Tag);
      hb_ntxSetKeyRec(pNewPage, pNewPage->uiKeys, pKey->Xtra);
      memcpy(hb_ntxGetKeyVal(pNewPage, pNewPage->uiKeys), pKey->key, pTag->KeyLength);
    } else {
      memcpy(hb_ntxGetKeyPtr(pNewPage, pNewPage->uiKeys), hb_ntxGetKeyPtr(pPage, j), uiLen);
      j++;
    }
    pNewPage->uiKeys++;
  }

  if (uiHalf == uiPos) {
    pKeyNew->Xtra = pKey->Xtra;
    memcpy(pKeyNew->key, pKey->key, pTag->KeyLength);
    hb_ntxSetKeyPage(pNewPage, pNewPage->uiKeys, pKey->Tag);
  } else {
    pKeyNew->Xtra = hb_ntxGetKeyRec(pPage, j);
    memcpy(pKeyNew->key, hb_ntxGetKeyVal(pPage, j), pTag->KeyLength);
    hb_ntxSetKeyPage(pNewPage, pNewPage->uiKeys, hb_ntxGetKeyPage(pPage, j));
    j++;
  }
  pKeyNew->Tag = pNewPage->Page;

  i = 0;
  while (++uiHalf < uiKeys) {
    if (uiHalf == uiPos) {
      hb_ntxSetKeyPage(pPage, i, pKey->Tag);
      hb_ntxSetKeyRec(pPage, i, pKey->Xtra);
      memcpy(hb_ntxGetKeyVal(pPage, i), pKey->key, pTag->KeyLength);
    } else {
      u = hb_ntxGetKeyOffset(pPage, j);
      hb_ntxSetKeyOffset(pPage, j, hb_ntxGetKeyOffset(pPage, i));
      hb_ntxSetKeyOffset(pPage, i, u);
      j++;
    }
    i++;
  }
  ulPage = hb_ntxGetKeyPage(pPage, pPage->uiKeys);
  hb_ntxSetKeyPage(pPage, pPage->uiKeys, 0);
  hb_ntxSetKeyPage(pPage, i, ulPage);
  pPage->uiKeys = i;

  pPage->Changed = pNewPage->Changed = true;
#ifdef HB_NTX_DEBUG
  hb_ntxPageCheckKeys(pNewPage, pTag, uiPos, 1);
  hb_ntxPageCheckKeys(pPage, pTag, uiPos - pNewPage->uiKeys, 2);
#endif
  hb_ntxPageRelease(pTag, pNewPage);

  return pKeyNew;
}

/*
 * join two neighbour pages and update the parent page key
 */
static void hb_ntxPageJoin(LPTAGINFO pTag, LPPAGEINFO pBasePage, HB_USHORT uiPos, LPPAGEINFO pFirst, LPPAGEINFO pLast)
{
  HB_USHORT uiLen = pTag->KeyLength + 8;

  hb_ntxSetKeyRec(pFirst, pFirst->uiKeys, hb_ntxGetKeyRec(pBasePage, uiPos));
  memcpy(hb_ntxGetKeyVal(pFirst, pFirst->uiKeys), hb_ntxGetKeyVal(pBasePage, uiPos), pTag->KeyLength);
  pFirst->uiKeys++;
  hb_ntxPageKeyDel(pBasePage, uiPos);
  hb_ntxSetKeyPage(pBasePage, uiPos, pFirst->Page);
  for (HB_USHORT i = 0; i < pLast->uiKeys; i++) {
    memcpy(hb_ntxGetKeyPtr(pFirst, pFirst->uiKeys), hb_ntxGetKeyPtr(pLast, i), uiLen);
    pFirst->uiKeys++;
  }
  hb_ntxSetKeyPage(pFirst, pFirst->uiKeys, hb_ntxGetKeyPage(pLast, pLast->uiKeys));
  pLast->uiKeys = 0;
  hb_ntxPageFree(pTag, pLast);
  pFirst->Changed = pLast->Changed = true;
#ifdef HB_NTX_DEBUG
  hb_ntxPageCheckKeys(pBasePage, pTag, uiPos, 11);
  hb_ntxPageCheckKeys(pFirst, pTag, 0, 12);
#endif
}

/*
 * balance keys in two neighbour pages and update the parent page key
 */
static void hb_ntxBalancePages(LPTAGINFO pTag, LPPAGEINFO pBasePage, HB_USHORT uiPos, LPPAGEINFO pFirst,
                               LPPAGEINFO pLast)
{
  HB_USHORT uiLen = pTag->KeyLength + 8, n;
  int i, j, iMove = ((pFirst->uiKeys + pLast->uiKeys + 1) >> 1) - pFirst->uiKeys;

  /*
   * such situation should not exist even max keys, though it does not cost
   * much and I want to be able to call hb_ntxBalancePages() in any case for
   * some advanced balancing
   */
  if (iMove == 0) {
    return;
  }

#ifdef HB_NTX_DEBUG
  hb_ntxPageCheckKeys(pBasePage, pTag, uiPos, 31);
  hb_ntxPageCheckKeys(pFirst, pTag, iMove, 32);
  hb_ntxPageCheckKeys(pLast, pTag, iMove, 33);
#endif

  if (iMove > 0) {
    hb_ntxSetKeyRec(pFirst, pFirst->uiKeys, hb_ntxGetKeyRec(pBasePage, uiPos));
    memcpy(hb_ntxGetKeyVal(pFirst, pFirst->uiKeys), hb_ntxGetKeyVal(pBasePage, uiPos), pTag->KeyLength);
    pFirst->uiKeys++;
    i = 0;
    while (--iMove) {
      memcpy(hb_ntxGetKeyPtr(pFirst, pFirst->uiKeys), hb_ntxGetKeyPtr(pLast, i), uiLen);
      pFirst->uiKeys++;
      i++;
    }
    hb_ntxSetKeyRec(pBasePage, uiPos, hb_ntxGetKeyRec(pLast, i));
    memcpy(hb_ntxGetKeyVal(pBasePage, uiPos), hb_ntxGetKeyVal(pLast, i), pTag->KeyLength);
    hb_ntxSetKeyPage(pFirst, pFirst->uiKeys, hb_ntxGetKeyPage(pLast, i));
    i++;
    pLast->uiKeys -= static_cast<HB_USHORTCAST>(i);
    /* TODO?: update to keep last key pointer fixed */
    for (j = 0; j <= pLast->uiKeys; j++) {
      n = hb_ntxGetKeyOffset(pLast, j);
      hb_ntxSetKeyOffset(pLast, j, hb_ntxGetKeyOffset(pLast, j + i));
      hb_ntxSetKeyOffset(pLast, j + i, n);
    }
  } else {
    /* TODO?: update to keep last key pointer fixed */
    for (j = pLast->uiKeys; j >= 0; j--) {
      n = hb_ntxGetKeyOffset(pLast, j - iMove);
      hb_ntxSetKeyOffset(pLast, j - iMove, hb_ntxGetKeyOffset(pLast, j));
      hb_ntxSetKeyOffset(pLast, j, n);
    }
    i = -iMove - 1;
    hb_ntxSetKeyRec(pLast, i, hb_ntxGetKeyRec(pBasePage, uiPos));
    memcpy(hb_ntxGetKeyVal(pLast, i), hb_ntxGetKeyVal(pBasePage, uiPos), pTag->KeyLength);
    hb_ntxSetKeyPage(pLast, i, hb_ntxGetKeyPage(pFirst, pFirst->uiKeys));
    while (--i >= 0) {
      pFirst->uiKeys--;
      memcpy(hb_ntxGetKeyPtr(pLast, i), hb_ntxGetKeyPtr(pFirst, pFirst->uiKeys), uiLen);
    }
    pLast->uiKeys -= static_cast<HB_USHORTCAST>(iMove);
    pFirst->uiKeys--;
    hb_ntxSetKeyRec(pBasePage, uiPos, hb_ntxGetKeyRec(pFirst, pFirst->uiKeys));
    memcpy(hb_ntxGetKeyVal(pBasePage, uiPos), hb_ntxGetKeyVal(pFirst, pFirst->uiKeys), pTag->KeyLength);
  }
  pFirst->Changed = pLast->Changed = pBasePage->Changed = true;
#ifdef HB_NTX_DEBUG
  hb_ntxPageCheckKeys(pBasePage, pTag, uiPos, 21);
  hb_ntxPageCheckKeys(pFirst, pTag, iMove, 22);
  hb_ntxPageCheckKeys(pLast, pTag, iMove, 23);
#endif
}

/*
 * add key to the index at the current page path
 */
static bool hb_ntxTagKeyAdd(LPTAGINFO pTag, LPKEYINFO pKey)
{
  int iLevel, iKey;
  LPPAGEINFO pPage = nullptr;
  LPKEYINFO pNewKey = nullptr;
  auto fFound = false;
  auto fBottom = false;

  if (pTag->UniqueKey) {
    HB_ULONG ulRecNo = pKey->Xtra;

    pKey->Xtra = NTX_IGNORE_REC_NUM;
    fFound = hb_ntxTagKeyFind(pTag, pKey, pTag->KeyLength);
    pKey->Xtra = ulRecNo;
    if (fFound) {
      return false;
    }
    fBottom = true;
  } else {
    pKey->Tag = NTX_MAX_REC_NUM;
    fFound = hb_ntxTagKeyFind(pTag, pKey, pTag->KeyLength);
    pKey->Tag = 0;
    if (fFound) {
      if (pTag->MultiKey) {
        fBottom = true;
      } else {
        return false;
      }
    }
  }

  iLevel = pTag->stackLevel - 1;
  if (fBottom) {
    HB_ULONG ulPage;
    pPage = hb_ntxPageLoad(pTag, pTag->stack[iLevel].page);
    if (!pPage) {
      return false;
    }
    ulPage = hb_ntxGetKeyPage(pPage, pTag->stack[iLevel].ikey);
    if (ulPage) {
      hb_ntxPageRelease(pTag, pPage);
      pPage = hb_ntxPageBottomMove(pTag, ulPage);
      if (!pPage) {
        return false;
      }
      iLevel = pTag->stackLevel - 1;
      if (pTag->stack[iLevel].ikey < static_cast<HB_SHORT>(pPage->uiKeys)) {
        pTag->stack[iLevel].ikey++;
      }
    }
  }

  pTag->CurKeyInfo = hb_ntxKeyCopy(pTag->CurKeyInfo, pKey, pTag->KeyLength);

  while (iLevel >= 0 && pKey) {
    if (pPage) {
      hb_ntxPageRelease(pTag, pPage);
    }
    pPage = hb_ntxPageLoad(pTag, pTag->stack[iLevel].page);
    if (!pPage) {
      if (pNewKey) {
        hb_ntxKeyFree(pNewKey);
      }
      pTag->stackLevel = 0;
      return false;
    }
    iKey = pTag->stack[iLevel].ikey;
    if (pPage->uiKeys < pTag->MaxKeys) {
      hb_ntxPageKeyAdd(pTag, pPage, static_cast<HB_USHORTCAST>(iKey), pKey->Tag, pKey->Xtra, pKey->key);
      pKey = nullptr;
    } else {
      pTag->stackLevel = 0;
#if defined(HB_NTX_STRONG_BALANCE)
      if (iLevel > 0) {
        LPPAGEINFO pBasePage;
        HB_USHORT uiFirst, uiLast, uiBaseKey;
        pBasePage = hb_ntxPageLoad(pTag, pTag->stack[iLevel - 1].page);
        if (!pBasePage) {
          hb_ntxPageRelease(pTag, pPage);
          if (pNewKey) {
            hb_ntxKeyFree(pNewKey);
          }
          return false;
        }
        uiFirst = uiLast = uiBaseKey = pTag->stack[iLevel - 1].ikey;
        if (uiLast < pBasePage->uiKeys && hb_ntxGetKeyPage(pBasePage, uiLast + 1) != 0) {
          uiLast++;
        } else if (uiFirst > 0 && hb_ntxGetKeyPage(pBasePage, uiFirst - 1) != 0) {
          uiFirst--;
        }
        if (uiFirst != uiLast) {
          LPPAGEINFO pFirst, pLast;

          if (uiFirst == uiBaseKey) {
            pFirst = pPage;
            pLast = hb_ntxPageLoad(pTag, hb_ntxGetKeyPage(pBasePage, uiLast));
            if (!pLast) {
              hb_ntxPageRelease(pTag, pPage);
              hb_ntxPageRelease(pTag, pBasePage);
              if (pNewKey) {
                hb_ntxKeyFree(pNewKey);
              }
              return false;
            }
            uiBaseKey = static_cast<HB_USHORTCAST>(iKey);
          } else {
            pLast = pPage;
            pFirst = hb_ntxPageLoad(pTag, hb_ntxGetKeyPage(pBasePage, uiFirst));
            if (!pFirst) {
              hb_ntxPageRelease(pTag, pPage);
              hb_ntxPageRelease(pTag, pBasePage);
              if (pNewKey) {
                hb_ntxKeyFree(pNewKey);
              }
              return false;
            }
            uiBaseKey = pFirst->uiKeys + static_cast<HB_USHORTCAST>(iKey) + 1;
          }
          if ((pFirst->uiKeys + pLast->uiKeys) <= ((pTag->MaxKeys - 1) << 1)) {
            hb_ntxBalancePages(pTag, pBasePage, uiFirst, pFirst, pLast);
            if (pFirst->uiKeys >= uiBaseKey) {
              hb_ntxPageKeyAdd(pTag, pFirst, uiBaseKey, pKey->Tag, pKey->Xtra, pKey->key);
            } else {
              hb_ntxPageKeyAdd(pTag, pLast, uiBaseKey - pFirst->uiKeys - 1, pKey->Tag, pKey->Xtra, pKey->key);
            }
            pKey = nullptr;
          }
          if (pFirst != pPage) {
            hb_ntxPageRelease(pTag, pFirst);
          } else {
            hb_ntxPageRelease(pTag, pLast);
          }
          hb_ntxPageRelease(pTag, pBasePage);
          if (!pKey) {
            break;
          }
        }
      }
#endif
      pKey = hb_ntxPageSplit(pTag, pPage, pKey, static_cast<HB_USHORTCAST>(iKey));
      if (pNewKey) {
        hb_ntxKeyFree(pNewKey);
      }
      pNewKey = pKey;
    }
    iLevel--;
  }
  hb_ntxPageRelease(pTag, pPage);
  if (pKey) {
    pPage = hb_ntxPageNew(pTag, false);
    if (!pPage) {
      return false;
    }
    hb_ntxPageKeyAdd(pTag, pPage, 0, pKey->Tag, pKey->Xtra, pKey->key);
    hb_ntxSetKeyPage(pPage, 1, pTag->RootBlock);
    pTag->RootBlock = pPage->Page;
    pTag->HdrChanged = true;
    hb_ntxPageRelease(pTag, pPage);
    pTag->stackLevel = 0;
  }
  if (pNewKey) {
    hb_ntxKeyFree(pNewKey);
  }
  return true;
}

/*
 * del key at the current page path from the index
 */
static bool hb_ntxTagKeyDel(LPTAGINFO pTag, LPKEYINFO pKey)
{
  int iLevel, iBaseKey, iKey;
  LPPAGEINFO pBasePage, pPage;
  HB_ULONG ulPage;

  pKey->Tag = 0;
  if (pTag->stackLevel == 0 || pTag->CurKeyInfo->Xtra != pKey->Xtra ||
      memcmp(pTag->CurKeyInfo->key, pKey->key, pTag->KeyLength) != 0) {
    if (!hb_ntxTagKeyFind(pTag, pKey, pTag->KeyLength)) {
      return false;
    }
  }

  iLevel = pTag->stackLevel - 1;

  pPage = hb_ntxPageLoad(pTag, pTag->stack[iLevel].page);
  if (!pPage) {
    return false;
  }
  iKey = pTag->stack[iLevel].ikey;
  ulPage = hb_ntxGetKeyPage(pPage, iKey);

  if (ulPage) {
    pBasePage = pPage;
    iBaseKey = iKey;
    pPage = hb_ntxPageBottomMove(pTag, ulPage);
    if (!pPage) {
      hb_ntxPageRelease(pTag, pBasePage);
      return false;
    }
    iLevel = pTag->stackLevel - 1;
    iKey = pTag->stack[iLevel].ikey;

    hb_ntxSetKeyRec(pBasePage, iBaseKey, hb_ntxGetKeyRec(pPage, iKey));
    memcpy(hb_ntxGetKeyVal(pBasePage, iBaseKey), hb_ntxGetKeyVal(pPage, iKey), pTag->KeyLength);
    pBasePage->Changed = true;
#ifdef HB_NTX_DEBUG
    hb_ntxPageCheckKeys(pBasePage, pTag, iBaseKey, 61);
#endif
    hb_ntxPageRelease(pTag, pBasePage);
  }
  hb_ntxPageKeyDel(pPage, static_cast<HB_USHORTCAST>(iKey));

  while (iLevel > 0) {
    if (pPage->uiKeys < (pTag->MaxKeys >> 1)) {
      HB_USHORT uiFirst, uiLast, uiBaseKey;

      pBasePage = hb_ntxPageLoad(pTag, pTag->stack[iLevel - 1].page);
      if (!pBasePage) {
        hb_ntxPageRelease(pTag, pPage);
        return false;
      }
      uiFirst = uiLast = uiBaseKey = pTag->stack[iLevel - 1].ikey;
      if (uiLast < pBasePage->uiKeys && hb_ntxGetKeyPage(pBasePage, uiLast + 1) != 0) {
        uiLast++;
      } else if (uiFirst > 0 && hb_ntxGetKeyPage(pBasePage, uiFirst - 1) != 0) {
        uiFirst--;
      }

      if (uiFirst == uiLast) {
        if (pPage->uiKeys == 0) {
          hb_ntxSetKeyPage(pBasePage, uiBaseKey, 0);
          hb_ntxPageFree(pTag, pPage);
        }
        hb_ntxPageRelease(pTag, pPage);
      } else {
        LPPAGEINFO pFirst, pLast;

        if (uiFirst == uiBaseKey) {
          pFirst = pPage;
          pLast = hb_ntxPageLoad(pTag, hb_ntxGetKeyPage(pBasePage, uiLast));
          if (!pLast) {
            hb_ntxPageRelease(pTag, pPage);
            hb_ntxPageRelease(pTag, pBasePage);
            pTag->stackLevel = 0;
            return false;
          }
        } else {
          pLast = pPage;
          pFirst = hb_ntxPageLoad(pTag, hb_ntxGetKeyPage(pBasePage, uiFirst));
          if (!pFirst) {
            hb_ntxPageRelease(pTag, pPage);
            hb_ntxPageRelease(pTag, pBasePage);
            pTag->stackLevel = 0;
            return false;
          }
        }
        if (pFirst->uiKeys + pLast->uiKeys < pTag->MaxKeys) {
          hb_ntxPageJoin(pTag, pBasePage, uiFirst, pFirst, pLast);
        } else {
          hb_ntxBalancePages(pTag, pBasePage, uiFirst, pFirst, pLast);
        }
        hb_ntxPageRelease(pTag, pFirst);
        hb_ntxPageRelease(pTag, pLast);
      }
      pPage = pBasePage;
    } else {
      break;
    }
    iLevel--;
  }

  if (pPage->uiKeys == 0 && pPage->Page == pTag->RootBlock) {
    ulPage = hb_ntxGetKeyPage(pPage, 0);
    if (ulPage != 0) {
      pTag->RootBlock = ulPage;
      pTag->HdrChanged = true;
      hb_ntxPageFree(pTag, pPage);
    }
  }
  hb_ntxPageRelease(pTag, pPage);
  pTag->stackLevel = 0;
  return true;
}

/*
 * Skip in tag respecting record filter only
 */
static void hb_ntxTagSkipFilter(LPTAGINFO pTag, bool fForward)
{
  auto fBack = false;
  bool fEof = fForward ? pTag->TagEOF : pTag->TagBOF;

  fBack = pTag->fUsrDescend == pTag->AscendKey ? fForward : !fForward;

  while (!fEof && !hb_ntxCheckRecordScope(pTag->pIndex->pArea, pTag->CurKeyInfo->Xtra)) {
    if (fBack) {
      fEof = !hb_ntxTagPrevKey(pTag);
    } else {
      fEof = !hb_ntxTagNextKey(pTag);
    }

    if (!fEof && !hb_ntxKeyInScope(pTag, pTag->CurKeyInfo)) {
      fEof = true;
    }
  }
  if (fEof) {
    if (fForward) {
      pTag->TagEOF = true;
    } else {
      pTag->TagBOF = true;
    }
  }
}

/*
 * go to the first visible record in Tag
 */
static void hb_ntxTagGoTop(LPTAGINFO pTag)
{
  PHB_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->bottom : &pTag->top;

  if (pScope->scopeKeyLen) {
    hb_ntxTagKeyFind(pTag, pScope->scopeKey, pScope->scopeKeyLen);
  } else if (pTag->fUsrDescend == pTag->AscendKey) {
    hb_ntxTagBottomKey(pTag);
  } else {
    hb_ntxTagTopKey(pTag);
  }

  pTag->TagEOF = pTag->CurKeyInfo->Xtra == 0 || !hb_ntxKeyInScope(pTag, pTag->CurKeyInfo);

  if (!pTag->TagEOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter) {
    hb_ntxTagSkipFilter(pTag, true);
  }

  pTag->TagBOF = pTag->TagEOF;
}

/*
 * go to the last visible record in Tag
 */
static void hb_ntxTagGoBottom(LPTAGINFO pTag)
{
  PHB_NTXSCOPE pScope = pTag->fUsrDescend ? &pTag->top : &pTag->bottom;

  if (pScope->scopeKeyLen) {
    hb_ntxTagKeyFind(pTag, pScope->scopeKey, pScope->scopeKeyLen);
  } else if (pTag->fUsrDescend == pTag->AscendKey) {
    hb_ntxTagTopKey(pTag);
  } else {
    hb_ntxTagBottomKey(pTag);
  }

  pTag->TagBOF = pTag->CurKeyInfo->Xtra == 0 || !hb_ntxKeyInScope(pTag, pTag->CurKeyInfo);

  if (!pTag->TagBOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter) {
    hb_ntxTagSkipFilter(pTag, false);
  }

  pTag->TagEOF = pTag->TagBOF;
}

/*
 * skip to Next Key in the Tag
 */
static void hb_ntxTagSkipNext(LPTAGINFO pTag)
{
  pTag->TagBOF = false;

  if (pTag->stackLevel == 0) {
    pTag->TagEOF = true;
  } else if (!hb_ntxInTopScope(pTag, pTag->CurKeyInfo->key)) {
    hb_ntxTagGoTop(pTag);
  } else if (pTag->fUsrDescend == pTag->AscendKey) {
    pTag->TagEOF = !hb_ntxTagPrevKey(pTag);
  } else {
    pTag->TagEOF = !hb_ntxTagNextKey(pTag);
  }

  if (!pTag->TagEOF && !hb_ntxKeyInScope(pTag, pTag->CurKeyInfo)) {
    pTag->TagEOF = true;
  }

  if (!pTag->TagEOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter) {
    hb_ntxTagSkipFilter(pTag, true);
  }
}

/*
 * skip to Previous Key in the Tag
 */
static void hb_ntxTagSkipPrev(LPTAGINFO pTag)
{
  pTag->TagEOF = false;

  if (pTag->stackLevel == 0) {
    /* TODO?: check if this is NTX behavior,
       for sure CDX works in such way */
    hb_ntxTagGoBottom(pTag);
  } else if (pTag->fUsrDescend == pTag->AscendKey) {
    pTag->TagBOF = !hb_ntxTagNextKey(pTag);
  } else {
    pTag->TagBOF = !hb_ntxTagPrevKey(pTag);
  }

  if (!pTag->TagBOF && !hb_ntxKeyInScope(pTag, pTag->CurKeyInfo)) {
    pTag->TagBOF = true;
  }

  if (!pTag->TagBOF && pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter) {
    hb_ntxTagSkipFilter(pTag, false);
  }
}

/*
 * count keys in the given page and all subpages
 */
static HB_ULONG hb_ntxPageCountKeys(LPTAGINFO pTag, HB_ULONG ulPage)
{
  LPPAGEINFO pPage = hb_ntxPageLoad(pTag, ulPage);
  HB_ULONG ulKeys;

  if (!pPage) {
    return 0;
  }

  ulKeys = pPage->uiKeys;
  for (HB_USHORT u = 0; u <= pPage->uiKeys; u++) {
    ulPage = hb_ntxGetKeyPage(pPage, u);
    if (ulPage) {
      ulKeys += hb_ntxPageCountKeys(pTag, ulPage);
    }
  }
  hb_ntxPageRelease(pTag, pPage);

  return ulKeys;
}

/*
 * count relative position of current location in page stack
 */
static double hb_ntxTagCountRelKeyPos(LPTAGINFO pTag)
{
  int iLevel = pTag->stackLevel;
  double dPos = 1.0;

  while (--iLevel >= 0) {
    LPPAGEINFO pPage = hb_ntxPageLoad(pTag, pTag->stack[iLevel].page);
    int iKeys;
    if (!pPage) {
      break;
    }
    iKeys = pPage->uiKeys;
    if (hb_ntxGetKeyPage(pPage, pPage->uiKeys)) {
      ++iKeys;
    } else if (iLevel == pTag->stackLevel - 1) {
      dPos = 0.5;
    }
    if (iKeys) {
      dPos = (dPos + pTag->stack[iLevel].ikey) / iKeys;
    }
    hb_ntxPageRelease(pTag, pPage);
  }
  if (pTag->fUsrDescend == pTag->AscendKey) {
    dPos = 1.0 - dPos;
  }
  return dPos;
}

static void hb_ntxTagGoToRelKeyPos(LPTAGINFO pTag, double dPos)
{
  LPPAGEINFO pPage = nullptr;
  HB_ULONG ulPage = 0;
  int iKey, iKeys;

  if (pTag->fUsrDescend == pTag->AscendKey) {
    dPos = 1.0 - dPos;
  }

  pTag->stackLevel = 0;
  do {
    if (pPage) {
      hb_ntxPageRelease(pTag, pPage);
    }
    pPage = hb_ntxPageLoad(pTag, ulPage);
    if (!pPage) {
      pTag->stackLevel = 0;
      return;
    }
    if (pPage->uiKeys == 0) {
      iKey = 0;
    } else {
      iKeys = pPage->uiKeys;
      if (hb_ntxGetKeyPage(pPage, pPage->uiKeys)) {
        ++iKeys;
      }
      iKey = static_cast<int>(dPos * iKeys);
      if (iKey >= iKeys) {
        iKey = iKeys - 1;
      }
      dPos = dPos * iKeys - iKey;
      if (dPos <= 0.0) {
        dPos = 0.0;
      } else if (dPos >= 1.0) {
        dPos = 1.0;
      }
    }
    hb_ntxTagSetPageStack(pTag, pPage->Page, static_cast<HB_USHORTCAST>(iKey));
    ulPage = hb_ntxGetKeyPage(pPage, iKey);
  } while (ulPage != 0);

  hb_ntxPageGetKey(pPage, static_cast<HB_USHORTCAST>(iKey), pTag->CurKeyInfo, pTag->KeyLength);
  hb_ntxPageRelease(pTag, pPage);

  if (dPos > 0.75) {
    hb_ntxTagNextKey(pTag);
  } else if (dPos < 0.25) {
    hb_ntxTagPrevKey(pTag);
  }
}

/*
 * refresh CurKey value and set proper path from RootPage to LeafPage
 */
static bool hb_ntxCurKeyRefresh(LPTAGINFO pTag)
{
  NTXAREAP pArea = pTag->pIndex->pArea;

  if (pArea->dbfarea.lpdbPendingRel) {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  if (!pArea->dbfarea.fPositioned) {
    pTag->stackLevel = 0;
    pTag->TagBOF = pTag->TagEOF = true;
    pTag->CurKeyInfo->Xtra = 0;
    return false;
  } else if (pTag->stackLevel == 0 || pTag->CurKeyInfo->Xtra != pArea->dbfarea.ulRecNo) {
    HB_BYTE buf[NTX_MAX_KEY];
    auto fBuf = false;
    LPKEYINFO pKey = nullptr;
    /* Try to find previous if it's key for the same record */
    if (pTag->CurKeyInfo->Xtra == pArea->dbfarea.ulRecNo) {
      fBuf = true;
      memcpy(buf, pTag->CurKeyInfo->key, pTag->KeyLength);
      pKey = hb_ntxKeyCopy(pKey, pTag->CurKeyInfo, pTag->KeyLength);
      hb_ntxTagKeyFind(pTag, pKey, pTag->KeyLength);
    }
    if (pTag->CurKeyInfo->Xtra != pArea->dbfarea.ulRecNo) {
      bool fValidBuf = pArea->dbfarea.fValidBuffer;
      /* not found, create new key from DBF and if differs seek again */
      pKey = hb_ntxEvalKey(pKey, pTag);
      if (!fBuf || memcmp(buf, pKey->key, pTag->KeyLength) != 0) {
        hb_ntxTagKeyFind(pTag, pKey, pTag->KeyLength);
      }
      /* not found, if key was generated from DBF buffer then force to
       * update it, create the new key and if differs seek again */
      if (pTag->CurKeyInfo->Xtra != pArea->dbfarea.ulRecNo && fValidBuf) {
        SELF_GOTO(&pArea->dbfarea.area, pArea->dbfarea.ulRecNo);
        memcpy(buf, pKey->key, pTag->KeyLength);
        pKey = hb_ntxEvalKey(pKey, pTag);
        if (memcmp(buf, pKey->key, pTag->KeyLength) != 0) {
          hb_ntxTagKeyFind(pTag, pKey, pTag->KeyLength);
        }
      }
      if (pTag->CurKeyInfo->Xtra != pArea->dbfarea.ulRecNo && pTag->Template) {
        hb_ntxTagGoTop(pTag);
        while (!pTag->TagEOF) {
          if (pTag->CurKeyInfo->Xtra == pArea->dbfarea.ulRecNo) {
            break;
          }
          hb_ntxTagSkipNext(pTag);
        }
      }
    }
    hb_ntxKeyFree(pKey);
    return pTag->CurKeyInfo->Xtra != 0 && pTag->CurKeyInfo->Xtra == pArea->dbfarea.ulRecNo;
  }
  pTag->TagBOF = pTag->TagEOF = false;
  return true;
}

/*
 * free pages allocated by tag
 */
static bool hb_ntxTagPagesFree(LPTAGINFO pTag, HB_ULONG ulPage)
{
  LPPAGEINFO pPage = hb_ntxPageLoad(pTag, ulPage);
  bool fOK = pPage != nullptr;

  for (HB_USHORT u = 0; fOK && u <= pPage->uiKeys; u++) {
    ulPage = hb_ntxGetKeyPage(pPage, u);
    if (ulPage) {
      fOK = hb_ntxTagPagesFree(pTag, ulPage);
    }
  }

  if (fOK) {
    pPage->uiKeys = 0;
    hb_ntxPageFree(pTag, pPage);
    if (!pPage->pPrev) {
      fOK = hb_ntxPageSave(pTag->pIndex, pPage);
    }
  }
  hb_ntxPageRelease(pTag, pPage);

  return fOK;
}

/*
 * free space allocated by tag
 */
static HB_ERRCODE hb_ntxTagSpaceFree(LPTAGINFO pTag)
{
  if (hb_ntxTagHeaderCheck(pTag)) {
    if (pTag->RootBlock) {
      if (!hb_ntxTagPagesFree(pTag, pTag->RootBlock)) {
        return Harbour::FAILURE;
      }
    }
    hb_ntxPageAddFree(pTag, pTag->HeadBlock);
    hb_ntxIndexTagDel(pTag->pIndex, pTag->TagName);
    pTag->pIndex->Changed = true;
  }
  hb_ntxTagDelete(pTag);
  return Harbour::SUCCESS;
}

/*
 * create index file name
 */
static void hb_ntxCreateFName(NTXAREAP pArea, const char *szBagName, bool *fProd, char *szFileName, char *szTagName)
{
  PHB_FNAME pFileName;
  PHB_ITEM pExt = nullptr;
  bool fName = szBagName && *szBagName;

  pFileName = hb_fsFNameSplit(fName ? szBagName : pArea->dbfarea.szDataFileName);

  if (szTagName != nullptr) {
    if (pFileName->szName) {
      hb_strncpyUpperTrim(szTagName, pFileName->szName, NTX_MAX_TAGNAME);
    } else {
      szTagName[0] = '\0';
    }
  }

  if (!fName || (!pFileName->szExtension && hb_setGetDefExtension())) {
    DBORDERINFO pExtInfo{};
    pExt = pExtInfo.itmResult = hb_itemPutC(nullptr, nullptr);
    if (SELF_ORDINFO(&pArea->dbfarea.area, DBOI_BAGEXT, &pExtInfo) == Harbour::SUCCESS && hb_itemGetCLen(pExt) > 0) {
      pFileName->szExtension = hb_itemGetCPtr(pExt);
    }
  }
  hb_fsFNameMerge(szFileName, pFileName);

  if (fProd) {
    if (!pFileName->szName) {
      *fProd = false;
    } else if (!fName) {
      *fProd = true;
    } else {
      PHB_FNAME pTableFileName = hb_fsFNameSplit(pArea->dbfarea.szDataFileName);

      *fProd = pTableFileName->szName && hb_stricmp(pTableFileName->szName, pFileName->szName) == 0;
      if (*fProd && pFileName->szExtension && !pExt) {
        DBORDERINFO pExtInfo{};
        pExt = pExtInfo.itmResult = hb_itemPutC(nullptr, nullptr);
        if (SELF_ORDINFO(&pArea->dbfarea.area, DBOI_BAGEXT, &pExtInfo) == Harbour::SUCCESS) {
          *fProd = hb_stricmp(pFileName->szExtension, hb_itemGetCPtr(pExt)) == 0;
        }
      }
      hb_xfree(pTableFileName);
    }
  }
  hb_xfree(pFileName);
  if (pExt) {
    hb_itemRelease(pExt);
  }
}

/*
 * find order bag by its name
 */
static LPNTXINDEX hb_ntxFindBag(NTXAREAP pArea, const char *szBagName)
{
  LPNTXINDEX pIndex;
  PHB_FNAME pSeek;

  pSeek = hb_fsFNameSplit(szBagName);
  if (!pSeek->szName) {
    pSeek->szName = "";
  }

  pIndex = pArea->lpIndexes;
  while (pIndex) {
    auto fFound = false;
    PHB_FNAME pName = hb_fsFNameSplit(pIndex->IndexName);
    if (!pName->szName) {
      pName->szName = "";
    }
    fFound = !hb_stricmp(pName->szName, pSeek->szName) &&
             (!pSeek->szPath || (pName->szPath && !hb_stricmp(pName->szPath, pSeek->szPath))) &&
             (!pSeek->szExtension || (pName->szExtension && !hb_stricmp(pName->szExtension, pSeek->szExtension)));
    hb_xfree(pName);
    if (fFound) {
      break;
    }
    pIndex = pIndex->pNext;
  }
  hb_xfree(pSeek);
  return pIndex;
}

/*
 * Find tag by name in index bag
 */
static int hb_ntxFindTagByName(LPNTXINDEX pIndex, const char *szTag)
{
  for (auto i = 0; i < pIndex->iTags; i++) {
    if (!hb_strnicmp(pIndex->lpTags[i]->TagName, szTag, NTX_MAX_TAGNAME)) {
      return i + 1;
    }
  }
  return 0;
}

/*
 * Find the tag by its name or number
 */
static LPTAGINFO hb_ntxFindTag(NTXAREAP pArea, PHB_ITEM pTagItem, PHB_ITEM pBagItem)
{
  LPNTXINDEX pIndex;
  auto fBag = false;

  if (!pTagItem || (hb_itemType(pTagItem) & (Harbour::Item::STRING | Harbour::Item::NUMERIC)) == 0) {
    return pArea->lpCurTag;
  }

  fBag = pTagItem->isString() && hb_itemGetCLen(pBagItem) > 0;
  if (fBag) {
    pIndex = hb_ntxFindBag(pArea, hb_itemGetCPtr(pBagItem));
  } else {
    int iBag = hb_itemGetNI(pBagItem);

    pIndex = pArea->lpIndexes;
    if (iBag > 0) {
      fBag = true;
      while (pIndex) {
        if (--iBag == 0) {
          break;
        }
        pIndex = pIndex->pNext;
      }
    } else if (iBag < 0) {
      pIndex = nullptr;
    }
  }
  if (pIndex) {
    if (hb_itemType(pTagItem) & Harbour::Item::STRING) {
      auto szTag = pTagItem->getCPtr();
      int iTag;

      if (fBag) {
        iTag = hb_ntxFindTagByName(pIndex, szTag);
      } else {
        do {
          iTag = hb_ntxFindTagByName(pIndex, szTag);
          if (iTag) {
            break;
          }
          pIndex = pIndex->pNext;
        } while (pIndex);
      }
      if (iTag) {
        return pIndex->lpTags[iTag - 1];
      }
    } else {
      int i = hb_itemGetNI(pTagItem) - 1;

      if (i >= 0) {
        if (fBag) {
          if (i < pIndex->iTags) {
            return pIndex->lpTags[i];
          }
        } else {
          do {
            if (i < pIndex->iTags) {
              return pIndex->lpTags[i];
            }
            i -= pIndex->iTags;
            pIndex = pIndex->pNext;
          } while (pIndex);
        }
      }
    }
  }

  return nullptr;
}

/*
 * find the given tag number
 */
static int hb_ntxFindTagNum(NTXAREAP pArea, LPTAGINFO pTag)
{
  if (pArea->fSetTagNumbers) {
    LPNTXINDEX pIndex = pArea->lpIndexes;
    HB_USHORT uiNum = 0;

    pTag->uiNumber = 0;
    while (pIndex) {
      for (HB_USHORT i = 0; i < pIndex->iTags; i++) {
        pIndex->lpTags[i]->uiNumber = ++uiNum;
      }
      pIndex = pIndex->pNext;
    }
    pArea->fSetTagNumbers = false;
  }
  return pTag->uiNumber;
}

/*
 * count number of tags
 */
static int hb_ntxTagCount(NTXAREAP pArea)
{
  LPNTXINDEX pIndex = pArea->lpIndexes;
  int i = 0;

  while (pIndex) {
    i += pIndex->iTags;
    pIndex = pIndex->pNext;
  }

  return i;
}

/*
 * count number of keys in given tag
 */
static HB_ULONG hb_ntxOrdKeyCount(LPTAGINFO pTag)
{
  HB_ULONG ulKeyCount = 0;

  if (!pTag->pIndex->fShared && pTag->keyCount && !pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter) {
    return pTag->keyCount;
  }

  if (hb_ntxTagLockRead(pTag)) {
    hb_ntxTagRefreshScope(pTag);

    if (pTag->top.scopeKeyLen || pTag->bottom.scopeKeyLen || pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter) {
      hb_ntxTagGoTop(pTag);
      while (!pTag->TagEOF) {
        ulKeyCount++;
        hb_ntxTagSkipNext(pTag);
      }
    } else {
      ulKeyCount = hb_ntxPageCountKeys(pTag, 0);
    }
    if (!pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter) {
      pTag->keyCount = ulKeyCount;
    }
    hb_ntxTagUnLockRead(pTag);
  }

  return ulKeyCount;
}

/*
 * get the logical key position in the given tag
 */
static HB_ULONG hb_ntxOrdKeyNo(LPTAGINFO pTag)
{
  HB_ULONG ulKeyNo = 0;

  if (hb_ntxTagLockRead(pTag)) {
    hb_ntxTagRefreshScope(pTag);
    if (hb_ntxCurKeyRefresh(pTag)) {
      if (pTag->top.scopeKeyLen || pTag->bottom.scopeKeyLen || pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter) {
        if (hb_ntxKeyInScope(pTag, pTag->CurKeyInfo)) {
          do {
            ulKeyNo++;
            hb_ntxTagSkipPrev(pTag);
          } while (!pTag->TagBOF);
        }
      } else {
        int iLevel = pTag->stackLevel, iKey, iFirst = 1;
        bool fBack = pTag->fUsrDescend == pTag->AscendKey;
        HB_ULONG ulPage;

        while (--iLevel >= 0) {
          LPPAGEINFO pPage = hb_ntxPageLoad(pTag, pTag->stack[iLevel].page);
          if (!pPage) {
            break;
          }
          if (fBack) {
            iKey = pTag->stack[iLevel].ikey;
            ulKeyNo += pPage->uiKeys - iKey;
            while (++iKey <= pPage->uiKeys) {
              ulPage = hb_ntxGetKeyPage(pPage, iKey);
              if (ulPage) {
                ulKeyNo += hb_ntxPageCountKeys(pTag, ulPage);
              }
            }
          } else {
            ulKeyNo += iKey = pTag->stack[iLevel].ikey + iFirst;
            iFirst = 0;
            while (--iKey >= 0) {
              ulPage = hb_ntxGetKeyPage(pPage, iKey);
              if (ulPage) {
                ulKeyNo += hb_ntxPageCountKeys(pTag, ulPage);
              }
            }
          }
          hb_ntxPageRelease(pTag, pPage);
        }
      }
    }
    hb_ntxTagUnLockRead(pTag);
  }
  return ulKeyNo;
}

/*
 * set logical key position in given tag
 */
static bool hb_ntxOrdKeyGoto(LPTAGINFO pTag, HB_ULONG ulKeyNo)
{
  NTXAREAP pArea = pTag->pIndex->pArea;

  if (!ulKeyNo || !hb_ntxTagLockRead(pTag)) {
    return false;
  }

  hb_ntxTagRefreshScope(pTag);
  hb_ntxTagGoTop(pTag);
  while (!pTag->TagEOF && --ulKeyNo) {
    hb_ntxTagSkipNext(pTag);
  }

  if (pTag->TagEOF) {
    SELF_GOTO(&pArea->dbfarea.area, 0);
  } else {
    LPTAGINFO pSavedTag = pArea->lpCurTag;
    pArea->lpCurTag = pTag;
    if (SELF_GOTO(&pArea->dbfarea.area, pTag->CurKeyInfo->Xtra) == Harbour::SUCCESS) {
      SELF_SKIPFILTER(&pArea->dbfarea.area, 1);
    }
    pArea->lpCurTag = pSavedTag;
  }
  hb_ntxTagUnLockRead(pTag);
  return true;
}

/*
 * get the relative key position (from 0.0 to 1.0) in the given tag
 */
static double hb_ntxOrdGetRelKeyPos(LPTAGINFO pTag)
{
  double dPos = 0.0, dStart = 0.0, dStop = 1.0, dFact = 0.0000000000001;
  auto fOK = true;
  bool fFilter = pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter;

  if (!hb_ntxTagLockRead(pTag)) {
    return false;
  }

  hb_ntxTagRefreshScope(pTag);

  pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter = false;
  if (pTag->fUsrDescend ? pTag->bottom.scopeKeyLen : pTag->top.scopeKeyLen) {
    hb_ntxTagGoTop(pTag);
    if (pTag->TagEOF) {
      fOK = false;
    } else {
      dStart = hb_ntxTagCountRelKeyPos(pTag);
    }
  }
  if (fOK && (pTag->fUsrDescend ? pTag->top.scopeKeyLen : pTag->bottom.scopeKeyLen)) {
    hb_ntxTagGoBottom(pTag);
    if (pTag->TagBOF) {
      fOK = false;
    } else {
      dStop = hb_ntxTagCountRelKeyPos(pTag);
    }
  }
  pTag->pIndex->pArea->dbfarea.area.dbfi.fFilter = fFilter;

  if (fOK) {
    if (hb_ntxCurKeyRefresh(pTag) && hb_ntxKeyInScope(pTag, pTag->CurKeyInfo)) {
      if (dStart >= dStop - dFact) {
        dPos = 0.5;
      } else {
        dPos = hb_ntxTagCountRelKeyPos(pTag);
        dPos = (dPos - dStart) / (dStop - dStart);
        /* fix possible differences in FL representation */
        if (dPos <= 0.0) {
          dPos = 0.0;
        } else if (dPos >= 1.0) {
          dPos = 1.0;
        }
      }
    }
  }
  hb_ntxTagUnLockRead(pTag);

  return dPos;
}

/*
 * set the relative key position (from 0.0 to 1.0) in the given tag
 */
static void hb_ntxOrdSetRelKeyPos(LPTAGINFO pTag, double dPos)
{
  if (hb_ntxTagLockRead(pTag)) {
    NTXAREAP pArea = pTag->pIndex->pArea;
    double dStart = 0.0, dStop = 1.0, dFact = 0.0000000000001;
    auto fOK = true;
    bool fFilter = pArea->dbfarea.area.dbfi.fFilter;
    auto fForward = true;
    auto fTop = false;

    hb_ntxTagRefreshScope(pTag);

    if (dPos >= 1.0) {
      fForward = false;
    } else if (dPos <= 0.0) {
      fTop = true;
    } else {
      pArea->dbfarea.area.dbfi.fFilter = false;
      if (pTag->fUsrDescend ? pTag->bottom.scopeKeyLen : pTag->top.scopeKeyLen) {
        hb_ntxTagGoTop(pTag);
        if (pTag->TagEOF) {
          fOK = false;
        } else {
          dStart = hb_ntxTagCountRelKeyPos(pTag);
        }
      }
      if (fOK && (pTag->fUsrDescend ? pTag->top.scopeKeyLen : pTag->bottom.scopeKeyLen)) {
        hb_ntxTagGoBottom(pTag);
        if (pTag->TagBOF) {
          fOK = false;
        } else {
          dStop = hb_ntxTagCountRelKeyPos(pTag);
        }
      }
      pArea->dbfarea.area.dbfi.fFilter = fFilter;

      if (fOK) {
        if (dStart >= dStop - dFact) {
          fTop = true;
        } else {
          dPos = dPos * (dStop - dStart) + dStart;
          hb_ntxTagGoToRelKeyPos(pTag, dPos);
          if (pTag->CurKeyInfo->Xtra == 0) {
            fForward = false;
          } else if (!hb_ntxInTopScope(pTag, pTag->CurKeyInfo->key)) {
            fTop = true;
          } else if (!hb_ntxInBottomScope(pTag, pTag->CurKeyInfo->key)) {
            fForward = false;
          }
        }
      }
    }
    if (!fOK) {
      SELF_GOTO(&pArea->dbfarea.area, 0);
    } else {
      LPTAGINFO pSavedTag = pArea->lpCurTag;
      pArea->lpCurTag = pTag;

      pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = false;

      if (fForward) {
        if (fTop) {
          hb_ntxTagGoTop(pTag);
        }
        if (pTag->CurKeyInfo->Xtra != 0) {
          if (SELF_GOTO(&pArea->dbfarea.area, pTag->CurKeyInfo->Xtra) == Harbour::SUCCESS) {
            SELF_SKIPFILTER(&pArea->dbfarea.area, 1);
            if (pArea->dbfarea.area.fEof && !fTop) {
              fForward = false;
            }
          }
        } else if (fTop) {
          SELF_GOTO(&pArea->dbfarea.area, 0);
        } else {
          fForward = false;
        }
      }
      if (!fForward) {
        hb_ntxTagGoBottom(pTag);
        if (SELF_GOTO(&pArea->dbfarea.area, pTag->CurKeyInfo->Xtra) == Harbour::SUCCESS &&
            pTag->CurKeyInfo->Xtra != 0) {
          pArea->dbfarea.area.fBottom = true;
          SELF_SKIPFILTER(&pArea->dbfarea.area, -1);
        }
      }
      pArea->lpCurTag = pSavedTag;
    }
    hb_ntxTagUnLockRead(pTag);
  }
}

/*
 * skip to next/previous unique key
 */
static bool hb_ntxOrdSkipUnique(LPTAGINFO pTag, HB_LONG lToSkip)
{
  NTXAREAP pArea = pTag->pIndex->pArea;

  if (pArea->dbfarea.lpdbPendingRel) {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = false;

  if (hb_ntxTagLockRead(pTag)) {
    auto fOut = false;
    auto fEof = false;
    bool fForward = (lToSkip >= 0);

    LPTAGINFO pSavedTag = pArea->lpCurTag;
    pArea->lpCurTag = pTag;

    hb_ntxTagRefreshScope(pTag);
    if (hb_ntxCurKeyRefresh(pTag)) {
      char keyVal[NTX_MAX_KEY];
      memcpy(keyVal, pTag->CurKeyInfo->key, pTag->KeyLength);

      do {
        if (fForward) {
          hb_ntxTagSkipNext(pTag);
        } else {
          hb_ntxTagSkipPrev(pTag);
        }
        fOut = pTag->TagEOF || pTag->TagBOF;
      } while (!fOut &&
               hb_ntxValCompare(pTag, pTag->CurKeyInfo->key, pTag->KeyLength, keyVal, pTag->KeyLength, true) == 0);
    } else if (!fForward && !pArea->dbfarea.fPositioned) {
      hb_ntxTagGoBottom(pTag);
      fEof = pTag->TagEOF;
    } else {
      fOut = true;
    }
    if (fOut) {
      if (fForward) {
        fEof = true;
      } else {
        hb_ntxTagGoTop(pTag);
        fEof = pTag->TagEOF;
      }
    }
    hb_ntxTagUnLockRead(pTag);

    if (SELF_GOTO(&pArea->dbfarea.area, fEof ? 0 : pTag->CurKeyInfo->Xtra) == Harbour::SUCCESS && !fEof) {
      SELF_SKIPFILTER(&pArea->dbfarea.area, (fForward || fOut) ? 1 : -1);
      if (!fForward && fOut) {
        pArea->dbfarea.area.fBof = true;
      }
    }

    /* Update Bof and Eof flags */
    if (fForward) {
      pArea->dbfarea.area.fBof = false;
    } else {
      pArea->dbfarea.area.fEof = false;
    }

    pArea->lpCurTag = pSavedTag;
    return true;
  }
  return false;
}

/*
 * skip while code block doesn't return true
 */
static bool hb_ntxOrdSkipEval(LPTAGINFO pTag, bool fForward, PHB_ITEM pEval)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxOrdSkipEval(%p, %d, %p)", static_cast<void*>(pTag), fForward, static_cast<void*>(pEval)));
#endif

  NTXAREAP pArea = pTag->pIndex->pArea;
  auto fFound = false;

  if ((hb_itemType(pEval) & Harbour::Item::BLOCK) == 0) {
    if (SELF_SKIP(&pArea->dbfarea.area, fForward ? 1 : -1) != Harbour::SUCCESS) {
      return false;
    }
    return fForward ? !pArea->dbfarea.area.fEof : !pArea->dbfarea.area.fBof;
  }

  if (pArea->dbfarea.lpdbPendingRel) {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = false;

  if (hb_ntxTagLockRead(pTag)) {
    LPTAGINFO pSavedTag = pArea->lpCurTag;
    pArea->lpCurTag = pTag;

    hb_ntxTagRefreshScope(pTag);
    if (hb_ntxCurKeyRefresh(pTag)) {
      if (fForward) {
        hb_ntxTagSkipNext(pTag);
      } else {
        hb_ntxTagSkipPrev(pTag);
      }

      while (fForward ? !pTag->TagEOF : !pTag->TagBOF) {
        if (SELF_GOTO(&pArea->dbfarea.area, pTag->CurKeyInfo->Xtra) != Harbour::SUCCESS) {
          break;
        }
        if (hb_ntxEvalSeekCond(pTag, pEval)) {
          HB_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
          if (SELF_SKIPFILTER(&pArea->dbfarea.area, fForward ? 1 : -1) != Harbour::SUCCESS ||
              pArea->dbfarea.ulRecNo == ulRecNo || hb_ntxEvalSeekCond(pTag, pEval)) {
            fFound = true;
            break;
          }
        }
        if (fForward) {
          hb_ntxTagSkipNext(pTag);
        } else {
          hb_ntxTagSkipPrev(pTag);
        }
      }
      if (!fFound) {
        if (fForward) {
          SELF_GOTO(&pArea->dbfarea.area, 0);
        } else {
          SELF_GOTOP(&pArea->dbfarea.area);
          pArea->dbfarea.area.fBof = true;
        }
      }
    }
    pArea->lpCurTag = pSavedTag;
    hb_ntxTagUnLockRead(pTag);
  }

  /* Update Bof and Eof flags */
  if (fForward) {
    pArea->dbfarea.area.fBof = false;
  } else {
    pArea->dbfarea.area.fEof = false;
  }

  return fFound;
}

/*
 * skip while code block doesn't return true
 */
static bool hb_ntxOrdSkipWild(LPTAGINFO pTag, bool fForward, PHB_ITEM pWildItm)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxOrdSkipWild(%p, %d, %p)", static_cast<void*>(pTag), fForward, static_cast<void*>(pWildItm)));
#endif

  NTXAREAP pArea = pTag->pIndex->pArea;
  char *szFree = nullptr;
  auto fFound = false;
  int iFixed = 0;

  auto szPattern = hb_itemGetCPtr(pWildItm);

  if (pTag->KeyType != 'C' || !szPattern || !*szPattern) {
    if (SELF_SKIP(&pArea->dbfarea.area, fForward ? 1 : -1) != Harbour::SUCCESS) {
      return false;
    }
    return fForward ? !pArea->dbfarea.area.fEof : !pArea->dbfarea.area.fBof;
  }

  if (pArea->dbfarea.area.cdPage != hb_vmCDP()) {
    szPattern = szFree = hb_cdpDup(szPattern, hb_vmCDP(), pArea->dbfarea.area.cdPage);
  }

  while (iFixed < pTag->KeyLength && szPattern[iFixed] && szPattern[iFixed] != '*' && szPattern[iFixed] != '?') {
    ++iFixed;
  }

  if (pArea->dbfarea.lpdbPendingRel) {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = false;

  if (hb_ntxTagLockRead(pTag)) {
    LPTAGINFO pSavedTag = pArea->lpCurTag;
    pArea->lpCurTag = pTag;

    hb_ntxTagRefreshScope(pTag);
    if (hb_ntxCurKeyRefresh(pTag)) {
      int iStop = fForward ? -1 : 1;
      if (pTag->fUsrDescend) {
        iStop = -iStop;
      }
      if (iFixed && hb_ntxValCompare(pTag, szPattern, iFixed, pTag->CurKeyInfo->key, iFixed, false) == -iStop) {
        LPKEYINFO pKey;
        pKey = hb_ntxKeyNew(nullptr, pTag->KeyLength);
        memcpy(pKey->key, szPattern, iFixed);
        pKey->key[iFixed] = '\0';
        pKey->Xtra = pArea->lpCurTag->fUsrDescend == pArea->lpCurTag->AscendKey ? NTX_MAX_REC_NUM : NTX_IGNORE_REC_NUM;
        if (!hb_ntxTagKeyFind(pTag, pKey, static_cast<HB_USHORTCAST>(iFixed))) {
          if (fForward) {
            pTag->TagEOF = true;
          } else {
            pTag->TagBOF = true;
          }
        }
        hb_ntxKeyFree(pKey);
      } else if (fForward) {
        hb_ntxTagSkipNext(pTag);
      } else {
        hb_ntxTagSkipPrev(pTag);
      }

      while (fForward ? !pTag->TagEOF : !pTag->TagBOF) {
        if (hb_strMatchWild(pTag->CurKeyInfo->key, szPattern)) {
          HB_ULONG ulRecNo = pTag->CurKeyInfo->Xtra;
          if (SELF_GOTO(&pArea->dbfarea.area, ulRecNo) != Harbour::SUCCESS) {
            break;
          }
          if (SELF_SKIPFILTER(&pArea->dbfarea.area, fForward ? 1 : -1) != Harbour::SUCCESS ||
              pArea->dbfarea.ulRecNo == ulRecNo || hb_strMatchWild(pTag->CurKeyInfo->key, szPattern)) {
            fFound = true;
            break;
          }
        }
        if (iFixed && hb_ntxValCompare(pTag, szPattern, iFixed, pTag->CurKeyInfo->key, iFixed, false) == iStop) {
          break;
        }
        if (fForward) {
          hb_ntxTagSkipNext(pTag);
        } else {
          hb_ntxTagSkipPrev(pTag);
        }
      }
      if (!fFound) {
        if (fForward) {
          SELF_GOTO(&pArea->dbfarea.area, 0);
        } else {
          SELF_GOTOP(&pArea->dbfarea.area);
          pArea->dbfarea.area.fBof = true;
        }
      }
    }
    pArea->lpCurTag = pSavedTag;
    hb_ntxTagUnLockRead(pTag);
  }

  /* Update Bof and Eof flags */
  if (fForward) {
    pArea->dbfarea.area.fBof = false;
  } else {
    pArea->dbfarea.area.fEof = false;
  }

  if (szFree != nullptr) {
    hb_xfree(szFree);
  }

  return fFound;
}

static bool hb_ntxRegexMatch(LPTAGINFO pTag, PHB_REGEX pRegEx, const char *szKey)
{
  HB_SIZE nLen = pTag->KeyLength;
  char szBuff[NTX_MAX_KEY + 1];

  if (pTag->pIndex->pArea->dbfarea.area.cdPage != hb_vmCDP()) {
    nLen = sizeof(szBuff) - 1;
    hb_cdpnDup2(szKey, pTag->KeyLength, szBuff, &nLen, pTag->pIndex->pArea->dbfarea.area.cdPage, hb_vmCDP());
    szBuff[nLen] = '\0';
    szKey = szBuff;
  }

  return hb_regexMatch(pRegEx, szKey, nLen, false);
}

/*
 * skip while regular expression on index key val doesn't return true
 */
static bool hb_ntxOrdSkipRegEx(LPTAGINFO pTag, bool fForward, PHB_ITEM pRegExItm)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxOrdSkipRegEx(%p, %d, %p)", static_cast<void*>(pTag), fForward, static_cast<void*>(pRegExItm)));
#endif

  NTXAREAP pArea = pTag->pIndex->pArea;
  auto fFound = false;
  PHB_REGEX pRegEx;

  if (pTag->KeyType != 'C' || (pRegEx = hb_regexGet(pRegExItm, 0)) == nullptr) {
    if (SELF_SKIP(&pArea->dbfarea.area, fForward ? 1 : -1) != Harbour::SUCCESS) {
      return false;
    }
    return fForward ? !pArea->dbfarea.area.fEof : !pArea->dbfarea.area.fBof;
  }

  if (pArea->dbfarea.lpdbPendingRel) {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = false;

  if (hb_ntxTagLockRead(pTag)) {
    LPTAGINFO pSavedTag = pArea->lpCurTag;
    pArea->lpCurTag = pTag;

    hb_ntxTagRefreshScope(pTag);
    if (hb_ntxCurKeyRefresh(pTag)) {
      if (fForward) {
        hb_ntxTagSkipNext(pTag);
      } else {
        hb_ntxTagSkipPrev(pTag);
      }

      while (fForward ? !pTag->TagEOF : !pTag->TagBOF) {
        if (SELF_GOTO(&pArea->dbfarea.area, pTag->CurKeyInfo->Xtra) != Harbour::SUCCESS) {
          break;
        }

        if (hb_ntxRegexMatch(pTag, pRegEx, static_cast<const char *>(pTag->CurKeyInfo->key))) {
          HB_ULONG ulRecNo = pArea->dbfarea.ulRecNo;
          if (SELF_SKIPFILTER(&pArea->dbfarea.area, fForward ? 1 : -1) != Harbour::SUCCESS ||
              pArea->dbfarea.ulRecNo == ulRecNo ||
              hb_ntxRegexMatch(pTag, pRegEx, static_cast<const char *>(pTag->CurKeyInfo->key))) {
            fFound = true;
            break;
          }
        }
        if (fForward) {
          hb_ntxTagSkipNext(pTag);
        } else {
          hb_ntxTagSkipPrev(pTag);
        }
      }
      if (!fFound) {
        if (fForward) {
          SELF_GOTO(&pArea->dbfarea.area, 0);
        } else {
          SELF_GOTOP(&pArea->dbfarea.area);
          pArea->dbfarea.area.fBof = true;
        }
      }
    }
    pArea->lpCurTag = pSavedTag;
    hb_ntxTagUnLockRead(pTag);
  }

  /* Update Bof and Eof flags */
  if (fForward) {
    pArea->dbfarea.area.fBof = false;
  } else {
    pArea->dbfarea.area.fEof = false;
  }

  hb_regexFree(pRegEx);

  return fFound;
}

/*
 * add key to custom tag (ordKeyAdd())
 * user key value is not implemented
 */
static bool hb_ntxOrdKeyAdd(LPTAGINFO pTag, PHB_ITEM pItem)
{
  NTXAREAP pArea = pTag->pIndex->pArea;
  auto fResult = false;
  LPKEYINFO pKey;

  if (pArea->dbfarea.lpdbPendingRel) {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  if (!pArea->dbfarea.fPositioned) {
    return false;
  }

  if (pTag->pForItem && !hb_ntxEvalCond(pArea, pTag->pForItem, true)) {
    return false;
  }

  if (pTag->Template && pItem && hb_itemType(pItem) != Harbour::Item::NIL) {
    pKey = hb_ntxKeyPutItem(nullptr, pItem, pArea->dbfarea.ulRecNo, pTag, true, nullptr);
  } else {
    pKey = hb_ntxEvalKey(nullptr, pTag);
  }

  if (hb_ntxTagLockWrite(pTag)) {
    if (hb_ntxTagKeyAdd(pTag, pKey)) {
      fResult = true;
      if (!pTag->pIndex->fShared && pTag->keyCount && hb_ntxKeyInScope(pTag, pKey)) {
        pTag->keyCount++;
      }
    }
    hb_ntxTagUnLockWrite(pTag);
  }
  hb_ntxKeyFree(pKey);
  return fResult;
}

/*
 * del key from custom tag (ordKeyDel())
 * user key value is not implemented
 */
static bool hb_ntxOrdKeyDel(LPTAGINFO pTag, PHB_ITEM pItem)
{
  NTXAREAP pArea = pTag->pIndex->pArea;
  auto fResult = false;
  LPKEYINFO pKey = nullptr;

  if (pArea->dbfarea.lpdbPendingRel) {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  if (!pArea->dbfarea.fPositioned) {
    return false;
  }

  if (pTag->pForItem && !hb_ntxEvalCond(pArea, pTag->pForItem, true)) {
    return false;
  }

  if (pTag->Template && pItem && hb_itemType(pItem) != Harbour::Item::NIL) {
    pKey = hb_ntxKeyPutItem(nullptr, pItem, pArea->dbfarea.ulRecNo, pTag, true, nullptr);
  }

  if (hb_ntxTagLockWrite(pTag)) {
    if (pKey == nullptr) {
      if (hb_ntxCurKeyRefresh(pTag)) {
        pKey = hb_ntxKeyCopy(nullptr, pTag->CurKeyInfo, pTag->KeyLength);
      } else {
        pKey = hb_ntxEvalKey(nullptr, pTag);
      }
    }
    if (hb_ntxTagKeyDel(pTag, pKey)) {
      fResult = true;
      if (!pTag->pIndex->fShared && pTag->keyCount && hb_ntxKeyInScope(pTag, pKey)) {
        pTag->keyCount--;
      }
    }
    hb_ntxTagUnLockWrite(pTag);
  }
  hb_ntxKeyFree(pKey);
  return fResult;
}

/*
 * DBOI_FINDREC find a specific record in the tag - it's useful for
 * custom indexes when the same record can be stored more then once
 * or when the used index key is unknown
 */
static bool hb_ntxOrdFindRec(LPTAGINFO pTag, HB_ULONG ulRecNo, bool fCont)
{
  NTXAREAP pArea = pTag->pIndex->pArea;
  auto fFound = false;

  if (pTag && ulRecNo) {
    if (pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped) {
      SELF_FORCEREL(&pArea->dbfarea.area);
    }

    if (hb_ntxTagLockRead(pTag)) {
      hb_ntxTagRefreshScope(pTag);
      if (fCont) {
        if (!hb_ntxCurKeyRefresh(pTag)) {
          ulRecNo = 0;
        } else {
          hb_ntxTagSkipNext(pTag);
        }
      } else {
        hb_ntxTagGoTop(pTag);
      }
      if (ulRecNo) {
        while (!pTag->TagEOF) {
          if (pTag->CurKeyInfo->Xtra == ulRecNo) {
            fFound = true;
            break;
          }
          hb_ntxTagSkipNext(pTag);
        }
      }
      hb_ntxTagUnLockRead(pTag);
    }
  }
  SELF_GOTO(&pArea->dbfarea.area, fFound ? ulRecNo : 0);
  return fFound;
}

/*
 * evaluate given C function in given scope
 */
static HB_ULONG hb_ntxOrdScopeEval(LPTAGINFO pTag, HB_EVALSCOPE_FUNC pFunc, void *pParam, PHB_ITEM pItemLo,
                                   PHB_ITEM pItemHi)
{
  HB_ULONG ulCount = 0, ulLen = static_cast<HB_ULONG>(pTag->KeyLength);
  auto pItemTop = hb_itemNew(nullptr);
  auto pItemBottom = hb_itemNew(nullptr);

  hb_ntxTagGetScope(pTag, 0, pItemTop);
  hb_ntxTagGetScope(pTag, 1, pItemBottom);
  hb_ntxTagSetScope(pTag, 0, pItemLo);
  hb_ntxTagSetScope(pTag, 1, pItemHi);

  if (hb_ntxTagLockRead(pTag)) {
    hb_ntxTagGoTop(pTag);
    while (!pTag->TagEOF) {
      pFunc(pTag->CurKeyInfo->Xtra, reinterpret_cast<HB_BYTE *>(pTag->CurKeyInfo->key), ulLen, pParam);
      ulCount++;
      hb_ntxTagSkipNext(pTag);
    }
    hb_ntxTagUnLockRead(pTag);
  }

  hb_ntxTagSetScope(pTag, 0, pItemTop);
  hb_ntxTagSetScope(pTag, 1, pItemBottom);
  hb_itemRelease(pItemTop);
  hb_itemRelease(pItemBottom);

  return ulCount;
}

/* ************************************************************************* */
/* create index: hb_ntxTagCreate() */
/* ************************************************************************* */

static int hb_ntxQuickSortCompare(LPNTXSORTINFO pSort, HB_BYTE *pKey1, HB_BYTE *pKey2)
{
  int iLen = pSort->keyLen, i;

  i = hb_ntxValCompare(pSort->pTag, reinterpret_cast<const char *>(pKey1), iLen, reinterpret_cast<const char *>(pKey2),
                       iLen, true);
  if (i == 0) {
    if (pSort->pTag->fSortRec) {
      i = (HB_GET_LE_UINT32(pKey1 + iLen) < HB_GET_LE_UINT32(pKey2 + iLen)) ? -1 : 1;
    }
  } else if (!pSort->pTag->AscendKey) {
    i = -i;
  }

  return i;
}

static bool hb_ntxQSort(LPNTXSORTINFO pSort, HB_BYTE *pSrc, HB_BYTE *pBuf, HB_LONG lKeys)
{
  if (lKeys > 1) {
    int iLen = pSort->keyLen + 4;
    HB_LONG l1, l2;
    HB_BYTE *pPtr1, *pPtr2, *pDst;
    auto f1 = false;
    auto f2 = false;

    l1 = lKeys >> 1;
    l2 = lKeys - l1;
    pPtr1 = &pSrc[0];
    pPtr2 = &pSrc[l1 * iLen];

    f1 = hb_ntxQSort(pSort, pPtr1, &pBuf[0], l1);
    f2 = hb_ntxQSort(pSort, pPtr2, &pBuf[l1 * iLen], l2);
    if (f1) {
      pDst = pBuf;
    } else {
      pDst = pSrc;
      pPtr1 = &pBuf[0];
    }
    if (!f2) {
      pPtr2 = &pBuf[l1 * iLen];
    }
    while (l1 > 0 && l2 > 0) {
      if (hb_ntxQuickSortCompare(pSort, pPtr1, pPtr2) <= 0) {
        memcpy(pDst, pPtr1, iLen);
        pPtr1 += iLen;
        l1--;
      } else {
        memcpy(pDst, pPtr2, iLen);
        pPtr2 += iLen;
        l2--;
      }
      pDst += iLen;
    }
    if (l1 > 0) {
      memcpy(pDst, pPtr1, iLen * l1);
    } else if (l2 > 0 && f1 == f2) {
      memcpy(pDst, pPtr2, iLen * l2);
    }
    return !f1;
  }
  return true;
}

static void hb_ntxSortSortPage(LPNTXSORTINFO pSort)
{
  HB_SIZE nSize = static_cast<HB_SIZE>(pSort->ulKeys) * (pSort->keyLen + 4);

  if (!hb_ntxQSort(pSort, pSort->pKeyPool, &pSort->pKeyPool[nSize], pSort->ulKeys)) {
    pSort->pStartKey = &pSort->pKeyPool[nSize];
  } else {
    pSort->pStartKey = pSort->pKeyPool;
  }
}

static void hb_ntxSortBufferFlush(LPNTXSORTINFO pSort)
{
  if (pSort->ulPagesIO) {
    LPNTXINDEX pIndex = pSort->pTag->pIndex;
    HB_SIZE nSize = static_cast<HB_SIZE>(pSort->ulPagesIO) * NTXBLOCKSIZE;
    if (hb_fileWriteAt(pIndex->DiskFile, pSort->pBuffIO, nSize, hb_ntxFileOffset(pIndex, pSort->ulFirstIO)) != nSize) {
      hb_ntxErrorRT(pIndex->pArea, EG_WRITE, EDBF_WRITE, pIndex->IndexName, hb_fsError(), 0, nullptr);
    }
    pSort->ulPagesIO = 0;
    pIndex->fFlush = true;
    if (pIndex->fShared) {
      pIndex->Changed = true;
    }
  }
}

static void hb_ntxSortStorePage(LPNTXSORTINFO pSort, LPPAGEINFO pPage)
{
  LPNTXINDEX pIndex = pSort->pTag->pIndex;

  if (!pPage->Page) {
    pPage->Page = hb_ntxPageAlloc(pIndex);
    if (pSort->ulSizeIO) {
      if (pSort->ulPagesIO == pSort->ulSizeIO) {
        hb_ntxSortBufferFlush(pSort);
      }
      if (!pSort->ulPagesIO ||
          hb_ntxFileOffset(pIndex, pSort->ulLastIO) + NTXBLOCKSIZE == hb_ntxFileOffset(pIndex, pPage->Page)) {
        hb_ntxSetKeyCount(pPage, pPage->uiKeys);
        memcpy(pSort->pBuffIO + pSort->ulPagesIO * NTXBLOCKSIZE, hb_ntxPageBuffer(pPage), NTXBLOCKSIZE);
        pSort->ulLastIO = pPage->Page;
        if (!pSort->ulPagesIO++) {
          pSort->ulFirstIO = pPage->Page;
        }
        pPage->Changed = false;
        return;
      }
    }
  }
  if (!pPage->pPrev) {
    hb_ntxPageSave(pIndex, pPage);
  }
}

static void hb_ntxSortAddNodeKey(LPNTXSORTINFO pSort, HB_BYTE *pKeyVal, HB_ULONG ulRec)
{
  LPPAGEINFO pPage;
  HB_ULONG ulPage = 0;
  int iLevel = 0;

  for (;;) {
    pPage = pSort->NodeList[iLevel];
    if (pPage == nullptr) {
      pPage = pSort->NodeList[iLevel] = hb_ntxPageNew(pSort->pTag, true);
      break;
    } else if (pPage->uiKeys >= pSort->pTag->MaxKeys) {
      hb_ntxSetKeyPage(pPage, pPage->uiKeys, ulPage);
      hb_ntxSortStorePage(pSort, pPage);
      ulPage = pPage->Page;
      hb_ntxPageRelease(pSort->pTag, pPage);
      pSort->NodeList[iLevel++] = hb_ntxPageNew(pSort->pTag, true);
    } else {
      break;
    }
  }

  memcpy(hb_ntxGetKeyVal(pPage, pPage->uiKeys), pKeyVal, pSort->pTag->KeyLength);
  hb_ntxSetKeyRec(pPage, pPage->uiKeys, ulRec);
  hb_ntxSetKeyPage(pPage, pPage->uiKeys, ulPage);
  pPage->uiKeys++;
}

static void hb_ntxSortWritePage(LPNTXSORTINFO pSort)
{
  HB_SIZE nSize = static_cast<HB_SIZE>(pSort->ulKeys) * (pSort->keyLen + 4);

  hb_ntxSortSortPage(pSort);

  if (pSort->pTempFile == nullptr) {
    char szName[HB_PATH_MAX];
    pSort->pTempFile = hb_fileCreateTemp(nullptr, nullptr, FC_NORMAL, szName);
    if (pSort->pTempFile == nullptr) {
      hb_ntxErrorRT(pSort->pTag->pIndex->pArea, EG_CREATE, EDBF_CREATE_TEMP, szName, hb_fsError(), 0, nullptr);
    } else {
      pSort->szTempFileName = hb_strdup(szName);
    }
  }

  pSort->pSwapPage[pSort->ulCurPage].ulKeys = pSort->ulKeys;
  if (pSort->pTempFile != nullptr) {
    pSort->pSwapPage[pSort->ulCurPage].nOffset = hb_fileSize(pSort->pTempFile);
    if (hb_fileWriteAt(pSort->pTempFile, pSort->pStartKey, nSize, pSort->pSwapPage[pSort->ulCurPage].nOffset) !=
        nSize) {
      hb_ntxErrorRT(pSort->pTag->pIndex->pArea, EG_WRITE, EDBF_WRITE_TEMP, pSort->szTempFileName, hb_fsError(), 0,
                    nullptr);
    }
  } else {
    pSort->pSwapPage[pSort->ulCurPage].nOffset = 0;
  }
  pSort->ulKeys = 0;
  pSort->ulCurPage++;
}

static void hb_ntxSortGetPageKey(LPNTXSORTINFO pSort, HB_ULONG ulPage, HB_BYTE **pKeyVal, HB_ULONG *pulRec)
{
  int iLen = pSort->keyLen;

  if (pSort->pSwapPage[ulPage].ulKeyBuf == 0) {
    HB_ULONG ulKeys = HB_MIN(pSort->ulPgKeys, pSort->pSwapPage[ulPage].ulKeys);
    HB_SIZE nSize = static_cast<HB_SIZE>(ulKeys) * (iLen + 4);

    if (pSort->pTempFile != nullptr && hb_fileReadAt(pSort->pTempFile, pSort->pSwapPage[ulPage].pKeyPool, nSize,
                                                     pSort->pSwapPage[ulPage].nOffset) != nSize) {
      hb_ntxErrorRT(pSort->pTag->pIndex->pArea, EG_READ, EDBF_READ_TEMP, pSort->szTempFileName, hb_fsError(), 0,
                    nullptr);
    }
    pSort->pSwapPage[ulPage].nOffset += nSize;
    pSort->pSwapPage[ulPage].ulKeyBuf = ulKeys;
    pSort->pSwapPage[ulPage].ulCurKey = 0;
  }
  *pKeyVal = &pSort->pSwapPage[ulPage].pKeyPool[pSort->pSwapPage[ulPage].ulCurKey * (iLen + 4)];
  *pulRec = HB_GET_LE_UINT32(*pKeyVal + iLen);
}

static void hb_ntxSortOrderPages(LPNTXSORTINFO pSort)
{
  pSort->ulFirst = 0;
  pSort->pSortedPages = static_cast<HB_ULONG *>(hb_xgrab(pSort->ulPages * sizeof(HB_ULONG)));
  pSort->pSortedPages[0] = 0;

  if (pSort->ulTotKeys > 0) {
    int iLen = pSort->keyLen;
    HB_BYTE *pKey = nullptr;

    for (HB_ULONG n = 0; n < pSort->ulPages; n++) {
      HB_LONG l, r;
      HB_ULONG ulRec;

      hb_ntxSortGetPageKey(pSort, n, &pKey, &ulRec);
      l = 0;
      r = n - 1;
      while (l <= r) {
        int i;
        HB_ULONG ulPage;
        HB_LONG m;
        HB_BYTE *pTmp;

        m = (l + r) >> 1;
        ulPage = pSort->pSortedPages[m];
        pTmp = &pSort->pSwapPage[ulPage].pKeyPool[pSort->pSwapPage[ulPage].ulCurKey * (iLen + 4)];
        i = hb_ntxValCompare(pSort->pTag, reinterpret_cast<const char *>(pKey), iLen,
                             reinterpret_cast<const char *>(pTmp), iLen, true);
        if (i == 0) {
          if (pSort->pTag->fSortRec) {
            i = (ulRec < HB_GET_LE_UINT32(&pTmp[iLen])) ? -1 : 1;
          }
        } else if (!pSort->pTag->AscendKey) {
          i = -i;
        }
        if (i >= 0) {
          l = m + 1;
        } else {
          r = m - 1;
        }
      }
      for (r = n; r > l; r--) {
        pSort->pSortedPages[r] = pSort->pSortedPages[r - 1];
      }
      pSort->pSortedPages[l] = n;
    }
  }
}

static bool hb_ntxSortKeyGet(LPNTXSORTINFO pSort, HB_BYTE **pKeyVal, HB_ULONG *pulRec)
{
  HB_ULONG ulPage = pSort->pSortedPages[pSort->ulFirst];

  /* check if first page has some keys yet */
  if (pSort->pSwapPage[ulPage].ulKeys > 0) {
    int iLen = pSort->keyLen;
    HB_BYTE *pKey;
    HB_ULONG ulRec;
    HB_LONG l, r;

    /*
     * last key was taken from this page - we have to resort it.
     * This is done intentionally here to be sure that the key
     * value return by this function will not be overwritten by
     * next keys in page read from temporary file in function
     * hb_ntxSortGetPageKey() - please do not move this part down
     * even it seems to be correct
     */
    hb_ntxSortGetPageKey(pSort, ulPage, &pKey, &ulRec);

    l = pSort->ulFirst + 1;
    r = pSort->ulPages - 1;
    while (l <= r) {
      int i;
      HB_ULONG ulPg;
      HB_LONG m;
      HB_BYTE *pTmp;

      m = (l + r) >> 1;
      ulPg = pSort->pSortedPages[m];
      pTmp = &pSort->pSwapPage[ulPg].pKeyPool[pSort->pSwapPage[ulPg].ulCurKey * (iLen + 4)];
      i = hb_ntxValCompare(pSort->pTag, reinterpret_cast<const char *>(pKey), iLen,
                           reinterpret_cast<const char *>(pTmp), iLen, true);
      if (i == 0) {
        if (pSort->pTag->fSortRec) {
          i = (ulRec < HB_GET_LE_UINT32(&pTmp[iLen])) ? -1 : 1;
        } else {
          i = (ulPage < ulPg) ? -1 : 1;
        }
      } else if (!pSort->pTag->AscendKey) {
        i = -i;
      }
      if (i > 0) {
        l = m + 1;
      } else {
        r = m - 1;
      }
    }
    if (l > static_cast<HB_LONG>(pSort->ulFirst) + 1) {
      ulPage = pSort->pSortedPages[pSort->ulFirst];
      for (r = pSort->ulFirst + 1; r < l; r++) {
        pSort->pSortedPages[r - 1] = pSort->pSortedPages[r];
      }
      pSort->pSortedPages[l - 1] = ulPage;
    }
  } else {
    pSort->ulFirst++;
  }
  if (pSort->ulFirst < pSort->ulPages) {
    ulPage = pSort->pSortedPages[pSort->ulFirst];
    hb_ntxSortGetPageKey(pSort, ulPage, pKeyVal, pulRec);
    pSort->pSwapPage[ulPage].ulCurKey++;
    pSort->pSwapPage[ulPage].ulKeys--;
    pSort->pSwapPage[ulPage].ulKeyBuf--;
    return true;
  }

  *pKeyVal = nullptr;
  *pulRec = 0;

  return false;
}

static void hb_ntxSortKeyAdd(LPNTXSORTINFO pSort, HB_ULONG ulRec, const char *pKeyVal, int iKeyLen)
{
  int iLen = pSort->keyLen;
  HB_BYTE *pDst;

  if (pSort->ulKeys >= pSort->ulPgKeys) {
    hb_ntxSortWritePage(pSort);
  }
  pDst = &pSort->pKeyPool[pSort->ulKeys * (iLen + 4)];

  if (iLen > iKeyLen) {
    memcpy(pDst, pKeyVal, iKeyLen);
    memset(&pDst[iKeyLen], ' ', iLen - iKeyLen);
  } else {
    memcpy(pDst, pKeyVal, iLen);
  }
  HB_PUT_LE_UINT32(&pDst[iLen], ulRec);
  pSort->ulKeys++;
  pSort->ulTotKeys++;
}

static LPNTXSORTINFO hb_ntxSortNew(LPTAGINFO pTag, HB_ULONG ulRecCount)
{
  HB_BYTE *pBuf;
  int iLen = pTag->KeyLength;
  HB_ULONG ulSize, ulMax, ulMin;

  if (ulRecCount == 0) {
    ulRecCount = 1;
  }

  auto pSort = static_cast<LPNTXSORTINFO>(hb_xgrabz(sizeof(NTXSORTINFO)));

  ulMin = static_cast<HB_ULONG>(ceil(sqrt(static_cast<double>(ulRecCount))));
  ulMax = (static_cast<HB_ULONG>(ceil(sqrt(static_cast<double>(ulRecCount) / (iLen + 4))))) << 7;
  /*
   * this effectively increase allocated memory buffer for very large files
   * moving the maximum to: 270'566'400 for 4'294'967'295 records and 256
   * index key length.
   * if you want to force smaller buffer I wrote below then add here:
   * ulMax = ulMin;
   */
  ulSize = (1L << 20) / (iLen + 4);
  while (ulMax < ulSize) {
    ulMax <<= 1;
  }
  if (ulMax > ulRecCount) {
    ulMax = ulRecCount;
  }

  do {
    ulSize = ulMax * (iLen + 4);
    pBuf = static_cast<HB_BYTE *>(hb_xalloc(ulSize << 2));
    if (pBuf) {
      hb_xfree(pBuf);
      pBuf = static_cast<HB_BYTE *>(hb_xalloc(ulSize << 1));
    } else {
      ulMax >>= 1;
    }
  } while (!pBuf && ulMax >= ulMin);

  if (!pBuf) {
    /* call hb_xgrab() to force out of memory error,
     * though in multi process environment this call may return
     * with success when other process free some memory
     * (also the size of buf is reduced to absolute minimum).
     * Sorry but I'm to lazy to implement indexing with smaller
     * memory though it's possible - just simply I can even create
     * index on-line by key adding like in normal update process.
     * The memory necessary to index file is now ~
     *    ~ (keySize+4+sizeof(NTXSWAPPAGE)) * sqrt(ulRecCount) * 2
     * so the maximum is for DBF with 2^32 records and keySize 256 ~
     * ~ 2^17 * 284 ~=~ 37 MB
     * this is not a problem for current computers and I do not see
     * any way to use DBFs with four billions records and indexes with
     * such long (256 bytes) keys on the old ones - they will be simply
     * to slow. IMHO it's also better to signal out of memory here and
     * force some system upgrades then run process which will have to
     * take many hours, Druzus.
     */
    ulMax = ulMin;
    pBuf = static_cast<HB_BYTE *>(hb_xgrab((ulMax << 1) * (iLen + 4)));
  }

  pSort->pTag = pTag;
  pSort->pTempFile = nullptr;
  pSort->keyLen = iLen;
  pSort->fUnique = pTag->UniqueKey;
  pSort->ulMaxKey = ulMax << 1;
  pSort->ulPgKeys = ulMax;
  pSort->ulMaxRec = ulRecCount;
  pSort->pKeyPool = pBuf;
  pSort->ulPages = (ulRecCount + pSort->ulPgKeys - 1) / pSort->ulPgKeys;
  /* check for overflow on 32-bit machines when number of records is nearly 2^32 */
  if (!pSort->ulPages) {
    pSort->ulPages = ulRecCount / pSort->ulPgKeys + 1;
  }
  pSort->pSwapPage = static_cast<LPNTXSWAPPAGE>(hb_xgrabz(sizeof(NTXSWAPPAGE) * pSort->ulPages));
  return pSort;
}

static void hb_ntxSortFree(LPNTXSORTINFO pSort, bool fFull)
{
  if (pSort->pTempFile != nullptr) {
    hb_fileClose(pSort->pTempFile);
    pSort->pTempFile = nullptr;
  }
  if (pSort->szTempFileName) {
    hb_fileDelete(pSort->szTempFileName);
    hb_xfree(pSort->szTempFileName);
    pSort->szTempFileName = nullptr;
  }
  if (pSort->pKeyPool) {
    hb_xfree(pSort->pKeyPool);
    pSort->pKeyPool = nullptr;
  }
  if (pSort->pSwapPage) {
    hb_xfree(pSort->pSwapPage);
    pSort->pSwapPage = nullptr;
  }
  if (pSort->pBuffIO) {
    hb_xfree(pSort->pBuffIO);
    pSort->pBuffIO = nullptr;
  }
  if (pSort->pSortedPages) {
    hb_xfree(pSort->pSortedPages);
    pSort->pSortedPages = nullptr;
  }
  if (fFull) {
    hb_xfree(pSort);
  }
}

static void hb_ntxSortOut(LPNTXSORTINFO pSort)
{
  bool fUnique = pSort->fUnique;
  auto fBalance = false;
  auto fNext = false;
  LPTAGINFO pTag = pSort->pTag;
  HB_ULONG ulPage, ulRec, ulKey;
  HB_USHORT uiHalf;
  HB_BYTE *pKeyVal;
  int iLen = pSort->keyLen, iLevel;

  pSort->ulPages = pSort->ulCurPage + 1;
  pSort->ulPgKeys = pSort->ulMaxKey / pSort->ulPages;
  if (pSort->ulPages > 1) {
    HB_BYTE *pBuf = pSort->pKeyPool;
    hb_ntxSortWritePage(pSort);
    for (ulPage = 0; ulPage < pSort->ulPages; ulPage++) {
      pSort->pSwapPage[ulPage].ulKeyBuf = 0;
      pSort->pSwapPage[ulPage].ulCurKey = 0;
      pSort->pSwapPage[ulPage].pKeyPool = pBuf;
      pBuf += pSort->ulPgKeys * (pSort->keyLen + 4);
    }
  } else {
    hb_ntxSortSortPage(pSort);
    pSort->pSwapPage[0].ulKeys = pSort->ulKeys;
    pSort->pSwapPage[0].ulKeyBuf = pSort->ulKeys;
    pSort->pSwapPage[0].ulCurKey = 0;
    pSort->pSwapPage[0].pKeyPool = pSort->pStartKey;
  }
#if 0
   printf("pSort->ulPages=%ld, pSort->ulPgKeys=%ld", pSort->ulPages, pSort->ulPgKeys);
   fflush(stdout);
#endif

  hb_ntxSortOrderPages(pSort);

  if (hb_vmRequestQuery() != 0) {
    return;
  }

  for (ulKey = 0; ulKey < pSort->ulTotKeys; ulKey++) {
    if (!hb_ntxSortKeyGet(pSort, &pKeyVal, &ulRec)) {
      if (hb_vmRequestQuery() != 0) {
        return;
      }
      hb_errInternal(9309, "hb_ntxSortOut: memory structure corrupted.", nullptr, nullptr);
    }
    if (fUnique) {
      if (ulKey != 0 && hb_ntxValCompare(pTag, reinterpret_cast<const char *>(pSort->pLastKey), iLen,
                                         reinterpret_cast<const char *>(pKeyVal), iLen, true) == 0) {
        continue;
      }
#ifndef HB_NTX_DEBUG_EXT
      else {
        memcpy(pSort->pLastKey, pKeyVal, iLen);
      }
#endif
    }
#ifdef HB_NTX_DEBUG_EXT
    if (ulKey != 0) {
      int i = hb_ntxValCompare(pTag, static_cast<const char *>(pSort->pLastKey), iLen,
                               static_cast<const char *>(pKeyVal), iLen, true);
      if (!pTag->AscendKey) {
        i = -i;
      }
      if (i == 0) {
        i = (pSort->ulLastRec < ulRec) ? -1 : 1;
      }
      if (i > 0) {
        printf("\r\nulKey=%ld, pKeyVal=[%s][%ld], pKeyLast=[%s][%ld]\r\n", ulKey, pKeyVal, ulRec, pSort->pLastKey,
               pSort->ulLastRec);
        fflush(stdout);
        if (hb_vmRequestQuery() != 0) {
          return;
        }
        hb_errInternal(9310, "hb_ntxSortOut: sorting fails.", nullptr, nullptr);
      }
    }
    memcpy(pSort->pLastKey, pKeyVal, iLen);
    pSort->ulLastRec = ulRec;
#endif
    hb_ntxSortAddNodeKey(pSort, pKeyVal, ulRec);
  }

#ifdef HB_NTX_DEBUG
  if (hb_ntxSortKeyGet(pSort, &pKeyVal, &ulRec)) {
    if (hb_vmRequestQuery() != 0) {
      return;
    }
    hb_errInternal(9311, "hb_ntxSortOut: memory structure corrupted(2).", nullptr, nullptr);
  }
#endif

  if (pSort->NodeList[0] == nullptr) {
    pSort->NodeList[0] = hb_ntxPageNew(pTag, true);
  }
  hb_ntxSetKeyPage(pSort->NodeList[0], pSort->NodeList[0]->uiKeys, 0);

  iLevel = 0;
  fNext = true;
  fBalance = false;
  uiHalf = pTag->MaxKeys >> 1;
  do {
    hb_ntxSortStorePage(pSort, pSort->NodeList[iLevel]);
    if (iLevel + 1 == NTX_STACKSIZE || pSort->NodeList[iLevel + 1] == nullptr) {
      pTag->RootBlock = pSort->NodeList[iLevel]->Page;
      fNext = false;
    } else {
      hb_ntxSetKeyPage(pSort->NodeList[iLevel + 1], pSort->NodeList[iLevel + 1]->uiKeys, pSort->NodeList[iLevel]->Page);
      if (pSort->NodeList[iLevel]->uiKeys < uiHalf) {
        fBalance = true;
      }
    }
    hb_ntxPageRelease(pTag, pSort->NodeList[iLevel]);
    iLevel++;
  } while (fNext);

  hb_ntxSortBufferFlush(pSort);
  hb_ntxSortFree(pSort, false);

  if (fBalance) {
    LPPAGEINFO pFirst, pLast;

    ulPage = pTag->RootBlock;
    while (ulPage) {
      LPPAGEINFO pPage = hb_ntxPageLoad(pTag, ulPage);
      if (!pPage) {
        return;
      }
      ulPage = hb_ntxGetKeyPage(pPage, pPage->uiKeys);
      if (ulPage && pPage->uiKeys) {
        pLast = hb_ntxPageLoad(pTag, ulPage);
        if (!pLast) {
          hb_ntxPageRelease(pTag, pPage);
          return;
        }
        if (pLast->uiKeys < uiHalf) {
          pFirst = hb_ntxPageLoad(pTag, hb_ntxGetKeyPage(pPage, pPage->uiKeys - 1));
          if (!pFirst) {
            hb_ntxPageRelease(pTag, pPage);
            hb_ntxPageRelease(pTag, pLast);
            return;
          }
          hb_ntxBalancePages(pTag, pPage, pPage->uiKeys - 1, pFirst, pLast);
          hb_ntxPageRelease(pTag, pFirst);
        }
        hb_ntxPageRelease(pTag, pLast);
      }
      hb_ntxPageRelease(pTag, pPage);
    }
  }
}

/*
 * create tag in index file
 */
static HB_ERRCODE hb_ntxTagCreate(LPTAGINFO pTag, bool fReindex)
{
  LPNTXAREA pArea = pTag->pIndex->pArea;
  PHB_ITEM pWhileItem = nullptr, pEvalItem = nullptr;
  HB_ULONG ulRecCount, ulRecNo = pArea->dbfarea.ulRecNo;
  LPNTXSORTINFO pSort;
  HB_LONG lStep = 0;
  HB_ERRCODE errCode = Harbour::SUCCESS;

  if (pArea->dbfarea.area.lpdbOrdCondInfo) {
    pWhileItem = pArea->dbfarea.area.lpdbOrdCondInfo->itmCobWhile;
    lStep = pArea->dbfarea.area.lpdbOrdCondInfo->lStep;
    pEvalItem = pArea->dbfarea.area.lpdbOrdCondInfo->itmCobEval;
  }

  if (pTag->Custom) {
    ulRecCount = 0;
  } else {
    errCode = SELF_RECCOUNT(&pArea->dbfarea.area, &ulRecCount);
    if (errCode != Harbour::SUCCESS) {
      return errCode;
    }
  }
  pArea->pSort = pSort = hb_ntxSortNew(pTag, ulRecCount);
  pSort->fReindex = fReindex;

  if (ulRecCount == 0) {
    LPPAGEINFO pPage = hb_ntxPageNew(pTag, false);

    if (pPage) {
      pTag->RootBlock = pPage->Page;
      hb_ntxPageRelease(pTag, pPage);
    } else {
      errCode = Harbour::FAILURE;
    }
  } else {
    LPTAGINFO pSaveTag = pArea->lpCurTag;
    HB_ULONG ulStartRec = 0, ulNextCount = 0;
    auto fDirectRead = false;
    auto fUseFilter = false;
    HB_BYTE *pSaveRecBuff = pArea->dbfarea.pRecord;
    char szBuffer[NTX_MAX_KEY];
    int iRecBuff = 0, iRecBufSize, iRec;
    auto cdpTmp = hb_cdpSelect(pArea->dbfarea.area.cdPage);
    PHB_ITEM pForItem, pItem = nullptr;

    pForItem = pTag->pForItem;
    if (pTag->nField) {
      pItem = hb_itemNew(nullptr);
    }

    if (!pArea->dbfarea.area.lpdbOrdCondInfo || pArea->dbfarea.area.lpdbOrdCondInfo->fAll) {
      pArea->lpCurTag = nullptr;
    } else {
      if (pArea->dbfarea.area.lpdbOrdCondInfo->itmRecID) {
        ulStartRec = hb_itemGetNL(pArea->dbfarea.area.lpdbOrdCondInfo->itmRecID);
      }
      if (ulStartRec) {
        ulNextCount = 1;
      } else if (pArea->dbfarea.area.lpdbOrdCondInfo->fRest || pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount > 0) {
        if (pArea->dbfarea.area.lpdbOrdCondInfo->itmStartRecID) {
          ulStartRec = hb_itemGetNL(pArea->dbfarea.area.lpdbOrdCondInfo->itmStartRecID);
        }
        if (!ulStartRec) {
          ulStartRec = ulRecNo;
        }
        if (pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount > 0) {
          ulNextCount = pArea->dbfarea.area.lpdbOrdCondInfo->lNextCount;
        }
      } else if (pArea->dbfarea.area.lpdbOrdCondInfo->fUseFilter) {
        fUseFilter = true;
      } else if (!pArea->dbfarea.area.lpdbOrdCondInfo->fUseCurrent) {
        pArea->lpCurTag = nullptr;
      } else if (pArea->lpCurTag) {
        if (hb_ntxTagLockRead(pArea->lpCurTag)) {
          hb_ntxTagRefreshScope(pArea->lpCurTag);
          hb_ntxTagGoTop(pArea->lpCurTag);
          if (!pArea->lpCurTag->TagEOF) {
            ulStartRec = pArea->lpCurTag->CurKeyInfo->Xtra;
          }
          hb_ntxTagUnLockRead(pArea->lpCurTag);
        }
      }
    }

    pSort->ulSizeIO = (1 << 16) / NTXBLOCKSIZE;
    pSort->pBuffIO = static_cast<HB_BYTE *>(hb_xgrab(pSort->ulSizeIO * NTXBLOCKSIZE));
    iRecBufSize = (pSort->ulSizeIO * NTXBLOCKSIZE) / pArea->dbfarea.uiRecordLen;
    fDirectRead = !hb_setGetStrictRead() && iRecBufSize > 1 && /* !pArea->dbfarea.area.lpdbRelations && */
                  (!pArea->dbfarea.area.lpdbOrdCondInfo || pArea->dbfarea.area.lpdbOrdCondInfo->fAll ||
                   (pArea->lpCurTag == nullptr && !fUseFilter));

    if (ulStartRec == 0 && pArea->lpCurTag == nullptr) {
      ulStartRec = 1;
    }

    if (ulStartRec == 0) {
      errCode = SELF_GOTOP(&pArea->dbfarea.area);
    } else {
      errCode = SELF_GOTO(&pArea->dbfarea.area, ulStartRec);
      if (fUseFilter && errCode == Harbour::SUCCESS) {
        errCode = SELF_SKIPFILTER(&pArea->dbfarea.area, 1);
      }
    }

    ulRecNo = pArea->dbfarea.ulRecNo;

    while (errCode == Harbour::SUCCESS && !pArea->dbfarea.area.fEof) {
      if (hb_vmRequestQuery() != 0) {
        errCode = Harbour::FAILURE;
        break;
      }

      if (fDirectRead) {
        if (ulRecNo > ulRecCount) {
          break;
        }
        if (iRecBuff == 0 || iRecBuff >= iRecBufSize) {
          HB_SIZE nSize;

          if (ulRecCount - ulRecNo >= static_cast<HB_ULONG>(iRecBufSize)) {
            iRec = iRecBufSize;
          } else {
            iRec = ulRecCount - ulRecNo + 1;
          }
          if (ulNextCount > 0 && ulNextCount < static_cast<HB_ULONG>(iRec)) {
            iRec = static_cast<int>(ulNextCount);
          }
          nSize = static_cast<HB_SIZE>(iRec) * pArea->dbfarea.uiRecordLen;
          if (hb_fileReadAt(pArea->dbfarea.pDataFile, pSort->pBuffIO, nSize,
                            static_cast<HB_FOFFSET>(pArea->dbfarea.uiHeaderLen) +
                                static_cast<HB_FOFFSET>(ulRecNo - 1) *
                                    static_cast<HB_FOFFSET>(pArea->dbfarea.uiRecordLen)) != nSize) {
            hb_ntxErrorRT(pTag->pIndex->pArea, EG_READ, EDBF_READ, pTag->pIndex->IndexName, hb_fsError(), 0, nullptr);
            break;
          }
          iRecBuff = 0;
        }
        pArea->dbfarea.pRecord = pSort->pBuffIO + iRecBuff * pArea->dbfarea.uiRecordLen;
        pArea->dbfarea.ulRecNo = ulRecNo;
        if (SELF_GETREC(&pArea->dbfarea.area, nullptr) == Harbour::FAILURE) {
          break;
        }
        pArea->dbfarea.fValidBuffer = pArea->dbfarea.fPositioned = true;
        pArea->dbfarea.fDeleted = pArea->dbfarea.pRecord[0] == '*';
        /* Force relational movement in child WorkAreas */
        if (pArea->dbfarea.area.lpdbRelations) {
          errCode = SELF_SYNCCHILDREN(&pArea->dbfarea.area);
          if (errCode != Harbour::SUCCESS) {
            break;
          }
        }
        iRecBuff++;
      }

      if (pWhileItem && !hb_ntxEvalCond(nullptr, pWhileItem, false)) {
        break;
      }

      if (ulRecNo <= ulRecCount && (pForItem == nullptr || hb_ntxEvalCond(pArea, pForItem, false))) {
        if (pTag->nField) {
          errCode = SELF_GETVALUE(&pArea->dbfarea.area, pTag->nField, pItem);
        } else {
          pItem = hb_vmEvalBlockOrMacro(pTag->pKeyItem);
        }

        switch (hb_itemType(pItem)) {
        case Harbour::Item::STRING:
        case Harbour::Item::MEMO:
          hb_ntxSortKeyAdd(pSort, pArea->dbfarea.ulRecNo, pItem->getCPtr(), static_cast<HB_INTCAST>(pItem->getCLen()));
          break;

        case Harbour::Item::INTEGER:
        case Harbour::Item::LONG:
        case Harbour::Item::DOUBLE:
          hb_ntxNumToStr(pItem, szBuffer, pTag->KeyLength, pTag->KeyDec);
          hb_ntxSortKeyAdd(pSort, pArea->dbfarea.ulRecNo, szBuffer, pTag->KeyLength);
          break;

        case Harbour::Item::TIMESTAMP:
          if (pTag->KeyType == 'T') {
            hb_itemGetTS(pItem, szBuffer);
            hb_ntxSortKeyAdd(pSort, pArea->dbfarea.ulRecNo, szBuffer, 17);
            break;
          }
          /* fallthrough */
        case Harbour::Item::DATE:
          pItem->getDS(szBuffer);
          hb_ntxSortKeyAdd(pSort, pArea->dbfarea.ulRecNo, szBuffer, 8);
          break;

        case Harbour::Item::LOGICAL:
          szBuffer[0] = pItem->getL() ? 'T' : 'F';
          hb_ntxSortKeyAdd(pSort, pArea->dbfarea.ulRecNo, szBuffer, 1);
          break;

        default:
          hb_ntxErrorRT(pArea, EG_DATATYPE, EDBF_INVALIDKEY, pTag->pIndex->IndexName, 0, 0, nullptr);
          errCode = Harbour::FAILURE;
          pTag->Partial = true;
          pEvalItem = nullptr;
          ulNextCount = 1;
          break;
        }
      }

      if (ulNextCount > 0) {
        if (--ulNextCount == 0) {
          break;
        }
      }

      if (pEvalItem) {
        if (lStep >= pArea->dbfarea.area.lpdbOrdCondInfo->lStep) {
          lStep = 0;
          if (!hb_ntxEvalCond(pArea, pEvalItem, false)) {
            pTag->Partial = true;
            break;
          }
        }
        ++lStep;
      }

      if (fDirectRead) {
        ulRecNo++;
      } else if (errCode == Harbour::SUCCESS) {
        errCode = SELF_SKIPRAW(&pArea->dbfarea.area, 1);
        if (fUseFilter && errCode == Harbour::SUCCESS) {
          errCode = SELF_SKIPFILTER(&pArea->dbfarea.area, 1);
        }
        ulRecNo = pArea->dbfarea.ulRecNo;
      }
    }

    if (fDirectRead) {
      pArea->dbfarea.pRecord = pSaveRecBuff;
      pArea->dbfarea.fValidBuffer = false;
      if (errCode == Harbour::SUCCESS) {
        errCode = SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
      }
    }

    if (errCode == Harbour::SUCCESS) {
      hb_ntxSortOut(pSort);
    }

    if (pTag->nField) {
      hb_itemRelease(pItem);
    }

    pArea->lpCurTag = pSaveTag;
    hb_cdpSelect(cdpTmp);
  }

  hb_ntxSortFree(pSort, true);
  pArea->pSort = nullptr;

  return errCode;
}

/*
 * recreate tags in index file
 */
static HB_ERRCODE hb_ntxReIndex(LPNTXINDEX pIndex)
{
  HB_ERRCODE errCode = Harbour::FAILURE;

  if (hb_ntxIndexLockWrite(pIndex, false)) {
    errCode = Harbour::SUCCESS;
    hb_ntxIndexTrunc(pIndex);

    for (auto i = 0; i < pIndex->iTags; i++) {
      LPTAGINFO pTag = pIndex->lpTags[i];
      pTag->HeadBlock = pTag->RootBlock = pTag->keyCount = 0;
      pTag->HdrChanged = true;
      errCode = hb_ntxTagCreate(pTag, true);
      if (errCode != Harbour::SUCCESS) {
        break;
      }
    }
    hb_ntxIndexUnLockWrite(pIndex);
  }
  return errCode;
}

/* ************************************************************************* */

/* Implementation of exported functions */

#define hb_ntxBof nullptr
#define hb_ntxEof nullptr
#define hb_ntxFound nullptr

static HB_ERRCODE hb_ntxGoBottom(NTXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxGoBottom(%p)", static_cast<void*>(pArea)));
#endif

  HB_ERRCODE retval;

  if (SELF_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE) {
    return Harbour::FAILURE;
  }

  if (!pArea->lpCurTag) {
    return SUPER_GOBOTTOM(&pArea->dbfarea.area);
  }

  if (pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped) {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  if (!hb_ntxTagLockRead(pArea->lpCurTag)) {
    return Harbour::FAILURE;
  }
  hb_ntxTagRefreshScope(pArea->lpCurTag);

  hb_ntxTagGoBottom(pArea->lpCurTag);

  pArea->dbfarea.area.fTop = false;
  pArea->dbfarea.area.fBottom = true;

  if (pArea->lpCurTag->TagEOF) {
    retval = SELF_GOTO(&pArea->dbfarea.area, 0);
  } else {
    retval = SELF_GOTO(&pArea->dbfarea.area, pArea->lpCurTag->CurKeyInfo->Xtra);
    if (retval != Harbour::FAILURE && pArea->dbfarea.fPositioned) {
      retval = SELF_SKIPFILTER(&pArea->dbfarea.area, -1);
    }
  }
  hb_ntxTagUnLockRead(pArea->lpCurTag);

  return retval;
}

#define hb_ntxGoTo nullptr
#define hb_ntxGoToId nullptr

static HB_ERRCODE hb_ntxGoTop(NTXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxGoTop(%p)", static_cast<void*>(pArea)));
#endif

  HB_ERRCODE retval;

  if (SELF_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE) {
    return Harbour::FAILURE;
  }

  if (!pArea->lpCurTag) {
    return SUPER_GOTOP(&pArea->dbfarea.area);
  }

  if (pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped) {
    SELF_FORCEREL(&pArea->dbfarea.area);
  }

  if (!hb_ntxTagLockRead(pArea->lpCurTag)) {
    return Harbour::FAILURE;
  }
  hb_ntxTagRefreshScope(pArea->lpCurTag);

  hb_ntxTagGoTop(pArea->lpCurTag);

  pArea->dbfarea.area.fTop = true;
  pArea->dbfarea.area.fBottom = false;

  if (pArea->lpCurTag->TagEOF) {
    retval = SELF_GOTO(&pArea->dbfarea.area, 0);
  } else {
    retval = SELF_GOTO(&pArea->dbfarea.area, pArea->lpCurTag->CurKeyInfo->Xtra);
    if (retval != Harbour::FAILURE && pArea->dbfarea.fPositioned) {
      retval = SELF_SKIPFILTER(&pArea->dbfarea.area, 1);
    }
  }
  hb_ntxTagUnLockRead(pArea->lpCurTag);

  return retval;
}

static HB_ERRCODE hb_ntxSeek(NTXAREAP pArea, HB_BOOL fSoftSeek, PHB_ITEM pItem, HB_BOOL fFindLast)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxSeek(%p, %d, %p, %d)", static_cast<void*>(pArea), fSoftSeek, static_cast<void*>(pItem), fFindLast));
#endif

  if (SELF_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE) {
    return Harbour::FAILURE;
  }

  if (!pArea->lpCurTag) {
    hb_ntxErrorRT(pArea, EG_NOORDER, EDBF_NOTINDEXED, nullptr, 0, EF_CANDEFAULT, nullptr);
    return Harbour::FAILURE;
  } else {
    LPKEYINFO pKey;
    HB_ERRCODE retval = Harbour::SUCCESS;
    auto fEOF = false;
    auto fLast = false;
    HB_USHORT uiLen;
    HB_ULONG ulRec;

    if (pArea->dbfarea.lpdbPendingRel && pArea->dbfarea.lpdbPendingRel->isScoped) {
      SELF_FORCEREL(&pArea->dbfarea.area);
    }

    pArea->dbfarea.area.fTop = pArea->dbfarea.area.fBottom = false;
    pArea->dbfarea.area.fEof = false;

    fLast = pArea->lpCurTag->fUsrDescend == pArea->lpCurTag->AscendKey ? !fFindLast : fFindLast;

    pKey =
        hb_ntxKeyPutItem(nullptr, pItem, fLast ? NTX_MAX_REC_NUM : NTX_IGNORE_REC_NUM, pArea->lpCurTag, true, &uiLen);

    if (!hb_ntxTagLockRead(pArea->lpCurTag)) {
      hb_ntxKeyFree(pKey);
      return Harbour::FAILURE;
    }
    hb_ntxTagRefreshScope(pArea->lpCurTag);

    if (hb_ntxTagKeyFind(pArea->lpCurTag, pKey, uiLen)) {
      ulRec = pArea->lpCurTag->CurKeyInfo->Xtra;
    } else {
      ulRec = 0;
    }

    if ((ulRec == 0 && !fSoftSeek) || pArea->lpCurTag->TagEOF) {
      fEOF = true;
    } else {
      if (!hb_ntxInBottomScope(pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo->key)) {
        fEOF = true;
      } else if (!hb_ntxInTopScope(pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo->key)) {
        hb_ntxTagGoTop(pArea->lpCurTag);
        if (pArea->lpCurTag->CurKeyInfo->Xtra == 0 || pArea->lpCurTag->TagEOF) {
          fEOF = true;
        }
      }
    }
    hb_ntxTagUnLockRead(pArea->lpCurTag);
    if (!fEOF) {
      retval = SELF_GOTO(&pArea->dbfarea.area, pArea->lpCurTag->CurKeyInfo->Xtra);
      if (retval != Harbour::FAILURE && pArea->dbfarea.fPositioned) {
        retval = SELF_SKIPFILTER(&pArea->dbfarea.area, fFindLast ? -1 : 1);
        if (retval != Harbour::FAILURE && ulRec && pArea->dbfarea.fPositioned) {
          pArea->dbfarea.area.fFound =
              (ulRec == pArea->dbfarea.ulRecNo ||
               hb_ntxValCompare(pArea->lpCurTag, pKey->key, uiLen, pArea->lpCurTag->CurKeyInfo->key,
                                pArea->lpCurTag->KeyLength, false) == 0);
          if (!pArea->dbfarea.area.fFound && !fSoftSeek) {
            fEOF = true;
          }
        }
      }
    }
    if (retval != Harbour::FAILURE && (fEOF || !hb_ntxKeyInScope(pArea->lpCurTag, pArea->lpCurTag->CurKeyInfo))) {
      retval = SELF_GOTO(&pArea->dbfarea.area, 0);
    }
    if (pArea->dbfarea.fPositioned || pArea->dbfarea.ulRecNo != 1) {
      pArea->dbfarea.area.fBof = false;
    }
    hb_ntxKeyFree(pKey);
    return retval;
  }
}

#define hb_ntxSkip nullptr
#define hb_ntxSkipFilter nullptr

static HB_ERRCODE hb_ntxSkipRaw(NTXAREAP pArea, HB_LONG lToSkip)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxSkipRaw(%p, %ld)", static_cast<void*>(pArea), lToSkip));
#endif

  HB_ERRCODE retval;
  auto fOut = false;
  auto fForward = false;

  if (SELF_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE) {
    return Harbour::FAILURE;
  }

  if (!pArea->lpCurTag || lToSkip == 0) {
    return SUPER_SKIPRAW(&pArea->dbfarea.area, lToSkip);
  }

  if (!hb_ntxTagLockRead(pArea->lpCurTag)) {
    return Harbour::FAILURE;
  }
  hb_ntxTagRefreshScope(pArea->lpCurTag);

  fForward = (lToSkip > 0);

  if (!hb_ntxCurKeyRefresh(pArea->lpCurTag)) {
    if (fForward || pArea->dbfarea.fPositioned) {
      fOut = true;
    } else {
      hb_ntxTagGoBottom(pArea->lpCurTag);
      fOut = pArea->lpCurTag->TagEOF;
      lToSkip++;
    }
  }

  if (fForward) {
    while (!fOut && !pArea->lpCurTag->TagEOF && lToSkip-- > 0) {
      hb_ntxTagSkipNext(pArea->lpCurTag);
    }
    retval = SELF_GOTO(&pArea->dbfarea.area, (pArea->lpCurTag->TagEOF || fOut) ? 0 : pArea->lpCurTag->CurKeyInfo->Xtra);
  } else { /* if( lToSkip < 0 ) */
    while (!fOut && !pArea->lpCurTag->TagBOF && lToSkip++ < 0) {
      hb_ntxTagSkipPrev(pArea->lpCurTag);
    }
    if (fOut || pArea->lpCurTag->TagBOF) {
      hb_ntxTagGoTop(pArea->lpCurTag);
      fOut = true;
    }
    retval = SELF_GOTO(&pArea->dbfarea.area, pArea->lpCurTag->TagEOF ? 0 : pArea->lpCurTag->CurKeyInfo->Xtra);
    pArea->dbfarea.area.fBof = fOut;
  }

  hb_ntxTagUnLockRead(pArea->lpCurTag);
  /* Update Bof and Eof flags */
#if 0
   if( fForward ) {
      pArea->dbfarea.area.fBof = false;
   } else {
      pArea->dbfarea.area.fEof = false;
   }
#endif
  return retval;
}

#define hb_ntxAddField nullptr
#define hb_ntxAppend nullptr
#define hb_ntxCreateFields nullptr
#define hb_ntxDeleteRec nullptr
#define hb_ntxDeleted nullptr
#define hb_ntxFieldCount nullptr
#define hb_ntxFieldDisplay nullptr
#define hb_ntxFieldInfo nullptr
#define hb_ntxFieldName nullptr

/*
 * Flush _system_ buffers to disk
 */
static HB_ERRCODE hb_ntxFlush(NTXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxFlush(%p)", static_cast<void*>(pArea)));
#endif

  HB_ERRCODE errCode = SELF_GOCOLD(&pArea->dbfarea.area);
  if (errCode == Harbour::SUCCESS) {
    errCode = SUPER_FLUSH(&pArea->dbfarea.area);

    if (hb_setGetHardCommit()) {
      LPNTXINDEX pIndex = pArea->lpIndexes;
      while (pIndex) {
        if (pIndex->fFlush /* && !pIndex->Temporary */) {
          hb_fileCommit(pIndex->DiskFile);
          pIndex->fFlush = false;
        }
        pIndex = pIndex->pNext;
      }
    }
  }

  return errCode;
}

#define hb_ntxGetRec nullptr
#define hb_ntxGetValue nullptr
#define hb_ntxGetVarLen nullptr

/*
 * Perform a write of WorkArea memory to the data store.
 */
static HB_ERRCODE hb_ntxGoCold(NTXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxGoCold(%p)", static_cast<void*>(pArea)));
#endif

  bool fRecordChanged = pArea->dbfarea.fRecordChanged;
  bool fAppend = pArea->dbfarea.fAppend;

  if (SUPER_GOCOLD(&pArea->dbfarea.area) == Harbour::SUCCESS) {
    if (fRecordChanged || pArea->fNtxAppend) {
      if (fAppend && pArea->dbfarea.fShared) {
        if (pArea->fNtxAppend) {
          hb_errInternal(9312, "hb_ntxGoCold: multiple appending without GOCOLD.", nullptr, nullptr);
        }
        pArea->fNtxAppend = true;
      } else {
        LPNTXINDEX pIndex = pArea->lpIndexes;
        LPTAGINFO pTag;
        LPKEYINFO pKey;
        auto fAdd = false;
        auto fDel = false;
        auto fLck = false;
        int i;

        /* The pending relation may move the record pointer so we should
           disable them for KEY/FOR evaluation */
        LPDBRELINFO lpdbPendingRel = pArea->dbfarea.lpdbPendingRel;
        pArea->dbfarea.lpdbPendingRel = nullptr;

        if (pArea->dbfarea.fShared) {
          fAppend = pArea->fNtxAppend;
          pArea->fNtxAppend = false;
        }

        while (pIndex) {
          for (i = 0; i < pIndex->iTags; i++) {
            pTag = pIndex->lpTags[i];
            if (pIndex->fReadonly || pTag->Custom || (pTag->pIndex->Compound && !pTag->HeadBlock) ||
                (fAppend && pTag->ChgOnly)) {
              continue;
            }

            pKey = hb_ntxEvalKey(nullptr, pTag);

            fAdd = (pTag->pForItem == nullptr || hb_ntxEvalCond(pArea, pTag->pForItem, true));
            if (fAppend) {
              fDel = false;
            } else {
              if (hb_ntxValCompare(pTag, pKey->key, pTag->KeyLength, pTag->HotKeyInfo->key, pTag->KeyLength, true) ==
                  0) {
                if (pTag->HotFor ? fAdd : !fAdd) {
                  fAdd = fDel = false;
                } else {
                  fDel = !fAdd;
                }
              } else {
                fDel = pTag->HotFor || pTag->Partial;
              }
            }
            if (fDel || fAdd) {
              if (!fLck) {
                if (!hb_ntxIndexLockWrite(pIndex, true)) {
                  hb_ntxKeyFree(pKey);
                  break;
                }
                fLck = true;
              }
              if ((pTag->pIndex->Compound && !pTag->HeadBlock) || !hb_ntxTagHeaderCheck(pTag)) {
                fAdd = fDel = false;
              }
              if (fDel) {
                if (hb_ntxTagKeyDel(pTag, pTag->HotKeyInfo)) {
                  if (!pIndex->fShared && pTag->keyCount && hb_ntxKeyInScope(pTag, pTag->HotKeyInfo)) {
                    pTag->keyCount--;
                  }
                } else {
                  if (pTag->ChgOnly) {
                    fAdd = false;
                  } else if (!pTag->Partial && !pTag->UniqueKey) {
                    hb_ntxErrorRT(pTag->pIndex->pArea, EG_CORRUPTION, EDBF_CORRUPT, pTag->pIndex->IndexName, 0, 0,
                                  nullptr);
                  }
                }
              }
              if (fAdd) {
                if (hb_ntxTagKeyAdd(pTag, pKey)) {
                  if (!pIndex->fShared && pTag->keyCount && hb_ntxKeyInScope(pTag, pKey)) {
                    pTag->keyCount++;
                  }
                }
              }
            }
            hb_ntxKeyFree(pKey);
          }
          if (fLck) {
            hb_ntxIndexUnLockWrite(pIndex);
            fLck = false;
          }
          pIndex = pIndex->pNext;
        }

        /* Restore disabled pending relation */
        pArea->dbfarea.lpdbPendingRel = lpdbPendingRel;
      }
    }
    return Harbour::SUCCESS;
  }
  return Harbour::FAILURE;
}

/*
 * Mark the WorkArea data buffer as hot.
 */
static HB_ERRCODE hb_ntxGoHot(NTXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxGoHot(%p)", static_cast<void*>(pArea)));
#endif

  HB_ERRCODE errCode = SUPER_GOHOT(&pArea->dbfarea.area);
  if (errCode == Harbour::SUCCESS) {
    if (!pArea->fNtxAppend) {
      LPNTXINDEX pIndex = pArea->lpIndexes;
      LPTAGINFO pTag;

      while (pIndex) {
        if (!pIndex->fReadonly) {
          for (auto i = 0; i < pIndex->iTags; i++) {
            pTag = pIndex->lpTags[i];
            if (!pTag->Custom) {
              pTag->HotKeyInfo = hb_ntxEvalKey(pTag->HotKeyInfo, pTag);
              pTag->HotFor = (pTag->pForItem == nullptr || hb_ntxEvalCond(pArea, pTag->pForItem, true));
            }
          }
        }
        pIndex = pIndex->pNext;
      }
    }
    return Harbour::SUCCESS;
  }
  return errCode;
}

#define hb_ntxPutRec nullptr
#define hb_ntxPutValue nullptr
#define hb_ntxRecall nullptr
#define hb_ntxRecCount nullptr
#define hb_ntxRecInfo nullptr
#define hb_ntxRecNo nullptr
#define hb_ntxRecId nullptr
#define hb_ntxSetFieldsExtent nullptr
#define hb_ntxAlias nullptr

/*
 * Close the table in the WorkArea.
 */
static HB_ERRCODE hb_ntxClose(NTXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxClose(%p)", static_cast<void*>(pArea)));
#endif

  if (SELF_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE) {
    return Harbour::FAILURE;
  }

  HB_ERRCODE errCode = SUPER_CLOSE(&pArea->dbfarea.area);

  if (errCode == Harbour::SUCCESS) {
    if (pArea->pSort) {
      hb_ntxSortFree(pArea->pSort, true);
      pArea->pSort = nullptr;
    }

    SELF_ORDLSTCLEAR(&pArea->dbfarea.area);

    /* close also production indexes if any */
    while (pArea->lpIndexes) {
      LPNTXINDEX pIndex = pArea->lpIndexes;
      pArea->lpIndexes = pIndex->pNext;
      hb_ntxIndexFree(pIndex);
    }

#ifdef HB_NTX_DEBUG_DISP
    printf("\r\n#reads=%ld, #writes=%ld\r\n", s_rdNO, s_wrNO);
    fflush(stdout);
#endif
  }

  return errCode;
}

#define hb_ntxCreate nullptr
#define hb_ntxInfo nullptr
#define hb_ntxNewArea nullptr

/*
 * Retrieve the size of the WorkArea structure.
 */
static HB_ERRCODE hb_ntxStructSize(NTXAREAP pArea, HB_USHORT *uiSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxStructSize(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(uiSize)));
#endif
  HB_SYMBOL_UNUSED(pArea);

  *uiSize = sizeof(NTXAREA);
  return Harbour::SUCCESS;
}

/*
 * Open a data store in the WorkArea.
 */
static HB_ERRCODE hb_ntxOpen(NTXAREAP pArea, LPDBOPENINFO pOpenInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxOpen(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOpenInfo)));
#endif

  HB_ERRCODE errCode = SUPER_OPEN(&pArea->dbfarea.area, pOpenInfo);

  if (errCode == Harbour::SUCCESS && DBFAREA_DATA(&pArea->dbfarea)->fStruct &&
      (DBFAREA_DATA(&pArea->dbfarea)->fStrictStruct ? pArea->dbfarea.fHasTags : hb_setGetAutOpen())) {
    char szFileName[HB_PATH_MAX];

    hb_ntxCreateFName(pArea, nullptr, nullptr, szFileName, nullptr);
    /* CL5.2 DBFCDX and Six3 CDX/NSX RDDs looking for
       production indexes respect SET PATH but Harbour in
       core DBF* index RDDs is CL5.3/COMIX compatible and
       looks for production indexes only in the directory
       where DBF file is located and only SIXCDX Harbour
       RDD is CL5.2/Six3 compatible [druzus] */
    if (hb_fileExists(szFileName, nullptr) || DBFAREA_DATA(&pArea->dbfarea)->fStrictStruct) {
      DBORDERINFO pOrderInfo;

      pOrderInfo.itmResult = hb_itemPutNI(nullptr, 0);
      pOrderInfo.atomBagName = hb_itemPutC(nullptr, szFileName);
      pOrderInfo.itmNewVal = nullptr;
      pOrderInfo.itmOrder = nullptr;
      errCode = SELF_ORDLSTADD(&pArea->dbfarea.area, &pOrderInfo);
      if (errCode == Harbour::SUCCESS) {
        pOrderInfo.itmOrder = hb_itemPutNI(nullptr, hb_setGetAutOrder());
        errCode = SELF_ORDLSTFOCUS(&pArea->dbfarea.area, &pOrderInfo);
        hb_itemRelease(pOrderInfo.itmOrder);
        if (errCode == Harbour::SUCCESS) {
          errCode = SELF_GOTOP(&pArea->dbfarea.area);
        }
      }
      hb_itemRelease(pOrderInfo.atomBagName);
      hb_itemRelease(pOrderInfo.itmResult);
    }
  }

  return errCode;
}

#define hb_ntxRelease nullptr
#define hb_ntxSysName nullptr
#define hb_ntxEval nullptr

static HB_ERRCODE hb_ntxPack(NTXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxPack(%p)", static_cast<void*>(pArea)));
#endif

  HB_ERRCODE errCode = SUPER_PACK(&pArea->dbfarea.area);
  if (errCode == Harbour::SUCCESS) {
    return SELF_ORDLSTREBUILD(&pArea->dbfarea.area);
  }

  return errCode;
}

#define ntPackRec nullptr
#define hb_ntxSort nullptr
#define hb_ntxTrans nullptr
#define hb_ntxTransRec nullptr

static HB_ERRCODE hb_ntxZap(NTXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxZap(%p)", static_cast<void*>(pArea)));
#endif

  HB_ERRCODE errCode = SUPER_ZAP(&pArea->dbfarea.area);
  if (errCode == Harbour::SUCCESS) {
    return SELF_ORDLSTREBUILD(&pArea->dbfarea.area);
  }

  return errCode;
}

#define hb_ntxchildEnd nullptr
#define hb_ntxchildStart nullptr
#define hb_ntxchildSync nullptr
#define hb_ntxsyncChildren nullptr
#define hb_ntxclearRel nullptr
#define hb_ntxforceRel nullptr
#define hb_ntxrelArea nullptr
#define hb_ntxrelEval nullptr
#define hb_ntxrelText nullptr
#define hb_ntxsetRel nullptr

#define hb_ntxOrderCondition nullptr

static HB_ERRCODE hb_ntxOrderCreate(NTXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxOrderCreate(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOrderInfo)));
#endif

  PHB_ITEM pResult, pKeyExp, pForExp = nullptr;
  int iLen, iDec, iTag, i;
  char szFileName[HB_PATH_MAX], szTagName[NTX_MAX_TAGNAME + 1];
  const char *szFor = nullptr;
  LPNTXINDEX pIndex, *pIndexPtr;
  LPTAGINFO pTag = nullptr;
  LPDBFDATA pData;
  HB_ULONG ulRecNo;
  auto fProd = false;
  auto fCompound = false;
  auto fTagName = false;
  auto fBagName = false;
  auto fLocked = false;
  auto fAscend = true;
  auto fCustom = false;
  auto fTemporary = false;
  auto fExclusive = false;
  HB_BYTE bType;

  HB_ERRCODE errCode = SELF_GOCOLD(&pArea->dbfarea.area);
  if (errCode != Harbour::SUCCESS) {
    return errCode;
  }

  if (pArea->dbfarea.lpdbPendingRel) {
    errCode = SELF_FORCEREL(&pArea->dbfarea.area);
    if (errCode != Harbour::SUCCESS) {
      return errCode;
    }
  }

  auto szKey = hb_itemGetCPtr(pOrderInfo->abExpr);
  /* If we have a codeblock for the expression, use it */
  if (pOrderInfo->itmCobExpr) {
    pKeyExp = hb_itemNew(pOrderInfo->itmCobExpr);
  } else { /* Otherwise, try compiling the key expression string */
    errCode = SELF_COMPILE(&pArea->dbfarea.area, szKey);
    if (errCode != Harbour::SUCCESS) {
      return errCode;
    }
    pKeyExp = pArea->dbfarea.area.valResult;
    pArea->dbfarea.area.valResult = nullptr;
  }

  /* Get a blank record before testing expression */
  ulRecNo = pArea->dbfarea.ulRecNo;
  errCode = SELF_GOTO(&pArea->dbfarea.area, 0);
  if (errCode != Harbour::SUCCESS) {
    return errCode;
  }

  errCode = SELF_EVALBLOCK(&pArea->dbfarea.area, pKeyExp);
  if (errCode != Harbour::SUCCESS) {
    hb_vmDestroyBlockOrMacro(pKeyExp);
    SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
    return errCode;
  }
  pResult = pArea->dbfarea.area.valResult;
  pArea->dbfarea.area.valResult = nullptr;

  bType = hb_ntxItemType(pResult);
  iLen = iDec = 0;
  switch (bType) {
  case 'N':
    hb_itemGetNLen(pResult, &iLen, &iDec);
    if (iDec) {
      iLen += iDec + 1;
    }
    break;
  case 'D':
    iLen = 8;
    break;
  case 'T':
    iLen = 17;
    break;
  case 'L':
    iLen = 1;
    break;
  case 'C':
    iLen = static_cast<HB_INTCAST>(pResult->getCLen());
    if (iLen > NTX_MAX_KEY) {
      iLen = NTX_MAX_KEY;
    }
    break;
  default:
    bType = 'U';
  }
  hb_itemRelease(pResult);

  /* Make sure KEY has proper type and iLen is not 0 */
  if (bType == 'U' || iLen == 0) {
    hb_vmDestroyBlockOrMacro(pKeyExp);
    SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
    hb_ntxErrorRT(pArea, bType == 'U' ? EG_DATATYPE : EG_DATAWIDTH, EDBF_INVALIDKEY, nullptr, 0, 0, nullptr);
    return Harbour::FAILURE;
  }

  if (pArea->dbfarea.area.lpdbOrdCondInfo) {
    fAscend = !pArea->dbfarea.area.lpdbOrdCondInfo->fDescending;
    fCustom = pArea->dbfarea.area.lpdbOrdCondInfo->fCustom;
    fTemporary = pArea->dbfarea.area.lpdbOrdCondInfo->fTemporary;
    fExclusive = pArea->dbfarea.area.lpdbOrdCondInfo->fExclusive;
    /* Check conditional expression */
    szFor = pArea->dbfarea.area.lpdbOrdCondInfo->abFor;
    if (pArea->dbfarea.area.lpdbOrdCondInfo->itmCobFor) {
      /* If we have a codeblock for the conditional expression, use it */
      pForExp = hb_itemNew(pArea->dbfarea.area.lpdbOrdCondInfo->itmCobFor);
    } else if (szFor != nullptr) {
      /* Otherwise, try compiling the conditional expression string */
      errCode = SELF_COMPILE(&pArea->dbfarea.area, szFor);
      if (errCode != Harbour::SUCCESS) {
        hb_vmDestroyBlockOrMacro(pKeyExp);
        SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
        return errCode;
      }
      pForExp = pArea->dbfarea.area.valResult;
      pArea->dbfarea.area.valResult = nullptr;
    }
  }

  if (pArea->dbfarea.fTemporary) {
    fTemporary = true;
  }

  /* Test conditional expression */
  if (pForExp) {
    auto fOK = false;

    errCode = SELF_EVALBLOCK(&pArea->dbfarea.area, pForExp);
    if (errCode != Harbour::SUCCESS) {
      hb_vmDestroyBlockOrMacro(pKeyExp);
      hb_vmDestroyBlockOrMacro(pForExp);
      SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
      return errCode;
    }
    fOK = hb_itemType(pArea->dbfarea.area.valResult) & Harbour::Item::LOGICAL;
    hb_itemRelease(pArea->dbfarea.area.valResult);
    pArea->dbfarea.area.valResult = nullptr;
    if (!fOK) {
      hb_vmDestroyBlockOrMacro(pKeyExp);
      hb_vmDestroyBlockOrMacro(pForExp);
      SELF_GOTO(&pArea->dbfarea.area, ulRecNo);
      hb_ntxErrorRT(pArea, EG_DATATYPE, EDBF_INVALIDFOR, nullptr, 0, 0, nullptr);
      return Harbour::FAILURE;
    }
  }

  SELF_GOTO(&pArea->dbfarea.area, ulRecNo);

  pData = DBFAREA_DATA(&pArea->dbfarea);
  /*
   * abBagName -> cBag, atomBagName -> cTag
   * The following scheme implemented:
   * 1. abBagName == nullptr   -> add the Tag to the structural index
   *    if no compound index support then create new separate index
   *    with atomBagName
   * 2. atomBagName == nullptr -> overwrite any index file of abBagName
   * 3. ads the Tag to index file
   */
  fTagName = pOrderInfo->atomBagName && pOrderInfo->atomBagName[0];
  fBagName = pOrderInfo->abBagName && pOrderInfo->abBagName[0];
#if defined(HB_NTX_NOMULTITAG)
  fCompound = false;
#else
  fCompound = fTagName && pData->fMultiTag;
#endif
  hb_ntxCreateFName(pArea, (fBagName || fCompound) ? pOrderInfo->abBagName : pOrderInfo->atomBagName, &fProd,
                    szFileName, szTagName);
  if (fTagName) {
    hb_strncpyUpperTrim(szTagName, pOrderInfo->atomBagName, NTX_MAX_TAGNAME);
  }

  pIndex = hb_ntxFindBag(pArea, szFileName);
  if (pIndex && !fCompound) {
    pIndexPtr = &pArea->lpIndexes;
    while (*pIndexPtr) {
      if (pIndex == *pIndexPtr) {
        *pIndexPtr = pIndex->pNext;
        hb_ntxIndexFree(pIndex);
        break;
      }
      pIndexPtr = &(*pIndexPtr)->pNext;
    }
    pIndex = nullptr;
  }

  if (pIndex) {
    if (pIndex->fReadonly) {
      hb_vmDestroyBlockOrMacro(pKeyExp);
      if (pForExp != nullptr) {
        hb_vmDestroyBlockOrMacro(pForExp);
      }
      hb_ntxErrorRT(pArea, EG_READONLY, EDBF_READONLY, pIndex->IndexName, 0, 0, nullptr);
      return Harbour::FAILURE;
    }
#if 0 /* enable this code if you want to forbid tag deleting in shared mode */
      else if( pIndex->fShared ) {
         hb_vmDestroyBlockOrMacro(pKeyExp);
         if( pForExp != nullptr ) {
            hb_vmDestroyBlockOrMacro(pForExp);
         }
         hb_ntxErrorRT(pArea, EG_SHARED, EDBF_SHARED, pIndex->IndexName, 0, 0, nullptr);
         return Harbour::FAILURE;
      }
#endif
  } else {
    PHB_FILE pFile;
    auto bRetry = false;
    auto fOld = false;
    bool fShared = pArea->dbfarea.fShared && !fTemporary && !fExclusive;
    PHB_ITEM pError = nullptr;
    char szSpFile[HB_PATH_MAX];

    fOld = fCompound;
    do {
      if (fTemporary) {
        pFile = hb_fileCreateTemp(nullptr, nullptr, FC_NORMAL, szSpFile);
        fOld = false;
      } else {
        pFile =
            hb_fileExtOpen(szFileName, nullptr,
                           FO_READWRITE | (fShared ? FO_DENYNONE : FO_EXCLUSIVE) | (fOld ? FXO_APPEND : FXO_TRUNCATE) |
                               FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME | FXO_NOSEEKPOS,
                           nullptr, pError);
      }
      if (!pFile) {
        bRetry = hb_ntxErrorRT(pArea, EG_CREATE, EDBF_CREATE, szFileName, hb_fsError(), EF_CANRETRY | EF_CANDEFAULT,
                               &pError) == E_RETRY;
      } else {
        bRetry = false;
        if (fOld) {
          fOld = hb_fileSize(pFile) != 0;
        }
      }
    } while (bRetry);

    if (pError) {
      hb_errRelease(pError);
    }

    if (!pFile) {
      hb_vmDestroyBlockOrMacro(pKeyExp);
      if (pForExp != nullptr) {
        hb_vmDestroyBlockOrMacro(pForExp);
      }
      return Harbour::FAILURE;
    }

    pIndex = hb_ntxIndexNew(pArea);
    pIndex->IndexName = hb_strdup(szFileName);
    pIndex->fReadonly = false;
    pIndex->fShared = fShared;
    pIndex->DiskFile = pFile;
    pIndex->fDelete = fTemporary;
    if (fTemporary) {
      pIndex->RealName = hb_strdup(szSpFile);
    } else {
      pIndex->Production = fProd;
    }

    pIndexPtr = &pArea->lpIndexes;
    while (*pIndexPtr) {
      pIndexPtr = &(*pIndexPtr)->pNext;
    }
    *pIndexPtr = pIndex;
    pArea->fSetTagNumbers = true;
    if (fOld) {
      if (!hb_ntxIndexLockWrite(pIndex, true)) {
        errCode = Harbour::FAILURE;
      } else {
        errCode = hb_ntxIndexLoad(pIndex, szTagName);
        if (errCode != Harbour::SUCCESS) {
          hb_ntxIndexUnLockWrite(pIndex);
        } else {
          fLocked = true;
        }
      }
      if (errCode != Harbour::SUCCESS) {
        *pIndexPtr = pIndex->pNext;
        hb_ntxIndexFree(pIndex);
        hb_vmDestroyBlockOrMacro(pKeyExp);
        if (pForExp != nullptr) {
          hb_vmDestroyBlockOrMacro(pForExp);
        }
        hb_ntxErrorRT(pArea, EG_CORRUPTION, EDBF_CORRUPT, szFileName, 0, 0, nullptr);
        return errCode;
      }
    } else {
      pIndex->LargeFile = (pIndex->pArea->dbfarea.bLockType == DB_DBFLOCK_HB64);
    }
  }

  iTag = hb_ntxFindTagByName(pIndex, szTagName);
  fCompound = (pIndex->iTags > (iTag ? 1 : 0));

  if (!iTag && pIndex->iTags == CTX_MAX_TAGS) {
    if (fLocked) {
      hb_ntxIndexUnLockWrite(pIndex);
    }
    hb_vmDestroyBlockOrMacro(pKeyExp);
    if (pForExp != nullptr) {
      hb_vmDestroyBlockOrMacro(pForExp);
    }
    hb_ntxErrorRT(pArea, EG_LIMIT, EDBF_LIMITEXCEEDED, pIndex->IndexName, 0, 0, nullptr);
    return Harbour::FAILURE;
  }

  if (!fLocked && !hb_ntxIndexLockWrite(pIndex, fCompound)) {
    errCode = Harbour::FAILURE;
  } else {
    if (static_cast<bool>(pIndex->Compound) != fCompound) {
      pIndex->Compound = fCompound;
      if (fCompound) {
        if (!pIndex->HeaderBuff) {
          pIndex->HeaderBuff = static_cast<HB_BYTE *>(hb_xgrab(NTXBLOCKSIZE));
        }
        memset(pIndex->HeaderBuff, 0, NTXBLOCKSIZE);
        pIndex->fValidHeader = true;
      }
      for (i = 0; i < pIndex->iTags; i++) {
        pIndex->lpTags[i]->HdrChanged = true;
        pIndex->lpTags[i]->HeadBlock = 0;
        if (fCompound) {
          hb_ntxIndexTagAdd(pIndex, pIndex->lpTags[i]);
        }
      }
    }
    pTag = hb_ntxTagNew(pIndex, szTagName, fTagName, szKey, pKeyExp, bType, static_cast<HB_USHORT>(iLen),
                        static_cast<HB_USHORT>(iDec), szFor, pForExp, fAscend, pOrderInfo->fUnique, fCustom,
                        pData->fSortRecNo);
    pTag->Partial = (pArea->dbfarea.area.lpdbOrdCondInfo && !pArea->dbfarea.area.lpdbOrdCondInfo->fAll);

    if (!pIndex->Compound) {
      while (pIndex->iTags) {
        hb_ntxTagDelete(pIndex->lpTags[0]);
      }
      hb_ntxIndexTrunc(pIndex);
      iTag = 0;
    }

    if (iTag) {
      pTag->HeadBlock = pIndex->lpTags[iTag - 1]->HeadBlock;
      if (hb_ntxTagHeaderCheck(pIndex->lpTags[iTag - 1]) &&
          !hb_ntxTagPagesFree(pIndex->lpTags[iTag - 1], pIndex->lpTags[iTag - 1]->RootBlock)) {
        errCode = Harbour::FAILURE;
      } else {
        pTag->uiNumber = pIndex->lpTags[iTag - 1]->uiNumber;
        hb_ntxTagFree(pIndex->lpTags[iTag - 1]);
        pIndex->lpTags[iTag - 1] = pTag;
      }
    } else {
      hb_ntxTagAdd(pIndex, pTag);
      if (pIndex->Compound) {
        hb_ntxIndexTagAdd(pIndex, pTag);
      }
    }

    if (errCode == Harbour::SUCCESS) {
      pIndex->Update = pIndex->Changed = pTag->HdrChanged = true;
      errCode = hb_ntxTagCreate(pTag, false);
    }
    hb_ntxIndexUnLockWrite(pIndex);
  }

  pIndexPtr = &pArea->lpIndexes;
  while (*pIndexPtr && *pIndexPtr != pIndex) {
    pIndexPtr = &(*pIndexPtr)->pNext;
  }

  /* It should not happen, reentrance? */
  if (!*pIndexPtr) {
    return Harbour::FAILURE;
  }

  if (errCode != Harbour::SUCCESS) {
    *pIndexPtr = pIndex->pNext;
    hb_ntxIndexFree(pIndex);
    return errCode;
  }

  if (!pArea->dbfarea.area.lpdbOrdCondInfo || !pArea->dbfarea.area.lpdbOrdCondInfo->fAdditive) {
    *pIndexPtr = pIndex->pNext;
    pIndex->pNext = nullptr;
    SELF_ORDLSTCLEAR(&pArea->dbfarea.area);
    pIndexPtr = &pArea->lpIndexes;
    while (*pIndexPtr) {
      pIndexPtr = &(*pIndexPtr)->pNext;
    }
    *pIndexPtr = pIndex;
  }
  if (pIndex->Production && !pArea->dbfarea.fHasTags && pData->fStruct &&
      (pData->fStrictStruct || hb_setGetAutOpen())) {
    pArea->dbfarea.fHasTags = true;
    if (!pArea->dbfarea.fReadonly && (pArea->dbfarea.dbfHeader.bHasTags & 0x01) == 0) {
      SELF_WRITEDBHEADER(&pArea->dbfarea.area);
    }
  }
  pArea->lpCurTag = pTag;
  SELF_ORDSETCOND(&pArea->dbfarea.area, nullptr);
  return SELF_GOTOP(&pArea->dbfarea.area);
}

static HB_ERRCODE hb_ntxOrderDestroy(NTXAREAP pArea, LPDBORDERINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxOrderDestroy(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOrderInfo)));
#endif

  HB_ERRCODE errCode = SELF_GOCOLD(&pArea->dbfarea.area);
  if (errCode != Harbour::SUCCESS) {
    return errCode;
  }

  if (pArea->dbfarea.lpdbPendingRel) {
    errCode = SELF_FORCEREL(&pArea->dbfarea.area);
    if (errCode != Harbour::SUCCESS) {
      return errCode;
    }
  }

  if (pOrderInfo->itmOrder) {
    LPTAGINFO pTag = hb_ntxFindTag(pArea, pOrderInfo->itmOrder, pOrderInfo->atomBagName);

    if (pTag != nullptr) {
      LPNTXINDEX pIndex = pTag->pIndex;

      if (pIndex->iTags == 1) {
        bool fProd = pIndex->Production;
        LPNTXINDEX *pIndexPtr = &pArea->lpIndexes;
        while (*pIndexPtr != pIndex) {
          pIndexPtr = &(*pIndexPtr)->pNext;
        }
        *pIndexPtr = pIndex->pNext;
        pIndex->fDelete = true;
        hb_ntxIndexFree(pIndex);
        if (fProd && pArea->dbfarea.fHasTags && DBFAREA_DATA(&pArea->dbfarea)->fStruct &&
            (DBFAREA_DATA(&pArea->dbfarea)->fStrictStruct || hb_setGetAutOpen())) {
          pArea->dbfarea.fHasTags = false;
          if (!pArea->dbfarea.fReadonly && (pArea->dbfarea.dbfHeader.bHasTags & 0x01) != 0) {
            SELF_WRITEDBHEADER(&pArea->dbfarea.area);
          }
        }
      } else if (pIndex->fReadonly) {
        hb_ntxErrorRT(pArea, EG_READONLY, EDBF_READONLY, pIndex->IndexName, 0, 0, nullptr);
        return Harbour::FAILURE;
      }
#if 0 /* enable this code if you want to forbid tag deleting in shared mode */
         else if( pIndex->fShared ) {
            hb_ntxErrorRT(pArea, EG_SHARED, EDBF_SHARED, pIndex->IndexName, 0, 0, nullptr);
            return Harbour::FAILURE;
         }
#endif
      else if (!hb_ntxIndexLockWrite(pIndex, true)) {
        return Harbour::FAILURE;
      } else {
        errCode = hb_ntxTagSpaceFree(pTag);
        hb_ntxIndexUnLockWrite(pIndex);
      }
    }
  }

  return errCode;
}

static HB_ERRCODE hb_ntxOrderInfo(NTXAREAP pArea, HB_USHORT uiIndex, LPDBORDERINFO pInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxOrderInfo(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pInfo)));
#endif

  LPTAGINFO pTag;

  switch (uiIndex) {
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
    pInfo->itmResult = hb_itemPutC(pInfo->itmResult, pArea->pSort ? pArea->pSort->pTag->TagName : nullptr);
    return Harbour::SUCCESS;
  case DBOI_I_BAGNAME:
    pInfo->itmResult = hb_itemPutC(pInfo->itmResult, pArea->pSort ? pArea->pSort->pTag->pIndex->IndexName : nullptr);
    return Harbour::SUCCESS;
  case DBOI_ISREINDEX:
    pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pArea->pSort ? pArea->pSort->fReindex : false);
    return Harbour::SUCCESS;
  case DBOI_LOCKOFFSET:
  case DBOI_HPLOCKING: {
    HB_DBFLOCKDATA lockData;

    hb_dbfLockIdxGetData(pArea->dbfarea.bLockType, &lockData);
    if (uiIndex == DBOI_LOCKOFFSET) {
      pInfo->itmResult = hb_itemPutNInt(pInfo->itmResult, lockData.offset);
    } else {
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, lockData.size > 0);
    }
    return Harbour::SUCCESS;
  }
  case DBOI_ORDERCOUNT: {
    int i;

    if (hb_itemGetCLen(pInfo->atomBagName) > 0) {
      LPNTXINDEX pIndex = hb_ntxFindBag(pArea, pInfo->atomBagName->getCPtr());
      i = pIndex ? pIndex->iTags : 0;
    } else {
      i = hb_ntxTagCount(pArea);
    }

    pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, i);
    return Harbour::SUCCESS;
  }
  case DBOI_BAGCOUNT: {
    int i = 0;
    LPNTXINDEX pIndex = pArea->lpIndexes;
    while (pIndex) {
      ++i;
      pIndex = pIndex->pNext;
    }
    pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, i);
    return Harbour::SUCCESS;
  }
  case DBOI_BAGNUMBER: {
    LPNTXINDEX pIndex = pArea->lpIndexes, pIndexSeek = nullptr;
    int i = 0;

    if (hb_itemGetCLen(pInfo->atomBagName) > 0) {
      pIndexSeek = hb_ntxFindBag(pArea, pInfo->atomBagName->getCPtr());
    } else if (pArea->lpCurTag) {
      pIndexSeek = pArea->lpCurTag->pIndex;
    }

    if (pIndexSeek) {
      do {
        ++i;
        if (pIndex == pIndexSeek) {
          break;
        }
        pIndex = pIndex->pNext;
      } while (pIndex);
    }
    pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, pIndex ? i : 0);
    return Harbour::SUCCESS;
  }
  case DBOI_BAGORDER: {
    LPNTXINDEX pIndex = pArea->lpIndexes, pIndexSeek = nullptr;
    int i = 0;

    if (hb_itemGetCLen(pInfo->atomBagName) > 0) {
      pIndexSeek = hb_ntxFindBag(pArea, pInfo->atomBagName->getCPtr());
    } else if (pArea->lpCurTag) {
      pIndexSeek = pArea->lpCurTag->pIndex;
    }

    if (pIndexSeek) {
      ++i;
      do {
        if (pIndex == pIndexSeek) {
          break;
        }
        i += pIndex->iTags;
        pIndex = pIndex->pNext;
      } while (pIndex);
    }
    pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, pIndex ? i : 0);
    return Harbour::SUCCESS;
  }
  case DBOI_RESETPOS:
    return Harbour::SUCCESS;
  }

  if (SELF_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE) {
    return Harbour::FAILURE;
  }

  pTag = hb_ntxFindTag(pArea, pInfo->itmOrder, pInfo->atomBagName);

  if (pTag != nullptr) {
    switch (uiIndex) {
    case DBOI_CONDITION:
      pInfo->itmResult = hb_itemPutC(pInfo->itmResult, pTag->ForExpr ? pTag->ForExpr : nullptr);
      if (hb_itemType(pInfo->itmNewVal) & Harbour::Item::STRING) {
        auto szForExpr = pInfo->itmNewVal->getCPtr();
        if (pTag->ForExpr ? strncmp(pTag->ForExpr, szForExpr, NTX_MAX_EXP) != 0 : *szForExpr) {
          PHB_ITEM pForItem = nullptr;
          bool fOK = *szForExpr == 0;
          if (!fOK) {
            if (SELF_COMPILE(&pArea->dbfarea.area, szForExpr) == Harbour::SUCCESS) {
              pForItem = pArea->dbfarea.area.valResult;
              pArea->dbfarea.area.valResult = nullptr;
              if (SELF_EVALBLOCK(&pArea->dbfarea.area, pForItem) == Harbour::SUCCESS) {
                fOK = hb_itemType(pArea->dbfarea.area.valResult) & Harbour::Item::LOGICAL;
                hb_itemRelease(pArea->dbfarea.area.valResult);
                pArea->dbfarea.area.valResult = nullptr;
              }
            }
          }
          if (fOK && hb_ntxTagLockWrite(pTag)) {
            if (pTag->ForExpr) {
              hb_xfree(pTag->ForExpr);
            }
            if (pTag->pForItem) {
              hb_vmDestroyBlockOrMacro(pTag->pForItem);
            }
            if (pForItem) {
              pTag->ForExpr = hb_strndup(szForExpr, NTX_MAX_EXP);
              pTag->pForItem = pForItem;
              pForItem = nullptr;
            } else {
              pTag->ForExpr = nullptr;
              pTag->pForItem = nullptr;
            }
            pTag->Partial = true;
            pTag->HdrChanged = true;
            pTag->pIndex->Update = true;
            hb_ntxTagUnLockWrite(pTag);
          }
          if (pForItem) {
            hb_vmDestroyBlockOrMacro(pForItem);
          }
        }
      }
      break;
    case DBOI_EXPRESSION:
      pInfo->itmResult = hb_itemPutC(pInfo->itmResult, pTag->KeyExpr);
      break;
    case DBOI_BAGNAME: {
      PHB_FNAME pFileName = hb_fsFNameSplit(pTag->pIndex->IndexName);
      pInfo->itmResult = hb_itemPutC(pInfo->itmResult, pFileName->szName);
      hb_xfree(pFileName);
      break;
    }
    case DBOI_NAME:
      pInfo->itmResult = hb_itemPutC(pInfo->itmResult, pTag->TagName);
      break;
    case DBOI_NUMBER:
      pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, hb_ntxFindTagNum(pArea, pTag));
      break;
    case DBOI_FILEHANDLE:
      pInfo->itmResult =
          hb_itemPutNInt(pInfo->itmResult, static_cast<HB_NHANDLE>(hb_fileHandle(pTag->pIndex->DiskFile)));
      break;
    case DBOI_FULLPATH:
      pInfo->itmResult = hb_itemPutC(pInfo->itmResult, pTag->pIndex->IndexName);
      break;
    case DBOI_KEYCOUNT:
    case DBOI_KEYCOUNTRAW:
      pInfo->itmResult = hb_itemPutNL(pInfo->itmResult, hb_ntxOrdKeyCount(pTag));
      break;
    case DBOI_POSITION:
    case DBOI_KEYNORAW:
      /* case DBOI_RECNO: */
      if (hb_itemType(pInfo->itmNewVal) & Harbour::Item::NUMERIC) {
        pInfo->itmResult = hb_itemPutL(pInfo->itmResult, hb_ntxOrdKeyGoto(pTag, pInfo->itmNewVal->getNL()));
      } else {
        pInfo->itmResult = hb_itemPutNL(pInfo->itmResult, hb_ntxOrdKeyNo(pTag));
      }
      break;
    case DBOI_RELKEYPOS:
      if (hb_itemType(pInfo->itmNewVal) & Harbour::Item::NUMERIC) {
        hb_ntxOrdSetRelKeyPos(pTag, pInfo->itmNewVal->getND());
      } else {
        pInfo->itmResult = hb_itemPutND(pInfo->itmResult, hb_ntxOrdGetRelKeyPos(pTag));
      }
      break;
    case DBOI_ISCOND:
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->ForExpr != nullptr);
      break;
    case DBOI_ISDESC:
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->fUsrDescend);
      if (hb_itemType(pInfo->itmNewVal) & Harbour::Item::LOGICAL) {
        pTag->fUsrDescend = pInfo->itmNewVal->getL();
      }
      break;
    case DBOI_UNIQUE:
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->UniqueKey);
      break;
    case DBOI_CUSTOM:
      if (!pTag->Template && hb_itemType(pInfo->itmNewVal) & Harbour::Item::LOGICAL) {
        bool fNewVal = pInfo->itmNewVal->getL();
        if (pTag->Custom ? !fNewVal : fNewVal) {
          if (hb_ntxTagLockWrite(pTag)) {
            if (!pTag->Template && (pTag->Custom ? !fNewVal : fNewVal)) {
              pTag->Custom = fNewVal;
              pTag->Partial = true;
              pTag->ChgOnly = false;
              pTag->HdrChanged = true;
            }
            hb_ntxTagUnLockWrite(pTag);
          }
        }
      }
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->Custom);
      break;
    case DBOI_CHGONLY:
      if (!pTag->Custom && hb_itemType(pInfo->itmNewVal) & Harbour::Item::LOGICAL) {
        bool fNewVal = pInfo->itmNewVal->getL();
        if (pTag->ChgOnly ? !fNewVal : fNewVal) {
          if (hb_ntxTagLockWrite(pTag)) {
            if (!pTag->Custom && (pTag->ChgOnly ? !fNewVal : fNewVal)) {
              pTag->ChgOnly = fNewVal;
              pTag->Partial = true;
              pTag->HdrChanged = true;
            }
            hb_ntxTagUnLockWrite(pTag);
          }
        }
      }
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->ChgOnly);
      break;
    case DBOI_TEMPLATE:
      if (pTag->Custom && !pTag->Template && hb_itemGetL(pInfo->itmNewVal)) {
        if (hb_ntxTagLockWrite(pTag)) {
          if (pTag->Custom && !pTag->Template) {
            pTag->Template = true;
            pTag->HdrChanged = true;
          }
          hb_ntxTagUnLockWrite(pTag);
        }
      }
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->Template);
      break;
    case DBOI_MULTIKEY:
      if (pTag->Custom && !pTag->MultiKey && hb_itemGetL(pInfo->itmNewVal)) {
        if (hb_ntxTagLockWrite(pTag)) {
          if (pTag->Custom && !pTag->MultiKey) {
            pTag->MultiKey = true;
            pTag->HdrChanged = true;
          }
          hb_ntxTagUnLockWrite(pTag);
        }
      }
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->MultiKey);
      break;
    case DBOI_PARTIAL:
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->Partial);
      break;
    case DBOI_SCOPETOP:
      if (pInfo->itmResult) {
        hb_ntxTagGetScope(pTag, 0, pInfo->itmResult);
      }
      if (pInfo->itmNewVal) {
        hb_ntxTagSetScope(pTag, 0, pInfo->itmNewVal);
      }
      break;
    case DBOI_SCOPEBOTTOM:
      if (pInfo->itmResult) {
        hb_ntxTagGetScope(pTag, 1, pInfo->itmResult);
      }
      if (pInfo->itmNewVal) {
        hb_ntxTagSetScope(pTag, 1, pInfo->itmNewVal);
      }
      break;
    case DBOI_SCOPESET:
      if (pInfo->itmNewVal) {
        hb_ntxTagSetScope(pTag, 0, pInfo->itmNewVal);
        hb_ntxTagSetScope(pTag, 1, pInfo->itmNewVal);
      }
      if (pInfo->itmResult) {
        hb_itemClear(pInfo->itmResult);
      }
      break;
    case DBOI_SCOPETOPCLEAR:
      if (pInfo->itmResult) {
        hb_ntxTagGetScope(pTag, 0, pInfo->itmResult);
      }
      hb_ntxTagClearScope(pTag, 0);
      break;
    case DBOI_SCOPEBOTTOMCLEAR:
      if (pInfo->itmResult) {
        hb_ntxTagGetScope(pTag, 1, pInfo->itmResult);
      }
      hb_ntxTagClearScope(pTag, 1);
      break;
    case DBOI_SCOPECLEAR:
      hb_ntxTagClearScope(pTag, 0);
      hb_ntxTagClearScope(pTag, 1);
      if (pInfo->itmResult) {
        hb_itemClear(pInfo->itmResult);
      }
      break;
    case DBOI_KEYADD:
      if (pTag->pIndex->fReadonly) {
        hb_ntxErrorRT(pArea, EG_READONLY, EDBF_READONLY, pTag->pIndex->IndexName, 0, 0, nullptr);
        return Harbour::FAILURE;
      }
      if (pTag->Custom) {
        pInfo->itmResult = hb_itemPutL(pInfo->itmResult, hb_ntxOrdKeyAdd(pTag, pInfo->itmNewVal));
      } else {
        hb_ntxErrorRT(pArea, 0, EDBF_NOTCUSTOM, nullptr, 0, 0, nullptr);
        return Harbour::FAILURE;
      }
      break;
    case DBOI_KEYDELETE:
      if (pTag->pIndex->fReadonly) {
        hb_ntxErrorRT(pArea, EG_READONLY, EDBF_READONLY, pTag->pIndex->IndexName, 0, 0, nullptr);
        return Harbour::FAILURE;
      }
      if (pTag->Custom) {
        pInfo->itmResult = hb_itemPutL(pInfo->itmResult, hb_ntxOrdKeyDel(pTag, pInfo->itmNewVal));
      } else {
        hb_ntxErrorRT(pArea, 0, EDBF_NOTCUSTOM, nullptr, 0, 0, nullptr);
        return Harbour::FAILURE;
      }
      break;
    case DBOI_KEYTYPE: {
      char szType[2];
      szType[0] = static_cast<char>(pTag->KeyType);
      szType[1] = 0;
      pInfo->itmResult = hb_itemPutC(pInfo->itmResult, szType);
    } break;
    case DBOI_KEYSIZE:
      pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, pTag->KeyLength);
      break;
    case DBOI_KEYDEC:
      pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, pTag->KeyDec);
      break;
    case DBOI_KEYVAL:
      if (hb_ntxTagLockRead(pTag)) {
        if (hb_ntxCurKeyRefresh(pTag)) {
          pInfo->itmResult = hb_ntxKeyGetItem(pInfo->itmResult, pTag->CurKeyInfo, pTag, true);
        } else if (pInfo->itmResult) {
          hb_itemClear(pInfo->itmResult);
        }
        hb_ntxTagUnLockRead(pTag);
      }
      break;
    case DBOI_SKIPUNIQUE:
      pInfo->itmResult = hb_itemPutL(
          pInfo->itmResult,
          hb_ntxOrdSkipUnique(pTag, pInfo->itmNewVal && pInfo->itmNewVal->isNumeric() ? pInfo->itmNewVal->getNL() : 1));
      break;
    case DBOI_SKIPEVAL:
    case DBOI_SKIPEVALBACK:
      pInfo->itmResult =
          hb_itemPutL(pInfo->itmResult, hb_ntxOrdSkipEval(pTag, uiIndex == DBOI_SKIPEVAL, pInfo->itmNewVal));
      break;
    case DBOI_SKIPWILD:
    case DBOI_SKIPWILDBACK:
      pInfo->itmResult =
          hb_itemPutL(pInfo->itmResult, hb_ntxOrdSkipWild(pTag, uiIndex == DBOI_SKIPWILD, pInfo->itmNewVal));
      break;
    case DBOI_SKIPREGEX:
    case DBOI_SKIPREGEXBACK:
      pInfo->itmResult =
          hb_itemPutL(pInfo->itmResult, hb_ntxOrdSkipRegEx(pTag, uiIndex == DBOI_SKIPREGEX, pInfo->itmNewVal));
      break;
    case DBOI_FINDREC:
    case DBOI_FINDRECCONT:
      pInfo->itmResult = hb_itemPutL(
          pInfo->itmResult, hb_ntxOrdFindRec(pTag, hb_itemGetNL(pInfo->itmNewVal), uiIndex == DBOI_FINDRECCONT));
      break;
    case DBOI_SCOPEEVAL:
      if ((hb_itemType(pInfo->itmNewVal) & Harbour::Item::ARRAY) && hb_arrayLen(pInfo->itmNewVal) == DBRMI_SIZE &&
          hb_arrayGetPtr(pInfo->itmNewVal, DBRMI_FUNCTION) != nullptr) {
        pInfo->itmResult = hb_itemPutNL(
            pInfo->itmResult,
            hb_ntxOrdScopeEval(
                pTag, reinterpret_cast<HB_EVALSCOPE_FUNC>(hb_arrayGetPtr(pInfo->itmNewVal, DBRMI_FUNCTION)),
                hb_arrayGetPtr(pInfo->itmNewVal, DBRMI_PARAM), hb_arrayGetItemPtr(pInfo->itmNewVal, DBRMI_LOVAL),
                hb_arrayGetItemPtr(pInfo->itmNewVal, DBRMI_HIVAL)));
      } else {
        pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, 0);
      }
      break;
    case DBOI_UPDATECOUNTER:
      /* refresh update counter */
      if (hb_ntxIndexLockRead(pTag->pIndex)) {
        hb_ntxIndexUnLockRead(pTag->pIndex);
      }
      pInfo->itmResult = hb_itemPutNInt(pInfo->itmResult, pTag->pIndex->Version);
      break;
    case DBOI_READLOCK:
      if (hb_itemType(pInfo->itmNewVal) & Harbour::Item::LOGICAL) {
        pInfo->itmResult =
            hb_itemPutL(pInfo->itmResult, pInfo->itmNewVal->getL() ? hb_ntxIndexLockRead(pTag->pIndex)
                                                                   : hb_ntxIndexUnLockRead(pTag->pIndex));
      } else {
        pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->pIndex->lockRead > 0);
      }
      break;
    case DBOI_WRITELOCK:
      if (hb_itemType(pInfo->itmNewVal) & Harbour::Item::LOGICAL) {
        pInfo->itmResult =
            hb_itemPutL(pInfo->itmResult, pInfo->itmNewVal->getL() ? hb_ntxIndexLockWrite(pTag->pIndex, true)
                                                                   : hb_ntxIndexUnLockWrite(pTag->pIndex));
      } else {
        pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->pIndex->lockWrite > 0);
      }
      break;
    case DBOI_ISSORTRECNO:
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->fSortRec);
      break;
    case DBOI_ISMULTITAG:
#if defined(HB_NTX_NOMULTITAG)
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, false);
#else
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->pIndex->Compound);
#endif
      break;
    case DBOI_LARGEFILE:
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->pIndex->LargeFile);
      break;
    case DBOI_SHARED:
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->pIndex->fShared);
      if (hb_itemType(pInfo->itmNewVal) & Harbour::Item::LOGICAL) {
        pTag->pIndex->fShared = pInfo->itmNewVal->getL();
      }
      break;
    case DBOI_ISREADONLY:
      pInfo->itmResult = hb_itemPutL(pInfo->itmResult, pTag->pIndex->fReadonly);
      break;
    case DBOI_INDEXTYPE:
#if defined(HB_NTX_NOMULTITAG)
      pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, DBOI_TYPE_NONCOMPACT);
#else
      pInfo->itmResult =
          hb_itemPutNI(pInfo->itmResult, pTag->pIndex->Compound ? DBOI_TYPE_COMPOUND : DBOI_TYPE_NONCOMPACT);
#endif
      break;
    case DBOI_INDEXPAGESIZE:
      pInfo->itmResult = hb_itemPutNI(pInfo->itmResult, NTXBLOCKSIZE);
      break;
    }
  } else if (pInfo->itmResult) {
    switch (uiIndex) {
    case DBOI_KEYCOUNT:
    case DBOI_KEYCOUNTRAW: {
      HB_ULONG ulRecCount = 0;
      SELF_RECCOUNT(&pArea->dbfarea.area, &ulRecCount);
      hb_itemPutNInt(pInfo->itmResult, ulRecCount);
      break;
    }
    case DBOI_POSITION:
    case DBOI_KEYNORAW:
      /* case DBOI_RECNO: */
      if (pInfo->itmNewVal && hb_itemType(pInfo->itmNewVal) & Harbour::Item::NUMERIC) {
        hb_itemPutL(pInfo->itmResult, SELF_GOTO(&pArea->dbfarea.area, pInfo->itmNewVal->getNL()) == Harbour::SUCCESS);
      } else {
        SELF_RECID(&pArea->dbfarea.area, pInfo->itmResult);
      }
      break;
    case DBOI_RELKEYPOS:
      if (hb_itemType(pInfo->itmNewVal) & Harbour::Item::NUMERIC) {
        auto dPos = pInfo->itmNewVal->getND();
        LPTAGINFO pSavedTag = pArea->lpCurTag;
        pArea->lpCurTag = nullptr;
        if (dPos >= 1.0) {
          SELF_GOBOTTOM(&pArea->dbfarea.area);
        } else if (dPos <= 0.0) {
          SELF_GOTOP(&pArea->dbfarea.area);
        } else {
          HB_ULONG ulRecCount, ulRecNo;
          SELF_RECCOUNT(&pArea->dbfarea.area, &ulRecCount);
          ulRecNo = static_cast<HB_ULONG>(dPos) * ulRecCount + 1;
          if (ulRecNo >= ulRecCount) {
            ulRecNo = ulRecCount;
          }
          if (SELF_GOTO(&pArea->dbfarea.area, ulRecNo) == Harbour::SUCCESS &&
              SELF_SKIPFILTER(&pArea->dbfarea.area, 1) == Harbour::SUCCESS && pArea->dbfarea.area.fEof) {
            SELF_GOTOP(&pArea->dbfarea.area);
          }
        }
        pArea->lpCurTag = pSavedTag;
      } else {
        HB_ULONG ulRecNo = 0, ulRecCount = 0;
        double dPos = 0.0;
        /* resolve any pending relations */
        if (SELF_RECNO(&pArea->dbfarea.area, &ulRecNo) == Harbour::SUCCESS) {
          if (!pArea->dbfarea.fPositioned) {
            if (ulRecNo > 1) {
              dPos = 1.0;
            }
          } else {
            SELF_RECCOUNT(&pArea->dbfarea.area, &ulRecCount);
            if (ulRecCount != 0) {
              dPos = (0.5 + ulRecNo) / ulRecCount;
            }
          }
        }
        hb_itemPutND(pInfo->itmResult, dPos);
      }
      break;
    case DBOI_SKIPUNIQUE:
      hb_itemPutL(pInfo->itmResult, SELF_SKIP(&pArea->dbfarea.area, pInfo->itmNewVal && pInfo->itmNewVal->isNumeric()
                                                                        ? pInfo->itmNewVal->getNL()
                                                                        : 1) == Harbour::SUCCESS);
      break;
    case DBOI_SKIPEVAL:
    case DBOI_SKIPEVALBACK:
    case DBOI_SKIPWILD:
    case DBOI_SKIPWILDBACK:
    case DBOI_SKIPREGEX:
    case DBOI_SKIPREGEXBACK:
    case DBOI_FINDREC:
    case DBOI_FINDRECCONT:
      SELF_GOTO(&pArea->dbfarea.area, 0);
      hb_itemPutL(pInfo->itmResult, false);
      break;
    case DBOI_ISCOND:
    case DBOI_ISDESC:
    case DBOI_UNIQUE:
    case DBOI_CUSTOM:
    case DBOI_KEYADD:
    case DBOI_KEYDELETE:

    case DBOI_ISSORTRECNO:
    case DBOI_ISMULTITAG:
    case DBOI_LARGEFILE:
    case DBOI_TEMPLATE:
    case DBOI_MULTIKEY:
    case DBOI_PARTIAL:
    case DBOI_CHGONLY:
    case DBOI_SHARED:
    case DBOI_ISREADONLY:
    case DBOI_WRITELOCK:
    case DBOI_READLOCK:
      hb_itemPutL(pInfo->itmResult, false);
      break;
    case DBOI_KEYVAL:
    case DBOI_SCOPETOP:
    case DBOI_SCOPEBOTTOM:
    case DBOI_SCOPESET:
    case DBOI_SCOPETOPCLEAR:
    case DBOI_SCOPEBOTTOMCLEAR:
    case DBOI_SCOPECLEAR:
      hb_itemClear(pInfo->itmResult);
      break;
    case DBOI_KEYSIZE:
    case DBOI_KEYDEC:
    case DBOI_NUMBER:
    case DBOI_ORDERCOUNT:
    case DBOI_SCOPEEVAL:
    case DBOI_UPDATECOUNTER:
    case DBOI_INDEXPAGESIZE:
      hb_itemPutNI(pInfo->itmResult, 0);
      break;
    case DBOI_FILEHANDLE:
      hb_itemPutNInt(pInfo->itmResult, static_cast<HB_NHANDLE>(FS_ERROR));
      break;
    case DBOI_INDEXTYPE:
      hb_itemPutNI(pInfo->itmResult, DBOI_TYPE_UNDEF);
      break;
    case DBOI_BAGNAME:
    case DBOI_CONDITION:
    case DBOI_EXPRESSION:
    case DBOI_FULLPATH:
    case DBOI_NAME:
    case DBOI_KEYTYPE:
      hb_itemPutC(pInfo->itmResult, nullptr);
      break;
    default:
      hb_itemClear(pInfo->itmResult);
    }
  }
  return Harbour::SUCCESS;
}

static HB_ERRCODE hb_ntxOrderListAdd(NTXAREAP pArea, LPDBORDERINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxOrderListAdd(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOrderInfo)));
#endif

  PHB_FILE pFile;
  char szFileName[HB_PATH_MAX], szTagName[NTX_MAX_TAGNAME + 1];
  LPNTXINDEX pIndex;
  auto fProd = false;

  HB_ERRCODE errCode = SELF_GOCOLD(&pArea->dbfarea.area);
  if (errCode != Harbour::SUCCESS) {
    return errCode;
  }

  if (hb_itemGetCLen(pOrderInfo->atomBagName) == 0) {
    return Harbour::FAILURE;
  }

  hb_ntxCreateFName(pArea, hb_itemGetCPtr(pOrderInfo->atomBagName), &fProd, szFileName, szTagName);

#if 0
   if( !szTagName[0] ) {
      return Harbour::FAILURE;
   }
#endif

  pIndex = hb_ntxFindBag(pArea, szFileName);

  if (!pIndex) {
    PHB_ITEM pError = nullptr;
    LPNTXINDEX *pIndexPtr;
    auto fRetry = false;
    auto fReadonly = false;
    auto fShared = false;

    fReadonly = pArea->dbfarea.fReadonly;
    fShared = pArea->dbfarea.fShared;
    do {
      fRetry = false;
      pFile = hb_fileExtOpen(szFileName, nullptr,
                             (fReadonly ? FO_READ : FO_READWRITE) | (fShared ? FO_DENYNONE : FO_EXCLUSIVE) |
                                 FXO_DEFAULTS | FXO_SHARELOCK | FXO_COPYNAME | FXO_NOSEEKPOS,
                             nullptr, pError);
      if (!pFile) {
        fRetry = hb_ntxErrorRT(pArea, EG_OPEN, EDBF_OPEN_INDEX, szFileName, hb_fsError(), EF_CANRETRY | EF_CANDEFAULT,
                               &pError) == E_RETRY;
      }
    } while (fRetry);

    if (pError) {
      hb_errRelease(pError);
    }

    if (!pFile) {
      return Harbour::FAILURE;
    }

    pIndex = hb_ntxIndexNew(pArea);
    pIndex->IndexName = hb_strdup(szFileName);
    pIndex->fReadonly = fReadonly;
    pIndex->fShared = fShared;
    pIndex->DiskFile = pFile;
    pIndex->Production = fProd;

    pIndexPtr = &pArea->lpIndexes;
    while (*pIndexPtr) {
      pIndexPtr = &(*pIndexPtr)->pNext;
    }
    *pIndexPtr = pIndex;

    if (hb_ntxIndexLockRead(pIndex)) {
      errCode = hb_ntxIndexLoad(pIndex, szTagName);
      hb_ntxIndexUnLockRead(pIndex);
    } else {
      errCode = Harbour::FAILURE;
    }

    if (errCode != Harbour::SUCCESS) {
      *pIndexPtr = pIndex->pNext;
      hb_ntxIndexFree(pIndex);
      hb_ntxErrorRT(pArea, EG_CORRUPTION, EDBF_CORRUPT, szFileName, 0, 0, nullptr);
      return errCode;
    }
  }

  if (!pArea->lpCurTag && pIndex->iTags) {
    pArea->lpCurTag = pIndex->lpTags[0];
    errCode = SELF_GOTOP(&pArea->dbfarea.area);
  }
  return errCode;
}

static HB_ERRCODE hb_ntxOrderListClear(NTXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxOrderListClear(%p)", static_cast<void*>(pArea)));
#endif

  LPNTXINDEX *pIndexPtr, pIndex;

  if (SELF_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE) {
    return Harbour::FAILURE;
  }

  pArea->lpCurTag = nullptr;
  pIndexPtr = &pArea->lpIndexes;
  while (*pIndexPtr) {
    pIndex = *pIndexPtr;
    if (DBFAREA_DATA(&pArea->dbfarea)->fStruct && pIndex->Production &&
        (DBFAREA_DATA(&pArea->dbfarea)->fStrictStruct ? pArea->dbfarea.fHasTags : hb_setGetAutOpen())) {
      pIndexPtr = &pIndex->pNext;
    } else {
      *pIndexPtr = pIndex->pNext;
      hb_ntxIndexFree(pIndex);
    }
  }
  return Harbour::SUCCESS;
}

static HB_ERRCODE hb_ntxOrderListDelete(NTXAREAP pArea, LPDBORDERINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxOrderListDelete(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOrderInfo)));
#endif

  char szTagName[NTX_MAX_TAGNAME + 1];
  char szFileName[HB_PATH_MAX];
  LPNTXINDEX pIndex;
  auto fProd = false;

  if (SELF_GOCOLD(&pArea->dbfarea.area) == Harbour::FAILURE) {
    return Harbour::FAILURE;
  }

  hb_ntxCreateFName(pArea, hb_itemGetCPtr(pOrderInfo->atomBagName), &fProd, szFileName, szTagName);
  pIndex = hb_ntxFindBag(pArea, szFileName);

  if (pIndex && !(pIndex->Production && DBFAREA_DATA(&pArea->dbfarea)->fStruct &&
                  (DBFAREA_DATA(&pArea->dbfarea)->fStrictStruct ? pArea->dbfarea.fHasTags : hb_setGetAutOpen()))) {
    LPNTXINDEX *pIndexPtr = &pArea->lpIndexes;
    while (*pIndexPtr) {
      if (pIndex == *pIndexPtr) {
        *pIndexPtr = pIndex->pNext;
        hb_ntxIndexFree(pIndex);
        break;
      }
      pIndexPtr = &(*pIndexPtr)->pNext;
    }
  }
  return Harbour::SUCCESS;
}

static HB_ERRCODE hb_ntxOrderListFocus(NTXAREAP pArea, LPDBORDERINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxOrderListFocus(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOrderInfo)));
#endif

  pOrderInfo->itmResult = hb_itemPutC(pOrderInfo->itmResult, pArea->lpCurTag ? pArea->lpCurTag->TagName : nullptr);

  if (pOrderInfo->itmOrder) {
    LPTAGINFO pTag = hb_ntxFindTag(pArea, pOrderInfo->itmOrder, pOrderInfo->atomBagName);
    /*
     * In Clipper tag is not changed when bad name is given in DBFNTX
     * but not in DBFCDX. I'd like to keep the same behavior in
     * [x]Harbour RDDs and I chosen DBFCDX one as default. [druzus]
     */
#ifdef HB_CLP_STRICT
    if (pTag || (pOrderInfo->itmOrder->isNumeric() && pOrderInfo->itmOrder->getNI() == 0) ||
        (pOrderInfo->itmOrder->isString() && pOrderInfo->itmOrder->getCLen() == 0))
#endif
      pArea->lpCurTag = pTag;
  }

  return Harbour::SUCCESS;
}

static HB_ERRCODE hb_ntxOrderListRebuild(NTXAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxOrderListRebuild(%p)", static_cast<void*>(pArea)));
#endif

  LPTAGINFO pCurrTag;
  LPNTXINDEX pIndex;

  HB_ERRCODE errCode = SELF_GOCOLD(&pArea->dbfarea.area);
  if (errCode != Harbour::SUCCESS) {
    return errCode;
  }

  if (pArea->dbfarea.fShared) {
    hb_ntxErrorRT(pArea, EG_SHARED, EDBF_SHARED, pArea->dbfarea.szDataFileName, 0, 0, nullptr);
    return Harbour::FAILURE;
  }
  if (pArea->dbfarea.fReadonly) {
    hb_ntxErrorRT(pArea, EG_READONLY, EDBF_READONLY, pArea->dbfarea.szDataFileName, 0, 0, nullptr);
    return Harbour::FAILURE;
  }

  if (pArea->dbfarea.lpdbPendingRel) {
    errCode = SELF_FORCEREL(&pArea->dbfarea.area);
    if (errCode != Harbour::SUCCESS) {
      return errCode;
    }
  }
  pCurrTag = pArea->lpCurTag;
  pArea->lpCurTag = nullptr;
  pIndex = pArea->lpIndexes;
  while (pIndex && errCode == Harbour::SUCCESS) {
    errCode = hb_ntxReIndex(pIndex);
    pIndex = pIndex->pNext;
  }
  if (errCode == Harbour::SUCCESS) {
    pArea->lpCurTag = pCurrTag;
    errCode = SELF_GOTOP(&pArea->dbfarea.area);
  }
  return errCode;
}

#define hb_ntxClearFilter nullptr
#define hb_ntxClearLocate nullptr
#define hb_ntxClearScope nullptr

static HB_ERRCODE hb_ntxCountScope(NTXAREAP pArea, void *pPtr, HB_LONG *plRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxCountScope(%p, %p, %p)", static_cast<void*>(pArea), pPtr, static_cast<void*>(plRecNo)));
#endif

  if (pPtr == nullptr) {
    return Harbour::SUCCESS;
  }
  return SUPER_COUNTSCOPE(&pArea->dbfarea.area, pPtr, plRecNo);
}

#define hb_ntxFilterText nullptr
#define hb_ntxScopeInfo nullptr
#define hb_ntxSetFilter nullptr
#define hb_ntxSetLocate nullptr
#define hb_ntxSetScope nullptr
#define hb_ntxSkipScope nullptr
#define hb_ntxLocate nullptr
#define hb_ntxCompile nullptr
#define hb_ntxError nullptr
#define hb_ntxEvalBlock nullptr
#define hb_ntxRawLock nullptr
#define hb_ntxLock nullptr
#define hb_ntxUnLock nullptr
#define hb_ntxCloseMemFile nullptr
#define hb_ntxCreateMemFile nullptr
#define hb_ntxGetValueFile nullptr
#define hb_ntxOpenMemFile nullptr
#define hb_ntxPutValueFile nullptr
#define hb_ntxReadDBHeader nullptr
#define hb_ntxWriteDBHeader nullptr

#define hb_ntxInit nullptr
#define hb_ntxExit nullptr
#define hb_ntxDrop nullptr
#define hb_ntxExists nullptr
#define hb_ntxRename nullptr

static HB_ERRCODE hb_ntxRddInfo(LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ntxRddInfo(%p, %hu, %lu, %p)", static_cast<void*>(pRDD), uiIndex, ulConnect, static_cast<void*>(pItem)));
#endif

  LPDBFDATA pData;

  pData = DBFNODE_DATA(pRDD);

  if (pData->bMemoType == 0) {
    pData->bMemoType = DB_MEMO_DBT;
#if !defined(HB_NTX_NOMULTITAG)
    pData->fMultiTag = true;
#endif
  }

  switch (uiIndex) {
  case RDDI_ORDBAGEXT:
  case RDDI_ORDEREXT:
  case RDDI_ORDSTRUCTEXT: {
    auto szNew = hb_itemGetCPtr(pItem);
    char *szNewVal;

    szNewVal = szNew[0] == '.' && szNew[1] ? hb_strdup(szNew) : nullptr;
    hb_itemPutC(pItem, pData->szIndexExt[0] ? pData->szIndexExt : NTX_INDEXEXT);
    if (szNewVal != nullptr) {
      hb_strncpy(pData->szIndexExt, szNewVal, sizeof(pData->szIndexExt) - 1);
      hb_xfree(szNewVal);
    }
    break;
  }

  case RDDI_MULTITAG: {
#if defined(HB_NTX_NOMULTITAG)
    hb_itemPutL(pItem, false);
#else
    bool fMultiTag = pData->fMultiTag;
    if (hb_itemType(pItem) & Harbour::Item::LOGICAL) {
      pData->fMultiTag = pItem->getL();
    }
    hb_itemPutL(pItem, fMultiTag);
#endif
    break;
  }

  case RDDI_SORTRECNO: {
    bool fSortRecNo = pData->fSortRecNo;
    if (hb_itemType(pItem) & Harbour::Item::LOGICAL) {
      pData->fSortRecNo = pItem->getL();
    }
    hb_itemPutL(pItem, fSortRecNo);
    break;
  }

  case RDDI_STRUCTORD: {
    bool fStruct = pData->fStruct;
    if (hb_itemType(pItem) & Harbour::Item::LOGICAL) {
      pData->fStruct = pItem->getL();
    }
    hb_itemPutL(pItem, fStruct);
    break;
  }

  case RDDI_STRICTSTRUCT: {
    bool fStrictStruct = pData->fStrictStruct;
    if (hb_itemType(pItem) & Harbour::Item::LOGICAL) {
      pData->fStrictStruct = pItem->getL();
    }
    hb_itemPutL(pItem, fStrictStruct);
    break;
  }

  case RDDI_MULTIKEY: {
    bool fMultiKey = pData->fMultiKey;
    if (hb_itemType(pItem) & Harbour::Item::LOGICAL) {
      pData->fMultiKey = pItem->getL();
    }
    hb_itemPutL(pItem, fMultiKey);
    break;
  }

  default:
    return SUPER_RDDINFO(pRDD, uiIndex, ulConnect, pItem);
  }

  return Harbour::SUCCESS;
}

#define hb_ntxWhoCares nullptr

static const RDDFUNCS ntxTable = {hb_ntxBof,
                                  hb_ntxEof,
                                  hb_ntxFound,
                                  (DBENTRYP_V)hb_ntxGoBottom,
                                  hb_ntxGoTo,
                                  hb_ntxGoToId,
                                  (DBENTRYP_V)hb_ntxGoTop,
                                  (DBENTRYP_BIB)hb_ntxSeek,
                                  hb_ntxSkip,
                                  hb_ntxSkipFilter,
                                  (DBENTRYP_L)hb_ntxSkipRaw,
                                  hb_ntxAddField,
                                  (DBENTRYP_B)hb_ntxAppend,
                                  hb_ntxCreateFields,
                                  hb_ntxDeleteRec,
                                  hb_ntxDeleted,
                                  hb_ntxFieldCount,
                                  hb_ntxFieldDisplay,
                                  hb_ntxFieldInfo,
                                  hb_ntxFieldName,
                                  (DBENTRYP_V)hb_ntxFlush,
                                  hb_ntxGetRec,
                                  hb_ntxGetValue,
                                  hb_ntxGetVarLen,
                                  (DBENTRYP_V)hb_ntxGoCold,
                                  (DBENTRYP_V)hb_ntxGoHot,
                                  hb_ntxPutRec,
                                  hb_ntxPutValue,
                                  hb_ntxRecall,
                                  hb_ntxRecCount,
                                  hb_ntxRecInfo,
                                  hb_ntxRecNo,
                                  hb_ntxRecId,
                                  hb_ntxSetFieldsExtent,
                                  hb_ntxAlias,
                                  (DBENTRYP_V)hb_ntxClose,
                                  hb_ntxCreate,
                                  hb_ntxInfo,
                                  hb_ntxNewArea,
                                  (DBENTRYP_VO)hb_ntxOpen,
                                  hb_ntxRelease,
                                  (DBENTRYP_SP)hb_ntxStructSize,
                                  hb_ntxSysName,
                                  hb_ntxEval,
                                  (DBENTRYP_V)hb_ntxPack,
                                  ntPackRec,
                                  hb_ntxSort,
                                  hb_ntxTrans,
                                  hb_ntxTransRec,
                                  (DBENTRYP_V)hb_ntxZap,
                                  hb_ntxchildEnd,
                                  hb_ntxchildStart,
                                  hb_ntxchildSync,
                                  hb_ntxsyncChildren,
                                  hb_ntxclearRel,
                                  hb_ntxforceRel,
                                  hb_ntxrelArea,
                                  hb_ntxrelEval,
                                  hb_ntxrelText,
                                  hb_ntxsetRel,
                                  (DBENTRYP_VOI)hb_ntxOrderListAdd,
                                  (DBENTRYP_V)hb_ntxOrderListClear,
                                  (DBENTRYP_VOI)hb_ntxOrderListDelete,
                                  (DBENTRYP_VOI)hb_ntxOrderListFocus,
                                  (DBENTRYP_V)hb_ntxOrderListRebuild,
                                  hb_ntxOrderCondition,
                                  (DBENTRYP_VOC)hb_ntxOrderCreate,
                                  (DBENTRYP_VOI)hb_ntxOrderDestroy,
                                  (DBENTRYP_SVOI)hb_ntxOrderInfo,
                                  hb_ntxClearFilter,
                                  hb_ntxClearLocate,
                                  hb_ntxClearScope,
                                  (DBENTRYP_VPLP)hb_ntxCountScope,
                                  hb_ntxFilterText,
                                  hb_ntxScopeInfo,
                                  hb_ntxSetFilter,
                                  hb_ntxSetLocate,
                                  hb_ntxSetScope,
                                  hb_ntxSkipScope,
                                  hb_ntxLocate,
                                  hb_ntxCompile,
                                  hb_ntxError,
                                  hb_ntxEvalBlock,
                                  hb_ntxRawLock,
                                  hb_ntxLock,
                                  hb_ntxUnLock,
                                  hb_ntxCloseMemFile,
                                  hb_ntxCreateMemFile,
                                  hb_ntxGetValueFile,
                                  hb_ntxOpenMemFile,
                                  hb_ntxPutValueFile,
                                  hb_ntxReadDBHeader,
                                  hb_ntxWriteDBHeader,
                                  hb_ntxInit,
                                  hb_ntxExit,
                                  hb_ntxDrop,
                                  hb_ntxExists,
                                  hb_ntxRename,
                                  hb_ntxRddInfo,
                                  hb_ntxWhoCares};

HB_FUNC_TRANSLATE(DBFNTX, _DBF)

HB_FUNC_STATIC(DBFNTX_GETFUNCTABLE)
{
  auto puiCount = static_cast<HB_USHORT *>(hb_parptr(1));
  auto pTable = static_cast<RDDFUNCS *>(hb_parptr(2));
  auto uiRddId = static_cast<HB_USHORT>(hb_parni(4));
  auto puiSuperRddId = static_cast<HB_USHORT *>(hb_parptr(5));

  if (pTable) {
    if (puiCount) {
      *puiCount = RDDFUNCSCOUNT;
    }
    HB_ERRCODE errCode = hb_rddInheritEx(pTable, &ntxTable, &ntxSuper, "DBFFPT", puiSuperRddId);
    if (errCode != Harbour::SUCCESS) {
      errCode = hb_rddInheritEx(pTable, &ntxTable, &ntxSuper, "DBFDBT", puiSuperRddId);
    }
    if (errCode != Harbour::SUCCESS) {
      errCode = hb_rddInheritEx(pTable, &ntxTable, &ntxSuper, "DBF", puiSuperRddId);
    }
    if (errCode == Harbour::SUCCESS) {
      /*
       * we successfully register our RDD so now we can initialize it
       * You may think that this place is RDD init statement, Druzus
       */
      s_uiRddId = uiRddId;
    }
    hb_retni(errCode);
  } else {
    hb_retni(Harbour::FAILURE);
  }
}

static void hb_dbfntxRddInit(void *cargo)
{
  HB_SYMBOL_UNUSED(cargo);

  if (hb_rddRegister("DBF", RDT_FULL) <= 1) {
    hb_rddRegister("DBFFPT", RDT_FULL);
    if (hb_rddRegister("DBFNTX", RDT_FULL) <= 1) {
      return;
    }
  }

  hb_errInternal(HB_EI_RDDINVALID, nullptr, nullptr, nullptr);
}

HB_INIT_SYMBOLS_BEGIN(dbfntx1__InitSymbols){"DBFNTX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME(DBFNTX)}, nullptr},
    {"DBFNTX_GETFUNCTABLE",
     {HB_FS_PUBLIC | HB_FS_LOCAL},
     {HB_FUNCNAME(DBFNTX_GETFUNCTABLE)},
     nullptr} HB_INIT_SYMBOLS_END(dbfntx1__InitSymbols)

        HB_CALL_ON_STARTUP_BEGIN(_hb_dbfntx_rdd_init_) hb_vmAtInit(hb_dbfntxRddInit, nullptr);
HB_CALL_ON_STARTUP_END(_hb_dbfntx_rdd_init_)

#if defined(HB_PRAGMA_STARTUP)
#pragma startup dbfntx1__InitSymbols
#pragma startup _hb_dbfntx_rdd_init_
#elif defined(HB_DATASEG_STARTUP)
#define HB_DATASEG_BODY                                                                                                \
  HB_DATASEG_FUNC(dbfntx1__InitSymbols)                                                                                \
  HB_DATASEG_FUNC(_hb_dbfntx_rdd_init_)
#include "hbiniseg.hpp"
#endif
