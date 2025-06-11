/*
 * Advantage Database Server RDD
 *
 * Copyright 1999 Alexander Kresin <alex@belacy.belgorod.su>
 *
 */

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

#define SUPERTABLE (&adsSuper)
#define MAX_STR_LEN 255

#include "rddads.hpp"

#include <hbvm.hpp>
#include <hbinit.hpp>
#include <hbapistr.hpp>
#include <hbapierr.hpp>
#include "hbdbferr.hpp"
#include <hbapilng.hpp>
#include <hbdate.hpp>
#include <hbset.hpp>
#include <hbstack.hpp>

#include "rddsys.ch"

static int s_iSetListenerHandle = 0;

static HB_USHORT s_uiRddCount = 0;
static HB_USHORT s_uiRddIdADS = static_cast<HB_USHORT>(-1);
static HB_USHORT s_uiRddIdADSADT = static_cast<HB_USHORT>(-1);
static HB_USHORT s_uiRddIdADSNTX = static_cast<HB_USHORT>(-1);
static HB_USHORT s_uiRddIdADSCDX = static_cast<HB_USHORT>(-1);
#if ADS_LIB_VERSION >= 900
static HB_USHORT s_uiRddIdADSVFP = static_cast<HB_USHORT>(-1);
#endif

static RDDFUNCS adsSuper;

#define ERROR_BUFFER_LEN 512

typedef struct _RDDADSDATA
{
  UNSIGNED32 ulError;
  UNSIGNED32 ulInsertID;
  UNSIGNED32 ulAffectedRows;
  UNSIGNED8 szError[ERROR_BUFFER_LEN + 1];
  char *szQuery;
} RDDADSDATA, *LPRDDADSDATA;

#define RDDADSNODE_DATA(r) (static_cast<LPRDDADSDATA>(hb_stackGetTSD(static_cast<PHB_TSD>((r)->lpvCargo))))

/*
 * -- HELPER FUNCTIONS --
 */

static void adsSetListener_callback(HB_set_enum setting, HB_set_listener_enum when)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsSetListener_callback (%d, %d)", setting, when));
#endif

  if (when == HB_SET_LISTENER_AFTER)
  { /* we don't do anything with BEFORE calls */
    switch (setting)
    {
    case HB_SET_DATEFORMAT:
      AdsSetDateFormat(reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_setGetCPtr(HB_SET_DATEFORMAT))));
      break;
    case HB_SET_DEFAULT:
      AdsSetDefault(reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_setGetCPtr(HB_SET_DEFAULT))));
      break;
    case HB_SET_DELETED:
      AdsShowDeleted(static_cast<UNSIGNED16>(!hb_setGetL(HB_SET_DELETED)));
      break;
    case HB_SET_EPOCH:
      AdsSetEpoch(static_cast<UNSIGNED16>(hb_setGetNI(HB_SET_EPOCH)));
      break;
    case HB_SET_EXACT:
      AdsSetExact(static_cast<UNSIGNED16>(hb_setGetL(HB_SET_EXACT)));
      break;
    case HB_SET_PATH:
      AdsSetSearchPath(reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_setGetCPtr(HB_SET_PATH))));
      break;
    case HB_SET_DECIMALS:
      AdsSetDecimals(static_cast<UNSIGNED16>(hb_setGetNI(HB_SET_DECIMALS)));
      break;
#if 0 /* Possible TODO? */
         case HB_SET_MFILEEXT:
            if( hb_setGetCPtr(HB_SET_MFILEEXT) ) {
               hb_retc(hb_setGetCPtr(HB_SET_MFILEEXT));
            }
            break;
         case HB_SET_STRICTREAD:
            hb_retl(hb_setGetL(HB_SET_STRICTREAD));
            break;
#endif
    default:
      break;
    }
  }
}

static void adsSetSend(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsSetSend()"));
#endif

  AdsSetDateFormat(reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_setGetCPtr(HB_SET_DATEFORMAT))));
  AdsSetDefault(reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_setGetCPtr(HB_SET_DEFAULT))));
  AdsShowDeleted(static_cast<UNSIGNED16>(!hb_setGetL(HB_SET_DELETED)));
  AdsSetEpoch(static_cast<UNSIGNED16>(hb_setGetNI(HB_SET_EPOCH)));
  AdsSetExact(static_cast<UNSIGNED16>(hb_setGetL(HB_SET_EXACT)));
  AdsSetSearchPath(reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_setGetCPtr(HB_SET_PATH))));
  AdsSetDecimals(static_cast<UNSIGNED16>(hb_setGetNI(HB_SET_DECIMALS)));
}

static HB_ERRCODE commonError(ADSAREAP pArea, HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, HB_ERRCODE errOsCode,
                              const char *szFileName, HB_USHORT uiFlags, PHB_ITEM *pErrorPtr)
{
  HB_ERRCODE errCode = Harbour::FAILURE;

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
    if (errSubCode > 1000)
    {
      UNSIGNED8 aucError[ADS_MAX_ERROR_LEN + 1] = {0};
      UNSIGNED16 usLength = ADS_MAX_ERROR_LEN + 1;
      UNSIGNED32 ulErrCode;

      AdsGetLastError(&ulErrCode, aucError, &usLength);
      if (ulErrCode != static_cast<UNSIGNED32>(errSubCode))
      {
        AdsGetErrorString(static_cast<UNSIGNED32>(errSubCode), aucError, &usLength);
      }
      hb_errPutDescription(pError, reinterpret_cast<char *>(aucError));
    }
    else
    {
      hb_errPutDescription(pError, hb_langDGetErrorDesc(errGenCode));
    }
    if (errOsCode)
    {
      hb_errPutOsCode(pError, errOsCode);
    }
    if (szFileName)
    {
      hb_errPutFileName(pError, szFileName);
    }
    if (uiFlags)
    {
      hb_errPutFlags(pError, uiFlags);
    }
    errCode = SUPER_ERROR(&pArea->area, pError);
    if (!pErrorPtr)
    {
      hb_itemRelease(pError);
    }
  }
  return errCode;
}

/*
 * it's not used - temporary disabled to avoid warnings,
 * enable it if necessary
 */
#if 0
static void DumpArea(ADSAREAP pArea)  /* For debugging: call this to dump ads server settings to HB_TRACE. Currently in a quick-and-dirty state... */
{
   UNSIGNED8  pucTemp[1025];
   UNSIGNED16 pusLen = 1024;
   UNSIGNED32 u32RetVal, ulRetAOF, ulRetFilt;
   UNSIGNED8  pucFormat[16];
   UNSIGNED8  pucFilter[1025];
#if 0
   UNSIGNED8  aucBuffer[MAX_STR_LEN + 1];
#endif
   UNSIGNED8  pucIndexName[MAX_STR_LEN + 1];
   UNSIGNED8  pucIndexExpr[MAX_STR_LEN + 1];
   UNSIGNED8  pucIndexCond[MAX_STR_LEN + 1];

   if( pArea != nullptr ) {
      pusLen = 15;
      AdsGetDateFormat(pucFormat, &pusLen);
      pusLen = 1024;
      u32RetVal = AdsGetTableAlias(pArea->hTable, pucTemp, &pusLen);
      AdsGetEpoch(&pusLen);
#if 0
      HB_TRACE(HB_TR_ALWAYS, ("DumpArea: \n    pArea: %p  hTable: %lu  Alias: %s (RetVal %lu)\n      Eof: %d  DateFormat: %s  Epoch: %d",
                              static_cast<void*>(pArea), pArea->hTable, pucTemp, u32RetVal, pArea->area.fEof, pucFormat, pusLen));
#endif

      pusLen = 1024;
      ulRetAOF = AdsGetAOF(pArea->hTable, pucTemp, &pusLen);
      pusLen = 1024;
      ulRetFilt = AdsGetFilter(pArea->hTable, pucFilter, &pusLen);
#if 0
      HB_TRACE(HB_TR_ALWAYS, ("DumpArea AOF: (RetVal %lu) %s \n     Filter: (RetVal %lu) %s", ulRetAOF, pucTemp, ulRetFilt, pucFilter));
#endif

      if( pArea->hOrdCurrent ) {
         pusLen = MAX_STR_LEN;
         AdsGetIndexName(pArea->hOrdCurrent, pucIndexName, &pusLen);
         pusLen = MAX_STR_LEN;
         AdsGetIndexCondition(pArea->hOrdCurrent, pucIndexCond, &pusLen);
         pusLen = MAX_STR_LEN;
         AdsGetIndexExpr(pArea->hOrdCurrent, pucIndexExpr, &pusLen);

         pusLen = 1024;   /* ADS top/bottom are 1,2 instead of 0,1 */
         u32RetVal = AdsGetScope(pArea->hOrdCurrent, ADS_TOP, pucTemp, &pusLen);
         pusLen = 1024;
         ulRetFilt = AdsGetScope(pArea->hOrdCurrent, ADS_BOTTOM, pucFilter, &pusLen);

#if 0
         HB_TRACE(HB_TR_ALWAYS, ("DumpArea Index: %s   Expr: %s  Cond: %s\n        Scope: (RetVal %lu) %s  Bottom: (RetVal %lu) %s",
                                 pucIndexName, pucIndexExpr, pucIndexCond, u32RetVal, pucTemp, ulRetFilt, pucFilter));
#endif
      }
   }
}
#endif

static bool adsIndexKeyCmp(ADSHANDLE hIndex, UNSIGNED8 *pszKey, UNSIGNED16 u16KeyLen)
{
  UNSIGNED32 u32RetVal;
  UNSIGNED8 pucCurKey[ADS_MAX_KEY_LENGTH + 1];
  UNSIGNED16 u16CurKeyLen = ADS_MAX_KEY_LENGTH;

  /*
   * test if current record has fields that match the given key expression.
   * This is used to evaluate if a seek expression continues to eval to .t.
   * when skipping through filtered records
   */

  u32RetVal = AdsExtractKey(hIndex, pucCurKey, &u16CurKeyLen);
  if (u32RetVal == AE_SUCCESS)
  {
    if (u16CurKeyLen)
    {
      if (u16CurKeyLen >= u16KeyLen &&
          memcmp(static_cast<UNSIGNED8 *>(pucCurKey), static_cast<UNSIGNED8 *>(pszKey), u16KeyLen) == 0)
      {
        return true;
      }
    }
  }

  return false;
}

static int adsGetRddType(HB_USHORT uiRddID)
{
  if (uiRddID == s_uiRddIdADSCDX)
  {
    return ADS_CDX;
  }
  else if (uiRddID == s_uiRddIdADSNTX)
  {
    return ADS_NTX;
  }
  else if (uiRddID == s_uiRddIdADSADT)
  {
    return ADS_ADT;
  }
#if ADS_LIB_VERSION >= 900
  else if (uiRddID == s_uiRddIdADSVFP)
  {
    return ADS_VFP;
  }
#endif
  else if (uiRddID == s_uiRddIdADS)
  {
    return ADS_DEFAULT;
  }
  else if (hb_rddIsDerivedFrom(uiRddID, s_uiRddIdADSCDX))
  {
    return ADS_CDX;
  }
  else if (hb_rddIsDerivedFrom(uiRddID, s_uiRddIdADSNTX))
  {
    return ADS_NTX;
  }
  else if (hb_rddIsDerivedFrom(uiRddID, s_uiRddIdADSADT))
  {
    return ADS_ADT;
  }
#if ADS_LIB_VERSION >= 900
  else if (hb_rddIsDerivedFrom(uiRddID, s_uiRddIdADSVFP))
  {
    return ADS_VFP;
  }
#endif
  else if (hb_rddIsDerivedFrom(uiRddID, s_uiRddIdADS))
  {
    return ADS_DEFAULT;
  }
  else
  {
    return -1;
  }
}

static int adsGetFileType(HB_USHORT uiRddID)
{
  int iType = adsGetRddType(uiRddID);

  return iType > 0 ? iType : hb_ads_iFileType;
}

static const char *adsTableExt(int iFileType)
{
  return iFileType == ADS_ADT ? ".adt" : ".dbf";
}

static const char *adsMemoExt(int iFileType)
{
  switch (iFileType)
  {
  case ADS_ADT:
    return ".adm";
  case ADS_NTX:
    return ".dbt";
  }

  return ".fpt";
}

static const char *adsIndexExt(int iFileType)
{
  switch (iFileType)
  {
  case ADS_ADT:
    return ".adi";
  case ADS_NTX:
    return ".ntx";
  }

  return ".cdx";
}

static int adsIndexPageSize(int iFileType)
{
  switch (iFileType)
  {
#if ADS_LIB_VERSION >= 900
  case ADS_VFP:
#endif
  case ADS_CDX:
    return 512;
  case ADS_NTX:
    return 1024;
  case ADS_ADT:
    return hb_ads_getIndexPageSize();
  }

  return 0;
}

static ADSHANDLE hb_adsFindBag(ADSAREAP pArea, const char *szBagName)
{
  /* This method seems to be most easy one though I'm doubt
     it's really the best one */
#if 1
  ADSHANDLE ahIndex[1];
  UNSIGNED16 u16Count = 1;
  UNSIGNED32 u32Result;

  u32Result =
      AdsOpenIndex(pArea->hTable, reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(szBagName)), ahIndex, &u16Count);
  if (u32Result == AE_INDEX_ALREADY_OPEN)
  {
    return ahIndex[0];
  }

  if (u32Result == AE_SUCCESS)
  {
    AdsCloseIndex(ahIndex[0]);
  }

  return 0;
#else
  UNSIGNED8 pucName[MAX_STR_LEN + 1];
  UNSIGNED16 u16Option, u16Count, u16len;
  ADSHANDLE hIndex = 0, hOrder;
  PHB_FNAME pFileName;

  pFileName = hb_fsFNameSplit(szBagName);
  if (pFileName->szPath)
  {
    u16Option = ADS_FULLPATHNAME;
  }
  else if (pFileName->szExtension)
  {
    u16Option = ADS_BASENAMEANDEXT;
  }
  else
  {
    u16Option = ADS_BASENAME;
  }
  hb_xfree(pFileName);

  if (AdsGetNumIndexes(pArea->hTable, &u16Count) == AE_SUCCESS)
  {
    for (UNSIGNED16 u = 1; u <= u16Count; ++u)
    {
      if (AdsGetIndexHandleByOrder(pArea->hTable, u, &hOrder) != AE_SUCCESS)
      {
        break;
      }
      u16len = MAX_STR_LEN;
      if (AdsGetIndexFilename(hOrder, u16Option, pucName, &u16len) != AE_SUCCESS)
      {
        break;
      }
      if (!hb_stricmp(szBagName, static_cast<char *>(pucName)))
      {
        hIndex = hOrder;
        break;
      }
    }
  }
  return hIndex;
#endif
}

static HB_ERRCODE hb_adsUpdateAreaFlags(ADSAREAP pArea)
{
  UNSIGNED16 u16Bof, u16Eof, u16Found;

  AdsAtBOF(pArea->hTable, &u16Bof);
  AdsAtEOF(pArea->hTable, &u16Eof);
  AdsIsFound(pArea->hTable, &u16Found);

  pArea->area.fBof = u16Bof != 0;
  pArea->area.fEof = u16Eof != 0;
  pArea->area.fFound = u16Found != 0;

  pArea->fPositioned = !pArea->area.fBof && !pArea->area.fEof;

  return Harbour::SUCCESS;
}

static HB_ERRCODE hb_adsCheckLock(ADSAREAP pArea)
{
  if (hb_ads_bTestRecLocks && pArea->fShared && !pArea->fFLocked)
  {
    UNSIGNED16 u16Locked = 0;
    UNSIGNED32 u32RetVal;

    u32RetVal = AdsIsRecordLocked(pArea->hTable, 0, &u16Locked);
    if (u32RetVal != AE_SUCCESS)
    {
      commonError(pArea, EG_UNLOCKED, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, 0, nullptr);
      return Harbour::FAILURE;
    }
    if (!u16Locked)
    {
      commonError(pArea, EG_UNLOCKED, EDBF_UNLOCKED - 900, 0, nullptr, 0, nullptr);
      return Harbour::FAILURE;
    }
  }
  return Harbour::SUCCESS;
}

static void adsGetKeyItem(ADSAREAP pArea, PHB_ITEM pItem, int iKeyType, char *pKeyBuf, int iKeyLen)
{
  double dValue;

  switch (iKeyType)
  {
  /*
     TODO: ADS_RAW only partially supported.  Presumed string.
           ADT files can use ";" concatenation operator, which returns index key types as Raw
   */
  case ADS_RAW:
    /* hack for timestamp values, we need something better to detect timestamp indexes */
    if (pArea->iFileType == ADS_ADT && pKeyBuf[0] == 0 && (iKeyLen == 8 || iKeyLen == 4))
    {
      long lDate;
      lDate = HB_GET_BE_UINT32(pKeyBuf);
      if (iKeyLen == 8)
      {
        long lTime = HB_GET_BE_UINT32(&pKeyBuf[4]);
        /* ADS stores milliseconds in raw ADT form increased by one */
        if (lTime)
        {
          --lTime;
        }
        hb_itemPutTDT(pItem, lDate, lTime);
      }
      else
      {
        hb_itemPutDL(pItem, lDate);
      }
      break;
    }
#if ADS_LIB_VERSION >= 900
    else if (pArea->iFileType == ADS_VFP && iKeyLen == 8)
    {
      HB_ORD2DBL(pKeyBuf, &dValue);
      hb_itemPutTD(pItem, dValue);
      break;
    }
#endif
    /* fallthrough */
  case ADS_STRING:
    hb_itemPutCL(pItem, pKeyBuf, iKeyLen);
    break;

  case ADS_NUMERIC:
    if (pArea->iFileType == ADS_NTX)
    {
      int iLen = iKeyLen, iDec;
      HB_MAXINT lValue;

      if (*pKeyBuf == '0' - 4)
      { /* negative number */
        char *ptr = pKeyBuf;

        while (iLen--)
        {
          if (*ptr != '.')
          {
            *ptr = '0' - (*ptr - '0' + 4);
          }
          ++ptr;
        }
        *ptr = '\0';
        pKeyBuf[0] = '-';
      }
      if (hb_valStrnToNum(pKeyBuf, iKeyLen, &lValue, &dValue, &iDec, &iLen))
      {
        hb_itemPutNDLen(pItem, dValue, iLen, iDec);
      }
      else
      {
        hb_itemPutNIntLen(pItem, lValue, iKeyLen);
      }
    }
    else
    { /* ADS_CDX, ADS_ADT */
      HB_ORD2DBL(pKeyBuf, &dValue);
      hb_itemPutND(pItem, dValue);
    }
    break;

  case ADS_DATE:
    if (pArea->iFileType == ADS_NTX)
    {
      hb_itemPutDS(pItem, pKeyBuf);
    }
    else
    { /* ADS_CDX, ADS_ADT, ADS_VFP */
      HB_ORD2DBL(pKeyBuf, &dValue);
      hb_itemPutDL(pItem, static_cast<long>(dValue));
    }
    break;

  case ADS_LOGICAL:
    hb_itemPutL(pItem, *pKeyBuf == 'T');
    break;

  default:
    hb_itemClear(pItem);
  }
}

static void adsScopeGet(ADSAREAP pArea, ADSHANDLE hOrder, HB_USHORT nScope, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsScopeGet(%p, %lu, %hu, %p)", static_cast<void*>(pArea), hOrder, nScope, static_cast<void*>(pItem)));
#endif

  UNSIGNED8 pucScope[ADS_MAX_KEY_LENGTH + 1];
  UNSIGNED16 u16Len = ADS_MAX_KEY_LENGTH;
  UNSIGNED32 u32RetVal;
  UNSIGNED16 u16KeyType = 0;

  if (hOrder)
  {
    /*ADS top/bottom are 1,2 instead of 0,1*/
    nScope = (nScope == 0) ? ADS_TOP : ADS_BOTTOM;

    u32RetVal = AdsGetScope(hOrder, static_cast<UNSIGNED16>(nScope), pucScope, &u16Len);

    if (u32RetVal == AE_SUCCESS)
    {
      AdsGetKeyType(hOrder, &u16KeyType);
      adsGetKeyItem(pArea, pItem, u16KeyType, reinterpret_cast<char *>(pucScope), u16Len);
    }
    else
    {
      hb_itemClear(pItem);
    }
  }
}

static HB_ERRCODE adsScopeSet(ADSAREAP pArea, ADSHANDLE hOrder, HB_USHORT nScope, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsScopeSet(%p, %lu, %hu, %p)", static_cast<void*>(pArea), hOrder, nScope, static_cast<void*>(pItem)));
#endif

  HB_SYMBOL_UNUSED(pArea);

  if (hOrder)
  {
    nScope = (nScope == 0) ? ADS_TOP : ADS_BOTTOM;
    if (pItem != nullptr)
    {
      UNSIGNED16 u16KeyType = 0;
      AdsGetKeyType(hOrder, &u16KeyType);

      /* make sure passed item has same type as index */
      switch (u16KeyType)
      {
      case ADS_RAW: /* ADT files need the ";" concatenation operator (instead of "+") to be optimized */
        /* ADS timestamp values */
        if (pItem->isDateTime())
        {
          if (pArea->iFileType == ADS_ADT)
          {
            UNSIGNED8 pKeyBuf[8];
            long lDate, lTime;
            hb_itemGetTDT(pItem, &lDate, &lTime);
            /* ADS stores milliseconds in raw ADT form increased by one */
            ++lTime;
            HB_PUT_BE_UINT32(pKeyBuf, lDate);
            HB_PUT_BE_UINT32(&pKeyBuf[4], lTime);
            AdsSetScope(hOrder, nScope, pKeyBuf, pItem->isTimeStamp() ? 8 : 4, ADS_RAWKEY);
            break;
          }
#if ADS_LIB_VERSION >= 900
          else if (pArea->iFileType == ADS_VFP)
          {
            double dTemp;
            dTemp = hb_itemGetTD(pItem);
            AdsSetScope(hOrder, nScope, reinterpret_cast<UNSIGNED8 *>(&dTemp), static_cast<UNSIGNED16>(sizeof(dTemp)),
                        ADS_DOUBLEKEY);
            break;
          }
#endif
        }
        /* fallthrough */
      case ADS_STRING:
        if (pItem->isString())
        {
          UNSIGNED16 u16DataType = ADS_STRINGKEY;
          auto ucLen = static_cast<UNSIGNED16>(hb_itemGetCLen(pItem));
          auto pucScope = reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(pItem)));
#if defined(ADS_USE_OEM_TRANSLATION) && ADS_LIB_VERSION < 600
          UNSIGNED8 *pszKeyFree = nullptr;
#endif
#ifdef ADS_USE_OEM_TRANSLATION
          if (hb_ads_bOEM)
          {
#if ADS_LIB_VERSION >= 600
            u16DataType = ADS_RAWKEY;
#else
            pucScope = pszKeyFree =
                static_cast<UNSIGNED8 *>(hb_adsOemToAnsi(static_cast<const char *>(pucScope), ucLen));
#endif
          }
#endif
          AdsSetScope(hOrder, nScope, pucScope, ucLen, u16DataType);
#if defined(ADS_USE_OEM_TRANSLATION) && ADS_LIB_VERSION < 600
          if (pszKeyFree)
          {
            hb_adsOemAnsiFree(static_cast<char *>(pszKeyFree));
          }
#endif
        }
        break;

      case ADS_NUMERIC:
        if (pItem->isNumeric())
        {
          auto dTemp = hb_itemGetND(pItem);
          AdsSetScope(hOrder, nScope, reinterpret_cast<UNSIGNED8 *>(&dTemp), static_cast<UNSIGNED16>(sizeof(dTemp)),
                      ADS_DOUBLEKEY);
        }
        break;

      case ADS_DATE:
        if (pItem->isDateTime())
        {
          double dTemp;
          dTemp = hb_itemGetDL(pItem);
          AdsSetScope(hOrder, nScope, reinterpret_cast<UNSIGNED8 *>(&dTemp), static_cast<UNSIGNED16>(sizeof(dTemp)),
                      ADS_DOUBLEKEY);
        }
        break;

      case ADS_LOGICAL:
        AdsSetScope(hOrder, nScope, reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetL(pItem) ? "T" : "F")),
                    1, ADS_STRINGKEY);
        break;
      }
    }
    else
    {
      AdsClearScope(hOrder, nScope);
    }

    return Harbour::SUCCESS;
  }
  else
  {
    return Harbour::FAILURE;
  }
}

static double adsGetRelPos(ADSAREAP pArea, ADSHANDLE hOrder)
{
  HB_ULONG ulRecNo, ulRecCount;

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  if (!pArea->fPositioned)
  {
    SELF_RECNO(&pArea->area, &ulRecNo);
    return ulRecNo > 1 ? 1.0 : 0.0;
  }
  else if (hOrder)
  {
    DOUBLE dPos = 0.0;
    AdsGetRelKeyPos(hOrder, &dPos);
    return dPos;
  }
  else
  {
    SELF_RECNO(&pArea->area, &ulRecNo);
    SELF_RECCOUNT(&pArea->area, &ulRecCount);
    if (ulRecNo == 0 || ulRecCount == 0)
    {
      return 0.0;
    }
    else
    {
      /* ADS counts relative record position in this way */
      return (0.5 + ulRecNo) / ulRecCount;
    }
  }
}

static void adsSetRelPos(ADSAREAP pArea, ADSHANDLE hOrder, double dPos)
{
  ADSHANDLE hCurrOrder = pArea->hOrdCurrent;

  pArea->hOrdCurrent = hOrder;
  if (dPos >= 1.0)
  {
    SELF_GOBOTTOM(&pArea->area);
  }
  else if (dPos <= 0.0)
  {
    SELF_GOTOP(&pArea->area);
  }
  else if (hOrder)
  {
    /* reset any pending relations */
    SELF_RESETREL(pArea);

    AdsSetRelKeyPos(hOrder, dPos);
    hb_adsUpdateAreaFlags(pArea);
    /* Force relational movement in child WorkAreas */
    if (pArea->area.lpdbRelations)
    {
      SELF_SYNCCHILDREN(&pArea->area);
    }

    SELF_SKIPFILTER(&pArea->area, 1);
    if (pArea->area.fEof)
    {
      SELF_GOTOP(&pArea->area);
    }
  }
  else
  {
    HB_ULONG ulRecCount, ulRecNo;

    SELF_RECCOUNT(&pArea->area, &ulRecCount);
    ulRecNo = static_cast<HB_ULONG>(dPos) * ulRecCount + 1;
    if (ulRecNo >= ulRecCount)
    {
      ulRecNo = ulRecCount;
    }
    SELF_GOTO(&pArea->area, ulRecNo);

    SELF_SKIPFILTER(&pArea->area, 1);
    if (pArea->area.fEof)
    {
      SELF_GOTOP(&pArea->area);
    }
  }
  pArea->hOrdCurrent = hCurrOrder;
}

HB_ERRCODE hb_adsCloseCursor(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_adsCloseCursor(%p)", static_cast<void*>(pArea)));
#endif

  HB_ERRCODE errCode;

  pArea->hOrdCurrent = 0;
  if (pArea->hTable)
  {
    UNSIGNED32 u32RetVal = AdsCloseTable(pArea->hTable);

    if (u32RetVal != AE_SUCCESS)
    {
#if 0
         HB_TRACE(HB_TR_DEBUG, ("adsCloseTable(%lu, %s) failed", static_cast<HB_ULONG>(u32RetVal), pArea->szDataFileName));
#endif
    }

    pArea->hTable = 0;
  }
  if (pArea->hStatement)
  {
    AdsCloseSQLStatement(pArea->hStatement);
    pArea->hStatement = 0;
  }

  errCode = SUPER_CLOSE(&pArea->area);

  /* Free buffer */
  if (pArea->pRecord)
  {
    hb_xfree(pArea->pRecord);
    pArea->pRecord = nullptr;
  }

  /* Free all filenames */
  if (pArea->szDataFileName)
  {
    hb_xfree(pArea->szDataFileName);
    pArea->szDataFileName = nullptr;
  }
  return errCode;
}

/*
 * -- ADS METHODS --
 */

static HB_ERRCODE adsBof(ADSAREAP pArea, HB_BOOL *pBof)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsBof(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pBof)));
#endif

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  *pBof = pArea->area.fBof;

  return Harbour::SUCCESS;
}

static HB_ERRCODE adsEof(ADSAREAP pArea, HB_BOOL *pEof)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsEof(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pEof)));
#endif

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  *pEof = pArea->area.fEof;

  return Harbour::SUCCESS;
}

static HB_ERRCODE adsFound(ADSAREAP pArea, HB_BOOL *pFound)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsFound(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pFound)));
#endif

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  *pFound = pArea->area.fFound;

  return Harbour::SUCCESS;
}

static HB_ERRCODE adsGoBottom(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsGoBottom(%p)", static_cast<void*>(pArea)));
#endif

  UNSIGNED32 u32RetVal;

  /* reset any pending relations */
  SELF_RESETREL(pArea);

  pArea->area.fTop = false;
  pArea->area.fBottom = true;

  u32RetVal = AdsGotoBottom((pArea->hOrdCurrent) ? pArea->hOrdCurrent : pArea->hTable);
  if (u32RetVal != AE_SUCCESS)
  {
    commonError(pArea, EG_CORRUPTION, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, EF_CANDEFAULT, nullptr);
    return Harbour::FAILURE;
  }

  hb_adsUpdateAreaFlags(pArea);

  /* Force relational movement in child WorkAreas */
  if (pArea->area.lpdbRelations)
  {
    SELF_SYNCCHILDREN(&pArea->area);
  }

  return SELF_SKIPFILTER(&pArea->area, -1);
}

static HB_ERRCODE adsGoTo(ADSAREAP pArea, HB_ULONG ulRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsGoTo(%p, %lu)", static_cast<void*>(pArea), ulRecNo));
#endif

  /*
   * TODO: bh: Note  Explicitly moving to a deleted record when using the
   * Advantage proprietary table format (ADT) is an illegal operation and
   * will return the error 5022 (AE_INVALID_RECORD_NUMBER), invalid record
   * number.
   *
   * 2005-09-24 16:24:18 CEST, Druzus
   * IMHO such GOTO operation should reposition our RDD to phantom record
   */
  UNSIGNED32 u32RetVal, u32RecNo;
  HB_ULONG ulRecCount;

  /* 2001-07-19 15:04, BH
     The following call is a necessary workaround for ace32.dll
     prior to 6.1.  There were bugs where
     AdsGotoRecord() can FAIL to move the record pointer
     after some sequences of setting/clearing relations.
     A call to AdsGetRecordNum() before it clears the problem. */

  /* 2005-08-25 11:56:20 CEST, Druzus
   * This trick force to resolving pending relations. It means
   * that this ADS clients does not reset pending relation in
   * AdsGotoRecord() and it is resolved later. Similar situation
   * can appear also in APPEND() where I can see call to AdsAtEOF()
   * which will have exactly the same side effect as AdsGetRecordNum()
   * - both reset pending relations by resolving them
   */

  /* reset our pending relations */
  SELF_RESETREL(pArea);

  /* force to reset ACE pending relations by resolving them */
  AdsGetRecordNum(pArea->hTable, ADS_IGNOREFILTERS, &u32RecNo);

  /* always make GOTO - it's necessary to sync children inside ACE
     internals */
  u32RetVal = AdsGotoRecord(pArea->hTable, static_cast<UNSIGNED32>(ulRecNo));

  if (u32RetVal == AE_INVALID_RECORD_NUMBER)
  {
    /*
     * We are going to ADT deleted record - see above. GO to phantom
     * record in such case
     */
    ulRecNo = 0;
    u32RetVal = AdsGotoRecord(pArea->hTable, 0);
  }

  /* Usually AdsGotoRecord(*, 0) is valid call and returns no error,
   * but if previous error was AE_INVALID_RECORD_NUMBER, the following
   * AdsGotoRecord(*, 0) returns the same error. I'm not sure if this
   * AdsGotoRecord(*, 0) will position to phantom record and return
   * empty fields. I hope I have not made any regressions in code, since
   * it replicates previous behaviour. [Mindaugas]
   */
  if (u32RetVal != AE_SUCCESS && u32RetVal != AE_INVALID_RECORD_NUMBER)
  {
    commonError(pArea, EG_CORRUPTION, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, EF_CANDEFAULT, nullptr);
    return Harbour::FAILURE;
  }

  /* update area flag */
  hb_adsUpdateAreaFlags(pArea);
  pArea->ulRecNo = ulRecNo;

  if (!pArea->fPositioned)
  {
    /* set our record number value */
    SELF_RECCOUNT(&pArea->area, &ulRecCount);
    /* eliminate possible race condition in this operation */
    if (ulRecNo != 0 && ulRecNo <= ulRecCount)
    {
      pArea->ulRecNo = ulRecNo;
    }
    else
    {
      pArea->ulRecNo = ulRecCount + 1;
    }
  }
  else if (ulRecNo == u32RecNo)
  {
    /* is it really necessary? doesn't AdsGotoRecord() refresh buffers? */
    AdsRefreshRecord(pArea->hTable);
  }

  /* Force relational movement in child WorkAreas */
  if (pArea->area.lpdbRelations)
  {
    SELF_SYNCCHILDREN(&pArea->area);
  }

  return u32RetVal == AE_SUCCESS ? Harbour::SUCCESS : Harbour::FAILURE;
}

static HB_ERRCODE adsGoToId(ADSAREAP pArea, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsGoToId(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pItem)));
#endif

  if (pItem->isNumeric())
  {
    HB_ULONG ulRecNo = hb_itemGetNL(pItem);
    return SELF_GOTO(&pArea->area, ulRecNo);
  }
  else
  {
    commonError(pArea, EG_DATATYPE, EDBF_DATATYPE - 900, 0, nullptr, 0, nullptr);
    return Harbour::FAILURE;
  }
}

static HB_ERRCODE adsGoTop(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsGoTop(%p)", static_cast<void*>(pArea)));
#endif

  UNSIGNED32 u32RetVal;

  /* reset any pending relations */
  SELF_RESETREL(pArea);

  pArea->area.fTop = true;
  pArea->area.fBottom = false;
  u32RetVal = AdsGotoTop((pArea->hOrdCurrent) ? pArea->hOrdCurrent : pArea->hTable);
  if (u32RetVal != AE_SUCCESS)
  {
    commonError(pArea, EG_CORRUPTION, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, EF_CANDEFAULT, nullptr);
    return Harbour::FAILURE;
  }

  hb_adsUpdateAreaFlags(pArea);

  /* Force relational movement in child WorkAreas */
  if (pArea->area.lpdbRelations)
  {
    SELF_SYNCCHILDREN(&pArea->area);
  }

  return SELF_SKIPFILTER(&pArea->area, 1);
}

static HB_ERRCODE adsSeek(ADSAREAP pArea, HB_BOOL bSoftSeek, PHB_ITEM pKey, HB_BOOL bFindLast)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsSeek(%p, %d, %p, %d)", static_cast<void*>(pArea), bSoftSeek, static_cast<void*>(pKey), bFindLast));
#endif

  UNSIGNED32 u32RecNo = 0, u32NewRec, u32RetVal;
  UNSIGNED16 u16SeekType = (bSoftSeek) ? ADS_SOFTSEEK : ADS_HARDSEEK, u16KeyType, u16Found, u16KeyLen;
  UNSIGNED8 *pszKey, pKeyBuf[8];
  double dValue;
  UNSIGNED8 *pucSavedKey = nullptr;
  UNSIGNED16 u16SavedKeyLen =
      ADS_MAX_KEY_LENGTH; /* this may be longer than the actual seek expression, so we don't pass it along */
#if defined(ADS_USE_OEM_TRANSLATION) && ADS_LIB_VERSION < 600
  UNSIGNED8 *pszKeyFree = nullptr;
#endif

  if (!pArea->hOrdCurrent)
  {
    commonError(pArea, EG_NOORDER, EDBF_NOTINDEXED - 900, 0, nullptr, EF_CANDEFAULT, nullptr);
    return Harbour::FAILURE;
  }

  /* build a seek key */
  if (pKey->isString())
  {
    pszKey = reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(pKey)));
    u16KeyLen = static_cast<UNSIGNED16>(hb_itemGetCLen(pKey));
#ifdef ADS_USE_OEM_TRANSLATION
#if ADS_LIB_VERSION >= 600
    u16KeyType = hb_ads_bOEM ? ADS_RAWKEY : ADS_STRINGKEY;
#else
    u16KeyType = ADS_STRINGKEY;
    if (hb_ads_bOEM)
    {
      pszKey = pszKeyFree = static_cast<UNSIGNED8 *>(hb_adsOemToAnsi(static_cast<const char *>(pszKey), u16KeyLen));
    }
#endif
#else
    u16KeyType = ADS_STRINGKEY;
#endif
  }
  else if (pKey->isDateTime())
  {
    u16KeyType = 0;
    AdsGetKeyType(pArea->hOrdCurrent, &u16KeyType);
    /* index on timestamp values */
    if (pArea->iFileType == ADS_ADT && u16KeyType == ADS_RAW)
    {
      long lDate, lTime;
      hb_itemGetTDT(pKey, &lDate, &lTime);
      /* ADS stores milliseconds in raw ADT form increased by one */
      ++lTime;
      HB_PUT_BE_UINT32(pKeyBuf, lDate);
      HB_PUT_BE_UINT32(&pKeyBuf[4], lTime);
      pszKey = pKeyBuf;
      u16KeyLen = pKey->isTimeStamp() ? 8 : 4;
      u16KeyType = ADS_RAWKEY;
    }
    else
    {
      dValue = hb_itemGetTD(pKey);
      pszKey = reinterpret_cast<UNSIGNED8 *>(&dValue);
      u16KeyLen = static_cast<UNSIGNED16>(sizeof(double));
      u16KeyType = ADS_DOUBLEKEY;
    }
  }
  else if (pKey->isNumeric())
  {
    dValue = hb_itemGetND(pKey);
    pszKey = reinterpret_cast<UNSIGNED8 *>(&dValue);
    u16KeyLen = static_cast<UNSIGNED16>(sizeof(double));
    u16KeyType = ADS_DOUBLEKEY;
  }
  else if (pKey->isLogical())
  {
    pszKey = reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetL(pKey) ? "1" : "0"));
    u16KeyLen = 1;
    u16KeyType = ADS_STRINGKEY;
  }
  else
  {
    commonError(pArea, EG_DATATYPE, EDBF_DATATYPE - 900, 0, nullptr, 0, nullptr);
    return Harbour::FAILURE;
  }

  /* reset any pending relations - I hope ACE make the same and the problem
     reported in GOTO() does not exist here */
  SELF_RESETREL(pArea);

  /*
   * This flag should be cleaned inside seek
   * They could be used by SKIP_FILTER and gives differ result
   * when seek is called just after GOTOP or GOBOTTOM, Druzus.
   */
  pArea->area.fTop = pArea->area.fBottom = false;

  if (bFindLast)
  {
    u32RetVal = AdsSeekLast(pArea->hOrdCurrent, pszKey, u16KeyLen, u16KeyType, &u16Found);
    if (u32RetVal == AE_SUCCESS && bSoftSeek && !u16Found)
    {
      /* in such case ADS set record at EOF position so we
         should make normal soft seek and then skip -1 to emulate
         Clipper behavior, Druzus */
      u32RetVal = AdsSeek(pArea->hOrdCurrent, pszKey, u16KeyLen, u16KeyType, u16SeekType, &u16Found);

      if (u32RetVal == AE_SUCCESS)
      {
        u32RetVal = AdsSkip(pArea->hOrdCurrent, -1);
      }
    }
  }
  else
  {
    u32RetVal = AdsSeek(pArea->hOrdCurrent, pszKey, u16KeyLen, u16KeyType, u16SeekType, &u16Found);
  }

  if (u32RetVal != AE_SUCCESS)
  {
    commonError(pArea, EG_CORRUPTION, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, EF_CANDEFAULT, nullptr);
    return Harbour::FAILURE;
  }

  hb_adsUpdateAreaFlags(pArea);

  /* check if we are at ADS BOF phantom record. It's possible only after
   * AdsSkip(-1) above */
  if (pArea->area.fBof && !pArea->area.fEof)
  {
    HB_ERRCODE errCode = SELF_GOTO(&pArea->area, 0);
#if 0
      HB_ERRCODE errCode = SELF_GOTOP(&pArea->area);
#endif
    pArea->area.fBof = false;
#if defined(ADS_USE_OEM_TRANSLATION) && ADS_LIB_VERSION < 600
    if (pszKeyFree)
    {
      hb_adsOemAnsiFree(static_cast<char *>(pszKeyFree));
    }
#endif
    return errCode;
  }

  /* Force relational movement in child WorkAreas */
  if (pArea->area.lpdbRelations)
  {
    SELF_SYNCCHILDREN(&pArea->area);
  }

  /* BH:
     If a filter is set that is not valid for ADS, we need to skip
     off of any invalid records (IOW, filter at the Harbour level if ADS cannot
     because the filter has UDFs or PUBLICVAR references).
     To make sure the skipped-to record still matches the seek-ed key, we need to
     be able to construct a comparable key for the subsequent record.
     This is annoyingly complex with the various ads key types for various table types.
     AdsExtractKey would seem to be the api of choice, but here on the starting end the
     key we seek on does NOT match the format of what we get back from AdsExtractKey.
     So I'm saving off the first found record's key, and passing that to our
     adsIndexKeyCmp() to compare to the new record's key.
     We're relying on testing to verify that partial key searches and binary
     raw keys all end up working right. */
  if (pArea->area.dbfi.itmCobExpr && !pArea->area.dbfi.fOptimized && !pArea->area.fEof)
  {
    /* Remember FOUND flag for updating after SKIPFILTER() */
    u16Found = static_cast<UNSIGNED16>(pArea->area.fFound ? 1 : 0);

    if (u16Found && u16KeyLen > 0)
    {
      /*
       * remember the record number for faster checking if we should update
       * fFound after SKIPFILTER. Also get its extracted key to simplify
       * that comparison
       */
      pucSavedKey = static_cast<UNSIGNED8 *>(hb_xgrab(ADS_MAX_KEY_LENGTH + 1));

      AdsGetRecordNum(pArea->hTable, ADS_IGNOREFILTERS, &u32RecNo);
      if (AdsExtractKey(pArea->hOrdCurrent, pucSavedKey, &u16SavedKeyLen) != AE_SUCCESS)
      {
        u16SavedKeyLen = 0;
      }
      else if (u16SavedKeyLen > u16KeyLen)
      {
        /* Initial found key from index is longer than Seek key:
           Did a partial search */
        if (AdsGetKeyType(pArea->hOrdCurrent, &u16KeyType) == AE_SUCCESS &&
            (u16KeyType == ADS_STRING || u16KeyType == ADS_RAW))
        {
          /*
           * do partial comparison below on String and Raw indexes only.
           * Note that we can search a different type index with a string
           * expression, but ads does internal conversions and the length
           * of our string may be drastically different than the real key
           */
          u16SavedKeyLen = u16KeyLen;
        }
      }
    }

    /*
     * TODO: Possible optimization: if !softseek, skipfilter should abort
     * skipping once keys no longer match.
     * Perhaps use temp replacement of scope for this -- but remember ads
     * does scopes on client and if last good scoped record fails the filter,
     * the server will skip to the end anyway
     */
    if (SELF_SKIPFILTER(&pArea->area, bFindLast ? -1 : 1) != Harbour::SUCCESS)
    {
      if (pucSavedKey)
      {
        hb_xfree(pucSavedKey);
      }
#if defined(ADS_USE_OEM_TRANSLATION) && ADS_LIB_VERSION < 600
      if (pszKeyFree)
      {
        hb_adsOemAnsiFree(static_cast<char *>(pszKeyFree));
      }
#endif
      return Harbour::FAILURE;
    }

    if (u16Found)
    {
      if (pArea->area.fEof)
      {
        u16Found = 0;
      }
      /* seek empty string is synonymous with GoTop */
      else if (u16KeyLen > 0)
      {
        AdsGetRecordNum(pArea->hTable, ADS_IGNOREFILTERS, &u32NewRec);
        /* SkipFilter moved us?  see if index key is still a match. */
        if (u32RecNo != u32NewRec)
        {
          if (u16SavedKeyLen == 0)
          {
            u16Found = 0;
          }
          else
          {
            u16Found = static_cast<UNSIGNED16>(adsIndexKeyCmp(pArea->hOrdCurrent, pucSavedKey, u16SavedKeyLen));
          }
        }
      }
    }
    pArea->area.fFound = u16Found != 0;
  }

  /*
   * Clipper clears BOF after seek - I found only one strange situation
   * when it doesn't but only sometimes. I've not been able to detect
   * the rule yet. Any how it's much safer to clear it, Druzus.
   */
  pArea->area.fBof = false;

  if (pucSavedKey)
  {
    hb_xfree(pucSavedKey);
  }
#if defined(ADS_USE_OEM_TRANSLATION) && ADS_LIB_VERSION < 600
  if (pszKeyFree)
  {
    hb_adsOemAnsiFree(static_cast<char *>(pszKeyFree));
  }
#endif

  return Harbour::SUCCESS;
}

static HB_ERRCODE adsSkip(ADSAREAP pArea, HB_LONG lToSkip)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsSkip(%p, %ld)", static_cast<void*>(pArea), lToSkip));
#endif

  UNSIGNED32 u32RetVal;

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  /* Brian Hays:

     In ADS, if you GO 0 (as opposed to skipping past lastrec),
     it considers the record pointer "unpositioned".
     If you then try to skip -1 you end up at Top with BOF True.
     (If you skip past lastrec, then skip -1 it works right.)
     To fix this we need to trap for a (negative lToSkip .AND. EOF)
     and do a GoBottom--but only after letting ads try first and
     testing for BOF.  We need to avoid our GoBottom hack as much as
     possible because with a filter set it could be quite slow.

     In addition, if lToSkip > 1 we need to iterate calls to AdsSkip(1) and
     test the filter each time since, even if the server has a filter set,
     AdsSkip(5) may only test the filter after 5 raw skips (according to the
     Extended Systems developers.) */

  if (lToSkip == 0)
  {
    /* Clipper does not update record at EOF position in SKIP(0) */
    if (pArea->fPositioned)
    {
      bool fBof, fEof;

      /* Save flags */
      fBof = pArea->area.fBof;
      fEof = pArea->area.fEof;

      u32RetVal = AdsSkip((pArea->hOrdCurrent) ? pArea->hOrdCurrent : pArea->hTable, 0);
      if (u32RetVal != AE_SUCCESS)
      {
        commonError(pArea, EG_CORRUPTION, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, EF_CANDEFAULT, nullptr);
        return Harbour::FAILURE;
      }
      /* TODO: is this really necessary, doesn't AdsSkip(0) do that? */
      AdsRefreshRecord(pArea->hTable);
      hb_adsUpdateAreaFlags(pArea);

      /* Restore flags */
      pArea->area.fBof = fBof;
      pArea->area.fEof = fEof;

      /* Force relational movement in child WorkAreas */
      if (pArea->area.lpdbRelations)
      {
        SELF_SYNCCHILDREN(&pArea->area);
      }
    }
    return Harbour::SUCCESS;
  }
  else
  {
    HB_ERRCODE errCode = Harbour::SUCCESS;
    HB_LONG lSkipper;

    if (!pArea->fPositioned && lToSkip < 0)
    {
      errCode = SELF_GOBOTTOM(&pArea->area);
      ++lToSkip;
    }

    if (!pArea->fPositioned)
    {
      if (lToSkip > 0)
      {
        pArea->area.fEof = true;
      }
      else
      {
        pArea->area.fBof = true;
      }
    }
    else if (lToSkip)
    {
      pArea->area.fTop = pArea->area.fBottom = false;

      /*
       * This causes that skip is done on the server side but it
       * may cause bad side effect - in multiple skip ADS respects
       * only index keys and bitmap filter so it may skip differ
       * number of records than we asked, [druzus]
       */
      if (pArea->area.dbfi.itmCobExpr == nullptr || pArea->area.dbfi.fOptimized)
      {
        lSkipper = lToSkip;
      }
      else
      {
        lSkipper = lToSkip < 0 ? -1 : 1;
      }

      if (lToSkip > 0)
      {
        while (errCode == Harbour::SUCCESS && !pArea->area.fEof && lToSkip)
        {
          u32RetVal = AdsSkip((pArea->hOrdCurrent) ? pArea->hOrdCurrent : pArea->hTable, lSkipper);
          if (u32RetVal != AE_SUCCESS)
          {
            commonError(pArea, EG_CORRUPTION, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, EF_CANDEFAULT, nullptr);
            return Harbour::FAILURE;
          }
          hb_adsUpdateAreaFlags(pArea);
          /* Force relational movement in child WorkAreas */
          if (pArea->area.lpdbRelations)
          {
            SELF_SYNCCHILDREN(&pArea->area);
          }
          errCode = SELF_SKIPFILTER(&pArea->area, lSkipper);
          lToSkip -= lSkipper;
        }
        pArea->area.fBof = false;
      }
      else
      {
        while (errCode == Harbour::SUCCESS && !pArea->area.fBof && lToSkip)
        {
          u32RetVal = AdsSkip((pArea->hOrdCurrent) ? pArea->hOrdCurrent : pArea->hTable, lSkipper);
          if (u32RetVal != AE_SUCCESS)
          {
            commonError(pArea, EG_CORRUPTION, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, EF_CANDEFAULT, nullptr);
            return Harbour::FAILURE;
          }
          hb_adsUpdateAreaFlags(pArea);
          /* Force relational movement in child WorkAreas */
          if (pArea->area.lpdbRelations)
          {
            SELF_SYNCCHILDREN(&pArea->area);
          }
          errCode = SELF_SKIPFILTER(&pArea->area, lSkipper);
          lToSkip -= lSkipper;
        }
        pArea->area.fEof = false;
      }
    }

    return errCode;
  }
}

static HB_ERRCODE adsSkipFilter(ADSAREAP pArea, HB_LONG lUpDown)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsSkipFilter(%p, %ld)", static_cast<void*>(pArea), lUpDown));
#endif

  UNSIGNED32 u32RetVal;
  bool fBottom;
  HB_ERRCODE errCode;

  lUpDown = (lUpDown < 0 ? -1 : 1);

  /* remember if we are here after SELF_GOTOP() */
  fBottom = pArea->area.fBottom;

  /*
   * because ADS respect DELETED flag and SET_DELETED is synchronized
   * with ADS we only have to check for filter expressions which cannot
   * be optimized by ADS server, Druzus.
   */
  if (pArea->area.dbfi.itmCobExpr != nullptr && !pArea->area.dbfi.fOptimized)
  {
    while (!pArea->area.fBof && !pArea->area.fEof)
    {
      auto pResult = hb_vmEvalBlock(pArea->area.dbfi.itmCobExpr);
      if (!pResult->isLogical() || hb_itemGetL(pResult))
      {
        break;
      }

      u32RetVal = AdsSkip((pArea->hOrdCurrent) ? pArea->hOrdCurrent : pArea->hTable, lUpDown);
      if (u32RetVal != AE_SUCCESS)
      {
        commonError(pArea, EG_CORRUPTION, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, EF_CANDEFAULT, nullptr);
        return Harbour::FAILURE;
      }
      hb_adsUpdateAreaFlags(pArea);

      /* Force relational movement in child WorkAreas, the child fields
         can be a part of evaluated block so ti's necessary, here */
      if (pArea->area.lpdbRelations)
      {
        SELF_SYNCCHILDREN(&pArea->area);
      }
    }
  }

  /*
   * The only one situation when we should repos is backward skipping
   * if we are at BOTTOM position (it's SKIPFILTER called from GOBOTTOM)
   * then GOEOF() if not then GOTOP()
   */
  if (pArea->area.fBof && lUpDown < 0)
  {
    if (fBottom)
    {
      /* we were passing from bottom to this place (BOF) and we do not
         find any valid record so just simply make GOEOF() */
      errCode = SELF_GOTO(&pArea->area, 0);
    }
    else
    {
      errCode = SELF_GOTOP(&pArea->area);
      pArea->area.fBof = true;
    }
  }
  else if (!pArea->fPositioned)
  {
    HB_ULONG ulRecCount;
    /* set our record number value */
    errCode = SELF_RECCOUNT(&pArea->area, &ulRecCount);
    pArea->ulRecNo = ulRecCount + 1;
  }
  else
  {
    errCode = Harbour::SUCCESS;
  }

  return errCode;
}

#define adsSkipRaw nullptr
#define adsAddField nullptr

static HB_ERRCODE adsAppend(ADSAREAP pArea, HB_BOOL fUnLockAll)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsAppend(%p, %d)", static_cast<void*>(pArea), static_cast<int>(fUnLockAll)));
#endif

  UNSIGNED32 u32RetVal;

  /* reset any pending relations */
  SELF_RESETREL(pArea);

  /* ADS does not do the same so force to reset its relations by resolving them */
  if (pArea->area.uiParents)
  {
    UNSIGNED16 u16Eof;
    AdsAtEOF(pArea->hTable, &u16Eof);
  }

  if (fUnLockAll && pArea->fShared && !pArea->fFLocked)
  {
    SELF_RAWLOCK(&pArea->area, FILE_UNLOCK, 0);
  }

  u32RetVal = AdsAppendRecord(pArea->hTable);
  if (u32RetVal == AE_SUCCESS)
  {
    if (pArea->fShared && !pArea->fFLocked)
    {
      HB_ULONG ulRecNo;

      if (SELF_RECNO(&pArea->area, &ulRecNo) == Harbour::SUCCESS)
      {
        /* to avoid unnecessary record refreshing after locking */
        pArea->fPositioned = true;
        SELF_RAWLOCK(&pArea->area, REC_LOCK, ulRecNo);
      }
    }
    pArea->area.fBof = false;
    pArea->area.fEof = false;
    pArea->area.fFound = false;
    pArea->fPositioned = true;
    return Harbour::SUCCESS;
  }
  else if (u32RetVal == AE_TABLE_READONLY)
  {
    commonError(pArea, EG_READONLY, EDBF_READONLY - 900, 0, nullptr, 0, nullptr);
  }
  else if (u32RetVal == 1024 /* Append Lock Failed */)
  {
    commonError(pArea, EG_APPENDLOCK, EDBF_APPENDLOCK - 900, 0, nullptr, EF_CANDEFAULT, nullptr);
  }
  else
  {
    commonError(pArea, EG_CORRUPTION, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, EF_CANDEFAULT, nullptr);
  }

  return Harbour::FAILURE;
}

static HB_ERRCODE adsCreateFields(ADSAREAP pArea, PHB_ITEM pStruct)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsCreateFields(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pStruct)));
#endif

  HB_USHORT uiCount;
  HB_ERRCODE errCode = Harbour::SUCCESS;
  DBFIELDINFO dbFieldInfo{};
  const char *szType;

  auto uiItems = static_cast<HB_USHORT>(hb_arrayLen(pStruct));
  SELF_SETFIELDEXTENT(&pArea->area, uiItems);

  for (uiCount = 0; uiCount < uiItems; uiCount++)
  {
    HB_USHORT uiLen;
    const char *szFieldType;
    int iData;

    dbFieldInfo.uiTypeExtended = 0;
    auto pFieldDesc = hb_arrayGetItemPtr(pStruct, uiCount + 1);
    dbFieldInfo.atomName = hb_arrayGetCPtr(pFieldDesc, DBS_NAME);
    iData = hb_arrayGetNI(pFieldDesc, DBS_LEN);
    if (iData < 0)
    {
      iData = 0;
    }
    uiLen = dbFieldInfo.uiLen = static_cast<HB_USHORT>(iData);
    iData = hb_arrayGetNI(pFieldDesc, DBS_DEC);
    if (iData < 0)
    {
      iData = 0;
    }
    auto uiDec = static_cast<HB_USHORT>(iData);
    dbFieldInfo.uiDec = 0;
    szFieldType = szType = hb_arrayGetCPtr(pFieldDesc, DBS_TYPE);
    auto iNameLen = static_cast<int>(strlen(szFieldType));
    iData = HB_TOUPPER(szFieldType[0]);
#ifdef DBS_FLAG
    dbFieldInfo.uiFlags = hb_arrayGetNI(pFieldDesc, DBS_FLAG);
#else
    dbFieldInfo.uiFlags = 0;
    while (*++szType)
    {
      if (*szType == ':')
      {
        iNameLen = 1;
        while (*++szType)
        {
          switch (HB_TOUPPER(*szType))
          {
          case 'N':
            dbFieldInfo.uiFlags |= HB_FF_NULLABLE;
            break;
          case 'B':
            dbFieldInfo.uiFlags |= HB_FF_BINARY;
            break;
          case '+':
            dbFieldInfo.uiFlags |= HB_FF_AUTOINC;
            break;
          case 'Z':
            dbFieldInfo.uiFlags |= HB_FF_COMPRESSED;
            break;
          case 'E':
            dbFieldInfo.uiFlags |= HB_FF_ENCRYPTED;
            break;
          case 'U':
            dbFieldInfo.uiFlags |= HB_FF_UNICODE;
            break;
          }
        }
        break;
      }
    }
#endif
    while (iNameLen > 0 && szFieldType[iNameLen - 1] == ' ')
    {
      --iNameLen;
    }

    if (iNameLen > 1)
    {
      if (!hb_strnicmp(szFieldType, "autoinc", 2))
      {
        iData = '+';
      }
      else if (!hb_strnicmp(szFieldType, "binary", 2))
      {
        iData = 'W';
      }
      else if (!hb_strnicmp(szFieldType, "character", 2))
      {
        iData = 'C';
      }
#if ADS_LIB_VERSION >= 710
      else if (!hb_strnicmp(szFieldType, "cicharacter", 2))
      {
        iData = 'c';
      }
#endif
      else if (!hb_strnicmp(szFieldType, "curdouble", 2))
      {
        iData = 'Z';
      }
      else if (!hb_strnicmp(szFieldType, "date", 2))
      {
        iData = 'D';
      }
      else if (!hb_strnicmp(szFieldType, "double", 2))
      {
        iData = 'B';
      }
      else if (!hb_strnicmp(szFieldType, "image", 2))
      {
        iData = 'P';
      }
      else if (!hb_strnicmp(szFieldType, "integer", 2))
      {
        iData = 'I';
      }
      else if (!hb_strnicmp(szFieldType, "logical", 3))
      {
        iData = 'L';
      }
#if ADS_LIB_VERSION >= 700
      else if (!hb_strnicmp(szFieldType, "longlong", 3))
      {
        iData = 'I';
        uiLen = 8;
      }
#endif
      else if (!hb_strnicmp(szFieldType, "memo", 3))
      {
        iData = 'M';
      }
#if ADS_LIB_VERSION >= 800
      else if (!hb_strnicmp(szFieldType, "modtime", 3))
      {
        iData = '=';
      }
#endif
#if ADS_LIB_VERSION >= 700
      else if (!hb_strnicmp(szFieldType, "money", 3))
      {
        iData = 'Y';
      }
#endif
      else if (!hb_strnicmp(szFieldType, "numeric", 2))
      {
        iData = 'N';
      }
#if ADS_LIB_VERSION >= 1000
      else if (!hb_strnicmp(szFieldType, "nchar", 2))
      {
        iData = 'C';
        dbFieldInfo.uiFlags |= HB_FF_UNICODE;
      }
      else if (!hb_strnicmp(szFieldType, "nmemo", 2))
      {
        iData = 'M';
        dbFieldInfo.uiFlags |= HB_FF_UNICODE;
      }
      else if (!hb_strnicmp(szFieldType, "nvarchar", 2))
      {
        iData = 'Q';
        dbFieldInfo.uiFlags |= HB_FF_UNICODE;
      }
#endif
      else if (!hb_strnicmp(szFieldType, "shortdate", 6))
      {
        iData = 'D';
#if ADS_LIB_VERSION >= 900
        uiLen = (pArea->iFileType == ADS_ADT || pArea->iFileType == ADS_VFP) ? 4 : 3;
#else
        uiLen = pArea->iFileType == ADS_ADT ? 4 : 3;
#endif
      }
      else if (!hb_strnicmp(szFieldType, "shortint", 6))
      {
        iData = 'I';
        uiLen = 2;
      }
      else if (!hb_stricmp(szFieldType, "time"))
      {
        iData = 'T';
        uiLen = 4;
      }
      else if (!hb_strnicmp(szFieldType, "timestamp", 5))
      {
        iData = '@';
        uiLen = 8;
      }
      else if (!hb_strnicmp(szFieldType, "raw", 2))
      {
        iData = 'C';
        dbFieldInfo.uiFlags |= HB_FF_BINARY;
      }
      else if (!hb_strnicmp(szFieldType, "rowversion", 2))
      {
        iData = '^';
      }
#if ADS_LIB_VERSION >= 900
      else if (!hb_strnicmp(szFieldType, "varcharfox", 8))
      {
        iData = 'Q';
        dbFieldInfo.uiTypeExtended = ADS_VARCHAR_FOX;
      }
      else if (!hb_strnicmp(szFieldType, "varbinaryfox", 10))
      {
        iData = 'Q';
        dbFieldInfo.uiTypeExtended = ADS_VARBINARY_FOX;
        dbFieldInfo.uiFlags |= HB_FF_BINARY;
      }
#endif
      else if (!hb_strnicmp(szFieldType, "varchar", 4))
      {
        iData = 'Q';
      }
      else if (!hb_strnicmp(szFieldType, "varbinary", 4))
      {
        iData = 'Q';
        dbFieldInfo.uiFlags |= HB_FF_BINARY;
      }
      else
      {
        iData = 0;
      }
    }

    switch (iData)
    {
#if ADS_LIB_VERSION >= 710
    case 'c':
      if (pArea->iFileType == ADS_ADT)
      {
        dbFieldInfo.uiType = Harbour::DB::Field::STRING;
        dbFieldInfo.uiTypeExtended = ADS_CISTRING;
        dbFieldInfo.uiLen = uiLen;
        dbFieldInfo.uiFlags = 0;
      }
      else
      {
        errCode = Harbour::FAILURE;
      }
      break;
#endif

    case 'C':
      dbFieldInfo.uiType = Harbour::DB::Field::STRING;
#if ADS_LIB_VERSION >= 1000
      if (dbFieldInfo.uiFlags & HB_FF_UNICODE)
      {
        dbFieldInfo.uiTypeExtended = ADS_NCHAR;
      }
      else
#endif
          if (dbFieldInfo.uiFlags & HB_FF_BINARY)
      {
        dbFieldInfo.uiTypeExtended = ADS_RAW;
      }
      else
      {
        dbFieldInfo.uiTypeExtended = ADS_STRING;
      }
      dbFieldInfo.uiLen = uiLen;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE | HB_FF_BINARY | HB_FF_COMPRESSED | HB_FF_ENCRYPTED | HB_FF_UNICODE;
      break;

    case 'M':
      dbFieldInfo.uiType = Harbour::DB::Field::MEMO;
#if ADS_LIB_VERSION >= 1000
      if (dbFieldInfo.uiFlags & HB_FF_UNICODE)
      {
        dbFieldInfo.uiTypeExtended = ADS_NMEMO;
      }
      else
#endif
          if (dbFieldInfo.uiFlags & HB_FF_BINARY)
      {
        dbFieldInfo.uiTypeExtended = ADS_BINARY;
      }
      else
      {
        dbFieldInfo.uiTypeExtended = ADS_MEMO;
      }
#if ADS_LIB_VERSION >= 900
      if (pArea->iFileType == ADS_VFP)
      {
        dbFieldInfo.uiLen = 4;
      }
      else
#endif
        dbFieldInfo.uiLen = pArea->iFileType == ADS_ADT ? 9 : 10;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE | HB_FF_BINARY | HB_FF_COMPRESSED | HB_FF_ENCRYPTED | HB_FF_UNICODE;
      break;

    case 'Q':
      dbFieldInfo.uiType = Harbour::DB::Field::VARLENGTH;
#if ADS_LIB_VERSION >= 1000
      if (dbFieldInfo.uiFlags & HB_FF_UNICODE)
      {
        dbFieldInfo.uiTypeExtended = ADS_NVARCHAR;
      }
      else
#endif
#if ADS_LIB_VERSION >= 900
          if (pArea->iFileType == ADS_VFP)
      {
        if (dbFieldInfo.uiFlags & HB_FF_BINARY)
        {
          dbFieldInfo.uiTypeExtended = ADS_VARBINARY_FOX;
        }
        else
        {
          dbFieldInfo.uiTypeExtended = ADS_VARCHAR_FOX;
        }
      }
      else
#endif
          if (dbFieldInfo.uiTypeExtended == 0)
      {
        /* TOCHECK: I've used ADS_VARBINARY_FOX here since there is no
                    better constant for this [Mindaugas] */
        if (dbFieldInfo.uiFlags & HB_FF_BINARY)
        {
#if ADS_LIB_VERSION >= 900
          dbFieldInfo.uiTypeExtended = ADS_VARBINARY_FOX;
#else
          dbFieldInfo.uiTypeExtended = ADS_RAW;
#endif
        }
        else
        {
          dbFieldInfo.uiTypeExtended = ADS_VARCHAR;
        }
      }
      dbFieldInfo.uiLen = uiLen;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE | HB_FF_BINARY | HB_FF_COMPRESSED | HB_FF_ENCRYPTED | HB_FF_UNICODE;
      break;

    case 'W':
      dbFieldInfo.uiType = Harbour::DB::Field::BLOB;
      dbFieldInfo.uiTypeExtended = ADS_BINARY;
      dbFieldInfo.uiFlags = HB_FF_BINARY;
#if ADS_LIB_VERSION >= 900
      if (pArea->iFileType == ADS_VFP)
      {
        dbFieldInfo.uiLen = 4;
      }
      else
#endif
        dbFieldInfo.uiLen = pArea->iFileType == ADS_ADT ? 9 : 10;
      break;

    case 'P':
      dbFieldInfo.uiType = Harbour::DB::Field::IMAGE;
      dbFieldInfo.uiTypeExtended = ADS_IMAGE;
      dbFieldInfo.uiFlags = HB_FF_BINARY;
#if ADS_LIB_VERSION >= 900
      if (pArea->iFileType == ADS_VFP)
      {
        dbFieldInfo.uiLen = 4;
      }
      else
#endif
        dbFieldInfo.uiLen = pArea->iFileType == ADS_ADT ? 9 : 10;
      break;

    case 'D':
      dbFieldInfo.uiType = Harbour::DB::Field::DATE;
      dbFieldInfo.uiLen = (pArea->iFileType == ADS_ADT || uiLen == 4) ? 4 : (uiLen == 3 ? 3 : 8);
      dbFieldInfo.uiTypeExtended = dbFieldInfo.uiLen == 3 ? ADS_COMPACTDATE : ADS_DATE;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      break;

    case 'L':
      dbFieldInfo.uiType = Harbour::DB::Field::LOGICAL;
      dbFieldInfo.uiTypeExtended = ADS_LOGICAL;
      dbFieldInfo.uiLen = 1;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      break;

    case 'T':
      if (pArea->iFileType == ADS_ADT && uiLen != 8)
      {
        dbFieldInfo.uiType = Harbour::DB::Field::TIME;
        dbFieldInfo.uiTypeExtended = ADS_TIME;
        dbFieldInfo.uiLen = 4;
        dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
        break;
      }
      /* fallthrough */

    case '@':
#if ADS_LIB_VERSION >= 900
      if (pArea->iFileType == ADS_ADT || pArea->iFileType == ADS_VFP)
#else
      if (pArea->iFileType == ADS_ADT)
#endif
      {
        dbFieldInfo.uiType = Harbour::DB::Field::TIMESTAMP;
        dbFieldInfo.uiTypeExtended = ADS_TIMESTAMP;
        dbFieldInfo.uiLen = 8;
        dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      }
      else
      {
        errCode = Harbour::FAILURE;
      }
      break;

    case 'N':
      dbFieldInfo.uiType = Harbour::DB::Field::LONG;
      dbFieldInfo.uiTypeExtended = ADS_NUMERIC;
      dbFieldInfo.uiLen = uiLen;
      dbFieldInfo.uiDec = uiDec;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      if (uiLen > 32)
      {
        errCode = Harbour::FAILURE;
      }
      break;

    case 'I':
      dbFieldInfo.uiType = Harbour::DB::Field::INTEGER;
#if ADS_LIB_VERSION >= 700
      dbFieldInfo.uiLen = (pArea->iFileType == ADS_ADT && uiLen == 2) ? 2 : (uiLen == 8 ? 8 : 4);
      dbFieldInfo.uiTypeExtended =
          dbFieldInfo.uiLen == 2 ? ADS_SHORTINT : (dbFieldInfo.uiLen == 8 ? ADS_LONGLONG : ADS_INTEGER);
#else
      dbFieldInfo.uiLen = (pArea->iFileType == ADS_ADT && uiLen == 2) ? 2 : 4;
      dbFieldInfo.uiTypeExtended = dbFieldInfo.uiLen == 2 ? ADS_SHORTINT : ADS_INTEGER;
#endif
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      break;

#if ADS_LIB_VERSION >= 700
    case 'Y':
      dbFieldInfo.uiType = Harbour::DB::Field::CURRENCY;
      dbFieldInfo.uiTypeExtended = ADS_MONEY;
      dbFieldInfo.uiLen = 8;
      dbFieldInfo.uiDec = 4;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      break;
#endif

    case 'Z':
      if (pArea->iFileType == ADS_ADT)
      {
        dbFieldInfo.uiType = Harbour::DB::Field::CURDOUBLE;
        dbFieldInfo.uiTypeExtended = ADS_CURDOUBLE;
        dbFieldInfo.uiLen = 8;
        dbFieldInfo.uiDec = uiDec;
        dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      }
      else
      {
        errCode = Harbour::FAILURE;
      }
      break;

    case '8':
    case 'B':
      dbFieldInfo.uiType = Harbour::DB::Field::DOUBLE;
      dbFieldInfo.uiTypeExtended = ADS_DOUBLE;
      dbFieldInfo.uiLen = 8;
      dbFieldInfo.uiDec = uiDec;
      dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      if (uiDec > 20)
      {
        errCode = Harbour::FAILURE;
      }
      break;

    case '+':
#if ADS_LIB_VERSION >= 900
      if (pArea->iFileType == ADS_ADT || pArea->iFileType == ADS_VFP)
#else
      if (pArea->iFileType == ADS_ADT)
#endif
      {
        dbFieldInfo.uiType = Harbour::DB::Field::AUTOINC;
        dbFieldInfo.uiTypeExtended = ADS_AUTOINC;
        dbFieldInfo.uiLen = 4;
        dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      }
      else
      {
        errCode = Harbour::FAILURE;
      }
      break;

#if ADS_LIB_VERSION >= 800
    case '=':
      if (pArea->iFileType == ADS_ADT)
      {
        dbFieldInfo.uiType = Harbour::DB::Field::MODTIME;
        dbFieldInfo.uiTypeExtended = ADS_MODTIME;
        dbFieldInfo.uiLen = 8;
        dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      }
      else
      {
        errCode = Harbour::FAILURE;
      }
      break;

    case '^':
      if (pArea->iFileType == ADS_ADT)
      {
        dbFieldInfo.uiType = Harbour::DB::Field::ROWVER;
        dbFieldInfo.uiTypeExtended = ADS_ROWVERSION;
        dbFieldInfo.uiLen = 8;
        dbFieldInfo.uiFlags &= HB_FF_NULLABLE;
      }
      else
      {
        errCode = Harbour::FAILURE;
      }
      break;
#endif

    default:
      errCode = Harbour::FAILURE;
      break;
    }

    if (errCode == Harbour::SUCCESS)
    {
      errCode = SELF_ADDFIELD(&pArea->area, &dbFieldInfo); /* Add field */
    }

    if (errCode != Harbour::SUCCESS)
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_DBCMDBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      return errCode;
    }
  }
  return Harbour::SUCCESS;
}

static HB_ERRCODE adsDeleteRec(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsDeleteRec(%p)", static_cast<void*>(pArea)));
#endif

  UNSIGNED32 u32RetVal;

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  if (!pArea->fPositioned)
  {
    return Harbour::SUCCESS;
  }

  if (hb_ads_bTestRecLocks)
  {
    if (hb_adsCheckLock(pArea) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  u32RetVal = AdsDeleteRecord(pArea->hTable);

  return u32RetVal == AE_SUCCESS ? Harbour::SUCCESS : Harbour::FAILURE;
}

static HB_ERRCODE adsDeleted(ADSAREAP pArea, HB_BOOL *pDeleted)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsDeleted(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pDeleted)));
#endif

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  if (!pArea->fPositioned)
  {
    /* AdsIsRecordDeleted() has error AE_NO_CURRENT_RECORD at EOF; avoid server call */
    *pDeleted = HB_FALSE;
  }
  else
  {
    UNSIGNED16 u16Deleted = 0;
    UNSIGNED32 u32RetVal;

    u32RetVal = AdsIsRecordDeleted(pArea->hTable, &u16Deleted);
    *pDeleted = u16Deleted != 0;

    return u32RetVal == AE_SUCCESS ? Harbour::SUCCESS : Harbour::FAILURE;
  }
  return Harbour::SUCCESS;
}

static HB_ERRCODE adsFieldCount(ADSAREAP pArea, HB_USHORT *uiFields)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsFieldCount(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(uiFields)));
#endif

  UNSIGNED16 u16Fields;

  AdsGetNumFields(pArea->hTable, &u16Fields);
  *uiFields = u16Fields;

  return Harbour::SUCCESS;
}

#define adsFieldDisplay nullptr

static HB_ERRCODE adsFieldInfo(ADSAREAP pArea, HB_USHORT uiIndex, HB_USHORT uiType, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsFieldInfo(%p, %hu, %hu, %p)", static_cast<void*>(pArea), uiIndex, uiType, static_cast<void*>(pItem)));
#endif

  if (uiIndex > pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  switch (uiType)
  {
  case DBS_ISNULL: {
    UNSIGNED16 u16Null = 1;

    if (pArea->fPositioned)
    {
      UNSIGNED32 u32RetVal;

#if ADS_LIB_VERSION >= 900
      u32RetVal = AdsIsNull(pArea->hTable, ADSFIELD(uiIndex), &u16Null);
#else
      u32RetVal = AdsIsEmpty(pArea->hTable, ADSFIELD(uiIndex), &u16Null);
#endif
      if (u32RetVal != AE_SUCCESS)
      {
        commonError(pArea, EG_READ, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, 0, nullptr);
        return Harbour::FAILURE;
      }
    }
    hb_itemPutL(pItem, u16Null != 0);
    break;
  }
  case DBS_TYPE:
#if ADS_LIB_VERSION >= 710
    if (pArea->area.lpFields[uiIndex - 1].uiTypeExtended == ADS_CISTRING)
    {
      hb_itemPutC(pItem, "CICHARACTER");
      break;
    }
#endif
    /* fallthrough */
  default:
    return SUPER_FIELDINFO(&pArea->area, uiIndex, uiType, pItem);
  }
  return Harbour::SUCCESS;
}

static HB_ERRCODE adsFieldName(ADSAREAP pArea, HB_USHORT uiIndex, void *szName)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsFieldName(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, szName));
#endif

  if (uiIndex <= pArea->area.uiFieldCount)
  {
    UNSIGNED16 u16Len = pArea->area.uiMaxFieldNameLength + 1;

    AdsGetFieldName(pArea->hTable, uiIndex, static_cast<UNSIGNED8 *>(szName), &u16Len);

    return Harbour::SUCCESS;
  }
  else
  {
    return Harbour::FAILURE;
  }
}

static HB_ERRCODE adsFlush(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsFlush(%p)", static_cast<void*>(pArea)));
#endif

  /* This function should flush current record buffer if hot and
     send to OS request to flush its file buffers to disk, so as well as
     of AdsWriteRecord(), AdsFlushFileBuffers() should be used FOR LOCAL
     TABLES (it's ignored by Remote Server).
     AdsWriteRecord() "flushes to the Advantage server",
     AdsFlushFileBuffers() tells the local server to flush to disk.
     Without it, we are dependent on the adslocal.cfg Flush Frequency setting.
   */

  if (!pArea->fReadonly)
  {
    AdsWriteRecord(pArea->hTable);
#if ADS_LIB_VERSION >= 610
    if (hb_setGetL(HB_SET_HARDCOMMIT))
    {
      AdsFlushFileBuffers(pArea->hTable);
    }
#endif
  }

  return Harbour::SUCCESS;
}

static HB_ERRCODE adsGetRec(ADSAREAP pArea, HB_BYTE **pBuffer)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsGetRec(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pBuffer)));
#endif

  auto u32Len = static_cast<UNSIGNED32>(pArea->ulRecordLen);
  UNSIGNED32 u32Result;

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  *pBuffer = pArea->pRecord;
  if (!pArea->fPositioned)
  {
    memset(pArea->pRecord, ' ', u32Len);
    return Harbour::FAILURE;
  }
  else
  {
    u32Result = AdsGetRecord(pArea->hTable, pArea->pRecord, &u32Len);
  }

  return u32Result == AE_SUCCESS ? Harbour::SUCCESS : Harbour::FAILURE;
}

static HB_ERRCODE adsGetValue(ADSAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsGetValue(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pItem)));
#endif

  LPFIELD pField;
  HB_BYTE *pBuffer = pArea->pRecord;
  UNSIGNED32 u32Length;
  UNSIGNED32 u32RetVal;

  if (!uiIndex || uiIndex > pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  pField = pArea->area.lpFields + uiIndex - 1;

  /* This code was optimized for use ADSFIELD() macro instead */
  /* AdsGetFieldName() function for speed. ToninhoFwi, 2003-07-22 */

  switch (pField->uiType)
  {
  case Harbour::DB::Field::STRING:
  case Harbour::DB::Field::VARLENGTH:
    u32Length = pArea->maxFieldLen + 1;
    if (!pArea->fPositioned)
    {
      u32RetVal = AE_SUCCESS;
    }
#if ADS_LIB_VERSION >= 1000
    else if ((pField->uiFlags & HB_FF_UNICODE) != 0)
    {
      u32RetVal =
          AdsGetFieldW(pArea->hTable, ADSFIELD(uiIndex), reinterpret_cast<WCHAR *>(pBuffer), &u32Length, ADS_NONE);
      if (u32RetVal == AE_SUCCESS)
      {
        hb_itemPutStrLenU16(pItem, HB_CDP_ENDIAN_LITTLE, reinterpret_cast<const HB_WCHAR *>(pBuffer), u32Length);
        break;
      }
    }
#endif
#ifdef ADS_USE_OEM_TRANSLATION
    else if (hb_ads_bOEM && (pField->uiFlags & HB_FF_BINARY) == 0)
    {
#if ADS_LIB_VERSION >= 600
      u32RetVal = AdsGetFieldRaw(pArea->hTable, ADSFIELD(uiIndex), pBuffer, &u32Length);
      if (u32RetVal == AE_INSUFFICIENT_BUFFER && pField->uiType == Harbour::DB::Field::VARLENGTH)
      {
        auto pucBuf = static_cast<UNSIGNED8 *>(hb_xgrab(u32Length));
        u32RetVal = AdsGetFieldRaw(pArea->hTable, ADSFIELD(uiIndex), pucBuf, &u32Length);
        if (u32RetVal == AE_SUCCESS)
        {
          hb_itemPutCLPtr(pItem, reinterpret_cast<char *>(pucBuf), u32Length);
          break;
        }
        hb_xfree(pucBuf);
      }
#else
      u32RetVal = AdsGetField(pArea->hTable, ADSFIELD(uiIndex), pBuffer, &u32Length, ADS_NONE);
      if (u32RetVal == AE_SUCCESS)
      {
        char *pBufOem = hb_adsAnsiToOem(static_cast<char *>(pBuffer), u32Length);
        hb_itemPutCL(pItem, pBufOem, u32Length);
        hb_adsOemAnsiFree(pBufOem);
        break;
      }
      else if (u32RetVal == AE_INSUFFICIENT_BUFFER && pField->uiType == Harbour::DB::Field::VARLENGTH)
      {
        auto pucBuf = static_cast<UNSIGNED8 *>(hb_xgrab(u32Length));
        u32RetVal = AdsGetField(pArea->hTable, ADSFIELD(uiIndex), pucBuf, &u32Length, ADS_NONE);
        if (u32RetVal == AE_SUCCESS)
        {
          char *pBufOem = hb_adsAnsiToOem(static_cast<char *>(pucBuf), u32Length);
          hb_itemPutCL(pItem, pBufOem, u32Length);
          hb_adsOemAnsiFree(pBufOem);
          hb_xfree(pucBuf);
          break;
        }
        else
        {
          hb_xfree(pucBuf);
        }
      }
#endif
    }
#endif
    else
    {
      u32RetVal = AdsGetField(pArea->hTable, ADSFIELD(uiIndex), pBuffer, &u32Length, ADS_NONE);
      if (u32RetVal == AE_INSUFFICIENT_BUFFER && pField->uiType == Harbour::DB::Field::VARLENGTH)
      {
        auto pucBuf = static_cast<UNSIGNED8 *>(hb_xgrab(u32Length));
        u32RetVal = AdsGetField(pArea->hTable, ADSFIELD(uiIndex), pucBuf, &u32Length, ADS_NONE);
        if (u32RetVal == AE_SUCCESS)
        {
          hb_itemPutCLPtr(pItem, reinterpret_cast<char *>(pucBuf), u32Length);
          break;
        }
        hb_xfree(pucBuf);
      }
    }

    if (!pArea->fPositioned || u32RetVal != AE_SUCCESS)
    {
      if (pField->uiType == Harbour::DB::Field::STRING)
      {
        u32Length = pField->uiLen;
        memset(pBuffer, ' ', u32Length);
      }
      else
      {
        u32Length = 0;
      }
    }
    hb_itemPutCL(pItem, reinterpret_cast<char *>(pBuffer), u32Length);
    break;

  case Harbour::DB::Field::TIME:
  case Harbour::DB::Field::TIMESTAMP:
  case Harbour::DB::Field::MODTIME: {
    SIGNED32 lTime = 0, lDate = 0;
    u32RetVal = AdsGetMilliseconds(pArea->hTable, ADSFIELD(uiIndex), &lTime);
    if (u32RetVal != AE_SUCCESS)
    {
      lTime = 0;
      pArea->area.fEof = true;
    }
    else if (pField->uiType != Harbour::DB::Field::TIME)
    {
      u32RetVal = AdsGetJulian(pArea->hTable, ADSFIELD(uiIndex), &lDate);
      if (u32RetVal != AE_SUCCESS)
      {
        pArea->area.fEof = true;
        lDate = 0;
      }
    }
    hb_itemPutTDT(pItem, lDate, lTime);
    break;
  }
  case Harbour::DB::Field::INTEGER:

#if ADS_LIB_VERSION >= 700
    if (pField->uiTypeExtended == ADS_LONGLONG)
    {
#ifndef HB_LONG_LONG_OFF
      SIGNED64 qVal = 0;
      u32RetVal = AdsGetLongLong(pArea->hTable, ADSFIELD(uiIndex), &qVal);
      if (u32RetVal != AE_SUCCESS)
      {
        qVal = 0;
        pArea->area.fEof = true;
      }
      hb_itemPutNIntLen(pItem, static_cast<HB_MAXINT>(qVal), 20);
#else
      DOUBLE dVal = 0;
      u32RetVal = AdsGetDouble(pArea->hTable, ADSFIELD(uiIndex), &dVal);
      if (u32RetVal != AE_SUCCESS)
      {
        dVal = 0.0;
        pArea->area.fEof = true;
      }
      hb_itemPutNLen(pItem, dVal, 20, 0);
#endif
    }
    else
#endif
    {
      SIGNED32 lVal = 0;
      u32RetVal = AdsGetLong(pArea->hTable, ADSFIELD(uiIndex), &lVal);
      if (u32RetVal != AE_SUCCESS)
      {
        lVal = 0;
        pArea->area.fEof = true;
      }
      if (pField->uiTypeExtended == ADS_SHORTINT)
      {
        hb_itemPutNILen(pItem, static_cast<int>(lVal), 6);
      }
      else
      {
        hb_itemPutNLLen(pItem, static_cast<long>(lVal), 11);
      }
    }
    break;
#if ADS_LIB_VERSION >= 700 && !defined(HB_LONG_LONG_OFF)
  case Harbour::DB::Field::AUTOINC: {
    SIGNED64 qVal = 0;
    u32RetVal = AdsGetLongLong(pArea->hTable, ADSFIELD(uiIndex), &qVal);
    if (u32RetVal != AE_SUCCESS)
    {
      qVal = 0;
      pArea->area.fEof = true;
    }
    hb_itemPutNIntLen(pItem, static_cast<HB_MAXINT>(qVal), 10);
    break;
  }
  case Harbour::DB::Field::ROWVER: {
    SIGNED64 qVal = 0;
    u32RetVal = AdsGetLongLong(pArea->hTable, ADSFIELD(uiIndex), &qVal);
    if (u32RetVal != AE_SUCCESS)
    {
      qVal = 0;
      pArea->area.fEof = true;
    }
    hb_itemPutNIntLen(pItem, static_cast<HB_MAXINT>(qVal), 20);
    break;
  }
#else
  case Harbour::DB::Field::AUTOINC:
  case Harbour::DB::Field::ROWVER: {
    DOUBLE dVal = 0;

    u32RetVal = AdsGetDouble(pArea->hTable, ADSFIELD(uiIndex), &dVal);
    if (u32RetVal != AE_SUCCESS)
    {
      dVal = 0.0;
      pArea->area.fEof = true;
    }
    hb_itemPutNLen(pItem, dVal, pField->uiTypeExtended == Harbour::DB::Field::AUTOINC ? 10 : 20, 0);
    break;
  }
#endif
  case Harbour::DB::Field::LONG:
  case Harbour::DB::Field::DOUBLE:
  case Harbour::DB::Field::CURDOUBLE:
  case Harbour::DB::Field::CURRENCY: {
    DOUBLE dVal = 0;

    u32RetVal = AdsGetDouble(pArea->hTable, ADSFIELD(uiIndex), &dVal);
    if (u32RetVal != AE_SUCCESS)
    {
      dVal = 0.0;
      pArea->area.fEof = true;
    }
#if ADS_LIB_VERSION >= 700
    if (pField->uiTypeExtended == ADS_CURDOUBLE || pField->uiTypeExtended == ADS_DOUBLE ||
        pField->uiTypeExtended == ADS_MONEY)
#else
    if (pField->uiTypeExtended == ADS_CURDOUBLE || pField->uiTypeExtended == ADS_DOUBLE)
#endif
    {
      hb_itemPutNDLen(pItem, dVal, 20 - (pField->uiDec > 0 ? (pField->uiDec + 1) : 0), static_cast<int>(pField->uiDec));
    }
    else if (pField->uiDec)
    {
      hb_itemPutNDLen(pItem, dVal, static_cast<int>(pField->uiLen) - (pField->uiDec + 1),
                      static_cast<int>(pField->uiDec));
    }
    else
    {
      hb_itemPutNLen(pItem, dVal, static_cast<int>(pField->uiLen), 0);
    }
    break;
  }
  case Harbour::DB::Field::DATE: {
    SIGNED32 lDate;

    u32RetVal = AdsGetJulian(pArea->hTable, ADSFIELD(uiIndex), &lDate);
    if (u32RetVal != AE_SUCCESS)
    {
      pArea->area.fEof = true;
      lDate = 0;
    }
    hb_itemPutDL(pItem, lDate);
    break;
  }

  case Harbour::DB::Field::LOGICAL: {
    UNSIGNED16 pbValue = 0;
    u32RetVal = AdsGetLogical(pArea->hTable, ADSFIELD(uiIndex), &pbValue);
    if (u32RetVal != AE_SUCCESS)
    {
      pbValue = 0;
      pArea->area.fEof = true;
    }
    hb_itemPutL(pItem, pbValue != 0);
    break;
  }

  case Harbour::DB::Field::MEMO:
  case Harbour::DB::Field::BLOB:
  case Harbour::DB::Field::IMAGE: {
    UNSIGNED8 *pucBuf;
    UNSIGNED16 u16Type;

    u32RetVal = AdsGetMemoDataType(pArea->hTable, ADSFIELD(uiIndex), &u16Type);
    if (u32RetVal != AE_SUCCESS)
    {
      hb_itemPutC(pItem, nullptr);
    }
    else if (u16Type == ADS_BINARY || u16Type == ADS_IMAGE)
    {
      u32RetVal = AdsGetBinaryLength(pArea->hTable, ADSFIELD(uiIndex), &u32Length);
      if (u32RetVal != AE_SUCCESS || u32Length == 0)
      {
        hb_itemPutC(pItem, nullptr);
      }
      else
      {
        pucBuf = static_cast<UNSIGNED8 *>(hb_xgrab(++u32Length)); /* ++ to make room for NULL */
        u32RetVal = AdsGetBinary(pArea->hTable, ADSFIELD(uiIndex), 0, pucBuf, &u32Length);
        if (u32RetVal != AE_SUCCESS)
        {
          hb_xfree(pucBuf);
          hb_itemPutC(pItem, nullptr);
        }
        else
        {
          hb_itemPutCLPtr(pItem, reinterpret_cast<char *>(pucBuf), u32Length);
        }
      }
    }
    else
    {
      u32RetVal = AdsGetMemoLength(pArea->hTable, ADSFIELD(uiIndex), &u32Length);
      if (u32RetVal != AE_SUCCESS || u32Length == 0)
      {
        hb_itemPutC(pItem, nullptr);
      }
#if ADS_LIB_VERSION >= 1000
      else if ((pField->uiFlags & HB_FF_UNICODE) != 0)
      {
        auto pwBuffer = static_cast<HB_WCHAR *>(hb_xgrab(++u32Length * sizeof(HB_WCHAR)));
        u32RetVal =
            AdsGetStringW(pArea->hTable, ADSFIELD(uiIndex), static_cast<WCHAR *>(pwBuffer), &u32Length, ADS_NONE);
        if (u32RetVal != AE_SUCCESS)
        {
          hb_itemPutC(pItem, nullptr);
        }
        else
        {
          hb_itemPutStrLenU16(pItem, HB_CDP_ENDIAN_LITTLE, pwBuffer, u32Length);
        }
        hb_xfree(pwBuffer);
      }
#endif
      else
      {
        pucBuf = static_cast<UNSIGNED8 *>(hb_xgrab(++u32Length)); /* ++ to make room for NULL */
        u32RetVal = AdsGetString(pArea->hTable, ADSFIELD(uiIndex), pucBuf, &u32Length, ADS_NONE);
        if (u32RetVal != AE_SUCCESS)
        {
          hb_itemPutC(pItem, nullptr);
        }
        else
        {
#ifdef ADS_USE_OEM_TRANSLATION
          char *szRet = hb_adsAnsiToOem(reinterpret_cast<char *>(pucBuf), u32Length);
          hb_itemPutCL(pItem, szRet, u32Length);
          hb_adsOemAnsiFree(szRet);
#else
          hb_itemPutCLPtr(pItem, static_cast<char *>(pucBuf), u32Length);
          pucBuf = nullptr;
#endif
        }
        if (pucBuf)
        {
          hb_xfree(pucBuf);
        }
      }
    }
    hb_itemSetCMemo(pItem);
    break;
  }
  default:
    commonError(pArea, EG_DATATYPE, EDBF_DATATYPE - 900, 0, nullptr, 0, nullptr);
    return Harbour::FAILURE;
  }

  if (u32RetVal == AE_NO_CURRENT_RECORD)
  {
    if (pArea->fPositioned)
    {
      /* It should not happen - something desynchronize WA with ADS,
         update area flags, Druzus */
      hb_adsUpdateAreaFlags(pArea);
    }
  }
  else if (u32RetVal != AE_SUCCESS)
  {
    if (u32RetVal == AE_DATA_TOO_LONG)
    {
      commonError(pArea, EG_DATAWIDTH, EDBF_DATAWIDTH - 900, 0, nullptr, 0, nullptr);
    }
    else
    {
      commonError(pArea, EG_READ, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, 0, nullptr);
    }
    return Harbour::FAILURE;
  }

  return Harbour::SUCCESS;
}

static HB_ERRCODE adsGetVarLen(ADSAREAP pArea, HB_USHORT uiIndex, HB_ULONG *ulLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsGetVarLen(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(ulLen)));
#endif

  LPFIELD pField;

  if (uiIndex > pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  pField = pArea->area.lpFields + uiIndex - 1;
  if (pField->uiType == 'M')
  {
    UNSIGNED32 u32Len;

    /* resolve any pending relations */
    if (pArea->lpdbPendingRel)
    {
      SELF_FORCEREL(&pArea->area);
    }

    if (!pArea->fPositioned)
    {
      *ulLen = 0;
    }
    else if (AdsGetMemoLength(pArea->hTable, ADSFIELD(uiIndex), &u32Len) != AE_SUCCESS)
    {
      /* It should not happen - something desynchronize WA with ADS,
         update area flags, Druzus */
      hb_adsUpdateAreaFlags(pArea);
      *ulLen = 0;
    }
    else
    {
      *ulLen = u32Len;
    }
  }
  else
  {
    *ulLen = pField->uiLen;
  }

#if 0
   if( pField->uiType == 'M' ) {
      *ulLen = (static_cast<LPDBFMEMO>(pField->memo))->uiLen;
   } else {
      *ulLen = pField->uiLen;
   }
#endif

  return Harbour::SUCCESS;
}

static HB_ERRCODE adsGoCold(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsGoCold(%p)", static_cast<void*>(pArea)));
#endif

  if (!pArea->fReadonly)
  {
    AdsWriteRecord(pArea->hTable);
  }

  return Harbour::SUCCESS;
}

#define adsGoHot nullptr

static HB_ERRCODE adsPutRec(ADSAREAP pArea, const HB_BYTE *pBuffer)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsGetRec(%p, %p)", static_cast<void*>(pArea), static_cast<const void*>(pBuffer)));
#endif

  auto u32Len = static_cast<UNSIGNED32>(pArea->ulRecordLen);
  UNSIGNED32 u32Result;

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  if (!pArea->fPositioned)
  {
    return Harbour::SUCCESS;
  }

  if (hb_ads_bTestRecLocks)
  {
    if (hb_adsCheckLock(pArea) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  u32Result = AdsSetRecord(pArea->hTable, static_cast<UNSIGNED8 *>(const_cast<HB_BYTE *>(pBuffer)), u32Len);

  return u32Result == AE_SUCCESS ? Harbour::SUCCESS : Harbour::FAILURE;
}

static HB_ERRCODE adsPutValue(ADSAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsPutValue(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pItem)));
#endif

  LPFIELD pField;
  HB_SIZE nLen;
  HB_BYTE *szText;
  bool bTypeError = true;
  UNSIGNED32 u32RetVal = 0;

  if (!uiIndex || uiIndex > pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  /* 2003-10-30 15:54

     ADS has Implicit Record locking that can mask programming errors.
     Implicit locking can occur the first time a value is written to a
     field with no lock in effect. The lock can potentially remain in
     effect indefinitely if the record pointer is not moved.

     The hb_ads_bTestRecLocks flag can be set using
         AdsTestRecLocks(lOnOff) in adsfunc.c.
     If ON, we see if the file is open exclusively or locked, and whether
     the record has been explicitly locked already. If not, we throw
     an error so the developer can catch the missing lock condition.
     For performance reasons, Release code should leave this OFF.
        Although the call to AdsIsRecordLocked is documented as a client
        call, not a server request, and should be fast, it will be
        called for EACH FIELD as it is assigned a value. */

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  if (!pArea->fPositioned)
  {
    return Harbour::SUCCESS;
  }

  if (hb_ads_bTestRecLocks)
  {
    if (hb_adsCheckLock(pArea) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  pField = pArea->area.lpFields + uiIndex - 1;
  szText = pArea->pRecord;

  /* This code was optimized for use ADSFIELD() macro instead */
  /* AdsGetFieldName() function for speed. ToninhoFwi, 2003-07-22 */

  switch (pField->uiType)
  {
  case Harbour::DB::Field::STRING:
  case Harbour::DB::Field::VARLENGTH:
    if (pItem->isString())
    {
      bTypeError = false;
#if ADS_LIB_VERSION >= 1000
      if ((pField->uiFlags & HB_FF_UNICODE) != 0)
      {
        void *hString;
        const HB_WCHAR *pwBuffer = hb_itemGetStrU16(pItem, HB_CDP_ENDIAN_LITTLE, &hString, &nLen);
        if (nLen > static_cast<HB_SIZE>(pField->uiLen) && pField->uiType == Harbour::DB::Field::STRING)
        {
          nLen = pField->uiLen;
        }
        u32RetVal =
            AdsSetStringW(pArea->hTable, ADSFIELD(uiIndex), static_cast<WCHAR *>(const_cast<HB_WCHAR *>(pwBuffer)),
                          static_cast<UNSIGNED32>(nLen));
        hb_strfree(hString);
      }
      else
#endif
      {
        nLen = hb_itemGetCLen(pItem);
        if (nLen > static_cast<HB_SIZE>(pField->uiLen))
        {
#if ADS_LIB_VERSION >= 900
          if (pField->uiType == Harbour::DB::Field::STRING || pArea->iFileType == ADS_VFP)
#else
          if (pField->uiType == Harbour::DB::Field::STRING)
#endif
          {
            nLen = pField->uiLen;
          }
          else if (nLen > 64000)
          {
            /* maximum VarChar field size is 64000 */
            nLen = 64000;
          }
        }
#ifdef ADS_USE_OEM_TRANSLATION
        if (hb_ads_bOEM)
        {
#if ADS_LIB_VERSION >= 600
          u32RetVal = AdsSetFieldRaw(pArea->hTable, ADSFIELD(uiIndex),
                                     reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(pItem))),
                                     static_cast<UNSIGNED32>(nLen));
#else
          char *pBuffer = hb_adsOemToAnsi(hb_itemGetCPtr(pItem), nLen);
          u32RetVal = AdsSetString(pArea->hTable, ADSFIELD(uiIndex), static_cast<UNSIGNED8 *>(pBuffer), nLen);
          hb_adsOemAnsiFree(pBuffer);
#endif
        }
        else
#endif
        {
          u32RetVal = AdsSetString(pArea->hTable, ADSFIELD(uiIndex),
                                   reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(pItem))),
                                   static_cast<UNSIGNED32>(nLen));
        }
      }
      /* varchar unicode fields in ADT tables and varchar/varbinary
         fields in VFP DBFs have fixed size so we should ignore this
         error when longer data is passed [druzus] */
      if (u32RetVal == AE_DATA_TRUNCATED)
      {
        u32RetVal = AE_SUCCESS;
      }
    }
    break;

  case Harbour::DB::Field::ROWVER:
  case Harbour::DB::Field::AUTOINC:
  case Harbour::DB::Field::INTEGER:
#if ADS_LIB_VERSION >= 700 && !defined(HB_LONG_LONG_OFF)
    if (pItem->isNumeric())
    {
      bTypeError = false;
      u32RetVal = AdsSetLongLong(pArea->hTable, ADSFIELD(uiIndex), hb_itemGetNInt(pItem));
      /* write to auto-increment field will generate error 5066 */
    }
    break;
#endif
  case Harbour::DB::Field::LONG:
  case Harbour::DB::Field::DOUBLE:
  case Harbour::DB::Field::CURDOUBLE:
  case Harbour::DB::Field::CURRENCY:
    if (pItem->isNumeric())
    {
      bTypeError = false;
      u32RetVal = AdsSetDouble(pArea->hTable, ADSFIELD(uiIndex), hb_itemGetND(pItem));
      /* write to auto-increment field will generate error 5066
         #if HB_TR_LEVEL >= HB_TR_DEBUG
            if( pField->uiTypeExtended == ADS_AUTOINC )
               HB_TRACE(HB_TR_INFO, ("adsPutValue() error"));
         #endif
       */
    }
    break;

  case Harbour::DB::Field::TIME:
  case Harbour::DB::Field::TIMESTAMP:
  case Harbour::DB::Field::MODTIME:
    if (pItem->isDateTime())
    {
      long lDate, lTime;
      bTypeError = false;
      hb_itemGetTDT(pItem, &lDate, &lTime);
      u32RetVal = AdsSetMilliseconds(pArea->hTable, ADSFIELD(uiIndex), lTime);
      if (u32RetVal == AE_SUCCESS && pField->uiType != Harbour::DB::Field::TIME)
      {
        u32RetVal = AdsSetJulian(pArea->hTable, ADSFIELD(uiIndex), lDate);
      }
    }
    break;

  case Harbour::DB::Field::DATE:
    if (pItem->isDateTime())
    {
      long lDate = hb_itemGetDL(pItem);

      /* ADS does not support dates before 0001-01-01. It generates corrupted
         DBF records and fires ADS error 5095 on FieldGet() later. [Mindaugas] */
      if (pField->uiLen != 4 && lDate < 1721426)
      { /* 1721426 ~= 0001-01-01 */
        lDate = 0;
      }

      bTypeError = false;
      u32RetVal = AdsSetJulian(pArea->hTable, ADSFIELD(uiIndex), lDate);
    }
    break;

  case Harbour::DB::Field::LOGICAL:
    if (pItem->isLogical())
    {
      bTypeError = false;
      *szText = hb_itemGetL(pItem) ? 'T' : 'F';
      u32RetVal = AdsSetLogical(pArea->hTable, ADSFIELD(uiIndex), static_cast<UNSIGNED16>(hb_itemGetL(pItem)));
    }
    break;

  case Harbour::DB::Field::MEMO:
  case Harbour::DB::Field::BLOB:
  case Harbour::DB::Field::IMAGE:
    if (pItem->isString())
    {
      bTypeError = false;
      nLen = hb_itemGetCLen(pItem);

      /* ToninhoFwi - 2006-12-09 - In the previous code nLen was limited to 0xFFFF
         so, I comment it, because ADS support up to 4 GiB in memo/binary/image fields.
         Advantage documentations says that we need use AdsSetBinary in binary/image
         fields. I tested these special fields with AdsSetString() and it works, but
         is a little bit slower to save big image file in the fields, so I keep
         AdsSetString() only for common memo fields and AdsSetBinary() for the others.
       */
      if (pField->uiTypeExtended == ADS_BINARY || pField->uiTypeExtended == ADS_IMAGE)
      {
        u32RetVal = AdsSetBinary(
            pArea->hTable, ADSFIELD(uiIndex), pField->uiTypeExtended, static_cast<UNSIGNED32>(nLen), 0,
            reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(pItem))), static_cast<UNSIGNED32>(nLen));
      }
#if ADS_LIB_VERSION >= 1000
      else if ((pField->uiFlags & HB_FF_UNICODE) != 0)
      {
        void *hString;
        const HB_WCHAR *pwBuffer = hb_itemGetStrU16(pItem, HB_CDP_ENDIAN_LITTLE, &hString, &nLen);
        u32RetVal =
            AdsSetStringW(pArea->hTable, ADSFIELD(uiIndex), static_cast<WCHAR *>(const_cast<HB_WCHAR *>(pwBuffer)),
                          static_cast<UNSIGNED32>(nLen));
        hb_strfree(hString);
      }
#endif
      else
      {
#ifdef ADS_USE_OEM_TRANSLATION
        char *szRet = hb_adsOemToAnsi(hb_itemGetCPtr(pItem), nLen);
        u32RetVal = AdsSetString(pArea->hTable, ADSFIELD(uiIndex), reinterpret_cast<UNSIGNED8 *>(szRet),
                                 static_cast<UNSIGNED32>(nLen));
        hb_adsOemAnsiFree(szRet);
#else
        u32RetVal = AdsSetString(pArea->hTable, ADSFIELD(uiIndex),
                                 static_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(pItem))),
                                 static_cast<UNSIGNED32>(nLen));
#endif
      }
    }
    break;
  }

  if (bTypeError)
  {
    commonError(pArea, EG_DATATYPE, EDBF_DATATYPE - 900, 0, nullptr, 0, nullptr);
    return Harbour::FAILURE;
  }

  if (u32RetVal != AE_SUCCESS)
  {
    if (u32RetVal == AE_LOCK_FAILED || u32RetVal == AE_RECORD_NOT_LOCKED)
    {
      commonError(pArea, EG_UNLOCKED, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, 0, nullptr);
    }
    else if (u32RetVal == AE_TABLE_READONLY)
    {
      commonError(pArea, EG_READONLY, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, 0, nullptr);
    }
#ifdef AE_VALUE_OVERFLOW /* ADS_LIB_VERSION >= 700 */
    else if (u32RetVal == AE_DATA_TOO_LONG || u32RetVal == AE_VALUE_OVERFLOW)
#else
    else if (u32RetVal == AE_DATA_TOO_LONG)
#endif
    {
      return commonError(pArea, EG_DATAWIDTH, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, EF_CANDEFAULT, nullptr) ==
                     E_DEFAULT
                 ? Harbour::SUCCESS
                 : Harbour::FAILURE;
    }
    else
    {
      commonError(pArea, EG_WRITE, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, 0, nullptr);
    }
    return Harbour::FAILURE;
  }

  return Harbour::SUCCESS;
}

static HB_ERRCODE adsRecall(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsRecall(%p)", static_cast<void*>(pArea)));
#endif

  UNSIGNED32 u32RetVal;

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  if (!pArea->fPositioned)
  {
    return Harbour::SUCCESS;
  }

  if (hb_ads_bTestRecLocks)
  {
    if (hb_adsCheckLock(pArea) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  u32RetVal = AdsRecallRecord(pArea->hTable);

  return u32RetVal == AE_SUCCESS ? Harbour::SUCCESS : Harbour::FAILURE;
}

static HB_ERRCODE adsRecCount(ADSAREAP pArea, HB_ULONG *pRecCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsRecCount(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pRecCount)));
#endif

  UNSIGNED32 u32RecCount = 0, u32Result;

  u32Result = AdsGetRecordCount(pArea->hTable, ADS_IGNOREFILTERS | ADS_REFRESHCOUNT, &u32RecCount);
  *pRecCount = static_cast<HB_ULONG>(u32RecCount);

  return u32Result == AE_SUCCESS ? Harbour::SUCCESS : Harbour::FAILURE;
}

static HB_ERRCODE adsRecInfo(ADSAREAP pArea, PHB_ITEM pRecID, HB_USHORT uiInfoType, PHB_ITEM pInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsRecInfo(%p, %p, %hu, %p)", static_cast<void*>(pArea), static_cast<void*>(pRecID), uiInfoType, static_cast<void*>(pInfo)));
#endif

  HB_ULONG ulRecNo = hb_itemGetNL(pRecID);
  HB_ERRCODE uiRetVal = Harbour::SUCCESS;

  switch (uiInfoType)
  {
  case DBRI_DELETED: {
    HB_BOOL fDeleted = false;
    HB_ULONG ulCurrRec = 0;

    if (ulRecNo != 0)
    {
      SELF_RECNO(&pArea->area, &ulCurrRec);
      if (ulCurrRec == ulRecNo)
      {
        ulCurrRec = 0;
      }
      else
      {
        SELF_GOTO(&pArea->area, ulRecNo);
      }
    }
    uiRetVal = SELF_DELETED(&pArea->area, &fDeleted);
    if (ulCurrRec != 0)
    {
      SELF_GOTO(&pArea->area, ulCurrRec);
    }
    hb_itemPutL(pInfo, fDeleted);
    break;
  }
  case DBRI_LOCKED: {
    UNSIGNED16 u16Locked = 0;

    if (ulRecNo == 0)
    {
      uiRetVal = SELF_RECNO(&pArea->area, &ulRecNo);
    }

    if (AdsIsRecordLocked(pArea->hTable, ulRecNo, &u16Locked) != AE_SUCCESS)
    {
      uiRetVal = Harbour::FAILURE;
    }
    hb_itemPutL(pInfo, u16Locked != 0);
    break;
  }
  case DBRI_RECSIZE:
    hb_itemPutNL(pInfo, pArea->ulRecordLen);
    break;

  case DBRI_RECNO:
    if (ulRecNo == 0)
    {
      uiRetVal = SELF_RECNO(&pArea->area, &ulRecNo);
    }
    hb_itemPutNL(pInfo, ulRecNo);
    break;

  case DBRI_UPDATED:
    /* TODO: this will not work properly with current ADS RDD */
    hb_itemPutL(pInfo, false);
    break;

  default:
    return SUPER_RECINFO(&pArea->area, pRecID, uiInfoType, pInfo);
  }
  return uiRetVal;
}

static HB_ERRCODE adsRecNo(ADSAREAP pArea, HB_ULONG *ulRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsRecNo(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(ulRecNo)));
#endif

  UNSIGNED32 u32RecNo, u32Result;

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  u32Result = AdsGetRecordNum(pArea->hTable, ADS_IGNOREFILTERS, &u32RecNo);
  if (u32RecNo != 0 && u32Result == AE_SUCCESS)
  {
    pArea->ulRecNo = u32RecNo;
  }

  *ulRecNo = pArea->ulRecNo;

  return u32Result == AE_SUCCESS ? Harbour::SUCCESS : Harbour::FAILURE;
}

static HB_ERRCODE adsRecId(ADSAREAP pArea, PHB_ITEM pRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsRecId(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pRecNo)));
#endif

  HB_ERRCODE errCode;
  HB_ULONG ulRecNo;

  errCode = SELF_RECNO(&pArea->area, &ulRecNo);
  hb_itemPutNL(pRecNo, ulRecNo);
  return errCode;
}

#define adsSetFieldExtent nullptr
#define adsAlias nullptr

static HB_ERRCODE adsClose(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsClose(%p)", static_cast<void*>(pArea)));
#endif

  if (pArea->szQuery)
  {
    hb_xfree(pArea->szQuery);
    pArea->szQuery = nullptr;
  }
  return hb_adsCloseCursor(pArea);
}

static HB_ERRCODE adsCreate(ADSAREAP pArea, LPDBOPENINFO pCreateInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsCreate(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pCreateInfo)));
#endif

  ADSHANDLE hTable, hConnection;
  UNSIGNED32 uRetVal, u32Length, uiFldLen, uiLen;
  UNSIGNED8 *ucfieldPtr;
  char szBuffer[MAX_STR_LEN + 1];
  HB_USHORT uiCount;
  LPFIELD pField;
  bool fUnicode;

  hConnection = HB_ADS_DEFCONNECTION(pCreateInfo->ulConnection, pCreateInfo->abName);

  pArea->szDataFileName = hb_strdup(pCreateInfo->abName);

  fUnicode = false;
  pArea->maxFieldLen = 0;

  /* uiLen = length of buffer for all field definition info times number of fields.
     For extended types cType may be up to 6 chars, and
     there are up to 4 punctuation symbols ( ,; ),
     field length (8) and number of decimals (2).
     So, per field it should be  (6 + 4 + 8 + 2 = 20):
        uiMaxFieldNameLength + 20

   * 2006-09-19 BH: this is an oversized buffer since most fields don't have
   * 128-byte names. But the overhead in counting up the bytes is worse than
   * allocating a bigger buffer. We need to make sure it's not too big, though.
   * ADS docs say max # of fields is fnameLen + 10
   *     65135 / (10 + AverageFieldNameLength)
   */

  uiLen = static_cast<UNSIGNED32>(pArea->area.uiFieldCount) * (pArea->area.uiMaxFieldNameLength + 20) + 1;
  if (uiLen > 65135)
  {
    uiLen = 65135;
  }

  auto ucfieldDefs = static_cast<UNSIGNED8 *>(hb_xgrab(uiLen));
  ucfieldDefs[0] = '\0';
  ucfieldPtr = ucfieldDefs;

  pField = pArea->area.lpFields;
  for (uiCount = 0; uiCount < pArea->area.uiFieldCount; uiCount++)
  {
    const char *cType;

    if (static_cast<HB_ULONG>(pField->uiLen) > pArea->maxFieldLen)
    {
      pArea->maxFieldLen = pField->uiLen;
    }

    cType = nullptr;
    switch (pField->uiType)
    {
    case Harbour::DB::Field::STRING:
      if (pField->uiFlags & HB_FF_BINARY || pField->uiTypeExtended == ADS_RAW)
      {
        cType = "Raw";
      }
      else if (pField->uiFlags & HB_FF_UNICODE)
      {
        cType = "NChar";
        fUnicode = true;
      }
#if ADS_LIB_VERSION >= 710
      else if (pField->uiTypeExtended == ADS_CISTRING)
      {
        cType = "CICharacter";
      }
#endif
      else
      {
        cType = "Character";
      }
      break;

    case Harbour::DB::Field::LOGICAL:
      cType = "Logical";
      break;

    case Harbour::DB::Field::DATE:
      if (pField->uiTypeExtended == ADS_COMPACTDATE)
      {
        cType = "ShortDate";
      }
      else
      {
        cType = "Date";
      }
      break;

    case Harbour::DB::Field::LONG:
      cType = "Numeric";
      break;

    case Harbour::DB::Field::INTEGER:
      if (pField->uiTypeExtended == ADS_SHORTINT)
      {
        cType = "ShortInt";
      }
#if ADS_LIB_VERSION >= 700
      else if (pField->uiTypeExtended == ADS_LONGLONG)
      {
        cType = "Longlong";
      }
#endif
      else
      {
        cType = "Integer";
      }
      break;

    case Harbour::DB::Field::DOUBLE:
      cType = "Double";
      break;

    case Harbour::DB::Field::TIME:
      cType = "Time";
      break;

    case Harbour::DB::Field::TIMESTAMP:
      cType = "TimeStamp";
      break;

    case Harbour::DB::Field::MODTIME:
      cType = "ModTime";
      break;

    case Harbour::DB::Field::ROWVER:
      cType = "RowVersion";
      break;

    case Harbour::DB::Field::AUTOINC:
      cType = "Autoinc";
      break;

    case Harbour::DB::Field::CURRENCY:
      cType = "Money";
      break;

    case Harbour::DB::Field::CURDOUBLE:
      cType = "CurDouble";
      break;

    case Harbour::DB::Field::VARLENGTH:
      if (pField->uiFlags & HB_FF_UNICODE)
      {
        fUnicode = true;
        cType = "NVarChar";
      }
#if ADS_LIB_VERSION >= 900
      else if (pField->uiTypeExtended == ADS_VARCHAR_FOX)
      {
        cType = "VarCharFox";
      }
      else if (pField->uiTypeExtended == ADS_VARBINARY_FOX)
      {
        cType = "VarBinaryFox";
      }
#endif
      else if (pField->uiFlags & HB_FF_BINARY)
      {
        cType = "VarBinary";
      }
      else
      {
        cType = "VarChar";
      }
      break;

    case Harbour::DB::Field::MEMO:
      if (pField->uiFlags & HB_FF_UNICODE)
      {
        cType = "NMemo";
        fUnicode = true;
      }
      else
      {
        cType = "Memo";
      }
      break;

    case Harbour::DB::Field::BLOB:
      cType = "Binary";
      break;

    case Harbour::DB::Field::IMAGE:
      cType = "Image";
      break;
    }

    if (cType == nullptr)
    {
      hb_xfree(ucfieldDefs);
      return Harbour::FAILURE; /* RT_ERROR */
    }

    switch (pField->uiType)
    {
    case Harbour::DB::Field::LOGICAL:
    case Harbour::DB::Field::DATE:
    case Harbour::DB::Field::TIME:
    case Harbour::DB::Field::TIMESTAMP:
    case Harbour::DB::Field::MODTIME:
    case Harbour::DB::Field::ROWVER:
    case Harbour::DB::Field::AUTOINC:
    case Harbour::DB::Field::IMAGE:
    case Harbour::DB::Field::BLOB:
      uiFldLen = hb_snprintf(szBuffer, sizeof(szBuffer), "%.*s,%s;", static_cast<int>(pArea->area.uiMaxFieldNameLength),
                             hb_dynsymName(static_cast<PHB_DYNS>(pField->sym)), cType);
      break;

    case Harbour::DB::Field::STRING:
    case Harbour::DB::Field::INTEGER:
    case Harbour::DB::Field::MEMO:
    case Harbour::DB::Field::VARLENGTH:
      uiFldLen =
          hb_snprintf(szBuffer, sizeof(szBuffer), "%.*s,%s,%d;", static_cast<int>(pArea->area.uiMaxFieldNameLength),
                      hb_dynsymName(static_cast<PHB_DYNS>(pField->sym)), cType, pField->uiLen);
      break;

    default:
      uiFldLen =
          hb_snprintf(szBuffer, sizeof(szBuffer), "%.*s,%s,%d,%d;", static_cast<int>(pArea->area.uiMaxFieldNameLength),
                      hb_dynsymName(static_cast<PHB_DYNS>(pField->sym)), cType, pField->uiLen, pField->uiDec);
      break;
    }

    if (uiFldLen >= uiLen)
    {
      hb_xfree(ucfieldDefs);
      /* RT_ERROR; probably too many fields */
      return Harbour::FAILURE;
    }
    memcpy(ucfieldPtr, szBuffer, uiFldLen);
    uiLen -= uiFldLen;
    ucfieldPtr += uiFldLen;

    pField++;
  }
  *ucfieldPtr = '\0';

  if (fUnicode)
  {
    pArea->maxFieldLen <<= 1;
  }
  if (pArea->maxFieldLen < 24)
  {
    pArea->maxFieldLen = 24;
  }

  uRetVal = AdsCreateTable(hConnection, static_cast<UNSIGNED8 *>(HB_UNCONST(pCreateInfo->abName)),
                           static_cast<UNSIGNED8 *>(HB_UNCONST(pCreateInfo->atomAlias)),
                           static_cast<UNSIGNED16>(pArea->iFileType), static_cast<UNSIGNED16>(hb_ads_iCharType),
                           static_cast<UNSIGNED16>(hb_ads_iLockType), static_cast<UNSIGNED16>(hb_ads_iCheckRights),
                           static_cast<UNSIGNED16>(hb_setGetNI(HB_SET_MBLOCKSIZE)), ucfieldDefs, &hTable);
  hb_xfree(ucfieldDefs);

  if (uRetVal != AE_SUCCESS)
  {
#if 0
      HB_TRACE(HB_TR_INFO, ("adsCreate() error"));
#endif
    commonError(pArea, EG_CREATE, static_cast<HB_ERRCODE>(uRetVal), 0, pCreateInfo->abName, 0, nullptr);
    return Harbour::FAILURE;
  }
  /*
   * In Clipper CREATE() keeps database open on success [druzus]
   */
  pArea->hTable = hTable;
  pArea->fShared = false;   /* pCreateInfo->fShared; */
  pArea->fReadonly = false; /* pCreateInfo->fReadonly */

  /* If successful call SUPER_CREATE to finish system jobs */
  if (SUPER_CREATE(&pArea->area, pCreateInfo) != Harbour::SUCCESS)
  {
    SELF_CLOSE(&pArea->area);
    return Harbour::FAILURE;
  }

  AdsGetRecordLength(pArea->hTable, &u32Length);
  pArea->ulRecordLen = u32Length;
  /* Alloc record buffer - because it's also used for some extended types
     conversion it has to be at least 25 bytes size */
  pArea->pRecord = static_cast<HB_BYTE *>(hb_xgrab(HB_MAX(pArea->ulRecordLen, pArea->maxFieldLen) + 1));

  return SELF_GOTOP(&pArea->area);
}

static HB_ERRCODE adsInfo(ADSAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsInfo(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pItem)));
#endif

  UNSIGNED32 uRetVal;

  switch (uiIndex)
  {
  case DBI_ISDBF:
  case DBI_CANPUTREC:
    hb_itemPutL(pItem, true);
    break;

  case DBI_GETHEADERSIZE:
    hb_itemPutNL(pItem, pArea->iFileType == ADS_ADT ? 0 : 32 + pArea->area.uiFieldCount * 32 + 2);
    break;

  case DBI_LASTUPDATE: {
    UNSIGNED8 pucFormat[11];
    UNSIGNED16 pusLen = 11;
    UNSIGNED8 pucDate[11];

    AdsGetDateFormat(pucFormat, &pusLen);
    AdsSetDateFormat(reinterpret_cast<UNSIGNED8 *>(const_cast<char *>("YYYYMMDD")));
    AdsGetLastTableUpdate(pArea->hTable, pucDate, &pusLen);
    *(pucDate + 8) = '\0';
    hb_itemPutDS(pItem, reinterpret_cast<char *>(pucDate));
    AdsSetDateFormat(pucFormat);
    break;
  }
  case DBI_GETRECSIZE:
    hb_itemPutNL(pItem, pArea->ulRecordLen);
    break;

  case DBI_GETLOCKARRAY: {
    UNSIGNED16 u16Count;
    uRetVal = AdsGetNumLocks(pArea->hTable, &u16Count);
    if (uRetVal != AE_SUCCESS)
    {
      return Harbour::FAILURE;
    }

    if (u16Count)
    {
      auto puLocks = static_cast<UNSIGNED32 *>(hb_xgrab((u16Count + 1) * sizeof(UNSIGNED32)));
      AdsGetAllLocks(pArea->hTable, puLocks, &u16Count);

      if (u16Count)
      {
        hb_arrayNew(pItem, u16Count);
        for (UNSIGNED16 u16 = 0; u16 < u16Count; ++u16)
        {
          hb_itemPutNL(hb_arrayGetItemPtr(pItem, static_cast<HB_ULONG>(u16) + 1), puLocks[u16]);
        }
      }
      hb_xfree(puLocks);
    }
    if (!u16Count)
    {
      hb_arrayNew(pItem, 0); /* don't return nil */
    }

    break;
  }

  case DBI_TABLEEXT:
    hb_itemPutC(pItem, adsTableExt(pArea->iFileType));
    break;

  case DBI_FULLPATH: {
    UNSIGNED8 aucBuffer[MAX_STR_LEN + 1];
    UNSIGNED16 pusLen = MAX_STR_LEN;
    AdsGetTableFilename(pArea->hTable, ADS_FULLPATHNAME, aucBuffer, &pusLen);
    hb_itemPutCL(pItem, reinterpret_cast<char *>(aucBuffer), pusLen);
    break;
  }

  case DBI_ISFLOCK:
    hb_itemPutL(pItem, pArea->fFLocked);
    break;

  case DBI_ISREADONLY:
    hb_itemPutL(pItem, pArea->fReadonly);
    break;

  case DBI_POSITIONED:
    hb_itemPutL(pItem, pArea->fPositioned);
    break;

  case DBI_LOCKCOUNT: {
    UNSIGNED16 u16Count;
    uRetVal = AdsGetNumLocks(pArea->hTable, &u16Count);
    if (uRetVal != AE_SUCCESS)
    {
      return Harbour::FAILURE;
    }

    hb_itemPutNL(pItem, static_cast<long>(u16Count));
    break;
  }

  case DBI_SHARED:
    hb_itemPutL(pItem, pArea->fShared);
    break;

  case DBI_MEMOEXT:
    hb_itemPutC(pItem, adsMemoExt(pArea->iFileType));
    break;

  case DBI_DB_VERSION: { /* HOST driver Version */
    UNSIGNED32 ulMajor;
    UNSIGNED32 ulMinor;
    UNSIGNED8 ucLetter;
    UNSIGNED8 ucDesc[128];
    UNSIGNED16 usDescLen = sizeof(ucDesc) - 1;
    char szVersion[256];

    AdsGetVersion(&ulMajor, &ulMinor, &ucLetter, ucDesc, &usDescLen);

    hb_snprintf(szVersion, sizeof(szVersion), "%s, v%lu.%lu%c", reinterpret_cast<char *>(ucDesc),
                static_cast<HB_ULONG>(ulMajor), static_cast<HB_ULONG>(ulMinor), ucLetter);
    hb_itemPutC(pItem, szVersion);
    break;
  }

  case DBI_RDD_VERSION: /* RDD version (current RDD) */
    hb_itemPutC(pItem, HB_RDD_ADS_VERSION_STRING);
    break;

  /* unsupported options */
  case DBI_FILEHANDLE:    /* Handle of opened file */
  case DBI_VALIDBUFFER:   /* Is the current buffer valid */
  case DBI_GETSCOPE:      /* Locate codeblock */
  case DBI_LOCKOFFSET:    /* New locking offset */
  case DBI_MEMOHANDLE:    /* Dos handle for memo file */
  case DBI_MEMOBLOCKSIZE: /* Blocksize in memo files */
    break;

  /* use workarea.c implementation */
  default:
    return SUPER_INFO(&pArea->area, uiIndex, pItem);
  }
  return Harbour::SUCCESS;
}

static HB_ERRCODE adsNewArea(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsNewArea(%p)", static_cast<void*>(pArea)));
#endif

  HB_ERRCODE errCode;

  errCode = SUPER_NEW(&pArea->area);
  if (errCode == Harbour::SUCCESS)
  {
    switch (adsGetRddType(pArea->area.rddID))
    {
    case ADS_NTX:
      pArea->iFileType = ADS_NTX;
      pArea->area.uiMaxFieldNameLength = ADS_MAX_DBF_FIELD_NAME;
      break;
    case ADS_CDX:
      pArea->iFileType = ADS_CDX;
      pArea->area.uiMaxFieldNameLength = ADS_MAX_DBF_FIELD_NAME;
      break;
    case ADS_ADT:
      pArea->iFileType = ADS_ADT;
      pArea->area.uiMaxFieldNameLength = ADS_MAX_FIELD_NAME;
      break;
#if ADS_LIB_VERSION >= 900
    case ADS_VFP:
      pArea->iFileType = ADS_VFP;
      pArea->area.uiMaxFieldNameLength = ADS_MAX_DBF_FIELD_NAME;
      break;
#endif
    default: /* ADS_DEFAULT */
      pArea->iFileType = hb_ads_iFileType;
      pArea->area.uiMaxFieldNameLength = (pArea->iFileType == ADS_ADT) ? ADS_MAX_FIELD_NAME : ADS_MAX_DBF_FIELD_NAME;
      break;
    }
  }
  return errCode;
}

static HB_ERRCODE adsOpen(ADSAREAP pArea, LPDBOPENINFO pOpenInfo)
{
  ADSHANDLE hTable = 0, hStatement = 0, hConnection;
  UNSIGNED32 u32RetVal, u32Length;
  HB_USHORT uiFields = 0, uiCount;
  UNSIGNED8 szName[ADS_MAX_FIELD_NAME + 1];
  /* See adsGettValue() for why we don't use pArea->area.uiMaxFieldNameLength here */
  UNSIGNED16 usBufLen, usType, usDecimals;
  DBFIELDINFO dbFieldInfo{};
  char szAlias[HB_RDD_MAX_ALIAS_LEN + 1];
  const char *szFile;
  bool fDictionary = false, fUnicode = false;

#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsOpen(%p)", static_cast<void*>(pArea)));
#endif

  hConnection = HB_ADS_DEFCONNECTION(pOpenInfo->ulConnection, pOpenInfo->abName);
  u32RetVal = AdsGetHandleType(hConnection, &usType);
  if (u32RetVal == AE_SUCCESS)
  {
#if ADS_LIB_VERSION >= 600 /* ADS_*_CONNECTION was added in >= 6.00 */
#if ADS_LIB_VERSION < 900  /* ADS_SYS_ADMIN_CONNECTION was removed in >= 9.00 */
    fDictionary = (usType == ADS_DATABASE_CONNECTION || usType == ADS_SYS_ADMIN_CONNECTION);
#else
    fDictionary = (usType == ADS_DATABASE_CONNECTION);
#endif
#endif
  }
  szFile = pOpenInfo->abName;

  if (pArea->hTable != 0)
  {
    /*
     * table was open by AdsExecuteSQL[Direct]() function
     * I do not like the way it was implemented but I also
     * do not have time to change it so I simply restored this
     * functionality, Druzus.
     */
    hTable = pArea->hTable;
    hStatement = pArea->hStatement;
  }
  else if (szFile && ((hb_strnicmp(szFile, "SELECT ", 7) == 0) || (hb_strnicmp(szFile, "SQL:", 4) == 0)))
  {
    if (hb_strnicmp(szFile, "SQL:", 4) == 0)
    {
      szFile += 4;
    }

    pArea->szQuery = hb_strdup(szFile);

    u32RetVal = AdsCreateSQLStatement(hConnection, &hStatement);
    if (u32RetVal == AE_SUCCESS)
    {
#ifdef ADS_USE_OEM_TRANSLATION
      char *szSQL = hb_adsOemToAnsi(szFile, strlen(szFile));
#endif
#if ADS_LIB_VERSION >= 900
      if (pArea->iFileType == ADS_CDX || pArea->iFileType == ADS_VFP)
#else
      if (pArea->iFileType == ADS_CDX)
#endif
      {
        AdsStmtSetTableType(hStatement, static_cast<UNSIGNED16>(pArea->iFileType));
      }
#ifdef ADS_USE_OEM_TRANSLATION
      u32RetVal = AdsExecuteSQLDirect(hStatement, reinterpret_cast<UNSIGNED8 *>(szSQL), &hTable);
      hb_adsOemAnsiFree(szSQL);
#else
      u32RetVal = AdsExecuteSQLDirect(hStatement, static_cast<UNSIGNED8 *>(const_cast<char *>(szFile)), &hTable);
#endif

      if (u32RetVal != AE_SUCCESS)
      {
        commonError(pArea, EG_OPEN, static_cast<HB_ERRCODE>(u32RetVal), 0, pOpenInfo->abName, 0, nullptr);
        AdsCloseSQLStatement(hStatement);
        return Harbour::FAILURE;
      }
    }
    else
    {
      commonError(pArea, EG_OPEN, static_cast<HB_ERRCODE>(u32RetVal), 0, pOpenInfo->abName, 0, nullptr);
      return Harbour::FAILURE;
    }
  }
  else
  { /* if( hb_strnicmp(szFile, "TABLE:", 6) == 0 ) */
    PHB_ITEM pError = nullptr;
    bool fRetry;

    if (szFile && (hb_strnicmp(szFile, "TABLE:", 6) == 0))
    {
      szFile += 6;
    }

    /* Use an  Advantage Data Dictionary
     * if fDictionary was set for this connection
     */
    do
    {
      u32RetVal = AdsOpenTable(hConnection, reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(szFile)),
                               static_cast<UNSIGNED8 *>(HB_UNCONST(pOpenInfo->atomAlias)),
                               (fDictionary ? ADS_DEFAULT : static_cast<UNSIGNED16>(pArea->iFileType)),
                               static_cast<UNSIGNED16>(hb_ads_iCharType), static_cast<UNSIGNED16>(hb_ads_iLockType),
                               static_cast<UNSIGNED16>(hb_ads_iCheckRights),
                               (pOpenInfo->fShared ? ADS_SHARED : ADS_EXCLUSIVE) |
                                   (pOpenInfo->fReadonly ? ADS_READONLY : ADS_DEFAULT),
                               &hTable);
      if (u32RetVal != AE_SUCCESS)
      {
        /* 1001 and 7008 are standard ADS Open Errors that will usually be sharing issues */
        HB_ERRCODE errOsCode = u32RetVal == 1001 || u32RetVal == 7008 ? 32 : 0;
        fRetry = commonError(pArea, EG_OPEN, static_cast<HB_ERRCODE>(u32RetVal), errOsCode, pOpenInfo->abName,
                             EF_CANRETRY | EF_CANDEFAULT, &pError) == E_RETRY;
      }
      else
      {
        fRetry = false;
      }
    } while (fRetry);

    if (pError)
    {
      hb_errRelease(pError);
    }

    if (u32RetVal != AE_SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  /* Set default alias if necessary */
  if (!pOpenInfo->atomAlias)
  {
    UNSIGNED16 uiAliasLen = HB_RDD_MAX_ALIAS_LEN + 1;
    if (AdsGetTableAlias(hTable, reinterpret_cast<UNSIGNED8 *>(szAlias), &uiAliasLen) == AE_SUCCESS)
    {
      pOpenInfo->atomAlias = szAlias;
    }
    else
    {
      pOpenInfo->atomAlias = "";
    }
  }

  pArea->szDataFileName = hb_strdup(pOpenInfo->abName);
  pArea->hTable = hTable;
  pArea->hStatement = hStatement;
  pArea->hOrdCurrent = 0;
  pArea->fShared = pOpenInfo->fShared;
  pArea->fReadonly = pOpenInfo->fReadonly;

  SELF_FIELDCOUNT(&pArea->area, &uiFields);

  SELF_SETFIELDEXTENT(&pArea->area, uiFields);

  pArea->maxFieldLen = 0;

  u32RetVal = AE_SUCCESS;
  for (uiCount = 1; uiCount <= uiFields; uiCount++)
  {
    usBufLen = ADS_MAX_FIELD_NAME;
    if ((u32RetVal = AdsGetFieldName(pArea->hTable, uiCount, szName, &usBufLen)) != AE_SUCCESS)
    {
      break;
    }

    szName[usBufLen] = '\0';
    dbFieldInfo.atomName = reinterpret_cast<char *>(szName);

    if ((u32RetVal = AdsGetFieldType(pArea->hTable, szName, &usType)) != AE_SUCCESS)
    {
      break;
    }

    if ((u32RetVal = AdsGetFieldLength(pArea->hTable, szName, &u32Length)) != AE_SUCCESS)
    {
      break;
    }

    dbFieldInfo.uiLen = static_cast<HB_USHORT>(u32Length);
    dbFieldInfo.uiDec = 0;
    dbFieldInfo.uiFlags = 0;
    if (u32Length > pArea->maxFieldLen)
    {
      pArea->maxFieldLen = u32Length;
    }

    dbFieldInfo.uiTypeExtended = usType;
    switch (usType)
    {
    case ADS_STRING:
      dbFieldInfo.uiType = Harbour::DB::Field::STRING;
      break;

    case ADS_RAW:
      dbFieldInfo.uiType = Harbour::DB::Field::STRING;
      dbFieldInfo.uiFlags = HB_FF_BINARY;
      break;

#if ADS_LIB_VERSION >= 710
    case ADS_CISTRING:
      dbFieldInfo.uiType = Harbour::DB::Field::STRING;
      break;
#endif

    case ADS_NUMERIC:
      dbFieldInfo.uiType = Harbour::DB::Field::LONG;
      AdsGetFieldDecimals(pArea->hTable, szName, &usDecimals);
      dbFieldInfo.uiDec = static_cast<HB_USHORT>(usDecimals);
      break;

    case ADS_DOUBLE:
      dbFieldInfo.uiType = Harbour::DB::Field::DOUBLE;
      AdsGetFieldDecimals(pArea->hTable, szName, &usDecimals);
      dbFieldInfo.uiDec = static_cast<HB_USHORT>(usDecimals);
      break;

    case ADS_CURDOUBLE:
      dbFieldInfo.uiType = Harbour::DB::Field::CURDOUBLE;
      AdsGetFieldDecimals(pArea->hTable, szName, &usDecimals);
      dbFieldInfo.uiDec = static_cast<HB_USHORT>(usDecimals);
      break;

#if ADS_LIB_VERSION >= 700
    case ADS_MONEY:
      dbFieldInfo.uiType = Harbour::DB::Field::CURRENCY;
      AdsGetFieldDecimals(pArea->hTable, szName, &usDecimals);
      dbFieldInfo.uiDec = static_cast<HB_USHORT>(usDecimals);
      break;
#endif

    case ADS_INTEGER:
    case ADS_SHORTINT:
#if ADS_LIB_VERSION >= 700
    case ADS_LONGLONG:
#endif
      dbFieldInfo.uiType = Harbour::DB::Field::INTEGER;
      break;

    case ADS_TIME:
      dbFieldInfo.uiType = Harbour::DB::Field::TIME;
      break;

    case ADS_TIMESTAMP:
      dbFieldInfo.uiType = Harbour::DB::Field::TIMESTAMP;
      break;

#if ADS_LIB_VERSION >= 800
    case ADS_MODTIME:
      dbFieldInfo.uiType = Harbour::DB::Field::MODTIME;
      break;
#endif

    case ADS_AUTOINC:
      dbFieldInfo.uiType = Harbour::DB::Field::AUTOINC;
      break;

#if ADS_LIB_VERSION >= 800
    case ADS_ROWVERSION:
      dbFieldInfo.uiType = Harbour::DB::Field::ROWVER;
      break;
#endif

    case ADS_LOGICAL:
      dbFieldInfo.uiType = Harbour::DB::Field::LOGICAL;
      break;

    case ADS_DATE:
    case ADS_COMPACTDATE:
      dbFieldInfo.uiType = Harbour::DB::Field::DATE;
      break;

#if ADS_LIB_VERSION >= 900
    case ADS_VARCHAR_FOX:
      dbFieldInfo.uiType = Harbour::DB::Field::VARLENGTH;
      break;

    case ADS_VARBINARY_FOX:
      dbFieldInfo.uiType = Harbour::DB::Field::VARLENGTH;
      dbFieldInfo.uiFlags = HB_FF_BINARY;
      break;
#endif

    case ADS_MEMO:
      dbFieldInfo.uiType = Harbour::DB::Field::MEMO;
      break;

    case ADS_VARCHAR:
      dbFieldInfo.uiType = Harbour::DB::Field::VARLENGTH;
      break;

    case ADS_BINARY:
      dbFieldInfo.uiType = Harbour::DB::Field::BLOB;
      dbFieldInfo.uiFlags = HB_FF_BINARY;
      break;

    case ADS_IMAGE:
      dbFieldInfo.uiType = Harbour::DB::Field::IMAGE;
      dbFieldInfo.uiFlags = HB_FF_BINARY;
      break;

#if ADS_LIB_VERSION >= 1000
    case ADS_NCHAR:
      dbFieldInfo.uiType = Harbour::DB::Field::STRING;
      dbFieldInfo.uiFlags = HB_FF_UNICODE;
      fUnicode = true;
      break;

    case ADS_NVARCHAR:
      dbFieldInfo.uiType = Harbour::DB::Field::VARLENGTH;
      dbFieldInfo.uiFlags = HB_FF_UNICODE;
      fUnicode = true;
      break;

    case ADS_NMEMO:
      dbFieldInfo.uiType = Harbour::DB::Field::MEMO;
      dbFieldInfo.uiFlags = HB_FF_UNICODE;
      fUnicode = true;
      break;
#endif
    default:
      u32RetVal = EDBF_CORRUPT;
      break;
    }

    if (u32RetVal == AE_SUCCESS)
    {
      u32RetVal = SELF_ADDFIELD(&pArea->area, &dbFieldInfo) == Harbour::FAILURE ? EDBF_CORRUPT : AE_SUCCESS;
    }

    if (u32RetVal != AE_SUCCESS)
    {
      break;
    }
  }

  if (u32RetVal != AE_SUCCESS)
  {
    commonError(pArea, EG_CORRUPTION, static_cast<HB_ERRCODE>(u32RetVal), 0, pOpenInfo->abName, EF_CANDEFAULT, nullptr);
    SELF_CLOSE(&pArea->area);
    return Harbour::FAILURE;
  }

  if (fUnicode)
  {
    pArea->maxFieldLen <<= 1;
  }
  if (pArea->maxFieldLen < 24)
  {
    pArea->maxFieldLen = 24;
  }

  AdsGetRecordLength(pArea->hTable, &u32Length);
  pArea->ulRecordLen = u32Length;
  /* Alloc record buffer - because it's also used for some extended types
     conversion it has to be at least 25 bytes size */
  pArea->pRecord = static_cast<HB_BYTE *>(hb_xgrab(HB_MAX(pArea->ulRecordLen, pArea->maxFieldLen) + 1));

  /* If successful call SUPER_OPEN to finish system jobs */
  if (SUPER_OPEN(&pArea->area, pOpenInfo) == Harbour::FAILURE)
  {
    SELF_CLOSE(&pArea->area);
    return Harbour::FAILURE;
  }

  if (hb_setGetNI(HB_SET_AUTORDER))
  {
    DBORDERINFO pOrderInfo;
    pOrderInfo.itmResult = hb_itemPutNI(nullptr, 0);
    pOrderInfo.itmNewVal = nullptr;
    pOrderInfo.itmOrder = hb_itemPutNI(nullptr, hb_setGetNI(HB_SET_AUTORDER));
    pOrderInfo.atomBagName = nullptr;
    SELF_ORDLSTFOCUS(&pArea->area, &pOrderInfo);
    hb_itemRelease(pOrderInfo.itmOrder);
    hb_itemRelease(pOrderInfo.itmResult);
  }

  return SELF_GOTOP(&pArea->area);
}

#define adsRelease nullptr

static HB_ERRCODE adsStructSize(ADSAREAP pArea, HB_USHORT *StructSize)
{
  HB_SYMBOL_UNUSED(pArea);

  *StructSize = sizeof(ADSAREA);

  return Harbour::SUCCESS;
}

static HB_ERRCODE adsSysName(ADSAREAP pArea, HB_BYTE *pBuffer)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsSysName(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pBuffer)));
#endif

  UNSIGNED16 u16TableType;
  UNSIGNED32 u32RetVal;

  if (pArea->hTable)
  {
    u32RetVal = AdsGetTableType(pArea->hTable, &u16TableType);
    if (u32RetVal != AE_SUCCESS)
    {
#if 0
         HB_TRACE(HB_TR_DEBUG, ("Error in adsSysName: %lu  pArea->hTable %p", static_cast<HB_ULONG>(u32RetVal), static_cast<void*>(static_cast<HB_PTRUINT>(pArea->hTable))));
#endif
      u16TableType = static_cast<UNSIGNED16>(pArea->iFileType);
    }
  }
  else
  {
    u16TableType = static_cast<UNSIGNED16>(pArea->iFileType);
  }

  switch (u16TableType)
  {
  case ADS_NTX:
    hb_strncpy(reinterpret_cast<char *>(pBuffer), "ADSNTX", HB_RDD_MAX_DRIVERNAME_LEN);
    break;
  case ADS_CDX:
    hb_strncpy(reinterpret_cast<char *>(pBuffer), "ADSCDX", HB_RDD_MAX_DRIVERNAME_LEN);
    break;
#if ADS_LIB_VERSION >= 900
  case ADS_VFP:
    hb_strncpy(reinterpret_cast<char *>(pBuffer), "ADSVFP", HB_RDD_MAX_DRIVERNAME_LEN);
    break;
#endif
  case ADS_ADT:
    hb_strncpy(reinterpret_cast<char *>(pBuffer), "ADSADT", HB_RDD_MAX_DRIVERNAME_LEN);
    break;
  }

  return Harbour::SUCCESS;
}

#define adsEval nullptr

static HB_ERRCODE adsPack(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsPack(%p)", static_cast<void*>(pArea)));
#endif

  if (pArea->fReadonly)
  {
    commonError(pArea, EG_READONLY, EDBF_READONLY - 900, 0, nullptr, 0, nullptr);
    return Harbour::FAILURE;
  }
  if (pArea->fShared)
  {
    commonError(pArea, EG_SHARED, EDBF_SHARED - 900, 0, nullptr, 0, nullptr);
    return Harbour::FAILURE;
  }

  AdsPackTable(pArea->hTable);

  return SELF_GOTOP(&pArea->area);
}

#define adsPackRec nullptr
#define adsSort nullptr
#define adsTrans nullptr
#define adsTransRec nullptr

static HB_ERRCODE adsZap(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsZap(%p)", static_cast<void*>(pArea)));
#endif

  if (pArea->fReadonly)
  {
    commonError(pArea, EG_READONLY, EDBF_READONLY - 900, 0, nullptr, 0, nullptr);
    return Harbour::FAILURE;
  }
  if (pArea->fShared)
  {
    commonError(pArea, EG_SHARED, EDBF_SHARED - 900, 0, nullptr, 0, nullptr);
    return Harbour::FAILURE;
  }

  AdsZapTable(pArea->hTable);

  return SELF_GOTOP(&pArea->area);
}

static HB_ERRCODE adsChildEnd(ADSAREAP pArea, LPDBRELINFO pRelInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsChildEnd(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pRelInfo)));
#endif

  HB_ERRCODE errCode;

  if (pArea->lpdbPendingRel == pRelInfo)
  {
    errCode = SELF_FORCEREL(&pArea->area);
  }
  else
  {
    errCode = Harbour::SUCCESS;
  }

  SUPER_CHILDEND(&pArea->area, pRelInfo);

  return errCode;
}

static HB_ERRCODE adsChildStart(ADSAREAP pArea, LPDBRELINFO pRelInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsChildStart(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pRelInfo)));
#endif

  SELF_CHILDSYNC(&pArea->area, pRelInfo);

  return SUPER_CHILDSTART(&pArea->area, pRelInfo);
}

static HB_ERRCODE adsChildSync(ADSAREAP pArea, LPDBRELINFO pRelInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsChildSync(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pRelInfo)));
#endif

  pArea->lpdbPendingRel = pRelInfo;

  if (pArea->area.lpdbRelations)
  {
    SELF_SYNCCHILDREN(&pArea->area);
  }

  return Harbour::SUCCESS;
}

#define adsSyncChildren nullptr

static HB_ERRCODE adsClearRel(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsClearRel(%p)", static_cast<void*>(pArea)));
#endif

  SUPER_CLEARREL(&pArea->area);
  AdsClearRelation(pArea->hTable);

  return Harbour::SUCCESS;
}

static HB_ERRCODE adsForceRel(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsForceRel(%p)", static_cast<void*>(pArea)));
#endif

  if (pArea->lpdbPendingRel)
  {
    LPDBRELINFO lpdbPendingRel;

    lpdbPendingRel = pArea->lpdbPendingRel;
    pArea->lpdbPendingRel = nullptr;

    if (!lpdbPendingRel->isOptimized)
    {
      SELF_RELEVAL(&pArea->area, lpdbPendingRel);
    }

    hb_adsUpdateAreaFlags(pArea);
  }

  return Harbour::SUCCESS;
}

#define adsRelArea nullptr
#define adsRelEval nullptr
#define adsRelText nullptr

static HB_ERRCODE adsSetRel(ADSAREAP pArea, LPDBRELINFO lpdbRelations)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsSetRel(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(lpdbRelations)));
#endif

  auto u32RetVal = static_cast<UNSIGNED32>(~AE_SUCCESS);

  auto szExp = reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(lpdbRelations->abKey)));
  if (*szExp && adsGetRddType(lpdbRelations->lpaChild->rddID) >= 0)
  {
    ADSHANDLE hIndex = (reinterpret_cast<ADSAREAP>(lpdbRelations->lpaChild))->hOrdCurrent;

    if (hIndex)
    {
      if (lpdbRelations->isScoped)
      {
        u32RetVal = AdsSetScopedRelation(pArea->hTable, hIndex, szExp);
      }
      else
      {
        u32RetVal = AdsSetRelation(pArea->hTable, hIndex, szExp);
      }
    }
  }
  lpdbRelations->isOptimized = (u32RetVal == AE_SUCCESS);

  return SUPER_SETREL(&pArea->area, lpdbRelations);
}

static HB_ERRCODE adsOrderListAdd(ADSAREAP pArea, LPDBORDERINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsOrderListAdd(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOrderInfo)));
#endif

  ADSHANDLE ahIndex[256];
  UNSIGNED16 u16ArrayLen = 256;
  UNSIGNED32 u32RetVal;

  u32RetVal = AdsOpenIndex(pArea->hTable,
                           reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(pOrderInfo->atomBagName))),
                           ahIndex, &u16ArrayLen);
  if (u32RetVal != AE_SUCCESS && u32RetVal != AE_INDEX_ALREADY_OPEN)
  {
    /* 1001 and 7008 are standard ADS Open Errors that will usually be sharing issues */
    HB_ERRCODE errOsCode = u32RetVal == 1001 || u32RetVal == 7008 ? 32 : 0;
    commonError(pArea, EG_OPEN, static_cast<HB_ERRCODE>(u32RetVal), errOsCode, hb_itemGetCPtr(pOrderInfo->atomBagName),
                EF_CANDEFAULT, nullptr);
    return Harbour::FAILURE;
  }
  if (!pArea->hOrdCurrent && u16ArrayLen > 0)
  {
    pArea->hOrdCurrent = ahIndex[0];
    return SELF_GOTOP(&pArea->area);
  }

  return Harbour::SUCCESS;
}

static HB_ERRCODE adsOrderListClear(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsOrderListClear(%p)", static_cast<void*>(pArea)));
#endif

#if ADS_LIB_VERSION >= 610
  if (!pArea->fReadonly)
  {
    AdsFlushFileBuffers(pArea->hTable); /* meaningful with local server; ignored by remote server */
  }
#endif

  AdsCloseAllIndexes(pArea->hTable);
  pArea->hOrdCurrent = 0;

  return Harbour::SUCCESS;
}

static HB_ERRCODE adsOrderListDelete(ADSAREAP pArea, LPDBORDERINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsOrderListDelete(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOrderInfo)));
#endif

  if (hb_itemGetCLen(pOrderInfo->atomBagName) > 0)
  {
    ADSHANDLE hIndex;

    hIndex = hb_adsFindBag(pArea, hb_itemGetCPtr(pOrderInfo->atomBagName));
    if (hIndex)
    {
      if (AdsCloseIndex(hIndex) == AE_SUCCESS)
      {
        if (pArea->hOrdCurrent)
        {
          UNSIGNED16 u16Order;
          if (AdsGetIndexOrderByHandle(pArea->hOrdCurrent, &u16Order) != AE_SUCCESS)
          {
            pArea->hOrdCurrent = 0;
          }
        }
        return Harbour::SUCCESS;
      }
    }
  }
  return Harbour::FAILURE;
}

static HB_ERRCODE adsOrderListFocus(ADSAREAP pArea, LPDBORDERINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsOrderListFocus(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOrderInfo)));
#endif

  ADSHANDLE hIndex;
  UNSIGNED8 pucTagName[ADS_MAX_TAG_NAME + 1];
  UNSIGNED16 u16Len = ADS_MAX_TAG_NAME + 1, u16Order;
  UNSIGNED32 u32RetVal = AE_SUCCESS;

  if (!pArea->hOrdCurrent)
  {
    *pucTagName = '\0';
    u16Len = 0;
  }
  else
  {
    AdsGetIndexName(pArea->hOrdCurrent, pucTagName, &u16Len);
  }

  pOrderInfo->itmResult = hb_itemPutCL(pOrderInfo->itmResult, reinterpret_cast<char *>(pucTagName), u16Len);

  if (pOrderInfo->itmOrder)
  {
    if (pOrderInfo->itmOrder->isString())
    {
      /* ADS cannot handle a space-padded string--we have to trim it */
      hb_strncpyUpperTrim(reinterpret_cast<char *>(pucTagName), hb_itemGetCPtr(pOrderInfo->itmOrder),
                          sizeof(pucTagName) - 1);
      if (!pucTagName[0])
      {
        pArea->hOrdCurrent = 0;
        return Harbour::SUCCESS;
      }
      u32RetVal = AdsGetIndexHandle(pArea->hTable, pucTagName, &hIndex);
    }
    else if (pOrderInfo->itmOrder->isNumeric())
    {
      u16Order = static_cast<UNSIGNED16>(hb_itemGetNI(pOrderInfo->itmOrder));
      if (!u16Order)
      {
        pArea->hOrdCurrent = 0;
        return Harbour::SUCCESS;
      }
      u32RetVal = AdsGetIndexHandleByOrder(pArea->hTable, u16Order, &hIndex);
    }
    else
    {
      hIndex = pArea->hOrdCurrent;
    }

    if (u32RetVal != AE_SUCCESS)
    {
      /* NTX compatibility: keep current order if failed */
      if (pArea->iFileType == ADS_NTX)
      {
        return Harbour::SUCCESS;
      }

      pArea->hOrdCurrent = 0;
      return Harbour::FAILURE;
    }
    pArea->hOrdCurrent = hIndex;
  }
  return Harbour::SUCCESS;
}

static HB_ERRCODE adsOrderListRebuild(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsOrderListRebuild(%p)", static_cast<void*>(pArea)));
#endif

  AdsReindex(pArea->hTable);

  return SELF_GOTOP(&pArea->area);
}

#define adsOrderCondition nullptr

static HB_ERRCODE adsOrderCreate(ADSAREAP pArea, LPDBORDERCREATEINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsOrderCreate(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOrderInfo)));
#endif

  ADSHANDLE hIndex;
  ADSHANDLE hTableOrIndex;
  UNSIGNED32 u32RetVal;
  UNSIGNED32 u32Options = ADS_DEFAULT;
  PHB_ITEM pExprItem = pOrderInfo->abExpr;
  UNSIGNED16 u16 = 0;
  UNSIGNED8 pucWhile[(ADS_MAX_KEY_LENGTH << 1) + 3];
  UNSIGNED16 u16Len = ADS_MAX_KEY_LENGTH;
  bool fClose = true;

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  if (!pArea->area.lpdbOrdCondInfo || (pArea->area.lpdbOrdCondInfo->fAll && !pArea->area.lpdbOrdCondInfo->fAdditive))
  {
    SELF_ORDLSTCLEAR(&pArea->area);
    fClose = false;
  }
  else if (pArea->area.lpdbOrdCondInfo->fAdditive)
  {
    fClose = false;
  }

  if (!pOrderInfo->abBagName || *(pOrderInfo->abBagName) == '\0')
  {
    u32Options = ADS_COMPOUND;
  }
  else if (pOrderInfo->atomBagName && *(pOrderInfo->atomBagName) != '\0')
  {
    u32Options = ADS_COMPOUND;
  }

  pucWhile[0] = 0;
  if (pArea->area.lpdbOrdCondInfo && pArea->area.lpdbOrdCondInfo->fUseCurrent && pArea->hOrdCurrent)
  {
    UNSIGNED8 pucScope[ADS_MAX_KEY_LENGTH + 1];
    UNSIGNED16 u16BufLen = ADS_MAX_KEY_LENGTH;
    /*
       ADS subIndex does not obey scope, so create a While expression
       from the index key and scope expression if there is one.
     */
    u32RetVal = AdsGetScope(pArea->hOrdCurrent, ADS_BOTTOM, pucScope, &u16BufLen);

    if (u32RetVal == AE_SUCCESS && u16BufLen)
    {
      /* TODO:
         if tag/file exists AND a bagname specifies a non-structural bag, it does not subindex!
         Have to see if it's there already and delete it!  For now, warn users to delete
         secondary bags before creating temp indexes with USECURRENT
       */
      AdsGetKeyType(pArea->hOrdCurrent, &u16);
      AdsGetIndexExpr(pArea->hOrdCurrent, pucWhile, &u16Len);
      pucWhile[u16Len] = 0;
      if (u16 == ADS_STRING)
      { /* add quotation marks around the key */
        hb_strncat(reinterpret_cast<char *>(pucWhile), "<=\"", sizeof(pucWhile) - 1);
        hb_strncat(reinterpret_cast<char *>(pucWhile), reinterpret_cast<char *>(pucScope), sizeof(pucWhile) - 1);
        hb_strncat(reinterpret_cast<char *>(pucWhile), "\"", sizeof(pucWhile) - 1);
      }
      else
      {
        hb_strncat(reinterpret_cast<char *>(pucWhile), "<=", sizeof(pucWhile) - 1);
        hb_strncat(reinterpret_cast<char *>(pucWhile), reinterpret_cast<char *>(pucScope), sizeof(pucWhile) - 1);
      }
    }
    hTableOrIndex = pArea->hOrdCurrent;
  }
  else
  {
    hTableOrIndex = pArea->hTable;
  }

  if (pArea->area.lpdbOrdCondInfo && pArea->area.lpdbOrdCondInfo->abWhile)
  {
    if (pucWhile[0])
    {
      hb_strncat(reinterpret_cast<char *>(pucWhile), ".AND.(", sizeof(pucWhile) - 1);
      hb_strncat(reinterpret_cast<char *>(pucWhile), static_cast<char *>(pArea->area.lpdbOrdCondInfo->abWhile),
                 sizeof(pucWhile) - 1);
      hb_strncat(reinterpret_cast<char *>(pucWhile), ")", sizeof(pucWhile) - 1);
    }
    else
    {
      hb_strncat(reinterpret_cast<char *>(pucWhile), static_cast<char *>(pArea->area.lpdbOrdCondInfo->abWhile),
                 sizeof(pucWhile) - 1);
    }

    if (pArea->hOrdCurrent)
    {
      hTableOrIndex = pArea->hOrdCurrent;
    }
  }

  if (pArea->area.lpdbOrdCondInfo)
  {
    if (pArea->area.lpdbOrdCondInfo->fCustom)
    {
      u32Options |= ADS_CUSTOM;
    }

    if (pArea->area.lpdbOrdCondInfo->fDescending)
    {
      u32Options |= ADS_DESCENDING;
    }
  }

  if (pOrderInfo->fUnique)
  {
    u32Options |= ADS_UNIQUE;
  }

#if ADS_LIB_VERSION >= 610
  u32RetVal = AdsCreateIndex61(
      hTableOrIndex, static_cast<UNSIGNED8 *>(HB_UNCONST(pOrderInfo->abBagName)),
      static_cast<UNSIGNED8 *>(HB_UNCONST(pOrderInfo->atomBagName)),
      reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(pExprItem))),
      pArea->area.lpdbOrdCondInfo ? reinterpret_cast<UNSIGNED8 *>(pArea->area.lpdbOrdCondInfo->abFor) : nullptr,
      pucWhile, u32Options, adsGetFileType(pArea->area.rddID) == ADS_ADT ? adsIndexPageSize(ADS_ADT) : ADS_DEFAULT,
      &hIndex);
#else
  u32RetVal = AdsCreateIndex(hTableOrIndex, static_cast<UNSIGNED8 *>(HB_UNCONST(pOrderInfo->abBagName)),
                             static_cast<UNSIGNED8 *>(HB_UNCONST(pOrderInfo->atomBagName)),
                             static_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(pExprItem))),
                             pArea->area.lpdbOrdCondInfo ? static_cast<UNSIGNED8 *>(pArea->area.lpdbOrdCondInfo->abFor)
                                                         : nullptr,
                             pucWhile, u32Options, &hIndex);
#endif

  SELF_ORDSETCOND(&pArea->area, nullptr);

  if (u32RetVal != AE_SUCCESS)
  {
    commonError(pArea, EG_CREATE, static_cast<HB_ERRCODE>(u32RetVal), 0, pOrderInfo->abBagName, 0, nullptr);
    return Harbour::FAILURE;
  }
  else
  {
    pArea->hOrdCurrent = hIndex;
  }

  if (fClose)
  {
    ADSHANDLE ahIndex[256];
    UNSIGNED16 usArrayLen = 256;

    u32RetVal =
        AdsOpenIndex(pArea->hTable, static_cast<UNSIGNED8 *>(HB_UNCONST(pOrderInfo->abBagName)), ahIndex, &usArrayLen);
    if (u32RetVal != AE_SUCCESS && u32RetVal != AE_INDEX_ALREADY_OPEN)
    {
      SELF_ORDSETCOND(&pArea->area, nullptr);
      return Harbour::FAILURE;
    }
    pArea->hOrdCurrent = usArrayLen ? ahIndex[0] : 0;
  }

  return SELF_GOTOP(&pArea->area);
}

static HB_ERRCODE adsOrderDestroy(ADSAREAP pArea, LPDBORDERINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsOrderDestroy(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pOrderInfo)));
#endif

  ADSHANDLE hIndex;
  UNSIGNED32 u32RetVal;

  if (pOrderInfo->itmOrder->isString())
  {
    UNSIGNED8 pucTagName[ADS_MAX_TAG_NAME + 1];

    hb_strncpyUpperTrim(reinterpret_cast<char *>(pucTagName), hb_itemGetCPtr(pOrderInfo->itmOrder),
                        sizeof(pucTagName) - 1);
    u32RetVal = AdsGetIndexHandle(pArea->hTable, pucTagName, &hIndex);

    if (u32RetVal != AE_SUCCESS)
    {
      return Harbour::FAILURE;
    }

    u32RetVal = AdsDeleteIndex(hIndex);

    if (u32RetVal != AE_SUCCESS)
    {
      return Harbour::FAILURE;
    }

    if (hIndex == pArea->hOrdCurrent)
    {
      pArea->hOrdCurrent = 0;
    }
  }
  else
  {
    return Harbour::FAILURE;
  }

  return Harbour::SUCCESS;
}

static HB_ERRCODE adsOrderInfo(ADSAREAP pArea, HB_USHORT uiIndex, LPDBORDERINFO pOrderInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsOrderInfo(%p, %hu, %p)", static_cast<void*>(pArea), uiIndex, static_cast<void*>(pOrderInfo)));
#endif

  ADSHANDLE hIndex = 0;
  UNSIGNED8 aucBuffer[MAX_STR_LEN + 1];
  UNSIGNED16 u16len = MAX_STR_LEN;
  UNSIGNED16 u16 = 0;
  UNSIGNED32 u32 = 0;
  UNSIGNED32 u32RetVal;

  aucBuffer[0] = 0;

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  /* all others need an index handle */
  if (uiIndex != DBOI_ORDERCOUNT && pOrderInfo->itmOrder && !pOrderInfo->itmOrder->isNil())
  {
    u32RetVal = AE_SUCCESS;

    if (pOrderInfo->itmOrder->isString())
    {
      UNSIGNED8 pucTagName[ADS_MAX_TAG_NAME + 1];

      hb_strncpyUpperTrim(reinterpret_cast<char *>(pucTagName), hb_itemGetCPtr(pOrderInfo->itmOrder),
                          sizeof(pucTagName) - 1);
      u32RetVal = AdsGetIndexHandle(pArea->hTable, pucTagName, &hIndex);
    }
    else if (pOrderInfo->itmOrder->isNumeric())
    {
      u32RetVal =
          AdsGetIndexHandleByOrder(pArea->hTable, static_cast<UNSIGNED16>(hb_itemGetNI(pOrderInfo->itmOrder)), &hIndex);
    }

    if (u32RetVal != AE_SUCCESS)
    {
      hIndex = 0;
    }
  }
  else
  {
    hIndex = pArea->hOrdCurrent;
  }

  switch (uiIndex)
  {
  case DBOI_CONDITION:
    if (hIndex && AdsGetIndexCondition(hIndex, aucBuffer, &u16len) == AE_SUCCESS)
    {
      pOrderInfo->itmResult = hb_itemPutCL(pOrderInfo->itmResult, reinterpret_cast<const char *>(aucBuffer), u16len);
    }
    else
    {
      pOrderInfo->itmResult = hb_itemPutC(pOrderInfo->itmResult, nullptr);
    }
    break;

  case DBOI_EXPRESSION:
    if (hIndex && AdsGetIndexExpr(hIndex, aucBuffer, &u16len) == AE_SUCCESS)
    {
      pOrderInfo->itmResult = hb_itemPutCL(pOrderInfo->itmResult, reinterpret_cast<const char *>(aucBuffer), u16len);
    }
    else
    {
      pOrderInfo->itmResult = hb_itemPutC(pOrderInfo->itmResult, nullptr);
    }
    break;

  case DBOI_ISCOND:
    if (hIndex)
    {
      AdsGetIndexCondition(hIndex, aucBuffer, &u16);
    }
    else
    {
      u16 = 0;
    }
    pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, u16 != 0);
    break;

  case DBOI_ISDESC:
    if (hIndex)
    {
      AdsIsIndexDescending(hIndex, &u16);

#if ADS_LIB_VERSION >= 900
      if (pOrderInfo->itmNewVal && pOrderInfo->itmNewVal->isLogical())
      {
        if (hb_itemGetL(pOrderInfo->itmNewVal) ? u16 == 0 : u16 != 0)
        {
          AdsSetIndexDirection(hIndex, true);
        }
      }
#endif
    }
    else
    {
      u16 = 0;
    }
    pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, u16 != 0);
    break;

  case DBOI_UNIQUE:
    if (hIndex)
    {
      AdsIsIndexUnique(hIndex, &u16);
    }
    else
    {
      u16 = 0;
    }
    pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, u16 != 0);
    break;

  case DBOI_KEYTYPE: {
    const char *szType = nullptr;
    if (hIndex)
    {
      AdsGetKeyType(hIndex, &u16);
      switch (u16)
      {
      case ADS_STRING:
        szType = "C";
        break;
      case ADS_NUMERIC:
        szType = "N";
        break;
      case ADS_DATE:
        szType = "D";
        break;
      case ADS_LOGICAL:
        szType = "L";
        break;
#if 0
               case ADS_RAW:
                  szType = nullptr;
                  break;
#endif
      }
    }
    pOrderInfo->itmResult = hb_itemPutC(pOrderInfo->itmResult, szType);
    break;
  }
  case DBOI_KEYSIZE:
    if (hIndex)
    {
      AdsGetKeyLength(hIndex, &u16);
    }
    else
    {
      u16 = 0;
    }
    pOrderInfo->itmResult = hb_itemPutNI(pOrderInfo->itmResult, u16);
    break;

  case DBOI_KEYVAL:
    if (!pArea->area.fEof && hIndex)
    {
      /* From ads docs: It is important to note that the key generated
         by this function is built on the client, and the key may not
         exist in the index. */
      AdsExtractKey(hIndex, aucBuffer, &u16len);
      AdsGetKeyType(hIndex, &u16);

      if (!pOrderInfo->itmResult)
      {
        pOrderInfo->itmResult = hb_itemNew(nullptr);
      }
      adsGetKeyItem(pArea, pOrderInfo->itmResult, u16, reinterpret_cast<char *>(aucBuffer), u16len);
    }
    else if (pOrderInfo->itmResult)
    {
      hb_itemClear(pOrderInfo->itmResult);
    }

    break;

  case DBOI_POSITION:
    if (pOrderInfo->itmNewVal && pOrderInfo->itmNewVal->isNumeric())
    {
      /* TODO: results will be wrong if filter is not valid for ADS server */
      if ((u32RetVal = AdsGotoTop(hIndex)) == AE_SUCCESS)
      {
        u32RetVal = AdsSkip(hIndex, hb_itemGetNL(pOrderInfo->itmNewVal) - 1);
      }
      if (u32RetVal != AE_SUCCESS)
      {
        commonError(pArea, EG_CORRUPTION, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, EF_CANDEFAULT, nullptr);
        return Harbour::FAILURE;
      }
      hb_adsUpdateAreaFlags(pArea);
      /* Force relational movement in child WorkAreas */
      if (pArea->area.lpdbRelations)
      {
        SELF_SYNCCHILDREN(&pArea->area);
      }

      pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, !pArea->area.fEof);
    }
    else
    {
      if (hIndex)
      {
        UNSIGNED16 usFilterOption = (pArea->area.dbfi.itmCobExpr ? ADS_RESPECTFILTERS : ADS_RESPECTSCOPES);
        AdsGetKeyNum(hIndex, usFilterOption, &u32);
      }
      else
      {
        UNSIGNED16 usFilterOption = (pArea->area.dbfi.itmCobExpr ? ADS_RESPECTFILTERS : ADS_IGNOREFILTERS);
        AdsGetRecordNum(pArea->hTable, usFilterOption, &u32);
      }
      /*
         TODO: This count will be wrong if server doesn't know full filter!
       */
      pOrderInfo->itmResult = hb_itemPutNL(pOrderInfo->itmResult, u32);
    }
    break;

  case DBOI_RECNO: /* TODO: OR IS THIS JUST RECNO?? */
  case DBOI_KEYNORAW:
    if (hIndex)
    {
      AdsGetKeyNum(hIndex, ADS_RESPECTSCOPES, &u32);
      pOrderInfo->itmResult = hb_itemPutNL(pOrderInfo->itmResult, u32);
    }
    else
    {
      HB_ULONG ulRecNo;
      SELF_RECNO(&pArea->area, &ulRecNo);
      pOrderInfo->itmResult = hb_itemPutNL(pOrderInfo->itmResult, ulRecNo);
    }
    break;

  case DBOI_RELKEYPOS:
    if (pOrderInfo->itmNewVal && pOrderInfo->itmNewVal->isNumeric())
    {
      adsSetRelPos(pArea, hIndex, hb_itemGetND(pOrderInfo->itmNewVal));
    }
    else
    {
      pOrderInfo->itmResult = hb_itemPutND(pOrderInfo->itmResult, adsGetRelPos(pArea, hIndex));
    }
    break;

  case DBOI_NAME:
    if (hIndex)
    {
      AdsGetIndexName(hIndex, aucBuffer, &u16len);
    }
    else
    {
      u16len = 0;
    }
    pOrderInfo->itmResult = hb_itemPutCL(pOrderInfo->itmResult, reinterpret_cast<const char *>(aucBuffer), u16len);
    break;

  case DBOI_NUMBER: {
    UNSIGNED16 usOrder = 0;
    if (hIndex)
    {
      AdsGetIndexOrderByHandle(hIndex, &usOrder);
    }
    else
    {
      usOrder = 0;
    }
    pOrderInfo->itmResult = hb_itemPutNI(pOrderInfo->itmResult, usOrder);
    break;
  }

  case DBOI_BAGNAME:
    if (hIndex)
    {
      AdsGetIndexFilename(hIndex, ADS_BASENAME, aucBuffer, &u16len);
    }
    else
    {
      u16len = 0;
    }
    pOrderInfo->itmResult = hb_itemPutCL(pOrderInfo->itmResult, reinterpret_cast<const char *>(aucBuffer), u16len);
    break;

  case DBOI_FULLPATH:
    if (hIndex)
    {
      AdsGetIndexFilename(hIndex, ADS_FULLPATHNAME, aucBuffer, &u16len);
    }
    else
    {
      u16len = 0;
    }
    pOrderInfo->itmResult = hb_itemPutCL(pOrderInfo->itmResult, reinterpret_cast<const char *>(aucBuffer), u16len);
    break;

  case DBOI_BAGEXT:
    pOrderInfo->itmResult = hb_itemPutC(pOrderInfo->itmResult, adsIndexExt(pArea->iFileType));
    break;

  case DBOI_ORDERCOUNT:
    if (hb_itemGetCLen(pOrderInfo->atomBagName) > 0)
    {
      /* if already open, ads fills other info OK.
         TODO: verify it is already open, or be sure to close it!
         (AE_INDEX_ALREADY_OPEN)
       */
      u32 = AdsOpenIndex(pArea->hTable,
                         reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(pOrderInfo->atomBagName))),
                         nullptr, &u16);

      if (u32 != AE_INDEX_ALREADY_OPEN)
      {
        /* Close the index if we open new one */
        if (u32 == AE_SUCCESS)
        {
          ADSHANDLE ahIndex[1];
          u16 = 1;
          if (AdsOpenIndex(pArea->hTable,
                           reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(pOrderInfo->atomBagName))),
                           ahIndex, &u16) == AE_INDEX_ALREADY_OPEN)
          {
            AdsCloseIndex(ahIndex[0]);
          }
        }
        u16 = 0;
      }
    }
    else
    { /* no specific bag requested; get all current indexes */
      AdsGetNumIndexes(pArea->hTable, &u16);
    }

    pOrderInfo->itmResult = hb_itemPutNI(pOrderInfo->itmResult, u16);
    break;

  case DBOI_KEYCOUNT:
    /*
       TODO: This count will be wrong if server doesn't know full filter!
       TODO: If there are child areas that are not at the top of scope,
             Skip movement may move them to first related record
     */
    if (hIndex)
    {
      if (pArea->area.dbfi.itmCobExpr)
      {
        AdsGetScope(hIndex, ADS_BOTTOM, aucBuffer, &u16len);

        if (u16len)
        {
          HB_ULONG ulRecNo;
          /* Have a scope, so walk it. Need Skips to obey filters with a scope.
             With ADS_RESPECTFILTERS, ADS will respect both the
             scope and the filter BUT it will walk all keys in the index
             to do it!!  This is apparently because Scopes in ADS are implemented in the
             client -- the server itself is NOT aware of them -- so a combined
             scope and filter count on the server itself cannot be first limited
             to the scope and then filtered.
           */
          SELF_RECNO(&pArea->area, &ulRecNo);
          if ((u32RetVal = AdsGotoTop(hIndex)) == AE_SUCCESS)
          {
            for (;;)
            {
              AdsAtEOF(pArea->hTable, &u16);
              if (u16)
              {
                break;
              }
              u32++;
              u32RetVal = AdsSkip(hIndex, 1);
              if (u32RetVal != AE_SUCCESS)
              {
                break;
              }
            }
            SELF_GOTO(&pArea->area, ulRecNo);
          }
        }
        else
        { /* no scope set */
          u32RetVal = AdsGetRecordCount(hIndex, ADS_RESPECTFILTERS, &u32);
        }
      }
      else
      { /* no filter set */
        u32RetVal = AdsGetRecordCount(hIndex, ADS_RESPECTSCOPES, &u32);
      }
    }
    else
    {
      u32RetVal = AdsGetRecordCount(pArea->hTable, ADS_RESPECTFILTERS, &u32);
    }

    if (u32RetVal != AE_SUCCESS)
    {
      commonError(pArea, EG_CORRUPTION, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, EF_CANDEFAULT, nullptr);
    }
    pOrderInfo->itmResult = hb_itemPutNL(pOrderInfo->itmResult, u32);
    break;

  case DBOI_KEYCOUNTRAW: /* ignore filter but RESPECT SCOPE */
    u32RetVal = AdsGetRecordCount((hIndex ? hIndex : pArea->hTable), ADS_RESPECTSCOPES, &u32);
    if (u32RetVal != AE_SUCCESS)
    {
      commonError(pArea, EG_CORRUPTION, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, EF_CANDEFAULT, nullptr);
    }
    pOrderInfo->itmResult = hb_itemPutNL(pOrderInfo->itmResult, u32);
    break;

  case DBOI_SCOPETOP:
    if (hIndex)
    {
      if (pOrderInfo->itmResult)
      {
        adsScopeGet(pArea, hIndex, 0, pOrderInfo->itmResult);
      }
      if (pOrderInfo->itmNewVal)
      {
        adsScopeSet(pArea, hIndex, 0, pOrderInfo->itmNewVal);
      }
    }
    else if (pOrderInfo->itmResult)
    {
      hb_itemClear(pOrderInfo->itmResult);
    }
    break;

  case DBOI_SCOPEBOTTOM:
    if (hIndex)
    {
      if (pOrderInfo->itmResult)
      {
        adsScopeGet(pArea, hIndex, 1, pOrderInfo->itmResult);
      }
      if (pOrderInfo->itmNewVal)
      {
        adsScopeSet(pArea, hIndex, 1, pOrderInfo->itmNewVal);
      }
    }
    else if (pOrderInfo->itmResult)
    {
      hb_itemClear(pOrderInfo->itmResult);
    }
    break;

  case DBOI_SCOPESET:
    if (hIndex)
    {
      if (pOrderInfo->itmNewVal)
      {
        adsScopeSet(pArea, hIndex, 0, pOrderInfo->itmNewVal);
        adsScopeSet(pArea, hIndex, 1, pOrderInfo->itmNewVal);
      }
    }
    if (pOrderInfo->itmResult)
    {
      hb_itemClear(pOrderInfo->itmResult);
    }
    break;

  case DBOI_SCOPETOPCLEAR:
    if (hIndex)
    {
      if (pOrderInfo->itmResult)
      {
        adsScopeGet(pArea, hIndex, 0, pOrderInfo->itmResult);
      }
      AdsClearScope(hIndex, ADS_TOP); /* ADS scopes are 1/2 instead of 0/1 */
    }
    else if (pOrderInfo->itmResult)
    {
      hb_itemClear(pOrderInfo->itmResult);
    }
    break;

  case DBOI_SCOPEBOTTOMCLEAR:
    if (hIndex)
    {
      if (pOrderInfo->itmResult)
      {
        adsScopeGet(pArea, hIndex, 1, pOrderInfo->itmResult);
      }
      AdsClearScope(hIndex, ADS_BOTTOM);
    }
    else if (pOrderInfo->itmResult)
    {
      hb_itemClear(pOrderInfo->itmResult);
    }
    break;

  case DBOI_SCOPECLEAR:
    if (hIndex)
    {
      AdsClearScope(hIndex, ADS_TOP); /* ADS scopes are 1/2 instead of 0/1 */
      AdsClearScope(hIndex, ADS_BOTTOM);
    }
    if (pOrderInfo->itmResult)
    {
      hb_itemClear(pOrderInfo->itmResult);
    }
    break;

  case DBOI_CUSTOM:
    if (hIndex)
    {
      AdsIsIndexCustom(hIndex, &u16);
    }
    pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, u16 != 0);
    break;

#if ADS_LIB_VERSION >= 900
  case DBOI_SKIPUNIQUE: {
    HB_LONG lToSkip =
        pOrderInfo->itmNewVal && pOrderInfo->itmNewVal->isNumeric() ? hb_itemGetNL(pOrderInfo->itmNewVal) : 1;
    if (hIndex)
    {
      pOrderInfo->itmResult =
          hb_itemPutL(pOrderInfo->itmResult, AdsSkipUnique(hIndex, lToSkip >= 0 ? 1 : -1) == AE_SUCCESS);
      hb_adsUpdateAreaFlags(pArea);
      /* Force relational movement in child WorkAreas */
      if (pArea->area.lpdbRelations)
      {
        SELF_SYNCCHILDREN(&pArea->area);
      }
      SELF_SKIPFILTER(&pArea->area, lToSkip);
    }
    else
    {
      pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, SELF_SKIP(&pArea->area, lToSkip) == Harbour::SUCCESS);
    }
    break;
  }
#endif

  case DBOI_OPTLEVEL:
    AdsGetAOFOptLevel(pArea->hTable, &u16, nullptr, nullptr);
    switch (u16)
    {
    case ADS_OPTIMIZED_FULL: /* ADS values are different from Harbour */
      u16 = DBOI_OPTIMIZED_FULL;
      break;
    case ADS_OPTIMIZED_PART:
      u16 = DBOI_OPTIMIZED_PART;
      break;
    default:
      u16 = DBOI_OPTIMIZED_NONE;
    }
    pOrderInfo->itmResult = hb_itemPutNI(pOrderInfo->itmResult, u16);
    break;

  case DBOI_KEYADD:
    pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, hIndex && AdsAddCustomKey(hIndex) == AE_SUCCESS);
    break;

  case DBOI_KEYDELETE:
    pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, hIndex && AdsDeleteCustomKey(hIndex) == AE_SUCCESS);
    break;

    /*
       Unsupported TODO:

       DBOI_FILEHANDLE
       DBOI_SETCODEBLOCK
       DBOI_KEYDEC
       DBOI_HPLOCKING
       DBOI_LOCKOFFSET
       DBOI_KEYSINCLUDED
       these are really global settings:
       DBOI_STRICTREAD
       DBOI_OPTIMIZE
       DBOI_AUTOORDER
       DBOI_AUTOSHARE
     */

  case DBOI_AUTOOPEN:
    pOrderInfo->itmResult = hb_itemPutL(pOrderInfo->itmResult, true);
    /* TODO: Since ADS always opens structural indexes throw some kind of error if caller tries to set to False
       OR be prepared to close indexes (if ADS will allow it) if autoopen is False
     */
    break;

  default:
    return SUPER_ORDINFO(&pArea->area, uiIndex, pOrderInfo);
  }
  return Harbour::SUCCESS;
}

static HB_ERRCODE adsClearFilter(ADSAREAP pArea)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsClearFilter(%p)", static_cast<void*>(pArea)));
#endif

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  /*
     We don't know if an AOF was used.
     Since a call to the server would need to be made to see if there's an AOF
     anyway, just always attempt to clear it.
   */
  AdsClearAOF(pArea->hTable);
  AdsClearFilter(pArea->hTable);

  return SUPER_CLEARFILTER(&pArea->area);
}

#define adsClearLocate nullptr
#define adsClearScope nullptr
#define adsCountScope nullptr
#define adsFilterText nullptr
#define adsScopeInfo nullptr

static HB_ERRCODE adsSetFilter(ADSAREAP pArea, LPDBFILTERINFO pFilterInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsSetFilter(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pFilterInfo)));
#endif

  /* NOTE:
     See if the server can evaluate the filter.
     If not, don't pass it to the server; let the super level
     filter the records locally. */

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  /* must do this first as it calls clearFilter */
  if (SUPER_SETFILTER(&pArea->area, pFilterInfo) == Harbour::SUCCESS)
  {
    UNSIGNED16 bValidExpr = 0;
    UNSIGNED16 usResolve = ADS_RESOLVE_DYNAMIC; /*ADS_RESOLVE_IMMEDIATE ;get this from a SETting*/
    UNSIGNED32 u32RetVal = AE_INVALID_EXPRESSION;
    auto pucFilter = hb_itemGetCPtr(pFilterInfo->abFilterText);

    if (*pucFilter)
    {
      AdsIsExprValid(pArea->hTable, reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(pucFilter)), &bValidExpr);
    }

    if (bValidExpr)
    {
#ifdef ADS_USE_OEM_TRANSLATION
      char *szFilter = hb_adsOemToAnsi(pucFilter, hb_itemGetCLen(pFilterInfo->abFilterText));

      if (hb_setGetL(HB_SET_OPTIMIZE))
      {
        u32RetVal = AdsSetAOF(pArea->hTable, reinterpret_cast<UNSIGNED8 *>(szFilter), usResolve);
      }
      else
      {
        u32RetVal = AdsSetFilter(pArea->hTable, reinterpret_cast<UNSIGNED8 *>(szFilter));
      }

      hb_adsOemAnsiFree(szFilter);
#else
      if (hb_setGetL(HB_SET_OPTIMIZE))
      {
        u32RetVal = AdsSetAOF(pArea->hTable, static_cast<UNSIGNED8 *>(const_cast<char *>(pucFilter)), usResolve);
      }
      else
      {
        u32RetVal = AdsSetFilter(pArea->hTable, static_cast<UNSIGNED8 *>(const_cast<char *>(pucFilter)));
      }
#endif
    } /* else let SUPER handle filtering */
    pArea->area.dbfi.fOptimized = u32RetVal == AE_SUCCESS;
    return Harbour::SUCCESS;
  }

  return Harbour::FAILURE;
}

#define adsSetLocate nullptr
#define adsSetScope nullptr
#define adsSkipScope nullptr
#define adsLocate nullptr
#define adsCompile nullptr
#define adsError nullptr
#define adsEvalBlock nullptr

static HB_ERRCODE adsRawLock(ADSAREAP pArea, HB_USHORT uiAction, HB_ULONG ulRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsRawLock(%p, %hu, %lu)", static_cast<void*>(pArea), uiAction, ulRecNo));
#endif

  UNSIGNED32 u32RetVal;

  switch (uiAction)
  {
  case REC_LOCK:
    if (!pArea->fShared || pArea->fFLocked)
    {
      return Harbour::SUCCESS;
    }

    u32RetVal = AdsLockRecord(pArea->hTable, ulRecNo);
    if (u32RetVal != AE_SUCCESS)
    {
      return Harbour::FAILURE;
    }

    /* Update phantom record status after locking */
    if (!pArea->fPositioned)
    {
      HB_ULONG ulCurRec;
      SELF_RECNO(&pArea->area, &ulCurRec);
      SELF_GOTO(&pArea->area, ulCurRec);
    }
    break;

  case REC_UNLOCK:
    if (!pArea->fShared || pArea->fFLocked)
    {
      return Harbour::SUCCESS;
    }

    u32RetVal = AdsUnlockRecord(pArea->hTable, ulRecNo);
    if (u32RetVal != AE_SUCCESS)
    {
      return Harbour::FAILURE;
    }
    break;

  case FILE_LOCK:
    if (!pArea->fShared || pArea->fFLocked)
    {
      return Harbour::SUCCESS;
    }

    u32RetVal = AdsLockTable(pArea->hTable);
    if (u32RetVal != AE_SUCCESS)
    {
      return Harbour::FAILURE;
    }

    pArea->fFLocked = true;
    /* Update phantom record status after locking */
    if (!pArea->fPositioned)
    {
      HB_ULONG ulCurRec;
      SELF_RECNO(&pArea->area, &ulCurRec);
      SELF_GOTO(&pArea->area, ulCurRec);
    }
    break;

  case FILE_UNLOCK:
    if (!pArea->fShared)
    {
      return true;
    }

    u32RetVal = AdsUnlockTable(pArea->hTable);
    if (u32RetVal == AE_SUCCESS || u32RetVal == AE_TABLE_NOT_LOCKED || u32RetVal == AE_TABLE_NOT_SHARED)
    {
      pArea->fFLocked = false;
    }
    else
    {
      return Harbour::FAILURE;
    }
    break;
  }
  return Harbour::SUCCESS;
}

static HB_ERRCODE adsLock(ADSAREAP pArea, LPDBLOCKINFO pLockInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsLock(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pLockInfo)));
#endif

  HB_USHORT uiAction;

  auto ulRecNo = static_cast<HB_ULONG>(hb_itemGetNL(pLockInfo->itmRecID));

  switch (pLockInfo->uiMethod)
  {
  case DBLM_EXCLUSIVE:
    if (pArea->fShared && !pArea->fFLocked)
    {
      AdsUnlockTable(pArea->hTable);
    }

    if (!ulRecNo)
    {
      SELF_RECNO(&pArea->area, &ulRecNo);
    }

    uiAction = REC_LOCK;
    break;

  case DBLM_MULTIPLE:
    if (!ulRecNo)
    {
      SELF_RECNO(&pArea->area, &ulRecNo);
    }

    uiAction = REC_LOCK;
    break;

  case DBLM_FILE:
    uiAction = FILE_LOCK;
    break;

  default:
    /* This should probably throw a real error... */
#if 0
         HB_TRACE(HB_TR_INFO, ("adsLock() error in pLockInfo->uiMethod"));
#endif
    pLockInfo->fResult = false;
    return Harbour::FAILURE;
  }

  pLockInfo->fResult = SELF_RAWLOCK(&pArea->area, uiAction, ulRecNo) == Harbour::SUCCESS;
  return Harbour::SUCCESS;
}

static HB_ERRCODE adsUnLock(ADSAREAP pArea, PHB_ITEM pRecNo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsUnLock(%p, %p)", static_cast<void*>(pArea), static_cast<void*>(pRecNo)));
#endif

  HB_ULONG ulRecNo;

  ulRecNo = hb_itemGetNL(pRecNo);

  return SELF_RAWLOCK(&pArea->area, ulRecNo ? REC_UNLOCK : FILE_UNLOCK, ulRecNo);
}

#define adsCloseMemFile nullptr
#define adsCreateMemFile nullptr

static HB_ERRCODE adsGetValueFile(ADSAREAP pArea, HB_USHORT uiIndex, const char *szFile, HB_USHORT uiMode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsGetValueFile(%p, %hu, %s, %hu)", static_cast<void*>(pArea), uiIndex, szFile, uiMode));
#endif

  UNSIGNED32 u32RetVal;

  HB_SYMBOL_UNUSED(uiMode);

  if (!uiIndex || uiIndex > pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  if (!pArea->fPositioned)
  {
    return Harbour::SUCCESS;
  }

  u32RetVal =
      AdsBinaryToFile(pArea->hTable, ADSFIELD(uiIndex), reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(szFile)));
  if (u32RetVal != AE_SUCCESS)
  {
#if 0
      commonError(pArea, EG_READ, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, 0, nullptr);
#endif
    return Harbour::FAILURE;
  }
  return Harbour::SUCCESS;
}

#define adsOpenMemFile nullptr

static HB_ERRCODE adsPutValueFile(ADSAREAP pArea, HB_USHORT uiIndex, const char *szFile, HB_USHORT uiMode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsPutValueFile(%p, %hu, %s, %hu)", static_cast<void*>(pArea), uiIndex, szFile, uiMode));
#endif

  UNSIGNED32 u32RetVal;

  if (!uiIndex || uiIndex > pArea->area.uiFieldCount)
  {
    return Harbour::FAILURE;
  }

  /* resolve any pending relations */
  if (pArea->lpdbPendingRel)
  {
    SELF_FORCEREL(&pArea->area);
  }

  if (!pArea->fPositioned)
  {
    return Harbour::SUCCESS;
  }

  if (hb_ads_bTestRecLocks)
  {
    if (hb_adsCheckLock(pArea) != Harbour::SUCCESS)
    {
      return Harbour::FAILURE;
    }
  }

  if (uiMode != ADS_BINARY && uiMode != ADS_IMAGE)
  {
    uiMode = ADS_BINARY;
  }

  u32RetVal = AdsFileToBinary(pArea->hTable, ADSFIELD(uiIndex), uiMode,
                              reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(szFile)));
  if (u32RetVal != AE_SUCCESS)
  {
    commonError(pArea, EG_WRITE, static_cast<HB_ERRCODE>(u32RetVal), 0, nullptr, 0, nullptr);
    return Harbour::FAILURE;
  }
  return Harbour::SUCCESS;
}

#define adsReadDBHeader nullptr
#define adsWriteDBHeader nullptr

/* TODO: Use AdsDeleteFile() */
static HB_ERRCODE adsDrop(LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, HB_ULONG ulConnect)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsDrop(%p, %p, %p, %lu)", static_cast<void*>(pRDD), static_cast<void*>(pItemTable), static_cast<void*>(pItemIndex), ulConnect));
#endif

  char szFileName[HB_PATH_MAX];
  const char *szExt;
  PHB_ITEM pFileExt = nullptr;
  PHB_FNAME pFileName;
  bool fTable = false, fResult = false;

  auto szFile = hb_itemGetCPtr(pItemIndex);
  if (!szFile[0])
  {
    /* Try to delete index file */
    szFile = hb_itemGetCPtr(pItemTable);
    if (!szFile[0])
    {
      return false;
    }
    fTable = true;
  }

  pFileName = hb_fsFNameSplit(szFile);

  if (!pFileName->szExtension)
  {
    /* Add default extension if missing */
    pFileExt = hb_itemPutC(nullptr, nullptr);
    if (SELF_RDDINFO(pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt) == Harbour::SUCCESS)
    {
      pFileName->szExtension = hb_itemGetCPtr(pFileExt);
    }
  }
  hb_fsFNameMerge(szFileName, pFileName);
  hb_xfree(pFileName);

  /* Use hb_spFile() first to locate table which can be in differ path */
  if (hb_spFile(szFileName, szFileName))
  {
    fResult = hb_fsDelete(szFileName);
    if (fResult && fTable)
    {
      /*
       * Database table file has been deleted, now check if memo is
       * supported and if yes then try to delete memo file if it exists
       * in the same directory as table file
       * hb_fsFNameSplit() repeated intentionally to respect
       * the path set by hb_spFile()
       */
      pFileName = hb_fsFNameSplit(szFileName);
      pFileExt = hb_itemPutC(pFileExt, nullptr);
      if (SELF_RDDINFO(pRDD, RDDI_MEMOEXT, ulConnect, pFileExt) == Harbour::SUCCESS)
      {
        szExt = hb_itemGetCPtr(pFileExt);
        if (szExt[0])
        {
          pFileName->szExtension = szExt;
          hb_fsFNameMerge(szFileName, pFileName);
          hb_fsDelete(szFileName);
        }
      }
      /*
       * and try to delete production index also if it exists
       * in the same directory as table file
       */
      pFileExt = hb_itemPutC(pFileExt, nullptr);
      if (SELF_RDDINFO(pRDD, RDDI_ORDSTRUCTEXT, ulConnect, pFileExt) == Harbour::SUCCESS)
      {
        szExt = hb_itemGetCPtr(pFileExt);
        if (szExt[0])
        {
          pFileName->szExtension = szExt;
          hb_fsFNameMerge(szFileName, pFileName);
          hb_fsDelete(szFileName);
        }
      }
      hb_xfree(pFileName);
    }
  }

  if (pFileExt)
  {
    hb_itemRelease(pFileExt);
  }

  return fResult ? Harbour::SUCCESS : Harbour::FAILURE;
}

/* TODO: Use AdsCheckExistence()
         UNSIGNED32 ENTRYPOINT AdsCheckExistence(ADSHANDLE hConnect, UNSIGNED8 *pucFileName, UNSIGNED16 *pusOnDisk);
 */
static HB_ERRCODE adsExists(LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, HB_ULONG ulConnect)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsExists(%p, %p, %p, %lu)", static_cast<void*>(pRDD), static_cast<void*>(pItemTable), static_cast<void*>(pItemIndex), ulConnect));
#endif

  char szFileName[HB_PATH_MAX];
  PHB_ITEM pFileExt = nullptr;
  PHB_FNAME pFileName;
  bool fTable = false;

  auto szFile = hb_itemGetCPtr(pItemIndex);
  if (!szFile[0])
  {
    szFile = hb_itemGetCPtr(pItemTable);
    if (!szFile[0])
    {
      return false;
    }
    fTable = true;
  }

  pFileName = hb_fsFNameSplit(szFile);

  if (!pFileName->szExtension)
  {
    pFileExt = hb_itemPutC(nullptr, nullptr);
    if (SELF_RDDINFO(pRDD, fTable ? RDDI_TABLEEXT : RDDI_ORDBAGEXT, ulConnect, pFileExt) == Harbour::SUCCESS)
    {
      pFileName->szExtension = hb_itemGetCPtr(pFileExt);
    }
  }
  hb_fsFNameMerge(szFileName, pFileName);
  hb_xfree(pFileName);

  if (pFileExt)
  {
    hb_itemRelease(pFileExt);
  }

  return hb_spFile(szFileName, nullptr) ? Harbour::SUCCESS : Harbour::FAILURE;
}

static HB_ERRCODE adsRename(LPRDDNODE pRDD, PHB_ITEM pItemTable, PHB_ITEM pItemIndex, PHB_ITEM pNewName,
                            HB_ULONG ulConnect)
{
  HB_SYMBOL_UNUSED(pRDD);
  HB_SYMBOL_UNUSED(pItemTable);
  HB_SYMBOL_UNUSED(pItemIndex);
  HB_SYMBOL_UNUSED(pNewName);
  HB_SYMBOL_UNUSED(ulConnect);

  return Harbour::FAILURE;
}

static void adsTSDRelease(void *cargo)
{
  auto pData = static_cast<LPRDDADSDATA>(cargo);

  if (pData->szQuery)
  {
    hb_xfree(pData->szQuery);
  }
}

static HB_ERRCODE adsInit(LPRDDNODE pRDD)
{
  auto pTSD = static_cast<PHB_TSD>(hb_xgrab(sizeof(HB_TSD)));
  HB_TSD_INIT(pTSD, sizeof(RDDADSDATA), nullptr, adsTSDRelease);
  pRDD->lpvCargo = static_cast<void *>(pTSD);

  if (ISSUPER_INIT(pRDD))
  {
    return SUPER_INIT(pRDD);
  }
  else
  {
    return Harbour::SUCCESS;
  }
}

static HB_ERRCODE adsExit(LPRDDNODE pRDD)
{
  HB_SYMBOL_UNUSED(pRDD);

  if (s_uiRddCount)
  {
    if (!--s_uiRddCount)
    {
      if (s_iSetListenerHandle)
      {
        hb_setListenerRemove(s_iSetListenerHandle);
        s_iSetListenerHandle = 0;
      }
#ifdef __BORLANDC__
#pragma option push -w-pro
#endif
      AdsApplicationExit();
#ifdef __BORLANDC__
#pragma option pop
#endif
    }
  }

  if (pRDD->lpvCargo)
  {
    hb_stackReleaseTSD(static_cast<PHB_TSD>(pRDD->lpvCargo));
    hb_xfree(pRDD->lpvCargo);
    pRDD->lpvCargo = nullptr;
  }

  if (ISSUPER_EXIT(pRDD))
  {
    return SUPER_EXIT(pRDD);
  }
  else
  {
    return Harbour::SUCCESS;
  }
}

static HB_ERRCODE adsRddInfo(LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("adsRddInfo(%p, %hu, %lu, %p)", static_cast<void*>(pRDD), uiIndex, ulConnect, static_cast<void*>(pItem)));
#endif

  switch (uiIndex)
  {
  case RDDI_REMOTE:
    hb_itemPutL(pItem, true);
    break;

  case RDDI_CONNECTION: {
    ADSHANDLE hOldConnection = hb_ads_getConnection();

    hb_ads_setConnection(HB_ADS_GETCONNECTION(pItem));
    HB_ADS_PUTCONNECTION(pItem, hOldConnection);
    break;
  }

  case RDDI_CONNECT: {
    ADSHANDLE hConnect = 0;
    UNSIGNED32 u32RetVal;
    LPRDDADSDATA pData = RDDADSNODE_DATA(pRDD);

    if (pItem->isArray())
    {
#if ADS_LIB_VERSION >= 600
      u32RetVal = AdsConnect60(static_cast<UNSIGNED8 *>(HB_UNCONST(hb_arrayGetCPtr(pItem, 1))) /* pucServerPath */,
                               static_cast<UNSIGNED16>(hb_arrayGetNI(pItem, 2)) /* usServerTypes */,
                               static_cast<UNSIGNED8 *>(HB_UNCONST(hb_arrayGetCPtr(pItem, 3))) /* pucUserName */,
                               static_cast<UNSIGNED8 *>(HB_UNCONST(hb_arrayGetCPtr(pItem, 4))) /* pucPassword */,
                               static_cast<UNSIGNED32>(hb_arrayGetNL(pItem, 5)) /* ulOptions */, &hConnect);

#else
      u32RetVal = AdsConnect(static_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(pItem))), &hConnect);
#endif
    }
    else
    {
      u32RetVal = AdsConnect(reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(pItem))), &hConnect);
    }

    if (u32RetVal == AE_SUCCESS)
    {
      hb_ads_setConnection(hConnect); /* set new default */
      pData->ulError = pData->ulInsertID = pData->ulAffectedRows = 0;
      pData->szError[0] = '\0';
      HB_ADS_PUTCONNECTION(pItem, hConnect);
    }
    else
    {
      UNSIGNED16 usLen = sizeof(pData->szError) - 1;

      pData->ulError = u32RetVal;
      AdsGetLastError(&u32RetVal, pData->szError, &usLen);
      pData->szError[usLen] = '\0';
      HB_ADS_PUTCONNECTION(pItem, 0);
    }
    break;
  }

  case RDDI_DISCONNECT: {
    ADSHANDLE hConnect = HB_ADS_GETCONNECTION(pItem);

    /* NOTE: Only allow disconnect of 0 if explicitly passed.
             The thread default connection handle might be 0 if caller
             accidentally disconnects twice. */

    if ((hConnect != 0 || pItem->isNumeric()) && AdsDisconnect(hConnect) == AE_SUCCESS)
    {
      hb_ads_clrConnection(hConnect);
      hb_itemPutL(pItem, true);
    }
    else
    {
      hb_itemPutL(pItem, false);
    }

    break;
  }

  case RDDI_ISDBF:
    hb_itemPutL(pItem, adsGetFileType(pRDD->rddID) != ADS_ADT);
    break;

  case RDDI_CANPUTREC:
    hb_itemPutL(pItem, true);
    break;

  case RDDI_TABLEEXT:
    hb_itemPutC(pItem, adsTableExt(adsGetFileType(pRDD->rddID)));
    break;

  case RDDI_MEMOEXT:
    hb_itemPutC(pItem, adsMemoExt(adsGetFileType(pRDD->rddID)));
    break;

  case RDDI_ORDEREXT:
  case RDDI_ORDBAGEXT:
  case RDDI_ORDSTRUCTEXT:
    hb_itemPutC(pItem, adsIndexExt(adsGetFileType(pRDD->rddID)));
    break;

  case RDDI_INDEXPAGESIZE: {
    auto iPageSize = hb_itemGetNI(pItem);

    hb_itemPutNI(pItem, adsIndexPageSize(adsGetFileType(pRDD->rddID)));

    if (adsGetFileType(pRDD->rddID) == ADS_ADT && iPageSize >= 0x200 && iPageSize <= 0x2000 &&
        ((iPageSize - 1) & iPageSize) == 0)
    {
      hb_ads_setIndexPageSize(iPageSize);
    }
    break;
  }

  case RDDI_ERRORNO: {
    LPRDDADSDATA pData = RDDADSNODE_DATA(pRDD);
    hb_itemPutNL(pItem, static_cast<unsigned long>(pData->ulError));
    break;
  }

  case RDDI_ERROR: {
    LPRDDADSDATA pData = RDDADSNODE_DATA(pRDD);
    hb_itemPutC(pItem, reinterpret_cast<char *>(pData->szError));
    break;
  }

  case RDDI_INSERTID: {
    LPRDDADSDATA pData = RDDADSNODE_DATA(pRDD);
    hb_itemPutNL(pItem, static_cast<unsigned long>(pData->ulInsertID));
    break;
  }

  case RDDI_AFFECTEDROWS: {
    LPRDDADSDATA pData = RDDADSNODE_DATA(pRDD);
    hb_itemPutNL(pItem, static_cast<unsigned long>(pData->ulAffectedRows));
    break;
  }

  case RDDI_EXECUTE: {
    LPRDDADSDATA pData = RDDADSNODE_DATA(pRDD);
    ADSHANDLE hConnect = ulConnect ? static_cast<ADSHANDLE>(ulConnect) : hb_ads_getConnection();
    ADSHANDLE hStatement = 0;
    UNSIGNED32 u32RetVal;

    pData->ulError = pData->ulInsertID = pData->ulAffectedRows = 0;
    pData->szError[0] = '\0';

    u32RetVal = AdsCreateSQLStatement(hConnect, &hStatement);
    if (u32RetVal == AE_SUCCESS)
    {
      ADSHANDLE hCursor = 0;

      AdsStmtSetTableType(hStatement, adsGetFileType(pRDD->rddID));

      u32RetVal = AdsExecuteSQLDirect(
          hStatement, reinterpret_cast<UNSIGNED8 *>(const_cast<char *>(hb_itemGetCPtr(pItem))), &hCursor);
      if (u32RetVal == AE_SUCCESS)
      {
        if (AdsGetLastAutoinc(hStatement, &u32RetVal) == AE_SUCCESS)
        {
          pData->ulInsertID = u32RetVal;
        }

        if (AdsGetRecordCount(hStatement, ADS_IGNOREFILTERS, &u32RetVal) == AE_SUCCESS)
        {
          pData->ulAffectedRows = u32RetVal;
        }

        if (hCursor)
        {
          AdsCloseTable(hCursor);
        }

        u32RetVal = AE_SUCCESS;
      }
      else
      {
        UNSIGNED16 usLen = sizeof(pData->szError) - 1;

        pData->ulError = u32RetVal;
        AdsGetLastError(&u32RetVal, pData->szError, &usLen);
        pData->szError[usLen] = '\0';
      }

      AdsCloseSQLStatement(hStatement);
    }
    else
    {
      UNSIGNED16 usLen = sizeof(pData->szError) - 1;

      pData->ulError = u32RetVal;
      AdsGetLastError(&u32RetVal, pData->szError, &usLen);
      pData->szError[usLen] = '\0';
    }
    hb_itemPutL(pItem, u32RetVal == AE_SUCCESS);
    break;
  }

  default:
    return SUPER_RDDINFO(pRDD, uiIndex, ulConnect, pItem);
  }

  return Harbour::SUCCESS;
}

#define adsWhoCares nullptr

// clang-format off
static const RDDFUNCS adsTable = { ( DBENTRYP_BP ) adsBof,
                                   ( DBENTRYP_BP ) adsEof,
                                   ( DBENTRYP_BP ) adsFound,
                                   ( DBENTRYP_V ) adsGoBottom,
                                   ( DBENTRYP_UL ) adsGoTo,
                                   ( DBENTRYP_I ) adsGoToId,
                                   ( DBENTRYP_V ) adsGoTop,
                                   ( DBENTRYP_BIB ) adsSeek,
                                   ( DBENTRYP_L ) adsSkip,
                                   ( DBENTRYP_L ) adsSkipFilter,
                                   ( DBENTRYP_L ) adsSkipRaw,
                                   ( DBENTRYP_VF ) adsAddField,
                                   ( DBENTRYP_B ) adsAppend,
                                   ( DBENTRYP_I ) adsCreateFields,
                                   ( DBENTRYP_V ) adsDeleteRec,
                                   ( DBENTRYP_BP ) adsDeleted,
                                   ( DBENTRYP_SP ) adsFieldCount,
                                   ( DBENTRYP_VF ) adsFieldDisplay,
                                   ( DBENTRYP_SSI ) adsFieldInfo,
                                   ( DBENTRYP_SCP ) adsFieldName,
                                   ( DBENTRYP_V ) adsFlush,
                                   ( DBENTRYP_PP ) adsGetRec,
                                   ( DBENTRYP_SI ) adsGetValue,
                                   ( DBENTRYP_SVL ) adsGetVarLen,
                                   ( DBENTRYP_V ) adsGoCold,
                                   ( DBENTRYP_V ) adsGoHot,
                                   ( DBENTRYP_P ) adsPutRec,
                                   ( DBENTRYP_SI ) adsPutValue,
                                   ( DBENTRYP_V ) adsRecall,
                                   ( DBENTRYP_ULP ) adsRecCount,
                                   ( DBENTRYP_ISI ) adsRecInfo,
                                   ( DBENTRYP_ULP ) adsRecNo,
                                   ( DBENTRYP_I ) adsRecId,
                                   ( DBENTRYP_S ) adsSetFieldExtent,
                                   ( DBENTRYP_CP ) adsAlias,
                                   ( DBENTRYP_V ) adsClose,
                                   ( DBENTRYP_VO ) adsCreate,
                                   ( DBENTRYP_SI ) adsInfo,
                                   ( DBENTRYP_V ) adsNewArea,
                                   ( DBENTRYP_VO ) adsOpen,
                                   ( DBENTRYP_V ) adsRelease,
                                   ( DBENTRYP_SP ) adsStructSize,
                                   ( DBENTRYP_CP ) adsSysName,
                                   ( DBENTRYP_VEI ) adsEval,
                                   ( DBENTRYP_V ) adsPack,
                                   ( DBENTRYP_LSP ) adsPackRec,
                                   ( DBENTRYP_VS ) adsSort,
                                   ( DBENTRYP_VT ) adsTrans,
                                   ( DBENTRYP_VT ) adsTransRec,
                                   ( DBENTRYP_V ) adsZap,
                                   ( DBENTRYP_VR ) adsChildEnd,
                                   ( DBENTRYP_VR ) adsChildStart,
                                   ( DBENTRYP_VR ) adsChildSync,
                                   ( DBENTRYP_V ) adsSyncChildren,
                                   ( DBENTRYP_V ) adsClearRel,
                                   ( DBENTRYP_V ) adsForceRel,
                                   ( DBENTRYP_SSP ) adsRelArea,
                                   ( DBENTRYP_VR ) adsRelEval,
                                   ( DBENTRYP_SI ) adsRelText,
                                   ( DBENTRYP_VR ) adsSetRel,
                                   ( DBENTRYP_VOI ) adsOrderListAdd,
                                   ( DBENTRYP_V ) adsOrderListClear,
                                   ( DBENTRYP_VOI ) adsOrderListDelete,
                                   ( DBENTRYP_VOI ) adsOrderListFocus,
                                   ( DBENTRYP_V ) adsOrderListRebuild,
                                   ( DBENTRYP_VOO ) adsOrderCondition,
                                   ( DBENTRYP_VOC ) adsOrderCreate,
                                   ( DBENTRYP_VOI ) adsOrderDestroy,
                                   ( DBENTRYP_SVOI ) adsOrderInfo,
                                   ( DBENTRYP_V ) adsClearFilter,
                                   ( DBENTRYP_V ) adsClearLocate,
                                   ( DBENTRYP_V ) adsClearScope,
                                   ( DBENTRYP_VPLP ) adsCountScope,
                                   ( DBENTRYP_I ) adsFilterText,
                                   ( DBENTRYP_SI ) adsScopeInfo,
                                   ( DBENTRYP_VFI ) adsSetFilter,
                                   ( DBENTRYP_VLO ) adsSetLocate,
                                   ( DBENTRYP_VOS ) adsSetScope,
                                   ( DBENTRYP_VPL ) adsSkipScope,
                                   ( DBENTRYP_B ) adsLocate,
                                   ( DBENTRYP_CC ) adsCompile,
                                   ( DBENTRYP_I ) adsError,
                                   ( DBENTRYP_I ) adsEvalBlock,
                                   ( DBENTRYP_VSP ) adsRawLock,
                                   ( DBENTRYP_VL ) adsLock,
                                   ( DBENTRYP_I ) adsUnLock,
                                   ( DBENTRYP_V ) adsCloseMemFile,
                                   ( DBENTRYP_VO ) adsCreateMemFile,
                                   ( DBENTRYP_SCCS ) adsGetValueFile,
                                   ( DBENTRYP_VO ) adsOpenMemFile,
                                   ( DBENTRYP_SCCS ) adsPutValueFile,
                                   ( DBENTRYP_V ) adsReadDBHeader,
                                   ( DBENTRYP_V ) adsWriteDBHeader,
                                   ( DBENTRYP_R ) adsInit,
                                   ( DBENTRYP_R ) adsExit,
                                   ( DBENTRYP_RVVL ) adsDrop,
                                   ( DBENTRYP_RVVL ) adsExists,
                                   ( DBENTRYP_RVVVL ) adsRename,
                                   ( DBENTRYP_RSLV ) adsRddInfo,
                                   ( DBENTRYP_SVP ) adsWhoCares };
// clang-format on

static void adsRegisterRDD(HB_USHORT *pusRddId)
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
    errCode = hb_rddInheritEx(pTable, &adsTable, &adsSuper, nullptr, puiSuperRddId);
    if (errCode == Harbour::SUCCESS)
    {
      /*
       * we successfully register our RDD so now we can initialize it
       * You may think that this place is RDD init statement, Druzus
       */
      *pusRddId = uiRddId;
      ++s_uiRddCount;
      if (!s_iSetListenerHandle)
      {
        adsSetSend();
        s_iSetListenerHandle = hb_setListenerAdd(adsSetListener_callback);
      }
    }
    hb_retni(errCode);
  }
  else
  {
    hb_retni(Harbour::FAILURE);
  }
}

HB_FUNC_STATIC(ADS_GETFUNCTABLE)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("ADS_GETFUNCTABLE()"));
#endif

  adsRegisterRDD(&s_uiRddIdADS);
}

HB_FUNC_STATIC(ADSADT_GETFUNCTABLE)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("ADSADT_GETFUNCTABLE()"));
#endif

  adsRegisterRDD(&s_uiRddIdADSADT);
}

HB_FUNC_STATIC(ADSNTX_GETFUNCTABLE)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("ADSNTX_GETFUNCTABLE()"));
#endif

  adsRegisterRDD(&s_uiRddIdADSNTX);
}

HB_FUNC_STATIC(ADSCDX_GETFUNCTABLE)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("ADSCDX_GETFUNCTABLE()"));
#endif

  adsRegisterRDD(&s_uiRddIdADSCDX);
}

#if ADS_LIB_VERSION >= 900

HB_FUNC_STATIC(ADSVFP_GETFUNCTABLE)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("ADSVFP_GETFUNCTABLE()"));
#endif

  adsRegisterRDD(&s_uiRddIdADSVFP);
}

#endif

HB_FUNC(ADS)
{
  ;
}
HB_FUNC(ADSADT)
{
  ;
}
HB_FUNC(ADSNTX)
{
  ;
}
HB_FUNC(ADSCDX)
{
  ;
}
HB_FUNC(ADSVFP)
{
  ;
}

static void hb_adsRddInit(void *cargo)
{
  HB_SYMBOL_UNUSED(cargo);

  if (hb_rddRegister("ADS", RDT_FULL) > 1 || hb_rddRegister("ADSADT", RDT_FULL) > 1 ||
#if ADS_LIB_VERSION >= 900
      hb_rddRegister("ADSVFP", RDT_FULL) > 1 ||
#endif
      hb_rddRegister("ADSCDX", RDT_FULL) > 1 || hb_rddRegister("ADSNTX", RDT_FULL) > 1)
  {
    hb_errInternal(HB_EI_RDDINVALID, nullptr, nullptr, nullptr);
  }
}

HB_FUNC(HB_RDDADSREGISTER)
{
  hb_adsRddInit(nullptr);
}

// clang-format off
HB_INIT_SYMBOLS_BEGIN( ads1__InitSymbols )
{ "ADS",                 {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADS )}, nullptr },
{ "ADS_GETFUNCTABLE",    {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADS_GETFUNCTABLE )}, nullptr },
{ "ADSADT",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSADT )}, nullptr },
{ "ADSADT_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSADT_GETFUNCTABLE )}, nullptr },
#if ADS_LIB_VERSION >= 900
{ "ADSVFP",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSVFP )}, nullptr },
{ "ADSVFP_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSVFP_GETFUNCTABLE )}, nullptr },
#endif
{ "ADSNTX",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSNTX )}, nullptr },
{ "ADSNTX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSNTX_GETFUNCTABLE )}, nullptr },
{ "ADSCDX",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSCDX )}, nullptr },
{ "ADSCDX_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( ADSCDX_GETFUNCTABLE )}, nullptr }
HB_INIT_SYMBOLS_END( ads1__InitSymbols )
    // clang-format on

    HB_CALL_ON_STARTUP_BEGIN(_hb_ads_rdd_init_) hb_vmAtInit(hb_adsRddInit, nullptr);
HB_CALL_ON_STARTUP_END(_hb_ads_rdd_init_)

// clang-format off
#if defined(HB_PRAGMA_STARTUP)
   #pragma startup ads1__InitSymbols
   #pragma startup _hb_ads_rdd_init_
#elif defined(HB_DATASEG_STARTUP)
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC(ads1__InitSymbols) \
                              HB_DATASEG_FUNC(_hb_ads_rdd_init_)
   #include "hbiniseg.hpp"
#endif
// clang-format on

ADSAREAP hb_adsGetWorkAreaPointer(void)
{
  auto pArea = static_cast<ADSAREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    if (adsGetRddType(pArea->area.rddID) >= 0)
    {
      return pArea;
    }
  }
  return nullptr;
}

HB_FUNC(ADSGETRELKEYPOS)
{
  ADSAREAP pArea = hb_adsGetWorkAreaPointer();

  if (pArea != nullptr)
  {
    hb_retnd(adsGetRelPos(pArea, pArea->hOrdCurrent));
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, 2001, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ADSSETRELKEYPOS)
{
  ADSAREAP pArea = hb_adsGetWorkAreaPointer();

  if (pArea != nullptr)
  {
    adsSetRelPos(pArea, pArea->hOrdCurrent, hb_parnd(1));
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, 2001, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ADSCUSTOMIZEAOF)
{
  ADSAREAP pArea;
  HB_ULONG ulRecord = 0;
  UNSIGNED32 u32NumRecs = 0;
  auto u32RetVal = static_cast<UNSIGNED32>(~AE_SUCCESS); /* initialize to something other than success */
  UNSIGNED16 u16Option = ADS_AOF_ADD_RECORD;

  pArea = hb_adsGetWorkAreaPointer();
  if (pArea != nullptr)
  {
    if (HB_ISNUM(2))
    { /* add, delete or toggle */
      u16Option = static_cast<UNSIGNED16>(hb_parni(2));
    }

    if (HB_ISNIL(1))
    { /* default to current record */
      u32NumRecs = 1;
      SELF_RECNO(&pArea->area, &ulRecord);
    }
    else if (HB_ISNUM(1))
    { /* Passed a single recno */
      u32NumRecs = 1;
      ulRecord = hb_parnl(1);
    }
    else if (HB_ISARRAY(1))
    { /* convert array of recnos to C array */
      u32NumRecs = static_cast<UNSIGNED32>(hb_parinfa(1, 0));
    }

    if (u32NumRecs)
    {
      auto pu32Records = static_cast<UNSIGNED32 *>(hb_xgrab(u32NumRecs * sizeof(UNSIGNED32)));

      if (HB_ISARRAY(1))
      { /* convert array of recnos to C array */
        for (ulRecord = 0; ulRecord < u32NumRecs; ulRecord++)
        {
          pu32Records[ulRecord] = hb_parvnl(1, ulRecord + 1);
        }
      }
      else
      {
        pu32Records[0] = ulRecord;
      }

      u32RetVal = AdsCustomizeAOF(pArea->hTable, u32NumRecs, pu32Records, u16Option);

      /* I cannot understand what this code should do, Druzus */
#if 0
         /* if server has Customized AOF, clear the super filter so bits won't get flipped off! */
         if( u32RetVal == AE_SUCCESS ) {
            SUPER_CLEARFILTER(&pArea->area);
         }
#endif
      hb_xfree(pu32Records);
    }

    hb_retnl(u32RetVal);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, 2001, nullptr, HB_ERR_FUNCNAME);
  }
}
