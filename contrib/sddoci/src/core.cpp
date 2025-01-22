//
// Oracle (via OCILIB) Database Driver
//
// Copyright 2010 Viktor Szakats (vszakats.net/harbour)
// Originally based on ODBC driver by:
// Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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

#include <hbapi.hpp>
#include <hbapiitm.hpp>
#include <hbdate.hpp>
#include <hbapistr.hpp>
#include <hbset.hpp>
#include <hbvm.hpp>

#include "hbrddsql.hpp"

#include <ocilib.h>

#if defined(OCI_CHARSET_UNICODE) || defined(OCI_CHARSET_WIDE)
   #define M_HB_ARRAYGETSTR(arr, n, phstr, plen)    hb_arrayGetStrU16(arr, n, HB_CDP_ENDIAN_NATIVE, phstr, plen)
   #define M_HB_ITEMCOPYSTR(itm, str, len)          hb_itemCopyStrU16(itm, HB_CDP_ENDIAN_NATIVE, str, len)
   #define M_HB_ITEMGETSTR(itm, phstr, plen)        hb_itemGetStrU16(itm, HB_CDP_ENDIAN_NATIVE, phstr, plen)
   #define M_HB_ITEMPUTSTR(itm, str)                hb_itemPutStrU16(itm, HB_CDP_ENDIAN_NATIVE, str)
   #define M_HB_ITEMPUTSTRLEN(itm, str, len)        hb_itemPutStrLenU16(itm, HB_CDP_ENDIAN_NATIVE, str, len)
   #define M_HB_CHAR  HB_WCHAR
#else
   #define M_HB_ARRAYGETSTR(arr, n, phstr, plen)    hb_arrayGetStr(arr, n, hb_setGetOSCP(), phstr, plen)
   #define M_HB_ITEMCOPYSTR(itm, str, len)          hb_itemCopyStr(itm, hb_setGetOSCP(), str, len)
   #define M_HB_ITEMGETSTR(itm, phstr, plen)        hb_itemGetStr(itm, hb_setGetOSCP(), phstr, plen)
   #define M_HB_ITEMPUTSTR(itm, str)                hb_itemPutStr(itm, hb_setGetOSCP(), str)
   #define M_HB_ITEMPUTSTRLEN(itm, str, len)        hb_itemPutStrLen(itm, hb_setGetOSCP(), str, len)
   #define M_HB_CHAR  char
#endif

#if defined(OCI_CHARSET_UNICODE) || defined(OCI_CHARSET_WIDE) || defined(OCI_CHARSET_MIXED)
   #define D_HB_ARRAYGETSTR(arr, n, phstr, plen)    hb_arrayGetStrU16(arr, n, HB_CDP_ENDIAN_NATIVE, phstr, plen)
   #define D_HB_ITEMCOPYSTR(itm, str, len)          hb_itemCopyStrU16(itm, HB_CDP_ENDIAN_NATIVE, str, len)
   #define D_HB_ITEMGETSTR(itm, phstr, plen)        hb_itemGetStrU16(itm, HB_CDP_ENDIAN_NATIVE, phstr, plen)
   #define D_HB_ITEMPUTSTR(itm, str)                hb_itemPutStrU16(itm, HB_CDP_ENDIAN_NATIVE, str)
   #define D_HB_ITEMPUTSTRLEN(itm, str, len)        hb_itemPutStrLenU16(itm, HB_CDP_ENDIAN_NATIVE, str, len)
   #define D_HB_CHAR  HB_WCHAR
#else
   #define D_HB_ARRAYGETSTR(arr, n, phstr, plen)    hb_arrayGetStr(arr, n, hb_setGetOSCP(), phstr, plen)
   #define D_HB_ITEMCOPYSTR(itm, str, len)          hb_itemCopyStr(itm, hb_setGetOSCP(), str, len)
   #define D_HB_ITEMGETSTR(itm, phstr, plen)        hb_itemGetStr(itm, hb_setGetOSCP(), phstr, plen)
   #define D_HB_ITEMPUTSTR(itm, str)                hb_itemPutStr(itm, hb_setGetOSCP(), str)
   #define D_HB_ITEMPUTSTRLEN(itm, str, len)        hb_itemPutStrLen(itm, hb_setGetOSCP(), str, len)
   #define D_HB_CHAR  char
#endif

struct SDDCONN
{
   OCI_Connection * pConn;
};

struct SDDDATA
{
   OCI_Statement * pStmt;
};

static HB_ERRCODE ocilibConnect(SQLDDCONNECTION * pConnection, PHB_ITEM pItem);
static HB_ERRCODE ocilibDisconnect(SQLDDCONNECTION * pConnection);
static HB_ERRCODE ocilibExecute(SQLDDCONNECTION * pConnection, PHB_ITEM pItem);
static HB_ERRCODE ocilibOpen(SQLBASEAREAP pArea);
static HB_ERRCODE ocilibClose(SQLBASEAREAP pArea);
static HB_ERRCODE ocilibGoTo(SQLBASEAREAP pArea, HB_ULONG ulRecNo);

static SDDNODE s_ocidd =
{
   nullptr,
   "OCILIB",
   static_cast<SDDFUNC_CONNECT>(ocilibConnect),
   static_cast<SDDFUNC_DISCONNECT>(ocilibDisconnect),
   static_cast<SDDFUNC_EXECUTE>(ocilibExecute),
   static_cast<SDDFUNC_OPEN>(ocilibOpen),
   static_cast<SDDFUNC_CLOSE>(ocilibClose),
   static_cast<SDDFUNC_GOTO>(ocilibGoTo),
   static_cast<SDDFUNC_GETVALUE>(nullptr),
   static_cast<SDDFUNC_GETVARLEN>(nullptr)
};

static void hb_ocidd_init(void * cargo)
{
   HB_SYMBOL_UNUSED(cargo);

   if( !OCI_Initialize(nullptr, nullptr, OCI_ENV_DEFAULT | OCI_ENV_CONTEXT | OCI_ENV_THREADED) ) {
      hb_errInternal(8000, nullptr, nullptr, nullptr);
   } else if( !hb_sddRegister(&s_ocidd) ) {
      hb_errInternal(HB_EI_RDDINVALID, nullptr, nullptr, nullptr);
   }
}

static void hb_ocidd_exit(void * cargo)
{
   HB_SYMBOL_UNUSED(cargo);

   OCI_Cleanup();
}

HB_FUNC(HB_SDDOCI_REGISTER)
{
   hb_ocidd_init(nullptr);
}

/* force SQLBASE linking */
HB_FUNC_TRANSLATE(SDDOCI, SQLBASE)

HB_INIT_SYMBOLS_BEGIN(ocidd__InitSymbols)
{
   "SDDOCI", {HB_FS_PUBLIC}, {HB_FUNCNAME(SDDOCI)}, nullptr
},
HB_INIT_SYMBOLS_END(ocidd__InitSymbols)

HB_CALL_ON_STARTUP_BEGIN(_hb_ocidd_init_)
hb_vmAtInit(hb_ocidd_init, nullptr);
hb_vmAtExit(hb_ocidd_exit, nullptr);
HB_CALL_ON_STARTUP_END(_hb_ocidd_init_)

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup ocidd__InitSymbols
   #pragma startup _hb_ocidd_init_
#elif defined(HB_DATASEG_STARTUP)
   #define HB_DATASEG_BODY  \
   HB_DATASEG_FUNC(ocidd__InitSymbols) \
   HB_DATASEG_FUNC(_hb_ocidd_init_)
   #include "hbiniseg.hpp"
#endif
/* --- */

static HB_USHORT hb_errRT_OCIDD(HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ERRCODE errOsCode)
{
   auto pError = hb_errRT_New(ES_ERROR, "SDDOCI", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE);
   HB_USHORT uiAction = hb_errLaunch(pError);
   hb_itemRelease(pError);
   return uiAction;
}

static char * ocilibGetError(HB_ERRCODE * pErrCode)
{
   OCI_Error * err = OCI_GetLastError();

   char * szRet;
   int iNativeErr;

   if( err ) {
      PHB_ITEM pRet = M_HB_ITEMPUTSTR(nullptr, OCI_ErrorGetString(err));
      szRet = hb_strdup(hb_itemGetCPtr(pRet));
      hb_itemRelease(pRet);
      iNativeErr = OCI_ErrorGetOCICode(err);
   } else {
      szRet = hb_strdup("Could not get the error message");
      iNativeErr = 9999;
   }

   if( pErrCode ) {
      *pErrCode = static_cast<HB_ERRCODE>(iNativeErr);
   }

   return szRet;
}

/* --- SDD METHODS --- */

static HB_ERRCODE ocilibConnect(SQLDDCONNECTION * pConnection, PHB_ITEM pItem)
{
   void * hConn;
   void * hUser;
   void * hPass;

   OCI_Connection * cn = OCI_ConnectionCreate(static_cast<mtext*>(const_cast<char*>(M_HB_ARRAYGETSTR(pItem, 2, &hConn, nullptr))),
                             static_cast<mtext*>(const_cast<char*>(M_HB_ARRAYGETSTR(pItem, 3, &hUser, nullptr))),
                             static_cast<mtext*>(const_cast<char*>(M_HB_ARRAYGETSTR(pItem, 4, &hPass, nullptr))), OCI_SESSION_DEFAULT);

   hb_strfree(hConn);
   hb_strfree(hUser);
   hb_strfree(hPass);

   if( cn ) {
      pConnection->pSDDConn = hb_xgrab(sizeof(SDDCONN));
      (static_cast<SDDCONN*>(pConnection->pSDDConn))->pConn = cn;
      return Harbour::SUCCESS;
   }

   return Harbour::FAILURE;
}

static HB_ERRCODE ocilibDisconnect(SQLDDCONNECTION * pConnection)
{
   HB_ERRCODE errCode = OCI_ConnectionFree((static_cast<SDDCONN*>(pConnection->pSDDConn))->pConn) ? Harbour::SUCCESS : Harbour::FAILURE;
   hb_xfree(pConnection->pSDDConn);
   return errCode;
}

static HB_ERRCODE ocilibExecute(SQLDDCONNECTION * pConnection, PHB_ITEM pItem)
{
   OCI_Statement * st = OCI_StatementCreate((static_cast<SDDCONN*>(pConnection->pSDDConn))->pConn);

   char * szError;
   HB_ERRCODE errCode;

   if( !st ) {
      szError = ocilibGetError(&errCode);
      hb_errRT_OCIDD(EG_OPEN, ESQLDD_STMTALLOC, szError, hb_itemGetCPtr(pItem), errCode);
      hb_xfree(szError);
      return Harbour::FAILURE;
   }

   void * hStatement;

   if( OCI_ExecuteStmt(st, M_HB_ITEMGETSTR(pItem, &hStatement, nullptr)) ) {
      hb_strfree(hStatement);

      /* TODO: new id */
      hb_rddsqlSetError(0, nullptr, hb_itemGetCPtr(pItem), nullptr, static_cast<unsigned long>(OCI_GetAffectedRows(st)));
      OCI_StatementFree(st);
      return Harbour::SUCCESS;
   } else {
      hb_strfree(hStatement);
   }

   szError = ocilibGetError(&errCode);
   hb_rddsqlSetError(errCode, szError, hb_itemGetCPtr(pItem), nullptr, 0);
   hb_xfree(szError);
   OCI_StatementFree(st);
   return Harbour::FAILURE;
}

static HB_ERRCODE ocilibOpen(SQLBASEAREAP pArea)
{
   OCI_Statement * st = OCI_StatementCreate((static_cast<SDDCONN*>(pArea->pConnection->pSDDConn))->pConn);

   pArea->pSDDData = memset(hb_xgrab(sizeof(SDDDATA)), 0, sizeof(SDDDATA));
   auto pSDDData = static_cast<SDDDATA*>(pArea->pSDDData);

   char * szError;
   HB_ERRCODE errCode;

   if( !st ) {
      szError = ocilibGetError(&errCode);
      hb_errRT_OCIDD(EG_OPEN, ESQLDD_STMTALLOC, szError, pArea->szQuery, errCode);
      hb_xfree(szError);
      return Harbour::FAILURE;
   }

   auto pItem = hb_itemPutC(nullptr, pArea->szQuery);
   void * hQuery;

   if( !OCI_ExecuteStmt(st, M_HB_ITEMGETSTR(pItem, &hQuery, nullptr)) ) {
      hb_strfree(hQuery);
      hb_itemRelease(pItem);
      szError = ocilibGetError(&errCode);
      OCI_StatementFree(st);
      hb_errRT_OCIDD(EG_OPEN, ESQLDD_INVALIDQUERY, szError, pArea->szQuery, errCode);
      hb_xfree(szError);
      return Harbour::FAILURE;
   } else {
      hb_strfree(hQuery);
      hb_itemRelease(pItem);
   }

   OCI_Resultset * rs = OCI_GetResultset(st);

   auto uiFields = static_cast<HB_USHORT>(OCI_GetColumnCount(rs));
   SELF_SETFIELDEXTENT(&pArea->area, uiFields);

   auto pItemEof = hb_itemArrayNew(uiFields);
   pItem = hb_itemNew(nullptr);

#if 0
   HB_TRACE(HB_TR_ALWAYS, ("fieldcount=%d", iNameLen));
#endif

   errCode = 0;
   bool bError = false;
   for( HB_USHORT uiIndex = 0; uiIndex < uiFields; ++uiIndex ) {
      OCI_Column * col = OCI_GetColumn(rs, uiIndex + 1);

      if( !col ) {
         hb_itemRelease(pItemEof);
         hb_itemRelease(pItem);
         szError = ocilibGetError(nullptr);
         OCI_StatementFree(st);
         hb_errRT_OCIDD(EG_OPEN, ESQLDD_STMTDESCR + 1001, szError, pArea->szQuery, 0);
         hb_xfree(szError);
         return Harbour::FAILURE;
      }

      DBFIELDINFO dbFieldInfo{};
      PHB_ITEM pName = D_HB_ITEMPUTSTR(nullptr, OCI_ColumnGetName(col));
      dbFieldInfo.atomName = hb_itemGetCPtr(pName);

      unsigned int uiDataType = OCI_ColumnGetType(col);
      unsigned int uiSize = OCI_ColumnGetSize(col);
      int iDec = OCI_ColumnGetPrecision(col);
      bool bNullable = OCI_ColumnGetNullable(col);

      if( bNullable ) {
         dbFieldInfo.uiFlags |= HB_FF_NULLABLE;
      }

      dbFieldInfo.uiLen = static_cast<HB_USHORT>(uiSize);
      dbFieldInfo.uiDec = static_cast<HB_USHORT>(iDec);

#if 0
      HB_TRACE(HB_TR_ALWAYS, ("field: name=%s type=%d len=%d dec=%d nullable=%d %d %d %d %d", dbFieldInfo.atomName, uiDataType, uiSize, iDec, bNullable, OCI_ColumnGetScale(col), OCI_ColumnGetPrecision(col), OCI_ColumnGetFractionalPrecision(col), OCI_ColumnGetLeadingPrecision(col)));
#endif

      switch( uiDataType ) {
         case OCI_CDT_TEXT:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            break;

         case OCI_CDT_NUMERIC:
            dbFieldInfo.uiType = Harbour::DB::Field::LONG;
            /* For plain 'NUMERIC', precision is zero and scale is -127 */
            if( OCI_ColumnGetPrecision(col) > 0 ) {
               dbFieldInfo.uiLen = static_cast<HB_USHORT>(OCI_ColumnGetPrecision(col));
            }
            if( OCI_ColumnGetScale(col) >= 0 ) {
               dbFieldInfo.uiDec = static_cast<HB_USHORT>(OCI_ColumnGetScale(col));
            } else {
               dbFieldInfo.uiDec = static_cast<HB_USHORT>(hb_setGetDecimals());
            }
            break;

         case OCI_CDT_LONG:
            dbFieldInfo.uiType = Harbour::DB::Field::VARLENGTH;
            break;

         case OCI_CDT_RAW:
            dbFieldInfo.uiType = Harbour::DB::Field::BLOB;
            break;

         case OCI_CDT_DATETIME:
         case OCI_CDT_TIMESTAMP:
         case OCI_CDT_INTERVAL:
            dbFieldInfo.uiType = Harbour::DB::Field::TIME;
            break;

         default:
#if 0
            HB_TRACE(HB_TR_ALWAYS, ("new sql type=%d", uiDataType));
#endif
            bError = true;
            errCode = static_cast<HB_ERRCODE>(uiDataType);
            break;
      }

      if( !bError ) {
         switch( dbFieldInfo.uiType ) {
            case Harbour::DB::Field::STRING:
            {
               auto pStr = static_cast<char*>(hb_xgrab(static_cast<HB_SIZE>(dbFieldInfo.uiLen) + 1));
               memset(pStr, ' ', dbFieldInfo.uiLen);
               pStr[dbFieldInfo.uiLen] = '\0';
               hb_itemPutCLPtr(pItem, pStr, dbFieldInfo.uiLen);
               break;
            }
            case Harbour::DB::Field::MEMO:
            case Harbour::DB::Field::VARLENGTH:
            case Harbour::DB::Field::BLOB:
               hb_itemPutC(pItem, nullptr);
               break;

            case Harbour::DB::Field::INTEGER:
               hb_itemPutNI(pItem, 0);
               break;

            case Harbour::DB::Field::LONG:
               if( dbFieldInfo.uiDec == 0 ) {
                  hb_itemPutNLLen(pItem, 0, dbFieldInfo.uiLen);
               } else {
                  hb_itemPutNDLen(pItem, 0.0, dbFieldInfo.uiLen, dbFieldInfo.uiDec);
               }
               break;

            case Harbour::DB::Field::DOUBLE:
               hb_itemPutNDLen(pItem, 0.0, dbFieldInfo.uiLen, dbFieldInfo.uiDec);
               break;

            case Harbour::DB::Field::LOGICAL:
               hb_itemPutL(pItem, false);
               break;

            case Harbour::DB::Field::DATE:
               hb_itemPutDL(pItem, 0);
               break;

            case Harbour::DB::Field::TIME:
            case Harbour::DB::Field::TIMESTAMP:
               hb_itemPutTDT(pItem, 0, 0);
               break;

            default:
               hb_itemClear(pItem);
               bError = true;
         }

         hb_arraySetForward(pItemEof, uiIndex + 1, pItem);

         if( !bError ) {
            bError = (SELF_ADDFIELD(&pArea->area, &dbFieldInfo) == Harbour::FAILURE);
         }
      }

      hb_itemRelease(pName);

      if( bError ) {
         break;
      }
   }

   hb_itemRelease(pItem);

   if( bError ) {
      hb_itemRelease(pItemEof);
      OCI_StatementFree(st);
      hb_errRT_OCIDD(EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", pArea->szQuery, errCode);
      return Harbour::FAILURE;
   }

   pArea->ulRecCount = 0;
   pArea->ulRecMax = SQLDD_ROWSET_INIT;

   pArea->pRow = static_cast<void**>(hb_xgrab(SQLDD_ROWSET_INIT * sizeof(void*)));
   pArea->pRowFlags = static_cast<HB_BYTE*>(hb_xgrab(SQLDD_ROWSET_INIT * sizeof(HB_BYTE)));

   pArea->pRow[0] = pItemEof;
   pArea->pRowFlags[0] = SQLDD_FLAG_CACHED;

   pSDDData->pStmt = st;
   return Harbour::SUCCESS;
}

static HB_ERRCODE ocilibClose(SQLBASEAREAP pArea)
{
   auto pSDDData = static_cast<SDDDATA*>(pArea->pSDDData);

   if( pSDDData ) {
      if( pSDDData->pStmt ) {
         OCI_StatementFree(pSDDData->pStmt);
      }

      hb_xfree(pSDDData);
      pArea->pSDDData = nullptr;
   }
   return Harbour::SUCCESS;
}

static HB_ERRCODE ocilibGoTo(SQLBASEAREAP pArea, HB_ULONG ulRecNo)
{
   OCI_Statement * st = (static_cast<SDDDATA*>(pArea->pSDDData))->pStmt;
   OCI_Resultset * rs = OCI_GetResultset(st);

   while( ulRecNo > pArea->ulRecCount && !pArea->fFetched ) {
      if( !OCI_FetchNext(rs) ) {
         pArea->fFetched = true;
         break;
      }

      auto pArray = hb_itemArrayNew(pArea->area.uiFieldCount);

      PHB_ITEM  pItem = nullptr;

      for( HB_USHORT ui = 1; ui <= pArea->area.uiFieldCount; ++ui ) {
         LPFIELD  pField = pArea->area.lpFields + ui - 1;

         switch( pField->uiType ) {
            case Harbour::DB::Field::STRING:
               if( OCI_IsNull(rs, ui) ) {
                  auto pStr = static_cast<char*>(hb_xgrab(static_cast<HB_SIZE>(pField->uiLen) + 1));
                  memset(pStr, ' ', pField->uiLen);
                  pStr[pField->uiLen] = '\0';
                  pItem = hb_itemPutCLPtr(pItem, pStr, pField->uiLen);
               } else {
                  const dtext * val;
                  if( (val = OCI_GetString(rs, ui)) != nullptr ) {
                     //pItem = D_HB_ITEMPUTSTRLEN(pItem, val, static_cast<HB_SIZE>(dtslen(val)));  /* TODO: Pad it to pField->uiLen size with spaces? */
                     pItem = D_HB_ITEMPUTSTR(pItem, val);  /* TODO: Pad it to pField->uiLen size with spaces? */
                  }
               }
               break;

            case Harbour::DB::Field::LONG:
            case Harbour::DB::Field::INTEGER:
               if( pField->uiDec == 0 ) {
#if HB_VMLONG_MAX == INT32_MAX || defined(HB_LONG_LONG_OFF)
                  pItem = hb_itemPutNIntLen(pItem, OCI_GetInt(rs, ui), pField->uiLen);
#else
                  pItem = hb_itemPutNIntLen(pItem, OCI_GetBigInt(rs, ui), pField->uiLen);
#endif
               } else {
                  pItem = hb_itemPutNDLen(pItem, OCI_GetDouble(rs, ui), pField->uiLen, pField->uiDec);
               }
               break;

            case Harbour::DB::Field::VARLENGTH:
            case Harbour::DB::Field::MEMO:
            {
               OCI_Long * val = OCI_GetLong(rs, ui);
               if( val ) {
                  unsigned int uiSize = OCI_LongGetSize(val);
                  if( OCI_LongGetType(val) == OCI_CLONG ) {
                     pItem = D_HB_ITEMPUTSTRLEN(pItem, static_cast<D_HB_CHAR*>(OCI_LongGetBuffer(val)), uiSize);
                  } else {
                     pItem = hb_itemPutCL(pItem, static_cast<const char*>(OCI_LongGetBuffer(val)), uiSize);
                  }
               }
               break;
            }

            case Harbour::DB::Field::IMAGE:
            case Harbour::DB::Field::BLOB:
            case Harbour::DB::Field::OLE:
            {
               OCI_Long * val = OCI_GetLong(rs, ui);
               if( val ) {
                  pItem = hb_itemPutCL(pItem, static_cast<const char*>(OCI_LongGetBuffer(val)), OCI_LongGetSize(val));
               }
               break;
            }

            case Harbour::DB::Field::CURRENCY:
            case Harbour::DB::Field::CURDOUBLE:
            case Harbour::DB::Field::FLOAT:
            case Harbour::DB::Field::DOUBLE:
               pItem = hb_itemPutNDLen(pItem, OCI_GetDouble(rs, ui), pField->uiLen, pField->uiDec);
               break;

            case Harbour::DB::Field::DATE:
            {
               OCI_Date * date = OCI_GetDate(rs, ui);
               int iYear, iMonth, iDay;
               if( date && OCI_DateGetDate(date, &iYear, &iMonth, &iDay) ) {
                  pItem = hb_itemPutD(pItem, iYear, iMonth, iDay);
               }
               break;
            }

            case Harbour::DB::Field::TIME:
            {
               OCI_Date * date = OCI_GetDate(rs, ui);
               int iYear, iMonth, iDay, iHour, iMin, iSec;

               if( date && OCI_DateGetDateTime(date, &iYear, &iMonth, &iDay, &iHour, &iMin, &iSec) ) {
                  pItem = hb_itemPutTDT(pItem, hb_dateEncode(iYear, iMonth, iDay), hb_timeEncode(iHour, iMin, iSec, 0));
               }
               break;
            }

            case Harbour::DB::Field::TIMESTAMP:
            {
               OCI_Timestamp * ts = OCI_GetTimestamp(rs, ui);
               int iYear, iMonth, iDay, iHour, iMin, iSec, iFSec;
               if( ts && OCI_TimestampGetDateTime(ts, &iYear, &iMonth, &iDay, &iHour, &iMin, &iSec, &iFSec) ) {
                  pItem = hb_itemPutTDT(pItem, hb_dateEncode(iYear, iMonth, iDay), hb_timeEncode(iHour, iMin, iSec, iFSec / 1000000));
               }
               break;
            }
         }

         if( pItem != nullptr ) {
            hb_arraySetForward(pArray, ui, pItem);
         }
      }
      hb_itemRelease(pItem);

      if( pArea->ulRecCount + 1 >= pArea->ulRecMax ) {
         pArea->pRow = static_cast<void**>(hb_xrealloc(pArea->pRow, (pArea->ulRecMax + SQLDD_ROWSET_RESIZE) * sizeof(void*)));
         pArea->pRowFlags = static_cast<HB_BYTE*>(hb_xrealloc(pArea->pRowFlags, (pArea->ulRecMax + SQLDD_ROWSET_RESIZE) * sizeof(HB_BYTE)));
         pArea->ulRecMax += SQLDD_ROWSET_RESIZE;
      }

      pArea->ulRecCount++;
      pArea->pRow[pArea->ulRecCount] = pArray;
      pArea->pRowFlags[pArea->ulRecCount] = SQLDD_FLAG_CACHED;
   }

   if( ulRecNo == 0 || ulRecNo > pArea->ulRecCount ) {
      pArea->pRecord = pArea->pRow[0];
      pArea->bRecordFlags = pArea->pRowFlags[0];
      pArea->fPositioned = false;
   } else {
      pArea->pRecord = pArea->pRow[ulRecNo];
      pArea->bRecordFlags = pArea->pRowFlags[ulRecNo];
      pArea->fPositioned = true;
   }

   return Harbour::SUCCESS;
}
