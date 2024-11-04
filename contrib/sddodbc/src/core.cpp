//
// ODBC Database Driver
//
// Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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

#include "hbrddsql.hpp"

#include <hbapiitm.hpp>
#include <hbdate.hpp>
#include <hbapistr.hpp>
#include <hbset.hpp>
#include <hbvm.hpp>

/* Required by headers on Windows */
#if defined(HB_OS_WIN)
   #include <windows.h>
/* Required for WIN32_LEAN_AND_MEAN mode */
   #if !defined(WIN32)
      #define WIN32
   #endif
#endif

#include <sql.h>
#include <sqlext.h>

#if !defined(HB_OS_WIN)
   #if !defined(SQLLEN) && !defined(SQLTCHAR) && !defined(UODBCINT64) && !defined(SIZEOF_LONG_INT)
      using SQLTCHAR = unsigned char;
      using SQLLEN = long;
      using SQLULEN = unsigned long;
      #ifndef SQL_WCHAR
         #define SQL_WCHAR         (-8)
      #endif
      #ifndef SQL_WVARCHAR
         #define SQL_WVARCHAR      (-9)
      #endif
      #ifndef SQL_WLONGVARCHAR
         #define SQL_WLONGVARCHAR  (-10)
      #endif
   #endif
#endif

#if defined(UNICODE)
   #define O_HB_ARRAYGETSTR(arr, n, phstr, plen)  hb_arrayGetStrU16(arr, n, HB_CDP_ENDIAN_NATIVE, phstr, plen)
   #define O_HB_ITEMCOPYSTR(itm, str, len)        hb_itemCopyStrU16(itm, HB_CDP_ENDIAN_NATIVE, str, len)
   #define O_HB_ITEMGETSTR(itm, phstr, plen)      hb_itemGetStrU16(itm, HB_CDP_ENDIAN_NATIVE, phstr, plen)
   #define O_HB_ITEMPUTSTR(itm, str)              hb_itemPutStrU16(itm, HB_CDP_ENDIAN_NATIVE, str)
   #define O_HB_ITEMPUTSTRLEN(itm, str, len)      hb_itemPutStrLenU16(itm, HB_CDP_ENDIAN_NATIVE, str, len)
   #define O_HB_CHARDUP(str)                      hb_osStrU16Encode(str)
   #define O_HB_OSSTRDUP(str)                     hb_osStrU16Decode(str)
   #define O_HB_STRLEN(str)                       hb_wstrlen(str)
   #define O_HB_CHAR  HB_WCHAR
#else
   #define O_HB_ARRAYGETSTR(arr, n, phstr, plen)  hb_arrayGetStr(arr, n, hb_setGetOSCP(), phstr, plen)
   #define O_HB_ITEMCOPYSTR(itm, str, len)        hb_itemCopyStr(itm, hb_setGetOSCP(), str, len)
   #define O_HB_ITEMGETSTR(itm, phstr, plen)      hb_itemGetStr(itm, hb_setGetOSCP(), phstr, plen)
   #define O_HB_ITEMPUTSTR(itm, str)              hb_itemPutStr(itm, hb_setGetOSCP(), str)
   #define O_HB_ITEMPUTSTRLEN(itm, str, len)      hb_itemPutStrLen(itm, hb_setGetOSCP(), str, len)
   #define O_HB_CHARDUP(str)                      hb_osStrEncode(str)
   #define O_HB_OSSTRDUP(str)                     hb_osStrDecode(str)
   #define O_HB_STRLEN(str)                       strlen(str)
   #define O_HB_CHAR  char
#endif

struct SDDCONN
{
   SQLHENV hEnv;
   SQLHDBC hConn;
};

struct SDDDATA
{
   SQLHSTMT hStmt;
};

static HB_ERRCODE odbcConnect(SQLDDCONNECTION * pConnection, PHB_ITEM pItem);
static HB_ERRCODE odbcDisconnect(SQLDDCONNECTION * pConnection);
static HB_ERRCODE odbcExecute(SQLDDCONNECTION * pConnection, PHB_ITEM pItem);
static HB_ERRCODE odbcOpen(SQLBASEAREAP pArea);
static HB_ERRCODE odbcClose(SQLBASEAREAP pArea);
static HB_ERRCODE odbcGoTo(SQLBASEAREAP pArea, HB_ULONG ulRecNo);

static SDDNODE s_odbcdd =
{
   nullptr,
   "ODBC",
   static_cast<SDDFUNC_CONNECT>(odbcConnect),
   static_cast<SDDFUNC_DISCONNECT>(odbcDisconnect),
   static_cast<SDDFUNC_EXECUTE>(odbcExecute),
   static_cast<SDDFUNC_OPEN>(odbcOpen),
   static_cast<SDDFUNC_CLOSE>(odbcClose),
   static_cast<SDDFUNC_GOTO>(odbcGoTo),
   static_cast<SDDFUNC_GETVALUE>(nullptr),
   static_cast<SDDFUNC_GETVARLEN>(nullptr)
};

static void hb_odbcdd_init(void * cargo)
{
   HB_SYMBOL_UNUSED(cargo);

   if( !hb_sddRegister(&s_odbcdd) ) {
      hb_errInternal(HB_EI_RDDINVALID, nullptr, nullptr, nullptr);
   }
}

HB_FUNC( HB_SDDODBC_REGISTER )
{
   hb_odbcdd_init(nullptr);
}

/* force SQLBASE linking */
HB_FUNC_TRANSLATE(SDDODBC, SQLBASE)

HB_INIT_SYMBOLS_BEGIN(odbcdd__InitSymbols)
{
   "SDDODBC", {HB_FS_PUBLIC}, {HB_FUNCNAME(SDDODBC)}, nullptr
},
HB_INIT_SYMBOLS_END(odbcdd__InitSymbols)

HB_CALL_ON_STARTUP_BEGIN(_hb_odbcdd_init_)
hb_vmAtInit(hb_odbcdd_init, nullptr);
HB_CALL_ON_STARTUP_END(_hb_odbcdd_init_)

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup odbcdd__InitSymbols
   #pragma startup _hb_odbcdd_init_
#elif defined(HB_DATASEG_STARTUP)
   #define HB_DATASEG_BODY  \
   HB_DATASEG_FUNC(odbcdd__InitSymbols) \
   HB_DATASEG_FUNC(_hb_odbcdd_init_)
   #include "hbiniseg.hpp"
#endif

/* --- */

static HB_USHORT hb_errRT_ODBCDD(HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ERRCODE errOsCode)
{
   auto pError = hb_errRT_New(ES_ERROR, "SDDODBC", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE);
   HB_USHORT uiAction = hb_errLaunch(pError);
   hb_itemRelease(pError);
   return uiAction;
}

static char * odbcGetError(SQLHENV hEnv, SQLHDBC hConn, SQLHSTMT hStmt, HB_ERRCODE * pErrCode)
{
   SQLTCHAR szError[6 + SQL_MAX_MESSAGE_LENGTH];
   SQLINTEGER iNativeErr = 9999;
   SQLSMALLINT iLen;
   char * szRet;

   if( SQL_SUCCEEDED(SQLError(hEnv, hConn, hStmt, szError, &iNativeErr, szError + 6, SQL_MAX_MESSAGE_LENGTH, &iLen)) ) {
      szError[5] = ' ';
      PHB_ITEM pRet = O_HB_ITEMPUTSTR(nullptr, reinterpret_cast<O_HB_CHAR*>(szError));
      szRet = hb_strdup(hb_itemGetCPtr(pRet));
      hb_itemRelease(pRet);
   } else {
      szRet = hb_strdup("HY000 Could not get the error message");
   }

   if( pErrCode ) {
      *pErrCode = static_cast<HB_ERRCODE>(iNativeErr);
   }
   return szRet;
}

/* --- SDD METHODS --- */

static HB_ERRCODE odbcConnect(SQLDDCONNECTION * pConnection, PHB_ITEM pItem)
{
   SQLHENV hEnv = nullptr;
   SQLHDBC hConnect = nullptr;
   char * szError;
   HB_ERRCODE errCode;

#if ODBCVER >= 0x0300
   if( SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv)) )
#else
   if( SQL_SUCCEEDED(SQLAllocEnv(&hEnv)) )
#endif
   {
#if ODBCVER >= 0x0300
      SQLSetEnvAttr(hEnv, SQL_ATTR_ODBC_VERSION, reinterpret_cast<SQLPOINTER>(SQL_OV_ODBC3), SQL_IS_UINTEGER);
#endif

#if ODBCVER >= 0x0300
      if( SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_DBC, hEnv, &hConnect)) )
#else
      if( SQL_SUCCEEDED(SQLAllocConnect(hEnv, &hConnect)) )
#endif
      {
         const O_HB_CHAR * pchConStr;
         void * hConnStr;
         HB_SIZE nConnLen;
         SQLTCHAR cBuffer[1024];
         SQLSMALLINT iLen = HB_SIZEOFARRAY(cBuffer);

         cBuffer[0] = '\0';
         pchConStr = O_HB_ARRAYGETSTR(pItem, 2, &hConnStr, &nConnLen);

         if( SQL_SUCCEEDED(SQLDriverConnect(hConnect,
                                            nullptr,
                                            reinterpret_cast<SQLTCHAR*>(const_cast<O_HB_CHAR*>(pchConStr)),
                                            static_cast<SQLSMALLINT>(nConnLen),
                                            cBuffer,
                                            HB_SIZEOFARRAY(cBuffer),
                                            &iLen,
                                            SQL_DRIVER_NOPROMPT)) ) {
            hb_strfree(hConnStr);
            pConnection->pSDDConn = hb_xgrab(sizeof(SDDCONN));
            (static_cast<SDDCONN*>(pConnection->pSDDConn))->hConn = hConnect;
            (static_cast<SDDCONN*>(pConnection->pSDDConn))->hEnv = hEnv;
            return Harbour::SUCCESS;
         } else {
            hb_strfree(hConnStr);
            szError = odbcGetError(hEnv, hConnect, SQL_NULL_HSTMT, &errCode);
            hb_rddsqlSetError(errCode, szError, nullptr, nullptr, 0);
            hb_xfree(szError);
         }
#if ODBCVER >= 0x0300
         SQLFreeHandle(SQL_HANDLE_DBC, hConnect);
#else
         SQLFreeConnect(hConnect);
#endif
      } else {
         szError = odbcGetError(hEnv, SQL_NULL_HDBC, SQL_NULL_HSTMT, &errCode);
         hb_errRT_ODBCDD(EG_OPEN, ESQLDD_CONNALLOC, szError, hb_arrayGetCPtr(pItem, 2), errCode);
         hb_xfree(szError);
      }
#if ODBCVER >= 0x0300
      SQLFreeHandle(SQL_HANDLE_ENV, hEnv);
#else
      SQLFreeEnv(hEnv);
#endif
   } else {
      szError = odbcGetError(SQL_NULL_HENV, SQL_NULL_HDBC, SQL_NULL_HSTMT, &errCode);
      hb_errRT_ODBCDD(EG_OPEN, ESQLDD_ENVALLOC, szError, hb_arrayGetCPtr(pItem, 2), errCode);
      hb_xfree(szError);
   }
   return Harbour::FAILURE;
}

static HB_ERRCODE odbcDisconnect(SQLDDCONNECTION * pConnection)
{
   auto pSDDConn = static_cast<SDDCONN*>(pConnection->pSDDConn);

   SQLDisconnect(pSDDConn->hConn);
#if ODBCVER >= 0x0300
   SQLFreeHandle(SQL_HANDLE_DBC, pSDDConn->hConn);
   SQLFreeHandle(SQL_HANDLE_ENV, pSDDConn->hEnv);
#else
   SQLFreeConnect(pSDDConn->hConn);
   SQLFreeEnv(pSDDConn->hEnv);
#endif
   hb_xfree(pSDDConn);
   return Harbour::SUCCESS;
}

static HB_ERRCODE odbcExecute(SQLDDCONNECTION * pConnection, PHB_ITEM pItem)
{
   auto pSDDConn = static_cast<SDDCONN*>(pConnection->pSDDConn);
   SQLHSTMT hStmt;
   char * szError;
   HB_ERRCODE errCode;

#if ODBCVER >= 0x0300
   if( !SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_STMT, pSDDConn->hConn, &hStmt)) )
#else
   if( !SQL_SUCCEEDED(SQLAllocStmt(pSDDConn->hConn, &hStmt)) )
#endif
   {
      szError = odbcGetError(pSDDConn->hEnv, pSDDConn->hConn, SQL_NULL_HSTMT, &errCode);
      hb_errRT_ODBCDD(EG_OPEN, ESQLDD_STMTALLOC, szError, hb_itemGetCPtr(pItem), errCode);
      hb_xfree(szError);
      return Harbour::FAILURE;
   }

   void * hStatement;
   HB_SIZE nStatementLen;

   const O_HB_CHAR * pchStatement = O_HB_ITEMGETSTR(pItem, &hStatement, &nStatementLen);
   SQLRETURN result = SQLExecDirect(hStmt,
      reinterpret_cast<SQLTCHAR*>(const_cast<O_HB_CHAR*>(pchStatement)),
      static_cast<SQLINTEGER>(nStatementLen));
   hb_strfree(hStatement);

   SQLLEN iCount;

   if( SQL_SUCCEEDED(result) ) {

      if( SQL_SUCCEEDED(SQLRowCount(hStmt, &iCount)) ) {
         /* TODO: new id */
         hb_rddsqlSetError(0, nullptr, hb_itemGetCPtr(pItem), nullptr, static_cast<unsigned long>(iCount));
#if ODBCVER >= 0x0300
         SQLFreeHandle(SQL_HANDLE_STMT, hStmt);
#else
         SQLFreeStmt(hStmt, SQL_DROP);
#endif
         return Harbour::SUCCESS;
      }
   }

   szError = odbcGetError(pSDDConn->hEnv, pSDDConn->hConn, hStmt, &errCode);
   hb_rddsqlSetError(errCode, szError, hb_itemGetCPtr(pItem), nullptr, errCode);
   hb_xfree(szError);
#if ODBCVER >= 0x0300
   SQLFreeHandle(SQL_HANDLE_STMT, hStmt);
#else
   SQLFreeStmt(hStmt, SQL_DROP);
#endif
   return Harbour::FAILURE;
}

static HB_ERRCODE odbcOpen(SQLBASEAREAP pArea)
{
   auto pSDDConn = static_cast<SDDCONN*>(pArea->pConnection->pSDDConn);

   pArea->pSDDData = memset(hb_xgrab(sizeof(SDDDATA)), 0, sizeof(SDDDATA));
   auto pSDDData = static_cast<SDDDATA*>(pArea->pSDDData);

   SQLHSTMT hStmt;
   char * szError;
   HB_ERRCODE errCode;

#if ODBCVER >= 0x0300
   if( !SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_STMT, pSDDConn->hConn, &hStmt)) )
#else
   if( !SQL_SUCCEEDED(SQLAllocStmt(pSDDConn->hConn, &hStmt)) )
#endif
   {
      szError = odbcGetError(pSDDConn->hEnv, pSDDConn->hConn, SQL_NULL_HSTMT, &errCode);
      hb_errRT_ODBCDD(EG_OPEN, ESQLDD_STMTALLOC, szError, pArea->szQuery, errCode);
      hb_xfree(szError);
      return Harbour::FAILURE;
   }

   O_HB_CHAR * pchQuery = O_HB_CHARDUP(pArea->szQuery);
   SQLRETURN result = SQLExecDirect(hStmt, reinterpret_cast<SQLTCHAR*>(const_cast<O_HB_CHAR*>(pchQuery)), static_cast<SQLINTEGER>(O_HB_STRLEN(pchQuery)));
   hb_xfree(pchQuery);

   if( !SQL_SUCCEEDED(result) ) {
      szError = odbcGetError(pSDDConn->hEnv, pSDDConn->hConn, hStmt, &errCode);
#if ODBCVER >= 0x0300
      SQLFreeHandle(SQL_HANDLE_STMT, hStmt);
#else
      SQLFreeStmt(hStmt, SQL_DROP);
#endif
      hb_errRT_ODBCDD(EG_OPEN, ESQLDD_INVALIDQUERY, szError, pArea->szQuery, errCode);
      hb_xfree(szError);
      return Harbour::FAILURE;
   }

   SQLSMALLINT iNameLen;

   if( !SQL_SUCCEEDED(SQLNumResultCols(hStmt, &iNameLen)) ) {
      szError = odbcGetError(pSDDConn->hEnv, pSDDConn->hConn, hStmt, &errCode);
#if ODBCVER >= 0x0300
      SQLFreeHandle(SQL_HANDLE_STMT, hStmt);
#else
      SQLFreeStmt(hStmt, SQL_DROP);
#endif
      hb_errRT_ODBCDD(EG_OPEN, ESQLDD_STMTDESCR + 1000, szError, pArea->szQuery, errCode);
      hb_xfree(szError);
      return Harbour::FAILURE;
   }

   auto uiFields = static_cast<HB_USHORT>(iNameLen);
   SELF_SETFIELDEXTENT(&pArea->area, uiFields);

   auto pItemEof = hb_itemArrayNew(uiFields);
   auto pItem = hb_itemNew(nullptr);

#if 0
   HB_TRACE(HB_TR_ALWAYS, ("fieldcount=%d", iNameLen));
#endif

   errCode = 0;
   bool bError = false;
   for( HB_USHORT uiIndex = 0; uiIndex < uiFields; uiIndex++ ) {
      SQLTCHAR cName[256];
      SQLSMALLINT iDataType;
      SQLULEN uiSize;
      SQLSMALLINT iDec, iNull;

      if( !SQL_SUCCEEDED(SQLDescribeCol(hStmt, static_cast<SQLSMALLINT>(uiIndex) + 1,
                         cName, HB_SIZEOFARRAY(cName),
                         &iNameLen, &iDataType, &uiSize, &iDec, &iNull)) ) {
         hb_itemRelease(pItemEof);
         hb_itemRelease(pItem);
         szError = odbcGetError(pSDDConn->hEnv, pSDDConn->hConn, hStmt, nullptr);
#if ODBCVER >= 0x0300
         SQLFreeHandle(SQL_HANDLE_STMT, hStmt);
#else
         SQLFreeStmt(hStmt, SQL_DROP);
#endif
         hb_errRT_ODBCDD(EG_OPEN, ESQLDD_STMTDESCR + 1001, szError, pArea->szQuery, 0);
         hb_xfree(szError);
         return Harbour::FAILURE;
      }

      char * pszName;
      DBFIELDINFO dbFieldInfo{};
      dbFieldInfo.atomName = pszName = O_HB_OSSTRDUP(reinterpret_cast<O_HB_CHAR*>(cName));

      /*
         We do mapping of many SQL types to one Harbour field type here, so, we need store
         real SQL type in uiTypeExtended. SQL types are signed, so, HB_USHORT type casting
         is a little hackish. We need to remember use this casting also in expressions like
         this:
            if( pField->uiTypeExtended == static_cast<HB_USHORT>(SQL_BIGINT) )
         or introduce our own unsigned SQL types.
         [Mindaugas]
       */
      dbFieldInfo.uiTypeExtended = static_cast<HB_USHORT>(iDataType);
      dbFieldInfo.uiLen = static_cast<HB_USHORT>(uiSize);
      dbFieldInfo.uiDec = iDec;
      if( iNull == SQL_NULLABLE ) {
         dbFieldInfo.uiFlags |= HB_FF_NULLABLE;
      }

#if 0
      HB_TRACE(HB_TR_ALWAYS, ("field: name=%s type=%d len=%d dec=%d null=%d", dbFieldInfo.atomName, iDataType, uiSize, iDec, iNull));
#endif

      switch( iDataType ) {
         case SQL_CHAR:
         case SQL_VARCHAR:
         case SQL_LONGVARCHAR:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            break;

         case SQL_WCHAR:
         case SQL_WVARCHAR:
         case SQL_WLONGVARCHAR:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            dbFieldInfo.uiFlags |= HB_FF_UNICODE;
            break;

         case SQL_BINARY:
         case SQL_VARBINARY:
         case SQL_LONGVARBINARY:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            dbFieldInfo.uiFlags |= HB_FF_BINARY;
            break;

         case SQL_TINYINT:
         case SQL_SMALLINT:
         case SQL_INTEGER:
         case SQL_BIGINT:
            dbFieldInfo.uiType = Harbour::DB::Field::INTEGER;
            break;

         case SQL_DECIMAL:
         case SQL_NUMERIC:
            dbFieldInfo.uiType = Harbour::DB::Field::LONG;
            break;

         case SQL_REAL:
         case SQL_FLOAT:
         case SQL_DOUBLE:
            dbFieldInfo.uiType = Harbour::DB::Field::DOUBLE;
            break;

         case SQL_BIT:
            dbFieldInfo.uiType = Harbour::DB::Field::LOGICAL;
            break;

         case SQL_DATE:
#if ODBCVER >= 0x0300
         case SQL_TYPE_DATE:
#endif
            dbFieldInfo.uiType = Harbour::DB::Field::DATE;
            break;

         case SQL_TIME:
#if ODBCVER >= 0x0300
         case SQL_TYPE_TIME:
#endif
            dbFieldInfo.uiType = Harbour::DB::Field::TIME;
            break;

         /* SQL_DATETIME = SQL_DATE = 9 */
         case SQL_TIMESTAMP:
#if ODBCVER >= 0x0300
         case SQL_TYPE_TIMESTAMP:
#endif
            dbFieldInfo.uiType = Harbour::DB::Field::TIMESTAMP;
            break;

         default:
#if 0
            HB_TRACE(HB_TR_ALWAYS, ("new sql type=%d", iDataType));
#endif
            bError = true;
            errCode = static_cast<HB_ERRCODE>(iDataType);
            break;
      }

      if( !bError ) {
         switch( dbFieldInfo.uiType ) {
            case Harbour::DB::Field::STRING:
            {
               auto pStr = static_cast<char*>(hb_xgrab(static_cast<HB_SIZE>(dbFieldInfo.uiLen) + 1));
               memset(pStr, ' ', dbFieldInfo.uiLen);
               pStr[dbFieldInfo.uiLen] = '\0';
               hb_itemPutCL(pItem, pStr, dbFieldInfo.uiLen);
               hb_xfree(pStr);
               break;
            }

            case Harbour::DB::Field::MEMO:
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
               hb_itemPutTDT(pItem, 0, 0);
               break;

            case Harbour::DB::Field::TIMESTAMP:
               hb_itemPutTDT(pItem, 0, 0);
               break;

            default:
               hb_itemClear(pItem);
               bError = true;
               break;
         }

         hb_arraySetForward(pItemEof, uiIndex + 1, pItem);

         if( !bError ) {
            bError = (SELF_ADDFIELD(&pArea->area, &dbFieldInfo) == Harbour::FAILURE);
         }

         hb_xfree(pszName);
      }

      if( bError ) {
         break;
      }
   }
   hb_itemRelease(pItem);

   if( bError ) {
      hb_itemRelease(pItemEof);
#if ODBCVER >= 0x0300
      SQLFreeHandle(SQL_HANDLE_STMT, hStmt);
#else
      SQLFreeStmt(hStmt, SQL_DROP);
#endif
      hb_errRT_ODBCDD(EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", pArea->szQuery, errCode);
      return Harbour::FAILURE;
   }

   pArea->ulRecCount = 0;
   pArea->ulRecMax = SQLDD_ROWSET_INIT;

   pArea->pRow = static_cast<void**>(hb_xgrab(SQLDD_ROWSET_INIT * sizeof(void*)));
   pArea->pRowFlags = static_cast<HB_BYTE*>(hb_xgrab(SQLDD_ROWSET_INIT * sizeof(HB_BYTE)));

   pArea->pRow[0] = pItemEof;
   pArea->pRowFlags[0] = SQLDD_FLAG_CACHED;

   pSDDData->hStmt = hStmt;
   return Harbour::SUCCESS;
}

static HB_ERRCODE odbcClose(SQLBASEAREAP pArea)
{
   auto pSDDData = static_cast<SDDDATA*>(pArea->pSDDData);

   if( pSDDData ) {
      if( pSDDData->hStmt ) {
#if ODBCVER >= 0x0300
         SQLFreeHandle(SQL_HANDLE_STMT, pSDDData->hStmt);
#else
         SQLFreeStmt(pSDDData->hStmt, SQL_DROP);
#endif
      }

      hb_xfree(pSDDData);
      pArea->pSDDData = nullptr;
   }
   return Harbour::SUCCESS;
}

static HB_ERRCODE odbcGoTo(SQLBASEAREAP pArea, HB_ULONG ulRecNo)
{
   /* No pArea->pSDDData for DBCreate() area...
    * though pArea->fFetched == true for them
    */
   SQLHSTMT hStmt = pArea->pSDDData ? (static_cast<SDDDATA*>(pArea->pSDDData))->hStmt : nullptr;

   SQLRETURN res;
   SQLLEN iLen;
   PHB_ITEM pArray, pItem;
   LPFIELD pField;
   HB_USHORT ui;

   while( ulRecNo > pArea->ulRecCount && !pArea->fFetched ) {
      if( !SQL_SUCCEEDED(SQLFetch(hStmt)) ) {
         pArea->fFetched = true;
         break;
      }

      pArray = hb_itemArrayNew(pArea->area.uiFieldCount);
      pItem = nullptr;
      for( ui = 1; ui <= pArea->area.uiFieldCount; ui++ ) {
         iLen = SQL_NULL_DATA;
         res = 0;
         pField = pArea->area.lpFields + ui - 1;
         switch( pField->uiType ) {
            case Harbour::DB::Field::STRING:
               if( pField->uiType & HB_FF_BINARY ) {
                  char buffer[1];
                  iLen = 0;
                  if( SQL_SUCCEEDED(res = SQLGetData(hStmt, ui, SQL_C_BINARY, buffer, 0, &iLen)) ) {
                     if( iLen >= 0 ) {
                        auto val = static_cast<char*>(hb_xgrab(iLen + 1));
                        if( SQL_SUCCEEDED(res = SQLGetData(hStmt, ui, SQL_C_BINARY, val, iLen + 1, &iLen)) ) {
                           pItem = hb_itemPutCLPtr(pItem, val, static_cast<HB_SIZE>(iLen));
                        } else {
                           hb_xfree(val);
                        }
                     }
                  }
               } else {
                  O_HB_CHAR buffer[1];
#if defined(UNICODE)
                  SQLSMALLINT iTargetType = SQL_C_WCHAR;
#else
                  SQLSMALLINT iTargetType = SQL_C_CHAR;
#endif
                  iLen = 0;
                  if( SQL_SUCCEEDED(res = SQLGetData(hStmt, ui, iTargetType, buffer, 0, &iLen)) ) {
                     if( iLen >= 0 ) {
                        auto val = static_cast<O_HB_CHAR*>(hb_xgrab(iLen + sizeof(O_HB_CHAR)));
                        if( SQL_SUCCEEDED(res = SQLGetData(hStmt, ui, iTargetType, val, iLen + sizeof(O_HB_CHAR), &iLen)) ) {
#if defined(UNICODE)
                           iLen >>= 1;
#endif
                           pItem = O_HB_ITEMPUTSTRLEN(pItem, val, static_cast<HB_SIZE>(iLen));
                        }
                        hb_xfree(val);
                     }
                  }
               }
               break;

            case Harbour::DB::Field::INTEGER:
#if ODBCVER >= 0x0300
               if( pField->uiTypeExtended == static_cast<HB_USHORT>(SQL_BIGINT) ) {
                  HB_I64 val = 0;
                  /* NOTE: SQL_C_SBIGINT not available before ODBC 3.0 */
                  if( SQL_SUCCEEDED(res = SQLGetData(hStmt, ui, SQL_C_SBIGINT, &val, sizeof(val), &iLen)) ) {
                     pItem = hb_itemPutNIntLen(pItem, val, pField->uiLen);
                  }
               }
               else
#endif
               {
                  SQLINTEGER val = 0;
                  if( SQL_SUCCEEDED(res = SQLGetData(hStmt, ui, SQL_C_LONG, &val, sizeof(val), &iLen)) ) {
                     pItem = hb_itemPutNLLen(pItem, val, pField->uiLen);
                  }
               }
               break;

            case Harbour::DB::Field::LONG:
               if( pField->uiDec == 0 && pField->uiLen < 10 ) {
                  SQLINTEGER val = 0;
                  if( SQL_SUCCEEDED(res = SQLGetData(hStmt, ui, SQL_C_LONG, &val, sizeof(val), &iLen)) ) {
                     pItem = hb_itemPutNLLen(pItem, val, pField->uiLen);
                  }
               } else {
                  double val = 0.0;
                  if( SQL_SUCCEEDED(res = SQLGetData(hStmt, ui, SQL_C_DOUBLE, &val, sizeof(val), &iLen)) ) {
                     pItem = hb_itemPutNDLen(pItem, val, pField->uiLen, pField->uiDec);
                  }
               }
               break;

            case Harbour::DB::Field::DOUBLE:
            {
               double val = 0.0;
               if( SQL_SUCCEEDED(res = SQLGetData(hStmt, ui, SQL_C_DOUBLE, &val, sizeof(val), &iLen)) ) {
                  pItem = hb_itemPutNDLen(pItem, val, pField->uiLen, pField->uiDec);
               }
               break;
            }

            case Harbour::DB::Field::LOGICAL:
            {
               unsigned char val = 0;
               if( SQL_SUCCEEDED(res = SQLGetData(hStmt, ui, SQL_C_BIT, &val, sizeof(val), &iLen)) ) {
                  pItem = hb_itemPutL(pItem, val != 0);
               }
               break;
            }

            case Harbour::DB::Field::DATE:
            {
               DATE_STRUCT val = {0, 0, 0};
               if( SQL_SUCCEEDED(res = SQLGetData(hStmt, ui, SQL_C_DATE, &val, sizeof(val), &iLen)) ) {
                  pItem = hb_itemPutD(pItem, val.year, val.month, val.day);
               }
               break;
            }

            case Harbour::DB::Field::TIME:
            {
               TIME_STRUCT val = {0, 0, 0};
               if( SQL_SUCCEEDED(res = SQLGetData(hStmt, ui, SQL_C_TIME, &val, sizeof(val), &iLen)) ) {
                  pItem = hb_itemPutTDT(pItem, 0, hb_timeEncode(val.hour, val.minute, val.second, 0));
               }
               break;
            }

            case Harbour::DB::Field::TIMESTAMP:
            {
               TIMESTAMP_STRUCT val = {0, 0, 0, 0, 0, 0, 0};
               if( SQL_SUCCEEDED(res = SQLGetData(hStmt, ui, SQL_C_TIMESTAMP, &val, sizeof(val), &iLen)) ) {
                  pItem = hb_itemPutTDT(pItem, hb_dateEncode(val.year, val.month, val.day), hb_timeEncode(val.hour, val.minute, val.second, val.fraction / 1000000));
               }
               break;
            }
         }

         /* TODO: check SQL_SUCCEEDED(res) */
         /* TODO: check for SQL_NO_TOTAL. What does this mean? */
         HB_SYMBOL_UNUSED(res);

         if( pItem != nullptr ) {
            /* nullptr -> NIL */
            if( iLen == SQL_NULL_DATA ) {
               hb_itemClear(pItem);
            } else {
               hb_arraySetForward(pArray, ui, pItem);
            }
         }
      }
      if( pItem != nullptr ) {
         hb_itemRelease(pItem);
      }

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
