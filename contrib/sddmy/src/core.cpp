/*
 * MariaDB/MySQL Database Driver
 *
 * Copyright 2007 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbrddsql.hpp"

#include "hbapiitm.hpp"
#include "hbvm.hpp"

#ifndef my_socket_defined
#define my_socket_defined
using my_socket = int;
#endif

#include <mysql.h>

#ifndef MYSQL_TYPE_NEWDECIMAL
#define MYSQL_TYPE_NEWDECIMAL  246
#endif

struct SDDCONN
{
   MYSQL * pMySql;
};

struct SDDDATA
{
   MYSQL_RES * pResult;
   MYSQL_ROW pNatRecord;
   unsigned long * pNatLength;
};

static HB_ERRCODE mysqlConnect(SQLDDCONNECTION * pConnection, PHB_ITEM pItem);
static HB_ERRCODE mysqlDisconnect(SQLDDCONNECTION * pConnection);
static HB_ERRCODE mysqlExecute(SQLDDCONNECTION * pConnection, PHB_ITEM pItem);
static HB_ERRCODE mysqlOpen(SQLBASEAREAP pArea);
static HB_ERRCODE mysqlClose(SQLBASEAREAP pArea);
static HB_ERRCODE mysqlGoTo(SQLBASEAREAP pArea, HB_ULONG ulRecNo);
static HB_ERRCODE mysqlGetValue(SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem);

static SDDNODE s_mysqldd =
{
   nullptr,
   "MYSQL",
   static_cast<SDDFUNC_CONNECT>(mysqlConnect),
   static_cast<SDDFUNC_DISCONNECT>(mysqlDisconnect),
   static_cast<SDDFUNC_EXECUTE>(mysqlExecute),
   static_cast<SDDFUNC_OPEN>(mysqlOpen),
   static_cast<SDDFUNC_CLOSE>(mysqlClose),
   static_cast<SDDFUNC_GOTO>(mysqlGoTo),
   static_cast<SDDFUNC_GETVALUE>(mysqlGetValue),
   static_cast<SDDFUNC_GETVARLEN>(nullptr)
};

static void hb_mysqldd_init(void * cargo)
{
   HB_SYMBOL_UNUSED(cargo);

   if( !hb_sddRegister(&s_mysqldd) || (sizeof(MYSQL_ROW_OFFSET) != sizeof(void*)) ) {
      hb_errInternal(HB_EI_RDDINVALID, nullptr, nullptr, nullptr);
   }
}

HB_FUNC( HB_SDDMY_REGISTER )
{
   hb_mysqldd_init(nullptr);
}

/* force SQLBASE linking */
HB_FUNC_TRANSLATE(SDDMY, SQLBASE)

HB_INIT_SYMBOLS_BEGIN(mysqldd__InitSymbols)
{
   "SDDMY", {HB_FS_PUBLIC}, {HB_FUNCNAME(SDDMY)}, nullptr
},
HB_INIT_SYMBOLS_END(mysqldd__InitSymbols)

HB_CALL_ON_STARTUP_BEGIN(_hb_mysqldd_init_)
hb_vmAtInit(hb_mysqldd_init, nullptr);
HB_CALL_ON_STARTUP_END(_hb_mysqldd_init_)

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup mysqldd__InitSymbols
   #pragma startup _hb_mysqldd_init_
#elif defined(HB_DATASEG_STARTUP)
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC(mysqldd__InitSymbols) \
   HB_DATASEG_FUNC(_hb_mysqldd_init_)
   #include "hbiniseg.hpp"
#endif

/* --- */

static HB_USHORT hb_errRT_MySQLDD(HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ERRCODE errOsCode)
{
   PHB_ITEM pError = hb_errRT_New(ES_ERROR, "SDDMY", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE);
   HB_USHORT uiAction = hb_errLaunch(pError);
   hb_itemRelease(pError);
   return uiAction;
}

/* --- SDD METHODS --- */

static HB_ERRCODE mysqlConnect(SQLDDCONNECTION * pConnection, PHB_ITEM pItem)
{
   auto pItemUnixSocket = hb_arrayGetItemPtr(pItem, 7);

   MYSQL * pMySql = mysql_init(nullptr);
   if( !mysql_real_connect(pMySql,
                           hb_arrayGetCPtr(pItem, 2) /* host */,
                           hb_arrayGetCPtr(pItem, 3) /* user */,
                           hb_arrayGetCPtr(pItem, 4) /* password */,
                           hb_arrayGetCPtr(pItem, 5) /* db */,
                           hb_arrayGetNI(pItem, 6) /* port */,
                           pItemUnixSocket && HB_IS_STRING(pItemUnixSocket) ? hb_itemGetCPtr(pItemUnixSocket) : nullptr,
                           hb_arrayGetNI(pItem, 8) /* flags*/) ) {
      hb_rddsqlSetError(mysql_errno(pMySql), mysql_error(pMySql), nullptr, nullptr, 0);
      mysql_close(pMySql);
      return Harbour::FAILURE;
   }
   pConnection->pSDDConn = hb_xgrab(sizeof(SDDCONN));
   (static_cast<SDDCONN*>(pConnection->pSDDConn))->pMySql = pMySql;
   return Harbour::SUCCESS;
}

static HB_ERRCODE mysqlDisconnect(SQLDDCONNECTION * pConnection)
{
   mysql_close((static_cast<SDDCONN*>(pConnection->pSDDConn))->pMySql);
   hb_xfree(pConnection->pSDDConn);
   return Harbour::SUCCESS;
}

static HB_ERRCODE mysqlExecute(SQLDDCONNECTION * pConnection, PHB_ITEM pItem)
{
   MYSQL * pMySql = (static_cast<SDDCONN*>(pConnection->pSDDConn))->pMySql;

   if( mysql_real_query(pMySql, hb_itemGetCPtr(pItem), static_cast<unsigned long>(hb_itemGetCLen(pItem))) ) {
      hb_rddsqlSetError(mysql_errno(pMySql), mysql_error(pMySql), hb_itemGetCPtr(pItem), nullptr, 0);
      return Harbour::FAILURE;
   }

   MYSQL_RES * pResult = mysql_store_result(pMySql);

   HB_ULONG ulAffectedRows;
   PHB_ITEM pNewID = nullptr;

   if( pResult ) {
      ulAffectedRows = static_cast<HB_ULONG>(mysql_num_rows(pResult));
      mysql_free_result(pResult);
      hb_rddsqlSetError(0, nullptr, hb_itemGetCPtr(pItem), nullptr, ulAffectedRows);
   } else {
      if( mysql_field_count(pMySql) == 0 ) {
         ulAffectedRows = static_cast<HB_ULONG>(mysql_affected_rows(pMySql));
         if( mysql_insert_id(pMySql) != 0 ) {
            pNewID = hb_itemPutNInt(nullptr, mysql_insert_id(pMySql));
         }
         hb_rddsqlSetError(0, nullptr, hb_itemGetCPtr(pItem), pNewID, ulAffectedRows);
         if( pNewID ) {
            hb_itemRelease(pNewID);
         }
      } else { /* error */
         hb_rddsqlSetError(mysql_errno(pMySql ), mysql_error(pMySql), hb_itemGetCPtr(pItem), nullptr, 0);
         return Harbour::FAILURE;
      }
   }
   return Harbour::SUCCESS;
}

static HB_ERRCODE mysqlOpen(SQLBASEAREAP pArea)
{
   MYSQL * pMySql = (static_cast<SDDCONN*>(pArea->pConnection->pSDDConn))->pMySql;

   pArea->pSDDData = memset(hb_xgrab(sizeof(SDDDATA)), 0, sizeof(SDDDATA));
   auto pSDDData = static_cast<SDDDATA*>(pArea->pSDDData);

   if( mysql_real_query(pMySql, pArea->szQuery, static_cast<unsigned long>(strlen(pArea->szQuery))) ) {
      hb_errRT_MySQLDD(EG_OPEN, ESQLDD_INVALIDQUERY, static_cast<const char*>(mysql_error(pMySql)), pArea->szQuery, mysql_errno(pMySql));
      return Harbour::FAILURE;
   }

   if( (pSDDData->pResult = mysql_store_result(pMySql)) == nullptr ) {
      hb_errRT_MySQLDD(EG_MEM, ESQLDD_INVALIDQUERY, static_cast<const char*>(mysql_error(pMySql)), pArea->szQuery, mysql_errno(pMySql));
      return Harbour::FAILURE;
   }

   auto uiFields = static_cast<HB_USHORT>(mysql_num_fields(pSDDData->pResult));
   SELF_SETFIELDEXTENT(&pArea->area, uiFields);

   auto pItemEof = hb_itemArrayNew(uiFields);

   MYSQL_FIELD * pMyField;
   PHB_ITEM pItem;
   HB_ERRCODE errCode = 0;

   bool bError = false;
   for( HB_USHORT uiCount = 0; uiCount < uiFields; uiCount++ ) {
      pMyField = mysql_fetch_field_direct(pSDDData->pResult, uiCount);

      DBFIELDINFO dbFieldInfo{};
      dbFieldInfo.atomName = pMyField->name;
      dbFieldInfo.uiLen = static_cast<HB_USHORT>(pMyField->length);

      switch( pMyField->type ) {
         case MYSQL_TYPE_TINY:
         case MYSQL_TYPE_SHORT:
            dbFieldInfo.uiType = Harbour::DB::Field::INTEGER;
            break;

         case MYSQL_TYPE_LONG:
         case MYSQL_TYPE_LONGLONG:
         case MYSQL_TYPE_INT24:
            dbFieldInfo.uiType = Harbour::DB::Field::LONG;
            break;

         case MYSQL_TYPE_DECIMAL:
         case MYSQL_TYPE_NEWDECIMAL:
         case MYSQL_TYPE_FLOAT:
         case MYSQL_TYPE_DOUBLE:
            dbFieldInfo.uiType = Harbour::DB::Field::DOUBLE;
            dbFieldInfo.uiDec = static_cast<HB_USHORT>(pMyField->decimals);
            break;

         case MYSQL_TYPE_STRING:
         case MYSQL_TYPE_VAR_STRING:
         case MYSQL_TYPE_ENUM:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            break;

         case MYSQL_TYPE_DATE:
            dbFieldInfo.uiType = Harbour::DB::Field::DATE;
            break;

         case MYSQL_TYPE_TINY_BLOB:
         case MYSQL_TYPE_MEDIUM_BLOB:
         case MYSQL_TYPE_LONG_BLOB:
         case MYSQL_TYPE_BLOB:
            dbFieldInfo.uiType = Harbour::DB::Field::MEMO;
            break;

         case MYSQL_TYPE_TIMESTAMP:
         case MYSQL_TYPE_DATETIME:
#if MYSQL_VERSION_ID >= 50610
         case MYSQL_TYPE_TIMESTAMP2:
         case MYSQL_TYPE_DATETIME2:
#endif
            dbFieldInfo.uiType = Harbour::DB::Field::TIMESTAMP;
            dbFieldInfo.uiLen = 8;
            break;

         case MYSQL_TYPE_TIME:
#if MYSQL_VERSION_ID >= 50610
         case MYSQL_TYPE_TIME2:
#endif
            dbFieldInfo.uiType = Harbour::DB::Field::TIME;
            dbFieldInfo.uiLen = 4;
            break;

#if 0
         case MYSQL_TYPE_NULL:
         case MYSQL_TYPE_YEAR:
         case MYSQL_TYPE_NEWDATE:
         case MYSQL_TYPE_SET:
         case MYSQL_TYPE_VARCHAR:
         case MYSQL_TYPE_BIT:
         case MYSQL_TYPE_GEOMETRY:
#endif
         default:
            bError = true;
            errCode = static_cast<HB_ERRCODE>(pMyField->type);
            break;
      }

      if( !bError ) {
         switch( dbFieldInfo.uiType ) {
            case Harbour::DB::Field::STRING: {
               auto pStr = static_cast<char*>(hb_xgrab(dbFieldInfo.uiLen + 1));
               memset(pStr, ' ', dbFieldInfo.uiLen);
               pStr[dbFieldInfo.uiLen] = '\0';
               pItem = hb_itemPutCL(nullptr, pStr, dbFieldInfo.uiLen);
               hb_xfree(pStr);
               break;
            }

            case Harbour::DB::Field::MEMO:
               pItem = hb_itemPutC(nullptr, nullptr);
               break;

            case Harbour::DB::Field::INTEGER:
               pItem = hb_itemPutNI(nullptr, 0);
               break;

            case Harbour::DB::Field::LONG:
               pItem = hb_itemPutNL(nullptr, 0);
               break;

            case Harbour::DB::Field::DOUBLE:
               pItem = hb_itemPutND(nullptr, 0.0);
               break;

            case Harbour::DB::Field::DATE:
               pItem = hb_itemPutDS(nullptr, nullptr);
               break;

            case Harbour::DB::Field::TIMESTAMP:
            case Harbour::DB::Field::TIME:
               pItem = hb_itemPutTDT(nullptr, 0, 0);
               break;

            default:
               pItem = hb_itemNew(nullptr);
               bError = true;
               break;
         }

         hb_arraySetForward(pItemEof, uiCount + 1, pItem);
         hb_itemRelease(pItem);

#if 0
         if( dbFieldInfo.uiType == Harbour::Item::DOUBLE || dbFieldInfo.uiType == Harbour::Item::INTEGER ) {
            dbFieldInfo.uiType = Harbour::Item::LONG;
         }
#endif

         if( !bError ) {
            bError = (SELF_ADDFIELD(&pArea->area, &dbFieldInfo) == Harbour::FAILURE);
         }
      }

      if( bError ) {
         break;
      }
   }

   if( bError ) {
      hb_itemRelease(pItemEof);
      hb_errRT_MySQLDD(EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", pArea->szQuery, errCode);
      return Harbour::FAILURE;
   }

   pArea->ulRecCount = static_cast<HB_ULONG>(mysql_num_rows(pSDDData->pResult));
   pArea->ulRecMax = pArea->ulRecCount + 1;

   pArea->pRow = static_cast<void**>(hb_xgrab((pArea->ulRecCount + 1) * sizeof(void*)));
   pArea->pRowFlags = static_cast<HB_BYTE*>(hb_xgrabz((pArea->ulRecCount + 1) * sizeof(HB_BYTE)));

   void ** pRow = pArea->pRow;

   *pRow++ = pItemEof;
   pArea->pRowFlags[0] = SQLDD_FLAG_CACHED;

   for( HB_ULONG ulIndex = 1; ulIndex <= pArea->ulRecCount; ulIndex++ ) {
      *pRow++ = static_cast<void*>(mysql_row_tell(pSDDData->pResult));
      mysql_fetch_row(pSDDData->pResult);
   }
   pArea->fFetched = true;

   return Harbour::SUCCESS;
}

static HB_ERRCODE mysqlClose(SQLBASEAREAP pArea)
{
   auto pSDDData = static_cast<SDDDATA*>(pArea->pSDDData);

   if( pSDDData ) {
      if( pSDDData->pResult ) {
         mysql_free_result(pSDDData->pResult);
      }

      hb_xfree(pSDDData);
      pArea->pSDDData = nullptr;
   }
   return Harbour::SUCCESS;
}

static HB_ERRCODE mysqlGoTo(SQLBASEAREAP pArea, HB_ULONG ulRecNo)
{
   auto pSDDData = static_cast<SDDDATA*>(pArea->pSDDData);

   if( ulRecNo == 0 || ulRecNo > pArea->ulRecCount ) {
      pArea->pRecord = pArea->pRow[0];
      pArea->bRecordFlags = pArea->pRowFlags[0];

      pArea->fPositioned = false;
   } else {
      pArea->pRecord = pArea->pRow[ulRecNo];
      pArea->bRecordFlags = pArea->pRowFlags[ulRecNo];

      if( !(pArea->bRecordFlags & SQLDD_FLAG_CACHED) ) {
         mysql_row_seek(pSDDData->pResult, static_cast<MYSQL_ROW_OFFSET>(pArea->pRecord));
         pSDDData->pNatRecord = mysql_fetch_row(pSDDData->pResult);
         pSDDData->pNatLength = mysql_fetch_lengths(pSDDData->pResult);
      }

      pArea->fPositioned = true;
   }
   return Harbour::SUCCESS;
}

static HB_ERRCODE mysqlGetValue(SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
   auto pSDDData = static_cast<SDDDATA*>(pArea->pSDDData);

   bool bError = false;
   uiIndex--;
   LPFIELD pField = pArea->area.lpFields + uiIndex;

   char * pValue = pSDDData->pNatRecord[uiIndex];
   HB_SIZE nLen = pSDDData->pNatLength[uiIndex];

   /* NULL => NIL (?) */
   if( !pValue ) {
      hb_itemClear(pItem);
      return Harbour::SUCCESS;
   }

   char szBuffer[64];

   switch( pField->uiType ) {
      case Harbour::DB::Field::STRING: {
#if 0
         /* Expand strings to field length */
         auto pStr = static_cast<char*>(hb_xgrab(pField->uiLen + 1));
         memcpy(pStr, pValue, nLen);

         if( static_cast<HB_SIZE>(pField->uiLen) > nLen ) {
            memset(pStr + nLen, ' ', pField->uiLen - nLen);
         }

         pStr[pField->uiLen] = '\0';
         hb_itemPutCRaw(pItem, pStr, pField->uiLen);
#else
         /* Do not expand strings */
         hb_itemPutCL(pItem, pValue, nLen);
#endif
         break;
      }

      case Harbour::DB::Field::MEMO:
         hb_itemPutCL(pItem, pValue, nLen);
         hb_itemSetCMemo(pItem);
         break;

      case Harbour::DB::Field::INTEGER:
      case Harbour::DB::Field::LONG:
      case Harbour::DB::Field::DOUBLE:
         hb_strncpy(szBuffer, pValue, sizeof(szBuffer) - 1);

         if( pField->uiDec ) {
            hb_itemPutNDLen(pItem, atof(szBuffer), static_cast<int>(pField->uiLen) - (static_cast<int>(pField->uiDec) + 1), static_cast<int>(pField->uiDec));
         } else {
            hb_itemPutNLLen(pItem, atol(szBuffer), static_cast<int>(pField->uiLen));
         }
         break;

      case Harbour::DB::Field::DATE: {
         char szDate[9];

         szDate[0] = pValue[0];
         szDate[1] = pValue[1];
         szDate[2] = pValue[2];
         szDate[3] = pValue[3];
         szDate[4] = pValue[5];
         szDate[5] = pValue[6];
         szDate[6] = pValue[8];
         szDate[7] = pValue[9];
         szDate[8] = '\0';
         hb_itemPutDS(pItem, szDate);
         break;
      }

      case Harbour::DB::Field::TIMESTAMP: {
         char szTimeStamp[15];

         szTimeStamp[0] = pValue[0];
         szTimeStamp[1] = pValue[1];
         szTimeStamp[2] = pValue[2];
         szTimeStamp[3] = pValue[3];
         szTimeStamp[4] = pValue[5];
         szTimeStamp[5] = pValue[6];
         szTimeStamp[6] = pValue[8];
         szTimeStamp[7] = pValue[9];

         szTimeStamp[ 8] = pValue[11];
         szTimeStamp[ 9] = pValue[12];
         szTimeStamp[10] = pValue[14];
         szTimeStamp[11] = pValue[15];
         szTimeStamp[12] = pValue[17];
         szTimeStamp[13] = pValue[18];
         szTimeStamp[14] = '\0';
         hb_itemPutTS(pItem, szTimeStamp);
         break;
      }

      case Harbour::DB::Field::TIME: {
         char szTimeStamp[15];

         szTimeStamp[0] = '0';
         szTimeStamp[1] = '0';
         szTimeStamp[2] = '0';
         szTimeStamp[3] = '0';
         szTimeStamp[4] = '0';
         szTimeStamp[5] = '0';
         szTimeStamp[6] = '0';
         szTimeStamp[7] = '0';

         szTimeStamp[ 8] = pValue[0];
         szTimeStamp[ 9] = pValue[1];
         szTimeStamp[10] = pValue[3];
         szTimeStamp[11] = pValue[4];
         szTimeStamp[12] = pValue[6];
         szTimeStamp[13] = pValue[7];
         szTimeStamp[14] = '\0';
         hb_itemPutTS(pItem, szTimeStamp);
         break;
      }

      default:
         bError = true;
         break;
   }

   if( bError ) {
      PHB_ITEM pError = hb_errNew();
      hb_errPutGenCode(pError, EG_DATATYPE);
      hb_errPutDescription(pError, hb_langDGetErrorDesc(EG_DATATYPE));
      hb_errPutSubCode(pError, EDBF_DATATYPE);
      SELF_ERROR(&pArea->area, pError);
      hb_itemRelease(pError);
      return Harbour::FAILURE;
   }
   return Harbour::SUCCESS;
}
