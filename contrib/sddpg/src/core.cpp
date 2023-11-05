/*
 * Postgre SQL Database Driver
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

#include <libpq-fe.h>

#define BOOLOID         16
#define BYTEAOID        17
#define CHAROID         18
#define NAMEOID         19
#define INT8OID         20
#define INT2OID         21
#define INT4OID         23
#define TEXTOID         25
#define OIDOID          26
#define CIDROID         650
#define FLOAT4OID       700
#define FLOAT8OID       701
#define CASHOID         790
#define MACADDROID      829
#define INETOID         869
#define BPCHAROID       1042
#define VARCHAROID      1043
#define DATEOID         1082
#define TIMEOID         1083
#define TIMESTAMPOID    1114
#define TIMESTAMPTZOID  1184
#define TIMETZOID       1266
#define BITOID          1560
#define VARBITOID       1562
#define NUMERICOID      1700

struct SDDCONN
{
   PGconn * pConn;
};

struct SDDDATA
{
   PGresult * pResult;
};

static HB_ERRCODE pgsqlConnect(SQLDDCONNECTION * pConnection, PHB_ITEM pItem);
static HB_ERRCODE pgsqlDisconnect(SQLDDCONNECTION * pConnection);
static HB_ERRCODE pgsqlExecute(SQLDDCONNECTION * pConnection, PHB_ITEM pItem);
static HB_ERRCODE pgsqlOpen(SQLBASEAREAP pArea);
static HB_ERRCODE pgsqlClose(SQLBASEAREAP pArea);
static HB_ERRCODE pgsqlGetValue(SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem);

static SDDNODE s_pgsqldd = {
   nullptr,
   "POSTGRESQL",
   static_cast<SDDFUNC_CONNECT>(pgsqlConnect),
   static_cast<SDDFUNC_DISCONNECT>(pgsqlDisconnect),
   static_cast<SDDFUNC_EXECUTE>(pgsqlExecute),
   static_cast<SDDFUNC_OPEN>(pgsqlOpen),
   static_cast<SDDFUNC_CLOSE>(pgsqlClose),
   static_cast<SDDFUNC_GOTO>(nullptr),
   static_cast<SDDFUNC_GETVALUE>(pgsqlGetValue),
   static_cast<SDDFUNC_GETVARLEN>(nullptr)
};

static void hb_pgsqldd_init(void * cargo)
{
   HB_SYMBOL_UNUSED(cargo);

   if( !hb_sddRegister(&s_pgsqldd) ) {
      hb_errInternal(HB_EI_RDDINVALID, nullptr, nullptr, nullptr);
   }
}

HB_FUNC( HB_SDDPG_REGISTER )
{
   hb_pgsqldd_init(nullptr);
}

/* force SQLBASE linking */
HB_FUNC_TRANSLATE(SDDPG, SQLBASE)

HB_INIT_SYMBOLS_BEGIN(sddpostgre__InitSymbols)
{
   "SDDPG", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME(SDDPG)}, nullptr
},
HB_INIT_SYMBOLS_END(sddpostgre__InitSymbols)

HB_CALL_ON_STARTUP_BEGIN(_hb_sddpostgre_init_)
hb_vmAtInit(hb_pgsqldd_init, nullptr);
HB_CALL_ON_STARTUP_END(_hb_sddpostgre_init_)

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup sddpostgre__InitSymbols
   #pragma startup _hb_sddpostgre_init_
#elif defined(HB_DATASEG_STARTUP)
   #define HB_DATASEG_BODY  \
   HB_DATASEG_FUNC(sddpostgre__InitSymbols) \
   HB_DATASEG_FUNC(_hb_sddpostgre_init_)
   #include "hbiniseg.hpp"
#endif

/* --- */

static HB_USHORT hb_errRT_PostgreSQLDD(HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation, HB_ERRCODE errOsCode)
{
   PHB_ITEM pError = hb_errRT_New(ES_ERROR, "SDDPG", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE);
   HB_USHORT uiAction = hb_errLaunch(pError);
   hb_itemRelease(pError);
   return uiAction;
}

/* --- SDD METHODS --- */

static HB_ERRCODE pgsqlConnect(SQLDDCONNECTION * pConnection, PHB_ITEM pItem)
{
   const char * pszHost = hb_arrayGetCPtr(pItem, 2);
   PGconn * pConn;
   if( pszHost && (strncmp(pszHost, "postgresql://", 13) == 0 || strchr(pszHost, '=')) ) {
      pConn = PQconnectdb(pszHost);
   } else {
      pConn = PQsetdbLogin(pszHost, hb_arrayGetCPtr(pItem, 6), hb_arrayGetCPtr(pItem, 7), hb_arrayGetCPtr(pItem, 8), hb_arrayGetCPtr(pItem, 5), hb_arrayGetCPtr(pItem, 3), hb_arrayGetCPtr(pItem, 4));
   }

   if( !pConn ) { /* Low memory, etc */
      /* TODO: error */
      return Harbour::FAILURE;
   }
   ConnStatusType status = PQstatus(pConn);
   if( status != CONNECTION_OK ) {
      /* TODO: error */
      PQfinish(pConn);
      return Harbour::FAILURE;
   }
   pConnection->pSDDConn = hb_xgrab(sizeof(SDDCONN));
   (static_cast<SDDCONN*>(pConnection->pSDDConn))->pConn = pConn;
   return Harbour::SUCCESS;
}

static HB_ERRCODE pgsqlDisconnect(SQLDDCONNECTION * pConnection)
{
   PQfinish((static_cast<SDDCONN*>(pConnection->pSDDConn))->pConn);
   hb_xfree(pConnection->pSDDConn);
   return Harbour::SUCCESS;
}

static HB_ERRCODE pgsqlExecute(SQLDDCONNECTION * pConnection, PHB_ITEM pItem)
{
   PGconn * pConn = (static_cast<SDDCONN*>(pConnection->pSDDConn))->pConn;

   PGresult * pResult = PQexec(pConn, hb_itemGetCPtr(pItem));
   if( !pResult ) {
      hb_rddsqlSetError(1, PQerrorMessage(pConn), hb_itemGetCPtr(pItem), nullptr, 0);
      return Harbour::FAILURE;
   }

   ExecStatusType status = PQresultStatus(pResult);
   if( status != PGRES_TUPLES_OK && status != PGRES_COMMAND_OK ) {
      hb_rddsqlSetError(status, PQresultErrorMessage(pResult), hb_itemGetCPtr(pItem), nullptr, 0);
      return Harbour::FAILURE;
   }

   int iTuples = PQntuples(pResult);
   unsigned long ulAffectedRows;
   if( iTuples > 0 ) {
      ulAffectedRows = static_cast<unsigned long>(iTuples);
   } else {
      ulAffectedRows = static_cast<unsigned long>(atol(PQcmdTuples(pResult)));
   }

   hb_rddsqlSetError(0, nullptr, hb_itemGetCPtr(pItem), nullptr, ulAffectedRows);
   PQclear(pResult);
   return Harbour::SUCCESS;
}

static HB_ERRCODE pgsqlOpen(SQLBASEAREAP pArea)
{
   PGconn * pConn = (static_cast<SDDCONN*>(pArea->pConnection->pSDDConn))->pConn;

   pArea->pSDDData = memset(hb_xgrab(sizeof(SDDDATA)), 0, sizeof(SDDDATA));
   SDDDATA * pSDDData = static_cast<SDDDATA*>(pArea->pSDDData);

   PGresult * pResult = PQexec(pConn, pArea->szQuery);
   if( !pResult ) {
      hb_errRT_PostgreSQLDD(EG_OPEN, ESQLDD_LOWMEMORY, "Query failed", nullptr, 0);  /* Low memory, etc */
      return Harbour::FAILURE;
   }

   ExecStatusType status = PQresultStatus(pResult);
   if( status != PGRES_TUPLES_OK && status != PGRES_COMMAND_OK ) {
      hb_errRT_PostgreSQLDD(EG_OPEN, ESQLDD_INVALIDQUERY, PQresultErrorMessage(pResult), pArea->szQuery, static_cast<HB_ERRCODE>(status));
      PQclear(pResult);
      return Harbour::FAILURE;
   }

   pSDDData->pResult = pResult;

   HB_USHORT uiFields = static_cast<HB_USHORT>(PQnfields(pResult));
   SELF_SETFIELDEXTENT(&pArea->area, uiFields);

   auto pItemEof = hb_itemArrayNew(uiFields);
   auto pItem = hb_itemNew(nullptr);

   bool bError = false;
   for( HB_USHORT uiCount = 0; uiCount < uiFields; uiCount++ ) {
      DBFIELDINFO dbFieldInfo{};
      dbFieldInfo.atomName = PQfname(pResult, static_cast<int>(uiCount));

      switch( PQftype(pResult, static_cast<int>(uiCount)) ) {
         case BPCHAROID:
         case VARCHAROID:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            dbFieldInfo.uiLen = static_cast<HB_USHORT>(PQfmod(pResult, uiCount)) - 4;
            break;

         case TEXTOID:
            dbFieldInfo.uiType = Harbour::DB::Field::MEMO;
            dbFieldInfo.uiLen = 10;
            break;

         case NUMERICOID:
            dbFieldInfo.uiType = Harbour::DB::Field::DOUBLE;
            dbFieldInfo.uiLen = (PQfmod(pResult, uiCount) - 4) >> 16;
            dbFieldInfo.uiDec = (PQfmod(pResult, uiCount) - 4) & 0xFFFF;
            break;

         case INT2OID:
            dbFieldInfo.uiType = Harbour::DB::Field::INTEGER;
            dbFieldInfo.uiLen = 6;
            break;

         case INT4OID:
            dbFieldInfo.uiType = Harbour::DB::Field::INTEGER;
            dbFieldInfo.uiLen = 11;
            break;

         case INT8OID:
         case OIDOID:
            dbFieldInfo.uiType = Harbour::DB::Field::LONG;
            dbFieldInfo.uiLen = 20;
            break;

         case FLOAT4OID:
         case FLOAT8OID:
         case CASHOID:  /* TODO: ??? */
            dbFieldInfo.uiType = Harbour::DB::Field::DOUBLE;
            dbFieldInfo.uiLen = 16;
            dbFieldInfo.uiDec = 2;   /* TODO: hb_set.SET_DECIMALS ??? */
            break;

         case BOOLOID:
            dbFieldInfo.uiType = Harbour::DB::Field::LOGICAL;
            dbFieldInfo.uiLen = 1;
            break;

         case DATEOID:
            dbFieldInfo.uiType = Harbour::DB::Field::DATE;
            dbFieldInfo.uiLen = 8;
            break;

         case INETOID:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            dbFieldInfo.uiLen = 29;
            break;

         case CIDROID:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            dbFieldInfo.uiLen = 32;
            break;

         case MACADDROID:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            dbFieldInfo.uiLen = 17;
            break;

         case BITOID:
         case VARBITOID:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            dbFieldInfo.uiLen = static_cast<HB_USHORT>(PQfsize(pResult, uiCount));
            break;

         case TIMEOID:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            dbFieldInfo.uiLen = 12;
            break;

         case TIMESTAMPOID:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            dbFieldInfo.uiLen = 23;
            break;

         case TIMETZOID:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            dbFieldInfo.uiLen = 15;
            break;

         case TIMESTAMPTZOID:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            dbFieldInfo.uiLen = 26;
            break;

         case NAMEOID:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            dbFieldInfo.uiLen = 63;
            break;

         case BYTEAOID:
            dbFieldInfo.uiType = Harbour::DB::Field::STRING;
            break;

         default:
            bError = true;
            break;
      }
#if 0
      HB_TRACE(HB_TR_ALWAYS, ("field:%s type=%d size=%d format=%d mod=%d err=%d", dbFieldInfo.atomName, PQftype(pResult, static_cast<int>(uiCount)), PQfsize(pResult, uiCount), PQfformat(pResult, uiCount), PQfmod(pResult, uiCount), bError));
#endif

      if( !bError ) {
         switch( dbFieldInfo.uiType ) {
            case Harbour::DB::Field::STRING: {
               char * pStr = static_cast<char*>(hb_xgrab(dbFieldInfo.uiLen + 1));
               memset(pStr, ' ', dbFieldInfo.uiLen);
               pStr[dbFieldInfo.uiLen] = '\0';
               hb_itemPutCL(pItem, pStr, dbFieldInfo.uiLen);
               hb_xfree(pStr);
               break;
            }
            case Harbour::DB::Field::MEMO:
               hb_itemPutC(pItem, nullptr);
               hb_itemSetCMemo(pItem);
               break;

            case Harbour::DB::Field::INTEGER:
               hb_itemPutNI(pItem, 0);
               break;

            case Harbour::DB::Field::LONG:
               hb_itemPutNL(pItem, 0);
               break;

            case Harbour::DB::Field::DOUBLE:
               hb_itemPutND(pItem, 0.0);
               break;

            case Harbour::DB::Field::LOGICAL:
               hb_itemPutL(pItem, false);
               break;

            case Harbour::DB::Field::DATE:
               hb_itemPutDS(pItem, nullptr);
               break;

            default:
               hb_itemClear(pItem);
               bError = true;
               break;
         }

         hb_arraySetForward(pItemEof, uiCount + 1, pItem);

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

   hb_itemRelease(pItem);

   if( bError ) {
      hb_itemClear(pItemEof);
      hb_itemRelease(pItemEof);
      hb_errRT_PostgreSQLDD(EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", pArea->szQuery, 0);
      return Harbour::FAILURE;
   }

   pArea->ulRecCount = static_cast<HB_ULONG>(PQntuples(pResult));
   pArea->ulRecMax = pArea->ulRecCount + 1;

   pArea->pRow = static_cast<void**>(hb_xgrab((pArea->ulRecCount + 1) * sizeof(void*)));
   pArea->pRowFlags = static_cast<HB_BYTE*>(hb_xgrabz((pArea->ulRecCount + 1) * sizeof(HB_BYTE)));

   pArea->pRow[0] = pItemEof;
   pArea->pRowFlags[0] = SQLDD_FLAG_CACHED;
   pArea->fFetched = true;

   return Harbour::SUCCESS;
}

static HB_ERRCODE pgsqlClose(SQLBASEAREAP pArea)
{
   SDDDATA * pSDDData = static_cast<SDDDATA*>(pArea->pSDDData);

   if( pSDDData ) {
      if( pSDDData->pResult ) {
         PQclear(pSDDData->pResult);
      }

      hb_xfree(pSDDData);
      pArea->pSDDData = nullptr;
   }
   return Harbour::SUCCESS;
}

static HB_ERRCODE pgsqlGetValue(SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
   SDDDATA * pSDDData = static_cast<SDDDATA*>(pArea->pSDDData);

   bool bError = false;
   uiIndex--;
   LPFIELD pField = pArea->area.lpFields + uiIndex;

   if( PQgetisnull(pSDDData->pResult, pArea->ulRecNo - 1, uiIndex) ) {
      hb_itemClear(pItem);
      /* FIXME: it breaks defined field type */
      return Harbour::SUCCESS;
   }

   char * pValue = PQgetvalue(pSDDData->pResult, pArea->ulRecNo - 1, uiIndex);
   HB_SIZE nLen = static_cast<HB_SIZE>(PQgetlength(pSDDData->pResult, pArea->ulRecNo - 1, uiIndex));

#if 0
   HB_TRACE(HB_TR_ALWAYS, ("fieldget recno=%d index=%d value=%s len=%d", dbFieldInfo.atomName, PQftype(pResult, static_cast<int>(uiCount)), pArea->ulRecNo, uiIndex, pValue, nLen));
#endif

   switch( pField->uiType ) {
      case Harbour::DB::Field::STRING:
         hb_itemPutCL(pItem, pValue, nLen);
         break;

      case Harbour::DB::Field::MEMO:
         hb_itemPutCL(pItem, pValue, nLen);
         hb_itemSetCMemo(pItem);
         break;

      case Harbour::DB::Field::INTEGER:
      case Harbour::DB::Field::LONG:
      case Harbour::DB::Field::DOUBLE:
         if( pField->uiDec ) {
            hb_itemPutNDLen(pItem, atof(pValue), static_cast<int>(pField->uiLen) - (static_cast<int>(pField->uiDec) + 1), static_cast<int>(pField->uiDec));
         } else {
            if( pField->uiLen > 9 ) {
               hb_itemPutNDLen(pItem, atof(pValue), static_cast<int>(pField->uiLen), static_cast<int>(pField->uiDec));
            } else {
               hb_itemPutNLLen(pItem, atol(pValue), static_cast<int>(pField->uiLen));
            }
         }
         break;

      case Harbour::DB::Field::LOGICAL:
         hb_itemPutL(pItem, pValue[0] == 'T' || pValue[0] == 'Y');
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
