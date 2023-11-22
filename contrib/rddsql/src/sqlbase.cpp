/*
 * SQL Base Database Driver
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

#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbvm.hpp"
#include "hbset.hpp"
#include "hbrddsql.hpp"
#include "rddsys.ch"
#include "hbtrace.hpp"

#define SUPERTABLE              (&sqlbaseSuper)

#define CONNECTION_LIST_EXPAND  4

static HB_USHORT s_rddidSQLBASE = 0;

static SQLDDCONNECTION ** s_pConnection = nullptr;
static HB_ULONG s_ulConnectionCount = 0;
static HB_ULONG s_ulConnectionCurrent = 0;

static char * s_szError = nullptr;
static HB_ERRCODE s_errCode = 0;

static char * s_szQuery = nullptr;
static PHB_ITEM s_pItemNewID = nullptr;
static unsigned long s_ulAffectedRows = 0;

static RDDFUNCS sqlbaseSuper;

void hb_rddsqlSetError(HB_ERRCODE errCode, const char * szError, const char * szQuery, PHB_ITEM pItem, unsigned long ulAffectedRows)
{
   s_errCode = errCode;

   if( s_szError ) {
      hb_xfree(s_szError);
      s_szError = nullptr;
   }
   if( szError ) {
      s_szError = hb_strdup(szError);
   }
   if( s_szQuery ) {
      hb_xfree(s_szQuery);
      s_szQuery = nullptr;
   }
   if( szQuery ) {
      s_szQuery = hb_strdup(szQuery);
   }
   if( pItem != nullptr ) {
      hb_itemCopy(s_pItemNewID, pItem);
   } else {
      hb_itemClear(s_pItemNewID);
   }

   s_ulAffectedRows = ulAffectedRows;
}

static HB_ERRCODE hb_errRT_SQLBASE(HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation)
{
   HB_ERRCODE iRet = Harbour::FAILURE;

   if( hb_vmRequestQuery() == 0 ) {
      PHB_ITEM pError = hb_errRT_New(ES_ERROR, "SQLBASE", errGenCode, errSubCode, szDescription, szOperation, 0, EF_NONE);
      iRet = hb_errLaunch(pError);
      hb_itemRelease(pError);
   }
   return iRet;
}

/* --- NULL SDD --- */

static HB_ERRCODE sddConnect(SQLDDCONNECTION * pConnection, PHB_ITEM pItem);
static HB_ERRCODE sddDisconnect(SQLDDCONNECTION * pConnection);
static HB_ERRCODE sddExecute(SQLDDCONNECTION * pConnection, PHB_ITEM pItem);
static HB_ERRCODE sddOpen(SQLBASEAREAP pArea);
static HB_ERRCODE sddClose(SQLBASEAREAP pArea);
static HB_ERRCODE sddGoTo(SQLBASEAREAP pArea, HB_ULONG ulRecNo);
static HB_ERRCODE sddGetValue(SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem);
static HB_ERRCODE sddGetVarLen(SQLBASEAREAP pArea, HB_USHORT uiIndex, HB_ULONG * pLength);

static const SDDNODE s_sddNull = {
   nullptr,
   "NULL",
   static_cast<SDDFUNC_CONNECT>(sddConnect),
   static_cast<SDDFUNC_DISCONNECT>(sddDisconnect),
   static_cast<SDDFUNC_EXECUTE>(sddExecute),
   static_cast<SDDFUNC_OPEN>(sddOpen),
   static_cast<SDDFUNC_CLOSE>(sddClose),
   static_cast<SDDFUNC_GOTO>(sddGoTo),
   static_cast<SDDFUNC_GETVALUE>(sddGetValue),
   static_cast<SDDFUNC_GETVARLEN>(sddGetVarLen)
};

static HB_ERRCODE sddConnect(SQLDDCONNECTION * pConnection, PHB_ITEM pItem)
{
   HB_SYMBOL_UNUSED(pConnection);
   HB_SYMBOL_UNUSED(pItem);
   hb_errRT_SQLBASE(EG_UNSUPPORTED, ESQLDD_NULLSDD, nullptr, nullptr);
   return Harbour::FAILURE;
}

static HB_ERRCODE sddDisconnect(SQLDDCONNECTION * pConnection)
{
   HB_SYMBOL_UNUSED(pConnection);
   hb_errRT_SQLBASE(EG_UNSUPPORTED, ESQLDD_NULLSDD, nullptr, nullptr);
   return Harbour::FAILURE;
}

static HB_ERRCODE sddExecute(SQLDDCONNECTION * pConnection, PHB_ITEM pItem)
{
   HB_SYMBOL_UNUSED(pConnection);
   HB_SYMBOL_UNUSED(pItem);
   hb_errRT_SQLBASE(EG_UNSUPPORTED, ESQLDD_NULLSDD, nullptr, nullptr);
   return Harbour::FAILURE;
}

static HB_ERRCODE sddOpen(SQLBASEAREAP pArea)
{
   HB_SYMBOL_UNUSED(pArea);
   hb_errRT_SQLBASE(EG_UNSUPPORTED, ESQLDD_NULLSDD, nullptr, nullptr);
   return Harbour::FAILURE;
}

static HB_ERRCODE sddClose(SQLBASEAREAP pArea)
{
   HB_SYMBOL_UNUSED(pArea);
   return Harbour::SUCCESS;
}

static HB_ERRCODE sddGoTo(SQLBASEAREAP pArea, HB_ULONG ulRecNo)
{
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

static HB_ERRCODE sddGetValue(SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
   HB_SYMBOL_UNUSED(pArea);
   HB_SYMBOL_UNUSED(uiIndex);
   HB_SYMBOL_UNUSED(pItem);
   hb_errRT_SQLBASE(EG_UNSUPPORTED, ESQLDD_NULLSDD, nullptr, nullptr);
   return Harbour::FAILURE;
}

static HB_ERRCODE sddGetVarLen(SQLBASEAREAP pArea, HB_USHORT uiIndex, HB_ULONG * pLength)
{
   HB_SYMBOL_UNUSED(pArea);
   HB_SYMBOL_UNUSED(uiIndex);
   HB_SYMBOL_UNUSED(pLength);
   hb_errRT_SQLBASE(EG_UNSUPPORTED, ESQLDD_NULLSDD, nullptr, nullptr);
   return Harbour::SUCCESS;
}

/* --- SDD registration --- */

static PSDDNODE s_pSdd = nullptr;

int hb_sddRegister(PSDDNODE pSdd)
{
   PSDDNODE pNode = s_pSdd;

   // "Inheritance" from NULL SDD
   if( pSdd->Connect == nullptr ) {
      pSdd->Connect = s_sddNull.Connect;
   }
   if( pSdd->Disconnect == nullptr ) {
      pSdd->Disconnect = s_sddNull.Disconnect;
   }
   if( pSdd->Execute == nullptr ) {
      pSdd->Execute = s_sddNull.Execute;
   }
   if( pSdd->Open == nullptr ) {
      pSdd->Open = s_sddNull.Open;
   }
   if( pSdd->Close == nullptr ) {
      pSdd->Close = s_sddNull.Close;
   }
   if( pSdd->GoTo == nullptr ) {
      pSdd->GoTo = s_sddNull.GoTo;
   }
   if( pSdd->GetValue == nullptr ) {
      pSdd->GetValue = s_sddNull.GetValue;
   }
   if( pSdd->GetVarLen == nullptr ) {
      pSdd->GetVarLen = s_sddNull.GetVarLen;
   }

   while( pNode ) {
      if( !hb_stricmp(pNode->Name, pSdd->Name) ) {
         return 0;
      }
      pNode = pNode->pNext;
   }
   pSdd->pNext = s_pSdd;
   s_pSdd = pSdd;
   return 1;
}

/* --- RDD METHODS --- */

static HB_ERRCODE sqlbaseGoBottom(SQLBASEAREAP pArea)
{
   if( SELF_GOCOLD(&pArea->area) == Harbour::FAILURE ) {
      return Harbour::FAILURE;
   }

   if( !pArea->fFetched && pArea->pSDD->GoTo(pArea, static_cast<HB_ULONG>(-1)) == Harbour::FAILURE ) {
      return Harbour::FAILURE;
   }

   pArea->area.fTop = false;
   pArea->area.fBottom = true;

   if( SELF_GOTO(&pArea->area, pArea->ulRecCount) != Harbour::SUCCESS ) {
      return Harbour::FAILURE;
   }

   return SELF_SKIPFILTER(&pArea->area, -1);
}

static HB_ERRCODE sqlbaseGoTo(SQLBASEAREAP pArea, HB_ULONG ulRecNo)
{
   if( SELF_GOCOLD(&pArea->area) == Harbour::FAILURE ) {
      return Harbour::FAILURE;
   }

   if( pArea->pSDD->GoTo(pArea, ulRecNo) == Harbour::FAILURE ) {
      return Harbour::FAILURE;
   }

   if( pArea->fPositioned ) {
      pArea->ulRecNo = ulRecNo;
      pArea->area.fBof = pArea->area.fEof = false;
   } else {
      pArea->ulRecNo = pArea->ulRecCount + 1;
      pArea->area.fBof = pArea->area.fEof = true;
   }
   pArea->area.fFound = false;

   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseGoToId(SQLBASEAREAP pArea, PHB_ITEM pItem)
{
   if( HB_IS_NUMERIC(pItem) ) {
      return SELF_GOTO(&pArea->area, hb_itemGetNL(pItem));
   } else {
      PHB_ITEM pError = hb_errNew();
      hb_errPutGenCode(pError, EG_DATATYPE);
      hb_errPutDescription(pError, hb_langDGetErrorDesc(EG_DATATYPE));
      hb_errPutSubCode(pError, EDBF_DATATYPE);
      SELF_ERROR(&pArea->area, pError);
      hb_itemRelease(pError);
      return Harbour::FAILURE;
   }
}

static HB_ERRCODE sqlbaseGoTop(SQLBASEAREAP pArea)
{
   pArea->area.fTop = true;
   pArea->area.fBottom = false;

   if( SELF_GOTO(&pArea->area, 1) == Harbour::FAILURE ) {
      return Harbour::FAILURE;
   }

   return SELF_SKIPFILTER(&pArea->area, 1);
}

static HB_ERRCODE sqlbaseSkip(SQLBASEAREAP pArea, HB_LONG lToSkip)
{
   if( pArea->lpdbPendingRel ) {
      if( SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS ) {
         return Harbour::FAILURE;
      }
   }

   pArea->area.fTop = pArea->area.fBottom = false;

   if( lToSkip == 0 || hb_setGetDeleted() || pArea->area.dbfi.itmCobExpr || pArea->area.dbfi.fFilter ) {
      return SUPER_SKIP(&pArea->area, lToSkip);
   }

   HB_ERRCODE errCode = SELF_SKIPRAW(&pArea->area, lToSkip);

   // Move first record and set Bof flag
   if( errCode == Harbour::SUCCESS && pArea->area.fBof && lToSkip < 0 ) {
      errCode = SELF_GOTOP(&pArea->area);
      pArea->area.fBof = true;
   }

   if( lToSkip < 0 ) {
      pArea->area.fEof = false;
   } else /* if( lToSkip > 0 ) */ {
      pArea->area.fBof = false;
   }

   return errCode;
}

static HB_ERRCODE sqlbaseSkipRaw(SQLBASEAREAP pArea, HB_LONG lToSkip)
{
   if( pArea->lpdbPendingRel ) {
      if( SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS ) {
         return Harbour::FAILURE;
      }
   }

   HB_ERRCODE errCode;

   if( lToSkip == 0 ) {
      // TODO: maybe gocold is enough here?!

      // Save flags
      bool bBof = pArea->area.fBof;
      bool bEof = pArea->area.fEof;

      errCode = SELF_GOTO(&pArea->area, pArea->ulRecNo);

      // Restore flags
      pArea->area.fBof = bBof;
      pArea->area.fEof = bEof;
   } else if( lToSkip < 0 && static_cast<HB_ULONG>(-lToSkip) >= pArea->ulRecNo ) {
      errCode = SELF_GOTO(&pArea->area, 1);
      pArea->area.fBof = true;
   } else {
      errCode = SELF_GOTO(&pArea->area, pArea->ulRecNo + lToSkip);
   }

   return errCode;
}

static HB_ERRCODE sqlbaseAppend(SQLBASEAREAP pArea, HB_BOOL bUnLockAll)
{
   HB_SYMBOL_UNUSED(bUnLockAll);

   // This GOTO is GOCOLD + GOEOF
   if( SELF_GOTO(&pArea->area, 0) == Harbour::FAILURE ) {
      return Harbour::FAILURE;
   }

   if( !pArea->fRecordChanged && SELF_GOHOT(&pArea->area) == Harbour::FAILURE ) {
      return Harbour::FAILURE;
   }

   if( pArea->ulRecCount + 1 >= pArea->ulRecMax ) {
      pArea->pRow = static_cast<void**>(hb_xrealloc(pArea->pRow, (pArea->ulRecMax + SQLDD_ROWSET_RESIZE) * sizeof(void*)));
      pArea->pRowFlags = static_cast<HB_BYTE*>(hb_xrealloc(pArea->pRowFlags, (pArea->ulRecMax + SQLDD_ROWSET_RESIZE) * sizeof(HB_BYTE)));
      pArea->ulRecMax += SQLDD_ROWSET_RESIZE;
   }

   pArea->fAppend = pArea->fPositioned = true;
   pArea->ulRecCount++;
   pArea->ulRecNo = pArea->ulRecCount;
   pArea->area.fBof = pArea->area.fEof = pArea->area.fFound = false;
   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseDeleteRec(SQLBASEAREAP pArea)
{
   if( !pArea->fPositioned ) {
      return Harbour::SUCCESS;
   }

   if( !pArea->fRecordChanged && SELF_GOHOT(&pArea->area) == Harbour::FAILURE ) {
      return Harbour::FAILURE;
   }

   pArea->bRecordFlags |= SQLDD_FLAG_DELETED;
   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseDeleted(SQLBASEAREAP pArea, HB_BOOL * pDeleted)
{
   *pDeleted = pArea->bRecordFlags & SQLDD_FLAG_DELETED;
   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseGetValue(SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
   if( uiIndex == 0 || uiIndex > pArea->area.uiFieldCount ) {
      return Harbour::FAILURE;
   }

   if( pArea->bRecordFlags & SQLDD_FLAG_CACHED ) {
      hb_arrayGet(static_cast<PHB_ITEM>(pArea->pRecord), uiIndex, pItem);
      return Harbour::SUCCESS;
   }
   return pArea->pSDD->GetValue(pArea, uiIndex, pItem);
}

static HB_ERRCODE sqlbaseGetVarLen(SQLBASEAREAP pArea, HB_USHORT uiIndex, HB_ULONG * pLength)
{
   // TODO: should we use this code?
#if 0
   if( pArea->area.lpFields[uiIndex].uiType == Harbour::Item::MEMO ) {
      return pArea->pSDD->GetVarLen(pArea, uiIndex, pLength);
   }
#endif

   *pLength = pArea->area.lpFields[uiIndex - 1].uiLen;
   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseGoCold(SQLBASEAREAP pArea)
{
   if( pArea->fRecordChanged ) {
      if( !pArea->fAppend && pArea->pRowFlags[pArea->ulRecNo] & SQLDD_FLAG_CACHED ) {
         hb_itemRelease(static_cast<PHB_ITEM>(pArea->pRow[pArea->ulRecNo]));
      }
      pArea->pRow[pArea->ulRecNo] = pArea->pRecord;
      pArea->pRowFlags[pArea->ulRecNo] = pArea->bRecordFlags;
      pArea->fRecordChanged = false;
      pArea->fAppend = false;
   }
   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseGoHot(SQLBASEAREAP pArea)
{
   auto pArray = hb_itemArrayNew(pArea->area.uiFieldCount);
   auto pItem = hb_itemNew(nullptr);
   for( HB_USHORT us = 1; us <= pArea->area.uiFieldCount; us++ ) {
      if( SELF_GETVALUE(&pArea->area, us, pItem) == Harbour::SUCCESS ) {
         hb_arraySetForward(pArray, us, pItem);
      }
   }
   hb_itemRelease(pItem);
   pArea->pRecord = pArray;
   pArea->bRecordFlags |= SQLDD_FLAG_CACHED;
   pArea->fRecordChanged = true;
   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbasePutValue(SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
   if( uiIndex == 0 || uiIndex > pArea->area.uiFieldCount ) {
      return Harbour::FAILURE;
   }

   if( !pArea->fPositioned ) {
      return Harbour::SUCCESS;
   }

   if( !pArea->fRecordChanged && SELF_GOHOT(&pArea->area) == Harbour::FAILURE ) {
      return Harbour::FAILURE;
   }

   HB_ERRCODE errCode = Harbour::SUCCESS;
   LPFIELD pField  = pArea->area.lpFields + (uiIndex - 1);

   if(    ((HB_IS_MEMO(pItem) || HB_IS_STRING(pItem)) && (pField->uiType == Harbour::DB::Field::STRING || pField->uiType == Harbour::DB::Field::MEMO))
       || (HB_IS_DATE(pItem) && pField->uiType == Harbour::DB::Field::DATE)
       || (HB_IS_TIMESTAMP(pItem) && pField->uiType == Harbour::DB::Field::TIMESTAMP)
       || (HB_IS_NUMBER(pItem) && (pField->uiType == Harbour::DB::Field::INTEGER || pField->uiType == Harbour::DB::Field::LONG || pField->uiType == Harbour::DB::Field::FLOAT || pField->uiType == Harbour::DB::Field::DOUBLE))
       || (HB_IS_LOGICAL(pItem) && pField->uiType == Harbour::DB::Field::LOGICAL)
       || pField->uiType == Harbour::DB::Field::ANY
       || HB_IS_NIL(pItem) ) {
      hb_arraySet(static_cast<PHB_ITEM>(pArea->pRecord), uiIndex, pItem);
   } else {
      PHB_ITEM pError = hb_errNew();
      hb_errPutGenCode(pError, EG_DATATYPE);
      hb_errPutDescription(pError, hb_langDGetErrorDesc(EG_DATATYPE));
      hb_errPutOperation(pError, hb_dynsymName(static_cast<PHB_DYNS>(pField->sym)));
      hb_errPutSubCode(pError, errCode);
      hb_errPutFlags(pError, EF_CANDEFAULT);
      errCode = SELF_ERROR(&pArea->area, pError);
      hb_itemRelease(pError);
      return errCode == E_DEFAULT ? Harbour::SUCCESS : Harbour::FAILURE;
   }

   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseRecall(SQLBASEAREAP pArea)
{
   if( !pArea->fPositioned ) {
      return Harbour::SUCCESS;
   }

   if( !pArea->fRecordChanged && SELF_GOHOT(&pArea->area) != Harbour::SUCCESS ) {
      return Harbour::FAILURE;
   }

   pArea->bRecordFlags &= ~SQLDD_FLAG_DELETED;

   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseRecCount(SQLBASEAREAP pArea, HB_ULONG * pRecCount)
{
   *pRecCount = pArea->ulRecCount;
   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseRecNo(SQLBASEAREAP pArea, HB_ULONG * ulRecNo)
{
   *ulRecNo = pArea->ulRecNo;
   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseRecId(SQLBASEAREAP pArea, PHB_ITEM pRecNo)
{
   HB_ULONG ulRecNo;
   HB_ERRCODE errCode = SELF_RECNO(&pArea->area, &ulRecNo);
   hb_itemPutNInt(pRecNo, ulRecNo);
   return errCode;
}

static HB_ERRCODE sqlbaseZap(SQLBASEAREAP pArea)
{
   for( HB_ULONG ulIndex = 1; ulIndex <= pArea->ulRecCount; ulIndex++ ) {
      if( pArea->pRowFlags[ulIndex] & SQLDD_FLAG_CACHED ) {
         hb_itemRelease(static_cast<PHB_ITEM>(pArea->pRow[ulIndex]));
      }
   }

   pArea->ulRecCount = 0;
   pArea->ulRecNo = 0;

   pArea->pRow = static_cast<void**>(hb_xrealloc(pArea->pRow, SQLDD_ROWSET_RESIZE * sizeof(void*)));
   pArea->pRowFlags = static_cast<HB_BYTE*>(hb_xrealloc(pArea->pRowFlags, SQLDD_ROWSET_RESIZE * sizeof(HB_BYTE)));
   pArea->ulRecMax = SQLDD_ROWSET_RESIZE;

   pArea->fFetched = true;

   pArea->fPositioned = false;

   return SELF_GOTOP(&pArea->area);
}

static HB_ERRCODE sqlbaseClose(SQLBASEAREAP pArea)
{
   if( SELF_GOCOLD(&pArea->area) == Harbour::FAILURE ) {
      return Harbour::FAILURE;
   }

   if( SUPER_CLOSE(&pArea->area) == Harbour::FAILURE ) {
      return Harbour::FAILURE;
   }

   if( pArea->pSDD ) {
      pArea->pSDD->Close(pArea);
   }

   if( pArea->pRow ) {
      for( HB_ULONG ulIndex = 0; ulIndex <= pArea->ulRecCount; ulIndex++ ) {
         if( pArea->pRowFlags[ulIndex] & SQLDD_FLAG_CACHED ) {
            hb_itemRelease(static_cast<PHB_ITEM>(pArea->pRow[ulIndex]));
         }
      }
      hb_xfree(pArea->pRow);
      hb_xfree(pArea->pRowFlags);
      pArea->pRow      = nullptr;
      pArea->pRowFlags = nullptr;
   }

   if( pArea->szQuery ) {
      hb_xfree(pArea->szQuery);
      pArea->szQuery = nullptr;
   }
   if( pArea->pConnection ) {
      // It is possible to have areas without connection and SDD driver. Ex., arrayrdd. [Mindaugas]
      pArea->pConnection->uiAreaCount--;
      pArea->pConnection = nullptr;
   }
   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseCreate(SQLBASEAREAP pArea, LPDBOPENINFO pOpenInfo)
{
   pArea->ulConnection = pOpenInfo->ulConnection ? pOpenInfo->ulConnection : s_ulConnectionCurrent;

   if( pArea->ulConnection > s_ulConnectionCount || (pArea->ulConnection && !s_pConnection[pArea->ulConnection - 1]) ) {
      hb_errRT_SQLBASE(EG_OPEN, ESQLDD_NOTCONNECTED, "Not connected", nullptr);
      return Harbour::FAILURE;
   }

   if( pArea->ulConnection ) {
      pArea->pConnection = s_pConnection[pArea->ulConnection - 1];
      pArea->pConnection->uiAreaCount++;
      pArea->pSDD = pArea->pConnection->pSDD;
   } else {
      pArea->pSDD = &s_sddNull;
   }

   auto pItemEof = hb_itemArrayNew(pArea->area.uiFieldCount);

   PHB_ITEM pItem;

   bool bError = false;
   for( HB_USHORT uiCount = 0; uiCount < pArea->area.uiFieldCount; uiCount++ ) {
      LPFIELD pField = pArea->area.lpFields + uiCount;

      switch( pField->uiType ) {
         case Harbour::DB::Field::STRING: {
            auto pStr = static_cast<char*>(hb_xgrab(pField->uiLen + 1));
            memset(pStr, ' ', pField->uiLen);
            pStr[pField->uiLen] = '\0';
            pItem = hb_itemPutCL(nullptr, pStr, pField->uiLen);
            hb_xfree(pStr);
            break;
         }

         case Harbour::DB::Field::MEMO:
            pItem = hb_itemPutC(nullptr, nullptr);
            break;

         case Harbour::DB::Field::INTEGER:
            if( pField->uiDec ) {
               pItem = hb_itemPutND(nullptr, 0.0);
            } else {
               pItem = hb_itemPutNI(nullptr, 0);
            }
            break;

         case Harbour::DB::Field::LONG:
            if( pField->uiDec ) {
               pItem = hb_itemPutND(nullptr, 0.0);
            } else {
               pItem = hb_itemPutNL(nullptr, 0);
            }
            break;

         case Harbour::DB::Field::FLOAT:
            pItem = hb_itemPutND(nullptr, 0.0);
            break;

         case Harbour::DB::Field::DOUBLE:
            pItem = hb_itemPutND(nullptr, 0.0);
            break;

         case Harbour::DB::Field::DATE:
            pItem = hb_itemPutDS(nullptr, nullptr);
            break;

         case Harbour::DB::Field::LOGICAL:
            pItem = hb_itemPutL(nullptr, false);
            break;

         default:
            pItem = hb_itemNew(nullptr);
            bError = true;
            break;
      }

      hb_arraySetForward(pItemEof, uiCount + 1, pItem);
      hb_itemRelease(pItem);

      if( bError ) {
         break;
      }
   }

   if( bError ) {
      hb_itemClear(pItemEof);
      hb_itemRelease(pItemEof);
      hb_errRT_SQLBASE(EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", nullptr);
      SELF_CLOSE(&pArea->area);
      return Harbour::FAILURE;
   }

   pArea->ulRecCount = 0;

   pArea->pRow = static_cast<void**>(hb_xgrab(SQLDD_ROWSET_RESIZE * sizeof(void*)));
   pArea->pRowFlags = static_cast<HB_BYTE*>(hb_xgrab(SQLDD_ROWSET_RESIZE * sizeof(HB_BYTE)));
   pArea->ulRecMax = SQLDD_ROWSET_RESIZE;

   pArea->pRow[0] = pItemEof;
   pArea->pRowFlags[0] = SQLDD_FLAG_CACHED;
   pArea->fFetched = true;

   if( SUPER_CREATE(&pArea->area, pOpenInfo) != Harbour::SUCCESS ) {
      SELF_CLOSE(&pArea->area);
      return Harbour::FAILURE;
   }

   return SELF_GOTOP(&pArea->area);
}

static HB_ERRCODE sqlbaseInfo(SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem)
{
   switch( uiIndex ) {
      case DBI_QUERY:
         hb_itemPutC(pItem, pArea->szQuery);
         break;

      default:
         return SUPER_INFO(&pArea->area, uiIndex, pItem);
   }

   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseOpen(SQLBASEAREAP pArea, LPDBOPENINFO pOpenInfo)
{
   pArea->ulConnection = pOpenInfo->ulConnection ? pOpenInfo->ulConnection : s_ulConnectionCurrent;

   if( pArea->ulConnection == 0 || pArea->ulConnection > s_ulConnectionCount || !s_pConnection[pArea->ulConnection - 1] ) {
      hb_errRT_SQLBASE(EG_OPEN, ESQLDD_NOTCONNECTED, "Not connected", nullptr);
      return Harbour::FAILURE;
   }

   if( pArea->area.uiFieldCount ) {
      // This should not happen (in __dbTrans()), because RDD is registered with RDT_FULL
      return Harbour::FAILURE;
   }

   pArea->pConnection = s_pConnection[pArea->ulConnection - 1];
   pArea->pConnection->uiAreaCount++;
   pArea->pSDD = pArea->pConnection->pSDD;

   // filename is a query
   pArea->szQuery = hb_strdup(pOpenInfo->abName);

   HB_ERRCODE errCode = pArea->pSDD->Open(pArea);

   if( errCode == Harbour::SUCCESS ) {
      errCode = SUPER_OPEN(&pArea->area, pOpenInfo);
   }

   if( errCode != Harbour::SUCCESS ) {
      SELF_CLOSE(&pArea->area);
      return Harbour::FAILURE;
   }
   return SELF_GOTOP(&pArea->area);
}

static HB_ERRCODE sqlbaseStructSize(SQLBASEAREAP pArea, HB_USHORT * uiSize)
{
   HB_SYMBOL_UNUSED(pArea);
   *uiSize = sizeof(SQLBASEAREA);
   return Harbour::SUCCESS;
}

#if 0
static HB_ERRCODE sqlbaseChildEnd(SQLBASEAREAP pArea, LPDBRELINFO pRelInfo)
{
   HB_ERRCODE errCode;
   if( pArea->lpdbPendingRel == pRelInfo ) {
      errCode = SELF_FORCEREL(&pArea->area);
   } else {
      errCode = Harbour::SUCCESS;
   }
   SUPER_CHILDEND(&pArea->area, pRelInfo);
   return errCode;
}

static HB_ERRCODE sqlbaseChildStart(SQLBASEAREAP pArea, LPDBRELINFO pRelInfo)
{
   if( SELF_CHILDSYNC(&pArea->area, pRelInfo) != Harbour::SUCCESS ) {
      return Harbour::FAILURE;
   }
   return SUPER_CHILDSTART(&pArea->area, pRelInfo);
}

static HB_ERRCODE sqlbaseChildSync(SQLBASEAREAP pArea, LPDBRELINFO pRelInfo)
{
   if( SELF_GOCOLD(&pArea->area) != Harbour::SUCCESS ) {
      return Harbour::FAILURE;
   }

   pArea->lpdbPendingRel = pRelInfo;

   if( pArea->lpdbRelations ) {
      return SELF_SYNCCHILDREN(&pArea->area);
   }

   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseForceRel(SQLBASEAREAP pArea)
{
   if( pArea->lpdbPendingRel ) {
      LPDBRELINFO lpdbPendingRel = pArea->lpdbPendingRel;
      pArea->lpdbPendingRel = nullptr;
      return SELF_RELEVAL(&pArea->area, lpdbPendingRel);
   }
   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseSetFilter(SQLBASEAREAP pArea, LPDBFILTERINFO pFilterInfo)
{
   if( pArea->lpdbPendingRel ) {
      if( SELF_FORCEREL(&pArea->area) != Harbour::SUCCESS ) {
         return Harbour::FAILURE;
      }
   }
   return SUPER_SETFILTER(&pArea->area, pFilterInfo);
}
#endif

static HB_ERRCODE sqlbaseInit(LPRDDNODE pRDD)
{
   HB_SYMBOL_UNUSED(pRDD);
   s_pItemNewID = hb_itemNew(nullptr);
   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseExit(LPRDDNODE pRDD)
{
   HB_SYMBOL_UNUSED(pRDD);

   if( s_pConnection ) {
      // Disconnect all connections
      for( HB_ULONG ul = 0; ul < s_ulConnectionCount; ul++ ) {
         if( s_pConnection[ul] ) {
            s_pConnection[ul]->pSDD->Disconnect(s_pConnection[ul]);
            hb_xfree(s_pConnection[ul]);
         }
      }
      hb_xfree(s_pConnection);
      s_pConnection = nullptr;
      s_ulConnectionCount = 0;
      s_ulConnectionCurrent = 0;
      if( s_szError ) {
         hb_xfree(s_szError);
         s_szError = nullptr;
      }
      if( s_szQuery ) {
         hb_xfree(s_szQuery);
         s_szQuery = nullptr;
      }
      hb_itemRelease(s_pItemNewID);
      s_pItemNewID = nullptr;
   }

   return Harbour::SUCCESS;
}

static HB_ERRCODE sqlbaseRddInfo(LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem)
{
   HB_SYMBOL_UNUSED(pRDD);

   HB_ULONG ulConn = ulConnect ? ulConnect : s_ulConnectionCurrent;
   SQLDDCONNECTION * pConn;
   if( ulConn > 0 && ulConn <= s_ulConnectionCount ) {
      pConn = s_pConnection[ulConn - 1];
   } else {
      pConn = nullptr;
   }

   switch( uiIndex ) {
      case RDDI_REMOTE:
         hb_itemPutL(pItem, true);
         break;

      case RDDI_CONNECTION: {
         HB_ULONG ulNewConnection = 0;

         if( hb_itemType(pItem) & Harbour::Item::NUMERIC ) {
            ulNewConnection = hb_itemGetNL(pItem);
         }

         hb_itemPutNL(pItem, ulConnect ? ulConnect : s_ulConnectionCurrent);

         if( ulNewConnection ) {
            s_ulConnectionCurrent = ulNewConnection;
         }
         break;
      }

      case RDDI_ISDBF:
         hb_itemPutL(pItem, false);
         break;

      case RDDI_CANPUTREC:
         hb_itemPutL(pItem, true);
         break;

      case RDDI_CONNECT: {
         PSDDNODE pNode = nullptr;
         HB_ULONG ul;

         auto pStr = hb_arrayGetCPtr(pItem, 1);
         if( pStr ) {
            pNode = s_pSdd;
            while( pNode ) {
               if( !hb_stricmp(pNode->Name, pStr) ) {
                  break;
               }
               pNode = pNode->pNext;
            }
         }

         hb_rddsqlSetError(0, nullptr, nullptr, nullptr, 0);
         pConn = static_cast<SQLDDCONNECTION*>(hb_xgrabz(sizeof(SQLDDCONNECTION)));
         if( pNode && pNode->Connect(pConn, pItem) == Harbour::SUCCESS ) {
            pConn->pSDD = pNode;

            // Find free connection handle
            for( ul = 0; ul < s_ulConnectionCount; ul++ ) {
               if( !s_pConnection[ul] ) {
                  break;
               }
            }
            if( ul >= s_ulConnectionCount ) {
               // Realloc connection table
               if( s_pConnection ) {
                  s_pConnection = static_cast<SQLDDCONNECTION**>(hb_xrealloc(s_pConnection, sizeof(SQLDDCONNECTION*) * (s_ulConnectionCount + CONNECTION_LIST_EXPAND)));
               } else {
                  s_pConnection = static_cast<SQLDDCONNECTION**>(hb_xgrab(sizeof(SQLDDCONNECTION*) * CONNECTION_LIST_EXPAND));
               }

               memset(s_pConnection + s_ulConnectionCount, 0, sizeof(SQLDDCONNECTION*) * CONNECTION_LIST_EXPAND);
               ul = s_ulConnectionCount;
               s_ulConnectionCount += CONNECTION_LIST_EXPAND;
            }
            s_pConnection[ul] = pConn;
            ul++;
            s_ulConnectionCurrent = ul;
         } else {
            hb_xfree(pConn);
            ul = 0;
         }

         hb_itemPutNI(pItem, ul);
         break;
      }

      case RDDI_DISCONNECT:
         hb_rddsqlSetError(0, nullptr, nullptr, nullptr, 0);
         if( pConn && !pConn->uiAreaCount && pConn->pSDD->Disconnect(pConn) == Harbour::SUCCESS ) {
            hb_xfree(pConn);
            s_pConnection[ulConn - 1] = nullptr;
            if( s_ulConnectionCurrent == ulConn ) {
               s_ulConnectionCurrent = 0;
            }

            hb_itemPutL(pItem, true);
            return Harbour::SUCCESS;
         }
         hb_itemPutL(pItem, false);
         return Harbour::SUCCESS;

      case RDDI_EXECUTE:
         hb_rddsqlSetError(0, nullptr, nullptr, nullptr, 0);
         if( pConn ) {
            hb_itemPutL(pItem, pConn->pSDD->Execute(pConn, pItem) == Harbour::SUCCESS);
         } else {
            hb_itemPutL(pItem, false);
         }

         return Harbour::SUCCESS;

      case RDDI_ERROR:
         hb_itemPutC(pItem, s_szError);
         return Harbour::SUCCESS;

      case RDDI_ERRORNO:
         hb_itemPutNI(pItem, s_errCode);
         return Harbour::SUCCESS;

      case RDDI_QUERY:
         hb_itemPutC(pItem, s_szQuery);
         return Harbour::SUCCESS;

      case RDDI_INSERTID:
         hb_itemCopy(pItem, s_pItemNewID);
         return Harbour::SUCCESS;

      case RDDI_AFFECTEDROWS:
         hb_itemPutNInt(pItem, s_ulAffectedRows);
         return Harbour::SUCCESS;

#if 0
      default:
         return SUPER_RDDINFO(pRDD, uiIndex, ulConnect, pItem);
#endif

   }

   return Harbour::SUCCESS;
}

/* --- */

static RDDFUNCS sqlbaseTable =
{
   ( DBENTRYP_BP ) nullptr,             /* sqlbaseBof */
   ( DBENTRYP_BP ) nullptr,             /* sqlbaseEof */
   ( DBENTRYP_BP ) nullptr,             /* sqlbaseFound */
   ( DBENTRYP_V ) sqlbaseGoBottom,
   ( DBENTRYP_UL ) sqlbaseGoTo,
   ( DBENTRYP_I ) sqlbaseGoToId,
   ( DBENTRYP_V ) sqlbaseGoTop,
   ( DBENTRYP_BIB ) nullptr,            /* sqlbaseSeek */
   ( DBENTRYP_L ) sqlbaseSkip,
   ( DBENTRYP_L ) nullptr,              /* sqlbaseSkipFilter */
   ( DBENTRYP_L ) sqlbaseSkipRaw,
   ( DBENTRYP_VF ) nullptr,             /* sqlbaseAddField */
   ( DBENTRYP_B ) sqlbaseAppend,
   ( DBENTRYP_I ) nullptr,              /* sqlbaseCreateFields */
   ( DBENTRYP_V ) sqlbaseDeleteRec,
   ( DBENTRYP_BP ) sqlbaseDeleted,
   ( DBENTRYP_SP ) nullptr,             /* sqlbaseFieldCount */
   ( DBENTRYP_VF ) nullptr,             /* sqlbaseFieldDisplay */
   ( DBENTRYP_SSI ) nullptr,            /* sqlbaseFieldInfo */
   ( DBENTRYP_SCP ) nullptr,            /* sqlbaseFieldName */
   ( DBENTRYP_V ) nullptr,              /* sqlbaseFlush */
   ( DBENTRYP_PP ) nullptr,             /* sqlbaseGetRec */
   ( DBENTRYP_SI ) sqlbaseGetValue,
   ( DBENTRYP_SVL ) sqlbaseGetVarLen,
   ( DBENTRYP_V ) sqlbaseGoCold,
   ( DBENTRYP_V ) sqlbaseGoHot,
   ( DBENTRYP_P ) nullptr,              /* sqlbasePutRec */
   ( DBENTRYP_SI ) sqlbasePutValue,
   ( DBENTRYP_V ) sqlbaseRecall,
   ( DBENTRYP_ULP ) sqlbaseRecCount,
   ( DBENTRYP_ISI ) nullptr,            /* sqlbaseRecInfo */
   ( DBENTRYP_ULP ) sqlbaseRecNo,
   ( DBENTRYP_I ) sqlbaseRecId,
   ( DBENTRYP_S ) nullptr,              /* sqlbaseSetFieldExtent */
   ( DBENTRYP_CP ) nullptr,             /* sqlbaseAlias */
   ( DBENTRYP_V ) sqlbaseClose,
   ( DBENTRYP_VO ) sqlbaseCreate,
   ( DBENTRYP_SI ) sqlbaseInfo,
   ( DBENTRYP_V ) nullptr,              /* sqlbaseNewArea */
   ( DBENTRYP_VO ) sqlbaseOpen,
   ( DBENTRYP_V ) nullptr,              /* sqlbaseRelease */
   ( DBENTRYP_SP ) sqlbaseStructSize,
   ( DBENTRYP_CP ) nullptr,             /* sqlbaseSysName */
   ( DBENTRYP_VEI ) nullptr,            /* sqlbaseEval */
   ( DBENTRYP_V ) nullptr,              /* sqlbasePack */
   ( DBENTRYP_LSP ) nullptr,            /* sqlbasePackRec */
   ( DBENTRYP_VS ) nullptr,             /* sqlbaseSort */
   ( DBENTRYP_VT ) nullptr,             /* sqlbaseTrans */
   ( DBENTRYP_VT ) nullptr,             /* sqlbaseTransRec */
   ( DBENTRYP_V ) sqlbaseZap,
   ( DBENTRYP_VR ) nullptr,             /* sqlbaseChildEnd */
   ( DBENTRYP_VR ) nullptr,             /* sqlbaseChildStart */
   ( DBENTRYP_VR ) nullptr,             /* sqlbaseChildSync */
   ( DBENTRYP_V ) nullptr,              /* sqlbaseSyncChildren */
   ( DBENTRYP_V ) nullptr,              /* sqlbaseClearRel */
   ( DBENTRYP_V ) nullptr,              /* sqlbaseForceRel */
   ( DBENTRYP_SSP ) nullptr,            /* sqlbaseRelArea */
   ( DBENTRYP_VR ) nullptr,             /* sqlbaseRelEval */
   ( DBENTRYP_SI ) nullptr,             /* sqlbaseRelText */
   ( DBENTRYP_VR ) nullptr,             /* sqlbaseSetRel */
   ( DBENTRYP_VOI ) nullptr,            /* sqlbaseOrderListAdd */
   ( DBENTRYP_V ) nullptr,              /* sqlbaseOrderListClear */
   ( DBENTRYP_VOI ) nullptr,            /* sqlbaseOrderListDelete */
   ( DBENTRYP_VOI ) nullptr,            /* sqlbaseOrderListFocus */
   ( DBENTRYP_V ) nullptr,              /* sqlbaseOrderListRebuild */
   ( DBENTRYP_VOO ) nullptr,            /* sqlbaseOrderCondition */
   ( DBENTRYP_VOC ) nullptr,            /* sqlbaseOrderCreate */
   ( DBENTRYP_VOI ) nullptr,            /* sqlbaseOrderDestroy */
   ( DBENTRYP_SVOI ) nullptr,           /* sqlbaseOrderInfo */
   ( DBENTRYP_V ) nullptr,              /* sqlbaseClearFilter */
   ( DBENTRYP_V ) nullptr,              /* sqlbaseClearLocate */
   ( DBENTRYP_V ) nullptr,              /* sqlbaseClearScope */
   ( DBENTRYP_VPLP ) nullptr,           /* sqlbaseCountScope */
   ( DBENTRYP_I ) nullptr,              /* sqlbaseFilterText */
   ( DBENTRYP_SI ) nullptr,             /* sqlbaseScopeInfo */
   ( DBENTRYP_VFI ) nullptr,            /* sqlbaseSetFilter */
   ( DBENTRYP_VLO ) nullptr,            /* sqlbaseSetLocate */
   ( DBENTRYP_VOS ) nullptr,            /* sqlbaseSetScope */
   ( DBENTRYP_VPL ) nullptr,            /* sqlbaseSkipScope */
   ( DBENTRYP_B ) nullptr,              /* sqlbaseLocate */
   ( DBENTRYP_CC ) nullptr,             /* sqlbaseCompile */
   ( DBENTRYP_I ) nullptr,              /* sqlbaseError */
   ( DBENTRYP_I ) nullptr,              /* sqlbaseEvalBlock */
   ( DBENTRYP_VSP ) nullptr,            /* sqlbaseRawLock */
   ( DBENTRYP_VL ) nullptr,             /* sqlbaseLock */
   ( DBENTRYP_I ) nullptr,              /* sqlbaseUnLock */
   ( DBENTRYP_V ) nullptr,              /* sqlbaseCloseMemFile */
   ( DBENTRYP_VO ) nullptr,             /* sqlbaseCreateMemFile */
   ( DBENTRYP_SCCS ) nullptr,           /* sqlbaseGetValueFile */
   ( DBENTRYP_VO ) nullptr,             /* sqlbaseOpenMemFile */
   ( DBENTRYP_SCCS ) nullptr,           /* sqlbasePutValueFile */
   ( DBENTRYP_V ) nullptr,              /* sqlbaseReadDBHeader */
   ( DBENTRYP_V ) nullptr,              /* sqlbaseWriteDBHeader */
   ( DBENTRYP_R ) sqlbaseInit,
   ( DBENTRYP_R ) sqlbaseExit,
   ( DBENTRYP_RVVL ) nullptr,           /* sqlbaseDrop */
   ( DBENTRYP_RVVL ) nullptr,           /* sqlbaseExists */
   ( DBENTRYP_RVVVL ) nullptr,          /* sqlbaseRename */
   ( DBENTRYP_RSLV ) sqlbaseRddInfo,
   ( DBENTRYP_SVP ) nullptr             /* sqlbaseWhoCares */
};

/* --- Module initialization code --- */

HB_FUNC( SQLBASE )
{
}

HB_FUNC_STATIC( SQLBASE_GETFUNCTABLE )
{
   auto puiCount = static_cast<HB_USHORT*>(hb_parptr(1));
   auto pTable = static_cast<RDDFUNCS*>(hb_parptr(2));
   auto uiRddId = static_cast<HB_USHORT>(hb_parni(4));

   if( pTable ) {
      if( puiCount ) {
         *puiCount = RDDFUNCSCOUNT;
      }

      HB_ERRCODE errCode = hb_rddInheritEx(pTable, &sqlbaseTable, &sqlbaseSuper, nullptr, nullptr);
      if( errCode == Harbour::SUCCESS ) {
         s_rddidSQLBASE = uiRddId;
      }

      hb_retni(errCode);
   } else {
      hb_retni(Harbour::FAILURE);
   }
}

static void hb_sqlbaseInit(void * cargo)
{
   HB_SYMBOL_UNUSED(cargo);

   if( hb_rddRegister("SQLBASE", RDT_FULL) > 1 ) {
      hb_errInternal(HB_EI_RDDINVALID, nullptr, nullptr, nullptr);
   }
}

HB_INIT_SYMBOLS_BEGIN(sqlbase__InitSymbols)
{
   "SQLBASE", {HB_FS_PUBLIC}, {HB_FUNCNAME(SQLBASE)}, nullptr
},
{"SQLBASE_GETFUNCTABLE", {HB_FS_PUBLIC}, {HB_FUNCNAME(SQLBASE_GETFUNCTABLE)}, nullptr}
HB_INIT_SYMBOLS_END(sqlbase__InitSymbols)

HB_CALL_ON_STARTUP_BEGIN(_hb_sqlbase_init_)
hb_vmAtInit(hb_sqlbaseInit, nullptr);
HB_CALL_ON_STARTUP_END(_hb_sqlbase_init_)

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup sqlbase__InitSymbols
   #pragma startup _hb_sqlbase_init_
#elif defined(HB_DATASEG_STARTUP)
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC(sqlbase__InitSymbols) \
   HB_DATASEG_FUNC(_hb_sqlbase_init_)
   #include "hbiniseg.hpp"
#endif
