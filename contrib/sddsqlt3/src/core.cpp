//
// SQLite3 Database Driver
//
// Copyright 2010 Viktor Szakats (vszakats.net/harbour)
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

#include "hbrddsql.hpp"

#include <hbapiitm.hpp>
#include <hbapistr.hpp>
#include <hbapicdp.hpp>
#include <hbdate.hpp>
#include <hbset.hpp>
#include <hbvm.hpp>
#include <hbset.hpp>

#include <sqlite3.h>

#define S_HB_ARRAYGETSTR(arr, n, phstr, plen) hb_arrayGetStrUTF8(arr, n, phstr, plen)
#define S_HB_ITEMCOPYSTR(itm, str, len) hb_itemCopyStrUTF8(itm, str, len)
#define S_HB_ITEMGETSTR(itm, phstr, plen) hb_itemGetStrUTF8(itm, phstr, plen)
#define S_HB_ITEMPUTSTR(itm, str) hb_itemPutStrUTF8(itm, str)
#define S_HB_ITEMPUTSTRLEN(itm, str, len) hb_itemPutStrLenUTF8(itm, str, len)

struct SDDCONN
{
  sqlite3 *pDb;
};

struct SDDDATA
{
  sqlite3_stmt *pStmt;
};

static HB_ERRCODE sqlite3Connect(SQLDDCONNECTION *pConnection, PHB_ITEM pItem);
static HB_ERRCODE sqlite3Disconnect(SQLDDCONNECTION *pConnection);
static HB_ERRCODE sqlite3Execute(SQLDDCONNECTION *pConnection, PHB_ITEM pItem);
static HB_ERRCODE sqlite3Open(SQLBASEAREAP pArea);
static HB_ERRCODE sqlite3Close(SQLBASEAREAP pArea);
static HB_ERRCODE sqlite3GoTo(SQLBASEAREAP pArea, HB_ULONG ulRecNo);

static SDDNODE s_sqlt3dd = {nullptr,
                            "SQLITE3",
                            static_cast<SDDFUNC_CONNECT>(sqlite3Connect),
                            static_cast<SDDFUNC_DISCONNECT>(sqlite3Disconnect),
                            static_cast<SDDFUNC_EXECUTE>(sqlite3Execute),
                            static_cast<SDDFUNC_OPEN>(sqlite3Open),
                            static_cast<SDDFUNC_CLOSE>(sqlite3Close),
                            static_cast<SDDFUNC_GOTO>(sqlite3GoTo),
                            static_cast<SDDFUNC_GETVALUE>(nullptr),
                            static_cast<SDDFUNC_GETVARLEN>(nullptr)};

static void hb_sqlt3dd_init(void *cargo)
{
  HB_SYMBOL_UNUSED(cargo);

#if SQLITE_VERSION_NUMBER >= 3006000
  sqlite3_initialize();
#endif

  if (!hb_sddRegister(&s_sqlt3dd))
  {
    hb_errInternal(HB_EI_RDDINVALID, nullptr, nullptr, nullptr);
  }
}

static void hb_sqlt3dd_exit(void *cargo)
{
  HB_SYMBOL_UNUSED(cargo);

#if SQLITE_VERSION_NUMBER >= 3006000
  sqlite3_shutdown();
#endif
}

HB_FUNC(HB_SDDSQLITE3_REGISTER)
{
  hb_sqlt3dd_init(nullptr);
}

/* force SQLBASE linking */
HB_FUNC_TRANSLATE(SDDSQLITE3, SQLBASE)

HB_INIT_SYMBOLS_BEGIN(sqlt3dd__InitSymbols){"SDDSQLITE3", {HB_FS_PUBLIC}, {HB_FUNCNAME(SDDSQLITE3)}, nullptr},
    HB_INIT_SYMBOLS_END(sqlt3dd__InitSymbols)

        HB_CALL_ON_STARTUP_BEGIN(_hb_sqlt3dd_init_) hb_vmAtInit(hb_sqlt3dd_init, nullptr);
hb_vmAtExit(hb_sqlt3dd_exit, nullptr);
HB_CALL_ON_STARTUP_END(_hb_sqlt3dd_init_)

#if defined(HB_PRAGMA_STARTUP)
#pragma startup sqlt3dd__InitSymbols
#pragma startup _hb_sqlt3dd_init_
#elif defined(HB_DATASEG_STARTUP)
#define HB_DATASEG_BODY                                                                                                \
  HB_DATASEG_FUNC(sqlt3dd__InitSymbols)                                                                                \
  HB_DATASEG_FUNC(_hb_sqlt3dd_init_)
#include "hbiniseg.hpp"
#endif

/* --- */
static HB_USHORT hb_errRT_SQLT3DD(HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char *szDescription,
                                  const char *szOperation, HB_ERRCODE errOsCode)
{
  auto pError =
      hb_errRT_New(ES_ERROR, "SDDSQLITE3", errGenCode, errSubCode, szDescription, szOperation, errOsCode, EF_NONE);
  HB_USHORT uiAction = hb_errLaunch(pError);
  hb_itemRelease(pError);
  return uiAction;
}

static char *sqlite3GetError(sqlite3 *pDb, HB_ERRCODE *pErrCode)
{
  char *szRet;
  int iNativeErr;

  if (pDb)
  {
    PHB_ITEM pRet = S_HB_ITEMPUTSTR(nullptr, sqlite3_errmsg(pDb));
    szRet = hb_strdup(hb_itemGetCPtr(pRet));
    hb_itemRelease(pRet);
    iNativeErr = sqlite3_errcode(pDb);
  }
  else
  {
    szRet = hb_strdup("Could not get the error message");
    iNativeErr = 9999;
  }

  if (pErrCode)
  {
    *pErrCode = static_cast<HB_ERRCODE>(iNativeErr);
  }

  return szRet;
}

static HB_USHORT sqlite3DeclType(sqlite3_stmt *st, HB_USHORT uiIndex)
{
  const char *szDeclType = sqlite3_column_decltype(st, uiIndex);
  /* the order of comparisons below is important to replicate
   * type precedence used by SQLITE3
   */
  if (szDeclType != nullptr)
  {
    HB_SIZE nLen = strlen(szDeclType);

    if (hb_strAtI("INT", 3, szDeclType, nLen) != 0)
    {
      return Harbour::DB::Field::INTEGER;
    }
    if (hb_strAtI("CHAR", 4, szDeclType, nLen) != 0 || hb_strAtI("TEXT", 4, szDeclType, nLen) != 0 ||
        hb_strAtI("CLOB", 4, szDeclType, nLen) != 0)
    {
      return Harbour::DB::Field::STRING;
    }
    if (hb_strAtI("BLOB", 4, szDeclType, nLen) != 0)
    {
      return Harbour::DB::Field::ANY;
    }
    if (hb_strAtI("REAL", 4, szDeclType, nLen) != 0 || hb_strAtI("FLOA", 4, szDeclType, nLen) != 0 ||
        hb_strAtI("DOUB", 4, szDeclType, nLen) != 0)
    {
      return Harbour::DB::Field::LONG; /* logically HB_FT_DOUBLE, what was the idea? */
    }
#ifdef HB_SQLT3_MAP_DECLARED_EMULATED
    /* types not handled in a specific way by SQLITE3
     * but anyway we should try to look at declarations
     */
    if (hb_strAtI("TIME", 4, szDeclType, nLen) != 0)
    {
      return Harbour::DB::Field::TIMESTAMP;
    }
    if (hb_strAtI("DATE", 4, szDeclType, nLen) != 0)
    {
      return Harbour::DB::Field::DATE;
    }
    if (hb_strAtI("NUME", 4, szDeclType, nLen) != 0 || hb_strAtI("NUMB", 4, szDeclType, nLen) != 0)
    {
      return Harbour::DB::Field::LONG;
    }
#endif
  }

#ifdef HB_SQLT3_MAP_UNDECLARED_TYPES_AS_ANY
  return Harbour::DB::Field::ANY;
#else
  switch (sqlite3_column_type(st, uiIndex))
  {
  case SQLITE_TEXT:
    return Harbour::DB::Field::STRING;

  case SQLITE_FLOAT:
    return Harbour::DB::Field::LONG;

  case SQLITE_INTEGER:
    return Harbour::DB::Field::INTEGER;

  case SQLITE_BLOB:
    return Harbour::DB::Field::BLOB;

  case SQLITE_NULL:
    return Harbour::DB::Field::ANY;
  }

  return Harbour::DB::Field::NONE;
#endif
}

#ifdef HB_SQLT3_MAP_DECLARED_EMULATED
static void sqlite3DeclStru(sqlite3_stmt *st, HB_USHORT uiIndex, HB_USHORT *puiLen, HB_USHORT *puiDec)
{
  const char *szDeclType = sqlite3_column_decltype(st, uiIndex);

  if (szDeclType != nullptr)
  {
    HB_SIZE nLen = strlen(szDeclType);
    HB_SIZE nAt;
    int iOverflow;
    HB_MAXINT iRetLen = 0;

    /* SQLite doesn't normally have field size limits,
     * but column declarations are freeform - let's
     * try some really stupid guesswork on schema...
     */

    if ((nAt = hb_strAt("(", 1, szDeclType, nLen)) > 0)
    {
      if (puiLen)
      {
        iRetLen = hb_strValInt(szDeclType + nAt, &iOverflow);
        if (!puiDec || (iRetLen > 0 && iRetLen < 100))
        {
          *puiLen = static_cast<HB_USHORT>(iRetLen);
        }
      }

      if (!puiDec)
      {
        return;
      }

      if (*puiLen < 2)
      {
        *puiDec = 0;
      }
      else if (puiLen && (nAt = hb_strAt(",", 1, szDeclType + nAt, nLen - nAt - 1)) > 0)
      {
        if ((iRetLen = hb_strValInt(szDeclType + nAt, &iOverflow)) > 0)
        {
          *puiDec = static_cast<HB_USHORT>(HB_MIN(*puiLen - 1, iRetLen));

          /* SQL column declaration doesn't include space for
           * decimal separator, while xBase stores it.
           */

          *puiLen = static_cast<HB_USHORT>(++iRetLen);
        }
        else if (iRetLen == 0)
        {
          *puiDec = 0;
        }
      }
    }
  }
}
#endif

/* --- SDD METHODS --- */
static HB_ERRCODE sqlite3Connect(SQLDDCONNECTION *pConnection, PHB_ITEM pItem)
{
  sqlite3 *db;
  void *hConn;

  if (sqlite3_open(S_HB_ARRAYGETSTR(pItem, 2, &hConn, nullptr), &db) == SQLITE_OK)
  {
    pConnection->pSDDConn = hb_xgrab(sizeof(SDDCONN));
    (static_cast<SDDCONN *>(pConnection->pSDDConn))->pDb = db;
  }
  else
  {
    sqlite3_close(db);
  }

  hb_strfree(hConn);

  return db ? Harbour::SUCCESS : Harbour::FAILURE;
}

static HB_ERRCODE sqlite3Disconnect(SQLDDCONNECTION *pConnection)
{
  HB_ERRCODE errCode = sqlite3_close((static_cast<SDDCONN *>(pConnection->pSDDConn))->pDb) == SQLITE_OK
                           ? Harbour::SUCCESS
                           : Harbour::FAILURE;
  if (errCode == Harbour::SUCCESS)
  {
    hb_xfree(pConnection->pSDDConn);
  }

  return errCode;
}

static HB_ERRCODE sqlite3Execute(SQLDDCONNECTION *pConnection, PHB_ITEM pItem)
{
  sqlite3 *pDb = (static_cast<SDDCONN *>(pConnection->pSDDConn))->pDb;
  HB_ERRCODE errCode;
  int iRow, iCol;
  void *hStatement;
  char **pResult = nullptr;
  char *pszErrMsg = nullptr;

  if (sqlite3_get_table(pDb, S_HB_ITEMGETSTR(pItem, &hStatement, nullptr), &pResult, &iRow, &iCol, &pszErrMsg) !=
      SQLITE_OK)
  {
    hb_strfree(hStatement);
    hb_xfree(sqlite3GetError(pDb, &errCode));
    hb_errRT_SQLT3DD(EG_OPEN, ESQLDD_STMTALLOC, pszErrMsg, hb_itemGetCPtr(pItem), errCode);
    hb_xfree(pszErrMsg);
    return Harbour::FAILURE;
  }
  else
  {
    hb_strfree(hStatement);
  }

  sqlite3_free_table(pResult);

  /* TODO: new id */
  hb_rddsqlSetError(0, nullptr, hb_itemGetCPtr(pItem), nullptr, static_cast<unsigned long>(iRow));
  return Harbour::SUCCESS;
}

static HB_ERRCODE sqlite3Open(SQLBASEAREAP pArea)
{
  sqlite3 *pDb = (static_cast<SDDCONN *>(pArea->pConnection->pSDDConn))->pDb;

  pArea->pSDDData = memset(hb_xgrab(sizeof(SDDDATA)), 0, sizeof(SDDDATA));
  auto pSDDData = static_cast<SDDDATA *>(pArea->pSDDData);

  auto pItem = hb_itemPutC(nullptr, pArea->szQuery);
  void *hQuery;
  HB_SIZE nQueryLen;
  const char *pszQuery = S_HB_ITEMGETSTR(pItem, &hQuery, &nQueryLen);

  sqlite3_stmt *st = nullptr;

#if SQLITE_VERSION_NUMBER >= 3020000
  int result = sqlite3_prepare_v3(pDb, pszQuery, static_cast<int>(nQueryLen), 0, &st, nullptr);
#else
  int result = sqlite3_prepare_v2(pDb, pszQuery, static_cast<int>(nQueryLen), &st, nullptr);
#endif

  char *szError;
  HB_ERRCODE errCode;

  if (result != SQLITE_OK)
  {
    hb_strfree(hQuery);
    hb_itemRelease(pItem);
    szError = sqlite3GetError(pDb, &errCode);
    hb_errRT_SQLT3DD(EG_OPEN, ESQLDD_INVALIDQUERY, szError, pArea->szQuery, errCode);
    sqlite3_finalize(st);
    hb_xfree(szError);
    return Harbour::FAILURE;
  }
  else
  {
    hb_strfree(hQuery);
    hb_itemRelease(pItem);
  }

  int iStatus;

  if ((iStatus = sqlite3_step(st)) == SQLITE_DONE)
  {
    pArea->fFetched = true;
  }
  else if (iStatus != SQLITE_ROW)
  {
    szError = sqlite3GetError(pDb, &errCode);
    hb_errRT_SQLT3DD(EG_OPEN, ESQLDD_INVALIDQUERY, szError, pArea->szQuery, errCode);
    sqlite3_finalize(st);
    hb_xfree(szError);
    return Harbour::FAILURE;
  }

  auto uiFields = static_cast<HB_USHORT>(sqlite3_column_count(st));
  SELF_SETFIELDEXTENT(&pArea->area, uiFields);

  PHB_ITEM pName = nullptr;
  errCode = 0;
  bool bError = false;
  auto pItemEof = hb_itemArrayNew(uiFields);
  for (HB_USHORT uiIndex = 0; uiIndex < uiFields; ++uiIndex)
  {
    DBFIELDINFO dbFieldInfo{};
#ifdef HB_SQLT3_FIELDNAME_STRICT
    HB_SIZE nPos;
#endif
    pName = S_HB_ITEMPUTSTR(pName, sqlite3_column_name(st, uiIndex));

#ifdef HB_SQLT3_FIELDNAME_STRICT
    /* WA->T.FIELD syntax is not valid, but FieldPos("t.field") is OK */
    if ((nPos = hb_strAt(".", 1, hb_itemGetCPtr(pName), hb_itemGetCLen(pName))) != 0)
    {
      dbFieldInfo.atomName = hb_itemGetCPtr(pName) + nPos;
    }
    else
#endif
    {
      dbFieldInfo.atomName = hb_itemGetCPtr(pName);
    }

    dbFieldInfo.uiType = sqlite3DeclType(st, uiIndex);
    pItem = hb_arrayGetItemPtr(pItemEof, uiIndex + 1);

    /* There are no field length limits stored in the SQLite3 database,
       so we're resorting to setting some arbitrary default values to
       make apps relying on these (f.e. Browse()/GET) to behave somewhat
       better. For better results, update apps to untie UI metrics from
       any database field/value widths. [vszakats] */

    switch (dbFieldInfo.uiType)
    {
    case Harbour::DB::Field::STRING:
    {
      HB_SIZE nSize = hb_cdpUTF8StringLength(reinterpret_cast<const char *>(sqlite3_column_text(st, uiIndex)),
                                             sqlite3_column_bytes(st, uiIndex));

      /* sqlite3_column_bytes() returns variable lengths for UTF-8
         strings - *_bytes16() UTF-16 could do that too, but not
         for mostly used character sets. Yet seems better to use
         hb_cdpUTF8StringLength() */

      HB_USHORT uiRetLen = 10;

#ifdef HB_SQLT3_MAP_DECLARED_EMULATED
      sqlite3DeclStru(st, uiIndex, &uiRetLen, nullptr);
#endif
      dbFieldInfo.uiLen = static_cast<HB_USHORT>(HB_MAX(nSize, static_cast<HB_SIZE>(uiRetLen)));
      auto pStr = static_cast<char *>(hb_xgrab(static_cast<HB_SIZE>(dbFieldInfo.uiLen) + 1));
      memset(pStr, ' ', dbFieldInfo.uiLen);
      hb_itemPutCLPtr(pItem, pStr, dbFieldInfo.uiLen);
      break;
    }
    case Harbour::DB::Field::BLOB:
      dbFieldInfo.uiLen = 4;
      hb_itemPutC(pItem, nullptr);
      break;

    case Harbour::DB::Field::INTEGER:
      dbFieldInfo.uiLen = 8;
      hb_itemPutNInt(pItem, 0);
      break;

    case Harbour::DB::Field::LONG:
      dbFieldInfo.uiLen = 20;
      dbFieldInfo.uiDec = static_cast<HB_USHORT>(hb_setGetDecimals());
#ifdef HB_SQLT3_MAP_DECLARED_EMULATED
      sqlite3DeclStru(st, uiIndex, &dbFieldInfo.uiLen, &dbFieldInfo.uiDec);
#endif
      hb_itemPutNDDec(pItem, 0.0, dbFieldInfo.uiDec);
      break;

#ifdef HB_SQLT3_MAP_DECLARED_EMULATED
    case Harbour::DB::Field::DATE:
      dbFieldInfo.uiLen = 8;
      hb_itemPutDS(pItem, nullptr);
      break;

    case Harbour::DB::Field::TIMESTAMP:
      dbFieldInfo.uiLen = 8;
      hb_itemPutTDT(pItem, 0, 0);
      break;
#endif

    case Harbour::DB::Field::ANY:
      dbFieldInfo.uiLen = 6;
      break;

    default:
      bError = true;
    }

    if (!bError)
    {
      bError = (SELF_ADDFIELD(&pArea->area, &dbFieldInfo) == Harbour::FAILURE);
    }

    if (bError)
    {
      break;
    }
  }
  hb_itemRelease(pName);

  if (bError)
  {
    hb_itemRelease(pItemEof);
    sqlite3_finalize(st);
    hb_errRT_SQLT3DD(EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", pArea->szQuery, errCode);
    return Harbour::FAILURE;
  }

  pArea->ulRecCount = 0;
  pArea->ulRecMax = SQLDD_ROWSET_INIT;

  pArea->pRow = static_cast<void **>(hb_xgrab(SQLDD_ROWSET_INIT * sizeof(void *)));
  pArea->pRowFlags = static_cast<HB_BYTE *>(hb_xgrab(SQLDD_ROWSET_INIT * sizeof(HB_BYTE)));

  pArea->pRow[0] = pItemEof;
  pArea->pRowFlags[0] = SQLDD_FLAG_CACHED;

  pSDDData->pStmt = st;
  return Harbour::SUCCESS;
}

static HB_ERRCODE sqlite3Close(SQLBASEAREAP pArea)
{
  auto pSDDData = static_cast<SDDDATA *>(pArea->pSDDData);

  if (pSDDData)
  {
    if (pSDDData->pStmt)
    {
      sqlite3_finalize(pSDDData->pStmt);
    }

    hb_xfree(pSDDData);
    pArea->pSDDData = nullptr;
  }
  return Harbour::SUCCESS;
}

static HB_ERRCODE sqlite3GoTo(SQLBASEAREAP pArea, HB_ULONG ulRecNo)
{
  sqlite3_stmt *st = (static_cast<SDDDATA *>(pArea->pSDDData))->pStmt;

  while (ulRecNo > pArea->ulRecCount && !pArea->fFetched)
  {
    auto pArray = hb_itemArrayNew(pArea->area.uiFieldCount);

    for (HB_USHORT ui = 0; ui < pArea->area.uiFieldCount; ++ui)
    {
      PHB_ITEM pItem = nullptr;
      LPFIELD pField = pArea->area.lpFields + ui;
      HB_USHORT uiType = pField->uiType;

      if (uiType == Harbour::DB::Field::ANY)
      {
        switch (sqlite3_column_type(st, ui))
        {
        case SQLITE_TEXT:
          uiType = Harbour::DB::Field::STRING;
          break;

        case SQLITE_FLOAT:
        case SQLITE_INTEGER:
          uiType = Harbour::DB::Field::LONG;
          break;

        case SQLITE_BLOB:
          uiType = Harbour::DB::Field::BLOB;
          break;
        }
      }

      switch (uiType)
      {
      case Harbour::DB::Field::STRING:
        pItem = S_HB_ITEMPUTSTR(nullptr, reinterpret_cast<const char *>(sqlite3_column_text(st, ui)));
        break;

      case Harbour::DB::Field::INTEGER:
#if HB_VMLONG_MAX > INT32_MAX && !defined(HB_LONG_LONG_OFF)
        pItem = hb_itemPutNInt(nullptr, sqlite3_column_int64(st, ui));
        break;
#endif
      case Harbour::DB::Field::LONG:
        pItem = hb_itemPutNDDec(nullptr, sqlite3_column_double(st, ui), pField->uiDec);
        break;

#ifdef HB_SQLT3_MAP_DECLARED_EMULATED
      case Harbour::DB::Field::DATE:
        if (sqlite3_column_bytes(st, ui) >= 10)
        {
          char szDate[9];
          auto pValue = static_cast<const char *>(sqlite3_column_text(st, ui));
          szDate[0] = pValue[0];
          szDate[1] = pValue[1];
          szDate[2] = pValue[2];
          szDate[3] = pValue[3];
          szDate[4] = pValue[5];
          szDate[5] = pValue[6];
          szDate[6] = pValue[8];
          szDate[7] = pValue[9];
          szDate[8] = '\0';
          pItem = hb_itemPutDS(nullptr, szDate);
        }
        else if (sqlite3_column_bytes(st, ui) == 8)
        {
          pItem = hb_itemPutDS(nullptr, static_cast<const char *>(sqlite3_column_text(st, ui)));
        }
        break;

      case Harbour::DB::Field::TIMESTAMP:
        if (sqlite3_column_bytes(st, ui) >= 10)
        {
          long lDate, lTime;
          auto pValue = static_cast<const char *>(sqlite3_column_text(st, ui));
          hb_timeStampStrGetDT(pValue, &lDate, &lTime);
          pItem = hb_itemPutTDT(nullptr, lDate, lTime);
          break;
        }
#endif
      case Harbour::DB::Field::BLOB:
        pItem =
            hb_itemPutCL(nullptr, static_cast<const char *>(sqlite3_column_blob(st, ui)), sqlite3_column_bytes(st, ui));
        break;
      }

      if (pItem != nullptr)
      {
        hb_arraySetForward(pArray, ui + 1, pItem);
        hb_itemRelease(pItem);
      }
    }
    if (pArea->ulRecCount + 1 >= pArea->ulRecMax)
    {
      pArea->pRow =
          static_cast<void **>(hb_xrealloc(pArea->pRow, (pArea->ulRecMax + SQLDD_ROWSET_RESIZE) * sizeof(void *)));
      pArea->pRowFlags = static_cast<HB_BYTE *>(
          hb_xrealloc(pArea->pRowFlags, (pArea->ulRecMax + SQLDD_ROWSET_RESIZE) * sizeof(HB_BYTE)));
      pArea->ulRecMax += SQLDD_ROWSET_RESIZE;
    }

    pArea->ulRecCount++;
    pArea->pRow[pArea->ulRecCount] = pArray;
    pArea->pRowFlags[pArea->ulRecCount] = SQLDD_FLAG_CACHED;

    if (sqlite3_step(st) != SQLITE_ROW)
    {
      pArea->fFetched = true;
      break;
    }
  }

  if (ulRecNo == 0 || ulRecNo > pArea->ulRecCount)
  {
    pArea->pRecord = pArea->pRow[0];
    pArea->bRecordFlags = pArea->pRowFlags[0];
    pArea->fPositioned = false;
  }
  else
  {
    pArea->pRecord = pArea->pRow[ulRecNo];
    pArea->bRecordFlags = pArea->pRowFlags[ulRecNo];
    pArea->fPositioned = true;
  }
  return Harbour::SUCCESS;
}
