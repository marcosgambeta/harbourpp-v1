/*
 * SQLite3 library low-level (client API) interface code
 *
 * Copyright 2007-2010 P.Chornyj <myorg63@mail.ru>
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

#include "sqlite3.h"

#include <hbapi.hpp>
#include <hbvm.hpp>
#include <hbapiitm.hpp>
#include <hbapierr.hpp>
#include <hbapifs.hpp>
#include <hbapistr.hpp>
#include <hbstack.hpp>

/* FIXME: verify the exact SQLITE3 version */
#if SQLITE_VERSION_NUMBER <= 3004001
#define sqlite3_int64 HB_LONGLONG
#define sqlite3_uint64 HB_ULONGLONG
#endif

#define HB_SQLITE3_DB 6000001

#define HB_ERR_MEMSTRU_NOT_MEM_BLOCK 4001
#define HB_ERR_MEMSTRU_WRONG_MEMSTRU_BLOCK 4002
#define HB_ERR_MEMSTRU_DESTROYED 4003

#ifdef SQLITE3_DYNLIB
extern char *sqlite3_temp_directory;
#endif // SQLITE3_DYNLIB

static PHB_ITEM hb_sqlite3_itemPut(PHB_ITEM pItem, void *pMemAddr, int iType);
static void *hb_sqlite3_itemGet(PHB_ITEM pItem, int iType, bool fError);
static void hb_sqlite3_ret(void *pMemAddr, int iType);
static void *hb_sqlite3_param(int iParam, int iType, bool fError);

static int callback(void *, int, char **, char **);
static int authorizer(void *, int, const char *, const char *, const char *, const char *);
static int busy_handler(void *, int);
static int progress_handler(void *);
static int hook_commit(void *);
static void hook_rollback(void *);
static void func(sqlite3_context *, int, sqlite3_value **);
#if SQLITE_VERSION_NUMBER >= 3014000
static int trace_handler(unsigned, void *, void *, void *);
#endif

struct HB_SQLITE3
{
  sqlite3 *db;
  PHB_ITEM cbAuthorizer;
  PHB_ITEM cbBusyHandler;
  PHB_ITEM cbProgressHandler;
  PHB_ITEM cbHookCommit;
  PHB_ITEM cbHookRollback;
  PHB_ITEM cbFunc;
#if SQLITE_VERSION_NUMBER >= 3014000
  PHB_ITEM cbTraceHandler;
#else
  PHB_ITEM sProfileFileName;
  PHB_ITEM sTraceFileName;
#endif
};

using PHB_SQLITE3 = HB_SQLITE3 *;

struct HB_SQLITE3_HOLDER
{
  int type;
  HB_SQLITE3 *hbsqlite3;
};

using PHB_SQLITE3_HOLDER = HB_SQLITE3_HOLDER *;

using psqlite3_stmt = sqlite3_stmt *;

/**
   destructor, it's executed automatically
 */

static HB_GARBAGE_FUNC(hb_sqlite3_destructor)
{
  auto pStructHolder = static_cast<PHB_SQLITE3_HOLDER>(Cargo);

  if (pStructHolder && pStructHolder->hbsqlite3)
  {
    if (pStructHolder->hbsqlite3->db)
    {
      sqlite3_close(pStructHolder->hbsqlite3->db);
      pStructHolder->hbsqlite3->db = nullptr;
    }

    if (pStructHolder->hbsqlite3->cbAuthorizer)
    {
      hb_itemRelease(pStructHolder->hbsqlite3->cbAuthorizer);
      pStructHolder->hbsqlite3->cbAuthorizer = nullptr;
    }

    if (pStructHolder->hbsqlite3->cbBusyHandler)
    {
      hb_itemRelease(pStructHolder->hbsqlite3->cbBusyHandler);
      pStructHolder->hbsqlite3->cbBusyHandler = nullptr;
    }

    if (pStructHolder->hbsqlite3->cbProgressHandler)
    {
      hb_itemRelease(pStructHolder->hbsqlite3->cbProgressHandler);
      pStructHolder->hbsqlite3->cbProgressHandler = nullptr;
    }

    if (pStructHolder->hbsqlite3->cbHookCommit)
    {
      hb_itemRelease(pStructHolder->hbsqlite3->cbHookCommit);
      pStructHolder->hbsqlite3->cbHookCommit = nullptr;
    }

    if (pStructHolder->hbsqlite3->cbHookRollback)
    {
      hb_itemRelease(pStructHolder->hbsqlite3->cbHookRollback);
      pStructHolder->hbsqlite3->cbHookRollback = nullptr;
    }

    if (pStructHolder->hbsqlite3->cbFunc)
    {
      hb_itemRelease(pStructHolder->hbsqlite3->cbFunc);
      pStructHolder->hbsqlite3->cbFunc = nullptr;
    }

#if SQLITE_VERSION_NUMBER >= 3014000
    if (pStructHolder->hbsqlite3->cbTraceHandler)
    {
      hb_itemRelease(pStructHolder->hbsqlite3->cbTraceHandler);
      pStructHolder->hbsqlite3->cbTraceHandler = nullptr;
    }
#else
    if (pStructHolder->hbsqlite3->sProfileFileName)
    {
      hb_itemRelease(pStructHolder->hbsqlite3->sProfileFileName);
      pStructHolder->hbsqlite3->sProfileFileName = nullptr;
    }

    if (pStructHolder->hbsqlite3->sTraceFileName)
    {
      hb_itemRelease(pStructHolder->hbsqlite3->sTraceFileName);
      pStructHolder->hbsqlite3->sTraceFileName = nullptr;
    }
#endif

    hb_xfree(pStructHolder->hbsqlite3);
    pStructHolder->hbsqlite3 = nullptr;
  }
}

static HB_GARBAGE_FUNC(hb_sqlite3_mark)
{
  auto pStructHolder = static_cast<PHB_SQLITE3_HOLDER>(Cargo);

  if (pStructHolder && pStructHolder->hbsqlite3)
  {
    if (pStructHolder->hbsqlite3->cbAuthorizer)
    {
      hb_gcMark(pStructHolder->hbsqlite3->cbAuthorizer);
    }

    if (pStructHolder->hbsqlite3->cbBusyHandler)
    {
      hb_gcMark(pStructHolder->hbsqlite3->cbBusyHandler);
    }

    if (pStructHolder->hbsqlite3->cbProgressHandler)
    {
      hb_gcMark(pStructHolder->hbsqlite3->cbProgressHandler);
    }

    if (pStructHolder->hbsqlite3->cbHookCommit)
    {
      hb_gcMark(pStructHolder->hbsqlite3->cbHookCommit);
    }

    if (pStructHolder->hbsqlite3->cbHookRollback)
    {
      hb_gcMark(pStructHolder->hbsqlite3->cbHookRollback);
    }

    if (pStructHolder->hbsqlite3->cbFunc)
    {
      hb_gcMark(pStructHolder->hbsqlite3->cbFunc);
    }

#if SQLITE_VERSION_NUMBER >= 3014000
    if (pStructHolder->hbsqlite3->cbTraceHandler)
    {
      hb_gcMark(pStructHolder->hbsqlite3->cbTraceHandler);
    }
#else
    if (pStructHolder->hbsqlite3->sProfileFileName)
    {
      hb_gcMark(pStructHolder->hbsqlite3->sProfileFileName);
    }

    if (pStructHolder->hbsqlite3->sTraceFileName)
    {
      hb_gcMark(pStructHolder->hbsqlite3->sTraceFileName);
    }
#endif
  }
}

static const HB_GC_FUNCS s_gcSqlite3Funcs = {hb_sqlite3_destructor, hb_sqlite3_mark};

static PHB_ITEM hb_sqlite3_itemPut(PHB_ITEM pItem, void *pMemAddr, int iType)
{
  if (pItem != nullptr)
  {
    if (pItem->isComplex())
    {
      hb_itemClear(pItem);
    }
  }
  else
  {
    pItem = hb_itemNew(pItem);
  }

  auto pStructHolder = static_cast<PHB_SQLITE3_HOLDER>(hb_gcAllocate(sizeof(HB_SQLITE3_HOLDER), &s_gcSqlite3Funcs));
  pStructHolder->hbsqlite3 = static_cast<HB_SQLITE3 *>(pMemAddr);
  pStructHolder->type = iType;

  return hb_itemPutPtrGC(pItem, pStructHolder);
}

static void *hb_sqlite3_itemGet(PHB_ITEM pItem, int iType, bool fError)
{
  auto pStructHolder = static_cast<PHB_SQLITE3_HOLDER>(hb_itemGetPtrGC(pItem, &s_gcSqlite3Funcs));
  int iError = 0;

  HB_SYMBOL_UNUSED(iError);

  if (!pStructHolder)
  {
    iError = HB_ERR_MEMSTRU_NOT_MEM_BLOCK;
  }
  else if (pStructHolder->type != iType)
  {
    iError = HB_ERR_MEMSTRU_WRONG_MEMSTRU_BLOCK;
  }
  else if (!pStructHolder->hbsqlite3)
  {
    iError = HB_ERR_MEMSTRU_DESTROYED;
  }
  else
  {
    return pStructHolder->hbsqlite3;
  }

  if (fError)
  {
    hb_errRT_BASE_SubstR(EG_ARG, iError, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }

  return nullptr;
}

static void hb_sqlite3_ret(void *pMemAddr, int iType)
{
  hb_sqlite3_itemPut(hb_stackReturnItem(), pMemAddr, iType);
}

static void *hb_sqlite3_param(int iParam, int iType, bool fError)
{
  return hb_sqlite3_itemGet(hb_param(iParam, Harbour::Item::POINTER), iType, fError);
}

/**
   Callback helpers:
      Compile-Time Authorization Callback
      A Callback To Handle SQLITE_BUSY Errors
      Query Progress Callbacks
      Commit And Rollback Notification Callback
 */

static int callback(void *Cargo, int argc, char **argv, char **azColName)
{
  auto pCallback = static_cast<PHB_ITEM>(Cargo);

  if (pCallback && hb_vmRequestReenter())
  {
    auto pArrayValue = hb_itemArrayNew(argc);
    auto pArrayColName = hb_itemArrayNew(argc);

    for (auto i = 0; i < argc; i++)
    {
      hb_arraySetStrUTF8(pArrayValue, i + 1, static_cast<const char *>(argv[i] ? argv[i] : "NULL"));
      hb_arraySetStrUTF8(pArrayColName, i + 1, static_cast<const char *>(azColName[i]));
    }

    hb_vmPushEvalSym();
    hb_vmPush(pCallback);
    hb_vmPushInteger(argc);
    hb_vmPush(pArrayValue);
    hb_vmPush(pArrayColName);
    hb_vmSend(3);

    auto iRes = hb_parni(-1);

    hb_itemRelease(pArrayValue);
    hb_itemRelease(pArrayColName);

    hb_vmRequestRestore();

    return iRes;
  }

  return 0;
}

static int authorizer(void *Cargo, int iAction, const char *sName1, const char *sName2, const char *sName3,
                      const char *sName4)
{
  auto pCallback = static_cast<PHB_ITEM>(Cargo);

  if (pCallback && hb_vmRequestReenter())
  {
    auto pItem1 = hb_itemPutStrUTF8(nullptr, sName1);
    auto pItem2 = hb_itemPutStrUTF8(nullptr, sName2);
    auto pItem3 = hb_itemPutStrUTF8(nullptr, sName3);
    auto pItem4 = hb_itemPutStrUTF8(nullptr, sName4);

    hb_vmPushEvalSym();
    hb_vmPush(pCallback);
    hb_vmPushInteger(iAction);
    hb_vmPush(pItem1);
    hb_vmPush(pItem2);
    hb_vmPush(pItem3);
    hb_vmPush(pItem4);

    hb_vmSend(5);
    auto iRes = hb_parni(-1);

    hb_itemRelease(pItem1);
    hb_itemRelease(pItem2);
    hb_itemRelease(pItem3);
    hb_itemRelease(pItem4);

    hb_vmRequestRestore();

    return iRes;
  }

  return 0;
}

static int busy_handler(void *Cargo, int iNumberOfTimes)
{
  auto pCallback = static_cast<PHB_ITEM>(Cargo);

  if (pCallback && hb_vmRequestReenter())
  {
    hb_vmPushEvalSym();
    hb_vmPush(pCallback);
    hb_vmPushInteger(iNumberOfTimes);
    hb_vmSend(1);
    auto iRes = hb_parni(-1);
    hb_vmRequestRestore();
    return iRes;
  }

  return 0;
}

static int progress_handler(void *Cargo)
{
  auto pCallback = static_cast<PHB_ITEM>(Cargo);

  if (pCallback && hb_vmRequestReenter())
  {
    hb_vmPushEvalSym();
    hb_vmPush(pCallback);
    hb_vmSend(0);
    auto iRes = hb_parni(-1);
    hb_vmRequestRestore();
    return iRes;
  }

  return 0;
}

static int hook_commit(void *Cargo)
{
  auto pCallback = static_cast<PHB_ITEM>(Cargo);

  if (pCallback && hb_vmRequestReenter())
  {
    hb_vmPushEvalSym();
    hb_vmPush(pCallback);
    hb_vmSend(0);
    auto iRes = hb_parni(-1);
    hb_vmRequestRestore();
    return iRes;
  }

  return 0;
}

static void hook_rollback(void *Cargo)
{
  auto pCallback = static_cast<PHB_ITEM>(Cargo);

  if (pCallback && hb_vmRequestReenter())
  {
    hb_vmPushEvalSym();
    hb_vmPush(pCallback);
    hb_vmSend(0);
    hb_vmRequestRestore();
  }
}

static void func(sqlite3_context *ctx, int argc, sqlite3_value **argv)
{
  auto pCallback = static_cast<PHB_ITEM>(sqlite3_user_data(ctx));

  if (pCallback && hb_vmRequestReenter())
  {
    hb_vmPushEvalSym();
    hb_vmPush(pCallback);
    hb_vmPushInteger(argc);

    if (argc > 0)
    {
      for (auto i = 0; i < argc; i++)
      {
        switch (sqlite3_value_type(argv[i]))
        {
        case SQLITE_NULL:
          hb_vmPushNil();
          break;

        case SQLITE_TEXT:
          hb_itemPutStrUTF8(hb_stackAllocItem(), reinterpret_cast<const char *>(sqlite3_value_text(argv[i])));
          break;

        case SQLITE_FLOAT:
          hb_vmPushDouble(sqlite3_value_double(argv[i]), HB_DEFAULT_DECIMALS);
          break;

        case SQLITE_INTEGER:
#if HB_VMLONG_MAX == INT32_MAX || defined(HB_LONG_LONG_OFF)
          hb_vmPushInteger(sqlite3_value_int(argv[i]));
#else
          hb_vmPushNumInt(sqlite3_value_int64(argv[i]));
#endif
          break;

        case SQLITE_BLOB:
          hb_vmPushString(static_cast<const char *>(sqlite3_value_blob(argv[i])), sqlite3_value_bytes(argv[i]));
          break;

        default:
          hb_itemPutCConst(hb_stackAllocItem(), ":default:");
          break;
        }
      }
    }
    hb_vmSend(static_cast<HB_USHORT>(argc) + 1);

    auto pResult = hb_param(-1, Harbour::Item::ANY);

    switch (hb_itemType(pResult))
    {
    case Harbour::Item::NIL:
      sqlite3_result_null(ctx);
      break;

    case Harbour::Item::INTEGER:
    case Harbour::Item::LONG:
#if HB_VMLONG_MAX == INT32_MAX || defined(HB_LONG_LONG_OFF)
      sqlite3_result_int(ctx, hb_itemGetNI(pResult));
#else
      sqlite3_result_int64(ctx, hb_itemGetNInt(pResult));
#endif
      break;

    case Harbour::Item::DOUBLE:
      sqlite3_result_double(ctx, hb_itemGetND(pResult));
      break;

    case Harbour::Item::STRING:
    {
      void *hText;
      HB_SIZE nText;
      const char *pszText = hb_itemGetStrUTF8(pResult, &hText, &nText);
      sqlite3_result_text(ctx, pszText, static_cast<int>(nText), SQLITE_TRANSIENT);
      hb_strfree(hText);
      break;
    }

    default:
      sqlite3_result_error_code(ctx, -1);
      break;
    }

    hb_vmRequestRestore();
  }
}

/**
   sqlite3_libversion()         --> cVersion
   sqlite3_libversion_number()  --> nVersionNumber
   sqlite3_sourceid()           --> cSourceID

   Returns values equivalent to the header constants
   SQLITE_VERSION, SQLITE_VERSION_NUMBER, SQLITE_SOURCE_ID.
 */

HB_FUNC(SQLITE3_LIBVERSION)
{
  hb_retc(sqlite3_libversion());
}

HB_FUNC(SQLITE3_LIBVERSION_NUMBER)
{
  hb_retni(sqlite3_libversion_number());
}

HB_FUNC(SQLITE3_SOURCEID)
{
#if SQLITE_VERSION_NUMBER >= 3006018
  hb_retc(sqlite3_sourceid());
#else
  hb_retc_null();
#endif
}

/**
   sqlite3_initialize() --> nResult
   sqlite3_shutdown()   --> nResult

   The sqlite3_initialize() routine initializes the SQLite library.
   The sqlite3_shutdown() routine deallocates any resources that were
   allocated by sqlite3_initialize()
 */

HB_FUNC(SQLITE3_INITIALIZE)
{
#if SQLITE_VERSION_NUMBER >= 3006000
  hb_retni(sqlite3_initialize());
#else
  hb_retni(-1);
#endif
}

HB_FUNC(SQLITE3_SHUTDOWN)
{
#if SQLITE_VERSION_NUMBER >= 3006000
  hb_retni(sqlite3_shutdown());
#else
  hb_retni(-1);
#endif
}

/**
   Enable Or Disable Extended Result Codes

   sqlite3_extended_result_codes(db, lOnOff) --> nResultCode
 */

HB_FUNC(SQLITE3_EXTENDED_RESULT_CODES)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    hb_retni(sqlite3_extended_result_codes(pHbSqlite3->db, hb_parl(2)));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Error Codes And Messages

   sqlite3_errcode(db) --> returns the numeric result code or extended result code
   sqlite3_errmsg(db)  --> return English-language text that describes the error
 */

HB_FUNC(SQLITE3_ERRCODE)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    hb_retni(sqlite3_errcode(pHbSqlite3->db));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SQLITE3_EXTENDED_ERRCODE)
{
#if SQLITE_VERSION_NUMBER >= 3006005
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    hb_retni(sqlite3_extended_errcode(pHbSqlite3->db));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_retni(-1);
#endif
}

HB_FUNC(SQLITE3_ERRMSG)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    hb_retstr_utf8(sqlite3_errmsg(pHbSqlite3->db));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SQLITE3_ERRSTR)
{
#if SQLITE_VERSION_NUMBER >= 3007015
  hb_retstr_utf8(sqlite3_errstr(hb_parni(1)));
#else
  hb_retc_null();
#endif
}

/**
   Suspend Execution For A Short Time

   sqlite3_sleep(ms)
 */

HB_FUNC(SQLITE3_SLEEP)
{
  hb_retni(sqlite3_sleep(hb_parni(1)));
}

/**
   Last Insert Rowid

   sqlite3_last_insert_rowid(db) --> nROWID
 */

HB_FUNC(SQLITE3_LAST_INSERT_ROWID)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    hb_retnint(sqlite3_last_insert_rowid(pHbSqlite3->db));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Name Of The Directory Holding Temporary Files

   sqlite3_temp_directory(cDirName) --> lResult
 */

HB_FUNC(SQLITE3_TEMP_DIRECTORY)
{
  bool bResult = false;

#ifdef SQLITE3_DYNLIB
  {
    char *pszFree;
    auto pszDirName = hb_fsNameConv(hb_parcx(1), &pszFree);

    if (hb_fsIsDirectory(pszDirName))
    {
      bResult = true;
    }
    else
    {
      if (hb_parl(2))
      { /* create temp directory if not exist */
        if (hb_fsMkDir(pszDirName))
        {
          bResult = true;
        }
        else
        {
#if 0
               HB_TRACE(HB_TR_DEBUG, ("sqlite_temp_directory(): Could not create directory %s", pszDirName));
#endif
        }
      }
      else
      {
#if 0
            HB_TRACE(HB_TR_DEBUG, ("sqlite_temp_directory(): Directory does not exist %s", pszDirName));
#endif
      }
    }

    if (bResult)
    {
      sqlite3_temp_directory = hb_strdup(pszDirName);
    }

    if (pszFree)
    {
      hb_xfree(pszFree);
    }
  }
#endif // SQLITE3_DYNLIB
  hb_retl(bResult);
}

/**
   Opening(creating) A New Database Connection

   sqlite3_open(cDatabace, lCreateIfNotExist) --> return pointer to Db or NIL if error occurs
   sqlite3_open_v2(cDatabace, nOpenMode)      --> return pHbSqlite3 or NIL
 */

HB_FUNC(SQLITE3_OPEN)
{
  sqlite3 *db;
  char *pszFree;
  auto pszdbName = hb_fsNameConv(hb_parcx(1), &pszFree);

  if (hb_fsFileExists(pszdbName) || hb_parl(2))
  {
    if (sqlite3_open(pszdbName, &db) == SQLITE_OK)
    {
      auto hbsqlite3 = static_cast<HB_SQLITE3 *>(hb_xgrabz(sizeof(HB_SQLITE3)));
      hbsqlite3->db = db;
      hb_sqlite3_ret(hbsqlite3, HB_SQLITE3_DB);
    }
    else
    {
      sqlite3_close(db);
      hb_retptr(nullptr);
    }
  }
  else
  {
#if 0
      HB_TRACE(HB_TR_DEBUG, ("sqlite3_open(): Database does not exist %s", pszdbName));
#endif

    hb_retptr(nullptr);
  }

  if (pszFree)
  {
    hb_xfree(pszFree);
  }
}

HB_FUNC(SQLITE3_OPEN_V2)
{
#if SQLITE_VERSION_NUMBER >= 3005000
  sqlite3 *db;
  char *pszFree;
  auto pszdbName = hb_fsNameConv(hb_parcx(1), &pszFree);

  if (sqlite3_open_v2(pszdbName, &db, hb_parni(2), nullptr) == SQLITE_OK)
  {
    auto hbsqlite3 = static_cast<HB_SQLITE3 *>(hb_xgrabz(sizeof(HB_SQLITE3)));
    hbsqlite3->db = db;
    hb_sqlite3_ret(hbsqlite3, HB_SQLITE3_DB);
  }
  else
  {
    sqlite3_close(db);
    hb_retptr(nullptr);
  }

  if (pszFree)
  {
    hb_xfree(pszFree);
  }
#else
  hb_retptr(nullptr);
#endif
}

/**
   One-Step Query Execution Interface

   sqlite3_exec(db, cSQLTEXT, [pCallbackFunc | cCallbackFunc]) --> nResultCode
 */

HB_FUNC(SQLITE3_EXEC)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    void *hSQLText;
    char *pszErrMsg = nullptr;
    int rc;

    if (HB_ISEVALITEM(3))
    {
      rc = sqlite3_exec(pHbSqlite3->db, hb_parstr_utf8(2, &hSQLText, nullptr), callback,
                        static_cast<void *>(hb_param(3, Harbour::Item::EVALITEM)), &pszErrMsg);
    }
    else
    {
      rc = sqlite3_exec(pHbSqlite3->db, hb_parstr_utf8(2, &hSQLText, nullptr), nullptr, 0, &pszErrMsg);
    }

    if (rc != SQLITE_OK)
    {
#if 0
         HB_TRACE(HB_TR_DEBUG, ("sqlite3_exec(): Returned error: %s", pszErrMsg));
#endif
      sqlite3_free(pszErrMsg);
    }

    hb_strfree(hSQLText);

    hb_retni(rc);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Compiling An SQL Statement

   sqlite3_prepare(db, cSQLTEXT, [nPrepFlags])
   --> return pointer to compiled statement or NIL if error occurs

   TODO: pszTail?
 */

HB_FUNC(SQLITE3_PREPARE)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    void *hSQLText;
    HB_SIZE nSQLText;

    const char *pszSQLText = hb_parstr_utf8(2, &hSQLText, &nSQLText);
    psqlite3_stmt pStmt;
    const char *pszTail;
    int result;

#if SQLITE_VERSION_NUMBER >= 3020000
    result = sqlite3_prepare_v3(pHbSqlite3->db, pszSQLText, static_cast<int>(nSQLText),
                                static_cast<unsigned int>(hb_parnl(3)), &pStmt, &pszTail);
#else
    result = sqlite3_prepare_v2(pHbSqlite3->db, pszSQLText, static_cast<int>(nSQLText), &pStmt, &pszTail);
#endif

    if (result == SQLITE_OK)
    {
      hb_retptr(pStmt);
    }
    else
    {
      sqlite3_finalize(pStmt);
      hb_retptr(nullptr);
    }

    hb_strfree(hSQLText);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Determine If An SQL Statement Is Complete

   sqlite3_complete(sqlText) --> lResult
 */

HB_FUNC(SQLITE3_COMPLETE)
{
  if (HB_ISCHAR(1))
  {
    void *hSQLText;
    hb_retl(sqlite3_complete(hb_parstr_utf8(1, &hSQLText, nullptr)));
    hb_strfree(hSQLText);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   This interface can be used to retrieve a saved copy of the original SQL text
   used to create a prepared statement
   if that statement was compiled using either sqlite3_prepare()

   sqlite3_sql(pStmt) --> cSQLTEXT
 */

HB_FUNC(SQLITE3_SQL)
{
/* FIXME: verify the exact SQLITE3 version */
#if SQLITE_VERSION_NUMBER > 3004001
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retstr_utf8(sqlite3_sql(pStmt));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_retc_null();
#endif
}

/**
   Prepared Statement Status.

   sqlite3_stmt_status(pStmt, nOp, lResetFlag) --> nStatus
 */

HB_FUNC(SQLITE3_STMT_STATUS)
{
#if SQLITE_VERSION_NUMBER >= 3006004
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_stmt_status(pStmt, hb_parni(2), static_cast<int>(hb_parl(3))));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_retni(-1);
#endif
}

/**
   sqlite3_stmt_readonly(pStmt) --> lResult

   Determine If An SQL Statement Writes The Database
 */
HB_FUNC(SQLITE3_STMT_READONLY)
{
#if SQLITE_VERSION_NUMBER >= 3007004
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retl(static_cast<HB_BOOL>(sqlite3_stmt_readonly(pStmt)));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_retni(-1);
#endif
}

/**
   Find The Database Handle Associated With A Prepared Statement

   sqlite3_db_handle(pStmt) --> pHbSqlite3
 */

#if 0
HB_FUNC(SQLITE3_DB_HANDLE)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if( pStmt != nullptr )
  {
    /* ... */
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}
#endif

/**
   Evaluate An Prepared SQL Statement

   sqlite3_step(pStmt) --> nResultCode
 */

HB_FUNC(SQLITE3_STEP)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_step(pStmt));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Reset All Bindings On A Prepared Statement

   sqlite3_clear_bindings(pStmt) --> nResultCode

   Contrary to the intuition of many,
   sqlite3_reset() does not reset the bindings on a prepared statement.
   Use this routine to reset all host parameters to NULL.
 */

HB_FUNC(SQLITE3_CLEAR_BINDINGS)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_clear_bindings(pStmt));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Reset A Prepared Statement Object

   sqlite3_reset(pStmt) --> nResultCode
 */

HB_FUNC(SQLITE3_RESET)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_reset(pStmt));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Finalize A Prepared Statement Object

   sqlite3_finalize(pStmt) --> nResultCode
 */

HB_FUNC(SQLITE3_FINALIZE)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_finalize(pStmt));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/*
   int sqlite3_bind_blob(sqlite3_stmt*, int, const void*, int n, void(*)(void*));
   int sqlite3_bind_double(sqlite3_stmt*, int, double);
   int sqlite3_bind_int(sqlite3_stmt*, int, int);
   int sqlite3_bind_int64(sqlite3_stmt*, int, sqlite3_int64);
   int sqlite3_bind_null(sqlite3_stmt*, int);
   int sqlite3_bind_text(sqlite3_stmt*, int, const char*, int n, void(*)(void*));
   int sqlite3_bind_value(sqlite3_stmt*, int, const sqlite3_value*);
   int sqlite3_bind_zeroblob(sqlite3_stmt*, int, int n)
 */

/**
   Binding Values To Prepared Statements

   These routines return SQLITE_OK on success or an error code if anything
   goes wrong.
   SQLITE_RANGE is returned if the parameter index is out of range.
   SQLITE_NOMEM is returned if malloc fails.
   SQLITE_MISUSE is returned if these routines are called on a virtual machine
   that is the wrong state or which has already been finalized.
 */

HB_FUNC(SQLITE3_BIND_BLOB)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_bind_blob(pStmt, hb_parni(2), hb_parcx(3), static_cast<int>(hb_parcsiz(3)) - 1, SQLITE_TRANSIENT));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SQLITE3_BIND_DOUBLE)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_bind_double(pStmt, hb_parni(2), hb_parnd(3)));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SQLITE3_BIND_INT)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_bind_int(pStmt, hb_parni(2), hb_parni(3)));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SQLITE3_BIND_INT64)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));
  sqlite3_int64 int64 = hb_parnint(3);

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_bind_int64(pStmt, hb_parni(2), int64));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SQLITE3_BIND_NULL)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_bind_null(pStmt, hb_parni(2)));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SQLITE3_BIND_TEXT)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    void *hSQLText;
    HB_SIZE nSQLText;
    const char *pszSQLText = hb_parstr_utf8(3, &hSQLText, &nSQLText);
    hb_retni(sqlite3_bind_text(pStmt, hb_parni(2), pszSQLText, static_cast<int>(nSQLText), SQLITE_TRANSIENT));
    hb_strfree(hSQLText);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SQLITE3_BIND_ZEROBLOB)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_bind_zeroblob(pStmt, hb_parni(2), hb_parni(3)));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Number Of Host Parameters

   sqlite3_bind_parameter_count(pStmt) --> nResult
 */

HB_FUNC(SQLITE3_BIND_PARAMETER_COUNT)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_bind_parameter_count(pStmt));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Index Of A Parameter With A Given Name

   sqlite3_bind_parameter_index(pStmt, cParameterName) --> nResult
 */

HB_FUNC(SQLITE3_BIND_PARAMETER_INDEX)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    void *hParameterName;
    hb_retni(sqlite3_bind_parameter_index(pStmt, hb_parstr_utf8(2, &hParameterName, nullptr)));
    hb_strfree(hParameterName);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Name Of A Host Parameter

   sqlite3_bind_parameter_name(pStmt, nParameterIndex) --> cParameterName
 */

HB_FUNC(SQLITE3_BIND_PARAMETER_NAME)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retstr_utf8(sqlite3_bind_parameter_name(pStmt, hb_parni(2)));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Count The Number Of Rows Modified

   sqlite3_changes(db) --> nRowCount
 */

HB_FUNC(SQLITE3_CHANGES)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    hb_retni(sqlite3_changes(pHbSqlite3->db));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Total Number Of Rows Modified

   sqlite3_total_changes(db) --> nRowCount
 */

HB_FUNC(SQLITE3_TOTAL_CHANGES)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    hb_retni(sqlite3_total_changes(pHbSqlite3->db));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Number Of Columns In A Result Set

   sqlite3_column_count(pStmt) --> nColumnCount
 */

HB_FUNC(SQLITE3_COLUMN_COUNT)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_column_count(pStmt));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   sqlite3_column_type(pStmt, nIndex) --> nColumnType
   nColumnType is Datatype code for the initial data type of the result column

   SQLITE_INTEGER      1
   SQLITE_FLOAT        2
   SQLITE_TEXT         3
   SQLITE3_TEXT        3
   SQLITE_BLOB         4
   SQLITE_NULL         5

   Declared Datatype Of A Query Result (see doc)
   sqlite3_column_decltype(pStmt, nIndex) --> nColumnDeclType
 */

HB_FUNC(SQLITE3_COLUMN_TYPE)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_column_type(pStmt, hb_parni(2) - 1));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SQLITE3_COLUMN_DECLTYPE)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retstr_utf8(sqlite3_column_decltype(pStmt, hb_parni(2) - 1));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Column Names In A Result Set

   sqlite3_column_name(pStmt, columnIndex) --> columnName
 */

HB_FUNC(SQLITE3_COLUMN_NAME)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retstr_utf8(sqlite3_column_name(pStmt, hb_parni(2) - 1));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   sqlite3_column_bytes(pStmt, columnIndex)
   --> returns the number of bytes in that BLOB or string

   Results Values From A Query

   sqlite3_column_blob(pStmt, columnIndex)   --> value as BLOB
   sqlite3_column_double(pStmt, columnIndex) --> value as double
   sqlite3_column_int(pStmt, columnIndex)    --> value as integer
   sqlite3_column_int64(pStmt, columnIndex)  --> value as long long
   sqlite3_column_text(pStmt, columnIndex)   --> value as text
 */

HB_FUNC(SQLITE3_COLUMN_BYTES)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_column_bytes(pStmt, hb_parni(2) - 1));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SQLITE3_COLUMN_BLOB)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    int iIndex = hb_parni(2) - 1;
    hb_retclen(static_cast<const char *>(sqlite3_column_blob(pStmt, iIndex)), sqlite3_column_bytes(pStmt, iIndex));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SQLITE3_COLUMN_DOUBLE)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retnd(sqlite3_column_double(pStmt, hb_parni(2) - 1));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SQLITE3_COLUMN_INT)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retni(sqlite3_column_int(pStmt, hb_parni(2) - 1));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SQLITE3_COLUMN_INT64)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retnint(sqlite3_column_int64(pStmt, hb_parni(2) - 1));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SQLITE3_COLUMN_TEXT)
{
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    int iIndex = hb_parni(2) - 1;
    hb_retstrlen_utf8(reinterpret_cast<const char *>(sqlite3_column_text(pStmt, iIndex)),
                      sqlite3_column_bytes(pStmt, iIndex));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(SQLITE3_LOAD_EXTENSION)
{
#ifndef SQLITE_OMIT_LOAD_EXTENSION
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    char *zErrMsg = nullptr;
    hb_retni(sqlite3_load_extension(pHbSqlite3->db, hb_parcx(2), hb_parc(3), &zErrMsg));
    hb_storc(zErrMsg, 4);
    sqlite3_free(zErrMsg);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_retni(-1);
#endif // SQLITE_OMIT_LOAD_EXTENSION
}

/**
   Enable Or Disable Extension Loading

   sqlite3_enable_load_extension(db, lOnOff) --> prev.state
 */

HB_FUNC(SQLITE3_ENABLE_LOAD_EXTENSION)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
#ifndef SQLITE_OMIT_LOAD_EXTENSION
    hb_retni(sqlite3_enable_load_extension(pHbSqlite3->db, hb_parl(2)));
#else
    hb_retni(-1);
#endif // SQLITE_OMIT_LOAD_EXTENSION
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Reset Automatic Extension Loading

   sqlite3_reset_auto_extension()
 */

HB_FUNC(SQLITE3_RESET_AUTO_EXTENSION)
{
  sqlite3_reset_auto_extension();
}

/**
   Set A Busy Timeout

   sqlite3_busy_timeout(db, ms)
 */

HB_FUNC(SQLITE3_BUSY_TIMEOUT)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    hb_retni(sqlite3_busy_timeout(pHbSqlite3->db, hb_parni(2)));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Convenience Routines For Running Queries

   sqlite3_get_table(db, sqlText) --> aResult
 */

HB_FUNC(SQLITE3_GET_TABLE)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    void *hSQLText;
    auto pResultList = hb_itemArrayNew(0);
    int iRow, iCol;
    char *pszErrMsg = nullptr;
    char **pResult;

    if (sqlite3_get_table(pHbSqlite3->db, hb_parstr_utf8(2, &hSQLText, nullptr), &pResult, &iRow, &iCol, &pszErrMsg) ==
        SQLITE_OK)
    {
      int k = 0;

      for (auto i = 0; i < iRow + 1; i++)
      {
        auto pArray = hb_itemArrayNew(iCol);

        for (auto j = 1; j <= iCol; j++, k++)
        {
          hb_arraySetStrUTF8(pArray, j, static_cast<const char *>(pResult[k]));
        }

        hb_arrayAddForward(pResultList, pArray);
        hb_itemRelease(pArray);
      }
    }
    else
    {
#if 0
         HB_TRACE(HB_TR_DEBUG, ("sqlite3_get_table(): Returned error: %s", pszErrMsg));
#endif
      sqlite3_free(pszErrMsg);
    }

    sqlite3_free_table(pResult);
    hb_strfree(hSQLText);
    hb_itemReturnRelease(pResultList);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Extract Metadata About A Column Of A Table
   based on
   int sqlite3_table_column_metadata(
     sqlite3 *db,                - IN:  Connection handle
     const char *zDbName,        - IN:  Database name or nullptr
     const char *zTableName,     - IN:  Table name
     const char *zColumnName,    - IN:  Column name
     char const **pzDataType,    - OUT: Declared data type
     char const **pzCollSeq,     - OUT: Collation sequence name
     int *pNotNull,              - OUT: True if NOT nullptr constraint exists
     int *pPrimaryKey,           - OUT: True if column part of PK
     int *pAutoinc               - OUT: True if column is auto-increment
   );
 */

HB_FUNC(SQLITE3_TABLE_COLUMN_METADATA)
{
#ifdef SQLITE_ENABLE_COLUMN_METADATA
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    char const *pzDataType = nullptr;
    char const *pzCollSeq = nullptr;
    int iNotNull = 0;
    int iPrimaryKey = 0;
    int iAutoinc = 0;

    void *hDbName;
    void *hTableName;
    void *hColumnName;

    if (sqlite3_table_column_metadata(pHbSqlite3->db, hb_parstr_utf8(2, &hDbName, nullptr),
                                      hb_parstr_utf8(3, &hTableName, nullptr), hb_parstr_utf8(4, &hColumnName, nullptr),
                                      &pzDataType /* pzDataDtype */, &pzCollSeq /* pzCollSeq */, &iNotNull,
                                      &iPrimaryKey, &iAutoinc) == SQLITE_OK)
    {
      auto pArray = hb_itemArrayNew(5);

      hb_arraySetStrUTF8(pArray, 1, pzDataType);
      hb_arraySetStrUTF8(pArray, 2, pzCollSeq);
      hb_arraySetL(pArray, 3, static_cast<HB_BOOL>(iNotNull != 0));
      hb_arraySetL(pArray, 4, static_cast<HB_BOOL>(iPrimaryKey != 0));
      hb_arraySetL(pArray, 5, static_cast<HB_BOOL>(iAutoinc != 0));

      hb_itemReturnRelease(pArray);
    }

    hb_strfree(hDbName);
    hb_strfree(hTableName);
    hb_strfree(hColumnName);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_reta(0);
#endif // SQLITE_ENABLE_COLUMN_METADATA
}

/**
   Source Of Data In A Query Result

   sqlite3_column_database_name(pStmt, ColumnIndex) --> cDatabaseName
   sqlite3_column_table_name(pStmt, ColumnIndex)    --> cTableName
   sqlite3_column_origin_name(pStmt, ColumnIndex)   --> cColumnName
 */

HB_FUNC(SQLITE3_COLUMN_DATABASE_NAME)
{
#ifdef SQLITE_ENABLE_COLUMN_METADATA
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retstr_utf8(sqlite3_column_database_name(pStmt, hb_parni(2) - 1));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_retc_null();
#endif // SQLITE_ENABLE_COLUMN_METADATA
}

HB_FUNC(SQLITE3_COLUMN_TABLE_NAME)
{
#ifdef SQLITE_ENABLE_COLUMN_METADATA
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retstr_utf8(sqlite3_column_table_name(pStmt, hb_parni(2) - 1));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_retc_null();
#endif // SQLITE_ENABLE_COLUMN_METADATA
}

HB_FUNC(SQLITE3_COLUMN_ORIGIN_NAME)
{
#ifdef SQLITE_ENABLE_COLUMN_METADATA
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));

  if (pStmt != nullptr)
  {
    hb_retstr_utf8(sqlite3_column_origin_name(pStmt, hb_parni(2) - 1));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_retc_null();
#endif // SQLITE_ENABLE_COLUMN_METADATA
}

/*
   BLOB I/O
 */

/**
   Open A BLOB For Incremental I/O

   Open a handle to the blob located in row iRow, column zColumn, table zTable
   in database zDb. i.e. the same blob that would be selected by:

   SELECT zColumn FROM zDb.zTable WHERE rowid = iRow;
 */

HB_FUNC(SQLITE3_BLOB_OPEN)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    sqlite3_blob *ppBlob = nullptr;

    void *hDbName;
    void *hTableName;
    void *hColumnName;

    if (sqlite3_blob_open(pHbSqlite3->db, hb_parstr_utf8(2, &hDbName, nullptr), hb_parstr_utf8(3, &hTableName, nullptr),
                          hb_parstr_utf8(4, &hColumnName, nullptr),
                          static_cast<sqlite3_int64>(hb_parnint(5)) /* iRow */, hb_parni(6) /* flags */,
                          &ppBlob) == SQLITE_OK)
    {
      hb_retptr(ppBlob);
    }
    else
    {
      hb_retptr(nullptr);
    }

    hb_strfree(hDbName);
    hb_strfree(hTableName);
    hb_strfree(hColumnName);
  }
  else
  {
    hb_retptr(nullptr);
  }
}

/**
   Move a BLOB Handle to a New Row
 */

HB_FUNC(SQLITE3_BLOB_REOPEN)
{
#if SQLITE_VERSION_NUMBER >= 3007004
  auto pBlob = static_cast<sqlite3_blob *>(hb_parptr(1));

  if (pBlob != nullptr)
  {
    hb_retni(sqlite3_blob_reopen(pBlob, hb_parnint(2)));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_retni(-1);
#endif
}

/**
   Close A BLOB Handle
 */

HB_FUNC(SQLITE3_BLOB_CLOSE)
{
  auto pBlob = static_cast<sqlite3_blob *>(hb_parptr(1));

  if (pBlob != nullptr)
  {
    hb_retni(sqlite3_blob_close(pBlob));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Return The Size Of An Open BLOB
 */

HB_FUNC(SQLITE3_BLOB_BYTES)
{
  auto pBlob = static_cast<sqlite3_blob *>(hb_parptr(1));

  if (pBlob != nullptr)
  {
    hb_retni(sqlite3_blob_bytes(pBlob));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Read Data From A BLOB Incrementally
 */

HB_FUNC(SQLITE3_BLOB_READ)
{
  auto pBlob = static_cast<sqlite3_blob *>(hb_parptr(1));

  if (pBlob != nullptr)
  {
    auto iLen = hb_parni(2);

    if (iLen == 0)
    {
      iLen = sqlite3_blob_bytes(pBlob);
    }

    auto buffer = static_cast<char *>(hb_xgrab(iLen + 1));

    if (SQLITE_OK == sqlite3_blob_read(pBlob, static_cast<void *>(buffer), iLen, hb_parni(3)))
    {
      hb_retclen_buffer(buffer, iLen);
    }
    else
    {
      hb_xfree(buffer);
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Write Data Into A BLOB Incrementally
 */

HB_FUNC(SQLITE3_BLOB_WRITE)
{
  auto pBlob = static_cast<sqlite3_blob *>(hb_parptr(1));

  if (pBlob != nullptr)
  {
    auto iLen = hb_parni(3);

    if (iLen == 0)
    {
      iLen = static_cast<int>(hb_parcsiz(2)) - 1;
    }

    hb_retni(sqlite3_blob_write(pBlob, hb_parcx(2), iLen, hb_parni(4)));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
    Test To See If The Database Is In Auto-Commit Mode

    sqlite3_get_autocommit(db) --> lResult
 */

HB_FUNC(SQLITE3_GET_AUTOCOMMIT)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    hb_retl(sqlite3_get_autocommit(pHbSqlite3->db));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/**
   Enable Or Disable Shared Pager Cache

   sqlite3_enable_shared_cache(lOnOff) --> nResultCode
 */

HB_FUNC(SQLITE3_ENABLE_SHARED_CACHE)
{
  hb_retni(sqlite3_enable_shared_cache(hb_parl(1)));
}

/**
   Tracing And Profiling Functions

   sqlite3_trace_v2( db, nMask, [ bCallback ] ) // Starting with 3.14.0

   sqlite3_trace( db, lOnOff, [ filename ] )     // Deprecated in 3.14.0
   sqlite3_profile( db, lOnOff, [ filename ] )   // Deprecated in 3.14.0
 */

#if SQLITE_VERSION_NUMBER >= 3014000
static int trace_handler(unsigned uType, void *cbTraceHandler, void *p, void *x)
{
  PHB_ITEM pCallback = static_cast<PHB_ITEM>(cbTraceHandler);
  int iRes = 0;

  if (pCallback && hb_vmRequestReenter())
  {
    hb_vmPushEvalSym();
    hb_vmPush(pCallback);
    hb_vmPushNumInt(uType);
    switch (uType)
    {
    case SQLITE_TRACE_STMT:
      hb_vmPushPointer(p);
      hb_vmPushString(reinterpret_cast<const char *>(x), strlen(reinterpret_cast<char *>(x)));
      hb_vmSend(3);
      break;
    case SQLITE_TRACE_PROFILE:
      hb_vmPushPointer(p);
      hb_vmPushNumInt(*(sqlite3_uint64 *)x);
      hb_vmSend(3);
      break;
    case SQLITE_TRACE_ROW:
      HB_SYMBOL_UNUSED(x);
      hb_vmPushPointer(p);
      hb_vmSend(2);
      break;
    case SQLITE_TRACE_CLOSE:
    {
      PHB_ITEM pItem = hb_itemNew(nullptr);
      HB_SQLITE3 *hbsqlite3 = static_cast<HB_SQLITE3 *>(hb_xgrabz(sizeof(HB_SQLITE3)));
      HB_SYMBOL_UNUSED(x);

      hbsqlite3->db = static_cast<sqlite3 *>(p);
      hb_sqlite3_itemPut(pItem, hbsqlite3, HB_SQLITE3_DB);
      hb_vmPush(pItem);
      hb_vmSend(2);

      /* We don't want sqlite3_close() called recursively
       * and don't want to implement a weak reference engine yet
       * so we just clear the pointer before hb_itemRelease(). */
      hbsqlite3->db = nullptr;
      hb_itemRelease(pItem);
      break;
    }
    }
    iRes = hb_parni(-1);
    hb_vmRequestRestore();
  }
  return iRes;
}
#endif

HB_FUNC(SQLITE3_TRACE_V2)
{
#if SQLITE_VERSION_NUMBER >= 3014000
  HB_SQLITE3 *pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    unsigned uMask = static_cast<unsigned int>(hb_parnint(2));
    int iRes;

    if (pHbSqlite3->cbTraceHandler)
    {
      hb_itemRelease(pHbSqlite3->cbTraceHandler);
      pHbSqlite3->cbTraceHandler = nullptr;
    }

    if (HB_ISEVALITEM(3))
    {
      pHbSqlite3->cbTraceHandler = hb_itemNew(hb_param(3, Harbour::Item::EVALITEM));
      hb_gcUnlock(pHbSqlite3->cbTraceHandler);

      iRes = sqlite3_trace_v2(pHbSqlite3->db, uMask, uMask ? trace_handler : nullptr,
                              uMask ? (void *)pHbSqlite3->cbTraceHandler : nullptr);
    }
    else
    {
      iRes = sqlite3_trace_v2(pHbSqlite3->db, 0, nullptr, nullptr);
    }
    hb_retni(iRes);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_errRT_BASE_SubstR(EG_UNSUPPORTED, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
#endif
}

#if SQLITE_VERSION_NUMBER < 3014000
static void SQL3ProfileLog(void *sFile, const char *sProfileMsg, sqlite3_uint64 uint64)
{
  if (sProfileMsg)
  {
    auto hFile = hb_fopen(sFile ? static_cast<const char *>(sFile) : "hbsq3_pr.log", "a");

    if (hFile)
    {
      fprintf(hFile, "%s - %" PFLL "u\n", sProfileMsg, uint64);
      fclose(hFile);
    }
  }
}

static void SQL3TraceLog(void *sFile, const char *sTraceMsg)
{
  if (sTraceMsg)
  {
    auto hFile = hb_fopen(sFile ? static_cast<const char *>(sFile) : "hbsq3_tr.log", "a");

    if (hFile)
    {
      fprintf(hFile, "%s\n", sTraceMsg);
      fclose(hFile);
    }
  }
}
#endif

HB_FUNC(SQLITE3_PROFILE)
{
#if SQLITE_VERSION_NUMBER < 3014000
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    bool bFlag = hb_parl(2);
    if (pHbSqlite3->sProfileFileName)
    {
      hb_itemRelease(pHbSqlite3->sProfileFileName);
      pHbSqlite3->sProfileFileName = nullptr;
    }
    if (bFlag && HB_ISCHAR(3))
    {
      pHbSqlite3->sProfileFileName = hb_itemNew(hb_param(3, Harbour::Item::STRING));
      hb_gcUnlock(pHbSqlite3->sProfileFileName);
    }

    sqlite3_profile(pHbSqlite3->db, bFlag ? SQL3ProfileLog : nullptr,
                    pHbSqlite3->sProfileFileName ? const_cast<char *>(hb_itemGetCPtr(pHbSqlite3->sProfileFileName))
                                                 : nullptr);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_errRT_BASE_SubstR(EG_UNSUPPORTED, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
#endif
}

HB_FUNC(SQLITE3_TRACE)
{
#if SQLITE_VERSION_NUMBER < 3014000
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    bool bFlag = hb_parl(2);
    if (pHbSqlite3->sTraceFileName)
    {
      hb_itemRelease(pHbSqlite3->sTraceFileName);
      pHbSqlite3->sTraceFileName = nullptr;
    }
    if (bFlag && HB_ISCHAR(3))
    {
      pHbSqlite3->sTraceFileName = hb_itemNew(hb_param(3, Harbour::Item::STRING));
      hb_gcUnlock(pHbSqlite3->sTraceFileName);
    }

    sqlite3_trace(pHbSqlite3->db, bFlag ? SQL3TraceLog : nullptr,
                  pHbSqlite3->sTraceFileName ? const_cast<char *>(hb_itemGetCPtr(pHbSqlite3->sTraceFileName))
                                             : nullptr);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_errRT_BASE_SubstR(EG_UNSUPPORTED, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
#endif
}

/**
   BLOB Import/export
 */

HB_FUNC(SQLITE3_FILE_TO_BUFF)
{
  HB_SIZE nSize;
  char *pBuffer = reinterpret_cast<char *>(hb_fileLoad(hb_parcx(1), 0, &nSize));

  if (pBuffer)
  {
    hb_retclen_buffer(pBuffer, nSize);
  }
  else
  {
    hb_retc_null();
  }
}

HB_FUNC(SQLITE3_BUFF_TO_FILE)
{
  if (HB_ISCHAR(1))
  {
    PHB_FILE handle = hb_fileExtOpen(
        hb_parc(1), nullptr, FO_WRITE | FO_EXCLUSIVE | FO_PRIVATE | FXO_TRUNCATE | FXO_SHARELOCK | FXO_NOSEEKPOS,
        nullptr, nullptr);

    if (handle)
    {
      HB_SIZE nSize = hb_parclen(2);
      hb_retns(hb_fileWrite(handle, hb_parcx(2), nSize, -1) == nSize ? 0 : -1);
      hb_fileClose(handle);
      return;
    }
  }

  hb_retns(-1);
}

/**
   Causes any pending database operation to abort and return at its
   earliest opportunity.

   sqlite3_interrupt(db) --> NIL
 */

HB_FUNC(SQLITE3_INTERRUPT)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    sqlite3_interrupt(pHbSqlite3->db);
  }
}

/**
   A Callback To Handle SQLITE_BUSY Errors

   sqlite3_busy_handler(db, nNumOfOpCodes, [cFunc|sFunc])
 */

HB_FUNC(SQLITE3_BUSY_HANDLER)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    if (pHbSqlite3->cbBusyHandler)
    {
      hb_itemRelease(pHbSqlite3->cbBusyHandler);
      pHbSqlite3->cbBusyHandler = nullptr;
    }

    if (HB_ISEVALITEM(2))
    {
      pHbSqlite3->cbBusyHandler = hb_itemNew(hb_param(2, Harbour::Item::EVALITEM));
      hb_gcUnlock(pHbSqlite3->cbBusyHandler);

      sqlite3_busy_handler(pHbSqlite3->db, busy_handler, static_cast<void *>(pHbSqlite3->cbBusyHandler));
    }
    else
    {
      sqlite3_busy_handler(pHbSqlite3->db, nullptr, nullptr);
    }
  }
}

/**
   Query Progress Callbacks

   sqlite3_progress_handler(db, nNumOfOpCodes, [cFunc|sFunc])
 */

HB_FUNC(SQLITE3_PROGRESS_HANDLER)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    if (pHbSqlite3->cbProgressHandler)
    {
      hb_itemRelease(pHbSqlite3->cbProgressHandler);
      pHbSqlite3->cbProgressHandler = nullptr;
    }

    if (HB_ISNUM(2) && HB_ISEVALITEM(3))
    {
      pHbSqlite3->cbProgressHandler = hb_itemNew(hb_param(3, Harbour::Item::EVALITEM));
      hb_gcUnlock(pHbSqlite3->cbProgressHandler);

      sqlite3_progress_handler(pHbSqlite3->db, hb_parni(2), progress_handler,
                               static_cast<void *>(pHbSqlite3->cbProgressHandler));
    }
    else
    {
      sqlite3_progress_handler(pHbSqlite3->db, 0, nullptr, nullptr);
    }
  }
}

/**
   Commit And Rollback Notification Callbacks

   sqlite3_commit_hook(db, [cFunc|sFunc])
   sqlite3_rollback_hook(db, [cFunc|sFunc])
 */

HB_FUNC(SQLITE3_COMMIT_HOOK)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    if (pHbSqlite3->cbHookCommit)
    {
      hb_itemRelease(pHbSqlite3->cbHookCommit);
      pHbSqlite3->cbHookCommit = nullptr;
    }

    if (HB_ISEVALITEM(2))
    {
      pHbSqlite3->cbHookCommit = hb_itemNew(hb_param(2, Harbour::Item::EVALITEM));
      hb_gcUnlock(pHbSqlite3->cbHookCommit);

      sqlite3_commit_hook(pHbSqlite3->db, hook_commit, static_cast<void *>(pHbSqlite3->cbHookCommit));
    }
    else
    {
      sqlite3_commit_hook(pHbSqlite3->db, nullptr, nullptr);
    }
  }
}

HB_FUNC(SQLITE3_ROLLBACK_HOOK)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    if (pHbSqlite3->cbHookRollback)
    {
      hb_itemRelease(pHbSqlite3->cbHookRollback);
      pHbSqlite3->cbHookRollback = nullptr;
    }

    if (HB_ISEVALITEM(2))
    {
      pHbSqlite3->cbHookRollback = hb_itemNew(hb_param(2, Harbour::Item::EVALITEM));
      hb_gcUnlock(pHbSqlite3->cbHookRollback);

      sqlite3_rollback_hook(pHbSqlite3->db, hook_rollback, static_cast<void *>(pHbSqlite3->cbHookRollback));
    }
    else
    {
      sqlite3_rollback_hook(pHbSqlite3->db, nullptr, nullptr);
    }
  }
}

/**
   Compile-Time Authorization Callbacks

   sqlite3_set_authorizer(pDb, [cFunc|sFunc])
 */

HB_FUNC(SQLITE3_SET_AUTHORIZER)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db)
  {
    if (pHbSqlite3->cbAuthorizer)
    {
      hb_itemRelease(pHbSqlite3->cbAuthorizer);
      pHbSqlite3->cbAuthorizer = nullptr;
    }

    if (HB_ISEVALITEM(2))
    {
      pHbSqlite3->cbAuthorizer = hb_itemNew(hb_param(2, Harbour::Item::EVALITEM));
      hb_gcUnlock(pHbSqlite3->cbAuthorizer);

      hb_retni(sqlite3_set_authorizer(pHbSqlite3->db, authorizer, static_cast<void *>(pHbSqlite3->cbAuthorizer)));
    }
    else
    {
      hb_retni(sqlite3_set_authorizer(pHbSqlite3->db, nullptr, nullptr));
    }
  }
}

/**
   This API is used to overwrite the contents of one database with that
   of another. It is useful either for creating backups of databases or
   for copying in-memory databases to or from persistent files.

   ! Experimental !

   sqlite3_backup_init(DbDest, cDestName, DbSource, cSourceName) ->
               return pointer to Backup or NIL if error occurs

   sqlite3_backup_step(pBackup, nPage) --> nResult
   sqlite3_backup_finish(pBackup)      --> nResult
   sqlite3_backup_remaining(pBackup)   --> nResult
   sqlite3_backup_pagecount(pBackup)   --> nResult
 */

HB_FUNC(SQLITE3_BACKUP_INIT)
{
#if SQLITE_VERSION_NUMBER >= 3006011
  auto pHbSqlite3Dest = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));
  auto pHbSqlite3Source = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(3, HB_SQLITE3_DB, true));

  if (pHbSqlite3Dest && pHbSqlite3Dest->db && pHbSqlite3Source && pHbSqlite3Source->db && HB_ISCHAR(2) && HB_ISCHAR(4))
  {
    sqlite3_backup *pBackup = sqlite3_backup_init(pHbSqlite3Dest->db, hb_parcx(2), pHbSqlite3Source->db, hb_parcx(4));

    if (pBackup)
    {
      hb_retptr(pBackup); /* FIXME: Create GC collected pointer */
    }
    else
    {
      hb_retptr(nullptr);
    }
  }
  else
  {
    hb_retptr(nullptr);
  }
#else
  hb_retptr(nullptr);
#endif
}

HB_FUNC(SQLITE3_BACKUP_STEP)
{
#if SQLITE_VERSION_NUMBER >= 3006011
  /* FIXME: Use GC collected pointer */
  auto pBackup = static_cast<sqlite3_backup *>(hb_parptr(1));

  if (pBackup != nullptr)
  {
    hb_retni(sqlite3_backup_step(pBackup, hb_parni(2)));
  }
  else
  {
    hb_retni(-1);
  }
#else
  hb_retni(-1);
#endif
}

HB_FUNC(SQLITE3_BACKUP_FINISH)
{
#if SQLITE_VERSION_NUMBER >= 3006011
  /* FIXME: Use and free GC collected pointer */
  auto pBackup = static_cast<sqlite3_backup *>(hb_parptr(1));

  if (pBackup != nullptr)
  {
    hb_retni(sqlite3_backup_finish(pBackup));
  }
  else
  {
    hb_retni(-1);
  }
#else
  hb_retni(-1);
#endif
}

HB_FUNC(SQLITE3_BACKUP_REMAINING)
{
#if SQLITE_VERSION_NUMBER >= 3006011
  /* FIXME: Use GC collected pointer */
  auto pBackup = static_cast<sqlite3_backup *>(hb_parptr(1));

  if (pBackup != nullptr)
  {
    hb_retni(sqlite3_backup_remaining(pBackup));
  }
  else
  {
    hb_retni(-1);
  }
#else
  hb_retni(-1);
#endif
}

HB_FUNC(SQLITE3_BACKUP_PAGECOUNT)
{
#if SQLITE_VERSION_NUMBER >= 3006011
  /* FIXME: Use GC collected pointer */
  auto pBackup = static_cast<sqlite3_backup *>(hb_parptr(1));

  if (pBackup != nullptr)
  {
    hb_retni(sqlite3_backup_pagecount(pBackup));
  }
  else
  {
    hb_retni(-1);
  }
#else
  hb_retni(-1);
#endif
}

/**
   Memory Allocator Statistics

   sqlite3_memory_used() --> nResult
   sqlite3_memory_highwater(lResetFlag) --> nResult
 */

HB_FUNC(SQLITE3_MEMORY_USED)
{
/* FIXME: verify the exact SQLITE3 version */
#if SQLITE_VERSION_NUMBER > 3004001
  hb_retnint(sqlite3_memory_used());
#else
  hb_retnint(-1);
#endif
}

HB_FUNC(SQLITE3_MEMORY_HIGHWATER)
{
/* FIXME: verify the exact SQLITE3 version */
#if SQLITE_VERSION_NUMBER > 3004001
  hb_retnint(sqlite3_memory_highwater(static_cast<int>(hb_parl(1))));
#else
  hb_retnint(-1);
#endif
}

/**
   Test to see if the library is thread-safe

   sqlite3_threadsafe() --> nResult
 */

HB_FUNC(SQLITE3_THREADSAFE)
{
/* FIXME: verify the exact SQLITE3 version */
#if SQLITE_VERSION_NUMBER > 3004001
  hb_retni(sqlite3_threadsafe());
#else
  hb_retni(-1);
#endif
}

/**
   SQLite Runtime Status

   sqlite3_status(nOp, @nCurrent, @nHighwater, lResetFlag) --> nResult
 */

HB_FUNC(SQLITE3_STATUS)
{
#if SQLITE_VERSION_NUMBER >= 3006000
  if (hb_pcount() > 3 && (HB_ISNUM(2) && HB_ISBYREF(2)) && (HB_ISNUM(3) && HB_ISBYREF(3)))
  {
    int iCurrent, iHighwater;
    hb_retni(sqlite3_status(hb_parni(1), &iCurrent, &iHighwater, static_cast<int>(hb_parl(4))));
    hb_storni(iCurrent, 2);
    hb_storni(iHighwater, 3);
    return;
  }
#endif
  hb_storni(0, 3);
  hb_storni(0, 4);
  hb_retni(-1);
}

HB_FUNC(SQLITE3_STATUS64)
{
#if SQLITE_VERSION_NUMBER >= 3080900 && !defined(HB_LONG_LONG_OFF)
  if (hb_pcount() > 3 && (HB_ISNUM(2) && HB_ISBYREF(2)) && (HB_ISNUM(3) && HB_ISBYREF(3)))
  {
    sqlite3_int64 iCurrent, iHighwater;
    hb_retni(sqlite3_status(hb_parni(1), &iCurrent, &iHighwater, static_cast<int>(hb_parl(4))));
    hb_stornint(iCurrent, 2);
    hb_stornint(iHighwater, 3);
    return;
  }
#endif
  hb_stornint(0, 2);
  hb_stornint(0, 3);
  hb_retni(-1);
}

/**
   Database Connection Status

   sqlite3_db_status(pDb, nOp, @nCurrent, @nHighwater, lResetFlag) --> nResult
 */

HB_FUNC(SQLITE3_DB_STATUS)
{
#if SQLITE_VERSION_NUMBER >= 3006001
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db && (hb_pcount() > 4) && (HB_ISNUM(3) && HB_ISBYREF(3)) &&
      (HB_ISNUM(4) && HB_ISBYREF(4)))
  {
    int iCurrent, iHighwater;
    hb_retni(sqlite3_db_status(pHbSqlite3->db, hb_parni(2), &iCurrent, &iHighwater, static_cast<int>(hb_parl(5))));
    hb_storni(iCurrent, 3);
    hb_storni(iHighwater, 4);
    return;
  }
#endif
  hb_storni(0, 3);
  hb_storni(0, 4);
  hb_retni(-1);
}

/**
   Run-time Limits

   sqlite3_limit(pDb, nId, nNewVal) --> nOldVal
 */

HB_FUNC(SQLITE3_LIMIT)
{
#if SQLITE_VERSION_NUMBER >= 3005008
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db && (hb_pcount() > 2) && HB_ISNUM(2) && HB_ISNUM(3))
  {
    hb_retni(sqlite3_limit(pHbSqlite3->db, hb_parni(2), hb_parni(3)));
  }
  else
  {
    hb_retni(-1);
  }
#else
  hb_retni(-1);
#endif
}

/**
   Run-Time Library Compilation Options Diagnostics

   sqlite3_compileoption_used(cOptName) --> nResult
   sqlite3_compileoption_get(nOptNum)   --> cResult
 */

HB_FUNC(SQLITE3_COMPILEOPTION_USED)
{
#if SQLITE_VERSION_NUMBER >= 3006023
  hb_retl(static_cast<HB_BOOL>(sqlite3_compileoption_used(hb_parcx(1))));
#else
  hb_retl(false);
#endif
}

HB_FUNC(SQLITE3_COMPILEOPTION_GET)
{
#if SQLITE_VERSION_NUMBER >= 3006023
  hb_retc(sqlite3_compileoption_get(hb_parni(1)));
#else
  hb_retc_null();
#endif
}

/**
   Create Or Redefine SQL Functions

   sqlite3_create_function( db, cFuncName, nArg, [cFunc|sFunc], [nFunctionFlags] )

   Only scalar function creation now supported.
 */
HB_FUNC(SQLITE3_CREATE_FUNCTION)
{
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db && HB_ISCHAR(2))
  {
    if (pHbSqlite3->cbFunc)
    {
      hb_itemRelease(pHbSqlite3->cbFunc);
      pHbSqlite3->cbFunc = nullptr;
    }

    void *hFuncName = nullptr;

    if (HB_ISEVALITEM(4))
    {
      pHbSqlite3->cbFunc = hb_itemNew(hb_param(4, Harbour::Item::EVALITEM));
      hb_gcUnlock(pHbSqlite3->cbFunc);
      hb_retni(sqlite3_create_function(pHbSqlite3->db, hb_parstr_utf8(2, &hFuncName, nullptr), hb_parnidef(4, -1),
                                       SQLITE_UTF8 | hb_parnidef(5, 0), pHbSqlite3->cbFunc, func, nullptr, nullptr));
    }
    else
    {
      hb_retni(sqlite3_create_function(pHbSqlite3->db, hb_parstr_utf8(2, &hFuncName, nullptr), -1,
                                       SQLITE_UTF8 | hb_parnidef(5, 0), nullptr, nullptr, nullptr, nullptr));
    }

    if (hFuncName)
    {
      hb_strfree(hFuncName);
    }
  }
  else
  {
    hb_retni(SQLITE_ERROR);
  }
}

/**
   Get database filename for given connection and database name

   sqlite3_db_filename( pDb, sDbName )
  */
HB_FUNC(SQLITE3_DB_FILENAME)
{
#if SQLITE_VERSION_NUMBER >= 3007010
  auto pHbSqlite3 = static_cast<HB_SQLITE3 *>(hb_sqlite3_param(1, HB_SQLITE3_DB, true));

  if (pHbSqlite3 && pHbSqlite3->db && HB_ISCHAR(2))
  {
    hb_retc(static_cast<const char *>(sqlite3_db_filename(pHbSqlite3->db, hb_parc(2))));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_errRT_BASE_SubstR(EG_UNSUPPORTED, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
#endif
}

/**
   Get expanded SQL for a prepared statement

   sqlite3_expanded_sql( pPreparedStatement )
  */
HB_FUNC(SQLITE3_EXPANDED_SQL)
{
#if SQLITE_VERSION_NUMBER >= 3014000
  auto pStmt = static_cast<psqlite3_stmt>(hb_parptr(1));
  if (pStmt)
  {
    char *sql = sqlite3_expanded_sql(pStmt);
    hb_retstr_utf8(sql);
    sqlite3_free(sql);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
#else
  hb_errRT_BASE_SubstR(EG_UNSUPPORTED, 0, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
#endif
}
