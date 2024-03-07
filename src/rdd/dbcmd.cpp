/*
 * Base RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2004-2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 2002 Horacio Roldan <harbour_ar@yahoo.com.ar> (hb_rddIterateWorkAreas(), hb_rddGetTempAlias())
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
#include "hbapirdd.hpp"
#include "hbapierr.hpp"
#include "hbapiitm.hpp"
#include "hbvm.hpp"
#include "hbset.hpp"

/* The 5th parameter is Harbour extension */
HB_FUNC(AFIELDS)
{
  HB_USHORT uiFields;
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto pName = hb_param(1, Harbour::Item::ARRAY);
  auto pType = hb_param(2, Harbour::Item::ARRAY);
  auto pLen = hb_param(3, Harbour::Item::ARRAY);
  auto pDec = hb_param(4, Harbour::Item::ARRAY);

#ifdef DBS_FLAG
  auto pFlags = hb_param(5, Harbour::Item::ARRAY);
#else
  PHB_ITEM pFlags = nullptr;
#endif

  if (!pArea || (!pName && !pType && !pLen && !pDec && !pFlags))
  {
    hb_retni(0);
    return;
  }

  if (SELF_FIELDCOUNT(pArea, &uiFields) != Harbour::SUCCESS)
  {
    return;
  }

  if (pName)
  {
    auto uiArrayLen = static_cast<HB_USHORT>(hb_arrayLen(pName));
    if (uiArrayLen < uiFields)
    {
      uiFields = uiArrayLen;
    }
  }
  if (pType)
  {
    auto uiArrayLen = static_cast<HB_USHORT>(hb_arrayLen(pType));
    if (uiArrayLen < uiFields)
    {
      uiFields = uiArrayLen;
    }
  }
  if (pLen)
  {
    auto uiArrayLen = static_cast<HB_USHORT>(hb_arrayLen(pLen));
    if (uiArrayLen < uiFields)
    {
      uiFields = uiArrayLen;
    }
  }
  if (pDec)
  {
    auto uiArrayLen = static_cast<HB_USHORT>(hb_arrayLen(pDec));
    if (uiArrayLen < uiFields)
    {
      uiFields = uiArrayLen;
    }
  }

  if (pFlags)
  {
    auto uiArrayLen = static_cast<HB_USHORT>(hb_arrayLen(pFlags));
    if (uiArrayLen < uiFields)
    {
      uiFields = uiArrayLen;
    }
  }

  if (pName)
  {
    for (HB_USHORT uiCount = 1; uiCount <= uiFields; ++uiCount)
    {
      if (SELF_FIELDINFO(pArea, uiCount, DBS_NAME, hb_arrayGetItemPtr(pName, uiCount)) != Harbour::SUCCESS)
      {
        return;
      }
    }
  }
  if (pType)
  {
    for (HB_USHORT uiCount = 1; uiCount <= uiFields; ++uiCount)
    {
      if (SELF_FIELDINFO(pArea, uiCount, DBS_TYPE, hb_arrayGetItemPtr(pType, uiCount)) != Harbour::SUCCESS)
      {
        return;
      }
    }
  }
  if (pLen)
  {
    for (HB_USHORT uiCount = 1; uiCount <= uiFields; ++uiCount)
    {
      if (SELF_FIELDINFO(pArea, uiCount, DBS_LEN, hb_arrayGetItemPtr(pLen, uiCount)) != Harbour::SUCCESS)
      {
        return;
      }
    }
  }
  if (pDec)
  {
    for (HB_USHORT uiCount = 1; uiCount <= uiFields; ++uiCount)
    {
      if (SELF_FIELDINFO(pArea, uiCount, DBS_DEC, hb_arrayGetItemPtr(pDec, uiCount)) != Harbour::SUCCESS)
      {
        return;
      }
    }
  }
#ifdef DBS_FLAG
  if (pFlags)
  {
    for (HB_USHORT uiCount = 1; uiCount <= uiFields; ++uiCount)
    {
      if (SELF_FIELDINFO(pArea, uiCount, DBS_FLAG, hb_arrayGetItemPtr(pFlags, uiCount)) != Harbour::SUCCESS)
      {
        return;
      }
    }
  }
#endif

  hb_retni(uiFields);
}

// NOTES FOR HARBOUR++ V2:
// deprecate ALIAS
// change ALIAS to DBALIAS
// maintain ALIAS using HB_FUNC_TRANSLATE
HB_FUNC(ALIAS)
{
  auto pArea = static_cast<AREAP>(hb_rddGetWorkAreaPointer(static_cast<HB_AREANO>(hb_parni(1))));

  if (pArea != nullptr)
  {
    char szAlias[HB_RDD_MAX_ALIAS_LEN + 1];

    if (SELF_ALIAS(pArea, szAlias) == Harbour::SUCCESS)
    {
      hb_retc(szAlias);
      return;
    }
  }
  hb_retc_null();
}

HB_FUNC(DBEVAL)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBEVALINFO pEvalInfo{};
    pEvalInfo.itmBlock = hb_param(1, Harbour::Item::BLOCK);
    if (!pEvalInfo.itmBlock)
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_EVAL_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      return;
    }

    pEvalInfo.dbsci.itmCobFor = hb_param(2, Harbour::Item::BLOCK);
    if (!pEvalInfo.dbsci.itmCobFor && !HB_ISNIL(2))
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_EVAL_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      return;
    }

    pEvalInfo.dbsci.itmCobWhile = hb_param(3, Harbour::Item::BLOCK);
    if (!pEvalInfo.dbsci.itmCobWhile && !HB_ISNIL(3))
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_EVAL_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      return;
    }

    pEvalInfo.dbsci.lNext = hb_param(4, Harbour::Item::NUMERIC);
    if (!pEvalInfo.dbsci.lNext && !HB_ISNIL(4))
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_EVAL_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      return;
    }

    pEvalInfo.dbsci.itmRecID = hb_param(5, Harbour::Item::NUMERIC);
    if (!pEvalInfo.dbsci.itmRecID && !HB_ISNIL(5))
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_EVAL_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      return;
    }

    pEvalInfo.dbsci.fRest = hb_param(6, Harbour::Item::LOGICAL);
    if (!pEvalInfo.dbsci.fRest && !HB_ISNIL(6))
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_EVAL_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      return;
    }

    SELF_DBEVAL(pArea, &pEvalInfo);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBF)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    char szAlias[HB_RDD_MAX_ALIAS_LEN + 1];

    if (SELF_ALIAS(pArea, szAlias) == Harbour::SUCCESS)
    {
      hb_retc(szAlias);
      return;
    }
  }
  hb_retc_null();
}

// NOTES FOR HARBOUR++ V2:
// deprecate BOF
// change BOF to DBBOF
// maintain BOF using HB_FUNC_TRANSLATE
HB_FUNC(BOF)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  HB_BOOL bBof = true;
  if (pArea != nullptr)
  {
    SELF_BOF(pArea, &bBof);
  }

  hb_retl(bBof);
}

/* dbAppend([<lUnLockAll>=.T.]) --> <lSuccess> */
HB_FUNC(DBAPPEND)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    bool bUnLockAll = hb_parldef(1, true);

    /* Clipper clears NETERR flag before APPEND */
    hb_rddSetNetErr(false);
    HB_ERRCODE errCode = SELF_APPEND(pArea, bUnLockAll);
    hb_retl(errCode == Harbour::SUCCESS);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBCLOSEALL)
{
  hb_rddCloseAll();
}

HB_FUNC(DBCLOSEAREA)
{
  hb_rddReleaseCurrentArea();
}

HB_FUNC(DBCOMMIT)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    SELF_FLUSH(pArea);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBCOMMITALL)
{
  hb_rddFlushAll();
}

/*
 * In Clipper the arguments are:
 *    dbCreate(cFile, aStruct, cRDD, lKeepOpen, cAlias, cDelimArg) --> NIL
 * In Harbour (HB_EXTENSION):
 *    dbCreate(cFile, aStruct, cRDD, lKeepOpen, cAlias, cDelimArg, ;
 *              cCodePage, nConnection) --> <lSuccess>
 */
HB_FUNC(DBCREATE)
{
  /*
   * NOTE: 4th, 5th and 6th parameters are undocumented Clipper ones
   * 4th is boolean flag indicating if file should stay open (any boolean
   *     value will enable this behavior)
   * 5th is alias - if not given then WA is open without alias
   * 6th is optional DELIMITED value used by some RDDs like DELIM
   */

  auto szFileName = hb_parc(1);
  auto pStruct = hb_param(2, Harbour::Item::ARRAY);
  auto szDriver = hb_parc(3);
  bool fKeepOpen = HB_ISLOG(4);
  bool fCurrArea = fKeepOpen && !hb_parl(4);
  auto szAlias = hb_parc(5);
  auto pDelim = hb_param(6, Harbour::Item::ANY);
  auto szCpId = hb_parc(7);
  HB_ULONG ulConnection = hb_parnl(8);

  /*
   * Clipper allows to use empty struct array for RDDs which does not
   * support fields, f.e.: DBFBLOB in CL5.3
   * In CL5.3 it's also possible to create DBF file without fields.
   * if some RDD wants to block it then they should serve it in lower
   * level, [druzus]
   */
  if (!pStruct ||
#ifdef HB_CLP_STRICT
      hb_arrayLen(pStruct) == 0 ||
#endif
      !szFileName)
  {
    hb_errRT_DBCMD(EG_ARG, EDBCMD_DBCMDBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    return;
  }

  auto uiLen = static_cast<HB_USHORT>(hb_arrayLen(pStruct));

  for (HB_USHORT uiSize = 1; uiSize <= uiLen; ++uiSize)
  {
    auto pFieldDesc = hb_arrayGetItemPtr(pStruct, uiSize);

    /* Validate items types of fields */
    if (hb_arrayLen(pFieldDesc) < 4 || !(hb_arrayGetType(pFieldDesc, 1) & Harbour::Item::STRING) ||
        !(hb_arrayGetType(pFieldDesc, 2) & Harbour::Item::STRING) ||
        !(hb_arrayGetType(pFieldDesc, 3) & Harbour::Item::NUMERIC) ||
        !(hb_arrayGetType(pFieldDesc, 4) & Harbour::Item::NUMERIC))
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_DBCMDBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      return;
    }
  }

  hb_retl(hb_rddCreateTable(szFileName, szDriver,
                            fCurrArea ? static_cast<HB_AREANO>(hb_rddGetCurrentWorkAreaNumber()) : 0, szAlias,
                            fKeepOpen, szCpId, ulConnection, pStruct, pDelim) == Harbour::SUCCESS);
}

/*
 * hb_dbCreateTemp(<cAlias>, <aStruct>, <cRDD>, <cCodePage>, <nConnection>) --> <lSuccess>
 */
HB_FUNC(HB_DBCREATETEMP)
{
  auto szAlias = hb_parc(1);
  auto pStruct = hb_param(2, Harbour::Item::ARRAY);
  auto szDriver = hb_parc(3);
  auto szCpId = hb_parc(4);
  HB_ULONG ulConnection = hb_parnl(5);

  /*
   * Clipper allows to use empty struct array for RDDs which does not
   * support fields, f.e.: DBFBLOB in CL5.3
   * In CL5.3 it's also possible to create DBF file without fields.
   * if some RDD wants to block it then they should serve it in lower
   * level, [druzus]
   */
  if (!szAlias || !pStruct
#ifdef HB_CLP_STRICT
      || hb_arrayLen(pStruct) == 0
#endif
  )
  {
    hb_errRT_DBCMD(EG_ARG, EDBCMD_DBCMDBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    return;
  }

  auto uiLen = static_cast<HB_USHORT>(hb_arrayLen(pStruct));

  for (HB_USHORT uiSize = 1; uiSize <= uiLen; ++uiSize)
  {
    auto pFieldDesc = hb_arrayGetItemPtr(pStruct, uiSize);

    /* Validate items types of fields */
    if (hb_arrayLen(pFieldDesc) < 4 || !(hb_arrayGetType(pFieldDesc, 1) & Harbour::Item::STRING) ||
        !(hb_arrayGetType(pFieldDesc, 2) & Harbour::Item::STRING) ||
        !(hb_arrayGetType(pFieldDesc, 3) & Harbour::Item::NUMERIC) ||
        !(hb_arrayGetType(pFieldDesc, 4) & Harbour::Item::NUMERIC))
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_DBCMDBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      return;
    }
  }

  hb_retl(hb_rddCreateTableTemp(szDriver, szAlias, szCpId, ulConnection, pStruct) == Harbour::SUCCESS);
}

/*
 * I'm not sure if lKeepOpen open works exactly like in dbCreate(), I haven't
 * tested it with Clipper yet. If it doesn't then please inform me about it
 * and I'll update the code. [druzus]
 */

/* NOTE: The created table will be kept open if lOpenMode parameter
         is of logical type. If .T. it will be opened in a new workarea,
         if .F. it will be opened in the current one. */
/* NOTE: Has an identical parameter list with dbCreate() */

/* __dbOpenSDF( cFile, aStruct, cRDD, lKeepOpen, cAlias, cDelimArg, cCodePage, nConnection ) --> <lSuccess> */
HB_FUNC(__DBOPENSDF)
{
  /*
   * NOTE: 4th and 5th parameters are undocumented Clipper ones
   * 4th is boolean flag indicating if file should stay open and
   * 5th is alias - if not given then WA is open without alias
   */

  auto szFileName = hb_parc(1);
  auto pStruct = hb_param(2, Harbour::Item::ARRAY);
  auto szDriver = hb_parc(3);
  bool fKeepOpen = HB_ISLOG(4);
  bool fCurrArea = fKeepOpen && !hb_parl(4);
  auto szAlias = hb_parc(5);
  auto pDelim = hb_param(6, Harbour::Item::ANY);
  auto szCpId = hb_parc(7);
  HB_ULONG ulConnection = hb_parnl(8);

  if (!pStruct || hb_arrayLen(pStruct) == 0 || !szFileName || !szFileName[0])
  {
    hb_errRT_DBCMD(EG_ARG, EDBCMD_DBCMDBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    return;
  }

  auto uiLen = static_cast<HB_USHORT>(hb_arrayLen(pStruct));

  for (HB_USHORT uiSize = 1; uiSize <= uiLen; ++uiSize)
  {
    auto pFieldDesc = hb_arrayGetItemPtr(pStruct, uiSize);

    /* Validate items types of fields */
    if (hb_arrayLen(pFieldDesc) < 4 || !(hb_arrayGetType(pFieldDesc, 1) & Harbour::Item::STRING) ||
        !(hb_arrayGetType(pFieldDesc, 2) & Harbour::Item::STRING) ||
        !(hb_arrayGetType(pFieldDesc, 3) & Harbour::Item::NUMERIC) ||
        !(hb_arrayGetType(pFieldDesc, 4) & Harbour::Item::NUMERIC))
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_DBCMDBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      return;
    }
  }

  HB_ERRCODE errCode =
      hb_rddOpenTable(szFileName, szDriver, fCurrArea ? static_cast<HB_AREANO>(hb_rddGetCurrentWorkAreaNumber()) : 0,
                      szAlias, true, true, szCpId, ulConnection, pStruct, pDelim);

  if (!fKeepOpen && errCode == Harbour::SUCCESS)
  {
    hb_rddReleaseCurrentArea();
  }

  hb_retl(errCode == Harbour::SUCCESS);
}

HB_FUNC(DBDELETE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    SELF_DELETE(pArea);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBRECALL)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    SELF_RECALL(pArea);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBGOBOTTOM)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    SELF_GOBOTTOM(pArea);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBGOTO)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto pItem = hb_param(1, Harbour::Item::ANY);
    if (!pItem)
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_NOVAR, nullptr, HB_ERR_FUNCNAME);
    }
    else
    {
      SELF_GOTOID(pArea, pItem);
    }
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBGOTOP)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    SELF_GOTOP(pArea);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(__DBLOCATE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBSCOPEINFO dbScopeInfo;

    dbScopeInfo.itmCobFor = hb_param(1, Harbour::Item::BLOCK);
    dbScopeInfo.lpstrFor = nullptr;
    dbScopeInfo.itmCobWhile = hb_param(2, Harbour::Item::BLOCK);
    dbScopeInfo.lpstrWhile = nullptr;
    dbScopeInfo.lNext = hb_param(3, Harbour::Item::NUMERIC);
    dbScopeInfo.itmRecID = hb_param(4, Harbour::Item::NUMERIC);
    dbScopeInfo.fRest = hb_param(5, Harbour::Item::LOGICAL);

    dbScopeInfo.fIgnoreFilter = true;
    dbScopeInfo.fIncludeDeleted = true;
    dbScopeInfo.fLast = false;
    dbScopeInfo.fIgnoreDuplicates = false;
    dbScopeInfo.fBackward = false;

    if (SELF_SETLOCATE(pArea, &dbScopeInfo) == Harbour::SUCCESS)
    {
      SELF_LOCATE(pArea, false);
    }
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EG_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(__DBSETLOCATE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto pLocate = hb_param(1, Harbour::Item::BLOCK);
    if (pLocate)
    {
      DBSCOPEINFO pScopeInfo{};
      pScopeInfo.itmCobFor = pLocate;
      SELF_SETLOCATE(pArea, &pScopeInfo);
    }
  }
}

HB_FUNC(__DBCONTINUE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    SELF_LOCATE(pArea, true);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(__DBPACK)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    /*
     * Additional feature: __dbPack([<bBlock>, [<nEvery>])
     * Code Block to execute for every record.
     */
    auto pBlock = hb_param(1, Harbour::Item::BLOCK);
    if (pBlock)
    {
      hb_itemRelease(pArea->valResult);
      pArea->valResult = hb_itemArrayNew(2);
      hb_arraySet(pArea->valResult, 1, pBlock);
      auto pEvery = hb_param(2, Harbour::Item::NUMERIC);
      if (pEvery)
      {
        hb_arraySet(pArea->valResult, 2, pEvery);
      }
      else
      {
        hb_arraySetNI(pArea->valResult, 2, 0);
      }
    }
    else
    {
      if (pArea->valResult)
      {
        hb_itemClear(pArea->valResult);
      }
      else
      {
        pArea->valResult = hb_itemNew(nullptr);
      }
    }
    SELF_PACK(pArea);
    if (pBlock)
    {
      hb_itemClear(pArea->valResult);
    }
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBRLOCK)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBLOCKINFO dbLockInfo;
    dbLockInfo.fResult = false;
    dbLockInfo.itmRecID = hb_param(1, Harbour::Item::ANY);
    if (!dbLockInfo.itmRecID || HB_ISNIL(1))
    {
      dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
    }
    else
    {
      dbLockInfo.uiMethod = DBLM_MULTIPLE;
    }
    SELF_LOCK(pArea, &dbLockInfo);
    hb_retl(dbLockInfo.fResult);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBRLOCKLIST)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto pList = hb_itemArrayNew(0);
    SELF_INFO(pArea, DBI_GETLOCKARRAY, pList);
    hb_itemReturnRelease(pList);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBRUNLOCK)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    SELF_UNLOCK(pArea, hb_param(1, Harbour::Item::ANY));
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBSEEK)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    if (!HB_ISNIL(1))
    {
      auto pKey = hb_param(1, Harbour::Item::ANY);
      bool bSoftSeek = HB_ISLOG(2) ? static_cast<HB_BOOL>(hb_parl(2)) : hb_setGetSoftSeek();
      bool bFindLast = hb_parl(3) /* HB_EXTENSION */;
      HB_BOOL fFound = false;
      if (SELF_SEEK(pArea, bSoftSeek, pKey, bFindLast) == Harbour::SUCCESS)
      {
        if (SELF_FOUND(pArea, &fFound) != Harbour::SUCCESS)
        {
          fFound = false;
        }
      }
      hb_retl(fFound);
    }
    else
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_SEEK_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    }
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC( DBSELECTAREA )
{
  auto szAlias = hb_parc(1);

  if (szAlias != nullptr)
  {
    hb_rddSelectWorkAreaAlias(szAlias);
  }
  else
  {
    auto iNewArea = hb_parni(1);

    if (iNewArea == 0)
    {
      auto pItem = hb_param(1, Harbour::Item::SYMBOL);
      if (pItem != nullptr)
      {
        auto pSymAlias = hb_itemGetSymbol(pItem);
        if (pSymAlias->pDynSym != nullptr)
        {
          iNewArea = static_cast<int>(hb_dynsymAreaHandle(pSymAlias->pDynSym));
        }
      }
    }
    hb_rddSelectWorkAreaNumber(iNewArea);
  }
}

HB_FUNC(__DBSETFOUND)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto pFound = hb_param(1, Harbour::Item::LOGICAL);
    if (pFound)
    {
      pArea->fFound = hb_itemGetL(pFound);
    }
  }
}

HB_FUNC(DBSETFILTER)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBFILTERINFO pFilterInfo;

    auto pBlock = hb_param(1, Harbour::Item::BLOCK);
    auto pText = hb_param(2, Harbour::Item::STRING);
    /* Cl*pper allows to set text filter without codeblock. In local
       RDDs it effectively does nothing and only dbFilter() returns it
       but RDDs with automatic filter optimization like CL53/DBFCDX /
       COMIX/ClipMore or RDDs working with remote data base servers
       may use only text version of filter and ignore or use with
       lower priority the codeblock so Harbour has to work like
       Cl*pper here. [druzus] */
    if (pBlock || hb_itemGetCLen(pText) > 0)
    {
      pFilterInfo.itmCobExpr = pBlock;
      if (pText)
      {
        pFilterInfo.abFilterText = pText;
      }
      else
      {
        pFilterInfo.abFilterText = hb_itemPutC(nullptr, nullptr);
      }
      pFilterInfo.fFilter = true;
      pFilterInfo.lpvCargo = nullptr;
      pFilterInfo.fOptimized = false;
      SELF_SETFILTER(pArea, &pFilterInfo);
      if (!pText)
      {
        hb_itemRelease(pFilterInfo.abFilterText);
      }
    }
    else
    {
      SELF_CLEARFILTER(pArea);
    }
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBCLEARFILTER)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    SELF_CLEARFILTER(pArea);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBFILTER)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto pFilter = hb_itemPutC(nullptr, nullptr);
    SELF_FILTERTEXT(pArea, pFilter);
    hb_itemReturnRelease(pFilter);
  }
  else
  {
    hb_retc_null();
  }
}

/* Harbour extension to retrieve filter codeblock */
HB_FUNC(HB_DBGETFILTER)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    hb_itemReturn(pArea->dbfi.itmCobExpr);
  }
  else
  {
    hb_ret();
  }
}

HB_FUNC(DBSKIP)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    SELF_SKIP(pArea, hb_parnldef(1, 1));
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBSTRUCT)
{
  auto pStruct = hb_itemArrayNew(0);
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    hb_tblStructure(pArea, pStruct, DBS_ALEN);
  }
  hb_itemReturnRelease(pStruct);
}

HB_FUNC(DBTABLEEXT)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto pItem = hb_itemNew(nullptr);
  HB_ERRCODE errCode = Harbour::FAILURE;

  if (!pArea)
  {
    HB_USHORT uiRddID;
    auto pRddNode = hb_rddFindNode(hb_rddDefaultDrv(nullptr), &uiRddID);
    if (pRddNode)
    {
      pArea = static_cast<AREAP>(hb_rddNewAreaNode(pRddNode, uiRddID));
      if (pArea != nullptr)
      {
        errCode = SELF_INFO(pArea, DBI_TABLEEXT, pItem);
        SELF_RELEASE(pArea);
      }
    }
  }
  else
  {
    errCode = SELF_INFO(pArea, DBI_TABLEEXT, pItem);
  }

  if (errCode != Harbour::SUCCESS)
  {
    hb_itemPutC(pItem, nullptr);
  }
  hb_itemReturnRelease(pItem);
}

HB_FUNC(DBUNLOCK)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    SELF_UNLOCK(pArea, nullptr);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBUNLOCKALL)
{
  hb_rddUnLockAll();
}

/* dbUseArea([<lNewArea>], [<cDriver>], <cName>, [<xcAlias>], ;
             [<lShared>], [<lReadonly>], [<cCodePage>], ;
             [<nConnection>]) --> <lSuccess> */
HB_FUNC(DBUSEAREA)
{
  hb_retl(hb_rddOpenTable(hb_parc(3), hb_parc(2),
                          hb_parl(1) ? 0 : static_cast<HB_AREANO>(hb_rddGetCurrentWorkAreaNumber()), hb_parc(4),
                          HB_ISLOG(5) ? hb_parl(5) : !hb_setGetExclusive(), hb_parl(6), hb_parc(7), hb_parnl(8),
                          nullptr, nullptr) == Harbour::SUCCESS);
}

HB_FUNC(__DBZAP)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    SELF_ZAP(pArea);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

// NOTES FOR HARBOUR++ V2:
// deprecate DELETED
// change DELETED to DBDELETED
// maintain DELETED using HB_FUNC_TRANSLATE
HB_FUNC(DELETED)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  HB_BOOL bDeleted = false;
  if (pArea != nullptr)
  {
    SELF_DELETED(pArea, &bDeleted);
  }
  hb_retl(bDeleted);
}

// NOTES FOR HARBOUR++ V2:
// deprecate EOF
// change EOF to DBEOF
// maintain EOF using HB_FUNC_TRANSLATE
HB_FUNC(EOF)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  HB_BOOL bEof = true;
  if (pArea != nullptr)
  {
    SELF_EOF(pArea, &bEof);
  }
  hb_retl(bEof);
}

// NOTES FOR HARBOUR++ V2:
// deprecate FCOUNT
// change FCOUNT to DBFCOUNT
// maintain FCOUNT using HB_FUNC_TRANSLATE
HB_FUNC(FCOUNT)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  HB_USHORT uiFields = 0;
  if (pArea != nullptr)
  {
    SELF_FIELDCOUNT(pArea, &uiFields);
  }
  hb_retni(uiFields);
}

// NOTES FOR HARBOUR++ V2:
// deprecate FIELDGET
// change FIELDGET to DBFIELDGET
// maintain FIELDGET using HB_FUNC_TRANSLATE
HB_FUNC(FIELDGET)
{
  auto pItem = hb_itemNew(nullptr);
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  HB_USHORT uiField = static_cast<HB_FIELDNO>(hb_parni(1)); // TODO: cast != type

  if (pArea && uiField)
  {
    SELF_GETVALUE(pArea, uiField, pItem);
  }

  hb_itemReturnRelease(pItem);
}

// NOTES FOR HARBOUR++ V2:
// deprecate FIELDNAME
// change FIELDNAME to DBFIELDNAME
// maintain FIELDNAME using HB_FUNC_TRANSLATE
HB_FUNC(FIELDNAME)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  HB_USHORT uiFields, uiIndex = static_cast<HB_FIELDNO>(hb_parni(1)); // TODO: cast != type

  if (pArea && uiIndex)
  {
    if (SELF_FIELDCOUNT(pArea, &uiFields) == Harbour::SUCCESS && uiIndex <= uiFields)
    {
      auto szName = static_cast<char *>(hb_xgrab(pArea->uiMaxFieldNameLength + 1));
      szName[0] = '\0';
      SELF_FIELDNAME(pArea, uiIndex, szName);
      hb_retc_buffer(szName);
      return;
    }
    /* This is not Clipper compatible! - David G. Holm <dholm@jsd-llc.com> */
#if 0
      hb_errRT_DBCMD(EG_ARG, EDBCMD_FIELDNAME_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
#endif
  }
  hb_retc_null();
}

// NOTES FOR HARBOUR++ V2:
// deprecate FIELDPOS
// change FIELDPOS to DBFIELDPOS
// maintain FIELDPOS using HB_FUNC_TRANSLATE
HB_FUNC(FIELDPOS)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea && hb_parclen(1) > 0)
  {
    hb_retni(hb_rddFieldIndex(pArea, hb_parc(1)));
  }
  else
  {
    hb_retni(0);
  }
}

// NOTES FOR HARBOUR++ V2:
// deprecate FIELDPUT
// change FIELDPUT to DBFIELDPUT
// maintain FIELDPUT using HB_FUNC_TRANSLATE
HB_FUNC(FIELDPUT)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    HB_USHORT uiIndex = static_cast<HB_FIELDNO>(hb_parni(1)); // TODO: cast != type

    if (uiIndex > 0)
    {
      auto pItem = hb_param(2, Harbour::Item::ANY);
      if (pItem && !HB_IS_NIL(pItem))
      {
        if (SELF_PUTVALUE(pArea, uiIndex, pItem) == Harbour::SUCCESS)
        {
          hb_itemReturn(pItem);
        }
      }
    }
  }
}

// NOTES FOR HARBOUR++ V2:
// deprecate FLOCK
// change FLOCK to DBFLOCK
// maintain FLOCK using HB_FUNC_TRANSLATE
HB_FUNC(FLOCK)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBLOCKINFO dbLockInfo;
    dbLockInfo.fResult = false;
    dbLockInfo.itmRecID = nullptr;
    dbLockInfo.uiMethod = DBLM_FILE;
    SELF_LOCK(pArea, &dbLockInfo);
    hb_retl(dbLockInfo.fResult);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

// NOTES FOR HARBOUR++ V2:
// deprecate FOUND
// change FOUND to DBFOUND
// maintain FOUND using HB_FUNC_TRANSLATOR
HB_FUNC(FOUND)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  HB_BOOL bFound = false;
  if (pArea != nullptr)
  {
    SELF_FOUND(pArea, &bFound);
  }
  hb_retl(bFound);
}

// NOTES FOR HARBOUR++ V2:
// deprecate HEADER
// change HEADER to DBHEADER
// maintain HEADER using HB_FUNC_TRANSLATE
HB_FUNC(HEADER)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (!pArea)
  {
    hb_retni(0);
  }
  else
  {
    auto pItem = hb_itemNew(nullptr);
    SELF_INFO(pArea, DBI_GETHEADERSIZE, pItem);
    hb_itemReturnRelease(pItem);
  }
}

// NOTES FOR HARBOUR++ V2:
// deprecate INDEXORD
// change INDEXORD to DBINDEXORD
// maintain INDEXORD using HB_FUNC_TRANSLATE
HB_FUNC(INDEXORD)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO pInfo{};
    pInfo.itmResult = hb_itemPutNI(nullptr, 0);
    SELF_ORDINFO(pArea, DBOI_NUMBER, &pInfo);
    hb_retni(hb_itemGetNI(pInfo.itmResult));
    hb_itemRelease(pInfo.itmResult);
  }
  else
  {
    hb_retni(0);
  }
}

// NOTES FOR HARBOUR++ V2:
// deprecate LASTREC
// change LASTREC to DBLASTREC
// maintain LASTREC using HB_FUNC_TRANSLATE
HB_FUNC(LASTREC)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  HB_ULONG ulRecCount = 0;
  if (pArea != nullptr)
  {
    SELF_RECCOUNT(pArea, &ulRecCount);
  }

  hb_retnint(ulRecCount);
}

HB_FUNC(LOCK)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBLOCKINFO dbLockInfo;
    dbLockInfo.fResult = false;
    dbLockInfo.itmRecID = nullptr;
    dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
    SELF_LOCK(pArea, &dbLockInfo);
    hb_retl(dbLockInfo.fResult);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

// NOTES FOR HARBOUR++ V2:
// deprecate LUPDATE
// change LUPDATE to DBLUPDATE
// maintain LUPDATE using HB_FUNC_TRANSLATE
HB_FUNC(LUPDATE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto pItem = hb_itemNew(nullptr);
    SELF_INFO(pArea, DBI_LASTUPDATE, pItem);
    hb_itemReturnRelease(pItem);
  }
  else
  {
    hb_retds(nullptr);
  }
}

// NOTES FOR HARBOUR++ V2:
// deprecate NETERR
// change NETERR to DBNETERR
// maintain NETERR using HB_FUNC_TRANSLATE
HB_FUNC(NETERR)
{
  hb_retl(hb_rddGetNetErr());

  if (HB_ISLOG(1))
  {
    hb_rddSetNetErr(hb_parl(1));
  }
}

HB_FUNC(ORDBAGEXT)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  DBORDERINFO pInfo{};
  pInfo.itmResult = hb_itemPutC(nullptr, nullptr);
  if (!pArea)
  {
    HB_USHORT uiRddID;
    auto pRddNode = hb_rddFindNode(hb_rddDefaultDrv(nullptr), &uiRddID);
    if (pRddNode)
    {
      pArea = static_cast<AREAP>(hb_rddNewAreaNode(pRddNode, uiRddID));
      if (pArea != nullptr)
      {
        SELF_ORDINFO(pArea, DBOI_BAGEXT, &pInfo);
        SELF_RELEASE(pArea);
      }
    }
  }
  else
  {
    SELF_ORDINFO(pArea, DBOI_BAGEXT, &pInfo);
  }
  hb_itemReturnRelease(pInfo.itmResult);
}

HB_FUNC(ORDBAGNAME)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmOrder = hb_param(1, Harbour::Item::ANY);
    if (pOrderInfo.itmOrder && !HB_IS_STRING(pOrderInfo.itmOrder))
    {
      if (HB_IS_NIL(pOrderInfo.itmOrder))
      {
        pOrderInfo.itmOrder = nullptr;
      }
      else if (HB_IS_NUMERIC(pOrderInfo.itmOrder))
      {
        if (hb_itemGetNI(pOrderInfo.itmOrder) == 0)
        {
          pOrderInfo.itmOrder = nullptr;
        }
      }
      else
      {
        hb_errRT_DBCMD(EG_ARG, EDBCMD_ORD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
        return;
      }
    }
    pOrderInfo.itmResult = hb_itemPutC(nullptr, nullptr);
    SELF_ORDINFO(pArea, DBOI_BAGNAME, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDCONDSET)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto lpdbOrdCondInfo = static_cast<LPDBORDERCONDINFO>(hb_xgrab(sizeof(DBORDERCONDINFO)));
    lpdbOrdCondInfo->abFor = hb_parclen(1) > 0 ? hb_strdup(hb_parc(1)) : nullptr;
    auto pItem = hb_param(2, Harbour::Item::BLOCK);
    lpdbOrdCondInfo->itmCobFor = pItem ? hb_itemNew(pItem) : nullptr;

    lpdbOrdCondInfo->fAll = hb_parldef(3, true);

    lpdbOrdCondInfo->abWhile = hb_parclen(17) > 0 ? hb_strdup(hb_parc(17)) : nullptr;
    pItem = hb_param(4, Harbour::Item::BLOCK);
    lpdbOrdCondInfo->itmCobWhile = pItem ? hb_itemNew(pItem) : nullptr;

    pItem = hb_param(5, Harbour::Item::BLOCK);
    lpdbOrdCondInfo->itmCobEval = pItem ? hb_itemNew(pItem) : nullptr;

    lpdbOrdCondInfo->lStep = hb_parnl(6);
    lpdbOrdCondInfo->itmStartRecID = HB_ISNIL(7) ? nullptr : hb_itemNew(hb_param(7, Harbour::Item::ANY));
    lpdbOrdCondInfo->lNextCount = hb_parnl(8);
    lpdbOrdCondInfo->itmRecID = HB_ISNIL(9) ? nullptr : hb_itemNew(hb_param(9, Harbour::Item::ANY));
    lpdbOrdCondInfo->fRest = hb_parl(10);
    lpdbOrdCondInfo->fDescending = hb_parl(11);
    /* 12th parameter is always nil in CL5.3, in CL5.2 it's compound flag */
    lpdbOrdCondInfo->fCompound = hb_parl(12);
    lpdbOrdCondInfo->fAdditive = hb_parl(13);
    lpdbOrdCondInfo->fUseCurrent = hb_parl(14);
    lpdbOrdCondInfo->fCustom = hb_parl(15);
    lpdbOrdCondInfo->fNoOptimize = hb_parl(16);
    /* 18th parameter in [x]Harbour is MEMORY flag added by Alexander for
       DBFNTX, so far it was served in hacked way inside SELF_ORDSETCOND()
       so it was working only if this method was called from ordCondSet()
       function. I also do not like the idea that it was called MEMORY.
       It should be RDD decision how such index will be served on low
       level and it should be IMHO called TEMPORARY - if RDD wants then
       it can make it fully in memory or in temporary file which will
       be removed on index close operation */
    lpdbOrdCondInfo->fTemporary = hb_parl(18);
    /* 19th parameter is CL5.2 USEFILTER parameter which means
       that RDD should respect SET FILTER and SET DELETED flag */
    lpdbOrdCondInfo->fUseFilter = hb_parl(19);
    /* 20th parameter is Harbour extension and informs RDD that
       index is not shared between other clients */
    lpdbOrdCondInfo->fExclusive = hb_parl(20);

    if (lpdbOrdCondInfo->itmCobWhile)
    {
      lpdbOrdCondInfo->fRest = true;
    }
    if (lpdbOrdCondInfo->lNextCount || lpdbOrdCondInfo->itmRecID || lpdbOrdCondInfo->fRest ||
        lpdbOrdCondInfo->fUseCurrent || lpdbOrdCondInfo->fUseFilter)
    {
      lpdbOrdCondInfo->fAll = false;
    }

    lpdbOrdCondInfo->fActive = !lpdbOrdCondInfo->fAll || lpdbOrdCondInfo->abFor || lpdbOrdCondInfo->itmCobFor ||
                               lpdbOrdCondInfo->abWhile || lpdbOrdCondInfo->itmCobWhile ||
                               lpdbOrdCondInfo->fNoOptimize || lpdbOrdCondInfo->itmCobEval ||
                               lpdbOrdCondInfo->fTemporary;

    lpdbOrdCondInfo->fScoped = !lpdbOrdCondInfo->fAll;
    lpdbOrdCondInfo->lpvCargo = nullptr;

    hb_retl(SELF_ORDSETCOND(pArea, lpdbOrdCondInfo) == Harbour::SUCCESS);
  }
  else
  {
    hb_retl(false);
  }
}

HB_FUNC(ORDCREATE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERCREATEINFO dbOrderInfo;
    dbOrderInfo.lpdbOrdCondInfo = pArea->lpdbOrdCondInfo;
    dbOrderInfo.abBagName = hb_parcx(1);
    dbOrderInfo.atomBagName = hb_parcx(2);
    dbOrderInfo.itmOrder = nullptr;
    dbOrderInfo.fUnique = HB_ISLOG(5) ? static_cast<HB_BOOL>(hb_parl(5)) : hb_setGetUnique(); // TODO: cast to HB_BOOL ?
    dbOrderInfo.abExpr = hb_param(3, Harbour::Item::STRING);
    if (((dbOrderInfo.abBagName == nullptr || dbOrderInfo.abBagName[0] == 0) &&
         (dbOrderInfo.atomBagName == nullptr || dbOrderInfo.atomBagName[0] == 0)) ||
        !dbOrderInfo.abExpr)
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_ORD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      return;
    }
    dbOrderInfo.itmCobExpr = hb_param(4, Harbour::Item::BLOCK);

    DBCONSTRAINTINFO dbConstrInfo;
    dbConstrInfo.abConstrName = hb_parc(6);
    dbConstrInfo.abTargetName = hb_parc(7);
    dbConstrInfo.itmRelationKey = hb_param(8, Harbour::Item::ARRAY);
    if (dbConstrInfo.abConstrName && dbConstrInfo.abTargetName && dbConstrInfo.itmRelationKey)
    {
      dbConstrInfo.fEnabled = hb_parl(9);
      dbOrderInfo.lpdbConstraintInfo = &dbConstrInfo;
    }
    else
    {
      dbOrderInfo.lpdbConstraintInfo = nullptr;
    }

    SELF_ORDCREATE(pArea, &dbOrderInfo);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDBAGCLEAR)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.atomBagName = hb_param(1, Harbour::Item::STRING | Harbour::Item::NUMERIC);
    hb_retl(SELF_ORDLSTDELETE(pArea, &pOrderInfo) == Harbour::SUCCESS);
  }
  else
  {
    hb_retl(false);
  }
}

HB_FUNC(ORDDESTROY)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmOrder = hb_param(1, Harbour::Item::STRING | Harbour::Item::NUMERIC);
    pOrderInfo.atomBagName = hb_param(2, Harbour::Item::STRING);
    hb_retl(SELF_ORDDESTROY(pArea, &pOrderInfo) == Harbour::SUCCESS);
  }
  else
  {
    hb_retl(false);
  }
}

HB_FUNC(ORDFOR)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO pOrderInfo;
    pOrderInfo.itmOrder = hb_param(1, Harbour::Item::ANY);
    if (pOrderInfo.itmOrder && !HB_IS_STRING(pOrderInfo.itmOrder))
    {
      if (HB_IS_NIL(pOrderInfo.itmOrder))
      {
        pOrderInfo.itmOrder = nullptr;
      }
      else if (HB_IS_NUMERIC(pOrderInfo.itmOrder))
      {
        if (hb_itemGetNI(pOrderInfo.itmOrder) == 0)
        {
          pOrderInfo.itmOrder = nullptr;
        }
      }
      else
      {
        hb_errRT_DBCMD(EG_ARG, EDBCMD_ORD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
        return;
      }
    }
    pOrderInfo.atomBagName = hb_param(2, Harbour::Item::STRING);
    pOrderInfo.itmNewVal = hb_param(3, Harbour::Item::STRING);
    pOrderInfo.itmResult = hb_itemPutC(nullptr, nullptr);
    pOrderInfo.itmCobExpr = nullptr;
    pOrderInfo.fAllTags = false;
    SELF_ORDINFO(pArea, DBOI_CONDITION, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDKEY)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmOrder = hb_param(1, Harbour::Item::ANY);
    if (pOrderInfo.itmOrder && !HB_IS_STRING(pOrderInfo.itmOrder))
    {
      if (HB_IS_NIL(pOrderInfo.itmOrder))
      {
        pOrderInfo.itmOrder = nullptr;
      }
      else if (HB_IS_NUMERIC(pOrderInfo.itmOrder))
      {
        if (hb_itemGetNI(pOrderInfo.itmOrder) == 0)
        {
          pOrderInfo.itmOrder = nullptr;
        }
      }
      else
      {
        hb_errRT_DBCMD(EG_ARG, EDBCMD_ORD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
        return;
      }
    }
    pOrderInfo.atomBagName = hb_param(2, Harbour::Item::STRING);
    pOrderInfo.itmResult = hb_itemPutC(nullptr, nullptr);
    SELF_ORDINFO(pArea, DBOI_EXPRESSION, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDLISTADD)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO pOrderInfo{};

    /* Clipper clears NETERR flag when index is open */
    hb_rddSetNetErr(false);

    pOrderInfo.atomBagName = hb_param(1, Harbour::Item::STRING);
    pOrderInfo.itmOrder = hb_param(2, Harbour::Item::STRING);

    if (!pOrderInfo.atomBagName)
    {
      if (!HB_ISNIL(1))
      {
        hb_errRT_DBCMD(EG_ARG, EDBCMD_ORDLSTADD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      }
      return;
    }

    pOrderInfo.itmResult = hb_itemNew(nullptr);

    HB_ERRCODE errCode = SELF_ORDLSTADD(pArea, &pOrderInfo);

    if (!pOrderInfo.itmResult || HB_IS_NIL(pOrderInfo.itmResult))
    {
      hb_retl(errCode == Harbour::SUCCESS);
    }
    else
    {
      hb_itemReturn(pOrderInfo.itmResult);
    }

    hb_itemRelease(pOrderInfo.itmResult);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDLISTCLEAR)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    SELF_ORDLSTCLEAR(pArea);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDLISTREBUILD)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    SELF_ORDLSTREBUILD(pArea);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDNAME)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmOrder = hb_param(1, Harbour::Item::ANY);
    if (pOrderInfo.itmOrder)
    {
      if (HB_IS_NIL(pOrderInfo.itmOrder))
      {
        pOrderInfo.itmOrder = nullptr;
      }
      else if (HB_IS_NUMERIC(pOrderInfo.itmOrder))
      {
        if (hb_itemGetNI(pOrderInfo.itmOrder) == 0)
        {
          pOrderInfo.itmOrder = nullptr;
        }
      }
      else
      {
        hb_errRT_DBCMD(EG_ARG, EDBCMD_ORD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
        return;
      }
    }

    pOrderInfo.atomBagName = hb_param(2, Harbour::Item::STRING);
    pOrderInfo.itmResult = hb_itemPutC(nullptr, nullptr);
    SELF_ORDINFO(pArea, DBOI_NAME, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDNUMBER)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmOrder = hb_param(1, Harbour::Item::STRING);
    pOrderInfo.atomBagName = hb_param(2, Harbour::Item::STRING);
    if (!(pOrderInfo.itmOrder || HB_ISNIL(1)) || !(pOrderInfo.atomBagName || HB_ISNIL(2)))
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_ORD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      return;
    }
    pOrderInfo.itmResult = hb_itemPutNI(nullptr, 0);
    SELF_ORDINFO(pArea, DBOI_NUMBER, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDSETFOCUS)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO pInfo{};
    pInfo.itmOrder = hb_param(1, Harbour::Item::STRING | Harbour::Item::NUMERIC);
    pInfo.atomBagName = hb_param(2, Harbour::Item::STRING);
    pInfo.itmResult = hb_itemPutC(nullptr, nullptr);
    SELF_ORDLSTFOCUS(pArea, &pInfo);
    hb_itemReturnRelease(pInfo.itmResult);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(RDDLIST)
{
  hb_itemReturnRelease(hb_rddList(static_cast<HB_USHORT>(hb_parni(1))));
}

HB_FUNC(RDDNAME)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    char szRddName[HB_RDD_MAX_DRIVERNAME_LEN + 1];
    SELF_SYSNAME(pArea, szRddName);
    hb_retc(szRddName);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(RDDREGISTER)
{
  auto uiLen = static_cast<HB_USHORT>(hb_parclen(1));

  if (uiLen > 0)
  {
    char szDriver[HB_RDD_MAX_DRIVERNAME_LEN + 1];

    if (uiLen > HB_RDD_MAX_DRIVERNAME_LEN)
    {
      uiLen = HB_RDD_MAX_DRIVERNAME_LEN;
    }

    hb_strncpyUpper(szDriver, hb_parc(1), uiLen);
    /*
     * hb_rddRegister returns:
     *
     * 0: Ok, RDD registered
     * 1: RDD already registerd
     * > 1: error
     */
    if (hb_rddRegister(szDriver, static_cast<HB_USHORT>(hb_parni(2))) > 1)
    {
      hb_errInternal(HB_EI_RDDINVALID, nullptr, nullptr, nullptr);
    }
  }
}

/* Same as LastRec() */
HB_FUNC_TRANSLATE(RECCOUNT, LASTREC)

// NOTES FOR HARBOUR++ V2:
// deprecate RECNO
// change RECNO to DBRECNO
// maintain RECNO using HB_FUNC_TRANSLATE
HB_FUNC(RECNO)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto pRecNo = hb_itemPutNL(nullptr, 0);

  if (pArea != nullptr)
  {
    SELF_RECID(pArea, pRecNo);
  }
  hb_itemReturnRelease(pRecNo);
}

// NOTES FOR HARBOUR++ V2:
// deprecate RECSIZE
// change RECSIZE to DBRECSIZE
// maintain RECSIZE using HB_FUNC_TRANSLATE
HB_FUNC(RECSIZE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto pItem = hb_itemNew(nullptr);
    SELF_INFO(pArea, DBI_GETRECSIZE, pItem);
    hb_itemReturnRelease(pItem);
  }
  else
  {
    hb_retni(0);
  }
}

// NOTES FOR HARBOUR++ V2:
// deprecate RLOCK
// join with DBRLOCK
// maintain RLOCK using HB_FUNC_TRANSLATE
HB_FUNC(RLOCK)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBLOCKINFO dbLockInfo;
    dbLockInfo.fResult = false;
    dbLockInfo.itmRecID = nullptr;
    dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
    SELF_LOCK(pArea, &dbLockInfo);
    hb_retl(dbLockInfo.fResult);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

// NOTES FOR HARBOUR++ V2:
// deprecate SELECT
// change SELECT to DBSELECT
// maintain SELECT using HB_FUNC_TRANSLATE
HB_FUNC(SELECT)
{
  if (hb_pcount() == 0)
  {
    hb_retni(hb_rddGetCurrentWorkAreaNumber());
  }
  else
  {
    auto szAlias = hb_parc(1);
    auto iArea = 0;

    if (szAlias != nullptr)
    {
#if defined(HB_CLP_STRICT) || 1
      /*
       * I do not like this Clipper behavior, in some constructions
       * programmer may use "<aliasNum>" in some others not. [Druzus]
       */
      if (hb_rddVerifyAliasName(szAlias) == Harbour::SUCCESS)
#endif
        hb_rddGetAliasNumber(szAlias, &iArea);
    }
    else
    {
      auto pItem = hb_param(1, Harbour::Item::SYMBOL);
      if (pItem)
      {
        PHB_SYMB pSymAlias = hb_itemGetSymbol(pItem);
        if (pSymAlias->pDynSym)
        {
          iArea = static_cast<int>(hb_dynsymAreaHandle(pSymAlias->pDynSym));
        }
      }
    }
    hb_retni(iArea);
  }
}

// NOTES FOR HARBOUR++ V2:
// deprecate USED
// change USED to DBUSED
// maintain USED using HB_FUNC_TRANSLATE
HB_FUNC(USED)
{
  hb_retl(hb_rddGetCurrentWorkAreaPointer() != nullptr);
}

HB_FUNC(RDDSETDEFAULT)
{
  hb_retc(hb_rddDefaultDrv(nullptr));

  if (hb_parclen(1) > 0)
  {
    if (!hb_rddDefaultDrv(hb_parc(1)))
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    }
  }
}

HB_FUNC(DBSETDRIVER)
{
  hb_retc(hb_rddDefaultDrv(nullptr));

  if (hb_parclen(1) > 0)
  {
    if (!hb_rddDefaultDrv(hb_parc(1)))
    {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    }
  }
}

HB_FUNC(ORDSCOPE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    HB_USHORT uiAction;
    auto iScope = hb_parni(1);
    DBORDERINFO pInfo{};
    pInfo.itmResult = hb_itemNew(nullptr);
    if (iScope == 2)
    {
      if (hb_pcount() > 1 && !HB_ISNIL(2))
      {
        uiAction = DBOI_SCOPESET;
        pInfo.itmNewVal = hb_param(2, Harbour::Item::ANY);
      }
      else
      {
        uiAction = DBOI_SCOPECLEAR;
      }
    }
    else
    {
      uiAction = (iScope == 0) ? DBOI_SCOPETOP : DBOI_SCOPEBOTTOM;
      if (hb_pcount() > 1)
      {
        if (HB_ISNIL(2))
        {
          uiAction = (iScope == 0) ? DBOI_SCOPETOPCLEAR : DBOI_SCOPEBOTTOMCLEAR;
        }
        else
        {
          pInfo.itmNewVal = hb_param(2, Harbour::Item::ANY);
        }
      }
    }
    SELF_ORDINFO(pArea, uiAction, &pInfo);
    hb_itemReturnRelease(pInfo.itmResult);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBRELATION) /* (<nRelation>) --> cLinkExp */
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto pRelExpr = hb_itemPutC(nullptr, nullptr);
    auto uiRelNo = static_cast<HB_USHORT>(hb_parni(1));
    SELF_RELTEXT(pArea, uiRelNo ? uiRelNo : 1, pRelExpr);
    hb_itemReturnRelease(pRelExpr);
  }
  else
  {
    hb_retc_null();
  }
}

HB_FUNC(DBRSELECT) /* (<nRelation>) --> nWorkArea */
{
  auto uiRelation = static_cast<HB_USHORT>(hb_parni(1));
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  HB_USHORT uiWorkArea = 0;
  if (pArea != nullptr)
  {
    /* undocumented CA-Cl*pper behavior:
     * When parameter is missing, wrong or 0 then 1 is used as
     * relation number [druzus]
     */
    SELF_RELAREA(pArea, uiRelation ? uiRelation : 1, &uiWorkArea);
  }

  hb_retni(uiWorkArea);
}

HB_FUNC(DBCLEARRELATION)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    SELF_CLEARREL(pArea);
  }
}

HB_FUNC(DBSETRELATION)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto iArea = hb_rddGetCurrentWorkAreaNumber();
    auto szAlias = hb_parc(1);
    auto pBlock = hb_param(2, Harbour::Item::BLOCK);
    auto pText = hb_param(3, Harbour::Item::STRING);
    AREAP pChildArea = nullptr;

    if (szAlias != nullptr)
    {
      if (hb_rddSelectWorkAreaAlias(szAlias) == HB_SUCCESS)
      {
        pChildArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
      }
      if (hb_vmRequestQuery())
      {
        return;
      }
      hb_rddSelectWorkAreaNumber(iArea);
    }
    else
    {
      pChildArea = static_cast<AREAP>(hb_rddGetWorkAreaPointer(hb_parni(1)));
    }

    if (pArea == pChildArea)
      hb_errRT_DBCMD(EG_ARG, EDBCMD_REL_SAMEALIAS, nullptr, HB_ERR_FUNCNAME);
#ifdef HB_CLP_STRICT
    else if (!pChildArea || !pBlock)
#else
    else if (!pChildArea || !(pBlock || hb_itemGetCLen(pText) > 0))
#endif
      hb_errRT_DBCMD(EG_ARG, EDBCMD_REL_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    else
    {
      DBRELINFO dbRelations{};

      dbRelations.itmCobExpr = hb_itemNew(pBlock);
      dbRelations.abKey = hb_itemNew(pText);
      dbRelations.isScoped = hb_parl(4);
      dbRelations.isOptimized = false;
      dbRelations.lpaChild = pChildArea;
      dbRelations.lpaParent = pArea;
      dbRelations.lpdbriNext = nullptr;

      SELF_SETREL(pArea, &dbRelations);
    }
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME);
  }
}

/* __dbArrange( nToArea, aStruct, bFor, bWhile, nNext, nRecord, lRest, aFields ) */
HB_FUNC(__DBARRANGE)
{
  HB_ERRCODE errCode = Harbour::FAILURE;

  auto pSrcArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto pDstArea = static_cast<AREAP>(hb_rddGetWorkAreaPointer(static_cast<HB_AREANO>(hb_parni(1))));

  /* TODO: check what Clipper does when pDstArea == nullptr or pSrcArea == pDstArea */
  if (pSrcArea && pDstArea && pSrcArea != pDstArea)
  {
    DBSORTINFO dbSortInfo{};
    /* structure with fields copied copied from source WorkArea */
    auto pStruct = hb_param(2, Harbour::Item::ARRAY);
    /* array with sorted fields in source WorkArea */
    auto pFields = hb_param(8, Harbour::Item::ARRAY);

    errCode = hb_dbTransStruct(pSrcArea, pDstArea, &dbSortInfo.dbtri, nullptr, pStruct);
    if (errCode == Harbour::SUCCESS)
    {
      dbSortInfo.dbtri.dbsci.itmCobFor = hb_param(3, Harbour::Item::BLOCK);
      dbSortInfo.dbtri.dbsci.lpstrFor = nullptr;
      dbSortInfo.dbtri.dbsci.itmCobWhile = hb_param(4, Harbour::Item::BLOCK);
      dbSortInfo.dbtri.dbsci.lpstrWhile = nullptr;
      dbSortInfo.dbtri.dbsci.lNext = hb_param(5, Harbour::Item::NUMERIC);
      dbSortInfo.dbtri.dbsci.itmRecID = HB_ISNIL(6) ? nullptr : hb_param(6, Harbour::Item::ANY);
      dbSortInfo.dbtri.dbsci.fRest = hb_param(7, Harbour::Item::LOGICAL);

      dbSortInfo.dbtri.dbsci.fIgnoreFilter = dbSortInfo.dbtri.dbsci.fLast = dbSortInfo.dbtri.dbsci.fIgnoreDuplicates =
          dbSortInfo.dbtri.dbsci.fBackward = dbSortInfo.dbtri.dbsci.fOptimized = false;
      dbSortInfo.dbtri.dbsci.fIncludeDeleted = true;

      /* do not transfer record deleted flag to destination area */
      dbSortInfo.dbtri.uiFlags |= DBTF_RECALL;

      dbSortInfo.uiItemCount = pFields ? static_cast<HB_USHORT>(hb_arrayLen(pFields)) : 0;
      if (dbSortInfo.uiItemCount > 0)
      {
        HB_USHORT uiCount, uiDest;
        HB_SIZE nSize = 0;

        dbSortInfo.lpdbsItem = static_cast<LPDBSORTITEM>(hb_xgrab(dbSortInfo.uiItemCount * sizeof(DBSORTITEM)));
        for (uiCount = 1; uiCount <= dbSortInfo.uiItemCount; ++uiCount)
        {
          HB_SIZE nLine = hb_arrayGetCLen(pFields, uiCount);
          if (nLine > nSize)
          {
            nSize = nLine;
          }
        }
        auto szFieldLine = static_cast<char *>(hb_xgrab(nSize + 1));
        for (uiDest = 0, uiCount = 1; uiCount <= dbSortInfo.uiItemCount; ++uiCount)
        {
          char *szPos;
          dbSortInfo.lpdbsItem[uiDest].uiFlags = 0;
          hb_strncpyUpper(szFieldLine, hb_arrayGetCPtr(pFields, uiCount), hb_arrayGetCLen(pFields, uiCount));
          szPos = strchr(szFieldLine, '/');
          if (szPos != nullptr)
          {
            *szPos++ = 0;
            /* It's not Cl*pper compatible, Cl*pper checks only
               for /D flag and ignores any /A flags [druzus] */
            if (strchr(szPos, 'D') > strchr(szPos, 'A'))
            {
              dbSortInfo.lpdbsItem[uiDest].uiFlags |= SF_DESCEND;
            }
            else
            {
              dbSortInfo.lpdbsItem[uiDest].uiFlags |= SF_ASCEND;
            }
            if (strchr(szPos, 'C') != nullptr)
            {
              dbSortInfo.lpdbsItem[uiDest].uiFlags |= SF_CASE;
            }
          }
          else
          {
            dbSortInfo.lpdbsItem[uiDest].uiFlags |= SF_ASCEND;
          }

          /* Cl*pper sorts records using field values from source
             area only, destination area may not contain sorted
             fields at all [druzus] */
          dbSortInfo.lpdbsItem[uiDest].uiField = hb_rddFieldExpIndex(pSrcArea, szFieldLine);
          /* Field found */
          if (dbSortInfo.lpdbsItem[uiDest].uiField != 0)
          {
            ++uiDest;
          }
        }
        dbSortInfo.uiItemCount = uiDest;
        hb_xfree(szFieldLine);
      }

      PHB_ITEM pTransItm = hb_dbTransInfoPut(nullptr, &dbSortInfo.dbtri);
      errCode = SELF_INFO(dbSortInfo.dbtri.lpaDest, DBI_TRANSREC, pTransItm);
      if (errCode == Harbour::SUCCESS)
      {
        errCode = dbSortInfo.dbtri.uiItemCount == 0
                      ? Harbour::FAILURE
                      : (dbSortInfo.uiItemCount == 0 ? SELF_TRANS(pSrcArea, &dbSortInfo.dbtri)
                                                     : SELF_SORT(pSrcArea, &dbSortInfo));
        SELF_INFO(dbSortInfo.dbtri.lpaDest, DBI_TRANSREC, pTransItm);
        if (errCode == Harbour::SUCCESS && (dbSortInfo.dbtri.uiFlags & DBTF_CPYCTR))
        {
          errCode = hb_dbTransCounters(&dbSortInfo.dbtri);
        }
      }
      hb_itemRelease(pTransItm);
    }

    /* Free items */
    if (dbSortInfo.lpdbsItem)
    {
      hb_xfree(dbSortInfo.lpdbsItem);
    }
    if (dbSortInfo.dbtri.lpTransItems)
    {
      hb_xfree(dbSortInfo.dbtri.lpTransItems);
    }
  }

  hb_retl(errCode == Harbour::SUCCESS);
}

/* __dbTrans(nDstArea, aFieldsStru, bFor, bWhile, nNext, nRecord, lRest) --> <lSuccess> */
HB_FUNC(__DBTRANS)
{
  if (HB_ISNUM(1))
  {
    auto uiSrcArea = static_cast<HB_AREANO>(hb_rddGetCurrentWorkAreaNumber());
    auto pSrcArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
    auto uiDstArea = static_cast<HB_AREANO>(hb_parni(1));
    hb_rddSelectWorkAreaNumber(uiDstArea);
    auto pDstArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

    if (pSrcArea && pDstArea)
    {
      DBTRANSINFO dbTransInfo{};
      auto pFields = hb_param(2, Harbour::Item::ARRAY);

      HB_ERRCODE errCode = hb_dbTransStruct(pSrcArea, pDstArea, &dbTransInfo, nullptr, pFields);
      if (errCode == Harbour::SUCCESS)
      {
        hb_rddSelectWorkAreaNumber(dbTransInfo.lpaSource->uiArea);

        dbTransInfo.dbsci.itmCobFor = hb_param(3, Harbour::Item::BLOCK);
        dbTransInfo.dbsci.lpstrFor = nullptr;
        dbTransInfo.dbsci.itmCobWhile = hb_param(4, Harbour::Item::BLOCK);
        dbTransInfo.dbsci.lpstrWhile = nullptr;
        dbTransInfo.dbsci.lNext = hb_param(5, Harbour::Item::NUMERIC);
        dbTransInfo.dbsci.itmRecID = HB_ISNIL(6) ? nullptr : hb_param(6, Harbour::Item::ANY);
        dbTransInfo.dbsci.fRest = hb_param(7, Harbour::Item::LOGICAL);

        dbTransInfo.dbsci.fIgnoreFilter = dbTransInfo.dbsci.fLast = dbTransInfo.dbsci.fIgnoreDuplicates =
            dbTransInfo.dbsci.fBackward = dbTransInfo.dbsci.fOptimized = false;
        dbTransInfo.dbsci.fIncludeDeleted = true;

        PHB_ITEM pTransItm = hb_dbTransInfoPut(nullptr, &dbTransInfo);
        errCode = SELF_INFO(dbTransInfo.lpaDest, DBI_TRANSREC, pTransItm);
        if (errCode == Harbour::SUCCESS)
        {
          errCode = dbTransInfo.uiItemCount == 0 ? Harbour::FAILURE : SELF_TRANS(dbTransInfo.lpaSource, &dbTransInfo);
          /* we always call DBI_TRANSREC second time after TRANS() method
           * even if TRANS() failed - it's for RDDs which may need to store
           * pointer to dbTransInfo in first call and then release it and/or
           * clean some structures allocated for transfer operation [druzus]
           */
          SELF_INFO(dbTransInfo.lpaDest, DBI_TRANSREC, pTransItm);
          if (errCode == Harbour::SUCCESS && (dbTransInfo.uiFlags & DBTF_CPYCTR))
          {
            errCode = hb_dbTransCounters(&dbTransInfo);
          }
        }
        hb_itemRelease(pTransItm);
      }

      if (dbTransInfo.lpTransItems)
      {
        hb_xfree(dbTransInfo.lpTransItems);
      }

      hb_retl(errCode == Harbour::SUCCESS);
    }
    else
    {
      hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
    }

    hb_rddSelectWorkAreaNumber(uiSrcArea);
  }
  else
  {
    hb_errRT_DBCMD(EG_ARG, EDBCMD_USE_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
  }
}

/* __dbApp(<cNameName>, [<aFields>], ;
           [<bFor>], [<bWhile>], [<nNext>], [<nRecord>], [<lRest>], ;
           [<cRDD>], [<nConnection>], [<cCodePage>], ;
           [<xDelimiter>]) --> <lSuccess> */
HB_FUNC(__DBAPP)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    hb_retl(Harbour::SUCCESS == hb_rddTransRecords(pArea, hb_parc(1),                   /* file name */
                                                   hb_parc(8),                          /* RDD */
                                                   hb_parnl(9),                         /* connection */
                                                   hb_param(2, Harbour::Item::ARRAY),   /* Fields */
                                                   false,                               /* Export? */
                                                   hb_param(3, Harbour::Item::BLOCK),   /* cobFor */
                                                   nullptr,                             /* lpStrFor */
                                                   hb_param(4, Harbour::Item::BLOCK),   /* cobWhile */
                                                   nullptr,                             /* lpStrWhile */
                                                   hb_param(5, Harbour::Item::NUMERIC), /* Next */
                                                   HB_ISNIL(6) ? nullptr : hb_param(6, Harbour::Item::ANY), /* RecID */
                                                   hb_param(7, Harbour::Item::LOGICAL),                     /* Rest */
                                                   hb_parc(10),                        /* Codepage */
                                                   hb_param(11, Harbour::Item::ANY))); /* Delimiter */
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, "APPEND FROM");
  }
}

/* __dbCoppy(<cNameName>, [<aFields>], ;
             [<bFor>], [<bWhile>], [<nNext>], [<nRecord>], [<lRest>], ;
             [<cRDD>], [<nConnection>], [<cCodePage>], ;
             [<xDelimiter>]) --> <lSuccess> */
HB_FUNC(__DBCOPY)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    hb_retl(Harbour::SUCCESS == hb_rddTransRecords(pArea, hb_parc(1),                   /* file name */
                                                   hb_parc(8),                          /* RDD */
                                                   hb_parnl(9),                         /* connection */
                                                   hb_param(2, Harbour::Item::ARRAY),   /* Fields */
                                                   true,                                /* Export? */
                                                   hb_param(3, Harbour::Item::BLOCK),   /* cobFor */
                                                   nullptr,                             /* lpStrFor */
                                                   hb_param(4, Harbour::Item::BLOCK),   /* cobWhile */
                                                   nullptr,                             /* lpStrWhile */
                                                   hb_param(5, Harbour::Item::NUMERIC), /* Next */
                                                   HB_ISNIL(6) ? nullptr : hb_param(6, Harbour::Item::ANY), /* RecID */
                                                   hb_param(7, Harbour::Item::LOGICAL),                     /* Rest */
                                                   hb_parc(10),                        /* Codepage */
                                                   hb_param(11, Harbour::Item::ANY))); /* Delimiter */
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, "COPY TO");
  }
}

HB_FUNC(HB_RDDGETTEMPALIAS)
{
  char szAliasTmp[HB_RDD_MAX_ALIAS_LEN + 1];

  if (hb_rddGetTempAlias(szAliasTmp) == Harbour::SUCCESS)
  {
    hb_retc(szAliasTmp);
  }
}

HB_FUNC(HB_RDDINFO)
{
  auto szDriver = hb_parc(3);
  if (!szDriver)
  { /* no VIA RDD parameter, use default */
    szDriver = hb_rddDefaultDrv(nullptr);
  }

  HB_ULONG ulConnection = hb_parnl(4);

  HB_USHORT uiRddID;
  auto pRDDNode = hb_rddFindNode(szDriver, &uiRddID); /* find the RDDNODE */
  auto pIndex = hb_param(1, Harbour::Item::NUMERIC);

  if (pRDDNode && pIndex)
  {
    PHB_ITEM pInfo = hb_itemParam(2);
    SELF_RDDINFO(pRDDNode, static_cast<HB_USHORT>(hb_itemGetNI(pIndex)), ulConnection, pInfo);
    hb_itemReturnRelease(pInfo);
  }
  else
  {
    hb_errRT_DBCMD(EG_ARG, EDBCMD_EVAL_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(HB_DBDROP)
{
  LPRDDNODE pRDDNode = nullptr;
  HB_ULONG ulConnection = hb_parnl(4);
  auto szName = hb_parc(1);

  if (szName != nullptr)
  {
    if (!szName[0])
    {
      szName = hb_parc(2);
    }
    auto szDriver = hb_rddFindDrv(hb_parc(3), szName);
    if (szDriver != nullptr)
    {
      pRDDNode = hb_rddFindNode(szDriver, nullptr); /* find the RDDNODE */
    }
  }

  if (pRDDNode)
  {
    hb_retl(SELF_DROP(pRDDNode, hb_param(1, Harbour::Item::STRING), hb_param(2, Harbour::Item::STRING), ulConnection) ==
            Harbour::SUCCESS);
  }
  else
  {
    hb_errRT_DBCMD(EG_ARG, EDBCMD_EVAL_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(HB_DBEXISTS)
{
  LPRDDNODE pRDDNode = nullptr;
  HB_ULONG ulConnection = hb_parnl(4);
  auto szName = hb_parc(1);

  if (szName != nullptr)
  {
    if (!szName[0])
    {
      szName = hb_parc(2);
    }
    auto szDriver = hb_rddFindDrv(hb_parc(3), szName);
    if (szDriver != nullptr)
    {
      pRDDNode = hb_rddFindNode(szDriver, nullptr); /* find the RDDNODE */
    }
  }
  if (pRDDNode)
  {
    hb_retl(SELF_EXISTS(pRDDNode, hb_param(1, Harbour::Item::STRING), hb_param(2, Harbour::Item::STRING),
                        ulConnection) == Harbour::SUCCESS);
  }
  else
  {
    hb_errRT_DBCMD(EG_ARG, EDBCMD_EVAL_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(HB_DBRENAME)
{
  LPRDDNODE pRDDNode = nullptr;
  HB_ULONG ulConnection = hb_parnl(5);
  auto szName = hb_parc(1);

  if (szName != nullptr)
  {
    if (!szName[0])
    {
      szName = hb_parc(2);
    }
    auto szDriver = hb_rddFindDrv(hb_parc(4), szName);
    if (szDriver != nullptr)
    {
      pRDDNode = hb_rddFindNode(szDriver, nullptr); /* find the RDDNODE */
    }
  }

  auto pTable = hb_param(1, Harbour::Item::STRING);
  auto pIndex = hb_param(2, Harbour::Item::STRING);
  auto pNewName = hb_param(3, Harbour::Item::STRING);
  if (pIndex && !pNewName)
  {
    pNewName = pIndex;
    pIndex = nullptr;
  }

  if (pRDDNode && pTable && pNewName)
  {
    hb_retl(SELF_RENAME(pRDDNode, pTable, pIndex, pNewName, ulConnection) == Harbour::SUCCESS);
  }
  else
  {
    hb_errRT_DBCMD(EG_ARG, EDBCMD_EVAL_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(HB_FIELDLEN)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto szField = hb_parc(1);

    HB_USHORT uiIndex;
    if (szField != nullptr)
    {
      uiIndex = hb_rddFieldIndex(pArea, szField);
    }
    else
    {
      uiIndex = static_cast<HB_FIELDNO>(hb_parni(1));
    }

    if (uiIndex > 0)
    {
      auto pItem = hb_itemNew(nullptr);

      if (SELF_FIELDINFO(pArea, uiIndex, DBS_LEN, pItem) == Harbour::SUCCESS)
      {
        hb_itemReturnRelease(pItem);
        return;
      }
      hb_itemRelease(pItem);
    }
  }

  hb_retni(0);
}

HB_FUNC(HB_FIELDDEC)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto szField = hb_parc(1);

    HB_USHORT uiIndex;
    if (szField != nullptr)
    {
      uiIndex = hb_rddFieldIndex(pArea, szField);
    }
    else
    {
      uiIndex = static_cast<HB_FIELDNO>(hb_parni(1));
    }

    if (uiIndex > 0)
    {
      auto pItem = hb_itemNew(nullptr);

      if (SELF_FIELDINFO(pArea, uiIndex, DBS_DEC, pItem) == Harbour::SUCCESS)
      {
        hb_itemReturnRelease(pItem);
        return;
      }
      hb_itemRelease(pItem);
    }
  }

  hb_retni(0);
}

HB_FUNC(HB_FIELDTYPE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto szField = hb_parc(1);

    HB_USHORT uiIndex;
    if (szField != nullptr)
    {
      uiIndex = hb_rddFieldIndex(pArea, szField);
    }
    else
    {
      uiIndex = static_cast<HB_FIELDNO>(hb_parni(1));
    }

    if (uiIndex > 0)
    {
      auto pItem = hb_itemNew(nullptr);

      if (SELF_FIELDINFO(pArea, uiIndex, DBS_TYPE, pItem) == Harbour::SUCCESS)
      {
        hb_itemReturnRelease(pItem);
        return;
      }
      hb_itemRelease(pItem);
    }
  }

  hb_retc_null();
}

HB_FUNC(HB_FIELDGET)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto szField = hb_parc(1);

    HB_USHORT uiField;
    if (szField != nullptr)
    {
      uiField = hb_rddFieldIndex(pArea, szField);
    }
    else
    {
      uiField = static_cast<HB_FIELDNO>(hb_parni(1));
    }

    if (uiField > 0)
    {
      auto pItem = hb_itemNew(nullptr);
      SELF_GETVALUE(pArea, uiField, pItem);
      hb_itemReturnRelease(pItem);
    }
  }
}

HB_FUNC(HB_FIELDPUT)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto szField = hb_parc(1);

    HB_USHORT uiField;
    if (szField != nullptr)
    {
      uiField = hb_rddFieldIndex(pArea, szField);
    }
    else
    {
      uiField = static_cast<HB_FIELDNO>(hb_parni(1));
    }

    if (uiField > 0)
    {
      auto pItem = hb_param(2, Harbour::Item::ANY);
      if (pItem)
      {
        if (SELF_PUTVALUE(pArea, uiField, pItem) == Harbour::SUCCESS)
        {
          hb_itemReturn(pItem);
        }
      }
    }
  }
}

HB_FUNC(HB_WAEVAL)
{
  auto pBlock = hb_param(1, Harbour::Item::BLOCK);

  if (pBlock)
  {
    hb_rddEvalWA(pBlock);
  }
  else
  {
    hb_errRT_DBCMD(EG_ARG, EDBCMD_USE_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
  }
}

#ifndef HB_CLP_STRICT

HB_FUNC(__DBSKIPPER)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    HB_LONG lSkipped = 0;
    HB_BOOL fBEof;
    HB_ULONG ulRecords = 0;

    if (SELF_RECCOUNT(pArea, &ulRecords) == Harbour::SUCCESS && ulRecords > 0)
    {
      HB_LONG lRecs = 1;

      if (HB_ISNUM(1))
      {
        lRecs = hb_parnl(1);
      }

      if (lRecs == 0)
      {
        SELF_SKIP(pArea, 0);
      }
      else if (lRecs > 0)
      {
        /* the condition below is exact Clipper behavior anyhow
         * we cannot replicate it without introducing serious problem:
         * some RDDs use non continuous record numbers (i.e. ADT) and
         * the condition: ulRecNo != ulRecords + 1 can be true also for
         * normal records not only for the phantom EOF record. [druzus]
         */
#if 0
            HB_ULONG ulRecNo = 0;
            if( SELF_RECNO(pArea, &ulRecNo) == Harbour::SUCCESS && ulRecNo != ulRecords + 1 )
#endif
        {
          while (lSkipped < lRecs)
          {
            if (SELF_SKIP(pArea, 1) != Harbour::SUCCESS)
            {
              break;
            }
            if (SELF_EOF(pArea, &fBEof) != Harbour::SUCCESS)
            {
              break;
            }
            if (fBEof)
            {
              SELF_SKIP(pArea, -1);
              break;
            }
            lSkipped++;
          }
        }
      }
      else /* if( lRecs < 0 ) */
      {
        while (lSkipped > lRecs)
        {
          if (SELF_SKIP(pArea, -1) != Harbour::SUCCESS)
          {
            break;
          }
          if (SELF_BOF(pArea, &fBEof) != Harbour::SUCCESS)
          {
            break;
          }
          if (fBEof)
          {
            break;
          }
          lSkipped--;
        }
      }
    }
    hb_retnl(lSkipped);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

#endif
