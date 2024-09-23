//
// SIX compatible functions:
//       sx_GetLocks()
//       sx_IsFLocked()
//       sx_IsReadonly()
//       sx_IsShared()
//       sx_IDType()
//       sx_TableType()
//       sx_TableName()
//       sx_Rollback()
//       sx_Rlock()
//       sx_Unlock()
//       sx_SetPass()
//       sx_DbfEncrypt()
//       sx_DbfDecrypt()
//       sx_MemoPack()
//       sx_SetTurbo()
//       sx_TurboArea()
//       _sxOpenInit() (internal function used by _sx_IniInit())
//
// Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapifs.hpp"
#include "hbapirdd.hpp"
#include "hbapierr.hpp"

HB_FUNC(SX_GETLOCKS)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto pList = hb_itemArrayNew(0);
    SELF_INFO(pArea, DBI_GETLOCKARRAY, pList);
    hb_itemReturnRelease(pList);
  }
}

HB_FUNC(SX_ISFLOCKED)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fLocked = false;

  if (pArea != nullptr)
  {
    auto pItem = hb_itemNew(nullptr);
    SELF_INFO(pArea, DBI_ISFLOCK, pItem);
    fLocked = hb_itemGetL(pItem);
    hb_itemRelease(pItem);
  }

  hb_retl(fLocked);
}

HB_FUNC(SX_ISREADONLY)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fReadOnly = false;

  if (pArea != nullptr)
  {
    auto pItem = hb_itemNew(nullptr);
    SELF_INFO(pArea, DBI_ISREADONLY, pItem);
    fReadOnly = hb_itemGetL(pItem);
    hb_itemRelease(pItem);
  }

  hb_retl(fReadOnly);
}

HB_FUNC(SX_ISSHARED)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fShared = false;

  if (pArea != nullptr)
  {
    auto pItem = hb_itemNew(nullptr);
    SELF_INFO(pArea, DBI_SHARED, pItem);
    fShared = hb_itemGetL(pItem);
    hb_itemRelease(pItem);
  }

  hb_retl(fShared);
}

HB_FUNC(SX_IDTYPE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  int iType = 0;

  if (pArea != nullptr)
  {
    auto pItem = hb_itemNew(nullptr);
    if (SELF_RECINFO(pArea, nullptr, DBRI_ENCRYPTED, pItem) == Harbour::SUCCESS)
    {
      iType = hb_itemGetL(pItem) ? 2 : 1;
    }
    hb_itemRelease(pItem);
  }

  hb_retni(iType);
}

HB_FUNC(SX_TABLETYPE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  int iType = 0;

  if (pArea != nullptr)
  {
    auto pItem = hb_itemNew(nullptr);
    if (SELF_INFO(pArea, DBI_ISENCRYPTED, pItem) == Harbour::SUCCESS)
    {
      iType = hb_itemGetL(pItem) ? 2 : 1;
    }
    hb_itemRelease(pItem);
  }

  hb_retni(iType);
}

HB_FUNC(SX_TABLENAME)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto pList = hb_itemNew(nullptr);
    SELF_INFO(pArea, DBI_FULLPATH, pList);
    hb_itemReturnRelease(pList);
  }
  else
  {
    hb_retc_null();
  }
}

static void hb_sxRollBackChild(AREAP pArea, PHB_ITEM pItem)
{
  LPDBRELINFO lpdbRelation = pArea->lpdbRelations;

  while (lpdbRelation)
  {
    if (SELF_INFO(lpdbRelation->lpaChild, DBI_ROLLBACK, pItem) != Harbour::SUCCESS)
    {
      break;
    }
    hb_sxRollBackChild(lpdbRelation->lpaChild, pItem);
    lpdbRelation = lpdbRelation->lpdbriNext;
  }
}

HB_FUNC(SX_ROLLBACK)
{
  auto fResult = false;
  auto fRollChild = false;
  int iArea = 0;
  AREAP pArea;

  if (HB_ISNUM(1))
  {
    iArea = hb_parni(1);
    fRollChild = iArea == 0;
  }

  if (iArea)
  {
    pArea = static_cast<AREAP>(hb_rddGetWorkAreaPointer(iArea));
  }
  else
  {
    pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  }

  if (pArea != nullptr)
  {
    auto pItem = hb_itemNew(nullptr);
    fResult = SELF_INFO(pArea, DBI_ROLLBACK, pItem) == Harbour::SUCCESS;
    if (fResult && fRollChild)
    {
      hb_sxRollBackChild(pArea, pItem);
    }
    hb_itemRelease(pItem);
  }

  hb_retl(fResult);
}

HB_FUNC(SX_RLOCK)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fResult = false;
  PHB_ITEM pResult = nullptr;

  if (pArea != nullptr)
  {
    auto pRecords = hb_param(1, Harbour::Item::ARRAY);
    DBLOCKINFO dbLockInfo;
    dbLockInfo.fResult = false;
    dbLockInfo.uiMethod = DBLM_MULTIPLE;
    if (pRecords)
    {
      HB_SIZE nLen = hb_arrayLen(pRecords);
      pResult = hb_itemArrayNew(nLen);
      for (HB_SIZE nPos = 1; nPos <= nLen; ++nPos)
      {
        dbLockInfo.itmRecID = hb_arrayGetItemPtr(pRecords, nPos);
        SELF_LOCK(pArea, &dbLockInfo);
        hb_arraySetL(pResult, nPos, dbLockInfo.fResult);
      }
    }
    else
    {
      dbLockInfo.itmRecID = hb_param(1, Harbour::Item::ANY);
      SELF_LOCK(pArea, &dbLockInfo);
      fResult = dbLockInfo.fResult;
    }
  }

  if (pResult)
  {
    hb_itemReturnRelease(pResult);
  }
  else
  {
    hb_retl(fResult);
  }
}

HB_FUNC(SX_UNLOCK)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    auto pRecords = hb_param(1, Harbour::Item::ARRAY);
    if (pRecords)
    {
      HB_SIZE nLen = hb_arrayLen(pRecords);
      for (HB_SIZE nPos = 1; nPos <= nLen; ++nPos)
      {
        SELF_UNLOCK(pArea, hb_arrayGetItemPtr(pRecords, nPos));
      }
    }
    else
    {
      SELF_UNLOCK(pArea, hb_param(1, Harbour::Item::ANY));
    }
  }
}

HB_FUNC(SX_SETPASS)
{
  auto iPCount = hb_pcount();
  auto fResult = false;
  PHB_ITEM pItem;

  if (iPCount == 1)
  {
    if (HB_ISCHAR(1))
    {
      auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
      if (pArea != nullptr)
      {
        pItem = hb_itemParam(1);
        if (SELF_INFO(pArea, DBI_PASSWORD, pItem) == Harbour::SUCCESS)
        {
          fResult = true;
        }
        hb_itemRelease(pItem);
      }
    }
  }
  else if (iPCount >= 2 && iPCount <= 4)
  {
    if (HB_ISCHAR(1) && HB_ISNUM(2) && (iPCount < 3 || HB_ISCHAR(3)) && (iPCount < 4 || HB_ISNUM(4)))
    {
      /* Set pending password for table which will be open
       * 3rd and 4th parameters are optional Harbour extensions
       * with RDD name and connection number.
       */
      LPRDDNODE pRDDNode;
      HB_USHORT uiRddID;
      const char *szDriver;

      if (iPCount == 2)
      { /* no RDD parameter, use default */
        szDriver = hb_rddDefaultDrv(nullptr);
      }
      else
      {
        szDriver = hb_parc(3);
      }
      pRDDNode = hb_rddFindNode(szDriver, &uiRddID); /* find the RDDNODE */
      if (pRDDNode)
      {
        pItem = hb_itemParam(1);
        if (SELF_RDDINFO(pRDDNode, RDDI_PENDINGPASSWORD, hb_parnl(4), pItem) == Harbour::SUCCESS)
        {
          fResult = true;
        }
        hb_itemRelease(pItem);
      }
    }
    else if (iPCount == 2 && HB_ISNUM(1) && HB_ISCHAR(2))
    {
      auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
      if (pArea != nullptr)
      {
        /* Undocumented SIX3 extension */
        switch (hb_parni(1))
        {
        case 1: /* return current password key in raw form */
          pItem = hb_itemNew(nullptr);
          if (SELF_INFO(pArea, DBI_PASSWORD, pItem) == Harbour::SUCCESS)
          {
            hb_itemReturn(pItem);
          }
          hb_itemRelease(pItem);
          break;
        case 2: /* set raw password key */
          /* not implemented */
          break;
        case 3: /* mark table as encrypted */
          /* intentionally not implemented */
          break;
        case 4: /* mark table as decrypted */
          /* intentionally not implemented */
          break;
        }
        return;
      }
    }
  }

  hb_retl(fResult);
}

HB_FUNC(SX_DBFENCRYPT)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fResult = false;

  if (pArea != nullptr)
  {
    PHB_ITEM pItem = hb_itemParam(1);

    if (SELF_INFO(pArea, DBI_ENCRYPT, pItem) == Harbour::SUCCESS)
    {
      fResult = hb_itemGetL(pItem);
    }
    hb_itemRelease(pItem);
  }
  hb_retl(fResult);
}

HB_FUNC(SX_DBFDECRYPT)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fResult = false;

  if (pArea != nullptr)
  {
    PHB_ITEM pItem = hb_itemParam(1);
    if (SELF_INFO(pArea, DBI_DECRYPT, pItem) == Harbour::SUCCESS)
    {
      fResult = hb_itemGetL(pItem);
    }
    hb_itemRelease(pItem);
  }
  hb_retl(fResult);
}

HB_FUNC(SX_MEMOPACK)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fResult = false;

  if (pArea != nullptr)
  {
    auto pItem = hb_itemArrayNew(3);
    auto iPCount = hb_pcount();
    for (auto i = 1; i <= iPCount; ++i)
    {
      hb_arraySet(pItem, i, hb_param(i, Harbour::Item::ANY));
    }
    fResult = SELF_INFO(pArea, DBI_MEMOPACK, pItem) == Harbour::SUCCESS;
    hb_itemRelease(pItem);
  }
  hb_retl(fResult);
}

HB_FUNC(SX_TURBOAREA)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    PHB_ITEM pItem = hb_itemParam(1);
    if (hb_pcount() > 0 && pItem->isNil())
    {
      hb_itemPutNI(pItem, 0);
    }
    if (SELF_INFO(pArea, DBI_DIRTYREAD, pItem) != Harbour::SUCCESS)
    {
      hb_itemPutL(pItem, false);
    }
    hb_itemReturnRelease(pItem);
  }
  else
  {
    hb_retl(false);
  }
}

HB_FUNC(SX_SETTURBO)
{
  LPRDDNODE pRDDNode;
  HB_USHORT uiRddID;

  auto szDriver = hb_parc(2);
  if (!szDriver)
  { /* no VIA RDD parameter, use default */
    szDriver = hb_rddDefaultDrv(nullptr);
  }

  pRDDNode = hb_rddFindNode(szDriver, &uiRddID); /* find the RDDNODE */
  if (!pRDDNode)
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
  else
  {
    PHB_ITEM pItem = hb_itemParam(1);
    if (hb_pcount() > 0 && pItem->isNil())
    {
      hb_itemPutNI(pItem, 0);
    }
    if (SELF_RDDINFO(pRDDNode, RDDI_DIRTYREAD, 0, pItem) != Harbour::SUCCESS)
    {
      hb_itemPutL(pItem, false);
    }
    hb_itemReturnRelease(pItem);
  }
}

/*
 * _sxOpenInit(nArea, xShared, xReadOnly, xAlias)
 */
HB_FUNC(_SXOPENINIT)
{
  AREAP pArea = nullptr;
  auto iArea = hb_parni(1);

  if (iArea)
  {
    pArea = static_cast<AREAP>(hb_rddGetWorkAreaPointer(iArea));
  }

  if (pArea != nullptr)
  {
    LPDBOPENINFO pInfo = nullptr;
    auto pItem = hb_itemNew(nullptr);

    if (SELF_INFO(pArea, DBI_OPENINFO, pItem))
    {
      pInfo = static_cast<LPDBOPENINFO>(hb_itemGetPtr(pItem));
    }
    hb_itemRelease(pItem);
    if (pInfo)
    {
      if (HB_ISLOG(2))
      {
        pInfo->fShared = hb_parl(2);
      }
      if (HB_ISLOG(3))
      {
        pInfo->fReadonly = hb_parl(2);
      }
      if (HB_ISCHAR(4))
      {
        auto szAlias = hb_parc(1);
        if (szAlias != nullptr && szAlias[0])
        {
          pInfo->atomAlias = hb_dynsymName(hb_dynsymGet(szAlias));
        }
        else
        {
          pInfo->atomAlias = "";
        }
      }
    }
  }
}
