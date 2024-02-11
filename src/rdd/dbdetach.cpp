/*
 * .prg functions for workarea detaching
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbapiitm.hpp"
#include "hbapierr.hpp"
#include "hbthread.hpp"

/*
 * hb_dbDetach( [<nWorkArea>|<cAlias>], [<xCargo>] ) --> <lSuccess>
 */
HB_FUNC(HB_DBDETACH)
{
  auto pAlias = hb_param(1, Harbour::Item::ANY);
  auto pCargo = hb_param(2, Harbour::Item::ANY); /* Harbour::Item::BLOCK in Xbase++ */
  AREAP pArea = nullptr;
  int iArea;

  if (!pAlias || HB_IS_NIL(pAlias))
  {
    pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  }
  else if (HB_IS_STRING(pAlias))
  {
    auto szAlias = hb_itemGetCPtr(pAlias);
    hb_rddGetAliasNumber(szAlias, &iArea);
    if (iArea > 0)
    {
      pArea = static_cast<AREAP>(hb_rddGetWorkAreaPointer(iArea));
    }
  }
  else if (HB_IS_NUMBER(pAlias))
  {
    iArea = hb_itemGetNI(pAlias);
    if (iArea > 0)
    {
      pArea = static_cast<AREAP>(hb_rddGetWorkAreaPointer(iArea));
    }
  }
  else
  {
    hb_errRT_DBCMD(EG_ARG, EDBCMD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    return;
  }

  if (pArea != nullptr)
  {
    hb_retl(hb_rddDetachArea(pArea, pCargo) == Harbour::SUCCESS);
  }
  else
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

/*
 * hb_dbRequest([<cAlias>], [<lFreeArea>], [<@xCargo>], [<nTimeOut>|<lWait>])
 *          --> <lSuccess>
 */
HB_FUNC(HB_DBREQUEST)
{
  if (HB_ISNIL(1) || HB_ISCHAR(1))
  {
    auto szAlias = hb_parc(1);
    bool fNewArea = hb_parl(2);
    PHB_ITEM pCargo = HB_ISBYREF(3) ? hb_itemNew(nullptr) : nullptr;
    HB_ULONG ulMilliSec = HB_THREAD_INFINITE_WAIT;

    if (HB_ISNUM(4))
    {
      double dTimeOut = hb_parnd(4);
      ulMilliSec = dTimeOut > 0 ? static_cast<HB_ULONG>(dTimeOut * 1000) : 0;
    }
    else if (!hb_parl(4))
    {
      ulMilliSec = 0;
    }

    auto pArea = hb_rddRequestArea(szAlias, pCargo, fNewArea, ulMilliSec);
    if (pArea != nullptr)
    {
      hb_rddSelectWorkAreaNumber(pArea->uiArea);
    }

    if (pCargo)
    {
      hb_itemParamStoreForward(3, pCargo);
      hb_itemRelease(pCargo);
    }

    hb_retl(pArea != nullptr);
  }
  else
  {
    hb_errRT_DBCMD(EG_ARG, EDBCMD_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
  }
}
