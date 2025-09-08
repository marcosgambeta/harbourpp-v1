//
// Base RDD module
//
// Copyright 1999 Bruno Cantero <bruno@issnet.net>
// Copyright 2004-2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
// Copyright 2002 Horacio Roldan <harbour_ar@yahoo.com.ar> (ordKeyVal(), ordKeyAdd(), ordKeyDel())
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

#include "hbapi.hpp"
#include "hbapirdd.hpp"
#include "hbapierr.hpp"
#include "hbapiitm.hpp"
#include "hbvm.hpp"
#include "hbset.hpp"

#ifdef HB_COMPAT_C53

HB_FUNC(ORDKEYCOUNT)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmOrder = hb_param(1, Harbour::Item::STRING | Harbour::Item::NUMERIC);
    pOrderInfo.atomBagName = hb_param(2, Harbour::Item::STRING);
    // Either or both may be NIL

    pOrderInfo.itmResult = hb_itemPutNL(nullptr, 0);
    SELF_ORDINFO(pArea, DBOI_KEYCOUNT, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDKEYNO)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmOrder = hb_param(1, Harbour::Item::STRING | Harbour::Item::NUMERIC);
    pOrderInfo.atomBagName = hb_param(2, Harbour::Item::STRING);
    // Either or both may be NIL
    pOrderInfo.itmNewVal = nullptr;
    pOrderInfo.itmResult = hb_itemPutNL(nullptr, 0);
    SELF_ORDINFO(pArea, DBOI_POSITION, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDKEYGOTO)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmNewVal = hb_param(1, Harbour::Item::NUMERIC);
    pOrderInfo.itmResult = hb_itemPutL(nullptr, false);
    SELF_ORDINFO(pArea, DBOI_POSITION, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDKEYRELPOS)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmNewVal = hb_param(1, Harbour::Item::NUMERIC);
    pOrderInfo.itmResult = hb_itemPutNI(nullptr, 0);
    SELF_ORDINFO(pArea, DBOI_RELKEYPOS, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDFINDREC)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmNewVal = hb_param(1, Harbour::Item::NUMERIC);
    pOrderInfo.itmResult = hb_itemPutL(nullptr, false);
    SELF_ORDINFO(pArea, hb_parl(2) ? DBOI_FINDRECCONT : DBOI_FINDREC, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDSKIPRAW)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    SELF_SKIPRAW(pArea, hb_parnldef(1, 1));
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDSKIPUNIQUE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmNewVal = hb_param(1, Harbour::Item::ANY);
    pOrderInfo.itmResult = hb_itemPutL(nullptr, false);
    SELF_ORDINFO(pArea, DBOI_SKIPUNIQUE, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDKEYVAL)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmResult = hb_itemNew(nullptr);
    SELF_ORDINFO(pArea, DBOI_KEYVAL, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDKEYADD)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmOrder = hb_param(1, Harbour::Item::STRING | Harbour::Item::NUMERIC);
    pOrderInfo.atomBagName = hb_param(2, Harbour::Item::STRING);
    // Either or both may be NIL
    pOrderInfo.itmNewVal = hb_param(3, Harbour::Item::ANY);
    pOrderInfo.itmResult = hb_itemPutNL(nullptr, 0);
    SELF_ORDINFO(pArea, DBOI_KEYADD, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDKEYDEL)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmOrder = hb_param(1, Harbour::Item::STRING | Harbour::Item::NUMERIC);
    pOrderInfo.atomBagName = hb_param(2, Harbour::Item::STRING);
    // Either or both may be NIL
    pOrderInfo.itmNewVal = hb_param(3, Harbour::Item::ANY);
    pOrderInfo.itmResult = hb_itemPutNL(nullptr, 0);
    SELF_ORDINFO(pArea, DBOI_KEYDELETE, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDDESCEND)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmOrder = hb_param(1, Harbour::Item::STRING | Harbour::Item::NUMERIC);
    pOrderInfo.atomBagName = hb_param(2, Harbour::Item::STRING);
    // Either or both may be NIL
    pOrderInfo.itmNewVal = hb_param(3, Harbour::Item::LOGICAL);
    pOrderInfo.itmResult = hb_itemPutL(nullptr, false);
    SELF_ORDINFO(pArea, DBOI_ISDESC, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDISUNIQUE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmOrder = hb_param(1, Harbour::Item::STRING | Harbour::Item::NUMERIC);
    pOrderInfo.atomBagName = hb_param(2, Harbour::Item::STRING);
    // Harbour extension: NewVal to set/reset unique flag
    pOrderInfo.itmNewVal = hb_param(3, Harbour::Item::LOGICAL);
    pOrderInfo.itmResult = hb_itemPutL(nullptr, false);
    SELF_ORDINFO(pArea, DBOI_UNIQUE, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(ORDCUSTOM)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    DBORDERINFO pOrderInfo{};
    pOrderInfo.itmOrder = hb_param(1, Harbour::Item::STRING | Harbour::Item::NUMERIC);
    pOrderInfo.atomBagName = hb_param(2, Harbour::Item::STRING);
    // Either or both may be NIL
    pOrderInfo.itmNewVal = hb_param(3, Harbour::Item::LOGICAL);
    pOrderInfo.itmResult = hb_itemPutL(nullptr, false);
    SELF_ORDINFO(pArea, DBOI_CUSTOM, &pOrderInfo);
    hb_itemReturnRelease(pOrderInfo.itmResult);
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBINFO)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    auto pIndex = hb_param(1, Harbour::Item::NUMERIC);
    if (pIndex) {
      PHB_ITEM pInfo = hb_itemParam(2);

      SELF_INFO(pArea, static_cast<HB_USHORT>(pIndex->getNI()), pInfo);
      hb_itemReturnRelease(pInfo);
    } else {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_DBINFOBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    }
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBORDERINFO)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    auto pType = hb_param(1, Harbour::Item::NUMERIC);
    if (pType) {
      DBORDERINFO pOrderInfo;

      // atomBagName may be NIL
      pOrderInfo.atomBagName = hb_param(2, Harbour::Item::STRING | Harbour::Item::NUMERIC);
      pOrderInfo.itmOrder = hb_param(3, Harbour::Item::STRING | Harbour::Item::NUMERIC);

      pOrderInfo.itmNewVal = hb_param(4, Harbour::Item::ANY);
      pOrderInfo.itmResult = hb_itemNew(nullptr);
      pOrderInfo.itmCobExpr = nullptr;
      pOrderInfo.fAllTags = false;
      SELF_ORDINFO(pArea, static_cast<HB_USHORT>(pType->getNI()), &pOrderInfo);
      hb_itemReturnRelease(pOrderInfo.itmResult);
    } else {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_DBCMDBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    }
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBFIELDINFO)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    HB_USHORT uiFields, uiIndex;
    auto pType = hb_param(1, Harbour::Item::NUMERIC);
    uiIndex = static_cast<HB_FIELDNO>(hb_parni(2));
    if (pType && SELF_FIELDCOUNT(pArea, &uiFields) == Harbour::SUCCESS && uiIndex > 0 && uiIndex <= uiFields) {
      auto pInfo = hb_itemNew(hb_param(3, Harbour::Item::ANY));

      SELF_FIELDINFO(pArea, uiIndex, static_cast<HB_USHORT>(pType->getNI()), pInfo);
      hb_itemReturnRelease(pInfo);
    } else {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_DBCMDBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    }
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

HB_FUNC(DBRECORDINFO)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    auto pType = hb_param(1, Harbour::Item::NUMERIC);
    auto pRecNo = hb_param(2, Harbour::Item::ANY);
    if (pType) {
      PHB_ITEM pInfo = hb_itemParam(3);

      SELF_RECINFO(pArea, pRecNo, static_cast<HB_USHORT>(pType->getNI()), pInfo);
      hb_itemReturnRelease(pInfo);
    } else
      hb_errRT_DBCMD(EG_ARG, EDBCMD_INFOBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

// dbFileGet()/Blob2File() - retrieve memo contents into file
HB_FUNC(DBFILEGET)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    HB_USHORT uiFields, uiIndex;
    auto szField = hb_parc(1);

    if (szField != nullptr) {
      uiIndex = hb_rddFieldIndex(pArea, szField);
    } else {
      uiIndex = static_cast<HB_FIELDNO>(hb_parni(1));
    }

    auto pMode = hb_param(3, Harbour::Item::NUMERIC);
    if (uiIndex > 0 && pMode && hb_parclen(2) > 0 && SELF_FIELDCOUNT(pArea, &uiFields) == Harbour::SUCCESS &&
        uiIndex <= uiFields) {
      hb_retl(SELF_GETVALUEFILE(pArea, uiIndex, hb_parc(2), static_cast<HB_USHORT>(pMode->getNI())) ==
              Harbour::SUCCESS);
    } else {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_DBFILEGETBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    }
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

// dbFilePut()/File2Blob() - store file contents in MEMO
HB_FUNC(DBFILEPUT)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr) {
    HB_USHORT uiFields, uiIndex;
    auto szField = hb_parc(1);

    if (szField != nullptr) {
      uiIndex = hb_rddFieldIndex(pArea, szField);
    } else {
      uiIndex = static_cast<HB_FIELDNO>(hb_parni(1));
    }
    if (uiIndex > 0 && hb_parclen(2) > 0 && SELF_FIELDCOUNT(pArea, &uiFields) == Harbour::SUCCESS &&
        uiIndex <= uiFields) {
      hb_retl(SELF_PUTVALUEFILE(pArea, uiIndex, hb_parc(2), static_cast<HB_USHORT>(hb_parni(3))) == Harbour::SUCCESS);
    } else {
      hb_errRT_DBCMD(EG_ARG, EDBCMD_DBFILEPUTBADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    }
  } else {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
  }
}

#endif
