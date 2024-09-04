//
// SIX compatible function:
//       sx_TagOrder() *
//       sx_TagNo()
//       sx_Freeze()
//       sx_Warm()
//       sx_Chill()
//       sx_Thermometer()
//       sx_ClrScope()
//       sx_SetScope()
//       sx_IsReindex()
//       sx_Step()
//       sx_KeySincluded()
//       sx_I_IndexName()
//       sx_I_TagName()
//       sx_IndexCount()
//       sx_IndexName()
//       sx_IndexType()
//       sx_KeyAdd()
//       sx_KeyDrop()
//       sx_KeyData()
//       sx_KeySkip()
//       sx_KeyCount()
//       sx_KeyNo()
//       sx_KeyGoto()
//       sx_SkipUnique()
//       sx_SeekLast()
//       sx_TagUnique()
//       sx_WildSeek()
//       sx_ROXLock()
//       sx_ROXUnlock()
//       sx_IsMyROX()
//       sx_IsROXLock()
//       sx_SortOption()
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

#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapifs.hpp"
#include "hbapirdd.hpp"

static bool hb_sxOrdParam(LPDBORDERINFO pInfo)
{
  memset(pInfo, 0, sizeof(DBORDERINFO));

  if (HB_ISCHAR(1))
  {
    pInfo->itmOrder = hb_param(1, Harbour::Item::STRING);
    pInfo->atomBagName = hb_param(2, Harbour::Item::STRING);
  }
  else if (HB_ISNUM(1))
  {
    pInfo->itmOrder = hb_param(1, Harbour::Item::NUMERIC);
    if (!HB_ISNIL(2))
    { /* hb_pcount() > 2 */
      pInfo->atomBagName = hb_param(2, Harbour::Item::NUMERIC);
      if (hb_parni(2) <= 0)
      {
        return false;
      }
    }
  }
  return true;
}

HB_FUNC(SX_TAGORDER)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  int iOrder = 0;

  if (pArea != nullptr)
  {
    DBORDERINFO Info;

    if (hb_sxOrdParam(&Info))
    {
      Info.itmResult = hb_itemPutNI(nullptr, 0);
      SELF_ORDINFO(pArea, DBOI_NUMBER, &Info);
      iOrder = hb_itemGetNI(Info.itmResult);
      hb_itemRelease(Info.itmResult);
    }
  }

  hb_retni(iOrder);
}

/*
 * sx_TagNo(tag, bag) --> nTagPosInBag
 * returns order position in order bag
 */
HB_FUNC(SX_TAGNO)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  int iBagOrder = 0;

  if (pArea != nullptr)
  {
    DBORDERINFO Info;

    if (hb_sxOrdParam(&Info))
    {
      Info.itmResult = hb_itemPutNI(nullptr, 0);
      if (SELF_ORDINFO(pArea, DBOI_NUMBER, &Info) == Harbour::SUCCESS)
      {
        int iOrder = hb_itemGetNI(Info.itmResult);
        if (iOrder)
        {
          Info.itmOrder = hb_itemPutNI(nullptr, iOrder);
          Info.atomBagName = nullptr;
          hb_itemClear(Info.itmResult);
          if (SELF_ORDINFO(pArea, DBOI_FULLPATH, &Info) == Harbour::SUCCESS && hb_itemGetCLen(Info.itmResult) > 0)
          {
            Info.atomBagName = Info.itmResult;
            Info.itmResult = Info.itmOrder;
            Info.itmOrder = nullptr;
            hb_itemClear(Info.itmResult);
            if (SELF_ORDINFO(pArea, DBOI_BAGORDER, &Info) == Harbour::SUCCESS)
            {
              iBagOrder = iOrder - hb_itemGetNI(Info.itmResult) + 1;
            }
            Info.itmOrder = Info.atomBagName;
          }
          hb_itemRelease(Info.itmOrder);
        }
      }
      hb_itemRelease(Info.itmResult);
    }
  }

  hb_retni(iBagOrder);
}

HB_FUNC(SX_FREEZE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO Info;

    if (hb_sxOrdParam(&Info))
    {
      auto fResult = false;
      Info.itmNewVal = hb_itemPutL(nullptr, true);
      Info.itmResult = hb_itemNew(nullptr);
      if (SELF_ORDINFO(pArea, DBOI_CUSTOM, &Info) == Harbour::SUCCESS)
      {
        fResult = HB_IS_LOGICAL(Info.itmResult) && hb_itemGetL(Info.itmResult);
      }
      hb_itemRelease(Info.itmNewVal);
      hb_itemRelease(Info.itmResult);
      hb_retl(fResult);
    }
  }
}

HB_FUNC(SX_WARM)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO Info;

    if (hb_sxOrdParam(&Info))
    {
      auto fResult = false;
      Info.itmNewVal = hb_itemPutL(nullptr, false);
      Info.itmResult = hb_itemNew(nullptr);
      if (SELF_ORDINFO(pArea, DBOI_CHGONLY, &Info) == Harbour::SUCCESS)
      {
        fResult = HB_IS_LOGICAL(Info.itmResult) && !hb_itemGetL(Info.itmResult);
      }
      hb_itemRelease(Info.itmNewVal);
      hb_itemRelease(Info.itmResult);
      hb_retl(fResult);
    }
  }
}

HB_FUNC(SX_CHILL)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO Info;

    if (hb_sxOrdParam(&Info))
    {
      auto fResult = false;
      Info.itmNewVal = hb_itemPutL(nullptr, true);
      Info.itmResult = hb_itemNew(nullptr);
      if (SELF_ORDINFO(pArea, DBOI_CHGONLY, &Info) == Harbour::SUCCESS)
      {
        fResult = HB_IS_LOGICAL(Info.itmResult) && hb_itemGetL(Info.itmResult);
      }
      hb_itemRelease(Info.itmNewVal);
      hb_itemRelease(Info.itmResult);
      hb_retl(fResult);
    }
  }
}

/*
 * 1 - Full Update
 * 2 - Full Update (partial index)
 * 3 - Changes Only
 * 4 - No Update
 * -1 - not table or no order
 */
HB_FUNC(SX_THERMOMETER)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  int iTemperature = -1;

  if (pArea != nullptr)
  {
    DBORDERINFO Info;

    if (hb_sxOrdParam(&Info))
    {
      int i;
      Info.itmResult = hb_itemPutNI(nullptr, 0);
      SELF_ORDINFO(pArea, DBOI_NUMBER, &Info);
      i = hb_itemGetNI(Info.itmResult);
      if (i)
      {
        static const HB_USHORT s_iStates[] = {DBOI_CUSTOM, DBOI_CHGONLY, DBOI_PARTIAL};
        iTemperature = 4;
        for (i = 0; i < 3; ++i, --iTemperature)
        {
          hb_itemClear(Info.itmResult);
          if (SELF_ORDINFO(pArea, s_iStates[i], &Info) == Harbour::SUCCESS && HB_IS_LOGICAL(Info.itmResult) &&
              hb_itemGetL(Info.itmResult))
          {
            break;
          }
        }
      }
      hb_itemRelease(Info.itmResult);
    }
  }

  hb_retni(iTemperature);
}

HB_FUNC(SX_CLRSCOPE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO Info;

    if (hb_sxOrdParam(&Info))
    {
      auto iScope = hb_parnidef(1, 2);
      Info.itmResult = hb_itemNew(nullptr);
      if (iScope)
      {
        SELF_ORDINFO(pArea, DBOI_SCOPEBOTTOMCLEAR, &Info);
      }
      if (iScope == 0 || iScope == 2)
      {
        SELF_ORDINFO(pArea, DBOI_SCOPETOPCLEAR, &Info);
      }
      hb_itemRelease(Info.itmResult);
    }
  }
}

HB_FUNC(SX_SETSCOPE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO Info;

    if (hb_sxOrdParam(&Info))
    {
      auto iScope = hb_parni(1);
      Info.itmResult = hb_itemNew(nullptr);
      if (!HB_ISNIL(2))
      {
        Info.itmNewVal = hb_param(2, Harbour::Item::ANY);
      }
      SELF_ORDINFO(pArea, static_cast<HB_USHORT>(iScope ? DBOI_SCOPEBOTTOM : DBOI_SCOPETOP), &Info);
      hb_itemReturnRelease(Info.itmResult);
    }
  }
}

HB_FUNC(SX_ISREINDEX)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fReindex = false;

  if (pArea != nullptr)
  {
    DBORDERINFO Info{};
    Info.itmResult = hb_itemNew(nullptr);
    SELF_ORDINFO(pArea, DBOI_ISREINDEX, &Info);
    fReindex = hb_itemGetL(Info.itmResult);
    hb_itemRelease(Info.itmResult);
  }

  hb_retl(fReindex);
}

HB_FUNC(SX_STEP)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  HB_LONG lStep = 0;

  if (pArea != nullptr)
  {
    DBORDERINFO Info{};
    Info.itmResult = hb_itemNew(nullptr);
    SELF_ORDINFO(pArea, DBOI_EVALSTEP, &Info);
    lStep = hb_itemGetNL(Info.itmResult);
    hb_itemRelease(Info.itmResult);
  }

  hb_retnint(lStep);
}

HB_FUNC(SX_KEYSINCLUDED)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  HB_ULONG ulKeys = 0;

  if (pArea != nullptr)
  {
    DBORDERINFO Info{};
    Info.itmResult = hb_itemNew(nullptr);
    SELF_ORDINFO(pArea, DBOI_KEYSINCLUDED, &Info);
    ulKeys = hb_itemGetNL(Info.itmResult);
    hb_itemRelease(Info.itmResult);
  }

  hb_retnint(ulKeys);
}

HB_FUNC(SX_I_INDEXNAME)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO Info{};
    Info.itmResult = hb_itemNew(nullptr);
    SELF_ORDINFO(pArea, DBOI_I_BAGNAME, &Info);
    hb_itemReturnRelease(Info.itmResult);
    return;
  }

  hb_retc_null();
}

HB_FUNC(SX_I_TAGNAME)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO Info{};
    Info.itmResult = hb_itemNew(nullptr);
    SELF_ORDINFO(pArea, DBOI_I_TAGNAME, &Info);
    hb_itemReturnRelease(Info.itmResult);
    return;
  }

  hb_retc_null();
}

HB_FUNC(SX_INDEXCOUNT)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  int iCount = 0;

  if (pArea != nullptr)
  {
    DBORDERINFO Info{};
    Info.itmResult = hb_itemNew(nullptr);
    SELF_ORDINFO(pArea, DBOI_BAGCOUNT, &Info);
    iCount = hb_itemGetNI(Info.itmResult);
    hb_itemRelease(Info.itmResult);
  }

  hb_retni(iCount);
}

HB_FUNC(SX_INDEXNAME)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO Info;
    if (hb_sxOrdParam(&Info))
    {
      Info.itmResult = hb_itemNew(nullptr);
      SELF_ORDINFO(pArea, DBOI_FULLPATH, &Info);
      hb_itemReturnRelease(Info.itmResult);
    }
    else
    {
      hb_retc_null();
    }
  }
}

HB_FUNC(SX_INDEXTYPE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  int iType = DBOI_TYPE_UNDEF;

  if (pArea != nullptr)
  {
    DBORDERINFO Info;
    if (hb_sxOrdParam(&Info))
    {
      if (hb_pcount() == 1 && HB_ISCHAR(1))
      {
        Info.atomBagName = Info.itmOrder;
        Info.itmOrder = nullptr;
      }
      Info.itmResult = hb_itemNew(nullptr);
      if (SELF_ORDINFO(pArea, DBOI_INDEXTYPE, &Info) == Harbour::SUCCESS)
      {
        iType = hb_itemGetNI(Info.itmResult);
      }
      hb_itemRelease(Info.itmResult);
    }
  }
  hb_retni(iType);
}

HB_FUNC(SX_DESCEND)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO Info;
    if (hb_sxOrdParam(&Info))
    {
      Info.itmResult = hb_itemNew(nullptr);
      if (SELF_ORDINFO(pArea, DBOI_ISDESC, &Info) == Harbour::SUCCESS)
      {
        Info.itmNewVal = hb_itemPutL(nullptr, !hb_itemGetL(Info.itmResult));
        SELF_ORDINFO(pArea, DBOI_ISDESC, &Info);
        hb_itemRelease(Info.itmNewVal);
      }
      hb_itemRelease(Info.itmResult);
    }
  }
}

HB_FUNC(SX_KEYADD)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fResult = false;

  if (pArea != nullptr)
  {
    DBORDERINFO Info;
    if (hb_sxOrdParam(&Info))
    {
      Info.itmResult = hb_itemPutL(nullptr, false);
      Info.itmNewVal = hb_param(3, Harbour::Item::ANY);
      SELF_ORDINFO(pArea, DBOI_KEYADD, &Info);
      fResult = hb_itemGetL(Info.itmResult);
      hb_itemRelease(Info.itmResult);
    }
  }
  hb_retl(fResult);
}

HB_FUNC(SX_KEYDROP)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fResult = false;

  if (pArea != nullptr)
  {
    DBORDERINFO Info;
    if (hb_sxOrdParam(&Info))
    {
      Info.itmResult = hb_itemPutL(nullptr, false);
      Info.itmNewVal = hb_param(3, Harbour::Item::ANY);
      SELF_ORDINFO(pArea, DBOI_KEYDELETE, &Info);
      fResult = hb_itemGetL(Info.itmResult);
      hb_itemRelease(Info.itmResult);
    }
  }
  hb_retl(fResult);
}

HB_FUNC(SX_KEYDATA)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO Info;
    if (hb_sxOrdParam(&Info))
    {
      Info.itmResult = hb_itemNew(nullptr);
      SELF_ORDINFO(pArea, DBOI_KEYVAL, &Info);
      hb_itemReturnRelease(Info.itmResult);
    }
  }
}

HB_FUNC(SX_KEYSKIP)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fResult = false;
  HB_BOOL fBEof = false;

  if (pArea != nullptr)
  {
    if (SELF_SKIPRAW(pArea, hb_parnldef(1, 1)) == Harbour::SUCCESS)
    {
      if (SELF_EOF(pArea, &fBEof) == Harbour::SUCCESS && !fBEof)
      {
        fResult = SELF_BOF(pArea, &fBEof) == Harbour::SUCCESS && !fBEof;
      }
    }
  }
  hb_retl(fResult);
}

HB_FUNC(SX_KEYCOUNT)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  HB_ULONG ulKeys = 0;

  if (pArea != nullptr)
  {
    DBORDERINFO Info;
    if (hb_sxOrdParam(&Info))
    {
      Info.itmResult = hb_itemNew(nullptr);
      SELF_ORDINFO(pArea, DBOI_KEYCOUNT, &Info);
      ulKeys = hb_itemGetNL(Info.itmResult);
      hb_itemRelease(Info.itmResult);
    }
  }

  hb_retnint(ulKeys);
}

HB_FUNC(SX_KEYNO)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  HB_ULONG ulKeyNo = 0;

  if (pArea != nullptr)
  {
    DBORDERINFO Info;
    if (hb_sxOrdParam(&Info))
    {
      Info.itmResult = hb_itemNew(nullptr);
      SELF_ORDINFO(pArea, DBOI_POSITION, &Info);
      ulKeyNo = hb_itemGetNL(Info.itmResult);
      hb_itemRelease(Info.itmResult);
    }
  }

  hb_retnint(ulKeyNo);
}

HB_FUNC(SX_KEYGOTO)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fResult = false;

  if (pArea && hb_parnl(3) != 0)
  {
    DBORDERINFO Info;
    if (hb_sxOrdParam(&Info))
    {
      Info.itmNewVal = hb_param(3, Harbour::Item::NUMERIC);
      Info.itmResult = hb_itemNew(nullptr);
      SELF_ORDINFO(pArea, DBOI_POSITION, &Info);
      fResult = hb_itemGetL(Info.itmResult);
      hb_itemRelease(Info.itmResult);
    }
  }

  hb_retl(fResult);
}

HB_FUNC(SX_SKIPUNIQUE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO Info{};
    Info.itmNewVal = hb_param(1, Harbour::Item::ANY);
    Info.itmResult = hb_itemNew(nullptr);
    SELF_ORDINFO(pArea, DBOI_SKIPUNIQUE, &Info);
    hb_itemRelease(Info.itmResult);
  }
}

HB_FUNC(SX_SEEKLAST)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  HB_BOOL fFound = false;

  if (pArea && hb_pcount() > 0)
  {
    auto pKey = hb_param(1, Harbour::Item::ANY);
    bool bSoftSeek = hb_parl(2);

    if (SELF_SEEK(pArea, bSoftSeek, pKey, true) == Harbour::SUCCESS)
    {
      if (SELF_FOUND(pArea, &fFound) != Harbour::SUCCESS)
      {
        fFound = false;
      }
    }
  }
  hb_retl(fFound);
}

HB_FUNC(SX_TAGUNIQUE)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO Info;
    if (hb_sxOrdParam(&Info))
    {
      Info.itmResult = hb_itemPutL(nullptr, false);
      SELF_ORDINFO(pArea, DBOI_UNIQUE, &Info);
      hb_itemReturnRelease(Info.itmResult);
    }
  }
}

HB_FUNC(SX_WILDSEEK)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto szPattern = hb_parc(1);
  bool fCont = hb_parl(2);
  auto fFound = false;

  if (pArea != nullptr)
  {
    int iOrder = 0;

    DBORDERINFO Info{};
    Info.itmResult = hb_itemNew(nullptr);

    if (szPattern != nullptr && szPattern[0])
    {
      if (SELF_ORDINFO(pArea, DBOI_NUMBER, &Info) == Harbour::SUCCESS)
      {
        iOrder = hb_itemGetNI(Info.itmResult);
      }
    }
    if (iOrder > 0)
    {
      HB_ERRCODE errCode = Harbour::SUCCESS;
      if (!fCont)
      {
        errCode = SELF_GOTOP(pArea);
        if (errCode == Harbour::SUCCESS)
        {
          errCode = SELF_ORDINFO(pArea, DBOI_KEYVAL, &Info);
          if (errCode == Harbour::SUCCESS)
          {
            auto szKey = hb_itemGetCPtr(Info.itmResult);
            fFound = hb_strMatchWild(szKey, szPattern);
          }
        }
      }
      if (!fFound && errCode == Harbour::SUCCESS)
      {
        Info.itmNewVal = hb_param(1, Harbour::Item::STRING);
        if (SELF_ORDINFO(pArea, DBOI_SKIPWILD, &Info) == Harbour::SUCCESS)
        {
          fFound = HB_IS_LOGICAL(Info.itmResult) && hb_itemGetL(Info.itmResult);
        }
      }
    }
    else
    {
      SELF_GOTO(pArea, 0);
    }
    hb_itemRelease(Info.itmResult);
  }

  hb_retl(fFound);
}

HB_FUNC(SX_ROXLOCK)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fLocked = false;

  if (pArea != nullptr)
  {
    DBORDERINFO Info;
    if (hb_sxOrdParam(&Info))
    {
      Info.itmNewVal = hb_itemPutL(nullptr, true);
      Info.itmResult = hb_itemPutL(nullptr, false);
      if (SELF_ORDINFO(pArea, DBOI_READLOCK, &Info) == Harbour::SUCCESS)
      {
        fLocked = hb_itemGetL(Info.itmResult);
      }
      hb_itemRelease(Info.itmNewVal);
      hb_itemRelease(Info.itmResult);
    }
  }
  hb_retl(fLocked);
}

HB_FUNC(SX_ROXUNLOCK)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  if (pArea != nullptr)
  {
    DBORDERINFO Info;
    if (hb_sxOrdParam(&Info))
    {
      Info.itmNewVal = hb_itemPutL(nullptr, false);
      Info.itmResult = hb_itemPutL(nullptr, false);
      SELF_ORDINFO(pArea, DBOI_READLOCK, &Info);
      hb_itemRelease(Info.itmNewVal);
      hb_itemRelease(Info.itmResult);
    }
  }
}

HB_FUNC(SX_ISMYROX)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fLocked = false;

  if (pArea != nullptr)
  {
    DBORDERINFO Info;
    if (hb_sxOrdParam(&Info))
    {
      Info.itmResult = hb_itemNew(nullptr);
      if (SELF_ORDINFO(pArea, DBOI_READLOCK, &Info) == Harbour::SUCCESS)
      {
        fLocked = hb_itemGetL(Info.itmResult);
      }
      hb_itemRelease(Info.itmResult);
    }
  }
  hb_retl(fLocked);
}

HB_FUNC(SX_ISROXLOCK)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fLocked = false;

  if (pArea != nullptr)
  {
    DBORDERINFO Info;
    if (hb_sxOrdParam(&Info))
    {
      Info.itmResult = hb_itemNew(nullptr);
      if (SELF_ORDINFO(pArea, DBOI_READLOCK, &Info) == Harbour::SUCCESS)
      {
        fLocked = hb_itemGetL(Info.itmResult);
      }
      if (!fLocked)
      {
        Info.itmNewVal = hb_itemPutL(nullptr, true);
        if (SELF_ORDINFO(pArea, DBOI_READLOCK, &Info) == Harbour::SUCCESS)
        {
          fLocked = hb_itemGetL(Info.itmResult);
        }
        if (fLocked)
        {
          hb_itemPutL(Info.itmNewVal, false);
          SELF_ORDINFO(pArea, DBOI_READLOCK, &Info);
        }
        hb_itemRelease(Info.itmNewVal);
      }
      hb_itemRelease(Info.itmResult);
    }
  }
  hb_retl(fLocked);
}

HB_FUNC(SX_SORTOPTION)
{
  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
  auto fUseCurrent = true;

  if (pArea != nullptr)
  {
    DBORDERINFO Info;
    if (hb_sxOrdParam(&Info))
    {
      Info.itmResult = hb_itemNew(nullptr);
      Info.itmNewVal = hb_param(1, Harbour::Item::LOGICAL);
      if (SELF_ORDINFO(pArea, DBOI_USECURRENT, &Info) == Harbour::SUCCESS)
      {
        fUseCurrent = hb_itemGetL(Info.itmResult);
      }
      hb_itemRelease(Info.itmResult);
    }
  }
  hb_retl(fUseCurrent);
}
