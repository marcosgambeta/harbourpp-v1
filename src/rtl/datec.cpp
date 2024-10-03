//
// CMonth(), CDoW(), hb_CDay() functions
//
// Copyright 2014 Viktor Szakats (vszakats.net/harbour) (hb_CDay())
// Copyright 1999 Jose Lalin <dezac@corevia.com>
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
#include "hbapierr.hpp"
#include "hbapilng.hpp"
#include "hbdate.hpp"

#if defined(__CODEGUARD__)
static const char s_nullStr[4] = {0};
#else
#define s_nullStr ""
#endif

const char *hb_dateCMonth(int iMonth)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dateCMonth(%d)", iMonth));
#endif

  return (iMonth >= 1 && iMonth <= 12) ? hb_langDGetItem(HB_LANG_ITEM_BASE_MONTH + iMonth - 1) : s_nullStr;
}

const char *hb_dateCDOW(int iDay)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dateCDOW(%d)", iDay));
#endif

  return (iDay >= 1 && iDay <= 7) ? hb_langDGetItem(HB_LANG_ITEM_BASE_DAY + iDay - 1) : s_nullStr;
}

HB_FUNC(CMONTH)
{
  auto pDate = hb_param(1, Harbour::Item::DATETIME);

  if (pDate)
  {
    int iYear, iMonth, iDay;

    hb_dateDecode(pDate->getDL(), &iYear, &iMonth, &iDay);
    hb_retc_const(hb_dateCMonth(iMonth));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 1116, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(CDOW)
{
  auto pDate = hb_param(1, Harbour::Item::DATETIME);

  if (pDate)
  {
    long lDate = pDate->getDL();

    if (lDate)
    {
      int iYear, iMonth, iDay;

      hb_dateDecode(lDate, &iYear, &iMonth, &iDay);
      hb_retc_const(hb_dateCDOW(hb_dateDOW(iYear, iMonth, iDay)));
    }
    else
    {
      hb_retc_null();
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 1117, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_CDAY)
{
  auto pDay = hb_param(1, Harbour::Item::NUMERIC);

  if (pDay)
  {
    hb_retc_const(hb_dateCDOW(pDay->getNI()));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 1117, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}
