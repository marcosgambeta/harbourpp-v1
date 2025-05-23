//
// ValType() function
//
// Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
#include "hbapiitm.hpp"
#include "hbapierr.hpp"

HB_FUNC(VALTYPE)
{
  hb_retc(hb_itemTypeStr(hb_param(1, Harbour::Item::ANY)));
}

#if defined(HB_LEGACY_LEVEL5)

HB_FUNC(HB_ISNIL)
{
  hb_retl(HB_ISNIL(1));
}

#endif

HB_FUNC(HB_ISNUMERIC)
{
  hb_retl(HB_ISNUM(1));
}

HB_FUNC(HB_ISLOGICAL)
{
  hb_retl(HB_ISLOG(1));
}

HB_FUNC(HB_ISDATE)
{
  hb_retl(HB_ISDATE(1));
}

HB_FUNC(HB_ISDATETIME)
{
  hb_retl(HB_ISDATETIME(1));
}

HB_FUNC(HB_ISTIMESTAMP)
{
  hb_retl(HB_ISTIMESTAMP(1));
}

HB_FUNC(HB_ISBLOCK)
{
  hb_retl(HB_ISBLOCK(1));
}

HB_FUNC(HB_ISPOINTER)
{
  hb_retl(HB_ISPOINTER(1));
}

HB_FUNC(HB_ISSYMBOL)
{
  hb_retl(HB_ISSYMBOL(1));
}

HB_FUNC(HB_ISSTRING)
{
  hb_retl(HB_ISCHAR(1));
}

HB_FUNC(HB_ISCHAR)
{
  hb_retl((hb_parinfo(1) & (Harbour::Item::MEMO | Harbour::Item::STRING)) == Harbour::Item::STRING);
}

HB_FUNC(HB_ISMEMO)
{
  hb_retl(HB_ISMEMO(1));
}

HB_FUNC(HB_ISARRAY)
{
  hb_retl(hb_extIsArray(1));
}

HB_FUNC(HB_ISOBJECT)
{
  hb_retl(HB_ISOBJECT(1));
}

HB_FUNC(HB_ISHASH)
{
  hb_retl(HB_ISHASH(1));
}

HB_FUNC(HB_ISHASHKEY)
{
  hb_retl((hb_parinfo(1) & Harbour::Item::HASHKEY) != 0);
}

HB_FUNC(HB_ISEVALITEM)
{
  auto pItem = hb_param(1, Harbour::Item::ANY);
  hb_retl(pItem && pItem->isEvalItem());
}

HB_FUNC(HB_ISNULL)
{
  auto pItem = hb_param(1, Harbour::Item::ANY);

  if (pItem != nullptr)
  {
    if (pItem->isString())
    {
      hb_retl(pItem->getCLen() == 0);
      return;
    }
    else if (pItem->isArray())
    {
      hb_retl(hb_arrayLen(pItem) == 0);
      return;
    }
    else if (pItem->isHash())
    {
      hb_retl(hb_hashLen(pItem) == 0);
      return;
    }
  }
  hb_errRT_BASE_SubstR(EG_ARG, 1111, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}
