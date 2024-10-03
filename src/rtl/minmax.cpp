//
// Min(), Max() functions
//
// Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
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

/* returns the maximum of two date or numerics */
/* NOTE: CA-Cl*pper returns 1st item when they are equal [druzus] */
HB_FUNC(MAX)
{
  auto p1 = hb_param(1, Harbour::Item::ANY);
  auto p2 = hb_param(2, Harbour::Item::ANY);

  if (p1 && p2)
  {
    if (p1->isNumInt() && p2->isNumInt())
    {
      HB_MAXINT l1 = p1->getNInt();
      HB_MAXINT l2 = p2->getNInt();
      if (l1 >= l2)
      {
        hb_itemReturn(p1);
      }
      else
      {
        hb_itemReturn(p2);
      }
      return;
    }
    else if (p1->isNumeric() && p2->isNumeric())
    {
      auto d1 = p1->getND();
      auto d2 = p2->getND();
      if (d1 >= d2)
      {
        hb_itemReturn(p1);
      }
      else
      {
        hb_itemReturn(p2);
      }
      return;
    }
    else if (p1->isLogical() && p2->isLogical())
    {
      HB_BOOL b1 = p1->getL();
      HB_BOOL b2 = p2->getL();
      hb_retl(b1 >= b2 ? b1 : b2);
      return;
    }
    else if (p1->isDate() && p2->isDate())
    {
      long l1 = p1->getDL();
      long l2 = p2->getDL();
      hb_retdl(l1 >= l2 ? l1 : l2);
      return;
    }
    else if (p1->isDateTime() && p2->isDateTime())
    {
      if (p1->isDate() && p1->getDL() == p2->getDL())
      {
        hb_itemReturn(p1);
      }
      else if (p2->isDate() && p1->getDL() == p2->getDL())
      {
        hb_itemReturn(p2);
      }
      else
      {
        hb_itemReturn(hb_itemGetTD(p1) >= hb_itemGetTD(p2) ? p1 : p2);
      }
      return;
    }
  }
  hb_errRT_BASE_SubstR(EG_ARG, 1093, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

/* returns the minimum of two date or numerics */
/* NOTE: CA-Cl*pper returns 1st item when they are equal [druzus] */
HB_FUNC(MIN)
{
  auto p1 = hb_param(1, Harbour::Item::ANY);
  auto p2 = hb_param(2, Harbour::Item::ANY);

  if (p1 && p2)
  {
    if (p1->isNumInt() && p2->isNumInt())
    {
      HB_MAXINT l1 = p1->getNInt();
      HB_MAXINT l2 = p2->getNInt();
      if (l1 <= l2)
      {
        hb_itemReturn(p1);
      }
      else
      {
        hb_itemReturn(p2);
      }
      return;
    }
    else if (p1->isNumeric() && p2->isNumeric())
    {
      auto d1 = p1->getND();
      auto d2 = p2->getND();
      if (d1 <= d2)
      {
        hb_itemReturn(p1);
      }
      else
      {
        hb_itemReturn(p2);
      }
      return;
    }
    else if (p1->isLogical() && p2->isLogical())
    {
      HB_BOOL b1 = p1->getL();
      HB_BOOL b2 = p2->getL();
      hb_retl(b1 <= b2 ? b1 : b2);
      return;
    }
    else if (p1->isDate() && p2->isDate())
    {
      long l1 = p1->getDL();
      long l2 = p2->getDL();

      hb_retdl(l1 <= l2 ? l1 : l2);
      return;
    }
    else if (p1->isDateTime() && p2->isDateTime())
    {
      if (p1->isDate() && p1->getDL() == p2->getDL())
      {
        hb_itemReturn(p1);
      }
      else if (p2->isDate() && p1->getDL() == p2->getDL())
      {
        hb_itemReturn(p2);
      }
      else
      {
        hb_itemReturn(hb_itemGetTD(p1) <= hb_itemGetTD(p2) ? p1 : p2);
      }
      return;
    }
  }

  hb_errRT_BASE_SubstR(EG_ARG, 1092, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}
