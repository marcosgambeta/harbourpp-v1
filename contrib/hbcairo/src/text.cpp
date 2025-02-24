//
// Cairo library: text
//
// Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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

#include "hbcairo.hpp"
#include <hbapistr.hpp>
#include <hbapierr.hpp>

HB_FUNC(CAIRO_FONT_EXTENTS)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    auto pItem = hb_stackReturnItem();
    cairo_font_extents_t fe;
    cairo_font_extents(pCairo, &fe);
    hb_arrayNew(pItem, 5);
    hb_arraySetND(pItem, 1, fe.ascent);
    hb_arraySetND(pItem, 2, fe.descent);
    hb_arraySetND(pItem, 3, fe.height);
    hb_arraySetND(pItem, 4, fe.max_x_advance);
    hb_arraySetND(pItem, 5, fe.max_y_advance);
  }
}

HB_FUNC(CAIRO_GET_FONT_MATRIX)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    auto pItem = hb_stackReturnItem();
    cairo_matrix_t m;
    cairo_get_font_matrix(pCairo, &m);
    hb_arrayNew(pItem, 6);
    hb_arraySetND(pItem, 1, m.xx);
    hb_arraySetND(pItem, 2, m.yx);
    hb_arraySetND(pItem, 3, m.xy);
    hb_arraySetND(pItem, 4, m.yy);
    hb_arraySetND(pItem, 5, m.x0);
    hb_arraySetND(pItem, 6, m.y0);
  }
}

HB_FUNC(CAIRO_SELECT_FONT_FACE)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    void *hFamily;
    cairo_select_font_face(pCairo, hb_parstr_utf8(2, &hFamily, nullptr), static_cast<cairo_font_slant_t>(hb_parni(3)),
                           static_cast<cairo_font_weight_t>(hb_parni(4)));
    hb_strfree(hFamily);
  }
}

HB_FUNC(CAIRO_SET_FONT_MATRIX)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    PHB_ITEM pItem;
    if ((pItem = hb_param(2, Harbour::Item::ARRAY)) != nullptr && hb_arrayLen(pItem) == 6)
    {
      cairo_matrix_t m;
      m.xx = hb_arrayGetND(pItem, 1);
      m.yx = hb_arrayGetND(pItem, 2);
      m.xy = hb_arrayGetND(pItem, 3);
      m.yy = hb_arrayGetND(pItem, 4);
      m.x0 = hb_arrayGetND(pItem, 5);
      m.y0 = hb_arrayGetND(pItem, 6);
      cairo_set_font_matrix(pCairo, &m);
    }
    else
    {
      hb_errRT_BASE(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    }
  }
}

HB_FUNC(CAIRO_SET_FONT_SIZE)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_set_font_size(pCairo, hb_parnd(2));
  }
}

HB_FUNC(CAIRO_SHOW_TEXT)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    void *hText;
    cairo_show_text(pCairo, hb_parstr_utf8(2, &hText, nullptr));
    hb_strfree(hText);
  }
}

HB_FUNC(CAIRO_TEXT_EXTENTS)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    void *hText;
    auto pItem = hb_stackReturnItem();
    cairo_text_extents_t te;
    cairo_text_extents(pCairo, hb_parstr_utf8(2, &hText, nullptr), &te);
    hb_strfree(hText);
    hb_arrayNew(pItem, 6);
    hb_arraySetND(pItem, 1, te.x_bearing);
    hb_arraySetND(pItem, 2, te.y_bearing);
    hb_arraySetND(pItem, 3, te.width);
    hb_arraySetND(pItem, 4, te.height);
    hb_arraySetND(pItem, 5, te.x_advance);
    hb_arraySetND(pItem, 6, te.y_advance);
  }
}
