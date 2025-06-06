//
// Cairo library: drawing context
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
#include <hbapiitm.hpp>
#include <hbapierr.hpp>

HB_FUNC(CAIRO_CLIP)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_clip(pCairo);
  }
}

HB_FUNC(CAIRO_CLIP_EXTENTS)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    auto pItem = hb_stackReturnItem();
    double x1, y1, x2, y2;
    cairo_clip_extents(pCairo, &x1, &y1, &x2, &y2);
    hb_arrayNew(pItem, 4);
    hb_arraySetND(pItem, 1, x1);
    hb_arraySetND(pItem, 2, y1);
    hb_arraySetND(pItem, 3, x2);
    hb_arraySetND(pItem, 4, y2);
  }
}

HB_FUNC(CAIRO_CLIP_PRESERVE)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_clip_preserve(pCairo);
  }
}

HB_FUNC(CAIRO_COPY_PAGE)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_copy_page(pCairo);
  }
}

HB_FUNC(CAIRO_CREATE)
{
  auto pSurface = hb_cairo_surface_param(1);

  if (pSurface != nullptr)
  {
    hb_cairo_ret(cairo_create(pSurface));
  }
}

HB_FUNC(CAIRO_FILL)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_fill(pCairo);
  }
}

HB_FUNC(CAIRO_FILL_PRESERVE)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_fill_preserve(pCairo);
  }
}

HB_FUNC(CAIRO_GET_DASH)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    int iCount = cairo_get_dash_count(pCairo);
    auto pDashes = static_cast<double *>(hb_xgrab(iCount * sizeof(double)));
    double dOffset;
    cairo_get_dash(pCairo, pDashes, &dOffset);
    hb_stornd(dOffset, 3);
    auto pItem = hb_itemArrayNew(static_cast<HB_SIZE>(iCount));
    for (auto i = 0; i < iCount; i++)
    {
      hb_arraySetND(pItem, static_cast<HB_SIZE>(i) + 1, pDashes[i]);
    }
    hb_xfree(pDashes);
    hb_itemParamStoreForward(2, pItem);
    hb_itemRelease(pItem);
  }
}

HB_FUNC(CAIRO_GET_LINE_WIDTH)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    hb_retnd(cairo_get_line_width(pCairo));
  }
}

HB_FUNC(CAIRO_IN_FILL)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    hb_retl(cairo_in_fill(pCairo, hb_parnd(2), hb_parnd(3)));
  }
}

HB_FUNC(CAIRO_IN_STROKE)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    hb_retl(cairo_in_stroke(pCairo, hb_parnd(2), hb_parnd(3)));
  }
}

HB_FUNC(CAIRO_PAINT)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_paint(pCairo);
  }
}

HB_FUNC(CAIRO_PAINT_WITH_ALPHA)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_paint_with_alpha(pCairo, hb_parnd(2));
  }
}

HB_FUNC(CAIRO_RESET_CLIP)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_reset_clip(pCairo);
  }
}

HB_FUNC(CAIRO_RESTORE)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_restore(pCairo);
  }
}

HB_FUNC(CAIRO_SAVE)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_save(pCairo);
  }
}

HB_FUNC(CAIRO_SET_DASH)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    PHB_ITEM pItem;

    if ((pItem = hb_param(2, Harbour::Item::ARRAY)) != nullptr)
    {
      auto iCount = static_cast<int>(hb_arrayLen(pItem));
      double *pDashes = nullptr;

      if (iCount)
      {
        pDashes = static_cast<double *>(hb_xgrab(iCount * sizeof(double)));
      }

      for (auto i = 0; i < iCount; i++)
      {
        pDashes[i] = hb_arrayGetND(pItem, static_cast<HB_SIZE>(i) + 1);
      }
      cairo_set_dash(pCairo, pDashes, iCount, hb_parnd(3));

      if (pDashes != nullptr)
      {
        hb_xfree(pDashes);
      }
    }
    else
    {
      hb_errRT_BASE(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
    }
  }
}

HB_FUNC(CAIRO_SET_FILL_RULE)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_set_fill_rule(pCairo, static_cast<cairo_fill_rule_t>(hb_parni(2)));
  }
}

HB_FUNC(CAIRO_SET_LINE_CAP)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_set_line_cap(pCairo, static_cast<cairo_line_cap_t>(hb_parni(2)));
  }
}

HB_FUNC(CAIRO_SET_LINE_JOIN)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_set_line_join(pCairo, static_cast<cairo_line_join_t>(hb_parni(2)));
  }
}

HB_FUNC(CAIRO_SET_LINE_WIDTH)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_set_line_width(pCairo, hb_parnd(2));
  }
}

HB_FUNC(CAIRO_SET_MITER_LIMIT)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_set_miter_limit(pCairo, hb_parnd(2));
  }
}

HB_FUNC(CAIRO_SET_OPERATOR)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_set_operator(pCairo, static_cast<cairo_operator_t>(hb_parni(2)));
  }
}

HB_FUNC(CAIRO_SET_SOURCE_RGB)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_set_source_rgb(pCairo, hb_parnd(2), hb_parnd(3), hb_parnd(4));
  }
}

HB_FUNC(CAIRO_SET_SOURCE_RGBA)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_set_source_rgba(pCairo, hb_parnd(2), hb_parnd(3), hb_parnd(4), hb_parnd(5));
  }
}

HB_FUNC(CAIRO_SET_TOLERANCE)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_set_tolerance(pCairo, hb_parnd(2));
  }
}

HB_FUNC(CAIRO_SHOW_PAGE)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_show_page(pCairo);
  }
}

HB_FUNC(CAIRO_STROKE)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_stroke(pCairo);
  }
}

HB_FUNC(CAIRO_STATUS)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    hb_retni(cairo_status(pCairo));
  }
}

HB_FUNC(CAIRO_STROKE_EXTENTS)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    auto pItem = hb_stackReturnItem();
    double x1, y1, x2, y2;
    cairo_stroke_extents(pCairo, &x1, &y1, &x2, &y2);
    hb_arrayNew(pItem, 4);
    hb_arraySetND(pItem, 1, x1);
    hb_arraySetND(pItem, 2, y1);
    hb_arraySetND(pItem, 3, x2);
    hb_arraySetND(pItem, 4, y2);
  }
}

HB_FUNC(CAIRO_STROKE_PRESERVE)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_stroke_preserve(pCairo);
  }
}

HB_FUNC(CAIRO_PUSH_GROUP)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_push_group(pCairo);
  }
}

HB_FUNC(CAIRO_POP_GROUP_TO_SOURCE)
{
  auto pCairo = hb_cairo_param(1);

  if (pCairo != nullptr)
  {
    cairo_pop_group_to_source(pCairo);
  }
}

HB_FUNC(CAIRO_SET_SOURCE_SURFACE)
{
  auto pCairo = hb_cairo_param(1);
  auto pSurface = hb_cairo_surface_param(2);
  cairo_set_source_surface(pCairo, pSurface, hb_parnd(3), hb_parnd(4));
}

HB_FUNC(CAIRO_SET_SOURCE)
{
  auto pCairo = hb_cairo_param(1);
  auto pPattern = hb_cairo_pattern_param(2);

  if (pCairo != nullptr)
  {
    cairo_set_source(pCairo, pPattern);
  }
}
