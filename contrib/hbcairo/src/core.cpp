//
// Cairo library: core
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

// --- cairo_t * support ---
static HB_GARBAGE_FUNC(hb_cairo_destructor)
{
  auto ppCairo = static_cast<cairo_t **>(Cargo);

  if (*ppCairo)
  {
    cairo_destroy(*ppCairo);
    *ppCairo = nullptr;
  }
}

static const HB_GC_FUNCS s_gcCairoFuncs = {hb_cairo_destructor, hb_gcDummyMark};

cairo_t *hb_cairoItemGet(PHB_ITEM pItem)
{
  auto ppCairo = static_cast<cairo_t **>(hb_itemGetPtrGC(pItem, &s_gcCairoFuncs));

  return ppCairo ? *ppCairo : nullptr;
}

PHB_ITEM hb_cairoItemPut(PHB_ITEM pItem, cairo_t *pCairo)
{
  auto ppCairo = static_cast<cairo_t **>(hb_gcAllocate(sizeof(cairo_t *), &s_gcCairoFuncs));

  *ppCairo = pCairo;
  return hb_itemPutPtrGC(pItem, ppCairo);
}

cairo_t *hb_cairo_param(int iParam)
{
  auto ppCairo = static_cast<cairo_t **>(hb_parptrGC(&s_gcCairoFuncs, iParam));

  if (ppCairo && *ppCairo)
  {
    return *ppCairo;
  }

  hb_errRT_BASE(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  return nullptr;
}

void hb_cairo_ret(cairo_t *pCairo)
{
  hb_cairoItemPut(hb_stackReturnItem(), pCairo);
}

HB_FUNC(CAIRO_DESTROY)
{
  auto ppCairo = static_cast<cairo_t **>(hb_parptrGC(&s_gcCairoFuncs, 1));

  if (ppCairo && *ppCairo)
  {
    cairo_destroy(*ppCairo);
    *ppCairo = nullptr;
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// --- cairo_surface_t * support ---
static HB_GARBAGE_FUNC(hb_cairo_surface_destructor)
{
  auto ppSurface = static_cast<cairo_surface_t **>(Cargo);

  if (*ppSurface)
  {
    cairo_surface_destroy(*ppSurface);
    *ppSurface = nullptr;
  }
}

static const HB_GC_FUNCS s_gcSurfaceFuncs = {hb_cairo_surface_destructor, hb_gcDummyMark};

cairo_surface_t *hb_cairoSurfaceItemGet(PHB_ITEM pItem)
{
  auto ppSurface = static_cast<cairo_surface_t **>(hb_itemGetPtrGC(pItem, &s_gcSurfaceFuncs));

  return ppSurface ? *ppSurface : nullptr;
}

PHB_ITEM hb_cairoSurfaceItemPut(PHB_ITEM pItem, cairo_surface_t *pSurface)
{
  auto ppSurface = static_cast<cairo_surface_t **>(hb_gcAllocate(sizeof(cairo_surface_t *), &s_gcSurfaceFuncs));

  *ppSurface = pSurface;
  return hb_itemPutPtrGC(pItem, ppSurface);
}

void hb_cairoSurfaceStor(cairo_surface_t *pSurface, int iParam)
{
  auto ppSurface = static_cast<cairo_surface_t **>(hb_gcAllocate(sizeof(cairo_surface_t *), &s_gcSurfaceFuncs));

  *ppSurface = pSurface;
  hb_storptrGC(ppSurface, iParam);
}

cairo_surface_t *hb_cairo_surface_param(int iParam)
{
  auto ppSurface = static_cast<cairo_surface_t **>(hb_parptrGC(&s_gcSurfaceFuncs, iParam));

  if (ppSurface && *ppSurface)
  {
    return *ppSurface;
  }

  hb_errRT_BASE(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  return nullptr;
}

void hb_cairo_surface_ret(cairo_surface_t *pSurface)
{
  hb_cairoSurfaceItemPut(hb_stackReturnItem(), pSurface);
}

HB_FUNC(CAIRO_SURFACE_DESTROY)
{
  auto ppSurface = static_cast<cairo_surface_t **>(hb_parptrGC(&s_gcSurfaceFuncs, 1));

  if (ppSurface && *ppSurface)
  {
    cairo_surface_destroy(*ppSurface);
    *ppSurface = nullptr;
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// --- cairo_path_t * support ---
static HB_GARBAGE_FUNC(hb_cairo_path_destructor)
{
  auto ppPath = static_cast<cairo_path_t **>(Cargo);

  if (*ppPath)
  {
    cairo_path_destroy(*ppPath);
    *ppPath = nullptr;
  }
}

static const HB_GC_FUNCS s_gcPathFuncs = {hb_cairo_path_destructor, hb_gcDummyMark};

cairo_path_t *hb_cairoPathItemGet(PHB_ITEM pItem)
{
  auto ppPath = static_cast<cairo_path_t **>(hb_itemGetPtrGC(pItem, &s_gcPathFuncs));

  return ppPath ? *ppPath : nullptr;
}

PHB_ITEM hb_cairoPathItemPut(PHB_ITEM pItem, cairo_path_t *pPath)
{
  auto ppPath = static_cast<cairo_path_t **>(hb_gcAllocate(sizeof(cairo_path_t *), &s_gcPathFuncs));

  *ppPath = pPath;
  return hb_itemPutPtrGC(pItem, ppPath);
}

cairo_path_t *hb_cairo_path_param(int iParam)
{
  auto ppPath = static_cast<cairo_path_t **>(hb_parptrGC(&s_gcPathFuncs, iParam));

  if (ppPath && *ppPath)
  {
    return *ppPath;
  }

  hb_errRT_BASE(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  return nullptr;
}

void hb_cairo_path_ret(cairo_path_t *pPath)
{
  hb_cairoPathItemPut(hb_stackReturnItem(), pPath);
}

HB_FUNC(CAIRO_PATH_DESTROY)
{
  auto ppPath = static_cast<cairo_path_t **>(hb_parptrGC(&s_gcPathFuncs, 1));

  if (ppPath && *ppPath)
  {
    cairo_path_destroy(*ppPath);
    *ppPath = nullptr;
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// --- cairo_path_t * iterator support ---

// NOTE: Path iterator functions are is not cairo functions.
//       This is only a way to pass path data to .prg level

struct HB_CAIRO_PATH_ITERATOR
{
  cairo_path_t **ppPath;
  int iPos;
};

using PHB_CAIRO_PATH_ITERATOR = HB_CAIRO_PATH_ITERATOR *;

static HB_GARBAGE_FUNC(hb_cairo_path_iterator_destructor)
{
  auto pIterator = static_cast<PHB_CAIRO_PATH_ITERATOR>(Cargo);

  if (pIterator->ppPath)
  {
    hb_gcRefFree(pIterator->ppPath);
    pIterator->ppPath = nullptr;
  }
}

static HB_GARBAGE_FUNC(hb_cairo_path_iterator_mark)
{
  auto pIterator = static_cast<PHB_CAIRO_PATH_ITERATOR>(Cargo);

  if (pIterator->ppPath)
  {
    hb_gcMark(pIterator->ppPath);
  }
}

static const HB_GC_FUNCS s_gcIteratorFuncs = {hb_cairo_path_iterator_destructor, hb_cairo_path_iterator_mark};

HB_FUNC(CAIRO_PATH_ITERATOR_CREATE)
{
  auto ppPath = static_cast<cairo_path_t **>(hb_parptrGC(&s_gcPathFuncs, 1));

  if (ppPath && *ppPath)
  {
    auto pIterator =
        static_cast<PHB_CAIRO_PATH_ITERATOR>(hb_gcAllocate(sizeof(HB_CAIRO_PATH_ITERATOR), &s_gcIteratorFuncs));
    pIterator->ppPath = ppPath;
    hb_gcRefInc(ppPath);
    pIterator->iPos = -1;
    hb_itemPutPtrGC(hb_stackReturnItem(), pIterator);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(CAIRO_PATH_ITERATOR_DESTROY)
{
  auto pIterator = static_cast<PHB_CAIRO_PATH_ITERATOR>(hb_parptrGC(&s_gcIteratorFuncs, 1));

  if (pIterator && pIterator->ppPath)
  {
    hb_gcRefFree(pIterator->ppPath);
    pIterator->ppPath = nullptr;
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(CAIRO_PATH_ITERATOR_NEXT)
{
  auto pIterator = static_cast<PHB_CAIRO_PATH_ITERATOR>(hb_parptrGC(&s_gcIteratorFuncs, 1));
  cairo_path_t *pPath;

  if (pIterator && pIterator->ppPath && (pPath = *(pIterator->ppPath)) != nullptr)
  {
    // Skip
    if (pIterator->iPos == -1)
    {
      pIterator->iPos = 0;
    }
    else if (pIterator->iPos < pPath->num_data)
    {
      pIterator->iPos += pPath->data[pIterator->iPos].header.length;
    }

    // return type
    if (pIterator->iPos < pPath->num_data)
    {
      hb_retni(pPath->data[pIterator->iPos].header.type);
    }
    else
    {
      hb_ret();
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(CAIRO_PATH_ITERATOR_GET_POINTS)
{
  auto pIterator = static_cast<PHB_CAIRO_PATH_ITERATOR>(hb_parptrGC(&s_gcIteratorFuncs, 1));
  cairo_path_t *pPath;

  if (pIterator && pIterator->ppPath && (pPath = *(pIterator->ppPath)) != nullptr)
  {
    if (pIterator->iPos < pPath->num_data && pIterator->iPos != -1)
    {
      cairo_path_data_t *pData;
      pData = pPath->data + pIterator->iPos;
      auto pArray = hb_itemArrayNew(pData->header.length - 1);
      for (auto i = 1; i < pData->header.length; i++)
      {
        auto pItem = hb_arrayGetItemPtr(pArray, i);
        hb_arrayNew(pItem, 2);
        hb_arraySetND(pItem, 1, pData[i].point.x);
        hb_arraySetND(pItem, 2, pData[i].point.y);
      }
      hb_itemReturnRelease(pArray);
    }
    else
    {
      hb_ret();
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(CAIRO_PATH_ITERATOR_SET_POINTS)
{
  auto pIterator = static_cast<PHB_CAIRO_PATH_ITERATOR>(hb_parptrGC(&s_gcIteratorFuncs, 1));
  auto pArray = hb_param(2, Harbour::Item::ARRAY);
  cairo_path_t *pPath;

  if (pIterator && pIterator->ppPath && (pPath = *(pIterator->ppPath)) != nullptr && pArray)
  {
    HB_SIZE nLen = hb_arrayLen(pArray);
    if (pIterator->iPos < pPath->num_data && pIterator->iPos != -1 &&
        static_cast<HB_SIZE>(pPath->data[pIterator->iPos].header.length) == nLen + 1)
    {
      cairo_path_data_t *pData = pPath->data + pIterator->iPos;
      for (auto i = 1; i < pData->header.length; i++)
      {
        auto pItem = hb_arrayGetItemPtr(pArray, i);
        if (hb_arrayLen(pItem) == 2)
        {
          pData[i].point.x = hb_arrayGetND(pItem, 1);
          pData[i].point.y = hb_arrayGetND(pItem, 2);
        }
        else
        {
          hb_retl(false);
          return;
        }
      }
      hb_retl(true);
    }
    else
    {
      hb_retl(false);
    }
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}
