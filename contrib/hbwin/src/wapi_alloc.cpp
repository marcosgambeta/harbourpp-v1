//
// Low-level Windows object handling functions
//
// Copyright 2008 Viktor Szakats (vszakats.net/harbour)
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

#include "hbwapi.hpp"

static HB_GARBAGE_FUNC(s_gc_HDC_release)
{
  auto ph = static_cast<void **>(Cargo);

  // Check if pointer is not nullptr to avoid multiple freeing
  if (ph && *ph) {
    // Destroy the object
    DeleteDC(static_cast<HDC>(*ph));

    // set pointer to nullptr to avoid multiple freeing
    *ph = nullptr;
  }
}

static const HB_GC_FUNCS s_gc_HDC_funcs = {s_gc_HDC_release, hb_gcDummyMark};

void hbwapi_ret_HDC(HDC p)
{
  if (p) {
    auto ph = static_cast<void **>(hb_gcAllocate(sizeof(HDC *), &s_gc_HDC_funcs));

    *ph = p;

    hb_retptrGC(ph);
  } else {
    hb_retptr(nullptr);
  }
}

HDC hbwapi_par_HDC(int iParam)
{
  auto ph = static_cast<void **>(hb_parptrGC(&s_gc_HDC_funcs, iParam));

  return ph ? static_cast<HDC>(*ph) : static_cast<HDC>(hb_parptr(iParam));
}

static HB_GARBAGE_FUNC(s_gc_HPEN_release)
{
  auto ph = static_cast<void **>(Cargo);

  // Check if pointer is not nullptr to avoid multiple freeing
  if (ph && *ph) {
    // Destroy the object
    DeleteObject(static_cast<HPEN>(*ph));

    // set pointer to nullptr to avoid multiple freeing
    *ph = nullptr;
  }
}

static const HB_GC_FUNCS s_gc_HPEN_funcs = {s_gc_HPEN_release, hb_gcDummyMark};

void hbwapi_ret_HPEN(HPEN p)
{
  if (p) {
    auto ph = static_cast<void **>(hb_gcAllocate(sizeof(HPEN *), &s_gc_HPEN_funcs));

    *ph = p;

    hb_retptrGC(ph);
  } else {
    hb_retptr(nullptr);
  }
}

HPEN hbwapi_par_HPEN(int iParam)
{
  auto ph = static_cast<void **>(hb_parptrGC(&s_gc_HPEN_funcs, iParam));

  return ph ? static_cast<HPEN>(*ph) : nullptr;
}

static HB_GARBAGE_FUNC(s_gc_HBRUSH_release)
{
  auto ph = static_cast<void **>(Cargo);

  // Check if pointer is not nullptr to avoid multiple freeing
  if (ph && *ph) {
    // Destroy the object
    DeleteObject(static_cast<HBRUSH>(*ph));

    // set pointer to nullptr to avoid multiple freeing
    *ph = nullptr;
  }
}

static const HB_GC_FUNCS s_gc_HBRUSH_funcs = {s_gc_HBRUSH_release, hb_gcDummyMark};

void hbwapi_ret_HBRUSH(HBRUSH p)
{
  if (p) {
    auto ph = static_cast<void **>(hb_gcAllocate(sizeof(HBRUSH *), &s_gc_HBRUSH_funcs));

    *ph = p;

    hb_retptrGC(ph);
  } else {
    hb_retptr(nullptr);
  }
}

HBRUSH hbwapi_par_HBRUSH(int iParam)
{
  auto ph = static_cast<void **>(hb_parptrGC(&s_gc_HBRUSH_funcs, iParam));

  return ph ? static_cast<HBRUSH>(*ph) : nullptr;
}

static HB_GARBAGE_FUNC(s_gc_HFONT_release)
{
  auto ph = static_cast<void **>(Cargo);

  // Check if pointer is not nullptr to avoid multiple freeing
  if (ph && *ph) {
    // Destroy the object
    DeleteObject(static_cast<HFONT>(*ph));

    // set pointer to nullptr to avoid multiple freeing
    *ph = nullptr;
  }
}

static const HB_GC_FUNCS s_gc_HFONT_funcs = {s_gc_HFONT_release, hb_gcDummyMark};

void hbwapi_ret_HFONT(HFONT p)
{
  if (p) {
    auto ph = static_cast<void **>(hb_gcAllocate(sizeof(HFONT *), &s_gc_HFONT_funcs));

    *ph = p;

    hb_retptrGC(ph);
  } else {
    hb_retptr(nullptr);
  }
}

HFONT hbwapi_par_HFONT(int iParam)
{
  auto ph = static_cast<void **>(hb_parptrGC(&s_gc_HFONT_funcs, iParam));

  return ph ? static_cast<HFONT>(*ph) : nullptr;
}

static HB_GARBAGE_FUNC(s_gc_PDEVMODE_release)
{
  auto ph = static_cast<void **>(Cargo);

  // Check if pointer is not nullptr to avoid multiple freeing
  if (ph && *ph) {
    // Destroy the object
    hb_xfree(*ph);

    // set pointer to nullptr to avoid multiple freeing
    *ph = nullptr;
  }
}

static const HB_GC_FUNCS s_gc_PDEVMODE_funcs = {s_gc_PDEVMODE_release, hb_gcDummyMark};

void hbwapi_ret_PDEVMODE(PDEVMODE p)
{
  if (p) {
    auto ph = static_cast<void **>(hb_gcAllocate(sizeof(PDEVMODE *), &s_gc_PDEVMODE_funcs));

    *ph = p;

    hb_retptrGC(ph);
  } else {
    hb_retptr(nullptr);
  }
}

PDEVMODE hbwapi_par_PDEVMODE(int iParam)
{
  auto ph = static_cast<void **>(hb_parptrGC(&s_gc_PDEVMODE_funcs, iParam));

  return ph ? static_cast<PDEVMODE>(*ph) : nullptr;
}
