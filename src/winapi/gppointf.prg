//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2025 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2025 Marcos Antonio Gambeta
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// NOTE: source code generated with the help of a code generator

// clang-format off

#include "hbclass.ch"

CLASS WAGPPOINTF

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // REAL X
   ASSIGN X(n) INLINE ::setX(n)
   ACCESS X INLINE ::getX()
   METHOD setX
   METHOD getX

   // REAL Y
   ASSIGN Y(n) INLINE ::setY(n)
   ACCESS Y INLINE ::getY()
   METHOD setY
   METHOD getY

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAGPPOINTF
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

// clang-format on

#pragma BEGINDUMP

#include <windows.h>
#include <gdiplus.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbapierr.hpp"
#include "winapi.hpp"

using namespace Gdiplus;

HB_FUNC_STATIC(WAGPPOINTF_NEW)
{
  if (hb_pcount() == 0) {
    // PointF()
    auto self = hb_stackSelfItem();
    hb_objDataPutPtr(self, "_PTR", new GpPointF());
    hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
    hb_itemReturn(self);
  } else if (hb_pcount() == 2 && HB_ISNUM(1) && HB_ISNUM(2)) {
    // PointF(REAL x, REAL y)
    auto self = hb_stackSelfItem();
    hb_objDataPutPtr(self, "_PTR", new GpPointF(wa_par_REAL(1), wa_par_REAL(2)));
    hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
    hb_itemReturn(self);
  }
#if 0
  else if (hb_pcount() == 1 && HB_ISOBJECT(1) /* PointF */)
  {
    // PointF(const PointF& point)
    auto self = hb_stackSelfItem();
    hb_objDataPutPtr(self, "_PTR", new GpPointF());
    hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
    hb_itemReturn(self);
  }
  else if (hb_pcount() == 1 && HB_ISOBJECT(1) /* SizeF */)
  {
    // PointF(const SizeF& size)
    auto self = hb_stackSelfItem();
    hb_objDataPutPtr(self, "_PTR", new GpPointF());
    hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
    hb_itemReturn(self);
  }
#endif
  else {
    hb_errRT_BASE(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC_STATIC(WAGPPOINTF_DELETE)
{
  auto obj = static_cast<GpPointF *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// REAL X

HB_FUNC_STATIC(WAGPPOINTF_SETX)
{
  auto obj = static_cast<GpPointF *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    obj->X = wa_par_REAL(1);
  }
}

HB_FUNC_STATIC(WAGPPOINTF_GETX)
{
  auto obj = static_cast<GpPointF *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_REAL(obj->X);
  }
}

// REAL Y

HB_FUNC_STATIC(WAGPPOINTF_SETY)
{
  auto obj = static_cast<GpPointF *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    obj->Y = wa_par_REAL(1);
  }
}

HB_FUNC_STATIC(WAGPPOINTF_GETY)
{
  auto obj = static_cast<GpPointF *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_REAL(obj->Y);
  }
}

/*
typedef struct PointF {
   REAL X;
   REAL Y;
} PointF;
*/

#pragma ENDDUMP
