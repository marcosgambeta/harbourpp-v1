//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2024 Marcos Antonio Gambeta
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

#include "hbclass.ch"

CLASS WAGPRECT

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // INT X
   ASSIGN X(n) INLINE ::setX(n)
   ACCESS X INLINE ::getX()
   METHOD setX
   METHOD getX

   // INT Y
   ASSIGN Y(n) INLINE ::setY(n)
   ACCESS Y INLINE ::getY()
   METHOD setY
   METHOD getY

   // INT Width
   ASSIGN Width(n) INLINE ::setWidth(n)
   ACCESS Width INLINE ::getWidth()
   METHOD setWidth
   METHOD getWidth

   // INT Height
   ASSIGN Height(n) INLINE ::setHeight(n)
   ACCESS Height INLINE ::getHeight()
   METHOD setHeight
   METHOD getHeight

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAGPRECT
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include <gdiplus.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include <hbapierr.h>
#include "winapi.hpp"

using namespace Gdiplus;

HB_FUNC_STATIC(WAGPRECT_NEW)
{
  if( hb_pcount() == 0 )
  {
    // Rect()
    auto self = hb_stackSelfItem();
    hb_objDataPutPtr(self, "_PTR", new GpRect());
    hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
    hb_itemReturn(self);
  }
#if 0
  else if( hb_pcount() == 2 ) // TODO:
  {
    // Rect(const Point& location, const Size& size)
    auto self = hb_stackSelfItem();
    hb_objDataPutPtr(self, "_PTR", new GpRect());
    hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
    hb_itemReturn(self);
  }
#endif
  else if( hb_pcount() == 4 && HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) )
  {
    // Rect(INT x, INT y, INT width, INT height)
    auto self = hb_stackSelfItem();
    hb_objDataPutPtr(self, "_PTR", new GpRect(wa_par_INT(1), wa_par_INT(2), wa_par_INT(3), wa_par_INT(4)));
    hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
    hb_itemReturn(self);
  }
  else
  {
    hb_errRT_BASE(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC_STATIC(WAGPRECT_DELETE)
{
  auto obj = static_cast<GpRect*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// INT X

HB_FUNC_STATIC(WAGPRECT_SETX)
{
  auto obj = static_cast<GpRect*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->X = wa_par_INT(1);
  }
}

HB_FUNC_STATIC(WAGPRECT_GETX)
{
  auto obj = static_cast<GpRect*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_INT(obj->X);
  }
}

// INT Y

HB_FUNC_STATIC(WAGPRECT_SETY)
{
  auto obj = static_cast<GpRect*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->Y = wa_par_INT(1);
  }
}

HB_FUNC_STATIC(WAGPRECT_GETY)
{
  auto obj = static_cast<GpRect*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_INT(obj->Y);
  }
}

// INT Width

HB_FUNC_STATIC(WAGPRECT_SETWIDTH)
{
  auto obj = static_cast<GpRect*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->Width = wa_par_INT(1);
  }
}

HB_FUNC_STATIC(WAGPRECT_GETWIDTH)
{
  auto obj = static_cast<GpRect*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_INT(obj->Width);
  }
}

// INT Height

HB_FUNC_STATIC(WAGPRECT_SETHEIGHT)
{
  auto obj = static_cast<GpRect*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->Height = wa_par_INT(1);
  }
}

HB_FUNC_STATIC(WAGPRECT_GETHEIGHT)
{
  auto obj = static_cast<GpRect*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_INT(obj->Height);
  }
}

/*
typedef struct Rect {
   INT X;
   INT Y;
   INT Width;
   INT Height;
} Rect;
*/

#pragma ENDDUMP
