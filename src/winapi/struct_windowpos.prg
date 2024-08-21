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

FUNCTION wasWINDOWPOS()
RETURN was_WINDOWPOS():new()

CLASS WAS_WINDOWPOS

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // HWND hwnd
   ASSIGN hwnd(n) INLINE ::sethwnd(n)
   ACCESS hwnd INLINE ::gethwnd()
   METHOD sethwnd
   METHOD gethwnd

   // HWND hwndInsertAfter
   ASSIGN hwndInsertAfter(n) INLINE ::sethwndInsertAfter(n)
   ACCESS hwndInsertAfter INLINE ::gethwndInsertAfter()
   METHOD sethwndInsertAfter
   METHOD gethwndInsertAfter

   // int x
   ASSIGN x(n) INLINE ::setx(n)
   ACCESS x INLINE ::getx()
   METHOD setx
   METHOD getx

   // int y
   ASSIGN y(n) INLINE ::sety(n)
   ACCESS y INLINE ::gety()
   METHOD sety
   METHOD gety

   // int cx
   ASSIGN cx(n) INLINE ::setcx(n)
   ACCESS cx INLINE ::getcx()
   METHOD setcx
   METHOD getcx

   // int cy
   ASSIGN cy(n) INLINE ::setcy(n)
   ACCESS cy INLINE ::getcy()
   METHOD setcy
   METHOD getcy

   // UINT flags
   ASSIGN flags(n) INLINE ::setflags(n)
   ACCESS flags INLINE ::getflags()
   METHOD setflags
   METHOD getflags

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_WINDOWPOS
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "winapi.hpp"

HB_FUNC_STATIC( WAS_WINDOWPOS_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new WINDOWPOS());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_WINDOWPOS_DELETE )
{
  auto obj = static_cast<WINDOWPOS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// HWND hwnd

HB_FUNC_STATIC( WAS_WINDOWPOS_SETHWND )
{
  auto obj = static_cast<WINDOWPOS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hwnd = wa_par_HWND(1);
  }
}

HB_FUNC_STATIC( WAS_WINDOWPOS_GETHWND )
{
  auto obj = static_cast<WINDOWPOS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HWND(obj->hwnd);
  }
}

// HWND hwndInsertAfter

HB_FUNC_STATIC( WAS_WINDOWPOS_SETHWNDINSERTAFTER )
{
  auto obj = static_cast<WINDOWPOS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hwndInsertAfter = wa_par_HWND(1);
  }
}

HB_FUNC_STATIC( WAS_WINDOWPOS_GETHWNDINSERTAFTER )
{
  auto obj = static_cast<WINDOWPOS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HWND(obj->hwndInsertAfter);
  }
}

// int x

HB_FUNC_STATIC( WAS_WINDOWPOS_SETX )
{
  auto obj = static_cast<WINDOWPOS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->x = wa_par_int(1);
  }
}

HB_FUNC_STATIC( WAS_WINDOWPOS_GETX )
{
  auto obj = static_cast<WINDOWPOS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_int(obj->x);
  }
}

// int y

HB_FUNC_STATIC( WAS_WINDOWPOS_SETY )
{
  auto obj = static_cast<WINDOWPOS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->y = wa_par_int(1);
  }
}

HB_FUNC_STATIC( WAS_WINDOWPOS_GETY )
{
  auto obj = static_cast<WINDOWPOS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_int(obj->y);
  }
}

// int cx

HB_FUNC_STATIC( WAS_WINDOWPOS_SETCX )
{
  auto obj = static_cast<WINDOWPOS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cx = wa_par_int(1);
  }
}

HB_FUNC_STATIC( WAS_WINDOWPOS_GETCX )
{
  auto obj = static_cast<WINDOWPOS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_int(obj->cx);
  }
}

// int cy

HB_FUNC_STATIC( WAS_WINDOWPOS_SETCY )
{
  auto obj = static_cast<WINDOWPOS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cy = wa_par_int(1);
  }
}

HB_FUNC_STATIC( WAS_WINDOWPOS_GETCY )
{
  auto obj = static_cast<WINDOWPOS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_int(obj->cy);
  }
}

// UINT flags

HB_FUNC_STATIC( WAS_WINDOWPOS_SETFLAGS )
{
  auto obj = static_cast<WINDOWPOS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->flags = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC( WAS_WINDOWPOS_GETFLAGS )
{
  auto obj = static_cast<WINDOWPOS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->flags);
  }
}

/*
typedef struct tagWINDOWPOS {
  HWND hwnd;
  HWND hwndInsertAfter;
  int  x;
  int  y;
  int  cx;
  int  cy;
  UINT flags;
} WINDOWPOS, *LPWINDOWPOS, *PWINDOWPOS;
*/

#pragma ENDDUMP
