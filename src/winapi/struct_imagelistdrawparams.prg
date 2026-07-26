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

FUNCTION wasIMAGELISTDRAWPARAMS()
RETURN was_IMAGELISTDRAWPARAMS():new()

CLASS WAS_IMAGELISTDRAWPARAMS

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD cbSize
   ASSIGN cbSize(n) INLINE ::setcbSize(n)
   ACCESS cbSize INLINE ::getcbSize()
   METHOD setcbSize
   METHOD getcbSize

   // HIMAGELIST himl
   ASSIGN himl(p) INLINE ::sethiml(p)
   ACCESS himl INLINE ::gethiml()
   METHOD sethiml
   METHOD gethiml

   // int i
   ASSIGN i(n) INLINE ::seti(n)
   ACCESS i INLINE ::geti()
   METHOD seti
   METHOD geti

   // HDC hdcDst
   ASSIGN hdcDst(p) INLINE ::sethdcDst(p)
   ACCESS hdcDst INLINE ::gethdcDst()
   METHOD sethdcDst
   METHOD gethdcDst

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

   // int xBitmap
   ASSIGN xBitmap(n) INLINE ::setxBitmap(n)
   ACCESS xBitmap INLINE ::getxBitmap()
   METHOD setxBitmap
   METHOD getxBitmap

   // int yBitmap
   ASSIGN yBitmap(n) INLINE ::setyBitmap(n)
   ACCESS yBitmap INLINE ::getyBitmap()
   METHOD setyBitmap
   METHOD getyBitmap

   // COLORREF rgbBk
   ASSIGN rgbBk(n) INLINE ::setrgbBk(n)
   ACCESS rgbBk INLINE ::getrgbBk()
   METHOD setrgbBk
   METHOD getrgbBk

   // COLORREF rgbFg
   ASSIGN rgbFg(n) INLINE ::setrgbFg(n)
   ACCESS rgbFg INLINE ::getrgbFg()
   METHOD setrgbFg
   METHOD getrgbFg

   // UINT fStyle
   ASSIGN fStyle(n) INLINE ::setfStyle(n)
   ACCESS fStyle INLINE ::getfStyle()
   METHOD setfStyle
   METHOD getfStyle

   // DWORD dwRop
   ASSIGN dwRop(n) INLINE ::setdwRop(n)
   ACCESS dwRop INLINE ::getdwRop()
   METHOD setdwRop
   METHOD getdwRop

   // DWORD fState
   ASSIGN fState(n) INLINE ::setfState(n)
   ACCESS fState INLINE ::getfState()
   METHOD setfState
   METHOD getfState

   // DWORD Frame
   ASSIGN Frame(n) INLINE ::setFrame(n)
   ACCESS Frame INLINE ::getFrame()
   METHOD setFrame
   METHOD getFrame

   // COLORREF crEffect
   ASSIGN crEffect(n) INLINE ::setcrEffect(n)
   ACCESS crEffect INLINE ::getcrEffect()
   METHOD setcrEffect
   METHOD getcrEffect

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_IMAGELISTDRAWPARAMS
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

// clang-format on

#pragma BEGINDUMP

#include <windows.h>
#include <commctrl.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "winapi.hpp"

#define GET_PTR_FROM_SELF(obj) auto obj = static_cast<IMAGELISTDRAWPARAMS *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"))

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_NEW)
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new IMAGELISTDRAWPARAMS());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_DELETE)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD cbSize

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETCBSIZE)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->cbSize = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETCBSIZE)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_DWORD(obj->cbSize);
  }
}

// HIMAGELIST himl

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETHIML)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->himl = wa_par_HIMAGELIST(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETHIML)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_HIMAGELIST(obj->himl);
  }
}

// int i

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETI)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->i = wa_par_int(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETI)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_int(obj->i);
  }
}

// HDC hdcDst

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETHDCDST)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->hdcDst = wa_par_HDC(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETHDCDST)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_HDC(obj->hdcDst);
  }
}

// int x

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETX)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->x = wa_par_int(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETX)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_int(obj->x);
  }
}

// int y

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETY)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->y = wa_par_int(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETY)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_int(obj->y);
  }
}

// int cx

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETCX)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->cx = wa_par_int(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETCX)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_int(obj->cx);
  }
}

// int cy

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETCY)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->cy = wa_par_int(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETCY)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_int(obj->cy);
  }
}

// int xBitmap

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETXBITMAP)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->xBitmap = wa_par_int(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETXBITMAP)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_int(obj->xBitmap);
  }
}

// int yBitmap

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETYBITMAP)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->yBitmap = wa_par_int(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETYBITMAP)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_int(obj->yBitmap);
  }
}

// COLORREF rgbBk

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETRGBBK)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->rgbBk = wa_par_COLORREF(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETRGBBK)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_COLORREF(obj->rgbBk);
  }
}

// COLORREF rgbFg

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETRGBFG)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->rgbFg = wa_par_COLORREF(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETRGBFG)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_COLORREF(obj->rgbFg);
  }
}

// UINT fStyle

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETFSTYLE)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->fStyle = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETFSTYLE)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_UINT(obj->fStyle);
  }
}

// DWORD dwRop

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETDWROP)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->dwRop = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETDWROP)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_DWORD(obj->dwRop);
  }
}

// DWORD fState

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETFSTATE)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->fState = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETFSTATE)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_DWORD(obj->fState);
  }
}

// DWORD Frame

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETFRAME)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->Frame = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETFRAME)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_DWORD(obj->Frame);
  }
}

// COLORREF crEffect

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_SETCREFFECT)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    obj->crEffect = wa_par_COLORREF(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGELISTDRAWPARAMS_GETCREFFECT)
{
  GET_PTR_FROM_SELF(obj);

  if (obj != nullptr) {
    wa_ret_COLORREF(obj->crEffect);
  }
}

/*
typedef struct _IMAGELISTDRAWPARAMS {
  DWORD      cbSize;
  HIMAGELIST himl;
  int        i;
  HDC        hdcDst;
  int        x;
  int        y;
  int        cx;
  int        cy;
  int        xBitmap;
  int        yBitmap;
  COLORREF   rgbBk;
  COLORREF   rgbFg;
  UINT       fStyle;
  DWORD      dwRop;
  DWORD      fState;
  DWORD      Frame;
  COLORREF   crEffect;
} IMAGELISTDRAWPARAMS;
*/

#pragma ENDDUMP
