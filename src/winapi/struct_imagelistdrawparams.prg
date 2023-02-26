/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2022-2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2022-2023 Marcos Antonio Gambeta

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/*
  NOTE: source code generated with the help of a code generator
*/

#include "hbclass.ch"

CLASS WINAPI_STRUCT_IMAGELISTDRAWPARAMS

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

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_IMAGELISTDRAWPARAMS
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include <commctrl.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_NEW )
{
  PHB_ITEM self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new IMAGELISTDRAWPARAMS());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_DELETE )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD cbSize

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETCBSIZE )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->cbSize = static_cast<DWORD>(hb_parnl(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETCBSIZE )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->cbSize);
  }
}

// HIMAGELIST himl

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETHIML )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->himl = static_cast<HIMAGELIST>(hb_parptr(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETHIML )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retptr(obj->himl);
  }
}

// int i

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETI )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->i = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETI )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->i);
  }
}

// HDC hdcDst

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETHDCDST )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->hdcDst = static_cast<HDC>(hb_parptr(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETHDCDST )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retptr(obj->hdcDst);
  }
}

// int x

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETX )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->x = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETX )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->x);
  }
}

// int y

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETY )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->y = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETY )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->y);
  }
}

// int cx

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETCX )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->cx = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETCX )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->cx);
  }
}

// int cy

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETCY )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->cy = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETCY )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->cy);
  }
}

// int xBitmap

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETXBITMAP )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->xBitmap = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETXBITMAP )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->xBitmap);
  }
}

// int yBitmap

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETYBITMAP )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->yBitmap = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETYBITMAP )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->yBitmap);
  }
}

// COLORREF rgbBk

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETRGBBK )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->rgbBk = static_cast<COLORREF>(hb_parnl(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETRGBBK )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->rgbBk);
  }
}

// COLORREF rgbFg

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETRGBFG )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->rgbFg = static_cast<COLORREF>(hb_parnl(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETRGBFG )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->rgbFg);
  }
}

// UINT fStyle

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETFSTYLE )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->fStyle = static_cast<UINT>(hb_parni(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETFSTYLE )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->fStyle);
  }
}

// DWORD dwRop

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETDWROP )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->dwRop = static_cast<DWORD>(hb_parnl(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETDWROP )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->dwRop);
  }
}

// DWORD fState

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETFSTATE )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->fState = static_cast<DWORD>(hb_parnl(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETFSTATE )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->fState);
  }
}

// DWORD Frame

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETFRAME )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->Frame = static_cast<DWORD>(hb_parnl(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETFRAME )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->Frame);
  }
}

// COLORREF crEffect

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_SETCREFFECT )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->crEffect = static_cast<COLORREF>(hb_parnl(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_IMAGELISTDRAWPARAMS_GETCREFFECT )
{
  auto obj = static_cast<IMAGELISTDRAWPARAMS*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->crEffect);
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
