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

#include "hbclass.ch"

FUNCTION wasTRIVERTEX()
RETURN was_TRIVERTEX():new()

CLASS WAS_TRIVERTEX

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // LONG x
   ASSIGN x(n) INLINE ::setx(n)
   ACCESS x INLINE ::getx()
   METHOD setx
   METHOD getx

   // LONG y
   ASSIGN y(n) INLINE ::sety(n)
   ACCESS y INLINE ::gety()
   METHOD sety
   METHOD gety

   // COLOR16 Red
   ASSIGN Red(n) INLINE ::setRed(n)
   ACCESS Red INLINE ::getRed()
   METHOD setRed
   METHOD getRed

   // COLOR16 Green
   ASSIGN Green(n) INLINE ::setGreen(n)
   ACCESS Green INLINE ::getGreen()
   METHOD setGreen
   METHOD getGreen

   // COLOR16 Blue
   ASSIGN Blue(n) INLINE ::setBlue(n)
   ACCESS Blue INLINE ::getBlue()
   METHOD setBlue
   METHOD getBlue

   // COLOR16 Alpha
   ASSIGN Alpha(n) INLINE ::setAlpha(n)
   ACCESS Alpha INLINE ::getAlpha()
   METHOD setAlpha
   METHOD getAlpha

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_TRIVERTEX
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

HB_FUNC_STATIC(WAS_TRIVERTEX_NEW)
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new TRIVERTEX());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_TRIVERTEX_DELETE)
{
  auto obj = static_cast<TRIVERTEX *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// LONG x

HB_FUNC_STATIC(WAS_TRIVERTEX_SETX)
{
  auto obj = static_cast<TRIVERTEX *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->x = wa_par_LONG(1);
  }
}

HB_FUNC_STATIC(WAS_TRIVERTEX_GETX)
{
  auto obj = static_cast<TRIVERTEX *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_LONG(obj->x);
  }
}

// LONG y

HB_FUNC_STATIC(WAS_TRIVERTEX_SETY)
{
  auto obj = static_cast<TRIVERTEX *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->y = wa_par_LONG(1);
  }
}

HB_FUNC_STATIC(WAS_TRIVERTEX_GETY)
{
  auto obj = static_cast<TRIVERTEX *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_LONG(obj->y);
  }
}

// COLOR16 Red

HB_FUNC_STATIC(WAS_TRIVERTEX_SETRED)
{
  auto obj = static_cast<TRIVERTEX *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->Red = wa_par_COLOR16(1);
  }
}

HB_FUNC_STATIC(WAS_TRIVERTEX_GETRED)
{
  auto obj = static_cast<TRIVERTEX *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_COLOR16(obj->Red);
  }
}

// COLOR16 Green

HB_FUNC_STATIC(WAS_TRIVERTEX_SETGREEN)
{
  auto obj = static_cast<TRIVERTEX *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->Green = wa_par_COLOR16(1);
  }
}

HB_FUNC_STATIC(WAS_TRIVERTEX_GETGREEN)
{
  auto obj = static_cast<TRIVERTEX *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_COLOR16(obj->Green);
  }
}

// COLOR16 Blue

HB_FUNC_STATIC(WAS_TRIVERTEX_SETBLUE)
{
  auto obj = static_cast<TRIVERTEX *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->Blue = wa_par_COLOR16(1);
  }
}

HB_FUNC_STATIC(WAS_TRIVERTEX_GETBLUE)
{
  auto obj = static_cast<TRIVERTEX *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_COLOR16(obj->Blue);
  }
}

// COLOR16 Alpha

HB_FUNC_STATIC(WAS_TRIVERTEX_SETALPHA)
{
  auto obj = static_cast<TRIVERTEX *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->Alpha = wa_par_COLOR16(1);
  }
}

HB_FUNC_STATIC(WAS_TRIVERTEX_GETALPHA)
{
  auto obj = static_cast<TRIVERTEX *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_COLOR16(obj->Alpha);
  }
}

/*
typedef struct _TRIVERTEX {
  LONG    x;
  LONG    y;
  COLOR16 Red;
  COLOR16 Green;
  COLOR16 Blue;
  COLOR16 Alpha;
} TRIVERTEX, *PTRIVERTEX, *LPTRIVERTEX;
*/

#pragma ENDDUMP
