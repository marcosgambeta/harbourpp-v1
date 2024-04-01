/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2024 Marcos Antonio Gambeta

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

FUNCTION wasCOLORMAP()
RETURN was_COLORMAP():new()

CLASS WAS_COLORMAP

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // COLORREF from
   ASSIGN from(n) INLINE ::setfrom(n)
   ACCESS from INLINE ::getfrom()
   METHOD setfrom
   METHOD getfrom

   // COLORREF to
   ASSIGN to(n) INLINE ::setto(n)
   ACCESS to INLINE ::getto()
   METHOD setto
   METHOD getto

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_COLORMAP
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
#include "winapi.hpp"

HB_FUNC_STATIC( WAS_COLORMAP_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new COLORMAP());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_COLORMAP_DELETE )
{
  auto obj = static_cast<COLORMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// COLORREF from

HB_FUNC_STATIC( WAS_COLORMAP_SETFROM )
{
  auto obj = static_cast<COLORMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->from = wa_par_COLORREF(1);
  }
}

HB_FUNC_STATIC( WAS_COLORMAP_GETFROM )
{
  auto obj = static_cast<COLORMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_COLORREF(obj->from);
  }
}

// COLORREF to

HB_FUNC_STATIC( WAS_COLORMAP_SETTO )
{
  auto obj = static_cast<COLORMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->to = wa_par_COLORREF(1);
  }
}

HB_FUNC_STATIC( WAS_COLORMAP_GETTO )
{
  auto obj = static_cast<COLORMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_COLORREF(obj->to);
  }
}

/*
typedef struct _COLORMAP {
  COLORREF from;
  COLORREF to;
} COLORMAP, *LPCOLORMAP;
*/

#pragma ENDDUMP
