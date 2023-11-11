/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2023 Marcos Antonio Gambeta

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

CLASS WINAPI_STRUCT_LOGBRUSH

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // UINT lbStyle
   ASSIGN lbStyle(n) INLINE ::setlbStyle(n)
   ACCESS lbStyle INLINE ::getlbStyle()
   METHOD setlbStyle
   METHOD getlbStyle

   // COLORREF lbColor
   ASSIGN lbColor(n) INLINE ::setlbColor(n)
   ACCESS lbColor INLINE ::getlbColor()
   METHOD setlbColor
   METHOD getlbColor

   // ULONG_PTR lbHatch
   ASSIGN lbHatch(n) INLINE ::setlbHatch(n)
   ACCESS lbHatch INLINE ::getlbHatch()
   METHOD setlbHatch
   METHOD getlbHatch

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_LOGBRUSH
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

HB_FUNC_STATIC( WINAPI_STRUCT_LOGBRUSH_NEW )
{
  PHB_ITEM self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new LOGBRUSH());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WINAPI_STRUCT_LOGBRUSH_DELETE )
{
  auto obj = static_cast<LOGBRUSH*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// UINT lbStyle

HB_FUNC_STATIC( WINAPI_STRUCT_LOGBRUSH_SETLBSTYLE )
{
  auto obj = static_cast<LOGBRUSH*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->lbStyle = winapi_par_UINT(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_LOGBRUSH_GETLBSTYLE )
{
  auto obj = static_cast<LOGBRUSH*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_UINT(obj->lbStyle);
  }
}

// COLORREF lbColor

HB_FUNC_STATIC( WINAPI_STRUCT_LOGBRUSH_SETLBCOLOR )
{
  auto obj = static_cast<LOGBRUSH*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->lbColor = winapi_par_COLORREF(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_LOGBRUSH_GETLBCOLOR )
{
  auto obj = static_cast<LOGBRUSH*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_COLORREF(obj->lbColor);
  }
}

// ULONG_PTR lbHatch

HB_FUNC_STATIC( WINAPI_STRUCT_LOGBRUSH_SETLBHATCH )
{
  auto obj = static_cast<LOGBRUSH*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->lbHatch = winapi_par_ULONG_PTR(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_LOGBRUSH_GETLBHATCH )
{
  auto obj = static_cast<LOGBRUSH*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_ULONG_PTR(obj->lbHatch);
  }
}

/*
typedef struct tagLOGBRUSH {
  UINT      lbStyle;
  COLORREF  lbColor;
  ULONG_PTR lbHatch;
} LOGBRUSH, *PLOGBRUSH, *NPLOGBRUSH, *LPLOGBRUSH;
*/

#pragma ENDDUMP
