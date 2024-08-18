//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

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

// NOTE: source code generated with the help of a code generator

#include "hbclass.ch"

FUNCTION wasGRADIENT_RECT()
RETURN was_GRADIENT_RECT():new()

CLASS WAS_GRADIENT_RECT

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // ULONG UpperLeft
   ASSIGN UpperLeft(n) INLINE ::setUpperLeft(n)
   ACCESS UpperLeft INLINE ::getUpperLeft()
   METHOD setUpperLeft
   METHOD getUpperLeft

   // ULONG LowerRight
   ASSIGN LowerRight(n) INLINE ::setLowerRight(n)
   ACCESS LowerRight INLINE ::getLowerRight()
   METHOD setLowerRight
   METHOD getLowerRight

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_GRADIENT_RECT
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

HB_FUNC_STATIC( WAS_GRADIENT_RECT_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new GRADIENT_RECT());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_GRADIENT_RECT_DELETE )
{
  auto obj = static_cast<GRADIENT_RECT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// ULONG UpperLeft

HB_FUNC_STATIC( WAS_GRADIENT_RECT_SETUPPERLEFT )
{
  auto obj = static_cast<GRADIENT_RECT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->UpperLeft = wa_par_ULONG(1);
  }
}

HB_FUNC_STATIC( WAS_GRADIENT_RECT_GETUPPERLEFT )
{
  auto obj = static_cast<GRADIENT_RECT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_ULONG(obj->UpperLeft);
  }
}

// ULONG LowerRight

HB_FUNC_STATIC( WAS_GRADIENT_RECT_SETLOWERRIGHT )
{
  auto obj = static_cast<GRADIENT_RECT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->LowerRight = wa_par_ULONG(1);
  }
}

HB_FUNC_STATIC( WAS_GRADIENT_RECT_GETLOWERRIGHT )
{
  auto obj = static_cast<GRADIENT_RECT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_ULONG(obj->LowerRight);
  }
}

/*
typedef struct _GRADIENT_RECT {
  ULONG UpperLeft;
  ULONG LowerRight;
} GRADIENT_RECT, *PGRADIENT_RECT, *LPGRADIENT_RECT;
*/

#pragma ENDDUMP
