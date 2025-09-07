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

FUNCTION wasPOINTF()
RETURN was_POINTF():new()

CLASS WAS_POINTF

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // FLOAT x
   ASSIGN x(n) INLINE ::setx(n)
   ACCESS x INLINE ::getx()
   METHOD setx
   METHOD getx

   // FLOAT y
   ASSIGN y(n) INLINE ::sety(n)
   ACCESS y INLINE ::gety()
   METHOD sety
   METHOD gety

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_POINTF
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

// clang-format on

#pragma BEGINDUMP

#include <windows.h>
#include <ocidl.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "winapi.hpp"

HB_FUNC_STATIC(WAS_POINTF_NEW)
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new POINTF());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_POINTF_DELETE)
{
  auto obj = static_cast<POINTF *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// FLOAT x

HB_FUNC_STATIC(WAS_POINTF_SETX)
{
  auto obj = static_cast<POINTF *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    obj->x = wa_par_FLOAT(1);
  }
}

HB_FUNC_STATIC(WAS_POINTF_GETX)
{
  auto obj = static_cast<POINTF *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_FLOAT(obj->x);
  }
}

// FLOAT y

HB_FUNC_STATIC(WAS_POINTF_SETY)
{
  auto obj = static_cast<POINTF *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    obj->y = wa_par_FLOAT(1);
  }
}

HB_FUNC_STATIC(WAS_POINTF_GETY)
{
  auto obj = static_cast<POINTF *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_FLOAT(obj->y);
  }
}

/*
typedef struct tagPOINTF {
  FLOAT x;
  FLOAT y;
} POINTF, *LPPOINTF;
*/

#pragma ENDDUMP
