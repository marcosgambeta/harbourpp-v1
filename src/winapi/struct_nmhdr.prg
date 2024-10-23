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

FUNCTION wasNMHDR()
RETURN was_NMHDR():new()

CLASS WAS_NMHDR

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // HWND hwndFrom
   ASSIGN hwndFrom(p) INLINE ::sethwndFrom(p)
   ACCESS hwndFrom INLINE ::gethwndFrom()
   METHOD sethwndFrom
   METHOD gethwndFrom

   // UINT_PTR idFrom
   ASSIGN idFrom(n) INLINE ::setidFrom(n)
   ACCESS idFrom INLINE ::getidFrom()
   METHOD setidFrom
   METHOD getidFrom

   // UINT code
   ASSIGN code(n) INLINE ::setcode(n)
   ACCESS code INLINE ::getcode()
   METHOD setcode
   METHOD getcode

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_NMHDR
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

HB_FUNC_STATIC(WAS_NMHDR_NEW)
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new NMHDR());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_NMHDR_DELETE)
{
  auto obj = static_cast<NMHDR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// HWND hwndFrom

HB_FUNC_STATIC(WAS_NMHDR_SETHWNDFROM)
{
  auto obj = static_cast<NMHDR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hwndFrom = wa_par_HWND(1);
  }
}

HB_FUNC_STATIC(WAS_NMHDR_GETHWNDFROM)
{
  auto obj = static_cast<NMHDR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HWND(obj->hwndFrom);
  }
}

// UINT_PTR idFrom

HB_FUNC_STATIC(WAS_NMHDR_SETIDFROM)
{
  auto obj = static_cast<NMHDR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->idFrom = wa_par_UINT_PTR(1);
  }
}

HB_FUNC_STATIC(WAS_NMHDR_GETIDFROM)
{
  auto obj = static_cast<NMHDR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT_PTR(obj->idFrom);
  }
}

// UINT code

HB_FUNC_STATIC(WAS_NMHDR_SETCODE)
{
  auto obj = static_cast<NMHDR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->code = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC(WAS_NMHDR_GETCODE)
{
  auto obj = static_cast<NMHDR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->code);
  }
}

/*
typedef struct tagNMHDR {
  HWND     hwndFrom;
  UINT_PTR idFrom;
  UINT     code;
} NMHDR;
*/

#pragma ENDDUMP
