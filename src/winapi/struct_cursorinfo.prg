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

FUNCTION wasCURSORINFO()
RETURN was_CURSORINFO():new()

CLASS WAS_CURSORINFO

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD cbSize
   //ASSIGN cbSize(n) INLINE ::setcbSize(n)
   ACCESS cbSize INLINE ::getcbSize()
   //METHOD setcbSize
   METHOD getcbSize

   // DWORD flags
   ASSIGN flags(n) INLINE ::setflags(n)
   ACCESS flags INLINE ::getflags()
   METHOD setflags
   METHOD getflags

   // HCURSOR hCursor
   ASSIGN hCursor(n) INLINE ::sethCursor(n)
   ACCESS hCursor INLINE ::gethCursor()
   METHOD sethCursor
   METHOD gethCursor

   // POINT ptScreenPos
   // TODO:

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_CURSORINFO
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

// clang-format on

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "winapi.hpp"

HB_FUNC_STATIC(WAS_CURSORINFO_NEW)
{
  auto obj = new CURSORINFO();
  obj->cbSize = sizeof(CURSORINFO);
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", obj);
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_CURSORINFO_DELETE)
{
  auto obj = static_cast<CURSORINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD cbSize

// HB_FUNC_STATIC(WAS_CURSORINFO_SETCBSIZE)
// {
//   auto obj = static_cast<CURSORINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if (obj != nullptr)
//   {
//     obj->cbSize = wa_par_DWORD(1);
//   }
// }

HB_FUNC_STATIC(WAS_CURSORINFO_GETCBSIZE)
{
  auto obj = static_cast<CURSORINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_DWORD(obj->cbSize);
  }
}

// DWORD flags

HB_FUNC_STATIC(WAS_CURSORINFO_SETFLAGS)
{
  auto obj = static_cast<CURSORINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    obj->flags = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_CURSORINFO_GETFLAGS)
{
  auto obj = static_cast<CURSORINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_DWORD(obj->flags);
  }
}

// HCURSOR hCursor

HB_FUNC_STATIC(WAS_CURSORINFO_SETHCURSOR)
{
  auto obj = static_cast<CURSORINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    obj->hCursor = wa_par_HCURSOR(1);
  }
}

HB_FUNC_STATIC(WAS_CURSORINFO_GETHCURSOR)
{
  auto obj = static_cast<CURSORINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_HCURSOR(obj->hCursor);
  }
}

// POINT ptScreenPos
// TODO:

/*
typedef struct tagCURSORINFO {
  DWORD   cbSize;
  DWORD   flags;
  HCURSOR hCursor;
  POINT   ptScreenPos;
} CURSORINFO, *PCURSORINFO, *LPCURSORINFO;
*/

#pragma ENDDUMP
