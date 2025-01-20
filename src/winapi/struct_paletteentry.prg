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

FUNCTION wasPALETTEENTRY()
RETURN was_PALETTEENTRY():new()

CLASS WAS_PALETTEENTRY

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // BYTE peRed

   ASSIGN peRed(n) INLINE ::setpeRed(n)
   ACCESS peRed INLINE ::getpeRed()
   METHOD setpeRed
   METHOD getpeRed

   // BYTE peGreen

   ASSIGN peGreen(n) INLINE ::setpeGreen(n)
   ACCESS peGreen INLINE ::getpeGreen()
   METHOD setpeGreen
   METHOD getpeGreen

   // BYTE peBlue

   ASSIGN peBlue(n) INLINE ::setpeBlue(n)
   ACCESS peBlue INLINE ::getpeBlue()
   METHOD setpeBlue
   METHOD getpeBlue

   // BYTE peFlags

   ASSIGN peFlags(n) INLINE ::setpeFlags(n)
   ACCESS peFlags INLINE ::getpeFlags()
   METHOD setpeFlags
   METHOD getpeFlags

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_PALETTEENTRY
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

HB_FUNC_STATIC(WAS_PALETTEENTRY_NEW)
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new PALETTEENTRY());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_PALETTEENTRY_DELETE)
{
  auto obj = static_cast<PALETTEENTRY *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// BYTE peRed

HB_FUNC_STATIC(WAS_PALETTEENTRY_SETPERED)
{
  auto obj = static_cast<PALETTEENTRY *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->peRed = wa_par_BYTE(1);
  }
}

HB_FUNC_STATIC(WAS_PALETTEENTRY_GETPERED)
{
  auto obj = static_cast<PALETTEENTRY *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_BYTE(obj->peRed);
  }
}

// BYTE peGreen

HB_FUNC_STATIC(WAS_PALETTEENTRY_SETPEGREEN)
{
  auto obj = static_cast<PALETTEENTRY *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->peGreen = wa_par_BYTE(1);
  }
}

HB_FUNC_STATIC(WAS_PALETTEENTRY_GETPEGREEN)
{
  auto obj = static_cast<PALETTEENTRY *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_BYTE(obj->peGreen);
  }
}

// BYTE peBlue

HB_FUNC_STATIC(WAS_PALETTEENTRY_SETPEBLUE)
{
  auto obj = static_cast<PALETTEENTRY *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->peBlue = wa_par_BYTE(1);
  }
}

HB_FUNC_STATIC(WAS_PALETTEENTRY_GETPEBLUE)
{
  auto obj = static_cast<PALETTEENTRY *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_BYTE(obj->peBlue);
  }
}

// BYTE peFlags

HB_FUNC_STATIC(WAS_PALETTEENTRY_SETPEFLAGS)
{
  auto obj = static_cast<PALETTEENTRY *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->peFlags = wa_par_BYTE(1);
  }
}

HB_FUNC_STATIC(WAS_PALETTEENTRY_GETPEFLAGS)
{
  auto obj = static_cast<PALETTEENTRY *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_BYTE(obj->peFlags);
  }
}

/*
typedef struct tagPALETTEENTRY {
  BYTE peRed;
  BYTE peGreen;
  BYTE peBlue;
  BYTE peFlags;
} PALETTEENTRY;
*/

#pragma ENDDUMP
