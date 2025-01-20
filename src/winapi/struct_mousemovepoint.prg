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

FUNCTION wasMOUSEMOVEPOINT()
RETURN was_MOUSEMOVEPOINT():new()

CLASS WAS_MOUSEMOVEPOINT

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

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

   // DWORD time
   ASSIGN time(n) INLINE ::settime(n)
   ACCESS time INLINE ::gettime()
   METHOD settime
   METHOD gettime

   // ULONG_PTR dwExtraInfo
   ASSIGN dwExtraInfo(n) INLINE ::setdwExtraInfo(n)
   ACCESS dwExtraInfo INLINE ::getdwExtraInfo()
   METHOD setdwExtraInfo
   METHOD getdwExtraInfo

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_MOUSEMOVEPOINT
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

HB_FUNC_STATIC(WAS_MOUSEMOVEPOINT_NEW)
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new MOUSEMOVEPOINT());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_MOUSEMOVEPOINT_DELETE)
{
  auto obj = static_cast<MOUSEMOVEPOINT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// int x

HB_FUNC_STATIC(WAS_MOUSEMOVEPOINT_SETX)
{
  auto obj = static_cast<MOUSEMOVEPOINT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->x = wa_par_int(1);
  }
}

HB_FUNC_STATIC(WAS_MOUSEMOVEPOINT_GETX)
{
  auto obj = static_cast<MOUSEMOVEPOINT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_int(obj->x);
  }
}

// int y

HB_FUNC_STATIC(WAS_MOUSEMOVEPOINT_SETY)
{
  auto obj = static_cast<MOUSEMOVEPOINT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->y = wa_par_int(1);
  }
}

HB_FUNC_STATIC(WAS_MOUSEMOVEPOINT_GETY)
{
  auto obj = static_cast<MOUSEMOVEPOINT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_int(obj->y);
  }
}

// DWORD time

HB_FUNC_STATIC(WAS_MOUSEMOVEPOINT_SETTIME)
{
  auto obj = static_cast<MOUSEMOVEPOINT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->time = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_MOUSEMOVEPOINT_GETTIME)
{
  auto obj = static_cast<MOUSEMOVEPOINT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_DWORD(obj->time);
  }
}

// ULONG_PTR dwExtraInfo

HB_FUNC_STATIC(WAS_MOUSEMOVEPOINT_SETDWEXTRAINFO)
{
  auto obj = static_cast<MOUSEMOVEPOINT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->dwExtraInfo = wa_par_ULONG_PTR(1);
  }
}

HB_FUNC_STATIC(WAS_MOUSEMOVEPOINT_GETDWEXTRAINFO)
{
  auto obj = static_cast<MOUSEMOVEPOINT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_ULONG_PTR(obj->dwExtraInfo);
  }
}

/*
typedef struct tagMOUSEMOVEPOINT {
  int       x;
  int       y;
  DWORD     time;
  ULONG_PTR dwExtraInfo;
} MOUSEMOVEPOINT, *PMOUSEMOVEPOINT, *LPMOUSEMOVEPOINT;
*/

#pragma ENDDUMP
