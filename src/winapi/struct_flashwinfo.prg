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

FUNCTION wasFLASHWINFO()
RETURN was_FLASHWINFO():new()

CLASS WAS_FLASHWINFO

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // UINT cbSize
   //ASSIGN cbSize(n) INLINE ::setcbSize(n)
   ACCESS cbSize INLINE ::getcbSize()
   //METHOD setcbSize
   METHOD getcbSize

   // HWND hwnd
   ASSIGN hwnd(n) INLINE ::sethwnd(n)
   ACCESS hwnd INLINE ::gethwnd()
   METHOD sethwnd
   METHOD gethwnd

   // DWORD dwFlags
   ASSIGN dwFlags(n) INLINE ::setdwFlags(n)
   ACCESS dwFlags INLINE ::getdwFlags()
   METHOD setdwFlags
   METHOD getdwFlags

   // UINT uCount
   ASSIGN uCount(n) INLINE ::setuCount(n)
   ACCESS uCount INLINE ::getuCount()
   METHOD setuCount
   METHOD getuCount

   // DWORD dwTimeout
   ASSIGN dwTimeout(n) INLINE ::setdwTimeout(n)
   ACCESS dwTimeout INLINE ::getdwTimeout()
   METHOD setdwTimeout
   METHOD getdwTimeout

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_FLASHWINFO
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

HB_FUNC_STATIC(WAS_FLASHWINFO_NEW)
{
  auto obj = new FLASHWINFO();
  obj->cbSize = sizeof(FLASHWINFO);
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", obj);
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_FLASHWINFO_DELETE)
{
  auto obj = static_cast<FLASHWINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// UINT cbSize

// HB_FUNC_STATIC(WAS_FLASHWINFO_SETCBSIZE)
// {
//   auto obj = static_cast<FLASHWINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if (obj != nullptr)
//   {
//     obj->cbSize = wa_par_UINT(1);
//   }
// }

HB_FUNC_STATIC(WAS_FLASHWINFO_GETCBSIZE)
{
  auto obj = static_cast<FLASHWINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_UINT(obj->cbSize);
  }
}

// HWND hwnd

HB_FUNC_STATIC(WAS_FLASHWINFO_SETHWND)
{
  auto obj = static_cast<FLASHWINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    obj->hwnd = wa_par_HWND(1);
  }
}

HB_FUNC_STATIC(WAS_FLASHWINFO_GETHWND)
{
  auto obj = static_cast<FLASHWINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_HWND(obj->hwnd);
  }
}

// DWORD dwFlags

HB_FUNC_STATIC(WAS_FLASHWINFO_SETDWFLAGS)
{
  auto obj = static_cast<FLASHWINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    obj->dwFlags = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_FLASHWINFO_GETDWFLAGS)
{
  auto obj = static_cast<FLASHWINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_DWORD(obj->dwFlags);
  }
}

// UINT uCount

HB_FUNC_STATIC(WAS_FLASHWINFO_SETUCOUNT)
{
  auto obj = static_cast<FLASHWINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    obj->uCount = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC(WAS_FLASHWINFO_GETUCOUNT)
{
  auto obj = static_cast<FLASHWINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_UINT(obj->uCount);
  }
}

// DWORD dwTimeout

HB_FUNC_STATIC(WAS_FLASHWINFO_SETDWTIMEOUT)
{
  auto obj = static_cast<FLASHWINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    obj->dwTimeout = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_FLASHWINFO_GETDWTIMEOUT)
{
  auto obj = static_cast<FLASHWINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_DWORD(obj->dwTimeout);
  }
}

/*
typedef struct {
  UINT  cbSize;
  HWND  hwnd;
  DWORD dwFlags;
  UINT  uCount;
  DWORD dwTimeout;
} FLASHWINFO, *PFLASHWINFO;
*/

#pragma ENDDUMP
