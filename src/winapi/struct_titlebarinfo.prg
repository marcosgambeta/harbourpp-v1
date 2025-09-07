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

FUNCTION wasTITLEBARINFO()
RETURN was_TITLEBARINFO():new()

CLASS WAS_TITLEBARINFO

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD cbSize
   //ASSIGN cbSize(n) INLINE ::setcbSize(n)
   ACCESS cbSize INLINE ::getcbSize()
   //METHOD setcbSize
   METHOD getcbSize

   // RECT rcTitleBar
   // TODO:

   // DWORD rgstate[CCHILDREN_TITLEBAR + 1]
   //ASSIGN rgstate(n) INLINE ::setrgstate(n)
   ACCESS rgstate INLINE ::getrgstate()
   //METHOD setrgstate
   METHOD getrgstate

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_TITLEBARINFO
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

HB_FUNC_STATIC(WAS_TITLEBARINFO_NEW)
{
  auto obj = new TITLEBARINFO();
  obj->cbSize = sizeof(TITLEBARINFO);
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", obj);
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_TITLEBARINFO_DELETE)
{
  auto obj = static_cast<TITLEBARINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD cbSize

// HB_FUNC_STATIC(WAS_TITLEBARINFO_SETCBSIZE)
// {
//   auto obj = static_cast<TITLEBARINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if (obj != nullptr)
//   {
//     obj->cbSize = wa_par_DWORD(1);
//   }
// }

HB_FUNC_STATIC(WAS_TITLEBARINFO_GETCBSIZE)
{
  auto obj = static_cast<TITLEBARINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_DWORD(obj->cbSize);
  }
}

// RECT rcTitleBar
// TODO:

// DWORD rgstate[CCHILDREN_TITLEBAR + 1]

HB_FUNC_STATIC(WAS_TITLEBARINFO_GETRGSTATE)
{
  auto obj = static_cast<TITLEBARINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    hb_reta(6);
    hb_storvnl(obj->rgstate[0], -1, 1);
    hb_storvnl(obj->rgstate[1], -1, 2);
    hb_storvnl(obj->rgstate[2], -1, 3);
    hb_storvnl(obj->rgstate[3], -1, 4);
    hb_storvnl(obj->rgstate[4], -1, 5);
    hb_storvnl(obj->rgstate[5], -1, 6);
  }
}

/*
typedef struct tagTITLEBARINFO {
  DWORD cbSize;
  RECT  rcTitleBar;
  DWORD rgstate[CCHILDREN_TITLEBAR + 1];
} TITLEBARINFO, *PTITLEBARINFO, *LPTITLEBARINFO;
*/

#pragma ENDDUMP
