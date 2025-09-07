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

FUNCTION wasACCEL()
RETURN was_ACCEL():new()

CLASS WAS_ACCEL

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // BYTE fVirt
   ASSIGN fVirt(n) INLINE ::setfVirt(n)
   ACCESS fVirt INLINE ::getfVirt()
   METHOD setfVirt
   METHOD getfVirt

   // WORD key
   ASSIGN key(n) INLINE ::setkey(n)
   ACCESS key INLINE ::getkey()
   METHOD setkey
   METHOD getkey

   // WORD cmd
   ASSIGN cmd(n) INLINE ::setcmd(n)
   ACCESS cmd INLINE ::getcmd()
   METHOD setcmd
   METHOD getcmd

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_ACCEL
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

HB_FUNC_STATIC(WAS_ACCEL_NEW)
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new ACCEL());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_ACCEL_DELETE)
{
  auto obj = static_cast<ACCEL *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// BYTE fVirt

HB_FUNC_STATIC(WAS_ACCEL_SETFVIRT)
{
  auto obj = static_cast<ACCEL *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    obj->fVirt = wa_par_BYTE(1);
  }
}

HB_FUNC_STATIC(WAS_ACCEL_GETFVIRT)
{
  auto obj = static_cast<ACCEL *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_BYTE(obj->fVirt);
  }
}

// WORD key

HB_FUNC_STATIC(WAS_ACCEL_SETKEY)
{
  auto obj = static_cast<ACCEL *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    obj->key = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC(WAS_ACCEL_GETKEY)
{
  auto obj = static_cast<ACCEL *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_WORD(obj->key);
  }
}

// WORD cmd

HB_FUNC_STATIC(WAS_ACCEL_SETCMD)
{
  auto obj = static_cast<ACCEL *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    obj->cmd = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC(WAS_ACCEL_GETCMD)
{
  auto obj = static_cast<ACCEL *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr) {
    wa_ret_WORD(obj->cmd);
  }
}

/*
typedef struct tagACCEL {
#if ...
  BYTE  fVirt;
#if ...
  WORD  key;
#if ...
  WORD  cmd;
#else
  WORD  fVirt;
#endif
#else
  WORD  key;
#endif
#else
  DWORD cmd;
#endif
} ACCEL, *LPACCEL;
*/

#pragma ENDDUMP
