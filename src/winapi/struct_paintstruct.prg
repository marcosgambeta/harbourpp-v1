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

FUNCTION wasPAINTSTRUCT()
RETURN was_PAINTSTRUCT():new()

CLASS WAS_PAINTSTRUCT

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // HDC hdc
   ASSIGN hdc(n) INLINE ::sethdc(n)
   ACCESS hdc INLINE ::gethdc()
   METHOD sethdc
   METHOD gethdc

   // BOOL fErase
   ASSIGN fErase(n) INLINE ::setfErase(n)
   ACCESS fErase INLINE ::getfErase()
   METHOD setfErase
   METHOD getfErase

   // RECT rcPaint

   // BOOL fRestore
   ASSIGN fRestore(n) INLINE ::setfRestore(n)
   ACCESS fRestore INLINE ::getfRestore()
   METHOD setfRestore
   METHOD getfRestore

   // BOOL fIncUpdate
   ASSIGN fIncUpdate(n) INLINE ::setfIncUpdate(n)
   ACCESS fIncUpdate INLINE ::getfIncUpdate()
   METHOD setfIncUpdate
   METHOD getfIncUpdate

   // BYTE rgbReserved[32]

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_PAINTSTRUCT
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

HB_FUNC_STATIC(WAS_PAINTSTRUCT_NEW)
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new PAINTSTRUCT());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_PAINTSTRUCT_DELETE)
{
  auto obj = static_cast<PAINTSTRUCT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// HDC hdc

HB_FUNC_STATIC(WAS_PAINTSTRUCT_SETHDC)
{
  auto obj = static_cast<PAINTSTRUCT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->hdc = wa_par_HDC(1);
  }
}

HB_FUNC_STATIC(WAS_PAINTSTRUCT_GETHDC)
{
  auto obj = static_cast<PAINTSTRUCT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_HDC(obj->hdc);
  }
}

// BOOL fErase

HB_FUNC_STATIC(WAS_PAINTSTRUCT_SETFERASE)
{
  auto obj = static_cast<PAINTSTRUCT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->fErase = wa_par_BOOL(1);
  }
}

HB_FUNC_STATIC(WAS_PAINTSTRUCT_GETFERASE)
{
  auto obj = static_cast<PAINTSTRUCT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_BOOL(obj->fErase);
  }
}

// RECT rcPaint

// BOOL fRestore

HB_FUNC_STATIC(WAS_PAINTSTRUCT_SETFRESTORE)
{
  auto obj = static_cast<PAINTSTRUCT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->fRestore = wa_par_BOOL(1);
  }
}

HB_FUNC_STATIC(WAS_PAINTSTRUCT_GETFRESTORE)
{
  auto obj = static_cast<PAINTSTRUCT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_BOOL(obj->fRestore);
  }
}

// BOOL fIncUpdate

HB_FUNC_STATIC(WAS_PAINTSTRUCT_SETFINCUPDATE)
{
  auto obj = static_cast<PAINTSTRUCT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->fIncUpdate = wa_par_BOOL(1);
  }
}

HB_FUNC_STATIC(WAS_PAINTSTRUCT_GETFINCUPDATE)
{
  auto obj = static_cast<PAINTSTRUCT *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_BOOL(obj->fIncUpdate);
  }
}

// BYTE rgbReserved[32]

/*
typedef struct tagPAINTSTRUCT {
  HDC  hdc;
  BOOL fErase;
  RECT rcPaint;
  BOOL fRestore;
  BOOL fIncUpdate;
  BYTE rgbReserved[32];
} PAINTSTRUCT, *PPAINTSTRUCT, *NPPAINTSTRUCT, *LPPAINTSTRUCT;
*/

#pragma ENDDUMP
