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

FUNCTION wasCOMSTAT()
RETURN was_COMSTAT():new()

CLASS WAS_COMSTAT

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD fCtsHold : 1
   ASSIGN fCtsHold(n) INLINE ::setfCtsHold(n)
   ACCESS fCtsHold INLINE ::getfCtsHold()
   METHOD setfCtsHold
   METHOD getfCtsHold

   // DWORD fDsrHold : 1
   ASSIGN fDsrHold(n) INLINE ::setfDsrHold(n)
   ACCESS fDsrHold INLINE ::getfDsrHold()
   METHOD setfDsrHold
   METHOD getfDsrHold

   // DWORD fRlsdHold : 1
   ASSIGN fRlsdHold(n) INLINE ::setfRlsdHold(n)
   ACCESS fRlsdHold INLINE ::getfRlsdHold()
   METHOD setfRlsdHold
   METHOD getfRlsdHold

   // DWORD fXoffHold : 1
   ASSIGN fXoffHold(n) INLINE ::setfXoffHold(n)
   ACCESS fXoffHold INLINE ::getfXoffHold()
   METHOD setfXoffHold
   METHOD getfXoffHold

   // DWORD fXoffSent : 1
   ASSIGN fXoffSent(n) INLINE ::setfXoffSent(n)
   ACCESS fXoffSent INLINE ::getfXoffSent()
   METHOD setfXoffSent
   METHOD getfXoffSent

   // DWORD fEof : 1
   ASSIGN fEof(n) INLINE ::setfEof(n)
   ACCESS fEof INLINE ::getfEof()
   METHOD setfEof
   METHOD getfEof

   // DWORD fTxim : 1
   ASSIGN fTxim(n) INLINE ::setfTxim(n)
   ACCESS fTxim INLINE ::getfTxim()
   METHOD setfTxim
   METHOD getfTxim

   // DWORD fReserved : 25
   ASSIGN fReserved(n) INLINE ::setfReserved(n)
   ACCESS fReserved INLINE ::getfReserved()
   METHOD setfReserved
   METHOD getfReserved

   // DWORD cbInQue
   ASSIGN cbInQue(n) INLINE ::setcbInQue(n)
   ACCESS cbInQue INLINE ::getcbInQue()
   METHOD setcbInQue
   METHOD getcbInQue

   // DWORD cbOutQue
   ASSIGN cbOutQue(n) INLINE ::setcbOutQue(n)
   ACCESS cbOutQue INLINE ::getcbOutQue()
   METHOD setcbOutQue
   METHOD getcbOutQue

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_COMSTAT
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

HB_FUNC_STATIC(WAS_COMSTAT_NEW)
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new COMSTAT());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_COMSTAT_DELETE)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD fCtsHold : 1

HB_FUNC_STATIC(WAS_COMSTAT_SETFCTSHOLD)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fCtsHold = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_COMSTAT_GETFCTSHOLD)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->fCtsHold);
  }
}

// DWORD fDsrHold : 1

HB_FUNC_STATIC(WAS_COMSTAT_SETFDSRHOLD)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fDsrHold = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_COMSTAT_GETFDSRHOLD)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->fDsrHold);
  }
}

// DWORD fRlsdHold : 1

HB_FUNC_STATIC(WAS_COMSTAT_SETFRLSDHOLD)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fRlsdHold = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_COMSTAT_GETFRLSDHOLD)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->fRlsdHold);
  }
}

// DWORD fXoffHold : 1

HB_FUNC_STATIC(WAS_COMSTAT_SETFXOFFHOLD)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fXoffHold = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_COMSTAT_GETFXOFFHOLD)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->fXoffHold);
  }
}

// DWORD fXoffSent : 1

HB_FUNC_STATIC(WAS_COMSTAT_SETFXOFFSENT)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fXoffSent = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_COMSTAT_GETFXOFFSENT)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->fXoffSent);
  }
}

// DWORD fEof : 1

HB_FUNC_STATIC(WAS_COMSTAT_SETFEOF)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fEof = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_COMSTAT_GETFEOF)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->fEof);
  }
}

// DWORD fTxim : 1

HB_FUNC_STATIC(WAS_COMSTAT_SETFTXIM)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fTxim = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_COMSTAT_GETFTXIM)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->fTxim);
  }
}

// DWORD fReserved : 25

HB_FUNC_STATIC(WAS_COMSTAT_SETFRESERVED)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fReserved = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_COMSTAT_GETFRESERVED)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->fReserved);
  }
}

// DWORD cbInQue

HB_FUNC_STATIC(WAS_COMSTAT_SETCBINQUE)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cbInQue = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_COMSTAT_GETCBINQUE)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->cbInQue);
  }
}

// DWORD cbOutQue

HB_FUNC_STATIC(WAS_COMSTAT_SETCBOUTQUE)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cbOutQue = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_COMSTAT_GETCBOUTQUE)
{
  auto obj = static_cast<COMSTAT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->cbOutQue);
  }
}

/*
typedef struct _COMSTAT {
  DWORD fCtsHold : 1;
  DWORD fDsrHold : 1;
  DWORD fRlsdHold : 1;
  DWORD fXoffHold : 1;
  DWORD fXoffSent : 1;
  DWORD fEof : 1;
  DWORD fTxim : 1;
  DWORD fReserved : 25;
  DWORD cbInQue;
  DWORD cbOutQue;
} COMSTAT, *LPCOMSTAT;
*/

#pragma ENDDUMP
