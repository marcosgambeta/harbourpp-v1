/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2024 Marcos Antonio Gambeta

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/*
  NOTE: source code generated with the help of a code generator
*/

#include "hbclass.ch"

FUNCTION wasKERNINGPAIR()
RETURN was_KERNINGPAIR():new()

CLASS WAS_KERNINGPAIR

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // WORD wFirst
   ASSIGN wFirst(n) INLINE ::setwFirst(n)
   ACCESS wFirst INLINE ::getwFirst()
   METHOD setwFirst
   METHOD getwFirst

   // WORD wSecond
   ASSIGN wSecond(n) INLINE ::setwSecond(n)
   ACCESS wSecond INLINE ::getwSecond()
   METHOD setwSecond
   METHOD getwSecond

   // int iKernAmount
   ASSIGN iKernAmount(n) INLINE ::setiKernAmount(n)
   ACCESS iKernAmount INLINE ::getiKernAmount()
   METHOD setiKernAmount
   METHOD getiKernAmount

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_KERNINGPAIR
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

HB_FUNC_STATIC( WAS_KERNINGPAIR_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new KERNINGPAIR());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_KERNINGPAIR_DELETE )
{
  auto obj = static_cast<KERNINGPAIR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// WORD wFirst

HB_FUNC_STATIC( WAS_KERNINGPAIR_SETWFIRST )
{
  auto obj = static_cast<KERNINGPAIR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->wFirst = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC( WAS_KERNINGPAIR_GETWFIRST )
{
  auto obj = static_cast<KERNINGPAIR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->wFirst);
  }
}

// WORD wSecond

HB_FUNC_STATIC( WAS_KERNINGPAIR_SETWSECOND )
{
  auto obj = static_cast<KERNINGPAIR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->wSecond = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC( WAS_KERNINGPAIR_GETWSECOND )
{
  auto obj = static_cast<KERNINGPAIR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->wSecond);
  }
}

// int iKernAmount

HB_FUNC_STATIC( WAS_KERNINGPAIR_SETIKERNAMOUNT )
{
  auto obj = static_cast<KERNINGPAIR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->iKernAmount = wa_par_int(1);
  }
}

HB_FUNC_STATIC( WAS_KERNINGPAIR_GETIKERNAMOUNT )
{
  auto obj = static_cast<KERNINGPAIR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_int(obj->iKernAmount);
  }
}

/*
typedef struct tagKERNINGPAIR {
  WORD wFirst;
  WORD wSecond;
  int  iKernAmount;
} KERNINGPAIR, *LPKERNINGPAIR;
*/

#pragma ENDDUMP
