/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2023 Marcos Antonio Gambeta

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

CLASS WINAPI_STRUCT_SIZE

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // LONG cx
   ASSIGN cx(n) INLINE ::setcx(n)
   ACCESS cx INLINE ::getcx()
   METHOD setcx
   METHOD getcx

   // LONG cy
   ASSIGN cy(n) INLINE ::setcy(n)
   ACCESS cy INLINE ::getcy()
   METHOD setcy
   METHOD getcy

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_SIZE
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

HB_FUNC_STATIC( WINAPI_STRUCT_SIZE_NEW )
{
  PHB_ITEM self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new SIZE());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WINAPI_STRUCT_SIZE_DELETE )
{
  auto obj = static_cast<SIZE*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// LONG cx

HB_FUNC_STATIC( WINAPI_STRUCT_SIZE_SETCX )
{
  auto obj = static_cast<SIZE*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cx = winapi_par_LONG(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SIZE_GETCX )
{
  auto obj = static_cast<SIZE*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_LONG(obj->cx);
  }
}

// LONG cy

HB_FUNC_STATIC( WINAPI_STRUCT_SIZE_SETCY )
{
  auto obj = static_cast<SIZE*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cy = winapi_par_LONG(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SIZE_GETCY )
{
  auto obj = static_cast<SIZE*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_LONG(obj->cy);
  }
}

/*
typedef struct tagSIZE {
  LONG cx;
  LONG cy;
} SIZE, *PSIZE, *LPSIZE;
*/

#pragma ENDDUMP
