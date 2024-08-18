//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

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

FUNCTION wasGRADIENT_TRIANGLE()
RETURN was_GRADIENT_TRIANGLE():new()

CLASS WAS_GRADIENT_TRIANGLE

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // ULONG Vertex1
   ASSIGN Vertex1(n) INLINE ::setVertex1(n)
   ACCESS Vertex1 INLINE ::getVertex1()
   METHOD setVertex1
   METHOD getVertex1

   // ULONG Vertex2
   ASSIGN Vertex2(n) INLINE ::setVertex2(n)
   ACCESS Vertex2 INLINE ::getVertex2()
   METHOD setVertex2
   METHOD getVertex2

   // ULONG Vertex3
   ASSIGN Vertex3(n) INLINE ::setVertex3(n)
   ACCESS Vertex3 INLINE ::getVertex3()
   METHOD setVertex3
   METHOD getVertex3

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_GRADIENT_TRIANGLE
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

HB_FUNC_STATIC( WAS_GRADIENT_TRIANGLE_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new GRADIENT_TRIANGLE());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_GRADIENT_TRIANGLE_DELETE )
{
  auto obj = static_cast<GRADIENT_TRIANGLE*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// ULONG Vertex1

HB_FUNC_STATIC( WAS_GRADIENT_TRIANGLE_SETVERTEX1 )
{
  auto obj = static_cast<GRADIENT_TRIANGLE*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->Vertex1 = wa_par_ULONG(1);
  }
}

HB_FUNC_STATIC( WAS_GRADIENT_TRIANGLE_GETVERTEX1 )
{
  auto obj = static_cast<GRADIENT_TRIANGLE*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_ULONG(obj->Vertex1);
  }
}

// ULONG Vertex2

HB_FUNC_STATIC( WAS_GRADIENT_TRIANGLE_SETVERTEX2 )
{
  auto obj = static_cast<GRADIENT_TRIANGLE*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->Vertex2 = wa_par_ULONG(1);
  }
}

HB_FUNC_STATIC( WAS_GRADIENT_TRIANGLE_GETVERTEX2 )
{
  auto obj = static_cast<GRADIENT_TRIANGLE*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_ULONG(obj->Vertex2);
  }
}

// ULONG Vertex3

HB_FUNC_STATIC( WAS_GRADIENT_TRIANGLE_SETVERTEX3 )
{
  auto obj = static_cast<GRADIENT_TRIANGLE*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->Vertex3 = wa_par_ULONG(1);
  }
}

HB_FUNC_STATIC( WAS_GRADIENT_TRIANGLE_GETVERTEX3 )
{
  auto obj = static_cast<GRADIENT_TRIANGLE*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_ULONG(obj->Vertex3);
  }
}

/*
typedef struct _GRADIENT_TRIANGLE {
  ULONG Vertex1;
  ULONG Vertex2;
  ULONG Vertex3;
} GRADIENT_TRIANGLE, *PGRADIENT_TRIANGLE, *LPGRADIENT_TRIANGLE;
*/

#pragma ENDDUMP
