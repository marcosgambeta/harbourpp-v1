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

FUNCTION wasXFORM()
RETURN was_XFORM():new()

CLASS WAS_XFORM

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // FLOAT eM11
   ASSIGN eM11(n) INLINE ::seteM11(n)
   ACCESS eM11 INLINE ::geteM11()
   METHOD seteM11
   METHOD geteM11

   // FLOAT eM12
   ASSIGN eM12(n) INLINE ::seteM12(n)
   ACCESS eM12 INLINE ::geteM12()
   METHOD seteM12
   METHOD geteM12

   // FLOAT eM21
   ASSIGN eM21(n) INLINE ::seteM21(n)
   ACCESS eM21 INLINE ::geteM21()
   METHOD seteM21
   METHOD geteM21

   // FLOAT eM22
   ASSIGN eM22(n) INLINE ::seteM22(n)
   ACCESS eM22 INLINE ::geteM22()
   METHOD seteM22
   METHOD geteM22

   // FLOAT eDx
   ASSIGN eDx(n) INLINE ::seteDx(n)
   ACCESS eDx INLINE ::geteDx()
   METHOD seteDx
   METHOD geteDx

   // FLOAT eDy
   ASSIGN eDy(n) INLINE ::seteDy(n)
   ACCESS eDy INLINE ::geteDy()
   METHOD seteDy
   METHOD geteDy

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_XFORM
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

HB_FUNC_STATIC( WAS_XFORM_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new XFORM());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_XFORM_DELETE )
{
  auto obj = static_cast<XFORM*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// FLOAT eM11

HB_FUNC_STATIC( WAS_XFORM_SETEM11 )
{
  auto obj = static_cast<XFORM*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->eM11 = wa_par_FLOAT(1);
  }
}

HB_FUNC_STATIC( WAS_XFORM_GETEM11 )
{
  auto obj = static_cast<XFORM*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_FLOAT(obj->eM11);
  }
}

// FLOAT eM12

HB_FUNC_STATIC( WAS_XFORM_SETEM12 )
{
  auto obj = static_cast<XFORM*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->eM12 = wa_par_FLOAT(1);
  }
}

HB_FUNC_STATIC( WAS_XFORM_GETEM12 )
{
  auto obj = static_cast<XFORM*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_FLOAT(obj->eM12);
  }
}

// FLOAT eM21

HB_FUNC_STATIC( WAS_XFORM_SETEM21 )
{
  auto obj = static_cast<XFORM*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->eM21 = wa_par_FLOAT(1);
  }
}

HB_FUNC_STATIC( WAS_XFORM_GETEM21 )
{
  auto obj = static_cast<XFORM*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_FLOAT(obj->eM21);
  }
}

// FLOAT eM22

HB_FUNC_STATIC( WAS_XFORM_SETEM22 )
{
  auto obj = static_cast<XFORM*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->eM22 = wa_par_FLOAT(1);
  }
}

HB_FUNC_STATIC( WAS_XFORM_GETEM22 )
{
  auto obj = static_cast<XFORM*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_FLOAT(obj->eM22);
  }
}

// FLOAT eDx

HB_FUNC_STATIC( WAS_XFORM_SETEDX )
{
  auto obj = static_cast<XFORM*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->eDx = wa_par_FLOAT(1);
  }
}

HB_FUNC_STATIC( WAS_XFORM_GETEDX )
{
  auto obj = static_cast<XFORM*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_FLOAT(obj->eDx);
  }
}

// FLOAT eDy

HB_FUNC_STATIC( WAS_XFORM_SETEDY )
{
  auto obj = static_cast<XFORM*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->eDy = wa_par_FLOAT(1);
  }
}

HB_FUNC_STATIC( WAS_XFORM_GETEDY )
{
  auto obj = static_cast<XFORM*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_FLOAT(obj->eDy);
  }
}


/*
typedef struct tagXFORM {
  FLOAT eM11;
  FLOAT eM12;
  FLOAT eM21;
  FLOAT eM22;
  FLOAT eDx;
  FLOAT eDy;
} XFORM, *PXFORM, *LPXFORM;
*/

#pragma ENDDUMP
