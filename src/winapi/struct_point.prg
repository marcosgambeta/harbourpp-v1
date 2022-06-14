/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (C) 2022 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2022 Marcos Antonio Gambeta

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

CLASS WINAPI_STRUCT_POINT

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // LONG x
   ASSIGN x(n) INLINE ::setx(n)
   ACCESS x INLINE ::getx()
   METHOD setx
   METHOD getx

   // LONG y
   ASSIGN y(n) INLINE ::sety(n)
   ACCESS y INLINE ::gety()
   METHOD sety
   METHOD gety

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_POINT
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"

HB_FUNC_STATIC( WINAPI_STRUCT_POINT_NEW )
{
  auto obj = new POINT();
  PHB_ITEM self = hb_stackSelfItem();
  PHB_ITEM ptr = hb_itemPutPtr( nullptr, ( void * ) obj );
  hb_objSendMsg( self, "_ptr", 1, ptr );
  hb_itemRelease( ptr );
  PHB_ITEM des = hb_itemPutL( nullptr, true );
  hb_objSendMsg( self, "_SELF_DESTRUCTION", 1, des );
  hb_itemRelease( des );
  hb_itemReturn( self );
}

HB_FUNC_STATIC( WINAPI_STRUCT_POINT_DELETE )
{
  auto obj = static_cast<POINT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    delete obj;
    obj = nullptr;
    PHB_ITEM self = hb_stackSelfItem();
    PHB_ITEM ptr = hb_itemPutPtr( nullptr, nullptr );
    hb_objSendMsg( self, "_ptr", 1, ptr );
    hb_itemRelease( ptr );
  }

  hb_itemReturn( hb_stackSelfItem() );
}

// LONG x

HB_FUNC_STATIC( WINAPI_STRUCT_POINT_SETX )
{
  auto obj = static_cast<POINT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->x = hb_parnl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_POINT_GETX )
{
  auto obj = static_cast<POINT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->x);
  }
}

// LONG y

HB_FUNC_STATIC( WINAPI_STRUCT_POINT_SETY )
{
  auto obj = static_cast<POINT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->y = hb_parnl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_POINT_GETY )
{
  auto obj = static_cast<POINT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->y);
  }
}

/*
typedef struct tagPOINT {
  LONG x;
  LONG y;
} POINT, *PPOINT, *NPPOINT, *LPPOINT;
*/

#pragma ENDDUMP
