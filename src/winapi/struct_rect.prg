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

CLASS WINAPI_STRUCT_RECT

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // LONG left
   ASSIGN left(n) INLINE ::setleft(n)
   ACCESS left INLINE ::getleft()
   METHOD setleft
   METHOD getleft

   // LONG top
   ASSIGN top(n) INLINE ::settop(n)
   ACCESS top INLINE ::gettop()
   METHOD settop
   METHOD gettop

   // LONG right
   ASSIGN right(n) INLINE ::setright(n)
   ACCESS right INLINE ::getright()
   METHOD setright
   METHOD getright

   // LONG bottom
   ASSIGN bottom(n) INLINE ::setbottom(n)
   ACCESS bottom INLINE ::getbottom()
   METHOD setbottom
   METHOD getbottom

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_RECT
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"

HB_FUNC_STATIC( WINAPI_STRUCT_RECT_NEW )
{
  auto obj = new RECT();
  PHB_ITEM self = hb_stackSelfItem();
  PHB_ITEM ptr = hb_itemPutPtr( nullptr, ( void * ) obj );
  hb_objSendMsg( self, "_ptr", 1, ptr );
  hb_itemRelease( ptr );
  PHB_ITEM des = hb_itemPutL( nullptr, true );
  hb_objSendMsg( self, "_SELF_DESTRUCTION", 1, des );
  hb_itemRelease( des );
  hb_itemReturn( self );
}

HB_FUNC_STATIC( WINAPI_STRUCT_RECT_DELETE )
{
  auto obj = static_cast<RECT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

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

// LONG left

HB_FUNC_STATIC( WINAPI_STRUCT_RECT_SETLEFT )
{
  auto obj = static_cast<RECT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->left = hb_parnl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_RECT_GETLEFT )
{
  auto obj = static_cast<RECT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->left);
  }
}

// LONG top

HB_FUNC_STATIC( WINAPI_STRUCT_RECT_SETTOP )
{
  auto obj = static_cast<RECT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->top = hb_parnl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_RECT_GETTOP )
{
  auto obj = static_cast<RECT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->top);
  }
}

// LONG right

HB_FUNC_STATIC( WINAPI_STRUCT_RECT_SETRIGHT )
{
  auto obj = static_cast<RECT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->right = hb_parnl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_RECT_GETRIGHT )
{
  auto obj = static_cast<RECT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->right);
  }
}

// LONG bottom

HB_FUNC_STATIC( WINAPI_STRUCT_RECT_SETBOTTOM )
{
  auto obj = static_cast<RECT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->bottom = hb_parnl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_RECT_GETBOTTOM )
{
  auto obj = static_cast<RECT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->bottom);
  }
}

/*
typedef struct tagRECT {
  LONG left;
  LONG top;
  LONG right;
  LONG bottom;
} RECT, *PRECT, *NPRECT, *LPRECT;
*/

#pragma ENDDUMP