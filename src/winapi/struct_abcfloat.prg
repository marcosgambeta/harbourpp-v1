/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2022-2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2022-2023 Marcos Antonio Gambeta

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

CLASS WINAPI_STRUCT_ABCFLOAT

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // float abcfA
   ASSIGN abcfA(n) INLINE ::setabcfA(n)
   ACCESS abcfA INLINE ::getabcfA()
   METHOD setabcfA
   METHOD getabcfA

   // float abcfB
   ASSIGN abcfB(n) INLINE ::setabcfB(n)
   ACCESS abcfB INLINE ::getabcfB()
   METHOD setabcfB
   METHOD getabcfB

   // float abcfC
   ASSIGN abcfC(n) INLINE ::setabcfC(n)
   ACCESS abcfC INLINE ::getabcfC()
   METHOD setabcfC
   METHOD getabcfC

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_ABCFLOAT
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.hpp"
#include "hbapiitm.h"
#include "hbapicls.h"

HB_FUNC_STATIC( WINAPI_STRUCT_ABCFLOAT_NEW )
{
  PHB_ITEM self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new ABCFLOAT());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WINAPI_STRUCT_ABCFLOAT_DELETE )
{
  auto obj = static_cast<ABCFLOAT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// float abcfA

HB_FUNC_STATIC( WINAPI_STRUCT_ABCFLOAT_SETABCFA )
{
  auto obj = static_cast<ABCFLOAT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->abcfA = static_cast<FLOAT>(hb_parnd(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_ABCFLOAT_GETABCFA )
{
  auto obj = static_cast<ABCFLOAT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnd(obj->abcfA);
  }
}

// float abcfB

HB_FUNC_STATIC( WINAPI_STRUCT_ABCFLOAT_SETABCFB )
{
  auto obj = static_cast<ABCFLOAT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->abcfB = static_cast<FLOAT>(hb_parnd(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_ABCFLOAT_GETABCFB )
{
  auto obj = static_cast<ABCFLOAT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnd(obj->abcfB);
  }
}

// float abcfC

HB_FUNC_STATIC( WINAPI_STRUCT_ABCFLOAT_SETABCFC )
{
  auto obj = static_cast<ABCFLOAT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->abcfC = static_cast<FLOAT>(hb_parnd(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_ABCFLOAT_GETABCFC )
{
  auto obj = static_cast<ABCFLOAT*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnd(obj->abcfC);
  }
}

/*
typedef struct _ABCFLOAT {
  FLOAT abcfA;
  FLOAT abcfB;
  FLOAT abcfC;
} ABCFLOAT, *PABCFLOAT, *NPABCFLOAT, *LPABCFLOAT;
*/

#pragma ENDDUMP
