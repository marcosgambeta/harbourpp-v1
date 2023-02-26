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

CLASS WINAPI_STRUCT_FILETIME

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD dwLowDateTime
   ASSIGN dwLowDateTime(n) INLINE ::setdwLowDateTime(n)
   ACCESS dwLowDateTime INLINE ::getdwLowDateTime()
   METHOD setdwLowDateTime
   METHOD getdwLowDateTime

   // DWORD dwHighDateTime
   ASSIGN dwHighDateTime(n) INLINE ::setdwHighDateTime(n)
   ACCESS dwHighDateTime INLINE ::getdwHighDateTime()
   METHOD setdwHighDateTime
   METHOD getdwHighDateTime

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_FILETIME
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"

HB_FUNC_STATIC( WINAPI_STRUCT_FILETIME_NEW )
{
  PHB_ITEM self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new FILETIME());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WINAPI_STRUCT_FILETIME_DELETE )
{
  auto obj = static_cast<FILETIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD dwLowDateTime

HB_FUNC_STATIC( WINAPI_STRUCT_FILETIME_SETDWLOWDATETIME )
{
  auto obj = static_cast<FILETIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->dwLowDateTime = hb_parnl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_FILETIME_GETDWLOWDATETIME )
{
  auto obj = static_cast<FILETIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->dwLowDateTime);
  }
}

// DWORD dwHighDateTime

HB_FUNC_STATIC( WINAPI_STRUCT_FILETIME_SETDWHIGHDATETIME )
{
  auto obj = static_cast<FILETIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->dwHighDateTime = hb_parnl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_FILETIME_GETDWHIGHDATETIME )
{
  auto obj = static_cast<FILETIME*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->dwHighDateTime);
  }
}

/*
typedef struct _FILETIME {
  DWORD dwLowDateTime;
  DWORD dwHighDateTime;
} FILETIME, *PFILETIME, *LPFILETIME;
*/

#pragma ENDDUMP
