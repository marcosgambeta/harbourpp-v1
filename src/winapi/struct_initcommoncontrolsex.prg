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

CLASS WINAPI_STRUCT_INITCOMMONCONTROLSEX

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD dwSize
   ASSIGN dwSize(n) INLINE ::setdwSize(n)
   ACCESS dwSize INLINE ::getdwSize()
   METHOD setdwSize
   METHOD getdwSize

   // DWORD dwICC
   ASSIGN dwICC(n) INLINE ::setdwICC(n)
   ACCESS dwICC INLINE ::getdwICC()
   METHOD setdwICC
   METHOD getdwICC

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_INITCOMMONCONTROLSEX
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"

HB_FUNC_STATIC( WINAPI_STRUCT_INITCOMMONCONTROLSEX_NEW )
{
  auto obj = new INITCOMMONCONTROLSEX();
  obj->dwSize = sizeof(INITCOMMONCONTROLSEX);
  PHB_ITEM self = hb_stackSelfItem();
  PHB_ITEM ptr = hb_itemPutPtr( nullptr, ( void * ) obj );
  hb_objSendMsg( self, "_ptr", 1, ptr );
  hb_itemRelease( ptr );
  PHB_ITEM des = hb_itemPutL( nullptr, true );
  hb_objSendMsg( self, "_SELF_DESTRUCTION", 1, des );
  hb_itemRelease( des );
  hb_itemReturn( self );
}

HB_FUNC_STATIC( WINAPI_STRUCT_INITCOMMONCONTROLSEX_DELETE )
{
  auto obj = static_cast<INITCOMMONCONTROLSEX*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

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

// DWORD dwSize

HB_FUNC_STATIC( WINAPI_STRUCT_INITCOMMONCONTROLSEX_SETDWSIZE )
{
  auto obj = static_cast<INITCOMMONCONTROLSEX*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->dwSize = static_cast<DWORD>(hb_parnl(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_INITCOMMONCONTROLSEX_GETDWSIZE )
{
  auto obj = static_cast<INITCOMMONCONTROLSEX*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->dwSize);
  }
}

// DWORD dwICC

HB_FUNC_STATIC( WINAPI_STRUCT_INITCOMMONCONTROLSEX_SETDWICC )
{
  auto obj = static_cast<INITCOMMONCONTROLSEX*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->dwICC = static_cast<DWORD>(hb_parnl(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_INITCOMMONCONTROLSEX_GETDWICC )
{
  auto obj = static_cast<INITCOMMONCONTROLSEX*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->dwICC);
  }
}

/*
typedef struct tagINITCOMMONCONTROLSEX {
  DWORD dwSize;
  DWORD dwICC;
} INITCOMMONCONTROLSEX, *LPINITCOMMONCONTROLSEX;
*/

#pragma ENDDUMP