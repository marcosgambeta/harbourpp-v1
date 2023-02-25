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

CLASS WINAPI_STRUCT_MSG

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // HWND hwnd
   ASSIGN hwnd(p) INLINE ::sethwnd(p)
   ACCESS hwnd INLINE ::gethwnd()
   METHOD sethwnd
   METHOD gethwnd

   // UINT message
   ASSIGN message(n) INLINE ::setmessage(n)
   ACCESS message INLINE ::getmessage()
   METHOD setmessage
   METHOD getmessage

   // WPARAM wParam
   ASSIGN wParam(n) INLINE ::setwParam(n)
   ACCESS wParam INLINE ::getwParam()
   METHOD setwParam
   METHOD getwParam

   // LPARAM lParam
   ASSIGN lParam(n) INLINE ::setlParam(n)
   ACCESS lParam INLINE ::getlParam()
   METHOD setLParam
   METHOD getLParam

   // DWORD time
   ASSIGN time(n) INLINE ::settime(n)
   ACCESS time INLINE ::gettime()
   METHOD setTime
   METHOD getTime

   // POINT pt
   ASSIGN pt(p) INLINE ::setpt(p)
   ACCESS pt INLINE ::getpt()
   METHOD setPt
   METHOD getPt

   // DWORD lPrivate

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_MSG
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.hpp"
#include "hbapiitm.h"
#include "hbapicls.h"

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_NEW )
{
  PHB_ITEM self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new MSG());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_DELETE )
{
  auto obj = static_cast<MSG*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// HWND hwnd

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_SETHWND )
{
  auto obj = static_cast<MSG*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->hwnd = static_cast<HWND>(hb_parptr(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_GETHWND )
{
  auto obj = static_cast<MSG*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retptr(obj->hwnd);
  }
}

// UINT message

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_SETMESSAGE )
{
  auto obj = static_cast<MSG*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->message = static_cast<UINT>(hb_parni(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_GETMESSAGE )
{
  auto obj = static_cast<MSG*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retni(obj->message);
  }
}

// WPARAM wParam

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_SETWPARAM )
{
  auto obj = static_cast<MSG*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->wParam = static_cast<WPARAM>(hb_parnl(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_GETWPARAM )
{
  auto obj = static_cast<MSG*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->wParam);
  }
}

// LPARAM lParam

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_SETLPARAM )
{
  auto obj = static_cast<MSG*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->lParam = static_cast<LPARAM>(hb_parnl(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_GETLPARAM )
{
  auto obj = static_cast<MSG*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->lParam);
  }
}

// DWORD time

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_SETTIME )
{
  auto obj = static_cast<MSG*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->time = static_cast<DWORD>(hb_parnl(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_GETTIME )
{
  auto obj = static_cast<MSG*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->time);
  }
}

// POINT pt

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_SETPT )
{
  auto obj = static_cast<MSG*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    // obj->pt = ...; TODO: implementar
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_GETPT )
{
  auto obj = static_cast<MSG*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    //hb_ret...(obj->pt); TODO: implementar
  }
}

/*
typedef struct tagMSG {
  HWND   hwnd;
  UINT   message;
  WPARAM wParam;
  LPARAM lParam;
  DWORD  time;
  POINT  pt;
  DWORD  lPrivate;
} MSG, *PMSG, *NPMSG, *LPMSG;
*/

#pragma ENDDUMP
