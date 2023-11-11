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
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "winapi.hpp"

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new MSG());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_DELETE )
{
  auto obj = static_cast<MSG*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

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
  auto obj = static_cast<MSG*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hwnd = winapi_par_HWND(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_GETHWND )
{
  auto obj = static_cast<MSG*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_HWND(obj->hwnd);
  }
}

// UINT message

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_SETMESSAGE )
{
  auto obj = static_cast<MSG*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->message = winapi_par_UINT(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_GETMESSAGE )
{
  auto obj = static_cast<MSG*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_UINT(obj->message);
  }
}

// WPARAM wParam

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_SETWPARAM )
{
  auto obj = static_cast<MSG*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->wParam = winapi_par_WPARAM(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_GETWPARAM )
{
  auto obj = static_cast<MSG*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_WPARAM(obj->wParam);
  }
}

// LPARAM lParam

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_SETLPARAM )
{
  auto obj = static_cast<MSG*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->lParam = winapi_par_LPARAM(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_GETLPARAM )
{
  auto obj = static_cast<MSG*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_LPARAM(obj->lParam);
  }
}

// DWORD time

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_SETTIME )
{
  auto obj = static_cast<MSG*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->time = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_GETTIME )
{
  auto obj = static_cast<MSG*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->time);
  }
}

// POINT pt

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_SETPT )
{
  auto obj = static_cast<MSG*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    // obj->pt = ...; TODO: implementar
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_MSG_GETPT )
{
  auto obj = static_cast<MSG*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

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
