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

FUNCTION wasWNDCLASS()
RETURN was_WNDCLASS():new()

CLASS WAS_WNDCLASS

   DATA ptr
   DATA self_destruction INIT .F.

   DATA strMenuName
   DATA strClassName

   METHOD new
   METHOD delete

   // UINT style
   ASSIGN style(n) INLINE ::setstyle(n)
   ACCESS style INLINE ::getstyle()
   METHOD setstyle
   METHOD getstyle

   // WNDPROC lpfnWndProc
   ASSIGN lpfnWndProc(n) INLINE ::setlpfnWndProc(n)
   ACCESS lpfnWndProc INLINE ::getlpfnWndProc()
   METHOD setlpfnWndProc
   METHOD getlpfnWndProc

   // int cbClsExtra
   ASSIGN cbClsExtra(n) INLINE ::setcbClsExtra(n)
   ACCESS cbClsExtra INLINE ::getcbClsExtra()
   METHOD setcbClsExtra
   METHOD getcbClsExtra

   // int cbWndExtra
   ASSIGN cbWndExtra(n) INLINE ::setcbWndExtra(n)
   ACCESS cbWndExtra INLINE ::getcbWndExtra()
   METHOD setcbWndExtra
   METHOD getcbWndExtra

   // HINSTANCE hInstance
   ASSIGN hInstance(n) INLINE ::sethInstance(n)
   ACCESS hInstance INLINE ::gethInstance()
   METHOD sethInstance
   METHOD gethInstance

   // HICON hIcon
   ASSIGN hIcon(n) INLINE ::sethIcon(n)
   ACCESS hIcon INLINE ::gethIcon()
   METHOD sethIcon
   METHOD gethIcon

   // HCURSOR hCursor
   ASSIGN hCursor(n) INLINE ::sethCursor(n)
   ACCESS hCursor INLINE ::gethCursor()
   METHOD sethCursor
   METHOD gethCursor

   // HBRUSH hbrBackground
   ASSIGN hbrBackground(n) INLINE ::sethbrBackground(n)
   ACCESS hbrBackground INLINE ::gethbrBackground()
   METHOD sethbrBackground
   METHOD gethbrBackground

   // LPCWSTR lpszMenuName
   ASSIGN lpszMenuName(n) INLINE ::setlpszMenuName(n)
   ACCESS lpszMenuName INLINE ::getlpszMenuName()
   METHOD setlpszMenuName
   METHOD getlpszMenuName

   // LPCWSTR lpszClassName
   ASSIGN lpszClassName(n) INLINE ::setlpszClassName(n)
   ACCESS lpszClassName INLINE ::getlpszClassName()
   METHOD setlpszClassName
   METHOD getlpszClassName

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_WNDCLASS
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

HB_FUNC_STATIC( WAS_WNDCLASS_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new WNDCLASS());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_objDataPutPtr(self, "_STRMENUNAME", nullptr);
  hb_objDataPutPtr(self, "_STRCLASSNAME", nullptr);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_WNDCLASS_DELETE )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    if( !IS_INTRESOURCE(hb_objDataGetPtr(hb_stackSelfItem(), "STRMENUNAME")) )
    {
       hb_strfree(hb_objDataGetPtr(hb_stackSelfItem(), "STRMENUNAME"));
    }
    hb_strfree(hb_objDataGetPtr(hb_stackSelfItem(), "STRCLASSNAME"));
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
    hb_objDataPutPtr(hb_stackSelfItem(), "_STRMENUNAME", nullptr);
    hb_objDataPutPtr(hb_stackSelfItem(), "_STRCLASSNAME", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// UINT style

HB_FUNC_STATIC( WAS_WNDCLASS_SETSTYLE )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->style = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC( WAS_WNDCLASS_GETSTYLE )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->style);
  }
}

// WNDPROC lpfnWndProc

HB_FUNC_STATIC( WAS_WNDCLASS_SETLPFNWNDPROC )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->lpfnWndProc = wa_par_WNDPROC(1);
  }
}

HB_FUNC_STATIC( WAS_WNDCLASS_GETLPFNWNDPROC )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WNDPROC(obj->lpfnWndProc);
  }
}

// int cbClsExtra

HB_FUNC_STATIC( WAS_WNDCLASS_SETCBCLSEXTRA )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cbClsExtra = wa_par_int(1);
  }
}

HB_FUNC_STATIC( WAS_WNDCLASS_GETCBCLSEXTRA )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_int(obj->cbClsExtra);
  }
}

// int cbWndExtra

HB_FUNC_STATIC( WAS_WNDCLASS_SETCBWNDEXTRA )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cbWndExtra = wa_par_int(1);
  }
}

HB_FUNC_STATIC( WAS_WNDCLASS_GETCBWNDEXTRA )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_int(obj->cbWndExtra);
  }
}

// HINSTANCE hInstance

HB_FUNC_STATIC( WAS_WNDCLASS_SETHINSTANCE )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hInstance = wa_par_HINSTANCE(1);
  }
}

HB_FUNC_STATIC( WAS_WNDCLASS_GETHINSTANCE )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HINSTANCE(obj->hInstance);
  }
}

// HICON hIcon

HB_FUNC_STATIC( WAS_WNDCLASS_SETHICON )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hIcon = wa_par_HICON(1);
  }
}

HB_FUNC_STATIC( WAS_WNDCLASS_GETHICON )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HICON(obj->hIcon);
  }
}

// HCURSOR hCursor

HB_FUNC_STATIC( WAS_WNDCLASS_SETHCURSOR )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hCursor = wa_par_HCURSOR(1);
  }
}

HB_FUNC_STATIC( WAS_WNDCLASS_GETHCURSOR )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HCURSOR(obj->hCursor);
  }
}

// HBRUSH hbrBackground

HB_FUNC_STATIC( WAS_WNDCLASS_SETHBRBACKGROUND )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hbrBackground = wa_par_HBRUSH(1);
  }
}

HB_FUNC_STATIC( WAS_WNDCLASS_GETHBRBACKGROUND )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HBRUSH(obj->hbrBackground);
  }
}

// LPCWSTR lpszMenuName

HB_FUNC_STATIC( WAS_WNDCLASS_SETLPSZMENUNAME )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    void * str = hb_objDataGetPtr(hb_stackSelfItem(), "STRMENUNAME");
    if( str != nullptr && !IS_INTRESOURCE(str) )
    {
      hb_strfree(str);
    }
    str = nullptr;
    obj->lpszMenuName = HB_ISCHAR(1) ? HB_PARSTR(1, &str, nullptr) : MAKEINTRESOURCE(hb_parni(1));
    hb_objDataPutPtr(hb_stackSelfItem(), "_STRMENUNAME", str);
  }
}

HB_FUNC_STATIC( WAS_WNDCLASS_GETLPSZMENUNAME )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    if( !IS_INTRESOURCE(obj->lpszMenuName) )
    {
      HB_RETSTR(obj->lpszMenuName);
    }
    else
    {
      hb_retni(reinterpret_cast<int>(obj->lpszMenuName));
    }
  }
}

// LPCWSTR lpszClassName

HB_FUNC_STATIC( WAS_WNDCLASS_SETLPSZCLASSNAME )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    void * str = hb_objDataGetPtr(hb_stackSelfItem(), "STRCLASSNAME");
    if( str != nullptr )
    {
      hb_strfree(str);
    }
    obj->lpszClassName = HB_PARSTR(1, &str, nullptr);
    hb_objDataPutPtr(hb_stackSelfItem(), "_STRCLASSNAME", str);
  }
}

HB_FUNC_STATIC( WAS_WNDCLASS_GETLPSZCLASSNAME )
{
  auto obj = static_cast<WNDCLASS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    HB_RETSTR(obj->lpszClassName);
  }
}

/*
typedef struct tagWNDCLASSA {
  UINT      style;
  WNDPROC   lpfnWndProc;
  int       cbClsExtra;
  int       cbWndExtra;
  HINSTANCE hInstance;
  HICON     hIcon;
  HCURSOR   hCursor;
  HBRUSH    hbrBackground;
  LPCSTR    lpszMenuName;
  LPCSTR    lpszClassName;
} WNDCLASSA, *PWNDCLASSA, *NPWNDCLASSA, *LPWNDCLASSA;
*/

/*
typedef struct tagWNDCLASSW {
  UINT      style;
  WNDPROC   lpfnWndProc;
  int       cbClsExtra;
  int       cbWndExtra;
  HINSTANCE hInstance;
  HICON     hIcon;
  HCURSOR   hCursor;
  HBRUSH    hbrBackground;
  LPCWSTR   lpszMenuName;
  LPCWSTR   lpszClassName;
} WNDCLASSW, *PWNDCLASSW, *NPWNDCLASSW, *LPWNDCLASSW;
*/

#pragma ENDDUMP
