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

FUNCTION wasWINDOWINFO()
RETURN was_WINDOWINFO():new()

CLASS WAS_WINDOWINFO

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD cbSize
   // ASSIGN cbSize(n) INLINE ::setcbSize(n)
   ACCESS cbSize INLINE ::getcbSize()
   // METHOD setcbSize
   METHOD getcbSize

   // RECT rcWindow // TODO:
   // ASSIGN rcWindow(n) INLINE ::setrcWindow(n)
   // ACCESS rcWindow INLINE ::getrcWindow()
   // METHOD setrcWindow
   // METHOD getrcWindow

   // RECT rcClient // TODO:
   // ASSIGN rcClient(n) INLINE ::setrcClient(n)
   // ACCESS rcClient INLINE ::getrcClient()
   // METHOD setrcClient
   // METHOD getrcClient

   // DWORD dwStyle
   // ASSIGN dwStyle(n) INLINE ::setdwStyle(n)
   ACCESS dwStyle INLINE ::getdwStyle()
   // METHOD setdwStyle
   METHOD getdwStyle

   // DWORD dwExStyle
   // ASSIGN dwExStyle(n) INLINE ::setdwExStyle(n)
   ACCESS dwExStyle INLINE ::getdwExStyle()
   // METHOD setdwExStyle
   METHOD getdwExStyle

   // DWORD dwWindowStatus
   // ASSIGN dwWindowStatus(n) INLINE ::setdwWindowStatus(n)
   ACCESS dwWindowStatus INLINE ::getdwWindowStatus()
   // METHOD setdwWindowStatus
   METHOD getdwWindowStatus

   // UINT cxWindowBorders
   // ASSIGN cxWindowBorders(n) INLINE ::setcxWindowBorders(n)
   ACCESS cxWindowBorders INLINE ::getcxWindowBorders()
   // METHOD setcxWindowBorders
   METHOD getcxWindowBorders

   // UINT cyWindowBorders
   // ASSIGN cyWindowBorders(n) INLINE ::setcyWindowBorders(n)
   ACCESS cyWindowBorders INLINE ::getcyWindowBorders()
   // METHOD setcyWindowBorders
   METHOD getcyWindowBorders

   // ATOM atomWindowType
   // ASSIGN atomWindowType(n) INLINE ::setatomWindowType(n)
   ACCESS atomWindowType INLINE ::getatomWindowType()
   // METHOD setatomWindowType
   METHOD getatomWindowType

   // WORD wCreatorVersion
   // ASSIGN wCreatorVersion(n) INLINE ::setwCreatorVersion(n)
   ACCESS wCreatorVersion INLINE ::getwCreatorVersion()
   // METHOD setwCreatorVersion
   METHOD getwCreatorVersion

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_WINDOWINFO
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

HB_FUNC_STATIC( WAS_WINDOWINFO_NEW )
{
  auto obj = new WINDOWINFO();
  obj->cbSize = sizeof(WINDOWINFO);
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", obj);
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_WINDOWINFO_DELETE )
{
  auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD cbSize

// HB_FUNC_STATIC( WAS_WINDOWINFO_SETCBSIZE )
// {
//   auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if( obj != nullptr )
//   {
//     obj->cbSize = wa_par_DWORD(1);
//   }
// }

HB_FUNC_STATIC( WAS_WINDOWINFO_GETCBSIZE )
{
  auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->cbSize);
  }
}

// RECT rcWindow

// TODO:

// RECT rcClient

// TODO:

// DWORD dwStyle

// HB_FUNC_STATIC( WAS_WINDOWINFO_SETDWSTYLE )
// {
//   auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if( obj != nullptr )
//   {
//     obj->dwStyle = wa_par_DWORD(1);
//   }
// }

HB_FUNC_STATIC( WAS_WINDOWINFO_GETDWSTYLE )
{
  auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->dwStyle);
  }
}

// DWORD dwExStyle

// HB_FUNC_STATIC( WAS_WINDOWINFO_SETDWEXSTYLE )
// {
//   auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if( obj != nullptr )
//   {
//     obj->dwExStyle = wa_par_DWORD(1);
//   }
// }

HB_FUNC_STATIC( WAS_WINDOWINFO_GETDWEXSTYLE )
{
  auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->dwExStyle);
  }
}

// DWORD dwWindowStatus

// HB_FUNC_STATIC( WAS_WINDOWINFO_SETDWWINDOWSTATUS )
// {
//   auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if( obj != nullptr )
//   {
//     obj->dwWindowStatus = wa_par_DWORD(1);
//   }
// }

HB_FUNC_STATIC( WAS_WINDOWINFO_GETDWWINDOWSTATUS )
{
  auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->dwWindowStatus);
  }
}

// UINT cxWindowBorders

// HB_FUNC_STATIC( WAS_WINDOWINFO_SETCXWINDOWBORDERS )
// {
//   auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if( obj != nullptr )
//   {
//     obj->cxWindowBorders = wa_par_UINT(1);
//   }
// }

HB_FUNC_STATIC( WAS_WINDOWINFO_GETCXWINDOWBORDERS )
{
  auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->cxWindowBorders);
  }
}

// UINT cyWindowBorders

// HB_FUNC_STATIC( WAS_WINDOWINFO_SETCYWINDOWBORDERS )
// {
//   auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if( obj != nullptr )
//   {
//     obj->cyWindowBorders = wa_par_UINT(1);
//   }
// }

HB_FUNC_STATIC( WAS_WINDOWINFO_GETCYWINDOWBORDERS )
{
  auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->cyWindowBorders);
  }
}

// ATOM atomWindowType

// HB_FUNC_STATIC( WAS_WINDOWINFO_SETATOMWINDOWTYPE )
// {
//   auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if( obj != nullptr )
//   {
//     obj->atomWindowType = wa_par_ATOM(1);
//   }
// }

HB_FUNC_STATIC( WAS_WINDOWINFO_GETATOMWINDOWTYPE )
{
  auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_ATOM(obj->atomWindowType);
  }
}

// WORD wCreatorVersion

// HB_FUNC_STATIC( WAS_WINDOWINFO_SETWCREATORVERSION )
// {
//   auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if( obj != nullptr )
//   {
//     obj->wCreatorVersion = wa_par_WORD(1);
//   }
// }

HB_FUNC_STATIC( WAS_WINDOWINFO_GETWCREATORVERSION )
{
  auto obj = static_cast<WINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->wCreatorVersion);
  }
}

/*
typedef struct tagWINDOWINFO {
  DWORD cbSize;
  RECT  rcWindow;
  RECT  rcClient;
  DWORD dwStyle;
  DWORD dwExStyle;
  DWORD dwWindowStatus;
  UINT  cxWindowBorders;
  UINT  cyWindowBorders;
  ATOM  atomWindowType;
  WORD  wCreatorVersion;
} WINDOWINFO, *PWINDOWINFO, *LPWINDOWINFO;*/

#pragma ENDDUMP
