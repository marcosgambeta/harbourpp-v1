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

FUNCTION wasMENUBARINFO()
RETURN was_MENUBARINFO():new()

CLASS WAS_MENUBARINFO

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD cbSize
   //ASSIGN cbSize(n) INLINE ::setcbSize(n)
   ACCESS cbSize INLINE ::getcbSize()
   //METHOD setcbSize
   METHOD getcbSize

   // RECT rcBar
   // TODO:

   // HMENU hMenu
   ASSIGN hMenu(n) INLINE ::sethMenu(n)
   ACCESS hMenu INLINE ::gethMenu()
   METHOD sethMenu
   METHOD gethMenu

   // HWND hwndMenu
   ASSIGN hwndMenu(n) INLINE ::sethwndMenu(n)
   ACCESS hwndMenu INLINE ::gethwndMenu()
   METHOD sethwndMenu
   METHOD gethwndMenu

   // BOOL fBarFocused : 1
   ASSIGN fBarFocused(n) INLINE ::setfBarFocused(n)
   ACCESS fBarFocused INLINE ::getfBarFocused()
   METHOD setfBarFocused
   METHOD getfBarFocused

   // BOOL fFocused : 1
   ASSIGN fFocused(n) INLINE ::setfFocused(n)
   ACCESS fFocused INLINE ::getfFocused()
   METHOD setfFocused
   METHOD getfFocused

   // BOOL fUnused : 30

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_MENUBARINFO
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

HB_FUNC_STATIC( WAS_MENUBARINFO_NEW )
{
  auto obj = new MENUBARINFO();
  obj->cbSize = sizeof(MENUBARINFO);
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", obj);
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_MENUBARINFO_DELETE )
{
  auto obj = static_cast<MENUBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD cbSize

// HB_FUNC_STATIC( WAS_MENUBARINFO_SETCBSIZE )
// {
//   auto obj = static_cast<MENUBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if( obj != nullptr )
//   {
//     obj->cbSize = wa_par_DWORD(1);
//   }
// }

HB_FUNC_STATIC( WAS_MENUBARINFO_GETCBSIZE )
{
  auto obj = static_cast<MENUBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->cbSize);
  }
}

// RECT rcBar
// TODO:

// HMENU hMenu

HB_FUNC_STATIC( WAS_MENUBARINFO_SETHMENU )
{
  auto obj = static_cast<MENUBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hMenu = wa_par_HMENU(1);
  }
}

HB_FUNC_STATIC( WAS_MENUBARINFO_GETHMENU )
{
  auto obj = static_cast<MENUBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HMENU(obj->hMenu);
  }
}

// HWND hwndMenu

HB_FUNC_STATIC( WAS_MENUBARINFO_SETHWNDMENU )
{
  auto obj = static_cast<MENUBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hwndMenu = wa_par_HWND(1);
  }
}

HB_FUNC_STATIC( WAS_MENUBARINFO_GETHWNDMENU )
{
  auto obj = static_cast<MENUBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HWND(obj->hwndMenu);
  }
}

// BOOL fBarFocused : 1

HB_FUNC_STATIC( WAS_MENUBARINFO_SETFBARFOCUSED )
{
  auto obj = static_cast<MENUBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fBarFocused = wa_par_BOOL(1);
  }
}

HB_FUNC_STATIC( WAS_MENUBARINFO_GETFBARFOCUSED )
{
  auto obj = static_cast<MENUBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_BOOL(obj->fBarFocused);
  }
}

// BOOL fFocused : 1

HB_FUNC_STATIC( WAS_MENUBARINFO_SETFFOCUSED )
{
  auto obj = static_cast<MENUBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fFocused = wa_par_BOOL(1);
  }
}

HB_FUNC_STATIC( WAS_MENUBARINFO_GETFFOCUSED )
{
  auto obj = static_cast<MENUBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_BOOL(obj->fFocused);
  }
}

// BOOL fUnused : 30

/*
typedef struct tagMENUBARINFO {
  DWORD cbSize;
  RECT  rcBar;
  HMENU hMenu;
  HWND  hwndMenu;
  BOOL  fBarFocused : 1;
  BOOL  fFocused : 1;
  BOOL  fUnused : 30;
} MENUBARINFO, *PMENUBARINFO, *LPMENUBARINFO;
*/

#pragma ENDDUMP
