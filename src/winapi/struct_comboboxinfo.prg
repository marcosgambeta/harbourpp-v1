//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2025 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2025 Marcos Antonio Gambeta
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// NOTE: source code generated with the help of a code generator

#include "hbclass.ch"

FUNCTION wasCOMBOBOXINFO()
RETURN was_COMBOBOXINFO():new()

CLASS WAS_COMBOBOXINFO

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD cbSize
   //ASSIGN cbSize(n) INLINE ::setcbSize(n)
   ACCESS cbSize INLINE ::getcbSize()
   //METHOD setcbSize
   METHOD getcbSize

   // RECT rcItem
   // TODO:

   // RECT rcButton
   // TODO:

   // DWORD stateButton
   ASSIGN stateButton(n) INLINE ::setstateButton(n)
   ACCESS stateButton INLINE ::getstateButton()
   METHOD setstateButton
   METHOD getstateButton

   // HWND hwndCombo
   ASSIGN hwndCombo(n) INLINE ::sethwndCombo(n)
   ACCESS hwndCombo INLINE ::gethwndCombo()
   METHOD sethwndCombo
   METHOD gethwndCombo

   // HWND hwndItem
   ASSIGN hwndItem(n) INLINE ::sethwndItem(n)
   ACCESS hwndItem INLINE ::gethwndItem()
   METHOD sethwndItem
   METHOD gethwndItem

   // HWND hwndList
   ASSIGN hwndList(n) INLINE ::sethwndList(n)
   ACCESS hwndList INLINE ::gethwndList()
   METHOD sethwndList
   METHOD gethwndList

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_COMBOBOXINFO
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

HB_FUNC_STATIC(WAS_COMBOBOXINFO_NEW)
{
  auto obj = new COMBOBOXINFO();
  obj->cbSize = sizeof(COMBOBOXINFO);
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", obj);
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_COMBOBOXINFO_DELETE)
{
  auto obj = static_cast<COMBOBOXINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD cbSize

// HB_FUNC_STATIC(WAS_COMBOBOXINFO_SETCBSIZE)
// {
//   auto obj = static_cast<COMBOBOXINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if (obj != nullptr)
//   {
//     obj->cbSize = wa_par_DWORD(1);
//   }
// }

HB_FUNC_STATIC(WAS_COMBOBOXINFO_GETCBSIZE)
{
  auto obj = static_cast<COMBOBOXINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_DWORD(obj->cbSize);
  }
}

// RECT rcItem
// TODO:

// RECT rcButton
// TODO:

// DWORD stateButton

HB_FUNC_STATIC(WAS_COMBOBOXINFO_SETSTATEBUTTON)
{
  auto obj = static_cast<COMBOBOXINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->stateButton = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_COMBOBOXINFO_GETSTATEBUTTON)
{
  auto obj = static_cast<COMBOBOXINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_DWORD(obj->stateButton);
  }
}

// HWND hwndCombo

HB_FUNC_STATIC(WAS_COMBOBOXINFO_SETHWNDCOMBO)
{
  auto obj = static_cast<COMBOBOXINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->hwndCombo = wa_par_HWND(1);
  }
}

HB_FUNC_STATIC(WAS_COMBOBOXINFO_GETHWNDCOMBO)
{
  auto obj = static_cast<COMBOBOXINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_HWND(obj->hwndCombo);
  }
}

// HWND hwndItem

HB_FUNC_STATIC(WAS_COMBOBOXINFO_SETHWNDITEM)
{
  auto obj = static_cast<COMBOBOXINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->hwndItem = wa_par_HWND(1);
  }
}

HB_FUNC_STATIC(WAS_COMBOBOXINFO_GETHWNDITEM)
{
  auto obj = static_cast<COMBOBOXINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_HWND(obj->hwndItem);
  }
}

// HWND hwndList

HB_FUNC_STATIC(WAS_COMBOBOXINFO_SETHWNDLIST)
{
  auto obj = static_cast<COMBOBOXINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->hwndList = wa_par_HWND(1);
  }
}

HB_FUNC_STATIC(WAS_COMBOBOXINFO_GETHWNDLIST)
{
  auto obj = static_cast<COMBOBOXINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_HWND(obj->hwndList);
  }
}

/*
typedef struct tagCOMBOBOXINFO {
  DWORD cbSize;
  RECT  rcItem;
  RECT  rcButton;
  DWORD stateButton;
  HWND  hwndCombo;
  HWND  hwndItem;
  HWND  hwndList;
} COMBOBOXINFO, *PCOMBOBOXINFO, *LPCOMBOBOXINFO;
*/

#pragma ENDDUMP
