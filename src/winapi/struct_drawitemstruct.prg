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

FUNCTION wasDRAWITEMSTRUCT()
RETURN was_DRAWITEMSTRUCT():new()

CLASS WAS_DRAWITEMSTRUCT

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // UINT CtlType
   ASSIGN CtlType(n) INLINE ::setCtlType(n)
   ACCESS CtlType INLINE ::getCtlType()
   METHOD setCtlType
   METHOD getCtlType

   // UINT CtlID
   ASSIGN CtlID(n) INLINE ::setCtlID(n)
   ACCESS CtlID INLINE ::getCtlID()
   METHOD setCtlID
   METHOD getCtlID

   // UINT itemID
   ASSIGN itemID(n) INLINE ::setitemID(n)
   ACCESS itemID INLINE ::getitemID()
   METHOD setitemID
   METHOD getitemID

   // UINT itemAction
   ASSIGN itemAction(n) INLINE ::setitemAction(n)
   ACCESS itemAction INLINE ::getitemAction()
   METHOD setitemAction
   METHOD getitemAction

   // UINT itemState
   ASSIGN itemState(n) INLINE ::setitemState(n)
   ACCESS itemState INLINE ::getitemState()
   METHOD setitemState
   METHOD getitemState

   // HWND hwndItem
   ASSIGN hwndItem(n) INLINE ::sethwndItem(n)
   ACCESS hwndItem INLINE ::gethwndItem()
   METHOD sethwndItem
   METHOD gethwndItem

   // HDC hDC
   ASSIGN hDC(n) INLINE ::sethDC(n)
   ACCESS hDC INLINE ::gethDC()
   METHOD sethDC
   METHOD gethDC

   // RECT rcItem
   // TODO:

   // ULONG_PTR itemData
   ASSIGN itemData(n) INLINE ::setitemData(n)
   ACCESS itemData INLINE ::getitemData()
   METHOD setitemData
   METHOD getitemData

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_DRAWITEMSTRUCT
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

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new DRAWITEMSTRUCT());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_DELETE )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// UINT CtlType

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_SETCTLTYPE )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->CtlType = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_GETCTLTYPE )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->CtlType);
  }
}

// UINT CtlID

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_SETCTLID )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->CtlID = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_GETCTLID )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->CtlID);
  }
}

// UINT itemID

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_SETITEMID )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->itemID = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_GETITEMID )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->itemID);
  }
}

// UINT itemAction

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_SETITEMACTION )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->itemAction = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_GETITEMACTION )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->itemAction);
  }
}

// UINT itemState

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_SETITEMSTATE )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->itemState = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_GETITEMSTATE )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->itemState);
  }
}

// HWND hwndItem

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_SETHWNDITEM )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hwndItem = wa_par_HWND(1);
  }
}

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_GETHWNDITEM )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HWND(obj->hwndItem);
  }
}

// HDC hDC

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_SETHDC )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hDC = wa_par_HDC(1);
  }
}

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_GETHDC )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HDC(obj->hDC);
  }
}

// RECT rcItem
// TODO:

// ULONG_PTR itemData

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_SETITEMDATA )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->itemData = wa_par_ULONG_PTR(1);
  }
}

HB_FUNC_STATIC( WAS_DRAWITEMSTRUCT_GETITEMDATA )
{
  auto obj = static_cast<DRAWITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_ULONG_PTR(obj->itemData);
  }
}

/*
typedef struct tagDRAWITEMSTRUCT {
  UINT      CtlType;
  UINT      CtlID;
  UINT      itemID;
  UINT      itemAction;
  UINT      itemState;
  HWND      hwndItem;
  HDC       hDC;
  RECT      rcItem;
  ULONG_PTR itemData;
} DRAWITEMSTRUCT, *PDRAWITEMSTRUCT, *LPDRAWITEMSTRUCT;
*/

#pragma ENDDUMP
