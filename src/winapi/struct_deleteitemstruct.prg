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

FUNCTION wasDELETEITEMSTRUCT()
RETURN was_DELETEITEMSTRUCT():new()

CLASS WAS_DELETEITEMSTRUCT

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

   // HWND hwndItem
   ASSIGN hwndItem(n) INLINE ::sethwndItem(n)
   ACCESS hwndItem INLINE ::gethwndItem()
   METHOD sethwndItem
   METHOD gethwndItem

   // ULONG_PTR itemData
   ASSIGN itemData(n) INLINE ::setitemData(n)
   ACCESS itemData INLINE ::getitemData()
   METHOD setitemData
   METHOD getitemData

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_DELETEITEMSTRUCT
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

HB_FUNC_STATIC( WAS_DELETEITEMSTRUCT_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new DELETEITEMSTRUCT());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_DELETEITEMSTRUCT_DELETE )
{
  auto obj = static_cast<DELETEITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// UINT CtlType

HB_FUNC_STATIC( WAS_DELETEITEMSTRUCT_SETCTLTYPE )
{
  auto obj = static_cast<DELETEITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->CtlType = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC( WAS_DELETEITEMSTRUCT_GETCTLTYPE )
{
  auto obj = static_cast<DELETEITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->CtlType);
  }
}

// UINT CtlID

HB_FUNC_STATIC( WAS_DELETEITEMSTRUCT_SETCTLID )
{
  auto obj = static_cast<DELETEITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->CtlID = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC( WAS_DELETEITEMSTRUCT_GETCTLID )
{
  auto obj = static_cast<DELETEITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->CtlID);
  }
}

// UINT itemID

HB_FUNC_STATIC( WAS_DELETEITEMSTRUCT_SETITEMID )
{
  auto obj = static_cast<DELETEITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->itemID = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC( WAS_DELETEITEMSTRUCT_GETITEMID )
{
  auto obj = static_cast<DELETEITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->itemID);
  }
}

// HWND hwndItem

HB_FUNC_STATIC( WAS_DELETEITEMSTRUCT_SETHWNDITEM )
{
  auto obj = static_cast<DELETEITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hwndItem = wa_par_HWND(1);
  }
}

HB_FUNC_STATIC( WAS_DELETEITEMSTRUCT_GETHWNDITEM )
{
  auto obj = static_cast<DELETEITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HWND(obj->hwndItem);
  }
}

// ULONG_PTR itemData

HB_FUNC_STATIC( WAS_DELETEITEMSTRUCT_SETITEMDATA )
{
  auto obj = static_cast<DELETEITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->itemData = wa_par_ULONG_PTR(1);
  }
}

HB_FUNC_STATIC( WAS_DELETEITEMSTRUCT_GETITEMDATA )
{
  auto obj = static_cast<DELETEITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_ULONG_PTR(obj->itemData);
  }
}

/*
typedef struct tagDELETEITEMSTRUCT {
  UINT      CtlType;
  UINT      CtlID;
  UINT      itemID;
  HWND      hwndItem;
  ULONG_PTR itemData;
} DELETEITEMSTRUCT, *PDELETEITEMSTRUCT, *LPDELETEITEMSTRUCT;
*/

#pragma ENDDUMP
