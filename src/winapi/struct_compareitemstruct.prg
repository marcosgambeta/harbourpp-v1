//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2024 Marcos Antonio Gambeta
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

FUNCTION wasCOMPAREITEMSTRUCT()
RETURN was_COMPAREITEMSTRUCT():new()

CLASS WAS_COMPAREITEMSTRUCT

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

   // HWND hwndItem
   ASSIGN hwndItem(n) INLINE ::sethwndItem(n)
   ACCESS hwndItem INLINE ::gethwndItem()
   METHOD sethwndItem
   METHOD gethwndItem

   // UINT itemID1
   ASSIGN itemID1(n) INLINE ::setitemID1(n)
   ACCESS itemID1 INLINE ::getitemID1()
   METHOD setitemID1
   METHOD getitemID1

   // ULONG_PTR itemData1
   ASSIGN itemData1(n) INLINE ::setitemData1(n)
   ACCESS itemData1 INLINE ::getitemData1()
   METHOD setitemData1
   METHOD getitemData1

   // UINT itemID2
   ASSIGN itemID2(n) INLINE ::setitemID2(n)
   ACCESS itemID2 INLINE ::getitemID2()
   METHOD setitemID2
   METHOD getitemID2

   // ULONG_PTR itemData2
   ASSIGN itemData2(n) INLINE ::setitemData2(n)
   ACCESS itemData2 INLINE ::getitemData2()
   METHOD setitemData2
   METHOD getitemData2

   // DWORD dwLocaleId
   ASSIGN dwLocaleId(n) INLINE ::setdwLocaleId(n)
   ACCESS dwLocaleId INLINE ::getdwLocaleId()
   METHOD setdwLocaleId
   METHOD getdwLocaleId

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_COMPAREITEMSTRUCT
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

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_NEW)
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new COMPAREITEMSTRUCT());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_DELETE)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// UINT CtlType

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_SETCTLTYPE)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->CtlType = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_GETCTLTYPE)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->CtlType);
  }
}

// UINT CtlID

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_SETCTLID)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->CtlID = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_GETCTLID)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->CtlID);
  }
}

// HWND hwndItem

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_SETHWNDITEM)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hwndItem = wa_par_HWND(1);
  }
}

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_GETHWNDITEM)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HWND(obj->hwndItem);
  }
}

// UINT itemID1

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_SETITEMID1)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->itemID1 = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_GETITEMID1)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->itemID1);
  }
}

// ULONG_PTR itemData1

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_SETITEMDATA1)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->itemData1 = wa_par_ULONG_PTR(1);
  }
}

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_GETITEMDATA1)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_ULONG_PTR(obj->itemData1);
  }
}

// UINT itemID2

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_SETITEMID2)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->itemID2 = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_GETITEMID2)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->itemID2);
  }
}

// ULONG_PTR itemData2

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_SETITEMDATA2)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->itemData2 = wa_par_ULONG_PTR(1);
  }
}

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_GETITEMDATA2)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_ULONG_PTR(obj->itemData2);
  }
}

// DWORD dwLocaleId

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_SETDWLOCALEID)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwLocaleId = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_COMPAREITEMSTRUCT_GETDWLOCALEID)
{
  auto obj = static_cast<COMPAREITEMSTRUCT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->dwLocaleId);
  }
}

/*
typedef struct tagCOMPAREITEMSTRUCT {
  UINT      CtlType;
  UINT      CtlID;
  HWND      hwndItem;
  UINT      itemID1;
  ULONG_PTR itemData1;
  UINT      itemID2;
  ULONG_PTR itemData2;
  DWORD     dwLocaleId;
} COMPAREITEMSTRUCT, *PCOMPAREITEMSTRUCT, *LPCOMPAREITEMSTRUCT;
*/

#pragma ENDDUMP
