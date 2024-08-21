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

FUNCTION wasMENUINFO()
RETURN was_MENUINFO():new()

CLASS WAS_MENUINFO

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD cbSize
   //ASSIGN cbSize(n) INLINE ::setcbSize(n)
   ACCESS cbSize INLINE ::getcbSize()
   //METHOD setcbSize
   METHOD getcbSize

   // DWORD fMask
   ASSIGN fMask(n) INLINE ::setfMask(n)
   ACCESS fMask INLINE ::getfMask()
   METHOD setfMask
   METHOD getfMask

   // DWORD dwStyle
   ASSIGN dwStyle(n) INLINE ::setdwStyle(n)
   ACCESS dwStyle INLINE ::getdwStyle()
   METHOD setdwStyle
   METHOD getdwStyle

   // UINT cyMax
   ASSIGN cyMax(n) INLINE ::setcyMax(n)
   ACCESS cyMax INLINE ::getcyMax()
   METHOD setcyMax
   METHOD getcyMax

   // HBRUSH hbrBack
   ASSIGN hbrBack(n) INLINE ::sethbrBack(n)
   ACCESS hbrBack INLINE ::gethbrBack()
   METHOD sethbrBack
   METHOD gethbrBack

   // DWORD dwContextHelpID
   ASSIGN dwContextHelpID(n) INLINE ::setdwContextHelpID(n)
   ACCESS dwContextHelpID INLINE ::getdwContextHelpID()
   METHOD setdwContextHelpID
   METHOD getdwContextHelpID

   // ULONG_PTR dwMenuData
   ASSIGN dwMenuData(n) INLINE ::setdwMenuData(n)
   ACCESS dwMenuData INLINE ::getdwMenuData()
   METHOD setdwMenuData
   METHOD getdwMenuData

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_MENUINFO
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

HB_FUNC_STATIC( WAS_MENUINFO_NEW )
{
  auto obj = new MENUINFO();
  obj->cbSize = sizeof(MENUINFO);
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", obj);
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_MENUINFO_DELETE )
{
  auto obj = static_cast<MENUINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD cbSize

// HB_FUNC_STATIC( WAS_MENUINFO_SETCBSIZE )
// {
//   auto obj = static_cast<MENUINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if( obj != nullptr )
//   {
//     obj->cbSize = wa_par_DWORD(1);
//   }
// }

HB_FUNC_STATIC( WAS_MENUINFO_GETCBSIZE )
{
  auto obj = static_cast<MENUINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->cbSize);
  }
}

// DWORD fMask

HB_FUNC_STATIC( WAS_MENUINFO_SETFMASK )
{
  auto obj = static_cast<MENUINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fMask = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WAS_MENUINFO_GETFMASK )
{
  auto obj = static_cast<MENUINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->fMask);
  }
}

// DWORD dwStyle

HB_FUNC_STATIC( WAS_MENUINFO_SETDWSTYLE )
{
  auto obj = static_cast<MENUINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwStyle = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WAS_MENUINFO_GETDWSTYLE )
{
  auto obj = static_cast<MENUINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->dwStyle);
  }
}

// UINT cyMax

HB_FUNC_STATIC( WAS_MENUINFO_SETCYMAX )
{
  auto obj = static_cast<MENUINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cyMax = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC( WAS_MENUINFO_GETCYMAX )
{
  auto obj = static_cast<MENUINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->cyMax);
  }
}

// HBRUSH hbrBack

HB_FUNC_STATIC( WAS_MENUINFO_SETHBRBACK )
{
  auto obj = static_cast<MENUINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hbrBack = wa_par_HBRUSH(1);
  }
}

HB_FUNC_STATIC( WAS_MENUINFO_GETHBRBACK )
{
  auto obj = static_cast<MENUINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HBRUSH(obj->hbrBack);
  }
}

// DWORD dwContextHelpID

HB_FUNC_STATIC( WAS_MENUINFO_SETDWCONTEXTHELPID )
{
  auto obj = static_cast<MENUINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwContextHelpID = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WAS_MENUINFO_GETDWCONTEXTHELPID )
{
  auto obj = static_cast<MENUINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->dwContextHelpID);
  }
}

// ULONG_PTR dwMenuData

HB_FUNC_STATIC( WAS_MENUINFO_SETDWMENUDATA )
{
  auto obj = static_cast<MENUINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwMenuData = wa_par_ULONG_PTR(1);
  }
}

HB_FUNC_STATIC( WAS_MENUINFO_GETDWMENUDATA )
{
  auto obj = static_cast<MENUINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_ULONG_PTR(obj->dwMenuData);
  }
}

/*
typedef struct tagMENUINFO {
  DWORD     cbSize;
  DWORD     fMask;
  DWORD     dwStyle;
  UINT      cyMax;
  HBRUSH    hbrBack;
  DWORD     dwContextHelpID;
  ULONG_PTR dwMenuData;
} MENUINFO, *LPMENUINFO;
*/

#pragma ENDDUMP
