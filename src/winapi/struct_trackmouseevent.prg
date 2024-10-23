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

FUNCTION wasTRACKMOUSEEVENT()
RETURN was_TRACKMOUSEEVENT():new()

CLASS WAS_TRACKMOUSEEVENT

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD cbSize
   //ASSIGN cbSize(n) INLINE ::setcbSize(n)
   ACCESS cbSize INLINE ::getcbSize()
   //METHOD setcbSize
   METHOD getcbSize

   // DWORD dwFlags
   ASSIGN dwFlags(n) INLINE ::setdwFlags(n)
   ACCESS dwFlags INLINE ::getdwFlags()
   METHOD setdwFlags
   METHOD getdwFlags

   // HWND hwndTrack
   ASSIGN hwndTrack(n) INLINE ::sethwndTrack(n)
   ACCESS hwndTrack INLINE ::gethwndTrack()
   METHOD sethwndTrack
   METHOD gethwndTrack

   // DWORD dwHoverTime
   ASSIGN dwHoverTime(n) INLINE ::setdwHoverTime(n)
   ACCESS dwHoverTime INLINE ::getdwHoverTime()
   METHOD setdwHoverTime
   METHOD getdwHoverTime

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_TRACKMOUSEEVENT
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

HB_FUNC_STATIC(WAS_TRACKMOUSEEVENT_NEW)
{
  auto obj = new TRACKMOUSEEVENT();
  obj->cbSize = sizeof(TRACKMOUSEEVENT);
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", obj);
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_TRACKMOUSEEVENT_DELETE)
{
  auto obj = static_cast<TRACKMOUSEEVENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD cbSize

// HB_FUNC_STATIC(WAS_TRACKMOUSEEVENT_SETCBSIZE )
// {
//   auto obj = static_cast<TRACKMOUSEEVENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
// 
//   if( obj != nullptr )
//   {
//     obj->cbSize = wa_par_DWORD(1);
//   }
// }

HB_FUNC_STATIC(WAS_TRACKMOUSEEVENT_GETCBSIZE)
{
  auto obj = static_cast<TRACKMOUSEEVENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->cbSize);
  }
}

// DWORD dwFlags

HB_FUNC_STATIC(WAS_TRACKMOUSEEVENT_SETDWFLAGS)
{
  auto obj = static_cast<TRACKMOUSEEVENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwFlags = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_TRACKMOUSEEVENT_GETDWFLAGS)
{
  auto obj = static_cast<TRACKMOUSEEVENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->dwFlags);
  }
}

// HWND hwndTrack

HB_FUNC_STATIC(WAS_TRACKMOUSEEVENT_SETHWNDTRACK)
{
  auto obj = static_cast<TRACKMOUSEEVENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hwndTrack = wa_par_HWND(1);
  }
}

HB_FUNC_STATIC(WAS_TRACKMOUSEEVENT_GETHWNDTRACK)
{
  auto obj = static_cast<TRACKMOUSEEVENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HWND(obj->hwndTrack);
  }
}

// DWORD dwHoverTime

HB_FUNC_STATIC(WAS_TRACKMOUSEEVENT_SETDWHOVERTIME)
{
  auto obj = static_cast<TRACKMOUSEEVENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwHoverTime = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_TRACKMOUSEEVENT_GETDWHOVERTIME)
{
  auto obj = static_cast<TRACKMOUSEEVENT*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->dwHoverTime);
  }
}

/*
typedef struct tagTRACKMOUSEEVENT {
  DWORD cbSize;
  DWORD dwFlags;
  HWND  hwndTrack;
  DWORD dwHoverTime;
} TRACKMOUSEEVENT, *LPTRACKMOUSEEVENT;
*/

#pragma ENDDUMP
