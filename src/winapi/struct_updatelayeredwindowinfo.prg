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

// WORK IN PROGRESS - SUBJECT TO CHANGES

#include "hbclass.ch"

FUNCTION wasUPDATELAYEREDWINDOWINFO()
RETURN was_UPDATELAYEREDWINDOWINFO():new()

CLASS WAS_UPDATELAYEREDWINDOWINFO

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD cbSize
   //ASSIGN cbSize(n) INLINE ::setcbSize(n)
   ACCESS cbSize INLINE ::getcbSize()
   //METHOD setcbSize
   METHOD getcbSize

   // HDC hdcDst
   ASSIGN hdcDst(n) INLINE ::sethdcDst(n)
   ACCESS hdcDst INLINE ::gethdcDst()
   METHOD sethdcDst
   METHOD gethdcDst

   // const POINT *pptDst
   ASSIGN pptDst(n) INLINE ::setpptDst(n)
   ACCESS pptDst INLINE ::getpptDst()
   METHOD setpptDst
   METHOD getpptDst

   // const SIZE *psize
   ASSIGN psize(n) INLINE ::setpsize(n)
   ACCESS psize INLINE ::getpsize()
   METHOD setpsize
   METHOD getpsize

   // HDC hdcSrc
   ASSIGN hdcSrc(n) INLINE ::sethdcSrc(n)
   ACCESS hdcSrc INLINE ::gethdcSrc()
   METHOD sethdcSrc
   METHOD gethdcSrc

   // const POINT *pptSrc
   ASSIGN pptSrc(n) INLINE ::setpptSrc(n)
   ACCESS pptSrc INLINE ::getpptSrc()
   METHOD setpptSrc
   METHOD getpptSrc

   // COLORREF crKey
   ASSIGN crKey(n) INLINE ::setcrKey(n)
   ACCESS crKey INLINE ::getcrKey()
   METHOD setcrKey
   METHOD getcrKey

   // const BLENDFUNCTION *pblend
   ASSIGN pblend(n) INLINE ::setpblend(n)
   ACCESS pblend INLINE ::getpblend()
   METHOD setpblend
   METHOD getpblend

   // DWORD dwFlags
   ASSIGN dwFlags(n) INLINE ::setdwFlags(n)
   ACCESS dwFlags INLINE ::getdwFlags()
   METHOD setdwFlags
   METHOD getdwFlags

   // const RECT *prcDirty
   ASSIGN prcDirty(n) INLINE ::setprcDirty(n)
   ACCESS prcDirty INLINE ::getprcDirty()
   METHOD setprcDirty
   METHOD getprcDirty

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_UPDATELAYEREDWINDOWINFO
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

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_NEW)
{
  auto obj = new UPDATELAYEREDWINDOWINFO();
  obj->cbSize = sizeof(UPDATELAYEREDWINDOWINFO);
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", obj);
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_DELETE)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD cbSize

// HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_SETCBSIZE )
// {
//   auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if( obj != nullptr )
//   {
//     obj->cbSize = wa_par_DWORD(1);
//   }
// }

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_GETCBSIZE)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->cbSize);
  }
}

// HDC hdcDst

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_SETHDCDST)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hdcDst = wa_par_HDC(1);
  }
}

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_GETHDCDST)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HDC(obj->hdcDst);
  }
}

// const POINT *pptDst

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_SETPPTDST)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->pptDst = wa_par_POINT(1);
  }
}

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_GETPPTDST)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retptr(const_cast<POINT*>(obj->pptDst));
  }
}

// const SIZE *psize

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_SETPSIZE)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->psize = wa_par_SIZE(1);
  }
}

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_GETPSIZE)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retptr(const_cast<SIZE*>(obj->psize));
  }
}

// HDC hdcSrc

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_SETHDCSRC)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hdcSrc = wa_par_HDC(1);
  }
}

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_GETHDCSRC)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HDC(obj->hdcSrc);
  }
}

// const POINT *pptSrc

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_SETPPTSRC)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->pptSrc = wa_par_POINT(1);
  }
}

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_GETPPTSRC)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retptr(const_cast<POINT*>(obj->pptSrc));
  }
}

// COLORREF crKey

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_SETCRKEY)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->crKey = wa_par_COLORREF(1);
  }
}

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_GETCRKEY)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_COLORREF(obj->crKey);
  }
}

// const BLENDFUNCTION *pblend

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_SETPBLEND)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->pblend = wa_par_BLENDFUNCTION(1);
  }
}

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_GETPBLEND)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retptr(const_cast<BLENDFUNCTION*>(obj->pblend));
  }
}

// DWORD dwFlags

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_SETDWFLAGS)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwFlags = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_GETDWFLAGS)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->dwFlags);
  }
}

// const RECT *prcDirty

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_SETPRCDIRTY)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->prcDirty = wa_par_RECT(1);
  }
}

HB_FUNC_STATIC(WAS_UPDATELAYEREDWINDOWINFO_GETPRCDIRTY)
{
  auto obj = static_cast<UPDATELAYEREDWINDOWINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retptr(const_cast<RECT*>(obj->prcDirty));
  }
}

/*
typedef struct tagUPDATELAYEREDWINDOWINFO {
  DWORD               cbSize;
  HDC                 hdcDst;
  const POINT         *pptDst;
  const SIZE          *psize;
  HDC                 hdcSrc;
  const POINT         *pptSrc;
  COLORREF            crKey;
  const BLENDFUNCTION *pblend;
  DWORD               dwFlags;
  const RECT          *prcDirty;
} UPDATELAYEREDWINDOWINFO, *PUPDATELAYEREDWINDOWINFO;
*/

#pragma ENDDUMP
