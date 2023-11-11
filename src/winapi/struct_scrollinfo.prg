/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2023 Marcos Antonio Gambeta

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

CLASS WINAPI_STRUCT_SCROLLINFO

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // UINT cbSize
   ASSIGN cbSize(n) INLINE ::setcbSize(n)
   ACCESS cbSize INLINE ::getcbSize()
   METHOD setcbSize
   METHOD getcbSize

   // UINT fMask
   ASSIGN fMask(n) INLINE ::setfMask(n)
   ACCESS fMask INLINE ::getfMask()
   METHOD setfMask
   METHOD getfMask

   // int nMin
   ASSIGN nMin(n) INLINE ::setnMin(n)
   ACCESS nMin INLINE ::getnMin()
   METHOD setnMin
   METHOD getnMin

   // int nMax
   ASSIGN nMax(n) INLINE ::setnMax(n)
   ACCESS nMax INLINE ::getnMax()
   METHOD setnMax
   METHOD getnMax

   // UINT nPage
   ASSIGN nPage(n) INLINE ::setnPage(n)
   ACCESS nPage INLINE ::getnPage()
   METHOD setnPage
   METHOD getnPage

   // int nPos
   ASSIGN nPos(n) INLINE ::setnPos(n)
   ACCESS nPos INLINE ::getnPos()
   METHOD setnPos
   METHOD getnPos

   // int nTrackPos
   ASSIGN nTrackPos(n) INLINE ::setnTrackPos(n)
   ACCESS nTrackPos INLINE ::getnTrackPos()
   METHOD setnTrackPos
   METHOD getnTrackPos

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_SCROLLINFO
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

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new SCROLLINFO());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_DELETE )
{
  auto obj = static_cast<SCROLLINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// UINT cbSize

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_SETCBSIZE )
{
  auto obj = static_cast<SCROLLINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cbSize = winapi_par_UINT(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_GETCBSIZE )
{
  auto obj = static_cast<SCROLLINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_UINT(obj->cbSize);
  }
}

// UINT fMask

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_SETFMASK )
{
  auto obj = static_cast<SCROLLINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fMask = winapi_par_UINT(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_GETFMASK )
{
  auto obj = static_cast<SCROLLINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_UINT(obj->fMask);
  }
}

// int nMin

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_SETNMIN )
{
  auto obj = static_cast<SCROLLINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->nMin = winapi_par_int(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_GETNMIN )
{
  auto obj = static_cast<SCROLLINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_int(obj->nMin);
  }
}

// int nMax

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_SETNMAX )
{
  auto obj = static_cast<SCROLLINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->nMax = winapi_par_int(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_GETNMAX )
{
  auto obj = static_cast<SCROLLINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_int(obj->nMax);
  }
}

// UINT nPage

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_SETNPAGE )
{
  auto obj = static_cast<SCROLLINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->nPage = winapi_par_UINT(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_GETNPAGE )
{
  auto obj = static_cast<SCROLLINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_UINT(obj->nPage);
  }
}

// int nPos

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_SETNPOS )
{
  auto obj = static_cast<SCROLLINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->nPos = winapi_par_int(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_GETNPOS )
{
  auto obj = static_cast<SCROLLINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_int(obj->nPos);
  }
}

// int nTrackPos

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_SETNTRACKPOS )
{
  auto obj = static_cast<SCROLLINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->nTrackPos = winapi_par_int(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_SCROLLINFO_GETNTRACKPOS )
{
  auto obj = static_cast<SCROLLINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_int(obj->nTrackPos);
  }
}

/*
typedef struct tagSCROLLINFO {
  UINT cbSize;
  UINT fMask;
  int  nMin;
  int  nMax;
  UINT nPage;
  int  nPos;
  int  nTrackPos;
} SCROLLINFO, *LPSCROLLINFO;
*/

#pragma ENDDUMP
