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

// NOTE: source code generated with the help of a code generator

#include "hbclass.ch"

FUNCTION wasSCROLLBARINFO()
RETURN was_SCROLLBARINFO():new()

CLASS WAS_SCROLLBARINFO

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD cbSize
   //ASSIGN cbSize(n) INLINE ::setcbSize(n)
   ACCESS cbSize INLINE ::getcbSize()
   //METHOD setcbSize
   METHOD getcbSize

   // RECT rcScrollBar

   // int dxyLineButton
   ASSIGN dxyLineButton(n) INLINE ::setdxyLineButton(n)
   ACCESS dxyLineButton INLINE ::getdxyLineButton()
   METHOD setdxyLineButton
   METHOD getdxyLineButton

   // int xyThumbTop
   ASSIGN xyThumbTop(n) INLINE ::setxyThumbTop(n)
   ACCESS xyThumbTop INLINE ::getxyThumbTop()
   METHOD setxyThumbTop
   METHOD getxyThumbTop

   // int xyThumbBottom
   ASSIGN xyThumbBottom(n) INLINE ::setxyThumbBottom(n)
   ACCESS xyThumbBottom INLINE ::getxyThumbBottom()
   METHOD setxyThumbBottom
   METHOD getxyThumbBottom

   // int reserved

   // DWORD rgstate[CCHILDREN_SCROLLBAR + 1]
   //ASSIGN rgstate(n) INLINE ::setrgstate(n)
   ACCESS rgstate INLINE ::getrgstate()
   //METHOD setrgstate
   METHOD getrgstate

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_SCROLLBARINFO
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

HB_FUNC_STATIC( WAS_SCROLLBARINFO_NEW )
{
  auto obj = new SCROLLBARINFO();
  obj->cbSize = sizeof(SCROLLBARINFO);
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", obj);
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_SCROLLBARINFO_DELETE )
{
  auto obj = static_cast<SCROLLBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD cbSize

// HB_FUNC_STATIC( WAS_SCROLLBARINFO_SETCBSIZE )
// {
//   auto obj = static_cast<SCROLLBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if( obj != nullptr )
//   {
//     obj->cbSize = wa_par_DWORD(1);
//   }
// }

HB_FUNC_STATIC( WAS_SCROLLBARINFO_GETCBSIZE )
{
  auto obj = static_cast<SCROLLBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->cbSize);
  }
}

// RECT rcScrollBar
// TODO:

// int dxyLineButton

HB_FUNC_STATIC( WAS_SCROLLBARINFO_SETDXYLINEBUTTON )
{
  auto obj = static_cast<SCROLLBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dxyLineButton = wa_par_int(1);
  }
}

HB_FUNC_STATIC( WAS_SCROLLBARINFO_GETDXYLINEBUTTON )
{
  auto obj = static_cast<SCROLLBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_int(obj->dxyLineButton);
  }
}

// int xyThumbTop

HB_FUNC_STATIC( WAS_SCROLLBARINFO_SETXYTHUMBTOP )
{
  auto obj = static_cast<SCROLLBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->xyThumbTop = wa_par_int(1);
  }
}

HB_FUNC_STATIC( WAS_SCROLLBARINFO_GETXYTHUMBTOP )
{
  auto obj = static_cast<SCROLLBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_int(obj->xyThumbTop);
  }
}

// int xyThumbBottom

HB_FUNC_STATIC( WAS_SCROLLBARINFO_SETXYTHUMBBOTTOM )
{
  auto obj = static_cast<SCROLLBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->xyThumbBottom = wa_par_int(1);
  }
}

HB_FUNC_STATIC( WAS_SCROLLBARINFO_GETXYTHUMBBOTTOM )
{
  auto obj = static_cast<SCROLLBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_int(obj->xyThumbBottom);
  }
}

// int reserved

// DWORD rgstate[CCHILDREN_SCROLLBAR + 1]

HB_FUNC_STATIC( WAS_SCROLLBARINFO_GETRGSTATE )
{
  auto obj = static_cast<SCROLLBARINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_reta(6);
    hb_storvnl(obj->rgstate[0], -1, 1);
    hb_storvnl(obj->rgstate[1], -1, 2);
    hb_storvnl(obj->rgstate[2], -1, 3);
    hb_storvnl(obj->rgstate[3], -1, 4);
    hb_storvnl(obj->rgstate[4], -1, 5);
    hb_storvnl(obj->rgstate[5], -1, 6);
  }
}

/*
typedef struct tagSCROLLBARINFO {
  DWORD cbSize;
  RECT  rcScrollBar;
  int   dxyLineButton;
  int   xyThumbTop;
  int   xyThumbBottom;
  int   reserved;
  DWORD rgstate[CCHILDREN_SCROLLBAR + 1];
} SCROLLBARINFO, *PSCROLLBARINFO, *LPSCROLLBARINFO;
*/

#pragma ENDDUMP
