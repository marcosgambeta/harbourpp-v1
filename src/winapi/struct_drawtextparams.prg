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

CLASS WASDRAWTEXTPARAMS

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // UINT cbSize
   //ASSIGN cbSize(n) INLINE ::setcbSize(n)
   ACCESS cbSize INLINE ::getcbSize()
   //METHOD setcbSize
   METHOD getcbSize

   // int iTabLength
   ASSIGN iTabLength(n) INLINE ::setiTabLength(n)
   ACCESS iTabLength INLINE ::getiTabLength()
   METHOD setiTabLength
   METHOD getiTabLength

   // int iLeftMargin
   ASSIGN iLeftMargin(n) INLINE ::setiLeftMargin(n)
   ACCESS iLeftMargin INLINE ::getiLeftMargin()
   METHOD setiLeftMargin
   METHOD getiLeftMargin

   // int iRightMargin
   ASSIGN iRightMargin(n) INLINE ::setiRightMargin(n)
   ACCESS iRightMargin INLINE ::getiRightMargin()
   METHOD setiRightMargin
   METHOD getiRightMargin

   // UINT uiLengthDrawn
   ASSIGN uiLengthDrawn(n) INLINE ::setuiLengthDrawn(n)
   ACCESS uiLengthDrawn INLINE ::getuiLengthDrawn()
   METHOD setuiLengthDrawn
   METHOD getuiLengthDrawn

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WASDRAWTEXTPARAMS
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

HB_FUNC_STATIC( WASDRAWTEXTPARAMS_NEW )
{
  auto obj = new DRAWTEXTPARAMS();
  obj->cbSize = sizeof(DRAWTEXTPARAMS);
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", obj);
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WASDRAWTEXTPARAMS_DELETE )
{
  auto obj = static_cast<DRAWTEXTPARAMS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// UINT cbSize

// HB_FUNC_STATIC( WASDRAWTEXTPARAMS_SETCBSIZE )
// {
//   auto obj = static_cast<DRAWTEXTPARAMS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));
//
//   if( obj != nullptr )
//   {
//     obj->cbSize = wa_par_UINT(1);
//   }
// }

HB_FUNC_STATIC( WASDRAWTEXTPARAMS_GETCBSIZE )
{
  auto obj = static_cast<DRAWTEXTPARAMS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->cbSize);
  }
}

// int iTabLength

HB_FUNC_STATIC( WASDRAWTEXTPARAMS_SETITABLENGTH )
{
  auto obj = static_cast<DRAWTEXTPARAMS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->iTabLength = wa_par_int(1);
  }
}

HB_FUNC_STATIC( WASDRAWTEXTPARAMS_GETITABLENGTH )
{
  auto obj = static_cast<DRAWTEXTPARAMS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_int(obj->iTabLength);
  }
}

// int iLeftMargin

HB_FUNC_STATIC( WASDRAWTEXTPARAMS_SETILEFTMARGIN )
{
  auto obj = static_cast<DRAWTEXTPARAMS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->iLeftMargin = wa_par_int(1);
  }
}

HB_FUNC_STATIC( WASDRAWTEXTPARAMS_GETILEFTMARGIN )
{
  auto obj = static_cast<DRAWTEXTPARAMS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_int(obj->iLeftMargin);
  }
}

// int iRightMargin

HB_FUNC_STATIC( WASDRAWTEXTPARAMS_SETIRIGHTMARGIN )
{
  auto obj = static_cast<DRAWTEXTPARAMS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->iRightMargin = wa_par_int(1);
  }
}

HB_FUNC_STATIC( WASDRAWTEXTPARAMS_GETIRIGHTMARGIN )
{
  auto obj = static_cast<DRAWTEXTPARAMS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_int(obj->iRightMargin);
  }
}

// UINT uiLengthDrawn

HB_FUNC_STATIC( WASDRAWTEXTPARAMS_SETUILENGTHDRAWN )
{
  auto obj = static_cast<DRAWTEXTPARAMS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->uiLengthDrawn = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC( WASDRAWTEXTPARAMS_GETUILENGTHDRAWN )
{
  auto obj = static_cast<DRAWTEXTPARAMS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->uiLengthDrawn);
  }
}

/*
typedef struct tagDRAWTEXTPARAMS {
  UINT cbSize;
  int  iTabLength;
  int  iLeftMargin;
  int  iRightMargin;
  UINT uiLengthDrawn;
} DRAWTEXTPARAMS, *LPDRAWTEXTPARAMS;
*/

#pragma ENDDUMP
