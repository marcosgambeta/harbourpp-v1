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

CLASS WINAPI_STRUCT_RGBQUAD

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // BYTE rgbBlue
   ASSIGN rgbBlue(n) INLINE ::setrgbBlue(n)
   ACCESS rgbBlue INLINE ::getrgbBlue()
   METHOD setrgbBlue
   METHOD getrgbBlue

   // BYTE rgbGreen
   ASSIGN rgbGreen(n) INLINE ::setrgbGreen(n)
   ACCESS rgbGreen INLINE ::getrgbGreen()
   METHOD setrgbGreen
   METHOD getrgbGreen

   // BYTE rgbRed
   ASSIGN rgbRed(n) INLINE ::setrgbRed(n)
   ACCESS rgbRed INLINE ::getrgbRed()
   METHOD setrgbRed
   METHOD getrgbRed

   // BYTE rgbReserved
   ASSIGN rgbReserved(n) INLINE ::setrgbReserved(n)
   ACCESS rgbReserved INLINE ::getrgbReserved()
   METHOD setrgbReserved
   METHOD getrgbReserved

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_RGBQUAD
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

HB_FUNC_STATIC( WINAPI_STRUCT_RGBQUAD_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new RGBQUAD());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WINAPI_STRUCT_RGBQUAD_DELETE )
{
  auto obj = static_cast<RGBQUAD*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// BYTE rgbBlue

HB_FUNC_STATIC( WINAPI_STRUCT_RGBQUAD_SETRGBBLUE )
{
  auto obj = static_cast<RGBQUAD*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->rgbBlue = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_RGBQUAD_GETRGBBLUE )
{
  auto obj = static_cast<RGBQUAD*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->rgbBlue);
  }
}

// BYTE rgbGreen

HB_FUNC_STATIC( WINAPI_STRUCT_RGBQUAD_SETRGBGREEN )
{
  auto obj = static_cast<RGBQUAD*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->rgbGreen = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_RGBQUAD_GETRGBGREEN )
{
  auto obj = static_cast<RGBQUAD*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->rgbGreen);
  }
}

// BYTE rgbRed

HB_FUNC_STATIC( WINAPI_STRUCT_RGBQUAD_SETRGBRED )
{
  auto obj = static_cast<RGBQUAD*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->rgbRed = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_RGBQUAD_GETRGBRED )
{
  auto obj = static_cast<RGBQUAD*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->rgbRed);
  }
}

// BYTE rgbReserved

HB_FUNC_STATIC( WINAPI_STRUCT_RGBQUAD_SETRGBRESERVED )
{
  auto obj = static_cast<RGBQUAD*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->rgbReserved = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_RGBQUAD_GETRGBRESERVED )
{
  auto obj = static_cast<RGBQUAD*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->rgbReserved);
  }
}

/*
typedef struct tagRGBQUAD {
  BYTE rgbBlue;
  BYTE rgbGreen;
  BYTE rgbRed;
  BYTE rgbReserved;
} RGBQUAD;
*/

#pragma ENDDUMP
