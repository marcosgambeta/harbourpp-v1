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

CLASS WASBITMAP

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // LONG bmType

   ASSIGN bmType(n) INLINE ::setbmType(n)
   ACCESS bmType INLINE ::getbmType()
   METHOD setbmType
   METHOD getbmType

   // LONG bmWidth

   ASSIGN bmWidth(n) INLINE ::setbmWidth(n)
   ACCESS bmWidth INLINE ::getbmWidth()
   METHOD setbmWidth
   METHOD getbmWidth

   // LONG bmHeight

   ASSIGN bmHeight(n) INLINE ::setbmHeight(n)
   ACCESS bmHeight INLINE ::getbmHeight()
   METHOD setbmHeight
   METHOD getbmHeight

   // LONG bmWidthBytes

   ASSIGN bmWidthBytes(n) INLINE ::setbmWidthBytes(n)
   ACCESS bmWidthBytes INLINE ::getbmWidthBytes()
   METHOD setbmWidthBytes
   METHOD getbmWidthBytes

   // WORD bmPlanes

   ASSIGN bmPlanes(n) INLINE ::setbmPlanes(n)
   ACCESS bmPlanes INLINE ::getbmPlanes()
   METHOD setbmPlanes
   METHOD getbmPlanes

   // WORD bmBitsPixel

   ASSIGN bmBitsPixel(n) INLINE ::setbmBitsPixel(n)
   ACCESS bmBitsPixel INLINE ::getbmBitsPixel()
   METHOD setbmBitsPixel
   METHOD getbmBitsPixel

   // LPVOID bmBits // TODO:

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WASBITMAP
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

HB_FUNC_STATIC( WASBITMAP_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new BITMAP());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WASBITMAP_DELETE )
{
  auto obj = static_cast<BITMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// LONG bmType

HB_FUNC_STATIC( WASBITMAP_SETBMTYPE )
{
  auto obj = static_cast<BITMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->bmType = wa_par_LONG(1);
  }
}

HB_FUNC_STATIC( WASBITMAP_GETBMTYPE )
{
  auto obj = static_cast<BITMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_LONG(obj->bmType);
  }
}

// LONG bmWidth

HB_FUNC_STATIC( WASBITMAP_SETBMWIDTH )
{
  auto obj = static_cast<BITMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->bmWidth = wa_par_LONG(1);
  }
}

HB_FUNC_STATIC( WASBITMAP_GETBMWIDTH )
{
  auto obj = static_cast<BITMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_LONG(obj->bmWidth);
  }
}

// LONG bmHeight

HB_FUNC_STATIC( WASBITMAP_SETBMHEIGHT )
{
  auto obj = static_cast<BITMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->bmHeight = wa_par_LONG(1);
  }
}

HB_FUNC_STATIC( WASBITMAP_GETBMHEIGHT )
{
  auto obj = static_cast<BITMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_LONG(obj->bmHeight);
  }
}

// LONG bmWidthBytes

HB_FUNC_STATIC( WASBITMAP_SETBMWIDTHBYTES )
{
  auto obj = static_cast<BITMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->bmWidthBytes = wa_par_LONG(1);
  }
}

HB_FUNC_STATIC( WASBITMAP_GETBMWIDTHBYTES )
{
  auto obj = static_cast<BITMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_LONG(obj->bmWidthBytes);
  }
}

// WORD bmPlanes

HB_FUNC_STATIC( WASBITMAP_SETBMPLANES )
{
  auto obj = static_cast<BITMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->bmPlanes = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC( WASBITMAP_GETBMPLANES )
{
  auto obj = static_cast<BITMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->bmPlanes);
  }
}

// WORD bmBitsPixel

HB_FUNC_STATIC( WASBITMAP_SETBMBITSPIXEL )
{
  auto obj = static_cast<BITMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->bmBitsPixel = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC( WASBITMAP_GETBMBITSPIXEL )
{
  auto obj = static_cast<BITMAP*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_WORD(obj->bmBitsPixel);
  }
}

// LPVOID bmBits // TODO:

/*
typedef struct tagBITMAP {
  LONG   bmType;
  LONG   bmWidth;
  LONG   bmHeight;
  LONG   bmWidthBytes;
  WORD   bmPlanes;
  WORD   bmBitsPixel;
  LPVOID bmBits;
} BITMAP, *PBITMAP, *NPBITMAP, *LPBITMAP;
*/

#pragma ENDDUMP
