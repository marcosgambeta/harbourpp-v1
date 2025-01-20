//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2025 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2025 Marcos Antonio Gambeta
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

FUNCTION wasBITMAPINFOHEADER()
RETURN was_BITMAPINFOHEADER():new()

CLASS WAS_BITMAPINFOHEADER

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // DWORD biSize
   ASSIGN biSize(n) INLINE ::setbiSize(n)
   ACCESS biSize INLINE ::getbiSize()
   METHOD setbiSize
   METHOD getbiSize

   // LONG biWidth
   ASSIGN biWidth(n) INLINE ::setbiWidth(n)
   ACCESS biWidth INLINE ::getbiWidth()
   METHOD setbiWidth
   METHOD getbiWidth

   // LONG biHeight
   ASSIGN biHeight(n) INLINE ::setbiHeight(n)
   ACCESS biHeight INLINE ::getbiHeight()
   METHOD setbiHeight
   METHOD getbiHeight

   // WORD biPlanes
   ASSIGN biPlanes(n) INLINE ::setbiPlanes(n)
   ACCESS biPlanes INLINE ::getbiPlanes()
   METHOD setbiPlanes
   METHOD getbiPlanes

   // WORD biBitCount
   ASSIGN biBitCount(n) INLINE ::setbiBitCount(n)
   ACCESS biBitCount INLINE ::getbiBitCount()
   METHOD setbiBitCount
   METHOD getbiBitCount

   // DWORD biCompression
   ASSIGN biCompression(n) INLINE ::setbiCompression(n)
   ACCESS biCompression INLINE ::getbiCompression()
   METHOD setbiCompression
   METHOD getbiCompression

   // DWORD biSizeImage
   ASSIGN biSizeImage(n) INLINE ::setbiSizeImage(n)
   ACCESS biSizeImage INLINE ::getbiSizeImage()
   METHOD setbiSizeImage
   METHOD getbiSizeImage

   // LONG biXPelsPerMeter
   ASSIGN biXPelsPerMeter(n) INLINE ::setbiXPelsPerMeter(n)
   ACCESS biXPelsPerMeter INLINE ::getbiXPelsPerMeter()
   METHOD setbiXPelsPerMeter
   METHOD getbiXPelsPerMeter

   // LONG biYPelsPerMeter
   ASSIGN biYPelsPerMeter(n) INLINE ::setbiYPelsPerMeter(n)
   ACCESS biYPelsPerMeter INLINE ::getbiYPelsPerMeter()
   METHOD setbiYPelsPerMeter
   METHOD getbiYPelsPerMeter

   // DWORD biClrUsed
   ASSIGN biClrUsed(n) INLINE ::setbiClrUsed(n)
   ACCESS biClrUsed INLINE ::getbiClrUsed()
   METHOD setbiClrUsed
   METHOD getbiClrUsed

   // DWORD biClrImportant
   ASSIGN biClrImportant(n) INLINE ::setbiClrImportant(n)
   ACCESS biClrImportant INLINE ::getbiClrImportant()
   METHOD setbiClrImportant
   METHOD getbiClrImportant

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_BITMAPINFOHEADER
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

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_NEW)
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new BITMAPINFOHEADER());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_DELETE)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// DWORD biSize

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_SETBISIZE)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->biSize = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_GETBISIZE)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_DWORD(obj->biSize);
  }
}

// LONG biWidth

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_SETBIWIDTH)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->biWidth = wa_par_LONG(1);
  }
}

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_GETBIWIDTH)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_LONG(obj->biWidth);
  }
}

// LONG biHeight

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_SETBIHEIGHT)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->biHeight = wa_par_LONG(1);
  }
}

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_GETBIHEIGHT)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_LONG(obj->biHeight);
  }
}

// WORD biPlanes

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_SETBIPLANES)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->biPlanes = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_GETBIPLANES)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_WORD(obj->biPlanes);
  }
}

// WORD biBitCount

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_SETBIBITCOUNT)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->biBitCount = wa_par_WORD(1);
  }
}

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_GETBIBITCOUNT)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_WORD(obj->biBitCount);
  }
}

// DWORD biCompression

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_SETBICOMPRESSION)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->biCompression = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_GETBICOMPRESSION)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_DWORD(obj->biCompression);
  }
}

// DWORD biSizeImage

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_SETBISIZEIMAGE)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->biSizeImage = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_GETBISIZEIMAGE)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_DWORD(obj->biSizeImage);
  }
}

// LONG biXPelsPerMeter

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_SETBIXPELSPERMETER)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->biXPelsPerMeter = wa_par_LONG(1);
  }
}

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_GETBIXPELSPERMETER)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_LONG(obj->biXPelsPerMeter);
  }
}

// LONG biYPelsPerMeter

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_SETBIYPELSPERMETER)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->biYPelsPerMeter = wa_par_LONG(1);
  }
}

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_GETBIYPELSPERMETER)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_LONG(obj->biYPelsPerMeter);
  }
}

// DWORD biClrUsed

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_SETBICLRUSED)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->biClrUsed = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_GETBICLRUSED)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_DWORD(obj->biClrUsed);
  }
}

// DWORD biClrImportant

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_SETBICLRIMPORTANT)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->biClrImportant = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC(WAS_BITMAPINFOHEADER_GETBICLRIMPORTANT)
{
  auto obj = static_cast<BITMAPINFOHEADER *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_DWORD(obj->biClrImportant);
  }
}

/*
typedef struct tagBITMAPINFOHEADER {
  DWORD biSize;
  LONG  biWidth;
  LONG  biHeight;
  WORD  biPlanes;
  WORD  biBitCount;
  DWORD biCompression;
  DWORD biSizeImage;
  LONG  biXPelsPerMeter;
  LONG  biYPelsPerMeter;
  DWORD biClrUsed;
  DWORD biClrImportant;
} BITMAPINFOHEADER, *PBITMAPINFOHEADER;
*/

#pragma ENDDUMP
