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

FUNCTION wasIMAGEINFO()
RETURN was_IMAGEINFO():new()

CLASS WAS_IMAGEINFO

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // HBITMAP hbmImage
   ASSIGN hbmImage(n) INLINE ::sethbmImage(n)
   ACCESS hbmImage INLINE ::gethbmImage()
   METHOD sethbmImage
   METHOD gethbmImage

   // HBITMAP hbmMask
   ASSIGN hbmMask(n) INLINE ::sethbmMask(n)
   ACCESS hbmMask INLINE ::gethbmMask()
   METHOD sethbmMask
   METHOD gethbmMask

   // int Unused1
   ASSIGN Unused1(n) INLINE ::setUnused1(n)
   ACCESS Unused1 INLINE ::getUnused1()
   METHOD setUnused1
   METHOD getUnused1

   // int Unused2
   ASSIGN Unused2(n) INLINE ::setUnused2(n)
   ACCESS Unused2 INLINE ::getUnused2()
   METHOD setUnused2
   METHOD getUnused2

   // RECT rcImage // TODO:

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_IMAGEINFO
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include <commctrl.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "winapi.hpp"

HB_FUNC_STATIC(WAS_IMAGEINFO_NEW)
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new IMAGEINFO());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC(WAS_IMAGEINFO_DELETE)
{
  auto obj = static_cast<IMAGEINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// HBITMAP hbmImage

HB_FUNC_STATIC(WAS_IMAGEINFO_SETHBMIMAGE)
{
  auto obj = static_cast<IMAGEINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->hbmImage = wa_par_HBITMAP(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGEINFO_GETHBMIMAGE)
{
  auto obj = static_cast<IMAGEINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_HBITMAP(obj->hbmImage);
  }
}

// HBITMAP hbmMask

HB_FUNC_STATIC(WAS_IMAGEINFO_SETHBMMASK)
{
  auto obj = static_cast<IMAGEINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->hbmMask = wa_par_HBITMAP(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGEINFO_GETHBMMASK)
{
  auto obj = static_cast<IMAGEINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_HBITMAP(obj->hbmMask);
  }
}

// int Unused1

HB_FUNC_STATIC(WAS_IMAGEINFO_SETUNUSED1)
{
  auto obj = static_cast<IMAGEINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->Unused1 = wa_par_int(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGEINFO_GETUNUSED1)
{
  auto obj = static_cast<IMAGEINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_int(obj->Unused1);
  }
}

// int Unused2

HB_FUNC_STATIC(WAS_IMAGEINFO_SETUNUSED2)
{
  auto obj = static_cast<IMAGEINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    obj->Unused2 = wa_par_int(1);
  }
}

HB_FUNC_STATIC(WAS_IMAGEINFO_GETUNUSED2)
{
  auto obj = static_cast<IMAGEINFO *>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if (obj != nullptr)
  {
    wa_ret_int(obj->Unused2);
  }
}

// RECT rcImage // TODO:

/*
typedef struct _IMAGEINFO {
  HBITMAP hbmImage;
  HBITMAP hbmMask;
  int     Unused1;
  int     Unused2;
  RECT    rcImage;
} IMAGEINFO, *LPIMAGEINFO;
*/

#pragma ENDDUMP
