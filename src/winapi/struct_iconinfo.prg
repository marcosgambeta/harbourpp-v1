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

/*
  NOTE: source code generated with the help of a code generator
*/

#include "hbclass.ch"

FUNCTION wasICONINFO()
RETURN was_ICONINFO():new()

CLASS WAS_ICONINFO

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // BOOL fIcon
   ASSIGN fIcon(l) INLINE ::setfIcon(l)
   ACCESS fIcon INLINE ::getfIcon()
   METHOD setfIcon
   METHOD getfIcon

   // DWORD xHotspot
   ASSIGN xHotspot(n) INLINE ::setxHotspot(n)
   ACCESS xHotspot INLINE ::getxHotspot()
   METHOD setxHotspot
   METHOD getxHotspot

   // DWORD yHotspot
   ASSIGN yHotspot(n) INLINE ::setyHotspot(n)
   ACCESS yHotspot INLINE ::getyHotspot()
   METHOD setyHotspot
   METHOD getyHotspot

   // HBITMAP hbmMask
   ASSIGN hbmMask(p) INLINE ::sethbmMask(p)
   ACCESS hbmMask INLINE ::gethbmMask()
   METHOD sethbmMask
   METHOD gethbmMask

   // HBITMAP hbmColor
   ASSIGN hbmColor(p) INLINE ::sethbmColor(p)
   ACCESS hbmColor INLINE ::gethbmColor()
   METHOD sethbmColor
   METHOD gethbmColor

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WAS_ICONINFO
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

HB_FUNC_STATIC( WAS_ICONINFO_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new ICONINFO());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WAS_ICONINFO_DELETE )
{
  auto obj = static_cast<ICONINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// BOOL fIcon

HB_FUNC_STATIC( WAS_ICONINFO_SETFICON )
{
  auto obj = static_cast<ICONINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fIcon = wa_par_BOOL(1);
  }
}

HB_FUNC_STATIC( WAS_ICONINFO_GETFICON )
{
  auto obj = static_cast<ICONINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_BOOL(obj->fIcon);
  }
}

// DWORD xHotspot

HB_FUNC_STATIC( WAS_ICONINFO_SETXHOTSPOT )
{
  auto obj = static_cast<ICONINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->xHotspot = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WAS_ICONINFO_GETXHOTSPOT )
{
  auto obj = static_cast<ICONINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->xHotspot);
  }
}

// DWORD yHotspot

HB_FUNC_STATIC( WAS_ICONINFO_SETYHOTSPOT )
{
  auto obj = static_cast<ICONINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->yHotspot = wa_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WAS_ICONINFO_GETYHOTSPOT )
{
  auto obj = static_cast<ICONINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_DWORD(obj->yHotspot);
  }
}

// HBITMAP hbmMask

HB_FUNC_STATIC( WAS_ICONINFO_SETHBMMASK )
{
  auto obj = static_cast<ICONINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hbmMask = wa_par_HBITMAP(1);
  }
}

HB_FUNC_STATIC( WAS_ICONINFO_GETHBMMASK )
{
  auto obj = static_cast<ICONINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HBITMAP(obj->hbmMask);
  }
}

// HBITMAP hbmColor

HB_FUNC_STATIC( WAS_ICONINFO_SETHBMCOLOR )
{
  auto obj = static_cast<ICONINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hbmColor = wa_par_HBITMAP(1);
  }
}

HB_FUNC_STATIC( WAS_ICONINFO_GETHBMCOLOR )
{
  auto obj = static_cast<ICONINFO*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HBITMAP(obj->hbmColor);
  }
}

/*
typedef struct _ICONINFO {
  BOOL    fIcon;
  DWORD   xHotspot;
  DWORD   yHotspot;
  HBITMAP hbmMask;
  HBITMAP hbmColor;
} ICONINFO;
*/

#pragma ENDDUMP
