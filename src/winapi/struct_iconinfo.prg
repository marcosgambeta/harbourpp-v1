/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (C) 2022 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2022 Marcos Antonio Gambeta

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

CLASS WINAPI_STRUCT_ICONINFO

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

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_ICONINFO
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"

HB_FUNC_STATIC( WINAPI_STRUCT_ICONINFO_NEW )
{
  auto obj = new ICONINFO();
  PHB_ITEM self = hb_stackSelfItem();
  PHB_ITEM ptr = hb_itemPutPtr( nullptr, ( void * ) obj );
  hb_objSendMsg( self, "_ptr", 1, ptr );
  hb_itemRelease( ptr );
  PHB_ITEM des = hb_itemPutL( nullptr, true );
  hb_objSendMsg( self, "_SELF_DESTRUCTION", 1, des );
  hb_itemRelease( des );
  hb_itemReturn( self );
}

HB_FUNC_STATIC( WINAPI_STRUCT_ICONINFO_DELETE )
{
  auto obj = static_cast<ICONINFO*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    delete obj;
    obj = nullptr;
    PHB_ITEM self = hb_stackSelfItem();
    PHB_ITEM ptr = hb_itemPutPtr( nullptr, nullptr );
    hb_objSendMsg( self, "_ptr", 1, ptr );
    hb_itemRelease( ptr );
  }

  hb_itemReturn( hb_stackSelfItem() );
}

// BOOL fIcon

HB_FUNC_STATIC( WINAPI_STRUCT_ICONINFO_SETFICON )
{
  auto obj = static_cast<ICONINFO*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->fIcon = hb_parl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_ICONINFO_GETFICON )
{
  auto obj = static_cast<ICONINFO*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retl(obj->fIcon);
  }
}

// DWORD xHotspot

HB_FUNC_STATIC( WINAPI_STRUCT_ICONINFO_SETXHOTSPOT )
{
  auto obj = static_cast<ICONINFO*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->xHotspot = hb_parnl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_ICONINFO_GETXHOTSPOT )
{
  auto obj = static_cast<ICONINFO*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->xHotspot);
  }
}

// DWORD yHotspot

HB_FUNC_STATIC( WINAPI_STRUCT_ICONINFO_SETYHOTSPOT )
{
  auto obj = static_cast<ICONINFO*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->yHotspot = hb_parnl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_ICONINFO_GETYHOTSPOT )
{
  auto obj = static_cast<ICONINFO*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retnl(obj->yHotspot);
  }
}

// HBITMAP hbmMask

HB_FUNC_STATIC( WINAPI_STRUCT_ICONINFO_SETHBMMASK )
{
  auto obj = static_cast<ICONINFO*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->hbmMask = static_cast<HBITMAP>(hb_parptr(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_ICONINFO_GETHBMMASK )
{
  auto obj = static_cast<ICONINFO*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retptr(obj->hbmMask);
  }
}

// HBITMAP hbmColor

HB_FUNC_STATIC( WINAPI_STRUCT_ICONINFO_SETHBMCOLOR )
{
  auto obj = static_cast<ICONINFO*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    obj->hbmColor = static_cast<HBITMAP>(hb_parptr(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_ICONINFO_GETHBMCOLOR )
{
  auto obj = static_cast<ICONINFO*>(hb_itemGetPtr(hb_objSendMsg(hb_stackSelfItem(), "PTR", 0)));

  if( obj != nullptr )
  {
    hb_retptr(obj->hbmColor);
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