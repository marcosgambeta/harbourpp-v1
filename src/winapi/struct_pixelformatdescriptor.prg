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

CLASS WINAPI_STRUCT_PIXELFORMATDESCRIPTOR

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // WORD nSize
   ASSIGN nSize(n) INLINE ::setnSize(n)
   ACCESS nSize INLINE ::getnSize()
   METHOD setnSize
   METHOD getnSize

   // WORD nVersion
   ASSIGN nVersion(n) INLINE ::setnVersion(n)
   ACCESS nVersion INLINE ::getnVersion()
   METHOD setnVersion
   METHOD getnVersion

   // DWORD dwFlags
   ASSIGN dwFlags(n) INLINE ::setdwFlags(n)
   ACCESS dwFlags INLINE ::getdwFlags()
   METHOD setdwFlags
   METHOD getdwFlags

   // BYTE iPixelType
   ASSIGN iPixelType(n) INLINE ::setiPixelType(n)
   ACCESS iPixelType INLINE ::getiPixelType()
   METHOD setiPixelType
   METHOD getiPixelType

   // BYTE cColorBits
   ASSIGN cColorBits(n) INLINE ::setcColorBits(n)
   ACCESS cColorBits INLINE ::getcColorBits()
   METHOD setcColorBits
   METHOD getcColorBits

   // BYTE cRedBits
   ASSIGN cRedBits(n) INLINE ::setcRedBits(n)
   ACCESS cRedBits INLINE ::getcRedBits()
   METHOD setcRedBits
   METHOD getcRedBits

   // BYTE cRedShift
   ASSIGN cRedShift(n) INLINE ::setcRedShift(n)
   ACCESS cRedShift INLINE ::getcRedShift()
   METHOD setcRedShift
   METHOD getcRedShift

   // BYTE cGreenBits
   ASSIGN cGreenBits(n) INLINE ::setcGreenBits(n)
   ACCESS cGreenBits INLINE ::getcGreenBits()
   METHOD setcGreenBits
   METHOD getcGreenBits

   // BYTE cGreenShift
   ASSIGN cGreenShift(n) INLINE ::setcGreenShift(n)
   ACCESS cGreenShift INLINE ::getcGreenShift()
   METHOD setcGreenShift
   METHOD getcGreenShift

   // BYTE cBlueBits
   ASSIGN cBlueBits(n) INLINE ::setcBlueBits(n)
   ACCESS cBlueBits INLINE ::getcBlueBits()
   METHOD setcBlueBits
   METHOD getcBlueBits

   // BYTE cBlueShift
   ASSIGN cBlueShift(n) INLINE ::setcBlueShift(n)
   ACCESS cBlueShift INLINE ::getcBlueShift()
   METHOD setcBlueShift
   METHOD getcBlueShift

   // BYTE cAlphaBits
   ASSIGN cAlphaBits(n) INLINE ::setcAlphaBits(n)
   ACCESS cAlphaBits INLINE ::getcAlphaBits()
   METHOD setcAlphaBits
   METHOD getcAlphaBits

   // BYTE cAlphaShift
   ASSIGN cAlphaShift(n) INLINE ::setcAlphaShift(n)
   ACCESS cAlphaShift INLINE ::getcAlphaShift()
   METHOD setcAlphaShift
   METHOD getcAlphaShift

   // BYTE cAccumBits
   ASSIGN cAccumBits(n) INLINE ::setcAccumBits(n)
   ACCESS cAccumBits INLINE ::getcAccumBits()
   METHOD setcAccumBits
   METHOD getcAccumBits

   // BYTE cAccumRedBits
   ASSIGN cAccumRedBits(n) INLINE ::setcAccumRedBits(n)
   ACCESS cAccumRedBits INLINE ::getcAccumRedBits()
   METHOD setcAccumRedBits
   METHOD getcAccumRedBits

   // BYTE cAccumGreenBits
   ASSIGN cAccumGreenBits(n) INLINE ::setcAccumGreenBits(n)
   ACCESS cAccumGreenBits INLINE ::getcAccumGreenBits()
   METHOD setcAccumGreenBits
   METHOD getcAccumGreenBits

   // BYTE cAccumBlueBits
   ASSIGN cAccumBlueBits(n) INLINE ::setcAccumBlueBits(n)
   ACCESS cAccumBlueBits INLINE ::getcAccumBlueBits()
   METHOD setcAccumBlueBits
   METHOD getcAccumBlueBits

   // BYTE cAccumAlphaBits
   ASSIGN cAccumAlphaBits(n) INLINE ::setcAccumAlphaBits(n)
   ACCESS cAccumAlphaBits INLINE ::getcAccumAlphaBits()
   METHOD setcAccumAlphaBits
   METHOD getcAccumAlphaBits

   // BYTE cDepthBits
   ASSIGN cDepthBits(n) INLINE ::setcDepthBits(n)
   ACCESS cDepthBits INLINE ::getcDepthBits()
   METHOD setcDepthBits
   METHOD getcDepthBits

   // BYTE cStencilBits
   ASSIGN cStencilBits(n) INLINE ::setcStencilBits(n)
   ACCESS cStencilBits INLINE ::getcStencilBits()
   METHOD setcStencilBits
   METHOD getcStencilBits

   // BYTE cAuxBuffers
   ASSIGN cAuxBuffers(n) INLINE ::setcAuxBuffers(n)
   ACCESS cAuxBuffers INLINE ::getcAuxBuffers()
   METHOD setcAuxBuffers
   METHOD getcAuxBuffers

   // BYTE iLayerType
   ASSIGN iLayerType(n) INLINE ::setiLayerType(n)
   ACCESS iLayerType INLINE ::getiLayerType()
   METHOD setiLayerType
   METHOD getiLayerType

   // BYTE bReserved
   ASSIGN bReserved(n) INLINE ::setbReserved(n)
   ACCESS bReserved INLINE ::getbReserved()
   METHOD setbReserved
   METHOD getbReserved

   // DWORD dwLayerMask
   ASSIGN dwLayerMask(n) INLINE ::setdwLayerMask(n)
   ACCESS dwLayerMask INLINE ::getdwLayerMask()
   METHOD setdwLayerMask
   METHOD getdwLayerMask

   // DWORD dwVisibleMask
   ASSIGN dwVisibleMask(n) INLINE ::setdwVisibleMask(n)
   ACCESS dwVisibleMask INLINE ::getdwVisibleMask()
   METHOD setdwVisibleMask
   METHOD getdwVisibleMask

   // DWORD dwDamageMask
   ASSIGN dwDamageMask(n) INLINE ::setdwDamageMask(n)
   ACCESS dwDamageMask INLINE ::getdwDamageMask()
   METHOD setdwDamageMask
   METHOD getdwDamageMask

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_PIXELFORMATDESCRIPTOR
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

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new PIXELFORMATDESCRIPTOR());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_DELETE )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// WORD nSize

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETNSIZE )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->nSize = winapi_par_WORD(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETNSIZE )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_WORD(obj->nSize);
  }
}

// WORD nVersion

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETNVERSION )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->nVersion = winapi_par_WORD(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETNVERSION )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_WORD(obj->nVersion);
  }
}

// DWORD dwFlags

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETDWFLAGS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwFlags = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETDWFLAGS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->dwFlags);
  }
}

// BYTE iPixelType

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETIPIXELTYPE )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->iPixelType = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETIPIXELTYPE )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->iPixelType);
  }
}

// BYTE cColorBits

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCCOLORBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cColorBits = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCCOLORBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cColorBits);
  }
}

// BYTE cRedBits

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCREDBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cRedBits = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCREDBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cRedBits);
  }
}

// BYTE cRedShift

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCREDSHIFT )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cRedShift = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCREDSHIFT )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cRedShift);
  }
}

// BYTE cGreenBits

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCGREENBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cGreenBits = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCGREENBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cGreenBits);
  }
}

// BYTE cGreenShift

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCGREENSHIFT )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cGreenShift = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCGREENSHIFT )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cGreenShift);
  }
}

// BYTE cBlueBits

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCBLUEBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cBlueBits = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCBLUEBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cBlueBits);
  }
}

// BYTE cBlueShift

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCBLUESHIFT )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cBlueShift = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCBLUESHIFT )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cBlueShift);
  }
}

// BYTE cAlphaBits

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCALPHABITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cAlphaBits = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCALPHABITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cAlphaBits);
  }
}

// BYTE cAlphaShift

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCALPHASHIFT )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cAlphaShift = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCALPHASHIFT )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cAlphaShift);
  }
}

// BYTE cAccumBits

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCACCUMBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cAccumBits = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCACCUMBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cAccumBits);
  }
}

// BYTE cAccumRedBits

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCACCUMREDBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cAccumRedBits = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCACCUMREDBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cAccumRedBits);
  }
}

// BYTE cAccumGreenBits

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCACCUMGREENBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cAccumGreenBits = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCACCUMGREENBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cAccumGreenBits);
  }
}

// BYTE cAccumBlueBits

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCACCUMBLUEBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cAccumBlueBits = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCACCUMBLUEBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cAccumBlueBits);
  }
}

// BYTE cAccumAlphaBits

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCACCUMALPHABITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cAccumAlphaBits = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCACCUMALPHABITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cAccumAlphaBits);
  }
}

// BYTE cDepthBits

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCDEPTHBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cDepthBits = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCDEPTHBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cDepthBits);
  }
}

// BYTE cStencilBits

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCSTENCILBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cStencilBits = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCSTENCILBITS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cStencilBits);
  }
}

// BYTE cAuxBuffers

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETCAUXBUFFERS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->cAuxBuffers = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETCAUXBUFFERS )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->cAuxBuffers);
  }
}

// BYTE iLayerType

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETILAYERTYPE )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->iLayerType = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETILAYERTYPE )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->iLayerType);
  }
}

// BYTE bReserved

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETBRESERVED )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->bReserved = winapi_par_BYTE(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETBRESERVED )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_BYTE(obj->bReserved);
  }
}

// DWORD dwLayerMask

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETDWLAYERMASK )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwLayerMask = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETDWLAYERMASK )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->dwLayerMask);
  }
}

// DWORD dwVisibleMask

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETDWVISIBLEMASK )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwVisibleMask = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETDWVISIBLEMASK )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->dwVisibleMask);
  }
}

// DWORD dwDamageMask

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_SETDWDAMAGEMASK )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->dwDamageMask = winapi_par_DWORD(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_PIXELFORMATDESCRIPTOR_GETDWDAMAGEMASK )
{
  auto obj = static_cast<PIXELFORMATDESCRIPTOR*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    winapi_ret_DWORD(obj->dwDamageMask);
  }
}

/*
typedef struct tagPIXELFORMATDESCRIPTOR {
  WORD  nSize;
  WORD  nVersion;
  DWORD dwFlags;
  BYTE  iPixelType;
  BYTE  cColorBits;
  BYTE  cRedBits;
  BYTE  cRedShift;
  BYTE  cGreenBits;
  BYTE  cGreenShift;
  BYTE  cBlueBits;
  BYTE  cBlueShift;
  BYTE  cAlphaBits;
  BYTE  cAlphaShift;
  BYTE  cAccumBits;
  BYTE  cAccumRedBits;
  BYTE  cAccumGreenBits;
  BYTE  cAccumBlueBits;
  BYTE  cAccumAlphaBits;
  BYTE  cDepthBits;
  BYTE  cStencilBits;
  BYTE  cAuxBuffers;
  BYTE  iLayerType;
  BYTE  bReserved;
  DWORD dwLayerMask;
  DWORD dwVisibleMask;
  DWORD dwDamageMask;
} PIXELFORMATDESCRIPTOR, *PPIXELFORMATDESCRIPTOR, *LPPIXELFORMATDESCRIPTOR;
*/

#pragma ENDDUMP
