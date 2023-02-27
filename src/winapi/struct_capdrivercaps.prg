/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2022-2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2022-2023 Marcos Antonio Gambeta

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

CLASS WINAPI_STRUCT_CAPDRIVERCAPS

   DATA ptr
   DATA self_destruction INIT .F.

   METHOD new
   METHOD delete

   // UINT wDeviceIndex
   ASSIGN wDeviceIndex(n) INLINE ::setwDeviceIndex(n)
   ACCESS wDeviceIndex INLINE ::getwDeviceIndex()
   METHOD setwDeviceIndex
   METHOD getwDeviceIndex

   // BOOL fHasOverlay
   ASSIGN fHasOverlay(l) INLINE ::setfHasOverlay(l)
   ACCESS fHasOverlay INLINE ::getfHasOverlay()
   METHOD setfHasOverlay
   METHOD getfHasOverlay

   // BOOL fHasDlgVideoSource
   ASSIGN fHasDlgVideoSource(l) INLINE ::setfHasDlgVideoSource(l)
   ACCESS fHasDlgVideoSource INLINE ::getfHasDlgVideoSource()
   METHOD setfHasDlgVideoSource
   METHOD getfHasDlgVideoSource

   // BOOL fHasDlgVideoFormat
   ASSIGN fHasDlgVideoFormat(l) INLINE ::setfHasDlgVideoFormat(l)
   ACCESS fHasDlgVideoFormat INLINE ::getfHasDlgVideoFormat()
   METHOD setfHasDlgVideoFormat
   METHOD getfHasDlgVideoFormat

   // BOOL fHasDlgVideoDisplay
   ASSIGN fHasDlgVideoDisplay(l) INLINE ::setfHasDlgVideoDisplay(l)
   ACCESS fHasDlgVideoDisplay INLINE ::getfHasDlgVideoDisplay()
   METHOD setfHasDlgVideoDisplay
   METHOD getfHasDlgVideoDisplay

   // BOOL fCaptureInitialized
   ASSIGN fCaptureInitialized(l) INLINE ::setfCaptureInitialized(l)
   ACCESS fCaptureInitialized INLINE ::getfCaptureInitialized()
   METHOD setfCaptureInitialized
   METHOD getfCaptureInitialized

   // BOOL fDriverSuppliesPalettes
   ASSIGN fDriverSuppliesPalettes(l) INLINE ::setfDriverSuppliesPalettes(l)
   ACCESS fDriverSuppliesPalettes INLINE ::getfDriverSuppliesPalettes()
   METHOD setfDriverSuppliesPalettes
   METHOD getfDriverSuppliesPalettes

   // HANDLE hVideoIn
   ASSIGN hVideoIn(p) INLINE ::sethVideoIn(p)
   ACCESS hVideoIn INLINE ::gethVideoIn()
   METHOD sethVideoIn
   METHOD gethVideoIn

   // HANDLE hVideoOut
   ASSIGN hVideoOut(p) INLINE ::sethVideoOut(p)
   ACCESS hVideoOut INLINE ::gethVideoOut()
   METHOD sethVideoOut
   METHOD gethVideoOut

   // HANDLE hVideoExtIn
   ASSIGN hVideoExtIn(p) INLINE ::sethVideoExtIn(p)
   ACCESS hVideoExtIn INLINE ::gethVideoExtIn()
   METHOD sethVideoExtIn
   METHOD gethVideoExtIn

   // HANDLE hVideoExtOut
   ASSIGN hVideoExtOut(p) INLINE ::sethVideoExtOut(p)
   ACCESS hVideoExtOut INLINE ::gethVideoExtOut()
   METHOD sethVideoExtOut
   METHOD gethVideoExtOut

   DESTRUCTOR destroyObject

END CLASS

PROCEDURE destroyObject() CLASS WINAPI_STRUCT_CAPDRIVERCAPS
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

#include <windows.h>
#include <vfw.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_NEW )
{
  PHB_ITEM self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new CAPDRIVERCAPS());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_DELETE )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    delete obj;
    hb_objDataPutPtr(hb_stackSelfItem(), "_PTR", nullptr);
  }

  hb_itemReturn(hb_stackSelfItem());
}

// UINT wDeviceIndex

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_SETWDEVICEINDEX )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->wDeviceIndex = hb_parni(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_GETWDEVICEINDEX )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retni(obj->wDeviceIndex);
  }
}

// BOOL fHasOverlay

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_SETFHASOVERLAY )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fHasOverlay = hb_parl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_GETFHASOVERLAY )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retl(obj->fHasOverlay);
  }
}

// BOOL fHasDlgVideoSource

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_SETFHASDLGVIDEOSOURCE )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fHasDlgVideoSource = hb_parl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_GETFHASDLGVIDEOSOURCE )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retl(obj->fHasDlgVideoSource);
  }
}

// BOOL fHasDlgVideoFormat

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_SETFHASDLGVIDEOFORMAT )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fHasDlgVideoFormat = hb_parl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_GETFHASDLGVIDEOFORMAT )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retl(obj->fHasDlgVideoFormat);
  }
}

// BOOL fHasDlgVideoDisplay

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_SETFHASDLGVIDEODISPLAY )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fHasDlgVideoDisplay = hb_parl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_GETFHASDLGVIDEODISPLAY )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retl(obj->fHasDlgVideoDisplay);
  }
}

// BOOL fCaptureInitialized

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_SETFCAPTUREINITIALIZED )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fCaptureInitialized = hb_parl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_GETFCAPTUREINITIALIZED )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retl(obj->fCaptureInitialized);
  }
}

// BOOL fDriverSuppliesPalettes

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_SETFDRIVERSUPPLIESPALETTES )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fDriverSuppliesPalettes = hb_parl(1);
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_GETFDRIVERSUPPLIESPALETTES )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retl(obj->fDriverSuppliesPalettes);
  }
}

// HANDLE hVideoIn

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_SETHVIDEOIN )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hVideoIn = static_cast<HANDLE>(hb_parptr(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_GETHVIDEOIN )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retptr(obj->hVideoIn);
  }
}

// HANDLE hVideoOut

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_SETHVIDEOOUT )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hVideoOut = static_cast<HANDLE>(hb_parptr(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_GETHVIDEOOUT )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retptr(obj->hVideoOut);
  }
}

// HANDLE hVideoExtIn

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_SETHVIDEOEXTIN )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hVideoExtIn = static_cast<HANDLE>(hb_parptr(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_GETHVIDEOEXTIN )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retptr(obj->hVideoExtIn);
  }
}

// HANDLE hVideoExtOut

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_SETHVIDEOEXTOUT )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hVideoExtOut = static_cast<HANDLE>(hb_parptr(1));
  }
}

HB_FUNC_STATIC( WINAPI_STRUCT_CAPDRIVERCAPS_GETHVIDEOEXTOUT )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    hb_retptr(obj->hVideoExtOut);
  }
}

/*
typedef struct tagCapDriverCaps {
  UINT   wDeviceIndex;
  BOOL   fHasOverlay;
  BOOL   fHasDlgVideoSource;
  BOOL   fHasDlgVideoFormat;
  BOOL   fHasDlgVideoDisplay;
  BOOL   fCaptureInitialized;
  BOOL   fDriverSuppliesPalettes;
  HANDLE hVideoIn;
  HANDLE hVideoOut;
  HANDLE hVideoExtIn;
  HANDLE hVideoExtOut;
} CAPDRIVERCAPS, *PCAPDRIVERCAPS, *LPCAPDRIVERCAPS;
*/

#pragma ENDDUMP
