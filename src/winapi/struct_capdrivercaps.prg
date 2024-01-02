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

CLASS WASCAPDRIVERCAPS

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

PROCEDURE destroyObject() CLASS WASCAPDRIVERCAPS
   IF ::self_destruction
      ::delete()
   ENDIF
RETURN

#pragma BEGINDUMP

// to avoid warning
#ifdef UNICODE
#define _UNICODE
#endif

#include <windows.h>
#include <vfw.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "winapi.hpp"

HB_FUNC_STATIC( WASCAPDRIVERCAPS_NEW )
{
  auto self = hb_stackSelfItem();
  hb_objDataPutPtr(self, "_PTR", new CAPDRIVERCAPS());
  hb_objDataPutL(self, "_SELF_DESTRUCTION", true);
  hb_itemReturn(self);
}

HB_FUNC_STATIC( WASCAPDRIVERCAPS_DELETE )
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

HB_FUNC_STATIC( WASCAPDRIVERCAPS_SETWDEVICEINDEX )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->wDeviceIndex = wa_par_UINT(1);
  }
}

HB_FUNC_STATIC( WASCAPDRIVERCAPS_GETWDEVICEINDEX )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_UINT(obj->wDeviceIndex);
  }
}

// BOOL fHasOverlay

HB_FUNC_STATIC( WASCAPDRIVERCAPS_SETFHASOVERLAY )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fHasOverlay = wa_par_BOOL(1);
  }
}

HB_FUNC_STATIC( WASCAPDRIVERCAPS_GETFHASOVERLAY )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_BOOL(obj->fHasOverlay);
  }
}

// BOOL fHasDlgVideoSource

HB_FUNC_STATIC( WASCAPDRIVERCAPS_SETFHASDLGVIDEOSOURCE )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fHasDlgVideoSource = wa_par_BOOL(1);
  }
}

HB_FUNC_STATIC( WASCAPDRIVERCAPS_GETFHASDLGVIDEOSOURCE )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_BOOL(obj->fHasDlgVideoSource);
  }
}

// BOOL fHasDlgVideoFormat

HB_FUNC_STATIC( WASCAPDRIVERCAPS_SETFHASDLGVIDEOFORMAT )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fHasDlgVideoFormat = wa_par_BOOL(1);
  }
}

HB_FUNC_STATIC( WASCAPDRIVERCAPS_GETFHASDLGVIDEOFORMAT )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_BOOL(obj->fHasDlgVideoFormat);
  }
}

// BOOL fHasDlgVideoDisplay

HB_FUNC_STATIC( WASCAPDRIVERCAPS_SETFHASDLGVIDEODISPLAY )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fHasDlgVideoDisplay = wa_par_BOOL(1);
  }
}

HB_FUNC_STATIC( WASCAPDRIVERCAPS_GETFHASDLGVIDEODISPLAY )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_BOOL(obj->fHasDlgVideoDisplay);
  }
}

// BOOL fCaptureInitialized

HB_FUNC_STATIC( WASCAPDRIVERCAPS_SETFCAPTUREINITIALIZED )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fCaptureInitialized = wa_par_BOOL(1);
  }
}

HB_FUNC_STATIC( WASCAPDRIVERCAPS_GETFCAPTUREINITIALIZED )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_BOOL(obj->fCaptureInitialized);
  }
}

// BOOL fDriverSuppliesPalettes

HB_FUNC_STATIC( WASCAPDRIVERCAPS_SETFDRIVERSUPPLIESPALETTES )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->fDriverSuppliesPalettes = wa_par_BOOL(1);
  }
}

HB_FUNC_STATIC( WASCAPDRIVERCAPS_GETFDRIVERSUPPLIESPALETTES )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_BOOL(obj->fDriverSuppliesPalettes);
  }
}

// HANDLE hVideoIn

HB_FUNC_STATIC( WASCAPDRIVERCAPS_SETHVIDEOIN )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hVideoIn = wa_par_HANDLE(1);
  }
}

HB_FUNC_STATIC( WASCAPDRIVERCAPS_GETHVIDEOIN )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HANDLE(obj->hVideoIn);
  }
}

// HANDLE hVideoOut

HB_FUNC_STATIC( WASCAPDRIVERCAPS_SETHVIDEOOUT )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hVideoOut = wa_par_HANDLE(1);
  }
}

HB_FUNC_STATIC( WASCAPDRIVERCAPS_GETHVIDEOOUT )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HANDLE(obj->hVideoOut);
  }
}

// HANDLE hVideoExtIn

HB_FUNC_STATIC( WASCAPDRIVERCAPS_SETHVIDEOEXTIN )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hVideoExtIn = wa_par_HANDLE(1);
  }
}

HB_FUNC_STATIC( WASCAPDRIVERCAPS_GETHVIDEOEXTIN )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HANDLE(obj->hVideoExtIn);
  }
}

// HANDLE hVideoExtOut

HB_FUNC_STATIC( WASCAPDRIVERCAPS_SETHVIDEOEXTOUT )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    obj->hVideoExtOut = wa_par_HANDLE(1);
  }
}

HB_FUNC_STATIC( WASCAPDRIVERCAPS_GETHVIDEOEXTOUT )
{
  auto obj = static_cast<CAPDRIVERCAPS*>(hb_objDataGetPtr(hb_stackSelfItem(), "PTR"));

  if( obj != nullptr )
  {
    wa_ret_HANDLE(obj->hVideoExtOut);
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
