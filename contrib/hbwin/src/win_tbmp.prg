//
// win_BMP() class
//
// Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software and Systems Ltd
//

// $HB_BEGIN_LICENSE$
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file LICENSE.txt.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
//
// As a special exception, the Harbour Project gives permission for
// additional uses of the text contained in its release of Harbour.
//
// The exception is that, if you link the Harbour libraries with other
// files to produce an executable, this does not by itself cause the
// resulting executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of
// linking the Harbour library code into it.
//
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.
//
// This exception applies only to the code released by the Harbour
// Project under the name Harbour.  If you copy code from other
// Harbour Project or Free Software Foundation releases into a copy of
// Harbour, as the General Public License permits, the exception does
// not apply to the code that you add in this way.  To avoid misleading
// anyone as to the status of such modified files, you must delete
// this exception notice from them.
//
// If you write modifications of your own for Harbour, it is your choice
// whether to permit this exception to apply to your modifications.
// If you do not wish that, delete this exception notice.
// $HB_END_LICENSE$

#include <hbclass.ch>

#include "hbwin.ch"

CREATE CLASS win_BMP

   EXPORTED:

   METHOD New()
   METHOD LoadFile(cFileName, aDimXY)
   METHOD Create()
   METHOD Destroy()
   METHOD IsSupported(oPrn, /* @ */ nError)
   METHOD Draw(oPrn, aRectangle, /* @ */ nError)

   VAR Type     INIT 0                  // Type BitMap: 1 == BM, 2 == JPEG, 3 == PNG
   VAR DimXY    INIT { 0, 0 }           // Image Dimensions X Y pixels
   VAR Rect     INIT { 0, 0, 0, 0 }     // Coordinates to print BitMap
                                        //   XDest,                    : x-coord of destination upper-left corner
                                        //   YDest,                    : y-coord of destination upper-left corner
                                        //   nDestWidth,               : width of destination rectangle
                                        //   nDestHeight,              : height of destination rectangle
                                        // See WinApi StretchDIBits()
   VAR BitMap   INIT ""
   VAR FileName INIT ""

ENDCLASS

METHOD win_BMP:New()
   RETURN Self

METHOD win_BMP:LoadFile(cFileName, aDimXY)

   ::FileName := cFileName
   ::Bitmap := win_LoadBitmapFile(::FileName)
   IF Empty(::Bitmap)
      ::Type := 0
      ::DimXY := { 0, 0 }
   ELSE
      ::Type := win_bitmapType(::Bitmap)
      IF hb_IsArray(aDimXY)
         ::DimXY := aDimXY
      ELSEIF !win_bitmapDimensions(::Bitmap, @::DimXY[1], @::DimXY[2])
         ::DimXY := { 1, 1 } // Driver may use the original dimensions
      ENDIF
   ENDIF

   RETURN ::Type != HB_WIN_BITMAP_UNKNOWN

METHOD win_BMP:Create()  // Compatibility function for Alaska Xbase++
   RETURN Self

METHOD win_BMP:Destroy()  // Compatibility function for Alaska Xbase++
   RETURN NIL

METHOD win_BMP:IsSupported(oPrn, /* @ */ nError)
   RETURN (nError := win_bitmapIsSupported(oPrn:hPrinterDc, ::Bitmap)) == 0

METHOD win_BMP:Draw(oPrn, aRectangle, /* @ */ nError) // Pass a win_Prn object reference and rectangle array

   IF hb_IsArray(aRectangle)
      ::Rect := aRectangle
   ENDIF

   RETURN IIf(::IsSupported(oPrn, @nError), oPrn:DrawBitMap(Self), .F.)
