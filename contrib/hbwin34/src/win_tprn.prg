//
// Printing subsystem for Windows using GUI printing
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

/*
   win_Prn() was designed to make it easy to emulate Clipper Dot Matrix printing.
   Dot Matrix printing was in CPI ( Characters per inch and Lines per inch ).
   Even though "Mapping Mode" for win_Prn() is WIN_MM_TEXT, ::SetFont() accepts the
   xWidth parameter in CPI not Pixels. Also the default ::LineHeight is for
   6 lines per inch so ::NewLine() works as per "LineFeed" on Dot Matrix printers.
   If you do not like this then inherit from the class and override anything you want

   Simple example

   TODO: Color printing
         etc....

   Peter Rees 2004-01-21 <peter@rees.co.nz>
 */

#include <hbclass.ch>
#include <hbver.ch>
#include "hbwin.ch"

CREATE CLASS win_Prn

   METHOD New(cPrinter)
   METHOD Create()                  // CreatesDC and sets "Courier New" font, set Orientation, Copies, Bin#
                                    // Create() ( and StartDoc() ) must be called before printing can start.
   METHOD Destroy()                 // Calls EndDoc() - restores default font, Deletes DC.
   DESTRUCTOR Destruct()

   METHOD StartDoc(cDocName)        // Calls StartPage()
   METHOD EndDoc(lAbortDoc)         // Calls EndPage() if lAbortDoc not .T.
   METHOD StartPage()
   METHOD EndPage(lStartNewPage)    // If lStartNewPage == .T. then StartPage() is called for the next page of output
   METHOD NewLine()
   METHOD NewPage(lDelay)           // If lDelay == .T. then new page is not created immediately but just before 1st output
   METHOD CheckPage()
   METHOD GetDocumentProperties()
   METHOD SetFont(cFontName, nPointSize, xWidth, nBold, lUnderline, lItalic, nCharSet, lManualSize)
                                                      // NB: xWidth is in "CharactersPerInch"
                                                      //     _OR_ { nMul, nDiv } which equates to "CharactersPerInch"
                                                      //     _OR_ ZERO (0) which uses the default width of the font
                                                      //          for the nPointSize
                                                      //   IF xWidth (or nDiv) is < 0 then Fixed font is emulated

   METHOD SetDefaultFont()

   METHOD GetFonts()                                  // Returns array of { "FontName", lFixed, lTrueType, nCharSetRequired }
   METHOD Bold(nWeight)
   METHOD UnderLine(lUnderline)
   METHOD Italic(lItalic)
   METHOD SetDuplexType(nDuplexType)                  // Get/Set current duplex mode
   METHOD SetPrintQuality(nPrintQuality)              // Get/Set print quality
   METHOD CharSet(nCharSet)

   METHOD SetPos(nPosX, nPosY)                        // WARNING: ( Col, Row ) _NOT_ ( Row, Col )
   METHOD SetColor(nClrText, nClrPane, nAlign)
   METHOD SetBkMode(nMode)                            // Set background mode: WIN_TRANSPARENT or WIN_OPAQUE

   METHOD TextOut(cString, lNewLine, lUpdatePosX, nAlign)    // nAlign : WIN_TA_*
   METHOD TextOutAt(nPosX, nPosY, cString, lNewLine, lUpdatePosX, nAlign)     // WARNING: ( Col, Row ) _NOT_ ( Row, Col )

   METHOD SetPen(nStyle, nWidth, nColor)
   METHOD Line(nX1, nY1, nX2, nY2)
   METHOD Box(nX1, nY1, nX2, nY2, nWidth, nHeight)
   METHOD Arc(nX1, nY1, nX2, nY2)
   METHOD Ellipse(nX1, nY1, nX2, nY2)
   METHOD FillRect(nX1, nY1, nX2, nY2, nColor)
   METHOD GetCharWidth()
   METHOD GetCharHeight()
   METHOD GetTextWidth(cString)
   METHOD GetTextHeight(cString)
   METHOD DrawBitmap(oBmp)

   /* Clipper compatible functions. */
   METHOD SetPRC(nRow, nCol)     // Based on ::LineHeight and current ::CharWidth
   METHOD PRow()
   METHOD PCol()
   METHOD MaxRow()               // Based on ::LineHeight and form dimensions
   METHOD MaxCol()               // Based on ::CharWidth and form dimensions

   METHOD MM_TO_POSX(nMm)        // Convert position on page from MM to pixel location Column
   METHOD MM_TO_POSY(nMm)        //   "       "      "    "    "   "  "   "      "     Row
   METHOD INCH_TO_POSX(nInch)    // Convert position on page from INCH to pixel location Column
   METHOD INCH_TO_POSY(nInch)    //   "       "      "    "    "   "    "   "       "    Row

   METHOD TextAtFont(nPosX, nPosY, cString, cFont, nPointSize, ;      // Print text string at location
                     nWidth, nBold, lUnderLine, lItalic, nCharSet, ;  // in specified font and color.
                     lNewLine, lUpdatePosX, nColor, nAlign)           // Restore original font and color
                                                                      // after printing.

   METHOD GetDeviceCaps(nCaps)

   VAR PrinterName      INIT ""
   VAR Printing         INIT .F.
   VAR HavePrinted      INIT .F.
   VAR PageInit         INIT .F.
   VAR PageNumber       INIT 0
   VAR hPrinterDc

   VAR AskProperties    INIT .F.

   /* These next 6 variables must be set before calling ::Create() if
      you wish to alter the defaults */
   VAR FormType         INIT 0
   VAR BinNumber        INIT 0
   VAR Landscape        INIT .F.
   VAR Copies           INIT 1
   VAR Collate          INIT .F.
   VAR PaperLength      INIT 0                           // Value is * 1/10 of mm   1000 == 10cm
   VAR PaperWidth       INIT 0                           //   "    "    "     "       "     "

   VAR SetFontOk        INIT .F.
   VAR hFont
   VAR FontName         INIT ""                          // Current point size for font
   VAR FontPointSize    INIT 12                          // Point size for font
   VAR FontWidth        INIT {0, 0}                      // { Mul, Div } Calc width: nWidth := wapi_MulDiv( nMul, wapi_GetDeviceCaps( HDC, WIN_LOGPIXELSX ), nDiv )
                                                         // If font width is specified it is in "characters per inch" to emulate DotMatrix
   VAR fBold            INIT 0                   HIDDEN  // font darkness weight (Bold). See wingdi.h or Windows SDK CreateFont() for valid values.
   VAR fUnderLine       INIT .F.                 HIDDEN  // UnderLine is on or off
   VAR fItalic          INIT .F.                 HIDDEN  // Italic is on or off
   VAR fCharSet         INIT WIN_DEFAULT_CHARSET HIDDEN

   VAR PixelsPerInchY   INIT 0
   VAR PixelsPerInchX   INIT 0
   VAR PageHeight       INIT 0
   VAR PageWidth        INIT 0
   VAR TopMargin        INIT 0
   VAR BottomMargin     INIT 0
   VAR LeftMargin       INIT 0
   VAR RightMargin      INIT 0
   VAR LineHeight       INIT 0
   VAR CharHeight       INIT 0
   VAR CharWidth        INIT 0
   VAR fCharWidth       INIT 0      HIDDEN
   VAR BitmapsOk        INIT .F.
   VAR NumColors        INIT 1
   VAR fDuplexType      INIT 0      HIDDEN
   VAR fPrintQuality    INIT 0      HIDDEN
   VAR fNewDuplexType   INIT 0      HIDDEN
   VAR fNewPrintQuality INIT 0      HIDDEN
   VAR fOldLandScape    INIT .F.    HIDDEN
   VAR fOldBinNumber    INIT 0      HIDDEN
   VAR fOldFormType     INIT 0      HIDDEN
   VAR fOldPaperLength  INIT 0      HIDDEN
   VAR fOldPaperWidth   INIT 0      HIDDEN

   VAR PosX             INIT 0
   VAR PosY             INIT 0

   VAR TextColor
   VAR BkColor
   VAR TextAlign

   VAR BkMode

   VAR hPen
   VAR PenStyle
   VAR PenWidth
   VAR PenColor

ENDCLASS

METHOD win_Prn:New(cPrinter)

   ::PrinterName := IIf(!HB_IsString(cPrinter) .OR. Empty(cPrinter), win_printerGetDefault(), cPrinter)
   /* Initialized with the current properties of the printer [jarabal] */
   ::GetDocumentProperties()

   RETURN Self

METHOD win_Prn:Create()

   LOCAL lResult := .F.

   ::Destroy()  // Finish current print job if any
   IF !Empty(::hPrinterDC := wapi_CreateDC(, ::PrinterName))

      // Set Form Type
      // Set Number of Copies, optionally enable collation
      // Set Orientation
      // Set Duplex mode
      // Set PrintQuality

      IF ::AskProperties
         lResult := win_SetDocumentProperties(::hPrinterDC, ::PrinterName, @::FormType, @::Landscape, @::Copies, @::BinNumber, ;
            @::fDuplexType, @::fPrintQuality, @::PaperLength, @::PaperWidth, @::Collate)
      ELSE
         lResult := win_SetDocumentProperties(::hPrinterDC, ::PrinterName, ::FormType, ::Landscape, ::Copies, ::BinNumber, ;
            ::fDuplexType, ::fPrintQuality, ::PaperLength, ::PaperWidth, ::Collate)
      ENDIF

      IF lResult
         IF HB_IsNumeric(::BkMode)
            wapi_SetBkMode(::hPrinterDc, ::BkMode)
         ENDIF
         // Set mapping mode to pixels, top-left down
         wapi_SetMapMode(::hPrinterDC, WIN_MM_TEXT)
#if 0
         win_SetTextCharacterExtra(::hPrinterDC, 0)  // do not add extra char spacing even if bold
#endif
         // Get Margins etc... here
         ::PageWidth        := wapi_GetDeviceCaps(::hPrinterDC, WIN_PHYSICALWIDTH)
         ::PageHeight       := wapi_GetDeviceCaps(::hPrinterDC, WIN_PHYSICALHEIGHT)
         ::LeftMargin       := wapi_GetDeviceCaps(::hPrinterDC, WIN_PHYSICALOFFSETX)
         ::RightMargin      := wapi_GetDeviceCaps(::hPrinterDC, WIN_HORZRES)
         ::PixelsPerInchY   := wapi_GetDeviceCaps(::hPrinterDC, WIN_LOGPIXELSY)
         ::PixelsPerInchX   := wapi_GetDeviceCaps(::hPrinterDC, WIN_LOGPIXELSX)
         ::LineHeight       := Int(::PixelsPerInchY / 6)  // Default 6 lines per inch == # of pixels per line
         ::TopMargin        := wapi_GetDeviceCaps(::hPrinterDC, WIN_PHYSICALOFFSETY)
         ::BottomMargin     := wapi_GetDeviceCaps(::hPrinterDC, WIN_VERTRES)

         // Set .T. if can print bitmaps
         ::BitMapsOk := hb_bitAnd(wapi_GetDeviceCaps(::hPrinterDC, WIN_RASTERCAPS), WIN_RC_STRETCHDIB) != 0

         // supports Color
         ::NumColors := wapi_GetDeviceCaps(::hPrinterDC, WIN_NUMCOLORS)

         // Set the standard font
         ::SetDefaultFont()
         ::PageNumber := 0
         ::HavePrinted := ::Printing := ::PageInit := .F.
         ::fOldFormType     := ::FormType  // Last form type used
         ::fOldLandScape    := ::LandScape
         ::fOldBinNumber    := ::BinNumber
         ::fNewDuplexType   := ::fDuplexType
         ::fNewPrintQuality := ::fPrintQuality
         ::fOldPaperLength  := ::PaperLength
         ::fOldPaperWidth   := ::PaperWidth
      ELSE
         ::hPrinterDC := NIL
      ENDIF
   ENDIF

   RETURN lResult

METHOD win_Prn:Destroy()

   IF !Empty(::hPrinterDc)
      IF ::Printing
         ::EndDoc()
      ENDIF
      ::hPrinterDC := NIL
   ENDIF

   RETURN .T.

METHOD PROCEDURE win_Prn:Destruct()

   ::Destroy()

   RETURN

METHOD win_Prn:StartDoc(cDocName)

   LOCAL lResult

   IF !Empty(::hPrinterDc)

      IF !HB_IsString(cDocName)
         cDocName := hb_ProgName() + " [" + hb_TToC(hb_DateTime(), "yyyy-mm-dd", "hh:mm:ss") + "]"
      ENDIF

      IF (lResult := (wapi_StartDoc(::hPrinterDc, {"lpszDocName" => cDocName}) > 0))
         IF (lResult := ::StartPage(::hPrinterDc))
            ::Printing := .T.
         ELSE
            ::EndDoc(.T.)
         ENDIF
      ENDIF
   ELSE
      lResult := .F.
   ENDIF

   RETURN lResult

METHOD win_Prn:EndDoc(lAbortDoc)

   IF !Empty(::hPrinterDc)
      IF !::HavePrinted .OR. hb_defaultValue(lAbortDoc, .F.)
         wapi_AbortDoc(::hPrinterDC)
      ELSE
         ::EndPage(.F.)
         wapi_EndDoc(::hPrinterDC)
      ENDIF
      ::HavePrinted := ::Printing := ::PageInit := .F.
      ::PageNumber := 0
   ENDIF

   RETURN .T.

METHOD win_Prn:StartPage()

   LOCAL lLLandScape
   LOCAL nLBinNumber
   LOCAL nLFormType
   LOCAL nLDuplexType
   LOCAL nLPrintQuality
   LOCAL nLPaperLength
   LOCAL nLPaperWidth
   LOCAL lChangeDP

   IF !Empty(::hPrinterDc)

      lChangeDP := .F.
      IF ::LandScape != ::fOldLandScape  // Direct-modify property
         lLLandScape := ::fOldLandScape := ::LandScape
         lChangeDP := .T.
      ENDIF
      IF ::BinNumber != ::fOldBinNumber  // Direct-modify property
         nLBinNumber := ::fOldBinNumber := ::BinNumber
         lChangeDP := .T.
      ENDIF
      IF ::FormType != ::fOldFormType  // Direct-modify property
         nLFormType := ::fOldFormType := ::FormType
         lChangeDP := .T.
      ENDIF
      IF ::fDuplexType != ::fNewDuplexType  // Get/Set property
         nLDuplexType := ::fDuplexType := ::fNewDuplexType
         lChangeDP := .T.
      ENDIF
      IF ::fPrintQuality != ::fNewPrintQuality  // Get/Set property
         nLPrintQuality := ::fPrintQuality := ::fNewPrintQuality
         lChangeDP := .T.
      ENDIF
      IF ::fOldPaperLength != ::PaperLength .OR. ; // Get/Set property
         ::fOldPaperWidth != ::PaperWidth          // Get/Set property
         nLFormType := ::FormType
         nLPaperLength := ::fOldPaperLength := ::PaperLength
         nLPaperWidth := ::fOldPaperWidth := ::PaperWidth
         lChangeDP := .T.
      ENDIF
      IF ::fOldPaperWidth != ::PaperWidth  // Get/Set property
         lChangeDP := .T.
      ENDIF
      IF lChangeDP
         win_SetDocumentProperties(::hPrinterDC, ::PrinterName, nLFormType, lLLandscape, , nLBinNumber, nLDuplexType, nLPrintQuality, nLPaperLength, nLPaperWidth)
      ENDIF
      wapi_StartPage(::hPrinterDC)
      ::PageNumber++
      ::PageInit := .F.
      ::PosX := ::LeftMargin
      ::PosY := ::TopMargin
   ENDIF

   RETURN .T.

METHOD win_Prn:CheckPage()

   IF !Empty(::hPrinterDc) .AND. ::PageInit

      ::PageInit := .F.
      wapi_StartPage(::hPrinterDC)
      ::PageNumber++
      IF hb_osIsWin9x()  // Reset font on Win9x
         ::SetFont()
      ENDIF
   ENDIF

   RETURN ::Printing

METHOD win_Prn:EndPage(lStartNewPage)

   IF !Empty(::hPrinterDc)
      wapi_EndPage(::hPrinterDC)
      IF hb_defaultValue(lStartNewPage, .T.)
         IF ::PageInit
            ::PosX := ::LeftMargin
            ::PosY := ::TopMargin
         ELSE
            ::StartPage()
            IF hb_osIsWin9x()  // Reset font on Win9x
               ::SetFont()
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN .T.

METHOD win_Prn:NewLine()

   ::PosX := ::LeftMargin
   ::PosY += ::LineHeight

   RETURN ::PosY

METHOD win_Prn:NewPage(lDelay)

   IF ::Printing
      IF hb_defaultValue(lDelay, .F.)
         ::PageInit := .T.
      ENDIF
      ::EndPage(.T.)
   ENDIF

   RETURN .T.

METHOD win_Prn:GetDocumentProperties()
   RETURN win_GetDocumentProperties(::PrinterName, @::FormType, @::Landscape, @::Copies, @::BinNumber, @::fDuplexType, @::fPrintQuality, ;
      @::PaperLength, @::PaperWidth, @::Collate)

// If font width is specified it is in "characters per inch" to emulate DotMatrix
// An array { nMul, nDiv } is used to get precise size such a the Dot Matrix equivalent
// of Compressed print == 16.67 char per inch == { 3,-50 }
// If nDiv is < 0 then Fixed width printing is forced via ExtTextOut()
METHOD win_Prn:SetFont(cFontName, nPointSize, xWidth, nBold, lUnderline, lItalic, nCharSet, lManualSize)

   IF HB_IsString(cFontName)
      ::FontName := cFontName
   ENDIF
   IF HB_IsNumeric(nPointSize)
      ::FontPointSize := nPointSize
   ENDIF
   DO CASE
   CASE HB_IsArray(xWidth) .AND. Len(xWidth) >= 2 .AND. HB_IsNumeric(xWidth[1]) .AND. HB_IsNumeric(xWidth[2])
      ::FontWidth := xWidth
   CASE HB_IsNumeric(xWidth) .AND. xWidth != 0
      ::FontWidth := {1, xWidth}
   CASE xWidth != NIL
      ::FontWidth := {0, 0}
   ENDCASE
   IF HB_IsNumeric(nBold)
      ::fBold := nBold
   ENDIF
   IF HB_IsLogical(lUnderLine)
      ::fUnderline := lUnderLine
   ENDIF
   IF HB_IsLogical(lItalic)
      ::fItalic := lItalic
   ENDIF
   IF HB_IsNumeric(nCharSet)
      ::fCharSet := nCharSet
   ENDIF
   IF (::SetFontOk := !Empty(::hFont := __win_CreateFont(::hPrinterDC, ::FontName, ::FontPointSize, ::FontWidth[1], ::FontWidth[2], ::fBold, ::fUnderLine, ::fItalic, ::fCharSet, lManualSize)))
      ::fCharWidth  := ::GetCharWidth()
      ::CharWidth   := Abs(::fCharWidth)
      ::CharHeight  := ::GetCharHeight()
   ENDIF
   IF !Empty(::hPrinterDC)
      ::FontName := wapi_GetTextFace(::hPrinterDC)  // Get the font name that Windows actually used
   ENDIF

   RETURN ::SetFontOk

METHOD win_Prn:GetCharWidth()

   LOCAL nWidth

   IF ::FontWidth[2] < 0 .AND. ::FontWidth[1] != 0
      nWidth := wapi_MulDiv(::FontWidth[1], ::PixelsPerInchX, ::FontWidth[2])
   ELSE
      nWidth := win_GetCharSize(::hPrinterDC)
   ENDIF

   RETURN nWidth

METHOD win_Prn:GetCharHeight()
   RETURN win_GetCharSize(::hPrinterDC, .T.)

METHOD win_Prn:SetDefaultFont()
   RETURN ::SetFont("Courier New", 12, {1, 10}, 0, .F., .F., 0)

METHOD win_Prn:Bold(nWeight)

   LOCAL nOldValue := ::fBold

   IF HB_IsNumeric(nWeight)
      ::fBold := nWeight
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF

   RETURN nOldValue

METHOD win_Prn:Underline(lUnderLine)

   LOCAL lOldValue := ::fUnderline

   IF HB_IsLogical(lUnderLine)
      ::fUnderLine := lUnderLine
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF

   RETURN lOldValue

METHOD win_Prn:Italic(lItalic)

   LOCAL lOldValue := ::fItalic

   IF HB_IsLogical(lItalic)
      ::fItalic := lItalic
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF

   RETURN lOldValue

METHOD win_Prn:CharSet(nCharSet)

   LOCAL nOldValue := ::fCharSet

   IF HB_IsNumeric(nCharSet)
      ::fCharSet := nCharSet
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF

   RETURN nOldValue

METHOD win_Prn:SetDuplexType(nDuplexType)

   LOCAL nOldValue := ::fDuplexType

   IF HB_IsNumeric(nDuplexType)
      ::fNewDuplexType := nDuplexType
      IF !::Printing
         ::fDuplexType := nDuplexType
      ENDIF
   ENDIF

   RETURN nOldValue

METHOD win_Prn:SetPrintQuality(nPrintQuality)

   LOCAL nOldValue := ::fPrintQuality

   IF HB_IsNumeric(nPrintQuality)
      ::fNewPrintQuality := nPrintQuality
      IF !::Printing
         ::fPrintQuality := nPrintQuality
      ENDIF
   ENDIF

   RETURN nOldValue

METHOD win_Prn:GetFonts()
   RETURN win_EnumFonts(::hPrinterDC)

METHOD win_Prn:SetPos(nPosX, nPosY)

   LOCAL aOldValue := {::PosX, ::PosY}

   IF HB_IsNumeric(nPosX)
      ::PosX := Int(nPosX)
   ENDIF
   IF HB_IsNumeric(nPosY)
      ::PosY := Int(nPosY)
   ENDIF

   RETURN aOldValue

METHOD win_Prn:SetColor(nClrText, nClrPane, nAlign)

   LOCAL nOldColor

   IF !Empty(::hPrinterDc)
      IF HB_IsNumeric(nClrText)
         nOldColor := wapi_SetTextColor(::hPrinterDC, ::TextColor := nClrText)
      ELSE
         nOldColor := wapi_GetTextColor(::hPrinterDC)
      ENDIF
      IF HB_IsNumeric(nClrPane)
         wapi_SetBkColor(::hPrinterDC, ::BkColor := nClrPane)
      ENDIF
      IF HB_IsNumeric(nAlign)
         wapi_SetTextAlign(::hPrinterDC, ::TextAlign := nAlign)
      ENDIF
   ELSE
      nOldColor := WIN_CLR_INVALID
   ENDIF

   RETURN nOldColor

METHOD win_Prn:SetBkMode(nMode)

   IF !Empty(::hPrinterDc)
      IF HB_IsNumeric(nMode)
         ::BkMode := nMode
         RETURN wapi_SetBkMode(::hPrinterDc, nMode)
      ELSE
         RETURN wapi_GetBkMode(::hPrinterDc)
      ENDIF
   ENDIF

   RETURN 0

METHOD win_Prn:TextOut(cString, lNewLine, lUpdatePosX, nAlign)

   LOCAL lResult := .F.
   LOCAL size
   LOCAL nPosX

   IF !Empty(::hPrinterDc) .AND. HB_IsString(cString) .AND. !cString == "" .AND. ::CheckPage()

      wapi_SetTextAlign(::hPrinterDC, hb_bitOr(WIN_TA_NOUPDATECP, hb_defaultValue(nAlign, hb_bitOr(WIN_TA_BOTTOM, WIN_TA_LEFT))))

      nPosX := 0
      IF ::fCharWidth < 0
         IF wapi_ExtTextOut(::hPrinterDC, ::PosX, ::PosY,,, cString, AFill(Array(hb_ULen(cString)), -::fCharWidth))
            nPosX := hb_ULen(cString) * -::fCharWidth
         ENDIF
      ELSEIF wapi_ExtTextOut(::hPrinterDC, ::PosX, ::PosY,,, cString)
         size := {=>}
         wapi_GetTextExtentPoint32(::hPrinterDC, cString, size)  /* Get the length of the text in device size */
         nPosX := size["cx"]  /* return the width so we can update the current pen position (::PosY) */
      ENDIF

      ::HavePrinted := lResult := .T.

      IF hb_defaultValue(lUpdatePosX, .T.)
         ::PosX += nPosX
      ENDIF
      IF hb_defaultValue(lNewLine, .F.)
         ::NewLine()
      ENDIF
   ENDIF

   RETURN lResult

METHOD win_Prn:TextOutAt(nPosX, nPosY, cString, lNewLine, lUpdatePosX, nAlign)

   ::SetPos(nPosX, nPosY)

   RETURN ::TextOut(cString, lNewLine, lUpdatePosX, nAlign)

METHOD win_Prn:TextAtFont(nPosX, nPosY, cString, cFont, nPointSize, nWidth, nBold, lUnderLine, lItalic, nCharSet, lNewLine, lUpdatePosX, nColor, nAlign)

   LOCAL lResult
   LOCAL nDiv := 0
   LOCAL hFont

   IF !Empty(::hPrinterDc) .AND. ::CheckPage()

      IF HB_IsString(cFont)
         DO CASE
         CASE HB_IsArray(nWidth)
            nDiv   := nWidth[1]
            nWidth := nWidth[2]
         CASE HB_IsNumeric(nWidth) .AND. nWidth != 0
            nDiv := 1
         ENDCASE
         hFont := ::hFont
         ::hFont := __win_CreateFont(::hPrinterDC, cFont, hb_defaultValue(nPointSize, ::FontPointSize), nDiv, nWidth, nBold, lUnderLine, lItalic, nCharSet)
      ENDIF
      IF HB_IsNumeric(nColor)
         nColor := wapi_SetTextColor(::hPrinterDC, nColor)
      ENDIF

      lResult := ::TextOutAt(nPosX, nPosY, cString, lNewLine, lUpdatePosX, nAlign)

      IF HB_IsString(cFont)
         ::hFont := hFont  // Reset Font
      ENDIF
      IF HB_IsNumeric(nColor)
         wapi_SetTextColor(::hPrinterDC, nColor)  // Reset Color
      ENDIF
   ELSE
      lResult := .F.
   ENDIF

   RETURN lResult

METHOD win_Prn:SetPen(nStyle, nWidth, nColor)

   ::PenStyle := nStyle
   ::PenWidth := nWidth
   ::PenColor := nColor

   RETURN !Empty(::hPen := win_SetPen(::hPrinterDC, nStyle, nWidth, nColor))

METHOD win_Prn:Line(nX1, nY1, nX2, nY2)

   LOCAL lResult := .F.

   IF !Empty(::hPrinterDc) .AND. ::CheckPage()

      IF (lResult := (wapi_MoveToEx(::hPrinterDC, nX1, nY1) .AND. wapi_LineTo(::hPrinterDC, nX2, nY2)))
         ::HavePrinted := .T.
      ENDIF
   ENDIF

   RETURN lResult

METHOD win_Prn:Box(nX1, nY1, nX2, nY2, nWidth, nHeight)

   LOCAL lResult := .F.

   IF !Empty(::hPrinterDc) .AND. ::CheckPage()

      hb_default(@nWidth, 0)
      hb_default(@nHeight, 0)

      IF nWidth != 0 .AND. nHeight != 0
         lResult := wapi_RoundRect(::hPrinterDC, nX1, nY1, nX2, nY2, nWidth, nHeight)
      ELSE
         lResult := wapi_Rectangle(::hPrinterDC, nX1, nY1, nX2, nY2)
      ENDIF

      IF lResult
         ::HavePrinted := .T.
      ENDIF
   ENDIF

   RETURN lResult

METHOD win_Prn:Arc(nX1, nY1, nX2, nY2)

   LOCAL lResult := .F.

   IF !Empty(::hPrinterDc) .AND. ::CheckPage() .AND. (lResult := wapi_Arc(::hPrinterDC, nX1, nY1, nX2, nY2))
      ::HavePrinted := .T.
   ENDIF

   RETURN lResult

METHOD win_Prn:Ellipse(nX1, nY1, nX2, nY2)

   LOCAL lResult := .F.

   IF !Empty(::hPrinterDc) .AND. ::CheckPage() .AND. (lResult := wapi_Ellipse(::hPrinterDC, nX1, nY1, nX2, nY2))
      ::HavePrinted := .T.
   ENDIF

   RETURN lResult

METHOD win_Prn:FillRect(nX1, nY1, nX2, nY2, nColor)

   LOCAL lResult := .F.

   IF !Empty(::hPrinterDc) .AND. ::CheckPage() .AND. (lResult := (wapi_FillRect(::hPrinterDC, {nX1, nY1, nX2, nY2}, wapi_CreateSolidBrush(nColor)) != 0))
      ::HavePrinted := .T.
   ENDIF

   RETURN lResult

METHOD win_Prn:GetTextWidth(cString)

   LOCAL size

   IF ::FontWidth[2] < 0 .AND. ::FontWidth[1] != 0
      RETURN hb_ULen(cString) * ::CharWidth
   ELSEIF !Empty(::hPrinterDc)
      size := {=>}
      wapi_GetTextExtentPoint32(::hPrinterDC, cString, size)
      RETURN size["cx"]  // Return width in device units
   ENDIF

   RETURN 0

METHOD win_Prn:GetTextHeight(cString)

   LOCAL size

   IF !Empty(::hPrinterDc)
      size := {=>}
      wapi_GetTextExtentPoint32(::hPrinterDC, cString, size)
      RETURN size["cy"]  // Return height in device units
   ENDIF

   RETURN 0

METHOD win_Prn:DrawBitmap(oBmp)

   LOCAL lResult := .F.

   IF ::BitMapsOk .AND. ::CheckPage() .AND. !Empty(oBmp:BitMap) .AND. ;
      (lResult := win_DrawBitmap(::hPrinterDc, oBmp:BitMap, oBmp:Rect[1], oBmp:Rect[2], oBmp:Rect[3], oBmp:Rect[4], oBmp:DimXY[1], oBmp:DimXY[2]))
      ::HavePrinted := .T.
   ENDIF

   RETURN lResult

METHOD PROCEDURE win_Prn:SetPRC(nRow, nCol)

   ::SetPos((nCol * ::CharWidth) + ::LeftMArgin, (nRow * ::LineHeight) + ::TopMargin)

   RETURN

METHOD win_Prn:PRow()
   RETURN Int((::PosY - ::TopMargin) / ::LineHeight)  // No test for division by zero

METHOD win_Prn:PCol()
   RETURN Int((::PosX - ::LeftMargin) / ::CharWidth)  // Uses width of current character

METHOD win_Prn:MaxRow()
   RETURN Int(((::BottomMargin - ::TopMargin) + 1) / ::LineHeight ) - 1

METHOD win_Prn:MaxCol()
   RETURN Int(((::RightMargin - ::LeftMargin) + 1) / ::CharWidth ) - 1

METHOD win_Prn:MM_To_PosX(nMm)
   RETURN Int(((nMm * ::PixelsPerInchX) / WIN_MM_TO_INCH) - ::LeftMargin)

METHOD win_Prn:MM_To_PosY(nMm)
   RETURN Int(((nMm * ::PixelsPerInchY) / WIN_MM_TO_INCH) - ::TopMargin)

METHOD win_Prn:Inch_To_PosX(nInch)
   RETURN Int((nInch * ::PixelsPerInchX) - ::LeftMargin)

METHOD win_Prn:Inch_To_PosY(nInch)
   RETURN Int((nInch * ::PixelsPerInchY) - ::TopMargin)

METHOD win_Prn:GetDeviceCaps(nCaps)
   RETURN IIf(Empty(::hPrinterDc), 0, wapi_GetDeviceCaps(::hPrinterDC, nCaps))

STATIC FUNCTION __win_CreateFont(hDC, cName, nHeight, nMul, nWidth, nWeight, lUnderline, lItalic, nCharSet, lManualSize)

   LOCAL hFont

   IF !Empty(hDC)

      nWeight := hb_defaultValue(nWeight, 0)
      IF nWeight <= 0
         nWeight := WIN_FW_NORMAL
      ENDIF

      IF !hb_defaultValue(lManualSize, .F.)  /* Ugly hack to enable full control for caller */
         nHeight := -wapi_MulDiv(nHeight, wapi_GetDeviceCaps(hDC, WIN_LOGPIXELSY), 72)

         IF nWidth != 0
            nWidth := wapi_MulDiv(Abs(nMul), wapi_GetDeviceCaps(hDC, WIN_LOGPIXELSX), Abs(nWidth))
         ENDIF
      ENDIF

      hFont := wapi_CreateFont(nHeight, nWidth, 0, 0, nWeight, lItalic, lUnderline, .F., nCharSet, ;
         IIf(hb_Version(HB_VERSION_PLATFORM) == "WCE",, WIN_OUT_DEVICE_PRECIS ),, WIN_DRAFT_QUALITY,, cName)

      IF !Empty(hFont)
         wapi_SelectObject(hDC, hFont)
      ENDIF
   ENDIF

   RETURN hFont

#if 0
STATIC FUNCTION win_GetCharSize(hDC, lHeight)

   LOCAL tm

   IF Empty(hDC)
      RETURN 0
   ENDIF

   tm := {=>}
   wapi_GetTextMetrics(hDC, tm)

   RETURN tm[IIf(hb_defaultValue(lHeight, .F.), "tmHeight", "tmAveCharWidth")]
#endif
