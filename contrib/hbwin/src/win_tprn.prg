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

// win_Prn() was designed to make it easy to emulate Clipper Dot Matrix printing.
// Dot Matrix printing was in CPI ( Characters per inch and Lines per inch ).
// Even though "Mapping Mode" for win_Prn() is WIN_MM_TEXT, ::SetFont() accepts the
// xWidth parameter in CPI not Pixels. Also the default ::LineHeight is for
// 6 lines per inch so ::NewLine() works as per "LineFeed" on Dot Matrix printers.
// If you do not like this then inherit from the class and override anything you want
//
// Simple example
//
// TODO: Color printing
//       etc....
//
// Peter Rees 2004-01-21 <peter@rees.co.nz>

#include <hbclass.ch>

#include "hbwin.ch"

#define MM_TO_INCH                  25.4

CREATE CLASS win_Prn

   METHOD New(cPrinter)
   METHOD Create()                  // CreatesDC and sets "Courier New" font, set Orientation, Copies, Bin#
                                    // Create() ( and StartDoc() ) must be called before printing can start.
   METHOD Destroy()                 // Calls EndDoc() - restores default font, Deletes DC.
   DESTRUCTOR Destruct()

   METHOD StartDoc(cDocName)      // Calls StartPage()
   METHOD EndDoc(lAbortDoc)       // Calls EndPage() if lAbortDoc not .T.
   METHOD StartPage()
   METHOD EndPage(lStartNewPage)  // If lStartNewPage == .T. then StartPage() is called for the next page of output
   METHOD NewLine()
   METHOD NewPage(lDelay)         // If lDelay == .T. then new page is not created immediately but just before 1st output
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
   METHOD SetDuplexType(nDuplexType)                // Get/Set current duplex mode
   METHOD SetPrintQuality(nPrintQuality)            // Get/Set print quality
   METHOD CharSet(nCharSet)


   METHOD SetPos(nPosX, nPosY)                      // WARNING: ( Col, Row ) _NOT_ ( Row, Col )
   METHOD SetColor(nClrText, nClrPane, nAlign)
   METHOD SetBkMode(nMode)                          // Set background mode: WIN_TRANSPARENT or WIN_OPAQUE

   METHOD TextOut(cString, lNewLine, lUpdatePosX, nAlign)  // nAlign : WIN_TA_*
   METHOD TextOutAt(nPosX, nPosY, cString, lNewLine, lUpdatePosX, nAlign)   // WARNING: ( Col, Row ) _NOT_ ( Row, Col )


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

   // Clipper compatible functions.
   METHOD SetPRC(nRow, nCol)   // Based on ::LineHeight and current ::CharWidth
   METHOD PRow()
   METHOD PCol()
   METHOD MaxRow()               // Based on ::LineHeight and form dimensions
   METHOD MaxCol()               // Based on ::CharWidth and form dimensions

   METHOD MM_TO_POSX(nMm)      // Convert position on page from MM to pixel location Column
   METHOD MM_TO_POSY(nMm)      //   "       "      "    "    "   "  "   "      "     Row
   METHOD INCH_TO_POSX(nInch)  // Convert position on page from INCH to pixel location Column
   METHOD INCH_TO_POSY(nInch)  //   "       "      "    "    "   "    "   "       "    Row

   METHOD TextAtFont(nPosX, nPosY, cString, cFont, nPointSize, ;     // Print text string at location
                     nWidth, nBold, lUnderLine, lItalic, nCharSet, ; // in specified font and color.
                     lNewLine, lUpdatePosX, nColor, nAlign)         // Restore original font and color
                                                                      // after printing.

   METHOD GetDeviceCaps(nCaps)

   VAR PrinterName      INIT ""
   VAR Printing         INIT .F.
   VAR HavePrinted      INIT .F.
   VAR PageInit         INIT .F.
   VAR PageNumber       INIT 0
   VAR hPrinterDc       INIT 0

   VAR AskProperties    INIT .F.

   // These next 6 variables must be set before calling ::Create() if
   // you wish to alter the defaults
   VAR FormType         INIT 0
   VAR BinNumber        INIT 0
   VAR Landscape        INIT .F.
   VAR Copies           INIT 1
   VAR PaperLength      INIT 0                           // Value is * 1/10 of mm   1000 == 10cm
   VAR PaperWidth       INIT 0                           //   "    "    "     "       "     "

   VAR SetFontOk        INIT .F.
   VAR hFont            INIT 0
   VAR FontName         INIT ""                          // Current Point size for font
   VAR FontPointSize    INIT 12                          // Point size for font
   VAR FontWidth        INIT { 0, 0 }                    // { Mul, Div } Calc width: nWidth := wapi_MulDiv( nMul, win_GetDeviceCaps( HDC, WIN_LOGPIXELSX ), nDiv )
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

   VAR hPen             INIT 0
   VAR PenStyle
   VAR PenWidth
   VAR PenColor

ENDCLASS

METHOD win_Prn:New(cPrinter)

   ::PrinterName := IIf(Empty(cPrinter), win_printerGetDefault(), cPrinter)
   // Initialized with the current properties of the printer [jarabal]
   ::GetDocumentProperties()

   RETURN Self

METHOD win_Prn:Create()

   LOCAL lResult := .F.

   ::Destroy()                            // Finish current print job if any
   IF !Empty(::hPrinterDC := win_CreateDC(::PrinterName))

      // Set Form Type
      // Set Number of Copies
      // Set Orientation
      // Set Duplex mode
      // Set PrintQuality

      IF ::AskProperties
         lResult := win_SetDocumentProperties(::hPrinterDC, ::PrinterName, ;
            @::FormType, @::Landscape, ;
            @::Copies, @::BinNumber, ;
            @::fDuplexType, @::fPrintQuality, ;
            @::PaperLength, @::PaperWidth)
      ELSE
         lResult := win_SetDocumentProperties(::hPrinterDC, ::PrinterName, ;
            ::FormType, ::Landscape, ;
            ::Copies, ::BinNumber, ;
            ::fDuplexType, ::fPrintQuality, ;
            ::PaperLength, ::PaperWidth)
      ENDIF

      IF !lResult
         ::hPrinterDC := NIL
      ELSE
         IF ::BkMode != NIL
            win_SetBkMode(::hPrinterDc, ::BkMode)
         ENDIF
         // Set mapping mode to pixels, topleft down
         win_SetMapMode(::hPrinterDC, WIN_MM_TEXT)
//       win_SetTextCharacterExtra(::hPrinterDC, 0) // do not add extra char spacing even if bold
         // Get Margins etc... here
         ::PageWidth        := win_GetDeviceCaps(::hPrinterDC, WIN_PHYSICALWIDTH)
         ::PageHeight       := win_GetDeviceCaps(::hPrinterDC, WIN_PHYSICALHEIGHT)
         ::LeftMargin       := win_GetDeviceCaps(::hPrinterDC, WIN_PHYSICALOFFSETX)
         ::RightMargin      := (::PageWidth - ::LeftMargin) + 1
         ::PixelsPerInchY   := win_GetDeviceCaps(::hPrinterDC, WIN_LOGPIXELSY)
         ::PixelsPerInchX   := win_GetDeviceCaps(::hPrinterDC, WIN_LOGPIXELSX)
         ::LineHeight       := Int(::PixelsPerInchY / 6)  // Default 6 lines per inch == # of pixels per line
         ::TopMargin        := win_GetDeviceCaps(::hPrinterDC, WIN_PHYSICALOFFSETY)
         ::BottomMargin     := (::PageHeight - ::TopMargin) + 1

         // Set .T. if can print bitmaps
         ::BitMapsOk := win_BitmapsOK(::hPrinterDC)

         // supports Colour
         ::NumColors := win_GetDeviceCaps(::hPrinterDC, WIN_NUMCOLORS)

         // Set the standard font
         ::SetDefaultFont()
         ::PageNumber := 0
         ::HavePrinted := ::Printing := ::PageInit := .F.
         ::fOldFormType     := ::FormType  // Last formtype used
         ::fOldLandScape    := ::LandScape
         ::fOldBinNumber    := ::BinNumber
         ::fNewDuplexType   := ::fDuplexType
         ::fNewPrintQuality := ::fPrintQuality
         ::fOldPaperLength  := ::PaperLength
         ::fOldPaperWidth   := ::PaperWidth
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

   IF !hb_IsString(cDocName)
      cDocName := hb_argv(0) + " [" + DToC(Date()) + " - " + Time() + "]"
   ENDIF

   IF (lResult := win_StartDoc(::hPrinterDc, cDocName))
      IF !(lResult := ::StartPage(::hPrinterDc))
         ::EndDoc(.T.)
      ELSE
         ::Printing := .T.
      ENDIF
   ENDIF

   RETURN lResult

METHOD win_Prn:EndDoc(lAbortDoc)

   IF ::HavePrinted
      hb_default(@lAbortDoc, .F.)
   ELSE
      lAbortDoc := .T.
   ENDIF
   IF lAbortDoc
      win_AbortDoc(::hPrinterDC)
   ELSE
      ::EndPage(.F.)
      win_EndDoc(::hPrinterDC)
   ENDIF
   ::HavePrinted := ::Printing := ::PageInit := .F.
   ::PageNumber := 0

   RETURN .T.

METHOD win_Prn:StartPage()

   LOCAL lLLandScape
   LOCAL nLBinNumber
   LOCAL nLFormType
   LOCAL nLDuplexType
   LOCAL nLPrintQuality
   LOCAL nLPaperLength
   LOCAL nLPaperWidth
   LOCAL lChangeDP := .F.

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
      win_SetDocumentProperties(::hPrinterDC, ::PrinterName, ;
         nLFormType, lLLandscape, , ;
         nLBinNumber, nLDuplexType, nLPrintQuality, ;
         nLPaperLength, nLPaperWidth)
   ENDIF
   win_StartPage(::hPrinterDC)
   ::PageNumber++
   ::PageInit := .F.
   ::PosX := ::LeftMargin
   ::PosY := ::TopMargin

   RETURN .T.

METHOD win_Prn:CheckPage()

   IF ::PageInit
      ::PageInit := .F.
      win_StartPage(::hPrinterDC)
      ::PageNumber++
      IF hb_osIsWin9x() // Reset font on Win9X
         ::SetFont()
      ENDIF
   ENDIF

   RETURN ::Printing

METHOD win_Prn:EndPage(lStartNewPage)

   hb_default(@lStartNewPage, .T.)

   win_EndPage(::hPrinterDC)
   IF lStartNewPage
      IF ::PageInit
         ::PosX := ::LeftMargin
         ::PosY := ::TopMargin
      ELSE
         ::StartPage()
         IF hb_osIsWin9x() // Reset font on Win9X
            ::SetFont()
         ENDIF
      ENDIF
   ENDIF

   RETURN .T.

METHOD win_Prn:NewLine()

   ::PosX := ::LeftMargin
   ::PosY += ::LineHeight

   RETURN ::PosY

METHOD win_Prn:NewPage(lDelay)

   hb_default(@lDelay, .F.)

   IF ::Printing
      IF lDelay
         ::PageInit := .T.
      ENDIF
      ::EndPage(.T.)
   ENDIF

   RETURN .T.

METHOD win_Prn:GetDocumentProperties()
   RETURN win_GetDocumentProperties(::PrinterName, ;
      @::FormType, @::Landscape, ;
      @::Copies, @::BinNumber, ;
      @::fDuplexType, @::fPrintQuality, ;
      @::PaperLength, @::PaperWidth)

// If font width is specified it is in "characters per inch" to emulate DotMatrix
// An array {nMul,nDiv} is used to get precise size such a the Dot Matric equivalent
// of Compressed print == 16.67 char per inch == { 3,-50 }
// If nDiv is < 0 then Fixed width printing is forced via ExtTextOut()
METHOD win_Prn:SetFont(cFontName, nPointSize, xWidth, nBold, lUnderline, lItalic, nCharSet, lManualSize)

   LOCAL cType

   IF cFontName != NIL
      ::FontName := cFontName
   ENDIF
   IF nPointSize != NIL
      ::FontPointSize := nPointSize
   ENDIF
   IF xWidth != NIL
      cType := ValType(xWidth)
      IF cType == "A"
         ::FontWidth := xWidth
      ELSEIF cType == "N" .AND. !Empty(xWidth)
         ::FontWidth := { 1, xWidth }
      ELSE
         ::FontWidth := { 0, 0 }
      ENDIF
   ENDIF
   IF nBold != NIL
      ::fBold := nBold
   ENDIF
   IF lUnderLine != NIL
      ::fUnderline := lUnderLine
   ENDIF
   IF lItalic != NIL
      ::fItalic := lItalic
   ENDIF
   IF nCharSet != NIL
      ::fCharSet := nCharSet
   ENDIF
   IF (::SetFontOk := !Empty(::hFont := win_CreateFont(::hPrinterDC, ::FontName, ::FontPointSize, ::FontWidth[1], ::FontWidth[2], ::fBold, ::fUnderLine, ::fItalic, ::fCharSet, lManualSize)))
      ::fCharWidth  := ::GetCharWidth()
      ::CharWidth   := Abs(::fCharWidth)
      ::CharHeight  := ::GetCharHeight()
   ENDIF
   ::FontName := win_GetPrinterFontName(::hPrinterDC)  // Get the font name that Windows actually used

   RETURN ::SetFontOk

METHOD win_Prn:GetCharWidth()

   LOCAL nWidth

   IF ::FontWidth[2] < 0 .AND. !Empty(::FontWidth[1])
      nWidth := wapi_MulDiv(::FontWidth[1], ::PixelsPerInchX, ::FontWidth[2])
   ELSE
      nWidth := win_GetCharSize(::hPrinterDC)
   ENDIF

   RETURN nWidth

METHOD win_Prn:GetCharHeight()
   RETURN win_GetCharSize(::hPrinterDC, .T.)

METHOD win_Prn:SetDefaultFont()
   RETURN ::SetFont("Courier New", 12, { 1, 10 }, 0, .F., .F., 0)

METHOD win_Prn:Bold(nWeight)

   LOCAL nOldValue := ::fBold

   IF nWeight != NIL
      ::fBold := nWeight
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF

   RETURN nOldValue

METHOD win_Prn:Underline(lUnderLine)

   LOCAL lOldValue := ::fUnderline

   IF lUnderLine != NIL
      ::fUnderLine := lUnderLine
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF

   RETURN lOldValue

METHOD win_Prn:Italic(lItalic)

   LOCAL lOldValue := ::fItalic

   IF lItalic != NIL
      ::fItalic := lItalic
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF

   RETURN lOldValue

METHOD win_Prn:CharSet(nCharSet)

   LOCAL nOldValue := ::fCharSet

   IF nCharSet != NIL
      ::fCharSet := nCharSet
      IF ::Printing
         ::SetFont()
      ENDIF
   ENDIF

   RETURN nOldValue

METHOD win_Prn:SetDuplexType(nDuplexType)

   LOCAL nOldValue := ::fDuplexType

   IF nDuplexType != NIL
      ::fNewDuplexType := nDuplexType
      IF !::Printing
         ::fDuplexType := nDuplexType
      ENDIF
   ENDIF

   RETURN nOldValue

METHOD win_Prn:SetPrintQuality(nPrintQuality)

   LOCAL nOldValue := ::fPrintQuality

   IF nPrintQuality != NIL
      ::fNewPrintQuality := nPrintQuality
      IF !::Printing
         ::fPrintQuality := nPrintQuality
      ENDIF
   ENDIF

   RETURN nOldValue

METHOD win_Prn:GetFonts()
   RETURN win_EnumFonts(::hPrinterDC)

METHOD win_Prn:SetPos(nPosX, nPosY)

   LOCAL aOldValue := { ::PosX, ::PosY }

   IF nPosX != NIL
      ::PosX := Int(nPosX)
   ENDIF
   IF nPosY != NIL
      ::PosY := Int(nPosY)
   ENDIF

   RETURN aOldValue

METHOD win_Prn:SetColor(nClrText, nClrPane, nAlign)

   IF hb_IsNumeric(nClrText)
      ::TextColor := nClrText
   ENDIF
   IF hb_IsNumeric(nClrPane)
      ::BkColor := nClrPane
   ENDIF
   IF hb_IsNumeric(nAlign)
      ::TextAlign := nAlign
   ENDIF

   RETURN win_SetColor(::hPrinterDC, nClrText, nClrPane, nAlign)

METHOD win_Prn:SetBkMode(nMode)

   IF hb_IsNumeric(nMode)
      ::BkMode := nMode
   ENDIF

   RETURN win_SetBkMode(::hPrinterDc, nMode)

METHOD win_Prn:TextOut(cString, lNewLine, lUpdatePosX, nAlign)

   LOCAL lResult := .F.
   LOCAL nPosX

   IF cString != NIL .AND. ::CheckPage()

      hb_default(@lNewLine, .F.)
      hb_default(@lUpdatePosX, .T.)
      hb_default(@nAlign, hb_bitOr(WIN_TA_BOTTOM, WIN_TA_LEFT))

      nPosX := win_TextOut(::hPrinterDC, ::PosX, ::PosY, cString, Len(cString), ::fCharWidth, nAlign)

      ::HavePrinted := lResult := .T.

      IF lUpdatePosX
         ::PosX += nPosX
      ENDIF
      IF lNewLine
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
   LOCAL cType
   LOCAL hFont

   IF ::CheckPage()

      IF !hb_IsNumeric(nPointSize)
         nPointSize := ::FontPointSize
      ENDIF

      IF cFont != NIL
         cType := ValType(nWidth)
         IF cType == "A"
            nDiv   := nWidth[1]
            nWidth := nWidth[2]
         ELSEIF cType == "N" .AND. !Empty(nWidth)
            nDiv := 1
         ENDIF
         hFont := ::hFont
         ::hFont := win_CreateFont(::hPrinterDC, cFont, nPointSize, nDiv, nWidth, nBold, lUnderLine, lItalic, nCharSet)
      ENDIF
      IF nColor != NIL
         nColor := win_SetColor(::hPrinterDC, nColor)
      ENDIF

      lResult := ::TextOutAt(nPosX, nPosY, cString, lNewLine, lUpdatePosX, nAlign)

      IF cFont != NIL
         ::hFont := hFont     // Reset Font
      ENDIF
      IF nColor != NIL
         win_SetColor(::hPrinterDC, nColor)  // Reset Color
      ENDIF

   ENDIF

   RETURN lResult

METHOD win_Prn:SetPen(nStyle, nWidth, nColor)

   ::PenStyle := nStyle
   ::PenWidth := nWidth
   ::PenColor := nColor

   RETURN !Empty(::hPen := win_SetPen(::hPrinterDC, nStyle, nWidth, nColor))

METHOD win_Prn:Line(nX1, nY1, nX2, nY2)

   LOCAL lResult := .F.

   IF ::CheckPage()
      lResult := win_LineTo(::hPrinterDC, nX1, nY1, nX2, nY2)
      IF lResult
         ::HavePrinted := .T.
      ENDIF
   ENDIF

   RETURN lResult

METHOD win_Prn:Box(nX1, nY1, nX2, nY2, nWidth, nHeight)

   LOCAL lResult := .F.

   IF ::CheckPage()
      lResult := win_Rectangle(::hPrinterDC, nX1, nY1, nX2, nY2, nWidth, nHeight)
      IF lResult
         ::HavePrinted := .T.
      ENDIF
   ENDIF

   RETURN lResult

METHOD win_Prn:Arc(nX1, nY1, nX2, nY2)

   LOCAL lResult := .F.

   IF ::CheckPage()
      lResult := win_Arc(::hPrinterDC, nX1, nY1, nX2, nY2)
      IF lResult
         ::HavePrinted := .T.
      ENDIF
   ENDIF

   RETURN lResult

METHOD win_Prn:Ellipse(nX1, nY1, nX2, nY2)

   LOCAL lResult := .F.

   IF ::CheckPage()
      lResult := win_Ellipse(::hPrinterDC, nX1, nY1, nX2, nY2)
      IF lResult
         ::HavePrinted := .T.
      ENDIF
   ENDIF

   RETURN lResult

METHOD win_Prn:FillRect(nX1, nY1, nX2, nY2, nColor)

   LOCAL lResult := .F.

   IF ::CheckPage()
      lResult := win_FillRect(::hPrinterDC, nX1, nY1, nX2, nY2, nColor)
      IF lResult
         ::HavePrinted := .T.
      ENDIF
   ENDIF

   RETURN lResult

METHOD win_Prn:GetTextWidth(cString)

   LOCAL nWidth

   IF ::FontWidth[2] < 0 .AND. !Empty(::FontWidth[1])
      nWidth := Len(cString) * ::CharWidth
   ELSE
      nWidth := win_GetTextSize(::hPrinterDC, cString, Len(cString))  // Return Width in device units
   ENDIF

   RETURN nWidth

METHOD win_Prn:GetTextHeight(cString)
   RETURN win_GetTextSize(::hPrinterDC, cString, Len(cString), .F.)  // Return Height in device units

METHOD win_Prn:DrawBitmap(oBmp)

   LOCAL lResult := .F.

   IF ::BitMapsOk .AND. ::CheckPage() .AND. !Empty(oBmp:BitMap)
      IF (lResult := win_DrawBitmap(::hPrinterDc, oBmp:BitMap, oBmp:Rect[1], oBmp:Rect[2], oBmp:Rect[3], oBmp:Rect[4], oBmp:DimXY[1], oBmp:DimXY[2]))
         ::HavePrinted := .T.
      ENDIF
   ENDIF

   RETURN lResult

METHOD win_Prn:SetPRC(nRow, nCol)

   ::SetPos((nCol * ::CharWidth) + ::LeftMArgin, (nRow * ::LineHeight) + ::TopMargin)

   RETURN NIL

METHOD win_Prn:PRow()
   RETURN Int((::PosY - ::TopMargin) / ::LineHeight)   // No test for Div by ZERO

METHOD win_Prn:PCol()
   RETURN Int((::PosX - ::LeftMargin) / ::CharWidth)   // Uses width of current character

METHOD win_Prn:MaxRow()
   RETURN Int(((::BottomMargin - ::TopMargin) + 1) / ::LineHeight) - 1

METHOD win_Prn:MaxCol()
   RETURN Int(((::RightMargin - ::LeftMargin) + 1) / ::CharWidth) - 1

METHOD win_Prn:MM_To_PosX(nMm)
   RETURN Int(((nMM * ::PixelsPerInchX) / MM_TO_INCH) - ::LeftMargin)

METHOD win_Prn:MM_To_PosY(nMm)
   RETURN Int(((nMM * ::PixelsPerInchY) / MM_TO_INCH) - ::TopMargin)

METHOD win_Prn:Inch_To_PosX(nInch)
   RETURN Int((nInch * ::PixelsPerInchX) - ::LeftMargin)

METHOD win_Prn:Inch_To_PosY(nInch)
   RETURN Int((nInch * ::PixelsPerInchY) - ::TopMargin)

METHOD win_Prn:GetDeviceCaps(nCaps)
   RETURN win_GetDeviceCaps(::hPrinterDC, nCaps)
