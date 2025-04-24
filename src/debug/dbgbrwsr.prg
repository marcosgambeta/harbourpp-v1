//
// The Debugger Browser
//
// Copyright 2004 Ryszard Glab <rglab@imid.med.pl>
// Copyright 2007 Phil Krylov <phil a t newstar.rinet.ru>
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

#pragma -b-

#define HB_CLS_NOTOBJECT      // do not inherit from HBObject class
#include "hbclass.ch"

/* HBDbBrowser
 *
 * A minimalistic TBrowse implementation just enough for use in
 * the debugger instead of the HBBrowse monster
 */
CREATE CLASS HBDbBrowser

   VAR Window
   VAR cargo

   VAR nTop
   VAR nLeft
   VAR nBottom
   VAR nRight
   VAR cColorSpec
   VAR autoLite INIT .T.

   VAR goTopBlock
   VAR goBottomBlock
   VAR skipBlock

   VAR stable INIT .F.
   VAR rowCount INIT 0
   VAR rowPos INIT 1
   VAR colCount INIT 0
   VAR colPos INIT 1
   VAR hitBottom INIT .F.
   VAR freeze INIT 0

   VAR aColumns INIT {}
   VAR aRowState INIT {}
   VAR aColorSpec INIT {}
   VAR nFirstVisible INIT 1
   VAR lConfigured INIT .F.

   METHOD New(nTop, nLeft, nBottom, nRight, oParentWindow)
   METHOD AddColumn(oCol)    INLINE AAdd(::aColumns, oCol), ::colCount++, Self
   METHOD Resize(nTop, nLeft, nBottom, nRight)
   ACCESS ColorSpec            INLINE ::cColorSpec
   ASSIGN ColorSpec(cColors) METHOD SetColorSpec(cColors)
   METHOD Configure()
   METHOD DeHiLite()           INLINE ::DispRow(::rowPos, .F.)
   METHOD HiLite()             INLINE ::DispRow(::rowPos, .T.)
   METHOD DispRow(nRow, lHiLite)
   METHOD MoveCursor(nSkip)
   METHOD GoTo(nRow)
   METHOD GoTop()              INLINE ::GoTo(1), ::rowPos := 1, ::nFirstVisible := 1, ::RefreshAll()
   METHOD GoBottom()
   METHOD Down()               INLINE ::MoveCursor(1)
   METHOD Up()                 INLINE ::MoveCursor(-1)
   METHOD PageDown()           INLINE ::MoveCursor(::rowCount)
   METHOD PageUp()             INLINE ::MoveCursor(-::rowCount)
   METHOD GetColumn(nColumn) INLINE ::aColumns[nColumn]
   METHOD RefreshAll()         INLINE AFill(::aRowState, .F.), Self
   METHOD RefreshCurrent()     INLINE IIf(::rowCount >= 1 .AND. ::rowPos <= Len(::aRowState), ::aRowState[::rowPos] := .F., NIL), Self
   METHOD Invalidate()         INLINE ::RefreshAll()
   METHOD Stabilize()          INLINE ::ForceStable()
   METHOD ForceStable()

ENDCLASS

METHOD HBDbBrowser:New(nTop, nLeft, nBottom, nRight, oParentWindow)

   ::Window := oParentWindow
   ::nTop := nTop
   ::nLeft := nLeft
   ::nBottom := nBottom
   ::nRight := nRight

   RETURN Self

METHOD HBDbBrowser:Configure()

   ::rowCount := ::nBottom - ::nTop + 1
   IF ::rowPos > ::rowCount
      ::nFirstVisible += ::rowPos - ::rowCount
      ::rowPos := ::rowCount
   ENDIF
   AFill(ASize(::aRowState, ::rowCount), .F.)
   ::lConfigured := .T.

   RETURN Self

METHOD HBDbBrowser:SetColorSpec(cColors)

   IF hb_IsString(cColors)
      ::cColorSpec := cColors
      ::aColorSpec := hb_ATokens(::cColorSpec, ",")
   ENDIF

   RETURN ::cColorSpec

METHOD HBDbBrowser:MoveCursor(nSkip)

   LOCAL nSkipped

   nSkipped := ::GoTo(::rowPos + ::nFirstVisible - 1 + nSkip)
   IF !::hitBottom .OR. nSkipped != 0
      IF IIf(nSkipped > 0, ::rowPos + nSkipped <= ::rowCount, ::rowPos + nSkipped >= 1)
         ::RefreshCurrent()
         ::rowPos += nSkipped
         ::RefreshCurrent()
      ELSE
         ::nFirstVisible := Max(1, nSkipped + ::nFirstVisible)
         ::RefreshAll()
      ENDIF
   ENDIF

   RETURN Self

METHOD HBDbBrowser:DispRow(nRow, lHiLite)

   LOCAL nColX
   LOCAL nWid
   LOCAL aClr
   LOCAL nClr
   LOCAL xData
   LOCAL oCol

   ::GoTo(::nFirstVisible + nRow - 1)
   IF ::hitBottom
      hb_Scroll(::nTop + nRow - 1, ::nLeft, ::nTop + nRow - 1, ::nRight, NIL, NIL, ::aColorSpec[1])
   ELSE
      DispBegin()
      nColX := ::nLeft
      FOR EACH oCol IN ::aColumns
         IF nColX <= ::nRight
            xData := Eval(oCol:block)
            nClr := IIf(lHiLite, 2, 1)
            aClr := Eval(oCol:colorBlock, xData)
            IF hb_IsArray(aClr)
               nClr := aClr[nClr]
            ELSE
               nClr := oCol:defColor[nClr]
            ENDIF
            nWid := oCol:width
            IF nWid == NIL
               nWid := Len(xData)
            ENDIF
            hb_DispOutAt(::nTop + nRow - 1, nColX, Left(PadR(xData, nWid) + IIf(oCol:__enumIsLast(), "", " "), ::nRight - nColX + 1), ::aColorSpec[nClr])
            nColX += nWid + 1
         ENDIF
      NEXT
      DispEnd()
   ENDIF

   RETURN Self

METHOD HBDbBrowser:ForceStable()

   LOCAL nRow

   IF !::lConfigured
      ::Configure()
   ENDIF
   DispBegin()
   FOR nRow := 1 TO ::rowCount
      IF !::aRowState[nRow]
         ::DispRow(nRow, nRow == ::rowPos)
         ::aRowState[nRow] := .T.
      ENDIF
   NEXT
   ::GoTo(::nFirstVisible + ::rowPos - 1)
   SetPos(::nTop + ::rowPos - 1, ::nLeft)
   DispEnd()

   RETURN Self

METHOD HBDbBrowser:GoTo(nRow)

   LOCAL nOldRow := ::nFirstVisible + ::rowPos - 1
   LOCAL nSkipped := 0

   Eval(::goTopBlock)
   IF nRow == 1
      ::hitBottom := .F.
   ELSE
      nSkipped := Eval(::skipBlock, nRow - 1)
      ::hitBottom := (nSkipped != nRow - 1)
   ENDIF

   RETURN nSkipped - nOldRow + 1

METHOD HBDbBrowser:GoBottom()

   LOCAL nScroll

   DO WHILE !::hitBottom
      ::PageDown()
   ENDDO
   IF ::rowPos < ::rowCount .AND. ::nFirstVisible > 1
      nScroll := Min(::nFirstVisible - 1, ::rowCount - ::rowPos)
      ::nFirstVisible -= nScroll
      ::rowPos += nScroll
   ENDIF

   RETURN Self

METHOD HBDbBrowser:Resize(nTop, nLeft, nBottom, nRight)

   LOCAL lResize := .F.

   IF hb_IsNumeric(nTop) .AND. nTop != ::nTop
      ::nTop := nTop
      lResize := .T.
   ENDIF
   IF hb_IsNumeric(nLeft) .AND. nLeft != ::nLeft
      ::nLeft := nLeft
      lResize := .T.
   ENDIF
   IF hb_IsNumeric(nBottom) .AND. nBottom != ::nBottom
      ::nBottom := nBottom
      lResize := .T.
   ENDIF
   IF hb_IsNumeric(nRight) .AND. nRight != ::nRight
      ::nRight := nRight
      lResize := .T.
   ENDIF

   IF lResize
      ::Configure():ForceStable()
   ENDIF

   RETURN Self

CREATE CLASS HBDbColumn

   EXPORTED:

   VAR block      AS CODEBLOCK                  // Code block to retrieve data for the column
   VAR colorBlock AS CODEBLOCK INIT {|| NIL }   // column color block
   VAR defColor   AS ARRAY     INIT {1, 2}      // Array of numeric indexes into the color table
   VAR width      AS USUAL                      // Column display width

   METHOD New(cHeading, bBlock)

ENDCLASS

METHOD HBDbColumn:New(cHeading, bBlock)

   HB_SYMBOL_UNUSED(cHeading)
   ::block := bBlock

   RETURN Self

FUNCTION HBDbColumnNew(cHeading, bBlock)
   RETURN HBDbColumn():New(cHeading, bBlock)
