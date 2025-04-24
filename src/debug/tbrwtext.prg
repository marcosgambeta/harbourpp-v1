//
// Text file browser class
//
// Copyright 2008 Lorenzo Fiorini <lorenzo.fiorini@gmail.com>
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

CREATE CLASS HBBrwText

   VAR cFileName
   VAR aRows
   VAR nRows
   VAR nLineNoLen
   VAR nActiveLine
   VAR lLineNumbers
   VAR nRow
   VAR nFirstCol
   VAR nCol

   VAR oBrw

   VAR nLineOffset   INIT 1
   VAR nMaxLineLen
   VAR nTabWidth     INIT 4

   VAR nTop
   VAR nLeft
   VAR nBottom
   VAR nRight

   VAR nWidth
   VAR nHeight

   METHOD New(nTop, nLeft, nBottom, nRight, cFileName, cColors, lLineNumbers)

   METHOD RefreshAll() INLINE ::oBrw:ForceStable():RefreshAll(), Self
   METHOD ForceStable() INLINE ::oBrw:ForceStable(), Self
   METHOD RefreshCurrent() INLINE ::oBrw:RefreshCurrent(), Self
   METHOD GotoLine(n)
   METHOD SetActiveLine(n)
   METHOD GetLine()
   METHOD GetLineText()
   METHOD GetLineColor()
   METHOD Search(cString, lCaseSensitive, nMode)

   METHOD GoFirst()
   METHOD GoLast()
   METHOD Skip(n)
   METHOD GoNext()
   METHOD GoPrev()

   METHOD Resize(nTop, nLeft, nBottom, nRight)

   METHOD Up() INLINE ::oBrw:Up():ForceStable(), Self
   METHOD Down() INLINE ::oBrw:Down():ForceStable(), Self
   METHOD PageUp() INLINE ::oBrw:PageUp():ForceStable(), Self
   METHOD PageDown() INLINE ::oBrw:PageDown():ForceStable(), Self
   METHOD GoTop() INLINE ::oBrw:GoTop():ForceStable(), Self
   METHOD GoBottom() INLINE ::oBrw:GoBottom():ForceStable(), Self

   METHOD Home() INLINE IIf(::nLineOffset > 1, (::nLineOffset := 1, ::oBrw:RefreshAll():ForceStable()), NIL), Self
   METHOD End() INLINE ::nLineOffset := Max(1, ::nMaxLineLen - (::nWidth - IIf(::lLineNumbers, ::nLineNoLen, 0)) + 1), ::oBrw:RefreshAll():ForceStable(), Self

   METHOD Right() INLINE IIf(::nLineOffset < ::nMaxLineLen + IIf(::lLineNumbers, ::nLineNoLen, 0), (::nLineOffset++, ::oBrw:RefreshAll():ForceStable()), NIL), Self
   METHOD Left() INLINE IIf(::nLineOffset > 1, (::nLineOffset--, ::oBrw:RefreshAll():ForceStable()), NIL), Self

   METHOD RowPos() INLINE ::nRow

   METHOD LoadFile(cFileName)

   VAR colorSpec IS colorSpec IN oBrw

ENDCLASS

METHOD HBBrwText:New(nTop, nLeft, nBottom, nRight, cFileName, cColors, lLineNumbers)

   LOCAL oCol

   ::nTop := nTop
   ::nLeft := nLeft
   ::nBottom := nBottom
   ::nRight := nRight

   ::nWidth := nRight - nLeft + 1
   ::nHeight := nBottom - nTop

   ::lLineNumbers := lLineNumbers

   ::oBrw := HBDbBrowser():New(::nTop, ::nLeft, ::nBottom, ::nRight)

   ::oBrw:colorSpec := cColors

   oCol := HBDbColumnNew("", {||::GetLineText()})

   oCol:colorBlock := {||::GetLineColor()}

   ::oBrw:AddColumn(oCol)

   ::oBrw:goTopBlock := {||::nRow := 1}
   ::oBrw:goBottomBlock := {||::nRow := ::nRows}
   ::oBrw:skipBlock := {|n|::Skip(n)}

   IF !Empty(cFileName)
      ::LoadFile(cFileName)
   ENDIF

   RETURN Self

METHOD HBBrwText:GotoLine(n)

   ::oBrw:MoveCursor(n - ::nRow)
   ::RefreshAll()

   RETURN Self

METHOD HBBrwText:SetActiveLine(n)

   ::nActiveLine := n
   ::RefreshAll()

   RETURN Self

METHOD HBBrwText:GetLine()
   RETURN IIf(::lLineNumbers, PadR(hb_ntos(::nRow) + ":", ::nLineNoLen), "") + ;
      MemoLine(::aRows[::nRow], ::nMaxLineLen, 1, ::nTabWidth, .F.)

METHOD HBBrwText:GetLineText()
   RETURN PadR(SubStr(::GetLine(), ::nLineOffset), ::nWidth)

METHOD HBBrwText:GetLineColor()

   LOCAL aColor

   IF __dbgIsBreak(__dbg():pInfo, ::cFileName, ::nRow) >= 0
      aColor := IIf(::nRow == ::nActiveLine, {4, 4}, {3, 3})
   ELSE
      aColor := IIf(::nRow == ::nActiveLine, {2, 2}, {1, 1})
   ENDIF

   RETURN aColor

METHOD PROCEDURE HBBrwText:LoadFile(cFileName)

   LOCAL nMaxLineLen := 0
   LOCAL cLine

   ::cFileName := cFileName
   ::aRows := __dbgTextToArray(MemoRead(cFileName))
   ::nRows := Len(::aRows)
   ::nLineNoLen := Len(hb_ntos(::nRows)) + 2

   FOR EACH cLine IN ::aRows
      nMaxLineLen := Max(nMaxLineLen, Len(RTrim(MemoLine(cLine, Len(cLine) + 256, 1, ::nTabWidth, .F.))))
   NEXT
   ::nMaxLineLen := nMaxLineLen
   ::nLineOffset := 1

   RETURN

METHOD HBBrwText:Resize(nTop, nLeft, nBottom, nRight)

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
      ::oBrw:Resize(nTop, nLeft, nBottom, nRight)
      ::nWidth := ::nRight - ::nLeft + 1
   ENDIF

   RETURN Self

METHOD HBBrwText:Search(cString, lCaseSensitive, nMode)

   LOCAL bMove
   LOCAL lFound := .F.
   LOCAL n

   IF !lCaseSensitive
      cString := Upper(cString)
   ENDIF

   SWITCH hb_defaultValue(nMode, 0)
   CASE 0 // From Top
      ::GoTop()
      bMove := {||::Skip(1)}
      EXIT
   CASE 1 // Forward
      bMove := {||::Skip(1)}
      EXIT
   CASE 2 // Backward
      bMove := {||::Skip(-1)}
      EXIT
   ENDSWITCH

   n := ::nRow

   DO WHILE Eval(bMove) != 0
      IF cString $ IIf(lCaseSensitive, ::aRows[::nRow], Upper(::aRows[::nRow]))
         lFound := .T.
         ::oBrw:MoveCursor(::nRow - n)
         ::RefreshAll()
         EXIT
      ENDIF
   ENDDO

   RETURN lFound

METHOD HBBrwText:GoFirst()

   ::nRow := 1

   RETURN .T.

METHOD HBBrwText:GoLast()

   ::nRow := ::nRows

   RETURN .T.

METHOD HBBrwText:Skip(n)

   LOCAL nSkipped := 0

   IF n > 0
      IF ::nRow < ::nRows
         nSkipped := Min(::nRows - ::nRow, n)
         ::nRow += nSkipped
      ENDIF
   ELSEIF n < 0
      IF ::nRow > 1
         nSkipped := Max(1 - ::nRow, n)
         ::nRow += nSkipped
      ENDIF
   ENDIF

   RETURN nSkipped

METHOD HBBrwText:GoPrev()

   LOCAL lMoved := .F.

   IF ::nRow > 1
      ::nRow--
      lMoved := .T.
   ENDIF

   RETURN lMoved

METHOD HBBrwText:GoNext()

   LOCAL lMoved := .F.

   IF ::nRow < ::nRows
      ::nRow++
      lMoved := .T.
   ENDIF

   RETURN lMoved
