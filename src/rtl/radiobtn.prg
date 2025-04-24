//
// RadioButton class
//
// Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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

#include "hbclass.ch"

#include "button.ch"
#include "color.ch"

// NOTE: Harbour doesn't support CA-Cl*pper 5.3 GUI functionality, but
//       it has all related variables and methods.

// NOTE: CA-Cl*pper 5.3 uses a mixture of QQOut(), DevOut(), Disp*()
//       functions to generate screen output. Harbour uses Disp*()
//       functions only. [vszakats]

#ifdef HB_COMPAT_C53

CREATE CLASS RadioButtn FUNCTION HBRadioButton

   EXPORTED:

   VAR cargo                                 // NOTE: CA-Cl*pper 5.3 has a bug, where this var is filled with NIL every time its value is read ( cargo := o:cargo ).

   METHOD display()
   METHOD hitTest(nMRow, nMCol)
   METHOD isAccel(xKey)
   METHOD killFocus()
   METHOD select(lState)
   METHOD setFocus()

   METHOD bitmaps(aBitmaps) SETGET
   METHOD buffer() SETGET
   METHOD data(cData) SETGET               // NOTE: Undocumented CA-Cl*pper 5.3 method.
   METHOD capCol(nCapCol) SETGET
   METHOD capRow(nCapRow) SETGET
   METHOD caption(cCaption) SETGET
   METHOD col(nCol) SETGET
   METHOD colorSpec(cColorSpec) SETGET
   METHOD fBlock(bFBlock) SETGET
   METHOD hasFocus() SETGET
   METHOD row(nRow) SETGET
   METHOD sBlock(bSBlock) SETGET
   METHOD style(cStyle) SETGET

   METHOD Init(nRow, nCol, cCaption, cData)  // NOTE: This method is a Harbour extension [vszakats]

   PROTECTED:

   VAR aBitmaps   INIT {"radio_f.bmu", "radio_e.bmu"}
   VAR lBuffer    INIT .F.
   VAR cData
   VAR nCapCol
   VAR nCapRow
   VAR cCaption
   VAR nCol
   VAR cColorSpec
   VAR bFBlock
   VAR lHasFocus  INIT .F.
   VAR nRow
   VAR bSBlock
   VAR cStyle     INIT "(* )"

ENDCLASS

METHOD RadioButtn:setFocus()

   IF !::lHasFocus
      ::lHasFocus := .T.
      ::display()

      IF hb_IsEvalItem(::bFBlock)
         Eval(::bFBlock)
      ENDIF
   ENDIF

   RETURN Self

METHOD RadioButtn:select(lState)

   LOCAL lOldState := ::lBuffer

   ::lBuffer := IIf(hb_IsLogical(lState), lState, !::lBuffer)

   IF lOldState != ::lBuffer .AND. hb_IsEvalItem(::bSBlock)

      Eval(::bSBlock)
   ENDIF

   RETURN Self

METHOD RadioButtn:killFocus()

   IF ::lHasFocus
      ::lHasFocus := .F.

      IF hb_IsEvalItem(::bFBlock)
         Eval(::bFBlock)
      ENDIF

      ::display()
   ENDIF

   RETURN Self

METHOD RadioButtn:display()

   LOCAL cColor
   LOCAL cStyle := ::cStyle
   LOCAL nPos
   LOCAL cOldCaption

   DispBegin()

   cColor := IIf(::lBuffer, hb_ColorIndex(::cColorSpec, 3), hb_ColorIndex(::cColorSpec, 1))
   hb_DispOutAt(::nRow, ::nCol, Left(cStyle, 1) + IIf(::lBuffer, SubStr(cStyle, 2, 1), SubStr(cStyle, 3, 1)) + Right(cStyle, 1), cColor)

   IF !Empty(cOldCaption := ::cCaption)

      IF (nPos := At("&", cOldCaption)) == 0
      ELSEIF nPos == Len(cOldCaption)
         nPos := 0
      ELSE
         cOldCaption := Stuff(cOldCaption, nPos, 1, "")
      ENDIF

      hb_DispOutAt(::nCapRow, ::nCapCol, cOldCaption, hb_ColorIndex(::cColorSpec, 4))

      IF nPos != 0
         hb_DispOutAt(::nCapRow, ::nCapCol + nPos - 1, SubStr(cOldCaption, nPos, 1), IIf(::lHasfocus, hb_ColorIndex(::cColorSpec, 6), hb_ColorIndex(::cColorSpec, 5)))
      ENDIF
   ENDIF

   DispEnd()

   RETURN Self

METHOD RadioButtn:isAccel(xKey)

   LOCAL cKey

   DO CASE
   CASE hb_IsString(xKey)
      cKey := xKey
   CASE hb_IsNumeric(xKey)
      cKey := hb_keyChar(xKey)
   OTHERWISE
      RETURN .F.
   ENDCASE

   RETURN Len(cKey) > 0 .AND. hb_AtI("&" + cKey, ::cCaption) > 0

METHOD RadioButtn:hitTest(nMRow, nMCol)

   LOCAL nPos
   LOCAL nLen

   IF nMRow == ::Row .AND. nMCol >= ::Col .AND. nMCol < ::Col + 3
      RETURN HTCLIENT
   ENDIF

   nLen := Len(::cCaption)

   IF (nPos := At("&", ::cCaption)) == 0 .AND. nPos < nLen
      nLen--
   ENDIF

   IF nMRow == ::CapRow .AND. nMCol >= ::CapCol .AND. nMCol < ::CapCol + nLen
      RETURN HTCLIENT
   ENDIF

   RETURN HTNOWHERE

METHOD RadioButtn:bitmaps(aBitmaps)

   IF aBitmaps != NIL
      ::aBitmaps := __eInstVar53(Self, "BITMAPS", aBitmaps, "A", 1001, {||Len(aBitmaps) == 2})
   ENDIF

   RETURN ::aBitmaps

METHOD RadioButtn:buffer()
   RETURN ::lBuffer

METHOD RadioButtn:data(cData)

   IF PCount() > 0
      ::cData := IIf(cData == NIL, NIL, __eInstVar53(Self, "DATA", cData, "C", 1001))
   ENDIF

   RETURN IIf(::cData == NIL, __Caption(::Caption), ::cData)

METHOD RadioButtn:capCol(nCapCol)

   IF nCapCol != NIL
      ::nCapCol := __eInstVar53(Self, "CAPCOL", nCapCol, "N", 1001)
   ENDIF

   RETURN ::nCapCol

METHOD RadioButtn:capRow(nCapRow)

   IF nCapRow != NIL
      ::nCapRow := __eInstVar53(Self, "CAPROW", nCapRow, "N", 1001)
   ENDIF

   RETURN ::nCapRow

METHOD RadioButtn:caption(cCaption)

   IF cCaption != NIL
      ::cCaption := __eInstVar53(Self, "CAPTION", cCaption, "C", 1001)
   ENDIF

   RETURN ::cCaption

METHOD RadioButtn:col(nCol)

   IF nCol != NIL
      ::nCol := __eInstVar53(Self, "COL", nCol, "N", 1001)
   ENDIF

   RETURN ::nCol

METHOD RadioButtn:colorSpec(cColorSpec)

   IF cColorSpec != NIL
      ::cColorSpec := __eInstVar53(Self, "COLORSPEC", cColorSpec, "C", 1001, {||!Empty(hb_ColorIndex(cColorSpec, 6)) .AND. Empty(hb_ColorIndex(cColorSpec, 7))})
   ENDIF

   RETURN ::cColorSpec

METHOD RadioButtn:fBlock(bFBlock)

   IF PCount() > 0
      ::bFBlock := IIf(bFBlock == NIL, NIL, __eInstVar53(Self, "FBLOCK", bFBlock, "B", 1001))
   ENDIF

   RETURN ::bFBlock

METHOD RadioButtn:hasFocus()
   RETURN ::lHasFocus

METHOD RadioButtn:row(nRow)

   IF nRow != NIL
      ::nRow := __eInstVar53(Self, "ROW", nRow, "N", 1001)
   ENDIF

   RETURN ::nRow

METHOD RadioButtn:sBlock(bSBlock)

   IF PCount() > 0
      ::bSBlock := IIf(bSBlock == NIL, NIL, __eInstVar53(Self, "SBLOCK", bSBlock, "B", 1001))
   ENDIF

   RETURN ::bSBlock

METHOD RadioButtn:style(cStyle)

   IF cStyle != NIL
      ::cStyle := __eInstVar53(Self, "STYLE", cStyle, "C", 1001, {||Len(cStyle) == 0 .OR. Len(cStyle) == 4})
   ENDIF

   RETURN ::cStyle

METHOD RadioButtn:Init(nRow, nCol, cCaption, cData)

   LOCAL cColor

   IF !hb_IsNumeric(nRow) .OR. !hb_IsNumeric(nCol)
      RETURN NIL
   ENDIF

   ::nCapRow  := nRow
   ::nCapCol  := nCol + 3 + 1
   ::cCaption := hb_defaultValue(cCaption, "")
   ::nCol     := nCol
   ::nRow     := nRow
   ::cData    := cData  // NOTE: Every type is allowed here to be fully compatible

   IF IsDefColor()
      ::cColorSpec := "W/N,W+/N,W+/N,N/W,W/N,W/N,W+/N"
   ELSE
      cColor := SetColor()
      ::cColorSpec := ;
         hb_ColorIndex(cColor, CLR_UNSELECTED) + "," + ;
         hb_ColorIndex(cColor, CLR_UNSELECTED) + "," + ;
         hb_ColorIndex(cColor, CLR_ENHANCED) + "," + ;
         hb_ColorIndex(cColor, CLR_ENHANCED) + "," + ;
         hb_ColorIndex(cColor, CLR_STANDARD) + "," + ;
         hb_ColorIndex(cColor, CLR_STANDARD) + "," + ;
         hb_ColorIndex(cColor, CLR_BACKGROUND)
   ENDIF

   RETURN Self

FUNCTION RadioButto(nRow, nCol, cCaption, cData) // NOTE: cData argument is undocumented
   RETURN HBRadioButton():New(nRow, nCol, cCaption, cData)

#endif
