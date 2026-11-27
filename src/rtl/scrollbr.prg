//
// ScrollBar class
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

CREATE CLASS ScrollBar Function HBScrollBar

   EXPORTED:

   VAR cargo

   METHOD barLength() SETGET
   METHOD bitmaps(aBitmaps) SETGET
   METHOD colorSpec(cColorSpec) SETGET
   METHOD current(nCurrent) SETGET
   METHOD end(nEnd) SETGET
   METHOD offset(nOffset) SETGET
   METHOD orient(nOrient) SETGET
   METHOD sBlock(bSBlock) SETGET
   METHOD start(nStart) SETGET
   METHOD style(cStyle) SETGET
   METHOD thumbPos(nThumbPos) SETGET
   METHOD total(nTotal) SETGET

   METHOD display()
   METHOD update()
   METHOD hitTest(nMRow, nMCol)

   METHOD Init(nStart, nEnd, nOffset, bSBlock, nOrient)  // NOTE: This method is a Harbour extension [vszakats]

   PROTECTED:

   VAR aBitmaps
   VAR nBarLength
   VAR cColorSpec
   VAR cStyle
   VAR nCurrent   INIT 1
   VAR nEnd       INIT 0
   VAR nOffset
   VAR nOrient
   VAR nStart     INIT 0
   VAR nThumbPos  INIT 1
   VAR nTotal     INIT 100
   VAR bSBlock

   VAR lOverride  INIT False

   METHOD CalcThumbPos()

ENDCLASS

METHOD ScrollBar:display()

   Local cColor
   Local cStyle
   Local nOffset
   Local nStart
   Local nEnd
   Local nPos

   IF ::CalcThumbPos()

      cStyle  := ::cStyle
      nOffset := ::nOffset
      nStart  := ::nStart
      nEnd    := ::nEnd - 1

      DispBegin()

      cColor := hb_ColorIndex(::cColorSpec, 0)

      IF ::nOrient == SCROLL_VERTICAL

         For nPos := nStart + 1 To nEnd
            hb_DispOutAt(nPos, nOffset, SubStr(cStyle, 2, 1), cColor)
         Next

         cColor := hb_ColorIndex(::cColorSpec, 1)
         hb_DispOutAt(nStart, nOffset, SubStr(cStyle, 1, 1), cColor)
         hb_DispOutAt(nStart + ::nThumbPos, nOffset, SubStr(cStyle, 3, 1), cColor)
         hb_DispOutAt(nEnd + 1, nOffset, SubStr(cStyle, 4, 1), cColor)
      ELSE

         hb_DispOutAt(nOffset, nStart + 1, Replicate(SubStr(cStyle, 2, 1), nEnd - nStart), cColor)

         cColor := hb_ColorIndex(::cColorSpec, 1)
         hb_DispOutAt(nOffset, nStart, SubStr(cStyle, 1, 1), cColor)
         hb_DispOutAt(nOffset, nStart + ::nThumbPos, SubStr(cStyle, 3, 1), cColor)
         hb_DispOutAt(nOffset, nEnd + 1, SubStr(cStyle, 4, 1), cColor)

      ENDIF

      DispEnd()

      Return True
   ENDIF

   Return False

METHOD ScrollBar:update()

   Local nOldThumbPos := ::nThumbPos

   IF hb_IsEvalItem(::bSBlock)
      Eval(::bSBlock)
   ENDIF

   IF ::CalcThumbPos() .AND. nOldThumbPos != ::nThumbPos

      DispBegin()

      IF ::nOrient == SCROLL_VERTICAL
         hb_DispOutAt(::nStart + nOldThumbPos, ::nOffSet, SubStr(::cStyle, 2, 1), hb_ColorIndex(::cColorSpec, 0))
         hb_DispOutAt(::nStart + ::nThumbPos, ::nOffset, SubStr(::cStyle, 3, 1), hb_ColorIndex(::cColorSpec, 1))
      ELSE
         hb_DispOutAt(::nOffset, ::nStart + nOldThumbPos, SubStr(::cStyle, 2, 1), hb_ColorIndex(::cColorSpec, 0))
         hb_DispOutAt(::nOffset, ::nStart + ::nThumbPos, SubStr(::cStyle, 3, 1), hb_ColorIndex(::cColorSpec, 1))
      ENDIF

      DispEnd()

      Return True
   ENDIF

   Return False

METHOD ScrollBar:hitTest(nMRow, nMCol)

   IF ::nOrient == SCROLL_VERTICAL

      Do Case
      Case nMCol != ::nOffset
      Case nMRow < ::nStart
      Case nMRow > ::nEnd
      Case nMRow == ::nStart
         RETURN HTSCROLLUNITDEC
      Case nMRow == ::nEnd
         RETURN HTSCROLLUNITINC
      Case nMRow < ::nThumbPos + ::nStart
         RETURN HTSCROLLBLOCKDEC
      Case nMRow > ::nThumbPos + ::nStart
         RETURN HTSCROLLBLOCKINC
      Case nMRow == ::nThumbPos + ::nStart
         RETURN HTSCROLLTHUMBDRAG
      EndCase

      IF nMCol == ::nOffset + 1 .OR. nMCol == ::nOffset

         Do Case
         Case nMCol != ::nOffset .AND. nMCol != ::nOffset + 1
         Case nMRow < ::nStart
         Case nMRow > ::nEnd
         Case nMRow == ::nStart
            RETURN HTSCROLLUNITDEC
         Case nMRow == ::nEnd
            RETURN HTSCROLLUNITINC
         Case nMRow < ::nThumbPos + ::nStart
            RETURN HTSCROLLBLOCKDEC
         Case nMRow > ::nThumbPos + ::nStart
            RETURN HTSCROLLBLOCKINC
         Case nMRow == ::nThumbPos + ::nStart
            RETURN HTSCROLLTHUMBDRAG
         EndCase
      ENDIF
   ELSE

      Do Case
      Case nMRow != ::nOffset
      Case nMCol < ::nStart
      Case nMCol > ::nEnd
      Case nMCol == ::nStart
         RETURN HTSCROLLUNITDEC
      Case nMCol == ::nEnd
         RETURN HTSCROLLUNITINC
      Case nMCol < ::nThumbPos + ::nStart
         RETURN HTSCROLLBLOCKDEC
      Case nMCol > ::nThumbPos + ::nStart
         RETURN HTSCROLLBLOCKINC
      Case nMCol == ::nThumbPos + ::nStart
         RETURN HTSCROLLTHUMBDRAG
      EndCase
   ENDIF

   RETURN HTNOWHERE

METHOD ScrollBar:barLength()
   RETURN ::nBarLength

METHOD ScrollBar:bitmaps(aBitmaps)

   IF hb_IsArray(aBitmaps) .AND. Len(aBitmaps) == 3

      ::aBitmaps := aBitmaps
   ENDIF

   RETURN ::aBitmaps

METHOD ScrollBar:colorSpec(cColorSpec)

   IF hb_IsString(cColorSpec) .AND. !Empty(hb_ColorIndex(cColorSpec, 1)) .AND. Empty(hb_ColorIndex(cColorSpec, 2))

      ::cColorSpec := cColorSpec
   ENDIF

   RETURN ::cColorSpec

METHOD ScrollBar:current(nCurrent)

   IF hb_IsNumeric(nCurrent) .AND. nCurrent <= ::nTotal .AND. nCurrent != ::nCurrent

      ::nCurrent := nCurrent
   ENDIF

   RETURN ::nCurrent

METHOD ScrollBar:end(nEnd)

   IF hb_IsNumeric(nEnd) .AND. nEnd >= ::nStart .AND. nEnd != ::nEnd

      ::nEnd := nEnd
      ::nBarLength := nEnd - ::nStart - 1
   ENDIF

   RETURN ::nEnd

METHOD ScrollBar:offset(nOffset)

   IF hb_IsNumeric(nOffset) .AND. nOffset != ::nOffset

      ::nOffset := nOffset
   ENDIF

   RETURN ::nOffset

METHOD ScrollBar:orient(nOrient)

   IF hb_IsNumeric(nOrient) .AND. (nOrient == SCROLL_VERTICAL .OR. nOrient == SCROLL_HORIZONTAL)

      ::nOrient := nOrient
   ENDIF

   RETURN ::nOrient

METHOD ScrollBar:sBlock(bSBlock)

   IF hb_IsEvalItem(bSBlock)
      ::bSBlock := bSBlock
   ENDIF

   RETURN ::bSBlock

METHOD ScrollBar:start(nStart)

   IF hb_IsNumeric(nStart) .AND. nStart <= ::nEnd .AND. nStart != ::nStart

      ::nStart := nStart
      ::nBarLength := ::nEnd - nStart - 1
   ENDIF

   RETURN ::nStart

METHOD ScrollBar:style(cStyle)

   IF hb_IsString(cStyle) .AND. Len(cStyle) == 4

      ::cStyle := cStyle
   ENDIF

   RETURN ::cStyle

METHOD ScrollBar:thumbPos(nThumbPos)

   IF hb_IsNumeric(nThumbPos)

      Do Case
      Case nThumbPos < 1
         ::nThumbPos := 1
      Case nThumbPos >= ::nBarLength
         ::nThumbPos := ::nBarLength
      Case nThumbPos >= ::nBarLength - 1
         ::nThumbPos := nThumbPos
      Otherwise
         ::nThumbPos := nThumbPos
      EndCase

      ::lOverride := (nThumbPos != 0)
   ENDIF

   RETURN ::nThumbPos

METHOD ScrollBar:total(nTotal)

   IF hb_IsNumeric(nTotal) .AND. nTotal >= 2 .AND. nTotal != ::nTotal

      ::nTotal := nTotal
   ENDIF

   RETURN ::nTotal

METHOD ScrollBar:CalcThumbPos()

   Local nBarLength := ::nBarLength
   Local nTotal := ::nTotal

   IF nBarLength < 2 .OR. nTotal < 2
      Return False
   ENDIF

   IF !::lOverride
      ::nThumbPos := Min(Max(Round(::nCurrent * ((nBarLength - 1) / nTotal) + 1, 0), 1), nBarLength)
   ENDIF

   Return True

// New definitions for better coding. These are screen codepage dependent,
// but can be changed with the setStyle method.
#define SB_UPARROW      Chr(24)  // LOW-ASCII "↑"
#define SB_DNARROW      Chr(25)  // LOW-ASCII "↓"
#define SB_RIGHTARROW   Chr(27)  // LOW-ASCII "→"
#define SB_LEFTARROW    Chr(26)  // LOW-ASCII "←"

#define SB_THUMB        hb_UTF8ToStr("░")
#define SB_TRACK        hb_UTF8ToStr("▓")

METHOD ScrollBar:Init(nStart, nEnd, nOffset, bSBlock, nOrient)

   Local cColor

   __defaultNIL(@nOrient, SCROLL_VERTICAL)

   IF !hb_IsNumeric(nStart) .OR. ;
      !hb_IsNumeric(nEnd) .OR. ;
      !hb_IsNumeric(nOffset) .OR. ;
      !ValType(bSBlock) $ "BU" .OR. ;
      !hb_IsNumeric(nOrient) .OR. ;
      (nOrient != SCROLL_VERTICAL .AND. nOrient != SCROLL_HORIZONTAL)
      Return Nil
   ENDIF

   ::end        := nEnd
   ::offSet     := nOffset
   ::orient     := nOrient
   ::sBlock     := bSBlock
   ::start      := nStart
   ::nBarLength := nEnd - nStart - 1

   Switch nOrient
   Case SCROLL_VERTICAL
      ::cStyle := SB_UPARROW + SB_THUMB + SB_TRACK + SB_DNARROW
      ::aBitmaps := {"arrow_u.bmu", "arrow_d.bmu", "arrow_e.bmu"}
      Exit
   Case SCROLL_HORIZONTAL
      ::cStyle := SB_LEFTARROW + SB_THUMB + SB_TRACK + SB_RIGHTARROW
      ::aBitmaps := {"arrow_l.bmu", "arrow_r.bmu", "arrow_e.bmu"}
      Exit
   EndSwitch

   cColor := SetColor()
   ::cColorSpec := hb_ColorIndex(cColor, CLR_UNSELECTED) + "," + hb_ColorIndex(cColor, CLR_ENHANCED)

   Return Self

Function ScrollBar(nStart, nEnd, nOffset, bSBlock, nOrient)
   RETURN HBScrollBar():New(nStart, nEnd, nOffset, bSBlock, nOrient)

#endif
