//
// Listbox class
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

#include "box.ch"
#include "button.ch"
#include "color.ch"
#include "inkey.ch"
#include "setcurs.ch"

/* NOTE: Harbour doesn't support CA-Cl*pper 5.3 GUI functionality, but
         it has all related variables and methods. */

/* NOTE: CA-Cl*pper 5.3 uses a mixture of QQOut(), DevOut(), Disp*()
         functions to generate screen output. Harbour uses Disp*()
         functions only. [vszakats] */

#ifdef HB_COMPAT_C53

#define _ITEM_cText         1
#define _ITEM_xData         2

#define _LISTBOX_ITEMDATA(aItem)  IIf(aItem[_ITEM_xData] == NIL, aItem[_ITEM_cText], aItem[_ITEM_xData])

CREATE CLASS ListBox FUNCTION HBListBox

   PROTECTED:

   /* --- Start of CA-Cl*pper compatible instance area --- */
   VAR nBottom
   VAR xBuffer
   VAR cCaption   INIT ""
   VAR nCapCol
   VAR nCapRow
   VAR cargo      EXPORTED
   VAR cColdBox   INIT HB_B_SINGLE_UNI
   VAR cColorSpec
   VAR aItems     INIT {}
   VAR lDropDown
   VAR bFBlock
   VAR lHasFocus  INIT .F.
   VAR cHotBox    INIT HB_B_DOUBLE_UNI
   VAR nItemCount INIT 0
   VAR nLeft
   VAR cMessage   INIT ""
   VAR aSaveScr
   VAR lIsOpen
   VAR nRight
   VAR bSBlock
   VAR nCursor
   VAR cStyle     INIT Chr(31) /* LOW-ASCII "▼" */
   VAR cTextValue INIT ""
   VAR nTop
   VAR nTopItem   INIT 0
   VAR oVScroll
   VAR nValue     INIT 0
   VAR cBitmap    INIT "dropbox.bmu"

   EXPORTED:

   METHOD addItem(cText, xData)
   METHOD close()
   METHOD delItem(nPos)
   METHOD display()
   METHOD findText(cText, nPos, lCaseSensitive, lExact)
   METHOD findData(xData, nPos, lCaseSensitive, lExact)  /* NOTE: Undocumented CA-Cl*pper method. */
   METHOD getData(nPos)
   METHOD getItem(nPos)
   METHOD getText(nPos)
   METHOD hitTest(nMRow, nMCol)
   METHOD insItem(nPos, cText, xData)
   METHOD killFocus()
   METHOD nextItem()
   METHOD open()
   METHOD prevItem()
   METHOD scroll(nMethod)
   METHOD select(xPos)
   METHOD setData(nPos, xData)
   METHOD setFocus()
   METHOD setItem(nPos, aItem)
   METHOD setText(nPos, cText)

   METHOD bitmap(cBitmap) SETGET
   METHOD bottom(nBottom) SETGET
   METHOD buffer() SETGET
   METHOD capCol(nCapCol) SETGET
   METHOD capRow(nCapRow) SETGET
   METHOD caption(cCaption) SETGET
   METHOD coldBox(cColdBox) SETGET
   METHOD colorSpec(cColorSpec) SETGET
   METHOD dropDown(lDropDown) SETGET
   METHOD fBlock(bFBlock) SETGET
   METHOD hasFocus() SETGET
   METHOD hotBox(cHotBox) SETGET
   METHOD isOpen() SETGET
   METHOD itemCount() SETGET
   METHOD left(nLeft) SETGET
   METHOD message(cMessage) SETGET
   METHOD right(nRight) SETGET
   METHOD sBlock(bSBlock) SETGET
   METHOD style(cStyle) SETGET                           /* NOTE: Undocumented CA-Cl*pper method. */
   METHOD textValue() SETGET                               /* NOTE: Undocumented CA-Cl*pper method. */
   METHOD top(nTop) SETGET
   METHOD topItem(nTopItem) SETGET
   METHOD typeOut() SETGET
   METHOD value() SETGET                                   /* NOTE: Undocumented CA-Cl*pper method. */
   METHOD vScroll(oVScroll) SETGET

   METHOD Init(nTop, nLeft, nBottom, nRight, lDropDown)   /* NOTE: This method is a Harbour extension [vszakats] */

   PROTECTED:

   METHOD changeItem(nOldPos, nNewPos)
   METHOD scrollbarPos()

ENDCLASS

METHOD ListBox:addItem(cText, xData)

   IF hb_IsString(cText)

      AAdd(::aItems, {cText, xData})

      ::nItemCount++

      IF ::nItemCount == 1
         ::nTopItem := 1
         IF ::oVScroll != NIL
            ::oVScroll:total := (::nItemCount - (::nBottom - ::nTop - 2))
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

METHOD ListBox:close()

   IF ::lIsOpen
      RestScreen(::aSaveScr[1], ::aSaveScr[2], ::aSaveScr[3], ::aSaveScr[4], ::aSaveScr[5])
      ::lIsOpen := .F.
      ::aSaveScr := NIL
   ENDIF

   RETURN Self

METHOD ListBox:delItem(nPos)

   IF nPos >= 1 .AND. nPos <= ::nItemCount

      hb_ADel(::aItems, nPos, .T.)
      ::nItemCount--

      IF ::nValue > ::nItemCount
         ::nValue := ::nItemCount

         ::cTextValue := IIf(::nValue == 0, "", _LISTBOX_ITEMDATA(::aItems[::nItemCount]))

         IF ::xBuffer == NIL
         ELSEIF hb_IsNumeric(::xBuffer)
            ::xBuffer := ::nItemCount
         ELSEIF ::nValue > 0
            ::xBuffer := ::cTextValue
         ENDIF
      ENDIF

      IF ::nTopItem > ::nItemCount
         ::nTopItem := ::nItemCount
      ENDIF

      IF ::oVScroll != NIL
         ::oVScroll:total := ::nItemCount - (::nBottom - ::nTop - 2)
      ENDIF
   ENDIF

   RETURN Self

METHOD ListBox:display()

   LOCAL nItem
   LOCAL nEnd
   LOCAL cColor4
   LOCAL cColor3
   LOCAL cColorAny
   LOCAL cColorScrl
   LOCAL nTop := ::nTop
   LOCAL nLeft := ::nLeft
   LOCAL nSize := ::nRight - nLeft + 1
   LOCAL cHotBox
   LOCAL cCaption
   LOCAL nPos

   IF ::lHasFocus
      cHotBox   := ::cHotBox
      cColor3   := hb_ColorIndex(::cColorSpec, 2)
      cColor4   := hb_ColorIndex(::cColorSpec, 3)
      cColorAny := IIf(::lIsOpen, hb_ColorIndex(::cColorSpec, 1), hb_ColorIndex(::cColorSpec, 3))
   ELSE
      cHotBox   := ::cColdBox
      cColor3   := hb_ColorIndex(::cColorSpec, 0)
      cColor4   := hb_ColorIndex(::cColorSpec, 1)
      cColorAny := hb_ColorIndex(::cColorSpec, 1)
   ENDIF

   DispBegin()

   nEnd := ::nTopItem + ::nBottom - ::nTop

   IF ::lDropDown

      hb_DispOutAt(nTop, nLeft, IIf(::nValue == 0, Space(nSize - 1), PadR(::aItems[::nValue][_ITEM_cText], nSize - 1)), cColorAny)

      hb_DispOutAt(nTop++, nLeft + nSize - 1, ::cStyle, hb_ColorIndex(::cColorSpec, 7))

      nEnd--
   ENDIF

   IF ::lIsOpen
      IF !Empty(cHotBox)

         cColorScrl := hb_ColorIndex(::cColorSpec, 4)
         hb_Scroll(nTop, nLeft, ::nBottom, ::nRight, NIL, NIL, cColorScrl)
         hb_DispBox(nTop, nLeft, ::nBottom, ::nRight, cHotBox, cColorScrl)

         IF ::oVScroll != NIL
            ::oVScroll:display()
         ENDIF

         nTop++
         nLeft++
         nSize -= 2
         nEnd -= 2
      ENDIF

      IF nEnd > ::nItemCount
         nEnd := ::nItemCount
      ENDIF

      FOR nItem := ::nTopItem TO nEnd
         hb_DispOutAt(nTop++, nLeft, PadR(::aItems[nItem][_ITEM_cText], nSize), IIf(nItem == ::nValue, cColor4, cColor3))
      NEXT
   ENDIF

   IF !Empty(cCaption := ::cCaption)

      IF (nPos := At("&", cCaption)) == 0
      ELSEIF nPos == Len(cCaption)
         nPos := 0
      ELSE
         cCaption := Stuff(cCaption, nPos, 1, "")
      ENDIF

      hb_DispOutAt(::nCapRow, ::nCapCol - 1, cCaption, hb_ColorIndex(::cColorSpec, 5))

      IF nPos != 0
         hb_DispOutAt(::nCapRow, ::nCapCol + nPos - 2, SubStr(cCaption, nPos, 1), hb_ColorIndex(::cColorSpec, 6))
      ENDIF
   ENDIF

   DispEnd()

   RETURN Self

METHOD ListBox:findText(cText, nPos, lCaseSensitive, lExact)

   LOCAL nPosFound
   LOCAL bSearch

#ifndef HB_CLP_STRICT
   /* NOTE: Cl*pper will RTE if passed a non-string cText */
   IF !hb_IsString(cText)
      RETURN 0
   ENDIF
#endif

   hb_default(@nPos, 1)
   hb_default(@lCaseSensitive, .T.)
   IF !hb_IsLogical(lExact)
      lExact := Set(_SET_EXACT)
   ENDIF

   IF lExact
      IF lCaseSensitive
         bSearch := {|aItem|aItem[_ITEM_cText] == cText}
      ELSE
         cText := Lower(cText)
         bSearch := {|aItem|Lower(aItem[_ITEM_cText]) == cText}
      ENDIF
   ELSE
      IF lCaseSensitive
         bSearch := {|aItem|hb_LeftEq(aItem[_ITEM_cText], cText)}
      ELSE
         bSearch := {|aItem|hb_LeftEqI(aItem[_ITEM_cText], cText)}
      ENDIF
   ENDIF

   IF (nPosFound := AScan(::aItems, bSearch, nPos, Len(::aItems) - nPos + 1)) == 0 .AND. nPos > 1
      nPosFound := AScan(::aItems, bSearch, 1, nPos - 1)
   ENDIF

   RETURN nPosFound

/* NOTE: Both Cl*pper and Harbour may RTE when searching for
         a different type than present in an item value. The RTE
         will be different and in Cl*pper, but will occur under
         the same conditions. */
METHOD ListBox:findData(xData, nPos, lCaseSensitive, lExact)

   LOCAL nPosFound
   LOCAL bSearch

   hb_default(@nPos, 1)
   hb_default(@lCaseSensitive, .T.)
   IF !hb_IsLogical(lExact)
      lExact := Set(_SET_EXACT)
   ENDIF

   IF lExact
      IF lCaseSensitive
         bSearch := {|aItem|_LISTBOX_ITEMDATA(aItem) == xData}
      ELSE
         /* Cl*pper will also RTE here, if xData is not a string */
         xData := Lower(xData)
         bSearch := {|aItem|Lower(_LISTBOX_ITEMDATA(aItem)) == xData}
      ENDIF
   ELSE
      IF lCaseSensitive
         bSearch := {|aItem, xItemData|xItemData := _LISTBOX_ITEMDATA(aItem), IIf(hb_IsString(xItemData), hb_LeftEq(xItemData, xData), xItemData == xData)}
      ELSE
         /* Cl*pper will also RTE here, if xData is not a string */
         bSearch := {|aItem|hb_LeftEqI(_LISTBOX_ITEMDATA(aItem), xData)}
      ENDIF
   ENDIF

   IF (nPosFound := AScan(::aItems, bSearch, nPos, Len(::aItems) - nPos + 1)) == 0 .AND. nPos > 1
      nPosFound := AScan(::aItems, bSearch, 1, nPos - 1)
   ENDIF

   RETURN nPosFound

METHOD ListBox:getData(nPos)
   RETURN IIf(nPos >= 1 .AND. nPos <= ::nItemCount, ::aItems[nPos][_ITEM_xData], NIL)

METHOD ListBox:getItem(nPos)
   RETURN IIf(nPos >= 1 .AND. nPos <= ::nItemCount, ::aItems[nPos], NIL)

METHOD ListBox:getText(nPos)
   RETURN IIf(nPos >= 1 .AND. nPos <= ::nItemCount, ::aItems[nPos][_ITEM_cText], NIL)

METHOD ListBox:hitTest(nMRow, nMCol)

   LOCAL nTop := ::nTop
   LOCAL nOffset := 0
   LOCAL nHit := 0

   // Check hit on the scrollbar
   IF ::lIsOpen .AND. ::oVScroll != NIL .AND. (nHit := ::oVScroll:hitTest(nMRow, nMCol)) != 0
      RETURN nHit
   ENDIF

   IF ::lIsOpen .AND. ::lDropDown
      nTop++
   ENDIF

   // NOTE: Harbour extension over Cl*pper abilities, CLP5.3
   //       with mouse support will crash, RTE "Argument Error: +"
   //       when a borderless listbox/dropdown is mouse-clicked
   IF ::lIsOpen .AND. !Empty(::cHotBox + ::cColdBox)

      nOffset := 1
      DO CASE
      CASE nMRow == nTop
         DO CASE
         CASE nMCol == ::nLeft
            RETURN HTTOPLEFT
         CASE nMCol == ::nRight
            RETURN HTTOPRIGHT
         CASE nMCol >= ::nLeft .AND. nMCol <= ::nRight
            RETURN HTTOP
         ENDCASE
      CASE nMRow == ::nBottom
         DO CASE
         CASE nMCol == ::nLeft
            RETURN HTBOTTOMLEFT
         CASE nMCol == ::nRight
            RETURN HTBOTTOMRIGHT
         CASE nMCol >= ::nLeft .AND. nMCol <= ::nRight
            RETURN HTBOTTOM
         ENDCASE
      CASE nMCol == ::nLeft
         IF nMRow >= nTop .AND. nMRow <= ::nBottom
            RETURN HTLEFT
         ELSE
            RETURN HTNOWHERE
         ENDIF
      CASE nMCol == ::nRight
         IF nMRow >= nTop .AND. nMRow <= ::nBottom
            RETURN HTRIGHT
         ELSE
            RETURN HTNOWHERE
         ENDIF
      ENDCASE
   ENDIF

   DO CASE
   CASE !::lIsOpen
   CASE nMRow < nTop + nOffset
   CASE nMRow > ::nBottom - nOffset
   CASE nMCol < ::nLeft + nOffset
   CASE nMCol <= ::nRight - nOffset
      RETURN ::nTopItem + nMRow - (nTop + nOffset)
   ENDCASE

   DO CASE
   CASE !::lDropDown
   CASE nMRow != nTop
   CASE nMCol < ::nLeft
   CASE nMCol < ::nRight
      RETURN HTCLIENT
   CASE nMCol == ::nRight
      RETURN HTDROPBUTTON
   ENDCASE

   DO CASE
   CASE Empty(::cCaption)
   CASE nMRow != ::nCapRow
   CASE nMCol < ::nCapCol
   CASE nMCol < ::nCapCol + __CapLength(::cCaption)
      RETURN HTCAPTION
   ENDCASE

   RETURN HTNOWHERE

METHOD ListBox:insItem(nPos, cText, xData)

   IF hb_IsString(cText) .AND. hb_IsNumeric(nPos) .AND. nPos < ::nItemCount

      hb_AIns(::aItems, nPos, {cText, xData}, .T.)
      ::nItemCount++

      IF ::nItemCount == 1
         ::nTopItem := 1
      ENDIF

      IF ::oVScroll != NIL
         ::oVScroll:total := ::nItemCount - (::nBottom - ::nTop - 2)
      ENDIF
   ENDIF

   RETURN Self

METHOD ListBox:killFocus()

   LOCAL nOldMCur

   IF ::lHasFocus
      ::lHasFocus := .F.

      IF hb_IsEvalItem(::bFBlock)
         Eval(::bFBlock)
      ENDIF

      nOldMCur := MSetCursor(.F.)
      DispBegin()

      IF ::lDropDown .AND. ::lIsOpen
         ::close()
      ENDIF
      ::display()

      DispEnd()
      MSetCursor(nOldMCur)

      SetCursor(::nCursor)
   ENDIF

   RETURN Self

METHOD ListBox:nextItem()

   LOCAL nOldValue

   IF ::lHasFocus .AND. ::nItemCount > 0
      ::changeItem(nOldValue := ::nValue, IIf(nOldValue == ::nItemCount, nOldValue, nOldValue + 1))
   ENDIF

   RETURN Self

METHOD ListBox:open()

   IF !::lIsOpen

      ::aSaveScr := {::nTop + 1, ::nLeft, ::nBottom, ::nRight, SaveScreen(::nTop + 1, ::nLeft, ::nBottom, ::nRight)}

      ::lIsOpen := .T.
      ::display()
   ENDIF

   RETURN Self

METHOD ListBox:prevItem()

   LOCAL nOldValue

   IF ::lHasFocus .AND. ::nItemCount > 0

      IF (nOldValue := ::nValue) == 0
         ::changeItem(nOldValue, 1)
      ELSEIF nOldValue > 1
         ::changeItem(nOldValue, nOldValue - 1)
      ENDIF
   ENDIF

   RETURN Self

METHOD ListBox:scroll(nMethod)

   LOCAL nPos
   LOCAL nTopItem
   LOCAL nItemCount
   LOCAL nThumbPos
   LOCAL nCurrent
   LOCAL nBarLength
   LOCAL nTotal
   LOCAL nSize
   LOCAL nMRow
   LOCAL nPrevMRow
   LOCAL nKey
   LOCAL nCount

   IF hb_IsNumeric(nMethod)

      SWITCH nMethod
      CASE HTSCROLLTHUMBDRAG

         nPrevMRow := MRow()

         DO WHILE ((nKey := Inkey(0)) != K_LBUTTONUP)

            IF nKey == K_MOUSEMOVE

               nMRow := MRow()

               IF nMRow <= ::oVScroll:start()
                  nMRow := ::oVScroll:start() + 1
               ENDIF
               IF nMRow >= ::oVScroll:end()
                  nMRow := ::oVScroll:end() - 1
               ENDIF

               IF nMRow != nPrevMRow
                  nThumbPos  := ::oVScroll:thumbPos() + (nMRow - nPrevMRow)
                  nBarLength := ::oVScroll:barLength()
                  nTotal     := ::oVScroll:total()
                  nSize      := Min(Max((nThumbPos * (nTotal - nBarLength - 2) + 2 * nBarLength + 1 - nTotal) / (nBarLength - 1), 1), nTotal)
                  nCurrent   := ::oVScroll:current()
                  IF nSize - nCurrent > 0
                     FOR nCount := 1 TO nSize - nCurrent
                        ::scroll(HTSCROLLUNITINC)
                     NEXT
                  ELSE
                     FOR nCount := 1 TO nCurrent - nSize
                        ::scroll(HTSCROLLUNITDEC)
                     NEXT
                  ENDIF

                  nPrevMRow := nMRow
               ENDIF
            ENDIF
         ENDDO
         EXIT

      CASE HTSCROLLUNITDEC

         IF ::nTopItem > 1
            ::nTopItem--
            ::oVScroll:current := ::scrollbarPos()
            ::display()
         ENDIF
         EXIT

      CASE HTSCROLLUNITINC

         IF (::nTopItem + ::nBottom - ::nTop) <= ::nItemCount + 1
            ::nTopItem++
            ::oVScroll:current := ::scrollbarPos()
            ::display()
         ENDIF
         EXIT

      CASE HTSCROLLBLOCKDEC

         nPos     := ::nBottom - ::nTop - IIf(::lDropDown, 2, 1)
         nTopItem := ::nTopItem - nPos
         IF ::nTopItem > 1
            ::nTopItem := Max(nTopItem, 1)
            ::oVScroll:current := ::scrollbarPos()
            ::display()
         ENDIF
         EXIT

      CASE HTSCROLLBLOCKINC

         nPos       := ::nBottom - ::nTop - 1
         nItemCount := ::nItemCount
         nTopItem   := ::nTopItem + nPos
         IF ::nTopItem < nItemCount - nPos + 1
            IF nTopItem + nPos - 1 > nItemCount
               nTopItem := nItemCount - nPos + 1
            ENDIF
            ::nTopItem := nTopItem
            ::oVScroll:current := ::scrollbarPos()
            ::display()
         ENDIF
         EXIT

      ENDSWITCH
   ENDIF

   RETURN Self

METHOD ListBox:select(xPos)

   LOCAL nValue
   LOCAL nPos
   LOCAL cType := ValType(xPos)

   DO CASE
   CASE cType == "C"
      nPos := ::findData(xPos)
      IF !ValType(::xBuffer) $ "CU"
         ::xBuffer := nPos
      ELSEIF ::nValue == 0
         ::xBuffer := xPos
      ELSE
         ::xBuffer := _LISTBOX_ITEMDATA(::aItems[nPos])
      ENDIF
   CASE !cType == "N"
      RETURN ::nValue
   CASE xPos < 1
      RETURN ::nValue
   CASE xPos > ::nItemCount
      RETURN ::nValue
   CASE xPos == ::nValue
      RETURN ::nValue
   OTHERWISE
      nPos := xPos
      IF ValType(::xBuffer) $ "NU"
         ::xBuffer := nPos
      ELSEIF nPos == 0
         ::xBuffer := ""
      ELSE
         ::xBuffer := _LISTBOX_ITEMDATA(::aItems[nPos])
      ENDIF
   ENDCASE
   ::nValue := nPos

   ::cTextValue := IIf(nPos == 0, "", _LISTBOX_ITEMDATA(::aItems[nPos]))

   nValue := ::nValue - (::nBottom - ::nTop - IIf(Empty(::cHotBox + ::cColdBox), 0, 2))
   IF ::nTopItem <= nValue
      ::nTopItem := nValue
      IF ::oVScroll != NIL
         ::oVScroll:current := ::scrollbarPos()
      ENDIF
   ELSEIF ::nValue != 0 .AND. ::nTopItem > ::nValue
      ::nTopItem := ::nValue
      IF ::oVScroll != NIL
         ::oVScroll:current := ::scrollbarPos()
      ENDIF
   ENDIF

   ::display()

   IF hb_IsEvalItem(::bSBlock)
      Eval(::bSBlock)
   ENDIF

   RETURN ::nValue

/* NOTE: This function does nothing in Cl*pper, due to a bug. */
METHOD ListBox:setData(nPos, xData)

   IF nPos >= 1 .AND. nPos <= ::nItemCount
      ::aItems[nPos][_ITEM_xData] := xData
   ENDIF

   RETURN Self

METHOD ListBox:setFocus()

   IF !::lHasFocus

      ::nCursor := SetCursor(SC_NONE)
      ::lHasFocus := .T.

      ::display()

      IF hb_IsEvalItem(::bFBlock)
         Eval(::bFBlock)
      ENDIF
   ENDIF

   RETURN Self

METHOD ListBox:setItem(nPos, aItem)

   IF nPos >= 1 .AND. nPos <= ::nItemCount .AND. Len(aItem) == _ITEM_xData .AND. hb_IsString(aItem[_ITEM_cText])

      ::aItems[nPos] := aItem
   ENDIF

   RETURN Self

METHOD ListBox:setText(nPos, cText)

   IF nPos >= 1 .AND. nPos <= ::nItemCount
      ::aItems[nPos][_ITEM_cText] := cText
   ENDIF

   RETURN Self

/* --- */

METHOD ListBox:changeItem(nOldPos, nNewPos)

   LOCAL nValue

   IF nOldPos != nNewPos

      ::nValue := nNewPos
      ::cTextValue := IIf(::nValue == 0, "", _LISTBOX_ITEMDATA(::aItems[::nValue]))

      IF ::xBuffer == NIL
      ELSEIF hb_IsNumeric(::xBuffer)
         ::xBuffer := ::nValue
      ELSEIF ::nValue > 0
         ::xBuffer := ::cTextValue
      ENDIF

      IF ::nTopItem > ::nValue
         ::nTopItem := ::nValue
         IF ::oVScroll != NIL
            ::oVScroll:current := ::scrollbarPos()
         ENDIF
      ELSE
         nValue := ::nValue - (::nBottom - ::nTop - (IIf(Empty(::cHotBox + ::cColdBox), 0, 2) + IIf(::lDropDown, 1, 0)))

         IF ::nTopItem <= nValue
            ::nTopItem := nValue
            IF ::oVScroll != NIL
               ::oVScroll:current := ::scrollbarPos()
            ENDIF
         ENDIF
      ENDIF

      ::display()

      IF hb_IsEvalItem(::bSBlock)
         Eval(::bSBlock)
      ENDIF
   ENDIF

   RETURN Self

METHOD ListBox:scrollbarPos()

   LOCAL nSize   := ::nBottom - ::nTop - IIf(::lDropDown, 2, 1)
   LOCAL nCount  := ::nItemCount
   LOCAL nLength := ::oVScroll:barLength

#ifndef HB_CLP_STRICT
   /* NOTE: Cl*pper will RTE with non default division by zero error handler */
   IF nCount == nSize
      RETURN 0
   ENDIF
#endif
   RETURN ((nCount - nLength) * ::nTopItem + nLength - nSize) / (nCount - nSize)

/* --- */

METHOD ListBox:bitmap(cBitmap)

   IF cBitmap != NIL .AND. ::lDropDown
      ::cBitmap := __eInstVar53(Self, "BITMAP", cBitmap, "C", 1001)
   ENDIF

   RETURN ::cBitmap

METHOD ListBox:bottom(nBottom)

   IF nBottom != NIL
      ::nBottom := __eInstVar53(Self, "BOTTOM", nBottom, "N", 1001)
      IF ::oVScroll != NIL
         ::oVScroll:end := ::nBottom - 1
      ENDIF
   ENDIF

   RETURN ::nBottom

METHOD ListBox:buffer()
   RETURN ::xBuffer

METHOD ListBox:capCol(nCapCol)

   IF nCapCol != NIL
      ::nCapCol := __eInstVar53(Self, "CAPCOL", nCapCol, "N", 1001)
   ENDIF

   RETURN ::nCapCol

METHOD ListBox:capRow(nCapRow)

   IF nCapRow != NIL
      ::nCapRow := __eInstVar53(Self, "CAPROW", nCapRow, "N", 1001)
   ENDIF

   RETURN ::nCapRow

METHOD ListBox:caption(cCaption)

   IF cCaption != NIL
      ::cCaption := __eInstVar53(Self, "CAPTION", cCaption, "C", 1001)
      IF ::nCapCol == NIL
         ::nCapRow := ::nTop
         ::nCapCol := ::nLeft - Len(::cCaption)
      ENDIF
   ENDIF

   RETURN ::cCaption

METHOD ListBox:coldBox(cColdBox)

   IF cColdBox != NIL
      ::cColdBox := __eInstVar53(Self, "COLDBOX", cColdBox, "C", 1001, {||Len(cColdBox) == 0 .OR. Len(cColdBox) == 8})
   ENDIF

   RETURN ::cColdBox

METHOD ListBox:colorSpec(cColorSpec)

   IF cColorSpec != NIL
      ::cColorSpec := __eInstVar53(Self, "COLORSPEC", cColorSpec, "C", 1001, ;
         IIf(::lDropDown, ;
            {||!Empty(hb_ColorIndex(cColorSpec, 7)) .AND. Empty(hb_ColorIndex(cColorSpec, 8))}, ;
            {||!Empty(hb_ColorIndex(cColorSpec, 6)) .AND. Empty(hb_ColorIndex(cColorSpec, 7))}))
   ENDIF

   RETURN ::cColorSpec

METHOD ListBox:dropDown(lDropDown)

   IF lDropDown != NIL

      ::lDropDown := __eInstVar53(Self, "DROPDOWN", lDropDown, "L", 1001)

      IF !::lDropDown .AND. !::lIsOpen
         ::lIsOpen := .T.
      ENDIF

      ::display()
   ENDIF

   RETURN ::lDropDown

METHOD ListBox:fBlock(bFBlock)

   IF PCount() > 0
      ::bFBlock := IIf(bFBlock == NIL, NIL, __eInstVar53(Self, "FBLOCK", bFBlock, "B", 1001))
   ENDIF

   RETURN ::bFBlock

METHOD ListBox:hasFocus()
   RETURN ::lHasFocus

METHOD ListBox:hotBox(cHotBox)

   IF cHotBox != NIL
      ::cHotBox := __eInstVar53(Self, "HOTBOX", cHotBox, "C", 1001, {||Len(cHotBox) == 0 .OR. Len(cHotBox) == 8})
   ENDIF

   RETURN ::cHotBox

METHOD ListBox:isOpen()
   RETURN ::lIsOpen

METHOD ListBox:itemCount()
   RETURN ::nItemCount

METHOD ListBox:left(nLeft)

   IF nLeft != NIL
      ::nLeft := __eInstVar53(Self, "LEFT", nLeft, "N", 1001)
   ENDIF

   RETURN ::nLeft

METHOD ListBox:message(cMessage)

   IF cMessage != NIL
      ::cMessage := __eInstVar53(Self, "MESSAGE", cMessage, "C", 1001)
   ENDIF

   RETURN ::cMessage

METHOD ListBox:right(nRight)

   IF nRight != NIL
      ::nRight := __eInstVar53(Self, "RIGHT", nRight, "N", 1001)
      IF ::oVScroll != NIL
         ::oVScroll:offset := ::nRight
      ENDIF
   ENDIF

   RETURN ::nRight

METHOD ListBox:sBlock(bSBlock)

   IF PCount() > 0
      ::bSBlock := IIf(bSBlock == NIL, NIL, __eInstVar53(Self, "SBLOCK", bSBlock, "B", 1001))
   ENDIF

   RETURN ::bSBlock

METHOD ListBox:style(cStyle)

   IF cStyle != NIL
      ::cStyle := __eInstVar53(Self, "STYLE", cStyle, "C", 1001, {||Len(cStyle) == 1})
   ENDIF

   RETURN ::cStyle

METHOD ListBox:textValue()
   RETURN ::cTextValue

METHOD ListBox:top(nTop)

   IF nTop != NIL
      ::nTop := __eInstVar53(Self, "TOP", nTop, "N", 1001)
      IF ::oVScroll != NIL
         ::oVScroll:start := ::nTop + 1
      ENDIF
   ENDIF

   RETURN ::nTop

METHOD ListBox:topItem(nTopItem)

   IF nTopItem != NIL

      __eInstVar53(Self, "TOPITEM", nTopItem, "N", 1001, {||nTopItem > 0 .AND. nTopItem <= ::nItemCount})

      nTopItem := Min(nTopItem, ::nItemCount - (::nBottom - ::nTop - IIf(Empty(::cHotBox + ::cColdBox), 0, 2)))

      IF ::nTopItem != nTopItem
         ::nTopItem := nTopItem

         IF ::oVScroll != NIL
            ::oVScroll:current := ::scrollbarPos()
         ENDIF

         ::display()
      ENDIF
   ENDIF

   RETURN ::nTopItem

METHOD ListBox:typeOut()
   RETURN ::nItemCount == 0

METHOD ListBox:value()
   RETURN ::nValue

METHOD ListBox:vScroll(oVScroll)

   IF PCount() > 0
      IF oVScroll == NIL
         ::oVScroll := NIL
      ELSE
         ::oVScroll := __eInstVar53(Self, "VSCROLL", oVScroll, "O", 1001, {||oVScroll:ClassName() == "SCROLLBAR" .AND. oVScroll:orient == SCROLL_VERTICAL})
         ::oVScroll:total := ::nItemCount
      ENDIF
   ENDIF

   RETURN ::oVScroll

/* --- */

METHOD ListBox:Init(nTop, nLeft, nBottom, nRight, lDropDown)

   LOCAL cColor

   IF !hb_IsNumeric(nTop) .OR. !hb_IsNumeric(nLeft) .OR. !hb_IsNumeric(nBottom) .OR. !hb_IsNumeric(nRight)
      RETURN NIL
   ENDIF

   hb_default(@lDropDown, .F.)

   ::nBottom   := nBottom
   ::nRight    := nRight
   ::nTop      := nTop
   ::nLeft     := nLeft
   ::nCapCol   := nLeft
   ::nCapRow   := nTop
   ::lIsOpen   := !lDropDown
   ::lDropDown := lDropDown
   ::aSaveScr  := {nTop + 1, nleft, nBottom, nRight, SaveScreen(nTop + 1, nLeft, nBottom, nRight)}

   IF IsDefColor()
      ::cColorSpec := "W/N,W+/N,W+/N,N/W,W/N,W/N,W+/N,W/N"
   ELSE
      cColor := SetColor()
      ::cColorSpec := ;
         hb_ColorIndex(cColor, CLR_UNSELECTED) + "," + ;
         hb_ColorIndex(cColor, CLR_UNSELECTED) + "," + ;
         hb_ColorIndex(cColor, CLR_UNSELECTED) + "," + ;
         hb_ColorIndex(cColor, CLR_ENHANCED) + "," + ;
         hb_ColorIndex(cColor, CLR_BORDER) + "," + ;
         hb_ColorIndex(cColor, CLR_STANDARD) + "," + ;
         hb_ColorIndex(cColor, CLR_BACKGROUND)
   ENDIF

   RETURN Self

FUNCTION ListBox(nTop, nLeft, nBottom, nRight, lDropDown)
   RETURN HBListBox():New(nTop, nLeft, nBottom, nRight, lDropDown)

FUNCTION _ListBox_(nTop, nLeft, nBottom, nRight, xPos, aItems, cCaption, cMessage, cColorSpec, bFBlock, bSBlock, lDropDown, lScrollBar, cBitmap)

   LOCAL o
   LOCAL xItem

   IF (o := HBListBox():New(nTop, nLeft, nBottom, nRight, lDropDown)) != NIL

      IF hb_IsString(cCaption)
         o:caption := cCaption
         o:capCol  := nLeft - __CapLength(cCaption)
      ENDIF

      o:colorSpec := cColorSpec
      o:message   := cMessage
      o:fBlock    := bFBlock
      o:sBlock    := bSBlock

      FOR EACH xItem IN aItems
         DO CASE
         CASE !hb_IsArray(xItem)
            o:addItem(xItem)
         CASE Len(xItem) == _ITEM_cText
            o:addItem(xItem[_ITEM_cText])
#ifdef HB_CLP_STRICT
         OTHERWISE  /* Cl*pper will RTE on empty subarray */
#else
         CASE Len(xItem) >= _ITEM_xData
#endif
            o:addItem(xItem[_ITEM_cText], xItem[_ITEM_xData])
         ENDCASE
      NEXT

      IF hb_defaultValue(lScrollBar, .F.)
         IF hb_defaultValue(lDropDown, .F.)
            nTop++
         ENDIF
         o:VScroll := ScrollBar(nTop + 1, nBottom - 1, nRight)
      ENDIF

      IF hb_IsString(cBitmap)
         o:bitmap := cBitmap
      ENDIF

      o:select(xPos)
   ENDIF

   RETURN o

#endif
