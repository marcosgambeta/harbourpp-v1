//
// TopBar menu class
//
// Copyright 2000 Jose Lalin <dezac@corevia.com>
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
#include "inkey.ch"

// NOTE: Harbour doesn't support CA-Cl*pper 5.3 GUI functionality, but
//       it has all related variables and methods.

#ifdef HB_COMPAT_C53

CREATE CLASS TopBarMenu FUNCTION HBTopBarMenu

   EXPORTED:

   VAR cargo

   METHOD addItem(oItem)
   METHOD delItem(nPos)
   METHOD display()
   METHOD getFirst()
   METHOD getItem(nPos)
   METHOD getLast()
   METHOD getNext()
   METHOD getPrev()
   METHOD getAccel(nKey)
   METHOD getShortCt(nKey)                        // NOTE: This method exists but it is not documented in the manuals nor the NG's [jlalin]
   METHOD hitTest(nMRow, nMCol)
   METHOD insItem(nPos, oItem)
   METHOD select(nPos)
   METHOD setItem(nPos, oItem)

   METHOD colorSpec(cColorSpec) SETGET
   METHOD current() SETGET
   METHOD itemCount() SETGET
   METHOD left(nLeft) SETGET
   METHOD right(nRight) SETGET
   METHOD row(nRow) SETGET

   METHOD Init(nRow, nLeft, nRight)                // NOTE: This method is a Harbour extension [vszakats]

   PROTECTED:

   VAR cColorSpec
   VAR nCurrent   INIT 0
   VAR nItemCount INIT 0
   VAR nLeft
   VAR nRight
   VAR nRow

   VAR aItems     INIT {}
   VAR nWidth     INIT 0

ENDCLASS

METHOD TopBarMenu:addItem(oItem)

   IF hb_IsObject(oItem) .AND. oItem:ClassName() == "MENUITEM"

      ::nItemCount++
      AAdd(::aItems, oItem)

      ::nWidth := Max(__CapMetrics(oItem), ::nWidth)
   ENDIF

   RETURN Self

METHOD TopBarMenu:delItem(nPos)

   LOCAL nLen
   LOCAL nWidth
   LOCAL item

   IF nPos >= 1 .AND. nPos <= ::nItemCount

      nLen := Len(::aItems[nPos]:caption)

      hb_ADel(::aItems, nPos, .T.)
      ::nItemCount--

      IF ::nWidth == nLen + 2
         nWidth := 0
         FOR EACH item IN ::aItems
            nWidth := Max(__CapMetrics(item), nWidth)
         NEXT
         ::nWidth := nWidth
      ENDIF
   ENDIF

   RETURN Self

METHOD TopBarMenu:display()

   LOCAL nRow := ::nRow
   LOCAL nLeft := ::nLeft
   LOCAL nRight := ::nRight
   LOCAL nCurrent := ::nCurrent
   LOCAL item
   LOCAL cColor1 := hb_ColorIndex(::cColorSpec, 0)
   LOCAL cColor2 := hb_ColorIndex(::cColorSpec, 1)
   LOCAL oPopUp
   LOCAL cCaption
   LOCAL nCaptionLen
   LOCAL nPos

   DispBegin()

   hb_DispOutAt(nRow, nLeft, Space(nRight - nLeft + 1), cColor1)

   FOR EACH item IN ::aItems

      cCaption := " " + RTrim(item:caption) + " "
      nCaptionLen := Len(cCaption)

      IF nCaptionLen == 0
         LOOP
      ENDIF

      IF nLeft > nRight - nCaptionLen
         nLeft := nRight - nCaptionLen
      ENDIF

      item:__row := nRow
      item:__col := nLeft

      IF item:isPopUp()
         oPopUp := item:data
         oPopUp:top := nRow + 1
         oPopUp:left := nLeft
         oPopUp:bottom := NIL
         oPopUp:right := NIL
      ENDIF

      IF (nPos := At("&", cCaption)) > 0
         IF nPos == Len(cCaption)
            nPos := 0
         ELSE
            cCaption := Stuff(cCaption, nPos, 1, "")
            nCaptionLen--
         ENDIF
      ENDIF

      hb_DispOutAt(nRow, nLeft, cCaption, IIf(item:__enumIndex() == nCurrent, cColor2, IIf(item:enabled, cColor1, hb_ColorIndex(::cColorSpec, 4))))

      IF item:enabled .AND. nPos > 0
         hb_DispOutAt(nRow, nLeft + nPos - 1, SubStr(cCaption, nPos, 1), IIf(item:__enumIndex() == nCurrent, hb_ColorIndex(::cColorSpec, 3), hb_ColorIndex(::cColorSpec, 2)))
      ENDIF

      nLeft += nCaptionLen
   NEXT

   IF nCurrent != 0 .AND. ::aItems[nCurrent]:isPopUp() .AND. ::aItems[nCurrent]:data:isOpen

      ::aItems[nCurrent]:data:display()
   ENDIF

   DispEnd()

   RETURN Self

METHOD TopBarMenu:getFirst()

   LOCAL item

   FOR EACH item IN ::aItems
      IF item:enabled
         RETURN item:__enumIndex()
      ENDIF
   NEXT

   RETURN 0

METHOD TopBarMenu:getItem(nPos)
   RETURN IIf(nPos >= 1 .AND. nPos <= ::nItemCount, ::aItems[nPos], NIL)

METHOD TopBarMenu:getLast()

   LOCAL item

   FOR EACH item IN ::aItems DESCEND
      IF item:enabled
         RETURN item:__enumIndex()
      ENDIF
   NEXT

   RETURN 0

METHOD TopBarMenu:getNext()

   LOCAL n

   FOR n := ::nCurrent + 1 TO ::nItemCount
      IF ::aItems[n]:enabled
         RETURN n
      ENDIF
   NEXT

   RETURN 0

METHOD TopBarMenu:getPrev()

   LOCAL n

   FOR n := ::nCurrent - 1 TO 1 STEP -1
      IF ::aItems[n]:enabled
         RETURN n
      ENDIF
   NEXT

   RETURN 0

// NOTE: This method corrects two bugs in Cl*pper:
//       1) when two menuitems have the same key and the
//          first item is disabled
//       2) when a menuitem is disabled it will ignore the key [jlalin]

METHOD TopBarMenu:getAccel(nKey)

   LOCAL nIndex := AScan({ ;
      K_ALT_A, K_ALT_B, K_ALT_C, K_ALT_D, K_ALT_E, K_ALT_F, ;
      K_ALT_G, K_ALT_H, K_ALT_I, K_ALT_J, K_ALT_K, K_ALT_L, ;
      K_ALT_M, K_ALT_N, K_ALT_O, K_ALT_P, K_ALT_Q, K_ALT_R, ;
      K_ALT_S, K_ALT_T, K_ALT_U, K_ALT_V, K_ALT_W, K_ALT_X, ;
      K_ALT_Y, K_ALT_Z, K_ALT_1, K_ALT_2, K_ALT_3, K_ALT_4, ;
      K_ALT_5, K_ALT_6, K_ALT_7, K_ALT_8, K_ALT_9, K_ALT_0}, nKey)
   LOCAL cKey
   LOCAL item

   IF nIndex > 0
      cKey := "&" + SubStr("ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890", nIndex, 1)
      FOR EACH item IN ::aItems
         IF hb_AtI(cKey, item:caption) > 0
            RETURN item:__enumIndex()
         ENDIF
      NEXT
   ENDIF

   RETURN 0

METHOD TopBarMenu:getShortCt(nKey)

   LOCAL item

   FOR EACH item IN ::aItems
      IF item:shortcut == nKey
         RETURN item:__enumIndex()
      ENDIF
   NEXT

   RETURN 0

// NOTE: In my tests I can't get other values than HTNOWHERE or a value
//       greather than 0 (selected item), althought the NG's says that
//       it returns other HT* values [jlalin]
//
//       This method correct a bug in Cl*pper:
//       when click on a disabled menuitem it will ignore it [jlalin]

METHOD TopBarMenu:hitTest(nMRow, nMCol)

   LOCAL nColumn
   LOCAL item

   IF nMRow == ::nRow

      FOR EACH item IN ::aItems

         nColumn := item:__col

         IF nMCol >= nColumn .AND. nMCol <= nColumn + Len(item:caption)
            RETURN item:__enumIndex()
         ENDIF
      NEXT
   ENDIF

   RETURN HTNOWHERE

METHOD TopBarMenu:insItem(nPos, oItem)

   IF nPos >= 1 .AND. nPos <= ::nItemCount .AND. hb_IsObject(oItem) .AND. oItem:ClassName() == "MENUITEM"

      hb_AIns(::aItems, nPos, oItem, .T.)
      ::nItemCount++

      ::nWidth := Max(__CapMetrics(oItem), ::nWidth)
   ENDIF

   RETURN Self

METHOD TopBarMenu:select(nPos)

   IF (nPos >= 1 .AND. nPos <= ::nItemCount .AND. ::nCurrent != nPos .AND. ::aItems[nPos]:enabled) .OR. nPos == 0

#if 0
      IF ::isOpen() .AND. ::nCurrent > 0 .AND. ::aItems[::nCurrent]:isPopUp()

         ::aItems[::nCurrent]:data:close()
      ENDIF
#endif

      ::nCurrent := nPos
   ENDIF

   RETURN Self

METHOD TopBarMenu:setItem(nPos, oItem)

   IF nPos >= 1 .AND. nPos <= ::nItemCount .AND. hb_IsObject(oItem) .AND. oItem:ClassName() == "MENUITEM"

      ::aItems[nPos] := oItem

      ::nWidth := Max(__CapMetrics(oItem), ::nWidth)
   ENDIF

   RETURN Self

METHOD TopBarMenu:colorSpec(cColorSpec)

   IF cColorSpec != NIL
      ::cColorSpec := __eInstVar53(Self, "COLORSPEC", cColorSpec, "C", 1001, {||!Empty(hb_ColorIndex(cColorSpec, 5)) .AND. Empty(hb_ColorIndex(cColorSpec, 6))})

   ENDIF

   RETURN ::cColorSpec

METHOD TopBarMenu:current()
   RETURN ::nCurrent

METHOD TopBarMenu:itemCount()
   RETURN ::nItemCount

METHOD TopBarMenu:left(nLeft)

   IF nLeft != NIL
      ::nLeft := __eInstVar53(Self, "LEFT", nLeft, "N", 1001)
   ENDIF

   RETURN ::nLeft

METHOD TopBarMenu:right(nRight)

   IF nRight != NIL
      ::nRight := __eInstVar53(Self, "RIGHT", nRight, "N", 1001)
   ENDIF

   RETURN ::nRight

METHOD TopBarMenu:row(nRow)

   IF nRow != NIL
      // NOTE: CA-Cl*pper 5.3 has a bug, where it would show "TOP" in case of an error.
      ::nRow := __eInstVar53(Self, "ROW", nRow, "N", 1001)
   ENDIF

   RETURN ::nRow

// --------------------------------------------

METHOD TopBarMenu:Init(nRow, nLeft, nRight)

   LOCAL cColor

   IF !hb_IsNumeric(nRow) .OR. !hb_IsNumeric(nLeft) .OR. !hb_IsNumeric(nRight)
      RETURN NIL
   ENDIF

   ::nLeft  := nLeft
   ::nRight := nRight
   ::nRow   := nRow

   IF IsDefColor()
      ::cColorSpec := "N/W,W/N,W+/W,W+/N,N+/W,W/N"
   ELSE
      cColor := SetColor()
      ::cColorSpec := ;
         hb_ColorIndex(cColor, CLR_UNSELECTED) + "," + ;
         hb_ColorIndex(cColor, CLR_ENHANCED) + "," + ;
         hb_ColorIndex(cColor, CLR_BACKGROUND) + "," + ;
         hb_ColorIndex(cColor, CLR_ENHANCED) + "," + ;
         hb_ColorIndex(cColor, CLR_STANDARD) + "," + ;
         hb_ColorIndex(cColor, CLR_BORDER)
   ENDIF

   RETURN Self

// --------------------------------------------

FUNCTION TopBar(nRow, nLeft, nRight)
   RETURN HBTopBarMenu():New(nRow, nLeft, nRight)

#endif
