//
// POPUP menu class (Harbour extended)
//
// Copyright 2011 Viktor Szakats (vszakats.net/harbour)
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

#ifdef HB_COMPAT_C53

CREATE CLASS hb_PopupMenu INHERIT HBPopupMenu

   EXPORTED:

   METHOD shadowed(lShadowed) SETGET

   METHOD setCoors(nRow, nCol, lTop)
   METHOD isShortCut(nKey, /* @ */ nID)
   METHOD isQuick(nKey, /* @ */ nID)

ENDCLASS

METHOD hb_PopupMenu:shadowed(lShadowed)

   IF lShadowed != NIL
      ::lShadowed := __eInstVar53(Self, "SHADOWED", lShadowed, "L", 1001)
   ENDIF

   RETURN ::lShadowed

METHOD hb_PopupMenu:setCoors(nRow, nCol, lTop)

   Local oItem
   Local nDif

   ::setMetrics()

   IF ::nTop == -1 .OR. ::nLeft == -1
      ::nTop    := nRow
      ::nLeft   := nCol
      ::nBottom := ::nTop + ::nItemCount + 1
      ::nRight  := ::nLeft + ::nWidth - 1

      IF ::nRight > MaxCol()
         nDif     := ::nRight - MaxCol()
         ::nRight -= nDif
         ::nLeft  -= nDif
         IF !lTop
            ::nTop++
            ::nBottom++
         ENDIF
      ENDIF

      IF ::nLeft < 0
         nDif     := ::nLeft
         ::nRight -= nDif
         ::nLeft  -= nDif
      ENDIF

      IF ::nBottom > MaxRow()
         nDif      := ::nBottom - MaxRow()
         ::nBottom -= nDif
         ::nTop    -= nDif
      ENDIF

      IF ::nTop < 0
         nDif      := ::nTop
         ::nBottom -= nDif
         ::nTop    -= nDif
      ENDIF

      For Each oItem In ::aItems
         IF oItem:isPopup()
            oItem:data:setCoors(nRow + oItem:__enumIndex(), ::nRight + 1, False)
         ENDIF
      Next
   ENDIF

   Return Self

METHOD hb_PopupMenu:isShortCut(nKey, /* @ */ nID)

   Local nItem
   Local nTotal
   Local nShortCut
   Local oItem
   Local i

   Do Case
   // Test and assign top menu item shortCut, enabled, and !PopUp:
   // Changed by enclosing assignment before ':Enabled':
   Case (nShortCut := ::getShortCt(nKey)) > 0 .AND. (oItem := ::getItem(nShortcut)):enabled .AND. !oItem:isPopUp()

      ::select(nShortCut)
      Eval(oItem:data, oItem)
      nID := oItem:id

      Return True

   // Test and assignment for TopBar MenuItem:
   Case nShortCut == 0
      nTotal := ::nItemCount
      nItem  := ::nCurrent
      IF nItem == 0
         nItem := 1
      ENDIF

      // Loop to wrap around through TopMenu from Current Item:
      FOR i := 1 TO nTotal
         IF (oItem := ::getItem(nItem)):enabled .AND. oItem:isPopUp() .AND. oItem:data:isQuick(nKey, @nID)
            Return True
         ENDIF
         IF ++nItem > nTotal
            nItem := 1
         ENDIF
      NEXT

   EndCase

   Return False

METHOD hb_PopupMenu:isQuick(nKey, /* @ */ nID)

   Local nShortCut
   Local oItem

   IF (nShortCut := ::getShortCt(nKey)) == 0

      For Each oItem In ::aItems
         IF oItem:Enabled .AND. oItem:isPopUp() .AND. oItem:data:isQuick(nKey, @nID)
            Return True
         ENDIF
      Next

   ELSEIF !(oItem := ::getItem(nShortCut)):IsPopUp()

      IF oItem:enabled
         ::select(nShortCut)
         Eval(oItem:data, oItem)
         nID := oItem:id
         Return True
      ENDIF

   ENDIF

   Return False

#endif
