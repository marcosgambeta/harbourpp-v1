//
// Xbase++ xbpStatusBar Compatible Class
//
// Copyright 2008-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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

//                               EkOnkar
//                         ( The LORD is ONE )

#include <hbclass.ch>
#include <inkey.ch>
#include <hbgtinfo.ch>

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgStatusBar INHERIT WvgWindow // WvgActiveXControl

   VAR caption INIT ""
   VAR sizeGrip INIT .T.

   VAR aItems INIT {}

   METHOD numItems() INLINE Len(::aItems)

   METHOD new(oParent, oOwner, aPos, aSize, aPresParams, lVisible)
   METHOD create(oParent, oOwner, aPos, aSize, aPresParams, lVisible)
   METHOD configure(oParent, oOwner, aPos, aSize, aPresParams, lVisible)
   METHOD destroy()
   METHOD handleEvent(nMessage, aNM)

   METHOD addItem(cCaption, xImage, cDLL, nStyle, cKey, nMode)
   METHOD delItem(nItemORcKey)
   METHOD getItem(nItemORcKey)
   METHOD clear()
   METHOD panelClick(xParam) SETGET
   METHOD panelDblClick(xParam) SETGET

ENDCLASS

METHOD WvgStatusBar:new(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   ::wvgWindow:new(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   ::style := WS_CHILD + WS_BORDER + SBARS_TOOLTIPS
   ::className := STATUSCLASSNAME
   ::objType := objTypeStatusBar

   RETURN Self

METHOD WvgStatusBar:create(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   ::wvgWindow:create(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   IF ::sizeGrip
      ::style += SBARS_SIZEGRIP
   ENDIF

   ::oParent:AddChild(Self)

   ::createControl()

   ::SetWindowProcCallback()

   IF ::visible
      ::show()
   ENDIF

   ::addItem(, , , , , -1)

   RETURN Self

METHOD WvgStatusBar:handleEvent(nMessage, aNM)

   LOCAL nHandled := 1
   LOCAL nObj
   LOCAL aNMH

   SWITCH nMessage

   CASE HB_GTE_RESIZED
      ::sendMessage(WM_SIZE, 0, 0)
      RETURN 0

   CASE HB_GTE_COMMAND
      IF hb_IsBlock(::sl_lbClick)
         Eval(::sl_lbClick, , , Self)
         RETURN 0
      ENDIF
      EXIT

   CASE HB_GTE_NOTIFY
      aNMH := wvg_GetNMMouseInfo(aNM[2])

      DO CASE
      CASE aNMH[NMH_code] == NM_CLICK

         IF hb_IsBlock(::sl_lbClick)
            IF aNMH[NMH_dwItemSpec] >= 0
               nObj := aNMH[NMH_dwItemSpec] + 1

               Eval(::sl_lbClick, ::aItems[nObj], , Self)
            ENDIF

            nHandled := 0
         ENDIF

      ENDCASE
      EXIT

   CASE HB_GTE_CTLCOLOR
      IF hb_IsNumeric(::clr_FG)
         wvg_SetTextColor(aNM[1], ::clr_FG)
      ENDIF
      IF hb_IsNumeric(::hBrushBG)
         wvg_SetBkMode(aNM[1], 1)
         RETURN ::hBrushBG
      ELSE
         RETURN wvg_GetCurrentBrush(aNM[1])
      ENDIF

   ENDSWITCH

   RETURN nHandled

METHOD PROCEDURE WvgStatusBar:destroy()

   LOCAL i
   LOCAL nItems

   IF (nItems := Len(::aItems)) > 0
      FOR i := 1 TO nItems

      NEXT
   ENDIF

   ::wvgWindow:destroy()

   RETURN

METHOD WvgStatusBar:configure(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   ::Initialize(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   RETURN Self

METHOD WvgStatusBar:addItem(cCaption, xImage, cDLL, nStyle, cKey, nMode)

   LOCAL oPanel
   LOCAL lSuccess

   __defaultNIL(@nMode, 0)

   HB_SYMBOL_UNUSED(xImage)
   HB_SYMBOL_UNUSED(cDLL)

   oPanel := WvgStatusBarPanel():new(cCaption, nStyle, cKey)
   oPanel:oParent := Self

   oPanel:index := ::numItems + 1

   lSuccess := wvg_StatusBarCreatePanel(::hWnd, nMode)

   IF lSuccess
      AAdd(::aItems, oPanel)
   ELSE
      RETURN NIL
   ENDIF

   RETURN oPanel

METHOD WvgStatusBar:delItem(nItemORcKey)

   LOCAL nIndex := 0

   DO CASE
   CASE hb_IsNumeric(nItemORcKey)
      nIndex := AScan(::aItems, {|o|o:key == nItemORcKey})
   CASE hb_IsNumeric(nItemORcKey)
      nIndex := nItemORcKey
   ENDCASE

   IF nIndex > 0
      // Delete panel by window
      hb_ADel(::aItems, nIndex, .T.)
   ENDIF

   RETURN Self

METHOD WvgStatusBar:getItem(nItemORcKey)

   LOCAL nIndex := 0
   LOCAL oPanel

   DO CASE
   CASE hb_IsString(nItemORcKey)
      nIndex := AScan(::aItems, {|o|o:key == nItemORcKey})
   CASE hb_IsNumeric(nItemORcKey)
      nIndex := nItemORcKey
   ENDCASE

   IF nIndex > 0
      oPanel := ::aItems[nIndex]
   ENDIF

   RETURN oPanel

METHOD WvgStatusBar:clear()

   LOCAL i

   FOR i := 1 TO ::numItems
      // Remove off window
   NEXT

   ::aItems := {}

   RETURN Self

METHOD WvgStatusBar:panelClick(xParam)

   IF hb_IsBlock(xParam) .OR. xParam == NIL
      ::sl_lbClick := xParam
   ENDIF

   RETURN Self

METHOD WvgStatusBar:panelDblClick(xParam)

   IF hb_IsBlock(xParam) .OR. xParam == NIL
      ::sl_lbDblClick := xParam
   ENDIF

   RETURN Self
