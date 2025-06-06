//
// Xbase++ xbpTreeView compatible Class
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

//                                EkOnkar
//                          ( The LORD is ONE )

#include <hbclass.ch>
#include <inkey.ch>
#include <hbgtinfo.ch>

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgListBox INHERIT WvgWindow, WvgDataRef

   VAR adjustHeight INIT .F.
   VAR horizScroll INIT .F.
   VAR markMode INIT WVGLISTBOX_MM_SINGLE
   VAR multiColumn INIT .F.
   VAR vertScroll INIT .T.
   VAR drawMode INIT WVG_DRAW_NORMAL

   METHOD new(oParent, oOwner, aPos, aSize, aPresParams, lVisible)
   METHOD create(oParent, oOwner, aPos, aSize, aPresParams, lVisible)
   METHOD configure(oParent, oOwner, aPos, aSize, aPresParams, lVisible)
   METHOD destroy()

   METHOD handleEvent(nMessage, aNM)

   METHOD getItemHeight() INLINE ::sendMessage(LB_GETITEMHEIGHT, 0, 0)
   METHOD getTopItem() INLINE ::sendMessage(LB_GETTOPINDEX, 0, 0)
   METHOD getVisibleItems() VIRTUAL
   METHOD numItems() INLINE ::sendMessage(LB_GETCOUNT, 0, 0)
   METHOD setItemsHeight(nPixel) INLINE ::sendMessage(LB_SETITEMHEIGHT, 0, nPixel)
   METHOD setTopItem(nIndex) INLINE ::sendMessage(LB_SETTOPINDEX, nIndex - 1, 0)

   METHOD addItem(cItem) INLINE wvg_SendMessageText(::hWnd, LB_ADDSTRING, 0, cItem)
   METHOD clear()
   METHOD delItem(nIndex) INLINE ::sendMessage(LB_DELETESTRING, nIndex - 1, 0)
   METHOD getItem(nIndex) INLINE wvg_lbGetText(::hWnd, nIndex - 1)
   METHOD getTabstops() VIRTUAL
   METHOD insItem(nIndex, cItem) INLINE wvg_SendMessageText(::hWnd, LB_INSERTSTRING, nIndex - 1, cItem)
   METHOD setColumnWidth() VIRTUAL
   METHOD setItem(nIndex, cItem) INLINE ::delItem(nIndex), ::insItem(nIndex, cItem)
   METHOD setTabstops() VIRTUAL


   VAR sl_hScroll
   ACCESS hScroll INLINE ::sl_hScroll
   ASSIGN hScroll(bBlock) INLINE ::sl_hScroll := bBlock

   VAR sl_vScroll
   ACCESS vScroll INLINE ::sl_vScroll
   ASSIGN vScroll(bBlock) INLINE ::sl_vScroll := bBlock

   VAR sl_itemMarked
   ACCESS itemMarked INLINE ::sl_itemMarked
   ASSIGN itemMarked(bBlock) INLINE ::sl_itemMarked := bBlock

   VAR sl_itemSelected
   ACCESS itemSelected INLINE ::sl_itemSelected
   ASSIGN itemSelected(bBlock) INLINE ::sl_itemSelected := bBlock

   VAR sl_drawItem
   ACCESS drawItem INLINE ::sl_drawItem
   ASSIGN drawItem(bBlock) INLINE ::sl_drawItem := bBlock

   VAR sl_measureItem
   ACCESS measureItem INLINE ::sl_measureItem
   ASSIGN measureItem(bBlock) INLINE ::sl_measureItem := bBlock

   VAR nCurSelected INIT 0
   METHOD getCurItem() INLINE ::getItem(::nCurSelected)

ENDCLASS

METHOD WvgListBox:new(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   ::wvgWindow:new(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   ::style := WS_CHILD + WS_OVERLAPPED + WS_TABSTOP + WS_CLIPSIBLINGS + LBS_NOINTEGRALHEIGHT + LBS_WANTKEYBOARDINPUT
   ::exStyle := WS_EX_CLIENTEDGE // + WS_EX_LEFT + WS_EX_LTRREADING + WS_EX_RIGHTSCROLLBAR
   ::className := "LISTBOX"
   ::objType := objTypeListBox

   RETURN Self

METHOD WvgListBox:create(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   ::wvgWindow:create(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   IF ::horizScroll
      ::style += WS_HSCROLL
   ENDIF
   IF ::vertScroll
      ::style += WS_VSCROLL
   ENDIF
   IF ::multiColumn
      ::style += LBS_MULTICOLUMN
   ENDIF
   ::style += LBS_NOTIFY

   ::oParent:AddChild(Self)

   ::createControl()
#if 0
   ::SetWindowProcCallback()   // Let parent handle the notifications otherwise remove LBS_NOTIFY bit
#endif
   IF ::visible
      ::show()
   ENDIF
   ::setPosAndSize()

   RETURN Self

METHOD WvgListBox:handleEvent(nMessage, aNM)

   SWITCH nMessage

   CASE HB_GTE_RESIZED
      IF ::isParentCrt()
         ::rePosition()
      ENDIF
      ::sendMessage(WM_SIZE, 0, 0)
      EXIT

   CASE HB_GTE_COMMAND
      SWITCH aNM[1]
      CASE LBN_SELCHANGE
         ::nCurSelected := wvg_lbGetCurSel(::hWnd) + 1
         IF ::isParentCrt()
            ::oParent:setFocus()
         ENDIF
         IF hb_IsBlock(::sl_itemMarked)
            Eval(::sl_itemMarked, , , Self)
         ENDIF
         IF ::isParentCrt()
            IF ::pointerFocus
               ::setFocus()
            ENDIF
         ENDIF
         EXIT
      CASE LBN_DBLCLK
         ::editBuffer := ::nCurSelected
         IF ::isParentCrt()
            ::oParent:setFocus()
         ENDIF
         IF hb_IsBlock(::sl_itemSelected)
            Eval(::sl_itemSelected, , , Self)
         ENDIF
         IF ::isParentCrt()
            IF ::pointerFocus
               ::setFocus()
            ENDIF
         ENDIF
         EXIT
      CASE LBN_KILLFOCUS
         ::killInputFocus()
         EXIT
      CASE LBN_SETFOCUS
         ::setInputFocus()
      ENDSWITCH
      EXIT

   CASE HB_GTE_KEYTOITEM
      IF aNM[1] == K_ENTER
         IF ::isParentCrt()
            ::oParent:setFocus()
         ENDIF
         IF hb_IsBlock(::sl_itemSelected)
            Eval(::sl_itemSelected, , , Self)
         ENDIF
         IF ::isParentCrt()
            IF ::pointerFocus
               ::setFocus()
            ENDIF
         ENDIF
      ENDIF
      EXIT

   CASE HB_GTE_NOTIFY
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
      EXIT

   CASE HB_GTE_ANY               // This will never be reached
      SWITCH aNM[1]
      CASE WM_LBUTTONUP
         ::nCurSelected := wvg_lbGetCurSel(::hWnd) + 1
         IF hb_IsBlock(::sl_itemMarked)
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            Eval(::sl_itemMarked, , , Self)
            IF ::isParentCrt()
               ::setFocus()
            ENDIF
         ENDIF
         EXIT
      CASE WM_LBUTTONDBLCLK
         ::editBuffer := ::nCurSelected
         IF hb_IsBlock(::sl_itemSelected)
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            Eval(::sl_itemSelected, , , Self)
            IF ::isParentCrt()
               ::setFocus()
            ENDIF
            RETURN EVENT_HANDELLED
         ENDIF
         EXIT
      CASE WM_KEYUP
         IF ::nCurSelected != wvg_lbGetCurSel(::hWnd) + 1
            ::nCurSelected := wvg_lbGetCurSel(::hWnd) + 1
            IF hb_IsBlock(::sl_itemMarked)
               IF ::isParentCrt()
                  ::oParent:setFocus()
               ENDIF
               Eval(::sl_itemMarked, , , Self)
               IF ::isParentCrt()
                  ::setFocus()
               ENDIF
            ENDIF
         ENDIF
      ENDSWITCH

   ENDSWITCH

   RETURN EVENT_UNHANDELLED

METHOD WvgListBox:clear()

   ::sendMessage(LB_RESETCONTENT, 0, 0)

   RETURN Self

METHOD WvgListBox:configure(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   ::Initialize(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   RETURN Self

METHOD PROCEDURE WvgListBox:destroy()

   ::WvgWindow:destroy()

   RETURN
