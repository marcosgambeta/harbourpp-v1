//
// Xbase++ Compatible xbpMenuBar Class
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

CREATE CLASS WvgMenuBar INHERIT wvgWindow

   CLASS VAR nMenuItemID INIT 0

   VAR hMenu
   VAR pMenu

   // Event CallBack Slots
   VAR sl_beginMenu
   VAR sl_endMenu
   VAR sl_itemMarked
   VAR sl_itemSelected
   VAR sl_drawItem
   VAR sl_measureItem
   VAR sl_onMenuKey

   VAR aMenuItems INIT {}

   VAR nPass INIT 0

   VAR caption INIT ""
   VAR nItemID INIT 0
   VAR aIds INIT {}

   VAR className INIT "MENUBAR"

   METHOD numItems() INLINE Len(::aMenuItems)

   METHOD new(oParent, aPresParams, lVisible)
   METHOD create(oParent, aPresParams, lVisible)
   METHOD configure(oParent, aPresParams, lVisible)
   METHOD destroy()
   METHOD delAllItems()
   METHOD delItem(nItemNum)
   METHOD addItem(aItem, p2, p3, p4)
   METHOD findMenuItemById(nId)
   METHOD findMenuPosById(nId)
   METHOD checkItem(nItemNum, lCheck)
   METHOD enableItem(nItemNum)
   METHOD disableItem(nItemNum)

   METHOD getItem(nItemNum)
   METHOD insItem(nItemNum, aItem)
   METHOD isItemChecked(nItemNum)
   METHOD isItemEnabled(nItemNum)
   METHOD selectItem(nItemNum)
   METHOD setItem(nItemNum, aItem)

   // Event Callback Methods
   METHOD beginMenu(xParam) SETGET
   METHOD endMenu(xParam) SETGET
   METHOD itemMarked(xParam) SETGET
   METHOD itemSelected(xParam) SETGET
   METHOD drawItem(xParam) SETGET
   METHOD measureItem(xParam) SETGET
   METHOD onMenuKey(xParam) SETGET

   PROTECTED:
   METHOD putItem(aItem, nPos, lInsert)

ENDCLASS

METHOD WvgMenuBar:new(oParent, aPresParams, lVisible)

   __defaultNIL(@oParent, ::oParent)
   __defaultNIL(@aPresParams, ::aPresParams)
   __defaultNIL(@lVisible, ::visible)

   ::oParent := oParent
   ::aPresParams := aPresParams
   ::visible := lVisible

   ::wvgWindow:new(::oParent, , , , ::aPresParams, ::visible)

   RETURN Self

METHOD WvgMenuBar:create(oParent, aPresParams, lVisible)

   __defaultNIL(@oParent, ::oParent)
   __defaultNIL(@aPresParams, ::aPresParams)
   __defaultNIL(@lVisible, ::visible)

   ::oParent := oParent
   ::aPresParams := aPresParams
   ::visible := lVisible

   ::wvgWindow:create(::oParent, , , , ::aPresParams, ::visible)

   ::hMenu := wvg_CreateMenu()

   IF ::hMenu != 0
      // TODO: check for if the parent already has a menu
      //       we need to destroy that first
      // finally set the menu
#if 0
      wvg_SetMenu(::oParent:getHWND(), ::hMenu)
#endif

#if 0
      // how to make menu invisible ?
      IF ::visible
         wvg_ShowWindow(::oParent:getHWND(), SW_MINIMIZE)
         wvg_ShowWindow(::oParent:getHWND(), SW_NORMAL)
      ENDIF
#endif

      ::oParent:oMenu := Self

      ::pMenu := win_N2P(::hMenu)
   ENDIF

   RETURN Self

METHOD WvgMenuBar:configure(oParent, aPresParams, lVisible)

   __defaultNIL(@oParent, ::oParent)
   __defaultNIL(@aPresParams, ::aPresParams)
   __defaultNIL(@lVisible, ::visible)

   ::oParent := oParent
   ::aPresParams := aPresParams
   ::visible := lVisible

   RETURN Self

METHOD WvgMenuBar:destroy()

   IF !Empty(::hMenu)
      ::DelAllItems()

      IF !wvg_DestroyMenu(::hMenu)
#if 0
         Throw(ErrorNew("wvtMenu", 1000, "wvtMenu:Destroy()", "Destroy menu FAILED", {}))
#endif
      ENDIF

      ::hMenu := 0
   ENDIF

   RETURN .T.

METHOD WvgMenuBar:delAllItems()

   LOCAL lResult := .T.
   LOCAL nItems

   nItems := ::numItems()
   DO WHILE nItems > 0 .AND. lResult
      lResult := ::DelItem(nItems)
      nItems--
   ENDDO

   RETURN lResult

METHOD WvgMenuBar:delItem(nItemNum)

   LOCAL lResult := .F.

   IF nItemNum > 0 .AND. nItemNum <= ::numItems()
      IF ::aMenuItems[nItemNum][WVT_MENU_TYPE] == MF_POPUP
         ::aMenuItems[nItemNum][WVT_MENU_MENUOBJ]:Destroy()
      ENDIF

      IF (lResult := wvg_DeleteMenu(::hMenu, nItemNum - 1, MF_BYPOSITION)) // Remember ZERO base
         hb_ADel(::aMenuItems, nItemNum, .T.)
      ELSE
#if 0
         Throw(ErrorNew("wvtMenu", 1000, "wvtMenu:DelItem()", "Delete menu item FAILED", {nItemNum}))
#endif
      ENDIF
   ENDIF

   RETURN lResult

// {xCaption, bAction, nStyle, nAttrb}
METHOD WvgMenuBar:addItem(aItem, p2, p3, p4)

   LOCAL xCaption
   LOCAL bAction
   LOCAL nStyle
   LOCAL nAttrib

   IF PCount() == 1 .AND. hb_IsArray(aItem)
      ASize(aItem, 4)
      xCaption := aItem[1]
      bAction := aItem[2]
      nStyle := aItem[3]
      nAttrib := aItem[4]
   ELSE
      xCaption := aItem
      bAction := p2
      nStyle := p3
      nAttrib := p4
   ENDIF

   RETURN ::putItem({xCaption, bAction, nStyle, nAttrib}, -1, .T.)

METHOD WvgMenuBar:putItem(aItem, nPos, lInsert)

   LOCAL nItemIndex
   LOCAL cCaption
   LOCAL xCaption
   LOCAL bAction
   LOCAL nStyle
   LOCAL nAttrib

   __defaultNIL(@lInsert, .T.)

   ASize(aItem, 4)

   xCaption := aItem[1]
   bAction := aItem[2]
   nStyle := aItem[3]
   nAttrib := aItem[4]

   // xCaption : NIL | cPrompt | ncResource | oMenu
   SWITCH ValType(xCaption)
   CASE "U"  // Separator
      aItem := {MF_SEPARATOR, 0, 0, , nStyle, nAttrib}
      EXIT

   CASE "C"
      IF Left(xCaption, 1) == "-"
         aItem := {MF_SEPARATOR, 0, 0, , nStyle, nAttrib}
      ELSE
         aItem := {MF_STRING, ++::nMenuItemID, xCaption, bAction, nStyle, nAttrib}
      ENDIF
      EXIT

   CASE "O"
      cCaption := IIf(bAction == NIL, xCaption:title, bAction)
      aItem := {MF_POPUP, xCaption:hMenu, cCaption, xCaption, nStyle, nAttrib}
      EXIT

   CASE "N"  // Resource ID
      EXIT

   ENDSWITCH

   IF nPos <= 0
      AAdd(::aMenuItems, aItem)
      nItemIndex := Len(::aMenuItems)
      wvg_AppendMenu(::hMenu, aItem[1], aItem[2], IIf(hb_IsString(aItem[3]), StrTran(aItem[3], "~", "&"), aItem[3]))
   ELSE
      nItemIndex := nPos
      IF lInsert
         ::aMenuItems := hb_AIns(::aMenuItems, nPos, aItem, .T.)
         wvg_InsertMenu(::hMenu, nItemIndex - 1, aItem[1] + MF_BYPOSITION, aItem[2], IIf(hb_IsString(aItem[3]), StrTran(aItem[3], "~", "&"), aItem[3]))
      ELSE
         IF hb_IsString(xCaption)
            aItem[2] := ::aMenuItems[nItemIndex][2]
         ENDIF
         ::aMenuItems[nItemIndex] := aItem
         wvg_SetMenuItem(::hMenu, nItemIndex - 1, aItem[2], IIf(hb_IsString(aItem[3]), StrTran(aItem[3], "~", "&"), aItem[3]), hb_IsString(xCaption))
      ENDIF
   ENDIF

   IF ++::nPass == 1
      IF ::oParent:className() $ "WVGCRT,WVGDIALOG"
         wvg_SetMenu(::oParent:getHWND(), ::hMenu)
      ENDIF
   ELSE
      IF ::oParent:className() $ "WVGCRT,WVGDIALOG"
         wvg_DrawMenuBar(::oParent:getHWND())
      ENDIF
   ENDIF

   RETURN nItemIndex

METHOD WvgMenuBar:findMenuItemById(nId)

   LOCAL x
   LOCAL aResult := {}

   IF !Empty(nId)
      x := ::numItems()

      DO WHILE x > 0 .AND. Empty(aResult)
         IF ::aMenuItems[x][WVT_MENU_TYPE] == MF_POPUP
            aResult := ::aMenuItems[x][WVT_MENU_MENUOBJ]:findMenuItemById(nId)

         ELSEIF ::aMenuItems[x][WVT_MENU_IDENTIFIER] == nId
            aResult := {x, ::aMenuItems[x][WVT_MENU_ACTION], ::sl_itemSelected, Self}

         ENDIF
         x--
      ENDDO
   ENDIF

   RETURN aResult

METHOD WvgMenuBar:findMenuPosById(nId)

   LOCAL x
   LOCAL nPos

   IF !Empty(nId)
      x := ::numItems()

      DO WHILE x > 0 .AND. Empty(nPos)
         IF ::aMenuItems[x][WVT_MENU_TYPE] == MF_POPUP
            nPos := ::aMenuItems[x][WVT_MENU_MENUOBJ]:findMenuPosById(nId)

         ELSEIF ::aMenuItems[x][WVT_MENU_IDENTIFIER] == nId
            nPos := x

         ENDIF
         x--
      ENDDO
   ENDIF

   RETURN nPos

METHOD WvgMenuBar:checkItem(nItemNum, lCheck)

   LOCAL nRet := -1

   __defaultNIL(@lCheck, .T.)

   IF !Empty(::hMenu) .AND. hb_IsNumeric(nItemNum)
      nRet := wvg_CheckMenuItem(::hMenu, nItemNum - 1, MF_BYPOSITION + IIf(lCheck, MF_CHECKED, MF_UNCHECKED))
   ENDIF

   RETURN IIf(nRet == -1, .F., .T.)

METHOD WvgMenuBar:enableItem(nItemNum)

   LOCAL lSuccess := .F.

   IF !Empty(::hMenu) .AND. hb_IsNumeric(nItemNum)
      lSuccess := wvg_EnableMenuItem(::hMenu, nItemNum - 1, MF_BYPOSITION + MF_ENABLED)
   ENDIF

   RETURN lSuccess

METHOD WvgMenuBar:disableItem(nItemNum)

   LOCAL lSuccess := .F.

   IF !Empty(::hMenu) .AND. !Empty(nItemNum)
      lSuccess := wvg_EnableMenuItem(::hMenu, nItemNum - 1, MF_BYPOSITION + MF_GRAYED)
   ENDIF

   RETURN lSuccess

METHOD WvgMenuBar:getItem(nItemNum)

   IF hb_IsNumeric(nItemNum) .AND. nItemNum > 0 .AND. nItemNum <= Len(::aMenuItems)
      RETURN {::aMenuItems[nItemNum][3], ::aMenuItems[nItemNum][4], ::aMenuItems[nItemNum][5], ::aMenuItems[nItemNum][6]}
   ENDIF

   RETURN NIL

METHOD WvgMenuBar:insItem(nItemNum, aItem)

   ::putItem(aItem, nItemNum, .T.)

   RETURN Self

METHOD WvgMenuBar:isItemChecked(nItemNum)
   RETURN wvg_IsMenuItemChecked(::hMenu, nItemNum - 1)

METHOD WvgMenuBar:isItemEnabled(nItemNum)
   RETURN wvg_IsMenuItemEnabled(::hMenu, nItemNum - 1)

METHOD WvgMenuBar:selectItem(nItemNum)
   RETURN !hb_IsNumeric(nItemNum)

METHOD WvgMenuBar:setItem(nItemNum, aItem)
   RETURN ::putItem(aItem, nItemNum, .F.)

METHOD WvgMenuBar:beginMenu(xParam)

   IF hb_IsBlock(xParam) .OR. xParam == NIL
      ::sl_beginMenu := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgMenuBar:endMenu(xParam)

   IF hb_IsBlock(xParam) .OR. xParam == NIL
      ::sl_endMenu := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgMenuBar:itemMarked(xParam)

   IF hb_IsBlock(xParam) .OR. xParam == NIL
      ::sl_itemMarked := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgMenuBar:itemSelected(xParam)

   IF hb_IsBlock(xParam) .OR. xParam == NIL
      ::sl_itemSelected := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgMenuBar:drawItem(xParam)

   IF hb_IsBlock(xParam) .OR. xParam == NIL
      ::sl_drawItem := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgMenuBar:measureItem(xParam)

   IF hb_IsBlock(xParam) .OR. xParam == NIL
      ::sl_measureItem := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgMenuBar:onMenuKey(xParam)

   IF hb_IsBlock(xParam) .OR. xParam == NIL
      ::sl_onMenuKey := xParam
      RETURN NIL
   ENDIF

   RETURN Self
