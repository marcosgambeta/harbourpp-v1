//
// Wvt*Classes
//
// Copyright 2007-2012 Pritpal Bedi <bedipritpal@hotmail.com>
// Based On:
// Video subsystem for Windows using GUI windows instead of Console
//     Copyright 2003 Peter Rees <peter@rees.co.nz>
//                    Rees Software and Systems Ltd
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

#include <hbclass.ch>
#include <hbgtinfo.ch>
#include <inkey.ch>
#include <setcurs.ch>

#include "wvtwin.ch"

// Class WvtMenu [Peter Rees]
CREATE CLASS wvtMenu

   METHOD create( cCaption )
   METHOD AddItem( cCaption, bAction )
   METHOD DelAllItems()
   METHOD DelItem( nItemNum )
   METHOD EnableItem( nItemNum )
   METHOD DisableItem( nItemNum )
   METHOD NumItems()
   METHOD Destroy()
   METHOD GetItem( nItemNum )
   METHOD FindMenuItemById(nId)
   METHOD DrawMenuBar()

   CLASS VAR MenuItemId                            INIT 1

   VAR    aItems
   VAR    hMenu
   VAR    Caption
   VAR    IdNumber

ENDCLASS

METHOD wvtMenu:Create( cCaption )

   ::aItems := {}

   IF Empty(::hMenu := wapi_CreateMenu())
#if 0
      Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:Init()", "Create Menu Error", { cCaption, cCaption } ) )
#endif
   ENDIF
   ::Caption := IIf(cCaption == NIL, "", cCaption)

   RETURN Self

METHOD wvtMenu:Destroy()

   IF !Empty(::hMenu)
      ::DelAllItems()

      IF !wapi_DestroyMenu( ::hMenu )
#if 0
         Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:Destroy()", "Destroy menu FAILED", {} ) )
#endif
      ENDIF
      ::hMenu := 0
   ENDIF

   RETURN .T.

METHOD wvtMenu:AddItem( cCaption, bAction )

   LOCAL aItem

   IF !Empty(::hMenu) .AND. ( !Empty(cCaption) .OR. !Empty(bAction) )
      IF hb_IsObject(bAction)
         cCaption := IIf(Empty(cCaption), bAction:Caption, cCaption)
         aItem := { WIN_MF_POPUP, bAction:hMenu, cCaption, bAction }   // bAction is a wvtMenu object reference
      ELSEIF hb_IsEvalItem(bAction)
         aItem := { WIN_MF_STRING, ::MenuItemId++, cCaption, bAction } // bAction is a code block to execute
      ELSEIF hb_LeftEq( cCaption, "-" )
         aItem := { WIN_MF_SEPARATOR, 0, 0, NIL }
      ELSE
#if 0
         Throw( ErrorNew( "wvtMenu", 3101, "wvtMenu:AddItem()", "Argument Error", { cCaption, bAction } ) )
#endif
      ENDIF

      IF !wapi_AppendMenu( ::hMenu, aItem[WVT_MENU_TYPE], aItem[WVT_MENU_IDENTIFIER], aItem[WVT_MENU_CAPTION] )
#if 0
         Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:AddItem()", "Add menu item", { cCaption, bAction } ) )
#endif
      ENDIF

      AAdd(::aItems, aItem)

      RETURN .T.
   ENDIF

   RETURN .F.

METHOD wvtMenu:DelAllItems()

   LOCAL lResult := .T., nItems

   nItems := ::NumItems()
   DO WHILE nItems > 0 .AND. lResult
      lResult := ::DelItem( nItems )
      nItems--
   ENDDO

   RETURN lResult

METHOD wvtMenu:DelItem( nItemNum )

   LOCAL lResult := .F.

   IF nItemNum >= 1 .AND. nItemNum <= ::NumItems()
      IF ::aItems[nItemNum][WVT_MENU_TYPE] == WIN_MF_POPUP
         ::aItems[nItemNum][WVT_MENU_MENUOBJ]:Destroy()
      ENDIF

      IF ( lResult := wapi_DeleteMenu( ::hMenu, nItemNum - 1, WIN_MF_BYPOSITION ) ) // Remember ZERO base
         hb_ADel( ::aItems, nItemNum, .T. )
      ELSE
#if 0
         Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:DelItem()", "Delete menu item FAILED", { nItemNum } ) )
#endif
      ENDIF
   ENDIF

   RETURN lResult

METHOD wvtMenu:EnableItem( nItemNum )

   IF !Empty(::hMenu) .AND. hb_IsNumeric(nItemNum) .AND. nItemNum >= 1
      RETURN wapi_EnableMenuItem( ::hMenu, nItemNum - 1, WIN_MF_BYPOSITION + WIN_MF_ENABLED )
   ENDIF

   RETURN -1

METHOD wvtMenu:DisableItem( nItemNum )

   IF !Empty(::hMenu) .AND. hb_IsNumeric(nItemNum) .AND. nItemNum >= 1
      RETURN wapi_EnableMenuItem( ::hMenu, nItemNum - 1, WIN_MF_BYPOSITION + WIN_MF_GRAYED )
   ENDIF

   RETURN -1

METHOD wvtMenu:NumItems()
   RETURN Len(::aItems)

METHOD wvtMenu:GetItem( nItemNum )

   LOCAL nItems := ::NumItems(), aResult := NIL

   IF nItemNum >= 1 .AND. nItemNum <= nItems
      aResult := ::aItems[nItemNum]
   ENDIF

   RETURN aResult

METHOD wvtMenu:FindMenuItemById(nId)

   LOCAL x, aResult := {}

   IF !Empty(nId)
      x := ::NumItems()
      DO WHILE x > 0 .AND. Empty(aResult)
         IF ::aItems[x][WVT_MENU_TYPE] == WIN_MF_POPUP
            aResult := ::aItems[x][WVT_MENU_MENUOBJ]:FindMenuItemById(nId)
         ELSEIF ::aItems[x][WVT_MENU_IDENTIFIER] == nId
            aResult := ::aItems[x]
         ENDIF
         x--
      ENDDO
   ENDIF

   RETURN aResult

METHOD PROCEDURE wvtMenu:DrawMenuBar()

   wvt_DrawMenuBar()

   RETURN
