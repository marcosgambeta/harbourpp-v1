/*
 * Xbase++ Compatible xbpMenuBar Class
 *
 * Copyright 2008-2012 Pritpal Bedi <bedipritpal@hotmail.com>
 *
 */

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

/*                               EkOnkar
 *                         ( The LORD is ONE )
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

/* Xbase++ compatible xbpMenu class */
CREATE CLASS WvgMenu INHERIT WvgMenuBar

   VAR title INIT ""

   METHOD new(oParent, aPresParams, lVisible)
   METHOD create(oParent, aPresParams, lVisible)

   METHOD getTitle()
   METHOD setTitle(cTitle)
   METHOD Popup(oXbp, aPos, nDefaultItem, nControl)

ENDCLASS

METHOD WvgMenu:new(oParent, aPresParams, lVisible)

   __defaultNIL(@oParent, ::oParent)
   __defaultNIL(@aPresParams, ::aPresParams)
   __defaultNIL(@lVisible, ::visible)

   ::oParent := oParent
   ::aPresParams := aPresParams
   ::visible := lVisible

   RETURN Self

METHOD WvgMenu:create(oParent, aPresParams, lVisible)

   __defaultNIL(@oParent, ::oParent)
   __defaultNIL(@aPresParams, ::aPresParams)
   __defaultNIL(@lVisible, ::visible)

   ::oParent := oParent
   ::aPresParams := aPresParams
   ::visible := lVisible

   ::className := "POPUPMENU"

   ::hMenu := wvg_CreatePopupMenu()

   RETURN Self

METHOD WvgMenu:getTitle()
   RETURN ::title

METHOD WvgMenu:setTitle(cTitle)
   RETURN ::title := cTitle

METHOD WvgMenu:Popup(oXbp, aPos, nDefaultItem, nControl)

   LOCAL nCmd
   LOCAL aMenuItem

   HB_SYMBOL_UNUSED(nDefaultItem)
   HB_SYMBOL_UNUSED(nControl)

   nCmd := wvg_TrackPopupMenu(::hMenu, TPM_LEFTALIGN + TPM_TOPALIGN + TPM_RETURNCMD, aPos[1], aPos[2], oXbp:hWnd)

   aMenuItem := ::findMenuItemById(nCmd)
   IF HB_ISARRAY(aMenuItem) .AND. HB_ISBLOCK(aMenuItem[2])
      Eval(aMenuItem[2], aMenuItem[1], , aMenuItem[4])
   ENDIF

   RETURN 0
