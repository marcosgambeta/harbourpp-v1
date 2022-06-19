/*
 * Xbase++ xbpTreeViewItem compatible Class
 *
 * Copyright 2008-2012 Pritpal Bedi <bedipritpal@hotmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*                                EkOnkar
 *                          ( The LORD is ONE )
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgTreeViewItem

   VAR    caption                               INIT ""
   VAR    dllName
   VAR    expandedImage                         INIT -1
   VAR    image                                 INIT -1
   VAR    markedImage                           INIT -1

   VAR    hTree
   VAR    hItem
   VAR    oParent
   VAR    oWnd

   VAR    className                              INIT "TREEVIEWITEM"
   VAR    objType                                INIT objTypeTreeViewItem

   METHOD new()
   METHOD create()
   METHOD configure()
   METHOD destroy()

   METHOD Expand( lExpand ) INLINE wvg_TreeView_Expand( ::hTree, ::hItem, hb_defaultValue( lExpand, .T. ) )
   METHOD isExpanded()
   METHOD setCaption( cCaption )
   METHOD setExpandedImage( nResIdoBitmap )
   METHOD setImage( nResIdoBitmap )
   METHOD setMarkedImage( nResIdoBitmap )

   METHOD addItem( cCaption )
   METHOD delItem()
   METHOD getChildItems()
   METHOD getParentItem()
   METHOD insItem()

ENDCLASS

METHOD WvgTreeViewItem:new()
   RETURN Self

METHOD WvgTreeViewItem:create()
   RETURN Self

METHOD WvgTreeViewItem:configure()
   RETURN Self

METHOD PROCEDURE WvgTreeViewItem:destroy()
   RETURN

METHOD PROCEDURE WvgTreeViewItem:isExpanded()
   RETURN

METHOD PROCEDURE WvgTreeViewItem:setCaption( cCaption )

   HB_SYMBOL_UNUSED( cCaption )

   RETURN

METHOD PROCEDURE WvgTreeViewItem:setExpandedImage( nResIdoBitmap )

   HB_SYMBOL_UNUSED( nResIdoBitmap )

   RETURN

METHOD PROCEDURE WvgTreeViewItem:setImage( nResIdoBitmap )

   HB_SYMBOL_UNUSED( nResIdoBitmap )

   RETURN

METHOD PROCEDURE WvgTreeViewItem:setMarkedImage( nResIdoBitmap )

   HB_SYMBOL_UNUSED( nResIdoBitmap )

   RETURN

METHOD WvgTreeViewItem:addItem( cCaption )

   LOCAL oItem := WvgTreeViewItem():New()

   oItem:hTree   := ::hTree
   oItem:oParent := Self
   oItem:caption := cCaption
   oItem:oWnd    := ::oWnd

   oItem:hItem := wvg_TreeView_AddItem( oItem:hTree, iif( HB_ISOBJECT( oItem:oParent ), oItem:oParent:hItem, ), oItem:caption )

   AAdd( oItem:oWnd:aItems, oItem )

   RETURN oItem

METHOD PROCEDURE WvgTreeViewItem:delItem()
   RETURN

METHOD PROCEDURE WvgTreeViewItem:getChildItems()
   RETURN

METHOD PROCEDURE WvgTreeViewItem:getParentItem()
   RETURN

METHOD PROCEDURE WvgTreeViewItem:insItem()
   RETURN
