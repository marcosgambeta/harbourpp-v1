/*
 * Xbase++ xbpStatusBar Compatible Class
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

/*                               EkOnkar
 *                         ( The LORD is ONE )
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

/* WvgToolBarButton() Class compatible with XbpToolbarButton() */
CREATE CLASS WvgStatusBarPanel

   VAR    alignment                             INIT WVGALIGN_LEFT
   VAR    autosize                              INIT WVGSTATUSBAR_AUTOSIZE_NONE
   VAR    bevel                                 INIT WVGSTATUSBAR_BEVEL_INSET
   VAR    enabled                               INIT .T.
   VAR    index                                 INIT 0
   VAR    key                                   INIT ""
   VAR    style                                 INIT WVGSTATUSBAR_PANEL_TEXT
   VAR    sl_caption                            INIT ""
   VAR    image                                 INIT NIL
   VAR    tooltipText                           INIT ""
   VAR    visible                               INIT .T.
   VAR    left                                  INIT 0
   VAR    width                                 INIT 0
   VAR    minWidth                              INIT 0

   METHOD new( cCaption, nStyle, cKey )
   METHOD caption( cCaption )                   SETGET

   VAR    oParent

ENDCLASS

METHOD WvgStatusBarPanel:new( cCaption, nStyle, cKey )

   __defaultNIL( @cCaption, ::sl_caption )
   __defaultNIL( @nStyle, ::style )
   __defaultNIL( @cKey, ::key )

   ::sl_caption     := cCaption
   ::style          := nStyle
   ::key            := cKey

   RETURN Self

METHOD WvgStatusBarPanel:caption( cCaption )

   IF cCaption == NIL
      RETURN ::sl_caption
   ELSE
      __defaultNIL( @cCaption, ::sl_caption )

      ::sl_caption := cCaption

      wvg_StatusBarSetText( ::oParent:hWnd, ::index, cCaption )
   ENDIF

   RETURN Self
