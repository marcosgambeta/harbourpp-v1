/*
 * Wvt*Classes
 *
 * Copyright 2007-2012 Pritpal Bedi <bedipritpal@hotmail.com>
 * Based On:
 * Video subsystem for Windows using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software and Systems Ltd
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

#include "hbclass.ch"
#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

#include "wvtwin.ch"

/* Class WvtTextBox */
CREATE CLASS WvtTextBox INHERIT WvtObject

   VAR    cText                                   INIT ""

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD Configure()
   METHOD Refresh()
   METHOD SetText( cText )
   METHOD HoverOn()
   METHOD HoverOff()

ENDCLASS

METHOD WvtTextBox:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_TEXTBOX, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD WvtTextBox:Create()

   ::nTextColorHoverOff := ::nTextColor

   ::hFont := wvt_CreateFont( ::cFont, ::nFontHeight, ::nFontWidth, ;
      ::nFontWeight, ::lItalic, ::lUnderline, ::lStrikeout, ;
      ::nCharSet, ::nFontQuality, 0 )

   IF ! Empty(::hFont)
      ::bPaint := {|| wvt_DrawTextBox( ::nTop, ::nLeft, ::nBottom, ::nRight, ;
         ::aPxlTLBR, ::cText, ::nAlignHorz, ::nAlignVert, ;
         ::nTextColor, ::nBackColor, ::nBackMode, ::hFont ) }

      AAdd( ::aPaint, { ::bPaint, { WVT_BLOCK_LABEL, ::nTop, ::nLeft, ::nBottom, ::nRight } } )
   ENDIF

   ::Super:Create()

   RETURN Self

METHOD WvtTextBox:Refresh()

   Eval( ::bPaint )

   RETURN Self

METHOD WvtTextBox:Configure()
   RETURN Self

METHOD WvtTextBox:SetText( cText )

   IF cText != NIL
      ::cText := cText
      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtTextBox:HoverOn( /* cText */ )

   IF ::nTextColorHoverOn != NIL
      ::nTextColor := ::nTextColorHoverOn
      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtTextBox:HoverOff( /* cText */ )

   IF ::nTextColorHoverOn != NIL
      ::nTextColor := ::nTextColorHoverOff
      ::Refresh()
   ENDIF

   RETURN Self
