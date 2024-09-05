//
// Wvt*Classes
//
// Copyright 2007-2012 Pritpal Bedi <bedipritpal@hotmail.com>
// Based On:
// Video subsystem for Windows using GUI windows instead of Console
//     Copyright 2003 Peter Rees <peter@rees.co.nz>
//                    Rees Software and Systems Ltd
//

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

#include "hbclass.ch"
#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

#include "wvtwin.ch"

// Class WvtToolButton
CREATE CLASS WvtToolButton INHERIT WvtObject

   VAR    cFileImage
   VAR    nCurState             INIT 0
   VAR    nBtnType              INIT TLB_BUTTON_TYPE_IMAGE
   VAR    aPxlOffSet            INIT { 0, -1, -3, 1 }

   METHOD New( oParent )
   METHOD create()
   METHOD Refresh()
   METHOD LeftDown()
   METHOD LeftUp()
   METHOD HoverOn()
   METHOD HoverOff()
   METHOD PaintButton()

ENDCLASS

METHOD WvtToolButton:New( oParent )

   ::Super:New( oParent, DLG_OBJ_BUTTON )

   RETURN Self

METHOD WvtToolButton:Create()

   ::bPaint := {|| ::PaintButton() }
   AAdd( ::aPaint, { ::bPaint, ;
      { WVT_BLOCK_BUTTON, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   ::Super:Create()

   RETURN Self

METHOD WvtToolButton:Refresh()

   IF ::lActive
      Eval( ::bPaint )
   ENDIF

   RETURN Self

METHOD WvtToolButton:PaintButton()

   IF ::lActive
      IF ::nBtnType == TLB_BUTTON_TYPE_IMAGE
         wvt_DrawImage( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cFileImage, { 4, 4, -6, -4 } )
      ELSE
         wvt_DrawLine( ::nTop, ::nLeft, ::nBottom, ::nRight, 1, 1, , , , ::oParent:nRGBSep )
      ENDIF
   ENDIF

   RETURN Self

METHOD WvtToolButton:LeftDown()

   LOCAL lRet := .F.

   IF ::lActive .AND. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet, 2 )
      lRet := .T.
   ENDIF

   RETURN lRet

METHOD WvtToolButton:LeftUp()

   IF ::lActive .AND. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet, 1 )
      Eval( ::bOnLeftUp )
      RETURN .T.
   ENDIF

   RETURN .F.

METHOD WvtToolButton:HoverOn()

   ::oParent:HoverOn()

   IF ::lActive .AND. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet, 1 )
   ENDIF

   RETURN Self

METHOD WvtToolButton:HoverOff()

   ::oParent:HoverOff()

   IF ::lActive .AND. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet, 0 )
   ENDIF

   RETURN Self
