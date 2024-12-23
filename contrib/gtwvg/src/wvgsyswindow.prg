//
// Xbase++ Compatible xbpPartHandler Class
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

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgSysWindow INHERIT WvgPartHandler

   METHOD new(oParent, oOwner, aPos)
   METHOD create(oParent, oOwner, aPos)
   METHOD configure()
   METHOD destroy()

   METHOD disable()
   METHOD enable()
   METHOD hide()
   METHOD show()
   METHOD SetPos(aPos)

   METHOD currentPos()
   METHOD currentSize()

   VAR aPos INIT {0, 0}

   VAR hWnd PROTECTED
   VAR nOldProc PROTECTED
   VAR nWndProc PROTECTED


   VAR sl_helpRequest
   ACCESS helpRequest INLINE ::sl_helpRequest
   ASSIGN helpRequest(bBlock) INLINE ::sl_helpRequest := bBlock

   VAR sl_move
   ACCESS move INLINE ::sl_move
   ASSIGN move(bBlock) INLINE ::sl_move := bBlock

   VAR sl_quit
   ACCESS quit INLINE ::sl_quit
   ASSIGN quit(bBlock) INLINE ::sl_quit := bBlock

ENDCLASS

METHOD WvgSysWindow:new(oParent, oOwner, aPos)

   __defaultNIL(@oParent, ::oParent)
   __defaultNIL(@oOwner, ::oOwner)
   __defaultNIL(@aPos, ::aPos)

   ::oParent := oParent
   ::oOwner := oOwner
   ::aPos := aPos

   ::WvgPartHandler:new(oParent, oOwner)

   RETURN Self

METHOD WvgSysWindow:create(oParent, oOwner, aPos)

   __defaultNIL(@oParent, ::oParent)
   __defaultNIL(@oOwner, ::oOwner)
   __defaultNIL(@aPos, ::aPos)

   ::oParent := oParent
   ::oOwner := oOwner
   ::aPos := aPos

   ::WvgPartHandler:create(oParent, oOwner)

   RETURN Self

METHOD WvgSysWindow:configure()
   RETURN Self

METHOD WvgSysWindow:destroy()
   RETURN Self

METHOD WvgSysWindow:disable()
   RETURN Self

METHOD WvgSysWindow:enable()
   RETURN Self

METHOD WvgSysWindow:hide()
   RETURN Self

METHOD WvgSysWindow:show()
   RETURN Self

METHOD WvgSysWindow:SetPos(aPos)

   wvg_SetWindowPosition(::hWnd, aPos[1], aPos[2], .F.)

   RETURN Self

METHOD WvgSysWindow:currentPos()

   LOCAL aRect

   aRect := wvg_GetWindowRect(::hWnd)

   RETURN {aRect[1], aRect[2]}

METHOD WvgSysWindow:currentSize()

   LOCAL aRect

   aRect := wvg_GetClientRect(::hWnd)

   RETURN {aRect[3] - aRect[1], aRect[4] - aRect[2]}
