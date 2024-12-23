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

#include "hbclass.ch"
#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

#include "wvtwin.ch"

// CLASS WvtBanner
CREATE CLASS WvtBanner INHERIT WvtObject

   VAR    nTimeDelay                              INIT 0.5    // One-half Second
   VAR    nDirection                              INIT 0      // LEFT 1-RIGHT
   VAR    nCharToSkip                             INIT 1
   VAR    cText                                   INIT ""
   VAR    cDispText                               INIT ""
   VAR    nTextLen                                INIT 0
   VAR    nTextIndex                              INIT 0

   VAR    oLabel

   VAR    nAlignVert                              INIT 2     // Center

   VAR    nCurSeconds                             INIT 0
   VAR    nCurAlign

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD Configure()
   METHOD Refresh()
   METHOD HoverOn()
   METHOD HoverOff()
   METHOD OnTimer()
   METHOD SetText( cText )
   METHOD Destroy()

ENDCLASS

METHOD WvtBanner:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_BANNER, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD WvtBanner:Create()

   ::cDispText := ::cText

   ::oLabel := WvtLabel():New( ::oParent, , ::nTop, ::nLeft, ::nBottom, ::nRight )
   ::oLabel:Text              := ::cDispText
   ::oLabel:cFont             := ::cFont
   ::oLabel:nFontHeight       := ::nFontHeight
   ::oLabel:nFontWidth        := ::nFontWidth
   ::oLabel:nFontWeight       := ::nFontWeight
   ::oLabel:nFontQuality      := ::nFontQuality
   ::oLabel:lItalic           := ::lItalic
   ::oLabel:lStrikeout        := ::lStrikeout
   ::oLabel:lUnderline        := ::lUnderline
   ::oLabel:nAlignVert        := ::nAlignVert
   ::oLabel:nAlignHorz        := iif(::nDirection == 0, 0, 1)
   ::oLabel:nTextColor        := ::nTextColor
   ::oLabel:nBackColor        := ::nBackColor
   ::oLabel:nTextColorHoverOn := ::nTextColorHoverOn
   ::oLabel:nBackColorHoverOn := ::nBackColorHoverOn

   ::oLabel:Create()

   ::nCurSeconds := Seconds()
   ::nTextLen    := Len(::cText)
   ::nTextIndex  := iif(::nDirection == 0, 1, ::nTextLen)
   ::nCurAlign   := ::nDirection

   ::Super:Create()

   RETURN Self

METHOD WvtBanner:Destroy()

   wvg_DeleteObject( ::oLabel:hFont )

   RETURN NIL

METHOD WvtBanner:Configure()
   RETURN Self

METHOD WvtBanner:OnTimer()

   ::Refresh()

   RETURN Self

METHOD WvtBanner:SetText( cText )

   IF cText != NIL
      ::cText := cText
      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtBanner:Refresh()

   LOCAL nNewTime

   IF Abs( ( nNewTime := Seconds() ) - ::nCurSeconds ) >= ::nTimeDelay
      ::nCurSeconds := nNewTime

      IF ::nDirection == 0
         ::nTextIndex++
         IF ::nTextIndex > ::nTextLen
            ::nTextIndex := 1
            ::nCurAlign  := iif(::nCurAlign == 0, 1, 0)
         ENDIF

         IF ::nCurAlign == 0   // Left
            ::cDispText := SubStr(::cText, ::nTextIndex)
         ELSE                  // Right
            ::cDispText := Left(::cText, ::nTextIndex)
         ENDIF
      ELSE
         ::nTextIndex--
         IF ::nTextIndex < 0
            ::nTextIndex := ::nTextLen
            ::nCurAlign := iif(::nCurAlign == 0, 1, 0)
         ENDIF

         IF ::nCurAlign == 0   // Left
            ::cDispText := SubStr(::cText, ::nTextIndex)
         ELSE                  // Right
            ::cDispText := Left(::cText, ::nTextIndex)
         ENDIF
      ENDIF

      ::oLabel:nAlignHorz := ::nCurAlign
      ::oLabel:SetText( ::cDispText )
      ::oLabel:Refresh()
   ENDIF

   RETURN Self

METHOD WvtBanner:HoverOn()

   ::oLabel:HoverOn()

   RETURN Self

METHOD WvtBanner:HoverOff()

   ::oLabel:HoverOff()

   RETURN Self
