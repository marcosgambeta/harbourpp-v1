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

// Class WvtProgressBar
CREATE CLASS WvtProgressBar INHERIT WvtObject

   VAR    cImage
   VAR    nDirection                              INIT 0      // 0-Left-Right,Top-Bottom  1-Right-Left,Bottom-Top
   VAR    nStyle                                  INIT 0
   VAR    lVertical                               INIT .F.
   VAR    lActive                                 INIT .F.

   VAR    nBarColor                               INIT WIN_RGB(0, 0, 128)
   VAR    nCurrent                                INIT 0
   VAR    nTotal                                  INIT 1
   VAR    nPercent                                INIT 0
   VAR    cBackColor                              INIT "W/W"

   VAR    cScreen

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD display( nCurrent, nTotal )
   METHOD Activate()
   METHOD DeActivate()

ENDCLASS

METHOD WvtProgressBar:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_PROGRESSBAR, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD WvtProgressBar:Create()

   __defaultNIL( @::nTop, 0 )
   __defaultNIL( @::nLeft, 0 )
   __defaultNIL( @::nBottom, IIf(::lVertical, ::nTop + 9, ::nTop) )
   __defaultNIL( @::nRight, IIf(::lVertical, ::nLeft + 1, ::nLeft + 19) )
   __defaultNIL( @::nTextColor, WIN_RGB(255, 255, 255) )
   __defaultNIL( @::nBackColor, WIN_RGB(198, 198, 198) )

   ::bPaint := {|| ::Display() }
   AAdd(::aPaint, { ::bPaint, { WVT_BLOCK_LABEL, ::nTop, ::nLeft, ::nBottom, ::nRight } })

   ::Super:Create()

   RETURN Self

METHOD WvtProgressBar:Display( nCurrent, nTotal )

   IF !::lActive
      RETURN Self
   ENDIF

   __defaultNIL( @nCurrent, ::nCurrent )
   __defaultNIL( @nTotal, ::nTotal )

   ::nCurrent := nCurrent
   ::nTotal   := nTotal

   IF ::nCurrent > ::nTotal
      ::nCurrent := ::nTotal
   ENDIF

   ::nPercent := Int( ::nCurrent / ::nTotal * 100 )

   wvt_DrawProgressBar( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlTLBR, ::nPercent, ;
      ::nBackColor, ::nBarColor, ::cImage, ::lVertical, ::nDirection )

   RETURN Self

METHOD WvtProgressBar:Activate()

   ::cScreen := SaveScreen( ::nTop, ::nLeft, ::nBottom, ::nRight )
   hb_DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, "         ", ::cBackColor )
   ::lActive := .T.

   RETURN Self

METHOD WvtProgressBar:DeActivate()

   ::lActive  := .F.
   ::nCurrent := 0
   ::nTotal   := 1
   RestScreen( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cScreen )
   ::cScreen := NIL

   RETURN Self
