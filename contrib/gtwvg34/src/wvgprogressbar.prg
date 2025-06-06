//
// Progressbar Class
//
// Copyright 2016 José M. C. Quintas
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
#include <inkey.ch>
#include <hbgtinfo.ch>

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgProgressBar INHERIT WvgWindow

   VAR    autosize         INIT .F.
   VAR    Border           INIT .F.
   VAR    cancel           INIT .F.
   VAR    caption
   VAR    default          INIT .F.
   VAR    drawMode         INIT WVG_DRAW_NORMAL
   VAR    preSelect        INIT .F.
   VAR    pointerFocus     INIT .F.
   VAR    Style            INIT 0
   VAR    lVertical        INIT .F.
   VAR    lSmooth          INIT .F.
   VAR    nValue
   VAR    nRangeMin
   VAR    nRangeMax
   VAR    nVelocity
   VAR    lMarquee         INIT .F.
   VAR    nSpeed           INIT 30
   VAR    nColorBarFG
   VAR    nColorBarBG

   METHOD SetColorBarFG( nColor )
   METHOD SetColorBarBG( nColor )
   METHOD SetValue( nValue, nRangeMin, nRangeMax, nSpeed )

   METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD destroy()
   METHOD handleEvent( nMessage, aNM )

   METHOD activate( xParam )      SETGET
   METHOD setCaption( cCaption )
   METHOD draw( xParam )          SETGET

   METHOD setColorFG()            INLINE NIL
   METHOD setColorBG()            INLINE NIL

ENDCLASS

METHOD WvgProgressBar:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

// https://msdn.microsoft.com/library/bb760820
// Smooth style is supported only in the Windows Classic theme.
//
// https://msdn.microsoft.com/library/bb760838
// When visual styles are enabled, color messages has no effect.

METHOD WvgProgressBar:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::ClassName := "msctls_progress32"
   ::ObjType   := objTypeStatic
   ::Style     += WIN_WS_CHILD + WIN_WS_GROUP + ;
      IIf(::lMarquee, PBS_MARQUEE, 0) + ;
      IIf(::lVertical, PBS_VERTICAL, 0) + ;
      IIf(::lSmooth, PBS_SMOOTH, 0) + WIN_WS_EX_CLIENTEDGE

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oParent:AddChild(Self)

   ::createControl()

   ::SetValue( ::nValue, ::nRangeMin, ::nRangeMax, ::nSpeed )
   IF hb_IsNumeric(::nColorBarFG)
      ::SetColorBarFG( ::nColorBarFG )
   ENDIF
   IF hb_IsNumeric(::nColorBarBG)
      ::SetColorBarGB(::nColorBarBG)
   ENDIF

   IF ::visible
      ::show()
   ENDIF
   ::setPosAndSize()

   RETURN Self

METHOD WvgProgressBar:handleEvent( nMessage, aNM )

   DO CASE
   CASE nMessage == HB_GTE_RESIZED

      IF ::isParentCrt()
         ::rePosition()
      ENDIF
      ::sendMessage( WIN_WM_SIZE, 0, 0 )
      IF hb_IsEvalItem(::sl_resize)
         Eval(::sl_resize,,, Self)
      ENDIF

   CASE nMessage == HB_GTE_NOTIFY
      // do nothing

   CASE nMessage == HB_GTE_CTLCOLOR

      IF hb_IsNumeric(::clr_FG)
         wapi_SetTextColor(aNM[1], ::clr_FG)
      ENDIF
      IF !Empty(::hBrushBG)
         wapi_SetBkMode( aNM[1], WIN_TRANSPARENT )
         RETURN ::hBrushBG
      ENDIF

   ENDCASE

   RETURN EVENT_UNHANDLED

METHOD PROCEDURE WvgProgressBar:destroy()

   ::wvgWindow:destroy()

   RETURN

METHOD WvgProgressBar:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

METHOD PROCEDURE WvgProgressBar:setCaption( cCaption )

   IF hb_IsString(cCaption)
      ::Caption := cCaption
   ENDIF
   IF hb_IsString(::Caption)
      ::sendMessage( WIN_WM_SETTEXT, 0, ::Caption )
   ENDIF

   RETURN

METHOD WvgProgressBar:draw( xParam )

   IF hb_IsEvalItem(xParam) .OR. xParam == NIL
      ::sl_paint := xParam
   ENDIF

   RETURN Self

METHOD WvgProgressBar:activate( xParam )

   IF hb_IsEvalItem(xParam) .OR. xParam == NIL
      ::sl_lbClick := xParam
   ENDIF

   RETURN Self

METHOD WvgProgressBar:SetValue( nValue, nRangeMin, nRangeMax, nSpeed )

   IF hb_IsNumeric(nRangeMin) .AND. hb_IsNumeric(nRangeMax) .AND. !::lMarquee
      ::nRangeMin := nRangeMin
      ::nRangeMax := nRangeMax
      ::sendMessage( PBM_SETRANGE, 0, WIN_MAKELONG( ::nRangeMin, ::nRangeMax ) )
   ENDIF
   IF hb_IsNumeric(nValue) .AND. !::lMarquee
      ::sendMessage( PBM_SETPOS, nValue, 0 )
      ::nValue := nValue
   ENDIF
   IF hb_IsNumeric(nSpeed) .AND. ::lMarquee
      ::sendMessage( PBM_SETMARQUEE, 1, nSpeed )
      ::nSpeed := nSpeed
   ENDIF

   RETURN ::sendMessage( PBM_GETPOS, 0, 0 )

METHOD PROCEDURE WvgProgressBar:SetCOlorBarFG( nColor )

   IF hb_IsNumeric(nColor)
      ::sendMessage( PBM_SETBARCOLOR, 0, nColor )
   ENDIF

   RETURN

METHOD PROCEDURE WvgProgressBar:SetColorBarBG( nColor )

   IF hb_IsNumeric(nColor)
      ::sendMessage( PBM_SETBKCOLOR, 0, nColor )
   ENDIF

   RETURN
