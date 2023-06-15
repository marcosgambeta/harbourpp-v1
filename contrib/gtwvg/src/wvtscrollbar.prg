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

#define K_LBUTTONPRESSED        1021
#define K_RBUTTONPRESSED        1022
#define K_MBUTTONPRESSED        1023

#define K_SBLINEUP              1051
#define K_SBLINEDOWN            1052
#define K_SBPAGEUP              1053
#define K_SBPAGEDOWN            1054

#define K_SBLINELEFT            1055
#define K_SBLINERIGHT           1056
#define K_SBPAGELEFT            1057
#define K_SBPAGERIGHT           1058

#define K_SBTHUMBTRACKVERT      1059
#define K_SBTHUMBTRACKHORZ      1060

#define OBJ_CHILD_OBJ             1
#define OBJ_CHILD_EVENTS          2
#define OBJ_CHILD_DATABLOCK       3
#define OBJ_CHILD_REFRESHBLOCK    4

/* Class WvtScrollBar */
CREATE CLASS WvtScrollBar INHERIT WvtObject

   VAR    nBarType                                INIT WVT_SCROLLBAR_VERT

   VAR    nTotal                                  INIT 100
   VAR    nCurrent                                INIT 1
   VAR    nThumbPos                               INIT 0
   VAR    nBlockNo                                INIT 1

   VAR    nSTop
   VAR    nSLeft
   VAR    nSBottom
   VAR    nSRight

   VAR    nBtn1Top
   VAR    nBtn1Left
   VAR    nBtn1Bottom
   VAR    nBtn1Right

   VAR    nBtn2Top
   VAR    nBtn2Left
   VAR    nBtn2Bottom
   VAR    nBtn2Right
   VAR    bBtnLeftTop
   VAR    bBtnLeftTopDep
   VAR    bBtnRightBottom
   VAR    bBtnRightBottomDep
   VAR    bBtnScroll
   VAR    bTotal
   VAR    bCurrent
   VAR    lHidden                                 INIT .T.

   VAR    aPxlBtnTop                              INIT { 0, 0, 0, 0 }
   VAR    aPxlBtnLft                              INIT { 0, 0, 0, 0 }
   VAR    aPxlBtnBtm                              INIT { 0, 0, 0, 0 }
   VAR    aPxlBtnRgt                              INIT { 0, 0, 0, 0 }
   VAR    aPxlScroll                              INIT { 0, 0, 0, 0 }

   VAR    lLeftDown                               INIT .F.
   VAR    lOnThumb                                INIT .F.
   VAR    lAnchored                               INIT .F.
   VAR    lOnLeftDown                             INIT .F.

   VAR    nScrollUnits                            INIT 0

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD Configure( nTop, nLeft, nBottom, nRight )
   METHOD Refresh()
   METHOD HandleEvent( nKey )
   METHOD SetPos( nTotal, nCurrent )
   METHOD GetPos()
   METHOD ThumbPos()
   METHOD SetTooltip()

ENDCLASS

METHOD wvtScrollbar:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_SCROLLBAR, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD wvtScrollbar:Create()

   IF ::nTop == NIL .OR. ::nLeft == NIL
      RETURN NIL
   ENDIF

   IF ::nBarType == WVT_SCROLLBAR_VERT
      __defaultNIL(@::nBottom, ::nTop + 5)
      __defaultNIL(@::nRight, ::nLeft + 1)

      ::nRight       := ::nLeft + 1
      ::nBottom      := Max( 7, ::nBottom )

      ::nBtn1Top     := ::nTop
      ::nBtn1Left    := ::nLeft
      ::nBtn1Bottom  := ::nTop
      ::nBtn1Right   := ::nRight

      ::nBtn2Top     := ::nBottom
      ::nBtn2Left    := ::nLeft
      ::nBtn2Bottom  := ::nBottom
      ::nBtn2Right   := ::nRight

      ::nSTop        := ::nTop + 1
      ::nSLeft       := ::nLeft
      ::nSBottom     := ::nBottom - 1
      ::nSRight      := ::nRight

      ::nScrollUnits := ::nSBottom - ::nSTop + 1

      ::nTotal       := Eval(::bTotal)
      ::nCurrent     := Eval(::bCurrent)
      ::ThumbPos()

      ::bBtnLeftTop := ;
         {|| wvt_DrawScrollButton( ::nBtn1Top, ::nBtn1Left, ::nBtn1Bottom, ::nBtn1Right, ::aPxlBtnTop, 1 ) }
      ::bBtnRightBottom := ;
         {|| wvt_DrawScrollButton( ::nBtn2Top, ::nBtn2Left, ::nBtn2Bottom, ::nBtn2Right, ::aPxlBtnBtm, 3 ) }
      ::bBtnScroll := ;
         {|| wvt_DrawScrollThumbVert( ::nSTop, ::nSLeft, ::nSBottom, ::nSRight, ::aPxlScroll, ;
         ::nThumbPos ) }
      ::bBtnLeftTopDep := ;
         {|| wvt_DrawScrollButton( ::nBtn1Top, ::nBtn1Left, ::nBtn1Bottom, ::nBtn1Right, ::aPxlBtnTop, 1, .T. ) }
      ::bBtnRightBottomDep := ;
         {|| wvt_DrawScrollButton( ::nBtn2Top, ::nBtn2Left, ::nBtn2Bottom, ::nBtn2Right, ::aPxlBtnBtm, 3, .T. ) }

   ELSE
      __defaultNIL(@::nBottom, ::nTop)
      __defaultNIL(@::nRight, ::nLeft + 11)

      ::nBottom      := ::nTop
      ::nRight       := Max( 11, ::nRight )

      ::nBtn1Top     := ::nTop
      ::nBtn1Left    := ::nLeft
      ::nBtn1Bottom  := ::nBottom
      ::nBtn1Right   := ::nLeft + 1

      ::nBtn2Top     := ::nTop
      ::nBtn2Left    := ::nRight - 1
      ::nBtn2Bottom  := ::nBottom
      ::nBtn2Right   := ::nRight

      ::nSTop        := ::nTop
      ::nSLeft       := ::nLeft + 2
      ::nSBottom     := ::nBottom
      ::nSRight      := ::nRight - 2

      ::nScrollUnits := ::nSRight - ::nSLeft + 1

      ::nTotal       := Eval(::bTotal)
      ::nCurrent     := Eval(::bCurrent)

      ::ThumbPos()

      ::bBtnLeftTop := ;
         {|| wvt_DrawScrollButton( ::nBtn1Top, ::nBtn1Left, ::nBtn1Bottom, ::nBtn1Right, ::aPxlBtnLft, 2 ) }
      ::bBtnRightBottom := ;
         {|| wvt_DrawScrollButton( ::nBtn2Top, ::nBtn2Left, ::nBtn2Bottom, ::nBtn2Right, ::aPxlBtnRgt, 4 ) }
      ::bBtnScroll := ;
         {|| wvt_DrawScrollThumbHorz( ::nSTop, ::nSLeft, ::nSBottom, ::nSRight, ::aPxlScroll, ::nThumbPos ) }
      ::bBtnLeftTopDep := ;
         {|| wvt_DrawScrollButton( ::nBtn1Top, ::nBtn1Left, ::nBtn1Bottom, ::nBtn1Right, ::aPxlBtnLft, 2, .T. ) }
      ::bBtnRightBottomDep := ;
         {|| wvt_DrawScrollButton( ::nBtn2Top, ::nBtn2Left, ::nBtn2Bottom, ::nBtn2Right, ::aPxlBtnRgt, 4, .T. ) }

   ENDIF

   ::bOnLeftUp      := {|| ::HandleEvent( K_LBUTTONUP      ) }
   ::bOnLeftDown    := {|| ::HandleEvent( K_LBUTTONDOWN    ), .F. }
   ::bOnMMLeftDown  := {|| ::HandleEvent( K_MMLEFTDOWN     ) }
   ::bOnLeftPressed := {|| ::HandleEvent( K_LBUTTONPRESSED ) }

   Eval(::bBtnLeftTop)
   Eval(::bBtnRightBottom)

   ::Super:Create()

   RETURN Self

METHOD wvtScrollbar:Configure( nTop, nLeft, nBottom, nRight )

   ::nTop     := nTop
   ::nLeft    := nLeft
   ::nBottom  := nBottom
   ::nRight   := nRight

   IF ::nBarType == WVT_SCROLLBAR_VERT
      ::nRight       := ::nLeft + 1
      ::nBottom      := Max( 7, ::nBottom )

      ::nBtn1Top     := ::nTop
      ::nBtn1Left    := ::nLeft
      ::nBtn1Bottom  := ::nTop
      ::nBtn1Right   := ::nRight

      ::nBtn2Top     := ::nBottom
      ::nBtn2Left    := ::nLeft
      ::nBtn2Bottom  := ::nBottom
      ::nBtn2Right   := ::nRight

      ::nSTop        := ::nTop + 1
      ::nSLeft       := ::nLeft
      ::nSBottom     := ::nBottom - 1
      ::nSRight      := ::nRight

      ::nScrollUnits := ::nSBottom - ::nSTop + 1

      ::nTotal       := Eval(::bTotal)
      ::nCurrent     := Eval(::bCurrent)
      ::ThumbPos()
   ELSE
      ::nBottom      := ::nTop
      ::nRight       := Max( 11, ::nRight )

      ::nBtn1Top     := ::nTop
      ::nBtn1Left    := ::nLeft
      ::nBtn1Bottom  := ::nBottom
      ::nBtn1Right   := ::nLeft + 1

      ::nBtn2Top     := ::nTop
      ::nBtn2Left    := ::nRight - 1
      ::nBtn2Bottom  := ::nBottom
      ::nBtn2Right   := ::nRight

      ::nSTop        := ::nTop
      ::nSLeft       := ::nLeft + 2
      ::nSBottom     := ::nBottom
      ::nSRight      := ::nRight - 2

      ::nScrollUnits := ::nSRight - ::nSLeft + 1

      ::nTotal       := Eval(::bTotal)
      ::nCurrent     := Eval(::bCurrent)

      ::ThumbPos()
   ENDIF

   ::Refresh()

   RETURN Self

METHOD wvtScrollbar:Refresh()

   Eval(::bBtnScroll)

   RETURN Self

METHOD wvtScrollbar:SetPos( nTotal, nCurrent )

   __defaultNIL(@nTotal, Eval(::bTotal))
   __defaultNIL(@nCurrent, Eval(::bCurrent))

   ::nTotal   := nTotal
   ::nCurrent := nCurrent

   ::ThumbPos()
   ::Refresh()

   RETURN Self

METHOD wvtScrollbar:ThumbPos()

   LOCAL nNewPos, nRecPerUnit, nCurUnit

   IF ::nBarType == WVT_SCROLLBAR_VERT
      nRecPerUnit := ::nTotal / ::nScrollUnits
      nCurUnit    := Int( ::nCurrent / nRecPerUnit )

      DO CASE
      CASE ::nCurrent == 1
         nCurUnit := 0
      CASE ::nCurrent == ::nTotal
         nCurUnit := ::nScrollUnits
      ENDCASE
      nNewPos     := ::nSTop + nCurUnit

      DO CASE
      CASE nNewPos < ::nSTop
         nNewPos  := ::nSTop
      CASE nNewPos > ::nSBottom
         nNewPos  := ::nSBottom
      ENDCASE

   ELSE
      IF ::nTotal < ::nScrollUnits
         nCurUnit := ::nCurrent * Int( ::nScrollUnits / ::nTotal )
      ELSE
         nRecPerUnit := ::nTotal / ::nScrollUnits
         nCurUnit    := Int( ::nCurrent / nRecPerUnit )
      ENDIF

      DO CASE
      CASE ::nCurrent == 1
         nCurUnit := 0
      CASE ::nCurrent == ::nTotal
         nCurUnit := ::nScrollUnits
      ENDCASE

      nNewPos := ::nSLeft + nCurUnit

      DO CASE
      CASE nNewPos < ::nSLeft
         nNewPos := ::nSLeft
      CASE nNewPos > ::nSRight - 1
         nNewPos := ::nSRight - 1
      ENDCASE

   ENDIF

   ::nThumbPos := nNewPos

   RETURN Self

METHOD wvtScrollbar:GetPos()
   RETURN ::nCurrent

METHOD wvtScrollbar:SetTooltip()

   ::Tooltip := hb_ntos( Int( ::nCurrent ) ) + " / " + hb_ntos( Int( ::nTotal ) )

   wvt_SetToolTip( ::nTop, ::nLeft, ::nBottom, ::nRight, ::Tooltip )

   RETURN Self

METHOD wvtScrollbar:HandleEvent( nKey )

   LOCAL nmRow, nmCol, nOff
   LOCAL lHit  := .F.
   LOCAL mKeys_ := { K_LBUTTONDOWN, K_LBUTTONUP, K_MMLEFTDOWN, K_LBUTTONPRESSED }

   IF AScan( mKeys_, nKey ) == 0
      RETURN .F.
   ENDIF

   nmRow := MRow()
   nmCol := MCol()

   DO CASE
   CASE ::nBarType == WVT_SCROLLBAR_VERT
      lHit := .T.

      DO CASE
      CASE ::lAnchored .AND. nKey == K_MMLEFTDOWN
         IF nmRow != ::nThumbPos
            nOff := ::nThumbPos - nmRow
            IF nOff > 0
               ::nThumbPos := Max( ::nTop + 1, nmRow )
            ELSE
               ::nThumbPos := Min( ::nBottom - 1, nmRow )
            ENDIF
            ::nCurrent := ( ::nTotal * ( ::nThumbPos - ::nTop ) / ::nScrollUnits )

            IF ::nCurrent > ::nTotal
               ::nCurrent := ::nTotal
            ENDIF
            IF ::nCurrent < 1
               ::nCurrent := 1
            ENDIF

            ::SetPos( ::nTotal, ::nCurrent )

            ::SetTooltip()
            wvt_Keyboard( K_SBTHUMBTRACKVERT )
         ELSE
            lHit := .F.
         ENDIF

      CASE ::lAnchored .AND. nKey == K_LBUTTONUP
         ::lAnchored := .F.

      OTHERWISE
         lHit := .F.

         IF nmCol >= ::nLeft .AND. nmCol <= ::nRight
            lHit := .T.

            DO CASE
            CASE nmRow == ::nThumbPos .AND. nKey == K_LBUTTONDOWN
               ::lAnchored := .T.

            CASE nKey == K_LBUTTONUP
               IF ( lHit := ::lOnLeftDown )
                  DO CASE
                  CASE nmRow == ::nTop
                     Eval(::bBtnLeftTop)
                  CASE nmRow == ::nBottom
                     Eval(::bBtnRightBottom)
                  CASE nmRow < ::nThumbPos .AND. nmRow > ::nTop
                  CASE nmRow > ::nThumbPos .AND. nmRow < ::nBottom
                  OTHERWISE
                     lHit := .F.
                  ENDCASE
                  IF lHit
                     ::lOnLeftDown := .F.
                  ENDIF
               ENDIF

            CASE nKey == K_LBUTTONPRESSED
               IF ( lHit := ::lOnLeftDown )
                  DO CASE
                  CASE nmRow == ::nTop
                     wvt_Keyboard( K_SBLINEUP   )
                  CASE nmRow == ::nBottom
                     wvt_Keyboard( K_SBLINEDOWN )
                  CASE nmRow < ::nThumbPos .AND. nmRow > ::nTop
                     wvt_Keyboard( K_SBPAGEUP )
                  CASE nmRow > ::nThumbPos .AND. nmRow < ::nBottom
                     wvt_Keyboard( K_SBPAGEDOWN )
                  OTHERWISE
                     lHit := .F.
                  ENDCASE
               ENDIF

            CASE nKey == K_LBUTTONDOWN
               DO CASE
               CASE nmRow == ::nTop
                  Eval(::bBtnLeftTopDep)
                  wvt_Keyboard( K_SBLINEUP )
               CASE nmRow == ::nBottom
                  Eval(::bBtnRightBottomDep)
                  wvt_Keyboard( K_SBLINEDOWN )
               CASE nmRow < ::nThumbPos .AND. nmRow > ::nTop
                  wvt_Keyboard( K_SBPAGEUP   )
               CASE nmRow > ::nThumbPos .AND. nmRow < ::nBottom
                  wvt_Keyboard( K_SBPAGEDOWN )
               OTHERWISE
                  lHit := .F.
               ENDCASE
               IF lHit
                  ::lOnLeftDown := .T.
               ENDIF
            ENDCASE
         ENDIF

      ENDCASE

   CASE ::nBarType == WVT_SCROLLBAR_HORZ
      DO CASE
      CASE ::lAnchored .AND. nKey == K_MMLEFTDOWN
         IF ( lHit := ( nmCol < ::nThumbPos .OR. nmCol > ::nThumbPos + 1 ) )

            nOff := ::nThumbPos - nmCol
            IF nOff > 0
               ::nThumbPos := Max( ::nLeft + 2, nmCol )
            ELSE
               ::nThumbPos := Min( ::nRight - 2, nmCol )
            ENDIF

            ::nCurrent := ( ::nTotal * ( ::nThumbPos - ::nLeft + 1 ) / ::nScrollUnits )

            IF ::nCurrent > ::nTotal
               ::nCurrent := ::nTotal
            ENDIF
            IF ::nCurrent < 1
               ::nCurrent := 1
            ENDIF

            ::SetPos( ::nTotal, ::nCurrent )

            wvt_Keyboard( K_SBTHUMBTRACKHORZ )
         ENDIF

      CASE ::lAnchored .AND. nKey == K_LBUTTONUP
         ::lAnchored := .F.
         lHit := .T.

      OTHERWISE

         IF ( lHit := nmRow == ::nTop .AND. nmCol >= ::nLeft .AND. nmCol <= ::nRight )

            DO CASE
            CASE nKey == K_LBUTTONDOWN .AND. nmCol >= ::nThumbPos .AND. nmCol <= ::nThumbPos + 1
               ::lAnchored := .T.

            CASE nKey == K_LBUTTONUP

               IF ( lHit := ::lOnLeftDown )
                  DO CASE
                  CASE nmCol >= ::nLeft    .AND. nmCol <= ::nLeft + 1
                     Eval(::bBtnLeftTop)
                  CASE nmCol >= ::nRight - 1 .AND. nmCol <= ::nRight
                     Eval(::bBtnRightBottom)
                  CASE nmCol <  ::nThumbPos
                  CASE nmCol >  ::nThumbPos + 1
                  OTHERWISE
                     lHit := .F.
                  ENDCASE
                  IF lHit
                     ::lOnLeftDown := .F.
                  ENDIF
               ENDIF

            CASE nKey == K_LBUTTONPRESSED
               IF ( lHit := ::lOnLeftDown )
                  DO CASE
                  CASE nmCol == ::nLeft  .OR. nmCol == ::nLeft + 1
                     wvt_Keyboard( K_SBLINELEFT )
                  CASE nmCol == ::nRight .OR. nmCol == ::nRight - 1
                     wvt_Keyboard( K_SBLINERIGHT )
                  CASE nmCol < ::nThumbPos
                     wvt_Keyboard( K_SBPAGELEFT )
                  CASE nmCol > ::nThumbPos + 1
                     wvt_Keyboard( K_SBPAGERIGHT )
                  OTHERWISE
                     lHit := .F.
                  ENDCASE
               ENDIF

            CASE nKey == K_LBUTTONDOWN
               DO CASE
               CASE nmCol == ::nLeft  .OR. nmCol == ::nLeft + 1
                  Eval(::bBtnLeftTopDep)
                  wvt_Keyboard( K_SBLINELEFT )
               CASE nmCol == ::nRight .OR. nmCol == ::nRight - 1
                  Eval(::bBtnRightBottomDep)
                  wvt_Keyboard( K_SBLINERIGHT )
               CASE nmCol < ::nThumbPos
                  wvt_Keyboard( K_SBPAGELEFT )
               CASE nmCol > ::nThumbPos + 1
                  wvt_Keyboard( K_SBPAGERIGHT )
               OTHERWISE
                  lHit := .F.
               ENDCASE
               IF lHit
                  ::lOnLeftDown := .T.
               ENDIF
            ENDCASE
         ENDIF
      ENDCASE
   ENDCASE

   RETURN lHit
