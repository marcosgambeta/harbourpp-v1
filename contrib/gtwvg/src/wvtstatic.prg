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

/* Class WvtStatic */
CREATE CLASS WvtStatic INHERIT WvtObject

   VAR    nStatic
   VAR    nOrient
   VAR    nFormat
   VAR    nAlign
   VAR    nStyle
   VAR    nThick
   VAR    nColor

   VAR    nfTop
   VAR    nfLeft
   VAR    nfBottom
   VAR    nfRight

   VAR    nHorzVert                               INIT 0
   VAR    aRGBb
   VAR    aRGBe

   VAR    aPxlOffSet                              INIT {}

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD Refresh()
   METHOD HoverOn()
   METHOD HoverOff()

ENDCLASS

METHOD WvtStatic:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_STATIC, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD WvtStatic:Create()

   LOCAL lInside := .F.

   SWITCH ::nStatic

   CASE WVT_STATIC_LINE
      lInside := .T.
      ::bPaint  := {|| wvt_DrawLine( ::nTop, ::nLeft, ::nBottom, ::nRight, ;
         ::nOrient, ::nFormat, ::nAlign, ::nStyle, ::nThick, ::nColor ) }
      EXIT

   CASE WVT_STATIC_BOXRAISED
      ::bPaint := {|| wvt_DrawBoxRaised( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_BOXRECESSED
      ::bPaint := {|| wvt_DrawBoxRecessed( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_BOXGROUP
      ::bPaint := {|| wvt_DrawBoxGroup( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_BOXGROUPRAISED
      ::bPaint := {|| wvt_DrawBoxGroupRaised( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_OUTLINE
      ::bPaint := {|| wvt_DrawOutline( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_RECTANGLE
      lInside := .T.
      ::bPaint := {|| wvt_DrawRectangle( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_ROUNDRECT
      lInside := .T.
      ::bPaint := {|| wvt_DrawRoundRect( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_FOCUSRECT
      lInside := .T.
      ::bPaint := {|| wvt_DrawFocusRect( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_ELLIPSE
      lInside := .T.
      ::bPaint := {|| wvt_DrawEllipse( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_SHADEDRECT
      lInside := .T.
      ::bPaint := {|| wvt_DrawShadedRect( ::nTop, ::nLeft, ::nBottom, ::nRight, ;
         ::aPxlOffSet, ::nHorzVert, ::aRGBb, ::aRGBe ) }
      EXIT

   ENDSWITCH

   IF lInside
      ::nfTop    := ::nTop
      ::nfLeft   := ::nLeft
      ::nfBottom := ::nBottom
      ::nfRight  := ::nRight
   ELSE
      ::nfTop    := ::nTop    - 1
      ::nfLeft   := ::nLeft   - 1
      ::nfBottom := ::nBottom + 1
      ::nfRight  := ::nRight  + 1
   ENDIF

   AAdd( ::aPaint, { ::bPaint, ;
      { WVT_BLOCK_STATIC, ::nfTop, ::nfLeft, ::nfBottom, ::nfRight } } )

   ::Super:Create()

   RETURN Self

METHOD WvtStatic:HoverOn()
   RETURN Self

METHOD WvtStatic:HoverOff()
   RETURN Self

METHOD WvtStatic:Refresh()

   Eval( ::bPaint )

   RETURN Self
