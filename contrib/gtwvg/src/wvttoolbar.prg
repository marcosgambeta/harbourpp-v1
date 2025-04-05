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

#include <hbclass.ch>
#include <hbgtinfo.ch>
#include <inkey.ch>
#include <setcurs.ch>

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

// Class WvtToolBar
CREATE CLASS WvtToolBar INHERIT WvtObject

   VAR nPaintID
   VAR aObjects INIT {}
   VAR lHidden INIT .F.
   VAR nCurButton INIT 0
   VAR lActive
   VAR lFloating
   VAR wScreen
   VAR cScreen
   VAR nBtnLeft INIT 0
   VAR nRGBSep INIT RGB(150, 150, 150)

   ACCESS nButtons INLINE Len(::aButtons)

   METHOD New(oParent, nID, nTop, nLeft, nBottom, nRight)
   METHOD create()
   METHOD Refresh()
   METHOD AddButton(cFileImage, bBlock, cTooltip)
   METHOD PaintToolBar()
   METHOD HoverOn()
   METHOD HoverOff()

ENDCLASS

METHOD WvtToolBar:New(oParent, nID, nTop, nLeft, nBottom, nRight)

   nTop := 0
   nLeft := 0
   __defaultNIL(@nBottom, 1)
   nRight := oParent:MaxCol()

   ::Super:New(oParent, DLG_OBJ_TOOLBAR, nID, nTop, nLeft, nBottom, nRight)

   ::lActive := .T.
   ::lFloating := .F.
   ::nPaintID := ::oParent:nPaintID++

   RETURN Self

METHOD WvtToolBar:Create()

   IF ::lFloating
      ::lActive := .F.
      ::lHidden := .T.
   ENDIF

   AEval(::aObjects, {|o|o:lActive := ::lActive})

   ::bPaint := {||::PaintToolBar()}
   AAdd(::aPaint, {::bPaint, {WVT_BLOCK_TOOLBAR, ::nTop, ::nLeft, ::nBottom, ::nRight}})

   ::Super:Create()

   RETURN Self

METHOD WvtToolBar:Refresh()

   IF ::lFloating
      hb_DispBox(::nTop, ::nLeft, ::nBottom, ::nRight, "         ", "n/w")
   ELSE
      wvt_InvalidateRect(::nTop, ::nLeft, ::nTop, ::nLeft)
   ENDIF

   RETURN Self

METHOD WvtToolBar:PaintToolBar()

   IF ::lActive
      wvt_DrawLine(::nTop, ::nLeft, ::nBottom, ::nRight, 0, 1, 2, , , ::nRGBSep)
   ENDIF

   RETURN Self

METHOD WvtToolBar:AddButton(cFileImage, bBlock, cTooltip)

   LOCAL oObj
   LOCAL nCol

   nCol := (::nBottom - ::nTop + 1) * 2

   oObj := WvtToolButton():New(Self)

   oObj:lActive := ::lActive
   oObj:nTop := ::nTop
   oObj:nLeft := ::nBtnLeft + 1
   oObj:nBottom := ::nBottom

   IF HB_IsString(cFileImage)
      oObj:nBtnType := TLB_BUTTON_TYPE_IMAGE
      oObj:nRight := oObj:nLeft + nCol - 1
      oObj:cFileImage := cFileImage
      oObj:bOnLeftUp := bBlock
      oObj:Tooltip := cTooltip
   ELSE
      oObj:nBtnType := TLB_BUTTON_TYPE_SEPARATOR
      oObj:nRight := oObj:nLeft
   ENDIF

   AAdd(::aObjects, oObj)

   ::nBtnLeft := oObj:nRight + 1
   ::nCurButton++

   ::oParent:AddObject(oObj)

   RETURN Self

METHOD WvtToolBar:HoverOn()

   IF ::lFloating .AND. ::lHidden
      ::lHidden := .F.
      ::lActive := .T.
#if 0
      ::cScreen := SaveScreen(::nTop, ::nLeft, ::nBottom, ::nRight)
      ::wScreen := wvt_SaveScreen(::nTop, ::nLeft, ::nBottom, ::nRight)
#endif
      AEval(::aObjects, {|o|o:lActive := ::lActive})

      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtToolBar:HoverOff()

   IF ::lFloating .AND. !::lHidden
      ::lHidden := .T.
      ::lActive := .F.
      AEval(::aObjects, {|o|o:lActive := ::lActive})
#if 0
      RestScreen(::nTop, ::nLeft, ::nBottom, ::nRight, ::cScreen)
      wvt_RestScreen(::nTop, ::nLeft, ::nBottom, ::nRight, ::wScreen, .F.)
#endif
      ::Refresh()
   ENDIF

   RETURN Self
