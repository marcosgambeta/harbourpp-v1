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

// Class WvtLabel
CREATE CLASS WvtLabel INHERIT WvtObject

   ACCESS TEXT INLINE IIf(::cText == NIL, "", ::cText)
   ASSIGN TEXT(cTxt) INLINE ::cText := IIf(cTxt == NIL, "", cTxt)

   METHOD New(oParent, nID, nTop, nLeft, nBottom, nRight)
   METHOD create(lConfg)
   METHOD Configure()
   METHOD Refresh()
   METHOD HoverOn()
   METHOD HoverOff()
   METHOD SetText(ctxt)
   METHOD SetTextColor(nRGB)
   METHOD SetBackColor(nRGB)

ENDCLASS

METHOD WvtLabel:New(oParent, nID, nTop, nLeft, nBottom, nRight)

   ::Super:New(oParent, DLG_OBJ_LABEL, nID, nTop, nLeft, nBottom, nRight)

   RETURN Self

METHOD WvtLabel:Create(lConfg)

   __defaultNIL(@lConfg, .F.)

   __defaultNIL(@::nBottom, ::nTop)
   __defaultNIL(@::nRight, ::nLeft + Len(::Text))
   __defaultNIL(@::nTextColor, RGB(0, 0, 0))

   ::nTextColorHoverOff := ::nTextColor
   ::nBackColorHoverOff := ::nBackColor

   ::hFont := wvt_CreateFont(::cFont, ::nFontHeight, ::nFontWidth, ::nFontWeight, ::lItalic, ::lUnderline, ::lStrikeout, ::nCharSet, ::nFontQuality, ::nAngle)
   IF ::hFont != 0
      IF !lConfg
         ::bPaint := {||wvt_DrawLabelObj(::nTop, ::nLeft, ::nBottom, ::nRight, ::Text, ::nAlignHorz, ::nAlignVert, ::nTextColor, ::nBackColor, ::hFont)}
         AAdd(::aPaint, {::bPaint, {WVT_BLOCK_LABEL, ::nTop, ::nLeft, ::nBottom, ::nRight}})
      ENDIF
   ENDIF

   ::Super:Create()

   RETURN Self

METHOD WvtLabel:Refresh()

   Eval(::bPaint)

   RETURN Self

METHOD WvtLabel:SetText(cTxt)

   IF HB_IsString(cTxt)
      ::Text := cTxt
      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtLabel:SetTextColor(nRGB)

   IF HB_IsNumeric(nRGB)
      ::nTextColor := nRGB
      ::nTextColorHoverOff := nRGB
      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtLabel:SetBackColor(nRGB)

   IF HB_IsNumeric(nRGB)
      ::nBackColor := nRGB
      ::nBackColorHoverOff := nRGB
      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtLabel:Configure()

   ::nTextColorHoverOff := ::nTextColor
   ::nBackColorHoverOff := ::nBackColor

   IF ::hFont != 0
      wvg_DeleteObject(::hFont)
   ENDIF

   ::hFont := wvt_CreateFont(::cFont, ::nFontHeight, ::nFontWidth, ::nFontWeight, ::lItalic, ::lUnderline, ::lStrikeout, ::nCharSet, ::nFontQuality, ::nAngle)

   RETURN Self

METHOD WvtLabel:HoverOn()

   LOCAL lOn := .F.

   IF ::nTextColorHoverOn != NIL
      lOn := .T.
      ::nTextColor := ::nTextColorHoverOn
   ENDIF
   IF ::nBackColorHoverOn != NIL
      lOn := .T.
      ::nBackColor := ::nBackColorHoverOn
   ENDIF

   IF lOn
      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtLabel:HoverOff()

   LOCAL lOn := .F.

   IF ::nTextColorHoverOn != NIL
      lOn := .T.
      ::nTextColor := ::nTextColorHoverOff
   ENDIF
   IF ::nBackColorHoverOn != NIL
      lOn := .T.
      ::nBackColor := ::nBackColorHoverOff
   ENDIF

   IF lOn
      ::Refresh()
   ENDIF

   RETURN Self
