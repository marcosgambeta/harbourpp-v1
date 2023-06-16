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

/* Class WvtBrowse */
CREATE CLASS WvtBrowse INHERIT WvtObject

   VAR    cAlias
   VAR    oBrw
   VAR    lHSBar                                  INIT .T.
   VAR    lVSBar                                  INIT .T.
   VAR    oHBar
   VAR    oVBar
   VAR    bTotalRecords
   VAR    bCurrentRecord
   VAR    bTotalColumns
   VAR    bCurrentColumn

   ACCESS cDesc                                   INLINE iif(::cText == NIL, "", ::cText)
   ASSIGN cDesc(cText)                          INLINE ::cText := cText

   METHOD New(oParent, nID, nTop, nLeft, nBottom, nRight)
   METHOD create()
   METHOD PaintBlock(nPaintObj)
   METHOD Hilite()
   METHOD DeHilite()
   METHOD HandleEvent(nKey)
   METHOD Refresh()
   METHOD SetVBar()
   METHOD SetHBar()
   METHOD SetTooltip()
   METHOD SaveSettings()
   METHOD RestSettings()
   METHOD NotifyChild(nIndex, nKey, oCurObj)

ENDCLASS

METHOD WvtBrowse:New(oParent, nID, nTop, nLeft, nBottom, nRight)

   ::Super:New(oParent, DLG_OBJ_BROWSE, nID, nTop, nLeft, nBottom, nRight)

   RETURN Self

METHOD WvtBrowse:Create()

   dbSelectArea(::cAlias)
#if 0
   ::nTop    := ::oBrw:nTop - 2
   ::nLeft   := ::oBrw:nLeft - 2
   ::nBottom := iif(::lHSBar, ::oBrw:nBottom, ::oBrw:nBottom + 1)
   ::nRight  := iif(::lVSBar, ::oBrw:nRight, ::oBrw:nRight + 2)
#else
   ::nTop    := ::oBrw:nTop
   ::nLeft   := ::oBrw:nLeft
   ::nBottom := ::oBrw:nBottom
   ::nRight  := ::oBrw:nRight
#endif
   ::PaintBlock(1)
   ::PaintBlock(2)
   ::PaintBlock(3)
   ::PaintBlock(4)

   ::Super:Create()

   __defaultNIL(@::bTotalRecords, {|| (::cAlias)->(ordKeyCount()) })
   __defaultNIL(@::bCurrentRecord, {|| (::cAlias)->(ordKeyNo()) })
   ::SetVBar()

   __defaultNIL(@::bTotalColumns, {|| ::oBrw:ColCount })
   __defaultNIL(@::bCurrentColumn, {|| ::oBrw:ColPos   })
   ::SetHBar()

   ::oBrw:ForceStable()
   ::DeHilite()

   RETURN Self

METHOD WvtBrowse:SetVBar()

   IF ::lVSBar
      ::oVBar := WvtScrollBar():New(Self, 999991, ;
         ::oBrw:nTop, ::oBrw:nRight + 1, ::oBrw:nBottom, ::oBrw:nRight + 2)
      ::oVBar:nBarType   := WVT_SCROLLBAR_VERT
      ::oVBar:bTotal     := ::bTotalRecords
      ::oVBar:bCurrent   := ::bCurrentRecord
      ::oVBar:aPxlBtnTop := { -2, 2, 0, 0 }
      ::oVBar:aPxlBtnBtm := {  0, 2, 2, 0 }
      ::oVBar:aPxlScroll := {  0, 2, 0, 0 }
      ::oVBar:Create()

      AAdd(::aPaint, { ::oVBar:bBtnLeftTop, ;
         { WVT_BLOCK_BUTTON, ::oVBar:nBtn1Top, ::oVBar:nBtn1Left, ;
         ::oVBar:nBtn1Bottom, ::oVBar:nBtn1Right } })

      AAdd(::aPaint, { ::oVBar:bBtnRightBottom, ;
         { WVT_BLOCK_BUTTON, ::oVBar:nBtn2Top, ::oVBar:nBtn2Left, ;
         ::oVBar:nBtn2Bottom, ::oVBar:nBtn2Right } })

      AAdd(::aPaint, { ::oVBar:bBtnScroll, ;
         { WVT_BLOCK_BUTTON, ::oVBar:nSTop, ::oVBar:nSLeft, ;
         ::oVBar:nSBottom, ::oVBar:nSRight } })

      ::oParent:AddObject(::oVBar)
   ENDIF

   RETURN Self

METHOD WvtBrowse:SetHBar()

   IF ::lHSBar
      ::oHBar := WvtScrollBar():New(Self, 999990, ;
         ::oBrw:nBottom + 1, ::oBrw:nLeft, ::oBrw:nBottom + 1, ::oBrw:nRight)
      ::oHBar:nBarType   := 2
      ::oHBar:bTotal     := ::bTotalColumns
      ::oHBar:bCurrent   := ::bCurrentColumn
      ::oHBar:aPxlBtnLft := { 2, -2, 0, 0 }
      ::oHBar:aPxlBtnRgt := { 2, 0, 0, 2 }
      ::oHBar:aPxlScroll := { 2, 0, 0, 0 }
      ::oHBar:Create()

      AAdd(::aPaint, { ::oHBar:bBtnLeftTop, ;
         { WVT_BLOCK_BUTTON, ::oHBar:nBtn1Top, ::oHBar:nBtn1Left, ;
         ::oHBar:nBtn1Bottom, ::oHBar:nBtn1Right } })
      AAdd(::aPaint, { ::oHBar:bBtnRightBottom, ;
         { WVT_BLOCK_BUTTON, ::oHBar:nBtn2Top, ::oHBar:nBtn2Left, ;
         ::oHBar:nBtn2Bottom, ::oHBar:nBtn2Right } })
      AAdd(::aPaint, { ::oHBar:bBtnScroll, ;
         { WVT_BLOCK_BUTTON, ::oHBar:nSTop, ::oHBar:nSLeft, ;
         ::oHBar:nSBottom, ::oHBar:nSRight } })

      ::oParent:AddObject(::oHBar)
   ENDIF

   RETURN Self

METHOD WvtBrowse:Refresh()

   LOCAL nWorkArea := Select()

   IF HB_ISBLOCK(::bOnRefresh)
      Eval(::bOnRefresh, Self)
   ELSE
      Select(::cAlias)

      ::oBrw:RefreshAll()
      ::oBrw:ForceStable()

      Select(nWorkArea)
   ENDIF

   RETURN Self

METHOD WvtBrowse:HandleEvent(nKey)

   LOCAL lRet := .F.

   IF HB_ISBLOCK(::bHandleEvent)
      lRet := Eval(::bHandleEvent, Self, ::oParent:cPaintBlockID, ::oBrw, nKey)
   ENDIF

   RETURN lRet

METHOD WvtBrowse:NotifyChild(nIndex, nKey, oCurObj)

   LOCAL xData, i

   IF nIndex > 0 .AND. nIndex <= Len(::aChildren)
      IF HB_ISBLOCK(::aChildren[nIndex][OBJ_CHILD_DATABLOCK])
         xData := Eval(::aChildren[nIndex][OBJ_CHILD_DATABLOCK])
      ENDIF

      Eval(::aChildren[nIndex][OBJ_CHILD_REFRESHBLOCK], ;
         ::aChildren[nIndex][OBJ_CHILD_OBJ], ;
         ::aChildren[nIndex][OBJ_CHILD_OBJ]:oParent:cPaintBlockID, ;
         ::aChildren[nIndex][OBJ_CHILD_OBJ]:oBrw, ;
         nKey, ;
         xData)

      IF ::aChildren[nIndex][OBJ_CHILD_OBJ]:nChildren > 0
         /* Pretend IF focus is current on this object */
         Eval(::aChildren[nIndex][OBJ_CHILD_OBJ]:bOnFocus, ::aChildren[nIndex][OBJ_CHILD_OBJ])

         FOR i := 1 to ::aChildren[nIndex][OBJ_CHILD_OBJ]:nChildren
            ::aChildren[nIndex][OBJ_CHILD_OBJ]:NotifyChild(i, nKey, ::aChildren[nIndex][OBJ_CHILD_OBJ])
         NEXT

         /* Restore previous environments */
         Eval(oCurObj:bOnFocus, oCurObj)
      ENDIF
   ENDIF

   RETURN Self

METHOD WvtBrowse:Hilite()

   LOCAL b := ::oBrw

   hb_DispOutAt(b:nTop - 2, b:nLeft - 2, PadR(" " + ::cDesc, b:nRight - b:nLeft + 5), ::cColorHilite)

   RETURN Self

METHOD WvtBrowse:DeHilite()

   LOCAL b := ::oBrw

   hb_DispOutAt(b:nTop - 2, b:nLeft - 2, PadR(" " + ::cDesc, b:nRight - b:nLeft + 5), ::cColorDeHilite)

   RETURN Self

METHOD WvtBrowse:SetTooltip()

   LOCAL cTip, nArea

   IF HB_ISBLOCK(::bTooltip)
      ::SaveSettings()
      nArea := Select(::cAlias)

      Select(::cAlias)

      cTip := Eval(::bTooltip)

      Select(nArea)

      ::RestSettings()
   ENDIF

   IF cTip != NIL
      ::Tooltip := cTip
   ENDIF

   wvt_SetToolTip(::nTop, ::nLeft, ::nBottom, ::nRight, ::Tooltip)

   RETURN Self

METHOD WvtBrowse:SaveSettings()

   IF HB_ISBLOCK(::bSaveSettings)
      ::xSettings := Eval(::bSaveSettings, Self)
   ENDIF

   RETURN Self

METHOD WvtBrowse:RestSettings()

   IF ::xSettings != NIL .AND. HB_ISBLOCK(::bRestSettings)
      Eval(::bRestSettings, Self)
   ENDIF

   RETURN Self

METHOD WvtBrowse:PaintBlock(nPaintObj)

   LOCAL bBlock, b := ::oBrw

   SWITCH nPaintObj

   CASE 1
      bBlock := {|| wvt_DrawBoxRaised(b:nTop - 2, b:nLeft - 2, b:nBottom + 1, b:nRight + 2) }
      AAdd(::aPaint, { bBlock, { WVT_BLOCK_BOX, b:nTop - 3, b:nLeft - 3, b:nBottom + 2, b:nRight + 3 } })
      EXIT

   CASE 2
      bBlock := {|| wvt_DrawBoxRecessed(b:nTop, b:nLeft, b:nBottom, b:nRight) }
      AAdd(::aPaint, { bBlock, { WVT_BLOCK_BOX, b:nTop - 1, b:nLeft - 1, b:nBottom + 1, b:nRight + 1 } })
      EXIT

   CASE 3
      bBlock := {|| wvt_DrawGridHorz(b:nTop + 3, b:nLeft, b:nRight, b:nBottom - b:nTop - 2) }
      AAdd(::aPaint, { bBlock, { WVT_BLOCK_GRID_H, b:nTop + 4, b:nLeft + 1, b:nBottom - 1, b:nRight - 1 } })
      EXIT

   CASE 4
      bBlock := {|| wvt_DrawGridVert(b:nTop, b:nBottom, b:aColumnsSep, Len(b:aColumnsSep)) }
      AAdd(::aPaint, { bBlock, { WVT_BLOCK_GRID_V, b:nTop + 1, b:nLeft + 1, b:nBottom - 1, b:nRight - 1, b } })
      EXIT

   ENDSWITCH

   RETURN Self
