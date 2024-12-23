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

CREATE CLASS WvtDialog

   // To hold previous settings
   VAR nOldRows
   VAR nOldCols
   VAR aOldFont
   VAR cOldTitle
   VAR cOldColor
   VAR nOldCursor
   VAR aPalette
   VAR cScreen
   VAR aWvtScreen
   VAR aOldPnt
   VAR oldTooltipActive
   VAR oldTooltipWidth
   VAR oldTooltipBkColor
   VAR oldTooltipTextColor
   VAR oldMenuHandle
   VAR oldMenuBlock
   VAR lGui

   // Dialog parameters
   VAR nRows
   VAR nCols
   VAR cFont
   VAR nFontHeight
   VAR nFontWidth
   VAR nFontBold
   VAR nFontQuality
   VAR cTitle
   VAR cColor

   // Objects handling
   VAR aObjects INIT {}
   VAR oCurObj
   VAR oLastObj
   VAR oObjOver
   VAR oLastOver
   VAR nCurObj INIT 1
   VAR nLastObj INIT 0
   VAR nObjOver INIT 0
   VAR nLastOver INIT -1
   VAR nUseObj
   VAR oMenu
   VAR aDialogKeys INIT {}
   VAR cDialogID INIT ""

   // Tooltip Management
   VAR nTooltipWidth
   VAR nTooltipBkColor
   VAR nTooltipTextColor

   // Miscellaneous
   VAR ClassName INIT "WVTDIALOG"
   VAR cPaintBlockID
   VAR nPaintID INIT 1
   VAR nObjID INIT 5000
   VAR nKey
   VAR hFonts INIT {}
   VAR lEventHandled
   VAR lTabStops INIT .F.
   VAR bOnCreate

   ACCESS nObjects INLINE Len(::aObjects)

   METHOD New(nRows, nCols, cTitle, cFont, nFontHeight, nFontWidth, nFontBold, nFontQuality)
   METHOD create()
   METHOD Destroy()
   METHOD Event()
   METHOD Execute()
   METHOD Inkey()
   METHOD MouseOver()
   METHOD Update()
   METHOD CreateObjects()
   METHOD Eval(bBlock, p1, p2, p3, p4, p5)
   METHOD ActivateMenu()

   METHOD AddObject(oObject) INLINE AAdd(::aObjects, oObject)
   METHOD MaxRow() INLINE ::nRows - 1
   METHOD MaxCol() INLINE ::nCols - 1
   METHOD OnTimer() INLINE AEval(::aObjects, {|o|o:OnTimer()})

ENDCLASS

METHOD WvtDialog:New(nRows, nCols, cTitle, cFont, nFontHeight, nFontWidth, nFontBold, nFontQuality)

   LOCAL fnt_ := wvt_GetFontInfo()

   __defaultNIL(@nRows        , 25)
   __defaultNIL(@nCols        , 80)
   __defaultNIL(@cTitle       , wvt_GetTitle())
   __defaultNIL(@cFont        , fnt_[1])
   __defaultNIL(@nFontHeight  , fnt_[2])
   __defaultNIL(@nFontWidth   , fnt_[3])
   __defaultNIL(@nFontBold    , fnt_[4])
   __defaultNIL(@nFontQuality , fnt_[5])

   IF Empty(cFont)
      cFont := fnt_[1]
   ENDIF
   IF Empty(nFontHeight)
      nFontHeight := fnt_[2]
   ENDIF
   IF Empty(nFontWidth)
      nFontWidth := fnt_[3]
   ENDIF

   ::nOldRows := MaxRow() + 1
   ::nOldCols := MaxCol() + 1
   ::aOldFont := wvt_GetFontInfo()
   ::cOldTitle := wvt_GetTitle()
   ::cOldColor := SetColor()
   ::nOldCursor := SetCursor()
   ::aPalette := wvt_GetPalette()

   ::oldMenuHandle := wvt_GetMenu()
   ::oldMenuBlock := SetKey(wvt_SetMenuKeyEvent())

   ::oldTooltipWidth := wvt_GetToolTipWidth()
   ::oldTooltipBkColor := wvt_GetToolTipBkColor()
   ::oldTooltipTextColor := wvt_GetToolTipTextColor()

   ::nRows := nRows
   ::nCols := nCols
   ::cTitle := cTitle
   ::cFont := cFont
   ::nFontHeight := nFontHeight
   ::nFontWidth := nFontWidth
   ::nFontBold := nFontBold
   ::nFontQuality := nFontQuality

   ::cPaintBlockID := StrZero(hb_Random(99999998), 8)
   ::nObjOver := 0
   ::nKey := 0
   ::cColor := "N/W"
   ::nUseObj := 0
   ::lGui := wvt_SetGUI(.F.)

   RETURN Self

METHOD WvtDialog:Create()

   LOCAL aPalette
   LOCAL i
   LOCAL j

   ::oldToolTipActive := wvt_SetToolTipActive(.T.)
   IF ::nTooltipWidth != NIL
      wvt_SetToolTipWidth(::nTooltipWidth)
   ENDIF
   IF ::nTooltipBkColor != NIL
      wvt_SetToolTipBkColor(::nTooltipBkColor)
   ENDIF
   IF ::nTooltipTextColor != NIL
      wvt_SetToolTipTextColor(::nTooltipTextColor)
   ENDIF

   aPalette := wvt_GetPalette()
   aPalette[9] := RGB(175, 175, 175)
   wvt_SetPalette(aPalette)

   ::cScreen := SaveScreen(0, 0, MaxRow(), MaxCol())
   ::aWvtScreen := wvt_SaveScreen(0, 0, MaxRow(), MaxCol())
   ::aOldPnt := WvtSetPaint({})

   SetMode(::nRows, ::nCols)
   DO WHILE .T.
      IF wvt_SetFont(::cFont, ::nFontHeight, ::nFontWidth, ::nFontBold, ::nFontQuality)
         EXIT
      ENDIF
      ::nFontHeight--
   ENDDO
#if 0
   wvt_SetFont(::cFont, ::nFontHeight, ::nFontWidth, ::nFontBold, ::nFontQuality)
#endif
   SetMode(::nRows, ::nCols)

   wvt_SetTitle(::cTitle)

   SetColor(::cColor)
   CLS
   ::Eval(::bOnCreate)

   ::CreateObjects()

   IF Len(::aObjects) > 0
      ::oCurObj := ::aObjects[1]
   ENDIF

   FOR i := 1 TO Len(::aObjects)
      IF !Empty(::aObjects[i]:aPaint)
         FOR j := 1 TO Len(::aObjects[i]:aPaint)
            wvg_SetPaint(::cPaintBlockID, ::nPaintID++, ::aObjects[i]:aPaint[j][1], ::aObjects[i]:aPaint[j][2])
         NEXT
      ENDIF
   NEXT
   WvtSetPaint(wvg_GetPaint(::cPaintBlockID))

   IF AScan(::aObjects, {|o|o:lTabStop}) > 0
      ::lTabStops := .T.
   ENDIF

   ::Update()

   IF HB_ISOBJECT(::oMenu)
      wvt_SetMenu(::oMenu:hMenu)
      wvt_DrawMenuBar()
      SetKey(wvt_SetMenuKeyEvent(), {||::ActivateMenu(::oMenu)})
   ENDIF

   RETURN Self

METHOD PROCEDURE WvtDialog:Destroy()

   IF HB_ISOBJECT(::oMenu)
      ::oMenu:Destroy()
   ENDIF

   AEval(::aObjects, {|o|o:destroy()})

   wvt_SetToolTip(0, 0, 0, 0, "")
   wvt_SetToolTipActive(::oldToolTipActive)
   wvt_SetToolTipWidth(::oldTooltipWidth)
   wvt_SetToolTipBkColor(::oldTooltipBkColor)
   wvt_SetToolTipTextColor(::oldTooltipTextColor)

   // Here set mode is before setting the font
   SetMode(::nOldRows, ::nOldCols)
   wvt_SetFont(::aOldFont[1], ::aOldFont[2], ::aOldFont[3], ::aOldFont[4], ::aOldFont[5])
   wvt_SetTitle(::cOldTitle)
   wvt_SetPalette(::aPalette)
   wvt_SetPointer(WVT_IDC_ARROW)
   wvt_SetMousePos(MRow(), MCol())

   SetColor(::cOldColor)
   SetCursor(::nOldCursor)

   IF ::oldMenuHandle != NIL .AND. ::oldMenuHandle != 0
      wvt_SetMenu(::oldMenuHandle)
   ENDIF
   SetKey(wvt_SetMenuKeyEvent(), ::oldMenuBlock)
   RestScreen(0, 0, MaxRow(), MaxCol(), ::cScreen)
   wvt_RestScreen(0, 0, MaxRow(), MaxCol(), ::aWvtScreen)
   wvg_PurgePaint(::cPaintBlockID)
   WvtSetPaint(::aOldPnt)
   wvt_SetGUI(::lGui)

   RETURN

METHOD WvtDialog:Event()

   LOCAL nKey

   IF (nKey := Inkey(0.1, INKEY_ALL + HB_INKEY_GTEVENT)) == 0
      IF wvt_IsLButtonPressed()
         nKey := K_LBUTTONPRESSED
      ENDIF
   ENDIF

   RETURN nKey

METHOD WvtDialog:Execute()

   IF ::nObjects == 0
      DO WHILE .T.
         IF Inkey(0.1, hb_bitOr(INKEY_ALL, HB_INKEY_GTEVENT)) == K_ESC
            EXIT
         ENDIF
      ENDDO
   ELSE
      DO WHILE ::Inkey() != K_ESC
      ENDDO
   ENDIF

   RETURN ::nKey

METHOD WvtDialog:Inkey()

   LOCAL n
   LOCAL oObj
   LOCAL nID
   LOCAL i

   ::lEventHandled := .F.
   ::nUseObj := 0

   ::nKey := ::Event()
   ::OnTimer()

   IF ::nKey != 0
      IF ::nKey == K_ESC .OR. ::nKey == K_CTRL_ENTER
         RETURN K_ESC
      ENDIF

      SWITCH ::nKey

      CASE K_TAB
         IF ::lTabStops
            DO WHILE .T.
               ::nCurObj++
               IF ::nCurObj > ::nObjects
                  ::nCurObj := 1
               ENDIF
               IF ::aObjects[::nCurObj]:lTabStop
                  EXIT
               ENDIF
            ENDDO
         ENDIF

         ::lEventHandled := .T.
         
         EXIT

      CASE K_SH_TAB
         IF ::lTabStops
            DO WHILE .T.
               ::nCurObj--
               IF ::nCurObj < 1
                  ::nCurObj := ::nObjects
               ENDIF
               IF ::aObjects[::nCurObj]:lTabStop
                  EXIT
               ENDIF
            ENDDO
         ENDIF

         ::lEventHandled := .T.
         
         EXIT

      CASE K_MOUSEMOVE
      CASE K_MMLEFTDOWN
         ::MouseOver()
         IF ::nObjOver == 0
            wvt_SetPointer(WVT_IDC_ARROW)
         ELSEIF ::oObjOver:nPointer != NIL .AND. ::oObjOver:lActive
            wvt_SetPointer(::oObjOver:nPointer)
         ELSE
            wvt_SetPointer(WVT_IDC_ARROW)
         ENDIF
         ::lEventHandled := .T.

      ENDSWITCH

      IF      ::nKey == K_LBUTTONDOWN    ;
         .OR. ::nKey == K_LBUTTONUP      ;
         .OR. ::nKey == K_LDBLCLK        ;
         .OR. ::nKey == K_MMLEFTDOWN     ;
         .OR. ::nKey == K_LBUTTONPRESSED ;
         .OR. ::nKey == K_RBUTTONDOWN

         ::MouseOver()

         IF ::nObjOver > 0
            IF      ::aObjects[::nObjOver]:nType == DLG_OBJ_BUTTON     ;
               .OR. ::aObjects[::nObjOver]:nType == DLG_OBJ_TOOLBAR    ;
               .OR. ::aObjects[::nObjOver]:nType == DLG_OBJ_PUSHBUTTON ;
               .OR. ::aObjects[::nObjOver]:nType == DLG_OBJ_SCROLLBAR

               oObj := ::aObjects[::nObjOver]
               IF oObj:oParent:className() == "WVTBROWSE"
                  nID := oObj:oParent:nID
                  IF (n := AScan(::aObjects, {|o|o:nID == nID})) > 0
                     ::nCurObj := n
                  ENDIF
               ENDIF
            ELSE
               ::nCurObj := ::nObjOver
            ENDIF
            ::nUseObj := ::nObjOver
         ELSE
            ::lEventHandled := .T.
         ENDIF
      ENDIF

      IF ::nLastOver != ::nObjOver
         IF ::nLastOver > 0
            ::aObjects[::nLastOver]:HoverOff()
         ENDIF

         ::nLastOver := ::nObjOver

         IF ::nObjOver > 0
            ::oObjOver:HoverOn()
         ENDIF

         IF ::nObjOver == 0
            wvt_SetToolTip(0, 0, 0, 0, "")
         ELSEIF ::oObjOver:lActive
            ::oObjOver:SetTooltip()
         ELSE
            wvt_SetToolTip(0, 0, 0, 0, "")
         ENDIF
      ENDIF

      IF ::nCurObj != ::nLastObj
         IF ::nLastObj == 0
            ::aObjects[::nCurObj]:Hilite()
         ELSE
            ::aObjects[::nLastObj]:DeHilite()
            ::aObjects[::nCurObj]:Hilite()
         ENDIF

         ::nLastObj := ::nCurObj
         ::oCurObj := ::aObjects[::nCurObj]
         ::oLastObj := ::aObjects[::nCurObj]

         IF ::oCurObj:nType == DLG_OBJ_BROWSE
            dbSelectArea(::oCurObj:cAlias)
         ENDIF

         ::Eval(::oCurObj:bOnFocus, ::oCurObj)
      ENDIF

      IF ::nKey == K_LBUTTONDOWN
         IF ::nUseObj > 0
            IF !(::lEventHandled := ::aObjects[::nUseObj]:LeftDown())
               ::lEventHandled := ::Eval(::aObjects[::nUseObj]:bOnLeftDown)
               IF ::aObjects[::nUseObj]:className() == "WVTBROWSE"
                  ::lEventHandled := .F.
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF ::nKey == K_LBUTTONUP
         IF ::nUseObj > 0
            IF !(::lEventHandled := ::aObjects[::nUseObj]:LeftUp())
               ::lEventHandled := ::Eval(::aObjects[::nUseObj]:bOnLeftUp)
            ENDIF
         ENDIF
      ENDIF

      IF ::nKey == K_MMLEFTDOWN
         IF ::nUseObj > 0
            IF !(::lEventHandled := ::aObjects[::nUseObj]:MMLeftDown())
               ::lEventHandled := ::Eval(::aObjects[::nUseObj]:bOnMMLeftDown)
            ENDIF
         ENDIF
      ENDIF

      IF ::nKey == K_LBUTTONPRESSED
         IF ::nUseObj > 0
            IF !(::lEventHandled := ::aObjects[::nUseObj]:LeftPressed())
               ::lEventHandled := ::Eval(::aObjects[::nUseObj]:bOnLeftPressed)
            ENDIF
         ENDIF
      ENDIF

      IF ::nKey == K_LDBLCLK
         IF ::nUseObj > 0
            ::lEventHandled := ::Eval(::aObjects[::nUseObj]:bOnSelect)
         ENDIF
      ENDIF

      IF ::nKey == K_RBUTTONDOWN .AND. ::nUseObj > 0
         ::lEventHandled := ::aObjects[::nUseObj]:ShowPopup()
      ENDIF

      IF !::lEventHandled
         IF ::nCurObj > 0
            IF !Empty(::aDialogKeys)
               IF (n := AScan(::aDialogKeys, {|e_|e_[1] == ::nKey})) > 0
                  Eval(::aDialogKeys[n][2], Self, ::oCurObj)
               ENDIF
            ENDIF

            ::lEventHandled := ::oCurObj:HandleEvent(::nKey)

            IF ::lEventHandled
               IF ::oCurObj:nChildren > 0
                  FOR i := 1 to ::oCurObj:nChildren
                     IF AScan(::oCurObj:aChildren[i][OBJ_CHILD_EVENTS], ::nKey) > 0
                        ::oCurObj:NotifyChild(i, ::nKey, ::oCurObj)
                     ENDIF
                  NEXT
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF !::lEventHandled
         IF HB_ISBLOCK(SetKey(::nKey))
            Eval(SetKey(::nKey))
         ENDIF
      ENDIF
   ENDIF

   RETURN ::nKey

METHOD WvtDialog:MouseOver()

   LOCAL mRow := MRow()
   LOCAL mCol := MCol()
   LOCAL nObj

   nObj := AScan(::aObjects, {|o|o:nType != DLG_OBJ_STATIC .AND. o:nType != DLG_OBJ_TOOLBAR .AND. ;
      mRow >= o:nTop  .AND. mRow <= o:nBottom .AND. mCol >= o:nLeft .AND. mCol <= o:nRight})

   ::nObjOver := nObj
   ::oObjOver := iif(nObj > 0, ::aObjects[nObj], NIL)
   IF nObj > 0
      ::aObjects[nObj]:nmRow := mRow
      ::aObjects[nObj]:nmCol := mCol
   ENDIF

   RETURN Self

METHOD WvtDialog:Update()

   wvt_InvalidateRect(0, 0, ::MaxRow(), ::MaxCol())

   RETURN Self

METHOD WvtDialog:CreateObjects()

   LOCAL i
   LOCAL nObjs

   nObjs := Len(::aObjects)

   FOR i := 1 TO nObjs
      SWITCH ::aObjects[i]:nType

      CASE DLG_OBJ_BROWSE
         ::aObjects[i]:Create()
         EXIT
      CASE DLG_OBJ_STATUSBAR
         ::aObjects[i]:Create()
         EXIT
      CASE DLG_OBJ_LABEL
         ::aObjects[i]:Create()
         EXIT
      CASE DLG_OBJ_TOOLBAR
         ::aObjects[i]:Create()
         EXIT
      CASE DLG_OBJ_BUTTON
         ::aObjects[i]:Create()
         EXIT
      CASE DLG_OBJ_PUSHBUTTON
         ::aObjects[i]:Create()
         EXIT
      CASE DLG_OBJ_IMAGE
         ::aObjects[i]:Create()
         EXIT
      CASE DLG_OBJ_STATIC
         ::aObjects[i]:Create()
         EXIT
#if 0
      CASE DLG_OBJ_SCROLLBAR
         ::aObjects[i]:Create()
         EXIT
#endif
      CASE DLG_OBJ_GETS
         ::aObjects[i]:Create()
         EXIT
      CASE DLG_OBJ_BANNER
         ::aObjects[i]:Create()
         EXIT
      CASE DLG_OBJ_TEXTBOX
         ::aObjects[i]:Create()
         EXIT
      CASE DLG_OBJ_PROGRESSBAR
         ::aObjects[i]:Create()
         EXIT
      ENDSWITCH
   NEXT

   RETURN Self

METHOD WvtDialog:Eval(bBlock, p1, p2, p3, p4, p5)

   LOCAL lRet

   IF (lRet := HB_ISBLOCK(bBlock))
      Eval(bBlock, p1, p2, p3, p4, p5)
   ENDIF

   RETURN lRet

METHOD WvtDialog:ActivateMenu()

   LOCAL nMenu := wvt_GetLastMenuEvent()
   LOCAL aMenuItem

   IF !Empty(nMenu)
      IF HB_ISOBJECT(::oMenu)
         IF !Empty(aMenuItem := ::oMenu:FindMenuItemById(nMenu))
            IF HB_ISBLOCK(aMenuItem[WVT_MENU_ACTION])
               Eval(aMenuItem[WVT_MENU_ACTION])
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN Self
