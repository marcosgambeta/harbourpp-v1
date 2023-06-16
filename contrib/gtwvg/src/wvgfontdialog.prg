/*
 * Xbase++ Compatible xbpPartHandler Class
 *
 * Copyright 2008-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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

/*                               EkOnkar
 *                         ( The LORD is ONE )
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

/* Class WvgFontDialog() */
CREATE CLASS WvgFontDialog INHERIT WvgSysWindow

   /* Appearance */
   VAR    title                                 INIT ""
   VAR    buttonApply                           INIT .F.
   VAR    buttonCancel                          INIT .T.
   VAR    buttonHelp                            INIT .F.
   VAR    buttonOk                              INIT .T.
   VAR    buttonReset                           INIT .F.
   VAR    strikeOut                             INIT .T.
   VAR    underscore                            INIT .T.

   VAR    name                                  INIT .T.
   VAR    style                                 INIT .T.
   VAR    size                                  INIT .T.

   VAR    displayFilter                         INIT .T.
   VAR    printerFilter                         INIT .T.

   VAR    familyName                            INIT " "
   VAR    nominalPointSize                      INIT 0

   VAR    bitmapOnly                            INIT .F.
   VAR    fixedOnly                             INIT .F.
   VAR    proportionalOnly                      INIT .T.


   VAR    outLine                               INIT .T.
   VAR    previewBGClr                          INIT RGB(255, 255, 255)
   VAR    previewFGClr                          INIT RGB(0, 0, 0)
   VAR    previewString                         INIT " "
   VAR    printerPS                             INIT NIL
   VAR    screenPS                              INIT NIL

   VAR    synthesizeFonts                       INIT .T.

   VAR    vectorOnly                            INIT .F.
   VAR    vectorSizes                           INIT {}

   VAR    viewPrinterFonts                      INIT .F.
   VAR    viewScreenFonts                       INIT .T.

   METHOD new(oParent, oOwner, oScreenPS, oPrinterPS, aPos)
   METHOD create(oParent, oOwner, oScreenPS, oPrinterPS, aPos)
   METHOD destroy()
   METHOD display(nMode)

   VAR    sl_activateApply
   ACCESS activateApply                         INLINE ::sl_activateApply
   ASSIGN activateApply(bBlock)               INLINE ::sl_activateApply := bBlock

   VAR    sl_activateCancel
   ACCESS activateCancel                        INLINE ::sl_activateCancel
   ASSIGN activateCancel(bBlock)              INLINE ::sl_activateCancel := bBlock

   VAR    sl_activateOk
   ACCESS activateOk                            INLINE ::sl_activateOk
   ASSIGN activateOk(bBlock)                  INLINE ::sl_activateOk := bBlock

   VAR    sl_activateReset
   ACCESS activateReset                         INLINE ::sl_activateReset
   ASSIGN activateReset(bBlock)               INLINE ::sl_activateReset := bBlock

   VAR    oScreenPS
   VAR    oPrinterPS
   VAR    aPos                                  INIT {0, 0}
   VAR    ok                                    INIT .F.

   METHOD wndProc(hWnd, nMessage, nwParam, nlParam)
   METHOD GetWvgFont(aFont)                   PROTECTED

ENDCLASS

METHOD WvgFontDialog:new(oParent, oOwner, oScreenPS, oPrinterPS, aPos)

   __defaultNIL(@oParent, ::oParent)
   __defaultNIL(@oOwner, ::oOwner)
   __defaultNIL(@oScreenPS, ::oScreenPS)
   __defaultNIL(@oPrinterPS, ::oPrinterPS)
   __defaultNIL(@aPos, ::aPos)

   ::oParent    := oParent
   ::oOwner     := oOwner
   ::oScreenPS  := oScreenPS
   ::oPrinterPS := oPrinterPS
   ::aPos       := aPos

   ::WvgSysWindow:new(oParent, oOwner)

   RETURN Self

METHOD WvgFontDialog:create(oParent, oOwner, oScreenPS, oPrinterPS, aPos)

   __defaultNIL(@oParent, ::oParent)
   __defaultNIL(@oOwner, ::oOwner)
   __defaultNIL(@oScreenPS, ::oScreenPS)
   __defaultNIL(@oPrinterPS, ::oPrinterPS)
   __defaultNIL(@aPos, ::aPos)

   ::oParent    := oParent
   ::oOwner     := oOwner
   ::oScreenPS  := oScreenPS
   ::oPrinterPS := oPrinterPS
   ::aPos       := aPos

   IF ::viewPrinterFonts .AND. ::oPrinterPS == NIL
      ::viewPrinterFonts := .F.
   ENDIF
   IF !::viewScreenFonts .AND. !::viewPrinterFonts
      ::viewScreenFonts := .T.
   ENDIF

   ::WvgSysWindow:create(oParent, oOwner)

#if 0
   ::nWndProc := hb_AsCallBack("WNDPROC", Self)
#endif

   RETURN Self

METHOD WvgFontDialog:wndProc(hWnd, nMessage, nwParam, nlParam)

   LOCAL aRect
   LOCAL nL
   LOCAL nH

   HB_SYMBOL_UNUSED(nlParam)

   DO CASE

   CASE nMessage == WM_INITDIALOG
      ::hWnd := hWnd

      IF !Empty(::title)
         wvg_SetWindowText(::hWnd, ::title)
      ENDIF
      IF !::buttonCancel
         wvg_EnableWindow(wvg_GetDlgItem(::hWnd, IDCANCEL), .F.)
      ENDIF
      IF !::buttonApply
         wvg_EnableWindow(wvg_GetDlgItem(::hWnd, 1026), .F.)
      ENDIF
      IF !::buttonHelp
         wvg_EnableWindow(wvg_GetDlgItem(::hWnd, 1038), .F.)
      ENDIF
      IF !::strikeOut
         wvg_EnableWindow(wvg_GetDlgItem(::hWnd, 1040), .F.)
      ENDIF
      IF !::underscore
         wvg_EnableWindow(wvg_GetDlgItem(::hWnd, 1041), .F.)
      ENDIF
      IF !::name
         wvg_EnableWindow(wvg_GetDlgItem(::hWnd, 1136), .F.)
      ENDIF
      IF !::style
         wvg_EnableWindow(wvg_GetDlgItem(::hWnd, 1137), .F.)
      ENDIF
      IF !::size
         wvg_EnableWindow(wvg_GetDlgItem(::hWnd, 1138), .F.)
      ENDIF

      IF ::aPos[1] > 0 .OR. ::aPos[2] > 0
         aRect := wvg_GetWindowRect(::hWnd)
         wvg_MoveWindow(::hWnd, ::aPos[1], ::aPos[2], aRect[3] - aRect[1], aRect[4] - aRect[2], .F.)
      ENDIF

      RETURN 1

   CASE nMessage == WM_COMMAND
      nL := wvg_LOWORD(nwParam)
      nH := wvg_HIWORD(nwParam)

      HB_SYMBOL_UNUSED(nH)

      SWITCH nL

      CASE IDOK
         ::ok := .T.
         IF HB_ISBLOCK(::sl_activateOk)
            Eval(::sl_activateOk, ::GetWvgFont(), , Self)
         ENDIF
         EXIT

      CASE IDCANCEL
         IF HB_ISBLOCK(::sl_activateCancel)
            Eval(::sl_activateCancel, , , Self)
         ENDIF
         EXIT

      CASE 1026
         IF HB_ISBLOCK(::sl_activateApply)
            Eval(::sl_activateApply, ::GetWvgFont(), , Self)
         ENDIF
         EXIT

      CASE 1038  /* Help */

      ENDSWITCH

   ENDCASE

   RETURN 0

METHOD WvgFontDialog:display(nMode)

   LOCAL hWnd
   LOCAL aInfo

   IF nMode == 0
      hWnd := ::oParent:hWnd
   ELSE
      hWnd := wvg_GetDesktopWindow()
   ENDIF

   ::ok := .F.
   aInfo := wvg_ChooseFont(hWnd, {|h, m, w, l|::wndProc(h, m, w, l)}, ::familyName, ::nominalPointSize, ::viewScreenFonts, ::viewPrinterFonts)
   IF !::ok
      RETURN NIL
   ENDIF

   RETURN ::GetWvgFont(aInfo)

METHOD WvgFontDialog:destroy()

#if 0
   hb_FreeCallBack(::nWndProc)
#endif

   RETURN Self

/* Only callable from ::activateOK and ::activateApply */
METHOD WvgFontDialog:GetWvgFont(aFont)

   LOCAL oWvgFont

   IF !HB_ISARRAY(aFont)
      aFont := wvg_ChooseFont_GetLogFont(::hWnd)
   ENDIF

   oWvgFont := WvgFont():new()

   oWvgFont:familyName       := aFont[1]
   oWvgFont:height           := aFont[2]
   oWvgFont:nominalPointSize := wvg_HeightToPointSize(/* hdc */, oWvgFont:height)
   oWvgFont:width            := aFont[3]
   oWvgFont:bold             := aFont[4] > 400
   oWvgFont:italic           := aFont[5]
   oWvgFont:underscore       := aFont[6]
   oWvgFont:strikeOut        := aFont[7]
   oWvgFont:codePage         := aFont[8]
   oWvgFont:setCompoundName(RTrim(aFont[1] + " " + iif(oWvgFont:bold, "Bold ", "") + ;
      iif(oWvgFont:italic, "Italic", "")))
   oWvgFont:create()

   RETURN oWvgFont
