// Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
// Demo program to show how to make use of WVW_INPUTFOCUS() feature of GTWVW

#require "gtwvw34"

#include <inkey.ch>
#include <setcurs.ch>

#include "hbwin.ch"

// icon indexes for standard bitmap (from commctrl.h)
#define STD_DELETE              5
#define STD_FILENEW             6

// icon indexes for standard view bitmap
#define VIEW_PARENTFOLDER       8

// our command ids
#define IDM_OPENWIN     100
#define IDM_CLOSEWIN    101
#define IDM_ARRANGEWIN  102

// maximum windows opened
#define _MAX_WINNUM  10

// array of codeblock
STATIC s_akeyhandlers[_MAX_WINNUM]

PROCEDURE Main()

   LOCAL ch

#if defined(__HBSCRIPT__HBSHELL) .AND. defined(__PLATFORM__WINDOWS)
   hbshell_gtSelect( "GTWVW" )
#endif

   IF !SetMode(25, 80)
      wvw_MessageBox( 0, "Cannot set to (25,80) screen", "Warning", WIN_MB_OK + WIN_MB_ICONEXCLAMATION )
   ENDIF
   SetColor("W*/N+")
   CLS
   SetCursor( SC_NONE )
   @ 0, 0 SAY PadC("This will demonstrate how to handle input on non-topmost window", MaxCol() + 1)
   @ 1, 0 SAY PadC("(Sorry that currently caret is shown on topmost window only)", MaxCol() + 1)

   CreateToolbar(0)
   CreateStatusbar(0)

   DO WHILE ( ch := hb_keyStd(Inkey(0)) ) != K_ESC
      IF ch == wvw_SetMenuKeyEvent(0)
         MenuAction( 0, wvw_GetLastMenuEvent(0) )
      ENDIF
   ENDDO

   wvw_MessageBox( 0, "Thanks for trying this program", "Goodbye", WIN_MB_OK )

   // let toolbar and statusbar be auto-destroyed

   RETURN

// for toolbar:
STATIC FUNCTION CreateToolbar( nWinNum )

   LOCAL nSysBitmap := 1      // 0:none 1:small 2:large
   LOCAL lDisplayText := .F.  // text will be displayed as tooltip instead

   wvw_tbDestroy( nWinNum )   // just in case

   IF Empty(wvw_tbCreate( nWinNum, lDisplayText, , nSysBitmap ))
      wvw_MessageBox( nWinNum, "FAILED to create toolbar", "Error", WIN_MB_OK + WIN_MB_ICONEXCLAMATION )
      RETURN .F.
   ENDIF

   /* using system std & view bitmaps */
   wvw_tbAddButton( nWinNum, IDM_OPENWIN,  STD_FILENEW, "Open a new typewriter window", 1 /*system std bitmap*/ )
   wvw_tbAddButton( nWinNum, IDM_CLOSEWIN, STD_DELETE, "Close last window", 1 /*system std bitmap*/ )
   wvw_tbAddButton( nWinNum, IDM_ARRANGEWIN, VIEW_PARENTFOLDER, "Reposition all windows", 2 /*system view bitmap*/ )

   RETURN .T.

STATIC FUNCTION CreateStatusbar( nWinNum )

   wvw_sbDestroy( nWinNum )  // just in case

   IF Empty(wvw_sbCreate( nWinNum ))
      wvw_MessageBox( nWinNum, "FAILED to create statusbar", "Error", WIN_MB_OK + WIN_MB_ICONEXCLAMATION )
      RETURN .F.
   ENDIF

   RETURN .T.

// Handle Menu/Toolbar actions
STATIC PROCEDURE MenuAction( nWinNum, nCommand )

   DO CASE
   CASE nCommand == IDM_OPENWIN
      OpenNewWindow()
   CASE nCommand == IDM_CLOSEWIN
      CloseLastWindow()
   CASE nCommand == IDM_ARRANGEWIN
      wvw_XReposWindow()
   OTHERWISE
      wvw_MessageBox( nWinNum, "Unrecognized menu command", "Internal Error", WIN_MB_OK + WIN_MB_ICONEXCLAMATION )
   ENDCASE

   RETURN

// opens a new typewriter window
STATIC FUNCTION OpenNewWindow()

   LOCAL nWinNum := wvw_nNumWindows()
   LOCAL ctitle, nrow1, ncol1, nrow2, ncol2
   LOCAL ch

   IF nWinNum > _MAX_WINNUM
      wvw_MessageBox( nWinNum - 1, "Sorry, I don't think you can handle that many of windows :-)", ;
         "Sorry", WIN_MB_OK + WIN_MB_ICONASTERISK )
      RETURN .F.
   ENDIF

   // prepare titles and coordinates
   ctitle := "Win #" + hb_ntos(nWinNum)
   nrow1  := 4 + ( nWinNum - 1 )
   ncol1  := 1 + ( nWinNum - 1 ) * 3
   nrow2  := WinMaxRow(0) - _MAX_WINNUM + 1 + ( nWinNum - 1 )
   ncol2  := WinMaxCol(0) - ( _MAX_WINNUM + 1 ) * 3 + ( nWinNum - 1 ) * 3

   // open a window whose parent is Main Window
   SetColor("W+/N")
   IF wvw_nOpenWindow( ctitle, nrow1, ncol1, nrow2, ncol2, , 0 ) != nWinNum
      // currently wvw_nOpenWindow() will always return sequentially numbered window
      wvw_MessageBox( 0, "Something horrible has happened, program aborted", ;
         "Internal Error", WIN_MB_OK + WIN_MB_ICONHAND )
      QUIT
   ENDIF
   wvw_NoClose( nWinNum )  // disable close button

   // assign the key handler for previous window
   IF nWinNum > 1
      s_akeyhandlers[nWinNum - 1] := {| n, ch | KeyHandler( n, ch ) }
   ENDIF

   // then echoing user input, until user press ESC
   SetCursor( SC_NORMAL )
   DO WHILE ( ch := hb_keyStd(Inkey(0)) ) != K_ESC
      typing( ch )
   ENDDO

   // close current window
   wvw_lCloseWindow()

   // release keyhandler for previous window, we're going back there
   IF nWinNum > 1
      s_akeyhandlers[nWinNum - 1] := NIL
   ELSEIF nWinNum == 1
      SetCursor( SC_NONE )
   ENDIF

   RETURN .T.

// closes the last window. If no window left, Main Window will be closed too.
// Closing is done indirectly by stuffing K_ESC into kbd buffer of the
// designated window.
STATIC PROCEDURE CloseLastWindow()

   LOCAL nWinNum := wvw_nNumWindows() - 1

   wvw_nSetCurWindow( nWinNum )
   hb_keyPut( K_ESC )

   RETURN

STATIC PROCEDURE KeyHandler( nWinNum, ch )

   LOCAL nOldWin := wvw_nSetCurWindow( nWinNum )

   typing( ch )

   wvw_nSetCurWindow( nOldWin )

   RETURN

STATIC PROCEDURE typing( ch )

   DO CASE
   CASE ch == K_ENTER
      ?? Chr(10)
   CASE ch == K_BS
      ?? " " + Chr(8)
   OTHERWISE
      ?? hb_keyChar( ch )
   ENDCASE

   RETURN

/* WVW_INPUTFOCUS() is a special, callback function
 * This function will be called by GTWVW every time input occurs on
 * non-topmost window.
 * This includes Menu, Toolbar, Pushbutton, Scrollbar, or plain keyboard
 * and mouse inputs.
 *
 * Parameter passed to this function is somewhat 'raw', so their handling
 * is a bit tricky.
 *
 * This function should return .T. if it has handled the event,
 * otherwise return .F. to sign GTWVW that the input is considered invalid.
 */
FUNCTION WVW_INPUTFOCUS( nWinNum, hWnd, message, wParam, lParam )  /* must be a public function */

   LOCAL wParamLow := wapi_LOWORD(wParam)
   LOCAL nCommand, ch

   HB_SYMBOL_UNUSED(hWnd)
   HB_SYMBOL_UNUSED(lParam)

   // did user perform a menu/toolbar action on Main Window?
   IF message == WIN_WM_COMMAND .AND. nWinNum == 0  // menu,toolbar,pushbutton
      nCommand := wParamLow
      MenuAction( 0, nCommand )
      RETURN .T.
   ENDIF

   // other types of input on main window is not handled here
   IF nWinNum == 0
      RETURN .F.
   ENDIF

   // now we handle input on other non-topmost windows

   // is it a pushbutton action?
   // (TODO: create a sample of pushbutton event here)

   DO CASE
   CASE message == WIN_WM_CHAR
      ch := wParam
      Eval(s_akeyhandlers[nWinNum], nWinNum, ch)
      RETURN .T.
   OTHERWISE
      // let it be ignored
      RETURN .T.
   ENDCASE

#if 0
   wvw_MessageBox( 0, "Sorry, we cannot handle this event:" + hb_eol() + ;
      "nWinNum == " + hb_ntos(nWinNum) + hb_eol() + ;
      "message == " + hb_ntos(message) + hb_eol() + ;
      "wParam == " + hb_ntos(wParam) + hb_eol() + ;
      "wParamLow == " + hb_ntos(wParamLow) + hb_eol() + ;
      "wParamHi == " + hb_ntos(wParamHi), "Debug", WIN_MB_OK )
#endif

   RETURN .F.

// SUPPORTING FUNCTIONS

// returns MaxRow() of window nWinNum
STATIC FUNCTION winMaxRow( nWinNum )

   LOCAL nOldWin := wvw_nSetCurWindow( nWinNum )
   LOCAL nmaxrow := MaxRow()

   wvw_nSetCurWindow( nOldWin )

   RETURN nmaxrow

// returns MaxCol() of window nWinNum
STATIC FUNCTION winMaxCol( nWinNum )

   LOCAL nOldWin := wvw_nSetCurWindow( nWinNum )
   LOCAL nmaxCol := MaxCol()

   wvw_nSetCurWindow( nOldWin )

   RETURN nmaxCol
