/*
 * Video subsystem for Windows using GUI windows instead of Console
 * WITH MULTIPLE WINDOW SUPPORT
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 *
 * initially based on:
 *
 * Header File for Video subsystem for Windows using GUI windows instead of Console
 * Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                Rees Software & Systems Ltd
 *
 */

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

/* NOTE: User programs should never call this layer directly! */

/* FIXME: MT support */

#ifndef HB_WVW_H_
#define HB_WVW_H_

#ifndef _WIN32_IE
#define _WIN32_IE   0x0500
#endif

#include <hbapi.hpp>
#include "hbwapi.hpp"
#include "hbwinole.hpp"

#if defined(_MSC_VER)
#  pragma warning(push)
#  pragma warning(disable:4201)  /* warning C4201: nonstandard extension used: nameless struct/union */
#endif
#include <olectl.h>
#if defined(_MSC_VER)
#  pragma warning(pop)
#endif

#include <commctrl.h>
#include <commdlg.h>

#include <hbapierr.hpp>
#include <hbapiitm.hpp>
#include <hbgtcore.hpp>

#ifndef GRADIENT_FILL_RECT_H
#define GRADIENT_FILL_RECT_H      0x00000000
#endif
#ifndef TPM_RECURSE
#define TPM_RECURSE               0x0001L
#endif
#ifndef TTS_BALLOON
#define TTS_BALLOON               0x40
#endif
#ifndef BTNS_WHOLEDROPDOWN
#define BTNS_WHOLEDROPDOWN        0x0080
#endif

/* xHarbour compatible definitions */
#if !defined(K_SH_LEFT)
#define K_SH_LEFT                 K_LEFT   /* Shift-Left  == Left  */
#define K_SH_UP                   K_UP     /* Shift-Up    == Up    */
#define K_SH_RIGHT                K_RIGHT  /* Shift-Right == Right */
#define K_SH_DOWN                 K_DOWN   /* Shift-Down  == Down  */
#define K_SH_INS                  K_INS    /* Shift-Ins   == Ins   */
#define K_SH_DEL                  K_DEL    /* Shift-Del   == Del   */
#define K_SH_HOME                 K_HOME   /* Shift-Home  == Home  */
#define K_SH_END                  K_END    /* Shift-End   == End   */
#define K_SH_PGUP                 K_PGUP   /* Shift-PgUp  == PgUp  */
#define K_SH_PGDN                 K_PGDN   /* Shift-PgDn  == PgDn  */
#define K_SH_RETURN               K_RETURN /* Shift-Enter == Enter */
#define K_SH_ENTER                K_ENTER  /* Shift-Enter == Enter */
#endif

#define WVW_DEFAULT_MENUKEYEVENT  1024

#define WVW_ID_SYSTEM_TIMER       1

#define WVW_ID_BASE_STATUSBAR     100
#define WVW_ID_BASE_TIMER         100

#define WVW_ID_BASE_TOOLTIP       100

#define WVW_ID_BASE_TOOLBAR       100

#define WVW_ID_BASE_SCROLLBAR     100

#define WVW_ID_BASE_PROGRESSBAR   100
#define WVW_ID_BASE_STATIC        200

#define WVW_ID_BASE_PUSHBUTTON    64000
#define WVW_ID_BASE_CHECKBOX      64000

#define WVW_ID_MAX_PUSHBUTTON     (WVW_ID_BASE_PUSHBUTTON + 200 - 1)
#define WVW_ID_MAX_CHECKBOX       (WVW_ID_BASE_CHECKBOX + 200 - 1)
/* ie. effectively there are max 200 buttons on a window */

#define WVW_ID_BASE_COMBOBOX      (WVW_ID_MAX_PUSHBUTTON + 1)
#define WVW_CB_KBD_STANDARD       0
#define WVW_CB_KBD_CLIPPER        1

#define WVW_COMBOBOX_MAXLEN       255  /* maximum length of combobox string */

#define WVW_ID_MAX_COMBOBOX       (WVW_ID_BASE_COMBOBOX + 200 - 1)

#define WVW_ID_BASE_EDITBOX       (WVW_ID_MAX_COMBOBOX + 1)
#define WVW_ID_MAX_EDITBOX        (WVW_ID_BASE_EDITBOX + 200 - 1)

#define WVW_EB_SINGLELINE         1
#define WVW_EB_MULTILINE          2

#define WVW_CHAR_QUEUE_SIZE       128
#define WVW_MAX_TITLE_SIZE        128
#define WVW_CHAR_BUFFER           1024
#define WVW_MAX_ROWS              256
#define WVW_MAX_COLS              256
#define WVW_DEFAULT_ROWS          25
#define WVW_DEFAULT_COLS          80

#define WVW_DLGML_MAX             20
#define WVW_DLGMD_MAX             20

/* default maximum number of user bitmap cache
   One bitmap cache currently takes 280 bytes (see WVW_BMP).
   See also wvw_SetMaxBMCache() */
#define WVW_DEFAULT_MAX_BMCACHE   20

#define WVW_TB_LABELMAXLENGTH     100

#define BLACK                     RGB(0x00, 0x00, 0x00)
#define BLUE                      RGB(0x00, 0x00, 0x85)
#define GREEN                     RGB(0x00, 0x85, 0x00)
#define CYAN                      RGB(0x00, 0x85, 0x85)
#define RED                       RGB(0x85, 0x00, 0x00)
#define MAGENTA                   RGB(0x85, 0x00, 0x85)
#define BROWN                     RGB(0x85, 0x85, 0x00)
#define LIGHT_GRAY                RGB(0xC6, 0xC6, 0xC6)
#define GRAY                      RGB(0x60, 0x60, 0x60)
#define BRIGHT_BLUE               RGB(0x00, 0x00, 0xFF)
#define BRIGHT_GREEN              RGB(0x60, 0xFF, 0x60)
#define BRIGHT_CYAN               RGB(0x60, 0xFF, 0xFF)
#define BRIGHT_RED                RGB(0xF8, 0x00, 0x26)
#define BRIGHT_MAGENTA            RGB(0xFF, 0x60, 0xFF)
#define YELLOW                    RGB(0xFF, 0xFF, 0x00)
#define WHITE                     RGB(0xFF, 0xFF, 0xFF)

#define WM_MY_UPDATE_CARET        (WM_USER + 0x0101)

using wvwGradientFill = BOOL (WINAPI *)(HDC hdc, TRIVERTEX * pVertex, ULONG nVertex, PVOID pMesh, ULONG nMesh, ULONG ulMode);

struct _WVW_BMP
{
   char    szFilename[HB_PATH_MAX + 1];
   HBITMAP hBitmap;
   int32_t     iWidth, iHeight;
   struct _WVW_BMP * pNext;
};

using WVW_BMP = _WVW_BMP;

struct _WVW_IPIC
{
   char       szFilename[HB_PATH_MAX + 1];
   IPicture * pPicture;
   int32_t        iWidth, iHeight;
   struct _WVW_IPIC * pNext;
};

using WVW_IPIC = _WVW_IPIC;

#define WVW_CONTROL_SCROLLBAR    1
#define WVW_CONTROL_PUSHBUTTON   2
#define WVW_CONTROL_CHECKBOX     2
#define WVW_CONTROL_PROGRESSBAR  3
#define WVW_CONTROL_COMBOBOX     4
#define WVW_CONTROL_EDITBOX      5
#define WVW_CONTROL_STATIC       6

struct _WVW_CTL
{
   int32_t      nClass;
   HWND     hWnd;
   int32_t      nId;
   PHB_ITEM pBlock;
   bool     fBusy;
   int32_t      nBusy;
   RECT     rect;
   RECT     offs;

   /* SCROLLBAR specifics */
   /* also used by combobox to store kbd type */
   /* also used by editbox to store editbox type */
   int32_t      nStyle;

   /* PUSHBUTTON & CHECKBOX specifics */
   WNDPROC  OldProc;

   struct _WVW_CTL * pNext;

};

using WVW_CTL = _WVW_CTL;
using PWVW_CTL = _WVW_CTL *;

struct WVW_WIN
{
   int32_t     nWinId;                 /* Window's Id, a number 0..WVWMAXWINDOWS */
   TCHAR   szWinName[256];       /* name of Window ~ szAppName for Window 0 */

   int32_t     iLineSpacing;           /* linespacing in pixels */
   int32_t     iLSpaceColor;           /* linespacing color index */

   int32_t     iRowOfs;                /* offset to Main Window's (0,0) */
   int32_t     iColOfs;                /* offset to Main Window's (0,0) */
   int32_t     iDispCount;             /* pending DispEnd() request */
   bool    fPaintPending;          /* pending WVW_PAINT() execution */
   RECT    rPaintPending;          /* rect of pending fPaintPending */
   HWND    hStatusBar;             /* handle to status bar */
   int32_t     iSBHeight;              /* height of status bar */

   HWND    hToolBar;               /* TB handle to toolbar */
   int32_t     iTBHeight;              /* TB height of toolbar */
   int32_t     iStartStdBitmap;
   int32_t     iStartViewBitmap;
   int32_t     iStartHistBitmap;       /* start of bitmap index */
   int32_t     iTBImgWidth;
   int32_t     iTBImgHeight;           /* image width and height */
   WNDPROC tbOldProc;

   PWVW_CTL ctlList;               /* lists of created controls, eg. scrollbars */

   HFONT hPBfont;                  /* handle to font used by pushbuttons & checkboxes */
   HFONT hCBfont;                  /* handle to font used by comboboxes */
   HFONT hEBfont;                  /* handle to font used by editboxes */
   HFONT hSBfont;                  /* handle to font used by pushbuttons & checkboxes */
   HFONT hCXfont;                  /* handle to font used by checkboxes when 'focused' */
   HFONT hSTfont;                  /* handle to font used by checkboxes when 'focused' */

   bool      fSBPaint;
   COLORREF  cSBColorForeground;
   COLORREF  cSBColorBackground;

   bool      fIgnoreWM_SYSCHAR;
   bool      fPaint;
   bool      fGetFocus;

   POINT     PTEXTSIZE;                                /* size of the fixed width font */
   bool      FixedFont;                                /* true if current font is a fixed font */
   int32_t       FixedSize[WVW_MAX_COLS];                /* buffer for ExtTextOut() to emulate fixed pitch when Proportional font selected */
   TCHAR     TextLine[WVW_MAX_COLS];                 /* buffer for ExtTextOut() */
   int32_t       ROWS;                                     /* number of displayable rows in window */
   int32_t       COLS;                                     /* number of displayable columns in window */
   COLORREF  foreground;                               /* foreground color */
   COLORREF  background;                               /* background color */

   HB_SIZE   BUFFERSIZE;                               /* size of the screen text buffer */

   HB_SCREENCELL screenBuffer[WVW_MAX_ROWS * WVW_MAX_COLS];

   POINT     caretPos;                                 /* the current caret position */
   int32_t       CaretSize;                                /* this may be specific to each windows, eg. different font size */
   POINT     mousePos;                                 /* the last mouse down position */
   bool      MouseMove;                                /* Flag to say whether to return mouse movement events */
   HWND      hWnd;                                     /* the window handle */
   int32_t       Keys[WVW_CHAR_QUEUE_SIZE];              /* Array to hold the characters & events */
   int32_t       keyPointerIn;                             /* Offset into key array for character to be placed */
   int32_t       keyPointerOut;                            /* Offset into key array of next character to read */
   int32_t       keyLast;

   RECT      RectInvalid;               /* Invalid rectangle if DispBegin() active */
   HFONT     hFont;
   long      fontHeight;                /* requested font height */
   long      fontWidth;                 /* requested font width */
   long      fontWeight;                /* Bold level */
   int32_t       fontQuality;
   TCHAR     fontFace[LF_FACESIZE];   /* requested font face name LF_FACESIZE #defined in wingdi.h */

   int32_t       LastMenuEvent;             /* Last menu item selected */
   int32_t       MenuKeyEvent;              /* User definable event number for windows menu command */
   bool      CentreWindow;              /* True if window is to be Reset into centre of window */

   /* if CentreWindow is false, two following settings are examined */
   bool      HCentreWindow;             /* True if window is to be Reset into centre of window, horizontally */
   bool      VCentreWindow;             /* True if window is to be Reset into centre of window, vertically */

   int32_t       CodePage;                  /* Code page to use for display characters */

   bool      InvalidateWindow;          /* Flag for controlling whether to use ScrollWindowEx() */
   bool      EnableShortCuts;           /* Determines whether ALT key enables menu or system menu */

   HDC       hdc;                       /* Handle to Windows Device Context (FIXME: ? non-MT compatible to store it, even with CS_OWNDC?) */
   HMENU     hPopup;                    /* Handle of context menu invokable with right click */

   HDC       hCompDC;                   /* Compatible DC to _s.hdc */
   HWND      hWndTT;                    /* Handle to hold tooltip information */
   bool      fToolTipActive;            /* Flag to set whether tooltip is active or not */
   HICON     hIcon;

};

using PWVW_WIN = WVW_WIN *;

struct WVW_GLO
{
   int32_t     iPaintRefresh;        /* milliseconds between timer check */
   bool    fMainCoordMode;       /* in this mode, all HB_GT_FUNC() uses Main Window's coordinate */
   bool    fVertCaret;           /* if true, caret is in Vertical style */
   bool    fNOSTARTUPSUBWINDOW;  /* if true, subwindow will not be displayed during opening */
   /* use wvw_NoStartupSubWindow() to check/set it */
   bool    fDevCentreWindow;     /* default CentreWindow setting for subwindows */
   bool    fDevHCentreWindow;    /* default HCentreWindow setting for subwindows */
   bool    fDevVCentreWindow;    /* default VCentreWindow setting for subwindows */
   int32_t     iDefLineSpacing;      /* default line spacing */
   int32_t     iDefLSpaceColor;      /* if >= 0 this will be the color index                                          for spacing between lines */
   bool    fAllowNonTop;         /* allow non-topmost window's control to  accept input */
   bool    fRecurseCBlock;       /* allow control's codeblock to recurse */

   LOGFONT lfPB;                 /* default font for pushbuttons */
   LOGFONT lfSB;                 /* default font for statusbar */
   LOGFONT lfCB;                 /* default font for comboboxes */
   LOGFONT lfEB;                 /* default font for editboxes */
   LOGFONT lfCX;                 /* font for 'focused' checkbox */
   LOGFONT lfST;                 /* font for static control */

   HWND hWndTT;                  /* Window handle Tool Tip */

   /* read only by user */

   /* for GTWVW private use */
   bool    fQuickSetMode;      /* quick SetMode(), to reset MaxRow() and MaxCol() only */
   bool    fFlashingWindow;    /* topmost window is flashing due to invalid input on other window */

   int32_t iScrolling;             /* scrollbar is scrolling */
   int32_t iWrongButtonUp;         /* number of consecutive scrollbar's WM_LBUTTONUP encountered by gtProcessMessages */
   int32_t iMaxWrongButtonUp;      /* max number of iWrongButtonUp. If it goes higher than this number, the scrollbar is forced to stop */

   TCHAR   szAppName[13];
   TCHAR   szSubWinName[25];
   bool    fSWRegistered;

   int32_t iCursorStyle;

   HB_FHANDLE iStdIn, iStdOut, iStdErr;

   HINSTANCE hInstance;

   int32_t iNumWindows;                             /* number of windows */
   int32_t iCurWindow;                              /* current window handled by HB_GT_FUNC(...) */

   PWVW_WIN pWin[40];

   /* --- former WVW_APP --- */

   struct
   {
      bool    CaretExist;                       /* true if a caret has been created */
      bool    displayCaret;                     /* flag to indicate if caret is on */

      bool    Win9X;                            /* Flag to say if running on Win9x */
      bool    AltF4Close;                       /* Can use Alt+F4 to close application */

      HPEN   penWhite;                          /* White pen to draw GDI elements */
      HPEN   penBlack;                          /* Black pen to draw GDI elements */
      HPEN   penWhiteDim;                       /* White dim pen to draw GDI elements */
      HPEN   penDarkGray;                       /* Dark gray pen to draw GDI elements */
      HPEN   penGray;                           /* Gray pen equivalent to Cl*pper White */
      HPEN   penNull;                           /* Null pen */
      HPEN   OriginalPen;                       /* Handle da Pen original do Device Context */
      HPEN   currentPen;                        /* Handle to current pen settable at runtime */
      HPEN   gridPen;                           /* Handle da Pen para Grid */
      HBRUSH currentBrush;                      /* Handle to current brush settable by runtime */
      HBRUSH diagonalBrush;                     /* Handle to diagonal brush to draw scrollbars */
      HBRUSH solidBrush;                        /* Handle to solid brush */
      HBRUSH wvwWhiteBrush;                     /* WVW specific White colored brush */
      HBRUSH OriginalBrush;                     /* Handle da Brush original do Device Context */

      IPicture * pPicture[20];                /* Array to hold the Picture Streams to avoid recurring loading and unloading */
      HFONT      hUserFonts[20];              /* User defined font handles */
      HPEN       hUserPens[20];               /* User defined pens */

      HWND     hDlgModeless[WVW_DLGML_MAX];   /* Handle to a modeless dialog */
      PHB_ITEM pFunc[WVW_DLGML_MAX];          /* Function pointer for WndProc */
      int32_t      iType[WVW_DLGML_MAX];          /* Type of Function Pointers - Function 1, Block 2, Method 3 */

      HWND     hDlgModal[WVW_DLGMD_MAX];      /* Handle to a modal dialog */
      PHB_ITEM pFuncModal[WVW_DLGMD_MAX];     /* Function pointer for WndProc */
      int32_t      iTypeModal[WVW_DLGMD_MAX];     /* Type of Function Pointers - Function 1, Block 2, Method 3 */

      WVW_BMP *  pbhBitmapList;
      WVW_IPIC * pphPictureList;

      WVW_BMP * pbhUserBitmap;            /* User bitmap (wvw_DrawImage()) */
      int32_t       iBMcache;                 /* number of bitmap cached */
      int32_t       iMaxBMcache;              /* maximum number of bitmap cached */

      PHB_DYNS pSymWVW_PAINT;             /* Pointer to WVW_PAINT() function */
      PHB_DYNS pSymWVW_SETFOCUS;          /* Pointer to WVW_SETFOCUS() function */
      PHB_DYNS pSymWVW_KILLFOCUS;         /* Pointer to WVW_KILLFOCUS() function */
      PHB_DYNS pSymWVW_MOUSE;             /* Pointer to WVW_MOUSE() function */
      PHB_DYNS pSymWVW_TBMOUSE;           /* Pointer to WVW_TBMOUSE() function */
      PHB_DYNS pSymWVW_MENUSELECT;        /* Pointer to WVW_MENUSELECT() function*/
      PHB_DYNS pSymWVW_SIZE;              /* Pointer to WVW_SIZE() function */
      PHB_DYNS pSymWVW_MOVE;              /* Pointer to WVW_MOVE() function */
      PHB_DYNS pSymWVW_INPUTFOCUS;        /* Pointer to WVW_INPUTFOCUS() function */
      PHB_DYNS pSymWVW_TIMER;             /* Pointer to WVW_TIMER() function */
      PHB_DYNS pSymWVW_ONCTLCOLOR;        /* Pointer to WVW_ONCTLCOLOR() function */
   } a;

};

using PWVW_GLO = WVW_GLO *;

HB_EXTERN_BEGIN

/* Get functions for internal Data */
extern PWVW_GLO   hb_gt_wvw();
extern int32_t        hb_gt_wvw_nWin_N(int32_t iPar);
extern PWVW_WIN   hb_gt_wvw_win(int32_t nWin);
extern PWVW_WIN   hb_gt_wvw_win_par();
extern PWVW_WIN   hb_gt_wvw_win_top();
extern PWVW_WIN   hb_gt_wvw_win_cur();
extern bool       hb_gt_wvw_GetMainCoordMode();
extern TCHAR *    hb_gt_wvw_GetAppName();

extern void       hb_gt_wvw_ResetWindow(PWVW_WIN wvw_win);
extern int32_t        hb_gt_wvw_SetMenuKeyEvent(PWVW_WIN wvw_win, int32_t iMenuKeyEvent);
/* bitmap caching functions: */
extern HBITMAP    hb_gt_wvw_FindBitmapHandle(const char * szFileName, int32_t * piWidth, int32_t * piHeight);
extern void       hb_gt_wvw_AddBitmapHandle(const char * szFileName, HBITMAP hBitmap, int32_t iWidth, int32_t iHeight);

extern void       hb_gt_wvw_KillCaret(PWVW_WIN wvw_win);
extern void       hb_gt_wvw_CreateCaret(PWVW_WIN wvw_win);

extern HICON      hb_gt_wvw_SetWindowIcon(PWVW_WIN wvw_win, int32_t nIcon, LPCTSTR szIconName);
extern HICON      hb_gt_wvw_SetWindowIconFromFile(PWVW_WIN wvw_win, LPCTSTR szIconName);
extern void       hb_gt_wvw_SetInvalidRect(PWVW_WIN wvw_win, int32_t iLeft, int32_t iTop, int32_t iRight, int32_t iBottom);

extern void       hb_gt_wvw_ResetWindowSize(PWVW_WIN wvw_win, HWND hWnd);
extern bool       hb_gt_wvw_ValidWindowSize(PWVW_WIN wvw_win, int32_t iRows, int32_t iCols, HFONT hFont, int32_t iWidth, int32_t * piMaxRows, int32_t * piMaxCols);
extern int32_t        hb_gt_wvw_SetCodePage(PWVW_WIN wvw_win, int32_t iCodePage);
extern void       hb_gt_wvw_HBFUNCPrologue(PWVW_WIN wvw_win, int32_t * piRow1, int32_t * piCol1, int32_t * piRow2, int32_t * piCol2);
extern RECT       hb_gt_wvw_GetXYFromColRowRect(PWVW_WIN wvw_win, RECT colrow);
extern POINT      hb_gt_wvw_GetXYFromColRow(PWVW_WIN wvw_win, int32_t iCol, int32_t iRow);
extern POINT      hb_gt_wvw_GetColRowFromXY(PWVW_WIN wvw_win, int32_t x, int32_t y);
extern COLORREF   hb_gt_wvw_GetColorData(int32_t iIndex);
extern bool       hb_gt_wvw_GetImageDimension(const char * szImage, int32_t * piWidth, int32_t * piHeight);
extern bool       hb_gt_wvw_GetIPictDimension(IPicture * pPicture, int32_t * piWidth, int32_t * piHeight);
extern RECT       hb_gt_wvw_GetColRowFromXYRect(PWVW_WIN pWIndowData, RECT xy);
extern int32_t        hb_gt_wvw_LineHeight(PWVW_WIN wvw_win);
extern WPARAM     hb_gt_wvw_ProcessMessages(PWVW_WIN wvw_win);
extern HBITMAP    hb_gt_wvw_PrepareBitmap(const char * szBitmap, HB_UINT uiBitmap, int32_t iExpWidth, int32_t iExpHeight, bool bMap3Dcolors, HWND hCtrl);
/* control (eg. scrollbar) supporters: */
extern int32_t        hb_gt_wvw_GetControlClass(PWVW_WIN wvw_win, HWND hWnd);
extern HWND       hb_gt_wvw_FindControlHandle(PWVW_WIN wvw_win, int32_t nClass, int32_t nId, int32_t * pnStyle);
extern int32_t        hb_gt_wvw_FindControlId(PWVW_WIN wvw_win, int32_t nClass, HWND hWnd, int32_t * pnStyle);
extern int32_t        hb_gt_wvw_LastControlId(PWVW_WIN wvw_win, int32_t nClass);
extern void       hb_gt_wvw_AddControlHandle(PWVW_WIN wvw_win, int32_t nClass, HWND hWnd, int32_t nId, PHB_ITEM pBlock, RECT rect, RECT offs, int32_t nStyle);
extern bool       hb_gt_wvw_StoreControlProc(PWVW_WIN wvw_win, int32_t nClass, HWND hWnd, WNDPROC OldProc);
extern WNDPROC    hb_gt_wvw_GetControlProc(PWVW_WIN wvw_win, int32_t nClass, HWND hWnd);
extern int32_t        hb_gt_wvw_ButtonCreate(PWVW_WIN wvw_win, int32_t usTop, int32_t usLeft, int32_t usBottom, int32_t usRight, LPCTSTR szCaption,
                                         const char * szBitmap, HB_UINT uiBitmap, PHB_ITEM pBlock,
                                         int32_t iOffTop, int32_t iOffLeft, int32_t iOffBottom, int32_t iOffRight,
                                         double dStretch, bool bMap3Dcolors,
                                         int32_t iStyle, HWND * phWnd);
extern HFONT      hb_gt_wvw_GetFont(const TCHAR * pszFace, int32_t iHeight, int32_t iWidth, int32_t iWeight, int32_t iQuality, int32_t iCodePage);
extern int32_t        hb_gt_wvw_GetMouseX(PWVW_WIN wvw_win);
extern int32_t        hb_gt_wvw_GetMouseY(PWVW_WIN wvw_win);
extern int32_t        hb_gt_wvw_RowOfs(PWVW_WIN wvw_win);
extern int32_t        hb_gt_wvw_ColOfs(PWVW_WIN wvw_win);
extern IPicture * hb_gt_wvw_LoadPicture(const char * szImage);
extern void       hb_gt_wvw_GetCoord(PWVW_WIN wvw_win, int32_t iBase, int32_t iOffs, int32_t * piTop, int32_t * piLeft, int32_t * piBottom, int32_t * piRight);

extern bool       hb_gt_wvw_BufferedKey(int32_t iKey);
extern int32_t        hb_gt_wvw_key_ansi_to_oem(int32_t c);
extern int32_t        hb_gt_wvw_JustTranslateKey(int32_t key, int32_t shiftkey, int32_t altkey, int32_t controlkey);
extern bool       hb_gt_wvw_AcceptingInput();
extern void       hb_gt_wvw_AddCharToInputQueue(int32_t data);

extern void       hb_gt_wvw_SetMouseX(PWVW_WIN wvw_win, int32_t ix);
extern void       hb_gt_wvw_SetMouseY(PWVW_WIN wvw_win, int32_t iy);

extern PWVW_CTL   hb_gt_wvw_ctl(PWVW_WIN wvw_win, int32_t nClass, HWND hWnd, int32_t nId);

extern int32_t        hb_gt_wvw_OpenWindow(LPCTSTR szWinName, int32_t iRow1, int32_t iCol1, int32_t iRow2, int32_t iCol2, DWORD dwStyle, HWND hWndParent);
extern void       hb_gt_wvw_CloseWindow();
extern int32_t        hb_gt_wvw_SetCurWindow(int32_t nWin);
extern void       hb_gt_wvw_CreateToolTipWindow(PWVW_WIN wvw_win);

/* bitmap caching functions for user drawn bitmaps (wvw_DrawImage()) */
extern HBITMAP    hb_gt_wvw_FindUserBitmapHandle(const char * szFileName, int32_t * piWidth, int32_t * piHeight);
extern void       hb_gt_wvw_AddUserBitmapHandle(const char * szFileName, HBITMAP hBitmap, int32_t iWidth, int32_t iHeight);

extern HB_EXPORT bool          hb_gt_wvw_DestroyPicture(IPicture * pPicture);
extern HB_EXPORT BOOL CALLBACK hb_gt_wvw_DlgProcMLess(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam);
extern HB_EXPORT BOOL CALLBACK hb_gt_wvw_DlgProcModal(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam);

HB_EXTERN_END

#endif
