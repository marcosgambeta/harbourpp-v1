/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW toolbar functions
 * GTWVW is initially created based on:
 * =Id: gtwvt.c,v 1.60 2004-01-26 08:14:07 vouchcac Exp =
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

#include "hbgtwvw.hpp"
#include "hbvm.hpp"

/*
add one button to existing Toolbar
uiBitmap is resource id
*/
static bool hb_gt_wvw_AddTBButton(HWND hWndToolbar, const char * szBitmap, HB_UINT uiBitmap, LPCTSTR pszLabel, int iCommand, int iBitmapType, bool fMap3Dcolors, PWVW_WIN wvw_win, bool fDropdown)
{
   TBBUTTON tbb;
   TBADDBITMAP tbab;
   TCHAR szBuffer[WVW_TB_LABELMAXLENGTH + 2];
   int iNewBitmap;
   int iOffset;

   if( iCommand == 0 )
   {
      tbb.iBitmap   = 0;
      tbb.idCommand = 0;
      tbb.fsState   = TBSTATE_ENABLED;
      tbb.fsStyle   = TBSTYLE_SEP;
      tbb.dwData    = 0;
      tbb.iString   = 0;

      return static_cast<bool>(SendMessage(hWndToolbar, TB_ADDBUTTONS, static_cast<WPARAM>(1), reinterpret_cast<LPARAM>(static_cast<LPTBBUTTON>(&tbb))));
   }

   switch( iBitmapType )
   {
      case 0:
         iOffset = 0;
         break;
      case 1:
         iOffset = wvw_win->iStartStdBitmap;
         break;
      case 2:
         iOffset = wvw_win->iStartViewBitmap;
         break;
      case 3:
         iOffset = wvw_win->iStartHistBitmap;
         break;
      default:
         iOffset = 0;
   }

   if( iBitmapType == 0 )
   {
      HBITMAP hBitmap = hb_gt_wvw_PrepareBitmap(szBitmap, uiBitmap, wvw_win->iTBImgWidth, wvw_win->iTBImgHeight, fMap3Dcolors, hWndToolbar);

      if( !hBitmap )
      {
         return false;
      }

      tbab.hInst = nullptr;
      tbab.nID = reinterpret_cast<UINT_PTR>(hBitmap);
      iNewBitmap = static_cast<int>(SendMessage(hWndToolbar, TB_ADDBITMAP, 1, reinterpret_cast<WPARAM>(&tbab)));
   }
   else /* system bitmap */
   {
      iNewBitmap = static_cast<int>(uiBitmap) + iOffset;
   }

   HB_STRNCPY(szBuffer, pszLabel, HB_SIZEOFARRAY(szBuffer) - 1);

   int iNewString = static_cast<int>(SendMessage(hWndToolbar, TB_ADDSTRING, 0, reinterpret_cast<LPARAM>(szBuffer)));

   tbb.iBitmap   = iNewBitmap;
   tbb.idCommand = iCommand;
   tbb.fsState   = TBSTATE_ENABLED;
   tbb.fsStyle   = TBSTYLE_BUTTON;
   if( fDropdown )
   {
      tbb.fsStyle |= BTNS_WHOLEDROPDOWN;
   }
   tbb.dwData  = 0;
   tbb.iString = iNewString;

   return static_cast<bool>(SendMessage(hWndToolbar, TB_ADDBUTTONS, static_cast<WPARAM>(1), reinterpret_cast<LPARAM>(static_cast<LPTBBUTTON>(&tbb))));
}

static int hb_gt_wvw_IndexToCommand(HWND hWndTB, int iIndex)
{
   TBBUTTON tbb;

   if( SendMessage(hWndTB, TB_GETBUTTON, static_cast<WPARAM>(iIndex), reinterpret_cast<LPARAM>(static_cast<LPTBBUTTON>(&tbb))) )
   {
      return tbb.idCommand;
   }
   else
   {
      return 0;
   }
}

static int hb_gt_wvw_CommandToIndex(HWND hWndTB, int iCommand)
{
   return static_cast<int>(SendMessage(hWndTB, TB_COMMANDTOINDEX, static_cast<WPARAM>(iCommand), 0));
}

static void hb_gt_wvw_TBinitSize(PWVW_WIN wvw_win, HWND hWndTB)
{
   SendMessage(hWndTB, TB_AUTOSIZE, 0, 0);

   RECT rTB{};

   if( GetClientRect(hWndTB, &rTB) )
   {
      wvw_win->iTBHeight = rTB.bottom + 2;
   }
}

static POINT hb_gt_wvw_TBGetColRowFromXY(PWVW_WIN wvw_win, int x, int y)
{
   POINT colrow;
   colrow.x = x / wvw_win->PTEXTSIZE.x;
   colrow.y = y / (wvw_win->PTEXTSIZE.y + wvw_win->iLineSpacing);
   return colrow;
}

static void hb_gt_wvw_TBMouseEvent(PWVW_WIN wvw_win, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   HB_SYMBOL_UNUSED(hWnd);
   HB_SYMBOL_UNUSED(wParam);

   PWVW_GLO wvw = hb_gt_wvw();

   if( message == WM_MOUSEMOVE || message == WM_NCMOUSEMOVE )
   {
      if( !wvw_win->MouseMove )
      {
         return;
      }
   }

   POINT xy;
   xy.x = LOWORD(lParam);
   xy.y = HIWORD(lParam);

   POINT colrow = hb_gt_wvw_TBGetColRowFromXY(wvw_win, xy.x, xy.y);

   hb_gt_wvw_SetMouseX(wvw_win, colrow.x);
   hb_gt_wvw_SetMouseY(wvw_win, colrow.y);

   SHORT keyCode  = 0;
   SHORT keyState = 0;

   switch( message )
   {
      case WM_LBUTTONDBLCLK:
         keyCode = K_LDBLCLK;
         break;

      case WM_RBUTTONDBLCLK:
         keyCode = K_RDBLCLK;
         break;

      case WM_LBUTTONDOWN:
      {
         HWND hWndFocus = GetFocus();

         if( hb_gt_wvw_GetControlClass(wvw_win, hWndFocus) > 0 )
         {
            SetFocus(hWnd);
         }

         keyCode = K_LBUTTONDOWN;
         break;
      }

      case WM_RBUTTONDOWN:
         keyCode = K_RBUTTONDOWN;
         break;

      case WM_LBUTTONUP:
         keyCode = K_LBUTTONUP;
         break;

      case WM_RBUTTONUP:
         if( wvw_win->hPopup )
         {
            int nPopupRet;
            GetCursorPos(&xy);
            nPopupRet = static_cast<int>(TrackPopupMenu(wvw_win->hPopup, TPM_CENTERALIGN + TPM_RETURNCMD, xy.x, xy.y, 0, hWnd, nullptr));
            if( nPopupRet )
            {
               hb_gt_wvw_AddCharToInputQueue(nPopupRet);
            }
            return;
         }
         else
         {
            keyCode = K_RBUTTONUP;
            break;
         }

      case WM_MBUTTONDOWN:
         keyCode = K_MBUTTONDOWN;
         break;

      case WM_MBUTTONUP:
         keyCode = K_MBUTTONUP;
         break;

      case WM_MBUTTONDBLCLK:
         keyCode = K_MDBLCLK;
         break;

      case WM_MOUSEMOVE:
         keyState = static_cast<SHORT>(wParam);

         if( keyState == MK_LBUTTON )
         {
            keyCode = K_MMLEFTDOWN;
         }
         else if( keyState == MK_RBUTTON )
         {
            keyCode = K_MMRIGHTDOWN;
         }
         else if( keyState == MK_MBUTTON )
         {
            keyCode = K_MMMIDDLEDOWN;
         }
         else
         {
            keyCode = K_MOUSEMOVE;
         }
         break;

      case WM_MOUSEWHEEL:
         keyState = HIWORD(wParam);

         if( keyState > 0 )
         {
            keyCode = K_MWFORWARD;
         }
         else
         {
            keyCode = K_MWBACKWARD;
         }

         break;

      case WM_NCMOUSEMOVE:
         keyCode = K_NCMOUSEMOVE;
         break;
   }

   if( wvw->a.pSymWVW_TBMOUSE && keyCode != 0 && hb_vmRequestReenter() )
   {
      hb_vmPushDynSym(wvw->a.pSymWVW_TBMOUSE);
      hb_vmPushNil();
      hb_vmPushInteger(wvw_win->nWinId);
      hb_vmPushInteger(keyCode);
      hb_vmPushInteger(colrow.y);
      hb_vmPushInteger(colrow.x);
      hb_vmPushInteger(keyState);
      hb_vmDo(5);
      hb_vmRequestRestore();
   }

   hb_gt_wvw_AddCharToInputQueue(keyCode);
}

static LRESULT CALLBACK hb_gt_wvw_TBProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   HWND hWndParent = GetParent(hWnd);
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw == nullptr || hWndParent == nullptr )
   {
      hb_errInternal(10012, "ToolBar: Parent window is missing", nullptr, nullptr);
      return DefWindowProc(hWnd, message, wParam, lParam);
   }

   int nWin;

   for( nWin = 0; nWin < wvw->iNumWindows; ++nWin )
   {
      if( wvw->pWin[nWin]->hWnd == hWndParent )
      {
         break;
      }
   }

   if( nWin >= wvw->iNumWindows )
   {
      hb_errInternal(10013, "ToolBar: Invalid parent Window ID", nullptr, nullptr);
      return DefWindowProc(hWnd, message, wParam, lParam);
   }

   PWVW_WIN wvw_win = wvw->pWin[nWin];

   switch( message )
   {
      case WM_RBUTTONDOWN:
      case WM_LBUTTONDOWN:
      case WM_RBUTTONUP:
      case WM_LBUTTONUP:
      case WM_RBUTTONDBLCLK:
      case WM_LBUTTONDBLCLK:
      case WM_MBUTTONDOWN:
      case WM_MBUTTONUP:
      case WM_MBUTTONDBLCLK:
      case WM_MOUSEMOVE:
      case WM_MOUSEWHEEL:
      case WM_NCMOUSEMOVE:
         if( !hb_gt_wvw_AcceptingInput() || (nWin != wvw->iNumWindows - 1) )
         {
            return 0;
         }
         hb_gt_wvw_TBMouseEvent(wvw_win, hWnd, message, wParam, lParam);
         break;

      case WM_PAINT:
      {
         CallWindowProc(wvw_win->tbOldProc, hWnd, message, wParam, lParam);

         RECT rTB{};
         GetClientRect(hWnd, &rTB);
         int iTop = rTB.bottom - 3;
         int iRight = rTB.right;

         HDC hdc = GetDC(hWnd);

         HGDIOBJ hOldObj = SelectObject(hdc, wvw->a.penWhite);

         MoveToEx(hdc, 0, iTop, nullptr);          /* Top */
         LineTo(hdc, iRight, iTop);

         SelectObject(hdc, wvw->a.penBlack);

         MoveToEx(hdc, 0, iTop + 2, nullptr);      /* Bottom */
         LineTo(hdc, iRight, iTop + 2);

         SelectObject(hdc, wvw->a.penDarkGray);
         MoveToEx(hdc, 0, iTop + 1, nullptr);      /* Middle */
         LineTo(hdc, iRight, iTop + 1);

         SelectObject(wvw_win->hdc, hOldObj);
         ReleaseDC(hWnd, hdc);

         return 0;
      }
   }

   return CallWindowProc(wvw_win->tbOldProc, hWnd, message, wParam, lParam);
}

/*
wvw_tbCreate([nWinNum], lDisplayText, nStyle, nSystemBitmap, nImageWidth, nImageHeight)
creates a toolbar at the top (no button initially)
lDisplayText==.F. button's string is used as tooltips (default)
nStyle: toolbar style, defaults to TBSTYLE_FLAT | TBSTYLE_TOOLTIPS
nSystemBitmap: 0:none, 1:small, 2:large (defaults: 1)
              small=16x16 large=24x24
nImageWidth/Height are in effect only if nSystemBitmap==0
*/
HB_FUNC( WVW_TBCREATE )
{
   PWVW_GLO wvw = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win && wvw_win->hToolBar == nullptr )
   {
      DWORD dwStyle = hb_parnidef(3, TBSTYLE_ALTDRAG | TBSTYLE_FLAT | TBSTYLE_TOOLTIPS | TBSTYLE_TRANSPARENT | TBSTYLE_WRAPABLE);

      int iSystemBitmap = hb_parnidef(4, 1);
      int iImageWidth   = iSystemBitmap == 0 && HB_ISNUM(5) ? hb_parni(5) : -1;
      int iImageHeight  = iSystemBitmap == 0 && HB_ISNUM(6) ? hb_parni(6) : -1;

      InitCommonControls();

      if( iImageWidth < 0 )
      {
         switch( iSystemBitmap )
         {
            case 1:
               iImageWidth = 16;
               break;
            case 2:
               iImageWidth = 24;
               break;
            default:
               iImageWidth = 16;
         }
      }
      if( iImageHeight < 0 )
      {
         switch( iSystemBitmap )
         {
            case 1:
               iImageHeight = 16;
               break;
            case 2:
               iImageHeight = 24;
               break;
            default:
               iImageHeight = iImageWidth;
         }
      }

      HWND hWnd = CreateToolbarEx(
         wvw_win->hWnd,
         WS_CHILD | WS_VISIBLE | dwStyle,
         WVW_ID_BASE_TOOLBAR + wvw_win->nWinId,
         0,
         GetModuleHandle(nullptr),
         0,
         nullptr,
         0,
         0,
         0,
         iImageWidth,
         iImageHeight,
         sizeof(TBBUTTON));

      if( hWnd )
      {
         wvw_win->tbOldProc = reinterpret_cast<WNDPROC>(SetWindowLongPtr(hWnd, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(hb_gt_wvw_TBProc)));

         if( iSystemBitmap > 0 )
         {
            TBADDBITMAP tbab{};
            tbab.hInst = HINST_COMMCTRL;
            tbab.nID = iSystemBitmap == 1 ? IDB_STD_SMALL_COLOR : IDB_STD_LARGE_COLOR;
            wvw_win->iStartStdBitmap = static_cast<int>(SendMessage(hWnd, TB_ADDBITMAP, 0, reinterpret_cast<WPARAM>(&tbab)));
            tbab.nID = iSystemBitmap == 1 ? IDB_VIEW_SMALL_COLOR : IDB_VIEW_LARGE_COLOR;
            wvw_win->iStartViewBitmap = static_cast<int>(SendMessage(hWnd, TB_ADDBITMAP, 0, reinterpret_cast<WPARAM>(&tbab)));
            tbab.nID = iSystemBitmap == 1 ? IDB_HIST_SMALL_COLOR : IDB_HIST_LARGE_COLOR;
            wvw_win->iStartHistBitmap = static_cast<int>(SendMessage(hWnd, TB_ADDBITMAP, 0, reinterpret_cast<WPARAM>(&tbab)));
         }
         else
         {
            wvw_win->iStartStdBitmap  = 0;
            wvw_win->iStartViewBitmap = 0;
            wvw_win->iStartHistBitmap = 0;
         }

         wvw_win->iTBImgWidth  = iImageWidth;
         wvw_win->iTBImgHeight = iImageHeight;

         SendMessage(hWnd, TB_SETMAXTEXTROWS, static_cast<WPARAM>(hb_parl(2) ? 1 : 0), 0);

         hb_stornl(wvw_win->iStartStdBitmap, 7);
         hb_stornl(wvw_win->iStartViewBitmap, 8);
         hb_stornl(wvw_win->iStartHistBitmap, 9);

         hb_gt_wvw_TBinitSize(wvw_win, hWnd);

         wvw_win->hToolBar = hWnd;

         hb_gt_wvw_ResetWindow(wvw_win);

         hbwapi_ret_raw_HANDLE(hWnd);
         return;
      }
      else
      {
         hb_errRT_TERM(EG_CREATE, 10001, "Windows API CreateToolbarEx() failed", HB_ERR_FUNCNAME, 0, 0);
      }
   }

   hb_stornl(0, 7);
   hb_stornl(0, 8);
   hb_stornl(0, 9);

   hbwapi_ret_raw_HANDLE(nullptr);
}

/*
wvw_tbAddButton([nWinNum], nCommand, xBitmap, cLabel, nBitmapType, ;
                           lMap3Dcolors, lDropdown)
adds one button on the right of existing buttons
xBitmap:
nBitmap is resource id. or use cBitmap as bitmap file name.
(bitmap from resources cannot have > 256 colors)

cLabel: if lDisplayText, it will be displayed below the bitmap
     otherwise it will be used as tooltip
nBitmapType: 0:custom, 1:system std bitmap, 2:system view bitmap, 3:system hist bitmap
lMap3Dcolors: defaults to .F.
        (meaningful for custom bitmap only)
        if .T. the following color mapping will be performed:
           RGB(192, 192, 192) --> COLOR_3DFACE   ("transparent")
           RGB(128, 128, 128) --> COLOR_3DSHADOW
           RGB(223, 223, 223) --> COLOR_3DLIGHT
        This might be desirable to have transparent effect.
        LIMITATION: this will work on 256 colored bitmaps only
*/
HB_FUNC( WVW_TBADDBUTTON )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int iCommand = hb_parni(2);
      HB_UINT uiBitmap = static_cast<HB_UINT>(hb_parni(3));
      const char * szBitmap = hb_parc(3);
      int iBitmapType = hb_parni(5);
      bool fMap3Dcolors = hb_parl(6);
      bool fDropdown = hb_parl(7);

      HWND hWnd = wvw_win->hToolBar;

      if( hWnd == nullptr )
      {
         hb_retl(false);
         return;
      }

      if( iCommand >= WVW_ID_BASE_PUSHBUTTON )
      {
         hb_errRT_TERM(EG_ARG, 10001, "ToolBar button command ID too high. Potential conflict with PushButton.", HB_ERR_FUNCNAME, 0, 0);
         hb_retl(false);
         return;
      }

      void * hLabel;
      HB_SIZE nLen;
      LPCTSTR szLabel = HB_PARSTRDEF(4, &hLabel, &nLen);

      if( nLen > WVW_TB_LABELMAXLENGTH )
      {
         hb_strfree(hLabel);
         hb_errRT_TERM(EG_LIMIT, 10001, "ToolBar label too long.", HB_ERR_FUNCNAME, 0, 0);
         hb_retl(false);
         return;
      }

      int iOldHeight = wvw_win->iTBHeight;

      if( !hb_gt_wvw_AddTBButton(hWnd, szBitmap, uiBitmap, szLabel, iCommand, iBitmapType, fMap3Dcolors, wvw_win, fDropdown) )
      {
         if( iBitmapType == 0 )
         {
            if( !hb_gt_wvw_AddTBButton(hWnd, szBitmap, uiBitmap, szLabel, iCommand, 1, fMap3Dcolors, wvw_win, fDropdown) )
            {
               hb_strfree(hLabel);
               hb_errRT_TERM(EG_CREATE, 10001, "Failed hb_gt_wvw_AddTBButton()", HB_ERR_FUNCNAME, 0, 0);
               hb_retl(false);
               return;
            }
         }
         else
         {
            hb_strfree(hLabel);
            hb_errRT_TERM(EG_CREATE, 10002, "Failed hb_gt_wvw_AddTBButton()", HB_ERR_FUNCNAME, 0, 0);
            hb_retl(false);
            return;
         }
      }

      hb_strfree(hLabel);

      hb_gt_wvw_TBinitSize(wvw_win, hWnd);

      if( wvw_win->iTBHeight != iOldHeight )
      {
         hb_gt_wvw_ResetWindow(wvw_win);
      }

      hb_retl(true);
   }
   else
   {
      hb_retl(false);
   }
}

/*
wvw_tbButtonCount([nWinNum])
returns number of buttons in toolbar on window nWinNum
*/
HB_FUNC( WVW_TBBUTTONCOUNT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      HWND hWnd = wvw_win->hToolBar;
      hb_retni(hWnd ? static_cast<int>(SendMessage(hWnd, TB_BUTTONCOUNT, 0, 0)) : 0);
   }
}

/*
wvw_tbDelButton([nWinNum], nButton)
nButton is zero based index of button to delete
index=0 is the leftmost button
NOTE: button separator is indexed and deleteable too
*/
HB_FUNC( WVW_TBDELBUTTON )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   bool fResult = false;

   if( wvw_win )
   {
      int iButton = hb_parnidef(2, -1);
      HWND hWnd = wvw_win->hToolBar;

      if( hWnd && iButton >= 0 )
      {
         int iOldHeight = wvw_win->iTBHeight;

         if( SendMessage(hWnd, TB_DELETEBUTTON, static_cast<WPARAM>(iButton), 0) )
         {
            hb_gt_wvw_TBinitSize(wvw_win, hWnd);

            if( wvw_win->iTBHeight != iOldHeight )
            {
               hb_gt_wvw_ResetWindow(wvw_win);
            }

            fResult = true;
         }
      }
   }

   hb_retl(fResult);
}

/*
wvw_tbGetButtonRect([nWinNum], nButton)
return an array {nRowStart, nColStart, nRowStop, nColStop}
*/
HB_FUNC( WVW_TBGETBUTTONRECT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int iButton = hb_parnidef(2, -1);
      HWND hWnd = wvw_win->hToolBar;
      RECT rc;

      if( hWnd && iButton >= 0 && SendMessage(hWnd, TB_GETRECT, static_cast<WPARAM>(iButton), reinterpret_cast<LPARAM>(&rc)) )
      {
         PHB_ITEM aXY = hb_itemArrayNew(4);
         RECT rcRect = hb_gt_wvw_GetColRowFromXYRect(wvw_win, rc);
         hb_arraySetNL(aXY, 1, HB_MAX(0, rcRect.top));
         hb_arraySetNL(aXY, 2, rcRect.left);
         hb_arraySetNL(aXY, 3, HB_MIN(wvw_win->ROWS - 1, rcRect.bottom));
         hb_arraySetNL(aXY, 4, rcRect.right);
         hb_itemReturnRelease(aXY);
      }
   }
}

/*
wvw_tbEnableButton([nWinNum], nButton, [lToggle])
nButton is zero based index of button to enable/disable
index=0 is the leftmost button
NOTE: button separator is indexed too
returns .T. if successful
*/
HB_FUNC( WVW_TBENABLEBUTTON )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   bool fResult = false;

   if( wvw_win )
   {
      int iButton = hb_parnidef(2, -1);
      HWND hWnd = wvw_win->hToolBar;

      if( hWnd && iButton >= 0 )
      {
         int iCommand = hb_gt_wvw_IndexToCommand(hWnd, iButton);
         if( iCommand >= 0 )
         {
            int iOldHeight = wvw_win->iTBHeight;

            if( SendMessage(hWnd, TB_ENABLEBUTTON, static_cast<WPARAM>(iCommand), static_cast<LPARAM>(MAKELONG(static_cast<BOOL>(hb_parldef(3, true)) /* fEnable */, 0))) )
            {
               hb_gt_wvw_TBinitSize(wvw_win, hWnd);

               if( wvw_win->iTBHeight != iOldHeight )
               {
                  hb_gt_wvw_ResetWindow(wvw_win);
               }

               fResult = true;
            }
         }
      }
   }

   hb_retl(fResult);
}

/*
wvw_tbDestroy([nWinNum])
destroy toolbar for window nWinNum
*/
HB_FUNC( WVW_TBDESTROY )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win && wvw_win->hToolBar )
   {
      DestroyWindow(wvw_win->hToolBar);
      wvw_win->hToolBar  = nullptr;
      wvw_win->iTBHeight = 0;

      hb_gt_wvw_ResetWindow(wvw_win);
   }
}

/*
wvw_tbIndex2Cmd([nWinNum], nIndex)
returns Command Id of button nIndex (0 based)
returns -1 if the button does not exist
*/
HB_FUNC( WVW_TBINDEX2CMD )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int iCmd = hb_gt_wvw_IndexToCommand(wvw_win->hToolBar, hb_parni(2));

      if( iCmd > 0 )
      {
         hb_retni(iCmd);
         return;
      }
   }

   hb_retni(-1);
}

/*
wvw_tbCmd2Index([nWinNum], nCmd)
returns Index (0 based) of button whose command id is nCmd
returns -1 if the button does not exist
*/
HB_FUNC( WVW_TBCMD2INDEX )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      hb_retni(hb_gt_wvw_CommandToIndex(wvw_win->hToolBar, hb_parni(2)));
   }
}

HB_FUNC( WVW_TOOLBARADDBUTTONS )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win && HB_ISARRAY(3) )
   {
      PHB_ITEM pArray = hb_param(3, Harbour::Item::ARRAY);
      int iButtons = static_cast<int>(hb_arrayLen(pArray));

      if( iButtons > 0 )
      {
         HWND hWndCtrl = hbwapi_par_raw_HWND(2);

         TBBUTTON * tb = static_cast<TBBUTTON*>(hb_xgrab(iButtons * sizeof(TBBUTTON)));
         void ** hStr = static_cast<void**>(hb_xgrab(iButtons * sizeof(void*)));

         int iOldHeight = wvw_win->iTBHeight;

         SetWindowLong(hWndCtrl, GWL_STYLE, GetWindowLong( hWndCtrl, GWL_STYLE ) | TBSTYLE_TOOLTIPS | TBSTYLE_FLAT);
         SendMessage(hWndCtrl, TB_BUTTONSTRUCTSIZE, sizeof(TBBUTTON), 0);

         for( int nCount = 0; nCount < iButtons; ++nCount )
         {
            PHB_ITEM pTemp = hb_arrayGetItemPtr(pArray, nCount + 1);

            tb[nCount].idCommand = hb_arrayGetNI(pTemp, 2);
            tb[nCount].fsState   = static_cast<BYTE>(hb_arrayGetNI(pTemp, 3));
            tb[nCount].fsStyle   = static_cast<BYTE>(hb_arrayGetNI(pTemp, 4));
            tb[nCount].dwData    = hb_arrayGetNI(pTemp, 5);
            tb[nCount].iString   = reinterpret_cast<INT_PTR>(HB_ARRAYGETSTR(pTemp, 6, &hStr[nCount], nullptr));
         }

         SendMessage(hWndCtrl, TB_ADDBUTTONS, static_cast<WPARAM>(iButtons), reinterpret_cast<LPARAM>(static_cast<LPTBBUTTON>(tb)));
         SendMessage(hWndCtrl, TB_AUTOSIZE, 0, 0);

         hb_gt_wvw_TBinitSize(wvw_win, hWndCtrl);

         if( wvw_win->iTBHeight != iOldHeight )
         {
            hb_gt_wvw_ResetWindow(wvw_win);
         }

         hb_xfree(tb);

         for( int nCount = 0; nCount < iButtons; ++nCount )
         {
            hb_strfree(hStr[nCount]);
         }

         hb_xfree(hStr);
      }
   }
}
