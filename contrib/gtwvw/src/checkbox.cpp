/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW checkbox functions
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

/* wvw_cxCreate( [nWinNum], nTop, nLeft, nBottom, nRight, cText, cImage/nImage, bBlock, aOffset, ;
 *               nStretchBitmap, lMap3Dcolors, @hControl, nStyle )
 * create CHECKBOX for window nWinNum
 * nTop: row of top/left corner (in character unit)
 * nLeft: col of top/left corner (in character unit)
 * nBottom: row of bottom/right corner (in character unit) defaults==nTop
 * nRight: col of bottom/right corner (in character unit) defaults==??
 * cText: caption, default == ""
 *
 * cImage: bitmap file name, can be supplied as nImage: bitmap resource id
 * nStretchBitmap: a number between 0 and 1 (inclusive) as a factor to
 *                stretch the bitmap.
 *                1.0: bitmap covers the whole button
 *                0.5: bitmap covers 50% of button
 *                0: bitmap is not stretch
 *               (default is 1)
 * lMap3Dcolors: defaults to .F.
 *           if .T. the following color mapping will be performed:
 *              RGB(192, 192, 192) --> COLOR_3DFACE   ("transparent")
 *              RGB(128, 128, 128) --> COLOR_3DSHADOW
 *              RGB(223, 223, 223) --> COLOR_3DLIGHT
 *           This might be desirable to have transparent effect.
 *           LIMITATION: this will work on 256 colored bitmaps only
 *
 * aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *         dimension of CHECKBOX.
 *         defaults for CHECKBOX: {-2,-2,+2,+2}
 *
 * bBlock:  codeblock to execute on every BN_CLICK event.
 *         This codeblock will be evaluated with these parameters:
 *         nWinNum: window number
 *         nCXid  : CHECKBOX id
 *
 * returns control id of newly created CHECKBOX of windows nWinNum
 * returns 0 if failed
 */
HB_FUNC( WVW_CXCREATE )
{
   auto usWinNum = WVW_WHICH_WINDOW;
   /* int   iStyle; */
   UINT   uiPBid;
   USHORT usTop         = static_cast<BYTE>(hb_parni(2)),
          usLeft        = static_cast<BYTE>(hb_parni(3)),
          usBottom      = static_cast<BYTE>(hb_parni(4)),
          usRight       = static_cast<BYTE>(hb_parni(5));
   LPCTSTR lpszCaption  = HB_ISCHAR(6) ? hb_parcx(6) : nullptr;
   char *  szBitmap     = HB_ISCHAR(7) ? const_cast<char*>(hb_parcx(7)) : nullptr;
   UINT    uiBitmap     = HB_ISNUM(7) ? static_cast<UINT>(hb_parni(7)) : 0;
   double  dStretch     = !HB_ISNIL(10) ? hb_parnd(10) : 1;
   BOOL    bMap3Dcolors = HB_ISLOG(11) ? static_cast<BOOL>(hb_parl(11)) : FALSE;

   if( !HB_ISBLOCK(8) ) {
      hb_retnl(0);
      return;
   }

   int iOffTop    = !HB_ISNIL(9) ? hb_parvni(9, 1) : -2;
   int iOffLeft   = !HB_ISNIL(9) ? hb_parvni(9, 2) : -2;
   int iOffBottom = !HB_ISNIL(9) ? hb_parvni(9, 3) : +2;
   int iOffRight  = !HB_ISNIL(9) ? hb_parvni(9, 4) : +2;

   uiPBid = ButtonCreate(usWinNum, usTop, usLeft, usBottom, usRight, lpszCaption,
                         szBitmap, uiBitmap, hb_param(8, Harbour::Item::BLOCK),
                         iOffTop, iOffLeft, iOffBottom, iOffRight,
                         dStretch, bMap3Dcolors,
                         BS_AUTOCHECKBOX);
   hb_retnl(static_cast<LONG>(uiPBid));
}

/*wvw_cxDestroy( [nWinNum], nCXid )
 * destroy checkbox nCXid for window nWinNum
 */
HB_FUNC( WVW_CXDESTROY )
{
   auto usWinNum = WVW_WHICH_WINDOW;
   auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
   auto uiCXid = static_cast<UINT>(HB_ISNIL(2) ? 0  : hb_parni(2));
   CONTROL_DATA * pcd         = pWindowData->pcdCtrlList;
   CONTROL_DATA * pcdPrev     = nullptr;

   while( pcd ) {
      if( pcd->byCtrlClass == WVW_CONTROL_CHECKBOX && pcd->uiCtrlid == uiCXid ) {
         break;
      }
      pcdPrev = pcd;
      pcd     = pcd->pNext;
   }

   if( pcd == nullptr ) {
      return;
   }

   DestroyWindow(pcd->hWndCtrl);

   if( pcdPrev == nullptr ) {
      pWindowData->pcdCtrlList = pcd->pNext;
   } else {
      pcdPrev->pNext = pcd->pNext;
   }

   if( pcd->phiCodeBlock ) {
      hb_itemRelease(pcd->phiCodeBlock);
   }

   hb_xfree(pcd);
}

/*wvw_cxSetFocus( [nWinNum], nButtonId )
 * set the focus to checkbox nButtonId in window nWinNum
 */
HB_FUNC( WVW_CXSETFOCUS )
{
   auto usWinNum = WVW_WHICH_WINDOW;
   UINT uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
   byte bStyle;
   auto hWndCX = FindControlHandle(usWinNum, WVW_CONTROL_CHECKBOX, uiCtrlId, &bStyle);

   if( hWndCX ) {
      hb_retl(SetFocus(hWndCX) != nullptr);
   } else {
      hb_retl(false);
   }
}

/*wvw_cxEnable( [nWinNum], nButtonId, [lToggle] )
 * enable/disable checkbox nButtonId on window nWinNum
 *(lToggle defaults to .t., ie. enabling the checkbox)
 * return previous state of the checkbox (TRUE:enabled FALSE:disabled)
 *(if nButtonId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_CXENABLE )
{
   auto usWinNum = WVW_WHICH_WINDOW;
   UINT       uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
   BOOL       bEnable  = HB_ISNIL(3) ? TRUE : hb_parl(3);
   byte       bStyle;
   auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
   auto hWndCX = FindControlHandle(usWinNum, WVW_CONTROL_CHECKBOX, uiCtrlId, &bStyle);

   if( hWndCX ) {
      hb_retl(EnableWindow(hWndCX, bEnable) == 0);

      if( !bEnable ) {
         SetFocus(pWindowData->hWnd);
      }
   } else {
      hb_retl(false);
   }
}

/*wvw_cxSetCodeblock( [nWinNum], nCXid, bBlock )
 * assign (new) codeblock bBlock to button nCXid for window nWinNum
 *
 * return .t. if successful
 */
HB_FUNC( WVW_CXSETCODEBLOCK )
{
   auto usWinNum = WVW_WHICH_WINDOW;
   auto pData = hb_getWvwData();
   auto uiCXid = static_cast<UINT>(HB_ISNIL(2) ? 0  : hb_parni(2));
   auto pcd = GetControlData(usWinNum, WVW_CONTROL_CHECKBOX, nullptr, uiCXid);
   auto phiCodeBlock = hb_param(3, Harbour::Item::BLOCK);
   BOOL           bOldSetting  = pData->s_bRecurseCBlock;

   if( !phiCodeBlock || pcd == nullptr || pcd->bBusy ) {
      hb_retl(false);
      return;
   }

   pData->s_bRecurseCBlock = FALSE;
   pcd->bBusy = TRUE;

   if( pcd->phiCodeBlock ) {
      hb_itemRelease(pcd->phiCodeBlock);
   }

   pcd->phiCodeBlock = hb_itemNew(phiCodeBlock);

   pcd->bBusy = FALSE;
   pData->s_bRecurseCBlock = bOldSetting;

   hb_retl(true);
}

/* wvw_cxSetCheck( [nWinNum], nCXid, nCheckState )
 * assigns check-state of checkbox nCXid
 *           0==unchecked    BST_UNCHECKED
 *           1==checked      BST_CHECKED
 *           2==indeterminate BST_INDETERMINATE
 * this function always returns .t.
 */
HB_FUNC( WVW_CXSETCHECK )
{
   auto usWinNum = WVW_WHICH_WINDOW;
   auto uiCXid = static_cast<UINT>(HB_ISNIL(2) ? 0  : hb_parni(2));
   auto ulCheck = static_cast<ULONG>(HB_ISNIL(3) ? BST_CHECKED  : hb_parni(3));
   auto pcd = GetControlData(usWinNum, WVW_CONTROL_CHECKBOX, nullptr, uiCXid);

   if( pcd->hWndCtrl ) {
      SendMessage(pcd->hWndCtrl, BM_SETCHECK, static_cast<WPARAM>(ulCheck), 0);
   }

   hb_retl(true);
}

/*wvw_cxGetCheck( [nWinNum], nCXid )
 * returns check-state of checkbox nCXid
 *           0==unchecked    BST_UNCHECKED
 *           1==checked      BST_CHECKED
 *           2==indeterminate BST_INDETERMINATE
 */
HB_FUNC( WVW_CXGETCHECK )
{
   auto usWinNum = WVW_WHICH_WINDOW;
   auto uiCXid = static_cast<UINT>(HB_ISNIL(2) ? 0  : hb_parni(2));
   ULONG ulCheck      = 0;
   auto pcd = GetControlData(usWinNum, WVW_CONTROL_CHECKBOX, nullptr, uiCXid);

   if( pcd->hWndCtrl ) {
      ulCheck = SendMessage(pcd->hWndCtrl, BM_GETCHECK, 0, 0);
   }

   hb_retnl(ulCheck);
}

/*wvw_cxSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality,;
 *                             lItalic, lUnderline, lStrikeout
 *
 */
HB_FUNC( WVW_CXSETFONT )
{
   auto usWinNum = WVW_WHICH_WINDOW;
   auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
   auto pData = hb_getWvwData();

   BOOL retval = TRUE;

   pData->s_lfCX.lfHeight      = HB_ISNIL(3) ? pWindowData->fontHeight - 2 : hb_parnl(3);
   pData->s_lfCX.lfWidth       = HB_ISNIL(4) ? pData->s_lfCX.lfWidth : hb_parni(4);
   pData->s_lfCX.lfEscapement  = 0;
   pData->s_lfCX.lfOrientation = 0;
   pData->s_lfCX.lfWeight      = HB_ISNIL(5) ? pData->s_lfCX.lfWeight : hb_parni(5);
   pData->s_lfCX.lfItalic      = HB_ISNIL(7) ? pData->s_lfCX.lfItalic    : static_cast<BYTE>(hb_parl(7));
   pData->s_lfCX.lfUnderline   = HB_ISNIL(8) ? pData->s_lfCX.lfUnderline : static_cast<BYTE>(hb_parl(8));
   pData->s_lfCX.lfStrikeOut   = HB_ISNIL(9) ? pData->s_lfCX.lfStrikeOut : static_cast<BYTE>(hb_parl(9));
   pData->s_lfCX.lfCharSet     = DEFAULT_CHARSET;

   pData->s_lfCX.lfQuality        = HB_ISNIL(6) ? pData->s_lfCX.lfQuality : static_cast<BYTE>(hb_parni(6));
   pData->s_lfCX.lfPitchAndFamily = FF_DONTCARE;
   if( HB_ISCHAR(2) ) {
      strcpy(pData->s_lfCX.lfFaceName, hb_parcx(2));
   }

   if( pWindowData->hCXfont ) {
      HFONT hOldFont = pWindowData->hCXfont;
      auto hFont = CreateFontIndirect(&pData->s_lfCX);
      if( hFont ) {
         /*CONTROL_DATA * pcd = pWindowData->pcdCtrlList;

            while (pcd) {
               if( (pcd->byCtrlClass == WVW_CONTROL_PUSHBUTTON) &&
                  ((HFONT) SendMessage(pcd->hWndCtrl, WM_GETFONT, (WPARAM) 0, (LPARAM) 0) == hOldFont) ) {
                 SendMessage(pcd->hWndCtrl, WM_SETFONT, (WPARAM) hFont, (LPARAM) TRUE);
               }

               pcd = pcd->pNext;
            } */

         pWindowData->hCXfont = hFont;
         DeleteObject(static_cast<HFONT>(hOldFont));

      } else {
         retval = FALSE;
      }
   }

   hb_retl(retval);
}

HB_FUNC( WVW_CXSTATUSFONT )
{
   auto usWinNum = WVW_WHICH_WINDOW;
   auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

   auto uiPBid = static_cast<UINT>(HB_ISNIL(2) ? 0 : hb_parni(2));
   BOOL bFocus        = HB_ISNIL(3) ? TRUE : hb_parl(3);
   auto pcd = GetControlData(usWinNum, WVW_CONTROL_PUSHBUTTON, nullptr, uiPBid);

   if( pcd->hWndCtrl ) {
      if( bFocus ) {
         SendMessage(pcd->hWndCtrl, WM_SETFONT, reinterpret_cast<WPARAM>(pWindowData->hCXfont), static_cast<LPARAM>(TRUE));
      } else {
         SendMessage(pcd->hWndCtrl, WM_SETFONT, reinterpret_cast<WPARAM>(pWindowData->hPBfont), static_cast<LPARAM>(TRUE));
      }
   }

   hb_retl(true);
}

/* CHECKBOX ends                                                     */

/* PROGRESSBAR begins                                                 */

/* wvw_pgCreate( [nWinNum], nTop, nLeft, nBottom, nRight, [aOffset],
 *                         [nBackColor], [nBarColor], [lSmooth], [lVertical])
 * create progress bar for window nWinNum
 * nTop: row of top/left corner (in character unit)
 * nLeft: col of top/left corner (in character unit)
 * nBottom: row of bottom/right corner (in character unit)
 * nRight: col of bottom/right corner (in character unit)
 * aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *        dimension of progress bar. defaults: {0, 0, 0, 0}
 * nBackColor: color of background (as RGB value)
 * nBarColor: color of bar (as RGB value)
 * lSmooth: if .t., draw as smooth bar (default is .f.)
 * lVertical: if .t., draw as vertical progress bar (default is .f.)
 *
 * returns control id of newly created progress bar of windows nWinNum
 * returns 0 if failed
 *
 * example:
 * wvw_pgCreate( , 5, 10, 5, 30)
 *  :: creates horiz progressbar on current window at (5,10) to (5,30)
 *     colors using default ones.
 *
 * wvw_pgCreate( , 5, 10, 5, 30, {-1, 0, +1, 0} )
 *  :: same as above, but the bar is enlarged 1 pixel to the top
 *     and 1 pixel to the bottom
 *
 * NOTES:
 * ProgressRange is initially set as 0 - 100.
 * Initial ProgressPos is 0
 */

HB_FUNC( WVW_PGCREATE )
{
   HANDLE     hInstance   = nullptr;
   auto usWinNum = WVW_WHICH_WINDOW;
   auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
   HWND       hWndParent  = pWindowData->hWnd;
   HWND       hWndPG;
   POINT      xy{};
   int        iTop, iLeft, iBottom, iRight;
   int        iStyle     = 0;
   BOOL       bBackColor = !HB_ISNIL(7);
   BOOL       bBarColor  = !HB_ISNIL(8);
   BOOL       bSmooth    = (!HB_ISLOG(9) ? FALSE : hb_parl(9));
   BOOL       bVertical  = (!HB_ISLOG(10) ? FALSE : hb_parl(10));
   UINT       uiPGid;
   auto usTop = static_cast<USHORT>(hb_parni(2));
   auto usLeft = static_cast<USHORT>(hb_parni(3));
   auto usBottom = static_cast<USHORT>(hb_parni(4));
   auto usRight = static_cast<USHORT>(hb_parni(5));

   InitCommonControls();

   int iOffTop    = !HB_ISNIL(6) ? hb_parvni(6, 1) : 0;
   int iOffLeft   = !HB_ISNIL(6) ? hb_parvni(6, 2) : 0;
   int iOffBottom = !HB_ISNIL(6) ? hb_parvni(6, 3) : 0;
   int iOffRight  = !HB_ISNIL(6) ? hb_parvni(6, 4) : 0;

   if( hb_gt_wvw_GetMainCoordMode() ) {
      hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy    = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy      = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);
   xy.y   -= pWindowData->byLineSpacing;
   iBottom = xy.y - 1 + iOffBottom;
   iRight  = xy.x - 1 + iOffRight;

   uiPGid = LastControlId(usWinNum, WVW_CONTROL_PROGRESSBAR);
   if( uiPGid == 0 ) {
      uiPGid = WVW_ID_BASE_PROGRESSBAR;
   } else {
      uiPGid++;
   }

   if( bVertical ) {
      iStyle = iStyle | PBS_VERTICAL;
   }
   if( bSmooth ) {
      iStyle = iStyle | PBS_SMOOTH;
   }

   hb_winmainArgGet(&hInstance, nullptr, nullptr);

   hWndPG = CreateWindowEx(
      0L,
      PROGRESS_CLASS,
      static_cast<LPSTR>(nullptr),
      WS_CHILD | WS_VISIBLE | static_cast<DWORD>(iStyle),
      iLeft,
      iTop,
      iRight - iLeft + 1,
      iBottom - iTop + 1,
      hWndParent,
      reinterpret_cast<HMENU>(uiPGid),
      static_cast<HINSTANCE>(hInstance),
      static_cast<LPVOID>(nullptr));

   if( hWndPG ) {
      RECT rXB{};
      RECT rOffXB{};

      if( bBackColor ) {
         SendMessage(hWndPG, PBM_SETBKCOLOR, 0, static_cast<LPARAM>(static_cast<COLORREF>(hb_parnl(7))));
      }
      if( bBarColor ) {
         SendMessage(hWndPG, PBM_SETBARCOLOR, 0, static_cast<LPARAM>(static_cast<COLORREF>(hb_parnl(8))));
      }

      SendMessage(hWndPG, PBM_SETRANGE, 0, MAKELPARAM(0, 100));
      SendMessage(hWndPG, PBM_SETPOS, 0, 0);

      rXB.top       = usTop;     rXB.left = usLeft;
      rXB.bottom    = usBottom; rXB.right = usRight;
      rOffXB.top    = iOffTop;     rOffXB.left = iOffLeft;
      rOffXB.bottom = iOffBottom; rOffXB.right = iOffRight;

      AddControlHandle(usWinNum, WVW_CONTROL_PROGRESSBAR, hWndPG, uiPGid, static_cast<PHB_ITEM>(nullptr), rXB, rOffXB, static_cast<byte>(iStyle));

      hb_retnl(static_cast<LONG>(uiPGid));
   } else {
      hb_retnl(0);
   }
}

/*wvw_pgDestroy( [nWinNum], nPGid )
 * destroy progressbar nPGid for window nWinNum
 * This function has no return value.
 */
HB_FUNC( WVW_PGDESTROY )
{
   auto usWinNum = WVW_WHICH_WINDOW;
   auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
   auto uiPGid = static_cast<UINT>(HB_ISNIL(2) ? 0 : hb_parni(2));
   CONTROL_DATA * pcd         = pWindowData->pcdCtrlList;
   CONTROL_DATA * pcdPrev     = nullptr;

   while( pcd ) {
      if( pcd->byCtrlClass == WVW_CONTROL_PROGRESSBAR && pcd->uiCtrlid == uiPGid ) {
         break;
      }

      pcdPrev = pcd;
      pcd     = pcd->pNext;
   }
   if( pcd == nullptr ) {
      return;
   }

   DestroyWindow(pcd->hWndCtrl);

   if( pcdPrev == nullptr ) {
      pWindowData->pcdCtrlList = pcd->pNext;
   } else {
      pcdPrev->pNext = pcd->pNext;
   }

   if( pcd->phiCodeBlock ) {
      hb_itemRelease(pcd->phiCodeBlock);
   }

   hb_xfree(pcd);
}

/* wvw_pgSetRange(nWinNum, PGid, [nMin], [nMax])
 *  update progressbar data range (default is 0-100)
 *  nMin: a number in range of -32767 to +32767
 *  nMax: a number in range of -32767 to +32767
 *
 * Remark: progress position is reset to nMin
 *
 * returns .t. if operation considered successfull
 */
HB_FUNC( WVW_PGSETRANGE )
{
   auto usWinNum = WVW_WHICH_WINDOW;

   auto uiPGid = static_cast<UINT>(HB_ISNIL(2) ? 0 : hb_parni(2));
   byte bStyle;
   auto hWndPG = FindControlHandle(usWinNum, WVW_CONTROL_PROGRESSBAR, uiPGid, &bStyle);
   auto iMin = static_cast<int>((HB_ISNIL(3) ? 0 : hb_parni(3)));
   auto iMax = static_cast<int>((HB_ISNIL(4) ? 0 : hb_parni(4)));

   if( uiPGid == 0 || hWndPG == nullptr || (iMin > iMax) ) {
      hb_retl(false);
      return;
   }

   SendMessage(hWndPG, PBM_SETRANGE, 0, MAKELPARAM(iMin, iMax));
   SendMessage(hWndPG, PBM_SETPOS, static_cast<WPARAM>(iMin), 0);

   hb_retl(true);
}

/*wvw_pgSetPos(nWinNum, PGid, [nPos])
 * update progressbar position within current range
 * nPos: a number in range of current range
 * returns .t. if operation considered successfull
 */
HB_FUNC( WVW_PGSETPOS )
{
   auto usWinNum = WVW_WHICH_WINDOW;

   auto uiPGid = static_cast<UINT>(HB_ISNIL(2) ? 0 : hb_parni(2));
   byte    bStyle;
   auto hWndPG = FindControlHandle(usWinNum, WVW_CONTROL_PROGRESSBAR, uiPGid, &bStyle);
   auto iPos = static_cast<int>((HB_ISNIL(3) ? 0 : hb_parni(3)));
   PBRANGE pbrange;

   if( uiPGid == 0 || hWndPG == nullptr ) {
      hb_retl(false);
      return;
   }

   SendMessage(hWndPG, PBM_GETRANGE, static_cast<WPARAM>(TRUE), reinterpret_cast<LPARAM>(&pbrange));

   if( iPos < pbrange.iLow || iPos > pbrange.iHigh ) {
      hb_retl(false);
      return;
   }

   SendMessage(hWndPG, PBM_SETPOS, static_cast<WPARAM>(iPos), 0);

   hb_retl(true);
}

/*wvw_pgGetPos(nWinNum, PGid)
 * get progressbar current position
 * returns 0 if operation failed
 */
HB_FUNC( WVW_PGGETPOS )
{
   auto usWinNum = WVW_WHICH_WINDOW;

   auto uiPGid = static_cast<UINT>(HB_ISNIL(2) ? 0 : hb_parni(2));
   byte bStyle;
   auto hWndPG = FindControlHandle(usWinNum, WVW_CONTROL_PROGRESSBAR, uiPGid, &bStyle);

   if( uiPGid == 0 || hWndPG == nullptr ) {
      hb_retni(0);
      return;
   }

   hb_retni(static_cast<int>(SendMessage(hWndPG, PBM_GETPOS, 0, 0)));

}

/* PROGRESSBAR ends                                                   */
