/*
 * Video subsystem for Windows using GUI windows instead of Console
 *
 *    Copyright 2007-2012 Pritpal Bedi <bedipritpal@hotmail.com>
 * based on:
 *
 *    Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *   Video subsystem for Windows compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
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

#include "gtwvg.h"

#include <windowsx.h>

/* workaround for missing declaration in MinGW */
#if !defined(TTM_SETTITLE) && defined(TTM_SETTITLEA)
   #define TTM_SETTITLE  TTM_SETTITLEA
#endif

/* Pritpal Bedi <bedipritpal@hotmail.com> */

/*
wvt_ChooseFont(cFontName, nHeight, nWidth, nWeight, nQuality, lItalic, lUnderline, lStrikeout)
--> {cFontName, nHeight, nWidth, nWeight, nQuality, lItalic, lUnderline, lStrikeout, nRGB}
*/
HB_FUNC( WVT_CHOOSEFONT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      PHB_ITEM ary = hb_itemArrayNew(9);

      LONG PointSize = 0;
      COLORREF Colors = 0;
      LOGFONT lf;

      memset(&lf, 0, sizeof(lf));

      {
         CHOOSEFONT cf;

         memset(&cf, 0, sizeof(cf));

         if( HB_ISNUM(2) )
         {
            PointSize = -MulDiv(static_cast<LONG>(hb_parnl(2)), GetDeviceCaps(_s->hdc, LOGPIXELSY), 72);
         }

         lf.lfHeight         = PointSize;
         lf.lfWidth          = hb_parni(3);
         lf.lfWeight         = hb_parni(4);
         lf.lfItalic         = HB_ISNUM(6) ? static_cast<BYTE>(hb_parni(6)) : static_cast<BYTE>(hb_parl(6));
         lf.lfUnderline      = HB_ISNUM(7) ? static_cast<BYTE>(hb_parni(7)) : static_cast<BYTE>(hb_parl(7));
         lf.lfStrikeOut      = HB_ISNUM(8) ? static_cast<BYTE>(hb_parni(8)) : static_cast<BYTE>(hb_parl(8));
         lf.lfCharSet        = DEFAULT_CHARSET;
         lf.lfQuality        = static_cast<BYTE>(hb_parnidef(5, DEFAULT_QUALITY));
         lf.lfPitchAndFamily = FF_DONTCARE;
         if( HB_ISCHAR(1) )
         {
            void * hText;
            HB_STRNCPY(lf.lfFaceName, HB_PARSTR(1, &hText, nullptr), HB_SIZEOFARRAY(lf.lfFaceName) - 1);
            hb_strfree(hText);
         }

         cf.lStructSize    = sizeof(cf);
         cf.hwndOwner      = _s->hWnd;
         cf.hDC            = nullptr;
         cf.lpLogFont      = &lf;
         cf.iPointSize     = 0;
         cf.Flags          = CF_SCREENFONTS | CF_EFFECTS | CF_SHOWHELP | CF_INITTOLOGFONTSTRUCT;
         cf.rgbColors      = RGB(0, 0, 0);
         cf.lCustData      = 0;
         cf.lpfnHook       = nullptr;
         cf.lpTemplateName = nullptr;
         cf.hInstance      = nullptr;
         cf.lpszStyle      = nullptr;
         cf.nFontType      = SCREEN_FONTTYPE;
         cf.nSizeMin       = 0;
         cf.nSizeMax       = 0;

         if( ChooseFont(&cf) )
         {
            PointSize = -MulDiv(lf.lfHeight, 72, GetDeviceCaps(_s->hdc, LOGPIXELSY));
            Colors = cf.rgbColors;
         }
         else
         {
            PointSize = 0;
            memset(&lf, 0, sizeof(lf));
         }
      }

      HB_ARRAYSETSTR(ary, 1, lf.lfFaceName);
      hb_arraySetNL(ary, 2, static_cast<long>(PointSize));
      hb_arraySetNI(ary, 3, lf.lfWidth);
      hb_arraySetNI(ary, 4, lf.lfWeight);
      hb_arraySetNI(ary, 5, lf.lfQuality);
      hb_arraySetL(ary, 6, lf.lfItalic);
      hb_arraySetL(ary, 7, lf.lfUnderline);
      hb_arraySetL(ary, 8, lf.lfStrikeOut);
      hb_arraySetNInt(ary, 9, Colors);

      hb_itemReturnRelease(ary);
   }
}

/* Tooltips */

HB_FUNC( WVT_SETTOOLTIPACTIVE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retl(_s->bToolTipActive);

      if( HB_ISLOG(1) )
      {
         _s->bToolTipActive = hb_parl(1);
      }
   }
   else
   {
      hb_retl(HB_FALSE);
   }
}

/*
wvt_SetToolTip(nTop, nLeft, nBottom, nRight, cToolText)
*/
HB_FUNC( WVT_SETTOOLTIP )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      if( _s->bToolTipActive )
      {
         TOOLINFO ti;
         POINT xy = { 0, 0 };

         memset(&ti, 0, sizeof(ti));

         ti.cbSize = sizeof(ti);
         ti.hwnd = _s->hWnd;
         ti.uId = 100000;

         if( SendMessage(_s->hWndTT, TTM_GETTOOLINFO, 0, reinterpret_cast<LPARAM>(&ti)) )
         {
            int iTop, iLeft, iBottom, iRight;
            void * hText;

            xy = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));
            iTop = xy.y;
            iLeft = xy.x;

            xy = hb_wvt_gtGetXYFromColRow(hb_parni(4) + 1, hb_parni(3) + 1);
            iBottom = xy.y - 1;
            iRight = xy.x - 1;

            ti.lpszText    = static_cast<LPTSTR>(HB_UNCONST(HB_PARSTR(5, &hText, nullptr)));
            ti.rect.left   = iLeft;
            ti.rect.top    = iTop;
            ti.rect.right  = iRight;
            ti.rect.bottom = iBottom;

            SendMessage(_s->hWndTT, TTM_SETTOOLINFO, 0, reinterpret_cast<LPARAM>(&ti));

            hb_strfree(hText);
         }
      }
   }
}

HB_FUNC( WVT_SETTOOLTIPTEXT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      TOOLINFO ti;

      memset(&ti, 0, sizeof(ti));

      ti.cbSize = sizeof(ti);
      ti.hwnd = _s->hWnd;
      ti.uId = 100000;

      if( SendMessage(_s->hWndTT, TTM_GETTOOLINFO, 0, reinterpret_cast<LPARAM>(&ti)) )
      {
         void * hText;
         ti.lpszText = static_cast<LPTSTR>(HB_UNCONST(HB_PARSTR(1, &hText, nullptr)));
         SendMessage(_s->hWndTT, TTM_UPDATETIPTEXT, 0, reinterpret_cast<LPARAM>(&ti));
         hb_strfree(hText);
      }
   }
}

HB_FUNC( WVT_SETTOOLTIPMARGIN )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      RECT rc;

      rc.left   = hb_parni(2);
      rc.top    = hb_parni(1);
      rc.right  = hb_parni(4);
      rc.bottom = hb_parni(3);

      SendMessage(_s->hWndTT, TTM_SETMARGIN, 0, reinterpret_cast<LPARAM>(&rc));
   }
}

HB_FUNC( WVT_SETTOOLTIPWIDTH )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retni(static_cast<int>(SendMessage(_s->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0)));

      if( HB_ISNUM(1) )
      {
         SendMessage(_s->hWndTT, TTM_SETMAXTIPWIDTH, 0, static_cast<LPARAM>(hb_parnint(1)));
      }

      return;
   }
   hb_retni(0);
}

HB_FUNC( WVT_SETTOOLTIPBKCOLOR )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hbwapi_ret_COLORREF(SendMessage(_s->hWndTT, TTM_GETTIPBKCOLOR, 0, 0));

      if( HB_ISNUM(1) )
      {
         SendMessage(_s->hWndTT, TTM_SETTIPBKCOLOR, static_cast<WPARAM>(hbwapi_par_COLORREF(1)), 0);
      }

      return;
   }
   hbwapi_ret_COLORREF(0);
}

HB_FUNC( WVT_SETTOOLTIPTEXTCOLOR )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hbwapi_ret_COLORREF(SendMessage(_s->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0));

      if( HB_ISNUM(1) )
      {
         SendMessage(_s->hWndTT, TTM_SETTIPTEXTCOLOR, static_cast<WPARAM>(hbwapi_par_COLORREF(1)), 0);
      }

      return;
   }
   hbwapi_ret_COLORREF(0);
}

HB_FUNC( WVT_SETTOOLTIPTITLE )
{
#if ( _WIN32_IE > 0x400 )
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      if( HB_ISCHAR(2) )
      {
         void * hText;

         int iIcon = hb_parni(1);
         if( iIcon > 3 )
         {
            iIcon = 0;
         }

         SendMessage(_s->hWndTT, TTM_SETTITLE, static_cast<WPARAM>(iIcon), reinterpret_cast<LPARAM>(HB_PARSTR(2, &hText, nullptr)));
         hb_strfree(hText);
      }
   }
#endif
}

HB_FUNC( WVT_GETTOOLTIPWIDTH )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retni(static_cast<int>(SendMessage(_s->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0)));
      return;
   }
   hb_retni(0);
}

HB_FUNC( WVT_GETTOOLTIPBKCOLOR )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hbwapi_ret_COLORREF(SendMessage(_s->hWndTT, TTM_GETTIPBKCOLOR, 0, 0));
      return;
   }
   hbwapi_ret_COLORREF(0);
}

HB_FUNC( WVT_GETTOOLTIPTEXTCOLOR )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hbwapi_ret_COLORREF(SendMessage(_s->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0));
      return;
   }
   hbwapi_ret_COLORREF(0);
}

HB_FUNC( WVT_SETGUI )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retl(_s->bGui);

      if( HB_ISLOG(1) )
      {
         _s->bGui = hb_parl(1);
      }
   }
   else
   {
      hb_retl(HB_FALSE);
   }
}

HB_FUNC( WVT_SETMOUSEPOS )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      POINT xy = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));

      if( ClientToScreen(_s->hWnd, &xy) )
      {
         hb_retl(SetCursorPos(xy.x, xy.y + (_s->PTEXTSIZE.y / 2)));
         return;
      }
   }

   hb_retl(HB_FALSE);
}

HB_FUNC( WVT_GETPAINTRECT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      PHB_ITEM info = hb_itemArrayNew(4);

      hb_arraySetNI(info, 1, _s->rowStart);
      hb_arraySetNI(info, 2, _s->colStart);
      hb_arraySetNI(info, 3, _s->rowStop);
      hb_arraySetNI(info, 4, _s->colStop);

      hb_itemReturnRelease(info);
   }
}

HB_FUNC( WVT_SETPOINTER )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      HCURSOR hCursor;

      switch( hb_parni(1) )
      {
         case  1: hCursor = LoadCursor(nullptr, IDC_ARROW); break;
         case  2: hCursor = LoadCursor(nullptr, IDC_IBEAM); break;
         case  3: hCursor = LoadCursor(nullptr, IDC_WAIT); break;
         case  4: hCursor = LoadCursor(nullptr, IDC_CROSS); break;
         case  5: hCursor = LoadCursor(nullptr, IDC_UPARROW); break;
         case  6: hCursor = LoadCursor(nullptr, IDC_SIZE); break;
         case  7: hCursor = LoadCursor(nullptr, IDC_ICON); break;
         case  8: hCursor = LoadCursor(nullptr, IDC_SIZENWSE); break;
         case  9: hCursor = LoadCursor(nullptr, IDC_SIZENESW); break;
         case 10: hCursor = LoadCursor(nullptr, IDC_SIZEWE); break;
         case 11: hCursor = LoadCursor(nullptr, IDC_SIZENS); break;
         case 12: hCursor = LoadCursor(nullptr, IDC_SIZEALL); break;
         case 13: hCursor = LoadCursor(nullptr, IDC_NO); break;
         case 14: hCursor = LoadCursor(nullptr, IDC_HAND); break;
         case 15: hCursor = LoadCursor(nullptr, IDC_APPSTARTING); break;
         case 16: hCursor = LoadCursor(nullptr, IDC_HELP); break;
         default: hCursor = LoadCursor(nullptr, IDC_ARROW);
      }

      #ifndef GCL_HCURSOR
      #define GCL_HCURSOR  GCLP_HCURSOR
      #endif

      SetClassLongPtr(_s->hWnd, GCL_HCURSOR, reinterpret_cast<LONG_PTR>(hCursor));
   }
}

HB_FUNC( WVT_SETMOUSEMOVE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retl(_s->MouseMove);

      if( HB_ISLOG(1) )
      {
         _s->MouseMove = hb_parl(1);
      }
   }
   else
   {
      hb_retl(HB_FALSE);
   }
}

HB_FUNC( WVT_GETXYFROMROWCOL )
{
   PHB_ITEM info = hb_itemArrayNew(2);
   POINT xy = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));

   hb_arraySetNL(info, 1, xy.x);
   hb_arraySetNL(info, 2, xy.y);

   hb_itemReturnRelease(info);
}

HB_FUNC( WVT_GETFONTINFO )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      PHB_ITEM info = hb_itemArrayNew(7);

      HB_ARRAYSETSTR(info, 1, _s->fontFace);
      hb_arraySetNL(info, 2, _s->fontHeight);
      hb_arraySetNL(info, 3, _s->fontWidth);
      hb_arraySetNL(info, 4, _s->fontWeight);
      hb_arraySetNL(info, 5, _s->fontQuality);
      hb_arraySetNL(info, 6, _s->PTEXTSIZE.y);
      hb_arraySetNL(info, 7, _s->PTEXTSIZE.x);

      hb_itemReturnRelease(info);
   }
}

/* Peter Rees <peter@rees.co.nz> */

HB_FUNC( WVT_SETMENU )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      RECT wi = { 0, 0, 0, 0 };
      RECT ci = { 0, 0, 0, 0 };
      RECT rc = { 0, 0, 0, 0 };
      int height, width;

      SetMenu(_s->hWnd, hbwapi_par_raw_HMENU(1));

      GetWindowRect(_s->hWnd, &wi);
      GetClientRect(_s->hWnd, &ci);

      height = static_cast<int>(_s->PTEXTSIZE.y * _s->ROWS);
      width  = static_cast<int>(_s->PTEXTSIZE.x * _s->COLS);

      width  += static_cast<int>(wi.right - wi.left - ci.right);
      height += static_cast<int>(wi.bottom - wi.top - ci.bottom);

      if( _s->CentreWindow && SystemParametersInfo(SPI_GETWORKAREA, 0, &rc, 0) )
      {
         wi.left = rc.left + ((rc.right - rc.left - width) / 2);
         wi.top = rc.top + ((rc.bottom - rc.top - height) / 2);
      }
      SetWindowPos(_s->hWnd, nullptr, wi.left, wi.top, width, height, SWP_NOZORDER);
   }
}

HB_FUNC( WVT_SETPOPUPMENU )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hbwapi_ret_raw_HANDLE(_s->hPopup);

      _s->hPopup = hbwapi_par_raw_HMENU(1);
   }
   else
   {
      hbwapi_ret_raw_HANDLE(0);
   }
}

HB_FUNC( WVT_GETLASTMENUEVENT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   hb_retni(_s ? _s->LastMenuEvent : 0);
}

HB_FUNC( WVT_SETLASTMENUEVENT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retni(_s->LastMenuEvent);

      if( HB_ISNUM(1) )
      {
         _s->LastMenuEvent = hb_parni(1);
      }
   }
   else
   {
      hb_retni(0);
   }
}

HB_FUNC( WVT_SETMENUKEYEVENT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retni(_s->MenuKeyEvent);

      if( HB_ISNUM(1) )
      {
         _s->MenuKeyEvent = hb_parni(1);
      }
   }
   else
   {
      hb_retni(0);
   }
}

HB_FUNC( WVT_DRAWMENUBAR )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      DrawMenuBar(_s->hWnd);
   }
}

HB_FUNC( WVT_ENABLESHORTCUTS )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      hb_retl(_s->EnableShortCuts);

      if( HB_ISLOG(1) )
      {
         _s->EnableShortCuts = hb_parl(1);
      }
   }
   else
   {
      hb_retl(HB_FALSE);
   }
}

HB_FUNC( WVT_INVALIDATERECT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      RECT rc;
      POINT xy;

      xy        = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));
      rc.top    = xy.y;
      rc.left   = xy.x;
      xy        = hb_wvt_gtGetXYFromColRow(hb_parni(4) + 1, hb_parni(3) + 1);
      rc.bottom = xy.y - 1;
      rc.right  = xy.x - 1;

      InvalidateRect(_s->hWnd, &rc, TRUE);
   }
}

HB_FUNC( WVT_CLIENTTOSCREEN )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      PHB_ITEM info = hb_itemArrayNew(2);
      POINT xy = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));

      ClientToScreen(_s->hWnd, &xy);

      hb_arraySetNL(info, 1, xy.x);
      hb_arraySetNL(info, 2, xy.y);

      hb_itemReturnRelease(info);
   }
}

/* Modeless Dialogs Implementation */

static INT_PTR CALLBACK hb_wvt_gtDlgProcMLess(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();
   INT_PTR lReturn = 0;

   if( _s )
   {
      int iIndex, iType;
      PHB_ITEM pFunc = nullptr;

      iType = 0;

      for( iIndex = 0; iIndex < static_cast<int>(HB_SIZEOFARRAY(_s->hDlgModeless)); iIndex++ )
      {
         if( _s->hDlgModeless[iIndex] != nullptr && _s->hDlgModeless[iIndex] == hDlg )
         {
            if( _s->pFunc[iIndex] != nullptr )
            {
               pFunc = _s->pFunc[iIndex];
               iType = _s->iType[iIndex];
            }
            break;
         }
      }

      if( pFunc )
      {
         switch( iType )
         {
            case 1: /* Function Name */
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushDynSym(static_cast<PHB_DYNS>(pFunc));
                  hb_vmPushNil();
                  hbwapi_vmPush_HANDLE(hDlg);
                  hb_vmPushNumInt(message);
                  hb_vmPushNumInt(wParam);
                  hb_vmPushNumInt(lParam);
                  hb_vmDo(4);
                  lReturn = static_cast<INT_PTR>(hbwapi_par_RESULT(-1));
                  hb_vmRequestRestore();
               }
               break;

            case 2: /* Block */
               /* eval the codeblock */
               if( HB_IS_EVALITEM(pFunc) )
               {
                  if( hb_vmRequestReenter() )
                  {
                     hb_vmPushEvalSym();
                     hb_vmPush(_s->pFunc[iIndex]);
                     hbwapi_vmPush_HANDLE(hDlg);
                     hb_vmPushNumInt(message);
                     hb_vmPushNumInt(wParam);
                     hb_vmPushNumInt(lParam);
                     hb_vmSend(4);
                     lReturn = static_cast<INT_PTR>(hbwapi_par_RESULT(-1));
                     hb_vmRequestRestore();
                  }
               }
               else
               {
                  /* TODO: internal error: missing codeblock */
               }
               break;
         }

         switch( message )
         {
            case WM_COMMAND:
               switch( LOWORD(wParam) )
               {
                  case IDOK:
                     DestroyWindow(hDlg);
                     lReturn = 1;
                     break;

                  case IDCANCEL:
                     DestroyWindow(hDlg);
                     lReturn = 0;
                     break;
               }
               break;

            case WM_NCDESTROY:
               if( _s->pFunc[iIndex] != nullptr && _s->iType[iIndex] == 2 )
               {
                  hb_itemRelease(static_cast<PHB_ITEM>(_s->pFunc[iIndex]));
               }
               _s->hDlgModeless[iIndex] = nullptr;
               _s->pFunc[iIndex] = nullptr;
               _s->iType[iIndex] = 0;
               lReturn = 0;
               break;
         }
      }
   }

   return lReturn;
}

static INT_PTR CALLBACK hb_wvt_gtDlgProcModal(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();
   INT_PTR lReturn = 0;

   if( _s )
   {
      int iIndex, iType;
      PHB_ITEM pFunc = nullptr;
      int iFirst = static_cast<int>(lParam);

      if( iFirst > 0 && iFirst <= static_cast<int>(HB_SIZEOFARRAY(_s->hDlgModal)) )
      {
         _s->hDlgModal[iFirst - 1] = hDlg;
         SendMessage(hDlg, WM_INITDIALOG, 0, 0);
         return lReturn;
      }

      iType = 0;

      for( iIndex = 0; iIndex < static_cast<int>(HB_SIZEOFARRAY(_s->hDlgModal)); iIndex++ )
      {
         if( _s->hDlgModal[iIndex] != nullptr && _s->hDlgModal[iIndex] == hDlg )
         {
            if( _s->pFuncModal[iIndex] != nullptr )
            {
               pFunc = _s->pFuncModal[iIndex];
               iType = _s->iTypeModal[iIndex];
            }
            break;
         }
      }

      if( pFunc )
      {
         switch( iType )
         {
            case 1: /* Function Name */
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushDynSym(static_cast<PHB_DYNS>(pFunc));
                  hb_vmPushNil();
                  hbwapi_vmPush_HANDLE(hDlg);
                  hb_vmPushNumInt(message);
                  hb_vmPushNumInt(wParam);
                  hb_vmPushNumInt(lParam);
                  hb_vmDo(4);
                  lReturn = static_cast<INT_PTR>(hbwapi_par_RESULT(-1));
                  hb_vmRequestRestore();
               }
               break;

            case 2: /* Block */
               if( HB_IS_EVALITEM(pFunc) )  /* eval the codeblock */
               {
                  if( hb_vmRequestReenter() )
                  {
                     hb_vmPushEvalSym();
                     hb_vmPush(pFunc);
                     hbwapi_vmPush_HANDLE(hDlg);
                     hb_vmPushNumInt(message);
                     hb_vmPushNumInt(wParam);
                     hb_vmPushNumInt(lParam);
                     hb_vmSend(4);
                     lReturn = static_cast<INT_PTR>(hbwapi_par_RESULT(-1));
                     hb_vmRequestRestore();
                  }
               }
               else
               {
                  /* TODO: internal error: missing codeblock */
               }
               break;
         }

         switch( message )
         {
            case WM_COMMAND:
               switch( LOWORD(wParam) )
               {
                  case IDOK:
                     EndDialog(hDlg, IDOK);
                     lReturn = 0;
                     break;

                  case IDCANCEL:
                     EndDialog(hDlg, IDCANCEL);
                     lReturn = 0;
                     break;
               }
               break;

            case WM_NCDESTROY:
               if( _s->pFuncModal[iIndex] != nullptr && _s->iTypeModal[iIndex] == 2 )
               {
                  hb_itemRelease(static_cast<PHB_ITEM>(_s->pFuncModal[iIndex]));
               }
               _s->hDlgModal[iIndex] = nullptr;
               _s->pFuncModal[iIndex] = nullptr;
               _s->iTypeModal[iIndex] = 0;
               lReturn = 0;
               break;
         }
      }
   }

   return lReturn;
}

/* Dialogs */

HB_FUNC( WVT_CREATEDIALOGDYNAMIC )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();
   HWND hDlg = 0;

   if( _s )
   {
      int iIndex;

      /* check if we still have room for a new dialog */
      for( iIndex = 0; iIndex < static_cast<int>(HB_SIZEOFARRAY(_s->hDlgModeless)); iIndex++ )
      {
         if( _s->hDlgModeless[iIndex] == nullptr )
         {
            break;
         }
      }

      if( iIndex < static_cast<int>(HB_SIZEOFARRAY(_s->hDlgModeless)) )
      {
         PHB_ITEM pFirst = hb_param(3, Harbour::Item::ANY);
         PHB_ITEM pFunc = nullptr;
         PHB_DYNS pExecSym;
         int iType = 0;
         int iResource = hb_parni(4);

         if( HB_IS_EVALITEM(pFirst) )
         {
            /* pFunc is pointing to stored code block (later) */
            pFunc = hb_itemNew(pFirst);
            iType = 2;
         }
         else if( HB_IS_STRING(pFirst) )
         {
            pExecSym = hb_dynsymFindName(hb_itemGetCPtr(pFirst));
            if( pExecSym )
            {
               pFunc = static_cast<PHB_ITEM>(pExecSym);
            }
            iType = 1;
         }

         if( hbwapi_is_HANDLE(3) )
         {
            /* argument 1 is already unicode compliant, so no conversion */
            hDlg = CreateDialogIndirect(GetModuleHandle(nullptr), reinterpret_cast<LPCDLGTEMPLATE>(hb_parc(1)), hb_parl(2) ? _s->hWnd : nullptr, hbwapi_par_raw_DLGPROC(3));
         }
         else
         {
            switch( iResource )
            {
               case 0:
               {
                  void * hTemplate;
                  hDlg = CreateDialog(GetModuleHandle(nullptr), HB_PARSTR(1, &hTemplate, nullptr), hb_parl(2) ? _s->hWnd : nullptr, static_cast<DLGPROC>(hb_wvt_gtDlgProcMLess));
                  hb_strfree(hTemplate);
               }
               break;

               case 1:
                  hDlg = CreateDialog(GetModuleHandle(nullptr), MAKEINTRESOURCE(hb_parni(1)), hb_parl(2) ? _s->hWnd : nullptr, static_cast<DLGPROC>(hb_wvt_gtDlgProcMLess));
                  break;

               case 2:
                  /* argument 1 is already unicode compliant, so no conversion */
                  hDlg = CreateDialogIndirect(GetModuleHandle(nullptr), reinterpret_cast<LPCDLGTEMPLATE>(hb_parc(1)), hb_parl(2) ? _s->hWnd : nullptr, static_cast<DLGPROC>(hb_wvt_gtDlgProcMLess));
                  break;
            }
         }

         if( hDlg )
         {
            _s->hDlgModeless[iIndex] = hDlg;

            if( pFunc )
            {
               /* if codeblock, store the codeblock and lock it there */
               if( HB_IS_EVALITEM( pFirst ) )
               {
                  _s->pcbFunc[iIndex] = pFunc;
               }

               _s->pFunc[iIndex] = pFunc;
               _s->iType[iIndex] = iType;
            }
            else
            {
               _s->pFunc[iIndex] = nullptr;
               _s->iType[iIndex] = 0;
            }
            SendMessage(hDlg, WM_INITDIALOG, 0, 0);
         }
         else
         {
            /* if codeblock item created earlier, release it */
            if( iType == 2 && pFunc )
            {
               hb_itemRelease(pFunc);
            }

            _s->hDlgModeless[iIndex] = nullptr;
         }
      }
   }

   hbwapi_ret_raw_HANDLE(hDlg);
}

HB_FUNC( WVT_CREATEDIALOGMODAL )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();
   HB_PTRDIFF iResult = 0;

   if( _s )
   {
      int iIndex;

      /* check if we still have room for a new dialog */
      for( iIndex = 0; iIndex < static_cast<int>(HB_SIZEOFARRAY(_s->hDlgModal)); iIndex++ )
      {
         if( _s->hDlgModal[iIndex] == nullptr )
         {
            break;
         }
      }

      if( iIndex < static_cast<int>(HB_SIZEOFARRAY(_s->hDlgModal)) )
      {
         PHB_ITEM pFirst = hb_param(3, Harbour::Item::ANY);
         PHB_ITEM pFunc = nullptr;
         PHB_DYNS pExecSym;
         int iResource = hb_parni(4);
         HWND hParent = hbwapi_is_HANDLE(5) ? hbwapi_par_raw_HWND(5) : _s->hWnd;

         if( HB_IS_EVALITEM(pFirst) )
         {
            /* pFunc is pointing to stored code block (later) */
            _s->pcbFuncModal[iIndex] = hb_itemNew(pFirst);

            pFunc = _s->pcbFuncModal[iIndex];
            _s->pFuncModal[iIndex] = pFunc;
            _s->iTypeModal[iIndex] = 2;
         }
         else if( HB_IS_STRING(pFirst) )
         {
            pExecSym = hb_dynsymFindName(hb_itemGetCPtr(pFirst));
            if( pExecSym )
            {
               pFunc = static_cast<PHB_ITEM>(pExecSym);
            }
            _s->pFuncModal[iIndex] = pFunc;
            _s->iTypeModal[iIndex] = 1;
         }

         switch( iResource )
         {
            case 0:
            {
               void * hTemplate;
               iResult = DialogBoxParam(GetModuleHandle(nullptr), HB_PARSTR(1, &hTemplate, nullptr), hParent, static_cast<DLGPROC>(hb_wvt_gtDlgProcModal), static_cast<LPARAM>(static_cast<DWORD>(iIndex)) + 1);
               hb_strfree(hTemplate);
            }
            break;

            case 1:
               iResult = DialogBoxParam(GetModuleHandle(nullptr), MAKEINTRESOURCE(hb_parni(1)), hParent, static_cast<DLGPROC>(hb_wvt_gtDlgProcModal), static_cast<LPARAM>(static_cast<DWORD>(iIndex)) + 1);
               break;

            case 2:
               /* argument 1 is already unicode compliant, so no conversion */
               iResult = DialogBoxIndirectParam(GetModuleHandle(nullptr), reinterpret_cast<LPCDLGTEMPLATE>(hb_parc(1)), hParent, static_cast<DLGPROC>(hb_wvt_gtDlgProcModal), static_cast<LPARAM>(static_cast<DWORD>(iIndex)) + 1);
               break;
         }
      }
   }

   hb_retnint(iResult);
}

HB_FUNC( WVT_LBADDSTRING )
{
   void * hText;

   hb_retni(ListBox_AddString(GetDlgItem(hbwapi_par_raw_HWND(1), hb_parni(2) ), HB_PARSTR(3, &hText, nullptr)));

   hb_strfree(hText);
}

HB_FUNC( WVT_LBGETCOUNT )
{
   hb_retni(ListBox_GetCount(GetDlgItem(hbwapi_par_raw_HWND(1), hb_parni(2))));
}

HB_FUNC( WVT_LBDELETESTRING )
{
   hb_retni(ListBox_DeleteString(GetDlgItem(hbwapi_par_raw_HWND(1), hb_parni(2)), hb_parni(3)));
}

HB_FUNC( WVT_LBSETCURSEL )
{
   hb_retni(ListBox_SetCurSel(GetDlgItem(hbwapi_par_raw_HWND(1), hb_parni(2)), hb_parni(3)));
}

HB_FUNC( WVT_CBADDSTRING )
{
   void * hText;

   hb_retni(static_cast<int>(SendMessage(GetDlgItem(hbwapi_par_raw_HWND(1), hb_parni(2)), CB_ADDSTRING, 0, reinterpret_cast<LPARAM>(HB_PARSTR(3, &hText, nullptr)))));

   hb_strfree(hText);
}

HB_FUNC( WVT_CBSETCURSEL )
{
   hb_retni(static_cast<int>(SendMessage(GetDlgItem(hbwapi_par_raw_HWND(1), hb_parni(2)), CB_SETCURSEL, hb_parni(3), 0)));
}

HB_FUNC( WVT_GETFONTHANDLE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   if( _s )
   {
      HFONT hFont = 0;
      int iSlot = hb_parni(1) - 1;

      if( iSlot >= 0 && iSlot < static_cast<int>(HB_SIZEOFARRAY(_s->pGUI->hUserFonts)) )
      {
         hFont = _s->pGUI->hUserFonts[iSlot];
      }

      hbwapi_ret_raw_HANDLE(hFont);
   }
   else
   {
      hbwapi_ret_raw_HANDLE(0);
   }
}

HB_FUNC( WVG_N2P )  /* NOTE: Unsafe: allows to pass arbitrary pointers to functions, potentially causing a crash or worse. */
{
   hb_retptr(HB_ISPOINTER(1) ? hb_parptr(1) : reinterpret_cast<void *>(static_cast<HB_PTRUINT>(hb_parnint(1))));
}
