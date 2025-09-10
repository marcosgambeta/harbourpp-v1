/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW draw functions
 * GTWVW is initially created based on:
 * =Id: gtwvt.c,v 1.60 2004-01-26 08:14:07 vouchcac Exp =
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

#include "hbgtwvw.hpp"

static COLORREF _COLORS[] = {BLACK,      BLUE,           GREEN,      CYAN,        RED,          MAGENTA,
                             BROWN,      WHITE,          LIGHT_GRAY, BRIGHT_BLUE, BRIGHT_GREEN, BRIGHT_CYAN,
                             BRIGHT_RED, BRIGHT_MAGENTA, YELLOW,     BRIGHT_WHITE};

/*
 *
 *    wvw_DrawLabelObj( [nWinNum], nTop, nLeft, nBottom, nRight, cLabel, nAlignHorz, nAlignVert, nTextColor, nBkColor,
 * hFont, aOffset )
 */

HB_FUNC(WVW_DRAWLABELOBJ)
{
  auto usWinNum = WVW_WHICH_WINDOW;

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  POINT xy{};
  int iTop, iLeft, iBottom, iRight, x, y;
  int iOffTop, iOffLeft, iOffBottom, iOffRight;
  RECT rect{};
  int oldTextAlign, iAlignHorz, iAlignVert, iAlignH = 0, iAlignV;
  COLORREF oldBkColor, oldTextColor;
  UINT uiOptions;
  SIZE sz{};

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  iOffTop = !HB_ISNIL(12) ? hb_parvni(12, 1) : 0;
  iOffLeft = !HB_ISNIL(12) ? hb_parvni(12, 2) : 0;
  iOffBottom = !HB_ISNIL(12) ? hb_parvni(12, 3) : 0;
  iOffRight = !HB_ISNIL(12) ? hb_parvni(12, 4) : 0;

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + iOffTop;
  iLeft = xy.x + iOffLeft;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1 + 1 + iOffBottom;
  iRight = xy.x - 1 + 1 + iOffRight;

  iAlignHorz = HB_ISNIL(7) ? 0 : hb_parni(7);
  iAlignVert = HB_ISNIL(8) ? 0 : hb_parni(8);

  oldTextColor =
      SetTextColor(pWindowData->hdc, HB_ISNIL(9) ? pWindowData->foreground : static_cast<COLORREF>(hb_parnl(9)));
  oldBkColor =
      SetBkColor(pWindowData->hdc, HB_ISNIL(10) ? pWindowData->background : static_cast<COLORREF>(hb_parnl(10)));
  auto oldFont = static_cast<HFONT>(SelectObject(pWindowData->hdc, reinterpret_cast<HFONT>(HB_PARHANDLE(11))));

  GetTextExtentPoint32(pWindowData->hdc, hb_parcx(6), strlen(hb_parcx(6)), &sz);

  x = iLeft;
  y = iTop;

  switch (iAlignHorz) {
  case 0:
    iAlignH = TA_LEFT;
    break;
  case 1:
    iAlignH = TA_RIGHT;
    x = iRight;
    break;
  case 2:
    iAlignH = TA_CENTER;
    x = iLeft + ((iRight - iLeft + 1) / 2);
    break;
  }

  iAlignV = TA_TOP;

  switch (iAlignVert) {
  case 1:
    y = iBottom - sz.cy;
    break;
  case 2:
    y = iTop + ((iBottom - iTop + 1 - sz.cy) / 2);
    break;
  }

  oldTextAlign = SetTextAlign(pWindowData->hdc, iAlignH | iAlignV);

  rect.top = iTop;
  rect.left = iLeft;
  rect.bottom = iBottom;
  rect.right = iRight;

  uiOptions = ETO_CLIPPED | ETO_OPAQUE;

  ExtTextOut(pWindowData->hdc, x, y, uiOptions, &rect, hb_parcx(6), strlen(hb_parcx(6)), nullptr);

  SelectObject(pWindowData->hdc, oldFont);
  SetTextAlign(pWindowData->hdc, oldTextAlign);
  SetBkColor(pWindowData->hdc, oldBkColor);
  SetTextColor(pWindowData->hdc, oldTextColor);

  hb_retl(true);
}

/*                                                                                       */
/*    wvw_DrawToolButtonState( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOff, nState )*/
/*                                                                                       */
HB_FUNC(WVW_DRAWTOOLBUTTONSTATE)
{
  auto usWinNum = WVW_WHICH_WINDOW;

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto s_sApp = hb_gt_wvwGetAppData();
  POINT xy{};
  int iTop, iLeft, iBottom, iRight;
  auto iState = hb_parni(7);

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + hb_parvni(6, 1);
  iLeft = xy.x + hb_parvni(6, 2);

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1 + hb_parvni(6, 3);
  iRight = xy.x - 1 + hb_parvni(6, 4);

  switch (iState) {
  case 0:
    SelectObject(pWindowData->hdc, s_sApp->penGray);

    MoveToEx(pWindowData->hdc, iRight, iTop, nullptr); /* Right       */
    LineTo(pWindowData->hdc, iRight, iBottom + 1);

    MoveToEx(pWindowData->hdc, iLeft, iBottom, nullptr); /* Bottom      */
    LineTo(pWindowData->hdc, iRight, iBottom);

    MoveToEx(pWindowData->hdc, iLeft, iTop, nullptr); /* Left        */
    LineTo(pWindowData->hdc, iLeft, iBottom);

    MoveToEx(pWindowData->hdc, iLeft, iTop, nullptr); /* Top         */
    LineTo(pWindowData->hdc, iRight, iTop);
    break;

  case 1:
    SelectObject(pWindowData->hdc, s_sApp->penBlack);

    MoveToEx(pWindowData->hdc, iRight, iTop, nullptr); /* Right       */
    LineTo(pWindowData->hdc, iRight, iBottom + 1);

    MoveToEx(pWindowData->hdc, iLeft, iBottom, nullptr); /* Bottom      */
    LineTo(pWindowData->hdc, iRight, iBottom);

    SelectObject(pWindowData->hdc, s_sApp->penWhite);

    MoveToEx(pWindowData->hdc, iLeft, iTop, nullptr); /* Left        */
    LineTo(pWindowData->hdc, iLeft, iBottom);

    MoveToEx(pWindowData->hdc, iLeft, iTop, nullptr); /* Top         */
    LineTo(pWindowData->hdc, iRight, iTop);
    break;

  case 2:
    SelectObject(pWindowData->hdc, s_sApp->penWhite);

    MoveToEx(pWindowData->hdc, iRight, iTop, nullptr); /* Right       */
    LineTo(pWindowData->hdc, iRight, iBottom + 1);

    MoveToEx(pWindowData->hdc, iLeft, iBottom, nullptr); /* Bottom      */
    LineTo(pWindowData->hdc, iRight, iBottom);

    SelectObject(pWindowData->hdc, s_sApp->penBlack);

    MoveToEx(pWindowData->hdc, iLeft, iTop, nullptr); /* Left        */
    LineTo(pWindowData->hdc, iLeft, iBottom);

    MoveToEx(pWindowData->hdc, iLeft, iTop, nullptr); /* Top         */
    LineTo(pWindowData->hdc, iRight, iTop);
    break;
  }
  hb_retl(true);
}

/*                                                                                                 */
/*   wvw_DrawScrollButton( [nWinNum], nTop, nLeft, nBottom, nRight, aOffPixels, nTLBR, lDepressed )*/
/*                                                                                                 */
/* NOTE: with WVW_XB (scrollbar) this function does not seem to be usefull */
HB_FUNC(WVW_DRAWSCROLLBUTTON)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto s_sApp = hb_gt_wvwGetAppData();
  int iTop, iLeft, iBottom, iRight;
  POINT xy{};
  int iHeight, iOff;
  BOOL bDepressed = HB_ISNIL(8) ? FALSE : hb_parl(8);

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + hb_parvni(6, 1);
  iLeft = xy.x + hb_parvni(6, 2);

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1 + hb_parvni(6, 3);
  iRight = xy.x - 1 + hb_parvni(6, 4);

  auto Point = static_cast<POINT *>(hb_xgrab(3 * sizeof(POINT)));
  iOff = 6;

  iHeight = iBottom - iTop + 1;

  if (bDepressed) {
    hb_gt_wvwDrawBoxRecessed(usWinNum, iTop + 1, iLeft + 1, iBottom - 2, iRight - 2, FALSE);
  } else {
    hb_gt_wvwDrawBoxRaised(usWinNum, iTop + 1, iLeft + 1, iBottom - 2, iRight - 2, FALSE);
  }
  SelectObject(pWindowData->hdc, s_sApp->solidBrush);

  switch (hb_parni(7)) {
  case 1:
    xy.y = iTop + iOff - 1;
    xy.x = iLeft + ((iRight - iLeft + 1) / 2);
    Point[0] = xy;
    xy.y = iBottom - iOff - 1;
    xy.x = iLeft + iOff - 1;
    Point[1] = xy;
    xy.x = iRight - iOff + 1;
    Point[2] = xy;
    break;

  case 2:
    xy.y = iTop + ((iBottom - iTop + 1) / 2);
    xy.x = iLeft + iOff;
    Point[0] = xy;
    xy.x = iRight - iOff - 1;
    xy.y = iTop + iOff - 1;
    Point[1] = xy;
    xy.y = iBottom - iOff + 1;
    Point[2] = xy;
    break;

  case 3:
    xy.x = iLeft + ((iRight - iLeft + 1) / 2);
    xy.y = iBottom - iOff;
    Point[0] = xy;
    xy.x = iLeft + iOff - 1;
    xy.y = iBottom - iHeight + iOff + 1;
    Point[1] = xy;
    xy.x = iRight - iOff + 1;
    Point[2] = xy;
    break;

  case 4:
    xy.x = iRight - iOff - 1;
    xy.y = iTop + ((iBottom - iTop + 1) / 2);
    Point[0] = xy;
    xy.x = iLeft + iOff + 1;
    xy.y = iTop + iOff - 1;
    Point[1] = xy;
    xy.y = iBottom - iOff + 1;
    Point[2] = xy;
    break;
  }

  Polygon(pWindowData->hdc, Point, 3);

  hb_xfree(Point);
}

/*                                                                                              */
/*  Wvw_DrawScrollbarThumbVert( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlScroll, nThumbPos )*/
/*                                                                                              */
/* NOTE: with WVW_XB (scrollbar) this function does not seem to be usefull */
HB_FUNC(WVW_DRAWSCROLLTHUMBVERT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto s_sApp = hb_gt_wvwGetAppData();
  POINT xy{};
  int iTop, iLeft, iBottom, iRight;
  int iTabTop, iTabLft, iTabBtm, iTabRgt;

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  auto usTabTop = static_cast<USHORT>(hb_parni(7));

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
    hb_wvw_HBFUNCPrologue(usWinNum, &usTabTop, nullptr, nullptr, nullptr);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + hb_parvni(6, 1);
  iLeft = xy.x + hb_parvni(6, 2);

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1 + hb_parvni(6, 3);
  iRight = xy.x - 1 + hb_parvni(6, 4);

  SetBkColor(pWindowData->hdc, RGB(230, 230, 230));
  SelectObject(pWindowData->hdc, s_sApp->diagonalBrush);

  SelectObject(pWindowData->hdc, s_sApp->penNull);
  Rectangle(pWindowData->hdc, iLeft, iTop, iRight + 1, iBottom + 1);

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft /* dummy */, usTabTop);
  iTabTop = xy.y;

  iTabLft = iLeft;
  iTabBtm = iTabTop + pWindowData->PTEXTSIZE.y - 1;
  iTabRgt = iRight;

  SelectObject(pWindowData->hdc, s_sApp->wvwWhiteBrush);
  SelectObject(pWindowData->hdc, s_sApp->penGray);
  Rectangle(pWindowData->hdc, iTabLft, iTabTop, iTabRgt + 1, iTabBtm);

  hb_gt_wvwDrawBoxRaised(usWinNum, iTabTop + 1, iTabLft + 1, iTabBtm - 2, iTabRgt - 2, FALSE);
}

/*                                                                                              */
/*  Wvw_DrawScrollbarThumbHorz( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOffset, nThumbPos )*/
/*                                                                                              */
/* NOTE: with WVW_XB (scrollbar) this function does not seem to be usefull */
HB_FUNC(WVW_DRAWSCROLLTHUMBHORZ)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto s_sApp = hb_gt_wvwGetAppData();
  POINT xy{};
  int iThumbLeft, iThumbRight;
  int iTop, iLeft, iBottom, iRight;

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));
  auto usThumbLeft = static_cast<USHORT>(hb_parni(7));

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
    hb_wvw_HBFUNCPrologue(usWinNum, nullptr, &usThumbLeft, nullptr, nullptr);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + hb_parvni(6, 1);
  iLeft = xy.x + hb_parvni(6, 2);

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1 + hb_parvni(6, 3);
  iRight = xy.x - 1 + hb_parvni(6, 4);

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usThumbLeft, usTop /* dummy */);
  iThumbLeft = xy.x;

  iThumbRight = iThumbLeft + (pWindowData->PTEXTSIZE.x * 2) - 1;

  SetBkColor(pWindowData->hdc, RGB(230, 230, 230));
  SelectObject(pWindowData->hdc, s_sApp->diagonalBrush);
  SelectObject(pWindowData->hdc, s_sApp->penNull);
  Rectangle(pWindowData->hdc, iLeft, iTop, iRight + 1, iBottom + 1);

  SelectObject(pWindowData->hdc, s_sApp->wvwWhiteBrush);
  SelectObject(pWindowData->hdc, s_sApp->penGray);
  Rectangle(pWindowData->hdc, iThumbLeft, iTop, iThumbRight, iBottom);
  hb_gt_wvwDrawBoxRaised(usWinNum, iTop + 1, iThumbLeft + 1, iBottom - 2, iThumbRight - 2, FALSE);
}

/*                                                                                                      */
/*    wvw_DrawShadedRect( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOffSet, nHorVert, aRGBb, aRGBe  )*/
/*                                                                                                      */
#if 0
HB_FUNC(WVW_DRAWSHADEDRECT)
{
   auto usWinNum = WVW_WHICH_WINDOW;
   auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
   auto s_sApp = hb_gt_wvwGetAppData();
   BOOL       bGF         = FALSE;

   auto usTop = static_cast<USHORT>(hb_parni(2));
   auto usLeft = static_cast<USHORT>(hb_parni(3));
   auto usBottom = static_cast<USHORT>(hb_parni(4));
   auto usRight = static_cast<USHORT>(hb_parni(5));

   if( hb_gt_wvw_GetMainCoordMode() ) {
      hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   if( s_sApp->hMSImg32 ) {
      TRIVERTEX     vert[2];
      GRADIENT_RECT gRect{};

      int   iMode = HB_ISNIL(7) ? GRADIENT_FILL_RECT_H : hb_parni(7);
      POINT xy{};
      int   iTop, iLeft, iBottom, iRight;

      xy    = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
      iTop  = xy.y + hb_parvni(6, 1);
      iLeft = xy.x + hb_parvni(6, 2);

      xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

      xy.y -= pWindowData->byLineSpacing;

      iBottom = xy.y - 1 + hb_parvni(6, 3);
      iRight  = xy.x - 1 + hb_parvni(6, 4);

      vert[0].x     = iLeft;
      vert[0].y     = iTop;
      vert[0].Red   = static_cast<COLOR16>(hb_parvni(8, 1));
      vert[0].Green = static_cast<COLOR16>(hb_parvni(8, 2));
      vert[0].Blue  = static_cast<COLOR16>(hb_parvni(8, 3));
      vert[0].Alpha = static_cast<COLOR16>(hb_parvni(8, 4));

      vert[1].x     = iRight;
      vert[1].y     = iBottom;
      vert[1].Red   = static_cast<COLOR16>(hb_parvni(9, 1));
      vert[1].Green = static_cast<COLOR16>(hb_parvni(9, 2));
      vert[1].Blue  = static_cast<COLOR16>(hb_parvni(9, 3));
      vert[1].Alpha = static_cast<COLOR16>(hb_parvni(9, 4));

      gRect.UpperLeft  = 0;
      gRect.LowerRight = 1;

      bGF = static_cast<BOOL>(s_sApp->pfnGF(pWindowData->hdc, vert, 2, &gRect, 1, iMode));
   }
   hb_retl(bGF);
}
#endif

HB_FUNC(WVW_DRAWSHADEDRECT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  BOOL bGF = FALSE;

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  TRIVERTEX vert[2];
  GRADIENT_RECT gRect{};

  int iMode = HB_ISNIL(7) ? GRADIENT_FILL_RECT_H : hb_parni(7);
  POINT xy{};
  int iTop, iLeft, iBottom, iRight;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + hb_parvni(6, 1);
  iLeft = xy.x + hb_parvni(6, 2);

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1 + hb_parvni(6, 3);
  iRight = xy.x - 1 + hb_parvni(6, 4);

  vert[0].x = iLeft;
  vert[0].y = iTop;
  vert[0].Red = static_cast<COLOR16>(hb_parvni(8, 1));
  vert[0].Green = static_cast<COLOR16>(hb_parvni(8, 2));
  vert[0].Blue = static_cast<COLOR16>(hb_parvni(8, 3));
  vert[0].Alpha = static_cast<COLOR16>(hb_parvni(8, 4));

  vert[1].x = iRight;
  vert[1].y = iBottom;
  vert[1].Red = static_cast<COLOR16>(hb_parvni(9, 1));
  vert[1].Green = static_cast<COLOR16>(hb_parvni(9, 2));
  vert[1].Blue = static_cast<COLOR16>(hb_parvni(9, 3));
  vert[1].Alpha = static_cast<COLOR16>(hb_parvni(9, 4));

  gRect.UpperLeft = 0;
  gRect.LowerRight = 1;

  bGF = GradientFill(pWindowData->hdc, vert, 2, &gRect, 1, iMode);

  hb_retl(bGF);
}

/*                                                                                 */
/*   wvw_DrawTextBox( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOffSet, cText, ;*/
/*                    nAlignHorz, nAlignVert, nTextColor, nBackColor, ;            */
/*                    nBackMode, hFont )                                           */
/*                                                                                 */
HB_FUNC(WVW_DRAWTEXTBOX)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  POINT xy{};
  int iTop, iLeft, iBottom, iRight;

  int iAlignHorz = HB_ISNIL(8) ? 0 : hb_parni(8);

  int iAlignH = 0;

  COLORREF oldTextColor, oldBkColor;
  int oldTextAlign, oldBkMode;
  RECT rc{};

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + hb_parvni(6, 1);
  iLeft = xy.x + hb_parvni(6, 2);

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1 + hb_parvni(6, 3);
  iRight = xy.x - 1 + hb_parvni(6, 4);

  switch (iAlignHorz) {
  case 0:
    iAlignH = DT_LEFT;
    break;
  case 1:
    iAlignH = DT_RIGHT;
    break;
  case 2:
    iAlignH = DT_CENTER;
    break;
  }

  rc.top = iTop;
  rc.left = iLeft;
  rc.bottom = iBottom;
  rc.right = iRight;

  oldTextAlign = SetTextAlign(pWindowData->hdc, TA_TOP | TA_LEFT | TA_NOUPDATECP);
  oldTextColor =
      SetTextColor(pWindowData->hdc, HB_ISNIL(10) ? pWindowData->foreground : static_cast<COLORREF>(hb_parnl(10)));
  oldBkColor =
      SetBkColor(pWindowData->hdc, HB_ISNIL(11) ? pWindowData->background : static_cast<COLORREF>(hb_parnl(11)));
  oldBkMode = SetBkMode(pWindowData->hdc, HB_ISNIL(12) ? OPAQUE : hb_parni(12));
  auto oldFont = static_cast<HFONT>(SelectObject(pWindowData->hdc, reinterpret_cast<HFONT>(HB_PARHANDLE(13))));

  DrawText(pWindowData->hdc, hb_parcx(7), strlen(hb_parcx(7)), &rc, iAlignH | DT_WORDBREAK | DT_TOP);

  SetTextColor(pWindowData->hdc, oldTextColor);
  SetBkColor(pWindowData->hdc, oldBkColor);
  SetBkMode(pWindowData->hdc, oldBkMode);
  SetTextAlign(pWindowData->hdc, oldTextAlign);
  SelectObject(pWindowData->hdc, oldFont);
}

/*
 *
 * wvw_DrawProgressBar( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlTLBR, nPercent,;
 *                      nBackColor, nBarColor, cImage, lVertical, nDirection )
 */
HB_FUNC(WVW_DRAWPROGRESSBAR)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto pWinMainData = hb_gt_wvw_GetWindowsData(0);
  auto s_sApp = hb_gt_wvwGetAppData();
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));
  int iTop;
  int iLeft;
  int iBottom;
  int iRight;
  int iBarUpto, iDirection;
  BOOL bVertical, bImage;

  COLORREF crBarColor;
  HBRUSH hBrush;
  LOGBRUSH lb{};
  RECT rc{};
  POINT xy{};

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + hb_parvni(6, 1);
  iLeft = xy.x + hb_parvni(6, 2);

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1 + hb_parvni(6, 3);
  iRight = xy.x - 1 + hb_parvni(6, 4);

  auto iPercent = hb_parni(7);
  bImage = HB_ISNIL(10) ? FALSE : TRUE;
  bVertical = HB_ISNIL(11) ? FALSE : hb_parl(11);
  iDirection = HB_ISNIL(12) ? 0 : hb_parni(12);

  if (bVertical) {
    if (iDirection == 0) {
      iBarUpto = iTop + ((iBottom - iTop) * iPercent / 100);
      rc.top = iTop;
      rc.left = iLeft;
      rc.bottom = iBarUpto;
      rc.right = iRight;
    } else {
      iBarUpto = iBottom - ((iBottom - iTop) * iPercent / 100);
      rc.top = iBarUpto;
      rc.left = iLeft;
      rc.bottom = iBottom;
      rc.right = iRight;
    }
  } else {
    if (iDirection == 0) {
      iBarUpto = iLeft + ((iRight - iLeft) * iPercent / 100);
      rc.top = iTop;
      rc.left = iLeft;
      rc.bottom = iBottom;
      rc.right = iBarUpto;
    } else {
      iBarUpto = iRight - ((iRight - iLeft) * iPercent / 100);
      rc.top = iTop;
      rc.left = iBarUpto;
      rc.bottom = iBottom;
      rc.right = iRight;
    }
  }

  if (bImage) {
    hb_gt_wvwDrawImage(usWinNum, rc.left, rc.top, rc.right - rc.left + 1, rc.bottom - rc.top + 1, hb_parc(10), FALSE);
  } else {
    crBarColor = HB_ISNIL(9) ? hb_gt_wvwGetColorData(0) : static_cast<COLORREF>(hb_parnl(9));

    lb.lbStyle = BS_SOLID;
    lb.lbColor = crBarColor;
    lb.lbHatch = 0;

    hBrush = CreateBrushIndirect(&lb);

    rc.bottom++;
    rc.right++;
    FillRect(pWindowData->hdc, &rc, hBrush);

    SelectObject(pWinMainData->hdc, static_cast<HBRUSH>(s_sApp->OriginalBrush));
    DeleteObject(hBrush);
  }
}

/*
 *
 *   wvw_DrawBoxGet([nWinNum], nRow, nCol, nWidth,;
 *                   aOffset)   <-- additional parm, not exist in GTWVT
 *
 * NOTES: unlike GTWVT, GTWVW draw white lines on outer right and outer bottom
 *       Besides, scope is the same as DRAWBOXRECESSED, ie.
 *       two pixel out of char boundary
 */

HB_FUNC(WVW_DRAWBOXGET)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  POINT xy{};
  POINT yz{};
  auto s_sApp = hb_gt_wvwGetAppData();
  int iTop, iLeft, iBottom, iRight;
  int iOffTop, iOffLeft, iOffBottom, iOffRight;

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  auto usRow = static_cast<USHORT>(hb_parni(2));
  auto usCol = static_cast<USHORT>(hb_parni(3));
  auto usLen = static_cast<USHORT>(hb_parni(4));

  iOffTop = !HB_ISNIL(5) ? hb_parvni(5, 1) : 0;
  iOffLeft = !HB_ISNIL(5) ? hb_parvni(5, 2) : 0;
  iOffBottom = !HB_ISNIL(5) ? hb_parvni(5, 3) : 0;
  iOffRight = !HB_ISNIL(5) ? hb_parvni(5, 4) : 0;

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usRow, &usCol, nullptr, nullptr);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usCol, usRow);
  iTop = xy.y - 1 + iOffTop;
  iLeft = xy.x - 1 + iOffLeft;

  yz = hb_gt_wvwGetXYFromColRow(pWindowData, usCol + usLen, usRow + 1);

  yz.y -= pWindowData->byLineSpacing;

  iBottom = yz.y + iOffBottom;
  iRight = yz.x + iOffRight;

  SelectObject(pWindowData->hdc, s_sApp->penBlack);

  MoveToEx(pWindowData->hdc, iLeft, iTop, nullptr);

  LineTo(pWindowData->hdc, iRight, iTop);

  MoveToEx(pWindowData->hdc, iLeft, iTop, nullptr);

  LineTo(pWindowData->hdc, iLeft, iBottom);

  SelectObject(pWindowData->hdc, s_sApp->penDarkGray);

  MoveToEx(pWindowData->hdc, iLeft - 1, iTop - 1, nullptr);

  LineTo(pWindowData->hdc, iRight + 1, iTop - 1);

  MoveToEx(pWindowData->hdc, iLeft - 1, iTop - 1, nullptr);

  LineTo(pWindowData->hdc, iLeft - 1, iBottom + 1);

  /* GTWVW also draws right and bottom outer with single white line */
  SelectObject(pWindowData->hdc, s_sApp->penWhite);

  MoveToEx(pWindowData->hdc, iRight + 1, iTop - 1, nullptr);
  LineTo(pWindowData->hdc, iRight + 1, iBottom + 1 + 1);

  MoveToEx(pWindowData->hdc, iLeft - 1, iBottom + 1, nullptr);
  LineTo(pWindowData->hdc, iRight + 1, iBottom + 1);

  MoveToEx(pWindowData->hdc, iLeft, iBottom, nullptr);
  LineTo(pWindowData->hdc, iRight, iBottom);

  MoveToEx(pWindowData->hdc, iRight, iTop, nullptr);
  LineTo(pWindowData->hdc, iRight, iBottom + 1);

  hb_retl(true);
}

/*
 *
 *   wvw_DrawBoxGet_XP( [nWinNum], nRow, nCol, nWidth,;
 *                   aOffset )   <-- additional parm, not exist in GTWVT
 *
 * NOTES: unlike GTWVT, GTWVW draw white lines on outer right and outer bottom
 *       Besides, scope is the same as DRAWBOXRECESSED, ie.
 *       two pixel out of char boundary
 */

HB_FUNC(WVW_DRAWBOXGET_XP)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  POINT xy{};
  POINT yz{};
  auto s_sApp = hb_gt_wvwGetAppData();
  int iTop, iLeft, iBottom, iRight;
  int iOffTop, iOffLeft, iOffBottom, iOffRight;

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  auto usRow = static_cast<USHORT>(hb_parni(2));
  auto usCol = static_cast<USHORT>(hb_parni(3));
  auto usLen = static_cast<USHORT>(hb_parni(4));

  iOffTop = !HB_ISNIL(5) ? hb_parvni(5, 1) : 0;
  iOffLeft = !HB_ISNIL(5) ? hb_parvni(5, 2) : 0;
  iOffBottom = !HB_ISNIL(5) ? hb_parvni(5, 3) : 0;
  iOffRight = !HB_ISNIL(5) ? hb_parvni(5, 4) : 0;

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usRow, &usCol, nullptr, nullptr);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usCol, usRow);
  iTop = xy.y - 1 + iOffTop;
  iLeft = xy.x - 1 + iOffLeft;

  yz = hb_gt_wvwGetXYFromColRow(pWindowData, usCol + usLen, usRow + 1);

  yz.y -= pWindowData->byLineSpacing;

  iBottom = yz.y + iOffBottom;
  iRight = yz.x + iOffRight;

  SelectObject(pWindowData->hdc, s_sApp->penGray);

  MoveToEx(pWindowData->hdc, iLeft - 1, iTop - 1, nullptr);
  LineTo(pWindowData->hdc, iRight + 1, iTop - 1);

  MoveToEx(pWindowData->hdc, iLeft - 1, iTop - 1, nullptr);
  LineTo(pWindowData->hdc, iLeft - 1, iBottom + 1);

  MoveToEx(pWindowData->hdc, iLeft - 1, iBottom + 1, nullptr);
  LineTo(pWindowData->hdc, iRight + 1, iBottom + 1);

  MoveToEx(pWindowData->hdc, iRight + 1, iTop - 1, nullptr);
  LineTo(pWindowData->hdc, iRight + 1, iBottom + 1);

  hb_retl(true);
}

/*                                                                  */
/*   wvw_DrawBoxRaised(nWinNum,                                     */
/*                     nTop, nLeft, nBottom, nRight,                */
/*                     lTight/aOffset)                              */
/*                                                                  */
/*   if lTight, box is drawn inside the character region                               */
/*   AND top and left lines are lower two pixel down to make room for above/left object*/
/*   WARNING: gui object of this type subject to be overwritten by chars               */
/*   NOTE that these lines are to be overwritten by displayed char,                    */
/*        we are depending on the fact that gui object will be painted last            */
/*                                                                                     */
/*   lTight may be replaced with aOffset parm {top,left,bottom,right}                  */
/*     ie. offset in pixel unit                                                        */
/*                                                                                     */

HB_FUNC(WVW_DRAWBOXRAISED)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  POINT xy{};
  int iTop, iLeft, iBottom, iRight;
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));
  BOOL bUseArray = HB_ISARRAY(6);

  BOOL bTight = (bUseArray || HB_ISNIL(6) ? FALSE : hb_parl(6));
  int iOLeft, iOTop, iORight, iOBottom;

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  if (bTight) {
    iOTop = 2;
    iOLeft = 2;
    iOBottom = -1;
    iORight = -1;
  } else if (bUseArray) {
    iOTop = hb_parvni(6, 1) - 1;
    iOLeft = hb_parvni(6, 2) - 1;
    iOBottom = hb_parvni(6, 3);
    iORight = hb_parvni(6, 4);
  } else {
    iOTop = -1;
    iOLeft = -1;
    iOBottom = 0;
    iORight = 0;
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + iOTop;
  iLeft = xy.x + iOLeft;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y + iOBottom;
  iRight = xy.x + iORight;

  hb_gt_wvwDrawBoxRaised(pWindowData->byWinId, iTop, iLeft, iBottom, iRight, bTight);

  hb_retl(true);
}

/*                                                                                         */
/*    wvw_DrawBoxRecessed(nWinNum, ;                                                      */
/*                   nTop, nLeft, nBottom, nRight,                                         */
/*                   lTight/aOffset) <--none in gtwvt                                              */
/*                                                                                         */
/*   if lTight, box is drawn inside the character region                                   */
/*   AND top and left lines are lower two pixel down to make room for above/left object    */
/*   WARNING: gui object of this type subject to be overwritten by chars                   */
/*   NOTE that these lines are to be overwritten by displayed char,                        */
/*        we are depending on the fact that gui object will be painted last                */
/*                                                                                         */
/*   lTight may be replaced with aOffset parm {top,left,bottom,right}                      */
/*     ie. offset in pixel unit                                                            */
/*                                                                                         */

HB_FUNC(WVW_DRAWBOXRECESSED)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  POINT xy{};
  int iTop, iLeft, iBottom, iRight;
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));
  BOOL bUseArray = HB_ISARRAY(6);

  BOOL bTight = (bUseArray || HB_ISNIL(6) ? FALSE : hb_parl(6));
  int iOLeft, iOTop, iORight, iOBottom;

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  if (bTight) {
    iOTop = 2;
    iOLeft = 2;
    iOBottom = -1;
    iORight = -1;
  } else if (bUseArray) {
    iOTop = hb_parvni(6, 1) - 1;
    iOLeft = hb_parvni(6, 2) - 1;
    iOBottom = hb_parvni(6, 3);
    iORight = hb_parvni(6, 4);
  } else {
    iOTop = -1;
    iOLeft = -1;
    iOBottom = 0;
    iORight = 0;
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + iOTop;
  iLeft = xy.x + iOLeft;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y + iOBottom;
  iRight = xy.x + iORight;

  hb_gt_wvwDrawBoxRecessed(pWindowData->byWinId, iTop, iLeft, iBottom, iRight, bTight);

  hb_retl(true);
}

/*
 *
 *    wvw_DrawBoxGroup( nWinNum, ;
 *                   nTop, nLeft, nBottom, nRight,;
 *                   [aOffset] )
 *
 * NOTE: aOffset is TLBR offset in pixel. none in GTWVT
 */

HB_FUNC(WVW_DRAWBOXGROUP)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  POINT xy{};
  int iTop, iLeft, iBottom, iRight;
  int iOffTop, iOffLeft, iOffBottom, iOffRight;
  auto s_sApp = hb_gt_wvwGetAppData();
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  iOffTop = !HB_ISNIL(6) ? hb_parvni(6, 1) : 0;
  iOffLeft = !HB_ISNIL(6) ? hb_parvni(6, 2) : 0;
  iOffBottom = !HB_ISNIL(6) ? hb_parvni(6, 3) : 0;
  iOffRight = !HB_ISNIL(6) ? hb_parvni(6, 4) : 0;

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y - 1 + iOffTop;
  iLeft = xy.x - 1 + iOffLeft;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y + iOffBottom;
  iRight = xy.x + iOffRight;

  SelectObject(pWindowData->hdc, s_sApp->penDarkGray);

  MoveToEx(pWindowData->hdc, iRight, iTop, nullptr); /* Right Inner   */
  LineTo(pWindowData->hdc, iRight, iBottom);

  MoveToEx(pWindowData->hdc, iLeft, iBottom, nullptr); /* Bottom Inner  */
  LineTo(pWindowData->hdc, iRight, iBottom);

  MoveToEx(pWindowData->hdc, iLeft - 1, iTop - 1, nullptr); /* Left Outer    */
  LineTo(pWindowData->hdc, iLeft - 1, iBottom + 1);

  MoveToEx(pWindowData->hdc, iLeft - 1, iTop - 1, nullptr); /* Top Outer     */
  LineTo(pWindowData->hdc, iRight + 1, iTop - 1);

  SelectObject(pWindowData->hdc, s_sApp->penWhite);

  MoveToEx(pWindowData->hdc, iRight + 1, iTop, nullptr); /* Right Outer   */
  LineTo(pWindowData->hdc, iRight + 1, iBottom + 1);

  MoveToEx(pWindowData->hdc, iLeft - 1, iBottom + 1, nullptr); /* Bottom Outer  */
  LineTo(pWindowData->hdc, iRight + 1 + 1, iBottom + 1);

  MoveToEx(pWindowData->hdc, iLeft, iTop, nullptr); /* Left Inner    */
  LineTo(pWindowData->hdc, iLeft, iBottom);

  MoveToEx(pWindowData->hdc, iLeft, iTop, nullptr); /* Top Inner     */
  LineTo(pWindowData->hdc, iRight, iTop);

  hb_retl(true);
}

/*                                                                   */
/*    wvw_DrawBoxRaised(nWinNum, ;                                  */
/*                   nTop, nLeft, nBottom, nRight)                  */
/*                                                                   */

HB_FUNC(WVW_DRAWBOXGROUPRAISED)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  POINT xy{};
  int iTop, iLeft, iBottom, iRight;
  auto s_sApp = hb_gt_wvwGetAppData();
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y - 1;
  iLeft = xy.x - 1;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y;
  iRight = xy.x;

  SelectObject(pWindowData->hdc, s_sApp->penWhite);

  MoveToEx(pWindowData->hdc, iRight, iTop, nullptr); /* Right Inner   */
  LineTo(pWindowData->hdc, iRight, iBottom);

  MoveToEx(pWindowData->hdc, iLeft, iBottom, nullptr); /* Bottom Inner  */
  LineTo(pWindowData->hdc, iRight, iBottom);

  MoveToEx(pWindowData->hdc, iLeft - 1, iTop - 1, nullptr); /* Left Outer    */
  LineTo(pWindowData->hdc, iLeft - 1, iBottom + 1);

  MoveToEx(pWindowData->hdc, iLeft - 1, iTop - 1, nullptr); /* Top Outer     */
  LineTo(pWindowData->hdc, iRight + 1, iTop - 1);

  SelectObject(pWindowData->hdc, s_sApp->penDarkGray);

  MoveToEx(pWindowData->hdc, iRight + 1, iTop, nullptr); /* Right Outer   */
  LineTo(pWindowData->hdc, iRight + 1, iBottom + 1);

  MoveToEx(pWindowData->hdc, iLeft - 1, iBottom + 1, nullptr); /* Bottom Outer  */
  LineTo(pWindowData->hdc, iRight + 1 + 1, iBottom + 1);

  MoveToEx(pWindowData->hdc, iLeft, iTop, nullptr); /* Left Inner    */
  LineTo(pWindowData->hdc, iLeft, iBottom);

  MoveToEx(pWindowData->hdc, iLeft, iTop, nullptr); /* Top Inner     */
  LineTo(pWindowData->hdc, iRight, iTop);

  hb_retl(true);
}

/*                                                                        */
/*    wvw_DrawImage( nWinNum, ;                                           */
/*                   nTop, nLeft, nBottom, nRight, cImage/nPictureSlot, ; */
/*                   lTight/aOffset,;                                     */
/*                   lTransparent) <--none in gtwvt                     */
/*                                                                        */
/* 20060724 Notes:
   (1) Image dimension
   if nBottom is NIL, then image height will be proportional to image width.
   if nRight  is NIL, then image width will be proportional to image height.
   if nBottom and nRight are BOTH NIL then original image dimension is used

   (2) Transparency
   if lTransparent is .t., top-left pixel is used as the transparent color,

   (3) Caching
   Image will always be cached. See the WARNING in hb_gt_wvwDrawImage().
   Do not use this function to draw a large number of images.
   TODO: make it an option.
 */

HB_FUNC(WVW_DRAWIMAGE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto s_sApp = hb_gt_wvwGetAppData();
  POINT xy{};
  int iLeft, iTop, iRight = 0, iBottom = 0;

  BOOL bActBottom = HB_ISNIL(4), bActRight = HB_ISNIL(5);
  int iImgWidth = 0, iImgHeight = 0;

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  BOOL bTight = HB_ISARRAY(7) || HB_ISNIL(7) ? FALSE : hb_parl(7);
  BOOL bUseArray = HB_ISARRAY(7);
  BOOL bTransparent = HB_ISNIL(8) ? FALSE : hb_parl(8);
  int iOLeft, iOTop, iORight, iOBottom;
  BOOL bResult;

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  if (bTight) {
    iOTop = 2 + 1;
    iOLeft = 2 + 1;
    iOBottom = -1;
    iORight = -1;
  } else if (bUseArray) {
    iOTop = hb_parvni(7, 1);
    iOLeft = hb_parvni(7, 2);
    iOBottom = hb_parvni(7, 3);
    iORight = hb_parvni(7, 4);
  } else {
    iOTop = iOLeft = iOBottom = iORight = 0;
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + iOTop;
  iLeft = xy.x + iOLeft;

  if (bActRight || bActBottom) {
    if ((!HB_ISNUM(6) && !GetImageDimension(hb_parcx(6), &iImgWidth, &iImgHeight)) ||
        (HB_ISNUM(6) && !GetIPictDimension(s_sApp->iPicture[hb_parni(6) - 1], &iImgWidth, &iImgHeight))) {

      bActRight = FALSE;
      bActBottom = FALSE;
    } else if (bActRight && bActBottom) {
      iRight = iLeft + iImgWidth - 1;
      iBottom = iTop + iImgHeight - 1;
    }
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  if (!bActBottom) {
    iBottom = xy.y - 1 + iOBottom;
  }

  if (!bActRight) {
    iRight = xy.x - 1 + iORight;
  }

  if ((bActBottom || bActRight) && !(bActBottom && bActRight)) {
    int iDispWidth, iDispHeight;
    if (bActRight) {
      /* right corner (width) must be proportional to height */

      iDispHeight = iBottom - iTop + 1;
      iDispWidth = static_cast<int>(static_cast<float>(iImgWidth) / iImgHeight * iDispHeight);
      iRight = iLeft + iDispWidth - 1;
    } else {
      /* bottom corner (height) must be proportional to width */

      iDispWidth = iRight - iLeft + 1;
      iDispHeight = static_cast<int>(static_cast<float>(iImgHeight) / iImgWidth * iDispWidth);
      iBottom = iTop + iDispHeight - 1;
    }
  }

  if (HB_ISNUM(6)) {
    bResult = hb_gt_wvwRenderPicture(usWinNum, iLeft, iTop, (iRight - iLeft) + 1, (iBottom - iTop) + 1,
                                     s_sApp->iPicture[hb_parni(6) - 1], bTransparent);
  } else {
    bResult = hb_gt_wvwDrawImage(usWinNum, iLeft, iTop, (iRight - iLeft) + 1, (iBottom - iTop) + 1, hb_parcx(6),
                                 bTransparent);
  }

  hb_retl(bResult);
}

/*                                                                                       */
/*    wvw_DrawImage_Resource( nWinNum, ;                                                 */
/*                   nTop, nLeft, nBottom, nRight, nPictureResource/cPictureResource, ;  */
/*                   lTight/aOffset,;                                                    */
/*                   lTransparent) <--none in gtwvt                                      */
/*                                                                                       */
/* 20060724 Notes:
   (1) Image dimension
   if nBottom is NIL, then image height will be proportional to image width.
   if nRight  is NIL, then image width will be proportional to image height.
   if nBottom and nRight are BOTH NIL then original image dimension is used

   (2) Transparency
   if lTransparent is .t., top-left pixel is used as the transparent color,

   (3) Caching
   Image will always be cached. See the WARNING in hb_gt_wvwDrawImage().
   Do not use this function to draw a large number of images.
   TODO: make it an option.
 */

HB_FUNC(WVW_DRAWIMAGE_RESOURCE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  POINT xy{};
  int iLeft, iTop, iRight = 0, iBottom = 0;

  BOOL bActBottom = HB_ISNIL(4), bActRight = HB_ISNIL(5);
  int iImgWidth, iImgHeight;
  LONG lImgWidth, lImgHeight;

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  BOOL bTight = HB_ISARRAY(7) || HB_ISNIL(7) ? FALSE : hb_parl(7);
  BOOL bUseArray = HB_ISARRAY(7);
  BOOL bTransparent = HB_ISNIL(8) ? FALSE : hb_parl(8);
  int iOLeft, iOTop, iORight, iOBottom;
  BOOL bResult = FALSE;
  IPicture *pPic;

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  if (bTight) {
    iOTop = 2 + 1;
    iOLeft = 2 + 1;
    iOBottom = -1;
    iORight = -1;
  } else if (bUseArray) {
    iOTop = hb_parvni(7, 1);
    iOLeft = hb_parvni(7, 2);
    iOBottom = hb_parvni(7, 3);
    iORight = hb_parvni(7, 4);
  } else {
    iOTop = iOLeft = iOBottom = iORight = 0;
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + iOTop;
  iLeft = xy.x + iOLeft;
  lImgWidth = 0;
  lImgHeight = 0;
  iImgWidth = 0;
  iImgHeight = 0;

  if (!HB_ISNUM(6)) {
    pPic = rr_LoadPictureFromResource(hb_parcx(6), 0, &lImgWidth, &lImgHeight);

    if (pPic == nullptr) {
      pPic = rr_LoadPicture(hb_parcx(6), &lImgWidth, &lImgHeight);
    }
  } else {
    pPic = rr_LoadPictureFromResource(nullptr, hb_parni(6), &lImgWidth, &lImgHeight);
  }

#if 0
   lImgWidth  = iImgWidth;
   lImgHeight = iImgHeight;
#endif

  if (pPic == nullptr) {
    hb_retl(bResult);
  }

  if (bActRight || bActBottom) {

    if (!GetIPictDimension(pPic, &iImgWidth, &iImgHeight)) {
      bActRight = FALSE;
      bActBottom = FALSE;
    } else if (bActRight && bActBottom) {
      iRight = iLeft + iImgWidth;
      iBottom = iTop + iImgHeight;
    }
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  if (!bActBottom) {
    iBottom = xy.y - 1 + iOBottom;
  }

  if (!bActRight) {
    iRight = xy.x - 1 + iORight;
  }

  if ((bActBottom || bActRight) && !(bActBottom && bActRight)) {
    int iDispWidth, iDispHeight;
    if (bActRight) {
      /* right corner (width) must be proportional to height */

      iDispHeight = iBottom - iTop + 1;
      iDispWidth = static_cast<int>(static_cast<float>(iImgWidth) / iImgHeight * iDispHeight);
      iRight = iLeft + iDispWidth - 1;
    } else {
      /* bottom corner (height) must be proportional to width */

      iDispWidth = iRight - iLeft + 1;
      iDispHeight = static_cast<int>(static_cast<float>(iImgHeight) / iImgWidth * iDispWidth);
      iBottom = iTop + iDispHeight - 1;
    }
  }

  bResult =
      hb_gt_wvwRenderPicture(usWinNum, iLeft, iTop, (iRight - iLeft) + 1, (iBottom - iTop) + 1, pPic, bTransparent);

  hb_retl(bResult);
}

/*                                                                           */
/*    wvw_DrawLabel( nWinNum, ;                                              */
/*                   nRow, nCol, cLabel, nAlign, nEscapement, nTextColor, ;  */
/*                   nBkColor, cFontFace,nHeight, nWidth, nWeight, ;         */
/*                   nQuality, nCharSet, lItalic, lUnderline, lStrikeOut )   */
/*                                                                           */

HB_FUNC(WVW_DRAWLABEL)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  POINT xy{};
  auto usRow = static_cast<USHORT>(hb_parni(2));
  auto usCol = static_cast<USHORT>(hb_parni(3));

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usRow, &usCol, nullptr, nullptr);
  }

  LOGFONT logfont{};
  logfont.lfEscapement = HB_ISNIL(6) ? 0 : (hb_parni(6) * 10);
  logfont.lfOrientation = 0;
  logfont.lfWeight = HB_ISNIL(12) ? 0 : hb_parni(12);
  logfont.lfItalic = HB_ISNIL(15) ? 0 : static_cast<BYTE>(hb_parl(15));
  logfont.lfUnderline = HB_ISNIL(16) ? 0 : static_cast<BYTE>(hb_parl(16));
  logfont.lfStrikeOut = HB_ISNIL(17) ? 0 : static_cast<BYTE>(hb_parl(17));
  logfont.lfCharSet = HB_ISNIL(14) ? static_cast<BYTE>(pWindowData->CodePage) : static_cast<BYTE>(hb_parni(14));
  logfont.lfOutPrecision = 0;
  logfont.lfClipPrecision = 0;
  logfont.lfQuality = HB_ISNIL(13) ? DEFAULT_QUALITY : static_cast<BYTE>(hb_parni(13));
  logfont.lfPitchAndFamily = FF_DONTCARE;
  logfont.lfHeight = HB_ISNIL(10) ? pWindowData->fontHeight : hb_parni(10);
  logfont.lfWidth =
      HB_ISNIL(11) ? (pWindowData->fontWidth < 0 ? -pWindowData->fontWidth : pWindowData->fontWidth) : hb_parni(11);

  strcpy(logfont.lfFaceName, (HB_ISNIL(9) ? pWindowData->fontFace : hb_parcx(9)));

  auto hFont = CreateFontIndirect(&logfont);
  if (hFont) {
    xy = hb_gt_wvwGetXYFromColRow(pWindowData, usCol, usRow);

    COLORREF oldBkColor =
        SetBkColor(pWindowData->hdc, HB_ISNIL(8) ? pWindowData->background : static_cast<COLORREF>(hb_parnl(8)));
    COLORREF oldTextColor =
        SetTextColor(pWindowData->hdc, HB_ISNIL(7) ? pWindowData->foreground : static_cast<COLORREF>(hb_parnl(7)));
    int oldTextAlign = SetTextAlign(pWindowData->hdc, (HB_ISNIL(5) ? TA_LEFT : hb_parni(5)));
    auto oldFont = static_cast<HFONT>(SelectObject(pWindowData->hdc, hFont));

    ExtTextOut(pWindowData->hdc, xy.x, xy.y, 0, nullptr, hb_parcx(4), strlen(hb_parcx(4)), nullptr);

    SelectObject(pWindowData->hdc, oldFont);
    DeleteObject(hFont);
    SetTextAlign(pWindowData->hdc, oldTextAlign);
    SetBkColor(pWindowData->hdc, oldBkColor);
    SetTextColor(pWindowData->hdc, oldTextColor);

    hb_retl(true);
  }

  hb_retl(false);
}

/*                                                                   */
/*    wvw_DrawOutline( nWinNum, ;                                    */
/*                   nTop, nLeft, nBottom, nRight,                   */
/*                   nThick, nShape, nRGBColor )                     */
/*                                                                   */

HB_FUNC(WVW_DRAWOUTLINE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  HPEN hPen = 0, hOldPen = 0;
  POINT xy{};
  int iTop, iLeft, iBottom, iRight;

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));
  auto s_sApp = hb_gt_wvwGetAppData();

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y - 1;
  iLeft = xy.x - 1;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y;
  iRight = xy.x;

  if (HB_ISNUM(6)) {
    hPen = CreatePen(hb_parni(6), 0, (HB_ISNIL(8) ? 0 : static_cast<COLORREF>(hb_parnl(8))));
    if (hPen) {
      hOldPen = static_cast<HPEN>(SelectObject(pWindowData->hdc, hPen));
    }
  } else {
    /* hPen = 0; */
    SelectObject(pWindowData->hdc, s_sApp->penBlack);
  }

  hb_gt_wvwDrawOutline(usWinNum, iTop, iLeft, iBottom, iRight);

  if (hPen) {
    SelectObject(pWindowData->hdc, hOldPen);
    DeleteObject(hPen);
  }

  hb_retl(true);
}

/*                  1                                                                               */
/*                  2      3       4       5        6        7       8       9      10      11      */
/*                 12                                                                               */
/*   wvw_DrawLine( nWinNum, ;                                                                       */
/*                 nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nStyle, nThick, nColor,; */
/*                 aOffset)                                                                         */
/*                                                                                                  */

HB_FUNC(WVW_DRAWLINE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  POINT xy{};
  int iTop, iLeft, iBottom, iRight, iOffset;
  int iOffTop, iOffLeft, iOffBottom, iOffRight;
  int iOrient, iFormat, iAlign, iStyle, iThick;
  int x, y;
  COLORREF cr;
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  APP_DATA *s_sApp = hb_gt_wvwGetAppData();

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  iOffTop = !HB_ISNIL(12) ? hb_parvni(12, 1) : 0;
  iOffLeft = !HB_ISNIL(12) ? hb_parvni(12, 2) : 0;
  iOffBottom = !HB_ISNIL(12) ? hb_parvni(12, 3) : 0;
  iOffRight = !HB_ISNIL(12) ? hb_parvni(12, 4) : 0;

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + iOffTop;
  iLeft = xy.x + iOffLeft;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1 + iOffBottom;
  iRight = xy.x - 1 + iOffRight;

  iOrient = HB_ISNIL(6) ? 0 : hb_parni(6);
  iFormat = HB_ISNIL(7) ? 0 : hb_parni(7);
  iAlign = HB_ISNIL(8) ? 0 : hb_parni(8);
  iStyle = HB_ISNIL(9) ? 0 : hb_parni(9);
  iThick = HB_ISNIL(10) ? 0 : hb_parni(10);
  cr = HB_ISNIL(11) ? 0 : static_cast<COLORREF>(hb_parnl(11));

  x = iLeft;
  y = iTop;

  switch (iAlign) {
  case 0:               /* Center      */
    if (iOrient == 0) { /* Horizontal  */
      iOffset = ((iBottom - iTop) / 2);
      y = iTop + iOffset;
    } else {
      iOffset = ((iRight - iLeft) / 2);
      x = iLeft + iOffset;
    }
    break;

  case 1: /* Top      */
    break;

  case 2: /* bottom   */
    if (iFormat == 0 || iFormat == 1) {
      y = iBottom - 1;
    } else {
      y = iBottom;
    }
    break;

  case 3: /* Left     */
    break;

  case 4: /* Right    */
    if (iFormat == 0 || iFormat == 1) {
      x = iRight - 1;
    } else {
      x = iRight;
    }
    break;
  }

  auto hPen = CreatePen(iStyle, iThick, cr);
  auto hOldPen = static_cast<HPEN>(SelectObject(pWindowData->hdc, hPen));

  switch (iFormat) {
  case 0:               /* Raised        */
    if (iOrient == 0) { /*  Horizontal   */
      SelectObject(pWindowData->hdc, s_sApp->penWhite);
      MoveToEx(pWindowData->hdc, x, y, nullptr);
      LineTo(pWindowData->hdc, iRight, y);
      SelectObject(pWindowData->hdc, hPen);
      MoveToEx(pWindowData->hdc, x, y + 1, nullptr);
      LineTo(pWindowData->hdc, iRight, y + 1);
    } else { /*  Vertical     */
      SelectObject(pWindowData->hdc, s_sApp->penWhite);
      MoveToEx(pWindowData->hdc, x, y, nullptr);
      LineTo(pWindowData->hdc, x, iBottom);
      SelectObject(pWindowData->hdc, hPen);
      MoveToEx(pWindowData->hdc, x + 1, y, nullptr);
      LineTo(pWindowData->hdc, x + 1, iBottom);
    }
    break;

  case 1:               /* Recessed       */
    if (iOrient == 0) { /* Horizontal     */
      SelectObject(pWindowData->hdc, hPen);
      MoveToEx(pWindowData->hdc, x, y, nullptr);
      LineTo(pWindowData->hdc, iRight, y);
      SelectObject(pWindowData->hdc, s_sApp->penWhite);
      MoveToEx(pWindowData->hdc, x, y + 1, nullptr);
      LineTo(pWindowData->hdc, iRight, y + 1);
    } else { /*  Vertical      */
      SelectObject(pWindowData->hdc, hPen);
      MoveToEx(pWindowData->hdc, x, y, nullptr);
      LineTo(pWindowData->hdc, x, iBottom);
      SelectObject(pWindowData->hdc, s_sApp->penWhite);
      MoveToEx(pWindowData->hdc, x + 1, y, nullptr);
      LineTo(pWindowData->hdc, x + 1, iBottom);
    }
    break;

  case 2:               /* Plain          */
    if (iOrient == 0) { /* Horizontal     */
      SelectObject(pWindowData->hdc, hPen);
      MoveToEx(pWindowData->hdc, x, y, nullptr);
      LineTo(pWindowData->hdc, iRight, y);
    } else { /*  Vertical      */
      SelectObject(pWindowData->hdc, hPen);
      MoveToEx(pWindowData->hdc, x, y, nullptr);
      LineTo(pWindowData->hdc, x, iBottom);
    }
    break;
  }

  SelectObject(pWindowData->hdc, hOldPen);
  DeleteObject(hPen);
  hb_retl(true);
}

/*                                                                   */
/*    Inside the area requested!                                     */
/*    wvw_DrawEllipse( nWinNum, nTop, nLeft, nBottom, nRight ,;      */
/*                     aOffset)                                      */
/*                                                                   */

HB_FUNC(WVW_DRAWELLIPSE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  POINT xy{};
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto s_sApp = hb_gt_wvwGetAppData();
  int iTop, iLeft, iBottom, iRight;
  int iOffTop, iOffLeft, iOffBottom, iOffRight;
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  iOffTop = !HB_ISNIL(6) ? hb_parvni(6, 1) : 0;
  iOffLeft = !HB_ISNIL(6) ? hb_parvni(6, 2) : 0;
  iOffBottom = !HB_ISNIL(6) ? hb_parvni(6, 3) : 0;
  iOffRight = !HB_ISNIL(6) ? hb_parvni(6, 4) : 0;

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + iOffTop;
  iLeft = xy.x + iOffLeft;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1 + iOffBottom;
  iRight = xy.x - 1 + iOffRight;

  SelectObject(pWindowData->hdc, s_sApp->currentBrush);
  SelectObject(pWindowData->hdc, s_sApp->currentPen);

  hb_retl(Ellipse(pWindowData->hdc, iLeft, iTop, iRight, iBottom));
}

/*                                                                   */
/*    wvw_DrawRectangle( nWinNum, nTop, nLeft, nBottom, nRight )     */
/*                     aOffset, lUsaCurrentPen)                      */
/*                                                                   */

HB_FUNC(WVW_DRAWRECTANGLE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  POINT xy{};
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto s_sApp = hb_gt_wvwGetAppData();
  int iTop, iLeft, iBottom, iRight;
  int iOffTop, iOffLeft, iOffBottom, iOffRight;
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));
  /* Ref.: 28454 - Marson de Paula - 2007-11-27 */
  BOOL bUsaCurrentPen = HB_ISNIL(7) ? TRUE : hb_parl(7);

  iOffTop = !HB_ISNIL(6) ? hb_parvni(6, 1) : 0;
  iOffLeft = !HB_ISNIL(6) ? hb_parvni(6, 2) : 0;
  iOffBottom = !HB_ISNIL(6) ? hb_parvni(6, 3) : 0;
  iOffRight = !HB_ISNIL(6) ? hb_parvni(6, 4) : 0;

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + iOffTop;
  iLeft = xy.x + iOffLeft;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1 + iOffBottom;
  iRight = xy.x - 1 + iOffRight;

  SelectObject(pWindowData->hdc, s_sApp->currentBrush);
  /* Ref.: 28454 - Marson de Paula - 2007-11-27 */
  if (bUsaCurrentPen) {
    SelectObject(pWindowData->hdc, s_sApp->currentPen);
  } else {
    SelectObject(pWindowData->hdc, s_sApp->penBlack);
  }

  hb_retl(Rectangle(pWindowData->hdc, iLeft, iTop, iRight, iBottom));
}

/*                                                                                         */
/*    wvw_DrawRoundRect( nWinNum, nTop, nLeft, nBottom, nRight, ;
 *                       aOffset, ; <-- new parm
 *                       nRoundHeight, nRoundWidth */
/*                                                                                         */

/* WARNING!!!
 * unlike previous release of GTWVW, 6th parameter is now aOffset
 * This placement of new parameter is made in line with gtwvt's way of doing it
 */

HB_FUNC(WVW_DRAWROUNDRECT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  POINT xy{};
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto s_sApp = hb_gt_wvwGetAppData();
  int iTop, iLeft, iBottom, iRight, iWd, iHt;
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));
  int iOffTop, iOffLeft, iOffBottom, iOffRight;

  iOffTop = !HB_ISNIL(6) ? hb_parvni(6, 1) : 0;
  iOffLeft = !HB_ISNIL(6) ? hb_parvni(6, 2) : 0;
  iOffBottom = !HB_ISNIL(6) ? hb_parvni(6, 3) : 0;
  iOffRight = !HB_ISNIL(6) ? hb_parvni(6, 4) : 0;

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y + iOffTop;
  iLeft = xy.x + iOffLeft;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1 + iOffBottom;
  iRight = xy.x - 1 + iOffRight;

  iWd = HB_ISNIL(8) ? 0 : hb_parni(8);
  iHt = HB_ISNIL(7) ? 0 : hb_parni(7);

  SelectObject(pWindowData->hdc, s_sApp->currentBrush);
  SelectObject(pWindowData->hdc, s_sApp->currentPen);

  hb_retl(RoundRect(pWindowData->hdc, iLeft, iTop, iRight, iBottom, iWd, iHt));
}

/*                                                                   */
/*    wvw_DrawFocusRect( nWinNum, nTop, nLeft, nBottom, nRight,;     */
/*                 aOffset)                                          */
/*                                                                   */

HB_FUNC(WVW_DRAWFOCUSRECT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  RECT rc{};
  POINT xy{};
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));
  int iOffTop, iOffLeft, iOffBottom, iOffRight;

  iOffTop = !HB_ISNIL(6) ? hb_parvni(6, 1) : 0;
  iOffLeft = !HB_ISNIL(6) ? hb_parvni(6, 2) : 0;
  iOffBottom = !HB_ISNIL(6) ? hb_parvni(6, 3) : 0;
  iOffRight = !HB_ISNIL(6) ? hb_parvni(6, 4) : 0;

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  rc.top = xy.y + iOffTop;
  rc.left = xy.x + iOffLeft;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  rc.bottom = xy.y - 1 + iOffBottom;
  rc.right = xy.x - 1 + iOffRight;

  hb_retl(DrawFocusRect(pWindowData->hdc, &rc));
}

/*NOTE: this is compatibility function with GTWVT
 *      similar with wvw_FillRectangle()
 */
/*
 *
 *   wvw_DrawColorRect( nWinNum, nTop, nLeft, nBottom, nRight, aPxlOff, nRGB )
 */
HB_FUNC(WVW_DRAWCOLORRECT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto pWindowMainData = hb_gt_wvw_GetWindowsData(0);
  auto s_sApp = hb_gt_wvwGetAppData();
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));
  int iOffTop, iOffLeft, iOffBottom, iOffRight;
  RECT rc{};
  POINT xy{};

  iOffTop = !HB_ISNIL(6) ? hb_parvni(6, 1) : 0;
  iOffLeft = !HB_ISNIL(6) ? hb_parvni(6, 2) : 0;
  iOffBottom = !HB_ISNIL(6) ? hb_parvni(6, 3) : 0;
  iOffRight = !HB_ISNIL(6) ? hb_parvni(6, 4) : 0;

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  rc.top = xy.y + iOffTop;
  rc.left = xy.x + iOffLeft;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  rc.bottom = xy.y - 1 + iOffBottom;
  rc.right = xy.x - 1 + iOffRight;

  auto hBrush = CreateSolidBrush(static_cast<COLORREF>(hb_parnl(7)));

  if (hBrush) {
    hb_retl(FillRect(pWindowData->hdc, &rc, hBrush));

    SelectObject(pWindowMainData->hdc, static_cast<HBRUSH>(s_sApp->OriginalBrush));
    DeleteObject(hBrush);
  }
}

/*                                                                   */
/*   wvw_DrawGridHorz( nWinNum, ;                                    */
/*                   nTop, nLeft, nRight, nRows )                    */
/*                                                                   */

HB_FUNC(WVW_DRAWGRIDHORZ)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto usAtRow = static_cast<USHORT>(hb_parni(2));
  auto iRows = hb_parni(5);
  int y;
  int iLeft, iRight;
  auto s_sApp = hb_gt_wvwGetAppData();

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usRight = static_cast<USHORT>(hb_parni(4));

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usAtRow, &usLeft, nullptr, &usRight);
  }

  iLeft = (usLeft * pWindowData->PTEXTSIZE.x);
  iRight = (((usRight + 1) * pWindowData->PTEXTSIZE.x) - 1);

  if (s_sApp->gridPen == nullptr) {
    s_sApp->gridPen = CreatePen(0, 0, GetSysColor(COLOR_BTNFACE));
  }

  SelectObject(pWindowData->hdc, s_sApp->gridPen);

  for (auto i = 0; i < iRows; i++) {
    y = ((usAtRow)*hb_wvw_LineHeight(pWindowData));

    y += pWindowData->usTBHeight;

    MoveToEx(pWindowData->hdc, iLeft, y, nullptr);
    LineTo(pWindowData->hdc, iRight, y);

    usAtRow++;
  }

  hb_retl(true);
}

/*
 *
 *     wvw_DrawGridVert( nWinNum, ;
 *                   nTop, nBottom, aCols, nCols,;
 *                   [aOffset] )
 *
 * NOTE: aOffset is TLBR offset in pixel. none in GTWVT
 *       actually aOffset[4] (Right Offset) is not used here
 */

HB_FUNC(WVW_DRAWGRIDVERT)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  int iTop, iBottom, x;
  int iOffTop, iOffLeft, iOffBottom, iOffRight;
  int iCharHeight, iCharWidth;
  auto iTabs = hb_parni(5);
  USHORT usCol;
  auto s_sApp = hb_gt_wvwGetAppData();

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  if (!iTabs) {
    hb_retl(false);
  }

  iOffTop = !HB_ISNIL(6) ? hb_parvni(6, 1) : 0;
  iOffLeft = !HB_ISNIL(6) ? hb_parvni(6, 2) : 0;
  iOffBottom = !HB_ISNIL(6) ? hb_parvni(6, 3) : 0;
  iOffRight = !HB_ISNIL(6) ? hb_parvni(6, 4) : 0; /* is not actually used */

  HB_SYMBOL_UNUSED(iOffRight);

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usBottom = static_cast<USHORT>(hb_parni(3));
  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, nullptr, &usBottom, nullptr);
  }

  iCharWidth = pWindowData->PTEXTSIZE.x;

  iCharHeight = hb_wvw_LineHeight(pWindowData);

  iTop = (usTop * iCharHeight) + pWindowData->usTBHeight + iOffTop;
  iBottom = ((usBottom + 1) * iCharHeight) - 1 + pWindowData->usTBHeight + iOffBottom;

  if (s_sApp->gridPen == nullptr) {
    s_sApp->gridPen = CreatePen(0, 0, GetSysColor(COLOR_BTNFACE));
  }

  SelectObject(pWindowData->hdc, s_sApp->gridPen);

  for (auto i = 1; i <= iTabs; i++) {
    usCol = static_cast<USHORT>(hb_parvni(4, i));
    if (hb_gt_wvw_GetMainCoordMode()) {
      usCol -= pWindowData->usColOfs;
    }

    x = (usCol * iCharWidth) + iOffLeft;

    MoveToEx(pWindowData->hdc, x, iTop, nullptr);
    LineTo(pWindowData->hdc, x, iBottom);
  }

  hb_retl(true);
}

/*                                                                                  */
/*    wvw_DrawButton( nWinNum, ;                                                    */
/*                   nTop, nLeft, nBottom, nRight, cText, cImage/nImage, nFormat, ; */
/*                    nTextColor, nBkColor, nImageAt )                              */
/*                                                                                  */

HB_FUNC(WVW_DRAWBUTTON)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  SIZE sz{};
  POINT xy{};
  RECT rc{};
  int iTop, iLeft, iBottom, iRight;
  int iAlign, oldTextAlign, oldBkMode;
  int iTextHeight /*, iTextWidth */;
  int iImageWidth, iImageHeight;
  COLORREF /* oldBkColor, */ oldTextColor;
  LOGBRUSH lb{};
  IPicture *iPicture;

  BOOL bText = HB_ISCHAR(6);
  BOOL bImage = !(HB_ISNIL(7));
  int iFormat = HB_ISNIL(8) ? 0 : hb_parni(8);

  COLORREF textColor = HB_ISNIL(9) ? _COLORS[0] : static_cast<COLORREF>(hb_parnl(9));
  COLORREF bkColor = HB_ISNIL(10) ? _COLORS[7] : static_cast<COLORREF>(hb_parnl(10));

  auto pWindowMainData = hb_gt_wvw_GetWindowsData(0);
  auto s_sApp = hb_gt_wvwGetAppData();
  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y;
  iLeft = xy.x;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);

  xy.y -= pWindowData->byLineSpacing;

  iBottom = xy.y - 1;
  iRight = xy.x - 1;

  lb.lbStyle = BS_SOLID;
  lb.lbColor = bkColor;
  lb.lbHatch = 0;

  auto hBrush = CreateBrushIndirect(&lb);

  rc.left = iLeft;
  rc.top = iTop;
  rc.right = iRight + 1;
  rc.bottom = iBottom + 1;

  FillRect(pWindowData->hdc, &rc, hBrush);

  SelectObject(pWindowMainData->hdc, static_cast<HBRUSH>(s_sApp->OriginalBrush));
  DeleteObject(hBrush);

  switch (iFormat) {
  case 1:
    hb_gt_wvwDrawBoxRecessed(pWindowData->byWinId, iTop + 1, iLeft + 1, iBottom - 1, iRight - 1, FALSE);
    break;
  case 2:
    break;
  case 3:
    hb_gt_wvwDrawOutline(pWindowData->byWinId, iTop, iLeft, iBottom, iRight);
    break;
  case 4:
    break;
  default:
    hb_gt_wvwDrawBoxRaised(pWindowData->byWinId, iTop + 1, iLeft + 1, iBottom - 1, iRight - 1, FALSE);
    break;
  }

  if (bText) {
    SelectObject(pWindowData->hdc, GetStockObject(DEFAULT_GUI_FONT));

    GetTextExtentPoint32(pWindowData->hdc, hb_parcx(6), strlen(hb_parcx(6)), &sz);

    iTextHeight = sz.cy;

    xy.x = iLeft + ((iRight - iLeft + 1) / 2);

    if (bImage) {
      xy.y = (iBottom - 2 - iTextHeight);
    } else {
      xy.y = iTop + ((iBottom - iTop + 1 - iTextHeight) / 2);
    }

    if (iFormat == 1) {
      xy.x = xy.x + 2;
      xy.y = xy.y + 2;
    }

    iAlign = TA_CENTER + TA_TOP;

    oldTextAlign = SetTextAlign(pWindowData->hdc, iAlign);
    oldBkMode = SetBkMode(pWindowData->hdc, TRANSPARENT);
    oldTextColor = SetTextColor(pWindowData->hdc, textColor);

    ExtTextOut(pWindowData->hdc, xy.x, xy.y, 0, nullptr, hb_parcx(6), strlen(hb_parcx(6)), nullptr);

    SetTextColor(pWindowData->hdc, oldTextColor);
    SetBkMode(pWindowData->hdc, oldBkMode);
    SetTextAlign(pWindowData->hdc, oldTextAlign);
  } else {
    iTextHeight = -1;
  }

  if (bImage) {
    iImageWidth = (iRight - iLeft + 1 - 8);

    iImageHeight = (iBottom - iTop + 1 - 8 - iTextHeight);

    if (HB_ISNUM(7)) {
      iPicture = s_sApp->iPicture[hb_parni(7) - 1];

      hb_gt_wvwRenderPicture(usWinNum, iLeft + 4, iTop + 4, iImageWidth, iImageHeight, iPicture, FALSE);
    } else {
      hb_gt_wvwDrawImage(usWinNum, iLeft + 4, iTop + 4, iImageWidth, iImageHeight, hb_parcx(7), FALSE);
    }
  }

  hb_retl(true);
}

/* wvw_DrawStatusBar() is meant for WVT compatibility only.
   WVW_SBxxxx() functions are recommended instead.
 */

HB_FUNC(WVW_DRAWSTATUSBAR)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto s_sApp = hb_gt_wvwGetAppData();
  auto iPanels = hb_parni(2);
  int iNext;
  int iTop, iLeft, iBottom, iRight;
  POINT xy{};
  USHORT usTop, usLeft, usBottom, usRight;

  iNext = 0;

  for (auto i = 0; i < iPanels; i++) {
    usTop = static_cast<USHORT>(hb_parvni(3, iNext + 1));
    usLeft = static_cast<USHORT>(hb_parvni(3, iNext + 2));
    usBottom = static_cast<USHORT>(hb_parvni(3, iNext + 3));
    usRight = static_cast<USHORT>(hb_parvni(3, iNext + 4));

    if (hb_gt_wvw_GetMainCoordMode()) {
      hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
    }

    xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
    iTop = xy.y;
    iLeft = xy.x + 1;

    xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight, usBottom + 1);

    xy.y -= pWindowData->byLineSpacing;

    iBottom = xy.y - 1;
    iRight = xy.x - 2;

    SelectObject(pWindowData->hdc, s_sApp->penWhite);

    MoveToEx(pWindowData->hdc, iRight, iTop, nullptr); /* Right  */
    LineTo(pWindowData->hdc, iRight, iBottom);

    MoveToEx(pWindowData->hdc, iLeft, iBottom, nullptr); /* Bottom */
    LineTo(pWindowData->hdc, iRight, iBottom);

    SelectObject(pWindowData->hdc, s_sApp->penDarkGray);

    MoveToEx(pWindowData->hdc, iLeft, iTop, nullptr); /* Left   */
    LineTo(pWindowData->hdc, iLeft, iBottom);

    MoveToEx(pWindowData->hdc, iLeft, iTop, nullptr); /* Top    */
    LineTo(pWindowData->hdc, iRight, iTop);

    iNext = iNext + 4;
  }

  usTop = static_cast<USHORT>(hb_parvni(3, (4 * iPanels) - 1));
  usLeft = static_cast<USHORT>(hb_parvni(3, 4 * iPanels));

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, nullptr, nullptr);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop + 1);

  xy.y -= pWindowData->byLineSpacing;

  iTop = xy.y - 2;
  iLeft = xy.x - 2;
  iBottom = iTop;
  iRight = iLeft;

  SelectObject(pWindowData->hdc, s_sApp->penBlack);

  MoveToEx(pWindowData->hdc, iLeft - 4, iBottom, nullptr);
  LineTo(pWindowData->hdc, iRight, iTop - 4);
  MoveToEx(pWindowData->hdc, iLeft - 7, iBottom, nullptr);
  LineTo(pWindowData->hdc, iRight, iTop - 7);
  MoveToEx(pWindowData->hdc, iLeft - 10, iBottom, nullptr);
  LineTo(pWindowData->hdc, iRight, iTop - 10);

  SelectObject(pWindowData->hdc, s_sApp->penWhite);

  MoveToEx(pWindowData->hdc, iLeft - 5, iBottom, nullptr);
  LineTo(pWindowData->hdc, iRight, iTop - 5);
  MoveToEx(pWindowData->hdc, iLeft - 8, iBottom, nullptr);
  LineTo(pWindowData->hdc, iRight, iTop - 8);
  MoveToEx(pWindowData->hdc, iLeft - 11, iBottom, nullptr);
  LineTo(pWindowData->hdc, iRight, iTop - 11);
}

/*                                                                                        */
/*  wvw_DrawPicture( [nWinNum], nTop, nLeft, nBottom, nRight, nSlot, lTight/aAdj ) -> lOk */
/*  nSlot <= 20  aAdj == { 0,0,-2,-2 } To Adjust the pixels for { Top,Left,Bottom,Right } */
/*                                                                                        */

HB_FUNC(WVW_DRAWPICTURE)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  POINT xy{};
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto s_sApp = hb_gt_wvwGetAppData();
  int iTop, iLeft, iBottom, iRight;

  int iSlot = hb_parni(6) - 1;

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  BOOL bTight = HB_ISARRAY(7) || HB_ISNIL(7) ? FALSE : hb_parl(7);
  BOOL bUseArray = HB_ISARRAY(7);
  int iOLeft, iOTop, iORight, iOBottom;

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  if (bTight) {
    iOTop = 2 + 1;
    iOLeft = 2 + 1;
    iOBottom = -1;
    iORight = -1;
  } else if (bUseArray) {
    iOTop = hb_parvni(7, 1);
    iOLeft = hb_parvni(7, 2);
    iOBottom = hb_parvni(7, 3);
    iORight = hb_parvni(7, 4);
  } else {
    iOTop = iOLeft = iOBottom = iORight = 0;
  }

  if (iSlot < WVW_PICTURES_MAX) {
    if (s_sApp->iPicture[iSlot]) {
      xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
      iTop = xy.y + iOTop;
      iLeft = xy.x + iOLeft;

      xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);
      iBottom = xy.y - 1 + iOBottom;
      iRight = xy.x - 1 + iORight;

      hb_retl(hb_gt_wvwRenderPicture(usWinNum, iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1,
                                     s_sApp->iPicture[iSlot], FALSE));
    }
  }
}

/*                                                                                              */
/*    wvw_DrawLabelEx( [nWinNum], nRow, nCol, cLabel, nAlign, nTextColor, nBkColor, nSlotFont ) */
/*                                                                                              */

HB_FUNC(WVW_DRAWLABELEX)
{
  auto usWinNum = WVW_WHICH_WINDOW;
  auto s_sApp = hb_gt_wvwGetAppData();
  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  POINT xy{};
  HFONT oldFont;
  int oldTextAlign;
  COLORREF oldBkColor, oldTextColor;
  int iSlot = hb_parni(8) - 1;

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, nullptr, nullptr);
  }

  if (s_sApp->hUserFonts[iSlot]) {
    xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);

    oldBkColor =
        SetBkColor(pWindowData->hdc, HB_ISNIL(7) ? pWindowData->background : static_cast<COLORREF>(hb_parnl(7)));
    oldTextColor =
        SetTextColor(pWindowData->hdc, HB_ISNIL(6) ? pWindowData->foreground : static_cast<COLORREF>(hb_parnl(6)));
    oldTextAlign = SetTextAlign(pWindowData->hdc, (HB_ISNIL(5) ? TA_LEFT : hb_parni(5)));
    oldFont = static_cast<HFONT>(SelectObject(pWindowData->hdc, s_sApp->hUserFonts[iSlot]));

    ExtTextOut(pWindowData->hdc, xy.x, xy.y, 0, nullptr, hb_parcx(4), strlen(hb_parcx(4)), nullptr);

    SelectObject(pWindowData->hdc, oldFont);
    SetTextAlign(pWindowData->hdc, oldTextAlign);
    SetBkColor(pWindowData->hdc, oldBkColor);
    SetTextColor(pWindowData->hdc, oldTextColor);

    hb_retl(true);
  }

  hb_retl(false);
}

/*                        1      2       3       4        5        6       7       8      9        */
/*   wvw_DrawLineEx( [nWinNum], nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nSlotPen ) */
/*                                                                                                 */

HB_FUNC(WVW_DRAWLINEEX)
{
  auto usWinNum = WVW_WHICH_WINDOW;

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto s_sApp = hb_gt_wvwGetAppData();
  POINT xy{};
  int iTop, iLeft, iBottom, iRight, iOffset;
  int iOrient, iFormat, iAlign;
  int x, y;
  HPEN hPen;
  int iSlot = hb_parni(9) - 1;

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y;
  iLeft = xy.x;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);
  iBottom = xy.y - 1;
  iRight = xy.x - 1;

  iOrient = HB_ISNIL(6) ? 0 : hb_parni(6);
  iFormat = HB_ISNIL(7) ? 0 : hb_parni(7);
  iAlign = HB_ISNIL(8) ? 0 : hb_parni(8);

  x = iLeft;
  y = iTop;

  switch (iAlign) {
  case 0:               /* Center       */
    if (iOrient == 0) { /* Horizontal   */
      iOffset = ((iBottom - iTop) / 2);
      y = iTop + iOffset;
    } else {
      iOffset = ((iRight - iLeft) / 2);
      x = iLeft + iOffset;
    }
    break;

  case 1: /* Top     */
    break;

  case 2:                               /* bottom  */
    if (iFormat == 0 || iFormat == 1) { /* Raised/Recessd */
      y = iBottom - 1;
    } else {
      y = iBottom;
    }
    break;

  case 3: /* Left    */
    break;

  case 4:                               /* Right   */
    if (iFormat == 0 || iFormat == 1) { /* Raised/Recessd */
      x = iRight - 1;
    } else {
      x = iRight;
    }
    break;
  }

  hPen = s_sApp->hUserPens[iSlot];

  switch (iFormat) {
  case 0:               /* Raised       */
    if (iOrient == 0) { /*  Horizontal  */
      SelectObject(pWindowData->hdc, s_sApp->penWhite);
      MoveToEx(pWindowData->hdc, x, y, nullptr);
      LineTo(pWindowData->hdc, iRight, y);
      SelectObject(pWindowData->hdc, hPen);
      MoveToEx(pWindowData->hdc, x, y + 1, nullptr);
      LineTo(pWindowData->hdc, iRight, y + 1);
    } else { /*  Vertical    */
      SelectObject(pWindowData->hdc, s_sApp->penWhite);
      MoveToEx(pWindowData->hdc, x, y, nullptr);
      LineTo(pWindowData->hdc, x, iBottom);
      SelectObject(pWindowData->hdc, hPen);
      MoveToEx(pWindowData->hdc, x + 1, y, nullptr);
      LineTo(pWindowData->hdc, x + 1, iBottom);
    }
    break;

  case 1:               /* Recessed    */
    if (iOrient == 0) { /* Horizontal  */
      SelectObject(pWindowData->hdc, hPen);
      MoveToEx(pWindowData->hdc, x, y, nullptr);
      LineTo(pWindowData->hdc, iRight, y);
      SelectObject(pWindowData->hdc, s_sApp->penWhite);
      MoveToEx(pWindowData->hdc, x, y + 1, nullptr);
      LineTo(pWindowData->hdc, iRight, y + 1);
    } else { /*  Vertical   */
      SelectObject(pWindowData->hdc, hPen);
      MoveToEx(pWindowData->hdc, x, y, nullptr);
      LineTo(pWindowData->hdc, x, iBottom);
      SelectObject(pWindowData->hdc, s_sApp->penWhite);
      MoveToEx(pWindowData->hdc, x + 1, y, nullptr);
      LineTo(pWindowData->hdc, x + 1, iBottom);
    }
    break;

  case 2:               /* Plain      */
    if (iOrient == 0) { /* Horizontal */
      SelectObject(pWindowData->hdc, hPen);
      MoveToEx(pWindowData->hdc, x, y, nullptr);
      LineTo(pWindowData->hdc, iRight, y);
    } else { /*  Vertical  */
      SelectObject(pWindowData->hdc, hPen);
      MoveToEx(pWindowData->hdc, x, y, nullptr);
      LineTo(pWindowData->hdc, x, iBottom);
    }
    break;
  }

  hb_retl(true);
}

/*                                                                           */
/*    wvw_DrawOutlineEx( [nWinNum], nTop, nLeft, nBottom, nRight, nSlotPen ) */
/*                                                                           */

HB_FUNC(WVW_DRAWOUTLINEEX)
{
  auto usWinNum = WVW_WHICH_WINDOW;

  auto pWindowData = hb_gt_wvw_GetWindowsData(usWinNum);
  auto s_sApp = hb_gt_wvwGetAppData();
  POINT xy{};
  int iTop, iLeft, iBottom, iRight;
  int iSlot = hb_parni(6) - 1;

  auto usTop = static_cast<USHORT>(hb_parni(2));
  auto usLeft = static_cast<USHORT>(hb_parni(3));
  auto usBottom = static_cast<USHORT>(hb_parni(4));
  auto usRight = static_cast<USHORT>(hb_parni(5));

  if (hb_gt_wvw_GetMainCoordMode()) {
    hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
  }

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usLeft, usTop);
  iTop = xy.y - 1;
  iLeft = xy.x - 1;

  xy = hb_gt_wvwGetXYFromColRow(pWindowData, usRight + 1, usBottom + 1);
  iBottom = xy.y;
  iRight = xy.x;

  if (s_sApp->hUserPens[iSlot]) {
    SelectObject(pWindowData->hdc, s_sApp->hUserPens[iSlot]);
  } else {
    SelectObject(pWindowData->hdc, s_sApp->penBlack);
  }

  hb_gt_wvwDrawOutline(usWinNum, iTop, iLeft, iBottom, iRight);
}
