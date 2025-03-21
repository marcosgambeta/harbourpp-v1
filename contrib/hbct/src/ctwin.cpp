//
// Clipper Tools like window system
//
// Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

// NOTE: User programs should never call this layer directly!

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

// This definition has to be placed before #include <hbapigt.hpp>
#define HB_GT_NAME CTW

#include <hbgtcore.hpp>
#include <hbstack.hpp>
#include <hbinit.hpp>
#include <hbapiitm.hpp>
#include <hbapistr.hpp>

#include "ctwin.h"

static int s_GtId;
#undef HB_GTSUPERTABLE
#define HB_GTSUPERTABLE(g) (&(HB_GTCTW_GET(g)->SuperTable))
#define HB_GTID_PTR (&s_GtId)

#define HB_GTCTW_GET(p) (static_cast<PHB_GTCTW>(HB_GTLOCAL(p)))
#define HB_CTW_TSD(p) (static_cast<PHB_CTWDATA>(hb_stackGetTSD(&(p)->TSD)))

#define HB_CTW_GETCURRENT(p) hb_ctw_CurrentWindow(p)
#define HB_CTW_SETCURRENT(p, n) (HB_CTW_TSD(p)->iCurrWindow = (n))

#define HB_CTWIN_ALLOC 16

#ifdef HB_CLP_STRICT
#define HB_CTWIN_MINROWS 1
#define HB_CTWIN_MINCOLS 1
#define HB_CTWIN_MAXROWS 255
#define HB_CTWIN_MAXCOLS 255
#endif

#define HB_CTW_SHADOW_MASK 0x8000000

struct HB_CTWDATA
{
  int iCurrWindow;
};

using PHB_CTWDATA = HB_CTWDATA *;

struct HB_CT_WND
{
  int iHandle;

  HB_BOOL fHidden;
  int iLevel;

  int iShadowAttr;
  int iCursorStyle;

  int iRow;
  int iCol;

  int iTopMargin;
  int iLeftMargin;
  int iBottomMargin;
  int iRightMargin;

  HB_BOOL fClip;
  int iCliTop;
  int iCliLeft;
  int iCliBottom;
  int iCliRight;

  int iHeight;
  int iWidth;

  int iFirstRow;
  int iFirstCol;

  int iColorIndex;
  int iColorCount;
  int *piColors;

  PHB_SCREENCELL screenBuffer;
};

using PHB_CT_WND = HB_CT_WND *;

struct HB_GTCTW
{
  PHB_GT pGT;
  HB_GT_FUNCS SuperTable;

  HB_TSD TSD;

  int iShadowWidth;
  int iShadowAttr;

  int iOpenWindows;
  int iMaxWindow;

  int fBoardSet;
  int iBoardTop;
  int iBoardLeft;
  int iBoardBottom;
  int iBoardRight;

  int fBoardTop;
  int fBoardLeft;
  int fBoardBottom;
  int fBoardRight;

  int iMoveMode;
  int iVerticalStep;
  int iHorizontalStep;

  PHB_CT_WND *windows;
  int *windowStack;
  int *pWindowMap;
  int *pShadowMap;
  int iMapWidth;
  int iMapHeight;

  int iLastKey;
};

using PHB_GTCTW = HB_GTCTW *;

static const HB_WCHAR sc_szFrameW[] = HB_B_SINGLE_W;

static int hb_ctw_CalcShadowWidth(int iRows, int iCols)
{
  if (iRows + iRows >= iCols)
  {
    return 1;
  }
  else
  {
    return 2;
  }
}

static void hb_ctw_SetMap(PHB_GTCTW pCTW, int *piMap, int iWindow, int iTop, int iLeft, int iBottom, int iRight,
                          int iNested)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_SetMap(%p,%p,%d,%d,%d,%d,%d,%d)", static_cast<void*>(pCTW), static_cast<void*>(piMap), iWindow, iTop, iLeft, iBottom, iRight, iNested));
#endif

  HB_SIZE nIndex;
  int i;

  if (iTop < 0)
  {
    iTop = 0;
  }
  if (iBottom >= pCTW->iMapHeight)
  {
    iBottom = pCTW->iMapHeight - 1;
  }
  if (iLeft < 0)
  {
    iLeft = 0;
  }
  if (iRight >= pCTW->iMapWidth)
  {
    iRight = pCTW->iMapWidth - 1;
  }

  if (iNested == 0)
  {
    while (iTop <= iBottom)
    {
      nIndex = iTop * pCTW->iMapWidth + iLeft;
      for (i = iLeft; i <= iRight; ++i, ++nIndex)
      {
        piMap[nIndex] = iWindow;
      }
      ++iTop;
    }
  }
  else
  {
    while (iTop <= iBottom)
    {
      nIndex = iTop * pCTW->iMapWidth + iLeft;
      for (i = iLeft; i <= iRight; ++i, ++nIndex)
      {
        piMap[nIndex] = iWindow | ((piMap[nIndex] != 0 && piMap[nIndex] != iWindow) ? iNested : 0);
      }
      ++iTop;
    }
  }
}

static void hb_ctw_ClearMap(PHB_GTCTW pCTW)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_ClearMap(%p)", static_cast<void*>(pCTW)));
#endif

  HB_SIZE nSize;

  nSize = static_cast<HB_SIZE>(pCTW->iMapHeight) * pCTW->iMapWidth * sizeof(int);
  memset(pCTW->pWindowMap, 0, nSize);
  memset(pCTW->pShadowMap, 0, nSize);
}

static void hb_ctw_TouchLines(PHB_GTCTW pCTW, int iFrom, int iTo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_TouchLines(%p,%d,%d)", static_cast<void*>(pCTW), iFrom, iTo));
#endif

  while (iFrom <= iTo)
  {
    HB_GTSELF_TOUCHLINE(pCTW->pGT, iFrom);
    ++iFrom;
  }
}

static void hb_ctw_WindowMap(PHB_GTCTW pCTW, int iWindow, HB_BOOL fExpose)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_WindowMap(%p,%d,%d)", static_cast<void*>(pCTW), iWindow, static_cast<int>(fExpose)));
#endif

  PHB_CT_WND pWnd;

  pWnd = pCTW->windows[iWindow];

  if (!pWnd->fHidden)
  {
    int iLastRow = pWnd->iFirstRow + pWnd->iHeight - 1, iLastCol = pWnd->iFirstCol + pWnd->iWidth - 1;

    hb_ctw_SetMap(pCTW, pCTW->pWindowMap, iWindow, pWnd->iFirstRow, pWnd->iFirstCol, iLastRow, iLastCol, 0);
    hb_ctw_SetMap(pCTW, pCTW->pShadowMap, 0, pWnd->iFirstRow, pWnd->iFirstCol, iLastRow, iLastCol, 0);
    if (pWnd->iShadowAttr != HB_CTW_SHADOW_OFF && iLastRow >= pCTW->iBoardTop && iLastCol >= pCTW->iBoardLeft &&
        pWnd->iFirstRow <= pCTW->iBoardBottom && pWnd->iFirstCol <= pCTW->iBoardRight)
    {
      iLastRow += 1;
      iLastCol += pCTW->iShadowWidth;
      hb_ctw_SetMap(pCTW, pCTW->pShadowMap, iWindow, iLastRow, pWnd->iFirstCol + pCTW->iShadowWidth, iLastRow, iLastCol,
                    pWnd->iShadowAttr == HB_CTW_SHADOW_EXT2 ? HB_CTW_SHADOW_MASK : 0);
      hb_ctw_SetMap(pCTW, pCTW->pShadowMap, iWindow, pWnd->iFirstRow + 1, pWnd->iFirstCol + pWnd->iWidth, iLastRow - 1,
                    iLastCol, pWnd->iShadowAttr == HB_CTW_SHADOW_EXT2 ? HB_CTW_SHADOW_MASK : 0);
    }
    if (fExpose)
    {
      hb_ctw_TouchLines(pCTW, pWnd->iFirstRow, iLastRow);
    }
  }
}

static void hb_ctw_RemapAllWindows(PHB_GTCTW pCTW, int iFrom, HB_BOOL fExpose)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_RemapAllWindows(%p,%d,%d)", static_cast<void*>(pCTW), iFrom, static_cast<int>(fExpose)));
#endif

  if (pCTW->iMaxWindow)
  {
    if (iFrom == 0)
    {
      hb_ctw_ClearMap(pCTW);
    }
    for (int i = iFrom; i < pCTW->iOpenWindows; ++i)
    {
      hb_ctw_WindowMap(pCTW, pCTW->windowStack[i], false);
    }
    if (fExpose)
    {
      hb_ctw_TouchLines(pCTW, 0, pCTW->iMapHeight);
    }
  }
}

static int hb_ctw_SetShadowAttr(PHB_GTCTW pCTW, int iAttr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_SetShadowAttr(%p,%d)", static_cast<void*>(pCTW), iAttr));
#endif

  int iOldAttr;

  iOldAttr = pCTW->iShadowAttr;
  if (iAttr >= 0 || iAttr == HB_CTW_SHADOW_OFF || iAttr == HB_CTW_SHADOW_EXT || iAttr == HB_CTW_SHADOW_EXT2)
  {
    pCTW->iShadowAttr = iAttr;
  }

  return iOldAttr;
}

static int hb_ctw_SetMoveMode(PHB_GTCTW pCTW, int iMode)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_SetMoveMode(%p,%d)", static_cast<void*>(pCTW), iMode));
#endif

  int iOldMode;

  iOldMode = pCTW->iMoveMode;
  if (iMode >= 0)
  {
    pCTW->iMoveMode = iMode;
  }

  return iOldMode;
}

static int hb_ctw_SetMoveStep(PHB_GTCTW pCTW, int iVertical, int iHorizontal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_SetMoveStep(%p,%d,%d)", static_cast<void*>(pCTW), iVertical, iHorizontal));
#endif

  if (iVertical < pCTW->iMapHeight && iHorizontal < pCTW->iMapWidth)
  {
    pCTW->iVerticalStep = iVertical;
    pCTW->iHorizontalStep = iHorizontal;

    return 0;
  }

  return -1;
}

static int hb_ctw_SetWindowBoard(PHB_GTCTW pCTW, int iTop, int iLeft, int iBottom, int iRight)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_SetWindowBoard(%p,%d,%d,%d,%d)", static_cast<void*>(pCTW), iTop, iLeft, iBottom, iRight));
#endif

#ifdef HB_CLP_STRICT
  // This limitation is only for strict CT3 compatibility, the CTW GTs
  // can work in practice with any virtual board size and position and
  // is limited only by available physical memory, [druzus]
  if (iBottom >= pCTW->iMapHeight)
  {
    iBottom = pCTW->iMapHeight - 1;
  }
  if (iRight >= pCTW->iMapWidth)
  {
    iRight = pCTW->iMapWidth - 1;
  }
#endif

  if (iTop >= 0 && iLeft >= 0 && iTop < iBottom && iLeft < iRight)
  {
    pCTW->iBoardTop = iTop;
    pCTW->iBoardLeft = iLeft;
    pCTW->iBoardBottom = iBottom;
    pCTW->iBoardRight = iRight;
    pCTW->fBoardSet = true;
    hb_ctw_RemapAllWindows(pCTW, 0, true);

    return 0;
  }

  return -1;
}

static int hb_ctw_SetBorderMode(PHB_GTCTW pCTW, int iTop, int iLeft, int iBottom, int iRight)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_SetBorderMode(%p,%d,%d,%d,%d)", static_cast<void*>(pCTW), iTop, iLeft, iBottom, iRight));
#endif

  if (iTop >= 0)
  {
    pCTW->fBoardTop = iTop != 0;
  }
  if (iLeft >= 0)
  {
    pCTW->fBoardLeft = iLeft != 0;
  }
  if (iBottom >= 0)
  {
    pCTW->fBoardBottom = iBottom != 0;
  }
  if (iRight >= 0)
  {
    pCTW->fBoardRight = iRight != 0;
  }

  return 0;
}

static int hb_ctw_CurrentWindow(PHB_GTCTW pCTW)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_CurrentWindow(%p)", static_cast<void*>(pCTW)));
#endif

  PHB_CTWDATA pTSD;

  pTSD = HB_CTW_TSD(pCTW);

  // because other threads can close current window we need additional
  // protection here and we have to check if current handle is still
  // valid [druzus]
  if (pTSD->iCurrWindow > 0)
  {
    if (pTSD->iCurrWindow > pCTW->iMaxWindow || pCTW->windows[pTSD->iCurrWindow] == nullptr)
    {
      pTSD->iCurrWindow = pCTW->iOpenWindows > 0 ? pCTW->windowStack[pCTW->iOpenWindows - 1] : 0;
    }
  }

  return pTSD->iCurrWindow;
}

static int hb_ctw_SelectWindow(PHB_GTCTW pCTW, int iWindow, HB_BOOL fToTop)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_SelectWindow(%p,%d,%d)", static_cast<void*>(pCTW), iWindow, fToTop));
#endif

  if (iWindow == 0)
  {
    HB_CTW_SETCURRENT(pCTW, 0);
  }
  else if (iWindow > 0 && iWindow <= pCTW->iMaxWindow && pCTW->windows[iWindow] != nullptr)
  {
    HB_CTW_SETCURRENT(pCTW, iWindow);
    if (fToTop)
    {
      int i;

      // update window level
      i = pCTW->iOpenWindows - 1;
      while (i >= 0)
      {
        if (pCTW->windowStack[i] == iWindow)
        {
          int iPos = i;
          while (i < pCTW->iOpenWindows - 1 &&
                 pCTW->windows[pCTW->windowStack[i + 1]]->iLevel <= pCTW->windows[iWindow]->iLevel)
          {
            pCTW->windowStack[i] = pCTW->windowStack[i + 1];
            ++i;
          }
          pCTW->windowStack[i] = iWindow;

          if (iPos != i && !pCTW->windows[iWindow]->fHidden)
          {
            // INFO: CT effectively calls hb_ctw_RemapAllWindows() here
            if (i < pCTW->iOpenWindows - 1)
            {
              hb_ctw_RemapAllWindows(pCTW, i, true);
            }
            else
            {
              hb_ctw_WindowMap(pCTW, iWindow, true);
            }
          }
          break;
        }
        --i;
      }
    }
  }
  else
  {
    iWindow = HB_CTW_GETCURRENT(pCTW);
  }

  return iWindow;
}

static int hb_ctw_ChangeWindowHandle(PHB_GTCTW pCTW, int iNewWindow)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_ChangeWindowHandle(%p,%d)", static_cast<void*>(pCTW), iNewWindow));
#endif

  int iWindow;

  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow != iNewWindow)
  {
    if (iWindow > 0 && iNewWindow > 0 && iNewWindow <= 255 &&
        (iNewWindow > pCTW->iMaxWindow || pCTW->windows[iNewWindow] == nullptr))
    {
      PHB_CT_WND pWnd = pCTW->windows[iWindow];
      int i;

      if (iNewWindow > pCTW->iMaxWindow)
      {
        i = pCTW->iMaxWindow;
        while (iNewWindow > pCTW->iMaxWindow)
        {
          pCTW->iMaxWindow += HB_CTWIN_ALLOC;
        }
        pCTW->windows =
            static_cast<PHB_CT_WND *>(hb_xrealloc(pCTW->windows, (pCTW->iMaxWindow + 1) * sizeof(PHB_CT_WND)));
        pCTW->windowStack = static_cast<int *>(hb_xrealloc(pCTW->windowStack, pCTW->iMaxWindow * sizeof(int)));
        do
        {
          pCTW->windows[i + 1] = nullptr;
          pCTW->windowStack[i] = 0;
        } while (++i < pCTW->iMaxWindow);
      }
      pWnd->iHandle = iNewWindow;
      pCTW->windows[iWindow] = nullptr;
      pCTW->windows[iNewWindow] = pWnd;

      i = pCTW->iOpenWindows - 1;
      while (i >= 0 && pCTW->windowStack[i] != iWindow)
      {
        --i;
      }
      if (i >= 0)
      {
        pCTW->windowStack[i] = iNewWindow;
        if (!pWnd->fHidden)
        {
          if (pWnd->iShadowAttr == HB_CTW_SHADOW_EXT2)
          {
            i = 0;
          }
          hb_ctw_RemapAllWindows(pCTW, i, false);
        }
      }
    }
    else
    {
      iNewWindow = -1;
    }
  }
  return iNewWindow;
}

static int hb_ctw_GetWindowStack(PHB_GTCTW pCTW, const int **piStack)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_GetWindowStack(%p,%p)", static_cast<void*>(pCTW), static_cast<const void*>(piStack)));
#endif

  *piStack = pCTW->windowStack;

  return pCTW->iOpenWindows;
}

static int hb_ctw_Visible(PHB_GTCTW pCTW, int iWindow, int iVisible)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_Visible(%p,%d,%d)", static_cast<void*>(pCTW), iWindow, iVisible));
#endif

  int iResult = HB_CTW_UNDEF;

  if (iWindow == 0)
  {
    iResult = HB_CTW_VISIBLE;
  }
  else if (iWindow > 0 && iWindow <= pCTW->iMaxWindow && pCTW->windows[iWindow] != nullptr)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];

    iResult = pWnd->fHidden ? HB_CTW_HIDDEN : HB_CTW_VISIBLE;
    if (iVisible != HB_CTW_UNDEF && pWnd->fHidden != (iVisible == HB_CTW_HIDDEN))
    {
      pWnd->fHidden = (iVisible == HB_CTW_HIDDEN);
      hb_ctw_RemapAllWindows(pCTW, 0, true);
    }
  }

  return iResult;
}

static int hb_ctw_SetWindowLevel(PHB_GTCTW pCTW, int iWindow, int iLevel)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_SetWindowLevel(%p,%d,%d)", static_cast<void*>(pCTW), iWindow, iLevel));
#endif

  int iResult = -1;

  if (iWindow > 0 && iWindow <= pCTW->iMaxWindow && pCTW->windows[iWindow] != nullptr)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];

    iResult = pWnd->iLevel;
    if (iLevel >= HB_CTW_BOTTOM && iLevel <= HB_CTW_TOP && pWnd->iLevel != iLevel)
    {
      HB_BOOL fToTop;
      int i;

      // update window level
      fToTop = pWnd->iLevel < iLevel;
      pWnd->iLevel = iLevel;

      i = pCTW->iOpenWindows - 1;
      if (i > 0)
      {
        while (i >= 0 && pCTW->windowStack[i] != iWindow)
        {
          --i;
        }
        if (i >= 0)
        {
          int iPos = i;
          if (fToTop)
          {
            while (i < pCTW->iOpenWindows - 1 && pWnd->iLevel >= pCTW->windows[pCTW->windowStack[i + 1]]->iLevel)
            {
              pCTW->windowStack[i] = pCTW->windowStack[i + 1];
              ++i;
            }
            pCTW->windowStack[i] = iWindow;
          }
          else
          {
            while (i > 0 && pWnd->iLevel <= pCTW->windows[pCTW->windowStack[i - 1]]->iLevel)
            {
              pCTW->windowStack[i] = pCTW->windowStack[i - 1];
              --i;
            }
            pCTW->windowStack[i] = iWindow;
          }
          if (!pWnd->fHidden && iPos != i)
          {
            hb_ctw_RemapAllWindows(pCTW, HB_MIN(iPos, i), true);
          }
        }
      }
    }
  }

  return iResult;
}

static int hb_ctw_SetWindowShadow(PHB_GTCTW pCTW, int iWindow, int iAttr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_SetWindowShadow(%p,%d,%d)", static_cast<void*>(pCTW), iWindow, iAttr));
#endif

  int iResult = -1;

  if (iWindow > 0 && iWindow <= pCTW->iMaxWindow && pCTW->windows[iWindow] != nullptr)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];

    iResult = pWnd->iShadowAttr;
    if ((iAttr >= 0 || iAttr == HB_CTW_SHADOW_OFF || iAttr == HB_CTW_SHADOW_EXT || iAttr == HB_CTW_SHADOW_EXT2) &&
        pWnd->iShadowAttr != iAttr)
    {
      pWnd->iShadowAttr = iAttr;
      if (!pWnd->fHidden)
      {
        hb_ctw_RemapAllWindows(pCTW, 0, true);
      }
    }
  }

  return iResult;
}

static int hb_ctw_MaxWindow(PHB_GTCTW pCTW)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_MaxWindow(%p)", static_cast<void*>(pCTW)));
#endif

  int iMaxHandle = 0;

  for (auto i = 0; i < pCTW->iOpenWindows; ++i)
  {
    if (iMaxHandle < pCTW->windowStack[i])
    {
      iMaxHandle = pCTW->windowStack[i];
    }
  }

  return iMaxHandle;
}

static int hb_ctw_CreateWindow(PHB_GTCTW pCTW, int iTop, int iLeft, int iBottom, int iRight, HB_BOOL fClear, int iColor,
                               HB_BOOL fVisible)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_CreateWindow(%p,%d,%d,%d,%d,%d,%d,%d)", static_cast<void*>(pCTW), iTop, iLeft, iBottom, iRight, static_cast<int>(fClear), iColor, static_cast<int>(fVisible)));
#endif

  HB_BYTE bAttr;
  HB_USHORT usChar;
  int iRow, iCol, iHeight, iWidth, iTmp;
  long lIndex;

  if (pCTW->iOpenWindows == pCTW->iMaxWindow)
  {
    int i = pCTW->iMaxWindow;

    if (pCTW->iMaxWindow == 0)
    {
      HB_SIZE nSize;

      HB_GTSELF_GETSIZE(pCTW->pGT, &pCTW->iMapHeight, &pCTW->iMapWidth);
      pCTW->iShadowWidth = hb_ctw_CalcShadowWidth(pCTW->iMapHeight, pCTW->iMapWidth);
      if (!pCTW->fBoardSet)
      {
        hb_ctw_SetWindowBoard(pCTW, 0, 0, pCTW->iMapHeight - 1, pCTW->iMapWidth - 1);
      }
      nSize = static_cast<HB_SIZE>(pCTW->iMapHeight) * pCTW->iMapWidth * sizeof(int);
      pCTW->pWindowMap = static_cast<int *>(hb_xgrab(nSize));
      pCTW->pShadowMap = static_cast<int *>(hb_xgrab(nSize));
      hb_ctw_ClearMap(pCTW);

      pCTW->iMaxWindow = HB_CTWIN_ALLOC;
      pCTW->windows = static_cast<PHB_CT_WND *>(hb_xgrab((HB_CTWIN_ALLOC + 1) * sizeof(PHB_CT_WND)));
      pCTW->windowStack = static_cast<int *>(hb_xgrab(HB_CTWIN_ALLOC * sizeof(int)));
      pCTW->windows[0] = nullptr;
    }
    else
    {
      pCTW->iMaxWindow += HB_CTWIN_ALLOC;
      pCTW->windows =
          static_cast<PHB_CT_WND *>(hb_xrealloc(pCTW->windows, (pCTW->iMaxWindow + 1) * sizeof(PHB_CT_WND)));
      pCTW->windowStack = static_cast<int *>(hb_xrealloc(pCTW->windowStack, pCTW->iMaxWindow * sizeof(int)));
    }
    do
    {
      pCTW->windows[i + 1] = nullptr;
      pCTW->windowStack[i] = 0;
    } while (++i < pCTW->iMaxWindow);
  }

  iHeight = iBottom - iTop + 1;
  iWidth = iRight - iLeft + 1;
  iRow = iTop;
  iCol = iLeft;

  if (iHeight > pCTW->iBoardBottom - pCTW->iBoardTop + 1)
  {
    iHeight = pCTW->iBoardBottom - pCTW->iBoardTop + 1;
  }
  if (iWidth > pCTW->iBoardRight - pCTW->iBoardLeft + 1)
  {
    iWidth = pCTW->iBoardRight - pCTW->iBoardLeft + 1;
  }

#ifdef HB_CLP_STRICT
  if (iHeight < HB_CTWIN_MINROWS || iWidth < HB_CTWIN_MINCOLS || iHeight > HB_CTWIN_MAXROWS ||
      iWidth > HB_CTWIN_MAXCOLS)
  {
    return -1;
  }
#endif

  iTop = pCTW->iBoardTop - (pCTW->fBoardTop ? iHeight : 0);
  iBottom = pCTW->iBoardBottom + 1 - (pCTW->fBoardBottom ? 0 : iHeight);
  iLeft = pCTW->iBoardLeft - (pCTW->fBoardLeft ? iWidth : 0);
  iRight = pCTW->iBoardRight + 1 - (pCTW->fBoardRight ? 0 : iWidth);

  if (iRow < iTop)
  {
    iRow = iTop;
  }
  else if (iRow > iBottom)
  {
    iRow = iBottom;
  }
  if (iCol < iLeft)
  {
    iCol = iLeft;
  }
  else if (iCol > iRight)
  {
    iCol = iRight;
  }

  auto pWnd = static_cast<PHB_CT_WND>(hb_xgrab(sizeof(HB_CT_WND)));
  memset(pWnd, 0, sizeof(HB_CT_WND));

  pWnd->fHidden = !fVisible;
  pWnd->iLevel = HB_CTW_DEFAULT;
  pWnd->iShadowAttr = pCTW->iShadowAttr;
  pWnd->iCursorStyle = HB_GTSELF_GETCURSORSTYLE(pCTW->pGT);

  pWnd->iHeight = iHeight;
  pWnd->iWidth = iWidth;
  pWnd->iFirstRow = iRow;
  pWnd->iFirstCol = iCol;

  HB_GTSELF_GETCOLORDATA(pCTW->pGT, &pWnd->piColors, &pWnd->iColorCount, &pWnd->iColorIndex);

  pWnd->screenBuffer =
      static_cast<PHB_SCREENCELL>(hb_xgrab(static_cast<HB_SIZE>(pWnd->iHeight) * pWnd->iWidth * sizeof(HB_SCREENCELL)));

  if (pWnd->iShadowAttr >= 0)
  {
    fClear = true;
  }
  bAttr = 0;
  if (iColor < 0)
  {
    iColor = HB_GTSELF_GETCOLOR(pCTW->pGT);
  }
  usChar = HB_GTSELF_GETCLEARCHAR(pCTW->pGT);

  lIndex = 0;
  for (iRow = pWnd->iFirstRow; iRow < pWnd->iFirstRow + pWnd->iHeight; ++iRow)
  {
    for (iCol = pWnd->iFirstCol; iCol < pWnd->iFirstCol + pWnd->iWidth; ++iCol)
    {
      if (!fClear && !HB_GTSELF_GETSCRCHAR(pCTW->pGT, iRow, iCol, &iColor, &bAttr, &usChar))
      {
        usChar = HB_GTSELF_GETCLEARCHAR(pCTW->pGT);
        iColor = HB_GTSELF_GETCOLOR(pCTW->pGT);
        bAttr = 0;
      }
      pWnd->screenBuffer[lIndex].c.usChar = usChar;
      pWnd->screenBuffer[lIndex].c.bColor = static_cast<HB_BYTE>(iColor);
      pWnd->screenBuffer[lIndex].c.bAttr = 0;
      ++lIndex;
    }
  }

  for (iTmp = 1; iTmp < pCTW->iMaxWindow; ++iTmp)
  {
    if (pCTW->windows[iTmp] == nullptr)
    {
      break;
    }
  }
  pWnd->iHandle = iTmp;

  pCTW->windows[pWnd->iHandle] = pWnd;
  // update window level
  iTmp = pCTW->iOpenWindows++;
  while (iTmp > 0 && pCTW->windows[pCTW->windowStack[iTmp - 1]]->iLevel > pWnd->iLevel)
  {
    pCTW->windowStack[iTmp] = pCTW->windowStack[iTmp - 1];
    --iTmp;
  }
  pCTW->windowStack[iTmp] = pWnd->iHandle;
  HB_CTW_SETCURRENT(pCTW, pWnd->iHandle);
  if (!pWnd->fHidden)
  {
    if (iTmp < pCTW->iOpenWindows - 1)
    {
      hb_ctw_RemapAllWindows(pCTW, iTmp, true);
    }
    else
    {
      hb_ctw_WindowMap(pCTW, pWnd->iHandle, true);
    }
  }

  return pWnd->iHandle;
}

static int hb_ctw_CloseWindow(PHB_GTCTW pCTW, int iWindow)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_CloseWindow(%p,%d)", static_cast<void*>(pCTW), iWindow));
#endif

  if (iWindow > 0 && iWindow <= pCTW->iMaxWindow && pCTW->windows[iWindow])
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];
    int i, iWnd, iLast;
    HB_BOOL fHidden = pWnd->fHidden;

    hb_xfree(pWnd->screenBuffer);
    if (pWnd->iColorCount)
    {
      hb_xfree(pWnd->piColors);
    }
    hb_xfree(pWnd);
    pCTW->windows[iWindow] = nullptr;

    iWnd = 0;
    i = --pCTW->iOpenWindows;
    do
    {
      iLast = pCTW->windowStack[i];
      pCTW->windowStack[i] = iWnd;
      if (iLast == iWindow)
      {
        break;
      }
      iWnd = iLast;
    } while (--i >= 0);

    iLast = HB_CTW_GETCURRENT(pCTW);
    if (iWindow == iLast)
    {
      iLast = pCTW->iOpenWindows > 0 ? pCTW->windowStack[pCTW->iOpenWindows - 1] : 0;
      HB_CTW_SETCURRENT(pCTW, iLast);
    }

    if (!fHidden)
    {
      hb_ctw_RemapAllWindows(pCTW, 0, true);
    }

    return iLast;
  }

  return -1;
}

static int hb_ctw_CloseAllWindows(PHB_GTCTW pCTW)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_CloseAllWindows(%p)", static_cast<void*>(pCTW)));
#endif

  if (pCTW->iOpenWindows > 0)
  {
    for (auto i = 0; i < pCTW->iOpenWindows; ++i)
    {
      int iWindow = pCTW->windowStack[i];
      PHB_CT_WND pWnd = pCTW->windows[iWindow];
      pCTW->windowStack[i] = 0;
      pCTW->windows[iWindow] = nullptr;
      hb_xfree(pWnd->screenBuffer);
      if (pWnd->iColorCount)
      {
        hb_xfree(pWnd->piColors);
      }
      hb_xfree(pWnd);
    }
    pCTW->iOpenWindows = 0;
    HB_CTW_SETCURRENT(pCTW, 0);
    hb_ctw_RemapAllWindows(pCTW, 0, true);
    return 0;
  }

  return -1;
}

static int hb_ctw_CenterWindow(PHB_GTCTW pCTW, int iWindow, HB_BOOL fCenter)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_CenterWindow(%p,%d,%d)", static_cast<void*>(pCTW), iWindow, static_cast<int>(fCenter)));
#endif

  if (iWindow > 0 && iWindow <= pCTW->iOpenWindows)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];

    if (pWnd)
    {
      int iRow = pWnd->iFirstRow, iCol = pWnd->iFirstCol;

      if (fCenter)
      {
        int iHeight = pCTW->iBoardBottom - pCTW->iBoardTop + 1, iWidth = pCTW->iBoardRight - pCTW->iBoardLeft + 1;

        pWnd->iFirstRow = pCTW->iBoardTop;
        pWnd->iFirstCol = pCTW->iBoardLeft;

        if (iHeight > pWnd->iHeight)
        {
          pWnd->iFirstRow += (iHeight - pWnd->iHeight) >> 1;
        }
        if (iWidth > pWnd->iWidth)
        {
          pWnd->iFirstCol += (iWidth - pWnd->iWidth) >> 1;
        }
      }
      else
      {
        if (pWnd->iFirstRow > pCTW->iBoardBottom - pWnd->iHeight + 1)
        {
          pWnd->iFirstRow = pCTW->iBoardBottom - pWnd->iHeight + 1;
        }
        if (pWnd->iFirstRow < pCTW->iBoardTop)
        {
          pWnd->iFirstRow = pCTW->iBoardTop;
        }
        if (pWnd->iFirstCol > pCTW->iBoardRight - pWnd->iWidth + 1)
        {
          pWnd->iFirstCol = pCTW->iBoardRight - pWnd->iWidth + 1;
        }
        if (pWnd->iFirstCol < pCTW->iBoardLeft)
        {
          pWnd->iFirstCol = pCTW->iBoardLeft;
        }
      }

      if (!pWnd->fHidden && (iRow != pWnd->iFirstRow || iCol != pWnd->iFirstCol))
      {
        hb_ctw_RemapAllWindows(pCTW, 0, true);
      }

      return iWindow;
    }
  }

  return -1;
}

static int hb_ctw_MoveWindow(PHB_GTCTW pCTW, int iWindow, int iRow, int iCol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_MoveWindow(%p,%d,%d,%d)", static_cast<void*>(pCTW), iWindow, iRow, iCol));
#endif

  if (iWindow > 0 && iWindow <= pCTW->iOpenWindows)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];

    if (pWnd)
    {
      if ((iRow + (pCTW->fBoardTop ? pWnd->iHeight : 0) >= pCTW->iBoardTop) &&
          (iRow + (pCTW->fBoardBottom ? 0 : pWnd->iHeight) <= pCTW->iBoardBottom + 1) &&
          (iCol + (pCTW->fBoardLeft ? pWnd->iWidth : 0) >= pCTW->iBoardLeft) &&
          (iCol + (pCTW->fBoardRight ? 0 : pWnd->iWidth) <= pCTW->iBoardRight + 1))
      {
        pWnd->iFirstRow = iRow;
        pWnd->iFirstCol = iCol;
        if (!pWnd->fHidden)
        {
          hb_ctw_RemapAllWindows(pCTW, 0, true);
        }
        return iWindow;
      }
    }
  }

  return -1;
}

static int hb_ctw_ChangeMargins(PHB_GTCTW pCTW, int iWindow, int iTop, int iLeft, int iBottom, int iRight)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_ChangeMargins(%p,%d,%d,%d,%d,%d)", static_cast<void*>(pCTW), iWindow, iTop, iLeft, iBottom, iRight));
#endif

  if (iWindow > 0 && iWindow <= pCTW->iOpenWindows)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];

    if (pWnd)
    {
      if ((iTop += pWnd->iTopMargin) < 0)
      {
        iTop = 0;
      }
      if ((iLeft += pWnd->iLeftMargin) < 0)
      {
        iLeft = 0;
      }
      if ((iBottom += pWnd->iBottomMargin) < 0)
      {
        iBottom = 0;
      }
      if ((iRight += pWnd->iRightMargin) < 0)
      {
        iRight = 0;
      }

      if (iTop + iBottom < pWnd->iHeight && iLeft + iRight < pWnd->iWidth)
      {
        pWnd->iTopMargin = iTop;
        pWnd->iLeftMargin = iLeft;
        pWnd->iBottomMargin = iBottom;
        pWnd->iRightMargin = iRight;

        return iWindow;
      }
    }
  }

  return -1;
}

static int hb_ctw_SetWindowClip(PHB_GTCTW pCTW, int iWindow, int iTop, int iLeft, int iBottom, int iRight)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_SetWindowClip(%p,%d,%d,%d,%d,%d)", static_cast<void*>(pCTW), iWindow, iTop, iLeft, iBottom, iRight));
#endif

  if (iWindow > 0 && iWindow <= pCTW->iOpenWindows)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];

    if (pWnd)
    {
      if (iTop < 0)
      {
        iTop = 0;
      }
      if (iLeft < 0)
      {
        iLeft = 0;
      }
      if (iBottom >= pWnd->iHeight)
      {
        iBottom = pWnd->iHeight - 1;
      }
      if (iRight >= pWnd->iWidth)
      {
        iRight = pWnd->iWidth - 1;
      }
      if (iTop > iBottom || iLeft > iRight ||
          (iTop == 0 && iLeft == 0 && iBottom == pWnd->iHeight - 1 && iRight == pWnd->iWidth - 1))
      {
        pWnd->fClip = false;
      }
      else
      {
        pWnd->fClip = true;
        pWnd->iCliTop = iTop;
        pWnd->iCliLeft = iLeft;
        pWnd->iCliBottom = iBottom;
        pWnd->iCliRight = iRight;
      }

      return iWindow;
    }
  }
  return -1;
}

static int hb_ctw_GetWindowCords(PHB_GTCTW pCTW, int iWindow, HB_BOOL fCenter, int *piTop, int *piLeft, int *piBottom,
                                 int *piRight)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_GetWindowCords(%p,%d,%d,%p,%p,%p,%p)", static_cast<void*>(pCTW), iWindow, static_cast<int>(fCenter), static_cast<void*>(piTop), static_cast<void*>(piLeft), static_cast<void*>(piBottom), static_cast<void*>(piRight)));
#endif

  if (iWindow > 0 && iWindow <= pCTW->iOpenWindows)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];

    if (pWnd)
    {
      if (fCenter)
      {
        int iHeight = pCTW->iBoardBottom - pCTW->iBoardTop + 1, iWidth = pCTW->iBoardRight - pCTW->iBoardLeft + 1;

        *piTop = pCTW->iBoardTop;
        *piLeft = pCTW->iBoardLeft;

        if (iHeight > pWnd->iHeight)
        {
          *piTop += (iHeight - pWnd->iHeight) >> 1;
        }
        if (iWidth > pWnd->iWidth)
        {
          *piLeft += (iWidth - pWnd->iWidth) >> 1;
        }
      }
      else
      {
        *piTop = pWnd->iFirstRow;
        *piLeft = pWnd->iFirstCol;
      }
      *piBottom = *piTop + pWnd->iHeight - 1;
      *piRight = *piLeft + pWnd->iWidth - 1;

      return iWindow;
    }
  }

  *piTop = *piLeft = 0;
  *piBottom = HB_GTSELF_MAXROW(pCTW->pGT);
  *piRight = HB_GTSELF_MAXCOL(pCTW->pGT);

  return -1;
}

static int hb_ctw_GetFormatCords(PHB_GTCTW pCTW, int iWindow, HB_BOOL fRelative, int *piTop, int *piLeft, int *piBottom,
                                 int *piRight)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_GetFormatCords(%p,%d,%d,%p,%p,%p,%p)", static_cast<void*>(pCTW), iWindow, static_cast<int>(fRelative), static_cast<void*>(piTop), static_cast<void*>(piLeft), static_cast<void*>(piBottom), static_cast<void*>(piRight)));
#endif

  if (iWindow > 0 && iWindow <= pCTW->iOpenWindows)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];

    if (pWnd)
    {
      if (fRelative)
      {
        *piTop = pWnd->iTopMargin;
        *piLeft = pWnd->iLeftMargin;
        *piBottom = pWnd->iBottomMargin;
        *piRight = pWnd->iRightMargin;
      }
      else
      {
        *piTop = pWnd->iFirstRow + pWnd->iTopMargin;
        *piLeft = pWnd->iFirstCol + pWnd->iLeftMargin;
        *piBottom = pWnd->iFirstRow + pWnd->iHeight - pWnd->iBottomMargin - 1;
        *piRight = pWnd->iFirstCol + pWnd->iWidth - pWnd->iRightMargin - 1;
      }
      return iWindow;
    }
  }

  if (fRelative)
  {
    *piTop = *piLeft = *piBottom = *piRight = 0;
  }
  else
  {
    *piTop = *piLeft = 0;
    *piBottom = HB_GTSELF_MAXROW(pCTW->pGT);
    *piRight = HB_GTSELF_MAXCOL(pCTW->pGT);
  }

  return -1;
}

static int hb_ctw_AddWindowBox(PHB_GTCTW pCTW, int iWindow, const HB_WCHAR *szBoxW, int iColor)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_AddWindowBox(%p,%d,%p,%d)", static_cast<void*>(pCTW), iWindow, static_cast<const void*>(szBoxW), iColor));
#endif

  int iMaxRow, iMaxCol;

  iMaxRow = HB_GTSELF_MAXROW(pCTW->pGT);
  iMaxCol = HB_GTSELF_MAXCOL(pCTW->pGT);

  if (iMaxRow > 1 && iMaxCol > 1)
  {
    if (iColor < 0)
    {
      iColor = HB_GTSELF_GETCOLOR(pCTW->pGT);
    }
    HB_GTSELF_BOXW(pCTW->pGT, 0, 0, iMaxRow, iMaxCol, szBoxW, iColor);
    if (iWindow > 0 && iWindow <= pCTW->iOpenWindows && pCTW->windows[iWindow] != nullptr)
    {
      HB_GTSELF_SETPOS(pCTW->pGT, 0, 0);
      hb_ctw_ChangeMargins(pCTW, iWindow, 1, 1, 1, 1);
    }
    else
    {
      HB_GTSELF_SETPOS(pCTW->pGT, 1, 1);
    }

    return 0;
  }

  return -1;
}

static int hb_ctw_SwapWindows(PHB_GTCTW pCTW, int iWindow1, int iWindow2)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_SwapWindows(%p,%d,%d)", static_cast<void*>(pCTW), iWindow1, iWindow2));
#endif

  if (iWindow1 > 0 && iWindow1 <= pCTW->iOpenWindows && pCTW->windows[iWindow1] != nullptr && iWindow2 > 0 &&
      iWindow2 <= pCTW->iOpenWindows && pCTW->windows[iWindow2] != nullptr)
  {
    PHB_CT_WND pWnd;
    int iLevel;
    HB_BOOL fHidden;

    pWnd = pCTW->windows[iWindow1];
    pCTW->windows[iWindow1] = pCTW->windows[iWindow2];
    pCTW->windows[iWindow2] = pWnd;

    iLevel = pWnd->iLevel;
    pWnd->iLevel = pCTW->windows[iWindow1]->iLevel;
    pCTW->windows[iWindow1]->iLevel = iLevel;

    fHidden = pWnd->fHidden;
    pWnd->fHidden = pCTW->windows[iWindow1]->fHidden;
    pCTW->windows[iWindow1]->fHidden = fHidden;

    if (!fHidden || !pWnd->fHidden)
    {
      hb_ctw_RemapAllWindows(pCTW, 0, true);
    }
    return iWindow1;
  }

  return -1;
}

// ---

static void hb_ctw_Init(PHB_GTCTW pCTW)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_Init(%p)", static_cast<void*>(pCTW)));
#endif

  int iRow, iCol;

  pCTW->iShadowWidth = 2;
  pCTW->iShadowAttr = -1;
  pCTW->iMoveMode = 1;
  pCTW->iVerticalStep = 2;
  pCTW->iHorizontalStep = 5;

  // initialize thread local storage for current window number
  HB_TSD_INIT(&pCTW->TSD, sizeof(HB_CTWDATA), nullptr, nullptr);

  HB_GTSELF_GETSIZE(pCTW->pGT, &pCTW->iMapHeight, &pCTW->iMapWidth);

  // update cursor position to the rules used by CTWIN
  HB_GTSELF_GETPOS(pCTW->pGT, &iRow, &iCol);
  HB_GTSELF_SETPOS(pCTW->pGT, iRow, iCol);
}

// ---

static PHB_GTCTW hb_ctw_base(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_base()"));
#endif

  PHB_GT pGT;

  pGT = hb_gt_Base();
  if (pGT)
  {
    if (HB_GTCTW_GET(pGT))
    {
      return HB_GTCTW_GET(pGT);
    }
    else
    {
      auto pCTW = static_cast<PHB_GTCTW>(hb_xgrab(sizeof(HB_GTCTW)));

      memset(pCTW, 0, sizeof(HB_GTCTW));
      HB_GTLOCAL(pGT) = pCTW;
      pCTW->pGT = pGT;

      if (hb_gtLoad(HB_GT_DRVNAME(HB_GT_NAME), pGT, HB_GTSUPERTABLE(pGT)))
      {
        hb_ctw_Init(pCTW);
        return pCTW;
      }

      HB_GTLOCAL(pGT) = nullptr;
      hb_xfree(pCTW);
    }
    hb_gt_BaseFree(pGT);
  }

  return nullptr;
}

static void hb_ctw_gt_Exit(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_Exit(%p)", static_cast<void*>(pGT)));
#endif

  PHB_GTCTW pCTW;

  HB_GTSELF_REFRESH(pGT);

  pCTW = HB_GTCTW_GET(pGT);
  hb_ctw_CloseAllWindows(pCTW);

  HB_GTSUPER_EXIT(pGT);

  if (pCTW)
  {
    if (pCTW->iMaxWindow > 0)
    {
      hb_xfree(pCTW->windows);
      hb_xfree(pCTW->windowStack);
      hb_xfree(pCTW->pWindowMap);
      hb_xfree(pCTW->pShadowMap);
    }
    // release thread local storage for current window number
    hb_stackReleaseTSD(&pCTW->TSD);
    hb_xfree(pCTW);
  }
}

static int hb_ctw_MouseRow(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_MouseRow(%p)", static_cast<void*>(pGT)));
#endif

  PHB_GTCTW pCTW;
  int iRow, iWindow;

  iRow = HB_GTSUPER_MOUSEROW(pGT);

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow > 0)
  {
    iRow -= pCTW->windows[iWindow]->iFirstRow + pCTW->windows[iWindow]->iTopMargin;
  }

  return iRow;
}

static int hb_ctw_MouseCol(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_MouseCol(%p)", static_cast<void*>(pGT)));
#endif

  PHB_GTCTW pCTW;
  int iCol, iWindow;

  iCol = HB_GTSUPER_MOUSECOL(pGT);

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow > 0)
  {
    iCol -= pCTW->windows[iWindow]->iFirstCol + pCTW->windows[iWindow]->iLeftMargin;
  }

  return iCol;
}

static void hb_ctw_gt_GetPos(PHB_GT pGT, int *piRow, int *piCol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetPos(%p,%p,%p)", static_cast<void*>(pGT), static_cast<void*>(piRow), static_cast<void*>(piCol)));
#endif

  PHB_GTCTW pCTW;
  int iWindow;

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow > 0)
  {
    *piRow = pCTW->windows[iWindow]->iRow;
    *piCol = pCTW->windows[iWindow]->iCol;
  }
  else
  {
    HB_GTSUPER_GETPOS(pGT, piRow, piCol);
  }
}

// CTWIN uses differ rules when set cursor position out of screen visible
// area then standard Clipper's GT drivers so we have to replicate it in
// SetPos() method, [druzus]
static void hb_ctw_gt_SetPos(PHB_GT pGT, int iRow, int iCol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetPos(%p,%d,%d)", static_cast<void*>(pGT), iRow, iCol));
#endif

  PHB_GTCTW pCTW;
  int iHeight, iWidth, iWindow;

  iHeight = HB_GTSELF_MAXROW(pGT) + 1;
  iWidth = HB_GTSELF_MAXCOL(pGT) + 1;

  if (iCol > iWidth)
  {
    iCol = iWidth;
  }
  else if (iCol < 0)
  {
    iRow += iCol / iWidth - 1;
    iCol = iWidth + iCol % iWidth;
  }
  if (iRow > iHeight)
  {
    iRow = iHeight;
  }

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow > 0)
  {
    if (iRow < -pCTW->windows[iWindow]->iTopMargin)
    {
      iRow = -pCTW->windows[iWindow]->iTopMargin;
    }
    pCTW->windows[iWindow]->iRow = iRow;
    pCTW->windows[iWindow]->iCol = iCol;
  }
  else
  {
    if (iRow < 0)
    {
      iRow = 0;
    }
    HB_GTSUPER_SETPOS(pGT, iRow, iCol);
  }
}

static int hb_ctw_gt_MaxCol(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_MaxCol(%p)", static_cast<void*>(pGT)));
#endif

  PHB_GTCTW pCTW;
  int iWindow;

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow > 0)
  {
    return pCTW->windows[iWindow]->iWidth - pCTW->windows[iWindow]->iLeftMargin - pCTW->windows[iWindow]->iRightMargin -
           1;
  }
  else
  {
    return HB_GTSUPER_MAXCOL(pGT);
  }
}

static int hb_ctw_gt_MaxRow(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_MaxRow(%p)", static_cast<void*>(pGT)));
#endif

  PHB_GTCTW pCTW;
  int iWindow;

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow > 0)
  {
    return pCTW->windows[iWindow]->iHeight - pCTW->windows[iWindow]->iTopMargin -
           pCTW->windows[iWindow]->iBottomMargin - 1;
  }
  else
  {
    return HB_GTSUPER_MAXROW(pGT);
  }
}

// CTWIN uses differ rules in console output then standard Clipper's
// GT drivers so we have to overload WRITECON() method, [druzus]
#define WRITECON_BUFFER_SIZE 512

static void hb_ctw_gt_WriteCon(PHB_GT pGT, const char *szText, HB_SIZE nLength)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_WriteCon(%p,%p,%" HB_PFS "u)", static_cast<void*>(pGT), static_cast<const void*>(szText), nLength));
#endif

  int iLen = 0;
  HB_BOOL bDisp = false;
  HB_BOOL bBell = false;
  int iRow, iCol, iMaxRow, iMaxCol;
  HB_WCHAR szString[WRITECON_BUFFER_SIZE];
  PHB_CODEPAGE cdp = HB_GTSELF_HOSTCP(pGT);
  HB_SIZE nIndex = 0;
  HB_WCHAR wc;

  iMaxRow = HB_GTSELF_MAXROW(pGT);
  iMaxCol = HB_GTSELF_MAXCOL(pGT);

  // small hack for scrolling console output when client area is set
  {
    PHB_GTCTW pCTW = HB_GTCTW_GET(pGT);
    int iWindow = HB_CTW_GETCURRENT(pCTW);
    if (iWindow > 0 && pCTW->windows[iWindow]->fClip)
    {
      iMaxRow = pCTW->windows[iWindow]->iCliBottom;
    }
  }

  HB_GTSELF_GETPOS(pGT, &iRow, &iCol);

  if (iRow > iMaxRow || iCol > iMaxCol)
  {
    if (iRow > iMaxRow)
    {
      iRow = iMaxRow;
    }
    if (iCol > iMaxCol)
    {
      iCol = iMaxCol;
    }
    HB_GTSELF_SETPOS(pGT, iRow, iCol);
  }

  while (HB_CDPCHAR_GET(cdp, szText, nLength, &nIndex, &wc))
  {
    switch (wc)
    {
    case HB_CHAR_BEL:
      bDisp = bBell = true;
      break;

    case HB_CHAR_BS:
      if (iCol > 0)
      {
        --iCol;
        bDisp = true;
      }
      else if (iRow > 0)
      {
        iCol = iMaxCol;
        --iRow;
        bDisp = true;
      }
      if (bDisp)
      {
        if (iLen)
        {
          szString[iLen - 1] = ' ';
        }
        else
        {
          HB_GTSELF_SETPOS(pGT, iRow, iCol);
          szString[iLen++] = ' ';
        }
      }
      break;

    case HB_CHAR_LF:
      iCol = 0;
      ++iRow;
      bDisp = true;
      break;

    case HB_CHAR_CR:
      iCol = 0;
      if (nIndex < nLength && szText[nIndex] == HB_CHAR_LF)
      {
        ++iRow;
        ++nIndex;
      }
      bDisp = true;
      break;

    default:
      szString[iLen++] = wc;
      if (++iCol > iMaxCol)
      {
        iCol = 0;
        ++iRow;
        bDisp = true;
      }
      else if (iLen >= WRITECON_BUFFER_SIZE)
      {
        bDisp = true;
      }
    }

    if (bDisp || nIndex == nLength)
    {
      if (iLen)
      {
        HB_GTSELF_WRITEW(pGT, szString, iLen);
      }

      iLen = 0;
      if (iRow > iMaxRow)
      {
        HB_GTSELF_SCROLL(pGT, 0, 0, iMaxRow, iMaxCol, HB_GTSELF_GETCOLOR(pGT), HB_GTSELF_GETCLEARCHAR(pGT),
                         iRow - iMaxRow, 0);
        iRow = iMaxRow;
        iCol = 0;
      }
      HB_GTSELF_SETPOS(pGT, iRow, iCol);
      bDisp = false;

      // To emulate scrolling
      HB_GTSELF_FLUSH(pGT);

      if (bBell)
      {
        HB_GTSELF_BELL(pGT);
        bBell = false;
      }
    }
  }
}

static void hb_ctw_gt_WriteConW(PHB_GT pGT, const HB_WCHAR *szText, HB_SIZE nLength)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_WriteConW(%p,%p,%" HB_PFS "u)", static_cast<void*>(pGT), static_cast<const void*>(szText), nLength));
#endif

  int iLen = 0;
  HB_BOOL bDisp = false;
  HB_BOOL bBell = false;
  int iRow, iCol, iMaxRow, iMaxCol;
  HB_WCHAR szString[WRITECON_BUFFER_SIZE];
  HB_SIZE nIndex = 0;

  iMaxRow = HB_GTSELF_MAXROW(pGT);
  iMaxCol = HB_GTSELF_MAXCOL(pGT);

  // small hack for scrolling console output when client area is set
  {
    PHB_GTCTW pCTW = HB_GTCTW_GET(pGT);
    int iWindow = HB_CTW_GETCURRENT(pCTW);
    if (iWindow > 0 && pCTW->windows[iWindow]->fClip)
    {
      iMaxRow = pCTW->windows[iWindow]->iCliBottom;
    }
  }

  HB_GTSELF_GETPOS(pGT, &iRow, &iCol);

  if (iRow > iMaxRow || iCol > iMaxCol)
  {
    if (iRow > iMaxRow)
    {
      iRow = iMaxRow;
    }
    if (iCol > iMaxCol)
    {
      iCol = iMaxCol;
    }
    HB_GTSELF_SETPOS(pGT, iRow, iCol);
  }

  while (nIndex < nLength)
  {
    HB_WCHAR wc = szText[nIndex++];

    switch (wc)
    {
    case HB_CHAR_BEL:
      bDisp = bBell = true;
      break;

    case HB_CHAR_BS:
      if (iCol > 0)
      {
        --iCol;
        bDisp = true;
      }
      else if (iRow > 0)
      {
        iCol = iMaxCol;
        --iRow;
        bDisp = true;
      }
      if (bDisp)
      {
        if (iLen)
        {
          szString[iLen - 1] = ' ';
        }
        else
        {
          HB_GTSELF_SETPOS(pGT, iRow, iCol);
          szString[iLen++] = ' ';
        }
      }
      break;

    case HB_CHAR_LF:
      iCol = 0;
      ++iRow;
      bDisp = true;
      break;

    case HB_CHAR_CR:
      iCol = 0;
      if (nIndex < nLength && szText[nIndex] == HB_CHAR_LF)
      {
        ++iRow;
        ++nIndex;
      }
      bDisp = true;
      break;

    default:
      szString[iLen++] = wc;
      if (++iCol > iMaxCol)
      {
        iCol = 0;
        ++iRow;
        bDisp = true;
      }
      else if (iLen >= WRITECON_BUFFER_SIZE)
      {
        bDisp = true;
      }
    }

    if (bDisp || nIndex == nLength)
    {
      if (iLen)
      {
        HB_GTSELF_WRITEW(pGT, szString, iLen);
      }

      iLen = 0;
      if (iRow > iMaxRow)
      {
        HB_GTSELF_SCROLL(pGT, 0, 0, iMaxRow, iMaxCol, HB_GTSELF_GETCOLOR(pGT), HB_GTSELF_GETCLEARCHAR(pGT),
                         iRow - iMaxRow, 0);
        iRow = iMaxRow;
        iCol = 0;
      }
      HB_GTSELF_SETPOS(pGT, iRow, iCol);
      bDisp = false;

      // To emulate scrolling
      HB_GTSELF_FLUSH(pGT);

      if (bBell)
      {
        HB_GTSELF_BELL(pGT);
        bBell = false;
      }
    }
  }
}

static int hb_ctw_gt_GetCursorStyle(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetCursorStyle(%p)", static_cast<void*>(pGT)));
#endif

  PHB_GTCTW pCTW;
  int iWindow;

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow > 0)
  {
    return pCTW->windows[iWindow]->iCursorStyle;
  }
  else
  {
    return HB_GTSUPER_GETCURSORSTYLE(pGT);
  }
}

static void hb_ctw_gt_SetCursorStyle(PHB_GT pGT, int iStyle)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_SetCursorStyle(%p,%d)", static_cast<void*>(pGT), iStyle));
#endif

  PHB_GTCTW pCTW;
  int iWindow;

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow > 0)
  {
    switch (iStyle)
    {
    case SC_NONE:
    case SC_NORMAL:
    case SC_INSERT:
    case SC_SPECIAL1:
    case SC_SPECIAL2:
      pCTW->windows[iWindow]->iCursorStyle = iStyle;
      break;
    default:
      pCTW->windows[iWindow]->iCursorStyle = SC_NORMAL;
      break;
    }
  }
  else
  {
    HB_GTSUPER_SETCURSORSTYLE(pGT, iStyle);
  }
}

static void hb_ctw_gt_GetColorStr(PHB_GT pGT, char *pszColorString)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetColorStr(%p,%p)", static_cast<void*>(pGT), static_cast<void*>(pszColorString)));
#endif

  PHB_GTCTW pCTW;
  int iWindow;

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow > 0)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];
    HB_GTSUPER_COLORSTOSTRING(pGT, pWnd->piColors, pWnd->iColorCount, pszColorString, HB_CLRSTR_LEN);
  }
  else
  {
    HB_GTSUPER_GETCOLORSTR(pGT, pszColorString);
  }
}

static void hb_ctw_gt_SetColorStr(PHB_GT pGT, const char *szColorString)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_SetColorStr(%p,%s)", static_cast<void*>(pGT), szColorString));
#endif

  PHB_GTCTW pCTW;
  int iWindow;

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow > 0)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];
    HB_GTSUPER_STRINGTOCOLORS(pGT, szColorString, &pWnd->piColors, &pWnd->iColorCount);
    pWnd->iColorIndex = HB_CLR_STANDARD;
  }
  else
  {
    HB_GTSUPER_SETCOLORSTR(pGT, szColorString);
  }
}

static void hb_ctw_gt_ColorSelect(PHB_GT pGT, int iColorIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_ColorSelect(%p,%d)", static_cast<void*>(pGT), iColorIndex));
#endif

  PHB_GTCTW pCTW;
  int iWindow;

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow > 0)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];
    if (iColorIndex >= 0 && iColorIndex < pWnd->iColorCount)
    {
      pWnd->iColorIndex = iColorIndex;
    }
  }
  else
  {
    HB_GTSUPER_COLORSELECT(pGT, iColorIndex);
  }
}

static int hb_ctw_gt_GetColor(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetColor(%p)", static_cast<void*>(pGT)));
#endif

  PHB_GTCTW pCTW;
  int iWindow;

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow > 0)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];
    return pWnd->piColors[pWnd->iColorIndex];
  }
  else
  {
    return HB_GTSUPER_GETCOLOR(pGT);
  }
}

static void hb_ctw_gt_GetColorData(PHB_GT pGT, int **pColorsPtr, int *piColorCount, int *piColorIndex)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetColor(%p,%p,%p,%p)", static_cast<void*>(pGT), static_cast<void*>(pColorsPtr), static_cast<void*>(piColorCount), static_cast<void*>(piColorIndex)));
#endif

  PHB_GTCTW pCTW;
  int iWindow;

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow > 0)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];

    *pColorsPtr = static_cast<int *>(hb_xgrab(pWnd->iColorCount * sizeof(int)));
    memcpy(*pColorsPtr, pWnd->piColors, pWnd->iColorCount * sizeof(int));
    *piColorCount = pWnd->iColorCount;
    *piColorIndex = pWnd->iColorIndex;
  }
  else
  {
    HB_GTSUPER_GETCOLORDATA(pGT, pColorsPtr, piColorCount, piColorIndex);
  }
}

static void hb_ctw_gt_GetScrCursor(PHB_GT pGT, int *piRow, int *piCol, int *piStyle)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetScrCursor(%p,%p,%p,%p)", static_cast<void*>(pGT), static_cast<void*>(piRow), static_cast<void*>(piCol), static_cast<void*>(piStyle)));
#endif

  PHB_GTCTW pCTW;
  int iWindow;

  HB_GTSUPER_GETSCRCURSOR(pGT, piRow, piCol, piStyle);
  pCTW = HB_GTCTW_GET(pGT);
  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow > 0)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];

    *piRow += pWnd->iFirstRow + pWnd->iTopMargin;
    *piCol += pWnd->iFirstCol + pWnd->iLeftMargin;
    if (*piStyle != SC_NONE)
    {
      if (*piRow < pCTW->iBoardTop || *piRow > pCTW->iBoardBottom || *piCol < pCTW->iBoardLeft ||
          *piCol > pCTW->iBoardRight)
      {
        *piStyle = SC_NONE;
      }
      else
      {
        long lIndex = static_cast<long>(*piRow) * pCTW->iMapWidth + *piCol;
        if (pCTW->pWindowMap[lIndex] != iWindow)
        {
          *piStyle = SC_NONE;
        }
      }
    }
  }
}

static HB_BOOL hb_ctw_gt_GetScrChar(PHB_GT pGT, int iRow, int iCol, int *piColor, HB_BYTE *pbAttr, HB_USHORT *pusChar)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetScrChar(%p,%d,%d,%p,%p,%p)", static_cast<void*>(pGT), iRow, iCol, static_cast<void*>(piColor), static_cast<void*>(pbAttr), static_cast<void*>(pusChar)));
#endif

  PHB_GTCTW pCTW;
  int iWindow, iShadow;

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = iShadow = 0;
  if (pCTW->iOpenWindows > 0 && iRow >= pCTW->iBoardTop && iRow <= pCTW->iBoardBottom && iCol >= pCTW->iBoardLeft &&
      iCol <= pCTW->iBoardRight)
  {
    long lIndex = static_cast<long>(iRow) * pCTW->iMapWidth + iCol;
    iWindow = pCTW->pWindowMap[lIndex];
    iShadow = pCTW->pShadowMap[lIndex];
  }

  if (iWindow > 0)
  {
    PHB_CT_WND pWnd = pCTW->windows[iWindow];
    iRow -= pWnd->iFirstRow;
    iCol -= pWnd->iFirstCol;
    if (iCol >= 0 && iRow >= 0 && iRow < pWnd->iHeight && iCol < pWnd->iWidth)
    {
      long lIndex = static_cast<long>(iRow) * pWnd->iWidth + iCol;
      *pusChar = pWnd->screenBuffer[lIndex].c.usChar;
      *piColor = pWnd->screenBuffer[lIndex].c.bColor;
      *pbAttr = pWnd->screenBuffer[lIndex].c.bAttr;
    }
    else
    {
      return false;
    }
  }
  else if (!HB_GTSUPER_GETSCRCHAR(pGT, iRow, iCol, piColor, pbAttr, pusChar))
  {
    return false;
  }

  if (iShadow > 0)
  {
    PHB_CT_WND pWnd = pCTW->windows[iShadow & ~HB_CTW_SHADOW_MASK];
    if (pWnd->iShadowAttr >= 0)
    {
      *piColor = pWnd->iShadowAttr;
    }
    else if (pWnd->iShadowAttr == HB_CTW_SHADOW_EXT || pWnd->iShadowAttr == HB_CTW_SHADOW_EXT2)
    {
      if ((*piColor & 0x80) == 0)
      {
        *piColor &= 0x0F;
      }
      if ((*piColor & 0x08) == 0)
      {
        *piColor &= 0xF0;
      }
      if ((*piColor &= 0x77) == 0 || (iShadow & HB_CTW_SHADOW_MASK))
      {
        *piColor = 0x07;
      }
    }
    *pbAttr |= HB_GT_ATTR_SHADOW;
  }

  return true;
}

static HB_BOOL hb_ctw_gt_GetScrUC(PHB_GT pGT, int iRow, int iCol, int *piColor, HB_BYTE *pbAttr, HB_UCHAR *puChar,
                                  HB_BOOL fTerm)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetScrUC(%p,%d,%d,%p,%p,%p,%d)", static_cast<void*>(pGT), iRow, iCol, static_cast<void*>(piColor), static_cast<void*>(pbAttr), static_cast<void*>(puChar), fTerm));
#endif

  HB_USHORT usChar;

  if (hb_ctw_gt_GetScrChar(pGT, iRow, iCol, piColor, pbAttr, &usChar))
  {
    HB_UCHAR uc = 0;
    if (usChar)
    {
      if (fTerm && pGT->cdpTerm)
      {
        uc = hb_cdpGetUC(pGT->cdpTerm, usChar, 0);
      }
      if (uc == 0)
      {
        if (pGT->cdpBox && (!fTerm || pGT->cdpBox != pGT->cdpTerm) && pGT->cdpBox != pGT->cdpHost &&
            (*pbAttr & HB_GT_ATTR_BOX))
        {
          uc = hb_cdpGetUC(pGT->cdpBox, usChar, 0);
        }
        if (uc == 0)
        {
          if (pGT->cdpHost && pGT->cdpTerm != pGT->cdpHost)
          {
            uc = hb_cdpGetUC(pGT->cdpHost, usChar, 0);
          }
          if (uc == 0)
          {
            uc = hb_cdpGetUC(hb_vmCDP(), usChar, usChar < 32 ? static_cast<HB_UCHAR>(usChar) : '?');
          }
        }
      }
    }
    *puChar = uc;
    return true;
  }
  return false;
}

static HB_BOOL hb_ctw_gt_GetChar(PHB_GT pGT, int iRow, int iCol, int *piColor, HB_BYTE *pbAttr, HB_USHORT *pusChar)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetChar(%p,%d,%d,%p,%p,%p)", static_cast<void*>(pGT), iRow, iCol, static_cast<void*>(piColor), static_cast<void*>(pbAttr), static_cast<void*>(pusChar)));
#endif

  PHB_GTCTW pCTW;
  PHB_CT_WND pWnd;
  int iWindow;

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow == 0)
  {
    return HB_GTSELF_GETSCRCHAR(pGT, iRow, iCol, piColor, pbAttr, pusChar);
  }

  pWnd = pCTW->windows[iWindow];
  iRow += pWnd->iTopMargin;
  iCol += pWnd->iLeftMargin;

  if (iCol >= 0 && iRow >= 0 && iRow < pWnd->iHeight && iCol < pWnd->iWidth)
  {
    long lIndex = static_cast<long>(iRow) * pWnd->iWidth + iCol;
    *pusChar = pWnd->screenBuffer[lIndex].c.usChar;
    *piColor = pWnd->screenBuffer[lIndex].c.bColor;
    *pbAttr = pWnd->screenBuffer[lIndex].c.bAttr;
    return true;
  }

  return false;
}

static HB_BOOL hb_ctw_gt_PutChar(PHB_GT pGT, int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_PutChar(%p,%d,%d,%d,%d,%d)", static_cast<void*>(pGT), iRow, iCol, iColor, static_cast<int>(bAttr), static_cast<int>(usChar)));
#endif

  PHB_GTCTW pCTW;
  int iWindow, iCurrWindow;

  pCTW = HB_GTCTW_GET(pGT);
  iWindow = iCurrWindow = HB_CTW_GETCURRENT(pCTW);
  if (iWindow == 0 && pCTW->iOpenWindows > 0)
  {
    if (iRow >= pCTW->iBoardTop && iRow <= pCTW->iBoardBottom && iCol >= pCTW->iBoardLeft && iCol <= pCTW->iBoardRight)
    {
      long lIndex = static_cast<long>(iRow) * pCTW->iMapWidth + iCol;
      iWindow = pCTW->pWindowMap[lIndex];
#if 0
         // When window with shadow is closed CT3 restores attributes
         // which existed before shadow was displayed. In an application
         // which switches to window 0 for pass-throw output it causes that
         // wrong attributes appears after this operation. In Harbour it's
         // fixed so such problem do not exist. Anyhow some code may switch
         // to window 0, make SaveScreen()/RestScreen() and in such case
         // all shadow attributes are copied to window 0 buffer. The code
         // below is workaround for it. [druzus]
         if( pCTW->pShadowMap[lIndex] != 0 ) {
            int iShadow = pCTW->pShadowMap[lIndex] & ~HB_CTW_SHADOW_MASK;
            if( pCTW->windows[iShadow]->iShadowAttr >= 0 && pCTW->windows[iShadow]->iShadowAttr == iColor ) {
               int iClr;
               HB_BYTE bAtr;
               HB_USHORT usCh;
               if( HB_GTSELF_GETSCRCHAR(pGT, iRow, iCol, &iClr, &bAtr, &usCh) ) {
                  if( usCh == usChar && iClr == iColor ) {
                     return true;
                  }
               }
            }
         }
#endif
      pCTW->pShadowMap[lIndex] = 0;
    }
  }

  if (iWindow > 0)
  {
    int iWndRow, iWndCol, iWndHeight, iWndWidth;
    PHB_CT_WND pWnd = pCTW->windows[iWindow];

    if (iCurrWindow == 0)
    {
      iWndRow = iRow - pWnd->iFirstRow;
      iWndCol = iCol - pWnd->iFirstCol;
      iWndHeight = pWnd->iHeight;
      iWndWidth = pWnd->iWidth;
    }
    else if (pWnd->fClip &&
             (iRow < pWnd->iCliTop || iCol < pWnd->iCliLeft || iRow > pWnd->iCliBottom || iCol > pWnd->iCliRight))
    {
      return true;
    }
    else
    {
      iWndRow = iRow + pWnd->iTopMargin;
      iWndCol = iCol + pWnd->iLeftMargin;
      iRow = iWndRow + pWnd->iFirstRow;
      iCol = iWndCol + pWnd->iFirstCol;
      iWndHeight = pWnd->iHeight - pWnd->iBottomMargin;
      iWndWidth = pWnd->iWidth - pWnd->iRightMargin;
    }
    if (iWndRow >= 0 && iWndCol >= 0 && iWndRow < iWndHeight && iWndCol < iWndWidth)
    {
      long lIndex = static_cast<long>(iWndRow) * pWnd->iWidth + iWndCol;

      pWnd->screenBuffer[lIndex].c.usChar = usChar;
      pWnd->screenBuffer[lIndex].c.bColor = static_cast<HB_BYTE>(iColor);
      pWnd->screenBuffer[lIndex].c.bAttr = bAttr;
      if (!pWnd->fHidden)
      {
        if (iCurrWindow == 0 || (iRow >= pCTW->iBoardTop && iRow <= pCTW->iBoardBottom && iCol >= pCTW->iBoardLeft &&
                                 iCol <= pCTW->iBoardRight))
        {
          HB_GTSELF_TOUCHLINE(pGT, iRow);
        }
      }
      return true;
    }
    return false;
  }

  return HB_GTSUPER_PUTCHAR(pGT, iRow, iCol, iColor, bAttr, usChar);
}

static HB_BOOL hb_ctw_gt_Resize(PHB_GT pGT, int iRows, int iCols)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_Resize(%p,%d,%d)", static_cast<void*>(pGT), iRows, iCols));
#endif

  if (HB_GTSUPER_RESIZE(pGT, iRows, iCols))
  {
    PHB_GTCTW pCTW = HB_GTCTW_GET(pGT);

    if (pCTW->iMaxWindow > 0)
    {
      HB_SIZE nSize;

      pCTW->iMapHeight = iRows;
      pCTW->iMapWidth = iCols;
      pCTW->iShadowWidth = hb_ctw_CalcShadowWidth(pCTW->iMapHeight, pCTW->iMapWidth);
      nSize = static_cast<HB_SIZE>(pCTW->iMapHeight) * pCTW->iMapWidth * sizeof(int);
      pCTW->pWindowMap = static_cast<int *>(hb_xrealloc(pCTW->pWindowMap, nSize));
      pCTW->pShadowMap = static_cast<int *>(hb_xrealloc(pCTW->pShadowMap, nSize));
    }
    if (pCTW->fBoardSet)
    {
      hb_ctw_SetWindowBoard(pCTW, 0, 0, iRows - 1, iCols - 1);
    }
    return true;
  }
  return false;
}

static HB_BOOL hb_ctw_gt_Info(PHB_GT pGT, int iType, PHB_GT_INFO pInfo)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_Info(%p,%d,%p)", static_cast<void*>(pGT), iType, static_cast<void*>(pInfo)));
#endif

  switch (iType)
  {
  case HB_GTI_ISCTWIN:
    pInfo->pResult = hb_itemPutL(pInfo->pResult, true);
    break;

  case HB_GTI_NEWWIN:
  {
    HB_BOOL fResult;

    hb_ctw_SelectWindow(HB_GTCTW_GET(pGT), 0, true);
    fResult = HB_GTSUPER_INFO(pGT, iType, pInfo);

    if (fResult && hb_arrayLen(pInfo->pResult) >= 7)
    {
      PHB_GTCTW pCTW = HB_GTCTW_GET(pGT);
      hb_arraySetNI(pInfo->pResult, 7, HB_CTW_GETCURRENT(pCTW));
    }
    return fResult;
  }
  case HB_GTI_GETWIN:
  {
    HB_BOOL fResult;
    PHB_GTCTW pCTW = HB_GTCTW_GET(pGT);
    int iWindow = HB_CTW_GETCURRENT(pCTW);

    hb_ctw_SelectWindow(pCTW, 0, true);
    fResult = HB_GTSUPER_INFO(pGT, iType, pInfo);
    if (fResult && hb_arrayLen(pInfo->pResult) >= 7)
    {
      hb_arraySetNI(pInfo->pResult, 7, iWindow);
    }
    return fResult;
  }
  case HB_GTI_SETWIN:
  {
    HB_BOOL fResult;
    PHB_GTCTW pCTW = HB_GTCTW_GET(pGT);

    hb_ctw_SelectWindow(pCTW, 0, true);
    fResult = HB_GTSUPER_INFO(pGT, iType, pInfo);
    if (fResult && hb_arrayLen(pInfo->pNewVal) >= 7)
    {
      hb_ctw_SelectWindow(pCTW, hb_arrayGetNI(pInfo->pNewVal, 7), true);
    }
    return fResult;
  }
  default:
    return HB_GTSUPER_INFO(pGT, iType, pInfo);
  }

  return true;
}

static int hb_ctw_gt_Alert(PHB_GT pGT, PHB_ITEM pMessage, PHB_ITEM pOptions, int iClrNorm, int iClrHigh, double dDelay)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_Alert(%p,%p,%p,%d,%d,%f)", static_cast<void*>(pGT), static_cast<void*>(pMessage), static_cast<void*>(pOptions), iClrNorm, iClrHigh, dDelay));
#endif

  int iOptions;

  if (pMessage && pMessage->isString() && pOptions && (iOptions = static_cast<int>(hb_arrayLen(pOptions))) > 0)
  {
    int iRows, iCols;
    HB_BOOL fScreen;

    HB_GTSELF_GETSIZE(pGT, &iRows, &iCols);
    if (iCols <= 4 || iRows <= 4)
    {
      fScreen = false;
    }
    else
    {
      HB_GT_INFO gtInfo{};
      HB_GTSELF_INFO(pGT, HB_GTI_ISSCREENPOS, &gtInfo);
      fScreen = gtInfo.pResult && hb_itemGetL(gtInfo.pResult);
      HB_GTSELF_INFO(pGT, HB_GTI_KBDSUPPORT, &gtInfo);
      if (gtInfo.pResult)
      {
        if (!hb_itemGetL(gtInfo.pResult))
        {
          fScreen = false;
        }
        hb_itemRelease(gtInfo.pResult);
      }
    }
    if (fScreen)
    {
      int iRet = 0;

      PHB_GTCTW pCTW = HB_GTCTW_GET(pGT);
      HB_UINT ulWidth = 0, ulCurrWidth = 0, ulMsg = 0, ul2, ulMaxWidth, ulLast;
      char szKey[HB_MAX_CHAR_LEN];
      HB_SIZE nChar;
      int iDspCount, iLines = 0, iTop, iLeft, iBottom, iRight, iPos, iClr, iWnd, iPrevWnd, i;
      HB_SIZE nLen, nOptLen;
      void *hMessage, *hOpt;
      const HB_WCHAR *szMessageW = hb_itemGetStrU16(pMessage, HB_CDP_ENDIAN_NATIVE, &hMessage, &nLen), *szOptW;

      ulMaxWidth = iCols - 4;
      while (ulMsg < nLen)
      {
        if (szMessageW[ulMsg] == '\n')
        {
          ++iLines;
          if (ulCurrWidth > ulWidth)
          {
            ulWidth = ulCurrWidth;
          }
          ulCurrWidth = 0;
        }
        else
        {
          ++ulCurrWidth;
        }
        ++ulMsg;
      }
      if (ulCurrWidth)
      {
        ++iLines;
      }
      if (ulCurrWidth > ulWidth)
      {
        ulWidth = ulCurrWidth;
      }
      ulCurrWidth = 0;
      for (i = 1; i <= iOptions; ++i)
      {
        nOptLen = hb_itemCopyStrU16(hb_arrayGetItemPtr(pOptions, i), HB_CDP_ENDIAN_NATIVE, nullptr, 0);
        ulCurrWidth += static_cast<HB_UINT>(nOptLen) + (i > 1 ? 3 : 0);
      }
      if (ulCurrWidth > ulWidth)
      {
        ulWidth = ulCurrWidth;
      }
      if (ulWidth > ulMaxWidth)
      {
        ulWidth = ulMaxWidth;
      }
      if (iRows < iLines + 4)
      {
        iLines = iRows - 4;
      }
      iTop = (iRows - iLines - 4) >> 1;
      iLeft = (iCols - ulWidth - 4) >> 1;
      iBottom = iTop + iLines + 3;
      iRight = iLeft + ulWidth + 3;
      if (iClrNorm <= 0)
      {
        iClrNorm = 79;
      }
      if (iClrHigh <= 0)
      {
        iClrHigh = 31;
      }

      iDspCount = HB_GTSELF_DISPCOUNT(pGT);
      if (iDspCount == 0)
      {
        HB_GTSELF_DISPBEGIN(pGT);
      }

      iPrevWnd = hb_ctw_CurrentWindow(pCTW);
      iWnd = hb_ctw_CreateWindow(pCTW, iTop, iLeft, iBottom, iRight, true, iClrNorm, true);
      hb_ctw_AddWindowBox(pCTW, iWnd, sc_szFrameW, iClrNorm);
      HB_GTSELF_SETCURSORSTYLE(pGT, SC_NONE);
      ulLast = 0;
      i = 0;
      for (ulMsg = 0; ulMsg < nLen; ++ulMsg)
      {
        if (szMessageW[ulMsg] == '\n')
        {
          if (ulMsg > ulLast)
          {
            ul2 = ulMsg - ulLast;
            if (ul2 > ulWidth)
            {
              ul2 = ulWidth;
            }
            HB_GTSELF_PUTTEXTW(pGT, i, ((ulWidth - ul2 + 1) >> 1) + 1, iClrNorm, szMessageW + ulLast, ul2);
          }
          ulLast = ulMsg + 1;
          if (++i >= iLines)
          {
            break;
          }
        }
      }
      if (ulMsg > ulLast && i < iLines)
      {
        ul2 = ulMsg - ulLast;
        if (ul2 > ulWidth)
        {
          ul2 = ulWidth;
        }
        HB_GTSELF_PUTTEXTW(pGT, i, ((ulWidth - ul2 + 1) >> 1) + 1, iClrNorm, szMessageW + ulLast, ul2);
      }
      hb_strfree(hMessage);

      iPos = 1;
      while (iRet == 0)
      {
        int iKey, iMnuCol;

        HB_GTSELF_DISPBEGIN(pGT);
        iMnuCol = ((ulWidth - ulCurrWidth) >> 1) + 1;
        for (i = 1; i <= iOptions; ++i)
        {
          iClr = i == iPos ? iClrHigh : iClrNorm;
          szOptW = hb_arrayGetStrU16(pOptions, i, HB_CDP_ENDIAN_NATIVE, &hOpt, &nLen);
          HB_GTSELF_PUTTEXTW(pGT, iLines + 1, iMnuCol, iClr, szOptW, nLen);
          hb_strfree(hOpt);
          iMnuCol += static_cast<int>(nLen) + 3;
        }
        while (HB_GTSELF_DISPCOUNT(pGT))
        {
          HB_GTSELF_DISPEND(pGT);
        }
        HB_GTSELF_REFRESH(pGT);

        iKey = HB_GTSELF_INKEYGET(pGT, true, dDelay, INKEY_ALL);
        // TODO: add support for SET KEY blocks

        if (iKey == K_ESC)
        {
          break;
        }
        else if (iKey == K_ENTER || iKey == K_SPACE || iKey == 0)
        {
          iRet = iPos;
        }
        else if (iKey == K_LEFT || iKey == K_SH_TAB)
        {
          if (--iPos == 0)
          {
            iPos = iOptions;
          }
          dDelay = 0.0;
        }
        else if (iKey == K_RIGHT || iKey == K_TAB)
        {
          if (++iPos > iOptions)
          {
            iPos = 1;
          }
          dDelay = 0.0;
        }
#ifdef HB_COMPAT_C53
        else if (iKey == K_LBUTTONDOWN)
        {
          int iMRow = HB_GTSELF_MOUSEROW(pGT), iMCol = HB_GTSELF_MOUSECOL(pGT);
          if (iMRow == iLines + 1)
          {
            iMnuCol = ((ulWidth - ulCurrWidth) >> 1) + 1;
            for (i = 1; i <= iOptions; ++i)
            {
              nLen = hb_itemCopyStrU16(hb_arrayGetItemPtr(pOptions, i), HB_CDP_ENDIAN_NATIVE, nullptr, 0);
              if (iMCol >= iMnuCol && iMCol < iMnuCol + static_cast<int>(nLen))
              {
                iRet = i;
                break;
              }
              iMnuCol += static_cast<int>(nLen) + 3;
            }
          }
        }
#endif
        else if ((nChar = hb_inkeyKeyString(iKey, szKey, sizeof(szKey))) > 0)
        {
          auto cdp = hb_vmCDP();
          for (i = 1; i <= iOptions; ++i)
          {
            nOptLen = hb_arrayGetCLen(pOptions, i);
            if (nOptLen > 0)
            {
              HB_SIZE nIdx1 = 0, nIdx2 = 0;
              if (hb_cdpCharCaseEq(cdp, szKey, nChar, &nIdx1, hb_arrayGetCPtr(pOptions, i), nOptLen, &nIdx2))
              {
                iRet = i;
                break;
              }
            }
          }
        }
      }

      hb_ctw_CloseWindow(pCTW, iWnd);
      hb_ctw_SelectWindow(pCTW, iPrevWnd, true);
      HB_GTSELF_REFRESH(pGT);

      while (HB_GTSELF_DISPCOUNT(pGT) < iDspCount)
      {
        HB_GTSELF_DISPBEGIN(pGT);
      }

      return iRet;
    }
  }

  return HB_GTSUPER_ALERT(pGT, pMessage, pOptions, iClrNorm, iClrHigh, dDelay);
}

static int hb_ctw_gt_ReadKey(PHB_GT pGT, int iEventMask)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_ReadKey(%p,%d)", static_cast<void*>(pGT), iEventMask));
#endif

  int iKey;

  iKey = HB_GTSUPER_READKEY(pGT, iEventMask);

  if (iKey != 0)
  {
    HB_GTCTW_GET(pGT)->iLastKey = iKey;
  }

  return iKey;
}

// helper function
static HB_U32 hb_ctw_gt_cellValue(PHB_GT pGT, int iRow, int iCol)
{
  HB_SCREENCELL cell;
  int iColor;

  cell.uiValue = 0;
  HB_GTSELF_GETSCRCHAR(pGT, iRow, iCol, &iColor, &cell.c.bAttr, &cell.c.usChar);
  cell.c.bColor = static_cast<HB_BYTE>(iColor);

  return cell.uiValue;
}

static void hb_ctw_gt_RedrawDiff(PHB_GT pGT)
{
  if (HB_GTCTW_GET(pGT)->iOpenWindows == 0)
  {
    HB_GTSUPER_REDRAWDIFF(pGT);
  }
  else if (pGT->fRefresh)
  {
    int l, r, s;
    long lIndex;
    HB_U32 uiValue;

    for (auto i = 0; i < pGT->iHeight; ++i)
    {
      if (pGT->pLines[i])
      {
        lIndex = static_cast<long>(i) * pGT->iWidth;
        for (l = 0; l < pGT->iWidth; ++l, ++lIndex)
        {
          if (pGT->prevBuffer[lIndex].uiValue != (uiValue = hb_ctw_gt_cellValue(pGT, i, l)))
          {
            pGT->prevBuffer[lIndex].uiValue = uiValue;
            s = r = l;
            while (++l < pGT->iWidth)
            {
              ++lIndex;
              if (pGT->prevBuffer[lIndex].uiValue != (uiValue = hb_ctw_gt_cellValue(pGT, i, l)))
              {
                pGT->prevBuffer[lIndex].uiValue = uiValue;
                r = l;
              }
              else if (pGT->iRedrawMax != 0 && l - r >= pGT->iRedrawMax)
              {
                break;
              }
            }
            HB_GTSELF_REDRAW(pGT, i, s, r - s + 1);
          }
        }
        pGT->pLines[i] = false;
      }
    }
    pGT->fRefresh = false;
  }
}

// Public functions

HB_BOOL hb_ctwInit(void)
{
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    hb_gt_BaseFree(pCTW->pGT);
  }
  return pCTW != nullptr;
}

int hb_ctwSetShadowAttr(int iAttr)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_SetShadowAttr(pCTW, iAttr);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwSetMoveMode(int iMode)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_SetMoveMode(pCTW, iMode);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwSetMoveStep(int iVertical, int iHorizontal)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_SetMoveStep(pCTW, iVertical, iHorizontal);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwSetWindowBoard(int iTop, int iLeft, int iBottom, int iRight)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_SetWindowBoard(pCTW, iTop, iLeft, iBottom, iRight);
    HB_GTSELF_FLUSH(pCTW->pGT);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwSetBorderMode(int iTop, int iLeft, int iBottom, int iRight)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_SetBorderMode(pCTW, iTop, iLeft, iBottom, iRight);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwCreateWindow(int iTop, int iLeft, int iBottom, int iRight, HB_BOOL fClear, int iColor, HB_BOOL fVisible)
{
  int iResult = -1;

  if (iTop <= iBottom && iLeft <= iRight)
  {
    PHB_GTCTW pCTW = hb_ctw_base();

    if (pCTW)
    {
      iResult = hb_ctw_CreateWindow(pCTW, iTop, iLeft, iBottom, iRight, fClear, iColor, fVisible);
      HB_GTSELF_FLUSH(pCTW->pGT);
      hb_gt_BaseFree(pCTW->pGT);
    }
  }
  return iResult;
}

int hb_ctwCloseAllWindows(void)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_CloseAllWindows(pCTW);
    HB_GTSELF_FLUSH(pCTW->pGT);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwCloseWindow(int iWindow)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_CloseWindow(pCTW, iWindow);
    HB_GTSELF_FLUSH(pCTW->pGT);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwCurrentWindow(void)
{
  int iResult = 0;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_CurrentWindow(pCTW);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwSelectWindow(int iWindow, HB_BOOL fToTop)
{
  int iResult = 0;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_SelectWindow(pCTW, iWindow, fToTop);
    HB_GTSELF_FLUSH(pCTW->pGT);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwChangeWindowHandle(int iNewWindow)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_ChangeWindowHandle(pCTW, iNewWindow);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwGetWindowStack(const int **piStack)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_GetWindowStack(pCTW, piStack);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwVisible(int iWindow, int iVisible)
{
  int iResult = HB_CTW_UNDEF;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_Visible(pCTW, iWindow, iVisible);
    HB_GTSELF_FLUSH(pCTW->pGT);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwSetWindowLevel(int iWindow, int iLevel)
{
  int iResult = HB_CTW_UNDEF;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_SetWindowLevel(pCTW, iWindow, iLevel);
    HB_GTSELF_FLUSH(pCTW->pGT);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwSetWindowShadow(int iWindow, int iAttr)
{
  int iResult = HB_CTW_UNDEF;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_SetWindowShadow(pCTW, iWindow, iAttr);
    HB_GTSELF_FLUSH(pCTW->pGT);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwMaxWindow(void)
{
  int iResult = 0;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_MaxWindow(pCTW);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwChangeMargins(int iWindow, int iTop, int iLeft, int iBottom, int iRight)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_ChangeMargins(pCTW, iWindow, iTop, iLeft, iBottom, iRight);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwSetWindowClip(int iWindow, int iTop, int iLeft, int iBottom, int iRight)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_SetWindowClip(pCTW, iWindow, iTop, iLeft, iBottom, iRight);
    HB_GTSELF_FLUSH(pCTW->pGT);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwGetWindowCords(int iWindow, HB_BOOL fCenter, int *piTop, int *piLeft, int *piBottom, int *piRight)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_GetWindowCords(pCTW, iWindow, fCenter, piTop, piLeft, piBottom, piRight);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwGetFormatCords(int iWindow, HB_BOOL fRelative, int *piTop, int *piLeft, int *piBottom, int *piRight)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_GetFormatCords(pCTW, iWindow, fRelative, piTop, piLeft, piBottom, piRight);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwMoveWindow(int iWindow, int iRow, int iCol)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_MoveWindow(pCTW, iWindow, iRow, iCol);
    HB_GTSELF_FLUSH(pCTW->pGT);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwCenterWindow(int iWindow, HB_BOOL fCenter)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_CenterWindow(pCTW, iWindow, fCenter);
    HB_GTSELF_FLUSH(pCTW->pGT);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwAddWindowBox(int iWindow, const HB_WCHAR *szBoxW, int iColor)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_AddWindowBox(pCTW, iWindow, szBoxW, iColor);
    HB_GTSELF_FLUSH(pCTW->pGT);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwSwapWindows(int iWindow1, int iWindow2)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = hb_ctw_SwapWindows(pCTW, iWindow1, iWindow2);
    HB_GTSELF_FLUSH(pCTW->pGT);
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwGetPosWindow(int iRow, int iCol)
{
  int iResult = -1;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    if (pCTW->iMaxWindow && iRow >= pCTW->iBoardTop && iRow <= pCTW->iBoardBottom && iCol >= pCTW->iBoardLeft &&
        iCol <= pCTW->iBoardRight)
    {
      long lIndex = static_cast<long>(iRow) * pCTW->iMapWidth + iCol;
      iResult = pCTW->pWindowMap[lIndex];
    }
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

int hb_ctwLastKey(int *piNewKey)
{
  // keyread() in CT3 uses 64512 bytes length buffer
  // when it reach this limit and new key is added the
  // buffer size is decreased by 1024 to 63488 bytes
  // before adding key. TODO: check if buffer is shifted
  int iResult = 0;
  PHB_GTCTW pCTW = hb_ctw_base();

  if (pCTW)
  {
    iResult = pCTW->iLastKey;
    if (piNewKey)
    {
      pCTW->iLastKey = *piNewKey;
    }
    hb_gt_BaseFree(pCTW->pGT);
  }
  return iResult;
}

static HB_BOOL hb_gt_FuncInit(PHB_GT_FUNCS pFuncTable)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", static_cast<void*>(pFuncTable)));
#endif

  pFuncTable->Exit = hb_ctw_gt_Exit;
  pFuncTable->MouseRow = hb_ctw_MouseRow;
  pFuncTable->MouseCol = hb_ctw_MouseCol;
  pFuncTable->MaxCol = hb_ctw_gt_MaxCol;
  pFuncTable->MaxRow = hb_ctw_gt_MaxRow;
  pFuncTable->GetPos = hb_ctw_gt_GetPos;
  pFuncTable->SetPos = hb_ctw_gt_SetPos;
  pFuncTable->WriteCon = hb_ctw_gt_WriteCon;
  pFuncTable->WriteConW = hb_ctw_gt_WriteConW;
  pFuncTable->GetCursorStyle = hb_ctw_gt_GetCursorStyle;
  pFuncTable->SetCursorStyle = hb_ctw_gt_SetCursorStyle;
  pFuncTable->GetColorStr = hb_ctw_gt_GetColorStr;
  pFuncTable->SetColorStr = hb_ctw_gt_SetColorStr;
  pFuncTable->ColorSelect = hb_ctw_gt_ColorSelect;
  pFuncTable->GetColor = hb_ctw_gt_GetColor;
  pFuncTable->GetColorData = hb_ctw_gt_GetColorData;
  pFuncTable->GetScrCursor = hb_ctw_gt_GetScrCursor;
  pFuncTable->GetScrChar = hb_ctw_gt_GetScrChar;
  pFuncTable->GetScrUC = hb_ctw_gt_GetScrUC;
  pFuncTable->GetChar = hb_ctw_gt_GetChar;
  pFuncTable->PutChar = hb_ctw_gt_PutChar;
  pFuncTable->Resize = hb_ctw_gt_Resize;
  pFuncTable->Info = hb_ctw_gt_Info;
  pFuncTable->Alert = hb_ctw_gt_Alert;
  pFuncTable->ReadKey = hb_ctw_gt_ReadKey;
  pFuncTable->RedrawDiff = hb_ctw_gt_RedrawDiff;

  return true;
}

// ---

#define HB_GTSUPER NULL
#include "hbgtreg.h"

// ---
