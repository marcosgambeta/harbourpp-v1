//
// Harbour Graphic Terminal low-level code
//
// Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
//
// part of the code in hb_gt_def_* functions is based on the code
// from old hbapi.c copyrighted by:
// Copyright 1999 Bil Simser <bsimser@home.com>
// Copyright 1999 Paul Tucker <ptucker@sympatico.ca>
// Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
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

#define HB_GT_NAME NUL

#include "hbgtcore.hpp"
#include "hbapiitm.hpp"
#include "hbapistr.hpp"
#include "hbapifs.hpp"
#include "hbapierr.hpp"
#include "hbapicdp.hpp"
#include "hbdate.hpp"
#include "hbset.hpp"
#include "hbvm.hpp"
#include "hbthread.hpp"
#include "hbstack.hpp"

static const HB_WCHAR s_szSpaceW[] = {' ', 0};

PHB_GT hb_gt_Base(void)
{
  auto pGT = static_cast<PHB_GT>(hb_stackGetGT());

  if (pGT && HB_GTSELF_LOCK(pGT)) {
    return pGT;
  } else {
    return nullptr;
  }
}

void hb_gt_BaseFree(PHB_GT pGT)
{
  if (pGT != nullptr) {
    HB_GTSELF_UNLOCK(pGT);
  }
}

void hb_gt_BaseUnlock(PHB_GT pGT)
{
  HB_GTSELF_UNLOCK(pGT);
}

void hb_gt_BaseLock(PHB_GT pGT)
{
  HB_GTSELF_LOCK(pGT);
}

void hb_gtSleep(PHB_GT pGT, double dSeconds)
{
  HB_GTSELF_UNLOCK(pGT);
  hb_idleSleep(dSeconds);
  HB_GTSELF_LOCK(pGT);
}

// helper internal function
static void hb_gt_def_BaseInit(PHB_GT_BASE pGT)
{
  pGT->fVgaCell = true;
  pGT->fIsColor = true;
  pGT->fBlinking = true;
  pGT->fStdOutCon = false;
  pGT->fStdErrCon = false;
  pGT->iCursorShape = SC_NORMAL;
  pGT->iDispCount = 0;
  pGT->iExtCount = 0;
  pGT->usClearChar = ' ';
  pGT->iClearColor = 0x07;
  pGT->iHeight = 24;
  pGT->iWidth = 80;
  pGT->hStdIn = HB_STDIN_HANDLE;
  pGT->hStdOut = HB_STDOUT_HANDLE;
  pGT->hStdErr = HB_STDERR_HANDLE;

  pGT->iDoubleClickSpeed = 168; // In milliseconds

  pGT->inkeyBuffer = pGT->defaultKeyBuffer;
  pGT->inkeyBufferSize = HB_DEFAULT_INKEY_BUFSIZE;

  pGT->cdpTerm = nullptr;
  pGT->cdpHost = nullptr;
  pGT->cdpIn = nullptr;
  pGT->cdpBox = hb_cdpFind("EN");

  pGT->pMutex = hb_threadMutexCreate();
  if (pGT->pMutex) {
    hb_gcUnlock(pGT->pMutex);
  }
}

static void *hb_gt_def_New(PHB_GT pGT)
{
  HB_SIZE nSize, nIndex;
  HB_USHORT usChar;
  int iColor;
  HB_BYTE bAttr;
  int i;

  hb_gt_def_BaseInit(pGT);

  HB_GTSELF_GETSIZE(pGT, &pGT->iHeight, &pGT->iWidth);
  nSize = static_cast<HB_SIZE>(pGT->iHeight) * pGT->iWidth;

  pGT->screenBuffer = static_cast<PHB_SCREENCELL>(hb_xgrab(sizeof(HB_SCREENCELL) * nSize));
  pGT->prevBuffer = static_cast<PHB_SCREENCELL>(hb_xgrabz(sizeof(HB_SCREENCELL) * nSize));
  pGT->pLines = static_cast<HB_BOOL *>(hb_xgrab(sizeof(HB_BOOL) * pGT->iHeight));

  for (i = 0; i < pGT->iHeight; ++i) {
    pGT->pLines[i] = true;
  }

  usChar = HB_GTSELF_GETCLEARCHAR(pGT);
  iColor = HB_GTSELF_GETCLEARCOLOR(pGT);
  bAttr = 0;
  for (nIndex = 0; nIndex < nSize; ++nIndex) {
    pGT->screenBuffer[nIndex].c.usChar = usChar;
    pGT->screenBuffer[nIndex].c.bColor = static_cast<HB_BYTE>(iColor);
    pGT->screenBuffer[nIndex].c.bAttr = bAttr;
    pGT->prevBuffer[nIndex].c.bAttr = HB_GT_ATTR_REFRESH;
  }

  return pGT;
}

static void hb_gt_def_Free(PHB_GT pGT)
{
  if (pGT == static_cast<PHB_GT>(hb_stackGetGT())) {
    hb_stackSetGT(nullptr);
  }

  if (pGT->pNotifierBlock) {
    hb_itemRelease(pGT->pNotifierBlock);
    pGT->pNotifierBlock = nullptr;
  }
  if (pGT->pInkeyFilterBlock) {
    hb_itemRelease(pGT->pInkeyFilterBlock);
    pGT->pInkeyFilterBlock = nullptr;
  }
  if (pGT->pInkeyReadBlock) {
    hb_itemRelease(pGT->pInkeyReadBlock);
    pGT->pInkeyReadBlock = nullptr;
  }
  if (pGT->pCargo) {
    hb_itemRelease(pGT->pCargo);
    pGT->pCargo = nullptr;
  }
  if (pGT->pMutex) {
    hb_itemRelease(pGT->pMutex);
    pGT->pMutex = nullptr;
  }

  if (pGT->screenBuffer) {
    hb_xfree(pGT->screenBuffer);
  }
  if (pGT->prevBuffer) {
    hb_xfree(pGT->prevBuffer);
  }
  if (pGT->pLines) {
    hb_xfree(pGT->pLines);
  }
  if (pGT->iColorCount > 0) {
    hb_xfree(pGT->pColor);
  }
  if (pGT->pFuncTable) {
    hb_xfree(pGT->pFuncTable);
  }

  hb_xfree(pGT);
}

static void hb_gt_def_Mark(PHB_GT pGT)
{
  if (pGT->pNotifierBlock) {
    hb_gcMark(pGT->pNotifierBlock);
  }
  if (pGT->pInkeyFilterBlock) {
    hb_gcMark(pGT->pInkeyFilterBlock);
  }
  if (pGT->pInkeyReadBlock) {
    hb_gcMark(pGT->pInkeyReadBlock);
  }
  if (pGT->pCargo) {
    hb_gcMark(pGT->pCargo);
  }
  if (pGT->pMutex) {
    hb_gcMark(pGT->pMutex);
  }
}

static HB_BOOL hb_gt_def_Lock(PHB_GT pGT)
{
  return !pGT->pMutex || hb_threadMutexLock(pGT->pMutex);
}

static void hb_gt_def_Unlock(PHB_GT pGT)
{
  if (pGT->pMutex) {
    hb_threadMutexUnlock(pGT->pMutex);
  }
}

static void hb_gt_def_Init(PHB_GT pGT, HB_FHANDLE hStdIn, HB_FHANDLE hStdOut, HB_FHANDLE hStdErr)
{
  HB_GTSELF_NEW(pGT);

  pGT->hStdIn = hStdIn;
  pGT->hStdOut = hStdOut;
  pGT->hStdErr = hStdErr;

  HB_GTSELF_RESIZE(pGT, pGT->iHeight, pGT->iWidth);
  HB_GTSELF_MOUSEINIT(pGT);
  HB_GTSELF_MOUSEGETPOS(pGT, &pGT->iMouseLastRow, &pGT->iMouseLastCol);
}

static void hb_gt_def_Exit(PHB_GT pGT)
{
  HB_GTSELF_MOUSEEXIT(pGT);
  HB_GTSELF_INKEYEXIT(pGT);

  HB_GTSELF_FREE(pGT);
}

static HB_BOOL hb_gt_def_CheckPos(PHB_GT pGT, int iRow, int iCol, long *plIndex)
{
  if (iRow >= 0 && iCol >= 0) {
    int iHeight, iWidth;

    HB_GTSELF_GETSIZE(pGT, &iHeight, &iWidth);
    if (iRow < iHeight && iCol < iWidth) {
      if (plIndex) {
        *plIndex = static_cast<long>(iRow) * iWidth + iCol;
      }
      return true;
    }
  }
  return false;
}

static void hb_gt_def_GetPos(PHB_GT pGT, int *piRow, int *piCol)
{
  *piRow = pGT->iRow;
  *piCol = pGT->iCol;
}

static void hb_gt_def_SetPos(PHB_GT pGT, int iRow, int iCol)
{
  pGT->iRow = iRow;
  pGT->iCol = iCol;
}

static int hb_gt_def_MaxCol(PHB_GT pGT)
{
  return pGT->iWidth - 1;
}

static int hb_gt_def_MaxRow(PHB_GT pGT)
{
  return pGT->iHeight - 1;
}

static HB_BOOL hb_gt_def_IsColor(PHB_GT pGT)
{
  return pGT->fIsColor;
}

// NOTE: szColorString must be at least HB_CLRSTR_LEN wide by the NG. It seems
//       that CA-Cl*pper SetColor() will return string lengths up to 131+EOF.
//       That seems like a 127+1 buffer size, plus lazy overflow checking.
//       [vszakats]
static void hb_gt_def_GetColorStr(PHB_GT pGT, char *pszColorString)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_GetColorStr(%p,%s)", static_cast<void*>(pGT), pszColorString));
#endif

  HB_GTSELF_COLORSTOSTRING(pGT, pGT->pColor, pGT->iColorCount, pszColorString, HB_CLRSTR_LEN);
}

static void hb_gt_def_SetColorStr(PHB_GT pGT, const char *szColorString)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_SetColorStr(%p,%s)", static_cast<void*>(pGT), szColorString));
#endif

  HB_GTSELF_STRINGTOCOLORS(pGT, szColorString, &pGT->pColor, &pGT->iColorCount);
  pGT->iColorIndex = HB_CLR_STANDARD; // HB_GTSELF_COLORSELECT(pGT, HB_CLR_STANDARD);
}

static void hb_gt_def_ColorSelect(PHB_GT pGT, int iColorIndex)
{
  if (iColorIndex >= 0 && iColorIndex < pGT->iColorCount) {
    pGT->iColorIndex = iColorIndex;
  }
}

static int hb_gt_def_GetColor(PHB_GT pGT)
{
  if (pGT->iColorCount) {
    return pGT->pColor[pGT->iColorIndex];
  } else {
    return HB_GTSELF_GETCLEARCOLOR(pGT);
  }
}

static void hb_gt_def_GetColorData(PHB_GT pGT, int **pColorsPtr, int *piColorCount, int *piColorIndex)
{
  if (pGT->iColorCount) {
    *pColorsPtr = static_cast<int *>(hb_xgrab(pGT->iColorCount * sizeof(int)));
    memcpy(*pColorsPtr, pGT->pColor, pGT->iColorCount * sizeof(int));
    *piColorCount = pGT->iColorCount;
    *piColorIndex = pGT->iColorIndex;
  } else {
    *pColorsPtr = static_cast<int *>(hb_xgrab(sizeof(int)));
    *pColorsPtr[0] = 0;
    *piColorCount = 1;
    *piColorIndex = 0;
  }
}

static int hb_gt_def_GetClearColor(PHB_GT pGT)
{
  return pGT->iClearColor;
}

static void hb_gt_def_SetClearColor(PHB_GT pGT, int iColor)
{
  pGT->iClearColor = (iColor & 0xFF);
}

static HB_USHORT hb_gt_def_GetClearChar(PHB_GT pGT)
{
  return pGT->usClearChar;
}

static void hb_gt_def_SetClearChar(PHB_GT pGT, HB_USHORT usChar)
{
  pGT->usClearChar = usChar;
}

// helper internal function
// masks: 0x0007     Foreground
//        0x0070     Background
//        0x0008     Bright
//        0x0080     Blink
//        0x0800     Underline foreground
//        0x8000     Underline background

static const char *hb_gt_def_ColorDecode(const char *szColorString, int *piColor)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_ColorDecode(%s,%p)", szColorString, static_cast<void*>(piColor)));
#endif

  char c;
  int nColor = 0, iCount = 0;
  auto bFore = true;

  while ((c = *szColorString++) != 0) {
    switch (c) {
    case '*':
      nColor |= 0x80;
      break;

    case '+':
      nColor |= 0x08;
      break;

    case '/':
      if (!bFore) {
        nColor = ((nColor >> 4) & 0x0F07) | (nColor & 0x88);
      } else {
        bFore = false;
      }
      break;

    case 'b':
    case 'B':
      nColor |= bFore ? 0x01 : 0x10;
      break;

    case 'g':
    case 'G':
      nColor |= bFore ? 0x02 : 0x20;
      break;

    case 'r':
    case 'R':
      nColor |= bFore ? 0x04 : 0x40;
      break;

    case 'w':
    case 'W':
      nColor |= bFore ? 0x07 : 0x70;
      break;

    case 'n':
    case 'N':
      nColor &= bFore ? 0xFFF8 : 0xFF8F;
      break;

    case 'i':
    case 'I':
      bFore = false;
      nColor &= 0x88;
      nColor |= 0x70;
      break;

    case 'x':
    case 'X':
      nColor &= 0x88;
      break;

    case 'u':
    case 'U':
      if (bFore) {
        nColor = (nColor & 0xF0F8) | 0x0801;
      } else {
        nColor = (nColor & 0x0F8F) | 0x8010;
      }
      break;

    case ',':
      *piColor = iCount == 0 ? -1 : nColor;
      return szColorString;

    default:
      if (c >= '0' && c <= '9') {
        int iColor = c - '0';
        while (*szColorString >= '0' && *szColorString <= '9') {
          iColor = iColor * 10 + (*szColorString++ - '0');
        }
        iColor &= 0x0f;
        if (bFore) {
          nColor = (nColor & 0xF0F8) | iColor;
        } else {
          nColor = (nColor & 0x0F8F) | (iColor << 4);
        }
      } else {
        --iCount;
      }
    }
    ++iCount;
  }

  *piColor = iCount == 0 ? -1 : nColor;
  return nullptr;
}

static int hb_gt_def_ColorNum(PHB_GT pGT, const char *szColorString)
{
  int nColor;
  HB_SYMBOL_UNUSED(pGT);
  hb_gt_def_ColorDecode(szColorString, &nColor);
  return nColor;
}

static void hb_gt_def_StringToColors(PHB_GT pGT, const char *szColorString, int **pColorsPtr, int *piColorCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_StringToColors(%p,%s,%p,%p)", static_cast<void*>(pGT), szColorString, static_cast<void*>(pColorsPtr), static_cast<void*>(piColorCount)));
#endif

  int *pColors;
  int nColor;

  HB_SYMBOL_UNUSED(pGT);

  if (*piColorCount == 0) {
    *piColorCount = HB_CLR_MAX_ + 1;
    *pColorsPtr = static_cast<int *>(hb_xgrabz(*piColorCount * sizeof(int)));
  }

  pColors = *pColorsPtr;

  if (!szColorString || !*szColorString) {
    pColors[HB_CLR_STANDARD] = 0x07;
    pColors[HB_CLR_ENHANCED] = 0x70;
    pColors[HB_CLR_BORDER] = 0;
    pColors[HB_CLR_BACKGROUND] = 0;
    pColors[HB_CLR_UNSELECTED] = 0x70;
  } else {
    int nPos = 0;

    do {
      szColorString = hb_gt_def_ColorDecode(szColorString, &nColor);

      if (nPos == *piColorCount) {
        ++*piColorCount;
        pColors = *pColorsPtr = static_cast<int *>(hb_xrealloc(pColors, *piColorCount * sizeof(int)));
        pColors[nPos] = 0;
      }
      if (nColor != -1) {
        pColors[nPos] = nColor;
        if (nPos == HB_CLR_ENHANCED && *piColorCount > HB_CLR_UNSELECTED) {
          pColors[HB_CLR_UNSELECTED] = nColor;
        }
      }
      ++nPos;
    } while (szColorString);
  }
}

static void hb_gt_def_ColorsToString(PHB_GT pGT, int *pColors, int iColorCount, char *pszColorString, int iBufSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_ColorsToString(%p,%p,%d,%p,%d)", static_cast<void*>(pGT), static_cast<void*>(pColors), iColorCount, static_cast<void*>(pszColorString), iBufSize));
#endif

  int iPos;

  HB_SYMBOL_UNUSED(pGT);

  // Go on if there's space left for the largest color string plus EOF
  for (int iColorIndex = iPos = 0; iColorIndex < iColorCount && iPos < iBufSize - 8; ++iColorIndex) {
    int nColor = pColors[iColorIndex] & 7;

    if (iColorIndex > 0) {
      pszColorString[iPos++] = ',';
    }

    for (auto j = 0; j <= 1; j++) {
      if ((pColors[iColorIndex] & (j ? 0x8000 : 0x0800)) == 0) {
        if (nColor == 7) {
          pszColorString[iPos++] = 'W';
        } else if (nColor == 0) {
          pszColorString[iPos++] = 'N';
        } else {
          if ((nColor & 1) != 0) {
            pszColorString[iPos++] = 'B';
          }

          if ((nColor & 2) != 0) {
            pszColorString[iPos++] = 'G';
          }

          if ((nColor & 4) != 0) {
            pszColorString[iPos++] = 'R';
          }
        }
      } else {
        pszColorString[iPos++] = 'U';
      }

      if (j == 0) {
        // NOTE: When STRICT is on, Harbour will put both the "*" and "+"
        //       chars to the first half of the colorspec (like "W*+/B"),
        //       which is quite ugly, otherwise it will put the "+" to the
        //       first half and the "*" to the second (like "W+/B*"), which
        //       is how it should be done. [vszakats]
#ifdef HB_CLP_STRICT
        if ((pColors[iColorIndex] & 0x80) != 0) {
          pszColorString[iPos++] = '*';
        }
#endif

        if ((pColors[iColorIndex] & 0x08) != 0) {
          pszColorString[iPos++] = '+';
        }

        pszColorString[iPos++] = '/';
      }
#ifndef HB_CLP_STRICT
      else {
        if ((pColors[iColorIndex] & 0x80) != 0) {
          pszColorString[iPos++] = '*';
        }
      }
#endif
      nColor = (pColors[iColorIndex] >> 4) & 7;
    }
  }

  pszColorString[iPos] = '\0';
}

static int hb_gt_def_GetCursorStyle(PHB_GT pGT)
{
  return pGT->iCursorShape;
}

static void hb_gt_def_SetCursorStyle(PHB_GT pGT, int iStyle)
{
  switch (iStyle) {
  case SC_NONE:
  case SC_NORMAL:
  case SC_INSERT:
  case SC_SPECIAL1:
  case SC_SPECIAL2:
    pGT->iCursorShape = iStyle;
    break;
  default:
    pGT->iCursorShape = SC_NORMAL;
    break;
  }
}

static void hb_gt_def_GetScrCursor(PHB_GT pGT, int *piRow, int *piCol, int *piStyle)
{
  HB_GTSELF_GETPOS(pGT, piRow, piCol);
  if (*piRow < 0 || *piCol < 0 || *piRow > HB_GTSELF_MAXROW(pGT) || *piCol > HB_GTSELF_MAXCOL(pGT)) {
    *piStyle = SC_NONE;
  } else {
    *piStyle = HB_GTSELF_GETCURSORSTYLE(pGT);
  }
}

static HB_BOOL hb_gt_def_GetBlink(PHB_GT pGT)
{
  return pGT->fBlinking;
}

static void hb_gt_def_SetBlink(PHB_GT pGT, HB_BOOL fBlink)
{
  pGT->fBlinking = fBlink;
}

static void hb_gt_def_SetSnowFlag(PHB_GT pGT, HB_BOOL fNoSnow)
{
  // NOTE: This is a compatibility function which have to be implemented
  //       in low-level GT driver.
  //       If you're running on a CGA and snow is a problem speak up!

  HB_SYMBOL_UNUSED(pGT);
  HB_SYMBOL_UNUSED(fNoSnow);
}

static void hb_gt_def_DispBegin(PHB_GT pGT)
{
  pGT->iDispCount++;
}

static void hb_gt_def_DispEnd(PHB_GT pGT)
{
  if (pGT->iDispCount > 0) {
    pGT->iDispCount--;
  }
}

static int hb_gt_def_DispCount(PHB_GT pGT)
{
  return pGT->iDispCount;
}

static HB_BOOL hb_gt_def_PreExt(PHB_GT pGT)
{
  if (pGT->iExtCount == 0) {
    HB_GTSELF_REFRESH(pGT);
  }
  pGT->iExtCount++;

  return true;
}

static HB_BOOL hb_gt_def_PostExt(PHB_GT pGT)
{
  if (pGT->iExtCount) {
    pGT->iExtCount--;
  }

  return true;
}

static HB_BOOL hb_gt_def_Suspend(PHB_GT pGT)
{
  return HB_GTSELF_PREEXT(pGT);
}

static HB_BOOL hb_gt_def_Resume(PHB_GT pGT)
{
  return HB_GTSELF_POSTEXT(pGT);
}

static void hb_gt_def_OutStd(PHB_GT pGT, const char *szStr, HB_SIZE nLen)
{
  if (nLen) {
    if (pGT->fStdOutCon) {
      HB_GTSELF_WRITECON(pGT, szStr, nLen);
    } else {
      HB_GTSELF_PREEXT(pGT);
      if (pGT->fDispTrans) {
        char *szStrBuff = hb_cdpnDup(szStr, &nLen, pGT->cdpHost, pGT->cdpTerm);
        hb_fsWriteLarge(pGT->hStdOut, szStrBuff, nLen);
        hb_xfree(szStrBuff);
      } else {
        hb_fsWriteLarge(pGT->hStdOut, szStr, nLen);
      }
      HB_GTSELF_POSTEXT(pGT);
    }
  }
}

static void hb_gt_def_OutErr(PHB_GT pGT, const char *szStr, HB_SIZE nLen)
{
  if (nLen) {
    if (pGT->fStdErrCon) {
      HB_GTSELF_WRITECON(pGT, szStr, nLen);
    } else {
      HB_GTSELF_PREEXT(pGT);
      if (pGT->fDispTrans) {
        char *szStrBuff = hb_cdpnDup(szStr, &nLen, pGT->cdpHost, pGT->cdpTerm);
        hb_fsWriteLarge(pGT->hStdErr, szStrBuff, nLen);
        hb_xfree(szStrBuff);
      } else {
        hb_fsWriteLarge(pGT->hStdErr, szStr, nLen);
      }
      HB_GTSELF_POSTEXT(pGT);
    }
  }
}

static void hb_gt_def_Tone(PHB_GT pGT, double dFrequency, double dDuration)
{
  HB_SYMBOL_UNUSED(dFrequency);

  // convert Clipper (MS-DOS) timer tick units to seconds ( x / 18.2 )
  hb_gtSleep(pGT, dDuration / 18.2);
}

static void hb_gt_def_Bell(PHB_GT pGT)
{
  HB_GTSELF_TONE(pGT, 700.0, 3.0);
}

static const char *hb_gt_def_Version(PHB_GT pGT, int iType)
{
  HB_SYMBOL_UNUSED(pGT);

  if (iType == 0) {
    return "NUL";
  }

  return "Harbour++ Terminal: NULL";
}

static HB_BOOL hb_gt_def_GetChar(PHB_GT pGT, int iRow, int iCol, int *piColor, HB_BYTE *pbAttr, HB_USHORT *pusChar)
{
  long lIndex;

  if (HB_GTSELF_CHECKPOS(pGT, iRow, iCol, &lIndex)) {
    *pusChar = pGT->screenBuffer[lIndex].c.usChar;
    *piColor = pGT->screenBuffer[lIndex].c.bColor;
    *pbAttr = pGT->screenBuffer[lIndex].c.bAttr;
    return true;
  }
  return false;
}

static HB_BOOL hb_gt_def_GetUC(PHB_GT pGT, int iRow, int iCol, int *piColor, HB_BYTE *pbAttr, HB_UCHAR *puChar,
                               HB_BOOL fTerm)
{
  long lIndex;

  if (HB_GTSELF_CHECKPOS(pGT, iRow, iCol, &lIndex)) {
    HB_WCHAR wc = pGT->screenBuffer[lIndex].c.usChar;
    HB_UCHAR uc = 0;

    *piColor = pGT->screenBuffer[lIndex].c.bColor;
    *pbAttr = pGT->screenBuffer[lIndex].c.bAttr;

    if (wc) {
      if (fTerm && pGT->cdpTerm) {
        uc = hb_cdpGetUC(pGT->cdpTerm, wc, 0);
      }
      if (uc == 0) {
        if (pGT->cdpBox && (!fTerm || pGT->cdpBox != pGT->cdpTerm) && pGT->cdpBox != pGT->cdpHost &&
            (*pbAttr & HB_GT_ATTR_BOX)) {
          uc = hb_cdpGetUC(pGT->cdpBox, wc, 0);
        }
        if (uc == 0) {
          if (pGT->cdpHost && pGT->cdpTerm != pGT->cdpHost) {
            uc = hb_cdpGetUC(pGT->cdpHost, wc, 0);
          }
          if (uc == 0) {
            uc = hb_cdpGetUC(hb_vmCDP(), wc, wc < 32 ? static_cast<HB_UCHAR>(wc) : '?');
          }
        }
      }
    }
    *puChar = uc;
    return true;
  }
  return false;
}

static HB_BOOL hb_gt_def_PutChar(PHB_GT pGT, int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar)
{
  long lIndex;

  if (HB_GTSELF_CHECKPOS(pGT, iRow, iCol, &lIndex)) {
    pGT->screenBuffer[lIndex].c.usChar = usChar;
    pGT->screenBuffer[lIndex].c.bColor = static_cast<HB_BYTE>(iColor);
    pGT->screenBuffer[lIndex].c.bAttr = bAttr;
    pGT->pLines[iRow] = true;
    pGT->fRefresh = true;
    return true;
  }
  return false;
}

static int hb_gt_def_PutText(PHB_GT pGT, int iRow, int iCol, int iColor, const char *szText, HB_SIZE nLen)
{
  PHB_CODEPAGE cdp = HB_GTSELF_HOSTCP(pGT);
  HB_SIZE nIndex = 0;
  HB_WCHAR wc;

  while (HB_CDPCHAR_GET(cdp, szText, nLen, &nIndex, &wc)) {
    if (!HB_GTSELF_PUTCHAR(pGT, iRow, iCol++, iColor, 0, wc)) {
      while (HB_CDPCHAR_GET(cdp, szText, nLen, &nIndex, &wc)) {
        ++iCol;
      }
      break;
    }
  }
  return iCol;
}

static int hb_gt_def_PutTextW(PHB_GT pGT, int iRow, int iCol, int iColor, const HB_WCHAR *szText, HB_SIZE nLen)
{
  if (nLen) {
    do {
      if (!HB_GTSELF_PUTCHAR(pGT, iRow, iCol, iColor, 0, *szText++)) {
        break;
      }
      ++iCol;
    } while (--nLen);
  }

  return iCol + static_cast<int>(nLen);
}

static void hb_gt_def_Replicate(PHB_GT pGT, int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar,
                                HB_SIZE nLen)
{
  if (iCol < 0) {
    if (nLen < static_cast<HB_SIZE>(-iCol)) {
      nLen = 0;
    } else {
      nLen += iCol;
    }
    iCol = 0;
  }
  while (nLen--) {
    if (!HB_GTSELF_PUTCHAR(pGT, iRow, iCol, iColor, bAttr, usChar)) {
      break;
    }
    ++iCol;
  }
}

static void hb_gt_def_WriteAt(PHB_GT pGT, int iRow, int iCol, const char *szText, HB_SIZE nLength)
{
  iCol = HB_GTSELF_PUTTEXT(pGT, iRow, iCol, HB_GTSELF_GETCOLOR(pGT), szText, nLength);
  int iMaxCol = HB_GTSELF_MAXCOL(pGT);
  if (iCol > iMaxCol + 1) {
    iCol = iMaxCol + 1;
  }
  HB_GTSELF_SETPOS(pGT, iRow, iCol);
}

static void hb_gt_def_WriteAtW(PHB_GT pGT, int iRow, int iCol, const HB_WCHAR *szText, HB_SIZE nLength)
{
  int iMaxCol = HB_GTSELF_MAXCOL(pGT);
  // Truncate the text if the cursor will end up off the right edge
  iCol = HB_GTSELF_PUTTEXTW(pGT, iRow, iCol, HB_GTSELF_GETCOLOR(pGT), szText,
                            HB_MIN(nLength, static_cast<HB_SIZE>(iMaxCol - iCol + 1)));
  // Finally, save the new cursor position, even if off-screen
  HB_GTSELF_SETPOS(pGT, iRow, iCol);
}

static void hb_gt_def_Write(PHB_GT pGT, const char *szText, HB_SIZE nLength)
{
  int iRow, iCol;
  HB_GTSELF_GETPOS(pGT, &iRow, &iCol);
  HB_GTSELF_WRITEAT(pGT, iRow, iCol, szText, nLength);
}

static void hb_gt_def_WriteW(PHB_GT pGT, const HB_WCHAR *szText, HB_SIZE nLength)
{
  int iRow, iCol;
  HB_GTSELF_GETPOS(pGT, &iRow, &iCol);
  HB_GTSELF_WRITEATW(pGT, iRow, iCol, szText, nLength);
}

#define WRITECON_BUFFER_SIZE 512

static void hb_gt_def_WriteCon(PHB_GT pGT, const char *szText, HB_SIZE nLength)
{
  int iLen = 0;
  auto bDisp = false;
  auto bBell = false;
  auto bNewLine = false;
  HB_WCHAR szString[WRITECON_BUFFER_SIZE];
  PHB_CODEPAGE cdp = HB_GTSELF_HOSTCP(pGT);
  HB_SIZE nIndex = 0;
  HB_WCHAR wc;

  int iMaxRow = HB_GTSELF_MAXROW(pGT);
  int iMaxCol = HB_GTSELF_MAXCOL(pGT);

  int iRow, iCol;
  HB_GTSELF_GETPOS(pGT, &iRow, &iCol);

  // Limit the starting cursor position to MaxRow(),MaxCol()
  // on the high end, but don't limit it on the low end.

  if (iRow > iMaxRow || iCol > iMaxCol) {
    if (iRow > iMaxRow) {
      iRow = iMaxRow;
    }
    if (iCol > iMaxCol) {
      iCol = iMaxCol;
    }
    HB_GTSELF_SETPOS(pGT, iRow, iCol);
  }

  while (HB_CDPCHAR_GET(cdp, szText, nLength, &nIndex, &wc)) {
    switch (wc) {
    case HB_CHAR_BEL:
      bDisp = bBell = true;
      break;

    case HB_CHAR_BS:
      if (iCol > 0) {
        --iCol;
        bDisp = true;
      } else if (iCol == 0 && iRow > 0) {
        iCol = iMaxCol;
        --iRow;
        bDisp = true;
      }
      if (bDisp) {
        if (iLen) {
          szString[iLen - 1] = ' ';
        } else {
          HB_GTSELF_SETPOS(pGT, iRow, iCol);
          szString[iLen++] = ' ';
        }
      }
      break;

    case HB_CHAR_LF:
      iCol = 0;
      if (iRow >= 0) {
        ++iRow;
      }
      bDisp = true;
      bNewLine = true;
      break;

    case HB_CHAR_CR:
      iCol = 0;
      if (nIndex < nLength && szText[nIndex] == HB_CHAR_LF) {
        if (iRow >= 0) {
          ++iRow;
        }
        bNewLine = true;
        ++nIndex;
      }
      bDisp = true;
      break;

    default:
      ++iCol;
      if (iCol > iMaxCol || iCol <= 0) {
        // If the cursor position started off the left edge,
        // don't display the first character of the string
        if (iCol > 0) {
          szString[iLen++] = wc;
        }
        // Always advance to the first column of the next row
        // when the right edge is reached or when the cursor
        // started off the left edge, unless the cursor is off
        // the top edge, in which case only change the column
        iCol = 0;
        if (iRow >= 0) {
          ++iRow;
        }
        bDisp = true;
        bNewLine = true;
      } else {
        szString[iLen++] = wc;
      }

      // Special handling for a really wide screen or device
      if (iLen >= WRITECON_BUFFER_SIZE) {
        bDisp = true;
      }
    }

    if (bDisp || nIndex == nLength) {
      if (iLen) {
        HB_GTSELF_WRITEW(pGT, szString, iLen);
      }

      iLen = 0;
      if (iRow > iMaxRow) {
        // Normal scroll
        HB_GTSELF_SCROLL(pGT, 0, 0, iMaxRow, iMaxCol, HB_GTSELF_GETCOLOR(pGT), HB_GTSELF_GETCLEARCHAR(pGT),
                         iRow - iMaxRow, 0);
        iRow = iMaxRow;
        iCol = 0;
      } else if (iRow < 0 && bNewLine) {
        // Special case scroll when newline
        // and cursor off top edge of display
        HB_GTSELF_SCROLL(pGT, 0, 0, iMaxRow, iMaxCol, HB_GTSELF_GETCOLOR(pGT), HB_GTSELF_GETCLEARCHAR(pGT), 1, 0);
      }
      HB_GTSELF_SETPOS(pGT, iRow, iCol);
      bDisp = false;
      bNewLine = false;

      // To emulate scrolling
      HB_GTSELF_FLUSH(pGT);

      if (bBell) {
        HB_GTSELF_BELL(pGT);
        bBell = false;
      }
    }
  }
}

static void hb_gt_def_WriteConW(PHB_GT pGT, const HB_WCHAR *szText, HB_SIZE nLength)
{
  int iLen = 0;
  auto bDisp = false;
  auto bBell = false;
  auto bNewLine = false;
  HB_WCHAR szString[WRITECON_BUFFER_SIZE];
  HB_SIZE nIndex = 0;

  int iMaxRow = HB_GTSELF_MAXROW(pGT);
  int iMaxCol = HB_GTSELF_MAXCOL(pGT);

  int iRow, iCol;
  HB_GTSELF_GETPOS(pGT, &iRow, &iCol);

  // Limit the starting cursor position to MaxRow(),MaxCol()
  // on the high end, but don't limit it on the low end.

  if (iRow > iMaxRow || iCol > iMaxCol) {
    if (iRow > iMaxRow) {
      iRow = iMaxRow;
    }
    if (iCol > iMaxCol) {
      iCol = iMaxCol;
    }
    HB_GTSELF_SETPOS(pGT, iRow, iCol);
  }

  while (nIndex < nLength) {
    HB_WCHAR wc = szText[nIndex++];

    switch (wc) {
    case HB_CHAR_BEL:
      bDisp = bBell = true;
      break;

    case HB_CHAR_BS:
      if (iCol > 0) {
        --iCol;
        bDisp = true;
      } else if (iCol == 0 && iRow > 0) {
        iCol = iMaxCol;
        --iRow;
        bDisp = true;
      }
      if (bDisp) {
        if (iLen) {
          szString[iLen - 1] = ' ';
        } else {
          HB_GTSELF_SETPOS(pGT, iRow, iCol);
          szString[iLen++] = ' ';
        }
      }
      break;

    case HB_CHAR_LF:
      iCol = 0;
      if (iRow >= 0) {
        ++iRow;
      }
      bDisp = true;
      bNewLine = true;
      break;

    case HB_CHAR_CR:
      iCol = 0;
      if (nIndex < nLength && szText[nIndex] == HB_CHAR_LF) {
        if (iRow >= 0) {
          ++iRow;
        }
        bNewLine = true;
        ++nIndex;
      }
      bDisp = true;
      break;

    default:
      ++iCol;
      if (iCol > iMaxCol || iCol <= 0) {
        // If the cursor position started off the left edge,
        // don't display the first character of the string
        if (iCol > 0) {
          szString[iLen++] = wc;
        }
        // Always advance to the first column of the next row
        // when the right edge is reached or when the cursor
        // started off the left edge, unless the cursor is off
        // the top edge, in which case only change the column
        iCol = 0;
        if (iRow >= 0) {
          ++iRow;
        }
        bDisp = true;
        bNewLine = true;
      } else {
        szString[iLen++] = wc;
      }

      // Special handling for a really wide screen or device
      if (iLen >= WRITECON_BUFFER_SIZE) {
        bDisp = true;
      }
    }

    if (bDisp || nIndex == nLength) {
      if (iLen) {
        HB_GTSELF_WRITEW(pGT, szString, iLen);
      }

      iLen = 0;
      if (iRow > iMaxRow) {
        // Normal scroll
        HB_GTSELF_SCROLL(pGT, 0, 0, iMaxRow, iMaxCol, HB_GTSELF_GETCOLOR(pGT), HB_GTSELF_GETCLEARCHAR(pGT),
                         iRow - iMaxRow, 0);
        iRow = iMaxRow;
        iCol = 0;
      } else if (iRow < 0 && bNewLine) {
        // Special case scroll when newline
        // and cursor off top edge of display
        HB_GTSELF_SCROLL(pGT, 0, 0, iMaxRow, iMaxCol, HB_GTSELF_GETCOLOR(pGT), HB_GTSELF_GETCLEARCHAR(pGT), 1, 0);
      }
      HB_GTSELF_SETPOS(pGT, iRow, iCol);
      bDisp = false;
      bNewLine = false;

      // To emulate scrolling
      HB_GTSELF_FLUSH(pGT);

      if (bBell) {
        HB_GTSELF_BELL(pGT);
        bBell = false;
      }
    }
  }
}

static long hb_gt_def_RectSize(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight)
{
  int iRows = iBottom - iTop + 1;
  int iCols = iRight - iLeft + 1;

  if (iCols <= 0 || iRows <= 0) {
    return 0;
  } else {
    return (static_cast<long>(iRows) * iCols) << (pGT->fVgaCell ? 1 : 2);
  }
}

static void hb_gt_def_Save(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, void *pBuffer)
{
  auto pbyBuffer = static_cast<HB_BYTE *>(pBuffer);
  PHB_CODEPAGE cdp = pGT->fVgaCell ? HB_GTSELF_HOSTCP(pGT) : nullptr;

  while (iTop <= iBottom) {
    for (int iCol = iLeft; iCol <= iRight; ++iCol) {
      int iColor;
      HB_BYTE bAttr;
      HB_USHORT usChar;

      if (!HB_GTSELF_GETCHAR(pGT, iTop, iCol, &iColor, &bAttr, &usChar)) {
        usChar = HB_GTSELF_GETCLEARCHAR(pGT);
        iColor = HB_GTSELF_GETCLEARCOLOR(pGT);
        bAttr = 0x00;
      }

      if (pGT->fVgaCell) {
        *pbyBuffer++ = hb_cdpGetChar(cdp, usChar);
        *pbyBuffer++ = static_cast<HB_BYTE>(iColor);
      } else {
        HB_PUT_LE_UINT16(pbyBuffer, usChar);
        pbyBuffer += 2;
        *pbyBuffer++ = static_cast<HB_BYTE>(iColor);
        *pbyBuffer++ = bAttr;
      }
    }
    ++iTop;
  }
}

static void hb_gt_def_Rest(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const void *pBuffer)
{
  auto pbyBuffer = static_cast<const HB_BYTE *>(pBuffer);
  PHB_CODEPAGE cdp = pGT->fVgaCell ? HB_GTSELF_HOSTCP(pGT) : nullptr;

  while (iTop <= iBottom) {
    for (int iCol = iLeft; iCol <= iRight; ++iCol) {
      int iColor;
      HB_BYTE bAttr;
      HB_USHORT usChar;

      if (pGT->fVgaCell) {
        usChar = hb_cdpGetU16(cdp, *pbyBuffer++);
        iColor = *pbyBuffer++;
        bAttr = 0;
      } else {
        usChar = HB_GET_LE_UINT16(pbyBuffer);
        pbyBuffer += 2;
        iColor = *pbyBuffer++;
        bAttr = *pbyBuffer++;
      }
      HB_GTSELF_PUTCHAR(pGT, iTop, iCol, iColor, bAttr, usChar);
    }
    ++iTop;
  }
}

static void hb_gt_def_SetAttribute(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int iColor)
{
  while (iTop <= iBottom) {
    for (int iCol = iLeft; iCol <= iRight; ++iCol) {
      int iColorOld;
      HB_BYTE bAttr;
      HB_USHORT usChar;

      if (!HB_GTSELF_GETCHAR(pGT, iTop, iCol, &iColorOld, &bAttr, &usChar)) {
        break;
      }
      if (!HB_GTSELF_PUTCHAR(pGT, iTop, iCol, iColor, bAttr, usChar)) {
        break;
      }
    }
    ++iTop;
  }
}

static void hb_gt_def_DrawShadow(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int iColor)
{
  int iMaxRow, iMaxCol, i;

  if (iTop > iBottom) {
    i = iTop;
    iTop = iBottom;
    iBottom = i;
  }
  if (iLeft > iRight) {
    i = iLeft;
    iLeft = iRight;
    iRight = i;
  }

  iLeft += 2;
  ++iBottom;

  iMaxRow = HB_GTSELF_MAXROW(pGT);
  iMaxCol = HB_GTSELF_MAXCOL(pGT);

  // Draw the bottom edge
  if (iBottom <= iMaxRow && iLeft <= iMaxCol) {
    HB_GTSELF_SETATTRIBUTE(pGT, iBottom, iLeft, iBottom, HB_MIN(iRight, iMaxCol), iColor);
  }

  ++iRight;
  ++iTop;

  // Draw the right edge
  if (iTop <= iMaxRow && iRight <= iMaxCol) {
    HB_GTSELF_SETATTRIBUTE(pGT, iTop, iRight, iBottom, HB_MIN(iRight + 1, iMaxCol), iColor);
  }
}

static void hb_gt_def_Scroll(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int iColor, HB_USHORT usChar,
                             int iRows, int iCols)
{
  int iColOld, iColNew, iColSize, iColClear, iClrs, iLength;

  iColSize = iRight - iLeft;
  iLength = iColSize + 1;
  iColOld = iColNew = iLeft;
  if (iCols >= 0) {
    iColOld += iCols;
    iColSize -= iCols;
    iColClear = iColNew + iColSize + 1;
    iClrs = iCols;
  } else {
    iColNew -= iCols;
    iColSize += iCols;
    iColClear = iColOld;
    iClrs = -iCols;
  }

  if (iLength > 0 && iTop <= iBottom) {
    void *pBuffer = nullptr;
    int iFlag = 0;

    if ((iRows || iCols) && iColSize >= 0 && (iBottom - iTop >= iRows)) {
      HB_SIZE nSize;

      iFlag = HB_GTSELF_SETFLAG(pGT, HB_GTI_COMPATBUFFER, 0);
      nSize = HB_GTSELF_RECTSIZE(pGT, iTop, iColOld, iTop, iColOld + iColSize);
      if (nSize) {
        pBuffer = hb_xgrab(nSize);
      }
    }

    while (iTop <= iBottom) {
      int iRowPos;

      if (iRows >= 0) {
        iRowPos = iTop++;
      } else {
        iRowPos = iBottom--;
      }

      if (pBuffer && (iRows == 0 || (iRowPos + iRows >= iTop && iRowPos + iRows <= iBottom))) {
        HB_GTSELF_SAVE(pGT, iRowPos + iRows, iColOld, iRowPos + iRows, iColOld + iColSize, pBuffer);
        HB_GTSELF_REST(pGT, iRowPos, iColNew, iRowPos, iColNew + iColSize, pBuffer);
        if (iClrs) {
          HB_GTSELF_REPLICATE(pGT, iRowPos, iColClear, iColor, 0, usChar, iClrs);
        }
      } else {
        HB_GTSELF_REPLICATE(pGT, iRowPos, iLeft, iColor, 0, usChar, iLength);
      }
    }

    if (pBuffer) {
      hb_xfree(pBuffer);
    }
    if (iFlag != 0) {
      HB_GTSELF_SETFLAG(pGT, HB_GTI_COMPATBUFFER, iFlag);
    }
  }
}

static void hb_gt_def_ScrollArea(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, int iColor, HB_USHORT usChar,
                                 int iRows, int iCols)
{
  if (iRows || iCols) {
    int iColNew, iColSize, iColClear, iClrs, iLength, iHeight, iWidth;

    HB_GTSELF_GETSIZE(pGT, &iHeight, &iWidth);
    if (iTop < 0) {
      iTop = 0;
    }
    if (iLeft < 0) {
      iLeft = 0;
    }
    if (iBottom >= iHeight) {
      iBottom = iHeight - 1;
    }
    if (iRight >= iWidth) {
      iRight = iWidth - 1;
    }

    iColSize = iRight - iLeft;
    iLength = iColSize + 1;
    iColNew = iLeft;

    if (iCols >= 0) {
      iColSize -= iCols;
      iColClear = iColNew + iColSize + 1;
      iClrs = iCols;
    } else {
      iColClear = iColNew;
      iColNew -= iCols;
      iColSize += iCols;
      iClrs = -iCols;
    }

    if (iLength > 0) {
      long lIndex, lOffset = static_cast<long>(iRows) * iWidth + iCols;
      bool fMove = (iRows || iCols) && iColSize >= 0 && (iBottom - iTop >= iRows);

      while (iTop <= iBottom) {
        int iRowPos;

        if (iRows >= 0) {
          iRowPos = iTop++;
        } else {
          iRowPos = iBottom--;
        }

        if (fMove && (iRows == 0 || (iRowPos + iRows >= iTop && iRowPos + iRows <= iBottom))) {
          int i;

          lIndex = static_cast<long>(iRowPos) * iWidth + iColNew;
          if (lOffset < 0) {
            for (i = 0; i <= iColSize; ++i, ++lIndex) {
              pGT->screenBuffer[lIndex].uiValue = pGT->screenBuffer[lIndex + lOffset].uiValue;
              pGT->prevBuffer[lIndex].uiValue = pGT->prevBuffer[lIndex + lOffset].uiValue;
            }
          } else {
            for (i = iColSize, lIndex += iColSize; i >= 0; --i, --lIndex) {
              pGT->screenBuffer[lIndex].uiValue = pGT->screenBuffer[lIndex + lOffset].uiValue;
              pGT->prevBuffer[lIndex].uiValue = pGT->prevBuffer[lIndex + lOffset].uiValue;
            }
          }
          if (iClrs) {
            HB_GTSELF_REPLICATE(pGT, iRowPos, iColClear, iColor, 0, usChar, iClrs);
          }
        } else {
          HB_GTSELF_REPLICATE(pGT, iRowPos, iLeft, iColor, 0, usChar, iLength);
        }
      }
    }
  }
}

static void hb_gt_def_ScrollUp(PHB_GT pGT, int iRows, int iColor, HB_USHORT usChar)
{
  if (iRows > 0) {
    int i, j, iHeight, iWidth;
    long lIndex = 0, lOffset;
    HB_BYTE bAttr = 0;

    HB_GTSELF_GETSIZE(pGT, &iHeight, &iWidth);
    lOffset = static_cast<long>(iRows) * iWidth;
    for (i = iRows; i < iHeight; ++i) {
      pGT->pLines[i - iRows] = pGT->pLines[i];
      for (j = 0; j < iWidth; ++j) {
        pGT->screenBuffer[lIndex].uiValue = pGT->screenBuffer[lIndex + lOffset].uiValue;
        pGT->prevBuffer[lIndex].uiValue = pGT->prevBuffer[lIndex + lOffset].uiValue;
        ++lIndex;
      }
    }
    for (i = HB_MAX(0, iHeight - iRows); i < iHeight; ++i) {
      for (j = 0; j < iWidth; ++j) {
        pGT->screenBuffer[lIndex].c.usChar = usChar;
        pGT->screenBuffer[lIndex].c.bColor = static_cast<HB_BYTE>(iColor);
        pGT->screenBuffer[lIndex].c.bAttr = bAttr;
        ++lIndex;
      }
      pGT->pLines[i] = true;
    }
    pGT->fRefresh = true;
  }
}

static void hb_gt_def_BoxW(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const HB_WCHAR *szFrame,
                           int iColor)
{
  int iMaxRow, iMaxCol, i;

  if (iTop > iBottom) {
    i = iTop;
    iTop = iBottom;
    iBottom = i;
  }
  if (iLeft > iRight) {
    i = iLeft;
    iLeft = iRight;
    iRight = i;
  }
  iMaxRow = HB_GTSELF_MAXROW(pGT);
  iMaxCol = HB_GTSELF_MAXCOL(pGT);

  if (iTop <= iMaxRow && iLeft <= iMaxCol && iBottom >= 0 && iRight >= 0) {
    HB_WCHAR szBoxW[10];
    auto wcPadCh = static_cast<HB_WCHAR>(HB_GTSELF_GETCLEARCHAR(pGT));

    if (szFrame != nullptr && *szFrame) {
      for (i = 0; *szFrame && i < 9; ++i) {
        wcPadCh = szBoxW[i] = *szFrame++;
      }
      while (i < 8) {
        szBoxW[i++] = wcPadCh;
      }
    } else {
      for (i = 0; i < 9; ++i) {
        szBoxW[i] = ' ';
      }
    }

    szBoxW[i] = '\0';

    if (iTop == iBottom) {
      HB_GTSELF_HORIZLINE(pGT, iTop, iLeft, iRight, szBoxW[1], iColor);
    } else if (iLeft == iRight) {
      HB_GTSELF_VERTLINE(pGT, iLeft, iTop, iBottom, szBoxW[3], iColor);
    } else {
      HB_BYTE bAttr = HB_GT_ATTR_BOX;

      int iRows = (iBottom > iMaxRow ? iMaxRow + 1 : iBottom) - (iTop < 0 ? -1 : iTop) - 1;
      int iCols = (iRight > iMaxCol ? iMaxCol + 1 : iRight) - (iLeft < 0 ? -1 : iLeft) - 1;
      int iFirst = iLeft < 0 ? 0 : iLeft + 1;

      if (iTop >= 0) {
        if (iLeft >= 0) {
          HB_GTSELF_PUTCHAR(pGT, iTop, iLeft, iColor, bAttr, szBoxW[0]);
        }
        if (iCols) {
          HB_GTSELF_REPLICATE(pGT, iTop, iFirst, iColor, bAttr, szBoxW[1], iCols);
        }
        if (iRight <= iMaxCol) {
          HB_GTSELF_PUTCHAR(pGT, iTop, iFirst + iCols, iColor, bAttr, szBoxW[2]);
        }
        iTop++;
      } else {
        iTop = 0;
      }
      for (i = 0; i < iRows; ++i) {
        if (iLeft >= 0) {
          HB_GTSELF_PUTCHAR(pGT, iTop + i, iLeft, iColor, bAttr, szBoxW[7]);
        }
        if (iCols && szBoxW[8]) {
          HB_GTSELF_REPLICATE(pGT, iTop + i, iFirst, iColor, bAttr, szBoxW[8], iCols);
        }
        if (iRight <= iMaxCol) {
          HB_GTSELF_PUTCHAR(pGT, iTop + i, iFirst + iCols, iColor, bAttr, szBoxW[3]);
        }
      }
      if (iBottom <= iMaxRow) {
        if (iLeft >= 0) {
          HB_GTSELF_PUTCHAR(pGT, iBottom, iLeft, iColor, bAttr, szBoxW[6]);
        }
        if (iCols) {
          HB_GTSELF_REPLICATE(pGT, iBottom, iFirst, iColor, bAttr, szBoxW[5], iCols);
        }
        if (iRight <= iMaxCol) {
          HB_GTSELF_PUTCHAR(pGT, iBottom, iFirst + iCols, iColor, bAttr, szBoxW[4]);
        }
      }
    }
  }
}

static void hb_gt_def_Box(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const char *szFrame, int iColor)
{
  if (szFrame != nullptr) {
    PHB_CODEPAGE cdp = HB_GTSELF_BOXCP(pGT);
    HB_WCHAR szFrameW[10], wc;
    HB_SIZE nLen = strlen(szFrame), nIndex = 0, nPos = 0;

    while (nPos < 9 && HB_CDPCHAR_GET(cdp, szFrame, nLen, &nIndex, &wc)) {
      szFrameW[nPos++] = wc;
    }

    szFrameW[nPos] = 0;

    HB_GTSELF_BOXW(pGT, iTop, iLeft, iBottom, iRight, szFrameW, iColor);
  } else {
    HB_GTSELF_BOXW(pGT, iTop, iLeft, iBottom, iRight, nullptr, iColor);
  }
}

static void hb_gt_def_BoxS(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const char *szFrame, int iColor)
{
  static const HB_WCHAR s_szFrameW[] = HB_B_SINGLE_W;

  if (szFrame != nullptr) {
    HB_GTSELF_BOX(pGT, iTop, iLeft, iBottom, iRight, szFrame, iColor);
  } else {
    HB_GTSELF_BOXW(pGT, iTop, iLeft, iBottom, iRight, s_szFrameW, iColor);
  }
}

static void hb_gt_def_BoxD(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight, const char *szFrame, int iColor)
{
  static const HB_WCHAR s_szFrameW[] = HB_B_DOUBLE_W;

  if (szFrame != nullptr) {
    HB_GTSELF_BOX(pGT, iTop, iLeft, iBottom, iRight, szFrame, iColor);
  } else {
    HB_GTSELF_BOXW(pGT, iTop, iLeft, iBottom, iRight, s_szFrameW, iColor);
  }
}

static void hb_gt_def_HorizLine(PHB_GT pGT, int iRow, int iLeft, int iRight, HB_USHORT usChar, int iColor)
{
  int iLength, iCol;

  if (iLeft <= iRight) {
    iLength = iRight - iLeft + 1;
    iCol = iLeft;
  } else {
    iLength = iLeft - iRight + 1;
    iCol = iRight;
  }

  HB_GTSELF_REPLICATE(pGT, iRow, iCol, iColor, HB_GT_ATTR_BOX, usChar, iLength);
}

static void hb_gt_def_VertLine(PHB_GT pGT, int iCol, int iTop, int iBottom, HB_USHORT usChar, int iColor)
{
  int iLength, iRow;

  if (iTop <= iBottom) {
    iLength = iBottom - iTop + 1;
    iRow = iTop;
  } else {
    iLength = iTop - iBottom + 1;
    iRow = iBottom;
  }

  if (iRow < 0) {
    iLength += iRow;
    iRow = 0;
  }

  while (--iLength >= 0) {
    if (!HB_GTSELF_PUTCHAR(pGT, iRow, iCol, iColor, HB_GT_ATTR_BOX, usChar)) {
      break;
    }
    ++iRow;
  }
}

static HB_BOOL hb_gt_def_SetDispCP(PHB_GT pGT, const char *pszTermCDP, const char *pszHostCDP, HB_BOOL fBox)
{
  if (!pszHostCDP) {
    pszHostCDP = hb_cdpID();
  }
  if (!pszTermCDP) {
    pszTermCDP = pszHostCDP;
  }

  if (pszTermCDP && pszHostCDP) {
    pGT->cdpTerm = hb_cdpFindExt(pszTermCDP);
    pGT->cdpHost = hb_cdpFindExt(pszHostCDP);
    pGT->cdpBox = fBox ? pGT->cdpHost : hb_cdpFind("EN");
    pGT->fDispTrans = pGT->cdpTerm && pGT->cdpHost && pGT->cdpTerm != pGT->cdpHost;
    return true;
  }

  return false;
}

static HB_BOOL hb_gt_def_SetKeyCP(PHB_GT pGT, const char *pszTermCDP, const char *pszHostCDP)
{
  if (!pszHostCDP) {
    pszHostCDP = hb_cdpID();
  }
  if (!pszTermCDP) {
    pszTermCDP = pszHostCDP;
  }

  if (pszTermCDP) {
    pGT->cdpIn = hb_cdpFindExt(pszTermCDP);
    return true;
  }

  return false;
}

static void hb_gt_def_SetBlock(PHB_ITEM *pItemPtr, PHB_GT_INFO pInfo)
{
  if (*pItemPtr) {
    if (pInfo->pResult) {
      hb_itemCopy(pInfo->pResult, *pItemPtr);
    } else {
      pInfo->pResult = hb_itemNew(*pItemPtr);
    }
  }
  if (pInfo->pNewVal) {
    if (*pItemPtr) {
      hb_itemRelease(*pItemPtr);
      *pItemPtr = nullptr;
    }
    if (pInfo->pNewVal->isEvalItem()) {
      *pItemPtr = hb_itemNew(pInfo->pNewVal);
      hb_gcUnlock(*pItemPtr);
    }
  }
}

static HB_BOOL hb_gt_def_Info(PHB_GT pGT, int iType, PHB_GT_INFO pInfo)
{
  switch (iType) {
  case HB_GTI_ALTENTER:
  case HB_GTI_ISFULLSCREEN:
  case HB_GTI_ISGRAPHIC:
  case HB_GTI_ISSCREENPOS:
  case HB_GTI_KBDSUPPORT:
  case HB_GTI_ISCTWIN:
  case HB_GTI_ISMULTIWIN:
  case HB_GTI_ISUNICODE:
    pInfo->pResult = hb_itemPutL(pInfo->pResult, false);
    break;

  case HB_GTI_KBDSHIFTS:
    pInfo->pResult = hb_itemPutNI(pInfo->pResult, 0);
    break;

  case HB_GTI_INPUTFD:
    pInfo->pResult = hb_itemPutNInt(pInfo->pResult, static_cast<HB_NHANDLE>(pGT->hStdIn));
    break;

  case HB_GTI_OUTPUTFD:
    pInfo->pResult = hb_itemPutNInt(pInfo->pResult, static_cast<HB_NHANDLE>(pGT->hStdOut));
    break;

  case HB_GTI_ERRORFD:
    pInfo->pResult = hb_itemPutNInt(pInfo->pResult, static_cast<HB_NHANDLE>(pGT->hStdErr));
    break;

  case HB_GTI_COMPATBUFFER:
    pInfo->pResult = hb_itemPutL(pInfo->pResult, pGT->fVgaCell);
    if (hb_itemType(pInfo->pNewVal) & Harbour::Item::LOGICAL) {
      pGT->fVgaCell = pInfo->pNewVal->getL();
    }
    break;

  case HB_GTI_REDRAWMAX:
    pInfo->pResult = hb_itemPutNI(pInfo->pResult, pGT->iRedrawMax);
    if (hb_itemType(pInfo->pNewVal) & Harbour::Item::NUMERIC) {
      pGT->iRedrawMax = pInfo->pNewVal->getNI();
    }
    break;

  case HB_GTI_BOXCP:
    pInfo->pResult = hb_itemPutC(pInfo->pResult, pGT->cdpBox ? pGT->cdpBox->id : nullptr);
    if (hb_itemType(pInfo->pNewVal) & Harbour::Item::STRING) {
      if (pInfo->pNewVal->getCLen() > 0) {
        PHB_CODEPAGE cdpBox = hb_cdpFind(pInfo->pNewVal->getCPtr());
        if (cdpBox) {
          pGT->cdpBox = cdpBox;
        }
      } else {
        pGT->cdpBox = nullptr;
      }
    }
    break;

  case HB_GTI_VIEWMAXWIDTH:
    pInfo->pResult = hb_itemPutNInt(pInfo->pResult, HB_GTSELF_MAXCOL(pGT));
    break;

  case HB_GTI_VIEWMAXHEIGHT:
    pInfo->pResult = hb_itemPutNInt(pInfo->pResult, HB_GTSELF_MAXROW(pGT));
    break;

  case HB_GTI_NEWWIN: // clear screen area, set default cursor shape and position

    // Clear screen
    HB_GTSELF_DISPBEGIN(pGT);
    HB_GTSELF_SCROLL(pGT, 0, 0, HB_GTSELF_MAXROW(pGT), HB_GTSELF_MAXCOL(pGT), HB_GTSELF_GETCOLOR(pGT),
                     HB_GTSELF_GETCLEARCHAR(pGT), 0, 0);
    HB_GTSELF_SETPOS(pGT, 0, 0);
    HB_GTSELF_SETCURSORSTYLE(pGT, SC_NORMAL);
    HB_GTSELF_DISPEND(pGT);
    HB_GTSELF_FLUSH(pGT);
    // fallthrough

  case HB_GTI_GETWIN: { // save screen buffer, cursor shape and position
    int iRow, iCol, iFlag;
    HB_SIZE nSize;

    if (!pInfo->pResult) {
      pInfo->pResult = hb_itemNew(nullptr);
    }
    hb_arrayNew(pInfo->pResult, 7);
    // 7th item is allocated for GTCTW window number
    HB_GTSELF_GETPOS(pGT, &iRow, &iCol);
    hb_arraySetNI(pInfo->pResult, 1, iRow);
    hb_arraySetNI(pInfo->pResult, 2, iCol);
    hb_arraySetNI(pInfo->pResult, 3, HB_GTSELF_GETCURSORSTYLE(pGT));

    iRow = HB_GTSELF_MAXROW(pGT);
    iCol = HB_GTSELF_MAXCOL(pGT);
    hb_arraySetNI(pInfo->pResult, 4, iRow);
    hb_arraySetNI(pInfo->pResult, 5, iCol);

    iFlag = HB_GTSELF_SETFLAG(pGT, HB_GTI_COMPATBUFFER, 0);
    nSize = HB_GTSELF_RECTSIZE(pGT, 0, 0, iRow, iCol);
    if (nSize) {
      auto pBuffer = hb_xgrab(nSize + 1);
      HB_GTSELF_SAVE(pGT, 0, 0, iRow, iCol, pBuffer);
      hb_arraySetCLPtr(pInfo->pResult, 6, static_cast<char *>(pBuffer), nSize);
    }
    if (iFlag != 0) {
      HB_GTSELF_SETFLAG(pGT, HB_GTI_COMPATBUFFER, iFlag);
    }
    break;
  }
  case HB_GTI_SETWIN: // restore screen buffer, cursor shape and position
    if ((hb_itemType(pInfo->pNewVal) & Harbour::Item::ARRAY) && hb_arrayLen(pInfo->pNewVal) == 7) {
      HB_GTSELF_DISPBEGIN(pGT);
      if (hb_arrayGetCLen(pInfo->pNewVal, 6) > 0) {
        int iFlag = HB_GTSELF_SETFLAG(pGT, HB_GTI_COMPATBUFFER, 0);
        HB_GTSELF_REST(pGT, 0, 0, hb_arrayGetNI(pInfo->pNewVal, 4), hb_arrayGetNI(pInfo->pNewVal, 5),
                       hb_arrayGetCPtr(pInfo->pNewVal, 6));
        HB_GTSELF_SETFLAG(pGT, HB_GTI_COMPATBUFFER, iFlag);
      }
      HB_GTSELF_SETPOS(pGT, hb_arrayGetNI(pInfo->pNewVal, 1), hb_arrayGetNI(pInfo->pNewVal, 2));
      HB_GTSELF_SETCURSORSTYLE(pGT, hb_arrayGetNI(pInfo->pNewVal, 3));
      HB_GTSELF_DISPEND(pGT);
      HB_GTSELF_FLUSH(pGT);
    }
    break;

  case HB_GTI_CLIPBOARDDATA:
    if (hb_itemType(pInfo->pNewVal) & Harbour::Item::STRING) {
      // set new Clipboard value
      hb_gt_setClipboard(pInfo->pNewVal->getCPtr(), pInfo->pNewVal->getCLen());
    } else {
      // get Clipboard value
      char *pszClipData;
      HB_SIZE nLen;

      if (hb_gt_getClipboard(&pszClipData, &nLen)) {
        pInfo->pResult = hb_itemPutCLPtr(pInfo->pResult, pszClipData, nLen);
      } else {
        pInfo->pResult = hb_itemPutC(pInfo->pResult, nullptr);
      }
    }
    break;

  case HB_GTI_CLIPBOARDPASTE:
    if (HB_GTSELF_INFO(pGT, HB_GTI_CLIPBOARDDATA, pInfo)) {
      HB_GTSELF_INKEYSETTEXT(pGT, hb_itemGetCPtr(pInfo->pResult), hb_itemGetCLen(pInfo->pResult),
                             hb_itemGetL(pInfo->pNewVal));
    }
    break;

  case HB_GTI_NOTIFIERBLOCK:
    hb_gt_def_SetBlock(&pGT->pNotifierBlock, pInfo);
    break;

  case HB_GTI_INKEYFILTER:
    hb_gt_def_SetBlock(&pGT->pInkeyFilterBlock, pInfo);
    break;

  case HB_GTI_INKEYREAD:
    hb_gt_def_SetBlock(&pGT->pInkeyReadBlock, pInfo);
    break;

  case HB_GTI_CARGO:
    if (pGT->pCargo) {
      if (pInfo->pResult) {
        hb_itemCopy(pInfo->pResult, pGT->pCargo);
      } else {
        pInfo->pResult = hb_itemNew(pGT->pCargo);
      }
    }
    if (pInfo->pNewVal) {
      if (pGT->pCargo) {
        hb_itemRelease(pGT->pCargo);
        pGT->pCargo = nullptr;
      }
      pGT->pCargo = hb_itemNew(pInfo->pNewVal);
      hb_gcUnlock(pGT->pCargo);
    }
    break;

  case HB_GTI_RESIZEMODE:
    pInfo->pResult = hb_itemPutNInt(pInfo->pResult, HB_GTI_RESIZEMODE_FONT);
    break;

  case HB_GTI_VERSION:
    pInfo->pResult = hb_itemPutC(pInfo->pResult, HB_GTSELF_VERSION(pGT, hb_itemGetNI(pInfo->pNewVal)));
    break;

  default:
    return false;
  }

  return true;
}

static int hb_gt_def_Alert(PHB_GT pGT, PHB_ITEM pMessage, PHB_ITEM pOptions, int iClrNorm, int iClrHigh, double dDelay)
{
  int iRet = 0, iOptions;

  if (pMessage && pMessage->isHash()) {
    if (!pOptions || hb_arrayLen(pOptions) == 0) {
      pOptions = hb_hashGetCItemPtr(pMessage, "BTN");
    }
    if (dDelay <= 0) {
      auto pVal = hb_hashGetCItemPtr(pMessage, "TIM");
      if (pVal && pVal->isNumeric()) {
        dDelay = pVal->getND();
      }
    }
    pMessage = hb_hashGetCItemPtr(pMessage, "TXT");
  }

  if (pMessage && pMessage->isString() && pOptions && (iOptions = static_cast<int>(hb_arrayLen(pOptions))) > 0) {
    HB_SIZE nLen;
    void *hMessage;
    const HB_WCHAR *szMessageW = hb_itemGetStrU16(pMessage, HB_CDP_ENDIAN_NATIVE, &hMessage, &nLen);
    auto fScreen = false;
    auto fKeyBoard = false;
    PHB_CODEPAGE cdp = HB_GTSELF_HOSTCP(pGT);
    char szKey[HB_MAX_CHAR_LEN];
    HB_SIZE nChar;
    int iKey, i, iRows, iCols;
    HB_GT_INFO gtInfo;

    memset(&gtInfo, 0, sizeof(gtInfo));

    HB_GTSELF_INFO(pGT, HB_GTI_ISSCREENPOS, &gtInfo);
    if (gtInfo.pResult) {
      fScreen = gtInfo.pResult->getL();
    }
    HB_GTSELF_INFO(pGT, HB_GTI_KBDSUPPORT, &gtInfo);
    if (gtInfo.pResult) {
      fKeyBoard = gtInfo.pResult->getL();
      hb_itemRelease(gtInfo.pResult);
    }
    HB_GTSELF_GETSIZE(pGT, &iRows, &iCols);
    if (iCols <= 4 || iRows <= 4) {
      fScreen = false;
    }

    if (fScreen) {
      void *pBuffer = nullptr;
      int iDspCount, iStyle, iRow, iCol, iTop, iLeft, iBottom, iRight, iPos;
      HB_UINT ulLines = 0, ulWidth = 0, ulCurrWidth = 0, ulMsg = 0, ulDst = 0, ulLast = 0, ulSpace1 = 0, ulSpace2 = 0,
              ulDefWidth, ulMaxWidth;
      int iFlag;

      ulMaxWidth = iCols - 4;
      ulDefWidth = (ulMaxWidth * 3) >> 2;
      if (ulDefWidth == 0) {
        ulDefWidth = 1;
      }
      auto szMsgDsp = static_cast<HB_WCHAR *>(hb_xgrab((nLen + (nLen / ulDefWidth) + 1) * sizeof(HB_WCHAR)));

      while (ulMsg < nLen) {
        if (szMessageW[ulMsg] == '\n') {
          if (ulCurrWidth > ulMaxWidth) {
            ulDst = ulLast;
          } else {
            ++ulLines;
            if (ulCurrWidth > ulWidth) {
              ulWidth = ulCurrWidth;
            }
            ulCurrWidth = ulSpace1 = ulSpace2 = 0;
            szMsgDsp[ulDst++] = '\n';
            ulLast = ulDst;
          }
        } else {
          if (szMessageW[ulMsg] == ' ') {
            if (ulCurrWidth <= ulDefWidth) {
              ulSpace1 = ulMsg;
            } else if (ulCurrWidth <= ulMaxWidth && !ulSpace2) {
              ulSpace2 = ulMsg;
            }
          }
          szMsgDsp[ulDst++] = szMessageW[ulMsg];
          ++ulCurrWidth;
          if (ulCurrWidth > ulDefWidth && ulSpace1) {
            ulCurrWidth -= ulMsg - ulSpace1 + 1;
            ulDst -= ulMsg - ulSpace1 + 1;
            ulMsg = ulSpace1;
            ++ulLines;
            if (ulCurrWidth > ulWidth) {
              ulWidth = ulCurrWidth;
            }
            ulCurrWidth = ulSpace1 = ulSpace2 = 0;
            szMsgDsp[ulDst++] = '\n';
            ulLast = ulDst;
          } else if (ulCurrWidth > ulMaxWidth) {
            if (ulSpace2) {
              ulCurrWidth -= ulMsg - ulSpace2 + 1;
              ulDst -= ulMsg - ulSpace2 + 1;
              ulMsg = ulSpace2;
              ++ulLines;
              if (ulCurrWidth > ulWidth) {
                ulWidth = ulCurrWidth;
              }
              ulCurrWidth = ulSpace1 = ulSpace2 = 0;
              szMsgDsp[ulDst++] = '\n';
              ulLast = ulDst;
            }
#ifndef HB_CLP_STRICT
            else {
              ulCurrWidth--;
              ulDst--;
              ulMsg--;
              szMsgDsp[ulDst++] = '\n';
              ulLast = ulDst;
              ++ulLines;
              if (ulCurrWidth > ulWidth) {
                ulWidth = ulCurrWidth;
              }
              ulCurrWidth = ulSpace1 = ulSpace2 = 0;
            }
#endif
          }
        }
        ++ulMsg;
      }
      ulLines++;
      if (ulCurrWidth > ulWidth) {
        ulWidth = ulCurrWidth;
      }
      if (ulLines == 1 && ulWidth < ulDefWidth) {
        ulWidth += HB_MIN(4, ulDefWidth - ulWidth);
      }

      ulCurrWidth = 0;
      for (i = 1; i <= iOptions; ++i) {
        nLen = hb_itemCopyStrU16(hb_arrayGetItemPtr(pOptions, i), HB_CDP_ENDIAN_NATIVE, nullptr, 0);
        ulCurrWidth += static_cast<HB_UINT>(nLen) + 4;
      }
      if (ulCurrWidth > ulMaxWidth) {
        ulCurrWidth = ulMaxWidth;
      }
      if (ulCurrWidth > ulWidth) {
        ulWidth = ulCurrWidth;
      }
      if (static_cast<HB_SIZE>(iRows) < ulLines + 4) {
        ulLines = iRows - 4;
      }
      iTop = (iRows - ulLines - 4) >> 1;
      iLeft = (iCols - ulWidth - 4) >> 1;
      iBottom = iTop + ulLines + 3;
      iRight = iLeft + ulWidth + 3;

      if (iClrNorm == 0) {
        iClrNorm = 0x4F;
      }
      if (iClrHigh == 0) {
        iClrHigh = 0x1F;
      }
      iDspCount = HB_GTSELF_DISPCOUNT(pGT);
      if (iDspCount == 0) {
        HB_GTSELF_DISPBEGIN(pGT);
      }
      HB_GTSELF_GETPOS(pGT, &iRow, &iCol);
      iStyle = HB_GTSELF_GETCURSORSTYLE(pGT);
      HB_GTSELF_SETCURSORSTYLE(pGT, SC_NONE);
      iFlag = HB_GTSELF_SETFLAG(pGT, HB_GTI_COMPATBUFFER, 0);
      nLen = HB_GTSELF_RECTSIZE(pGT, iTop, iLeft, iBottom, iRight);
      if (nLen) {
        pBuffer = hb_xgrab(nLen);
        HB_GTSELF_SAVE(pGT, iTop, iLeft, iBottom, iRight, pBuffer);
      }
      HB_GTSELF_BOXS(pGT, iTop, iLeft, iBottom, iRight, nullptr, iClrNorm);
      HB_GTSELF_BOX(pGT, iTop + 1, iLeft + 1, iBottom - 1, iRight - 1, nullptr, iClrNorm);
      ulLast = 0;
      i = iTop + 1;
      for (ulMsg = 0; ulMsg < ulDst; ++ulMsg) {
        if (szMsgDsp[ulMsg] == '\n') {
          if (ulMsg > ulLast) {
            nLen = ulMsg - ulLast;
            if (nLen > ulWidth) {
              nLen = ulWidth;
            }
            HB_GTSELF_PUTTEXTW(pGT, i, iLeft + static_cast<int>((ulWidth - nLen + 1) >> 1) + 2, iClrNorm,
                               szMsgDsp + ulLast, nLen);
          }
          ulLast = ulMsg + 1;
          if (++i >= iBottom - 1) {
            break;
          }
        }
      }
      if (ulMsg > ulLast && i < iBottom - 1) {
        nLen = ulMsg - ulLast;
        if (nLen > ulWidth) {
          nLen = ulWidth;
        }
        HB_GTSELF_PUTTEXTW(pGT, i, iLeft + static_cast<int>((ulWidth - nLen + 1) >> 1) + 2, iClrNorm, szMsgDsp + ulLast,
                           nLen);
      }
      hb_xfree(szMsgDsp);

      iPos = 1;
      while (iRet == 0) {
        int iMnuCol;
        HB_GTSELF_DISPBEGIN(pGT);
        iMnuCol = iLeft + ((ulWidth - ulCurrWidth) >> 1) + 3;
        for (i = 1; i <= iOptions; ++i) {
          void *hOpt;
          const HB_WCHAR *szOptW;
          int iClr = i == iPos ? iClrHigh : iClrNorm;
          szOptW = hb_arrayGetStrU16(pOptions, i, HB_CDP_ENDIAN_NATIVE, &hOpt, &nLen);
          HB_GTSELF_PUTTEXTW(pGT, iBottom - 1, iMnuCol, iClr, s_szSpaceW, 1);
          HB_GTSELF_PUTTEXTW(pGT, iBottom - 1, iMnuCol + 1, iClr, szOptW, nLen);
          HB_GTSELF_PUTTEXTW(pGT, iBottom - 1, iMnuCol + 1 + static_cast<int>(nLen), iClr, s_szSpaceW, 1);
          hb_strfree(hOpt);
          iMnuCol += static_cast<int>(nLen) + 4;
        }
        while (HB_GTSELF_DISPCOUNT(pGT)) {
          HB_GTSELF_DISPEND(pGT);
        }
        HB_GTSELF_REFRESH(pGT);

        iKey = fKeyBoard ? HB_GTSELF_INKEYGET(pGT, true, dDelay, INKEY_ALL) : 0;

        if (iKey == K_ESC) {
          break;
        } else if (iKey == K_ENTER || iKey == K_SPACE || iKey == 0) {
          iRet = iPos;
        } else if (iKey == K_LEFT || iKey == K_SH_TAB) {
          if (--iPos == 0) {
            iPos = iOptions;
          }
          dDelay = 0.0;
        } else if (iKey == K_RIGHT || iKey == K_TAB) {
          if (++iPos > iOptions) {
            iPos = 1;
          }
          dDelay = 0.0;
        }
#ifdef HB_COMPAT_C53
        else if (iKey == K_LBUTTONDOWN) {
          int iMRow, iMCol;
          HB_GTSELF_MOUSEGETPOS(pGT, &iMRow, &iMCol);
          if (iMRow == iBottom - 1) {
            iMnuCol = iLeft + ((ulWidth - ulCurrWidth) >> 1) + 4;
            for (i = 1; i <= iOptions; ++i) {
              nLen = hb_itemCopyStrU16(hb_arrayGetItemPtr(pOptions, i), HB_CDP_ENDIAN_NATIVE, nullptr, 0);
              if (iMCol >= iMnuCol && iMCol < iMnuCol + static_cast<int>(nLen)) {
                iRet = i;
                break;
              }
              iMnuCol += static_cast<int>(nLen) + 4;
            }
          }
        }
#endif
        else if ((nChar = hb_inkeyKeyString(iKey, szKey, sizeof(szKey))) > 0) {
          for (i = 1; i <= iOptions; ++i) {
            nLen = hb_arrayGetCLen(pOptions, i);
            if (nLen > 0) {
              HB_SIZE nIdx1 = 0, nIdx2 = 0;
              if (hb_cdpCharCaseEq(cdp, szKey, nChar, &nIdx1, hb_arrayGetCPtr(pOptions, i), nLen, &nIdx2)) {
                iRet = i;
                break;
              }
            }
          }
        }
      }

      if (pBuffer) {
        HB_GTSELF_REST(pGT, iTop, iLeft, iBottom, iRight, pBuffer);
        hb_xfree(pBuffer);
      }
      if (iFlag != 0) {
        HB_GTSELF_SETFLAG(pGT, HB_GTI_COMPATBUFFER, iFlag);
      }
      HB_GTSELF_SETPOS(pGT, iRow, iCol);
      HB_GTSELF_SETCURSORSTYLE(pGT, iStyle);
      HB_GTSELF_REFRESH(pGT);
      while (HB_GTSELF_DISPCOUNT(pGT) < iDspCount) {
        HB_GTSELF_DISPBEGIN(pGT);
      }
    } else {
      HB_SIZE nMsg, nStart = 0;
      const char *szEol = hb_conNewLine();

      for (nMsg = 0; nMsg < nLen; ++nMsg) {
        if (szMessageW[nMsg] == '\n') {
          if (nMsg > nStart) {
            HB_GTSELF_WRITECONW(pGT, szMessageW + nStart, nMsg - nStart);
          }
          HB_GTSELF_WRITECON(pGT, szEol, strlen(szEol));
          nStart = nMsg + 1;
        }
      }
      if (nMsg > nStart) {
        HB_GTSELF_WRITECONW(pGT, szMessageW + nStart, nMsg - nStart);
      }
      HB_GTSELF_WRITECON(pGT, " (", 2);
      for (i = 1; i <= iOptions; ++i) {
        if (i > 1) {
          HB_GTSELF_WRITECON(pGT, ", ", 2);
        }
        HB_GTSELF_WRITECON(pGT, hb_arrayGetCPtr(pOptions, i), hb_arrayGetCLen(pOptions, i));
      }
      HB_GTSELF_WRITECON(pGT, ") ", 2);
      nChar = 0;
      while (iRet == 0) {
        iKey = fKeyBoard ? HB_GTSELF_INKEYGET(pGT, true, dDelay, INKEY_ALL) : 0;
        if (iKey == 0) {
          iRet = 1;
        } else if (iKey == K_ESC) {
          break;
        } else if ((nChar = hb_inkeyKeyString(iKey, szKey, sizeof(szKey))) > 0) {
          for (i = 1; i <= iOptions; ++i) {
            nLen = hb_arrayGetCLen(pOptions, i);
            if (nLen > 0) {
              HB_SIZE nIdx1 = 0, nIdx2 = 0;
              if (hb_cdpCharCaseEq(cdp, szKey, nChar, &nIdx1, hb_arrayGetCPtr(pOptions, i), nLen, &nIdx2)) {
                iRet = i;
                break;
              }
            }
          }
        }
      }
      if (iRet > 0 && nChar > 0) {
        HB_GTSELF_WRITECON(pGT, szKey, nChar);
      }
    }
    hb_strfree(hMessage);
  }

  return iRet;
}

static int hb_gt_def_SetFlag(PHB_GT pGT, int iType, int iNewValue)
{
  int iPrevValue = 0;

  switch (iType) {
  case HB_GTI_COMPATBUFFER:
    iPrevValue = pGT->fVgaCell;
    pGT->fVgaCell = iNewValue != 0;
    break;

  case HB_GTI_STDOUTCON:
    iPrevValue = pGT->fStdOutCon;
    pGT->fStdOutCon = iNewValue != 0;
    break;

  case HB_GTI_STDERRCON:
    iPrevValue = pGT->fStdErrCon;
    pGT->fStdErrCon = iNewValue != 0;
    break;

  case HB_GTI_REDRAWMAX:
    iPrevValue = pGT->iRedrawMax;
    pGT->iRedrawMax = iNewValue;
    break;
  }

  return iPrevValue;
}

static HB_BOOL hb_gt_def_SetMode(PHB_GT pGT, int iRows, int iCols)
{
  return HB_GTSELF_RESIZE(pGT, iRows, iCols);
}

static HB_BOOL hb_gt_def_Resize(PHB_GT pGT, int iRows, int iCols)
{
  if (iRows > 0 && iCols > 0 && pGT->screenBuffer) {
    if (pGT->iHeight != iRows || pGT->iWidth != iCols) {
      void *pBuffer = nullptr;
      HB_SIZE nLen = static_cast<HB_SIZE>(iRows) * iCols, nIndex;
      HB_SIZE nSize;
      int iFlag, i;

      iFlag = HB_GTSELF_SETFLAG(pGT, HB_GTI_COMPATBUFFER, 0);
      nSize = HB_GTSELF_RECTSIZE(pGT, 0, 0, iRows - 1, iCols - 1);
      if (nSize) {
        pBuffer = hb_xgrab(nSize);
        HB_GTSELF_SAVE(pGT, 0, 0, iRows - 1, iCols - 1, pBuffer);
      }

      pGT->screenBuffer = static_cast<PHB_SCREENCELL>(hb_xrealloc(pGT->screenBuffer, sizeof(HB_SCREENCELL) * nLen));
      pGT->prevBuffer = static_cast<PHB_SCREENCELL>(hb_xrealloc(pGT->prevBuffer, sizeof(HB_SCREENCELL) * nLen));
      pGT->pLines = static_cast<HB_BOOL *>(hb_xrealloc(pGT->pLines, sizeof(HB_BOOL) * iRows));

      memset(pGT->screenBuffer, 0, sizeof(HB_SCREENCELL) * nLen);
      memset(pGT->prevBuffer, 0, sizeof(HB_SCREENCELL) * nLen);
      for (i = 0; i < iRows; ++i) {
        pGT->pLines[i] = true;
      }
      for (nIndex = 0; nIndex < nLen; ++nIndex) {
        pGT->screenBuffer[nIndex].c.usChar = HB_GTSELF_GETCLEARCHAR(pGT);
        pGT->screenBuffer[nIndex].c.bColor = static_cast<HB_BYTE>(HB_GTSELF_GETCLEARCOLOR(pGT));
        pGT->screenBuffer[nIndex].c.bAttr = 0x00;
        pGT->prevBuffer[nIndex].c.bAttr = HB_GT_ATTR_REFRESH;
      }

      pGT->iHeight = iRows;
      pGT->iWidth = iCols;

      if (pGT->iRow >= pGT->iHeight) {
        pGT->iRow = pGT->iHeight - 1;
      }
      if (pGT->iCol >= pGT->iWidth) {
        pGT->iCol = pGT->iWidth - 1;
      }

      pGT->fRefresh = true;

      if (nSize) {
        HB_GTSELF_REST(pGT, 0, 0, iRows - 1, iCols - 1, pBuffer);
        hb_xfree(pBuffer);
      }
      if (iFlag != 0) {
        HB_GTSELF_SETFLAG(pGT, HB_GTI_COMPATBUFFER, iFlag);
      }
    }

    return true;
  }

  return false;
}

static void hb_gt_def_GetSize(PHB_GT pGT, int *piRows, int *piCols)
{
  *piRows = pGT->iHeight;
  *piCols = pGT->iWidth;
}

static void hb_gt_def_SemiCold(PHB_GT pGT)
{
  for (auto i = 0; i < pGT->iHeight; ++i) {
    pGT->pLines[i] = false;
  }
  pGT->fRefresh = false;
}

static void hb_gt_def_ColdArea(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight)
{
  long lIndex;
  int i;

  if (iTop > iBottom) {
    i = iTop;
    iTop = iBottom;
    iBottom = i;
  }
  if (iLeft > iRight) {
    i = iLeft;
    iLeft = iRight;
    iRight = i;
  }
  while (iTop <= iBottom) {
    for (i = iLeft; i <= iRight; ++i) {
      if (HB_GTSELF_CHECKPOS(pGT, iTop, i, &lIndex)) {
        pGT->screenBuffer[lIndex].c.bAttr &= ~HB_GT_ATTR_REFRESH;
        pGT->prevBuffer[lIndex].uiValue = pGT->screenBuffer[lIndex].uiValue;
      }
    }
    if (iLeft == 0 && iRight == pGT->iWidth - 1) {
      pGT->pLines[iTop] = false;
    }
    ++iTop;
  }
}

static void hb_gt_def_ExposeArea(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight)
{
  long lIndex;
  int i;

  if (iTop > iBottom) {
    i = iTop;
    iTop = iBottom;
    iBottom = i;
  }
  if (iLeft > iRight) {
    i = iLeft;
    iLeft = iRight;
    iRight = i;
  }
  while (iTop <= iBottom) {
    for (i = iLeft; i <= iRight; ++i) {
      if (HB_GTSELF_CHECKPOS(pGT, iTop, i, &lIndex)) {
        pGT->prevBuffer[lIndex].c.bAttr = HB_GT_ATTR_REFRESH;
        pGT->pLines[iTop] = true;
        pGT->fRefresh = true;
      }
    }
    ++iTop;
  }
}

static void hb_gt_def_TouchLine(PHB_GT pGT, int iRow)
{
  if (iRow >= 0 && iRow < pGT->iHeight) {
    pGT->pLines[iRow] = true;
    pGT->fRefresh = true;
  }
}

static void hb_gt_def_TouchCell(PHB_GT pGT, int iRow, int iCol)
{
  long lIndex;

  if (HB_GTSELF_CHECKPOS(pGT, iRow, iCol, &lIndex)) {
    pGT->prevBuffer[lIndex].c.bAttr = HB_GT_ATTR_REFRESH;
    pGT->pLines[iRow] = true;
    pGT->fRefresh = true;
  }
}

static void hb_gt_def_Redraw(PHB_GT pGT, int iRow, int iCol, int iSize)
{
  HB_SYMBOL_UNUSED(pGT);
  HB_SYMBOL_UNUSED(iRow);
  HB_SYMBOL_UNUSED(iCol);
  HB_SYMBOL_UNUSED(iSize);
}

static void hb_gt_def_RedrawDiff(PHB_GT pGT)
{
  if (pGT->fRefresh) {
    int r, s;
    long lIndex;

    for (auto i = 0; i < pGT->iHeight; ++i) {
      if (pGT->pLines[i]) {
        lIndex = static_cast<long>(i) * pGT->iWidth;
        for (auto l = 0; l < pGT->iWidth; ++l, ++lIndex) {
          if (pGT->prevBuffer[lIndex].uiValue != pGT->screenBuffer[lIndex].uiValue) {
            pGT->prevBuffer[lIndex].uiValue = pGT->screenBuffer[lIndex].uiValue;
            s = r = l;
            while (++l < pGT->iWidth) {
              ++lIndex;
              if (pGT->prevBuffer[lIndex].uiValue != pGT->screenBuffer[lIndex].uiValue) {
                pGT->prevBuffer[lIndex].uiValue = pGT->screenBuffer[lIndex].uiValue;
                r = l;
              } else if (pGT->iRedrawMax != 0 && l - r >= pGT->iRedrawMax) {
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

static void hb_gt_def_Refresh(PHB_GT pGT)
{
  HB_GTSELF_REDRAWDIFF(pGT);
}

static void hb_gt_def_Flush(PHB_GT pGT)
{
  if (HB_GTSELF_DISPCOUNT(pGT) == 0) {
    HB_GTSELF_REFRESH(pGT);
  }
}

static int hb_gt_def_ReadKey(PHB_GT pGT, int iEventMask)
{
  return HB_GTSELF_MOUSEREADKEY(pGT, iEventMask);
}

// helper internal function
static int hb_gt_def_InkeyFilter(PHB_GT pGT, int iKey, int iEventMask)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyFilter(%p,%d,%d)", static_cast<void*>(pGT), iKey, iEventMask));
#endif

  int iMask;

  HB_SYMBOL_UNUSED(pGT);

  if (HB_INKEY_ISEXT(iKey)) {
    if (HB_INKEY_ISEVENT(iKey)) {
      iMask = HB_INKEY_GTEVENT;
    } else if (HB_INKEY_ISMOUSEPOS(iKey)) {
      iMask = INKEY_MOVE;
    } else if (HB_INKEY_ISMOUSEKEY(iKey)) {
      switch (HB_INKEY_VALUE(iKey)) {
      case K_MOUSEMOVE:
      case K_MMLEFTDOWN:
      case K_MMRIGHTDOWN:
      case K_MMMIDDLEDOWN:
      case K_NCMOUSEMOVE:
        iMask = INKEY_MOVE;
        break;
      case K_LBUTTONDOWN:
      case K_LDBLCLK:
        iMask = INKEY_LDOWN;
        break;
      case K_LBUTTONUP:
        iMask = INKEY_LUP;
        break;
      case K_RBUTTONDOWN:
      case K_RDBLCLK:
        iMask = INKEY_RDOWN;
        break;
      case K_RBUTTONUP:
        iMask = INKEY_RUP;
        break;
      case K_MBUTTONDOWN:
      case K_MBUTTONUP:
      case K_MDBLCLK:
        iMask = INKEY_MMIDDLE;
        break;
      case K_MWFORWARD:
      case K_MWBACKWARD:
      case K_MWLEFT:
      case K_MWRIGHT:
        iMask = INKEY_MWHEEL;
        break;
      default:
        iMask = INKEY_KEYBOARD;
      }
    } else {
      iMask = INKEY_KEYBOARD;
    }
  } else {
    switch (iKey) {
    case K_MOUSEMOVE:
    case K_MMLEFTDOWN:
    case K_MMRIGHTDOWN:
    case K_MMMIDDLEDOWN:
    case K_NCMOUSEMOVE:
      iMask = INKEY_MOVE;
      break;
    case K_LBUTTONDOWN:
    case K_LDBLCLK:
      iMask = INKEY_LDOWN;
      break;
    case K_LBUTTONUP:
      iMask = INKEY_LUP;
      break;
    case K_RBUTTONDOWN:
    case K_RDBLCLK:
      iMask = INKEY_RDOWN;
      break;
    case K_RBUTTONUP:
      iMask = INKEY_RUP;
      break;
    case K_MBUTTONDOWN:
    case K_MBUTTONUP:
    case K_MDBLCLK:
      iMask = INKEY_MMIDDLE;
      break;
    case K_MWFORWARD:
    case K_MWBACKWARD:
    case K_MWLEFT:
    case K_MWRIGHT:
      iMask = INKEY_MWHEEL;
      break;
    case HB_K_RESIZE:
    case HB_K_CLOSE:
    case HB_K_GOTFOCUS:
    case HB_K_LOSTFOCUS:
    case HB_K_CONNECT:
    case HB_K_DISCONNECT:
    case HB_K_TERMINATE:
    case HB_K_MENU:
      iMask = HB_INKEY_GTEVENT;
      break;
    default:
      iMask = INKEY_KEYBOARD;
      break;
    }
  }

  if ((iMask & iEventMask) == 0) {
    return 0;
  }

  if (HB_INKEY_ISEXT(iKey) && (iEventMask & HB_INKEY_EXT) == 0) {
    iKey = hb_inkeyKeyStd(iKey);
  }

  return iKey;
}

// helper internal function: drop the next key in keyboard buffer
static void hb_gt_def_InkeyPop(PHB_GT pGT)
{
  if (pGT->StrBuffer) {
    if (++pGT->StrBufferPos >= pGT->StrBufferSize) {
      hb_xfree(pGT->StrBuffer);
      pGT->StrBuffer = nullptr;
    }
  } else if (pGT->inkeyHead != pGT->inkeyTail) {
    if (++pGT->inkeyTail >= pGT->inkeyBufferSize) {
      pGT->inkeyTail = 0;
    }
  }
}

// Put the key into keyboard buffer
static void hb_gt_def_InkeyPut(PHB_GT pGT, int iKey)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyPut(%p,%d)", static_cast<void*>(pGT), iKey));
#endif

  int iHead;

  iHead = pGT->inkeyHead;

  if (pGT->inkeyHead != pGT->inkeyTail && pGT->inkeyLastPos >= 0 &&
      (iKey == K_MOUSEMOVE || HB_INKEY_ISMOUSEPOS(iKey))) {
    // Clipper does not store in buffer repeated mouse movement
    // IMHO it's good idea to reduce unnecessary inkey buffer
    // overloading so I also implemented it, [druzus]

    int iLastKey = pGT->inkeyBuffer[pGT->inkeyLastPos];

    if (iLastKey == K_MOUSEMOVE || HB_INKEY_ISMOUSEPOS(iLastKey)) {
      if (HB_INKEY_ISMOUSEPOS(iKey)) {
        pGT->inkeyBuffer[pGT->inkeyLastPos] = iKey;
      }
      return;
    }
  }

  // When the buffer is full new event overwrite the last one
  // in the buffer - it's Clipper behavior, [druzus]

  pGT->inkeyBuffer[pGT->inkeyLastPos = iHead++] = iKey;
  if (iHead >= pGT->inkeyBufferSize) {
    iHead = 0;
  }

  if (iHead != pGT->inkeyTail) {
    pGT->inkeyHead = iHead;
  }
}

// Inset the key into head of keyboard buffer
static void hb_gt_def_InkeyIns(PHB_GT pGT, int iKey)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyIns(%p,%d)", static_cast<void*>(pGT), iKey));
#endif

  if (--pGT->inkeyTail < 0) {
    pGT->inkeyTail = pGT->inkeyBufferSize - 1;
  }

  pGT->inkeyBuffer[pGT->inkeyTail] = iKey;

  // When the buffer is full new event overwrite the last one
  // in the buffer. [druzus]

  if (pGT->inkeyHead == pGT->inkeyTail) {
    if (--pGT->inkeyHead < 0) {
      pGT->inkeyHead = pGT->inkeyBufferSize - 1;
    }
    pGT->inkeyLastPos = -1;
  }
}

// helper internal function
static HB_BOOL hb_gt_def_InkeyNextCheck(PHB_GT pGT, int iEventMask, int *iKey)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyNextCheck(%p,%p)", static_cast<void*>(pGT), static_cast<void*>(iKey)));
#endif

  if (pGT->StrBuffer) {
    *iKey = pGT->StrBuffer[pGT->StrBufferPos];
    if (*iKey >= 128) {
      *iKey = HB_INKEY_NEW_UNICODE(*iKey);
      if ((iEventMask & HB_INKEY_EXT) == 0) {
        *iKey = hb_inkeyKeyStd(*iKey);
      }
    }
  } else if (pGT->inkeyHead != pGT->inkeyTail) {
    *iKey = hb_gt_def_InkeyFilter(pGT, pGT->inkeyBuffer[pGT->inkeyTail], iEventMask);
  } else {
    return false;
  }

  if (*iKey == 0) {
    hb_gt_def_InkeyPop(pGT);
    return false;
  }

  return true;
}

// helper internal function
static void hb_gt_def_InkeyPollDo(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyPollDo(%p)", static_cast<void*>(pGT)));
#endif

  int iKey;

  iKey = HB_GTSELF_READKEY(pGT, HB_INKEY_ALL | HB_INKEY_EXT);

  if (iKey) {
    if (HB_INKEY_ISEXT(iKey)) {
      if (HB_INKEY_FLAGS(iKey) & HB_KF_ALT) {
        switch (HB_INKEY_VALUE(iKey)) {
        case 'C':
        case 'c':
          if (hb_setGetCancel()) {
            hb_vmRequestCancel(); // Request cancellation
            return;
          }
          break;
        case 'D':
        case 'd':
          if (hb_setGetDebug()) {
            hb_vmRequestDebug(); // Request the debugger
            return;
          }
        }
      }
    } else {
      switch (iKey) {
      case HB_BREAK_FLAG: // Check for Ctrl+Break
      case K_ALT_C:       // Check for normal Alt+C
        if (hb_setGetCancel()) {
          hb_vmRequestCancel(); // Request cancellation
          return;
        }
        break;
      case K_ALT_D: // Check for Alt+D
        if (hb_setGetDebug()) {
          hb_vmRequestDebug(); // Request the debugger
          return;
        }
      }
    }
    HB_GTSELF_INKEYPUT(pGT, iKey);
  }
}

// Poll the console keyboard to stuff the Harbour buffer
static void hb_gt_def_InkeyPoll(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyPoll(%p)", static_cast<void*>(pGT)));
#endif

  // Clipper 5.3 always poll events without respecting
  // _SET_TYPEAHEAD when CL5.2 only when it's non zero.
  // IMHO keeping CL5.2 behavior will be more accurate for Harbour
  // because it allows to control it by user what some times could be
  // necessary due to different low-level GT behavior on some platforms

  if (hb_setGetTypeAhead()) {
    hb_gt_def_InkeyPollDo(pGT);
  }
}

// Return the next key without extracting it
static int hb_gt_def_InkeyNext(PHB_GT pGT, int iEventMask)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyNext(%p,%d)", static_cast<void*>(pGT), iEventMask));
#endif

  int iKey = 0;
  HB_GTSELF_INKEYPOLL(pGT);
  hb_gt_def_InkeyNextCheck(pGT, iEventMask, &iKey);
  return iKey;
}

// Wait for keyboard input
static int hb_gt_def_InkeyGet(PHB_GT pGT, HB_BOOL fWait, double dSeconds, int iEventMask)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyGet(%p,%d,%f,%d)", static_cast<void*>(pGT), static_cast<int>(fWait), dSeconds, iEventMask));
#endif

  HB_MAXUINT timer;
  HB_MAXINT timeout;
  PHB_ITEM pKey;
  auto fPop = false;

  pKey = nullptr;

  if (pGT->pInkeyReadBlock) {
    int iKey;
    HB_GTSELF_UNLOCK(pGT);
    iKey = hb_vmEvalBlock(pGT->pInkeyReadBlock)->getNI();
    HB_GTSELF_LOCK(pGT);
    if (iKey != 0) {
      return iKey;
    }
  }

  // Wait forever ?, Use fixed value 100 for strict Clipper compatibility
  timeout = (fWait && dSeconds * 100 >= 1) ? static_cast<HB_MAXINT>(dSeconds * 1000) : -1;
  timer = hb_timerInit(timeout);

  for (;;) {
    hb_gt_def_InkeyPollDo(pGT);
    fPop = hb_gt_def_InkeyNextCheck(pGT, iEventMask, &pGT->inkeyLast);

    if (fPop) {
      hb_gt_def_InkeyPop(pGT);
      if (!pGT->pInkeyFilterBlock) {
        break;
      }
      pKey = hb_itemPutNI(pKey, pGT->inkeyLast);
      HB_GTSELF_UNLOCK(pGT);
      pGT->inkeyLast = hb_vmEvalBlockV(pGT->pInkeyFilterBlock, 1, pKey)->getNI();
      HB_GTSELF_LOCK(pGT);
      if (pGT->inkeyLast != 0) {
        break;
      }
    }

    // immediately break if a VM request is pending.
    if (!fWait || (timeout = hb_timerTest(timeout, &timer)) == 0 || hb_vmRequestQuery() != 0) {
      break;
    }

    HB_GTSELF_UNLOCK(pGT);
    hb_idleState();
    HB_GTSELF_LOCK(pGT);
  }

  if (pKey) {
    hb_itemRelease(pKey);
  }

  hb_idleReset();

  return fPop ? pGT->inkeyLast : 0;
}

// Return the value of the last key that was extracted
static int hb_gt_def_InkeyLast(PHB_GT pGT, int iEventMask)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyLast(%p,%d)", static_cast<void*>(pGT), iEventMask));
#endif

  HB_GTSELF_INKEYPOLL(pGT);
  return hb_gt_def_InkeyFilter(pGT, pGT->inkeyLast, iEventMask);
}

// Set LastKey() value and return previous value
static int hb_gt_def_InkeySetLast(PHB_GT pGT, int iKey)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeySetLast(%p,%d)", static_cast<void*>(pGT), iKey));
#endif

  int iLast;
  iLast = pGT->inkeyLast;
  pGT->inkeyLast = iKey;
  return iLast;
}

// Set text into inkey buffer
static void hb_gt_def_InkeySetText(PHB_GT pGT, const char *szText, HB_SIZE nLen, HB_BOOL fEol)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeySetText(%p,%s,%" HB_PFS "u, %d)", static_cast<void*>(pGT), szText, nLen, fEol));
#endif

  if (pGT->StrBuffer) {
    hb_xfree(pGT->StrBuffer);
    pGT->StrBuffer = nullptr;
  }

  if (szText != nullptr && nLen) {
    auto cdp = hb_vmCDP();
    HB_SIZE nIndex = 0;
    HB_WCHAR wc, prev = 0;

    pGT->StrBufferSize = pGT->StrBufferPos = 0;
    pGT->StrBuffer = static_cast<HB_WCHAR *>(hb_xgrab(nLen * sizeof(HB_WCHAR)));
    while (HB_CDPCHAR_GET(cdp, szText, nLen, &nIndex, &wc)) {
      if (fEol) {
        if (wc == HB_CHAR_LF) {
          if (prev == HB_CHAR_CR) {
            prev = 0;
            continue;
          } else {
            wc = HB_CHAR_CR;
          }
        }
      } else if (wc == ';') {
        wc = HB_CHAR_CR;
      }
      pGT->StrBuffer[pGT->StrBufferSize++] = prev = wc;
    }
    if (pGT->StrBufferSize == 0) {
      hb_xfree(pGT->StrBuffer);
      pGT->StrBuffer = nullptr;
    }
  }
}

// Reset the keyboard buffer
static void hb_gt_def_InkeyReset(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyReset(%p)", static_cast<void*>(pGT)));
#endif

  int iTypeAhead;

  if (pGT->StrBuffer) {
    hb_xfree(pGT->StrBuffer);
    pGT->StrBuffer = nullptr;
  }

  pGT->inkeyHead = 0;
  pGT->inkeyTail = 0;
  pGT->inkeyLastPos = -1;

  iTypeAhead = hb_setGetTypeAhead();

  if (iTypeAhead != pGT->inkeyBufferSize) {
    if (pGT->inkeyBufferSize > HB_DEFAULT_INKEY_BUFSIZE) {
      hb_xfree(pGT->inkeyBuffer);
    }

    if (iTypeAhead > HB_DEFAULT_INKEY_BUFSIZE) {
      pGT->inkeyBufferSize = iTypeAhead;
      pGT->inkeyBuffer = static_cast<int *>(hb_xgrab(pGT->inkeyBufferSize * sizeof(int)));
    } else {
      pGT->inkeyBufferSize = HB_DEFAULT_INKEY_BUFSIZE;
      pGT->inkeyBuffer = pGT->defaultKeyBuffer;
    }
  }
}

// reset inkey pool to default state and free any allocated resources
static void hb_gt_def_InkeyExit(PHB_GT pGT)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyExit(%p)", static_cast<void*>(pGT)));
#endif

  if (pGT->StrBuffer) {
    hb_xfree(pGT->StrBuffer);
    pGT->StrBuffer = nullptr;
  }

  if (pGT->inkeyBufferSize > HB_DEFAULT_INKEY_BUFSIZE) {
    hb_xfree(pGT->inkeyBuffer);
    pGT->inkeyBufferSize = HB_DEFAULT_INKEY_BUFSIZE;
    pGT->inkeyBuffer = pGT->defaultKeyBuffer;
  }
}

static void hb_gt_def_MouseInit(PHB_GT pGT)
{
  HB_SYMBOL_UNUSED(pGT);
}

static void hb_gt_def_MouseExit(PHB_GT pGT)
{
  HB_SYMBOL_UNUSED(pGT);
}

static HB_BOOL hb_gt_def_MouseIsPresent(PHB_GT pGT)
{
  HB_SYMBOL_UNUSED(pGT);
  return false;
}

static void hb_gt_def_MouseShow(PHB_GT pGT)
{
  HB_SYMBOL_UNUSED(pGT);
}

static void hb_gt_def_MouseHide(PHB_GT pGT)
{
  HB_SYMBOL_UNUSED(pGT);
}

static HB_BOOL hb_gt_def_MouseGetCursor(PHB_GT pGT)
{
  return pGT->fMouseVisible;
}

static void hb_gt_def_MouseSetCursor(PHB_GT pGT, HB_BOOL fVisible)
{
  if (fVisible) {
    HB_GTSELF_MOUSESHOW(pGT);
    pGT->fMouseVisible = true;
  } else if (pGT->fMouseVisible) {
    // mouse drivers use hide counters, so repeated calls to
    // HB_GTSELF_MOUSEHIDE(pGT) will need at least the same number of
    // calls to HB_GTSELF_MOUSESHOW() to make mouse cursor visible. This
    // behavior is not compatible with Clipper so call to
    // HB_GTSELF_MOUSEHIDE(pGT) is guarded by pGT->fMouseVisible.
    // The counter is not updated when mouse cursor is visible and
    // HB_GTSELF_MOUSESHOW() is called so this behavior is enough.
    // If some platform works in differ way then and this behavior
    // will create problems GT driver should overload
    // HB_GTSELF_MOUSESETCURSOR()/HB_GTSELF_MOUSEGETCURSOR() methods.
    // [druzus]

    HB_GTSELF_MOUSEHIDE(pGT);
    pGT->fMouseVisible = false;
  }
}

static int hb_gt_def_MouseRow(PHB_GT pGT)
{
  int iRow, iCol;
  HB_GTSELF_MOUSEGETPOS(pGT, &iRow, &iCol);
  return iRow;
}

static int hb_gt_def_MouseCol(PHB_GT pGT)
{
  int iRow, iCol;
  HB_GTSELF_MOUSEGETPOS(pGT, &iRow, &iCol);
  return iCol;
}

static void hb_gt_def_MouseGetPos(PHB_GT pGT, int *piRow, int *piCol)
{
  HB_SYMBOL_UNUSED(pGT);
  *piRow = *piCol = 0;
}

static void hb_gt_def_MouseSetPos(PHB_GT pGT, int iRow, int iCol)
{
  HB_SYMBOL_UNUSED(pGT);
  HB_SYMBOL_UNUSED(iRow);
  HB_SYMBOL_UNUSED(iCol);
}

static void hb_gt_def_MouseSetBounds(PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight)
{
  HB_SYMBOL_UNUSED(pGT);
  HB_SYMBOL_UNUSED(iTop);
  HB_SYMBOL_UNUSED(iLeft);
  HB_SYMBOL_UNUSED(iBottom);
  HB_SYMBOL_UNUSED(iRight);
}

static void hb_gt_def_MouseGetBounds(PHB_GT pGT, int *piTop, int *piLeft, int *piBottom, int *piRight)
{
  *piTop = *piLeft = 0;
  HB_GTSELF_GETSIZE(pGT, piBottom, piRight);
  --(*piBottom);
  --(*piRight);
}

struct _HB_MOUSE_STORAGE
{
  int iRow;
  int iCol;
  int fVisible;
  int iTop;
  int iLeft;
  int iBottom;
  int iRight;
};

static int hb_gt_def_mouseStorageSize(PHB_GT pGT)
{
  HB_SYMBOL_UNUSED(pGT);
  return sizeof(_HB_MOUSE_STORAGE);
}

static void hb_gt_def_mouseSaveState(PHB_GT pGT, void *pBuffer)
{
  auto pStore = static_cast<_HB_MOUSE_STORAGE *>(pBuffer);
  int iRow, iCol, iTop, iLeft, iBottom, iRight;

  HB_GTSELF_MOUSEGETPOS(pGT, &iRow, &iCol);
  HB_GTSELF_MOUSEGETBOUNDS(pGT, &iTop, &iLeft, &iBottom, &iRight);

  pStore->iRow = iRow;
  pStore->iCol = iCol;
  pStore->fVisible = HB_GTSELF_MOUSEGETCURSOR(pGT);
  pStore->iTop = iTop;
  pStore->iLeft = iLeft;
  pStore->iBottom = iBottom;
  pStore->iRight = iRight;
}

static void hb_gt_def_mouseRestoreState(PHB_GT pGT, const void *pBuffer)
{
  auto pStore = static_cast<const _HB_MOUSE_STORAGE *>(pBuffer);

  HB_GTSELF_MOUSESETBOUNDS(pGT, pStore->iTop, pStore->iLeft, pStore->iBottom, pStore->iRight);
  HB_GTSELF_MOUSESETPOS(pGT, pStore->iRow, pStore->iCol);
  HB_GTSELF_MOUSESETCURSOR(pGT, pStore->fVisible);
}

static int hb_gt_def_mouseGetDoubleClickSpeed(PHB_GT pGT)
{
  return pGT->iDoubleClickSpeed;
}

static void hb_gt_def_mouseSetDoubleClickSpeed(PHB_GT pGT, int iSpeed)
{
  if (iSpeed > 0) {
    pGT->iDoubleClickSpeed = iSpeed;
  }
}

static int hb_gt_def_MouseCountButton(PHB_GT pGT)
{
  HB_SYMBOL_UNUSED(pGT);
  return 0;
}

static HB_BOOL hb_gt_def_MouseButtonState(PHB_GT pGT, int iButton)
{
  HB_SYMBOL_UNUSED(pGT);
  HB_SYMBOL_UNUSED(iButton);
  return false;
}

static HB_BOOL hb_gt_def_MouseButtonPressed(PHB_GT pGT, int iButton, int *piRow, int *piCol)
{
  HB_SYMBOL_UNUSED(pGT);
  HB_SYMBOL_UNUSED(iButton);
  HB_SYMBOL_UNUSED(piRow);
  HB_SYMBOL_UNUSED(piCol);
  return false;
}

static HB_BOOL hb_gt_def_MouseButtonReleased(PHB_GT pGT, int iButton, int *piRow, int *piCol)
{
  HB_SYMBOL_UNUSED(pGT);
  HB_SYMBOL_UNUSED(iButton);
  HB_SYMBOL_UNUSED(piRow);
  HB_SYMBOL_UNUSED(piCol);
  return false;
}

static int hb_gt_def_MouseReadKey(PHB_GT pGT, int iEventMask)
{
  int iKey = 0, iRow, iCol;

  if (HB_GTSELF_MOUSEISPRESENT(pGT)) {
    if (iEventMask & INKEY_LDOWN && HB_GTSELF_MOUSEBUTTONPRESSED(pGT, 0, &iRow, &iCol)) {
      HB_MAXUINT timer = hb_timerGet();
      if (timer - pGT->nMouseLeftTimer <= static_cast<HB_MAXUINT>(HB_GTSELF_MOUSEGETDOUBLECLICKSPEED(pGT))) {
        iKey = K_LDBLCLK;
      } else {
        iKey = K_LBUTTONDOWN;
      }
      pGT->nMouseLeftTimer = timer;
    } else if (iEventMask & INKEY_LUP && HB_GTSELF_MOUSEBUTTONRELEASED(pGT, 0, &iRow, &iCol)) {
      iKey = K_LBUTTONUP;
    } else if (iEventMask & INKEY_RDOWN && HB_GTSELF_MOUSEBUTTONPRESSED(pGT, 1, &iRow, &iCol)) {
      HB_MAXUINT timer = hb_timerGet();
      if (timer - pGT->nMouseRightTimer <= static_cast<HB_MAXUINT>(HB_GTSELF_MOUSEGETDOUBLECLICKSPEED(pGT))) {
        iKey = K_RDBLCLK;
      } else {
        iKey = K_RBUTTONDOWN;
      }
      pGT->nMouseRightTimer = timer;
    } else if (iEventMask & INKEY_RUP && HB_GTSELF_MOUSEBUTTONRELEASED(pGT, 1, &iRow, &iCol)) {
      iKey = K_RBUTTONUP;
    } else if (iEventMask & INKEY_MMIDDLE && HB_GTSELF_MOUSEBUTTONPRESSED(pGT, 2, &iRow, &iCol)) {
      HB_MAXUINT timer = hb_timerGet();
      if (timer - pGT->nMouseMiddleTimer <= static_cast<HB_MAXUINT>(HB_GTSELF_MOUSEGETDOUBLECLICKSPEED(pGT))) {
        iKey = K_MDBLCLK;
      } else {
        iKey = K_MBUTTONDOWN;
      }
      pGT->nMouseMiddleTimer = timer;
    } else if (iEventMask & INKEY_MMIDDLE && HB_GTSELF_MOUSEBUTTONRELEASED(pGT, 2, &iRow, &iCol)) {
      iKey = K_MBUTTONUP;
    } else if (iEventMask & INKEY_MOVE) {
      HB_GTSELF_MOUSEGETPOS(pGT, &iRow, &iCol);
      if (iRow != pGT->iMouseLastRow || iCol != pGT->iMouseLastCol) {
        pGT->iMouseLastRow = iRow;
        pGT->iMouseLastCol = iCol;
        iKey = HB_INKEY_NEW_MPOS(iCol, iRow);
      }
    }
  }
  return iKey;
}

static int hb_gt_def_GfxPrimitive(PHB_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor)
{
  HB_SYMBOL_UNUSED(pGT);
  HB_SYMBOL_UNUSED(iType);
  HB_SYMBOL_UNUSED(iTop);
  HB_SYMBOL_UNUSED(iLeft);
  HB_SYMBOL_UNUSED(iBottom);
  HB_SYMBOL_UNUSED(iRight);
  HB_SYMBOL_UNUSED(iColor);
  return 0;
}

static void hb_gt_def_GfxText(PHB_GT pGT, int iTop, int iLeft, const char *szText, int iColor, int iSize, int iWidth)
{
  HB_SYMBOL_UNUSED(pGT);
  HB_SYMBOL_UNUSED(iTop);
  HB_SYMBOL_UNUSED(iLeft);
  HB_SYMBOL_UNUSED(szText);
  HB_SYMBOL_UNUSED(iColor);
  HB_SYMBOL_UNUSED(iSize);
  HB_SYMBOL_UNUSED(iWidth);
}

static void hb_gt_def_WhoCares(PHB_GT pGT, void *pCargo)
{
  HB_SYMBOL_UNUSED(pGT);
  HB_SYMBOL_UNUSED(pCargo);
}

// -

#if defined(__GNUC__) && 0
static const HB_GT_FUNCS s_gtCoreFunc = {
  Lock : hb_gt_def_Lock,
  Unlock : hb_gt_def_Unlock,
  Init : hb_gt_def_Init,
  Exit : hb_gt_def_Exit,
  New : hb_gt_def_New,
  Free : hb_gt_def_Free,
  Mark : hb_gt_def_Mark,
  Resize : hb_gt_def_Resize,
  SetMode : hb_gt_def_SetMode,
  GetSize : hb_gt_def_GetSize,
  SemiCold : hb_gt_def_SemiCold,
  ColdArea : hb_gt_def_ColdArea,
  ExposeArea : hb_gt_def_ExposeArea,
  ScrollArea : hb_gt_def_ScrollArea,
  TouchLine : hb_gt_def_TouchLine,
  TouchCell : hb_gt_def_TouchCell,
  Redraw : hb_gt_def_Redraw,
  RedrawDiff : hb_gt_def_RedrawDiff,
  Refresh : hb_gt_def_Refresh,
  Flush : hb_gt_def_Flush,
  MaxCol : hb_gt_def_MaxCol,
  MaxRow : hb_gt_def_MaxRow,
  CheckPos : hb_gt_def_CheckPos,
  SetPos : hb_gt_def_SetPos,
  GetPos : hb_gt_def_GetPos,
  IsColor : hb_gt_def_IsColor,
  GetColorStr : hb_gt_def_GetColorStr,
  SetColorStr : hb_gt_def_SetColorStr,
  ColorSelect : hb_gt_def_ColorSelect,
  GetColor : hb_gt_def_GetColor,
  ColorNum : hb_gt_def_ColorNum,
  ColorsToString : hb_gt_def_ColorsToString,
  StringToColors : hb_gt_def_StringToColors,
  GetColorData : hb_gt_def_GetColorData,
  GetClearColor : hb_gt_def_GetClearColor,
  SetClearColor : hb_gt_def_SetClearColor,
  GetClearChar : hb_gt_def_GetClearChar,
  SetClearChar : hb_gt_def_SetClearChar,
  GetCursorStyle : hb_gt_def_GetCursorStyle,
  SetCursorStyle : hb_gt_def_SetCursorStyle,
  GetScrCursor : hb_gt_def_GetScrCursor,
  GetScrChar : hb_gt_def_GetChar,
  PutScrChar : hb_gt_def_PutChar,
  GetScrUC : hb_gt_def_GetUC,
  DispBegin : hb_gt_def_DispBegin,
  DispEnd : hb_gt_def_DispEnd,
  DispCount : hb_gt_def_DispCount,
  GetChar : hb_gt_def_GetChar,
  PutChar : hb_gt_def_PutChar,
  RectSize : hb_gt_def_RectSize,
  Save : hb_gt_def_Save,
  Rest : hb_gt_def_Rest,
  PutText : hb_gt_def_PutText,
  PutTextW : hb_gt_def_PutTextW,
  Replicate : hb_gt_def_Replicate,
  WriteAt : hb_gt_def_WriteAt,
  WriteAtW : hb_gt_def_WriteAtW,
  Write : hb_gt_def_Write,
  WriteW : hb_gt_def_WriteW,
  WriteCon : hb_gt_def_WriteCon,
  WriteConW : hb_gt_def_WriteConW,
  SetAttribute : hb_gt_def_SetAttribute,
  DrawShadow : hb_gt_def_DrawShadow,
  Scroll : hb_gt_def_Scroll,
  ScrollUp : hb_gt_def_ScrollUp,
  Box : hb_gt_def_Box,
  BoxW : hb_gt_def_BoxW,
  BoxD : hb_gt_def_BoxD,
  BoxS : hb_gt_def_BoxS,
  HorizLine : hb_gt_def_HorizLine,
  VertLine : hb_gt_def_VertLine,
  GetBlink : hb_gt_def_GetBlink,
  SetBlink : hb_gt_def_SetBlink,
  SetSnowFlag : hb_gt_def_SetSnowFlag,
  Version : hb_gt_def_Version,
  Suspend : hb_gt_def_Suspend,
  Resume : hb_gt_def_Resume,
  PreExt : hb_gt_def_PreExt,
  PostExt : hb_gt_def_PostExt,
  OutStd : hb_gt_def_OutStd,
  OutErr : hb_gt_def_OutErr,
  Tone : hb_gt_def_Tone,
  Bell : hb_gt_def_Bell,
  Info : hb_gt_def_Info,
  Alert : hb_gt_def_Alert,
  SetFlag : hb_gt_def_SetFlag,
  SetDispCP : hb_gt_def_SetDispCP,
  SetKeyCP : hb_gt_def_SetKeyCP,
  ReadKey : hb_gt_def_ReadKey,
  InkeyGet : hb_gt_def_InkeyGet,
  InkeyPut : hb_gt_def_InkeyPut,
  InkeyIns : hb_gt_def_InkeyIns,
  InkeyLast : hb_gt_def_InkeyLast,
  InkeyNext : hb_gt_def_InkeyNext,
  InkeyPoll : hb_gt_def_InkeyPoll,
  InkeySetText : hb_gt_def_InkeySetText,
  InkeySetLast : hb_gt_def_InkeySetLast,
  InkeyReset : hb_gt_def_InkeyReset,
  InkeyExit : hb_gt_def_InkeyExit,
  MouseInit : hb_gt_def_MouseInit,
  MouseExit : hb_gt_def_MouseExit,
  MouseIsPresent : hb_gt_def_MouseIsPresent,
  MouseShow : hb_gt_def_MouseShow,
  MouseHide : hb_gt_def_MouseHide,
  MouseGetCursor : hb_gt_def_MouseGetCursor,
  MouseSetCursor : hb_gt_def_MouseSetCursor,
  MouseCol : hb_gt_def_MouseCol,
  MouseRow : hb_gt_def_MouseRow,
  MouseGetPos : hb_gt_def_MouseGetPos,
  MouseSetPos : hb_gt_def_MouseSetPos,
  MouseSetBounds : hb_gt_def_MouseSetBounds,
  MouseGetBounds : hb_gt_def_MouseGetBounds,
  MouseStorageSize : hb_gt_def_mouseStorageSize,
  MouseSaveState : hb_gt_def_mouseSaveState,
  MouseRestoreState : hb_gt_def_mouseRestoreState,
  MouseGetDoubleClickSpeed : hb_gt_def_mouseGetDoubleClickSpeed,
  MouseSetDoubleClickSpeed : hb_gt_def_mouseSetDoubleClickSpeed,
  MouseCountButton : hb_gt_def_MouseCountButton,
  MouseButtonState : hb_gt_def_MouseButtonState,
  MouseButtonPressed : hb_gt_def_MouseButtonPressed,
  MouseButtonReleased : hb_gt_def_MouseButtonReleased,
  MouseReadKey : hb_gt_def_MouseReadKey,
  GfxPrimitive : hb_gt_def_GfxPrimitive,
  GfxText : hb_gt_def_GfxText,
  WhoCares : hb_gt_def_WhoCares
};
#else
static const HB_GT_FUNCS s_gtCoreFunc = {hb_gt_def_Lock,
                                         hb_gt_def_Unlock,
                                         hb_gt_def_Init,
                                         hb_gt_def_Exit,
                                         hb_gt_def_New,
                                         hb_gt_def_Free,
                                         hb_gt_def_Mark,
                                         hb_gt_def_Resize,
                                         hb_gt_def_SetMode,
                                         hb_gt_def_GetSize,
                                         hb_gt_def_SemiCold,
                                         hb_gt_def_ColdArea,
                                         hb_gt_def_ExposeArea,
                                         hb_gt_def_ScrollArea,
                                         hb_gt_def_TouchLine,
                                         hb_gt_def_TouchCell,
                                         hb_gt_def_Redraw,
                                         hb_gt_def_RedrawDiff,
                                         hb_gt_def_Refresh,
                                         hb_gt_def_Flush,
                                         hb_gt_def_MaxCol,
                                         hb_gt_def_MaxRow,
                                         hb_gt_def_CheckPos,
                                         hb_gt_def_SetPos,
                                         hb_gt_def_GetPos,
                                         hb_gt_def_IsColor,
                                         hb_gt_def_GetColorStr,
                                         hb_gt_def_SetColorStr,
                                         hb_gt_def_ColorSelect,
                                         hb_gt_def_GetColor,
                                         hb_gt_def_ColorNum,
                                         hb_gt_def_ColorsToString,
                                         hb_gt_def_StringToColors,
                                         hb_gt_def_GetColorData,
                                         hb_gt_def_GetClearColor,
                                         hb_gt_def_SetClearColor,
                                         hb_gt_def_GetClearChar,
                                         hb_gt_def_SetClearChar,
                                         hb_gt_def_GetCursorStyle,
                                         hb_gt_def_SetCursorStyle,
                                         hb_gt_def_GetScrCursor,
                                         hb_gt_def_GetChar, // intentionally mapped to GetScrChar
                                         hb_gt_def_PutChar, // intentionally mapped to PutScrChar
                                         hb_gt_def_GetUC,   // intentionally mapped to GetScrUC
                                         hb_gt_def_DispBegin,
                                         hb_gt_def_DispEnd,
                                         hb_gt_def_DispCount,
                                         hb_gt_def_GetChar,
                                         hb_gt_def_PutChar,
                                         hb_gt_def_RectSize,
                                         hb_gt_def_Save,
                                         hb_gt_def_Rest,
                                         hb_gt_def_PutText,
                                         hb_gt_def_PutTextW,
                                         hb_gt_def_Replicate,
                                         hb_gt_def_WriteAt,
                                         hb_gt_def_WriteAtW,
                                         hb_gt_def_Write,
                                         hb_gt_def_WriteW,
                                         hb_gt_def_WriteCon,
                                         hb_gt_def_WriteConW,
                                         hb_gt_def_SetAttribute,
                                         hb_gt_def_DrawShadow,
                                         hb_gt_def_Scroll,
                                         hb_gt_def_ScrollUp,
                                         hb_gt_def_Box,
                                         hb_gt_def_BoxW,
                                         hb_gt_def_BoxD,
                                         hb_gt_def_BoxS,
                                         hb_gt_def_HorizLine,
                                         hb_gt_def_VertLine,
                                         hb_gt_def_GetBlink,
                                         hb_gt_def_SetBlink,
                                         hb_gt_def_SetSnowFlag,
                                         hb_gt_def_Version,
                                         hb_gt_def_Suspend,
                                         hb_gt_def_Resume,
                                         hb_gt_def_PreExt,
                                         hb_gt_def_PostExt,
                                         hb_gt_def_OutStd,
                                         hb_gt_def_OutErr,
                                         hb_gt_def_Tone,
                                         hb_gt_def_Bell,
                                         hb_gt_def_Info,
                                         hb_gt_def_Alert,
                                         hb_gt_def_SetFlag,
                                         hb_gt_def_SetDispCP,
                                         hb_gt_def_SetKeyCP,
                                         hb_gt_def_ReadKey,
                                         hb_gt_def_InkeyGet,
                                         hb_gt_def_InkeyPut,
                                         hb_gt_def_InkeyIns,
                                         hb_gt_def_InkeyLast,
                                         hb_gt_def_InkeyNext,
                                         hb_gt_def_InkeyPoll,
                                         hb_gt_def_InkeySetText,
                                         hb_gt_def_InkeySetLast,
                                         hb_gt_def_InkeyReset,
                                         hb_gt_def_InkeyExit,
                                         hb_gt_def_MouseInit,
                                         hb_gt_def_MouseExit,
                                         hb_gt_def_MouseIsPresent,
                                         hb_gt_def_MouseShow,
                                         hb_gt_def_MouseHide,
                                         hb_gt_def_MouseGetCursor,
                                         hb_gt_def_MouseSetCursor,
                                         hb_gt_def_MouseCol,
                                         hb_gt_def_MouseRow,
                                         hb_gt_def_MouseGetPos,
                                         hb_gt_def_MouseSetPos,
                                         hb_gt_def_MouseSetBounds,
                                         hb_gt_def_MouseGetBounds,
                                         hb_gt_def_mouseStorageSize,
                                         hb_gt_def_mouseSaveState,
                                         hb_gt_def_mouseRestoreState,
                                         hb_gt_def_mouseGetDoubleClickSpeed,
                                         hb_gt_def_mouseSetDoubleClickSpeed,
                                         hb_gt_def_MouseCountButton,
                                         hb_gt_def_MouseButtonState,
                                         hb_gt_def_MouseButtonPressed,
                                         hb_gt_def_MouseButtonReleased,
                                         hb_gt_def_MouseReadKey,
                                         hb_gt_def_GfxPrimitive,
                                         hb_gt_def_GfxText,
                                         hb_gt_def_WhoCares};
#endif

// -

static char s_gtNameBuf[HB_GT_NAME_MAX_ + 1];

// NOTE: Must be in sync with gtsys.c
#if defined(HB_GT_LIB)
static const char *s_szNameDefault = HB_GT_DRVNAME(HB_GT_LIB);
#elif defined(HB_OS_WIN)
static const char *s_szNameDefault = "win";
#elif defined(HB_OS_VXWORKS)
static const char *s_szNameDefault = "std";
#elif defined(HB_OS_UNIX)
static const char *s_szNameDefault = "trm";
#else
static const char *s_szNameDefault = "std";
#endif

static const HB_GT_INIT *s_gtInit[HB_GT_MAX_];
static int s_iGtCount = 0;

HB_FUNC_EXTERN(HB_GTSYS);

static const char *hb_gt_FindDefault(void)
{
  char szFuncName[15 + HB_GT_NAME_MAX_];

  for (auto iPos = 0; iPos < s_iGtCount; iPos++) {
    hb_snprintf(szFuncName, sizeof(szFuncName), "HB_GT_%s_DEFAULT", s_gtInit[iPos]->id);
    if (hb_dynsymFind(szFuncName)) {
      return s_gtInit[iPos]->id;
    }
  }

  if (hb_dynsymFind("HB_GT_NUL_DEFAULT")) {
    return "NUL";
  } else {
    return nullptr;
  }
}

static int hb_gt_FindEntry(const char *pszID)
{
  bool fGt = hb_strnicmp(pszID, "gt", 2) == 0;

  for (int iPos = -1; iPos < s_iGtCount; iPos++) {
    const char *id = iPos < 0 ? "nul" : s_gtInit[iPos]->id;

    if (hb_stricmp(pszID, id) == 0 || (fGt && hb_stricmp(pszID + 2, id) == 0)) {
      return iPos;
    }
  }

  return hb_stricmp(pszID + (fGt ? 2 : 0), "null") == 0 ? -1 : -2;
}

void hb_gtSetDefault(const char *szGtName)
{
  hb_strncpy(s_gtNameBuf, szGtName, sizeof(s_gtNameBuf) - 1);
  s_szNameDefault = s_gtNameBuf;
}

HB_BOOL hb_gtRegister(const HB_GT_INIT *gtInit)
{
  if (s_iGtCount < HB_GT_MAX_ && hb_gt_FindEntry(gtInit->id) < -1) {
    if (gtInit->pGtId) {
      *gtInit->pGtId = s_iGtCount;
    }
    s_gtInit[s_iGtCount++] = gtInit;
    return true;
  }
  return false;
}

PHB_GT hb_gtLoad(const char *szGtName, PHB_GT pGT, PHB_GT_FUNCS pSuperTable)
{
  if (szGtName != nullptr) {
    int iPos = hb_gt_FindEntry(szGtName);

    if (iPos == -1) {
      if (pGT || pSuperTable) {
        hb_errInternal(9996, "Harbour++ terminal (GT) initialization failure", nullptr, nullptr);
      }

      pGT = static_cast<PHB_GT_BASE>(hb_xgrabz(sizeof(HB_GT_BASE)));
      pGT->pFuncTable = static_cast<PHB_GT_FUNCS>(hb_xgrab(sizeof(HB_GT_FUNCS)));
      memcpy(pGT->pFuncTable, &s_gtCoreFunc, sizeof(HB_GT_FUNCS));
      pGT->iUsed++;
      return pGT;
    } else if (iPos >= 0) {
      bool fNew = pGT == nullptr;

      if (fNew) {
        pGT = static_cast<PHB_GT_BASE>(hb_xgrabz(sizeof(HB_GT_BASE)));
        pGT->pFuncTable = static_cast<PHB_GT_FUNCS>(hb_xgrab(sizeof(HB_GT_FUNCS)));
        memcpy(pGT->pFuncTable, &s_gtCoreFunc, sizeof(HB_GT_FUNCS));
        pGT->iUsed++;
      }

      if (pSuperTable == nullptr) {
        pSuperTable = s_gtInit[iPos]->pSuperTable;
      }
      if (pSuperTable != nullptr) {
        memcpy(pSuperTable, pGT->pFuncTable, sizeof(HB_GT_FUNCS));
      }

      if (s_gtInit[iPos]->init(pGT->pFuncTable)) {
        return pGT;
      } else if (fNew) {
        hb_xfree(pGT->pFuncTable);
        hb_xfree(pGT);
      }
    }
  }
  return nullptr;
}

void hb_gtIsGtRef(void *hGT)
{
  auto pGT = static_cast<PHB_GT>(hGT);

  if (pGT) {
    HB_GTSELF_MARK(pGT);
  }
}

void *hb_gtAlloc(void *hGT)
{
  PHB_GT pGT;

  if (hGT) {
    pGT = static_cast<PHB_GT>(hGT);
    if (!HB_GTSELF_LOCK(pGT)) {
      pGT = nullptr;
    }
  } else {
    pGT = hb_gt_Base();
  }

  if (pGT != nullptr) {
    pGT->iUsed++;
    hb_gt_BaseFree(pGT);
  }

  return static_cast<void *>(pGT);
}

void hb_gtRelease(void *hGT)
{
  PHB_GT pGT;

  if (hGT) {
    pGT = static_cast<PHB_GT>(hGT);
    if (!HB_GTSELF_LOCK(pGT)) {
      pGT = nullptr;
    }
  } else {
    pGT = hb_gt_Base();
  }

  if (pGT != nullptr) {
    if (--pGT->iUsed == 0) {
      while (HB_GTSELF_DISPCOUNT(pGT)) {
        HB_GTSELF_DISPEND(pGT);
      }
      HB_GTSELF_FLUSH(pGT);
      HB_GTSELF_EXIT(pGT);
    } else {
      hb_gt_BaseFree(pGT);
    }
  }
}

void hb_gtAttach(void *hGT)
{
  if (hGT != hb_stackGetGT()) {
    hb_gtRelease(nullptr);
    hb_stackSetGT(hGT);
  }
}

void *hb_gtSwap(void *hGT)
{
  void *hCurrGT = hb_stackGetGT();
  hb_stackSetGT(hGT);
  return hCurrGT;
}

HB_BOOL hb_gtReload(const char *szGtName, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr)
{
  auto fResult = false;

  if (szGtName != nullptr && hb_gt_FindEntry(szGtName) >= -1) {
    hb_gtRelease(nullptr);
    hb_stackSetGT(hb_gtLoad(szGtName, nullptr, nullptr));
    fResult = hb_stackGetGT() != nullptr;
    hb_gtInit(hFilenoStdin, hFilenoStdout, hFilenoStderr);
  }
  return fResult;
}

void *hb_gtCreate(const char *szGtName, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr)
{
  void *hCurrGT = hb_gtSwap(nullptr);

  if (szGtName != nullptr && hb_gt_FindEntry(szGtName) >= -1) {
    PHB_GT pGT = hb_gtLoad(szGtName, nullptr, nullptr);
    if (pGT != nullptr) {
      hb_stackSetGT(pGT);
      hb_gtInit(hFilenoStdin, hFilenoStdout, hFilenoStderr);
    }
  }
  return hb_gtSwap(hCurrGT);
}

static HB_BOOL hb_gtTryInit(const char *szGtName, HB_BOOL fFree)
{
  if (szGtName != nullptr) {
    if (hb_stackGetGT() == nullptr) {
      if (fFree) {
        auto pszStr = const_cast<char *>(strchr(szGtName, ':'));
        if (pszStr != nullptr) {
          *pszStr = '\0';
        }
      }

      if (*szGtName) {
        hb_stackSetGT(hb_gtLoad(szGtName, nullptr, nullptr));
      }
    }

    if (fFree) {
      hb_xfree(HB_UNCONST(szGtName));
    }
  }

  return hb_stackGetGT() != nullptr;
}

void hb_gtStartupInit(void)
{
  if (hb_gtTryInit(hb_cmdargString("GT"), true)) {
    return;
  }
  if (hb_gtTryInit(hb_getenv("HB_GT"), true)) {
    return;
  }
  if (s_szNameDefault == s_gtNameBuf && hb_gtTryInit(s_szNameDefault, false)) {
    return;
  }
  if (hb_gtTryInit(hb_gt_FindDefault(), false)) {
    return;
  }
  if (s_szNameDefault != s_gtNameBuf && hb_gtTryInit(s_szNameDefault, false)) {
    return;
  }
  if (hb_dynsymFind("HB_GT_NUL")) { // GTNUL was explicitly REQUESTed
    if (hb_gtTryInit("NUL", false)) {
      return;
    }
  }

  hb_errInternal(9998, "Harbour++ terminal (GT) initialization failure", nullptr, nullptr);

  // not executed, only to force linking hb_GTSYS()
  HB_FUNC_EXEC(HB_GTSYS);
}

HB_GT_ANNOUNCE(HB_GT_NAME)

static HB_GARBAGE_FUNC(hb_gt_Destructor)
{
  auto gtHolder = static_cast<void **>(Cargo);

  if (*gtHolder) {
    hb_gtRelease(*gtHolder);
    *gtHolder = nullptr;
  }
}

static HB_GARBAGE_FUNC(hb_gt_Mark)
{
  auto gtHolder = static_cast<void **>(Cargo);

  if (*gtHolder) {
    HB_GTSELF_MARK(static_cast<PHB_GT>(*gtHolder));
  }
}

static const HB_GC_FUNCS s_gcGTFuncs = {hb_gt_Destructor, hb_gt_Mark};

static void *hb_gtParam(int iParam)
{
  auto gtHolder = static_cast<void **>(hb_parptrGC(&s_gcGTFuncs, iParam));

  if (gtHolder && *gtHolder) {
    return *gtHolder;
  }

  hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  return nullptr;
}

PHB_GT hb_gt_ItemBase(PHB_ITEM pItemGT)
{
  auto gtHolder = static_cast<void **>(hb_itemGetPtrGC(pItemGT, &s_gcGTFuncs));

  if (gtHolder && *gtHolder) {
    auto pGT = static_cast<PHB_GT>(*gtHolder);
    if (HB_GTSELF_LOCK(pGT)) {
      return pGT;
    }
  }
  return nullptr;
}

HB_FUNC(HB_GTRELOAD)
{
  hb_retl(hb_gtReload(hb_parc(1), HB_ISNUM(2) ? hb_numToHandle(hb_parnint(1)) : HB_STDIN_HANDLE,
                      HB_ISNUM(3) ? hb_numToHandle(hb_parnint(2)) : HB_STDOUT_HANDLE,
                      HB_ISNUM(4) ? hb_numToHandle(hb_parnint(3)) : HB_STDERR_HANDLE));
}

HB_FUNC(HB_GTCREATE)
{
  void *hGT;

  hGT = hb_gtCreate(hb_parc(1), HB_ISNUM(2) ? hb_numToHandle(hb_parnint(1)) : HB_STDIN_HANDLE,
                    HB_ISNUM(3) ? hb_numToHandle(hb_parnint(2)) : HB_STDOUT_HANDLE,
                    HB_ISNUM(4) ? hb_numToHandle(hb_parnint(3)) : HB_STDERR_HANDLE);

  if (hGT) {
    auto gtHolder = static_cast<void **>(hb_gcAllocate(sizeof(void *), &s_gcGTFuncs));
    *gtHolder = hGT;
    hb_retptrGC(gtHolder);
  }
}

HB_FUNC(HB_GTSELECT)
{
  void *hGT;

  if (hb_pcount() > 0) {
    hGT = hb_gtParam(1);
    if (hGT) {
      hGT = hb_gtAlloc(hGT);
      if (hGT) {
        hGT = hb_gtSwap(hGT);
      }
    }
  } else {
    hGT = hb_gtAlloc(nullptr);
  }

  if (hGT) {
    auto gtHolder = static_cast<void **>(hb_gcAllocate(sizeof(void *), &s_gcGTFuncs));
    *gtHolder = hGT;
    hb_retptrGC(gtHolder);
  }
}

HB_FUNC(HB_GTEXISTS)
{
  auto pszGtName = hb_parc(1);
  hb_retl(pszGtName && hb_gt_FindEntry(pszGtName) >= -1);
}

HB_FUNC(HB_GTLIST)
{
  hb_reta(s_iGtCount + 1);
  hb_storvc("NUL", -1, 1);
  for (auto iPos = 0; iPos < s_iGtCount; ++iPos) {
    hb_storvc(s_gtInit[iPos]->id, -1, iPos + 2);
  }
}
