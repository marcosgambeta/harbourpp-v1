/*
 * Printing subsystem for Windows using GUI printing
 *
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software and Systems Ltd
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

#include "hbwin.hpp"
#include "hbwapi.hpp"
#include <hbapifs.hpp>
#include <hbapiitm.hpp>
#include <winspool.h>

HB_FUNC(WIN_CREATEDC)
{
  if (HB_ISCHAR(1))
  {
    void *hDevice;

    HDC hDC = CreateDC(TEXT(""), HB_PARSTR(1, &hDevice, nullptr), nullptr, nullptr);

    hbwapi_ret_HDC(hDC);

    hb_strfree(hDevice);
  }
  else
  {
    hb_retptr(nullptr);
  }
}

HB_FUNC(WIN_STARTDOC)
{
  HDC hDC = hbwapi_par_HDC(1);
  DOCINFO sDoc;
  HB_BOOL bResult = false;

  if (hDC)
  {
    void *hDocName;

    sDoc.cbSize = sizeof(DOCINFO);
    sDoc.lpszDocName = HB_PARSTR(2, &hDocName, nullptr);
    sDoc.lpszOutput = nullptr;
    sDoc.lpszDatatype = nullptr;
    sDoc.fwType = 0;
    bResult = (StartDoc(hDC, &sDoc) > 0);

    hb_strfree(hDocName);
  }

  hb_retl(bResult);
}

HB_FUNC(WIN_ENDDOC)
{
  HB_BOOL bResult = false;
  HDC hDC = hbwapi_par_HDC(1);

  if (hDC)
  {
    if (hb_parl(2))
    {
      bResult = (AbortDoc(hDC) > 0);
    }
    else
    {
      bResult = (EndDoc(hDC) > 0);
    }
  }

  hb_retl(bResult);
}

HB_FUNC(WIN_ABORTDOC)
{
  HDC hDC = hbwapi_par_HDC(1);

  hb_retl(hDC && (AbortDoc(hDC) > 0));
}

/* Compatibility dummy */
HB_FUNC(WIN_DELETEDC)
{
  hb_retni(0);
}

HB_FUNC(WIN_STARTPAGE)
{
  HDC hDC = hbwapi_par_HDC(1);

  hb_retl(hDC && StartPage(hDC) > 0);
}

HB_FUNC(WIN_ENDPAGE)
{
  HDC hDC = hbwapi_par_HDC(1);

  hb_retl(hDC && EndPage(hDC) > 0);
}

HB_FUNC(WIN_TEXTOUT)
{
  long lResult = 0;
  HDC hDC = hbwapi_par_HDC(1);

  if (hDC && HB_ISCHAR(4))
  {
    HB_SIZE nLen = hb_parnl(5);

    void *hData;
    HB_SIZE nDataLen;
    LPCTSTR lpData = HB_PARSTR(4, &hData, &nDataLen);

    if (nLen > nDataLen)
    {
      nLen = nDataLen;
    }

    if (nLen > 0)
    {
      SIZE sSize;

      auto iRow = hb_parni(2);
      auto iCol = hb_parni(3);
      auto iWidth = hb_parni(6); /* defaults to 0 */

      if (HB_ISNUM(7))
      {
        SetTextAlign(static_cast<HDC>(hDC), TA_NOUPDATECP | hb_parni(7));
      }

      if (iWidth < 0 && nLen < 1024)
      {
        auto n = static_cast<int>(nLen);
        int aFixed[1024];

        iWidth = -iWidth;

        while (n)
        {
          aFixed[--n] = iWidth;
        }

        if (ExtTextOut(hDC, iRow, iCol, 0, nullptr, lpData, static_cast<UINT>(nLen), aFixed))
        {
          lResult = static_cast<long>(nLen * iWidth);
        }
      }
      else if (ExtTextOut(hDC, iRow, iCol, 0, nullptr, lpData, static_cast<UINT>(nLen), nullptr))
      {
        GetTextExtentPoint32(hDC, lpData, static_cast<int>(nLen),
                             &sSize);          /* Get the length of the text in device size */
        lResult = static_cast<long>(sSize.cx); /* return the width so we can update the current pen position (::PosY) */
      }
    }

    hb_strfree(hData);
  }

  hb_retnl(lResult);
}

HB_FUNC(WIN_GETTEXTSIZE)
{
  long lResult = 0;
  HDC hDC = hbwapi_par_HDC(1);

  if (hDC && HB_ISCHAR(2))
  {
    HB_SIZE nLen = hb_parnl(3);

    void *hData;
    HB_SIZE nDataLen;
    LPCTSTR lpData = HB_PARSTR(2, &hData, &nDataLen);

    if (nLen > nDataLen)
    {
      nLen = nDataLen;
    }

    if (nLen > 0)
    {
      SIZE sSize;

      GetTextExtentPoint32(hDC, lpData, static_cast<int>(nLen), &sSize); /* Get the length of the text in device size */

      if (hb_parldef(4, true))
      {
        lResult = static_cast<long>(sSize.cx); /* return the width */
      }
      else
      {
        lResult = static_cast<long>(sSize.cy); /* return the height */
      }
    }

    hb_strfree(hData);
  }

  hb_retnl(lResult);
}

HB_FUNC(WIN_GETCHARSIZE)
{
  long lResult = 0;
  HDC hDC = hbwapi_par_HDC(1);

  if (hDC)
  {
    TEXTMETRIC tm;

    GetTextMetrics(hDC, &tm);
    if (hb_parl(2))
    {
      lResult = static_cast<long>(tm.tmHeight);
    }
    else
    {
      lResult = static_cast<long>(tm.tmAveCharWidth);
    }
  }

  hb_retnl(lResult);
}

HB_FUNC(WIN_GETDEVICECAPS)
{
  HDC hDC = hbwapi_par_HDC(1);

  hb_retni(hDC && HB_ISNUM(2) ? static_cast<long>(GetDeviceCaps(hDC, hb_parni(2))) : 0);
}

HB_FUNC(WIN_SETMAPMODE)
{
  HDC hDC = hbwapi_par_HDC(1);

  hb_retni(hDC && HB_ISNUM(2) ? SetMapMode(hDC, hb_parni(2)) : 0);
}

HB_FUNC(WIN_CREATEFONT)
{
  HDC hDC = hbwapi_par_HDC(1);

  if (hDC)
  {
    LOGFONT lf;
    HFONT hFont;
    int iHeight;
    int iWidth;
    auto iWeight = hb_parni(6);

    void *hfFaceName;
    LPCTSTR pfFaceName;
    HB_SIZE nLen;

    iWeight = iWeight > 0 ? iWeight : FW_NORMAL;

    if (hb_parl(10))
    { /* Ugly hack to enable full control for caller */
      iHeight = hb_parni(3);
      iWidth = hb_parni(5);
    }
    else
    {
      auto iMul = hb_parni(4);
      auto iDiv = hb_parni(5);

      iHeight = -MulDiv(hb_parni(3), GetDeviceCaps(hDC, LOGPIXELSY), 72);

      if (iDiv)
      {
        iWidth = MulDiv(abs(iMul), GetDeviceCaps(hDC, LOGPIXELSX), abs(iDiv));
      }
      else
      {
        iWidth = 0; /* Use the default font width */
      }
    }

    lf.lfHeight = static_cast<LONG>(iHeight);
    lf.lfWidth = static_cast<LONG>(iWidth);
    lf.lfEscapement = 0;
    lf.lfOrientation = 0;
    lf.lfWeight = static_cast<LONG>(iWeight);
    lf.lfItalic = static_cast<BYTE>(hb_parl(8));
    lf.lfUnderline = static_cast<BYTE>(hb_parl(7));
    lf.lfStrikeOut = static_cast<BYTE>(0);
    lf.lfCharSet = static_cast<BYTE>(hb_parnl(9));
    lf.lfOutPrecision = static_cast<BYTE>(OUT_DEVICE_PRECIS);
    lf.lfClipPrecision = static_cast<BYTE>(CLIP_DEFAULT_PRECIS);
    lf.lfQuality = static_cast<BYTE>(DRAFT_QUALITY);
    lf.lfPitchAndFamily = static_cast<BYTE>(DEFAULT_PITCH | FF_DONTCARE);

    pfFaceName = HB_PARSTR(2, &hfFaceName, &nLen);

    if (nLen > (LF_FACESIZE - 1))
    {
      nLen = LF_FACESIZE - 1;
    }

    memcpy(lf.lfFaceName, pfFaceName, nLen * sizeof(TCHAR));
    lf.lfFaceName[nLen] = TEXT('\0');

    hb_strfree(hfFaceName);

    hFont = CreateFontIndirect(&lf);

    hbwapi_ret_HFONT(hFont);

    if (hFont)
    {
      SelectObject(hDC, hFont);
    }
  }
  else
  {
    hb_retptr(nullptr);
  }
}

HB_FUNC(WIN_GETPRINTERFONTNAME)
{
  HDC hDC = hbwapi_par_HDC(1);

  if (hDC)
  {
    TCHAR tszFontName[128];

    GetTextFace(hDC, HB_SIZEOFARRAY(tszFontName) - 1, tszFontName);

    HB_RETSTR(tszFontName);
  }
  else
  {
    hb_retc_null();
  }
}

HB_FUNC(WIN_BITMAPSOK)
{
  HDC hDC = hbwapi_par_HDC(1);

  hb_retl(hDC && (GetDeviceCaps(hDC, RASTERCAPS) & RC_STRETCHDIB));
}

HB_FUNC(WIN_SETDOCUMENTPROPERTIES)
{
  HB_BOOL bResult = false;

  HDC hDC = hbwapi_par_HDC(1);

  if (hDC)
  {
    HANDLE hPrinter;
    void *hDeviceName;
    LPCTSTR lpDeviceName = HB_PARSTR(2, &hDeviceName, nullptr);

    if (OpenPrinter(const_cast<LPTSTR>(lpDeviceName), &hPrinter, nullptr))
    {
      LONG lSize = DocumentProperties(0, hPrinter, const_cast<LPTSTR>(lpDeviceName), nullptr, nullptr, 0);

      if (lSize > 0)
      {
        auto pDevMode = static_cast<PDEVMODE>(hb_xgrabz(lSize));

        if (DocumentProperties(0, hPrinter, const_cast<LPTSTR>(lpDeviceName), pDevMode, pDevMode, DM_OUT_BUFFER) ==
            IDOK)
        {
          DWORD dmFields = 0, fMode;
          HB_BOOL fUserDialog;
          int iProp, iProp2;

          fUserDialog = HB_ISBYREF(3) || HB_ISBYREF(4) || HB_ISBYREF(5) || HB_ISBYREF(6) || HB_ISBYREF(7) ||
                        HB_ISBYREF(8) || HB_ISBYREF(9) || HB_ISBYREF(10);

          if ((iProp = hb_parni(3)) != 0)
          { /* [2007-02-22] don't change if 0 */
            pDevMode->dmPaperSize = static_cast<short>(iProp);
            dmFields |= DM_PAPERSIZE;
          }

          if (HB_ISLOG(4))
          {
            pDevMode->dmOrientation = static_cast<short>(hb_parl(4) ? DMORIENT_LANDSCAPE : DMORIENT_PORTRAIT);
            dmFields |= DM_ORIENTATION;
          }

          if ((iProp = hb_parni(5)) > 0)
          {
            pDevMode->dmCopies = static_cast<short>(iProp);
            dmFields |= DM_COPIES;
          }

          if ((iProp = hb_parni(6)) != 0)
          { /* [2007-02-22] don't change if 0 */
            pDevMode->dmDefaultSource = static_cast<short>(iProp);
            dmFields |= DM_DEFAULTSOURCE;
          }

          if ((iProp = hb_parni(7)) != 0)
          { /* [2007-02-22] don't change if 0 */
            pDevMode->dmDuplex = static_cast<short>(iProp);
            dmFields |= DM_DUPLEX;
          }

          if ((iProp = hb_parni(8)) != 0)
          { /* [2007-02-22] don't change if 0 */
            pDevMode->dmPrintQuality = static_cast<short>(iProp);
            dmFields |= DM_PRINTQUALITY;
          }

          if (pDevMode->dmPaperSize == DMPAPER_USER && (iProp = hb_parni(9)) > 0 && (iProp2 = hb_parni(10)) > 0)
          {
            pDevMode->dmPaperLength = static_cast<short>(iProp);
            pDevMode->dmPaperWidth = static_cast<short>(iProp2);
            dmFields |= DM_PAPERLENGTH | DM_PAPERWIDTH;
          }

          pDevMode->dmFields = dmFields;

          fMode = DM_IN_BUFFER | DM_OUT_BUFFER;
          if (fUserDialog)
          {
            fMode |= DM_IN_PROMPT;
          }

          if (DocumentProperties(0, hPrinter, const_cast<LPTSTR>(lpDeviceName), pDevMode, pDevMode, fMode) == IDOK)
          {
            hb_storni(pDevMode->dmPaperSize, 3);
            hb_storl(pDevMode->dmOrientation == DMORIENT_LANDSCAPE, 4);
            hb_storni(pDevMode->dmCopies, 5);
            hb_storni(pDevMode->dmDefaultSource, 6);
            hb_storni(pDevMode->dmDuplex, 7);
            hb_storni(pDevMode->dmPrintQuality, 8);
            hb_storni(pDevMode->dmPaperLength, 9);
            hb_storni(pDevMode->dmPaperWidth, 10);

            bResult = (ResetDC(hDC, pDevMode) != nullptr);
          }
        }

        hb_xfree(pDevMode);
      }

      ClosePrinter(hPrinter);
    }

    hb_strfree(hDeviceName);
  }

  hb_retl(bResult);
}

HB_FUNC(WIN_GETDOCUMENTPROPERTIES)
{
  HB_BOOL bResult = false;

  HANDLE hPrinter;
  void *hDeviceName;
  LPCTSTR lpDeviceName = HB_PARSTR(1, &hDeviceName, nullptr);

  if (OpenPrinter(const_cast<LPTSTR>(lpDeviceName), &hPrinter, nullptr))
  {
    LONG lSize = DocumentProperties(0, hPrinter, const_cast<LPTSTR>(lpDeviceName), nullptr, nullptr, 0);

    if (lSize > 0)
    {
      auto pDevMode = static_cast<PDEVMODE>(hb_xgrabz(lSize));

      if (DocumentProperties(0, hPrinter, const_cast<LPTSTR>(lpDeviceName), pDevMode, pDevMode, DM_OUT_BUFFER) == IDOK)
      {
        hb_storni(pDevMode->dmPaperSize, 2);
        hb_storl(pDevMode->dmOrientation == DMORIENT_LANDSCAPE, 3);
        hb_storni(pDevMode->dmCopies, 4);
        hb_storni(pDevMode->dmDefaultSource, 5);
        hb_storni(pDevMode->dmDuplex, 6);
        hb_storni(pDevMode->dmPrintQuality, 7);
        hb_storni(pDevMode->dmPaperLength, 8);
        hb_storni(pDevMode->dmPaperWidth, 9);
        bResult = true;
      }

      hb_xfree(pDevMode);
    }

    ClosePrinter(hPrinter);
  }

  hb_strfree(hDeviceName);

  hb_retl(bResult);
}

static int CALLBACK FontEnumCallBack(LOGFONT *lplf, TEXTMETRIC *lpntm, DWORD dwFontType, LPARAM pArray)
{
  auto pSubItems = hb_itemArrayNew(4);

  HB_ARRAYSETSTR(pSubItems, 1, lplf->lfFaceName);
  hb_arraySetL(pSubItems, 2, (lplf->lfPitchAndFamily & FIXED_PITCH) != 0);
  hb_arraySetL(pSubItems, 3, (dwFontType & TRUETYPE_FONTTYPE) != 0);
  hb_arraySetNL(pSubItems, 4, lpntm->tmCharSet);
  hb_arrayAddForward(reinterpret_cast<PHB_ITEM>(pArray), pSubItems);

  hb_itemRelease(pSubItems);

  return 1;
}

HB_FUNC(WIN_ENUMFONTS)
{
  HDC hDC = hbwapi_par_HDC(1);
  HB_BOOL fNullDC = (!hDC);
  auto pArray = hb_itemArrayNew(0);

  if (fNullDC)
  {
    hDC = GetDC(nullptr);
  }

  EnumFonts(hDC, static_cast<LPCTSTR>(nullptr), reinterpret_cast<FONTENUMPROC>(FontEnumCallBack),
            reinterpret_cast<LPARAM>(pArray));

  if (fNullDC)
  {
    ReleaseDC(nullptr, hDC);
  }

  hb_itemReturnRelease(pArray);
}

HB_FUNC(WIN_ENUMFONTFAMILIES)
{
  auto pArray = hb_itemArrayNew(0);
  HDC hDC = hbwapi_par_HDC(1);
  HB_BOOL fNullDC = (!hDC);
  LOGFONT Logfont{};

  Logfont.lfCharSet = static_cast<BYTE>(hb_parnidef(1, DEFAULT_CHARSET));
  if (HB_ISCHAR(2))
  {
    void *hText;
    HB_STRNCPY(Logfont.lfFaceName, HB_PARSTR(2, &hText, nullptr), HB_SIZEOFARRAY(Logfont.lfFaceName) - 1);
    hb_strfree(hText);
  }

  if (fNullDC)
  {
    hDC = GetDC(nullptr);
  }

  EnumFontFamiliesEx(hDC, &Logfont, reinterpret_cast<FONTENUMPROC>(FontEnumCallBack), reinterpret_cast<LPARAM>(pArray),
                     0);

  if (fNullDC)
  {
    ReleaseDC(nullptr, hDC);
  }

  hb_itemReturnRelease(pArray);
}

HB_FUNC(WIN_SETCOLOR)
{
  HDC hDC = hbwapi_par_HDC(1);

  if (hDC)
  {
    if (HB_ISNUM(2))
    {
      hb_retnl(static_cast<long>(SetTextColor(hDC, static_cast<COLORREF>(hb_parnl(2)))));
    }
    else
    {
      hb_retnl(static_cast<long>(GetTextColor(hDC)));
    }

    if (HB_ISNUM(3))
    {
      SetBkColor(hDC, static_cast<COLORREF>(hb_parnl(3)));
    }

    if (HB_ISNUM(4))
    {
      SetTextAlign(hDC, hb_parni(4));
    }
  }
  else
  {
    hb_retnl(static_cast<long>(CLR_INVALID));
  }
}

HB_FUNC(WIN_SETPEN)
{
  HDC hDC = hbwapi_par_HDC(1);

  if (hDC)
  {
    HPEN hPen;

    if (HB_ISPOINTER(2))
    {
      hPen = hbwapi_par_HPEN(2);
    }
    else
    {
      hPen = CreatePen(hb_parni(2) /* pen style */, hb_parni(3) /* pen width */,
                       static_cast<COLORREF>(hb_parnl(4)) /* pen color */);

      hbwapi_ret_HPEN(hPen);
    }

    if (hPen)
    {
      SelectObject(hDC, hPen);
    }
  }
  else
  {
    hb_retptr(nullptr);
  }
}

HB_FUNC(WIN_FILLRECT)
{
  HDC hDC = hbwapi_par_HDC(1);
  HB_BOOL fResult = false;

  if (hDC)
  {
    auto hBrush = CreateSolidBrush(static_cast<COLORREF>(hb_parnl(6)));
    RECT rct;

    rct.left = hb_parnl(2);
    rct.top = hb_parnl(3);
    rct.right = hb_parnl(4);
    rct.bottom = hb_parnl(5);

    if (FillRect(hDC, &rct, hBrush))
    {
      fResult = true;
    }

    DeleteObject(hBrush);
  }
  hb_retl(fResult);
}

HB_FUNC(WIN_LINETO)
{
  HDC hDC = hbwapi_par_HDC(1);

  hb_retl(hDC ? MoveToEx(hDC, hb_parni(2) /* x1 */, hb_parni(3) /* y1 */, nullptr) &&
                    LineTo(hDC, hb_parni(4) /* x2 */, hb_parni(5) /* y2 */)
              : false);
}

HB_FUNC(WIN_RECTANGLE)
{
  HDC hDC = hbwapi_par_HDC(1);
  auto x1 = hb_parni(2);
  auto y1 = hb_parni(3);
  auto x2 = hb_parni(4);
  auto y2 = hb_parni(5);
  auto iWidth = hb_parni(6);
  auto iHeight = hb_parni(7);

  if (iWidth && iHeight)
  {
    hb_retl(hDC ? RoundRect(hDC, x1, y1, x2, y2, iWidth, iHeight) : false);
  }
  else
  {
    hb_retl(hDC ? Rectangle(hDC, x1, y1, x2, y2) : false);
  }
}

HB_FUNC(WIN_ARC)
{
  HDC hDC = hbwapi_par_HDC(1);

  hb_retl(hDC ? Arc(hDC /* hDC */, hb_parni(2) /* x1 */, hb_parni(3) /* y1 */, hb_parni(4) /* x2 */,
                    hb_parni(5) /* y2 */, 0, 0, 0, 0)
              : false);
}

HB_FUNC(WIN_ELLIPSE)
{
  HDC hDC = hbwapi_par_HDC(1);

  hb_retl(hDC ? Ellipse(hDC /* hDC */, hb_parni(2) /* x1 */, hb_parni(3) /* y1 */, hb_parni(4) /* x2 */,
                        hb_parni(5) /* y2 */)
              : false);
}

HB_FUNC(WIN_SETBKMODE)
{
  HDC hDC = hbwapi_par_HDC(1);
  int iMode = 0;

  if (hDC)
  {
    if (HB_ISNUM(2))
    {
      iMode = SetBkMode(hDC, hb_parni(2));
    }
    else
    {
      iMode = GetBkMode(hDC);
    }
  }
  hb_retni(iMode);
}
