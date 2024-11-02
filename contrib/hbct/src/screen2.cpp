/*
 * CT3 video functions:
 * SayDown(), SaySpread(), SayMoveIn(), ScreenStr(), StrScreen()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

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

#include <hbapigt.hpp>
#include <hbapistr.hpp>
#include <hbdate.hpp>

HB_FUNC(SAYDOWN)
{
  auto nLen = hb_parclen(1);

  if (nLen)
  {
    int iRow, iCol, iMaxRow, iMaxCol;
    long lDelay = hb_parnldef(2, 4);

    hb_gtGetPos(&iRow, &iCol);
    if (HB_ISNUM(3))
    {
      iRow = hb_parni(3);
    }
    if (HB_ISNUM(4))
    {
      iCol = hb_parni(4);
    }
    iMaxRow = hb_gtMaxRow();
    iMaxCol = hb_gtMaxCol();

    if (iRow >= 0 && iCol >= 0 && iRow <= iMaxRow && iCol <= iMaxCol)
    {
      auto szText = hb_parc(1);
      auto nTextLen = hb_parclen(1);

      HB_WCHAR wc;
      PHB_CODEPAGE cdp = hb_gtHostCP();
      HB_SIZE nIndex = 0;

      int iColor = hb_gtGetCurrColor();

      if (nLen > static_cast<HB_SIZE>(iMaxRow - iRow + 1))
      {
        nLen = static_cast<HB_SIZE>(iMaxRow - iRow + 1);
      }

      hb_gtBeginWrite();
      while (nLen--)
      {
        if (HB_CDPCHAR_GET(cdp, szText, nTextLen, &nIndex, &wc))
        {
          hb_gtPutChar(iRow++, iCol, iColor, 0, wc);
        }
        else
        {
          break;
        }

        if (lDelay)
        {
          hb_gtEndWrite();
          hb_idleSleep(static_cast<double>(lDelay) / 1000);
          hb_gtBeginWrite();
        }
      }
      hb_gtEndWrite();
    }
  }

  hb_retc_null();
}

HB_FUNC(SAYSPREAD)
{
  HB_SIZE nLen;
  void *hText;
  const HB_WCHAR *pwText = hb_parstr_u16(1, HB_CDP_ENDIAN_NATIVE, &hText, &nLen);

  if (nLen)
  {
    int iRow, iCol, iMaxRow, iMaxCol;
    long lDelay;

    lDelay = hb_parnldef(2, 4);

    iMaxRow = hb_gtMaxRow();
    iMaxCol = hb_gtMaxCol();
    hb_gtGetPos(&iRow, &iCol);
    if (HB_ISNUM(3))
    {
      iRow = hb_parni(3);
    }
    else
    {
      hb_gtGetPos(&iRow, &iCol);
    }
    iCol = HB_ISNUM(4) ? hb_parni(4) : (iMaxCol >> 1);

    if (iRow >= 0 && iCol >= 0 && iRow <= iMaxRow && iCol <= iMaxCol)
    {
      HB_SIZE nPos;
      int iColor = hb_gtGetCurrColor();

      nPos = nLen >> 1;
      nLen = nLen & 1;
      if (!nLen)
      {
        nLen = 2;
        --nPos;
      }

      hb_gtBeginWrite();
      do
      {
        for (HB_SIZE nPos2 = 0; nPos2 < nLen && iCol + static_cast<int>(nPos2) <= iMaxCol; ++nPos2)
        {
          hb_gtPutChar(iRow, iCol + static_cast<int>(nPos2), iColor, 0, pwText[nPos + nPos2]);
        }
        nLen += 2;
        if (lDelay)
        {
          hb_gtEndWrite();
          hb_idleSleep(static_cast<double>(lDelay) / 1000);
          hb_gtBeginWrite();
        }
      } while (nPos-- && iCol--);
      /* CT3 does not respect iCol in the above condition */
      hb_gtEndWrite();
    }
  }
  hb_strfree(hText);

  hb_retc_null();
}

HB_FUNC(SAYMOVEIN)
{
  HB_SIZE nLen;
  void *hText;
  const HB_WCHAR *pwText = hb_parstr_u16(1, HB_CDP_ENDIAN_NATIVE, &hText, &nLen);

  if (nLen)
  {
    int iRow, iCol, iMaxRow, iMaxCol;
    long lDelay;
    HB_BOOL fBack;

    lDelay = hb_parnldef(2, 4);
    fBack = hb_parl(5);

    iMaxRow = hb_gtMaxRow();
    iMaxCol = hb_gtMaxCol();
    hb_gtGetPos(&iRow, &iCol);
    if (HB_ISNUM(3))
    {
      iRow = hb_parni(3);
    }
    if (HB_ISNUM(4))
    {
      iCol = hb_parni(4);
    }

    if (iRow >= 0 && iCol >= 0 && iRow <= iMaxRow && iCol <= iMaxCol)
    {
      HB_SIZE nChars;
      int iColor = hb_gtGetCurrColor();
      int iNewCol;

      iNewCol = iCol + static_cast<int>(nLen);
      if (fBack)
      {
        iCol += static_cast<int>(nLen) - 1;
      }
      else
      {
        pwText += static_cast<int>(nLen) - 1;
      }
      nChars = 1;

      hb_gtBeginWrite();
      do
      {
        HB_SIZE nPos;

        if (fBack)
        {
          if (iCol <= iMaxCol)
          {
            for (nPos = 0; nPos < nChars; ++nPos)
            {
              hb_gtPutChar(iRow, iCol + static_cast<int>(nPos), iColor, 0, pwText[nPos]);
            }
          }
          --iCol;
        }
        else
        {
          for (nPos = 0; nPos < nChars; ++nPos)
          {
            hb_gtPutChar(iRow, iCol + static_cast<int>(nPos), iColor, 0, pwText[nPos]);
          }
          --pwText;
        }
        if (static_cast<int>(nChars) + iCol <= iMaxCol)
        {
          ++nChars;
        }

        if (lDelay)
        {
          hb_gtEndWrite();
          hb_idleSleep(static_cast<double>(lDelay) / 1000);
          hb_gtBeginWrite();
        }
      } while (--nLen);
      hb_gtSetPos(iRow, iNewCol);
      hb_gtEndWrite();
    }
  }
  hb_strfree(hText);

  hb_retc_null();
}

HB_FUNC(CLEARSLOW) /* TODO: Unicode support */
{
  int iMaxRow = hb_gtMaxRow();
  int iMaxCol = hb_gtMaxCol();
  long lDelay = hb_parnl(1);
  auto iTop = hb_parni(2);
  auto iLeft = hb_parni(3);
  auto iBottom = hb_parnidef(4, iMaxRow);
  auto iRight = hb_parnidef(5, iMaxCol);
  HB_UCHAR ucChar;

  if (HB_ISNUM(6))
  {
    ucChar = static_cast<HB_UCHAR>(hb_parni(6));
  }
  else if (HB_ISCHAR(6))
  {
    ucChar = static_cast<HB_UCHAR>(hb_parc(6)[0]);
  }
  else
  {
    ucChar = static_cast<HB_UCHAR>(hb_gtGetClearChar());
  }

  if (iTop >= 0 && iLeft >= 0 && iTop <= iBottom && iLeft <= iRight)
  {
    char pszFrame[2];
    int iColor = hb_gtGetCurrColor();
    double dX, dY, dXX, dYY;

    pszFrame[0] = static_cast<char>(ucChar);
    pszFrame[1] = '\0';

    dX = iRight - iLeft + 1;
    dY = iBottom - iTop + 1;
    if (dX > dY)
    {
      dY /= dX;
      dX = 1;
    }
    else
    {
      dX /= dY;
      dY = 1;
    }
    dXX = dYY = 0;

    hb_gtBeginWrite();
    for (;;)
    {
      hb_gtBoxEx(iTop, iLeft, iBottom, iRight, pszFrame, iColor);
      if (lDelay)
      {
        hb_gtEndWrite();
        hb_idleSleep(static_cast<double>(lDelay) / 1000);
        hb_gtBeginWrite();
      }

      if (iTop >= iBottom && iLeft >= iRight)
      {
        break;
      }

      if (iTop < iBottom)
      {
        dYY += dY;
        if (dYY >= 1)
        {
          iTop++;
          if (iBottom > iTop)
          {
            iBottom--;
          }
          dYY -= 1;
        }
      }
      if (iLeft < iRight)
      {
        dXX += dX;
        if (dXX >= 1)
        {
          iLeft++;
          if (iRight > iLeft)
          {
            iRight--;
          }
        }
      }
    }
    hb_gtEndWrite();
  }
}

HB_FUNC(SCREENSTR) /* TODO: Unicode support */
{
  int iRow, iCol, iMaxRow, iMaxCol;
  char *pBuffer;
  HB_SIZE nCount = HB_SIZE_MAX;

  hb_gtGetPos(&iRow, &iCol);
  if (HB_ISNUM(1))
  {
    iRow = hb_parni(1);
  }
  if (HB_ISNUM(2))
  {
    iCol = hb_parni(2);
  }
  if (HB_ISNUM(3))
  {
    nCount = hb_parns(3);
  }
  iMaxRow = hb_gtMaxRow();
  iMaxCol = hb_gtMaxCol();

  if (iRow >= 0 && iRow <= iMaxRow && iCol >= 0 && iCol <= iMaxCol && nCount)
  {
    char *szText;
    HB_SIZE nSize = static_cast<HB_SIZE>(iMaxRow - iRow + 1) * (iMaxCol - iCol + 1);
    if (nSize > nCount)
    {
      nSize = nCount;
    }
    nCount = nSize;
    nSize <<= 1;
    szText = pBuffer = static_cast<char *>(hb_xgrab(nSize + 1));
    do
    {
      int iC = iCol;
      do
      {
        int iColor;
        HB_BYTE bAttr;
        HB_USHORT usChar;
        hb_gtGetChar(iRow, iC, &iColor, &bAttr, &usChar);
        *szText++ = static_cast<char>(usChar);
        *szText++ = static_cast<char>(iColor);
      } while (--nCount && ++iC <= iMaxCol);
    } while (nCount && ++iRow <= iMaxRow);

    hb_retclen_buffer(pBuffer, nSize);
  }
  else
  {
    hb_retc_null();
  }
}

HB_FUNC(STRSCREEN) /* TODO: Unicode support */
{
  auto nLen = hb_parclen(1);

  if (nLen & 1)
  {
    nLen--;
  }

  if (nLen)
  {
    auto szText = hb_parc(1);
    int iRow, iCol, iMaxRow, iMaxCol;

    hb_gtGetPos(&iRow, &iCol);
    if (HB_ISNUM(2))
    {
      iRow = hb_parni(2);
    }
    if (HB_ISNUM(3))
    {
      iCol = hb_parni(3);
    }
    iMaxRow = hb_gtMaxRow();
    iMaxCol = hb_gtMaxCol();

    if (iRow >= 0 && iRow <= iMaxRow && iCol >= 0 && iCol <= iMaxCol)
    {
      hb_gtBeginWrite();
      do
      {
        int iC = iCol;
        do
        {
          HB_USHORT usChar = static_cast<HB_UCHAR>(*szText++);
          int iColor = static_cast<HB_UCHAR>(*szText++);
          hb_gtPutChar(iRow, iC, iColor, 0, usChar);
          nLen -= 2;
        } while (nLen && ++iC <= iMaxCol);
      } while (nLen && ++iRow <= iMaxRow);
      hb_gtEndWrite();
    }
  }

  hb_retc_null();
}

HB_FUNC(__HBCT_DSPTIME) /* Helper function for ShowTime() */
{
  int iColor;
  char szTime[10];

  auto iRow = hb_parni(1);
  auto iCol = hb_parni(2);
  if (HB_ISNUM(4))
  {
    iColor = hb_parni(4);
  }
  else if (HB_ISCHAR(4))
  {
    iColor = hb_gtColorToN(hb_parc(4));
    if (iColor == -1)
    {
      iColor = 0;
    }
  }
  else
  {
    iColor = hb_gtGetClearColor();
  }

  hb_dateTimeStr(szTime);
  auto iLen = 8;

  if (hb_parl(3))
  {
    iLen -= 3;
  }

  if (hb_parl(5))
  {
    int iHour = (szTime[0] - '0') * 10 + (szTime[1] - '0');

    if (hb_parl(6))
    {
      szTime[iLen++] = iHour >= 12 ? 'p' : 'a';
    }
    if (iHour > 12)
    {
      iHour -= 12;
    }
    else if (iHour == 0)
    {
      iHour = 12;
    }
    szTime[0] = static_cast<char>(iHour / 10) + '0';
    szTime[1] = static_cast<char>(iHour % 10) + '0';
  }

  if (szTime[0] == '0')
  {
    szTime[0] = ' ';
  }

  hb_gtPutText(iRow, iCol, szTime, iLen, iColor);
}
