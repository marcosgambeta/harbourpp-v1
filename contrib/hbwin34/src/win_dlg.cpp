//
// Windows dialogs
//
// Copyright 2010 Viktor Szakats (win_PrintDlgDC())
// Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl> (win_GetOpenFileName(), win_GetSaveFileName())
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

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

#include "hbwapi.hpp"

#include <commdlg.h>

// win_PrintDlgDC([@<cDevice>], [<nFromPage>], [<nToPage>], [<nCopies>], [nFlags]) --> <hDC>
HB_FUNC(WIN_PRINTDLGDC)
{
  PRINTDLG pd{};
  pd.lStructSize = sizeof(pd);
  pd.hwndOwner = GetActiveWindow();
  pd.Flags = static_cast<DWORD>(hb_parnl(5)) | PD_RETURNDC | PD_USEDEVMODECOPIESANDCOLLATE;
  pd.nFromPage = static_cast<WORD>(hb_parnidef(2, 1));
  pd.nToPage = static_cast<WORD>(hb_parnidef(3, 1));
  pd.nCopies = static_cast<WORD>(hb_parnidef(4, 1));

  if (PrintDlg(&pd))
  {
    if (pd.hDevNames)
    {
      auto lpdn = static_cast<LPDEVNAMES>(GlobalLock(pd.hDevNames));
      if (lpdn)
      {
        HB_STORSTR(reinterpret_cast<LPCTSTR>(lpdn) + lpdn->wDeviceOffset, 1);
      }
      else
      {
        hb_storc(nullptr, 1);
      }
      GlobalUnlock(pd.hDevNames);
      GlobalFree(pd.hDevNames);
    }

    if (pd.hDevMode)
    {
      GlobalFree(pd.hDevMode);
    }

    hbwapi_ret_HDC(pd.hDC);
  }
  else
  {
    hb_retptr(nullptr);
  }
}

static LPTSTR s_dialogPairs(int iParam, DWORD *pdwIndex)
{
  auto pItem = hb_param(iParam, Harbour::Item::ARRAY | Harbour::Item::STRING);
  LPTSTR lpStr = nullptr;
  DWORD dwMaxIndex = 0;

  if (pItem)
  {
    HB_SIZE nLen, n, n1;

    if (pItem->isArray())
    {
      HB_SIZE nSize, n2;
      PHB_ITEM pArrItem;

      nSize = hb_arrayLen(pItem);
      for (n = nLen = 0; n < nSize; ++n)
      {
        pArrItem = hb_arrayGetItemPtr(pItem, n + 1);
        if (pArrItem->isString())
        {
          n1 = HB_ITEMCOPYSTR(pArrItem, nullptr, 0);
          if (n1)
          {
            nLen += n1 * 2 + 2;
          }
        }
        else if (hb_arrayLen(pArrItem) >= 2)
        {
          n1 = HB_ITEMCOPYSTR(hb_arrayGetItemPtr(pArrItem, 1), nullptr, 0);
          n2 = HB_ITEMCOPYSTR(hb_arrayGetItemPtr(pArrItem, 2), nullptr, 0);
          if (n1 && n2)
          {
            nLen += n1 + n2 + 2;
          }
        }
      }
      if (nLen)
      {
        HB_SIZE nTotal = nLen + 1;
        lpStr = static_cast<LPTSTR>(hb_xgrab(nTotal * sizeof(TCHAR)));
        for (n = nLen = 0; n < nSize; ++n)
        {
          pArrItem = hb_arrayGetItemPtr(pItem, n + 1);
          if (pArrItem->isString())
          {
            n1 = HB_ITEMCOPYSTR(pArrItem, lpStr + nLen, nTotal - nLen);
            if (n1)
            {
              nLen += n1 + 1;
              n1 = HB_ITEMCOPYSTR(pArrItem, lpStr + nLen, nTotal - nLen);
              nLen += n1 + 1;
              dwMaxIndex++;
            }
          }
          else if (hb_arrayLen(pArrItem) >= 2)
          {
            n1 = HB_ITEMCOPYSTR(hb_arrayGetItemPtr(pArrItem, 1), lpStr + nLen, nTotal - nLen);
            if (n1)
            {
              n2 = HB_ITEMCOPYSTR(hb_arrayGetItemPtr(pArrItem, 2), lpStr + nLen + n1 + 1, nTotal - nLen - n1 - 1);
              if (n2)
              {
                nLen += n1 + n2 + 2;
                dwMaxIndex++;
              }
            }
          }
        }
        lpStr[nLen] = 0;
      }
    }
    else
    {
      nLen = HB_ITEMCOPYSTR(pItem, nullptr, 0);
      if (nLen)
      {
        lpStr = static_cast<LPTSTR>(hb_xgrab((nLen * 2 + 3) * sizeof(TCHAR)));
        HB_ITEMCOPYSTR(pItem, lpStr, nLen + 1);
        for (n = n1 = 0; n < nLen; ++n)
        {
          if (lpStr[n] == 0)
          {
            ++n1;
            if (lpStr[n + 1] == 0)
            {
              break;
            }
          }
        }
        if (n1 == 0)
        {
          HB_ITEMCOPYSTR(pItem, lpStr + nLen + 1, nLen + 1);
          lpStr[nLen * 2 + 2] = 0;
          dwMaxIndex = 1;
        }
        else
        {
          if (n == nLen && lpStr[n - 1] != 0)
          {
            lpStr[n + 1] = 0;
            ++n1;
          }
          if ((n1 & 1) == 0)
          {
            dwMaxIndex = static_cast<DWORD>(n1);
          }
          else
          {
            hb_xfree(lpStr);
            lpStr = nullptr;
          }
        }
      }
    }
  }

  if (pdwIndex)
  {
    if (dwMaxIndex < *pdwIndex)
    {
      *pdwIndex = dwMaxIndex;
    }
    else if (dwMaxIndex && *pdwIndex == 0)
    {
      *pdwIndex = 1;
    }
  }

  return lpStr;
}

static void s_GetFileName(HB_BOOL fSave)
{
  void *hInitDir, *hTitle, *hDefExt;
  LPTSTR lpstrFilter;
  OPENFILENAME ofn{};

#if defined(OPENFILENAME_SIZE_VERSION_400)
  ofn.lStructSize = OPENFILENAME_SIZE_VERSION_400;
#else
  ofn.lStructSize = sizeof(ofn);
#endif
  ofn.hwndOwner = GetActiveWindow();
  ofn.hInstance = GetModuleHandle(nullptr);

  ofn.nFilterIndex = hbwapi_par_DWORD(6);
  ofn.lpstrFilter = lpstrFilter = s_dialogPairs(5, &ofn.nFilterIndex);

  ofn.nMaxFile = hbwapi_par_DWORD(7);
  if (ofn.nMaxFile < 0x400)
  {
    ofn.nMaxFile = ofn.nMaxFile == 0 ? 0x10000 : 0x400;
  }
  ofn.lpstrFile = static_cast<LPTSTR>(hb_xgrabz(ofn.nMaxFile * sizeof(TCHAR)));

  ofn.lpstrInitialDir = HB_PARSTR(3, &hInitDir, nullptr);
  ofn.lpstrTitle = HB_PARSTR(2, &hTitle, nullptr);
  ofn.Flags =
      HB_ISNUM(1) ? hbwapi_par_DWORD(1) : (OFN_EXPLORER | OFN_ALLOWMULTISELECT | OFN_HIDEREADONLY | OFN_NOCHANGEDIR);
  ofn.lpstrDefExt = HB_PARSTR(4, &hDefExt, nullptr);
  if (ofn.lpstrDefExt && ofn.lpstrDefExt[0] == '.')
  {
    ++ofn.lpstrDefExt;
  }

  HB_ITEMCOPYSTR(hb_param(8, Harbour::Item::ANY), ofn.lpstrFile, ofn.nMaxFile);

  if (fSave ? GetSaveFileName(&ofn) : GetOpenFileName(&ofn))
  {
    HB_SIZE nLen;
    for (nLen = 0; nLen < ofn.nMaxFile; ++nLen)
    {
      if (ofn.lpstrFile[nLen] == 0 && (nLen + 1 == ofn.nMaxFile || ofn.lpstrFile[nLen + 1] == 0))
      {
        break;
      }
    }
    hb_stornint(ofn.Flags, 1);
    hb_stornint(ofn.nFilterIndex, 6);
    HB_RETSTRLEN(ofn.lpstrFile, nLen);
  }
  else
  {
    hb_retc_null();
  }

  hb_xfree(ofn.lpstrFile);
  if (lpstrFilter)
  {
    hb_xfree(lpstrFilter);
  }

  hb_strfree(hInitDir);
  hb_strfree(hTitle);
  hb_strfree(hDefExt);
}

// win_GetOpenFileName([[@]<nFlags>], [<cTitle>], [<cInitDir>], [<cDefExt>], ;
//                     [<acFilter>], [[@]<nFilterIndex>], [<nBufferSize>], [<cDefName>])
//    --> <cFilePath> | <cPath> + e"\0" + <cFile1> [ + e"\0" + <cFileN> ] | ""
HB_FUNC(WIN_GETOPENFILENAME)
{
  s_GetFileName(false);
}

// win_GetSaveFileName([[@]<nFlags>], [<cTitle>], [<cInitDir>], [<cDefExt>], ;
//                     [<acFilter>], [[@]<nFilterIndex>], [<nBufferSize>], [<cDefName>])
//    --> <cFilePath> | <cPath> + e"\0" + <cFile1> [ + e"\0" + <cFileN> ] | ""
HB_FUNC(WIN_GETSAVEFILENAME)
{
  s_GetFileName(true);
}
