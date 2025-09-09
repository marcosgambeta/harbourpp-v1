//
// Windows UNICODE conversion functions
//
// Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.hpp"

#if defined(HB_OS_WIN)

#include <windows.h>

static HB_SIZE hb_wcnlen(const wchar_t *szText, HB_SIZE nCount)
{
  HB_SIZE nLen = 0;

  while (nCount-- && szText[nLen]) {
    ++nLen;
  }

  return nLen;
}

int hb_wctomblen(const wchar_t *szText)
{
  return WideCharToMultiByte(CP_ACP, 0, szText, -1, nullptr, 0, nullptr, nullptr) - 1;
}

void hb_wcntombcpy(char *dstA, const wchar_t *srcW, HB_SIZE nLen)
{
  WideCharToMultiByte(CP_ACP, 0, srcW, -1, dstA, static_cast<int>(nLen), nullptr, nullptr);
  dstA[static_cast<int>(nLen)] = '\0';
}

void hb_mbntowccpy(wchar_t *dstW, const char *srcA, HB_SIZE nLen)
{
  MultiByteToWideChar(CP_ACP, 0, srcA, -1, dstW, static_cast<int>(nLen));
  dstW[static_cast<int>(nLen)] = L'\0';
}

wchar_t *hb_mbtowc(const char *srcA)
{
  int length = MultiByteToWideChar(CP_ACP, 0, srcA, -1, nullptr, 0);
  auto dstW = static_cast<wchar_t *>(hb_xgrab(length * sizeof(wchar_t)));
  MultiByteToWideChar(CP_ACP, 0, srcA, -1, dstW, length);
  return dstW;
}

char *hb_wctomb(const wchar_t *srcW)
{
  int length = WideCharToMultiByte(CP_ACP, 0, srcW, -1, nullptr, 0, nullptr, nullptr);
  auto dstA = static_cast<char *>(hb_xgrab(length));
  WideCharToMultiByte(CP_ACP, 0, srcW, -1, dstA, length, nullptr, nullptr);
  return dstA;
}

wchar_t *hb_mbntowc(const char *srcA, HB_SIZE nLen)
{
  nLen = hb_strnlen(srcA, nLen);
  int length = MultiByteToWideChar(CP_ACP, 0, srcA, static_cast<int>(nLen), nullptr, 0);
  auto dstW = static_cast<wchar_t *>(hb_xgrab((length + 1) * sizeof(wchar_t)));
  MultiByteToWideChar(CP_ACP, 0, srcA, static_cast<int>(nLen), dstW, length);
  dstW[length] = L'\0';
  return dstW;
}

char *hb_wcntomb(const wchar_t *srcW, HB_SIZE nLen)
{
  nLen = hb_wcnlen(srcW, nLen);
  int length = WideCharToMultiByte(CP_ACP, 0, srcW, static_cast<int>(nLen), nullptr, 0, nullptr, nullptr);
  auto dstA = static_cast<char *>(hb_xgrab(length + 1));
  WideCharToMultiByte(CP_ACP, 0, srcW, static_cast<int>(nLen), dstA, length, nullptr, nullptr);
  dstA[length] = '\0';
  return dstA;
}

#endif // HB_OS_WIN
