//
// The CodePages API
//
// Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
// Copyright 2009-2012 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbapierr.hpp"
#include "hbapicdp.hpp"
#include "hbthread.hpp"

// Note for Harbour++ v2: use only std::mutex
#if defined(HB_USE_CPP_MUTEX)

#include <iostream>
#include <thread>
#include <mutex>

std::mutex cdpMtx;

#define HB_CDP_LOCK() cdpMtx.lock()
#define HB_CDP_UNLOCK() cdpMtx.unlock()

#else

// MT macros
#define HB_CDP_LOCK() hb_threadEnterCriticalSection(&s_cdpMtx)
#define HB_CDP_UNLOCK() hb_threadLeaveCriticalSection(&s_cdpMtx)
static HB_CRITICAL_NEW(s_cdpMtx);

#endif // HB_USE_CPP_MUTEX

constexpr int NUMBER_OF_CHARS = 256; // #define NUMBER_OF_CHARS 256

constexpr HB_WCHAR HB_MAX_CTRL_CODE = 0x266B; // #define HB_MAX_CTRL_CODE 0x266B

static const HB_WCHAR s_uniCtrls[32] = {0x2007, 0x263A, 0x263B, 0x2665, 0x2666, 0x2663, 0x2660, 0x2022,
                                        0x25D8, 0x25CB, 0x25D9, 0x2642, 0x2640, 0x266A, 0x266B, 0x263C,
                                        0x25BA, 0x25C4, 0x2195, 0x203C, 0x00B6, 0x00A7, 0x25AC, 0x21A8,
                                        0x2191, 0x2193, 0x2192, 0x2190, 0x221F, 0x2194, 0x25B2, 0x25BC};

static HB_UCHAR *s_rev_ctrl = nullptr;

static const HB_WCHAR s_uniCodes[NUMBER_OF_CHARS] = {
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027, 0x0028, 0x0029,
    0x002A, 0x002B, 0x002C, 0x002D, 0x002E, 0x002F, 0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037,
    0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F, 0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045,
    0x0046, 0x0047, 0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F, 0x0050, 0x0051, 0x0052, 0x0053,
    0x0054, 0x0055, 0x0056, 0x0057, 0x0058, 0x0059, 0x005A, 0x005B, 0x005C, 0x005D, 0x005E, 0x005F, 0x0060, 0x0061,
    0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067, 0x0068, 0x0069, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F,
    0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077, 0x0078, 0x0079, 0x007A, 0x007B, 0x007C, 0x007D,
    0x007E, 0x007F, 0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7, 0x00EA, 0x00EB, 0x00E8, 0x00EF,
    0x00EE, 0x00EC, 0x00C4, 0x00C5, 0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9, 0x00FF, 0x00D6,
    0x00DC, 0x00A2, 0x00A3, 0x00A5, 0x20A7, 0x0192, 0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1, 0x00AA, 0x00BA,
    0x00BF, 0x2310, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB, 0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561,
    0x2562, 0x2556, 0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510, 0x2514, 0x2534, 0x252C, 0x251C,
    0x2500, 0x253C, 0x255E, 0x255F, 0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567, 0x2568, 0x2564,
    0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B, 0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
    0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4, 0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6,
    0x03B5, 0x2229, 0x2261, 0x00B1, 0x2265, 0x2264, 0x2320, 0x2321, 0x00F7, 0x2248, 0x00B0, 0x2219, 0x00B7, 0x221A,
    0x207F, 0x00B2, 0x25A0, 0x00A0};

HB_UNITABLE hb_uniTbl_437 = {HB_CPID_437, s_uniCodes, nullptr, 0};

static HB_CDP_GET_FUNC(hb_cdpStd_get);
static HB_CDP_PUT_FUNC(hb_cdpStd_put);
static HB_CDP_LEN_FUNC(hb_cdpStd_len);

static HB_CDP_CMP_FUNC(hb_cdpBin_cmp);
static HB_CDP_CMP_FUNC(hb_cdpBin_cmpi);

static HB_CDP_GET_FUNC(hb_cdpUTF8_get);
static HB_CDP_PUT_FUNC(hb_cdpUTF8_put);
static HB_CDP_LEN_FUNC(hb_cdpUTF8_len);

static HB_UNITABLE hb_uniTbl_UTF8 = {HB_CPID_437, s_uniCodes, nullptr, 0};

static HB_UCHAR s_en_buffer[0x300];

// pseudo codepage for translations only
static HB_CODEPAGE s_utf8_codepage = {"UTF8",
                                      "UTF-8",
                                      &hb_uniTbl_UTF8,
                                      nullptr,
                                      nullptr,
                                      nullptr,
                                      nullptr,
                                      nullptr,
                                      0,
                                      HB_CDP_TYPE_CUSTOM | HB_CDP_TYPE_UTF8 | HB_CDP_TYPE_BINSORT,
                                      hb_cdpUTF8_get,
                                      hb_cdpUTF8_put,
                                      hb_cdpUTF8_len,
                                      nullptr,
                                      nullptr,
                                      nullptr,
                                      hb_cdpBin_cmp,
                                      hb_cdpBin_cmpi,
                                      0,
                                      0,
                                      nullptr,
                                      nullptr,
                                      nullptr};

HB_CODEPAGE_ANNOUNCE(UTF8)

static HB_CODEPAGE s_en_codepage = {"EN",          "English CP-437", HB_UNITB_437,    nullptr, nullptr,
                                    nullptr,       nullptr,          nullptr,         0,       HB_CDP_TYPE_BINSORT,
                                    hb_cdpStd_get, hb_cdpStd_put,    hb_cdpStd_len,   nullptr, nullptr,
                                    nullptr,       hb_cdpBin_cmp,    hb_cdpBin_cmpi,  0,       0,
                                    nullptr,       nullptr,          &s_utf8_codepage};

HB_CODEPAGE_ANNOUNCE(EN)

static PHB_CODEPAGE s_cdpList = nullptr;

// conversions
void hb_cdpBuildTransTable(PHB_UNITABLE uniTable)
{
  HB_CDP_LOCK();
  if (uniTable->uniTrans == nullptr)
  {
    HB_WCHAR wcMax = 0;

    for (auto i = 0; i < 256; ++i)
    {
      HB_WCHAR wc = uniTable->uniCodes[i];
      if (wc > wcMax)
      {
        wcMax = wc;
      }
    }
    auto uniTrans = static_cast<HB_UCHAR *>(hb_xgrabz((wcMax + 1) * sizeof(HB_UCHAR)));
    for (auto i = 0; i < 256; ++i)
    {
      if (uniTable->uniCodes[i])
      {
        uniTrans[uniTable->uniCodes[i]] = static_cast<HB_UCHAR>(i);
      }
    }

    uniTable->wcMax = wcMax;
    uniTable->uniTrans = uniTrans;

    if (s_rev_ctrl == nullptr)
    {
      wcMax = HB_MAX_CTRL_CODE;
      s_rev_ctrl = static_cast<HB_UCHAR *>(hb_xgrabz((wcMax + 1) * sizeof(HB_UCHAR)));
      for (auto i = 0; i < 32; ++i)
      {
        s_rev_ctrl[s_uniCtrls[i]] = static_cast<HB_UCHAR>(i);
      }
    }
  }
  HB_CDP_UNLOCK();
}

// standard conversion functions
static HB_BOOL hb_cdpStd_get(PHB_CODEPAGE cdp, const char *pSrc, HB_SIZE nLen, HB_SIZE *pnIndex, HB_WCHAR *wc)
{
  if (*pnIndex < nLen)
  {
    auto uc = static_cast<HB_UCHAR>(pSrc[(*pnIndex)++]);

    *wc = cdp->uniTable->uniCodes[uc];
    if (*wc == 0)
    {
      *wc = uc;
    }

    return true;
  }
  return false;
}

static HB_BOOL hb_cdpStd_put(PHB_CODEPAGE cdp, char *pDst, HB_SIZE nLen, HB_SIZE *pnIndex, HB_WCHAR wc)
{
  if (*pnIndex < nLen)
  {
    if (cdp->uniTable->uniTrans == nullptr)
    {
      hb_cdpBuildTransTable(cdp->uniTable);
    }

    if (wc <= cdp->uniTable->wcMax && cdp->uniTable->uniTrans[wc])
    {
      pDst[(*pnIndex)++] = cdp->uniTable->uniTrans[wc];
    }
    else
    {
      pDst[(*pnIndex)++] = wc >= 0x100 ? '?' : static_cast<HB_UCHAR>(wc);
    }

    return true;
  }
  return false;
}

static int hb_cdpStd_len(PHB_CODEPAGE cdp, HB_WCHAR wc)
{
  HB_SYMBOL_UNUSED(cdp);
  HB_SYMBOL_UNUSED(wc);
  return 1;
}

static int hb_cdpBin_cmp(PHB_CODEPAGE cdp, const char *szFirst, HB_SIZE nLenFirst, const char *szSecond,
                         HB_SIZE nLenSecond, HB_BOOL fExact)
{
  HB_SYMBOL_UNUSED(cdp);

  HB_SIZE nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;
  int iRet = memcmp(szFirst, szSecond, nLen);

  if (iRet == 0)
  {
    if (nLenSecond > nLenFirst)
    {
      iRet = -1;
    }
    else if (fExact && nLenSecond < nLenFirst)
    {
      iRet = 1;
    }
  }
  else if (iRet > 0)
  {
    iRet = 1;
  }
  else
  {
    iRet = -1;
  }

  return iRet;
}

static int hb_cdpBin_cmpi(PHB_CODEPAGE cdp, const char *szFirst, HB_SIZE nLenFirst, const char *szSecond,
                          HB_SIZE nLenSecond, HB_BOOL fExact)
{
  HB_SIZE nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;
  int iRet = 0;

  while (nLen--)
  {
    HB_UCHAR u1 = cdp->upper[static_cast<HB_UCHAR>(*szFirst++)], u2 = cdp->upper[static_cast<HB_UCHAR>(*szSecond++)];
    if (u1 != u2)
    {
      iRet = (u1 < u2) ? -1 : 1;
      break;
    }
  }

  if (iRet == 0)
  {
    if (nLenSecond > nLenFirst)
    {
      iRet = -1;
    }
    else if (fExact && nLenSecond < nLenFirst)
    {
      iRet = 1;
    }
  }

  return iRet;
}

static int hb_cdpStd_cmp(PHB_CODEPAGE cdp, const char *szFirst, HB_SIZE nLenFirst, const char *szSecond,
                         HB_SIZE nLenSecond, HB_BOOL fExact)
{
  int iRet = 0, iAcc = 0, n1, n2;
  HB_SIZE nLen;

  nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;
  for (HB_SIZE nPos = 0; nPos < nLen; ++szFirst, ++szSecond, ++nPos)
  {
    if (*szFirst != *szSecond)
    {
      n1 = static_cast<HB_UCHAR>(cdp->sort[static_cast<HB_UCHAR>(*szFirst)]);
      n2 = static_cast<HB_UCHAR>(cdp->sort[static_cast<HB_UCHAR>(*szSecond)]);
      if (n1 != n2)
      {
        iRet = (n1 < n2) ? -1 : 1;
        break;
      }
      if (iAcc == 0 && (fExact || (nLenFirst == nLenSecond && cdp->acc)))
      {
        if (cdp->acc)
        {
          iAcc = (cdp->acc[static_cast<HB_UCHAR>(*szFirst)] < cdp->acc[static_cast<HB_UCHAR>(*szSecond)]) ? -1 : 1;
        }
        else
        {
          iAcc = (static_cast<HB_UCHAR>(*szFirst) < static_cast<HB_UCHAR>(*szSecond)) ? -1 : 1;
        }
      }
    }
  }

  if (!iRet)
  {
    if (iAcc)
    {
      iRet = iAcc;
    }
    else if (nLenSecond > nLenFirst)
    {
      iRet = -1;
    }
    else if (fExact && nLenSecond < nLenFirst)
    {
      iRet = 1;
    }
  }

  return iRet;
}

static int hb_cdpStd_cmpi(PHB_CODEPAGE cdp, const char *szFirst, HB_SIZE nLenFirst, const char *szSecond,
                          HB_SIZE nLenSecond, HB_BOOL fExact)
{
  int iRet = 0, iAcc = 0;
  HB_SIZE nLen;

  nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;
  for (HB_SIZE nPos = 0; nPos < nLen; ++szFirst, ++szSecond, ++nPos)
  {
    int u1 = cdp->upper[static_cast<HB_UCHAR>(*szFirst)];
    int u2 = cdp->upper[static_cast<HB_UCHAR>(*szSecond)];

    if (u1 != u2)
    {
      int n1 = static_cast<HB_UCHAR>(cdp->sort[u1]);
      int n2 = static_cast<HB_UCHAR>(cdp->sort[u2]);
      if (n1 != n2)
      {
        iRet = (n1 < n2) ? -1 : 1;
        break;
      }
      if (iAcc == 0 && (fExact || (nLenFirst == nLenSecond && cdp->acc)))
      {
        if (cdp->acc)
        {
          iAcc = (cdp->acc[u1] < cdp->acc[u2]) ? -1 : 1;
        }
        else
        {
          iAcc = (u1 < u2) ? -1 : 1;
        }
      }
    }
  }

  if (!iRet)
  {
    if (iAcc)
    {
      iRet = iAcc;
    }
    else if (nLenSecond > nLenFirst)
    {
      iRet = -1;
    }
    else if (fExact && nLenSecond < nLenFirst)
    {
      iRet = 1;
    }
  }

  return iRet;
}

static HB_BOOL hb_cdpUTF8_get(PHB_CODEPAGE cdp, const char *pSrc, HB_SIZE nLen, HB_SIZE *pnIndex, HB_WCHAR *wc)
{
  HB_SYMBOL_UNUSED(cdp);

  HB_SIZE nIndex = *pnIndex;
  int n = 0;

  *wc = 0;
  while (nIndex < nLen)
  {
    if (hb_cdpUTF8ToU16NextChar(static_cast<HB_UCHAR>(pSrc[nIndex]), &n, wc))
    {
      ++nIndex;
    }
    if (n == 0)
    {
      *pnIndex = nIndex;
      return true;
    }
  }
  if (n > 0)
  {
    *pnIndex = nIndex;
    return true;
  }
  return false;
}

static HB_BOOL hb_cdpUTF8_put(PHB_CODEPAGE cdp, char *pDst, HB_SIZE nLen, HB_SIZE *pnIndex, HB_WCHAR wc)
{
  HB_SYMBOL_UNUSED(cdp);

  int i = hb_cdpUTF8CharSize(wc);

  if (*pnIndex + i <= nLen)
  {
    hb_cdpU16CharToUTF8(&pDst[*pnIndex], wc);
    *pnIndex += i;
    return true;
  }
  return false;
}

static int hb_cdpUTF8_len(PHB_CODEPAGE cdp, HB_WCHAR wc)
{
  HB_SYMBOL_UNUSED(cdp);
  return hb_cdpUTF8CharSize(wc);
}

static HB_BOOL hb_cdpMulti_get(PHB_CODEPAGE cdp, const char *pSrc, HB_SIZE nLen, HB_SIZE *pnIndex, HB_WCHAR *wc)
{
  if (*pnIndex < nLen)
  {
    auto uc = static_cast<HB_UCHAR>(pSrc[(*pnIndex)++]);

    *wc = cdp->uniTable->uniCodes[uc];
    if (*wc == 0)
    {
      *wc = uc;
    }
    else if ((cdp->flags[uc] & HB_CDP_MULTI1) != 0 && *pnIndex < nLen)
    {
      auto uc2 = static_cast<HB_UCHAR>(pSrc[*pnIndex + 1]);
      if ((cdp->flags[uc2] & HB_CDP_MULTI2) != 0)
      {
        for (auto i = 0; i < cdp->nMulti; ++i)
        {
          if (uc2 == cdp->multi[i].cLast[0] || uc2 == cdp->multi[i].cLast[1])
          {
            if (uc == cdp->multi[i].cFirst[0])
            {
              if (cdp->multi[i].wcUp)
              {
                *wc = cdp->multi[i].wcUp;
                ++(*pnIndex);
              }
              break;
            }
            else if (uc == cdp->multi[i].cFirst[1])
            {
              if (cdp->multi[i].wcLo)
              {
                *wc = cdp->multi[i].wcLo;
                ++(*pnIndex);
              }
              break;
            }
          }
        }
      }
    }
    return true;
  }
  return false;
}

static HB_BOOL hb_cdpMulti_put(PHB_CODEPAGE cdp, char *pDst, HB_SIZE nLen, HB_SIZE *pnIndex, HB_WCHAR wc)
{
  if (*pnIndex < nLen)
  {
    if (cdp->uniTable->uniTrans == nullptr)
    {
      hb_cdpBuildTransTable(cdp->uniTable);
    }

    if (wc <= cdp->uniTable->wcMax && cdp->uniTable->uniTrans[wc])
    {
      pDst[(*pnIndex)++] = cdp->uniTable->uniTrans[wc];
    }
    else if (wc == 0)
    {
      pDst[(*pnIndex)++] = 0;
    }
    else
    {
      for (auto i = 0; i < cdp->nMulti; ++i)
      {
        if (wc == cdp->multi[i].wcUp)
        {
          pDst[(*pnIndex)++] = cdp->multi[i].cFirst[0];
          if (*pnIndex < nLen)
          {
            pDst[(*pnIndex)++] = cdp->multi[i].cLast[0];
          }
          return true;
        }
        if (wc == cdp->multi[i].wcLo)
        {
          pDst[(*pnIndex)++] = cdp->multi[i].cFirst[1];
          if (*pnIndex < nLen)
          {
            pDst[(*pnIndex)++] = cdp->multi[i].cLast[1];
          }
          return true;
        }
      }
      pDst[(*pnIndex)++] = wc >= 0x100 ? '?' : static_cast<HB_UCHAR>(wc);
    }
    return true;
  }
  return false;
}

static int hb_cdpMulti_len(PHB_CODEPAGE cdp, HB_WCHAR wc)
{
  int n = 1;

  if (wc)
  {
    for (auto i = 0; i < cdp->nMulti; ++i)
    {
      if (wc == cdp->multi[i].wcUp || wc == cdp->multi[i].wcLo)
      {
        ++n;
        break;
      }
    }
  }
  return n;
}

static int hb_cdpMulti_weight(PHB_CODEPAGE cdp, const char *szChar)
{
  PHB_MULTICHAR pmulti = cdp->multi;

  for (int i = cdp->nMulti; i; --i, ++pmulti)
  {
    if ((szChar[0] == pmulti->cFirst[0] || szChar[0] == pmulti->cFirst[1]) &&
        (szChar[1] == pmulti->cLast[0] || szChar[1] == pmulti->cLast[1]))
    {
      return (szChar[0] == pmulti->cFirst[0]) ? pmulti->sortUp : pmulti->sortLo;
    }
  }

  return 0;
}

static int hb_cdpMulti_cmp(PHB_CODEPAGE cdp, const char *szFirst, HB_SIZE nLenFirst, const char *szSecond,
                           HB_SIZE nLenSecond, HB_BOOL fExact)
{
  int iRet = 0, iAcc = 0, n;
  HB_SIZE nLen;

  nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;
  for (HB_SIZE nPos = 0; nPos < nLen; ++szFirst, ++szSecond, ++nPos)
  {
    int n1, n2;

    auto u1 = static_cast<HB_UCHAR>(*szFirst);
    auto u2 = static_cast<HB_UCHAR>(*szSecond);

    n1 = cdp->sort[u1];
    if ((cdp->flags[u1] & HB_CDP_MULTI1) != 0 && (nPos < nLenFirst - 1) &&
        (cdp->flags[static_cast<HB_UCHAR>(szFirst[1])] & HB_CDP_MULTI2) != 0)
    {
      n = hb_cdpMulti_weight(cdp, szFirst);
      if (n != 0)
      {
        n1 = n;
        ++szFirst;
        if (--nLenFirst < nLen)
        {
          nLen = nLenFirst;
        }
      }
    }
    n2 = cdp->sort[u2];
    if ((cdp->flags[u2] & HB_CDP_MULTI1) != 0 && (nPos < nLenSecond - 1) &&
        (cdp->flags[static_cast<HB_UCHAR>(szSecond[1])] & HB_CDP_MULTI2) != 0)
    {
      n = hb_cdpMulti_weight(cdp, szSecond);
      if (n != 0)
      {
        n2 = n;
        ++szSecond;
        if (--nLenSecond < nLen)
        {
          nLen = nLenSecond;
        }
      }
    }
    if (n1 != n2)
    {
      if (n1 == 0 || n2 == 0)
      {
        // One of characters doesn't belong to the national characters
        iRet = (u1 < u2) ? -1 : 1;
      }
      else
      {
        iRet = (n1 < n2) ? -1 : 1;
      }
      break;
    }
    else if (u1 != u2)
    {
      if (n1 == 0)
      {
        iRet = (u1 < u2) ? -1 : 1;
        break;
      }
      if (iAcc == 0 && (fExact || (nLenFirst == nLenSecond && cdp->acc)))
      {
        if (cdp->acc)
        {
          iAcc = (cdp->acc[u1] < cdp->acc[u2]) ? -1 : 1;
        }
        else
        {
          iAcc = (u1 < u2) ? -1 : 1;
        }
      }
    }
  }

  if (!iRet)
  {
    if (iAcc)
    {
      iRet = iAcc;
    }
    else if (nLenSecond > nLenFirst)
    {
      iRet = -1;
    }
    else if (fExact && nLenSecond < nLenFirst)
    {
      iRet = 1;
    }
  }

  return iRet;
}

static int hb_cdpMulti_weightI(PHB_CODEPAGE cdp, const char *szChar)
{
  PHB_MULTICHAR pmulti = cdp->multi;

  for (int i = cdp->nMulti; i; --i, ++pmulti)
  {
    if ((szChar[0] == pmulti->cFirst[0] || szChar[0] == pmulti->cFirst[1]) &&
        (szChar[1] == pmulti->cLast[0] || szChar[1] == pmulti->cLast[1]))
    {
      return pmulti->sortUp;
    }
  }

  return 0;
}

static int hb_cdpMulti_cmpi(PHB_CODEPAGE cdp, const char *szFirst, HB_SIZE nLenFirst, const char *szSecond,
                            HB_SIZE nLenSecond, HB_BOOL fExact)
{
  int iRet = 0, iAcc = 0;
  HB_SIZE nLen;

  nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;
  for (HB_SIZE nPos = 0; nPos < nLen; ++szFirst, ++szSecond, ++nPos)
  {
    int n, u1, u2, n1, n2;

    u1 = cdp->upper[static_cast<HB_UCHAR>(*szFirst)];
    u2 = cdp->upper[static_cast<HB_UCHAR>(*szSecond)];

    if ((cdp->flags[u1] & HB_CDP_MULTI1) != 0 && (nPos < nLenFirst - 1) &&
        (cdp->flags[static_cast<HB_UCHAR>(szFirst[1])] & HB_CDP_MULTI2) != 0)
    {
      n = hb_cdpMulti_weightI(cdp, szFirst);
      if (n != 0)
      {
        n1 = n;
        ++szFirst;
        if (--nLenFirst < nLen)
        {
          nLen = nLenFirst;
        }
      }
      else
      {
        n1 = cdp->sort[u1];
      }
    }
    else
    {
      n1 = cdp->sort[u1];
    }

    if ((cdp->flags[u2] & HB_CDP_MULTI1) != 0 && (nPos < nLenSecond - 1) &&
        (cdp->flags[static_cast<HB_UCHAR>(szSecond[1])] & HB_CDP_MULTI2) != 0)
    {
      n = hb_cdpMulti_weightI(cdp, szSecond);
      if (n != 0)
      {
        n2 = n;
        ++szSecond;
        if (--nLenSecond < nLen)
        {
          nLen = nLenSecond;
        }
      }
      else
      {
        n2 = cdp->sort[u2];
      }
    }
    else
    {
      n2 = cdp->sort[u2];
    }

    if (n1 != n2)
    {
      if (n1 == 0 || n2 == 0)
      {
        // One of characters doesn't belong to the national characters
        iRet = (u1 < u2) ? -1 : 1;
      }
      else
      {
        iRet = (n1 < n2) ? -1 : 1;
      }
      break;
    }
    else if (u1 != u2)
    {
      if (n1 == 0)
      {
        iRet = (u1 < u2) ? -1 : 1;
        break;
      }
      if (iAcc == 0 && (fExact || (nLenFirst == nLenSecond && cdp->acc)))
      {
        if (cdp->acc)
        {
          iAcc = (cdp->acc[u1] < cdp->acc[u2]) ? -1 : 1;
        }
        else
        {
          iAcc = (u1 < u2) ? -1 : 1;
        }
      }
    }
  }

  if (!iRet)
  {
    if (iAcc)
    {
      iRet = iAcc;
    }
    else if (nLenSecond > nLenFirst)
    {
      iRet = -1;
    }
    else if (fExact && nLenSecond < nLenFirst)
    {
      iRet = 1;
    }
  }

  return iRet;
}

// Warning: this functions works only with byte oriented CPs
HB_BOOL hb_cdpIsDigit(PHB_CODEPAGE cdp, int iChar)
{
  return (cdp != nullptr) ? (cdp->flags[iChar & 0x0ff] & HB_CDP_DIGIT) != 0 : HB_ISDIGIT(iChar);
}

HB_BOOL hb_cdpIsAlpha(PHB_CODEPAGE cdp, int iChar)
{
  return (cdp != nullptr) ? (cdp->flags[iChar & 0x0ff] & HB_CDP_ALPHA) != 0 : HB_ISALPHA(iChar);
}

HB_BOOL hb_cdpIsLower(PHB_CODEPAGE cdp, int iChar)
{
  return (cdp != nullptr) ? (cdp->flags[iChar & 0x0ff] & HB_CDP_LOWER) != 0 : HB_ISLOWER(iChar);
}

HB_BOOL hb_cdpIsUpper(PHB_CODEPAGE cdp, int iChar)
{
  return (cdp != nullptr) ? (cdp->flags[iChar & 0x0ff] & HB_CDP_UPPER) != 0 : HB_ISUPPER(iChar);
}

HB_BOOL hb_charIsDigit(int iChar)
{
  auto cdp = hb_vmCDP();
  return (cdp != nullptr) ? (cdp->flags[iChar & 0x0ff] & HB_CDP_DIGIT) != 0 : HB_ISDIGIT(iChar);
}

HB_BOOL hb_charIsAlpha(int iChar)
{
  auto cdp = hb_vmCDP();
  return (cdp != nullptr) ? (cdp->flags[iChar & 0x0ff] & HB_CDP_ALPHA) != 0 : HB_ISALPHA(iChar);
}

HB_BOOL hb_charIsLower(int iChar)
{
  auto cdp = hb_vmCDP();
  return (cdp != nullptr) ? (cdp->flags[iChar & 0x0ff] & HB_CDP_LOWER) != 0 : HB_ISLOWER(iChar);
}

HB_BOOL hb_charIsUpper(int iChar)
{
  auto cdp = hb_vmCDP();
  return (cdp != nullptr) ? (cdp->flags[iChar & 0x0ff] & HB_CDP_UPPER) != 0 : HB_ISUPPER(iChar);
}

int hb_charLower(int iChar)
{
  auto cdp = hb_vmCDP();
  return (cdp != nullptr) ? cdp->lower[iChar & 0x0ff] : HB_TOLOWER(iChar);
}

int hb_charUpper(int iChar)
{
  auto cdp = hb_vmCDP();
  return (cdp != nullptr) ? cdp->upper[iChar & 0x0ff] : HB_TOUPPER(iChar);
}

char *hb_strLower(char *szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strLower(%s, %" HB_PFS "u)", szText, nLen));
#endif

  auto cdp = hb_vmCDP();

  if (cdp != nullptr)
  {
    for (HB_SIZE u = 0; u < nLen; u++)
    {
      szText[u] = static_cast<char>(cdp->lower[static_cast<HB_UCHAR>(szText[u])]);
    }
  }
  else
  {
    for (HB_SIZE u = 0; u < nLen; u++)
    {
      szText[u] = HB_TOLOWER(szText[u]);
    }
  }

  return szText;
}

char *hb_strUpper(char *szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strUpper(%s, %" HB_PFS "u)", szText, nLen));
#endif

  auto cdp = hb_vmCDP();

  if (cdp != nullptr)
  {
    for (HB_SIZE u = 0; u < nLen; u++)
    {
      szText[u] = static_cast<char>(cdp->upper[static_cast<HB_UCHAR>(szText[u])]);
    }
  }
  else
  {
    for (HB_SIZE u = 0; u < nLen; u++)
    {
      szText[u] = HB_TOUPPER(szText[u]);
    }
  }

  return szText;
}

// end of byte only functions

// basic CP functions

HB_BOOL hb_strIsDigit(const char *szChar)
{
  auto cdp = hb_vmCDP();

  if (cdp != nullptr)
  {
    if (HB_CDP_ISCUSTOM(cdp) && cdp->wcharFlags)
    {
      HB_SIZE n = 0;
      HB_WCHAR wc;
      if (HB_CDPCHAR_GET(cdp, szChar, hb_strnlen(szChar, 6), &n, &wc))
      {
        return (HB_CDPCHAR_FLAGS(cdp, wc) & HB_CDP_DIGIT) != 0;
      }
      else
      {
        return false;
      }
    }
    else
    {
      return (cdp->flags[static_cast<HB_UCHAR>(*szChar)] & HB_CDP_DIGIT) != 0;
    }
  }
  else
  {
    int iChar = static_cast<HB_UCHAR>(*szChar);
    return HB_ISDIGIT(iChar);
  }
}

HB_BOOL hb_strIsAlpha(const char *szChar)
{
  auto cdp = hb_vmCDP();

  if (cdp != nullptr)
  {
    if (HB_CDP_ISCUSTOM(cdp) && cdp->wcharFlags)
    {
      HB_SIZE n = 0;
      HB_WCHAR wc;
      if (HB_CDPCHAR_GET(cdp, szChar, hb_strnlen(szChar, 6), &n, &wc))
      {
        return (HB_CDPCHAR_FLAGS(cdp, wc) & HB_CDP_ALPHA) != 0;
      }
      else
      {
        return false;
      }
    }
    else
    {
      return (cdp->flags[static_cast<HB_UCHAR>(*szChar)] & HB_CDP_ALPHA) != 0;
    }
  }
  else
  {
    int iChar = static_cast<HB_UCHAR>(*szChar);
    return HB_ISALPHA(iChar);
  }
}

HB_BOOL hb_strIsLower(const char *szChar)
{
  auto cdp = hb_vmCDP();

  if (cdp != nullptr)
  {
    if (HB_CDP_ISCUSTOM(cdp) && cdp->wcharFlags)
    {
      HB_SIZE n = 0;
      HB_WCHAR wc;
      if (HB_CDPCHAR_GET(cdp, szChar, hb_strnlen(szChar, 6), &n, &wc))
      {
        return (HB_CDPCHAR_FLAGS(cdp, wc) & HB_CDP_LOWER) != 0;
      }
      else
      {
        return false;
      }
    }
    else
    {
      return (cdp->flags[static_cast<HB_UCHAR>(*szChar)] & HB_CDP_LOWER) != 0;
    }
  }
  else
  {
    int iChar = static_cast<HB_UCHAR>(*szChar);
    return HB_ISLOWER(iChar);
  }
}

HB_BOOL hb_strIsUpper(const char *szChar)
{
  auto cdp = hb_vmCDP();

  if (cdp != nullptr)
  {
    if (HB_CDP_ISCUSTOM(cdp) && cdp->wcharFlags)
    {
      HB_SIZE n = 0;
      HB_WCHAR wc;
      if (HB_CDPCHAR_GET(cdp, szChar, hb_strnlen(szChar, 6), &n, &wc))
      {
        return (HB_CDPCHAR_FLAGS(cdp, wc) & HB_CDP_UPPER) != 0;
      }
      else
      {
        return false;
      }
    }
    else
    {
      return (cdp->flags[static_cast<HB_UCHAR>(*szChar)] & HB_CDP_UPPER) != 0;
    }
  }
  else
  {
    int iChar = static_cast<HB_UCHAR>(*szChar);
    return HB_ISUPPER(iChar);
  }
}

// comparison

const HB_UCHAR *hb_cdpGetSortTab(PHB_CODEPAGE cdp)
{
  return (cdp->nMulti == 0 && cdp->nACSort == 0 && cdp->wcharCmp == nullptr) ? cdp->sort : nullptr;
}

int hb_cdpcmp(const char *szFirst, HB_SIZE nLenFirst, const char *szSecond, HB_SIZE nLenSecond, PHB_CODEPAGE cdp,
              HB_BOOL fExact)
{
  return HB_CDPCHAR_CMP(cdp, szFirst, nLenFirst, szSecond, nLenSecond, fExact);
}

int hb_cdpicmp(const char *szFirst, HB_SIZE nLenFirst, const char *szSecond, HB_SIZE nLenSecond, PHB_CODEPAGE cdp,
               HB_BOOL fExact)
{
  return HB_CDPCHAR_CMPI(cdp, szFirst, nLenFirst, szSecond, nLenSecond, fExact);
}

// UTF-8 conversions

int hb_cdpUTF8CharSize(HB_WCHAR wc)
{
  if (wc < 0x0080)
  {
    return 1;
  }
  else if (wc < 0x0800)
  {
    return 2;
  }
  else
  { // if( wc <= 0xffff )
    return 3;
  }
}

int hb_cdpU16CharToUTF8(char *szUTF8, HB_WCHAR wc)
{
  int n;

  if (wc < 0x0080)
  {
    szUTF8[0] = wc & 0xff;
    n = 1;
  }
  else if (wc < 0x0800)
  {
    szUTF8[0] = 0xc0 | ((wc >> 6) & 0x1f);
    szUTF8[1] = 0x80 | (wc & 0x3f);
    n = 2;
  }
  else
  { // if( wc <= 0xffff )
    szUTF8[0] = 0xe0 | ((wc >> 12) & 0x0f);
    szUTF8[1] = 0x80 | ((wc >> 6) & 0x3f);
    szUTF8[2] = 0x80 | (wc & 0x3f);
    n = 3;
  }
#if 0
  else
  {
    n = 0;
  }
#endif
  return n;
}

HB_BOOL hb_cdpUTF8ToU16NextChar(HB_UCHAR ucChar, int *n, HB_WCHAR *pwc)
{
  if (*n > 0)
  {
    if ((ucChar & 0xc0) != 0x80)
    {
      *n = 0;
      return false;
    }
    *pwc = (*pwc << 6) | (ucChar & 0x3f);
    (*n)--;
    return true;
  }

  *n = 0;
  *pwc = ucChar;
  if (ucChar >= 0xc0)
  {
    if (ucChar < 0xe0)
    {
      *pwc &= 0x1f;
      *n = 1;
    }
    else if (ucChar < 0xf0)
    {
      *pwc &= 0x0f;
      *n = 2;
    }
    else if (ucChar < 0xf8)
    {
      *pwc &= 0x07;
      *n = 3;
    }
    else if (ucChar < 0xfc)
    {
      *pwc &= 0x03;
      *n = 4;
    }
    else if (ucChar < 0xfe)
    {
      *pwc &= 0x01;
      *n = 5;
    }
  }
  return true;
}

HB_SIZE hb_cdpUTF8StringLength(const char *pSrc, HB_SIZE nLen)
{
  HB_SIZE nDst;
  HB_WCHAR wc;
  int n = 0;

  for (HB_SIZE nPos = nDst = 0; nPos < nLen;)
  {
    if (hb_cdpUTF8ToU16NextChar(static_cast<HB_UCHAR>(pSrc[nPos]), &n, &wc))
    {
      ++nPos;
    }
    if (n == 0)
    {
      ++nDst;
    }
  }
  if (n > 0)
  {
    ++nDst;
  }

  return nDst;
}

HB_SIZE hb_cdpUTF8StringAt(const char *szNeedle, HB_SIZE nLenN, const char *szHaystack, HB_SIZE nLenH, HB_SIZE nStart,
                           HB_SIZE nEnd, HB_BOOL fReverse)
{
  HB_SIZE nPosN = 0;
  HB_SIZE nPosH = 0;
  HB_SIZE nPosX = 0;
  HB_SIZE nPos = 0;
  HB_SIZE nRAt = 0;
  HB_SIZE nAt = 0;

  HB_WCHAR wcN = 0;
  HB_WCHAR wcH = 0;
  int nN = 0;
  int nH = 0;

  while (nPosH < nLenH && nPosN < nLenN && nPos < nEnd)
  {
    do
    {
      if (!hb_cdpUTF8ToU16NextChar(static_cast<HB_UCHAR>(szHaystack[nPosH]), &nH, &wcH))
      {
        break;
      }
      ++nPosH;
    } while (nH && nPosH < nLenH);

    if (++nPos < nStart)
    {
      continue;
    }

    do
    {
      if (!hb_cdpUTF8ToU16NextChar(static_cast<HB_UCHAR>(szNeedle[nPosN]), &nN, &wcN))
      {
        break;
      }
      ++nPosN;
    } while (nN && nPosN < nLenN);

    if (wcH == wcN)
    {
      if (nAt == 0)
      {
        nAt = nPos;
        nPosX = nPosH;
      }

      if (nPosN == nLenN)
      {
        if (fReverse)
        {
          nRAt = nAt;
          nPos = nAt;
          nAt = 0;
          nPosH = nPosX;
          nPosX = 0;
          nPosN = 0;
        }
        else
        {
          return nAt;
        }
      }
    }
    else
    {
      if (nAt)
      {
        nPos = nAt;
        nAt = 0;
        nPosH = nPosX;
        nPosX = 0;
      }
      nPosN = 0;
    }
  }

  return nRAt;
}

HB_WCHAR hb_cdpUTF8StringPeek(const char *pSrc, HB_SIZE nLen, HB_SIZE nPos)
{
  if (nLen)
  {
    HB_SIZE nPos2;
    HB_WCHAR wc = 0;
    int n = 0;

    for (nPos2 = 0; nPos2 < nLen && nPos;)
    {
      if (hb_cdpUTF8ToU16NextChar(static_cast<HB_UCHAR>(pSrc[nPos2]), &n, &wc))
      {
        ++nPos2;
      }
      if (n == 0)
      {
        --nPos;
      }
    }

    if (nPos2 < nLen)
    {
      n = 0;
      do
      {
        if (hb_cdpUTF8ToU16NextChar(static_cast<HB_UCHAR>(pSrc[nPos2]), &n, &wc))
        {
          ++nPos2;
        }
        if (n == 0)
        {
          return wc;
        }
      } while (nPos2 < nLen);
    }
  }

  return 0;
}

// caller must free the returned buffer if not nullptr
char *hb_cdpUTF8StringSubstr(const char *pSrc, HB_SIZE nLen, HB_SIZE nFrom, HB_SIZE nCount, HB_SIZE *pulDest)
{
  HB_SIZE nDst = 0;
  HB_WCHAR wc;
  int n;
  char *pDst = nullptr;

  if (nCount && nLen)
  {
    HB_SIZE nPos;
    n = 0;
    for (nPos = 0; nPos < nLen && nFrom;)
    {
      if (hb_cdpUTF8ToU16NextChar(pSrc[nPos], &n, &wc))
      {
        ++nPos;
      }
      if (n == 0)
      {
        --nFrom;
      }
    }

    if (nPos < nLen)
    {
      HB_SIZE nCnt;
      nFrom = nPos;
      nCnt = nCount;
      n = 0;
      do
      {
        if (hb_cdpUTF8ToU16NextChar(pSrc[nPos], &n, &wc))
        {
          ++nPos;
        }
        if (n == 0)
        {
          --nCnt;
        }
      } while (nPos < nLen && nCnt);

      nDst = nPos - nFrom;
      pDst = static_cast<char *>(hb_xgrab(nDst + 1));
      memcpy(pDst, &pSrc[nFrom], nDst);
      pDst[nDst] = '\0';
    }
  }

  if (pulDest)
  {
    *pulDest = nDst;
  }

  return pDst;
}

HB_BOOL hb_cdpGetFromUTF8(PHB_CODEPAGE cdp, HB_UCHAR ch, int *n, HB_WCHAR *pwc)
{
  if (hb_cdpUTF8ToU16NextChar(ch, n, pwc))
  {
    if (*n == 0 && cdp)
    {
      if (HB_CDP_ISCUSTOM(cdp))
      {
        if (HB_CDPCHAR_LEN(cdp, *pwc) == 1)
        {
          HB_SIZE nSize = 0;
          char c;

          if (HB_CDPCHAR_PUT(cdp, &c, 1, &nSize, *pwc))
          {
            *pwc = static_cast<HB_UCHAR>(c);
          }
        }
      }
      else
      {
        if (cdp->uniTable->uniTrans == nullptr)
        {
          hb_cdpBuildTransTable(cdp->uniTable);
        }

        if (*pwc <= cdp->uniTable->wcMax)
        {
          HB_UCHAR uc = cdp->uniTable->uniTrans[*pwc];
          if (uc)
          {
            *pwc = uc;
          }
        }
      }
    }
    return true;
  }
  return false;
}

HB_SIZE hb_cdpStrAsUTF8Len(PHB_CODEPAGE cdp, const char *pSrc, HB_SIZE nSrc, HB_SIZE nMax)
{
  HB_SIZE nPosS, nPosD;
  int i, n;

  if (HB_CDP_ISUTF8(cdp))
  {
    return (nMax && nSrc > nMax) ? nMax : nSrc;
  }
  else if (HB_CDP_ISCUSTOM(cdp))
  {
    HB_WCHAR wc;
    nPosS = nPosD = 0;
    while (HB_CDPCHAR_GET(cdp, pSrc, nSrc, &nPosS, &wc))
    {
      i = hb_cdpUTF8CharSize(wc);
      if (nMax && nPosD + i > nMax)
      {
        break;
      }
      nPosD += i;
    }
  }
  else
  {
    const HB_WCHAR *uniCodes = cdp->uniTable->uniCodes;
    for (nPosS = nPosD = 0; nPosS < nSrc; ++nPosS)
    {
      auto uc = static_cast<HB_UCHAR>(pSrc[nPosS]);
      HB_WCHAR wc = uniCodes[uc];

      if (wc == 0)
      {
        wc = uc;
      }
      n = hb_cdpUTF8CharSize(wc);
      if (nMax && nPosD + n > nMax)
      {
        break;
      }
      nPosD += n;
    }
  }

  return nPosD;
}

HB_SIZE hb_cdpStrToUTF8(PHB_CODEPAGE cdp, const char *pSrc, HB_SIZE nSrc, char *pDst, HB_SIZE nDst)
{
  HB_SIZE nPosS, nPosD, u;

  if (HB_CDP_ISUTF8(cdp))
  {
    if (nSrc > nDst)
    {
      nSrc = nDst;
    }
    else if (nSrc < nDst)
    {
      pDst[nSrc] = '\0';
    }
    memcpy(pDst, pSrc, nSrc);
    return nSrc;
  }
  else if (HB_CDP_ISCUSTOM(cdp))
  {
    HB_WCHAR wc;
    nPosS = nPosD = 0;
    while (nPosD < nDst && HB_CDPCHAR_GET(cdp, pSrc, nSrc, &nPosS, &wc))
    {
      u = hb_cdpUTF8CharSize(wc);
      if (nPosD + u <= nDst)
      {
        hb_cdpU16CharToUTF8(&pDst[nPosD], wc);
        nPosD += u;
      }
      else
      {
        break;
      }
    }
  }
  else
  {
    const HB_WCHAR *uniCodes = cdp->uniTable->uniCodes;
    for (nPosS = nPosD = 0; nPosS < nSrc && nPosD < nDst; ++nPosS)
    {
      auto uc = static_cast<HB_UCHAR>(pSrc[nPosS]);
      HB_WCHAR wc = uniCodes[uc];

      if (wc == 0)
      {
        wc = uc;
      }
      u = hb_cdpUTF8CharSize(wc);
      if (nPosD + u <= nDst)
      {
        hb_cdpU16CharToUTF8(&pDst[nPosD], wc);
        nPosD += u;
      }
      else
      {
        break;
      }
    }
  }

  if (nPosD < nDst)
  {
    pDst[nPosD] = '\0';
  }

  return nPosD;
}

HB_SIZE hb_cdpStrToUTF8Disp(PHB_CODEPAGE cdp, const char *pSrc, HB_SIZE nSrc, char *pDst, HB_SIZE nDst)
{
  HB_SIZE nPosS, nPosD, u;

  if (HB_CDP_ISUTF8(cdp))
  {
    if (nSrc > nDst)
    {
      nSrc = nDst;
    }
    else if (nSrc < nDst)
    {
      pDst[nSrc] = '\0';
    }
    memcpy(pDst, pSrc, nSrc);
    return nSrc;
  }
  else if (HB_CDP_ISCUSTOM(cdp))
  {
    HB_WCHAR wc;
    nPosS = nPosD = 0;
    while (nPosD < nDst && HB_CDPCHAR_GET(cdp, pSrc, nSrc, &nPosS, &wc))
    {
      if (wc < 32)
      {
        wc = s_uniCtrls[wc];
      }
      u = hb_cdpUTF8CharSize(wc);
      if (nPosD + u <= nDst)
      {
        hb_cdpU16CharToUTF8(&pDst[nPosD], wc);
        nPosD += u;
      }
      else
      {
        break;
      }
    }
  }
  else
  {
    const HB_WCHAR *uniCodes = cdp->uniTable->uniCodes;
    for (nPosS = nPosD = 0; nPosS < nSrc && nPosD < nDst; ++nPosS)
    {
      auto uc = static_cast<HB_UCHAR>(pSrc[nPosS]);
      HB_WCHAR wc = uniCodes[uc];

      if (wc == 0)
      {
        wc = uc < 32 ? s_uniCtrls[uc] : s_uniCodes[uc];
      }

      u = hb_cdpUTF8CharSize(wc);
      if (nPosD + u <= nDst)
      {
        hb_cdpU16CharToUTF8(&pDst[nPosD], wc);
        nPosD += u;
      }
      else
      {
        break;
      }
    }
  }

  if (nPosD < nDst)
  {
    pDst[nPosD] = '\0';
  }

  return nPosD;
}

HB_SIZE hb_cdpUTF8AsStrLen(PHB_CODEPAGE cdp, const char *pSrc, HB_SIZE nSrc, HB_SIZE nMax)
{
  HB_WCHAR wc = 0;
  HB_SIZE nPosS, nPosD;
  int n = 0, i;

  if (HB_CDP_ISUTF8(cdp))
  {
    return (nMax && nSrc > nMax) ? nMax : nSrc;
  }
  else if (HB_CDP_ISCUSTOM(cdp))
  {
    for (nPosS = nPosD = 0; nPosS < nSrc;)
    {
      if (hb_cdpUTF8ToU16NextChar(static_cast<HB_UCHAR>(pSrc[nPosS]), &n, &wc))
      {
        ++nPosS;
      }

      if (n == 0)
      {
        i = HB_CDPCHAR_LEN(cdp, wc);
        if (nMax && nPosD + i > nMax)
        {
          break;
        }
        nPosD += i;
      }
    }
  }
  else
  {
    for (nPosS = nPosD = 0; nPosS < nSrc;)
    {
      if (hb_cdpUTF8ToU16NextChar(static_cast<HB_UCHAR>(pSrc[nPosS]), &n, &wc))
      {
        ++nPosS;
      }

      if (n == 0)
      {
        ++nPosD;
        if (nMax && nPosD >= nMax)
        {
          break;
        }
      }
    }
  }

  return nPosD;
}

HB_SIZE hb_cdpUTF8ToStr(PHB_CODEPAGE cdp, const char *pSrc, HB_SIZE nSrc, char *pDst, HB_SIZE nDst)
{
  HB_UCHAR *uniTrans;
  HB_WCHAR wcMax, wc = 0;
  HB_SIZE nPosS, nPosD;
  int n = 0;

  if (HB_CDP_ISUTF8(cdp))
  {
    if (nSrc > nDst)
    {
      nSrc = nDst;
    }
    else if (nSrc < nDst)
    {
      pDst[nSrc] = '\0';
    }
    memcpy(pDst, pSrc, nSrc);
    return nSrc;
  }
  else if (HB_CDP_ISCUSTOM(cdp))
  {
    for (nPosS = nPosD = 0; nPosS < nSrc && nPosD < nDst;)
    {
      if (hb_cdpUTF8ToU16NextChar(static_cast<HB_UCHAR>(pSrc[nPosS]), &n, &wc))
      {
        ++nPosS;
      }

      if (n == 0)
      {
        if (!HB_CDPCHAR_PUT(cdp, pDst, nDst, &nPosD, wc))
        {
          break;
        }
      }
    }
  }
  else
  {
    if (cdp->uniTable->uniTrans == nullptr)
    {
      hb_cdpBuildTransTable(cdp->uniTable);
    }
    uniTrans = cdp->uniTable->uniTrans;
    wcMax = cdp->uniTable->wcMax;

    for (nPosS = nPosD = 0; nPosS < nSrc && nPosD < nDst;)
    {
      if (hb_cdpUTF8ToU16NextChar(static_cast<HB_UCHAR>(pSrc[nPosS]), &n, &wc))
      {
        ++nPosS;
      }

      if (n == 0)
      {
        if (wc <= wcMax && uniTrans[wc])
        {
          pDst[nPosD++] = uniTrans[wc];
        }
        else
        {
          pDst[nPosD++] = wc >= 0x100 ? '?' : static_cast<HB_UCHAR>(wc);
        }
      }
    }
  }

  if (nPosD < nDst)
  {
    pDst[nPosD] = '\0';
  }

  return nPosD;
}

// U16 (hb wide char) conversions
HB_WCHAR hb_cdpGetU16(PHB_CODEPAGE cdp, HB_UCHAR ch)
{
  if (cdp != nullptr)
  {
    HB_WCHAR wc;
    if (HB_CDP_ISCUSTOM(cdp))
    {
      HB_SIZE n = 0;
      if (!HB_CDPCHAR_GET(cdp, reinterpret_cast<const char *>(&ch), 1, &n, &wc))
      {
        wc = 0;
      }
    }
    else
    {
      wc = cdp->uniTable->uniCodes[ch];
    }
    return wc == 0 ? ch : wc;
  }
  else
  {
    return ch;
  }
}

HB_WCHAR hb_cdpGetWC(PHB_CODEPAGE cdp, HB_UCHAR ch, HB_WCHAR wcDef)
{
  if (cdp != nullptr)
  {
    if (HB_CDP_ISCUSTOM(cdp))
    {
      HB_WCHAR wc;
      HB_SIZE n = 0;
      if (HB_CDPCHAR_GET(cdp, reinterpret_cast<const char *>(&ch), 1, &n, &wc))
      {
        wcDef = wc;
      }
    }
    else if (cdp->uniTable->uniCodes[ch])
    {
      wcDef = cdp->uniTable->uniCodes[ch];
    }
  }
  else if (ch >= 32 && ch <= 126)
  {
    wcDef = ch;
  }
  return wcDef;
}

HB_WCHAR hb_cdpGetU16Disp(PHB_CODEPAGE cdp, HB_UCHAR ch)
{
  if (cdp != nullptr)
  {
    HB_WCHAR wc;
    if (HB_CDP_ISCUSTOM(cdp))
    {
      HB_SIZE n = 0;
      if (!HB_CDPCHAR_GET(cdp, reinterpret_cast<const char *>(&ch), 1, &n, &wc))
      {
        wc = 0;
      }
    }
    else
    {
      wc = cdp->uniTable->uniCodes[ch];
    }
    if (wc == 0)
    {
      wc = ch < 32 ? s_uniCtrls[ch] : s_uniCodes[ch];
    }
    return wc;
  }
  else
  {
    return ch;
  }
}

HB_UCHAR hb_cdpGetChar(PHB_CODEPAGE cdp, HB_WCHAR wc)
{
  if (cdp != nullptr)
  {
    if (HB_CDP_ISCUSTOM(cdp))
    {
      if (HB_CDPCHAR_LEN(cdp, wc) == 1)
      {
        HB_SIZE n = 0;
        char c;

        if (!HB_CDPCHAR_PUT(cdp, &c, 1, &n, wc))
        {
          wc = '?';
        }
        else
        {
          wc = static_cast<HB_UCHAR>(c);
        }
      }
      else
      {
        wc = '?';
      }
    }
    else
    {
      if (cdp->uniTable->uniTrans == nullptr)
      {
        hb_cdpBuildTransTable(cdp->uniTable);
      }

      if (wc <= cdp->uniTable->wcMax)
      {
        HB_UCHAR uc = cdp->uniTable->uniTrans[wc];
        if (uc)
        {
          wc = uc;
        }
      }
    }
  }
  return wc >= 0x100 ? '?' : static_cast<HB_UCHAR>(wc);
}

HB_UCHAR hb_cdpGetUC(PHB_CODEPAGE cdp, HB_WCHAR wc, HB_UCHAR ucDef)
{
  if (cdp != nullptr)
  {
    if (HB_CDP_ISCUSTOM(cdp))
    {
      if (HB_CDPCHAR_LEN(cdp, wc) == 1)
      {
        HB_SIZE n = 0;
        char c;
        if (HB_CDPCHAR_PUT(cdp, &c, 1, &n, wc))
        {
          ucDef = static_cast<HB_UCHAR>(c);
        }
      }
    }
    else
    {
      if (cdp->uniTable->uniTrans == nullptr)
      {
        hb_cdpBuildTransTable(cdp->uniTable);
      }

      if (wc <= cdp->uniTable->wcMax)
      {
        HB_UCHAR uc = cdp->uniTable->uniTrans[wc];
        if (uc)
        {
          ucDef = uc;
        }
      }
      if (ucDef == 0 && wc <= HB_MAX_CTRL_CODE)
      {
        HB_UCHAR uc = s_rev_ctrl[wc];
        if (uc)
        {
          ucDef = uc;
        }
      }
    }
  }
  else if (wc <= 0xFF)
  {
    ucDef = static_cast<HB_UCHAR>(wc);
  }

  return ucDef;
}

HB_WCHAR hb_cdpGetU16Ctrl(HB_WCHAR wc)
{
  return wc < 32 ? s_uniCtrls[wc] : (wc == 0x007F ? 0x2302 : wc);
}

HB_SIZE hb_cdpStrAsU16Len(PHB_CODEPAGE cdp, const char *pSrc, HB_SIZE nSrc, HB_SIZE nMax)
{
  if (HB_CDP_ISUTF8(cdp))
  {
    nSrc = hb_cdpUTF8StringLength(pSrc, nSrc);
  }
  else if (HB_CDP_ISCUSTOM(cdp))
  {
    HB_WCHAR wc;
    HB_SIZE nPosS, nPosD;
    nPosS = nPosD = 0;
    while (HB_CDPCHAR_GET(cdp, pSrc, nSrc, &nPosS, &wc))
    {
      ++nPosD;
      if (nMax && nPosD >= nMax)
      {
        break;
      }
    }
    return nPosD;
  }

  return (nMax && nSrc > nMax) ? nMax : nSrc;
}

#undef HB_CDP_ENDIAN_SWAP
#if defined(HB_BIG_ENDIAN)
#define HB_CDP_ENDIAN_SWAP HB_CDP_ENDIAN_LITTLE
#elif defined(HB_LITTLE_ENDIAN)
#define HB_CDP_ENDIAN_SWAP HB_CDP_ENDIAN_BIG
#endif

HB_SIZE hb_cdpStrToU16(PHB_CODEPAGE cdp, int iEndian, const char *pSrc, HB_SIZE nSrc, HB_WCHAR *pDst, HB_SIZE nDst)
{
  const HB_WCHAR *uniCodes;
  HB_SIZE nPosS, nPosD;

  if (HB_CDP_ISUTF8(cdp))
  {
    HB_WCHAR wc = 0;
    int n = 0;

    for (nPosS = nPosD = 0; nPosS < nSrc && nPosD < nDst;)
    {
      if (hb_cdpUTF8ToU16NextChar(static_cast<HB_UCHAR>(pSrc[nPosS]), &n, &wc))
      {
        ++nPosS;
      }

      if (n == 0)
      {
#if defined(HB_CDP_ENDIAN_SWAP)
        if (iEndian == HB_CDP_ENDIAN_SWAP)
        {
          wc = HB_SWAP_UINT16(wc);
        }
        pDst[nPosD++] = wc;
#else
        if (iEndian == HB_CDP_ENDIAN_LITTLE)
        {
          HB_PUT_LE_UINT16(&pDst[nPosD], wc);
        }
        else if (iEndian == HB_CDP_ENDIAN_BIG)
        {
          HB_PUT_BE_UINT16(&pDst[nPosD], wc);
        }
        else
        {
          pDst[nPosD] = wc;
        }
        ++nPosD;
#endif
      }
    }
  }
  else if (HB_CDP_ISCUSTOM(cdp))
  {
    HB_WCHAR wc;
    nPosS = nPosD = 0;
    while (nPosD < nDst && HB_CDPCHAR_GET(cdp, pSrc, nSrc, &nPosS, &wc))
    {
#if defined(HB_CDP_ENDIAN_SWAP)
      if (iEndian == HB_CDP_ENDIAN_SWAP)
      {
        wc = HB_SWAP_UINT16(wc);
      }
      pDst[nPosD++] = wc;
#else
      if (iEndian == HB_CDP_ENDIAN_LITTLE)
      {
        HB_PUT_LE_UINT16(&pDst[nPosD], wc);
      }
      else if (iEndian == HB_CDP_ENDIAN_BIG)
      {
        HB_PUT_BE_UINT16(&pDst[nPosD], wc);
      }
      else
      {
        pDst[nPosD] = wc;
      }
      ++nPosD;
#endif
    }
  }
  else
  {
    uniCodes = cdp->uniTable->uniCodes;
    for (nPosS = nPosD = 0; nPosS < nSrc && nPosD < nDst; ++nPosS)
    {
      auto uc = static_cast<HB_UCHAR>(pSrc[nPosS]);
      HB_WCHAR wc = uniCodes[uc];

      if (wc == 0)
      {
        wc = uc;
      }
#if defined(HB_CDP_ENDIAN_SWAP)
      if (iEndian == HB_CDP_ENDIAN_SWAP)
      {
        wc = HB_SWAP_UINT16(wc);
      }
      pDst[nPosD++] = wc;
#else
      if (iEndian == HB_CDP_ENDIAN_LITTLE)
      {
        HB_PUT_LE_UINT16(&pDst[nPosD], wc);
      }
      else if (iEndian == HB_CDP_ENDIAN_BIG)
      {
        HB_PUT_BE_UINT16(&pDst[nPosD], wc);
      }
      else
      {
        pDst[nPosD] = wc;
      }
      ++nPosD;
#endif
    }
  }

  if (nPosD < nDst)
  {
    pDst[nPosD] = '\0';
  }

  return nPosD;
}

HB_WCHAR *hb_cdpnStrDupU16(PHB_CODEPAGE cdp, int iEndian, const char *pSrc, HB_SIZE nSrc, HB_SIZE *pnDst)
{
  HB_SIZE nLen = hb_cdpStrAsU16Len(cdp, pSrc, nSrc, 0);
  auto pDst = static_cast<HB_WCHAR *>(hb_xgrab((nLen + 1) * sizeof(HB_WCHAR)));

  hb_cdpStrToU16(cdp, iEndian, pSrc, nSrc, pDst, nLen + 1);
  if (pnDst)
  {
    *pnDst = nLen;
  }
  return pDst;
}

HB_WCHAR *hb_cdpStrDupU16(PHB_CODEPAGE cdp, int iEndian, const char *pSrc)
{
  return hb_cdpnStrDupU16(cdp, iEndian, pSrc, strlen(pSrc), nullptr);
}

HB_WCHAR *hb_cdpStrDupnU16(PHB_CODEPAGE cdp, int iEndian, const char *pSrc, HB_SIZE nLen)
{
  return hb_cdpnStrDupU16(cdp, iEndian, pSrc, hb_strnlen(pSrc, nLen), nullptr);
}

HB_SIZE hb_cdpU16AsStrLen(PHB_CODEPAGE cdp, const HB_WCHAR *pSrc, HB_SIZE nSrc, HB_SIZE nMax)
{
  HB_SIZE nPosS, nPosD;
  int i;

  if (HB_CDP_ISUTF8(cdp))
  {
    for (nPosS = nPosD = 0; nPosS < nSrc; ++nPosS)
    {
      i = hb_cdpUTF8CharSize(pSrc[nPosS]);
      if (nMax && nPosD + i > nMax)
      {
        break;
      }
      nPosD += i;
    }
  }
  else if (HB_CDP_ISCUSTOM(cdp))
  {
    for (nPosS = nPosD = 0; nPosS < nSrc; ++nPosS)
    {
      i = HB_CDPCHAR_LEN(cdp, pSrc[nPosS]);
      if (nMax && nPosD + i > nMax)
      {
        break;
      }
      nPosD += i;
    }
  }
  else
  {
    nPosD = (nMax && nSrc > nMax) ? nMax : nSrc;
  }

  return nPosD;
}

HB_SIZE hb_cdpU16ToStr(PHB_CODEPAGE cdp, int iEndian, const HB_WCHAR *pSrc, HB_SIZE nSrc, char *pDst, HB_SIZE nDst)
{
  HB_UCHAR *uniTrans;
  HB_WCHAR wcMax, wc;
  HB_SIZE nPosS, nPosD;

  if (HB_CDP_ISUTF8(cdp))
  {
    for (nPosS = nPosD = 0; nPosS < nSrc; ++nPosS)
    {
      int i;
#if defined(HB_CDP_ENDIAN_SWAP)
      wc = pSrc[nPosS];
      if (iEndian == HB_CDP_ENDIAN_SWAP)
      {
        wc = HB_SWAP_UINT16(wc);
      }
#else
      if (iEndian == HB_CDP_ENDIAN_LITTLE)
      {
        wc = HB_GET_LE_UINT16(&pSrc[nPosS]);
      }
      else if (iEndian == HB_CDP_ENDIAN_BIG)
      {
        wc = HB_GET_BE_UINT16(&pSrc[nPosS]);
      }
      else
      {
        wc = pSrc[nPosS];
      }
#endif
      i = hb_cdpUTF8CharSize(wc);
      if (nPosD + i <= nDst)
      {
        hb_cdpU16CharToUTF8(&pDst[nPosD], wc);
        nPosD += i;
      }
      else
      {
        break;
      }
    }
  }
  else if (HB_CDP_ISCUSTOM(cdp))
  {
    for (nPosS = nPosD = 0; nPosS < nSrc; ++nPosS)
    {
#if defined(HB_CDP_ENDIAN_SWAP)
      wc = pSrc[nPosS];
      if (iEndian == HB_CDP_ENDIAN_SWAP)
      {
        wc = HB_SWAP_UINT16(wc);
      }
#else
      if (iEndian == HB_CDP_ENDIAN_LITTLE)
      {
        wc = HB_GET_LE_UINT16(&pSrc[nPosS]);
      }
      else if (iEndian == HB_CDP_ENDIAN_BIG)
      {
        wc = HB_GET_BE_UINT16(&pSrc[nPosS]);
      }
      else
      {
        wc = pSrc[nPosS];
      }
#endif
      if (!HB_CDPCHAR_PUT(cdp, pDst, nDst, &nPosD, wc))
      {
        break;
      }
    }
  }
  else
  {
    if (cdp->uniTable->uniTrans == nullptr)
    {
      hb_cdpBuildTransTable(cdp->uniTable);
    }
    uniTrans = cdp->uniTable->uniTrans;
    wcMax = cdp->uniTable->wcMax;

    for (nPosS = nPosD = 0; nPosS < nSrc && nPosD < nDst; ++nPosS)
    {
#if defined(HB_CDP_ENDIAN_SWAP)
      wc = pSrc[nPosS];
      if (iEndian == HB_CDP_ENDIAN_SWAP)
      {
        wc = HB_SWAP_UINT16(wc);
      }
#else
      if (iEndian == HB_CDP_ENDIAN_LITTLE)
      {
        wc = HB_GET_LE_UINT16(&pSrc[nPosS]);
      }
      else if (iEndian == HB_CDP_ENDIAN_BIG)
      {
        wc = HB_GET_BE_UINT16(&pSrc[nPosS]);
      }
      else
      {
        wc = pSrc[nPosS];
      }
#endif
      if (wc <= wcMax && uniTrans[wc])
      {
        pDst[nPosD++] = uniTrans[wc];
      }
      else
      {
        pDst[nPosD++] = wc >= 0x100 ? '?' : static_cast<HB_UCHAR>(wc);
      }
    }
  }

  if (nPosD < nDst)
  {
    pDst[nPosD] = '\0';
  }

  return nPosD;
}

// CP translations
HB_SIZE hb_cdpTransLen(const char *pSrc, HB_SIZE nSrc, HB_SIZE nMax, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut)
{
  if (cdpIn && cdpOut && cdpIn != cdpOut &&
      (cdpIn->uniTable != cdpOut->uniTable || HB_CDP_ISCUSTOM(cdpIn) || HB_CDP_ISCUSTOM(cdpOut)))
  {
    if (HB_CDP_ISUTF8(cdpIn))
    {
      return hb_cdpUTF8AsStrLen(cdpOut, pSrc, nSrc, nMax);
    }
    else if (HB_CDP_ISUTF8(cdpOut))
    {
      return hb_cdpStrAsUTF8Len(cdpIn, pSrc, nSrc, nMax);
    }
    else if (HB_CDP_ISCUSTOM(cdpIn) || HB_CDP_ISCUSTOM(cdpOut))
    {
      HB_SIZE nPosS, nSize;
      HB_WCHAR wc;

      nPosS = nSize = 0;
      while (HB_CDPCHAR_GET(cdpIn, pSrc, nSrc, &nPosS, &wc))
      {
        int i = HB_CDPCHAR_LEN(cdpOut, wc);
        if (nMax && nSize + i > nMax)
        {
          break;
        }
        nSize += i;
      }
      return nSize;
    }
  }

  return (nMax && nSrc > nMax) ? nMax : nSrc;
}

HB_SIZE hb_cdpTransTo(const char *pSrc, HB_SIZE nSrc, char *pDst, HB_SIZE nDst, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut)
{
  HB_SIZE nSize;

  if (cdpIn && cdpOut && cdpIn != cdpOut &&
      (cdpIn->uniTable != cdpOut->uniTable || HB_CDP_ISCUSTOM(cdpIn) || HB_CDP_ISCUSTOM(cdpOut)))
  {
    if (HB_CDP_ISUTF8(cdpIn))
    {
      return hb_cdpUTF8ToStr(cdpOut, pSrc, nSrc, pDst, nDst);
    }
    else if (HB_CDP_ISUTF8(cdpOut))
    {
      return hb_cdpStrToUTF8(cdpIn, pSrc, nSrc, pDst, nDst);
    }
    else if (HB_CDP_ISCUSTOM(cdpIn) || HB_CDP_ISCUSTOM(cdpOut))
    {
      HB_SIZE nPosS;
      HB_WCHAR wc;

      nPosS = nSize = 0;
      while (nSize < nDst && HB_CDPCHAR_GET(cdpIn, pSrc, nSrc, &nPosS, &wc))
      {
        if (!HB_CDPCHAR_PUT(cdpOut, pDst, nDst, &nSize, wc))
        {
          break;
        }
      }
    }
    else
    {
      HB_UCHAR *uniTrans;
      HB_WCHAR wcMax;

      if (cdpOut->uniTable->uniTrans == nullptr)
      {
        hb_cdpBuildTransTable(cdpOut->uniTable);
      }
      uniTrans = cdpOut->uniTable->uniTrans;
      wcMax = cdpOut->uniTable->wcMax;

      if (nSrc > nDst)
      {
        nSrc = nDst;
      }
      for (nSize = 0; nSize < nSrc; ++nSize)
      {
        auto uc = static_cast<HB_UCHAR>(pSrc[nSize]);
        HB_WCHAR wc = cdpIn->uniTable->uniCodes[uc];
        if (wc && wc <= wcMax && uniTrans[wc])
        {
          uc = uniTrans[wc];
        }
        pDst[nSize] = uc;
      }
    }
  }
  else
  {
    nSize = (nSrc > nDst) ? nDst : nSrc;
    memcpy(pDst, pSrc, nSize);
  }

  if (nSize < nDst)
  {
    pDst[nSize] = '\0';
  }

  return nSize;
}

int hb_cdpTranslateChar(int iChar, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut)
{
  if (cdpIn && cdpOut && cdpIn != cdpOut &&
      (cdpIn->uniTable != cdpOut->uniTable || HB_CDP_ISCUSTOM(cdpIn) || HB_CDP_ISCUSTOM(cdpOut)) && iChar >= 0 &&
      iChar < 256)
  {
    HB_WCHAR wc;

    if (HB_CDP_ISCUSTOM(cdpIn) || HB_CDP_ISCUSTOM(cdpOut))
    {
      HB_SIZE n = 0;
      auto c = static_cast<char>(iChar);

      if (HB_CDPCHAR_GET(cdpIn, &c, 1, &n, &wc))
      {
        if (HB_CDPCHAR_PUT(cdpOut, &c, 1, &n, wc))
        {
          if (c != '?')
          {
            iChar = static_cast<HB_UCHAR>(c);
          }
        }
      }
    }
    else
    {
      wc = cdpIn->uniTable->uniCodes[iChar];

      if (wc)
      {
        if (cdpOut->uniTable->uniTrans == nullptr)
        {
          hb_cdpBuildTransTable(cdpOut->uniTable);
        }

        if (wc <= cdpOut->uniTable->wcMax)
        {
          wc = cdpOut->uniTable->uniTrans[wc];
          if (wc)
          {
            iChar = wc;
          }
        }
      }
    }
  }

  return iChar;
}

int hb_cdpTranslateDispChar(int iChar, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut)
{
  if (cdpIn && cdpOut && cdpIn != cdpOut &&
      (cdpIn->uniTable != cdpOut->uniTable || HB_CDP_ISCUSTOM(cdpIn) || HB_CDP_ISCUSTOM(cdpOut)) && iChar >= 0 &&
      iChar < 256)
  {
    HB_WCHAR wc;

    if (HB_CDP_ISCUSTOM(cdpIn) || HB_CDP_ISCUSTOM(cdpOut))
    {
      HB_SIZE n = 0;
      auto c = static_cast<char>(iChar);

      if (!HB_CDPCHAR_GET(cdpIn, &c, 1, &n, &wc))
      {
        wc = static_cast<HB_WCHAR>(iChar);
      }
      if (wc < 32)
      {
        wc = s_uniCtrls[iChar];
      }
      if (HB_CDPCHAR_PUT(cdpOut, &c, 1, &n, wc))
      {
        if (c != '?')
        {
          iChar = static_cast<HB_UCHAR>(c);
        }
      }
    }
    else
    {
      wc = cdpIn->uniTable->uniCodes[iChar];

      if (wc == 0)
      {
        wc = iChar < 32 ? s_uniCtrls[iChar] : s_uniCodes[iChar];
      }

      if (wc)
      {
        if (cdpOut->uniTable->uniTrans == nullptr)
        {
          hb_cdpBuildTransTable(cdpOut->uniTable);
        }

        if (wc <= cdpOut->uniTable->wcMax)
        {
          wc = cdpOut->uniTable->uniTrans[wc];
          if (wc)
          {
            iChar = wc;
          }
        }
      }
    }
  }

  return iChar;
}

HB_SIZE hb_cdpnDupLen(const char *pSrc, HB_SIZE nSrc, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut)
{
  return hb_cdpTransLen(pSrc, nSrc, 0, cdpIn, cdpOut);
}

HB_SIZE hb_cdpnDup2Len(const char *pSrc, HB_SIZE nSrc, HB_SIZE nMax, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut)
{
  return hb_cdpTransLen(pSrc, nSrc, nMax, cdpIn, cdpOut);
}

char *hb_cdpnDup(const char *pSrc, HB_SIZE *pnLen, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut)
{
  HB_SIZE nDst = hb_cdpTransLen(pSrc, *pnLen, 0, cdpIn, cdpOut);
  auto pDst = static_cast<char *>(hb_xgrab(nDst + 1));
  hb_cdpTransTo(pSrc, *pnLen, pDst, nDst + 1, cdpIn, cdpOut);
  *pnLen = nDst;
  return pDst;
}

const char *hb_cdpnDup2(const char *pSrc, HB_SIZE nSrc, char *pDst, HB_SIZE *pnDst, PHB_CODEPAGE cdpIn,
                        PHB_CODEPAGE cdpOut)
{
  *pnDst = hb_cdpTransTo(pSrc, nSrc, pDst, *pnDst, cdpIn, cdpOut);
  return pDst;
}

const char *hb_cdpnDup3(const char *pSrc, HB_SIZE nSrc, char *pDst, HB_SIZE *pnDst, char **pFree, HB_SIZE *pnSize,
                        PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut)
{
  if (cdpIn && cdpOut && cdpIn != cdpOut && nSrc && (!HB_CDP_ISUTF8(cdpIn) || !HB_CDP_ISUTF8(cdpOut)) &&
      (cdpIn->uniTable != cdpOut->uniTable || HB_CDP_ISCUSTOM(cdpIn) || HB_CDP_ISCUSTOM(cdpOut)))
  {
    char *pPrev = nullptr;
    HB_SIZE nDst = hb_cdpTransLen(pSrc, nSrc, 0, cdpIn, cdpOut);

    if (pDst == nullptr)
    {
      pDst = *pFree;
      if (pDst == nullptr && *pnSize > 0)
      {
        pDst = const_cast<char *>(pSrc);
      }
    }

    if (nDst >= *pnSize || (pDst == pSrc && HB_CDP_ISCUSTOM(cdpOut)))
    {
      pPrev = *pFree;
      pDst = *pFree = static_cast<char *>(hb_xgrab(nDst + 1));
      *pnSize = nDst + 1;
    }

    nDst = hb_cdpTransTo(pSrc, nSrc, pDst, *pnSize, cdpIn, cdpOut);

    if (pPrev)
    {
      hb_xfree(pPrev);
    }
    if (pnDst)
    {
      *pnDst = nDst;
    }
    return pDst;
  }

  if (pnDst)
  {
    *pnDst = nSrc;
  }
  return pSrc;
}

char *hb_cdpDup(const char *pszSrc, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut)
{
  HB_SIZE nLen = strlen(pszSrc);
  return hb_cdpnDup(pszSrc, &nLen, cdpIn, cdpOut);
}

char *hb_cdpDupn(const char *pszSrc, HB_SIZE nLen, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut)
{
  nLen = hb_strnlen(pszSrc, nLen);
  return hb_cdpnDup(pszSrc, &nLen, cdpIn, cdpOut);
}

char *hb_cdpnDupUpper(PHB_CODEPAGE cdp, const char *pszText, HB_SIZE *pnSize)
{
  HB_SIZE nSize = pnSize ? *pnSize : strlen(pszText);
  auto pszDst = static_cast<char *>(hb_xgrab(nSize + 1));

  if (cdp != nullptr)
  {
    if (HB_CDP_ISCUSTOM(cdp) && cdp->wcharUpper)
    {
      HB_SIZE nS = 0, nD = 0, nSrc = nSize;
      HB_WCHAR wc;

      while (HB_CDPCHAR_GET(cdp, pszText, nSrc, &nS, &wc))
      {
        wc = HB_CDPCHAR_UPPER(cdp, wc);
        if (!HB_CDPCHAR_PUT(cdp, pszDst, nSize, &nD, wc))
        {
          nSize += (nSrc - nS + 2);
          pszDst = static_cast<char *>(hb_xrealloc(pszDst, nSize + 1));
          if (!HB_CDPCHAR_PUT(cdp, pszDst, nSize, &nD, wc))
          {
            break;
          }
        }
      }
      nSize = nD;
      if (pnSize)
      {
        *pnSize = nSize;
      }
    }
    else
    {
      for (HB_SIZE n = 0; n < nSize; n++)
      {
        pszDst[n] = static_cast<char>(cdp->upper[static_cast<HB_UCHAR>(pszText[n])]);
      }
    }
  }
  else
  {
    for (HB_SIZE n = 0; n < nSize; n++)
    {
      pszDst[n] = HB_TOUPPER(pszText[n]);
    }
  }
  pszDst[nSize] = '\0';

  return pszDst;
}

char *hb_cdpnDupLower(PHB_CODEPAGE cdp, const char *pszText, HB_SIZE *pnSize)
{
  HB_SIZE nSize = pnSize ? *pnSize : strlen(pszText);
  auto pszDst = static_cast<char *>(hb_xgrab(nSize + 1));

  if (cdp != nullptr)
  {
    if (HB_CDP_ISCUSTOM(cdp) && cdp->wcharLower)
    {
      HB_SIZE nS = 0, nD = 0, nSrc = nSize;
      HB_WCHAR wc;

      while (HB_CDPCHAR_GET(cdp, pszText, nSrc, &nS, &wc))
      {
        wc = HB_CDPCHAR_LOWER(cdp, wc);
        if (!HB_CDPCHAR_PUT(cdp, pszDst, nSize, &nD, wc))
        {
          nSize += (nSrc - nS + 2);
          pszDst = static_cast<char *>(hb_xrealloc(pszDst, nSize + 1));
          if (!HB_CDPCHAR_PUT(cdp, pszDst, nSize, &nD, wc))
          {
            break;
          }
        }
      }
      nSize = nD;
      if (pnSize)
      {
        *pnSize = nSize;
      }
    }
    else
    {
      for (HB_SIZE n = 0; n < nSize; n++)
      {
        pszDst[n] = static_cast<char>(cdp->lower[static_cast<HB_UCHAR>(pszText[n])]);
      }
    }
  }
  else
  {
    for (HB_SIZE n = 0; n < nSize; n++)
    {
      pszDst[n] = HB_TOLOWER(pszText[n]);
    }
  }
  pszDst[nSize] = '\0';

  return pszDst;
}

HB_SIZE hb_cdpnDup2Upper(PHB_CODEPAGE cdp, const char *pszText, HB_SIZE nSize, char *pBuffer, HB_SIZE nBuffLen)
{
  HB_SIZE nMax = HB_MIN(nSize, nBuffLen);

  if (cdp == nullptr)
  {
    for (HB_SIZE n = 0; n < nMax; n++)
    {
      pBuffer[n] = HB_TOUPPER(pszText[n]);
    }
  }
  else if (!HB_CDP_ISCUSTOM(cdp) || !cdp->wcharUpper)
  {
    for (HB_SIZE n = 0; n < nMax; n++)
    {
      pBuffer[n] = static_cast<char>(cdp->upper[static_cast<HB_UCHAR>(pszText[n])]);
    }
  }
  else
  {
    HB_SIZE nS = 0;
    HB_WCHAR wc;

    nMax = 0;
    while (HB_CDPCHAR_GET(cdp, pszText, nSize, &nS, &wc))
    {
      wc = HB_CDPCHAR_UPPER(cdp, wc);
      if (!HB_CDPCHAR_PUT(cdp, pBuffer, nBuffLen, &nMax, wc))
      {
        break;
      }
    }
  }

  if (nMax < nBuffLen)
  {
    pBuffer[nMax] = '\0';
  }

  return nMax;
}

HB_SIZE hb_cdpnDup2Lower(PHB_CODEPAGE cdp, const char *pszText, HB_SIZE nSize, char *pBuffer, HB_SIZE nBuffLen)
{
  HB_SIZE nMax = HB_MIN(nSize, nBuffLen);

  if (cdp == nullptr)
  {
    for (HB_SIZE n = 0; n < nMax; n++)
    {
      pBuffer[n] = HB_TOLOWER(pszText[n]);
    }
  }
  else if (!HB_CDP_ISCUSTOM(cdp) || !cdp->wcharLower)
  {
    for (HB_SIZE n = 0; n < nMax; n++)
    {
      pBuffer[n] = static_cast<char>(cdp->lower[static_cast<HB_UCHAR>(pszText[n])]);
    }
  }
  else
  {
    HB_SIZE nS = 0;
    HB_WCHAR wc;

    nMax = 0;
    while (HB_CDPCHAR_GET(cdp, pszText, nSize, &nS, &wc))
    {
      wc = HB_CDPCHAR_LOWER(cdp, wc);
      if (!HB_CDPCHAR_PUT(cdp, pBuffer, nBuffLen, &nMax, wc))
      {
        break;
      }
    }
  }

  if (nMax < nBuffLen)
  {
    pBuffer[nMax] = '\0';
  }

  return nMax;
}

HB_WCHAR hb_cdpUpperWC(PHB_CODEPAGE cdp, HB_WCHAR wc)
{
  if (cdp == nullptr)
  {
    return HB_TOUPPER(wc);
  }
  else if (!HB_CDP_ISCUSTOM(cdp) || !cdp->wcharUpper)
  {
    if (cdp->uniTable->uniTrans == nullptr)
    {
      hb_cdpBuildTransTable(cdp->uniTable);
    }

    if (wc <= cdp->uniTable->wcMax && cdp->uniTable->uniTrans[wc])
    {
      wc = cdp->uniTable->uniCodes[cdp->upper[cdp->uniTable->uniTrans[wc]]];
    }
    return wc;
  }
  else
  {
    return HB_CDPCHAR_UPPER(cdp, wc);
  }
}

// functions operating on character indexes
HB_SIZE hb_cdpTextLen(PHB_CODEPAGE cdp, const char *pText, HB_SIZE nSize)
{
  if (cdp && HB_CDP_ISCUSTOM(cdp))
  {
    HB_SIZE nPos = 0, nIndex = 0;
    HB_WCHAR wc;

    while (HB_CDPCHAR_GET(cdp, pText, nSize, &nPos, &wc))
    {
      ++nIndex;
    }

    return nIndex;
  }
  return nSize;
}

HB_SIZE hb_cdpTextPos(PHB_CODEPAGE cdp, const char *pText, HB_SIZE nSize, HB_SIZE nIndex)
{
  if (nIndex > 0)
  {
    if (cdp && HB_CDP_ISCUSTOM(cdp))
    {
      HB_SIZE nPos = 0;
      HB_WCHAR wc;

      do
      {
        if (!HB_CDPCHAR_GET(cdp, pText, nSize, &nPos, &wc))
        {
          break;
        }
      } while (--nIndex);

      return nPos;
    }
    else
    {
      return nIndex >= nSize ? nSize : nIndex;
    }
  }
  return 0;
}

HB_SIZE hb_cdpTextPosEx(PHB_CODEPAGE cdp, const char *pText, HB_SIZE nSize, HB_SIZE *pnIndex)
{
  HB_SIZE nIndex = *pnIndex;

  if (nIndex > 0)
  {
    if (cdp && HB_CDP_ISCUSTOM(cdp))
    {
      HB_SIZE nPos = 0;
      HB_WCHAR wc;

      do
      {
        if (!HB_CDPCHAR_GET(cdp, pText, nSize, &nPos, &wc))
        {
          break;
        }
      } while (--nIndex);
      *pnIndex = nIndex;
      return nPos;
    }
    else if (nIndex > nSize)
    {
      *pnIndex -= nSize;
      return nSize;
    }
    else
    {
      *pnIndex = 0;
      return nIndex;
    }
  }
  *pnIndex = 0;
  return 0;
}

HB_WCHAR hb_cdpTextGetU16(PHB_CODEPAGE cdp, const char *szText, HB_SIZE nLen)
{
  HB_WCHAR wc = 0;

  if (szText != nullptr && nLen > 0)
  {
    if (cdp != nullptr)
    {
      if (HB_CDP_ISCUSTOM(cdp))
      {
        HB_SIZE n = 0;
        if (!HB_CDPCHAR_GET(cdp, szText, nLen, &n, &wc))
        {
          wc = 0;
        }
      }
      else
      {
        wc = cdp->uniTable->uniCodes[static_cast<HB_UCHAR>(*szText)];
      }
    }
    else
    {
      wc = static_cast<HB_UCHAR>(*szText);
    }
  }

  return wc;
}

HB_SIZE hb_cdpTextPutU16(PHB_CODEPAGE cdp, char *szText, HB_SIZE nSize, HB_WCHAR wc)
{
  HB_SIZE nLen = 0;

  if (szText != nullptr && nSize > 0)
  {
    if (cdp != nullptr)
    {
      if (HB_CDP_ISCUSTOM(cdp))
      {
        HB_CDPCHAR_PUT(cdp, szText, nSize, &nLen, wc);
      }
      else
      {
        if (cdp->uniTable->uniTrans == nullptr)
        {
          hb_cdpBuildTransTable(cdp->uniTable);
        }

        if (wc <= cdp->uniTable->wcMax)
        {
          HB_UCHAR uc = cdp->uniTable->uniTrans[wc];
          if (uc)
          {
            szText[nLen++] = uc;
          }
        }
      }
    }
    else
    {
      szText[nLen++] = static_cast<char>(wc);
    }

    if (nLen < nSize)
    {
      szText[nLen] = '\0';
    }
  }

  return nLen;
}

HB_BOOL hb_cdpCharEq(PHB_CODEPAGE cdp, const char *szText1, HB_SIZE nLen1, HB_SIZE *pnPos1, const char *szText2,
                     HB_SIZE nLen2, HB_SIZE *pnPos2)
{
  if (*pnPos1 < nLen1 && *pnPos2 < nLen2)
  {
    if (cdp && HB_CDP_ISCUSTOM(cdp))
    {
      HB_WCHAR wc1, wc2;
      return HB_CDPCHAR_GET(cdp, szText1, nLen1, pnPos1, &wc1) && HB_CDPCHAR_GET(cdp, szText2, nLen2, pnPos2, &wc2) &&
             wc1 == wc2;
    }
    else
    {
      return szText1[(*pnPos1)++] == szText2[(*pnPos2)++];
    }
  }
  return false;
}

HB_BOOL hb_cdpCharCaseEq(PHB_CODEPAGE cdp, const char *szText1, HB_SIZE nLen1, HB_SIZE *pnPos1, const char *szText2,
                         HB_SIZE nLen2, HB_SIZE *pnPos2)
{
  if (*pnPos1 < nLen1 && *pnPos2 < nLen2)
  {
    if (cdp == nullptr)
    {
      HB_UCHAR uc1 = szText1[(*pnPos1)++], uc2 = szText2[(*pnPos2)++];
      return HB_TOUPPER(uc1) == HB_TOUPPER(uc2);
    }
    else if (!HB_CDP_ISCUSTOM(cdp) || !cdp->wcharUpper)
    {
      HB_UCHAR uc1 = szText1[(*pnPos1)++], uc2 = szText2[(*pnPos2)++];
      return cdp->upper[uc1] == cdp->upper[uc2];
    }
    else
    {
      HB_WCHAR wc1, wc2;
      if (HB_CDPCHAR_GET(cdp, szText1, nLen1, pnPos1, &wc1) && HB_CDPCHAR_GET(cdp, szText2, nLen2, pnPos2, &wc2))
      {
        return wc1 == wc2 || HB_CDPCHAR_UPPER(cdp, wc1) == HB_CDPCHAR_UPPER(cdp, wc2);
      }
    }
  }
  return false;
}

// CP management
static HB_UCHAR hb_cdpUtf8Char(const char **pStrPtr, PHB_UNITABLE uniTable)
{
  const char *pszString = *pStrPtr;
  HB_UCHAR uc = 0;
  HB_WCHAR wc = 0;
  int n = 0;

  while (*pszString)
  {
    if (!hb_cdpUTF8ToU16NextChar(static_cast<HB_UCHAR>(*pszString++), &n, &wc))
    {
      break;
    }
    if (n == 0)
    {
      if (wc < 127)
      {
        uc = static_cast<HB_UCHAR>(wc);
      }
      else
      {
        for (n = 0; n < 256; ++n)
        {
          if (wc == uniTable->uniCodes[n])
          {
            uc = static_cast<HB_UCHAR>(n);
            break;
          }
        }
      }
      break;
    }
  }
  if (uc == 0)
  {
    while (*pszString)
    {
      pszString++;
    }
  }
  *pStrPtr = static_cast<const char *>(pszString);

  return uc;
}

#define _HB_CDP_GETUC(p) (!fUtf8 ? static_cast<HB_UCHAR>(*(p) ? *(p)++ : 0) : hb_cdpUtf8Char(&(p), uniTable))

static PHB_CODEPAGE hb_buildCodePage(const char *id, const char *info, PHB_UNITABLE uniTable, const char *pszUpper,
                                     const char *pszLower, unsigned int nACSort, unsigned int nCaseSort, bool fUtf8)
{
  int iMulti, iAcc, iAccUp, iAccLo, iSortUp, iSortLo, i;
  const char *pup, *plo;
  HB_UCHAR ucUp, ucLo, ucUp2, ucLo2;
  HB_SIZE nSize, ul;
  HB_UCHAR *flags, *upper, *lower, *sort, *acc;
  HB_UCHAR used[256];
  PHB_MULTICHAR multi;

  memset(used, '\0', sizeof(used));

  iMulti = iAcc = iSortUp = iSortLo = 0;
  auto lSort = false;
  auto fError = false;

  ucUp2 = ucLo2 = 0;
  pup = pszUpper;
  plo = pszLower;
  while (*pup || *plo)
  {
    ucUp = _HB_CDP_GETUC(pup);
    ucLo = _HB_CDP_GETUC(plo);
    if (ucUp == 0 || ucLo == 0 || (ucUp == '.' && ucLo != '.') || (ucUp == '~' && ucLo != '~'))
    {
      fError = true;
      break;
    }

    if (ucUp == '.')
    {
      HB_UCHAR ucU1, ucU2, ucL1, ucL2;

      ucU1 = _HB_CDP_GETUC(pup);
      ucU2 = _HB_CDP_GETUC(pup);
      ucL1 = _HB_CDP_GETUC(plo);
      ucL2 = _HB_CDP_GETUC(plo);

      if (ucU1 && ucU2 && (*pup == '.' || *pup == '=') && ucL1 && ucL2 && (*plo == '.' || *plo == '='))
      {
        if ((ucU1 != ' ' || ucL1 != ' ') && (ucU1 == ucU2 || (ucU1 != ' ' && ucU2 != ' ')) &&
            (ucL1 == ucL2 || (ucL1 != ' ' && ucL2 != ' ')))
        {
          if (ucU1 != ' ')
          {
            ++iSortLo;
          }

          if (*pup == '=')
          {
            do
            {
              ++pup;
            } while (HB_ISXDIGIT(*pup));
          }
          if (*plo == '=')
          {
            do
            {
              ++plo;
            } while (HB_ISXDIGIT(*plo));
          }
          if (*pup == '.' && *plo == '.')
          {
            lSort = true;
            iMulti++;
            pup++;
            plo++;
            continue;
          }
        }
      }
      fError = true;
      break;
    }
    if (ucUp == '~')
    {
      ucUp = _HB_CDP_GETUC(pup);
      ucLo = _HB_CDP_GETUC(plo);
      if (ucUp == '\0' || ucLo == '\0')
      {
        fError = true;
        break;
      }
      ++iAcc;
    }
    if (used[ucUp] != 0)
    {
      ucUp = ' ';
    }
    if (used[ucLo] != 0)
    {
      ucLo = ' ';
    }
    if (ucUp == ' ' && ucLo == ' ')
    {
      fError = true;
      break;
    }
    if (ucUp != ' ')
    {
      used[ucUp] = 1;
      if (ucUp < ucUp2)
      {
        lSort = true;
      }
      ucUp2 = ucUp;
      ++iSortLo;
    }
    if (ucLo != ' ')
    {
      used[ucLo] = 1;
      if (ucLo < ucLo2)
      {
        lSort = true;
      }
      ucLo2 = ucLo;
    }
  }

  if (iMulti > 64)
  {
    fError = true;
  }

  if (fError || nACSort > HB_CDP_ACSORT_INTERLEAVED || nCaseSort > HB_CDP_CSSORT_IGNORE)
  {
#ifdef __HB_IGNORE_CP_ERRORS
    fprintf(stderr, "Harbour++ CP (%s) initialization failure (1)\n", id);
    fflush(stderr);
    return nullptr;
#else
    hb_errInternal(9994, "Harbour++ CP (%s) initialization failure", id, nullptr);
#endif
  }

  if (iAcc == 0)
  {
    nACSort = HB_CDP_ACSORT_NONE;
  }
  else if (nACSort != HB_CDP_ACSORT_NONE)
  {
    lSort = true;
  }

  if (nCaseSort != HB_CDP_CSSORT_UPLO)
  {
    lSort = true;
  }

  nSize = 0x300;
  if (lSort)
  {
    nSize += 0x100;
    if (nACSort == HB_CDP_ACSORT_INTERLEAVED)
    {
      nSize += 0x100;
    }
  }
  ul = nSize;
  nSize += sizeof(HB_CODEPAGE);
  if (iMulti)
  {
    nSize += iMulti * sizeof(HB_MULTICHAR);
  }

  auto buffer = static_cast<HB_UCHAR *>(hb_xgrabz(nSize));
  auto cdp = reinterpret_cast<PHB_CODEPAGE>(&buffer[ul]);
  cdp->buffer = buffer;

  cdp->flags = flags = buffer;
  buffer += 0x100;
  cdp->upper = upper = buffer;
  buffer += 0x100;
  cdp->lower = lower = buffer;
  buffer += 0x100;
  sort = acc = nullptr;
  if (lSort)
  {
    cdp->sort = sort = buffer;
    buffer += 0x100;
    if (nACSort == HB_CDP_ACSORT_INTERLEAVED)
    {
      cdp->acc = acc = buffer;
      buffer += 0x100;
    }
  }
  if (iMulti)
  {
    cdp->multi = reinterpret_cast<PHB_MULTICHAR>(&buffer[sizeof(HB_CODEPAGE)]);
  }

  cdp->id = id;
  cdp->info = info;
  cdp->uniTable = uniTable;
  cdp->nACSort = nACSort;
  cdp->nMulti = iMulti;
  for (i = 0; i < 0x100; ++i)
  {
    if (HB_ISDIGIT(i))
    {
      flags[i] |= HB_CDP_DIGIT;
    }
    if (HB_ISALPHA(i))
    {
      flags[i] |= HB_CDP_ALPHA;
    }
    if (HB_ISUPPER(i))
    {
      flags[i] |= HB_CDP_UPPER;
    }
    if (HB_ISLOWER(i))
    {
      flags[i] |= HB_CDP_LOWER;
    }
    upper[i] = static_cast<HB_UCHAR>(HB_TOUPPER(i));
    lower[i] = static_cast<HB_UCHAR>(HB_TOLOWER(i));
  }

  iAccUp = iAccLo = 0;
  multi = cdp->multi;
  pup = pszUpper;
  plo = pszLower;
  ucUp2 = ucLo2 = 255;
  memset(used, '\0', sizeof(used));
  while (*pup)
  {
    ucUp = _HB_CDP_GETUC(pup);
    ucLo = _HB_CDP_GETUC(plo);
    if (ucUp == '.')
    {
      multi->cFirst[0] = _HB_CDP_GETUC(pup);
      multi->cLast[0] = _HB_CDP_GETUC(pup);
      multi->cFirst[1] = _HB_CDP_GETUC(plo);
      multi->cLast[1] = _HB_CDP_GETUC(plo);
      if (multi->cFirst[0] != ' ')
      {
        flags[static_cast<HB_UCHAR>(multi->cFirst[0])] |= HB_CDP_MULTI1;
        flags[static_cast<HB_UCHAR>(multi->cLast[0])] |= HB_CDP_MULTI2;
        multi->sortUp = ++iSortUp - iAccUp;
      }
      if (multi->cFirst[1] != ' ')
      {
        flags[static_cast<HB_UCHAR>(multi->cFirst[1])] |= HB_CDP_MULTI1;
        flags[static_cast<HB_UCHAR>(multi->cLast[1])] |= HB_CDP_MULTI2;

        if (nCaseSort == HB_CDP_CSSORT_UPLO)
        {
          ++iSortLo;
        }
        else if (nCaseSort == HB_CDP_CSSORT_MIXED)
        {
          iSortLo = ++iSortUp;
        }
        else
        {
          iSortLo = iSortUp;
        }
        multi->sortLo = iSortLo - iAccLo;
      }
      if (*pup == '=')
      {
        ++pup;
        while (HB_ISXDIGIT(*pup))
        {
          multi->wcUp =
              (multi->wcUp << 4) | (*pup >= 'a' ? (*pup - 'a' + 10) : (*pup >= 'A' ? (*pup - 'A' + 10) : (*pup - '0')));
          ++pup;
        }
      }
      pup++;
      if (*plo == '=')
      {
        ++plo;
        while (HB_ISXDIGIT(*plo))
        {
          multi->wcLo =
              (multi->wcLo << 4) | (*plo >= 'a' ? (*plo - 'a' + 10) : (*plo >= 'A' ? (*plo - 'A' + 10) : (*plo - '0')));
          ++plo;
        }
      }
      plo++;
      if (multi->wcUp || multi->wcLo)
      {
        cdp->nMultiUC++;
      }
      multi++;
    }
    else
    {
      iAcc = 0;
      if (ucUp == '~')
      {
        iAcc = 1;
        ucUp = _HB_CDP_GETUC(pup);
        ucLo = _HB_CDP_GETUC(plo);
      }
      if (ucUp != ' ')
      {
        flags[ucUp] |= HB_CDP_ALPHA;
        flags[ucUp] |= HB_CDP_UPPER;
        if (ucLo != ' ' && (used[ucUp] & HB_CDP_UPPER) == 0)
        {
          lower[ucUp] = ucLo;
          used[ucUp] |= HB_CDP_UPPER;
        }
        if (sort)
        {
          if (sort[ucUp] == 0)
          {
            if (iAcc && nACSort != HB_CDP_ACSORT_NONE)
            {
              ++iAccUp;
            }
            sort[ucUp] = static_cast<HB_UCHAR>(++iSortUp - iAccUp);
            if (acc)
            {
              acc[ucUp] = static_cast<HB_UCHAR>(iSortUp);
            }
            if (ucUp2 > ucUp)
            {
              ucUp2 = ucUp;
            }
          }
        }
      }
      if (ucLo != ' ')
      {
        flags[ucLo] |= HB_CDP_ALPHA;
        flags[ucLo] |= HB_CDP_LOWER;
        if (ucUp != ' ' && (used[ucLo] & HB_CDP_LOWER) == 0)
        {
          upper[ucLo] = ucUp;
          used[ucLo] |= HB_CDP_LOWER;
        }
        if (sort)
        {
          if (sort[ucLo] == 0)
          {
            if (nCaseSort == HB_CDP_CSSORT_UPLO)
            {
              if (iAcc && nACSort != HB_CDP_ACSORT_NONE)
              {
                ++iAccLo;
              }
              ++iSortLo;
            }
            else
            {
              if (nCaseSort == HB_CDP_CSSORT_MIXED)
              {
                if (iAcc && nACSort != HB_CDP_ACSORT_NONE)
                {
                  ++iAccUp;
                }
                ++iSortUp;
              }
              iAccLo = iAccUp;
              iSortLo = iSortUp;
            }
            sort[ucLo] = static_cast<HB_UCHAR>(iSortLo - iAccLo);
            if (acc)
            {
              acc[ucLo] = static_cast<HB_UCHAR>(iSortLo);
            }
            if (ucLo2 > ucLo)
            {
              ucLo2 = ucLo;
            }
          }
        }
      }
    }
  }

  if (sort)
  {
    int iUp, iLo, iSort1, iSort2, iSort3, iAdd;

    if (iMulti > 0)
    {
      if (iMulti > ucUp2 || iMulti > ucLo2)
      {
#ifdef __HB_IGNORE_CP_ERRORS
        fprintf(stderr, "Harbour++ CP (%s) initialization failure (2)\n", id);
        fflush(stderr);
        hb_xfree(buffer);
        return nullptr;
#else
        hb_errInternal(9994, "Harbour++ CP (%s) initialization failure", id, nullptr);
#endif
      }

      if (iMulti <= 32)
      {
        iMulti = 33;
      }
      else
      {
        iMulti = 65;
      }
    }
    else
    {
      iMulti = 1;
    }

    if (nCaseSort != HB_CDP_CSSORT_UPLO)
    {
      ucLo2 = 0;
    }

    for (iUp = iLo = 0, i = iMulti; i < 256; ++i)
    {
      if (sort[i] == 0)
      {
        if (i < static_cast<int>(ucUp2))
        {
          ++iUp;
        }
        else if (i < static_cast<int>(ucLo2))
        {
          ++iLo;
        }
      }
    }
    for (iSort1 = iSort2 = iSort3 = 0, i = iMulti; i < 256; ++i)
    {
      if (sort[i] == 0)
      {
        if (i < static_cast<int>(ucUp2))
        {
          iAdd = ++iSort1;
        }
        else if (i < static_cast<int>(ucLo2))
        {
          iAdd = ++iSort2 + iSortUp + iUp;
        }
        else
        {
          iAdd = ++iSort3 + iUp + iSortLo + iLo;
        }
      }
      else if (sort[i] <= iSortUp)
      {
        iAdd = iUp;
      }
      else
      {
        iAdd = iUp + iLo;
      }

      sort[i] += static_cast<HB_UCHAR>(iAdd);
      if (acc)
      {
        acc[i] += static_cast<HB_UCHAR>(iAdd);
      }
    }
    for (i = 0; i < cdp->nMulti; ++i)
    {
      cdp->multi[i].sortUp += iUp;
      cdp->multi[i].sortLo += iUp + iLo;
    }
  }

  if (cdp->nMultiUC)
  {
    cdp->wcharGet = hb_cdpMulti_get;
    cdp->wcharPut = hb_cdpMulti_put;
    cdp->wcharLen = hb_cdpMulti_len;
    cdp->type |= HB_CDP_TYPE_CUSTOM;
  }
  else
  {
    cdp->wcharGet = hb_cdpStd_get;
    cdp->wcharPut = hb_cdpStd_put;
    cdp->wcharLen = hb_cdpStd_len;
  }

  if (cdp->sort == nullptr)
  {
    cdp->wcharCmp = hb_cdpBin_cmp;
    cdp->wcharCmpI = hb_cdpBin_cmpi;
    cdp->type |= HB_CDP_TYPE_BINSORT;
  }
  else if (cdp->nMulti)
  {
    cdp->wcharCmp = hb_cdpMulti_cmp;
    cdp->wcharCmpI = hb_cdpMulti_cmpi;
  }
  else
  {
    cdp->wcharCmp = hb_cdpStd_cmp;
    cdp->wcharCmpI = hb_cdpStd_cmpi;
  }

  return cdp;
}

static PHB_CODEPAGE *hb_cdpFindPos(const char *id)
{
  if (s_cdpList == nullptr)
  {
    HB_UCHAR *flags, *upper, *lower;

    memset(s_en_buffer, '\0', 0x300);
    s_en_codepage.flags = flags = static_cast<HB_UCHAR *>(s_en_buffer);
    s_en_codepage.upper = upper = static_cast<HB_UCHAR *>(s_en_buffer) + 0x100;
    s_en_codepage.lower = lower = static_cast<HB_UCHAR *>(s_en_buffer) + 0x200;
    for (auto i = 0; i < 0x100; ++i)
    {
      if (HB_ISDIGIT(i))
      {
        flags[i] |= HB_CDP_DIGIT;
      }
      if (HB_ISALPHA(i))
      {
        flags[i] |= HB_CDP_ALPHA;
      }
      if (HB_ISUPPER(i))
      {
        flags[i] |= HB_CDP_UPPER;
      }
      if (HB_ISLOWER(i))
      {
        flags[i] |= HB_CDP_LOWER;
      }
      upper[i] = static_cast<HB_UCHAR>(HB_TOUPPER(i));
      lower[i] = static_cast<HB_UCHAR>(HB_TOLOWER(i));
    }
    s_utf8_codepage.flags = s_en_codepage.flags;
    s_utf8_codepage.upper = s_en_codepage.upper;
    s_utf8_codepage.lower = s_en_codepage.lower;
    s_utf8_codepage.next = nullptr;
    s_en_codepage.next = &s_utf8_codepage;
    s_cdpList = &s_en_codepage;
  }

  PHB_CODEPAGE *cdp_ptr = &s_cdpList;

  while (*cdp_ptr)
  {
    if (strcmp((*cdp_ptr)->id, id) == 0)
    {
      break;
    }
    if (hb_stricmp((*cdp_ptr)->uniTable->uniID, id) == 0)
    {
      break;
    }
    cdp_ptr = &(*cdp_ptr)->next;
  }

  return cdp_ptr;
}

HB_BOOL hb_cdpRegisterRaw(PHB_CODEPAGE cdp)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdpRegisterRaw(%p)", static_cast<void*>(cdp)));
#endif

  PHB_CODEPAGE *cdp_ptr = hb_cdpFindPos(cdp->id);
  if (*cdp_ptr == nullptr)
  {
    if (!HB_CDP_ISCUSTOM(cdp))
    {
      cdp->wcharGet = hb_cdpStd_get;
      cdp->wcharPut = hb_cdpStd_put;
      cdp->wcharLen = hb_cdpStd_len;
    }
    if (cdp->wcharCmp == nullptr)
    {
      cdp->wcharCmp = cdp->sort == nullptr ? hb_cdpBin_cmp : (cdp->nMulti ? hb_cdpMulti_cmp : hb_cdpStd_cmp);
    }
    if (cdp->wcharCmpI == nullptr)
    {
      cdp->wcharCmpI = cdp->sort == nullptr ? hb_cdpBin_cmpi : (cdp->nMulti ? hb_cdpMulti_cmpi : hb_cdpStd_cmpi);
    }

    if (cdp->wcharCmp == hb_cdpBin_cmp && cdp->wcharCmpI == hb_cdpBin_cmpi)
    {
      cdp->type |= HB_CDP_TYPE_BINSORT;
    }

    *cdp_ptr = cdp;
    return true;
  }
  return false;
}

HB_BOOL hb_cdpRegisterNew(const char *id, const char *info, PHB_UNITABLE uniTable, const char *pszUpper,
                          const char *pszLower, unsigned int nACSort, unsigned int nCaseSort, HB_BOOL fUtf8)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdpRegisterNew(%s,%s,%s,%s,%u,%u,%d)", id, info, pszUpper, pszLower, nACSort, nCaseSort, fUtf8));
#endif

  PHB_CODEPAGE *cdp_ptr = hb_cdpFindPos(id);
  if (*cdp_ptr == nullptr)
  {
    *cdp_ptr = hb_buildCodePage(id, info, uniTable, pszUpper, pszLower, nACSort, nCaseSort, fUtf8);
    return *cdp_ptr != nullptr;
  }
  return false;
}

void hb_cdpReleaseAll(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdpReleaseAll()"));
#endif

  while (s_cdpList)
  {
    void *buffer = s_cdpList->buffer;
    if (s_cdpList->uniTable->uniTrans)
    {
      hb_xfree(s_cdpList->uniTable->uniTrans);
      s_cdpList->uniTable->uniTrans = nullptr;
    }
    s_cdpList = s_cdpList->next;
    if (buffer)
    {
      hb_xfree(buffer);
    }
  }
  if (s_rev_ctrl != nullptr)
  {
    hb_xfree(s_rev_ctrl);
    s_rev_ctrl = nullptr;
  }
}

PHB_CODEPAGE hb_cdpFind(const char *id)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdpFind(%s)", id));
#endif

  return id ? *hb_cdpFindPos(id) : nullptr;
}

PHB_CODEPAGE hb_cdpFindExt(const char *id)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdpFindExt(%s)", id));
#endif

  if (id)
  {
    PHB_CODEPAGE cdp = *hb_cdpFindPos(id);

    if (cdp != nullptr)
    {
      return cdp;
    }

    hb_errRT_BASE(EG_ARG, 1302, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
  return nullptr;
}

HB_BOOL hb_cdpIsUTF8(PHB_CODEPAGE cdp)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdpIsUTF8(%p)", static_cast<void*>(cdp)));
#endif

  if (cdp == nullptr)
  {
    cdp = hb_vmCDP();
  }

  return HB_CDP_ISUTF8(cdp);
}

PHB_CODEPAGE hb_cdpSelect(PHB_CODEPAGE cdp)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdpSelect(%p)", static_cast<void*>(cdp)));
#endif

  auto cdpOld = hb_vmCDP();
  if (cdp != nullptr)
  {
    hb_vmSetCDP(cdp);
  }

  return cdpOld;
}

const char *hb_cdpID(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdpID()"));
#endif

  auto cdp = hb_vmCDP();
  return cdp ? cdp->id : nullptr;
}

const char *hb_cdpSelectID(const char *id)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_cdpSelectID(%s)", id));
#endif

  auto cdp = hb_cdpSelect(hb_cdpFindExt(id));
  return cdp ? cdp->id : nullptr;
}

// Caller must release the pointer
const char **hb_cdpList(void)
{
  PHB_CODEPAGE cdp = s_cdpList;
  int iCount = 0;
  while (cdp != nullptr)
  {
    ++iCount;
    cdp = cdp->next;
  }

  auto list = static_cast<const char **>(hb_xgrab((iCount + 1) * sizeof(char *)));

  cdp = s_cdpList;
  int iPos = 0;
  while (cdp && iPos < iCount)
  {
    list[iPos++] = cdp->id;
    cdp = cdp->next;
  }
  list[iPos] = nullptr;

  return list;
}
