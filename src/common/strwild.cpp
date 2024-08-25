/*
 * Wildcards / file match functions
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.hpp"
#include "hbapicdp.hpp"

#if defined(HB_OS_UNIX) && !defined(HB_NO_FNMATCH)
#include <fnmatch.h>
#endif

#define HB_MAX_WILDPATTERN 256

static bool hb_strMatchWildRaw(const char *szString, const char *szPattern, bool fExact, bool fCase, bool fFile)
{
  auto fMatch = true;
  auto fAny = false;
  HB_SIZE pnBufPosP[HB_MAX_WILDPATTERN], pnBufPosV[HB_MAX_WILDPATTERN], nBufSize = HB_MAX_WILDPATTERN;
  HB_SIZE *nAnyPosP = pnBufPosP, *nAnyPosV = pnBufPosV, nSize, nLen, nAny, nPosP, nPosV;

  nPosP = nPosV = nAny = 0;
  nLen = strlen(szString);
  nSize = strlen(szPattern);
  while (nPosP < nSize || (fExact && !fAny && nPosV < nLen))
  {
    if (nPosP < nSize && szPattern[nPosP] == '*')
    {
      fAny = true;
      nPosP++;
    }
    else if (nPosV < nLen && nPosP < nSize &&
             (szPattern[nPosP] == '?' || (!fCase ? szPattern[nPosP] == szString[nPosV]
                                                 : (hb_charUpper(szPattern[nPosP]) == hb_charUpper(szString[nPosV])))))
    {
      if (fAny)
      {
        if (nAny >= nBufSize)
        {
          if ((nBufSize <<= 1) == (HB_MAX_WILDPATTERN << 1))
          {
            nAnyPosP = static_cast<HB_SIZE *>(hb_xgrab(nBufSize * sizeof(HB_SIZE)));
            nAnyPosV = static_cast<HB_SIZE *>(hb_xgrab(nBufSize * sizeof(HB_SIZE)));
            memcpy(nAnyPosP, pnBufPosP, HB_MAX_WILDPATTERN * sizeof(HB_SIZE));
            memcpy(nAnyPosV, pnBufPosV, HB_MAX_WILDPATTERN * sizeof(HB_SIZE));
          }
          else
          {
            nAnyPosP = static_cast<HB_SIZE *>(hb_xrealloc(nAnyPosP, nBufSize * sizeof(HB_SIZE)));
            nAnyPosV = static_cast<HB_SIZE *>(hb_xrealloc(nAnyPosV, nBufSize * sizeof(HB_SIZE)));
          }
        }
        nAnyPosP[nAny] = nPosP;
        nAnyPosV[nAny] = nPosV;
        nAny++;
        fAny = false;
      }
      nPosV++;
      nPosP++;
    }
    else if (fFile && nPosV == nLen && nPosP < nSize && szPattern[nPosP] == '.' &&
             (nPosP + 1 == nSize || (nPosP + 2 == nSize && szPattern[nPosP + 1] == '*')))
    {
      break;
    }
    else if (fAny && nPosV < nLen)
    {
      nPosV++;
    }
    else if (nAny > 0)
    {
      nAny--;
      nPosP = nAnyPosP[nAny];
      nPosV = nAnyPosV[nAny] + 1;
      fAny = true;
    }
    else
    {
      fMatch = false;
      break;
    }
  }
  if (nBufSize > HB_MAX_WILDPATTERN)
  {
    hb_xfree(nAnyPosP);
    hb_xfree(nAnyPosV);
  }
  return fMatch;
}

static bool hb_strMatchWildCDP(const char *szString, const char *szPattern, bool fExact, bool fCase, bool fFile,
                               PHB_CODEPAGE cdp)
{
  auto fMatch = true;
  auto fAny = false;
  HB_SIZE pnBufPosP[HB_MAX_WILDPATTERN], pnBufPosV[HB_MAX_WILDPATTERN], nBufSize = HB_MAX_WILDPATTERN;
  HB_SIZE *nAnyPosP = pnBufPosP, *nAnyPosV = pnBufPosV, nSize, nLen, nAny, nPosP, nPosV;

  nPosP = nPosV = nAny = 0;
  nLen = strlen(szString);
  nSize = strlen(szPattern);
  while (nPosP < nSize || (fExact && !fAny && nPosV < nLen))
  {
    if (nPosP < nSize && szPattern[nPosP] == '*')
    {
      fAny = true;
      nPosP++;
      continue;
    }

    if (nPosV < nLen && nPosP < nSize)
    {
      HB_SIZE nPP = nPosP, nPV = nPosV;

      if (szPattern[nPosP] == '?')
      {
        nPosP++;
        nPosV += hb_cdpTextPos(cdp, szString + nPosV, nLen - nPosV, 1);
      }
      else if (fCase)
      {
        if (!hb_cdpCharCaseEq(cdp, szString, nLen, &nPosV, szPattern, nSize, &nPosP))
        {
          nPosV = nPV;
          nPosP = nPP;
        }
      }
      else
      {
        if (!hb_cdpCharEq(cdp, szString, nLen, &nPosV, szPattern, nSize, &nPosP))
        {
          nPosV = nPV;
          nPosP = nPP;
        }
      }
      if (nPP != nPosP)
      {
        if (fAny)
        {
          if (nAny >= nBufSize)
          {
            if ((nBufSize <<= 1) == (HB_MAX_WILDPATTERN << 1))
            {
              nAnyPosP = static_cast<HB_SIZE *>(hb_xgrab(nBufSize * sizeof(HB_SIZE)));
              nAnyPosV = static_cast<HB_SIZE *>(hb_xgrab(nBufSize * sizeof(HB_SIZE)));
              memcpy(nAnyPosP, pnBufPosP, HB_MAX_WILDPATTERN * sizeof(HB_SIZE));
              memcpy(nAnyPosV, pnBufPosV, HB_MAX_WILDPATTERN * sizeof(HB_SIZE));
            }
            else
            {
              nAnyPosP = static_cast<HB_SIZE *>(hb_xrealloc(nAnyPosP, nBufSize * sizeof(HB_SIZE)));
              nAnyPosV = static_cast<HB_SIZE *>(hb_xrealloc(nAnyPosV, nBufSize * sizeof(HB_SIZE)));
            }
          }
          nAnyPosP[nAny] = nPP;
          nAnyPosV[nAny] = nPosV;
          nAny++;
          fAny = false;
        }
        continue;
      }
    }

    if (fFile && nPosV == nLen && nPosP < nSize && szPattern[nPosP] == '.' &&
        (nPosP + 1 == nSize || (nPosP + 2 == nSize && szPattern[nPosP + 1] == '*')))
    {
      break;
    }
    else if (fAny && nPosV < nLen)
    {
      nPosV += hb_cdpTextPos(cdp, szString + nPosV, nLen - nPosV, 1);
    }
    else if (nAny > 0)
    {
      nAny--;
      nPosP = nAnyPosP[nAny];
      nPosV = nAnyPosV[nAny];
      fAny = true;
    }
    else
    {
      fMatch = false;
      break;
    }
  }
  if (nBufSize > HB_MAX_WILDPATTERN)
  {
    hb_xfree(nAnyPosP);
    hb_xfree(nAnyPosV);
  }
  return fMatch;
}

HB_BOOL hb_strMatchWild(const char *szString, const char *szPattern)
{
  auto cdp = hb_vmCDP();

  if (cdp && HB_CDP_ISCHARIDX(cdp))
  {
    return hb_strMatchWildCDP(szString, szPattern, false, false, false, cdp);
  }
  else
  {
    return hb_strMatchWildRaw(szString, szPattern, false, false, false);
  }
}

HB_BOOL hb_strMatchWildExact(const char *szString, const char *szPattern)
{
  auto cdp = hb_vmCDP();

  if (cdp && HB_CDP_ISCHARIDX(cdp))
  {
    return hb_strMatchWildCDP(szString, szPattern, true, false, false, cdp);
  }
  else
  {
    return hb_strMatchWildRaw(szString, szPattern, true, false, false);
  }
}

HB_BOOL hb_strMatchCaseWildExact(const char *szString, const char *szPattern)
{
  auto cdp = hb_vmCDP();

  if (cdp && HB_CDP_ISCHARIDX(cdp))
  {
    return hb_strMatchWildCDP(szString, szPattern, true, true, false, cdp);
  }
  else
  {
    return hb_strMatchWildRaw(szString, szPattern, true, true, false);
  }
}

HB_BOOL hb_strMatchFile(const char *szString, const char *szPattern)
{
#if defined(HB_OS_UNIX)
#if defined(HB_NO_FNMATCH)
  return hb_strMatchWildExact(szString, szPattern);
#else
  return fnmatch(szPattern, szString, FNM_PATHNAME) == 0;
#endif
#elif defined(HB_OS_WIN)
  auto cdp = hb_vmCDP();

  if (cdp && HB_CDP_ISCHARIDX(cdp))
  {
    return hb_strMatchWildCDP(szString, szPattern, true, true, true, cdp);
  }
  else
  {
    return hb_strMatchWildRaw(szString, szPattern, true, true, true);
  }
#else
  return hb_strMatchCaseWildExact(szString, szPattern);
#endif
}
