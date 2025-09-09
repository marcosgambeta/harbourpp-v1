//
// Harbour common string functions (accessed from standalone utilities and the RTL)
//
// Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
// Copyright 1999 David G. Holm <dholm@jsd-llc.com> (hb_stricmp())
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
#include "hbmath.hpp"

// clang-format off
const char * const hb_szAscii[256] = {
   "\x00", "\x01", "\x02", "\x03", "\x04", "\x05", "\x06", "\x07", "\x08", "\x09", "\x0A", "\x0B", "\x0C", "\x0D", "\x0E", "\x0F",
   "\x10", "\x11", "\x12", "\x13", "\x14", "\x15", "\x16", "\x17", "\x18", "\x19", "\x1A", "\x1B", "\x1C", "\x1D", "\x1E", "\x1F",
   "\x20", "\x21", "\x22", "\x23", "\x24", "\x25", "\x26", "\x27", "\x28", "\x29", "\x2A", "\x2B", "\x2C", "\x2D", "\x2E", "\x2F",
   "\x30", "\x31", "\x32", "\x33", "\x34", "\x35", "\x36", "\x37", "\x38", "\x39", "\x3A", "\x3B", "\x3C", "\x3D", "\x3E", "\x3F",
   "\x40", "\x41", "\x42", "\x43", "\x44", "\x45", "\x46", "\x47", "\x48", "\x49", "\x4A", "\x4B", "\x4C", "\x4D", "\x4E", "\x4F",
   "\x50", "\x51", "\x52", "\x53", "\x54", "\x55", "\x56", "\x57", "\x58", "\x59", "\x5A", "\x5B", "\x5C", "\x5D", "\x5E", "\x5F",
   "\x60", "\x61", "\x62", "\x63", "\x64", "\x65", "\x66", "\x67", "\x68", "\x69", "\x6A", "\x6B", "\x6C", "\x6D", "\x6E", "\x6F",
   "\x70", "\x71", "\x72", "\x73", "\x74", "\x75", "\x76", "\x77", "\x78", "\x79", "\x7A", "\x7B", "\x7C", "\x7D", "\x7E", "\x7F",
   "\x80", "\x81", "\x82", "\x83", "\x84", "\x85", "\x86", "\x87", "\x88", "\x89", "\x8A", "\x8B", "\x8C", "\x8D", "\x8E", "\x8F",
   "\x90", "\x91", "\x92", "\x93", "\x94", "\x95", "\x96", "\x97", "\x98", "\x99", "\x9A", "\x9B", "\x9C", "\x9D", "\x9E", "\x9F",
   "\xA0", "\xA1", "\xA2", "\xA3", "\xA4", "\xA5", "\xA6", "\xA7", "\xA8", "\xA9", "\xAA", "\xAB", "\xAC", "\xAD", "\xAE", "\xAF",
   "\xB0", "\xB1", "\xB2", "\xB3", "\xB4", "\xB5", "\xB6", "\xB7", "\xB8", "\xB9", "\xBA", "\xBB", "\xBC", "\xBD", "\xBE", "\xBF",
   "\xC0", "\xC1", "\xC2", "\xC3", "\xC4", "\xC5", "\xC6", "\xC7", "\xC8", "\xC9", "\xCA", "\xCB", "\xCC", "\xCD", "\xCE", "\xCF",
   "\xD0", "\xD1", "\xD2", "\xD3", "\xD4", "\xD5", "\xD6", "\xD7", "\xD8", "\xD9", "\xDA", "\xDB", "\xDC", "\xDD", "\xDE", "\xDF",
   "\xE0", "\xE1", "\xE2", "\xE3", "\xE4", "\xE5", "\xE6", "\xE7", "\xE8", "\xE9", "\xEA", "\xEB", "\xEC", "\xED", "\xEE", "\xEF",
   "\xF0", "\xF1", "\xF2", "\xF3", "\xF4", "\xF5", "\xF6", "\xF7", "\xF8", "\xF9", "\xFA", "\xFB", "\xFC", "\xFD", "\xFE", "\xFF"
};
// clang-format on

HB_SIZE hb_strAt(const char *szSub, HB_SIZE nSubLen, const char *szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strAt(%s, %" HB_PFS "u, %s, %" HB_PFS "u)", szSub, nSubLen, szText, nLen));
#endif

  if (nSubLen > 0 && nLen >= nSubLen) {
    HB_SIZE nPos = 0;
    nLen -= nSubLen;
    do {
      if (szText[nPos] == *szSub) {
        HB_SIZE nSubPos = nSubLen;
        do {
          if (--nSubPos == 0) {
            return nPos + 1;
          }
        } while (szText[nPos + nSubPos] == szSub[nSubPos]);
      }
    } while (nPos++ < nLen);
  }

  return 0;
}

HB_SIZE hb_strAtI(const char *szSub, HB_SIZE nSubLen, const char *szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strAt(%s, %" HB_PFS "u, %s, %" HB_PFS "u)", szSub, nSubLen, szText, nLen));
#endif

  if (nSubLen > 0 && nLen >= nSubLen) {
    HB_SIZE nPos = 0;
    nLen -= nSubLen;
    do {
      if (HB_TOUPPER(szText[nPos]) == HB_TOUPPER(*szSub)) {
        HB_SIZE nSubPos = nSubLen;
        do {
          if (--nSubPos == 0) {
            return nPos + 1;
          }
        } while (HB_TOUPPER(szText[nPos + nSubPos]) == HB_TOUPPER(szSub[nSubPos]));
      }
    } while (nPos++ < nLen);
  }

  return 0;
}

HB_BOOL hb_strEmpty(const char *szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strEmpty(%s, %" HB_PFS "u)", szText, nLen));
#endif

  while (nLen--) {
    char c = szText[nLen];

    if (!HB_ISSPACE(c)) {
      return false;
    }
  }

  return true;
}

char *hb_strupr(char *pszText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strupr(%s)", pszText));
#endif

  for (char *pszPos = pszText; *pszPos; pszPos++) {
    *pszPos = static_cast<char>(HB_TOUPPER(static_cast<HB_UCHAR>(*pszPos)));
  }

  return pszText;
}

char *hb_strlow(char *pszText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strlow(%s)", pszText));
#endif

  for (char *pszPos = pszText; *pszPos; pszPos++) {
    *pszPos = static_cast<char>(HB_TOLOWER(static_cast<HB_UCHAR>(*pszPos)));
  }

  return pszText;
}

char *hb_strdup(const char *pszText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strdup(%s)", pszText));
#endif

  HB_SIZE nLen = strlen(pszText) + 1;
  auto pszDup = static_cast<char *>(hb_xgrab(nLen));
  memcpy(pszDup, pszText, nLen);

  return pszDup;
}

char *hb_strndup(const char *pszText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strndup(%.*s, %" HB_PFS "d)", static_cast<int>(nLen), pszText, nLen));
#endif

  HB_SIZE nPos = 0;
  while (nLen-- && pszText[nPos]) {
    ++nPos;
  }

  auto pszDup = static_cast<char *>(hb_xgrab(nPos + 1));
  memcpy(pszDup, pszText, nPos);
  pszDup[nPos] = '\0';

  return pszDup;
}

HB_SIZE hb_strnlen(const char *pszText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strnlen(%.*s, %" HB_PFS "d)", static_cast<int>(nLen), pszText, nLen));
#endif

  HB_SIZE nPos = 0;

  while (nLen-- && *pszText++) {
    ++nPos;
  }

  return nPos;
}

char *hb_strduptrim(const char *pszText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strduptrim(%s)", pszText));
#endif

  while (pszText[0] == ' ') {
    ++pszText;
  }

  HB_SIZE nLen = strlen(pszText);
  while (nLen && pszText[nLen - 1] == ' ') {
    --nLen;
  }

  auto pszDup = static_cast<char *>(hb_xgrab(nLen + 1));
  memcpy(pszDup, pszText, nLen);
  pszDup[nLen] = '\0';

  return pszDup;
}

HB_SIZE hb_strlentrim(const char *pszText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strlentrim(%s)", pszText));
#endif

  HB_SIZE nPos = 0;

  while (pszText[0] == ' ') {
    ++pszText;
  }

  while (pszText[nPos]) {
    ++nPos;
  }

  while (nPos && pszText[nPos - 1] == ' ') {
    --nPos;
  }

  return nPos;
}

int hb_stricmp(const char *s1, const char *s2)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_stricmp(%s, %s)", s1, s2));
#endif

  int rc = 0, c1;

  do {
    int c2;

    c1 = HB_TOUPPER(static_cast<unsigned char>(*s1));
    c2 = HB_TOUPPER(static_cast<unsigned char>(*s2));

    if (c1 != c2) {
      rc = (c1 < c2 ? -1 : 1);
      break;
    }

    s1++;
    s2++;
  } while (c1);

  return rc;
}

// Warning: It is not case sensitive
int hb_strnicmp(const char *s1, const char *s2, HB_SIZE count)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strnicmp(%.*s, %.*s, %" HB_PFS "u)", static_cast<int>(count), s1, static_cast<int>(count), s2, count));
#endif

  int rc = 0;

  for (HB_SIZE nCount = 0; nCount < count; nCount++) {
    unsigned char c1 = static_cast<char>(HB_TOUPPER(static_cast<unsigned char>(s1[nCount])));
    unsigned char c2 = static_cast<char>(HB_TOUPPER(static_cast<unsigned char>(s2[nCount])));

    if (c1 != c2) {
      rc = (c1 < c2 ? -1 : 1);
      break;
    } else if (!c1) {
      break;
    }
  }

  return rc;
}

// AJ: 2004-02-23
// Concatenates multiple strings into a single result.
// Eg. hb_xstrcat(buffer, "A", "B", nullptr) stores "AB" in buffer.
char *hb_xstrcat(char *szDest, const char *szSrc, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xstrcat(%p, %p, ...)", static_cast<void*>(szDest), static_cast<const void*>(szSrc)));
#endif

  char *szResult = szDest;
  va_list va;

  while (*szDest) {
    szDest++;
  }

  va_start(va, szSrc);
  while (szSrc) {
    while (*szSrc) {
      *szDest++ = *szSrc++;
    }
    szSrc = va_arg(va, char *);
  }
  *szDest = '\0';
  va_end(va);

  return szResult;
}

// AJ: 2004-02-23
// Concatenates multiple strings into a single result.
// Eg. hb_xstrcpy(buffer, "A", "B", nullptr) stores "AB" in buffer.
// Returns szDest.
// Any existing contents of szDest are cleared. If the szDest buffer is nullptr,
// allocates a new buffer with the required length and returns that. The
// buffer is allocated using hb_xgrab(), and should eventually be freed
// using hb_xfree().
char *hb_xstrcpy(char *szDest, const char *szSrc, ...)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_xstrcpy(%p, %p, ...)", static_cast<void*>(szDest), static_cast<const void*>(szSrc)));
#endif

  va_list va;

  if (szDest == nullptr) {
    const char *szSrcPtr = szSrc;
    HB_SIZE nSize = 1;
    va_start(va, szSrc);
    while (szSrcPtr) {
      nSize += strlen(szSrcPtr);
      szSrcPtr = va_arg(va, char *);
    }
    va_end(va);
    szDest = static_cast<char *>(hb_xgrab(nSize));
  }
  char *szResult = szDest;

  va_start(va, szSrc);
  while (szSrc) {
    while (*szSrc) {
      *szDest++ = *szSrc++;
    }
    szSrc = va_arg(va, char *);
  }
  *szDest = '\0';
  va_end(va);

  return szResult;
}

static double hb_numPow10(int nPrecision)
{
  static const double s_dPow10[16] = {1.0,                 // 0
                                      10.0,                // 1
                                      100.0,               // 2
                                      1000.0,              // 3
                                      10000.0,             // 4
                                      100000.0,            // 5
                                      1000000.0,           // 6
                                      10000000.0,          // 7
                                      100000000.0,         // 8
                                      1000000000.0,        // 9
                                      10000000000.0,       // 10
                                      100000000000.0,      // 11
                                      1000000000000.0,     // 12
                                      10000000000000.0,    // 13
                                      100000000000000.0,   // 14
                                      1000000000000000.0}; // 15

  if (nPrecision < 16) {
    if (nPrecision >= 0) {
      return s_dPow10[nPrecision];
    } else if (nPrecision > -16) {
      return 1.0 / s_dPow10[static_cast<unsigned int>(-nPrecision)];
    }
  }

  return pow(10.0, static_cast<double>(nPrecision));
}

double hb_numRound(double dNum, int iDec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_numRound(%lf, %d)", dNum, iDec));
#endif

  static const double doBase = 10.0;
  double doComplete5, doComplete5i, dPow;

  if (dNum == 0.0) {
    return 0.0;
  }

  if (iDec < 0) {
    dPow = hb_numPow10(-iDec);
    doComplete5 = dNum / dPow * doBase;
  } else {
    dPow = hb_numPow10(iDec);
    doComplete5 = dNum * dPow * doBase;
  }

  // double precision if 15 digit the 16th one is usually wrong but
  // can give some information about number,
  // Clipper display 16 digit only others are set to 0
  // many people don't know/understand FL arithmetic. They expect
  // that it will behaves in the same way as real numbers. It's not
  // true but in business application we can try to hide this problem
  // for them. Usually they not need such big precision in presented
  // numbers so we can decrease the precision to 15 digits and use
  // the cut part for proper rounding. It should resolve
  // most of problems. But if someone totally  not understand FL
  // and will try to convert big matrix or something like that it's quite
  // possible that he chose one of the natural school algorithm which
  // works nice with real numbers but can give very bad results in FL.
  // In such case it could be good to decrease precision even more.
  // It not fixes the used algorithm of course but will make many users
  // happy because they can see nice (proper) result.
  // So maybe it will be good to add SET PRECISION TO <n> for them and
  // use the similar hack in ==, >=, <=, <, > operations if it's set.

  // #define HB_NUM_PRECISION  16

#ifdef HB_NUM_PRECISION
  // this is a hack for people who cannot live without hacked FL values
  // in rounding
  {
    int iPrec;
    auto fNeg = false;

    if (dNum < 0) {
      fNeg = true;
      dNum = -dNum;
    } else {
      fNeg = false;
    }

    auto iDecR = static_cast<int>(log10(dNum));
    iPrec = iDecR + iDec;

    if (iPrec < -1) {
      return 0.0;
    } else {
      if (iPrec > HB_NUM_PRECISION) {
        iDec = HB_NUM_PRECISION - (dNum < 1.0 ? 0 : 1) - iDecR;
        iPrec = -1;
      } else {
        iPrec -= HB_NUM_PRECISION;
      }
    }
    if (iDec < 0) {
      dPow = hb_numPow10(-iDec);
      doComplete5 = dNum / dPow * doBase + 5.0 + hb_numPow10(iPrec);
    } else {
      dPow = hb_numPow10(iDec);
      doComplete5 = dNum * dPow * doBase + 5.0 + hb_numPow10(iPrec);
    }

    if (fNeg) {
      doComplete5 = -doComplete5;
    }
  }
#else
  if (dNum < 0.0) {
    doComplete5 -= 5.0;
  } else {
    doComplete5 += 5.0;
  }
#endif

  doComplete5 /= doBase;

#if defined(HB_DBLFL_PREC_FACTOR) && !defined(HB_CLP_STRICT)
  // similar operation is done by Cl5.3
  //  it's a hack to force rounding FL values UP
  doComplete5 *= HB_DBLFL_PREC_FACTOR;
#endif

  (void)modf(doComplete5, &doComplete5i); // TODO: C++ cast

  if (iDec < 0) {
    return doComplete5i * dPow;
  } else {
    return doComplete5i / dPow;
  }
}

double hb_numInt(double dNum)
{
  double dInt;

#if defined(HB_DBLFL_PREC_FACTOR) && !defined(HB_CLP_STRICT)
  // Similar hack as in round to make this functions compatible
  dNum *= HB_DBLFL_PREC_FACTOR;
#endif
  (void)modf(dNum, &dInt); // TODO: C++ cast

  return dInt;
}

double hb_numDecConv(double dNum, int iDec)
{
  if (iDec > 0) {
    return hb_numRound(dNum / hb_numPow10(iDec), iDec);
  } else if (iDec < 0) {
    return hb_numRound(dNum * hb_numPow10(-iDec), 0);
  } else {
    return hb_numRound(dNum, 0);
  }
}

double hb_numExpConv(double dNum, int iExp)
{
  if (iExp > 0) {
    return dNum / hb_numPow10(iExp);
  } else if (iExp < 0) {
    return dNum * hb_numPow10(-iExp);
  } else {
    return dNum;
  }
}

static bool hb_str2number(bool fPCode, const char *szNum, HB_SIZE nLen, HB_MAXINT *lVal, double *dVal, int *piDec,
                          int *piWidth)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_str2number(%d, %p, %" HB_PFS "u, %p, %p, %p, %p)", static_cast<int>(fPCode), static_cast<const void*>(szNum), nLen, static_cast<void*>(lVal), static_cast<void*>(dVal), static_cast<void*>(piDec), static_cast<void*>(piWidth)));
#endif

  auto fDbl = false;
  auto fDec = false;
  auto fNeg = false;
  auto fHex = false;
  int iPos = 0;
  int c, iWidth, iDec = 0, iDecR = 0;

  auto iLen = static_cast<int>(nLen);

  while (iPos < iLen && HB_ISSPACE(szNum[iPos])) {
    iPos++;
  }

  if (iPos >= iLen) {
    fNeg = false;
  } else if (szNum[iPos] == '-') {
    fNeg = true;
    iPos++;
  } else {
    fNeg = false;
    if (szNum[iPos] == '+') {
      iPos++;
    }
  }

  *dVal = 0;
  *lVal = 0;

  // Hex Number
  if (fPCode && iPos + 1 < iLen && szNum[iPos] == '0' && (szNum[iPos + 1] == 'X' || szNum[iPos + 1] == 'x')) {
    iPos += 2;
    iWidth = HB_DEFAULT_WIDTH;
    fHex = true;
    for (; iPos < iLen; iPos++) {
      c = szNum[iPos];
      if (c >= '0' && c <= '9') {
        c -= '0';
      } else if (c >= 'A' && c <= 'F') {
        c -= 'A' - 10;
      } else if (c >= 'a' && c <= 'f') {
        c -= 'a' - 10;
      } else {
        break;
      }
      *lVal = (*lVal << 4) + c;
    }
  } else {
    HB_MAXINT lLimV;

    lLimV = HB_VMLONG_MAX / 10;
    auto iLimC = static_cast<int>(HB_VMLONG_MAX % 10);

    iWidth = iPos;

    for (; iPos < iLen; iPos++) {
      c = szNum[iPos];
      if (c >= '0' && c <= '9') {
        if (fDbl) {
          *dVal = *dVal * 10.0 + (c - '0');
        } else if (*lVal < lLimV || (*lVal <= lLimV && (static_cast<int>(c - '0')) <= iLimC)) {
          *lVal = *lVal * 10 + (c - '0');
        } else {
          *dVal = static_cast<double>(*lVal) * 10.0 + (c - '0');
          fDbl = true;
        }
        if (fDec) {
          iDec++;
        } else {
          iWidth++;
        }
      } else if (c == '.' && !fDec) {
        fDec = true;
      } else {
        while (!fDec && iPos < iLen) {
          if (szNum[iPos++] == '.') {
            fDec = true;
          } else {
            iWidth++;
          }
        }
        if (fDec) {
          iDecR = iLen - iPos;
        }
        break;
      }
    }
  }

  if (fNeg) {
    if (fDbl) {
      *dVal = -*dVal;
    } else {
      *lVal = -*lVal;
    }
  }
  if (!fDbl && (
#if defined(PCODE_LONG_LIM)
                   (fPCode && !fHex && !PCODE_LONG_LIM(*lVal)) ||
#endif
                   fDec)) {
    *dVal = static_cast<double>(*lVal);
    fDbl = true;
  }
  if (iDec) {
    *dVal /= hb_numPow10(iDec);
  }

  if (piDec) {
    *piDec = iDec + iDecR;
  }
  if (piWidth) {
    if (fHex) {
      *piWidth = iWidth;
    } else {
      if (fPCode) {
        if (iWidth < 10 || fNeg) {
          *piWidth = fDbl ? HB_DBL_LENGTH(*dVal) : HB_LONG_LENGTH(*lVal);
        } else {
          *piWidth = iWidth + (iDec == 0 ? 1 : 0);
        }
      } else if (iWidth > 10) {
        *piWidth = fDbl ? HB_DBL_LENGTH(*dVal) : HB_LONG_LENGTH(*lVal);
      } else {
        if (iDec + iDecR == 0) {
          *piWidth = iLen;
        } else if (iWidth == 0) {
          *piWidth = 1;
        } else if (fNeg && iWidth == 1 && *dVal != 0) {
          *piWidth = 2;
        } else {
          *piWidth = iWidth;
        }
      }
    }
  }

  return fDbl;
}

HB_BOOL hb_compStrToNum(const char *szNum, HB_SIZE nLen, HB_MAXINT *plVal, double *pdVal, int *piDec, int *piWidth)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_compStrToNum( %s, %" HB_PFS "u, %p, %p, %p, %p)", szNum, nLen, static_cast<void*>(plVal), static_cast<void*>(pdVal), static_cast<void*>(piDec), static_cast<void*>(piWidth)));
#endif
  return hb_str2number(true, szNum, nLen, plVal, pdVal, piDec, piWidth);
}

HB_BOOL hb_valStrnToNum(const char *szNum, HB_SIZE nLen, HB_MAXINT *plVal, double *pdVal, int *piDec, int *piWidth)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_valStrnToNum( %s, %" HB_PFS "u, %p, %p, %p, %p)", szNum, nLen, static_cast<void*>(plVal), static_cast<void*>(pdVal), static_cast<void*>(piDec), static_cast<void*>(piWidth)));
#endif
  return hb_str2number(false, szNum, nLen, plVal, pdVal, piDec, piWidth);
}

HB_BOOL hb_strToNum(const char *szNum, HB_MAXINT *plVal, double *pdVal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strToNum(%s, %p, %p)", szNum, static_cast<void*>(plVal), static_cast<void*>(pdVal)));
#endif
  return hb_str2number(false, szNum, strlen(szNum), plVal, pdVal, nullptr, nullptr);
}

HB_BOOL hb_strnToNum(const char *szNum, HB_SIZE nLen, HB_MAXINT *plVal, double *pdVal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strnToNum(%.*s, %" HB_PFS "u, %p, %p)", static_cast<int>(nLen), szNum, nLen, static_cast<void*>(plVal), static_cast<void*>(pdVal)));
#endif
  return hb_str2number(false, szNum, nLen, plVal, pdVal, nullptr, nullptr);
}

// returns the numeric value of a character string representation of a number
double hb_strVal(const char *szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strVal(%.*s, %" HB_PFS "u)", static_cast<int>(nLen), szText, nLen));
#endif

  HB_MAXINT lVal;
  double dVal;

  if (!hb_str2number(false, szText, nLen, &lVal, &dVal, nullptr, nullptr)) {
    dVal = static_cast<double>(lVal);
  }
  return dVal;
}

HB_MAXINT hb_strValInt(const char *szText, int *iOverflow)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strValInt(%s)", szText));
#endif

  HB_MAXINT lVal;
  double dVal;

  if (hb_str2number(true, szText, strlen(szText), &lVal, &dVal, nullptr, nullptr)) {
    *iOverflow = 1;
    return 0;
  }
  *iOverflow = 0;
  return lVal;
}

char *hb_numToStr(char *szBuf, HB_SIZE nSize, HB_MAXINT lNumber)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_numToStr(%p, %" HB_PFS "u, %" PFHL "i)", static_cast<void*>(szBuf), nSize, lNumber));
#endif

  auto iPos = static_cast<int>(nSize);
  auto fNeg = false;

  szBuf[--iPos] = '\0';
  if (lNumber < 0) {
    fNeg = true;
    lNumber = -lNumber;
  }

  while (--iPos >= 0) {
    szBuf[iPos] = '0' + static_cast<char>(lNumber % 10);
    lNumber /= 10;
    if (lNumber == 0) {
      break;
    }
  }
  if (fNeg && --iPos >= 0) {
    szBuf[iPos] = '-';
  }

  if (iPos > 0) {
    memset(szBuf, ' ', iPos);
  } else if (iPos < 0) {
    memset(szBuf, '*', nSize - 1);
    iPos = 0;
  }

  return &szBuf[iPos];
}

// if you want to be sure that size of buffer is enough to hold each
// double number with '\0' terminating character then it should have
// at least HB_MAX_DOUBLE_LENGTH bytes. If buffer is not large enough
// then nullptr is returned
char *hb_dblToStr(char *szBuf, HB_SIZE nSize, double dNumber, int iMaxDec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dblToStr(%p, %" HB_PFS "u, %f, %d)", static_cast<void*>(szBuf), nSize, dNumber, iMaxDec));
#endif

  double dInt, dFract, dDig, doBase = 10.0;
  int iPos, iPrec;
  char *szResult;
  auto fFirst = false;

  auto iLen = static_cast<int>(nSize - 1);
  if (iLen <= 0) {
    return nullptr;
  }
#ifdef HB_NUM_PRECISION
  iPrec = HB_NUM_PRECISION;
#else
  iPrec = 16;
#endif
  szResult = szBuf;
  if (dNumber < 0) {
    dFract = modf(-dNumber, &dInt);
    if (--iLen == 0) {
      if (dInt < 1 && dFract < 0.5) {
        szBuf[0] = '0';
        szBuf[1] = '\0';
        return szBuf;
      }
      return nullptr;
    }
    *szBuf++ = '-';
  } else {
    dFract = modf(dNumber, &dInt);
  }

  iPos = iLen;
  do {
    if (iPos == 0) {
      return nullptr;
    }
    dDig = modf(dInt / doBase + 0.01, &dInt) * doBase;
    szBuf[--iPos] = '0' + static_cast<char>(dDig + 0.01);
  } while (dInt >= 1);
  if (iPos > 0) {
    memmove(szBuf, szBuf + iPos, HB_MIN(iLen - iPos, iPrec + 1));
  }
  iPos = iLen - iPos;

  fFirst = iPos > 1 || szBuf[0] != '0';
  if (fFirst) {
    if (iPos >= iPrec) {
      fFirst = iPos == iPrec ? dFract >= 0.5 : szBuf[iPrec] >= '5';
      if (iPrec < iPos) {
        memset(szBuf + iPrec, '0', iPos - iPrec);
      }
      if (fFirst) {
        for (;;) {
          if (--iPrec < 0) {
            if (iPos == iLen) {
              return nullptr;
            }
            memmove(szBuf + 1, szBuf, iPos);
            *szBuf = '1';
            ++iPos;
            break;
          }
          if (szBuf[iPrec] != '9') {
            ++szBuf[iPrec];
            break;
          }
          szBuf[iPrec] = '0';
        }
      }
      iPrec = 0;
    } else {
      iPrec -= iPos;
    }
  }

  if (iPrec > 0 && iLen - iPos > 1 && iMaxDec != 0 && dFract > 0) {
    int iDec = iPos;

    szBuf[iPos] = '.';
    while (++iPos < iLen && iPrec > 0 && iMaxDec-- != 0) {
      dFract = modf(dFract * doBase, &dDig);
      szBuf[iPos] = '0' + static_cast<char>(dDig + 0.01);
      if (szBuf[iPos] != '0') {
        fFirst = true;
      }
      if (fFirst) {
        --iPrec;
      }
    }
    if (dFract > (iPrec > 0 ? 0.5 - hb_numPow10(-iPrec) : 0.2)) {
      iPrec = iPos;
      for (;;) {
        if (--iPrec < 0) {
          memmove(szBuf + 1, szBuf, iPos);
          *szBuf = '1';
          if (iPos < iLen) {
            ++iPos;
          }
          ++iDec;
          break;
        }
        if (iPrec == iDec) {
          --iPrec;
        }
        if (szBuf[iPrec] != '9') {
          ++szBuf[iPrec];
          break;
        }
        szBuf[iPrec] = '0';
      }
    }
    while (iPos > iDec && szBuf[iPos - 1] == '0') {
      --iPos;
    }
    if (szBuf[iPos - 1] == '.') {
      --iPos;
    }
  }

  szBuf[iPos] = '\0';
  return iPos == 1 && *szResult == '-' && *szBuf == '0' ? szBuf : szResult;
}

// This function copies szText to destination buffer.
// NOTE: Unlike the documentation for strncpy, this routine will always append
//       a null and the nLen param is pDest size not pSource limit
char *hb_strncpy(char *pDest, const char *pSource, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strncpy(%p, %.*s, %" HB_PFS "u)", static_cast<void*>(pDest), static_cast<int>(nLen), pSource, nLen));
#endif

  char *pBuf = pDest;

  pDest[nLen] = '\0';

  while (nLen && (*pDest++ = *pSource++) != '\0') {
    nLen--;
  }

  return pBuf;
}

// This function copies szText to destination buffer.
// NOTE: Unlike the documentation for strncat, this routine will always append
//       a null and the nLen param is pDest size not pSource limit
char *hb_strncat(char *pDest, const char *pSource, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strncat(%p, %.*s, %" HB_PFS "u)", static_cast<void*>(pDest), static_cast<int>(nLen), pSource, nLen));
#endif

  char *pBuf = pDest;

  pDest[nLen] = '\0';

  while (nLen && *pDest) {
    pDest++;
    nLen--;
  }

  while (nLen && (*pDest++ = *pSource++) != '\0') {
    nLen--;
  }

  return pBuf;
}

// This function copies and converts szText to lower case.

// NOTE: Unlike the documentation for strncpy, this routine will always append
//       a null [pt]
char *hb_strncpyLower(char *pDest, const char *pSource, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strncpyLower(%p, %.*s, %" HB_PFS "u)", static_cast<void*>(pDest), static_cast<int>(nLen), pSource, nLen));
#endif

  char *pBuf = pDest;

  pDest[nLen] = '\0';

  while (nLen && (*pDest++ = static_cast<char>(HB_TOLOWER(static_cast<HB_UCHAR>(*pSource)))) != '\0') {
    nLen--;
    pSource++;
  }

  return pBuf;
}

// This function copies and converts szText to upper case.

// NOTE: Unlike the documentation for strncpy, this routine will always append
//       a null [pt]
char *hb_strncpyUpper(char *pDest, const char *pSource, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strncpyUpper(%p, %.*s, %" HB_PFS "u)", static_cast<void*>(pDest), static_cast<int>(nLen), pSource, nLen));
#endif

  char *pBuf = pDest;

  pDest[nLen] = '\0';

  while (nLen && (*pDest++ = static_cast<char>(HB_TOUPPER(static_cast<HB_UCHAR>(*pSource)))) != '\0') {
    nLen--;
    pSource++;
  }

  return pBuf;
}

// This function copies and converts szText to upper case AND Trims it

// NOTE: Unlike the documentation for strncpy, this routine will always append
//       a null [pt]
char *hb_strncpyUpperTrim(char *pDest, const char *pSource, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strncpyUpperTrim(%p, %.*s, %" HB_PFS "u)", static_cast<void*>(pDest), static_cast<int>(nLen), pSource, nLen));
#endif

  char *pBuf = pDest;
  HB_SIZE nSLen;

  nSLen = 0;
  while (nSLen < nLen && pSource[nSLen]) {
    nSLen++;
  }

  while (nSLen && pSource[nSLen - 1] == ' ') {
    nSLen--;
  }

  while (nLen && nSLen && (*pDest++ = static_cast<char>(HB_TOUPPER(static_cast<HB_UCHAR>(*pSource)))) != '\0') {
    nSLen--;
    nLen--;
    pSource++;
  }

  *pDest = '\0';

  return pBuf;
}

// This function copies trimed szText to destination buffer.

// NOTE: Unlike the documentation for strncpy, this routine will always append
//       a null
char *hb_strncpyTrim(char *pDest, const char *pSource, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strncpyTrim(%p, %.*s, %" HB_PFS "u)", static_cast<void*>(pDest), static_cast<int>(nLen), pSource, nLen));
#endif

  char *pBuf = pDest;
  HB_SIZE nSLen;

  nSLen = 0;
  while (nSLen < nLen && pSource[nSLen]) {
    nSLen++;
  }

  while (nSLen && pSource[nSLen - 1] == ' ') {
    nSLen--;
  }

  while (nLen && nSLen && (*pDest++ = *pSource++) != '\0') {
    nSLen--;
    nLen--;
  }

  *pDest = '\0';

  return pBuf;
}

char *hb_strRemEscSeq(char *str, HB_SIZE *pnLen)
{
  HB_SIZE nPos = *pnLen, nStripped = 0;
  char *ptr, *dst;

  ptr = dst = str;
  while (nPos) {
    if (*ptr == '\\') {
      break;
    }
    ++ptr;
    ++dst;
    --nPos;
  }

  while (nPos--) {
    char ch = *ptr++;
    if (ch == '\\') {
      ++nStripped;
      if (nPos) {
        nPos--;
        ch = *ptr++;
        switch (ch) {
        case 'r':
          ch = '\r';
          break;
        case 'n':
          ch = '\n';
          break;
        case 't':
          ch = '\t';
          break;
        case 'b':
          ch = '\b';
          break;
        case 'f':
          ch = '\f';
          break;
        case 'v':
          ch = '\v';
          break;
        case 'a':
          ch = '\a';
          break;
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
          ch -= '0';
          if (nPos && *ptr >= '0' && *ptr <= '7') {
            ch = (ch << 3) | (*ptr++ - '0');
            ++nStripped;
            if (--nPos && *ptr >= '0' && *ptr <= '7') {
              ch = (ch << 3) | (*ptr++ - '0');
              ++nStripped;
              --nPos;
            }
          }
          break;
        case 'x':
          ch = 0;
          while (nPos) {
            if (*ptr >= '0' && *ptr <= '9') {
              ch = (ch << 4) | (*ptr++ - '0');
            } else if (*ptr >= 'A' && *ptr <= 'F') {
              ch = (ch << 4) | (*ptr++ - 'A' + 10);
            } else if (*ptr >= 'a' && *ptr <= 'f') {
              ch = (ch << 4) | (*ptr++ - 'a' + 10);
            } else {
              break;
            }
            ++nStripped;
            --nPos;
          }
          break;
        case '\\':
        default:
          break;
        }
      } else {
        break;
      }
    }
    *dst++ = ch;
  }

  if (nStripped) {
    *dst = '\0';
    *pnLen -= nStripped;
  }

  return str;
}

char *hb_compEncodeString(int iMethod, const char *szText, HB_SIZE *pnLen)
{
  auto pBuffer = static_cast<char *>(hb_xgrab(*pnLen + 1));

  memcpy(pBuffer, szText, *pnLen);
  pBuffer[*pnLen] = '\0';
  if (iMethod == 1) {
    for (HB_SIZE nPos = 0; nPos < *pnLen; nPos++) {
      pBuffer[nPos] ^= 0xF3;
    }
  }
  return pBuffer;
}

char *hb_compDecodeString(int iMethod, const char *szText, HB_SIZE *pnLen)
{
  auto pBuffer = static_cast<char *>(hb_xgrab(*pnLen + 1));

  memcpy(pBuffer, szText, *pnLen);
  pBuffer[*pnLen] = '\0';
  if (iMethod == 1) {
    for (HB_SIZE nPos = 0; nPos < *pnLen; nPos++) {
      pBuffer[nPos] ^= 0xF3;
    }
  }
  return pBuffer;
}

// 'pDest' must be double the size of 'size'. [vszakats]
void hb_strtohex(const char *pSource, HB_SIZE size, char *pDest)
{
  for (HB_SIZE i = 0; i < size; i++) {
    int b;
    b = (static_cast<HB_UCHAR>(pSource[i]) >> 4) & 0x0F;
    *pDest++ = static_cast<char>(b + (b > 9 ? 'a' - 10 : '0'));
    b = static_cast<HB_UCHAR>(pSource[i]) & 0x0F;
    *pDest++ = static_cast<char>(b + (b > 9 ? 'a' - 10 : '0'));
  }
}
