//
// CT3 Date & Time functions:
//   WaitPeriod(), TimeValid(), SetTime(), SetDate()
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

// stime() exists only in SVr4, SVID, X/OPEN and Linux
#ifndef _SVID_SOURCE
#define _SVID_SOURCE
#endif
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE
#endif

#include <hbapi.hpp>
#include <hbdate.hpp>
#include <hbstack.hpp>

#if defined(HB_OS_WIN)
#include <windows.h>
#endif
#include <time.h>

struct CT_DATE
{
  // even if these are chars, variable must be int, since we need an extra -1
  double dTimeSet;
  double dTimeCounter;
};

using PCT_DATE = CT_DATE *;

static void s_ct_date_init(void *cargo)
{
  auto ct_date = static_cast<PCT_DATE>(cargo);

  ct_date->dTimeSet = 0;
  ct_date->dTimeCounter = 0;
}

static HB_TSD_NEW(s_ct_date, sizeof(CT_DATE), s_ct_date_init, nullptr);

HB_FUNC(WAITPERIOD)
{
  auto ct_date = static_cast<PCT_DATE>(hb_stackGetTSD(&s_ct_date));

  double d = hb_dateSeconds();

  if (hb_pcount() > 0)
  {
    ct_date->dTimeSet = d;
    ct_date->dTimeCounter = d + hb_parnd(1) / 100.0;
  }

  if (d < ct_date->dTimeSet)
  {
    d += 86400.0;
  }

  hb_retl(d < ct_date->dTimeCounter);
}

static HB_BOOL _hb_timeValid(const char *szTime, HB_SIZE nLen, int *piDecode)
{
  HB_BOOL fValid = false;

  if (nLen == 2 || nLen == 5 || nLen == 8 || nLen == 11)
  {
    static const int sc_iMax[] = {23, 59, 59, 99};
    int i;
    HB_SIZE nPos;

    fValid = true;
    for (nPos = 0; fValid && nPos < nLen; ++nPos)
    {
      fValid = nPos % 3 == 2 ? szTime[nPos] == ':' : (szTime[nPos] >= '0' && szTime[nPos] <= '9');
    }
    for (nPos = 0, i = 0; fValid && nPos < nLen; nPos += 3, ++i)
    {
      int iVal;
      iVal = 10 * (szTime[nPos] - '0') + (szTime[nPos + 1] - '0');
      fValid = iVal <= sc_iMax[i];
      if (piDecode)
      {
        piDecode[i] = iVal;
      }
    }
  }

  return fValid;
}

HB_FUNC(TIMEVALID)
{
  hb_retl(_hb_timeValid(hb_parc(1), hb_parclen(1), nullptr));
}

HB_FUNC(SETTIME)
{
  HB_BOOL fResult = false;
  int iTime[4];

  iTime[0] = iTime[1] = iTime[2] = iTime[3] = 0;
  if (_hb_timeValid(hb_parc(1), hb_parclen(1), iTime))
  {
#if defined(HB_OS_WIN)
    SYSTEMTIME st;
    GetLocalTime(&st);
    st.wHour = static_cast<WORD>(iTime[0]);
    st.wMinute = static_cast<WORD>(iTime[1]);
    st.wSecond = static_cast<WORD>(iTime[2]);
    st.wMilliseconds = static_cast<WORD>(iTime[3]) * 10;
    fResult = SetLocalTime(&st);
#elif defined(HB_OS_LINUX) && !defined(HB_OS_ANDROID)
    HB_ULONG lNewTime;
    time_t tm;

    lNewTime = iTime[0] * 3600 + iTime[1] * 60 + iTime[2];
    tm = time(nullptr);
    tm += lNewTime - (tm % 86400);
#if (defined(__GLIBC__) && ((__GLIBC__ > 2) || ((__GLIBC__ == 2) && (__GLIBC_MINOR__ >= 31))))
    // stime() is deprecated in glibc 2.31+
    struct timespec ts = {tm, 0};
    fResult = clock_settime(CLOCK_REALTIME, &ts) == 0;
#else
    // stime() exists only in SVr4, SVID, X/OPEN and Linux
    fResult = stime(&tm) == 0;
#endif
#endif
  }

  hb_retl(fResult);
}

HB_FUNC(SETDATE)
{
  HB_BOOL fResult = false;
  long lDate = hb_pardl(1);

  if (lDate)
  {
    int iYear, iMonth, iDay;

    hb_dateDecode(lDate, &iYear, &iMonth, &iDay);
    if (iYear >= 1970)
    {
#if defined(HB_OS_WIN)
      SYSTEMTIME st;
      GetLocalTime(&st);
      st.wYear = static_cast<WORD>(iYear);
      st.wMonth = static_cast<WORD>(iMonth);
      st.wDay = static_cast<WORD>(iDay);
      st.wDayOfWeek = static_cast<WORD>(hb_dateJulianDOW(lDate));
      fResult = SetLocalTime(&st);
#elif defined(HB_OS_LINUX) && !defined(HB_OS_ANDROID)
      // stime() exists only in SVr4, SVID, X/OPEN and Linux
      long lNewDate = lDate - hb_dateEncode(1970, 1, 1);
#if (defined(__GLIBC__) && ((__GLIBC__ > 2) || ((__GLIBC__ == 2) && (__GLIBC_MINOR__ >= 31))))
      // stime() is deprecated in glibc 2.31+
      struct timespec ts = {0, 0};
      clock_gettime(CLOCK_REALTIME, &ts); // keep tv_nsec
      ts.tv_sec = lNewDate * 86400 + (ts.tv_sec % 86400);
      fResult = clock_settime(CLOCK_REALTIME, &ts) == 0;
#else
      time_t tm;
      tm = time(nullptr);
      tm = lNewDate * 86400 + (tm % 86400);
      fResult = stime(&tm) == 0;
#endif
#endif
    }
  }

  hb_retl(fResult);
}
