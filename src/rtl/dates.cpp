//
// The Date API (C level)
//
// Copyright 1999 David G. Holm <dholm@jsd-llc.com>
// Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl> (hb_timeFormat(), hb_timeUnformat(),
// hb_timeStampFormat(), hb_timeStampUnformat())
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
#include "hbdate.hpp"
#include "hbset.hpp"

char *hb_dateFormat(const char *szDate, char *szFormattedDate, const char *szDateFormat)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dateFormat(%s, %p, %s)", szDate, static_cast<void*>(szFormattedDate), szDateFormat));
#endif

  // NOTE: szFormattedDate must point to a buffer of at least 11 bytes.
  //       szDateFormat must point to a buffer holding the date format to use.

  int format_count, digit_count;

  // Determine the maximum size of the formatted date string
  auto size = static_cast<int>(strlen(szDateFormat));
  if (size > 10) {
    size = 10;
  }

  if (szDate != nullptr && strlen(szDate) == 8) { // A valid date is always 8 characters
    auto used_d = false;
    auto used_m = false;
    auto used_y = false;
    format_count = 0;
    const char *szPtr = szDateFormat;

    while (format_count < size) {
      int digit = HB_TOUPPER(static_cast<HB_UCHAR>(*szPtr));
      szPtr++;
      digit_count = 1;
      while (HB_TOUPPER(static_cast<HB_UCHAR>(*szPtr)) == digit && format_count < size) {
        szPtr++;
        if (format_count + digit_count < size) {
          digit_count++;
        }
      }
      switch (digit) {
      case 'D':
        switch (digit_count) {
        case 4:
          if (!used_d && format_count < size) {
#if 0
                        szFormattedDate[format_count++] = '0';
#endif
            szFormattedDate[format_count++] = szDate[6];
            digit_count--;
          }
          // fallthrough
        case 3:
          if (!used_d && format_count < size) {
#if 0
                        szFormattedDate[format_count++] = '0';
#endif
            szFormattedDate[format_count++] = szDate[6];
            digit_count--;
          }
          // fallthrough
        case 2:
          if (!used_d && format_count < size) {
            szFormattedDate[format_count++] = szDate[6];
            digit_count--;
          }
          // fallthrough
        default:
          if (!used_d && format_count < size) {
            szFormattedDate[format_count++] = szDate[7];
            digit_count--;
          }
          while (digit_count-- > 0 && format_count < size) {
            szFormattedDate[format_count++] = static_cast<char>(digit);
          }
        }
        used_d = true;
        break;

      case 'M':
        switch (digit_count) {
        case 4:
          if (!used_m && format_count < size) {
#if 0
                        szFormattedDate[format_count++] = '0';
#endif
            szFormattedDate[format_count++] = szDate[4];
            digit_count--;
          }
          // fallthrough
        case 3:
          if (!used_m && format_count < size) {
#if 0
                        szFormattedDate[format_count++] = '0';
#endif
            szFormattedDate[format_count++] = szDate[4];
            digit_count--;
          }
          // fallthrough
        case 2:
          if (!used_m && format_count < size) {
            szFormattedDate[format_count++] = szDate[4];
            digit_count--;
          }
          // fallthrough
        default:
          if (!used_m && format_count < size) {
            szFormattedDate[format_count++] = szDate[5];
            digit_count--;
          }
          while (digit_count-- > 0 && format_count < size) {
            szFormattedDate[format_count++] = static_cast<char>(digit);
          }
        }
        used_m = true;
        break;

      case 'Y':
        switch (digit_count) {
        case 4:
          if (!used_y && format_count < size) {
            szFormattedDate[format_count++] = szDate[0];
            digit_count--;
          }
          // fallthrough
        case 3:
          if (!used_y && format_count < size) {
            szFormattedDate[format_count++] = szDate[1];
            digit_count--;
          }
          // fallthrough
        case 2:
          if (!used_y && format_count < size) {
            szFormattedDate[format_count++] = szDate[2];
            digit_count--;
          }
          // fallthrough
        default:
          if (!used_y && format_count < size) {
            szFormattedDate[format_count++] = szDate[3];
            digit_count--;
          }
          while (digit_count-- > 0 && format_count < size) {
            szFormattedDate[format_count++] = static_cast<char>(digit);
          }
        }
        used_y = true;
        break;

      default:
        while (digit_count-- > 0 && format_count < size) {
          szFormattedDate[format_count++] = static_cast<char>(digit);
        }
      }
    }
  } else {
    // Not a valid date string, so return a blank date with separators
    format_count = size; // size is either 8 or 10
    hb_strncpy(szFormattedDate, szDateFormat, size);

    for (digit_count = 0; digit_count < size; digit_count++) {
      switch (szFormattedDate[digit_count]) {
      case 'D':
      case 'd':
      case 'M':
      case 'm':
      case 'Y':
      case 'y':
        szFormattedDate[digit_count] = ' ';
      }
    }
  }

  szFormattedDate[format_count] = '\0';

  return szFormattedDate;
}

static int hb_dateUnformatRaw(const char *szDate, const char *szDateFormat, long *plDate)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dateUnformatRaw(%s, %s, %p)", szDate, szDateFormat, static_cast<void*>(plDate)));
#endif

  int d_value = 0, m_value = 0, y_value = 0;
  int iSize = 0;

  if (szDate != nullptr) {
    int d_pos = 0, m_pos = 0, y_pos = 0;
    int count, digit, non_digit, used;

    if (!szDateFormat) {
      szDateFormat = hb_setGetDateFormat();
    }
    auto size = static_cast<int>(strlen(szDateFormat));

    for (count = used = 0; count < size && used < 3; count++) {
      switch (szDateFormat[count]) {
      case 'D':
      case 'd':
        if (d_pos == 0) {
          ++used;
          if (m_pos == 0 && y_pos == 0) {
            d_pos = 1;
          } else if (m_pos == 0 || y_pos == 0) {
            d_pos = 2;
          } else {
            d_pos = 3;
          }
        }
        break;
      case 'M':
      case 'm':
        if (m_pos == 0) {
          ++used;
          if (d_pos == 0 && y_pos == 0) {
            m_pos = 1;
          } else if (d_pos == 0 || y_pos == 0) {
            m_pos = 2;
          } else {
            m_pos = 3;
          }
        }
        break;
      case 'Y':
      case 'y':
        if (y_pos == 0) {
          ++used;
          if (m_pos == 0 && d_pos == 0) {
            y_pos = 1;
          } else if (m_pos == 0 || d_pos == 0) {
            y_pos = 2;
          } else {
            y_pos = 3;
          }
        }
      }
    }

    // If there are non-digits at the start of the date field,
    // they are not to be treated as date field separators
    non_digit = 1;
    size = static_cast<int>(strlen(szDate));
    for (count = used = 0; count < size; count++) {
      digit = szDate[count];
      if (HB_ISDIGIT(digit)) {
        // Process the digit for the current date field
        if (d_pos == 1) {
          d_value = (d_value * 10) + digit - '0';
        } else if (m_pos == 1) {
          m_value = (m_value * 10) + digit - '0';
        } else if (y_pos == 1) {
          y_value = (y_value * 10) + digit - '0';
        }
        // Treat the next non-digit as a date field separator
        non_digit = 0;
      } else {
        // Process the non-digit
        if (non_digit == 0) {
          // Only move to the next date field on the first
          // consecutive non-digit that is encountered
          non_digit = 1;
          d_pos--;
          m_pos--;
          y_pos--;
          if (++used >= 3) {
            break;
          }
        }
      }
    }
    iSize = count;

    y_value = hb_setUpdateEpoch(y_value);
  }

  *plDate = hb_dateEncode(y_value, m_value, d_value);

  return iSize;
}

long hb_dateUnformat(const char *szDate, const char *szDateFormat)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_dateFormat(%s, %s)", szDate, szDateFormat));
#endif

  long lDate;
  hb_dateUnformatRaw(szDate, szDateFormat, &lDate);
  return lDate;
}

// time modifiers:
//    H - hour
//    M - minutes
//    S - seconds
//    F - fractional part of seconds
//    P - PM/AM marker
// maximal size of time pattern:
//    16 for "hh:mm:ss:ffff pp"
// always safe buffer size is 17 (+1 for 0)
char *hb_timeFormat(char *szBuffer, const char *szTimeFormat, long lMilliSec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_timeFormat(%p, %s, %ld)", static_cast<void*>(szBuffer), szTimeFormat, lMilliSec));
#endif

  int iPM, i12;
  int i, value, digits, skip;

  int iHour, iMinutes, iSeconds, iMSec;
  hb_timeDecode(lMilliSec, &iHour, &iMinutes, &iSeconds, &iMSec);
  char *szTimeBuffer = szBuffer;

  auto size = static_cast<int>(hb_strnlen(szTimeFormat, 16));
  iPM = i12 = 0;
  for (i = 0; i < size; ++i) {
    if (HB_TOUPPER(szTimeFormat[i]) == 'P') {
      if (iHour >= 12) {
        iPM = 1;
        iHour -= 12;
      }
      if (iHour == 0) {
        iHour += 12;
      }
      if (iHour < 10) {
        i12 = 1;
      }
      break;
    }
  }

  i = 0;
  while (i < size) {
    int count = -i;
    int ch = HB_TOUPPER(szTimeFormat[i]);
    ++i;
    while (ch == HB_TOUPPER(szTimeFormat[i]) && i < size) {
      ++i;
    }
    count += i;
    switch (ch) {
    case 'H':
      value = iHour;
      if (count == 2 && value >= 0) {
        if (i12) {
          *szTimeBuffer++ = ' ';
          --count;
        }
        digits = count;
      } else {
        digits = 1;
      }
      iHour = -1;
      break;
    case 'M':
      value = iMinutes;
      iMinutes = -1;
      digits = count > 2 ? 1 : count;
      break;
    case 'S':
      value = iSeconds;
      iSeconds = -1;
      digits = count > 2 ? 1 : count;
      break;
    case 'F':
      value = iMSec;
      iMSec = -1;
      digits = count > 4 ? 1 : count;
      switch (digits) {
      case 4:
        value *= 10;
        break;
      case 2:
        value = (value + 5) / 10;
        break;
      case 1:
        value = (value + 50) / 100;
        break;
      }
      break;
    case 'P':
      if (iPM >= 0) {
        *szTimeBuffer++ = iPM ? 'P' : 'A';
        if (--count) {
          *szTimeBuffer++ = 'M';
          --count;
        }
        iPM = -1;
      }
      // fallthrough
    default:
      digits = value = 0;
    }
    if (digits && value >= 0) {
      skip = digits;
      count -= digits;
      do {
        szTimeBuffer[--digits] = static_cast<char>('0' + value % 10);
        value /= 10;
      } while (digits);
      szTimeBuffer += skip;
    }
    while (count--) {
      *szTimeBuffer++ = static_cast<char>(ch);
    }
  }

  *szTimeBuffer = '\0';

  return szBuffer;
}

// maximal size of time pattern:
//    16 for "hh:mm:ss:ffff pp"
// total maximal size of formatted timestamp value: 10 + 16 = 26
// always safe buffer size is: 27
char *hb_timeStampFormat(char *szBuffer, const char *szDateFormat, const char *szTimeFormat, long lJulian,
                         long lMilliSec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_timeStampFormat(%p, %s, %s, %ld, %ld)", static_cast<void*>(szBuffer), szDateFormat, szTimeFormat, lJulian, lMilliSec));
#endif

  char szDate[9], *szTimeBuffer;
  hb_dateDecStr(szDate, lJulian);
  hb_dateFormat(szDate, szBuffer, szDateFormat);
  szTimeBuffer = szBuffer + strlen(szBuffer);
  if (*szBuffer) {
    *szTimeBuffer++ = ' ';
  }
  hb_timeFormat(szTimeBuffer, szTimeFormat, lMilliSec);
  return szBuffer;
}

long hb_timeUnformat(const char *szTime, const char *szTimeFormat)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_timeUnformat(%s, %s)", szTime, szTimeFormat));
#endif

  if (!szTime) {
    return 0;
  }

  int iHour, iMinutes, iSeconds, iMSec, iPM;
  int i, count, *pValue;

  if (!szTimeFormat) {
    szTimeFormat = hb_setGetTimeFormat();
  }

  auto size = static_cast<int>(hb_strnlen(szTime, hb_strnlen(szTimeFormat, 16)));
  iHour = iMinutes = iSeconds = iMSec = iPM = -1;
  int prec = 0;
  for (i = count = 0; i < size && szTime[count]; ++i) {
    switch (szTimeFormat[i]) {
    case 'H':
    case 'h':
      pValue = &iHour;
      break;
    case 'M':
    case 'm':
      pValue = &iMinutes;
      break;
    case 'S':
    case 's':
      pValue = &iSeconds;
      break;
    case 'F':
    case 'f':
      pValue = &iMSec;
      break;
    case 'P':
    case 'p':
      if (iPM < 0) {
        while (szTime[count] && !HB_ISDIGIT(szTime[count]) && szTime[count] != 'P' && szTime[count] != 'p' &&
               szTime[count] != 'A' && szTime[count] != 'a') {
          ++count;
        }
        if (szTime[count] == 'P' || szTime[count] == 'p') {
          iPM = 1;
        } else if (szTime[count] == 'A' || szTime[count] == 'a') {
          iPM = 0;
        }
      }
      // fallthrough
    default:
      pValue = nullptr;
    }
    if (pValue && *pValue < 0) {
      *pValue = 0;
      while (szTime[count] && !HB_ISDIGIT(szTime[count])) {
        ++count;
      }
      while (HB_ISDIGIT(szTime[count])) {
        *pValue = *pValue * 10 + (szTime[count] - '0');
        ++count;
        if (pValue == &iMSec) {
          ++prec;
        }
      }
    }
  }
  if (iHour < 0) {
    iHour = 0;
  }
  if (iMinutes < 0) {
    iMinutes = 0;
  }
  if (iSeconds < 0) {
    iSeconds = 0;
  }
  if (iMSec < 0) {
    iMSec = 0;
  } else if (iMSec > 0) {
    if (prec > 3) {
      do {
        iMSec /= 10;
      } while (--prec > 3);
    } else {
      while (prec++ < 3) {
        iMSec *= 10;
      }
    }
  }
  if (iPM > 0) {
    if (iHour == 0) {
      iHour = 24; // wrong time
    } else if (iHour != 12) {
      iHour += 12;
    }
  } else if (iPM == 0) {
    if (iHour == 0) {
      iHour = 24; // wrong time
    } else if (iHour == 12) {
      iHour = 0;
    }
  }

  return hb_timeEncode(iHour, iMinutes, iSeconds, iMSec);
}

void hb_timeStampUnformat(const char *szDateTime, const char *szDateFormat, const char *szTimeFormat, long *plJulian,
                          long *plMilliSec)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_timeStampUnformat(%s, %s, %s, %p, %p)", szDateTime, szDateFormat, szTimeFormat, static_cast<void*>(plJulian), static_cast<void*>(plMilliSec)));
#endif

  if (szDateTime != nullptr) {
    if (!szDateFormat) {
      szDateFormat = hb_setGetDateFormat();
    }
    int size = hb_dateUnformatRaw(szDateTime, szDateFormat, plJulian);
    *plMilliSec = hb_timeUnformat(szDateTime + size, szTimeFormat);
  } else {
    *plJulian = *plMilliSec = 0;
  }
}
