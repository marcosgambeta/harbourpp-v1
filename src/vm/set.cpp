//
// Set functions
//
// Copyright 1999-2003 David G. Holm <dholm@jsd-llc.com>
// Copyright 2008-2009 Viktor Szakats (vszakats.net/harbour) (hb_osEncode(), hb_osDecode())
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

#define _HB_SET_INTERNAL_

#include "hbvmopt.hpp"
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapierr.hpp"
#include "hbapifs.hpp"
#include "hbapigt.hpp"
#include "hbapilng.hpp"
#include "hbapicdp.hpp"
#include "hbapistr.hpp"
#include "hbset.hpp"
#include "hbstack.hpp"
#include "hbvm.hpp"

struct HB_SET_LISTENER_
{
  int listener;
  HB_SET_LISTENER_CALLBACK *callback;
  struct HB_SET_LISTENER_ *next;
};

using HB_SET_LISTENER = HB_SET_LISTENER_;
using PHB_SET_LISTENER = HB_SET_LISTENER *;

struct HB_SET_LISTENER_LST
{
  PHB_SET_LISTENER first;
  PHB_SET_LISTENER last;
  int counter;
};

using PHB_SET_LISTENER_LST = HB_SET_LISTENER_LST *;

static char set_char(PHB_ITEM pItem, char oldChar)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("set_char(%p, %c)", static_cast<void*>(pItem), oldChar));
#endif

  char newChar = oldChar;

  if (pItem->isString())
  {
    // Only replace if string has at least one character.
    auto nLen = pItem->getCLen();
    if (nLen > 0)
    {
      newChar = *(pItem->getCPtr());
    }
  }
  return newChar;
}

// Change the setting if the parameter is a logical value, or is
// either "ON" or "OFF" (regardless of case)
static bool set_logical(PHB_ITEM pItem, bool bDefault)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("set_logical(%p)", static_cast<void*>(pItem)));
#endif

  bool bLogical = bDefault;

  if (pItem != nullptr)
  {
    if (pItem->isLogical())
    {
      bLogical = pItem->getL();
    }
    else if (pItem->isString())
    {
      auto szString = pItem->getCPtr();
      auto nLen = pItem->getCLen();

      if (nLen >= 2 && (static_cast<HB_UCHAR>(szString[0]) == 'O' || static_cast<HB_UCHAR>(szString[0]) == 'o') &&
          (static_cast<HB_UCHAR>(szString[1]) == 'N' || static_cast<HB_UCHAR>(szString[1]) == 'n'))
      {
        bLogical = true;
      }
      else if (nLen >= 3 && (static_cast<HB_UCHAR>(szString[0]) == 'O' || static_cast<HB_UCHAR>(szString[0]) == 'o') &&
               (static_cast<HB_UCHAR>(szString[1]) == 'F' || static_cast<HB_UCHAR>(szString[1]) == 'f') &&
               (static_cast<HB_UCHAR>(szString[2]) == 'F' || static_cast<HB_UCHAR>(szString[2]) == 'f'))
      {
        bLogical = false;
      }
    }
  }

  return bLogical;
}

static int set_number(PHB_ITEM pItem, int iOldValue)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("set_number(%p, %d)", static_cast<void*>(pItem), iOldValue));
#endif

  return pItem->isNumeric() ? pItem->getNI() : iOldValue;
}

static char *set_string(PHB_ITEM pItem, char *szOldString)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("set_string(%p, %s)", static_cast<void*>(pItem), szOldString));
#endif

  char *szString;

  if (pItem->isString() || pItem->isNil())
  {
    if (szOldString != nullptr)
    {
      hb_xfree(szOldString);
    }
    // Limit size of SET strings to 64 KiB, truncating if source is longer
    szString = hb_strndup(pItem->getCPtr(), USHRT_MAX);
  }
  else
  {
    szString = szOldString;
  }

  return szString;
}

static void close_handle(PHB_SET_STRUCT pSet, HB_set_enum set_specifier)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("close_handle(%p, %d)", static_cast<void*>(pSet), static_cast<int>(set_specifier)));
#endif

  PHB_FILE *handle_ptr;

  switch (set_specifier)
  {
  case HB_SET_ALTFILE:
    handle_ptr = &pSet->hb_set_althan;
    break;
  case HB_SET_PRINTFILE:
    handle_ptr = &pSet->hb_set_printhan;
    break;
  case HB_SET_EXTRAFILE:
    handle_ptr = &pSet->hb_set_extrahan;
    break;
  default:
    return;
  }

  if (*handle_ptr != nullptr)
  {
    if (set_specifier != HB_SET_PRINTFILE && pSet->HB_SET_EOF)
    {
      hb_fileWrite(*handle_ptr, "\x1A", 1, -1);
    }
    hb_fileClose(*handle_ptr);
    *handle_ptr = nullptr;
  }
}

static const char *is_devicename(const char *szFileName)
{
  if (szFileName != nullptr && *szFileName)
  {
#if defined(HB_OS_WIN)
    const char *szDevices[] = {"NUL",  "PRN",  "CON",  "LPT1", "LPT2", "LPT3", "COM1", "COM2",
                               "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9"};
    int iSkip = 0;

    if ((szFileName[0] == '\\' || szFileName[0] == '/') && (szFileName[1] == '\\' || szFileName[1] == '/'))
    {
      if (szFileName[2] == '.' && (szFileName[3] == '\\' || szFileName[3] == '/'))
      {
        iSkip = 4;
        if (hb_strnicmp(szFileName + 4, "PIPE", 4) == 0 && (szFileName[8] == '\\' || szFileName[8] == '/'))
        {
          return szFileName;
        }
      }
      if (szFileName[2] != '\\' && szFileName[2] != '/')
      {
        int iFrom, iTo;
        for (iFrom = 2, iTo = 0; szFileName[iFrom]; ++iFrom)
        {
          if (szFileName[iFrom] == '\\' || szFileName[iFrom] == '/')
          {
            if (iTo++)
            {
              break;
            }
          }
        }
        if (iTo == 1)
        {
          return szFileName;
        }
      }
    }
    auto iLen = static_cast<int>(strlen(szFileName + iSkip));
    if (iLen >= 3 && iLen <= 4)
    {
      int iFrom, iTo;

      if (iLen == 3)
      {
        iFrom = 0;
        iTo = 3;
      }
      else
      {
        iFrom = 3;
        iTo = HB_SIZEOFARRAY(szDevices);
      }
      for (; iFrom < iTo; ++iFrom)
      {
        if (hb_stricmp(szFileName + iSkip, szDevices[iFrom]) == 0)
        {
          return iSkip ? szFileName : szDevices[iFrom];
        }
      }
    }
#elif defined(HB_OS_UNIX)
    if (strncmp(szFileName, "/dev/", 5) == 0)
    {
      return szFileName;
    }
    else
    {
      HB_FATTR ulAttr = 0;
      if (hb_fsGetAttr(szFileName, &ulAttr))
      {
        if (ulAttr & (HB_FA_CHRDEVICE | HB_FA_BLKDEVICE | HB_FA_FIFO | HB_FA_SOCKET))
        {
          return szFileName;
        }
      }
    }
#endif
  }
  return nullptr;
}

static void open_handle(PHB_SET_STRUCT pSet, const char *file_name, bool fAppend, HB_set_enum set_specifier)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("open_handle(%p, %s, %d, %d)", static_cast<void*>(pSet), file_name, static_cast<int>(fAppend), static_cast<int>(set_specifier)));
#endif

  HB_STACK_TLS_PRELOAD
  PHB_ITEM pError = nullptr;
  PHB_FILE handle;
  PHB_FILE *handle_ptr;
  HB_ERRCODE uiError;
  const char *szDevice = nullptr;
  const char *def_ext;
  char *szFileName = nullptr;
  char **set_value;
  auto fPipe = false;

  switch (set_specifier)
  {
  case HB_SET_ALTFILE:
    uiError = 2013;
    set_value = &pSet->HB_SET_ALTFILE;
    handle_ptr = &pSet->hb_set_althan;
    def_ext = ".txt";
    break;
  case HB_SET_PRINTFILE:
    uiError = 2014;
    set_value = &pSet->HB_SET_PRINTFILE;
    handle_ptr = &pSet->hb_set_printhan;
    def_ext = ".prn";
    break;
  case HB_SET_EXTRAFILE:
    uiError = 2015;
    set_value = &pSet->HB_SET_EXTRAFILE;
    handle_ptr = &pSet->hb_set_extrahan;
    def_ext = ".prn";
    break;
  default:
    return;
  }

  if (file_name && file_name[0] != '\0')
  {
#if defined(HB_OS_UNIX)
    fPipe = file_name[0] == '|';
    if (fPipe)
    {
      szFileName = hb_strdup(file_name);
    }
    else
#endif
    {
      szDevice = is_devicename(file_name);
      if (szDevice != nullptr)
      {
        szFileName = hb_strdup(szDevice);
        def_ext = nullptr;
      }
      else
      {
        szFileName = hb_strdup(file_name);
      }
    }
  }

  // free the old value before setting the new one (CA-Cl*pper does it).
  // This code must be executed after setting szFileName, [druzus]
  close_handle(pSet, set_specifier);
  if (*set_value)
  {
    hb_xfree(*set_value);
    *set_value = nullptr;
  }

  if (!szFileName)
  {
    return;
  }

  bool fStripEof = fAppend && szDevice == nullptr && !fPipe;

  // Open the file either in append (fAppend) or truncate mode (!fAppend), but
  // always use binary mode

  // QUESTION: What sharing mode does Clipper use ? [vszakats]

  do
  {
    if (fPipe)
    {
      handle = hb_filePOpen(szFileName + 1, "w");
    }
    else
    {
      handle =
          hb_fileExtOpen(szFileName, hb_stackSetStruct()->HB_SET_DEFEXTENSIONS ? def_ext : nullptr,
                         (!fStripEof || set_specifier == HB_SET_PRINTFILE ? FO_WRITE : FO_READWRITE) | FO_DENYWRITE |
                             FXO_SHARELOCK | (fAppend ? FXO_APPEND : FXO_TRUNCATE) | (szDevice ? 0 : FXO_DEFAULTS),
                         nullptr, pError);
    }

    if (handle == nullptr)
    {
      pError = hb_errRT_FileError(pError, HB_ERR_SS_TERMINAL, EG_CREATE, uiError, szFileName);
      if (hb_errLaunch(pError) != E_RETRY)
      {
        break;
      }
    }
  } while (handle == nullptr);

  if (pError)
  {
    hb_itemRelease(pError);
  }

  if (handle != nullptr && fStripEof)
  {
    // Position to EOF
    if (hb_fileSeek(handle, 0, FS_END) > 0)
    {
      // Special binary vs. text file handling - even for UN*X, now
      // that there's an HB_SET_EOF flag.

      // PRINTFILE is always binary and needs no special handling.
      if (set_specifier != HB_SET_PRINTFILE)
      {
        // All other files are text files and may have an EOF
        // ('\x1A') character at the end (both UN*X and non-UN*X,
        // now that theres an HB_SET_EOF flag).
        char cEOF = '\0';
        hb_fileSeek(handle, -1, FS_END);   // Position to last char.
        hb_fileRead(handle, &cEOF, 1, -1); // Read the last char.
        if (cEOF == '\x1A')
        { // If it's an EOF, Then write over it.
          hb_fileSeek(handle, -1, FS_END);
        }
      }
    }
  }

  // user RT error handler can open it too so we have to
  // close it again if necessary
  if (handle == nullptr)
  {
    hb_xfree(szFileName);
    szFileName = nullptr;
  }

  close_handle(pSet, set_specifier);
  *handle_ptr = handle;
  if (*set_value)
  {
    hb_xfree(*set_value);
  }
  *set_value = szFileName;
}

int hb_setUpdateEpoch(int iYear)
{
  if (iYear >= 0 && iYear < 100)
  {
    int iEpoch = hb_setGetEpoch();
    int iCentury = iEpoch / 100;

    if (iYear < iEpoch % 100)
    {
      ++iCentury;
    }
    iYear += iCentury * 100;
  }
  return iYear;
}

HB_BOOL hb_setSetCentury(HB_BOOL new_century_setting)
{
  HB_STACK_TLS_PRELOAD
  PHB_SET_STRUCT pSet = hb_stackSetStruct();
  bool old_century_setting = pSet->hb_set_century;

  pSet->hb_set_century = new_century_setting;
  // if the setting changed, adjust the current date format to use
  // the correct number of year digits.
  if (old_century_setting != new_century_setting)
  {
    int y_start, y_stop;

    // Convert to upper case and determine where year is
    y_start = y_stop = -1;
    char *szDateFormat = pSet->HB_SET_DATEFORMAT;
    auto size = static_cast<int>(strlen(szDateFormat));
    for (auto count = 0; count < size; count++)
    {
      int digit = HB_TOUPPER(static_cast<HB_UCHAR>(szDateFormat[count]));
      if (digit == 'Y')
      {
        if (y_start == -1)
        {
          y_start = count;
        }
      }
      else if (y_start > -1 && y_stop == -1)
      {
        y_stop = count;
      }
      szDateFormat[count] = static_cast<char>(digit);
    }
    // Determine size of year in current format
    if (y_start < 0)
    {
      y_start = 0; // There is no year in the current format
      y_stop = 0;
    }
    else if (y_stop < 0)
    {
      y_stop = size; // All digits are year digits
    }
    int y_size = y_stop - y_start;
    // Calculate size of new format
    size -= y_size;
    if (new_century_setting)
    {
      size += 4;
    }
    else
    {
      size += 2;
    }

    // Create the new date format
    auto szNewFormat = static_cast<char *>(hb_xgrab(size + 1));

    {
      if (y_start > 0)
      {
        memcpy(szNewFormat, szDateFormat, y_start);
      }
      szNewFormat[y_start] = '\0';
      hb_strncat(szNewFormat, "YY", size);
      if (new_century_setting)
      {
        hb_strncat(szNewFormat, "YY", size);
      }
      auto format_len = static_cast<int>(strlen(szDateFormat));
      if (y_stop < format_len)
      {
        hb_strncat(szNewFormat, szDateFormat + y_stop, size);
      }
      // DATE FORMAT is under direct control of SET, so notify when it
      // it is changed indirectly via __SetCentury()
      hb_setListenerNotify(HB_SET_DATEFORMAT, HB_SET_LISTENER_BEFORE);
      hb_xfree(szDateFormat);
      pSet->HB_SET_DATEFORMAT = szNewFormat;
      hb_setListenerNotify(HB_SET_DATEFORMAT, HB_SET_LISTENER_AFTER);
    }
  }

  // Return the previous setting
  return old_century_setting;
}

HB_FUNC(__SETCENTURY)
{
  HB_STACK_TLS_PRELOAD
  bool old_century_setting = hb_setGetCentury();
  auto pNewVal = hb_param(1, Harbour::Item::ANY);

  if (pNewVal)
  {
    hb_setSetCentury(set_logical(pNewVal, old_century_setting));
  }

  hb_retl(old_century_setting);
}

HB_FUNC(SETCANCEL)
{
  HB_STACK_TLS_PRELOAD
  hb_retl(hb_setGetCancel());
  // SetCancel() accepts only logical parameters
  hb_setSetItem(HB_SET_CANCEL, hb_param(1, Harbour::Item::LOGICAL));
}

// return default printer device
static char *hb_set_PRINTFILE_default(void)
{
#if defined(HB_OS_UNIX)
  return hb_strdup("|lpr");
#elif defined(HB_OS_WIN)
  return hb_strdup("LPT1");
#else
  return hb_strdup("PRN"); // FIXME
#endif
}

PHB_ITEM hb_setGetItem(HB_set_enum set_specifier, PHB_ITEM pResult, PHB_ITEM pArg1, PHB_ITEM pArg2)
{
  HB_STACK_TLS_PRELOAD
  PHB_SET_STRUCT pSet = hb_stackSetStruct();

  if (pArg1 != nullptr)
  {
    hb_setListenerNotify(set_specifier, HB_SET_LISTENER_BEFORE);
  }

  switch (set_specifier)
  {
  case HB_SET_ALTERNATE:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_ALTERNATE);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_ALTERNATE = set_logical(pArg1, pSet->HB_SET_ALTERNATE);
    }
    break;
  case HB_SET_ALTFILE:
    pResult = hb_itemPutC(pResult, pSet->HB_SET_ALTFILE);
    if (pArg1 && pArg1->isString())
    {
      open_handle(pSet, pArg1->getCPtr(), set_logical(pArg2, false), HB_SET_ALTFILE);
    }
    break;
  case HB_SET_AUTOPEN:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_AUTOPEN);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_AUTOPEN = set_logical(pArg1, pSet->HB_SET_AUTOPEN);
    }
    break;
  case HB_SET_AUTORDER:
    pResult = hb_itemPutNI(pResult, pSet->HB_SET_AUTORDER);
    if (pArg1 != nullptr)
    {
      if (set_number(pArg1, pSet->HB_SET_AUTORDER) < 0)
      {
        hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
      else
      {
        pSet->HB_SET_AUTORDER = set_number(pArg1, pSet->HB_SET_AUTORDER);
      }
    }
    break;
  case HB_SET_AUTOSHARE:
    pResult = hb_itemPutNI(pResult, pSet->HB_SET_AUTOSHARE);
    if (pArg1 != nullptr)
    {
      if (set_number(pArg1, pSet->HB_SET_AUTOSHARE) < 0)
      {
        hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
      else
      {
        pSet->HB_SET_AUTOSHARE = set_number(pArg1, pSet->HB_SET_AUTOSHARE);
      }
    }
    break;
  case HB_SET_BELL:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_BELL);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_BELL = set_logical(pArg1, pSet->HB_SET_BELL);
    }
    break;
  case HB_SET_CANCEL:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_CANCEL);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_CANCEL = set_logical(pArg1, pSet->HB_SET_CANCEL);
    }
    break;
  case HB_SET_COLOR:
    pResult = hb_itemPutC(pResult, hb_conSetColor(pArg1 != nullptr && pArg1->isString() ? pArg1->getCPtr() : nullptr));
    break;
  case HB_SET_CONFIRM:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_CONFIRM);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_CONFIRM = set_logical(pArg1, pSet->HB_SET_CONFIRM);
    }
    break;
  case HB_SET_CONSOLE:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_CONSOLE);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_CONSOLE = set_logical(pArg1, pSet->HB_SET_CONSOLE);
    }
    break;
  case HB_SET_CURSOR:
    if (pArg1 != nullptr && pArg1->isNumeric())
    {
      pResult = hb_itemPutNI(pResult, hb_conSetCursor(true, pArg1->getNI()));
    }
    else
    {
      pResult = hb_itemPutNI(pResult, hb_conSetCursor(false, 0));
    }
    break;
  case HB_SET_DATEFORMAT:
    pResult = hb_itemPutC(pResult, pSet->HB_SET_DATEFORMAT);
    if (pArg1 != nullptr)
    {
      char *value = pSet->HB_SET_DATEFORMAT = set_string(pArg1, pSet->HB_SET_DATEFORMAT);
      int year = 0;
      while (*value)
      {
        if (*value == 'Y' || *value == 'y')
        {
          year++;
        }
        else if (year)
        { // Only count the first set of consecutive "Y"s.
          break;
        }
        ++value;
      }
      // CENTURY is not controlled directly by SET, so there is no
      // notification for changing it indirectly via DATE FORMAT.
      pSet->hb_set_century = year >= 4;
    }
    break;
  case HB_SET_TIMEFORMAT:
    pResult = hb_itemPutC(pResult, pSet->HB_SET_TIMEFORMAT);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_TIMEFORMAT = set_string(pArg1, pSet->HB_SET_TIMEFORMAT);
    }
    break;
  case HB_SET_DEBUG:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_DEBUG);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_DEBUG = set_logical(pArg1, pSet->HB_SET_DEBUG);
    }
    break;
  case HB_SET_DECIMALS:
    pResult = hb_itemPutNI(pResult, pSet->HB_SET_DECIMALS);
    if (pArg1 != nullptr)
    {
      if (set_number(pArg1, pSet->HB_SET_DECIMALS) < 0)
      {
        hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
      else
      {
        pSet->HB_SET_DECIMALS = set_number(pArg1, pSet->HB_SET_DECIMALS);
      }
    }
    break;
  case HB_SET_DEFAULT:
    pResult = hb_itemPutC(pResult, pSet->HB_SET_DEFAULT);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_DEFAULT = set_string(pArg1, pSet->HB_SET_DEFAULT);
    }
    break;
  case HB_SET_DELETED:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_DELETED);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_DELETED = set_logical(pArg1, pSet->HB_SET_DELETED);
    }
    break;
  case HB_SET_DELIMCHARS:
    pResult = hb_itemPutC(pResult, pSet->HB_SET_DELIMCHARS);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_DELIMCHARS = set_string(pArg1, pSet->HB_SET_DELIMCHARS);
    }
    break;
  case HB_SET_DELIMITERS:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_DELIMITERS);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_DELIMITERS = set_logical(pArg1, pSet->HB_SET_DELIMITERS);
    }
    break;
  case HB_SET_DEVICE:
    pResult = hb_itemPutC(pResult, pSet->HB_SET_DEVICE);
    if (pArg1 && pArg1->isString())
    {
      // If the print file is not already open, open it in overwrite mode.
      pSet->HB_SET_DEVICE = set_string(pArg1, pSet->HB_SET_DEVICE);
      pSet->hb_set_prndevice = strlen(pSet->HB_SET_DEVICE) >= 4 && hb_strnicmp(pSet->HB_SET_DEVICE, "PRIN", 4) == 0;
    }
    break;
  case HB_SET_EOF:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_EOF);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_EOF = set_logical(pArg1, pSet->HB_SET_EOF);
    }
    break;
  case HB_SET_EPOCH:
    pResult = hb_itemPutNI(pResult, pSet->HB_SET_EPOCH);
    if (pArg1 != nullptr)
    {
      if (set_number(pArg1, pSet->HB_SET_EPOCH) < 0)
      {
        hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
      else
      {
        pSet->HB_SET_EPOCH = set_number(pArg1, pSet->HB_SET_EPOCH);
      }
    }
    break;
  case HB_SET_ESCAPE:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_ESCAPE);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_ESCAPE = set_logical(pArg1, pSet->HB_SET_ESCAPE);
    }
    break;
  case HB_SET_EVENTMASK:
    pResult = hb_itemPutNI(pResult, pSet->HB_SET_EVENTMASK);
    if (pArg1 != nullptr)
    {
      if (set_number(pArg1, pSet->HB_SET_EVENTMASK) < 0)
      {
        hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
      else
      {
        pSet->HB_SET_EVENTMASK = set_number(pArg1, pSet->HB_SET_EVENTMASK);
      }
    }
    break;
  case HB_SET_EXACT:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_EXACT);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_EXACT = set_logical(pArg1, pSet->HB_SET_EXACT);
    }
    break;
  case HB_SET_EXCLUSIVE:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_EXCLUSIVE);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_EXCLUSIVE = set_logical(pArg1, pSet->HB_SET_EXCLUSIVE);
    }
    break;
  case HB_SET_EXIT:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_EXIT);
    // NOTE: Otherwise ReadExit() will always set the value. [vszakats]
    if (pArg1 != nullptr && !pArg1->isNil())
    {
      pSet->HB_SET_EXIT = set_logical(pArg1, pSet->HB_SET_EXIT);
    }
    break;
  case HB_SET_EXTRA:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_EXTRA);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_EXTRA = set_logical(pArg1, pSet->HB_SET_EXTRA);
    }
    break;
  case HB_SET_EXTRAFILE:
    pResult = hb_itemPutC(pResult, pSet->HB_SET_EXTRAFILE);
    if (pArg1 && pArg1->isString())
    {
      open_handle(pSet, pArg1->getCPtr(), set_logical(pArg2, false), HB_SET_EXTRAFILE);
    }
    break;
  case HB_SET_FIXED:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_FIXED);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_FIXED = set_logical(pArg1, pSet->HB_SET_FIXED);
    }
    break;
  case HB_SET_INSERT:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_INSERT);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_INSERT = set_logical(pArg1, pSet->HB_SET_INSERT);
    }
    break;
  case HB_SET_INTENSITY:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_INTENSITY);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_INTENSITY = set_logical(pArg1, pSet->HB_SET_INTENSITY);
    }
    break;
  case HB_SET_MARGIN:
    pResult = hb_itemPutNI(pResult, pSet->HB_SET_MARGIN);
    if (pArg1 != nullptr)
    {
      if (set_number(pArg1, pSet->HB_SET_MARGIN) < 0)
      {
        hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
      else
      {
        pSet->HB_SET_MARGIN = set_number(pArg1, pSet->HB_SET_MARGIN);
      }
    }
    break;
  case HB_SET_MBLOCKSIZE:
    pResult = hb_itemPutNI(pResult, pSet->HB_SET_MBLOCKSIZE);
    if (pArg1 != nullptr)
    {
      if (set_number(pArg1, pSet->HB_SET_MBLOCKSIZE) < 0)
      {
        hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
      else
      {
        pSet->HB_SET_MBLOCKSIZE = set_number(pArg1, pSet->HB_SET_MBLOCKSIZE);
      }
    }
    break;
  case HB_SET_MCENTER:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_MCENTER);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_MCENTER = set_logical(pArg1, pSet->HB_SET_MCENTER);
    }
    break;
  case HB_SET_MESSAGE:
    pResult = hb_itemPutNI(pResult, pSet->HB_SET_MESSAGE);
    if (pArg1 != nullptr)
    {
      if (set_number(pArg1, pSet->HB_SET_MESSAGE) < 0)
      {
        hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
      else
      {
        pSet->HB_SET_MESSAGE = set_number(pArg1, pSet->HB_SET_MESSAGE);
      }
    }
    break;
  case HB_SET_MFILEEXT:
    pResult = hb_itemPutC(pResult, pSet->HB_SET_MFILEEXT);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_MFILEEXT = set_string(pArg1, pSet->HB_SET_MFILEEXT);
    }
    break;
  case HB_SET_OPTIMIZE:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_OPTIMIZE);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_OPTIMIZE = set_logical(pArg1, pSet->HB_SET_OPTIMIZE);
    }
    break;
  case HB_SET_FORCEOPT:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_FORCEOPT);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_FORCEOPT = set_logical(pArg1, pSet->HB_SET_FORCEOPT);
    }
    break;
  case HB_SET_STRICTREAD:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_STRICTREAD);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_STRICTREAD = set_logical(pArg1, pSet->HB_SET_STRICTREAD);
    }
    break;
  case HB_SET_HARDCOMMIT:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_HARDCOMMIT);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_HARDCOMMIT = set_logical(pArg1, pSet->HB_SET_HARDCOMMIT);
    }
    break;
  case HB_SET_PATH:
    pResult = hb_itemPutC(pResult, pSet->HB_SET_PATH);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_PATH = set_string(pArg1, pSet->HB_SET_PATH);
      hb_fsFreeSearchPath(pSet->hb_set_path);
      pSet->hb_set_path = nullptr;
      hb_fsAddSearchPath(pSet->HB_SET_PATH, &pSet->hb_set_path);
    }
    break;
  case HB_SET_PRINTER:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_PRINTER);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_PRINTER = set_logical(pArg1, pSet->HB_SET_PRINTER);
    }
    break;
  case HB_SET_PRINTFILE:
    pResult = hb_itemPutC(pResult, pSet->HB_SET_PRINTFILE);
    if (pArg1 && pArg1->isString())
    {
      open_handle(pSet, pArg1->getCPtr(), set_logical(pArg2, false), HB_SET_PRINTFILE);
      // With SET PRINTER TO or Set(_SET_PRINTFILE, "") are expected to activate the default printer [jarabal]
      if (pSet->HB_SET_PRINTFILE == nullptr)
      {
        pSet->HB_SET_PRINTFILE = hb_set_PRINTFILE_default();
      }
    }
    break;
  case HB_SET_SCOREBOARD:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_SCOREBOARD);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_SCOREBOARD = set_logical(pArg1, pSet->HB_SET_SCOREBOARD);
    }
    break;
  case HB_SET_SCROLLBREAK:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_SCROLLBREAK);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_SCROLLBREAK = set_logical(pArg1, pSet->HB_SET_SCROLLBREAK);
    }
    break;
  case HB_SET_SOFTSEEK:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_SOFTSEEK);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_SOFTSEEK = set_logical(pArg1, pSet->HB_SET_SOFTSEEK);
    }
    break;
  case HB_SET_TYPEAHEAD:
    pResult = hb_itemPutNI(pResult, pSet->HB_SET_TYPEAHEAD);
    if (pArg1 != nullptr)
    {
      // Set the value and limit the range
      pSet->HB_SET_TYPEAHEAD = set_number(pArg1, pSet->HB_SET_TYPEAHEAD);
      if (pSet->HB_SET_TYPEAHEAD == 0)
      {
        /* Do nothing */;
      }
      else if (pSet->HB_SET_TYPEAHEAD < 16)
      {
        pSet->HB_SET_TYPEAHEAD = 16;
      }
      else if (pSet->HB_SET_TYPEAHEAD > 4096)
      {
        pSet->HB_SET_TYPEAHEAD = 4096;
      }
      // reset keyboard buffer
      hb_inkeyReset();
    }
    break;
  case HB_SET_UNIQUE:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_UNIQUE);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_UNIQUE = set_logical(pArg1, pSet->HB_SET_UNIQUE);
    }
    break;
  case HB_SET_VIDEOMODE:
    pResult = hb_itemPutNI(pResult, pSet->HB_SET_VIDEOMODE);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_VIDEOMODE = set_number(pArg1, pSet->HB_SET_VIDEOMODE);
    }
    break;
  case HB_SET_WRAP:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_WRAP);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_WRAP = set_logical(pArg1, pSet->HB_SET_WRAP);
    }
    break;
  case HB_SET_LANGUAGE:
    pResult = hb_itemPutC(pResult, hb_langID());
    if (pArg1 != nullptr)
    {
      if (pArg1->isString())
      {
        hb_langSelectID(pArg1->getCPtr());
      }
      else
      {
        hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
    }
    break;
  case HB_SET_CODEPAGE:
    pResult = hb_itemPutC(pResult, hb_cdpID());
    if (pArg1 != nullptr)
    {
      if (pArg1->isString())
      {
        hb_cdpSelectID(pArg1->getCPtr());
      }
      else
      {
        hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
    }
    break;
  case HB_SET_IDLEREPEAT:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_IDLEREPEAT);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_IDLEREPEAT = set_logical(pArg1, pSet->HB_SET_IDLEREPEAT);
    }
    break;
  case HB_SET_FILECASE:
    pResult = hb_itemPutNI(pResult, pSet->HB_SET_FILECASE);
    if (pArg1 != nullptr)
    {
      if (pArg1->isString())
      {
        if (!hb_stricmp(pArg1->getCPtr(), "LOWER"))
        {
          pSet->HB_SET_FILECASE = HB_SET_CASE_LOWER;
        }
        else if (!hb_stricmp(pArg1->getCPtr(), "UPPER"))
        {
          pSet->HB_SET_FILECASE = HB_SET_CASE_UPPER;
        }
        else if (!hb_stricmp(pArg1->getCPtr(), "MIXED"))
        {
          pSet->HB_SET_FILECASE = HB_SET_CASE_MIXED;
        }
        else
        {
          hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
        }
      }
      else if (pArg1->isNumeric())
      {
        int iValue = set_number(pArg1, pSet->HB_SET_FILECASE);
        if (iValue == HB_SET_CASE_LOWER || iValue == HB_SET_CASE_UPPER || iValue == HB_SET_CASE_MIXED)
        {
          pSet->HB_SET_FILECASE = iValue;
        }
        else
        {
          hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
        }
      }
      else
      {
        hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
    }
    break;
  case HB_SET_DIRCASE:
    pResult = hb_itemPutNI(pResult, pSet->HB_SET_DIRCASE);
    if (pArg1 != nullptr)
    {
      if (pArg1->isString())
      {
        if (!hb_stricmp(pArg1->getCPtr(), "LOWER"))
        {
          pSet->HB_SET_DIRCASE = HB_SET_CASE_LOWER;
        }
        else if (!hb_stricmp(pArg1->getCPtr(), "UPPER"))
        {
          pSet->HB_SET_DIRCASE = HB_SET_CASE_UPPER;
        }
        else if (!hb_stricmp(pArg1->getCPtr(), "MIXED"))
        {
          pSet->HB_SET_DIRCASE = HB_SET_CASE_MIXED;
        }
        else
        {
          hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
        }
      }
      else if (pArg1->isNumeric())
      {
        int iValue = set_number(pArg1, pSet->HB_SET_DIRCASE);
        if (iValue == HB_SET_CASE_LOWER || iValue == HB_SET_CASE_UPPER || iValue == HB_SET_CASE_MIXED)
        {
          pSet->HB_SET_DIRCASE = iValue;
        }
        else
        {
          hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
        }
      }
      else
      {
        hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
    }
    break;
  case HB_SET_DIRSEPARATOR: {
    char szDirSep[2];
    szDirSep[0] = static_cast<char>(pSet->HB_SET_DIRSEPARATOR);
    szDirSep[1] = '\0';
    pResult = hb_itemPutC(pResult, szDirSep);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_DIRSEPARATOR = set_char(pArg1, static_cast<char>(pSet->HB_SET_DIRSEPARATOR));
    }
    break;
  }
  case HB_SET_DBFLOCKSCHEME:
    pResult = hb_itemPutNI(pResult, pSet->HB_SET_DBFLOCKSCHEME);
    if (pArg1 != nullptr)
    {
      if (set_number(pArg1, pSet->HB_SET_DBFLOCKSCHEME) < 0)
      {
        hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
      else
      {
        pSet->HB_SET_DBFLOCKSCHEME = set_number(pArg1, pSet->HB_SET_DBFLOCKSCHEME);
      }
    }
    break;
  case HB_SET_DEFEXTENSIONS:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_DEFEXTENSIONS);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_DEFEXTENSIONS = set_logical(pArg1, pSet->HB_SET_DEFEXTENSIONS);
    }
    break;
  case HB_SET_EOL:
    pResult = hb_itemPutC(pResult, pSet->HB_SET_EOL);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_EOL = set_string(pArg1, pSet->HB_SET_EOL);
    }
    break;
  case HB_SET_TRIMFILENAME:
    pResult = hb_itemPutL(pResult, pSet->HB_SET_TRIMFILENAME);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_TRIMFILENAME = set_logical(pArg1, pSet->HB_SET_TRIMFILENAME);
    }
    break;
  case HB_SET_HBOUTLOG:
    pResult = hb_itemPutC(pResult, pSet->HB_SET_HBOUTLOG);
    if (pArg1 != nullptr && (pArg1->isString() || pArg1->isNil()))
    {
      if (pSet->HB_SET_HBOUTLOG)
      {
        hb_xfree(pSet->HB_SET_HBOUTLOG);
      }
      if (pArg1->isNil())
      {
        pSet->HB_SET_HBOUTLOG = nullptr;
      }
      else
      {
        // Limit size of SET strings to 64 KiB, truncating if source is longer
        pSet->HB_SET_HBOUTLOG = hb_strndup(pArg1->getCPtr(), USHRT_MAX);
      }
      hb_xsetfilename(pSet->HB_SET_HBOUTLOG);
    }
    break;
  case HB_SET_HBOUTLOGINFO:
    pResult = hb_itemPutC(pResult, pSet->HB_SET_HBOUTLOGINFO);
    if (pArg1 != nullptr)
    {
      pSet->HB_SET_HBOUTLOGINFO = set_string(pArg1, pSet->HB_SET_HBOUTLOGINFO);
      hb_xsetinfo(pSet->HB_SET_HBOUTLOGINFO);
    }
    break;
  case HB_SET_OSCODEPAGE:
    if (pSet->hb_set_oscp)
    {
      pResult = hb_itemPutC(pResult, (static_cast<PHB_CODEPAGE>(pSet->hb_set_oscp))->id);
    }
    else if (pResult)
    {
      pResult->clear();
    }
    else
    {
      pResult = hb_itemNew(nullptr);
    }
    if (pArg1 != nullptr)
    {
      if (pArg1->isNil())
      {
        pSet->hb_set_oscp = nullptr;
      }
      else if (pArg1->isString())
      {
        PHB_CODEPAGE cdp = hb_cdpFindExt(pArg1->getCPtr());
        if (cdp)
        {
          pSet->hb_set_oscp = static_cast<void *>(cdp);
        }
      }
      else
      {
        hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
    }
    break;
  case HB_SET_DBCODEPAGE:
    if (pSet->hb_set_dbcp)
    {
      pResult = hb_itemPutC(pResult, (static_cast<PHB_CODEPAGE>(pSet->hb_set_dbcp))->id);
    }
    else if (pResult)
    {
      pResult->clear();
    }
    else
    {
      pResult = hb_itemNew(nullptr);
    }
    if (pArg1 != nullptr)
    {
      if (pArg1->isNil())
      {
        pSet->hb_set_dbcp = nullptr;
      }
      else if (pArg1->isString())
      {
        PHB_CODEPAGE cdp = hb_cdpFindExt(pArg1->getCPtr());
        if (cdp)
        {
          pSet->hb_set_dbcp = static_cast<void *>(cdp);
        }
      }
      else
      {
        hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
      }
    }
    break;

  case HB_SET_INVALID_:
    // Return NIL if called with invalid SET specifier
    break;

#if 0
      // intentionally removed default: clause to enable C compiler warning
      // when not all HB_SET_* cases are implemented. [druzus]
      default:
         break;
#endif
  }
  if (pArg1 != nullptr)
  {
    hb_setListenerNotify(set_specifier, HB_SET_LISTENER_AFTER);
  }

  return pResult;
}

HB_FUNC(SET)
{
  HB_STACK_TLS_PRELOAD
  hb_setGetItem(static_cast<HB_set_enum>(hb_parnidef(1, HB_SET_INVALID_)), hb_stackReturnItem(),
                hb_param(2, Harbour::Item::ANY), hb_param(3, Harbour::Item::ANY));
}

void hb_setInitialize(PHB_SET_STRUCT pSet)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_setInitialize(%p)", static_cast<void*>(pSet)));
#endif

  pSet->HB_SET_ALTERNATE = false;
  pSet->HB_SET_ALTFILE = nullptr;
  pSet->hb_set_althan = nullptr;
  pSet->HB_SET_AUTOPEN = true;
  pSet->HB_SET_AUTORDER = 0;
  pSet->HB_SET_AUTOSHARE = 0;
  pSet->HB_SET_BELL = false;
  pSet->HB_SET_CANCEL = true;
  pSet->hb_set_century = false;
  pSet->hb_set_prndevice = false;
  pSet->HB_SET_COLOR = static_cast<char *>(hb_xgrab(HB_CLRSTR_LEN + 1));
  // NOTE: color must be synced with the one in IsDefColor() function
  hb_strncpy(pSet->HB_SET_COLOR, "W/N,N/W,N/N,N/N,N/W", HB_CLRSTR_LEN);
  pSet->HB_SET_CONFIRM = false;
  pSet->HB_SET_CONSOLE = true;
  pSet->HB_SET_DATEFORMAT = hb_strdup("mm/dd/yy");
  pSet->HB_SET_TIMEFORMAT = hb_strdup("hh:mm:ss.fff");
// Tests shows that Clipper has two different flags to control ALT+D
// and AltD() behavior and on startup these flags are not synchronized.
// When application starts _SET_DEBUG is set to HB_FALSE but debugger
// can be activated by hitting K_ALT_D or calling AltD() function without
// parameter. It means that some other internal flag enables these
// operations.
// Because Harbour is using _SET_DEBUG flag only then we have to
// initialize it to HB_TRUE when debugger is linked to keep real Clipper
// behavior or we will have to add second flag too and try to replicate
// exactly unsynchronized behavior of these flags which exists in Clipper.
// IMHO it's a bug in Clipper (side effect of some internal solutions) and
// we should not try to emulate it [druzus].
#if 0
   pSet->HB_SET_DEBUG = false;
#endif
  pSet->HB_SET_DEBUG = hb_dynsymFind("__DBGENTRY") ? true : false;
  pSet->HB_SET_DECIMALS = 2;
  pSet->HB_SET_DEFAULT = hb_strdup("");
  pSet->HB_SET_DELETED = false;
  pSet->HB_SET_DELIMCHARS = hb_strdup("::");
  pSet->HB_SET_DELIMITERS = false;
  pSet->HB_SET_DEVICE = hb_strdup("SCREEN");
  pSet->HB_SET_EOF = true;
  pSet->HB_SET_EPOCH = 1900;
  pSet->HB_SET_ESCAPE = true;
  pSet->HB_SET_EVENTMASK = INKEY_KEYBOARD;
  pSet->HB_SET_EXACT = false;
  pSet->HB_SET_EXCLUSIVE = true;
  pSet->HB_SET_EXIT = false;
  pSet->HB_SET_EXTRA = false;
  pSet->HB_SET_EXTRAFILE = nullptr;
  pSet->hb_set_extrahan = nullptr;
  pSet->HB_SET_FIXED = false;
  pSet->HB_SET_FORCEOPT = false;
  pSet->HB_SET_HARDCOMMIT = true;
  pSet->HB_SET_IDLEREPEAT = true;
  pSet->HB_SET_INSERT = false;
  pSet->HB_SET_INTENSITY = true;
  pSet->HB_SET_MARGIN = 0;
  pSet->HB_SET_MBLOCKSIZE = 64;
  pSet->HB_SET_MCENTER = false;
  pSet->HB_SET_MESSAGE = 0;
  pSet->HB_SET_MFILEEXT = hb_strdup("");
  pSet->HB_SET_OPTIMIZE = true;
  pSet->HB_SET_PATH = hb_strdup("");
  pSet->hb_set_path = nullptr;
  pSet->HB_SET_PRINTER = false;
  pSet->HB_SET_PRINTFILE = hb_set_PRINTFILE_default();
  pSet->hb_set_printhan = nullptr;
  pSet->HB_SET_SCOREBOARD = true;
  pSet->HB_SET_SCROLLBREAK = true;
  pSet->HB_SET_SOFTSEEK = false;
  pSet->HB_SET_STRICTREAD = false;
  pSet->HB_SET_TYPEAHEAD = HB_DEFAULT_INKEY_BUFSIZE;
  pSet->HB_SET_UNIQUE = false;
  pSet->HB_SET_FILECASE = HB_SET_CASE_MIXED;
  pSet->HB_SET_DIRCASE = HB_SET_CASE_MIXED;
  pSet->HB_SET_DIRSEPARATOR = HB_OS_PATH_DELIM_CHR;
  pSet->HB_SET_VIDEOMODE = 0;
  pSet->HB_SET_WRAP = false;
  pSet->HB_SET_DBFLOCKSCHEME = 0;
  pSet->HB_SET_DEFEXTENSIONS = true;
  pSet->HB_SET_EOL = hb_strdup(hb_conNewLine());
  pSet->HB_SET_TRIMFILENAME = false;
  pSet->HB_SET_HBOUTLOG = hb_strdup("hb_out.log");
  pSet->HB_SET_HBOUTLOGINFO = hb_strdup("");

  hb_xsetfilename(pSet->HB_SET_HBOUTLOG);
  hb_xsetinfo(pSet->HB_SET_HBOUTLOGINFO);

  pSet->hb_set_oscp = nullptr;
  pSet->hb_set_dbcp = nullptr;

  pSet->hb_set_listener = nullptr;
}

void hb_setRelease(PHB_SET_STRUCT pSet)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_setRelease()"));
#endif

  close_handle(pSet, HB_SET_ALTFILE);
  close_handle(pSet, HB_SET_EXTRAFILE);
  close_handle(pSet, HB_SET_PRINTFILE);

  if (pSet->HB_SET_ALTFILE)
  {
    hb_xfree(pSet->HB_SET_ALTFILE);
  }
  if (pSet->HB_SET_DATEFORMAT)
  {
    hb_xfree(pSet->HB_SET_DATEFORMAT);
  }
  if (pSet->HB_SET_TIMEFORMAT)
  {
    hb_xfree(pSet->HB_SET_TIMEFORMAT);
  }
  if (pSet->HB_SET_DEFAULT)
  {
    hb_xfree(pSet->HB_SET_DEFAULT);
  }
  if (pSet->HB_SET_DELIMCHARS)
  {
    hb_xfree(pSet->HB_SET_DELIMCHARS);
  }
  if (pSet->HB_SET_DEVICE)
  {
    hb_xfree(pSet->HB_SET_DEVICE);
  }
  if (pSet->HB_SET_EXTRAFILE)
  {
    hb_xfree(pSet->HB_SET_EXTRAFILE);
  }
  if (pSet->HB_SET_MFILEEXT)
  {
    hb_xfree(pSet->HB_SET_MFILEEXT);
  }
  if (pSet->HB_SET_PATH)
  {
    hb_xfree(pSet->HB_SET_PATH);
  }
  if (pSet->HB_SET_PRINTFILE)
  {
    hb_xfree(pSet->HB_SET_PRINTFILE);
  }
  if (pSet->HB_SET_COLOR)
  {
    hb_xfree(pSet->HB_SET_COLOR);
  }
  if (pSet->HB_SET_EOL)
  {
    hb_xfree(pSet->HB_SET_EOL);
  }
  if (pSet->HB_SET_HBOUTLOG)
  {
    hb_xfree(pSet->HB_SET_HBOUTLOG);
  }
  if (pSet->HB_SET_HBOUTLOGINFO)
  {
    hb_xfree(pSet->HB_SET_HBOUTLOGINFO);
  }

  hb_fsFreeSearchPath(pSet->hb_set_path);

  // Free all set listeners
  if (pSet->hb_set_listener)
  {
    PHB_SET_LISTENER pListener = (static_cast<PHB_SET_LISTENER_LST>(pSet->hb_set_listener))->first;
    while (pListener)
    {
      PHB_SET_LISTENER pNext = pListener->next;
      hb_xfree(pListener);
      pListener = pNext;
    }
    hb_xfree(pSet->hb_set_listener);
  }
}

PHB_SET_STRUCT hb_setClone(PHB_SET_STRUCT pSrc)
{
  auto pSet = static_cast<PHB_SET_STRUCT>(hb_xgrab(sizeof(HB_SET_STRUCT)));

  memcpy(pSet, pSrc, sizeof(HB_SET_STRUCT));

  pSet->hb_set_althan = pSet->hb_set_extrahan = pSet->hb_set_printhan = nullptr;
  pSet->hb_set_path = nullptr;
  pSet->hb_set_listener = nullptr;

  pSet->HB_SET_TYPEAHEAD = HB_DEFAULT_INKEY_BUFSIZE;

  pSet->HB_SET_COLOR = static_cast<char *>(hb_xgrab(HB_CLRSTR_LEN + 1));
  hb_strncpy(pSet->HB_SET_COLOR, pSrc->HB_SET_COLOR, HB_CLRSTR_LEN);

  if (pSet->HB_SET_ALTFILE)
  {
    pSet->HB_SET_ALTFILE = hb_strdup(pSet->HB_SET_ALTFILE);
  }
  if (pSet->HB_SET_DATEFORMAT)
  {
    pSet->HB_SET_DATEFORMAT = hb_strdup(pSet->HB_SET_DATEFORMAT);
  }
  if (pSet->HB_SET_TIMEFORMAT)
  {
    pSet->HB_SET_TIMEFORMAT = hb_strdup(pSet->HB_SET_TIMEFORMAT);
  }
  if (pSet->HB_SET_DEFAULT)
  {
    pSet->HB_SET_DEFAULT = hb_strdup(pSet->HB_SET_DEFAULT);
  }
  if (pSet->HB_SET_DELIMCHARS)
  {
    pSet->HB_SET_DELIMCHARS = hb_strdup(pSet->HB_SET_DELIMCHARS);
  }
  if (pSet->HB_SET_DEVICE)
  {
    pSet->HB_SET_DEVICE = hb_strdup(pSet->HB_SET_DEVICE);
  }
  if (pSet->HB_SET_EXTRAFILE)
  {
    pSet->HB_SET_EXTRAFILE = hb_strdup(pSet->HB_SET_EXTRAFILE);
  }
  if (pSet->HB_SET_MFILEEXT)
  {
    pSet->HB_SET_MFILEEXT = hb_strdup(pSet->HB_SET_MFILEEXT);
  }
  if (pSet->HB_SET_PATH)
  {
    pSet->HB_SET_PATH = hb_strdup(pSet->HB_SET_PATH);
  }
  if (pSet->HB_SET_PRINTFILE)
  {
    pSet->HB_SET_PRINTFILE = hb_strdup(pSet->HB_SET_PRINTFILE);
  }
  if (pSet->HB_SET_EOL)
  {
    pSet->HB_SET_EOL = hb_strdup(pSet->HB_SET_EOL);
  }
  if (pSet->HB_SET_HBOUTLOG)
  {
    pSet->HB_SET_HBOUTLOG = hb_strdup(pSet->HB_SET_HBOUTLOG);
  }
  if (pSet->HB_SET_HBOUTLOGINFO)
  {
    pSet->HB_SET_HBOUTLOGINFO = hb_strdup(pSet->HB_SET_HBOUTLOGINFO);
  }

  hb_fsAddSearchPath(pSet->HB_SET_PATH, &pSet->hb_set_path);

  return pSet;
}

int hb_setListenerAdd(HB_SET_LISTENER_CALLBACK *callback)
{
  HB_STACK_TLS_PRELOAD
  PHB_SET_STRUCT pSet = hb_stackSetStruct();
  auto p_sl = static_cast<PHB_SET_LISTENER>(hb_xgrab(sizeof(HB_SET_LISTENER)));

  if (!pSet->hb_set_listener)
  {
    pSet->hb_set_listener = hb_xgrabz(sizeof(HB_SET_LISTENER_LST));
  }

  auto pList = static_cast<PHB_SET_LISTENER_LST>(pSet->hb_set_listener);

  p_sl->callback = callback;
  p_sl->listener = ++pList->counter;
  p_sl->next = nullptr;

  if (pList->last)
  {
    pList->last->next = p_sl;
  }
  else if (!pList->first)
  {
    pList->first = p_sl;
  }
  pList->last = p_sl;

  return p_sl->listener;
}

void hb_setListenerNotify(HB_set_enum set, HB_set_listener_enum when)
{
  HB_STACK_TLS_PRELOAD
  auto pList = static_cast<PHB_SET_LISTENER_LST>(hb_stackSetStruct()->hb_set_listener);

  if (pList)
  {
    PHB_SET_LISTENER p_sl = pList->first;
    while (p_sl)
    {
      (*p_sl->callback)(set, when);
      p_sl = p_sl->next;
    }
  }
}

int hb_setListenerRemove(int listener)
{
  HB_STACK_TLS_PRELOAD
  auto pList = static_cast<PHB_SET_LISTENER_LST>(hb_stackSetStruct()->hb_set_listener);

  if (pList)
  {
    PHB_SET_LISTENER p_sl = pList->first;
    PHB_SET_LISTENER p_sl_prev = nullptr;
    while (p_sl)
    {
      if (listener == p_sl->listener)
      {
        listener = -listener;
        if (p_sl_prev)
        {
          p_sl_prev->next = p_sl->next;
        }
        else
        {
          pList->first = p_sl->next;
        }
        if (p_sl == pList->last)
        {
          pList->last = p_sl_prev;
        }
        hb_xfree(p_sl);
        break;
      }
      p_sl_prev = p_sl;
      p_sl = p_sl->next;
    }
  }
  return listener;
}

HB_BOOL hb_setSetItem(HB_set_enum set_specifier, PHB_ITEM pItem)
{
  HB_STACK_TLS_PRELOAD
  auto fResult = false;

  if (pItem != nullptr)
  {
    PHB_SET_STRUCT pSet = hb_stackSetStruct();
    char *szValue;
    int iValue;

    hb_setListenerNotify(set_specifier, HB_SET_LISTENER_BEFORE);

    switch (set_specifier)
    {
    case HB_SET_ALTFILE:
    case HB_SET_EXTRAFILE:
    case HB_SET_PRINTFILE:
      // This sets needs 3rd parameter to indicate additive mode
      // so they cannot be fully supported by this function
      if (pItem->isString() || pItem->isNil())
      {
        open_handle(pSet, pItem->getCPtr(), false, set_specifier);
        fResult = true;
        if (set_specifier == HB_SET_PRINTFILE && pSet->HB_SET_PRINTFILE == nullptr)
        {
          pSet->HB_SET_PRINTFILE = hb_set_PRINTFILE_default();
        }
      }
      break;

    case HB_SET_ALTERNATE:
      if (pItem->isLogical())
      {
        pSet->HB_SET_ALTERNATE = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_AUTOPEN:
      if (pItem->isLogical())
      {
        pSet->HB_SET_AUTOPEN = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_BELL:
      if (pItem->isLogical())
      {
        pSet->HB_SET_BELL = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_CANCEL:
      if (pItem->isLogical())
      {
        pSet->HB_SET_CANCEL = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_CONFIRM:
      if (pItem->isLogical())
      {
        pSet->HB_SET_CONFIRM = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_CONSOLE:
      if (pItem->isLogical())
      {
        pSet->HB_SET_CONSOLE = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_DEBUG:
      if (pItem->isLogical())
      {
        pSet->HB_SET_DEBUG = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_DELETED:
      if (pItem->isLogical())
      {
        pSet->HB_SET_DELETED = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_DELIMITERS:
      if (pItem->isLogical())
      {
        pSet->HB_SET_DELIMITERS = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_EOF:
      if (pItem->isLogical())
      {
        pSet->HB_SET_EOF = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_ESCAPE:
      if (pItem->isLogical())
      {
        pSet->HB_SET_ESCAPE = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_EXACT:
      if (pItem->isLogical())
      {
        pSet->HB_SET_EXACT = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_EXCLUSIVE:
      if (pItem->isLogical())
      {
        pSet->HB_SET_EXCLUSIVE = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_EXIT:
      if (pItem->isLogical())
      {
        pSet->HB_SET_EXIT = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_EXTRA:
      if (pItem->isLogical())
      {
        pSet->HB_SET_EXTRA = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_FIXED:
      if (pItem->isLogical())
      {
        pSet->HB_SET_FIXED = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_IDLEREPEAT:
      if (pItem->isLogical())
      {
        pSet->HB_SET_IDLEREPEAT = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_INSERT:
      if (pItem->isLogical())
      {
        pSet->HB_SET_INSERT = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_INTENSITY:
      if (pItem->isLogical())
      {
        pSet->HB_SET_INTENSITY = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_MCENTER:
      if (pItem->isLogical())
      {
        pSet->HB_SET_MCENTER = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_OPTIMIZE:
      if (pItem->isLogical())
      {
        pSet->HB_SET_OPTIMIZE = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_FORCEOPT:
      if (pItem->isLogical())
      {
        pSet->HB_SET_FORCEOPT = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_PRINTER:
      if (pItem->isLogical())
      {
        pSet->HB_SET_PRINTER = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_SCOREBOARD:
      if (pItem->isLogical())
      {
        pSet->HB_SET_SCOREBOARD = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_SCROLLBREAK:
      if (pItem->isLogical())
      {
        pSet->HB_SET_SCROLLBREAK = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_SOFTSEEK:
      if (pItem->isLogical())
      {
        pSet->HB_SET_SOFTSEEK = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_STRICTREAD:
      if (pItem->isLogical())
      {
        pSet->HB_SET_STRICTREAD = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_UNIQUE:
      if (pItem->isLogical())
      {
        pSet->HB_SET_UNIQUE = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_WRAP:
      if (pItem->isLogical())
      {
        pSet->HB_SET_WRAP = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_HARDCOMMIT:
      if (pItem->isLogical())
      {
        pSet->HB_SET_HARDCOMMIT = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_DEFEXTENSIONS:
      if (pItem->isLogical())
      {
        pSet->HB_SET_DEFEXTENSIONS = pItem->getL();
        fResult = true;
      }
      break;
    case HB_SET_TRIMFILENAME:
      if (pItem->isLogical())
      {
        pSet->HB_SET_TRIMFILENAME = pItem->getL();
        fResult = true;
      }
      break;

    case HB_SET_DECIMALS:
      if (pItem->isNumeric())
      {
        iValue = pItem->getNI();
        if (iValue >= 0)
        {
          pSet->HB_SET_DECIMALS = iValue;
          fResult = true;
        }
      }
      break;
    case HB_SET_EPOCH:
      if (pItem->isNumeric())
      {
        iValue = pItem->getNI();
        if (iValue >= 0)
        {
          pSet->HB_SET_EPOCH = iValue;
          fResult = true;
        }
      }
      break;
    case HB_SET_MBLOCKSIZE:
      if (pItem->isNumeric())
      {
        iValue = pItem->getNI();
        if (iValue >= 0)
        {
          pSet->HB_SET_MBLOCKSIZE = iValue;
          fResult = true;
        }
      }
      break;
    case HB_SET_DBFLOCKSCHEME:
      if (pItem->isNumeric())
      {
        iValue = pItem->getNI();
        if (iValue >= 0)
        {
          pSet->HB_SET_DBFLOCKSCHEME = iValue;
          fResult = true;
        }
      }
      break;
    case HB_SET_AUTORDER:
      if (pItem->isNumeric())
      {
        iValue = pItem->getNI();
        if (iValue >= 0)
        {
          pSet->HB_SET_AUTORDER = iValue;
          fResult = true;
        }
      }
      break;
    case HB_SET_AUTOSHARE:
      if (pItem->isNumeric())
      {
        iValue = pItem->getNI();
        if (iValue >= 0)
        {
          pSet->HB_SET_AUTOSHARE = iValue;
          fResult = true;
        }
      }
      break;
    case HB_SET_CURSOR:
      if (pItem->isNumeric())
      {
        hb_conSetCursor(true, pItem->getNI());
        fResult = true;
      }
      break;
    case HB_SET_EVENTMASK:
      if (pItem->isNumeric())
      {
        iValue = pItem->getNI();
        if (iValue >= 0)
        {
          pSet->HB_SET_EVENTMASK = iValue;
          fResult = true;
        }
      }
      break;
    case HB_SET_MARGIN:
      if (pItem->isNumeric())
      {
        iValue = pItem->getNI();
        if (iValue >= 0)
        {
          pSet->HB_SET_MARGIN = iValue;
          fResult = true;
        }
      }
      break;
    case HB_SET_MESSAGE:
      if (pItem->isNumeric())
      {
        iValue = pItem->getNI();
        if (iValue >= 0)
        {
          pSet->HB_SET_MESSAGE = iValue;
          fResult = true;
        }
      }
      break;
    case HB_SET_TYPEAHEAD:
      if (pItem->isNumeric())
      {
        // Set the value and limit the range
        pSet->HB_SET_TYPEAHEAD = pItem->getNI();
        if (pSet->HB_SET_TYPEAHEAD == 0)
        {
          /* Do nothing */;
        }
        else if (pSet->HB_SET_TYPEAHEAD < 16)
        {
          pSet->HB_SET_TYPEAHEAD = 16;
        }
        else if (pSet->HB_SET_TYPEAHEAD > 4096)
        {
          pSet->HB_SET_TYPEAHEAD = 4096;
        }
        // reset keyboard buffer
        hb_inkeyReset();
        fResult = true;
      }
      break;
    case HB_SET_VIDEOMODE:
      if (pItem->isNumeric())
      {
        pSet->HB_SET_VIDEOMODE = pItem->getNI();
        fResult = true;
      }
      break;

    case HB_SET_COLOR:
      if (pItem->isString())
      {
        hb_conSetColor(pItem->getCPtr());
        fResult = true;
      }
      break;
    case HB_SET_LANGUAGE:
      if (pItem->isString())
      {
        hb_langSelectID(pItem->getCPtr());
        fResult = true;
      }
      break;
    case HB_SET_CODEPAGE:
      if (pItem->isString())
      {
        hb_cdpSelectID(pItem->getCPtr());
        fResult = true;
      }
      break;
    case HB_SET_FILECASE:
    case HB_SET_DIRCASE:
      iValue = -1;
      if (pItem->isString())
      {
        if (!hb_stricmp(pItem->getCPtr(), "LOWER"))
        {
          iValue = HB_SET_CASE_LOWER;
        }
        else if (!hb_stricmp(pItem->getCPtr(), "UPPER"))
        {
          iValue = HB_SET_CASE_UPPER;
        }
        else if (!hb_stricmp(pItem->getCPtr(), "MIXED"))
        {
          iValue = HB_SET_CASE_MIXED;
        }
      }
      else if (pItem->isNumeric())
      {
        iValue = pItem->getNI();
      }

      if (iValue == HB_SET_CASE_LOWER || iValue == HB_SET_CASE_UPPER || iValue == HB_SET_CASE_MIXED)
      {
        if (set_specifier == HB_SET_FILECASE)
        {
          pSet->HB_SET_FILECASE = iValue;
        }
        else
        {
          pSet->HB_SET_DIRCASE = iValue;
        }
        fResult = true;
      }
      break;
    case HB_SET_DATEFORMAT:
      if (pItem->isString())
      {
        int iYear = 0;

        szValue = hb_strndup(pItem->getCPtr(), USHRT_MAX);
        if (pSet->HB_SET_DATEFORMAT)
        {
          hb_xfree(pSet->HB_SET_DATEFORMAT);
        }
        pSet->HB_SET_DATEFORMAT = szValue;
        while (*szValue)
        {
          if (*szValue == 'Y' || *szValue == 'y')
          {
            ++iYear;
          }
          else if (iYear)
          { // Only count the first set of consecutive "Y"s.
            break;
          }
          ++szValue;
        }
        // CENTURY is not controlled directly by SET, so there is no
        // notification for changing it indirectly via DATE FORMAT.
        pSet->hb_set_century = iYear >= 4;
        fResult = true;
      }
      break;
    case HB_SET_TIMEFORMAT:
      if (pItem->isString())
      {
        szValue = hb_strndup(pItem->getCPtr(), USHRT_MAX);
        if (pSet->HB_SET_TIMEFORMAT)
        {
          hb_xfree(pSet->HB_SET_TIMEFORMAT);
        }
        pSet->HB_SET_TIMEFORMAT = szValue;
        fResult = true;
      }
      break;
    case HB_SET_DIRSEPARATOR:
      if (pItem->getCLen() > 0)
      {
        pSet->HB_SET_DIRSEPARATOR = pItem->getCPtr()[0];
        fResult = true;
      }
      break;
    case HB_SET_DEVICE:
      if (pItem->isString())
      {
        szValue = hb_strndup(pItem->getCPtr(), USHRT_MAX);
        if (pSet->HB_SET_DEVICE)
        {
          hb_xfree(pSet->HB_SET_DEVICE);
        }
        pSet->HB_SET_DEVICE = szValue;
        pSet->hb_set_prndevice = strlen(szValue) >= 4 && hb_strnicmp(szValue, "PRIN", 4) == 0;
        fResult = true;
      }
      break;
    case HB_SET_MFILEEXT:
      if (pItem->isString() || pItem->isNil())
      {
        szValue = hb_strndup(pItem->getCPtr(), USHRT_MAX);
        if (pSet->HB_SET_MFILEEXT)
        {
          hb_xfree(pSet->HB_SET_MFILEEXT);
        }
        pSet->HB_SET_MFILEEXT = szValue;
        fResult = true;
      }
      break;
    case HB_SET_DEFAULT:
      if (pItem->isString() || pItem->isNil())
      {
        szValue = hb_strndup(pItem->getCPtr(), USHRT_MAX);
        if (pSet->HB_SET_DEFAULT)
        {
          hb_xfree(pSet->HB_SET_DEFAULT);
        }
        pSet->HB_SET_DEFAULT = szValue;
        fResult = true;
      }
      break;
    case HB_SET_PATH:
      if (pItem->isString() || pItem->isNil())
      {
        szValue = hb_strndup(pItem->getCPtr(), USHRT_MAX);
        if (pSet->HB_SET_PATH)
        {
          hb_xfree(pSet->HB_SET_PATH);
        }
        pSet->HB_SET_PATH = szValue;

        hb_fsFreeSearchPath(pSet->hb_set_path);
        pSet->hb_set_path = nullptr;
        hb_fsAddSearchPath(pSet->HB_SET_PATH, &pSet->hb_set_path);

        fResult = true;
      }
      break;
    case HB_SET_DELIMCHARS:
      if (pItem->isString() || pItem->isNil())
      {
        szValue = hb_strndup(pItem->getCPtr(), USHRT_MAX);
        if (pSet->HB_SET_DELIMCHARS)
        {
          hb_xfree(pSet->HB_SET_DELIMCHARS);
        }
        pSet->HB_SET_DELIMCHARS = szValue;
        fResult = true;
      }
      break;
    case HB_SET_EOL:
      if (pItem->isString() || pItem->isNil())
      {
        szValue = hb_strndup(pItem->getCPtr(), USHRT_MAX);
        if (pSet->HB_SET_EOL)
        {
          hb_xfree(pSet->HB_SET_EOL);
        }
        pSet->HB_SET_EOL = szValue;
        fResult = true;
      }
      break;
    case HB_SET_HBOUTLOG:
      if (pItem->isString() || pItem->isNil())
      {
        if (pItem->isNil())
        {
          szValue = nullptr;
        }
        else
        {
          szValue = hb_strndup(pItem->getCPtr(), USHRT_MAX);
        }
        if (pSet->HB_SET_HBOUTLOG)
        {
          hb_xfree(pSet->HB_SET_HBOUTLOG);
        }
        pSet->HB_SET_HBOUTLOG = szValue;
        hb_xsetfilename(pSet->HB_SET_HBOUTLOG);
        fResult = true;
      }
      break;
    case HB_SET_HBOUTLOGINFO:
      if (pItem->isString() || pItem->isNil())
      {
        szValue = hb_strndup(pItem->getCPtr(), USHRT_MAX);
        if (pSet->HB_SET_HBOUTLOGINFO)
        {
          hb_xfree(pSet->HB_SET_HBOUTLOGINFO);
        }
        pSet->HB_SET_HBOUTLOGINFO = szValue;
        hb_xsetinfo(pSet->HB_SET_HBOUTLOGINFO);
        fResult = true;
      }
      break;
    case HB_SET_OSCODEPAGE:
      if (pItem->isNil())
      {
        pSet->hb_set_oscp = nullptr;
        fResult = true;
      }
      else if (pItem->isString())
      {
        PHB_CODEPAGE cdp = hb_cdpFindExt(pItem->getCPtr());
        if (cdp)
        {
          pSet->hb_set_oscp = static_cast<void *>(cdp);
          fResult = true;
        }
      }
      break;
    case HB_SET_DBCODEPAGE:
      if (pItem->isNil())
      {
        pSet->hb_set_dbcp = nullptr;
        fResult = true;
      }
      else if (pItem->isString())
      {
        PHB_CODEPAGE cdp = hb_cdpFindExt(pItem->getCPtr());
        if (cdp)
        {
          pSet->hb_set_dbcp = static_cast<void *>(cdp);
          fResult = true;
        }
      }
      break;

    case HB_SET_INVALID_:
      break;
#if 0
         // intentionally removed default: clause to enable C compiler warning
         // when not all HB_SET_* cases are implemented. [druzus]
         default:
            break;
#endif
    }
    hb_setListenerNotify(set_specifier, HB_SET_LISTENER_AFTER);
  }

  return fResult;
}

HB_BOOL hb_setSetItem2(HB_set_enum set_specifier, PHB_ITEM pItem1, PHB_ITEM pItem2)
{
  auto fResult = false;

  if (pItem1)
  {
    switch (set_specifier)
    {
    case HB_SET_ALTFILE:
    case HB_SET_EXTRAFILE:
    case HB_SET_PRINTFILE:
      if (pItem1->isString() || pItem1->isNil())
      {
        HB_STACK_TLS_PRELOAD
        PHB_SET_STRUCT pSet = hb_stackSetStruct();

        hb_setListenerNotify(set_specifier, HB_SET_LISTENER_BEFORE);

        open_handle(pSet, pItem1->getCPtr(), set_logical(pItem2, false), set_specifier);
        fResult = true;
        if (set_specifier == HB_SET_PRINTFILE && pSet->HB_SET_PRINTFILE == nullptr)
        {
          pSet->HB_SET_PRINTFILE = hb_set_PRINTFILE_default();
        }

        hb_setListenerNotify(set_specifier, HB_SET_LISTENER_AFTER);
      }
      break;
    default:
      fResult = hb_setSetItem(set_specifier, pItem1);
    }
  }
  return fResult;
}

HB_BOOL hb_setGetL(HB_set_enum set_specifier)
{
  HB_STACK_TLS_PRELOAD
  PHB_SET_STRUCT pSet = hb_stackSetStruct();

  switch (set_specifier)
  {
  case HB_SET_ALTERNATE:
    return pSet->HB_SET_ALTERNATE;
  case HB_SET_AUTOPEN:
    return pSet->HB_SET_AUTOPEN;
  case HB_SET_BELL:
    return pSet->HB_SET_BELL;
  case HB_SET_CANCEL:
    return pSet->HB_SET_CANCEL;
  case HB_SET_CONFIRM:
    return pSet->HB_SET_CONFIRM;
  case HB_SET_CONSOLE:
    return pSet->HB_SET_CONSOLE;
  case HB_SET_DEBUG:
    return pSet->HB_SET_DEBUG;
  case HB_SET_DELETED:
    return pSet->HB_SET_DELETED;
  case HB_SET_DELIMITERS:
    return pSet->HB_SET_DELIMITERS;
  case HB_SET_EOF:
    return pSet->HB_SET_EOF;
  case HB_SET_ESCAPE:
    return pSet->HB_SET_ESCAPE;
  case HB_SET_EXACT:
    return pSet->HB_SET_EXACT;
  case HB_SET_EXCLUSIVE:
    return pSet->HB_SET_EXCLUSIVE;
  case HB_SET_EXIT:
    return pSet->HB_SET_EXIT;
  case HB_SET_EXTRA:
    return pSet->HB_SET_EXTRA;
  case HB_SET_FIXED:
    return pSet->HB_SET_FIXED;
  case HB_SET_IDLEREPEAT:
    return pSet->HB_SET_IDLEREPEAT;
  case HB_SET_INSERT:
    return pSet->HB_SET_INSERT;
  case HB_SET_INTENSITY:
    return pSet->HB_SET_INTENSITY;
  case HB_SET_MCENTER:
    return pSet->HB_SET_MCENTER;
  case HB_SET_OPTIMIZE:
    return pSet->HB_SET_OPTIMIZE;
  case HB_SET_FORCEOPT:
    return pSet->HB_SET_FORCEOPT;
  case HB_SET_PRINTER:
    return pSet->HB_SET_PRINTER;
  case HB_SET_SCOREBOARD:
    return pSet->HB_SET_SCOREBOARD;
  case HB_SET_SCROLLBREAK:
    return pSet->HB_SET_SCROLLBREAK;
  case HB_SET_SOFTSEEK:
    return pSet->HB_SET_SOFTSEEK;
  case HB_SET_STRICTREAD:
    return pSet->HB_SET_STRICTREAD;
  case HB_SET_UNIQUE:
    return pSet->HB_SET_UNIQUE;
  case HB_SET_WRAP:
    return pSet->HB_SET_WRAP;
  case HB_SET_HARDCOMMIT:
    return pSet->HB_SET_HARDCOMMIT;
  case HB_SET_DEFEXTENSIONS:
    return pSet->HB_SET_DEFEXTENSIONS;
  case HB_SET_TRIMFILENAME:
    return pSet->HB_SET_TRIMFILENAME;

  case HB_SET_ALTFILE:
  case HB_SET_AUTORDER:
  case HB_SET_AUTOSHARE:
  case HB_SET_COLOR:
  case HB_SET_CURSOR:
  case HB_SET_DATEFORMAT:
  case HB_SET_TIMEFORMAT:
  case HB_SET_DECIMALS:
  case HB_SET_DEFAULT:
  case HB_SET_DELIMCHARS:
  case HB_SET_DEVICE:
  case HB_SET_EPOCH:
  case HB_SET_EVENTMASK:
  case HB_SET_EXTRAFILE:
  case HB_SET_MARGIN:
  case HB_SET_MBLOCKSIZE:
  case HB_SET_MESSAGE:
  case HB_SET_MFILEEXT:
  case HB_SET_PATH:
  case HB_SET_PRINTFILE:
  case HB_SET_TYPEAHEAD:
  case HB_SET_VIDEOMODE:
  case HB_SET_LANGUAGE:
  case HB_SET_CODEPAGE:
  case HB_SET_FILECASE:
  case HB_SET_DIRCASE:
  case HB_SET_DIRSEPARATOR:
  case HB_SET_DBFLOCKSCHEME:
  case HB_SET_EOL:
  case HB_SET_HBOUTLOG:
  case HB_SET_HBOUTLOGINFO:
  case HB_SET_OSCODEPAGE:
  case HB_SET_DBCODEPAGE:
  case HB_SET_INVALID_:
    break;
#if 0
      // intentionally removed default: clause to enable C compiler warning
      // when not all HB_SET_* cases are implemented. [druzus]
      default:
         break;
#endif
  }

  hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, 0);
  return false;
}

const char *hb_setGetCPtr(HB_set_enum set_specifier)
{
  HB_STACK_TLS_PRELOAD
  PHB_SET_STRUCT pSet = hb_stackSetStruct();

  switch (set_specifier)
  {
  case HB_SET_ALTFILE:
    return pSet->HB_SET_ALTFILE;
  case HB_SET_COLOR:
    return pSet->HB_SET_COLOR;
  case HB_SET_DATEFORMAT:
    return pSet->HB_SET_DATEFORMAT;
  case HB_SET_TIMEFORMAT:
    return pSet->HB_SET_TIMEFORMAT;
  case HB_SET_DEFAULT:
    return pSet->HB_SET_DEFAULT;
  case HB_SET_DELIMCHARS:
    return pSet->HB_SET_DELIMCHARS;
  case HB_SET_DEVICE:
    return pSet->HB_SET_DEVICE;
  case HB_SET_EXTRAFILE:
    return pSet->HB_SET_EXTRAFILE;
  case HB_SET_PATH:
    return pSet->HB_SET_PATH;
  case HB_SET_MFILEEXT:
    return pSet->HB_SET_MFILEEXT;
  case HB_SET_PRINTFILE:
    return pSet->HB_SET_PRINTFILE;
  case HB_SET_EOL:
    return pSet->HB_SET_EOL;
  case HB_SET_HBOUTLOG:
    return pSet->HB_SET_HBOUTLOG;
  case HB_SET_HBOUTLOGINFO:
    return pSet->HB_SET_HBOUTLOGINFO;
  case HB_SET_OSCODEPAGE:
    return pSet->hb_set_oscp ? (static_cast<PHB_CODEPAGE>(pSet->hb_set_oscp))->id : nullptr;
  case HB_SET_DBCODEPAGE:
    return pSet->hb_set_dbcp ? (static_cast<PHB_CODEPAGE>(pSet->hb_set_dbcp))->id : nullptr;
  case HB_SET_LANGUAGE:
    return hb_langID();
  case HB_SET_CODEPAGE:
    return hb_cdpID();
  case HB_SET_ALTERNATE:
  case HB_SET_AUTOPEN:
  case HB_SET_AUTORDER:
  case HB_SET_AUTOSHARE:
  case HB_SET_BELL:
  case HB_SET_CANCEL:
  case HB_SET_CONFIRM:
  case HB_SET_CONSOLE:
  case HB_SET_CURSOR:
  case HB_SET_DEBUG:
  case HB_SET_DECIMALS:
  case HB_SET_DELETED:
  case HB_SET_DELIMITERS:
  case HB_SET_EOF:
  case HB_SET_EPOCH:
  case HB_SET_ESCAPE:
  case HB_SET_EVENTMASK:
  case HB_SET_EXACT:
  case HB_SET_EXCLUSIVE:
  case HB_SET_EXIT:
  case HB_SET_EXTRA:
  case HB_SET_FIXED:
  case HB_SET_INSERT:
  case HB_SET_INTENSITY:
  case HB_SET_MARGIN:
  case HB_SET_MBLOCKSIZE:
  case HB_SET_MCENTER:
  case HB_SET_MESSAGE:
  case HB_SET_OPTIMIZE:
  case HB_SET_FORCEOPT:
  case HB_SET_STRICTREAD:
  case HB_SET_HARDCOMMIT:
  case HB_SET_PRINTER:
  case HB_SET_SCOREBOARD:
  case HB_SET_SCROLLBREAK:
  case HB_SET_SOFTSEEK:
  case HB_SET_TYPEAHEAD:
  case HB_SET_UNIQUE:
  case HB_SET_VIDEOMODE:
  case HB_SET_WRAP:
  case HB_SET_IDLEREPEAT:
  case HB_SET_FILECASE:
  case HB_SET_DIRCASE:
  case HB_SET_DIRSEPARATOR:
  case HB_SET_DBFLOCKSCHEME:
  case HB_SET_DEFEXTENSIONS:
  case HB_SET_TRIMFILENAME:
  case HB_SET_INVALID_:
    break;
#if 0
      // intentionally removed default: clause to enable C compiler warning
      // when not all HB_SET_* cases are implemented. [druzus]
      default:
         break;
#endif
  }

  hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, 0);
  return nullptr;
}

int hb_setGetNI(HB_set_enum set_specifier)
{
  HB_STACK_TLS_PRELOAD
  PHB_SET_STRUCT pSet = hb_stackSetStruct();

  switch (set_specifier)
  {
  case HB_SET_AUTORDER:
    return pSet->HB_SET_AUTORDER;
  case HB_SET_AUTOSHARE:
    return pSet->HB_SET_AUTOSHARE;
  case HB_SET_DECIMALS:
    return pSet->HB_SET_DECIMALS;
  case HB_SET_EPOCH:
    return pSet->HB_SET_EPOCH;
  case HB_SET_EVENTMASK:
    return pSet->HB_SET_EVENTMASK;
  case HB_SET_MARGIN:
    return pSet->HB_SET_MARGIN;
  case HB_SET_MBLOCKSIZE:
    return pSet->HB_SET_MBLOCKSIZE;
  case HB_SET_MESSAGE:
    return pSet->HB_SET_MESSAGE;
  case HB_SET_TYPEAHEAD:
    return pSet->HB_SET_TYPEAHEAD;
  case HB_SET_FILECASE:
    return pSet->HB_SET_FILECASE;
  case HB_SET_DIRCASE:
    return pSet->HB_SET_DIRCASE;
  case HB_SET_DIRSEPARATOR:
    return pSet->HB_SET_DIRSEPARATOR;
  case HB_SET_VIDEOMODE:
    return pSet->HB_SET_VIDEOMODE;
  case HB_SET_DBFLOCKSCHEME:
    return pSet->HB_SET_DBFLOCKSCHEME;

  case HB_SET_ALTERNATE:
  case HB_SET_ALTFILE:
  case HB_SET_AUTOPEN:
  case HB_SET_BELL:
  case HB_SET_CANCEL:
  case HB_SET_COLOR:
  case HB_SET_CONFIRM:
  case HB_SET_CONSOLE:
  case HB_SET_CURSOR:
  case HB_SET_DATEFORMAT:
  case HB_SET_TIMEFORMAT:
  case HB_SET_DEBUG:
  case HB_SET_DEFAULT:
  case HB_SET_DELETED:
  case HB_SET_DELIMCHARS:
  case HB_SET_DELIMITERS:
  case HB_SET_DEVICE:
  case HB_SET_EOF:
  case HB_SET_ESCAPE:
  case HB_SET_EXACT:
  case HB_SET_EXCLUSIVE:
  case HB_SET_EXIT:
  case HB_SET_EXTRA:
  case HB_SET_EXTRAFILE:
  case HB_SET_FIXED:
  case HB_SET_INSERT:
  case HB_SET_INTENSITY:
  case HB_SET_MCENTER:
  case HB_SET_MFILEEXT:
  case HB_SET_OPTIMIZE:
  case HB_SET_FORCEOPT:
  case HB_SET_STRICTREAD:
  case HB_SET_HARDCOMMIT:
  case HB_SET_PATH:
  case HB_SET_PRINTER:
  case HB_SET_PRINTFILE:
  case HB_SET_SCOREBOARD:
  case HB_SET_SCROLLBREAK:
  case HB_SET_SOFTSEEK:
  case HB_SET_UNIQUE:
  case HB_SET_WRAP:
  case HB_SET_LANGUAGE:
  case HB_SET_CODEPAGE:
  case HB_SET_IDLEREPEAT:
  case HB_SET_EOL:
  case HB_SET_DEFEXTENSIONS:
  case HB_SET_TRIMFILENAME:
  case HB_SET_HBOUTLOG:
  case HB_SET_HBOUTLOGINFO:
  case HB_SET_OSCODEPAGE:
  case HB_SET_DBCODEPAGE:
  case HB_SET_INVALID_:
    break;
#if 0
      // intentionally removed default: clause to enable C compiler warning
      // when not all HB_SET_* cases are implemented. [druzus]
      default:
         break;
#endif
  }

  hb_errRT_BASE(EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, 0);
  return 0;
}

long hb_setGetNL(HB_set_enum set_specifier)
{
  return hb_setGetNI(set_specifier);
}

HB_PATHNAMES *hb_setGetFirstSetPath(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->hb_set_path;
}

PHB_FILE hb_setGetAltHan(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->hb_set_althan;
}

HB_BOOL hb_setGetCentury(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->hb_set_century;
}

PHB_FILE hb_setGetExtraHan(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->hb_set_extrahan;
}

PHB_FILE hb_setGetPrintHan(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->hb_set_printhan;
}

HB_BOOL hb_setGetAlternate(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_ALTERNATE;
}

const char *hb_setGetAltFile(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_ALTFILE;
}

HB_BOOL hb_setGetAutOpen(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_AUTOPEN;
}

int hb_setGetAutOrder(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_AUTORDER;
}

int hb_setGetAutoShare(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_AUTOSHARE;
}

HB_BOOL hb_setGetBell(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_BELL;
}

HB_BOOL hb_setGetCancel(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_CANCEL;
}

char *hb_setGetColor(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_COLOR;
}

HB_BOOL hb_setGetConfirm(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_CONFIRM;
}

HB_BOOL hb_setGetConsole(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_CONSOLE;
}

const char *hb_setGetDateFormat(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_DATEFORMAT;
}

const char *hb_setGetTimeFormat(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_TIMEFORMAT;
}

HB_BOOL hb_setGetDebug(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_DEBUG;
}

int hb_setGetDecimals(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_DECIMALS;
}

const char *hb_setGetDefault(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_DEFAULT;
}

HB_BOOL hb_setGetDeleted(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_DELETED;
}

const char *hb_setGetDelimChars(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_DELIMCHARS;
}

HB_BOOL hb_setGetDelimiters(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_DELIMITERS;
}

const char *hb_setGetDevice(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_DEVICE;
}

HB_BOOL hb_setGetEOF(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_EOF;
}

int hb_setGetEpoch(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_EPOCH;
}

HB_BOOL hb_setGetEscape(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_ESCAPE;
}

int hb_setGetEventMask(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_EVENTMASK;
}

HB_BOOL hb_setGetExact(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_EXACT;
}

HB_BOOL hb_setGetExclusive(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_EXCLUSIVE;
}

HB_BOOL hb_setGetExit(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_EXIT;
}

HB_BOOL hb_setGetExtra(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_EXTRA;
}

const char *hb_setGetExtraFile(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_EXTRAFILE;
}

HB_BOOL hb_setGetFixed(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_FIXED;
}

HB_BOOL hb_setGetIdleRepeat(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_IDLEREPEAT;
}

HB_BOOL hb_setGetInsert(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_INSERT;
}

HB_BOOL hb_setGetIntensity(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_INTENSITY;
}

const char *hb_setGetPath(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_PATH;
}

int hb_setGetMargin(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_MARGIN;
}

int hb_setGetMBlockSize(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_MBLOCKSIZE;
}

HB_BOOL hb_setGetMCenter(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_MCENTER;
}

int hb_setGetMessage(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_MESSAGE;
}

const char *hb_setGetMFileExt(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_MFILEEXT;
}

HB_BOOL hb_setGetOptimize(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_OPTIMIZE;
}

HB_BOOL hb_setGetPrinter(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_PRINTER;
}

const char *hb_setGetPrintFile(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_PRINTFILE;
}

HB_BOOL hb_setGetScoreBoard(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_SCOREBOARD;
}

HB_BOOL hb_setGetScrollBreak(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_SCROLLBREAK;
}

HB_BOOL hb_setGetSoftSeek(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_SOFTSEEK;
}

HB_BOOL hb_setGetStrictRead(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_STRICTREAD;
}

int hb_setGetTypeAhead(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_TYPEAHEAD;
}

HB_BOOL hb_setGetUnique(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_UNIQUE;
}

int hb_setGetFileCase(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_FILECASE;
}

void hb_setSetFileCase(int iFileCase)
{
  HB_STACK_TLS_PRELOAD
  hb_stackSetStruct()->HB_SET_FILECASE = iFileCase;
}

int hb_setGetDirCase(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_DIRCASE;
}

void hb_setSetDirCase(int iDirCase)
{
  HB_STACK_TLS_PRELOAD
  hb_stackSetStruct()->HB_SET_DIRCASE = iDirCase;
}

int hb_setGetDirSeparator(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_DIRSEPARATOR;
}

void hb_setSetDirSeparator(int iSeparator)
{
  HB_STACK_TLS_PRELOAD
  hb_stackSetStruct()->HB_SET_DIRSEPARATOR = iSeparator;
}

HB_BOOL hb_setGetTrimFileName(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_TRIMFILENAME;
}

void hb_setSetTrimFileName(HB_BOOL fTrim)
{
  HB_STACK_TLS_PRELOAD
  hb_stackSetStruct()->HB_SET_TRIMFILENAME = fTrim;
}

int hb_setGetVideoMode(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_VIDEOMODE;
}

HB_BOOL hb_setGetWrap(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_WRAP;
}

int hb_setGetDBFLockScheme(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_DBFLOCKSCHEME;
}

HB_BOOL hb_setGetHardCommit(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_HARDCOMMIT;
}

HB_BOOL hb_setGetForceOpt(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_FORCEOPT;
}

HB_BOOL hb_setGetDefExtension(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_DEFEXTENSIONS;
}

const char *hb_setGetEOL(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_EOL;
}

const char *hb_setGetHBOUTLOG(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_HBOUTLOG;
}

const char *hb_setGetHBOUTLOGINFO(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->HB_SET_HBOUTLOGINFO;
}

const char *hb_setGetOSCODEPAGE(void)
{
  HB_STACK_TLS_PRELOAD
  PHB_SET_STRUCT pSet = hb_stackSetStruct();

  return pSet->hb_set_oscp ? (static_cast<PHB_CODEPAGE>(pSet->hb_set_oscp))->id : nullptr;
}

void *hb_setGetOSCP(void)
{
  HB_STACK_TLS_PRELOAD
  return hb_stackSetStruct()->hb_set_oscp;
}

const char *hb_setGetDBCODEPAGE(void)
{
  HB_STACK_TLS_PRELOAD
  PHB_SET_STRUCT pSet = hb_stackSetStruct();

  return pSet->hb_set_dbcp ? (static_cast<PHB_CODEPAGE>(pSet->hb_set_dbcp))->id : nullptr;
}

HB_BOOL hb_osUseCP(void)
{
  HB_STACK_TLS_PRELOAD

#if defined(HB_MT_VM)
  if (hb_stackId())
#endif
  {
    auto cdpOS = static_cast<PHB_CODEPAGE>(hb_stackSetStruct()->hb_set_oscp);
    if (cdpOS)
    {
      auto cdpHost = hb_vmCDP();
      return cdpHost && cdpHost != cdpOS;
    }
  }

  return false;
}

const char *hb_osEncodeCP(const char *szName, char **pszFree, HB_SIZE *pnSize)
{
  if (hb_vmIsReady())
  {
    HB_STACK_TLS_PRELOAD
    auto cdpOS = static_cast<PHB_CODEPAGE>(hb_stackSetStruct()->hb_set_oscp);
    if (cdpOS)
    {
      auto cdpHost = hb_vmCDP();
      if (cdpHost && cdpHost != cdpOS)
      {
        HB_SIZE nSize = 0;

        if (pszFree == nullptr)
        {
          pszFree = static_cast<char **>(HB_UNCONST(&szName));
          nSize = strlen(szName);
        }
        char *pszBuf = *pszFree;
        if (pnSize == nullptr)
        {
          pnSize = &nSize;
        }
        else if (*pnSize > 0)
        {
          nSize = *pnSize - 1;
        }

        szName = hb_cdpnDup3(szName, strlen(szName), pszBuf, &nSize, pszFree, pnSize, cdpHost, cdpOS);
      }
    }
  }

  return szName;
}

const char *hb_osDecodeCP(const char *szName, char **pszFree, HB_SIZE *pnSize)
{
  if (hb_vmIsReady())
  {
    HB_STACK_TLS_PRELOAD
    auto cdpOS = static_cast<PHB_CODEPAGE>(hb_stackSetStruct()->hb_set_oscp);
    if (cdpOS)
    {
      auto cdpHost = hb_vmCDP();
      if (cdpHost && cdpHost != cdpOS)
      {
        HB_SIZE nSize = 0;

        if (pszFree == nullptr)
        {
          pszFree = static_cast<char **>(HB_UNCONST(&szName));
          nSize = strlen(szName);
        }
        char *pszBuf = *pszFree;
        if (pnSize == nullptr)
        {
          pnSize = &nSize;
        }
        else if (*pnSize > 0)
        {
          nSize = *pnSize - 1;
        }

        szName = hb_cdpnDup3(szName, strlen(szName), pszBuf, &nSize, pszFree, pnSize, cdpOS, cdpHost);
      }
    }
  }

  return szName;
}

char *hb_osStrEncode(const char *pszName)
{
  if (hb_vmIsReady())
  {
    HB_STACK_TLS_PRELOAD
    auto cdpOS = static_cast<PHB_CODEPAGE>(hb_stackSetStruct()->hb_set_oscp);
    if (cdpOS)
    {
      auto cdpHost = hb_vmCDP();
      if (cdpHost && cdpHost != cdpOS)
      {
        return hb_cdpDup(pszName, cdpHost, cdpOS);
      }
    }
  }

  return hb_strdup(pszName);
}

char *hb_osStrEncodeN(const char *pszName, HB_SIZE nLen)
{
  if (hb_vmIsReady())
  {
    HB_STACK_TLS_PRELOAD
    auto cdpOS = static_cast<PHB_CODEPAGE>(hb_stackSetStruct()->hb_set_oscp);
    if (cdpOS)
    {
      auto cdpHost = hb_vmCDP();
      if (cdpHost && cdpHost != cdpOS)
      {
        return hb_cdpDupn(pszName, nLen, cdpHost, cdpOS);
      }
    }
  }

  return hb_strndup(pszName, nLen);
}

char *hb_osStrEncode2(const char *pszName, char *pszBuffer, HB_SIZE nSize)
{
  if (hb_vmIsReady())
  {
    HB_STACK_TLS_PRELOAD
    auto cdpOS = static_cast<PHB_CODEPAGE>(hb_stackSetStruct()->hb_set_oscp);
    if (cdpOS)
    {
      auto cdpHost = hb_vmCDP();
      if (cdpHost && cdpHost != cdpOS)
      {
        pszBuffer[nSize] = 0;
        hb_cdpnDup2(pszName, strlen(pszName), pszBuffer, &nSize, cdpHost, cdpOS);
        return pszBuffer;
      }
    }
  }

  return hb_strncpy(pszBuffer, pszName, nSize);
}

char *hb_osStrDecode(const char *pszName)
{
  if (hb_vmIsReady())
  {
    HB_STACK_TLS_PRELOAD
    auto cdpOS = static_cast<PHB_CODEPAGE>(hb_stackSetStruct()->hb_set_oscp);
    if (cdpOS)
    {
      auto cdpHost = hb_vmCDP();
      if (cdpHost && cdpHost != cdpOS)
      {
        return hb_cdpDup(pszName, cdpOS, cdpHost);
      }
    }
  }

  return hb_strdup(pszName);
}

char *hb_osStrDecode2(const char *pszName, char *pszBuffer, HB_SIZE nSize)
{
  if (hb_vmIsReady())
  {
    HB_STACK_TLS_PRELOAD
    auto cdpOS = static_cast<PHB_CODEPAGE>(hb_stackSetStruct()->hb_set_oscp);
    if (cdpOS)
    {
      auto cdpHost = hb_vmCDP();
      if (cdpHost && cdpHost != cdpOS)
      {
        pszBuffer[nSize] = 0;
        hb_cdpnDup2(pszName, strlen(pszName), pszBuffer, &nSize, cdpOS, cdpHost);
        return pszBuffer;
      }
    }
  }

  return hb_strncpy(pszBuffer, pszName, nSize);
}

#if defined(HB_OS_WIN)
HB_WCHAR *hb_osStrU16Encode(const char *pszName)
{
  if (hb_vmIsReady())
  {
    auto cdp = hb_vmCDP();
    if (cdp)
    {
      HB_SIZE nLen = strlen(pszName);
      HB_SIZE nSize = hb_cdpStrAsU16Len(cdp, pszName, nLen, 0);
      auto pszBufferW = static_cast<HB_WCHAR *>(hb_xgrab((nSize + 1) * sizeof(HB_WCHAR)));
      hb_cdpStrToU16(cdp, HB_CDP_ENDIAN_NATIVE, pszName, nLen, pszBufferW, nSize + 1);
      return pszBufferW;
    }
  }

  return hb_mbtowc(pszName); // No HVM stack
}

HB_WCHAR *hb_osStrU16EncodeN(const char *pszName, HB_SIZE nLen)
{
  if (hb_vmIsReady())
  {
    auto cdp = hb_vmCDP();
    if (cdp)
    {
      nLen = hb_strnlen(pszName, nLen);
      HB_SIZE nSize = hb_cdpStrAsU16Len(cdp, pszName, nLen, 0);
      auto pszBufferW = static_cast<HB_WCHAR *>(hb_xgrab((nSize + 1) * sizeof(HB_WCHAR)));
      hb_cdpStrToU16(cdp, HB_CDP_ENDIAN_NATIVE, pszName, nLen, pszBufferW, nSize + 1);
      return pszBufferW;
    }
  }

  return hb_mbntowc(pszName, nLen); // No HVM stack
}

HB_WCHAR *hb_osStrU16Encode2(const char *pszName, HB_WCHAR *pszBufferW, HB_SIZE nSize)
{
  if (hb_vmIsReady())
  {
    auto cdp = hb_vmCDP();
    if (cdp)
    {
      hb_cdpStrToU16(cdp, HB_CDP_ENDIAN_NATIVE, pszName, strlen(pszName), pszBufferW, nSize + 1);
      pszBufferW[nSize] = 0;
      return pszBufferW;
    }
  }
  hb_mbntowccpy(pszBufferW, pszName, nSize); // No HVM stack
  return pszBufferW;
}

char *hb_osStrU16Decode(const HB_WCHAR *pszNameW)
{
  if (hb_vmIsReady())
  {
    auto cdp = hb_vmCDP();
    if (cdp)
    {
      HB_SIZE nLen = hb_wstrlen(pszNameW);
      HB_SIZE nSize = hb_cdpU16AsStrLen(cdp, pszNameW, nLen, 0);
      auto pszBuffer = static_cast<char *>(hb_xgrab(nSize + 1));
      hb_cdpU16ToStr(cdp, HB_CDP_ENDIAN_NATIVE, pszNameW, nLen, pszBuffer, nSize + 1);
      return pszBuffer;
    }
  }

  return hb_wctomb(pszNameW); // No HVM stack
}

char *hb_osStrU16Decode2(const HB_WCHAR *pszNameW, char *pszBuffer, HB_SIZE nSize)
{
  if (hb_vmIsReady())
  {
    auto cdp = hb_vmCDP();
    if (cdp)
    {
      hb_cdpU16ToStr(cdp, HB_CDP_ENDIAN_NATIVE, pszNameW, hb_wstrlen(pszNameW), pszBuffer, nSize);
      pszBuffer[nSize] = 0;
      return pszBuffer;
    }
  }

  hb_wcntombcpy(pszBuffer, pszNameW, nSize); // No HVM stack
  return pszBuffer;
}
#endif

PHB_FILE hb_setGetPrinterHandle(int iType)
{
  HB_STACK_TLS_PRELOAD
  PHB_SET_STRUCT pSet = hb_stackSetStruct();

  switch (iType)
  {
  case HB_SET_PRN_DEV:
    if (!pSet->hb_set_prndevice)
    {
      return nullptr;
    }
    break;
  case HB_SET_PRN_CON:
    if (!pSet->HB_SET_PRINTER)
    {
      return nullptr;
    }
    break;
  case HB_SET_PRN_ANY:
    break;
  default:
    return nullptr;
  }

  if (pSet->hb_set_printhan == nullptr && pSet->HB_SET_PRINTFILE)
  {
    open_handle(pSet, pSet->HB_SET_PRINTFILE, false, HB_SET_PRINTFILE);
  }

  return pSet->hb_set_printhan;
}
