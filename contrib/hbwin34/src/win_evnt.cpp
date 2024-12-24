/*
 * Windows functions (event handling)
 *
 * Copyright 2010 Viktor Szakats
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

#include "hbwapi.hpp"
#include <hbapierr.hpp>

HB_FUNC(WIN_REPORTEVENT)
{
  bool bRetVal = false;

  HANDLE hEventLog;

  void *hServerName;
  void *hSourceName;

  hEventLog = RegisterEventSource(HB_PARSTR(1, &hServerName, nullptr), HB_PARSTRDEF(2, &hSourceName, nullptr));

  hb_strfree(hServerName);
  hb_strfree(hSourceName);

  if (hEventLog != nullptr && hEventLog != reinterpret_cast<HANDLE>(ERROR_ACCESS_DENIED))
  {
    WORD wNumStrings = 0;
    LPCTSTR *lpStrings = nullptr;
    void **hStrings = nullptr;

    auto pStrings = hb_param(6, Harbour::Item::ARRAY);

    if (pStrings && (wNumStrings = static_cast<WORD>(hb_arrayLen(pStrings))) > 0)
    {
      lpStrings = static_cast<LPCTSTR *>(hb_xgrab(sizeof(LPCTSTR) * wNumStrings));
      hStrings = static_cast<void **>(hb_xgrab(sizeof(void *) * wNumStrings));

      for (WORD i = 0; i < wNumStrings; ++i)
      {
        lpStrings[i] = static_cast<LPCTSTR>(HB_ARRAYGETSTR(pStrings, i + 1, &hStrings[i], nullptr));
      }
    }
    else if (HB_ISCHAR(6))
    {
      wNumStrings = 1;

      lpStrings = static_cast<LPCTSTR *>(hb_xgrab(sizeof(LPCTSTR)));
      hStrings = static_cast<void **>(hb_xgrab(sizeof(void *)));

      lpStrings[0] = static_cast<LPCTSTR>(HB_ITEMGETSTR(hb_param(6, Harbour::Item::STRING), &hStrings[0], nullptr));
    }

    if (ReportEvent(hEventLog, static_cast<WORD>(hb_parni(3)) /* wType */,
                    static_cast<WORD>(hb_parni(4)) /* wCategory */, static_cast<DWORD>(hb_parnl(5)) /* dwEventID */,
                    nullptr /* lpUserSid */, wNumStrings, static_cast<DWORD>(hb_parclen(7)), lpStrings,
                    static_cast<LPVOID>(const_cast<char *>(hb_parc(7)))))
    {
      bRetVal = true;
    }

    if (lpStrings)
    {
      while (wNumStrings)
      {
        hb_strfree(hStrings[--wNumStrings]);
      }

      hb_xfree(hStrings);
      hb_xfree(static_cast<void *>(lpStrings));
    }

    DeregisterEventSource(hEventLog);
  }

  hb_retl(bRetVal);
}
