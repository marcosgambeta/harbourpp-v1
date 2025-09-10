//
// Misc Windows API functions
//
// Copyright 2008-2009 Viktor Szakats
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

#include "hbwapi.hpp"

HB_FUNC(WIN_RUNDETACHED)
{
  void *hCommandName;
  void *hCommandLine;

  HB_SIZE nLen;
  LPCTSTR lpCommandRO = HB_PARSTR(2, &hCommandLine, &nLen);

  STARTUPINFO si{};
  PROCESS_INFORMATION pi{};

  si.cb = sizeof(si);

  if (CreateProcess(HB_PARSTR(1, &hCommandName, nullptr), // Command name
                    HB_STRUNSHARE(&hCommandLine, lpCommandRO,
                                  nLen), // Command-line (Unicode version needs an non-const buffer)
                    nullptr,             // Process handle not inheritable
                    nullptr,             // Thread handle not inheritable
                    FALSE,               // Set handle inheritance to FALSE
                    hb_parl(4) ? CREATE_NO_WINDOW : CREATE_NEW_CONSOLE, // Creation flags
                    nullptr,                                            // Use parent's environment block
                    nullptr,                                            // Use parent's starting directory
                    &si,                                                // Pointer to STARTUPINFO structure
                    &pi)                                                // Pointer to PROCESS_INFORMATION structure
  ) {
    hb_retl(true);

    hb_stornint(pi.dwProcessId, 3);

    // Close process and thread handles.
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
  } else {
    hb_stornint(-1, 3);
    hb_retl(false);
  }

  hb_strfree(hCommandName);
  hb_strfree(hCommandLine);
}
