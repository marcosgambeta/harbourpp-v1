//
// NetName() function
//
// Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
// Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net> (Support for DJGPP/GCC/OS2 for NetName())
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
#include "hbwinuni.hpp"

#elif defined(HB_OS_UNIX)

#if defined(HB_OS_VXWORKS)
#include <hostLib.h>
#endif
#include <unistd.h>

#endif

#if !defined(MAXGETHOSTNAME) && (defined(HB_OS_UNIX))
#define MAXGETHOSTNAME 256 // should be enough for a host name
#endif

// NOTE: Clipper will only return a maximum of 15 bytes from this function.
//       And it will be padded with spaces. Harbour does the same on the
//       MS-DOS platform.
//       [vszakats]

// NOTE: The caller must free the returned buffer. [vszakats]

char *hb_netname(void)
{
#if defined(HB_OS_WIN)

  DWORD dwLen = MAX_COMPUTERNAME_LENGTH + 1;
  TCHAR lpValue[MAX_COMPUTERNAME_LENGTH + 1];

  lpValue[0] = TEXT('\0');
  GetComputerName(lpValue, &dwLen);
  lpValue[MAX_COMPUTERNAME_LENGTH] = TEXT('\0');

  if (lpValue[0])
  {
    return HB_OSSTRDUP(lpValue);
  }

#elif (defined(HB_OS_UNIX))

  char szValue[MAXGETHOSTNAME + 1];
  szValue[0] = szValue[MAXGETHOSTNAME] = '\0';
  gethostname(szValue, MAXGETHOSTNAME);
  if (szValue[0])
  {
    return hb_osStrDecode(szValue);
  }

#endif

  return hb_getenv("HOSTNAME");
}

HB_FUNC(NETNAME)
{
  char *buffer = hb_netname();

  if (buffer)
  {
    hb_retc_buffer(buffer);
  }
  else
  {
    hb_retc_null();
  }
}
