//
// Windows Harbour DLL entry point
//
// Copyright 2001 Antonio Linares <alinares@fivetech.com>
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

#include "hbvm.hpp"
#include "hbapiitm.hpp"

#if defined(HB_OS_WIN)
#include <windows.h>
#endif

#if defined(HB_OS_WIN)

BOOL WINAPI HB_DLL_ENTRY_POINT(HINSTANCE hInstance, DWORD fdwReason, PVOID pvReserved)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("DllEntryPoint(%p, %lu, %p)", hInstance, fdwReason, pvReserved));
#endif

  HB_SYMBOL_UNUSED(hInstance);
  HB_SYMBOL_UNUSED(fdwReason);
  HB_SYMBOL_UNUSED(pvReserved);

  switch (fdwReason)
  {
  case DLL_PROCESS_ATTACH:
    break;

  case DLL_PROCESS_DETACH:
    break;
  }

  return TRUE;
}

#endif
