/*
 * DiskSpace() function
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* NOTE: DiskSpace() supports larger disks than 2 GiB. CA-Cl*pper will always
         return a (long) value, Harbour may return a (double) for large
         values, the decimal places are always set to zero, though. */

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapifs.h"

#if defined(HB_OS_UNIX)
#  include <unistd.h>
#  include <sys/types.h>
#  if defined(__CEGCC__) || defined(HB_OS_VXWORKS)
#     include <sys/stat.h>
#  elif defined(HB_OS_ANDROID)
#     include <sys/statfs.h>
#  elif defined(HB_OS_DARWIN)
#     include <sys/param.h>
#     include <sys/mount.h>
#  else
#     include <sys/statvfs.h>
#  endif
#elif defined(HB_OS_WIN)
#  include <windows.h>
#  include "hbwinuni.h"
#endif

HB_FUNC( DISKSPACE )
{
   double dSpace = 0.0;
   bool bError = false;

#if defined(HB_OS_WIN)
   {
#if defined(_MSC_VER) || (defined(__GNUC__))

#  define HB_GET_LARGE_UINT(v)  (static_cast<double>((v).LowPart) + static_cast<double>((v).HighPart) * ((static_cast<double>(0xFFFFFFFF)) + 1))

#else
   /* NOTE: For compilers that don't seem to deal with the
            unnamed struct that is part of ULARGE_INTEGER [pt] */
#  define HB_GET_LARGE_UINT(v)  (static_cast<double>((v).u.LowPart) + static_cast<double>((v).u.HighPart) * ((static_cast<double>(0xFFFFFFFF)) + 1))
#endif

      int iDrive = hb_parni(1);

      if( iDrive >= 0 )
      {
         ULARGE_INTEGER i64FreeBytesToCaller, i64TotalBytes, i64FreeBytes;
         UINT uiErrMode = SetErrorMode( SEM_FAILCRITICALERRORS );

         TCHAR lpPath[4];

         if( iDrive == 0 )
         {
            iDrive = hb_fsCurDrv() + 1;
         }

         lpPath[0] = static_cast<TCHAR>(iDrive + 'A' - 1);
         lpPath[1] = TEXT(':');
         lpPath[2] = TEXT('\\');
         lpPath[3] = TEXT('\0');

         bError = GetDiskFreeSpaceEx(lpPath,
                                     static_cast<PULARGE_INTEGER>(&i64FreeBytesToCaller),
                                     static_cast<PULARGE_INTEGER>(&i64TotalBytes),
                                     static_cast<PULARGE_INTEGER>(&i64FreeBytes)) ? false : true;
         if( !bError )
         {
            dSpace = HB_GET_LARGE_UINT(i64FreeBytesToCaller);
         }

         SetErrorMode( uiErrMode );
      }
      else
      {
         bError = true;
      }
   }
#elif defined(HB_OS_UNIX)
   {
      const char * szName = hb_parc(1);
      char * pszFree = nullptr;

      if( !szName )
      {
         szName = "/";
      }
      else
      {
         szName = hb_fsNameConv(szName, &pszFree);
      }

      {
#if defined(__CEGCC__)
         int iTODO;

         bError = false;
#else
#if defined(HB_OS_DARWIN) || defined(HB_OS_ANDROID) || defined(HB_OS_VXWORKS)
         struct statfs st;
         bError = statfs(szName, &st) != 0;
#else
         struct statvfs st;
         bError = statvfs(szName, &st) != 0;
#endif
         if( !bError )
         {
#if !defined(HB_OS_VXWORKS)
            if( getuid() == 0 )
            {
               dSpace = static_cast<double>(st.f_bfree) * static_cast<double>(st.f_bsize);
            }
            else
#endif
               dSpace = static_cast<double>(st.f_bavail) * static_cast<double>(st.f_bsize);
         }
#endif
      }

      if( pszFree )
      {
         hb_xfree(pszFree);
      }
   }
#else
   bError = false;
#endif

   if( bError )
   {
      hb_errRT_BASE_Ext1(EG_OPEN, 2018, nullptr, nullptr, 0, EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS);
   }

   hb_retnlen(dSpace, -1, 0);
}
