/*
 * hb_DiskSpace() function
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

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapifs.h"

#if defined( HB_OS_DARWIN )
   #include <sys/param.h>
   #include <sys/mount.h>
#elif defined( HB_OS_ANDROID )
   #include <sys/statfs.h>
#elif defined( HB_OS_UNIX ) && !( defined( __CEGCC__ ) )
   #if defined( HB_OS_VXWORKS )
      #include <sys/stat.h>
   #else
      #include <sys/statvfs.h>
   #endif
#elif defined( HB_OS_WIN )
   #include <windows.h>
   #include "hbwinuni.h"
#endif

double hb_fsDiskSpace( const char * pszPath, HB_USHORT uiType )
{
   char szPathBuf[ 2 ];
   double dSpace = 0.0;

   if( uiType > HB_DISK_TOTAL )
   {
      uiType = HB_DISK_AVAIL;
   }

   if( ! pszPath )
   {
      szPathBuf[ 0 ] = HB_OS_PATH_DELIM_CHR;
      szPathBuf[ 1 ] = '\0';
      pszPath = szPathBuf;
   }

#if defined( HB_OS_WIN )
   {
      LPCTSTR lpPath;
      LPTSTR lpFree;

      lpPath = HB_FSNAMECONV( pszPath, &lpFree );

      {
         UINT uiErrMode = SetErrorMode( SEM_FAILCRITICALERRORS );
         HB_BOOL fResult;

#if ! defined( HB_OS_WIN_64 )
         /* NOTE: We need to call this function dynamically to maintain support
                  Win95 first edition. It was introduced in Win95B (aka OSR2) [vszakats] */
         typedef BOOL ( WINAPI * P_GDFSE )( LPCTSTR, PULARGE_INTEGER, PULARGE_INTEGER, PULARGE_INTEGER );
         static P_GDFSE s_pGetDiskFreeSpaceEx = nullptr;
         static HB_BOOL s_fInit = HB_FALSE;

         if( ! s_fInit )
         {
            HMODULE hModule = GetModuleHandle( HB_WINAPI_KERNEL32_DLL() );
            if( hModule )
            {
               s_pGetDiskFreeSpaceEx = reinterpret_cast< P_GDFSE >( HB_WINAPI_GETPROCADDRESST( hModule, "GetDiskFreeSpaceEx" ) );
            }
            s_fInit = HB_TRUE;
         }

         if( ! s_pGetDiskFreeSpaceEx )
         {
            DWORD dwSectorsPerCluster;
            DWORD dwBytesPerSector;
            DWORD dwNumberOfFreeClusters;
            DWORD dwTotalNumberOfClusters;

            fResult = GetDiskFreeSpace( lpPath,
                                        &dwSectorsPerCluster,
                                        &dwBytesPerSector,
                                        &dwNumberOfFreeClusters,
                                        &dwTotalNumberOfClusters ) ? HB_TRUE : HB_FALSE;
            hb_fsSetIOError( fResult, 0 );

            if( fResult )
            {
               switch( uiType )
               {
                  case HB_DISK_AVAIL:
                  case HB_DISK_FREE:
                     dSpace = static_cast< double >( dwNumberOfFreeClusters ) * static_cast< double >( dwSectorsPerCluster ) * static_cast< double >( dwBytesPerSector );
                     break;

                  case HB_DISK_USED:
                  case HB_DISK_TOTAL:
                     dSpace = static_cast< double >( dwTotalNumberOfClusters ) * static_cast< double >( dwSectorsPerCluster ) * static_cast< double >( dwBytesPerSector );

                     if( uiType == HB_DISK_USED )
                     {
                        dSpace -= static_cast< double >( dwNumberOfFreeClusters ) * static_cast< double >( dwSectorsPerCluster ) * static_cast< double >( dwBytesPerSector );
                     }
                     break;
               }
            }
         }
         else
#endif
         {
#if defined( _MSC_VER ) || ( defined( __GNUC__ ) )

#  define HB_GET_LARGE_UINT( v )  ( static_cast< double >( (v).LowPart ) + static_cast< double >( (v).HighPart ) * ( ( static_cast< double >( 0xFFFFFFFF ) ) + 1 ) )

#else
   /* NOTE: Borland doesn't seem to deal with the un-named
            struct that is part of ULARGE_INTEGER
            [pt] */
#  define HB_GET_LARGE_UINT( v )  ( static_cast< double >( (v).u.LowPart ) + static_cast< double >( (v).u.HighPart ) * ( ( static_cast< double >( 0xFFFFFFFF ) ) + 1 ) )
#endif

            ULARGE_INTEGER i64FreeBytesToCaller, i64TotalBytes, i64FreeBytes;

#if ! defined( HB_OS_WIN_64 )
            fResult = s_pGetDiskFreeSpaceEx( lpPath,
                                             static_cast< PULARGE_INTEGER >( &i64FreeBytesToCaller ),
                                             static_cast< PULARGE_INTEGER >( &i64TotalBytes ),
                                             static_cast< PULARGE_INTEGER >( &i64FreeBytes ) );
#else
            fResult = GetDiskFreeSpaceEx( lpPath,
                                          static_cast< PULARGE_INTEGER >( &i64FreeBytesToCaller ),
                                          static_cast< PULARGE_INTEGER >( &i64TotalBytes ),
                                          static_cast< PULARGE_INTEGER >( &i64FreeBytes ) );
#endif
            hb_fsSetIOError( fResult, 0 );

            if( fResult )
            {
               switch( uiType )
               {
                  case HB_DISK_AVAIL:
                     dSpace = HB_GET_LARGE_UINT( i64FreeBytesToCaller );
                     break;

                  case HB_DISK_FREE:
                     dSpace = HB_GET_LARGE_UINT( i64FreeBytes );
                     break;

                  case HB_DISK_TOTAL:
                     dSpace = HB_GET_LARGE_UINT( i64TotalBytes );
                     break;

                  case HB_DISK_USED:
                     dSpace = HB_GET_LARGE_UINT( i64TotalBytes ) - HB_GET_LARGE_UINT( i64FreeBytes );
                     break;
               }
            }
         }
         SetErrorMode( uiErrMode );
      }
      if( lpFree )
      {
         hb_xfree( lpFree );
      }
   }
#elif defined( HB_OS_UNIX ) && !( defined( __CEGCC__ ) )
   {
#if defined( HB_OS_DARWIN ) || defined( HB_OS_ANDROID ) || defined( HB_OS_VXWORKS )
      struct statfs sf;
#else
      struct statvfs sf;
#endif
      char * pszFree;

      pszPath = hb_fsNameConv( pszPath, &pszFree );

#if defined( HB_OS_DARWIN ) || defined( HB_OS_ANDROID ) || defined( HB_OS_VXWORKS )
      if( statfs( pszPath, &sf ) == 0 )
#else
      if( statvfs( pszPath, &sf ) == 0 )
#endif
      {
         switch( uiType )
         {
            case HB_DISK_AVAIL:
               dSpace = static_cast< double >( sf.f_bavail ) * static_cast< double >( sf.f_bsize );
               break;

            case HB_DISK_FREE:
               dSpace = static_cast< double >( sf.f_bfree ) * static_cast< double >( sf.f_bsize );
               break;

            case HB_DISK_USED:
               dSpace = static_cast< double >( sf.f_blocks - sf.f_bfree ) * static_cast< double >( sf.f_bsize );
               break;

            case HB_DISK_TOTAL:
               dSpace = static_cast< double >( sf.f_blocks ) * static_cast< double >( sf.f_bsize );
               break;
         }
         hb_fsSetIOError( HB_TRUE, 0 );
      }
      else
      {
         hb_fsSetIOError( HB_FALSE, 0 );
      }

      if( pszFree )
      {
         hb_xfree( pszFree );
      }
   }
#else
   {
      int iTODO;

      HB_SYMBOL_UNUSED( uiType );
   }
#endif

   return dSpace;
}

HB_FUNC( HB_DISKSPACE )
{
   const char * pszPath = hb_parc(1);
   HB_USHORT uiType = static_cast< HB_USHORT >( hb_parnidef( 2, HB_DISK_AVAIL ) );

#ifdef HB_OS_HAS_DRIVE_LETTER
   char szPathBuf[ 4 ];

   if( ! pszPath )
   {
      int iDrive = hb_parni(1);

      if( iDrive >= 1 && iDrive < 32 )
      {
         szPathBuf[ 0 ] = static_cast< char >( iDrive ) + 'A' - 1;
         szPathBuf[ 1 ] = HB_OS_DRIVE_DELIM_CHR;
         szPathBuf[ 2 ] = HB_OS_PATH_DELIM_CHR;
         szPathBuf[ 3 ] = '\0';
         pszPath = szPathBuf;
      }
   }
#endif

   hb_retnlen( hb_fsDiskSpace( pszPath, uiType ), -1, 0 );
}
