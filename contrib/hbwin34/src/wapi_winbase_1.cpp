/*
 * Windows API functions (winbase)
 *
 * Copyright 2009 Viktor Szakats
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

#include "hbwapi.hpp"
#include "hbapierr.hpp"
#include "hbapiitm.hpp"

HB_FUNC( WAPI_GETCOMMANDLINE )
{
   HB_RETSTR(GetCommandLine());
}

HB_FUNC( WAPI_GETCURRENTPROCESS )
{
   hbwapi_ret_raw_HANDLE(GetCurrentProcess());
}

HB_FUNC( WAPI_GETCURRENTTHREAD )
{
   hbwapi_ret_raw_HANDLE(GetCurrentThread());
}

HB_FUNC( WAPI_WAITFORSINGLEOBJECT )
{
   DWORD dwResult = WaitForSingleObject(hbwapi_par_raw_HANDLE(1), static_cast<DWORD>(hb_parnl(2)));
   hbwapi_SetLastError(GetLastError());
   hb_retnint(dwResult);
}

HB_FUNC( WAPI_WAITFORSINGLEOBJECTEX )
{
   DWORD dwResult = WaitForSingleObjectEx(hbwapi_par_raw_HANDLE(1), static_cast<DWORD>(hb_parnl(2)), hb_parl(3));
   DWORD dwLastError = GetLastError();
   hbwapi_SetLastError(dwLastError);
   hb_retnint(dwResult);
}

HB_FUNC( WAPI_WAITFORMULTIPLEOBJECTS )
{
   auto pArray = hb_param(2, Harbour::Item::ARRAY);
   DWORD nCount = pArray ? static_cast<DWORD>(hb_arrayLen(pArray)) : 0;

   if( nCount > 0 && nCount <= MAXIMUM_WAIT_OBJECTS )
   {
      auto handles = static_cast<HANDLE*>(hb_xgrab(nCount * sizeof(HANDLE)));

      for( DWORD nPos = 0; nPos < nCount; ++nPos )
      {
         handles[nPos] = hb_arrayGetPtr(pArray, nPos + 1);
      }

      DWORD dwResult = WaitForMultipleObjects(nCount, handles, hb_parl(3), static_cast<DWORD>(hb_parnl(4)));
      hbwapi_SetLastError(GetLastError());
      hb_retnint(dwResult);
      hb_xfree(handles);
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 1001, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_WAITFORMULTIPLEOBJECTSEX )
{
   auto pArray = hb_param(2, Harbour::Item::ARRAY);
   DWORD nCount = pArray ? static_cast<DWORD>(hb_arrayLen(pArray)) : 0;

   if( nCount > 0 && nCount <= MAXIMUM_WAIT_OBJECTS )
   {
      auto handles = static_cast<HANDLE*>(hb_xgrab(nCount * sizeof(HANDLE)));

      for( DWORD nPos = 0; nPos < nCount; ++nPos )
      {
         handles[nPos] = hb_arrayGetPtr(pArray, nPos + 1);
      }

      DWORD dwResult = WaitForMultipleObjectsEx(nCount, handles, hb_parl(3), static_cast<DWORD>(hb_parnl(4)), hb_parl(5));
      hbwapi_SetLastError(GetLastError());
      hb_retnint(dwResult);
      hb_xfree(handles);
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 1001, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/*
WAPI_SETPROCESSWORKINGSETSIZE(HProcess, dwMinimumWorkingSetSize, dwMaximumWorkingSetSize) --> .T.|.F.
*/
HB_FUNC( WAPI_SETPROCESSWORKINGSETSIZE )
{
   BOOL bResult = SetProcessWorkingSetSize(hbwapi_par_raw_HANDLE(1), static_cast<SIZE_T>(hb_parnint(2)), static_cast<SIZE_T>(hb_parnint(3)));
   DWORD dwLastError = GetLastError();
   hbwapi_SetLastError(dwLastError);
   hbwapi_ret_L(bResult);
}

HB_FUNC( WAPI_SETLASTERROR )
{
   DWORD dwLastError = static_cast<DWORD>(hb_parnl(1));
   SetLastError(dwLastError);
   hbwapi_SetLastError(dwLastError);
}

HB_FUNC( WAPI_SETERRORMODE )
{
   hbwapi_ret_UINT(SetErrorMode(hbwapi_par_UINT(1)));
}

HB_FUNC( WAPI_LOADLIBRARYEX )
{
   void * hFileName;
   HMODULE hResult = LoadLibraryEx(HB_PARSTRDEF(1, &hFileName, nullptr), nullptr, hbwapi_par_DWORD(3));
   hbwapi_SetLastError(GetLastError());
   hb_retptr(hResult);
   hb_strfree(hFileName);
}

HB_FUNC( WAPI_LOADLIBRARY )
{
   void * hFileName;
   HMODULE hResult = LoadLibrary(HB_PARSTRDEF(1, &hFileName, nullptr));
   hbwapi_SetLastError(GetLastError());
   hb_retptr(hResult);
   hb_strfree(hFileName);
}

HB_FUNC( WAPI_FREELIBRARY )
{
   BOOL bResult = FreeLibrary(static_cast<HMODULE>(hb_parptr(1)));
   hbwapi_SetLastError(GetLastError());
   hbwapi_ret_L(bResult);
}

HB_FUNC( WAPI_GETPROCADDRESS )
{
   FARPROC pProc = GetProcAddress(static_cast<HMODULE>(hb_parptr(1)), HB_ISCHAR(2) ? hb_parc(2) : reinterpret_cast<LPCSTR>(static_cast<HB_PTRUINT>(hb_parnint(2))));
   DWORD dwLastError = GetLastError();
   hbwapi_SetLastError(dwLastError);
   hb_retptr(reinterpret_cast<void*>(reinterpret_cast<HB_PTRUINT>(pProc)));
}

/* HMODULE WINAPI GetModuleHandle(__in_opt LPCTSTR lpModuleName); */
HB_FUNC( WAPI_GETMODULEHANDLE )
{
   void * hModuleName;
   HMODULE hResult = GetModuleHandle(HB_PARSTR(1, &hModuleName, nullptr));
   hbwapi_SetLastError(GetLastError());
   hbwapi_ret_raw_HANDLE(hResult);
   hb_strfree(hModuleName);
}

HB_FUNC( WAPI_MULDIV )
{
   hb_retni(MulDiv(hb_parni(1), hb_parni(2), hb_parni(3)));
}

typedef DWORD (WINAPI * _HB_GETPATHNAME)(LPCTSTR, LPTSTR, DWORD);

static void s_getPathName(_HB_GETPATHNAME getPathName)
{
   void * hLongPath;
   DWORD length = 0;
   LPCTSTR lpszLongPath = HB_PARSTR(1, &hLongPath, nullptr);

   if( lpszLongPath )
   {
      if( HB_ISBYREF(2) )
      {
         TCHAR buffer[HB_PATH_MAX];
         DWORD cchBuffer = static_cast<DWORD>(HB_SIZEOFARRAY(buffer));
         LPTSTR lpszShortPath = buffer;
         bool fSize = HB_ISNUM(3);

         if( fSize )    /* the size of buffer is limited by user */
         {
            cchBuffer = static_cast<DWORD>(hb_parnl(3));
            if( cchBuffer == 0 )
            {
               lpszShortPath = nullptr;
            }
            else if( cchBuffer > static_cast<DWORD>(HB_SIZEOFARRAY(buffer)) )
            {
               lpszShortPath = static_cast<LPTSTR>(hb_xgrab(cchBuffer * sizeof(TCHAR)));
            }
         }

         length = getPathName(lpszLongPath, lpszShortPath, cchBuffer);
         if( !fSize && length > cchBuffer )  /* default buffer size was too small */
         {
            cchBuffer = length;
            lpszShortPath = static_cast<LPTSTR>(hb_xgrab(cchBuffer * sizeof(TCHAR)));
            length = getPathName(lpszLongPath, lpszShortPath, cchBuffer);
         }
         hbwapi_SetLastError(GetLastError());
         HB_STORSTRLEN(lpszShortPath, length > cchBuffer ? 0 : length, 2);
         if( lpszShortPath && lpszShortPath != buffer )
         {
            hb_xfree(lpszShortPath);
         }
      }
      else if( getPathName )
      {
         length = getPathName(lpszLongPath, nullptr, 0);
         hbwapi_SetLastError(GetLastError());
      }
   }
   hb_retnl(length);
   hb_strfree(hLongPath);
}

HB_FUNC( WAPI_GETSHORTPATHNAME )
{
   s_getPathName(GetShortPathName);
}

HB_FUNC( WAPI_GETLONGPATHNAME )
{
   s_getPathName(GetLongPathName);
}

HB_FUNC( WAPI_GETSYSTEMDIRECTORY )
{
   UINT nLen = GetSystemDirectory(nullptr, 0);

   if( nLen )
   {
      auto buffer = static_cast<LPTSTR>(hb_xgrab((nLen + 1) * sizeof(TCHAR)));
      nLen = GetSystemDirectory(buffer, nLen);
      hbwapi_SetLastError(GetLastError());
      HB_RETSTRLEN(buffer, nLen);
      hb_xfree(buffer);
   }
   else
   {
      hbwapi_SetLastError(GetLastError());
      hb_retc_null();
   }
}

HB_FUNC( WAPI_GETWINDOWSDIRECTORY )
{
   UINT nLen = GetWindowsDirectory(nullptr, 0);

   if( nLen )
   {
      auto buffer = static_cast<LPTSTR>(hb_xgrab((nLen + 1) * sizeof(TCHAR)));
      nLen = GetWindowsDirectory(buffer, nLen);
      hbwapi_SetLastError(GetLastError());
      HB_RETSTRLEN(buffer, nLen);
      hb_xfree(buffer);
   }
   else
   {
      hbwapi_SetLastError(GetLastError());
      hb_retc_null();
   }
}

HB_FUNC( WAPI_QUERYPERFORMANCECOUNTER )
{
   LARGE_INTEGER counter;
   BOOL result = QueryPerformanceCounter(&counter);
   hb_stornint(result ? HBWAPI_GET_LARGEUINT(counter) : 0, 1);
   hb_retl(result != 0);
}

HB_FUNC( WAPI_QUERYPERFORMANCEFREQUENCY )
{
   LARGE_INTEGER frequency;
   BOOL result = QueryPerformanceFrequency(&frequency);
   hb_stornint(result ? HBWAPI_GET_LARGEUINT(frequency) : 0, 1);
   hb_retl(result != 0);
}

#define TARGET_PATH_BUFFER_SIZE     4096

HB_FUNC( WAPI_QUERYDOSDEVICE )
{
   void * hDeviceName;
   auto lpTargetPath = static_cast<LPTSTR>(hb_xgrab(TARGET_PATH_BUFFER_SIZE * sizeof(TCHAR)));

   DWORD dwResult = QueryDosDevice(HB_PARSTR(1, &hDeviceName, nullptr), lpTargetPath, TARGET_PATH_BUFFER_SIZE);
   hbwapi_SetLastError(GetLastError());
   if( dwResult )
   {
      auto pArray = hb_itemArrayNew(0);
      PHB_ITEM pItem = nullptr;
      DWORD dwPos, dwStart;

      dwPos = dwStart = 0;
      while( lpTargetPath[dwPos] )
      {
         if( !lpTargetPath[++dwPos] )
         {
            pItem = HB_ITEMPUTSTRLEN(pItem, lpTargetPath + dwStart, dwPos - dwStart - 1);
            hb_arrayAdd(pArray, pItem);
            dwStart = ++dwPos;
         }
      }
      hb_itemRelease(pItem);
      hb_itemReturnRelease(pArray);
   }
   else
   {
      hb_reta(0);
   }

   hb_strfree(hDeviceName);
   hb_xfree(lpTargetPath);
}

/*
wapi_GetVolumeInformation(<cRootPath>, @<cVolumeName>, @<nSerial>, @<nMaxComponentLength>, @<nFileSystemFlags>, @<cFileSystemName>) --> <lSuccess>
*/

HB_FUNC( WAPI_GETVOLUMEINFORMATION )
{
   DWORD dwSerialNumber = 0;
   DWORD dwMaxFileNameLen = 0;
   DWORD dwFileSystemFlags = 0;
   DWORD dwVolNameSize = 0;
   DWORD dwFSNameSize = 0;
   LPTSTR lpVolNameBuf = nullptr;
   LPTSTR lpFSNameBuf = nullptr;

   if( HB_ISBYREF(2) )
   {
      dwVolNameSize = MAX_PATH + 1;
      lpVolNameBuf = static_cast<LPTSTR>(hb_xgrab(dwVolNameSize * sizeof(TCHAR)));
   }
   if( HB_ISBYREF(6) )
   {
      dwFSNameSize = MAX_PATH + 1;
      lpFSNameBuf = static_cast<LPTSTR>(hb_xgrab(dwFSNameSize * sizeof(TCHAR)));
   }

   void * hRootPath;
   BOOL bResult = GetVolumeInformation(HB_PARSTR(1, &hRootPath, nullptr), lpVolNameBuf, dwVolNameSize, &dwSerialNumber, &dwMaxFileNameLen, &dwFileSystemFlags, lpFSNameBuf, dwFSNameSize);
   hb_strfree(hRootPath);

   if( lpVolNameBuf )
   {
      HB_STORSTR(lpVolNameBuf, 2);
      hb_xfree(lpVolNameBuf);
   }
   hb_stornint(dwSerialNumber, 3);
   hb_stornint(dwMaxFileNameLen, 4);
   hb_stornint(dwFileSystemFlags, 5);
   if( lpFSNameBuf )
   {
      HB_STORSTR(lpFSNameBuf, 6);
      hb_xfree(lpFSNameBuf);
   }

   hb_retl(bResult != 0);
}

HB_FUNC( WAPI_COPYFILE )
{
   void * hSrc;
   void * hDst;
   bool bResult = CopyFile(HB_PARSTRDEF(1, &hSrc, nullptr), HB_PARSTRDEF(2, &hDst, nullptr), hbwapi_par_BOOL(3));
   hbwapi_SetLastError(GetLastError());
   hbwapi_ret_L(bResult);
   hb_strfree(hSrc);
   hb_strfree(hDst);
}
