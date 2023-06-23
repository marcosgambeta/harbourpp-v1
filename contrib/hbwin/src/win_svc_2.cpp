/*
 * Windows Service Management API
 *
 * Copyright 2010 Jose Luis Capel <jlcapel at hotmail . com>
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

HB_FUNC( WIN_SERVICEINSTALL )
{
   HB_BOOL bRetVal = HB_FALSE;

   void * hPath;
   LPCTSTR lpPath = HB_PARSTR(3, &hPath, nullptr);

   TCHAR lpPathBuffer[MAX_PATH];

   if( lpPath == nullptr )
   {
      if( GetModuleFileName(nullptr, lpPathBuffer, HB_SIZEOFARRAY(lpPathBuffer)) )
      {
         lpPath = lpPathBuffer;
      }
      else
      {
         hbwapi_SetLastError(GetLastError());
      }
   }

   if( lpPath )
   {
      SC_HANDLE schSCM = OpenSCManager(nullptr, nullptr, SC_MANAGER_ALL_ACCESS);

      if( schSCM )
      {
         SC_HANDLE schSrv;

         void * hServiceName;
         void * hDisplayName;
         void * hAccountName;
         void * hPassword;

         LPCTSTR lpServiceName = HB_PARSTRDEF(1, &hServiceName, nullptr);
         LPCTSTR lpDisplayName = HB_PARSTR(2, &hDisplayName, nullptr);
         LPCTSTR lpAccountName = HB_PARSTR(5, &hAccountName, nullptr);
         LPCTSTR lpPassword = HB_PARSTR(6, &hPassword, nullptr);

         schSrv = CreateService(schSCM,                    /* SCM database */
                                lpServiceName,             /* name of service */
                                lpDisplayName,             /* service name to display */
                                SERVICE_ALL_ACCESS,        /* desired access */
                                SERVICE_WIN32_OWN_PROCESS, /* service type */
                                static_cast<DWORD>(hb_parnldef(4, SERVICE_DEMAND_START)),  /* start type */
                                SERVICE_ERROR_NORMAL,      /* error control type */
                                lpPath,                    /* path to service's binary */
                                nullptr,                      /* no load ordering group */
                                nullptr,                      /* no tag identifier */
                                nullptr,                      /* no dependencies */
                                lpAccountName,             /* default: LocalSystem account */
                                lpPassword);               /* default: no password */

         hbwapi_SetLastError(GetLastError());

         if( schSrv )
         {
            bRetVal = HB_TRUE;

            CloseServiceHandle(schSrv);
         }

         hb_strfree(hServiceName);
         hb_strfree(hDisplayName);
         hb_strfree(hAccountName);
         hb_strfree(hPassword);

         CloseServiceHandle(schSCM);
      }
      else
      {
         hbwapi_SetLastError(GetLastError());
      }
   }

   hb_strfree(hPath);

   hb_retl(bRetVal);
}

HB_FUNC( WIN_SERVICEDELETE )
{
   HB_BOOL bRetVal = HB_FALSE;

   SC_HANDLE schSCM = OpenSCManager(nullptr, nullptr, SC_MANAGER_ALL_ACCESS);

   if( schSCM )
   {
      void * hServiceName;

      SC_HANDLE schSrv = OpenService(schSCM, HB_PARSTRDEF(1, &hServiceName, nullptr), SERVICE_ALL_ACCESS);

      if( schSrv )
      {
         if( hb_parl(2) )  /* Check if service is up and stop it */
         {
            SERVICE_STATUS ssStatus;

            if( ControlService(schSrv, SERVICE_CONTROL_STOP, &ssStatus) )
            {
               while( ssStatus.dwCurrentState != SERVICE_STOPPED && QueryServiceStatus(schSrv, &ssStatus) )
               {
                  hb_idleSleep(1.0);
               }
            }
         }

         bRetVal = static_cast<HB_BOOL>(DeleteService(schSrv));
         hbwapi_SetLastError(GetLastError());

         CloseServiceHandle(schSrv);
      }
      else
      {
         hbwapi_SetLastError(GetLastError());
      }

      hb_strfree(hServiceName);

      CloseServiceHandle(schSCM);
   }
   else
   {
      hbwapi_SetLastError(GetLastError());
   }
   hb_retl(bRetVal);
}

HB_FUNC( WIN_SERVICECONTROL )
{
   HB_BOOL bRetVal = HB_FALSE;

   SC_HANDLE schSCM = OpenSCManager(nullptr, nullptr, SC_MANAGER_ALL_ACCESS);

   if( schSCM )
   {
      void * hServiceName;

      SC_HANDLE schSrv = OpenService(schSCM, HB_PARSTRDEF(1, &hServiceName, nullptr), SERVICE_ALL_ACCESS);

      if( schSrv )
      {
         SERVICE_STATUS ssStatus{};
         bRetVal = static_cast<HB_BOOL>(ControlService(schSrv, static_cast<DWORD>(hb_parnl(2)), &ssStatus));
         hbwapi_SetLastError(GetLastError());

         CloseServiceHandle(schSrv);
      }
      else
      {
         hbwapi_SetLastError(GetLastError());
      }

      hb_strfree(hServiceName);

      CloseServiceHandle(schSCM);
   }
   else
   {
      hbwapi_SetLastError(GetLastError());
   }
   hb_retl(bRetVal);
}

HB_FUNC( WIN_SERVICERUN )
{
   HB_BOOL bRetVal = HB_FALSE;

   SC_HANDLE schSCM = OpenSCManager(nullptr, nullptr, SC_MANAGER_ALL_ACCESS);

   if( schSCM )
   {
      void * hServiceName;

      SC_HANDLE schSrv = OpenService(schSCM, HB_PARSTRDEF(1, &hServiceName, nullptr), SERVICE_ALL_ACCESS);

      if( schSrv )
      {
         DWORD dwArgs, pos;
         void ** hArgs;
         LPCTSTR * lpArgs;

         if( hb_pcount() >= 2 )
         {
            dwArgs = hb_pcount() - 1;
            hArgs = static_cast<void**>(hb_xgrab(dwArgs * sizeof(void*)));
            lpArgs = static_cast<LPCTSTR*>(hb_xgrab(dwArgs * sizeof(LPCTSTR)));

            for( pos = 0; pos < dwArgs; ++pos )
            {
               lpArgs[pos] = HB_PARSTRDEF(pos + 2, &hArgs[pos], nullptr);
            }
         }
         else
         {
            dwArgs = 0;
            hArgs = nullptr;
            lpArgs = nullptr;
         }

         bRetVal = static_cast<HB_BOOL>(StartService(schSrv, dwArgs, lpArgs));
         hbwapi_SetLastError(GetLastError());

         if( hArgs )
         {
            for( pos = 0; pos < dwArgs; ++pos )
            {
               hb_strfree(hArgs[pos]);
            }

            hb_xfree(hArgs);
            hb_xfree(lpArgs);
         }

         CloseServiceHandle(schSrv);
      }
      else
      {
         hbwapi_SetLastError(GetLastError());
      }

      hb_strfree(hServiceName);

      CloseServiceHandle(schSCM);
   }
   else
   {
      hbwapi_SetLastError(GetLastError());
   }
   hb_retl(bRetVal);
}
