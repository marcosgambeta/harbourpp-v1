/*
 * Windows Service API
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

#include "hbwapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"

static SERVICE_STATUS        s_ServiceStatus;
static SERVICE_STATUS_HANDLE s_hStatus;
static PHB_ITEM              s_pHarbourEntryFunc = nullptr;
static PHB_ITEM              s_pHarbourControlFunc = nullptr;
static TCHAR                 s_lpServiceName[256];

/* Control handler function */
static VOID WINAPI hbwin_SvcControlHandler(DWORD fdwControl)
{
   if( s_pHarbourControlFunc )
   {
      if( hb_vmRequestReenterExt() )
      {
         hb_vmPushEvalSym();
         hb_vmPush(s_pHarbourControlFunc);
         hb_vmPushNumInt(static_cast<HB_MAXINT>(fdwControl));
         hb_vmSend(1);
         hb_vmRequestRestore();
      }
      else
      {
#if 0
         HB_TRACE(HB_TR_DEBUG, ("HVM stack not available"));
#endif
      }
      return;
   }

   switch( fdwControl )
   {
      case SERVICE_CONTROL_STOP:
         s_ServiceStatus.dwWin32ExitCode = 0;
         s_ServiceStatus.dwCurrentState  = SERVICE_STOPPED;
         break;

      case SERVICE_CONTROL_SHUTDOWN:
         s_ServiceStatus.dwWin32ExitCode = 0;
         s_ServiceStatus.dwCurrentState  = SERVICE_STOPPED;
         break;

      default:
         return;
   }

   SetServiceStatus(s_hStatus, &s_ServiceStatus);  /* Report current status */
}

static VOID WINAPI hbwin_SvcMainFunction(DWORD dwArgc, LPTSTR * lpszArgv)
{
   s_ServiceStatus.dwServiceType             = SERVICE_WIN32;
   s_ServiceStatus.dwCurrentState            = SERVICE_START_PENDING;
   s_ServiceStatus.dwControlsAccepted        = SERVICE_ACCEPT_STOP | SERVICE_ACCEPT_SHUTDOWN;
   s_ServiceStatus.dwWin32ExitCode           = 0;
   s_ServiceStatus.dwServiceSpecificExitCode = 0;
   s_ServiceStatus.dwCheckPoint              = 0;
   s_ServiceStatus.dwWaitHint                = 0;

   s_hStatus = RegisterServiceCtrlHandler(s_lpServiceName, static_cast<LPHANDLER_FUNCTION>(hbwin_SvcControlHandler));

   if( s_hStatus != static_cast<SERVICE_STATUS_HANDLE>(0) )
   {
      if( s_pHarbourEntryFunc != nullptr )
      {
         if( hb_vmRequestReenterExt() )
         {
            int iArgCount = 0;

            if( !s_pHarbourControlFunc )
            {
               /* We report the running status to SCM. */
               s_ServiceStatus.dwCurrentState = SERVICE_RUNNING;
               SetServiceStatus(s_hStatus, &s_ServiceStatus);
            }

            hb_vmPushEvalSym();
            hb_vmPush(s_pHarbourEntryFunc);

            for( DWORD i = 1; i < dwArgc; ++i )
            {
               PHB_ITEM pItem = hb_stackAllocItem();

               HB_ITEMPUTSTR(pItem, lpszArgv[i]);
               if( hb_cmdargIsInternal(hb_itemGetCPtr(pItem), nullptr) )
               {
                  hb_stackPop();
               }
               else
               {
                  ++iArgCount;
               }
            }

            hb_vmSend(static_cast<HB_USHORT>(iArgCount));

            hb_vmRequestRestore();
         }
         else
         {
#if 0
            HB_TRACE(HB_TR_DEBUG, ("HVM stack not available"));
#endif
         }
      }
      else
      {
#if 0
         HB_TRACE(HB_TR_DEBUG, ("Harbour service entry function not found"));
#endif
      }
   }
   else
   {
#if 0
      HB_TRACE(HB_TR_DEBUG, ("Error registering service"));
#endif
   }
}

HB_FUNC( WIN_SERVICEGETSTATUS )
{
   hb_retnint(s_ServiceStatus.dwCurrentState);
}

HB_FUNC( WIN_SERVICESETSTATUS )
{
   HB_BOOL bRetVal;
   s_ServiceStatus.dwCurrentState = static_cast<DWORD>(hb_parnl(1));
   bRetVal = static_cast<HB_BOOL>(SetServiceStatus(s_hStatus, &s_ServiceStatus));
   hbwapi_SetLastError(GetLastError());
   hb_retl(bRetVal);
}

HB_FUNC( WIN_SERVICESETEXITCODE )
{
   HB_BOOL bRetVal;
   s_ServiceStatus.dwWin32ExitCode = static_cast<DWORD>(hb_parnl(1));
   bRetVal = static_cast<HB_BOOL>(SetServiceStatus(s_hStatus, &s_ServiceStatus));
   hbwapi_SetLastError(GetLastError());
   hb_retl(bRetVal);
}

HB_FUNC( WIN_SERVICESTOP )
{
   HB_BOOL bRetVal;
   s_ServiceStatus.dwCurrentState = SERVICE_STOPPED;
   bRetVal = static_cast<HB_BOOL>(SetServiceStatus(s_hStatus, &s_ServiceStatus));
   hbwapi_SetLastError(GetLastError());
   hb_retl(bRetVal);
}

HB_FUNC( WIN_SERVICESTART )
{
   HB_BOOL bRetVal;

   PHB_ITEM pEntryFunc;

   SERVICE_TABLE_ENTRY lpServiceTable[2];

   HB_ITEMCOPYSTR(hb_param(1, Harbour::Item::STRING), s_lpServiceName, HB_SIZEOFARRAY(s_lpServiceName));

   if( s_pHarbourEntryFunc )
   {
      hb_itemRelease(s_pHarbourEntryFunc);
      s_pHarbourEntryFunc = nullptr;
   }

   pEntryFunc = hb_param(2, Harbour::Item::EVALITEM);

   if( pEntryFunc )
   {
      s_pHarbourEntryFunc = hb_itemNew(pEntryFunc);
   }

   if( s_pHarbourControlFunc )
   {
      hb_itemRelease(s_pHarbourControlFunc);
      s_pHarbourControlFunc = nullptr;
   }

   pEntryFunc = hb_param(3, Harbour::Item::EVALITEM);

   if( pEntryFunc )
   {
      s_pHarbourControlFunc = hb_itemNew(pEntryFunc);
   }

   lpServiceTable[0].lpServiceName = s_lpServiceName;
   lpServiceTable[0].lpServiceProc = static_cast<LPSERVICE_MAIN_FUNCTION>(hbwin_SvcMainFunction);

   lpServiceTable[1].lpServiceName = nullptr;
   lpServiceTable[1].lpServiceProc = nullptr;

   bRetVal = static_cast<HB_BOOL>(StartServiceCtrlDispatcher(lpServiceTable));
   hbwapi_SetLastError(GetLastError());
   hb_retl(bRetVal);
}
