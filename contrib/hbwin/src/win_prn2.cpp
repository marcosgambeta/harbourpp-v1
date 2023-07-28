/*
 * Harbour Windows Printing support functions
 *
 * Copyright 2009 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2002 Luiz Rafael Culik <culikr@uol.com.br>
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

#include "hbwin.hpp"
#include "hbwapi.hpp"
#include "hbapifs.hpp"
#include "hbapiitm.hpp"
#include "hbwin.ch"
#include <winspool.h>

#define _ENUMPRN_FLAGS_  (PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS)

static HB_BOOL hb_IsLegacyDevice(const char * pszPrinterName)
{
   static const char * s_pszPrnDev[] = { "lpt1", "lpt2", "lpt3", "lpt4", "lpt5", "lpt6", "com1", "com2", "com3", "com4", nullptr };

   for( int i = 0; s_pszPrnDev[i]; ++i )
   {
      if( hb_strnicmp(pszPrinterName, s_pszPrnDev[i], strlen(s_pszPrnDev[i])) == 0 )
      {
         return true;
      }
   }

   return false;
}

HB_FUNC( WIN_PRINTEREXISTS )
{
   HB_BOOL bResult = HB_FALSE;

   if( HB_ISCHAR(1) )
   {
      const char * pszPrinterName = hb_parc(1);

      /* Don't bother with test if '\' in string */
      if( !strchr(pszPrinterName, HB_OS_PATH_LIST_SEP_CHR) && !hb_IsLegacyDevice(pszPrinterName) )
      {
         DWORD dwNeeded = 0, dwReturned = 0;

         EnumPrinters(_ENUMPRN_FLAGS_, nullptr, 5, nullptr, 0, &dwNeeded, &dwReturned);
         if( dwNeeded )
         {
            PRINTER_INFO_5 * pPrinterEnumBak;
            PRINTER_INFO_5 * pPrinterEnum = pPrinterEnumBak = static_cast<PRINTER_INFO_5*>(hb_xgrabz(dwNeeded));

            if( EnumPrinters(_ENUMPRN_FLAGS_, nullptr, 5, reinterpret_cast<LPBYTE>(pPrinterEnum), dwNeeded, &dwNeeded, &dwReturned) )
            {
               PHB_ITEM pTemp = hb_itemNew(nullptr);

               for( DWORD i = 0; !bResult && i < dwReturned; ++i, ++pPrinterEnum )
               {
                  HB_ITEMPUTSTR(pTemp, pPrinterEnum->pPrinterName);
                  bResult = (strcmp(pszPrinterName, hb_itemGetCPtr(pTemp)) == 0);
               }

               hb_itemRelease(pTemp);
            }

            hb_xfree(pPrinterEnumBak);
         }
      }
   }

   hb_retl(bResult);
}

static void hb_GetDefaultPrinter(PHB_ITEM pPrinterName)
{
   bool bResult = false;

   hb_itemPutC(pPrinterName, nullptr);

   TCHAR lpPrinterName[256];
   DWORD dwSize = HB_SIZEOFARRAY(lpPrinterName) - 1;
   bResult = GetDefaultPrinter(lpPrinterName, &dwSize);
   HB_ITEMPUTSTR(pPrinterName, lpPrinterName);

   if( !bResult ) /* Win9x and Windows NT 4.0 or earlier & 2000+ if necessary for some reason i.e. dll could not load! */
   {
      TCHAR lpPrinterName[256];

      DWORD dwSize = GetProfileString(TEXT("windows"), TEXT("device"), TEXT(""), lpPrinterName, static_cast<DWORD>(HB_SIZEOFARRAY(lpPrinterName)) - 1);

      if( dwSize && dwSize < HB_SIZEOFARRAY(lpPrinterName) )
      {
         dwSize = 0;
         while( lpPrinterName[dwSize] != '\0' && lpPrinterName[dwSize] != ',' )
         {
            dwSize++;
         }
         lpPrinterName[dwSize] = '\0';

         bResult = true;

         HB_ITEMPUTSTRLEN(pPrinterName, lpPrinterName, dwSize);
      }
   }
}

HB_FUNC( WIN_PRINTERGETDEFAULT )
{
   PHB_ITEM pPrinterName = hb_itemNew(nullptr);

   hb_GetDefaultPrinter(pPrinterName);

   hb_itemReturnRelease(pPrinterName);
}

static HB_BOOL hb_GetJobs(HANDLE hPrinter, JOB_INFO_2 ** ppJobInfo, DWORD * pdwJobs)
{
   HB_BOOL bResult = HB_FALSE;
   DWORD dwNeeded = 0;

   GetPrinter(hPrinter, 2, nullptr, 0, &dwNeeded);
   if( dwNeeded )
   {
      PRINTER_INFO_2 * pPrinterInfo = static_cast<PRINTER_INFO_2*>(hb_xgrabz(dwNeeded));
      DWORD dwUsed = 0;

      if( GetPrinter(hPrinter, 2, reinterpret_cast<LPBYTE>(pPrinterInfo), dwNeeded, &dwUsed) )
      {
         DWORD dwReturned = 0;

         EnumJobs(hPrinter, 0, pPrinterInfo->cJobs, 2, nullptr, 0, &dwNeeded, &dwReturned);
         if( dwNeeded )
         {
            JOB_INFO_2 * pJobInfo = static_cast<JOB_INFO_2*>(hb_xgrabz(dwNeeded));

            if( EnumJobs(hPrinter, 0, dwReturned, 2, reinterpret_cast<LPBYTE>(pJobInfo), dwNeeded, &dwUsed, &dwReturned) )
            {
               *pdwJobs = dwReturned;
               *ppJobInfo = pJobInfo;
               bResult = true;
            }
            else
            {
               hb_xfree(pJobInfo);
            }
         }
      }
      hb_xfree(pPrinterInfo);
   }

   return bResult;
}

HB_FUNC( WIN_PRINTERSTATUS )
{
   long nStatus = HB_WIN_PRINTER_STATUS_ERROR;

   PHB_ITEM pPrinterName = hb_itemParam(1);

   if( hb_itemGetCLen(pPrinterName) == 0 )
   {
      hb_GetDefaultPrinter(pPrinterName);
   }

   if( hb_itemGetCLen(pPrinterName) > 0 )
   {
      void * hPrinterName;
      LPCTSTR lpPrinterName = HB_ITEMGETSTR(pPrinterName, &hPrinterName, nullptr);
      HANDLE hPrinter;

      if( OpenPrinter(const_cast<LPTSTR>(lpPrinterName), &hPrinter, nullptr) )
      {
         DWORD dwNeeded = 0;

         GetPrinter(hPrinter, 2, nullptr, 0, &dwNeeded);
         if( dwNeeded )
         {
            PRINTER_INFO_2 * pPrinterInfo = static_cast<PRINTER_INFO_2*>(hb_xgrabz(dwNeeded));

            if( GetPrinter(hPrinter, 2, reinterpret_cast<LPBYTE>(pPrinterInfo), dwNeeded, &dwNeeded) )
            {
               nStatus = static_cast<long>(pPrinterInfo->Status);
            }

            hb_xfree(pPrinterInfo);
         }

         if( nStatus == 0 )
         {
            JOB_INFO_2 * pJobs = nullptr;
            DWORD dwJobs = 0;

            if( hb_GetJobs(hPrinter, &pJobs, &dwJobs) )
            {
               for( DWORD i = 0; nStatus == 0 && i < dwJobs; ++i )
               {
                  if( pJobs[i].Status & JOB_STATUS_ERROR )
                  {
                     nStatus = -20;
                  }
                  else if( pJobs[i].Status & JOB_STATUS_OFFLINE )
                  {
                     nStatus = -21;
                  }
                  else if( pJobs[i].Status & JOB_STATUS_PAPEROUT )
                  {
                     nStatus = -22;
                  }
                  else if( pJobs[i].Status & JOB_STATUS_BLOCKED_DEVQ )
                  {
                     nStatus = -23;
                  }
               }
               hb_xfree(pJobs);
            }
         }

         ClosePrinter(hPrinter);
      }

      hb_strfree(hPrinterName);
   }

   hb_itemRelease(pPrinterName);

   hb_retnl(nStatus);
}

HB_FUNC( WIN_PRINTERPORTTONAME )
{
   /* Set default return value */
   hb_retc_null();

   if( hb_parclen(1) > 0 )
   {
      DWORD dwNeeded = 0, dwReturned = 0;

      EnumPrinters(_ENUMPRN_FLAGS_, nullptr, 5, nullptr, 0, &dwNeeded, &dwReturned);
      if( dwNeeded )
      {
         PRINTER_INFO_5 * pPrinterEnumBak;
         PRINTER_INFO_5 * pPrinterEnum = pPrinterEnumBak = static_cast<PRINTER_INFO_5*>(hb_xgrabz(dwNeeded));

         if( EnumPrinters(_ENUMPRN_FLAGS_, nullptr, 5, reinterpret_cast<LPBYTE>(pPrinterEnum), dwNeeded, &dwNeeded, &dwReturned) )
         {
            const char * pszPortNameFind = hb_parc(1);
            HB_BOOL bSubStr = hb_parl(2);
            HB_BOOL bFound = HB_FALSE;
            PHB_ITEM pTemp = hb_itemNew(nullptr);

            for( DWORD i = 0; i < dwReturned && !bFound; ++i, ++pPrinterEnum )
            {
               HB_ITEMPUTSTR(pTemp, pPrinterEnum->pPortName);

               if( bSubStr )
               {
                  bFound = (hb_strnicmp(hb_itemGetCPtr(pTemp), pszPortNameFind, strlen(pszPortNameFind)) == 0);
               }
               else
               {
                  bFound = (hb_stricmp(hb_itemGetCPtr(pTemp), pszPortNameFind) == 0);
               }

               if( bFound )
               {
                  HB_RETSTR(pPrinterEnum->pPrinterName);
               }
            }

            hb_itemRelease(pTemp);
         }

         hb_xfree(pPrinterEnumBak);
      }
   }
}

#define HB_PRINT_BUFFER_SIZE  (32 * 1024)

HB_FUNC( WIN_PRINTFILERAW )
{
   HB_ISIZ nResult = -1;

   if( HB_ISCHAR(1) && HB_ISCHAR(2) )
   {
      const char * pszFileName = hb_parc(2);

      HANDLE hPrinter;
      void * hDeviceName;
      LPCTSTR lpDeviceName = HB_PARSTR(1, &hDeviceName, nullptr);

      if( OpenPrinter(const_cast<LPTSTR>(lpDeviceName), &hPrinter, nullptr) != 0 )
      {
         void * hDocName;
         DOC_INFO_1 DocInfo;

         DocInfo.pDocName = const_cast<LPTSTR>(HB_PARSTR(HB_ISCHAR(3) ? 3 : 2, &hDocName, nullptr));
         DocInfo.pOutputFile = nullptr;
         DocInfo.pDatatype = const_cast<LPTSTR>(TEXT("RAW"));

         if( StartDocPrinter(hPrinter, 1, reinterpret_cast<LPBYTE>(&DocInfo)) != 0 )
         {
            if( StartPagePrinter(hPrinter) != 0 )
            {
               PHB_FILE pFile = hb_fileExtOpen(pszFileName, nullptr, FO_READ | FO_SHARED | FO_PRIVATE | FXO_SHARELOCK, nullptr, nullptr);
               if( pFile != nullptr )
               {
                  HB_BYTE * pbyBuffer = static_cast<HB_BYTE*>(hb_xgrab(HB_PRINT_BUFFER_SIZE));
                  HB_SIZE nRead;

                  nResult = 1;
                  while( (nRead = hb_fileRead(pFile, pbyBuffer, HB_PRINT_BUFFER_SIZE, -1)) > 0 && nRead != static_cast<HB_SIZE>(FS_ERROR) )
                  {
                     HB_SIZE nWritten = 0;

                     while( nWritten < nRead )
                     {
                        DWORD dwWritten = 0;
                        if( !WritePrinter(hPrinter, &pbyBuffer[nWritten], static_cast<DWORD>(nRead - nWritten), &dwWritten) )
                        {
                           nResult = -7;
                           break;
                        }
                        else if( dwWritten == 0 )
                        {
                           nResult = -8;
                           break;
                        }
                        nWritten += dwWritten;
                     }
                     if( nWritten < nRead )
                     {
                        break;
                     }
                  }
                  hbwapi_SetLastError(GetLastError());

                  hb_fileClose(pFile);
                  hb_xfree(pbyBuffer);
               }
               else
               {
                  hbwapi_SetLastError(hb_fsOsError());
                  nResult = -6;
               }
               EndPagePrinter(hPrinter);
            }
            else
            {
               hbwapi_SetLastError(GetLastError());
               nResult = -4;
            }
            EndDocPrinter(hPrinter);
         }
         else
         {
            hbwapi_SetLastError(GetLastError());
            nResult = -3;
         }
         ClosePrinter(hPrinter);
         hb_strfree(hDocName);
      }
      else
      {
         hbwapi_SetLastError(GetLastError());
         nResult = -2;
      }
      hb_strfree(hDeviceName);
   }

   hb_retns(nResult);
}

HB_FUNC( WIN_PRINTDATARAW )
{
   HB_ISIZ nResult = -1;

   if( HB_ISCHAR(1) && HB_ISCHAR(2) )
   {
      HANDLE hPrinter;
      void * hDeviceName;
      LPCTSTR lpDeviceName = HB_PARSTR(1, &hDeviceName, nullptr);

      if( OpenPrinter(const_cast<LPTSTR>(lpDeviceName), &hPrinter, nullptr) != 0 )
      {
         void * hDocName;
         DOC_INFO_1 DocInfo;

         DocInfo.pDocName = const_cast<LPTSTR>(HB_PARSTR(3, &hDocName, nullptr));
         DocInfo.pOutputFile = nullptr;
         DocInfo.pDatatype = const_cast<LPTSTR>(TEXT("RAW"));
         if( DocInfo.pDocName == nullptr )
         {
            DocInfo.pDocName = DocInfo.pDatatype;
         }

         if( StartDocPrinter(hPrinter, 1, reinterpret_cast<LPBYTE>(&DocInfo)) != 0 )
         {
            if( StartPagePrinter(hPrinter) != 0 )
            {
               HB_BYTE * pbData = reinterpret_cast<HB_BYTE*>(const_cast<char*>(hb_parc(2)));
               HB_SIZE nLen = hb_parclen(2);

               nResult = 0;
               while( static_cast<HB_SIZE>(nResult) < nLen )
               {
                  DWORD dwWritten = 0;
                  if( !WritePrinter(hPrinter, &pbData[nResult], static_cast<DWORD>(nLen - nResult), &dwWritten) || dwWritten == 0 )
                  {
                     break;
                  }
                  nResult += dwWritten;
               }
               hbwapi_SetLastError(GetLastError());
               EndPagePrinter(hPrinter);
            }
            else
            {
               hbwapi_SetLastError(GetLastError());
               nResult = -4;
            }
            EndDocPrinter(hPrinter);
         }
         else
         {
            hbwapi_SetLastError(GetLastError());
            nResult = -3;
         }
         ClosePrinter(hPrinter);
         hb_strfree(hDocName);
      }
      else
      {
         hbwapi_SetLastError(GetLastError());
         nResult = -2;
      }
      hb_strfree(hDeviceName);
   }

   hb_retns(nResult);
}

HB_FUNC( WIN_PRINTERLIST )
{
   PHB_ITEM pPrinterArray = hb_itemArrayNew(0);

   HB_BOOL bPrinterNamesOnly = !hb_parl(1);
   HB_BOOL bLocalPrintersOnly = hb_parl(2);
   DWORD dwNeeded = 0, dwReturned = 0;

   EnumPrinters(_ENUMPRN_FLAGS_, nullptr, 5, nullptr, 0, &dwNeeded, &dwReturned);
   if( dwNeeded )
   {
      PRINTER_INFO_5 * pPrinterEnumBak;
      PRINTER_INFO_5 * pPrinterEnum = pPrinterEnumBak = static_cast<PRINTER_INFO_5*>(hb_xgrabz(dwNeeded));

      if( EnumPrinters(_ENUMPRN_FLAGS_, nullptr, 5, reinterpret_cast<LPBYTE>(pPrinterEnum), dwNeeded, &dwNeeded, &dwReturned) )
      {
         PHB_ITEM pTempItem = hb_itemNew(nullptr);

         for( DWORD i = 0; i < dwReturned; ++i, ++pPrinterEnum )
         {
            if( !bLocalPrintersOnly || pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_LOCAL )
            {
               if( bPrinterNamesOnly )
               {
                  hb_arrayAddForward(pPrinterArray, HB_ITEMPUTSTR(pTempItem, pPrinterEnum->pPrinterName));
               }
               else
               {
                  HANDLE hPrinter;

                  if( OpenPrinter(pPrinterEnum->pPrinterName, &hPrinter, nullptr) )
                  {
                     GetPrinter(hPrinter, 2, nullptr, 0, &dwNeeded);
                     if( dwNeeded )
                     {
                        hb_arrayNew(pTempItem, HB_WINPRN_LEN_);

                        HB_ARRAYSETSTR(pTempItem, HB_WINPRN_NAME, pPrinterEnum->pPrinterName);

                        {
                           PRINTER_INFO_2 * pPrinterInfo2 = static_cast<PRINTER_INFO_2*>(hb_xgrabz(dwNeeded));

                           if( GetPrinter(hPrinter, 2, reinterpret_cast<LPBYTE>(pPrinterInfo2), dwNeeded, &dwNeeded) )
                           {
                              HB_ARRAYSETSTR(pTempItem, HB_WINPRN_PORT, pPrinterInfo2->pPortName);
                              HB_ARRAYSETSTR(pTempItem, HB_WINPRN_DRIVER, pPrinterInfo2->pDriverName);
                              HB_ARRAYSETSTR(pTempItem, HB_WINPRN_SHARE, pPrinterInfo2->pShareName);
                              HB_ARRAYSETSTR(pTempItem, HB_WINPRN_SERVER, pPrinterInfo2->pServerName);
                           }
                           else
                           {
                              hb_arraySetC(pTempItem, HB_WINPRN_PORT, nullptr);
                              hb_arraySetC(pTempItem, HB_WINPRN_DRIVER, nullptr);
                              hb_arraySetC(pTempItem, HB_WINPRN_SHARE, nullptr);
                              hb_arraySetC(pTempItem, HB_WINPRN_SERVER, nullptr);
                           }

                           hb_xfree(pPrinterInfo2);
                        }

                        if( pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_LOCAL )
                        {
                           hb_arraySetC(pTempItem, HB_WINPRN_TYPE, "LOCAL");
                        }
                        else if( pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_NETWORK )
                        {
                           hb_arraySetC(pTempItem, HB_WINPRN_TYPE, "NETWORK");
                        }
                        else
                        {
                           hb_arraySetC(pTempItem, HB_WINPRN_TYPE, nullptr);
                        }

                        hb_arrayAddForward(pPrinterArray, pTempItem);
                     }
                     ClosePrinter(hPrinter);
                  }
               }
            }
         }

         hb_itemRelease(pTempItem);
      }
      hb_xfree(pPrinterEnumBak);
   }

   hb_itemReturnRelease(pPrinterArray);
}
