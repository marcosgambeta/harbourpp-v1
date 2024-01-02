/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2023 Marcos Antonio Gambeta

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/*
  NOTE: source code generated with the help of a code generator
*/

#include <windows.h>
#include <winspool.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

/*
WINBOOL WINAPI EnumPrintersA(DWORD Flags,LPSTR Name,DWORD Level,LPBYTE pPrinterEnum,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
WINBOOL WINAPI EnumPrintersW(DWORD Flags,LPWSTR Name,DWORD Level,LPBYTE pPrinterEnum,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
WINBOOL WINAPI OpenPrinterA(LPSTR pPrinterName,LPHANDLE phPrinter,LPPRINTER_DEFAULTSA pDefault)
*/

/*
WINBOOL WINAPI OpenPrinterW(LPWSTR pPrinterName,LPHANDLE phPrinter,LPPRINTER_DEFAULTSW pDefault)
*/

/*
WINBOOL WINAPI ResetPrinterA(HANDLE hPrinter,LPPRINTER_DEFAULTSA pDefault)
*/

/*
WINBOOL WINAPI ResetPrinterW(HANDLE hPrinter,LPPRINTER_DEFAULTSW pDefault)
*/

/*
WINBOOL WINAPI SetJobA(HANDLE hPrinter,DWORD JobId,DWORD Level,LPBYTE pJob,DWORD Command)
*/

/*
WINBOOL WINAPI SetJobW(HANDLE hPrinter,DWORD JobId,DWORD Level,LPBYTE pJob,DWORD Command)
*/

/*
WINBOOL WINAPI GetJobA(HANDLE hPrinter,DWORD JobId,DWORD Level,LPBYTE pJob,DWORD cbBuf,LPDWORD pcbNeeded)
*/

/*
WINBOOL WINAPI GetJobW(HANDLE hPrinter,DWORD JobId,DWORD Level,LPBYTE pJob,DWORD cbBuf,LPDWORD pcbNeeded)
*/

/*
WINBOOL WINAPI EnumJobsA(HANDLE hPrinter,DWORD FirstJob,DWORD NoJobs,DWORD Level,LPBYTE pJob,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
WINBOOL WINAPI EnumJobsW(HANDLE hPrinter,DWORD FirstJob,DWORD NoJobs,DWORD Level,LPBYTE pJob,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
HANDLE WINAPI AddPrinterA(LPSTR pName,DWORD Level,LPBYTE pPrinter)
*/

/*
HANDLE WINAPI AddPrinterW(LPWSTR pName,DWORD Level,LPBYTE pPrinter)
*/

/*
WINBOOL WINAPI DeletePrinter(HANDLE hPrinter)
*/
HB_FUNC( WADELETEPRINTER )
{
  winapi_ret_BOOL(DeletePrinter(wa_par_HANDLE(1)));
}

/*
WINBOOL WINAPI SetPrinterA(HANDLE hPrinter,DWORD Level,LPBYTE pPrinter,DWORD Command)
*/

/*
WINBOOL WINAPI SetPrinterW(HANDLE hPrinter,DWORD Level,LPBYTE pPrinter,DWORD Command)
*/

/*
WINBOOL WINAPI GetPrinterA(HANDLE hPrinter,DWORD Level,LPBYTE pPrinter,DWORD cbBuf,LPDWORD pcbNeeded)
*/

/*
WINBOOL WINAPI GetPrinterW(HANDLE hPrinter,DWORD Level,LPBYTE pPrinter,DWORD cbBuf,LPDWORD pcbNeeded)
*/

/*
WINBOOL WINAPI AddPrinterDriverA(LPSTR pName,DWORD Level,LPBYTE pDriverInfo)
*/

/*
WINBOOL WINAPI AddPrinterDriverW(LPWSTR pName,DWORD Level,LPBYTE pDriverInfo)
*/

/*
WINBOOL WINAPI AddPrinterDriverExA(LPSTR pName,DWORD Level,LPBYTE pDriverInfo,DWORD dwFileCopyFlags)
*/

/*
WINBOOL WINAPI AddPrinterDriverExW(LPWSTR pName,DWORD Level,LPBYTE pDriverInfo,DWORD dwFileCopyFlags)
*/

/*
WINBOOL WINAPI EnumPrinterDriversA(LPSTR pName,LPSTR pEnvironment,DWORD Level,LPBYTE pDriverInfo,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
WINBOOL WINAPI EnumPrinterDriversW(LPWSTR pName,LPWSTR pEnvironment,DWORD Level,LPBYTE pDriverInfo,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
WINBOOL WINAPI GetPrinterDriverA(HANDLE hPrinter,LPSTR pEnvironment,DWORD Level,LPBYTE pDriverInfo,DWORD cbBuf,LPDWORD pcbNeeded)
*/

/*
WINBOOL WINAPI GetPrinterDriverW(HANDLE hPrinter,LPWSTR pEnvironment,DWORD Level,LPBYTE pDriverInfo,DWORD cbBuf,LPDWORD pcbNeeded)
*/

/*
WINBOOL WINAPI GetPrinterDriverDirectoryA(LPSTR pName,LPSTR pEnvironment,DWORD Level,LPBYTE pDriverDirectory,DWORD cbBuf,LPDWORD pcbNeeded)
*/

/*
WINBOOL WINAPI GetPrinterDriverDirectoryW(LPWSTR pName,LPWSTR pEnvironment,DWORD Level,LPBYTE pDriverDirectory,DWORD cbBuf,LPDWORD pcbNeeded)
*/

/*
WINBOOL WINAPI DeletePrinterDriverA(LPSTR pName,LPSTR pEnvironment,LPSTR pDriverName)
*/
HB_FUNC( WADELETEPRINTERDRIVERA )
{
  winapi_ret_BOOL(DeletePrinterDriverA(const_cast<LPSTR>(hb_parc(1)), const_cast<LPSTR>(hb_parc(2)), const_cast<LPSTR>(hb_parc(3))));
}

/*
WINBOOL WINAPI DeletePrinterDriverW(LPWSTR pName,LPWSTR pEnvironment,LPWSTR pDriverName)
*/
HB_FUNC( WADELETEPRINTERDRIVERW )
{
  winapi_ret_BOOL(DeletePrinterDriverW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3)))));
}

/*
WINBOOL WINAPI DeletePrinterDriverExA(LPSTR pName,LPSTR pEnvironment,LPSTR pDriverName,DWORD dwDeleteFlag,DWORD dwVersionFlag)
*/
HB_FUNC( WADELETEPRINTERDRIVEREXA )
{
  winapi_ret_BOOL(DeletePrinterDriverExA(const_cast<LPSTR>(hb_parc(1)), const_cast<LPSTR>(hb_parc(2)), const_cast<LPSTR>(hb_parc(3)), wa_par_DWORD(4), wa_par_DWORD(5)));
}

/*
WINBOOL WINAPI DeletePrinterDriverExW(LPWSTR pName,LPWSTR pEnvironment,LPWSTR pDriverName,DWORD dwDeleteFlag,DWORD dwVersionFlag)
*/
HB_FUNC( WADELETEPRINTERDRIVEREXW )
{
  winapi_ret_BOOL(DeletePrinterDriverExW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3))), wa_par_DWORD(4), wa_par_DWORD(5)));
}

/*
WINBOOL WINAPI AddPrintProcessorA(LPSTR pName,LPSTR pEnvironment,LPSTR pPathName,LPSTR pPrintProcessorName)
*/
HB_FUNC( WAADDPRINTPROCESSORA )
{
  winapi_ret_BOOL(AddPrintProcessorA(const_cast<LPSTR>(hb_parc(1)), const_cast<LPSTR>(hb_parc(2)), const_cast<LPSTR>(hb_parc(3)), const_cast<LPSTR>(hb_parc(4))));
}

/*
WINBOOL WINAPI AddPrintProcessorW(LPWSTR pName,LPWSTR pEnvironment,LPWSTR pPathName,LPWSTR pPrintProcessorName)
*/
HB_FUNC( WAADDPRINTPROCESSORW )
{
  winapi_ret_BOOL(AddPrintProcessorW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(4)))));
}

/*
WINBOOL WINAPI EnumPrintProcessorsA(LPSTR pName,LPSTR pEnvironment,DWORD Level,LPBYTE pPrintProcessorInfo,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
WINBOOL WINAPI EnumPrintProcessorsW(LPWSTR pName,LPWSTR pEnvironment,DWORD Level,LPBYTE pPrintProcessorInfo,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
WINBOOL WINAPI GetPrintProcessorDirectoryA(LPSTR pName,LPSTR pEnvironment,DWORD Level,LPBYTE pPrintProcessorInfo,DWORD cbBuf,LPDWORD pcbNeeded)
*/

/*
WINBOOL WINAPI GetPrintProcessorDirectoryW(LPWSTR pName,LPWSTR pEnvironment,DWORD Level,LPBYTE pPrintProcessorInfo,DWORD cbBuf,LPDWORD pcbNeeded)
*/

/*
WINBOOL WINAPI EnumPrintProcessorDatatypesA(LPSTR pName,LPSTR pPrintProcessorName,DWORD Level,LPBYTE pDatatypes,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
WINBOOL WINAPI EnumPrintProcessorDatatypesW(LPWSTR pName,LPWSTR pPrintProcessorName,DWORD Level,LPBYTE pDatatypes,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
WINBOOL WINAPI DeletePrintProcessorA(LPSTR pName,LPSTR pEnvironment,LPSTR pPrintProcessorName)
*/
HB_FUNC( WADELETEPRINTPROCESSORA )
{
  winapi_ret_BOOL(DeletePrintProcessorA(const_cast<LPSTR>(hb_parc(1)), const_cast<LPSTR>(hb_parc(2)), const_cast<LPSTR>(hb_parc(3))));
}

/*
WINBOOL WINAPI DeletePrintProcessorW(LPWSTR pName,LPWSTR pEnvironment,LPWSTR pPrintProcessorName)
*/
HB_FUNC( WADELETEPRINTPROCESSORW )
{
  winapi_ret_BOOL(DeletePrintProcessorW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3)))));
}

/*
DWORD WINAPI StartDocPrinterA(HANDLE hPrinter,DWORD Level,LPBYTE pDocInfo)
*/

/*
DWORD WINAPI StartDocPrinterW(HANDLE hPrinter,DWORD Level,LPBYTE pDocInfo)
*/

/*
WINBOOL WINAPI StartPagePrinter(HANDLE hPrinter)
*/
HB_FUNC( WASTARTPAGEPRINTER )
{
  winapi_ret_BOOL(StartPagePrinter(wa_par_HANDLE(1)));
}

/*
WINBOOL WINAPI WritePrinter(HANDLE hPrinter,LPVOID pBuf,DWORD cbBuf,LPDWORD pcWritten)
*/
HB_FUNC( WAWRITEPRINTER )
{
  winapi_ret_BOOL(WritePrinter(wa_par_HANDLE(1), static_cast<LPVOID>(hb_parptr(2)), wa_par_DWORD(3), static_cast<LPDWORD>(hb_parptr(4))));
}

/*
WINBOOL WINAPI FlushPrinter(HANDLE hPrinter,LPVOID pBuf,DWORD cbBuf,LPDWORD pcWritten,DWORD cSleep)
*/
HB_FUNC( WAFLUSHPRINTER )
{
  winapi_ret_BOOL(FlushPrinter(wa_par_HANDLE(1), static_cast<LPVOID>(hb_parptr(2)), wa_par_DWORD(3), static_cast<LPDWORD>(hb_parptr(4)), wa_par_DWORD(5)));
}

/*
WINBOOL WINAPI EndPagePrinter(HANDLE hPrinter)
*/
HB_FUNC( WAENDPAGEPRINTER )
{
  winapi_ret_BOOL(EndPagePrinter(wa_par_HANDLE(1)));
}

/*
WINBOOL WINAPI AbortPrinter(HANDLE hPrinter)
*/
HB_FUNC( WAABORTPRINTER )
{
  winapi_ret_BOOL(AbortPrinter(wa_par_HANDLE(1)));
}

/*
WINBOOL WINAPI ReadPrinter(HANDLE hPrinter,LPVOID pBuf,DWORD cbBuf,LPDWORD pNoBytesRead)
*/
HB_FUNC( WAREADPRINTER )
{
  winapi_ret_BOOL(ReadPrinter(wa_par_HANDLE(1), static_cast<LPVOID>(hb_parptr(2)), wa_par_DWORD(3), static_cast<LPDWORD>(hb_parptr(4))));
}

/*
WINBOOL WINAPI EndDocPrinter(HANDLE hPrinter)
*/
HB_FUNC( WAENDDOCPRINTER )
{
  winapi_ret_BOOL(EndDocPrinter(wa_par_HANDLE(1)));
}

/*
WINBOOL WINAPI AddJobA(HANDLE hPrinter,DWORD Level,LPBYTE pData,DWORD cbBuf,LPDWORD pcbNeeded)
*/

/*
WINBOOL WINAPI AddJobW(HANDLE hPrinter,DWORD Level,LPBYTE pData,DWORD cbBuf,LPDWORD pcbNeeded)
*/

/*
WINBOOL WINAPI ScheduleJob(HANDLE hPrinter,DWORD JobId)
*/
HB_FUNC( WASCHEDULEJOB )
{
  winapi_ret_BOOL(ScheduleJob(wa_par_HANDLE(1), wa_par_DWORD(2)));
}

/*
WINBOOL WINAPI PrinterProperties(HWND hWnd,HANDLE hPrinter)
*/
HB_FUNC( WAPRINTERPROPERTIES )
{
  winapi_ret_BOOL(PrinterProperties(wa_par_HWND(1), wa_par_HANDLE(2)));
}

/*
LONG WINAPI DocumentPropertiesA(HWND hWnd,HANDLE hPrinter,LPSTR pDeviceName,PDEVMODEA pDevModeOutput,PDEVMODEA pDevModeInput,DWORD fMode)
*/

/*
LONG WINAPI DocumentPropertiesW(HWND hWnd,HANDLE hPrinter,LPWSTR pDeviceName,PDEVMODEW pDevModeOutput,PDEVMODEW pDevModeInput,DWORD fMode)
*/

/*
LONG WINAPI AdvancedDocumentPropertiesA(HWND hWnd,HANDLE hPrinter,LPSTR pDeviceName,PDEVMODEA pDevModeOutput,PDEVMODEA pDevModeInput)
*/

/*
LONG WINAPI AdvancedDocumentPropertiesW(HWND hWnd,HANDLE hPrinter,LPWSTR pDeviceName,PDEVMODEW pDevModeOutput,PDEVMODEW pDevModeInput)
*/

/*
LONG ExtDeviceMode(HWND hWnd,HANDLE hInst,LPDEVMODEA pDevModeOutput,LPSTR pDeviceName,LPSTR pPort,LPDEVMODEA pDevModeInput,LPSTR pProfile,DWORD fMode)
*/

/*
DWORD WINAPI GetPrinterDataA(HANDLE hPrinter,LPSTR pValueName,LPDWORD pType,LPBYTE pData,DWORD nSize,LPDWORD pcbNeeded)
*/

/*
DWORD WINAPI GetPrinterDataW(HANDLE hPrinter,LPWSTR pValueName,LPDWORD pType,LPBYTE pData,DWORD nSize,LPDWORD pcbNeeded)
*/

/*
DWORD WINAPI GetPrinterDataExA(HANDLE hPrinter,LPCSTR pKeyName,LPCSTR pValueName,LPDWORD pType,LPBYTE pData,DWORD nSize,LPDWORD pcbNeeded)
*/

/*
DWORD WINAPI GetPrinterDataExW(HANDLE hPrinter,LPCWSTR pKeyName,LPCWSTR pValueName,LPDWORD pType,LPBYTE pData,DWORD nSize,LPDWORD pcbNeeded)
*/

/*
DWORD WINAPI EnumPrinterDataA(HANDLE hPrinter,DWORD dwIndex,LPSTR pValueName,DWORD cbValueName,LPDWORD pcbValueName,LPDWORD pType,LPBYTE pData,DWORD cbData,LPDWORD pcbData)
*/

/*
DWORD WINAPI EnumPrinterDataW(HANDLE hPrinter,DWORD dwIndex,LPWSTR pValueName,DWORD cbValueName,LPDWORD pcbValueName,LPDWORD pType,LPBYTE pData,DWORD cbData,LPDWORD pcbData)
*/

/*
DWORD WINAPI EnumPrinterDataExA(HANDLE hPrinter,LPCSTR pKeyName,LPBYTE pEnumValues,DWORD cbEnumValues,LPDWORD pcbEnumValues,LPDWORD pnEnumValues)
*/

/*
DWORD WINAPI EnumPrinterDataExW(HANDLE hPrinter,LPCWSTR pKeyName,LPBYTE pEnumValues,DWORD cbEnumValues,LPDWORD pcbEnumValues,LPDWORD pnEnumValues)
*/

/*
DWORD WINAPI EnumPrinterKeyA(HANDLE hPrinter,LPCSTR pKeyName,LPSTR pSubkey,DWORD cbSubkey,LPDWORD pcbSubkey)
*/
HB_FUNC( WAENUMPRINTERKEYA )
{
  hb_retnl(EnumPrinterKeyA(wa_par_HANDLE(1), wa_par_LPCSTR(2), const_cast<LPSTR>(hb_parc(3)), wa_par_DWORD(4), static_cast<LPDWORD>(hb_parptr(5))));
}

/*
DWORD WINAPI EnumPrinterKeyW(HANDLE hPrinter,LPCWSTR pKeyName,LPWSTR pSubkey,DWORD cbSubkey,LPDWORD pcbSubkey)
*/
HB_FUNC( WAENUMPRINTERKEYW )
{
  hb_retnl(EnumPrinterKeyW(wa_par_HANDLE(1), wa_par_LPCWSTR(2), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3))), wa_par_DWORD(4), static_cast<LPDWORD>(hb_parptr(5))));
}

/*
DWORD WINAPI SetPrinterDataA(HANDLE hPrinter,LPSTR pValueName,DWORD Type,LPBYTE pData,DWORD cbData)
*/

/*
DWORD WINAPI SetPrinterDataW(HANDLE hPrinter,LPWSTR pValueName,DWORD Type,LPBYTE pData,DWORD cbData)
*/

/*
DWORD WINAPI SetPrinterDataExA(HANDLE hPrinter,LPCSTR pKeyName,LPCSTR pValueName,DWORD Type,LPBYTE pData,DWORD cbData)
*/

/*
DWORD WINAPI SetPrinterDataExW(HANDLE hPrinter,LPCWSTR pKeyName,LPCWSTR pValueName,DWORD Type,LPBYTE pData,DWORD cbData)
*/

/*
DWORD WINAPI DeletePrinterDataA(HANDLE hPrinter,LPSTR pValueName)
*/
HB_FUNC( WADELETEPRINTERDATAA )
{
  hb_retnl(DeletePrinterDataA(wa_par_HANDLE(1), const_cast<LPSTR>(hb_parc(2))));
}

/*
DWORD WINAPI DeletePrinterDataW(HANDLE hPrinter,LPWSTR pValueName)
*/
HB_FUNC( WADELETEPRINTERDATAW )
{
  hb_retnl(DeletePrinterDataW(wa_par_HANDLE(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2)))));
}

/*
DWORD WINAPI DeletePrinterDataExA(HANDLE hPrinter,LPCSTR pKeyName,LPCSTR pValueName)
*/
HB_FUNC( WADELETEPRINTERDATAEXA )
{
  hb_retnl(DeletePrinterDataExA(wa_par_HANDLE(1), wa_par_LPCSTR(2), wa_par_LPCSTR(3)));
}

/*
DWORD WINAPI DeletePrinterDataExW(HANDLE hPrinter,LPCWSTR pKeyName,LPCWSTR pValueName)
*/
HB_FUNC( WADELETEPRINTERDATAEXW )
{
  hb_retnl(DeletePrinterDataExW(wa_par_HANDLE(1), wa_par_LPCWSTR(2), wa_par_LPCWSTR(3)));
}

/*
DWORD WINAPI DeletePrinterKeyA(HANDLE hPrinter,LPCSTR pKeyName)
*/
HB_FUNC( WADELETEPRINTERKEYA )
{
  hb_retnl(DeletePrinterKeyA(wa_par_HANDLE(1), wa_par_LPCSTR(2)));
}

/*
DWORD WINAPI DeletePrinterKeyW(HANDLE hPrinter,LPCWSTR pKeyName)
*/
HB_FUNC( WADELETEPRINTERKEYW )
{
  hb_retnl(DeletePrinterKeyW(wa_par_HANDLE(1), wa_par_LPCWSTR(2)));
}

/*
DWORD WINAPI PrinterMessageBoxA(HANDLE hPrinter,DWORD Error,HWND hWnd,LPSTR pText,LPSTR pCaption,DWORD dwType)
*/
HB_FUNC( WAPRINTERMESSAGEBOXA )
{
  hb_retnl(PrinterMessageBoxA(wa_par_HANDLE(1), wa_par_DWORD(2), wa_par_HWND(3), const_cast<LPSTR>(hb_parc(4)), const_cast<LPSTR>(hb_parc(5)), wa_par_DWORD(6)));
}

/*
DWORD WINAPI PrinterMessageBoxW(HANDLE hPrinter,DWORD Error,HWND hWnd,LPWSTR pText,LPWSTR pCaption,DWORD dwType)
*/
HB_FUNC( WAPRINTERMESSAGEBOXW )
{
  hb_retnl(PrinterMessageBoxW(wa_par_HANDLE(1), wa_par_DWORD(2), wa_par_HWND(3), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(4))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(5))), wa_par_DWORD(6)));
}

/*
WINBOOL WINAPI ClosePrinter(HANDLE hPrinter)
*/
HB_FUNC( WACLOSEPRINTER )
{
  winapi_ret_BOOL(ClosePrinter(wa_par_HANDLE(1)));
}

/*
WINBOOL WINAPI AddFormA(HANDLE hPrinter,DWORD Level,LPBYTE pForm)
*/

/*
WINBOOL WINAPI AddFormW(HANDLE hPrinter,DWORD Level,LPBYTE pForm)
*/

/*
WINBOOL WINAPI DeleteFormA(HANDLE hPrinter,LPSTR pFormName)
*/
HB_FUNC( WADELETEFORMA )
{
  winapi_ret_BOOL(DeleteFormA(wa_par_HANDLE(1), const_cast<LPSTR>(hb_parc(2))));
}

/*
WINBOOL WINAPI DeleteFormW(HANDLE hPrinter,LPWSTR pFormName)
*/
HB_FUNC( WADELETEFORMW )
{
  winapi_ret_BOOL(DeleteFormW(wa_par_HANDLE(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2)))));
}

/*
WINBOOL WINAPI GetFormA(HANDLE hPrinter,LPSTR pFormName,DWORD Level,LPBYTE pForm,DWORD cbBuf,LPDWORD pcbNeeded)
*/

/*
WINBOOL WINAPI GetFormW(HANDLE hPrinter,LPWSTR pFormName,DWORD Level,LPBYTE pForm,DWORD cbBuf,LPDWORD pcbNeeded)
*/

/*
WINBOOL WINAPI SetFormA(HANDLE hPrinter,LPSTR pFormName,DWORD Level,LPBYTE pForm)
*/

/*
WINBOOL WINAPI SetFormW(HANDLE hPrinter,LPWSTR pFormName,DWORD Level,LPBYTE pForm)
*/

/*
WINBOOL WINAPI EnumFormsA(HANDLE hPrinter,DWORD Level,LPBYTE pForm,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
WINBOOL WINAPI EnumFormsW(HANDLE hPrinter,DWORD Level,LPBYTE pForm,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
WINBOOL WINAPI EnumMonitorsA(LPSTR pName,DWORD Level,LPBYTE pMonitor,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
WINBOOL WINAPI EnumMonitorsW(LPWSTR pName,DWORD Level,LPBYTE pMonitor,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
WINBOOL WINAPI AddMonitorA(LPSTR pName,DWORD Level,LPBYTE pMonitorInfo)
*/

/*
WINBOOL WINAPI AddMonitorW(LPWSTR pName,DWORD Level,LPBYTE pMonitorInfo)
*/

/*
WINBOOL WINAPI DeleteMonitorA(LPSTR pName,LPSTR pEnvironment,LPSTR pMonitorName)
*/
HB_FUNC( WADELETEMONITORA )
{
  winapi_ret_BOOL(DeleteMonitorA(const_cast<LPSTR>(hb_parc(1)), const_cast<LPSTR>(hb_parc(2)), const_cast<LPSTR>(hb_parc(3))));
}

/*
WINBOOL WINAPI DeleteMonitorW(LPWSTR pName,LPWSTR pEnvironment,LPWSTR pMonitorName)
*/
HB_FUNC( WADELETEMONITORW )
{
  winapi_ret_BOOL(DeleteMonitorW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3)))));
}

/*
WINBOOL WINAPI EnumPortsA(LPSTR pName,DWORD Level,LPBYTE pPorts,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
WINBOOL WINAPI EnumPortsW(LPWSTR pName,DWORD Level,LPBYTE pPorts,DWORD cbBuf,LPDWORD pcbNeeded,LPDWORD pcReturned)
*/

/*
WINBOOL WINAPI AddPortA(LPSTR pName,HWND hWnd,LPSTR pMonitorName)
*/
HB_FUNC( WAADDPORTA )
{
  winapi_ret_BOOL(AddPortA(const_cast<LPSTR>(hb_parc(1)), wa_par_HWND(2), const_cast<LPSTR>(hb_parc(3))));
}

/*
WINBOOL WINAPI AddPortW(LPWSTR pName,HWND hWnd,LPWSTR pMonitorName)
*/
HB_FUNC( WAADDPORTW )
{
  winapi_ret_BOOL(AddPortW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), wa_par_HWND(2), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3)))));
}

/*
WINBOOL WINAPI ConfigurePortA(LPSTR pName,HWND hWnd,LPSTR pPortName)
*/
HB_FUNC( WACONFIGUREPORTA )
{
  winapi_ret_BOOL(ConfigurePortA(const_cast<LPSTR>(hb_parc(1)), wa_par_HWND(2), const_cast<LPSTR>(hb_parc(3))));
}

/*
WINBOOL WINAPI ConfigurePortW(LPWSTR pName,HWND hWnd,LPWSTR pPortName)
*/
HB_FUNC( WACONFIGUREPORTW )
{
  winapi_ret_BOOL(ConfigurePortW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), wa_par_HWND(2), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3)))));
}

/*
WINBOOL WINAPI DeletePortA(LPSTR pName,HWND hWnd,LPSTR pPortName)
*/
HB_FUNC( WADELETEPORTA )
{
  winapi_ret_BOOL(DeletePortA(const_cast<LPSTR>(hb_parc(1)), wa_par_HWND(2), const_cast<LPSTR>(hb_parc(3))));
}

/*
WINBOOL WINAPI DeletePortW(LPWSTR pName,HWND hWnd,LPWSTR pPortName)
*/
HB_FUNC( WADELETEPORTW )
{
  winapi_ret_BOOL(DeletePortW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), wa_par_HWND(2), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3)))));
}

/*
WINBOOL WINAPI XcvDataW(HANDLE hXcv,PCWSTR pszDataName,PBYTE pInputData,DWORD cbInputData,PBYTE pOutputData,DWORD cbOutputData,PDWORD pcbOutputNeeded,PDWORD pdwStatus)
*/

/*
WINBOOL WINAPI GetDefaultPrinterA(LPSTR pszBuffer,LPDWORD pcchBuffer)
*/
HB_FUNC( WAGETDEFAULTPRINTERA )
{
  winapi_ret_BOOL(GetDefaultPrinterA(const_cast<LPSTR>(hb_parc(1)), static_cast<LPDWORD>(hb_parptr(2))));
}

/*
WINBOOL WINAPI GetDefaultPrinterW(LPWSTR pszBuffer,LPDWORD pcchBuffer)
*/
HB_FUNC( WAGETDEFAULTPRINTERW )
{
  winapi_ret_BOOL(GetDefaultPrinterW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), static_cast<LPDWORD>(hb_parptr(2))));
}

/*
WINBOOL WINAPI SetDefaultPrinterA(LPCSTR pszPrinter)
*/
HB_FUNC( WASETDEFAULTPRINTERA )
{
  winapi_ret_BOOL(SetDefaultPrinterA(wa_par_LPCSTR(1)));
}

/*
WINBOOL WINAPI SetDefaultPrinterW(LPCWSTR pszPrinter)
*/
HB_FUNC( WASETDEFAULTPRINTERW )
{
  winapi_ret_BOOL(SetDefaultPrinterW(wa_par_LPCWSTR(1)));
}

/*
WINBOOL WINAPI SetPortA(LPSTR pName,LPSTR pPortName,DWORD dwLevel,LPBYTE pPortInfo)
*/

/*
WINBOOL WINAPI SetPortW(LPWSTR pName,LPWSTR pPortName,DWORD dwLevel,LPBYTE pPortInfo)
*/

/*
WINBOOL WINAPI AddPrinterConnectionA(LPSTR pName)
*/
HB_FUNC( WAADDPRINTERCONNECTIONA )
{
  winapi_ret_BOOL(AddPrinterConnectionA(const_cast<LPSTR>(hb_parc(1))));
}

/*
WINBOOL WINAPI AddPrinterConnectionW(LPWSTR pName)
*/
HB_FUNC( WAADDPRINTERCONNECTIONW )
{
  winapi_ret_BOOL(AddPrinterConnectionW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1)))));
}

/*
WINBOOL WINAPI DeletePrinterConnectionA(LPSTR pName)
*/
HB_FUNC( WADELETEPRINTERCONNECTIONA )
{
  winapi_ret_BOOL(DeletePrinterConnectionA(const_cast<LPSTR>(hb_parc(1))));
}

/*
WINBOOL WINAPI DeletePrinterConnectionW(LPWSTR pName)
*/
HB_FUNC( WADELETEPRINTERCONNECTIONW )
{
  winapi_ret_BOOL(DeletePrinterConnectionW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1)))));
}

/*
HANDLE WINAPI ConnectToPrinterDlg(HWND hwnd,DWORD Flags)
*/
HB_FUNC( WACONNECTTOPRINTERDLG )
{
  hb_retptr(ConnectToPrinterDlg(wa_par_HWND(1), wa_par_DWORD(2)));
}

/*
WINBOOL WINAPI AddPrintProvidorA(LPSTR pName,DWORD level,LPBYTE pProvidorInfo)
*/

/*
WINBOOL WINAPI AddPrintProvidorW(LPWSTR pName,DWORD level,LPBYTE pProvidorInfo)
*/

/*
WINBOOL WINAPI DeletePrintProvidorA(LPSTR pName,LPSTR pEnvironment,LPSTR pPrintProvidorName)
*/
HB_FUNC( WADELETEPRINTPROVIDORA )
{
  winapi_ret_BOOL(DeletePrintProvidorA(const_cast<LPSTR>(hb_parc(1)), const_cast<LPSTR>(hb_parc(2)), const_cast<LPSTR>(hb_parc(3))));
}

/*
WINBOOL WINAPI DeletePrintProvidorW(LPWSTR pName,LPWSTR pEnvironment,LPWSTR pPrintProvidorName)
*/
HB_FUNC( WADELETEPRINTPROVIDORW )
{
  winapi_ret_BOOL(DeletePrintProvidorW(reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(1))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(3)))));
}

/*
WINBOOL WINAPI IsValidDevmodeA (PDEVMODEA pDevmode,size_t DevmodeSize)
*/

/*
WINBOOL WINAPI IsValidDevmodeW (PDEVMODEW pDevmode,size_t DevmodeSize)
*/

/*
WINBOOL AddPrinterConnection2W(HWND hWnd,LPCWSTR pszName,DWORD dwLevel,PVOID pConnectionInfo)
*/

/*
WINBOOL AddPrinterConnection2A(HWND hWnd,LPCSTR pszName,DWORD dwLevel,PVOID pConnectionInfo)
*/
/*Not supported and returns ERROR_NOT_SUPPORTED.*/

/*
HRESULT WINAPI DeletePrinterDriverPackageA(LPCSTR pszServer, LPCSTR pszInfPath, LPCSTR pszEnvironment)
*/

/*
HRESULT WINAPI DeletePrinterDriverPackageW(LPCWSTR pszServer, LPCWSTR pszInfPath, LPCWSTR pszEnvironment)
*/

/*
HRESULT DocumentEventA(HANDLE hPrinter, HDC hdc, INT iEsc, ULONG cbIn, PVOID pvIn, ULONG cbOut, PVOID pvOut)
*/

/*
HRESULT DocumentEventW(HANDLE hPrinter, HDC hdc, INT iEsc, ULONG cbIn, PVOID pvIn, ULONG cbOut, PVOID pvOut)
*/

/*
HRESULT ReportJobProcessingProgress(HANDLE printerHandle, ULONG jobId, EPrintXPSJobOperation jobOperation, EPrintXPSJobProgress jobProgress)
*/

/*
HRESULT WINAPI GetCorePrinterDriversA(LPCSTR pszServer, LPCSTR pszEnvironment, LPCSTR pszzCoreDriverDependencies, DWORD cCorePrinterDrivers, PCORE_PRINTER_DRIVERA pCorePrinterDrivers)
*/

/*
HRESULT WINAPI GetCorePrinterDriversW(LPCWSTR pszServer, LPCWSTR pszEnvironment, LPCWSTR pszzCoreDriverDependencies, DWORD cCorePrinterDrivers, PCORE_PRINTER_DRIVERW pCorePrinterDrivers)
*/

/*
WINBOOL WINAPI GetPrinterDriver2A(HWND hWnd, HANDLE hPrinter, LPSTR pEnvironment, DWORD Level, LPBYTE pDriverInfo, DWORD cbBuf, LPDWORD pcbNeeded)
*/

/*
WINBOOL WINAPI GetPrinterDriver2W(HWND hWnd, HANDLE hPrinter, LPWSTR pEnvironment, DWORD Level, LPBYTE pDriverInfo, DWORD cbBuf, LPDWORD pcbNeeded)
*/

/*
HRESULT WINAPI GetPrinterDriverPackagePathA(LPCSTR pszServer, LPCSTR pszEnvironment, LPCSTR pszLanguage, LPCSTR pszPackageID, LPSTR  pszDriverPackageCab, DWORD  cchDriverPackageCab, LPDWORD pcchRequiredSize)
*/

/*
HRESULT WINAPI GetPrinterDriverPackagePathW(LPCWSTR pszServer, LPCWSTR pszEnvironment, LPCWSTR pszLanguage, LPCWSTR pszPackageID, LPWSTR  pszDriverPackageCab, DWORD   cchDriverPackageCab, LPDWORD pcchRequiredSize)
*/

/*
HANDLE WINAPI GetSpoolFileHandleA(HANDLE hPrinter)
*/

/*
HANDLE WINAPI GetSpoolFileHandleW(HANDLE hPrinter)
*/

/*
HANDLE WINAPI CommitSpoolData(HANDLE hPrinter, HANDLE hSpoolFile, DWORD cbCommit)
*/

/*
WINBOOL WINAPI CloseSpoolFileHandle(HANDLE hPrinter, HANDLE hSpoolFile)
*/

/*
WINBOOL WINAPI OpenPrinter2A(LPCSTR pPrinterName, LPHANDLE phPrinter, LPPRINTER_DEFAULTS pDefault, PPRINTER_OPTIONS pOptions)
*/

/*
WINBOOL WINAPI OpenPrinter2W(LPCWSTR pPrinterName, LPHANDLE phPrinter, LPPRINTER_DEFAULTS pDefault, PPRINTER_OPTIONS pOptions)
*/

/*
HRESULT WINAPI UploadPrinterDriverPackageA(LPCSTR pszServer, LPCSTR pszInfPath, LPCSTR pszEnvironment, DWORD dwFlags, HWND hwnd, LPSTR pszDestInfPath, PULONG pcchDestInfPath)
*/

/*
HRESULT WINAPI UploadPrinterDriverPackageW(LPCWSTR pszServer, LPCWSTR pszInfPath, LPCWSTR pszEnvironment, DWORD dwFlags, HWND hwnd, LPWSTR pszDestInfPath, PULONG pcchDestInfPath)
*/
