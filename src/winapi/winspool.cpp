/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (C) 2022 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2022 Marcos Antonio Gambeta

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
#include "winapi.h"

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
HB_FUNC( WINAPI_DELETEPRINTER )
{
  winapi_ret_BOOL(DeletePrinter(winapi_par_HANDLE(1)));
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
HB_FUNC( WINAPI_DELETEPRINTERDRIVERA )
{
  winapi_ret_BOOL(DeletePrinterDriverA(( LPSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), ( LPSTR ) hb_parc(3)));
}

/*
WINBOOL WINAPI DeletePrinterDriverW(LPWSTR pName,LPWSTR pEnvironment,LPWSTR pDriverName)
*/
HB_FUNC( WINAPI_DELETEPRINTERDRIVERW )
{
  winapi_ret_BOOL(DeletePrinterDriverW(( LPWSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), ( LPWSTR ) hb_parc(3)));
}

/*
WINBOOL WINAPI DeletePrinterDriverExA(LPSTR pName,LPSTR pEnvironment,LPSTR pDriverName,DWORD dwDeleteFlag,DWORD dwVersionFlag)
*/
HB_FUNC( WINAPI_DELETEPRINTERDRIVEREXA )
{
  winapi_ret_BOOL(DeletePrinterDriverExA(( LPSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), ( LPSTR ) hb_parc(3), winapi_par_DWORD(4), winapi_par_DWORD(5)));
}

/*
WINBOOL WINAPI DeletePrinterDriverExW(LPWSTR pName,LPWSTR pEnvironment,LPWSTR pDriverName,DWORD dwDeleteFlag,DWORD dwVersionFlag)
*/
HB_FUNC( WINAPI_DELETEPRINTERDRIVEREXW )
{
  winapi_ret_BOOL(DeletePrinterDriverExW(( LPWSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), ( LPWSTR ) hb_parc(3), winapi_par_DWORD(4), winapi_par_DWORD(5)));
}

/*
WINBOOL WINAPI AddPrintProcessorA(LPSTR pName,LPSTR pEnvironment,LPSTR pPathName,LPSTR pPrintProcessorName)
*/
HB_FUNC( WINAPI_ADDPRINTPROCESSORA )
{
  winapi_ret_BOOL(AddPrintProcessorA(( LPSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), ( LPSTR ) hb_parc(3), ( LPSTR ) hb_parc(4)));
}

/*
WINBOOL WINAPI AddPrintProcessorW(LPWSTR pName,LPWSTR pEnvironment,LPWSTR pPathName,LPWSTR pPrintProcessorName)
*/
HB_FUNC( WINAPI_ADDPRINTPROCESSORW )
{
  winapi_ret_BOOL(AddPrintProcessorW(( LPWSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), ( LPWSTR ) hb_parc(3), ( LPWSTR ) hb_parc(4)));
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
HB_FUNC( WINAPI_DELETEPRINTPROCESSORA )
{
  winapi_ret_BOOL(DeletePrintProcessorA(( LPSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), ( LPSTR ) hb_parc(3)));
}

/*
WINBOOL WINAPI DeletePrintProcessorW(LPWSTR pName,LPWSTR pEnvironment,LPWSTR pPrintProcessorName)
*/
HB_FUNC( WINAPI_DELETEPRINTPROCESSORW )
{
  winapi_ret_BOOL(DeletePrintProcessorW(( LPWSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), ( LPWSTR ) hb_parc(3)));
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
HB_FUNC( WINAPI_STARTPAGEPRINTER )
{
  winapi_ret_BOOL(StartPagePrinter(winapi_par_HANDLE(1)));
}

/*
WINBOOL WINAPI WritePrinter(HANDLE hPrinter,LPVOID pBuf,DWORD cbBuf,LPDWORD pcWritten)
*/
HB_FUNC( WINAPI_WRITEPRINTER )
{
  winapi_ret_BOOL(WritePrinter(winapi_par_HANDLE(1), static_cast<LPVOID>(hb_parptr(2)), winapi_par_DWORD(3), static_cast<LPDWORD>(hb_parptr(4))));
}

/*
WINBOOL WINAPI FlushPrinter(HANDLE hPrinter,LPVOID pBuf,DWORD cbBuf,LPDWORD pcWritten,DWORD cSleep)
*/
HB_FUNC( WINAPI_FLUSHPRINTER )
{
  winapi_ret_BOOL(FlushPrinter(winapi_par_HANDLE(1), static_cast<LPVOID>(hb_parptr(2)), winapi_par_DWORD(3), static_cast<LPDWORD>(hb_parptr(4)), winapi_par_DWORD(5)));
}

/*
WINBOOL WINAPI EndPagePrinter(HANDLE hPrinter)
*/
HB_FUNC( WINAPI_ENDPAGEPRINTER )
{
  winapi_ret_BOOL(EndPagePrinter(winapi_par_HANDLE(1)));
}

/*
WINBOOL WINAPI AbortPrinter(HANDLE hPrinter)
*/
HB_FUNC( WINAPI_ABORTPRINTER )
{
  winapi_ret_BOOL(AbortPrinter(winapi_par_HANDLE(1)));
}

/*
WINBOOL WINAPI ReadPrinter(HANDLE hPrinter,LPVOID pBuf,DWORD cbBuf,LPDWORD pNoBytesRead)
*/
HB_FUNC( WINAPI_READPRINTER )
{
  winapi_ret_BOOL(ReadPrinter(winapi_par_HANDLE(1), static_cast<LPVOID>(hb_parptr(2)), winapi_par_DWORD(3), static_cast<LPDWORD>(hb_parptr(4))));
}

/*
WINBOOL WINAPI EndDocPrinter(HANDLE hPrinter)
*/
HB_FUNC( WINAPI_ENDDOCPRINTER )
{
  winapi_ret_BOOL(EndDocPrinter(winapi_par_HANDLE(1)));
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
HB_FUNC( WINAPI_SCHEDULEJOB )
{
  winapi_ret_BOOL(ScheduleJob(winapi_par_HANDLE(1), winapi_par_DWORD(2)));
}

/*
WINBOOL WINAPI PrinterProperties(HWND hWnd,HANDLE hPrinter)
*/
HB_FUNC( WINAPI_PRINTERPROPERTIES )
{
  winapi_ret_BOOL(PrinterProperties(winapi_par_HWND(1), winapi_par_HANDLE(2)));
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
HB_FUNC( WINAPI_ENUMPRINTERKEYA )
{
  hb_retnl(EnumPrinterKeyA(winapi_par_HANDLE(1), ( LPCSTR ) hb_parc(2), ( LPSTR ) hb_parc(3), winapi_par_DWORD(4), static_cast<LPDWORD>(hb_parptr(5))));
}

/*
DWORD WINAPI EnumPrinterKeyW(HANDLE hPrinter,LPCWSTR pKeyName,LPWSTR pSubkey,DWORD cbSubkey,LPDWORD pcbSubkey)
*/
HB_FUNC( WINAPI_ENUMPRINTERKEYW )
{
  hb_retnl(EnumPrinterKeyW(winapi_par_HANDLE(1), ( LPCWSTR ) hb_parc(2), ( LPWSTR ) hb_parc(3), winapi_par_DWORD(4), static_cast<LPDWORD>(hb_parptr(5))));
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
HB_FUNC( WINAPI_DELETEPRINTERDATAA )
{
  hb_retnl(DeletePrinterDataA(winapi_par_HANDLE(1), ( LPSTR ) hb_parc(2)));
}

/*
DWORD WINAPI DeletePrinterDataW(HANDLE hPrinter,LPWSTR pValueName)
*/
HB_FUNC( WINAPI_DELETEPRINTERDATAW )
{
  hb_retnl(DeletePrinterDataW(winapi_par_HANDLE(1), ( LPWSTR ) hb_parc(2)));
}

/*
DWORD WINAPI DeletePrinterDataExA(HANDLE hPrinter,LPCSTR pKeyName,LPCSTR pValueName)
*/
HB_FUNC( WINAPI_DELETEPRINTERDATAEXA )
{
  hb_retnl(DeletePrinterDataExA(winapi_par_HANDLE(1), ( LPCSTR ) hb_parc(2), ( LPCSTR ) hb_parc(3)));
}

/*
DWORD WINAPI DeletePrinterDataExW(HANDLE hPrinter,LPCWSTR pKeyName,LPCWSTR pValueName)
*/
HB_FUNC( WINAPI_DELETEPRINTERDATAEXW )
{
  hb_retnl(DeletePrinterDataExW(winapi_par_HANDLE(1), ( LPCWSTR ) hb_parc(2), ( LPCWSTR ) hb_parc(3)));
}

/*
DWORD WINAPI DeletePrinterKeyA(HANDLE hPrinter,LPCSTR pKeyName)
*/
HB_FUNC( WINAPI_DELETEPRINTERKEYA )
{
  hb_retnl(DeletePrinterKeyA(winapi_par_HANDLE(1), ( LPCSTR ) hb_parc(2)));
}

/*
DWORD WINAPI DeletePrinterKeyW(HANDLE hPrinter,LPCWSTR pKeyName)
*/
HB_FUNC( WINAPI_DELETEPRINTERKEYW )
{
  hb_retnl(DeletePrinterKeyW(winapi_par_HANDLE(1), ( LPCWSTR ) hb_parc(2)));
}

/*
DWORD WINAPI PrinterMessageBoxA(HANDLE hPrinter,DWORD Error,HWND hWnd,LPSTR pText,LPSTR pCaption,DWORD dwType)
*/
HB_FUNC( WINAPI_PRINTERMESSAGEBOXA )
{
  hb_retnl(PrinterMessageBoxA(winapi_par_HANDLE(1), winapi_par_DWORD(2), winapi_par_HWND(3), ( LPSTR ) hb_parc(4), ( LPSTR ) hb_parc(5), winapi_par_DWORD(6)));
}

/*
DWORD WINAPI PrinterMessageBoxW(HANDLE hPrinter,DWORD Error,HWND hWnd,LPWSTR pText,LPWSTR pCaption,DWORD dwType)
*/
HB_FUNC( WINAPI_PRINTERMESSAGEBOXW )
{
  hb_retnl(PrinterMessageBoxW(winapi_par_HANDLE(1), winapi_par_DWORD(2), winapi_par_HWND(3), ( LPWSTR ) hb_parc(4), ( LPWSTR ) hb_parc(5), winapi_par_DWORD(6)));
}

/*
WINBOOL WINAPI ClosePrinter(HANDLE hPrinter)
*/
HB_FUNC( WINAPI_CLOSEPRINTER )
{
  winapi_ret_BOOL(ClosePrinter(winapi_par_HANDLE(1)));
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
HB_FUNC( WINAPI_DELETEFORMA )
{
  winapi_ret_BOOL(DeleteFormA(winapi_par_HANDLE(1), ( LPSTR ) hb_parc(2)));
}

/*
WINBOOL WINAPI DeleteFormW(HANDLE hPrinter,LPWSTR pFormName)
*/
HB_FUNC( WINAPI_DELETEFORMW )
{
  winapi_ret_BOOL(DeleteFormW(winapi_par_HANDLE(1), ( LPWSTR ) hb_parc(2)));
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
HB_FUNC( WINAPI_DELETEMONITORA )
{
  winapi_ret_BOOL(DeleteMonitorA(( LPSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), ( LPSTR ) hb_parc(3)));
}

/*
WINBOOL WINAPI DeleteMonitorW(LPWSTR pName,LPWSTR pEnvironment,LPWSTR pMonitorName)
*/
HB_FUNC( WINAPI_DELETEMONITORW )
{
  winapi_ret_BOOL(DeleteMonitorW(( LPWSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), ( LPWSTR ) hb_parc(3)));
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
HB_FUNC( WINAPI_ADDPORTA )
{
  winapi_ret_BOOL(AddPortA(( LPSTR ) hb_parc(1), winapi_par_HWND(2), ( LPSTR ) hb_parc(3)));
}

/*
WINBOOL WINAPI AddPortW(LPWSTR pName,HWND hWnd,LPWSTR pMonitorName)
*/
HB_FUNC( WINAPI_ADDPORTW )
{
  winapi_ret_BOOL(AddPortW(( LPWSTR ) hb_parc(1), winapi_par_HWND(2), ( LPWSTR ) hb_parc(3)));
}

/*
WINBOOL WINAPI ConfigurePortA(LPSTR pName,HWND hWnd,LPSTR pPortName)
*/
HB_FUNC( WINAPI_CONFIGUREPORTA )
{
  winapi_ret_BOOL(ConfigurePortA(( LPSTR ) hb_parc(1), winapi_par_HWND(2), ( LPSTR ) hb_parc(3)));
}

/*
WINBOOL WINAPI ConfigurePortW(LPWSTR pName,HWND hWnd,LPWSTR pPortName)
*/
HB_FUNC( WINAPI_CONFIGUREPORTW )
{
  winapi_ret_BOOL(ConfigurePortW(( LPWSTR ) hb_parc(1), winapi_par_HWND(2), ( LPWSTR ) hb_parc(3)));
}

/*
WINBOOL WINAPI DeletePortA(LPSTR pName,HWND hWnd,LPSTR pPortName)
*/
HB_FUNC( WINAPI_DELETEPORTA )
{
  winapi_ret_BOOL(DeletePortA(( LPSTR ) hb_parc(1), winapi_par_HWND(2), ( LPSTR ) hb_parc(3)));
}

/*
WINBOOL WINAPI DeletePortW(LPWSTR pName,HWND hWnd,LPWSTR pPortName)
*/
HB_FUNC( WINAPI_DELETEPORTW )
{
  winapi_ret_BOOL(DeletePortW(( LPWSTR ) hb_parc(1), winapi_par_HWND(2), ( LPWSTR ) hb_parc(3)));
}

/*
WINBOOL WINAPI XcvDataW(HANDLE hXcv,PCWSTR pszDataName,PBYTE pInputData,DWORD cbInputData,PBYTE pOutputData,DWORD cbOutputData,PDWORD pcbOutputNeeded,PDWORD pdwStatus)
*/

/*
WINBOOL WINAPI GetDefaultPrinterA(LPSTR pszBuffer,LPDWORD pcchBuffer)
*/
HB_FUNC( WINAPI_GETDEFAULTPRINTERA )
{
  winapi_ret_BOOL(GetDefaultPrinterA(( LPSTR ) hb_parc(1), static_cast<LPDWORD>(hb_parptr(2))));
}

/*
WINBOOL WINAPI GetDefaultPrinterW(LPWSTR pszBuffer,LPDWORD pcchBuffer)
*/
HB_FUNC( WINAPI_GETDEFAULTPRINTERW )
{
  winapi_ret_BOOL(GetDefaultPrinterW(( LPWSTR ) hb_parc(1), static_cast<LPDWORD>(hb_parptr(2))));
}

/*
WINBOOL WINAPI SetDefaultPrinterA(LPCSTR pszPrinter)
*/
HB_FUNC( WINAPI_SETDEFAULTPRINTERA )
{
  winapi_ret_BOOL(SetDefaultPrinterA(( LPCSTR ) hb_parc(1)));
}

/*
WINBOOL WINAPI SetDefaultPrinterW(LPCWSTR pszPrinter)
*/
HB_FUNC( WINAPI_SETDEFAULTPRINTERW )
{
  winapi_ret_BOOL(SetDefaultPrinterW(( LPCWSTR ) hb_parc(1)));
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
HB_FUNC( WINAPI_ADDPRINTERCONNECTIONA )
{
  winapi_ret_BOOL(AddPrinterConnectionA(( LPSTR ) hb_parc(1)));
}

/*
WINBOOL WINAPI AddPrinterConnectionW(LPWSTR pName)
*/
HB_FUNC( WINAPI_ADDPRINTERCONNECTIONW )
{
  winapi_ret_BOOL(AddPrinterConnectionW(( LPWSTR ) hb_parc(1)));
}

/*
WINBOOL WINAPI DeletePrinterConnectionA(LPSTR pName)
*/
HB_FUNC( WINAPI_DELETEPRINTERCONNECTIONA )
{
  winapi_ret_BOOL(DeletePrinterConnectionA(( LPSTR ) hb_parc(1)));
}

/*
WINBOOL WINAPI DeletePrinterConnectionW(LPWSTR pName)
*/
HB_FUNC( WINAPI_DELETEPRINTERCONNECTIONW )
{
  winapi_ret_BOOL(DeletePrinterConnectionW(( LPWSTR ) hb_parc(1)));
}

/*
HANDLE WINAPI ConnectToPrinterDlg(HWND hwnd,DWORD Flags)
*/
HB_FUNC( WINAPI_CONNECTTOPRINTERDLG )
{
  hb_retptr(ConnectToPrinterDlg(winapi_par_HWND(1), winapi_par_DWORD(2)));
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
HB_FUNC( WINAPI_DELETEPRINTPROVIDORA )
{
  winapi_ret_BOOL(DeletePrintProvidorA(( LPSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), ( LPSTR ) hb_parc(3)));
}

/*
WINBOOL WINAPI DeletePrintProvidorW(LPWSTR pName,LPWSTR pEnvironment,LPWSTR pPrintProvidorName)
*/
HB_FUNC( WINAPI_DELETEPRINTPROVIDORW )
{
  winapi_ret_BOOL(DeletePrintProvidorW(( LPWSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), ( LPWSTR ) hb_parc(3)));
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
