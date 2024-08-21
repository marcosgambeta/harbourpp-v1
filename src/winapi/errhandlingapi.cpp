//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

/*
MIT License

Copyright (c) 2024 Marcos Antonio Gambeta

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

// NOTE: source code generated with the help of a code generator

#include <windows.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

// WINBASEAPI UINT WINAPI SetErrorMode (UINT uMode)
HB_FUNC( WASETERRORMODE )
{
  wa_ret_UINT(SetErrorMode(wa_par_UINT(1)));
}

// WINBASEAPI LPTOP_LEVEL_EXCEPTION_FILTER WINAPI SetUnhandledExceptionFilter (LPTOP_LEVEL_EXCEPTION_FILTER lpTopLevelExceptionFilter)

// WINBASEAPI LONG WINAPI UnhandledExceptionFilter (struct _EXCEPTION_POINTERS *ExceptionInfo)

// WINBASEAPI PVOID WINAPI AddVectoredExceptionHandler (ULONG First, PVECTORED_EXCEPTION_HANDLER Handler)

// WINBASEAPI ULONG WINAPI RemoveVectoredExceptionHandler (PVOID Handle)

// WINBASEAPI PVOID WINAPI AddVectoredContinueHandler (ULONG First, PVECTORED_EXCEPTION_HANDLER Handler)

// WINBASEAPI ULONG WINAPI RemoveVectoredContinueHandler (PVOID Handle)

// #if _WIN32_WINNT >= 0x0600
// WINBASEAPI UINT WINAPI GetErrorMode (VOID)
// #endif
#if 0
HB_FUNC( WAGETERRORMODE )
{
  wa_ret_UINT(GetErrorMode());
}
#endif

// WINBASEAPI VOID WINAPI RestoreLastError (DWORD dwErrCode)

// WINBASEAPI VOID WINAPI RaiseException (DWORD dwExceptionCode, DWORD dwExceptionFlags, DWORD nNumberOfArguments, CONST ULONG_PTR *lpArguments)

// WINBASEAPI DWORD WINAPI GetLastError (VOID)
HB_FUNC( WAGETLASTERROR )
{
  wa_ret_DWORD(GetLastError());
}

// WINBASEAPI VOID WINAPI SetLastError (DWORD dwErrCode)
HB_FUNC( WASETLASTERROR )
{
  SetLastError(wa_par_DWORD(1));
}
