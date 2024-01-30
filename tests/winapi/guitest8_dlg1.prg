/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

#include "winapi_windows.ch"

#include "guitest8.h"

#define MAKEINTRESOURCE(n) (n)

FUNCTION ShowDialog1(hwnd)

   LOCAL nRet

   nRet := waDialogBoxParam(NIL, MAKEINTRESOURCE(ID_DIALOG1), hwnd, GetDialogProc1(), 0)

   IF nRet == IDOK
      waMessageBox(hwnd, "IDOK", "Dialog-1 result", MB_OK)
   ELSEIF nRet == IDCANCEL
      waMessageBox(hwnd, "IDCANCEL", "Dialog-1 result", MB_OK)
   ENDIF

RETURN NIL

FUNCTION DialogProc1(hwnd, uMsg, wParam, lParam)

   SWITCH uMsg

   CASE WM_INITDIALOG
      RETURN 1

   CASE WM_COMMAND
      SWITCH waLOWORD(wParam)
      CASE IDOK
         IF waMessageBox(hwnd, "Confirm ?", "Question", MB_ICONQUESTION + MB_YESNO) == IDYES
            waEndDialog(hwnd, IDOK)
         ENDIF
         EXIT
      CASE IDCANCEL
         waEndDialog(hwnd, IDCANCEL)
      ENDSWITCH
      RETURN 1

   ENDSWITCH

RETURN 0

#pragma BEGINDUMP

#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include "winapi.hpp"
#include "hbapi.hpp"
#include "hbvm.hpp"
#include "hbwinuni.hpp"
#include "guitest6.h"

INT_PTR CALLBACK DialogProc1(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

INT_PTR DialogProc1(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS s_pDynSym = nullptr;

   if( s_pDynSym == nullptr ) {
      s_pDynSym = hb_dynsymGetCase("DIALOGPROC1");
   }

   if( hb_dynsymIsFunction(s_pDynSym) ) {
      hb_vmPushDynSym(s_pDynSym);
      hb_vmPushNil();
      hb_vmPushPointer(hwnd);
      hb_vmPushInteger(uMsg);
      hb_vmPushLong(wParam);
      hb_vmPushLong(lParam);
      hb_vmDo(4);
      return hb_parnl(-1);
   }

   return 0;
}

HB_FUNC_STATIC( GETDIALOGPROC1 )
{
   hb_retptr(reinterpret_cast<void*>(DialogProc1));
}

#pragma ENDDUMP
