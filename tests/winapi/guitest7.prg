/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

// Compile with:
// hbmk2 guitest7 guitest7.rc -gtgui -i.

#include "winapi_windows.ch"

#define ID_DIALOG 101

#define IDOK 1
#define IDCANCEL 2

#define MAKEINTRESOURCE(n) (n)

PROCEDURE Main()

   LOCAL nRet

   nRet := waDialogBoxParam(NIL, MAKEINTRESOURCE(ID_DIALOG), NIL, GetDialogProc(), 0)

   IF nRet == IDOK
      waMessageBox(NIL, "IDOK", "Dialog result", MB_OK)
   ELSEIF nRet == IDCANCEL
      waMessageBox(NIL, "IDCANCEL", "Dialog result", MB_OK)
   ENDIF

RETURN

FUNCTION DialogProc(hwnd, uMsg, wParam, lParam)

   SWITCH uMsg

   CASE WM_INITDIALOG
      RETURN 1

   CASE WM_COMMAND
      SWITCH waLOWORD(wParam)
      CASE IDOK
         waEndDialog(hwnd, IDOK)
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

INT_PTR CALLBACK DialogProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

INT_PTR DialogProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS s_pDynSym = nullptr;

   if( s_pDynSym == nullptr ) {
      s_pDynSym = hb_dynsymGetCase("DIALOGPROC");
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

HB_FUNC_STATIC( GETDIALOGPROC )
{
   hb_retptr(reinterpret_cast<void*>(DialogProc));
}

#pragma ENDDUMP