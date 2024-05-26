/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

#include "winapi_windows.ch"

#include "guitest8.h"

#define MAKEINTRESOURCE(n) (n)

FUNCTION ShowDialog2(hwnd)

   LOCAL nRet

   nRet := waDialogBoxParam(NIL, MAKEINTRESOURCE(ID_DIALOG2), hwnd, GetDialogProc2(), 0)

   IF nRet == IDOK
      waMessageBox(hwnd, "IDOK", "Dialog-2 result", MB_OK)
   ELSEIF nRet == IDCANCEL
      waMessageBox(hwnd, "IDCANCEL", "Dialog-2 result", MB_OK)
   ENDIF

RETURN NIL

FUNCTION DialogProc2(hwnd, uMsg, wParam, lParam)

   LOCAL handle

   SWITCH uMsg

   CASE WM_INITDIALOG
      // configure field 1
      waSendMessage(waGetDlgItem(hwnd, ID_EDIT1_DLG2), EM_SETLIMITTEXT, 40, 0)
      waSetDlgItemText(hwnd, ID_EDIT1_DLG2, "field1")
      // configure field 2
      waSendMessage(waGetDlgItem(hwnd, ID_EDIT2_DLG2), EM_SETLIMITTEXT, 40, 0)
      waSetDlgItemText(hwnd, ID_EDIT2_DLG2, "field2")
      // configure field 3
      handle := waGetDlgItem(hwnd, ID_EDIT3_DLG2)
      waSendMessage(handle, CB_ADDSTRING, 0, "Item 1")
      waSendMessage(handle, CB_ADDSTRING, 0, "Item 2")
      waSendMessage(handle, CB_ADDSTRING, 0, "Item 3")
      waSendMessage(handle, CB_ADDSTRING, 0, "Item 4")
      waSendMessage(handle, CB_ADDSTRING, 0, "Item 5")
      waSendMessage(handle, CB_SETCURSEL, 0, 0)
      // configure field 4
      waSendMessage(waGetDlgItem(hwnd, ID_EDIT4_DLG2), EM_SETLIMITTEXT, 512, 0)
      waSetDlgItemText(hwnd, ID_EDIT4_DLG2, memoread("guitest8.txt"))
      //
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

INT_PTR CALLBACK DialogProc2(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

INT_PTR CALLBACK DialogProc2(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static PHB_DYNS s_pDynSym = nullptr;

  if (s_pDynSym == nullptr)
  {
    s_pDynSym = hb_dynsymGetCase("DIALOGPROC2");
  }

  if (hb_dynsymIsFunction(s_pDynSym))
  {
    hb_vmPushDynSym(s_pDynSym);
    hb_vmPushNil();
    hb_vmPushPointer(hwnd);
    hb_vmPushInteger(uMsg);
    hb_vmPushNumInt(wParam);
    hb_vmPushNumInt(lParam);
    hb_vmDo(4);
    return hb_parnl(-1);
  }

  return 0;
}

HB_FUNC_STATIC(GETDIALOGPROC2)
{
  hb_retptr(reinterpret_cast<void *>(DialogProc2));
}

#pragma ENDDUMP
