/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

#include "winapi_windows.ch"

#include "guitest8.h"

#define MAKEINTRESOURCE(n) (n)

STATIC aFields

FUNCTION ShowDialog1(hwnd)

   LOCAL nRet
   LOCAL cMsg

   aFields := array(5)

   nRet := waDialogBoxParam(NIL, MAKEINTRESOURCE(ID_DIALOG1), hwnd, GetDialogProc1(), 0)

   IF nRet == IDOK
      cMsg := ""
      cMsg += "Field1: " + aFields[1] + Chr(13) + Chr(10)
      cMsg += "Field2: " + aFields[2] + Chr(13) + Chr(10)
      cMsg += "Field3: " + aFields[3] + Chr(13) + Chr(10)
      cMsg += "Field4: " + aFields[4] + Chr(13) + Chr(10)
      cMsg += "Field5: " + aFields[5] + Chr(13) + Chr(10)
      waMessageBox(hwnd, cMsg, "Dialog-1 result", MB_OK)
   ELSEIF nRet == IDCANCEL
      waMessageBox(hwnd, "IDCANCEL", "Dialog-1 result", MB_OK)
   ENDIF

RETURN NIL

FUNCTION DialogProc1(hwnd, uMsg, wParam, lParam)

   SWITCH uMsg

   CASE WM_INITDIALOG
      waSendMessage(waGetDlgItem(hwnd, ID_EDIT1_DLG1), EM_SETLIMITTEXT, 40, 0)
      waSendMessage(waGetDlgItem(hwnd, ID_EDIT2_DLG1), EM_SETLIMITTEXT, 40, 0)
      waSendMessage(waGetDlgItem(hwnd, ID_EDIT3_DLG1), EM_SETLIMITTEXT, 40, 0)
      waSendMessage(waGetDlgItem(hwnd, ID_EDIT4_DLG1), EM_SETLIMITTEXT, 40, 0)
      waSendMessage(waGetDlgItem(hwnd, ID_EDIT5_DLG1), EM_SETLIMITTEXT, 40, 0)
      RETURN 1

   CASE WM_COMMAND
      SWITCH waLOWORD(wParam)
      CASE IDOK
         IF waMessageBox(hwnd, "Confirm ?", "Question", MB_ICONQUESTION + MB_YESNO) == IDYES
            aFields[1] := GetText(hwnd, ID_EDIT1_DLG1)
            aFields[2] := GetText(hwnd, ID_EDIT2_DLG1)
            aFields[3] := GetText(hwnd, ID_EDIT3_DLG1)
            aFields[4] := GetText(hwnd, ID_EDIT4_DLG1)
            aFields[5] := GetText(hwnd, ID_EDIT5_DLG1)
            waEndDialog(hwnd, IDOK)
         ENDIF
         EXIT
      CASE IDCANCEL
         waEndDialog(hwnd, IDCANCEL)
      ENDSWITCH
      RETURN 1

   ENDSWITCH

RETURN 0

STATIC FUNCTION GetText(hwnd, id)

   LOCAL cBuffer := space(40)

   waGetDlgItemText(hwnd, id, @cBuffer, len(cBuffer))

RETURN cBuffer

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

INT_PTR CALLBACK DialogProc1(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static PHB_DYNS s_pDynSym = nullptr;

  if (s_pDynSym == nullptr)
  {
    s_pDynSym = hb_dynsymGetCase("DIALOGPROC1");
  }

  if (hb_dynsymIsFunction(s_pDynSym))
  {
    hb_vmPushDynSym(s_pDynSym);
    hb_vmPushNil();
    wa_vmPushHWND(hwnd);
    wa_vmPushUINT(uMsg);
    wa_vmPushWPARAM(wParam);
    wa_vmPushLPARAM(lParam);
    hb_vmDo(4);
    return wa_par_INT_PTR(-1);
  }

  return 0;
}

HB_FUNC_STATIC(GETDIALOGPROC1)
{
  wa_ret_DLGPROC(DialogProc1);
}

#pragma ENDDUMP
