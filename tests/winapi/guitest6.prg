/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

// Compile with:
// hbmk2 guitest6 guitest6.rc -gtgui -i.

// NOTE: do a right click to show the popup menu

#include "winapi_windows.ch"

#define IDR_MYMENU 101
#define IDM_FILE_DLG1 1
#define IDM_FILE_DLG2 2
#define IDM_FILE_QUIT 3

#define ID_DIALOG1 201
#define ID_DIALOG2 202

#define IDOK 1
#define IDCANCEL 2

#define MAKEINTRESOURCE(n) (n)

PROCEDURE Main()

   LOCAL wc
   LOCAL hwnd
   LOCAL CLASS_NAME := "Sample Window Class"
   LOCAL msg

   // register the window class

   wc := wasWNDCLASS()
   wc:lpfnWndProc   := GetWindowProc()
   wc:hInstance     := waGetModuleHandle(NIL)
   wc:lpszClassName := CLASS_NAME
   wc:hCursor       := waLoadCursor(NIL, IDC_ARROW)
   wc:hbrBackground := waNToP(COLOR_WINDOW + 1)
   wc:lpszMenuName  := MAKEINTRESOURCE(IDR_MYMENU)
   waRegisterClass(wc)

   // create the window

   hwnd := waCreateWindowEx(0, ;
                            CLASS_NAME, ;
                            "Testing Harbour++ and WinApi", ;
                            WS_OVERLAPPEDWINDOW, ;
                            CW_USEDEFAULT, ;
                            CW_USEDEFAULT, ;
                            CW_USEDEFAULT, ;
                            CW_USEDEFAULT, ;
                            NIL, ;
                            NIL, ;
                            waGetModuleHandle(NIL), ;
                            NIL)

   IF Empty(hwnd)
      waMessageBox(NIL, "Window creation failed", "Error", MB_ICONERROR + MB_OK)
      QUIT
   ENDIF

   waShowWindow(hwnd, SW_SHOWMAXIMIZED)
   waUpdateWindow(hwnd)

   // run the message loop

   msg := wasMSG()
   DO WHILE waGetMessage(msg, NIL, 0, 0)
      waTranslateMessage(msg)
      waDispatchMessage(msg)
   ENDDO

RETURN

FUNCTION WindowProc(hwnd, uMsg, wParam, lParam)

   LOCAL point
   LOCAL hmenu
   LOCAL nRet

   SWITCH uMsg

   CASE WM_COMMAND
      SWITCH waLOWORD(wParam)
      CASE IDM_FILE_DLG1
         nRet := waDialogBoxParam(NIL, MAKEINTRESOURCE(ID_DIALOG1), hwnd, GetDialogProc(), 0)
         IF nRet == IDOK
            waMessageBox(hwnd, "IDOK", "Dialog-1 result", MB_OK)
         ELSEIF nRet == IDCANCEL
            waMessageBox(hwnd, "IDCANCEL", "Dialog-1 result", MB_OK)
         ENDIF
         EXIT
      CASE IDM_FILE_DLG2
         nRet := waDialogBoxParam(NIL, MAKEINTRESOURCE(ID_DIALOG2), hwnd, GetDialogProc(), 0)
         IF nRet == IDOK
            waMessageBox(hwnd, "IDOK", "Dialog-2 result", MB_OK)
         ELSEIF nRet == IDCANCEL
            waMessageBox(hwnd, "IDCANCEL", "Dialog-2 result", MB_OK)
         ENDIF
         EXIT
      CASE IDM_FILE_QUIT
         IF waMessageBox(hwnd, "Close the application ?", "Question", MB_ICONQUESTION + MB_YESNO) == IDYES
            waSendMessage(hwnd, WM_CLOSE, 0, 0)
         ENDIF
      ENDSWITCH
      RETURN 0

   CASE WM_RBUTTONUP
      point := wasPOINT()
      point:x := waLOWORD(lParam)
      point:y := waHIWORD(lParam)
      hMenu := waCreatePopupMenu()
      waClientToScreen(hwnd, point)
      waAppendMenu(hMenu, MF_STRING, IDM_FILE_DLG1, "Dialog &1")
      waAppendMenu(hMenu, MF_STRING, IDM_FILE_DLG2, "Dialog &2")
      waAppendMenu(hMenu, MF_SEPARATOR, 0, NIL)
      waAppendMenu(hMenu, MF_STRING, IDM_FILE_QUIT, "&Quit")
      waTrackPopupMenu(hMenu, TPM_RIGHTBUTTON, point:x, point:y, 0, hwnd, NIL)
      waDestroyMenu(hMenu)
      RETURN 0

   CASE WM_DESTROY
      waPostQuitMessage(0)
      RETURN 0

   ENDSWITCH

RETURN waDefWindowProc(hwnd, uMsg, wParam, lParam)

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

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
INT_PTR CALLBACK DialogProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static PHB_DYNS s_pDynSym = nullptr;

  if (s_pDynSym == nullptr)
  {
    s_pDynSym = hb_dynsymGetCase("WINDOWPROC");
  }

  if (hb_dynsymIsFunction(s_pDynSym))
  {
    hb_vmPushDynSym(s_pDynSym);
    hb_vmPushNil();
    hb_vmPushPointer(hwnd);
    hb_vmPushInteger(uMsg);
    hb_vmPushLong(wParam);
    hb_vmPushNumInt(lParam);
    hb_vmDo(4);
    return hb_parnl(-1);
  }

  return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

HB_FUNC_STATIC(GETWINDOWPROC)
{
  hb_retptr(reinterpret_cast<void *>(WindowProc));
}

INT_PTR DialogProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  static PHB_DYNS s_pDynSym = nullptr;

  if (s_pDynSym == nullptr)
  {
    s_pDynSym = hb_dynsymGetCase("DIALOGPROC");
  }

  if (hb_dynsymIsFunction(s_pDynSym))
  {
    hb_vmPushDynSym(s_pDynSym);
    hb_vmPushNil();
    hb_vmPushPointer(hwnd);
    hb_vmPushInteger(uMsg);
    hb_vmPushLong(wParam);
    hb_vmPushNumInt(lParam);
    hb_vmDo(4);
    return hb_parnl(-1);
  }

  return 0;
}

HB_FUNC_STATIC(GETDIALOGPROC)
{
  hb_retptr(reinterpret_cast<void *>(DialogProc));
}

#pragma ENDDUMP
