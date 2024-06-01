/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

// Compile with:
// hbmk2 guitest8.hbp

// NOTE: do a right click to show the popup menu

#include "winapi_windows.ch"

#include "guitest8.h"

#define MAKEINTRESOURCE(n) (n)

REQUEST HB_CODEPAGE_UTF8EX

PROCEDURE Main()

   LOCAL wc
   LOCAL hwnd
   LOCAL CLASS_NAME := "Sample Window Class"
   LOCAL msg
   
   hb_cdpSelect("UTF8EX")

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

   SWITCH uMsg

   CASE WM_COMMAND
      SWITCH waLOWORD(wParam)
      CASE IDM_FILE_DLG1
         ShowDialog1(hwnd)
         EXIT
      CASE IDM_FILE_DLG2
         ShowDialog2(hwnd)
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
    hb_vmPushNumInt(wParam);
    hb_vmPushNumInt(lParam);
    hb_vmDo(4);
    return wa_par_LRESULT(-1);
  }

  return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

HB_FUNC_STATIC(GETWINDOWPROC)
{
  wa_ret_WNDPROC(WindowProc);
}

#pragma ENDDUMP
