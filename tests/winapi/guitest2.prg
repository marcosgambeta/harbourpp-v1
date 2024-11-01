//
// WinApi test
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// Compile with:
// hbmk2 guitest2 -gtgui

#include <winapi_windows.ch>

#define IDM_FILE_NEW  1
#define IDM_FILE_OPEN 2
#define IDM_FILE_QUIT 3

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

   waShowWindow(hwnd, SW_SHOW)
   waUpdateWindow(hwnd)

   // run the message loop

   msg := wasMSG()
   DO WHILE waGetMessage(msg, NIL, 0, 0)
      waTranslateMessage(msg)
      waDispatchMessage(msg)
   ENDDO

RETURN

FUNCTION WindowProc(hwnd, uMsg, wParam, lParam)

   SWITCH uMsg

   CASE WM_CREATE
      CreateMenus(hwnd)
      RETURN 0

   CASE WM_COMMAND
      SWITCH waLOWORD(wParam)
      CASE IDM_FILE_NEW
         waMessageBox(hwnd, "IDM_FILE_NEW", "Option", MB_OK)
         EXIT
      CASE IDM_FILE_OPEN
         waMessageBox(hwnd, "IDM_FILE_OPEN", "Option", MB_OK)
         EXIT
      CASE IDM_FILE_QUIT
         waSendMessage(hwnd, WM_CLOSE, 0, 0)
      ENDSWITCH
      RETURN 0

   CASE WM_DESTROY
      waPostQuitMessage(0)
      RETURN 0

   ENDSWITCH

RETURN waDefWindowProc(hwnd, uMsg, wParam, lParam)

STATIC FUNCTION CreateMenus(hwnd)

   LOCAL hMenubar
   LOCAL hMenu

   hMenubar := waCreateMenu()
   hMenu := waCreateMenu()

   waAppendMenu(hMenu, MF_STRING, IDM_FILE_NEW, "&New")
   waAppendMenu(hMenu, MF_STRING, IDM_FILE_OPEN, "&Open")
   waAppendMenu(hMenu, MF_SEPARATOR, 0, NIL)
   waAppendMenu(hMenu, MF_STRING, IDM_FILE_QUIT, "&Quit")

   waAppendMenu(hMenubar, MF_POPUP, hMenu, "&File")
   waSetMenu(hwnd, hMenubar)

RETURN NIL

#pragma BEGINDUMP

#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <winapi.hpp>
#include <hbapi.hpp>
#include <hbvm.hpp>
#include <hbwinuni.hpp>

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
    wa_vmPushHWND(hwnd);
    wa_vmPushUINT(uMsg);
    wa_vmPushWPARAM(wParam);
    wa_vmPushLPARAM(lParam);
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
