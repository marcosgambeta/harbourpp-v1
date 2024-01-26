/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

// Compile with:
// hbmk2 guitest3 -gtgui

// NOTE: do a right click to show the popup menu

#include "winapi_winuser.ch"
#include "winapi_wingdi.ch"

#define IDM_FILE_NEW  1
#define IDM_FILE_OPEN 2
#define IDM_FILE_QUIT 3

PROCEDURE Main()

   LOCAL hwnd
   LOCAL CLASS_NAME := "Sample Window Class"
   LOCAL msg

   // register the window class

   RegisterWindowClass()

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

   LOCAL point
   LOCAL hmenu

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

   CASE WM_RBUTTONUP
      point := wasPOINT()
      point:x := waLOWORD(lParam)
      point:y := waHIWORD(lParam)
      hMenu := waCreatePopupMenu()
      waClientToScreen(hwnd, point)
      waAppendMenu(hMenu, MF_STRING, IDM_FILE_NEW, "&New")
      waAppendMenu(hMenu, MF_STRING, IDM_FILE_OPEN, "&Open")
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
#include "winapi.hpp"
#include "hbapi.hpp"
#include "hbvm.hpp"
#include "hbwinuni.hpp"

static PHB_DYNS s_pDynSym = nullptr;

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
   if( s_pDynSym == nullptr ) {
      s_pDynSym = hb_dynsymGetCase("WINDOWPROC");
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

   return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

// auxiliary functions

HB_FUNC_STATIC( REGISTERWINDOWCLASS )
{
   WNDCLASS wc{};
   wc.lpfnWndProc   = WindowProc;
   wc.hInstance     = GetModuleHandle(nullptr);
   wc.lpszClassName = L"Sample Window Class";
   wc.hCursor       = LoadCursor(nullptr, IDC_ARROW);
   wc.hbrBackground = reinterpret_cast<HBRUSH>(COLOR_WINDOW + 1);
   RegisterClass(&wc);
}

#pragma ENDDUMP
