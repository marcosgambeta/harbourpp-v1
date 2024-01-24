/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

// Compile with:
// hbmk2 guitest2 -gtgui

#include "winapi_winuser.ch"
#include "winapi_wingdi.ch"

PROCEDURE Main()

   LOCAL hwnd
   LOCAL CLASS_NAME := "Sample Window Class"
   LOCAL msg

   waGdiplusStartup()

   // register the window class

   RegisterWindowClass()

   // create the window

   hwnd := waCreateWindowEx(0, ;
                            CLASS_NAME, ;
                            "Testing Harbour++ and WinApi/GDI+", ;
                            WS_OVERLAPPEDWINDOW, ;
                            CW_USEDEFAULT, ;
                            CW_USEDEFAULT, ;
                            CW_USEDEFAULT, ;
                            CW_USEDEFAULT, ;
                            NIL, ;
                            NIL, ;
                            GetModuleHandle(), ;
                            NIL)

   IF Empty(hwnd)
      QUIT
   ENDIF

   waShowWindow(hwnd, SW_SHOWMAXIMIZED)
   waUpdateWindow(hwnd)

   // run the message loop

   msg := wasMSG():new()
   DO WHILE waGetMessage(msg, NIL, 0, 0)
      waTranslateMessage(msg)
      waDispatchMessage(msg)
   ENDDO

   waGdiplusShutdown()

RETURN

FUNCTION WindowProc(hwnd, uMsg, wParam, lParam)

   LOCAL oPS
   LOCAL pDC
   LOCAL oRect
   LOCAL pGraphics
   LOCAL pBrush

   SWITCH uMsg

   CASE WM_DESTROY
      waPostQuitMessage(0)
      RETURN 0

   CASE WM_SIZE
      waInvalidateRgn(hwnd, NIL, .T.)
      RETURN 0

   CASE WM_PAINT
      oRect := wasRECT():new()
      waGetClientRect(hwnd, oRect)
      oPS := wasPAINTSTRUCT():new()
      pDC := waBeginPaint(hwnd, oPS)
      waGdipCreateFromHDC(pDC, @pGraphics)
      waGdipCreateLineBrushI(waGpPoint():new(0, 0), waGpPoint():new(0, oRect:bottom - oRect:top), 0xFFADD8E6, 0xFF000000, NIL, @pBrush)
      waGdipFillRectangleI(pGraphics, pBrush, 0, 0, oRect:right - oRect:left, oRect:bottom - oRect:top)
      waGdipDeleteBrush(pBrush)
      waGdipDeleteGraphics(pGraphics)
      waEndPaint(hwnd, oPS)
      RETURN 0

   ENDSWITCH

RETURN waDefWindowProc(hwnd, uMsg, wParam, lParam)

#pragma BEGINDUMP

#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include "hbapi.hpp"
#include "hbvm.hpp"

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

HB_FUNC_STATIC( GETMODULEHANDLE )
{
   hb_retptr(GetModuleHandle(nullptr));
}

#pragma ENDDUMP
