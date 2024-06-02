/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

// Compile with:
// hbmk2 gdiptest3 -gtgui

#include <winapi_windows.ch>

PROCEDURE Main()

   LOCAL wc
   LOCAL hwnd
   LOCAL CLASS_NAME := "Sample Window Class"
   LOCAL msg

   waGdiplusStartup()

   // register the window class

   wc := wasWNDCLASS()
   wc:lpfnWndProc   := GetWindowProc()
   wc:hInstance     := waGetModuleHandle(NIL)
   wc:lpszClassName := CLASS_NAME
   wc:hCursor       := waLoadCursor(NIL, IDC_ARROW)
   wc:hbrBackground := waNtoP(COLOR_WINDOW + 1)
   waRegisterClass(wc)

   // create the window

   hwnd := waCreateWindowEx(0, ;
                            CLASS_NAME, ;
                            "Testing Harbour++ and WinApi/GDI+", ;
                            WS_OVERLAPPEDWINDOW, ;
                            CW_USEDEFAULT, ;
                            CW_USEDEFAULT, ;
                            682, ;
                            340, ;
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

   waGdiplusShutdown()

RETURN

FUNCTION WindowProc(hwnd, uMsg, wParam, lParam)

   LOCAL oPS
   LOCAL pDC
   LOCAL oRect
   LOCAL pGraphics
   LOCAL pImage

   SWITCH uMsg

   CASE WM_DESTROY
      waPostQuitMessage(0)
      RETURN 0

   CASE WM_SIZE
      waInvalidateRgn(hwnd, NIL, .T.)
      RETURN 0

   CASE WM_PAINT
      oRect := wasRECT()
      waGetClientRect(hwnd, oRect)
      oPS := wasPAINTSTRUCT()
      pDC := waBeginPaint(hwnd, oPS)
      waGdipCreateFromHDC(pDC, @pGraphics)
      waGdipLoadImageFromFile("harbour.gif", @pImage)
      waGdipDrawImageRect(pGraphics, pImage, 0, 0, oRect:right - oRect:left, oRect:bottom - oRect:top)
      waGdipDisposeImage(pImage)
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
#include <hbapi.hpp>
#include <hbvm.hpp>
#include <winapi.hpp>

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
