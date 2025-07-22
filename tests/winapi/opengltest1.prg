//
// WinApi test
//
// Copyright (c) 2025 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// Compile with:
// hbmk2 opengltest1 -gtgui

#include <winapi_windows.ch>
#include <winapi_gl.ch>

STATIC s_hDC // Device context
STATIC s_hRC // OpenGL rendering context

PROCEDURE Main()

   LOCAL wc
   LOCAL hwnd
   LOCAL CLASS_NAME := "Sample Window Class"
   LOCAL msg
   LOCAL lQuit := .F.

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
                            "Testing Harbour++ and OpenGL", ;
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
   //waUpdateWindow(hwnd)

   // Enable OpenGL
   EnableOpenGL(hwnd)

   // Main loop
   msg := wasMSG()
   DO WHILE !lQuit
      IF waPeekMessage(msg, NIL, 0, 0, PM_REMOVE)
         IF msg:message == WM_QUIT
            lQuit := .T.
         ELSE
            waDispatchMessage(msg)
         ENDIF
      ELSE
         // OpenGL rendering
         waglClearColor(0.1, 0.2, 0.3, 1.0)
         waglClear(GL_COLOR_BUFFER_BIT)
         waSwapBuffers(s_hDC)
         Sleep(1) // Limit CPU usage
      ENDIF
   ENDDO

   // Shutdown
   DisableOpenGL(hwnd)

RETURN

FUNCTION WindowProc(hwnd, uMsg, wParam, lParam)

   SWITCH uMsg

   CASE WM_CLOSE
      waPostQuitMessage(0)
      RETURN 0

   CASE WM_DESTROY
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

// void Sleep([in] DWORD dwMilliseconds)
HB_FUNC_STATIC(SLEEP)
{
  Sleep(wa_par_DWORD(1));
}

#pragma ENDDUMP

// Enable OpenGL
STATIC FUNCTION EnableOpenGL(hWnd)

   LOCAL pfd := wasPIXELFORMATDESCRIPTOR()
   LOCAL format

   // Get device context
   s_hDC := waGetDC(hWnd)

   // Set pixel format
   //ZeroMemory(&pfd, sizeof(pfd));
   //pfd.nSize = sizeof(pfd);
   pfd:nVersion := 1
   pfd:dwFlags := hb_bitor(PFD_DRAW_TO_WINDOW, PFD_SUPPORT_OPENGL, PFD_DOUBLEBUFFER)
   pfd:iPixelType := PFD_TYPE_RGBA
   pfd:cColorBits := 24
   pfd:cDepthBits := 16
   pfd:iLayerType := PFD_MAIN_PLANE

   format := waChoosePixelFormat(s_hDC, pfd)
   waSetPixelFormat(s_hDC, format, pfd)

   // Create and activate OpenGL context
   s_hRC := wawglCreateContext(s_hDC)
   wawglMakeCurrent(s_hDC, s_hRC)

RETURN NIL

// Disable OpenGL
STATIC FUNCTION DisableOpenGL(hWnd)

   wawglMakeCurrent(NIL, NIL)
   wawglDeleteContext(s_hRC)
   waReleaseDC(hWnd, s_hDC)

RETURN NIL
