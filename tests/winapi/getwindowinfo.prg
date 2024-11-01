//
// WinApi test
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// Compile with:
// hbmk2 getwindowinfo

#include <hbgtinfo.ch>

REQUEST HB_GT_WVT_DEFAULT

PROCEDURE Main()

   LOCAL pWinHandle
   LOCAL oWindowInfo

   CLS

   pWinHandle := hb_GtInfo(HB_GTI_WINHANDLE)
   ? "pWinHandle=", pWinHandle

   ?

   oWindowInfo := wasWINDOWINFO()
   ? "waGetWindowInfo=", waGetWindowInfo(pWinHandle, oWindowInfo)
   ? "oWindowInfo=", oWindowInfo:ptr

   ?

   ? "cbSize.........=", oWindowInfo:cbSize
   //RECT  rcWindow; // TODO:
   //RECT  rcClient; // TODO:
   ? "dwStyle........=", oWindowInfo:dwStyle
   ? "dwExStyle......=", oWindowInfo:dwExStyle
   ? "dwWindowStatus.=", oWindowInfo:dwWindowStatus
   ? "cxWindowBorders=", oWindowInfo:cxWindowBorders
   ? "cyWindowBorders=", oWindowInfo:cyWindowBorders
   ? "atomWindowType.=", oWindowInfo:atomWindowType
   ? "wCreatorVersion=", oWindowInfo:wCreatorVersion

   WAIT

RETURN
