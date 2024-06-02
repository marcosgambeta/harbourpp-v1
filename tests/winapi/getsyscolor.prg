/*
 * WinApi test
 *
 * Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

#include <winapi_winuser.ch>

PROCEDURE Main()

   ? "COLOR_SCROLLBAR..............=", waGetSysColor(COLOR_SCROLLBAR)
   ? "COLOR_BACKGROUND.............=", waGetSysColor(COLOR_BACKGROUND)
   ? "COLOR_ACTIVECAPTION..........=", waGetSysColor(COLOR_ACTIVECAPTION)
   ? "COLOR_INACTIVECAPTION........=", waGetSysColor(COLOR_INACTIVECAPTION)
   ? "COLOR_MENU...................=", waGetSysColor(COLOR_MENU)
   ? "COLOR_WINDOW.................=", waGetSysColor(COLOR_WINDOW)
   ? "COLOR_WINDOWFRAME............=", waGetSysColor(COLOR_WINDOWFRAME)
   ? "COLOR_MENUTEXT...............=", waGetSysColor(COLOR_MENUTEXT)
   ? "COLOR_WINDOWTEXT.............=", waGetSysColor(COLOR_WINDOWTEXT)
   ? "COLOR_CAPTIONTEXT............=", waGetSysColor(COLOR_CAPTIONTEXT)
   ? "COLOR_ACTIVEBORDER...........=", waGetSysColor(COLOR_ACTIVEBORDER)
   ? "COLOR_INACTIVEBORDER.........=", waGetSysColor(COLOR_INACTIVEBORDER)
   ? "COLOR_APPWORKSPACE...........=", waGetSysColor(COLOR_APPWORKSPACE)
   ? "COLOR_HIGHLIGHT..............=", waGetSysColor(COLOR_HIGHLIGHT)
   ? "COLOR_HIGHLIGHTTEXT..........=", waGetSysColor(COLOR_HIGHLIGHTTEXT)
   ? "COLOR_BTNFACE................=", waGetSysColor(COLOR_BTNFACE)
   ? "COLOR_BTNSHADOW..............=", waGetSysColor(COLOR_BTNSHADOW)
   ? "COLOR_GRAYTEXT...............=", waGetSysColor(COLOR_GRAYTEXT)
   ? "COLOR_BTNTEXT................=", waGetSysColor(COLOR_BTNTEXT)
   ? "COLOR_INACTIVECAPTIONTEXT....=", waGetSysColor(COLOR_INACTIVECAPTIONTEXT)
   ? "COLOR_BTNHIGHLIGHT...........=", waGetSysColor(COLOR_BTNHIGHLIGHT)
   ? "COLOR_3DDKSHADOW.............=", waGetSysColor(COLOR_3DDKSHADOW)
   ? "COLOR_3DLIGHT................=", waGetSysColor(COLOR_3DLIGHT)
   ? "COLOR_INFOTEXT...............=", waGetSysColor(COLOR_INFOTEXT)
   ? "COLOR_INFOBK.................=", waGetSysColor(COLOR_INFOBK)
   ? "COLOR_HOTLIGHT...............=", waGetSysColor(COLOR_HOTLIGHT)
   ? "COLOR_GRADIENTACTIVECAPTION..=", waGetSysColor(COLOR_GRADIENTACTIVECAPTION)
   ? "COLOR_GRADIENTINACTIVECAPTION=", waGetSysColor(COLOR_GRADIENTINACTIVECAPTION)
   ? "COLOR_MENUHILIGHT............=", waGetSysColor(COLOR_MENUHILIGHT)
   ? "COLOR_MENUBAR................=", waGetSysColor(COLOR_MENUBAR)
   
   WAIT

RETURN
