//
// WinApi test
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

#include <winapi_winuser.ch>

PROCEDURE Main()

   ? "SM_CXSCREEN...................=", waGetSystemMetrics(SM_CXSCREEN)
   ? "SM_CYSCREEN...................=", waGetSystemMetrics(SM_CYSCREEN)
   ? "SM_CXVSCROLL..................=", waGetSystemMetrics(SM_CXVSCROLL)
   ? "SM_CYHSCROLL..................=", waGetSystemMetrics(SM_CYHSCROLL)
   ? "SM_CYCAPTION..................=", waGetSystemMetrics(SM_CYCAPTION)
   ? "SM_CXBORDER...................=", waGetSystemMetrics(SM_CXBORDER)
   ? "SM_CYBORDER...................=", waGetSystemMetrics(SM_CYBORDER)
   ? "SM_CXDLGFRAME.................=", waGetSystemMetrics(SM_CXDLGFRAME)
   ? "SM_CYDLGFRAME.................=", waGetSystemMetrics(SM_CYDLGFRAME)
   ? "SM_CYVTHUMB...................=", waGetSystemMetrics(SM_CYVTHUMB)
   ? "SM_CXHTHUMB...................=", waGetSystemMetrics(SM_CXHTHUMB)
   ? "SM_CXICON.....................=", waGetSystemMetrics(SM_CXICON)
   ? "SM_CYICON.....................=", waGetSystemMetrics(SM_CYICON)
   ? "SM_CXCURSOR...................=", waGetSystemMetrics(SM_CXCURSOR)
   ? "SM_CYCURSOR...................=", waGetSystemMetrics(SM_CYCURSOR)
   ? "SM_CYMENU.....................=", waGetSystemMetrics(SM_CYMENU)
   ? "SM_CXFULLSCREEN...............=", waGetSystemMetrics(SM_CXFULLSCREEN)
   ? "SM_CYFULLSCREEN...............=", waGetSystemMetrics(SM_CYFULLSCREEN)
   ? "SM_CYKANJIWINDOW..............=", waGetSystemMetrics(SM_CYKANJIWINDOW)
   ? "SM_MOUSEPRESENT...............=", waGetSystemMetrics(SM_MOUSEPRESENT)
   ? "SM_CYVSCROLL..................=", waGetSystemMetrics(SM_CYVSCROLL)
   ? "SM_CXHSCROLL..................=", waGetSystemMetrics(SM_CXHSCROLL)
   ? "SM_DEBUG......................=", waGetSystemMetrics(SM_DEBUG)
   ? "SM_SWAPBUTTON.................=", waGetSystemMetrics(SM_SWAPBUTTON)
   ? "SM_RESERVED1..................=", waGetSystemMetrics(SM_RESERVED1)
   ? "SM_RESERVED2..................=", waGetSystemMetrics(SM_RESERVED2)
   ? "SM_RESERVED3..................=", waGetSystemMetrics(SM_RESERVED3)
   ? "SM_RESERVED4..................=", waGetSystemMetrics(SM_RESERVED4)
   ? "SM_CXMIN......................=", waGetSystemMetrics(SM_CXMIN)
   ? "SM_CYMIN......................=", waGetSystemMetrics(SM_CYMIN)
   ? "SM_CXSIZE.....................=", waGetSystemMetrics(SM_CXSIZE)
   ? "SM_CYSIZE.....................=", waGetSystemMetrics(SM_CYSIZE)
   ? "SM_CXFRAME....................=", waGetSystemMetrics(SM_CXFRAME)
   ? "SM_CYFRAME....................=", waGetSystemMetrics(SM_CYFRAME)
   ? "SM_CXMINTRACK.................=", waGetSystemMetrics(SM_CXMINTRACK)
   ? "SM_CYMINTRACK.................=", waGetSystemMetrics(SM_CYMINTRACK)
   ? "SM_CXDOUBLECLK................=", waGetSystemMetrics(SM_CXDOUBLECLK)
   ? "SM_CYDOUBLECLK................=", waGetSystemMetrics(SM_CYDOUBLECLK)
   ? "SM_CXICONSPACING..............=", waGetSystemMetrics(SM_CXICONSPACING)
   ? "SM_CYICONSPACING..............=", waGetSystemMetrics(SM_CYICONSPACING)
   ? "SM_MENUDROPALIGNMENT..........=", waGetSystemMetrics(SM_MENUDROPALIGNMENT)
   ? "SM_PENWINDOWS.................=", waGetSystemMetrics(SM_PENWINDOWS)
   ? "SM_DBCSENABLED................=", waGetSystemMetrics(SM_DBCSENABLED)
   ? "SM_CMOUSEBUTTONS..............=", waGetSystemMetrics(SM_CMOUSEBUTTONS)
   ? "SM_CXFIXEDFRAME...............=", waGetSystemMetrics(SM_CXFIXEDFRAME)
   ? "SM_CYFIXEDFRAME...............=", waGetSystemMetrics(SM_CYFIXEDFRAME)
   ? "SM_CXSIZEFRAME................=", waGetSystemMetrics(SM_CXSIZEFRAME)
   ? "SM_CYSIZEFRAME................=", waGetSystemMetrics(SM_CYSIZEFRAME)
   ? "SM_SECURE.....................=", waGetSystemMetrics(SM_SECURE)
   ? "SM_CXEDGE.....................=", waGetSystemMetrics(SM_CXEDGE)
   ? "SM_CYEDGE.....................=", waGetSystemMetrics(SM_CYEDGE)
   ? "SM_CXMINSPACING...............=", waGetSystemMetrics(SM_CXMINSPACING)
   ? "SM_CYMINSPACING...............=", waGetSystemMetrics(SM_CYMINSPACING)
   ? "SM_CXSMICON...................=", waGetSystemMetrics(SM_CXSMICON)
   ? "SM_CYSMICON...................=", waGetSystemMetrics(SM_CYSMICON)
   ? "SM_CYSMCAPTION................=", waGetSystemMetrics(SM_CYSMCAPTION)
   ? "SM_CXSMSIZE...................=", waGetSystemMetrics(SM_CXSMSIZE)
   ? "SM_CYSMSIZE...................=", waGetSystemMetrics(SM_CYSMSIZE)
   ? "SM_CXMENUSIZE.................=", waGetSystemMetrics(SM_CXMENUSIZE)
   ? "SM_CYMENUSIZE.................=", waGetSystemMetrics(SM_CYMENUSIZE)
   ? "SM_ARRANGE....................=", waGetSystemMetrics(SM_ARRANGE)
   ? "SM_CXMINIMIZED................=", waGetSystemMetrics(SM_CXMINIMIZED)
   ? "SM_CYMINIMIZED................=", waGetSystemMetrics(SM_CYMINIMIZED)
   ? "SM_CXMAXTRACK.................=", waGetSystemMetrics(SM_CXMAXTRACK)
   ? "SM_CYMAXTRACK.................=", waGetSystemMetrics(SM_CYMAXTRACK)
   ? "SM_CXMAXIMIZED................=", waGetSystemMetrics(SM_CXMAXIMIZED)
   ? "SM_CYMAXIMIZED................=", waGetSystemMetrics(SM_CYMAXIMIZED)
   ? "SM_NETWORK....................=", waGetSystemMetrics(SM_NETWORK)
   ? "SM_CLEANBOOT..................=", waGetSystemMetrics(SM_CLEANBOOT)
   ? "SM_CXDRAG.....................=", waGetSystemMetrics(SM_CXDRAG)
   ? "SM_CYDRAG.....................=", waGetSystemMetrics(SM_CYDRAG)
   ? "SM_SHOWSOUNDS.................=", waGetSystemMetrics(SM_SHOWSOUNDS)
   ? "SM_CXMENUCHECK................=", waGetSystemMetrics(SM_CXMENUCHECK)
   ? "SM_CYMENUCHECK................=", waGetSystemMetrics(SM_CYMENUCHECK)
   ? "SM_SLOWMACHINE................=", waGetSystemMetrics(SM_SLOWMACHINE)
   ? "SM_MIDEASTENABLED.............=", waGetSystemMetrics(SM_MIDEASTENABLED)
   ? "SM_MOUSEWHEELPRESENT..........=", waGetSystemMetrics(SM_MOUSEWHEELPRESENT)
   ? "SM_XVIRTUALSCREEN.............=", waGetSystemMetrics(SM_XVIRTUALSCREEN)
   ? "SM_YVIRTUALSCREEN.............=", waGetSystemMetrics(SM_YVIRTUALSCREEN)
   ? "SM_CXVIRTUALSCREEN............=", waGetSystemMetrics(SM_CXVIRTUALSCREEN)
   ? "SM_CYVIRTUALSCREEN............=", waGetSystemMetrics(SM_CYVIRTUALSCREEN)
   ? "SM_CMONITORS..................=", waGetSystemMetrics(SM_CMONITORS)
   ? "SM_SAMEDISPLAYFORMAT..........=", waGetSystemMetrics(SM_SAMEDISPLAYFORMAT)
   ? "SM_IMMENABLED.................=", waGetSystemMetrics(SM_IMMENABLED)
   ? "SM_CXFOCUSBORDER..............=", waGetSystemMetrics(SM_CXFOCUSBORDER)
   ? "SM_CYFOCUSBORDER..............=", waGetSystemMetrics(SM_CYFOCUSBORDER)
   ? "SM_TABLETPC...................=", waGetSystemMetrics(SM_TABLETPC)
   ? "SM_MEDIACENTER................=", waGetSystemMetrics(SM_MEDIACENTER)
   ? "SM_STARTER....................=", waGetSystemMetrics(SM_STARTER)
   ? "SM_SERVERR2...................=", waGetSystemMetrics(SM_SERVERR2)
   ? "SM_MOUSEHORIZONTALWHEELPRESENT=", waGetSystemMetrics(SM_MOUSEHORIZONTALWHEELPRESENT)
   ? "SM_CXPADDEDBORDER.............=", waGetSystemMetrics(SM_CXPADDEDBORDER)
   ? "SM_DIGITIZER..................=", waGetSystemMetrics(SM_DIGITIZER)
   ? "SM_MAXIMUMTOUCHES.............=", waGetSystemMetrics(SM_MAXIMUMTOUCHES)
   // ? "SM_CMETRICS...................=", waGetSystemMetrics(SM_CMETRICS)
   ? "SM_REMOTESESSION..............=", waGetSystemMetrics(SM_REMOTESESSION)
   ? "SM_SHUTTINGDOWN...............=", waGetSystemMetrics(SM_SHUTTINGDOWN)
   ? "SM_REMOTECONTROL..............=", waGetSystemMetrics(SM_REMOTECONTROL)
   ? "SM_CARETBLINKINGENABLED.......=", waGetSystemMetrics(SM_CARETBLINKINGENABLED)
   ? "SM_CONVERTIBLESLATEMODE.......=", waGetSystemMetrics(SM_CONVERTIBLESLATEMODE)
   ? "SM_SYSTEMDOCKED...............=", waGetSystemMetrics(SM_SYSTEMDOCKED)
   
   WAIT

RETURN
