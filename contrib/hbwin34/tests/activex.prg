#require "hbwin34"

#include <hbgtinfo.ch>
#include <hbclass.ch>

#if !defined(__HBSCRIPT__HBSHELL)
REQUEST HB_GT_WVT_DEFAULT
#endif

PROCEDURE Main()

   LOCAL oMSCAL

#if defined(__HBSCRIPT__HBSHELL) .AND. defined(__PLATFORM__WINDOWS)
   hbshell_gtSelect( "GTWVT" )
#endif

   ? "ActiveX demo"  /* do not remove this line, we need it to have the window appear */
   oMSCAL := HActiveX():Init( hb_gtInfo( HB_GTI_WINHANDLE ), "MSCAL.Calendar", 0, 0, 300, 300 )
   WAIT "Press any key to exit"

   HB_SYMBOL_UNUSED(oMSCAL)

   RETURN

CREATE CLASS HActiveX

   VAR oOLE
   VAR hWnd
   METHOD Init( hWnd, cProgId, nTop, nLeft, nWidth, nHeight, cID )
   METHOD Event(...)
   ERROR HANDLER OnError()
   DESTRUCTOR Close()

ENDCLASS

METHOD HActiveX:Init( hWnd, cProgId, nTop, nLeft, nWidth, nHeight, cID )

   LOCAL nStyle := WIN_WS_CHILD + WIN_WS_VISIBLE + WIN_WS_CLIPCHILDREN

   win_axInit()
   ::hWnd := wapi_CreateWindowEx( 0, "AtlAxWin", cProgId, nStyle, nLeft, nTop, nWidth, nHeight, hWnd, 0 )
#if 0
   wapi_SetWindowPos( ::hWnd, WIN_HWND_TOPMOST, 0, 0, 1, 1, hb_bitOr( WIN_SWP_NOSIZE, WIN_SWP_DRAWFRAME ) )
#endif
   ::oOLE := win_axGetControl( ::hWnd, {|event, ...| ::Event(event, ...) }, cID )

   RETURN Self

PROCEDURE HActiveX:Event(...)

   LOCAL cEvents := ""
   LOCAL aEvents := {...}

   AEval(aEvents, {| xEvent | cEvents += hb_ValToStr(xEvent) + ", " })
   wapi_OutputDebugString( cEvents )

   RETURN

METHOD HActiveX:OnError()
   RETURN hb_ExecFromArray( ::oOLE, __GetMessage(), hb_AParams() )

METHOD PROCEDURE HActiveX:Close()

   wapi_OutputDebugString( "Close" )
   wapi_DestroyWindow( ::hWnd )
   ::hWnd := NIL
   ::oOLE := NIL
   wapi_OutputDebugString( "After Close" )

   RETURN
