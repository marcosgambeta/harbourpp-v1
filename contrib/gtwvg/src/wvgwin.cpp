/*
 * Video subsystem for Windows using GUI windows instead of Console
 *
 *    Copyright 2007-2012 Pritpal Bedi <bedipritpal@hotmail.com>
 * based on:
 *
 *    Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *   Video subsystem for Windows compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* Direct WinApi Functions - Prefixed wvg_*() */

#include "gtwvg.h"
#include "hbwapi.h"

#include <windowsx.h>

#if ! defined( GCLP_HBRBACKGROUND )
#  define GCLP_HBRBACKGROUND     -10
#endif

#if ! defined( CB_GETCOMBOBOXINFO )
#  define CB_GETCOMBOBOXINFO     0x0164
#endif

#define WIN_STATUSBAR_MAX_PARTS  256

#define wvg_parwparam( n )    ( ( WPARAM ) ( HB_PTRUINT ) ( hb_parnint( n ) ) )
#define wvg_parlparam( n )    ( ( LPARAM ) ( HB_PTRUINT ) ( hb_parnint( n ) ) )
#define wvg_parhandle( n )    ( ( HANDLE ) ( HB_PTRUINT ) ( hb_parnint( n ) ) )
#define wvg_parhwnd( n )      ( ( HWND ) ( HB_PTRUINT ) ( hb_parnint( n ) ) )
#define wvg_parwndproc( n )   ( ( WNDPROC ) ( HB_PTRUINT ) ( hb_parnint( n ) ) )
#define wvg_parhdc( n )       ( ( HDC ) ( HB_PTRUINT ) ( hb_parnint( n ) ) )
#define wvg_parcolor( n )     ( ( COLORREF ) ( HB_PTRUINT ) ( hb_parnint( n ) ) )

#define wvg_rethandle( n )    ( hb_retnint( ( HB_PTRUINT ) ( n ) ) )

#if defined( __BORLANDC__ ) && ! defined( HB_ARCH_64BIT )
    #undef MAKELONG
    #define MAKELONG( a, b )  ( ( LONG ) ( ( static_cast< WORD >( static_cast< DWORD_PTR >( a ) & 0xffff ) ) | \
                                           ( ( static_cast< DWORD >( static_cast< WORD >( static_cast< DWORD_PTR >( b ) & 0xffff ) ) ) << 16 ) ) )
#endif

static HINSTANCE wvg_hInstance( void )
{
   HANDLE hInstance;

   hb_winmainArgGet( &hInstance, nullptr, nullptr );

   return ( HINSTANCE ) hInstance;
}

HB_FUNC( WVG_SENDMESSAGE )
{
   void *  hText  = nullptr;
   HB_SIZE nLen   = 0;
   LPCTSTR lpText = HB_PARSTR( 4, &hText, &nLen );

   if( lpText && HB_ISBYREF( 4 ) )
   {
      lpText = HB_STRUNSHARE( &hText, lpText, nLen );
   }

   hb_retnl( static_cast< HB_ULONG >( SendMessage( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ),
                                      static_cast< UINT >( hb_parni( 2 ) ),
                                      ( ! HB_ISNUM( 3 ) ? 0 : static_cast< WPARAM >( hb_parnint( 3 ) ) ),
                                      ( lpText ? reinterpret_cast< LPARAM >( lpText ) : static_cast< LPARAM >( hb_parnint( 4 ) ) ) ) ) );
   if( lpText )
   {
      HB_STORSTR( lpText, 4 );
      hb_strfree( hText );
   }
}

HB_FUNC( WVG_SENDDLGITEMMESSAGE )
{
   PHB_ITEM pText = hb_param( 5, HB_IT_STRING );
   char *   cText = nullptr;
   HB_ISIZ  iLen  = 0;

   if( pText )
   {
      iLen  = hb_itemGetCLen( pText );
      cText = static_cast< char * >( hb_xgrab( iLen + 1 ) );
      hb_xmemcpy( cText, hb_itemGetCPtr( pText ), iLen + 1 );
   }

   hb_retnl( static_cast< long >( SendDlgItemMessage( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ),
                                  static_cast< int >( hb_parni( 2 ) ),
                                  static_cast< UINT >( hb_parni( 3 ) ),
                                  static_cast< WPARAM >( hb_parnint( 4 ) ),
                                  ( cText ? reinterpret_cast< LPARAM >( cText ) : static_cast< LPARAM >( hb_parnint( 5 ) ) )
                                  ) ) );

   if( cText )
   {
      hb_storclen( cText, iLen, 5 );
      hb_xfree( cText );
   }
}

/*
 *  win_SetTimer( hWnd, nIdentifier, nTimeOut )
 */
HB_FUNC( WVG_SETTIMER )
{
   hb_retl( SetTimer( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), hb_parni( 2 ), hb_parni( 3 ), nullptr ) != 0 );
}

HB_FUNC( WVG_SETFOCUS )
{
   SetFocus( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ) );
}

HB_FUNC( WVG_GETFOCUS )
{
   hb_retnint( reinterpret_cast< HB_PTRUINT >( GetFocus() ) );
}

HB_FUNC( WVG_SETTEXTCOLOR )
{
   hb_retnl( static_cast< HB_ULONG >( SetTextColor( reinterpret_cast< HDC >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), static_cast< COLORREF >( hb_parnl( 2 ) ) ) ) );
}

HB_FUNC( WVG_SETBKCOLOR )
{
   hb_retnl( static_cast< HB_ULONG >( SetBkColor( reinterpret_cast< HDC >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), static_cast< COLORREF >( hb_parnl( 2 ) ) ) ) );
}

HB_FUNC( WVG_SETBKMODE )
{
   hb_retni( static_cast< int >( SetBkMode( reinterpret_cast< HDC >(static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), hb_parni( 2 ) ) ) );
}

HB_FUNC( WVG_GETSTOCKOBJECT )
{
   hb_retnint( reinterpret_cast< HB_PTRUINT >( GetStockObject( hb_parni( 1 ) ) ) );
}

HB_FUNC( WVG_DELETEOBJECT )
{
   hb_retl( DeleteObject( reinterpret_cast< HGDIOBJ >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ) ) );
}

HB_FUNC( WVG_SELECTOBJECT )
{
   hb_retnint( reinterpret_cast< HB_PTRUINT >( SelectObject( reinterpret_cast< HDC >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), reinterpret_cast< HGDIOBJ >( static_cast< HB_PTRUINT >( hb_parnint( 2 ) ) ) ) ) );
}

HB_FUNC( WVG_LOWORD )
{
   hb_retnl( LOWORD( hb_parnl( 1 ) ) );
}

HB_FUNC( WVG_HIWORD )
{
   hb_retnl( HIWORD( hb_parnl( 1 ) ) );
}

HB_FUNC( WVG_GETDIALOGBASEUNITS )
{
   hb_retnl( static_cast< long >( GetDialogBaseUnits() ) );
}

HB_FUNC( WVG_SETDLGITEMTEXT )
{
   void * hText;

   SetDlgItemText( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), hb_parni( 2 ), HB_PARSTR( 3, &hText, nullptr ) );
   hb_strfree( hText );
}

HB_FUNC( WVG_GETDLGITEMTEXT )
{
   int    iLen  = static_cast< int >( SendMessage( GetDlgItem( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), hb_parni( 2 ) ), WM_GETTEXTLENGTH, 0, 0 ) ) + 1;
   LPTSTR cText = static_cast< LPTSTR >( hb_xgrab( iLen * sizeof( TCHAR ) ) );
   UINT   iResult;

   iResult = GetDlgItemText( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ),   /* handle of dialog box */
                             hb_parni( 2 ),                             /* identifier of control      */
                             cText,                                     /* address of buffer for text */
                             iLen                                       /* maximum size of string     */
                             );

   cText[ iResult ] = '\0';
   HB_RETSTR( cText );
   hb_xfree( cText );
}

HB_FUNC( WVG_CHECKDLGBUTTON )
{
   hb_retl( CheckDlgButton( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), hb_parni( 2 ),
                            static_cast< UINT >( ( HB_ISNUM( 3 ) ) ? hb_parni( 3 ) : hb_parl( 3 ) ) ) );
}

HB_FUNC( WVG_ISDLGBUTTONCHECKED )
{
   hb_retni( IsDlgButtonChecked( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), hb_parni( 2 ) ) );
}

HB_FUNC( WVG_CHECKRADIOBUTTON )
{
   hb_retl( CheckRadioButton( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ),  /* handle of dialog box */
                              hb_parni( 2 ),                            /* identifier of first radio button in group */
                              hb_parni( 3 ),                            /* identifier of last radio button in group  */
                              hb_parni( 4 )                             /* identifier of radio button to select      */
                              ) );
}

HB_FUNC( WVG_GETDLGITEM )
{
   hb_retnint( reinterpret_cast< HB_PTRUINT >( GetDlgItem( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), hb_parni( 2 ) ) ) );
}

HB_FUNC( WVG_MESSAGEBOX )
{
   HWND   hWnd = HB_ISNUM( 1 ) ? reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ) : GetActiveWindow();
   void * hMsg;
   void * hTitle;

   hb_retni( MessageBox( hWnd, HB_PARSTR( 2, &hMsg, nullptr ), HB_PARSTR( 3, &hTitle, nullptr ), hb_parnidef( 4, MB_OK ) ) );
   hb_strfree( hMsg );
   hb_strfree( hTitle );
}

HB_FUNC( WVG_INVALIDATERECT )
{
   if( HB_ISARRAY( 2 ) )
   {
      RECT rc = { 0, 0, 0, 0 };

      rc.left   = hb_parvni( 2, 1 );
      rc.top    = hb_parvni( 2, 2 );
      rc.right  = hb_parvni( 2, 3 );
      rc.bottom = hb_parvni( 2, 4 );

      hb_retl( InvalidateRect( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), &rc, TRUE ) );
   }
   else
   {
      hb_retl( InvalidateRect( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), nullptr, TRUE ) );
   }
}

/*
 *  win_LoadIcon( ncIcon )
 */
HB_FUNC( WVG_LOADICON )
{
   HICON hIcon;

   if( HB_ISNUM( 1 ) )
   {
      hIcon = LoadIcon( static_cast< HINSTANCE >( wvg_hInstance() ), MAKEINTRESOURCE( hb_parni( 1 ) ) );
   }
   else
   {
      void * hBuffer;
      hIcon = static_cast< HICON >( LoadImage( static_cast< HINSTANCE >( nullptr ), HB_PARSTR( 1, &hBuffer, nullptr ), IMAGE_ICON, 0, 0, LR_LOADFROMFILE ) );
      hb_strfree( hBuffer );
   }

   hb_retnint( reinterpret_cast< HB_PTRUINT >( hIcon ) );
}

/*
 *  win_LoadImage( ncImage, nSource, nBmpOrIcon, nWidth, nHeight ) -> hImage
 *    nSource == 0 ResourceIdByNumber
 *    nSource == 1 ResourceIdByName
 *    nSource == 2 ImageFromDiskFile
 */
HB_FUNC( WVG_LOADIMAGE )
{
   HANDLE  hImage = 0;
   void *  hBuffer;
   LPCTSTR lpBuffer = HB_PARSTR( 1, &hBuffer, nullptr );
   int     iSource  = hb_parni( 2 );

   switch( iSource )
   {
      case 0:   /* Image from resource by numeric id */
         if( HB_ISNUM( 3 ) && hb_parni( 3 ) == IMAGE_ICON )
         {
            hImage = LoadIcon( static_cast< HINSTANCE >( wvg_hInstance() ), MAKEINTRESOURCE( hb_parni( 1 ) ) );
         }
         else
         {
            hImage = LoadBitmap( static_cast< HINSTANCE >( wvg_hInstance() ), MAKEINTRESOURCE( hb_parni( 1 ) ) );
         }
         break;

      case 1:   /* image from resource by name */
         if( HB_ISNUM( 3 ) && hb_parni( 3 ) == IMAGE_ICON )
         {
            hImage = LoadIcon( static_cast< HINSTANCE >( wvg_hInstance() ), lpBuffer );
         }
         else
         {
            hImage = LoadBitmap( static_cast< HINSTANCE >( wvg_hInstance() ), lpBuffer );
         }
         break;

      case 2:   /* Image from disk file */
         if( HB_ISNUM( 3 ) && hb_parni( 3 ) == IMAGE_ICON )
         {
            hImage = static_cast< HICON >( LoadImage( static_cast< HINSTANCE >( nullptr ), lpBuffer, IMAGE_ICON, hb_parni( 4 ), hb_parni( 5 ), LR_LOADFROMFILE ) );
         }
         else
         {
            hImage = static_cast< HBITMAP >( LoadImage( static_cast< HINSTANCE >( nullptr ), lpBuffer, IMAGE_BITMAP, hb_parni( 4 ), hb_parni( 5 ), LR_LOADFROMFILE ) );
         }
         break;
   }

   hb_strfree( hBuffer );
   hb_retnint( reinterpret_cast< HB_PTRUINT >( hImage ) );
}

HB_FUNC( WVG_GETCLIENTRECT )
{
   RECT     rc   = { 0, 0, 0, 0 };
   PHB_ITEM info = hb_itemArrayNew( 4 );

   GetClientRect( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), &rc );

   hb_arraySetNI( info, 1, rc.left   );
   hb_arraySetNI( info, 2, rc.top    );
   hb_arraySetNI( info, 3, rc.right  );
   hb_arraySetNI( info, 4, rc.bottom );

   hb_itemReturnRelease( info );
}

/*
 *    Win_DrawImage( hdc, nLeft, nTop, nWidth, nHeight, cImage, lDoNotScale ) in Pixels
 */
HB_FUNC( WVG_DRAWIMAGE )
{
   void * hImage;

   hb_retl( hb_wvt_DrawImage( reinterpret_cast< HDC >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), hb_parni( 2 ), hb_parni( 3 ),
                              hb_parni( 4 ), hb_parni( 5 ), HB_PARSTR( 6, &hImage, nullptr ), hb_parl( 7 ) ) );
   hb_strfree( hImage );
}

HB_FUNC( WVG_GETDC )
{
   hb_retnint( reinterpret_cast< HB_PTRUINT >( GetDC( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ) ) ) );
}

HB_FUNC( WVG_RELEASEDC )
{
   hb_retl( ReleaseDC( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), reinterpret_cast< HDC >( static_cast< HB_PTRUINT >( hb_parnint( 2 ) ) ) ) );
}

HB_FUNC( WVG_CREATEBRUSH )
{
   LOGBRUSH lb = { 0, 0, 0 };

   lb.lbStyle = hb_parni( 1 );
   lb.lbColor = static_cast< COLORREF >( hb_parnldef( 2, RGB( 0, 0, 0 ) ) );
   lb.lbHatch = hb_parni( 3 );
   hb_retnint( reinterpret_cast< HB_PTRUINT >( CreateBrushIndirect( &lb ) ) );
}

/*
 *   win_DrawText( hDC, cText, aRect, nFormat )
 */
HB_FUNC( WVG_DRAWTEXT )
{
   RECT    rc = { 0, 0, 0, 0 };
   void *  hBuffer;
   LPCTSTR lpBuffer = HB_PARSTR( 2, &hBuffer, nullptr );

   rc.left   = hb_parvni( 3, 1 );
   rc.top    = hb_parvni( 3, 2 );
   rc.right  = hb_parvni( 3, 3 );
   rc.bottom = hb_parvni( 3, 4 );

   hb_retl( DrawText( reinterpret_cast< HDC >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), lpBuffer, lstrlen( lpBuffer ), &rc, hb_parni( 4 ) ) );
   hb_strfree( hBuffer );
}

HB_FUNC( WVG_GETWINDOWRECT )
{
   RECT     rc;
   PHB_ITEM info = hb_itemArrayNew( 4 );

   GetWindowRect( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), &rc );

   hb_arraySetNI( info, 1, rc.left   );
   hb_arraySetNI( info, 2, rc.top    );
   hb_arraySetNI( info, 3, rc.right  );
   hb_arraySetNI( info, 4, rc.bottom );

   hb_itemReturnRelease( info );
}

/*
 * Win_MoveWindow( hWnd, nLeft, nTop, nWidth, nHeight, lRePaint )
 */
HB_FUNC( WVG_MOVEWINDOW )
{
   MoveWindow( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), hb_parnl( 2 ), hb_parnl( 3 ), hb_parnl( 4 ), hb_parnl( 5 ), hb_parl( 6 ) );
}

HB_FUNC( WVG_GETDESKTOPWINDOW )
{
   wvg_rethandle( GetDesktopWindow() );
}

HB_FUNC( WVG_SETPARENT )
{
   hb_retnint( reinterpret_cast< HB_PTRUINT >( SetParent( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 2 ) ) ) ) ) );
}

HB_FUNC( WVG_BRINGWINDOWTOTOP )
{
   hb_retl( BringWindowToTop( wvg_parhwnd( 1 ) ) );
}

HB_FUNC( WVG_SETFOREGROUNDWINDOW )
{
   hb_retl( BringWindowToTop( wvg_parhwnd( 1 ) ) );
}

HB_FUNC( WVG_SETWINDOWTEXT )
{
   void * hText;

   SetWindowText( wvg_parhwnd( 1 ), HB_PARSTR( 2, &hText, nullptr ) );
   hb_strfree( hText );
}

HB_FUNC( WVG_SETWINDOWLONG )
{
   hb_retnl( SetWindowLong( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), hb_parni( 2 ), hb_parnl( 3 ) ) );
}

HB_FUNC( WVG_ISWINDOW )
{
   hb_retl( IsWindow( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ) ) );
}

HB_FUNC( WVG_ENABLEWINDOW )
{
   hb_retl( EnableWindow( wvg_parhwnd( 1 ), hb_parl( 2 ) ) );
}

HB_FUNC( WVG_DESTROYWINDOW )
{
   hb_retl( DestroyWindow( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ) ) );
}

HB_FUNC( WVG_CLIENTTOSCREEN )
{
   POINT    Point;
   PHB_ITEM pArray = hb_param( 2, HB_IT_ARRAY );

   if( wvt_Array2Point( pArray, &Point ) )
   {
      if( ClientToScreen( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), &Point ) )
      {
         wvt_Point2ArrayEx( &Point, pArray );
         hb_retl(true);
      }
      else
      {
         hb_retl( HB_FALSE );
      }
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

HB_FUNC( WVG_SCREENTOCLIENT )
{
   POINT    Point;
   PHB_ITEM pArray = hb_param( 2, HB_IT_ARRAY );

   if( wvt_Array2Point( pArray, &Point ) )
   {
      if( ScreenToClient( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), &Point ) > 0 )
      {
         wvt_Point2ArrayEx( &Point, pArray );
         hb_retl(true);
      }
      else
      {
         hb_retl( HB_FALSE );
      }
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

HB_FUNC( WVG_AND )
{
   hb_retnl( hb_parnl( 1 ) & hb_parnl( 2 ) );
}

HB_FUNC( WVG_OR )
{
   hb_retnl( hb_parnl( 1 ) | hb_parnl( 2 ) );
}

HB_FUNC( WVG_NOT )
{
   hb_retnl( ~( hb_parnl( 1 ) ) );
}

HB_FUNC( WVG_TRACKPOPUPMENU )
{
   HMENU hMenu  = reinterpret_cast< HMENU >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) );
   UINT  uFlags = hb_parnldef( 2, TPM_CENTERALIGN | TPM_RETURNCMD );
   int   x      = hb_parni( 3 );
   int   y      = hb_parni( 4 );
   HWND  hWnd   = HB_ISNUM( 5 ) ? reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 5 ) ) ) : GetActiveWindow();

   POINT xy = { 0, 0 };

   if( ! HB_ISNUM( 3 ) )
   {
      GetCursorPos( &xy );
   }
   else
   {
      xy.x = x;
      xy.y = y;
   }

   hb_retnl( TrackPopupMenu( hMenu, uFlags, xy.x, xy.y, 0, hWnd, nullptr ) );
}

HB_FUNC( WVG_CHOOSECOLOR )
{
   CHOOSECOLOR cc;
   COLORREF    crCustClr[ 16 ];

   for( int i = 0; i < static_cast< int >( HB_SIZEOFARRAY( crCustClr ) ); i++ )
   {
      crCustClr[ i ] = ( HB_ISARRAY( 2 ) ? static_cast< COLORREF >( hb_parvnl( 2, i + 1 ) ) : GetSysColor( COLOR_BTNFACE ) );
   }

   cc.lStructSize  = sizeof( CHOOSECOLOR );
   cc.hwndOwner    = HB_ISNUM( 4 ) ? reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 4 ) ) ) : nullptr;
   cc.rgbResult    = static_cast< COLORREF >( hb_parnl( 1 ) );
   cc.lpCustColors = crCustClr;
   cc.Flags        = static_cast< WORD >( hb_parnldef( 3, CC_ANYCOLOR | CC_RGBINIT | CC_FULLOPEN ) );

   if( ChooseColor( &cc ) )
   {
      hb_retnl( cc.rgbResult );
   }
   else
   {
      hb_retnl( -1 );
   }
}

HB_FUNC( WVG_FINDWINDOW )
{
   HWND   hwnd;
   void * hText;

   hwnd = FindWindow( nullptr, HB_PARSTR( 1, &hText, nullptr ) );
   hb_strfree( hText );

   if( hwnd )
   {
      hb_retnint( reinterpret_cast< HB_PTRUINT >( hwnd ) );
   }
   else
   {
      hb_retnint( -1 );
   }
}

HB_FUNC( WVG_SLEEP )
{
   Sleep( hb_parni( 1 ) );
}

/*                         Menu Manipulations                           */

HB_FUNC( WVG_SETMENU )
{
   HWND hWnd = reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) );

   #if 1
   HB_BOOL bSet;
   RECT    wi = { 0, 0, 0, 0 };
   RECT    ci = { 0, 0, 0, 0 };
   int     height, width;

   bSet = SetMenu( hWnd, reinterpret_cast< HMENU >( static_cast< HB_PTRUINT >( hb_parnint( 2 ) ) ) );

   GetWindowRect( hWnd, &wi );
   GetClientRect( hWnd, &ci );
   height = ( ci.bottom - ci.top );
   width  = ( ci.right - ci.left );

   width  += ( wi.right - wi.left - ci.right );
   height += ( wi.bottom - wi.top - ci.bottom );

   SetWindowPos( hWnd, nullptr, wi.left, wi.top, width, height, SWP_NOZORDER );

   hb_retl( bSet );
   #endif

   #if 0
   hb_retl( SetMenu( hWnd, static_cast< HMENU >( static_cast< HB_PTRUINT >( hb_parnint( 2 ) ) ) ) );
   #endif
}

HB_FUNC( WVG_CREATEMENU )
{
   hb_retnint( reinterpret_cast< HB_PTRUINT >( CreateMenu() ) );
}

HB_FUNC( WVG_CREATEPOPUPMENU )
{
   hb_retnint( reinterpret_cast< HB_PTRUINT >( CreatePopupMenu() ) );
}

HB_FUNC( WVG_APPENDMENU )
{
   if( HB_ISCHAR( 4 ) )
   {
      void * hBuffer;
      hb_retl( AppendMenu( reinterpret_cast< HMENU >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), static_cast< UINT >( hb_parni( 2 ) ), static_cast< HB_PTRUINT >( hb_parnint( 3 ) ), HB_PARSTR( 4, &hBuffer, nullptr ) ) );
      hb_strfree( hBuffer );
   }
   else /* It is a SEPARATOR or Submenu */
   {
      LPCTSTR lpszCaption = reinterpret_cast< LPCTSTR >( static_cast< HB_PTRUINT >( hb_parnint( 4 ) ) );
      hb_retl( AppendMenu( reinterpret_cast< HMENU >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), static_cast< UINT >( hb_parni( 2 ) ), static_cast< HB_PTRUINT >( hb_parnint( 3 ) ), static_cast< LPCTSTR >( lpszCaption ) ) );
   }
}

HB_FUNC( WVG_INSERTMENU )
{
   UINT flags = hb_parni( 3 );

   if( HB_ISCHAR( 5 ) )
   {
      void * hBuffer;
      hb_retl( InsertMenu( reinterpret_cast< HMENU >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), static_cast< UINT >( hb_parni( 2 ) ),
                           flags, static_cast< HB_PTRUINT >( hb_parnint( 4 ) ), HB_PARSTR( 5, &hBuffer, nullptr ) ) );
      hb_strfree( hBuffer );
   }
   else /* It is a SEPARATOR or Submenu */
   {
      LPCTSTR lpszCaption = reinterpret_cast< LPCTSTR >( static_cast< HB_PTRUINT >( hb_parnint( 5 ) ) );
      hb_retl( InsertMenu( reinterpret_cast< HMENU >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), static_cast< UINT >( hb_parni( 2 ) ),
                           flags, static_cast< HB_PTRUINT >( hb_parnint( 4 ) ), static_cast< LPCTSTR >( lpszCaption ) ) );
   }
}

HB_FUNC( WVG_DELETEMENU )
{
   hb_retl( DeleteMenu( reinterpret_cast< HMENU >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), static_cast< UINT >( hb_parni( 2 ) ), static_cast< UINT >( hb_parni( 3 ) ) ) );
}

HB_FUNC( WVG_DESTROYMENU )
{
   hb_retl( DestroyMenu( reinterpret_cast< HMENU >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ) ) );
}

HB_FUNC( WVG_ENABLEMENUITEM )
{
   hb_retl( EnableMenuItem( reinterpret_cast< HMENU >(static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), static_cast< UINT >(hb_parni( 2 ) ), static_cast< UINT >( hb_parni( 3 ) ) ) );
}

HB_FUNC( WVG_CHECKMENUITEM )
{
   hb_retni( CheckMenuItem( reinterpret_cast< HMENU >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), static_cast< UINT >( hb_parni( 2 ) ), static_cast< UINT >( hb_parni( 3 ) ) ) );
}

HB_FUNC( WVG_ISMENUITEMCHECKED )
{
   BOOL lSuccess;
   MENUITEMINFO lpmii;

   memset( &lpmii, 0, sizeof( MENUITEMINFO ) );
   lpmii.cbSize = sizeof( MENUITEMINFO );
   lpmii.fMask  = MIIM_STATE;

   lSuccess = GetMenuItemInfo( reinterpret_cast< HMENU >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), static_cast< UINT >( hb_parni( 2 ) ), TRUE, &lpmii );
   if( lSuccess )
   {
      hb_retl( lpmii.fState & MFS_CHECKED ? TRUE : FALSE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

HB_FUNC( WVG_ISMENUITEMENABLED )
{
   BOOL lSuccess;
   MENUITEMINFO lpmii;

   lpmii.cbSize = sizeof( MENUITEMINFO );
   lpmii.fMask  = MIIM_STATE;

   lSuccess = GetMenuItemInfo( reinterpret_cast< HMENU >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), static_cast< UINT >( hb_parni( 2 ) ), TRUE, &lpmii );
   if( lSuccess )
   {
      hb_retl( lpmii.fState & MFS_DISABLED ? FALSE : TRUE );
   }
   else
   {
      hb_retl( TRUE );
   }
}

HB_FUNC( WVG_SETMENUITEM )
{
   BOOL lSuccess;
   MENUITEMINFO lpmii;
   void *       hText = nullptr;

   memset( &lpmii, 0, sizeof( MENUITEMINFO ) );
   lpmii.cbSize = sizeof( MENUITEMINFO );
   lpmii.fMask  = hb_parl( 5 ) ? MIIM_STRING : MIIM_SUBMENU;
   if( hb_parl( 5 ) )
   {
      lpmii.dwTypeData = const_cast< LPTSTR >( HB_PARSTR( 4, &hText, nullptr ) );
   }

   lSuccess = SetMenuItemInfo( reinterpret_cast< HMENU >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), static_cast< UINT >( hb_parni( 2 ) ), TRUE, &lpmii );
   hb_retl( lSuccess );

   if( hText )
   {
      hb_strfree( hText );
   }
}

HB_FUNC( WVG_DRAWMENUBAR )
{
   DrawMenuBar( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ) );
}

HB_FUNC( WVG_UPDATEWINDOW )
{
   hb_retl( UpdateWindow( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ) ) );
}

HB_FUNC( WVG_SHOWWINDOW )
{
   hb_retl( ShowWindow( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), hb_parni( 2 ) ) );
}

HB_FUNC( WVG_MAKELPARAM )
{
   hb_retnint( MAKELPARAM( hb_parnint( 1 ), hb_parnint( 2 ) ) );
}

HB_FUNC( WVG_CREATEWINDOWEX )
{
   HWND   hWnd;
   void * hClassName;
   void * hWinName;

   hWnd = CreateWindowEx( static_cast< DWORD >( hb_parnint( 1 ) ),
                          HB_PARSTR( 2, &hClassName, nullptr ),
                          HB_PARSTR( 3, &hWinName, nullptr ),
                          static_cast< DWORD >( hb_parnint( 4 ) ),
                          hb_parni( 5 ), hb_parni( 6 ),
                          hb_parni( 7 ), hb_parni( 8 ),
                          reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 9 ) ) ),
                          HB_ISNUM( 10 ) ? reinterpret_cast< HMENU >( static_cast< HB_PTRUINT >( hb_parnint( 10 ) ) ) : nullptr,
                          HB_ISNUM( 11 ) ? reinterpret_cast< HINSTANCE >( static_cast< HB_PTRUINT >( hb_parnint( 11 ) ) ) : static_cast< HINSTANCE >( wvg_hInstance() ),
                          nullptr );
   hb_strfree( hClassName );
   hb_strfree( hWinName );

   hb_retnint( reinterpret_cast< HB_PTRUINT >( hWnd ) );
}

HB_FUNC( WVG_SENDMESSAGETEXT )
{
   void * hBuffer;

   SendMessage( reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) ), hb_parni( 2 ), static_cast< WPARAM >( hb_parni( 3 ) ), reinterpret_cast< LPARAM >( HB_PARSTR( 4, &hBuffer, nullptr ) ) );
   hb_strfree( hBuffer );
}

HB_FUNC( WVG_GETMESSAGETEXT )
{
   TCHAR cText[ 32000 ];

   SendMessage( wvg_parhwnd( 1 ), static_cast< UINT >( hb_parni( 2 ) ), wvg_parwparam( 3 ), reinterpret_cast< LPARAM >( cText ) );

   HB_RETSTR( cText );
}

HB_FUNC( WVG_SETWNDPROC )
{
   HWND    hWnd    = reinterpret_cast< HWND >( static_cast< HB_PTRUINT >( hb_parnint( 1 ) ) );
   WNDPROC wndProc = reinterpret_cast< WNDPROC >(static_cast< HB_PTRUINT >( hb_parnint( 2 ) ) );
   WNDPROC oldProc;

#if ( defined( _MSC_VER ) && ( _MSC_VER <= 1200 ) ) && ! defined( HB_ARCH_64BIT )
   oldProc = static_cast< WNDPROC >( SetWindowLong( hWnd, GWL_WNDPROC, static_cast< long >( wndProc ) ) );
#else
   oldProc = reinterpret_cast< WNDPROC >( SetWindowLongPtr( hWnd, GWLP_WNDPROC, reinterpret_cast< HB_PTRUINT >( wndProc ) ) );
#endif

   hb_retnint( reinterpret_cast< HB_PTRUINT >( oldProc ) );
}

HB_FUNC( WVG_DEFWINDOWPROC )
{
   hb_retnint( DefWindowProc( wvg_parhwnd( 1 ), hb_parni( 2 ), wvg_parwparam( 3 ), wvg_parlparam( 4 ) ) );
}

HB_FUNC( WVG_CALLWINDOWPROC )
{
   hb_retnint( CallWindowProc( wvg_parwndproc( 1 ), wvg_parhwnd( 2 ), static_cast< UINT >( hb_parnint( 3 ) ), wvg_parwparam( 4 ), wvg_parlparam( 5 ) ) );
}

/*                         TreeView Functions                           */

HB_FUNC( WVG_TREEVIEW_SETTEXTCOLOR )
{
   hb_retl( TreeView_SetTextColor( wvg_parhwnd( 1 ), wvg_parcolor( 2 ) ) );
}

HB_FUNC( WVG_TREEVIEW_SETBKCOLOR )
{
   hb_retl( TreeView_SetBkColor( wvg_parhwnd( 1 ), wvg_parcolor( 2 ) ) );
}

HB_FUNC( WVG_TREEVIEW_SETLINECOLOR )
{
   #if 0
   hb_retl( TreeView_SetLineColor( wvg_parhwnd( 1 ), wvg_parcolor( 2 ) ) );
   #endif
}

HB_FUNC( WVG_TREEVIEW_SELECTITEM )
{
   hb_retl( TreeView_SelectItem( wvg_parhwnd( 1 ), wvg_parhandle( 2 ) ) );
}

HB_FUNC( WVG_TREEVIEW_EXPAND )
{
   hb_retl( TreeView_Expand( wvg_parhwnd( 1 ), wvg_parhandle( 2 ), ( hb_parl( 3 ) ? TVE_EXPAND : TVE_COLLAPSE ) ) );
}

HB_FUNC( WVG_TVIS_EXPANDED )
{
   #if 0
   hb_retl( TreeView_GetItemState( wvg_parhwnd( 1 ), wvg_parhandle( 2 ), static_cast< UINT >( TVIS_EXPANDED ) ) );
   #endif
}

/*                          ListBox Functions                           */

HB_FUNC( WVG_LBGETTEXT )
{
   TCHAR text[ MAX_PATH + 1 ];

   SendMessage( wvg_parhwnd( 1 ), LB_GETTEXT, wvg_parwparam( 2 ), reinterpret_cast< LPARAM >( text ) );

   HB_RETSTR( text );
}

HB_FUNC( WVG_LBGETCURSEL )
{
   hb_retni( ListBox_GetCurSel( wvg_parhwnd( 1 ) ) );
}

HB_FUNC( WVG_LBSETCURSEL )
{
   hb_retni( ListBox_SetCurSel( wvg_parhwnd( 1 ), hb_parni( 2 ) ) );
}

/*                                Buttons                               */

HB_FUNC( WVG_BUTTON_GETCHECK )
{
   hb_retnl( Button_GetCheck( wvg_parhwnd( 1 ) ) );
}

HB_FUNC( WVG_ISICONIC )
{
   hb_retl( IsIconic( wvg_parhwnd( 1 ) ) );
}

HB_FUNC( WVG_ISZOOMED )
{
   hb_retl( IsZoomed( wvg_parhwnd( 1 ) ) );
}

/*
 * Win_SetDCBrushColor( hDC, nRGB )
 */
HB_FUNC( WVG_SETDCBRUSHCOLOR )
{
#if ( _WIN32_WINNT >= 0x0500 )
   wvg_rethandle( SetDCBrushColor( wvg_parhdc( 1 ), wvg_parcolor( 2 ) ) );
#else
   wvg_rethandle( nullptr );
#endif
}

/*
 * Win_SetDCPenColor( hDC, nRGB )
 */
HB_FUNC( WVG_SETDCPENCOLOR )
{
#if ( _WIN32_WINNT >= 0x0500 )
   wvg_rethandle( SetDCPenColor( wvg_parhdc( 1 ), wvg_parcolor( 2 ) ) );
#else
   wvg_rethandle( nullptr );
#endif
}

/*
 * Win_GetCurrentObject( hDC, nObjType )
 */
HB_FUNC( WVG_GETCURRENTOBJECT )
{
   wvg_rethandle( GetCurrentObject( wvg_parhdc( 1 ), hb_parni( 2 ) ) );
}

/*
 * Win_GetCurrentBrush( hDC )
 */
HB_FUNC( WVG_GETCURRENTBRUSH )
{
   wvg_rethandle( GetCurrentObject( wvg_parhdc( 1 ), OBJ_BRUSH ) );
}

/*
 * Win_GetCurrentFornt( hDC )
 */
HB_FUNC( WVG_GETCURRENTFONT )
{
   wvg_rethandle( GetCurrentObject( wvg_parhdc( 1 ), OBJ_FONT ) );
}

HB_FUNC( WVG_SETWINDOWPOSTOBACK )
{
   hb_retl( SetWindowPos( wvg_parhwnd( 1 ), HWND_BOTTOM, 0, 0, 0, 0, SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE ) );
}

HB_FUNC( WVG_SETWINDOWPOSTOTOP )
{
   hb_retl( SetWindowPos( wvg_parhwnd( 1 ), HWND_TOP, 0, 0, 0, 0, SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE ) );
}

HB_FUNC( WVG_SETWINDOWSIZE )
{
   hb_retl( SetWindowPos( wvg_parhwnd( 1 ), nullptr, 0, 0, hb_parni( 2 ), hb_parni( 3 ), hb_parl( 4 ) ? 0 : SWP_NOREDRAW | SWP_NOZORDER | SWP_NOMOVE | SWP_NOACTIVATE  ) );
}

HB_FUNC( WVG_SETWINDOWPOSITION )
{
   hb_retl( SetWindowPos( wvg_parhwnd( 1 ), nullptr, hb_parni( 2 ), hb_parni( 3 ), 0, 0, hb_parl( 4 ) ? 0 : SWP_NOREDRAW | SWP_NOZORDER | SWP_NOSIZE | SWP_NOACTIVATE ) );
}

HB_FUNC( WVG_SETWINDOWPOSANDSIZE )
{
   hb_retl( SetWindowPos( wvg_parhwnd( 1 ), nullptr, hb_parni( 2 ), hb_parni( 3 ),
                          hb_parni( 4 ), hb_parni( 5 ), ( hb_parl( 6 ) ? 0 : SWP_NOREDRAW ) | SWP_NOZORDER | SWP_NOACTIVATE | SWP_FRAMECHANGED ) );
}

HB_FUNC( WVG_POSTMESSAGE )
{
   hb_retl( PostMessage( wvg_parhwnd( 1 ), hb_parni( 2 ), static_cast< WPARAM >( hb_parni( 3 ) ), static_cast< LPARAM >( hb_parni( 4 ) ) ) );
}

HB_FUNC( WVG_FORCEWINDOWTOTOP )
{
   SetWindowPos( wvg_parhwnd( 1 ), HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE );
   SetWindowPos( wvg_parhwnd( 1 ), HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE );
}

/*
 * Win_SetLayeredWindowAttributes( hWnd, nRGB, nOpacityFactor [0-255] )
 */
HB_FUNC( WVG_SETLAYEREDWINDOWATTRIBUTES )
{
   HWND     hWnd = hbwapi_par_raw_HWND( 1 );
   COLORREF cr   = HB_ISNUM( 2 ) ? hbwapi_par_COLORREF( 2 ) : RGB( 255, 255, 255 );

   SetWindowLong( hWnd, GWL_EXSTYLE, GetWindowLong( hWnd, GWL_EXSTYLE ) | WS_EX_LAYERED );

   if( SetLayeredWindowAttributes( hWnd, cr, static_cast< BYTE >( hb_parni( 3 ) ), /*LWA_COLORKEY |*/ LWA_ALPHA ) == false )
   {
      /* Just to supress warning */
   }
}

HB_FUNC( WVG_SENDTOOLBARMESSAGE )
{
   HWND hTB = hbwapi_par_raw_HWND( 1 );
   int  msg = hbwapi_par_INT( 2 );

   switch( msg )
   {
      case TB_ADDBITMAP:
      {
         TBADDBITMAP tbab;

         tbab.hInst = nullptr;
#if ( _WIN32_IE >= 0x0500 )
         tbab.nID = reinterpret_cast< UINT_PTR >( hbwapi_par_raw_HBITMAP( 3 ) );
#else
         tbab.nID = static_cast< UINT >( hbwapi_par_raw_HBITMAP( 3 ) );
#endif
         hbwapi_ret_NI( static_cast< int >( SendMessage( hTB, TB_ADDBITMAP, static_cast< WPARAM >( 1 ), reinterpret_cast< LPARAM >( &tbab ) ) ) );
         break;
      }
      case TB_ADDBUTTONS:
      {
         TBBUTTON tbb;

         tbb.iBitmap   = hbwapi_par_INT( 3 );
         tbb.idCommand = hbwapi_par_INT( 4 );
         tbb.fsState   = TBSTATE_ENABLED;
         tbb.fsStyle   = TBSTYLE_BUTTON;
         tbb.dwData    = 0;
         tbb.iString   = hbwapi_par_INT( 5 );

         hbwapi_ret_L( SendMessage( hTB, TB_ADDBUTTONS, static_cast< WPARAM >( 1 ), reinterpret_cast< LPARAM >( static_cast< LPTBBUTTON >( &tbb ) ) ) );
         break;
      }
      case TB_ADDSTRING:
      {
         int    iString;
         void * hCaption;

         iString = static_cast< int >( SendMessage( hTB, TB_ADDSTRING, reinterpret_cast< WPARAM >( nullptr ), reinterpret_cast< LPARAM >( HB_PARSTR( 3, &hCaption, nullptr ) ) ) );
         hb_strfree( hCaption );

         hbwapi_ret_NI( iString );
         break;
      }
      case TB_AUTOSIZE:
         SendMessage( hTB, TB_AUTOSIZE, static_cast< WPARAM >( 0 ), static_cast< LPARAM >( 0 ) );
         break;
      case TB_BUTTONCOUNT:
         break;
      case TB_BUTTONSTRUCTSIZE:
         SendMessage( hTB, TB_BUTTONSTRUCTSIZE, sizeof( TBBUTTON ), 0 );
         break;
      case TB_CHANGEBITMAP:
      case TB_CHECKBUTTON:
      case TB_COMMANDTOINDEX:
      case TB_DELETEBUTTON:
      case TB_ENABLEBUTTON:
      case TB_GETBITMAP:
      case TB_GETBITMAPFLAGS:
      case TB_GETBUTTON:
      case TB_GETBUTTONINFO:
      case TB_GETBUTTONSIZE:
      case TB_GETBUTTONTEXT:
      case TB_GETDISABLEDIMAGELIST:
      case TB_GETIMAGELIST:
      case TB_GETITEMRECT:
      case TB_GETRECT:
      case TB_GETROWS:
      case TB_GETSTATE:
      case TB_GETSTYLE:
      case TB_GETTEXTROWS:
      case TB_GETTOOLTIPS:
      case TB_HIDEBUTTON:
      case TB_HITTEST:
      case TB_INDETERMINATE:
      case TB_INSERTBUTTON:
      case TB_ISBUTTONCHECKED:
      case TB_ISBUTTONENABLED:
      case TB_ISBUTTONHIDDEN:
      case TB_ISBUTTONHIGHLIGHTED:
      case TB_ISBUTTONINDETERMINATE:
      case TB_ISBUTTONPRESSED:
      case TB_LOADIMAGES:
      case TB_PRESSBUTTON:
      case TB_REPLACEBITMAP:
         break;
      case TB_SETBITMAPSIZE:
         SendMessage( hTB, TB_SETBITMAPSIZE, static_cast< WPARAM >( 0 ), static_cast< LPARAM >( MAKELONG( hbwapi_par_INT( 3 ), hbwapi_par_INT( 4 ) ) ) );
         break;
      case TB_SETBUTTONINFO:
         break;
      case TB_SETBUTTONSIZE:
         SendMessage( hTB, TB_SETBUTTONSIZE, static_cast< WPARAM >( 0 ), static_cast< LPARAM >( MAKELONG( hbwapi_par_INT( 3 ), hbwapi_par_INT( 4 ) ) ) );
         break;
      case TB_SETBUTTONWIDTH:
         SendMessage( hTB, TB_SETBUTTONWIDTH, static_cast< WPARAM >( 0 ), static_cast< LPARAM >( MAKELONG( hbwapi_par_INT( 3 ), hbwapi_par_INT( 4 ) ) ) );
         break;
      case TB_SETIMAGELIST:
         SendMessage( hTB, TB_SETIMAGELIST, static_cast< WPARAM >( 0 ), reinterpret_cast< LPARAM >( hbwapi_par_raw_HIMAGELIST( 3 ) ) );
         break;
      case TB_SETINDENT:
         SendMessage( hTB, TB_SETINDENT, static_cast< WPARAM >( hbwapi_par_INT( 3 ) ), static_cast< LPARAM >( 0 ) );
         break;
      case TB_SETMAXTEXTROWS:
         SendMessage( hTB, TB_SETMAXTEXTROWS, static_cast< WPARAM >( hbwapi_par_INT( 2 ) ), static_cast< LPARAM >( 0 ) );
         break;
      case TB_SETPARENT:
      case TB_SETROWS:
      case TB_SETSTATE:
      case TB_SETSTYLE:
      case TB_SETTOOLTIPS:
      case TB_SETCMDID:
      case TB_SETDISABLEDIMAGELIST:
      case TB_SETDRAWTEXTFLAGS:
         break;

      #if 0
      case TB_TRANSLATEACCELERATOR:
      case TB_SETPRESSEDIMAGELIST:
      case TB_SETWINDOWTHEME:
      case TB_GETIDEALSIZE:
      case TB_GETIMAGELISTCOUNT:
      case TB_GETMETRICS:
      case TB_GETPRESSEDIMAGELIST:
      case TB_GETSTRING:
      case TB_SETLISTGAP:
      case TB_GETITEMDROPDOWNRECT:
      case TB_SETHOTITEM2:
      case TB_SETMETRICS:
         break;
      #endif

      case TB_SETPADDING:
         SendMessage( hTB, TB_SETPADDING, static_cast< WPARAM >( 0 ), static_cast< LPARAM >( MAKELPARAM( hbwapi_par_INT( 2 ), hbwapi_par_INT( 3 ) ) ) );
         break;
      case TB_MARKBUTTON:
         SendMessage( hTB, TB_MARKBUTTON, static_cast< WPARAM >( hbwapi_par_INT( 3 ) ), static_cast< LPARAM >( MAKELONG( hb_parl( 4 ), 0 ) ) );
         break;
      case TB_SETINSERTMARK:
      case TB_SETINSERTMARKCOLOR:
      case TB_SETCOLORSCHEME:
      case TB_SETEXTENDEDSTYLE:
      case TB_SETHOTIMAGELIST:
      case TB_SETHOTITEM:
      case TB_INSERTMARKHITTEST:
      case TB_MAPACCELERATOR:
      case TB_MOVEBUTTON:
      case TB_GETINSERTMARK:
         break;
      case TB_GETCOLORSCHEME:
      {
         PHB_ITEM info = hb_itemArrayNew( 2 );
         COLORSCHEME colorScheme;

         colorScheme.dwSize = sizeof( COLORSCHEME );
         SendMessage( hTB, TB_GETCOLORSCHEME, static_cast< WPARAM >( 0 ), reinterpret_cast< LPARAM >( &colorScheme ) );

         hb_arraySetNInt( info, 1, colorScheme.clrBtnHighlight );
         hb_arraySetNInt( info, 2, colorScheme.clrBtnShadow );
         hb_itemReturnRelease( info );
         break;
      }
      case TB_CUSTOMIZE:
      case TB_GETANCHORHIGHLIGHT:
      case TB_GETEXTENDEDSTYLE:
      case TB_GETHOTIMAGELIST:
      case TB_GETINSERTMARKCOLOR:
      case TB_GETHOTITEM:
      case TB_GETOBJECT:
      case TB_GETUNICODEFORMAT:
      case TB_GETMAXSIZE:
      case TB_SAVERESTORE:
      case TB_SETANCHORHIGHLIGHT:
      case TB_SETUNICODEFORMAT:
         break;
   }
}

HB_FUNC( WVG_SENDEDITCONTROLMESSAGE )
{
   HWND hED = hbwapi_par_raw_HWND( 1 );
   int  msg = hbwapi_par_INT( 2 );

   switch( msg )
   {
      case EM_GETSEL:
      {
         DWORD min = 0;
         DWORD max = 0;
         SendMessage( hED, EM_GETSEL, reinterpret_cast< WPARAM >( &min ), reinterpret_cast< LPARAM >( &max ) );
         break;
      }
   }
}

HB_FUNC( WVG_SENDCBMESSAGE )
{
   HWND   hCB   = hbwapi_par_raw_HWND( 1 );
   int    msg   = hbwapi_par_INT( 2 );
   void * hText = nullptr;

   switch( msg )
   {
      case CB_ADDSTRING:
         hb_retnint( SendMessage( hCB, CB_ADDSTRING, reinterpret_cast< WPARAM >( nullptr ), reinterpret_cast< LPARAM >( static_cast< LPCTSTR >( HB_PARSTR( 3, &hText, nullptr ) ) ) ) );
         break;
      case CB_DELETESTRING:
         hb_retnint( SendMessage( hCB, CB_DELETESTRING, hb_parni( 3 ), 0 ) );
         break;
      case CB_DIR:
         hb_retnint( SendMessage( hCB, CB_DIR, static_cast< WPARAM >( hb_parni( 3 ) ), reinterpret_cast< LPARAM >( HB_PARSTR( 4, &hText, nullptr ) ) ) );
         break;
      case CB_FINDSTRING:
         hb_retnint( SendMessage( hCB, CB_FINDSTRING, static_cast< WPARAM >( hb_parni( 3 ) ), reinterpret_cast< LPARAM >( HB_PARSTR( 4, &hText, nullptr ) ) ) );
         break;
      case CB_FINDSTRINGEXACT:
         hb_retnint( SendMessage( hCB, CB_FINDSTRINGEXACT, static_cast< WPARAM >( hb_parni( 3 ) ), reinterpret_cast< LPARAM >( HB_PARSTR( 4, &hText, nullptr ) ) ) );
         break;
      case CB_GETCOMBOBOXINFO:
      {
         COMBOBOXINFO cbi;
         PHB_ITEM pCbi = hb_itemNew( nullptr );
         PHB_ITEM pRc1 = hb_itemNew( nullptr );
         PHB_ITEM pRc2 = hb_itemNew( nullptr );

         memset( &cbi, 0, sizeof( COMBOBOXINFO ) );
         cbi.cbSize = sizeof( COMBOBOXINFO );

         if( GetComboBoxInfo( hCB, &cbi ) )
         {
            hb_arrayNew( pCbi, 6 );
            hb_arrayNew( pRc1, 4 );
            hb_arrayNew( pRc2, 4 );

            hb_arraySetNI( pRc1, 1, cbi.rcItem.left );
            hb_arraySetNI( pRc1, 2, cbi.rcItem.top );
            hb_arraySetNI( pRc1, 3, cbi.rcItem.right );
            hb_arraySetNI( pRc1, 4, cbi.rcItem.bottom );

            hb_arraySet( pCbi, 1, pRc1 );

            hb_arraySetNI( pRc2, 1, cbi.rcButton.left );
            hb_arraySetNI( pRc2, 2, cbi.rcButton.top );
            hb_arraySetNI( pRc2, 3, cbi.rcButton.right );
            hb_arraySetNI( pRc2, 4, cbi.rcButton.bottom );

            hb_arraySet( pCbi, 2, pRc2 );

            hb_arraySetNInt( pCbi, 3, cbi.stateButton );
            hb_arraySetNInt( pCbi, 4, reinterpret_cast< HB_PTRUINT >( cbi.hwndCombo ) );
            hb_arraySetNInt( pCbi, 5, reinterpret_cast< HB_PTRUINT >( cbi.hwndItem ) );
            hb_arraySetNInt( pCbi, 6, reinterpret_cast< HB_PTRUINT >( cbi.hwndList ) );

            hb_itemReturnRelease( pCbi );
            hb_itemRelease( pRc1 );
            hb_itemRelease( pRc2 );
         }
         break;
      }
      case CB_GETCOUNT:
         hb_retnint( SendMessage( hCB, CB_GETCOUNT, 0, 0 ) );
         break;
#if defined( CB_GETCUEBANNER )
      case CB_GETCUEBANNER:
         break;
#endif
      case CB_GETCURSEL:
         hb_retnint( SendMessage( hCB, CB_GETCURSEL, 0, 0 ) );
         break;
      case CB_GETDROPPEDCONTROLRECT:
      {
         RECT     rc;
         PHB_ITEM pRect = hb_itemNew( nullptr );

         SendMessage( hCB, CB_GETDROPPEDCONTROLRECT, 0, reinterpret_cast< LPARAM >( &rc ) );

         hb_arrayNew( pRect, 4 );
         hb_arraySetNI( pRect, 1, rc.left );
         hb_arraySetNI( pRect, 2, rc.top );
         hb_arraySetNI( pRect, 3, rc.right );
         hb_arraySetNI( pRect, 4, rc.bottom );

         hb_itemReturnRelease( pRect );
         break;
      }
      case CB_GETDROPPEDSTATE:
         hb_retnint( SendMessage( hCB, CB_GETDROPPEDSTATE, 0, 0 ) );
         break;
      case CB_GETDROPPEDWIDTH:
         hb_retnint( SendMessage( hCB, CB_GETDROPPEDWIDTH, 0, 0 ) );
         break;
      case CB_GETEDITSEL:
      {
         DWORD    range = static_cast< DWORD >( SendMessage( hCB, CB_GETEDITSEL, reinterpret_cast< WPARAM >( nullptr ), reinterpret_cast< LPARAM >( nullptr ) ) );
         PHB_ITEM pRng  = hb_itemNew( nullptr );

         hb_arrayNew( pRng, 2 );
         hb_arraySetNI( pRng, 1, LOWORD( range ) );
         hb_arraySetNI( pRng, 1, HIWORD( range ) );
         hb_itemReturnRelease( pRng );

         break;
      }
      case CB_GETEXTENDEDUI:
         hb_retnint( SendMessage( hCB, CB_GETEXTENDEDUI, 0, 0 ) );
         break;
      case CB_GETHORIZONTALEXTENT:
         hb_retnint( SendMessage( hCB, CB_GETHORIZONTALEXTENT, 0, 0 ) );
         break;
      case CB_GETITEMDATA:
         hb_retnint( SendMessage( hCB, CB_GETITEMDATA, static_cast< WPARAM >( hb_parnint( 3 ) ), 0 ) );
         break;
      case CB_GETITEMHEIGHT:
         hb_retnint( SendMessage( hCB, CB_GETITEMHEIGHT, 0, 0 ) );
         break;
      case CB_GETLBTEXT:
      {
         HB_ISIZ iSize = SendMessage( hCB, CB_GETLBTEXTLEN, static_cast< WPARAM >( hb_parnint( 3 ) ), 0 );
         LPTSTR  text  = static_cast< LPTSTR >( hb_xgrab( iSize + 1 ) );
         SendMessage( hCB, CB_GETLBTEXT, iSize, reinterpret_cast< LPARAM >( text ) );
         HB_RETSTR( text );
         hb_xfree( text );
         break;
      }
      case CB_GETLBTEXTLEN:
         hb_retnint( SendMessage( hCB, CB_GETLBTEXTLEN, static_cast< WPARAM >( hb_parnint( 3 ) ), 0 ) );
         break;
      case CB_GETLOCALE:
#if ( _WIN32_IE >= 0x0600 )
      case CB_GETMINVISIBLE:
         hb_retnint( SendMessage( hCB, CB_GETMINVISIBLE, 0, 0 ) );
         break;
#endif
      case CB_GETTOPINDEX:
         hb_retnint( SendMessage( hCB, CB_GETTOPINDEX, 0, 0 ) );
         break;
      case CB_INITSTORAGE:
         break;
      case CB_INSERTSTRING:
         hb_retnint( SendMessage( hCB, CB_INSERTSTRING, static_cast< WPARAM >( hb_parnint( 3 ) ), reinterpret_cast< LPARAM >( HB_PARSTR( 4, &hText, nullptr ) ) ) );
         break;
      case CB_LIMITTEXT:
         SendMessage( hCB, CB_LIMITTEXT, hb_parni( 3 ), 0 );
         break;
      case CB_RESETCONTENT:
         SendMessage( hCB, CB_RESETCONTENT, 0, 0 );
         break;
      case CB_SELECTSTRING:
         hb_retnint( SendMessage( hCB, CB_SELECTSTRING, static_cast< WPARAM >( hb_parnint( 3 ) ), reinterpret_cast< LPARAM >( HB_PARSTR( 4, &hText, nullptr ) ) ) );
         break;
#if defined( CB_SETCUEBANNER )
      case CB_SETCUEBANNER:
         break;
#endif
      case CB_SETCURSEL:
         hb_retnint( SendMessage( hCB, CB_SETCURSEL, static_cast< WPARAM >( hb_parnint( 3 ) ), 0 ) );
         break;
      case CB_SETDROPPEDWIDTH:
         hb_retnint( SendMessage( hCB, CB_SETDROPPEDWIDTH, static_cast< WPARAM >( hb_parnint( 3 ) ), 0 ) );
         break;
      case CB_SETEDITSEL:
         break;
      case CB_SETEXTENDEDUI:
         SendMessage( hCB, CB_SETEXTENDEDUI, hb_parl( 3 ), 0 );
         break;
      case CB_SETHORIZONTALEXTENT:
         SendMessage( hCB, CB_SETHORIZONTALEXTENT, hb_parl( 3 ), 0 );
         break;
      case CB_SETITEMDATA:
         SendMessage( hCB, CB_SETITEMDATA, hb_parl( 3 ), static_cast< LPARAM >( hb_parnint( 4 ) ) );
         break;
      case CB_SETITEMHEIGHT:
         hb_retnint( SendMessage( hCB, CB_SETITEMHEIGHT, static_cast< WPARAM >( hb_parnint( 3 ) ), 0 ) );
         break;
      case CB_SETLOCALE:
         hb_retnint( SendMessage( hCB, CB_SETLOCALE, static_cast< WPARAM >( hb_parnint( 3 ) ), 0 ) );
         break;
#if ( _WIN32_IE >= 0x0600 )
      case CB_SETMINVISIBLE:
         hb_retl( SendMessage( hCB, CB_SETMINVISIBLE, static_cast< WPARAM >( hb_parnint( 3 ) ), 0 ) );
         break;
#endif
      case CB_SETTOPINDEX:
         hb_retl( SendMessage( hCB, CB_SETTOPINDEX, static_cast< WPARAM >( hb_parnint( 3 ) ), 0 ) ? FALSE : TRUE );
         break;
      case CB_SHOWDROPDOWN:
         SendMessage( hCB, CB_SHOWDROPDOWN, hb_parl( 3 ), 0 );
         break;
   }

   if( hText )
   {
      hb_strfree( hText );
   }
}
