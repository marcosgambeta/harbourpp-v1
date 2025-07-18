//
// Header File common for WGU and WVG
//
// Copyright 2003 Peter Rees <peter@rees.co.nz> Rees Software & Systems Ltd
//

// $HB_BEGIN_LICENSE$
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file LICENSE.txt.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
//
// As a special exception, the Harbour Project gives permission for
// additional uses of the text contained in its release of Harbour.
//
// The exception is that, if you link the Harbour libraries with other
// files to produce an executable, this does not by itself cause the
// resulting executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of
// linking the Harbour library code into it.
//
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.
//
// This exception applies only to the code released by the Harbour
// Project under the name Harbour.  If you copy code from other
// Harbour Project or Free Software Foundation releases into a copy of
// Harbour, as the General Public License permits, the exception does
// not apply to the code that you add in this way.  To avoid misleading
// anyone as to the status of such modified files, you must delete
// this exception notice from them.
//
// If you write modifications of your own for Harbour, it is your choice
// whether to permit this exception to apply to your modifications.
// If you do not wish that, delete this exception notice.
// $HB_END_LICENSE$

#ifndef _WIN32_IE
   #ifdef __MINGW32__
      #include <_mingw.h>
   #endif
   #ifndef __MINGW64_VERSION_MAJOR
      #define _WIN32_IE  0x0500
   #endif
#endif

#include <hbapi.hpp>
#include "hbwapi.hpp"
#include "hbwinole.hpp"

#if defined(_MSC_VER)
#   pragma warning(push)
#   pragma warning(disable:4201)  // warning C4201: nonstandard extension used: nameless struct/union
#endif
#include <olectl.h>
#if defined(_MSC_VER)
#   pragma warning(pop)
#endif

#include <commctrl.h>
#include <commdlg.h>
#include <shellapi.h>

#include <hbgtcore.hpp>
#include <hbapicdp.hpp>
#include <hbapierr.hpp>
#include "hbapifs.hpp"
#include <hbapiitm.hpp>
#include <hbset.hpp>
#include <hbinit.hpp>
#include <inkey.ch>
#include <error.ch>
#include <hbvm.hpp>
#include <hbthread.hpp>
#include "hbgfxdef.ch"
#include <hbwinuni.hpp>

#include "hbgtwvg.ch"

HB_EXTERN_BEGIN

// -

#define BLACK                       RGB(0x00, 0x00, 0x00)
#define BLUE                        RGB(0x00, 0x00, 0x85)
#define GREEN                       RGB(0x00, 0x85, 0x00)
#define CYAN                        RGB(0x00, 0x85, 0x85)
#define RED                         RGB(0x85, 0x00, 0x00)
#define MAGENTA                     RGB(0x85, 0x00, 0x85)
#define BROWN                       RGB(0x85, 0x85, 0x00)
#define LIGHT_GRAY                  RGB(0xC6, 0xC6, 0xC6)
#define GRAY                        RGB(0x60, 0x60, 0x60)
#define BRIGHT_BLUE                 RGB(0x00, 0x00, 0xFF)
#define BRIGHT_GREEN                RGB(0x60, 0xFF, 0x60)
#define BRIGHT_CYAN                 RGB(0x60, 0xFF, 0xFF)
#define BRIGHT_RED                  RGB(0xF8, 0x00, 0x26)
#define BRIGHT_MAGENTA              RGB(0xFF, 0x60, 0xFF)
#define YELLOW                      RGB(0xFF, 0xFF, 0x00)
#define WHITE                       RGB(0xFF, 0xFF, 0xFF)

#define WM_MY_UPDATE_CARET          ( WM_USER + 0x0101 )

#define SYS_EV_MARK                 1000
#define HB_MSG_NOTIFYICON           ( WM_USER + 1399 )
#define HB_ID_NOTIFYICON            99

#if defined(__BORLANDC__) && ( __BORLANDC__ == 0x0550 )
   #ifdef __cplusplus
      extern "C" { STDAPI OleLoadPicture( LPSTREAM, LONG, BOOL, REFIID, PVOID * ); }
   #else
      STDAPI OleLoadPicture( LPSTREAM, LONG, BOOL, REFIID, PVOID * );
   #endif
#endif

// -

using wvtGradientFill = BOOL (WINAPI *)(
                      HDC        hdc,
                      PTRIVERTEX pVertex,
                      ULONG      dwNumVertex,
                      PVOID      pMesh,
                      ULONG      dwNumMesh,
                      ULONG      dwMode );

using wvtSetLayeredWindowAttributes = BOOL (WINAPI *)(
                      HWND       hwnd,
                      COLORREF   crKey,
                      BYTE       bAlpha,
                      DWORD      dwFlags );

// -

enum HB_gt_object_enum
{
   GTO_POINT          = 0,
   GTO_LINE           = 1,
   GTO_SQUARE         = 3,
   GTO_RECTANGLE      = 4,
   GTO_CIRCLE         = 5,
   GTO_DISK           = 7,
   // TODO: add other types
   GTO_TEXT           = 100,
} ;

// Event subsystem

enum HB_gt_event_enum
{
   GTEVENT_RESIZE     = 0,
   GTEVENT_CLOSE      = 1,
   GTEVENT_ICONIZE    = 2,
   GTEVENT_MAXH       = 3,
   GTEVENT_MAXV       = 4,
   GTEVENT_MAXIMIZE   = 5,
   GTEVENT_DEICONIZE  = 6,
   GTEVENT_SHUTDOWN   = 7
} ;

// -

// xHarbour compatible definitions
#if ! defined(K_SH_LEFT)
#define K_SH_LEFT           K_LEFT   // Shift-Left  == Left
#define K_SH_UP             K_UP     // Shift-Up    == Up
#define K_SH_RIGHT          K_RIGHT  // Shift-Right == Right
#define K_SH_DOWN           K_DOWN   // Shift-Down  == Down
#define K_SH_INS            K_INS    // Shift-Ins   == Ins
#define K_SH_DEL            K_DEL    // Shift-Del   == Del
#define K_SH_HOME           K_HOME   // Shift-Home  == Home
#define K_SH_END            K_END    // Shift-End   == End
#define K_SH_PGUP           K_PGUP   // Shift-PgUp  == PgUp
#define K_SH_PGDN           K_PGDN   // Shift-PgDn  == PgDn
#define K_SH_RETURN         K_RETURN // Shift-Enter == Enter
#define K_SH_ENTER          K_ENTER  // Shift-Enter == Enter
#endif

// -

#ifndef WS_EX_LAYERED
#define WS_EX_LAYERED  0x00080000
#endif

#ifndef INVALID_FILE_SIZE
#define INVALID_FILE_SIZE  ( DWORD ) 0xFFFFFFFF
#endif

#ifndef CC_ANYCOLOR
#define CC_ANYCOLOR  0x00000100
#endif

#ifndef IDC_HAND
#define IDC_HAND  MAKEINTRESOURCE( 32649 )
#endif

#ifndef GRADIENT_FILL_RECT_H
#define GRADIENT_FILL_RECT_H  0x00
#endif

#ifndef GCLP_HCURSOR
#define GCLP_HCURSOR  ( -12 )
#endif

#ifndef WM_MOUSEWHEEL
#define WM_MOUSEWHEEL  0x020A
#endif
#ifndef WM_ENTERSIZEMOVE
#define WM_ENTERSIZEMOVE  561
#endif
#ifndef WM_EXITSIZEMOVE
#define WM_EXITSIZEMOVE   562
#endif

#ifndef SWP_DEFERERASE
#define SWP_DEFERERASE  0x2000
#endif

#ifndef SW_NORMAL
#define SW_NORMAL  1
#endif

#ifndef SC_MAXIMIZE
#define SC_MAXIMIZE  0xF030
#endif

#ifndef TVIS_EXPANDPARTIAL
#define TVIS_EXPANDPARTIAL  0x0080
#endif

#ifndef LWA_ALPHA
#define LWA_ALPHA  0x00000002
#endif

struct HB_GT_PARAMS
{
   DWORD     exStyle;
   DWORD     style;
   int       x;
   int       y;
   int       width;
   int       height;
   PHB_ITEM  pParentGT;
   HB_BOOL   bVisible;
   HB_BOOL   bRowCols;
   HB_BOOL   bConfigured;
   int       iWndType;
};

using PHB_GT_PARAMS = HB_GT_PARAMS *;
