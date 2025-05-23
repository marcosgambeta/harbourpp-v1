//
// Windows API functions
//
// Copyright 2011 Vailton Renato (and others)
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

#include "hbwin.hpp"
#include "hbwapi.hpp"
#include <hbvm.hpp>
#include <hbapiitm.hpp>

#include <windowsx.h>

// Application-defined callback used with the CreateDialog and DialogBox... It
// processes messages sent to a modal or modeless dialog box.
static INT_PTR CALLBACK wapi_DialogFuncProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
  PHB_SYMB pSymbol;

  if (message == WM_INITDIALOG && lParam)
  {
    pSymbol = reinterpret_cast<PHB_SYMB>(lParam);
    SetWindowLongPtr(hDlg, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(pSymbol));
  }
  else
  {
    pSymbol = reinterpret_cast<PHB_SYMB>(GetWindowLongPtr(hDlg, GWLP_USERDATA));
  }

  if (pSymbol)
  {
    hb_vmPushSymbol(pSymbol);
    hb_vmPushNil();
    hb_vmPushPointer(hDlg);
    hb_vmPushLong(message);
    hb_vmPushNumInt(wParam);
    hb_vmPushNumInt(lParam);

    if (message == WM_COMMAND)
    {
      hb_vmPushInteger(static_cast<int>(HIWORD(wParam)));
      hb_vmPushInteger(static_cast<int>(LOWORD(wParam)));
      hb_vmDo(6);
    }
    else
    {
      hb_vmDo(4);
    }
  }

  return static_cast<BOOL>(hb_parnl(-1));
}

// Creates a modal dialog box from a dialog box template resource.
HB_FUNC(WAPI_DIALOGBOXPARAM)
{
  INT_PTR nResult =
      DialogBoxParam(hbwapi_par_raw_HINSTANCE(1),                                                     // hInstance
                     static_cast<LPCTSTR>(MAKEINTRESOURCE(hbwapi_par_INT(2))),                        // lpTemplate
                     hbwapi_par_raw_HWND(3),                                                          // hWndParent
                     wapi_DialogFuncProc,                                                             // lpDialogFunc
                     reinterpret_cast<LPARAM>(hb_itemGetSymbol(hb_param(4, Harbour::Item::SYMBOL)))); // dwInitParam

  hbwapi_SetLastError(GetLastError());
  hbwapi_ret_NINT(nResult);
}

// Destroys a modal dialog box, causing the system to end any processing for the
// dialog box.
HB_FUNC(WAPI_ENDDIALOG)
{
  hbwapi_ret_L(EndDialog(hbwapi_par_raw_HWND(1), hbwapi_par_INT(2)));
  hbwapi_SetLastError(GetLastError());
}

// Sets the title or text of a control in a dialog box.
HB_FUNC(WAPI_SETDLGITEMTEXT)
{
  void *hStr;
  int iResult = SetDlgItemText(hbwapi_par_raw_HWND(1), hbwapi_par_INT(2), HB_PARSTR(3, &hStr, nullptr));

  hbwapi_SetLastError(GetLastError());
  hbwapi_ret_NI(iResult);
  hb_strfree(hStr);
}

// Retrieves the title or text associated with a control in a dialog box.
HB_FUNC(WAPI_GETDLGITEMTEXT)
{
  HWND nItem = GetDlgItem(hbwapi_par_raw_HWND(1), hbwapi_par_INT(2));
  auto nSize = static_cast<int>(SendMessage(nItem, WM_GETTEXTLENGTH, 0, 0));
  auto lpResult = static_cast<TCHAR *>(hb_xgrab((nSize + 1) * sizeof(TCHAR)));

  UINT nResult = GetDlgItemText(hbwapi_par_raw_HWND(1), hbwapi_par_INT(2), lpResult, nSize + 1);

  HB_RETSTRLEN(lpResult, static_cast<HB_SIZE>(nResult));
  hbwapi_SetLastError(GetLastError());
  hb_xfree(lpResult);
}

// Retrieves a handle to a control in the specified dialog box.
HB_FUNC(WAPI_GETDLGITEM)
{
  hbwapi_ret_raw_HWND(GetDlgItem(hbwapi_par_raw_HWND(1), hbwapi_par_INT(2)));
  hbwapi_SetLastError(GetLastError());
}

// Adds a string to a list in a combo box.
HB_FUNC(WAPI_COMBOBOX_ADDSTRING)
{
  void *hStr;
  int iResult = ComboBox_AddString(hbwapi_par_raw_HWND(1), HB_PARSTR(2, &hStr, nullptr));

  hbwapi_SetLastError(GetLastError());
  hbwapi_ret_NI(iResult);
  hb_strfree(hStr);
}
