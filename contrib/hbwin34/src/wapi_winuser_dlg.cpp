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

#include "hbwapi.hpp"
#include <hbvm.hpp>
#include <hbapiitm.hpp>
#include <windowsx.h>

// Application-defined callback used with the CreateDialog and DialogBox... It
// processes messages sent to a modal or modeless dialog box.
static BOOL CALLBACK wapi_DialogFuncProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
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
    hb_vmPushNumInt(message);

    if (message == WM_COMMAND)
    {
      hb_vmPushNumInt(wParam);
      if (HIWORD(wParam) == 0 || HIWORD(wParam) == 1)
      {
        hb_vmPushNumInt(lParam);
      }
      else
      {
        hbwapi_vmPush_HANDLE(reinterpret_cast<HWND>(lParam));
      }
      // TODO: rethink this. Called proc can do this on its own,
      //       no need to pass calculated params. Or, think of
      //       some generic solution.
      hb_vmPushInteger(static_cast<int>(HIWORD(wParam)));
      hb_vmPushInteger(static_cast<int>(LOWORD(wParam)));
      hb_vmDo(6);
    }
    else
    {
      hb_vmPushNumInt(wParam);
      hb_vmPushNumInt(lParam);
      hb_vmDo(4);
    }
  }

  return static_cast<BOOL>(hb_parnl(-1));
}

// Creates a modal dialog box from a dialog box template resource.

// WAPI_DIALOGBOXPARAM(hInstance, lpTemplate, hWndParent, dwInitParam) --> nResult
HB_FUNC(WAPI_DIALOGBOXPARAM)
{
  INT_PTR nResult = DialogBoxParam(hbwapi_par_raw_HINSTANCE(1), MAKEINTRESOURCE(hbwapi_par_INT(2)),
                                   hbwapi_par_raw_HWND(3), reinterpret_cast<DLGPROC>(wapi_DialogFuncProc),
                                   reinterpret_cast<LPARAM>(hb_itemGetSymbol(hb_param(4, Harbour::Item::SYMBOL))));
  hbwapi_SetLastError(GetLastError());
  hbwapi_ret_NINT(nResult);
}

// Destroys a modal dialog box, causing the system to end any processing for the
// dialog box.
HB_FUNC(WAPI_ENDDIALOG)
{
  BOOL bResult = EndDialog(hbwapi_par_raw_HWND(1), hbwapi_par_INT(2));
  hbwapi_SetLastError(GetLastError());
  hbwapi_ret_L(bResult);
}

HB_FUNC(WAPI_CHECKDLGBUTTON)
{
  BOOL bResult = CheckDlgButton(hbwapi_par_raw_HWND(1), hb_parni(2),
                                HB_ISNUM(3) ? hbwapi_par_UINT(3) : static_cast<UINT>(hb_parl(3)));
  hbwapi_SetLastError(GetLastError());
  hbwapi_ret_L(bResult);
}

HB_FUNC(WAPI_ISDLGBUTTONCHECKED)
{
  int iResult = IsDlgButtonChecked(hbwapi_par_raw_HWND(1), hb_parni(2));
  hbwapi_SetLastError(GetLastError());
  hb_retni(iResult);
}

// Sets the title or text of a control in a dialog box.
HB_FUNC(WAPI_SETDLGITEMTEXT)
{
  void *hStr;
  BOOL bResult = SetDlgItemText(hbwapi_par_raw_HWND(1), hbwapi_par_INT(2), HB_PARSTR(3, &hStr, nullptr));
  hbwapi_SetLastError(GetLastError());
  hbwapi_ret_L(bResult);
  hb_strfree(hStr);
}

// Retrieves the title or text associated with a control in a dialog box.
HB_FUNC(WAPI_GETDLGITEMTEXT)
{
  auto nSize =
      static_cast<HB_SIZE>(SendMessage(GetDlgItem(hbwapi_par_raw_HWND(1), hbwapi_par_INT(2)), WM_GETTEXTLENGTH, 0, 0));
  auto lpResult = static_cast<TCHAR *>(hb_xgrab((nSize + 1) * sizeof(TCHAR)));
  auto nResult = static_cast<HB_SIZE>(
      GetDlgItemText(hbwapi_par_raw_HWND(1), hbwapi_par_INT(2), lpResult, static_cast<int>(nSize + 1)));
  hbwapi_SetLastError(GetLastError());
  HB_RETSTRLEN(lpResult, nResult);
  hb_xfree(lpResult);
}

// Retrieves a handle to a control in the specified dialog box.
HB_FUNC(WAPI_GETDLGITEM)
{
  HWND hWnd = GetDlgItem(hbwapi_par_raw_HWND(1), hbwapi_par_INT(2));
  hbwapi_SetLastError(GetLastError());
  hbwapi_ret_raw_HWND(hWnd);
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

HB_FUNC(WAPI_GETDIALOGBASEUNITS)
{
  hb_retnl(GetDialogBaseUnits());
}

HB_FUNC(WAPI_SENDDLGITEMMESSAGE) // NOTE: unsafe function, may corrupt memory
{
  void *hText;
  HB_SIZE nLen;
  LPCTSTR szText = HB_PARSTR(5, &hText, &nLen);

  LRESULT result;

  if (szText)
  {
    szText = HB_STRUNSHARE(&hText, szText, nLen);
  }

  result = SendDlgItemMessage(hbwapi_par_raw_HWND(1), hb_parni(2), hbwapi_par_UINT(3), hbwapi_par_WPARAM(4),
                              szText ? reinterpret_cast<LPARAM>(szText) : hbwapi_par_LPARAM(5));
  hbwapi_SetLastError(GetLastError());
  hb_retnint(result);

  if (szText)
  {
    HB_STORSTRLEN(szText, nLen, 5);
  }
  else
  {
    hb_storc(nullptr, 5);
  }

  hb_strfree(hText);
}

#define _BUFFERSIZE 65534 // 64 KiB allows to build up to 255 items on the dialog

// Take an input pointer, return closest pointer that is
// aligned on a DWORD (4 byte) boundary.
static LPWORD s_AlignOnDWORD(LPWORD p)
{
  auto ul = reinterpret_cast<HB_PTRUINT>(p);

  ul += 3;
  ul >>= 2;
  ul <<= 2;

  return reinterpret_cast<LPWORD>(ul);
}

HB_FUNC(__WAPI_DLGTEMPLATE_RAW_NEW)
{
  WORD *p;
  WORD *pdlgtemplate = p = static_cast<WORD *>(hb_xgrabz(_BUFFERSIZE));
  WORD *pItems;

  // Parameters: 12 arrays
  // 1 for DLG template
  // 11 for item properties

  auto nItems = static_cast<WORD>(hb_parvni(1, 4));
  DWORD lStyle = hb_parvnl(1, 3);
  HB_SIZE nchar;

  // Start to fill in the DLGTEMPLATE information. Addressing by WORDs

  *p++ = 1;                       // version
  *p++ = 0xFFFF;                  // signature
  *p++ = LOWORD(hb_parvnl(1, 1)); // Help Id
  *p++ = HIWORD(hb_parvnl(1, 1));

  *p++ = LOWORD(hb_parvnl(1, 2)); // ext. style
  *p++ = HIWORD(hb_parvnl(1, 2));

  *p++ = LOWORD(lStyle);
  *p++ = HIWORD(lStyle);

  pItems = p;

  *p++ = static_cast<WORD>(nItems);           // NumberOfItems
  *p++ = static_cast<short>(hb_parvni(1, 5)); // x
  *p++ = static_cast<short>(hb_parvni(1, 6)); // y
  *p++ = static_cast<short>(hb_parvni(1, 7)); // cx
  *p++ = static_cast<short>(hb_parvni(1, 8)); // cy
  *p++ = static_cast<short>(0);               // Menu (ignored for now.)
  *p++ = static_cast<short>(0x00);            // Class also ignored

  if (hb_parinfa(1, 11) == Harbour::Item::STRING)
  {
    void *hText;
    LPCWSTR szText = hb_wstrnull(hb_parastr_u16(1, 11, HB_CDP_ENDIAN_NATIVE, &hText, &nchar));

    nchar = hb_wstrnlen(szText, nchar);

    if (nchar > 256)
    {
      nchar = 256;
    }

    hb_wstrncpy(reinterpret_cast<HB_WCHAR *>(p), szText, nchar);
    p += nchar + 1;

    hb_strfree(hText);
  }
  else
  {
    *p++ = 0;
  }

  // add in the wPointSize and szFontName here iff the DS_SETFONT bit on

  if ((lStyle & DS_SETFONT) != 0)
  {
    void *hText;
    LPCWSTR szText = hb_wstrnull(hb_parastr_u16(1, 15, HB_CDP_ENDIAN_NATIVE, &hText, &nchar));

    *p++ = static_cast<short>(hb_parvni(1, 12));
    *p++ = static_cast<short>(hb_parvni(1, 13));
    *p++ = static_cast<short>(hb_parvni(1, 14));

    nchar = hb_wstrnlen(szText, nchar);

    if (nchar > 256)
    {
      nchar = 256;
    }

    hb_wstrncpy(reinterpret_cast<HB_WCHAR *>(p), szText, nchar);
    p += nchar + 1;

    hb_strfree(hText);
  }

  for (WORD i = 1; i <= nItems; i++)
  {
    // make sure each item starts on a DWORD boundary
    p = s_AlignOnDWORD(p);

    *p++ = LOWORD(hb_parvnl(2, i)); // help id
    *p++ = HIWORD(hb_parvnl(2, i));

    *p++ = LOWORD(hb_parvnl(3, i)); // ext. style
    *p++ = HIWORD(hb_parvnl(3, i));

    *p++ = LOWORD(hb_parvnl(4, i)); // style
    *p++ = HIWORD(hb_parvnl(4, i));

    *p++ = static_cast<short>(hb_parvni(5, i)); // x
    *p++ = static_cast<short>(hb_parvni(6, i)); // y
    *p++ = static_cast<short>(hb_parvni(7, i)); // cx
    *p++ = static_cast<short>(hb_parvni(8, i)); // cy

    *p++ = LOWORD(hb_parvnl(9, i)); // id
    *p++ = HIWORD(hb_parvnl(9, i)); // id

    if (hb_parinfa(10, i) == Harbour::Item::STRING)
    {
      void *hText;
      LPCWSTR szText = hb_parastr_u16(10, i, HB_CDP_ENDIAN_NATIVE, &hText, &nchar);

      nchar = hb_wstrnlen(szText, nchar);

      if (nchar > 256)
      {
        nchar = 256;
      }

      hb_wstrncpy(reinterpret_cast<HB_WCHAR *>(p), szText, nchar);
      p += nchar + 1;

      hb_strfree(hText);
    }
    else
    {
      *p++ = 0xFFFF;
      *p++ = static_cast<WORD>(hb_parvni(10, i));
    }

    if (hb_parinfa(11, i) == Harbour::Item::STRING)
    {
      void *hText;
      LPCWSTR szText = hb_parastr_u16(11, i, HB_CDP_ENDIAN_NATIVE, &hText, &nchar);

      nchar = hb_wstrnlen(szText, nchar);

      if (nchar > 256)
      {
        nchar = 256;
      }

      hb_wstrncpy(reinterpret_cast<HB_WCHAR *>(p), szText, nchar);
      p += nchar + 1;

      hb_strfree(hText);
    }
    else
    {
      *p++ = 0xFFFF;
      *p++ = static_cast<WORD>(hb_parvni(11, i));
    }

    *p++ = 0x00; // extras (in array 12)

    // 768 is the maximum size of one item
    if ((reinterpret_cast<HB_PTRUINT>(p) - reinterpret_cast<HB_PTRUINT>(pdlgtemplate)) > _BUFFERSIZE - 768)
    {
      nItems = i;
      break;
    }
  }

  *pItems = static_cast<WORD>(nItems);

  p = s_AlignOnDWORD(p);

  hb_retclen(reinterpret_cast<char *>(pdlgtemplate),
             reinterpret_cast<HB_PTRUINT>(p) - reinterpret_cast<HB_PTRUINT>(pdlgtemplate));

  hb_xfree(pdlgtemplate);
}
