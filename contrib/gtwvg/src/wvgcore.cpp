/*
 * Copyright 2007-2012 Pritpal Bedi <bedipritpal@hotmail.com>
 * Based on:
 *
 * Video subsystem for Windows using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
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

/* GUI Drawing Function */

#include "hbwinole.hpp"
#include "gtwvg.hpp"

#define __SETGUI__

static void hb_wvt_DrawBoxRaised(HDC hdc, int iTop, int iLeft, int iBottom, int iRight);
static void hb_wvt_DrawBoxRecessed(HDC hdc, int iTop, int iLeft, int iBottom, int iRight);
static void hb_wvt_DrawOutline(HDC hdc, int iTop, int iLeft, int iBottom, int iRight);
static void hb_wvt_DrawBoxGet(HDC hdc, int iTop, int iLeft, int iBottom, int iRight);
static void hb_wvt_DrawBoxGroup(HDC hdc, int iTop, int iLeft, int iBottom, int iRight);
static void hb_wvt_DrawBoxGroupRaised(HDC hdc, int iTop, int iLeft, int iBottom, int iRight);
static void hb_wvt_DrawToolButtonFlat(HDC hdc, int iTop, int iLeft, int iBottom, int iRight);
static void hb_wvt_DrawToolButtonUp(HDC hdc, int iTop, int iLeft, int iBottom, int iRight);
static void hb_wvt_DrawToolButtonDown(HDC hdc, int iTop, int iLeft, int iBottom, int iRight);

HB_FUNC( WVT_CORE )
{
   /* Retained for legacy code. */
}

void hb_wvt_GetStringAttrib(int top, int left, int bottom, int right, HB_BYTE * sBuffer, HB_BYTE * sAttrib)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_wvt_GetStringAttrib( %d, %d, %d, %d, %p, %p )", top, left, bottom, right, static_cast<void*>(sBuffer), static_cast<void*>(sAttrib)));
#endif

   int irow, icol, j;

   for( j = 0, irow = top; irow <= bottom; irow++ )
   {
      for( icol = left; icol <= right; icol++ )
      {
         int       iColor;
         HB_BYTE   bAttr;
         HB_USHORT usChar;

         if( hb_gtGetScrChar(irow, icol, &iColor, &bAttr, &usChar) == Harbour::FAILURE )
         {
            break;
         }

         sBuffer[j] = static_cast<HB_BYTE>(usChar);
         sAttrib[j] = static_cast<HB_BYTE>(iColor);
         j++;
      }
   }
}

void hb_wvt_PutStringAttrib(int top, int left, int bottom, int right, HB_BYTE * sBuffer, HB_BYTE * sAttrib)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_wvt_PutStringAttrib( %d, %d, %d, %d, %p, %p )", top, left, bottom, right, static_cast<void*>(sBuffer), static_cast<void*>(sAttrib)));
#endif

   int irow, icol, j;

   hb_gtBeginWrite();
   for( j = 0, irow = top; irow <= bottom; irow++ )
   {
      for( icol = left; icol <= right; icol++ )
      {
         if( hb_gtPutScrChar(irow, icol, sAttrib[j], 0, sBuffer[j]) == Harbour::FAILURE )
         {
            break;
         }
         j++;
      }
   }
   hb_gtEndWrite();
}

/*
 *               Courtesy - Augusto Infante - Thanks
 */
IPicture * hb_wvt_gtLoadPictureFromResource(LPCTSTR resource, LPCTSTR section)
{
   HRSRC res = 0;
   LPVOID iPicture = nullptr;
   HANDLE hInstance;

   if( hb_winmainArgGet(&hInstance, nullptr, nullptr) )
   {
      res = FindResource(static_cast<HINSTANCE>(hInstance), resource, section);
   }

   if( res )
   {
      IStream * iStream   = nullptr;
      HGLOBAL   mem       = LoadResource(GetModuleHandle(nullptr), res);
      void *    data      = LockResource(mem);
      LONG      nFileSize = static_cast<LONG>(SizeofResource(GetModuleHandle(nullptr), res));
      HGLOBAL   hGlobal   = GlobalAlloc(GMEM_MOVEABLE, nFileSize);
      LPVOID    pvData    = GlobalLock(hGlobal);

      memcpy(pvData, data, nFileSize);

      GlobalUnlock(hGlobal);

      CreateStreamOnHGlobal(hGlobal, TRUE, &iStream);

      OleLoadPicture(iStream, nFileSize, TRUE, HB_ID_REF(IID_IPicture), &iPicture);

      FreeResource(mem);
   }

   return static_cast<IPicture*>(iPicture);
}

IPicture * hb_wvt_gtLoadPicture(LPCTSTR image)
{
   IStream * iStream;
   LPVOID    iPicture = nullptr;
   HGLOBAL   hGlobal;
   HANDLE    hFile;
   DWORD     nFileSize;
   DWORD     nReadByte;

   hFile = CreateFile(image, GENERIC_READ, 0, nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr);
   if( hFile != INVALID_HANDLE_VALUE )
   {
      nFileSize = GetFileSize(hFile, nullptr);

      if( nFileSize != INVALID_FILE_SIZE )
      {
         hGlobal = GlobalAlloc(GPTR, nFileSize);

         if( hGlobal )
         {
            if( ReadFile(hFile, hGlobal, nFileSize, &nReadByte, nullptr) )
            {
               CreateStreamOnHGlobal(hGlobal, TRUE, &iStream);
               OleLoadPicture(iStream, nFileSize, TRUE, HB_ID_REF(IID_IPicture), &iPicture);
            }
            GlobalFree(hGlobal);
         }
      }
      CloseHandle(hFile);
   }

   return static_cast<IPicture*>(iPicture);
}

HB_BOOL hb_wvt_gtRenderPicture(int x, int y, int wd, int ht, IPicture * iPicture, HB_BOOL bDoNotScale)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   LONG    lWidth, lHeight;
   int     xe, ye;
   HRGN    hrgn1;
   POINT   lpp     = {0, 0};
   HB_BOOL bResult = HB_FALSE;
   HDC     hdc     = _s->hdc;

   if( iPicture )
   {
      HB_VTBL(iPicture)->get_Width(HB_THIS_(iPicture) & lWidth);
      HB_VTBL(iPicture)->get_Height(HB_THIS_(iPicture) & lHeight);

      if( bDoNotScale )
      {
         int iWd, iHt;

         if( lHeight > lWidth )
         {
            iWd = static_cast<int>(static_cast<double>(ht) * lWidth / lHeight);
            iWd = HB_MIN(iWd, wd);
            iHt = static_cast<int>(static_cast<double>(iWd) * lHeight / lWidth);
         }
         else
         {
            iHt = static_cast<int>(static_cast<double>(wd) * lHeight / lWidth);
            iHt = HB_MIN(iHt, ht);
            iWd = static_cast<int>(static_cast<double>(iHt) * lWidth / lHeight);
         }
         x  += abs((iWd - wd) / 2);
         y  += abs((iHt - ht) / 2);
         wd  = iWd;
         ht  = iHt;
      }
      xe = x + wd - 1;
      ye = y + ht - 1;

      GetViewportOrgEx(hdc, &lpp);

      hrgn1 = CreateRectRgn(lpp.x + x, lpp.y + y, lpp.x + xe, lpp.y + ye);
      SelectClipRgn(hdc, hrgn1);

      HB_VTBL(iPicture)->Render(HB_THIS_(iPicture) hdc, x, y, wd, ht, 0, lHeight, lWidth, -lHeight, nullptr);

      SelectClipRgn(hdc, nullptr);
      DeleteObject(hrgn1);

      if( _s->bGui )
      {
         hdc = _s->hGuiDC;

         GetViewportOrgEx(hdc, &lpp);

         hrgn1 = CreateRectRgn(lpp.x + x, lpp.y + y, lpp.x + xe, lpp.y + ye);
         SelectClipRgn(hdc, hrgn1);

         HB_VTBL(iPicture)->Render(HB_THIS_(iPicture) hdc, x, y, wd, ht, 0, lHeight, lWidth, -lHeight, nullptr);

         SelectClipRgn(hdc, nullptr);
         DeleteObject(hrgn1);
      }

      bResult = HB_TRUE;
   }

   return bResult;
}

HB_BOOL hb_wvt_gtDestroyPicture(IPicture * iPicture)
{
   HB_BOOL bResult = HB_FALSE;

   if( iPicture )
   {
      HB_VTBL(iPicture)->Release(HB_THIS(iPicture));
      bResult = HB_TRUE;
   }

   return bResult;
}

POINT  hb_wvt_gtGetXYFromColRow(int col, int row)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   POINT xy;

   xy.x = col * _s->PTEXTSIZE.x;
   xy.y = row * _s->PTEXTSIZE.y;

   return xy;
}

/*
 *                 Modeless Dialogs Implementation
 */

BOOL CALLBACK hb_wvt_gtDlgProcMLess(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int      iIndex, iType;
   long int lReturn = 0;
   PHB_ITEM pFunc   = nullptr;

   iType = 0;

   for( iIndex = 0; iIndex < WVT_DLGML_MAX; iIndex++ )
   {
      if( (_s->hDlgModeless[iIndex] != nullptr) && (_s->hDlgModeless[iIndex] == hDlg) )
      {
         if( _s->pFunc[iIndex] != nullptr )
         {
            pFunc = _s->pFunc[iIndex];
            iType = _s->iType[iIndex];
         }
         break;
      }
   }

   if( pFunc )
   {
      switch( iType )
      {
         case 1:  /* Function Name */
            if( hb_vmRequestReenter() )
            {
               hb_vmPushDynSym(static_cast<PHB_DYNS>(pFunc));
               hb_vmPushNil();
               hb_vmPushNumInt(static_cast<HB_MAXINT>(reinterpret_cast<HB_PTRUINT>(hDlg)));
               hb_vmPushNumInt(message);
               hb_vmPushNumInt(wParam);
               hb_vmPushNumInt(lParam);
               hb_vmDo(4);
               lReturn = hb_parnl(-1);
               hb_vmRequestRestore();
            }
            break;

         case 2:  /* Block */
            /* eval the codeblock */
            if( HB_IS_BLOCK(pFunc) )
            {
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushEvalSym();
                  hb_vmPush(_s->pFunc[iIndex]);
                  hb_vmPushNumInt(static_cast<HB_MAXINT>(reinterpret_cast<HB_PTRUINT>(hDlg)));
                  hb_vmPushNumInt(message);
                  hb_vmPushNumInt(wParam);
                  hb_vmPushNumInt(lParam);
                  hb_vmSend(4);
                  lReturn = hb_parnl(-1);
                  hb_vmRequestRestore();
               }
            }
            else
            {
               /* TODO: internal error: missing codeblock */
            }
            break;
      }
   }

   switch( message )
   {
      case WM_COMMAND:
         switch( LOWORD(wParam) )
         {
            case IDOK:
               DestroyWindow(hDlg);
               lReturn = 1;
               break;

            case IDCANCEL:
               DestroyWindow(hDlg);
               lReturn = 0;
               break;
         }
         break;

      case WM_NCDESTROY:
         if( _s->pFunc[iIndex] != nullptr && _s->iType[iIndex] == 2 )
         {
            hb_itemRelease(static_cast<PHB_ITEM>(_s->pFunc[iIndex]));
         }
         _s->hDlgModeless[iIndex] = nullptr;
         _s->pFunc[iIndex]        = nullptr;
         _s->iType[iIndex]        = 0;
         lReturn = 0;
         break;
   }

   return lReturn;
}

BOOL CALLBACK hb_wvt_gtDlgProcModal(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int      iIndex, iType;
   long int lReturn = 0;
   PHB_ITEM pFunc   = nullptr;
   int      iFirst  = static_cast<int>(lParam);

   if( iFirst > 0 && iFirst <= WVT_DLGMD_MAX )
   {
      _s->hDlgModal[iFirst - 1] = hDlg;
      SendMessage(hDlg, WM_INITDIALOG, 0, 0);
      return lReturn;
   }

   iType = 0;

   for( iIndex = 0; iIndex < WVT_DLGMD_MAX; iIndex++ )
   {
      if( (_s->hDlgModal[iIndex] != nullptr) && (_s->hDlgModal[iIndex] == hDlg) )
      {
         if( _s->pFuncModal[iIndex] != nullptr )
         {
            pFunc = _s->pFuncModal[iIndex];
            iType = _s->iTypeModal[iIndex];
         }
         break;
      }
   }

   if( pFunc )
   {
      switch( iType )
      {
         case 1:  /* Function Name */
            if( hb_vmRequestReenter() )
            {
               hb_vmPushDynSym(static_cast<PHB_DYNS>(pFunc));
               hb_vmPushNil();
               hb_vmPushNumInt(static_cast<HB_MAXINT>(reinterpret_cast<HB_PTRUINT>(hDlg)));
               hb_vmPushNumInt(message);
               hb_vmPushNumInt(wParam);
               hb_vmPushNumInt(lParam);
               hb_vmDo(4);
               lReturn = hb_parnl(-1);
               hb_vmRequestRestore();
            }
            break;

         case 2:  /* Block */
            /* eval the codeblock */
            if( HB_IS_BLOCK(pFunc) )
            {
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushEvalSym();
                  hb_vmPush(pFunc);
                  hb_vmPushNumInt(static_cast<HB_MAXINT>(reinterpret_cast<HB_PTRUINT>(hDlg)));
                  hb_vmPushNumInt(message);
                  hb_vmPushNumInt(wParam);
                  hb_vmPushNumInt(lParam);
                  hb_vmSend(4);
                  lReturn = hb_parnl(-1);
                  hb_vmRequestRestore();
               }
            }
            else
            {
               /* TODO: internal error: missing codeblock */
            }
            break;
      }
   }

   switch( message )
   {
      case WM_COMMAND:
         switch( LOWORD(wParam) )
         {
            case IDOK:
               EndDialog(hDlg, IDOK);
               lReturn = 0;
               break;

            case IDCANCEL:
               EndDialog(hDlg, IDCANCEL);
               lReturn = 0;
               break;
         }
         break;

      case WM_NCDESTROY:
         if( _s->pFuncModal[iIndex] != nullptr && _s->iTypeModal[iIndex] == 2 )
         {
            hb_itemRelease(static_cast<PHB_ITEM>(_s->pFuncModal[iIndex]));
         }
         _s->hDlgModal[iIndex] = nullptr;
         _s->pFuncModal[iIndex] = nullptr;
         _s->iTypeModal[iIndex] = 0;
         lReturn = 0;
         break;
   }

   return lReturn;
}

HB_BOOL hb_wvt_DrawImage(HDC hdc, int x, int y, int wd, int ht, LPCTSTR lpImage, HB_BOOL bDoNotScale)
{
   HGLOBAL hGlobal;
   HANDLE  hFile;
   DWORD   nFileSize;
   DWORD   nReadByte;
   LONG    lWidth, lHeight;
   int     xe, ye;
   HRGN    hrgn1;
   POINT   lpp     = {0, 0};
   HB_BOOL bResult = HB_FALSE;

   hFile = CreateFile(lpImage, GENERIC_READ, 0, nullptr, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr);

   if( hFile != INVALID_HANDLE_VALUE )
   {
      nFileSize = GetFileSize(hFile, nullptr);

      if( nFileSize != INVALID_FILE_SIZE )
      {
         hGlobal = GlobalAlloc(GPTR, nFileSize);

         if( hGlobal )
         {
            if( ReadFile(hFile, hGlobal, nFileSize, &nReadByte, nullptr) )
            {
               IStream *   iStream;
               IPicture *  iPicture;
               IPicture ** iPictureRef = &iPicture;

               CreateStreamOnHGlobal(hGlobal, TRUE, &iStream);
               OleLoadPicture(iStream, nFileSize, TRUE, HB_ID_REF(IID_IPicture), reinterpret_cast<LPVOID*>(iPictureRef));

               if( iPicture )
               {
                  HB_VTBL(iPicture)->get_Width(HB_THIS_(iPicture) & lWidth);
                  HB_VTBL(iPicture)->get_Height(HB_THIS_(iPicture) & lHeight);

                  if( bDoNotScale )
                  {
                     int iWd, iHt;

                     if( lHeight > lWidth )
                     {
                        iWd = static_cast<int>(static_cast<double>(ht) * lWidth / lHeight);
                        iWd = HB_MIN(iWd, wd);
                        iHt = static_cast<int>(static_cast<double>(iWd) * lHeight / lWidth);
                     }
                     else
                     {
                        iHt = static_cast<int>(static_cast<double>(wd) * lHeight / lWidth);
                        iHt = HB_MIN(iHt, ht);
                        iWd = static_cast<int>(static_cast<double>(iHt) * lWidth / lHeight);
                     }
                     x  += abs((iWd - wd) / 2);
                     y  += abs((iHt - ht) / 2);
                     wd  = iWd;
                     ht  = iHt;
                  }
                  xe = x + wd - 1;
                  ye = y + ht - 1;

                  GetViewportOrgEx(hdc, &lpp);

                  hrgn1 = CreateRectRgn(lpp.x + x, lpp.y + y, lpp.x + xe, lpp.y + ye);
                  SelectClipRgn(hdc, hrgn1);

                  HB_VTBL(iPicture)->Render(HB_THIS_(iPicture) hdc, x, y, wd, ht, 0, lHeight, lWidth, -lHeight, nullptr);

                  SelectClipRgn(hdc, nullptr);
                  DeleteObject(hrgn1);

                  HB_VTBL(iPicture)->Release(HB_THIS(iPicture));
                  bResult = HB_TRUE;
               }
            }
            GlobalFree(hGlobal);
         }
      }
      CloseHandle(hFile);
   }
   return bResult;
}

static void hb_wvt_DrawBoxRaised(HDC hdc, int iTop, int iLeft, int iBottom, int iRight)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   SelectObject(hdc, _s->pGUI->penWhiteDim);
   MoveToEx(hdc, iLeft, iTop, nullptr);             /* Top Inner */
   LineTo(hdc, iRight, iTop);
   MoveToEx(hdc, iLeft, iTop, nullptr);             /* Left Inner */
   LineTo(hdc, iLeft, iBottom);

   SelectObject(hdc, _s->pGUI->penWhite);
   MoveToEx(hdc, iLeft - 1, iTop - 1, nullptr);     /* Top Outer */
   LineTo(hdc, iRight + 1, iTop - 1);
   MoveToEx(hdc, iLeft - 1, iTop - 1, nullptr);     /* Left Outer */
   LineTo(hdc, iLeft - 1, iBottom + 1);

   SelectObject(hdc, _s->pGUI->penDarkGray);
   MoveToEx(hdc, iLeft, iBottom, nullptr);          /* Bottom Inner */
   LineTo(hdc, iRight, iBottom);
   MoveToEx(hdc, iRight, iBottom, nullptr);         /* Right Inner */
   LineTo(hdc, iRight, iTop);

   SelectObject(hdc, _s->pGUI->penBlack);
   MoveToEx(hdc, iLeft - 1, iBottom + 1, nullptr);  /* Bottom Outer */
   LineTo(hdc, iRight + 1 + 1, iBottom + 1);
   MoveToEx(hdc, iRight + 1, iTop - 1, nullptr);    /* Right Outer */
   LineTo(hdc, iRight + 1, iBottom + 1);
}

static void hb_wvt_DrawBoxRecessed(HDC hdc, int iTop, int iLeft, int iBottom, int iRight)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   SelectObject(hdc, _s->pGUI->penWhiteDim);
   MoveToEx(hdc, iRight, iTop, nullptr);            /* Right Inner */
   LineTo(hdc, iRight, iBottom);
   MoveToEx(hdc, iLeft, iBottom, nullptr);          /* Bottom Inner */
   LineTo(hdc, iRight, iBottom);

   SelectObject(hdc, _s->pGUI->penWhite);
   MoveToEx(hdc, iRight + 1, iTop - 1, nullptr);    /* Right Outer */
   LineTo(hdc, iRight + 1, iBottom + 1);
   MoveToEx(hdc, iLeft - 1, iBottom + 1, nullptr);  /* Bottom Outer */
   LineTo(hdc, iRight + 2, iBottom + 1);

   SelectObject(hdc, _s->pGUI->penBlack);
   MoveToEx(hdc, iLeft, iTop, nullptr);             /* Left Inner */
   LineTo(hdc, iLeft, iBottom);
   MoveToEx(hdc, iLeft, iTop, nullptr);             /* Top Inner */
   LineTo(hdc, iRight, iTop);

   SelectObject(hdc, _s->pGUI->penDarkGray);
   MoveToEx(hdc, iLeft - 1, iTop - 1, nullptr);     /* Left Outer */
   LineTo(hdc, iLeft - 1, iBottom + 1);
   MoveToEx(hdc, iLeft - 1, iTop - 1, nullptr);     /* Top Outer */
   LineTo(hdc, iRight + 1, iTop - 1);
}

static void hb_wvt_DrawOutline(HDC hdc, int iTop, int iLeft, int iBottom, int iRight)
{
   MoveToEx(hdc, iLeft, iTop, nullptr);             /* Top */
   LineTo(hdc, iRight, iTop);

   MoveToEx(hdc, iLeft, iTop, nullptr);             /* Left */
   LineTo(hdc, iLeft, iBottom);

   MoveToEx(hdc, iLeft, iBottom, nullptr);          /* Bottom */
   LineTo(hdc, iRight, iBottom);

   MoveToEx(hdc, iRight, iTop, nullptr);            /* Right */
   LineTo(hdc, iRight, iBottom + 1);
}

static void hb_wvt_DrawBoxGet(HDC hdc, int iTop, int iLeft, int iBottom, int iRight)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   SelectObject(hdc, _s->pGUI->penBlack);
   MoveToEx(hdc, iLeft - 1, iTop - 1, nullptr);     /* Top Inner */
   LineTo(hdc, iRight - 1, iTop - 1);
   MoveToEx(hdc, iLeft - 1, iTop - 1, nullptr);     /* Left Inner */
   LineTo(hdc, iLeft - 1, iBottom - 1);

   SelectObject(hdc, _s->pGUI->penDarkGray);
   MoveToEx(hdc, iLeft - 2, iTop - 2, nullptr);     /* Top Outer */
   LineTo(hdc, iRight, iTop - 2);
   MoveToEx(hdc, iLeft - 2, iTop - 2, nullptr);     /* Left Outer */
   LineTo(hdc, iLeft - 2, iBottom);
}

static void hb_wvt_DrawBoxGroup(HDC hdc, int iTop, int iLeft, int iBottom, int iRight)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   SelectObject(hdc, _s->pGUI->penDarkGray);

   MoveToEx(hdc, iRight, iTop, nullptr);            /* Right Inner */
   LineTo(hdc, iRight, iBottom);

   MoveToEx(hdc, iLeft, iBottom, nullptr);          /* Bottom Inner */
   LineTo(hdc, iRight, iBottom);

   MoveToEx(hdc, iLeft - 1, iTop - 1, nullptr);     /* Left Outer */
   LineTo(hdc, iLeft - 1, iBottom + 1);

   MoveToEx(hdc, iLeft - 1, iTop - 1, nullptr);     /* Top Outer */
   LineTo(hdc, iRight + 1, iTop - 1);

   SelectObject(hdc, _s->pGUI->penWhite);

   MoveToEx(hdc, iRight + 1, iTop, nullptr);        /* Right Outer */
   LineTo(hdc, iRight + 1, iBottom + 1);

   MoveToEx(hdc, iLeft - 1, iBottom + 1, nullptr);  /* Bottom Outer */
   LineTo(hdc, iRight + 1 + 1, iBottom + 1);

   MoveToEx(hdc, iLeft, iTop, nullptr);             /* Left Inner */
   LineTo(hdc, iLeft, iBottom);

   MoveToEx(hdc, iLeft, iTop, nullptr);             /* Top Inner */
   LineTo(hdc, iRight, iTop);
}

static void hb_wvt_DrawBoxGroupRaised(HDC hdc, int iTop, int iLeft, int iBottom, int iRight)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   SelectObject(hdc, _s->pGUI->penWhite);

   MoveToEx(hdc, iRight, iTop, nullptr);            /* Right Inner */
   LineTo(hdc, iRight, iBottom);

   MoveToEx(hdc, iLeft, iBottom, nullptr);          /* Bottom Inner */
   LineTo(hdc, iRight, iBottom);

   MoveToEx(hdc, iLeft - 1, iTop - 1, nullptr);     /* Left Outer */
   LineTo(hdc, iLeft - 1, iBottom + 1);

   MoveToEx(hdc, iLeft - 1, iTop - 1, nullptr);     /* Top Outer */
   LineTo(hdc, iRight + 1, iTop - 1);

   SelectObject(hdc, _s->pGUI->penDarkGray);

   MoveToEx(hdc, iRight + 1, iTop, nullptr);        /* Right Outer */
   LineTo(hdc, iRight + 1, iBottom + 1);

   MoveToEx(hdc, iLeft - 1, iBottom + 1, nullptr);  /* Bottom Outer */
   LineTo(hdc, iRight + 1 + 1, iBottom + 1);

   MoveToEx(hdc, iLeft, iTop, nullptr);             /* Left Inner */
   LineTo(hdc, iLeft, iBottom);

   MoveToEx(hdc, iLeft, iTop, nullptr);             /* Top Inner */
   LineTo(hdc, iRight, iTop);
}

static void hb_wvt_DrawToolButtonFlat(HDC hdc, int iTop, int iLeft, int iBottom, int iRight)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   SelectObject(hdc, _s->pGUI->penGray);

   MoveToEx(hdc, iRight, iTop, nullptr);      /* Right */
   LineTo(hdc, iRight, iBottom + 1);

   MoveToEx(hdc, iLeft, iBottom, nullptr);    /* Bottom */
   LineTo(hdc, iRight, iBottom);

   MoveToEx(hdc, iLeft, iTop, nullptr);       /* Left */
   LineTo(hdc, iLeft, iBottom);

   MoveToEx(hdc, iLeft, iTop, nullptr);       /* Top */
   LineTo(hdc, iRight, iTop);
}

static void hb_wvt_DrawToolButtonUp(HDC hdc, int iTop, int iLeft, int iBottom, int iRight)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   SelectObject(hdc, _s->pGUI->penBlack);

   MoveToEx(hdc, iRight, iTop, nullptr);      /* Right */
   LineTo(hdc, iRight, iBottom + 1);

   MoveToEx(hdc, iLeft, iBottom, nullptr);    /* Bottom */
   LineTo(hdc, iRight, iBottom);

   SelectObject(hdc, _s->pGUI->penWhite);

   MoveToEx(hdc, iLeft, iTop, nullptr);       /* Left */
   LineTo(hdc, iLeft, iBottom);

   MoveToEx(hdc, iLeft, iTop, nullptr);       /* Top */
   LineTo(hdc, iRight, iTop);
}

static void hb_wvt_DrawToolButtonDown(HDC hdc, int iTop, int iLeft, int iBottom, int iRight)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   SelectObject(hdc, _s->pGUI->penWhite);

   MoveToEx(hdc, iRight, iTop, nullptr);      /* Right */
   LineTo(hdc, iRight, iBottom + 1);

   MoveToEx(hdc, iLeft, iBottom, nullptr);    /* Bottom */
   LineTo(hdc, iRight, iBottom);

   SelectObject(hdc, _s->pGUI->penBlack);

   MoveToEx(hdc, iLeft, iTop, nullptr);       /* Left */
   LineTo(hdc, iLeft, iBottom);

   MoveToEx(hdc, iLeft, iTop, nullptr);       /* Top */
   LineTo(hdc, iRight, iTop);
}

static COLORREF hb_wvt_BgColorParam(int iParam)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   COLORREF color;

   if( HB_ISNUM(iParam) )
   {
      color = static_cast<COLORREF>(hb_parnl(iParam));
   }
   else
   {
      int iColor = HB_ISCHAR(iParam) ? hb_gtColorToN(hb_parc(iParam)) : -1;
      if( iColor == -1 )
      {
         iColor = hb_gtGetCurrColor();
      }
      color = _s->COLORS[((iColor >> 4) & 0x0f)];
   }

   return color;
}

static COLORREF hb_wvt_FgColorParam(int iParam)
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   COLORREF color;

   if( HB_ISNUM(iParam) )
   {
      color = static_cast<COLORREF>(hb_parnl(iParam));
   }
   else
   {
      int iColor = HB_ISCHAR(iParam) ? hb_gtColorToN(hb_parc(iParam)) : -1;
      if( iColor == -1 )
      {
         iColor = hb_gtGetCurrColor();
      }
      color = _s->COLORS[((iColor >> 4) & 0x0f)];
   }

   return color;
}

/* wvt_SetPen( nPenStyle, nWidth, nColor ) */
HB_FUNC( WVT_SETPEN )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int      iPenWidth, iPenStyle;
   COLORREF crColor;
   HPEN     hPen;

   if( !HB_ISNUM(1) )
   {
      hb_retl(false);
   }

   iPenStyle = hb_parni(1);
   iPenWidth = hb_parni(2);
   crColor   = static_cast<COLORREF>(hb_parnldef(3, RGB(0, 0, 0)));

   hPen = CreatePen(iPenStyle, iPenWidth, crColor);

   if( hPen )
   {
      if( _s->currentPen )
      {
         DeleteObject(_s->currentPen);
      }
      _s->currentPen = hPen;

      hb_retl(true);
   }
   else
   {
      hb_retl(false);
   }
}

/* wvt_SetBrush( nStyle, nColor, [nHatch] ) */
HB_FUNC( WVT_SETBRUSH )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   HBRUSH   hBrush;
   LOGBRUSH lb = {0, 0, 0};

   if( !HB_ISNUM(1) )
   {
      hb_retl(false);
   }

   lb.lbStyle = hb_parnl(1);
   lb.lbColor = static_cast<COLORREF>(hb_parnldef(2, RGB(0, 0, 0)));
   lb.lbHatch = hb_parnl(3);
   hBrush = CreateBrushIndirect(&lb);
   if( hBrush )
   {
      if( _s->currentBrush )
      {
         DeleteObject(_s->currentBrush);
      }
      _s->currentBrush = hBrush;

      hb_retl(true);
   }
   else
   {
      hb_retl(false);
   }
}

/* wvt_DrawBoxGet(nRow, nCol, nWidth) */
HB_FUNC( WVT_DRAWBOXGET )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   POINT xy = {0, 0};
   POINT yz = {0, 0};

   xy = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));
   yz = hb_wvt_gtGetXYFromColRow((hb_parni(2) + hb_parni(3)), hb_parni(1) + 1);

   hb_wvt_DrawBoxGet(_s->hdc, xy.y, xy.x, yz.y, yz.x);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      hb_wvt_DrawBoxGet(_s->hGuiDC, xy.y, xy.x, yz.y, yz.x);
   }
   #endif
}

/* wvt_DrawBoxRaised( nTop, nLeft, nBottom, nRight, aPxlOff ) */
HB_FUNC( WVT_DRAWBOXRAISED )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);

   hb_wvt_DrawBoxRaised(_s->hdc, iTop - 1, iLeft - 1, iBottom + 1, iRight + 1);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      hb_wvt_DrawBoxRaised(_s->hGuiDC, iTop - 1, iLeft - 1, iBottom + 1, iRight + 1);
   }
   #endif
}

/* wvt_DrawBoxRecessed( nTop, nLeft, nBottom, nRight, aPxlOff ) */
HB_FUNC( WVT_DRAWBOXRECESSED )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);

   hb_wvt_DrawBoxRecessed(_s->hdc, iTop - 1, iLeft - 1, iBottom + 1, iRight + 1);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      hb_wvt_DrawBoxRecessed(_s->hGuiDC, iTop - 1, iLeft - 1, iBottom + 1, iRight + 1);
   }
   #endif
}

/* wvt_DrawBoxGroup( nTop, nLeft, nBottom, nRight, aPxlOff ) */
HB_FUNC( WVT_DRAWBOXGROUP )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);

   hb_wvt_DrawBoxGroup(_s->hdc, iTop, iLeft, iBottom, iRight);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      hb_wvt_DrawBoxGroup(_s->hGuiDC, iTop, iLeft, iBottom, iRight);
   }
   #endif
}

/* wvt_DrawBoxRaised( nTop, nLeft, nBottom, nRight, aPxlOff ) */
HB_FUNC( WVT_DRAWBOXGROUPRAISED )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);

   hb_wvt_DrawBoxGroupRaised(_s->hdc, iTop, iLeft, iBottom, iRight);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      hb_wvt_DrawBoxGroupRaised(_s->hGuiDC, iTop, iLeft, iBottom, iRight);
   }
   #endif
}

/* wvt_DrawImage( nTop, nLeft, nBottom, nRight, cImage/nPictureSlot, aPxlOff, lDoNotScale ) */
HB_FUNC( WVT_DRAWIMAGE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   POINT xy = {0, 0};
   int   iLeft, iTop, iRight, iBottom;

   xy    = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));
   iTop  = xy.y + hb_parvni(6, 1);
   iLeft = xy.x + hb_parvni(6, 2);

   xy      = hb_wvt_gtGetXYFromColRow(hb_parni(4) + 1, hb_parni(3) + 1);
   iBottom = xy.y - 1 + hb_parvni(6, 3);
   iRight  = xy.x - 1 + hb_parvni(6, 4);

   if( HB_ISNUM(5) )
   {
      hb_wvt_gtRenderPicture(iLeft, iTop, (iRight - iLeft) + 1, (iBottom - iTop) + 1, _s->pGUI->pPicture[hb_parni(5) - 1], hb_parl(7));
   }
   else
   {
      void * hImage;
      hb_wvt_DrawImage(_s->hdc, iLeft, iTop, (iRight - iLeft) + 1, (iBottom - iTop) + 1, HB_PARSTR(5, &hImage, nullptr), hb_parl(7));
      hb_strfree(hImage);
      #if defined(__SETGUI__)
      if( _s->bGui )
      {
         hb_wvt_DrawImage(_s->hGuiDC, iLeft, iTop, (iRight - iLeft) + 1, (iBottom - iTop) + 1, HB_PARSTR(5, &hImage, nullptr), hb_parl(7));
         hb_strfree(hImage);
      }
      #endif
   }

   hb_retl(true);
}

/*
 *    Wvt_DrawLabel( nRow, nCol, cLabel, nAlign, nEscapement, nTextColor, nBkColor,;
 *                     cFontFace, nFontHeight, nFontWidth, nFontWeight, nQuality,  ;
 *                        nCharSet, lItalics, lUnderline, lStrikeOut, aPxlOff )
 */
HB_FUNC( WVT_DRAWLABEL )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   POINT   xy = {0, 0};
   HFONT   hFont, hOldFont, hOldFontGui;
   void *  hText = nullptr;

   LOGFONT logfont{};
   logfont.lfEscapement     = hb_parni(5) * 10;
   logfont.lfOrientation    = 0;
   logfont.lfWeight         = hb_parni(11);
   logfont.lfItalic         = static_cast<BYTE>(hb_parl(14));
   logfont.lfUnderline      = static_cast<BYTE>(hb_parl(15));
   logfont.lfStrikeOut      = static_cast<BYTE>(hb_parl(16));
   logfont.lfCharSet        = static_cast<BYTE>(hb_parnidef(13, _s->CodePage));
   logfont.lfOutPrecision   = 0;
   logfont.lfClipPrecision  = 0;
   logfont.lfQuality        = static_cast<BYTE>(hb_parnidef(12, DEFAULT_QUALITY));
   logfont.lfPitchAndFamily = FF_DONTCARE;
   logfont.lfHeight         = hb_parnidef(9, _s->fontHeight);
   logfont.lfWidth = hb_parnidef(10, _s->fontWidth < 0 ? -_s->fontWidth : _s->fontWidth);

   HB_STRNCPY(logfont.lfFaceName, (HB_ISCHAR(8) ? HB_PARSTR(8, &hText, nullptr) : _s->fontFace ), HB_SIZEOFARRAY(logfont.lfFaceName) - 1);
   hb_strfree(hText);

   hFont = CreateFontIndirect(&logfont);
   if( hFont )
   {
      void *   hText;
      LPCTSTR  text  = HB_PARSTR(3, &hText, nullptr);
      COLORREF fgClr = hb_wvt_FgColorParam(6), bgClr = hb_wvt_BgColorParam(7);

      xy    = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));
      xy.x += hb_parvni(17, 2);
      xy.y += hb_parvni(17, 1);

      SetBkColor(_s->hdc, bgClr);
      SetTextColor(_s->hdc, fgClr);
      SetTextAlign(_s->hdc, hb_parnidef(4, TA_LEFT));
      hOldFont = static_cast<HFONT>(SelectObject(_s->hdc, hFont));

      ExtTextOut(_s->hdc, xy.x, xy.y, 0, nullptr, text, lstrlen(text), nullptr);

      SelectObject(_s->hdc, hOldFont);
      #if defined(__SETGUI__)
      if( _s->bGui )
      {
         SetBkColor(_s->hGuiDC, bgClr);
         SetTextColor(_s->hGuiDC, fgClr);
         SetTextAlign(_s->hGuiDC, hb_parnidef(4, TA_LEFT));
         hOldFontGui = static_cast<HFONT>(SelectObject(_s->hGuiDC, hFont));

         ExtTextOut(_s->hGuiDC, xy.x, xy.y, 0, nullptr, text, lstrlen(text), nullptr);
         SelectObject(_s->hGuiDC, hOldFontGui);
      }
      #endif
      hb_strfree(hText);
      DeleteObject(hFont);
      hb_retl(true);
   }

   hb_retl(false);
}

/*                   1      2       3        4       5       6        7         8
   wvt_DrawOutline( nTop, nLeft, nBottom, nRight, nThick, nShape, nRGBColor, aPxlOff ) */
HB_FUNC( WVT_DRAWOUTLINE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   HPEN  hPen, hOldPen, hOldPenGUI;
   POINT xy = {0, 0};
   int   iTop, iLeft, iBottom, iRight;

   xy      = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));
   iTop    = xy.y - 1 + hb_parvni(8, 1);
   iLeft   = xy.x - 1 + hb_parvni(8, 2);
   xy      = hb_wvt_gtGetXYFromColRow(hb_parni(4) + 1, hb_parni(3) + 1);
   iBottom = xy.y + hb_parvni(8, 3);
   iRight  = xy.x + hb_parvni(8, 4);

   hOldPenGUI = hOldPen = 0;

   if( HB_ISNUM(5) )
   {
      hPen = CreatePen(hb_parni(5), 0, static_cast<COLORREF>(hb_parnl(7)));
      if( hPen )
      {
         hOldPen = static_cast<HPEN>(SelectObject(_s->hdc, hPen));
      }
   }
   else
   {
      hPen = 0;
      SelectObject(_s->hdc, _s->pGUI->penBlack);
   }

   hb_wvt_DrawOutline(_s->hdc, iTop, iLeft, iBottom, iRight);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      if( hPen )
      {
         hOldPenGUI = static_cast<HPEN>(SelectObject(_s->hGuiDC, hPen));
      }
      else
      {
         hOldPenGUI = static_cast<HPEN>(SelectObject(_s->hGuiDC, _s->pGUI->penBlack));
         hb_wvt_DrawOutline(_s->hGuiDC, iTop, iLeft, iBottom, iRight);
      }
   }
   #endif

   if( hPen )
   {
      SelectObject(_s->hdc, hOldPen);
      if( hOldPenGUI )
      {
         SelectObject(_s->hGuiDC, hOldPenGUI);
      }
      DeleteObject(hPen);
   }
}

/*                1      2       3       4        5        6       7       8       9      10        11
   wvt_DrawLine( nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nStyle, nThick, nColor, aPxlOff ) */
HB_FUNC( WVT_DRAWLINE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(11, 1);
   int iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(11, 2);
   int iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(11, 3);
   int iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(11, 4);

   int      iOrient, iFormat, iAlign, iStyle, iThick;
   int      x, y, iOffset;
   COLORREF cr;
   HPEN     hPen, hOldPen, hOldPenGUI;

   /*   Resolve Parameters   */
   iOrient = hb_parni(5);
   iFormat = hb_parni(6);
   iAlign  = hb_parni(7);
   iStyle  = hb_parni(8);
   iThick  = hb_parni(9);
   cr      = static_cast<COLORREF>(hb_parnl(10));

   x = iLeft;
   y = iTop;

   switch( iAlign )
   {
      case 0:                    /* Center */
         if( iOrient == 0 )      /* Horizontal */
         {
            iOffset = ((iBottom - iTop) / 2);
            y       = iTop + iOffset;
         }
         else
         {
            iOffset = ((iRight - iLeft) / 2);
            x       = iLeft + iOffset;
         }
         break;

      case 1:                  /* Top    */
         break;

      case 2:                                /* bottom */
         if( iFormat == 0 || iFormat == 1 )  /* Raised/Recessd */
         {
            y = iBottom - 1;
         }
         else
         {
            y = iBottom;
         }
         break;

      case 3:                  /* Left  */
         break;

      case 4:                                /* Right */
         if( iFormat == 0 || iFormat == 1 )  /* Raised/Recessd */
         {
            x = iRight - 1;
         }
         else
         {
            x = iRight;
         }
         break;
   }

   hPen       = CreatePen(iStyle, iThick, cr);
   hOldPen    = static_cast<HPEN>(SelectObject(_s->hdc, hPen));
   hOldPenGUI = _s->bGui ? static_cast<HPEN>(SelectObject(_s->hGuiDC, hPen)) : 0;

   switch( iFormat )
   {
      case 0:                                         /* Raised */
         if( iOrient == 0 )                           /* Horizontal */
         {
            SelectObject(_s->hdc, _s->pGUI->penWhite);
            MoveToEx(_s->hdc, x, y, nullptr);
            LineTo(_s->hdc, iRight, y);
            SelectObject(_s->hdc, hPen);
            MoveToEx(_s->hdc, x, y + 1, nullptr);
            LineTo(_s->hdc, iRight, y + 1);
            #if defined(__SETGUI__)
            if( _s->bGui )
            {
               SelectObject(_s->hGuiDC, _s->pGUI->penWhite);
               MoveToEx(_s->hGuiDC, x, y, nullptr);
               LineTo(_s->hGuiDC, iRight, y);
               SelectObject(_s->hGuiDC, hPen);
               MoveToEx(_s->hGuiDC, x, y + 1, nullptr);
               LineTo(_s->hGuiDC, iRight, y + 1);
            }
            #endif
         }
         else                                       /* Vertical */
         {
            SelectObject(_s->hdc, _s->pGUI->penWhite);
            MoveToEx(_s->hdc, x, y, nullptr);
            LineTo(_s->hdc, x, iBottom);
            SelectObject(_s->hdc, hPen);
            MoveToEx(_s->hdc, x + 1, y, nullptr);
            LineTo(_s->hdc, x + 1, iBottom);
            #if defined(__SETGUI__)
            if( _s->bGui )
            {
               SelectObject(_s->hGuiDC, _s->pGUI->penWhite);
               MoveToEx(_s->hGuiDC, x, y, nullptr);
               LineTo(_s->hGuiDC, x, iBottom);
               SelectObject(_s->hGuiDC, hPen);
               MoveToEx(_s->hGuiDC, x + 1, y, nullptr);
               LineTo(_s->hGuiDC, x + 1, iBottom);
            }
            #endif
         }
         break;

      case 1:                                      /* Recessed */
         if( iOrient == 0 )                        /* Horizontal */
         {
            SelectObject(_s->hdc, hPen);
            MoveToEx(_s->hdc, x, y, nullptr);
            LineTo(_s->hdc, iRight, y);
            SelectObject(_s->hdc, _s->pGUI->penWhite);
            MoveToEx(_s->hdc, x, y + 1, nullptr);
            LineTo(_s->hdc, iRight, y + 1);
            #if defined(__SETGUI__)
            if( _s->bGui )
            {
               SelectObject(_s->hGuiDC, hPen);
               MoveToEx(_s->hGuiDC, x, y, nullptr);
               LineTo(_s->hGuiDC, iRight, y);
               SelectObject(_s->hGuiDC, _s->pGUI->penWhite);
               MoveToEx(_s->hGuiDC, x, y + 1, nullptr);
               LineTo(_s->hGuiDC, iRight, y + 1);
            }
            #endif
         }
         else                                      /* Vertical */
         {
            SelectObject(_s->hdc, hPen);
            MoveToEx(_s->hdc, x, y, nullptr);
            LineTo(_s->hdc, x, iBottom);
            SelectObject(_s->hdc, _s->pGUI->penWhite);
            MoveToEx(_s->hdc, x + 1, y, nullptr);
            LineTo(_s->hdc, x + 1, iBottom);
            #if defined(__SETGUI__)
            if( _s->bGui )
            {
               SelectObject(_s->hGuiDC, hPen);
               MoveToEx(_s->hGuiDC, x, y, nullptr);
               LineTo(_s->hGuiDC, x, iBottom);
               SelectObject(_s->hGuiDC, _s->pGUI->penWhite);
               MoveToEx(_s->hGuiDC, x + 1, y, nullptr);
               LineTo(_s->hGuiDC, x + 1, iBottom);
            }
            #endif
         }
         break;

      case 2:                                      /* Plain */
         if( iOrient == 0 )                        /* Horizontal */
         {
            SelectObject(_s->hdc, hPen);
            MoveToEx(_s->hdc, x, y, nullptr);
            LineTo(_s->hdc, iRight, y);
            #if defined(__SETGUI__)
            if( _s->bGui )
            {
               SelectObject(_s->hGuiDC, hPen);
               MoveToEx(_s->hGuiDC, x, y, nullptr);
               LineTo(_s->hGuiDC, iRight, y);
            }
            #endif
         }
         else                                      /* Vertical */
         {
            SelectObject(_s->hdc, hPen);
            MoveToEx(_s->hdc, x, y, nullptr);
            LineTo(_s->hdc, x, iBottom);
            #if defined(__SETGUI__)
            if( _s->bGui )
            {
               SelectObject(_s->hGuiDC, hPen);
               MoveToEx(_s->hGuiDC, x, y, nullptr);
               LineTo(_s->hGuiDC, x, iBottom);
            }
            #endif
         }
         break;
   }

   SelectObject(_s->hdc, hOldPen);
   if( hOldPenGUI )
   {
      SelectObject(_s->hGuiDC, hOldPenGUI);
   }
   DeleteObject(hPen);
   hb_retl(true);
}

/* Inside the area requested!
 * wvt_DrawEllipse( nTop, nLeft, nBottom, nRight, aPxlOff )
 */
HB_FUNC( WVT_DRAWELLIPSE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);

   SelectObject(_s->hdc, _s->currentBrush);
   SelectObject(_s->hdc, _s->currentPen);

   hb_retl(Ellipse(_s->hdc, iLeft, iTop, iRight, iBottom));
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      hb_retl(Ellipse(_s->hGuiDC, iLeft, iTop, iRight, iBottom));
   }
   #endif
}

/* wvt_DrawRectangle( nTop, nLeft, nBottom, nRight, aPxlOff ) */
HB_FUNC( WVT_DRAWRECTANGLE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);

   SelectObject(_s->hdc, _s->currentBrush);
   SelectObject(_s->hdc, _s->currentPen);

   hb_retl(Rectangle(_s->hdc, iLeft, iTop, iRight, iBottom));
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      hb_retl(Rectangle(_s->hGuiDC, iLeft, iTop, iRight, iBottom));
   }
   #endif
}

/* wvt_DrawRoundRect( nTop, nLeft, nBottom, nRight, aPxlOff, nRoundHeight, nRoundWidth ) */
HB_FUNC( WVT_DRAWROUNDRECT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);
   int iHt, iWd;

   iHt = hb_parni(6);
   iWd = hb_parni(7);

   SelectObject(_s->hdc, _s->currentBrush);
   SelectObject(_s->hdc, _s->currentPen);

   hb_retl(RoundRect(_s->hdc, iLeft, iTop, iRight, iBottom, iWd, iHt));
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      hb_retl(RoundRect(_s->hGuiDC, iLeft, iTop, iRight, iBottom, iWd, iHt));
   }
   #endif
}

/* wvt_DrawFocusRect( nTop, nLeft, nBottom, nRight, aPxlOff ) */
HB_FUNC( WVT_DRAWFOCUSRECT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int  iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int  iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int  iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int  iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);
   RECT rc      = {0, 0, 0, 0};

   rc.left   = iLeft;
   rc.top    = iTop;
   rc.right  = iRight;
   rc.bottom = iBottom;

   hb_retl(DrawFocusRect(_s->hdc, &rc));
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      hb_retl(DrawFocusRect(_s->hGuiDC, &rc));
   }
   #endif
}

/*
 *   Wvt_DrawColorRect( nTop, nLeft, nBottom, nRight, aPxlOff, nRGB )
 */
HB_FUNC( WVT_DRAWCOLORRECT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int    iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int    iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int    iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int    iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);
   RECT   rc      = {0, 0, 0, 0};
   HBRUSH hBrush;

   hBrush = CreateSolidBrush(static_cast<COLORREF>(hb_parnl(6)));

   if( hBrush )
   {
      rc.left   = iLeft;
      rc.top    = iTop;
      rc.right  = iRight;
      rc.bottom = iBottom;

      hb_retl(FillRect(_s->hdc, &rc, hBrush));
      #if defined(__SETGUI__)
      if( _s->bGui )
      {
         hb_retl(FillRect(_s->hGuiDC, &rc, hBrush));
      }
      #endif
      DeleteObject(hBrush);
   }
}

/*                        1     2       3      4       5
 *    Wvt_DrawGridHorz( nTop, nLeft, nRight, nRows, aPxlOff )
 *                                                  aPxlOff[2] and aPxlOff[4] used
 */
HB_FUNC( WVT_DRAWGRIDHORZ )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int iAtRow = hb_parni(1);
   int iRows  = hb_parni(4);
   int i, y;
   int iLeft, iRight;

   iLeft   = (hb_parni(2) * _s->PTEXTSIZE.x);
   iLeft  += hb_parvni(5, 2);
   iRight  = (((hb_parni(3) + 1) * _s->PTEXTSIZE.x) - 1);
   iRight += hb_parvni(5, 4);

   SelectObject(_s->hdc, _s->currentPen);

   for( i = 0; i < iRows; i++ )
   {
      y = ((iAtRow) * _s->PTEXTSIZE.y);

      MoveToEx(_s->hdc, iLeft, y, nullptr);
      LineTo(_s->hdc, iRight, y);

      iAtRow++;
   }
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      iAtRow = hb_parni(1);

      SelectObject(_s->hGuiDC, _s->currentPen);

      for( i = 0; i < iRows; i++ )
      {
         y = ((iAtRow) * _s->PTEXTSIZE.y);

         MoveToEx(_s->hGuiDC, iLeft, y, nullptr);
         LineTo(_s->hGuiDC, iRight, y);

         iAtRow++;
      }
   }
   #endif
   hb_retl(true);
}

/* wvt_DrawGridVert( nTop, nBottom, aCols, nCols, aPxlOff )
   aPxlOff[1] and aPxlOff[3] used */
HB_FUNC( WVT_DRAWGRIDVERT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int iTop, iBottom, x, i, iCharHeight, iCharWidth;
   int iTabs = hb_parni(4);

   if( !iTabs )
   {
      hb_retl(false);
   }

   iCharWidth  = _s->PTEXTSIZE.x;
   iCharHeight = _s->PTEXTSIZE.y;

   iTop     = (hb_parni(1) * iCharHeight);
   iTop    += hb_parvni(5, 1);
   iBottom  = ((hb_parni(2) + 1) * iCharHeight) - 1;
   iBottom += hb_parvni(5, 3);

   SelectObject(_s->hdc, _s->currentPen);

   for( i = 1; i <= iTabs; i++ )
   {
      x = (hb_parvni(3, i) * iCharWidth);

      MoveToEx(_s->hdc, x, iTop, nullptr);
      LineTo(_s->hdc, x, iBottom);
   }
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      SelectObject(_s->hGuiDC, _s->currentPen);
      for( i = 1; i <= iTabs; i++ )
      {
         x = (hb_parvni(3, i) * iCharWidth);

         MoveToEx(_s->hGuiDC, x, iTop, nullptr);
         LineTo(_s->hGuiDC, x, iBottom);
      }
   }
   #endif
   hb_retl(true);
}

/* wvt_DrawButton( nTop, nLeft, nBottom, nRight, cText, cnImage, ;
                  nFormat, nTextColor, nBkColor, nImageAt, aPxlOff ) */
HB_FUNC( WVT_DRAWBUTTON )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   SIZE     sz = {0, 0};
   POINT    xy = {0, 0};
   RECT     rc = {0, 0, 0, 0};
   int      iTop, iLeft, iBottom, iRight;
   int      iAlign;
   int      iTextHeight /*, iTextWidth */;
   LOGBRUSH lb = {0, 0, 0};
   HBRUSH   hBrush;

   HB_BOOL  bText     = HB_ISCHAR(5);
   HB_BOOL  bImage    = (HB_ISNUM(6) || HB_ISCHAR(6));
   int      iFormat   = hb_parni(7);
   COLORREF textColor = static_cast<COLORREF>(hb_parnldef(8, _s->COLORS[0]));
   COLORREF bkColor   = static_cast<COLORREF>(hb_parnldef(9, _s->COLORS[7]));

   /* int      iImageAt  = hb_parni(10); */

   xy      = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));
   iTop    = xy.y + hb_parvni(11, 1);
   iLeft   = xy.x + hb_parvni(11, 2);
   xy      = hb_wvt_gtGetXYFromColRow(hb_parni(4) + 1, hb_parni(3) + 1);
   iBottom = xy.y - 1 + hb_parvni(11, 3);
   iRight  = xy.x - 1 + hb_parvni(11, 4);

   lb.lbStyle = BS_SOLID;
   lb.lbColor = bkColor;
   lb.lbHatch = 0;
   hBrush = CreateBrushIndirect(&lb);
   rc.left   = iLeft;
   rc.top    = iTop;
   rc.right  = iRight + 1;
   rc.bottom = iBottom + 1;

   FillRect(_s->hdc, &rc, hBrush);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      FillRect(_s->hGuiDC, &rc, hBrush);
   }
   #endif
   DeleteObject(hBrush);

   switch( iFormat )
   {
      case 1:
         hb_wvt_DrawBoxRecessed(_s->hdc, iTop + 1, iLeft + 1, iBottom - 1, iRight - 1);
         if( _s->bGui )
         {
            hb_wvt_DrawBoxRecessed(_s->hGuiDC, iTop + 1, iLeft + 1, iBottom - 1, iRight - 1);
         }
         break;
      case 2:
         break;
      case 3:
         hb_wvt_DrawOutline(_s->hdc, iTop, iLeft, iBottom, iRight);
         if( _s->bGui )
         {
            hb_wvt_DrawOutline(_s->hGuiDC, iTop, iLeft, iBottom, iRight);
         }
         break;
      case 4:
         break;
      default:
         hb_wvt_DrawBoxRaised(_s->hdc, iTop + 1, iLeft + 1, iBottom - 1, iRight - 1);
         if( _s->bGui )
         {
            hb_wvt_DrawBoxRaised(_s->hGuiDC, iTop + 1, iLeft + 1, iBottom - 1, iRight - 1);
         }
         break;
   }

   if( bText )
   {
      void *  hText;
      LPCTSTR text = HB_PARSTR(5, &hText, nullptr);
      SelectObject(_s->hdc, GetStockObject(DEFAULT_GUI_FONT));
      GetTextExtentPoint32(_s->hdc, text, lstrlen(text), &sz);

      /* iTextWidth   = sz.cx; */
      iTextHeight = sz.cy;

      xy.x = iLeft + ((iRight - iLeft + 1) / 2);

      if( bImage )
      {
         xy.y = (iBottom - 2 - iTextHeight);
      }
      else
      {
         xy.y = iTop + ((iBottom - iTop + 1 - iTextHeight) / 2);
      }

      if( iFormat == 1 )
      {
         xy.x = xy.x + 2;
         xy.y = xy.y + 2;
      }

      iAlign = TA_CENTER + TA_TOP;

      SetTextAlign(_s->hdc, iAlign);
      SetBkMode(_s->hdc, TRANSPARENT);
      SetTextColor(_s->hdc, textColor);

      ExtTextOut(_s->hdc, xy.x, xy.y, 0, nullptr, text, lstrlen(text), nullptr);
      if( _s->bGui )
      {
         SelectObject(_s->hGuiDC, GetStockObject(DEFAULT_GUI_FONT));
         SetTextAlign(_s->hGuiDC, iAlign);
         SetBkMode(_s->hGuiDC, TRANSPARENT);
         SetTextColor(_s->hGuiDC, textColor);

         ExtTextOut(_s->hGuiDC, xy.x, xy.y, 0, nullptr, text, lstrlen(text), nullptr);
      }
      hb_strfree(hText);
   }
   else
   {
      iTextHeight = -1;
   }

   if( bImage )
   {
      int iImageWidth  = (iRight - iLeft + 1 - 8);
      int iImageHeight = (iBottom - iTop + 1 - 8 - iTextHeight);

      if( HB_ISNUM(6) )
      {
         IPicture * iPicture = _s->pGUI->pPicture[hb_parni(6) - 1];
         hb_wvt_gtRenderPicture(iLeft + 4, iTop + 4, iImageWidth, iImageHeight, iPicture, FALSE);
      }
      else
      {
         void * hImage;
         hb_wvt_DrawImage(_s->hdc, iLeft + 4, iTop + 4, iImageWidth, iImageHeight, HB_PARSTR(6, &hImage, nullptr), FALSE);
         hb_strfree(hImage);
         if( _s->bGui )
         {
            hb_wvt_DrawImage(_s->hGuiDC, iLeft + 4, iTop + 4, iImageWidth, iImageHeight, HB_PARSTR(6, &hImage, nullptr), FALSE);
            hb_strfree(hImage);
         }
      }
   }

   hb_retl(true);
}

/* wvt_DrawStatusBar( nNumPanels, aTLBRofPanels ) */
HB_FUNC( WVT_DRAWSTATUSBAR )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int   iPanels = hb_parni(1);
   int   i, iNext;
   int   iTop, iLeft, iBottom, iRight;
   POINT xy = {0, 0};

   iNext = 0;

   for( i = 0; i < iPanels; i++ )
   {
      xy    = hb_wvt_gtGetXYFromColRow(hb_parvni(2, iNext + 2), hb_parvni(2, iNext + 1));
      iTop  = xy.y;
      iLeft = xy.x + 1;

      xy      = hb_wvt_gtGetXYFromColRow(hb_parvni(2, iNext + 4), hb_parvni(2, iNext + 3) + 1);
      iBottom = xy.y - 1;
      iRight  = xy.x - 2;

      SelectObject(_s->hdc, _s->pGUI->penWhite);

      MoveToEx(_s->hdc, iRight, iTop, nullptr);            /* Right  */
      LineTo(_s->hdc, iRight, iBottom);

      MoveToEx(_s->hdc, iLeft, iBottom, nullptr);          /* Bottom */
      LineTo(_s->hdc, iRight, iBottom);

      SelectObject(_s->hdc, _s->pGUI->penDarkGray);

      MoveToEx(_s->hdc, iLeft, iTop, nullptr);             /* Left   */
      LineTo(_s->hdc, iLeft, iBottom);

      MoveToEx(_s->hdc, iLeft, iTop, nullptr);             /* Top    */
      LineTo(_s->hdc, iRight, iTop);

      iNext = iNext + 4;
   }

   xy      = hb_wvt_gtGetXYFromColRow(hb_parvni(2, 4 * iPanels), hb_parvni(2, (4 * iPanels) - 1) + 1);
   iTop    = xy.y - 2;
   iLeft   = xy.x - 2;
   iBottom = iTop;
   iRight  = iLeft;

   SelectObject(_s->hdc, _s->pGUI->penBlack);

   MoveToEx(_s->hdc, iLeft - 4, iBottom, nullptr);
   LineTo(_s->hdc, iRight, iTop - 4);
   MoveToEx(_s->hdc, iLeft - 7, iBottom, nullptr);
   LineTo(_s->hdc, iRight, iTop - 7);
   MoveToEx(_s->hdc, iLeft - 10, iBottom, nullptr);
   LineTo(_s->hdc, iRight, iTop - 10);

   SelectObject(_s->hdc, _s->pGUI->penWhite);

   MoveToEx(_s->hdc, iLeft - 5, iBottom, nullptr);
   LineTo(_s->hdc, iRight, iTop - 5);
   MoveToEx(_s->hdc, iLeft - 8, iBottom, nullptr);
   LineTo(_s->hdc, iRight, iTop - 8);
   MoveToEx(_s->hdc, iLeft - 11, iBottom, nullptr);
   LineTo(_s->hdc, iRight, iTop - 11);
}

/* wvt_DrawPicture( nTop, nLeft, nBottom, nRight, nSlot, aPxlOff, lDoNotScale ) --> lOk
   nSlot <= 20  aAdj == {0,0,-2,-2} To Adjust the pixels for {Top,Left,Bottom,Right} */
HB_FUNC( WVT_DRAWPICTURE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   POINT xy = {0, 0};
   int   iTop, iLeft, iBottom, iRight;
   int   iSlot = hb_parni(5) - 1;

   if( iSlot < WVT_PICTURES_MAX )
   {
      if( _s->pGUI->pPicture[iSlot] )
      {
         xy    = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));
         iTop  = xy.y + hb_parvni(6, 1);
         iLeft = xy.x + hb_parvni(6, 2);

         xy      = hb_wvt_gtGetXYFromColRow(hb_parni(4) + 1, hb_parni(3) + 1);
         iBottom = xy.y - 1 + hb_parvni(6, 3);
         iRight  = xy.x - 1 + hb_parvni(6, 4);

         hb_retl(hb_wvt_gtRenderPicture(iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1, _s->pGUI->pPicture[iSlot], hb_parl(7)));
      }
   }
}

/*
 *  Wvt_DrawPictureByHandle( nTop, nLeft, nBottom, nRight, hPicture, aPxlOff, lDoNotScale ) -> lOk
 */
HB_FUNC( WVT_DRAWPICTUREEX )
{
   POINT xy = {0, 0};
   int   iTop, iLeft, iBottom, iRight;

   if( HB_ISNUM(5) )
   {
      xy    = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));
      iTop  = xy.y + hb_parvni(6, 1);
      iLeft = xy.x + hb_parvni(6, 2);

      xy      = hb_wvt_gtGetXYFromColRow(hb_parni(4) + 1, hb_parni(3) + 1);
      iBottom = xy.y - 1 + hb_parvni(6, 3);
      iRight  = xy.x - 1 + hb_parvni(6, 4);

      hb_retl(hb_wvt_gtRenderPicture(iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1, reinterpret_cast<IPicture*>(static_cast<HB_PTRUINT>(hb_parnint(5))), hb_parl(7)));
   }
}

/*
 *    Wvt_DrawLabelEx( nRow, nCol, cLabel, nAlign, nTextColor, nBkColor, nSlotFont, aPxlOff )
 */
HB_FUNC( WVT_DRAWLABELEX )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   POINT xy    = {0, 0};
   int   iSlot = hb_parni(7) - 1;

   if( _s->pGUI->hUserFonts[iSlot] )
   {
      void *   hText;
      LPCTSTR  text  = HB_PARSTR(3, &hText, nullptr);
      COLORREF fgClr = hb_wvt_FgColorParam(5), bgClr = hb_wvt_BgColorParam(6);

      xy    = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));
      xy.x += hb_parvni(8, 2);
      xy.y += hb_parvni(8, 1);

      SetBkColor(_s->hdc, bgClr);
      SetTextColor(_s->hdc, fgClr);
      SetTextAlign(_s->hdc, hb_parnidef(4, TA_LEFT));
      SelectObject(_s->hdc, _s->pGUI->hUserFonts[iSlot]);

      ExtTextOut(_s->hdc, xy.x, xy.y, 0, nullptr, text, lstrlen(text), nullptr);
      #if defined(__SETGUI__)
      if( _s->bGui )
      {
         SetBkColor(_s->hGuiDC, bgClr);
         SetTextColor(_s->hGuiDC, fgClr);
         SetTextAlign(_s->hGuiDC, hb_parnidef(4, TA_LEFT));
         SelectObject(_s->hGuiDC, _s->pGUI->hUserFonts[iSlot]);

         ExtTextOut(_s->hGuiDC, xy.x, xy.y, 0, nullptr, text, lstrlen(text), nullptr);
      }
      #endif
      hb_strfree(hText);
      hb_retl(true);
   }

   hb_retl(false);
}

/*                    1      2       3       4        5        6       7       8          9
 *   Wvt_DrawLineEx( nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nSlotPen, aPxlOff )
 */
HB_FUNC( WVT_DRAWLINEEX )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   POINT xy = {0, 0};
   int   iTop, iLeft, iBottom, iRight, iOffset;
   int   iOrient, iFormat, iAlign;
   int   x, y;
   HPEN  hPen;
   int   iSlot = hb_parni(8) - 1;

   xy    = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));
   iTop  = xy.y + hb_parvni(9, 1);
   iLeft = xy.x + hb_parvni(9, 2);

   xy      = hb_wvt_gtGetXYFromColRow(hb_parni(4) + 1, hb_parni(3) + 1);
   iBottom = xy.y - 1 + hb_parvni(9, 4);
   iRight  = xy.x - 1 + hb_parvni(9, 4);

   /* Resolve Parameters */
   iOrient = hb_parni(5);
   iFormat = hb_parni(6);
   iAlign  = hb_parni(7);

   x = iLeft;
   y = iTop;

   switch( iAlign )
   {
      case 0:                    /* Center */
         if( iOrient == 0 )      /* Horizontal */
         {
            iOffset = ((iBottom - iTop) / 2);
            y       = iTop + iOffset;
         }
         else
         {
            iOffset = ((iRight - iLeft) / 2);
            x       = iLeft + iOffset;
         }
         break;

      case 1:                  /* Top */
         break;

      case 2:                                /* bottom */
         if( iFormat == 0 || iFormat == 1 )  /* Raised/Recessd */
         {
            y = iBottom - 1;
         }
         else
         {
            y = iBottom;
         }
         break;

      case 3:                  /* Left */
         break;

      case 4:                                /* Right */
         if( iFormat == 0 || iFormat == 1 )  /* Raised/Recessd */
         {
            x = iRight - 1;
         }
         else
         {
            x = iRight;
         }
         break;
   }

   hPen = _s->pGUI->hUserPens[iSlot];

   switch( iFormat )
   {
      case 0:                                         /* Raised */
         if( iOrient == 0 )                           /* Horizontal */
         {
            SelectObject(_s->hdc, _s->pGUI->penWhite);
            MoveToEx(_s->hdc, x, y, nullptr);
            LineTo(_s->hdc, iRight, y);
            SelectObject(_s->hdc, hPen);
            MoveToEx(_s->hdc, x, y + 1, nullptr);
            LineTo(_s->hdc, iRight, y + 1);
            #if defined(__SETGUI__)
            if( _s->bGui )
            {
               SelectObject(_s->hGuiDC, _s->pGUI->penWhite);
               MoveToEx(_s->hGuiDC, x, y, nullptr);
               LineTo(_s->hGuiDC, iRight, y);
               SelectObject(_s->hGuiDC, hPen);
               MoveToEx(_s->hGuiDC, x, y + 1, nullptr);
               LineTo(_s->hGuiDC, iRight, y + 1);
            }
            #endif
         }
         else                                       /* Vertical */
         {
            SelectObject(_s->hdc, _s->pGUI->penWhite);
            MoveToEx(_s->hdc, x, y, nullptr);
            LineTo(_s->hdc, x, iBottom);
            SelectObject(_s->hdc, hPen);
            MoveToEx(_s->hdc, x + 1, y, nullptr);
            LineTo(_s->hdc, x + 1, iBottom);
            #if defined(__SETGUI__)
            if( _s->bGui )
            {
               SelectObject(_s->hGuiDC, _s->pGUI->penWhite);
               MoveToEx(_s->hGuiDC, x, y, nullptr);
               LineTo(_s->hGuiDC, x, iBottom);
               SelectObject(_s->hGuiDC, hPen);
               MoveToEx(_s->hGuiDC, x + 1, y, nullptr);
               LineTo(_s->hGuiDC, x + 1, iBottom);
            }
            #endif
         }
         break;

      case 1:                                      /* Recessed */
         if( iOrient == 0 )                        /* Horizontal */
         {
            SelectObject(_s->hdc, hPen);
            MoveToEx(_s->hdc, x, y, nullptr);
            LineTo(_s->hdc, iRight, y);
            SelectObject(_s->hdc, _s->pGUI->penWhite);
            MoveToEx(_s->hdc, x, y + 1, nullptr);
            LineTo(_s->hdc, iRight, y + 1);
            #if defined(__SETGUI__)
            if( _s->bGui )
            {
               SelectObject(_s->hGuiDC, hPen);
               MoveToEx(_s->hGuiDC, x, y, nullptr);
               LineTo(_s->hGuiDC, iRight, y);
               SelectObject(_s->hGuiDC, _s->pGUI->penWhite);
               MoveToEx(_s->hGuiDC, x, y + 1, nullptr);
               LineTo(_s->hGuiDC, iRight, y + 1);
            }
            #endif
         }
         else                                      /* Vertical */
         {
            SelectObject(_s->hdc, hPen);
            MoveToEx(_s->hdc, x, y, nullptr);
            LineTo(_s->hdc, x, iBottom);
            SelectObject(_s->hdc, _s->pGUI->penWhite);
            MoveToEx(_s->hdc, x + 1, y, nullptr);
            LineTo(_s->hdc, x + 1, iBottom);
            #if defined(__SETGUI__)
            if( _s->bGui )
            {
               SelectObject(_s->hGuiDC, hPen);
               MoveToEx(_s->hGuiDC, x, y, nullptr);
               LineTo(_s->hGuiDC, x, iBottom);
               SelectObject(_s->hGuiDC, _s->pGUI->penWhite);
               MoveToEx(_s->hGuiDC, x + 1, y, nullptr);
               LineTo(_s->hGuiDC, x + 1, iBottom);
            }
            #endif
         }
         break;

      case 2:                                      /* Plain */
         if( iOrient == 0 )                        /* Horizontal */
         {
            SelectObject(_s->hdc, hPen);
            MoveToEx(_s->hdc, x, y, nullptr);
            LineTo(_s->hdc, iRight, y);
            #if defined(__SETGUI__)
            if( _s->bGui )
            {
               SelectObject(_s->hGuiDC, hPen);
               MoveToEx(_s->hGuiDC, x, y, nullptr);
               LineTo(_s->hGuiDC, iRight, y);
            }
            #endif
         }
         else                                      /* Vertical */
         {
            SelectObject(_s->hdc, hPen);
            MoveToEx(_s->hdc, x, y, nullptr);
            LineTo(_s->hdc, x, iBottom);
            #if defined(__SETGUI__)
            if( _s->bGui )
            {
               SelectObject(_s->hGuiDC, hPen);
               MoveToEx(_s->hGuiDC, x, y, nullptr);
               LineTo(_s->hGuiDC, x, iBottom);
            }
            #endif
         }
         break;
   }

   hb_retl(true);
}

/*
 *    Wvt_DrawOutlineEx( nTop, nLeft, nBottom, nRight, nSlotPen, aPxlOff )
 */
HB_FUNC( WVT_DRAWOUTLINEEX )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   POINT xy = {0, 0};
   int   iTop, iLeft, iBottom, iRight;
   int   iSlot = hb_parni(5) - 1;

   xy      = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));
   iTop    = xy.y - 1 + hb_parvni(6, 1);
   iLeft   = xy.x - 1 + hb_parvni(6, 2);
   xy      = hb_wvt_gtGetXYFromColRow(hb_parni(4) + 1, hb_parni(3) + 1);
   iBottom = xy.y + hb_parvni(6, 3);
   iRight  = xy.x + hb_parvni(6, 4);

   if( _s->pGUI->hUserPens[iSlot] )
   {
      SelectObject(_s->hdc, _s->pGUI->hUserPens[iSlot]);
   }
   else
   {
      SelectObject(_s->hdc, _s->pGUI->penBlack);
   }

   hb_wvt_DrawOutline(_s->hdc, iTop, iLeft, iBottom, iRight);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      hb_wvt_DrawOutline(_s->hGuiDC, iTop, iLeft, iBottom, iRight);
   }
   #endif
}

/*                        1     2       3        4       5         6           7         8            9      10       11
 *    Wvt_DrawLabelObj( nTop, nLeft, nBottom, nRight, cLabel, nAlignHorz, nAlignVert, nTextColor, nBkColor, hFont, aPxlOff )
 */
HB_FUNC( WVT_DRAWLABELOBJ )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   POINT xy   = {0, 0};
   RECT  rect = {0, 0, 0, 0};
   int   iTop, iLeft, iBottom, iRight, x, y;
   int   iAlignHorz, iAlignVert, iAlignH, iAlignV;
   UINT  uiOptions;
   SIZE  sz = {0, 0};
   void *   hText;
   LPCTSTR  text  = HB_PARSTR(5, &hText, nullptr);
   COLORREF fgClr = hb_wvt_FgColorParam(8), bgClr = hb_wvt_BgColorParam(9);

   xy      = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));
   iTop    = xy.y + hb_parvni(11, 1);
   iLeft   = xy.x + hb_parvni(11, 2);
   xy      = hb_wvt_gtGetXYFromColRow(hb_parni(4) + 1, hb_parni(3) + 1);
   iBottom = xy.y - 1 + hb_parvni(11, 3);
   iRight  = xy.x - 1 + hb_parvni(11, 4);

   iAlignHorz = hb_parni(6);   /* default is 0 */
   iAlignVert = hb_parni(7);   /* default is 0 */

   SetTextColor(_s->hdc, fgClr);
   SetBkColor(_s->hdc, bgClr);
   SelectObject(_s->hdc, reinterpret_cast<HFONT>(static_cast<HB_PTRUINT>(hb_parnint(10))));

   GetTextExtentPoint32(_s->hdc, text, lstrlen(text), &sz);

   x = iLeft;
   y = iTop;

   switch( iAlignHorz )
   {
      case 0:
         iAlignH = TA_LEFT;
         break;

      case 1:
         iAlignH = TA_RIGHT;
         x       = iRight;
         break;

      case 2:
         iAlignH = TA_CENTER;
         x       = iLeft + ((iRight - iLeft + 1) / 2);
         break;

      default:
         iAlignH = 0;
   }

   iAlignV = TA_TOP;

   switch( iAlignVert )
   {
      case 1:
         y = iBottom - sz.cy;
         break;

      case 2:
         y = iTop + ((iBottom - iTop + 1 - sz.cy) / 2);
         break;
   }

   SetTextAlign(_s->hdc, iAlignH | iAlignV);

   rect.top    = iTop;
   rect.left   = iLeft;
   rect.bottom = iBottom;
   rect.right  = iRight;

   uiOptions = ETO_CLIPPED | ETO_OPAQUE;

   ExtTextOut(_s->hdc, x, y, uiOptions, &rect, text, lstrlen(text), nullptr);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      SetTextColor(_s->hGuiDC, fgClr);
      SetBkColor(_s->hGuiDC, bgClr);
      SelectObject(_s->hGuiDC, reinterpret_cast<HFONT>(static_cast<HB_PTRUINT>(hb_parnint(10))));
      SetTextAlign(_s->hGuiDC, iAlignH | iAlignV);

      ExtTextOut(_s->hGuiDC, x, y, uiOptions, &rect, text, lstrlen(text), nullptr);
   }
   #endif
   hb_strfree(hText);
   hb_retl(true);
}

/*
 *    nState 0 Flat, 1 Raised, 2 Recessed
 *    Wvt_DrawToolButtonState( nTop, nLeft, nBottom, nRight, aPxlOff, nState )
 */
HB_FUNC( WVT_DRAWTOOLBUTTONSTATE )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);

   switch( hb_parni(6) )
   {
      case 0:     /* Flat */
         hb_wvt_DrawToolButtonFlat(_s->hdc, iTop, iLeft, iBottom, iRight);
         #if defined(__SETGUI__)
         if( _s->bGui )
         {
            hb_wvt_DrawToolButtonFlat(_s->hGuiDC, iTop, iLeft, iBottom, iRight);
         }
         #endif
         break;

      case 1:     /* Raised */
         hb_wvt_DrawToolButtonUp(_s->hdc, iTop, iLeft, iBottom, iRight);
         #if defined(__SETGUI__)
         if( _s->bGui )
         {
            hb_wvt_DrawToolButtonUp(_s->hGuiDC, iTop, iLeft, iBottom, iRight);
         }
         #endif
         break;

      case 2:     /* Recessed */
         hb_wvt_DrawToolButtonDown(_s->hdc, iTop, iLeft, iBottom, iRight);
         #if defined(__SETGUI__)
         if( _s->bGui )
         {
            hb_wvt_DrawToolButtonDown(_s->hGuiDC, iTop, iLeft, iBottom, iRight);
         }
         #endif
         break;
   }
   hb_retl(true);
}

/*
 *   Wvt_DrawScrollButton( nTop, nLeft, nBottom, nRight, aPxlOff, nTLBR, lDepressed )
 */
HB_FUNC( WVT_DRAWSCROLLBUTTON )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);

   POINT * Point;
   POINT   xy = {0, 0};
   int     iHeight, iOff;
   HB_BOOL bDepressed = hb_parl(7);

   Point = static_cast<POINT*>(hb_xgrab(3 * sizeof(POINT)));
   iOff  = 6;

   iHeight = iBottom - iTop + 1;

   if( bDepressed )
   {
      hb_wvt_DrawBoxRecessed(_s->hdc, iTop + 1, iLeft + 1, iBottom - 2, iRight - 2);
      #if defined(__SETGUI__)
      if( _s->bGui )
      {
         hb_wvt_DrawBoxRecessed(_s->hGuiDC, iTop + 1, iLeft + 1, iBottom - 2, iRight - 2);
      }
      #endif
   }
   else
   {
      hb_wvt_DrawBoxRaised(_s->hdc, iTop + 1, iLeft + 1, iBottom - 2, iRight - 2);
      #if defined(__SETGUI__)
      if( _s->bGui )
      {
         hb_wvt_DrawBoxRaised(_s->hGuiDC, iTop + 1, iLeft + 1, iBottom - 2, iRight - 2);
      }
      #endif
   }

   switch( hb_parni(6) )
   {
      case 1:   /* Top */
         xy.y       = iTop + iOff - 1;
         xy.x       = iLeft + ((iRight - iLeft + 1) / 2);
         Point[0] = xy;
         xy.y       = iBottom - iOff - 1;
         xy.x       = iLeft + iOff - 1;
         Point[1] = xy;
         xy.x       = iRight - iOff + 1;
         Point[2] = xy;
         break;

      case 2:  /* Left */
         xy.y       = iTop + ((iBottom - iTop + 1) / 2);
         xy.x       = iLeft + iOff;
         Point[0] = xy;
         xy.x       = iRight - iOff - 1;
         xy.y       = iTop + iOff - 1;
         Point[1] = xy;
         xy.y       = iBottom - iOff + 1;
         Point[2] = xy;
         break;

      case 3:  /* Bottom */
         xy.x       = iLeft + ((iRight - iLeft + 1) / 2);
         xy.y       = iBottom - iOff;
         Point[0] = xy;
         xy.x       = iLeft + iOff - 1;
         xy.y       = iBottom - iHeight + iOff + 1;
         Point[1] = xy;
         xy.x       = iRight - iOff + 1;
         Point[2] = xy;
         break;

      case 4:  /* Right */
         xy.x       = iRight - iOff - 1;
         xy.y       = iTop + ((iBottom - iTop + 1) / 2);
         Point[0] = xy;
         xy.x       = iLeft + iOff + 1;
         xy.y       = iTop + iOff - 1;
         Point[1] = xy;
         xy.y       = iBottom - iOff + 1;
         Point[2] = xy;
         break;
   }

   SelectObject(_s->hdc, _s->pGUI->solidBrush);
   Polygon(_s->hdc, Point, 3);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      SelectObject(_s->hGuiDC, _s->pGUI->solidBrush);
      Polygon(_s->hGuiDC, Point, 3);
   }
   #endif
   hb_xfree(Point);
}

/* wvt_DrawScrollThumbVert( nTop, nLeft, nBottom, nRight, aPxlOff, nThumbPos ) */
HB_FUNC( WVT_DRAWSCROLLTHUMBVERT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);
   int iTabTop, iTabLft, iTabBtm, iTabRgt;

   /* Background */
   SetBkMode(_s->hdc, OPAQUE);
   SetBkColor(_s->hdc, RGB(230, 230, 230));
   SelectObject(_s->hdc, _s->pGUI->diagonalBrush);
   SelectObject(_s->hdc, _s->pGUI->penNull);
   Rectangle(_s->hdc, iLeft, iTop, iRight + 1, iBottom + 1);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      SetBkMode(_s->hGuiDC, OPAQUE);
      SetBkColor(_s->hGuiDC, RGB(230, 230, 230));
      SelectObject(_s->hGuiDC, _s->pGUI->diagonalBrush);
      SelectObject(_s->hGuiDC, _s->pGUI->penNull);
      Rectangle(_s->hGuiDC, iLeft, iTop, iRight + 1, iBottom + 1);
   }
   #endif
   /* Thumb */
   iTabTop = _s->PTEXTSIZE.y * hb_parni(6);
   iTabLft = iLeft;
   iTabBtm = iTabTop + _s->PTEXTSIZE.y - 1;
   iTabRgt = iRight;

   SelectObject(_s->hdc, _s->pGUI->whiteBrush);
   SelectObject(_s->hdc, _s->pGUI->penGray);
   Rectangle(_s->hdc, iTabLft, iTabTop, iTabRgt + 1, iTabBtm);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      SelectObject(_s->hGuiDC, _s->pGUI->whiteBrush);
      SelectObject(_s->hGuiDC, _s->pGUI->penGray);
      Rectangle(_s->hGuiDC, iTabLft, iTabTop, iTabRgt + 1, iTabBtm);
   }
   #endif
   hb_wvt_DrawBoxRaised(_s->hdc, iTabTop + 1, iTabLft + 1, iTabBtm - 2, iTabRgt - 2);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      hb_wvt_DrawBoxRaised(_s->hGuiDC, iTabTop + 1, iTabLft + 1, iTabBtm - 2, iTabRgt - 2);
   }
   #endif
}

/* wvt_DrawScrollThumbHorz( nTop, nLeft, nBottom, nRight, aPxlOff, nThumbPos ) */
HB_FUNC( WVT_DRAWSCROLLTHUMBHORZ )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);
   int iThumbLeft, iThumbRight;

   iThumbLeft  = _s->PTEXTSIZE.x * hb_parni(6);
   iThumbRight = iThumbLeft + (_s->PTEXTSIZE.x * 2) - 1;

   /* Background */
   SetBkMode(_s->hdc, OPAQUE);
   SetBkColor(_s->hdc, RGB(230, 230, 230));
   SelectObject(_s->hdc, _s->pGUI->diagonalBrush);
   SelectObject(_s->hdc, _s->pGUI->penNull);
   Rectangle(_s->hdc, iLeft, iTop, iRight + 1, iBottom + 1);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      SetBkMode(_s->hGuiDC, OPAQUE);
      SetBkColor(_s->hGuiDC, RGB(230, 230, 230));
      SelectObject(_s->hGuiDC, _s->pGUI->diagonalBrush);
      SelectObject(_s->hGuiDC, _s->pGUI->penNull);
      Rectangle(_s->hGuiDC, iLeft, iTop, iRight + 1, iBottom + 1);
   }
   #endif
   /* Thumb */
   SelectObject(_s->hdc, _s->pGUI->whiteBrush);
   SelectObject(_s->hdc, _s->pGUI->penGray);
   Rectangle(_s->hdc, iThumbLeft, iTop, iThumbRight, iBottom);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      SelectObject(_s->hGuiDC, _s->pGUI->whiteBrush);
      SelectObject(_s->hGuiDC, _s->pGUI->penGray);
      Rectangle(_s->hGuiDC, iThumbLeft, iTop, iThumbRight, iBottom);
   }
   #endif
   hb_wvt_DrawBoxRaised(_s->hdc, iTop + 1, iThumbLeft + 1, iBottom - 2, iThumbRight - 2);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      hb_wvt_DrawBoxRaised(_s->hGuiDC, iTop + 1, iThumbLeft + 1, iBottom - 2, iThumbRight - 2);
   }
   #endif
}

/* #if WINVER > 0x500 */
/*
 *    Wvt_DrawShadedRect( nTop, nLeft, nBottom, nRight, aPxlOff, nHorVert, aRGBb, aRGBe  )
 */
HB_FUNC( WVT_DRAWSHADEDRECT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   HB_BOOL bGF = HB_FALSE;

   TRIVERTEX     vert[2];
   GRADIENT_RECT gRect = {0, 0};

   int iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);

   int iMode = hb_parnidef(6, GRADIENT_FILL_RECT_H);

   vert[0].x     = iLeft;
   vert[0].y     = iTop;
   vert[0].Red   = static_cast<COLOR16>(hb_parvni(7, 1));
   vert[0].Green = static_cast<COLOR16>(hb_parvni(7, 2));
   vert[0].Blue  = static_cast<COLOR16>(hb_parvni(7, 3));
   vert[0].Alpha = static_cast<COLOR16>(hb_parvni(7, 4));

   vert[1].x     = iRight;
   vert[1].y     = iBottom;
   vert[1].Red   = static_cast<COLOR16>(hb_parvni(8, 1));
   vert[1].Green = static_cast<COLOR16>(hb_parvni(8, 2));
   vert[1].Blue  = static_cast<COLOR16>(hb_parvni(8, 3));
   vert[1].Alpha = static_cast<COLOR16>(hb_parvni(8, 4));

   gRect.UpperLeft  = 0;
   gRect.LowerRight = 1;

   bGF = static_cast<HB_BOOL>(GradientFill(_s->hdc, vert, 2, &gRect, 1, iMode));
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      bGF = static_cast<HB_BOOL>(GradientFill(_s->hGuiDC, vert, 2, &gRect, 1, iMode));
   }
   #endif

   hb_retl(bGF);
}
/*#endif*/

/* wvt_DrawTextBox( nTop, nLeft, nBottom, nRight, aPxlOff, cText, ;
                    nAlignHorz, nAlignVert, nTextColor, nBackColor, ;
                    nBackMode, hFont ) */
HB_FUNC( WVT_DRAWTEXTBOX )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);

   int iAlignHorz = hb_parni(7);         /* default to 0 */
   int iAlignH    = 0;

   RECT     rc = {0, 0, 0, 0};
   void *   hText;
   LPCTSTR  text  = HB_PARSTR(6, &hText, nullptr);
   COLORREF fgClr = hb_wvt_FgColorParam(9), bgClr = hb_wvt_BgColorParam(10);

   switch( iAlignHorz )
   {
      case 0:
         iAlignH = DT_LEFT;
         break;

      case 1:
         iAlignH = DT_RIGHT;
         break;

      case 2:
         iAlignH = DT_CENTER;
         break;
   }

   rc.top    = iTop;
   rc.left   = iLeft;
   rc.bottom = iBottom;
   rc.right  = iRight;

   SetTextAlign(_s->hdc, TA_TOP | TA_LEFT | TA_NOUPDATECP);
   SetTextColor(_s->hdc, fgClr);
   SetBkColor(_s->hdc, bgClr);
   SetBkMode(_s->hdc, hb_parnidef(11, OPAQUE));
   SelectObject(_s->hdc, reinterpret_cast<HFONT>(static_cast<HB_PTRUINT>(hb_parnint(12))));

   DrawText(_s->hdc, text, lstrlen(text), &rc, iAlignH | DT_WORDBREAK | DT_TOP);
   #if defined(__SETGUI__)
   if( _s->bGui )
   {
      SetTextAlign(_s->hGuiDC, TA_TOP | TA_LEFT | TA_NOUPDATECP);
      SetTextColor(_s->hGuiDC, fgClr);
      SetBkColor(_s->hGuiDC, bgClr);
      SetBkMode(_s->hGuiDC, hb_parnidef(11, OPAQUE));
      SelectObject(_s->hGuiDC, reinterpret_cast<HFONT>(static_cast<HB_PTRUINT>(hb_parnint(12))));

      DrawText(_s->hGuiDC, text, lstrlen(text), &rc, iAlignH | DT_WORDBREAK | DT_TOP);
   }
   #endif
   hb_strfree(hText);
}

/* wvt_DrawProgressBar( nTop, nLeft, nBottom, nRight, aPxlOff, nPercent, ;
                        nBackColor, nBarColor, cImage, lVertical, nDirection ) */
HB_FUNC( WVT_DRAWPROGRESSBAR )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int      iTop    = (_s->PTEXTSIZE.y * hb_parni(1)) + hb_parvni(5, 1);
   int      iLeft   = (_s->PTEXTSIZE.x * hb_parni(2)) + hb_parvni(5, 2);
   int      iBottom = (_s->PTEXTSIZE.y * (hb_parni(3) + 1)) - 1 + hb_parvni(5, 3);
   int      iRight  = (_s->PTEXTSIZE.x * (hb_parni(4) + 1)) - 1 + hb_parvni(5, 4);
   int      iPercent, iBarUpto, iDirection;
   HB_BOOL  bVertical, bImage;
   COLORREF crBarColor;
   HBRUSH   hBrush;
   LOGBRUSH lb = {0, 0, 0};
   RECT     rc = {0, 0, 0, 0};

   iPercent   = hb_parni(6);
   bImage     = HB_ISCHAR(9);
   bVertical  = hb_parl(10);
   iDirection = hb_parni(11);

   if( bVertical )
   {
      if( iDirection == 0 )
      {
         iBarUpto  = iTop + ((iBottom - iTop) * iPercent / 100);
         rc.top    = iTop;
         rc.left   = iLeft;
         rc.bottom = iBarUpto;
         rc.right  = iRight;
      }
      else
      {
         iBarUpto  = iBottom - ((iBottom - iTop) * iPercent / 100);
         rc.top    = iBarUpto;
         rc.left   = iLeft;
         rc.bottom = iBottom;
         rc.right  = iRight;
      }
   }
   else
   {
      if( iDirection == 0 )
      {
         iBarUpto  = iLeft + ((iRight - iLeft) * iPercent / 100);
         rc.top    = iTop;
         rc.left   = iLeft;
         rc.bottom = iBottom;
         rc.right  = iBarUpto;
      }
      else
      {
         iBarUpto  = iRight - ((iRight - iLeft) * iPercent / 100);
         rc.top    = iTop;
         rc.left   = iBarUpto;
         rc.bottom = iBottom;
         rc.right  = iRight;
      }
   }

   if( bImage )
   {
      void * hImage;
      hb_wvt_DrawImage(_s->hdc, rc.left, rc.top, rc.right - rc.left + 1, rc.bottom - rc.top + 1, HB_PARSTR(9, &hImage, nullptr), FALSE);
      hb_strfree(hImage);
      #if defined(__SETGUI__)
      if( _s->bGui )
      {
         hb_wvt_DrawImage(_s->hGuiDC, rc.left, rc.top, rc.right - rc.left + 1, rc.bottom - rc.top + 1, HB_PARSTR(9, &hImage, nullptr), FALSE);
         hb_strfree(hImage);
      }
      #endif
   }
   else
   {
      crBarColor = static_cast<COLORREF>(hb_parnldef(8, _s->COLORS[0]));

      lb.lbStyle = BS_SOLID;
      lb.lbColor = crBarColor;
      lb.lbHatch = 0;
      hBrush = CreateBrushIndirect(&lb);
      rc.bottom++;
      rc.right++;

      FillRect(_s->hdc, &rc, hBrush);
      #if defined(__SETGUI__)
      if( _s->bGui )
      {
         FillRect(_s->hGuiDC, &rc, hBrush);
      }
      #endif
      DeleteObject(hBrush);
   }
}

/* wvt_CreateFont(cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline,
                  lStrikeout, nCharSet, nQuality, nEscapement) */
HB_FUNC( WVT_CREATEFONT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   LOGFONT logfont;     /* = {0,0,0}; */
   void *  hText = nullptr;

   logfont.lfEscapement     = hb_parni(10) * 10;
   logfont.lfOrientation    = 0;
   logfont.lfWeight         = (hb_parni(4));
   logfont.lfItalic         = static_cast<BYTE>(hb_parl(5));
   logfont.lfUnderline      = static_cast<BYTE>(hb_parl(6));
   logfont.lfStrikeOut      = static_cast<BYTE>(hb_parl(7));
   logfont.lfCharSet        = static_cast<BYTE>(hb_parnidef(8, _s->CodePage));
   logfont.lfOutPrecision   = 0;
   logfont.lfClipPrecision  = 0;
   logfont.lfQuality        = static_cast<BYTE>(hb_parnidef(9, DEFAULT_QUALITY));
   logfont.lfPitchAndFamily = FF_DONTCARE;
   logfont.lfHeight         = hb_parnidef(2, _s->fontHeight);
   logfont.lfWidth = hb_parnidef(3, _s->fontWidth < 0 ? -_s->fontWidth : _s->fontWidth);

   HB_STRNCPY(logfont.lfFaceName, (!HB_ISCHAR(1) ? _s->fontFace : HB_PARSTR(1, &hText, nullptr)), HB_SIZEOFARRAY(logfont.lfFaceName) - 1);
   hb_strfree(hText);

   hb_retnint(reinterpret_cast<HB_PTRUINT>(CreateFontIndirect(&logfont)));
}

/* wvt_LoadPicture( nSlot, cFilePic ) */
HB_FUNC( WVT_LOADPICTURE )
{
   HB_BOOL bResult = HB_FALSE;

   PHB_GTWVT  _s = hb_wvt_gtGetWVT();
   void *     hImage;
   IPicture * iPicture = hb_wvt_gtLoadPicture(HB_PARSTR(2, &hImage, nullptr));
   int        iSlot    = hb_parni(1) - 1;

   hb_strfree(hImage);
   if( iPicture )
   {
      if( _s->pGUI->pPicture[iSlot] )
      {
         hb_wvt_gtDestroyPicture(_s->pGUI->pPicture[iSlot]);
      }
      _s->pGUI->pPicture[iSlot] = iPicture;
      bResult = HB_TRUE;
   }
   hb_retl(bResult);
}

HB_FUNC( WVT_DESTROYPICTURE )
{
   IPicture * iPicture = reinterpret_cast<IPicture*>(static_cast<HB_PTRUINT>(hb_parnl(1)));
   hb_retl(hb_wvt_gtDestroyPicture(iPicture));
}

/* wvt_LoadPictureEx( cFilePic ) */
HB_FUNC( WVT_LOADPICTUREEX )
{
   void *     hImage;
   IPicture * iPicture = hb_wvt_gtLoadPicture(HB_PARSTR(1, &hImage, nullptr));

   hb_strfree(hImage);
   if( iPicture )
   {
      hb_retnint(reinterpret_cast<HB_PTRUINT>(iPicture));
   }
}

HB_FUNC( WVT_LOADPICTUREFROMRESOURCE )
{
   HB_BOOL bResult = HB_FALSE;

   PHB_GTWVT  _s = hb_wvt_gtGetWVT();
   void *     hResource;
   void *     hSection;
   IPicture * iPicture = hb_wvt_gtLoadPictureFromResource(HB_PARSTR(2, &hResource, nullptr), HB_PARSTR(3, &hSection, nullptr));
   int        iSlot    = hb_parni(1) - 1;

   hb_strfree(hResource);
   hb_strfree(hSection);
   if( iPicture )
   {
      if( _s->pGUI->pPicture[iSlot] )
      {
         hb_wvt_gtDestroyPicture(_s->pGUI->pPicture[iSlot]);
      }
      _s->pGUI->pPicture[iSlot] = iPicture;
      bResult = HB_TRUE;
   }
   hb_retl(bResult);
}

HB_FUNC( WVT_LOADPICTUREFROMRESOURCEEX )
{
   void *     hResource;
   void *     hSection;
   IPicture * iPicture = hb_wvt_gtLoadPictureFromResource(HB_PARSTR(1, &hResource, nullptr), HB_PARSTR(2, &hSection, nullptr));

   hb_strfree(hResource);
   hb_strfree(hSection);
   if( iPicture )
   {
      hb_retnint(reinterpret_cast<HB_PTRUINT>(iPicture));
   }
}

/* wvt_LoadFont(nSlotFont, cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline, lStrikeout,
                nCharSet, nQuality, nEscapement) */
HB_FUNC( WVT_LOADFONT )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   LOGFONT logfont;     /* = {0}; */
   int     iSlot = hb_parni(1) - 1;
   HFONT   hFont;
   void *  hF = nullptr;

   logfont.lfEscapement     = hb_parni(11) * 10;
   logfont.lfOrientation    = 0;
   logfont.lfWeight         = hb_parni(5);
   logfont.lfItalic         = static_cast<BYTE>(hb_parl(6));
   logfont.lfUnderline      = static_cast<BYTE>(hb_parl(7));
   logfont.lfStrikeOut      = static_cast<BYTE>(hb_parl(8));
   logfont.lfCharSet        = static_cast<BYTE>(hb_parnidef(9, _s->CodePage));
   logfont.lfOutPrecision   = 0;
   logfont.lfClipPrecision  = 0;
   logfont.lfQuality        = static_cast<BYTE>(hb_parnidef(10, DEFAULT_QUALITY));
   logfont.lfPitchAndFamily = FF_DONTCARE;
   logfont.lfHeight         = hb_parnidef(3, _s->fontHeight);
   logfont.lfWidth = hb_parnidef(4, _s->fontWidth < 0 ? -_s->fontWidth : _s->fontWidth);

   HB_STRNCPY(logfont.lfFaceName, (HB_ISCHAR(2) ? HB_PARSTR(2, &hF, nullptr) : _s->fontFace), HB_SIZEOFARRAY(logfont.lfFaceName) - 1);
   hb_strfree(hF);

   hFont = CreateFontIndirect(&logfont);
   if( hFont )
   {
      if( _s->pGUI->hUserFonts[iSlot] )
      {
         DeleteObject(_s->pGUI->hUserFonts[iSlot]);
      }
      _s->pGUI->hUserFonts[iSlot] = hFont;
   }
}

/* wvt_LoadPen( nSlot, nStyle, nWidth, nRGBColor ) */
HB_FUNC( WVT_LOADPEN )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   int      iPenWidth, iPenStyle;
   COLORREF crColor;
   HPEN     hPen;
   int      iSlot = hb_parni(1) - 1;

   iPenStyle = hb_parni(2);
   iPenWidth = hb_parni(3);
   crColor   = static_cast<COLORREF>(hb_parnldef(4, RGB(0, 0, 0)));

   hPen = CreatePen(iPenStyle, iPenWidth, crColor);

   if( hPen )
   {
      if( _s->pGUI->hUserPens[iSlot] )
      {
         DeleteObject(_s->pGUI->hUserPens[iSlot]);
      }
      _s->pGUI->hUserPens[iSlot] = hPen;

      hb_retl(true);
   }
   else
   {
      hb_retl(false);
   }
}

/* wvt_SaveScreen( nTop, nLeft, nBottom, nRight ) --> aSrc */
HB_FUNC( WVT_SAVESCREEN )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   HDC      hCompDC;
   HBITMAP  hBmp, oldBmp;
   POINT    xy = {0, 0};
   int      iTop, iLeft, iBottom, iRight, iWidth, iHeight;
   PHB_ITEM info = hb_itemArrayNew(3);

   xy    = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));
   iTop  = xy.y;
   iLeft = xy.x;

   xy      = hb_wvt_gtGetXYFromColRow(hb_parni(4) + 1, hb_parni(3) + 1);
   iBottom = xy.y - 1;
   iRight  = xy.x - 1;

   iWidth  = iRight - iLeft + 1;
   iHeight = iBottom - iTop + 1;

   hBmp = CreateCompatibleBitmap(_s->hdc, iWidth, iHeight);

   hCompDC = CreateCompatibleDC(_s->hdc);
   oldBmp  = static_cast<HBITMAP>(SelectObject(hCompDC, hBmp));
   BitBlt(hCompDC, 0, 0, iWidth, iHeight, _s->hdc, iLeft, iTop, SRCCOPY);
   SelectObject(hCompDC, oldBmp);
   DeleteDC(hCompDC);

   hb_arraySetNI(info, 1, iWidth);
   hb_arraySetNI(info, 2, iHeight);
   hb_arraySetNInt(info, 3, reinterpret_cast<HB_PTRUINT>(hBmp));

   hb_itemReturnRelease(info);
}

/* wvt_RestScreen( nTop, nLeft, nBottom, nRight, aScr, lDoNotDestroyBMP ) */
HB_FUNC( WVT_RESTSCREEN )
{
   PHB_GTWVT _s = hb_wvt_gtGetWVT();

   POINT   xy = {0, 0};
   int     iTop, iLeft, iBottom, iRight, iWidth, iHeight;
   HBITMAP hBmp;
   HDC     hCompDC;

   HB_BOOL bResult = HB_FALSE;
   HB_BOOL bDoNotDestroyBMP = hb_parl(6);

   xy    = hb_wvt_gtGetXYFromColRow(hb_parni(2), hb_parni(1));
   iTop  = xy.y;
   iLeft = xy.x;

   xy      = hb_wvt_gtGetXYFromColRow(hb_parni(4) + 1, hb_parni(3) + 1);
   iBottom = xy.y - 1;
   iRight  = xy.x - 1;

   iWidth  = iRight - iLeft + 1;
   iHeight = iBottom - iTop + 1;

   hCompDC = CreateCompatibleDC(_s->hdc);
   hBmp    = static_cast<HBITMAP>(SelectObject(hCompDC, reinterpret_cast<HBITMAP>(static_cast<HB_PTRUINT>(hb_parvnint(5, 3)))));
   if( hBmp )
   {
      if( (iWidth == hb_parvni(5, 1)) && (iHeight == hb_parvni(5, 2)) )
      {
         if( BitBlt(_s->hdc, iLeft, iTop, iWidth, iHeight, hCompDC, 0, 0, SRCCOPY) )
         {
            bResult = HB_TRUE;
         }
      }
      else
      {
         if( StretchBlt(_s->hdc, iLeft, iTop, iWidth, iHeight, hCompDC, 0, 0, hb_parvni(5, 1), hb_parvni(5, 2), SRCCOPY) )
         {
            bResult = HB_TRUE;
         }
      }
   }
   DeleteDC(hCompDC);

   if( !bDoNotDestroyBMP )
   {
      SelectObject(hCompDC, hBmp);
      DeleteObject(reinterpret_cast<HBITMAP>(static_cast<HB_PTRUINT>(hb_parvnint(5, 3))));
   }
   hb_retl(bResult);
}

HB_FUNC( WVG_GTINFOEX )
{
   if( HB_ISPOINTER(1) && HB_ISNUM(2) )
   {
      PHB_GT pGT = hb_gt_ItemBase(hb_param(1, Harbour::Item::ANY));

      if( pGT )
      {
         HB_GT_INFO gtInfo;

         gtInfo.pNewVal  = hb_param(3, Harbour::Item::ANY);
         gtInfo.pNewVal2 = hb_param(4, Harbour::Item::ANY);
         gtInfo.pResult  = nullptr;

         HB_GTSELF_INFO(pGT, hb_parni(2), &gtInfo);
         hb_gt_BaseFree(pGT);

         if( gtInfo.pResult )
         {
            hb_itemReturnRelease(gtInfo.pResult);
         }
      }
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}
