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

#if defined(__BORLANDC__)
#  if !defined(NONAMELESSUNION)
#     define NONAMELESSUNION
#  endif
#  if defined(DUMMYUNIONNAME)
#     undef DUMMYUNIONNAME
#  endif
#  if defined(DUMMYUNIONNAME2)
#     undef DUMMYUNIONNAME2
#  endif
#  if defined(DUMMYUNIONNAME3)
#     undef DUMMYUNIONNAME3
#  endif
#  if defined(DUMMYUNIONNAME4)
#     undef DUMMYUNIONNAME4
#  endif
#  if defined(DUMMYUNIONNAME5)
#     undef DUMMYUNIONNAME5
#  endif
#endif

#include "gtwvg.hpp"

#if defined(__BORLANDC__) && __BORLANDC__ >= 0x0610
   #undef NONAMELESSUNION
#endif

#if defined(NONAMELESSUNION)
#  define HB_WIN_V_UNION(x, z)  ((x).DUMMYUNIONNAME.z)
#else
#  define HB_WIN_V_UNION(x, z)  ((x).z)
#endif

#define WIN_STATUSBAR_MAX_PARTS  256

/* Bitmap Management Function. Courtesy GTWVW */

static BITMAPINFO * PackedDibLoad(const char * pszFileName)
{
   BITMAPINFO * pbmi = nullptr;

   PHB_FILE pFile = hb_fileExtOpen(pszFileName, nullptr, FO_READ | FO_SHARED | FO_PRIVATE | FXO_DEFAULTS | FXO_SHARELOCK, nullptr, nullptr);

   if( pFile )
   {
      BITMAPFILEHEADER bmfh;

      if( hb_fileRead(pFile, &bmfh, sizeof(bmfh), -1) == sizeof(bmfh) &&
          bmfh.bfType == 0x4d42 /* "BM" */ &&
          bmfh.bfSize > sizeof(bmfh) &&
          bmfh.bfSize <= (32 * 1024 * 1024) /* an arbitrary size limit */ )
      {
         HB_SIZE nPackedDibSize = bmfh.bfSize - sizeof(bmfh);

         pbmi = static_cast<BITMAPINFO*>(hb_xgrab(nPackedDibSize));

         if( hb_fileRead(pFile, pbmi, nPackedDibSize, -1) != nPackedDibSize )
         {
            hb_xfree(pbmi);
            pbmi = nullptr;
         }
      }

      hb_fileClose(pFile);
   }

   return pbmi;
}

static int PackedDibGetWidth(BITMAPINFO * pPackedDib)
{
   if( pPackedDib->bmiHeader.biSize == sizeof(BITMAPCOREHEADER) )
   {
      return (reinterpret_cast<PBITMAPCOREINFO>(pPackedDib))->bmciHeader.bcWidth;
   }
   else
   {
      return pPackedDib->bmiHeader.biWidth;
   }
}

static int PackedDibGetHeight(BITMAPINFO * pPackedDib)
{
   if( pPackedDib->bmiHeader.biSize == sizeof(BITMAPCOREHEADER) )
   {
      return (reinterpret_cast<PBITMAPCOREINFO>(pPackedDib))->bmciHeader.bcHeight;
   }
   else
   {
      return abs(pPackedDib->bmiHeader.biHeight);
   }
}

static int PackedDibGetBitCount(BITMAPINFO * pPackedDib)
{
   if( pPackedDib->bmiHeader.biSize == sizeof(BITMAPCOREHEADER) )
   {
      return (reinterpret_cast<PBITMAPCOREINFO>(pPackedDib))->bmciHeader.bcBitCount;
   }
   else
   {
      return pPackedDib->bmiHeader.biBitCount;
   }
}

static int PackedDibGetInfoHeaderSize(BITMAPINFO * pPackedDib)
{
   if( pPackedDib->bmiHeader.biSize == sizeof(BITMAPCOREHEADER) )
   {
      return (reinterpret_cast<PBITMAPCOREINFO>(pPackedDib))->bmciHeader.bcSize;
   }
   else if( pPackedDib->bmiHeader.biSize == sizeof(BITMAPINFOHEADER) )
   {
      return pPackedDib->bmiHeader.biSize + (pPackedDib->bmiHeader.biCompression == BI_BITFIELDS ? 12 : 0);
   }
   else
   {
      return pPackedDib->bmiHeader.biSize;
   }
}

static int PackedDibGetColorsUsed(BITMAPINFO * pPackedDib)
{
   if( pPackedDib->bmiHeader.biSize == sizeof(BITMAPCOREHEADER) )
   {
      return 0;
   }
   else
   {
      return pPackedDib->bmiHeader.biClrUsed;
   }
}

static int PackedDibGetNumColors(BITMAPINFO * pPackedDib)
{
   int iNumColors = PackedDibGetColorsUsed(pPackedDib);

   if( iNumColors == 0 && PackedDibGetBitCount(pPackedDib) < 16 )
   {
      iNumColors = 1 << PackedDibGetBitCount(pPackedDib);
   }

   return iNumColors;
}

static int PackedDibGetColorTableSize(BITMAPINFO * pPackedDib)
{
   if( pPackedDib->bmiHeader.biSize == sizeof(BITMAPCOREHEADER) )
   {
      return PackedDibGetNumColors(pPackedDib) * sizeof(RGBTRIPLE);
   }
   else
   {
      return PackedDibGetNumColors(pPackedDib) * sizeof(RGBQUAD);
   }
}

static BYTE * PackedDibGetBitsPtr(BITMAPINFO * pPackedDib)
{
   return reinterpret_cast<BYTE*>(pPackedDib) + PackedDibGetInfoHeaderSize(pPackedDib) + PackedDibGetColorTableSize(pPackedDib);
}

HB_FUNC( WVG_PREPAREBITMAPFROMFILE )
{
   HBITMAP hBitmap = nullptr;

   HB_BOOL fMap3Dcolors = hb_parl(4);
   BITMAPINFO * pPackedDib = fMap3Dcolors ? nullptr : PackedDibLoad(hb_parcx(1) /* szFileName */);

   if( pPackedDib || fMap3Dcolors )
   {
      int iWidth, iExpWidth = hb_parni(2);
      int iHeight, iExpHeight = hb_parni(3);
      HWND hCtrl = hbwapi_par_raw_HWND(5);

      HDC hdc = GetDC(hCtrl);

      if( fMap3Dcolors )
      {
         void * hString;

         hBitmap = static_cast<HBITMAP>(LoadImage(nullptr, HB_PARSTRDEF(1, &hString, nullptr), IMAGE_BITMAP, iExpWidth, iExpHeight, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS));

         hb_strfree(hString);

         iWidth = iExpWidth;
         iHeight = iExpHeight;
      }
      else
      {
         hBitmap = CreateDIBitmap(hdc, reinterpret_cast<PBITMAPINFOHEADER>(pPackedDib), CBM_INIT, PackedDibGetBitsPtr(pPackedDib), pPackedDib, DIB_RGB_COLORS);

         iWidth  = PackedDibGetWidth(pPackedDib);
         iHeight = PackedDibGetHeight(pPackedDib);
      }

      if( iExpWidth == 0 && iExpHeight == 0 )
      {
         iWidth  = iExpWidth;
         iHeight = iExpHeight;
      }

      if( hBitmap && (iExpWidth != iWidth || iExpHeight != iHeight) )
      {
         HDC hdcSource, hdcTarget;
         HBITMAP hBitmap2;

         hdcSource = CreateCompatibleDC(hdc);
         SelectObject(hdcSource, hBitmap);

         hdcTarget = CreateCompatibleDC(hdc);
         hBitmap2 = CreateCompatibleBitmap(hdcSource, iExpWidth, iExpHeight);
         SelectObject(hdcTarget, hBitmap2);

         /*
           [1] handle to destination DC
           [2] x-coord of destination upper-left corner
           [3] y-coord of destination upper-left corner
           [4] width of destination rectangle
           [5] height of destination rectangle
           [6] handle to source DC
           [7] x-coord of source upper-left corner
           [8] y-coord of source upper-left corner
           [9] width of source rectangle
           [10] height of source rectangle
           [11] SRCCOPY
         */

         if( StretchBlt(hdcTarget, 0, 0, iExpWidth, iExpHeight, hdcSource, 0, 0, iWidth, iHeight, SRCCOPY) )
         {
            DeleteObject(hBitmap);
            hBitmap = hBitmap2;
         }
         else
         {
            DeleteObject(hBitmap2);
         }

         DeleteDC(hdcSource);
         DeleteDC(hdcTarget);
      }

      ReleaseDC(hCtrl, hdc);

      if( pPackedDib )
      {
         hb_xfree(pPackedDib);
      }
   }

   hb_retptr(static_cast<void*>(hBitmap));
}

HB_FUNC( WVG_STATUSBARCREATEPANEL )
{
   HWND hWndSB = hbwapi_par_raw_HWND(1);

   if( hWndSB && IsWindow(hWndSB) )
   {
      switch( hb_parni(2) /* nMode */ )
      {
         case 0:
         {
            int ptArray[WIN_STATUSBAR_MAX_PARTS];
            RECT rc = { 0, 0, 0, 0 };
            int width;
            int iParts = static_cast<int>(SendMessage(hWndSB, SB_GETPARTS, static_cast<WPARAM>(HB_SIZEOFARRAY(ptArray)) - 1, reinterpret_cast<LPARAM>(static_cast<LPINT>(ptArray))));

            GetClientRect(hWndSB, &rc);
            width = static_cast<int>(rc.right / (iParts + 1));
            for( int n = 0; n < iParts; n++ )
            {
               ptArray[n] = width * (n + 1);
            }

            ptArray[iParts] = -1;

            hb_retl(static_cast<HB_BOOL>(SendMessage(hWndSB, SB_SETPARTS, static_cast<WPARAM>(iParts) + 1, reinterpret_cast<LPARAM>(static_cast<LPINT>(ptArray)))));
            return;
         }
         case -1:
         {
            RECT rc;

            if( GetClientRect(hWndSB, &rc) )
            {
               int ptArray = rc.right;

               hb_retl(static_cast<HB_BOOL>(SendMessage(hWndSB, SB_SETPARTS, static_cast<WPARAM>(1), reinterpret_cast<LPARAM>(&ptArray))));
               return;
            }
         }
      }
   }

   hb_retl(HB_FALSE);
}

HB_FUNC( WVG_STATUSBARSETTEXT )
{
   HWND hWndSB = hbwapi_par_raw_HWND(1);

   if( hWndSB && IsWindow(hWndSB) )
   {
      int iPart = LOBYTE(hb_parnidef(2, 1) - 1);
      int iFlags = static_cast<int>(HIWORD(SendMessage(hWndSB, SB_GETTEXTLENGTH, static_cast<WPARAM>(iPart), 0)));
      void * hCaption;

      SendMessage(hWndSB, SB_SETTEXT, static_cast<WPARAM>(iPart) | iFlags, reinterpret_cast<LPARAM>(HB_PARSTR(3, &hCaption, nullptr)));

      hb_strfree(hCaption);
   }
}

HB_FUNC( WVG_STATUSBARREFRESH )
{
#if 0
   HWND hWndSB = hbwapi_par_raw_HWND(1);

   if( hWndSB && IsWindow(hWndSB) )
   {
      int ptArray[WIN_STATUSBAR_MAX_PARTS];
      int iParts, i;

      iParts = SendMessage(hWndSB, SB_GETPARTS, static_cast<WPARAM>(HB_SIZEOFARRAY(ptArray)), static_cast<LPARAM>(static_cast<LPINT>(ptArray)));

      ptArray[iParts - 1] = -1;

      if( SendMessage(hWndSB, SB_SETPARTS, iParts, static_cast<LPARAM>(static_cast<LPINT>(ptArray))) )
      {
         hb_retl(HB_TRUE);
         return;
      }
   }

   hb_retl(HB_FALSE);
#endif
}

/*
wvg_GetNMHdrInfo(nlParam)
*/
HB_FUNC( WVG_GETNMHDRINFO )
{
   LPNMHDR lpnmh = static_cast<LPNMHDR>(hbwapi_par_raw_HANDLE(1));

   PHB_ITEM pEvParams = hb_itemArrayNew(3);

   hb_arraySetNI(pEvParams, 1, lpnmh->code);
   hb_arraySetNInt(pEvParams, 2, static_cast<HB_PTRUINT>(lpnmh->idFrom));
   hbwapi_arraySet_HANDLE( pEvParams, 3, lpnmh->hwndFrom );

   hb_itemReturnRelease(pEvParams);
}

/*
wvg_GetNMMouseInfo(nlParam)
*/
HB_FUNC( WVG_GETNMMOUSEINFO )
{
   LPNMMOUSE nmm = static_cast<LPNMMOUSE>(hbwapi_par_raw_HANDLE(1));
   NMHDR nmh = nmm->hdr;

   PHB_ITEM pEvParams = hb_itemArrayNew(4);

   hb_arraySetNI(pEvParams, 1, nmh.code);
   hb_arraySetNInt(pEvParams, 2, static_cast<HB_PTRUINT>(nmh.idFrom));
   hbwapi_arraySet_HANDLE( pEvParams, 3, nmh.hwndFrom );
   hb_arraySetNInt(pEvParams, 4, static_cast<HB_PTRUINT>(nmm->dwItemSpec));

   hb_itemReturnRelease(pEvParams);
}

/*
wvg_GetNMTreeViewInfo(nlParam)
*/
HB_FUNC( WVG_GETNMTREEVIEWINFO )
{
   LPNMTREEVIEW pnmtv = static_cast<LPNMTREEVIEW>(hbwapi_par_raw_HANDLE(1));
   NMHDR nmh = pnmtv->hdr;

   PHB_ITEM pEvParams = hb_itemArrayNew(4);

   hb_arraySetNI(pEvParams, 1, nmh.code);
   hb_arraySetNInt(pEvParams, 2, static_cast<HB_PTRUINT>(nmh.idFrom));
   hbwapi_arraySet_HANDLE(pEvParams, 3, nmh.hwndFrom);
   hb_arraySetNI(pEvParams, 4, pnmtv->action);

   hb_itemReturnRelease(pEvParams);
}

/*
wvg_TreeView_GetSelectionInfo(::hWnd, nlParam, @cParent, @cText, @hParentOfSelected, @hItemSelected)
*/
HB_FUNC( WVG_TREEVIEW_GETSELECTIONINFO )
{
   LPNMTREEVIEW pnmtv = static_cast<LPNMTREEVIEW>(hbwapi_par_raw_HANDLE(2));
   HTREEITEM hSelected = pnmtv->itemNew.hItem;

   if( hSelected != nullptr )
   {
      TCHAR text[MAX_PATH + 1];
      TCHAR Parent[MAX_PATH + 1];
      TV_ITEM item;
      HTREEITEM hParent;

      hbwapi_stor_HANDLE(hSelected, 6);

      item.mask = TVIF_HANDLE | TVIF_TEXT | TVIF_IMAGE;
      item.hItem = hSelected;
      item.pszText = text;
      item.cchTextMax = HB_SIZEOFARRAY(text) - 1;

      text[0] = TEXT('\0');

      if( TreeView_GetItem(hbwapi_par_raw_HWND(1), &item) )
      {
         HB_STORSTR(text, 4);
      }
      else
      {
         hb_storc(nullptr, 4);
      }

      hParent = TreeView_GetParent(hbwapi_par_raw_HWND(1), hSelected);
      hbwapi_stor_HANDLE(hParent, 5);

      item.mask = TVIF_HANDLE | TVIF_TEXT;
      item.hItem = hParent;
      item.pszText = Parent;
      item.cchTextMax = HB_SIZEOFARRAY(Parent) - 1;

      Parent[0] = TEXT('\0');

      if( TreeView_GetItem( hbwapi_par_raw_HWND(1), &item ) )
      {
         HB_STORSTR(Parent, 3);
      }
      else
      {
         hb_storc(nullptr, 3);
      }
   }
   else
   {
      hb_storc(nullptr, 3);
      hb_storc(nullptr, 4);
      hbwapi_stor_HANDLE(nullptr, 5);
      hbwapi_stor_HANDLE(nullptr, 6);
   }
}

/*
wvg_TreeView_AddItem(oItem:hTree, hParent, oItem:Caption) --> hItem
*/
HB_FUNC( WVG_TREEVIEW_ADDITEM )
{
   TVINSERTSTRUCT tvis;
   void * hText;

   tvis.hInsertAfter = TVI_LAST;
   HB_WIN_V_UNION(tvis, item.mask) = TVIF_TEXT | TVIF_IMAGE | TVIF_SELECTEDIMAGE | TVIF_STATE;
   HB_WIN_V_UNION(tvis, item.cchTextMax) = MAX_PATH + 1;
   HB_WIN_V_UNION(tvis, item.stateMask) = TVIS_BOLD | TVIS_CUT | TVIS_DROPHILITED |
                                          TVIS_EXPANDEDONCE | TVIS_SELECTED | TVIS_EXPANDPARTIAL |
                                          TVIS_OVERLAYMASK | TVIS_STATEIMAGEMASK | TVIS_USERMASK;

   HB_WIN_V_UNION(tvis, item.state) = 0;  /* TVI_BOLD */
   tvis.hParent = hbwapi_par_raw_HTREEITEM(2);
   HB_WIN_V_UNION(tvis, item.pszText ) = static_cast<LPTSTR>(HB_UNCONST(HB_PARSTRDEF(3, &hText, nullptr)));

   hbwapi_ret_raw_HANDLE(TreeView_InsertItem(hbwapi_par_raw_HWND(1), &tvis));

   hb_strfree(hText);
}

HB_FUNC( WVG_TREEVIEW_SHOWEXPANDED )
{
   HWND hwnd = hbwapi_par_raw_HWND(1);
   HTREEITEM hroot, hitem, hitem1, hitem2, hitem3;
   int iExpand = hb_parl(2) ? TVE_EXPAND : TVE_COLLAPSE;
   int iLevels = hb_parni(3) <= 0 ? 5 : hb_parni(3);

   hroot = TreeView_GetRoot(hwnd);
   if( hroot )
   {
      ( void ) TreeView_Expand(hwnd, hroot, iExpand); // TODO: C++ cast
      if( iLevels >= 2 )
      {
         hitem = TreeView_GetNextItem(hwnd, hroot, TVGN_CHILD);
         while( hitem )
         {
            ( void ) TreeView_Expand(hwnd, hitem, iExpand); // TODO: C++ cast
            if( iLevels >= 3 )
            {
               hitem1 = TreeView_GetNextItem(hwnd, hitem, TVGN_CHILD);
               while( hitem1 )
               {
                  ( void ) TreeView_Expand(hwnd, hitem1, iExpand); // TODO: C++ cast
                  if( iLevels >= 4 )
                  {
                     hitem2 = TreeView_GetNextItem(hwnd, hitem1, TVGN_CHILD);
                     while( hitem2 )
                     {
                        ( void ) TreeView_Expand(hwnd, hitem2, iExpand); // TODO: C++ cast
                        if( iLevels >= 5 )
                        {
                           hitem3 = TreeView_GetNextItem(hwnd, hitem2, TVGN_CHILD);
                           while( hitem3 )
                           {
                              ( void ) TreeView_Expand(hwnd, hitem3, iExpand); // TODO: C++ cast
                              hitem3 = TreeView_GetNextItem(hwnd, hitem3, TVGN_NEXT);
                           }
                        }
                        hitem2 = TreeView_GetNextItem(hwnd, hitem2, TVGN_NEXT);
                     }
                  }
                  hitem1 = TreeView_GetNextItem(hwnd, hitem1, TVGN_NEXT);
               }
            }
            hitem = TreeView_GetNextItem(hwnd, hitem, TVGN_NEXT);
         }
      }
   }
}

/* WvgFontDialog() */

static PHB_ITEM wvg_logfontTOarray(LPLOGFONT lf)
{
   PHB_ITEM aFont = hb_itemArrayNew(15);

   HB_ARRAYSETSTR(aFont, 1, lf->lfFaceName);
   hb_arraySetNL(aFont, 2, lf->lfHeight);
   hb_arraySetNL(aFont, 3, lf->lfWidth);
   hb_arraySetNL(aFont, 4, lf->lfWeight);
   hb_arraySetL(aFont, 5, lf->lfItalic);
   hb_arraySetL(aFont, 6, lf->lfUnderline);
   hb_arraySetL(aFont, 7, lf->lfStrikeOut);
   hb_arraySetNI(aFont, 8, lf->lfCharSet);
   hb_arraySetNI(aFont, 9, lf->lfEscapement);
   hb_arraySetNI(aFont, 10, lf->lfOrientation);
   hb_arraySetNI(aFont, 11, lf->lfOutPrecision);
   hb_arraySetNI(aFont, 12, lf->lfClipPrecision);
   hb_arraySetNI(aFont, 13, lf->lfQuality);
   hb_arraySetNI(aFont, 14, lf->lfPitchAndFamily);

   return aFont;
}

/* An Alternative to WndProc Callbacks */

static UINT_PTR CALLBACK WvgDialogProcChooseFont(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
   UINT_PTR bret = 0;
   HB_BOOL binit = HB_FALSE;
   PHB_ITEM block;

   if( msg == WM_INITDIALOG )
   {
      CHOOSEFONT * cf = reinterpret_cast<CHOOSEFONT*>(lParam);
      PHB_ITEM pBlock = hb_itemNew(reinterpret_cast<PHB_ITEM>(cf->lCustData));
      SetProp(hwnd, TEXT("DIALOGPROC"), pBlock);
      binit = HB_TRUE;
   }

   block = static_cast<PHB_ITEM>(GetProp(hwnd, TEXT("DIALOGPROC")));

   if( block )
   {
      hb_vmPushEvalSym();
      hb_vmPush(block);
      hbwapi_vmPush_HANDLE(hwnd);
      hb_vmPushNumInt(msg);
      hb_vmPushNumInt(static_cast<HB_PTRUINT>(wParam));
      hb_vmPushNumInt(static_cast<HB_PTRUINT>(lParam));
      hb_vmDo(4);

      bret = static_cast<UINT_PTR>(hbwapi_par_RESULT(-1));

      if( msg == WM_NCDESTROY )
      {
         RemoveProp(hwnd, TEXT("DIALOGPROC"));
         hb_itemRelease(block);
      }
   }

   return binit ? 1 : bret;
}

/*
wvg_ChooseFont(hWnd, nWndProc, familyName, nominalPointSize, viewScreenFonts, viewPrinterFonts)
*/
HB_FUNC( WVG_CHOOSEFONT )
{
   CHOOSEFONT cf;
   LOGFONT lf;
   DWORD Flags = CF_EFFECTS | CF_SHOWHELP | CF_APPLY | CF_INITTOLOGFONTSTRUCT | CF_ENABLEHOOK;
   LONG PointSize = 0;
   HWND hWnd = hbwapi_par_raw_HWND(1);

   memset(&cf, 0, sizeof(cf));
   memset(&lf, 0, sizeof(lf));

   if( HB_ISCHAR(3) )
   {
      void * hText;
      HB_STRNCPY(lf.lfFaceName, HB_PARSTR(3, &hText, nullptr), HB_SIZEOFARRAY(lf.lfFaceName) - 1);
      hb_strfree(hText);
   }

   if( hb_parnl(4) )
   {
      HDC hdc = GetDC(hWnd);
      PointSize = -MulDiv(static_cast<LONG>(hb_parnl(4)), GetDeviceCaps(hdc, LOGPIXELSY ), 72);
      ReleaseDC(hWnd, hdc);
   }

   lf.lfHeight         = PointSize;
   lf.lfWidth          = 0;
   lf.lfWeight         = 0;
   lf.lfItalic         = 0;
   lf.lfUnderline      = 0;
   lf.lfStrikeOut      = 0;
   lf.lfCharSet        = DEFAULT_CHARSET;
   lf.lfQuality        = DEFAULT_QUALITY;
   lf.lfPitchAndFamily = FF_DONTCARE;

   #if 0
   Flags |= CF_TTONLY;
   Flags |= CF_FIXEDPITCHONLY;
   Flags |= CF_SCALABLEONLY;
   Flags |= CF_NOVECTORFONTS;
   Flags |= CF_NOSCRIPTSEL;
   Flags |= CF_NOSIMULATIONS;  /* ::synthesizeFonts == .F. */
   #endif

   if( hb_parl(5) )
   {
      Flags |= CF_SCREENFONTS;
   }
   if( hb_parl(6) )
   {
      Flags |= CF_PRINTERFONTS;
   }

   cf.lStructSize = sizeof(cf);
   cf.hwndOwner   = hWnd;
   cf.hDC         = nullptr;  /* only when ::oPrinterPS is defined */
   cf.lpLogFont   = &lf;
   cf.iPointSize  = PointSize;
   cf.Flags       = Flags;
   cf.rgbColors   = RGB(0, 0, 0);

   cf.lCustData = reinterpret_cast<HB_PTRUINT>(hb_param(2, Harbour::Item::EVALITEM));
   cf.lpfnHook  = static_cast<LPCFHOOKPROC>(WvgDialogProcChooseFont);

   cf.lpTemplateName = nullptr;
   cf.hInstance      = nullptr;
   cf.lpszStyle      = nullptr;
   cf.nFontType      = SCREEN_FONTTYPE;  /* ?? */
   cf.nSizeMin       = 0;
   cf.nSizeMax       = 0;

   if( ChooseFont(&cf) )
   {
      PHB_ITEM aFont = wvg_logfontTOarray(&lf);
      PHB_ITEM aInfo = hb_itemArrayNew(4);

      hb_arraySetNI(aInfo, 1, cf.iPointSize);
      hb_arraySetNInt(aInfo, 2, cf.rgbColors);
      hb_arraySetNI(aInfo, 3, cf.nFontType);
      HB_ARRAYSETSTR(aInfo, 4, cf.lpszStyle);

      hb_arraySet(aFont, 15, aInfo);

      hb_itemReturnRelease(aFont);
      hb_itemRelease(aInfo);
   }
}

HB_FUNC( WVG_CHOOSEFONT_GETLOGFONT )
{
   LOGFONT lf;

   memset(&lf, 0, sizeof(lf));

   SendMessage(hbwapi_par_raw_HWND(1), WM_CHOOSEFONT_GETLOGFONT, 0, reinterpret_cast<LPARAM>(&lf));

   hb_itemReturnRelease(wvg_logfontTOarray(&lf));
}

HB_FUNC( WVG_FONTCREATE )
{
   LOGFONT lf;
   HFONT hFont;

   PHB_ITEM aFont = hb_param(1, Harbour::Item::ARRAY);

   memset(&lf, 0, sizeof(lf));

   if( aFont )
   {
      HB_ITEMCOPYSTR(hb_arrayGetItemPtr(aFont, 1), lf.lfFaceName, HB_SIZEOFARRAY(lf.lfFaceName) - 1);

      lf.lfHeight         = static_cast<LONG>(hb_arrayGetNL(aFont, 2));
      lf.lfWidth          = static_cast<LONG>(hb_arrayGetNL(aFont, 3));
      lf.lfWeight         = static_cast<LONG>(hb_arrayGetNL(aFont, 4));
      lf.lfItalic         = static_cast<BYTE>(hb_arrayGetL(aFont, 5));
      lf.lfUnderline      = static_cast<BYTE>(hb_arrayGetL(aFont, 6));
      lf.lfStrikeOut      = static_cast<BYTE>(hb_arrayGetL(aFont, 7));
      lf.lfCharSet        = static_cast<BYTE>(hb_arrayGetNI(aFont, 8));
      lf.lfEscapement     = static_cast<BYTE>(hb_arrayGetNI(aFont, 9));
      lf.lfOrientation    = static_cast<BYTE>(hb_arrayGetNI(aFont, 10));
      lf.lfOutPrecision   = static_cast<BYTE>(hb_arrayGetNI(aFont, 11));
      lf.lfClipPrecision  = static_cast<BYTE>(hb_arrayGetNI(aFont, 12));
      lf.lfQuality        = static_cast<BYTE>(hb_arrayGetNI(aFont, 13));
      lf.lfPitchAndFamily = static_cast<BYTE>(hb_arrayGetNI(aFont, 14));
   }

   hFont = CreateFontIndirect(&lf);
   if( hFont )
   {
      aFont = wvg_logfontTOarray(&lf);
      hbwapi_arraySet_HANDLE(aFont, 15, hFont);
   }
   else
   {
      memset(&lf, 0, sizeof(lf));
      aFont = wvg_logfontTOarray(&lf);
   }

   hb_itemReturnRelease(aFont);
}

/*
wvg_PointSizeToHeight(hdc, nPointSize)
*/
HB_FUNC( WVG_POINTSIZETOHEIGHT )
{
   HDC hdc = hbwapi_is_HANDLE(1) ? hbwapi_par_raw_HDC(1) : GetDC(GetDesktopWindow());

   hb_retnl(static_cast<long>(-MulDiv(static_cast<LONG>(hb_parnl(2)), GetDeviceCaps(hdc, LOGPIXELSY), 72)));

   if( !hbwapi_is_HANDLE(1) )
   {
      ReleaseDC(GetDesktopWindow(), hdc);
   }
}

/*
wvg_HeightToPointSize(hdc, nHeight)
*/
HB_FUNC( WVG_HEIGHTTOPOINTSIZE )
{
   HDC hdc = hbwapi_is_HANDLE(1) ? hbwapi_par_raw_HDC(1) : GetDC(GetDesktopWindow());

   hb_retnl(static_cast<long>(-MulDiv(static_cast<LONG>(hb_parnl(2)), 72, GetDeviceCaps(hdc, LOGPIXELSY))));

   if( !hbwapi_is_HANDLE(1) )
   {
      ReleaseDC(GetDesktopWindow(), hdc);
   }
}

HB_FUNC( WVG_SETCURRENTBRUSH )
{
#ifndef GCL_HBRBACKGROUND
#define GCL_HBRBACKGROUND  GCLP_HBRBACKGROUND
#endif

   SetClassLongPtr(hbwapi_par_raw_HWND(1), GCL_HBRBACKGROUND, reinterpret_cast<HB_PTRUINT>(hbwapi_par_HBRUSH(2)));
}

/*
                             IL  | DL
wvg_AddToolBarButton(hWndTB, nBtn|hBitmap, cCaption, nButtonID, nMode, lIsTooltip)
*/
HB_FUNC( WVG_ADDTOOLBARBUTTON )
{
   TBBUTTON tbb;
   HWND hWndTB = hbwapi_par_raw_HWND(1);

   switch( hb_parni(5) )
   {
      case 1:  /* button from image */
      {
         void * hCaption;
         int iNewString = static_cast<int>(SendMessage(hWndTB, TB_ADDSTRING, 0, reinterpret_cast<LPARAM>(HB_PARSTR(3, &hCaption, nullptr))));  /* set string */
         hb_strfree(hCaption);

         if( hb_parl(6) )
         {
            SendMessage(hWndTB, TB_SETMAXTEXTROWS, 0, 0);
         }

         /* add button */
         tbb.iBitmap   = hb_parni(2);
         tbb.idCommand = hb_parni(4);
         tbb.fsState   = TBSTATE_ENABLED;
         tbb.fsStyle   = TBSTYLE_BUTTON | TBSTYLE_AUTOSIZE;
         tbb.dwData    = 0;
         tbb.iString   = iNewString;

         /* Conversion of LRESULT to HB_BOOL:
            https://msdn.microsoft.com/library/bb787291 */
         hb_retl(static_cast<HB_BOOL>(SendMessage(hWndTB, TB_ADDBUTTONS, static_cast<WPARAM>(1), reinterpret_cast<LPARAM>(static_cast<LPTBBUTTON>(&tbb)))));
         SendMessage(hWndTB, TB_SETPADDING, 0, static_cast<LPARAM>(MAKELPARAM(10, 10)));
         break;
      }
      case 2:  /* system bitmap */

      case 3:  /* separator */
         tbb.iBitmap   = 0;  /* Can be the width of the separator */
         tbb.idCommand = 0;
         tbb.fsState   = TBSTATE_ENABLED;
         tbb.fsStyle   = TBSTYLE_SEP;
         tbb.dwData    = 0;
         tbb.iString   = 0;

         /* Conversion of LRESULT to HB_BOOL:
            https://msdn.microsoft.com/library/bb787291 */
         hb_retl(static_cast<HB_BOOL>(SendMessage(hWndTB, TB_ADDBUTTONS, static_cast<WPARAM>(1), reinterpret_cast<LPARAM>(static_cast<LPTBBUTTON>(&tbb)))));
         break;
   }
}

/*
wvg_RegisterClass_ByName(cClassName)
*/
HB_FUNC( WVG_REGISTERCLASS_BYNAME )
{
   WNDCLASS wndclass;
   void * hClass;

   memset(&wndclass, 0, sizeof(wndclass));

   wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
   wndclass.lpfnWndProc   = DefWindowProc;
   wndclass.hInstance     = GetModuleHandle(nullptr);
   wndclass.hIcon         = nullptr;
   wndclass.hCursor       = LoadCursor(nullptr, IDC_ARROW);
   wndclass.hbrBackground = nullptr;
   wndclass.lpszMenuName  = nullptr;
   wndclass.lpszClassName = HB_PARSTR(1, &hClass, nullptr);

   if( !RegisterClass(&wndclass) && GetLastError() != ERROR_CLASS_ALREADY_EXISTS )
   {
      hb_errInternal(10001, "Failed to register DA window class", nullptr, nullptr);
   }

   hb_strfree(hClass);
}

HB_FUNC( WVG_BEGINMOUSETRACKING )
{
   TRACKMOUSEEVENT tmi;

   tmi.cbSize      = sizeof(tmi);
   tmi.dwFlags     = TME_LEAVE | TME_HOVER;
   tmi.hwndTrack   = hbwapi_par_raw_HWND(1);
   tmi.dwHoverTime = 1;
   hbwapi_ret_L(_TrackMouseEvent(&tmi));
}

LRESULT CALLBACK ControlWindowProcedure(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
   PHB_ITEM pBlock = static_cast<PHB_ITEM>(GetProp(hwnd, TEXT("BLOCKCALLBACK")));

   if( pBlock )
   {
      if( HB_IS_POINTER(pBlock) )
      {
         hb_vmPushSymbol(hb_dynsymSymbol((static_cast<PHB_SYMB>(pBlock))->pDynSym));
         hb_vmPushNil();
      }
      else
      {
         hb_vmPushEvalSym();
         hb_vmPush(pBlock);
      }
      hbwapi_vmPush_HANDLE(hwnd);
      hb_vmPushNumInt(msg);
      hb_vmPushNumInt(static_cast<HB_PTRUINT>(wParam));
      hb_vmPushNumInt(static_cast<HB_PTRUINT>(lParam));
      hb_vmDo(4);

      return static_cast<LRESULT>(hbwapi_par_RESULT(-1));
   }

   return DefWindowProc(hwnd, msg, wParam, lParam);
}

HB_FUNC( WVG_SETWINDOWPROCBLOCK )
{
   HWND hWnd = hbwapi_par_raw_HWND(1);
   PHB_ITEM pBlock = hb_itemNew(hb_param(2, Harbour::Item::EVALITEM));

   SetProp(hWnd, TEXT("BLOCKCALLBACK"), pBlock);

   #ifndef GWL_WNDPROC
   #define GWL_WNDPROC  GWLP_WNDPROC
   #endif

   hbwapi_ret_raw_HANDLE(reinterpret_cast<WNDPROC>(SetWindowLongPtr(hWnd, GWL_WNDPROC, reinterpret_cast<HB_PTRUINT>(ControlWindowProcedure))));
}

HB_FUNC( WVG_RELEASEWINDOWPROCBLOCK )
{
   HWND hWnd = hbwapi_par_raw_HWND(1);
   PHB_ITEM pBlock = static_cast<PHB_ITEM>(RemoveProp(hWnd, TEXT("BLOCKCALLBACK")));

   if( pBlock )
   {
      hb_itemRelease(pBlock);
   }
}

/*
wvg_CreateToolTipWindow(hControl) --> hWndTT
*/
HB_FUNC( WVG_CREATETOOLTIPWINDOW )
{
   HWND hwndTip = CreateWindowEx(0, TOOLTIPS_CLASS, 0,
                                 WS_POPUP | TTS_ALWAYSTIP /* | TTS_BALLOON */,
                                 CW_USEDEFAULT, CW_USEDEFAULT,
                                 CW_USEDEFAULT, CW_USEDEFAULT,
                                 hbwapi_par_raw_HWND(1),
                                 nullptr,
                                 GetModuleHandle(nullptr),
                                 nullptr);

   if( hwndTip )
   {
      TOOLINFO ti;

      memset(&ti, 0, sizeof(ti));

      ti.cbSize   = sizeof(ti);
      ti.hwnd     = hbwapi_par_raw_HWND(1);
      ti.uFlags   = TTF_IDISHWND | TTF_SUBCLASS;
      ti.uId      = reinterpret_cast<UINT_PTR>(ti.hwnd);
      ti.lpszText = const_cast<LPTSTR>(TEXT(""));

      if( !SendMessage(hwndTip, TTM_ADDTOOL, 0, reinterpret_cast<LPARAM>(&ti)) )
      {
         hwndTip = nullptr;
      }
   }

   hbwapi_ret_raw_HANDLE(hwndTip);
}

HB_FUNC( WVG_SETTOOLTIPTEXT )
{
   TOOLINFO ti;
   void * hText;

   memset(&ti, 0, sizeof(ti));

   ti.cbSize   = sizeof(ti);
   ti.hwnd     = hbwapi_par_raw_HWND(1);
   ti.uFlags   = TTF_IDISHWND | TTF_SUBCLASS;
   ti.uId      = reinterpret_cast<UINT_PTR>(ti.hwnd);
   ti.lpszText = static_cast<LPTSTR>(HB_UNCONST(HB_PARSTRDEF(3, &hText, nullptr)));

   SendMessage(hbwapi_par_raw_HWND(2), TTM_SETTOOLINFO, 0, reinterpret_cast<LPARAM>(&ti));

   hb_strfree(hText);
}
