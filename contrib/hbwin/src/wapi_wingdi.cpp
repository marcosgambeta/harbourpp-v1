/*
 * Windows API functions (wingdi.h) (alpha)
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
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

/* WinCE MSDN documentation:
      https://msdn.microsoft.com/library/aa923590
 */

#include "hbwapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* For Arc() */
#include <winspool.h>

static void s_hb_hashSetCItemNL(PHB_ITEM pHash, const char * pszKey, long v)
{
   PHB_ITEM pKey = hb_itemPutC(nullptr, pszKey);
   PHB_ITEM pValue = hb_itemPutNL(nullptr, v);

   hb_hashAdd(pHash, pKey, pValue);

   hb_itemRelease(pValue);
   hb_itemRelease(pKey);
}

POINT * hbwapi_par_POINT( POINT * p, int iParam, HB_BOOL bMandatory )
{
   PHB_ITEM pStru = hb_param(iParam, Harbour::Item::ANY);

   memset(p, 0, sizeof(POINT));

   if( pStru && HB_IS_HASH(pStru) )
   {
      p->x = static_cast<LONG>(hb_itemGetNL(hb_hashGetCItemPtr(pStru, "x")));
      p->y = static_cast<LONG>(hb_itemGetNL(hb_hashGetCItemPtr(pStru, "y")));

      return p;
   }
   else if( pStru && HB_IS_ARRAY(pStru) && hb_arrayLen(pStru) >= 2 )
   {
      p->x = static_cast<LONG>(hb_arrayGetNL(pStru, 1));
      p->y = static_cast<LONG>(hb_arrayGetNL(pStru, 2));

      return p;
   }
   else if( bMandatory )
   {
      return p;
   }

   return nullptr;
}

void hbwapi_stor_POINT( POINT * p, int iParam )
{
   PHB_ITEM pStru = hb_param(iParam, Harbour::Item::ANY);

   if( pStru && HB_IS_HASH(pStru) )
   {
      s_hb_hashSetCItemNL(pStru, "x", p->x);
      s_hb_hashSetCItemNL(pStru, "y", p->y);
   }
   else if( pStru && HB_IS_ARRAY(pStru) && hb_arrayLen(pStru) >= 2 )
   {
      hb_arraySetNL(pStru, 1, p->x);
      hb_arraySetNL(pStru, 2, p->y);
   }
}

RECT * hbwapi_par_RECT( RECT * p, int iParam, HB_BOOL bMandatory )
{
   PHB_ITEM pStru = hb_param(iParam, Harbour::Item::ANY);

   memset(p, 0, sizeof(RECT));

   if( pStru && HB_IS_HASH(pStru) )
   {
      p->left   = static_cast<LONG>(hb_itemGetNL(hb_hashGetCItemPtr(pStru, "left")));
      p->top    = static_cast<LONG>(hb_itemGetNL(hb_hashGetCItemPtr(pStru, "top")));
      p->right  = static_cast<LONG>(hb_itemGetNL(hb_hashGetCItemPtr(pStru, "right")));
      p->bottom = static_cast<LONG>(hb_itemGetNL(hb_hashGetCItemPtr(pStru, "bottom")));

      return p;
   }
   else if( pStru && HB_IS_ARRAY(pStru) && hb_arrayLen(pStru) >= 4 )
   {
      p->left   = static_cast<LONG>(hb_arrayGetNL(pStru, 1));
      p->top    = static_cast<LONG>(hb_arrayGetNL(pStru, 2));
      p->right  = static_cast<LONG>(hb_arrayGetNL(pStru, 3));
      p->bottom = static_cast<LONG>(hb_arrayGetNL(pStru, 4));

      return p;
   }
   else if( bMandatory )
   {
      return p;
   }

   return nullptr;
}

void hbwapi_stor_RECT( RECT * p, int iParam )
{
   PHB_ITEM pStru = hb_param(iParam, Harbour::Item::ANY);

   if( pStru && HB_IS_HASH(pStru) )
   {
      s_hb_hashSetCItemNL(pStru, "left", p->left);
      s_hb_hashSetCItemNL(pStru, "top", p->top);
      s_hb_hashSetCItemNL(pStru, "right", p->right);
      s_hb_hashSetCItemNL(pStru, "bottom", p->bottom);
   }
   else if( pStru && HB_IS_ARRAY(pStru) && hb_arrayLen(pStru) >= 4 )
   {
      hb_arraySetNL(pStru, 1, p->left);
      hb_arraySetNL(pStru, 2, p->top);
      hb_arraySetNL(pStru, 3, p->right);
      hb_arraySetNL(pStru, 4, p->bottom);
   }
}

LOGFONT * hbwapi_par_LOGFONT( LOGFONT * p, int iParam, HB_BOOL bMandatory )
{
   PHB_ITEM pStru = hb_param(iParam, Harbour::Item::ANY);

   void * hfFaceName;
   LPCTSTR pfFaceName;
   HB_SIZE nLen;

   memset(p, 0, sizeof(LOGFONT));

   if( pStru && HB_IS_HASH(pStru) )
   {
      p->lfHeight         = static_cast<LONG>(hb_itemGetNL(hb_hashGetCItemPtr(pStru, "lfHeight")));
      p->lfWidth          = static_cast<LONG>(hb_itemGetNL(hb_hashGetCItemPtr(pStru, "lfWidth")));
      p->lfEscapement     = static_cast<LONG>(hb_itemGetNL(hb_hashGetCItemPtr(pStru, "lfEscapement")));
      p->lfOrientation    = static_cast<LONG>(hb_itemGetNL(hb_hashGetCItemPtr(pStru, "lfOrientation")));
      p->lfWeight         = static_cast<LONG>(hb_itemGetNL(hb_hashGetCItemPtr(pStru, "lfWeight")));
      p->lfItalic         = static_cast<BYTE>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "lfItalic")));
      p->lfUnderline      = static_cast<BYTE>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "lfUnderline")));
      p->lfStrikeOut      = static_cast<BYTE>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "lfStrikeOut")));
      p->lfCharSet        = static_cast<BYTE>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "lfCharSet")));
      p->lfOutPrecision   = static_cast<BYTE>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "lfOutPrecision")));
      p->lfClipPrecision  = static_cast<BYTE>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "lfClipPrecision")));
      p->lfQuality        = static_cast<BYTE>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "lfQuality")));
      p->lfPitchAndFamily = static_cast<BYTE>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "lfPitchAndFamily")));

      pfFaceName = HB_ITEMGETSTR(hb_hashGetCItemPtr(pStru, "lfFaceName"), &hfFaceName, &nLen);

      if( nLen > ( LF_FACESIZE - 1 ) )
      {
         nLen = LF_FACESIZE - 1;
      }

      memcpy(p->lfFaceName, pfFaceName, nLen * sizeof(TCHAR));
      p->lfFaceName[nLen] = TEXT('\0');

      hb_strfree(hfFaceName);

      return p;
   }
   else if( pStru && HB_IS_ARRAY(pStru) && hb_arrayLen(pStru) >= 14 )
   {
      p->lfHeight         = static_cast<LONG>(hb_arrayGetNL(pStru, 1));
      p->lfWidth          = static_cast<LONG>(hb_arrayGetNL(pStru, 2));
      p->lfEscapement     = static_cast<LONG>(hb_arrayGetNL(pStru, 3));
      p->lfOrientation    = static_cast<LONG>(hb_arrayGetNL(pStru, 4));
      p->lfWeight         = static_cast<LONG>(hb_arrayGetNL(pStru, 5));
      p->lfItalic         = static_cast<BYTE>(hb_arrayGetNI(pStru, 6));
      p->lfUnderline      = static_cast<BYTE>(hb_arrayGetNI(pStru, 7));
      p->lfStrikeOut      = static_cast<BYTE>(hb_arrayGetNI(pStru, 8));
      p->lfCharSet        = static_cast<BYTE>(hb_arrayGetNI(pStru, 9));
      p->lfOutPrecision   = static_cast<BYTE>(hb_arrayGetNI(pStru, 10));
      p->lfClipPrecision  = static_cast<BYTE>(hb_arrayGetNI(pStru, 11));
      p->lfQuality        = static_cast<BYTE>(hb_arrayGetNI(pStru, 12));
      p->lfPitchAndFamily = static_cast<BYTE>(hb_arrayGetNI(pStru, 13));

      pfFaceName = HB_ARRAYGETSTR(pStru, 14, &hfFaceName, &nLen);

      if( nLen > (LF_FACESIZE - 1) )
      {
         nLen = LF_FACESIZE - 1;
      }

      memcpy(p->lfFaceName, pfFaceName, nLen * sizeof(TCHAR));
      p->lfFaceName[nLen] = TEXT('\0');

      hb_strfree(hfFaceName);

      return p;
   }
   else if( bMandatory )
   {
      return p;
   }

   return nullptr;
}

DOCINFO * hbwapi_par_DOCINFO( DOCINFO * p, int iParam, HB_BOOL bMandatory, void *** ph )
{
   PHB_ITEM pStru = hb_param(iParam, Harbour::Item::ANY);
   void ** h = static_cast<void**>(hb_xgrabz(3 * sizeof(void*)));

   *ph = h;

   memset(p, 0, sizeof(DOCINFO));

   p->cbSize = sizeof(DOCINFO);

   if( pStru && HB_IS_HASH(pStru) )
   {
      p->lpszDocName  = HB_ITEMGETSTR(hb_hashGetCItemPtr(pStru, "lpszDocName"), &h[0], nullptr);
      p->lpszOutput   = HB_ITEMGETSTR(hb_hashGetCItemPtr(pStru, "lpszOutput"), &h[1], nullptr);
      p->lpszDatatype = HB_ITEMGETSTR(hb_hashGetCItemPtr(pStru, "lpszDatatype"), &h[2], nullptr);
      p->fwType       = static_cast<DWORD>(hb_itemGetNL(hb_hashGetCItemPtr(pStru, "fwType")));

      return p;
   }
   else if( bMandatory )
   {
      return p;
   }

   hb_xfree(h);
   *ph = nullptr;

   return nullptr;
}

void hbwapi_strfree_DOCINFO( void ** h )
{
   if( h )
   {
      for( int i = 0; i < 3; ++i )
      {
         hb_strfree(h[i]);
      }
   }
}

HB_FUNC( __WAPI_DEVMODE_NEW )
{
   HANDLE hPrinter;
   void * hDeviceName;
   LPCTSTR lpDeviceName = HB_PARSTR(1, &hDeviceName, nullptr);

   hb_retptr(nullptr);

   if( OpenPrinter(const_cast<LPTSTR>(lpDeviceName), &hPrinter, nullptr) )
   {
      LONG lSize = DocumentProperties(0, hPrinter, const_cast<LPTSTR>(lpDeviceName), nullptr, nullptr, 0);

      if( lSize > 0 )
      {
         PDEVMODE pDevMode = static_cast<PDEVMODE>(hb_xgrabz(lSize));

         if( DocumentProperties(0, hPrinter, const_cast<LPTSTR>(lpDeviceName), pDevMode, pDevMode, DM_OUT_BUFFER) == IDOK )
         {
            hbwapi_ret_PDEVMODE(pDevMode);
         }
         else
         {
            hb_xfree(pDevMode);
         }
      }

      ClosePrinter(hPrinter);
   }

   hb_strfree(hDeviceName);
}

HB_FUNC( __WAPI_DEVMODE_SET )
{
   PDEVMODE pDevMode = hbwapi_par_PDEVMODE(1);
   PHB_ITEM pStru = hb_param(2, Harbour::Item::ANY);

   if( pDevMode && pStru && HB_IS_HASH(pStru) )
   {
      pDevMode->dmOrientation   = static_cast<short>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "dmOrientation")));
      pDevMode->dmPaperSize     = static_cast<short>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "dmPaperSize")));
      pDevMode->dmPaperLength   = static_cast<short>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "dmPaperLength")));
      pDevMode->dmPaperWidth    = static_cast<short>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "dmPaperWidth")));
      pDevMode->dmScale         = static_cast<short>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "dmScale")));
      pDevMode->dmCopies        = static_cast<short>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "dmCopies")));
      pDevMode->dmDefaultSource = static_cast<short>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "dmDefaultSource")));
      pDevMode->dmPrintQuality  = static_cast<short>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "dmPrintQuality")));
      pDevMode->dmDuplex        = static_cast<short>(hb_itemGetNI(hb_hashGetCItemPtr(pStru, "dmDuplex")));

      pDevMode->dmFields = 0;
      if( hb_hashGetCItemPtr(pStru, "dmOrientation"  ) ) pDevMode->dmFields |= DM_ORIENTATION;
      if( hb_hashGetCItemPtr(pStru, "dmPaperSize"    ) ) pDevMode->dmFields |= DM_PAPERSIZE;
      if( hb_hashGetCItemPtr(pStru, "dmPaperLength"  ) ) pDevMode->dmFields |= DM_PAPERLENGTH;
      if( hb_hashGetCItemPtr(pStru, "dmPaperWidth"   ) ) pDevMode->dmFields |= DM_PAPERWIDTH;
      if( hb_hashGetCItemPtr(pStru, "dmScale"        ) ) pDevMode->dmFields |= DM_SCALE;
      if( hb_hashGetCItemPtr(pStru, "dmCopies"       ) ) pDevMode->dmFields |= DM_COPIES;
      if( hb_hashGetCItemPtr(pStru, "dmDefaultSource") ) pDevMode->dmFields |= DM_DEFAULTSOURCE;
      if( hb_hashGetCItemPtr(pStru, "dmPrintQuality" ) ) pDevMode->dmFields |= DM_PRINTQUALITY;
      if( hb_hashGetCItemPtr(pStru, "dmDuplex"       ) ) pDevMode->dmFields |= DM_DUPLEX;
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( __WAPI_DEVMODE_GET )
{
   PDEVMODE pDevMode = hbwapi_par_PDEVMODE(1);
   PHB_ITEM pStru = hb_param(2, Harbour::Item::ANY);

   if( pDevMode && pStru && HB_IS_HASH(pStru) )
   {
      s_hb_hashSetCItemNL(pStru, "dmOrientation", pDevMode->dmOrientation);
      s_hb_hashSetCItemNL(pStru, "dmPaperSize", pDevMode->dmPaperSize);
      s_hb_hashSetCItemNL(pStru, "dmPaperLength", pDevMode->dmPaperLength);
      s_hb_hashSetCItemNL(pStru, "dmPaperWidth", pDevMode->dmPaperWidth);
      s_hb_hashSetCItemNL(pStru, "dmScale", pDevMode->dmScale);
      s_hb_hashSetCItemNL(pStru, "dmCopies", pDevMode->dmCopies);
      s_hb_hashSetCItemNL(pStru, "dmDefaultSource", pDevMode->dmDefaultSource);
      s_hb_hashSetCItemNL(pStru, "dmPrintQuality", pDevMode->dmPrintQuality);
      s_hb_hashSetCItemNL(pStru, "dmDuplex", pDevMode->dmDuplex);
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_CREATEDC )
{
   void * hDriver;
   void * hDevice;
   void * hOutput;

   hbwapi_ret_HDC(CreateDC(HB_PARSTRDEF(1, &hDriver, nullptr), HB_PARSTRDEF(2, &hDevice, nullptr), HB_PARSTR(3, &hOutput, nullptr), hbwapi_par_PDEVMODE(4)));

   hb_strfree(hDriver);
   hb_strfree(hDevice);
   hb_strfree(hOutput);
}

HB_FUNC( WAPI_RESETDC )
{
   HDC hDC = hbwapi_par_HDC(1);
   PDEVMODE pDEVMODE = hbwapi_par_PDEVMODE(2);

   if( hDC )
   {
      hb_retl(ResetDC(hDC, pDEVMODE) == hDC);
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_STARTDOC )
{
   HDC hDC = hbwapi_par_HDC(1);
   void ** hDOCINFO = nullptr;
   DOCINFO di;

   if( hDC && hbwapi_par_DOCINFO( &di, 2, false, &hDOCINFO ) )
   {
      hb_retni(StartDoc(hDC, &di));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }

   hbwapi_strfree_DOCINFO(hDOCINFO);
}

HB_FUNC( WAPI_ENDDOC )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retni(EndDoc(hDC));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_ABORTDOC )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retni(AbortDoc(hDC));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_STARTPAGE )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retni(StartPage(hDC));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_ENDPAGE )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retni(EndPage(hDC));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_SETBKMODE )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retni(SetBkMode(hDC, hb_parni(2)));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_GETBKMODE )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retni(GetBkMode(hDC));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_GETDEVICECAPS )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retni(GetDeviceCaps(hDC, hb_parni(2)));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_SETMAPMODE )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retni(SetMapMode(hDC, hb_parni(2)));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_GETMAPMODE )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retni(GetMapMode(hDC));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_SETTEXTALIGN )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retni(static_cast<int>(SetTextAlign(hDC, static_cast<UINT>(hb_parni(2)))));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_GETTEXTALIGN )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retni(static_cast<int>(GetTextAlign(hDC)));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_TEXTOUT )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      void * hData;
      HB_SIZE nDataLen;
      LPCTSTR lpData = HB_PARSTR(4, &hData, &nDataLen);

      hb_retl(TextOut(hDC,
                      hb_parni(2) /* iRow */,
                      hb_parni(3) /* iCol */,
                      lpData,
                      static_cast<int>(nDataLen)));

      hb_strfree(hData);
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }

   hb_retnl(false);
}

HB_FUNC( WAPI_EXTTEXTOUT )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      void * hData;
      HB_SIZE nDataLen;
      LPCTSTR lpData = HB_PARSTR(6, &hData, &nDataLen);
      RECT rect;
      PHB_ITEM pFontWidths = hb_param(7, Harbour::Item::ARRAY);
      INT * lpFontWidths;

      if( pFontWidths )
      {
         HB_SIZE nFontWidthsLen = hb_arrayLen(pFontWidths);
         INT iWidth = 0;

         lpFontWidths = static_cast<INT*>(hb_xgrab(nDataLen * sizeof(INT)));

         for( HB_SIZE tmp = 0; tmp < nDataLen; ++tmp )
         {
            /* Pad width array with last known value if passed array was smaller than length of the string. */
            if( tmp < nFontWidthsLen )
            {
               iWidth = static_cast<INT>(hb_arrayGetNI(pFontWidths, tmp + 1));
            }

            lpFontWidths[tmp] = iWidth;
         }
      }
      else
      {
         lpFontWidths = nullptr;
      }

      hb_retl(ExtTextOut(hDC,
                         hb_parni(2) /* iRow */,
                         hb_parni(3) /* iCol */,
                         static_cast<UINT>(hb_parni(4)) /* fuOptions */,
                         hbwapi_par_RECT(&rect, 5, false),
                         lpData,
                         static_cast<UINT>(nDataLen),
                         lpFontWidths));

      if( lpFontWidths )
      {
         hb_xfree(lpFontWidths);
      }

      hb_strfree(hData);
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_SETTEXTCOLOR )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retnl(static_cast<long>(SetTextColor(hDC, static_cast<COLORREF>(hb_parnl(2)))));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_GETTEXTCOLOR )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retnl(static_cast<long>(GetTextColor(hDC)));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_GETTEXTFACE )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      TCHAR tszFontName[128];

      GetTextFace(hDC, HB_SIZEOFARRAY(tszFontName) - 1, tszFontName);

      HB_RETSTR(tszFontName);
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_SETBKCOLOR )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retnl(static_cast<long>(SetBkColor(hDC, static_cast<COLORREF>(hb_parnl(2)))));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_GETBKCOLOR )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retnl(static_cast<long>(GetBkColor(hDC)));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_CREATEPEN )
{
   hbwapi_ret_HPEN(CreatePen(hb_parni(1) /* fnPenStyle */, hb_parni(2) /* nWidth */, static_cast<COLORREF>(hb_parnl(3)) /* crColor */));
}

HB_FUNC( WAPI_CREATESOLIDBRUSH )
{
   HBRUSH h = CreateSolidBrush(static_cast<COLORREF>(hb_parnl(1)) /* crColor */);

   hbwapi_ret_HBRUSH(h);
}

HB_FUNC( WAPI_CREATEHATCHBRUSH )
{
   hbwapi_ret_HBRUSH(CreateHatchBrush(hb_parni(1) /* fnStyle */, static_cast<COLORREF>(hb_parnl(2)) /* crColor */));
}

HB_FUNC( WAPI_CREATEFONT )
{
   void * hFontFace;

   hbwapi_ret_HFONT(CreateFont(hb_parni(1) /* nHeight */,
                               hb_parni(2) /* nWidth */,
                               hb_parni(3) /* nEscapement */,
                               hb_parni(4) /* nOrientation */,
                               hb_parni(5) /* fnWeight */,
                               static_cast<DWORD>(hb_parl(6)) /* fdwItalic */,
                               static_cast<DWORD>(hb_parl(7)) /* fdwUnderline */,
                               static_cast<DWORD>(hb_parl(8)) /* fdwStrikeOut */,
                               static_cast<DWORD>(hb_parnl(9)) /* fdwCharSet */,
                               static_cast<DWORD>(hb_parnldef(10, OUT_DEFAULT_PRECIS)) /* fdwOutputPrecision */,
                               static_cast<DWORD>(hb_parnldef(11, CLIP_DEFAULT_PRECIS)) /* fdwClipPrecision */,
                               static_cast<DWORD>(hb_parnldef(12, DEFAULT_QUALITY)) /* fdwQuality */,
                               static_cast<DWORD>(hb_parnldef(13, DEFAULT_PITCH | FF_DONTCARE)) /* fdwPitchAndFamily */,
                               HB_PARSTR(14, &hFontFace, nullptr) /* lpszFace */));

   hb_strfree(hFontFace);
}

HB_FUNC( WAPI_CREATEFONTINDIRECT )
{
   LOGFONT p;

   if( hbwapi_par_LOGFONT( &p, 1, true ) )
   {
      hbwapi_ret_HFONT(CreateFontIndirect(&p));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_SELECTOBJECT )
{
   HDC hDC = hbwapi_par_HDC(1);
   HB_BOOL bRegion = HB_FALSE;
   HGDIOBJ h;

   if( (h = hbwapi_par_HPEN(2)) != nullptr )
   {
   }
   else if( (h = hbwapi_par_HBRUSH(2)) != nullptr )
   {
   }
   else if( (h = hbwapi_par_HFONT(2)) != nullptr )
   {
   }
   /* TODO: Add BITMAP, REGION */
   else
   {
      h = nullptr;
   }

   if( hDC && h )
   {
      /* TODO: Solve reference counting to 'h' handle. Also for returned one. */
      if( bRegion )
      {
         hb_retnint(reinterpret_cast<HB_PTRUINT>(SelectObject(hDC, h)));
      }
      else
      {
         hb_retl(SelectObject(hDC, h) != nullptr);  /* NOTE: We don't return a raw pointer. */
      }
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_MOVETOEX )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      POINT p;

      if( hbwapi_par_POINT( &p, 4, false ) )
      {
         hb_retl(MoveToEx(hDC, hb_parni(2) /* X */, hb_parni(3) /* Y */, &p));

         hbwapi_stor_POINT(&p, 4);
      }
      else
      {
         hb_retl(MoveToEx(hDC, hb_parni(2) /* X */, hb_parni(3) /* Y */, nullptr));
      }
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_LINETO )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retl(LineTo(hDC, hb_parni(2) /* XEnd */, hb_parni(3) /* YEnd */));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_FILLRECT )
{
   HDC hDC = hbwapi_par_HDC(1);
   RECT rect;
   HBRUSH hBrush = hbwapi_par_HBRUSH(3);

   if( hDC && hbwapi_par_RECT( &rect, 2, true ) && hBrush )
   {
      hb_retni(FillRect(hDC, &rect, hBrush));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_ROUNDRECT )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retl(RoundRect(hDC,
                        hb_parni(2) /* x1 */,
                        hb_parni(3) /* y1 */,
                        hb_parni(4) /* x2 */,
                        hb_parni(5) /* y2 */,
                        hb_parni(6) /* iWidth */,
                        hb_parni(7) /* iHeight */));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_RECTANGLE )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retl(Rectangle(hDC,
                        hb_parni(2) /* x1 */,
                        hb_parni(3) /* y1 */,
                        hb_parni(4) /* x2 */,
                        hb_parni(5) /* y2 */));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_ARC )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retl(Arc(hDC,
                  hb_parni(2) /* nLeftRect */,
                  hb_parni(3) /* nTopRect */,
                  hb_parni(4) /* nRightRect */,
                  hb_parni(5) /* nBottomRect */,
                  hb_parni(6) /* nXStartArc */,
                  hb_parni(7) /* nYStartArc */,
                  hb_parni(8) /* nXEndArc */,
                  hb_parni(9) /* nYEndArc */));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( WAPI_ELLIPSE )
{
   HDC hDC = hbwapi_par_HDC(1);

   if( hDC )
   {
      hb_retl(Ellipse(hDC,
                      hb_parni(2) /* nLeftRect */,
                      hb_parni(3) /* nTopRect */,
                      hb_parni(4) /* nRightRect */,
                      hb_parni(5) /* nBottomRect */));
   }
   else
   {
      hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}
