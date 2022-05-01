/*
 * The Terminal API
 *
 * Copyright 1999 Bil Simser <bsimser@home.com>
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca>
 *    hb_gtInit(), hb_gtExit()
 *    hb_gtDispBegin(), hb_gtDispEnd()
 *    hb_gtPreExt(), hb_gtPostExt()
 *    hb_gtGetColorStr(), hb_gtSetColorStr(), hb_gtSetMode()
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
 *    hb_gtDrawShadow()
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
 *    The body of these functions which were usable in new GT API
 *    have been moved to hbgtcore.c to hb_gt_def_*() functions
 *    some of my modifications.
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

#include "hbgtcore.h"
#include "hbset.h"

/* gt API functions */

HB_ERRCODE hb_gtInit( HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtInit()" ) );
#endif

   PHB_GT pGT;

   hb_gtStartupInit();

   pGT = hb_gt_Base();
   if( !pGT )
   {
      return HB_FAILURE;
   }

   HB_GTSELF_INIT(pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr);
   HB_GTSELF_SETCOLORSTR(pGT, hb_setGetColor());
   HB_GTSELF_SETCURSORSTYLE(pGT, SC_NORMAL);
   HB_GTSELF_FLUSH(pGT);
   hb_gt_BaseFree(pGT);

   return HB_SUCCESS;
}

HB_ERRCODE hb_gtExit(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtExit()" ) );
#endif

   hb_gtRelease(nullptr);

   /* clear internal clipboard data */
   hb_gt_setClipboard(nullptr, 0);

   return HB_SUCCESS;
}

HB_ERRCODE hb_gtLock(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtLock()" ) );
#endif

   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_LOCK(pGT) )
      {
         errCode = HB_SUCCESS;
      }
      hb_gt_BaseFree(pGT);
   }
   return errCode;
}

HB_ERRCODE hb_gtUnlock(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtUnlock()" ) );
#endif

   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_UNLOCK(pGT);
      errCode = HB_SUCCESS;
      hb_gt_BaseFree(pGT);
   }
   return errCode;
}

int hb_gtReadKey( int iEventMask )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtReadKey(%d)", iEventMask ) );
#endif

   int iKey = 0;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      iKey = HB_GTSELF_READKEY(pGT, iEventMask);
      hb_gt_BaseFree(pGT);
   }
   return iKey;
}

HB_ERRCODE hb_gtBox( int iTop, int iLeft, int iBottom, int iRight, const char * szFrame )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtBox(%d, %d, %d, %d, %p)", iTop, iLeft, iBottom, iRight, static_cast<const void*>(szFrame) ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_BOX(pGT, iTop, iLeft, iBottom, iRight, szFrame, HB_GTSELF_GETCOLOR(pGT));
      HB_GTSELF_SETPOS(pGT, iTop + 1, iLeft + 1);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtBoxEx( int iTop, int iLeft, int iBottom, int iRight, const char * szFrame, int iColor )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtBoxEx(%d, %d, %d, %d, %p, %d)", iTop, iLeft, iBottom, iRight, static_cast<const void*>(szFrame), iColor ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( iColor == -1 )
      {
         iColor = HB_GTSELF_GETCOLOR(pGT);
      }
      HB_GTSELF_BOX(pGT, iTop, iLeft, iBottom, iRight, szFrame, iColor);
      HB_GTSELF_SETPOS(pGT, iTop + 1, iLeft + 1);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtBoxD( int iTop, int iLeft, int iBottom, int iRight )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtBoxD(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_BOXD(pGT, iTop, iLeft, iBottom, iRight, nullptr, HB_GTSELF_GETCOLOR(pGT));
      HB_GTSELF_SETPOS(pGT, iTop + 1, iLeft + 1);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtBoxS( int iTop, int iLeft, int iBottom, int iRight )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtBoxS(%d, %d, %d, %d)", iTop, iLeft, iBottom, iRight ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_BOXS(pGT, iTop, iLeft, iBottom, iRight, nullptr, HB_GTSELF_GETCOLOR(pGT));
      HB_GTSELF_SETPOS(pGT, iTop + 1, iLeft + 1);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtDrawBox( int iTop, int iLeft, int iBottom, int iRight, const char * szFrame, int iColor )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtDrawBox(%d, %d, %d, %d, %p, %d)", iTop, iLeft, iBottom, iRight, static_cast<const void*>(szFrame), iColor ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( iColor == -1 )
      {
         iColor = HB_GTSELF_GETCOLOR(pGT);
      }

      HB_GTSELF_BOX(pGT, iTop, iLeft, iBottom, iRight, szFrame, iColor);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtColorSelect( int iColorIndex )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtColorSelect(%d)", iColorIndex ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_COLORSELECT(pGT, iColorIndex);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtDispBegin(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtDispBegin()" ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_DISPBEGIN(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

int hb_gtDispCount(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtDispCount()" ) );
#endif

   int    iCount = 0;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      iCount = HB_GTSELF_DISPCOUNT(pGT);
      hb_gt_BaseFree(pGT);
   }
   return iCount;
}

HB_ERRCODE hb_gtDispEnd(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtDispEnd()" ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_DISPEND(pGT);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtPreExt(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtPreExt()" ) );
#endif

   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_PREEXT(pGT) )
      {
         errCode = HB_SUCCESS;
      }
      hb_gt_BaseFree(pGT);
   }
   return errCode;
}

HB_ERRCODE hb_gtPostExt(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtPostExt()" ) );
#endif

   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_POSTEXT(pGT) )
      {
         errCode = HB_SUCCESS;
      }
      hb_gt_BaseFree(pGT);
   }
   return errCode;
}

/* NOTE: szColorString must be at least HB_CLRSTR_LEN wide by the NG. It seems
         that CA-Cl*pper SetColor() will return string lengths up to 131+EOF.
         That seems like a 127+1 buffer size, plus lazy overflow checking.
         [vszakats] */

HB_ERRCODE hb_gtGetColorStr( char * pszColorString )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtGetColorStr(%s)", pszColorString ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_GETCOLORSTR(pGT, pszColorString);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   pszColorString[0] = '\0';
   return HB_FAILURE;
}

int hb_gtColorToN( const char * szColorString )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtColorToN(%s)", szColorString ) );
#endif

   int iColor = 0;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      iColor = HB_GTSELF_COLORNUM(pGT, szColorString);
      hb_gt_BaseFree(pGT);
   }
   return iColor;
}

HB_ERRCODE hb_gtColorsToString(int * pColors, int iColorCount, char * pszColorString, int iBufSize)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtColorsToString(%p, %d, %p, %d)", static_cast<void*>(pColors), iColorCount, static_cast<void*>(pszColorString), iBufSize ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_COLORSTOSTRING(pGT, pColors, iColorCount, pszColorString, iBufSize);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   pszColorString[0] = '\0';
   return HB_FAILURE;
}

HB_ERRCODE hb_gtSetColorStr( const char * szColorString )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtSetColorStr(%s)", szColorString ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SETCOLORSTR(pGT, szColorString);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtGetCursor( int * piCursorStyle )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtGetCursor(%p)", static_cast<void*>(piCursorStyle) ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      *piCursorStyle = HB_GTSELF_GETCURSORSTYLE(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   *piCursorStyle = SC_NONE;
   return HB_FAILURE;
}

HB_ERRCODE hb_gtSetCursor( int iCursorStyle )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtSetCursor(%d)", iCursorStyle ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SETCURSORSTYLE(pGT, iCursorStyle);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtGetPos(int * piRow, int * piCol)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtGetPos(%p, %p)", static_cast<void*>(piRow), static_cast<void*>(piCol) ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_GETPOS(pGT, piRow, piCol);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   *piRow = *piCol = 0;
   return HB_FAILURE;
}

HB_ERRCODE hb_gtGetPosEx( int * piRow, int * piCol )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtGetPosEx(%p, %p)", static_cast<void*>(piRow), static_cast<void*>(piCol) ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_GETPOS(pGT, piRow, piCol);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   *piRow = *piCol = 0;
   return HB_FAILURE;
}

/* NOTE: Should be exactly the same as hb_gtSetPosContext(), but without the
         additional third parameter. */

HB_ERRCODE hb_gtSetPos(int iRow, int iCol)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtSetPos(%d, %d)", iRow, iCol ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SETPOS(pGT, iRow, iCol);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

int hb_gtMaxCol(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtMaxCol()" ) );
#endif

   PHB_GT pGT;
   int iMaxCol;

   pGT = hb_gt_Base();
   if( pGT )
   {
      iMaxCol = HB_GTSELF_MAXCOL(pGT);
      hb_gt_BaseFree(pGT);
   }
   else
   {
      iMaxCol = 79;
   }

   return iMaxCol;
}

int hb_gtMaxRow(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtMaxRow()" ) );
#endif

   PHB_GT pGT;
   int iMaxRow;

   pGT = hb_gt_Base();
   if( pGT )
   {
      iMaxRow = HB_GTSELF_MAXROW(pGT);
      hb_gt_BaseFree(pGT);
   }
   else
   {
      iMaxRow = 24;
   }

   return iMaxRow;
}

HB_ERRCODE hb_gtScrDim( int * piHeight, int * piWidth )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtScrDim(%p, %p)", static_cast<void*>(piHeight), static_cast<void*>(piWidth) ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_GETSIZE(pGT, piHeight, piWidth);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   *piHeight = *piWidth = 0;
   return HB_FAILURE;
}

HB_ERRCODE hb_gtSetSnowFlag( HB_BOOL fNoSnow )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtSetSnowFlag(%d)", static_cast<int>(fNoSnow) ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SETSNOWFLAG(pGT, fNoSnow);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtRectSize(int iTop, int iLeft, int iBottom, int iRight, HB_SIZE * pulBuffSize)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtRectSize(%d, %d, %d, %d, %p)", iTop, iLeft, iBottom, iRight, static_cast<void*>(pulBuffSize) ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      *pulBuffSize = HB_GTSELF_RECTSIZE(pGT, iTop, iLeft, iBottom, iRight);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   *pulBuffSize = 0;
   return HB_FAILURE;
}

HB_BOOL hb_gtIsColor(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtIsColor()" ) );
#endif

   HB_BOOL fColor = HB_TRUE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      fColor = HB_GTSELF_ISCOLOR(pGT);
      hb_gt_BaseFree(pGT);
   }
   return fColor;
}

HB_ERRCODE hb_gtRepChar( int iRow, int iCol, HB_USHORT usChar, HB_SIZE nCount )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtRepChar(%d, %d, %hu, %" HB_PFS "u)", iRow, iCol, usChar, nCount ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_REPLICATE(pGT, iRow, iCol, HB_GTSELF_GETCOLOR(pGT), 0, usChar, nCount);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtSave( int iTop, int iLeft, int iBottom, int iRight, void * pScrBuff )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtSave(%d, %d, %d, %d, %p)", iTop, iLeft, iBottom, iRight, pScrBuff ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SAVE(pGT, iTop, iLeft, iBottom, iRight, pScrBuff);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtRest( int iTop, int iLeft, int iBottom, int iRight, const void * pScrBuff )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtRest(%d, %d, %d, %d, %p)", iTop, iLeft, iBottom, iRight, pScrBuff ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_REST(pGT, iTop, iLeft, iBottom, iRight, pScrBuff);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtGetChar( int iRow, int iCol, int * piColor, HB_BYTE * pbAttr, HB_USHORT * pusChar )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtGetChar(%d, %d, %p, %p, %p)", iRow, iCol, static_cast<void*>(piColor), static_cast<void*>(pbAttr), static_cast<void*>(pusChar) ) );
#endif

   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_GETCHAR(pGT, iRow, iCol, piColor, pbAttr, pusChar) )
      {
         errCode = HB_SUCCESS;
      }
      hb_gt_BaseFree(pGT);
   }
   return errCode;
}

HB_ERRCODE hb_gtPutChar( int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtPutChar(%d, %d, %d, %u, %hu)", iRow, iCol, iColor, bAttr, usChar ) );
#endif

   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_PUTCHAR(pGT, iRow, iCol, iColor, bAttr, usChar) )
      {
         errCode = HB_SUCCESS;
      }
      hb_gt_BaseFree(pGT);
   }
   return errCode;
}

HB_ERRCODE hb_gtBeginWrite(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtBeginWrite()" ) );
#endif

   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_LOCK(pGT) )
      {
         errCode = HB_SUCCESS;
      }
      hb_gt_BaseFree(pGT);
   }

   return errCode;
}

HB_ERRCODE hb_gtEndWrite(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtEndWrite()" ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_FLUSH(pGT);
      HB_GTSELF_UNLOCK(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtGetBlink( HB_BOOL * bpBlink )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtGetBlink(%p)", static_cast<void*>(bpBlink) ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      *bpBlink = HB_GTSELF_GETBLINK(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   *bpBlink = 0;
   return HB_FAILURE;
}

HB_ERRCODE hb_gtSetBlink( HB_BOOL fBlink )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtSetBlink(%d)", static_cast<int>(fBlink) ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SETBLINK(pGT, fBlink);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtSetMode( int iRows, int iCols )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtSetMode(%d, %d)", iRows, iCols ) );
#endif

   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_SETMODE(pGT, iRows, iCols) )
      {
         errCode = HB_SUCCESS;
      }
      hb_gt_BaseFree(pGT);
   }
   return errCode;
}

HB_ERRCODE hb_gtPutText( int iRow, int iCol, const char * szStr, HB_SIZE nLength, int iColor )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtPutText(%d, %d, %p, %" HB_PFS "u, %d)", iRow, iCol, static_cast<const void*>(szStr), nLength, iColor ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( iColor == -1 )
      {
         iColor = HB_GTSELF_GETCOLOR(pGT);
      }

      HB_GTSELF_PUTTEXT(pGT, iRow, iCol, iColor, szStr, nLength);
      HB_GTSELF_FLUSH(pGT);

      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtWriteAt(int iRow, int iCol, const char * szStr, HB_SIZE nLength)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtWriteAt(%d, %d, %p, %" HB_PFS "u)", iRow, iCol, static_cast<const void*>(szStr), nLength ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_WRITEAT(pGT, iRow, iCol, szStr, nLength);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtWrite(const char * szStr, HB_SIZE nLength)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtWrite(%p, %" HB_PFS "u)", static_cast<const void*>(szStr), nLength ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_WRITE(pGT, szStr, nLength);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtWriteCon( const char * szStr, HB_SIZE nLength )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtWriteCon(%p, %" HB_PFS "u)", static_cast<const void*>(szStr), nLength ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_WRITECON(pGT, szStr, nLength);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtScroll( int iTop, int iLeft, int iBottom, int iRight, int iRows, int iCols )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtScroll(%d, %d, %d, %d, %d, %d)", iTop, iLeft, iBottom, iRight, iRows, iCols ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SCROLL(pGT, iTop, iLeft, iBottom, iRight, HB_GTSELF_GETCOLOR(pGT), ' ', iRows, iCols);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtScrollEx( int iTop, int iLeft, int iBottom, int iRight, int iColor, int iChar, int iRows, int iCols )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtScrollEx(%d, %d, %d, %d, %d, %d, %d, %d)", iTop, iLeft, iBottom, iRight, iColor, iChar, iRows, iCols ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( iColor == -1 )
      {
         iColor = HB_GTSELF_GETCOLOR(pGT);
      }
      if( iChar < 0 )
      {
         iChar = HB_GTSELF_GETCLEARCHAR(pGT);
      }
      HB_GTSELF_SCROLL(pGT, iTop, iLeft, iBottom, iRight, iColor, static_cast<HB_USHORT>(iChar), iRows, iCols);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }

   return HB_FAILURE;
}

HB_ERRCODE hb_gtScrollUp( int iRows )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtScrollUp(%d)", iRows ) );
#endif

   if( iRows != 0 )
   {
      PHB_GT pGT = hb_gt_Base();
      if( pGT )
      {
         HB_GTSELF_SCROLLUP(pGT, iRows, HB_GTSELF_GETCOLOR(pGT), ' ');
         HB_GTSELF_FLUSH(pGT);
         hb_gt_BaseFree(pGT);
         return HB_SUCCESS;
      }
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtDrawShadow( int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtDrawShadow(%d, %d, %d, %d, %d)", iTop, iLeft, iBottom, iRight, iColor ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_DRAWSHADOW(pGT, iTop, iLeft, iBottom, iRight, iColor);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtTone( double dFrequency, double dDuration )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtTone(%lf, %lf)", dFrequency, dDuration ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_TONE(pGT, dFrequency, dDuration);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

const char * hb_gtVersion( int iType )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtVersion(%d)", iType ) );
#endif

   const char * szVersion = "";
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      szVersion = HB_GTSELF_VERSION(pGT, iType);
      hb_gt_BaseFree(pGT);
   }
   return szVersion;
}

HB_ERRCODE hb_gtSetAttribute( int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtSetAttribute(%d, %d, %d, %d, %d)", iTop, iLeft, iBottom, iRight, iColor ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SETATTRIBUTE(pGT, iTop, iLeft, iBottom, iRight, iColor);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

/* prepare the terminal for system call */
HB_ERRCODE hb_gtSuspend(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtSuspend()" ) );
#endif

   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_SUSPEND(pGT) )
      {
         errCode = HB_SUCCESS;
      }
      hb_gt_BaseFree(pGT);
   }
   return errCode;
}

HB_ERRCODE hb_gtResume(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtResume()" ) );
#endif

   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_RESUME(pGT) )
      {
         errCode = HB_SUCCESS;
      }
      hb_gt_BaseFree(pGT);
   }
   return errCode;
}

HB_ERRCODE hb_gtOutStd( const char * szStr, HB_SIZE nLen )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtOutStd(%p, %" HB_PFS "u)", static_cast<const void*>(szStr), nLen ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_OUTSTD(pGT, szStr, nLen);
      hb_gt_BaseFree(pGT);
   }
   else
   {
      hb_fsWriteLarge(static_cast<HB_FHANDLE>(HB_STDOUT_HANDLE), szStr, nLen);
   }

   return HB_SUCCESS;
}

HB_ERRCODE hb_gtOutErr( const char * szStr, HB_SIZE nLen )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtOutErr(%p, %" HB_PFS "u)", static_cast<const void*>(szStr), nLen ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_OUTERR(pGT, szStr, nLen);
      hb_gt_BaseFree(pGT);
   }
   else
   {
      hb_fsWriteLarge(static_cast<HB_FHANDLE>(HB_STDERR_HANDLE), szStr, nLen);
   }

   return HB_SUCCESS;
}

HB_ERRCODE hb_gtSetDispCP( const char * pszTermCDP, const char * pszHostCDP, HB_BOOL fBox )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtSetDispCP(%s, %s, %d)", pszTermCDP, pszHostCDP, fBox ) );
#endif

   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_SETDISPCP(pGT, pszTermCDP, pszHostCDP, fBox) )
      {
         errCode = HB_SUCCESS;
      }
      hb_gt_BaseFree(pGT);
   }
   return errCode;
}

HB_ERRCODE hb_gtSetKeyCP( const char * pszTermCDP, const char * pszHostCDP )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtSetKeyCP(%s, %s)", pszTermCDP, pszHostCDP ) );
#endif

   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_SETKEYCP(pGT, pszTermCDP, pszHostCDP) )
      {
         errCode = HB_SUCCESS;
      }
      hb_gt_BaseFree(pGT);
   }
   return errCode;
}

PHB_CODEPAGE hb_gtHostCP(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtHostCP()" ) );
#endif

   PHB_CODEPAGE cdp = nullptr;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      cdp = HB_GTSELF_HOSTCP(pGT);
      hb_gt_BaseFree(pGT);
   }
   return cdp;
}

PHB_CODEPAGE hb_gtBoxCP(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtBoxCP()" ) );
#endif

   PHB_CODEPAGE cdp = nullptr;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      cdp = HB_GTSELF_BOXCP(pGT);
      hb_gt_BaseFree(pGT);
   }
   return cdp;
}

HB_ERRCODE hb_gtInfo(int iType, PHB_GT_INFO pInfo)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtInfo(%d, %p)", iType, static_cast<void*>(pInfo) ) );
#endif

   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_INFO(pGT, iType, pInfo) )
      {
         errCode = HB_SUCCESS;
      }
      hb_gt_BaseFree(pGT);
   }
   return errCode;
}

int hb_gtAlert( PHB_ITEM pMessage, PHB_ITEM pOptions, int iClrNorm, int iClrHigh, double dDelay )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtAlert(%p, %p, %d, %d, %f)", static_cast<void*>(pMessage), static_cast<void*>(pOptions), iClrNorm, iClrHigh, dDelay ) );
#endif

   int iResult = 0;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      iResult = HB_GTSELF_ALERT(pGT, pMessage, pOptions, iClrNorm, iClrHigh, dDelay);
      hb_gt_BaseFree(pGT);
   }
   return iResult;
}

int hb_gtSetFlag( int iType, int iNewValue )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtSetFlag(%d, %d)", iType, iNewValue ) );
#endif

   int iFlag = 0;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      iFlag = HB_GTSELF_SETFLAG(pGT, iType, iNewValue);
      hb_gt_BaseFree(pGT);
   }
   return iFlag;
}

int hb_gtGetCurrColor(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtGetCurrColor()" ) );
#endif

   int iColor;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      iColor = HB_GTSELF_GETCOLOR(pGT);
      hb_gt_BaseFree(pGT);
   }
   else
   {
      iColor = 0x07;
   }

   return iColor;
}

int hb_gtGetClearColor(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtGetClearColor()" ) );
#endif

   int iColor;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      iColor = HB_GTSELF_GETCLEARCOLOR(pGT);
      hb_gt_BaseFree(pGT);
   }
   else
   {
      iColor = 0x07;
   }

   return iColor;
}

HB_ERRCODE hb_gtSetClearColor( int iColor )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtSetClearColor(%d)", iColor ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SETCLEARCOLOR(pGT, iColor);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_USHORT hb_gtGetClearChar(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtGetClearChar()" ) );
#endif

   HB_USHORT usChar;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      usChar = HB_GTSELF_GETCLEARCHAR(pGT);
      hb_gt_BaseFree(pGT);
   }
   else
   {
      usChar = static_cast<HB_USHORT>(' ');
   }

   return usChar;
}

HB_ERRCODE hb_gtSetClearChar( HB_USHORT usChar )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtSetClearChar(%hu)", usChar ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_SETCLEARCHAR(pGT, usChar);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

HB_ERRCODE hb_gtGetScrChar( int iRow, int iCol, int * piColor, HB_BYTE * pbAttr, HB_USHORT * pusChar )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtGetScrChar(%d, %d, %p, %p, %p)", iRow, iCol, static_cast<void*>(piColor), static_cast<void*>(pbAttr), static_cast<void*>(pusChar) ) );
#endif

   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_GETSCRCHAR(pGT, iRow, iCol, piColor, pbAttr, pusChar) )
      {
         errCode = HB_SUCCESS;
      }
      hb_gt_BaseFree(pGT);
   }
   return errCode;
}

HB_ERRCODE hb_gtPutScrChar( int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtPutScrChar(%d, %d, %d, %d, %hu)", iRow, iCol, iColor, static_cast<int>(bAttr), usChar ) );
#endif

   HB_ERRCODE errCode = HB_FAILURE;
   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      if( HB_GTSELF_PUTSCRCHAR(pGT, iRow, iCol, iColor, bAttr, usChar) )
      {
         errCode = HB_SUCCESS;
      }
      hb_gt_BaseFree(pGT);
   }
   return errCode;
}

HB_ERRCODE hb_gtFlush(void)
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtFlush()" ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}

int hb_gtGfxPrimitive( int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtGfxText(%d, %d, %d, %d, %d, %d)", iType, iTop, iLeft, iBottom, iRight, iColor ) );
#endif

   PHB_GT pGT;
   int iResult = 0;

   pGT = hb_gt_Base();
   if( pGT )
   {
      iResult = HB_GTSELF_GFXPRIMITIVE(pGT, iType, iTop, iLeft, iBottom, iRight, iColor);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
   }
   return iResult;
}

HB_ERRCODE hb_gtGfxText( int iTop, int iLeft, const char * cBuf, int iColor, int iSize, int iWidth )
{
#if 0
   HB_TRACE( HB_TR_DEBUG, ( "hb_gtGfxText(%d, %d, %s, %d, %d, %d)", iTop, iLeft, cBuf, iColor, iSize, iWidth ) );
#endif

   PHB_GT pGT;

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_GFXTEXT(pGT, iTop, iLeft, cBuf, iColor, iSize, iWidth);
      HB_GTSELF_FLUSH(pGT);
      hb_gt_BaseFree(pGT);
      return HB_SUCCESS;
   }
   return HB_FAILURE;
}
