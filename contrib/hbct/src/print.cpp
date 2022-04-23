/*
 * CT3 Printer functions
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour) (PrintReady())
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru> (PrintSend())
 * Copyright 2001 Walter Negro - FOEESITRA" <waltern@foeesitra.org.ar> (PrintStat())
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

#include "hbapi.h"
#include "hbapifs.h"

HB_FUNC( PRINTSTAT )
{
   HB_USHORT uiPort = static_cast<HB_USHORT>(hb_parnidef(1, 1));
   int Status = 0;

   HB_SYMBOL_UNUSED(uiPort);

   hb_retni(Status);
}

HB_FUNC( PRINTREADY )
{
   char szLPT[8];

   hb_snprintf( szLPT, sizeof(szLPT), "LPT%hu", static_cast<HB_USHORT>(hb_parnidef(1, 1)));

   hb_retl(hb_printerIsReady(szLPT));
}

HB_FUNC( PRINTSEND )
{
#if defined(HB_OS_WIN)

   char szChr[2] = { ' ', '\0' };
   char szPort[5] = { 'l', 'p', 't', '1', '\0' };
   const char * szStr = nullptr;
   HB_SIZE nLen = 0, nRet = 0;

   if( HB_ISNUM(1) )
   {
      szChr[0] = static_cast<char>(hb_parni(1));
      szStr = szChr;
      nLen = 1;
   }
   else if( HB_ISCHAR(1) )
   {
      szStr = hb_parc(1);
      nLen = hb_parclen(1);
   }

   if( HB_ISNUM(2) )
   {
      szPort[3] = static_cast<char>(hb_parni(2)) + '0';
   }

   if( nLen )
   {
      HB_FHANDLE hFile = hb_fsOpen( szPort, FO_WRITE );
      if( hFile != FS_ERROR )
      {
         nRet = hb_fsWriteLarge( hFile, szStr, nLen );
         hb_fsClose( hFile );
      }
   }
   hb_retns( nRet );

#else

   hb_retns(0);

#endif
}
