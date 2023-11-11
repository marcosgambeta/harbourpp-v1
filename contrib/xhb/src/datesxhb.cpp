/*
 * The Date API (Harbour level)
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2004 Giancarlo Niccolai <gc@niccolai.ws>
 * Copyright 2006 Pavel Tsarenko <tpe2@mail.ru> (DaysInMonth(), DaysToMonth())
 * Copyright 2007 Walter Negro <anegro@overnet.com.ar>
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

#include "hbapi.hpp"
#include "hbapierr.hpp"
#include "hbdate.hpp"

/* NOTE: szTime must be 9 chars large. */

static HB_ULONG hb_TimeStrToSec(const char * pszTime)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_TimeStrToSec(%s)", pszTime));
#endif

   HB_SIZE  nLen;
   HB_ULONG ulTime = 0;

   nLen = strlen(pszTime);

   if( nLen >= 1 ) {
      ulTime += static_cast<HB_ULONG>(hb_strVal(pszTime, nLen)) * 3600;
   }

   if( nLen >= 4 ) {
      ulTime += static_cast<HB_ULONG>(hb_strVal(pszTime + 3, nLen - 3)) * 60;
   }

   if( nLen >= 7 ) {
      ulTime += static_cast<HB_ULONG>(hb_strVal(pszTime + 6, nLen - 6));
   }

   return ulTime;
}

HB_FUNC( TSSECS )
{
   hb_retnl(hb_TimeStrToSec(hb_parcx(1)));
}

HB_FUNC( TIMEOFDAY )
{
   char szResult[9];

   if( hb_pcount() == 0 ) {
      hb_dateTimeStr(szResult);
   } else {
      auto iSeconds = hb_parni(1);
      iSeconds %= 3600 * 24;
      hb_snprintf(szResult, sizeof(szResult), "%02d:%02d:%02d", iSeconds / 3600, (iSeconds % 3600) / 60, iSeconds % 60);
   }

   hb_retclen(szResult, 8);
}

HB_FUNC( HMS2D )
{
   auto iHour = hb_parni(1);
   auto iMin  = hb_parni(2);
   auto dSec  = hb_parnd(3);

   hb_retnd(hb_timeEncode(iHour, iMin, static_cast<int>(dSec), static_cast<int>((static_cast<double>(dSec - static_cast<double>(static_cast<int>(dSec)))) * 1000)));
}

HB_FUNC( TTOD )
{
   if( HB_ISDATE(1) ) {
      hb_retdl(hb_pardl(1));
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 1120, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}
