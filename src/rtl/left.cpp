/*
 * Left() function
 *
 * Copyright 2012 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
#include "hbapiitm.hpp"
#include "hbapicdp.hpp"
#include "hbapierr.hpp"

/* returns the left-most n characters in string */

HB_FUNC( LEFT )
{
   auto pText = hb_param(1, Harbour::Item::STRING);

   if( pText && HB_ISNUM(2) ) {
      HB_ISIZ nLen = hb_parns(2);
      if( nLen <= 0 ) {
         hb_retc_null();
      } else {
         auto nText = hb_itemGetCLen(pText);
         if( static_cast<HB_SIZE>(nLen) < nText ) {
            auto cdp = hb_vmCDP();
            if( HB_CDP_ISCHARIDX(cdp) ) {
               nLen = hb_cdpTextPos(cdp, hb_itemGetCPtr(pText), nText, nLen);
            }
         }
         if( static_cast<HB_SIZE>(nLen) >= nText ) {
            hb_itemReturn(pText);
         } else {
            hb_retclen(hb_itemGetCPtr(pText), nLen);
         }
      }
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 1124, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( HB_LEFTEQ )
{
   auto pItem1 = hb_param(1, Harbour::Item::STRING);
   auto pItem2 = hb_param(2, Harbour::Item::STRING);

   if( pItem1 && pItem2 ) {
      hb_retl(hb_cdpcmp(hb_itemGetCPtr(pItem1), hb_itemGetCLen(pItem1), hb_itemGetCPtr(pItem2), hb_itemGetCLen(pItem2), hb_vmCDP(), false) == 0);
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 1071, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

HB_FUNC( HB_LEFTEQI )
{
   auto pItem1 = hb_param(1, Harbour::Item::STRING);
   auto pItem2 = hb_param(2, Harbour::Item::STRING);

   if( pItem1 && pItem2 ) {
      hb_retl(hb_cdpicmp(hb_itemGetCPtr(pItem1), hb_itemGetCLen(pItem1), hb_itemGetCPtr(pItem2), hb_itemGetCLen(pItem2), hb_vmCDP(), false) == 0);
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 1071, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}
