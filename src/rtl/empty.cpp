/*
 * Empty() function
 *
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

#include "hbapi.h"
#include "hbapiitm.h"

HB_FUNC( EMPTY )
{
   PHB_ITEM pItem = hb_param(1, Harbour::Item::ANY);
   long lDate, lTime;
   PHB_SYMB pSym;

   switch( hb_itemType(pItem) )
   {
      case Harbour::Item::ARRAY:
         hb_retl(hb_arrayLen(pItem) == 0);
         break;

      case Harbour::Item::HASH:
         hb_retl(hb_hashLen(pItem) == 0);
         break;

      case Harbour::Item::STRING:
      case Harbour::Item::MEMO:
         hb_retl(hb_strEmpty(hb_itemGetCPtr(pItem), hb_itemGetCLen(pItem)));
         break;

      case Harbour::Item::INTEGER:
         hb_retl(hb_itemGetNI(pItem) == 0);
         break;

      case Harbour::Item::LONG:
         hb_retl(hb_itemGetNInt(pItem) == 0);
         break;

      case Harbour::Item::DOUBLE:
         hb_retl(hb_itemGetND(pItem) == 0.0);
         break;

      case Harbour::Item::DATE:
         hb_retl(hb_itemGetDL(pItem) == 0);
         break;

      case Harbour::Item::TIMESTAMP:
         hb_itemGetTDT(pItem, &lDate, &lTime);
         hb_retl(lDate == 0 && lTime == 0);
         break;

      case Harbour::Item::LOGICAL:
         hb_retl(!hb_itemGetL(pItem));
         break;

      case Harbour::Item::BLOCK:
         hb_retl(false);
         break;

      case Harbour::Item::POINTER:
         hb_retl(hb_itemGetPtr(pItem) == nullptr);
         break;

      case Harbour::Item::SYMBOL:
         pSym = hb_itemGetSymbol(pItem);
         if( pSym && (pSym->scope.value & HB_FS_DEFERRED) && \
             pSym->pDynSym )
         {
            pSym = hb_dynsymSymbol(pSym->pDynSym);
         }
         hb_retl(pSym == nullptr || pSym->value.pFunPtr == nullptr);
         break;

      default:
         hb_retl(true);
         break;
   }
}
