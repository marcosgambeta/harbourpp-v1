/*
 * CUPS wrappers
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
 *
 */

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

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

#include <hbapi.hpp>
#include <hbapiitm.hpp>

#include <cups/cups.h>

HB_FUNC(CUPSGETDEFAULT)
{
   hb_retc(cupsGetDefault());
}

HB_FUNC(CUPSGETDESTS)
{
   cups_dest_t * dest_list;
   int      num_dests = cupsGetDests(&dest_list);
   auto pArray = hb_itemArrayNew(static_cast<HB_SIZE>(num_dests));

   if( num_dests > 0 )
   {
      cups_dest_t * desk_list_bak = dest_list;
      int i;

      for( i = 1; i <= num_dests; ++i, ++dest_list )
         hb_arraySetC(pArray, i, dest_list->name);

      cupsFreeDests(num_dests, desk_list_bak);
   }

   hb_itemReturnRelease(pArray);
}

HB_FUNC(CUPSPRINTFILE)
{
   auto pOptions = hb_param(4, Harbour::Item::HASH | Harbour::Item::ARRAY);

   int num_options         = 0;
   cups_option_t * options = nullptr;

   if( pOptions )
   {
      HB_SIZE tmp;

      if( pOptions->isHash() )
      {
         for( tmp = 1; tmp <= hb_hashLen(pOptions); ++tmp )
         {
            PHB_ITEM pKey = hb_hashGetKeyAt(pOptions, tmp);
            PHB_ITEM pVal = hb_hashGetValueAt(pOptions, tmp);

            if( pKey && pKey->isString() && pVal )
               num_options = cupsAddOption(hb_itemGetCPtr(pKey), hb_itemGetCPtr(pVal), num_options, &options);
         }
      }
      else if( pOptions->isArray() )
      {
         for( tmp = 1; tmp <= hb_arrayLen(pOptions); ++tmp )
         {
            auto pszOption = hb_arrayGetCPtr(pOptions, tmp);

            if( pszOption )
               num_options = cupsParseOptions(pszOption, num_options, &options);
         }
      }
   }

   hb_retni(cupsPrintFile(hb_parcx(1) /* printername */, hb_parcx(2) /* filename */, hb_parcx(3) /* title */, num_options, options));

   cupsFreeOptions(num_options, options);
}
