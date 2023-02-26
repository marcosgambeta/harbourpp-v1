/*
 * Function to export chosen addresses of public API function
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbvm.hpp"

PHB_FUNC hb_vmProcAddress(const char * szFuncName)
{
   struct HB_FUNC_REF_ADDR
   {
      const char * szFuncName;
      PHB_FUNC     pFuncAddr;
   };

   /* NOTE: this table must be well sorted by function names */
   static const HB_FUNC_REF_ADDR s_funcTable[] =
   {
      { "hb_arrayAdd",         reinterpret_cast<PHB_FUNC>(hb_arrayAdd              ) },
      { "hb_arrayDel",         reinterpret_cast<PHB_FUNC>(hb_arrayDel              ) },
      { "hb_arrayGet",         reinterpret_cast<PHB_FUNC>(hb_arrayGet              ) },
      { "hb_arrayIns",         reinterpret_cast<PHB_FUNC>(hb_arrayIns              ) },
      { "hb_arrayIsObject",    reinterpret_cast<PHB_FUNC>(hb_arrayIsObject         ) },
      { "hb_arrayLast",        reinterpret_cast<PHB_FUNC>(hb_arrayLast             ) },
      { "hb_arrayLen",         reinterpret_cast<PHB_FUNC>(hb_arrayLen              ) },
      { "hb_arrayNew",         reinterpret_cast<PHB_FUNC>(hb_arrayNew              ) },
      { "hb_arraySet",         reinterpret_cast<PHB_FUNC>(hb_arraySet              ) },
      { "hb_arraySize",        reinterpret_cast<PHB_FUNC>(hb_arraySize             ) },
      { "hb_extIsArray",       reinterpret_cast<PHB_FUNC>(hb_extIsArray            ) },
      { "hb_extIsNil",         reinterpret_cast<PHB_FUNC>(hb_extIsNil              ) },
      { "hb_param",            reinterpret_cast<PHB_FUNC>(hb_param                 ) },
      { "hb_paramError",       reinterpret_cast<PHB_FUNC>(hb_paramError            ) },
      { "hb_parinfa",          reinterpret_cast<PHB_FUNC>(hb_parinfa               ) },
      { "hb_parinfo",          reinterpret_cast<PHB_FUNC>(hb_parinfo               ) },
      { "hb_parvc",            reinterpret_cast<PHB_FUNC>(hb_parvc                 ) },
      { "hb_parvclen",         reinterpret_cast<PHB_FUNC>(hb_parvclen              ) },
      { "hb_parvcsiz",         reinterpret_cast<PHB_FUNC>(hb_parvcsiz              ) },
      { "hb_parvds",           reinterpret_cast<PHB_FUNC>(hb_parvds                ) },
      { "hb_parvdsbuff",       reinterpret_cast<PHB_FUNC>(hb_parvdsbuff            ) },
      { "hb_parvl",            reinterpret_cast<PHB_FUNC>(hb_parvl                 ) },
      { "hb_parvnd",           reinterpret_cast<PHB_FUNC>(hb_parvnd                ) },
      { "hb_parvni",           reinterpret_cast<PHB_FUNC>(hb_parvni                ) },
      { "hb_parvnl",           reinterpret_cast<PHB_FUNC>(hb_parvnl                ) },
      { "hb_pcount",           reinterpret_cast<PHB_FUNC>(hb_pcount                ) },
      { "hb_ret",                   static_cast<PHB_FUNC>(hb_ret                   ) },
      { "hb_reta",             reinterpret_cast<PHB_FUNC>(hb_reta                  ) },
      { "hb_retc",             reinterpret_cast<PHB_FUNC>(hb_retc                  ) },
      { "hb_retclen",          reinterpret_cast<PHB_FUNC>(hb_retclen               ) },
      { "hb_retd",             reinterpret_cast<PHB_FUNC>(hb_retd                  ) },
      { "hb_retdl",            reinterpret_cast<PHB_FUNC>(hb_retdl                 ) },
      { "hb_retds",            reinterpret_cast<PHB_FUNC>(hb_retds                 ) },
      { "hb_retl",             reinterpret_cast<PHB_FUNC>(hb_retl                  ) },
      { "hb_retnd",            reinterpret_cast<PHB_FUNC>(hb_retnd                 ) },
      { "hb_retndlen",         reinterpret_cast<PHB_FUNC>(hb_retndlen              ) },
      { "hb_retni",            reinterpret_cast<PHB_FUNC>(hb_retni                 ) },
      { "hb_retnilen",         reinterpret_cast<PHB_FUNC>(hb_retnilen              ) },
      { "hb_retnl",            reinterpret_cast<PHB_FUNC>(hb_retnl                 ) },
      { "hb_retnlen",          reinterpret_cast<PHB_FUNC>(hb_retnlen               ) },
      { "hb_retnllen",         reinterpret_cast<PHB_FUNC>(hb_retnllen              ) },
      { "hb_storvc",           reinterpret_cast<PHB_FUNC>(hb_storvc                ) },
      { "hb_storvclen",        reinterpret_cast<PHB_FUNC>(hb_storvclen             ) },
      { "hb_storvds",          reinterpret_cast<PHB_FUNC>(hb_storvds               ) },
      { "hb_storvl",           reinterpret_cast<PHB_FUNC>(hb_storvl                ) },
      { "hb_storvnd",          reinterpret_cast<PHB_FUNC>(hb_storvnd               ) },
      { "hb_storvni",          reinterpret_cast<PHB_FUNC>(hb_storvni               ) },
      { "hb_storvnl",          reinterpret_cast<PHB_FUNC>(hb_storvnl               ) },
      { "hb_vmExecute",        reinterpret_cast<PHB_FUNC>(hb_vmExecute             ) },
      { "hb_vmProcessSymbols", reinterpret_cast<PHB_FUNC>(hb_vmProcessDynLibSymbols) },
      { "hb_xalloc",           reinterpret_cast<PHB_FUNC>(hb_xalloc                ) },
      { "hb_xfree",            reinterpret_cast<PHB_FUNC>(hb_xfree                 ) },
      { "hb_xgrab",            reinterpret_cast<PHB_FUNC>(hb_xgrab                 ) },
      { "hb_xrealloc",         reinterpret_cast<PHB_FUNC>(hb_xrealloc              ) }
   };

   unsigned int uiFirst = 0, uiLast = HB_SIZEOFARRAY(s_funcTable), uiMiddle;
   int iCmp;

   do
   {
      uiMiddle = (uiFirst + uiLast) >> 1;
      iCmp = strcmp(szFuncName, s_funcTable[uiMiddle].szFuncName);
      if( iCmp <= 0 )
      {
         uiLast = uiMiddle;
      }
      else
      {
         uiFirst = uiMiddle + 1;
      }
   }
   while( uiFirst < uiLast );

   if( uiFirst != uiMiddle )
   {
      iCmp = strcmp(szFuncName, s_funcTable[uiFirst].szFuncName);
   }

   return iCmp == 0 ? s_funcTable[uiFirst].pFuncAddr : nullptr;
}
