/*
 * FoxPro compatible database functions.
 *
 * Copyright 2013 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbapirdd.hpp"
#include "hbapierr.hpp"
#include "hbset.hpp"

static AREAP s_foxAreaPointer(int iParam)
{
   if( HB_ISNIL(iParam) )
   {
      return static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());
   }
   else
   {
      auto szAlias = hb_parc(iParam);
      int iArea;

      if( szAlias )
      {
         hb_rddGetAliasNumber(szAlias, &iArea);
      }
      else
      {
         iArea = hb_parni(iParam);
      }

      return static_cast<AREAP>(hb_rddGetWorkAreaPointer(iArea));
   }
}

HB_FUNC( FILTER )
{
   AREAP pArea = s_foxAreaPointer(1);

   if( pArea != nullptr )
   {
      PHB_ITEM pFilter = hb_itemPutC(nullptr, nullptr);
      SELF_FILTERTEXT(pArea, pFilter);
      hb_itemReturnRelease(pFilter);
   }
   else
   {
      hb_retc_null();
   }
}

HB_FUNC( NDX )
{
   AREAP pArea = s_foxAreaPointer(2);

   if( pArea != nullptr )
   {
      DBORDERINFO pOrderInfo;
      memset(&pOrderInfo, 0, sizeof(pOrderInfo));
      pOrderInfo.itmOrder = hb_param(1, Harbour::Item::NUMERIC);
      if( hb_itemGetNI(pOrderInfo.itmOrder) == 0 )
      {
         pOrderInfo.itmOrder = nullptr;
      }
      pOrderInfo.itmResult   = hb_itemPutC(nullptr, nullptr);
      SELF_ORDINFO(pArea, DBOI_NAME, &pOrderInfo);
      hb_itemReturnRelease(pOrderInfo.itmResult);
   }
   else
   {
      hb_retc_null();
   }
}

HB_FUNC( RELATION )
{
   AREAP pArea = s_foxAreaPointer(2);

   if( pArea != nullptr )
   {
      PHB_ITEM pRelExpr = hb_itemPutC(nullptr, nullptr);
      HB_USHORT uiRelNo = static_cast<HB_USHORT>(hb_parni(1));
      SELF_RELTEXT(pArea, uiRelNo ? uiRelNo : 1, pRelExpr);
      hb_itemReturnRelease(pRelExpr);
   }
   else
   {
      hb_retc_null();
   }
}

HB_FUNC( FSIZE )
{
   AREAP pArea = s_foxAreaPointer(2);

   if( pArea != nullptr )
   {
      HB_FIELDNO uiIndex;
      const char * szField;

      if( HB_ISNIL(1) )
      {
         uiIndex = 1;
      }
      else if( (szField = hb_parc(1)) != nullptr )
      {
         uiIndex = hb_rddFieldIndex(pArea, szField);
      }
      else
      {
         uiIndex = static_cast<HB_FIELDNO>(hb_parni(1));
      }

      if( uiIndex > 0 )
      {
         auto pItem = hb_itemNew(nullptr);

         if( SELF_FIELDINFO(pArea, uiIndex, DBS_LEN, pItem) == Harbour::SUCCESS )
         {
            hb_itemReturnRelease(pItem);
            return;
         }
         hb_itemRelease(pItem);
      }
   }

   hb_retni(0);
}

HB_FUNC( __FOX_USED )
{
   hb_retl(s_foxAreaPointer(1) != nullptr);
}

HB_FUNC( __FOX_SEEK )
{
   AREAP pArea = s_foxAreaPointer(4);

   if( pArea != nullptr )
   {
      if( !HB_ISNIL(1) )
      {
         auto pKey = hb_param(1, Harbour::Item::ANY);
         HB_BOOL fSoftSeek = HB_ISLOG(2) ? static_cast<HB_BOOL>(hb_parl(2)) : hb_setGetSoftSeek();
         HB_BOOL fFindLast = hb_parl(3), fFound = false;
         auto pTag = hb_param(5, Harbour::Item::NUMERIC | Harbour::Item::STRING);
         HB_ERRCODE errCode = Harbour::SUCCESS;

         if( pTag )
         {
            DBORDERINFO pInfo;
            memset(&pInfo, 0, sizeof(pInfo));
            pInfo.itmOrder = pTag;
            pInfo.itmResult = hb_itemNew(nullptr);
            errCode = SELF_ORDLSTFOCUS(pArea, &pInfo);
            hb_itemRelease(pInfo.itmResult);
         }

         if( errCode == Harbour::SUCCESS )
         {
            if( SELF_SEEK(pArea, fSoftSeek, pKey, fFindLast) == Harbour::SUCCESS )
            {
               if( SELF_FOUND(pArea, &fFound) != Harbour::SUCCESS )
               {
                  fFound = false;
               }
            }
         }

         hb_retl(fFound);
      }
      else
      {
         hb_errRT_DBCMD(EG_ARG, EDBCMD_SEEK_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
      }
   }
   else
   {
      hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
   }
}
