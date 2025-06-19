// National Collation Support Module (GBK)
// Copyright 2012 Dongming Wang <wangdongming / at / gmail.com>
// Copyright 2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>

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

#include "hbapi.hpp"
#include "hbapicdp.hpp"

#include "gbk.cpp"

static HB_CDP_GET_FUNC(GBK_get)
{
   *wc = 0;
   if( *pnIndex < nLen )
   {
      HB_UCHAR uc = pSrc[(*pnIndex)++];

      if( uc >= (HB_GBK_FIRST >> 8) && uc <= (HB_GBK_LAST >> 8) && *pnIndex < nLen )
      {
         *wc = s_gbk_to_ucs16((static_cast<int>(uc) << 8) | static_cast<HB_UCHAR>(pSrc[*pnIndex]) );
         if( *wc )
         {
            (*pnIndex)++;
            return true;
         }
      }
      *wc = cdp->uniTable->uniCodes[uc];
      if( *wc == 0 )
      {
         *wc = uc;
      }
      return true;
   }
   return false;
}

static HB_CDP_PUT_FUNC(GBK_put)
{
   if( *pnIndex < nLen )
   {
      HB_USHORT gb18030 = s_ucs16_to_gbk(wc);

      if( gb18030 )
      {
         if( *pnIndex + 1 < nLen )
         {
            HB_PUT_BE_UINT16(&pDst[(*pnIndex)], gb18030);
            *pnIndex += 2;
            return true;
         }
      } else {
         if( cdp->uniTable->uniTrans == nullptr )
         {
            hb_cdpBuildTransTable(cdp->uniTable);
         }

         if( wc <= cdp->uniTable->wcMax && cdp->uniTable->uniTrans[wc] )
         {
            pDst[(*pnIndex)++] = cdp->uniTable->uniTrans[wc];
         } else {
            pDst[(*pnIndex)++] = wc >= 0x100 ? '?' : static_cast<HB_UCHAR>(wc);
         }
         return true;
      }
   }
   return false;
}

static HB_CDP_LEN_FUNC(GBK_len)
{
   HB_USHORT gb18030 = s_ucs16_to_gbk(wc);

   HB_SYMBOL_UNUSED(cdp);

   return gb18030 ? 2 : 1;
}

static void hb_cp_init(PHB_CODEPAGE cdp)
{
   HB_UCHAR * flags, * upper, * lower;

   cdp->buffer = static_cast<HB_UCHAR*>(hb_xgrab(0x300));
   cdp->flags = flags = static_cast<HB_UCHAR*>(cdp->buffer);
   cdp->upper = upper = static_cast<HB_UCHAR*>(cdp->buffer) + 0x100;
   cdp->lower = lower = static_cast<HB_UCHAR*>(cdp->buffer) + 0x200;

   for( auto i = 0; i < 0x100; ++i )
   {
      flags[i] = 0;
      if( HB_ISDIGIT(i) )
      {
         flags[i] |= HB_CDP_DIGIT;
      }
      if( HB_ISALPHA(i) )
      {
         flags[i] |= HB_CDP_ALPHA;
      }
      if( HB_ISUPPER(i) )
      {
         flags[i] |= HB_CDP_UPPER;
      }
      if( HB_ISLOWER(i) )
      {
         flags[i] |= HB_CDP_LOWER;
      }
      upper[i] = static_cast<HB_UCHAR>(HB_TOUPPER(i));
      lower[i] = static_cast<HB_UCHAR>(HB_TOLOWER(i));
   }

#if 0
   for( i = 0; i < 0x10000; ++i )
   {
      HB_WCHAR wc = s_gbk_to_ucs16(i);
      if( wc )
      {
         if( i != s_ucs16_to_gbk(wc) )
         {
            printf("irreversible translation: (GBK)%04X -> U+%04X -> (GBK)%04X\r\n", i, wc, s_ucs16_to_gbk(wc));
            fflush(stdout);
         }
      }
   }
#endif
}

#define HB_CP_RAW

#define HB_CP_ID              GBK
constexpr const char *HB_CP_INFO = "CP936"; // #define HB_CP_INFO            "CP936"
#define HB_CP_UNITB           HB_UNITB_437

#define HB_CP_GET_FUNC        GBK_get
#define HB_CP_PUT_FUNC        GBK_put
#define HB_CP_LEN_FUNC        GBK_len

#define HB_CP_CMP_FUNC        nullptr
#define HB_CP_FLAGS_FUNC      nullptr
#define HB_CP_UPPER_FUNC      nullptr
#define HB_CP_LOWER_FUNC      nullptr

#define s_flags               nullptr
#define s_upper               nullptr
#define s_lower               nullptr
#define s_sort                nullptr

#define HB_CP_INIT hb_cp_init

// include CP registration code
#include "hbcdpreg.hpp"
