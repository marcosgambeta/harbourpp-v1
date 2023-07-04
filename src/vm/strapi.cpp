/*
 * String API functions
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbvmopt.hpp"
#include "hbapistr.hpp"
#include "hbapiitm.hpp"
#include "hbstack.hpp"

static const HB_WCHAR s_szConstStr[1] = { 0 };

HB_SIZE hb_wstrlen(const HB_WCHAR * szText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_wstrlen(%p)", static_cast<const void*>(szText)));
#endif

   HB_SIZE nLen = 0;

   if( szText ) {
      while( szText[nLen] ) {
         ++nLen;
      }
   }

   return nLen;
}

HB_SIZE hb_wstrnlen(const HB_WCHAR * szText, HB_SIZE nCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_wstrnlen(%p,%" HB_PFS "u)", static_cast<const void*>(szText), nCount));
#endif

   HB_SIZE nLen = 0;

   if( szText ) {
      while( nCount-- && szText[nLen] ) {
         ++nLen;
      }
   }

   return nLen;
}

int hb_wstrcmp(const HB_WCHAR * s1, const HB_WCHAR * s2)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_wstrcmp(%p, %p)", static_cast<const void*>(s1), static_cast<const void*>(s2)));
#endif

   int rc = 0;

   for( ;; ) {
      if( *s1 != *s2 ) {
         rc = (*s1 < *s2 ? -1 : 1);
         break;
      } else if( *s1 == 0 ) {
         break;
      }

      s1++;
      s2++;
   }

   return rc;
}

int hb_wstrncmp(const HB_WCHAR * s1, const HB_WCHAR * s2, HB_SIZE nCount)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_wstrncmp(%p, %p, %" HB_PFS "u)", static_cast<const void*>(s1), static_cast<const void*>(s2), nCount));
#endif

   int rc = 0;

   while( nCount-- ) {
      if( *s1 != *s2 ) {
         rc = (*s1 < *s2 ? -1 : 1);
         break;
      } else if( *s1 == 0 ) {
         break;
      }

      s1++;
      s2++;
   }

   return rc;
}

HB_WCHAR * hb_wstrncpy(HB_WCHAR * pDest, const HB_WCHAR * pSource, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_wstrncpy(%p, %p, %" HB_PFS "u)", static_cast<void*>(pDest), static_cast<const void*>(pSource), nLen));
#endif

   HB_WCHAR * pBuf = pDest;

   pDest[nLen] = '\0';

   while( nLen && (*pDest++ = *pSource++) != '\0' ) {
      nLen--;
   }

   return pBuf;
}

HB_WCHAR * hb_wstrncat(HB_WCHAR * pDest, const HB_WCHAR * pSource, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strncat(%p, %p, %" HB_PFS "u)", static_cast<void*>(pDest), static_cast<const void*>(pSource), nLen));
#endif

   HB_WCHAR * pBuf = pDest;

   pDest[nLen] = '\0';

   while( nLen && *pDest ) {
      pDest++;
      nLen--;
   }

   while( nLen && (*pDest++ = *pSource++) != '\0' ) {
      nLen--;
   }

   return pBuf;
}

HB_WCHAR * hb_wstrdup(const HB_WCHAR * szText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_wstrdup(%p)", static_cast<const void*>(szText)));
#endif

   HB_SIZE nSize = (hb_wstrlen(szText) + 1) * sizeof(HB_WCHAR);
   HB_WCHAR * pszDest = static_cast<HB_WCHAR*>(hb_xgrab(nSize));

   memcpy(pszDest, szText, nSize);

   return pszDest;
}

HB_WCHAR * hb_wstrndup(const HB_WCHAR * szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_wstrndup(%p,%" HB_PFS "u)", static_cast<const void*>(szText), nLen));
#endif

   HB_SIZE nSize = hb_wstrlen(szText);
   if( nSize < nLen ) {
      nLen = nSize;
   }
   nSize = nLen * sizeof(HB_WCHAR);
   HB_WCHAR * pszDest = static_cast<HB_WCHAR*>(hb_xgrab(nSize + sizeof(HB_WCHAR)));
   memcpy(pszDest, szText, nSize);
   pszDest[nLen] = 0;

   return pszDest;
}

HB_WCHAR * hb_wstrunshare(void ** phStr, const HB_WCHAR * pStr, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_wstrunshare(%p,%p,%" HB_PFS "u)", static_cast<void*>(phStr), static_cast<const void*>(pStr), nLen));
#endif

   if( pStr == nullptr || phStr == nullptr || *phStr == nullptr ) {
      return nullptr;
   }

   if( nLen > 0 && (*phStr == static_cast<const void*>(s_szConstStr) || hb_xRefCount(*phStr) > 1) ) {
      HB_WCHAR * pszDest = static_cast<HB_WCHAR*>(hb_xgrab((nLen + 1) * sizeof(HB_WCHAR)));
      memcpy(pszDest, pStr, nLen * sizeof(HB_WCHAR));
      pszDest[nLen] = 0;
      if( *phStr != static_cast<const void*>(s_szConstStr) ) {
         hb_xRefDec(*phStr);
      }
      *phStr = static_cast<void*>(pszDest);

      return pszDest;
   }

   return const_cast<HB_WCHAR*>(pStr);
}

char * hb_strunshare(void ** phStr, const char * pStr, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strunshare(%p,%p,%" HB_PFS "u)", static_cast<void*>(phStr), static_cast<const void*>(pStr), nLen));
#endif

   if( pStr == nullptr || phStr == nullptr || *phStr == nullptr ) {
      return nullptr;
   }

   if( nLen > 0 && (*phStr == static_cast<const void*>(s_szConstStr) || hb_xRefCount(*phStr) > 1) ) {
      char * pszDest = static_cast<char*>(hb_xgrab((nLen + 1) * sizeof(char)));
      memcpy(pszDest, pStr, nLen * sizeof(char));
      pszDest[nLen] = 0;
      if( *phStr != static_cast<const void*>(s_szConstStr) ) {
         hb_xRefDec(*phStr);
      }
      *phStr = static_cast<void*>(pszDest);

      return pszDest;
   }

   return const_cast<char*>(pStr);
}

const char * hb_strnull(const char * str)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strnull(%p)", static_cast<const void*>(str)));
#endif

   return str ? str : "";
}

const HB_WCHAR * hb_wstrnull(const HB_WCHAR * str)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_wstrnull(%p)", static_cast<const void*>(str)));
#endif

   return str ? str : s_szConstStr;
}

void hb_strfree(void * hString)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_strfree(%p)", hString));
#endif

   if( hString && hString != static_cast<const void*>(s_szConstStr) ) {
      hb_xRefFree(hString);
   }
}

const char * hb_itemGetStr(PHB_ITEM pItem, void * cdp, void ** phString, HB_SIZE * pnLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetStr(%p,%p,%p,%p)", static_cast<void*>(pItem), cdp, static_cast<void*>(phString), static_cast<void*>(pnLen)));
#endif

   if( pItem && HB_IS_STRING(pItem) ) {
      char * pFree = nullptr;
      HB_SIZE nSize = 0;

      const char * pString = hb_cdpnDup3(pItem->item.asString.value, pItem->item.asString.length, nullptr, pnLen, &pFree, &nSize, hb_vmCDP(), static_cast<PHB_CODEPAGE>(cdp));
      if( pFree != nullptr ) {
         *phString = static_cast<void*>(pFree);
      } else if( pItem->item.asString.allocated == 0 ) {
         *phString = HB_UNCONST(s_szConstStr);
      } else {
         *phString = static_cast<void*>(pItem->item.asString.value);
         hb_xRefInc(pItem->item.asString.value);
      }
      return pString;
   }

   if( pnLen ) {
      *pnLen = 0;
   }
   *phString = nullptr;

   return nullptr;
}

const char * hb_itemGetStrUTF8(PHB_ITEM pItem, void ** phString, HB_SIZE * pnLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetStrUTF8(%p,%p,%p)", static_cast<void*>(pItem), static_cast<void*>(phString), static_cast<void*>(pnLen)));
#endif

   if( pItem && HB_IS_STRING(pItem) ) {
      PHB_CODEPAGE cdp = hb_vmCDP();
      HB_SIZE nLen = hb_cdpStrAsUTF8Len(cdp, pItem->item.asString.value, pItem->item.asString.length, 0);
      if( pnLen ) {
         *pnLen = nLen;
      }

      if( nLen != pItem->item.asString.length ) {
         char * pszUtf8 = static_cast<char*>(hb_xgrab(nLen + 1));
         hb_cdpStrToUTF8(cdp, pItem->item.asString.value, pItem->item.asString.length, pszUtf8, nLen + 1);
         *phString = static_cast<void*>(pszUtf8);
         return pszUtf8;
      }

      if( pItem->item.asString.allocated != 0 ) {
         *phString = static_cast<void*>(pItem->item.asString.value);
         hb_xRefInc(pItem->item.asString.value);
      } else {
         *phString = HB_UNCONST(s_szConstStr);
      }
      return pItem->item.asString.value;
   }

   if( pnLen ) {
      *pnLen = 0;
   }
   *phString = nullptr;

   return nullptr;
}

const HB_WCHAR * hb_itemGetStrU16(PHB_ITEM pItem, int iEndian, void ** phString, HB_SIZE * pnLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetStrU16(%p,%d,%p,%p)", static_cast<void*>(pItem), iEndian, static_cast<void*>(phString), static_cast<void*>(pnLen)));
#endif

   if( pItem && HB_IS_STRING(pItem) ) {
      PHB_CODEPAGE cdp = hb_vmCDP();
      HB_SIZE nLen = hb_cdpStrAsU16Len(cdp, pItem->item.asString.value, pItem->item.asString.length, 0);
      if( pnLen ) {
         *pnLen = nLen;
      }

      if( nLen == 0 ) {
         *phString = HB_UNCONST(s_szConstStr);
         return s_szConstStr;
      }

      HB_WCHAR * pszU16 = static_cast<HB_WCHAR*>(hb_xgrab((nLen + 1) * sizeof(HB_WCHAR)));
      hb_cdpStrToU16(cdp, iEndian, pItem->item.asString.value, pItem->item.asString.length, pszU16, nLen + 1);

      *phString = static_cast<void*>(pszU16);
      return pszU16;
   }

   if( pnLen ) {
      *pnLen = 0;
   }
   *phString = nullptr;

   return nullptr;
}

HB_SIZE hb_itemCopyStr(PHB_ITEM pItem, void * cdp, char * pStrBuffer, HB_SIZE nSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyStr(%p,%p,%p,%" HB_PFS "u)", static_cast<void*>(pItem), cdp, static_cast<void*>(pStrBuffer), nSize));
#endif

   if( pItem && HB_IS_STRING(pItem) ) {
      if( pStrBuffer ) {
         return hb_cdpTransTo(pItem->item.asString.value, pItem->item.asString.length, pStrBuffer, nSize, hb_vmCDP(), static_cast<PHB_CODEPAGE>(cdp));
      } else {
         return hb_cdpnDup2Len(pItem->item.asString.value, pItem->item.asString.length, nSize, hb_vmCDP(), static_cast<PHB_CODEPAGE>(cdp));
      }
   } else if( pStrBuffer && nSize ) {
      pStrBuffer[0] = '\0';
   }

   return 0;
}

HB_SIZE hb_itemCopyStrUTF8(PHB_ITEM pItem, char * pStrBuffer, HB_SIZE nSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyStrUTF8(%p,%p,%" HB_PFS "u)", static_cast<void*>(pItem), static_cast<void*>(pStrBuffer), nSize));
#endif

   if( pItem && HB_IS_STRING(pItem) ) {
      if( pStrBuffer ) {
         nSize = hb_cdpStrToUTF8(hb_vmCDP(), pItem->item.asString.value, pItem->item.asString.length, pStrBuffer, nSize);
      } else {
         nSize = hb_cdpStrAsUTF8Len(hb_vmCDP(), pItem->item.asString.value, pItem->item.asString.length, nSize);
      }
      return nSize;
   } else if( pStrBuffer && nSize ) {
      pStrBuffer[0] = '\0';
   }

   return 0;
}

HB_SIZE hb_itemCopyStrU16(PHB_ITEM pItem, int iEndian, HB_WCHAR * pStrBuffer, HB_SIZE nSize)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyStrU16(%p,%d,%p,%" HB_PFS "u)", static_cast<void*>(pItem), iEndian, static_cast<void*>(pStrBuffer), nSize));
#endif

   if( pItem && HB_IS_STRING(pItem) ) {
      if( pStrBuffer ) {
         nSize = hb_cdpStrToU16(hb_vmCDP(), iEndian, pItem->item.asString.value, pItem->item.asString.length, pStrBuffer, nSize);
      } else {
         nSize = hb_cdpStrAsU16Len(hb_vmCDP(), pItem->item.asString.value, pItem->item.asString.length, nSize);
      }
      return nSize;
   } else if( pStrBuffer && nSize ) {
      pStrBuffer[0] = '\0';
   }

   return 0;
}

PHB_ITEM hb_itemPutStrLen(PHB_ITEM pItem, void * cdp, const char * pStr, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStrLen(%p,%p,%p,%" HB_PFS "u)", static_cast<void*>(pItem), cdp, static_cast<const void*>(pStr), nLen));
#endif

   if( nLen == 0 ) {
      return hb_itemPutC(pItem, nullptr);
   }

   char * pszText = hb_cdpnDup(pStr, &nLen, static_cast<PHB_CODEPAGE>(cdp), hb_vmCDP());

   return hb_itemPutCLPtr(pItem, pszText, nLen);
}

PHB_ITEM hb_itemPutStrLenUTF8(PHB_ITEM pItem, const char * pStr, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStrLenUTF8(%p,%p,%" HB_PFS "u)", static_cast<void*>(pItem), static_cast<const void*>(pStr), nLen));
#endif

   if( nLen == 0 ) {
      return hb_itemPutC(pItem, nullptr);
   }

   PHB_CODEPAGE cdp = hb_vmCDP();
   HB_SIZE nDest = hb_cdpUTF8AsStrLen(cdp, pStr, nLen, 0);
   char * pszDest = static_cast<char*>(hb_xgrab(nDest + 1));
   hb_cdpUTF8ToStr(cdp, pStr, nLen, pszDest, nDest + 1);

   return hb_itemPutCLPtr(pItem, pszDest, nDest);
}

PHB_ITEM hb_itemPutStrLenU16(PHB_ITEM pItem, int iEndian, const HB_WCHAR * pStr, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStrLenU16(%p,%d,%p,%" HB_PFS "u)", static_cast<void*>(pItem), iEndian, static_cast<const void*>(pStr), nLen));
#endif

   if( nLen == 0 ) {
      return hb_itemPutC(pItem, nullptr);
   }

   PHB_CODEPAGE cdp = hb_vmCDP();
   HB_SIZE nDest = hb_cdpU16AsStrLen(cdp, pStr, nLen, 0);
   char * pszDest = static_cast<char*>(hb_xgrab(nDest + 1));
   hb_cdpU16ToStr(cdp, iEndian, pStr, nLen, pszDest, nDest + 1);

   return hb_itemPutCLPtr(pItem, pszDest, nDest);
}

PHB_ITEM hb_itemPutStr(PHB_ITEM pItem, void * cdp, const char * pStr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStr(%p,%p,%p)", static_cast<void*>(pItem), cdp, static_cast<const void*>(pStr)));
#endif

   if( pStr == nullptr ) {
      return hb_itemPutC(pItem, nullptr);
   }

   HB_SIZE nLen = strlen(pStr);
   char * pszText = hb_cdpnDup(pStr, &nLen, static_cast<PHB_CODEPAGE>(cdp), hb_vmCDP());

   return hb_itemPutCLPtr(pItem, pszText, nLen);
}

PHB_ITEM hb_itemPutStrUTF8(PHB_ITEM pItem, const char * pStr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStrUTF8(%p,%p)", static_cast<void*>(pItem), static_cast<const void*>(pStr)));
#endif

   if( pStr == nullptr ) {
      return hb_itemPutC(pItem, nullptr);
   }

   PHB_CODEPAGE cdp = hb_vmCDP();
   HB_SIZE nLen = strlen(pStr);
   HB_SIZE nDest = hb_cdpUTF8AsStrLen(cdp, pStr, nLen, 0);
   char * pszDest = static_cast<char*>(hb_xgrab(nDest + 1));
   hb_cdpUTF8ToStr(cdp, pStr, nLen, pszDest, nDest + 1);

   return hb_itemPutCLPtr(pItem, pszDest, nDest);
}

PHB_ITEM hb_itemPutStrU16(PHB_ITEM pItem, int iEndian, const HB_WCHAR * pStr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStrU16(%p,%d,%p)", static_cast<void*>(pItem), iEndian, static_cast<const void*>(pStr)));
#endif

   if( pStr == nullptr ) {
      return hb_itemPutC(pItem, nullptr);
   }

   PHB_CODEPAGE cdp = hb_vmCDP();
   HB_SIZE nLen = hb_wstrlen(pStr);
   HB_SIZE nDest = hb_cdpU16AsStrLen(cdp, pStr, nLen, 0);
   char * pszDest = static_cast<char*>(hb_xgrab(nDest + 1));
   hb_cdpU16ToStr(cdp, iEndian, pStr, nLen, pszDest, nDest + 1);

   return hb_itemPutCLPtr(pItem, pszDest, nDest);
}

const char * hb_arrayGetStr(PHB_ITEM pArray, HB_SIZE nIndex, void * cdp, void ** phString, HB_SIZE * pnLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetStr(%p, %" HB_PFS "u, %p, %p, %p)", static_cast<void*>(pArray), nIndex, cdp, static_cast<void*>(phString), static_cast<void*>(pnLen)));
#endif

   if( HB_IS_ARRAY(pArray) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen ) {
      return hb_itemGetStr(pArray->item.asArray.value->pItems + nIndex - 1, cdp, phString, pnLen);
   }
   if( pnLen ) {
      *pnLen = 0;
   }
   *phString = nullptr;

   return nullptr;
}

const char * hb_arrayGetStrUTF8(PHB_ITEM pArray, HB_SIZE nIndex, void ** phString, HB_SIZE * pnLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetStrUTF8(%p, %" HB_PFS "u, %p, %p)", static_cast<void*>(pArray), nIndex, static_cast<void*>(phString), static_cast<void*>(pnLen)));
#endif

   if( HB_IS_ARRAY(pArray) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen ) {
      return hb_itemGetStrUTF8(pArray->item.asArray.value->pItems + nIndex - 1, phString, pnLen);
   }
   if( pnLen ) {
      *pnLen = 0;
   }
   *phString = nullptr;

   return nullptr;
}

const HB_WCHAR * hb_arrayGetStrU16(PHB_ITEM pArray, HB_SIZE nIndex, int iEndian, void ** phString, HB_SIZE * pnLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetStrU16(%p, %" HB_PFS "u, %d, %p, %p)", static_cast<void*>(pArray), nIndex, iEndian, static_cast<void*>(phString), static_cast<void*>(pnLen)));
#endif

   if( HB_IS_ARRAY(pArray) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen ) {
      return hb_itemGetStrU16(pArray->item.asArray.value->pItems + nIndex - 1, iEndian, phString, pnLen);
   }
   if( pnLen ) {
      *pnLen = 0;
   }
   *phString = nullptr;

   return nullptr;
}

HB_BOOL hb_arraySetStrLen(PHB_ITEM pArray, HB_SIZE nIndex, void * cdp, const char * pStr, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetStrLen(%p, %" HB_PFS "u, %p, %p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex, cdp, static_cast<const void*>(pStr), nLen));
#endif

   if( HB_IS_ARRAY(pArray) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen ) {
      hb_itemPutStrLen(pArray->item.asArray.value->pItems + nIndex - 1, cdp, pStr, nLen);
      return true;
   } else {
      return false;
   }
}

HB_BOOL hb_arraySetStrLenUTF8(PHB_ITEM pArray, HB_SIZE nIndex, const char * pStr, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetStrLenUTF8(%p, %" HB_PFS "u, %p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex, static_cast<const void*>(pStr), nLen));
#endif

   if( HB_IS_ARRAY(pArray) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen ) {
      hb_itemPutStrLenUTF8(pArray->item.asArray.value->pItems + nIndex - 1, pStr, nLen);
      return true;
   } else {
      return false;
   }
}

HB_BOOL hb_arraySetStrLenU16(PHB_ITEM pArray, HB_SIZE nIndex, int iEndian, const HB_WCHAR * pStr, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetStrLenU16(%p, %" HB_PFS "u, %d, %p, %" HB_PFS "u)", static_cast<void*>(pArray), nIndex, iEndian, static_cast<const void*>(pStr), nLen));
#endif

   if( HB_IS_ARRAY(pArray) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen ) {
      hb_itemPutStrLenU16(pArray->item.asArray.value->pItems + nIndex - 1, iEndian, pStr, nLen);
      return true;
   } else {
      return false;
   }
}

HB_BOOL hb_arraySetStr(PHB_ITEM pArray, HB_SIZE nIndex, void * cdp, const char * pStr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetStr(%p, %" HB_PFS "u, %p, %p)", static_cast<void*>(pArray), nIndex, cdp, static_cast<const void*>(pStr)));
#endif

   if( HB_IS_ARRAY(pArray) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen ) {
      hb_itemPutStr(pArray->item.asArray.value->pItems + nIndex - 1, cdp, pStr);
      return true;
   } else {
      return false;
   }
}

HB_BOOL hb_arraySetStrUTF8(PHB_ITEM pArray, HB_SIZE nIndex, const char * pStr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetStrUTF8(%p, %" HB_PFS "u, %p)", static_cast<void*>(pArray), nIndex, static_cast<const void*>(pStr)));
#endif

   if( HB_IS_ARRAY(pArray) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen ) {
      hb_itemPutStrUTF8(pArray->item.asArray.value->pItems + nIndex - 1, pStr);
      return true;
   } else {
      return false;
   }
}

HB_BOOL hb_arraySetStrU16(PHB_ITEM pArray, HB_SIZE nIndex, int iEndian, const HB_WCHAR * pStr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetStrU16(%p, %" HB_PFS "u, %d, %p)", static_cast<void*>(pArray), nIndex, iEndian, static_cast<const void*>(pStr)));
#endif

   if( HB_IS_ARRAY(pArray) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen ) {
      hb_itemPutStrU16(pArray->item.asArray.value->pItems + nIndex - 1, iEndian, pStr);
      return true;
   } else {
      return false;
   }
}

const char * hb_parstr(int iParam, void * cdp, void ** phString, HB_SIZE * pnLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parstr(%d,%p,%p,%p)", iParam, cdp, static_cast<void*>(phString), static_cast<void*>(pnLen)));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() ) {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) ) {
         pItem = hb_itemUnRef(pItem);
      }

      return hb_itemGetStr(pItem, cdp, phString, pnLen);
   }

   if( pnLen ) {
      *pnLen = 0;
   }
   *phString = nullptr;

   return nullptr;
}

const char * hb_parstr_utf8(int iParam, void ** phString, HB_SIZE * pnLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parstr_utf8(%d,%p,%p)", iParam, static_cast<void*>(phString), static_cast<void*>(pnLen)));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() ) {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) ) {
         pItem = hb_itemUnRef(pItem);
      }

      return hb_itemGetStrUTF8(pItem, phString, pnLen);
   }

   if( pnLen ) {
      *pnLen = 0;
   }
   *phString = nullptr;

   return nullptr;
}

const HB_WCHAR * hb_parstr_u16(int iParam, int iEndian, void ** phString, HB_SIZE * pnLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parstr_u16(%d,%d,%p,%p)", iParam, iEndian, static_cast<void*>(phString), static_cast<void*>(pnLen)));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() ) {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) ) {
         pItem = hb_itemUnRef(pItem);
      }

      return hb_itemGetStrU16(pItem, iEndian, phString, pnLen);
   }

   if( pnLen ) {
      *pnLen = 0;
   }
   *phString = nullptr;

   return nullptr;
}

const char * hb_parastr(int iParam, HB_SIZE nIndex, void * cdp, void ** phString, HB_SIZE * pnLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parastr(%d,%" HB_PFS "u,%p,%p,%p)", iParam, nIndex, cdp, static_cast<void*>(phString), static_cast<void*>(pnLen)));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() ) {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) ) {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) ) {
         return hb_arrayGetStr(pItem, nIndex, cdp, phString, pnLen);
      } else {
         return hb_itemGetStr(pItem, cdp, phString, pnLen);
      }
   }

   if( pnLen ) {
      *pnLen = 0;
   }
   *phString = nullptr;

   return nullptr;
}

const char * hb_parastr_utf8(int iParam, HB_SIZE nIndex, void ** phString, HB_SIZE * pnLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parastr_utf8(%d,%" HB_PFS "u,%p,%p)", iParam, nIndex, static_cast<void*>(phString), static_cast<void*>(pnLen)));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() ) {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) ) {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) ) {
         return hb_arrayGetStrUTF8(pItem, nIndex, phString, pnLen);
      } else {
         return hb_itemGetStrUTF8(pItem, phString, pnLen);
      }
   }

   if( pnLen ) {
      *pnLen = 0;
   }
   *phString = nullptr;

   return nullptr;
}

const HB_WCHAR * hb_parastr_u16(int iParam, HB_SIZE nIndex, int iEndian, void ** phString, HB_SIZE * pnLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_parastr_u16(%d,%" HB_PFS "u,%d,%p,%p)", iParam, nIndex, iEndian, static_cast<void*>(phString), static_cast<void*>(pnLen)));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam >= -1 && iParam <= hb_pcount() ) {
      PHB_ITEM pItem = (iParam == -1) ? hb_stackReturnItem() : hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) ) {
         pItem = hb_itemUnRef(pItem);
      }

      if( HB_IS_ARRAY(pItem) ) {
         return hb_arrayGetStrU16(pItem, nIndex, iEndian, phString, pnLen);
      } else {
         return hb_itemGetStrU16(pItem, iEndian, phString, pnLen);
      }
   }

   if( pnLen ) {
      *pnLen = 0;
   }
   *phString = nullptr;

   return nullptr;
}

void hb_retstr(void * cdp, const char * szText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retstr(%p,%s)", cdp, szText));
#endif

   HB_STACK_TLS_PRELOAD
   hb_itemPutStrLen(hb_stackReturnItem(), cdp, szText, szText ? strlen(szText) : 0);
}

void hb_retstr_utf8(const char * szText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retstr_utf8(%s)", szText));
#endif

   HB_STACK_TLS_PRELOAD
   hb_itemPutStrLenUTF8(hb_stackReturnItem(), szText, szText ? strlen(szText) : 0);
}

void hb_retstr_u16(int iEndian, const HB_WCHAR * szText)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retstr_u16(%d,%p)", iEndian, static_cast<const void*>(szText)));
#endif

   HB_STACK_TLS_PRELOAD
   hb_itemPutStrLenU16(hb_stackReturnItem(), iEndian, szText, hb_wstrlen(szText));
}

void hb_retstrlen(void * cdp, const char * szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retstrlen(%p,%s,%" HB_PFS "u)", cdp, szText, nLen));
#endif

   HB_STACK_TLS_PRELOAD
   hb_itemPutStrLen(hb_stackReturnItem(), cdp, szText, nLen);
}

void hb_retstrlen_utf8(const char * szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retstrlen_utf8(%s,%" HB_PFS "u)", szText, nLen));
#endif

   HB_STACK_TLS_PRELOAD
   hb_itemPutStrLenUTF8(hb_stackReturnItem(), szText, nLen);
}

void hb_retstrlen_u16(int iEndian, const HB_WCHAR * szText, HB_SIZE nLen)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_retstrlen_u16(%d,%p,%" HB_PFS "u)", iEndian, static_cast<const void*>(szText), nLen));
#endif

   HB_STACK_TLS_PRELOAD
   hb_itemPutStrLenU16(hb_stackReturnItem(), iEndian, szText, nLen);
}

int hb_storstr(void * cdp, const char * szText, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storstr(%p,%s,%d)", cdp, szText, iParam));
#endif

   HB_STACK_TLS_PRELOAD
   if( iParam == -1 ) {
      hb_itemPutStrLen(hb_stackReturnItem(), cdp, szText, szText ? strlen(szText) : 0);
      return 1;
   } else if( iParam >= 0 && iParam <= hb_pcount() ) {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) ) {
         hb_itemPutStrLen(hb_itemUnRef(pItem), cdp, szText, szText ? strlen(szText) : 0);
         return 1;
      }
   }

   return 0;
}

int hb_storstr_utf8(const char * szText, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storstr_utf8(%s,%d)", szText, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 ) {
      hb_itemPutStrLenUTF8(hb_stackReturnItem(), szText, szText ? strlen(szText) : 0);
      return 1;
   } else if( iParam >= 0 && iParam <= hb_pcount() ) {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) ) {
         hb_itemPutStrLenUTF8(hb_itemUnRef(pItem), szText, szText ? strlen(szText) : 0);
         return 1;
      }
   }

   return 0;
}

int hb_storstr_u16(int iEndian, const HB_WCHAR * szText, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storstr_u16(%d,%p,%d)", iEndian, static_cast<const void*>(szText), iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 ) {
      hb_itemPutStrLenU16(hb_stackReturnItem(), iEndian, szText, hb_wstrlen(szText));
      return 1;
   } else if( iParam >= 0 && iParam <= hb_pcount() ) {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) ) {
         hb_itemPutStrLenU16(hb_itemUnRef(pItem), iEndian, szText, hb_wstrlen(szText));
         return 1;
      }
   }

   return 0;
}

int hb_storstrlen(void * cdp, const char * szText, HB_SIZE nLen, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storstrlen(%p,%s,%" HB_PFS "u,%d)", cdp, szText, nLen, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 ) {
      hb_itemPutStrLen(hb_stackReturnItem(), cdp, szText, nLen);
      return 1;
   } else if( iParam >= 0 && iParam <= hb_pcount() ) {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) ) {
         hb_itemPutStrLen(hb_itemUnRef(pItem), cdp, szText, nLen);
         return 1;
      }
   }

   return 0;
}

int hb_storstrlen_utf8(const char * szText, HB_SIZE nLen, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storstrlen_utf8(%s,%" HB_PFS "u,%d)", szText, nLen, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 ) {
      hb_itemPutStrLenUTF8(hb_stackReturnItem(), szText, nLen);
      return 1;
   } else if( iParam >= 0 && iParam <= hb_pcount() ) {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) ) {
         hb_itemPutStrLenUTF8(hb_itemUnRef(pItem), szText, nLen);
         return 1;
      }
   }

   return 0;
}

int hb_storstrlen_u16(int iEndian, const HB_WCHAR * szText, HB_SIZE nLen, int iParam)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_storstrlen_u16(%d,%p,%" HB_PFS "u,%d)", iEndian, static_cast<const void*>(szText), nLen, iParam));
#endif

   HB_STACK_TLS_PRELOAD

   if( iParam == -1 ) {
      hb_itemPutStrLenU16(hb_stackReturnItem(), iEndian, szText, nLen);
      return 1;
   } else if( iParam >= 0 && iParam <= hb_pcount() ) {
      PHB_ITEM pItem = hb_stackItemFromBase(iParam);

      if( HB_IS_BYREF(pItem) ) {
         hb_itemPutStrLenU16(hb_itemUnRef(pItem), iEndian, szText, nLen);
         return 1;
      }
   }

   return 0;
}
