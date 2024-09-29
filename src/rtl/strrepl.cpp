//
// hb_StrReplace()
//
// Copyright 2013 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
//

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

#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapierr.hpp"

/* hb_StrReplace( <cString>, [ <cSource> | <acSource> | <hReplace> ], [ <cDest> | <acDest> ] )
 *    --> <cResult>
 */
HB_FUNC(HB_STRREPLACE)
{
  auto pText = hb_param(1, Harbour::Item::STRING);
  auto pSrc = hb_param(2, Harbour::Item::STRING | Harbour::Item::ARRAY |
                              (HB_ISNIL(3) ? Harbour::Item::HASH : Harbour::Item::NIL));

  if (pText && pSrc)
  {
    auto nText = hb_itemGetCLen(pText);
    HB_SIZE nSrc = hb_itemSize(pSrc);

    if (nText > 0 && nSrc > 0)
    {
      auto pDst = hb_param(3, Harbour::Item::STRING | Harbour::Item::ARRAY);
      const char *pszDst = pDst && pDst->isString() ? pDst->getCPtr() : nullptr;
      const char *pszSrc = pSrc->isString() ? pSrc->getCPtr() : nullptr;
      auto pszText = pText->getCPtr();
      const char *ptr;
      char *pszResult = nullptr;
      HB_SIZE *ptrOpt = nullptr;
      HB_BOOL fNext = false;
      HB_SIZE nDst, nSize, nPos, nAt, nSkip, nTmp;

      nDst = hb_itemSize(pSrc->isHash() ? pSrc : pDst);
      if (nText > 1024)
      {
        ptrOpt = static_cast<HB_SIZE *>(hb_xgrabz(256 * sizeof(HB_SIZE)));
        for (nAt = 0; nAt < nSrc; ++nAt)
        {
          HB_UCHAR uc;

          if (pszSrc)
          {
            uc = static_cast<HB_UCHAR>(pszSrc[nAt]);
          }
          else
          {
            PHB_ITEM pItem = pSrc->isHash() ? hb_hashGetKeyAt(pSrc, nAt + 1) : hb_arrayGetItemPtr(pSrc, nAt + 1);
            if (hb_itemGetCLen(pItem) == 0)
            {
              continue;
            }
            uc = static_cast<HB_UCHAR>(hb_itemGetCPtr(pItem)[0]);
          }
          if (ptrOpt[uc] == 0)
          {
            ptrOpt[uc] = nAt + 1;
          }
          else if (pszSrc == nullptr)
          {
            fNext = true;
          }
        }
      }

      nSize = nPos = nSkip = 0;
      while (nPos < nText)
      {
        if (ptrOpt)
        {
          nAt = ptrOpt[static_cast<HB_UCHAR>(pszText[nPos])];
          if (nAt == 0 || pszSrc)
          {
            nSkip = 1;
          }
          else
          {
            for (; nAt <= nSrc; ++nAt)
            {
              if (pSrc->isHash())
              {
                pDst = hb_hashGetKeyAt(pSrc, nAt);
                nSkip = hb_itemGetCLen(pDst);
                ptr = hb_itemGetCPtr(pDst);
              }
              else
              {
                nSkip = hb_arrayGetCLen(pSrc, nAt);
                ptr = hb_arrayGetCPtr(pSrc, nAt);
              }
              if (nSkip > 0 && nSkip <= nText - nPos && memcmp(pszText + nPos, ptr, nSkip) == 0)
              {
                break;
              }
              if (!fNext)
              {
                nAt = nSrc;
              }
            }
            if (nAt > nSrc)
            {
              nAt = 0;
              nSkip = 1;
            }
          }
        }
        else if (pszSrc)
        {
          ptr = static_cast<const char *>(memchr(pszSrc, static_cast<HB_UCHAR>(pszText[nPos]), nSrc));
          nAt = ptr ? ptr - pszSrc + 1 : 0;
          nSkip = 1;
        }
        else
        {
          for (nAt = 1; nAt <= nSrc; ++nAt)
          {
            if (pSrc->isHash())
            {
              pDst = hb_hashGetKeyAt(pSrc, nAt);
              nSkip = hb_itemGetCLen(pDst);
              ptr = hb_itemGetCPtr(pDst);
            }
            else
            {
              nSkip = hb_arrayGetCLen(pSrc, nAt);
              ptr = hb_arrayGetCPtr(pSrc, nAt);
            }
            if (nSkip > 0 && nSkip <= nText - nPos && memcmp(pszText + nPos, ptr, nSkip) == 0)
            {
              break;
            }
          }
          if (nAt > nSrc)
          {
            nAt = 0;
            nSkip = 1;
          }
        }

        if (pszResult)
        {
          if (nAt != 0)
          {
            if (nAt <= nDst)
            {
              if (pszDst)
              {
                pszResult[nSize++] = pszDst[nAt - 1];
              }
              else
              {
                if (pSrc->isHash())
                {
                  pDst = hb_hashGetValueAt(pSrc, nAt);
                  nTmp = hb_itemGetCLen(pDst);
                  ptr = hb_itemGetCPtr(pDst);
                }
                else
                {
                  nTmp = hb_arrayGetCLen(pDst, nAt);
                  ptr = hb_arrayGetCPtr(pDst, nAt);
                }
                memcpy(&pszResult[nSize], ptr, nTmp);
                nSize += nTmp;
              }
            }
          }
          else
          {
            pszResult[nSize++] = pszText[nPos];
          }
          nPos += nSkip;
        }
        else
        {
          if (nAt != 0)
          {
            if (nAt <= nDst)
            {
              if (pszDst)
              {
                nSize++;
              }
              else if (pSrc->isHash())
              {
                nSize += hb_itemGetCLen(hb_hashGetValueAt(pSrc, nAt));
              }
              else
              {
                nSize += hb_arrayGetCLen(pDst, nAt);
              }
            }
          }
          else
          {
            nSize++;
          }
          nPos += nSkip;
          if (nPos == nText)
          {
            pszResult = static_cast<char *>(hb_xgrab(nSize + 1));
            nSize = nPos = 0;
          }
        }
      }
      if (ptrOpt)
      {
        hb_xfree(ptrOpt);
      }
      hb_retclen_buffer(pszResult, nSize);
    }
    else
    {
      hb_itemReturn(pText);
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}
