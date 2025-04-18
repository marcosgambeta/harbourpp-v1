//
// hb_TabExpand() and hb_ReadLine() functions
//
// Copyright 2004 Marcelo Lombardo - lombardo@uol.com.br
//

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
#include <hbapifs.hpp>
#include <hbset.hpp>
#include <hbapiitm.hpp>
#include <hbapierr.hpp>

static void hb_readLine(const char *szText, HB_SIZE nTextLen, HB_SIZE nLineLen, HB_SIZE nTabLen, HB_BOOL bWrap,
                        const char **pTerm, HB_SIZE *pnTermSizes, HB_SIZE nTerms, HB_BOOL *pbFound, HB_BOOL *pbEOF,
                        HB_ISIZ *pnEnd, HB_SIZE *pnEndOffset)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_readLine(%p, %" HB_PFS "u, %" HB_PFS "u, %" HB_PFS "u, %d, %p, %p, %" HB_PFS "u, %p, %p, %p, %p)",
            static_cast<const void*>(szText), nTextLen, nLineLen, nTabLen, bWrap, static_cast<const void*>(pTerm),
            static_cast<void*>(pnTermSizes), nTerms, static_cast<void*>(pbFound), static_cast<void*>(pbEOF), static_cast<void*>(pnEnd), static_cast<void*>(pnEndOffset)));
#endif

  HB_SIZE nPosTerm, nPosition;
  HB_SIZE nPos, nCurrCol, nLastBlk;
  HB_BOOL bBreak = false;

  *pbFound = HB_FALSE;
  *pbEOF = HB_FALSE;
  *pnEnd = 0;
  *pnEndOffset = 0;
  nCurrCol = 0;
  nLastBlk = 0;

  if (nTextLen == 0)
  {
    *pnEnd = -1;
    *pbEOF = HB_TRUE;
    return;
  }

  if (nTabLen == 0)
  {
    nTabLen = 4;
  }

  for (nPos = 0; nPos < nTextLen; nPos++)
  {
    /* Check for line terminators */
    for (nPosTerm = 0; nPosTerm < nTerms; nPosTerm++)
    {
      if (szText[nPos] == pTerm[nPosTerm][0] && (nPos + pnTermSizes[nPosTerm] - 1) < nTextLen)
      {
        *pbFound = HB_TRUE;

        for (nPosition = 1; nPosition < pnTermSizes[nPosTerm]; nPosition++)
        {
          if (pTerm[nPosTerm][nPosition] != szText[nPos + nPosition])
          {
            *pbFound = HB_FALSE;
            break;
          }
        }

        if (*pbFound)
        {
          if (nPos == 0)
          {
            *pnEnd = -1;
            *pnEndOffset = pnTermSizes[nPosTerm];
          }
          else
          {
            *pnEnd = nPos - 1;
            *pnEndOffset = nPos + pnTermSizes[nPosTerm];
          }
          break;
        }
      }
    }

    if (szText[nPos] == HB_CHAR_HT)
    {
      nCurrCol += nTabLen - (nCurrCol % nTabLen);
    }
    else if (szText[nPos] == HB_CHAR_SOFT1 && szText[nPos + 1] == HB_CHAR_SOFT2)
    {
/* Clipper does NOT consider SOFT CR as a word separator - WHY?
   Should we not fix that? */
#if 0
         nLastBlk = nPos;
#endif

      nPos++;
    }
    else
    {
      nCurrCol++;
    }

    if (*pbFound)
    {
      break;
    }

    if (szText[nPos] == ' ' || szText[nPos] == HB_CHAR_HT)
    {
      nLastBlk = nPos;
    }

    if (nCurrCol > nLineLen)
    {
      if (!bWrap || nLastBlk == 0)
      {
        *pnEnd = nPos - 1;
        *pnEndOffset = nPos;
        bBreak = 1;
        break;
      }
      else if (bWrap && nLastBlk != 0)
      {
        *pnEnd = nLastBlk;
        *pnEndOffset = nLastBlk + 1;
        bBreak = 1;
        break;
      }
    }
  }

  if (!*pbFound && !bBreak)
  {
    *pnEnd = nTextLen - 1;
    *pnEndOffset = nTextLen - 1;
    *pbEOF = HB_TRUE;
  }
}

static HB_ISIZ hb_tabexpand(const char *szString, char *szRet, HB_ISIZ nEnd, HB_SIZE nTabLen)
{
  HB_ISIZ nPos, nSpAdded = 0;

  for (nPos = 0; nPos <= nEnd; nPos++)
  {
    if (szString[nPos] == HB_CHAR_HT)
    {
      nSpAdded += (nTabLen > 0 ? nTabLen - ((nPos + nSpAdded) % nTabLen) - 1 : 0);
    }
    else if ((nPos < nEnd && szString[nPos] == HB_CHAR_SOFT1 && szString[nPos + 1] == HB_CHAR_SOFT2) ||
             szString[nPos] == HB_CHAR_LF)
    {
      nSpAdded--;
    }
    else
    {
      *(szRet + nPos + nSpAdded) = *(szString + nPos);
    }
  }

  return nSpAdded + nEnd;
}

HB_FUNC(HB_TABEXPAND)
{
  auto szText = hb_parcx(1);
  HB_ISIZ nStrLen = hb_parclen(1);
  HB_SIZE nTabLen = hb_parns(2);
  HB_SIZE nTabCount = 0;
  HB_ISIZ nPos;

  for (nPos = 0; nPos < nStrLen; nPos++)
  {
    if (szText[nPos] == HB_CHAR_HT)
    {
      ++nTabCount;
    }
  }

  if (nStrLen == 0 || nTabCount == 0 || nTabLen == 0)
  {
    hb_retc(szText);
  }
  else
  {
    HB_ISIZ nSize;
    nSize = nStrLen + nTabCount * (nTabLen - 1);
    auto szRet = static_cast<char *>(hb_xgrab(nSize + 1));
    memset(szRet, ' ', nSize);
    nStrLen = hb_tabexpand(szText, szRet, nStrLen, nTabLen);
    hb_retclen_buffer(szRet, nStrLen);
  }
}

/* hb_ReadLine(<cText>, [<aTerminators | cTerminator>], <nLineLen>, <nTabLen>, <lWrap>, [<nStartOffset>], @nOffSet,
 * @nEnd, @lFound, @lEOF) */
HB_FUNC(HB_READLINE)
{
  PHB_ITEM pTerm1;
  auto szText = hb_parcx(1);
  const char **pTerm;
  HB_SIZE *pnTermSizes;
  HB_SIZE nTerms;
  HB_SIZE nLineSize = hb_parni(3);
  HB_BOOL bWrap = hb_parl(5);
  HB_BOOL bFound, bEOF;
  HB_SIZE nStartOffset;
  HB_SIZE nEndOffset;
  HB_ISIZ nEnd;
  HB_BOOL bAlloc_Term1 = false;

  if (!HB_ISCHAR(1))
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, 9, hb_paramError(1), hb_paramError(2),
                         hb_paramError(3), hb_paramError(4), hb_paramError(5), hb_paramError(6), hb_paramError(7),
                         hb_paramError(8), hb_paramError(9), hb_paramError(10));
    return;
  }

  auto nTextLen = hb_parclen(1);
  auto nTabLen = hb_parclen(4);

  nStartOffset = hb_parns(6);

  if (!(HB_ISARRAY(2) || HB_ISCHAR(2)))
  {
    if (!hb_setGetCPtr(HB_SET_EOL))
    {
      auto pEOL = hb_itemNew(nullptr);
      hb_itemPutC(pEOL, hb_conNewLine());
      hb_setSetItem(HB_SET_EOL, pEOL);
      hb_itemRelease(pEOL);
    }

    pTerm1 = hb_itemPutC(nullptr, hb_setGetCPtr(HB_SET_EOL));
    bAlloc_Term1 = true;
  }
  else
  {
    pTerm1 = hb_param(2, Harbour::Item::ANY);
  }

  auto pOpt = hb_itemNew(nullptr);

  if (pTerm1->isArray())
  {
    HB_SIZE i;

    nTerms = hb_arrayLen(pTerm1);
    pTerm = static_cast<const char **>(hb_xgrab(sizeof(char *) * nTerms));
    pnTermSizes = static_cast<HB_SIZE *>(hb_xgrab(sizeof(HB_SIZE) * nTerms));

    for (i = 0; i < nTerms; i++)
    {
      hb_arrayGet(pTerm1, i + 1, pOpt);
      pTerm[i] = hb_itemGetCPtr(pOpt);
      pnTermSizes[i] = hb_itemGetCLen(pOpt);
    }
  }
  else
  {
    pTerm = static_cast<const char **>(hb_xgrab(sizeof(char *)));
    pnTermSizes = static_cast<HB_SIZE *>(hb_xgrab(sizeof(HB_SIZE) * 1));
    pTerm[0] = hb_itemGetCPtr(pTerm1);
    pnTermSizes[0] = hb_itemGetCLen(pTerm1);
    nTerms = 1;
  }

  hb_itemRelease(pOpt);

  nStartOffset--;

  hb_readLine(szText + nStartOffset, nTextLen - nStartOffset, nLineSize, nTabLen, bWrap, pTerm, pnTermSizes, nTerms,
              &bFound, &bEOF, &nEnd, &nEndOffset);

  hb_storl(bFound, 7);
  hb_storl(bEOF, 8);
  hb_storns(nEnd + nStartOffset + 1, 9);
  hb_storns(nEndOffset + nStartOffset + 1, 10);

  if (bAlloc_Term1)
  {
    hb_itemRelease(pTerm1);
  }

  hb_xfree(static_cast<void *>(pTerm));
  hb_xfree(pnTermSizes);
}
