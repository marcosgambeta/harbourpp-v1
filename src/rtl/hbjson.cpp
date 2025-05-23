//
// JavaScript Object Notation (JSON)
//
// Copyright 2010 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>
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

#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapistr.hpp"
#include "hbset.hpp"
#include "hbjson.h"

// The application/json Media Type for JavaScript Object Notation (JSON)
// https://tools.ietf.org/html/rfc4627
//
//    C level functions:
//      char * hb_jsonEncode( PHB_ITEM pValue, HB_SIZE * pnLen, int iIndent );
//         pValue  - value to encode;
//         pnLen   - if pnLen is not nullptr, length of returned buffer is
//                   stored to *pnLen;
//         iIndent - indenting to be human readable;
//         returns pointer to encoded JSON buffer. buffer must be fried
//            by the caller.
//
//      HB_SIZE hb_jsonDecode( const char * szSource, PHB_ITEM pValue );
//         szSource - JSON source;
//         pValue   - item to store decoded value. Item value is
//                    undetermined in case of error;
//         returns number of bytes decoded from the buffer. This allows
//            to use the remaining part of the buffer for some other
//            purposes. Returns 0 on error.
//
//    Harbour level functions:
//      hb_jsonEncode( xValue [, lHuman = .F. | nIndent = 0 ] ) --> cJSON
//      hb_jsonDecode( cJSON ) --> xValue
//      hb_jsonDecode( cJSON, @xValue ) --> nLengthDecoded
//
//    Note:
//      - JSON encode functions are safe for recursive arrays and hashes.
//        Recursive part of array or hash will be stored as null. JSON
//        encoder still allows to use same structure in the leaves, in
//        this case content will be duplicate.
//        I.e.:
//           xI := { 1, NIL }
//           xI[2] := xI
//           ? hb_jsonEncode( xI )  // [1,null]
//        but:
//           xI := { 1, .T. }
//           xI := { 2, xI, xI }
//           ? hb_jsonEncode( xI )  // [2,[1,true],[1,true]]

struct HB_JSON_ENCODE_CTX
{
  char *pBuffer;
  char *pHead;
  HB_SIZE nAlloc;
  void **pId;
  HB_SIZE nAllocId;
  int iIndent;
  int iEolLen;
  const char *szEol;
};

using PHB_JSON_ENCODE_CTX = HB_JSON_ENCODE_CTX *;

#define INDENT_SIZE 2

static void _hb_jsonCtxAdd(PHB_JSON_ENCODE_CTX pCtx, const char *szString, HB_SIZE nLen)
{
  if (pCtx->pHead + nLen >= pCtx->pBuffer + pCtx->nAlloc)
  {
    HB_SIZE nSize = pCtx->pHead - pCtx->pBuffer;
    pCtx->nAlloc += (pCtx->nAlloc << 1) + nLen;
    pCtx->pBuffer = static_cast<char *>(hb_xrealloc(pCtx->pBuffer, pCtx->nAlloc));
    pCtx->pHead = pCtx->pBuffer + nSize;
  }
  if (szString != nullptr)
  {
    hb_xmemcpy(pCtx->pHead, szString, nLen);
    pCtx->pHead += nLen;
  }
}

static void _hb_jsonCtxAddIndent(PHB_JSON_ENCODE_CTX pCtx, HB_SIZE nLevel)
{
  if (nLevel > 0)
  {
    HB_SIZE nCount = nLevel * (pCtx->iIndent > 0 ? pCtx->iIndent : 1);
    if (pCtx->pHead + nCount >= pCtx->pBuffer + pCtx->nAlloc)
    {
      HB_SIZE nSize = pCtx->pHead - pCtx->pBuffer;
      pCtx->nAlloc += (pCtx->nAlloc << 1) + nCount;
      pCtx->pBuffer = static_cast<char *>(hb_xrealloc(pCtx->pBuffer, pCtx->nAlloc));
      pCtx->pHead = pCtx->pBuffer + nSize;
    }
    hb_xmemset(pCtx->pHead, pCtx->iIndent > 0 ? ' ' : '\t', nCount);
    pCtx->pHead += nCount;
  }
}

static void _hb_jsonEncode(PHB_ITEM pValue, PHB_JSON_ENCODE_CTX pCtx, HB_SIZE nLevel, bool fEOL, PHB_CODEPAGE cdp)
{
  // Protection against recursive structures
  if ((pValue->isArray() || pValue->isHash()) && hb_itemSize(pValue) > 0)
  {
    void *id = pValue->isHash() ? hb_hashId(pValue) : hb_arrayId(pValue);

    for (HB_SIZE nIndex = 0; nIndex < nLevel; nIndex++)
    {
      if (pCtx->pId[nIndex] == id)
      {
        if (!fEOL && pCtx->iIndent)
        {
          _hb_jsonCtxAddIndent(pCtx, nLevel);
        }
        _hb_jsonCtxAdd(pCtx, "null", 4);
        return;
      }
    }
    if (nLevel >= pCtx->nAllocId)
    {
      pCtx->nAllocId += 8;
      pCtx->pId = static_cast<void **>(hb_xrealloc(pCtx->pId, sizeof(void *) * pCtx->nAllocId));
    }
    pCtx->pId[nLevel] = id;
  }

  if (fEOL)
  {
    --pCtx->pHead;
    _hb_jsonCtxAdd(pCtx, pCtx->szEol, pCtx->iEolLen);
  }

  if (pValue->isString())
  {
    const char *szString;
    HB_SIZE nLen;
    void *hString = nullptr;
    char buf[8];

    if (cdp)
    {
      szString = hb_itemGetStr(pValue, cdp, &hString, &nLen);
    }
    else
    {
      szString = pValue->getCPtr();
      nLen = pValue->getCLen();
    }

    _hb_jsonCtxAdd(pCtx, "\"", 1);
    HB_SIZE nPos = 0;
    while (nPos < nLen)
    {
      unsigned char uch = szString[nPos];
      HB_SIZE nPos2 = nPos;
      while (uch >= ' ' && uch != '\\' && uch != '\"')
      {
        uch = szString[++nPos2];
      }
      if (nPos2 > nPos)
      {
        _hb_jsonCtxAdd(pCtx, szString + nPos, nPos2 - nPos);
        if (nPos2 >= nLen)
        {
          break;
        }
        nPos = nPos2;
      }

      switch (uch)
      {
      case '\\':
        _hb_jsonCtxAdd(pCtx, "\\\\", 2);
        break;
      case '\"':
        _hb_jsonCtxAdd(pCtx, "\\\"", 2);
        break;
      case '\b':
        _hb_jsonCtxAdd(pCtx, "\\b", 2);
        break;
      case '\f':
        _hb_jsonCtxAdd(pCtx, "\\f", 2);
        break;
      case '\n':
        _hb_jsonCtxAdd(pCtx, "\\n", 2);
        break;
      case '\r':
        _hb_jsonCtxAdd(pCtx, "\\r", 2);
        break;
      case '\t':
        _hb_jsonCtxAdd(pCtx, "\\t", 2);
        break;
      default:
        hb_snprintf(buf, sizeof(buf), "\\u00%02X", uch);
        _hb_jsonCtxAdd(pCtx, buf, 6);
        break;
      }
      nPos++;
    }
    _hb_jsonCtxAdd(pCtx, "\"", 1);
    hb_strfree(hString);
  }
  else if (pValue->isNumInt())
  {
    char buf[24];
    HB_MAXINT nVal = pValue->getNInt();
    bool fNeg = nVal < 0;
    int i = 0;

    if (fNeg)
    {
      nVal = -nVal;
    }
    do
    {
      buf[sizeof(buf) - ++i] = (nVal % 10) + '0';
    } while ((nVal /= 10) != 0);
    if (fNeg)
    {
      buf[sizeof(buf) - ++i] = '-';
    }
    _hb_jsonCtxAdd(pCtx, &buf[sizeof(buf) - i], i);
  }
  else if (pValue->isNumeric())
  {
    char buf[64];
    int iDec;
    double dblValue = hb_itemGetNDDec(pValue, &iDec);
    hb_snprintf(buf, sizeof(buf), "%.*f", iDec, dblValue);
    _hb_jsonCtxAdd(pCtx, buf, strlen(buf));
  }
  else if (pValue->isNil())
  {
    _hb_jsonCtxAdd(pCtx, "null", 4);
  }
  else if (pValue->isLogical())
  {
    if (pValue->getL())
    {
      _hb_jsonCtxAdd(pCtx, "true", 4);
    }
    else
    {
      _hb_jsonCtxAdd(pCtx, "false", 5);
    }
  }
  else if (pValue->isDate())
  {
    char szBuffer[10];
    pValue->getDS(szBuffer + 1);
    szBuffer[0] = '\"';
    szBuffer[9] = '\"';
    _hb_jsonCtxAdd(pCtx, szBuffer, 10);
  }
  else if (pValue->isTimeStamp())
  {
    char szBuffer[19];
    hb_itemGetTS(pValue, szBuffer + 1);
    szBuffer[0] = '\"';
    szBuffer[18] = '\"';
    _hb_jsonCtxAdd(pCtx, szBuffer, 19);
  }
  else if (pValue->isArray())
  {
    HB_SIZE nLen = hb_itemSize(pValue);

    if (nLen)
    {
      if (pCtx->iIndent)
      {
        _hb_jsonCtxAddIndent(pCtx, nLevel);
      }

      _hb_jsonCtxAdd(pCtx, "[", 1);

      for (HB_SIZE nIndex = 1; nIndex <= nLen; nIndex++)
      {
        auto pItem = hb_arrayGetItemPtr(pValue, nIndex);

        if (nIndex > 1)
        {
          _hb_jsonCtxAdd(pCtx, ",", 1);
        }

        if (pCtx->iIndent)
        {
          _hb_jsonCtxAdd(pCtx, pCtx->szEol, pCtx->iEolLen);
        }

        if (pCtx->iIndent && !((pItem->isArray() || pItem->isHash()) && hb_itemSize(pItem) > 0))
        {
          _hb_jsonCtxAddIndent(pCtx, (nLevel + 1));
        }

        _hb_jsonEncode(pItem, pCtx, nLevel + 1, false, cdp);
      }
      if (pCtx->iIndent)
      {
        _hb_jsonCtxAdd(pCtx, pCtx->szEol, pCtx->iEolLen);
        _hb_jsonCtxAddIndent(pCtx, nLevel);
      }
      _hb_jsonCtxAdd(pCtx, "]", 1);
    }
    else
    {
      _hb_jsonCtxAdd(pCtx, "[]", 2);
    }
  }
  else if (pValue->isHash())
  {
    HB_SIZE nLen = hb_hashLen(pValue);

    if (nLen)
    {
      if (pCtx->iIndent)
      {
        _hb_jsonCtxAddIndent(pCtx, nLevel);
      }

      _hb_jsonCtxAdd(pCtx, "{", 1);

      for (HB_SIZE nIndex = 1; nIndex <= nLen; nIndex++)
      {
        auto pKey = hb_hashGetKeyAt(pValue, nIndex);

        if (pKey->isString())
        {
          auto pItem = hb_hashGetValueAt(pValue, nIndex);

          if (nIndex > 1)
          {
            _hb_jsonCtxAdd(pCtx, ",", 1);
          }

          if (pCtx->iIndent)
          {
            _hb_jsonCtxAdd(pCtx, pCtx->szEol, pCtx->iEolLen);
            _hb_jsonCtxAddIndent(pCtx, (nLevel + 1));
          }
          _hb_jsonEncode(pKey, pCtx, nLevel + 1, false, cdp);

          if (pCtx->iIndent)
          {
            _hb_jsonCtxAdd(pCtx, ": ", 2);
            fEOL = (pItem->isArray() || pItem->isHash()) && hb_itemSize(pItem) > 0;
          }
          else
          {
            _hb_jsonCtxAdd(pCtx, ":", 1);
            fEOL = false;
          }

          _hb_jsonEncode(pItem, pCtx, nLevel + 1, fEOL, cdp);
        }
      }
      if (pCtx->iIndent)
      {
        _hb_jsonCtxAdd(pCtx, pCtx->szEol, pCtx->iEolLen);
        _hb_jsonCtxAddIndent(pCtx, nLevel);
      }
      _hb_jsonCtxAdd(pCtx, "}", 1);
    }
    else
    {
      _hb_jsonCtxAdd(pCtx, "{}", 2);
    }
  }
  else
  {
    // All unsupported types are replaced by null
    _hb_jsonCtxAdd(pCtx, "null", 4);
  }
}

static const char *_skipws(const char *szSource)
{
  while (*szSource == ' ' || *szSource == '\t' || *szSource == '\n' || *szSource == '\r')
  {
    szSource++;
  }
  return szSource;
}

static const char *_hb_jsonDecode(const char *szSource, PHB_ITEM pValue, PHB_CODEPAGE cdp)
{
  if (*szSource == '\"')
  {
    char *szDest, *szHead;
    HB_SIZE nAlloc = 16;

    szHead = szDest = static_cast<char *>(hb_xgrab(nAlloc));
    szSource++;
    while (*szSource != '\"')
    {
      if (szHead + 6 >= szDest + nAlloc)
      {
        HB_SIZE nLen = szHead - szDest;
        nAlloc += nAlloc << 1;
        szDest = static_cast<char *>(hb_xrealloc(szDest, nAlloc));
        szHead = szDest + nLen;
      }
      if (*szSource == '\\')
      {
        szSource++;
        switch (*szSource)
        {
        case '\"':
          *szHead++ = '\"';
          break;
        case '\\':
          *szHead++ = '\\';
          break;
        case '/':
          *szHead++ = '/';
          break;
        case 'b':
          *szHead++ = '\b';
          break;
        case 'f':
          *szHead++ = '\f';
          break;
        case 'n':
          *szHead++ = '\n';
          break;
        case 'r':
          *szHead++ = '\r';
          break;
        case 't':
          *szHead++ = '\t';
          break;
        case 'u':
        {
          HB_WCHAR wc = 0;

          for (auto i = 0; i < 4; i++)
          {
            char c = *++szSource;
            wc <<= 4;
            if (c >= '0' && c <= '9')
            {
              wc += c - '0';
            }
            else if (c >= 'A' && c <= 'F')
            {
              wc += c - 'A' + 10;
            }
            else if (c >= 'a' && c <= 'f')
            {
              wc += c - 'a' + 10;
            }
            else
            {
              hb_xfree(szDest);
              return nullptr;
            }
          }
          szHead +=
              hb_cdpU16ToStr(cdp ? cdp : hb_vmCDP(), HB_CDP_ENDIAN_NATIVE, &wc, 1, szHead, szDest + nAlloc - szHead);
          break;
        }
        default:
          hb_xfree(szDest);
          return nullptr;
        }
        szSource++;
      }
      else if (*reinterpret_cast<const unsigned char *>(szSource) >= ' ')
      {
        *szHead++ = *szSource++;
      }
      else
      {
        hb_xfree(szDest);
        return nullptr;
      }
    }
    if (cdp && hb_vmCDP() != cdp)
    {
      hb_itemPutStrLen(pValue, cdp, szDest, szHead - szDest);
    }
    else
    {
      hb_itemPutCL(pValue, szDest, szHead - szDest);
    }
    hb_xfree(szDest);
    return szSource + 1;
  }
  else if (*szSource == '-' || (*szSource >= '0' && *szSource <= '9'))
  {
    // NOTE: this function is much less strict to number format than
    //       JSON syntax definition. This is allowed behaviour [Mindaugas]
    HB_MAXINT nValue = 0;
    double dblValue = 0;
    auto fDbl = false;
    int iDec = 0;

    bool fNeg = *szSource == '-';
    if (fNeg)
    {
      szSource++;
    }

    while (*szSource >= '0' && *szSource <= '9')
    {
      nValue = nValue * 10 + *szSource - '0';
      szSource++;
    }
    if (*szSource == '.')
    {
      double mult = 1;
      dblValue = static_cast<double>(nValue);
      fDbl = true;
      szSource++;
      while (*szSource >= '0' && *szSource <= '9')
      {
        mult /= 10;
        dblValue += (static_cast<double>(*szSource - '0')) * mult;
        szSource++;
        iDec++;
      }
    }
    if (*szSource == 'e' || *szSource == 'E')
    {
      int iExp = 0;

      szSource++;
      bool fNegExp = *szSource == '-';
      if (fNegExp)
      {
        szSource++;
      }

      while (*szSource >= '0' && *szSource <= '9')
      {
        iExp = iExp * 10 + *szSource - '0';
        szSource++;
      }
      if (!fDbl)
      {
        dblValue = static_cast<double>(nValue);
        fDbl = true;
      }
      if (fNegExp)
      {
        iDec += iExp;
      }
      dblValue = hb_numExpConv(dblValue, fNegExp ? iExp : -iExp);
    }

    if (fDbl)
    {
      hb_itemPutNDDec(pValue, hb_numRound(fNeg ? -dblValue : dblValue, iDec), iDec);
    }
    else
    {
      hb_itemPutNInt(pValue, fNeg ? -nValue : nValue);
    }
    return szSource;
  }
  else if (!strncmp(szSource, "null", 4))
  {
    hb_itemClear(pValue);
    return szSource + 4;
  }
  else if (!strncmp(szSource, "true", 4))
  {
    hb_itemPutL(pValue, true);
    return szSource + 4;
  }
  else if (!strncmp(szSource, "false", 5))
  {
    hb_itemPutL(pValue, false);
    return szSource + 5;
  }
  else if (*szSource == '[')
  {
    hb_arrayNew(pValue, 0);
    szSource = _skipws(szSource + 1);
    if (*szSource != ']')
    {
      auto pItem = hb_itemNew(nullptr);

      for (;;)
      {
        szSource = _hb_jsonDecode(szSource, pItem, cdp);
        if (!szSource)
        {
          hb_itemRelease(pItem);
          return nullptr;
        }
        hb_arrayAddForward(pValue, pItem);

        szSource = _skipws(szSource);
        if (*szSource == ',')
        {
          szSource = _skipws(szSource + 1);
          continue;
        }
        else if (*szSource == ']')
        {
          break;
        }
        else
        {
          hb_itemRelease(pItem);
          return nullptr;
        }
      }
      hb_itemRelease(pItem);
    }
    return szSource + 1;
  }
  else if (*szSource == '{')
  {
    hb_hashNew(pValue);
    szSource = _skipws(szSource + 1);
    if (*szSource != '}')
    {
      auto pItemKey = hb_itemNew(nullptr);
      auto pItemValue = hb_itemNew(nullptr);

      for (;;)
      {
        // Do we need to check if key does not exist yet?
        if ((szSource = _hb_jsonDecode(szSource, pItemKey, cdp)) == nullptr || !pItemKey->isString() ||
            *(szSource = _skipws(szSource)) != ':' ||
            (szSource = _hb_jsonDecode(_skipws(szSource + 1), pItemValue, cdp)) == nullptr)
        {
          hb_itemRelease(pItemKey);
          hb_itemRelease(pItemValue);
          return nullptr;
        }

        hb_hashAdd(pValue, pItemKey, pItemValue);
        szSource = _skipws(szSource);
        if (*szSource == ',')
        {
          szSource = _skipws(szSource + 1);
          continue;
        }
        else if (*szSource == '}')
        {
          break;
        }
        else
        {
          hb_itemRelease(pItemKey);
          hb_itemRelease(pItemValue);
          return nullptr;
        }
      }
      hb_itemRelease(pItemKey);
      hb_itemRelease(pItemValue);
    }
    return szSource + 1;
  }
  return nullptr;
}

// C level API functions

char *hb_jsonEncodeCP(PHB_ITEM pValue, HB_SIZE *pnLen, int iIndent, PHB_CODEPAGE cdp)
{
  auto pCtx = static_cast<PHB_JSON_ENCODE_CTX>(hb_xgrab(sizeof(HB_JSON_ENCODE_CTX)));
  pCtx->nAlloc = 16;
  pCtx->pHead = pCtx->pBuffer = static_cast<char *>(hb_xgrab(pCtx->nAlloc));
  pCtx->nAllocId = 8;
  pCtx->pId = static_cast<void **>(hb_xgrab(sizeof(void *) * pCtx->nAllocId));
  pCtx->iIndent = iIndent;
  pCtx->szEol = hb_setGetEOL();
  if (!pCtx->szEol || !pCtx->szEol[0])
  {
    pCtx->szEol = hb_conNewLine();
  }
  pCtx->iEolLen = static_cast<int>(strlen(pCtx->szEol));

  _hb_jsonEncode(pValue, pCtx, 0, false, cdp);
  if (iIndent)
  {
    _hb_jsonCtxAdd(pCtx, pCtx->szEol, pCtx->iEolLen);
  }

  HB_SIZE nLen = pCtx->pHead - pCtx->pBuffer;
  auto szRet = static_cast<char *>(hb_xrealloc(pCtx->pBuffer, nLen + 1));
  szRet[nLen] = '\0';
  hb_xfree(pCtx->pId);
  hb_xfree(pCtx);
  if (pnLen)
  {
    *pnLen = nLen;
  }
  return szRet;
}

char *hb_jsonEncode(PHB_ITEM pValue, HB_SIZE *pnLen, int iIndent)
{
  return hb_jsonEncodeCP(pValue, pnLen, iIndent, nullptr);
}

HB_SIZE hb_jsonDecodeCP(const char *szSource, PHB_ITEM pValue, PHB_CODEPAGE cdp)
{
  PHB_ITEM pItem = pValue ? pValue : hb_itemNew(nullptr);

  const char *sz = szSource ? _hb_jsonDecode(_skipws(szSource), pItem, cdp) : nullptr;
  if (!pValue)
  {
    hb_itemRelease(pItem);
  }
  if (sz != nullptr)
  {
    return sz - szSource;
  }
  return 0;
}

HB_SIZE hb_jsonDecode(const char *szSource, PHB_ITEM pValue)
{
  return hb_jsonDecodeCP(szSource, pValue, nullptr);
}

// Harbour level API functions

static PHB_CODEPAGE _hb_jsonCdpPar(int iParam)
{
  if (hb_pcount() >= iParam)
  {
    auto szCdp = hb_parc(iParam);

    if (szCdp != nullptr)
    {
      return hb_cdpFindExt(szCdp);
    }
  }
  return nullptr;
}

HB_FUNC(HB_JSONENCODE)
{
  auto pItem = hb_param(1, Harbour::Item::ANY);

  if (pItem != nullptr)
  {
    HB_SIZE nLen;
    int iIndent = hb_parl(2) ? INDENT_SIZE : hb_parni(2);
    char *szRet = hb_jsonEncodeCP(pItem, &nLen, iIndent, _hb_jsonCdpPar(3));
    hb_retclen_buffer(szRet, nLen);
  }
}

HB_FUNC(HB_JSONDECODE)
{
  auto pItem = hb_itemNew(nullptr);
  HB_SIZE nSize = hb_jsonDecodeCP(hb_parc(1), pItem, _hb_jsonCdpPar(3));

  if (HB_ISBYREF(2))
  {
    hb_retns(static_cast<HB_ISIZ>(nSize));
    hb_itemParamStoreForward(2, pItem);
    hb_itemRelease(pItem);
  }
  else
  {
    hb_itemReturnRelease(pItem);
  }
}
