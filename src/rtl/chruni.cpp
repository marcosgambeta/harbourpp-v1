//
// Binary and unicode string functions:
//    hb_UChar(), hb_UCode(), hb_ULen(), hb_UPeek(), hb_UPoke()
//    hb_BChar(), hb_BCode(), hb_BLen(), hb_BPeek(), hb_BPoke()
//
// Copyright 2012 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
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
#include "hbapicdp.hpp"
#include "hbapiitm.hpp"
#include "hbapierr.hpp"

// hb_UChar(<nCode>) --> <cText>
// return string with U+nCode character in HVM CP encoding
HB_FUNC(HB_UCHAR)
{
  if (HB_ISNUM(1)) {
    char szChar[HB_MAX_CHAR_LEN];
    HB_SIZE nLen = hb_cdpTextPutU16(hb_vmCDP(), szChar, sizeof(szChar), static_cast<HB_WCHAR>(hb_parni(1)));
    hb_retclen(szChar, nLen);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1111, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_BChar(<nCode>) --> <cText>
// return 1 byte string with <nCode> value
HB_FUNC(HB_BCHAR)
{
  if (HB_ISNUM(1)) {
    auto c = static_cast<char>(hb_parni(1));
    hb_retclen(&c, 1);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1111, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_UCode(<cText>) --> <nCode>
// return unicode value of 1st character (not byte) in given string
HB_FUNC(HB_UCODE)
{
  auto pText = hb_param(1, Harbour::Item::STRING);

  if (pText) {
    hb_retni(hb_cdpTextGetU16(hb_vmCDP(), pText->getCPtr(), pText->getCLen()));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1111, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_BCode(<cText>) --> <nCode>
// return value of 1st byte in given string
HB_FUNC(HB_BCODE)
{
  auto szText = hb_parc(1);

  if (szText != nullptr) {
    hb_retni(static_cast<HB_UCHAR>(szText[0]));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1111, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_ULen(<cText>) --> <nChars>
// return string length in characters
HB_FUNC(HB_ULEN)
{
  auto pText = hb_param(1, Harbour::Item::STRING);

  if (pText) {
    hb_retns(hb_cdpTextLen(hb_vmCDP(), pText->getCPtr(), pText->getCLen()));
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1111, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_BLen(<cText>) --> <nBytes>
// return string length in bytes
HB_FUNC(HB_BLEN)
{
  auto pText = hb_param(1, Harbour::Item::STRING);

  if (pText) {
    hb_retns(pText->getCLen());
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1111, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_UPeek(<cText>, <n>) --> <nCode>
// return unicode value of <n>th character in given string
HB_FUNC(HB_UPEEK)
{
  auto pText = hb_param(1, Harbour::Item::STRING);

  if (pText && HB_ISNUM(2)) {
    auto cdp = hb_vmCDP();
    auto szText = pText->getCPtr();
    auto nLen = pText->getCLen();
    HB_SIZE nPos = hb_parns(2);
    HB_WCHAR wc = 0;

    if (nPos > 0 && nPos <= nLen) {
      nPos = hb_cdpTextPos(cdp, szText, nLen, nPos - 1);
      nLen -= nPos;
      if (nLen > 0) {
        wc = hb_cdpTextGetU16(cdp, szText + nPos, nLen);
      }
    }

    hb_retni(wc);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1111, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_BPeek(<cText>, <n>) --> <nCode>
// return value of <n>th byte in given string
HB_FUNC(HB_BPEEK)
{
  auto pText = hb_param(1, Harbour::Item::STRING);

  if (pText && HB_ISNUM(2)) {
    HB_SIZE nPos = hb_parns(2);
    hb_retni((nPos > 0 && nPos <= pText->getCLen()) ? static_cast<HB_UCHAR>(pText->getCPtr()[nPos - 1]) : 0);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1111, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_UPoke( [@]<cText>, <n>, <nVal> ) --> <cText>
// change <n>th character in given string to unicode <nVal> one and return modified text
HB_FUNC(HB_UPOKE)
{
  auto pText = hb_param(1, Harbour::Item::STRING);

  if (pText && HB_ISNUM(2) && HB_ISNUM(3)) {
    auto cdp = hb_vmCDP();
    auto szText = pText->getCPtr();
    auto nLen = pText->getCLen();
    HB_SIZE nPos = hb_parns(2);

    if (nPos > 0 && nPos <= nLen) {
      nPos = hb_cdpTextPos(cdp, szText, nLen, nPos - 1);
      if (nPos < nLen) {
        char szChar[HB_MAX_CHAR_LEN], *pszText;
        HB_SIZE nChar = hb_cdpTextPutU16(cdp, szChar, sizeof(szChar), static_cast<HB_WCHAR>(hb_parni(3)));
        HB_SIZE nOldChar = hb_cdpTextPos(cdp, szText + nPos, nLen - nPos, 1);
        if (nChar == nOldChar) {
          if (hb_itemGetWriteCL(pText, &pszText, &nLen) && nPos + nChar <= nLen) {
            memcpy(pszText + nPos, szChar, nChar);
          }
        } else {
          pszText = static_cast<char *>(hb_xgrab(nLen - nOldChar + nChar + 1));
          memcpy(pszText, szText, nPos);
          memcpy(pszText + nPos, szChar, nChar);
          memcpy(pszText + nPos + nChar, szText + nPos + nOldChar, nLen - nPos - nOldChar);
          if (HB_ISBYREF(1)) {
            hb_storclen(pszText, nLen - nOldChar + nChar, 1);
          }
          hb_retclen_buffer(pszText, nLen - nOldChar + nChar);
          return;
        }
      }
    }
    hb_itemReturn(pText);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1111, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_BPoke( [@]<cText>, <n>, <nVal> ) --> <cText>
// change <n>th byte in given string to <nVal> and return modified text
HB_FUNC(HB_BPOKE)
{
  auto pText = hb_param(1, Harbour::Item::STRING);

  if (pText && HB_ISNUM(2) && HB_ISNUM(3)) {
    HB_SIZE nPos = hb_parns(2), nLen;
    char *pszText;

    if (nPos > 0 && hb_itemGetWriteCL(pText, &pszText, &nLen) && nPos <= nLen) {
      pszText[nPos - 1] = static_cast<char>(hb_parni(3) & 0xff);
    }
    hb_itemReturn(pText);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1111, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_USubStr(<cString>, <nStart>, <nCount>) --> <cSubstring>
HB_FUNC(HB_USUBSTR)
{
  auto pText = hb_param(1, Harbour::Item::STRING);
  auto iPCount = hb_pcount();

  if (pText && HB_ISNUM(2) && (iPCount < 3 || HB_ISNUM(3))) {
    auto cdp = hb_vmCDP();
    auto pszText = pText->getCPtr();
    HB_ISIZ nSize = pText->getCLen();
    HB_ISIZ nFrom = hb_parns(2);
    HB_ISIZ nCount = iPCount < 3 ? nSize : hb_parns(3);

    if (nFrom > 0) {
      if (--nFrom > nSize) {
        nCount = 0;
      }
    }

    if (nCount > 0) {
      if (nFrom < 0) {
        nFrom += hb_cdpTextLen(cdp, pszText, nSize);
      }
      if (nFrom > 0) {
        nFrom = hb_cdpTextPos(cdp, pszText, nSize, nFrom);
        pszText += nFrom;
        nSize -= nFrom;
      }
      nCount = hb_cdpTextPos(cdp, pszText, nSize, nCount);
    }

    if (nCount > 0) {
      if (nFrom <= 0 && nCount == nSize) {
        hb_itemReturn(pText);
      } else {
        hb_retclen(pszText, nCount);
      }
    } else {
      hb_retc_null();
    }
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1110, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_BSubStr(<cString>, <nStart>, <nCount>) --> <cSubstring>
HB_FUNC(HB_BSUBSTR)
{
  auto pText = hb_param(1, Harbour::Item::STRING);
  auto iPCount = hb_pcount();

  if (pText && HB_ISNUM(2) && (iPCount < 3 || HB_ISNUM(3))) {
    auto pszText = pText->getCPtr();
    HB_ISIZ nSize = pText->getCLen();
    HB_ISIZ nFrom = hb_parns(2);
    HB_ISIZ nCount = iPCount < 3 ? nSize : hb_parns(3);

    if (nFrom > 0) {
      if (--nFrom > nSize) {
        nCount = 0;
      }
    }
    if (nCount > 0) {
      if (nFrom < 0) {
        nFrom += nSize;
      }
      if (nFrom > 0) {
        pszText += nFrom;
        nSize -= nFrom;
      }
      if (nCount > nSize) {
        nCount = nSize;
      }
    }

    if (nCount > 0) {
      if (nFrom <= 0 && nCount == nSize) {
        hb_itemReturn(pText);
      } else {
        hb_retclen(pszText, nCount);
      }
    } else {
      hb_retc_null();
    }
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1110, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_ULeft(<cString>, <nCount>) --> <cSubstring>
HB_FUNC(HB_ULEFT)
{
  auto pText = hb_param(1, Harbour::Item::STRING);

  if (pText && HB_ISNUM(2)) {
    HB_ISIZ nLen = hb_parns(2);
    if (nLen <= 0) {
      hb_retc_null();
    } else {
      auto nText = hb_itemGetCLen(pText);
      if (static_cast<HB_SIZE>(nLen) < nText) {
        nLen = hb_cdpTextPos(hb_vmCDP(), pText->getCPtr(), nText, nLen);
      }
      if (static_cast<HB_SIZE>(nLen) >= nText) {
        hb_itemReturn(pText);
      } else {
        hb_retclen(pText->getCPtr(), nLen);
      }
    }
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1124, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_BLeft(<cString>, <nCount>) --> <cSubstring>
HB_FUNC(HB_BLEFT)
{
  auto pText = hb_param(1, Harbour::Item::STRING);

  if (pText && HB_ISNUM(2)) {
    HB_ISIZ nLen = hb_parns(2);
    if (nLen <= 0) {
      hb_retc_null();
    } else {
      auto nText = hb_itemGetCLen(pText);
      if (static_cast<HB_SIZE>(nLen) >= nText) {
        hb_itemReturn(pText);
      } else {
        hb_retclen(pText->getCPtr(), nLen);
      }
    }
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1124, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_URight(<cString>, <nCount>) --> <cSubstring>
HB_FUNC(HB_URIGHT)
{
  auto pText = hb_param(1, Harbour::Item::STRING);
  auto nText = hb_itemGetCLen(pText);
  HB_ISIZ nLen = hb_parns(2);

  if (nLen > 0 && nText > 0) {
    if (static_cast<HB_SIZE>(nLen) < nText) {
      auto cdp = hb_vmCDP();
      HB_SIZE nChars = hb_cdpTextLen(cdp, pText->getCPtr(), nText);
      if (nChars > static_cast<HB_SIZE>(nLen)) {
        nLen = nText - hb_cdpTextPos(cdp, pText->getCPtr(), nText, nChars - nLen);
      } else {
        nLen = nText;
      }
    }
    if (static_cast<HB_SIZE>(nLen) >= nText) {
      hb_itemReturn(pText);
    } else {
      hb_retclen(pText->getCPtr() + nText - nLen, nLen);
    }
  } else {
    hb_retc_null();
  }
}

// hb_BRight(<cString>, <nCount>) --> <cSubstring>
HB_FUNC(HB_BRIGHT)
{
  auto pText = hb_param(1, Harbour::Item::STRING);
  auto nText = hb_itemGetCLen(pText);
  HB_ISIZ nLen = hb_parns(2);

  if (nLen > 0 && nText > 0) {
    if (static_cast<HB_SIZE>(nLen) >= nText) {
      hb_itemReturn(pText);
    } else {
      hb_retclen(pText->getCPtr() + nText - nLen, nLen);
    }
  } else {
    hb_retc_null();
  }
}

// hb_UAt(<cSubString>, <cString>, [<nFrom>], [<nTo>]) --> <nAt>
HB_FUNC(HB_UAT)
{
  auto pSub = hb_param(1, Harbour::Item::STRING);
  auto pText = hb_param(2, Harbour::Item::STRING);

  if (pText && pSub) {
    auto cdp = hb_vmCDP();
    auto pszText = pText->getCPtr();
    auto nTextLength = hb_itemGetCLen(pText);
    HB_SIZE nStart = hb_parns(3);
    HB_SIZE nFrom, nPos = 0;

    if (nStart <= 1) {
      nStart = nFrom = 0;
    } else {
      nFrom = hb_cdpTextPos(cdp, pszText, nTextLength, --nStart);
    }

    if (nFrom < nTextLength) {
      HB_SIZE nTo;

      pszText += nFrom;
      nTextLength -= nFrom;
      if (HB_ISNUM(4)) {
        nTo = hb_parns(4);
        if (nTo <= nStart) {
          nTo = 0;
        } else {
          nTo -= nStart;
          nTo = hb_cdpTextPos(cdp, pszText, nTextLength, nTo);
          if (nTo > nTextLength) {
            nTo = nTextLength;
          }
        }
      } else {
        nTo = nTextLength;
      }

      if (nTo > 0) {
        nPos = hb_strAt(pSub->getCPtr(), hb_itemGetCLen(pSub), pszText, nTo);
        if (nPos > 0) {
          nPos = hb_cdpTextLen(cdp, pszText, nPos - 1) + 1 + nStart;
        }
      }
    }
    hb_retns(nPos);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1108, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_BAt(<cSubString>, <cString>, [<nFrom>], [<nTo>]) --> <nAt>
HB_FUNC(HB_BAT)
{
  auto pSub = hb_param(1, Harbour::Item::STRING);
  auto pText = hb_param(2, Harbour::Item::STRING);

  if (pText && pSub) {
    auto pszText = pText->getCPtr();
    auto nTextLength = hb_itemGetCLen(pText);
    HB_SIZE nStart = hb_parns(3);
    HB_SIZE nFrom, nPos = 0;

    if (nStart <= 1) {
      nStart = nFrom = 0;
    } else {
      nFrom = --nStart;
    }

    if (nFrom < nTextLength) {
      HB_SIZE nTo;

      pszText += nFrom;
      nTextLength -= nFrom;
      if (HB_ISNUM(4)) {
        nTo = hb_parns(4);
        if (nTo <= nStart) {
          nTo = 0;
        } else {
          nTo -= nStart;
          if (nTo > nTextLength) {
            nTo = nTextLength;
          }
        }
      } else {
        nTo = nTextLength;
      }

      if (nTo > 0) {
        nPos = hb_strAt(pSub->getCPtr(), hb_itemGetCLen(pSub), pszText, nTo);
        if (nPos > 0) {
          nPos += nFrom;
        }
      }
    }
    hb_retns(nPos);
  } else {
    hb_errRT_BASE_SubstR(EG_ARG, 1108, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

// hb_BRAt(<cSubString>, <cString>, [<nFrom>], [<nTo>]) --> <nAt>
HB_FUNC(HB_BRAT)
{
  auto nSubLen = hb_parclen(1);
  HB_SIZE nPos = 0;

  if (nSubLen) {
    auto nLen = hb_parclen(2);
    HB_ISIZ nTo = nLen - nSubLen;

    if (nTo >= 0) {
      auto pszSub = hb_parc(1);
      auto pszText = hb_parc(2);
      HB_ISIZ nStart = hb_parns(3);
      HB_ISIZ nFrom;

      if (nStart <= 1) {
        nFrom = 0;
      } else {
        nFrom = --nStart;
      }

      if (nTo >= nFrom) {
        if (HB_ISNUM(4)) {
          HB_ISIZ nEnd = hb_parns(4) - nSubLen;

          if (nEnd < nTo) {
            nTo = nEnd;
          }
        }

        if (nTo >= nFrom) {
          do {
            if (pszText[nTo] == *pszSub && memcmp(pszSub, pszText + nTo, nSubLen) == 0) {
              nPos = nTo + 1;
              break;
            }
          } while (--nTo >= nFrom);
        }
      }
    }
  }

  hb_retns(nPos);
}

// hb_BStuff( <cString>, <nAt>, <nDel>, <cIns> ) --> <cResult>
HB_FUNC(HB_BSTUFF)
{
  auto szText = hb_parc(1);
  auto szIns = hb_parc(4);

  if (szText != nullptr && szIns != nullptr && HB_ISNUM(2) && HB_ISNUM(3)) {
    auto nLen = hb_parclen(1);
    HB_SIZE nPos = hb_parns(2);
    HB_SIZE nDel = hb_parns(3);
    auto nIns = hb_parclen(4);
    HB_SIZE nTot;

    if (nPos) {
      if (nPos < 1 || nPos > nLen) {
        nPos = nLen;
      } else {
        nPos--;
      }
    }
    if (nDel) {
      if (nDel < 1 || nDel > nLen - nPos) {
        nDel = nLen - nPos;
      }
    }

    if ((nTot = nLen + nIns - nDel) > 0) {
      auto szResult = static_cast<char *>(hb_xgrab(nTot + 1));

      hb_xmemcpy(szResult, szText, nPos);
      hb_xmemcpy(szResult + nPos, szIns, nIns);
      hb_xmemcpy(szResult + nPos + nIns, szText + nPos + nDel, nLen - (nPos + nDel));
      hb_retclen_buffer(szResult, nTot);
    } else {
      hb_retc_null();
    }
  } else {
    hb_retc_null();
  }
}

// hb_UStuff( <cString>, <nAt>, <nDel>, <cIns> ) --> <cResult>
HB_FUNC(HB_USTUFF)
{
  auto szText = hb_parc(1);
  auto szIns = hb_parc(4);

  if (szText != nullptr && szIns != nullptr && HB_ISNUM(2) && HB_ISNUM(3)) {
    auto cdp = hb_vmCDP();
    auto nLen = hb_parclen(1);
    HB_SIZE nPos = hb_parns(2);
    HB_SIZE nDel = hb_parns(3);
    auto nIns = hb_parclen(4);
    HB_SIZE nTot;

    if (nPos) {
      nPos = nPos < 1 ? nLen : hb_cdpTextPos(cdp, szText, nLen, nPos - 1);
    }
    if (nDel) {
      if (nPos < nLen) {
        nDel = hb_cdpTextPos(cdp, szText + nPos, nLen - nPos, nDel);
        if (nDel == 0) {
          nDel = nLen - nPos;
        }
      } else {
        nDel = 0;
      }
    }

    if ((nTot = nLen + nIns - nDel) > 0) {
      auto szResult = static_cast<char *>(hb_xgrab(nTot + 1));
      hb_xmemcpy(szResult, szText, nPos);
      hb_xmemcpy(szResult + nPos, szIns, nIns);
      hb_xmemcpy(szResult + nPos + nIns, szText + nPos + nDel, nLen - (nPos + nDel));
      hb_retclen_buffer(szResult, nTot);
    } else {
      hb_retc_null();
    }
  } else {
    hb_retc_null();
  }
}
