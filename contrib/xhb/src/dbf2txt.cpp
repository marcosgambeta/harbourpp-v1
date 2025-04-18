//
// Dbf2Text() function
//
// Copyright 1999 Andi Jahja <andij@aonlippo.co.id>
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

#include <hbapi.hpp>
#include <hbapiitm.hpp>
#include <hbapirdd.hpp>
#include <hbapierr.hpp>
#include <hbapicdp.hpp>
#include <hbapifs.hpp>
#include <hbset.hpp>
#include <hbvm.hpp>

/* Escaping delimited strings. Need to be cleaned/optimized/improved */
static char *hb_strescape(const char *szInput, HB_ISIZ nLen, const char *cDelim)
{
  HB_ISIZ nCnt = 0;
  const char *szChr;
  char *szEscape;
  char *szReturn;

  szReturn = szEscape = static_cast<char *>(hb_xgrab(nLen * 2 + 4));

  while (nLen && HB_ISSPACE(szInput[nLen - 1]))
  {
    nLen--;
  }

  szChr = szInput;

  while (*szChr && nCnt++ < nLen)
  {
    if (*szChr == *cDelim)
    {
      *szEscape++ = '\\';
    }

    *szEscape++ = *szChr++;
  }
  *szEscape = '\0';

  return szReturn;
}

/* Export field values to text file */
static HB_BOOL hb_ExportVar(HB_FHANDLE handle, PHB_ITEM pValue, const char *cDelim, PHB_CODEPAGE cdp)
{
  switch (hb_itemType(pValue))
  {
  /* a "C" field */
  case Harbour::Item::STRING:
  {
    char *szStrEsc;
    char *szString;

    szStrEsc = hb_strescape(hb_itemGetCPtr(pValue), hb_itemGetCLen(pValue), cDelim);
    if (cdp)
    {
      hb_cdpnDupLen(szStrEsc, strlen(szStrEsc), hb_vmCDP(), cdp);
    }

    szString = hb_xstrcpy(nullptr, cDelim, szStrEsc, cDelim, nullptr);

    /* FWrite(handle, szString) */
    hb_fsWriteLarge(handle, szString, strlen(szString));

    /* Orphaned, get rif off it */
    hb_xfree(szStrEsc);
    hb_xfree(szString);
    break;
  }
  /* a "D" field */
  case Harbour::Item::DATE:
  {
    auto szDate = static_cast<char *>(hb_xgrab(9));

    hb_itemGetDS(pValue, szDate);
    hb_fsWriteLarge(handle, szDate, strlen(szDate));
    hb_xfree(szDate);
    break;
  }
  /* an "L" field */
  case Harbour::Item::LOGICAL:
    hb_fsWriteLarge(handle, (hb_itemGetL(pValue) ? "T" : "F"), 1);
    break;
  /* an "N" field */
  case Harbour::Item::INTEGER:
  case Harbour::Item::LONG:
  case Harbour::Item::DOUBLE:
  {
    char *szResult = hb_itemStr(pValue, nullptr, nullptr);

    if (szResult)
    {
      HB_SIZE nLen = strlen(szResult);
      const char *szTrimmed = hb_strLTrim(szResult, &nLen);

      hb_fsWriteLarge(handle, szTrimmed, strlen(szTrimmed));
      hb_xfree(szResult);
    }
    break;
  }
  /* an "M" field or the other, might be a "V" in SixDriver */
  default:
    /* We do not want MEMO contents */
    return false;
  }
  return true;
}

HB_FUNC(DBF2TEXT)
{
  auto pWhile = hb_param(1, Harbour::Item::BLOCK);
  auto pFor = hb_param(2, Harbour::Item::BLOCK);
  auto pFields = hb_param(3, Harbour::Item::ARRAY);

  auto cDelim = hb_parc(4);
  auto handle = static_cast<HB_FHANDLE>(hb_parnint(5));
  auto cSep = hb_parc(6);
  auto nCount = hb_parni(7);
  PHB_CODEPAGE cdp = hb_cdpFind(hb_parcx(8));

  auto pArea = static_cast<AREAP>(hb_rddGetCurrentWorkAreaPointer());

  /* Export DBF content to text file */

  HB_ISIZ nSepLen;
  HB_USHORT uiFields = 0;
  HB_USHORT ui;
  HB_BOOL bWriteSep = false;

  HB_BOOL bEof = true;
  HB_BOOL bBof = true;

  HB_BOOL bNoFieldPassed = (pFields == nullptr || hb_arrayLen(pFields) == 0);

  if (!pArea)
  {
    hb_errRT_DBCMD(EG_NOTABLE, EDBCMD_NOTABLE, nullptr, HB_ERR_FUNCNAME);
    return;
  }

  if (!handle)
  {
    hb_errRT_DBCMD(EG_ARG, EDBCMD_EVAL_BADPARAMETER, nullptr, HB_ERR_FUNCNAME);
    return;
  }

  if (cdp && cdp == hb_vmCDP())
  {
    cdp = nullptr;
  }

  auto pTmp = hb_itemNew(nullptr);

  if (!cDelim)
  {
    cDelim = "\"";
  }

  if (cSep)
  {
    nSepLen = hb_parclen(6);
  }
  else
  {
    cSep = ",";
    nSepLen = 1;
  }

  SELF_FIELDCOUNT(pArea, &uiFields);

  while ((nCount == -1 || nCount > 0) && (!pWhile || hb_itemGetL(hb_vmEvalBlock(pWhile))))
  {
    /* WHILE !Bof() .AND. !Eof() */
    SELF_EOF(pArea, &bEof);
    SELF_BOF(pArea, &bBof);

    if (bEof || bBof)
    {
      break;
    }

    /* For condition is met */
    /* if For is nullptr, hb__Eval returns true */
    if (!pFor || hb_itemGetL(hb_vmEvalBlock(pFor)))
    {
      /* User does not request fields, copy all fields */
      if (bNoFieldPassed)
      {
        for (ui = 1; ui <= uiFields; ui++)
        {
          if (bWriteSep)
          {
            hb_fsWriteLarge(handle, cSep, nSepLen);
          }

          SELF_GETVALUE(pArea, ui, pTmp);
          bWriteSep = hb_ExportVar(handle, pTmp, cDelim, cdp);
          hb_itemClear(pTmp);
        }
        /* Only requested fields are exported here */
      }
      else
      {
        auto uiFieldCopy = static_cast<HB_USHORT>(hb_arrayLen(pFields));
        HB_USHORT uiItter;

        for (uiItter = 1; uiItter <= uiFieldCopy; uiItter++)
        {
          auto szFieldName = hb_arrayGetCPtr(pFields, uiItter);
          if (szFieldName)
          {
            int iPos = hb_rddFieldIndex(pArea, szFieldName);

            if (iPos)
            {
              if (bWriteSep)
              {
                hb_fsWriteLarge(handle, cSep, nSepLen);
              }

              SELF_GETVALUE(pArea, static_cast<HB_USHORT>(iPos), pTmp);
              bWriteSep = hb_ExportVar(handle, pTmp, cDelim, cdp);
              hb_itemClear(pTmp);
            }
          }
        }
      }
      hb_fsWriteLarge(handle, "\r\n", 2);
      bWriteSep = false;
    }

    if (nCount != -1)
    {
      nCount--;
    }

    /* dbSkip() */
    SELF_SKIP(pArea, 1);
  }

  /* Writing EOF */
  if (hb_setGetEOF())
  {
    hb_fsWriteLarge(handle, "\032", 1);
  }
  hb_itemRelease(pTmp);
}
