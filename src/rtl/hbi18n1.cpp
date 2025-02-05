//
// I18N translation Harbour functions
//
// Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#define _HB_I18N_INTERNAL_

#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapierr.hpp"
#include "hbapicdp.hpp"
#include "hbmath.hpp"
#include "hbvm.hpp"
#include "hbstack.hpp"
#include "hbthread.hpp"
#include "hbchksum.h"

// i18n hash table items:
//
// "LANG"            => <cLangID>
// "BASE_LANG"       => <cLangID>
// "CODEPAGE"        => <cCodePage>
// "BASE_CODEPAGE"   => <cCodePage>
// "DESCRIPTION"     => <cDescription>
// "PLURAL_EXP"      => <cPluralExp>
// "BASE_PLURAL_EXP" => <cPluralExp>
// "CONTEXT"         => ;
//             { "" => { <cMsg> => <cResult> | <aResult> } ; // default context
//    [, <cContext> => { <cMsg> => <cResult> | <aResult> } ] }
//
// i18n file format:
//
//    00-03 signature { 193, 'H', 'B', 'L' }
//    04-07 size of serialized i18n hash table, 32 bits in little endian order
//    08-11 CRC32 of serialized i18n hash table
//    12-15 unused
//    16-47 description
//    48-63 unused
//    64-.. serialized i18n hash table

#define HB_I18N_HEADER_SIZE 64
#define HB_I18N_SIG_OFFSET 0
#define HB_I18N_SIG_SIZE 4
#define HB_I18N_SIZE_OFFSET 4
#define HB_I18N_CRC_OFFSET 8
#define HB_I18N_TXT_OFFSET 16
#define HB_I18N_TXT_SIZE 32

#define HB_I18N_PLURAL_EN 1
#define HB_I18N_PLURAL_PL 2
#define HB_I18N_PLURAL_LT 3
#define HB_I18N_PLURAL_CS 4
#define HB_I18N_PLURAL_FR 5
#define HB_I18N_PLURAL_GA 6
#define HB_I18N_PLURAL_HR 7
#define HB_I18N_PLURAL_HU 8
#define HB_I18N_PLURAL_JA 9
#define HB_I18N_PLURAL_KO 10
#define HB_I18N_PLURAL_LV 11
#define HB_I18N_PLURAL_PT_BR 12
#define HB_I18N_PLURAL_RO 13
#define HB_I18N_PLURAL_RU 14
#define HB_I18N_PLURAL_SK 15
#define HB_I18N_PLURAL_SL 16
#define HB_I18N_PLURAL_SR 17
#define HB_I18N_PLURAL_TR 18
#define HB_I18N_PLURAL_UK 19
#define HB_I18N_PLURAL_VI 20

struct _HB_PLURAL_FORMS
{
  const char *szLangID;
  int iForm;
};

using HB_PLURAL_FORMS = _HB_PLURAL_FORMS;
using PHB_PLURAL_FORMS = _HB_PLURAL_FORMS *;

static const HB_PLURAL_FORMS s_plural_forms[] = {
    {"EN", HB_I18N_PLURAL_EN}, {"PL", HB_I18N_PLURAL_PL}, {"LT", HB_I18N_PLURAL_LT}, {"CS", HB_I18N_PLURAL_CS},
    {"FR", HB_I18N_PLURAL_FR}, {"GA", HB_I18N_PLURAL_GA}, {"HR", HB_I18N_PLURAL_HR}, {"HU", HB_I18N_PLURAL_HU},
    {"JA", HB_I18N_PLURAL_JA}, {"KO", HB_I18N_PLURAL_KO}, {"LV", HB_I18N_PLURAL_LV}, {"PT-BR", HB_I18N_PLURAL_PT_BR},
    {"RO", HB_I18N_PLURAL_RO}, {"RU", HB_I18N_PLURAL_RU}, {"SK", HB_I18N_PLURAL_SK}, {"SL", HB_I18N_PLURAL_SL},
    {"SR", HB_I18N_PLURAL_SR}, {"TR", HB_I18N_PLURAL_TR}, {"UK", HB_I18N_PLURAL_UK}, {"VI", HB_I18N_PLURAL_VI}};

#define HB_PLURAL_FOMRS_COUNT HB_SIZEOFARRAY(s_plural_forms)

static const HB_UCHAR s_signature[4] = {193, 'H', 'B', 'L'};
struct _HB_I18N_TRANS
{
  HB_COUNTER iUsers;
  PHB_CODEPAGE cdpage;
  PHB_CODEPAGE base_cdpage;
  PHB_ITEM table;
  PHB_ITEM context_table;
  PHB_ITEM default_context;
  PHB_ITEM plural_block;
  PHB_ITEM base_plural_block;
  int plural_form;
  int base_plural_form;
};

using HB_I18N_TRANS = _HB_I18N_TRANS;
using PHB_I18N_TRANS = _HB_I18N_TRANS *;

static PHB_I18N_TRANS hb_i18n_table(void)
{
  return static_cast<PHB_I18N_TRANS>(hb_vmI18N());
}

static int hb_i18n_pluralformfind(const char *szLang)
{
  for (auto i = 0; i < static_cast<int>(HB_PLURAL_FOMRS_COUNT); ++i)
  {
    if (hb_stricmp(szLang, s_plural_forms[i].szLangID) == 0)
    {
      return s_plural_forms[i].iForm;
    }
  }
  if (strlen(szLang) > 2)
  {
    for (auto i = 0; i < static_cast<int>(HB_PLURAL_FOMRS_COUNT); ++i)
    {
      if (hb_strnicmp(szLang, s_plural_forms[i].szLangID, 2) == 0)
      {
        return s_plural_forms[i].iForm;
      }
    }
  }
  return 0;
}

static const char *hb_i18n_pluralformid(int iForm)
{
  for (auto i = 0; i < static_cast<int>(HB_PLURAL_FOMRS_COUNT); ++i)
  {
    if (s_plural_forms[i].iForm == iForm)
    {
      return s_plural_forms[i].szLangID;
    }
  }
  return nullptr;
}

// NOTE: Source:
//       https://www.gnu.org/software/hello/manual/gettext/Plural-forms.html
//       [vszakats]

static long hb_i18n_pluralindex(int iForm, PHB_ITEM pNum)
{
  double n = hb_numRound(hb_itemGetND(pNum), 10), n10, n100;

  switch (iForm)
  {
  case HB_I18N_PLURAL_PL:
    n10 = fmod(n, 10.0);
    n100 = fmod(n, 100.0);
    return n == 1 ? 1 : (n10 >= 2 && n10 <= 4 && (n100 < 10 || n100 >= 20) ? 2 : 3);

  case HB_I18N_PLURAL_RO:
    n100 = fmod(n, 100.0);
    return n == 1 ? 1 : (n == 0 || (n100 > 0 && n100 < 20)) ? 2 : 3;

  case HB_I18N_PLURAL_HR:
  case HB_I18N_PLURAL_SR:
  case HB_I18N_PLURAL_RU:
  case HB_I18N_PLURAL_UK:
    n10 = fmod(n, 10.0);
    n100 = fmod(n, 100.0);
    return n10 == 1 && n100 != 11 ? 1 : n10 >= 2 && n10 <= 4 && (n100 < 10 || n100 >= 20) ? 2 : 3;

  case HB_I18N_PLURAL_CS:
  case HB_I18N_PLURAL_SK:
    return n == 1 ? 1 : ((n >= 2 && n <= 4) ? 2 : 3);

  case HB_I18N_PLURAL_SL:
    n100 = fmod(n, 100.0);
    return n100 == 1 ? 1 : (n100 == 2 ? 1 : (n100 == 3 || n100 == 4 ? 3 : 4));

  case HB_I18N_PLURAL_LT:
    n10 = fmod(n, 10.0);
    n100 = fmod(n, 100.0);
    return n10 == 1 && n100 != 11 ? 1 : (n10 != 0 && (n100 < 10 || n100 >= 20) ? 2 : 3);

  case HB_I18N_PLURAL_LV:
    n10 = fmod(n, 10.0);
    n100 = fmod(n, 100.0);
    return (n10 == 1 && n100 != 11) ? 1 : (n != 0 ? 2 : 3);

  case HB_I18N_PLURAL_GA:
    return n == 1 ? 1 : (n == 2 ? 2 : 3);

  case HB_I18N_PLURAL_JA:
  case HB_I18N_PLURAL_KO:
  case HB_I18N_PLURAL_VI:
  case HB_I18N_PLURAL_TR:
    return 1;

  case HB_I18N_PLURAL_FR:
  case HB_I18N_PLURAL_PT_BR:
    return n <= 1 ? 1 : 2;

  case HB_I18N_PLURAL_EN:
  case HB_I18N_PLURAL_HU:
  default:
    return n == 1 ? 1 : 2;
  }
}

static void hb_i18n_setitem(PHB_ITEM pHash, const char *szKey, const char *szValue)
{
  auto pKey = hb_itemPutC(nullptr, szKey);
  auto pValue = hb_itemPutC(nullptr, szValue);
  hb_hashAdd(pHash, pKey, pValue);
  hb_itemRelease(pKey);
  hb_itemRelease(pValue);
}

static PHB_ITEM hb_i18n_pluralexp_compile(PHB_ITEM pExp)
{
  auto nLen = hb_itemGetCLen(pExp);
  PHB_ITEM pBlock = nullptr;

  if (nLen > 0)
  {
    auto szMacro = static_cast<char *>(hb_xgrab(nLen + 6));
    szMacro[0] = '{';
    szMacro[1] = '|';
    szMacro[2] = 'n';
    szMacro[3] = '|';
    memcpy(&szMacro[4], pExp->getCPtr(), nLen);
    szMacro[4 + nLen] = '}';
    szMacro[5 + nLen] = '\0';
    auto pMacro = hb_itemPutCLPtr(nullptr, szMacro, nLen);
    const char *szType = hb_macroGetType(pMacro);
    if (*szType == 'B')
    {
      hb_vmPush(pMacro);
      hb_macroGetValue(hb_stackItemFromTop(-1), 0, 0);
      if (hb_vmRequestQuery() == 0)
      {
        pExp = hb_stackItemFromTop(-1);
        if (pExp->isBlock())
        {
          pBlock = hb_itemNew(pExp);
        }
        hb_stackPop();
      }
    }
    hb_itemRelease(pMacro);
  }

  return pBlock;
}

static PHB_I18N_TRANS hb_i18n_new(void)
{
  auto pI18N = static_cast<PHB_I18N_TRANS>(hb_xgrabz(sizeof(HB_I18N_TRANS)));
  hb_atomic_set(&pI18N->iUsers, 1);
  pI18N->table = hb_hashNew(hb_itemNew(nullptr));
  pI18N->context_table = hb_hashNew(hb_itemNew(nullptr));
  pI18N->default_context = hb_hashNew(hb_itemNew(nullptr));
  auto pKey = hb_itemPutCConst(nullptr, "CONTEXT");
  hb_hashAdd(pI18N->table, pKey, pI18N->context_table);
  pKey = hb_itemPutC(pKey, nullptr);
  hb_hashAdd(pI18N->context_table, pKey, pI18N->default_context);
  hb_itemRelease(pKey);
  return pI18N;
}

// HVM init
void hb_i18n_init(void)
{
  // do nothing in this implementation
}

// HVM exit
void hb_i18n_exit(void)
{
  // do nothing in this implementation
}

// make copy of i18n set for new thread
void *hb_i18n_alloc(void *cargo)
{
  if (cargo)
  {
    hb_atomic_inc(&(static_cast<PHB_I18N_TRANS>(cargo))->iUsers);
  }
  return cargo;
}

// release i18n set when thread is terminated
void hb_i18n_release(void *cargo)
{
  if (cargo)
  {
    auto pI18N = static_cast<PHB_I18N_TRANS>(cargo);

    if (hb_atomic_dec(&pI18N->iUsers))
    {
      if (pI18N->table)
      {
        hb_itemRelease(pI18N->table);
      }
      if (pI18N->context_table)
      {
        hb_itemRelease(pI18N->context_table);
      }
      if (pI18N->default_context)
      {
        hb_itemRelease(pI18N->default_context);
      }
      if (pI18N->base_plural_block)
      {
        hb_itemRelease(pI18N->base_plural_block);
      }
      if (pI18N->plural_block)
      {
        hb_itemRelease(pI18N->plural_block);
      }
      hb_xfree(pI18N);
    }
  }
}

static PHB_I18N_TRANS hb_i18n_initialize(PHB_ITEM pTable)
{
  PHB_I18N_TRANS pI18N = nullptr;

  if (pTable->isHash())
  {
    PHB_ITEM pDefContext = nullptr;

    auto pKey = hb_itemPutCConst(nullptr, "CONTEXT");
    auto pContext = hb_hashGetItemPtr(pTable, pKey, 0);
    if (pContext)
    {
      pKey = hb_itemPutC(pKey, nullptr);
      pDefContext = hb_hashGetItemPtr(pContext, pKey, 0);
    }

    if (pContext && pDefContext)
    {
      pI18N = static_cast<PHB_I18N_TRANS>(hb_xgrabz(sizeof(HB_I18N_TRANS)));
      hb_atomic_set(&pI18N->iUsers, 1);
      pI18N->table = pTable;
      pI18N->context_table = hb_itemNew(pContext);
      pI18N->default_context = hb_itemNew(pDefContext);

      pKey = hb_itemPutCConst(pKey, "BASE_CODEPAGE");
      auto pValue = hb_hashGetItemPtr(pTable, pKey, 0);
      if (pValue)
      {
        pI18N->base_cdpage = hb_cdpFind(pValue->getCPtr());
      }

      pKey = hb_itemPutCConst(pKey, "CODEPAGE");
      pValue = hb_hashGetItemPtr(pTable, pKey, 0);
      if (pValue)
      {
        pI18N->cdpage = hb_cdpFind(pValue->getCPtr());
      }

      pKey = hb_itemPutCConst(pKey, "BASE_LANG");
      pValue = hb_hashGetItemPtr(pTable, pKey, 0);
      if (pValue)
      {
        pI18N->base_plural_form = hb_i18n_pluralformfind(pValue->getCPtr());
      }

      pKey = hb_itemPutCConst(pKey, "LANG");
      pValue = hb_hashGetItemPtr(pTable, pKey, 0);
      if (pValue)
      {
        pI18N->plural_form = hb_i18n_pluralformfind(pValue->getCPtr());
      }

      pKey = hb_itemPutCConst(pKey, "BASE_PLURAL_EXP");
      pValue = hb_hashGetItemPtr(pTable, pKey, 0);
      if (pValue)
      {
        pI18N->base_plural_block = hb_i18n_pluralexp_compile(pValue);
      }

      pKey = hb_itemPutCConst(pKey, "PLURAL_EXP");
      pValue = hb_hashGetItemPtr(pTable, pKey, 0);
      if (pValue)
      {
        pI18N->plural_block = hb_i18n_pluralexp_compile(pValue);
      }
    }
    hb_itemRelease(pKey);
  }

  return pI18N;
}

static PHB_ITEM hb_i18n_serialize(PHB_I18N_TRANS pI18N)
{
  if (pI18N)
  {
    HB_SIZE nSize;
    char *pBuffer = hb_itemSerialize(pI18N->table, 0, &nSize);

    HB_U32 ulCRC = hb_crc32(0, pBuffer, nSize);
    auto pI18Nbuffer = static_cast<char *>(memset(hb_xgrab(nSize + HB_I18N_HEADER_SIZE + 1), 0, HB_I18N_HEADER_SIZE));
    memcpy(pI18Nbuffer + HB_I18N_HEADER_SIZE, pBuffer, nSize);
    hb_xfree(pBuffer);

    memcpy(pI18Nbuffer, s_signature, HB_I18N_SIG_SIZE);
    HB_PUT_LE_UINT32(&pI18Nbuffer[HB_I18N_SIZE_OFFSET], nSize);
    HB_PUT_LE_UINT32(&pI18Nbuffer[HB_I18N_CRC_OFFSET], ulCRC);

    auto pKey = hb_itemPutCConst(nullptr, "DESCRIPTION");
    auto pValue = hb_hashGetItemPtr(pI18N->table, pKey, 0);
    if (pValue)
    {
      hb_strncpy(&pI18Nbuffer[HB_I18N_TXT_OFFSET], pValue->getCPtr(), HB_I18N_TXT_SIZE);
    }

    return hb_itemPutCLPtr(pKey, pI18Nbuffer, nSize + HB_I18N_HEADER_SIZE);
  }

  return nullptr;
}

static bool hb_i18n_headercheck(const char *pBuffer, HB_SIZE nLen)
{
  if (nLen < HB_I18N_HEADER_SIZE)
  {
    return false;
  }

  nLen -= HB_I18N_HEADER_SIZE;
  return memcmp(pBuffer, s_signature, HB_I18N_SIG_SIZE) == 0 &&
         (nLen == 0 ||
          (HB_GET_LE_UINT32(&pBuffer[HB_I18N_SIZE_OFFSET]) == nLen &&
           HB_GET_LE_UINT32(&pBuffer[HB_I18N_CRC_OFFSET]) == hb_crc32(0, pBuffer + HB_I18N_HEADER_SIZE, nLen)));
}

static PHB_I18N_TRANS hb_i18n_deserialize(PHB_ITEM pItem)
{
  PHB_I18N_TRANS pI18N = nullptr;

  if (pItem && pItem->isString())
  {
    auto nLen = pItem->getCLen();
    auto pBuffer = pItem->getCPtr();
    if (nLen > HB_I18N_HEADER_SIZE && hb_i18n_headercheck(pBuffer, nLen))
    {
      pBuffer += HB_I18N_HEADER_SIZE;
      nLen -= HB_I18N_HEADER_SIZE;
      PHB_ITEM pTable = hb_itemDeserialize(&pBuffer, &nLen);
      if (pTable)
      {
        pI18N = hb_i18n_initialize(pTable);
        if (!pI18N)
        {
          hb_itemRelease(pTable);
        }
      }
    }
  }

  return pI18N;
}

static HB_GARBAGE_FUNC(hb_i18n_destructor)
{
  auto pI18NHolder = static_cast<PHB_I18N_TRANS *>(Cargo);

  if (*pI18NHolder)
  {
    hb_i18n_release(static_cast<void *>(*pI18NHolder));
    *pI18NHolder = nullptr;
  }
}

static const HB_GC_FUNCS s_gcI18NFuncs = {hb_i18n_destructor, hb_gcDummyMark};

static PHB_I18N_TRANS hb_i18n_param(int *piParam, bool fActive)
{
  auto pI18NHolder = static_cast<PHB_I18N_TRANS *>(hb_parptrGC(&s_gcI18NFuncs, *piParam));

  if (pI18NHolder)
  {
    (*piParam)++;
    return *pI18NHolder;
  }

  return fActive ? hb_i18n_table() : nullptr;
}

static PHB_ITEM hb_i18n_newitem(PHB_I18N_TRANS pI18N)
{
  auto pItem = hb_itemNew(nullptr);

  if (!pI18N)
  {
    pI18N = hb_i18n_new();
  }
  auto pI18NHolder = static_cast<PHB_I18N_TRANS *>(hb_gcAllocate(sizeof(PHB_I18N_TRANS), &s_gcI18NFuncs));
  *pI18NHolder = pI18N;

  return hb_itemPutPtrGC(pItem, pI18NHolder);
}

static bool hb_i18n_getpluralform(PHB_I18N_TRANS pI18N, PHB_ITEM pOldForm, bool fBase)
{
  auto fResult = false;

  if (pI18N)
  {
    if (pOldForm)
    {
      PHB_ITEM pBlock;
      int iForm;

      if (fBase)
      {
        pBlock = pI18N->base_plural_block;
        iForm = pI18N->base_plural_form;
      }
      else
      {
        pBlock = pI18N->plural_block;
        iForm = pI18N->plural_form;
      }

      if (pBlock)
      {
        hb_itemCopy(pOldForm, pBlock);
      }
      else if (iForm)
      {
        hb_itemPutC(pOldForm, hb_i18n_pluralformid(iForm));
      }
      else
      {
        hb_itemPutCConst(pOldForm, "EN"); // default is ENGLISH
      }
    }
    fResult = true;
  }
  return fResult;
}

static bool hb_i18n_setpluralform(PHB_I18N_TRANS pI18N, PHB_ITEM pForm, bool fBase)
{
  auto fResult = false;

  if (pI18N && pForm)
  {
    if (pForm->isEvalItem())
    {
      if (fBase)
      {
        if (pI18N->base_plural_block)
        {
          hb_itemCopy(pI18N->base_plural_block, pForm);
        }
        else
        {
          pI18N->base_plural_block = hb_itemNew(pForm);
        }
      }
      else
      {
        if (pI18N->plural_block)
        {
          hb_itemCopy(pI18N->plural_block, pForm);
        }
        else
        {
          pI18N->plural_block = hb_itemNew(pForm);
        }
      }
      fResult = true;
    }
    else if (pForm->isString())
    {
      int iForm = hb_i18n_pluralformfind(pForm->getCPtr());
      if (iForm)
      {
        const char *szKey;
        if (fBase)
        {
          if (pI18N->base_plural_block)
          {
            hb_itemRelease(pI18N->base_plural_block);
            pI18N->base_plural_block = nullptr;
          }
          pI18N->base_plural_form = iForm;
          szKey = "BASE_LANG";
        }
        else
        {
          if (pI18N->plural_block)
          {
            hb_itemRelease(pI18N->plural_block);
            pI18N->plural_block = nullptr;
          }
          pI18N->plural_form = iForm;
          szKey = "LANG";
        }
        hb_i18n_setitem(pI18N->table, szKey, hb_i18n_pluralformid(iForm));
        fResult = true;
      }
    }
  }
  return fResult;
}

static void hb_i18n_transitm(PHB_ITEM pText, PHB_CODEPAGE cdpIn, PHB_CODEPAGE cdpOut)
{
  auto nLen = hb_itemGetCLen(pText);

  if (nLen > 0)
  {
    char *szValue = hb_cdpnDup(pText->getCPtr(), &nLen, cdpIn, cdpOut);
    hb_itemPutCLPtr(pText, szValue, nLen);
  }
}

static const char *hb_i18n_setcodepage(PHB_I18N_TRANS pI18N, const char *szCdpID, bool fBase, bool fTranslate)
{
  const char *szOldCdpID = nullptr;

  if (pI18N)
  {
    PHB_CODEPAGE cdp = szCdpID ? hb_cdpFind(szCdpID) : nullptr;

    PHB_CODEPAGE cdpage = fBase ? pI18N->base_cdpage : pI18N->cdpage;
    if (cdpage)
    {
      szOldCdpID = cdpage->id;
    }
    if (cdp && cdp != cdpage)
    {
      const char *szKey;

      if (fTranslate && cdpage)
      {
        HB_SIZE nHashLen = hb_hashLen(pI18N->context_table), ul;
        for (ul = 1; ul <= nHashLen; ++ul)
        {
          auto pContext = hb_hashGetValueAt(pI18N->context_table, ul);
          HB_SIZE nCount = hb_hashLen(pContext);

          for (HB_SIZE u = 1; u <= nCount; ++u)
          {
            if (fBase)
            {
              hb_i18n_transitm(hb_hashGetKeyAt(pContext, u), cdpage, cdp);
            }
            else
            {
              auto pResult = hb_hashGetValueAt(pContext, u);
              if (pResult->isString())
              {
                hb_i18n_transitm(pResult, cdpage, cdp);
              }
              else if (pResult->isArray())
              {
                HB_SIZE nTrans = hb_arrayLen(pResult), u2;
                for (u2 = 1; u2 <= nTrans; ++u2)
                {
                  hb_i18n_transitm(hb_arrayGetItemPtr(pResult, u2), cdpage, cdp);
                }
              }
            }
          }
          if (fBase)
          {
            hb_i18n_transitm(hb_hashGetKeyAt(pI18N->context_table, ul), cdpage, cdp);
            hb_hashSetFlags(pContext, HB_HASH_RESORT);
          }
        }
        if (fBase)
        {
          hb_hashSetFlags(pI18N->context_table, HB_HASH_RESORT);
        }
      }

      if (fBase)
      {
        pI18N->base_cdpage = cdp;
        szKey = "BASE_CODEPAGE";
      }
      else
      {
        pI18N->cdpage = cdp;
        szKey = "CODEPAGE";
      }
      hb_i18n_setitem(pI18N->table, szKey, szCdpID);
    }
  }

  return szOldCdpID;
}

static const char *hb_i18n_description(PHB_I18N_TRANS pI18N, PHB_ITEM pItem)
{
  if (pI18N)
  {
    auto pKey = hb_itemPutCConst(nullptr, "DESCRIPTION");

    auto pValue = hb_hashGetItemPtr(pI18N->table, pKey, 0);
    if (pItem != nullptr)
    {
      if (pItem->isString())
      {
        if (pValue)
        {
          hb_itemCopy(pValue, pItem);
        }
        else
        {
          hb_hashAdd(pI18N->table, pKey, pItem);
          pValue = hb_hashGetItemPtr(pI18N->table, pKey, 0);
        }
      }
    }
    hb_itemRelease(pKey);

    return hb_itemGetCPtr(pValue);
  }

  return nullptr;
}

static void hb_i18n_addtext(PHB_I18N_TRANS pI18N, PHB_ITEM pMsgID, PHB_ITEM pTrans, PHB_ITEM pContext)
{
  PHB_ITEM pTable = pContext ? hb_hashGetItemPtr(pI18N->context_table, pContext, 0) : pI18N->default_context;

  if (!pTable)
  {
    pTable = hb_hashNew(hb_itemNew(nullptr));
    hb_hashAdd(pTable, pMsgID, pTrans);
    hb_hashAdd(pI18N->context_table, pContext, pTable);
    hb_itemRelease(pTable);
  }
  else
  {
    hb_hashAdd(pTable, pMsgID, pTrans);
  }
}

PHB_ITEM hb_i18n_gettext(PHB_ITEM pMsgID, PHB_ITEM pContext)
{
  PHB_I18N_TRANS pI18N = hb_i18n_table();
  PHB_CODEPAGE cdpage = nullptr;
  PHB_ITEM pMsgDst = pMsgID;

  if (pI18N)
  {
    PHB_ITEM pTable = pContext && pI18N->context_table ? hb_hashGetItemPtr(pI18N->context_table, pContext, 0)
                                                       : pI18N->default_context;

    cdpage = pI18N->base_cdpage;
    if (pTable)
    {
      pTable = hb_hashGetItemPtr(pTable, pMsgID, 0);
      if (pTable)
      {
        if (pTable->isArray())
        {
          pTable = hb_arrayGetItemPtr(pTable, 1);
        }
        if (pTable && pTable->isString())
        {
          pMsgID = pTable;
          cdpage = pI18N->cdpage;
        }
      }
    }
  }

  if (pMsgID)
  {
    if (pMsgID->isString())
    {
      if (cdpage)
      {
        auto cdp = hb_vmCDP();
        if (cdp && cdp != cdpage)
        {
          if (pMsgDst != pMsgID)
          {
            hb_itemCopy(pMsgDst, pMsgID);
            pMsgID = pMsgDst;
          }
          hb_i18n_transitm(pMsgID, cdpage, cdp);
        }
      }
    }
    else
    {
      pMsgID = nullptr;
    }
  }

  return pMsgID;
}

PHB_ITEM hb_i18n_ngettext(PHB_ITEM pNum, PHB_ITEM pMsgID, PHB_ITEM pContext)
{
  PHB_I18N_TRANS pI18N = hb_i18n_table();
  PHB_CODEPAGE cdpage = nullptr;
  PHB_ITEM pMsgDst = pMsgID;
  PHB_ITEM pBlock = nullptr;
  int iPluralForm = 0;

  if (pI18N)
  {
    PHB_ITEM pTable = pContext && pI18N->context_table ? hb_hashGetItemPtr(pI18N->context_table, pContext, 0)
                                                       : pI18N->default_context;

    cdpage = pI18N->base_cdpage;
    pBlock = pI18N->base_plural_block;
    iPluralForm = pI18N->base_plural_form;

    if (pTable)
    {
      PHB_ITEM pMsg = pMsgID->isArray() ? hb_arrayGetItemPtr(pMsgID, 1) : pMsgID;
      pTable = pMsg && pMsg->isString() ? hb_hashGetItemPtr(pTable, pMsg, 0) : nullptr;
      if (pTable)
      {
        if (pTable->isString() || (pTable->isArray() && (hb_arrayGetType(pTable, 1) & Harbour::Item::STRING) != 0))
        {
          pMsgID = pTable;
          cdpage = pI18N->cdpage;
          pBlock = pI18N->plural_block;
          iPluralForm = pI18N->plural_form;
        }
      }
    }
  }

  if (pMsgID && pMsgID->isArray())
  {
    long lIndex;

    if (!pNum)
    {
      lIndex = 1;
    }
    else if (pBlock)
    {
      hb_evalBlock1(pBlock, pNum);
      lIndex = hb_parnl(-1);
    }
    else
    {
      lIndex = hb_i18n_pluralindex(iPluralForm, pNum);
    }

    if (lIndex < 1 || (lIndex != 1 && (hb_arrayGetType(pMsgID, lIndex) & Harbour::Item::STRING) == 0))
    {
      lIndex = 1;
    }

    pMsgID = hb_arrayGetItemPtr(pMsgID, lIndex);
  }

  if (pMsgID)
  {
    if (pMsgID->isString())
    {
      if (cdpage)
      {
        auto cdp = hb_vmCDP();
        if (cdp && cdp != cdpage)
        {
          if (pMsgDst != pMsgID)
          {
            hb_itemCopy(pMsgDst, pMsgID);
            pMsgID = pMsgDst;
          }
          hb_i18n_transitm(pMsgID, cdpage, cdp);
        }
      }
    }
    else
    {
      pMsgID = nullptr;
    }
  }

  return pMsgID;
}

// base .prg i18n functions

HB_FUNC(HB_I18N_GETTEXT)
{
  auto pMsgID = hb_param(1, Harbour::Item::STRING);
  auto pContext = hb_param(2, Harbour::Item::STRING);

  if (pMsgID)
  {
    pMsgID = hb_i18n_gettext(pMsgID, pContext);
  }

  if (pMsgID && pMsgID->isString())
  {
    hb_itemReturn(pMsgID);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_I18N_NGETTEXT)
{
  auto pNum = hb_param(1, Harbour::Item::NUMERIC);
  auto pMsgID = hb_param(2, Harbour::Item::STRING | Harbour::Item::ARRAY);
  auto pContext = hb_param(3, Harbour::Item::STRING);

  if (!pNum)
  {
    pMsgID = nullptr;
  }
  else if (pMsgID)
  {
    pMsgID = hb_i18n_ngettext(pNum, pMsgID, pContext);
  }

  if (pMsgID && pMsgID->isString())
  {
    hb_itemReturn(pMsgID);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC_TRANSLATE(HB_I18N_GETTEXT_STRICT, HB_I18N_GETTEXT)
HB_FUNC_TRANSLATE(HB_I18N_NGETTEXT_STRICT, HB_I18N_NGETTEXT)

// extended .prg i18n functions to create and manage translation tables

HB_FUNC(HB_I18N_CREATE)
{
  hb_itemReturnRelease(hb_i18n_newitem(nullptr));
}

HB_FUNC(HB_I18N_CODEPAGE)
{
  int iParam = 1;

  PHB_I18N_TRANS pI18N = hb_i18n_param(&iParam, true);

  if (pI18N)
  {
    hb_retc(hb_i18n_setcodepage(pI18N, hb_parc(iParam), hb_parl(iParam + 1), hb_parl(iParam + 2)));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_I18N_PLURALFORM)
{
  int iParam = 1;

  PHB_I18N_TRANS pI18N = hb_i18n_param(&iParam, true);
  if (pI18N)
  {
    auto pOldForm = hb_itemNew(nullptr);
    auto pForm = hb_param(iParam, Harbour::Item::STRING | Harbour::Item::EVALITEM);
    bool fBase = hb_parl(iParam + 1);

    if (hb_i18n_getpluralform(pI18N, pOldForm, fBase))
    {
      hb_itemReturn(pOldForm);
    }
    hb_itemRelease(pOldForm);
    if (pForm)
    {
      hb_i18n_setpluralform(pI18N, pForm, fBase);
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_I18N_DESCRIPTION)
{
  int iParam = 1;

  PHB_I18N_TRANS pI18N = hb_i18n_param(&iParam, true);
  if (pI18N)
  {
    auto pNewDescript = hb_param(iParam, Harbour::Item::STRING);
    hb_retc(hb_i18n_description(pI18N, nullptr));
    if (pNewDescript)
    {
      hb_i18n_description(pI18N, pNewDescript);
    }
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_I18N_ADDTEXT)
{
  int iParam = 1;

  PHB_I18N_TRANS pI18N = hb_i18n_param(&iParam, false);
  if (pI18N)
  {
    auto pMsgID = hb_param(iParam, Harbour::Item::STRING);
    auto pTrans = hb_param(iParam + 1, Harbour::Item::STRING | Harbour::Item::ARRAY);
    auto pContext = hb_param(iParam + 2, Harbour::Item::STRING);

    if (pMsgID && pTrans)
    {
      if (pTrans->isArray())
      {
        HB_SIZE nLen = hb_arrayLen(pTrans);
        if (nLen != 0)
        {
          for (HB_SIZE n = 1; n <= nLen; ++n)
          {
            if (!hb_arrayGetItemPtr(pTrans, n)->isString())
            {
              pTrans = nullptr;
              break;
            }
          }
        }
        else
        {
          pTrans = nullptr;
        }
      }

      if (pTrans)
      {
        hb_i18n_addtext(pI18N, pMsgID, pTrans, pContext);
        return;
      }
    }
  }

  hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
}

HB_FUNC(HB_I18N_SET)
{
  if (hb_pcount() > 0)
  {
    if (HB_ISNIL(1))
    {
      hb_vmSetI18N(nullptr);
    }
    else
    {
      int iParam = 1;
      PHB_I18N_TRANS pI18N = hb_i18n_param(&iParam, false);
      if (pI18N)
      {
        hb_vmSetI18N(hb_i18n_alloc(pI18N));
      }
      else
      {
        hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
        return;
      }
    }
  }
  hb_retl(hb_i18n_table() != nullptr);
}

HB_FUNC(HB_I18N_SAVETABLE)
{
  int iParam = 1;

  PHB_I18N_TRANS pI18N = hb_i18n_param(&iParam, true);
  if (pI18N)
  {
    hb_itemReturnRelease(hb_i18n_serialize(pI18N));
  }
}

HB_FUNC(HB_I18N_RESTORETABLE)
{
  auto pItem = hb_param(1, Harbour::Item::STRING);

  if (pItem != nullptr)
  {
    PHB_I18N_TRANS pI18N = hb_i18n_deserialize(pItem);
    if (pI18N)
    {
      hb_itemReturnRelease(hb_i18n_newitem(pI18N));
    }
  }
}

HB_FUNC(HB_I18N_HEADERSIZE)
{
  hb_retni(HB_I18N_HEADER_SIZE);
}

HB_FUNC(HB_I18N_CHECK)
{
  hb_retl(hb_i18n_headercheck(hb_parc(1), hb_parclen(1)));
}

// unofficial function to access internal hash table used by i18n set
HB_FUNC(__I18N_HASHTABLE)
{
  PHB_I18N_TRANS pI18N;
  auto pTable = hb_param(1, Harbour::Item::HASH);

  if (pTable)
  {
    pTable = hb_itemNew(pTable);
    pI18N = hb_i18n_initialize(pTable);
    if (pI18N)
    {
      hb_itemReturnRelease(hb_i18n_newitem(pI18N));
    }
    else
    {
      hb_itemRelease(pTable);
    }
  }
  else
  {
    int iParam = 1;

    pI18N = hb_i18n_param(&iParam, true);
    if (pI18N)
    {
      hb_itemReturn(pI18N->table);
    }
  }
}
