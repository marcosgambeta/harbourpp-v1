/*
 * Windows pcode DLL entry point and VM/RTL routing functions
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl> (rewritten)
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

#include "hbtypes.h"

#if defined(HB_OS_WIN)

#include <windows.h>

#define HB_DLL_MSG_NO_FUNC(func)                                                                                       \
  do                                                                                                                   \
  {                                                                                                                    \
    MessageBox(nullptr, TEXT("Function '") TEXT(func) TEXT("' not found!"), TEXT(func), MB_OK | MB_ICONERROR);         \
  } while (false)

int hb_pcount(void)
{
  static HB_PCOUNT s_pcount = nullptr;

  if (!s_pcount)
  {
    s_pcount = reinterpret_cast<HB_PCOUNT>(hb_dllGetProcAddress("hb_pcount"));
    if (!s_pcount)
    {
      HB_DLL_MSG_NO_FUNC("hb_pcount");
    }
  }

  return s_pcount ? s_pcount() : 0;
}

HB_ULONG hb_parinfo(int iParam)
{
  static HB_PARINFO s_parinfo = nullptr;

  if (!s_parinfo)
  {
    s_parinfo = reinterpret_cast<HB_PARINFO>(hb_dllGetProcAddress("hb_parinfo"));
    if (!s_parinfo)
    {
      HB_DLL_MSG_NO_FUNC("hb_parinfo");
    }
  }
  return s_parinfo ? s_parinfo(iParam) : 0;
}

HB_SIZE hb_parinfa(int iParam, HB_SIZE nArrayIndex)
{
  static HB_PARINFA s_parinfa = nullptr;

  if (!s_parinfa)
  {
    s_parinfa = reinterpret_cast<HB_PARINFA>(hb_dllGetProcAddress("hb_parinfa"));
    if (!s_parinfa)
    {
      HB_DLL_MSG_NO_FUNC("hb_parinfa");
    }
  }
  return s_parinfa ? s_parinfa(iParam, nArrayIndex) : 0;
}

PHB_ITEM hb_param(int iParam, long lMask)
{
  static HB_PARAM s_param = nullptr;

  if (!s_param)
  {
    s_param = reinterpret_cast<HB_PARAM>(hb_dllGetProcAddress("hb_param"));
    if (!s_param)
    {
      HB_DLL_MSG_NO_FUNC("hb_param");
    }
  }

  return s_param ? s_param(iParam, lMask) : nullptr;
}

PHB_ITEM hb_paramError(int iParam)
{
  static HB_PARAMERROR s_paramError = nullptr;

  if (!s_paramError)
  {
    s_paramError = reinterpret_cast<HB_PARAMERROR>(hb_dllGetProcAddress("hb_paramError"));
    if (!s_paramError)
    {
      HB_DLL_MSG_NO_FUNC("hb_paramError");
    }
  }

  return s_paramError ? s_paramError(iParam) : nullptr;
}

HB_BOOL hb_extIsNil(int iParam)
{
  static HB_EXTISPARAM s_extIsNil = nullptr;

  if (!s_extIsNil)
  {
    s_extIsNil = reinterpret_cast<HB_EXTISPARAM>(hb_dllGetProcAddress("hb_extIsNil"));
    if (!s_extIsNil)
    {
      HB_DLL_MSG_NO_FUNC("hb_extIsNil");
    }
  }

  return s_extIsNil ? s_extIsNil(iParam) : FALSE;
}

HB_BOOL hb_extIsArray(int iParam)
{
  static HB_EXTISPARAM s_extIsArray = nullptr;

  if (!s_extIsArray)
  {
    s_extIsArray = reinterpret_cast<HB_EXTISPARAM>(hb_dllGetProcAddress("hb_extIsArray"));
    if (!s_extIsArray)
    {
      HB_DLL_MSG_NO_FUNC("hb_extIsArray");
    }
  }

  return s_extIsArray ? s_extIsArray(iParam) : FALSE;
}

HB_BOOL hb_extIsObject(int iParam)
{
  static HB_EXTISPARAM s_extIsObject = nullptr;

  if (!s_extIsObject)
  {
    s_extIsObject = reinterpret_cast<HB_EXTISPARAM>(hb_dllGetProcAddress("hb_extIsObject"));
    if (!s_extIsObject)
    {
      HB_DLL_MSG_NO_FUNC("hb_extIsObject");
    }
  }

  return s_extIsObject ? s_extIsObject(iParam) : FALSE;
}

void hb_ret(void)
{
  static HB_RET s_ret = nullptr;

  if (!s_ret)
  {
    s_ret = static_cast<HB_RET>(hb_dllGetProcAddress("hb_ret"));
    if (!s_ret)
    {
      HB_DLL_MSG_NO_FUNC("hb_ret");
    }
  }

  if (s_ret)
  {
    s_ret();
  }
}

void hb_retc(const char *szText)
{
  static HB_RETC s_retc = nullptr;

  if (!s_retc)
  {
    s_retc = reinterpret_cast<HB_RETC>(hb_dllGetProcAddress("hb_retc"));
    if (!s_retc)
    {
      HB_DLL_MSG_NO_FUNC("hb_retc");
    }
  }

  if (s_retc)
  {
    s_retc(szText);
  }
}

void hb_retclen(const char *szText, HB_SIZE nLen)
{
  static HB_RETCLEN s_retclen = nullptr;

  if (!s_retclen)
  {
    s_retclen = reinterpret_cast<HB_RETCLEN>(hb_dllGetProcAddress("hb_retclen"));
    if (!s_retclen)
    {
      HB_DLL_MSG_NO_FUNC("hb_retclen");
    }
  }

  if (s_retclen)
  {
    s_retclen(szText, nLen);
  }
}

void hb_retds(const char *szDate)
{
  static HB_RETDS s_retds = nullptr;

  if (!s_retds)
  {
    s_retds = reinterpret_cast<HB_RETDS>(hb_dllGetProcAddress("hb_retds"));
    if (!s_retds)
    {
      HB_DLL_MSG_NO_FUNC("hb_retds");
    }
  }

  if (s_retds)
  {
    s_retds(szDate);
  }
}

void hb_retd(int iYear, int iMonth, int iDay)
{
  static HB_RETD s_retd = nullptr;

  if (!s_retd)
  {
    s_retd = reinterpret_cast<HB_RETD>(hb_dllGetProcAddress("hb_retd"));
    if (!s_retd)
    {
      HB_DLL_MSG_NO_FUNC("hb_retd");
    }
  }

  if (s_retd)
  {
    s_retd(iYear, iMonth, iDay);
  }
}

void hb_retdl(long lJulian)
{
  static HB_RETDL s_retdl = nullptr;

  if (!s_retdl)
  {
    s_retdl = reinterpret_cast<HB_RETDL>(hb_dllGetProcAddress("hb_retdl"));
    if (!s_retdl)
    {
      HB_DLL_MSG_NO_FUNC("hb_retdl");
    }
  }

  if (s_retdl)
  {
    s_retdl(lJulian);
  }
}

void hb_retl(int iValue)
{
  static HB_RETL s_retl = nullptr;

  if (!s_retl)
  {
    s_retl = reinterpret_cast<HB_RETL>(hb_dllGetProcAddress("hb_retl"));
    if (!s_retl)
    {
      HB_DLL_MSG_NO_FUNC("hb_retl");
    }
  }

  if (s_retl)
  {
    s_retl(iValue);
  }
}

void hb_retnd(double dNumber)
{
  static HB_RETND s_retnd = nullptr;

  if (!s_retnd)
  {
    s_retnd = reinterpret_cast<HB_RETND>(hb_dllGetProcAddress("hb_retnd"));
    if (!s_retnd)
    {
      HB_DLL_MSG_NO_FUNC("hb_retnd");
    }
  }

  if (s_retnd)
  {
    s_retnd(dNumber);
  }
}

void hb_retni(int iNumber)
{
  static HB_RETNI s_retni = nullptr;

  if (!s_retni)
  {
    s_retni = reinterpret_cast<HB_RETNI>(hb_dllGetProcAddress("hb_retni"));
    if (!s_retni)
    {
      HB_DLL_MSG_NO_FUNC("hb_retni");
    }
  }

  if (s_retni)
  {
    s_retni(iNumber);
  }
}

void hb_retnl(long lNumber)
{
  static HB_RETNL s_retnl = nullptr;

  if (!s_retnl)
  {
    s_retnl = reinterpret_cast<HB_RETNL>(hb_dllGetProcAddress("hb_retnl"));
    if (!s_retnl)
    {
      HB_DLL_MSG_NO_FUNC("hb_retnl");
    }
  }

  if (s_retnl)
  {
    s_retnl(lNumber);
  }
}

void hb_retnlen(double dNumber, int iWidth, int iDec)
{
  static HB_RETNLEN s_retnlen = nullptr;

  if (!s_retnlen)
  {
    s_retnlen = reinterpret_cast<HB_RETNLEN>(hb_dllGetProcAddress("hb_retnlen"));
    if (!s_retnlen)
    {
      HB_DLL_MSG_NO_FUNC("hb_retnlen");
    }
  }

  if (s_retnlen)
  {
    s_retnlen(dNumber, iWidth, iDec);
  }
}

void hb_retndlen(double dNumber, int iWidth, int iDec)
{
  static HB_RETNDLEN s_retndlen = nullptr;

  if (!s_retndlen)
  {
    s_retndlen = reinterpret_cast<HB_RETNDLEN>(hb_dllGetProcAddress("hb_retndlen"));
    if (!s_retndlen)
    {
      HB_DLL_MSG_NO_FUNC("hb_retndlen");
    }
  }

  if (s_retndlen)
  {
    s_retndlen(dNumber, iWidth, iDec);
  }
}

void hb_retnilen(int iNumber, int iWidth)
{
  static HB_RETNILEN s_retnilen = nullptr;

  if (!s_retnilen)
  {
    s_retnilen = reinterpret_cast<HB_RETNILEN>(hb_dllGetProcAddress("hb_retnilen"));
    if (!s_retnilen)
    {
      HB_DLL_MSG_NO_FUNC("hb_retnilen");
    }
  }

  if (s_retnilen)
  {
    s_retnilen(iNumber, iWidth);
  }
}

void hb_retnllen(long lNumber, int iWidth)
{
  static HB_RETNLLEN s_retnllen = nullptr;

  if (!s_retnllen)
  {
    s_retnllen = reinterpret_cast<HB_RETNLLEN>(hb_dllGetProcAddress("hb_retnllen"));
    if (!s_retnllen)
    {
      HB_DLL_MSG_NO_FUNC("hb_retnllen");
    }
  }

  if (s_retnllen)
  {
    s_retnllen(lNumber, iWidth);
  }
}

void hb_reta(HB_SIZE nLen)
{
  static HB_RETA s_reta = nullptr;

  if (!s_reta)
  {
    s_reta = reinterpret_cast<HB_RETA>(hb_dllGetProcAddress("hb_reta"));
    if (!s_reta)
    {
      HB_DLL_MSG_NO_FUNC("hb_reta");
    }
  }

  if (s_reta)
  {
    s_reta(nLen);
  }
}

const char *hb_parvc(int iParam, ...)
{
  static HB_PARVC s_parvc = nullptr;

  if (!s_parvc)
  {
    s_parvc = reinterpret_cast<HB_PARVC>(hb_dllGetProcAddress("hb_parvc"));
    if (!s_parvc)
    {
      HB_DLL_MSG_NO_FUNC("hb_parvc");
    }
  }

  if (s_parvc)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_parvc(iParam, nArrayIndex);
  }

  return nullptr;
}

HB_SIZE hb_parvclen(int iParam, ...)
{
  static HB_PARVCLEN s_parvclen = nullptr;

  if (!s_parvclen)
  {
    s_parvclen = reinterpret_cast<HB_PARVCLEN>(hb_dllGetProcAddress("hb_parvclen"));
    if (!s_parvclen)
    {
      HB_DLL_MSG_NO_FUNC("hb_parvclen");
    }
  }

  if (s_parvclen)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_parvclen(iParam, nArrayIndex);
  }

  return 0;
}

HB_SIZE hb_parvcsiz(int iParam, ...)
{
  static HB_PARVCSIZ s_parvcsiz = nullptr;

  if (!s_parvcsiz)
  {
    s_parvcsiz = reinterpret_cast<HB_PARVCSIZ>(hb_dllGetProcAddress("hb_parvcsiz"));
    if (!s_parvcsiz)
    {
      HB_DLL_MSG_NO_FUNC("hb_parvcsiz");
    }
  }

  if (s_parvcsiz)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_parvcsiz(iParam, nArrayIndex);
  }

  return 0;
}

const char *hb_parvds(int iParam, ...)
{
  static HB_PARVDS s_parvds = nullptr;

  if (!s_parvds)
  {
    s_parvds = reinterpret_cast<HB_PARVDS>(hb_dllGetProcAddress("hb_parvds"));
    if (!s_parvds)
    {
      HB_DLL_MSG_NO_FUNC("hb_parvds");
    }
  }

  if (s_parvds)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_parvds(iParam, nArrayIndex);
  }

  return "        ";
}

char *hb_parvdsbuff(char *szDate, int iParam, ...)
{
  static HB_PARVDSBUFF s_parvdsbuff = nullptr;

  if (!s_parvdsbuff)
  {
    s_parvdsbuff = reinterpret_cast<HB_PARVDSBUFF>(hb_dllGetProcAddress("hb_parvdsbuff"));
    if (!s_parvdsbuff)
    {
      HB_DLL_MSG_NO_FUNC("hb_parvdsbuff");
    }
  }

  if (s_parvdsbuff)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_parvdsbuff(szDate, iParam, nArrayIndex);
  }

  return szDate;
}

int hb_parvl(int iParam, ...)
{
  static HB_PARVL s_parvl = nullptr;

  if (!s_parvl)
  {
    s_parvl = reinterpret_cast<HB_PARVL>(hb_dllGetProcAddress("hb_parvl"));
    if (!s_parvl)
    {
      HB_DLL_MSG_NO_FUNC("hb_parvl");
    }
  }

  if (s_parvl)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_parvl(iParam, nArrayIndex);
  }

  return 0;
}

double hb_parvnd(int iParam, ...)
{
  static HB_PARVND s_parvnd = nullptr;

  if (!s_parvnd)
  {
    s_parvnd = reinterpret_cast<HB_PARVND>(hb_dllGetProcAddress("hb_parvnd"));
    if (!s_parvnd)
    {
      HB_DLL_MSG_NO_FUNC("hb_parvnd");
    }
  }

  if (s_parvnd)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_parvnd(iParam, nArrayIndex);
  }

  return 0;
}

int hb_parvni(int iParam, ...)
{
  static HB_PARVNI s_parvni = nullptr;

  if (!s_parvni)
  {
    s_parvni = reinterpret_cast<HB_PARVNI>(hb_dllGetProcAddress("hb_parvni"));
    if (!s_parvni)
    {
      HB_DLL_MSG_NO_FUNC("hb_parvni");
    }
  }

  if (s_parvni)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_parvni(iParam, nArrayIndex);
  }

  return 0;
}

long hb_parvnl(int iParam, ...)
{
  static HB_PARVNL s_parvnl = nullptr;

  if (!s_parvnl)
  {
    s_parvnl = reinterpret_cast<HB_PARVNL>(hb_dllGetProcAddress("hb_parvnl"));
    if (!s_parvnl)
    {
      HB_DLL_MSG_NO_FUNC("hb_parvnl");
    }
  }

  if (s_parvnl)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_parvnl(iParam, nArrayIndex);
  }

  return 0;
}

int hb_storvc(const char *szText, int iParam, ...)
{
  static HB_STORVC s_storvc = nullptr;

  if (!s_storvc)
  {
    s_storvc = reinterpret_cast<HB_STORVC>(hb_dllGetProcAddress("hb_storvc"));
    if (!s_storvc)
    {
      HB_DLL_MSG_NO_FUNC("hb_storvc");
    }
  }

  if (s_storvc)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_storvc(szText, iParam, nArrayIndex);
  }

  return 0;
}

int hb_storvclen(const char *szText, HB_SIZE nLen, int iParam, ...)
{
  static HB_STORVCLEN s_storvclen = nullptr;

  if (!s_storvclen)
  {
    s_storvclen = reinterpret_cast<HB_STORVCLEN>(hb_dllGetProcAddress("hb_storvclen"));
    if (!s_storvclen)
    {
      HB_DLL_MSG_NO_FUNC("hb_storvclen");
    }
  }

  if (s_storvclen)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_storvclen(szText, nLen, iParam, nArrayIndex);
  }

  return 0;
}

int hb_storvds(const char *szDate, int iParam, ...)
{
  static HB_STORVDS s_storvds = nullptr;

  if (!s_storvds)
  {
    s_storvds = reinterpret_cast<HB_STORVDS>(hb_dllGetProcAddress("hb_storvds"));
    if (!s_storvds)
    {
      HB_DLL_MSG_NO_FUNC("hb_storvds");
    }
  }

  if (s_storvds)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_storvds(szDate, iParam, nArrayIndex);
  }

  return 0;
}

int hb_storvl(int iLogical, int iParam, ...)
{
  static HB_STORVL s_storvl = nullptr;

  if (!s_storvl)
  {
    s_storvl = reinterpret_cast<HB_STORVL>(hb_dllGetProcAddress("hb_storvl"));
    if (!s_storvl)
    {
      HB_DLL_MSG_NO_FUNC("hb_storvl");
    }
  }

  if (s_storvl)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_storvl(iLogical, iParam, nArrayIndex);
  }

  return 0;
}

int hb_storvni(int iValue, int iParam, ...)
{
  static HB_STORVNI s_storvni = nullptr;

  if (!s_storvni)
  {
    s_storvni = reinterpret_cast<HB_STORVNI>(hb_dllGetProcAddress("hb_storvni"));
    if (!s_storvni)
    {
      HB_DLL_MSG_NO_FUNC("hb_storvni");
    }
  }

  if (s_storvni)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_storvni(iValue, iParam, nArrayIndex);
  }

  return 0;
}

int hb_storvnl(long lValue, int iParam, ...)
{
  static HB_STORVNL s_storvnl = nullptr;

  if (!s_storvnl)
  {
    s_storvnl = reinterpret_cast<HB_STORVNL>(hb_dllGetProcAddress("hb_storvnl"));
    if (!s_storvnl)
    {
      HB_DLL_MSG_NO_FUNC("hb_storvnl");
    }
  }

  if (s_storvnl)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_storvnl(lValue, iParam, nArrayIndex);
  }

  return 0;
}

int hb_storvnd(double dNumber, int iParam, ...)
{
  static HB_STORVND s_storvnd = nullptr;

  if (!s_storvnd)
  {
    s_storvnd = reinterpret_cast<HB_STORVND>(hb_dllGetProcAddress("hb_storvnd"));
    if (!s_storvnd)
    {
      HB_DLL_MSG_NO_FUNC("hb_storvnd");
    }
  }

  if (s_storvnd)
  {
    HB_SIZE nArrayIndex = 0;

    if (hb_extIsArray(iParam))
    {
      va_list va;
      va_start(va, iParam);
      nArrayIndex = va_arg(va, HB_SIZE);
      va_end(va);
    }

    return s_storvnd(dNumber, iParam, nArrayIndex);
  }

  return 0;
}

HB_BOOL hb_arrayNew(PHB_ITEM pItem, HB_SIZE nLen)
{
  static HB_ARRAYNEW s_arrayNew = nullptr;

  if (!s_arrayNew)
  {
    s_arrayNew = reinterpret_cast<HB_ARRAYNEW>(hb_dllGetProcAddress("hb_arrayNew"));
    if (!s_arrayNew)
    {
      HB_DLL_MSG_NO_FUNC("hb_arrayNew");
    }
  }
  return s_arrayNew ? s_arrayNew(pItem, nLen) : false;
}

HB_SIZE hb_arrayLen(PHB_ITEM pArray)
{
  static HB_ARRAYLEN s_arrayLen = nullptr;

  if (!s_arrayLen)
  {
    s_arrayLen = reinterpret_cast<HB_ARRAYLEN>(hb_dllGetProcAddress("hb_arrayLen"));
    if (!s_arrayLen)
    {
      HB_DLL_MSG_NO_FUNC("hb_arrayLen");
    }
  }
  return s_arrayLen ? s_arrayLen(pArray) : 0;
}

HB_BOOL hb_arrayIsObject(PHB_ITEM pArray)
{
  static HB_ARRAYISOBJECT s_arrayIsObject = nullptr;

  if (!s_arrayIsObject)
  {
    s_arrayIsObject = reinterpret_cast<HB_ARRAYISOBJECT>(hb_dllGetProcAddress("hb_arrayIsObject"));
    if (!s_arrayIsObject)
    {
      HB_DLL_MSG_NO_FUNC("hb_arrayIsObject");
    }
  }
  return s_arrayIsObject ? s_arrayIsObject(pArray) : false;
}

HB_BOOL hb_arrayAdd(PHB_ITEM pArray, PHB_ITEM pItem)
{
  static HB_ARRAYADD s_arrayAdd = nullptr;

  if (!s_arrayAdd)
  {
    s_arrayAdd = reinterpret_cast<HB_ARRAYADD>(hb_dllGetProcAddress("hb_arrayAdd"));
    if (!s_arrayAdd)
    {
      HB_DLL_MSG_NO_FUNC("hb_arrayAdd");
    }
  }
  return s_arrayAdd ? s_arrayAdd(pArray, pItem) : false;
}

HB_BOOL hb_arrayIns(PHB_ITEM pArray, HB_SIZE nIndex)
{
  static HB_ARRAYINS s_arrayIns = nullptr;

  if (!s_arrayIns)
  {
    s_arrayIns = reinterpret_cast<HB_ARRAYINS>(hb_dllGetProcAddress("hb_arrayIns"));
    if (!s_arrayIns)
    {
      HB_DLL_MSG_NO_FUNC("hb_arrayIns");
    }
  }
  return s_arrayIns ? s_arrayIns(pArray, nIndex) : false;
}

HB_BOOL hb_arrayDel(PHB_ITEM pArray, HB_SIZE nIndex)
{
  static HB_ARRAYDEL s_arrayDel = nullptr;

  if (!s_arrayDel)
  {
    s_arrayDel = reinterpret_cast<HB_ARRAYDEL>(hb_dllGetProcAddress("hb_arrayDel"));
    if (!s_arrayDel)
    {
      HB_DLL_MSG_NO_FUNC("hb_arrayDel");
    }
  }
  return s_arrayDel ? s_arrayDel(pArray, nIndex) : false;
}

HB_BOOL hb_arraySize(PHB_ITEM pArray, HB_SIZE nLen)
{
  static HB_ARRAYSIZE s_arraySize = nullptr;

  if (!s_arraySize)
  {
    s_arraySize = reinterpret_cast<HB_ARRAYSIZE>(hb_dllGetProcAddress("hb_arraySize"));
    if (!s_arraySize)
    {
      HB_DLL_MSG_NO_FUNC("hb_arraySize");
    }
  }
  return s_arraySize ? s_arraySize(pArray, nLen) : false;
}

HB_BOOL hb_arrayLast(PHB_ITEM pArray, PHB_ITEM pResult)
{
  static HB_ARRAYLAST s_arrayLast = nullptr;

  if (!s_arrayLast)
  {
    s_arrayLast = reinterpret_cast<HB_ARRAYLAST>(hb_dllGetProcAddress("hb_arrayLast"));
    if (!s_arrayLast)
    {
      HB_DLL_MSG_NO_FUNC("hb_arrayLast");
    }
  }
  return s_arrayLast ? s_arrayLast(pArray, pResult) : false;
}

HB_BOOL hb_arraySet(PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem)
{
  static HB_ARRAYSET s_arraySet = nullptr;

  if (!s_arraySet)
  {
    s_arraySet = reinterpret_cast<HB_ARRAYSET>(hb_dllGetProcAddress("hb_arraySet"));
    if (!s_arraySet)
    {
      HB_DLL_MSG_NO_FUNC("hb_arraySet");
    }
  }
  return s_arraySet ? s_arraySet(pArray, nIndex, pItem) : false;
}

HB_BOOL hb_arrayGet(PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem)
{
  static HB_ARRAYGET s_arrayGet = nullptr;

  if (!s_arrayGet)
  {
    s_arrayGet = reinterpret_cast<HB_ARRAYGET>(hb_dllGetProcAddress("hb_arrayGet"));
    if (!s_arrayGet)
    {
      HB_DLL_MSG_NO_FUNC("hb_arrayGet");
    }
  }
  return s_arrayGet ? s_arrayGet(pArray, nIndex, pItem) : false;
}

void *hb_xalloc(HB_SIZE nSize)
{
  static HB_XALLOC s_xalloc = nullptr;

  if (!s_xalloc)
  {
    s_xalloc = reinterpret_cast<HB_XALLOC>(hb_dllGetProcAddress("hb_xalloc"));
    if (!s_xalloc)
    {
      HB_DLL_MSG_NO_FUNC("hb_xalloc");
    }
  }

  return s_xalloc ? s_xalloc(nSize) : nullptr;
}

void *hb_xgrab(HB_SIZE nSize)
{
  static HB_XGRAB s_xgrab = nullptr;

  if (!s_xgrab)
  {
    s_xgrab = reinterpret_cast<HB_XGRAB>(hb_dllGetProcAddress("hb_xgrab"));
    if (!s_xgrab)
    {
      HB_DLL_MSG_NO_FUNC("hb_xgrab");
    }
  }

  return s_xgrab ? s_xgrab(nSize) : nullptr;
}

void hb_xfree(void *pMem)
{
  static HB_XFREE s_xfree = nullptr;

  if (!s_xfree)
  {
    s_xfree = reinterpret_cast<HB_XFREE>(hb_dllGetProcAddress("hb_xfree"));
    if (!s_xfree)
    {
      HB_DLL_MSG_NO_FUNC("hb_xfree");
    }
  }

  if (s_xfree)
  {
    s_xfree(pMem);
  }
}

void *hb_xrealloc(void *pMem, HB_SIZE nSize)
{
  static HB_XREALLOC s_xrealloc = nullptr;

  if (!s_xrealloc)
  {
    s_xrealloc = reinterpret_cast<HB_XREALLOC>(hb_dllGetProcAddress("hb_xrealloc"));
    if (!s_xrealloc)
    {
      HB_DLL_MSG_NO_FUNC("hb_xrealloc");
    }
  }

  return s_xrealloc ? s_xrealloc(pMem, nSize) : nullptr;
}

void hb_macroTextValue(PHB_ITEM pItem)
{
  static HB_MACROTEXTVALUE s_macroTextValue = nullptr;

  if (!s_macroTextValue)
  {
    s_macroTextValue = reinterpret_cast<HB_MACROTEXTVALUE>(hb_dllGetProcAddress("macroTextValue"));
    if (!s_macroTextValue)
    {
      HB_DLL_MSG_NO_FUNC("macroTextValue");
    }
  }

  if (s_macroTextValue)
  {
    s_macroTextValue(pItem);
  }
}

#endif /* HB_OS_WIN */
