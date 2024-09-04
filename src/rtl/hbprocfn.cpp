//
// .prg level functions to create, wait and terminate processes
//
// Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
// based on xHarbour code by
// Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
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
#include "hbapifs.hpp"
#include "hbapierr.hpp"

HB_FUNC(HB_PROCESSOPEN)
{
  auto szName = hb_parc(1);
  auto pStdIn = hb_param(2, Harbour::Item::BYREF);
  auto pStdOut = hb_param(3, Harbour::Item::BYREF);
  auto pStdErr = hb_param(4, Harbour::Item::BYREF);

  if (szName != nullptr && (pStdIn || HB_ISNIL(2)) && (pStdOut || HB_ISNIL(3)) && (pStdErr || HB_ISNIL(4)) &&
      (HB_ISLOG(5) || HB_ISNIL(5)) && (HB_ISBYREF(6) || HB_ISNIL(6)) &&
      (!pStdIn || (pStdIn != pStdOut && pStdIn != pStdErr)))
  {
    bool fDetach = hb_parl(5);
    HB_FHANDLE hStdIn, *phStdIn, hStdOut, *phStdOut, hStdErr, *phStdErr;
    HB_ULONG ulPID;

    phStdIn = pStdIn ? &hStdIn : nullptr;
    phStdOut = pStdOut ? &hStdOut : nullptr;
    phStdErr = pStdErr ? (pStdOut == pStdErr ? phStdOut : &hStdErr) : nullptr;

    HB_FHANDLE hProcess = hb_fsProcessOpen(szName, phStdIn, phStdOut, phStdErr, fDetach, &ulPID);
    hb_fsSetFError(hb_fsError());
    if (hProcess != FS_ERROR)
    {
      if (phStdIn)
      {
        hb_stornint(static_cast<HB_NHANDLE>(*phStdIn), 2);
      }
      if (phStdOut)
      {
        hb_stornint(static_cast<HB_NHANDLE>(*phStdOut), 3);
      }
      if (phStdErr && phStdOut != phStdErr)
      {
        hb_stornint(static_cast<HB_NHANDLE>(*phStdErr), 4);
      }
      hb_stornint(ulPID, 6);
    }
    hb_retnint(static_cast<HB_NHANDLE>(hProcess));
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 4001, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_PROCESSVALUE)
{
  HB_FHANDLE hProcess = hb_numToHandle(hb_parnint(1));

  if (hProcess != 0 && hProcess != FS_ERROR && (hb_pcount() < 2 || HB_ISLOG(2)))
  {
    int iResult = hb_fsProcessValue(hProcess, hb_pcount() < 2 || hb_parl(2));
    hb_fsSetFError(hb_fsError());
    hb_retni(iResult);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 4001, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

HB_FUNC(HB_PROCESSCLOSE)
{
  HB_FHANDLE hProcess = hb_numToHandle(hb_parnint(1));

  if (hProcess != 0 && hProcess != FS_ERROR && (hb_pcount() < 2 || HB_ISLOG(2)))
  {
    bool fResult = hb_fsProcessClose(hProcess, hb_pcount() < 2 || hb_parl(2));
    hb_fsSetFError(hb_fsError());
    hb_retl(fResult);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 4001, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}

/* hb_processRun(<cCommand>, [ <cStdIn> ], [ @<cStdOut> ], [ @<cStdErr> ], [ <lDetach> ]) --> <nResult> */
HB_FUNC(HB_PROCESSRUN)
{
  auto szName = hb_parc(1);
  auto szStdIn = hb_parc(2);
  auto pStdOut = hb_param(3, Harbour::Item::BYREF);
  auto pStdErr = hb_param(4, Harbour::Item::BYREF);
  bool fDetach = hb_parl(5);

  if (szName != nullptr && (szStdIn || HB_ISNIL(2)) && (pStdOut || HB_ISNIL(3)) && (pStdErr || HB_ISNIL(4)) &&
      (HB_ISLOG(5) || HB_ISNIL(5)))
  {
    HB_SIZE nStdOut, nStdErr;
    char *pStdOutBuf, *pStdErrBuf;
    char **pStdOutPtr, **pStdErrPtr;

    nStdOut = nStdErr = 0;
    pStdOutBuf = pStdErrBuf = nullptr;
    pStdOutPtr = pStdOut ? &pStdOutBuf : nullptr;
    pStdErrPtr = pStdErr ? (pStdOut == pStdErr ? pStdOutPtr : &pStdErrBuf) : nullptr;

    int iResult = hb_fsProcessRun(szName, szStdIn, hb_parclen(2), pStdOutPtr, &nStdOut, pStdErrPtr, &nStdErr, fDetach);
    hb_fsSetFError(hb_fsError());

    if (pStdOutBuf)
    {
      if (!hb_storclen_buffer(pStdOutBuf, nStdOut, 3))
      {
        hb_xfree(pStdOutBuf);
      }
    }
    else if (pStdOut)
    {
      hb_storc(nullptr, 3);
    }

    if (pStdErrBuf)
    {
      if (!hb_storclen_buffer(pStdErrBuf, nStdErr, 4))
      {
        hb_xfree(pStdErrBuf);
      }
    }
    else if (pStdErr && pStdOut != pStdErr)
    {
      hb_storc(nullptr, 4);
    }

    hb_retni(iResult);
  }
  else
  {
    hb_errRT_BASE_SubstR(EG_ARG, 4001, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
  }
}
