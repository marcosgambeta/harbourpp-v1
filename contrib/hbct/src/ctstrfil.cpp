//
// StrFile(), FileStr(), ScreenFile(), FileScreen()
// SetFCreate(), CSetSafety()
//
// Copyright 2004 Pavel Tsarenko <tpe2@mail.ru>
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
#include <hbapifs.hpp>
#include <hbapigt.hpp>

#include "ctstrfil.h"

static HB_FATTR s_nFileAttr = HB_FA_NORMAL;
static HB_BOOL s_bSafety = false;

void ct_setfcreate(HB_FATTR nFileAttr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("ct_setfcreate(%u)", nFileAttr));
#endif

  s_nFileAttr = nFileAttr;
}

HB_FATTR ct_getfcreate(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("ct_getfcreate()"));
#endif

  return s_nFileAttr;
}

HB_FUNC(SETFCREATE)
{
  hb_retnl(ct_getfcreate());

  if (HB_ISNUM(1))
  {
    ct_setfcreate(hb_parnl(1));
  }
}

void ct_setsafety(HB_BOOL bSafety)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("ct_setsafety(%i)", bSafety));
#endif

  s_bSafety = bSafety;
}

HB_BOOL ct_getsafety(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("ct_getsafety()"));
#endif

  return s_bSafety;
}

HB_FUNC(CSETSAFETY)
{
  hb_retl(ct_getsafety());

  if (HB_ISLOG(1))
  {
    ct_setsafety(hb_parl(1));
  }
}

static HB_SIZE ct_StrFile(const char *pFileName, const char *pcStr, HB_SIZE nLen, HB_BOOL bOverwrite,
                          HB_FOFFSET nOffset, HB_BOOL bTrunc)
{
  HB_FHANDLE hFile;
  HB_BOOL bOpen = false;
  HB_BOOL bFile = hb_fsFile(pFileName);
  HB_SIZE nWrite = 0;

  if (bFile && bOverwrite)
  {
    hFile = hb_fsOpen(pFileName, FO_READWRITE);
    bOpen = true;
  }
  else if (!bFile || !ct_getsafety())
  {
    hFile = hb_fsCreate(pFileName, ct_getfcreate());
  }
  else
  {
    hFile = FS_ERROR;
  }

  if (hFile != FS_ERROR)
  {
    if (nOffset)
    {
      hb_fsSeekLarge(hFile, nOffset, FS_SET);
    }
    else if (bOpen)
    {
      hb_fsSeek(hFile, 0, FS_END);
    }

    nWrite = hb_fsWriteLarge(hFile, pcStr, nLen);
    if ((nWrite == nLen) && bOpen && bTrunc)
    {
      hb_fsWrite(hFile, nullptr, 0);
    }

    hb_fsClose(hFile);
  }
  return nWrite;
}

HB_FUNC(STRFILE)
{
  if (HB_ISCHAR(1) && HB_ISCHAR(2))
  {
    hb_retns(ct_StrFile(hb_parc(2), hb_parc(1), hb_parclen(1), hb_parl(3), static_cast<HB_FOFFSET>(hb_parnint(4)),
                        hb_parl(5)));
  }
  else
  {
    hb_retns(0);
  }
}

HB_FUNC(FILESTR)
{
  if (HB_ISCHAR(1))
  {
    HB_FHANDLE hFile = hb_fsOpen(hb_parc(1), FO_READ);

    if (hFile != FS_ERROR)
    {
      HB_FOFFSET nFileSize = hb_fsSeekLarge(hFile, 0, FS_END);
      HB_FOFFSET nPos = hb_fsSeekLarge(hFile, static_cast<HB_FOFFSET>(hb_parnint(3)), FS_SET);
      HB_ISIZ nLength;
      char *pCtrlZ;
      HB_BOOL bCtrlZ = hb_parl(4);

      if (HB_ISNUM(2))
      {
        nLength = hb_parns(2);
        if (nLength > static_cast<HB_ISIZ>(nFileSize - nPos))
        {
          nLength = static_cast<HB_ISIZ>(nFileSize - nPos);
        }
      }
      else
      {
        nLength = static_cast<HB_ISIZ>(nFileSize - nPos);
      }

      auto pcResult = static_cast<char *>(hb_xgrab(nLength + 1));
      if (nLength > 0)
      {
        nLength = hb_fsReadLarge(hFile, pcResult, static_cast<HB_SIZE>(nLength));
      }

      if (bCtrlZ)
      {
        pCtrlZ = static_cast<char *>(memchr(pcResult, 26, nLength));
        if (pCtrlZ)
        {
          nLength = pCtrlZ - pcResult;
        }
      }

      hb_fsClose(hFile);
      hb_retclen_buffer(pcResult, nLength);
    }
    else
    {
      hb_retc_null();
    }
  }
  else
  {
    hb_retc_null();
  }
}

HB_FUNC(SCREENFILE)
{
  if (HB_ISCHAR(1))
  {
    HB_SIZE nSize;

    hb_gtRectSize(0, 0, hb_gtMaxRow(), hb_gtMaxCol(), &nSize);
    auto pBuffer = static_cast<char *>(hb_xgrab(nSize));

    hb_gtSave(0, 0, hb_gtMaxRow(), hb_gtMaxCol(), pBuffer);

    hb_retns(ct_StrFile(hb_parc(1), pBuffer, nSize, hb_parl(2), static_cast<HB_FOFFSET>(hb_parnint(3)), hb_parl(4)));
    hb_xfree(pBuffer);
  }
  else
  {
    hb_retns(0);
  }
}

HB_FUNC(FILESCREEN)
{
  if (HB_ISCHAR(1))
  {
    HB_FHANDLE hFile = hb_fsOpen(hb_parc(1), FO_READ);

    if (hFile != FS_ERROR)
    {
      HB_SIZE nSize;
      HB_SIZE nLength;

      if (HB_ISNUM(2))
      {
        hb_fsSeekLarge(hFile, static_cast<HB_FOFFSET>(hb_parnint(2)), FS_SET);
      }

      hb_gtRectSize(0, 0, hb_gtMaxRow(), hb_gtMaxCol(), &nSize);
      auto pBuffer = static_cast<char *>(hb_xgrab(nSize));

      nLength = hb_fsReadLarge(hFile, pBuffer, nSize);
      hb_gtRest(0, 0, hb_gtMaxRow(), hb_gtMaxCol(), pBuffer);

      hb_xfree(pBuffer);

      hb_fsClose(hFile);
      hb_retns(nLength);
    }
    else
    {
      hb_retns(0);
    }
  }
  else
  {
    hb_retns(0);
  }
}
