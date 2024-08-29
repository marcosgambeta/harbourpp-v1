//
// hb_fsCopy(), hb_FCopy() functions
//
// Copyright 1991-2008 Viktor Szakats (vszakats.net/harbour)
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
#include "hbvm.hpp"
#include "hbapifs.hpp"

#define HB_FSCOPY_BUFFERSIZE 65536

HB_BOOL hb_fsCopy(const char *pszSource, const char *pszDest)
{
  auto fResult = false;
  PHB_FILE pSrcFile;

  if ((pSrcFile = hb_fileExtOpen(pszSource, nullptr, FO_READ | FO_SHARED | FXO_SHARELOCK, nullptr, nullptr)) != nullptr)
  {
    PHB_FILE pDstFile;
    HB_ERRCODE errCode;

    if ((pDstFile = hb_fileExtOpen(pszDest, nullptr, FXO_TRUNCATE | FO_READWRITE | FO_EXCLUSIVE | FXO_SHARELOCK,
                                   nullptr, nullptr)) != nullptr)
    {
      auto pbyBuffer = hb_xgrab(HB_FSCOPY_BUFFERSIZE);

      for (;;)
      {
        HB_SIZE nBytesRead;
        if ((nBytesRead = hb_fileRead(pSrcFile, pbyBuffer, HB_FSCOPY_BUFFERSIZE, -1)) > 0 &&
            nBytesRead != static_cast<HB_SIZE>(FS_ERROR))
        {
          if (nBytesRead != hb_fileWrite(pDstFile, pbyBuffer, nBytesRead, -1))
          {
            errCode = hb_fsError();
            break;
          }
        }
        else
        {
          errCode = hb_fsError();
          fResult = errCode == 0;
          break;
        }
      }

      hb_xfree(pbyBuffer);

      hb_fileClose(pDstFile);
    }
    else
    {
      errCode = hb_fsError();
    }

    hb_fileClose(pSrcFile);

    if (fResult)
    {
      HB_FATTR ulAttr;

      if (hb_fileAttrGet(pszSource, &ulAttr))
      {
        hb_fileAttrSet(pszDest, ulAttr);
      }
    }
    hb_fsSetError(errCode);
  }

  return fResult;
}

HB_BOOL hb_fileCopyEx(const char *pszSource, const char *pszDest, HB_SIZE nBufSize, HB_BOOL fTime, PHB_ITEM pCallBack)
{
  auto fResult = false;
  PHB_FILE pSrcFile;

  if (pCallBack && !HB_IS_EVALITEM(pCallBack))
  {
    pCallBack = nullptr;
  }

  if ((pSrcFile = hb_fileExtOpen(pszSource, nullptr, FO_READ | FO_SHARED | FXO_SHARELOCK, nullptr, nullptr)) != nullptr)
  {
    PHB_FILE pDstFile;
    HB_ERRCODE errCode = 0;

    if ((pDstFile = hb_fileExtOpen(pszDest, nullptr, FXO_TRUNCATE | FO_READWRITE | FO_EXCLUSIVE | FXO_SHARELOCK,
                                   nullptr, nullptr)) != nullptr)
    {
      HB_SIZE nTotal = pCallBack ? hb_fileSize(pSrcFile) : 0, nWritten = 0,
              nSize = nBufSize > 0 ? nBufSize : HB_FSCOPY_BUFFERSIZE;
      void *pbyBuffer = hb_xgrab(nSize);

      if (pCallBack)
      {
        hb_vmPushEvalSym();
        hb_vmPush(pCallBack);
        hb_vmPushInteger(0);
        hb_vmPushNumInt(static_cast<HB_MAXINT>(nTotal));
        hb_vmSend(2);
      }

      while (pCallBack == nullptr || hb_vmRequestQuery() == 0)
      {
        HB_SIZE nBytesRead;

        if ((nBytesRead = hb_fileRead(pSrcFile, pbyBuffer, nSize, -1)) > 0 &&
            nBytesRead != static_cast<HB_SIZE>(FS_ERROR))
        {
          if (nBytesRead != hb_fileWrite(pDstFile, pbyBuffer, nBytesRead, -1))
          {
            errCode = hb_fsError();
            break;
          }
          nWritten += nBytesRead;
          if (pCallBack)
          {
            hb_vmPushEvalSym();
            hb_vmPush(pCallBack);
            hb_vmPushNumInt(static_cast<HB_MAXINT>(nWritten));
            hb_vmPushNumInt(static_cast<HB_MAXINT>(nTotal));
            hb_vmSend(2);
          }
        }
        else
        {
          errCode = hb_fsError();
          fResult = errCode == 0;
          break;
        }
      }

      hb_xfree(pbyBuffer);

      hb_fileClose(pDstFile);
    }
    else
    {
      errCode = hb_fsError();
    }

    hb_fileClose(pSrcFile);

    if (fResult)
    {
      HB_FATTR ulAttr;

      if (hb_fileAttrGet(pszSource, &ulAttr))
      {
        hb_fileAttrSet(pszDest, ulAttr);
      }

      if (fTime)
      {
        long lJulian, lMillisec;

        if (hb_fileTimeGet(pszSource, &lJulian, &lMillisec))
        {
          hb_fileTimeSet(pszDest, lJulian, lMillisec);
        }
      }
    }
    hb_fsSetError(errCode);
  }

  return fResult;
}

HB_FUNC(HB_FCOPY)
{
  HB_ERRCODE errCode = 2; /* file not found */
  auto fResult = false;
  auto pszSource = hb_parc(1);
  auto pszDest = hb_parc(2);

  if (pszSource && pszDest)
  {
    fResult = hb_fsCopy(pszSource, pszDest);
    errCode = hb_fsError();
  }
  hb_fsSetFError(errCode);
  hb_retni(fResult ? 0 : F_ERROR);
}
