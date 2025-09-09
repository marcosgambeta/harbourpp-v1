//
// xhb_CopyFile() function
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

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

#include <hbapi.hpp>
#include <hbapierr.hpp>
#include <hbapifs.hpp>
#include <hbapiitm.hpp>
#include <hbvm.hpp>

#if defined(HB_OS_UNIX)
#include <sys/stat.h>
#include <unistd.h>
#endif

#define BUFFER_SIZE 8192

static HB_BOOL hb_copyfile(const char *pszSource, const char *pszDest, PHB_ITEM pBlock)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_copyfile(%s, %s, %p)", pszSource, pszDest, static_cast<void*>(pBlock)));
#endif

  HB_BOOL bRetVal = false;
  PHB_FILE pSource;
  PHB_ITEM pError = nullptr;

  do {
    pSource = hb_fileExtOpen(pszSource, nullptr, FO_READ | FO_SHARED | FO_PRIVATE | FXO_DEFAULTS | FXO_SHARELOCK,
                             nullptr, pError);
    if (pSource == nullptr) {
      pError = hb_errRT_FileError(pError, nullptr, EG_OPEN, 2012, pszSource);
      if (hb_errLaunch(pError) != E_RETRY) {
        break;
      }
    }
  } while (pSource == nullptr);

  if (pError) {
    hb_itemRelease(pError);
    pError = nullptr;
  }

  if (pSource != nullptr) {
    PHB_FILE pDest;

    do {
      pDest = hb_fileExtOpen(pszDest, nullptr,
                             FO_READWRITE | FO_EXCLUSIVE | FO_PRIVATE | FXO_TRUNCATE | FXO_DEFAULTS | FXO_SHARELOCK,
                             nullptr, pError);
      if (pDest == nullptr) {
        pError = hb_errRT_FileError(pError, nullptr, EG_CREATE, 2012, pszDest);
        if (hb_errLaunch(pError) != E_RETRY) {
          break;
        }
      }
    } while (pDest == nullptr);

    if (pError) {
      hb_itemRelease(pError);
      pError = nullptr;
    }

    if (pDest != nullptr) {
      PHB_ITEM pCount = nullptr;
      HB_SIZE nRead;

      auto buffer = static_cast<HB_UCHAR *>(hb_xgrab(BUFFER_SIZE));
      bRetVal = true;
      if (pBlock && pBlock->isEvalItem()) {
        pCount = hb_itemNew(nullptr);
      }

      while ((nRead = hb_fileRead(pSource, buffer, BUFFER_SIZE, -1)) != 0 && nRead != static_cast<HB_SIZE>(FS_ERROR)) {
        HB_SIZE nWritten = 0;

        while (nWritten < nRead) {
          HB_SIZE nDone = hb_fileWrite(pDest, buffer + nWritten, nRead - nWritten, -1);
          if (nDone != static_cast<HB_SIZE>(FS_ERROR)) {
            nWritten += nDone;
          }
          if (nWritten < nRead) {
            pError = hb_errRT_FileError(pError, nullptr, EG_WRITE, 2016, pszDest);
            if (hb_errLaunch(pError) != E_RETRY) {
              bRetVal = false;
              break;
            }
          }
        }

        if (pCount) {
          hb_vmEvalBlockV(pBlock, 1, hb_itemPutNInt(pCount, nRead));
        }
      }

      if (pError) {
        hb_itemRelease(pError);
      }

      if (pCount) {
        hb_itemRelease(pCount);
      }

      hb_xfree(buffer);

      hb_fileClose(pDest);
    }

    hb_fileClose(pSource);

    if (bRetVal) {
      long lJulian, lMillisec;
      HB_FATTR ulAttr;

      if (hb_fileAttrGet(pszSource, &ulAttr)) {
        hb_fileAttrSet(pszDest, ulAttr);
      }
      if (hb_fileTimeGet(pszSource, &lJulian, &lMillisec)) {
        hb_fileTimeSet(pszDest, lJulian, lMillisec);
      }
    }
  }

  return bRetVal;
}

/* Clipper returns .F. on failure and NIL on success */

HB_FUNC(XHB_COPYFILE)
{
  auto szSource = hb_parc(1);
  auto szDest = hb_parc(2);

  if (szSource && szDest) {
    if (!hb_copyfile(szSource, szDest, hb_param(3, Harbour::Item::EVALITEM))) {
      hb_retl(false);
    }
  } else {
    hb_errRT_BASE(EG_ARG, 2010, nullptr, HB_ERR_FUNCNAME,
                  HB_ERR_ARGS_BASEPARAMS); /* NOTE: Undocumented but existing Clipper Run-time error */
  }
}
