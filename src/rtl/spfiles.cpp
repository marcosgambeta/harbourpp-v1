//
// A search path shim for the FileSys API (C level)
//
// Copyright 2001 David G. Holm <dholm@jsd-llc.com>
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

#include "hbapifs.hpp"
#include "hbset.hpp"

HB_BOOL hb_spFile(const char *pszFileName, char *pszRetPath)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_spFile(%s, %p)", pszFileName, static_cast<void*>(pszRetPath)));
#endif

  char *pszPath;
  HB_BOOL bIsFile = false;
  PHB_FNAME pFilepath;

  if (pszRetPath) {
    pszPath = pszRetPath;
  } else {
    pszPath = static_cast<char *>(hb_xgrab(HB_PATH_MAX));
  }

  pFilepath = hb_fsFNameSplit(pszFileName);

  if (pFilepath->szPath) {
    hb_fsFNameMerge(pszPath, pFilepath);
    bIsFile = hb_fsFile(pszPath);
  } else {
    const char *szDefault = hb_setGetDefault();
    if (szDefault != nullptr) {
      pFilepath->szPath = szDefault;
      hb_fsFNameMerge(pszPath, pFilepath);
      bIsFile = hb_fsFile(pszPath);
    }

    if (!bIsFile && hb_setGetPath()) {
      HB_PATHNAMES *pNextPath = hb_setGetFirstSetPath();

      while (bIsFile == false && pNextPath) {
        pFilepath->szPath = pNextPath->szPath;
        hb_fsFNameMerge(pszPath, pFilepath);
        bIsFile = hb_fsFile(pszPath);
        pNextPath = pNextPath->pNext;
      }
    }

    // This code is intentional. To eliminate race condition,
    // in pending hb_spCreate()/hb_spOpen() call when we have to know
    // real path and file name we have to set its deterministic value
    // here. If it's not necessary the caller may drop this value.
    if (!bIsFile) {
      pFilepath->szPath = szDefault ? szDefault : ".";
      hb_fsFNameMerge(pszPath, pFilepath);
    }
  }

  hb_xfree(pFilepath);

  if (pszRetPath == nullptr) {
    hb_xfree(pszPath);
  }

  return bIsFile;
}

HB_BOOL hb_spFileExists(const char *pszFileName, char *pszRetPath)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_spFileExists(%s, %p)", pszFileName, static_cast<void*>(pszRetPath)));
#endif

  char *pszPath;

  if (pszRetPath) {
    pszPath = pszRetPath;
  } else {
    pszPath = static_cast<char *>(hb_xgrab(HB_PATH_MAX));
  }

  PHB_FNAME pFilepath = hb_fsFNameSplit(pszFileName);

  auto bIsFile = false;

  if (pFilepath->szPath) {
    hb_fsFNameMerge(pszPath, pFilepath);
    bIsFile = hb_fsFileExists(pszPath);
  } else {
    const char *szDefault = hb_setGetDefault();
    if (szDefault != nullptr) {
      pFilepath->szPath = szDefault;
      hb_fsFNameMerge(pszPath, pFilepath);
      bIsFile = hb_fsFileExists(pszPath);
    }

    if (!bIsFile && hb_setGetPath()) {
      HB_PATHNAMES *pNextPath = hb_setGetFirstSetPath();

      while (bIsFile == false && pNextPath) {
        pFilepath->szPath = pNextPath->szPath;
        hb_fsFNameMerge(pszPath, pFilepath);
        bIsFile = hb_fsFileExists(pszPath);
        pNextPath = pNextPath->pNext;
      }
    }

    // This code is intentional. To eliminate race condition,
    // in pending hb_spCreate()/hb_spOpen() call when we have to know
    // real path and file name we have to set its deterministic value
    // here. If it's not necessary the caller may drop this value.
    if (!bIsFile) {
      pFilepath->szPath = szDefault ? szDefault : ".";
      hb_fsFNameMerge(pszPath, pFilepath);
    }
  }

  hb_xfree(pFilepath);

  if (pszRetPath == nullptr) {
    hb_xfree(pszPath);
  }

  return bIsFile;
}

HB_FHANDLE hb_spOpen(const char *pszFileName, HB_USHORT uiFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_spOpen(%p, %hu)", static_cast<const void*>(pszFileName), uiFlags));
#endif

  char szPath[HB_PATH_MAX];

  if (hb_spFile(pszFileName, szPath)) {
    return hb_fsOpen(szPath, uiFlags);
  } else {
    return hb_fsOpen(pszFileName, uiFlags);
  }
}

HB_FHANDLE hb_spCreate(const char *pszFileName, HB_FATTR ulAttr)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_spCreate(%p, %u)", static_cast<const void*>(pszFileName), ulAttr));
#endif

  char szPath[HB_PATH_MAX];
  PHB_FNAME pFilepath = hb_fsFNameSplit(pszFileName);
  if (!pFilepath->szPath) {
    pFilepath->szPath = hb_setGetDefault();
  }
  hb_fsFNameMerge(szPath, pFilepath);
  hb_xfree(pFilepath);
  return hb_fsCreate(szPath, ulAttr);
}

HB_FHANDLE hb_spCreateEx(const char *pszFileName, HB_FATTR ulAttr, HB_USHORT uiFlags)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_spCreateEx(%p, %u, %hu)", static_cast<const void*>(pszFileName), ulAttr, uiFlags));
#endif

  char szPath[HB_PATH_MAX];
  PHB_FNAME pFilepath = hb_fsFNameSplit(pszFileName);
  if (!pFilepath->szPath) {
    pFilepath->szPath = hb_setGetDefault();
  }
  hb_fsFNameMerge(szPath, pFilepath);
  hb_xfree(pFilepath);
  return hb_fsCreateEx(szPath, ulAttr, uiFlags);
}
