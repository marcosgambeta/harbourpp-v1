//
// IOUSRD - library to create new FILE IO redirectors at .prg level
//
// Copyright 2014 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/* this has to be declared before hbapifs.h is included */
#define _HB_FILE_IMPLEMENTATION_

#include "hbapi.hpp"
#include "hbapifs.hpp"
#include "hbapierr.hpp"
#include "hbapiitm.hpp"
#include "hbxvm.hpp"
#include "hbstack.hpp"
#include "hbthread.hpp"

#include "hbiousr.ch"

#define HB_FILE_ERR_UNSUPPORTED  ((HB_ERRCODE)FS_ERROR)

struct _HB_IOUSR
{
  HB_FILE_FUNCS funcs;
  char *prefix;
  int prefix_len;
  PHB_SYMB prg_funcs[IOUSR_METHODCOUNT];
};

using HB_IOUSR = _HB_IOUSR;
using PHB_IOUSR = _HB_IOUSR *;

struct _HB_FILE
{
  const HB_FILE_FUNCS *pFuncs;
  PHB_ITEM pFileItm;
};

using HB_FILE = _HB_FILE;

static HB_CRITICAL_NEW(s_iousrMtx);
#define HB_IOUSR_LOCK()                                                                                                \
  do                                                                                                                   \
  {                                                                                                                    \
  hb_threadEnterCriticalSection(&s_iousrMtx)
#define HB_IOUSR_UNLOCK()                                                                                              \
  hb_threadLeaveCriticalSection(&s_iousrMtx);                                                                          \
  }                                                                                                                    \
  while (false)

static int s_iCount = 0;
static PHB_IOUSR s_ioUsrs[HB_FILE_TYPE_MAX];

static void s_errRT_IOUSR(HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char *szDescription)
{
  auto pError = hb_errRT_New(ES_ERROR, "IOUSR", errGenCode, errSubCode, szDescription, HB_ERR_FUNCNAME, 0, EF_NONE);
  PHB_ITEM pArray = hb_arrayBaseParams();
  if (pArray)
  {
    hb_errPutArgsArray(pError, pArray);
    hb_itemRelease(pArray);
  }
  hb_errLaunch(pError);
  hb_itemRelease(pError);
}

static void s_iousrFreeAll(void *cargo)
{
  HB_SYMBOL_UNUSED(cargo);

  while (s_iCount > 0)
  {
    PHB_IOUSR pIO = s_ioUsrs[--s_iCount];
    hb_xfree(pIO->prefix);
    hb_xfree(pIO);
  }
}

static PHB_FILE s_fileNew(PHB_IOUSR pIO, PHB_ITEM pFileItm)
{
  auto pFile = static_cast<PHB_FILE>(hb_xgrab(sizeof(HB_FILE)));
  pFile->pFuncs = &pIO->funcs;
  pFile->pFileItm = pFileItm;
  return pFile;
}

static PHB_IOUSR s_iousrAddNew(const char *pszPrefix)
{
  PHB_IOUSR pIO = nullptr;

  HB_IOUSR_LOCK();

  int iCount = s_iCount;
  while (--iCount >= 0)
  {
    if (hb_stricmp(pszPrefix, s_ioUsrs[iCount]->prefix) == 0)
    {
      break;
    }
  }
  if (iCount < 0)
  {
    if (s_iCount == 0)
    {
      hb_vmAtQuit(s_iousrFreeAll, nullptr);
    }
    pIO = static_cast<PHB_IOUSR>(hb_xgrabz(sizeof(HB_IOUSR)));
    pIO->prefix = hb_strdup(pszPrefix);
    pIO->prefix_len = static_cast<int>(strlen(pszPrefix));
    s_ioUsrs[s_iCount++] = pIO;
  }

  HB_IOUSR_UNLOCK();

  return pIO;
}

#define s_hasMethod(pIO, iMethod) ((pIO)->prg_funcs[(iMethod)-1] != nullptr)

#define s_getUsrIO(p) (static_cast<PHB_IOUSR>(HB_UNCONST(p)))

static void s_pushMethod(PHB_IOUSR pIO, int iMethod)
{
  hb_vmPushSymbol(pIO->prg_funcs[iMethod - 1]);
  hb_vmPushNil();
}

static HB_BOOL s_fileAccept(PHB_FILE_FUNCS pFuncs, const char *pszFileName)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  auto fResult = false;

  if (hb_strnicmp(pszFileName, pIO->prefix, pIO->prefix_len) == 0)
  {
    if (s_hasMethod(pIO, IOUSR_ACCEPT))
    {
      s_pushMethod(pIO, IOUSR_ACCEPT);
      hb_vmPushString(pszFileName, strlen(pszFileName));
      hb_vmDo(1);
      fResult = hb_parl(-1);
    }
    else if (pIO->prefix_len > 0)
    {
      fResult = true;
    }
  }

  return fResult;
}

static HB_BOOL s_fileExists(PHB_FILE_FUNCS pFuncs, const char *pszFileName, char *pRetPath)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  HB_SYMBOL_UNUSED(pRetPath);
  s_pushMethod(pIO, IOUSR_EXISTS);
  hb_vmPushString(pszFileName, strlen(pszFileName));
  hb_vmDo(1);
  return hb_parl(-1);
}

static HB_BOOL s_fileDelete(PHB_FILE_FUNCS pFuncs, const char *pszFileName)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  s_pushMethod(pIO, IOUSR_DELETE);
  hb_vmPushString(pszFileName, strlen(pszFileName));
  hb_vmDo(1);
  return hb_parl(-1);
}

static HB_BOOL s_fileRename(PHB_FILE_FUNCS pFuncs, const char *pszName, const char *pszNewName)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  s_pushMethod(pIO, IOUSR_RENAME);
  hb_vmPushString(pszName, strlen(pszName));
  hb_vmPushString(pszNewName, strlen(pszNewName));
  hb_vmDo(2);
  return hb_parl(-1);
}

static HB_BOOL s_fileCopy(PHB_FILE_FUNCS pFuncs, const char *pszSrcFile, const char *pszDstFile)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  s_pushMethod(pIO, IOUSR_COPY);
  hb_vmPushString(pszSrcFile, strlen(pszSrcFile));
  hb_vmPushString(pszDstFile, strlen(pszDstFile));
  hb_vmDo(2);
  return hb_parl(-1);
}

static HB_BOOL s_fileDirExists(PHB_FILE_FUNCS pFuncs, const char *pszDirName)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  s_pushMethod(pIO, IOUSR_DIREXISTS);
  hb_vmPushString(pszDirName, strlen(pszDirName));
  hb_vmDo(1);
  return hb_parl(-1);
}

static HB_BOOL s_fileDirMake(PHB_FILE_FUNCS pFuncs, const char *pszDirName)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  s_pushMethod(pIO, IOUSR_DIRMAKE);
  hb_vmPushString(pszDirName, strlen(pszDirName));
  hb_vmDo(1);
  return hb_parl(-1);
}

static HB_BOOL s_fileDirRemove(PHB_FILE_FUNCS pFuncs, const char *pszDirName)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  s_pushMethod(pIO, IOUSR_DIRREMOVE);
  hb_vmPushString(pszDirName, strlen(pszDirName));
  hb_vmDo(1);
  return hb_parl(-1);
}

static double s_fileDirSpace(PHB_FILE_FUNCS pFuncs, const char *pszDirName, HB_USHORT uiType)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  s_pushMethod(pIO, IOUSR_DIRSPACE);
  hb_vmPushString(pszDirName, strlen(pszDirName));
  hb_vmPushInteger(uiType);
  hb_vmDo(2);
  return hb_parnd(-1);
}

static PHB_ITEM s_fileDirectory(PHB_FILE_FUNCS pFuncs, const char *pszDirSpec, const char *pszAttr)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  s_pushMethod(pIO, IOUSR_DIRECTORY);
  hb_vmPushString(pszDirSpec, strlen(pszDirSpec));
  hb_vmPushString(pszAttr, strlen(pszAttr));
  hb_vmDo(2);
  return hb_itemNew(hb_stackReturnItem());
}

static HB_BOOL s_fileTimeGet(PHB_FILE_FUNCS pFuncs, const char *pszFileName, long *plJulian, long *plMillisec)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);

  auto iOffset = static_cast<int>(hb_stackTopOffset() - hb_stackBaseOffset());
  hb_vmPushNil();
  hb_vmPushNil();

  s_pushMethod(pIO, IOUSR_TIMEGET);
  hb_vmPushString(pszFileName, strlen(pszFileName));
  hb_xvmPushLocalByRef(static_cast<HB_SHORT>(iOffset));
  hb_xvmPushLocalByRef(static_cast<HB_SHORT>(iOffset + 1));
  hb_vmDo(3);

  bool fResult = hb_parl(-1);
  if (fResult)
  {
    *plJulian = hb_itemGetNL(hb_stackItemFromBase(iOffset));
    *plMillisec = hb_itemGetNL(hb_stackItemFromBase(iOffset + 1));
  }
  hb_stackPop();
  hb_stackPop();

  return fResult;
}

static HB_BOOL s_fileTimeSet(PHB_FILE_FUNCS pFuncs, const char *pszFileName, long lJulian, long lMillisec)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  s_pushMethod(pIO, IOUSR_TIMESET);
  hb_vmPushString(pszFileName, strlen(pszFileName));
  hb_vmPushLong(lJulian);
  hb_vmPushLong(lMillisec);
  hb_vmDo(3);
  return hb_parl(-1);
}

static HB_BOOL s_fileAttrGet(PHB_FILE_FUNCS pFuncs, const char *pszFileName, HB_FATTR *pnAttr)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);

  auto iOffset = static_cast<int>(hb_stackTopOffset() - hb_stackBaseOffset());
  hb_vmPushNil();

  s_pushMethod(pIO, IOUSR_ATTRGET);
  hb_vmPushString(pszFileName, strlen(pszFileName));
  hb_xvmPushLocalByRef(static_cast<HB_SHORT>(iOffset));
  hb_vmDo(2);

  bool fResult = hb_parl(-1);
  if (fResult)
  {
    *pnAttr = static_cast<HB_FATTR>(hb_itemGetNL(hb_stackItemFromBase(iOffset)));
  }
  hb_stackPop();

  return fResult;
}

static HB_BOOL s_fileAttrSet(PHB_FILE_FUNCS pFuncs, const char *pszFileName, HB_FATTR nAttr)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  s_pushMethod(pIO, IOUSR_ATTRSET);
  hb_vmPushString(pszFileName, strlen(pszFileName));
  hb_vmPushLong(nAttr);
  hb_vmDo(2);
  return hb_parl(-1);
}

static HB_BOOL s_fileLink(PHB_FILE_FUNCS pFuncs, const char *pszExisting, const char *pszNewName)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  s_pushMethod(pIO, IOUSR_LINK);
  hb_vmPushString(pszExisting, strlen(pszExisting));
  hb_vmPushString(pszNewName, strlen(pszNewName));
  hb_vmDo(2);
  return hb_parl(-1);
}

static HB_BOOL s_fileLinkSym(PHB_FILE_FUNCS pFuncs, const char *pszTarget, const char *pszNewName)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  s_pushMethod(pIO, IOUSR_LINKSYM);
  hb_vmPushString(pszTarget, strlen(pszTarget));
  hb_vmPushString(pszNewName, strlen(pszNewName));
  hb_vmDo(2);
  return hb_parl(-1);
}

static char *s_fileLinkRead(PHB_FILE_FUNCS pFuncs, const char *pszFileName)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  s_pushMethod(pIO, IOUSR_LINKREAD);
  hb_vmPushString(pszFileName, strlen(pszFileName));
  hb_vmDo(1);
  auto pszLink = hb_parc(-1);
  return pszLink != nullptr ? hb_strdup(pszLink) : nullptr;
}

static PHB_FILE s_fileOpen(PHB_FILE_FUNCS pFuncs, const char *pszName, const char *pszDefExt, HB_FATTR nExFlags,
                           const char *pPaths, PHB_ITEM pError)
{
  PHB_IOUSR pIO = s_getUsrIO(pFuncs);
  PHB_FILE pFile = nullptr;

  s_pushMethod(pIO, IOUSR_OPEN);
  hb_vmPushString(pszName, strlen(pszName));
  if (pszDefExt)
  {
    hb_vmPushString(pszDefExt, strlen(pszDefExt));
  }
  else
  {
    hb_vmPushNil();
  }
  hb_vmPushInteger(nExFlags);
  if (pPaths)
  {
    hb_vmPushString(pPaths, strlen(pPaths));
  }
  else
  {
    hb_vmPushNil();
  }
  if (pError)
  {
    hb_vmPush(pError);
  }
  else
  {
    hb_vmPushNil();
  }

  hb_vmDo(5);

  auto pFileItm = hb_stackReturnItem();
  if (!HB_IS_NIL(pFileItm))
  {
    pFile = s_fileNew(pIO, hb_itemNew(pFileItm));
  }

  return pFile;
}

static void s_fileClose(PHB_FILE pFile)
{
  PHB_IOUSR pIO = s_getUsrIO(pFile->pFuncs);
  s_pushMethod(pIO, IOUSR_CLOSE);
  hb_vmPush(pFile->pFileItm);
  hb_vmDo(1);
}

static HB_BOOL s_fileLock(PHB_FILE pFile, HB_FOFFSET nStart, HB_FOFFSET nLen, int iType)
{
  PHB_IOUSR pIO = s_getUsrIO(pFile->pFuncs);
  s_pushMethod(pIO, IOUSR_LOCK);
  hb_vmPush(pFile->pFileItm);
  hb_vmPushNumInt(static_cast<HB_MAXINT>(nStart));
  hb_vmPushNumInt(static_cast<HB_MAXINT>(nLen));
  hb_vmPushInteger(iType);
  hb_vmDo(4);
  return hb_parl(-1);
}

static int s_fileLockTest(PHB_FILE pFile, HB_FOFFSET nStart, HB_FOFFSET nLen, int iType)
{
  PHB_IOUSR pIO = s_getUsrIO(pFile->pFuncs);
  s_pushMethod(pIO, IOUSR_LOCKTEST);
  hb_vmPush(pFile->pFileItm);
  hb_vmPushNumInt(static_cast<HB_MAXINT>(nStart));
  hb_vmPushNumInt(static_cast<HB_MAXINT>(nLen));
  hb_vmPushInteger(iType);
  hb_vmDo(4);
  return hb_parni(-1);
}

static HB_SIZE s_fileRead(PHB_FILE pFile, void *data, HB_SIZE nSize, HB_MAXINT timeout)
{
  PHB_IOUSR pIO = s_getUsrIO(pFile->pFuncs);

  auto iOffset = static_cast<int>(hb_stackTopOffset() - hb_stackBaseOffset());
  memset(data, 0, nSize);
  hb_vmPushString(static_cast<const char *>(data), nSize);

  s_pushMethod(pIO, IOUSR_READ);
  hb_vmPush(pFile->pFileItm);
  hb_xvmPushLocalByRef(static_cast<HB_SHORT>(iOffset));
  hb_vmPushSize(nSize);
  hb_vmPushNumInt(timeout);
  hb_vmDo(4);

  HB_SIZE nResult = hb_parns(-1);
  if (nResult > 0)
  {
    nSize = hb_itemGetCLen(hb_stackItemFromBase(iOffset));
    if (nResult > nSize)
    {
      nResult = nSize;
    }
    memcpy(data, hb_itemGetCPtr(hb_stackItemFromBase(iOffset)), nSize);
  }
  hb_stackPop();

  return nResult;
}

static HB_SIZE s_fileWrite(PHB_FILE pFile, const void *data, HB_SIZE nSize, HB_MAXINT timeout)
{
  PHB_IOUSR pIO = s_getUsrIO(pFile->pFuncs);
  s_pushMethod(pIO, IOUSR_WRITE);
  hb_vmPush(pFile->pFileItm);
  hb_vmPushString(static_cast<const char *>(data), nSize);
  hb_vmPushSize(nSize);
  hb_vmPushNumInt(timeout);
  hb_vmDo(4);
  return hb_parns(-1);
}

static HB_SIZE s_fileReadAt(PHB_FILE pFile, void *buffer, HB_SIZE nSize, HB_FOFFSET nOffset)
{
  PHB_IOUSR pIO = s_getUsrIO(pFile->pFuncs);

  auto iOffset = static_cast<int>(hb_stackTopOffset() - hb_stackBaseOffset());
  memset(buffer, 0, nSize);
  hb_vmPushString(static_cast<const char *>(buffer), nSize);

  s_pushMethod(pIO, IOUSR_READAT);
  hb_vmPush(pFile->pFileItm);
  hb_xvmPushLocalByRef(static_cast<HB_SHORT>(iOffset));
  hb_vmPushSize(nSize);
  hb_vmPushNumInt(static_cast<HB_MAXINT>(nOffset));
  hb_vmDo(4);

  HB_SIZE nResult = hb_parns(-1);
  if (nResult > 0)
  {
    nSize = hb_itemGetCLen(hb_stackItemFromBase(iOffset));
    if (nResult > nSize)
    {
      nResult = nSize;
    }
    memcpy(buffer, hb_itemGetCPtr(hb_stackItemFromBase(iOffset)), nSize);
  }
  hb_stackPop();

  return nResult;
}

static HB_SIZE s_fileWriteAt(PHB_FILE pFile, const void *buffer, HB_SIZE nSize, HB_FOFFSET nOffset)
{
  PHB_IOUSR pIO = s_getUsrIO(pFile->pFuncs);
  s_pushMethod(pIO, IOUSR_WRITEAT);
  hb_vmPush(pFile->pFileItm);
  hb_vmPushString(static_cast<const char *>(buffer), nSize);
  hb_vmPushSize(nSize);
  hb_vmPushNumInt(static_cast<HB_MAXINT>(nOffset));
  hb_vmDo(4);
  return hb_parns(-1);
}

static HB_BOOL s_fileTruncAt(PHB_FILE pFile, HB_FOFFSET nOffset)
{
  PHB_IOUSR pIO = s_getUsrIO(pFile->pFuncs);
  s_pushMethod(pIO, IOUSR_TRUNCAT);
  hb_vmPush(pFile->pFileItm);
  hb_vmPushNumInt(static_cast<HB_MAXINT>(nOffset));
  hb_vmDo(2);
  return hb_parl(-1);
}

static HB_FOFFSET s_fileSeek(PHB_FILE pFile, HB_FOFFSET nOffset, HB_USHORT uiFlags)
{
  PHB_IOUSR pIO = s_getUsrIO(pFile->pFuncs);
  s_pushMethod(pIO, IOUSR_SEEK);
  hb_vmPush(pFile->pFileItm);
  hb_vmPushNumInt(static_cast<HB_MAXINT>(nOffset));
  hb_vmPushInteger(uiFlags);
  hb_vmDo(3);
  return static_cast<HB_FOFFSET>(hb_parnint(-1));
}

static HB_FOFFSET s_fileSize(PHB_FILE pFile)
{
  PHB_IOUSR pIO = s_getUsrIO(pFile->pFuncs);
  s_pushMethod(pIO, IOUSR_SIZE);
  hb_vmPush(pFile->pFileItm);
  hb_vmDo(1);
  return static_cast<HB_FOFFSET>(hb_parnint(-1));
}

static HB_BOOL s_fileEof(PHB_FILE pFile)
{
  PHB_IOUSR pIO = s_getUsrIO(pFile->pFuncs);
  s_pushMethod(pIO, IOUSR_EOF);
  hb_vmPush(pFile->pFileItm);
  hb_vmDo(1);
  return hb_parl(-1);
}

static void s_fileFlush(PHB_FILE pFile, HB_BOOL fDirty)
{
  PHB_IOUSR pIO = s_getUsrIO(pFile->pFuncs);
  s_pushMethod(pIO, IOUSR_FLUSH);
  hb_vmPush(pFile->pFileItm);
  hb_vmPushLogical(fDirty);
  hb_vmDo(2);
}

static void s_fileCommit(PHB_FILE pFile)
{
  PHB_IOUSR pIO = s_getUsrIO(pFile->pFuncs);
  s_pushMethod(pIO, IOUSR_COMMIT);
  hb_vmPush(pFile->pFileItm);
  hb_vmDo(1);
}

static HB_BOOL s_fileConfigure(PHB_FILE pFile, int iIndex, PHB_ITEM pValue)
{
  PHB_IOUSR pIO = s_getUsrIO(pFile->pFuncs);
  s_pushMethod(pIO, IOUSR_CONFIGURE);
  hb_vmPush(pFile->pFileItm);
  hb_vmPushInteger(iIndex);
  if (pValue != nullptr)
  {
    hb_vmPush(pValue);
  }
  hb_vmDo(pValue != nullptr ? 3 : 2);
  return hb_parl(-1);
}

static HB_FHANDLE s_fileHandle(PHB_FILE pFile)
{
  PHB_IOUSR pIO = s_getUsrIO(pFile->pFuncs);
  s_pushMethod(pIO, IOUSR_HANDLE);
  hb_vmPush(pFile->pFileItm);
  hb_vmDo(1);
  return static_cast<HB_FHANDLE>(hb_parns(-1));
}

static const HB_FILE_FUNCS s_fileFuncs = {
    s_fileAccept,

    s_fileExists,    s_fileDelete,  s_fileRename,    s_fileCopy,

    s_fileDirExists, s_fileDirMake, s_fileDirRemove, s_fileDirSpace, s_fileDirectory,

    s_fileTimeGet,   s_fileTimeSet, s_fileAttrGet,   s_fileAttrSet,

    s_fileLink,      s_fileLinkSym, s_fileLinkRead,

    s_fileOpen,      s_fileClose,   s_fileLock,      s_fileLockTest, s_fileRead,      s_fileWrite,
    s_fileReadAt,    s_fileWriteAt, s_fileTruncAt,   s_fileSeek,     s_fileSize,      s_fileEof,
    s_fileFlush,     s_fileCommit,  s_fileConfigure, s_fileHandle};

using HB_FILE_FUNC = HB_BOOL (*)(PHB_FILE_FUNCS pFuncs, const char *);
#define HB_FILE_FUNC_COUNT (sizeof(HB_FILE_FUNCS) / sizeof(HB_FILE_FUNC))

/* IOUSR_Register(<aMethods>, <cPrefix>) */
HB_FUNC(IOUSR_REGISTER)
{
  auto pMthItm = hb_param(1, Harbour::Item::ARRAY);
  auto pszPrefix = hb_parc(2);

  if (pMthItm && pszPrefix && *pszPrefix)
  {
    HB_SIZE nMethods = hb_arrayLen(pMthItm), nAt;

    if (nMethods > HB_MIN(IOUSR_METHODCOUNT, HB_FILE_FUNC_COUNT))
    {
      nMethods = HB_MIN(IOUSR_METHODCOUNT, HB_FILE_FUNC_COUNT);
    }

    for (nAt = 1; nAt <= nMethods; ++nAt)
    {
      auto pSymItm = hb_arrayGetItemPtr(pMthItm, nAt);

      if (!HB_IS_NIL(pSymItm) && !HB_IS_SYMBOL(pSymItm))
      {
        break;
      }
    }

    if (nAt > nMethods)
    {
      PHB_IOUSR pIO = s_iousrAddNew(pszPrefix);

      if (pIO != nullptr)
      {
        const HB_FILE_FUNC *pDummyFunc;
        HB_FILE_FUNC *pFunction;

        pDummyFunc = &s_fileFuncs.Accept;
        pFunction = &pIO->funcs.Accept;
        for (nAt = 1; nAt <= nMethods; ++nAt, pDummyFunc++, pFunction++)
        {
          pIO->prg_funcs[nAt - 1] = hb_arrayGetSymbol(pMthItm, nAt);
          if (nAt == 1 || pIO->prg_funcs[nAt - 1] != nullptr)
          {
            *pFunction = *pDummyFunc;
          }
        }
        if (!hb_fileRegisterPart(&pIO->funcs))
        {
          pIO = nullptr;
        }
      }
      if (pIO == nullptr)
      {
        s_errRT_IOUSR(EG_ARG, 1003, pszPrefix);
      }
    }
    else
    {
      s_errRT_IOUSR(EG_ARG, 1002, pszPrefix);
    }
  }
  else
  {
    s_errRT_IOUSR(EG_ARG, 1001, "Argument error");
  }
}

/* IOUSR_SetError([<nError> [, <nBase> ]]) --> <nPrevError> */
HB_FUNC(IOUSR_SETERROR)
{
  HB_ERRCODE errCodePrev = hb_fsError();

  if (HB_ISNUM(1))
  {
    auto errCodeNew = static_cast<HB_ERRCODE>(hb_parni(1));
    if (errCodeNew != 0)
    {
      errCodeNew += static_cast<HB_ERRCODE>(hb_parni(2));
    }
    hb_fsSetError(errCodeNew);
  }

  hb_retni(errCodePrev);
}
