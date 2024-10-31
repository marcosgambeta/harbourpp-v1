/*
 * I/O driver for BZIP2 compressed streams
 *
 * Copyright 2016 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#if !defined(_HB_API_INTERNAL_)
#define _HB_API_INTERNAL_
#endif

/* this has to be declared before hbapifs.h is included */
#define _HB_FILE_IMPLEMENTATION_

#include <hbapi.hpp>
#include <hbapifs.hpp>
#include <hbapierr.hpp>
#include <hbapiitm.hpp>
#include "hbinit.hpp"

#include <bzlib.h>

#define HB_BZ2_ERROR_BASE     100
#define HB_BZ2_BUFSIZE        8192
#define HB_BZ2_BLOCKSIZE      9

struct _HB_FILE
{
   const HB_FILE_FUNCS * pFuncs;
   PHB_FILE       pFile;

   HB_FOFFSET     seek_pos;
   HB_MAXINT      nTimeout;
   HB_BOOL        fEof;    // TODO: HB_BOOL -> bool
   HB_BOOL        fInited; // TODO: HB_BOOL -> bool
   int            iMode;
   int            iBlockSize;

   bz_stream      bz2;
   HB_BYTE        buffer[HB_BZ2_BUFSIZE];
};

using HB_FILE = _HB_FILE;

#define _PHB_FILE     pFile->pFile

static PHB_FILE s_filebz2New(PHB_FILE pFile, int iMode, int iBlockSize);

static void * s_filebz2Alloc(void * cargo, int nmemb, int size)
{
   HB_SYMBOL_UNUSED(cargo);

   return (nmemb > 0 && size > 0) ? hb_xalloc(static_cast<HB_SIZE>(nmemb) * size) : nullptr;
}

static void s_filebz2Free(void * cargo, void * ptr)
{
   HB_SYMBOL_UNUSED(cargo);

   if( ptr ) {
      hb_xfree(ptr);
   }   
}

static HB_SIZE s_bz2_write(PHB_FILE pFile, HB_MAXINT nTimeout)
{
   HB_SIZE nWritten = 0, nSize = HB_BZ2_BUFSIZE - pFile->bz2.avail_out;

   while( nWritten < nSize ) {
      HB_SIZE nWr = _PHB_FILE->pFuncs->Write(_PHB_FILE, pFile->buffer + nWritten, nSize - nWritten, nTimeout);
      if( nWr == static_cast<HB_SIZE>(-1) ) {
         return nWr;
      } else if( nWr == 0 ) {
         break;
      }

      nWritten += nWr;
      if( nTimeout > 0 ) {
         nTimeout = 0;
      }
   }

   if( nWritten > 0 ) {
      if( nWritten < nSize ) {
         memmove(pFile->buffer, pFile->buffer + nWritten, nSize - nWritten);
      }
      pFile->bz2.avail_out += static_cast<unsigned int>(nWritten);
      pFile->bz2.next_out -= nWritten;
   }

   return nWritten;
}

static void s_bz2_flush(PHB_FILE pFile, bool fClose)
{
   int err;

   if( pFile->bz2.avail_out > 0 ) {
      err = BZ2_bzCompress(&pFile->bz2, fClose ? BZ_FINISH : BZ_FLUSH);
   } else if( pFile->bz2.avail_out < HB_BZ2_BUFSIZE ) {
      err = BZ_FLUSH_OK;
   } else {
      err = BZ_STREAM_END;
   }

   while( pFile->bz2.avail_out < HB_BZ2_BUFSIZE ) {
      HB_SIZE nWr = s_bz2_write(pFile, pFile->nTimeout);
      if( nWr == 0 || nWr == static_cast<HB_SIZE>(-1) ) {
         return;
      }
      if( err == BZ_FLUSH_OK || err == BZ_FINISH_OK ) {
         err = BZ2_bzCompress(&pFile->bz2, fClose ? BZ_FINISH : BZ_FLUSH);
      }
   }

   if( err == BZ_STREAM_END || err == BZ_RUN_OK ) {
      hb_fsSetError(0);
   } else {
      hb_fsSetError(HB_BZ2_ERROR_BASE - err);
   }
}

static const char * s_bz2io_name(const char * pszFileName, int * piBlockSize)
{
   if( HB_TOUPPER(pszFileName[0]) == 'B' && HB_TOUPPER(pszFileName[1]) == 'Z' ) {
      if( pszFileName[2] == ':' ) {
         pszFileName += 3;
      } else if( pszFileName[2] >= '1' && pszFileName[2] <= '9'  && pszFileName[3] == ':' ) {
         if( piBlockSize ) {
            *piBlockSize = pszFileName[2] - '0';
         }
         pszFileName += 4;
      }
   }
   return pszFileName;
}

/*
 * file methods
 */

static HB_BOOL s_fileAccept(PHB_FILE_FUNCS pFuncs, const char * pszFileName) // FileFunc
{
   HB_SYMBOL_UNUSED(pFuncs);

   return s_bz2io_name(pszFileName, nullptr) != pszFileName;
}

static HB_BOOL s_fileExists(PHB_FILE_FUNCS pFuncs, const char * pszFileName, char * pRetPath) // FileFunc
{
   const char * pszName;
   bool fResult;

   HB_SYMBOL_UNUSED(pFuncs);

   pszName = s_bz2io_name(pszFileName, nullptr);
   if( pRetPath ) {
      auto pszNameBuf = static_cast<char*>(hb_xgrab(HB_PATH_MAX));
      auto iPref = static_cast<int>(pszName - pszFileName);

      fResult = hb_fileExists(pszName, pszNameBuf);
      if( pRetPath != pszFileName ) {
         memcpy(pRetPath, pszFileName, iPref);
      }
      hb_strncpy(pRetPath + iPref, pszNameBuf, HB_PATH_MAX - 1 - iPref);
      hb_xfree(pszNameBuf);
   } else {
      fResult = hb_fileExists(pszName, nullptr);
   }

   return fResult;
}

static HB_BOOL s_fileDelete(PHB_FILE_FUNCS pFuncs, const char * pszFileName) // FileFunc
{
   HB_SYMBOL_UNUSED(pFuncs);

   return hb_fileDelete(s_bz2io_name(pszFileName, nullptr));
}

static HB_BOOL s_fileRename(PHB_FILE_FUNCS pFuncs, const char * pszName, const char * pszNewName) // FileFunc
{
   HB_SYMBOL_UNUSED(pFuncs);

   return hb_fileRename(s_bz2io_name(pszName, nullptr), s_bz2io_name(pszNewName, nullptr));
}

static HB_BOOL s_fileCopy(PHB_FILE_FUNCS pFuncs, const char * pszSrcFile, const char * pszDstFile) // FileFunc
{
   int iSrcBlkSize = HB_BZ2_BLOCKSIZE,
       iDstBlkSize = HB_BZ2_BLOCKSIZE;
   const char * pszSrc = s_bz2io_name(pszSrcFile, &iSrcBlkSize ), * pszDst = s_bz2io_name(pszDstFile, &iDstBlkSize);

   HB_SYMBOL_UNUSED(pFuncs);

   if( pszDst != pszDstFile && iSrcBlkSize == iDstBlkSize ) {
      return hb_fsCopy(pszSrc, pszDst);
   } else {
      return hb_fsCopy(pszSrcFile, pszDstFile);
   }
}

static HB_BOOL s_fileDirExists(PHB_FILE_FUNCS pFuncs, const char * pszDirName) // FileFunc
{
   HB_SYMBOL_UNUSED(pFuncs);

   return hb_fileDirExists(s_bz2io_name(pszDirName, nullptr));
}

static HB_BOOL s_fileDirMake(PHB_FILE_FUNCS pFuncs, const char * pszDirName) // FileFunc
{
   HB_SYMBOL_UNUSED(pFuncs);

   return hb_fileDirMake(s_bz2io_name(pszDirName, nullptr));
}

static HB_BOOL s_fileDirRemove(PHB_FILE_FUNCS pFuncs, const char * pszDirName) // FileFunc
{
   HB_SYMBOL_UNUSED(pFuncs);

   return hb_fileDirRemove(s_bz2io_name(pszDirName, nullptr));
}

static double s_fileDirSpace(PHB_FILE_FUNCS pFuncs, const char * pszDirName, HB_USHORT uiType) // FileFunc
{
   HB_SYMBOL_UNUSED(pFuncs);

   return hb_fileDirSpace(s_bz2io_name(pszDirName, nullptr), uiType);
}

static PHB_ITEM s_fileDirectory(PHB_FILE_FUNCS pFuncs, const char * pszDirSpec, const char * pszAttr) // FileFunc
{
   HB_SYMBOL_UNUSED(pFuncs);

   return hb_fileDirectory(s_bz2io_name(pszDirSpec, nullptr), pszAttr);
}

static HB_BOOL s_fileTimeGet(PHB_FILE_FUNCS pFuncs, const char * pszFileName, long * plJulian, long * plMillisec) // FileFunc
{
   HB_SYMBOL_UNUSED(pFuncs);

   return hb_fileTimeGet(s_bz2io_name(pszFileName, nullptr), plJulian, plMillisec);
}

static HB_BOOL s_fileTimeSet(PHB_FILE_FUNCS pFuncs, const char * pszFileName, long lJulian, long lMillisec) // FileFunc
{
   HB_SYMBOL_UNUSED(pFuncs);

   return hb_fileTimeSet(s_bz2io_name(pszFileName, nullptr), lJulian, lMillisec);
}

static HB_BOOL s_fileAttrGet(PHB_FILE_FUNCS pFuncs, const char * pszFileName, HB_FATTR * pnAttr) // FileFunc
{
   HB_SYMBOL_UNUSED(pFuncs);

   return hb_fileAttrGet(s_bz2io_name(pszFileName, nullptr), pnAttr);
}

static HB_BOOL s_fileAttrSet(PHB_FILE_FUNCS pFuncs, const char * pszFileName, HB_FATTR nAttr) // FileFunc
{
   HB_SYMBOL_UNUSED(pFuncs);

   return hb_fileAttrSet(s_bz2io_name(pszFileName, nullptr), nAttr);
}

static HB_BOOL s_fileLink(PHB_FILE_FUNCS pFuncs, const char * pszExisting, const char * pszNewName) // FileFunc
{
   HB_SYMBOL_UNUSED(pFuncs);

   return hb_fileLink(s_bz2io_name(pszExisting, nullptr), s_bz2io_name(pszNewName, nullptr));
}

static HB_BOOL s_fileLinkSym(PHB_FILE_FUNCS pFuncs, const char * pszTarget, const char * pszNewName) // FileFunc
{
   HB_SYMBOL_UNUSED(pFuncs);

   return hb_fileLinkSym(s_bz2io_name(pszTarget, nullptr), s_bz2io_name(pszNewName, nullptr));
}

static char * s_fileLinkRead(PHB_FILE_FUNCS pFuncs, const char * pszFileName) // FileFunc
{
   HB_SYMBOL_UNUSED(pFuncs);

   return hb_fileLinkRead(s_bz2io_name(pszFileName, nullptr));
}

static PHB_FILE s_fileOpen(PHB_FILE_FUNCS pFuncs, const char * pszFileName, const char * pszDefExt, HB_FATTR nExFlags, const char * pPaths, PHB_ITEM pError) // FileFunc
{
   int iBlockSize = HB_BZ2_BLOCKSIZE;
   char * pszNameBuf = nullptr;
   const char * pszName = s_bz2io_name(pszFileName, &iBlockSize);
   auto iPref = static_cast<int>(pszName - pszFileName);
   PHB_FILE pFile;

   HB_SYMBOL_UNUSED(pFuncs);

   if( (nExFlags & FXO_COPYNAME) != 0 ) {
      pszName = pszNameBuf = hb_strncpy(static_cast<char*>(hb_xgrab(HB_PATH_MAX)), pszName, HB_PATH_MAX - 1);
   }

   pFile = hb_fileExtOpen(pszName, pszDefExt, nExFlags, pPaths, pError);

   if( (nExFlags & FXO_COPYNAME) != 0 ) {
      if( pFile ) {
         hb_strncpy(const_cast<char*>(pszFileName + iPref), pszNameBuf, HB_PATH_MAX - 1 - iPref);
      }
      hb_xfree(pszNameBuf);
   }
#if 0
   if( !pFile && pError ) {
      hb_errPutFileName(pError, pszFileName);
   }
#endif

   return s_filebz2New(pFile, nExFlags & (FO_READ | FO_WRITE | FO_READWRITE), iBlockSize);
}

static void s_fileClose(PHB_FILE pFile) // FileFunc
{
   if( pFile->iMode != FO_READ && pFile->fInited ) {
      s_bz2_flush(pFile, true);
   }

   _PHB_FILE->pFuncs->Close(_PHB_FILE);

   if( pFile->fInited ) {
      if( pFile->iMode == FO_READ ) {
         BZ2_bzDecompressEnd(&pFile->bz2);
      }
      else {
         BZ2_bzCompressEnd(&pFile->bz2);
      }
   }
   hb_xfree(pFile);
}

static HB_BOOL s_fileLock(PHB_FILE pFile, HB_FOFFSET nStart, HB_FOFFSET nLen, int iType) // FileFunc
{
   return _PHB_FILE->pFuncs->Lock(_PHB_FILE, nStart, nLen, iType);
}

static int s_fileLockTest(PHB_FILE pFile, HB_FOFFSET nStart, HB_FOFFSET nLen, int iType) // FileFunc
{
   return _PHB_FILE->pFuncs->LockTest(_PHB_FILE, nStart, nLen, iType);
}

static HB_SIZE s_fileRead(PHB_FILE pFile, void * buffer, HB_SIZE nSize, HB_MAXINT nTimeout) // FileFunc
{
   HB_SIZE nResult = 0;

   if( pFile->iMode != FO_WRITE ) {
      if( pFile->fEof ) {
         hb_fsSetError(0);
         return 0;
      } else if( !pFile->fInited ) {
         int err = BZ2_bzDecompressInit(&pFile->bz2, 0, 0);
         if( err != BZ_OK ) {
            hb_fsSetError(HB_BZ2_ERROR_BASE - err);
            return static_cast<HB_SIZE>(-1);
         }
         pFile->fInited = true;
         pFile->iMode = FO_READ;
      }

      hb_fsSetError(0);
      if( nTimeout == -1 ) {
         nTimeout = pFile->nTimeout;
      }

      pFile->bz2.next_out = static_cast<char*>(buffer);
      pFile->bz2.avail_out = static_cast<unsigned int>(nSize);
      pFile->bz2.total_out_hi32 = pFile->bz2.total_out_lo32 = 0;

      while( pFile->bz2.avail_out ) {
         int err = BZ2_bzDecompress(&pFile->bz2);

         if( err != BZ_OK ) {
            if( err == BZ_STREAM_END ) {
               BZ2_bzDecompressEnd(&pFile->bz2);
               pFile->fInited = false;
               pFile->fEof = true;
            } else {
               hb_fsSetError(HB_BZ2_ERROR_BASE - err);
               nResult = static_cast<HB_SIZE>(-1);
            }
            break;
         }
         if( pFile->bz2.avail_in == 0 ) {
            nResult = _PHB_FILE->pFuncs->Read(_PHB_FILE, pFile->buffer, HB_BZ2_BUFSIZE, pFile->bz2.avail_out ? nTimeout : 0);
            if( nResult == 0 || nResult == static_cast<HB_SIZE>(- 1) ) {
               break;
            }
            pFile->bz2.next_in = reinterpret_cast<char*>(pFile->buffer);
            pFile->bz2.avail_in = static_cast<unsigned int>(nResult);
         }
      }
      if( pFile->bz2.total_out_lo32 != 0 || pFile->bz2.total_out_hi32 != 0 ) {
#if HB_SIZE_MAX <= UINT_MAX
         nResult = static_cast<HB_SIZE>(pFile->bz2.total_out_lo32);
#else
         nResult = (static_cast<HB_SIZE>(pFile->bz2.total_out_hi32) << 32) | static_cast<HB_SIZE>(pFile->bz2.total_out_lo32);
#endif
      }
      pFile->seek_pos += hb_fileResult(nResult);
   } else {
      hb_fsSetError(HB_FILE_ERR_UNSUPPORTED);
   }

   return nResult;
}

static HB_SIZE s_fileWrite(PHB_FILE pFile, const void * buffer, HB_SIZE nSize, HB_MAXINT nTimeout) // FileFunc
{
   HB_SIZE nResult = 0;

   if( pFile->iMode != FO_READ ) {
      int err;

      if( !pFile->fInited ) {
         pFile->bz2.next_out  = reinterpret_cast<char*>(pFile->buffer);
         pFile->bz2.avail_out = HB_BZ2_BUFSIZE;
         err = BZ2_bzCompressInit(&pFile->bz2, pFile->iBlockSize, 0, 0);
         if( err != BZ_OK ) {
            hb_fsSetError(HB_BZ2_ERROR_BASE - err);
            return static_cast<HB_SIZE>(-1);
         }
         pFile->fInited = true;
         pFile->iMode = FO_WRITE;
      }

      hb_fsSetError(0);
      if( nTimeout == -1 ) {
         nTimeout = pFile->nTimeout;
      }

      pFile->bz2.next_in  = static_cast<char*>(const_cast<void*>(buffer));
      pFile->bz2.avail_in = static_cast<unsigned int>(nSize);

      while( pFile->bz2.avail_in ) {
         if( pFile->bz2.avail_out == 0 ) {
            nResult = s_bz2_write(pFile, nTimeout);
            if( nResult == 0 || nResult == static_cast<HB_SIZE>(- 1) ) {
               break;
            }
         }
         err = BZ2_bzCompress(&pFile->bz2, BZ_RUN);
         if( err != BZ_RUN_OK ) {
            hb_fsSetError(HB_BZ2_ERROR_BASE - err);
            nResult = static_cast<HB_SIZE>(-1);
            break;
         }
      }
      if( nResult != static_cast<HB_SIZE>(- 1) ) {
         nResult = nSize - pFile->bz2.avail_in;
      }
      pFile->seek_pos += hb_fileResult(nResult);
   } else {
      hb_fsSetError(HB_FILE_ERR_UNSUPPORTED);
   }

   return nResult;
}

static HB_SIZE s_fileReadAt(PHB_FILE pFile, void * buffer, HB_SIZE nSize, HB_FOFFSET nOffset) // FileFunc
{
   HB_SIZE nResult = 0;

   if( pFile->iMode != FO_WRITE && pFile->seek_pos == nOffset ) {
      nResult = pFile->pFuncs->Read(pFile, buffer, nSize, pFile->nTimeout);
   } else {
      hb_fsSetError(HB_FILE_ERR_UNSUPPORTED);
   }

   return nResult;
}

static HB_SIZE s_fileWriteAt(PHB_FILE pFile, const void * buffer, HB_SIZE nSize, HB_FOFFSET nOffset) // FileFunc
{
   HB_SIZE nResult = 0;

   if( pFile->iMode != FO_READ && pFile->seek_pos == nOffset ) {
      nResult = pFile->pFuncs->Write(pFile, buffer, nSize, pFile->nTimeout);
   } else {
      hb_fsSetError(HB_FILE_ERR_UNSUPPORTED);
   }

   return nResult;
}

static HB_BOOL s_fileTruncAt(PHB_FILE pFile, HB_FOFFSET nOffset) // FileFunc
{
   if( pFile->iMode != FO_READ && pFile->seek_pos == nOffset ) {
      hb_fsSetError(0);
      return true;
   }
   hb_fsSetError(HB_FILE_ERR_UNSUPPORTED);
   return false;
}

static HB_FOFFSET s_fileSeek(PHB_FILE pFile, HB_FOFFSET nOffset, HB_USHORT uiFlags) // FileFunc
{
   if( (uiFlags == FS_SET && nOffset == pFile->seek_pos) || (uiFlags == FS_RELATIVE && nOffset == 0) ) {
      hb_fsSetError(0);
   } else {
      hb_fsSetError(25); /* 'Seek Error' */
   }

   return pFile->seek_pos;
}

static HB_FOFFSET s_fileSize(PHB_FILE pFile) // FileFunc
{
   HB_SYMBOL_UNUSED(pFile);

   /* error below and 0 returned indicate stream File IO */
   hb_fsSetError(HB_FILE_ERR_UNSUPPORTED);

   return 0;
}

static HB_BOOL s_fileEof(PHB_FILE pFile) // FileFunc
{
   return pFile->iMode == FO_WRITE || pFile->fEof || _PHB_FILE->pFuncs->Eof(_PHB_FILE);
}

static void s_fileFlush(PHB_FILE pFile, HB_BOOL fDirty) // FileFunc
{
   if( pFile->iMode != FO_READ && pFile->fInited ) {
      #if 0
      s_bz2_flush(pFile, false);
      #endif
      _PHB_FILE->pFuncs->Flush(_PHB_FILE, fDirty);
   } else {
      hb_fsSetError(0);
   }
}

static void s_fileCommit(PHB_FILE pFile) // FileFunc
{
   if( pFile->iMode != FO_READ && pFile->fInited ) {
      pFile->pFuncs->Flush(pFile, true);
      _PHB_FILE->pFuncs->Commit(_PHB_FILE);
   } else {
      hb_fsSetError(0);
   }
}

static HB_BOOL s_fileConfigure(PHB_FILE pFile, int iIndex, PHB_ITEM pValue) // FileFunc
{
   switch( iIndex ) {
      case HB_VF_TIMEOUT:
      {
         HB_MAXINT nTimeout = pFile->nTimeout;

         if( pValue->isNumeric() ) {
            pFile->nTimeout = hb_itemGetNInt(pValue);
         }
         hb_itemPutNInt(pValue, nTimeout);
         return true;
      }
      case HB_VF_SHUTDOWN:
         hb_itemPutNI(pValue, pFile->iMode);
         return true;

      case HB_VF_RDHANDLE:
      case HB_VF_WRHANDLE:
         hb_itemPutNInt(pValue, _PHB_FILE->pFuncs->Handle(_PHB_FILE));
         return true;

      case HB_VF_IONAME:
      {
         const char * pszNext = nullptr;

         if( _PHB_FILE->pFuncs->Configure(_PHB_FILE, iIndex, pValue) ) {
            pszNext = hb_itemGetCPtr(pValue);
            if( *pszNext == '\0' ) {
               pszNext = nullptr;
            }
         }
         hb_itemPutCPtr(pValue, hb_xstrcpy(nullptr, "BZ:", pszNext, nullptr));
         return true;
      }

      /* TODO? GET/SET BZIP2 compression blockSize100k? */
   }

   return _PHB_FILE->pFuncs->Configure(_PHB_FILE, iIndex, pValue);
}

static HB_FHANDLE s_fileHandle(PHB_FILE pFile) // FileFunc
{
   return pFile ? _PHB_FILE->pFuncs->Handle(_PHB_FILE) : FS_ERROR;
}

static const HB_FILE_FUNCS s_fileFuncs =
{
   s_fileAccept,

   s_fileExists,
   s_fileDelete,
   s_fileRename,
   s_fileCopy,

   s_fileDirExists,
   s_fileDirMake,
   s_fileDirRemove,
   s_fileDirSpace,
   s_fileDirectory,

   s_fileTimeGet,
   s_fileTimeSet,
   s_fileAttrGet,
   s_fileAttrSet,

   s_fileLink,
   s_fileLinkSym,
   s_fileLinkRead,

   s_fileOpen,
   s_fileClose,
   s_fileLock,
   s_fileLockTest,
   s_fileRead,
   s_fileWrite,
   s_fileReadAt,
   s_fileWriteAt,
   s_fileTruncAt,
   s_fileSeek,
   s_fileSize,
   s_fileEof,
   s_fileFlush,
   s_fileCommit,
   s_fileConfigure,
   s_fileHandle
};

static PHB_FILE s_filebz2New(PHB_FILE pFile, int iMode, int iBlockSize)
{
   if( pFile ) {
      auto pFileBZ2 = static_cast<PHB_FILE>(hb_xgrabz(sizeof(HB_FILE)));

      pFileBZ2->pFuncs = &s_fileFuncs;
      pFileBZ2->pFile = pFile;
      pFileBZ2->seek_pos = 0;
      pFileBZ2->nTimeout = -1;
      pFileBZ2->fInited = false;
      pFileBZ2->iMode = iMode;
      pFileBZ2->iBlockSize = iBlockSize;

      pFileBZ2->bz2.bzalloc = s_filebz2Alloc;
      pFileBZ2->bz2.bzfree  = s_filebz2Free;
      pFileBZ2->bz2.opaque = nullptr;

      pFile = pFileBZ2;
   }
   return pFile;
}

HB_FUNC( HB_BZ2IO )
{
;
}

HB_CALL_ON_STARTUP_BEGIN(_hb_file_bz2io_init_)
   hb_fileRegisterFull(&s_fileFuncs);
HB_CALL_ON_STARTUP_END(_hb_file_bz2io_init_)

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup _hb_file_bz2io_init_
#elif defined(HB_DATASEG_STARTUP)
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC(_hb_file_bz2io_init_)
   #include "hbiniseg.hpp"
#endif
