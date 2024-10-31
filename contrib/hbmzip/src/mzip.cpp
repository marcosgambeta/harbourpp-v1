/*
 * Wrapper functions for minizip library
 * Some higher-level ZIP archive functions
 *
 * Copyright 2008 Mindaugas Kavaliauskas <dbtopas.at.dbtopas.lt>
 * Copyright 2011-2013 Viktor Szakats (vszakats.net/harbour) (codepage/unicode)
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

#if !defined(_LARGEFILE64_SOURCE)
#  define _LARGEFILE64_SOURCE  1
#endif

#include <hbapi.hpp>
#include <hbapiitm.hpp>
#include <hbapierr.hpp>
#include <hbapistr.hpp>
#include "hbdate.hpp"
#include "hbset.hpp"

#if !defined(HB_OS_UNIX)
#  undef _LARGEFILE64_SOURCE
#endif

#include "zip.h"
#include "unzip.h"

#if defined(HB_OS_UNIX)
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <unistd.h>
   #include <time.h>
   #include <utime.h>
#elif defined(HB_OS_WIN)
   #include <windows.h>
   #if !defined(INVALID_FILE_ATTRIBUTES)
      #define INVALID_FILE_ATTRIBUTES  (static_cast<DWORD>(-1))
   #endif
   #include "hbwinuni.hpp"
#endif

#if !defined(HB_USE_LARGEFILE64) && defined(HB_OS_UNIX)
   #if defined(__USE_LARGEFILE64)
      /*
       * The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
       * defined and effectively enables lseek64()/flock64()/ftruncate64()
       * functions on 32-bit machines.
       */
      #define HB_USE_LARGEFILE64
   #elif defined(HB_OS_UNIX) && defined(O_LARGEFILE)
      #define HB_USE_LARGEFILE64
   #endif
#endif

#define _ZIP_FLAG_UNICODE  (1 << 11)  /* Language encoding flag (EFS) */

#if defined(HB_OS_UNIX)
   #define _VER_PLATFORM   0x03  /* it's necessary for file attributes in unzip */
#else
   #define _VER_PLATFORM   0x00
#endif

static int _version_made_by(bool fUnicode)
{
   return (fUnicode ? 0x3F /* 6.3.x */ : 0x14 /* 2.0.x */) | (_VER_PLATFORM << 8);
}

#define HB_Z_IOBUF_SIZE    (1024 * 16)

static HB_GARBAGE_FUNC(hb_zipfile_destructor)
{
   auto phZip = static_cast<zipFile*>(Cargo);

   if( *phZip ) {
      zipClose(*phZip, nullptr);
      *phZip = nullptr;
   }
}

static const HB_GC_FUNCS s_gcZipFileFuncs =
{
   hb_zipfile_destructor,
   hb_gcDummyMark
};

static zipFile hb_zipfileParam(int iParam)
{
   auto phZip = static_cast<zipFile*>(hb_parptrGC(&s_gcZipFileFuncs, iParam));

   if( phZip && *phZip ) {
      return *phZip;
   }

   hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   return nullptr;
}

static HB_GARBAGE_FUNC(hb_unzipfile_destructor)
{
   auto phUnzip = static_cast<unzFile*>(Cargo);

   if( *phUnzip ) {
      unzClose(*phUnzip);
      *phUnzip = nullptr;
   }
}

static const HB_GC_FUNCS s_gcUnZipFileFuncs =
{
   hb_unzipfile_destructor,
   hb_gcDummyMark
};

static unzFile hb_unzipfileParam(int iParam)
{
   auto phUnzip = static_cast<unzFile*>(hb_parptrGC(&s_gcUnZipFileFuncs, iParam));

   if( phUnzip && *phUnzip ) {
      return *phUnzip;
   }

   hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   return nullptr;
}

static PHB_FILE hb_fileHandleParam(int iParam, bool * pfFree)
{
   PHB_FILE pFile = nullptr;

   *pfFree = false;
   if( HB_ISNUM(iParam) ) {
      HB_FHANDLE hFile = hb_numToHandle(hb_parnint(iParam));
      if( hFile != FS_ERROR ) {
         pFile = hb_fileFromHandle(hFile);
         *pfFree = true;
      }
   } else {
      pFile = hb_fileParam(iParam);
   }

   if( pFile != nullptr ) {
      return pFile;
   }

   hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   return nullptr;
}

static HB_FATTR hb_translateExtAttr(const char * szFileName, HB_FATTR ulExtAttr)
{
   auto iLen = static_cast<int>(strlen(szFileName));
   if( (iLen > 4 && (hb_stricmp(szFileName + iLen - 4, ".exe") == 0 ||
                     hb_stricmp(szFileName + iLen - 4, ".com") == 0 ||
                     hb_stricmp(szFileName + iLen - 4, ".bat") == 0 ||
                     hb_stricmp(szFileName + iLen - 4, ".cmd") == 0)) ||
       (iLen > 3 && hb_stricmp(szFileName + iLen - 3, ".sh") == 0) ) {
      ulExtAttr |= 0x00490000; /* --x--x--x */
   }

   if( ulExtAttr & HB_FA_READONLY ) {
      ulExtAttr |= 0x01240000;  /* r--r--r-- */
   } else {
      ulExtAttr |= 0x01B60000;  /* rw-rw-rw- */
   }

   if( ulExtAttr & HB_FA_DIRECTORY ) {
      ulExtAttr |= 0x40000000;
   } else {
      ulExtAttr |= 0x80000000;
   }

   return ulExtAttr;
}

/* hb_zipOpen(cFileName, [iMode = HB_ZIP_CREATE], [@cGlobalComment]) --> hZip */
HB_FUNC( HB_ZIPOPEN )
{
   auto szFileName = hb_parc(1);

   if( szFileName ) {
      const char * pszGlobalComment = nullptr;
      char *       pszFree;
      zipFile      hZip = zipOpen2(hb_fsNameConv(szFileName, &pszFree), hb_parnidef(2, APPEND_STATUS_CREATE), &pszGlobalComment, nullptr);

      if( pszFree ) {
         hb_xfree(pszFree);
      }

      if( hZip ) {
         auto phZip = static_cast<zipFile*>(hb_gcAllocate(sizeof(zipFile), &s_gcZipFileFuncs));

         *phZip = hZip;
         hb_retptrGC(phZip);

         if( pszGlobalComment ) {
            hb_storc(pszGlobalComment, 3);
         }
      }
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* hb_zipClose(hZip, [cGlobalComment]) --> nError */
HB_FUNC( HB_ZIPCLOSE )
{
   auto phZip = static_cast<zipFile*>(hb_parptrGC(&s_gcZipFileFuncs, 1));

   if( phZip && *phZip ) {
      zipFile hZip = *phZip;

      *phZip = nullptr;
      hb_retni(zipClose(hZip, hb_parc(2)));
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* hb_zipFileCreate(hZip, cZipName, dDate, cTime, nInternalAttr, nExternalAttr,
                    [nMethod = HB_ZLIB_METHOD_DEFLATE],
                    [nLevel = HB_ZLIB_COMPRESSION_DEFAULT],
                    [cPassword, ulFileCRC32], [cComment], [lUnicode]) --> nError */
HB_FUNC( HB_ZIPFILECREATE )
{
   auto szZipName = hb_parc(2);

   if( szZipName ) {
      auto hZip = hb_zipfileParam(1);

      if( hZip ) {
         auto iMethod = hb_parnidef(7, Z_DEFLATED);
         auto iLevel = hb_parnidef(8, Z_DEFAULT_COMPRESSION);
         long  lJulian, lMillisec;
         int   iYear, iMonth, iDay, iHour, iMinute, iSecond, iMSec;
         uLong flags = 0;

         bool         fUnicode = hb_parl(12);
         void *       hZipName = nullptr;
         void *       hComment = nullptr;
         const char * szComment;

         if( HB_ISTIMESTAMP(3) ) {
            hb_partdt(&lJulian, &lMillisec, 3);
            hb_dateDecode(lJulian, &iYear, &iMonth, &iDay);
            hb_timeDecode(lMillisec, &iHour, &iMinute, &iSecond, &iMSec);
         } else {
            hb_dateDecode(hb_pardl(3), &iYear, &iMonth, &iDay);
            hb_timeStrGet(hb_parc(4), &iHour, &iMinute, &iSecond, &iMSec);
         }

         zip_fileinfo zfi{};

         zfi.tmz_date.tm_hour = iHour;
         zfi.tmz_date.tm_min  = iMinute;
         zfi.tmz_date.tm_sec  = iSecond;

         zfi.tmz_date.tm_year = iYear;
         zfi.tmz_date.tm_mon  = iMonth - 1;
         zfi.tmz_date.tm_mday = iDay;

         zfi.internal_fa = hb_parnl(5);
         zfi.external_fa = hb_parnl(6);
#if !defined(HB_OS_UNIX)
         if( (zfi.external_fa & 0xFFFF0000) == 0 ) {
            zfi.external_fa = hb_translateExtAttr(szZipName, zfi.external_fa);
         }
#endif

         if( fUnicode ) {
            szZipName = hb_parstr_utf8(2, &hZipName, nullptr);
            szComment = hb_parstr_utf8(11, &hComment, nullptr);
            flags    |= _ZIP_FLAG_UNICODE;
         } else {
            szComment = hb_parc(11);
         }

         hb_retni(zipOpenNewFileInZip4(hZip, szZipName, &zfi,
                                       nullptr, 0, nullptr, 0,
                                       szComment, iMethod, iLevel, 0,
                                       -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                       hb_parc(9), hb_parnl(10), _version_made_by(fUnicode), flags));

         if( fUnicode ) {
            hb_strfree(hZipName);
            hb_strfree(hComment);
         }
      }
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* hb_zipFileWrite(hZip, cData [, nLen ]) --> nError */
HB_FUNC( HB_ZIPFILEWRITE )
{
   auto pData = hb_parc(2);

   if( pData ) {
      auto hZip = hb_zipfileParam(1);
      if( hZip ) {
         auto nLen = hb_parclen(2);

         if( HB_ISNUM(3) ) {
            HB_SIZE nWrite = hb_parns(3);
            if( nWrite < nLen ) {
               nLen = nWrite;
            }
         }

         hb_retni(zipWriteInFileInZip(hZip, pData, static_cast<unsigned>(nLen)));
      }
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* hb_zipFileClose(hZip) --> nError */
HB_FUNC( HB_ZIPFILECLOSE )
{
   auto hZip = hb_zipfileParam(1);

   if( hZip ) {
      hb_retni(zipCloseFileInZip(hZip));
   }
}

/* hb_unzipOpen(cFileName) --> hUnzip */
HB_FUNC( HB_UNZIPOPEN )
{
   auto szFileName = hb_parc(1);

   if( szFileName ) {
      char *  pszFree;
      unzFile hUnzip = unzOpen(hb_fsNameConv(szFileName, &pszFree));

      if( pszFree ) {
         hb_xfree(pszFree);
      }

      if( hUnzip ) {
         auto phUnzip = static_cast<unzFile*>(hb_gcAllocate(sizeof(unzFile), &s_gcUnZipFileFuncs));

         *phUnzip = hUnzip;
         hb_retptrGC(phUnzip);
      }
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* hb_unzipClose(hUnzip) --> nError */
HB_FUNC( HB_UNZIPCLOSE )
{
   auto phUnzip = static_cast<unzFile*>(hb_parptrGC(&s_gcUnZipFileFuncs, 1));

   if( phUnzip && *phUnzip ) {
      unzFile hUnzip = *phUnzip;

      *phUnzip = nullptr;
      hb_retni(unzClose(hUnzip));
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* hb_unzipGlobalInfo(hUnzip, @nEntries, @cGlobalComment) --> nError */
HB_FUNC( HB_UNZIPGLOBALINFO )
{
   auto hUnzip = hb_unzipfileParam(1);

   if( hUnzip ) {
      unz_global_info ugi;
      int iResult;

      iResult = unzGetGlobalInfo(hUnzip, &ugi);

      hb_retni(iResult);

      if( iResult == UNZ_OK ) {
         hb_storni(ugi.number_entry, 2);
         if( HB_ISBYREF(3) ) {
            if( ugi.size_comment > 0 ) {
               auto pszComment = static_cast<char*>(hb_xgrab(ugi.size_comment + 1));

               iResult = unzGetGlobalComment(hUnzip, pszComment, ugi.size_comment);
               if( iResult < 0 ) {
                  hb_xfree(pszComment);
                  hb_storc(nullptr, 3);
                  hb_retni(iResult);
               } else {
                  pszComment[iResult] = '\0';
                  if( !hb_storclen_buffer(pszComment, ugi.size_comment, 3) ) {
                     hb_xfree(pszComment);
                  }
               }
            }
         }
      } else {
         hb_storni(0, 2);
         hb_storc(nullptr, 3);
      }
   }
}

/* hb_unzipFileFirst(hUnzip) --> nError */
HB_FUNC( HB_UNZIPFILEFIRST )
{
   auto hUnzip = hb_unzipfileParam(1);

   if( hUnzip ) {
      hb_retni(unzGoToFirstFile(hUnzip));
   }
}

/* hb_unzipFileNext(hUnzip) --> nError */
HB_FUNC( HB_UNZIPFILENEXT )
{
   auto hUnzip = hb_unzipfileParam(1);

   if( hUnzip ) {
      hb_retni(unzGoToNextFile(hUnzip));
   }
}

/* hb_unzipFilePos(hUnzip) --> nPosition */
HB_FUNC( HB_UNZIPFILEPOS )
{
   auto hUnzip = hb_unzipfileParam(1);

   if( hUnzip ) {
      hb_retnint(unzGetOffset(hUnzip));
   }
}

/* hb_unzipFileGoto(hUnzip, nPosition) --> nError */
HB_FUNC( HB_UNZIPFILEGOTO )
{
   auto hUnzip = hb_unzipfileParam(1);

   if( hUnzip ) {
      hb_retni(unzSetOffset(hUnzip, static_cast<uLong>(hb_parnint(2))));
   }
}

/* hb_unzipFileInfo(hUnzip, @cZipName, @dDate, @cTime,
                    @nInternalAttr, @nExternalAttr,
                    @nMethod, @nSize, @nCompressedSize,
                    @lCrypted, @cComment, @nCRC) --> nError */
HB_FUNC( HB_UNZIPFILEINFO )
{
   auto hUnzip = hb_unzipfileParam(1);

   if( hUnzip ) {
      char szFileName[HB_PATH_MAX * 3];
      unz_file_info ufi;
      int  iResult;

      iResult = unzGetCurrentFileInfo(hUnzip, &ufi, szFileName, sizeof(szFileName) - 1, nullptr, 0, nullptr, 0);
      hb_retni(iResult);

      if( iResult == UNZ_OK ) {
         bool fUnicode = (ufi.flag & _ZIP_FLAG_UNICODE) != 0;

         long lJulian, lMillisec;

         szFileName[sizeof(szFileName) - 1] = '\0';

         if( fUnicode ) {
            hb_storstr_utf8(szFileName, 2);
         } else {
            hb_storc(szFileName, 2);
         }

         lJulian = hb_dateEncode(ufi.tmu_date.tm_year, ufi.tmu_date.tm_mon + 1, ufi.tmu_date.tm_mday);
         lMillisec = hb_timeEncode(ufi.tmu_date.tm_hour, ufi.tmu_date.tm_min, ufi.tmu_date.tm_sec, 0);

         hb_stortdt(lJulian, lMillisec, 3);
         if( HB_ISBYREF(4) ) {
            char buf[16];
            hb_snprintf(buf, sizeof(buf), "%02d:%02d:%02d", ufi.tmu_date.tm_hour, ufi.tmu_date.tm_min, ufi.tmu_date.tm_sec);
            hb_storc(buf, 4);
         }
         hb_stornl(ufi.internal_fa, 5);
         hb_stornl(ufi.external_fa, 6);
         hb_stornl(ufi.compression_method, 7);
         hb_storns(ufi.uncompressed_size, 8);
         hb_storns(ufi.compressed_size, 9);
         hb_storl((ufi.flag & 1) != 0, 10);
         hb_stornint(ufi.crc, 12);

         if( ufi.size_file_comment > 0 && HB_ISBYREF(11) ) {
            auto pszComment = static_cast<char*>(hb_xgrab(ufi.size_file_comment + 1));

            iResult = unzGetCurrentFileInfo(hUnzip, &ufi, nullptr, 0, nullptr, 0, pszComment, ufi.size_file_comment);
            pszComment[ufi.size_file_comment] = '\0';
            if( iResult != UNZ_OK ) {
               hb_xfree(pszComment);
               hb_storc(nullptr, 11);
            } else if( fUnicode ) {
               hb_storstrlen_utf8(pszComment, ufi.size_file_comment, 11);
               hb_xfree(pszComment);
            } else if( !hb_storclen_buffer(pszComment, ufi.size_file_comment, 11) ) {
               hb_xfree(pszComment);
            }
         }
      } else {
         hb_storc(nullptr, 2);
         hb_stortdt(0, 0, 3);
         hb_storc(nullptr, 4);
         hb_stornl(0, 5);
         hb_stornl(0, 6);
         hb_stornl(0, 7);
         hb_storns(0, 8);
         hb_storns(0, 9);
         hb_storl(false, 10);
         hb_storc(nullptr, 11);
      }
   }
}

/* hb_unzipFileOpen(hUnzip, [cPassword]) --> nError */
HB_FUNC( HB_UNZIPFILEOPEN )
{
   auto hUnzip = hb_unzipfileParam(1);

   if( hUnzip ) {
      hb_retni(unzOpenCurrentFilePassword(hUnzip, hb_parc(2)));
   }
}

/* hb_unzipFileRead(hUnzip, @cBuf [, nLen ]) --> nRead */
HB_FUNC( HB_UNZIPFILEREAD )
{
   auto pBuffer = hb_param(2, Harbour::Item::STRING);
   char *   buffer;
   HB_SIZE  nSize;

   if( pBuffer && HB_ISBYREF(2) && hb_itemGetWriteCL(pBuffer, &buffer, &nSize) ) {
      auto hUnzip = hb_unzipfileParam(1);

      if( hUnzip ) {
         if( HB_ISNUM(3) ) {
            HB_SIZE nRead = hb_parns(3);
            if( nRead < nSize ) {
               nSize = nRead;
            }
         }

         hb_retns(unzReadCurrentFile(hUnzip, buffer, static_cast<unsigned>(nSize)));
      }
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* hb_unzipFileClose(hUnzip) --> nError */
HB_FUNC( HB_UNZIPFILECLOSE )
{
   auto hUnzip = hb_unzipfileParam(1);

   if( hUnzip ) {
      hb_retni(unzCloseCurrentFile(hUnzip));
   }
}

/*
 *
 * Higher-level functions - not wrappers of minizip code
 *
 */

static HB_BOOL hb_zipGetFileInfoFromHandle(PHB_FILE pFile, HB_U32 * pulCRC, HB_BOOL * pfText)
{
   HB_BOOL fText = pfText != nullptr, fResult = false;
   HB_U32  ulCRC = 0;

   if( pFile != nullptr ) {
      auto pString = static_cast<unsigned char*>(hb_xgrab(HB_Z_IOBUF_SIZE));
      HB_SIZE         nRead;

      do {
         nRead = hb_fileRead(pFile, pString, HB_Z_IOBUF_SIZE, -1);
         if( nRead > 0 && nRead != static_cast<HB_SIZE>(FS_ERROR) ) {
            ulCRC = crc32(ulCRC, pString, static_cast<uInt>(nRead));
            if( fText ) {
               for( HB_SIZE u = 0; u < nRead; ++u ) {
                  if( pString[u] < 0x20 ?
                      (pString[u] != HB_CHAR_HT &&
                       pString[u] != HB_CHAR_LF &&
                       pString[u] != HB_CHAR_CR &&
                       pString[u] != HB_CHAR_EOF ) :
                      (pString[u] >= 0x7F && pString[u] < 0xA0 &&
                       pString[u] != static_cast<unsigned char>(HB_CHAR_SOFT1)) ) {
                     fText = false;
                     break;
                  }
               }
            }
         }
      } while( nRead == HB_Z_IOBUF_SIZE );

      fResult = (hb_fsError() == 0);

      hb_xfree(pString);
   }

   if( pulCRC ) {
      *pulCRC = ulCRC;
   }
   if( pfText ) {
      *pfText = fText;
   }

   return fResult;
}

static HB_BOOL hb_zipGetFileInfo(const char * pszFileName, HB_U32 * pulCRC, HB_BOOL * pfText)
{
   PHB_FILE pFile;
   HB_BOOL  fResult;

   pFile = hb_fileExtOpen(pszFileName, nullptr, FO_READ | FO_SHARED | FO_PRIVATE | FXO_SHARELOCK, nullptr, nullptr);
   fResult = hb_zipGetFileInfoFromHandle(pFile, pulCRC, pfText);
   if( pFile != nullptr ) {
      hb_fileClose(pFile);
   }

   return fResult;
}

/* hb_zipFileCRC32(cFileName) --> nCRC */
HB_FUNC( HB_ZIPFILECRC32 )
{
   auto szFileName = hb_parc(1);

   if( szFileName ) {
      HB_U32 ulCRC = 0;
      if( !hb_zipGetFileInfo(szFileName, &ulCRC, nullptr) ) {
         ulCRC = 0;
      }
      hb_retnint(ulCRC);
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

static int hb_zipStoreFile(zipFile hZip, int iParamFileName, int iParamZipName, const char * szPassword, int iParamComment, bool fUnicode)
{
   auto szFileName = hb_parc(iParamFileName);
   PHB_FILE     pFile;
   HB_SIZE      nLen;
   HB_FATTR     ulExtAttr;
   zip_fileinfo zfi;
   int          iResult;
   HB_BOOL      fError;
   HB_BOOL      fText;
   HB_U32       ulCRC;
   uLong        flags = 0;
   void *       hZipName = nullptr;
   void *       hComment = nullptr;
   char *       szZipName;
   const char * szComment;

   memset(&zfi, 0, sizeof(zfi));
   fError    = false;
   ulExtAttr = 0;

#if defined(HB_OS_WIN)
   if( hb_fileIsLocalName(szFileName) ) {
      LPTSTR  lpFileNameFree;
      LPCTSTR lpFileName = HB_FSNAMECONV(szFileName, &lpFileNameFree);
      DWORD   attr       = GetFileAttributes(lpFileName);

      if( attr != INVALID_FILE_ATTRIBUTES ) {
         ulExtAttr = attr & (FILE_ATTRIBUTE_READONLY | FILE_ATTRIBUTE_HIDDEN | FILE_ATTRIBUTE_SYSTEM | FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_ARCHIVE);
      } else {
         fError = true;
      }

      if( lpFileNameFree ) {
         hb_xfree(lpFileNameFree);
      }
   } else
#elif defined(HB_OS_UNIX)
   if( hb_fileIsLocalName(szFileName) ) {
      struct tm   st;
      time_t      ftime;
      char *      pszFree;
#  if defined(HB_USE_LARGEFILE64)
      struct stat64 statbuf;
      if( stat64(hb_fsNameConv(szFileName, &pszFree), &statbuf) == 0 )
#  else
      struct stat statbuf;
      if( stat(hb_fsNameConv(szFileName, &pszFree), &statbuf) == 0 )
#  endif
      {
         if( S_ISDIR(statbuf.st_mode) ) {
            ulExtAttr |= 0x40000000;
            ulExtAttr |= 0x10; /* FILE_ATTRIBUTE_DIRECTORY */
         } else {
            ulExtAttr |= 0x80000000;
            ulExtAttr |= 0x20; /* FILE_ATTRIBUTE_ARCHIVE */
         }

         ulExtAttr |= ((statbuf.st_mode & S_IXOTH) ? 0x00010000 : 0) |
                      ((statbuf.st_mode & S_IWOTH) ? 0x00020000 : 0) |
                      ((statbuf.st_mode & S_IROTH) ? 0x00040000 : 0) |
                      ((statbuf.st_mode & S_IXGRP) ? 0x00080000 : 0) |
                      ((statbuf.st_mode & S_IWGRP) ? 0x00100000 : 0) |
                      ((statbuf.st_mode & S_IRGRP) ? 0x00200000 : 0) |
                      ((statbuf.st_mode & S_IXUSR) ? 0x00400000 : 0) |
                      ((statbuf.st_mode & S_IWUSR) ? 0x00800000 : 0) |
                      ((statbuf.st_mode & S_IRUSR) ? 0x01000000 : 0);

         ftime = statbuf.st_mtime;
#  if defined(HB_HAS_LOCALTIME_R)
         localtime_r(&ftime, &st);
#  else
         st = *localtime(&ftime);
#  endif

         zfi.tmz_date.tm_sec  = st.tm_sec;
         zfi.tmz_date.tm_min  = st.tm_min;
         zfi.tmz_date.tm_hour = st.tm_hour;
         zfi.tmz_date.tm_mday = st.tm_mday;
         zfi.tmz_date.tm_mon  = st.tm_mon;
         zfi.tmz_date.tm_year = st.tm_year;
      } else {
         fError = true;
      }

      if( pszFree ) {
         hb_xfree(pszFree);
      }
   } else
#endif
   {
      HB_FATTR attr;
      long lJulian, lMillisec;

      if( !hb_fileAttrGet(szFileName, &attr) ) {
         ulExtAttr = 0x81B60020;  /* HB_FA_ARCHIVE | rw-rw-rw- */
      } else {
#if defined(HB_OS_UNIX)
         if( attr & HB_FA_DIRECTORY ) {
            ulExtAttr |= 0x40000000;
         } else {
            ulExtAttr |= 0x80000000;
            ulExtAttr |= HB_FA_ARCHIVE;
         }
         /* Harbour uses the same binary values for unix access rights and
          * DOS/WIN/OS2 attributes so we can use them directly
          */
         ulExtAttr |= attr & (HB_FA_RWXU | HB_FA_RWXG | HB_FA_RWXO);
#endif
         ulExtAttr |= attr & (HB_FA_READONLY | HB_FA_HIDDEN | HB_FA_SYSTEM | HB_FA_DIRECTORY | HB_FA_ARCHIVE);
      }

      if( hb_fileTimeGet(szFileName, &lJulian, &lMillisec) ) {
         int iYear, iMonth, iDay;
         int iHour, iMinute, iSecond, iMSec;

         hb_dateDecode(lJulian, &iYear, &iMonth, &iDay);
         hb_timeDecode(lMillisec, &iHour, &iMinute, &iSecond, &iMSec);

         zfi.tmz_date.tm_sec  = iSecond;
         zfi.tmz_date.tm_min  = iMinute;
         zfi.tmz_date.tm_hour = iHour;
         zfi.tmz_date.tm_mday = iDay;
         zfi.tmz_date.tm_mon  = iMonth - 1;
         zfi.tmz_date.tm_year = iYear;
      }
   }

   if( fError ) {
      return -200;
   }

#if !defined(HB_OS_UNIX)
   ulExtAttr = hb_translateExtAttr(szFileName, ulExtAttr);
#endif

   if( !HB_ISCHAR(iParamZipName) ) {
      iParamZipName = iParamFileName;
   }

   if( fUnicode ) {
      szZipName = hb_strdup(hb_parstr_utf8(iParamZipName, &hZipName, nullptr));
      szComment = hb_parstr_utf8(iParamComment, &hComment, nullptr);
      flags    |= _ZIP_FLAG_UNICODE;
   } else {
      szZipName = hb_strdup(hb_parc(iParamZipName));
      szComment = hb_parc(iParamComment);
   }

   nLen = strlen(szZipName);
   if( iParamZipName != iParamFileName ) {
      /* change path separators to '/' */
      while( nLen-- ) {
         if( szZipName[nLen] == '\\' ) {
            szZipName[nLen] = '/';
         }
      }
   } else {
      while( nLen-- ) {
         if( szZipName[nLen] == '/' || szZipName[nLen] == '\\' ) {
            memmove(szZipName, &szZipName[nLen + 1], strlen(szZipName) - nLen);
            break;
         }
      }
   }

   fText = false;
   ulCRC = 0;

   zfi.external_fa = ulExtAttr;
   /* TODO: zip.exe test: 0 for binary file, 1 for text. Does not depend on
      extension. We should analyze content of file to determine this??? */
   zfi.internal_fa = 0;

   if( ulExtAttr & 0x40000000 ) {
      iResult = zipOpenNewFileInZip4(hZip, szZipName, &zfi, nullptr, 0, nullptr, 0, szComment,
                                     Z_DEFLATED, Z_DEFAULT_COMPRESSION, 0,
                                     -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                     szPassword, ulCRC, _version_made_by(fUnicode), flags);
      if( iResult == 0 ) {
         zipCloseFileInZip(hZip);
      }
   } else {
      pFile = hb_fileExtOpen(szFileName, nullptr, FO_READ | FO_SHARED | FO_PRIVATE | FXO_SHARELOCK, nullptr, nullptr);
      if( pFile != nullptr ) {
#if defined(HB_OS_WIN)
         if( hb_fileIsLocal(pFile) ) {
            FILETIME   ftutc, ft;
            SYSTEMTIME st;

            if( GetFileTime(reinterpret_cast<HANDLE>(hb_fileHandle(pFile)), nullptr, nullptr, &ftutc) &&
                FileTimeToLocalFileTime(&ftutc, &ft) &&
                FileTimeToSystemTime(&ft, &st) ) {
               zfi.tmz_date.tm_sec  = st.wSecond;
               zfi.tmz_date.tm_min  = st.wMinute;
               zfi.tmz_date.tm_hour = st.wHour;
               zfi.tmz_date.tm_mday = st.wDay;
               zfi.tmz_date.tm_mon  = st.wMonth - 1;
               zfi.tmz_date.tm_year = st.wYear;
            }
         }
#endif
         if( szPassword ) {
            if( hb_zipGetFileInfo(szFileName, &ulCRC, &fText) ) {
               zfi.internal_fa = fText ? 1 : 0;
            }
         }

         iResult = zipOpenNewFileInZip4(hZip, szZipName, &zfi, nullptr, 0, nullptr, 0, szComment,
                                        Z_DEFLATED, Z_DEFAULT_COMPRESSION, 0,
                                        -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                        szPassword, ulCRC, _version_made_by(fUnicode), flags);
         if( iResult == 0 ) {
            auto pString = static_cast<char*>(hb_xgrab(HB_Z_IOBUF_SIZE));

            while( ( nLen = hb_fileRead(pFile, pString, HB_Z_IOBUF_SIZE, -1) ) > 0 && nLen != static_cast<HB_SIZE>(FS_ERROR) ) {
               zipWriteInFileInZip(hZip, pString, static_cast<unsigned>(nLen));
            }

            hb_xfree(pString);

            zipCloseFileInZip(hZip);
         }
         hb_fileClose(pFile);
      } else {
         iResult = -200 - hb_fsError();
      }
   }

   hb_xfree(szZipName);

   if( fUnicode ) {
      hb_strfree(hZipName);
      hb_strfree(hComment);
   }

   return iResult;
}

/* hb_zipStoreFile(hZip, cFileName, [cZipName], [cPassword], [cComment], [lUnicode]) --> nError */
HB_FUNC( HB_ZIPSTOREFILE )
{
   if( hb_parc(2) ) {
      auto hZip = hb_zipfileParam(1);

      if( hZip ) {
         hb_retni(hb_zipStoreFile(hZip, 2, 3, hb_parc(4), 5, hb_parl(6)));
      }
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

static int hb_zipStoreFileHandle(zipFile hZip, PHB_FILE pFile, int iParamZipName, const char * szPassword, int iParamComment, bool fUnicode)
{
   HB_SIZE      nLen;
   zip_fileinfo zfi;
   int          iResult;
   HB_BOOL      fText;
   HB_U32       ulCRC;

   uLong flags = 0;

   void *       hZipName = nullptr;
   void *       hComment = nullptr;
   char *       szZipName;
   const char * szComment;

   if( pFile == nullptr ) {
      return -200;
   }

   if( fUnicode ) {
      szZipName = hb_strdup(hb_parstr_utf8(iParamZipName, &hZipName, nullptr));
      szComment = hb_parstr_utf8(iParamComment, &hComment, nullptr);
      flags    |= _ZIP_FLAG_UNICODE;
   } else {
      szZipName = hb_strdup(hb_parc(iParamZipName));
      szComment = hb_parc(iParamComment);
   }

   /* change path separators to '/' */

   nLen = strlen(szZipName);
   while( nLen-- ) {
      if( szZipName[nLen] == '\\' ) {
         szZipName[nLen] = '/';
      }
   }

   memset(&zfi, 0, sizeof(zfi));

   zfi.external_fa      = 0x81B60020;
   zfi.tmz_date.tm_sec  = 0;
   zfi.tmz_date.tm_min  = 0;
   zfi.tmz_date.tm_hour = 0;
   zfi.tmz_date.tm_mday = 1;
   zfi.tmz_date.tm_mon  = 0;
   zfi.tmz_date.tm_year = 0;

   ulCRC = 0;
   fText = false;
   if( szPassword && hb_zipGetFileInfoFromHandle(pFile, &ulCRC, &fText) ) {
      zfi.internal_fa = fText ? 1 : 0;
   } else {
      /* TODO: zip.exe test: 0 for binary file, 1 for text. Does not depend on
         extension. We should analyze content of file to determine this??? */
      zfi.internal_fa = 0;
   }   

   iResult = zipOpenNewFileInZip4(hZip, szZipName, &zfi, nullptr, 0, nullptr, 0, szComment,
                                  Z_DEFLATED, Z_DEFAULT_COMPRESSION, 0,
                                  -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                  szPassword, ulCRC, _version_made_by(fUnicode), flags);
   if( iResult == 0 ) {
      auto pString = static_cast<char*>(hb_xgrab(HB_Z_IOBUF_SIZE));
      hb_fileSeek(pFile, 0, FS_SET);
      while( (nLen = hb_fileRead(pFile, pString, HB_Z_IOBUF_SIZE, -1)) > 0 && nLen != static_cast<HB_SIZE>(FS_ERROR) ) {
         zipWriteInFileInZip(hZip, pString, static_cast<unsigned>(nLen));
      }
      hb_xfree(pString);

      zipCloseFileInZip(hZip);
   }

   hb_xfree(szZipName);

   if( fUnicode ) {
      hb_strfree(hZipName);
      hb_strfree(hComment);
   }

   return iResult;
}

/* hb_zipStoreFileHandle(hZip, fhnd, cZipName, [cPassword], [cComment], [lUnicode]) --> nError */
HB_FUNC( HB_ZIPSTOREFILEHANDLE )
{
   if( HB_ISCHAR(3) ) {
      auto hZip = hb_zipfileParam(1);

      if( hZip ) {
         bool fFree;
         auto pFile = hb_fileHandleParam(2, &fFree);

         if( pFile != nullptr ) {
            hb_retni(hb_zipStoreFileHandle(hZip, pFile, 3, hb_parc(4), 5, hb_parl(6)));
            if( fFree ) {
               hb_fileDetach(pFile);
            }
         }
      }
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

static int hb_unzipExtractCurrentFile(unzFile hUnzip, const char * szFileName, const char * szPassword)
{
   char          szNameRaw[HB_PATH_MAX * 3];
   char *        szName;
   HB_SIZE       nPos, nLen;
   unz_file_info ufi;
   int           iResult;
   PHB_FILE      pFile;

   iResult = unzGetCurrentFileInfo(hUnzip, &ufi, szNameRaw, sizeof(szNameRaw) - 1, nullptr, 0, nullptr, 0);
   if( iResult != UNZ_OK ) {
      return iResult;
   }

   iResult = unzOpenCurrentFilePassword(hUnzip, szPassword);

   if( iResult != UNZ_OK ) {
      return iResult;
   }

   if( szFileName ) {
      szName = hb_strdup(szFileName);
   } else {
      bool fUnicode = (ufi.flag & _ZIP_FLAG_UNICODE) != 0;

      if( fUnicode ) {
         auto pTemp = hb_itemPutStrUTF8(nullptr, szNameRaw);

         szName = hb_strdup(hb_itemGetCPtr(pTemp));

         hb_itemRelease(pTemp);
      } else {
         szName = hb_strdup(szNameRaw);
      }
   }

   nLen = strlen(szName);

   /* Test shows that files in subdirectories can be stored to zip file without
      explicitly adding directory. So, let's create a required path */

   nPos = 1;
   while( nPos < nLen ) {
      char cSep = szName[nPos];

      /* allow both path separators, ignore terminating path separator */
      if( (cSep == '\\' || cSep == '/') && nPos < nLen - 1 ) {
         szName[nPos] = '\0';
         hb_fileDirMake(szName);
         szName[nPos] = cSep;
      }
      nPos++;
   }

   if( ufi.external_fa & 0x40000000 ) /* DIRECTORY */ {
      if( !hb_fileDirMake(szName) ) {
         iResult = -200 - hb_fsError();
      }
   } else {
      pFile = hb_fileExtOpen(szName, nullptr, FO_READWRITE | FO_EXCLUSIVE | FO_PRIVATE | FXO_TRUNCATE | FXO_SHARELOCK, nullptr, nullptr);
      if( pFile != nullptr ) {
         auto pString = static_cast<char*>(hb_xgrab(HB_Z_IOBUF_SIZE));

         while( (iResult = unzReadCurrentFile(hUnzip, pString, HB_Z_IOBUF_SIZE)) > 0 ) {
            if( hb_fileWrite(pFile, pString, static_cast<HB_SIZE>(iResult), -1) != static_cast<HB_SIZE>(iResult) ) {
               break;
            }
         }

         hb_xfree(pString);

#if defined(HB_OS_WIN)
         if( hb_fileIsLocal(pFile) ) {
            FILETIME   ftutc, ft;
            SYSTEMTIME st;

            st.wSecond       = static_cast<WORD>(ufi.tmu_date.tm_sec);
            st.wMinute       = static_cast<WORD>(ufi.tmu_date.tm_min);
            st.wHour         = static_cast<WORD>(ufi.tmu_date.tm_hour);
            st.wDay          = static_cast<WORD>(ufi.tmu_date.tm_mday);
            st.wMonth        = static_cast<WORD>(ufi.tmu_date.tm_mon) + 1;
            st.wYear         = static_cast<WORD>(ufi.tmu_date.tm_year);
            st.wMilliseconds = 0;

            if( SystemTimeToFileTime(&st, &ft) && LocalFileTimeToFileTime(&ft, &ftutc) ) {
               SetFileTime(reinterpret_cast<HANDLE>(hb_fileHandle(pFile)), &ftutc, &ftutc, &ftutc);
            }
         }
#endif

         hb_fileClose(pFile);
      } else {
         iResult = -200 - hb_fsError();
      }
   }
   unzCloseCurrentFile(hUnzip);

#if defined(HB_OS_WIN)
   if( hb_fileIsLocalName(szName) ) {
      LPTSTR  lpFileNameFree;
      LPCTSTR lpFileName = HB_FSNAMECONV(szName, &lpFileNameFree);

      SetFileAttributes(static_cast<LPCTSTR>(lpFileName), ufi.external_fa & 0xFF);

      if( lpFileNameFree ) {
         hb_xfree(lpFileNameFree);
      }
   } else
#elif defined(HB_OS_UNIX)
   if( hb_fileIsLocalName(szName) ) {
      struct utimbuf utim;
      struct tm      st;

      char *       pszFree;
      auto szNameOS = hb_fsNameConv(szName, &pszFree);

      HB_FATTR ulAttr = ufi.external_fa;

      if( (ulAttr & 0xFFFF0000) == 0 ) {
         ulAttr = hb_translateExtAttr(szName, ulAttr);
      }

      ( void ) chmod(szNameOS,
                     ((ulAttr & 0x00010000) ? S_IXOTH : 0) |
                     ((ulAttr & 0x00020000) ? S_IWOTH : 0) |
                     ((ulAttr & 0x00040000) ? S_IROTH : 0) |
                     ((ulAttr & 0x00080000) ? S_IXGRP : 0) |
                     ((ulAttr & 0x00100000) ? S_IWGRP : 0) |
                     ((ulAttr & 0x00200000) ? S_IRGRP : 0) |
                     ((ulAttr & 0x00400000) ? S_IXUSR : 0) |
                     ((ulAttr & 0x00800000) ? S_IWUSR : 0) |
                     ((ulAttr & 0x01000000) ? S_IRUSR : 0));
      memset(&st, 0, sizeof(st));

      st.tm_sec  = ufi.tmu_date.tm_sec;
      st.tm_min  = ufi.tmu_date.tm_min;
      st.tm_hour = ufi.tmu_date.tm_hour;
      st.tm_mday = ufi.tmu_date.tm_mday;
      st.tm_mon  = ufi.tmu_date.tm_mon;
      st.tm_year = ufi.tmu_date.tm_year - 1900;
      st.tm_isdst = -1;

      utim.actime = utim.modtime = mktime(&st);
      /*( void )*/ utime(szNameOS, &utim);

      if( pszFree ) {
         hb_xfree(pszFree);
      }
   } else
#endif
   {
      long lJulian, lMillisec;
      HB_FATTR ulAttr = ufi.external_fa;

      lJulian = hb_dateEncode(ufi.tmu_date.tm_year, ufi.tmu_date.tm_mon + 1, ufi.tmu_date.tm_mday);
      lMillisec = hb_timeEncode(ufi.tmu_date.tm_hour, ufi.tmu_date.tm_min, ufi.tmu_date.tm_sec, 0);
      hb_fileTimeSet(szName, lJulian, lMillisec);

#if defined(HB_OS_UNIX)
      if( (ulAttr & 0xFFFF0000) == 0 ) {
         ulAttr = hb_translateExtAttr(szName, ulAttr);
      }
      ulAttr &= 0x01FF0000;
#else
      ulAttr &= 0xFF;
#endif
      hb_fileAttrSet(szName, ulAttr);
   }

   hb_xfree(szName);

   return iResult;
}

/* hb_unzipExtractCurrentFile(hZip, [cFileName], [cPassword]) --> nError */
HB_FUNC( HB_UNZIPEXTRACTCURRENTFILE )
{
   auto hUnzip = hb_unzipfileParam(1);

   if( hUnzip ) {
      hb_retni(hb_unzipExtractCurrentFile(hUnzip, hb_parc(2), hb_parc(3)));
   }
}

static int hb_unzipExtractCurrentFileToHandle(unzFile hUnzip, PHB_FILE pFile, const char * szPassword)
{
   unz_file_info ufi;

   if( pFile == nullptr ) {
      return -200;
   }

   int iResult = unzGetCurrentFileInfo(hUnzip, &ufi, nullptr, 0, nullptr, 0, nullptr, 0);
   if( iResult != UNZ_OK ) {
      return iResult;
   }

   iResult = unzOpenCurrentFilePassword(hUnzip, szPassword);

   if( iResult != UNZ_OK ) {
      return iResult;
   }

   if( !(ufi.external_fa & 0x40000000) ) /* DIRECTORY */ {
      auto pString = static_cast<char*>(hb_xgrab(HB_Z_IOBUF_SIZE));

      while( (iResult = unzReadCurrentFile(hUnzip, pString, HB_Z_IOBUF_SIZE)) > 0 ) {
         if( hb_fileWrite(pFile, pString, static_cast<HB_SIZE>(iResult), -1) != static_cast<HB_SIZE>(iResult) ) {
            break;
         }
      }

      hb_xfree(pString);

#if defined(HB_OS_WIN)
      if( hb_fileIsLocal(pFile) ) {
         FILETIME   ftutc, ft;
         SYSTEMTIME st;

         st.wSecond       = static_cast<WORD>(ufi.tmu_date.tm_sec);
         st.wMinute       = static_cast<WORD>(ufi.tmu_date.tm_min);
         st.wHour         = static_cast<WORD>(ufi.tmu_date.tm_hour);
         st.wDay          = static_cast<WORD>(ufi.tmu_date.tm_mday);
         st.wMonth        = static_cast<WORD>(ufi.tmu_date.tm_mon) + 1;
         st.wYear         = static_cast<WORD>(ufi.tmu_date.tm_year);
         st.wMilliseconds = 0;

         if( SystemTimeToFileTime(&st, &ft) && LocalFileTimeToFileTime(&ft, &ftutc) ) {
            SetFileTime(reinterpret_cast<HANDLE>(hb_fileHandle(pFile)), &ftutc, &ftutc, &ftutc);
         }
      }
#endif
   }
   unzCloseCurrentFile(hUnzip);

   return iResult;
}

/* hb_unzipExtractCurrentFileToHandle(hZip, fhnd, [cPassword]) --> nError */
HB_FUNC( HB_UNZIPEXTRACTCURRENTFILETOHANDLE )
{
   auto hUnzip = hb_unzipfileParam(1);

   if( hUnzip ) {
      bool fFree;
      auto pFile = hb_fileHandleParam(2, &fFree);

      if( pFile != nullptr ) {
         hb_retni(hb_unzipExtractCurrentFileToHandle(hUnzip, pFile, hb_parc(3)));
         if( fFree ) {
            hb_fileDetach(pFile);
         }
      }
   }
}

static int hb_zipDeleteFile(const char * szZipFile, const char * szFileMask)
{
   char            szTempFile[HB_PATH_MAX];
   char            szCurrFile[HB_PATH_MAX * 3];
   unz_global_info ugi;
   unz_file_info   ufi;
   zip_fileinfo    zfi;
   char *          pszGlobalComment = nullptr;
   char *          pszFileComment   = nullptr;
   void *          pExtraField      = nullptr;
   void *          pLocalExtraField = nullptr;
   int    iFilesLeft = 0;
   int    iFilesDel  = 0;
   int    iExtraFieldLen;
   int    method;
   int    level;
   int    iResult;
   char * pszFree;

   /* open source file */
   unzFile hUnzip = unzOpen(hb_fsNameConv(szZipFile, &pszFree));

   if( pszFree ) {
      hb_xfree(pszFree);
   }

   if( hUnzip == nullptr ) {
      return UNZ_ERRNO;
   }

   PHB_FNAME pFileName = hb_fsFNameSplit(szZipFile);
   PHB_FILE pFile = hb_fileCreateTemp(pFileName->szPath, nullptr, FC_NORMAL, szTempFile);
   zipFile hZip = nullptr;
   if( pFile != nullptr ) {
      hb_fileClose(pFile);
      hZip = zipOpen(szTempFile, APPEND_STATUS_CREATE);
   }
   hb_xfree(pFileName);

   if( hZip == nullptr ) {
      unzClose(hUnzip);
      return UNZ_ERRNO;
   }

   iResult = unzGetGlobalInfo(hUnzip, &ugi);
   if( iResult == UNZ_OK ) {
      if( ugi.size_comment > 0 ) {
         pszGlobalComment = static_cast<char*>(hb_xgrab(ugi.size_comment + 1));
         if( static_cast<uLong>(unzGetGlobalComment(hUnzip, pszGlobalComment, ugi.size_comment)) != ugi.size_comment ) {
            iResult = UNZ_ERRNO;
         }
         pszGlobalComment[ugi.size_comment] = '\0';
      }
      if( iResult == UNZ_OK ) {
         iResult = unzGoToFirstFile(hUnzip);
      }
   }

   while( iResult == UNZ_OK ) {
      iResult = unzGetCurrentFileInfo(hUnzip, &ufi, szCurrFile, sizeof(szCurrFile) - 1, nullptr, 0, nullptr, 0);
      if( iResult != UNZ_OK ) {
         break;
      }

      if( hb_strMatchFile(szCurrFile, szFileMask) ) {
         iFilesDel++;
      } else {
         bool fUnicode;

         if( ufi.size_file_extra ) {
            pExtraField = static_cast<char*>(hb_xgrab(ufi.size_file_extra));
         }
         if( ufi.size_file_comment ) {
            pszFileComment = static_cast<char*>(hb_xgrab(ufi.size_file_comment + 1));
         }

         iResult = unzGetCurrentFileInfo(hUnzip, &ufi, nullptr, 0, pExtraField, ufi.size_file_extra, pszFileComment, ufi.size_file_comment);
         if( pszFileComment ) {
            pszFileComment[ufi.size_file_comment] = '\0';
         }
         if( iResult != UNZ_OK ) {
            break;
         }

         iResult = unzOpenCurrentFile2(hUnzip, &method, &level, 1);
         if( iResult != UNZ_OK ) {
            break;
         }

         iExtraFieldLen = unzGetLocalExtrafield(hUnzip, nullptr, 0);
         if( iExtraFieldLen < 0 ) {
            iResult = UNZ_ERRNO;
            break;
         } else if( iExtraFieldLen > 0 ) {
            pLocalExtraField = hb_xgrab(iExtraFieldLen);
            if( unzGetLocalExtrafield(hUnzip, pLocalExtraField, iExtraFieldLen) != iExtraFieldLen ) {
               iResult = UNZ_ERRNO;
               break;
            }
         }

         fUnicode = (ufi.flag & _ZIP_FLAG_UNICODE) != 0;

         memset(&zfi, 0, sizeof(zfi));
         memcpy(&zfi.tmz_date, &ufi.tmu_date, sizeof(tm_unz));
         zfi.dosDate     = ufi.dosDate;
         zfi.internal_fa = ufi.internal_fa;
         zfi.external_fa = ufi.external_fa;

         iResult = zipOpenNewFileInZip4(hZip, szCurrFile, &zfi, pLocalExtraField, iExtraFieldLen, pExtraField, ufi.size_file_extra, pszFileComment,
                                        method, level, 1,
                                        -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                        nullptr, 0, _version_made_by(fUnicode), ufi.flag);
         if( iResult != UNZ_OK ) {
            break;
         }

         if( ufi.compressed_size ) {
            auto buffer = hb_xgrab(HB_Z_IOBUF_SIZE);
            uLong  ulLeft = ufi.compressed_size;

            while( ulLeft > 0 ) {
               int iRead = HB_MIN(ulLeft, HB_Z_IOBUF_SIZE);
               iResult = unzReadCurrentFile(hUnzip, static_cast<voidp>(buffer), iRead);
               if( iResult < 0 ) {
                  break;
               }
               if( iResult != iRead ) {
                  iResult = UNZ_ERRNO;
                  break;
               }
               iResult = zipWriteInFileInZip(hZip, static_cast<voidp>(buffer), iRead);
               if( iResult != UNZ_OK ) {
                  break;
               }
               ulLeft -= iRead;
            }
            hb_xfree(buffer);
            if( iResult != UNZ_OK ) {
               break;
            }
         }

         iResult = zipCloseFileInZipRaw(hZip, ufi.uncompressed_size, ufi.crc);
         if( iResult != UNZ_OK ) {
            break;
         }

         iResult = unzCloseCurrentFile(hUnzip);
         if( iResult != UNZ_OK ) {
            break;
         }

         if( pExtraField ) {
            hb_xfree(pExtraField);
            pExtraField = nullptr;
         }
         if( pszFileComment ) {
            hb_xfree(pszFileComment);
            pszFileComment = nullptr;
         }
         if( pLocalExtraField ) {
            hb_xfree(pLocalExtraField);
            pLocalExtraField = nullptr;
         }
         iFilesLeft++;
      }
      iResult = unzGoToNextFile(hUnzip);
   }

   if( pExtraField ) {
      hb_xfree(pExtraField);
   }
   if( pszFileComment ) {
      hb_xfree(pszFileComment);
   }
   if( pLocalExtraField ) {
      hb_xfree(pLocalExtraField);
   }

   if( iFilesDel == 0 ) {
      iResult = UNZ_ERRNO;
   } else if( iResult == UNZ_END_OF_LIST_OF_FILE ) {
      iResult = UNZ_OK;
   }

   if( iResult != UNZ_OK ) {
      zipClose(hZip, nullptr);
   } else {
      iResult = zipClose(hZip, pszGlobalComment);
   }
   unzClose(hUnzip);
   if( pszGlobalComment ) {
      hb_xfree(pszGlobalComment);
   }

   if( iResult != UNZ_OK ) {
      hb_fileDelete(szTempFile);
   } else {
      hb_fileDelete(szZipFile);

      if( iFilesLeft == 0 ) {
         hb_fileDelete(szTempFile);
      } else if( !hb_fileRename(szTempFile, szZipFile) ) {
         iResult = UNZ_ERRNO;
      }
   }

   return iResult;
}

/* hb_zipDeleteFile(cZipFile, cFileMask) --> nError */
HB_FUNC( HB_ZIPDELETEFILE )
{
   auto szZipFile = hb_parc(1);
   auto szFileMask = hb_parc(2);

   if( szZipFile && szFileMask ) {
      hb_retni(hb_zipDeleteFile(szZipFile, szFileMask));
   } else {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}
