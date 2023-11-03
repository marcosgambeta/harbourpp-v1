/*
 * hb_compile*() - compiler interface
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbcomp.hpp"

static void s_pp_msg(void * cargo, int iErrorFmt, int iLine, const char * szModule, char cPrefix, int iValue,
                     const char * szText, const char * szPar1, const char * szPar2)
{
   HB_SYMBOL_UNUSED(cargo);

   /* ignore all warning messages and errors when break or quit request */
   if( cPrefix != 'W' && hb_vmRequestQuery() == 0 ) {
      char szMsgBuf[512], szLine[512];
      PHB_ITEM pError;

      hb_snprintf(szMsgBuf, sizeof(szMsgBuf), szText, szPar1, szPar2);
      if( !szModule || *szModule == 0 || strcmp(szModule, "{SOURCE}.prg") == 0 ) {
         hb_snprintf(szLine, sizeof(szLine), "line:%i", iLine);
      } else {
         hb_snprintf(szLine, sizeof(szLine), iErrorFmt == HB_ERRORFMT_CLIPPER ? "%s(%i)" : "%s:%i", szModule, iLine);
      }
      pError = hb_errRT_New(ES_ERROR, "COMPILER", 1001, static_cast<HB_ERRCODE>(iValue), szMsgBuf, szLine, 0 /*OsCode*/, EF_NONE);
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
}

static int s_pp_openFile(void * cargo, char * szFileName, HB_BOOL fBefore, HB_BOOL fSysFile, HB_BOOL fBinary, HB_PATHNAMES * pIncludePaths,
                         HB_BOOL * pfNested, FILE ** file_ptr, const char ** pBufPtr, HB_SIZE * pnLen, HB_BOOL * pfFree)
{
   HB_SYMBOL_UNUSED(fSysFile);
   HB_SYMBOL_UNUSED(fBinary);
   HB_SYMBOL_UNUSED(pIncludePaths);
   HB_SYMBOL_UNUSED(pfNested);
   HB_SYMBOL_UNUSED(file_ptr);

   if( !fBefore ) {
      HB_COMP_DECL = static_cast<PHB_COMP>(cargo);
      PHB_ITEM pIncItem = static_cast<PHB_ITEM>(HB_COMP_PARAM->cargo);

      if( pIncItem ) {
         if( HB_IS_HASH(pIncItem) ) {
            auto pFileItem = hb_hashGetCItemPtr(pIncItem, szFileName);

            if( pFileItem ) {
               HB_SIZE nLen = hb_itemGetCLen(pFileItem);
               if( nLen ) {
                  *pBufPtr = hb_itemGetCPtr(pFileItem);
                  *pnLen = nLen;
                  *pfFree = false;
                  return HB_PP_OPEN_OK;
               }
            }
         }
      }
   }

   return HB_PP_OPEN_FILE;
}

static void hb_compGenArgList(int iFirst, int iLast, int * pArgC, const char *** pArgV, PHB_ITEM * pIncItem, PHB_PP_OPEN_FUNC * pOpenFunc, PHB_PP_MSG_FUNC * pMsgFunc)
{
   if( pMsgFunc ) {
      *pMsgFunc = nullptr;
      if( HB_ISLOG(iFirst) ) {
         if( hb_parl(iFirst) ) {
            *pMsgFunc = s_pp_msg;
         }
         ++iFirst;
      }
   }

   if( pIncItem && pOpenFunc ) {
      *pOpenFunc = nullptr;
      *pIncItem = hb_param(iFirst, Harbour::Item::HASH);
      if( *pIncItem ) {
         ++iFirst;
         *pOpenFunc = s_pp_openFile;
      }
   }

   PHB_ITEM pParam;
   int argc = 1;

   for( int i = iFirst; i <= iLast; ++i ) {
      pParam = hb_param(i, Harbour::Item::ARRAY | Harbour::Item::STRING);
      if( pParam ) {
         if( HB_IS_ARRAY(pParam) ) {
            HB_SIZE nPos = hb_arrayLen(pParam);
            if( nPos ) {
               do {
                  if( hb_arrayGetType(pParam, nPos) & Harbour::Item::STRING ) {
                     ++argc;
                  }
               } while( --nPos );
            }
         } else if( HB_IS_STRING(pParam) ) {
            ++argc;
         }
      }
   }

   const char ** argv = static_cast<const char**>(hb_xgrab(sizeof(char*) * (argc + 1)));
   argc = 0;
   for( int i = iFirst; i <= iLast; ++i ) {
      pParam = hb_param(i, Harbour::Item::ARRAY | Harbour::Item::STRING);
      if( pParam ) {
         if( HB_IS_ARRAY(pParam) ) {
            HB_SIZE nLen = hb_arrayLen(pParam);
            for( HB_SIZE nPos = 1; nPos <= nLen; ++nPos ) {
               if( hb_arrayGetType(pParam, nPos) & Harbour::Item::STRING ) {
                  argv[argc++] = hb_arrayGetCPtr(pParam, nPos);
               }
            }
         } else if( HB_IS_STRING(pParam) ) {
            argv[argc++] = hb_itemGetCPtr(pParam);
         }
      }
   }
   argv[argc] = nullptr;

   *pArgC = argc;
   *pArgV = argv;
}

HB_FUNC( HB_COMPILE )
{
   int argc;
   const char ** argv;
   PHB_ITEM pIncItem;
   PHB_PP_OPEN_FUNC pOpenFunc;
   PHB_PP_MSG_FUNC pMsgFunc;
   hb_compGenArgList(1, hb_pcount(), &argc, &argv, &pIncItem, &pOpenFunc, &pMsgFunc);
   hb_retni(hb_compMainExt(argc, argv, nullptr, nullptr, nullptr, 0, pIncItem, pOpenFunc, pMsgFunc));
   hb_xfree(static_cast<void*>(argv));
}

HB_FUNC( HB_COMPILEBUF )
{
   int argc;
   const char ** argv;
   PHB_ITEM pIncItem;
   PHB_PP_OPEN_FUNC pOpenFunc;
   PHB_PP_MSG_FUNC pMsgFunc;
   hb_compGenArgList(1, hb_pcount(), &argc, &argv, &pIncItem, &pOpenFunc, &pMsgFunc);
   HB_BYTE * pBuffer;
   HB_SIZE nLen;
   int iResult = hb_compMainExt(argc, argv, &pBuffer, &nLen, nullptr, 0, pIncItem, pOpenFunc, pMsgFunc);
   hb_xfree(static_cast<void*>(argv));

   if( iResult == EXIT_SUCCESS && pBuffer ) {
      hb_retclen_buffer(reinterpret_cast<char*>(pBuffer), nLen);
   }
}

HB_FUNC( HB_COMPILEFROMBUF )
{
   auto szSource = hb_parc(1);

   if( szSource ) {
      int argc;
      const char ** argv;
      PHB_ITEM pIncItem;
      PHB_PP_OPEN_FUNC pOpenFunc;
      PHB_PP_MSG_FUNC pMsgFunc;
      hb_compGenArgList(2, hb_pcount(), &argc, &argv, &pIncItem, &pOpenFunc, &pMsgFunc);
      HB_BYTE * pBuffer;
      HB_SIZE nLen;
      int iResult = hb_compMainExt(argc, argv, &pBuffer, &nLen, szSource, 0, pIncItem, pOpenFunc, pMsgFunc);
      hb_xfree(static_cast<void*>(argv));

      if( iResult == EXIT_SUCCESS && pBuffer ) {
         hb_retclen_buffer(reinterpret_cast<char*>(pBuffer), nLen);
      }
   }
}
