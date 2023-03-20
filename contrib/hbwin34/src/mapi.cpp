/*
 * Simple MAPI wrapper
 *
 * Copyright 2009 Viktor Szakats
 * Copyright 2009 Toninho (toninhofwi yahoo.com.br)
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

#include "hbapi.hpp" /* for HB_OS_* detection */

#if defined(UNICODE)
#  define HB_UNICODE_ORI
#endif

#if defined(UNICODE)
#   undef UNICODE
#endif

#include "hbwapi.hpp"
#include <mapi.h>

#if !defined(MAPI_RECEIPT_REQUESTED)
#  if defined(MAPI_RECIPIENT_REQUESTED)
#     define MAPI_RECEIPT_REQUESTED  MAPI_RECIPIENT_REQUESTED
#  else
#     define MAPI_RECEIPT_REQUESTED  0x00000002L
#  endif
#endif

#if defined(__WATCOMC__) || defined(__CYGWIN__)
typedef ULONG (PASCAL * LPMAPISENDMAIL)(LHANDLE, ULONG, lpMapiMessage, FLAGS, ULONG);
#endif

HB_FUNC( WIN_MAPISENDMAIL )
{
   /* Set default return value */
   hb_retnint(-1);

   PHB_ITEM pFrom     = hb_param(8, Harbour::Item::ARRAY);
   PHB_ITEM pRecpList = hb_param(9, Harbour::Item::ARRAY);
   PHB_ITEM pFileList = hb_param(10, Harbour::Item::ARRAY);

   HB_SIZE nRecpCount = pRecpList ? hb_arrayLen(pRecpList) : 0;
   HB_SIZE nFileCount = pFileList ? hb_arrayLen(pFileList) : 0;

   int iString = 0;

   MapiRecipDesc origin;
   memset(&origin, 0, sizeof(origin));

   void ** hString = static_cast<void**>(hb_xgrab((4 + 2 + (2 * nRecpCount) + (2 * nFileCount)) * sizeof(void*)));

   MapiMessage note;
   memset(&note, 0, sizeof(note));
   note.lpszSubject      = const_cast<LPSTR>(HB_PARSTR(1, &hString[iString++], nullptr));
   note.lpszNoteText     = const_cast<LPSTR>(HB_PARSTR(2, &hString[iString++], nullptr));
   note.lpszMessageType  = const_cast<LPSTR>(HB_PARSTR(3, &hString[iString++], nullptr));
   note.lpszDateReceived = const_cast<LPSTR>(HB_PARSTRDEF(4, &hString[iString++], nullptr));

   if( nRecpCount )
   {
      note.lpRecips = static_cast<MapiRecipDesc*>(hb_xgrabz(nRecpCount * sizeof(MapiRecipDesc)));
   }

   if( nFileCount )
   {
      note.lpFiles = static_cast<MapiFileDesc*>(hb_xgrabz(nFileCount * sizeof(MapiFileDesc)));
   }

   if( hb_parl(6) )
   {
      note.flFlags |= MAPI_RECEIPT_REQUESTED;
   }

   FLAGS flags = MAPI_LOGON_UI;

   if( hb_parl(7) )
   {
      flags |= MAPI_DIALOG;
   }

   if( pFrom && hb_arrayLen(pFrom) >= 2 )
   {
      origin.lpszName = const_cast<LPSTR>(HB_ARRAYGETSTR(pFrom, 1, &hString[iString++], nullptr));
      origin.lpszAddress = const_cast<LPSTR>(HB_ARRAYGETSTR(pFrom, 2, &hString[iString++], nullptr)); /* optional */
      origin.ulRecipClass = MAPI_ORIG;
      note.lpOriginator = &origin;
   }
   else if( HB_ISCHAR(8) )
   {
      origin.lpszName = const_cast<LPSTR>(HB_PARSTR(8, &hString[iString++], nullptr));
      origin.ulRecipClass = MAPI_ORIG;
      note.lpOriginator = &origin;
   }

   for( HB_SIZE i = 0; i < nRecpCount; ++i )
   {
      PHB_ITEM pItem = hb_arrayGetItemPtr(pRecpList, i + 1);

      if( HB_IS_ARRAY(pItem) && hb_arrayLen(pItem) >= 2 )
      {
         if( hb_arrayGetCLen(pItem, 1) > 0 )
         {
            note.lpRecips[note.nRecipCount].lpszName = const_cast<LPSTR>(HB_ARRAYGETSTR(pItem, 1, &hString[iString++], nullptr));

            if( hb_arrayGetCLen(pItem, 2) > 0 )
            {
               note.lpRecips[note.nRecipCount].lpszAddress = const_cast<LPSTR>(HB_ARRAYGETSTR(pItem, 2, &hString[iString++], nullptr));
            }
         }
         else if( hb_arrayGetCLen(pItem, 2) > 0 )
         {
            note.lpRecips[note.nRecipCount].lpszName = const_cast<LPSTR>(HB_ARRAYGETSTR(pItem, 2, &hString[iString++], nullptr));
         }
         else
         {
            continue;
         }

         if( hb_arrayLen(pItem) >= 3 && HB_IS_NUMERIC(hb_arrayGetItemPtr(pItem, 3)) )
         {
            note.lpRecips[note.nRecipCount].ulRecipClass = static_cast<ULONG>(hb_arrayGetNL(pItem, 3));
         }
         else
         {
            note.lpRecips[note.nRecipCount].ulRecipClass = MAPI_TO;
         }

         ++note.nRecipCount;
      }
      else if( HB_IS_STRING(pItem) )
      {
         note.lpRecips[note.nRecipCount].lpszName = const_cast<LPSTR>(HB_ITEMGETSTR(pItem, &hString[iString++], nullptr));
         note.lpRecips[note.nRecipCount].ulRecipClass = MAPI_TO;

         ++note.nRecipCount;
      }
   }

   for( HB_SIZE i = 0; i < nFileCount; ++i )
   {
      PHB_ITEM pItem = hb_arrayGetItemPtr(pFileList, i + 1);

      if( HB_IS_ARRAY(pItem) && hb_arrayLen(pItem) >= 1 && hb_arrayGetCLen(pItem, 1) > 0 )
      {
         note.lpFiles[note.nFileCount].lpszPathName = const_cast<LPSTR>(HB_ARRAYGETSTR(pItem, 1, &hString[iString++], nullptr));
         note.lpFiles[note.nFileCount].lpszFileName = const_cast<LPSTR>(HB_ARRAYGETSTR(pItem, 2, &hString[iString++], nullptr)); /* optional */
         note.lpFiles[note.nFileCount].nPosition    = static_cast<ULONG>(-1);
         ++note.nFileCount;
      }
      else if( HB_IS_STRING(pItem) )
      {
         note.lpFiles[note.nFileCount].lpszPathName = const_cast<LPSTR>(HB_ITEMGETSTR(pItem, &hString[iString++], nullptr));
         note.lpFiles[note.nFileCount].nPosition    = static_cast<ULONG>(-1);
         ++note.nFileCount;
      }
   }

   hb_retnint(MAPISendMail(0, reinterpret_cast<ULONG_PTR>(GetActiveWindow()), &note, flags, 0));

   if( nRecpCount > 0 )
   {
      hb_xfree(note.lpRecips);
   }

   if( nFileCount > 0 )
   {
      hb_xfree(note.lpFiles);
   }

   while( --iString >= 0 )
   {
      hb_strfree(hString[iString]);
   }

   hb_xfree(hString);
}
