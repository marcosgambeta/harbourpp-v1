// DBF structures
// Copyright 1999 Bruno Cantero <bruno@issnet.net>

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

#ifndef HB_DBF_H_
#define HB_DBF_H_

#include "hbapirdd.hpp"

HB_EXTERN_BEGIN

/* DBF header */

typedef struct _DBFHEADER
{
   uint8_t   bVersion;
   uint8_t   bYear;
   uint8_t   bMonth;
   uint8_t   bDay;
   uint8_t   ulRecCount[ 4 ];
   uint8_t   uiHeaderLen[ 2 ];
   uint8_t   uiRecordLen[ 2 ];
   uint8_t   bReserved1[ 2 ];
   uint8_t   bTransaction;       /* 1-transaction begin */
   uint8_t   bEncrypted;         /* 1-encrypted table */
   uint8_t   bReserved2[ 12 ];
   uint8_t   bHasTags;           /* bit filed: 1-production index, 2-memo file in VFP */
   uint8_t   bCodePage;
   uint8_t   bReserved3[ 2 ];
} DBFHEADER;

typedef DBFHEADER * LPDBFHEADER;



/* DBF fields */

typedef struct _DBFFIELD
{
   uint8_t   bName[ 11 ];
   uint8_t   bType;
   uint8_t   bReserved1[ 4 ];      /* offset from record begin in FP */
   uint8_t   bLen;
   uint8_t   bDec;
   uint8_t   bFieldFlags;          /* 1-system column, 2-nullable, 4-binary */
   uint8_t   bCounter[ 4 ];        /* auto-increment counter */
   uint8_t   bStep;                /* auto-increment step */
   uint8_t   bReserved2[ 7 ];
   uint8_t   bHasTag;
} DBFFIELD;

typedef DBFFIELD * LPDBFFIELD;



/* SMT MEMO field */

typedef struct _SMTFIELD
{
   uint8_t   type[ 2 ];
   uint8_t   length[ 4 ];
   uint8_t   block[ 4 ];
} SMTFIELD;

typedef SMTFIELD * LPSMTFIELD;


HB_EXTERN_END

#endif // HB_DBF_H_
