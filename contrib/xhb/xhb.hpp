//
// Header file for C functions in xHarbour contrib directory
//
// Copyright 2008 {list of individual authors and e-mail addresses}
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

#ifndef HB_XHB_H_
#define HB_XHB_H_

#include "hbsetup.hpp"
#include <hbdate.hpp>

HB_EXTERN_BEGIN

/* functions in xhberror.c */
extern HB_EXPORT const char * hb_errGetProcName( HB_ITEM *pError );
extern HB_EXPORT HB_ITEM *hb_errPutProcName( HB_ITEM *pError, const char * szProcName );
extern HB_EXPORT uint32_t hb_errGetProcLine( HB_ITEM *pError );
extern HB_EXPORT HB_ITEM *hb_errPutProcLine( HB_ITEM *pError, uint32_t uiProcLine );
extern HB_EXPORT const char * hb_errGetModuleName( HB_ITEM *pError );
extern HB_EXPORT HB_ITEM *hb_errPutModuleName( HB_ITEM *pError, const char * szModuleName );
extern HB_EXPORT HB_ITEM *hb_errGetCallStack( HB_ITEM *pError );
extern HB_EXPORT HB_ITEM *hb_errPutCallStack( HB_ITEM *pError, HB_ITEM *pCallStack );

/* functions in hboutdbg.c */
extern HB_EXPORT HB_BOOL hb_OutDebugName( HB_ITEM *pName );
extern HB_EXPORT void hb_OutDebug( const char * szMsg, HB_SIZE nMsgLen );

/* functions in dbgfxc.c */
extern HB_EXPORT HB_BOOL hb_ToOutDebugOnOff( HB_BOOL bOnOff );
extern HB_EXPORT void hb_ToOutDebug( const char * sTraceMsg, ... );
extern HB_EXPORT HB_BOOL hb_ToLogFileOnOff( HB_BOOL bOnOff );
extern HB_EXPORT HB_BOOL hb_EmptyLogFile( HB_BOOL bOnOff );
extern HB_EXPORT void hb_ToLogFile( const char * sFile, const char * sTraceMsg, ... );

#define hb_seconds()  hb_dateSeconds()

HB_EXTERN_END

#endif
