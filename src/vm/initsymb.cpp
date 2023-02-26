/*
 * Forces initialization of runtime support symbols
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

#include "hbapi.hpp"
#include "hbvm.hpp"

HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( ABS );
HB_FUNC_EXTERN( ASC );
HB_FUNC_EXTERN( AT );
HB_FUNC_EXTERN( BOF );
HB_FUNC_EXTERN( BREAK );
HB_FUNC_EXTERN( CDOW );
HB_FUNC_EXTERN( CHR );
HB_FUNC_EXTERN( CMONTH );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( CTOD );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( DAY );
HB_FUNC_EXTERN( DELETED );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DOW );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DTOS );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( EXP );
HB_FUNC_EXTERN( FCOUNT );
HB_FUNC_EXTERN( FIELDNAME );
HB_FUNC_EXTERN( FLOCK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( INKEY );
HB_FUNC_EXTERN( INT );
HB_FUNC_EXTERN( LASTREC );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( LOCK );
HB_FUNC_EXTERN( LOG );
HB_FUNC_EXTERN( LOWER );
HB_FUNC_EXTERN( LTRIM );
HB_FUNC_EXTERN( MAX );
HB_FUNC_EXTERN( MIN );
HB_FUNC_EXTERN( MONTH );
HB_FUNC_EXTERN( PCOL );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC_EXTERN( PROW );
HB_FUNC_EXTERN( RECCOUNT );
HB_FUNC_EXTERN( RECNO );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( RLOCK );
HB_FUNC_EXTERN( ROUND );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( RTRIM );
HB_FUNC_EXTERN( SECONDS );
HB_FUNC_EXTERN( SELECT );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( SETPOSBS );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( SQRT );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( TIME );
HB_FUNC_EXTERN( TRANSFORM );
HB_FUNC_EXTERN( TRIM );
HB_FUNC_EXTERN( TYPE );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( VAL );
HB_FUNC_EXTERN( WORD );
HB_FUNC_EXTERN( YEAR );

static HB_SYMB symbols[] = {
   { "AADD",      { HB_FS_PUBLIC }, { HB_FUNCNAME( AADD )      }, nullptr },
   { "ABS",       { HB_FS_PUBLIC }, { HB_FUNCNAME( ABS )       }, nullptr },
   { "ASC",       { HB_FS_PUBLIC }, { HB_FUNCNAME( ASC )       }, nullptr },
   { "AT",        { HB_FS_PUBLIC }, { HB_FUNCNAME( AT )        }, nullptr },
   { "BOF",       { HB_FS_PUBLIC }, { HB_FUNCNAME( BOF )       }, nullptr },
   { "BREAK",     { HB_FS_PUBLIC }, { HB_FUNCNAME( BREAK )     }, nullptr },
   { "CDOW",      { HB_FS_PUBLIC }, { HB_FUNCNAME( CDOW )      }, nullptr },
   { "CHR",       { HB_FS_PUBLIC }, { HB_FUNCNAME( CHR )       }, nullptr },
   { "CMONTH",    { HB_FS_PUBLIC }, { HB_FUNCNAME( CMONTH )    }, nullptr },
   { "COL",       { HB_FS_PUBLIC }, { HB_FUNCNAME( COL )       }, nullptr },
   { "CTOD",      { HB_FS_PUBLIC }, { HB_FUNCNAME( CTOD )      }, nullptr },
   { "DATE",      { HB_FS_PUBLIC }, { HB_FUNCNAME( DATE )      }, nullptr },
   { "DAY",       { HB_FS_PUBLIC }, { HB_FUNCNAME( DAY )       }, nullptr },
   { "DELETED",   { HB_FS_PUBLIC }, { HB_FUNCNAME( DELETED )   }, nullptr },
   { "DEVPOS",    { HB_FS_PUBLIC }, { HB_FUNCNAME( DEVPOS )    }, nullptr },
   { "DOW",       { HB_FS_PUBLIC }, { HB_FUNCNAME( DOW )       }, nullptr },
   { "DTOC",      { HB_FS_PUBLIC }, { HB_FUNCNAME( DTOC )      }, nullptr },
   { "DTOS",      { HB_FS_PUBLIC }, { HB_FUNCNAME( DTOS )      }, nullptr },
   { "EMPTY",     { HB_FS_PUBLIC }, { HB_FUNCNAME( EMPTY )     }, nullptr },
   { "EOF",       { HB_FS_PUBLIC }, { HB_FUNCNAME( EOF )       }, nullptr },
   { "EXP",       { HB_FS_PUBLIC }, { HB_FUNCNAME( EXP )       }, nullptr },
   { "FCOUNT",    { HB_FS_PUBLIC }, { HB_FUNCNAME( FCOUNT )    }, nullptr },
   { "FIELDNAME", { HB_FS_PUBLIC }, { HB_FUNCNAME( FIELDNAME ) }, nullptr },
   { "FLOCK",     { HB_FS_PUBLIC }, { HB_FUNCNAME( FLOCK )     }, nullptr },
   { "FOUND",     { HB_FS_PUBLIC }, { HB_FUNCNAME( FOUND )     }, nullptr },
   { "INKEY",     { HB_FS_PUBLIC }, { HB_FUNCNAME( INKEY )     }, nullptr },
   { "INT",       { HB_FS_PUBLIC }, { HB_FUNCNAME( INT )       }, nullptr },
   { "LASTREC",   { HB_FS_PUBLIC }, { HB_FUNCNAME( LASTREC )   }, nullptr },
   { "LEFT",      { HB_FS_PUBLIC }, { HB_FUNCNAME( LEFT )      }, nullptr },
   { "LEN",       { HB_FS_PUBLIC }, { HB_FUNCNAME( LEN )       }, nullptr },
   { "LOCK",      { HB_FS_PUBLIC }, { HB_FUNCNAME( LOCK )      }, nullptr },
   { "LOG",       { HB_FS_PUBLIC }, { HB_FUNCNAME( LOG )       }, nullptr },
   { "LOWER",     { HB_FS_PUBLIC }, { HB_FUNCNAME( LOWER )     }, nullptr },
   { "LTRIM",     { HB_FS_PUBLIC }, { HB_FUNCNAME( LTRIM )     }, nullptr },
   { "MAX",       { HB_FS_PUBLIC }, { HB_FUNCNAME( MAX )       }, nullptr },
   { "MIN",       { HB_FS_PUBLIC }, { HB_FUNCNAME( MIN )       }, nullptr },
   { "MONTH",     { HB_FS_PUBLIC }, { HB_FUNCNAME( MONTH )     }, nullptr },
   { "PCOL",      { HB_FS_PUBLIC }, { HB_FUNCNAME( PCOL )      }, nullptr },
   { "PCOUNT",    { HB_FS_PUBLIC }, { HB_FUNCNAME( PCOUNT )    }, nullptr },
   { "PROW",      { HB_FS_PUBLIC }, { HB_FUNCNAME( PROW )      }, nullptr },
   { "RECCOUNT",  { HB_FS_PUBLIC }, { HB_FUNCNAME( RECCOUNT )  }, nullptr },
   { "RECNO",     { HB_FS_PUBLIC }, { HB_FUNCNAME( RECNO )     }, nullptr },
   { "REPLICATE", { HB_FS_PUBLIC }, { HB_FUNCNAME( REPLICATE ) }, nullptr },
   { "RLOCK",     { HB_FS_PUBLIC }, { HB_FUNCNAME( RLOCK )     }, nullptr },
   { "ROUND",     { HB_FS_PUBLIC }, { HB_FUNCNAME( ROUND )     }, nullptr },
   { "ROW",       { HB_FS_PUBLIC }, { HB_FUNCNAME( ROW )       }, nullptr },
   { "RTRIM",     { HB_FS_PUBLIC }, { HB_FUNCNAME( RTRIM )     }, nullptr },
   { "SECONDS",   { HB_FS_PUBLIC }, { HB_FUNCNAME( SECONDS )   }, nullptr },
   { "SELECT",    { HB_FS_PUBLIC }, { HB_FUNCNAME( SELECT )    }, nullptr },
   { "SETPOS",    { HB_FS_PUBLIC }, { HB_FUNCNAME( SETPOS )    }, nullptr },
   { "SETPOSBS",  { HB_FS_PUBLIC }, { HB_FUNCNAME( SETPOSBS )  }, nullptr },
   { "SPACE",     { HB_FS_PUBLIC }, { HB_FUNCNAME( SPACE )     }, nullptr },
   { "SQRT",      { HB_FS_PUBLIC }, { HB_FUNCNAME( SQRT )      }, nullptr },
   { "STR",       { HB_FS_PUBLIC }, { HB_FUNCNAME( STR )       }, nullptr },
   { "SUBSTR",    { HB_FS_PUBLIC }, { HB_FUNCNAME( SUBSTR )    }, nullptr },
   { "TIME",      { HB_FS_PUBLIC }, { HB_FUNCNAME( TIME )      }, nullptr },
   { "TRANSFORM", { HB_FS_PUBLIC }, { HB_FUNCNAME( TRANSFORM ) }, nullptr },
   { "TRIM",      { HB_FS_PUBLIC }, { HB_FUNCNAME( TRIM )      }, nullptr },
   { "TYPE",      { HB_FS_PUBLIC }, { HB_FUNCNAME( TYPE )      }, nullptr },
   { "UPPER",     { HB_FS_PUBLIC }, { HB_FUNCNAME( UPPER )     }, nullptr },
   { "VAL",       { HB_FS_PUBLIC }, { HB_FUNCNAME( VAL )       }, nullptr },
   { "WORD",      { HB_FS_PUBLIC }, { HB_FUNCNAME( WORD )      }, nullptr },
   { "YEAR",      { HB_FS_PUBLIC }, { HB_FUNCNAME( YEAR )      }, nullptr }
};

/* NOTE: The system symbol table with runtime functions HAVE TO be called last */

void hb_vmSymbolInit_RT(void)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_vmSymbolInit_RT()"));
#endif

   hb_vmProcessSymbols(symbols, HB_SIZEOFARRAY(symbols), "", 0, 0);
}
