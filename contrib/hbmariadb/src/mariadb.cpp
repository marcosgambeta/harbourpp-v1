/*
 * MySQL DBMS low-level (client API) interface code.
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour) (GC support)
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net> (DATATOSQL(), FILETOSQLBINARY())
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

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbapifs.h"

/* NOTE: To satisfy MySQL headers. */
#if defined( HB_OS_WIN )
   #include <winsock2.h>
#endif

#include "mysql.h"

/* NOTE: OS/2 EMX port of MySQL needs libmysqlclient.a from 3.21.33b build
         which has ST and MT versions of client library. I'm using ST version
         since Harbour is single threaded.
         You need also .h files from same distribution. */

/* GC object handlers */

static HB_GARBAGE_FUNC( MYSQL_release )
{
   void ** ph = static_cast< void ** >( Cargo );

   /* Check if pointer is not nullptr to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      mysql_close( static_cast< MYSQL * >( *ph ) );

      /* set pointer to nullptr to avoid multiple freeing */
      *ph = nullptr;
   }
}

static const HB_GC_FUNCS s_gcMYSQLFuncs =
{
   MYSQL_release,
   hb_gcDummyMark
};

static void hb_MYSQL_ret( MYSQL * p )
{
   if( p )
   {
      void ** ph = static_cast< void ** >( hb_gcAllocate( sizeof( MYSQL * ), &s_gcMYSQLFuncs ) );

      *ph = p;

      hb_retptrGC( ph );
   }
   else
   {
      hb_retptr( nullptr );
   }
}

static MYSQL * hb_MYSQL_par( int iParam )
{
   void ** ph = static_cast< void ** >( hb_parptrGC( &s_gcMYSQLFuncs, iParam ) );

   return ph ? static_cast< MYSQL * >( *ph ) : nullptr;
}

static HB_GARBAGE_FUNC( MYSQL_RES_release )
{
   void ** ph = static_cast< void ** >( Cargo );

   /* Check if pointer is not nullptr to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      mysql_free_result( static_cast< MYSQL_RES * >( *ph ) );

      /* set pointer to nullptr to avoid multiple freeing */
      *ph = nullptr;
   }
}

static const HB_GC_FUNCS s_gcMYSQL_RESFuncs =
{
   MYSQL_RES_release,
   hb_gcDummyMark
};

static void hb_MYSQL_RES_ret( MYSQL_RES * p )
{
   if( p )
   {
      void ** ph = static_cast< void ** >( hb_gcAllocate( sizeof( MYSQL_RES * ), &s_gcMYSQL_RESFuncs ) );

      *ph = p;

      hb_retptrGC( ph );
   }
   else
   {
      hb_retptr( nullptr );
   }
}

static MYSQL_RES * hb_MYSQL_RES_par( int iParam )
{
   void ** ph = static_cast< void ** >( hb_parptrGC( &s_gcMYSQL_RESFuncs, iParam ) );

   return ph ? static_cast< MYSQL_RES * >( *ph ) : nullptr;
}

/* API wrappers */

HB_FUNC( MYSQL_REAL_CONNECT ) /* MYSQL * mysql_real_connect( MYSQL *, char * host, char * user, char * password, char * db, uint port, char *, uint flags ) */
{
   const char * szHost = hb_parc( 1 );
   const char * szUser = hb_parc( 2 );
   const char * szPass = hb_parc( 3 );

#if MYSQL_VERSION_ID > 32200
   MYSQL *      mysql;
   unsigned int port  = static_cast< unsigned int >( hb_parnidef( 4, MYSQL_PORT ) );
   unsigned int flags = static_cast< unsigned int >( hb_parnidef( 5, 0 ) );

   if( ( mysql = mysql_init( static_cast< MYSQL * >( nullptr ) ) ) != nullptr )
   {
      /* from 3.22.x of MySQL there is a new parameter in mysql_real_connect() call, that is char * db
         which is not used here */
      if( mysql_real_connect( mysql, szHost, szUser, szPass, 0, port, nullptr, flags ) )
      {
         hb_MYSQL_ret( mysql );
      }
      else
      {
         mysql_close( mysql );
         hb_retptr( nullptr );
      }
   }
   else
   {
      hb_retptr( nullptr );
   }
#else
   hb_MYSQL_ret( mysql_real_connect( nullptr, szHost, szUser, szPass, 0, nullptr, 0 ) );
#endif
}

HB_FUNC( MYSQL_GET_SERVER_VERSION ) /* long mysql_get_server_version( MYSQL * ) */
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
#if MYSQL_VERSION_ID >= 40100
      hb_retnl( static_cast< long >( mysql_get_server_version( mysql ) ) );
#else
      const char * szVer = mysql_get_server_info( mysql );
      long         lVer  = 0;

      while( *szVer )
      {
         if( *szVer >= '0' && *szVer <= '9' )
         {
            lVer = lVer * 10 + *szVer;
         }
         ++szVer;
      }
      hb_retnl( lVer );
#endif
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_COMMIT ) /* bool mysql_commit( MYSQL * mysql ) */
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
#if MYSQL_VERSION_ID >= 40100
      hb_retnl( static_cast< long >( mysql_commit( mysql ) ) );
#else
      hb_retnl( static_cast< long >( mysql_query( mysql, "COMMIT" ) ) );
#endif
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_ROLLBACK ) /* bool mysql_rollback( MYSQL * mysql ) */
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
#if MYSQL_VERSION_ID >= 40100
      hb_retnl( static_cast< long >( mysql_rollback( mysql ) ) );
#else
      hb_retnl( static_cast< long >( mysql_query( mysql, "ROLLBACK" ) ) );
#endif
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_SELECT_DB ) /* int mysql_select_db( MYSQL *, char * ) */
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
      hb_retnl( static_cast< long >( mysql_select_db( mysql, static_cast< const char * >( hb_parc( 2 ) ) ) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_QUERY ) /* int mysql_query( MYSQL *, char * ) */
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
      hb_retnl( static_cast< long >( mysql_query( mysql, hb_parc( 2 ) ) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_STORE_RESULT ) /* MYSQL_RES * mysql_store_result( MYSQL * ) */
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
      hb_MYSQL_RES_ret( mysql_store_result( mysql ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_USE_RESULT ) /* MYSQL_RES * mysql_use_result( MYSQL * ) */
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
      hb_MYSQL_RES_ret( mysql_use_result( mysql ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_FETCH_ROW ) /* MYSQL_ROW * mysql_fetch_row( MYSQL_RES * ) */
{
   MYSQL_RES * mresult = hb_MYSQL_RES_par( 1 );

   if( mresult )
   {
      unsigned int num_fields = mysql_num_fields( mresult );
      PHB_ITEM     aRow       = hb_itemArrayNew( num_fields );
      MYSQL_ROW    mrow       = mysql_fetch_row( mresult );

      if( mrow )
      {
         unsigned long * lengths = mysql_fetch_lengths( mresult );
         for( unsigned int i = 0; i < num_fields; ++i )
         {
            hb_arraySetCL( aRow, i + 1, mrow[ i ], lengths[ i ] );
         }
      }

      hb_itemReturnRelease( aRow );
   }
}

HB_FUNC( MYSQL_DATA_SEEK ) /* void mysql_data_seek( MYSQL_RES *, unsigned int ) */
{
   MYSQL_RES * mresult = hb_MYSQL_RES_par( 1 );

   if( mresult )
   {
      mysql_data_seek( mresult, static_cast< unsigned int >( hb_parni( 2 ) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_NUM_ROWS ) /* my_ulongulong mysql_num_rows( MYSQL_RES * ) */
{
   MYSQL_RES * mresult = hb_MYSQL_RES_par( 1 );

   if( mresult )
   {
      hb_retnint( mysql_num_rows( ( mresult ) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_FETCH_FIELD ) /* MYSQL_FIELD * mysql_fetch_field( MYSQL_RES * ) */
{
   MYSQL_RES * mresult = hb_MYSQL_RES_par( 1 );

   if( mresult )
   {
      /* NOTE: field structure of MySQL has 8 members as of MySQL 3.22.x */
      PHB_ITEM      aField = hb_itemArrayNew( 8 );
      MYSQL_FIELD * mfield = mysql_fetch_field( mresult );

      if( mfield )
      {
         hb_arraySetC(  aField, 1, mfield->name );
         hb_arraySetC(  aField, 2, mfield->table );
         hb_arraySetC(  aField, 3, mfield->def );
         hb_arraySetNL( aField, 4, static_cast< long >( mfield->type ) );
         hb_arraySetNL( aField, 5, mfield->length );
         hb_arraySetNL( aField, 6, mfield->max_length );
         hb_arraySetNL( aField, 7, mfield->flags );
         hb_arraySetNL( aField, 8, mfield->decimals );
      }

      hb_itemReturnRelease( aField );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_FIELD_SEEK ) /* MYSQL_FIELD_OFFSET mysql_field_seek( MYSQL_RES *, MYSQL_FIELD_OFFSET ) */
{
   MYSQL_RES * mresult = hb_MYSQL_RES_par( 1 );

   if( mresult )
   {
      mysql_field_seek( mresult, static_cast< MYSQL_FIELD_OFFSET >( hb_parni( 2 ) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_NUM_FIELDS ) /* unsigned int mysql_num_fields( MYSQL_RES * ) */
{
   MYSQL_RES * mresult = hb_MYSQL_RES_par( 1 );

   if( mresult )
   {
      hb_retnl( mysql_num_fields( ( mresult ) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_FIELD_COUNT ) /* unsigned int mysql_field_count( MYSQL * ) */
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
#if MYSQL_VERSION_ID > 32200
      hb_retnl( mysql_field_count( ( mysql ) ) );
#else
      hb_retnl( 0 );
#endif
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_LIST_FIELDS ) /* MYSQL_RES * mysql_list_fields( MYSQL *, char * ); */
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
      hb_MYSQL_RES_ret( mysql_list_fields( mysql, hb_parc( 2 ), nullptr ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_ERRNO ) /* unsigned int mysql_errno( MYSQL * mysql ); */
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
      hb_retnint( mysql_errno( mysql ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_ERROR ) /* char * mysql_error( MYSQL * ); */
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
      hb_retc( mysql_error( mysql ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_LIST_DBS ) /* MYSQL_RES * mysql_list_dbs( MYSQL *, char * wild ); */
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
      MYSQL_RES * mresult = mysql_list_dbs( mysql, nullptr );
      HB_SIZE     nr      = static_cast< HB_SIZE >( mysql_num_rows( mresult ) );
      PHB_ITEM    aDBs    = hb_itemArrayNew( nr );

      for( HB_SIZE i = 0; i < nr; ++i )
      {
         MYSQL_ROW mrow = mysql_fetch_row( mresult );
         hb_arraySetC( aDBs, i + 1, mrow[ 0 ] );
      }

      mysql_free_result( mresult );
      hb_itemReturnRelease( aDBs );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_LIST_TABLES ) /* MYSQL_RES * mysql_list_tables( MYSQL *, char * wild ); */
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
      const char * cWild   = hb_parc( 2 );
      MYSQL_RES *  mresult = mysql_list_tables( mysql, cWild );
      long         nr      = static_cast< long >( mysql_num_rows( mresult ) );
      PHB_ITEM     aTables = hb_itemArrayNew( nr );

      for( long i = 0; i < nr; ++i )
      {
         MYSQL_ROW mrow = mysql_fetch_row( mresult );
         hb_arraySetC( aTables, i + 1, mrow[ 0 ] );
      }

      mysql_free_result( mresult );
      hb_itemReturnRelease( aTables );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_AFFECTED_ROWS )
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
      hb_retnl( static_cast< long >( mysql_affected_rows( mysql ) ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_GET_HOST_INFO )
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
      hb_retc( mysql_get_host_info( mysql ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_GET_SERVER_INFO )
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
      hb_retc( mysql_get_server_info( mysql ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_INSERT_ID )
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
      hb_retnint( mysql_insert_id( mysql ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_PING ) /* int mysql_ping( MYSQL * mysql ) */
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
      hb_retnint( mysql_ping( mysql ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_REAL_ESCAPE_STRING ) /* unsigned long STDCALL mysql_real_escape_string( MYSQL * mysql, char * to, const char * from, unsigned long length ); */
{
   MYSQL * mysql = hb_MYSQL_par( 1 );

   if( mysql )
   {
      const char *  from   = hb_parcx( 2 );
      unsigned long nSize  = static_cast< unsigned long >( hb_parclen( 2 ) );
      char *        buffer = static_cast< char * >( hb_xgrab( nSize * 2 + 1 ) );
      nSize = mysql_real_escape_string( mysql, buffer, from, nSize );
      hb_retclen_buffer( static_cast< char * >( buffer ), nSize );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 2020, nullptr, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( MYSQL_ESCAPE_STRING )
{
   const char *  from   = hb_parcx( 1 );
   unsigned long nSize  = static_cast< unsigned long >( hb_parclen( 1 ) );
   char *        buffer = static_cast< char * >( hb_xgrab( nSize * 2 + 1 ) );

   nSize = mysql_escape_string( buffer, from, nSize );
   hb_retclen_buffer( static_cast< char * >( buffer ), nSize );
}

static char * filetoBuff( const char * fname, unsigned long * size )
{
   char *     buffer = nullptr;
   HB_FHANDLE handle = hb_fsOpen( fname, FO_READWRITE );

   if( handle != FS_ERROR )
   {
      *size = hb_fsSeek( handle, 0, FS_END );
      hb_fsSeek( handle, 0, FS_SET );
      buffer = static_cast< char * >( hb_xgrab( *size + 1 ) );
      *size  = static_cast< unsigned long >( hb_fsReadLarge( handle, buffer, *size ) );
      buffer[ *size ] = '\0';
      hb_fsClose( handle );
   }
   else
   {
      *size = 0;
   }

   return buffer;
}

HB_FUNC( MYSQL_ESCAPE_STRING_FROM_FILE )
{
   unsigned long nSize;
   char *        from = filetoBuff( hb_parc( 1 ), &nSize );

   if( from )
   {
      char * buffer = static_cast< char * >( hb_xgrab( nSize * 2 + 1 ) );
      nSize = mysql_escape_string( buffer, from, nSize );
      hb_retclen_buffer( buffer, nSize );
      hb_xfree( from );
   }
}