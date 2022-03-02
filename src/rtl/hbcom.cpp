/*
 * Serial communication functions
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef _GNU_SOURCE
#  define _GNU_SOURCE
#endif

#include "hbapi.h"

#if defined( HB_OS_UNIX )
#  if defined( HB_OS_VXWORKS )
#     if ! defined( HB_HAS_SIOLIB )
#        define HB_HAS_SIOLIB
#     endif
#  else
#     if ! defined( HB_HAS_TERMIOS )
#        define HB_HAS_TERMIOS
#     endif
#  endif
#  if defined( HB_OS_SUNOS )
#     if ! defined( BSD_COMP )
#        define BSD_COMP
#     endif
#  endif
#endif

#if defined( HB_HAS_TERMIOS )
#  include <termios.h>
#  include <fcntl.h>
#  include <sys/ioctl.h>
#  include <unistd.h>
#  include <errno.h>
#  if defined( HB_OS_UNIX )
#     include <sys/time.h>
#     include <sys/types.h>
#     if ! defined( HB_HAS_POLL ) && ! defined( HB_NO_POLL ) && defined( _POSIX_C_SOURCE ) && _POSIX_C_SOURCE >= 200112L
         /* use poll() instead of select() to avoid FD_SETSIZE (1024 in Linux)
            file handle limit */
#        define HB_HAS_POLL
#     endif
#     if defined( HB_HAS_POLL )
#        include <poll.h>
#     endif
#  endif
#  if defined( HB_OS_HPUX )
#     include <sys/modem.h>
#  endif
#elif defined( HB_HAS_SIOLIB )
#  include <sioLib.h>
#elif defined( HB_HAS_DOSSRL )
#  include "../../src/3rd/hbdossrl/serial.h"
#elif defined( HB_HAS_PMCOM )
#  include "../../src/3rd/hbpmcom/com.h"
#elif defined( HB_OS_WIN )
#  include <windows.h>
#  include "hbwinuni.h"
#endif

#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbapicom.h"
#include "hbvm.h"
#include "hbinit.h"
#include "hbdate.h"
#include "hbthread.h"

struct HB_COM
{
#if defined( HB_HAS_TERMIOS )
   HB_FHANDLE     fd;
#  if ! defined( HB_OS_UNIX )
   HB_MAXINT      rdtimeout;
#  endif
#elif defined( HB_OS_WIN )
   HANDLE         hComm;
   HB_MAXINT      rdtimeout;
   HB_MAXINT      wrtimeout;
#endif
   int            status;
   int            error;
   int            oserr;
   int            port;
   char *         name;
   #if 0
   struct termios tio;
   #endif
};

using PHB_COM = HB_COM *;

static HB_CRITICAL_NEW( s_comMtx );
#define HB_COM_LOCK()      do { hb_threadEnterCriticalSection( &s_comMtx )
#define HB_COM_UNLOCK()    hb_threadLeaveCriticalSection( &s_comMtx ); } while(0)

static HB_COM s_comList[ HB_COM_PORT_MAX ];

static void hb_comCloseAll( void )
{
   for( int iPort = 0; iPort < HB_COM_PORT_MAX; ++iPort )
   {
      if( s_comList[ iPort ].status & HB_COM_OPEN )
      {
         hb_comClose( iPort + 1 );
      }

      if( s_comList[ iPort ].name )
      {
         hb_xfree( s_comList[ iPort ].name );
         s_comList[ iPort ].name = nullptr;
      }
   }
}

static void hb_comSetComError( PHB_COM pCom, int iError )
{
   pCom->error = iError;
   pCom->oserr = 0;
}

static PHB_COM hb_comGetPort( int iPort, int iStatus )
{
   if( iPort >= 1 && iPort <= HB_COM_PORT_MAX )
   {
      PHB_COM pCom = &s_comList[ iPort - 1 ];
      if( iStatus == HB_COM_ANY || ( iStatus & pCom->status ) != 0 )
      {
         return pCom;
      }
      if( iStatus & HB_COM_ENABLED )
      {
         hb_comSetComError( pCom, HB_COM_ERR_WRONGPORT );
      }
      else
      {
         hb_comSetComError( pCom, HB_COM_ERR_CLOSED );
      }
   }
   return nullptr;
}

static const char * hb_comGetNameRaw( PHB_COM pCom, char * buffer, int size )
{
   const char * name = pCom->name;

   if( name == nullptr )
   {
#if defined( HB_OS_UNIX )
#  if defined( HB_OS_SUNOS )
      hb_snprintf( buffer, size, "/dev/tty%c", pCom->port + 'a' - 1 );
#  elif defined( HB_OS_HPUX )
      hb_snprintf( buffer, size, "/dev/tty%dp0", pCom->port );
#  elif defined( HB_OS_AIX )
      hb_snprintf( buffer, size, "/dev/tty%d", pCom->port );
#  elif defined( HB_OS_MINIX )
      hb_snprintf( buffer, size, "/dev/tty%02d", pCom->port - 1 );
#  elif defined( HB_OS_IRIX )
      hb_snprintf( buffer, size, "/dev/ttyf%d", pCom->port );
#  elif defined( HB_OS_DIGITAL_UNIX )
      hb_snprintf( buffer, size, "/dev/ttyf%02d", pCom->port );
#  elif defined( HB_OS_DARWIN )
      hb_snprintf( buffer, size, "/dev/cuaa%d", pCom->port - 1 );
#  else /* defined( HB_OS_LINUX ) || defined( HB_OS_CYGWIN ) || ... */
      hb_snprintf( buffer, size, "/dev/ttyS%d", pCom->port - 1 );
#  endif
#else
      if( hb_iswinnt() || hb_iswince() )
      {
         hb_snprintf( buffer, size, "\\\\.\\COM%d", pCom->port );
      }
      else
      {
         hb_snprintf( buffer, size, "COM%d", pCom->port );
      }
#endif
      name = buffer;
   }
   return name;
}

static const char * hb_comGetName( PHB_COM pCom, char * buffer, int size )
{
   const char * name;

   HB_COM_LOCK();
   name = hb_comGetNameRaw( pCom, buffer, size );
   if( name != buffer )
   {
      name = hb_strncpy( buffer, name, size - 1 );
   }
   HB_COM_UNLOCK();

   return name;
}

static int hb_comGetPortNum( const char * pszName )
{
   int iPort = 0;

#if defined( HB_OS_UNIX )
#  if defined( HB_OS_SUNOS )
   if( strncmp( pszName, "/dev/tty", 8 ) == 0 && pszName[ 8 ] >= 'a' && pszName[ 9 ] == '\0' )
   {
      iPort = pszName[ 8 ] - 'a' + 1;
   }
#  else
   int iLen = 0;
#     if defined( HB_OS_HPUX ) || defined( HB_OS_AIX ) || defined( HB_OS_MINIX )
   if( strncmp( pszName, "/dev/tty", 8 ) == 0 )
   {
      iLen = 8;
   }
#     elif defined( HB_OS_IRIX ) || defined( HB_OS_DIGITAL_UNIX )
   if( strncmp( pszName, "/dev/ttyf", 9 ) == 0 )
   {
      iLen = 9;
   }
#     elif defined( HB_OS_DARWIN )
   if( strncmp( pszName, "/dev/cuaa", 9 ) == 0 )
   {
      iLen = 9;
   }
#     else /* defined( HB_OS_LINUX ) || defined( HB_OS_CYGWIN ) || ... */
   if( strncmp( pszName, "/dev/ttyS", 9 ) == 0 )
   {
      iLen = 9;
   }
#     endif
   if( iLen > 0 )
   {
      pszName += iLen;
      while( HB_ISDIGIT( *pszName ) )
      {
         iPort = iPort * 10 + ( *pszName++ - '0' );
      }

#     if ! defined( HB_OS_HPUX ) && \
         ! defined( HB_OS_AIX ) && \
         ! defined( HB_OS_IRIX ) && \
         ! defined( HB_OS_DIGITAL_UNIX )
      ++iPort;
#     endif

#     if defined( HB_OS_HPUX )
      if( strcmp( pszName, "p0" ) != 0 )
#     else
      if( *pszName != '\0' )
#     endif
         iPort = 0;
   }
#  endif
#else
   if( pszName[ 0 ] == '\\' && pszName[ 1 ] == '\\' && pszName[ 2 ] == '.'  && pszName[ 3 ] == '\\' )
   {
      pszName += 4;
   }
   if( HB_TOUPPER( pszName[ 0 ] ) == 'C' && HB_TOUPPER( pszName[ 1 ] ) == 'O' && HB_TOUPPER( pszName[ 2 ] ) == 'M' && pszName[ 3 ] >= '1' && pszName[ 3 ] <= '9' )
   {
      pszName += 3;
      while( HB_ISDIGIT( *pszName ) )
      {
         iPort = iPort * ( 10 + *pszName++ - '0' );
      }
      if( *pszName != '\0' )
      {
         iPort = 0;
      }
   }
#endif

   return iPort;
}

static HB_BOOL hb_comPortCmp( const char * pszDevName1, const char * pszDevName2 )
{
#if defined( HB_OS_UNIX )
   return strcmp( pszDevName1, pszDevName2 ) == 0;
#else
#  if defined( HB_OS_WIN )
   if( pszDevName1[ 0 ] == '\\' && pszDevName1[ 1 ] == '\\' && pszDevName1[ 2 ] == '.'  && pszDevName1[ 3 ] == '\\' )
   {
      pszDevName1 += 4;
   }
   if( pszDevName2[ 0 ] == '\\' && pszDevName2[ 1 ] == '\\' && pszDevName2[ 2 ] == '.'  && pszDevName2[ 3 ] == '\\' )
   {
      pszDevName2 += 4;
   }
#  endif
   return hb_stricmp( pszDevName1, pszDevName2 ) == 0;
#endif
}

int hb_comFindPort( const char * pszDevName, HB_BOOL fCreate )
{
   char buffer[ HB_COM_DEV_NAME_MAX ];
   PHB_COM pCom;
   int iPort;

   if( pszDevName == nullptr || *pszDevName == '\0' )
   {
      return 0;
   }

   iPort = hb_comGetPortNum( pszDevName );
   HB_COM_LOCK();
   if( iPort > 0 )
   {
      pCom = hb_comGetPort( iPort, HB_COM_ANY );
      if( pCom == nullptr || ! hb_comPortCmp( hb_comGetNameRaw( pCom, buffer, sizeof(buffer) ), pszDevName ) )
      {
         iPort = 0;
      }
   }

   if( iPort == 0 )
   {
      int iPortFree = 0;

      for( iPort = HB_COM_PORT_MAX; iPort > 0; --iPort )
      {
         pCom = &s_comList[ iPort - 1 ];
         if( pCom->name == nullptr )
         {
            if( iPortFree == 0 && iPort > 16 )
            {
               iPortFree = iPort;
            }
         }
         else if( hb_comPortCmp( pCom->name, pszDevName ) )
         {
            break;
         }
      }
#if defined( HB_OS_UNIX )
      if( iPort == 0 && fCreate && access( pszDevName, F_OK ) == 0 )
#else
      if( iPort == 0 && fCreate )
#endif
      {
         if( iPortFree != 0 )
         {
            iPort = iPortFree;
         }
         else
         {
            for( iPort = HB_COM_PORT_MAX; iPort > 0; --iPort )
            {
               pCom = &s_comList[ iPort - 1 ];
               if( ( pCom->status & HB_COM_OPEN ) == 0 )
               {
                  if( pCom->name )
                  {
                     hb_xfree( pCom->name );
                     pCom->name = nullptr;
                  }
                  break;
               }
            }
         }
         if( iPort != 0 )
         {
            pCom = &s_comList[ iPort - 1 ];
            if( ! hb_comPortCmp( hb_comGetNameRaw( pCom, buffer, sizeof(buffer) ), pszDevName ) )
            {
               pCom->name = hb_strdup( pszDevName );
            }
         }
      }
   }
   HB_COM_UNLOCK();

   return iPort;
}

const char * hb_comGetDevice( int iPort, char * buffer, int size )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ANY );
   const char * pszName = nullptr;

   if( pCom )
   {
      if( buffer && size > 0 )
      {
         pszName = hb_comGetName( pCom, buffer, size );
      }
      else
      {
         pszName = pCom->name;
      }
   }

   return pszName;
}

int hb_comSetDevice( int iPort, const char * szDevName )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ANY );

   if( pCom )
   {
      HB_COM_LOCK();
      if( pCom->name )
      {
         hb_xfree( pCom->name );
      }
      pCom->name = szDevName && *szDevName ? hb_strdup( szDevName ) : nullptr;
      HB_COM_UNLOCK();
   }

   return pCom ? 0 : -1;
}

HB_FHANDLE hb_comGetDeviceHandle( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ANY );
   HB_FHANDLE hFile = FS_ERROR;

   if( pCom )
   {
#if defined( HB_HAS_TERMIOS )
      hFile = pCom->fd;
#elif defined( HB_OS_WIN )
      hFile = ( HB_FHANDLE ) pCom->hComm;
#endif
   }

   return hFile;
}

void hb_comSetError( int iPort, int iError )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ANY );

   if( pCom )
   {
      pCom->error = iError;
   }
}

int hb_comGetError( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ANY );

   return pCom ? pCom->error : HB_COM_ERR_WRONGPORT;
}

int hb_comGetOsError( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ANY );

   return pCom ? pCom->oserr : 0;
}

int hb_comLastNum( void )
{
   int iPort;

   for( iPort = HB_COM_PORT_MAX; iPort; --iPort )
   {
      if( s_comList[ iPort - 1 ].status & HB_COM_ENABLED )
      {
         break;
      }
   }
   return iPort;
}

#if defined( HB_HAS_TERMIOS )

#define HB_COM_IS_EINTR( pCom )  ( ( pCom )->oserr == EINTR )
#define HB_COM_IS_EBADF( pCom )  ( ( pCom )->oserr  == EBADF )
#define HB_COM_GETERROR()        ( errno )

#if defined( HB_OS_LINUX )
#  define HB_HAS_SELECT_TIMER
#endif

static void hb_comSetOsError( PHB_COM pCom, HB_BOOL fError )
{
   pCom->oserr = fError ? HB_COM_GETERROR() : 0;
   switch( pCom->oserr )
   {
      case 0:
         pCom->error = 0;
         break;
      case EIO:
         pCom->error = HB_COM_ERR_IO;
         break;
      case EPIPE:
         pCom->error = HB_COM_ERR_PIPE;
         break;
      case EBUSY:
         pCom->error = HB_COM_ERR_BUSY;
         break;
      case EAGAIN:
         pCom->error = HB_COM_ERR_TIMEOUT;
         break;
      case EACCES:
#if defined( ETXTBSY )
      case ETXTBSY:
#endif
#if defined( EPERM )
      case EPERM:
#endif
         pCom->error = HB_COM_ERR_ACCESS;
         break;
      case ENOTTY:
      case ENOENT:
#if defined( ENOTDIR )
      case ENOTDIR:
#endif
         pCom->error = HB_COM_ERR_NOCOM;
         break;
      default:
         pCom->error = HB_COM_ERR_OTHER;
         break;
   }
}

#if defined( HB_OS_UNIX )
static int hb_comCanRead( PHB_COM pCom, HB_MAXINT timeout )
{
   int iResult;

#if defined( HB_HAS_POLL )
   HB_MAXUINT timer = hb_timerInit( timeout );
   struct pollfd fds;

   fds.fd = pCom->fd;
   fds.events = POLLIN;
   fds.revents = 0;

   do
   {
      int tout = timeout < 0 || timeout > 1000 ? 1000 : static_cast<int>( timeout );
      iResult = poll( &fds, 1, tout );
      hb_comSetOsError( pCom, iResult == -1 );
      if( iResult > 0 && ( fds.revents & POLLIN ) == 0 )
      {
         if( ( fds.revents & ( POLLHUP | POLLNVAL | POLLERR ) ) != 0 )
         {
            if( fds.revents & POLLNVAL )
            {
               pCom->fd = -1;
            }
            hb_comSetComError( pCom, HB_COM_ERR_PIPE );
            iResult = -1;
            break;
         }
         iResult = 0;
      }
      else if( iResult == -1 && HB_COM_IS_EINTR( pCom ) )
      {
         iResult = 0;
      }
   }
   while( iResult == 0 && ( timeout = hb_timerTest( timeout, &timer ) ) != 0 && hb_vmRequestQuery() == 0 );
#else /* ! HB_HAS_POLL */
   struct timeval tv;
   fd_set rfds;
#  if ! defined( HB_HAS_SELECT_TIMER )
   HB_MAXUINT timer = hb_timerInit( timeout );
#  else
   tv.tv_sec = static_cast<long>( timeout / 1000 );
   tv.tv_usec = static_cast<long>( timeout % 1000 ) * 1000;
#  endif

   for( ;; )
   {
      if( timeout < 0 )
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }
#  if ! defined( HB_HAS_SELECT_TIMER )
      else
      {
         tv.tv_sec = static_cast<long>( timeout / 1000 );
         tv.tv_usec = static_cast<long>( timeout % 1000 ) * 1000;
      }
#  endif

      FD_ZERO( &rfds );
      FD_SET( pCom->fd, &rfds );
      iResult = select( static_cast<int>( pCom->fd + 1 ), &rfds, nullptr, nullptr, &tv );
      hb_comSetOsError( pCom, iResult == -1 );
      if( iResult > 0 && ! FD_ISSET( pCom->fd, &rfds ) )
      {
         iResult = 0;
      }
      else if( iResult == -1 && HB_COM_IS_EINTR( pCom ) )
      {
         iResult = 0;
      }
#  if defined( HB_HAS_SELECT_TIMER )
      if( iResult != 0 || timeout >= 0 || hb_vmRequestQuery() != 0 )
      {
         break;
      }
#  else
      if( iResult != 0 || ( timeout = hb_timerTest( timeout, &timer ) ) == 0 || hb_vmRequestQuery() != 0 )
      {
         break;
      }
#  endif
   }
#endif /* ! HB_HAS_POLL */

   return iResult;
}

static int hb_comCanWrite( PHB_COM pCom, HB_MAXINT timeout )
{
   int iResult;

#if defined( HB_HAS_POLL )
   HB_MAXUINT timer = hb_timerInit( timeout );
   struct pollfd fds;

   fds.fd = pCom->fd;
   fds.events = POLLOUT;
   fds.revents = 0;

   do
   {
      int tout = timeout < 0 || timeout > 1000 ? 1000 : static_cast<int>( timeout );
      iResult = poll( &fds, 1, tout );
      hb_comSetOsError( pCom, iResult == -1 );
      if( iResult > 0 && ( fds.revents & POLLOUT ) == 0 )
      {
         if( ( fds.revents & ( POLLHUP | POLLNVAL | POLLERR ) ) != 0 )
         {
            if( fds.revents & POLLNVAL )
            {
               pCom->fd = -1;
            }
            hb_comSetComError( pCom, HB_COM_ERR_PIPE );
            iResult = -1;
            break;
         }
         iResult = 0;
      }
      else if( iResult == -1 && HB_COM_IS_EINTR( pCom ) )
      {
         iResult = 0;
      }
   }
   while( iResult == 0 && ( timeout = hb_timerTest( timeout, &timer ) ) != 0 && hb_vmRequestQuery() == 0 );
#else /* ! HB_HAS_POLL */
   struct timeval tv;
   fd_set wfds;
#  if ! defined( HB_HAS_SELECT_TIMER )
   HB_MAXUINT timer = hb_timerInit( timeout );
#  else
   tv.tv_sec = static_cast<long>( timeout / 1000 );
   tv.tv_usec = static_cast<long>( timeout % 1000 ) * 1000;
#  endif

   for( ;; )
   {
      if( timeout < 0 )
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }
#  if ! defined( HB_HAS_SELECT_TIMER )
      else
      {
         tv.tv_sec = static_cast<long>( timeout / 1000 );
         tv.tv_usec = static_cast<long>( timeout % 1000 ) * 1000;
      }
#  endif

      FD_ZERO( &wfds );
      FD_SET( pCom->fd, &wfds );
      iResult = select( static_cast<int>( pCom->fd + 1 ), nullptr, &wfds, nullptr, &tv );
      hb_comSetOsError( pCom, iResult == -1 );
      if( iResult > 0 && ! FD_ISSET( pCom->fd, &wfds ) )
      {
         iResult = 0;
      }
      else if( iResult == -1 && HB_COM_IS_EINTR( pCom ) )
      {
         iResult = 0;
      }
#  if defined( HB_HAS_SELECT_TIMER )
      if( iResult != 0 || timeout >= 0 || hb_vmRequestQuery() != 0 )
      {
         break;
      }
#  else
      if( iResult != 0 || ( timeout = hb_timerTest( timeout, &timer ) ) == 0 || hb_vmRequestQuery() != 0 )
      {
         break;
      }
#  endif
   }
#endif /* ! HB_HAS_POLL */

   return iResult;
}
#endif

int hb_comInputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = 0;

   if( pCom )
   {
#if defined( TIOCINQ )
      int iResult = ioctl( pCom->fd, TIOCINQ, &iCount );
      if( iResult == -1 )
      {
         iCount = 0;
      }
      hb_comSetOsError( pCom, iResult == -1 );
#elif defined( FIONREAD ) && ! defined( HB_OS_CYGWIN )
      /* Cygwin sys/termios.h explicitly says that "TIOCINQ is
       * utilized instead of FIONREAD which has been occupied for
       * other purposes under CYGWIN", so don't give Cygwin
       * even a chance to hit this code path. */
      int iResult = ioctl( pCom->fd, FIONREAD, &iCount );
      if( iResult == -1 )
      {
         iCount = 0;
      }
      hb_comSetOsError( pCom, iResult == -1 );
#else
      int TODO_TIOCINQ;
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
#endif
   }
   else
   {
      iCount = -1;
   }

   return iCount;
}

int hb_comOutputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = 0;

   if( pCom )
   {
#if defined( TIOCOUTQ )
      int iResult = ioctl( pCom->fd, TIOCOUTQ, &iCount );
      if( iResult == -1 )
      {
         iCount = 0;
      }
      hb_comSetOsError( pCom, iResult == -1 );
#elif defined( FIONWRITE )
      int iResult = ioctl( pCom->fd, FIONWRITE, &iCount );
      if( iResult == -1 )
      {
         iCount = 0;
      }
      hb_comSetOsError( pCom, iResult == -1 );
#else
      int TODO_TIOCOUTQ;
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
#endif
   }
   else
   {
      iCount = -1;
   }

   return iCount;
}

int hb_comFlush( int iPort, int iType )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      switch( iType )
      {
         case HB_COM_IFLUSH:
            iResult = tcflush( pCom->fd, TCIFLUSH );
            hb_comSetOsError( pCom, iResult == -1 );
            break;
         case HB_COM_OFLUSH:
            iResult = tcflush( pCom->fd, TCOFLUSH );
            hb_comSetOsError( pCom, iResult == -1 );
            break;
         case HB_COM_IOFLUSH:
            iResult = tcflush( pCom->fd, TCIOFLUSH );
            hb_comSetOsError( pCom, iResult == -1 );
            break;
         default:
            iResult = -1;
            hb_comSetComError( pCom, HB_COM_ERR_PARAMVALUE );
            break;
      }
   }
   return iResult;
}

/*
   TIOCM_LE          DSR (data set ready/line enable)
   TIOCM_DTR         DTR (data terminal ready)
   TIOCM_RTS         RTS (request to send)
   TIOCM_ST          Secondary TXD (transmit)
   TIOCM_SR          Secondary RXD (receive)
   TIOCM_CTS         CTS (clear to send)
   TIOCM_CAR         DCD (data carrier detect)
   TIOCM_CD           see TIOCM_CAR
   TIOCM_RNG         RNG (ring)
   TIOCM_RI           see TIOCM_RNG
   TIOCM_DSR         DSR (data set ready)

   supported only by few platforms (i.e. newer Linux kernels >= 2.4)
   TIOCM_OUT1        OUT 1 (auxiliary user-designated output 2)
   TIOCM_OUT2        OUT 2 (auxiliary user-designated output 1)
   TIOCM_LOOP        LOOP (loopback mode)
 */

#ifdef HB_OS_LINUX
   /* hack for missing definitions in standard header files */
#  ifndef TIOCM_OUT1
#     define TIOCM_OUT1    0x2000
#  endif
#  ifndef TIOCM_OUT2
#     define TIOCM_OUT2    0x4000
#  endif
#  ifndef TIOCM_LOOP
#     define TIOCM_LOOP    0x8000
#  endif
#endif

int hb_comMCR( int iPort, int * piValue, int iClr, int iSet )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
#if defined( TIOCMGET ) && defined( TIOCMSET )
      int iRawVal, iOldVal;

      iResult = ioctl( pCom->fd, TIOCMGET, &iRawVal );
      if( iResult == 0 )
      {
         if( iRawVal & TIOCM_DTR )
         {
            iValue |= HB_COM_MCR_DTR;
         }
         if( iRawVal & TIOCM_RTS )
         {
            iValue |= HB_COM_MCR_RTS;
         }
#ifdef TIOCM_OUT1
         if( iRawVal & TIOCM_OUT1 )
         {
            iValue |= HB_COM_MCR_OUT1;
         }
#endif
#ifdef TIOCM_OUT2
         if( iRawVal & TIOCM_OUT2 )
         {
            iValue |= HB_COM_MCR_OUT2;
         }
#endif
#ifdef TIOCM_LOOP
         if( iRawVal & TIOCM_LOOP )
         {
            iValue |= HB_COM_MCR_LOOP;
         }
#endif

         iOldVal = iRawVal;

         if( iSet & HB_COM_MCR_DTR )
         {
            iRawVal |= TIOCM_DTR;
         }
         else if( iClr & HB_COM_MCR_DTR )
         {
            iRawVal &= ~TIOCM_DTR;
         }

         if( iSet & HB_COM_MCR_RTS )
         {
            iRawVal |= TIOCM_RTS;
         }
         else if( iClr & HB_COM_MCR_RTS )
         {
            iRawVal &= ~TIOCM_RTS;
         }

#ifdef TIOCM_OUT1
         if( iSet & HB_COM_MCR_OUT1 )
         {
            iRawVal |= TIOCM_OUT1;
         }
         else if( iClr & HB_COM_MCR_OUT1 )
         {
            iRawVal &= ~TIOCM_OUT1;
         }
#endif
#ifdef TIOCM_OUT2
         if( iSet & HB_COM_MCR_OUT2 )
         {
            iRawVal |= TIOCM_OUT2;
         }
         else if( iClr & HB_COM_MCR_OUT2 )
         {
            iRawVal &= ~TIOCM_OUT2;
         }
#endif
#ifdef TIOCM_LOOP
         if( iSet & HB_COM_MCR_LOOP )
         {
            iRawVal |= TIOCM_LOOP;
         }
         else if( iClr & HB_COM_MCR_LOOP )
         {
            iRawVal &= ~TIOCM_LOOP;
         }
#endif

         if( iRawVal != iOldVal )
         {
            iResult = ioctl( pCom->fd, TIOCMSET, &iRawVal );
         }
      }
      hb_comSetOsError( pCom, iResult == -1 );
#else
      int TODO_TIOCMGET_MCR;
      HB_SYMBOL_UNUSED( iClr );
      HB_SYMBOL_UNUSED( iSet );
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
#endif
   }

   if( piValue )
   {
      *piValue = iValue;
   }

   return iResult;
}

int hb_comMSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
#if defined( TIOCMGET ) && defined( TIOCMSET )
      int iRawVal;

      iResult = ioctl( pCom->fd, TIOCMGET, &iRawVal );
      hb_comSetOsError( pCom, iResult == -1 );
      if( iResult == 0 )
      {
         if( iRawVal & TIOCM_CTS )
         {
            iValue |= HB_COM_MSR_CTS;
         }
         if( iRawVal & TIOCM_DSR )
         {
            iValue |= HB_COM_MSR_DSR;
         }
         if( iRawVal & TIOCM_RI )
         {
            iValue |= HB_COM_MSR_RI;
         }
         if( iRawVal & TIOCM_CD )
         {
            iValue |= HB_COM_MSR_DCD;
         }
      }
#else
      int TODO_TIOCMGET_MSR;
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
#endif
   }

   if( piValue )
   {
      *piValue = iValue;
   }

   return iResult;
}

int hb_comLSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
#ifdef TIOCSERGETLSR
      iResult = ioctl( pCom->fd, TIOCSERGETLSR, &iValue );
      hb_comSetOsError( pCom, iResult == -1 );
#else
      /* NOTE: most of systems do not give access to the
       *       Line Status Register (LSR)
       */
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
#endif
   }

   if( piValue )
   {
      *piValue = iValue;
   }

   return iResult;
}

int hb_comSendBreak( int iPort, int iDurationInMilliSecs )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: duration is implementation defined non portable extension
       *       we use 0 what means 'transmit zero-valued bits for at
       *       least 0.25 seconds, and not more that 0.5 seconds'
       */
      HB_SYMBOL_UNUSED( iDurationInMilliSecs );

      hb_vmUnlock();

      iResult = tcsendbreak( pCom->fd, 0 );
      hb_comSetOsError( pCom, iResult == -1 );

      hb_vmLock();
   }
   return iResult;
}

#if defined( CCTS_OFLOW ) && defined( CRTS_IFLOW )
   #define _HB_OCRTSCTS       CCTS_OFLOW
   #define _HB_ICRTSCTS       CRTS_IFLOW
#elif defined( CRTSCTS ) && defined( CRTSXOFF )
   #define _HB_OCRTSCTS       CRTSCTS
   #define _HB_ICRTSCTS       CRTSXOFF
#elif defined( CRTSCTS )
   #define _HB_OCRTSCTS       CRTSCTS
   #define _HB_ICRTSCTS       CRTSCTS
#elif defined( CNEW_RTSCTS )
   #define _HB_OCRTSCTS       CNEW_RTSCTS
   #define _HB_ICRTSCTS       CNEW_RTSCTS
#elif defined( CCTS_OFLOW )
   #define _HB_OCRTSCTS       CCTS_OFLOW
   #define _HB_ICRTSCTS       0
#elif defined( CRTS_IFLOW )
   #define _HB_OCRTSCTS       0
   #define _HB_ICRTSCTS       CCTS_IFLOW
#elif defined( CRTSXOFF )
   #define _HB_OCRTSCTS       0
   #define _HB_ICRTSCTS       CRTSXOFF
#else
   /* if you find compiler which does not support it then please check
    * if such flow control is supported by OS. If yes then check exact
    * value for this switch on given OS and define it only for given
    * compiler and OS
    */
#endif

int hb_comFlowControl( int iPort, int * piFlow, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
      /* NOTE: there is no support for DTR/DSR so we cannot use
       *       DTR/DSR handshake instead of the RTS/CTS handshake
       *       BSD systems support MDMBUF flags which enable output
       *       flow control using CD (Carrier Detect) flag.
       *       In SunOS TIOCSSOFTCAR can be used to control CLOCAL flag.
       *       CLOCAL termios structure c_cflag can be used to enable CD
       *       flow control in most portable way.
       */
      struct termios tio;

      iResult = tcgetattr( pCom->fd, &tio );
      hb_comSetOsError( pCom, iResult == -1 );
      if( iResult == 0 )
      {
         tcflag_t c_cflag = tio.c_cflag;
         tcflag_t c_iflag = tio.c_iflag;

#if defined( _HB_OCRTSCTS )
         if( ( tio.c_cflag & _HB_OCRTSCTS ) == _HB_OCRTSCTS )
         {
            iValue |= HB_COM_FLOW_ORTSCTS;
         }
         if( ( tio.c_cflag & _HB_ICRTSCTS ) == _HB_ICRTSCTS )
         {
            iValue |= HB_COM_FLOW_IRTSCTS;
         }

         if( iFlow >= 0 )
         {
            if( iFlow & HB_COM_FLOW_ORTSCTS )
            {
               tio.c_cflag |= _HB_OCRTSCTS;
            }
            else
            {
               tio.c_cflag &= ~_HB_OCRTSCTS;
            }
            if( iFlow & HB_COM_FLOW_IRTSCTS )
            {
               tio.c_cflag |= _HB_ICRTSCTS;
            }
            else
            {
               tio.c_cflag &= ~_HB_ICRTSCTS;
            }
         }
#else
         {
            int TODO_CRTSCTS;
         }
#endif

         if( ( tio.c_cflag & CLOCAL ) != CLOCAL )
         {
            iValue |= HB_COM_FLOW_DCD;
         }

         if( iFlow >= 0 )
         {
            if( iFlow & HB_COM_FLOW_DCD )
            {
               tio.c_cflag &= ~CLOCAL;
            }
            else
            {
               tio.c_cflag |= CLOCAL;
            }
         }


         if( ( tio.c_iflag & IXON ) == IXON )
         {
            iValue |= HB_COM_FLOW_XON;
         }
         if( ( tio.c_iflag & IXOFF ) == IXOFF )
         {
            iValue |= HB_COM_FLOW_XOFF;
         }

         if( iFlow >= 0 )
         {
            if( iFlow & HB_COM_FLOW_XON )
            {
               tio.c_iflag |= IXON;
            }
            else
            {
               tio.c_iflag &= ~IXON;
            }
            if( iFlow & HB_COM_FLOW_XOFF )
            {
               tio.c_iflag |= IXOFF;
            }
            else
            {
               tio.c_iflag &= ~IXOFF;
            }
         }

         if( c_cflag != tio.c_cflag || c_iflag != tio.c_iflag )
         {
            iResult = tcsetattr( pCom->fd, TCSANOW, &tio );
            hb_comSetOsError( pCom, iResult == -1 );
         }
      }
   }

   if( piFlow )
   {
      *piFlow = iValue;
   }

   return iResult;
}

int hb_comFlowSet( int iPort, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: HB_COM_FL_SOFT is ignored, we assume that user chose
       *       correct hardware/software flow control type which is
       *       the same as set in terminal device parameters
       */
      if( iFlow & HB_COM_FL_OON )
      {
         iResult = tcflow( pCom->fd, TCOON );
      }
      else if( iFlow & HB_COM_FL_OOFF )
      {
         iResult = tcflow( pCom->fd, TCOOFF );
      }
      else
      {
         iResult = 0;
      }

      if( iFlow & HB_COM_FL_ION )
      {
         if( tcflow( pCom->fd, TCION ) == -1 )
         {
            iResult = -1;
         }
      }
      else if( iFlow & HB_COM_FL_IOFF )
      {
         if( tcflow( pCom->fd, TCIOFF ) == -1 )
         {
            iResult = -1;
         }
      }
      hb_comSetOsError( pCom, iResult == -1 );
   }

   return iResult;
}

int hb_comFlowChars( int iPort, int iXONchar, int iXOFFchar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      iResult = 0;
      if( iXONchar >= 0 || iXOFFchar >= 0 )
      {
         struct termios tio;

         iResult = tcgetattr( pCom->fd, &tio );
         if( iResult == 0 )
         {
            if( iXONchar >= 0 )
            {
               tio.c_cc[ VSTART ] = iXONchar;
            }
            if( iXOFFchar >= 0 )
            {
               tio.c_cc[ VSTOP ] = iXOFFchar;
            }
            iResult = tcsetattr( pCom->fd, TCSANOW, &tio );
         }
      }
      hb_comSetOsError( pCom, iResult == -1 );
   }
   return iResult;
}

#if ! defined( _POSIX_VDISABLE )
#  define _POSIX_VDISABLE  '\0'
#endif

int hb_comDiscardChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
#if defined( VDISCARD ) && defined( IEXTEN )
      struct termios tio;

      iResult = tcgetattr( pCom->fd, &tio );
      hb_comSetOsError( pCom, iResult == -1 );
      if( iResult == 0 )
      {
         if( ( tio.c_lflag & IEXTEN ) != 0 && tio.c_cc[ VDISCARD ] != _POSIX_VDISABLE )
         {
            iResult = 1;
         }

         if( iChar == -1 ? iResult != 0 : ( iResult == 0 || tio.c_cc[ VDISCARD ] != iChar ) )
         {
            if( iChar == -1 )
            {
               tio.c_lflag &= ~IEXTEN;
               tio.c_cc[ VDISCARD ] = _POSIX_VDISABLE;
            }
            else
            {
               tio.c_lflag |= IEXTEN;
               tio.c_cc[ VDISCARD ] = iChar;
#if defined( VLNEXT )
               tio.c_cc[ VLNEXT ] = _POSIX_VDISABLE;
#endif
            }
            if( tcsetattr( pCom->fd, TCSANOW, &tio ) == -1 )
            {
               hb_comSetOsError( pCom, HB_TRUE );
               iResult = -1;
            }
         }
      }
#else
      int TODO_VDISCARD;
      HB_SYMBOL_UNUSED( iChar );
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
#endif
   }

   return iResult;
}

int hb_comErrorChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: there is no support for setting user defined error character
       */

      HB_SYMBOL_UNUSED( iChar );
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return iResult;
}

int hb_comOutputState( int iPort )
{
   /* NOTE: checking HB_COM_TX_* output flow states is unsupported */
   int iResult = hb_comOutputCount( iPort );

   if( iResult == 0 )
   {
      iResult = HB_COM_TX_EMPTY;
   }
   else if( iResult > 0 )
   {
      iResult = 0;
   }

   return iResult;
}

int hb_comInputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: checking HB_COM_RX_* input flow states is unsupported */
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return iResult;
}

long hb_comSend( int iPort, const void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lSent = -1;

   if( pCom )
   {
      hb_vmUnlock();

#if defined( HB_OS_UNIX )
      if( timeout >= 0 )
      {
         lSent = hb_comCanWrite( pCom, timeout );
         if( lSent == 0 )
         {
            hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
            lSent = -1;
         }
      }
      else
      {
         lSent = 0;
      }
#else
      /* NOTE: write timeout is unsupported */
      HB_SYMBOL_UNUSED( timeout );
      lSent = 0;
#endif

      if( lSent >= 0 )
      {
         do
         {
            lSent = write( pCom->fd, data, len );
            hb_comSetOsError( pCom, lSent == -1 );
         }
         while( lSent == -1 && HB_COM_IS_EINTR( pCom ) && hb_vmRequestQuery() == 0 );
      }
      hb_vmLock();
   }

   return lSent;
}

long hb_comRecv( int iPort, void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lReceived = -1;

   if( pCom )
   {
      hb_vmUnlock();

#if defined( HB_OS_UNIX )
      if( timeout >= 0 )
      {
         lReceived = hb_comCanRead( pCom, timeout );
         if( lReceived == 0 )
         {
            hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
            lReceived = -1;
         }
      }
      else
      {
         lReceived = 0;
      }
#else
      if( timeout != pCom->rdtimeout )
      {
         /* TODO: implement timeout settings
          *          tio.c_cc[ VTIME ] = ( timeout + 50 ) / 100;
          *          tio.c_cc[ VMIN ]  = 0;
          *       in DJGPP builds
          */
      }
      lReceived = 0;
#endif

      if( lReceived >= 0 )
      {
         do
         {
            lReceived = read( pCom->fd, static_cast<char*>( data ), len );
            hb_comSetOsError( pCom, lReceived == -1 );
         }
         while( lReceived == -1 && HB_COM_IS_EINTR( pCom ) && hb_vmRequestQuery() == 0 );
      }
      hb_vmLock();
   }

   return lReceived;
}

int hb_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      struct termios tio;

      iResult = tcgetattr( pCom->fd, &tio );
      hb_comSetOsError( pCom, iResult == -1 );
      if( iResult == 0 )
      {
#if defined( cfmakeraw ) || defined( HB_OS_LINUX )
         /* Raw input from device */
         cfmakeraw( &tio );
#endif
         tio.c_iflag &= ~( IGNBRK | IGNPAR | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON | IXANY | IXOFF );
         tio.c_oflag &= ~OPOST;
         tio.c_lflag &= ~( ECHO | ECHONL | ICANON | ISIG | IEXTEN );
         tio.c_cflag &= ~( CSIZE | PARENB );
         /* Enable the receiver and set local mode... */
         tio.c_cflag |= ( CLOCAL | CREAD );

         tio.c_cc[ VTIME ] = 0;  /* inter-character timer in 1/10 sec. */
#if 0
         tio.c_cc[ VMIN ]  = 0;  /* minimum number of characters for read */
#else
         /* workaround for bug in some Linux kernels (i.e. 3.13.0-64-generic
            *buntu) in which select() unconditionally accepts stdin for
            reading if c_cc[ VMIN ] = 0 [druzus] */
         tio.c_cc[ VMIN ] = 1;
#endif

         if( iBaud )
         {
            switch( iBaud )
            {
               case        0: iBaud =      B0; break;
               case       50: iBaud =     B50; break;
               case       75: iBaud =     B75; break;
               case      110: iBaud =    B110; break;
               case      150: iBaud =    B150; break;
               case      200: iBaud =    B200; break;
               case      300: iBaud =    B300; break;
               case      600: iBaud =    B600; break;
               case     1200: iBaud =   B1200; break;
               case     1800: iBaud =   B1800; break;
               case     2400: iBaud =   B2400; break;
               case     4800: iBaud =   B4800; break;
               case     9600: iBaud =   B9600; break;
               case    19200: iBaud =  B19200; break;
               case    38400: iBaud =  B38400; break;
#ifdef B57600
               case    57600: iBaud =  B57600; break;
#endif
#ifdef B115200
               case   115200: iBaud = B115200; break;
#endif
#ifdef B230400
               case   230400: iBaud = B230400; break;
#endif
#ifdef B460800
               case   460800: iBaud = B460800; break;
#endif
#ifdef B500000
               case   500000: iBaud = B500000; break;
#endif
#ifdef B576000
               case   576000: iBaud = B576000; break;
#endif
#ifdef B921600
               case   921600: iBaud = B921600; break;
#endif
#ifdef B1000000
               case  1000000: iBaud = B1000000; break;
#endif
               default:
                  iResult = -1;
            }
         }
         switch( iParity )
         {
            case 0:
            case 'N':
            case 'n':
               tio.c_cflag &= ~( PARENB | PARODD );
               tio.c_iflag &= ~INPCK;
               break;
            case 'E':
            case 'e':
               tio.c_cflag |= PARENB;
               tio.c_cflag &= ~PARODD;
               tio.c_iflag |= INPCK;
               break;
            case 'O':
            case 'o':
               tio.c_cflag |= PARENB | PARODD;
               tio.c_iflag |= INPCK;
               break;
#if defined( CMSPAR )
            case 'S':
            case 's':
               tio.c_cflag |= CMSPAR | PARENB;
               tio.c_cflag &= ~PARODD;
               tio.c_iflag |= INPCK;
               break;
            case 'M':
            case 'm':
               tio.c_cflag |= CMSPAR | PARENB | PARODD;
               tio.c_iflag |= INPCK;
               break;
#endif
            default:
               iResult = -1;
         }
         switch( iSize )
         {
            case 0:
            case 8: tio.c_cflag |= CS8; break;
            case 7: tio.c_cflag |= CS7; break;
            case 6: tio.c_cflag |= CS6; break;
            case 5: tio.c_cflag |= CS5; break;
            default:
               iResult = -1;
         }
         switch( iStop )
         {
            case 0:
            case 1: tio.c_cflag &= ~CSTOPB; break;
            case 2: tio.c_cflag |= CSTOPB; break;
            default:
               iResult = -1;
         }

         if( iResult == 0 )
         {
            if( iBaud )
            {
               cfsetispeed( &tio, iBaud );
               cfsetospeed( &tio, iBaud );
            }

            iResult = tcsetattr( pCom->fd, TCSAFLUSH, &tio );
#if ! defined( HB_OS_UNIX )
            if( iResult == 0 )
            {
               pCom->rdtimeout = 0;
            }
#endif
            hb_comSetOsError( pCom, iResult == -1 );
         }
         else
         {
            hb_comSetComError( pCom, HB_COM_ERR_PARAMVALUE );
         }
      }
   }

   return iResult;
}

int hb_comClose( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      hb_vmUnlock();
#if defined( TIOCNXCL )
      ioctl( pCom->fd, TIOCNXCL, 0 );
#endif
      do
      {
         iResult = close( pCom->fd );
         hb_comSetOsError( pCom, iResult == -1 );
      }
      while( iResult == -1 && HB_COM_IS_EINTR( pCom ) && hb_vmRequestQuery() == 0 );

      if( iResult != -1 || HB_COM_IS_EBADF( pCom ) )
      {
         pCom->fd = ( HB_FHANDLE ) FS_ERROR;
         pCom->status &= ~HB_COM_OPEN;
      }
      hb_vmLock();
   }

   return iResult;
}

int hb_comOpen( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ENABLED );
   int iResult = -1;

   if( pCom )
   {
      if( pCom->status & HB_COM_OPEN )
      {
         hb_comSetComError( pCom, HB_COM_ERR_ALREADYOPEN );
      }
      else
      {
         char buffer[ HB_COM_DEV_NAME_MAX ];
         const char * name = hb_comGetName( pCom, buffer, sizeof(buffer) );

         hb_vmUnlock();

         pCom->fd = open( name, O_RDWR | O_NOCTTY );
         if( pCom->fd != -1 )
         {
#if defined( TIOCEXCL ) /* TIOCNXCL */
            iResult = ioctl( pCom->fd, TIOCEXCL, 0 );
            if( iResult != 0 )
            {
               close( pCom->fd );
               pCom->fd = -1;
               hb_comSetComError( pCom, HB_COM_ERR_BUSY );
            }
            else
#else
            iResult = 0;
#endif
            pCom->status |= HB_COM_OPEN;
         }
         hb_comSetOsError( pCom, iResult == -1 );

         hb_vmLock();
      }
   }

   return iResult;
}

/* end of HB_HAS_TERMIOS */

#elif defined( HB_OS_WIN )

static void hb_comSetOsError( PHB_COM pCom, BOOL fError )
{
   pCom->oserr = fError ? GetLastError() : 0;

   switch( pCom->oserr )
   {
      case 0:
         pCom->error = 0;
         break;
      case ERROR_TIMEOUT:
         pCom->error = HB_COM_ERR_TIMEOUT;
         break;
      case ERROR_ACCESS_DENIED:
      case ERROR_SHARING_VIOLATION:
         pCom->error = HB_COM_ERR_BUSY;
         break;
      case ERROR_FILE_NOT_FOUND:
      case ERROR_PATH_NOT_FOUND:
         pCom->error = HB_COM_ERR_NOCOM;
         break;
      default:
         pCom->error = HB_COM_ERR_OTHER;
         break;
   }
}

int hb_comInputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = 0;

   if( pCom )
   {
      COMSTAT comStat;

      if( ClearCommError( pCom->hComm, nullptr, &comStat ) )
      {
         iCount = comStat.cbInQue;
         hb_comSetOsError( pCom, HB_FALSE );
      }
      else
      {
         hb_comSetOsError( pCom, HB_TRUE );
      }
   }
   else
   {
      iCount = -1;
   }

   return iCount;
}

int hb_comOutputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = 0;

   if( pCom )
   {
      COMSTAT comStat;

      if( ClearCommError( pCom->hComm, nullptr, &comStat ) )
      {
         iCount = comStat.cbOutQue;
         hb_comSetOsError( pCom, HB_FALSE );
      }
      else
      {
         hb_comSetOsError( pCom, HB_TRUE );
      }
   }
   else
   {
      iCount = -1;
   }

   return iCount;
}

int hb_comFlush( int iPort, int iType )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      switch( iType )
      {
         case HB_COM_IFLUSH:
            fResult = PurgeComm( pCom->hComm, PURGE_RXCLEAR );
            hb_comSetOsError( pCom, ! fResult );
            break;
         case HB_COM_OFLUSH:
            fResult = PurgeComm( pCom->hComm, PURGE_TXCLEAR );
            hb_comSetOsError( pCom, ! fResult );
            break;
         case HB_COM_IOFLUSH:
            fResult = PurgeComm( pCom->hComm, PURGE_TXCLEAR | PURGE_RXCLEAR );
            hb_comSetOsError( pCom, ! fResult );
            break;
         default:
            hb_comSetComError( pCom, HB_COM_ERR_PARAMVALUE );
            break;
      }
   }
   return fResult ? 0 : -1;
}

int hb_comMCR( int iPort, int * piValue, int iClr, int iSet )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      if( iSet & HB_COM_MCR_DTR )
      {
         fResult = EscapeCommFunction( pCom->hComm, SETDTR );
      }
      else if( iClr & HB_COM_MCR_DTR )
      {
         fResult = EscapeCommFunction( pCom->hComm, CLRDTR );
      }

      if( iSet & HB_COM_MCR_RTS )
      {
         fResult = EscapeCommFunction( pCom->hComm, SETRTS );
      }
      else if( iClr & HB_COM_MCR_RTS )
      {
         fResult = EscapeCommFunction( pCom->hComm, CLRRTS );
      }

      /* MCR_OUT1, MCR_OUT2, MCR_LOOP and reading current state
       * is unsupported
       */
      hb_comSetOsError( pCom, ! fResult );
   }

   if( piValue )
   {
      *piValue = iValue;
   }

   return fResult ? 0 : -1;
}

int hb_comMSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      DWORD dwModemStat = 0;

      fResult = GetCommModemStatus( pCom->hComm, &dwModemStat );
      if( fResult )
      {
         if( dwModemStat & MS_CTS_ON )
         {
            iValue |= HB_COM_MSR_CTS;
         }
         if( dwModemStat & MS_DSR_ON )
         {
            iValue |= HB_COM_MSR_DSR;
         }
         if( dwModemStat & MS_RING_ON )
         {
            iValue |= HB_COM_MSR_RI;
         }
         if( dwModemStat & MS_RLSD_ON )
         {
            iValue |= HB_COM_MSR_DCD;
         }

         /* MSR_DELTA_CTS, MSR_DELTA_DSR, MSR_TERI, MSR_DELTA_DCD
          * are unsupported
          */

      }
      hb_comSetOsError( pCom, ! fResult );
   }

   if( piValue )
   {
      *piValue = iValue;
   }

   return fResult ? 0 : -1;
}

int hb_comLSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      DWORD dwErrors = 0;

      fResult = ClearCommError( pCom->hComm, &dwErrors, nullptr );
      if( fResult )
      {
         if( dwErrors & CE_BREAK )
         {
            iValue |= HB_COM_LSR_BREAK;
         }
         if( dwErrors & CE_FRAME )
         {
            iValue |= HB_COM_LSR_FRAMING_ERR;
         }
         if( dwErrors & CE_OVERRUN )
         {
            iValue |= HB_COM_LSR_OVERRUN_ERR;
         }
         if( dwErrors & CE_RXPARITY )
         {
            iValue |= HB_COM_LSR_PARITY_ERR;
         }

         /* LSR_DATA_READY, LSR_TRANS_HOLD_EMPTY, LSR_TRANS_EMPTY
          * are unsupported
          */
      }
      hb_comSetOsError( pCom, ! fResult );
   }

   if( piValue )
   {
      *piValue = iValue;
   }

   return fResult ? 0 : -1;
}

int hb_comSendBreak( int iPort, int iDurationInMilliSecs )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      hb_vmUnlock();

      fResult = SetCommBreak( pCom->hComm );
      if( fResult )
      {
         Sleep( iDurationInMilliSecs );
         fResult = ClearCommBreak( pCom->hComm );
      }
      hb_comSetOsError( pCom, ! fResult );

      hb_vmLock();
   }
   return fResult ? 0 : -1;
}

int hb_comFlowControl( int iPort, int * piFlow, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      DCB dcb;

      dcb.DCBlength = sizeof(DCB);
      fResult = GetCommState( pCom->hComm, &dcb );
      if( fResult )
      {
         if( dcb.fRtsControl == RTS_CONTROL_HANDSHAKE )
         {
            iValue |= HB_COM_FLOW_IRTSCTS;
         }
         if( dcb.fOutxCtsFlow )
         {
            iValue |= HB_COM_FLOW_ORTSCTS;
         }

         if( dcb.fDtrControl == DTR_CONTROL_HANDSHAKE )
         {
            iValue |= HB_COM_FLOW_IDTRDSR;
         }
         if( dcb.fOutxDsrFlow )
         {
            iValue |= HB_COM_FLOW_ODTRDSR;
         }

         if( dcb.fInX )
         {
            iValue |= HB_COM_FLOW_XOFF;
         }
         if( dcb.fOutX )
         {
            iValue |= HB_COM_FLOW_XON;
         }

         if( iFlow >= 0 )
         {
            if( iFlow & HB_COM_FLOW_IRTSCTS )
            {
               dcb.fRtsControl = RTS_CONTROL_HANDSHAKE;
            }
            else if( dcb.fRtsControl == RTS_CONTROL_HANDSHAKE )
            {
               dcb.fRtsControl = RTS_CONTROL_ENABLE;
            }
            dcb.fOutxCtsFlow = ( iFlow & HB_COM_FLOW_ORTSCTS ) != 0;

            if( iFlow & HB_COM_FLOW_IDTRDSR )
            {
               dcb.fDtrControl = DTR_CONTROL_HANDSHAKE;
            }
            else if( dcb.fDtrControl == DTR_CONTROL_HANDSHAKE )
            {
               dcb.fDtrControl = DTR_CONTROL_ENABLE;
            }
            dcb.fOutxDsrFlow = ( iFlow & HB_COM_FLOW_ODTRDSR ) != 0;

            dcb.fInX = ( iFlow & HB_COM_FLOW_XOFF ) != 0;
            dcb.fOutX = ( iFlow & HB_COM_FLOW_XON ) != 0;

            fResult = SetCommState( pCom->hComm, &dcb );
         }
      }
      hb_comSetOsError( pCom, ! fResult );
   }

   if( piFlow )
   {
      *piFlow = iValue;
   }

   return fResult ? 0 : -1;
}

int hb_comFlowSet( int iPort, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE, fNotSup = FALSE;

   if( pCom )
   {
      if( iFlow & HB_COM_FL_SOFT )
      {
         if( iFlow & HB_COM_FL_OOFF )
         {
            fResult = EscapeCommFunction( pCom->hComm, SETXOFF );
         }
         else if( iFlow & HB_COM_FL_OON )
         {
            fResult = EscapeCommFunction( pCom->hComm, SETXON );
         }
         else
         {
            fNotSup = TRUE;
         }
         hb_comSetOsError( pCom, ! fResult );
      }
      else if( iFlow & HB_COM_FL_RTSCTS )
      {
         if( iFlow & HB_COM_FL_IOFF )
         {
            fResult = EscapeCommFunction( pCom->hComm, CLRRTS );
         }
         else if( iFlow & HB_COM_FL_ION )
         {
            fResult = EscapeCommFunction( pCom->hComm, SETRTS );
         }
         else
         {
            fNotSup = TRUE;
         }
      }
      else if( iFlow & HB_COM_FL_DTRDSR )
      {
         if( iFlow & HB_COM_FL_IOFF )
         {
            fResult = EscapeCommFunction( pCom->hComm, CLRDTR );
         }
         else if( iFlow & HB_COM_FL_ION )
         {
            fResult = EscapeCommFunction( pCom->hComm, SETDTR );
         }
         else
         {
            fNotSup = TRUE;
         }
      }
      else
      {
         fNotSup = TRUE;
      }

      if( fNotSup )
      {
         hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
      }
      else
      {
         hb_comSetOsError( pCom, ! fResult );
      }
   }

   return fResult ? 0 : -1;
}

int hb_comFlowChars( int iPort, int iXONchar, int iXOFFchar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      fResult = TRUE;
      if( iXONchar >= 0 || iXOFFchar >= 0 )
      {
         DCB dcb;

         dcb.DCBlength = sizeof(DCB);
         fResult = GetCommState( pCom->hComm, &dcb );
         if( fResult )
         {
            if( iXONchar >= 0 )
            {
               dcb.XonChar = static_cast<char>( iXONchar );
            }
            if( iXOFFchar >= 0 )
            {
               dcb.XoffChar = static_cast<char>( iXOFFchar );
            }
            fResult = SetCommState( pCom->hComm, &dcb );
         }
      }
      hb_comSetOsError( pCom, ! fResult );
   }
   return fResult ? 0 : -1;
}

int hb_comDiscardChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      /* NOTE: there is no support for setting user defined character
       * discarding input buffer
       */
      HB_SYMBOL_UNUSED( iChar );
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return iResult;
}

int hb_comErrorChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      DCB dcb;

      dcb.DCBlength = sizeof(DCB);
      fResult = GetCommState( pCom->hComm, &dcb );
      if( fResult )
      {
         if( iChar >= 0 )
         {
            dcb.fErrorChar = TRUE;
            dcb.ErrorChar = static_cast<char>( iChar );
         }
         else
         {
            dcb.fErrorChar = FALSE;
         }
         fResult = SetCommState( pCom->hComm, &dcb );
      }
      hb_comSetOsError( pCom, ! fResult );
   }
   return fResult ? 0 : -1;
}

int hb_comOutputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      COMSTAT comStat;

      fResult = ClearCommError( pCom->hComm, nullptr, &comStat );
      if( fResult )
      {
         /* NOTE: HB_COM_TX_RFLUSH is unsupported */

         if( comStat.fCtsHold )
         {
            iValue |= HB_COM_TX_CTS;
         }
         if( comStat.fDsrHold )
         {
            iValue |= HB_COM_TX_DSR;
         }
         if( comStat.fRlsdHold )
         {
            iValue |= HB_COM_TX_DCD;
         }
         if( comStat.fXoffHold )
         {
            iValue |= HB_COM_TX_XOFF;
         }
         if( comStat.cbOutQue == 0 )
         {
            iValue |= HB_COM_TX_EMPTY;
         }
      }
      hb_comSetOsError( pCom, ! fResult );
   }

   return fResult ? iValue : -1;
}

int hb_comInputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;
   int iValue = 0;

   if( pCom )
   {
      COMSTAT comStat;

      fResult = ClearCommError( pCom->hComm, nullptr, &comStat );
      if( fResult )
      {
         if( comStat.fXoffSent )
         {
            iValue |= HB_COM_RX_XOFF;
         }
      }
      hb_comSetOsError( pCom, ! fResult );
   }

   return fResult ? iValue : -1;
}

static BOOL hb_comSetTimeouts( PHB_COM pCom, HB_MAXINT rdtimeout, HB_MAXINT wrtimeout )
{
   COMMTIMEOUTS timeouts;
   BOOL fResult;

   if( rdtimeout == 0 )
   {
      timeouts.ReadIntervalTimeout = MAXDWORD;
      timeouts.ReadTotalTimeoutMultiplier = 0;
      timeouts.ReadTotalTimeoutConstant = 0;
   }
   else
   {
      timeouts.ReadIntervalTimeout = MAXDWORD;
      timeouts.ReadTotalTimeoutMultiplier = MAXDWORD;
      timeouts.ReadTotalTimeoutConstant = static_cast<DWORD>( rdtimeout );
   }
   timeouts.WriteTotalTimeoutMultiplier = 0;
   timeouts.WriteTotalTimeoutConstant = static_cast<DWORD>( HB_MAX( wrtimeout, 1 ) );

   fResult = SetCommTimeouts( pCom->hComm, &timeouts );
   if( fResult )
   {
      pCom->rdtimeout = rdtimeout;
      pCom->wrtimeout = wrtimeout;
   }

   return fResult;
}

long hb_comSend( int iPort, const void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lSent = -1;

   if( pCom )
   {
      hb_vmUnlock();

      if( timeout < 0 )
      {
         timeout = 0;
      }

      if( pCom->wrtimeout == timeout || hb_comSetTimeouts( pCom, pCom->rdtimeout, timeout ) )
      {
         DWORD dwWritten = 0;
         BOOL fResult;

         fResult = WriteFile( pCom->hComm, data, static_cast<DWORD>( len ), &dwWritten, nullptr );
         lSent = fResult ? static_cast<long>( dwWritten ) : -1;
         if( lSent == 0 )
         {
            hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
            lSent = -1;
         }
         else
         {
            hb_comSetOsError( pCom, ! fResult );
         }
      }
      else
      {
         hb_comSetOsError( pCom, HB_TRUE );
      }

      hb_vmLock();
   }

   return lSent;
}

long hb_comRecv( int iPort, void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lReceived = -1;

   if( pCom )
   {
      hb_vmUnlock();

      if( timeout < 0 )
      {
         timeout = 0;
      }

      if( pCom->rdtimeout == timeout || hb_comSetTimeouts( pCom, timeout, pCom->wrtimeout ) )
      {
         DWORD dwRead = 0;
         BOOL fResult;

         fResult = ReadFile( pCom->hComm, data, static_cast<DWORD>( len ), &dwRead, nullptr );
         lReceived = fResult ? static_cast<long>( dwRead ) : -1;
         if( lReceived == 0 )
         {
            hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
            lReceived = -1;
         }
         else
         {
            hb_comSetOsError( pCom, ! fResult );
         }
      }
      else
      {
         hb_comSetOsError( pCom, HB_TRUE );
      }

      hb_vmLock();
   }

   return lReceived;
}

int hb_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      DCB dcb;

      dcb.DCBlength = sizeof(DCB);
      fResult = GetCommState( pCom->hComm, &dcb );
      if( fResult )
      {
         switch( iParity )
         {
            case 0:
            case 'N':
            case 'n':
               iParity = NOPARITY;
               break;
            case 'E':
            case 'e':
               iParity = EVENPARITY;
               break;
            case 'O':
            case 'o':
               iParity = ODDPARITY;
               break;
            case 'S':
            case 's':
               iParity = SPACEPARITY;
               break;
            case 'M':
            case 'm':
               iParity = MARKPARITY;
               break;
            default:
               fResult = FALSE;
         }
         switch( iSize )
         {
            case 0:
               iSize = 8;
            case 8:
            case 7:
            case 6:
            case 5:
               break;
            default:
               fResult = FALSE;
         }
         switch( iStop )
         {
            case 0:
            case 1: iStop = ONESTOPBIT; break;
            case 2: iStop = TWOSTOPBITS; break;
            default:
               fResult = FALSE;
         }
         if( fResult )
         {
            if( iBaud )
            {
               dcb.BaudRate = static_cast<DWORD>( iBaud );
            }
            dcb.fBinary = 1;
            dcb.fParity = 0;
            dcb.fOutxCtsFlow = 0;
            dcb.fOutxDsrFlow = 0;
            dcb.fDtrControl = DTR_CONTROL_ENABLE;
            dcb.fDsrSensitivity = 0;
            dcb.fTXContinueOnXoff = 1;
            dcb.fOutX = 0;
            dcb.fInX = 0;
            dcb.fErrorChar = 0;
            dcb.fNull = 0;
            dcb.fRtsControl = RTS_CONTROL_ENABLE;
            dcb.fAbortOnError = 0;
          /*dcb.XonLim*/
          /*dcb.XoffLim*/
            dcb.ByteSize = static_cast<BYTE>( iSize );
            dcb.Parity = static_cast<BYTE>( iParity );
            dcb.StopBits = static_cast<BYTE>( iStop );
          /*dcb.XonChar*/
          /*dcb.XoffChar*/
            dcb.ErrorChar = '?';
          /*dcb.EofChar*/
          /*dcb.EvtChar*/

            fResult = SetCommState( pCom->hComm, &dcb );
            if( fResult )
            {
               fResult = hb_comSetTimeouts( pCom, 0, 0 );
            }

            hb_comSetOsError( pCom, ! fResult );
         }
         else
         {
            hb_comSetComError( pCom, HB_COM_ERR_PARAMVALUE );
         }
      }
      else
      {
         hb_comSetOsError( pCom, ! fResult );
      }
   }

   return fResult ? 0 : -1;
}

int hb_comClose( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   BOOL fResult = FALSE;

   if( pCom )
   {
      hb_vmUnlock();
      #if 0
      FlushFileBuffers( pCom->hComm );
      #endif
      fResult = CloseHandle( pCom->hComm );
      pCom->hComm = INVALID_HANDLE_VALUE;
      pCom->status &= ~HB_COM_OPEN;
      hb_comSetOsError( pCom, ! fResult );
      hb_vmLock();
   }

   return fResult ? 0 : -1;
}

int hb_comOpen( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ENABLED );
   BOOL fResult = FALSE;

   if( pCom )
   {
      if( pCom->status & HB_COM_OPEN )
      {
         hb_comSetComError( pCom, HB_COM_ERR_ALREADYOPEN );
      }
      else
      {
         char buffer[ HB_COM_DEV_NAME_MAX ];
         const char * szName = hb_comGetName( pCom, buffer, sizeof(buffer) );
         LPCTSTR lpName;
         LPTSTR lpFree;

         lpName = HB_FSNAMECONV( szName, &lpFree );

         hb_vmUnlock();

         pCom->hComm = CreateFile( lpName, GENERIC_READ | GENERIC_WRITE, 0, nullptr, OPEN_EXISTING, FILE_FLAG_NO_BUFFERING, nullptr );
         if( pCom->hComm != INVALID_HANDLE_VALUE )
         {
            fResult = TRUE;
            pCom->status |= HB_COM_OPEN;
         }
         hb_comSetOsError( pCom, ! fResult );

         hb_vmLock();

         if( lpFree )
         {
            hb_xfree( lpFree );
         }
      }
   }

   return fResult ? 0 : -1;
}

/* end of HB_OS_WIN */

#elif defined( HB_HAS_DOSSRL )

static void hb_comSetOsError( PHB_COM pCom, int iError )
{
   pCom->oserr = iError;

   switch( iError )
   {
      case SER_SUCCESS:                   /* Function completed successfully */
         pCom->error = 0;
         break;
      case SER_ERR_NOT_OPEN:              /* The specified COM port is not opened */
         pCom->error = HB_COM_ERR_CLOSED;
         break;
      case SER_ERR_ALREADY_OPEN:          /* The specified COM port is already opened */
         pCom->error = HB_COM_ERR_ALREADYOPEN;
         break;
      case SER_ERR_NO_UART:               /* Could not find a UART for this COM port */
         pCom->error = HB_COM_ERR_NOCOM;
         break;
      case SER_ERR_INVALID_COMPORT:       /* User specified an invalid COM port */
         pCom->error = HB_COM_ERR_WRONGPORT;
         break;
      case SER_ERR_INVALID_BPS:           /* User specified an invalid BPS rate */
      case SER_ERR_INVALID_DATA_BITS:     /* User specified an invalid number of data bits */
      case SER_ERR_INVALID_PARITY:        /* User specified an invalid parity type */
      case SER_ERR_INVALID_STOP_BITS:     /* User specified an invalid number of stop bits */
      case SER_ERR_INVALID_HANDSHAKING:   /* User specified an invalid handshaking type */
      case SER_ERR_INVALID_FIFO_THRESHOLD:/* User specified an invalid fifo threshold value */
      case SER_ERR_NULL_PTR:              /* User specified a buffer address that was nullptr */
         pCom->error = HB_COM_ERR_PARAMVALUE;
         break;
      case SER_ERR_INVALID_BASE:          /* User specified an invalid base address */
      case SER_ERR_INVALID_IRQ:           /* User specified an invalid IRQ number */
         pCom->error = HB_COM_ERR_PARAMVALUE;
         break;
      case SER_ERR_IRQ_NOT_FOUND:         /* Could not find an IRQ for the specified COM port */
      case SER_ERR_LOCK_MEM:              /* Could not lock memory in DPMI mode */
      case SER_ERR_UNKNOWN:               /* An unknown error occurred */
      default:
         pCom->error = iError < 0 ? HB_COM_ERR_OTHER : 0;
   }
}

int hb_comInputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = -1;

   if( pCom )
   {
      iCount = serial_get_rx_buffered( iPort - 1 );
      hb_comSetOsError( pCom, iCount );
      if( iCount < 0 )
      {
         iCount = -1;
      }
   }

   return iCount;
}

int hb_comOutputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = -1;

   if( pCom )
   {
      iCount = serial_get_tx_buffered( iPort - 1 );
      hb_comSetOsError( pCom, iCount );
      if( iCount < 0 )
      {
         iCount = -1;
      }
   }

   return iCount;
}

int hb_comFlush( int iPort, int iType )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      switch( iType )
      {
         case HB_COM_IFLUSH:
         case HB_COM_IOFLUSH:
            iResult = serial_clear_rx_buffer( iPort - 1 );
            if( iType == HB_COM_IFLUSH || iResult != SER_SUCCESS )
            {
               break;
            }
            break;
         case HB_COM_OFLUSH:
            iResult = serial_clear_tx_buffer( iPort - 1 );
            break;
         default:
            hb_comSetComError( pCom, HB_COM_ERR_PARAMVALUE );
            return -1;
      }

      hb_comSetOsError( pCom, iResult );
      if( iResult < 0 )
      {
         iResult = -1;
      }
   }

   return iResult;
}

int hb_comMCR( int iPort, int * piValue, int iClr, int iSet )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;
   int iValue = 0;

   if( pCom )
   {
      iResult = serial_get_mcr( iPort - 1 );
      if( iResult >= 0 )
      {
         int iNewVal = ( iResult & ~iClr ) | iSet;
         iValue = iResult;
         iResult = iValue == iNewVal ? 0 : serial_set_mcr( iPort - 1, iNewVal );
      }
      hb_comSetOsError( pCom, iResult );
      if( iResult < 0 )
      {
         iResult = -1;
      }
   }

   if( piValue )
   {
      *piValue = iValue;
   }

   return iResult;
}

int hb_comMSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;
   int iValue = 0;

   if( pCom )
   {
      iValue = serial_get_msr( iPort - 1 );
      hb_comSetOsError( pCom, iValue );
      if( iValue < 0 )
      {
         iResult = -1;
         iValue = 0;
      }
   }

   if( piValue )
   {
      *piValue = iValue;
   }

   return iResult;
}

int hb_comLSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
      iValue = serial_get_lsr( iPort - 1 );
      hb_comSetOsError( pCom, iValue );
      if( iValue < 0 )
      {
         iResult = -1;
         iValue = 0;
      }
   }

   if( piValue )
   {
      *piValue = iValue;
   }

   return iResult;
}

int hb_comSendBreak( int iPort, int iDurationInMilliSecs )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iDurationInMilliSecs );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comFlowControl( int iPort, int *piFlow, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
      iResult = serial_get_handshaking( iPort - 1 );

      if( iResult >= 0 )
      {
         switch( iResult )
         {
            case SER_HANDSHAKING_XONXOFF:
               iValue |= HB_COM_FLOW_XOFF | HB_COM_FLOW_XOFF;
               break;
            case SER_HANDSHAKING_RTSCTS:
               iValue |= HB_COM_FLOW_IRTSCTS | HB_COM_FLOW_ORTSCTS;
               break;
            case SER_HANDSHAKING_DTRDSR:
               iValue |= HB_COM_FLOW_IDTRDSR | HB_COM_FLOW_ODTRDSR;
               break;
            case SER_HANDSHAKING_NONE:
               break;
         }
         if( iFlow >= 0 )
         {
            int iFlowVal = 0;

            if( iFlow & ( HB_COM_FLOW_IRTSCTS | HB_COM_FLOW_ORTSCTS ) )
            {
               iFlowVal = SER_HANDSHAKING_RTSCTS;
            }
            else if( iFlow & ( HB_COM_FLOW_IDTRDSR | HB_COM_FLOW_ODTRDSR ) )
            {
               iFlowVal = SER_HANDSHAKING_DTRDSR;
            }
            else if( iFlow & ( HB_COM_FLOW_XON | HB_COM_FLOW_XOFF ) )
            {
               iFlowVal = SER_HANDSHAKING_XONXOFF;
            }

            if( iFlowVal != iResult )
            {
               iResult = serial_set_handshaking( iPort - 1, iFlowVal );
            }
         }
      }
      hb_comSetOsError( pCom, iResult );
      if( iResult < 0 )
      {
         iResult = -1;
      }
   }

   if( piFlow )
   {
      *piFlow = iValue;
   }

   return iResult;
}

int hb_comFlowSet( int iPort, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iFlow );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comFlowChars( int iPort, int iXONchar, int iXOFFchar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   HB_SYMBOL_UNUSED( iXONchar );
   HB_SYMBOL_UNUSED( iXOFFchar );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return iResult;
}

int hb_comDiscardChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iChar );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comErrorChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iChar );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comOutputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      iResult = serial_get_tx_buffered( iPort - 1 );
      hb_comSetOsError( pCom, iResult );
      if( iResult < 0 )
      {
         iResult = -1;
      }
      else if( iResult == 0 )
      {
         iResult = HB_COM_TX_EMPTY;
      }
      else
      {
         iResult = 0;
      }
   }

   return iResult;
}

int hb_comInputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return iResult;
}

long hb_comSend( int iPort, const void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lSent = -1;

   if( pCom )
   {
      const char * buffer = static_cast<const char*>( data );
      HB_MAXUINT timer = hb_timerInit( timeout );

      hb_comSetOsError( pCom, SER_SUCCESS );
      lSent = 0;

      hb_vmUnlock();

      while( len > 0 )
      {
         int iSent;

         iSent = serial_write_buffered( iPort - 1, buffer, len );
         if( iSent < 0 )
         {
            hb_comSetOsError( pCom, iSent );
            if( lSent == 0 )
            {
               lSent = -1;
            }
            break;
         }

         lSent += iSent;
         buffer += iSent;
         len -= iSent;

         if( len > 0 )
         {
            if( ( timeout = hb_timerTest( timeout, &timer ) ) == 0 )
            {
               if( lSent == 0 )
               {
                  hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
                  lSent = -1;
               }
               break;
            }
            hb_releaseCPU();
         }
      }

      hb_vmLock();
   }

   return lSent;
}

long hb_comRecv( int iPort, void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lReceived = -1;

   if( pCom )
   {
      char * buffer = static_cast<char*>( data );
      HB_MAXUINT timer = hb_timerInit( timeout );

      hb_comSetOsError( pCom, SER_SUCCESS );
      lReceived = 0;

      hb_vmUnlock();

      while( len > 0 )
      {
         int iRecv = serial_read( iPort - 1, buffer, len );

         if( iRecv < 0 )
         {
            hb_comSetOsError( pCom, iRecv );
            if( lReceived == 0 )
            {
               lReceived = -1;
            }
            break;
         }

         lReceived += iRecv;
         buffer += iRecv;
         len -= iRecv;

         if( lReceived > 0 || ( timeout = hb_timerTest( timeout, &timer ) ) == 0 )
         {
            if( lReceived == 0 )
            {
               hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
               lReceived = -1;
            }
            break;
         }
         hb_releaseCPU();
      }

      hb_vmLock();
   }

   return lReceived;
}

static int s_comChkPortParam( int *piBaud, int *piParity, int *piSize, int *piStop )
{
   int iResult = 0;

   if( *piBaud == 0 )
   {
      *piBaud = 9600;
   }

   *piParity = HB_TOLOWER( *piParity );
   switch( *piParity )
   {
      case 0:
         *piParity = 'n';
      case 'n':
      case 'e':
      case 'o':
      case 's':
      case 'm':
         break;

      default:
         iResult = -1;
   }

   switch( *piSize )
   {
      case 0:
         *piSize = 8;
      case 8:
      case 7:
      case 6:
      case 5:
         break;
      default:
         iResult = -1;
   }

   switch( *piStop )
   {
      case 0:
         *piStop = 1;
      case 1:
      case 2:
         break;
      default:
         iResult = -1;
   }

   return iResult;
}

int hb_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      iResult = s_comChkPortParam( &iBaud, &iParity, &iSize, &iStop );
      if( iResult == 0 )
      {
         iResult = serial_set_bps( iPort - 1, iBaud );
         if( iResult == SER_SUCCESS )
         {
            iResult = serial_set_data( iPort - 1, iSize );
         }
         if( iResult == SER_SUCCESS )
         {
            iResult = serial_set_parity( iPort - 1, iParity );
         }
         if( iResult == SER_SUCCESS )
         {
            iResult = serial_set_stop( iPort - 1, iStop );
         }
         hb_comSetOsError( pCom, iResult );
         if( iResult < 0 )
         {
            iResult = -1;
         }
      }
      else
      {
         hb_comSetComError( pCom, HB_COM_ERR_PARAMVALUE );
      }   
   }

   return iResult;
}

int hb_comClose( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      hb_vmUnlock();
      iResult = serial_close( iPort - 1 );
      pCom->status &= ~HB_COM_OPEN;
      hb_comSetOsError( pCom, iResult );
      if( iResult < 0 )
      {
         iResult = -1;
      }
      hb_vmLock();
   }

   return iResult;
}

int hb_comOpen( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ENABLED );
   int iResult = -1;

   if( pCom )
   {
      if( pCom->status & HB_COM_OPEN )
      {
         hb_comSetComError( pCom, HB_COM_ERR_ALREADYOPEN );
      }
      else
      {
         int iBaud, iParity, iSize, iStop, iFlowControl;

         hb_vmUnlock();

         iBaud = iParity = iSize = iStop = 0;
         iFlowControl = SER_HANDSHAKING_NONE;
         s_comChkPortParam( &iBaud, &iParity, &iSize, &iStop );
         iResult = serial_open( iPort - 1, iBaud, iSize, iParity, iStop, iFlowControl );
         if( iResult == 0 )
         {
            pCom->status |= HB_COM_OPEN;
            hb_comSetOsError( pCom, 0 );
         }
         else
         {
            serial_close( iPort - 1 );
            hb_comSetOsError( pCom, iResult );
            iResult = -1;
         }

         hb_vmLock();
      }
   }

   return iResult;
}

/* end of HB_HAS_DOSSRL */

#elif defined( HB_HAS_PMCOM )

static void hb_comSetOsError( PHB_COM pCom, int iError )
{
   pCom->oserr = iError;

   switch( iError )
   {
      case COMERR_NOCHIP:
         pCom->error = HB_COM_ERR_WRONGPORT;
         break;
      case COMERR_RXOVERFLOW:
      case COMERR_NOMEMORY:
      case COMERR_GENERAL:
      default:
         pCom->error = HB_COM_ERR_OTHER;
   }
}

int hb_comInputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = -1;

   if( pCom )
   {
      iCount = COMTXBufferUsed( iPort - 1 );
   }

   return iCount;
}

int hb_comOutputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iCount = -1;

   if( pCom )
   {
      iCount = COMRXBufferUsed( iPort - 1 );
   }

   return iCount;
}

int hb_comFlush( int iPort, int iType )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      iResult = 0;
      hb_comSetOsError( pCom, 0 );
      switch( iType )
      {
         case HB_COM_IFLUSH:
         case HB_COM_IOFLUSH:
            COMClearRXBuffer( iPort - 1 );
            if( iType == HB_COM_IFLUSH )
            {
               break;
            }
            break;
         case HB_COM_OFLUSH:
            COMClearTXBuffer( iPort - 1 );
            break;
         default:
            iResult = -1;
            hb_comSetComError( pCom, HB_COM_ERR_PARAMVALUE );
            break;
      }
   }

   return iResult;
}

int hb_comMCR( int iPort, int * piValue, int iClr, int iSet )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;
   int iValue = 0;

   if( pCom )
   {
      /* MCR_OUT1, MCR_OUT2, MCR_LOOP and reading current state
       * is unsupported
       */
      if( iSet & HB_COM_MCR_DTR )
      {
         COMSetDtr( iPort - 1, 1 );
      }
      else if( iClr & HB_COM_MCR_DTR )
      {
         COMSetDtr( iPort - 1, 0 );
      }

      if( iSet & HB_COM_MCR_RTS )
      {
         COMSetRts( iPort - 1, 1 );
      }
      else if( iClr & HB_COM_MCR_RTS )
      {
         COMSetRts( iPort - 1, 0 );
      }

      iResult = 0;
   }

   if( piValue )
   {
      *piValue = iValue;
   }

   return iResult;
}

int hb_comMSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;
   int iValue = 0;

   if( pCom )
   {
      int iMSR = COMGetModemStatus( iPort - 1 );

      if( iMSR & DELTA_CTS )
      {
         iValue |= HB_COM_MSR_DELTA_CTS;
      }
      if( iMSR & DELTA_DSR )
      {
         iValue |= HB_COM_MSR_DELTA_DSR;
      }
      if( iMSR & DELTA_RI )
      {
         iValue |= HB_COM_MSR_TERI;
      }
      if( iMSR & DELTA_CD )
      {
         iValue |= HB_COM_MSR_DELTA_DCD;
      }

      if( iMSR & CTS_LINE )
      {
         iValue |= HB_COM_MSR_CTS;
      }
      if( iMSR & DSR_LINE )
      {
         iValue |= HB_COM_MSR_DSR;
      }
      if( iMSR & RI_LINE )
      {
         iValue |= HB_COM_MSR_RI;
      }
      if( iMSR & CD_LINE )
      {
         iValue |= HB_COM_MSR_DCD;
      }

      iResult = 0;
   }

   if( piValue )
   {
      *piValue = iValue;
   }

   return iResult;
}

int hb_comLSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   if( piValue )
   {
      *piValue = iValue;
   }

   return iResult;
}

int hb_comSendBreak( int iPort, int iDurationInMilliSecs )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iDurationInMilliSecs );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comFlowControl( int iPort, int *piFlow, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1, iValue = 0;

   if( pCom )
   {
      int iFlowVal = COMGetFlowControl( iPort - 1 );

      if( iFlowVal & FLOW_RTS )
      {
         iValue |= HB_COM_FLOW_IRTSCTS;
      }
      if( iFlowVal & FLOW_CTS )
      {
         iValue |= HB_COM_FLOW_ORTSCTS;
      }
      if( iFlowVal & FLOW_DTR )
      {
         iValue |= HB_COM_FLOW_IDTRDSR;
      }
      if( iFlowVal & FLOW_DSR )
      {
         iValue |= HB_COM_FLOW_ODTRDSR;
      }
      if( iFlowVal & FLOW_DCD )
      {
         iValue |= HB_COM_FLOW_DCD;
      }
      if( iFlowVal & FLOW_XOFF )
      {
         iValue |= HB_COM_FLOW_XOFF;
      }
      if( iFlowVal & FLOW_XON )
      {
         iValue |= HB_COM_FLOW_XON;
      }

      if( iFlow >= 0 )
      {
         iFlowVal = 0;
         if( iFlow & HB_COM_FLOW_IRTSCTS )
         {
            iFlowVal |= FLOW_RTS;
         }
         if( iFlow & HB_COM_FLOW_ORTSCTS )
         {
            iFlowVal |= FLOW_CTS;
         }
         if( iFlow & HB_COM_FLOW_IDTRDSR )
         {
            iFlowVal |= FLOW_DTR;
         }
         if( iFlow & HB_COM_FLOW_ODTRDSR )
         {
            iFlowVal |= FLOW_DSR;
         }
         if( iFlow & HB_COM_FLOW_DCD )
         {
            iFlowVal |= FLOW_DCD;
         }
         if( iFlow & HB_COM_FLOW_XOFF )
         {
            iFlowVal |= FLOW_XOFF;
         }
         if( iFlow & HB_COM_FLOW_XON )
         {
            iFlowVal |= FLOW_XON;
         }

         COMSetFlowControl( iPort - 1, iFlowVal );
      }

      iResult = 0;
   }

   if( piFlow )
   {
      *piFlow = iValue;
   }

   return iResult;
}

int hb_comFlowSet( int iPort, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iFlow );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comFlowChars( int iPort, int iXONchar, int iXOFFchar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      COMSetFlowChars( iPort - 1, iXONchar, iXOFFchar );
      iResult = 0;
   }

   return iResult;
}

int hb_comDiscardChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iChar );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comErrorChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iChar );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comOutputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      iResult = COMRXBufferUsed( iPort - 1 );
      if( iResult == 0 )
      {
         iResult = HB_COM_TX_EMPTY;
      }
      else if( iResult > 0 )
      {
         iResult = 0;
      }
   }

   return iResult;
}

int hb_comInputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return iResult;
}

long hb_comSend( int iPort, const void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lSent = -1;

   if( pCom )
   {
      const char * buffer = static_cast<const char*>( data );
      HB_MAXUINT timer = hb_timerInit( timeout );

      hb_comSetOsError( pCom, 0 );
      lSent = 0;

      hb_vmUnlock();

      while( len > 0 )
      {
         int iSent, iErr;

         iErr = COMWriteBuffer( iPort - 1, buffer, nullptr, len, &iSent );
         lSent += iSent;
         if( iErr == COMERR_TXOVERFLOW )
         {
            buffer += iSent;
            len -= iSent;
            if( ( timeout = hb_timerTest( timeout, &timer ) ) == 0 )
            {
               if( lSent == 0 )
               {
                  hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
                  lSent = -1;
               }
               break;
            }
            hb_releaseCPU();
         }
         else
         {
            break;
         }
      }

      hb_vmLock();
   }

   return lSent;
}

long hb_comRecv( int iPort, void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   long lReceived = -1;

   if( pCom )
   {
      char * buffer = static_cast<char*>( data );
      HB_MAXUINT timer = hb_timerInit( timeout );

      hb_comSetOsError( pCom, 0 );
      lReceived = 0;

      hb_vmUnlock();

      while( len > 0 )
      {
         int iErr = COMReadChar( iPort - 1, buffer, nullptr );

         if( iErr == 0 )
         {
            ++buffer;
            --len;
            ++lReceived;
         }
         else if( iErr == COM_BUFEMPTY )
         {
            if( lReceived > 0 || ( timeout = hb_timerTest( timeout, &timer ) ) == 0 )
            {
               if( lReceived == 0 )
               {
                  hb_comSetComError( pCom, HB_COM_ERR_TIMEOUT );
                  lReceived = -1;
               }
               break;
            }
            hb_releaseCPU();
         }
         else
         {
            hb_comSetOsError( pCom, iErr );
            break;
         }
      }

      hb_vmLock();
   }

   return lReceived;
}

static int s_comChkPortParam( int *piBaud, int *piParity, int *piSize, int *piStop )
{
   int iResult = 0;

   if( *piBaud == 0 )
   {
      *piBaud = 9600;
   }

   *piParity = HB_TOUPPER( *piParity );
   switch( *piParity )
   {
      case 0:
         *piParity = 'N';
      case 'N':
      case 'E':
      case 'O':
      case 'S':
      case 'M':
         break;

      default:
         iResult = -1;
   }

   switch( *piSize )
   {
      case 0:
         *piSize = 8;
      case 8:
      case 7:
      case 6:
      case 5:
         break;
      default:
         iResult = -1;
   }

   switch( *piStop )
   {
      case 0:
         *piStop = 1;
      case 1:
      case 2:
         break;
      default:
         iResult = -1;
   }

   return iResult;
}

int hb_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      iResult = s_comChkPortParam( &iBaud, &iParity, &iSize, &iStop );
      if( iResult == 0 )
      {
         COMSetTransmitParameters( iPort - 1, iBaud, iSize, iParity, iStop );
         hb_comSetOsError( pCom, 0 );
      }
      else
      {
         hb_comSetComError( pCom, HB_COM_ERR_PARAMVALUE );
      }
   }

   return iResult;
}

int hb_comClose( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );
   int iResult = -1;

   if( pCom )
   {
      hb_vmUnlock();
      COMPortClose( iPort - 1 );
      pCom->status &= ~HB_COM_OPEN;
      hb_comSetOsError( pCom, 0 );
      iResult = 0;
      hb_vmLock();
   }

   return iResult;
}

int hb_comOpen( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ENABLED );
   int iResult = -1;

   if( pCom )
   {
      if( pCom->status & HB_COM_OPEN )
      {
         hb_comSetComError( pCom, HB_COM_ERR_ALREADYOPEN );
      }
      else
      {
         int iBaud, iParity, iSize, iStop, iFlowControl;

         hb_vmUnlock();

         iBaud = iParity = iSize = iStop = 0;
         iFlowControl = 0;
         s_comChkPortParam( &iBaud, &iParity, &iSize, &iStop );
         iResult = COMPortOpen( iPort - 1, iBaud, iSize, iParity, iStop, iFlowControl, nullptr );
         if( iResult == 0 )
         {
            pCom->status |= HB_COM_OPEN;
            hb_comSetOsError( pCom, 0 );
         }
         else
         {
            hb_comSetOsError( pCom, iResult );
            iResult = -1;
         }

         hb_vmLock();
      }
   }

   return iResult;
}

/* end of HB_HAS_PMCOM */

#else

int hb_comInputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comOutputCount( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comFlush( int iPort, int iType )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iType );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comMCR( int iPort, int * piValue, int iClr, int iSet )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( piValue );
   HB_SYMBOL_UNUSED( iClr );
   HB_SYMBOL_UNUSED( iSet );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comMSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( piValue );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comLSR( int iPort, int * piValue )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( piValue );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comSendBreak( int iPort, int iDurationInMilliSecs )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iDurationInMilliSecs );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comFlowControl( int iPort, int *piFlow, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( piFlow );
   HB_SYMBOL_UNUSED( iFlow );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comFlowSet( int iPort, int iFlow )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iFlow );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comFlowChars( int iPort, int iXONchar, int iXOFFchar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iXONchar );
   HB_SYMBOL_UNUSED( iXOFFchar );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comDiscardChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iChar );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comErrorChar( int iPort, int iChar )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iChar );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comOutputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comInputState( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

long hb_comSend( int iPort, const void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( data );
   HB_SYMBOL_UNUSED( len );
   HB_SYMBOL_UNUSED( timeout );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

long hb_comRecv( int iPort, void * data, long len, HB_MAXINT timeout )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( data );
   HB_SYMBOL_UNUSED( len );
   HB_SYMBOL_UNUSED( timeout );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   HB_SYMBOL_UNUSED( iBaud );
   HB_SYMBOL_UNUSED( iParity );
   HB_SYMBOL_UNUSED( iSize );
   HB_SYMBOL_UNUSED( iStop );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comClose( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_OPEN );

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

int hb_comOpen( int iPort )
{
   PHB_COM pCom = hb_comGetPort( iPort, HB_COM_ENABLED );
   int iTODO_serial_port_support;

   if( pCom )
   {
      hb_comSetComError( pCom, HB_COM_ERR_NOSUPPORT );
   }

   return -1;
}

#endif

static int s_iComInit = 0;

static void hb_com_exit( void* cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( s_iComInit )
   {
      hb_comCloseAll();
      s_iComInit = 0;
   }
}

static void hb_com_init( void* cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( ! s_iComInit )
   {
      for( int iPort = 0; iPort < HB_COM_PORT_MAX; ++iPort )
      {
         s_comList[ iPort ].port = iPort + 1;
         s_comList[ iPort ].status = HB_COM_ENABLED;
      }

      hb_vmAtQuit( hb_com_exit, nullptr );

      s_iComInit = 1;
   }
}

HB_CALL_ON_STARTUP_BEGIN( _hb_com_init_ )
   hb_vmAtInit( hb_com_init, nullptr );
HB_CALL_ON_STARTUP_END( _hb_com_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_com_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hb_com_init_ )
   #include "hbiniseg.h"
#endif
