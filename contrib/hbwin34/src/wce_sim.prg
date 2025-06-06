//
// SIM interface code
//
// Copyright 2009 Jose Luis Capel <jlcapel@hotmail.com>
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

#include <hbclass.ch>

#include "hbsim.ch"

CREATE CLASS wce_Sim

   VAR lInitialized INIT .F.
   VAR hSim
   VAR nLastError INIT SIM_E_OK

   METHOD New()
   METHOD End()

   METHOD lInitialize()    // Must be initialized before any other method
   METHOD lDeInitialize()  // Must be deinitialized....

   METHOD lNumberOfPhoneBookEntries(nType, /* @ */ nTotal, /* @ */ nUsed)
   METHOD aGetAllPhoneBookEntries(nType)  // --> array with phonebook entries of nType storage
   METHOD lGetSimPhoneEntry(nPos, nType, /* @ */ aEntry)  // --> aEntry by reference contains phonebook entry
   METHOD lSetSimPhoneEntry(nPos, nType, cNumber, cName, nPlan, nAddrType)  // --> .T. / .F. if phonebook entry written
   METHOD lDelSimPhoneEntry(nPos, nType)  // --> .T. / .F. if phonebook entry deleted

ENDCLASS

METHOD wce_Sim:New()
   RETURN Self

METHOD wce_Sim:lInitialize()

   ::nLastError := wce_SimInitialize(@::hSim)

   RETURN ::lInitialized := (::nLastError == SIM_E_OK)

METHOD wce_Sim:lDeInitialize()

   IF ::lInitialized
      ::nLastError := wce_SimDeInitialize(::hSim)
      ::lInitialized := ::nLastError != SIM_E_OK
   ELSE
      ::nLastError := SIM_E_HB_NOTINITIALIZED
   ENDIF

   RETURN ::nLastError == SIM_E_OK

METHOD wce_Sim:lNumberOfPhoneBookEntries(nType, /* @ */ nTotal, /* @ */ nUsed)

   IF ::lInitialized
      ::nLastError := wce_SimPhonebookStatus(::hSim, hb_defaultValue(nType, SIM_PBSTORAGE_SIM), @nTotal, @nUsed)
   ELSE
      ::nLastError := SIM_E_HB_NOTINITIALIZED
   ENDIF

   RETURN ::nLastError == SIM_E_OK

METHOD wce_Sim:aGetAllPhoneBookEntries(nType)

   LOCAL aEntries := {}
   LOCAL nTotal
   LOCAL nUsed
   LOCAL aEntry
   LOCAL nPos

   IF ::lInitialized

      hb_default(@nType, SIM_PBSTORAGE_SIM)

      ::nLastError := SIM_E_OK

      IF ::lNumberOfPhoneBookEntries(nType, @nTotal, @nUsed)
         FOR nPos := 1 TO nUsed
            aEntry := {}
            IF (::nLastError := wce_SimReadPhonebookEntry(::hSim, nType, nPos, @aEntry)) == SIM_E_OK
               AAdd(aEntries, aEntry)
            ELSE
               EXIT
            ENDIF
         NEXT
      ENDIF
   ELSE
      ::nLastError := SIM_E_HB_NOTINITIALIZED
   ENDIF

   RETURN aEntries

METHOD wce_Sim:lGetSimPhoneEntry(nPos, nType, /* @ */ aEntry)

   LOCAL a

   IF ::lInitialized
      ::nLastError := wce_SimReadPhonebookEntry(::hSim, hb_defaultValue(nType, SIM_PBSTORAGE_SIM), nPos, @a)
      aEntry := {a}
   ELSE
      ::nLastError := SIM_E_HB_NOTINITIALIZED
   ENDIF

   RETURN ::nLastError == SIM_E_OK

METHOD wce_Sim:lSetSimPhoneEntry(nPos, nType, cNumber, cName, nPlan, nAddrType)

   IF ::lInitialized
      ::nLastError := wce_SimWritePhonebookEntry(::hSim, hb_defaultValue(nType, SIM_PBSTORAGE_SIM), hb_defaultValue(nPos, SIM_PBINDEX_FIRSTAVAILABLE), cNumber, cName, nPlan, nAddrType)
   ELSE
      ::nLastError := SIM_E_HB_NOTINITIALIZED
   ENDIF

   RETURN ::nLastError == SIM_E_OK

METHOD wce_Sim:lDelSimPhoneEntry(nPos, nType)

   IF ::lInitialized
      ::nLastError := wce_SimDeletePhonebookEntry(::hSim, hb_defaultValue(nType, SIM_PBSTORAGE_SIM), nPos)
   ELSE
      ::nLastError := SIM_E_HB_NOTINITIALIZED
   ENDIF

   RETURN ::nLastError == SIM_E_OK

METHOD PROCEDURE wce_Sim:End()

   IF ::lInitialized
      ::lDeInitialize()
   ENDIF

   RETURN
