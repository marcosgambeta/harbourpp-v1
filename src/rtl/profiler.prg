//
// Profiler reporting classes
//
// Copyright 2001-2002 Dave Pearson <davep@davep.org>
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

// Rationale:
//
// There are three aspects to profiling:
//
// 1) Gathering profile information.
// 2) Taking a snapshot of an application's profile information.
// 3) Reporting on the data gathered in the snapshot.
//
// Point 1 is handled in Harbour's virtual machine. This source aims to
// provide code for performing points 2 and 3. A class is provided to
// gather, hold and manipulate the profile snapshot and a hierarchy of
// classes exist for reporting on that snapshot. The reporting classes are
// designed such that they are easy to inherit from and improve upon.
//
// The idea behind all of this is that the the design should provide all the
// necessary building blocks for writing profiler reporters that suite the
// user's needs (perhaps as part of an extended debugger or something).

// Notes:
//
// As much as possible, the profiler class and the profile report classes
// attempt to turn off the profiler to ensure that we don't get some sort of
// Heisenberg effect. In other words, we don't want the profiler showing up
// in the profiler.
//
// Many of the "PROTECTED:" scope specifiers in the source have been
// commented out where there's a problem with scope in Harbour's class
// system. Note that those comments will be removed when the bug is fixed.

// TODO:
// - Handle any TODO: items in the source.
// - Document the classes and the class hierarchy.

// Thanks:
//
// Thanks to Antonio and Patrick for agreeing to replace the old profile
// reporting code with this approach.

#include "hbclass.ch"
#include "fileio.ch"

// ---

CREATE CLASS HBProfileEntity

   EXPORTED:

   VAR cName    READONLY
   VAR nCalls   READONLY
   VAR nTicks   READONLY

   ACCESS nSeconds
   ACCESS nMeanTicks
   ACCESS nMeanSeconds

   METHOD init(cName, aInfo)
   METHOD describe()

ENDCLASS

//

METHOD HBProfileEntity:init(cName, aInfo)

   ::cName  := cName
   ::nCalls := aInfo[1]
   ::nTicks := aInfo[2]

   RETURN Self

ACCESS nSeconds CLASS HBProfileEntity
   RETURN hb_Clocks2Secs(::nTicks)

ACCESS nMeanTicks CLASS HBProfileEntity
   RETURN IIf(::nCalls == 0, 0, ::nTicks / ::nCalls)

ACCESS nMeanSeconds CLASS HBProfileEntity
   RETURN IIf(::nCalls == 0, 0, ::nSeconds / ::nCalls)

METHOD HBProfileEntity:describe()
   RETURN "Base Entity"

// ---

CREATE CLASS HBProfileFunction INHERIT HBProfileEntity

   EXPORTED:

   METHOD describe()

ENDCLASS

//

METHOD HBProfileFunction:describe()
   RETURN "Function"

// ---

CREATE CLASS HBProfileMethod INHERIT HBProfileEntity

   EXPORTED:

   METHOD describe()

ENDCLASS

//

METHOD HBProfileMethod:describe()
   RETURN "Method"

// ---

CREATE CLASS HBProfileOPCode INHERIT HBProfileEntity

   EXPORTED:

   METHOD describe()

ENDCLASS

//

METHOD HBProfileOPCode:describe()
   RETURN "OPCode"

// ---

CREATE CLASS HBProfile

   EXPORTED:

   VAR aProfile

   METHOD init()
   METHOD gather()
   METHOD forEach(b)
   METHOD sort(b)
   METHOD nameSort()
   METHOD callSort()
   METHOD timeSort()
   METHOD totalCalls()
   METHOD totalTicks()
   METHOD totalSeconds()

   PROTECTED:

   METHOD gatherFunctions()
   METHOD gatherMethods()
   METHOD reset()
   METHOD ignoreSymbol(cSymbol)

ENDCLASS

//

METHOD HBProfile:init()

   LOCAL lProfile := __SetProfiler(.F.)

   ::reset()

   __SetProfiler(lProfile)

   RETURN Self

METHOD HBProfile:reset()

   ::aProfile := {}

   RETURN Self

METHOD HBProfile:ignoreSymbol(cSymbol)
   RETURN hb_LeftEq(cSymbol, "HBPROFILE") .OR. cSymbol == "__SETPROFILER"

METHOD HBProfile:gatherFunctions()

   LOCAL lProfile  := __SetProfiler(.F.)
   LOCAL nSymCount := __dynsCount()
   LOCAL cName
   LOCAL n

   // For each known symbol.
   // TODO: Question: Will the symbol count have changed because
   //                 we've created variables?
   FOR n := 1 TO nSymCount

      // Is the symbol a function?
      IF __dynsIsFun(n)

         // If we're not ignoring the symbol...
         IF !::ignoreSymbol(cName := __dynsGetName(n))
            // Yes, it is, add it to the profile.
            AAdd(::aProfile, HBProfileFunction():new(cName, __dynsGetPrf(n)))
         ENDIF
      ENDIF
   NEXT

   __SetProfiler(lProfile)

   RETURN Self

METHOD HBProfile:gatherMethods()

   LOCAL lProfile := __SetProfiler(.F.)
   LOCAL n        := 1
   LOCAL cClass
   LOCAL nMembers
   LOCAL aMembers
   LOCAL nMember

   // For each class in the environment...
   DO WHILE !Empty(cClass := __className(n))

      // If we're not ignoring the class' methods...
      IF !::ignoreSymbol(cClass)

         // Collect class members.
         nMembers := Len(aMembers := __classSel(n))

         FOR nMember := 1 TO nMembers

            // If we've got a member name...
            IF !Empty(aMembers[nMember])
               // Add it to the profile.
               AAdd(::aProfile, HBProfileMethod():new(cClass + ":" + aMembers[nMember], __GetMsgPrf(n, aMembers[nMember])))
            ENDIF
         NEXT
      ENDIF

      ++n
   ENDDO

   __SetProfiler(lProfile)

   RETURN Self

METHOD HBProfile:gather()

   LOCAL lProfile  := __SetProfiler(.F.)

   ::reset()            // Reset the profile.
   ::gatherFunctions()  // Gather function calls
   ::gatherMethods()    // Gather method calls

   __SetProfiler(lProfile)

   RETURN Self

METHOD HBProfile:forEach(b)

   LOCAL lProfile := __SetProfiler(.F.)

   AEval(::aProfile, b)

   __SetProfiler(lProfile)

   RETURN Self

METHOD HBProfile:sort(b)

   LOCAL lProfile := __SetProfiler(.F.)

   ASort(::aProfile, NIL, NIL, b)

   __SetProfiler(lProfile)

   RETURN Self

METHOD HBProfile:nameSort()

   LOCAL lProfile := __SetProfiler(.F.)

   ::sort({|oX, oY|oX:cName < oY:cName})

   __SetProfiler(lProfile)

   RETURN Self

METHOD HBProfile:callSort()

   LOCAL lProfile := __SetProfiler(.F.)

   ::sort({|oX, oY|oX:nCalls > oY:nCalls})

   __SetProfiler(lProfile)

   RETURN Self

METHOD HBProfile:timeSort()

   LOCAL lProfile := __SetProfiler(.F.)

   ::sort({|oX, oY|oX:nTicks > oY:nTicks})

   __SetProfiler(lProfile)

   RETURN Self

METHOD HBProfile:totalCalls()

   LOCAL lProfile := __SetProfiler(.F.)
   LOCAL nCalls   := 0

   ::forEach({|o|nCalls += o:nCalls})

   __SetProfiler(lProfile)

   RETURN nCalls

METHOD HBProfile:totalTicks()

   LOCAL lProfile := __SetProfiler(.F.)
   LOCAL nTicks   := 0

   ::forEach({|o|nTicks += o:nTicks})

   __SetProfiler(lProfile)

   RETURN nTicks

METHOD HBProfile:totalSeconds()

   LOCAL lProfile := __SetProfiler(.F.)
   LOCAL nSeconds := 0

   ::forEach({|o|nSeconds += o:nSeconds})

   __SetProfiler(lProfile)

   RETURN nSeconds

// ---

CREATE CLASS HBProfileLowLevel INHERIT HBProfile

   EXPORTED:

   METHOD gather()

   PROTECTED:

   METHOD gatherOPCodes()

ENDCLASS

//

METHOD HBProfileLowLevel:gather()

   LOCAL lProfile := __SetProfiler(.F.)

   // Gather functions and methods.
   ::super:gather()

   // Also gather opcodes.
   ::gatherOPCodes()

   __SetProfiler(lProfile)

   RETURN Self

METHOD HBProfileLowLevel:gatherOPCodes()

   LOCAL nMax := __opCount()
   LOCAL cName
   LOCAL nOP

   // Loop over all the Harbour OP codes. Note that they start at 0.
   FOR nOP := 0 TO nMax - 1
      // If we're not ignoring this opcode.
      IF !::ignoreSymbol(cName := "OPCODE( " + Str(nOP, 3) + " )")
         // Add it to the profile.
         AAdd(::aProfile, HBProfileOpcode():new(cName, __opGetPrf(nOP)))
      ENDIF
   NEXT

   RETURN Self

// ---

CREATE CLASS HBProfileReport

   PROTECTED:

   VAR oProfile

   METHOD writeLines(aLines)
   METHOD header()
   METHOD emitHeader()
   METHOD line(oEntity)
   METHOD emitLine(oEntity)

   EXPORTED:

   METHOD init(oProfile)
   METHOD generate(bFilter)

ENDCLASS

//

METHOD HBProfileReport:init(oProfile)

   LOCAL lProfile := __SetProfiler(.F.)

   ::oProfile := oProfile

   __SetProfiler(lProfile)

   RETURN Self

METHOD HBProfileReport:writeLines(aLines)

   AEval(aLines, {|c|QOut(c)})

   RETURN Self

METHOD HBProfileReport:header()
   RETURN { ;
      "Name                                Type       Calls    Ticks       Seconds", ;
      "=================================== ========== ======== =========== ==========="}

METHOD HBProfileReport:emitHeader()

   ::writeLines(::Header())

   RETURN Self

METHOD HBProfileReport:line(oEntity)
   RETURN { ;
      PadR(oEntity:cName,      35) + " " + ;
      PadR(oEntity:describe(),  8) + " " + ;
      Str(oEntity:nCalls,      10) + " " + ;
      Str(oEntity:nTicks,      11) + " " + ;
      Str(oEntity:nSeconds,    11, 2)}

METHOD HBProfileReport:emitLine(oEntity)

   ::writeLines(::line(oEntity))

   RETURN Self

METHOD HBProfileReport:generate(bFilter)

   LOCAL lProfile := __SetProfiler(.F.)

   hb_default(@bFilter, {||.T.})

   ::emitHeader():oProfile:forEach({|o|IIf(Eval(bFilter, o), ::emitLine(o), NIL)})

   __SetProfiler(lProfile)

   RETURN Self

// ---

CREATE CLASS HBProfileReportToFile INHERIT HBProfileReport

   PROTECTED:

   VAR hFile

   METHOD writeLines(aLines)

   EXPORTED:

   METHOD generate(bFilter, cFile)

ENDCLASS

//

METHOD HBProfileReportToFile:writeLines(aLines)

   IF ::hFile != F_ERROR
      AEval(aLines, {|c|FWrite(::hFile, c + hb_eol())})
   ENDIF

   RETURN Self

METHOD HBProfileReportToFile:generate(bFilter, cFile)

   LOCAL lProfile := __SetProfiler(.F.)

   IF (::hFile := FCreate(hb_defaultValue(cFile, "hbprof.txt"))) != F_ERROR
      ::super:generate(bFilter)
      FClose(::hFile)
   ELSE
      // TODO: Throw an error
   ENDIF

   __SetProfiler(lProfile)

   RETURN Self

// ---

CREATE CLASS HBProfileReportToArray INHERIT HBProfileReport

   PROTECTED:

   VAR aReport

   METHOD writeLines(aLines)

   EXPORTED:

   METHOD generate(bFilter)

ENDCLASS

//

METHOD HBProfileReportToArray:writeLines(aLines)

   AEval(aLines, {|c|AAdd(::aReport, c)})

   RETURN Self

METHOD HBProfileReportToArray:generate(bFilter)

   ::aReport := {}
   ::super:generate(bFilter)

   RETURN ::aReport

// ---

CREATE CLASS HBProfileReportToString INHERIT HBProfileReportToArray

   EXPORTED:

   METHOD generate(bFilter)

ENDCLASS

//

METHOD HBProfileReportToString:generate(bFilter)

   LOCAL cReport := ""

   AEval(::super:generate(bFilter), {|c|cReport += c + hb_eol()})

   RETURN cReport

// ---

CREATE CLASS HBProfileReportToTBrowse INHERIT HBProfileReportToArray

   PROTECTED:

   VAR nEntity

   METHOD emitHeader()
   METHOD emitLine(oEntity)
   METHOD addColumns(oBrowse)

   EXPORTED:

   METHOD generate(bFilter, nTop, nLeft, nBottom, nRight)
   METHOD currentEntity()

ENDCLASS

//

// No header required.
METHOD HBProfileReportToTBrowse:emitHeader()
   RETURN Self

METHOD HBProfileReportToTBrowse:emitLine(oEntity)

   // Don't "emit" anything, simply add the entity to the array.
   AAdd(::aReport, oEntity)

   RETURN Self

METHOD HBProfileReportToTBrowse:generate(bFilter, nTop, nLeft, nBottom, nRight)

   LOCAL lProfile := __SetProfiler(.F.)
   LOCAL oBrowse

   ::nEntity := 1  // Start with the first entity.

   ::super:generate(bFilter)  // Generate the array.

   oBrowse := TBrowseNew(nTop, nLeft, nBottom, nRight)  // Build the browse.

   oBrowse:goTopBlock    := {||::nEntity := 1}
   oBrowse:goBottomBlock := {||::nEntity := Len(::aReport)}
   oBrowse:skipBlock     := {|nSkip, nPos|nPos := ::nEntity, ;
      ::nEntity := IIf(nSkip > 0, ;
      Min(Len(::aReport), ::nEntity + nSkip), ;
      Max(1, ::nEntity + nSkip)), ::nEntity - nPos}

   ::addColumns(oBrowse)

   __SetProfiler(lProfile)

   RETURN oBrowse

METHOD HBProfileReportToTBrowse:addColumns(oBrowse)

   oBrowse:addColumn(TBColumnNew("Name",         {||PadR(::currentEntity():cName,        35   )}))
   oBrowse:addColumn(TBColumnNew("Type",         {||PadR(::currentEntity():describe(),    8   )}))
   oBrowse:addColumn(TBColumnNew("Calls",        {||PadL(::currentEntity():nCalls,       10   )}))
   oBrowse:addColumn(TBColumnNew("Ticks",        {||PadL(::currentEntity():nTicks,       11   )}))
   oBrowse:addColumn(TBColumnNew("Seconds",      {|| Str(::currentEntity():nSeconds,     11, 2)}))
   oBrowse:addColumn(TBColumnNew("Mean;Ticks",   {|| Str(::currentEntity():nMeanTicks,   11, 2)}))
   oBrowse:addColumn(TBColumnNew("Mean;Seconds", {|| Str(::currentEntity():nMeanSeconds, 11, 2)}))

   RETURN Self

METHOD HBProfileReportToTBrowse:currentEntity()
   RETURN ::aReport[::nEntity]
