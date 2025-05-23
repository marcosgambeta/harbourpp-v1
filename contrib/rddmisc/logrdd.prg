//
// LOGRDD
//
// Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice.com>
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

/*
 * A simple RDD which introduce logging to file. It inherits from
 * any existent RDD but if you write / replace / delete something
 * on tables it writes changes in a log file.
 */

#include <dbinfo.ch>
#include <fileio.ch>
#include <hbusrrdd.ch>
#include <rddsys.ch>

#define ARRAY_FILENAME      1
#define ARRAY_FHANDLE       2
#define ARRAY_TAG           3
#define ARRAY_ACTIVE        4
#define ARRAY_RDDNAME       5
#define ARRAY_MSGLOGBLOCK   6
#define ARRAY_USERLOGBLOCK  7

ANNOUNCE LOGRDD

DYNAMIC hb_LogRddInherit  /* To be defined at user level */

STATIC s_nRddID := -1

STATIC FUNCTION LOGRDD_INIT( nRDD )

   /* Defaults */

   LOCAL cFileName := "changes.log"
   LOCAL lActive   := .F.
   LOCAL cTag      := NetName() + "\" + hb_UserName()
   LOCAL cRDDName  := hb_LogRddInherit()

   /* Log File will be open later so user can change parameters */

   /* Store data in RDD cargo */
   /* cFileName, hFile, cTag, lActive, cRDDName, bMsgLogBlock, bUserLogBlock */
   USRRDD_RDDDATA(nRDD, { cFileName, NIL, cTag, lActive, cRDDName, NIL, NIL })

   RETURN HB_SUCCESS

STATIC FUNCTION LOGRDD_EXIT( nRDD )

   LOCAL aRDDData  := USRRDD_RDDDATA(nRDD)

   /* Closing log file */

   IF aRDDData[ARRAY_FHANDLE] != NIL
      FClose( aRDDData[ARRAY_FHANDLE] )
      aRDDData[ARRAY_FHANDLE] := NIL
   ENDIF

   RETURN HB_SUCCESS

// Create database from current WA fields definition

STATIC FUNCTION LOGRDD_CREATE( nWA, aOpenInfo )

   LOCAL nResult

   IF ( nResult := UR_SUPER_CREATE( nWA, aOpenInfo ) ) == HB_SUCCESS
      ToLog( "CREATE", nWA, aOpenInfo )
   ENDIF

   RETURN nResult

// Creating fields for new DBF - dbCreate() in current workarea

STATIC FUNCTION LOGRDD_CREATEFIELDS( nWA, aStruct )

   LOCAL nResult

   IF ( nResult := UR_SUPER_CREATEFIELDS( nWA, aStruct ) ) == HB_SUCCESS
      ToLog( "CREATEFIELDS", nWA, aStruct )
   ENDIF

   RETURN nResult

// Open workarea

STATIC FUNCTION LOGRDD_OPEN( nWA, aOpenInfo )

   LOCAL nResult

   IF ( nResult := UR_SUPER_OPEN( nWA, aOpenInfo ) ) == HB_SUCCESS
      ToLog( "OPEN", nWA, aOpenInfo )
   ENDIF

   RETURN nResult

// Close workarea

STATIC FUNCTION LOGRDD_CLOSE( nWA )

   LOCAL cFile  := dbInfo( DBI_FULLPATH )
   LOCAL cAlias := Alias()
   LOCAL nResult

   IF ( nResult := UR_SUPER_CLOSE( nWA ) ) == HB_SUCCESS
      ToLog( "CLOSE", nWA, cFile, cAlias )
   ENDIF

   RETURN nResult

STATIC FUNCTION LOGRDD_APPEND( nWA, lUnlockAll )

   LOCAL nResult

   IF ( nResult := UR_SUPER_APPEND( nWA, lUnlockAll ) ) == HB_SUCCESS
      ToLog( "APPEND", nWA, lUnlockAll )
   ENDIF

   RETURN nResult

STATIC FUNCTION LOGRDD_DELETE( nWA )

   LOCAL nResult

   IF ( nResult := UR_SUPER_DELETE( nWA ) ) == HB_SUCCESS
      ToLog( "DELETE", nWA )
   ENDIF

   RETURN nResult

STATIC FUNCTION LOGRDD_RECALL( nWA )

   LOCAL nResult

   IF ( nResult := UR_SUPER_RECALL( nWA ) ) == HB_SUCCESS
      ToLog( "RECALL", nWA )
   ENDIF

   RETURN nResult

STATIC FUNCTION LOGRDD_PUTVALUE( nWA, nField, xValue )

   LOCAL xOldValue := FieldGet( nField )
   LOCAL nResult   := UR_SUPER_PUTVALUE( nWA, nField, xValue )

   // Log changes only

   IF !xOldValue == xValue
      ToLog( "PUTVALUE", nWA, nField, xValue, xOldValue )
   ENDIF

   RETURN nResult

STATIC FUNCTION LOGRDD_ZAP( nWA )

   LOCAL nResult

   IF ( nResult := UR_SUPER_ZAP( nWA ) ) == HB_SUCCESS
      ToLog( "ZAP", nWA )
   ENDIF

   RETURN nResult

/* Force linking DBFCDX from which our RDD inherits */

REQUEST DBFCDX

/*
 * This function have to exist in all RDD and then name have to be in
 * format: <RDDNAME>_GETFUNCTABLE
 */

FUNCTION LOGRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, pSuperRddID )

   LOCAL cSuperRDD := hb_LogRddInherit() /* We are inheriting from a User Defined RDD */
   LOCAL aMyFunc[UR_METHODCOUNT]

   s_nRddID := nRddID

   aMyFunc[UR_INIT] := @LOGRDD_INIT()
   aMyFunc[UR_EXIT] := @LOGRDD_EXIT()
   aMyFunc[UR_CREATE] := @LOGRDD_CREATE()
   aMyFunc[UR_CREATEFIELDS] := @LOGRDD_CREATEFIELDS()
   aMyFunc[UR_OPEN] := @LOGRDD_OPEN()
   aMyFunc[UR_CLOSE] := @LOGRDD_CLOSE()
   aMyFunc[UR_APPEND] := @LOGRDD_APPEND()
   aMyFunc[UR_DELETE] := @LOGRDD_DELETE()
   aMyFunc[UR_RECALL] := @LOGRDD_RECALL()
   aMyFunc[UR_PUTVALUE] := @LOGRDD_PUTVALUE()
   aMyFunc[UR_ZAP] := @LOGRDD_ZAP()

   RETURN USRRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, ;
      cSuperRDD, aMyFunc, pSuperRddID )

INIT PROCEDURE _LOGRDD_INIT()

   rddRegister( "LOGRDD", RDT_FULL )

   RETURN

/* User utility functions */

FUNCTION hb_LogRddLogFileName( cFileName )

   LOCAL aRDDData
   LOCAL cOldFileName

   IF s_nRddID >= 0

      aRDDData := USRRDD_RDDDATA(s_nRddID)

      cOldFileName := aRDDData[ARRAY_FILENAME]

      IF hb_IsString(cFileName)
         aRDDData[ARRAY_FILENAME] := cFileName
      ENDIF
   ENDIF

   RETURN cOldFileName

FUNCTION hb_LogRddTag( cTag )

   LOCAL aRDDData
   LOCAL cOldTag

   IF s_nRddID >= 0

      aRDDData := USRRDD_RDDDATA(s_nRddID)

      cOldTag := aRDDData[ARRAY_TAG]

      IF hb_IsString(cTag)
         aRDDData[ARRAY_TAG] := cTag
      ENDIF
   ENDIF

   RETURN cOldTag

FUNCTION hb_LogRddActive( lActive )

   LOCAL aRDDData
   LOCAL lOldActive

   IF s_nRddID >= 0

      aRDDData := USRRDD_RDDDATA(s_nRddID)

      lOldActive := aRDDData[ARRAY_ACTIVE]

      IF hb_IsLogical(lActive)
         aRDDData[ARRAY_ACTIVE] := lActive
      ENDIF
   ENDIF

   RETURN lOldActive

FUNCTION hb_LogRddMsgLogBlock( bMsgLogBlock )

   LOCAL aRDDData
   LOCAL bOldMsgLogBlock

   IF s_nRddID >= 0

      aRDDData := USRRDD_RDDDATA(s_nRddID)

      bOldMsgLogBlock := aRDDData[ARRAY_MSGLOGBLOCK]

      IF hb_IsEvalItem(bMsgLogBlock)
         aRDDData[ARRAY_MSGLOGBLOCK] := bMsgLogBlock
      ENDIF
   ENDIF

   RETURN bOldMsgLogBlock

FUNCTION hb_LogRddUserLogBlock( bUserLogBlock )

   LOCAL aRDDData
   LOCAL bOldUserLogBlock

   IF s_nRddID >= 0

      aRDDData := USRRDD_RDDDATA(s_nRddID)

      bOldUserLogBlock := aRDDData[ARRAY_MSGLOGBLOCK]

      IF hb_IsEvalItem(bUserLogBlock)
         aRDDData[ARRAY_USERLOGBLOCK] := bUserLogBlock
      ENDIF
   ENDIF

   RETURN bOldUserLogBlock

FUNCTION hb_LogRddValueToText( uValue )

   LOCAL cType
   LOCAL cText

   SWITCH cType := ValType(uValue)
   CASE "C"  ; cText := hb_StrToExp( uValue ) ; EXIT
   CASE "N"  ; cText := hb_ntos(uValue) ; EXIT
   CASE "D"  ; cText := DToS( uValue ) ; cText := "0d" + IIf(Empty(cText), "00000000", cText) ; EXIT
   OTHERWISE ; cText := hb_ValToStr(uValue)
   ENDSWITCH

   RETURN "[" + cType + "]>>>" + cText + "<<<"

/* Local utility functions */

STATIC PROCEDURE OpenLogFile( nWA )

   LOCAL aRDDData  := USRRDD_RDDDATA(USRRDD_ID( nWA ))
   LOCAL cFileName := aRDDData[ARRAY_FILENAME]
   LOCAL hFile     := aRDDData[ARRAY_FHANDLE]
   LOCAL lActive   := aRDDData[ARRAY_ACTIVE]

#if 0
   TraceLog( "hFile " + CStr(hFile) )
#endif

   IF lActive .AND. hFile == NIL

      /* Open Access Log File */
      IF hb_FileExists( cFileName )
         hFile := FOpen( cFileName, FO_READWRITE + FO_SHARED )
      ELSE
         hFile := FCreate( cFileName )
         /* Close and reopen in shared mode */
         IF FError() == 0 .AND. hFile != F_ERROR
            FClose( hFile )
            hFile := FOpen( cFileName, FO_READWRITE + FO_SHARED )
         ENDIF
      ENDIF
      IF FError() == 0 .AND. hFile != F_ERROR
         /* Move to end of file */
         FSeek( hFile, 0, FS_END )
      ELSE
         hFile := NIL
      ENDIF

      aRDDData[ARRAY_FHANDLE] := hFile
   ENDIF

   RETURN

STATIC FUNCTION ToString( cCmd, nWA, xPar1, xPar2, xPar3 )

   SWITCH cCmd
   CASE "CREATE"
      // Parameters received: xPar1: aOpenInfo
      RETURN xPar1[UR_OI_NAME]
   CASE "CREATEFIELDS"
      // Parameters received: xPar1: aStruct
      RETURN hb_ValToExp( xPar1 )
   CASE "OPEN"
      // Parameters received: xPar1: aOpenInfo
      RETURN 'Table: "' + xPar1[UR_OI_NAME] + '", Alias: "' + Alias() + '", WorkArea: ' + hb_ntos(nWA)
   CASE "CLOSE"
      // Parameters received: xPar1: cTableName, xPar2: cAlias
      RETURN 'Table: "' + xPar1 + '", Alias: "' + xPar2 + '", WorkArea: ' + hb_ntos(nWA)
   CASE "APPEND"
      // Parameters received: xPar1: lUnlockAll
      RETURN Alias() + "->RecNo() == " + hb_ntos(RecNo())
   CASE "DELETE"
      // Parameters received: none
      RETURN Alias() + "->RecNo() == " + hb_ntos(RecNo())
   CASE "RECALL"
      // Parameters received: none
      RETURN Alias() + "->RecNo() == " + hb_ntos(RecNo())
   CASE "PUTVALUE"
      // Parameters received: xPar1: nField, xPar2: xValue, xPar3: xOldValue
      HB_SYMBOL_UNUSED(xPar3) // Here don't log previous value
      RETURN Alias() + "(" + hb_ntos(RecNo()) + ")->" + PadR(FieldName( xPar1 ), 10) + " := " + hb_LogRddValueToText( xPar2 )
   CASE "ZAP"
      // Parameters received: none
      RETURN 'Alias: "' + Alias() + ' Table: "' + dbInfo( DBI_FULLPATH ) + '"'
   ENDSWITCH

   RETURN NIL

STATIC PROCEDURE ToLog( cCmd, nWA, xPar1, xPar2, xPar3 )

   LOCAL aRDDData := USRRDD_RDDDATA(USRRDD_ID( nWA ))
   LOCAL lActive  := aRDDData[ARRAY_ACTIVE]
   LOCAL hFile, cTag, cRDDName, bMsgLogBlock, bUserLogBlock, cLog

   // Check if logging system is active

   IF lActive

      cTag          := aRDDData[ARRAY_TAG]
      cRDDName      := aRDDData[ARRAY_RDDNAME]
      bUserLogBlock := aRDDData[ARRAY_USERLOGBLOCK]

      // If not defined a User codeblock
      IF !hb_IsEvalItem(bUserLogBlock)

         hFile := aRDDData[ARRAY_FHANDLE]

         // If log file is not already open I open now
         IF hFile == NIL
            OpenLogFile( nWA )
         ENDIF

         IF hFile != NIL

            bMsgLogBlock := aRDDData[ARRAY_MSGLOGBLOCK]

            // If defined a codeblock I send to user infos and he has to return a formatted string
            // Look at local ToString() function for details
            IF hb_IsEvalItem(bMsgLogBlock)
               cLog := Eval(bMsgLogBlock, cTag, cRDDName, cCmd, nWA, xPar1, xPar2, xPar3)
            ELSE
               cLog := DToS( Date() ) + " " + Time() + " " + cTag + ": " + PadR(cRDDName + "_" + cCmd, 20) + " - " + ToString( cCmd, nWA, xPar1, xPar2, xPar3 )
            ENDIF
            // Log to file only if cLog is a valid string
            IF hb_IsString(cLog)
               FWrite( hFile, cLog + hb_eol() )
            ENDIF
         ENDIF
      ELSE
         // Otherwise I send all to user that is responsible to log everywhere
         Eval(bUserLogBlock, cTag, cRDDName, cCmd, nWA, xPar1, xPar2, xPar3)
      ENDIF
   ENDIF

   RETURN
