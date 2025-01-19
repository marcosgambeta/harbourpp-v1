/*
 * SQLite3 JDBC like interface code.
 *
 * Copyright 2008 Lorenzo Fiorini lorenzo.fiorini@gmail.com
 *
 */

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

#include "hbclass.ch"
#include "error.ch"
#include "hbsqlit3.ch"

#define _TODO_ NIL

CREATE CLASS hdbcSQLTConnection

   PROTECTED:

   VAR pDb
   VAR lTrans
   VAR lTrace INIT .F.
   VAR pTrace

   EXPORTED:

   METHOD new(cDBFile, lCreateIfNotExist)
   METHOD close()

   METHOD startTransaction()
   /* method   transactionStatus */
   METHOD commit()
   METHOD rollback()

   METHOD getMetadata()

   METHOD createStatement()
   METHOD prepareStatement(cSql)

ENDCLASS

METHOD hdbcSQLTConnection:new(cDBFile, lCreateIfNotExist)

   ::pDB := sqlite3_open(cDbFile, lCreateIfNotExist)

   IF sqlite3_errcode(::pDb) != SQLITE_OK
      raiseError(sqlite3_errmsg(::pDb))
   ENDIF

   RETURN SELF

METHOD hdbcSQLTConnection:close()

   ::pDb := NIL

   RETURN NIL

METHOD hdbcSQLTConnection:startTransaction()

   IF sqlite3_exec(::pDB, "BEGIN TRANSACTION") != SQLITE_OK
      raiseError(sqlite3_errmsg(::pDb))
   ENDIF

   RETURN NIL

METHOD hdbcSQLTConnection:commit()

   IF sqlite3_exec(::pDB, "COMMIT") != SQLITE_OK
      raiseError(sqlite3_errmsg(::pDb))
   ENDIF

   RETURN NIL

METHOD hdbcSQLTConnection:rollback()

   IF sqlite3_exec(::pDB, "ROLLBACK") != SQLITE_OK
      raiseError(sqlite3_errmsg(::pDb))
   ENDIF

   RETURN NIL

METHOD hdbcSQLTConnection:createStatement()

   RETURN hdbcSQLTStatement():new(::pDB)

METHOD hdbcSQLTConnection:prepareStatement(cSql)

   RETURN hdbcSQLTPreparedStatement():new(::pDB, cSql)

METHOD hdbcSQLTConnection:getMetadata()

   RETURN hdbcSQLTDatabaseMetaData():new(::pDB)

CREATE CLASS hdbcSQLTStatement

   PROTECTED:

   VAR pDB
   VAR cSql
   VAR oRs

   EXPORTED:

   VAR pRes

   METHOD new(pDB, cSql)
   METHOD executeQuery(cSql)
   METHOD executeUpdate(cSql)
   METHOD close()

ENDCLASS

METHOD hdbcSQLTStatement:new(pDB, cSql)

   ::pDB := pDB
   ::cSql := cSql

   RETURN SELF

METHOD hdbcSQLTStatement:executeQuery(cSql)

   ::pRes := sqlite3_prepare(::pDB, cSql)

   IF !HB_IsPointer(::pRes)
      raiseError(sqlite3_errmsg(::pDb))
   ELSE
      ::oRs := hdbcSQLTResultSet():new(::pDB, SELF)
   ENDIF

   return ::oRs

METHOD hdbcSQLTStatement:executeUpdate(cSql)

   LOCAL nRows

   IF sqlite3_exec(::pDB, cSql) != SQLITE_OK
      raiseError(sqlite3_errmsg(::pDb))
   ELSE
      nRows := sqlite3_changes(::pDB)
   ENDIF

   RETURN nRows

METHOD hdbcSQLTStatement:close()

   IF ::pRes != NIL

      sqlite3_finalize(::pRes)

      ::pRes := NIL

   ENDIF

   RETURN NIL

CREATE CLASS hdbcSQLTPreparedStatement

   PROTECTED:

   VAR pDB
   VAR cSql
   VAR pRes
   VAR oRs
   VAR cName INIT "hdbcsqle"

   VAR lPrepared INIT .F.
   VAR nParams INIT 0
   VAR aParams INIT Array(128)

   EXPORTED:

   METHOD new(pDB, cSql)
   METHOD executeQuery()
   METHOD executeUpdate()
   METHOD close()

   METHOD setString(nParam, xValue)
   METHOD SetNumber(n, x) INLINE ::setString(n, Str(x))
   METHOD SetDate(n, x) INLINE ::setString(n, DToS(x))
   METHOD SetBoolean(n, x) INLINE ::setString(n, iif(x, "t", "f"))

ENDCLASS

METHOD hdbcSQLTPreparedStatement:new(pDB, cSql)

   ::pDB := pDB
   ::cSql := cSql

   RETURN SELF

METHOD hdbcSQLTPreparedStatement:executeQuery()

   IF !::lPrepared
      ::aParams := ASize(::aParams, ::nParams)
      /* TODO */
   ENDIF

   if ::lPrepared
      /* TODO */
   ENDIF

   RETURN _TODO_

METHOD hdbcSQLTPreparedStatement:executeUpdate()

   IF !::lPrepared
      ::aParams := ASize(::aParams, ::nParams)
      /* TODO */
   ENDIF

   if ::lPrepared
      /* TODO */
   ENDIF

   RETURN _TODO_

METHOD hdbcSQLTPreparedStatement:setString(nParam, xValue)

   ::aParams[nParam] := xValue

   IF !::lPrepared
      IF nParam > ::nParams
         ::nParams := nParam
      ENDIF
   ENDIF

   RETURN NIL

METHOD hdbcSQLTPreparedStatement:close()

   IF !Empty(::pRes)

      sqlite3_finalize(::pRes)

      ::pRes := NIL

   ENDIF

   RETURN NIL

CREATE CLASS hdbcSQLTResultSet

   PROTECTED:

   VAR pDB
   VAR pStmt
   VAR pRes

   VAR lBeforeFirst INIT .T.
   VAR lAfterLast INIT .F.

   VAR nRow INIT 0

   VAR cTableName
   VAR aPrimaryKeys
   VAR cPrimaryWhere
   VAR aBuffer
   VAR nCurrentRow
   VAR hColNames

   EXPORTED:

   VAR nRows INIT 0

   METHOD new(pDB, pStmt)
   METHOD close()

   METHOD beforeFirst()
   METHOD first() INLINE ::absolute(1)
   METHOD previous() INLINE ::relative(-1)
   METHOD next() INLINE (sqlite3_step(::pRes) == SQLITE_ROW) // ::relative(1)
   METHOD last() INLINE ::absolute(::nRows)
   METHOD afterLast()

   METHOD relative(nMove)
   METHOD absolute(nMove)

   METHOD isBeforeFirst() INLINE ::lBeforeFirst
   METHOD isFirst() INLINE (::nRow == 1)
   METHOD isLast() INLINE (::nRow == ::nRows)
   METHOD isAfterLast() INLINE ::lAfterLast
   METHOD getRow() INLINE ::nRow
   METHOD findColumn(cField)

   METHOD getString(nField)
   METHOD getNumber(nField) INLINE Val(::getString(nField))
   METHOD getDate(nField) INLINE hb_SToD(StrTran(::getString(nField), "-"))
   METHOD getBoolean(nField) INLINE (::getString(nField) == "t")

   METHOD getMetaData()

   METHOD setTableName(cTable) INLINE ::cTableName := cTable
   METHOD setPrimaryKeys(aKeys) INLINE ::aPrimaryKeys := aKeys

   METHOD moveToInsertRow()
   METHOD moveToCurrentRow()
   METHOD insertRow()
   METHOD updateRow()
   METHOD deleteRow()

   METHOD updateBuffer(nField, xValue, cType)
   METHOD updateString(nField, cValue) INLINE ::updateBuffer(nField, cValue, "C")
   METHOD updateNumber(nField, nValue) INLINE ::updateBuffer(nField, hb_ntos(nValue), "N")
   METHOD updateDate(nField, dValue) INLINE ::updateBuffer(nField, DToS(dValue), "D")
   METHOD updateBoolean(nField, lValue) INLINE ::updateBuffer(nField, iif(lValue, "t", "f"), "L")

ENDCLASS

METHOD hdbcSQLTResultSet:new(pDB, pStmt)

   ::pDB := pDB
   ::pStmt := pStmt
   ::pRes := pStmt:pRes /* FIXME ! */

   ::nRows := 100

   if ::nRows != 0
      ::nRow := 0
      ::lBeforeFirst := .T.
      ::lAfterLast := .F.
   ENDIF

   RETURN SELF

METHOD hdbcSQLTResultSet:close()

   RETURN NIL

METHOD hdbcSQLTResultSet:beforeFirst()

   ::nRow := 0
   ::lBeforeFirst := .T.
   ::lAfterLast := .F.

   RETURN NIL

METHOD hdbcSQLTResultSet:afterLast()

   ::nRow := ::nRows + 1
   ::lBeforeFirst := .F.
   ::lAfterLast := .T.

   RETURN NIL

METHOD hdbcSQLTResultSet:relative(nMove)

   LOCAL nRowNew := ::nRow + nMove

   IF nRowNew >= 1 .AND. nRowNew <= ::nRows

      ::nRow := nRowNew
      ::lBeforeFirst := .F.
      ::lAfterLast := .F.

      RETURN .T.

   ELSE

      IF nRowNew < 1
         ::nRow := 0
         ::lBeforeFirst := .T.
      ELSE
         ::nRow := ::nRows + 1
         ::lAfterLast := .T.
      ENDIF

   ENDIF

   RETURN .F.

METHOD hdbcSQLTResultSet:absolute(nMove)

   IF nMove > 0
      IF nMove <= ::nRows
         ::nRow := nMove
         ::lBeforeFirst := .F.
         ::lAfterLast := .F.
         RETURN .T.
      ENDIF
   ELSEIF nMove < 0
      IF -nMove <= ::nRows
         ::nRow := ::nRows + nMove
         ::lBeforeFirst := .F.
         ::lAfterLast := .F.
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

METHOD hdbcSQLTResultSet:findColumn(cField)

   LOCAL nCount
   LOCAL nMax

   IF !HB_ISHASH(::hColNames)
      ::hColNames := {=>}
      nMax := sqlite3_column_count(::pRes)
      FOR nCount := 1 TO nMax
         ::hColNames[Lower(sqlite3_column_name(::pRes, nCount))] := nCount
      NEXT
   ENDIF

   nCount := ::hColNames[cField]

   RETURN nCount

METHOD hdbcSQLTResultSet:getString(nField)

   IF HB_IsString(nField)
      nField := ::findColumn(nField)
   ENDIF

   RETURN sqlite3_column_text(::pRes, nField)

METHOD hdbcSQLTResultSet:getMetaData()

   RETURN hdbcSQLTResultSetMetaData():new(::pRes)

METHOD hdbcSQLTResultSet:moveToInsertRow()

   ::nCurrentRow := ::nRow

   ::aBuffer := Array(_TODO_)

   RETURN NIL

METHOD hdbcSQLTResultSet:moveToCurrentRow()

   ::nRow := ::nCurrentRow

   RETURN NIL

METHOD hdbcSQLTResultSet:updateBuffer(nField, xValue, cType)

   IF HB_IsString(nField)
      nField := ::findColumn(nField)
   ENDIF

   if ::aBuffer == NIL
      ::aBuffer := Array(_TODO_)
   ENDIF

   ::aBuffer[nField] := {xValue, cType}

   RETURN NIL

METHOD hdbcSQLTResultSet:insertRow()

   /* TODO */

   RETURN NIL

METHOD hdbcSQLTResultSet:updateRow()

   /* TODO */

   RETURN NIL

METHOD hdbcSQLTResultSet:deleteRow()

   /* TODO */

   RETURN NIL

CREATE CLASS hdbcSQLTResultSetMetaData

   PROTECTED:

   VAR pRes

   EXPORTED:

   METHOD new(pRes)
   METHOD getColumnCount()
   METHOD getColumnName(nColumn)
   METHOD getColumnDisplaySize(nColumn)

ENDCLASS

METHOD hdbcSQLTResultSetMetaData:new(pRes)

   ::pRes := pRes

   RETURN SELF

METHOD hdbcSQLTResultSetMetaData:getColumnCount()

   RETURN sqlite3_column_count(::pRes)

METHOD hdbcSQLTResultSetMetaData:getColumnName(nColumn)

   RETURN sqlite3_column_name(::pRes, nColumn)

METHOD hdbcSQLTResultSetMetaData:getColumnDisplaySize(nColumn)

   HB_SYMBOL_UNUSED(nColumn)

   RETURN _TODO_

CREATE CLASS hdbcSQLTDatabaseMetaData

   PROTECTED:

   VAR pDB

   EXPORTED:

   METHOD new(pDB)
   METHOD getTables()
   METHOD getPrimaryKeys()

ENDCLASS

METHOD hdbcSQLTDatabaseMetaData:new(pDB)

   ::pDB := pDB

   RETURN SELF

METHOD hdbcSQLTDatabaseMetaData:getTables()

   /* TODO */

   RETURN _TODO_

METHOD hdbcSQLTDatabaseMetaData:getPrimaryKeys()

   /* TODO */

   RETURN _TODO_

STATIC PROCEDURE raiseError(cErrMsg)

   LOCAL oErr

   oErr := ErrorNew()
   oErr:severity := ES_ERROR
   oErr:genCode := EG_OPEN
   oErr:subSystem := "HDBCSQLT"
   oErr:SubCode := 1000
   oErr:Description := cErrMsg

   Eval(ErrorBlock(), oErr)

   RETURN
