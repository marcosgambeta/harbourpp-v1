// Copyright 2009 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>

#include <hbclass.ch>

#pragma -km+

MEMVAR session, server, get, post

// ---

CREATE CLASS UWMain

   VAR aChilds     INIT {}

   METHOD Add(oWidget)
   METHOD Paint()

ENDCLASS

FUNCTION UWMainNew()

   LOCAL oW := UWMain()

   session["_uthis"]["main"] := oW

   RETURN oW

METHOD UWMain:Paint()

   UWrite( '<html><link href="/files/main.css" type=text/css rel=stylesheet>' )
   UWrite( '<meta http-equiv="content-type" content="text/html; charset=UTF-8">' )
   UWrite( '<script language="javascript" src="/files/main.js"></script>' )
   UWrite( '<body>' )
   AEval(::aChilds, {| x | x:Paint() })
   UWrite( '</body></html>' )

   RETURN Self

METHOD UWMain:Add(oWidget)

   AAdd(::aChilds, oWidget)

   RETURN Self

// ---

CREATE CLASS UWLayoutGrid

   VAR aChilds     INIT { { {} } }     // {{{}}, {{}}} ;   {{{}, {}}}

   METHOD Add(oWidget, nRow, nCol)
   METHOD Paint()

ENDCLASS

FUNCTION UWLayoutGridNew()
   RETURN UWLayoutGrid()

METHOD UWLayoutGrid:Paint()

   LOCAL aRow, aCell

   UWrite( '<table>' )
   FOR EACH aRow IN ::aChilds
      UWrite( '<tr>' )
      FOR EACH aCell IN aRow
         UWrite( '<td>' )
         AEval(aCell, {| o | o:Paint() })
         UWrite( '</td>' )
      NEXT
      UWrite( '</tr>' )
   NEXT
   UWrite( '</table>' )

   RETURN Self

METHOD UWLayoutGrid:Add(oWidget, nRow, nCol)

   LOCAL nI, nJ, aI

   IF nRow > Len(::aChilds)
      FOR nI := Len(::aChilds) + 1 TO nRow
         aI := Array( Len(::aChilds[1]) )
         FOR nJ := 1 TO Len(::aChilds[1])
            aI[nJ] := {}
         NEXT
         AAdd(::aChilds, aI)
      NEXT
   ENDIF
   IF nCol > Len(::aChilds[1])
      FOR nI := Len(::aChilds[1]) + 1 TO nCol
         AEval(::aChilds, {| x | AAdd(x, {}) })
      NEXT
   ENDIF
   AAdd(::aChilds[nRow][nCol], oWidget)

   RETURN Self

// ---

CREATE CLASS UWHtml

   VAR cText

   METHOD Paint()

ENDCLASS

FUNCTION UWHtmlNew( cText )

   LOCAL oW := UWHtml()

   oW:cText := cText

   RETURN oW

METHOD UWHtml:Paint()

   UWrite( ::cText )

   RETURN Self

// ---

CREATE CLASS UWLabel

   VAR cText
   VAR cID
   VAR cStyle

   METHOD Paint()

ENDCLASS

FUNCTION UWLabelNew( cText, cID, cStyle )

   LOCAL oW := UWLabel()

   oW:cText := cText
   SetWId(oW, cID)
   oW:cStyle := cStyle

   RETURN oW

METHOD UWLabel:Paint()

   UWrite( '<div' + IIf(::cID != NIL, ' id="' + ::cID + '"', "") + ;
      IIf(::cStyle != NIL, ' style="' + ::cStyle + '"', "") + '>' + ;
      UHtmlEncode( ::cText ) + '</span>' )

   RETURN Self

// ---

CREATE CLASS UWForm

   VAR cAction
   VAR cMethod   INIT "POST"
   VAR aChilds   INIT {}

   METHOD Add(oWidget)
   METHOD Paint()

ENDCLASS

FUNCTION UWFormNew( cAction )

   LOCAL oW := UWForm()

   oW:cAction := cAction

   RETURN oW

METHOD UWForm:Add(oWidget)

   AAdd(::aChilds, oWidget)

   RETURN Self

METHOD UWForm:Paint()

   UWrite( '<form action="' + ::cAction + '" method="' + ::cMethod + '">' )
   AEval(::aChilds, {| x | x:Paint() })
   UWrite( '</form>' )

   RETURN Self

// ---

CREATE CLASS UWInput

   VAR cName
   VAR cValue
   VAR cID
   VAR cStyle

   METHOD Paint()

ENDCLASS

FUNCTION UWInputNew( cName, cValue, cID, cStyle )

   LOCAL oW := UWInput()

   oW:cName := cName
   oW:cValue := cValue
   SetWId(oW, cID)
   oW:cStyle := cStyle

   RETURN oW

METHOD UWInput:Paint()

   UWrite( '<input type="text" name="' + IIf(::cName != NIL, ::cName, "") + ;
      '" value="' + IIf(::cValue != NIL, UHtmlEncode( ::cValue ), "") + '">' )

   RETURN Self

// ---

CREATE CLASS UWPassword

   VAR cName
   VAR cValue

   METHOD Paint()

ENDCLASS

FUNCTION UWPasswordNew( cName )

   LOCAL oW := UWPassword()

   oW:cName := cName

   RETURN oW

METHOD UWPassword:Paint()

   UWrite( '<input type="password" name="' + IIf(::cName != NIL, ::cName, "") + ;
      '" value="' + IIf(::cValue != NIL, ::cValue, "") + '">' )

   RETURN Self

// ---

CREATE CLASS UWSubmit

   VAR cName
   VAR cValue

   METHOD Paint()

ENDCLASS

FUNCTION UWSubmitNew( cName, cValue )

   LOCAL oW := UWSubmit()

   oW:cName := cName
   oW:cValue := cValue

   RETURN oW

METHOD UWSubmit:Paint()

   UWrite( '<input type="submit" name="' + IIf(::cName != NIL, ::cName, "") + ;
      '" value="' + IIf(::cValue != NIL, UHtmlEncode( ::cValue ), "") + '">' )

   RETURN Self

// ---

CREATE CLASS UWSeparator

   METHOD Paint()

ENDCLASS

FUNCTION UWSeparatorNew()
   RETURN UWSeparator()

METHOD UWSeparator:Paint()

   UWrite( '<hr>' )

   RETURN Self

// ---

CREATE CLASS UWMenu

   VAR aItems    INIT {}

   METHOD AddItem( cTitle, cLink )
   METHOD Paint()

ENDCLASS

FUNCTION UWMenuNew()
   RETURN UWMenu()

METHOD UWMenu:AddItem( cTitle, cLink )

   AAdd(::aItems, { cTitle, cLink })

   RETURN Self

METHOD UWMenu:Paint()

   LOCAL nI

   UWrite( '<div>' )
   FOR nI := 1 TO Len(::aItems)
      IF nI != 1
         UWrite( '&nbsp;|&nbsp;' )
      ENDIF
      UWrite( '<a href="' + ::aItems[nI][2] + '">' + UHtmlEncode( ::aItems[nI][1] ) + '</a>' )
   NEXT
   UWrite( '</div>' )

   RETURN Self

// ---

CREATE CLASS UWBrowse

   VAR aColumns   INIT {}
   VAR nPageSize  INIT 0
   VAR nPos       INIT 0

   METHOD AddColumn( nID, cTitle, cField, lRaw )
   METHOD Output()

ENDCLASS

FUNCTION UWBrowseNew()
   RETURN UWBrowse()

METHOD UWBrowse:AddColumn( nID, cTitle, cField, lRaw )

   AAdd(::aColumns, { nID, cTitle, cField, ! Empty(lRaw) })

   RETURN Self

METHOD UWBrowse:Output()

   LOCAL cRet := "", nI, xI, xField, nPos, cUrl, cI, lValidate

   cRet += '<table class="ubr"><tr>'

   // Header
   cRet += '<tr>'
   FOR nI := 1 TO Len(::aColumns)
      cRet += '<th>' + UHtmlEncode( ::aColumns[nI][2] ) + '</th>'
   NEXT
   cRet += '</tr>'

   // Body
   nPos := 0
   dbGoTop()
   IF ::nPageSize > 0 .AND. ::nPos > 0
      dbSkip( ::nPos )
   ENDIF
   DO WHILE !Eof()
      cRet += '<tr>'
      FOR nI := 1 TO Len(::aColumns)
         xField := ::aColumns[nI][3]
         DO CASE
         CASE hb_IsString(xField)
            xI := FieldGet( FieldPos( xField ) )
         CASE hb_IsEvalItem(xField)
            xI := Eval(xField)
         ENDCASE
         SWITCH ValType(xI)
         CASE "C"  ; xI := RTrim(xI); EXIT
         CASE "N"  ; xI := Str(xI); EXIT
         CASE "D"  ; xI := DToC(xI); EXIT
         OTHERWISE ; xI := "ValType()==" + ValType(xI)
         ENDSWITCH
         IF !::aColumns[nI][4]
            xI := UHtmlEncode( xI )
         ENDIF
         cRet += '<td><nobr>' + xI + '</nobr></td>'
      NEXT
      cRet += '</tr>'
      dbSkip()
      IF ++nPos >= ::nPageSize
         EXIT
      ENDIF
   ENDDO
   cRet += '</table>'
   IF !Eof() .OR. ::nPos > 0
      cUrl := server["REQUEST_URI"]
      IF ( nI := At( "?_ucs=", cUrl ) ) == 0
         nI := At( "&_ucs=", cUrl )
      ENDIF
      IF ( lValidate := nI > 0 )
         cUrl := Left(cUrl, nI - 1)
      ENDIF
      IF ( nI := At( "?_pos=", cUrl ) ) == 0
         nI := At( "&_pos=", cUrl )
      ENDIF
      IF nI > 0
         cUrl := Left(cUrl, nI - 1)
      ENDIF
      cUrl += IIf("?" $ cUrl, "&", "?") + "_pos="
      cRet := '<br />' + cRet
      IF !Eof()
         cI := cUrl + hb_ntos(::nPos + ::nPageSize)
         cRet := '<a href="' + IIf(lValidate, UUrlChecksum( cI ), cI) + '">&gt;&gt;</a>' + cRet
      ENDIF
      IF ::nPos > 0
         cI := cUrl + hb_ntos(Max(0, ::nPos - ::nPageSize))
         cRet := '<a href="' + IIf(lValidate, UUrlChecksum( cI ), cI) + '">&lt;&lt;</a>&nbsp;&nbsp;' + cRet
      ENDIF
   ENDIF

   RETURN cRet

// ---

CREATE CLASS UWOption

   VAR aOption   INIT {}
   VAR cValue

   METHOD Add(cTitle, cCode, lRaw)
   METHOD Output()

ENDCLASS

FUNCTION UWOptionNew()
   RETURN UWOption()

METHOD UWOption:Add(cTitle, cCode, lRaw)

   AAdd(::aOption, { IIf(Empty(lRaw), UHtmlEncode( cTitle ), cTitle), cCode })

   RETURN Self

METHOD UWOption:Output()

   LOCAL cRet := ""

   AEval(::aOption, {| X | cRet += hb_StrFormat( '<option value="%s"%s>%s</option>', UHtmlEncode( X[2] ), IIf(X[2] == ::cValue, " selected", ""), X[1] ) })

   RETURN cRet

/* Default procedure handlers */

PROCEDURE UProcWidgets( cURL, aMap )

   LOCAL aStack, aURL, aFrame, cI, nI, nL, lRet

   IF hb_HHasKey(aMap, cURL)
      // aStack[i] := { url_part, function, variables }
      IF ( aStack := hb_HGetDef( session, "_ustack" ) ) == NIL
         session["_ustack"] := aStack := {}
      ENDIF

      aURL := uhttpd_split( "/", cURL )
      nI := 1
      nL := Min(Len(aURL), Len(aStack))
      DO WHILE nI <= nL
         IF aStack[nI][1] == aURL[nI]
            nI++
         ELSE
            EXIT
         ENDIF
      ENDDO

      // Exit procedures
      DO WHILE nI <= Len(aStack)
         aFrame := ATail( aStack )
         IF aFrame[2] != NIL
            session["_uthis"] := aFrame[3]
            Eval(aFrame[2], "EXIT")
            session["_uthis"] := NIL
         ENDIF
         ASize( aStack, Len(aStack) - 1 )
      ENDDO
      aFrame := NIL

      lRet := .T.
      // Enter procedures
      DO WHILE nI <= Len(aURL)
         cI := uhttpd_join( "/", ASize( AClone( aURL ), nI ) )
         IF hb_HHasKey(aMap, cI)
            session["_uthis"] := { "idhash" => { => } }
            IF ( lRet := Eval(aMap[cI], "INIT") ) == .T.
               AAdd(aStack, { aURL[nI], aMap[cI], session["_uthis"] })
               session["_uthis"] := NIL
            ELSE
               session["_uthis"] := NIL
               EXIT
            ENDIF
         ELSE
            AAdd(aStack, { aURL[nI], NIL, NIL })
         ENDIF
         nI++
      ENDDO

      IF lRet
         session["_uthis"] := ATail( aStack )[3]
         DO CASE
         CASE server["REQUEST_METHOD"] == "GET"
            Eval(ATail( aStack )[2], "GET")
         CASE server["REQUEST_METHOD"] == "POST"
            Eval(ATail( aStack )[2], "POST")
         ENDCASE
         ATail( aStack )[3] := session["_uthis"]
         session["_uthis"] := NIL
      ENDIF
   ELSE
      USetStatusCode( 404 )
   ENDIF

   RETURN

PROCEDURE UWDefaultHandler( cMethod )

   LOCAL cID, oW

   IF cMethod == "GET"
      IF ( cID := hb_HGetDef( get, "ajax" ) ) == NIL
         session["_uthis"]["main"]:Paint()
      ELSE
         IF ( oW := UGetWidgetById(cID) ) != NIL
            UAddHeader( "Content-type", "text/html; charset=UTF-8" )
            oW:Ajax( hb_HGetDef( get, "action" ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE SetWId(oW, cID)

   IF cID != NIL
      oW:cID := cID
      session["_uthis"]["idhash"][cID] := oW
   ENDIF

   RETURN

FUNCTION UGetWidgetById(cID)
   RETURN hb_HGetDef( session["_uthis"]["idhash"], cID )

STATIC FUNCTION uhttpd_split( cSeparator, cString )

   LOCAL aRet := {}
   LOCAL nI

   DO WHILE ( nI := At( cSeparator, cString ) ) > 0
      AAdd(aRet, Left(cString, nI - 1))
      cString := SubStr(cString, nI + Len(cSeparator))
   ENDDO
   AAdd(aRet, cString)

   RETURN aRet

STATIC FUNCTION uhttpd_join( cSeparator, aData )

   LOCAL cRet := ""
   LOCAL nI

   FOR nI := 1 TO Len(aData)

      IF nI > 1
         cRet += cSeparator
      ENDIF

      SWITCH ValType(aData[nI])
      CASE "C"
      CASE "M" ; cRet += aData[nI]; EXIT
      CASE "N" ; cRet += hb_ntos(aData[nI]); EXIT
      CASE "D" ; cRet += IIf(Empty(aData[nI]), "", DToC(aData[nI])); EXIT
      ENDSWITCH
   NEXT

   RETURN cRet
