//
// OLE demo/test code
//
// Copyright 2007 Enrico Maria Giordano e.m.giordano at emagsoftware.it
// Copyright 2008-2017 Viktor Szakats
//    Exm_CDO(), Exm_OOOpen(), Exm_CreateShortcut()
// Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
//

#require "hbwin34"

#include <hbver.ch>

PROCEDURE Main()

   LOCAL cOption

   CLS

   DO WHILE .T.
      ?
      ? "Select OLE test:"
      ? "1) MS Excel"
      ? "2) MS Word"
      ? "3) MS Outlook (1)"
      ? "4) MS Outlook (2)"
      ? "5) Internet Explorer"
      ? "6) OpenOffice Calc"
      ? "7) OpenOffice Writer"
      ? "8) OpenOffice Open"
      ? "9) Send mail via CDO"
      ? "a) Read ADODB table"
      ? "b) SOAP Toolkit client"
      ? "c) PocketSOAP client"
      ? "d) Internet Explorer with callback"
      ? "e) Create shortcut"
      ? "f) HTTPS download"
      ? "q) Quit"
      ? ">", ""

      ?? cOption := Lower(hb_keyChar( Inkey(0) ))

      DO CASE
      CASE cOption == "1" ; Exm_MSExcel()
      CASE cOption == "2" ; Exm_MSWord()
      CASE cOption == "3" ; Exm_MSOutlook()
      CASE cOption == "4" ; Exm_MSOutlook2()
      CASE cOption == "5" ; Exm_IExplorer()
      CASE cOption == "6" ; Exm_OOCalc()
      CASE cOption == "7" ; Exm_OOWriter()
      CASE cOption == "8" ; Exm_OOOpen()
      CASE cOption == "9" ; Exm_CDO()
      CASE cOption == "a" ; Exm_ADODB()
      CASE cOption == "b" ; Exm_SOAP()
      CASE cOption == "c" ; Exm_PocketSOAP()
      CASE cOption == "d" ; Exm_IExplorer2()
      CASE cOption == "e" ; Exm_CreateShortcut()
      CASE cOption == "f" ; Exm_DownloadHTTPS()
      CASE cOption == "q" ; RETURN
      ENDCASE
   ENDDO

   RETURN

STATIC PROCEDURE Exm_MSExcel()

   LOCAL oExcel, oWorkBook, oWorkSheet, oAS
   LOCAL nI, nCount

   IF ( oExcel := win_oleCreateObject( "Excel.Application" ) ) != NIL

      oWorkBook := oExcel:WorkBooks:Add()

      // Enumerator test
      FOR EACH oWorkSheet IN oWorkBook:WorkSheets
         ? oWorkSheet:Name
      NEXT

      // oWorkBook:WorkSheets is a collection
      nCount := oWorkBook:WorkSheets:Count()

      // Elements of collection can be accessed using :Item() method
      FOR nI := 1 TO nCount
         ? oWorkBook:WorkSheets:Item( nI ):Name
      NEXT

      // OLE also allows to access collection elements by passing
      // indices to :Worksheets property
      FOR nI := 1 TO nCount
         ? oWorkBook:WorkSheets( nI ):Name
      NEXT

      oAS := oExcel:ActiveSheet()

      // Set font for all cells
      oAS:Cells:Font:Name := "Arial"
      oAS:Cells:Font:Size := 12

      oAS:Cells( 1, 1 ):Value := "OLE from Harbour"
      oAS:Cells( 1, 1 ):Font:Size := 16

      // oAS:Cells( 1, 1 ) is object, but oAS:Cells( 1, 1 ):Value has value of the cell
      ? "Object valtype:", ValType(oAS:Cells( 1, 1 )), "Value:", oAS:Cells( 1, 1 ):Value

      oAS:Cells( 3, 1 ):Value := "String:"
      oAS:Cells( 3, 2 ):Value := "Hello, World!"

      oAS:Cells( 4, 1 ):Value := "Numeric:"
      oAS:Cells( 4, 2 ):Value := 1234.56
      oAS:Cells( 4, 3 ):Value := oAS:Cells( 4, 2 ):Value
      oAS:Cells( 4, 4 ):Value := oAS:Cells( 4, 2 ):Value
      oAS:Cells( 4, 3 ):Value *= 2
      oAS:Cells( 4, 2 ):Value++

      oAS:Cells( 5, 1 ):Value := "Logical:"
      oAS:Cells( 5, 2 ):Value := .T.

      oAS:Cells( 6, 1 ):Value := "Date:"
      oAS:Cells( 6, 2 ):Value := Date()

      oAS:Cells( 7, 1 ):Value := "Timestamp:"
      oAS:Cells( 7, 2 ):Value := hb_DateTime()

      // Some formatting
      oAS:Columns(1):Font:Bold := .T.
      oAS:Columns(2):HorizontalAlignment := - 4152  // xlRight

      oAS:Columns(1):AutoFit()
      oAS:Columns(2):AutoFit()
      oAS:Columns(3):AutoFit()
      oAS:Columns(4):AutoFit()

      oAS:Cells( 3, 2 ):Font:ColorIndex := 3  // red

      oAS:Range( "A1:B1" ):HorizontalAlignment := 7
      oAS:Range( "A3:A7" ):Select()

      oExcel:Visible := .T.

      oExcel:Quit()
   ELSE
      ? "Error: MS Excel not available. [" + win_oleErrorText() + "]"
   ENDIF

   RETURN

STATIC PROCEDURE Exm_MSWord()

   LOCAL oWord, oText

   IF ( oWord := win_oleCreateObject( "Word.Application" ) ) != NIL

      oWord:Documents:Add()

      oText := oWord:Selection()

      oText:Text := "OLE from Harbour" + hb_eol()
      oText:Font:Name := "Arial"
      oText:Font:Size := 48
      oText:Font:Bold := .T.

      oWord:Visible := .T.
      oWord:WindowState := 1 /* Maximize */
   ELSE
      ? "Error. MS Word not available.", win_oleErrorText()
   ENDIF

   RETURN

STATIC PROCEDURE Exm_MSOutlook()

   LOCAL oOL, oList

   IF ( oOL := win_oleCreateObject( "Outlook.Application" ) ) != NIL
      oList := oOL:CreateItem( 7 /* olDistributionListItem */ )
      oList:DLName := "Distribution List"
      oList:Display(.F.)
   ELSE
      ? "Error. MS Outlook not available.", win_oleErrorText()
   ENDIF

   RETURN

STATIC PROCEDURE Exm_MSOutlook2()

   LOCAL oOL, oLista, oMail
   LOCAL i

   IF ( oOL := win_oleCreateObject( "Outlook.Application" ) ) != NIL

      oMail := oOL:CreateItem( 0 /* olMailItem */ )

      FOR i := 1 TO 10
         oMail:Recipients:Add( "Contact" + hb_ntos(i) + ;
            "<contact" + hb_ntos(i) + "@example.org>" )
      NEXT

      oLista := oOL:CreateItem( 7 /* olDistributionListItem */ )
      oLista:DLName := "Test with distribution list"
      oLista:Display(.F.)
      oLista:AddMembers( oMail:Recipients )
      oLista:Save()
      oLista:Close(0)
   ELSE
      ? "Error. MS Outlook not available.", win_oleErrorText()
   ENDIF

   RETURN

STATIC PROCEDURE Exm_IExplorer()

   LOCAL oIE

   IF ( oIE := win_oleCreateObject( "InternetExplorer.Application" ) ) != NIL
      oIE:Visible := .T.
      oIE:Navigate( hb_Version( HB_VERSION_URL_BASE ) )
   ELSE
      ? "Error. Internet Explorer not available.", win_oleErrorText()
   ENDIF

   RETURN

STATIC PROCEDURE Exm_IExplorer2()

   LOCAL oIE

   IF ( oIE := win_oleCreateObject( "InternetExplorer.Application" ) ) != NIL
      oIE:__hSink := __axRegisterHandler( oIE:__hObj, {| ... | QOut(...) } )
      oIE:Visible := .T.
      oIE:Navigate( hb_Version( HB_VERSION_URL_BASE ) )
      DO WHILE oIE:ReadyState != 4
         hb_idleSleep(0)
      ENDDO
   ELSE
      ? "Error. Internet Explorer not available.", win_oleErrorText()
   ENDIF

   RETURN

STATIC PROCEDURE Exm_OOCalc()

   LOCAL oServiceManager, oDesktop, oDoc, oSheet

   IF ( oServiceManager := win_oleCreateObject( "com.sun.star.ServiceManager" ) ) != NIL
      oDesktop := oServiceManager:createInstance( "com.sun.star.frame.Desktop" )
      oDoc := oDesktop:loadComponentFromURL( "private:factory/scalc", "_blank", 0, {} )

      oSheet := oDoc:getSheets:getByIndex(0)

      oSheet:getCellRangeByName( "A1" ):setString( "OLE from Harbour" )

      oSheet:getCellRangeByName( "A3" ):setString( "String:" )
      oSheet:getCellRangeByName( "B3" ):setString( "Hello, World!" )

      oSheet:getCellRangeByName( "A4" ):setString( "Numeric:" )
      oSheet:getCellRangeByName( "B4" ):setValue( 1234.56 )

      oSheet:getCellRangeByName( "A5" ):setString( "Logical:" )
      oSheet:getCellRangeByName( "B5" ):setValue(.T.)
      oSheet:getCellRangeByName( "B5" ):setPropertyValue( "NumberFormat", 99 ) // BOOLEAN

      oSheet:getCellRangeByName( "A6" ):setString( "Date:" )
      oSheet:getCellRangeByName( "B6" ):setValue( Date() )
      oSheet:getCellRangeByName( "B6" ):setPropertyValue( "NumberFormat", 36 ) // YYYY-MM-DD

      oSheet:getCellRangeByName( "A7" ):setString( "Timestamp:" )
      oSheet:getCellRangeByName( "B7" ):setValue( hb_DateTime() )
      oSheet:getCellRangeByName( "B7" ):setPropertyValue( "NumberFormat", 51 ) // YYYY-MM-DD HH:MM:SS

      oSheet:getCellRangeByName( "A3" ):setPropertyValue( "IsCellBackgroundTransparent", .F. )
      oSheet:getCellRangeByName( "A3" ):setPropertyValue( "CellBackColor", 255 ) // blue
      oSheet:getCellRangeByName( "B3" ):setPropertyValue( "CharColor", 255 * 256 * 256 ) // red
   ELSE
      ? "Error. OpenOffice not available.", win_oleErrorText()
   ENDIF

   RETURN

STATIC PROCEDURE Exm_OOWriter()

   LOCAL oServiceManager, oDesktop, oDoc, oText, oCursor, oTable, oRow, oCell, oRows

   IF ( oServiceManager := win_oleCreateObject( "com.sun.star.ServiceManager" ) ) != NIL
      oDesktop := oServiceManager:createInstance( "com.sun.star.frame.Desktop" )
      oDoc := oDesktop:loadComponentFromURL( "private:factory/swriter", "_blank", 0, {} )

      oText := oDoc:getText
      oCursor := oText:createTextCursor

      oText:insertString( oCursor, "OpenOffice Writer scripting from Harbour." + Chr(10), .F. )

      oText:insertString( oCursor, "This is the second line" + Chr(10), .F. )

      oTable := oDoc:createInstance( "com.sun.star.text.TextTable" )
      oTable:initialize( 2, 4 )

      oText:insertTextContent( oCursor, oTable, .F. )

      oTable:setPropertyValue( "BackTransparent", .F. )
      oTable:setPropertyValue( "BackColor", ( 255 * 256 + 255 ) * 256 + 192 )

      oRows := oTable:getRows
      oRow := oRows:getByIndex(0)
      oRow:setPropertyValue( "BackTransparent", .F. )
      oRow:setPropertyValue( "BackColor", ( 192 * 256 + 192 ) * 256 + 128 )

      oCell := oTable:getCellByName( "A1" )
      oCell:insertString( oCell:createTextCursor, "Jan", .F. )
      oCell := oTable:getCellByName( "B1" )
      oCell:insertString( oCell:createTextCursor, "Feb", .F. )
      oCell := oTable:getCellByName( "C1" )
      oCell:insertString( oCell:createTextCursor, "Mar", .F. )

      // I guess we can set text without cursor creation
      oTable:getCellByName( "D1" ):setString( "SUM" )

      oTable:getCellByName( "A2" ):setValue( 123.12 )
      oTable:getCellByName( "B2" ):setValue( 97.07 )
      oTable:getCellByName( "C2" ):setValue( 106.38 )
      oTable:getCellByName( "D2" ):setFormula("sum <A2:C2>")

      oText:insertControlCharacter( oCursor, 0, .F. )  // PARAGRAPH_BREAK

      oCursor:setPropertyValue( "CharColor", 255 )
      oText:insertString( oCursor, "Good bye!", .F. )
   ELSE
      ? "Error. OpenOffice not available.", win_oleErrorText()
   ENDIF

   RETURN

STATIC PROCEDURE Exm_OOOpen()

   LOCAL oOO_ServiceManager
   LOCAL oOO_Desktop
   LOCAL oOO_PropVal01
   LOCAL oOO_Doc

   IF ( oOO_ServiceManager := win_oleCreateObject( "com.sun.star.ServiceManager" ) ) != NIL

      oOO_Desktop := oOO_ServiceManager:createInstance( "com.sun.star.frame.Desktop" )
      oOO_PropVal01 := oOO_ServiceManager:Bridge_GetStruct( "com.sun.star.beans.PropertyValue" )
      oOO_Doc := oOO_Desktop:loadComponentFromURL( OO_ConvertToURL( hb_FNameMerge( hb_DirBase(), "test.odt" ) ), "_blank", 0, { oOO_PropVal01 } )

      ? "About to close OpenOffice"
      WAIT

      oOO_Doc:Close(.T.)
      oOO_Desktop:Terminate()
   ELSE
      ? "Error: OpenOffice not available.", win_oleErrorText()
   ENDIF

   RETURN

STATIC FUNCTION OO_ConvertToURL( cString )

   cString := StrTran(cString, "\", "/")

   // Handle UNC paths
   IF !hb_LeftEq( cString, "//" )
      cString := StrTran(cString, ":", "|")
      cString := "///" + cString
   ENDIF

   cString := StrTran(cString, " ", "%20")

   RETURN "file:" + cString

STATIC PROCEDURE Exm_CDO()  /* STARTTLS not supported by CDO */

   LOCAL oCDOMsg
   LOCAL oCDOConf

   LOCAL cFrom

   IF ( oCDOMsg := win_oleCreateObject( "CDO.Message" ) ) != NIL

      cFrom := "from@example.org"

      oCDOConf := win_oleCreateObject( "CDO.Configuration" )

      oCDOConf:Fields( "http:" + "//schemas.microsoft.com/cdo/configuration/sendusing" ):Value := 2  // cdoSendUsingPort: Send the message using SMTP over TCP/IP networking
      oCDOConf:Fields( "http:" + "//schemas.microsoft.com/cdo/configuration/smtpserver" ):Value := "smtp.example.org"
      oCDOConf:Fields( "http:" + "//schemas.microsoft.com/cdo/configuration/smtpserverport" ):Value := 465
      oCDOConf:Fields( "http:" + "//schemas.microsoft.com/cdo/configuration/smtpconnectiontimeout" ):Value := 120
      oCDOConf:Fields( "http:" + "//schemas.microsoft.com/cdo/configuration/smtpauthenticate" ):Value := 1  // cdoBasic: Basic authentication
      oCDOConf:Fields( "http:" + "//schemas.microsoft.com/cdo/configuration/sendusername" ):Value := cFrom
      oCDOConf:Fields( "http:" + "//schemas.microsoft.com/cdo/configuration/sendpassword" ):Value := "password"
      oCDOConf:Fields( "http:" + "//schemas.microsoft.com/cdo/configuration/smtpusessl" ):Value := .T.
      oCDOConf:Fields:Update()

      oCDOMsg:Configuration := oCDOConf
      oCDOMsg:BodyPart:Charset := "utf-8"
      oCDOMsg:To := "to@example.org"
      oCDOMsg:From := cFrom
      oCDOMsg:Subject := "Test message"
      oCDOMsg:TextBody := "Test message body"
      oCDOMsg:AddAttachment( hb_DirBase() + __FILE__ )

      BEGIN SEQUENCE WITH __BreakBlock()
         oCDOMsg:Send()
      RECOVER
         ? "Error: CDO send error.", win_oleErrorText()
      END SEQUENCE
   ELSE
      ? "Error: CDO subsystem not available (needs Windows XP or upper).", win_oleErrorText()
   ENDIF

   RETURN

#define adOpenForwardOnly      0
#define adOpenKeyset           1
#define adOpenDynamic          2
#define adOpenStatic           3

#define adLockReadOnly         1
#define adLockPessimistic      2
#define adLockOptimistic       3
#define adLockBatchOptimistic  4

#define adUseNone              1
#define adUseServer            2
#define adUseClient            3

STATIC PROCEDURE Exm_ADODB()

   LOCAL oRs

   IF ( oRs := win_oleCreateObject( "ADODB.Recordset" ) ) != NIL

      oRs:Open( "SELECT * FROM test ORDER BY First", ;
         "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + hb_DirBase() + "..\..\hbodbc\tests\test.mdb", ;
         adOpenForwardOnly, ;
         adLockReadOnly )

      DO WHILE !oRs:EOF
         ? oRs:Fields( "First" ):Value
         oRs:MoveNext()
      ENDDO

      oRs:Close()
   ENDIF

   RETURN

STATIC PROCEDURE Exm_SOAP()

   LOCAL oSoapClient

   IF ( oSoapClient := win_oleCreateObject( "MSSOAP.SoapClient30" ) ) != NIL

      oSoapClient:msSoapInit( "https://www.dataaccess.com/webservicesserver/textcasing.wso?WSDL" )

      ? oSoapClient:InvertStringCase( "lower UPPER" )
   ELSE
      ? "Error: SOAP Toolkit 3.0 not available.", win_oleErrorText()
   ENDIF

   RETURN

STATIC PROCEDURE Exm_PocketSOAP()

   LOCAL oHttp := win_oleCreateObject( "PocketSOAP.HTTPTransport.2" )
   LOCAL oEnvelope := win_oleCreateObject( "PocketSOAP.Envelope.2" )

   IF oHttp != NIL .AND. oEnvelope != NIL

      oEnvelope:EncodingStyle := ""
      oEnvelope:SetMethod( "InvertStringCase", "http:" + "//www.dataaccess.com/webservicesserver/" )
      oEnvelope:Parameters:Create( "sAString", "lower UPPER" )
      oHttp:Send( "https://www.dataaccess.com/webservicesserver/textcasing.wso?WSDL", oEnvelope:Serialize() )
      oEnvelope:Parse( oHttp )

      ? oEnvelope:Parameters:Item(0):Value
   ELSE
      ? "Error: PocketSOAP not available.", win_oleErrorText()
   ENDIF

   RETURN

STATIC PROCEDURE Exm_CreateShortcut()

   LOCAL oShell, oSC
   LOCAL cFileName

   IF ( oShell := win_oleCreateObject( "WScript.Shell" ) ) != NIL
      oSC := oShell:CreateShortcut( cFileName := hb_DirBase() + hb_FNameExtSet( __FILE__, ".lnk" ) )
      oSC:TargetPath := hb_ProgName()
      oSC:WorkingDirectory := hb_DirBase()
      oSC:IconLocation := '"' + hb_ProgName() + '"' + ",0"
      oSC:Save()
      ? "Created:", cFileName
   ELSE
      ? "Error: Shell not available. [" + win_oleErrorText() + "]"
   ENDIF

   RETURN

STATIC PROCEDURE Exm_DownloadHTTPS()

   LOCAL oHTTP

   IF ( oHTTP := win_oleCreateObject( "WinHttp.WinHttpRequest.5.1" ) ) != NIL
      oHTTP:Open( "GET", "https://example.org/index.html", .F. )
      oHTTP:Send()
      IF oHTTP:Status() == 200
         ? "Downloaded", hb_ntos(hb_BLen(oHTTP:responseBody)), "byte(s)"
      ENDIF
   ELSE
      ? "Error: WinHttp 5.1 not available. [" + win_oleErrorText() + "]"
   ENDIF

   RETURN
