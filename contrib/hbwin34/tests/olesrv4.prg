//
// Demonstration/test code for OLE server returning to client
// HVM objects as OLE object. It's also test for parameters
// passed by reference.
//
// Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
//

#define CLS_Name  "MyOleObjServer"
#define CLS_ID    "{23245C3F-4487-404B-985F-E33886698D23}"

#include <hbclass.ch>
#include <hbver.ch>

PROCEDURE DllMain()

   win_oleServerInit( CLS_ID, CLS_Name, OleObjServer():new() )

   RETURN


CREATE CLASS OleObjServer

   METHOD timer
   METHOD info
   METHOD ref

ENDCLASS

METHOD OleObjServer:timer()
   RETURN timerCls():new()

METHOD OleObjServer:info()
   RETURN infoCls():new()

METHOD OleObjServer:ref( p1, p2, p3, p4, p5, p6, p7 )

   p1 := Date()
   p2 := hb_DateTime()
   p3 := .T.
   p4 := { "A", "B", "C" }
   p5 := timerCls():new()
   p6 := 123.456
   p7 := "text"

   RETURN "DONE"


CREATE CLASS timerCls

   MESSAGE date     EXTERN Date()
   MESSAGE time     EXTERN Time()
   MESSAGE now      EXTERN hb_DateTime()

ENDCLASS

CREATE CLASS infoCls

   MESSAGE os       EXTERN OS()
   MESSAGE ver      EXTERN Version()
   MESSAGE compiler EXTERN hb_Compiler()
   MESSAGE build    EXTERN hb_Version( HB_VERSION_BUILD_DATE_STR )

ENDCLASS

ANNOUNCE GT_SYS
REQUEST HB_GT_GUI_DEFAULT
