//
// Demonstration/test code for OLE server which works like
// xHarbour.com OLE servers.
//
// Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
//

#define CLS_Name  "MyOleServer"
#define CLS_ID    "{466AC7B2-35D7-4509-B909-C3C2F8FDBD3C}"

PROCEDURE DllMain()

   PUBLIC Property1

   M->Property1 := "MyProperty"

   /* Initialize OLE server ID and name.
    * win_oleServerInit() should be executed from DllMain()
    */
   win_oleServerInit( CLS_ID, CLS_Name )

   RETURN


FUNCTION MyMethod(...)  /* must be a public function */
   RETURN "Hello from MyOleServer [" + hb_ValToExp( {...} ) + "]"


ANNOUNCE GT_SYS
REQUEST HB_GT_GUI_DEFAULT
