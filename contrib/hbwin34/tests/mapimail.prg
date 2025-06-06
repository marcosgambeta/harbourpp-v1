#require "hbwin34"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL cSubject := "Test subject"
   LOCAL cBody := "Test body"
   LOCAL lMailConf := .F.
   LOCAL lFromUser := .T.
   LOCAL aSender := { "test from", "from@example.org" }
   LOCAL aDest := { { "test to", "to@example.org", WIN_MAPI_TO } }
   LOCAL aFiles := { { __FILE__, hb_FNameName( __FILE__ ) } }

   ? win_MAPISendMail( ;
      cSubject, ;                       // subject
      cBody, ;                          // message
      NIL, ;                            // type of message
      DToS( Date() ) + " " + Time(), ;  // send date
      "", ;                             // conversation ID
      lMailConf, ;                      // acknowledgment
      lFromUser, ;                      // user intervention
      aSender, ;                        // sender
      aDest, ;                          // destinators
      aFiles )                          // attach

   // simple format

   ? win_MAPISendMail( ;
      cSubject, ;                       // subject
      cBody, ;                          // message
      NIL, ;                            // type of message
      DToS( Date() ) + " " + Time(), ;  // send date
      "", ;                             // conversation ID
      lMailConf, ;                      // acknowledgment
      lFromUser, ;                      // user intervention
      "from@example.org", ;             // sender
      { "to@example.org" }, ;           // destinators
      { __FILE__ } )                    // attach

   RETURN
