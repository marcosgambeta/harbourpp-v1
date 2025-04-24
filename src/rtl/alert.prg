//
// Alert(), hb_Alert() functions
//
// Released to Public Domain by Vladimir Kazimirchik <v_kazimirchik@yahoo.com>
// Further modifications 1999-2017 Viktor Szakats (vszakats.net/harbour)
//    Changes for higher Clipper compatibility, console mode, extensions, __NoNoAlert()
//

#include "box.ch"
#include "color.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "hbgtinfo.ch"

// FIXME: Clipper defines a clipped window for Alert() [vszakats]

// NOTE: Clipper will return NIL if the first parameter is not a string, but
//       this is not documented. [vszakats]

// NOTE: Clipper handles these buttons {"Ok", "", "Cancel"} in a buggy way.
//       This is fixed. [vszakats]

#ifdef HB_CLP_UNDOC
STATIC s_lNoAlert
#endif

FUNCTION Alert(cMessage, aOptions, cColorNorm)

   LOCAL cColorHigh
   LOCAL aOptionsOK
   LOCAL cOption
   LOCAL nPos

#ifdef HB_CLP_UNDOC

   IF s_lNoAlert == NIL
      s_lNoAlert := hb_argCheck("NOALERT")
   ENDIF

   IF s_lNoAlert
      RETURN NIL
   ENDIF

#endif

   IF !hb_IsString(cMessage)
      RETURN NIL
   ENDIF

   cMessage := StrTran(cMessage, ";", Chr(10))

   IF !hb_IsString(cColorNorm) .OR. Empty(cColorNorm)
      cColorNorm := "W+/R"  // first pair color (Box line and Text)
      cColorHigh := "W+/B"  // second pair color (Options buttons)
   ELSE
      cColorNorm := hb_ColorIndex(cColorNorm, CLR_STANDARD)
      cColorHigh := hb_StrReplace(IIf((nPos := At("/", cColorNorm)) > 0, SubStr(cColorNorm, nPos + 1) + "/" + Left(cColorNorm, nPos - 1), "N/" + cColorNorm), "+*")
   ENDIF

   aOptionsOK := {}
   FOR EACH cOption IN hb_defaultValue(aOptions, {})
      IF hb_IsString(cOption) .AND. !Empty(cOption)
         AAdd(aOptionsOK, cOption)
      ENDIF
   NEXT

   DO CASE
   CASE Len(aOptionsOK) == 0
      aOptionsOK := {"Ok"}
#ifdef HB_CLP_STRICT
   CASE Len(aOptionsOK) > 4  // NOTE: Clipper allows only four options [vszakats]
      ASize(aOptionsOK, 4)
#endif
   ENDCASE

   RETURN hb_gtAlert(cMessage, aOptionsOK, cColorNorm, cColorHigh)

// NOTE: xMessage can be of any type. This is a Harbour extension over Alert().
// NOTE: nDelay parameter is a Harbour extension over Alert().

FUNCTION hb_Alert(xMessage, aOptions, cColorNorm, nDelay)

   LOCAL cMessage
   LOCAL cColorHigh
   LOCAL aOptionsOK
   LOCAL cString
   LOCAL nPos

#ifdef HB_CLP_UNDOC

   IF s_lNoAlert == NIL
      s_lNoAlert := hb_argCheck("NOALERT")
   ENDIF

   IF s_lNoAlert
      RETURN NIL
   ENDIF

#endif

   IF PCount() == 0
      RETURN NIL
   ENDIF

   DO CASE
   CASE hb_IsArray(xMessage)
      cMessage := ""
      FOR EACH cString IN xMessage
         cMessage += IIf(cString:__enumIsFirst(), "", Chr(10)) + hb_CStr(cString)
      NEXT
   CASE hb_IsString(xMessage)
      cMessage := StrTran(xMessage, ";", Chr(10))
   CASE hb_IsHash(xMessage) .AND. "TXT" $ xMessage
      cMessage := xMessage
      IF !hb_IsArray(aOptions) .OR. Empty(aOptions)
         aOptions := hb_HGetDef(xMessage, "BTN")
      ENDIF
   OTHERWISE
      cMessage := hb_CStr(xMessage)
   ENDCASE

   IF !hb_IsString(cColorNorm) .OR. Empty(cColorNorm)
      cColorNorm := "W+/R"  // first pair color (Box line and Text)
      cColorHigh := "W+/B"  // second pair color (Options buttons)
   ELSE
      cColorNorm := hb_ColorIndex(cColorNorm, CLR_STANDARD)
      cColorHigh := hb_StrReplace(IIf((nPos := At("/", cColorNorm)) > 0, SubStr(cColorNorm, nPos + 1) + "/" + Left(cColorNorm, nPos - 1), "N/" + cColorNorm), "+*")
   ENDIF

   aOptionsOK := {}
   FOR EACH cString IN hb_defaultValue(aOptions, {})
      IF hb_IsString(cString) .AND. !Empty(cString)
         AAdd(aOptionsOK, cString)
      ENDIF
   NEXT

   DO CASE
   CASE Len(aOptionsOK) == 0
      aOptionsOK := {"Ok"}
#ifdef HB_CLP_STRICT
   CASE Len(aOptionsOK) > 4  // NOTE: Clipper allows only four options [vszakats]
      ASize(aOptionsOK, 4)
#endif
   ENDCASE

   RETURN hb_gtAlert(cMessage, aOptionsOK, cColorNorm, cColorHigh, nDelay)

#ifdef HB_CLP_UNDOC

PROCEDURE __NoNoAlert()

   s_lNoAlert := .F.

   RETURN

#endif
