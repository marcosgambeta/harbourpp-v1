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

Function Alert(cMessage, aOptions, cColorNorm)

#ifdef HB_CLP_UNDOC

   IF s_lNoAlert == NIL
      s_lNoAlert := hb_argCheck("NOALERT")
   ENDIF

   IF s_lNoAlert
      Return Nil
   ENDIF

#endif

   IF !hb_IsString(cMessage)
      Return Nil
   ENDIF

   cMessage := StrTran(cMessage, ";", Chr(10))

   Local cColorHigh

   IF !hb_IsString(cColorNorm) .OR. Empty(cColorNorm)
      cColorNorm := "W+/R"  // first pair color (Box line and Text)
      cColorHigh := "W+/B"  // second pair color (Options buttons)
   ELSE
      cColorNorm := hb_ColorIndex(cColorNorm, CLR_STANDARD)
      Local nPos
      cColorHigh := hb_StrReplace(IIf((nPos := At("/", cColorNorm)) > 0, SubStr(cColorNorm, nPos + 1) + "/" + Left(cColorNorm, nPos - 1), "N/" + cColorNorm), "+*")
   ENDIF

   Local cOption
   Local aOptionsOK := {}
   For Each cOption In hb_defaultValue(aOptions, {})
      IF hb_IsString(cOption) .AND. !Empty(cOption)
         AAdd(aOptionsOK, cOption)
      ENDIF
   Next

   Do Case
   Case Len(aOptionsOK) == 0
      aOptionsOK := {"Ok"}
#ifdef HB_CLP_STRICT
   Case Len(aOptionsOK) > 4  // NOTE: Clipper allows only four options [vszakats]
      ASize(aOptionsOK, 4)
#endif
   EndCase

   RETURN hb_gtAlert(cMessage, aOptionsOK, cColorNorm, cColorHigh)

// NOTE: xMessage can be of any type. This is a Harbour extension over Alert().
// NOTE: nDelay parameter is a Harbour extension over Alert().

Function hb_Alert(xMessage, aOptions, cColorNorm, nDelay)

#ifdef HB_CLP_UNDOC

   IF s_lNoAlert == NIL
      s_lNoAlert := hb_argCheck("NOALERT")
   ENDIF

   IF s_lNoAlert
      Return Nil
   ENDIF

#endif

   IF PCount() == 0
      Return Nil
   ENDIF

   Local cMessage
   Local cString

   Do Case
   Case hb_IsArray(xMessage)
      cMessage := ""
      For Each cString In xMessage
         cMessage += IIf(cString:__enumIsFirst(), "", Chr(10)) + hb_CStr(cString)
      Next
   Case hb_IsString(xMessage)
      cMessage := StrTran(xMessage, ";", Chr(10))
   Case hb_IsHash(xMessage) .AND. "TXT" $ xMessage
      cMessage := xMessage
      IF !hb_IsArray(aOptions) .OR. Empty(aOptions)
         aOptions := hb_HGetDef(xMessage, "BTN")
      ENDIF
   Otherwise
      cMessage := hb_CStr(xMessage)
   EndCase

   Local cColorHigh

   IF !hb_IsString(cColorNorm) .OR. Empty(cColorNorm)
      cColorNorm := "W+/R"  // first pair color (Box line and Text)
      cColorHigh := "W+/B"  // second pair color (Options buttons)
   ELSE
      cColorNorm := hb_ColorIndex(cColorNorm, CLR_STANDARD)
      Local nPos
      cColorHigh := hb_StrReplace(IIf((nPos := At("/", cColorNorm)) > 0, SubStr(cColorNorm, nPos + 1) + "/" + Left(cColorNorm, nPos - 1), "N/" + cColorNorm), "+*")
   ENDIF

   Local aOptionsOK := {}
   For Each cString In hb_defaultValue(aOptions, {})
      IF hb_IsString(cString) .AND. !Empty(cString)
         AAdd(aOptionsOK, cString)
      ENDIF
   Next

   Do Case
   Case Len(aOptionsOK) == 0
      aOptionsOK := {"Ok"}
#ifdef HB_CLP_STRICT
   Case Len(aOptionsOK) > 4  // NOTE: Clipper allows only four options [vszakats]
      ASize(aOptionsOK, 4)
#endif
   EndCase

   RETURN hb_gtAlert(cMessage, aOptionsOK, cColorNorm, cColorHigh, nDelay)

#ifdef HB_CLP_UNDOC

Procedure __NoNoAlert()

   s_lNoAlert := False

   RETURN

#endif
