// Copyright 2015 Viktor Szakats

#require "hbwin34"

#include "simpleio.ch"

PROCEDURE Main( cURL )

   LOCAL aURL := { "https://example.org/" }
   LOCAL cByPass

   IF hb_IsString(cURL)
      AAdd( aURL, cURL )
   ENDIF

   FOR EACH cURL IN aURL
      ? "URL:", cURL
      ? "Proxy (raw):", hb_ValToExp( __win_ProxyDetect( cURL ) )
      ? "Proxy:", hb_ValToExp( win_ProxyDetect( cURL, @cByPass ) )
      ? "ByPass:", hb_ValToExp( cByPass )
      ? "Result:", hb_ntos(wapi_GetLastError()), win_ErrorString( wapi_GetLastError() )
      ?
   NEXT

   RETURN
