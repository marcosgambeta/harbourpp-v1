//
// CP detection
//
// Copyright 2012 Viktor Szakats (vszakats.net/harbour)
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

#include "hbextcdp.ch"

Function hb_cdpTerm()

   Local cCP
   Local cLang

#if defined(__PLATFORM__WINDOWS)
   Local tmp
   cCP := __CPWinToCPStd(IIf((tmp := __wapi_GetConsoleOutputCP()) == 0, __wapi_GetOEMCP(), tmp))
   cLang := hb_UserLang()
#elif defined(__PLATFORM__UNIX)
   Local tmp
   cCP := __UnixParseLangCP(IIf(Empty(tmp := GetEnv("LANG")), GetEnv("LC_CTYPE"), tmp), @cLang)
#elif defined(__PLATFORM__DOS)
   // TODO
   cCP := cLang := NIL
#elif defined(__PLATFORM__OS2)
   // TODO
   cCP := cLang := NIL
#endif

   RETURN __CPStdToHb(cCP, cLang)

Function hb_cdpOS()

   Local cCP
   Local cLang

#if defined(__PLATFORM__WINDOWS)
   cCP := __CPWinToCPStd(__wapi_GetACP())
   cLang := hb_UserLang()
#elif defined(__PLATFORM__UNIX)
   Local tmp
   cCP := __UnixParseLangCP(IIf(Empty(tmp := GetEnv("LANG")), GetEnv("LC_CTYPE"), tmp), @cLang)
#elif defined(__PLATFORM__DOS)
   // TODO
   cCP := cLang := NIL
#elif defined(__PLATFORM__OS2)
   // TODO
   cCP := cLang := NIL
#endif

   RETURN __CPStdToHb(cCP, cLang)

#if defined(__PLATFORM__WINDOWS)
Static Function __CPWinToCPStd(nCPWin)

   Switch nCPWin
   Case 65001 ; RETURN "utf8"
   Case 1200  ; RETURN "utf16"
   Case 437
   Case 737
   Case 775
   Case 850
   Case 852
   Case 855
   Case 856
   Case 857
   Case 858
   Case 860
   Case 861
   Case 862
   Case 863
   Case 864
   Case 865
   Case 866
   Case 869
   Case 874
   Case 1250
   Case 1251
   Case 1252
   Case 1253
   Case 1254
   Case 1255
   Case 1256
   Case 1257
   Case 1258  ; RETURN "cp" + hb_ntos(nCPWin)
   Case 28591
   Case 28592
   Case 28593
   Case 28594
   Case 28595
   Case 28596
   Case 28597
   Case 28598
   Case 28599
   Case 28603
   Case 28605 ; RETURN "iso8859-" + hb_ntos(nCPWin - 28590)
   Case 20866 ; RETURN "koi-8"
   Case 21866 ; RETURN "koi-8u"
   Case 936   ; RETURN "GBK"
   Case 950   ; RETURN "CP950"
   EndSwitch

   Return Nil

#elif defined(__PLATFORM__UNIX)

// language[_territory][.codeset]
// [language[_territory][.codeset][@modifier]]
// TODO: handle "C"/"POSIX" values and values starting with "/"
Static Function __UnixParseLangCP(cString, /* @ */ cLang)

   Local tmp
   Local cCP

   IF (tmp := At(".", cString)) > 0
      cLang := Left(cString, tmp - 1)
      cCP := SubStr(cString, tmp + 1)
      IF (tmp := At("@", cString)) > 0
         cCP := Left(cString, tmp - 1)
      ENDIF
   ELSE
      cLang := cString
      cCP := "UTF-8"
   ENDIF

   // Tricks to make the manual translation table shorter
   cCP := hb_StrReplace(Lower(cCP), {"_" => "", "-" => "", "ibm" => "cp", "windows" => "cp"})
   IF hb_LeftEq(cCP, "iso8859")
      cCP := Stuff(cCP, Len("iso8859") + 1, 0, "-")
   ENDIF

   // Convert Unix CP name to Harbour CP ID
   Switch cCP
   Case "utf8"
   Case "cp437"
   Case "cp737"
   Case "cp775"
   Case "cp850"
   Case "cp852"
   Case "cp855"
   Case "cp856"
   Case "cp857"
   Case "cp858"
   Case "cp860"
   Case "cp861"
   Case "cp862"
   Case "cp863"
   Case "cp864"
   Case "cp865"
   Case "cp866"
   Case "cp869"
   Case "cp874"
   Case "cp1250"
   Case "cp1251"
   Case "cp1252"
   Case "cp1253"
   Case "cp1254"
   Case "cp1255"
   Case "cp1256"
   Case "cp1257"
   Case "cp1258"
   Case "iso8859-1"
   Case "iso8859-2"
   Case "iso8859-3"
   Case "iso8859-4"
   Case "iso8859-5"
   Case "iso8859-6"
   Case "iso8859-7"
   Case "iso8859-8"
   Case "iso8859-9"
   Case "iso8859-13"
   Case "iso8859-15" ; RETURN cCP
   Case "koi8r"      ; RETURN "koi-8"
   Case "koi8u"      ; RETURN "koi-8u"
   Case "gbk"        ; RETURN "GBK"
   Case "big5"       ; RETURN "BIG5"
   EndSwitch

   Return Nil

#endif

Static Function __CPStdToHb(cCPStd, cCtryStd)

   Local cCtryHb
   Local cdp
   Local aCP

   IF cCPStd != NIL
      Switch cCPStd := Lower(cCPStd)
      Case "utf8"
         RETURN "UTF8"
      Case "utf16"
         RETURN "UTF16LE"
      Otherwise
         aCP := hb_cdpList()
         cCtryHb := __LangStdToCPCtryHb(cCtryStd)
         For Each cdp In aCP
            IF hb_LeftEq(cdp, cCtryHb) .AND. cCPStd == hb_cdpUniID(cdp)
               RETURN cdp
            ENDIF
         Next
         For Each cdp In aCP
            IF cCPStd == hb_cdpUniID(cdp)
               RETURN hb_cdpUniID(cdp)
            ENDIF
         Next
      EndSwitch
   ENDIF

   Return Nil

Static Function __LangStdToCPCtryHb(cCtryStd)

   Switch Lower(cCtryStd)
#if 0
   Case "af-za"      ; Exit
   Case "af"         ; Exit
   Case "ar-ae"      ; Exit
   Case "ar-bh"      ; Exit
   Case "ar-dz"      ; Exit
   Case "ar-eg"      ; Exit
   Case "ar-iq"      ; Exit
   Case "ar-jo"      ; Exit
   Case "ar-kw"      ; Exit
   Case "ar-lb"      ; Exit
   Case "ar-ly"      ; Exit
   Case "ar-ma"      ; Exit
   Case "ar-om"      ; Exit
   Case "ar-qa"      ; Exit
   Case "ar-sa"      ; Exit
   Case "ar-sy"      ; Exit
   Case "ar-tn"      ; Exit
   Case "ar-ye"      ; Exit
   Case "ar"         ; Exit
   Case "az-az-cyrl" ; Exit
   Case "az-az-latn" ; Exit
   Case "az"         ; Exit
   Case "be-by"      ; Exit
   Case "be"         ; Exit
#endif
   Case "bg-bg"
   Case "bg"         ; RETURN "BG"
#if 0
   Case "ca-es"      ; Exit
   Case "ca"         ; Exit
   Case "cy-gb"      ; Exit
#endif
   Case "cs-cz"
   Case "cs"         ; RETURN "CS"
   Case "da-dk"
   Case "da"         ; RETURN "DK"
   Case "de-at"
   Case "de-ch"
   Case "de-de"
   Case "de-li"
   Case "de-lu"
   Case "de"         ; RETURN "DE"
#if 0
   Case "div-mv"     ; Exit
   Case "div"        ; Exit
#endif
   Case "el-gr"
   Case "el"         ; RETURN "EL"
   Case "en-au"
   Case "en-bz"
   Case "en-ca"
   Case "en-cb"
   Case "en-gb"
   Case "en-ie"
   Case "en-jm"
   Case "en-nz"
   Case "en-ph"
   Case "en-tt"
   Case "en-us"
   Case "en-za"
   Case "en-zw"
   Case "en"         ; RETURN "EN"
#if 0
   Case "eo"         ; Exit
#endif
   Case "es-419"
   Case "es-ar"
   Case "es-bo"
   Case "es-cl"
   Case "es-co"
   Case "es-cr"
   Case "es-do"
   Case "es-ec"
   Case "es-es"
   Case "es-gt"
   Case "es-hn"
   Case "es-mx"
   Case "es-ni"
   Case "es-pa"
   Case "es-pe"
   Case "es-pr"
   Case "es-py"
   Case "es-sv"
   Case "es-uy"
   Case "es-ve"
   Case "es"         ; RETURN "ES"
#if 0
   Case "et-ee"      ; Exit
   Case "et"         ; Exit
   Case "eu-es"      ; Exit
   Case "eu"         ; Exit
   Case "fa-ir"      ; Exit
   Case "fa"         ; Exit
#endif
   Case "fi-fi"
   Case "fi"         ; RETURN "FI"
#if 0
   Case "fo-fo"      ; Exit
   Case "fo"         ; Exit
#endif
   Case "fr-be"
   Case "fr-ca"
   Case "fr-ch"
   Case "fr-fr"
   Case "fr-lu"
   Case "fr-mc"
   Case "fr"         ; RETURN "FR"
#if 0
   Case "gl-es"      ; Exit
   Case "gl"         ; Exit
   Case "gu-in"      ; Exit
   Case "gu"         ; Exit
#endif
   Case "he-il"
   Case "he"         ; RETURN "HE"
#if 0
   Case "hi-in"      ; Exit
   Case "hi"         ; Exit
#endif
   Case "hr-hr"
   Case "hr"         ; RETURN "HR"
   Case "hu-hu"
   Case "hu"         ; RETURN "HU"
#if 0
   Case "hy-am"      ; Exit
   Case "hy"         ; Exit
   Case "id-id"      ; Exit
   Case "id"         ; Exit
#endif
   Case "is-is"
   Case "is"         ; RETURN "IS"
   Case "it-ch"
   Case "it-it"
   Case "it"         ; RETURN "IT"
#if 0
   Case "ja-jp"      ; Exit
   Case "ja"         ; Exit
   Case "ka-ge"      ; Exit
   Case "ka"         ; Exit
   Case "kk-kz"      ; Exit
   Case "kk"         ; Exit
   Case "kn-in"      ; Exit
   Case "kn"         ; Exit
   Case "ko-kr"      ; Exit
   Case "ko"         ; Exit
   Case "kok-in"     ; Exit
   Case "kok"        ; Exit
   Case "ky-kz"      ; Exit
   Case "ky"         ; Exit
#endif
   Case "lt-lt"
   Case "lt"         ; RETURN "LT"
#if 0
   Case "lv-lv"      ; Exit
   Case "lv"         ; Exit
   Case "mk-mk"      ; Exit
   Case "mk"         ; Exit
   Case "mn-mn"      ; Exit
   Case "mn"         ; Exit
   Case "mr-in"      ; Exit
   Case "mr"         ; Exit
   Case "ms-bn"      ; Exit
   Case "ms-my"      ; Exit
   Case "ms"         ; Exit
   Case "nb-no"      ; Exit
#endif
   Case "nl-be"
   Case "nl-nl"
   Case "nl"         ; RETURN "NL"
#if 0
   Case "nn-no"      ; Exit
#endif
   Case "no"         ; RETURN "NO"
#if 0
   Case "pa-in"      ; Exit
   Case "pa"         ; Exit
#endif
   Case "pl-pl"
   Case "pl"         ; RETURN "PL"
   Case "pt-br"
   Case "pt-pt"
   Case "pt"         ; RETURN "PT"
   Case "ro-ro"
   Case "ro"         ; RETURN "RO"
   Case "ru-ru"
   Case "ru"         ; RETURN "RU"
#if 0
   Case "sa-in"      ; Exit
   Case "sa"         ; Exit
#endif
   Case "sk-sk"
   Case "sk"         ; RETURN "SK"
   Case "sl-si"
   Case "sl"         ; RETURN "SL"
#if 0
   Case "sq-al"      ; Exit
   Case "sq"         ; Exit
#endif
   Case "sr-sp-cyrl"
   Case "sr-sp-latn" ; RETURN "SR"
   Case "sv-fi"
   Case "sv-se"
   Case "sv"         ; RETURN "SV"
#if 0
   Case "sw-ke"      ; Exit
   Case "sw"         ; Exit
   Case "syr-sy"     ; Exit
   Case "syr"        ; Exit
   Case "ta-in"      ; Exit
   Case "ta"         ; Exit
   Case "te-in"      ; Exit
   Case "te"         ; Exit
   Case "th-th"      ; Exit
   Case "th"         ; Exit
#endif
   Case "tr-tr"
   Case "tr"         ; RETURN "TR"
#if 0
   Case "tt-ru"      ; Exit
   Case "tt"         ; Exit
#endif
   Case "uk-ua"
   Case "uk"         ; RETURN "UA"
#if 0
   Case "ur-pk"      ; Exit
   Case "ur"         ; Exit
   Case "uz-uz-cyrl" ; Exit
   Case "uz-uz-latn" ; Exit
   Case "uz"         ; Exit
   Case "vi-vn"      ; Exit
   Case "vi"         ; Exit
   Case "zh-chs"     ; Exit
   Case "zh-cht"     ; Exit
   Case "zh-cn"      ; Exit
   Case "zh-hk"      ; Exit
   Case "zh-mo"      ; Exit
   Case "zh-sg"      ; Exit
   Case "zh-tw"      ; Exit
   Case "zh"         ; Exit
#endif
   EndSwitch

   RETURN Left(hb_cdpSelect(), 2)  // Caller assumes this never returns strings shorter than two chars
