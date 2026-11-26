//
// The Language API module selection
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

#ifdef HB_LEGACY_LEVEL4
// Required for legacy language modules with a two character ID.
// These cannot have a compatibility puller symbol in langlgcy.prg,
// which in turn pulls all CP modules, so we're pulling them from
// here.
REQUEST HB_CODEPAGE_CS852
REQUEST HB_CODEPAGE_DE850
REQUEST HB_CODEPAGE_EL737
#endif

FUNCTION hb_langSelect(cLangID, cCP)

   LOCAL tmp
   LOCAL cCPDef
   LOCAL cLangIDBase

   IF hb_IsString(cLangID)

      cCPDef := hb_cdpSelect()

#ifdef HB_LEGACY_LEVEL4

      // Emulate legacy Harbour language modules for compatibility
      Switch Upper(cLangID)
      Case "BE866"  ; cCPDef := "BG866" ; cLangIDBase := "be" ; EXIT
      Case "BEWIN"  ; cCPDef := "BGWIN" ; cLangIDBase := "be" ; EXIT
      Case "BG866"  ; cCPDef := "BG866" ; cLangIDBase := "bg" ; EXIT
      Case "BGISO"  ; cCPDef := "BGISO" ; cLangIDBase := "bg" ; EXIT
      Case "BGMIK"  ; cCPDef := "BGMIK" ; cLangIDBase := "bg" ; EXIT
      Case "BGWIN"  ; cCPDef := "BGWIN" ; cLangIDBase := "bg" ; EXIT
      Case "CS852"  ; cCPDef := "CS852" ; cLangIDBase := "cs" ; EXIT
      Case "CSISO"  ; cCPDef := "CSISO" ; cLangIDBase := "cs" ; EXIT
      Case "CSKAM"  ; cCPDef := "CSKAMC"; cLangIDBase := "cs" ; EXIT
      Case "CSWIN"  ; cCPDef := "CSWIN" ; cLangIDBase := "cs" ; EXIT
      Case "DEWIN"  ; cCPDef := "DEWIN" ; cLangIDBase := "de" ; EXIT
      Case "ELWIN"  ; cCPDef := "ELWIN" ; cLangIDBase := "el" ; EXIT
      Case "ESWIN"  ; cCPDef := "ESWIN" ; cLangIDBase := "es" ; EXIT
      Case "HE862"  ; cCPDef := "HE862" ; cLangIDBase := "he" ; EXIT
      Case "HEWIN"  ; cCPDef := "HEWIN" ; cLangIDBase := "he" ; EXIT
      Case "HR646"  ; cCPDef := "HR646" ; cLangIDBase := "hr" ; EXIT
      Case "HR852"  ; cCPDef := "HR852" ; cLangIDBase := "hr" ; EXIT
      Case "HRISO"  ; cCPDef := "HRISO" ; cLangIDBase := "hr" ; EXIT
      Case "HRWIN"  ; cCPDef := "HRWIN" ; cLangIDBase := "hr" ; EXIT
      Case "HU852"  ; cCPDef := "HU852" ; cLangIDBase := "hu" ; EXIT
      Case "HUISO"  ; cCPDef := "HUISO" ; cLangIDBase := "hu" ; EXIT
      Case "HUWIN"  ; cCPDef := "HUWIN" ; cLangIDBase := "hu" ; EXIT
      Case "IS850"  ; cCPDef := "IS850" ; cLangIDBase := "is" ; EXIT
      Case "LTWIN"  ; cCPDef := "LTWIN" ; cLangIDBase := "lt" ; EXIT
      Case "PL852"  ; cCPDef := "PL852" ; cLangIDBase := "pl" ; EXIT
      Case "PLISO"  ; cCPDef := "PLISO" ; cLangIDBase := "pl" ; EXIT
      Case "PLMAZ"  ; cCPDef := "PLMAZ" ; cLangIDBase := "pl" ; EXIT
      Case "PLWIN"  ; cCPDef := "PLWIN" ; cLangIDBase := "pl" ; EXIT
      Case "PTISO"  ; cCPDef := "PTISO" ; cLangIDBase := "pt" ; EXIT
      Case "RU866"  ; cCPDef := "RU866" ; cLangIDBase := "ru" ; EXIT
      Case "RUKOI8" ; cCPDef := "RUKOI8"; cLangIDBase := "ru" ; EXIT
      Case "RUWIN"  ; cCPDef := "RU1251"; cLangIDBase := "ru" ; EXIT
      Case "SK852"  ; cCPDef := "SK852" ; cLangIDBase := "sk" ; EXIT
      Case "SKISO"  ; cCPDef := "SKISO" ; cLangIDBase := "sk" ; EXIT
      Case "SKKAM"  ; cCPDef := "SKKAMC"; cLangIDBase := "sk" ; EXIT
      Case "SKWIN"  ; cCPDef := "SKWIN" ; cLangIDBase := "sk" ; EXIT
      Case "SL646"  ; cCPDef := "SL646" ; cLangIDBase := "sl" ; EXIT
      Case "SL852"  ; cCPDef := "SL852" ; cLangIDBase := "sl" ; EXIT
      Case "SLISO"  ; cCPDef := "SLISO" ; cLangIDBase := "sl" ; EXIT
      Case "SLWIN"  ; cCPDef := "SLWIN" ; cLangIDBase := "sl" ; EXIT
      Case "SR852"  ; cCPDef := "SL852" ; cLangIDBase := "sr_lat" ; EXIT
      Case "SRISO"  ; cCPDef := "SLISO" ; cLangIDBase := "sr_lat" ; EXIT
      Case "SRWIN"  ; cCPDef := "SRWIN" ; cLangIDBase := "sr_cyr" ; EXIT
      Case "SVWIN"  ; cCPDef := "SVWIN" ; cLangIDBase := "sv" ; EXIT
      Case "TR857"  ; cCPDef := "TR857" ; cLangIDBase := "tr" ; EXIT
      Case "TRWIN"  ; cCPDef := "TRWIN" ; cLangIDBase := "tr" ; EXIT
      Case "UA866"  ; cCPDef := "UA866" ; cLangIDBase := "uk" ; EXIT
      Case "UADOS"  ; cCPDef := "UA1125"; cLangIDBase := "uk" ; EXIT
      Case "UAKOI8" ; cCPDef := "UAKOI8"; cLangIDBase := "uk" ; EXIT
      Case "UAWIN"  ; cCPDef := "UA1251"; cLangIDBase := "uk" ; EXIT
      Case "ZHB5"   ; cCPDef := "BIG5"  ; cLangIDBase := "zh" ; EXIT
      Case "ZHGB"   ; cCPDef := "GBK"   ; cLangIDBase := "zh_sim" ; EXIT
      Otherwise
         // Case sensitive legacy IDs. Lowercase flavours denote new
         // language module IDs, so they won't be recognized as
         // compatibility ones. INCOMPATIBLE.
         Switch cLangID
         Case "CA"     ; cCPDef := "DE850" ; cLangIDBase := "ca" ; EXIT
         Case "DE"     ; cCPDef := "DE850" ; cLangIDBase := "de" ; EXIT
         Case "EL"     ; cCPDef := "EL737" ; cLangIDBase := "el" ; EXIT
         Case "EO"     ; cCPDef := "DE850" ; cLangIDBase := "eo" ; EXIT
         Case "ES"     ; cCPDef := "DE850" ; cLangIDBase := "es" ; EXIT
         Case "EU"     ; cCPDef := "DE850" ; cLangIDBase := "eu" ; EXIT
         Case "FR"     ; cCPDef := "DE850" ; cLangIDBase := "fr" ; EXIT
         Case "GL"     ; cCPDef := "DE850" ; cLangIDBase := "gl" ; EXIT
         Case "ID"     ; cCPDef := "EN"    ; cLangIDBase := "id" ; EXIT
         Case "IT"     ; cCPDef := "DE850" ; cLangIDBase := "it" ; EXIT
         // INCOMPATIBILITY: "KO" (Korean) using CP949 is not supported anymore.
         Case "NL"     ; cCPDef := "EN"    ; cLangIDBase := "nl" ; EXIT
         Case "PT"     ; cCPDef := "DE850" ; cLangIDBase := "pt" ; EXIT
         Case "RO"     ; cCPDef := "CS852" ; cLangIDBase := "ro" ; EXIT
         Case "SV"     ; cCPDef := "DE850" ; cLangIDBase := "sv" ; EXIT
         EndSwitch
      EndSwitch

#endif

      IF !hb_IsString(cCP)
         cCP := cCPDef
      ENDIF

#ifdef HB_LEGACY_LEVEL4
      IF Empty(cLangIDBase)
#endif
         // Support standard ISO language IDs
         IF Empty(tmp := __LangStdToLangHb(cLangID))
            // Normal case
            cLangIDBase := cLangID
         ELSE
            cLangID := cLangIDBase := tmp
         ENDIF
#ifdef HB_LEGACY_LEVEL4
      ELSE
         // Legacy emulation
         cLangID := cLangIDBase
      ENDIF
#endif

      IF !hb_cdpIsUTF8(cCP)
         cLangID += "." + cCP
         hb_langNew(cLangID, cCP, cLangIDBase, "UTF8")
      ENDIF
   ENDIF

   RETURN __hb_langSelect(cLangID)

STATIC FUNCTION __LangStdToLangHb(cLangStd)

   LOCAL cLangHb := ""

   IF hb_IsString(cLangStd)
      Switch Lower(StrTran(cLangStd, "_", "-"))
#if 0
      Case "af-za"      ; EXIT
      Case "af"         ; EXIT
      Case "ar-ae"      ; EXIT
      Case "ar-bh"      ; EXIT
      Case "ar-dz"      ; EXIT
      Case "ar-eg"      ; EXIT
      Case "ar-iq"      ; EXIT
      Case "ar-jo"      ; EXIT
      Case "ar-kw"      ; EXIT
      Case "ar-lb"      ; EXIT
      Case "ar-ly"      ; EXIT
      Case "ar-ma"      ; EXIT
      Case "ar-om"      ; EXIT
      Case "ar-qa"      ; EXIT
      Case "ar-sa"      ; EXIT
      Case "ar-sy"      ; EXIT
      Case "ar-tn"      ; EXIT
      Case "ar-ye"      ; EXIT
      Case "ar"         ; EXIT
      Case "az-az-cyrl" ; EXIT
      Case "az-az-latn" ; EXIT
      Case "az"         ; EXIT
      Case "be-by"      ; EXIT
#endif
      Case "be"         ; cLangHb := "be" ; EXIT
      Case "bg-bg"
      Case "bg"         ; cLangHb := "bg" ; EXIT
      Case "ca-es"
      Case "ca"         ; cLangHb := "ca" ; EXIT
#if 0
      Case "cy-gb"      ; EXIT
#endif
      Case "cs-cz"
      Case "cs"         ; cLangHb := "cs" ; EXIT
#if 0
      Case "da-dk"      ; EXIT
      Case "da"         ; EXIT
#endif
      Case "de-at"      ; cLangHb := "de_at" ; EXIT
      Case "de-ch"
      Case "de-de"
      Case "de-li"
      Case "de-lu"
      Case "de"         ; cLangHb := "de" ; EXIT
#if 0
      Case "div-mv"     ; EXIT
      Case "div"        ; EXIT
#endif
      Case "el-gr"
      Case "el"         ; cLangHb := "el" ; EXIT
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
      Case "en"         ; cLangHb := "en" ; EXIT
      Case "eo"         ; cLangHb := "eo" ; EXIT
      Case "es-419"     ; cLangHb := "es_419" ; EXIT
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
      Case "es"         ; cLangHb := "es" ; EXIT
#if 0
      Case "et-ee"      ; EXIT
      Case "et"         ; EXIT
#endif
      Case "eu-es"
      Case "eu"         ; cLangHb := "eu" ; EXIT
#if 0
      Case "fa-ir"      ; EXIT
      Case "fa"         ; EXIT
      Case "fi-fi"      ; EXIT
      Case "fi"         ; EXIT
      Case "fo-fo"      ; EXIT
      Case "fo"         ; EXIT
#endif
      Case "fr-be"
      Case "fr-ca"
      Case "fr-ch"
      Case "fr-fr"
      Case "fr-lu"
      Case "fr-mc"
      Case "fr"         ; cLangHb := "fr" ; EXIT
      Case "gl-es"
      Case "gl"         ; cLangHb := "gl" ; EXIT
#if 0
      Case "gu-in"      ; EXIT
      Case "gu"         ; EXIT
#endif
      Case "he-il"
      Case "he"         ; cLangHb := "he" ; EXIT
#if 0
      Case "hi-in"      ; EXIT
      Case "hi"         ; EXIT
#endif
      Case "hr-hr"
      Case "hr"         ; cLangHb := "hr" ; EXIT
      Case "hu-hu"
      Case "hu"         ; cLangHb := "hu" ; EXIT
#if 0
      Case "hy-am"      ; EXIT
      Case "hy"         ; EXIT
#endif
      Case "id-id"
      Case "id"         ; cLangHb := "id" ; EXIT
      Case "is-is"
      Case "is"         ; cLangHb := "is" ; EXIT
      Case "it-ch"
      Case "it-it"
      Case "it"         ; cLangHb := "it" ; EXIT
#if 0
      Case "ja-jp"      ; EXIT
      Case "ja"         ; EXIT
      Case "ka-ge"      ; EXIT
      Case "ka"         ; EXIT
      Case "kk-kz"      ; EXIT
      Case "kk"         ; EXIT
      Case "kn-in"      ; EXIT
      Case "kn"         ; EXIT
#endif
      Case "ko-kr"
      Case "ko"         ; cLangHb := "ko" ; EXIT
#if 0
      Case "kok-in"     ; EXIT
      Case "kok"        ; EXIT
      Case "ky-kz"      ; EXIT
      Case "ky"         ; EXIT
#endif
      Case "lt-lt"
      Case "lt"         ; cLangHb := "lt" ; EXIT
#if 0
      Case "lv-lv"      ; EXIT
      Case "lv"         ; EXIT
      Case "mk-mk"      ; EXIT
      Case "mk"         ; EXIT
      Case "mn-mn"      ; EXIT
      Case "mn"         ; EXIT
      Case "mr-in"      ; EXIT
      Case "mr"         ; EXIT
      Case "ms-bn"      ; EXIT
      Case "ms-my"      ; EXIT
      Case "ms"         ; EXIT
      Case "nb-no"      ; EXIT
#endif
      Case "nl-be"
      Case "nl-nl"
      Case "nl"         ; cLangHb := "nl" ; EXIT
#if 0
      Case "nn-no"      ; EXIT
      Case "no"         ; EXIT
      Case "pa-in"      ; EXIT
      Case "pa"         ; EXIT
#endif
      Case "pl-pl"
      Case "pl"         ; cLangHb := "pl" ; EXIT
      Case "pt-br"      ; cLangHb := "pt_br" ; EXIT
      Case "pt-pt"
      Case "pt"         ; cLangHb := "pt" ; EXIT
      Case "ro-ro"
      Case "ro"         ; cLangHb := "ro" ; EXIT
      Case "ru-ru"
      Case "ru"         ; cLangHb := "ru" ; EXIT
#if 0
      Case "sa-in"      ; EXIT
      Case "sa"         ; EXIT
#endif
      Case "sk-sk"
      Case "sk"         ; cLangHb := "sk" ; EXIT
      Case "sl-si"
      Case "sl"         ; cLangHb := "sl" ; EXIT
#if 0
      Case "sq-al"      ; EXIT
      Case "sq"         ; EXIT
#endif
      Case "sr-sp-cyrl" ; cLangHb := "sr_cyr" ; EXIT
      Case "sr-sp-latn" ; cLangHb := "sr_lat" ; EXIT
      Case "sv-fi"
      Case "sv-se"
      Case "sv"         ; cLangHb := "sv" ; EXIT
#if 0
      Case "sw-ke"      ; EXIT
      Case "sw"         ; EXIT
      Case "syr-sy"     ; EXIT
      Case "syr"        ; EXIT
      Case "ta-in"      ; EXIT
      Case "ta"         ; EXIT
      Case "te-in"      ; EXIT
      Case "te"         ; EXIT
      Case "th-th"      ; EXIT
      Case "th"         ; EXIT
#endif
      Case "tr-tr"
      Case "tr"         ; cLangHb := "tr" ; EXIT
#if 0
      Case "tt-ru"      ; EXIT
      Case "tt"         ; EXIT
#endif
      Case "uk-ua"
      Case "uk"         ; cLangHb := "uk" ; EXIT
#if 0
      Case "ur-pk"      ; EXIT
      Case "ur"         ; EXIT
      Case "uz-uz-cyrl" ; EXIT
      Case "uz-uz-latn" ; EXIT
      Case "uz"         ; EXIT
      Case "vi-vn"      ; EXIT
      Case "vi"         ; EXIT
#endif
      Case "zh-chs"     ; cLangHb := "zh_sim" ; EXIT
      Case "zh-cht"
      Case "zh-cn"
      Case "zh-hk"
      Case "zh-mo"
      Case "zh-sg"
      Case "zh-tw"
      Case "zh"         ; cLangHb := "zh" ; EXIT
      EndSwitch
   ENDIF

   RETURN cLangHb
