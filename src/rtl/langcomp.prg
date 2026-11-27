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

Function hb_langSelect(cLangID, cCP)

   Local tmp
   Local cCPDef
   Local cLangIDBase

   IF hb_IsString(cLangID)

      cCPDef := hb_cdpSelect()

#ifdef HB_LEGACY_LEVEL4

      // Emulate legacy Harbour language modules for compatibility
      Switch Upper(cLangID)
      Case "BE866"  ; cCPDef := "BG866" ; cLangIDBase := "be" ; Exit
      Case "BEWIN"  ; cCPDef := "BGWIN" ; cLangIDBase := "be" ; Exit
      Case "BG866"  ; cCPDef := "BG866" ; cLangIDBase := "bg" ; Exit
      Case "BGISO"  ; cCPDef := "BGISO" ; cLangIDBase := "bg" ; Exit
      Case "BGMIK"  ; cCPDef := "BGMIK" ; cLangIDBase := "bg" ; Exit
      Case "BGWIN"  ; cCPDef := "BGWIN" ; cLangIDBase := "bg" ; Exit
      Case "CS852"  ; cCPDef := "CS852" ; cLangIDBase := "cs" ; Exit
      Case "CSISO"  ; cCPDef := "CSISO" ; cLangIDBase := "cs" ; Exit
      Case "CSKAM"  ; cCPDef := "CSKAMC"; cLangIDBase := "cs" ; Exit
      Case "CSWIN"  ; cCPDef := "CSWIN" ; cLangIDBase := "cs" ; Exit
      Case "DEWIN"  ; cCPDef := "DEWIN" ; cLangIDBase := "de" ; Exit
      Case "ELWIN"  ; cCPDef := "ELWIN" ; cLangIDBase := "el" ; Exit
      Case "ESWIN"  ; cCPDef := "ESWIN" ; cLangIDBase := "es" ; Exit
      Case "HE862"  ; cCPDef := "HE862" ; cLangIDBase := "he" ; Exit
      Case "HEWIN"  ; cCPDef := "HEWIN" ; cLangIDBase := "he" ; Exit
      Case "HR646"  ; cCPDef := "HR646" ; cLangIDBase := "hr" ; Exit
      Case "HR852"  ; cCPDef := "HR852" ; cLangIDBase := "hr" ; Exit
      Case "HRISO"  ; cCPDef := "HRISO" ; cLangIDBase := "hr" ; Exit
      Case "HRWIN"  ; cCPDef := "HRWIN" ; cLangIDBase := "hr" ; Exit
      Case "HU852"  ; cCPDef := "HU852" ; cLangIDBase := "hu" ; Exit
      Case "HUISO"  ; cCPDef := "HUISO" ; cLangIDBase := "hu" ; Exit
      Case "HUWIN"  ; cCPDef := "HUWIN" ; cLangIDBase := "hu" ; Exit
      Case "IS850"  ; cCPDef := "IS850" ; cLangIDBase := "is" ; Exit
      Case "LTWIN"  ; cCPDef := "LTWIN" ; cLangIDBase := "lt" ; Exit
      Case "PL852"  ; cCPDef := "PL852" ; cLangIDBase := "pl" ; Exit
      Case "PLISO"  ; cCPDef := "PLISO" ; cLangIDBase := "pl" ; Exit
      Case "PLMAZ"  ; cCPDef := "PLMAZ" ; cLangIDBase := "pl" ; Exit
      Case "PLWIN"  ; cCPDef := "PLWIN" ; cLangIDBase := "pl" ; Exit
      Case "PTISO"  ; cCPDef := "PTISO" ; cLangIDBase := "pt" ; Exit
      Case "RU866"  ; cCPDef := "RU866" ; cLangIDBase := "ru" ; Exit
      Case "RUKOI8" ; cCPDef := "RUKOI8"; cLangIDBase := "ru" ; Exit
      Case "RUWIN"  ; cCPDef := "RU1251"; cLangIDBase := "ru" ; Exit
      Case "SK852"  ; cCPDef := "SK852" ; cLangIDBase := "sk" ; Exit
      Case "SKISO"  ; cCPDef := "SKISO" ; cLangIDBase := "sk" ; Exit
      Case "SKKAM"  ; cCPDef := "SKKAMC"; cLangIDBase := "sk" ; Exit
      Case "SKWIN"  ; cCPDef := "SKWIN" ; cLangIDBase := "sk" ; Exit
      Case "SL646"  ; cCPDef := "SL646" ; cLangIDBase := "sl" ; Exit
      Case "SL852"  ; cCPDef := "SL852" ; cLangIDBase := "sl" ; Exit
      Case "SLISO"  ; cCPDef := "SLISO" ; cLangIDBase := "sl" ; Exit
      Case "SLWIN"  ; cCPDef := "SLWIN" ; cLangIDBase := "sl" ; Exit
      Case "SR852"  ; cCPDef := "SL852" ; cLangIDBase := "sr_lat" ; Exit
      Case "SRISO"  ; cCPDef := "SLISO" ; cLangIDBase := "sr_lat" ; Exit
      Case "SRWIN"  ; cCPDef := "SRWIN" ; cLangIDBase := "sr_cyr" ; Exit
      Case "SVWIN"  ; cCPDef := "SVWIN" ; cLangIDBase := "sv" ; Exit
      Case "TR857"  ; cCPDef := "TR857" ; cLangIDBase := "tr" ; Exit
      Case "TRWIN"  ; cCPDef := "TRWIN" ; cLangIDBase := "tr" ; Exit
      Case "UA866"  ; cCPDef := "UA866" ; cLangIDBase := "uk" ; Exit
      Case "UADOS"  ; cCPDef := "UA1125"; cLangIDBase := "uk" ; Exit
      Case "UAKOI8" ; cCPDef := "UAKOI8"; cLangIDBase := "uk" ; Exit
      Case "UAWIN"  ; cCPDef := "UA1251"; cLangIDBase := "uk" ; Exit
      Case "ZHB5"   ; cCPDef := "BIG5"  ; cLangIDBase := "zh" ; Exit
      Case "ZHGB"   ; cCPDef := "GBK"   ; cLangIDBase := "zh_sim" ; Exit
      Otherwise
         // Case sensitive legacy IDs. Lowercase flavours denote new
         // language module IDs, so they won't be recognized as
         // compatibility ones. INCOMPATIBLE.
         Switch cLangID
         Case "CA"     ; cCPDef := "DE850" ; cLangIDBase := "ca" ; Exit
         Case "DE"     ; cCPDef := "DE850" ; cLangIDBase := "de" ; Exit
         Case "EL"     ; cCPDef := "EL737" ; cLangIDBase := "el" ; Exit
         Case "EO"     ; cCPDef := "DE850" ; cLangIDBase := "eo" ; Exit
         Case "ES"     ; cCPDef := "DE850" ; cLangIDBase := "es" ; Exit
         Case "EU"     ; cCPDef := "DE850" ; cLangIDBase := "eu" ; Exit
         Case "FR"     ; cCPDef := "DE850" ; cLangIDBase := "fr" ; Exit
         Case "GL"     ; cCPDef := "DE850" ; cLangIDBase := "gl" ; Exit
         Case "ID"     ; cCPDef := "EN"    ; cLangIDBase := "id" ; Exit
         Case "IT"     ; cCPDef := "DE850" ; cLangIDBase := "it" ; Exit
         // INCOMPATIBILITY: "KO" (Korean) using CP949 is not supported anymore.
         Case "NL"     ; cCPDef := "EN"    ; cLangIDBase := "nl" ; Exit
         Case "PT"     ; cCPDef := "DE850" ; cLangIDBase := "pt" ; Exit
         Case "RO"     ; cCPDef := "CS852" ; cLangIDBase := "ro" ; Exit
         Case "SV"     ; cCPDef := "DE850" ; cLangIDBase := "sv" ; Exit
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

   Return __hb_langSelect(cLangID)

Static Function __LangStdToLangHb(cLangStd)

   Local cLangHb := ""

   IF hb_IsString(cLangStd)
      Switch Lower(StrTran(cLangStd, "_", "-"))
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
#endif
      Case "be"         ; cLangHb := "be" ; Exit
      Case "bg-bg"
      Case "bg"         ; cLangHb := "bg" ; Exit
      Case "ca-es"
      Case "ca"         ; cLangHb := "ca" ; Exit
#if 0
      Case "cy-gb"      ; Exit
#endif
      Case "cs-cz"
      Case "cs"         ; cLangHb := "cs" ; Exit
#if 0
      Case "da-dk"      ; Exit
      Case "da"         ; Exit
#endif
      Case "de-at"      ; cLangHb := "de_at" ; Exit
      Case "de-ch"
      Case "de-de"
      Case "de-li"
      Case "de-lu"
      Case "de"         ; cLangHb := "de" ; Exit
#if 0
      Case "div-mv"     ; Exit
      Case "div"        ; Exit
#endif
      Case "el-gr"
      Case "el"         ; cLangHb := "el" ; Exit
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
      Case "en"         ; cLangHb := "en" ; Exit
      Case "eo"         ; cLangHb := "eo" ; Exit
      Case "es-419"     ; cLangHb := "es_419" ; Exit
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
      Case "es"         ; cLangHb := "es" ; Exit
#if 0
      Case "et-ee"      ; Exit
      Case "et"         ; Exit
#endif
      Case "eu-es"
      Case "eu"         ; cLangHb := "eu" ; Exit
#if 0
      Case "fa-ir"      ; Exit
      Case "fa"         ; Exit
      Case "fi-fi"      ; Exit
      Case "fi"         ; Exit
      Case "fo-fo"      ; Exit
      Case "fo"         ; Exit
#endif
      Case "fr-be"
      Case "fr-ca"
      Case "fr-ch"
      Case "fr-fr"
      Case "fr-lu"
      Case "fr-mc"
      Case "fr"         ; cLangHb := "fr" ; Exit
      Case "gl-es"
      Case "gl"         ; cLangHb := "gl" ; Exit
#if 0
      Case "gu-in"      ; Exit
      Case "gu"         ; Exit
#endif
      Case "he-il"
      Case "he"         ; cLangHb := "he" ; Exit
#if 0
      Case "hi-in"      ; Exit
      Case "hi"         ; Exit
#endif
      Case "hr-hr"
      Case "hr"         ; cLangHb := "hr" ; Exit
      Case "hu-hu"
      Case "hu"         ; cLangHb := "hu" ; Exit
#if 0
      Case "hy-am"      ; Exit
      Case "hy"         ; Exit
#endif
      Case "id-id"
      Case "id"         ; cLangHb := "id" ; Exit
      Case "is-is"
      Case "is"         ; cLangHb := "is" ; Exit
      Case "it-ch"
      Case "it-it"
      Case "it"         ; cLangHb := "it" ; Exit
#if 0
      Case "ja-jp"      ; Exit
      Case "ja"         ; Exit
      Case "ka-ge"      ; Exit
      Case "ka"         ; Exit
      Case "kk-kz"      ; Exit
      Case "kk"         ; Exit
      Case "kn-in"      ; Exit
      Case "kn"         ; Exit
#endif
      Case "ko-kr"
      Case "ko"         ; cLangHb := "ko" ; Exit
#if 0
      Case "kok-in"     ; Exit
      Case "kok"        ; Exit
      Case "ky-kz"      ; Exit
      Case "ky"         ; Exit
#endif
      Case "lt-lt"
      Case "lt"         ; cLangHb := "lt" ; Exit
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
      Case "nl"         ; cLangHb := "nl" ; Exit
#if 0
      Case "nn-no"      ; Exit
      Case "no"         ; Exit
      Case "pa-in"      ; Exit
      Case "pa"         ; Exit
#endif
      Case "pl-pl"
      Case "pl"         ; cLangHb := "pl" ; Exit
      Case "pt-br"      ; cLangHb := "pt_br" ; Exit
      Case "pt-pt"
      Case "pt"         ; cLangHb := "pt" ; Exit
      Case "ro-ro"
      Case "ro"         ; cLangHb := "ro" ; Exit
      Case "ru-ru"
      Case "ru"         ; cLangHb := "ru" ; Exit
#if 0
      Case "sa-in"      ; Exit
      Case "sa"         ; Exit
#endif
      Case "sk-sk"
      Case "sk"         ; cLangHb := "sk" ; Exit
      Case "sl-si"
      Case "sl"         ; cLangHb := "sl" ; Exit
#if 0
      Case "sq-al"      ; Exit
      Case "sq"         ; Exit
#endif
      Case "sr-sp-cyrl" ; cLangHb := "sr_cyr" ; Exit
      Case "sr-sp-latn" ; cLangHb := "sr_lat" ; Exit
      Case "sv-fi"
      Case "sv-se"
      Case "sv"         ; cLangHb := "sv" ; Exit
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
      Case "tr"         ; cLangHb := "tr" ; Exit
#if 0
      Case "tt-ru"      ; Exit
      Case "tt"         ; Exit
#endif
      Case "uk-ua"
      Case "uk"         ; cLangHb := "uk" ; Exit
#if 0
      Case "ur-pk"      ; Exit
      Case "ur"         ; Exit
      Case "uz-uz-cyrl" ; Exit
      Case "uz-uz-latn" ; Exit
      Case "uz"         ; Exit
      Case "vi-vn"      ; Exit
      Case "vi"         ; Exit
#endif
      Case "zh-chs"     ; cLangHb := "zh_sim" ; Exit
      Case "zh-cht"
      Case "zh-cn"
      Case "zh-hk"
      Case "zh-mo"
      Case "zh-sg"
      Case "zh-tw"
      Case "zh"         ; cLangHb := "zh" ; Exit
      EndSwitch
   ENDIF

   Return cLangHb
