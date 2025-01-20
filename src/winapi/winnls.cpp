//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2025 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2025 Marcos Antonio Gambeta
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// NOTE: source code generated with the help of a code generator

#include <windows.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

// WINBASEAPI WINBOOL WINAPI IsValidCodePage(UINT CodePage)
HB_FUNC(WAISVALIDCODEPAGE)
{
  wa_ret_BOOL(IsValidCodePage(wa_par_UINT(1)));
}

// WINBASEAPI UINT WINAPI GetACP(void)
HB_FUNC(WAGETACP)
{
  wa_ret_UINT(GetACP());
}

// WINBASEAPI WINBOOL WINAPI IsDBCSLeadByteEx(UINT CodePage, BYTE TestChar)
HB_FUNC(WAISDBCSLEADBYTEEX)
{
  wa_ret_BOOL(IsDBCSLeadByteEx(wa_par_UINT(1), wa_par_BYTE(2)));
}

// WINBASEAPI UINT WINAPI GetOEMCP(void)
HB_FUNC(WAGETOEMCP)
{
  wa_ret_UINT(GetOEMCP());
}

// WINBASEAPI int WINAPI CompareStringA(LCID Locale, DWORD dwCmpFlags, PCNZCH lpString1, int cchCount1, PCNZCH lpString2, int cchCount2)

// WINBASEAPI int WINAPI LCMapStringW(LCID Locale, DWORD dwMapFlags, LPCWSTR lpSrcStr, int cchSrc, LPWSTR lpDestStr, int cchDest)

// WINBASEAPI int WINAPI LCMapStringA(LCID Locale, DWORD dwMapFlags, LPCSTR lpSrcStr, int cchSrc, LPSTR lpDestStr, int cchDest)

// WINBASEAPI int WINAPI GetLocaleInfoW(LCID Locale, LCTYPE LCType, LPWSTR lpLCData, int cchData)

// WINBASEAPI int WINAPI GetLocaleInfoA(LCID Locale, LCTYPE LCType, LPSTR lpLCData, int cchData)

// WINBASEAPI WINBOOL WINAPI IsDBCSLeadByte(BYTE TestChar)
HB_FUNC(WAISDBCSLEADBYTE)
{
  wa_ret_BOOL(IsDBCSLeadByte(wa_par_BYTE(1)));
}

// WINBASEAPI int WINAPI GetNumberFormatA(LCID Locale, DWORD dwFlags, LPCSTR lpValue, CONST NUMBERFMTA *lpFormat, LPSTR lpNumberStr, int cchNumber)

// WINBASEAPI int WINAPI GetNumberFormatW(LCID Locale, DWORD dwFlags, LPCWSTR lpValue, CONST NUMBERFMTW *lpFormat, LPWSTR lpNumberStr, int cchNumber)

// WINBASEAPI int WINAPI GetCurrencyFormatA(LCID Locale, DWORD dwFlags, LPCSTR lpValue, CONST CURRENCYFMTA *lpFormat, LPSTR lpCurrencyStr, int cchCurrency)

// WINBASEAPI int WINAPI GetCurrencyFormatW(LCID Locale, DWORD dwFlags, LPCWSTR lpValue, CONST CURRENCYFMTW *lpFormat, LPWSTR lpCurrencyStr, int cchCurrency)

// WINBASEAPI WINBOOL WINAPI EnumCalendarInfoA(CALINFO_ENUMPROCA lpCalInfoEnumProc, LCID Locale, CALID Calendar, CALTYPE CalType)

// WINBASEAPI WINBOOL WINAPI EnumCalendarInfoW(CALINFO_ENUMPROCW lpCalInfoEnumProc, LCID Locale, CALID Calendar, CALTYPE CalType)

// WINBASEAPI WINBOOL WINAPI EnumCalendarInfoExA(CALINFO_ENUMPROCEXA lpCalInfoEnumProcEx, LCID Locale, CALID Calendar, CALTYPE CalType)

// WINBASEAPI WINBOOL WINAPI EnumCalendarInfoExW(CALINFO_ENUMPROCEXW lpCalInfoEnumProcEx, LCID Locale, CALID Calendar, CALTYPE CalType)

// WINBASEAPI WINBOOL WINAPI EnumTimeFormatsA(TIMEFMT_ENUMPROCA lpTimeFmtEnumProc, LCID Locale, DWORD dwFlags)

// WINBASEAPI WINBOOL WINAPI EnumTimeFormatsW(TIMEFMT_ENUMPROCW lpTimeFmtEnumProc, LCID Locale, DWORD dwFlags)

// WINBASEAPI WINBOOL WINAPI EnumDateFormatsA(DATEFMT_ENUMPROCA lpDateFmtEnumProc, LCID Locale, DWORD dwFlags)

// WINBASEAPI WINBOOL WINAPI EnumDateFormatsW(DATEFMT_ENUMPROCW lpDateFmtEnumProc, LCID Locale, DWORD dwFlags)

// WINBASEAPI WINBOOL WINAPI EnumDateFormatsExA(DATEFMT_ENUMPROCEXA lpDateFmtEnumProcEx, LCID Locale, DWORD dwFlags)

// WINBASEAPI WINBOOL WINAPI EnumDateFormatsExW(DATEFMT_ENUMPROCEXW lpDateFmtEnumProcEx, LCID Locale, DWORD dwFlags)

// WINBASEAPI WINBOOL WINAPI IsValidLanguageGroup(LGRPID LanguageGroup, DWORD dwFlags)

// WINBASEAPI WINBOOL WINAPI GetNLSVersion(NLS_FUNCTION Function, LCID Locale, LPNLSVERSIONINFO lpVersionInformation)

// WINBASEAPI WINBOOL WINAPI IsNLSDefinedString(NLS_FUNCTION Function, DWORD dwFlags, LPNLSVERSIONINFO lpVersionInformation, LPCWSTR lpString, INT cchStr)

// WINBASEAPI WINBOOL WINAPI IsValidLocale(LCID Locale, DWORD dwFlags)
HB_FUNC(WAISVALIDLOCALE)
{
  wa_ret_BOOL(IsValidLocale(wa_par_LCID(1), wa_par_DWORD(2)));
}

// WINBASEAPI WINBOOL WINAPI SetLocaleInfoA(LCID Locale, LCTYPE LCType, LPCSTR lpLCData)
#if 0
HB_FUNC(WASETLOCALEINFOA)
{
  wa_ret_BOOL(SetLocaleInfoA(wa_par_LCID(1), wa_par_LCTYPE(2), wa_par_LPCSTR(3)));
}
#endif

// WINBASEAPI WINBOOL WINAPI SetLocaleInfoW(LCID Locale, LCTYPE LCType, LPCWSTR lpLCData)
#if 0
HB_FUNC(WASETLOCALEINFOW)
{
  wa_ret_BOOL(SetLocaleInfoW(wa_par_LCID(1), wa_par_LCTYPE(2), wa_par_LPCWSTR(3)));
}
#endif

HB_FUNC(WASETLOCALEINFO)
{
  void *str{};
  wa_ret_BOOL(SetLocaleInfo(wa_par_LCID(1), wa_par_LCTYPE(2), HB_PARSTR(3, &str, nullptr)));
  hb_strfree(str);
}

// WINBASEAPI int WINAPI GetCalendarInfoA(LCID Locale, CALID Calendar, CALTYPE CalType, LPSTR lpCalData, int cchData, LPDWORD lpValue)

// WINBASEAPI int WINAPI GetCalendarInfoW(LCID Locale, CALID Calendar, CALTYPE CalType, LPWSTR lpCalData, int cchData, LPDWORD lpValue)

// WINBASEAPI WINBOOL WINAPI SetCalendarInfoA(LCID Locale, CALID Calendar, CALTYPE CalType, LPCSTR lpCalData)
#if 0
HB_FUNC(WASETCALENDARINFOA)
{
  wa_ret_BOOL(SetCalendarInfoA(wa_par_LCID(1), wa_par_CALID(2), wa_par_CALTYPE(3), wa_par_LPCSTR(4)));
}
#endif

// WINBASEAPI WINBOOL WINAPI SetCalendarInfoW(LCID Locale, CALID Calendar, CALTYPE CalType, LPCWSTR lpCalData)
#if 0
HB_FUNC(WASETCALENDARINFOW)
{
  wa_ret_BOOL(SetCalendarInfoW(wa_par_LCID(1), wa_par_CALID(2), wa_par_CALTYPE(3), wa_par_LPCWSTR(4)));
}
#endif

HB_FUNC(WASETCALENDARINFO)
{
  void *str{};
  wa_ret_BOOL(SetCalendarInfo(wa_par_LCID(1), wa_par_CALID(2), wa_par_CALTYPE(3), HB_PARSTR(4, &str, nullptr)));
  hb_strfree(str);
}

#if WINVER >= 0x0600

// WINBASEAPI int WINAPI GetDurationFormat(LCID Locale, DWORD dwFlags, CONST SYSTEMTIME *lpDuration, ULONGLONG ullDuration, LPCWSTR lpFormat, LPWSTR lpDurationStr, int cchDuration)

// WINBASEAPI int WINAPI FindNLSString(LCID Locale, DWORD dwFindNLSStringFlags, LPCWSTR lpStringSource, int cchSource, LPCWSTR lpStringValue, int cchValue, LPINT pcchFound)

#endif

#if WINVER >= 0x0601

// WINBASEAPI WINBOOL WINAPI LoadStringByReference(DWORD Flags, PCWSTR Language, PCWSTR SourceString, PWSTR Buffer, ULONG cchBuffer, PCWSTR Directory, PULONG pcchBufferOut)

#endif

// WINBASEAPI int WINAPI GetGeoInfoA(GEOID Location, GEOTYPE GeoType, LPSTR lpGeoData, int cchData, LANGID LangId)

// WINBASEAPI int WINAPI GetGeoInfoW(GEOID Location, GEOTYPE GeoType, LPWSTR lpGeoData, int cchData, LANGID LangId)

// WINBASEAPI WINBOOL WINAPI EnumSystemGeoID(GEOCLASS GeoClass, GEOID ParentGeoId, GEO_ENUMPROC lpGeoEnumProc)

// WINBASEAPI GEOID WINAPI GetUserGeoID(GEOCLASS GeoClass)
HB_FUNC(WAGETUSERGEOID)
{
  wa_ret_GEOID(GetUserGeoID(wa_par_GEOCLASS(1)));
}

// WINBASEAPI WINBOOL WINAPI GetCPInfo(UINT CodePage, LPCPINFO lpCPInfo)

// WINBASEAPI WINBOOL WINAPI GetCPInfoExA(UINT CodePage, DWORD dwFlags, LPCPINFOEXA lpCPInfoEx)

// WINBASEAPI WINBOOL WINAPI GetCPInfoExW(UINT CodePage, DWORD dwFlags, LPCPINFOEXW lpCPInfoEx)

#if WINVER >= 0x0600

// WINBASEAPI int WINAPI LCIDToLocaleName(LCID Locale, LPWSTR lpName, int cchName, DWORD dwFlags)

// WINBASEAPI LCID WINAPI LocaleNameToLCID(LPCWSTR lpName, DWORD dwFlags)
HB_FUNC(WALOCALENAMETOLCID)
{
  void *str{};
  wa_ret_LCID(LocaleNameToLCID(HB_PARSTR(1, &str, nullptr), wa_par_DWORD(2)));
  hb_strfree(str);
}

#endif

// WINBASEAPI WINBOOL WINAPI SetUserGeoID(GEOID GeoId)
HB_FUNC(WASETUSERGEOID)
{
  wa_ret_BOOL(SetUserGeoID(wa_par_GEOID(1)));
}

// WINBASEAPI LCID WINAPI ConvertDefaultLocale(LCID Locale)
HB_FUNC(WACONVERTDEFAULTLOCALE)
{
  wa_ret_LCID(ConvertDefaultLocale(wa_par_LCID(1)));
}

// WINBASEAPI LCID WINAPI GetThreadLocale(void)
HB_FUNC(WAGETTHREADLOCALE)
{
  wa_ret_LCID(GetThreadLocale());
}

// WINBASEAPI WINBOOL WINAPI SetThreadLocale(LCID Locale)
HB_FUNC(WASETTHREADLOCALE)
{
  wa_ret_BOOL(SetThreadLocale(wa_par_LCID(1)));
}

// WINBASEAPI LANGID WINAPI GetSystemDefaultUILanguage(void)
HB_FUNC(WAGETSYSTEMDEFAULTUILANGUAGE)
{
  wa_ret_LANGID(GetSystemDefaultUILanguage());
}

// WINBASEAPI LANGID WINAPI GetUserDefaultUILanguage(void)
HB_FUNC(WAGETUSERDEFAULTUILANGUAGE)
{
  wa_ret_LANGID(GetUserDefaultUILanguage());
}

// WINBASEAPI LANGID WINAPI GetSystemDefaultLangID(void)
HB_FUNC(WAGETSYSTEMDEFAULTLANGID)
{
  wa_ret_LANGID(GetSystemDefaultLangID());
}

// WINBASEAPI LANGID WINAPI GetUserDefaultLangID(void)
HB_FUNC(WAGETUSERDEFAULTLANGID)
{
  wa_ret_LANGID(GetUserDefaultLangID());
}

// WINBASEAPI LCID WINAPI GetSystemDefaultLCID(void)
HB_FUNC(WAGETSYSTEMDEFAULTLCID)
{
  wa_ret_LCID(GetSystemDefaultLCID());
}

// WINBASEAPI LCID WINAPI GetUserDefaultLCID(void)
HB_FUNC(WAGETUSERDEFAULTLCID)
{
  wa_ret_LCID(GetUserDefaultLCID());
}

// WINBASEAPI LANGID WINAPI SetThreadUILanguage(LANGID LangId)
HB_FUNC(WASETTHREADUILANGUAGE)
{
  wa_ret_LANGID(SetThreadUILanguage(wa_par_LANGID(1)));
}

// WINBASEAPI WINBOOL WINAPI GetStringTypeExA(LCID Locale, DWORD dwInfoType, LPCSTR lpSrcStr, int cchSrc, LPWORD lpCharType)

// WINBASEAPI WINBOOL WINAPI GetStringTypeA(LCID Locale, DWORD dwInfoType, LPCSTR lpSrcStr, int cchSrc, LPWORD lpCharType)

// WINBASEAPI int WINAPI FoldStringA(DWORD dwMapFlags, LPCSTR lpSrcStr, int cchSrc, LPSTR lpDestStr, int cchDest)

// WINBASEAPI WINBOOL WINAPI EnumSystemLocalesA(LOCALE_ENUMPROCA lpLocaleEnumProc, DWORD dwFlags)

// WINBASEAPI WINBOOL WINAPI EnumSystemLocalesW(LOCALE_ENUMPROCW lpLocaleEnumProc, DWORD dwFlags)

// WINBASEAPI WINBOOL WINAPI EnumSystemLanguageGroupsA(LANGUAGEGROUP_ENUMPROCA lpLanguageGroupEnumProc, DWORD dwFlags, LONG_PTR lParam)

// WINBASEAPI WINBOOL WINAPI EnumSystemLanguageGroupsW(LANGUAGEGROUP_ENUMPROCW lpLanguageGroupEnumProc, DWORD dwFlags, LONG_PTR lParam)

// WINBASEAPI WINBOOL WINAPI EnumLanguageGroupLocalesA(LANGGROUPLOCALE_ENUMPROCA lpLangGroupLocaleEnumProc, LGRPID LanguageGroup, DWORD dwFlags, LONG_PTR lParam)

// WINBASEAPI WINBOOL WINAPI EnumLanguageGroupLocalesW(LANGGROUPLOCALE_ENUMPROCW lpLangGroupLocaleEnumProc, LGRPID LanguageGroup, DWORD dwFlags, LONG_PTR lParam)

// WINBASEAPI WINBOOL WINAPI EnumUILanguagesA(UILANGUAGE_ENUMPROCA lpUILanguageEnumProc, DWORD dwFlags, LONG_PTR lParam)

// WINBASEAPI WINBOOL WINAPI EnumUILanguagesW(UILANGUAGE_ENUMPROCW lpUILanguageEnumProc, DWORD dwFlags, LONG_PTR lParam)

#if WINVER >= 0x0600

// WINBASEAPI LANGID WINAPI GetThreadUILanguage(void)
HB_FUNC(WAGETTHREADUILANGUAGE)
{
  wa_ret_LANGID(GetThreadUILanguage());
}

// WINBASEAPI WINBOOL WINAPI GetProcessPreferredUILanguages(DWORD dwFlags, PULONG pulNumLanguages, PZZWSTR pwszLanguagesBuffer, PULONG pcchLanguagesBuffer)

// WINBASEAPI WINBOOL WINAPI SetProcessPreferredUILanguages(DWORD dwFlags, PCZZWSTR pwszLanguagesBuffer, PULONG pulNumLanguages)

// WINBASEAPI WINBOOL WINAPI GetUserPreferredUILanguages(DWORD dwFlags, PULONG pulNumLanguages, PZZWSTR pwszLanguagesBuffer, PULONG pcchLanguagesBuffer)

// WINBASEAPI WINBOOL WINAPI GetSystemPreferredUILanguages(DWORD dwFlags, PULONG pulNumLanguages, PZZWSTR pwszLanguagesBuffer, PULONG pcchLanguagesBuffer)

// WINBASEAPI WINBOOL WINAPI GetThreadPreferredUILanguages(DWORD dwFlags, PULONG pulNumLanguages, PZZWSTR pwszLanguagesBuffer, PULONG pcchLanguagesBuffer)

// WINBASEAPI WINBOOL WINAPI SetThreadPreferredUILanguages(DWORD dwFlags, PCZZWSTR pwszLanguagesBuffer, PULONG pulNumLanguages)

// WINBASEAPI WINBOOL WINAPI GetFileMUIInfo(DWORD dwFlags, PCWSTR pcwszFilePath, PFILEMUIINFO pFileMUIInfo, DWORD *pcbFileMUIInfo)

// WINBASEAPI WINBOOL WINAPI GetFileMUIPath(DWORD dwFlags, PCWSTR pcwszFilePath, PWSTR pwszLanguage, PULONG pcchLanguage, PWSTR pwszFileMUIPath, PULONG pcchFileMUIPath, PULONGLONG pululEnumerator)

// WINBASEAPI WINBOOL WINAPI GetUILanguageInfo(DWORD dwFlags, PCZZWSTR pwmszLanguage, PZZWSTR pwszFallbackLanguages, PDWORD pcchFallbackLanguages, PDWORD pAttributes)

// WINBASEAPI WINBOOL WINAPI NotifyUILanguageChange(DWORD dwFlags, PCWSTR pcwstrNewLanguage, PCWSTR pcwstrPreviousLanguage, DWORD dwReserved, PDWORD pdwStatusRtrn)

#endif

// WINBASEAPI WINBOOL WINAPI EnumSystemCodePagesA(CODEPAGE_ENUMPROCA lpCodePageEnumProc, DWORD dwFlags)

// WINBASEAPI WINBOOL WINAPI EnumSystemCodePagesW(CODEPAGE_ENUMPROCW lpCodePageEnumProc, DWORD dwFlags)

// WINNORMALIZEAPI int WINAPI NormalizeString(NORM_FORM NormForm, LPCWSTR lpSrcString, int cwSrcLength, LPWSTR lpDstString, int cwDstLength)

// WINNORMALIZEAPI WINBOOL WINAPI IsNormalizedString(NORM_FORM NormForm, LPCWSTR lpString, int cwLength)

// WINNORMALIZEAPI int WINAPI IdnToAscii(DWORD dwFlags, LPCWSTR lpUnicodeCharStr, int cchUnicodeChar, LPWSTR lpASCIICharStr, int cchASCIIChar)

// WINNORMALIZEAPI int WINAPI IdnToNameprepUnicode(DWORD dwFlags, LPCWSTR lpUnicodeCharStr, int cchUnicodeChar, LPWSTR lpNameprepCharStr, int cchNameprepChar)

// WINNORMALIZEAPI int WINAPI IdnToUnicode(DWORD dwFlags, LPCWSTR lpASCIICharStr, int cchASCIIChar, LPWSTR lpUnicodeCharStr, int cchUnicodeChar)

// WINBASEAPI WINBOOL WINAPI VerifyScripts(DWORD dwFlags, LPCWSTR lpLocaleScripts, int cchLocaleScripts, LPCWSTR lpTestScripts, int cchTestScripts)

// WINBASEAPI int WINAPI GetStringScripts(DWORD dwFlags, LPCWSTR lpString, int cchString, LPWSTR lpScripts, int cchScripts)

// WINBASEAPI int WINAPI GetLocaleInfoEx(LPCWSTR lpLocaleName, LCTYPE LCType, LPWSTR lpLCData, int cchData)

// WINBASEAPI int WINAPI GetCalendarInfoEx(LPCWSTR lpLocaleName, CALID Calendar, LPCWSTR lpReserved, CALTYPE CalType, LPWSTR lpCalData, int cchData, LPDWORD lpValue)

// WINBASEAPI int WINAPI GetDurationFormatEx(LPCWSTR lpLocaleName, DWORD dwFlags, CONST SYSTEMTIME *lpDuration, ULONGLONG ullDuration, LPCWSTR lpFormat, LPWSTR lpDurationStr, int cchDuration)

// WINBASEAPI int WINAPI GetNumberFormatEx(LPCWSTR lpLocaleName, DWORD dwFlags, LPCWSTR lpValue, CONST NUMBERFMTW *lpFormat, LPWSTR lpNumberStr, int cchNumber)

// WINBASEAPI int WINAPI GetCurrencyFormatEx(LPCWSTR lpLocaleName, DWORD dwFlags, LPCWSTR lpValue, CONST CURRENCYFMTW *lpFormat, LPWSTR lpCurrencyStr, int cchCurrency)

// WINBASEAPI int WINAPI GetUserDefaultLocaleName(LPWSTR lpLocaleName, int cchLocaleName)

// WINBASEAPI int WINAPI GetSystemDefaultLocaleName(LPWSTR lpLocaleName, int cchLocaleName)

// WINBASEAPI WINBOOL WINAPI GetNLSVersionEx(NLS_FUNCTION function, LPCWSTR lpLocaleName, LPNLSVERSIONINFOEX lpVersionInformation)

// WINBASEAPI int WINAPI FindNLSStringEx(LPCWSTR lpLocaleName, DWORD dwFindNLSStringFlags, LPCWSTR lpStringSource, int cchSource, LPCWSTR lpStringValue, int cchValue, LPINT pcchFound, LPNLSVERSIONINFO lpVersionInformation, LPVOID lpReserved, LPARAM sortHandle)

// WINBASEAPI int WINAPI LCMapStringEx(LPCWSTR lpLocaleName, DWORD dwMapFlags, LPCWSTR lpSrcStr, int cchSrc, LPWSTR lpDestStr, int cchDest, LPNLSVERSIONINFO lpVersionInformation, LPVOID lpReserved, LPARAM sortHandle)

// WINBASEAPI WINBOOL WINAPI IsValidLocaleName(LPCWSTR lpLocaleName)
#if 0 // TODO: Windows Vista or upper
HB_FUNC(WAISVALIDLOCALENAME)
{
  void *str{};
  wa_ret_BOOL(IsValidLocaleName(HB_PARSTR(1, &str, nullptr)));
  hb_strfree(str);
}
#endif

// WINBASEAPI WINBOOL WINAPI EnumCalendarInfoExEx(CALINFO_ENUMPROCEXEX pCalInfoEnumProcExEx, LPCWSTR lpLocaleName, CALID Calendar, LPCWSTR lpReserved, CALTYPE CalType, LPARAM lParam)

// WINBASEAPI WINBOOL WINAPI EnumDateFormatsExEx(DATEFMT_ENUMPROCEXEX lpDateFmtEnumProcExEx, LPCWSTR lpLocaleName, DWORD dwFlags, LPARAM lParam)

// WINBASEAPI WINBOOL WINAPI EnumTimeFormatsEx(TIMEFMT_ENUMPROCEX lpTimeFmtEnumProcEx, LPCWSTR lpLocaleName, DWORD dwFlags, LPARAM lParam)

// WINBASEAPI WINBOOL WINAPI EnumSystemLocalesEx(LOCALE_ENUMPROCEX lpLocaleEnumProcEx, DWORD dwFlags, LPARAM lParam, LPVOID lpReserved)

#if WINVER >= 0x0601

// WINBASEAPI int WINAPI ResolveLocaleName(LPCWSTR lpNameToResolve, LPWSTR lpLocaleName, int cchLocaleName)

#endif

#if WINVER >= 0x0602

// WINBASEAPI DWORD WINAPI IsValidNLSVersion(NLS_FUNCTION function, LPCWSTR lpLocaleName, LPNLSVERSIONINFOEX lpVersionInformation)

#endif
