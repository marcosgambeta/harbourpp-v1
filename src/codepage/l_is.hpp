#if defined(__BORLANDC__)
//#define HB_CP_UPPER     "A\xC3\x81" "BCD\xC3\x90" "E\xC3\x89" "FGHI\xC3\x8D" "JKLMNO\xC3\x93" "PQRSTU\xC3\x9A" "VWXY\xC3\x9D" "Z\xC3\x9E\xC3\x86\xC3\x96"
constexpr const char *HB_CP_UPPER = "A\xC3\x81" "BCD\xC3\x90" "E\xC3\x89" "FGHI\xC3\x8D" "JKLMNO\xC3\x93" "PQRSTU\xC3\x9A" "VWXY\xC3\x9D" "Z\xC3\x9E\xC3\x86\xC3\x96";
//#define HB_CP_LOWER     "a\xC3\xA1" "bcd\xC3\xB0" "e\xC3\xA9" "fghi\xC3\xAD" "jklmno\xC3\xB3" "pqrstu\xC3\xBA" "vwxy\xC3\xBD" "z\xC3\xBE\xC3\xA6\xC3\xB6"
constexpr const char *HB_CP_LOWER = "a\xC3\xA1" "bcd\xC3\xB0" "e\xC3\xA9" "fghi\xC3\xAD" "jklmno\xC3\xB3" "pqrstu\xC3\xBA" "vwxy\xC3\xBD" "z\xC3\xBE\xC3\xA6\xC3\xB6";
#else
//#define HB_CP_UPPER     "AÁBCDÐEÉFGHIÍJKLMNOÓPQRSTUÚVWXYÝZÞÆÖ"
constexpr const char *HB_CP_UPPER = "AÁBCDÐEÉFGHIÍJKLMNOÓPQRSTUÚVWXYÝZÞÆÖ";
//#define HB_CP_LOWER     "aábcdðeéfghiíjklmnoópqrstuúvwxyýzþæö"
constexpr const char *HB_CP_LOWER = "aábcdðeéfghiíjklmnoópqrstuúvwxyýzþæö";
#endif
