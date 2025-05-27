// Guenther Steiner <byte-one@aon.at>

#if defined(__BORLANDC__)
//#define HB_CP_UPPER     "A\xC3\x84" "BCDEFGHIJKLMNO\xC3\x96" "PQRS TU\xC3\x9C" "VWXYZ"
constexpr const char *HB_CP_UPPER = "A\xC3\x84" "BCDEFGHIJKLMNO\xC3\x96" "PQRS TU\xC3\x9C" "VWXYZ";
//#define HB_CP_LOWER     "a\xC3\xA4" "bcdefghijklmno\xC3\xB6" "pqrs\xC3\x9F" "tu\xC3\xBC" "vwxyz"
constexpr const char *HB_CP_LOWER = "a\xC3\xA4" "bcdefghijklmno\xC3\xB6" "pqrs\xC3\x9F" "tu\xC3\xBC" "vwxyz";
#else
//#define HB_CP_UPPER     "AÄBCDEFGHIJKLMNOÖPQRS TUÜVWXYZ"
constexpr const char *HB_CP_UPPER = "AÄBCDEFGHIJKLMNOÖPQRS TUÜVWXYZ";
//#define HB_CP_LOWER     "aäbcdefghijklmnoöpqrsßtuüvwxyz"
constexpr const char *HB_CP_LOWER = "aäbcdefghijklmnoöpqrsßtuüvwxyz";
#endif
