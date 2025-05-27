// Pavel Tsarenko <tpe2@mail.ru>

#if defined(__BORLANDC__)
//#define HB_CP_UPPER     "A\xC4\x80" "BC\xC4\x8C" "DE\xC4\x92" "FG\xC4\xA2" "HI\xC4\xAA" "JK\xC4\xB6" "L\xC4\xBB" "MN\xC5\x85" "OPQRS\xC5\xA0" "TU\xC5\xAA" "VWXYZ\xC5\xBD"
constexpr const char *HB_CP_UPPER = "A\xC4\x80" "BC\xC4\x8C" "DE\xC4\x92" "FG\xC4\xA2" "HI\xC4\xAA" "JK\xC4\xB6" "L\xC4\xBB" "MN\xC5\x85" "OPQRS\xC5\xA0" "TU\xC5\xAA" "VWXYZ\xC5\xBD";
//#define HB_CP_LOWER     "a\xC4\x81" "bc\xC4\x8D" "de\xC4\x93" "fg\xC4\xA3" "hi\xC4\xAB" "jk\xC4\xB7" "l\xC4\xBC" "mn\xC5\x86" "opqrs\xC5\xA1" "tu\xC5\xAB" "vwxyz\xC5\xBE"
constexpr const char *HB_CP_LOWER = "a\xC4\x81" "bc\xC4\x8D" "de\xC4\x93" "fg\xC4\xA3" "hi\xC4\xAB" "jk\xC4\xB7" "l\xC4\xBC" "mn\xC5\x86" "opqrs\xC5\xA1" "tu\xC5\xAB" "vwxyz\xC5\xBE";
#else
//#define HB_CP_UPPER     "AĀBCČDEĒFGĢHIĪJKĶLĻMNŅOPQRSŠTUŪVWXYZŽ"
constexpr const char *HB_CP_UPPER = "AĀBCČDEĒFGĢHIĪJKĶLĻMNŅOPQRSŠTUŪVWXYZŽ";
//#define HB_CP_LOWER     "aābcčdeēfgģhiījkķlļmnņopqrsštuūvwxyzž"
constexpr const char *HB_CP_LOWER = "aābcčdeēfgģhiījkķlļmnņopqrsštuūvwxyzž";
#endif
