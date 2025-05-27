// Viktor Szakats (vszakats.net/harbour)

// NOTE: Ä/ä have been added to work like sixhu852.obj
//       for S*ccessWare SIx Driver, but they're not part
//       of the Hungarian alphabet. [vszakats]

#if defined(__BORLANDC__)
//#define HB_CP_UPPER     "A\xC3\x81\xC3\x84" "BCDE\xC3\x89" "FGHI\xC3\x8D" "JKLMNO\xC3\x93\xC3\x96\xC5\x90" "PQRSTU\xC3\x9A\xC3\x9C\xC5\xB0" "VWXYZ"
constexpr const char *HB_CP_UPPER = "A\xC3\x81\xC3\x84" "BCDE\xC3\x89" "FGHI\xC3\x8D" "JKLMNO\xC3\x93\xC3\x96\xC5\x90" "PQRSTU\xC3\x9A\xC3\x9C\xC5\xB0" "VWXYZ";
//#define HB_CP_LOWER     "a\xC3\xA1\xC3\xA4" "bcde\xC3\xA9" "fghi\xC3\xAD" "jklmno\xC3\xB3\xC3\xB6\xC5\x91" "pqrstu\xC3\xBA\xC3\xBC\xC5\xB1" "vwxyz"
constexpr const char *HB_CP_LOWER = "a\xC3\xA1\xC3\xA4" "bcde\xC3\xA9" "fghi\xC3\xAD" "jklmno\xC3\xB3\xC3\xB6\xC5\x91" "pqrstu\xC3\xBA\xC3\xBC\xC5\xB1" "vwxyz";
#else
//#define HB_CP_UPPER     "AÁÄBCDEÉFGHIÍJKLMNOÓÖŐPQRSTUÚÜŰVWXYZ"
constexpr const char *HB_CP_UPPER = "AÁÄBCDEÉFGHIÍJKLMNOÓÖŐPQRSTUÚÜŰVWXYZ";
//#define HB_CP_LOWER     "aáäbcdeéfghiíjklmnoóöőpqrstuúüűvwxyz"
constexpr const char *HB_CP_LOWER = "aáäbcdeéfghiíjklmnoóöőpqrstuúüűvwxyz";
#endif
