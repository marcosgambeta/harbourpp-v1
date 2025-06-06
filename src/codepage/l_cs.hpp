// Vojtech Obrdlik <vobrdlik@centrum.cz>

#if defined(__BORLANDC__)
//#define HB_CP_UPPER     "A\xC3\x81\xC3\x84" "BC\xC4\x8C" "D\xC4\x8E" "E\xC3\x89\xC4\x9A\xC3\x8B" "FGH.CH.I\xC3\x8D" "JKLMN\xC5\x87" "O\xC3\x93\xC3\x96" "PQR\xC5\x98" "S\xC5\xA0" "T\xC5\xA4" "U\xC3\x9A\xC5\xAE\xC3\x9C" "VWXY\xC3\x9D" "Z\xC5\xBD"
constexpr const char *HB_CP_UPPER = "A\xC3\x81\xC3\x84" "BC\xC4\x8C" "D\xC4\x8E" "E\xC3\x89\xC4\x9A\xC3\x8B" "FGH.CH.I\xC3\x8D" "JKLMN\xC5\x87" "O\xC3\x93\xC3\x96" "PQR\xC5\x98" "S\xC5\xA0" "T\xC5\xA4" "U\xC3\x9A\xC5\xAE\xC3\x9C" "VWXY\xC3\x9D" "Z\xC5\xBD";
//#define HB_CP_LOWER     "a\xC3\xA1\xC3\xA4" "bc\xC4\x8D" "d\xC4\x8F" "e\xC3\xA9\xC4\x9B\xC3\xAB" "fgh.ch.i\xC3\xAD" "jklmn\xC5\x88" "o\xC3\xB3\xC3\xB6" "pqr\xC5\x99" "s\xC5\xA1" "t\xC5\xA5" "u\xC3\xBA\xC5\xAF\xC3\xBC" "vwxy\xC3\xBD" "z\xC5\xBE"
constexpr const char *HB_CP_LOWER = "a\xC3\xA1\xC3\xA4" "bc\xC4\x8D" "d\xC4\x8F" "e\xC3\xA9\xC4\x9B\xC3\xAB" "fgh.ch.i\xC3\xAD" "jklmn\xC5\x88" "o\xC3\xB3\xC3\xB6" "pqr\xC5\x99" "s\xC5\xA1" "t\xC5\xA5" "u\xC3\xBA\xC5\xAF\xC3\xBC" "vwxy\xC3\xBD" "z\xC5\xBE";
#else
//#define HB_CP_UPPER     "AÁÄBCČDĎEÉĚËFGH.CH.IÍJKLMNŇOÓÖPQRŘSŠTŤUÚŮÜVWXYÝZŽ"
constexpr const char *HB_CP_UPPER = "AÁÄBCČDĎEÉĚËFGH.CH.IÍJKLMNŇOÓÖPQRŘSŠTŤUÚŮÜVWXYÝZŽ";
//#define HB_CP_LOWER     "aáäbcčdďeéěëfgh.ch.iíjklmnňoóöpqrřsštťuúůüvwxyýzž"
constexpr const char *HB_CP_LOWER = "aáäbcčdďeéěëfgh.ch.iíjklmnňoóöpqrřsštťuúůüvwxyýzž";
#endif
