/* Pavel Tsarenko <tpe2@mail.ru> */


#if defined(__BORLANDC__)
#define HB_CP_UPPER     "ABCDEFG\xC4\xA2" "HIJKLMNOPQRS\xC5\xA0" "Z\xC5\xBD" "TUVW\xC3\x95\xC3\x84\xC3\x96\xC3\x9C" "XY"
#define HB_CP_LOWER     "abcdefg\xC4\xA3" "hijklmnopqrs\xC5\xA1" "z\xC5\xBE" "tuvw\xC3\xB5\xC3\xA4\xC3\xB6\xC3\xBC" "xy"
#else
#define HB_CP_UPPER     "ABCDEFGĢHIJKLMNOPQRSŠZŽTUVWÕÄÖÜXY"
#define HB_CP_LOWER     "abcdefgģhijklmnopqrsšzžtuvwõäöüxy"
#endif
