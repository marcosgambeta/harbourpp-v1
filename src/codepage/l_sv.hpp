// Klas Engwall <klas dot engwall at engwall dot com>

// NOTE: Following collations contains extra characters
//       compared to pure Swedish alphabet.
//       Quoting Klas:
//       "the philosophy behind the extended collation strings [...]
//       was to allow *all* alphabetical characters provided by the
//       [Windows ANSI] codepage, including those not normally used
//       in Swedish words and names, to be sorted according to their
//       alphabetical context rather than an arbitrary Asc() value
//       - as suggested by the Swedish Language Council."

#if defined(__BORLANDC__)
#define HB_CP_UPPER     "A~\xC3\x81~\xC3\x80~\xC3\x82~\xC3\x83" "BC~\xC3\x87" "D~\xC3\x90" "E~\xC3\x89~\xC3\x88~\xC3\x8A~\xC3\x8B" "FGHI~\xC3\x8D~\xC3\x8C~\xC3\x8E~\xC3\x8F" "JKLMN~\xC3\x91" "O~\xC3\x93~\xC3\x92~\xC3\x94~\xC3\x95" "PQRS~\xC5\xA0" "TU~\xC3\x9A~\xC3\x99~\xC3\x9B" "V~WXY~\xC3\x9D~\xC5\xB8~\xC3\x9C" "Z~\xC5\xBD\xC3\x85\xC3\x84~\xC3\x86\xC3\x96~\xC3\x98~\xC5\x92"
#define HB_CP_LOWER     "a~\xC3\xA1~\xC3\xA0~\xC3\xA2~\xC3\xA3" "bc~\xC3\xA7" "d~\xC3\xB0" "e~\xC3\xA9~\xC3\xA8~\xC3\xAA~\xC3\xAB" "fghi~\xC3\xAD~\xC3\xAC~\xC3\xAE~\xC3\xAF" "jklmn~\xC3\xB1" "o~\xC3\xB3~\xC3\xB2~\xC3\xB4~\xC3\xB5" "pqrs~\xC5\xA1" "tu~\xC3\xBA~\xC3\xB9~\xC3\xBB" "v~wxy~\xC3\xBD~\xC3\xBF~\xC3\xBC" "z~\xC5\xBE\xC3\xA5\xC3\xA4~\xC3\xA6\xC3\xB6~\xC3\xB8~\xC5\x93"
#else
#define HB_CP_UPPER     "A~Á~À~Â~ÃBC~ÇD~ÐE~É~È~Ê~ËFGHI~Í~Ì~Î~ÏJKLMN~ÑO~Ó~Ò~Ô~ÕPQRS~ŠTU~Ú~Ù~ÛV~WXY~Ý~Ÿ~ÜZ~ŽÅÄ~ÆÖ~Ø~Œ"
#define HB_CP_LOWER     "a~á~à~â~ãbc~çd~ðe~é~è~ê~ëfghi~í~ì~î~ïjklmn~ño~ó~ò~ô~õpqrs~štu~ú~ù~ûv~wxy~ý~ÿ~üz~žåä~æö~ø~œ"
#endif
