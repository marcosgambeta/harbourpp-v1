//
// BBS.......: The Dark Knight Returns
// Date......: 1993-05-24
//
// This is an original work by Andy M Leighton and is placed in the
// public domain.
//

#include "hbapi.hpp"

HB_FUNC(GT_CHARODD)
{
  if (HB_ISCHAR(1))
  {
    auto s1 = hb_parc(1);
    HB_ISIZ len = hb_parclen(1);
    HB_ISIZ i;

    auto s2 = static_cast<char *>(hb_xgrab(len / 2 + 1)); /* grab us some memory to work with */

    for (i = 0; i <= len; i += 2)
      s2[i / 2] = s1[i] & 0x7f;

    hb_retclen_buffer(s2, len / 2);
  }
  else
    hb_retc_null();
}
