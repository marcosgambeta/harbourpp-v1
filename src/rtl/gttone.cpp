//
// Low-level tone code common to some GT drivers
//
// Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
//
// the body of Tone() function from Windows taken from GTWIN created by
// the following authors:
// Copyright 1999 David G. Holm <dholm@jsd-llc.com>
// Copyright 1999-2006 Paul Tucker <ptucker@sympatico.ca>
// Copyright 2005 Andi Jahja <andij@aonlippo.co.id>
// Copyright 2005 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

// NOTE: User programs should never call this layer directly!

#include "hbgtcore.hpp"

#if defined(HB_OS_WIN)

#include <windows.h>

#undef HB_HAS_WIN9X_TONE

#if defined(HB_CPU_X86) && ((defined(_MSC_VER) && _MSC_VER < 1900) || defined(__BORLANDC__) || defined(__MINGW32__))

#define HB_HAS_WIN9X_TONE

#if defined(_MSC_VER)
#include <conio.h>
#endif

static int hb_Inp9x(unsigned short int usPort)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_Inp9x(%hu)", usPort));
#endif

  unsigned short int usVal;

#if (defined(__BORLANDC__) && !defined(__clang__))

  _DX = usPort;
  __emit__(0xEC);       // ASM  IN AL, DX
  __emit__(0x32, 0xE4); // ASM XOR AH, AH
  usVal = _AX;

#elif defined(__BORLANDC__)

  __asm {
               mov   dx, usPort
               xor   ax, ax
               in    al, dx
               mov   usVal, ax
  }

#elif defined(__MINGW32__)

  __asm__ __volatile__("inb %w1,%b0" : "=a"(usVal) : "Nd"(usPort));

#else

  usVal = static_cast<unsigned short int>(_inp(usPort));

#endif

  return usVal;
}

static int hb_Outp9x(unsigned short int usPort, unsigned short int usVal)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_Outp9x(%hu, %hu)", usPort, usVal));
#endif

#if (defined(__BORLANDC__) && !defined(__clang__))

  _DX = usPort;
  _AL = usVal;
  __emit__(0xEE); // ASM OUT DX, AL

#elif defined(__BORLANDC__)

  __asm {
               mov   dx, usPort
               mov   ax, usVal
               out   dx, al
  }

#elif defined(__MINGW32__)

  __asm__ __volatile__("outb %b0,%w1" : : "a"(usVal), "Nd"(usPort));

#else

  _outp(usPort, usVal);

#endif

  return usVal;
}

// dDurat is in seconds
static void hb_gt_w9xTone(double dFreq, double dDurat)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_w9xtone(%lf, %lf)", dFreq, dDurat));
#endif

  int uLSB, uMSB;
  unsigned long lAdjFreq;

  // sync with internal clock with very small time period
  hb_idleSleep(0.01);

  // Clipper ignores Tone() requests (but delays anyway) if Frequency is
  // less than < 20 hz (and so should we) to maintain compatibility ..

  if (dFreq >= 20.0) {
    // Setup Sound Control Port Registers and timer channel 2
    hb_Outp9x(67, 182);

    lAdjFreq = static_cast<unsigned long>(1193180 / dFreq);

    if (static_cast<long>(lAdjFreq) < 0) {
      uLSB = lAdjFreq + 65536;
    } else {
      uLSB = lAdjFreq % 256;
    }

    if (static_cast<long>(lAdjFreq) < 0) {
      uMSB = lAdjFreq + 65536;
    } else {
      uMSB = lAdjFreq / 256;
    }

    // set the frequency ( LSB, MSB )

    hb_Outp9x(66, static_cast<unsigned short int>(uLSB));
    hb_Outp9x(66, static_cast<unsigned short int>(uMSB));

    // Get current Port setting
    // enable Speaker Data & Timer gate bits
    // (00000011B is bitmask to enable sound)
    // Turn on Speaker - sound Tone for duration..

    hb_Outp9x(97, static_cast<unsigned short int>(hb_Inp9x(97)) | 3);

    hb_idleSleep(dDurat);

    // Read back current Port value for Reset
    // disable Speaker Data & Timer gate bits
    // (11111100B is bitmask to disable sound)
    // Turn off the Speaker !

    hb_Outp9x(97, hb_Inp9x(97) & 0xFC);
  } else {
    hb_idleSleep(dDurat);
  }
}

#endif

// dDurat is in seconds
static void hb_gt_wNtTone(double dFreq, double dDurat)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_wNtTone(%lf, %lf)", dFreq, dDurat));
#endif

  // Clipper ignores Tone() requests (but delays anyway) if Frequency is
  // less than < 20 hz.  Windows NT minimum is 37...

  if (dFreq >= 37.0) {
    Beep(static_cast<DWORD>(dFreq), static_cast<DWORD>(dDurat * 1000)); // Beep wants Milliseconds
  } else {
    hb_idleSleep(dDurat);
  }
}

// dDuration is in 'Ticks' (18.2 per second)
void hb_gt_winapi_tone(double dFrequency, double dDuration)
{
#if 0
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_winapi_tone(%lf, %lf)", dFrequency, dDuration));
#endif

  // According to the Clipper NG, the duration in 'ticks' is truncated to the
  // integer portion  ... Depending on the platform, Harbour allows a finer
  // resolution, but the minimum is 1 tick (for compatibility)

  // Convert from ticks to seconds
  dDuration = (HB_MIN(HB_MAX(1.0, dDuration), ULONG_MAX)) / 18.2;

  // keep the frequency in an acceptable range
  dFrequency = HB_MIN(HB_MAX(0.0, dFrequency), 32767.0);

#if defined(HB_HAS_WIN9X_TONE)
  if (hb_iswin9x()) {
    // If Windows 95 or 98, use w9xTone for chosen C compilers
    hb_gt_w9xTone(dFrequency, dDuration);
  } else
#endif
    // If Windows NT or NT2k, use wNtTone, which redirects Tone() to
    // Windows API Beep() function
    hb_gt_wNtTone(dFrequency, dDuration);
}

#endif // HB_OS_WIN
