/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2023 Marcos Antonio Gambeta

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/*
  NOTE: source code generated with the help of a code generator
*/

#include <windows.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

/*
WINMMAPI LRESULT WINAPI CloseDriver(HDRVR hDriver,LPARAM lParam1,LPARAM lParam2)
*/
HB_FUNC( WACLOSEDRIVER )
{
  wa_ret_LRESULT(CloseDriver(wa_par_HDRVR(1), wa_par_LPARAM(2), wa_par_LPARAM(3)));
}

/*
WINMMAPI HDRVR WINAPI OpenDriver(LPCWSTR szDriverName,LPCWSTR szSectionName,LPARAM lParam2)
*/
HB_FUNC( WAOPENDRIVER )
{
  void * str1;
  void * str2;
  wa_ret_HDRVR(OpenDriver(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), wa_par_LPARAM(3)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINMMAPI LRESULT WINAPI SendDriverMessage(HDRVR hDriver,UINT message,LPARAM lParam1,LPARAM lParam2)
*/
HB_FUNC( WASENDDRIVERMESSAGE )
{
  wa_ret_LRESULT(SendDriverMessage(wa_par_HDRVR(1), wa_par_UINT(2), wa_par_LPARAM(3), wa_par_LPARAM(4)));
}

/*
WINMMAPI HMODULE WINAPI DrvGetModuleHandle(HDRVR hDriver)
*/
HB_FUNC( WADRVGETMODULEHANDLE )
{
  wa_ret_HMODULE(DrvGetModuleHandle(wa_par_HDRVR(1)));
}

/*
WINMMAPI HMODULE WINAPI GetDriverModuleHandle(HDRVR hDriver)
*/
HB_FUNC( WAGETDRIVERMODULEHANDLE )
{
  wa_ret_HMODULE(GetDriverModuleHandle(wa_par_HDRVR(1)));
}

/*
WINMMAPI LRESULT WINAPI DefDriverProc(DWORD_PTR dwDriverIdentifier,HDRVR hdrvr,UINT uMsg,LPARAM lParam1,LPARAM lParam2)
*/

/*
WINMMAPI WINBOOL WINAPI sndPlaySoundA(LPCSTR pszSound,UINT fuSound)
*/
#if 0
HB_FUNC( WASNDPLAYSOUNDA )
{
  wa_ret_BOOL(sndPlaySoundA(wa_par_LPCSTR(1), wa_par_UINT(2)));
}
#endif

/*
WINMMAPI WINBOOL WINAPI sndPlaySoundW(LPCWSTR pszSound,UINT fuSound)
*/
#if 0
HB_FUNC( WASNDPLAYSOUNDW )
{
  wa_ret_BOOL(sndPlaySoundW(wa_par_LPCWSTR(1), wa_par_UINT(2)));
}
#endif

HB_FUNC( WASNDPLAYSOUND )
{
  void * str1;
  wa_ret_BOOL(sndPlaySound(HB_PARSTR(1, &str1, nullptr), wa_par_UINT(2)));
  hb_strfree(str1);
}

/*
WINMMAPI WINBOOL WINAPI PlaySoundA(LPCSTR pszSound,HMODULE hmod,DWORD fdwSound)
*/
#if 0
HB_FUNC( WAPLAYSOUNDA )
{
  wa_ret_BOOL(PlaySoundA(wa_par_LPCSTR(1), wa_par_HMODULE(2), wa_par_DWORD(3)));
}
#endif

/*
WINMMAPI WINBOOL WINAPI PlaySoundW(LPCWSTR pszSound,HMODULE hmod,DWORD fdwSound)
*/
#if 0
HB_FUNC( WAPLAYSOUNDW )
{
  wa_ret_BOOL(PlaySoundW(wa_par_LPCWSTR(1), wa_par_HMODULE(2), wa_par_DWORD(3)));
}
#endif

HB_FUNC( WAPLAYSOUND )
{
  void * str1;
  wa_ret_BOOL(PlaySound(HB_PARSTR(1, &str1, nullptr), wa_par_HMODULE(2), wa_par_DWORD(3)));
  hb_strfree(str1);
}

/*
WINMMAPI UINT WINAPI waveOutGetNumDevs(void)
*/
HB_FUNC( WAWAVEOUTGETNUMDEVS )
{
  wa_ret_UINT(waveOutGetNumDevs());
}

/*
WINMMAPI MMRESULT WINAPI waveOutGetDevCapsA(UINT_PTR uDeviceID,LPWAVEOUTCAPSA pwoc,UINT cbwoc)
*/

/*
WINMMAPI MMRESULT WINAPI waveOutGetDevCapsW(UINT_PTR uDeviceID,LPWAVEOUTCAPSW pwoc,UINT cbwoc)
*/

/*
WINMMAPI MMRESULT WINAPI waveOutGetVolume(HWAVEOUT hwo,LPDWORD pdwVolume)
*/
HB_FUNC( WAWAVEOUTGETVOLUME )
{
  DWORD dwVolume;
  wa_ret_MMRESULT(waveOutGetVolume(wa_par_HWAVEOUT(1), &dwVolume));
  wa_stor_DWORD(dwVolume, 2);
}

/*
WINMMAPI MMRESULT WINAPI waveOutSetVolume(HWAVEOUT hwo,DWORD dwVolume)
*/
HB_FUNC( WAWAVEOUTSETVOLUME )
{
  wa_ret_MMRESULT(waveOutSetVolume(wa_par_HWAVEOUT(1), wa_par_DWORD(2)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutGetErrorTextA(MMRESULT mmrError,LPSTR pszText,UINT cchText)
*/
HB_FUNC( WAWAVEOUTGETERRORTEXTA )
{
  wa_ret_MMRESULT(waveOutGetErrorTextA(wa_par_MMRESULT(1), const_cast<LPSTR>(hb_parc(2)), wa_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutGetErrorTextW(MMRESULT mmrError,LPWSTR pszText,UINT cchText)
*/
HB_FUNC( WAWAVEOUTGETERRORTEXTW )
{
  wa_ret_MMRESULT(waveOutGetErrorTextW(wa_par_MMRESULT(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutOpen(LPHWAVEOUT phwo,UINT uDeviceID,LPCWAVEFORMATEX pwfx,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI waveOutClose(HWAVEOUT hwo)
*/
HB_FUNC( WAWAVEOUTCLOSE )
{
  wa_ret_MMRESULT(waveOutClose(wa_par_HWAVEOUT(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutPrepareHeader(HWAVEOUT hwo,LPWAVEHDR pwh,UINT cbwh)
*/

/*
WINMMAPI MMRESULT WINAPI waveOutUnprepareHeader(HWAVEOUT hwo,LPWAVEHDR pwh,UINT cbwh)
*/

/*
WINMMAPI MMRESULT WINAPI waveOutWrite(HWAVEOUT hwo,LPWAVEHDR pwh,UINT cbwh)
*/

/*
WINMMAPI MMRESULT WINAPI waveOutPause(HWAVEOUT hwo)
*/
HB_FUNC( WAWAVEOUTPAUSE )
{
  wa_ret_MMRESULT(waveOutPause(wa_par_HWAVEOUT(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutRestart(HWAVEOUT hwo)
*/
HB_FUNC( WAWAVEOUTRESTART )
{
  wa_ret_MMRESULT(waveOutRestart(wa_par_HWAVEOUT(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutReset(HWAVEOUT hwo)
*/
HB_FUNC( WAWAVEOUTRESET )
{
  wa_ret_MMRESULT(waveOutReset(wa_par_HWAVEOUT(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutBreakLoop(HWAVEOUT hwo)
*/
HB_FUNC( WAWAVEOUTBREAKLOOP )
{
  wa_ret_MMRESULT(waveOutBreakLoop(wa_par_HWAVEOUT(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutGetPosition(HWAVEOUT hwo,LPMMTIME pmmt,UINT cbmmt)
*/

/*
WINMMAPI MMRESULT WINAPI waveOutGetPitch(HWAVEOUT hwo,LPDWORD pdwPitch)
*/
HB_FUNC( WAWAVEOUTGETPITCH )
{
  DWORD dwPitch;
  wa_ret_MMRESULT(waveOutGetPitch(wa_par_HWAVEOUT(1), &dwPitch));
  wa_stor_DWORD(dwPitch, 2);
}

/*
WINMMAPI MMRESULT WINAPI waveOutSetPitch(HWAVEOUT hwo,DWORD dwPitch)
*/
HB_FUNC( WAWAVEOUTSETPITCH )
{
  wa_ret_MMRESULT(waveOutSetPitch(wa_par_HWAVEOUT(1), wa_par_DWORD(2)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutGetPlaybackRate(HWAVEOUT hwo,LPDWORD pdwRate)
*/
HB_FUNC( WAWAVEOUTGETPLAYBACKRATE )
{
  DWORD dwRate;
  wa_ret_MMRESULT(waveOutGetPlaybackRate(wa_par_HWAVEOUT(1), &dwRate));
  wa_stor_DWORD(dwRate, 2);
}

/*
WINMMAPI MMRESULT WINAPI waveOutSetPlaybackRate(HWAVEOUT hwo,DWORD dwRate)
*/
HB_FUNC( WAWAVEOUTSETPLAYBACKRATE )
{
  wa_ret_MMRESULT(waveOutSetPlaybackRate(wa_par_HWAVEOUT(1), wa_par_DWORD(2)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutGetID(HWAVEOUT hwo,LPUINT puDeviceID)
*/

/*
WINMMAPI MMRESULT WINAPI waveOutMessage(HWAVEOUT hwo,UINT uMsg,DWORD_PTR dw1,DWORD_PTR dw2)
*/

/*
WINMMAPI UINT WINAPI waveInGetNumDevs(void)
*/
HB_FUNC( WAWAVEINGETNUMDEVS )
{
  wa_ret_UINT(waveInGetNumDevs());
}

/*
WINMMAPI MMRESULT WINAPI waveInGetDevCapsA(UINT_PTR uDeviceID,LPWAVEINCAPSA pwic,UINT cbwic)
*/

/*
WINMMAPI MMRESULT WINAPI waveInGetDevCapsW(UINT_PTR uDeviceID,LPWAVEINCAPSW pwic,UINT cbwic)
*/

/*
WINMMAPI MMRESULT WINAPI waveInGetErrorTextA(MMRESULT mmrError,LPSTR pszText,UINT cchText)
*/
HB_FUNC( WAWAVEINGETERRORTEXTA )
{
  wa_ret_MMRESULT(waveInGetErrorTextA(wa_par_MMRESULT(1), const_cast<LPSTR>(hb_parc(2)), wa_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI waveInGetErrorTextW(MMRESULT mmrError,LPWSTR pszText,UINT cchText)
*/
HB_FUNC( WAWAVEINGETERRORTEXTW )
{
  wa_ret_MMRESULT(waveInGetErrorTextW(wa_par_MMRESULT(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI waveInOpen(LPHWAVEIN phwi,UINT uDeviceID,LPCWAVEFORMATEX pwfx,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI waveInClose(HWAVEIN hwi)
*/
HB_FUNC( WAWAVEINCLOSE )
{
  wa_ret_MMRESULT(waveInClose(wa_par_HWAVEIN(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveInPrepareHeader(HWAVEIN hwi,LPWAVEHDR pwh,UINT cbwh)
*/

/*
WINMMAPI MMRESULT WINAPI waveInUnprepareHeader(HWAVEIN hwi,LPWAVEHDR pwh,UINT cbwh)
*/

/*
WINMMAPI MMRESULT WINAPI waveInAddBuffer(HWAVEIN hwi,LPWAVEHDR pwh,UINT cbwh)
*/

/*
WINMMAPI MMRESULT WINAPI waveInStart(HWAVEIN hwi)
*/
HB_FUNC( WAWAVEINSTART )
{
  wa_ret_MMRESULT(waveInStart(wa_par_HWAVEIN(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveInStop(HWAVEIN hwi)
*/
HB_FUNC( WAWAVEINSTOP )
{
  wa_ret_MMRESULT(waveInStop(wa_par_HWAVEIN(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveInReset(HWAVEIN hwi)
*/
HB_FUNC( WAWAVEINRESET )
{
  wa_ret_MMRESULT(waveInReset(wa_par_HWAVEIN(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveInGetPosition(HWAVEIN hwi,LPMMTIME pmmt,UINT cbmmt)
*/

/*
WINMMAPI MMRESULT WINAPI waveInGetID(HWAVEIN hwi,LPUINT puDeviceID)
*/
HB_FUNC( WAWAVEINGETID )
{
  UINT uDeviceID;
  wa_ret_MMRESULT(waveInGetID(wa_par_HWAVEIN(1), &uDeviceID));
  wa_stor_UINT(uDeviceID, 2);
}

/*
WINMMAPI MMRESULT WINAPI waveInMessage(HWAVEIN hwi,UINT uMsg,DWORD_PTR dw1,DWORD_PTR dw2)
*/

/*
WINMMAPI UINT WINAPI midiOutGetNumDevs(void)
*/
HB_FUNC( WAMIDIOUTGETNUMDEVS )
{
  wa_ret_UINT(midiOutGetNumDevs());
}

/*
WINMMAPI MMRESULT WINAPI midiStreamOpen(LPHMIDISTRM phms,LPUINT puDeviceID,DWORD cMidi,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI midiStreamClose(HMIDISTRM hms)
*/
HB_FUNC( WAMIDISTREAMCLOSE )
{
  wa_ret_MMRESULT(midiStreamClose(wa_par_HMIDISTRM(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiStreamProperty(HMIDISTRM hms,LPBYTE lppropdata,DWORD dwProperty)
*/

/*
WINMMAPI MMRESULT WINAPI midiStreamPosition(HMIDISTRM hms,LPMMTIME lpmmt,UINT cbmmt)
*/

/*
WINMMAPI MMRESULT WINAPI midiStreamOut(HMIDISTRM hms,LPMIDIHDR pmh,UINT cbmh)
*/

/*
WINMMAPI MMRESULT WINAPI midiStreamPause(HMIDISTRM hms)
*/
HB_FUNC( WAMIDISTREAMPAUSE )
{
  wa_ret_MMRESULT(midiStreamPause(wa_par_HMIDISTRM(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiStreamRestart(HMIDISTRM hms)
*/
HB_FUNC( WAMIDISTREAMRESTART )
{
  wa_ret_MMRESULT(midiStreamRestart(wa_par_HMIDISTRM(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiStreamStop(HMIDISTRM hms)
*/
HB_FUNC( WAMIDISTREAMSTOP )
{
  wa_ret_MMRESULT(midiStreamStop(wa_par_HMIDISTRM(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiConnect(HMIDI hmi,HMIDIOUT hmo,LPVOID pReserved)
*/
HB_FUNC( WAMIDICONNECT )
{
  wa_ret_MMRESULT(midiConnect(wa_par_HMIDI(1), wa_par_HMIDIOUT(2), static_cast<LPVOID>(hb_parptr(3))));
}

/*
WINMMAPI MMRESULT WINAPI midiDisconnect(HMIDI hmi,HMIDIOUT hmo,LPVOID pReserved)
*/
HB_FUNC( WAMIDIDISCONNECT )
{
  wa_ret_MMRESULT(midiDisconnect(wa_par_HMIDI(1), wa_par_HMIDIOUT(2), static_cast<LPVOID>(hb_parptr(3))));
}

/*
WINMMAPI MMRESULT WINAPI midiOutGetDevCapsA(UINT_PTR uDeviceID,LPMIDIOUTCAPSA pmoc,UINT cbmoc)
*/

/*
WINMMAPI MMRESULT WINAPI midiOutGetDevCapsW(UINT_PTR uDeviceID,LPMIDIOUTCAPSW pmoc,UINT cbmoc)
*/

/*
WINMMAPI MMRESULT WINAPI midiOutGetVolume(HMIDIOUT hmo,LPDWORD pdwVolume)
*/
HB_FUNC( WAMIDIOUTGETVOLUME )
{
  DWORD dwVolume;
  wa_ret_MMRESULT(midiOutGetVolume(wa_par_HMIDIOUT(1), &dwVolume));
  wa_stor_DWORD(dwVolume, 2);
}

/*
WINMMAPI MMRESULT WINAPI midiOutSetVolume(HMIDIOUT hmo,DWORD dwVolume)
*/
HB_FUNC( WAMIDIOUTSETVOLUME )
{
  wa_ret_MMRESULT(midiOutSetVolume(wa_par_HMIDIOUT(1), wa_par_DWORD(2)));
}

/*
WINMMAPI MMRESULT WINAPI midiOutGetErrorTextA(MMRESULT mmrError,LPSTR pszText,UINT cchText)
*/
HB_FUNC( WAMIDIOUTGETERRORTEXTA )
{
  wa_ret_MMRESULT(midiOutGetErrorTextA(wa_par_MMRESULT(1), const_cast<LPSTR>(hb_parc(2)), wa_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI midiOutGetErrorTextW(MMRESULT mmrError,LPWSTR pszText,UINT cchText)
*/
HB_FUNC( WAMIDIOUTGETERRORTEXTW )
{
  wa_ret_MMRESULT(midiOutGetErrorTextW(wa_par_MMRESULT(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI midiOutOpen(LPHMIDIOUT phmo,UINT uDeviceID,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI midiOutClose(HMIDIOUT hmo)
*/
HB_FUNC( WAMIDIOUTCLOSE )
{
  wa_ret_MMRESULT(midiOutClose(wa_par_HMIDIOUT(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiOutPrepareHeader(HMIDIOUT hmo,LPMIDIHDR pmh,UINT cbmh)
*/

/*
WINMMAPI MMRESULT WINAPI midiOutUnprepareHeader(HMIDIOUT hmo,LPMIDIHDR pmh,UINT cbmh)
*/

/*
WINMMAPI MMRESULT WINAPI midiOutShortMsg(HMIDIOUT hmo,DWORD dwMsg)
*/
HB_FUNC( WAMIDIOUTSHORTMSG )
{
  wa_ret_MMRESULT(midiOutShortMsg(wa_par_HMIDIOUT(1), wa_par_DWORD(2)));
}

/*
WINMMAPI MMRESULT WINAPI midiOutLongMsg(HMIDIOUT hmo,LPMIDIHDR pmh,UINT cbmh)
*/

/*
WINMMAPI MMRESULT WINAPI midiOutReset(HMIDIOUT hmo)
*/
HB_FUNC( WAMIDIOUTRESET )
{
  wa_ret_MMRESULT(midiOutReset(wa_par_HMIDIOUT(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiOutCachePatches(HMIDIOUT hmo,UINT uBank,LPWORD pwpa,UINT fuCache)
*/

/*
WINMMAPI MMRESULT WINAPI midiOutCacheDrumPatches(HMIDIOUT hmo,UINT uPatch,LPWORD pwkya,UINT fuCache)
*/

/*
WINMMAPI MMRESULT WINAPI midiOutGetID(HMIDIOUT hmo,LPUINT puDeviceID)
*/

/*
WINMMAPI MMRESULT WINAPI midiOutMessage(HMIDIOUT hmo,UINT uMsg,DWORD_PTR dw1,DWORD_PTR dw2)
*/

/*
WINMMAPI UINT WINAPI midiInGetNumDevs(void)
*/
HB_FUNC( WAMIDIINGETNUMDEVS )
{
  wa_ret_UINT(midiInGetNumDevs());
}

/*
WINMMAPI MMRESULT WINAPI midiInGetDevCapsA(UINT_PTR uDeviceID,LPMIDIINCAPSA pmic,UINT cbmic)
*/

/*
WINMMAPI MMRESULT WINAPI midiInGetDevCapsW(UINT_PTR uDeviceID,LPMIDIINCAPSW pmic,UINT cbmic)
*/

/*
WINMMAPI MMRESULT WINAPI midiInGetErrorTextA(MMRESULT mmrError,LPSTR pszText,UINT cchText)
*/
HB_FUNC( WAMIDIINGETERRORTEXTA )
{
  wa_ret_MMRESULT(midiInGetErrorTextA(wa_par_MMRESULT(1), const_cast<LPSTR>(hb_parc(2)), wa_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI midiInGetErrorTextW(MMRESULT mmrError,LPWSTR pszText,UINT cchText)
*/
HB_FUNC( WAMIDIINGETERRORTEXTW )
{
  wa_ret_MMRESULT(midiInGetErrorTextW(wa_par_MMRESULT(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI midiInOpen(LPHMIDIIN phmi,UINT uDeviceID,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI midiInClose(HMIDIIN hmi)
*/
HB_FUNC( WAMIDIINCLOSE )
{
  wa_ret_MMRESULT(midiInClose(wa_par_HMIDIIN(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiInPrepareHeader(HMIDIIN hmi,LPMIDIHDR pmh,UINT cbmh)
*/

/*
WINMMAPI MMRESULT WINAPI midiInUnprepareHeader(HMIDIIN hmi,LPMIDIHDR pmh,UINT cbmh)
*/

/*
WINMMAPI MMRESULT WINAPI midiInAddBuffer(HMIDIIN hmi,LPMIDIHDR pmh,UINT cbmh)
*/

/*
WINMMAPI MMRESULT WINAPI midiInStart(HMIDIIN hmi)
*/
HB_FUNC( WAMIDIINSTART )
{
  wa_ret_MMRESULT(midiInStart(wa_par_HMIDIIN(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiInStop(HMIDIIN hmi)
*/
HB_FUNC( WAMIDIINSTOP )
{
  wa_ret_MMRESULT(midiInStop(wa_par_HMIDIIN(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiInReset(HMIDIIN hmi)
*/
HB_FUNC( WAMIDIINRESET )
{
  wa_ret_MMRESULT(midiInReset(wa_par_HMIDIIN(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiInGetID(HMIDIIN hmi,LPUINT puDeviceID)
*/
HB_FUNC( WAMIDIINGETID )
{
  UINT uDeviceID;
  wa_ret_MMRESULT(midiInGetID(wa_par_HMIDIIN(1), &uDeviceID));
  wa_stor_UINT(uDeviceID, 2);
}

/*
WINMMAPI MMRESULT WINAPI midiInMessage(HMIDIIN hmi,UINT uMsg,DWORD_PTR dw1,DWORD_PTR dw2)
*/
HB_FUNC( WAMIDIINMESSAGE )
{
  wa_ret_MMRESULT(midiInMessage(wa_par_HMIDIIN(1), wa_par_UINT(2), wa_par_DWORD_PTR(3), wa_par_DWORD_PTR(4)));
}

/*
WINMMAPI UINT WINAPI auxGetNumDevs(void)
*/
HB_FUNC( WAAUXGETNUMDEVS )
{
  wa_ret_UINT(auxGetNumDevs());
}

/*
WINMMAPI MMRESULT WINAPI auxGetDevCapsA(UINT_PTR uDeviceID,LPAUXCAPSA pac,UINT cbac)
*/

/*
WINMMAPI MMRESULT WINAPI auxGetDevCapsW(UINT_PTR uDeviceID,LPAUXCAPSW pac,UINT cbac)
*/

/*
WINMMAPI MMRESULT WINAPI auxSetVolume(UINT uDeviceID,DWORD dwVolume)
*/
HB_FUNC( WAAUXSETVOLUME )
{
  wa_ret_MMRESULT(auxSetVolume(wa_par_UINT(1), wa_par_DWORD(2)));
}

/*
WINMMAPI MMRESULT WINAPI auxGetVolume(UINT uDeviceID,LPDWORD pdwVolume)
*/
HB_FUNC( WAAUXGETVOLUME )
{
  DWORD dwVolume;
  wa_ret_MMRESULT(auxGetVolume(wa_par_UINT(1), &dwVolume));
  wa_stor_DWORD(dwVolume, 2);
}

/*
WINMMAPI MMRESULT WINAPI auxOutMessage(UINT uDeviceID,UINT uMsg,DWORD_PTR dw1,DWORD_PTR dw2)
*/
HB_FUNC( WAAUXOUTMESSAGE )
{
  wa_ret_MMRESULT(auxOutMessage(wa_par_UINT(1), wa_par_UINT(2), wa_par_DWORD_PTR(3), wa_par_DWORD_PTR(4)));
}

/*
WINMMAPI UINT WINAPI mixerGetNumDevs(void)
*/
HB_FUNC( WAMIXERGETNUMDEVS )
{
  wa_ret_UINT(mixerGetNumDevs());
}

/*
WINMMAPI MMRESULT WINAPI mixerGetDevCapsA(UINT_PTR uMxId,LPMIXERCAPSA pmxcaps,UINT cbmxcaps)
*/

/*
WINMMAPI MMRESULT WINAPI mixerGetDevCapsW(UINT_PTR uMxId,LPMIXERCAPSW pmxcaps,UINT cbmxcaps)
*/

/*
WINMMAPI MMRESULT WINAPI mixerOpen(LPHMIXER phmx,UINT uMxId,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI mixerClose(HMIXER hmx)
*/
HB_FUNC( WAMIXERCLOSE )
{
  wa_ret_MMRESULT(mixerClose(wa_par_HMIXER(1)));
}

/*
WINMMAPI DWORD WINAPI mixerMessage(HMIXER hmx,UINT uMsg,DWORD_PTR dwParam1,DWORD_PTR dwParam2)
*/
HB_FUNC( WAMIXERMESSAGE )
{
  wa_ret_DWORD(mixerMessage(wa_par_HMIXER(1), wa_par_UINT(2), wa_par_DWORD_PTR(3), wa_par_DWORD_PTR(4)));
}

/*
WINMMAPI MMRESULT WINAPI mixerGetLineInfoA(HMIXEROBJ hmxobj,LPMIXERLINEA pmxl,DWORD fdwInfo)
*/

/*
WINMMAPI MMRESULT WINAPI mixerGetLineInfoW(HMIXEROBJ hmxobj,LPMIXERLINEW pmxl,DWORD fdwInfo)
*/

/*
WINMMAPI MMRESULT WINAPI mixerGetID(HMIXEROBJ hmxobj,UINT *puMxId,DWORD fdwId)
*/

/*
WINMMAPI MMRESULT WINAPI mixerGetLineControlsA(HMIXEROBJ hmxobj,LPMIXERLINECONTROLSA pmxlc,DWORD fdwControls)
*/

/*
WINMMAPI MMRESULT WINAPI mixerGetLineControlsW(HMIXEROBJ hmxobj,LPMIXERLINECONTROLSW pmxlc,DWORD fdwControls)
*/

/*
WINMMAPI MMRESULT WINAPI mixerGetControlDetailsA(HMIXEROBJ hmxobj,LPMIXERCONTROLDETAILS pmxcd,DWORD fdwDetails)
*/

/*
WINMMAPI MMRESULT WINAPI mixerGetControlDetailsW(HMIXEROBJ hmxobj,LPMIXERCONTROLDETAILS pmxcd,DWORD fdwDetails)
*/

/*
WINMMAPI MMRESULT WINAPI mixerSetControlDetails(HMIXEROBJ hmxobj,LPMIXERCONTROLDETAILS pmxcd,DWORD fdwDetails)
*/

/*
WINMMAPI MMRESULT WINAPI timeGetSystemTime(LPMMTIME pmmt,UINT cbmmt)
*/

/*
WINMMAPI DWORD WINAPI timeGetTime(void)
*/
HB_FUNC( WATIMEGETTIME )
{
  wa_ret_DWORD(timeGetTime());
}

/*
WINMMAPI MMRESULT WINAPI timeSetEvent(UINT uDelay,UINT uResolution,LPTIMECALLBACK fptc,DWORD_PTR dwUser,UINT fuEvent)
*/

/*
WINMMAPI MMRESULT WINAPI timeKillEvent(UINT uTimerID)
*/
HB_FUNC( WATIMEKILLEVENT )
{
  wa_ret_MMRESULT(timeKillEvent(wa_par_UINT(1)));
}

/*
WINMMAPI MMRESULT WINAPI timeGetDevCaps(LPTIMECAPS ptc,UINT cbtc)
*/
HB_FUNC( WATIMEGETDEVCAPS )
{
  wa_ret_MMRESULT(timeGetDevCaps(static_cast<LPTIMECAPS>(wa_get_ptr(1)), wa_par_UINT(2)));
}

/*
WINMMAPI MMRESULT WINAPI timeBeginPeriod(UINT uPeriod)
*/
HB_FUNC( WATIMEBEGINPERIOD )
{
  wa_ret_MMRESULT(timeBeginPeriod(wa_par_UINT(1)));
}

/*
WINMMAPI MMRESULT WINAPI timeEndPeriod(UINT uPeriod)
*/
HB_FUNC( WATIMEENDPERIOD )
{
  wa_ret_MMRESULT(timeEndPeriod(wa_par_UINT(1)));
}

/*
WINMMAPI UINT WINAPI joyGetNumDevs(void)
*/
HB_FUNC( WAJOYGETNUMDEVS )
{
  wa_ret_UINT(joyGetNumDevs());
}

/*
WINMMAPI MMRESULT WINAPI joyGetDevCapsA(UINT_PTR uJoyID,LPJOYCAPSA pjc,UINT cbjc)
*/

/*
WINMMAPI MMRESULT WINAPI joyGetDevCapsW(UINT_PTR uJoyID,LPJOYCAPSW pjc,UINT cbjc)
*/

/*
WINMMAPI MMRESULT WINAPI joyGetPos(UINT uJoyID,LPJOYINFO pji)
*/

/*
WINMMAPI MMRESULT WINAPI joyGetPosEx(UINT uJoyID,LPJOYINFOEX pji)
*/

/*
WINMMAPI MMRESULT WINAPI joyGetThreshold(UINT uJoyID,LPUINT puThreshold)
*/

/*
WINMMAPI MMRESULT WINAPI joyReleaseCapture(UINT uJoyID)
*/
HB_FUNC( WAJOYRELEASECAPTURE )
{
  wa_ret_MMRESULT(joyReleaseCapture(wa_par_UINT(1)));
}

/*
WINMMAPI MMRESULT WINAPI joySetCapture(HWND hwnd,UINT uJoyID,UINT uPeriod,WINBOOL fChanged)
*/
HB_FUNC( WAJOYSETCAPTURE )
{
  wa_ret_MMRESULT(joySetCapture(wa_par_HWND(1), wa_par_UINT(2), wa_par_UINT(3), hb_parl(4)));
}

/*
WINMMAPI MMRESULT WINAPI joySetThreshold(UINT uJoyID,UINT uThreshold)
*/
HB_FUNC( WAJOYSETTHRESHOLD )
{
  wa_ret_MMRESULT(joySetThreshold(wa_par_UINT(1), wa_par_UINT(2)));
}

/*
WINMMAPI FOURCC WINAPI mmioStringToFOURCCA(LPCSTR sz,UINT uFlags)
*/

/*
WINMMAPI FOURCC WINAPI mmioStringToFOURCCW(LPCWSTR sz,UINT uFlags)
*/

/*
WINMMAPI LPMMIOPROC WINAPI mmioInstallIOProcA(FOURCC fccIOProc,LPMMIOPROC pIOProc,DWORD dwFlags)
*/

/*
WINMMAPI LPMMIOPROC WINAPI mmioInstallIOProcW(FOURCC fccIOProc,LPMMIOPROC pIOProc,DWORD dwFlags)
*/

/*
WINMMAPI HMMIO WINAPI mmioOpenA(LPSTR pszFileName,LPMMIOINFO pmmioinfo,DWORD fdwOpen)
*/

/*
WINMMAPI HMMIO WINAPI mmioOpenW(LPWSTR pszFileName,LPMMIOINFO pmmioinfo,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI mmioRenameA(LPCSTR pszFileName,LPCSTR pszNewFileName,LPCMMIOINFO pmmioinfo,DWORD fdwRename)
*/

/*
WINMMAPI MMRESULT WINAPI mmioRenameW(LPCWSTR pszFileName,LPCWSTR pszNewFileName,LPCMMIOINFO pmmioinfo,DWORD fdwRename)
*/

/*
WINMMAPI MMRESULT WINAPI mmioClose(HMMIO hmmio,UINT fuClose)
*/
HB_FUNC( WAMMIOCLOSE )
{
  wa_ret_MMRESULT(mmioClose(wa_par_HMMIO(1), wa_par_UINT(2)));
}

/*
WINMMAPI LONG WINAPI mmioRead(HMMIO hmmio,HPSTR pch,LONG cch)
*/

/*
WINMMAPI LONG WINAPI mmioWrite(HMMIO hmmio,const char _huge *pch,LONG cch)
*/

/*
WINMMAPI LONG WINAPI mmioSeek(HMMIO hmmio,LONG lOffset,int iOrigin)
*/
HB_FUNC( WAMMIOSEEK )
{
  wa_ret_LONG(mmioSeek(wa_par_HMMIO(1), hb_parnl(2), wa_par_int(3)));
}

/*
WINMMAPI MMRESULT WINAPI mmioGetInfo(HMMIO hmmio,LPMMIOINFO pmmioinfo,UINT fuInfo)
*/

/*
WINMMAPI MMRESULT WINAPI mmioSetInfo(HMMIO hmmio,LPCMMIOINFO pmmioinfo,UINT fuInfo)
*/

/*
WINMMAPI MMRESULT WINAPI mmioSetBuffer(HMMIO hmmio,LPSTR pchBuffer,LONG cchBuffer,UINT fuBuffer)
*/
HB_FUNC( WAMMIOSETBUFFER )
{
  wa_ret_MMRESULT(mmioSetBuffer(wa_par_HMMIO(1), const_cast<LPSTR>(hb_parc(2)), hb_parnl(3), wa_par_UINT(4)));
}

/*
WINMMAPI MMRESULT WINAPI mmioFlush(HMMIO hmmio,UINT fuFlush)
*/
HB_FUNC( WAMMIOFLUSH )
{
  wa_ret_MMRESULT(mmioFlush(wa_par_HMMIO(1), wa_par_UINT(2)));
}

/*
WINMMAPI MMRESULT WINAPI mmioAdvance(HMMIO hmmio,LPMMIOINFO pmmioinfo,UINT fuAdvance)
*/

/*
WINMMAPI LRESULT WINAPI mmioSendMessage(HMMIO hmmio,UINT uMsg,LPARAM lParam1,LPARAM lParam2)
*/
HB_FUNC( WAMMIOSENDMESSAGE )
{
  wa_ret_LRESULT(mmioSendMessage(wa_par_HMMIO(1), wa_par_UINT(2), wa_par_LPARAM(3), wa_par_LPARAM(4)));
}

/*
WINMMAPI MMRESULT WINAPI mmioDescend(HMMIO hmmio,LPMMCKINFO pmmcki,const MMCKINFO *pmmckiParent,UINT fuDescend)
*/

/*
WINMMAPI MMRESULT WINAPI mmioAscend(HMMIO hmmio,LPMMCKINFO pmmcki,UINT fuAscend)
*/

/*
WINMMAPI MMRESULT WINAPI mmioCreateChunk(HMMIO hmmio,LPMMCKINFO pmmcki,UINT fuCreate)
*/

/*
WINMMAPI MCIERROR WINAPI mciSendCommandA(MCIDEVICEID mciId,UINT uMsg,DWORD_PTR dwParam1,DWORD_PTR dwParam2)
*/

/*
WINMMAPI MCIERROR WINAPI mciSendCommandW(MCIDEVICEID mciId,UINT uMsg,DWORD_PTR dwParam1,DWORD_PTR dwParam2)
*/

/*
WINMMAPI MCIERROR WINAPI mciSendStringA(LPCSTR lpstrCommand,LPSTR lpstrReturnString,UINT uReturnLength,HWND hwndCallback)
*/
HB_FUNC( WAMCISENDSTRINGA )
{
  wa_ret_MCIERROR(mciSendStringA(wa_par_LPCSTR(1), const_cast<LPSTR>(hb_parc(2)), wa_par_UINT(3), wa_par_HWND(4)));
}

/*
WINMMAPI MCIERROR WINAPI mciSendStringW(LPCWSTR lpstrCommand,LPWSTR lpstrReturnString,UINT uReturnLength,HWND hwndCallback)
*/
HB_FUNC( WAMCISENDSTRINGW )
{
  wa_ret_MCIERROR(mciSendStringW(wa_par_LPCWSTR(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_UINT(3), wa_par_HWND(4)));
}

/*
WINMMAPI MCIDEVICEID WINAPI mciGetDeviceIDA(LPCSTR pszDevice)
*/
HB_FUNC( WAMCIGETDEVICEIDA )
{
  wa_ret_MCIDEVICEID(mciGetDeviceIDA(wa_par_LPCSTR(1)));
}

/*
WINMMAPI MCIDEVICEID WINAPI mciGetDeviceIDW(LPCWSTR pszDevice)
*/
HB_FUNC( WAMCIGETDEVICEIDW )
{
  wa_ret_MCIDEVICEID(mciGetDeviceIDW(wa_par_LPCWSTR(1)));
}

HB_FUNC( WAMCIGETDEVICEID )
{
  void * str1;
  wa_ret_MCIDEVICEID(mciGetDeviceID(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINMMAPI MCIDEVICEID WINAPI mciGetDeviceIDFromElementIDA(DWORD dwElementID,LPCSTR lpstrType)
*/
HB_FUNC( WAMCIGETDEVICEIDFROMELEMENTIDA )
{
  wa_ret_MCIDEVICEID(mciGetDeviceIDFromElementIDA(wa_par_DWORD(1), wa_par_LPCSTR(2)));
}

/*
WINMMAPI MCIDEVICEID WINAPI mciGetDeviceIDFromElementIDW(DWORD dwElementID,LPCWSTR lpstrType)
*/
HB_FUNC( WAMCIGETDEVICEIDFROMELEMENTIDW )
{
  wa_ret_MCIDEVICEID(mciGetDeviceIDFromElementIDW(wa_par_DWORD(1), wa_par_LPCWSTR(2)));
}

HB_FUNC( WAMCIGETDEVICEIDFROMELEMENTID )
{
  void * str2;
  wa_ret_MCIDEVICEID(mciGetDeviceIDFromElementID(wa_par_DWORD(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

/*
WINMMAPI WINBOOL WINAPI mciGetErrorStringA(MCIERROR mcierr,LPSTR pszText,UINT cchText)
*/
HB_FUNC( WAMCIGETERRORSTRINGA )
{
  wa_ret_BOOL(mciGetErrorStringA(wa_par_MCIERROR(1), const_cast<LPSTR>(hb_parc(2)), wa_par_UINT(3)));
}

/*
WINMMAPI WINBOOL WINAPI mciGetErrorStringW(MCIERROR mcierr,LPWSTR pszText,UINT cchText)
*/
HB_FUNC( WAMCIGETERRORSTRINGW )
{
  wa_ret_BOOL(mciGetErrorStringW(wa_par_MCIERROR(1), reinterpret_cast<LPWSTR>(const_cast<char*>(hb_parc(2))), wa_par_UINT(3)));
}

/*
WINMMAPI WINBOOL WINAPI mciSetYieldProc(MCIDEVICEID mciId,YIELDPROC fpYieldProc,DWORD dwYieldData)
*/

/*
WINMMAPI HTASK WINAPI mciGetCreatorTask(MCIDEVICEID mciId)
*/
HB_FUNC( WAMCIGETCREATORTASK )
{
  wa_ret_HTASK(mciGetCreatorTask(wa_par_MCIDEVICEID(1)));
}

/*
WINMMAPI YIELDPROC WINAPI mciGetYieldProc(MCIDEVICEID mciId,LPDWORD pdwYieldData)
*/
