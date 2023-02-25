/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (C) 2022 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2022 Marcos Antonio Gambeta

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
#include "hbapiitm.h"
#include "hbapicls.hpp"
#include "hbwinuni.h"
#include "winapi.h"

/*
WINMMAPI LRESULT WINAPI CloseDriver(HDRVR hDriver,LPARAM lParam1,LPARAM lParam2)
*/
HB_FUNC( WINAPI_CLOSEDRIVER )
{
  winapi_ret_LRESULT(CloseDriver(winapi_par_HDRVR(1), winapi_par_LPARAM(2), winapi_par_LPARAM(3)));
}

/*
WINMMAPI HDRVR WINAPI OpenDriver(LPCWSTR szDriverName,LPCWSTR szSectionName,LPARAM lParam2)
*/
HB_FUNC( WINAPI_OPENDRIVER )
{
  void * str1;
  void * str2;
  winapi_ret_HDRVR(OpenDriver(HB_PARSTR(1, &str1, nullptr), HB_PARSTR(2, &str2, nullptr), winapi_par_LPARAM(3)));
  hb_strfree(str1);
  hb_strfree(str2);
}

/*
WINMMAPI LRESULT WINAPI SendDriverMessage(HDRVR hDriver,UINT message,LPARAM lParam1,LPARAM lParam2)
*/
HB_FUNC( WINAPI_SENDDRIVERMESSAGE )
{
  winapi_ret_LRESULT(SendDriverMessage(winapi_par_HDRVR(1), winapi_par_UINT(2), winapi_par_LPARAM(3), winapi_par_LPARAM(4)));
}

/*
WINMMAPI HMODULE WINAPI DrvGetModuleHandle(HDRVR hDriver)
*/
HB_FUNC( WINAPI_DRVGETMODULEHANDLE )
{
  winapi_ret_HMODULE(DrvGetModuleHandle(winapi_par_HDRVR(1)));
}

/*
WINMMAPI HMODULE WINAPI GetDriverModuleHandle(HDRVR hDriver)
*/
HB_FUNC( WINAPI_GETDRIVERMODULEHANDLE )
{
  winapi_ret_HMODULE(GetDriverModuleHandle(winapi_par_HDRVR(1)));
}

/*
WINMMAPI LRESULT WINAPI DefDriverProc(DWORD_PTR dwDriverIdentifier,HDRVR hdrvr,UINT uMsg,LPARAM lParam1,LPARAM lParam2)
*/

/*
WINMMAPI WINBOOL WINAPI sndPlaySoundA(LPCSTR pszSound,UINT fuSound)
*/
#if 0
HB_FUNC( WINAPI_SNDPLAYSOUNDA )
{
  winapi_ret_BOOL(sndPlaySoundA(( LPCSTR ) hb_parc(1), winapi_par_UINT(2)));
}
#endif

/*
WINMMAPI WINBOOL WINAPI sndPlaySoundW(LPCWSTR pszSound,UINT fuSound)
*/
#if 0
HB_FUNC( WINAPI_SNDPLAYSOUNDW )
{
  winapi_ret_BOOL(sndPlaySoundW(( LPCWSTR ) hb_parc(1), winapi_par_UINT(2)));
}
#endif

HB_FUNC( WINAPI_SNDPLAYSOUND )
{
  void * str1;
  winapi_ret_BOOL(sndPlaySound(HB_PARSTR(1, &str1, nullptr), winapi_par_UINT(2)));
  hb_strfree(str1);
}

/*
WINMMAPI WINBOOL WINAPI PlaySoundA(LPCSTR pszSound,HMODULE hmod,DWORD fdwSound)
*/
#if 0
HB_FUNC( WINAPI_PLAYSOUNDA )
{
  winapi_ret_BOOL(PlaySoundA(( LPCSTR ) hb_parc(1), winapi_par_HMODULE(2), winapi_par_DWORD(3)));
}
#endif

/*
WINMMAPI WINBOOL WINAPI PlaySoundW(LPCWSTR pszSound,HMODULE hmod,DWORD fdwSound)
*/
#if 0
HB_FUNC( WINAPI_PLAYSOUNDW )
{
  winapi_ret_BOOL(PlaySoundW(( LPCWSTR ) hb_parc(1), winapi_par_HMODULE(2), winapi_par_DWORD(3)));
}
#endif

HB_FUNC( WINAPI_PLAYSOUND )
{
  void * str1;
  winapi_ret_BOOL(PlaySound(HB_PARSTR(1, &str1, nullptr), winapi_par_HMODULE(2), winapi_par_DWORD(3)));
  hb_strfree(str1);
}

/*
WINMMAPI UINT WINAPI waveOutGetNumDevs(void)
*/
HB_FUNC( WINAPI_WAVEOUTGETNUMDEVS )
{
  winapi_ret_UINT(waveOutGetNumDevs());
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
HB_FUNC( WINAPI_WAVEOUTGETVOLUME )
{
  DWORD dwVolume;
  winapi_ret_MMRESULT(waveOutGetVolume(winapi_par_HWAVEOUT(1), &dwVolume));
  winapi_stor_DWORD(dwVolume, 2);
}

/*
WINMMAPI MMRESULT WINAPI waveOutSetVolume(HWAVEOUT hwo,DWORD dwVolume)
*/
HB_FUNC( WINAPI_WAVEOUTSETVOLUME )
{
  winapi_ret_MMRESULT(waveOutSetVolume(winapi_par_HWAVEOUT(1), winapi_par_DWORD(2)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutGetErrorTextA(MMRESULT mmrError,LPSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_WAVEOUTGETERRORTEXTA )
{
  winapi_ret_MMRESULT(waveOutGetErrorTextA(winapi_par_MMRESULT(1), ( LPSTR ) hb_parc(2), winapi_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutGetErrorTextW(MMRESULT mmrError,LPWSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_WAVEOUTGETERRORTEXTW )
{
  winapi_ret_MMRESULT(waveOutGetErrorTextW(winapi_par_MMRESULT(1), ( LPWSTR ) hb_parc(2), winapi_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutOpen(LPHWAVEOUT phwo,UINT uDeviceID,LPCWAVEFORMATEX pwfx,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI waveOutClose(HWAVEOUT hwo)
*/
HB_FUNC( WINAPI_WAVEOUTCLOSE )
{
  winapi_ret_MMRESULT(waveOutClose(winapi_par_HWAVEOUT(1)));
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
HB_FUNC( WINAPI_WAVEOUTPAUSE )
{
  winapi_ret_MMRESULT(waveOutPause(winapi_par_HWAVEOUT(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutRestart(HWAVEOUT hwo)
*/
HB_FUNC( WINAPI_WAVEOUTRESTART )
{
  winapi_ret_MMRESULT(waveOutRestart(winapi_par_HWAVEOUT(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutReset(HWAVEOUT hwo)
*/
HB_FUNC( WINAPI_WAVEOUTRESET )
{
  winapi_ret_MMRESULT(waveOutReset(winapi_par_HWAVEOUT(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutBreakLoop(HWAVEOUT hwo)
*/
HB_FUNC( WINAPI_WAVEOUTBREAKLOOP )
{
  winapi_ret_MMRESULT(waveOutBreakLoop(winapi_par_HWAVEOUT(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutGetPosition(HWAVEOUT hwo,LPMMTIME pmmt,UINT cbmmt)
*/

/*
WINMMAPI MMRESULT WINAPI waveOutGetPitch(HWAVEOUT hwo,LPDWORD pdwPitch)
*/
HB_FUNC( WINAPI_WAVEOUTGETPITCH )
{
  DWORD dwPitch;
  winapi_ret_MMRESULT(waveOutGetPitch(winapi_par_HWAVEOUT(1), &dwPitch));
  winapi_stor_DWORD(dwPitch, 2);
}

/*
WINMMAPI MMRESULT WINAPI waveOutSetPitch(HWAVEOUT hwo,DWORD dwPitch)
*/
HB_FUNC( WINAPI_WAVEOUTSETPITCH )
{
  winapi_ret_MMRESULT(waveOutSetPitch(winapi_par_HWAVEOUT(1), winapi_par_DWORD(2)));
}

/*
WINMMAPI MMRESULT WINAPI waveOutGetPlaybackRate(HWAVEOUT hwo,LPDWORD pdwRate)
*/
HB_FUNC( WINAPI_WAVEOUTGETPLAYBACKRATE )
{
  DWORD dwRate;
  winapi_ret_MMRESULT(waveOutGetPlaybackRate(winapi_par_HWAVEOUT(1), &dwRate));
  winapi_stor_DWORD(dwRate, 2);
}

/*
WINMMAPI MMRESULT WINAPI waveOutSetPlaybackRate(HWAVEOUT hwo,DWORD dwRate)
*/
HB_FUNC( WINAPI_WAVEOUTSETPLAYBACKRATE )
{
  winapi_ret_MMRESULT(waveOutSetPlaybackRate(winapi_par_HWAVEOUT(1), winapi_par_DWORD(2)));
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
HB_FUNC( WINAPI_WAVEINGETNUMDEVS )
{
  winapi_ret_UINT(waveInGetNumDevs());
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
HB_FUNC( WINAPI_WAVEINGETERRORTEXTA )
{
  winapi_ret_MMRESULT(waveInGetErrorTextA(winapi_par_MMRESULT(1), ( LPSTR ) hb_parc(2), winapi_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI waveInGetErrorTextW(MMRESULT mmrError,LPWSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_WAVEINGETERRORTEXTW )
{
  winapi_ret_MMRESULT(waveInGetErrorTextW(winapi_par_MMRESULT(1), ( LPWSTR ) hb_parc(2), winapi_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI waveInOpen(LPHWAVEIN phwi,UINT uDeviceID,LPCWAVEFORMATEX pwfx,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI waveInClose(HWAVEIN hwi)
*/
HB_FUNC( WINAPI_WAVEINCLOSE )
{
  winapi_ret_MMRESULT(waveInClose(winapi_par_HWAVEIN(1)));
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
HB_FUNC( WINAPI_WAVEINSTART )
{
  winapi_ret_MMRESULT(waveInStart(winapi_par_HWAVEIN(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveInStop(HWAVEIN hwi)
*/
HB_FUNC( WINAPI_WAVEINSTOP )
{
  winapi_ret_MMRESULT(waveInStop(winapi_par_HWAVEIN(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveInReset(HWAVEIN hwi)
*/
HB_FUNC( WINAPI_WAVEINRESET )
{
  winapi_ret_MMRESULT(waveInReset(winapi_par_HWAVEIN(1)));
}

/*
WINMMAPI MMRESULT WINAPI waveInGetPosition(HWAVEIN hwi,LPMMTIME pmmt,UINT cbmmt)
*/

/*
WINMMAPI MMRESULT WINAPI waveInGetID(HWAVEIN hwi,LPUINT puDeviceID)
*/
HB_FUNC( WINAPI_WAVEINGETID )
{
  UINT uDeviceID;
  winapi_ret_MMRESULT(waveInGetID(winapi_par_HWAVEIN(1), &uDeviceID));
  winapi_stor_UINT(uDeviceID, 2);
}

/*
WINMMAPI MMRESULT WINAPI waveInMessage(HWAVEIN hwi,UINT uMsg,DWORD_PTR dw1,DWORD_PTR dw2)
*/

/*
WINMMAPI UINT WINAPI midiOutGetNumDevs(void)
*/
HB_FUNC( WINAPI_MIDIOUTGETNUMDEVS )
{
  winapi_ret_UINT(midiOutGetNumDevs());
}

/*
WINMMAPI MMRESULT WINAPI midiStreamOpen(LPHMIDISTRM phms,LPUINT puDeviceID,DWORD cMidi,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI midiStreamClose(HMIDISTRM hms)
*/
HB_FUNC( WINAPI_MIDISTREAMCLOSE )
{
  winapi_ret_MMRESULT(midiStreamClose(winapi_par_HMIDISTRM(1)));
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
HB_FUNC( WINAPI_MIDISTREAMPAUSE )
{
  winapi_ret_MMRESULT(midiStreamPause(winapi_par_HMIDISTRM(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiStreamRestart(HMIDISTRM hms)
*/
HB_FUNC( WINAPI_MIDISTREAMRESTART )
{
  winapi_ret_MMRESULT(midiStreamRestart(winapi_par_HMIDISTRM(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiStreamStop(HMIDISTRM hms)
*/
HB_FUNC( WINAPI_MIDISTREAMSTOP )
{
  winapi_ret_MMRESULT(midiStreamStop(winapi_par_HMIDISTRM(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiConnect(HMIDI hmi,HMIDIOUT hmo,LPVOID pReserved)
*/
HB_FUNC( WINAPI_MIDICONNECT )
{
  winapi_ret_MMRESULT(midiConnect(winapi_par_HMIDI(1), winapi_par_HMIDIOUT(2), static_cast<LPVOID>(hb_parptr(3))));
}

/*
WINMMAPI MMRESULT WINAPI midiDisconnect(HMIDI hmi,HMIDIOUT hmo,LPVOID pReserved)
*/
HB_FUNC( WINAPI_MIDIDISCONNECT )
{
  winapi_ret_MMRESULT(midiDisconnect(winapi_par_HMIDI(1), winapi_par_HMIDIOUT(2), static_cast<LPVOID>(hb_parptr(3))));
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
HB_FUNC( WINAPI_MIDIOUTGETVOLUME )
{
  DWORD dwVolume;
  winapi_ret_MMRESULT(midiOutGetVolume(winapi_par_HMIDIOUT(1), &dwVolume));
  winapi_stor_DWORD(dwVolume, 2);
}

/*
WINMMAPI MMRESULT WINAPI midiOutSetVolume(HMIDIOUT hmo,DWORD dwVolume)
*/
HB_FUNC( WINAPI_MIDIOUTSETVOLUME )
{
  winapi_ret_MMRESULT(midiOutSetVolume(winapi_par_HMIDIOUT(1), winapi_par_DWORD(2)));
}

/*
WINMMAPI MMRESULT WINAPI midiOutGetErrorTextA(MMRESULT mmrError,LPSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_MIDIOUTGETERRORTEXTA )
{
  winapi_ret_MMRESULT(midiOutGetErrorTextA(winapi_par_MMRESULT(1), ( LPSTR ) hb_parc(2), winapi_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI midiOutGetErrorTextW(MMRESULT mmrError,LPWSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_MIDIOUTGETERRORTEXTW )
{
  winapi_ret_MMRESULT(midiOutGetErrorTextW(winapi_par_MMRESULT(1), ( LPWSTR ) hb_parc(2), winapi_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI midiOutOpen(LPHMIDIOUT phmo,UINT uDeviceID,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI midiOutClose(HMIDIOUT hmo)
*/
HB_FUNC( WINAPI_MIDIOUTCLOSE )
{
  winapi_ret_MMRESULT(midiOutClose(winapi_par_HMIDIOUT(1)));
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
HB_FUNC( WINAPI_MIDIOUTSHORTMSG )
{
  winapi_ret_MMRESULT(midiOutShortMsg(winapi_par_HMIDIOUT(1), winapi_par_DWORD(2)));
}

/*
WINMMAPI MMRESULT WINAPI midiOutLongMsg(HMIDIOUT hmo,LPMIDIHDR pmh,UINT cbmh)
*/

/*
WINMMAPI MMRESULT WINAPI midiOutReset(HMIDIOUT hmo)
*/
HB_FUNC( WINAPI_MIDIOUTRESET )
{
  winapi_ret_MMRESULT(midiOutReset(winapi_par_HMIDIOUT(1)));
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
HB_FUNC( WINAPI_MIDIINGETNUMDEVS )
{
  winapi_ret_UINT(midiInGetNumDevs());
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
HB_FUNC( WINAPI_MIDIINGETERRORTEXTA )
{
  winapi_ret_MMRESULT(midiInGetErrorTextA(winapi_par_MMRESULT(1), ( LPSTR ) hb_parc(2), winapi_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI midiInGetErrorTextW(MMRESULT mmrError,LPWSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_MIDIINGETERRORTEXTW )
{
  winapi_ret_MMRESULT(midiInGetErrorTextW(winapi_par_MMRESULT(1), ( LPWSTR ) hb_parc(2), winapi_par_UINT(3)));
}

/*
WINMMAPI MMRESULT WINAPI midiInOpen(LPHMIDIIN phmi,UINT uDeviceID,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI midiInClose(HMIDIIN hmi)
*/
HB_FUNC( WINAPI_MIDIINCLOSE )
{
  winapi_ret_MMRESULT(midiInClose(winapi_par_HMIDIIN(1)));
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
HB_FUNC( WINAPI_MIDIINSTART )
{
  winapi_ret_MMRESULT(midiInStart(winapi_par_HMIDIIN(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiInStop(HMIDIIN hmi)
*/
HB_FUNC( WINAPI_MIDIINSTOP )
{
  winapi_ret_MMRESULT(midiInStop(winapi_par_HMIDIIN(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiInReset(HMIDIIN hmi)
*/
HB_FUNC( WINAPI_MIDIINRESET )
{
  winapi_ret_MMRESULT(midiInReset(winapi_par_HMIDIIN(1)));
}

/*
WINMMAPI MMRESULT WINAPI midiInGetID(HMIDIIN hmi,LPUINT puDeviceID)
*/
HB_FUNC( WINAPI_MIDIINGETID )
{
  UINT uDeviceID;
  winapi_ret_MMRESULT(midiInGetID(winapi_par_HMIDIIN(1), &uDeviceID));
  winapi_stor_UINT(uDeviceID, 2);
}

/*
WINMMAPI MMRESULT WINAPI midiInMessage(HMIDIIN hmi,UINT uMsg,DWORD_PTR dw1,DWORD_PTR dw2)
*/
HB_FUNC( WINAPI_MIDIINMESSAGE )
{
  winapi_ret_MMRESULT(midiInMessage(winapi_par_HMIDIIN(1), winapi_par_UINT(2), winapi_par_DWORD_PTR(3), winapi_par_DWORD_PTR(4)));
}

/*
WINMMAPI UINT WINAPI auxGetNumDevs(void)
*/
HB_FUNC( WINAPI_AUXGETNUMDEVS )
{
  winapi_ret_UINT(auxGetNumDevs());
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
HB_FUNC( WINAPI_AUXSETVOLUME )
{
  winapi_ret_MMRESULT(auxSetVolume(winapi_par_UINT(1), winapi_par_DWORD(2)));
}

/*
WINMMAPI MMRESULT WINAPI auxGetVolume(UINT uDeviceID,LPDWORD pdwVolume)
*/
HB_FUNC( WINAPI_AUXGETVOLUME )
{
  DWORD dwVolume;
  winapi_ret_MMRESULT(auxGetVolume(winapi_par_UINT(1), &dwVolume));
  winapi_stor_DWORD(dwVolume, 2);
}

/*
WINMMAPI MMRESULT WINAPI auxOutMessage(UINT uDeviceID,UINT uMsg,DWORD_PTR dw1,DWORD_PTR dw2)
*/
HB_FUNC( WINAPI_AUXOUTMESSAGE )
{
  winapi_ret_MMRESULT(auxOutMessage(winapi_par_UINT(1), winapi_par_UINT(2), winapi_par_DWORD_PTR(3), winapi_par_DWORD_PTR(4)));
}

/*
WINMMAPI UINT WINAPI mixerGetNumDevs(void)
*/
HB_FUNC( WINAPI_MIXERGETNUMDEVS )
{
  winapi_ret_UINT(mixerGetNumDevs());
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
HB_FUNC( WINAPI_MIXERCLOSE )
{
  winapi_ret_MMRESULT(mixerClose(winapi_par_HMIXER(1)));
}

/*
WINMMAPI DWORD WINAPI mixerMessage(HMIXER hmx,UINT uMsg,DWORD_PTR dwParam1,DWORD_PTR dwParam2)
*/
HB_FUNC( WINAPI_MIXERMESSAGE )
{
  winapi_ret_DWORD(mixerMessage(winapi_par_HMIXER(1), winapi_par_UINT(2), winapi_par_DWORD_PTR(3), winapi_par_DWORD_PTR(4)));
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
HB_FUNC( WINAPI_TIMEGETTIME )
{
  winapi_ret_DWORD(timeGetTime());
}

/*
WINMMAPI MMRESULT WINAPI timeSetEvent(UINT uDelay,UINT uResolution,LPTIMECALLBACK fptc,DWORD_PTR dwUser,UINT fuEvent)
*/

/*
WINMMAPI MMRESULT WINAPI timeKillEvent(UINT uTimerID)
*/
HB_FUNC( WINAPI_TIMEKILLEVENT )
{
  winapi_ret_MMRESULT(timeKillEvent(winapi_par_UINT(1)));
}

/*
WINMMAPI MMRESULT WINAPI timeGetDevCaps(LPTIMECAPS ptc,UINT cbtc)
*/
HB_FUNC( WINAPI_TIMEGETDEVCAPS )
{
  winapi_ret_MMRESULT(timeGetDevCaps(static_cast<LPTIMECAPS>(winapi_get_ptr(1)), winapi_par_UINT(2)));
}

/*
WINMMAPI MMRESULT WINAPI timeBeginPeriod(UINT uPeriod)
*/
HB_FUNC( WINAPI_TIMEBEGINPERIOD )
{
  winapi_ret_MMRESULT(timeBeginPeriod(winapi_par_UINT(1)));
}

/*
WINMMAPI MMRESULT WINAPI timeEndPeriod(UINT uPeriod)
*/
HB_FUNC( WINAPI_TIMEENDPERIOD )
{
  winapi_ret_MMRESULT(timeEndPeriod(winapi_par_UINT(1)));
}

/*
WINMMAPI UINT WINAPI joyGetNumDevs(void)
*/
HB_FUNC( WINAPI_JOYGETNUMDEVS )
{
  winapi_ret_UINT(joyGetNumDevs());
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
HB_FUNC( WINAPI_JOYRELEASECAPTURE )
{
  winapi_ret_MMRESULT(joyReleaseCapture(winapi_par_UINT(1)));
}

/*
WINMMAPI MMRESULT WINAPI joySetCapture(HWND hwnd,UINT uJoyID,UINT uPeriod,WINBOOL fChanged)
*/
HB_FUNC( WINAPI_JOYSETCAPTURE )
{
  winapi_ret_MMRESULT(joySetCapture(winapi_par_HWND(1), winapi_par_UINT(2), winapi_par_UINT(3), hb_parl(4)));
}

/*
WINMMAPI MMRESULT WINAPI joySetThreshold(UINT uJoyID,UINT uThreshold)
*/
HB_FUNC( WINAPI_JOYSETTHRESHOLD )
{
  winapi_ret_MMRESULT(joySetThreshold(winapi_par_UINT(1), winapi_par_UINT(2)));
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
HB_FUNC( WINAPI_MMIOCLOSE )
{
  winapi_ret_MMRESULT(mmioClose(winapi_par_HMMIO(1), winapi_par_UINT(2)));
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
HB_FUNC( WINAPI_MMIOSEEK )
{
  winapi_ret_LONG(mmioSeek(winapi_par_HMMIO(1), hb_parnl(2), winapi_par_int(3)));
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
HB_FUNC( WINAPI_MMIOSETBUFFER )
{
  winapi_ret_MMRESULT(mmioSetBuffer(winapi_par_HMMIO(1), ( LPSTR ) hb_parc(2), hb_parnl(3), winapi_par_UINT(4)));
}

/*
WINMMAPI MMRESULT WINAPI mmioFlush(HMMIO hmmio,UINT fuFlush)
*/
HB_FUNC( WINAPI_MMIOFLUSH )
{
  winapi_ret_MMRESULT(mmioFlush(winapi_par_HMMIO(1), winapi_par_UINT(2)));
}

/*
WINMMAPI MMRESULT WINAPI mmioAdvance(HMMIO hmmio,LPMMIOINFO pmmioinfo,UINT fuAdvance)
*/

/*
WINMMAPI LRESULT WINAPI mmioSendMessage(HMMIO hmmio,UINT uMsg,LPARAM lParam1,LPARAM lParam2)
*/
HB_FUNC( WINAPI_MMIOSENDMESSAGE )
{
  winapi_ret_LRESULT(mmioSendMessage(winapi_par_HMMIO(1), winapi_par_UINT(2), winapi_par_LPARAM(3), winapi_par_LPARAM(4)));
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
HB_FUNC( WINAPI_MCISENDSTRINGA )
{
  winapi_ret_MCIERROR(mciSendStringA(( LPCSTR ) hb_parc(1), ( LPSTR ) hb_parc(2), winapi_par_UINT(3), winapi_par_HWND(4)));
}

/*
WINMMAPI MCIERROR WINAPI mciSendStringW(LPCWSTR lpstrCommand,LPWSTR lpstrReturnString,UINT uReturnLength,HWND hwndCallback)
*/
HB_FUNC( WINAPI_MCISENDSTRINGW )
{
  winapi_ret_MCIERROR(mciSendStringW(( LPCWSTR ) hb_parc(1), ( LPWSTR ) hb_parc(2), winapi_par_UINT(3), winapi_par_HWND(4)));
}

/*
WINMMAPI MCIDEVICEID WINAPI mciGetDeviceIDA(LPCSTR pszDevice)
*/
HB_FUNC( WINAPI_MCIGETDEVICEIDA )
{
  winapi_ret_MCIDEVICEID(mciGetDeviceIDA(( LPCSTR ) hb_parc(1)));
}

/*
WINMMAPI MCIDEVICEID WINAPI mciGetDeviceIDW(LPCWSTR pszDevice)
*/
HB_FUNC( WINAPI_MCIGETDEVICEIDW )
{
  winapi_ret_MCIDEVICEID(mciGetDeviceIDW(( LPCWSTR ) hb_parc(1)));
}

HB_FUNC( WINAPI_MCIGETDEVICEID )
{
  void * str1;
  winapi_ret_MCIDEVICEID(mciGetDeviceID(HB_PARSTR(1, &str1, nullptr)));
  hb_strfree(str1);
}

/*
WINMMAPI MCIDEVICEID WINAPI mciGetDeviceIDFromElementIDA(DWORD dwElementID,LPCSTR lpstrType)
*/
HB_FUNC( WINAPI_MCIGETDEVICEIDFROMELEMENTIDA )
{
  winapi_ret_MCIDEVICEID(mciGetDeviceIDFromElementIDA(winapi_par_DWORD(1), ( LPCSTR ) hb_parc(2)));
}

/*
WINMMAPI MCIDEVICEID WINAPI mciGetDeviceIDFromElementIDW(DWORD dwElementID,LPCWSTR lpstrType)
*/
HB_FUNC( WINAPI_MCIGETDEVICEIDFROMELEMENTIDW )
{
  winapi_ret_MCIDEVICEID(mciGetDeviceIDFromElementIDW(winapi_par_DWORD(1), ( LPCWSTR ) hb_parc(2)));
}

HB_FUNC( WINAPI_MCIGETDEVICEIDFROMELEMENTID )
{
  void * str2;
  winapi_ret_MCIDEVICEID(mciGetDeviceIDFromElementID(winapi_par_DWORD(1), HB_PARSTR(2, &str2, nullptr)));
  hb_strfree(str2);
}

/*
WINMMAPI WINBOOL WINAPI mciGetErrorStringA(MCIERROR mcierr,LPSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_MCIGETERRORSTRINGA )
{
  winapi_ret_BOOL(mciGetErrorStringA(winapi_par_MCIERROR(1), ( LPSTR ) hb_parc(2), winapi_par_UINT(3)));
}

/*
WINMMAPI WINBOOL WINAPI mciGetErrorStringW(MCIERROR mcierr,LPWSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_MCIGETERRORSTRINGW )
{
  winapi_ret_BOOL(mciGetErrorStringW(winapi_par_MCIERROR(1), ( LPWSTR ) hb_parc(2), winapi_par_UINT(3)));
}

/*
WINMMAPI WINBOOL WINAPI mciSetYieldProc(MCIDEVICEID mciId,YIELDPROC fpYieldProc,DWORD dwYieldData)
*/

/*
WINMMAPI HTASK WINAPI mciGetCreatorTask(MCIDEVICEID mciId)
*/
HB_FUNC( WINAPI_MCIGETCREATORTASK )
{
  winapi_ret_HTASK(mciGetCreatorTask(winapi_par_MCIDEVICEID(1)));
}

/*
WINMMAPI YIELDPROC WINAPI mciGetYieldProc(MCIDEVICEID mciId,LPDWORD pdwYieldData)
*/
