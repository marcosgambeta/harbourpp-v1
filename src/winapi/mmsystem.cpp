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
#include "hbapi.h"

/*
WINMMAPI LRESULT WINAPI CloseDriver(HDRVR hDriver,LPARAM lParam1,LPARAM lParam2)
*/
HB_FUNC( WINAPI_CLOSEDRIVER )
{
  hb_retnl( ( LRESULT ) CloseDriver( ( HDRVR ) hb_parptr( 1 ), ( LPARAM ) hb_parnl( 2 ), ( LPARAM ) hb_parnl( 3 ) ) );
}

/*
WINMMAPI HDRVR WINAPI OpenDriver(LPCWSTR szDriverName,LPCWSTR szSectionName,LPARAM lParam2)
*/
HB_FUNC( WINAPI_OPENDRIVER )
{
  hb_retptr( ( HDRVR ) OpenDriver( ( LPCWSTR ) hb_parc( 1 ), ( LPCWSTR ) hb_parc( 2 ), ( LPARAM ) hb_parnl( 3 ) ) );
}

/*
WINMMAPI LRESULT WINAPI SendDriverMessage(HDRVR hDriver,UINT message,LPARAM lParam1,LPARAM lParam2)
*/
HB_FUNC( WINAPI_SENDDRIVERMESSAGE )
{
  hb_retnl( ( LRESULT ) SendDriverMessage( ( HDRVR ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( LPARAM ) hb_parnl( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
}

/*
WINMMAPI HMODULE WINAPI DrvGetModuleHandle(HDRVR hDriver)
*/
HB_FUNC( WINAPI_DRVGETMODULEHANDLE )
{
  hb_retptr( ( HMODULE ) DrvGetModuleHandle( ( HDRVR ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI HMODULE WINAPI GetDriverModuleHandle(HDRVR hDriver)
*/
HB_FUNC( WINAPI_GETDRIVERMODULEHANDLE )
{
  hb_retptr( ( HMODULE ) GetDriverModuleHandle( ( HDRVR ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI LRESULT WINAPI DefDriverProc(DWORD_PTR dwDriverIdentifier,HDRVR hdrvr,UINT uMsg,LPARAM lParam1,LPARAM lParam2)
*/

/*
WINMMAPI WINBOOL WINAPI sndPlaySoundA(LPCSTR pszSound,UINT fuSound)
*/
HB_FUNC( WINAPI_SNDPLAYSOUNDA )
{
  hb_retl( ( WINBOOL ) sndPlaySoundA( ( LPCSTR ) hb_parc( 1 ), ( UINT ) hb_parni( 2 ) ) );
}

/*
WINMMAPI WINBOOL WINAPI sndPlaySoundW(LPCWSTR pszSound,UINT fuSound)
*/
HB_FUNC( WINAPI_SNDPLAYSOUNDW )
{
  hb_retl( ( WINBOOL ) sndPlaySoundW( ( LPCWSTR ) hb_parc( 1 ), ( UINT ) hb_parni( 2 ) ) );
}

/*
WINMMAPI WINBOOL WINAPI PlaySoundA(LPCSTR pszSound,HMODULE hmod,DWORD fdwSound)
*/
HB_FUNC( WINAPI_PLAYSOUNDA )
{
  hb_retl( ( WINBOOL ) PlaySoundA( ( LPCSTR ) hb_parc( 1 ), ( HMODULE ) hb_parptr( 2 ), ( DWORD ) hb_parnl( 3 ) ) );
}

/*
WINMMAPI WINBOOL WINAPI PlaySoundW(LPCWSTR pszSound,HMODULE hmod,DWORD fdwSound)
*/
HB_FUNC( WINAPI_PLAYSOUNDW )
{
  hb_retl( ( WINBOOL ) PlaySoundW( ( LPCWSTR ) hb_parc( 1 ), ( HMODULE ) hb_parptr( 2 ), ( DWORD ) hb_parnl( 3 ) ) );
}

/*
WINMMAPI UINT WINAPI waveOutGetNumDevs(void)
*/
HB_FUNC( WINAPI_WAVEOUTGETNUMDEVS )
{
  hb_retni( ( UINT ) waveOutGetNumDevs() );
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
  hb_retni( ( MMRESULT ) waveOutGetVolume( ( HWAVEOUT ) hb_parptr( 1 ), ( LPDWORD ) hb_parptr( 2 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveOutSetVolume(HWAVEOUT hwo,DWORD dwVolume)
*/
HB_FUNC( WINAPI_WAVEOUTSETVOLUME )
{
  hb_retni( ( MMRESULT ) waveOutSetVolume( ( HWAVEOUT ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveOutGetErrorTextA(MMRESULT mmrError,LPSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_WAVEOUTGETERRORTEXTA )
{
  hb_retni( ( MMRESULT ) waveOutGetErrorTextA( ( MMRESULT ) hb_parni( 1 ), ( LPSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveOutGetErrorTextW(MMRESULT mmrError,LPWSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_WAVEOUTGETERRORTEXTW )
{
  hb_retni( ( MMRESULT ) waveOutGetErrorTextW( ( MMRESULT ) hb_parni( 1 ), ( LPWSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveOutOpen(LPHWAVEOUT phwo,UINT uDeviceID,LPCWAVEFORMATEX pwfx,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI waveOutClose(HWAVEOUT hwo)
*/
HB_FUNC( WINAPI_WAVEOUTCLOSE )
{
  hb_retni( ( MMRESULT ) waveOutClose( ( HWAVEOUT ) hb_parptr( 1 ) ) );
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
  hb_retni( ( MMRESULT ) waveOutPause( ( HWAVEOUT ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveOutRestart(HWAVEOUT hwo)
*/
HB_FUNC( WINAPI_WAVEOUTRESTART )
{
  hb_retni( ( MMRESULT ) waveOutRestart( ( HWAVEOUT ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveOutReset(HWAVEOUT hwo)
*/
HB_FUNC( WINAPI_WAVEOUTRESET )
{
  hb_retni( ( MMRESULT ) waveOutReset( ( HWAVEOUT ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveOutBreakLoop(HWAVEOUT hwo)
*/
HB_FUNC( WINAPI_WAVEOUTBREAKLOOP )
{
  hb_retni( ( MMRESULT ) waveOutBreakLoop( ( HWAVEOUT ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveOutGetPosition(HWAVEOUT hwo,LPMMTIME pmmt,UINT cbmmt)
*/

/*
WINMMAPI MMRESULT WINAPI waveOutGetPitch(HWAVEOUT hwo,LPDWORD pdwPitch)
*/
HB_FUNC( WINAPI_WAVEOUTGETPITCH )
{
  hb_retni( ( MMRESULT ) waveOutGetPitch( ( HWAVEOUT ) hb_parptr( 1 ), ( LPDWORD ) hb_parptr( 2 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveOutSetPitch(HWAVEOUT hwo,DWORD dwPitch)
*/
HB_FUNC( WINAPI_WAVEOUTSETPITCH )
{
  hb_retni( ( MMRESULT ) waveOutSetPitch( ( HWAVEOUT ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveOutGetPlaybackRate(HWAVEOUT hwo,LPDWORD pdwRate)
*/
HB_FUNC( WINAPI_WAVEOUTGETPLAYBACKRATE )
{
  hb_retni( ( MMRESULT ) waveOutGetPlaybackRate( ( HWAVEOUT ) hb_parptr( 1 ), ( LPDWORD ) hb_parptr( 2 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveOutSetPlaybackRate(HWAVEOUT hwo,DWORD dwRate)
*/
HB_FUNC( WINAPI_WAVEOUTSETPLAYBACKRATE )
{
  hb_retni( ( MMRESULT ) waveOutSetPlaybackRate( ( HWAVEOUT ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
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
  hb_retni( ( UINT ) waveInGetNumDevs() );
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
  hb_retni( ( MMRESULT ) waveInGetErrorTextA( ( MMRESULT ) hb_parni( 1 ), ( LPSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveInGetErrorTextW(MMRESULT mmrError,LPWSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_WAVEINGETERRORTEXTW )
{
  hb_retni( ( MMRESULT ) waveInGetErrorTextW( ( MMRESULT ) hb_parni( 1 ), ( LPWSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveInOpen(LPHWAVEIN phwi,UINT uDeviceID,LPCWAVEFORMATEX pwfx,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI waveInClose(HWAVEIN hwi)
*/
HB_FUNC( WINAPI_WAVEINCLOSE )
{
  hb_retni( ( MMRESULT ) waveInClose( ( HWAVEIN ) hb_parptr( 1 ) ) );
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
  hb_retni( ( MMRESULT ) waveInStart( ( HWAVEIN ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveInStop(HWAVEIN hwi)
*/
HB_FUNC( WINAPI_WAVEINSTOP )
{
  hb_retni( ( MMRESULT ) waveInStop( ( HWAVEIN ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveInReset(HWAVEIN hwi)
*/
HB_FUNC( WINAPI_WAVEINRESET )
{
  hb_retni( ( MMRESULT ) waveInReset( ( HWAVEIN ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI waveInGetPosition(HWAVEIN hwi,LPMMTIME pmmt,UINT cbmmt)
*/

/*
WINMMAPI MMRESULT WINAPI waveInGetID(HWAVEIN hwi,LPUINT puDeviceID)
*/

/*
WINMMAPI MMRESULT WINAPI waveInMessage(HWAVEIN hwi,UINT uMsg,DWORD_PTR dw1,DWORD_PTR dw2)
*/

/*
WINMMAPI UINT WINAPI midiOutGetNumDevs(void)
*/
HB_FUNC( WINAPI_MIDIOUTGETNUMDEVS )
{
  hb_retni( ( UINT ) midiOutGetNumDevs() );
}

/*
WINMMAPI MMRESULT WINAPI midiStreamOpen(LPHMIDISTRM phms,LPUINT puDeviceID,DWORD cMidi,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI midiStreamClose(HMIDISTRM hms)
*/
HB_FUNC( WINAPI_MIDISTREAMCLOSE )
{
  hb_retni( ( MMRESULT ) midiStreamClose( ( HMIDISTRM ) hb_parptr( 1 ) ) );
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
  hb_retni( ( MMRESULT ) midiStreamPause( ( HMIDISTRM ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI midiStreamRestart(HMIDISTRM hms)
*/
HB_FUNC( WINAPI_MIDISTREAMRESTART )
{
  hb_retni( ( MMRESULT ) midiStreamRestart( ( HMIDISTRM ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI midiStreamStop(HMIDISTRM hms)
*/
HB_FUNC( WINAPI_MIDISTREAMSTOP )
{
  hb_retni( ( MMRESULT ) midiStreamStop( ( HMIDISTRM ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI midiConnect(HMIDI hmi,HMIDIOUT hmo,LPVOID pReserved)
*/
HB_FUNC( WINAPI_MIDICONNECT )
{
  hb_retni( ( MMRESULT ) midiConnect( ( HMIDI ) hb_parptr( 1 ), ( HMIDIOUT ) hb_parptr( 2 ), ( LPVOID ) hb_parptr( 3 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI midiDisconnect(HMIDI hmi,HMIDIOUT hmo,LPVOID pReserved)
*/
HB_FUNC( WINAPI_MIDIDISCONNECT )
{
  hb_retni( ( MMRESULT ) midiDisconnect( ( HMIDI ) hb_parptr( 1 ), ( HMIDIOUT ) hb_parptr( 2 ), ( LPVOID ) hb_parptr( 3 ) ) );
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
  hb_retni( ( MMRESULT ) midiOutGetVolume( ( HMIDIOUT ) hb_parptr( 1 ), ( LPDWORD ) hb_parptr( 2 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI midiOutSetVolume(HMIDIOUT hmo,DWORD dwVolume)
*/
HB_FUNC( WINAPI_MIDIOUTSETVOLUME )
{
  hb_retni( ( MMRESULT ) midiOutSetVolume( ( HMIDIOUT ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI midiOutGetErrorTextA(MMRESULT mmrError,LPSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_MIDIOUTGETERRORTEXTA )
{
  hb_retni( ( MMRESULT ) midiOutGetErrorTextA( ( MMRESULT ) hb_parni( 1 ), ( LPSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI midiOutGetErrorTextW(MMRESULT mmrError,LPWSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_MIDIOUTGETERRORTEXTW )
{
  hb_retni( ( MMRESULT ) midiOutGetErrorTextW( ( MMRESULT ) hb_parni( 1 ), ( LPWSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI midiOutOpen(LPHMIDIOUT phmo,UINT uDeviceID,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI midiOutClose(HMIDIOUT hmo)
*/
HB_FUNC( WINAPI_MIDIOUTCLOSE )
{
  hb_retni( ( MMRESULT ) midiOutClose( ( HMIDIOUT ) hb_parptr( 1 ) ) );
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
  hb_retni( ( MMRESULT ) midiOutShortMsg( ( HMIDIOUT ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI midiOutLongMsg(HMIDIOUT hmo,LPMIDIHDR pmh,UINT cbmh)
*/

/*
WINMMAPI MMRESULT WINAPI midiOutReset(HMIDIOUT hmo)
*/
HB_FUNC( WINAPI_MIDIOUTRESET )
{
  hb_retni( ( MMRESULT ) midiOutReset( ( HMIDIOUT ) hb_parptr( 1 ) ) );
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
  hb_retni( ( UINT ) midiInGetNumDevs() );
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
  hb_retni( ( MMRESULT ) midiInGetErrorTextA( ( MMRESULT ) hb_parni( 1 ), ( LPSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI midiInGetErrorTextW(MMRESULT mmrError,LPWSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_MIDIINGETERRORTEXTW )
{
  hb_retni( ( MMRESULT ) midiInGetErrorTextW( ( MMRESULT ) hb_parni( 1 ), ( LPWSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI midiInOpen(LPHMIDIIN phmi,UINT uDeviceID,DWORD_PTR dwCallback,DWORD_PTR dwInstance,DWORD fdwOpen)
*/

/*
WINMMAPI MMRESULT WINAPI midiInClose(HMIDIIN hmi)
*/
HB_FUNC( WINAPI_MIDIINCLOSE )
{
  hb_retni( ( MMRESULT ) midiInClose( ( HMIDIIN ) hb_parptr( 1 ) ) );
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
  hb_retni( ( MMRESULT ) midiInStart( ( HMIDIIN ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI midiInStop(HMIDIIN hmi)
*/
HB_FUNC( WINAPI_MIDIINSTOP )
{
  hb_retni( ( MMRESULT ) midiInStop( ( HMIDIIN ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI midiInReset(HMIDIIN hmi)
*/
HB_FUNC( WINAPI_MIDIINRESET )
{
  hb_retni( ( MMRESULT ) midiInReset( ( HMIDIIN ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI midiInGetID(HMIDIIN hmi,LPUINT puDeviceID)
*/

/*
WINMMAPI MMRESULT WINAPI midiInMessage(HMIDIIN hmi,UINT uMsg,DWORD_PTR dw1,DWORD_PTR dw2)
*/

/*
WINMMAPI UINT WINAPI auxGetNumDevs(void)
*/
HB_FUNC( WINAPI_AUXGETNUMDEVS )
{
  hb_retni( ( UINT ) auxGetNumDevs() );
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
  hb_retni( ( MMRESULT ) auxSetVolume( ( UINT ) hb_parni( 1 ), ( DWORD ) hb_parnl( 2 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI auxGetVolume(UINT uDeviceID,LPDWORD pdwVolume)
*/
HB_FUNC( WINAPI_AUXGETVOLUME )
{
  hb_retni( ( MMRESULT ) auxGetVolume( ( UINT ) hb_parni( 1 ), ( LPDWORD ) hb_parptr( 2 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI auxOutMessage(UINT uDeviceID,UINT uMsg,DWORD_PTR dw1,DWORD_PTR dw2)
*/

/*
WINMMAPI UINT WINAPI mixerGetNumDevs(void)
*/
HB_FUNC( WINAPI_MIXERGETNUMDEVS )
{
  hb_retni( ( UINT ) mixerGetNumDevs() );
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
  hb_retni( ( MMRESULT ) mixerClose( ( HMIXER ) hb_parptr( 1 ) ) );
}

/*
WINMMAPI DWORD WINAPI mixerMessage(HMIXER hmx,UINT uMsg,DWORD_PTR dwParam1,DWORD_PTR dwParam2)
*/

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
  hb_retnl( ( DWORD ) timeGetTime() );
}

/*
WINMMAPI MMRESULT WINAPI timeSetEvent(UINT uDelay,UINT uResolution,LPTIMECALLBACK fptc,DWORD_PTR dwUser,UINT fuEvent)
*/

/*
WINMMAPI MMRESULT WINAPI timeKillEvent(UINT uTimerID)
*/
HB_FUNC( WINAPI_TIMEKILLEVENT )
{
  hb_retni( ( MMRESULT ) timeKillEvent( ( UINT ) hb_parni( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI timeGetDevCaps(LPTIMECAPS ptc,UINT cbtc)
*/

/*
WINMMAPI MMRESULT WINAPI timeBeginPeriod(UINT uPeriod)
*/
HB_FUNC( WINAPI_TIMEBEGINPERIOD )
{
  hb_retni( ( MMRESULT ) timeBeginPeriod( ( UINT ) hb_parni( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI timeEndPeriod(UINT uPeriod)
*/
HB_FUNC( WINAPI_TIMEENDPERIOD )
{
  hb_retni( ( MMRESULT ) timeEndPeriod( ( UINT ) hb_parni( 1 ) ) );
}

/*
WINMMAPI UINT WINAPI joyGetNumDevs(void)
*/
HB_FUNC( WINAPI_JOYGETNUMDEVS )
{
  hb_retni( ( UINT ) joyGetNumDevs() );
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
  hb_retni( ( MMRESULT ) joyReleaseCapture( ( UINT ) hb_parni( 1 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI joySetCapture(HWND hwnd,UINT uJoyID,UINT uPeriod,WINBOOL fChanged)
*/
HB_FUNC( WINAPI_JOYSETCAPTURE )
{
  hb_retni( ( MMRESULT ) joySetCapture( ( HWND ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ), ( WINBOOL ) hb_parl( 4 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI joySetThreshold(UINT uJoyID,UINT uThreshold)
*/
HB_FUNC( WINAPI_JOYSETTHRESHOLD )
{
  hb_retni( ( MMRESULT ) joySetThreshold( ( UINT ) hb_parni( 1 ), ( UINT ) hb_parni( 2 ) ) );
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
  hb_retni( ( MMRESULT ) mmioClose( ( HMMIO ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ) ) );
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
  hb_retnl( ( LONG ) mmioSeek( ( HMMIO ) hb_parptr( 1 ), ( LONG ) hb_parnl( 2 ), ( int ) hb_parni( 3 ) ) );
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
  hb_retni( ( MMRESULT ) mmioSetBuffer( ( HMMIO ) hb_parptr( 1 ), ( LPSTR ) hb_parc( 2 ), ( LONG ) hb_parnl( 3 ), ( UINT ) hb_parni( 4 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI mmioFlush(HMMIO hmmio,UINT fuFlush)
*/
HB_FUNC( WINAPI_MMIOFLUSH )
{
  hb_retni( ( MMRESULT ) mmioFlush( ( HMMIO ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ) ) );
}

/*
WINMMAPI MMRESULT WINAPI mmioAdvance(HMMIO hmmio,LPMMIOINFO pmmioinfo,UINT fuAdvance)
*/

/*
WINMMAPI LRESULT WINAPI mmioSendMessage(HMMIO hmmio,UINT uMsg,LPARAM lParam1,LPARAM lParam2)
*/
HB_FUNC( WINAPI_MMIOSENDMESSAGE )
{
  hb_retnl( ( LRESULT ) mmioSendMessage( ( HMMIO ) hb_parptr( 1 ), ( UINT ) hb_parni( 2 ), ( LPARAM ) hb_parnl( 3 ), ( LPARAM ) hb_parnl( 4 ) ) );
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
  hb_retnl( ( MCIERROR ) mciSendStringA( ( LPCSTR ) hb_parc( 1 ), ( LPSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ), ( HWND ) hb_parptr( 4 ) ) );
}

/*
WINMMAPI MCIERROR WINAPI mciSendStringW(LPCWSTR lpstrCommand,LPWSTR lpstrReturnString,UINT uReturnLength,HWND hwndCallback)
*/
HB_FUNC( WINAPI_MCISENDSTRINGW )
{
  hb_retnl( ( MCIERROR ) mciSendStringW( ( LPCWSTR ) hb_parc( 1 ), ( LPWSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ), ( HWND ) hb_parptr( 4 ) ) );
}

/*
WINMMAPI MCIDEVICEID WINAPI mciGetDeviceIDA(LPCSTR pszDevice)
*/
HB_FUNC( WINAPI_MCIGETDEVICEIDA )
{
  hb_retni( ( MCIDEVICEID ) mciGetDeviceIDA( ( LPCSTR ) hb_parc( 1 ) ) );
}

/*
WINMMAPI MCIDEVICEID WINAPI mciGetDeviceIDW(LPCWSTR pszDevice)
*/
HB_FUNC( WINAPI_MCIGETDEVICEIDW )
{
  hb_retni( ( MCIDEVICEID ) mciGetDeviceIDW( ( LPCWSTR ) hb_parc( 1 ) ) );
}

/*
WINMMAPI MCIDEVICEID WINAPI mciGetDeviceIDFromElementIDA(DWORD dwElementID,LPCSTR lpstrType)
*/
HB_FUNC( WINAPI_MCIGETDEVICEIDFROMELEMENTIDA )
{
  hb_retni( ( MCIDEVICEID ) mciGetDeviceIDFromElementIDA( ( DWORD ) hb_parnl( 1 ), ( LPCSTR ) hb_parc( 2 ) ) );
}

/*
WINMMAPI MCIDEVICEID WINAPI mciGetDeviceIDFromElementIDW(DWORD dwElementID,LPCWSTR lpstrType)
*/
HB_FUNC( WINAPI_MCIGETDEVICEIDFROMELEMENTIDW )
{
  hb_retni( ( MCIDEVICEID ) mciGetDeviceIDFromElementIDW( ( DWORD ) hb_parnl( 1 ), ( LPCWSTR ) hb_parc( 2 ) ) );
}

/*
WINMMAPI WINBOOL WINAPI mciGetErrorStringA(MCIERROR mcierr,LPSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_MCIGETERRORSTRINGA )
{
  hb_retl( ( WINBOOL ) mciGetErrorStringA( ( MCIERROR ) hb_parnl( 1 ), ( LPSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINMMAPI WINBOOL WINAPI mciGetErrorStringW(MCIERROR mcierr,LPWSTR pszText,UINT cchText)
*/
HB_FUNC( WINAPI_MCIGETERRORSTRINGW )
{
  hb_retl( ( WINBOOL ) mciGetErrorStringW( ( MCIERROR ) hb_parnl( 1 ), ( LPWSTR ) hb_parc( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

/*
WINMMAPI WINBOOL WINAPI mciSetYieldProc(MCIDEVICEID mciId,YIELDPROC fpYieldProc,DWORD dwYieldData)
*/

/*
WINMMAPI HTASK WINAPI mciGetCreatorTask(MCIDEVICEID mciId)
*/

/*
WINMMAPI YIELDPROC WINAPI mciGetYieldProc(MCIDEVICEID mciId,LPDWORD pdwYieldData)
*/
