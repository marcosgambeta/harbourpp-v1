/*

  WINAPI For Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
MIT License

Copyright (c) 2024 Marcos Antonio Gambeta

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

#ifndef _WINAPI_MMSYSTEM_
#define _WINAPI_MMSYSTEM_

// #ifndef _WINMM_
// #define WINMMAPI                                                     DECLSPEC_IMPORT
// #else
// #define WINMMAPI
// #endif
// #define _loadds
// #define _huge

#define MAXPNAMELEN                                                  32
#define MAXERRORLENGTH                                               256
#define MAX_JOYSTICKOEMVXDNAME                                       260

// #define _MMRESULT_

// #ifndef DEFINED_LPUINT
// #define DEFINED_LPUINT
// #endif

#define TIME_MS                                                      0x0001
#define TIME_SAMPLES                                                 0x0002
#define TIME_BYTES                                                   0x0004
#define TIME_SMPTE                                                   0x0008
#define TIME_MIDI                                                    0x0010
#define TIME_TICKS                                                   0x0020

// #ifndef MAKEFOURCC
// #define MAKEFOURCC(ch0,ch1,ch2,ch3)				\
// #endif /* MAKEFOURCC */

#define MM_JOY1MOVE                                                  0x3A0
#define MM_JOY2MOVE                                                  0x3A1
#define MM_JOY1ZMOVE                                                 0x3A2
#define MM_JOY2ZMOVE                                                 0x3A3
#define MM_JOY1BUTTONDOWN                                            0x3B5
#define MM_JOY2BUTTONDOWN                                            0x3B6
#define MM_JOY1BUTTONUP                                              0x3B7
#define MM_JOY2BUTTONUP                                              0x3B8

#define MM_MCINOTIFY                                                 0x3B9

#define MM_WOM_OPEN                                                  0x3BB
#define MM_WOM_CLOSE                                                 0x3BC
#define MM_WOM_DONE                                                  0x3BD

#define MM_WIM_OPEN                                                  0x3BE
#define MM_WIM_CLOSE                                                 0x3BF
#define MM_WIM_DATA                                                  0x3C0

#define MM_MIM_OPEN                                                  0x3C1
#define MM_MIM_CLOSE                                                 0x3C2
#define MM_MIM_DATA                                                  0x3C3
#define MM_MIM_LONGDATA                                              0x3C4
#define MM_MIM_ERROR                                                 0x3C5
#define MM_MIM_LONGERROR                                             0x3C6

#define MM_MOM_OPEN                                                  0x3C7
#define MM_MOM_CLOSE                                                 0x3C8
#define MM_MOM_DONE                                                  0x3C9

#ifndef MM_DRVM_OPEN
#define MM_DRVM_OPEN                                                 0x3D0
#define MM_DRVM_CLOSE                                                0x3D1
#define MM_DRVM_DATA                                                 0x3D2
#define MM_DRVM_ERROR                                                0x3D3
#endif

#define MM_STREAM_OPEN                                               0x3D4
#define MM_STREAM_CLOSE                                              0x3D5
#define MM_STREAM_DONE                                               0x3D6
#define MM_STREAM_ERROR                                              0x3D7

#define MM_MOM_POSITIONCB                                            0x3CA

#ifndef MM_MCISIGNAL
#define MM_MCISIGNAL                                                 0x3CB
#endif

#define MM_MIM_MOREDATA                                              0x3CC

#define MM_MIXM_LINE_CHANGE                                          0x3D0
#define MM_MIXM_CONTROL_CHANGE                                       0x3D1

#define MMSYSERR_BASE                                                0
#define WAVERR_BASE                                                  32
#define MIDIERR_BASE                                                 64
#define TIMERR_BASE                                                  96
#define JOYERR_BASE                                                  160
#define MCIERR_BASE                                                  256
#define MIXERR_BASE                                                  1024

#define MCI_STRING_OFFSET                                            512
#define MCI_VD_OFFSET                                                1024
#define MCI_CD_OFFSET                                                1088
#define MCI_WAVE_OFFSET                                              1152
#define MCI_SEQ_OFFSET                                               1216

#define MMSYSERR_NOERROR                                             0
#define MMSYSERR_ERROR                                               (MMSYSERR_BASE+1)
#define MMSYSERR_BADDEVICEID                                         (MMSYSERR_BASE+2)
#define MMSYSERR_NOTENABLED                                          (MMSYSERR_BASE+3)
#define MMSYSERR_ALLOCATED                                           (MMSYSERR_BASE+4)
#define MMSYSERR_INVALHANDLE                                         (MMSYSERR_BASE+5)
#define MMSYSERR_NODRIVER                                            (MMSYSERR_BASE+6)
#define MMSYSERR_NOMEM                                               (MMSYSERR_BASE+7)
#define MMSYSERR_NOTSUPPORTED                                        (MMSYSERR_BASE+8)
#define MMSYSERR_BADERRNUM                                           (MMSYSERR_BASE+9)
#define MMSYSERR_INVALFLAG                                           (MMSYSERR_BASE+10)
#define MMSYSERR_INVALPARAM                                          (MMSYSERR_BASE+11)
#define MMSYSERR_HANDLEBUSY                                          (MMSYSERR_BASE+12)

#define MMSYSERR_INVALIDALIAS                                        (MMSYSERR_BASE+13)
#define MMSYSERR_BADDB                                               (MMSYSERR_BASE+14)
#define MMSYSERR_KEYNOTFOUND                                         (MMSYSERR_BASE+15)
#define MMSYSERR_READERROR                                           (MMSYSERR_BASE+16)
#define MMSYSERR_WRITEERROR                                          (MMSYSERR_BASE+17)
#define MMSYSERR_DELETEERROR                                         (MMSYSERR_BASE+18)
#define MMSYSERR_VALNOTFOUND                                         (MMSYSERR_BASE+19)
#define MMSYSERR_NODRIVERCB                                          (MMSYSERR_BASE+20)
#define MMSYSERR_MOREDATA                                            (MMSYSERR_BASE+21)
#define MMSYSERR_LASTERROR                                           (MMSYSERR_BASE+21)

#ifndef MMNODRV

#ifndef DRV_LOAD
#define DRV_LOAD                                                     0x0001
#define DRV_ENABLE                                                   0x0002
#define DRV_OPEN                                                     0x0003
#define DRV_CLOSE                                                    0x0004
#define DRV_DISABLE                                                  0x0005
#define DRV_FREE                                                     0x0006
#define DRV_CONFIGURE                                                0x0007
#define DRV_QUERYCONFIGURE                                           0x0008
#define DRV_INSTALL                                                  0x0009
#define DRV_REMOVE                                                   0x000A
#define DRV_EXITSESSION                                              0x000B
#define DRV_POWER                                                    0x000F
#define DRV_RESERVED                                                 0x0800
#define DRV_USER                                                     0x4000

#define DRVCNF_CANCEL                                                0x0000
#define DRVCNF_OK                                                    0x0001
#define DRVCNF_RESTART                                               0x0002

#endif

#define DRV_CANCEL                                                   DRVCNF_CANCEL
#define DRV_OK                                                       DRVCNF_OK
#define DRV_RESTART                                                  DRVCNF_RESTART
#define DRV_MCI_FIRST                                                DRV_RESERVED
#define DRV_MCI_LAST                                                 (DRV_RESERVED+0xFFF)
#endif

#define CALLBACK_TYPEMASK                                            0x00070000
#define CALLBACK_NULL                                                0x00000000
#define CALLBACK_WINDOW                                              0x00010000
#define CALLBACK_TASK                                                0x00020000
#define CALLBACK_FUNCTION                                            0x00030000
// #define CALLBACK_THREAD                                              (CALLBACK_TASK)
#define CALLBACK_EVENT                                               0x00050000

// #ifndef MMNOMMSYSTEM
// #define OutputDebugStr                                               OutputDebugString
// #endif

#ifndef MMNOSOUND

// #define sndPlaySound                                                 __MINGW_NAME_AW(sndPlaySound)

#define SND_SYNC                                                     0x0000
#define SND_ASYNC                                                    0x0001
#define SND_NODEFAULT                                                0x0002
#define SND_MEMORY                                                   0x0004
#define SND_LOOP                                                     0x0008
#define SND_NOSTOP                                                   0x0010
#define SND_NOWAIT                                                   0x00002000
#define SND_ALIAS                                                    0x00010000
#define SND_ALIAS_ID                                                 0x00110000
#define SND_FILENAME                                                 0x00020000
#define SND_RESOURCE                                                 0x00040004
#define SND_PURGE                                                    0x0040
#define SND_APPLICATION                                              0x0080

#define SND_ALIAS_START                                              0

// #define sndAlias(c0,c1)                                              (SND_ALIAS_START+((DWORD)(BYTE)(c0)|((DWORD)(BYTE)(c1)<<8)))

// #define SND_ALIAS_SYSTEMASTERISK                                     sndAlias('S','*')
// #define SND_ALIAS_SYSTEMQUESTION                                     sndAlias('S','?')
// #define SND_ALIAS_SYSTEMHAND                                         sndAlias('S','H')
// #define SND_ALIAS_SYSTEMEXIT                                         sndAlias('S','E')
// #define SND_ALIAS_SYSTEMSTART                                        sndAlias('S','S')
// #define SND_ALIAS_SYSTEMWELCOME                                      sndAlias('S','W')
// #define SND_ALIAS_SYSTEMEXCLAMATION                                  sndAlias('S','!')
// #define SND_ALIAS_SYSTEMDEFAULT                                      sndAlias('S','D')

// #define PlaySound                                                    __MINGW_NAME_AW(PlaySound)
#endif

#ifndef MMNOWAVE

#define WAVERR_BADFORMAT                                             (WAVERR_BASE+0)
#define WAVERR_STILLPLAYING                                          (WAVERR_BASE+1)
#define WAVERR_UNPREPARED                                            (WAVERR_BASE+2)
#define WAVERR_SYNC                                                  (WAVERR_BASE+3)
#define WAVERR_LASTERROR                                             (WAVERR_BASE+3)

#define WOM_OPEN                                                     MM_WOM_OPEN
#define WOM_CLOSE                                                    MM_WOM_CLOSE
#define WOM_DONE                                                     MM_WOM_DONE
#define WIM_OPEN                                                     MM_WIM_OPEN
#define WIM_CLOSE                                                    MM_WIM_CLOSE
#define WIM_DATA                                                     MM_WIM_DATA

// #define WAVE_MAPPER                                                  ((UINT)-1)

#define WAVE_FORMAT_QUERY                                            0x0001
#define WAVE_ALLOWSYNC                                               0x0002
#define WAVE_MAPPED                                                  0x0004
#define WAVE_FORMAT_DIRECT                                           0x0008
#define WAVE_FORMAT_DIRECT_QUERY                                     hb_bitor(WAVE_FORMAT_QUERY, WAVE_FORMAT_DIRECT)

#define WHDR_DONE                                                    0x00000001
#define WHDR_PREPARED                                                0x00000002
#define WHDR_BEGINLOOP                                               0x00000004
#define WHDR_ENDLOOP                                                 0x00000008
#define WHDR_INQUEUE                                                 0x00000010

#define WAVECAPS_PITCH                                               0x0001
#define WAVECAPS_PLAYBACKRATE                                        0x0002
#define WAVECAPS_VOLUME                                              0x0004
#define WAVECAPS_LRVOLUME                                            0x0008
#define WAVECAPS_SYNC                                                0x0010
#define WAVECAPS_SAMPLEACCURATE                                      0x0020

#define WAVE_INVALIDFORMAT                                           0x00000000
#define WAVE_FORMAT_1M08                                             0x00000001
#define WAVE_FORMAT_1S08                                             0x00000002
#define WAVE_FORMAT_1M16                                             0x00000004
#define WAVE_FORMAT_1S16                                             0x00000008
#define WAVE_FORMAT_2M08                                             0x00000010
#define WAVE_FORMAT_2S08                                             0x00000020
#define WAVE_FORMAT_2M16                                             0x00000040
#define WAVE_FORMAT_2S16                                             0x00000080
#define WAVE_FORMAT_4M08                                             0x00000100
#define WAVE_FORMAT_4S08                                             0x00000200
#define WAVE_FORMAT_4M16                                             0x00000400
#define WAVE_FORMAT_4S16                                             0x00000800

#define WAVE_FORMAT_44M08                                            0x00000100
#define WAVE_FORMAT_44S08                                            0x00000200
#define WAVE_FORMAT_44M16                                            0x00000400
#define WAVE_FORMAT_44S16                                            0x00000800
#define WAVE_FORMAT_48M08                                            0x00001000
#define WAVE_FORMAT_48S08                                            0x00002000
#define WAVE_FORMAT_48M16                                            0x00004000
#define WAVE_FORMAT_48S16                                            0x00008000
#define WAVE_FORMAT_96M08                                            0x00010000
#define WAVE_FORMAT_96S08                                            0x00020000
#define WAVE_FORMAT_96M16                                            0x00040000
#define WAVE_FORMAT_96S16                                            0x00080000

#ifndef WAVE_FORMAT_PCM
#define WAVE_FORMAT_PCM                                              1
#endif

// #ifndef _WAVEFORMATEX_
// #define _WAVEFORMATEX_
// #endif

// #define waveOutGetDevCaps                                            __MINGW_NAME_AW(waveOutGetDevCaps)
// #define waveOutGetErrorText                                          __MINGW_NAME_AW(waveOutGetErrorText)
// #define waveInGetDevCaps                                             __MINGW_NAME_AW(waveInGetDevCaps)
// #define waveInGetErrorText                                           __MINGW_NAME_AW(waveInGetErrorText)

#endif

#ifndef MMNOMIDI

#define MIDIERR_UNPREPARED                                           (MIDIERR_BASE+0)
#define MIDIERR_STILLPLAYING                                         (MIDIERR_BASE+1)
#define MIDIERR_NOMAP                                                (MIDIERR_BASE+2)
#define MIDIERR_NOTREADY                                             (MIDIERR_BASE+3)
#define MIDIERR_NODEVICE                                             (MIDIERR_BASE+4)
#define MIDIERR_INVALIDSETUP                                         (MIDIERR_BASE+5)
#define MIDIERR_BADOPENMODE                                          (MIDIERR_BASE+6)
#define MIDIERR_DONT_CONTINUE                                        (MIDIERR_BASE+7)
#define MIDIERR_LASTERROR                                            (MIDIERR_BASE+7)

#define MIDIPATCHSIZE                                                128

#define MIM_OPEN                                                     MM_MIM_OPEN
#define MIM_CLOSE                                                    MM_MIM_CLOSE
#define MIM_DATA                                                     MM_MIM_DATA
#define MIM_LONGDATA                                                 MM_MIM_LONGDATA
#define MIM_ERROR                                                    MM_MIM_ERROR
#define MIM_LONGERROR                                                MM_MIM_LONGERROR
#define MOM_OPEN                                                     MM_MOM_OPEN
#define MOM_CLOSE                                                    MM_MOM_CLOSE
#define MOM_DONE                                                     MM_MOM_DONE

#define MIM_MOREDATA                                                 MM_MIM_MOREDATA
#define MOM_POSITIONCB                                               MM_MOM_POSITIONCB

// #define MIDIMAPPER                                                   ((UINT)-1)
// #define MIDI_MAPPER                                                  ((UINT)-1)

#define MIDI_IO_STATUS                                               0x00000020

#define MIDI_CACHE_ALL                                               1
#define MIDI_CACHE_BESTFIT                                           2
#define MIDI_CACHE_QUERY                                             3
#define MIDI_UNCACHE                                                 4

#define MOD_MIDIPORT                                                 1
#define MOD_SYNTH                                                    2
#define MOD_SQSYNTH                                                  3
#define MOD_FMSYNTH                                                  4
#define MOD_MAPPER                                                   5
#define MOD_WAVETABLE                                                6
#define MOD_SWSYNTH                                                  7

#define MIDICAPS_VOLUME                                              0x0001
#define MIDICAPS_LRVOLUME                                            0x0002
#define MIDICAPS_CACHE                                               0x0004
#define MIDICAPS_STREAM                                              0x0008

#define MHDR_DONE                                                    0x00000001
#define MHDR_PREPARED                                                0x00000002
#define MHDR_INQUEUE                                                 0x00000004
#define MHDR_ISSTRM                                                  0x00000008

#define MEVT_F_SHORT                                                 0x00000000
#define MEVT_F_LONG                                                  0x80000000
#define MEVT_F_CALLBACK                                              0x40000000

// #define MEVT_EVENTTYPE(x)                                            ((BYTE)(((x)>>24)&0xFF))
// #define MEVT_EVENTPARM(x)                                            ((DWORD)((x)&0x00FFFFFF))

// #define MEVT_SHORTMSG                                                ((BYTE)0x00)
// #define MEVT_TEMPO                                                   ((BYTE)0x01)
// #define MEVT_NOP                                                     ((BYTE)0x02)

// #define MEVT_LONGMSG                                                 ((BYTE)0x80)
// #define MEVT_COMMENT                                                 ((BYTE)0x82)
// #define MEVT_VERSION                                                 ((BYTE)0x84)

#define MIDISTRM_ERROR                                               (-2)

#define MIDIPROP_SET                                                 0x80000000
#define MIDIPROP_GET                                                 0x40000000

#define MIDIPROP_TIMEDIV                                             0x00000001
#define MIDIPROP_TEMPO                                               0x00000002

// #define midiOutGetDevCaps                                            __MINGW_NAME_AW(midiOutGetDevCaps)
// #define midiOutGetErrorText                                          __MINGW_NAME_AW(midiOutGetErrorText)
// #define midiInGetDevCaps                                             __MINGW_NAME_AW(midiInGetDevCaps)
// #define midiInGetErrorText                                           __MINGW_NAME_AW(midiInGetErrorText)

#endif

#ifndef MMNOAUX

// #define AUX_MAPPER                                                   ((UINT)-1)

#define AUXCAPS_CDAUDIO                                              1
#define AUXCAPS_AUXIN                                                2

#define AUXCAPS_VOLUME                                               0x0001
#define AUXCAPS_LRVOLUME                                             0x0002

// #define auxGetDevCaps                                                __MINGW_NAME_AW(auxGetDevCaps)

#endif

#ifndef MMNOMIXER

#define MIXER_SHORT_NAME_CHARS                                       16
#define MIXER_LONG_NAME_CHARS                                        64

#define MIXERR_INVALLINE                                             (MIXERR_BASE+0)
#define MIXERR_INVALCONTROL                                          (MIXERR_BASE+1)
#define MIXERR_INVALVALUE                                            (MIXERR_BASE+2)
#define MIXERR_LASTERROR                                             (MIXERR_BASE+2)

#define MIXER_OBJECTF_HANDLE                                         0x80000000
#define MIXER_OBJECTF_MIXER                                          0x00000000
#define MIXER_OBJECTF_HMIXER                                         hb_bitor(MIXER_OBJECTF_HANDLE, MIXER_OBJECTF_MIXER)
#define MIXER_OBJECTF_WAVEOUT                                        0x10000000
#define MIXER_OBJECTF_HWAVEOUT                                       hb_bitor(MIXER_OBJECTF_HANDLE, MIXER_OBJECTF_WAVEOUT)
#define MIXER_OBJECTF_WAVEIN                                         0x20000000
#define MIXER_OBJECTF_HWAVEIN                                        hb_bitor(MIXER_OBJECTF_HANDLE, MIXER_OBJECTF_WAVEIN)
#define MIXER_OBJECTF_MIDIOUT                                        0x30000000
#define MIXER_OBJECTF_HMIDIOUT                                       hb_bitor(MIXER_OBJECTF_HANDLE, MIXER_OBJECTF_MIDIOUT)
#define MIXER_OBJECTF_MIDIIN                                         0x40000000
#define MIXER_OBJECTF_HMIDIIN                                        hb_bitor(MIXER_OBJECTF_HANDLE, MIXER_OBJECTF_MIDIIN)
#define MIXER_OBJECTF_AUX                                            0x50000000

// #define mixerGetDevCaps                                              __MINGW_NAME_AW(mixerGetDevCaps)

#define MIXERLINE_LINEF_ACTIVE                                       0x00000001
#define MIXERLINE_LINEF_DISCONNECTED                                 0x00008000
#define MIXERLINE_LINEF_SOURCE                                       0x80000000

#define MIXERLINE_COMPONENTTYPE_DST_FIRST                            0x0
#define MIXERLINE_COMPONENTTYPE_DST_UNDEFINED                        (MIXERLINE_COMPONENTTYPE_DST_FIRST+0)
#define MIXERLINE_COMPONENTTYPE_DST_DIGITAL                          (MIXERLINE_COMPONENTTYPE_DST_FIRST+1)
#define MIXERLINE_COMPONENTTYPE_DST_LINE                             (MIXERLINE_COMPONENTTYPE_DST_FIRST+2)
#define MIXERLINE_COMPONENTTYPE_DST_MONITOR                          (MIXERLINE_COMPONENTTYPE_DST_FIRST+3)
#define MIXERLINE_COMPONENTTYPE_DST_SPEAKERS                         (MIXERLINE_COMPONENTTYPE_DST_FIRST+4)
#define MIXERLINE_COMPONENTTYPE_DST_HEADPHONES                       (MIXERLINE_COMPONENTTYPE_DST_FIRST+5)
#define MIXERLINE_COMPONENTTYPE_DST_TELEPHONE                        (MIXERLINE_COMPONENTTYPE_DST_FIRST+6)
#define MIXERLINE_COMPONENTTYPE_DST_WAVEIN                           (MIXERLINE_COMPONENTTYPE_DST_FIRST+7)
#define MIXERLINE_COMPONENTTYPE_DST_VOICEIN                          (MIXERLINE_COMPONENTTYPE_DST_FIRST+8)
#define MIXERLINE_COMPONENTTYPE_DST_LAST                             (MIXERLINE_COMPONENTTYPE_DST_FIRST+8)
#define MIXERLINE_COMPONENTTYPE_SRC_FIRST                            0x00001000
#define MIXERLINE_COMPONENTTYPE_SRC_UNDEFINED                        (MIXERLINE_COMPONENTTYPE_SRC_FIRST+0)
#define MIXERLINE_COMPONENTTYPE_SRC_DIGITAL                          (MIXERLINE_COMPONENTTYPE_SRC_FIRST+1)
#define MIXERLINE_COMPONENTTYPE_SRC_LINE                             (MIXERLINE_COMPONENTTYPE_SRC_FIRST+2)
#define MIXERLINE_COMPONENTTYPE_SRC_MICROPHONE                       (MIXERLINE_COMPONENTTYPE_SRC_FIRST+3)
#define MIXERLINE_COMPONENTTYPE_SRC_SYNTHESIZER                      (MIXERLINE_COMPONENTTYPE_SRC_FIRST+4)
#define MIXERLINE_COMPONENTTYPE_SRC_COMPACTDISC                      (MIXERLINE_COMPONENTTYPE_SRC_FIRST+5)
#define MIXERLINE_COMPONENTTYPE_SRC_TELEPHONE                        (MIXERLINE_COMPONENTTYPE_SRC_FIRST+6)
#define MIXERLINE_COMPONENTTYPE_SRC_PCSPEAKER                        (MIXERLINE_COMPONENTTYPE_SRC_FIRST+7)
#define MIXERLINE_COMPONENTTYPE_SRC_WAVEOUT                          (MIXERLINE_COMPONENTTYPE_SRC_FIRST+8)
#define MIXERLINE_COMPONENTTYPE_SRC_AUXILIARY                        (MIXERLINE_COMPONENTTYPE_SRC_FIRST+9)
#define MIXERLINE_COMPONENTTYPE_SRC_ANALOG                           (MIXERLINE_COMPONENTTYPE_SRC_FIRST+10)
#define MIXERLINE_COMPONENTTYPE_SRC_LAST                             (MIXERLINE_COMPONENTTYPE_SRC_FIRST+10)

#define MIXERLINE_TARGETTYPE_UNDEFINED                               0
#define MIXERLINE_TARGETTYPE_WAVEOUT                                 1
#define MIXERLINE_TARGETTYPE_WAVEIN                                  2
#define MIXERLINE_TARGETTYPE_MIDIOUT                                 3
#define MIXERLINE_TARGETTYPE_MIDIIN                                  4
#define MIXERLINE_TARGETTYPE_AUX                                     5

// #define mixerGetLineInfo                                             __MINGW_NAME_AW(mixerGetLineInfo)

#define MIXER_GETLINEINFOF_DESTINATION                               0x00000000
#define MIXER_GETLINEINFOF_SOURCE                                    0x00000001
#define MIXER_GETLINEINFOF_LINEID                                    0x00000002
#define MIXER_GETLINEINFOF_COMPONENTTYPE                             0x00000003
#define MIXER_GETLINEINFOF_TARGETTYPE                                0x00000004

#define MIXER_GETLINEINFOF_QUERYMASK                                 0x0000000F

#define MIXERCONTROL_CONTROLF_UNIFORM                                0x00000001
#define MIXERCONTROL_CONTROLF_MULTIPLE                               0x00000002
#define MIXERCONTROL_CONTROLF_DISABLED                               0x80000000

#define MIXERCONTROL_CT_CLASS_MASK                                   0xF0000000
#define MIXERCONTROL_CT_CLASS_CUSTOM                                 0x00000000
#define MIXERCONTROL_CT_CLASS_METER                                  0x10000000
#define MIXERCONTROL_CT_CLASS_SWITCH                                 0x20000000
#define MIXERCONTROL_CT_CLASS_NUMBER                                 0x30000000
#define MIXERCONTROL_CT_CLASS_SLIDER                                 0x40000000
#define MIXERCONTROL_CT_CLASS_FADER                                  0x50000000
#define MIXERCONTROL_CT_CLASS_TIME                                   0x60000000
#define MIXERCONTROL_CT_CLASS_LIST                                   0x70000000

#define MIXERCONTROL_CT_SUBCLASS_MASK                                0x0F000000

#define MIXERCONTROL_CT_SC_SWITCH_BOOLEAN                            0x00000000
#define MIXERCONTROL_CT_SC_SWITCH_BUTTON                             0x01000000

#define MIXERCONTROL_CT_SC_METER_POLLED                              0x00000000

#define MIXERCONTROL_CT_SC_TIME_MICROSECS                            0x00000000
#define MIXERCONTROL_CT_SC_TIME_MILLISECS                            0x01000000

#define MIXERCONTROL_CT_SC_LIST_SINGLE                               0x00000000
#define MIXERCONTROL_CT_SC_LIST_MULTIPLE                             0x01000000

#define MIXERCONTROL_CT_UNITS_MASK                                   0x00FF0000
#define MIXERCONTROL_CT_UNITS_CUSTOM                                 0x00000000
#define MIXERCONTROL_CT_UNITS_BOOLEAN                                0x00010000
#define MIXERCONTROL_CT_UNITS_SIGNED                                 0x00020000
#define MIXERCONTROL_CT_UNITS_UNSIGNED                               0x00030000
#define MIXERCONTROL_CT_UNITS_DECIBELS                               0x00040000
#define MIXERCONTROL_CT_UNITS_PERCENT                                0x00050000

#define MIXERCONTROL_CONTROLTYPE_CUSTOM                              hb_bitor(MIXERCONTROL_CT_CLASS_CUSTOM, MIXERCONTROL_CT_UNITS_CUSTOM)
#define MIXERCONTROL_CONTROLTYPE_BOOLEANMETER                        hb_bitor(MIXERCONTROL_CT_CLASS_METER, MIXERCONTROL_CT_SC_METER_POLLED, MIXERCONTROL_CT_UNITS_BOOLEAN)
#define MIXERCONTROL_CONTROLTYPE_SIGNEDMETER                         hb_bitor(MIXERCONTROL_CT_CLASS_METER, MIXERCONTROL_CT_SC_METER_POLLED, MIXERCONTROL_CT_UNITS_SIGNED)
#define MIXERCONTROL_CONTROLTYPE_PEAKMETER                           (MIXERCONTROL_CONTROLTYPE_SIGNEDMETER+1)
#define MIXERCONTROL_CONTROLTYPE_UNSIGNEDMETER                       hb_bitor(MIXERCONTROL_CT_CLASS_METER, MIXERCONTROL_CT_SC_METER_POLLED, MIXERCONTROL_CT_UNITS_UNSIGNED)
#define MIXERCONTROL_CONTROLTYPE_BOOLEAN                             hb_bitor(MIXERCONTROL_CT_CLASS_SWITCH, MIXERCONTROL_CT_SC_SWITCH_BOOLEAN, MIXERCONTROL_CT_UNITS_BOOLEAN)
#define MIXERCONTROL_CONTROLTYPE_ONOFF                               (MIXERCONTROL_CONTROLTYPE_BOOLEAN+1)
#define MIXERCONTROL_CONTROLTYPE_MUTE                                (MIXERCONTROL_CONTROLTYPE_BOOLEAN+2)
#define MIXERCONTROL_CONTROLTYPE_MONO                                (MIXERCONTROL_CONTROLTYPE_BOOLEAN+3)
#define MIXERCONTROL_CONTROLTYPE_LOUDNESS                            (MIXERCONTROL_CONTROLTYPE_BOOLEAN+4)
#define MIXERCONTROL_CONTROLTYPE_STEREOENH                           (MIXERCONTROL_CONTROLTYPE_BOOLEAN+5)
#define MIXERCONTROL_CONTROLTYPE_BASS_BOOST                          (MIXERCONTROL_CONTROLTYPE_BOOLEAN+0x00002277)
#define MIXERCONTROL_CONTROLTYPE_BUTTON                              hb_bitor(MIXERCONTROL_CT_CLASS_SWITCH, MIXERCONTROL_CT_SC_SWITCH_BUTTON, MIXERCONTROL_CT_UNITS_BOOLEAN)
#define MIXERCONTROL_CONTROLTYPE_DECIBELS                            hb_bitor(MIXERCONTROL_CT_CLASS_NUMBER, MIXERCONTROL_CT_UNITS_DECIBELS)
#define MIXERCONTROL_CONTROLTYPE_SIGNED                              hb_bitor(MIXERCONTROL_CT_CLASS_NUMBER, MIXERCONTROL_CT_UNITS_SIGNED)
#define MIXERCONTROL_CONTROLTYPE_UNSIGNED                            hb_bitor(MIXERCONTROL_CT_CLASS_NUMBER, MIXERCONTROL_CT_UNITS_UNSIGNED)
#define MIXERCONTROL_CONTROLTYPE_PERCENT                             hb_bitor(MIXERCONTROL_CT_CLASS_NUMBER, MIXERCONTROL_CT_UNITS_PERCENT)
#define MIXERCONTROL_CONTROLTYPE_SLIDER                              hb_bitor(MIXERCONTROL_CT_CLASS_SLIDER, MIXERCONTROL_CT_UNITS_SIGNED)
#define MIXERCONTROL_CONTROLTYPE_PAN                                 (MIXERCONTROL_CONTROLTYPE_SLIDER+1)
#define MIXERCONTROL_CONTROLTYPE_QSOUNDPAN                           (MIXERCONTROL_CONTROLTYPE_SLIDER+2)
#define MIXERCONTROL_CONTROLTYPE_FADER                               hb_bitor(MIXERCONTROL_CT_CLASS_FADER, MIXERCONTROL_CT_UNITS_UNSIGNED)
#define MIXERCONTROL_CONTROLTYPE_VOLUME                              (MIXERCONTROL_CONTROLTYPE_FADER+1)
#define MIXERCONTROL_CONTROLTYPE_BASS                                (MIXERCONTROL_CONTROLTYPE_FADER+2)
#define MIXERCONTROL_CONTROLTYPE_TREBLE                              (MIXERCONTROL_CONTROLTYPE_FADER+3)
#define MIXERCONTROL_CONTROLTYPE_EQUALIZER                           (MIXERCONTROL_CONTROLTYPE_FADER+4)
#define MIXERCONTROL_CONTROLTYPE_SINGLESELECT                        hb_bitor(MIXERCONTROL_CT_CLASS_LIST, MIXERCONTROL_CT_SC_LIST_SINGLE, MIXERCONTROL_CT_UNITS_BOOLEAN)
#define MIXERCONTROL_CONTROLTYPE_MUX                                 (MIXERCONTROL_CONTROLTYPE_SINGLESELECT+1)
#define MIXERCONTROL_CONTROLTYPE_MULTIPLESELECT                      hb_bitor(MIXERCONTROL_CT_CLASS_LIST, MIXERCONTROL_CT_SC_LIST_MULTIPLE, MIXERCONTROL_CT_UNITS_BOOLEAN)
#define MIXERCONTROL_CONTROLTYPE_MIXER                               (MIXERCONTROL_CONTROLTYPE_MULTIPLESELECT+1)
#define MIXERCONTROL_CONTROLTYPE_MICROTIME                           hb_bitor(MIXERCONTROL_CT_CLASS_TIME, MIXERCONTROL_CT_SC_TIME_MICROSECS, MIXERCONTROL_CT_UNITS_UNSIGNED)
#define MIXERCONTROL_CONTROLTYPE_MILLITIME                           hb_bitor(MIXERCONTROL_CT_CLASS_TIME, MIXERCONTROL_CT_SC_TIME_MILLISECS, MIXERCONTROL_CT_UNITS_UNSIGNED)

// #define mixerGetLineControls                                         __MINGW_NAME_AW(mixerGetLineControls)

#define MIXER_GETLINECONTROLSF_ALL                                   0x00000000
#define MIXER_GETLINECONTROLSF_ONEBYID                               0x00000001
#define MIXER_GETLINECONTROLSF_ONEBYTYPE                             0x00000002

#define MIXER_GETLINECONTROLSF_QUERYMASK                             0x0000000F

// #define mixerGetControlDetails                                       __MINGW_NAME_AW(mixerGetControlDetails)

#define MIXER_GETCONTROLDETAILSF_VALUE                               0x00000000
#define MIXER_GETCONTROLDETAILSF_LISTTEXT                            0x00000001

#define MIXER_GETCONTROLDETAILSF_QUERYMASK                           0x0000000F


#define MIXER_SETCONTROLDETAILSF_VALUE                               0x00000000
#define MIXER_SETCONTROLDETAILSF_CUSTOM                              0x00000001

#define MIXER_SETCONTROLDETAILSF_QUERYMASK                           0x0000000F
#endif

#ifndef MMNOTIMER

#define TIMERR_NOERROR                                               (0)
#define TIMERR_NOCANDO                                               (TIMERR_BASE+1)
#define TIMERR_STRUCT                                                (TIMERR_BASE+33)

#define TIME_ONESHOT                                                 0x0000
#define TIME_PERIODIC                                                0x0001

#define TIME_CALLBACK_FUNCTION                                       0x0000
#define TIME_CALLBACK_EVENT_SET                                      0x0010
#define TIME_CALLBACK_EVENT_PULSE                                    0x0020
#define TIME_KILL_SYNCHRONOUS                                        0x0100

#endif

#ifndef MMNOJOY

#define JOYERR_NOERROR                                               (0)
#define JOYERR_PARMS                                                 (JOYERR_BASE+5)
#define JOYERR_NOCANDO                                               (JOYERR_BASE+6)
#define JOYERR_UNPLUGGED                                             (JOYERR_BASE+7)

#define JOY_BUTTON1                                                  0x0001
#define JOY_BUTTON2                                                  0x0002
#define JOY_BUTTON3                                                  0x0004
#define JOY_BUTTON4                                                  0x0008
#define JOY_BUTTON1CHG                                               0x0100
#define JOY_BUTTON2CHG                                               0x0200
#define JOY_BUTTON3CHG                                               0x0400
#define JOY_BUTTON4CHG                                               0x0800

#define JOY_BUTTON5                                                  0x00000010
#define JOY_BUTTON6                                                  0x00000020
#define JOY_BUTTON7                                                  0x00000040
#define JOY_BUTTON8                                                  0x00000080
#define JOY_BUTTON9                                                  0x00000100
#define JOY_BUTTON10                                                 0x00000200
#define JOY_BUTTON11                                                 0x00000400
#define JOY_BUTTON12                                                 0x00000800
#define JOY_BUTTON13                                                 0x00001000
#define JOY_BUTTON14                                                 0x00002000
#define JOY_BUTTON15                                                 0x00004000
#define JOY_BUTTON16                                                 0x00008000
#define JOY_BUTTON17                                                 0x00010000
#define JOY_BUTTON18                                                 0x00020000
#define JOY_BUTTON19                                                 0x00040000
#define JOY_BUTTON20                                                 0x00080000
#define JOY_BUTTON21                                                 0x00100000
#define JOY_BUTTON22                                                 0x00200000
#define JOY_BUTTON23                                                 0x00400000
#define JOY_BUTTON24                                                 0x00800000
#define JOY_BUTTON25                                                 0x01000000
#define JOY_BUTTON26                                                 0x02000000
#define JOY_BUTTON27                                                 0x04000000
#define JOY_BUTTON28                                                 0x08000000
#define JOY_BUTTON29                                                 0x10000000
#define JOY_BUTTON30                                                 0x20000000
#define JOY_BUTTON31                                                 0x40000000
#define JOY_BUTTON32                                                 0x80000000

// #define JOY_POVCENTERED                                              (WORD)-1
#define JOY_POVFORWARD                                               0
#define JOY_POVRIGHT                                                 9000
#define JOY_POVBACKWARD                                              18000
#define JOY_POVLEFT                                                  27000

#define JOY_RETURNX                                                  0x00000001
#define JOY_RETURNY                                                  0x00000002
#define JOY_RETURNZ                                                  0x00000004
#define JOY_RETURNR                                                  0x00000008
#define JOY_RETURNU                                                  0x00000010
#define JOY_RETURNV                                                  0x00000020
#define JOY_RETURNPOV                                                0x00000040
#define JOY_RETURNBUTTONS                                            0x00000080
#define JOY_RETURNRAWDATA                                            0x00000100
#define JOY_RETURNPOVCTS                                             0x00000200
#define JOY_RETURNCENTERED                                           0x00000400
#define JOY_USEDEADZONE                                              0x00000800
#define JOY_RETURNALL                                                hb_bitor(JOY_RETURNX, JOY_RETURNY, JOY_RETURNZ, JOY_RETURNR, JOY_RETURNU, JOY_RETURNV, JOY_RETURNPOV, JOY_RETURNBUTTONS)
#define JOY_CAL_READALWAYS                                           0x00010000
#define JOY_CAL_READXYONLY                                           0x00020000
#define JOY_CAL_READ3                                                0x00040000
#define JOY_CAL_READ4                                                0x00080000
#define JOY_CAL_READXONLY                                            0x00100000
#define JOY_CAL_READYONLY                                            0x00200000
#define JOY_CAL_READ5                                                0x00400000
#define JOY_CAL_READ6                                                0x00800000
#define JOY_CAL_READZONLY                                            0x01000000
#define JOY_CAL_READRONLY                                            0x02000000
#define JOY_CAL_READUONLY                                            0x04000000
#define JOY_CAL_READVONLY                                            0x08000000

#define JOYSTICKID1                                                  0
#define JOYSTICKID2                                                  1

#define JOYCAPS_HASZ                                                 0x0001
#define JOYCAPS_HASR                                                 0x0002
#define JOYCAPS_HASU                                                 0x0004
#define JOYCAPS_HASV                                                 0x0008
#define JOYCAPS_HASPOV                                               0x0010
#define JOYCAPS_POV4DIR                                              0x0020
#define JOYCAPS_POVCTS                                               0x0040

// #define joyGetDevCaps                                                __MINGW_NAME_AW(joyGetDevCaps)

#endif

#ifndef MMNOMMIO

#define MMIOERR_BASE                                                 256
#define MMIOERR_FILENOTFOUND                                         (MMIOERR_BASE+1)
#define MMIOERR_OUTOFMEMORY                                          (MMIOERR_BASE+2)
#define MMIOERR_CANNOTOPEN                                           (MMIOERR_BASE+3)
#define MMIOERR_CANNOTCLOSE                                          (MMIOERR_BASE+4)
#define MMIOERR_CANNOTREAD                                           (MMIOERR_BASE+5)
#define MMIOERR_CANNOTWRITE                                          (MMIOERR_BASE+6)
#define MMIOERR_CANNOTSEEK                                           (MMIOERR_BASE+7)
#define MMIOERR_CANNOTEXPAND                                         (MMIOERR_BASE+8)
#define MMIOERR_CHUNKNOTFOUND                                        (MMIOERR_BASE+9)
#define MMIOERR_UNBUFFERED                                           (MMIOERR_BASE+10)
#define MMIOERR_PATHNOTFOUND                                         (MMIOERR_BASE+11)
#define MMIOERR_ACCESSDENIED                                         (MMIOERR_BASE+12)
#define MMIOERR_SHARINGVIOLATION                                     (MMIOERR_BASE+13)
#define MMIOERR_NETWORKERROR                                         (MMIOERR_BASE+14)
#define MMIOERR_TOOMANYOPENFILES                                     (MMIOERR_BASE+15)
#define MMIOERR_INVALIDFILE                                          (MMIOERR_BASE+16)

#define CFSEPCHAR                                                    '+'

#define MMIO_RWMODE                                                  0x00000003
#define MMIO_SHAREMODE                                               0x00000070

#define MMIO_CREATE                                                  0x00001000
#define MMIO_PARSE                                                   0x00000100
#define MMIO_DELETE                                                  0x00000200
#define MMIO_EXIST                                                   0x00004000
#define MMIO_ALLOCBUF                                                0x00010000
#define MMIO_GETTEMP                                                 0x00020000

#define MMIO_DIRTY                                                   0x10000000

#define MMIO_READ                                                    0x00000000
#define MMIO_WRITE                                                   0x00000001
#define MMIO_READWRITE                                               0x00000002

#define MMIO_COMPAT                                                  0x00000000
#define MMIO_EXCLUSIVE                                               0x00000010
#define MMIO_DENYWRITE                                               0x00000020
#define MMIO_DENYREAD                                                0x00000030
#define MMIO_DENYNONE                                                0x00000040

#define MMIO_FHOPEN                                                  0x0010
#define MMIO_EMPTYBUF                                                0x0010
#define MMIO_TOUPPER                                                 0x0010
#define MMIO_INSTALLPROC                                             0x00010000
#define MMIO_GLOBALPROC                                              0x10000000
#define MMIO_REMOVEPROC                                              0x00020000
#define MMIO_UNICODEPROC                                             0x01000000
#define MMIO_FINDPROC                                                0x00040000
#define MMIO_FINDCHUNK                                               0x0010
#define MMIO_FINDRIFF                                                0x0020
#define MMIO_FINDLIST                                                0x0040
#define MMIO_CREATERIFF                                              0x0020
#define MMIO_CREATELIST                                              0x0040

#define MMIOM_READ                                                   MMIO_READ
#define MMIOM_WRITE                                                  MMIO_WRITE
#define MMIOM_SEEK                                                   2
#define MMIOM_OPEN                                                   3
#define MMIOM_CLOSE                                                  4
#define MMIOM_WRITEFLUSH                                             5
#define MMIOM_RENAME                                                 6

#define MMIOM_USER                                                   0x8000

// #define FOURCC_RIFF                                                  mmioFOURCC('R','I','F','F')
// #define FOURCC_LIST                                                  mmioFOURCC('L','I','S','T')

// #define FOURCC_DOS                                                   mmioFOURCC('D','O','S','')
// #define FOURCC_MEM                                                   mmioFOURCC('M','E','M','')

#ifndef SEEK_SET
#define SEEK_SET                                                     0
#define SEEK_CUR                                                     1
#define SEEK_END                                                     2
#endif

#define MMIO_DEFAULTBUFFER                                           8192

// #define mmioFOURCC(ch0,ch1,ch2,ch3)                                  MAKEFOURCC(ch0,ch1,ch2,ch3)

// #define mmioStringToFOURCC                                           __MINGW_NAME_AW(mmioStringToFOURCC)
// #define mmioInstallIOProc                                            __MINGW_NAME_AW(mmioInstallIOProc)
// #define mmioOpen                                                     __MINGW_NAME_AW(mmioOpen)
// #define mmioRename                                                   __MINGW_NAME_AW(mmioRename)

#endif

#ifndef MMNOMCI

// #ifndef _MCIERROR_
// #define _MCIERROR_
// #endif

// #ifndef _MCIDEVICEID_
// #define _MCIDEVICEID_
// #endif

// #define mciSendCommand                                               __MINGW_NAME_AW(mciSendCommand)
// #define mciSendString                                                __MINGW_NAME_AW(mciSendString)
// #define mciGetDeviceID                                               __MINGW_NAME_AW(mciGetDeviceID)
// #define mciGetDeviceIDFromElementID                                  __MINGW_NAME_AW(mciGetDeviceIDFromElementID)
// #define mciGetErrorString                                            __MINGW_NAME_AW(mciGetErrorString)

#define MCIERR_INVALID_DEVICE_ID                                     (MCIERR_BASE+1)
#define MCIERR_UNRECOGNIZED_KEYWORD                                  (MCIERR_BASE+3)
#define MCIERR_UNRECOGNIZED_COMMAND                                  (MCIERR_BASE+5)
#define MCIERR_HARDWARE                                              (MCIERR_BASE+6)
#define MCIERR_INVALID_DEVICE_NAME                                   (MCIERR_BASE+7)
#define MCIERR_OUT_OF_MEMORY                                         (MCIERR_BASE+8)
#define MCIERR_DEVICE_OPEN                                           (MCIERR_BASE+9)
#define MCIERR_CANNOT_LOAD_DRIVER                                    (MCIERR_BASE+10)
#define MCIERR_MISSING_COMMAND_STRING                                (MCIERR_BASE+11)
#define MCIERR_PARAM_OVERFLOW                                        (MCIERR_BASE+12)
#define MCIERR_MISSING_STRING_ARGUMENT                               (MCIERR_BASE+13)
#define MCIERR_BAD_INTEGER                                           (MCIERR_BASE+14)
#define MCIERR_PARSER_INTERNAL                                       (MCIERR_BASE+15)
#define MCIERR_DRIVER_INTERNAL                                       (MCIERR_BASE+16)
#define MCIERR_MISSING_PARAMETER                                     (MCIERR_BASE+17)
#define MCIERR_UNSUPPORTED_FUNCTION                                  (MCIERR_BASE+18)
#define MCIERR_FILE_NOT_FOUND                                        (MCIERR_BASE+19)
#define MCIERR_DEVICE_NOT_READY                                      (MCIERR_BASE+20)
#define MCIERR_INTERNAL                                              (MCIERR_BASE+21)
#define MCIERR_DRIVER                                                (MCIERR_BASE+22)
#define MCIERR_CANNOT_USE_ALL                                        (MCIERR_BASE+23)
#define MCIERR_MULTIPLE                                              (MCIERR_BASE+24)
#define MCIERR_EXTENSION_NOT_FOUND                                   (MCIERR_BASE+25)
#define MCIERR_OUTOFRANGE                                            (MCIERR_BASE+26)
#define MCIERR_FLAGS_NOT_COMPATIBLE                                  (MCIERR_BASE+28)
#define MCIERR_FILE_NOT_SAVED                                        (MCIERR_BASE+30)
#define MCIERR_DEVICE_TYPE_REQUIRED                                  (MCIERR_BASE+31)
#define MCIERR_DEVICE_LOCKED                                         (MCIERR_BASE+32)
#define MCIERR_DUPLICATE_ALIAS                                       (MCIERR_BASE+33)
#define MCIERR_BAD_CONSTANT                                          (MCIERR_BASE+34)
#define MCIERR_MUST_USE_SHAREABLE                                    (MCIERR_BASE+35)
#define MCIERR_MISSING_DEVICE_NAME                                   (MCIERR_BASE+36)
#define MCIERR_BAD_TIME_FORMAT                                       (MCIERR_BASE+37)
#define MCIERR_NO_CLOSING_QUOTE                                      (MCIERR_BASE+38)
#define MCIERR_DUPLICATE_FLAGS                                       (MCIERR_BASE+39)
#define MCIERR_INVALID_FILE                                          (MCIERR_BASE+40)
#define MCIERR_NULL_PARAMETER_BLOCK                                  (MCIERR_BASE+41)
#define MCIERR_UNNAMED_RESOURCE                                      (MCIERR_BASE+42)
#define MCIERR_NEW_REQUIRES_ALIAS                                    (MCIERR_BASE+43)
#define MCIERR_NOTIFY_ON_AUTO_OPEN                                   (MCIERR_BASE+44)
#define MCIERR_NO_ELEMENT_ALLOWED                                    (MCIERR_BASE+45)
#define MCIERR_NONAPPLICABLE_FUNCTION                                (MCIERR_BASE+46)
#define MCIERR_ILLEGAL_FOR_AUTO_OPEN                                 (MCIERR_BASE+47)
#define MCIERR_FILENAME_REQUIRED                                     (MCIERR_BASE+48)
#define MCIERR_EXTRA_CHARACTERS                                      (MCIERR_BASE+49)
#define MCIERR_DEVICE_NOT_INSTALLED                                  (MCIERR_BASE+50)
#define MCIERR_GET_CD                                                (MCIERR_BASE+51)
#define MCIERR_SET_CD                                                (MCIERR_BASE+52)
#define MCIERR_SET_DRIVE                                             (MCIERR_BASE+53)
#define MCIERR_DEVICE_LENGTH                                         (MCIERR_BASE+54)
#define MCIERR_DEVICE_ORD_LENGTH                                     (MCIERR_BASE+55)
#define MCIERR_NO_INTEGER                                            (MCIERR_BASE+56)
#define MCIERR_WAVE_OUTPUTSINUSE                                     (MCIERR_BASE+64)
#define MCIERR_WAVE_SETOUTPUTINUSE                                   (MCIERR_BASE+65)
#define MCIERR_WAVE_INPUTSINUSE                                      (MCIERR_BASE+66)
#define MCIERR_WAVE_SETINPUTINUSE                                    (MCIERR_BASE+67)
#define MCIERR_WAVE_OUTPUTUNSPECIFIED                                (MCIERR_BASE+68)
#define MCIERR_WAVE_INPUTUNSPECIFIED                                 (MCIERR_BASE+69)
#define MCIERR_WAVE_OUTPUTSUNSUITABLE                                (MCIERR_BASE+70)
#define MCIERR_WAVE_SETOUTPUTUNSUITABLE                              (MCIERR_BASE+71)
#define MCIERR_WAVE_INPUTSUNSUITABLE                                 (MCIERR_BASE+72)
#define MCIERR_WAVE_SETINPUTUNSUITABLE                               (MCIERR_BASE+73)
#define MCIERR_SEQ_DIV_INCOMPATIBLE                                  (MCIERR_BASE+80)
#define MCIERR_SEQ_PORT_INUSE                                        (MCIERR_BASE+81)
#define MCIERR_SEQ_PORT_NONEXISTENT                                  (MCIERR_BASE+82)
#define MCIERR_SEQ_PORT_MAPNODEVICE                                  (MCIERR_BASE+83)
#define MCIERR_SEQ_PORT_MISCERROR                                    (MCIERR_BASE+84)
#define MCIERR_SEQ_TIMER                                             (MCIERR_BASE+85)
#define MCIERR_SEQ_PORTUNSPECIFIED                                   (MCIERR_BASE+86)
#define MCIERR_SEQ_NOMIDIPRESENT                                     (MCIERR_BASE+87)
#define MCIERR_NO_WINDOW                                             (MCIERR_BASE+90)
#define MCIERR_CREATEWINDOW                                          (MCIERR_BASE+91)
#define MCIERR_FILE_READ                                             (MCIERR_BASE+92)
#define MCIERR_FILE_WRITE                                            (MCIERR_BASE+93)
#define MCIERR_NO_IDENTITY                                           (MCIERR_BASE+94)
#define MCIERR_CUSTOM_DRIVER_BASE                                    (MCIERR_BASE+256)

#define MCI_FIRST                                                    DRV_MCI_FIRST

#define MCI_OPEN                                                     0x0803
#define MCI_CLOSE                                                    0x0804
#define MCI_ESCAPE                                                   0x0805
#define MCI_PLAY                                                     0x0806
#define MCI_SEEK                                                     0x0807
#define MCI_STOP                                                     0x0808
#define MCI_PAUSE                                                    0x0809
#define MCI_INFO                                                     0x080A
#define MCI_GETDEVCAPS                                               0x080B
#define MCI_SPIN                                                     0x080C
#define MCI_SET                                                      0x080D
#define MCI_STEP                                                     0x080E
#define MCI_RECORD                                                   0x080F
#define MCI_SYSINFO                                                  0x0810
#define MCI_BREAK                                                    0x0811
#define MCI_SAVE                                                     0x0813
#define MCI_STATUS                                                   0x0814
#define MCI_CUE                                                      0x0830
#define MCI_REALIZE                                                  0x0840
#define MCI_WINDOW                                                   0x0841
#define MCI_PUT                                                      0x0842
#define MCI_WHERE                                                    0x0843
#define MCI_FREEZE                                                   0x0844
#define MCI_UNFREEZE                                                 0x0845
#define MCI_LOAD                                                     0x0850
#define MCI_CUT                                                      0x0851
#define MCI_COPY                                                     0x0852
#define MCI_PASTE                                                    0x0853
#define MCI_UPDATE                                                   0x0854
#define MCI_RESUME                                                   0x0855
#define MCI_DELETE                                                   0x0856

#define MCI_USER_MESSAGES                                            (DRV_MCI_FIRST+0x400)
#define MCI_LAST                                                     0x0FFF

#define MCI_ALL_DEVICE_ID                                            ((MCIDEVICEID)-1)

#define MCI_DEVTYPE_VCR                                              513
#define MCI_DEVTYPE_VIDEODISC                                        514
#define MCI_DEVTYPE_OVERLAY                                          515
#define MCI_DEVTYPE_CD_AUDIO                                         516
#define MCI_DEVTYPE_DAT                                              517
#define MCI_DEVTYPE_SCANNER                                          518
#define MCI_DEVTYPE_ANIMATION                                        519
#define MCI_DEVTYPE_DIGITAL_VIDEO                                    520
#define MCI_DEVTYPE_OTHER                                            521
#define MCI_DEVTYPE_WAVEFORM_AUDIO                                   522
#define MCI_DEVTYPE_SEQUENCER                                        523

#define MCI_DEVTYPE_FIRST                                            MCI_DEVTYPE_VCR
#define MCI_DEVTYPE_LAST                                             MCI_DEVTYPE_SEQUENCER

#define MCI_DEVTYPE_FIRST_USER                                       0x1000

#define MCI_MODE_NOT_READY                                           (MCI_STRING_OFFSET+12)
#define MCI_MODE_STOP                                                (MCI_STRING_OFFSET+13)
#define MCI_MODE_PLAY                                                (MCI_STRING_OFFSET+14)
#define MCI_MODE_RECORD                                              (MCI_STRING_OFFSET+15)
#define MCI_MODE_SEEK                                                (MCI_STRING_OFFSET+16)
#define MCI_MODE_PAUSE                                               (MCI_STRING_OFFSET+17)
#define MCI_MODE_OPEN                                                (MCI_STRING_OFFSET+18)

#define MCI_FORMAT_MILLISECONDS                                      0
#define MCI_FORMAT_HMS                                               1
#define MCI_FORMAT_MSF                                               2
#define MCI_FORMAT_FRAMES                                            3
#define MCI_FORMAT_SMPTE_24                                          4
#define MCI_FORMAT_SMPTE_25                                          5
#define MCI_FORMAT_SMPTE_30                                          6
#define MCI_FORMAT_SMPTE_30DROP                                      7
#define MCI_FORMAT_BYTES                                             8
#define MCI_FORMAT_SAMPLES                                           9
#define MCI_FORMAT_TMSF                                              10

// #define MCI_MSF_MINUTE(msf)                                          ((BYTE)(msf))
// #define MCI_MSF_SECOND(msf)                                          ((BYTE)(((WORD)(msf))>>8))
// #define MCI_MSF_FRAME(msf)                                           ((BYTE)((msf)>>16))

// #define MCI_MAKE_MSF(m,s,f)                                          ((DWORD)(((BYTE)(m)|((WORD)(s)<<8))|(((DWORD)(BYTE)(f))<<16)))

// #define MCI_TMSF_TRACK(tmsf)                                         ((BYTE)(tmsf))
// #define MCI_TMSF_MINUTE(tmsf)                                        ((BYTE)(((WORD)(tmsf))>>8))
// #define MCI_TMSF_SECOND(tmsf)                                        ((BYTE)((tmsf)>>16))
// #define MCI_TMSF_FRAME(tmsf)                                         ((BYTE)((tmsf)>>24))

// #define MCI_MAKE_TMSF(t,m,s,f)                                       ((DWORD)(((BYTE)(t)|((WORD)(m)<<8))|(((DWORD)(BYTE)(s)|((WORD)(f)<<8))<<16)))

// #define MCI_HMS_HOUR(hms)                                            ((BYTE)(hms))
// #define MCI_HMS_MINUTE(hms)                                          ((BYTE)(((WORD)(hms))>>8))
// #define MCI_HMS_SECOND(hms)                                          ((BYTE)((hms)>>16))

// #define MCI_MAKE_HMS(h,m,s)                                          ((DWORD)(((BYTE)(h)|((WORD)(m)<<8))|(((DWORD)(BYTE)(s))<<16)))

#define MCI_NOTIFY_SUCCESSFUL                                        0x0001
#define MCI_NOTIFY_SUPERSEDED                                        0x0002
#define MCI_NOTIFY_ABORTED                                           0x0004
#define MCI_NOTIFY_FAILURE                                           0x0008

#define MCI_NOTIFY                                                   0x00000001
#define MCI_WAIT                                                     0x00000002
#define MCI_FROM                                                     0x00000004
#define MCI_TO                                                       0x00000008
#define MCI_TRACK                                                    0x00000010

#define MCI_OPEN_SHAREABLE                                           0x00000100
#define MCI_OPEN_ELEMENT                                             0x00000200
#define MCI_OPEN_ALIAS                                               0x00000400
#define MCI_OPEN_ELEMENT_ID                                          0x00000800
#define MCI_OPEN_TYPE_ID                                             0x00001000
#define MCI_OPEN_TYPE                                                0x00002000

#define MCI_SEEK_TO_START                                            0x00000100
#define MCI_SEEK_TO_END                                              0x00000200

#define MCI_STATUS_ITEM                                              0x00000100
#define MCI_STATUS_START                                             0x00000200

#define MCI_STATUS_LENGTH                                            0x00000001
#define MCI_STATUS_POSITION                                          0x00000002
#define MCI_STATUS_NUMBER_OF_TRACKS                                  0x00000003
#define MCI_STATUS_MODE                                              0x00000004
#define MCI_STATUS_MEDIA_PRESENT                                     0x00000005
#define MCI_STATUS_TIME_FORMAT                                       0x00000006
#define MCI_STATUS_READY                                             0x00000007
#define MCI_STATUS_CURRENT_TRACK                                     0x00000008

#define MCI_INFO_PRODUCT                                             0x00000100
#define MCI_INFO_FILE                                                0x00000200
#define MCI_INFO_MEDIA_UPC                                           0x00000400
#define MCI_INFO_MEDIA_IDENTITY                                      0x00000800
#define MCI_INFO_NAME                                                0x00001000
#define MCI_INFO_COPYRIGHT                                           0x00002000

#define MCI_GETDEVCAPS_ITEM                                          0x00000100

#define MCI_GETDEVCAPS_CAN_RECORD                                    0x00000001
#define MCI_GETDEVCAPS_HAS_AUDIO                                     0x00000002
#define MCI_GETDEVCAPS_HAS_VIDEO                                     0x00000003
#define MCI_GETDEVCAPS_DEVICE_TYPE                                   0x00000004
#define MCI_GETDEVCAPS_USES_FILES                                    0x00000005
#define MCI_GETDEVCAPS_COMPOUND_DEVICE                               0x00000006
#define MCI_GETDEVCAPS_CAN_EJECT                                     0x00000007
#define MCI_GETDEVCAPS_CAN_PLAY                                      0x00000008
#define MCI_GETDEVCAPS_CAN_SAVE                                      0x00000009

#define MCI_SYSINFO_QUANTITY                                         0x00000100
#define MCI_SYSINFO_OPEN                                             0x00000200
#define MCI_SYSINFO_NAME                                             0x00000400
#define MCI_SYSINFO_INSTALLNAME                                      0x00000800

#define MCI_SET_DOOR_OPEN                                            0x00000100
#define MCI_SET_DOOR_CLOSED                                          0x00000200
#define MCI_SET_TIME_FORMAT                                          0x00000400
#define MCI_SET_AUDIO                                                0x00000800
#define MCI_SET_VIDEO                                                0x00001000
#define MCI_SET_ON                                                   0x00002000
#define MCI_SET_OFF                                                  0x00004000

#define MCI_SET_AUDIO_ALL                                            0x00000000
#define MCI_SET_AUDIO_LEFT                                           0x00000001
#define MCI_SET_AUDIO_RIGHT                                          0x00000002

#define MCI_BREAK_KEY                                                0x00000100
#define MCI_BREAK_HWND                                               0x00000200
#define MCI_BREAK_OFF                                                0x00000400

#define MCI_RECORD_INSERT                                            0x00000100
#define MCI_RECORD_OVERWRITE                                         0x00000200

#define MCI_SAVE_FILE                                                0x00000100

#define MCI_LOAD_FILE                                                0x00000100

#define MCI_VD_MODE_PARK                                             (MCI_VD_OFFSET+1)

#define MCI_VD_MEDIA_CLV                                             (MCI_VD_OFFSET+2)
#define MCI_VD_MEDIA_CAV                                             (MCI_VD_OFFSET+3)
#define MCI_VD_MEDIA_OTHER                                           (MCI_VD_OFFSET+4)

#define MCI_VD_FORMAT_TRACK                                          0x4001

#define MCI_VD_PLAY_REVERSE                                          0x00010000
#define MCI_VD_PLAY_FAST                                             0x00020000
#define MCI_VD_PLAY_SPEED                                            0x00040000
#define MCI_VD_PLAY_SCAN                                             0x00080000
#define MCI_VD_PLAY_SLOW                                             0x00100000

#define MCI_VD_SEEK_REVERSE                                          0x00010000

#define MCI_VD_STATUS_SPEED                                          0x00004002
#define MCI_VD_STATUS_FORWARD                                        0x00004003
#define MCI_VD_STATUS_MEDIA_TYPE                                     0x00004004
#define MCI_VD_STATUS_SIDE                                           0x00004005
#define MCI_VD_STATUS_DISC_SIZE                                      0x00004006

#define MCI_VD_GETDEVCAPS_CLV                                        0x00010000
#define MCI_VD_GETDEVCAPS_CAV                                        0x00020000

#define MCI_VD_SPIN_UP                                               0x00010000
#define MCI_VD_SPIN_DOWN                                             0x00020000

#define MCI_VD_GETDEVCAPS_CAN_REVERSE                                0x00004002
#define MCI_VD_GETDEVCAPS_FAST_RATE                                  0x00004003
#define MCI_VD_GETDEVCAPS_SLOW_RATE                                  0x00004004
#define MCI_VD_GETDEVCAPS_NORMAL_RATE                                0x00004005

#define MCI_VD_STEP_FRAMES                                           0x00010000
#define MCI_VD_STEP_REVERSE                                          0x00020000

#define MCI_VD_ESCAPE_STRING                                         0x00000100

#define MCI_CDA_STATUS_TYPE_TRACK                                    0x00004001
#define MCI_CDA_TRACK_AUDIO                                          (MCI_CD_OFFSET+0)
#define MCI_CDA_TRACK_OTHER                                          (MCI_CD_OFFSET+1)

#define MCI_WAVE_PCM                                                 (MCI_WAVE_OFFSET+0)
#define MCI_WAVE_MAPPER                                              (MCI_WAVE_OFFSET+1)

#define MCI_WAVE_OPEN_BUFFER                                         0x00010000

#define MCI_WAVE_SET_FORMATTAG                                       0x00010000
#define MCI_WAVE_SET_CHANNELS                                        0x00020000
#define MCI_WAVE_SET_SAMPLESPERSEC                                   0x00040000
#define MCI_WAVE_SET_AVGBYTESPERSEC                                  0x00080000
#define MCI_WAVE_SET_BLOCKALIGN                                      0x00100000
#define MCI_WAVE_SET_BITSPERSAMPLE                                   0x00200000

#define MCI_WAVE_INPUT                                               0x00400000
#define MCI_WAVE_OUTPUT                                              0x00800000

#define MCI_WAVE_STATUS_FORMATTAG                                    0x00004001
#define MCI_WAVE_STATUS_CHANNELS                                     0x00004002
#define MCI_WAVE_STATUS_SAMPLESPERSEC                                0x00004003
#define MCI_WAVE_STATUS_AVGBYTESPERSEC                               0x00004004
#define MCI_WAVE_STATUS_BLOCKALIGN                                   0x00004005
#define MCI_WAVE_STATUS_BITSPERSAMPLE                                0x00004006
#define MCI_WAVE_STATUS_LEVEL                                        0x00004007

#define MCI_WAVE_SET_ANYINPUT                                        0x04000000
#define MCI_WAVE_SET_ANYOUTPUT                                       0x08000000

#define MCI_WAVE_GETDEVCAPS_INPUTS                                   0x00004001
#define MCI_WAVE_GETDEVCAPS_OUTPUTS                                  0x00004002

#define MCI_SEQ_DIV_PPQN                                             (0+MCI_SEQ_OFFSET)
#define MCI_SEQ_DIV_SMPTE_24                                         (1+MCI_SEQ_OFFSET)
#define MCI_SEQ_DIV_SMPTE_25                                         (2+MCI_SEQ_OFFSET)
#define MCI_SEQ_DIV_SMPTE_30DROP                                     (3+MCI_SEQ_OFFSET)
#define MCI_SEQ_DIV_SMPTE_30                                         (4+MCI_SEQ_OFFSET)

#define MCI_SEQ_FORMAT_SONGPTR                                       0x4001
#define MCI_SEQ_FILE                                                 0x4002
#define MCI_SEQ_MIDI                                                 0x4003
#define MCI_SEQ_SMPTE                                                0x4004
#define MCI_SEQ_NONE                                                 65533
#define MCI_SEQ_MAPPER                                               65535

#define MCI_SEQ_STATUS_TEMPO                                         0x00004002
#define MCI_SEQ_STATUS_PORT                                          0x00004003
#define MCI_SEQ_STATUS_SLAVE                                         0x00004007
#define MCI_SEQ_STATUS_MASTER                                        0x00004008
#define MCI_SEQ_STATUS_OFFSET                                        0x00004009
#define MCI_SEQ_STATUS_DIVTYPE                                       0x0000400A
#define MCI_SEQ_STATUS_NAME                                          0x0000400B
#define MCI_SEQ_STATUS_COPYRIGHT                                     0x0000400C

#define MCI_SEQ_SET_TEMPO                                            0x00010000
#define MCI_SEQ_SET_PORT                                             0x00020000
#define MCI_SEQ_SET_SLAVE                                            0x00040000
#define MCI_SEQ_SET_MASTER                                           0x00080000
#define MCI_SEQ_SET_OFFSET                                           0x01000000

#define MCI_ANIM_OPEN_WS                                             0x00010000
#define MCI_ANIM_OPEN_PARENT                                         0x00020000
#define MCI_ANIM_OPEN_NOSTATIC                                       0x00040000

#define MCI_ANIM_PLAY_SPEED                                          0x00010000
#define MCI_ANIM_PLAY_REVERSE                                        0x00020000
#define MCI_ANIM_PLAY_FAST                                           0x00040000
#define MCI_ANIM_PLAY_SLOW                                           0x00080000
#define MCI_ANIM_PLAY_SCAN                                           0x00100000

#define MCI_ANIM_STEP_REVERSE                                        0x00010000
#define MCI_ANIM_STEP_FRAMES                                         0x00020000

#define MCI_ANIM_STATUS_SPEED                                        0x00004001
#define MCI_ANIM_STATUS_FORWARD                                      0x00004002
#define MCI_ANIM_STATUS_HWND                                         0x00004003
#define MCI_ANIM_STATUS_HPAL                                         0x00004004
#define MCI_ANIM_STATUS_STRETCH                                      0x00004005

#define MCI_ANIM_INFO_TEXT                                           0x00010000

#define MCI_ANIM_GETDEVCAPS_CAN_REVERSE                              0x00004001
#define MCI_ANIM_GETDEVCAPS_FAST_RATE                                0x00004002
#define MCI_ANIM_GETDEVCAPS_SLOW_RATE                                0x00004003
#define MCI_ANIM_GETDEVCAPS_NORMAL_RATE                              0x00004004
#define MCI_ANIM_GETDEVCAPS_PALETTES                                 0x00004006
#define MCI_ANIM_GETDEVCAPS_CAN_STRETCH                              0x00004007
#define MCI_ANIM_GETDEVCAPS_MAX_WINDOWS                              0x00004008

#define MCI_ANIM_REALIZE_NORM                                        0x00010000
#define MCI_ANIM_REALIZE_BKGD                                        0x00020000

#define MCI_ANIM_WINDOW_HWND                                         0x00010000
#define MCI_ANIM_WINDOW_STATE                                        0x00040000
#define MCI_ANIM_WINDOW_TEXT                                         0x00080000
#define MCI_ANIM_WINDOW_ENABLE_STRETCH                               0x00100000
#define MCI_ANIM_WINDOW_DISABLE_STRETCH                              0x00200000

#define MCI_ANIM_WINDOW_DEFAULT                                      0x00000000

#define MCI_ANIM_RECT                                                0x00010000
#define MCI_ANIM_PUT_SOURCE                                          0x00020000
#define MCI_ANIM_PUT_DESTINATION                                     0x00040000

#define MCI_ANIM_WHERE_SOURCE                                        0x00020000
#define MCI_ANIM_WHERE_DESTINATION                                   0x00040000

#define MCI_ANIM_UPDATE_HDC                                          0x00020000

// #ifdef MCI_USE_OFFEXT
// #else
// #endif

#define MCI_OVLY_OPEN_WS                                             0x00010000
#define MCI_OVLY_OPEN_PARENT                                         0x00020000

#define MCI_OVLY_STATUS_HWND                                         0x00004001
#define MCI_OVLY_STATUS_STRETCH                                      0x00004002

#define MCI_OVLY_INFO_TEXT                                           0x00010000

#define MCI_OVLY_GETDEVCAPS_CAN_STRETCH                              0x00004001
#define MCI_OVLY_GETDEVCAPS_CAN_FREEZE                               0x00004002
#define MCI_OVLY_GETDEVCAPS_MAX_WINDOWS                              0x00004003

#define MCI_OVLY_WINDOW_HWND                                         0x00010000
#define MCI_OVLY_WINDOW_STATE                                        0x00040000
#define MCI_OVLY_WINDOW_TEXT                                         0x00080000
#define MCI_OVLY_WINDOW_ENABLE_STRETCH                               0x00100000
#define MCI_OVLY_WINDOW_DISABLE_STRETCH                              0x00200000

#define MCI_OVLY_WINDOW_DEFAULT                                      0x00000000

#define MCI_OVLY_RECT                                                0x00010000
#define MCI_OVLY_PUT_SOURCE                                          0x00020000
#define MCI_OVLY_PUT_DESTINATION                                     0x00040000
#define MCI_OVLY_PUT_FRAME                                           0x00080000
#define MCI_OVLY_PUT_VIDEO                                           0x00100000

#define MCI_OVLY_WHERE_SOURCE                                        0x00020000
#define MCI_OVLY_WHERE_DESTINATION                                   0x00040000
#define MCI_OVLY_WHERE_FRAME                                         0x00080000
#define MCI_OVLY_WHERE_VIDEO                                         0x00100000

// #ifdef MCI_USE_OFFEXT
// #else
// #endif

#endif

#ifndef NEWTRANSPARENT
#define NEWTRANSPARENT                                               3
#define QUERYROPSUPPORT                                              40
#endif

#define SELECTDIB                                                    41
// #define DIBINDEX(n)                                                  MAKELONG((n),0x10FF)

// #ifndef SC_SCREENSAVE
// #define SC_SCREENSAVE                                                0xF140
// #endif

#endif /* _WINAPI_MMSYSTEM_ */
