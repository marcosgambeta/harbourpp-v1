/*

  WINAPI For Harbour++ - Bindings libraries for Harbour++ and WINAPI

  Copyright (C) 2022 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

*/

/*
  NOTE: source code generated with the help of a code generator
*/

#ifndef _WINAPI_WINGDI_
#define _WINAPI_WINGDI_

// #ifdef _GDI32_
// #define WINGDIAPI
// #else
// #define WINGDIAPI                                                    DECLSPEC_IMPORT
// #endif

// #ifdef _SPOOL32_
// #define WINSPOOLAPI
// #else
// #define WINSPOOLAPI                                                  DECLSPEC_IMPORT
// #endif

// #ifdef __cplusplus
// #endif

// #ifndef WINVER
// #define WINVER                                                       0x0502
// #endif

#ifndef NOGDI

#ifndef NORASTEROPS

#define R2_BLACK                                                     1
#define R2_NOTMERGEPEN                                               2
#define R2_MASKNOTPEN                                                3
#define R2_NOTCOPYPEN                                                4
#define R2_MASKPENNOT                                                5
#define R2_NOT                                                       6
#define R2_XORPEN                                                    7
#define R2_NOTMASKPEN                                                8
#define R2_MASKPEN                                                   9
#define R2_NOTXORPEN                                                 10
#define R2_NOP                                                       11
#define R2_MERGENOTPEN                                               12
#define R2_COPYPEN                                                   13
#define R2_MERGEPENNOT                                               14
#define R2_MERGEPEN                                                  15
#define R2_WHITE                                                     16
#define R2_LAST                                                      16

#define SRCCOPY                                                      0x00CC0020
#define SRCPAINT                                                     0x00EE0086
#define SRCAND                                                       0x008800C6
#define SRCINVERT                                                    0x00660046
#define SRCERASE                                                     0x00440328
#define NOTSRCCOPY                                                   0x00330008
#define NOTSRCERASE                                                  0x001100A6
#define MERGECOPY                                                    0x00C000CA
#define MERGEPAINT                                                   0x00BB0226
#define PATCOPY                                                      0x00F00021
#define PATPAINT                                                     0x00FB0A09
#define PATINVERT                                                    0x005A0049
#define DSTINVERT                                                    0x00550009
#define BLACKNESS                                                    0x00000042
#define WHITENESS                                                    0x00FF0062
#define NOMIRRORBITMAP                                               0x80000000
#define CAPTUREBLT                                                   0x40000000
//#define MAKEROP4(fore,back)                                          (DWORD)((((back)<<8)&0xFF000000)|(fore))

#endif

#define GDI_ERROR                                                    0xFFFFFFFF
#define HGDI_ERROR                                                   0xFFFFFFFF

#define ERROR                                                        0
#define NULLREGION                                                   1
#define SIMPLEREGION                                                 2
#define COMPLEXREGION                                                3
#define RGN_ERROR                                                    ERROR

#define RGN_AND                                                      1
#define RGN_OR                                                       2
#define RGN_XOR                                                      3
#define RGN_DIFF                                                     4
#define RGN_COPY                                                     5
#define RGN_MIN                                                      RGN_AND
#define RGN_MAX                                                      RGN_COPY

#define BLACKONWHITE                                                 1
#define WHITEONBLACK                                                 2
#define COLORONCOLOR                                                 3
#define HALFTONE                                                     4
#define MAXSTRETCHBLTMODE                                            4

#define STRETCH_ANDSCANS                                             BLACKONWHITE
#define STRETCH_ORSCANS                                              WHITEONBLACK
#define STRETCH_DELETESCANS                                          COLORONCOLOR
#define STRETCH_HALFTONE                                             HALFTONE

#define ALTERNATE                                                    1
#define WINDING                                                      2
#define POLYFILL_LAST                                                2

#define LAYOUT_RTL                                                   0x00000001
#define LAYOUT_BTT                                                   0x00000002
#define LAYOUT_VBH                                                   0x00000004
#define LAYOUT_ORIENTATIONMASK                                       hb_bitor(LAYOUT_RTL, LAYOUT_BTT, LAYOUT_VBH)
#define LAYOUT_BITMAPORIENTATIONPRESERVED                            0x00000008

#define TA_NOUPDATECP                                                0
#define TA_UPDATECP                                                  1

#define TA_LEFT                                                      0
#define TA_RIGHT                                                     2
#define TA_CENTER                                                    6

#define TA_TOP                                                       0
#define TA_BOTTOM                                                    8
#define TA_BASELINE                                                  24
#define TA_RTLREADING                                                256
#define TA_MASK                                                      (TA_BASELINE+TA_CENTER+TA_UPDATECP+TA_RTLREADING)

#define VTA_BASELINE                                                 TA_BASELINE
#define VTA_LEFT                                                     TA_BOTTOM
#define VTA_RIGHT                                                    TA_TOP
#define VTA_CENTER                                                   TA_CENTER
#define VTA_BOTTOM                                                   TA_RIGHT
#define VTA_TOP                                                      TA_LEFT

#define ETO_OPAQUE                                                   0x0002
#define ETO_CLIPPED                                                  0x0004
#define ETO_GLYPH_INDEX                                              0x0010
#define ETO_RTLREADING                                               0x0080
#define ETO_NUMERICSLOCAL                                            0x0400
#define ETO_NUMERICSLATIN                                            0x0800
#define ETO_IGNORELANGUAGE                                           0x1000
#define ETO_PDY                                                      0x2000
// #if _WIN32_WINNT >= 0x0600
#define ETO_REVERSE_INDEX_MAP                                        0x10000
// #endif

#define ASPECT_FILTERING                                             0x0001

#define DCB_RESET                                                    0x0001
#define DCB_ACCUMULATE                                               0x0002
#define DCB_DIRTY                                                    DCB_ACCUMULATE
#define DCB_SET                                                      hb_birot(DCB_RESET, DCB_ACCUMULATE)
#define DCB_ENABLE                                                   0x0004
#define DCB_DISABLE                                                  0x0008

#ifndef NOMETAFILE

#define META_SETBKCOLOR                                              0x0201
#define META_SETBKMODE                                               0x0102
#define META_SETMAPMODE                                              0x0103
#define META_SETROP2                                                 0x0104
#define META_SETRELABS                                               0x0105
#define META_SETPOLYFILLMODE                                         0x0106
#define META_SETSTRETCHBLTMODE                                       0x0107
#define META_SETTEXTCHAREXTRA                                        0x0108
#define META_SETTEXTCOLOR                                            0x0209
#define META_SETTEXTJUSTIFICATION                                    0x020A
#define META_SETWINDOWORG                                            0x020B
#define META_SETWINDOWEXT                                            0x020C
#define META_SETVIEWPORTORG                                          0x020D
#define META_SETVIEWPORTEXT                                          0x020E
#define META_OFFSETWINDOWORG                                         0x020F
#define META_SCALEWINDOWEXT                                          0x0410
#define META_OFFSETVIEWPORTORG                                       0x0211
#define META_SCALEVIEWPORTEXT                                        0x0412
#define META_LINETO                                                  0x0213
#define META_MOVETO                                                  0x0214
#define META_EXCLUDECLIPRECT                                         0x0415
#define META_INTERSECTCLIPRECT                                       0x0416
#define META_ARC                                                     0x0817
#define META_ELLIPSE                                                 0x0418
#define META_FLOODFILL                                               0x0419
#define META_PIE                                                     0x081A
#define META_RECTANGLE                                               0x041B
#define META_ROUNDRECT                                               0x061C
#define META_PATBLT                                                  0x061D
#define META_SAVEDC                                                  0x001E
#define META_SETPIXEL                                                0x041F
#define META_OFFSETCLIPRGN                                           0x0220
#define META_TEXTOUT                                                 0x0521
#define META_BITBLT                                                  0x0922
#define META_STRETCHBLT                                              0x0B23
#define META_POLYGON                                                 0x0324
#define META_POLYLINE                                                0x0325
#define META_ESCAPE                                                  0x0626
#define META_RESTOREDC                                               0x0127
#define META_FILLREGION                                              0x0228
#define META_FRAMEREGION                                             0x0429
#define META_INVERTREGION                                            0x012A
#define META_PAINTREGION                                             0x012B
#define META_SELECTCLIPREGION                                        0x012C
#define META_SELECTOBJECT                                            0x012D
#define META_SETTEXTALIGN                                            0x012E
#define META_CHORD                                                   0x0830
#define META_SETMAPPERFLAGS                                          0x0231
#define META_EXTTEXTOUT                                              0x0a32
#define META_SETDIBTODEV                                             0x0d33
#define META_SELECTPALETTE                                           0x0234
#define META_REALIZEPALETTE                                          0x0035
#define META_ANIMATEPALETTE                                          0x0436
#define META_SETPALENTRIES                                           0x0037
#define META_POLYPOLYGON                                             0x0538
#define META_RESIZEPALETTE                                           0x0139
#define META_DIBBITBLT                                               0x0940
#define META_DIBSTRETCHBLT                                           0x0b41
#define META_DIBCREATEPATTERNBRUSH                                   0x0142
#define META_STRETCHDIB                                              0x0f43
#define META_EXTFLOODFILL                                            0x0548
#define META_SETLAYOUT                                               0x0149
#define META_DELETEOBJECT                                            0x01f0
#define META_CREATEPALETTE                                           0x00f7
#define META_CREATEPATTERNBRUSH                                      0x01F9
#define META_CREATEPENINDIRECT                                       0x02FA
#define META_CREATEFONTINDIRECT                                      0x02FB
#define META_CREATEBRUSHINDIRECT                                     0x02FC
#define META_CREATEREGION                                            0x06FF

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

#endif

#define NEWFRAME                                                     1
#define ABORTDOC                                                     2
#define NEXTBAND                                                     3
#define SETCOLORTABLE                                                4
#define GETCOLORTABLE                                                5
#define FLUSHOUTPUT                                                  6
#define DRAFTMODE                                                    7
#define QUERYESCSUPPORT                                              8
#define SETABORTPROC                                                 9
#define STARTDOC                                                     10
#define ENDDOC                                                       11
#define GETPHYSPAGESIZE                                              12
#define GETPRINTINGOFFSET                                            13
#define GETSCALINGFACTOR                                             14
#define MFCOMMENT                                                    15
#define GETPENWIDTH                                                  16
#define SETCOPYCOUNT                                                 17
#define SELECTPAPERSOURCE                                            18
#define DEVICEDATA                                                   19
#define PASSTHROUGH                                                  19
#define GETTECHNOLGY                                                 20
#define GETTECHNOLOGY                                                20
#define SETLINECAP                                                   21
#define SETLINEJOIN                                                  22
#define SETMITERLIMIT                                                23
#define BANDINFO                                                     24
#define DRAWPATTERNRECT                                              25
#define GETVECTORPENSIZE                                             26
#define GETVECTORBRUSHSIZE                                           27
#define ENABLEDUPLEX                                                 28
#define GETSETPAPERBINS                                              29
#define GETSETPRINTORIENT                                            30
#define ENUMPAPERBINS                                                31
#define SETDIBSCALING                                                32
#define EPSPRINTING                                                  33
#define ENUMPAPERMETRICS                                             34
#define GETSETPAPERMETRICS                                           35
#define POSTSCRIPT_DATA                                              37
#define POSTSCRIPT_IGNORE                                            38
#define MOUSETRAILS                                                  39
#define GETDEVICEUNITS                                               42

#define GETEXTENDEDTEXTMETRICS                                       256
#define GETEXTENTTABLE                                               257
#define GETPAIRKERNTABLE                                             258
#define GETTRACKKERNTABLE                                            259
#define EXTTEXTOUT                                                   512
#define GETFACENAME                                                  513
#define DOWNLOADFACE                                                 514
#define ENABLERELATIVEWIDTHS                                         768
#define ENABLEPAIRKERNING                                            769
#define SETKERNTRACK                                                 770
#define SETALLJUSTVALUES                                             771
#define SETCHARSET                                                   772

#define STRETCHBLT                                                   2048
#define METAFILE_DRIVER                                              2049
#define GETSETSCREENPARAMS                                           3072
#define QUERYDIBSUPPORT                                              3073
#define BEGIN_PATH                                                   4096
#define CLIP_TO_PATH                                                 4097
#define END_PATH                                                     4098
#define EXT_DEVICE_CAPS                                              4099
#define RESTORE_CTM                                                  4100
#define SAVE_CTM                                                     4101
#define SET_ARC_DIRECTION                                            4102
#define SET_BACKGROUND_COLOR                                         4103
#define SET_POLY_MODE                                                4104
#define SET_SCREEN_ANGLE                                             4105
#define SET_SPREAD                                                   4106
#define TRANSFORM_CTM                                                4107
#define SET_CLIP_BOX                                                 4108
#define SET_BOUNDS                                                   4109
#define SET_MIRROR_MODE                                              4110
#define OPENCHANNEL                                                  4110
#define DOWNLOADHEADER                                               4111
#define CLOSECHANNEL                                                 4112
#define POSTSCRIPT_PASSTHROUGH                                       4115
#define ENCAPSULATED_POSTSCRIPT                                      4116

#define POSTSCRIPT_IDENTIFY                                          4117
#define POSTSCRIPT_INJECTION                                         4118

#define CHECKJPEGFORMAT                                              4119
#define CHECKPNGFORMAT                                               4120

#define GET_PS_FEATURESETTING                                        4121
#define GDIPLUS_TS_QUERYVER                                          4122
#define GDIPLUS_TS_RECORD                                            4123

// #if _WIN32_WINNT >= 0x0600
#define MILCORE_TS_QUERYVER_RESULT_FALSE                             0x0
#define MILCORE_TS_QUERYVER_RESULT_TRUE                              0x7FFFFFFF
// #endif

#define SPCLPASSTHROUGH2                                             4568

#define PSIDENT_GDICENTRIC                                           0
#define PSIDENT_PSCENTRIC                                            1

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

#define PSINJECT_BEGINSTREAM                                         1
#define PSINJECT_PSADOBE                                             2
#define PSINJECT_PAGESATEND                                          3
#define PSINJECT_PAGES                                               4

#define PSINJECT_DOCNEEDEDRES                                        5
#define PSINJECT_DOCSUPPLIEDRES                                      6
#define PSINJECT_PAGEORDER                                           7
#define PSINJECT_ORIENTATION                                         8
#define PSINJECT_BOUNDINGBOX                                         9
#define PSINJECT_DOCUMENTPROCESSCOLORS                               10

#define PSINJECT_COMMENTS                                            11
#define PSINJECT_BEGINDEFAULTS                                       12
#define PSINJECT_ENDDEFAULTS                                         13
#define PSINJECT_BEGINPROLOG                                         14
#define PSINJECT_ENDPROLOG                                           15
#define PSINJECT_BEGINSETUP                                          16
#define PSINJECT_ENDSETUP                                            17
#define PSINJECT_TRAILER                                             18
#define PSINJECT_EOF                                                 19
#define PSINJECT_ENDSTREAM                                           20
#define PSINJECT_DOCUMENTPROCESSCOLORSATEND                          21

#define PSINJECT_PAGENUMBER                                          100
#define PSINJECT_BEGINPAGESETUP                                      101
#define PSINJECT_ENDPAGESETUP                                        102
#define PSINJECT_PAGETRAILER                                         103
#define PSINJECT_PLATECOLOR                                          104

#define PSINJECT_SHOWPAGE                                            105
#define PSINJECT_PAGEBBOX                                            106
#define PSINJECT_ENDPAGECOMMENTS                                     107

#define PSINJECT_VMSAVE                                              200
#define PSINJECT_VMRESTORE                                           201

#define FEATURESETTING_NUP                                           0
#define FEATURESETTING_OUTPUT                                        1
#define FEATURESETTING_PSLEVEL                                       2
#define FEATURESETTING_CUSTPAPER                                     3
#define FEATURESETTING_MIRROR                                        4
#define FEATURESETTING_NEGATIVE                                      5
#define FEATURESETTING_PROTOCOL                                      6

#define FEATURESETTING_PRIVATE_BEGIN                                 0x1000
#define FEATURESETTING_PRIVATE_END                                   0x1FFF

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

#define PSPROTOCOL_ASCII                                             0
#define PSPROTOCOL_BCP                                               1
#define PSPROTOCOL_TBCP                                              2
#define PSPROTOCOL_BINARY                                            3

#define QDI_SETDIBITS                                                1
#define QDI_GETDIBITS                                                2
#define QDI_DIBTOSCREEN                                              4
#define QDI_STRETCHDIB                                               8

#define SP_NOTREPORTED                                               0x4000
#define SP_ERROR                                                     (-1)
#define SP_APPABORT                                                  (-2)
#define SP_USERABORT                                                 (-3)
#define SP_OUTOFDISK                                                 (-4)
#define SP_OUTOFMEMORY                                               (-5)

#define PR_JOBSTATUS                                                 0x0000

#define OBJ_PEN                                                      1
#define OBJ_BRUSH                                                    2
#define OBJ_DC                                                       3
#define OBJ_METADC                                                   4
#define OBJ_PAL                                                      5
#define OBJ_FONT                                                     6
#define OBJ_BITMAP                                                   7
#define OBJ_REGION                                                   8
#define OBJ_METAFILE                                                 9
#define OBJ_MEMDC                                                    10
#define OBJ_EXTPEN                                                   11
#define OBJ_ENHMETADC                                                12
#define OBJ_ENHMETAFILE                                              13
#define OBJ_COLORSPACE                                               14

#define GDI_OBJ_LAST                                                 OBJ_COLORSPACE

#define MWT_IDENTITY                                                 1
#define MWT_LEFTMULTIPLY                                             2
#define MWT_RIGHTMULTIPLY                                            3

#define MWT_MIN                                                      MWT_IDENTITY
#define MWT_MAX                                                      MWT_RIGHTMULTIPLY

#define _XFORM_
// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

#define CS_ENABLE                                                    0x00000001
#define CS_DISABLE                                                   0x00000002
#define CS_DELETE_TRANSFORM                                          0x00000003

#define LCS_SIGNATURE                                                'PSOC'

#define LCS_sRGB                                                     'sRGB'
#define LCS_WINDOWS_COLOR_SPACE                                      'Win'

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
#define LCS_CALIBRATED_RGB                                           0x00000000

#define LCS_GM_BUSINESS                                              0x00000001
#define LCS_GM_GRAPHICS                                              0x00000002
#define LCS_GM_IMAGES                                                0x00000004
#define LCS_GM_ABS_COLORIMETRIC                                      0x00000008

#define CM_OUT_OF_GAMUT                                              255
#define CM_IN_GAMUT                                                  0

#define ICM_ADDPROFILE                                               1
#define ICM_DELETEPROFILE                                            2
#define ICM_QUERYPROFILE                                             3
#define ICM_SETDEFAULTPROFILE                                        4
#define ICM_REGISTERICMATCHER                                        5
#define ICM_UNREGISTERICMATCHER                                      6
#define ICM_QUERYMATCH                                               7

// #define GetKValue(cmyk)                                              ((BYTE)(cmyk))
// #define GetYValue(cmyk)                                              ((BYTE)((cmyk)>>8))
// #define GetMValue(cmyk)                                              ((BYTE)((cmyk)>>16))
// #define GetCValue(cmyk)                                              ((BYTE)((cmyk)>>24))

// #define CMYK(c,m,y,k)                                                ((COLORREF)((((BYTE)(k)|((WORD)((BYTE)(y))<<8))|(((DWORD)(BYTE)(m))<<16))|(((DWORD)(BYTE)(c))<<24)))

#endif
// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif
// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif
// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

#define PROFILE_LINKED                                               'LINK'
#define PROFILE_EMBEDDED                                             'MBED'

#define BI_RGB                                                       0
#define BI_RLE8                                                      1
#define BI_RLE4                                                      2
#define BI_BITFIELDS                                                 3
#define BI_JPEG                                                      4
#define BI_PNG                                                       5

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

// #define MAKEPOINTS(l)                                                (*((POINTS*)&(l)))

#ifndef NOFONTSIG

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

#define TCI_SRCCHARSET                                               1
#define TCI_SRCCODEPAGE                                              2
#define TCI_SRCFONTSIG                                               3
#define TCI_SRCLOCALE                                                0x1000

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif
#endif

#ifndef NOMETAFILE
// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif
// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif
// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif
#endif

#ifndef NOTEXTMETRIC
#define TMPF_FIXED_PITCH                                             0x01
#define TMPF_VECTOR                                                  0x02
#define TMPF_DEVICE                                                  0x08
#define TMPF_TRUETYPE                                                0x04

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #if defined(UNICODE)
// #else
// #endif
// #endif

#ifndef _TEXTMETRIC_DEFINED
#define _TEXTMETRIC_DEFINED
// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif
#endif

#define NTM_REGULAR                                                  0x00000040
#define NTM_BOLD                                                     0x00000020
#define NTM_ITALIC                                                   0x00000001

#define NTM_NONNEGATIVE_AC                                           0x00010000
#define NTM_PS_OPENTYPE                                              0x00020000
#define NTM_TT_OPENTYPE                                              0x00040000
#define NTM_MULTIPLEMASTER                                           0x00080000
#define NTM_TYPE1                                                    0x00100000
#define NTM_DSIG                                                     0x00200000

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif
#endif

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif
// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif
// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)

#ifndef _PALETTEENTRY_DEFINED
#define _PALETTEENTRY_DEFINED
#endif

#ifndef _LOGPALETTE_DEFINED
#define _LOGPALETTE_DEFINED
#endif

#ifndef LF_FACESIZE
#define LF_FACESIZE                                                  32
#endif

// #endif /* WINAPI_PARTITION_APP */

#define LF_FULLFACESIZE                                              64

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif /* WINAPI_PARTITION_DESKTOP */

#define OUT_DEFAULT_PRECIS                                           0
#define OUT_STRING_PRECIS                                            1
#define OUT_CHARACTER_PRECIS                                         2
#define OUT_STROKE_PRECIS                                            3
#define OUT_TT_PRECIS                                                4
#define OUT_DEVICE_PRECIS                                            5
#define OUT_RASTER_PRECIS                                            6
#define OUT_TT_ONLY_PRECIS                                           7
#define OUT_OUTLINE_PRECIS                                           8
#define OUT_SCREEN_OUTLINE_PRECIS                                    9
#define OUT_PS_ONLY_PRECIS                                           10

#define CLIP_DEFAULT_PRECIS                                          0
#define CLIP_CHARACTER_PRECIS                                        1
#define CLIP_STROKE_PRECIS                                           2
#define CLIP_MASK                                                    0xf
#define CLIP_LH_ANGLES                                               (1<<4)
#define CLIP_TT_ALWAYS                                               (2<<4)
// #if _WIN32_WINNT >= 0x0600
#define CLIP_DFA_DISABLE                                             (4<<4)
// #endif
#define CLIP_EMBEDDED                                                (8<<4)

#define DEFAULT_QUALITY                                              0
#define DRAFT_QUALITY                                                1
#define PROOF_QUALITY                                                2
#define NONANTIALIASED_QUALITY                                       3
#define ANTIALIASED_QUALITY                                          4

#define CLEARTYPE_QUALITY                                            5
#define CLEARTYPE_NATURAL_QUALITY                                    6

#define DEFAULT_PITCH                                                0
#define FIXED_PITCH                                                  1
#define VARIABLE_PITCH                                               2
#define MONO_FONT                                                    8

#define ANSI_CHARSET                                                 0
#define DEFAULT_CHARSET                                              1
#define SYMBOL_CHARSET                                               2
#define SHIFTJIS_CHARSET                                             128
#define HANGEUL_CHARSET                                              129
#define HANGUL_CHARSET                                               129
#define GB2312_CHARSET                                               134
#define CHINESEBIG5_CHARSET                                          136
#define OEM_CHARSET                                                  255
#define JOHAB_CHARSET                                                130
#define HEBREW_CHARSET                                               177
#define ARABIC_CHARSET                                               178
#define GREEK_CHARSET                                                161
#define TURKISH_CHARSET                                              162
#define VIETNAMESE_CHARSET                                           163
#define THAI_CHARSET                                                 222
#define EASTEUROPE_CHARSET                                           238
#define RUSSIAN_CHARSET                                              204

#define MAC_CHARSET                                                  77
#define BALTIC_CHARSET                                               186

#define FS_LATIN1                                                    0x00000001
#define FS_LATIN2                                                    0x00000002
#define FS_CYRILLIC                                                  0x00000004
#define FS_GREEK                                                     0x00000008
#define FS_TURKISH                                                   0x00000010
#define FS_HEBREW                                                    0x00000020
#define FS_ARABIC                                                    0x00000040
#define FS_BALTIC                                                    0x00000080
#define FS_VIETNAMESE                                                0x00000100
#define FS_THAI                                                      0x00010000
#define FS_JISJAPAN                                                  0x00020000
#define FS_CHINESESIMP                                               0x00040000
#define FS_WANSUNG                                                   0x00080000
#define FS_CHINESETRAD                                               0x00100000
#define FS_JOHAB                                                     0x00200000
#define FS_SYMBOL                                                    0x80000000

#define FF_DONTCARE                                                  (0<<4)
#define FF_ROMAN                                                     (1<<4)

#define FF_SWISS                                                     (2<<4)

#define FF_MODERN                                                    (3<<4)

#define FF_SCRIPT                                                    (4<<4)
#define FF_DECORATIVE                                                (5<<4)

#define FW_DONTCARE                                                  0
#define FW_THIN                                                      100
#define FW_EXTRALIGHT                                                200
#define FW_LIGHT                                                     300
#define FW_NORMAL                                                    400
#define FW_MEDIUM                                                    500
#define FW_SEMIBOLD                                                  600
#define FW_BOLD                                                      700
#define FW_EXTRABOLD                                                 800
#define FW_HEAVY                                                     900

#define FW_ULTRALIGHT                                                FW_EXTRALIGHT
#define FW_REGULAR                                                   FW_NORMAL
#define FW_DEMIBOLD                                                  FW_SEMIBOLD
#define FW_ULTRABOLD                                                 FW_EXTRABOLD
#define FW_BLACK                                                     FW_HEAVY

#define PANOSE_COUNT                                                 10
#define PAN_FAMILYTYPE_INDEX                                         0
#define PAN_SERIFSTYLE_INDEX                                         1
#define PAN_WEIGHT_INDEX                                             2
#define PAN_PROPORTION_INDEX                                         3
#define PAN_CONTRAST_INDEX                                           4
#define PAN_STROKEVARIATION_INDEX                                    5
#define PAN_ARMSTYLE_INDEX                                           6
#define PAN_LETTERFORM_INDEX                                         7
#define PAN_MIDLINE_INDEX                                            8
#define PAN_XHEIGHT_INDEX                                            9

#define PAN_CULTURE_LATIN                                            0

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)

#define PAN_ANY                                                      0
#define PAN_NO_FIT                                                   1

#define PAN_FAMILY_TEXT_DISPLAY                                      2
#define PAN_FAMILY_SCRIPT                                            3
#define PAN_FAMILY_DECORATIVE                                        4
#define PAN_FAMILY_PICTORIAL                                         5

#define PAN_SERIF_COVE                                               2
#define PAN_SERIF_OBTUSE_COVE                                        3
#define PAN_SERIF_SQUARE_COVE                                        4
#define PAN_SERIF_OBTUSE_SQUARE_COVE                                 5
#define PAN_SERIF_SQUARE                                             6
#define PAN_SERIF_THIN                                               7
#define PAN_SERIF_BONE                                               8
#define PAN_SERIF_EXAGGERATED                                        9
#define PAN_SERIF_TRIANGLE                                           10
#define PAN_SERIF_NORMAL_SANS                                        11
#define PAN_SERIF_OBTUSE_SANS                                        12
#define PAN_SERIF_PERP_SANS                                          13
#define PAN_SERIF_FLARED                                             14
#define PAN_SERIF_ROUNDED                                            15

#define PAN_WEIGHT_VERY_LIGHT                                        2
#define PAN_WEIGHT_LIGHT                                             3
#define PAN_WEIGHT_THIN                                              4
#define PAN_WEIGHT_BOOK                                              5
#define PAN_WEIGHT_MEDIUM                                            6
#define PAN_WEIGHT_DEMI                                              7
#define PAN_WEIGHT_BOLD                                              8
#define PAN_WEIGHT_HEAVY                                             9
#define PAN_WEIGHT_BLACK                                             10
#define PAN_WEIGHT_NORD                                              11

#define PAN_PROP_OLD_STYLE                                           2
#define PAN_PROP_MODERN                                              3
#define PAN_PROP_EVEN_WIDTH                                          4
#define PAN_PROP_EXPANDED                                            5
#define PAN_PROP_CONDENSED                                           6
#define PAN_PROP_VERY_EXPANDED                                       7
#define PAN_PROP_VERY_CONDENSED                                      8
#define PAN_PROP_MONOSPACED                                          9

#define PAN_CONTRAST_NONE                                            2
#define PAN_CONTRAST_VERY_LOW                                        3
#define PAN_CONTRAST_LOW                                             4
#define PAN_CONTRAST_MEDIUM_LOW                                      5
#define PAN_CONTRAST_MEDIUM                                          6
#define PAN_CONTRAST_MEDIUM_HIGH                                     7
#define PAN_CONTRAST_HIGH                                            8
#define PAN_CONTRAST_VERY_HIGH                                       9

#define PAN_STROKE_GRADUAL_DIAG                                      2
#define PAN_STROKE_GRADUAL_TRAN                                      3
#define PAN_STROKE_GRADUAL_VERT                                      4
#define PAN_STROKE_GRADUAL_HORZ                                      5
#define PAN_STROKE_RAPID_VERT                                        6
#define PAN_STROKE_RAPID_HORZ                                        7
#define PAN_STROKE_INSTANT_VERT                                      8

#define PAN_STRAIGHT_ARMS_HORZ                                       2
#define PAN_STRAIGHT_ARMS_WEDGE                                      3
#define PAN_STRAIGHT_ARMS_VERT                                       4
#define PAN_STRAIGHT_ARMS_SINGLE_SERIF                               5
#define PAN_STRAIGHT_ARMS_DOUBLE_SERIF                               6
#define PAN_BENT_ARMS_HORZ                                           7
#define PAN_BENT_ARMS_WEDGE                                          8
#define PAN_BENT_ARMS_VERT                                           9
#define PAN_BENT_ARMS_SINGLE_SERIF                                   10
#define PAN_BENT_ARMS_DOUBLE_SERIF                                   11

#define PAN_LETT_NORMAL_CONTACT                                      2
#define PAN_LETT_NORMAL_WEIGHTED                                     3
#define PAN_LETT_NORMAL_BOXED                                        4
#define PAN_LETT_NORMAL_FLATTENED                                    5
#define PAN_LETT_NORMAL_ROUNDED                                      6
#define PAN_LETT_NORMAL_OFF_CENTER                                   7
#define PAN_LETT_NORMAL_SQUARE                                       8
#define PAN_LETT_OBLIQUE_CONTACT                                     9
#define PAN_LETT_OBLIQUE_WEIGHTED                                    10
#define PAN_LETT_OBLIQUE_BOXED                                       11
#define PAN_LETT_OBLIQUE_FLATTENED                                   12
#define PAN_LETT_OBLIQUE_ROUNDED                                     13
#define PAN_LETT_OBLIQUE_OFF_CENTER                                  14
#define PAN_LETT_OBLIQUE_SQUARE                                      15

#define PAN_MIDLINE_STANDARD_TRIMMED                                 2
#define PAN_MIDLINE_STANDARD_POINTED                                 3
#define PAN_MIDLINE_STANDARD_SERIFED                                 4
#define PAN_MIDLINE_HIGH_TRIMMED                                     5
#define PAN_MIDLINE_HIGH_POINTED                                     6
#define PAN_MIDLINE_HIGH_SERIFED                                     7
#define PAN_MIDLINE_CONSTANT_TRIMMED                                 8
#define PAN_MIDLINE_CONSTANT_POINTED                                 9
#define PAN_MIDLINE_CONSTANT_SERIFED                                 10
#define PAN_MIDLINE_LOW_TRIMMED                                      11
#define PAN_MIDLINE_LOW_POINTED                                      12
#define PAN_MIDLINE_LOW_SERIFED                                      13

#define PAN_XHEIGHT_CONSTANT_SMALL                                   2
#define PAN_XHEIGHT_CONSTANT_STD                                     3
#define PAN_XHEIGHT_CONSTANT_LARGE                                   4
#define PAN_XHEIGHT_DUCKING_SMALL                                    5
#define PAN_XHEIGHT_DUCKING_STD                                      6
#define PAN_XHEIGHT_DUCKING_LARGE                                    7

#define ELF_VENDOR_SIZE                                              4

// #endif /* WINAPI_PARTITION_APP */

#define ELF_VERSION                                                  0
#define ELF_CULTURE_LATIN                                            0

#define RASTER_FONTTYPE                                              0x0001
#define DEVICE_FONTTYPE                                              0x002
#define TRUETYPE_FONTTYPE                                            0x004

// #define RGB(r,g,b)                                                   ((COLORREF)(((BYTE)(r)|((WORD)((BYTE)(g))<<8))|(((DWORD)(BYTE)(b))<<16)))
// #define PALETTERGB(r,g,b)                                            (0x02000000|RGB(r,g,b))
// #define PALETTEINDEX(i)                                              ((COLORREF)(0x01000000|(DWORD)(WORD)(i)))

#define PC_RESERVED                                                  0x01
#define PC_EXPLICIT                                                  0x02
#define PC_NOCOLLAPSE                                                0x04

// #define GetRValue(rgb)                                               (LOBYTE(rgb))
// #define GetGValue(rgb)                                               (LOBYTE(((WORD)(rgb))>>8))
// #define GetBValue(rgb)                                               (LOBYTE((rgb)>>16))

#define TRANSPARENT                                                  1
#define OPAQUE                                                       2
#define BKMODE_LAST                                                  2

#define GM_COMPATIBLE                                                1
#define GM_ADVANCED                                                  2
#define GM_LAST                                                      2

#define PT_CLOSEFIGURE                                               0x01
#define PT_LINETO                                                    0x02
#define PT_BEZIERTO                                                  0x04
#define PT_MOVETO                                                    0x06

#define MM_TEXT                                                      1
#define MM_LOMETRIC                                                  2
#define MM_HIMETRIC                                                  3
#define MM_LOENGLISH                                                 4
#define MM_HIENGLISH                                                 5
#define MM_TWIPS                                                     6
#define MM_ISOTROPIC                                                 7
#define MM_ANISOTROPIC                                               8

#define MM_MIN                                                       MM_TEXT
#define MM_MAX                                                       MM_ANISOTROPIC
#define MM_MAX_FIXEDSCALE                                            MM_TWIPS

#define ABSOLUTE                                                     1
#define RELATIVE                                                     2

#define WHITE_BRUSH                                                  0
#define LTGRAY_BRUSH                                                 1
#define GRAY_BRUSH                                                   2
#define DKGRAY_BRUSH                                                 3
#define BLACK_BRUSH                                                  4
#define NULL_BRUSH                                                   5
#define HOLLOW_BRUSH                                                 NULL_BRUSH
#define WHITE_PEN                                                    6
#define BLACK_PEN                                                    7
#define NULL_PEN                                                     8
#define OEM_FIXED_FONT                                               10
#define ANSI_FIXED_FONT                                              11
#define ANSI_VAR_FONT                                                12
#define SYSTEM_FONT                                                  13
#define DEVICE_DEFAULT_FONT                                          14
#define DEFAULT_PALETTE                                              15
#define SYSTEM_FIXED_FONT                                            16
#define DEFAULT_GUI_FONT                                             17
#define DC_BRUSH                                                     18
#define DC_PEN                                                       19

#define STOCK_LAST                                                   19

#define CLR_INVALID                                                  0xFFFFFFFF

#define BS_SOLID                                                     0
#define BS_NULL                                                      1
#define BS_HOLLOW                                                    BS_NULL
#define BS_HATCHED                                                   2
#define BS_PATTERN                                                   3
#define BS_INDEXED                                                   4
#define BS_DIBPATTERN                                                5
#define BS_DIBPATTERNPT                                              6
#define BS_PATTERN8X8                                                7
#define BS_DIBPATTERN8X8                                             8
#define BS_MONOPATTERN                                               9

#define HS_HORIZONTAL                                                0
#define HS_VERTICAL                                                  1
#define HS_FDIAGONAL                                                 2
#define HS_BDIAGONAL                                                 3
#define HS_CROSS                                                     4
#define HS_DIAGCROSS                                                 5
#define HS_API_MAX                                                   12

#define PS_SOLID                                                     0
#define PS_DASH                                                      1
#define PS_DOT                                                       2
#define PS_DASHDOT                                                   3
#define PS_DASHDOTDOT                                                4
#define PS_NULL                                                      5
#define PS_INSIDEFRAME                                               6
#define PS_USERSTYLE                                                 7
#define PS_ALTERNATE                                                 8
#define PS_STYLE_MASK                                                0x0000000F

#define PS_ENDCAP_ROUND                                              0x00000000
#define PS_ENDCAP_SQUARE                                             0x00000100
#define PS_ENDCAP_FLAT                                               0x00000200
#define PS_ENDCAP_MASK                                               0x00000F00

#define PS_JOIN_ROUND                                                0x00000000
#define PS_JOIN_BEVEL                                                0x00001000
#define PS_JOIN_MITER                                                0x00002000
#define PS_JOIN_MASK                                                 0x0000F000

#define PS_COSMETIC                                                  0x00000000
#define PS_GEOMETRIC                                                 0x00010000
#define PS_TYPE_MASK                                                 0x000F0000

#define AD_COUNTERCLOCKWISE                                          1
#define AD_CLOCKWISE                                                 2

#define DRIVERVERSION                                                0
#define TECHNOLOGY                                                   2
#define HORZSIZE                                                     4
#define VERTSIZE                                                     6
#define HORZRES                                                      8
#define VERTRES                                                      10
#define BITSPIXEL                                                    12
#define PLANES                                                       14
#define NUMBRUSHES                                                   16
#define NUMPENS                                                      18
#define NUMMARKERS                                                   20
#define NUMFONTS                                                     22
#define NUMCOLORS                                                    24
#define PDEVICESIZE                                                  26
#define CURVECAPS                                                    28
#define LINECAPS                                                     30
#define POLYGONALCAPS                                                32
#define TEXTCAPS                                                     34
#define CLIPCAPS                                                     36
#define RASTERCAPS                                                   38
#define ASPECTX                                                      40
#define ASPECTY                                                      42
#define ASPECTXY                                                     44

#define LOGPIXELSX                                                   88
#define LOGPIXELSY                                                   90

#define SIZEPALETTE                                                  104
#define NUMRESERVED                                                  106
#define COLORRES                                                     108

#define PHYSICALWIDTH                                                110
#define PHYSICALHEIGHT                                               111
#define PHYSICALOFFSETX                                              112
#define PHYSICALOFFSETY                                              113
#define SCALINGFACTORX                                               114
#define SCALINGFACTORY                                               115

#define VREFRESH                                                     116
#define DESKTOPVERTRES                                               117
#define DESKTOPHORZRES                                               118
#define BLTALIGNMENT                                                 119

#define SHADEBLENDCAPS                                               120
#define COLORMGMTCAPS                                                121

#ifndef NOGDICAPMASKS
#define DT_PLOTTER                                                   0
#define DT_RASDISPLAY                                                1
#define DT_RASPRINTER                                                2
#define DT_RASCAMERA                                                 3
#define DT_CHARSTREAM                                                4
#define DT_METAFILE                                                  5
#define DT_DISPFILE                                                  6

#define CC_NONE                                                      0
#define CC_CIRCLES                                                   1
#define CC_PIE                                                       2
#define CC_CHORD                                                     4
#define CC_ELLIPSES                                                  8
#define CC_WIDE                                                      16
#define CC_STYLED                                                    32
#define CC_WIDESTYLED                                                64
#define CC_INTERIORS                                                 128
#define CC_ROUNDRECT                                                 256

#define LC_NONE                                                      0
#define LC_POLYLINE                                                  2
#define LC_MARKER                                                    4
#define LC_POLYMARKER                                                8
#define LC_WIDE                                                      16
#define LC_STYLED                                                    32
#define LC_WIDESTYLED                                                64
#define LC_INTERIORS                                                 128

#define PC_NONE                                                      0
#define PC_POLYGON                                                   1
#define PC_RECTANGLE                                                 2
#define PC_WINDPOLYGON                                               4
#define PC_TRAPEZOID                                                 4
#define PC_SCANLINE                                                  8
#define PC_WIDE                                                      16
#define PC_STYLED                                                    32
#define PC_WIDESTYLED                                                64
#define PC_INTERIORS                                                 128
#define PC_POLYPOLYGON                                               256
#define PC_PATHS                                                     512

#define CP_NONE                                                      0
#define CP_RECTANGLE                                                 1
#define CP_REGION                                                    2

#define TC_OP_CHARACTER                                              0x00000001
#define TC_OP_STROKE                                                 0x00000002
#define TC_CP_STROKE                                                 0x00000004
#define TC_CR_90                                                     0x00000008
#define TC_CR_ANY                                                    0x00000010
#define TC_SF_X_YINDEP                                               0x00000020
#define TC_SA_DOUBLE                                                 0x00000040
#define TC_SA_INTEGER                                                0x00000080
#define TC_SA_CONTIN                                                 0x00000100
#define TC_EA_DOUBLE                                                 0x00000200
#define TC_IA_ABLE                                                   0x00000400
#define TC_UA_ABLE                                                   0x00000800
#define TC_SO_ABLE                                                   0x00001000
#define TC_RA_ABLE                                                   0x00002000
#define TC_VA_ABLE                                                   0x00004000
#define TC_RESERVED                                                  0x00008000
#define TC_SCROLLBLT                                                 0x00010000
#endif

#define RC_NONE
#define RC_BITBLT                                                    1
#define RC_BANDING                                                   2
#define RC_SCALING                                                   4
#define RC_BITMAP64                                                  8
#define RC_GDI20_OUTPUT                                              0x0010
#define RC_GDI20_STATE                                               0x0020
#define RC_SAVEBITMAP                                                0x0040
#define RC_DI_BITMAP                                                 0x0080
#define RC_PALETTE                                                   0x0100
#define RC_DIBTODEV                                                  0x0200
#define RC_BIGFONT                                                   0x0400
#define RC_STRETCHBLT                                                0x0800
#define RC_FLOODFILL                                                 0x1000
#define RC_STRETCHDIB                                                0x2000
#define RC_OP_DX_OUTPUT                                              0x4000
#define RC_DEVBITS                                                   0x8000

#define SB_NONE                                                      0x00000000
#define SB_CONST_ALPHA                                               0x00000001
#define SB_PIXEL_ALPHA                                               0x00000002
#define SB_PREMULT_ALPHA                                             0x00000004

#define SB_GRAD_RECT                                                 0x00000010
#define SB_GRAD_TRI                                                  0x00000020

#define CM_NONE                                                      0x00000000
#define CM_DEVICE_ICM                                                0x00000001
#define CM_GAMMA_RAMP                                                0x00000002
#define CM_CMYK_COLOR                                                0x00000004

#define DIB_RGB_COLORS                                               0
#define DIB_PAL_COLORS                                               1

#define SYSPAL_ERROR                                                 0
#define SYSPAL_STATIC                                                1
#define SYSPAL_NOSTATIC                                              2
#define SYSPAL_NOSTATIC256                                           3

#define CBM_INIT                                                     0x04

#define FLOODFILLBORDER                                              0
#define FLOODFILLSURFACE                                             1

#ifndef CCHDEVICENAME
#define CCHDEVICENAME                                                32
#endif

#define CCHFORMNAME                                                  32

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif

#define DM_SPECVERSION                                               0x0401

#define DM_ORIENTATION                                               0x00000001
#define DM_PAPERSIZE                                                 0x00000002
#define DM_PAPERLENGTH                                               0x00000004
#define DM_PAPERWIDTH                                                0x00000008
#define DM_SCALE                                                     0x00000010
#define DM_POSITION                                                  0x00000020
#define DM_NUP                                                       0x00000040
#define DM_DISPLAYORIENTATION                                        0x00000080
#define DM_COPIES                                                    0x00000100
#define DM_DEFAULTSOURCE                                             0x00000200
#define DM_PRINTQUALITY                                              0x00000400
#define DM_COLOR                                                     0x00000800
#define DM_DUPLEX                                                    0x00001000
#define DM_YRESOLUTION                                               0x00002000
#define DM_TTOPTION                                                  0x00004000
#define DM_COLLATE                                                   0x00008000
#define DM_FORMNAME                                                  0x00010000
#define DM_LOGPIXELS                                                 0x00020000
#define DM_BITSPERPEL                                                0x00040000
#define DM_PELSWIDTH                                                 0x00080000
#define DM_PELSHEIGHT                                                0x00100000
#define DM_DISPLAYFLAGS                                              0x00200000
#define DM_DISPLAYFREQUENCY                                          0x00400000
#define DM_ICMMETHOD                                                 0x00800000
#define DM_ICMINTENT                                                 0x01000000
#define DM_MEDIATYPE                                                 0x02000000
#define DM_DITHERTYPE                                                0x04000000
#define DM_PANNINGWIDTH                                              0x08000000
#define DM_PANNINGHEIGHT                                             0x10000000
#define DM_DISPLAYFIXEDOUTPUT                                        0x20000000

#define DMORIENT_PORTRAIT                                            1
#define DMORIENT_LANDSCAPE                                           2

#define DMPAPER_FIRST                                                DMPAPER_LETTER
#define DMPAPER_LETTER                                               1
#define DMPAPER_LETTERSMALL                                          2
#define DMPAPER_TABLOID                                              3
#define DMPAPER_LEDGER                                               4
#define DMPAPER_LEGAL                                                5
#define DMPAPER_STATEMENT                                            6
#define DMPAPER_EXECUTIVE                                            7
#define DMPAPER_A3                                                   8
#define DMPAPER_A4                                                   9
#define DMPAPER_A4SMALL                                              10
#define DMPAPER_A5                                                   11
#define DMPAPER_B4                                                   12
#define DMPAPER_B5                                                   13
#define DMPAPER_FOLIO                                                14
#define DMPAPER_QUARTO                                               15
#define DMPAPER_10X14                                                16
#define DMPAPER_11X17                                                17
#define DMPAPER_NOTE                                                 18
#define DMPAPER_ENV_9                                                19
#define DMPAPER_ENV_10                                               20
#define DMPAPER_ENV_11                                               21
#define DMPAPER_ENV_12                                               22
#define DMPAPER_ENV_14                                               23
#define DMPAPER_CSHEET                                               24
#define DMPAPER_DSHEET                                               25
#define DMPAPER_ESHEET                                               26
#define DMPAPER_ENV_DL                                               27
#define DMPAPER_ENV_C5                                               28
#define DMPAPER_ENV_C3                                               29
#define DMPAPER_ENV_C4                                               30
#define DMPAPER_ENV_C6                                               31
#define DMPAPER_ENV_C65                                              32
#define DMPAPER_ENV_B4                                               33
#define DMPAPER_ENV_B5                                               34
#define DMPAPER_ENV_B6                                               35
#define DMPAPER_ENV_ITALY                                            36
#define DMPAPER_ENV_MONARCH                                          37
#define DMPAPER_ENV_PERSONAL                                         38
#define DMPAPER_FANFOLD_US                                           39
#define DMPAPER_FANFOLD_STD_GERMAN                                   40
#define DMPAPER_FANFOLD_LGL_GERMAN                                   41
#define DMPAPER_ISO_B4                                               42
#define DMPAPER_JAPANESE_POSTCARD                                    43
#define DMPAPER_9X11                                                 44
#define DMPAPER_10X11                                                45
#define DMPAPER_15X11                                                46
#define DMPAPER_ENV_INVITE                                           47
#define DMPAPER_RESERVED_48                                          48
#define DMPAPER_RESERVED_49                                          49
#define DMPAPER_LETTER_EXTRA                                         50
#define DMPAPER_LEGAL_EXTRA                                          51
#define DMPAPER_TABLOID_EXTRA                                        52
#define DMPAPER_A4_EXTRA                                             53
#define DMPAPER_LETTER_TRANSVERSE                                    54
#define DMPAPER_A4_TRANSVERSE                                        55
#define DMPAPER_LETTER_EXTRA_TRANSVERSE                              56
#define DMPAPER_A_PLUS                                               57
#define DMPAPER_B_PLUS                                               58
#define DMPAPER_LETTER_PLUS                                          59
#define DMPAPER_A4_PLUS                                              60
#define DMPAPER_A5_TRANSVERSE                                        61
#define DMPAPER_B5_TRANSVERSE                                        62
#define DMPAPER_A3_EXTRA                                             63
#define DMPAPER_A5_EXTRA                                             64
#define DMPAPER_B5_EXTRA                                             65
#define DMPAPER_A2                                                   66
#define DMPAPER_A3_TRANSVERSE                                        67
#define DMPAPER_A3_EXTRA_TRANSVERSE                                  68
#define DMPAPER_DBL_JAPANESE_POSTCARD                                69
#define DMPAPER_A6                                                   70
#define DMPAPER_JENV_KAKU2                                           71
#define DMPAPER_JENV_KAKU3                                           72
#define DMPAPER_JENV_CHOU3                                           73
#define DMPAPER_JENV_CHOU4                                           74
#define DMPAPER_LETTER_ROTATED                                       75
#define DMPAPER_A3_ROTATED                                           76
#define DMPAPER_A4_ROTATED                                           77
#define DMPAPER_A5_ROTATED                                           78
#define DMPAPER_B4_JIS_ROTATED                                       79
#define DMPAPER_B5_JIS_ROTATED                                       80
#define DMPAPER_JAPANESE_POSTCARD_ROTATED                            81
#define DMPAPER_DBL_JAPANESE_POSTCARD_ROTATED                        82
#define DMPAPER_A6_ROTATED                                           83
#define DMPAPER_JENV_KAKU2_ROTATED                                   84
#define DMPAPER_JENV_KAKU3_ROTATED                                   85
#define DMPAPER_JENV_CHOU3_ROTATED                                   86
#define DMPAPER_JENV_CHOU4_ROTATED                                   87
#define DMPAPER_B6_JIS                                               88
#define DMPAPER_B6_JIS_ROTATED                                       89
#define DMPAPER_12X11                                                90
#define DMPAPER_JENV_YOU4                                            91
#define DMPAPER_JENV_YOU4_ROTATED                                    92
#define DMPAPER_P16K                                                 93
#define DMPAPER_P32K                                                 94
#define DMPAPER_P32KBIG                                              95
#define DMPAPER_PENV_1                                               96
#define DMPAPER_PENV_2                                               97
#define DMPAPER_PENV_3                                               98
#define DMPAPER_PENV_4                                               99
#define DMPAPER_PENV_5                                               100
#define DMPAPER_PENV_6                                               101
#define DMPAPER_PENV_7                                               102
#define DMPAPER_PENV_8                                               103
#define DMPAPER_PENV_9                                               104
#define DMPAPER_PENV_10                                              105
#define DMPAPER_P16K_ROTATED                                         106
#define DMPAPER_P32K_ROTATED                                         107
#define DMPAPER_P32KBIG_ROTATED                                      108
#define DMPAPER_PENV_1_ROTATED                                       109
#define DMPAPER_PENV_2_ROTATED                                       110
#define DMPAPER_PENV_3_ROTATED                                       111
#define DMPAPER_PENV_4_ROTATED                                       112
#define DMPAPER_PENV_5_ROTATED                                       113
#define DMPAPER_PENV_6_ROTATED                                       114
#define DMPAPER_PENV_7_ROTATED                                       115
#define DMPAPER_PENV_8_ROTATED                                       116
#define DMPAPER_PENV_9_ROTATED                                       117
#define DMPAPER_PENV_10_ROTATED                                      118

#define DMPAPER_LAST                                                 DMPAPER_PENV_10_ROTATED

#define DMPAPER_USER                                                 256

#define DMBIN_FIRST                                                  DMBIN_UPPER
#define DMBIN_UPPER                                                  1
#define DMBIN_ONLYONE                                                1
#define DMBIN_LOWER                                                  2
#define DMBIN_MIDDLE                                                 3
#define DMBIN_MANUAL                                                 4
#define DMBIN_ENVELOPE                                               5
#define DMBIN_ENVMANUAL                                              6
#define DMBIN_AUTO                                                   7
#define DMBIN_TRACTOR                                                8
#define DMBIN_SMALLFMT                                               9
#define DMBIN_LARGEFMT                                               10
#define DMBIN_LARGECAPACITY                                          11
#define DMBIN_CASSETTE                                               14
#define DMBIN_FORMSOURCE                                             15
#define DMBIN_LAST                                                   DMBIN_FORMSOURCE

#define DMBIN_USER                                                   256

#define DMRES_DRAFT                                                  (-1)
#define DMRES_LOW                                                    (-2)
#define DMRES_MEDIUM                                                 (-3)
#define DMRES_HIGH                                                   (-4)

#define DMCOLOR_MONOCHROME                                           1
#define DMCOLOR_COLOR                                                2

#define DMDUP_SIMPLEX                                                1
#define DMDUP_VERTICAL                                               2
#define DMDUP_HORIZONTAL                                             3

#define DMTT_BITMAP                                                  1
#define DMTT_DOWNLOAD                                                2
#define DMTT_SUBDEV                                                  3
#define DMTT_DOWNLOAD_OUTLINE                                        4

#define DMCOLLATE_FALSE                                              0
#define DMCOLLATE_TRUE                                               1

#define DMDO_DEFAULT                                                 0
#define DMDO_90                                                      1
#define DMDO_180                                                     2
#define DMDO_270                                                     3

#define DMDFO_DEFAULT                                                0
#define DMDFO_STRETCH                                                1
#define DMDFO_CENTER                                                 2

#define DM_INTERLACED                                                0x00000002
#define DMDISPLAYFLAGS_TEXTMODE                                      0x00000004

#define DMNUP_SYSTEM                                                 1
#define DMNUP_ONEUP                                                  2

#define DMICMMETHOD_NONE                                             1
#define DMICMMETHOD_SYSTEM                                           2
#define DMICMMETHOD_DRIVER                                           3
#define DMICMMETHOD_DEVICE                                           4

#define DMICMMETHOD_USER                                             256

#define DMICM_SATURATE                                               1
#define DMICM_CONTRAST                                               2
#define DMICM_COLORIMETRIC                                           3
#define DMICM_ABS_COLORIMETRIC                                       4

#define DMICM_USER                                                   256

#define DMMEDIA_STANDARD                                             1
#define DMMEDIA_TRANSPARENCY                                         2
#define DMMEDIA_GLOSSY                                               3

#define DMMEDIA_USER                                                 256

#define DMDITHER_NONE                                                1
#define DMDITHER_COARSE                                              2
#define DMDITHER_FINE                                                3
#define DMDITHER_LINEART                                             4
#define DMDITHER_ERRORDIFFUSION                                      5
#define DMDITHER_RESERVED6                                           6
#define DMDITHER_RESERVED7                                           7
#define DMDITHER_RESERVED8                                           8
#define DMDITHER_RESERVED9                                           9
#define DMDITHER_GRAYSCALE                                           10

#define DMDITHER_USER                                                256

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif

#define DISPLAY_DEVICE_ATTACHED_TO_DESKTOP                           0x00000001
#define DISPLAY_DEVICE_MULTI_DRIVER                                  0x00000002
#define DISPLAY_DEVICE_PRIMARY_DEVICE                                0x00000004
#define DISPLAY_DEVICE_MIRRORING_DRIVER                              0x00000008
#define DISPLAY_DEVICE_VGA_COMPATIBLE                                0x00000010
#define DISPLAY_DEVICE_REMOVABLE                                     0x00000020
// #if _WIN32_WINNT >= 0x0602
#define DISPLAY_DEVICE_ACC_DRIVER                                    0x00000040
// #endif
#define DISPLAY_DEVICE_TS_COMPATIBLE                                 0x00200000
// #if _WIN32_WINNT >= 0x0600
#define DISPLAY_DEVICE_UNSAFE_MODES_ON                               0x00080000
// #endif
#define DISPLAY_DEVICE_MODESPRUNED                                   0x08000000
#define DISPLAY_DEVICE_REMOTE                                        0x04000000
#define DISPLAY_DEVICE_DISCONNECT                                    0x02000000

#define DISPLAY_DEVICE_ACTIVE                                        0x00000001
#define DISPLAY_DEVICE_ATTACHED                                      0x00000002

// #if WINVER >= 0x0601
#define DISPLAYCONFIG_MAXPATH                                        1024

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)

#define DISPLAYCONFIG_PATH_MODE_IDX_INVALID                          0xffffffff

#define DISPLAYCONFIG_SOURCE_IN_USE                                  0x1

#define DISPLAYCONFIG_TARGET_IN_USE                                  0x00000001
#define DISPLAYCONFIG_TARGET_FORCIBLE                                0x00000002
#define DISPLAYCONFIG_TARGET_FORCED_AVAILABILITY_BOOT                0x00000004
#define DISPLAYCONFIG_TARGET_FORCED_AVAILABILITY_PATH                0x00000008
#define DISPLAYCONFIG_TARGET_FORCED_AVAILABILITY_SYSTEM              0x00000010

#define DISPLAYCONFIG_PATH_ACTIVE                                    0x1

// #endif /* WINAPI_PARTITION_APP */

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif /* WINAPI_PARTITION_DESKTOP */

#define QDC_ALL_PATHS                                                0x00000001
#define QDC_ONLY_ACTIVE_PATHS                                        0x00000002
#define QDC_DATABASE_CURRENT                                         0x00000004

#define SDC_TOPOLOGY_INTERNAL                                        0x00000001
#define SDC_TOPOLOGY_CLONE                                           0x00000002
#define SDC_TOPOLOGY_EXTEND                                          0x00000004
#define SDC_TOPOLOGY_EXTERNAL                                        0x00000008
#define SDC_TOPOLOGY_SUPPLIED                                        0x00000010
// #define SDC_USE_DATABASE_CURRENT                                     (SDC_TOPOLOGY_INTERNAL|SDC_TOPOLOGY_CLONE\ /* TOFIX: incompleto */

#define SDC_USE_SUPPLIED_DISPLAY_CONFIG                              0x00000020
#define SDC_VALIDATE                                                 0x00000040
#define SDC_APPLY                                                    0x00000080
#define SDC_NO_OPTIMIZATION                                          0x00000100
#define SDC_SAVE_TO_DATABASE                                         0x00000200
#define SDC_ALLOW_CHANGES                                            0x00000400
#define SDC_PATH_PERSIST_IF_REQUIRED                                 0x00000800
#define SDC_FORCE_MODE_ENUMERATION                                   0x00001000
#define SDC_ALLOW_PATH_ORDER_CHANGES                                 0x00002000

// #endif /* WINVER >= 0x0601 */

#define RDH_RECTANGLES                                               1

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif

#define SYSRGN                                                       4

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

#ifndef NOTEXTMETRIC
// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif
#endif

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

#define GGO_METRICS                                                  0
#define GGO_BITMAP                                                   1
#define GGO_NATIVE                                                   2
#define GGO_BEZIER                                                   3
#define GGO_GRAY2_BITMAP                                             4
#define GGO_GRAY4_BITMAP                                             5
#define GGO_GRAY8_BITMAP                                             6
#define GGO_GLYPH_INDEX                                              0x0080
#define GGO_UNHINTED                                                 0x0100

#define TT_POLYGON_TYPE                                              24

#define TT_PRIM_LINE                                                 1
#define TT_PRIM_QSPLINE                                              2
#define TT_PRIM_CSPLINE                                              3

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

#define GCP_DBCS                                                     0x0001
#define GCP_REORDER                                                  0x0002
#define GCP_USEKERNING                                               0x0008
#define GCP_GLYPHSHAPE                                               0x0010
#define GCP_LIGATE                                                   0x0020
#define GCP_DIACRITIC                                                0x0100
#define GCP_KASHIDA                                                  0x0400
#define GCP_ERROR                                                    0x8000
#define FLI_MASK                                                     0x103B

#define GCP_JUSTIFY                                                  0x00010000
#define FLI_GLYPHS                                                   0x00040000
#define GCP_CLASSIN                                                  0x00080000
#define GCP_MAXEXTENT                                                0x00100000
#define GCP_JUSTIFYIN                                                0x00200000
#define GCP_DISPLAYZWG                                               0x00400000
#define GCP_SYMSWAPOFF                                               0x00800000
#define GCP_NUMERICOVERRIDE                                          0x01000000
#define GCP_NEUTRALOVERRIDE                                          0x02000000
#define GCP_NUMERICSLATIN                                            0x04000000
#define GCP_NUMERICSLOCAL                                            0x08000000

#define GCPCLASS_LATIN                                               1
#define GCPCLASS_HEBREW                                              2
#define GCPCLASS_ARABIC                                              2
#define GCPCLASS_NEUTRAL                                             3
#define GCPCLASS_LOCALNUMBER                                         4
#define GCPCLASS_LATINNUMBER                                         5
#define GCPCLASS_LATINNUMERICTERMINATOR                              6
#define GCPCLASS_LATINNUMERICSEPARATOR                               7
#define GCPCLASS_NUMERICSEPARATOR                                    8
#define GCPCLASS_PREBOUNDLTR                                         0x80
#define GCPCLASS_PREBOUNDRTL                                         0x40
#define GCPCLASS_POSTBOUNDLTR                                        0x20
#define GCPCLASS_POSTBOUNDRTL                                        0x10

#define GCPGLYPH_LINKBEFORE                                          0x8000
#define GCPGLYPH_LINKAFTER                                           0x4000

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif

#define TT_AVAILABLE                                                 0x0001
#define TT_ENABLED                                                   0x0002

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif

#define PFD_TYPE_RGBA                                                0
#define PFD_TYPE_COLORINDEX                                          1

#define PFD_MAIN_PLANE                                               0
#define PFD_OVERLAY_PLANE                                            1
#define PFD_UNDERLAY_PLANE                                           (-1)

#define PFD_DOUBLEBUFFER                                             0x00000001
#define PFD_STEREO                                                   0x00000002
#define PFD_DRAW_TO_WINDOW                                           0x00000004
#define PFD_DRAW_TO_BITMAP                                           0x00000008
#define PFD_SUPPORT_GDI                                              0x00000010
#define PFD_SUPPORT_OPENGL                                           0x00000020
#define PFD_GENERIC_FORMAT                                           0x00000040
#define PFD_NEED_PALETTE                                             0x00000080
#define PFD_NEED_SYSTEM_PALETTE                                      0x00000100
#define PFD_SWAP_EXCHANGE                                            0x00000200
#define PFD_SWAP_COPY                                                0x00000400
#define PFD_SWAP_LAYER_BUFFERS                                       0x00000800
#define PFD_GENERIC_ACCELERATED                                      0x00001000
#define PFD_SUPPORT_DIRECTDRAW                                       0x00002000
#define PFD_DIRECT3D_ACCELERATED                                     0x00004000
#define PFD_SUPPORT_COMPOSITION                                      0x00008000

#define PFD_DEPTH_DONTCARE                                           0x20000000
#define PFD_DOUBLEBUFFER_DONTCARE                                    0x40000000
#define PFD_STEREO_DONTCARE                                          0x80000000

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)

// #ifndef NOTEXTMETRIC
// #define OLDFONTENUMPROC                                              __MINGW_NAME_AW(OLDFONTENUMPROC)
// #else
// #define OLDFONTENUMPROC                                              __MINGW_NAME_AW(OLDFONTENUMPROC)
// #endif

// #define AddFontResource                                              __MINGW_NAME_AW(AddFontResource)
// #define CopyMetaFile                                                 __MINGW_NAME_AW(CopyMetaFile)
// #define CreateDC                                                     __MINGW_NAME_AW(CreateDC)
// #define CreateFontIndirect                                           __MINGW_NAME_AW(CreateFontIndirect)
// #define CreateFont                                                   __MINGW_NAME_AW(CreateFont)
// #define CreateIC                                                     __MINGW_NAME_AW(CreateIC)
// #define CreateMetaFile                                               __MINGW_NAME_AW(CreateMetaFile)
// #define CreateScalableFontResource                                   __MINGW_NAME_AW(CreateScalableFontResource)

// #endif /* WINAPI_PARTITION_DESKTOP */

#define DM_UPDATE                                                    1
#define DM_COPY                                                      2
#define DM_PROMPT                                                    4
#define DM_MODIFY                                                    8

#define DM_IN_BUFFER                                                 DM_MODIFY
#define DM_IN_PROMPT                                                 DM_PROMPT
#define DM_OUT_BUFFER                                                DM_COPY
#define DM_OUT_DEFAULT                                               DM_UPDATE

#define DC_FIELDS                                                    1
#define DC_PAPERS                                                    2
#define DC_PAPERSIZE                                                 3
#define DC_MINEXTENT                                                 4
#define DC_MAXEXTENT                                                 5
#define DC_BINS                                                      6
#define DC_DUPLEX                                                    7
#define DC_SIZE                                                      8
#define DC_EXTRA                                                     9
#define DC_VERSION                                                   10
#define DC_DRIVER                                                    11
#define DC_BINNAMES                                                  12
#define DC_ENUMRESOLUTIONS                                           13
#define DC_FILEDEPENDENCIES                                          14
#define DC_TRUETYPE                                                  15
#define DC_PAPERNAMES                                                16
#define DC_ORIENTATION                                               17
#define DC_COPIES                                                    18
#define DC_BINADJUST                                                 19
#define DC_EMF_COMPLIANT                                             20
#define DC_DATATYPE_PRODUCED                                         21
#define DC_COLLATE                                                   22
#define DC_MANUFACTURER                                              23
#define DC_MODEL                                                     24
#define DC_PERSONALITY                                               25
#define DC_PRINTRATE                                                 26
#define DC_PRINTRATEUNIT                                             27
#define PRINTRATEUNIT_PPM                                            1
#define PRINTRATEUNIT_CPS                                            2
#define PRINTRATEUNIT_LPM                                            3
#define PRINTRATEUNIT_IPM                                            4
#define DC_PRINTERMEM                                                28
#define DC_MEDIAREADY                                                29
#define DC_STAPLE                                                    30
#define DC_PRINTRATEPPM                                              31
#define DC_COLORDEVICE                                               32
#define DC_NUP                                                       33
#define DC_MEDIATYPENAMES                                            34
#define DC_MEDIATYPES                                                35

#define DCTT_BITMAP                                                  0x0000001
#define DCTT_DOWNLOAD                                                0x0000002
#define DCTT_SUBDEV                                                  0x0000004
#define DCTT_DOWNLOAD_OUTLINE                                        0x0000008

#define DCBA_FACEUPNONE                                              0x0000
#define DCBA_FACEUPCENTER                                            0x0001
#define DCBA_FACEUPLEFT                                              0x0002
#define DCBA_FACEUPRIGHT                                             0x0003
#define DCBA_FACEDOWNNONE                                            0x0100
#define DCBA_FACEDOWNCENTER                                          0x0101
#define DCBA_FACEDOWNLEFT                                            0x0102
#define DCBA_FACEDOWNRIGHT                                           0x0103

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #define DeviceCapabilities                                           __MINGW_NAME_AW(DeviceCapabilities)
// #define EnumFontFamiliesEx                                           __MINGW_NAME_AW(EnumFontFamiliesEx)
// #define EnumFontFamilies                                             __MINGW_NAME_AW(EnumFontFamilies)
// #define EnumFonts                                                    __MINGW_NAME_AW(EnumFonts)
// #define GetCharWidth                                                 __MINGW_NAME_AW(GetCharWidth)
// #define GetCharWidth32                                               __MINGW_NAME_AW(GetCharWidth32)
// #define GetCharWidthFloat                                            __MINGW_NAME_AW(GetCharWidthFloat)
// #define GetCharABCWidths                                             __MINGW_NAME_AW(GetCharABCWidths)
// #define GetCharABCWidthsFloat                                        __MINGW_NAME_AW(GetCharABCWidthsFloat)
// #define GetGlyphOutline                                              __MINGW_NAME_AW(GetGlyphOutline)
// #define GetMetaFile                                                  __MINGW_NAME_AW(GetMetaFile)

// #ifndef NOTEXTMETRIC
// #define GetOutlineTextMetrics                                        __MINGW_NAME_AW(GetOutlineTextMetrics)
// #endif

// #define GetTextExtentPoint                                           __MINGW_NAME_AW(GetTextExtentPoint)
// #define GetTextExtentPoint32                                         __MINGW_NAME_AW(GetTextExtentPoint32)
// #define GetTextExtentExPoint                                         __MINGW_NAME_AW(GetTextExtentExPoint)
// #define GetCharacterPlacement                                        __MINGW_NAME_AW(GetCharacterPlacement)

#define GS_8BIT_INDICES                                              0x00000001

#define GGI_MARK_NONEXISTING_GLYPHS                                  0X0001

// #define GetGlyphIndices                                              __MINGW_NAME_AW(GetGlyphIndices)

// #define STAMP_DESIGNVECTOR                                           (0x8000000+'d'+('v'<<8))
// #define STAMP_AXESLIST                                               (0x8000000+'a'+('l'<<8))
#define MM_MAX_NUMAXES                                               16

// #define AddFontResourceEx                                            __MINGW_NAME_AW(AddFontResourceEx)
// #define RemoveFontResourceEx                                         __MINGW_NAME_AW(RemoveFontResourceEx)

#define FR_PRIVATE                                                   0x10
#define FR_NOT_ENUM                                                  0x20

#define MM_MAX_AXES_NAMELEN                                          16

// #define CreateFontIndirectEx                                         __MINGW_NAME_AW(CreateFontIndirectEx)

// #ifndef NOTEXTMETRIC
// #endif

// #define ResetDC                                                      __MINGW_NAME_AW(ResetDC)
// #define RemoveFontResource                                           __MINGW_NAME_AW(RemoveFontResource)

// #if defined (COMBOX_SANDBOX) &&  _WIN32_WINNT >= 0x0600
#define GDIREGISTERDDRAWPACKETVERSION                                0x1
// #endif

// #endif /* WINAPI_PARTITION_DESKTOP */

// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif
// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
// #endif
// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP)
// #endif
// #if WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_DESKTOP)
#define AC_SRC_OVER                                                  0x00
#define AC_SRC_ALPHA                                                 0x01

#define GRADIENT_FILL_RECT_H                                         0x00000000
#define GRADIENT_FILL_RECT_V                                         0x00000001
#define GRADIENT_FILL_TRIANGLE                                       0x00000002
#define GRADIENT_FILL_OP_FLAG                                        0x000000ff

// #ifndef NOMETAFILE
// #define CopyEnhMetaFile                                              __MINGW_NAME_AW(CopyEnhMetaFile)
// #define CreateEnhMetaFile                                            __MINGW_NAME_AW(CreateEnhMetaFile)
// #define GetEnhMetaFile                                               __MINGW_NAME_AW(GetEnhMetaFile)
// #define GetEnhMetaFileDescription                                    __MINGW_NAME_AW(GetEnhMetaFileDescription)
// #endif

// #ifndef NOTEXTMETRIC
// #define GetTextMetrics                                               __MINGW_NAME_AW(GetTextMetrics)
// #endif

#define CA_NEGATIVE                                                  0x0001
#define CA_LOG_FILTER                                                0x0002

#define ILLUMINANT_DEVICE_DEFAULT                                    0
#define ILLUMINANT_A                                                 1
#define ILLUMINANT_B                                                 2
#define ILLUMINANT_C                                                 3
#define ILLUMINANT_D50                                               4
#define ILLUMINANT_D55                                               5
#define ILLUMINANT_D65                                               6
#define ILLUMINANT_D75                                               7
#define ILLUMINANT_F2                                                8
#define ILLUMINANT_MAX_INDEX                                         ILLUMINANT_F2

#define ILLUMINANT_TUNGSTEN                                          ILLUMINANT_A
#define ILLUMINANT_DAYLIGHT                                          ILLUMINANT_C
#define ILLUMINANT_FLUORESCENT                                       ILLUMINANT_F2
#define ILLUMINANT_NTSC                                              ILLUMINANT_C

#define RGB_GAMMA_MIN                                                02500
#define RGB_GAMMA_MAX                                                65000

#define REFERENCE_WHITE_MIN                                          6000
#define REFERENCE_WHITE_MAX                                          10000
#define REFERENCE_BLACK_MIN                                          0
#define REFERENCE_BLACK_MAX                                          4000

#define COLOR_ADJ_MIN                                                -100
#define COLOR_ADJ_MAX                                                100

#define DI_APPBANDING                                                0x00000001
#define DI_ROPS_READ_DESTINATION                                     0x00000002

// #define StartDoc                                                     __MINGW_NAME_AW(StartDoc)
// #define GetObject                                                    __MINGW_NAME_AW(GetObject)
// #define TextOut                                                      __MINGW_NAME_AW(TextOut)
// #define ExtTextOut                                                   __MINGW_NAME_AW(ExtTextOut)
// #define PolyTextOut                                                  __MINGW_NAME_AW(PolyTextOut)
// #define GetTextFace                                                  __MINGW_NAME_AW(GetTextFace)

#define FONTMAPPER_MAX                                               10

// #define GetKerningPairs                                              __MINGW_NAME_AW(GetKerningPairs)

#define ICM_OFF                                                      1
#define ICM_ON                                                       2
#define ICM_QUERY                                                    3
#define ICM_DONE_OUTSIDEDC                                           4

// #define ICMENUMPROC                                                  __MINGW_NAME_AW(ICMENUMPROC)
// #define EnumICMProfiles                                              __MINGW_NAME_AW(EnumICMProfiles)
// #define UpdateICMRegKey                                              __MINGW_NAME_AW(UpdateICMRegKey)
// #define GetLogColorSpace                                             __MINGW_NAME_AW(GetLogColorSpace)
// #define CreateColorSpace                                             __MINGW_NAME_AW(CreateColorSpace)
// #define GetICMProfile                                                __MINGW_NAME_AW(GetICMProfile)
// #define SetICMProfile                                                __MINGW_NAME_AW(SetICMProfile)

#ifndef NOMETAFILE

#define ENHMETA_SIGNATURE                                            0x464D4520
#define ENHMETA_STOCK_OBJECT                                         0x80000000

#define EMR_HEADER                                                   1
#define EMR_POLYBEZIER                                               2
#define EMR_POLYGON                                                  3
#define EMR_POLYLINE                                                 4
#define EMR_POLYBEZIERTO                                             5
#define EMR_POLYLINETO                                               6
#define EMR_POLYPOLYLINE                                             7
#define EMR_POLYPOLYGON                                              8
#define EMR_SETWINDOWEXTEX                                           9
#define EMR_SETWINDOWORGEX                                           10
#define EMR_SETVIEWPORTEXTEX                                         11
#define EMR_SETVIEWPORTORGEX                                         12
#define EMR_SETBRUSHORGEX                                            13
#define EMR_EOF                                                      14
#define EMR_SETPIXELV                                                15
#define EMR_SETMAPPERFLAGS                                           16
#define EMR_SETMAPMODE                                               17
#define EMR_SETBKMODE                                                18
#define EMR_SETPOLYFILLMODE                                          19
#define EMR_SETROP2                                                  20
#define EMR_SETSTRETCHBLTMODE                                        21
#define EMR_SETTEXTALIGN                                             22
#define EMR_SETCOLORADJUSTMENT                                       23
#define EMR_SETTEXTCOLOR                                             24
#define EMR_SETBKCOLOR                                               25
#define EMR_OFFSETCLIPRGN                                            26
#define EMR_MOVETOEX                                                 27
#define EMR_SETMETARGN                                               28
#define EMR_EXCLUDECLIPRECT                                          29
#define EMR_INTERSECTCLIPRECT                                        30
#define EMR_SCALEVIEWPORTEXTEX                                       31
#define EMR_SCALEWINDOWEXTEX                                         32
#define EMR_SAVEDC                                                   33
#define EMR_RESTOREDC                                                34
#define EMR_SETWORLDTRANSFORM                                        35
#define EMR_MODIFYWORLDTRANSFORM                                     36
#define EMR_SELECTOBJECT                                             37
#define EMR_CREATEPEN                                                38
#define EMR_CREATEBRUSHINDIRECT                                      39
#define EMR_DELETEOBJECT                                             40
#define EMR_ANGLEARC                                                 41
#define EMR_ELLIPSE                                                  42
#define EMR_RECTANGLE                                                43
#define EMR_ROUNDRECT                                                44
#define EMR_ARC                                                      45
#define EMR_CHORD                                                    46
#define EMR_PIE                                                      47
#define EMR_SELECTPALETTE                                            48
#define EMR_CREATEPALETTE                                            49
#define EMR_SETPALETTEENTRIES                                        50
#define EMR_RESIZEPALETTE                                            51
#define EMR_REALIZEPALETTE                                           52
#define EMR_EXTFLOODFILL                                             53
#define EMR_LINETO                                                   54
#define EMR_ARCTO                                                    55
#define EMR_POLYDRAW                                                 56
#define EMR_SETARCDIRECTION                                          57
#define EMR_SETMITERLIMIT                                            58
#define EMR_BEGINPATH                                                59
#define EMR_ENDPATH                                                  60
#define EMR_CLOSEFIGURE                                              61
#define EMR_FILLPATH                                                 62
#define EMR_STROKEANDFILLPATH                                        63
#define EMR_STROKEPATH                                               64
#define EMR_FLATTENPATH                                              65
#define EMR_WIDENPATH                                                66
#define EMR_SELECTCLIPPATH                                           67
#define EMR_ABORTPATH                                                68

#define EMR_GDICOMMENT                                               70
#define EMR_FILLRGN                                                  71
#define EMR_FRAMERGN                                                 72
#define EMR_INVERTRGN                                                73
#define EMR_PAINTRGN                                                 74
#define EMR_EXTSELECTCLIPRGN                                         75
#define EMR_BITBLT                                                   76
#define EMR_STRETCHBLT                                               77
#define EMR_MASKBLT                                                  78
#define EMR_PLGBLT                                                   79
#define EMR_SETDIBITSTODEVICE                                        80
#define EMR_STRETCHDIBITS                                            81
#define EMR_EXTCREATEFONTINDIRECTW                                   82
#define EMR_EXTTEXTOUTA                                              83
#define EMR_EXTTEXTOUTW                                              84
#define EMR_POLYBEZIER16                                             85
#define EMR_POLYGON16                                                86
#define EMR_POLYLINE16                                               87
#define EMR_POLYBEZIERTO16                                           88
#define EMR_POLYLINETO16                                             89
#define EMR_POLYPOLYLINE16                                           90
#define EMR_POLYPOLYGON16                                            91
#define EMR_POLYDRAW16                                               92
#define EMR_CREATEMONOBRUSH                                          93
#define EMR_CREATEDIBPATTERNBRUSHPT                                  94
#define EMR_EXTCREATEPEN                                             95
#define EMR_POLYTEXTOUTA                                             96
#define EMR_POLYTEXTOUTW                                             97

#define EMR_SETICMMODE                                               98
#define EMR_CREATECOLORSPACE                                         99
#define EMR_SETCOLORSPACE                                            100
#define EMR_DELETECOLORSPACE                                         101
#define EMR_GLSRECORD                                                102
#define EMR_GLSBOUNDEDRECORD                                         103
#define EMR_PIXELFORMAT                                              104
#define EMR_RESERVED_105                                             105
#define EMR_RESERVED_106                                             106
#define EMR_RESERVED_107                                             107
#define EMR_RESERVED_108                                             108
#define EMR_RESERVED_109                                             109
#define EMR_RESERVED_110                                             110
#define EMR_COLORCORRECTPALETTE                                      111
#define EMR_SETICMPROFILEA                                           112
#define EMR_SETICMPROFILEW                                           113
#define EMR_ALPHABLEND                                               114
#define EMR_SETLAYOUT                                                115
#define EMR_TRANSPARENTBLT                                           116
#define EMR_RESERVED_117                                             117
#define EMR_GRADIENTFILL                                             118
#define EMR_RESERVED_119                                             119
#define EMR_RESERVED_120                                             120
#define EMR_COLORMATCHTOTARGETW                                      121
#define EMR_CREATECOLORSPACEW                                        122

#define EMR_MIN                                                      1

#define EMR_MAX                                                      122

#define SETICMPROFILE_EMBEDED                                        0x00000001

#define CREATECOLORSPACE_EMBEDED                                     0x00000001

#define COLORMATCHTOTARGET_EMBEDED                                   0x00000001

#define GDICOMMENT_IDENTIFIER                                        0x43494447
#define GDICOMMENT_WINDOWS_METAFILE                                  0x80000001
#define GDICOMMENT_BEGINGROUP                                        0x00000002
#define GDICOMMENT_ENDGROUP                                          0x00000003
#define GDICOMMENT_MULTIFORMATS                                      0x40000004
#define EPS_SIGNATURE                                                0x46535045
#define GDICOMMENT_UNICODE_STRING                                    0x00000040
#define GDICOMMENT_UNICODE_END                                       0x00000080
#endif

// #define wglUseFontBitmaps                                            __MINGW_NAME_AW(wglUseFontBitmaps)

#define WGL_FONT_LINES                                               0
#define WGL_FONT_POLYGONS                                            1

// #define wglUseFontOutlines                                           __MINGW_NAME_AW(wglUseFontOutlines)

#define LPD_DOUBLEBUFFER                                             0x00000001
#define LPD_STEREO                                                   0x00000002
#define LPD_SUPPORT_GDI                                              0x00000010
#define LPD_SUPPORT_OPENGL                                           0x00000020
#define LPD_SHARE_DEPTH                                              0x00000040
#define LPD_SHARE_STENCIL                                            0x00000080
#define LPD_SHARE_ACCUM                                              0x00000100
#define LPD_SWAP_EXCHANGE                                            0x00000200
#define LPD_SWAP_COPY                                                0x00000400
#define LPD_TRANSPARENT                                              0x00001000

#define LPD_TYPE_RGBA                                                0
#define LPD_TYPE_COLORINDEX                                          1

#define WGL_SWAP_MAIN_PLANE                                          0x00000001
#define WGL_SWAP_OVERLAY1                                            0x00000002
#define WGL_SWAP_OVERLAY2                                            0x00000004
#define WGL_SWAP_OVERLAY3                                            0x00000008
#define WGL_SWAP_OVERLAY4                                            0x00000010
#define WGL_SWAP_OVERLAY5                                            0x00000020
#define WGL_SWAP_OVERLAY6                                            0x00000040
#define WGL_SWAP_OVERLAY7                                            0x00000080
#define WGL_SWAP_OVERLAY8                                            0x00000100
#define WGL_SWAP_OVERLAY9                                            0x00000200
#define WGL_SWAP_OVERLAY10                                           0x00000400
#define WGL_SWAP_OVERLAY11                                           0x00000800
#define WGL_SWAP_OVERLAY12                                           0x00001000
#define WGL_SWAP_OVERLAY13                                           0x00002000
#define WGL_SWAP_OVERLAY14                                           0x00004000
#define WGL_SWAP_OVERLAY15                                           0x00008000
#define WGL_SWAP_UNDERLAY1                                           0x00010000
#define WGL_SWAP_UNDERLAY2                                           0x00020000
#define WGL_SWAP_UNDERLAY3                                           0x00040000
#define WGL_SWAP_UNDERLAY4                                           0x00080000
#define WGL_SWAP_UNDERLAY5                                           0x00100000
#define WGL_SWAP_UNDERLAY6                                           0x00200000
#define WGL_SWAP_UNDERLAY7                                           0x00400000
#define WGL_SWAP_UNDERLAY8                                           0x00800000
#define WGL_SWAP_UNDERLAY9                                           0x01000000
#define WGL_SWAP_UNDERLAY10                                          0x02000000
#define WGL_SWAP_UNDERLAY11                                          0x04000000
#define WGL_SWAP_UNDERLAY12                                          0x08000000
#define WGL_SWAP_UNDERLAY13                                          0x10000000
#define WGL_SWAP_UNDERLAY14                                          0x20000000
#define WGL_SWAP_UNDERLAY15                                          0x40000000

#define WGL_SWAPMULTIPLE_MAX                                         16

// #endif

// #endif /* WINAPI_PARTITION_DESKTOP */

// #ifdef __cplusplus
// #endif

#endif /* _WINAPI_WINGDI_ */
