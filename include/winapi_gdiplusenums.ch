/*

  WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI

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

#ifndef __GDIPLUS_ENUMS_CH
#define Gdiplus___GDIPLUS_ENUMS_CH

// enum BrushType
#define Gdiplus_BrushTypeSolidColor                         0
#define Gdiplus_BrushTypeHatchFill                          1
#define Gdiplus_BrushTypeTextureFill                        2
#define Gdiplus_BrushTypePathGradient                       3
#define Gdiplus_BrushTypeLinearGradient                     4

// enum CombineMode
#define Gdiplus_CombineModeReplace                          0
#define Gdiplus_CombineModeIntersect                        1
#define Gdiplus_CombineModeUnion                            2
#define Gdiplus_CombineModeXor                              3
#define Gdiplus_CombineModeExclude                          4
#define Gdiplus_CombineModeComplement                       5

// enum CompositingMode
#define Gdiplus_CompositingModeSourceOver                   0
#define Gdiplus_CompositingModeSourceCopy                   1

// enum CompositingQuality
#define Gdiplus_CompositingQualityDefault                   0
#define Gdiplus_CompositingQualityHighSpeed                 1
#define Gdiplus_CompositingQualityHighQuality               2
#define Gdiplus_CompositingQualityGammaCorrected            3
#define Gdiplus_CompositingQualityAssumeLinear              4

// enum CoordinateSpace
#define Gdiplus_CoordinateSpaceWorld                        0
#define Gdiplus_CoordinateSpacePage                         1
#define Gdiplus_CoordinateSpaceDevice                       2

// enum CustomLineCapType
#define Gdiplus_CustomLineCapTypeDefault                    0
#define Gdiplus_CustomLineCapTypeAdjustableArrow            1

// enum DashCap
#define Gdiplus_DashCapFlat                                 0
#define Gdiplus_DashCapRound                                2
#define Gdiplus_DashCapTriangle                             3

// enum DashStyle
#define Gdiplus_DashStyleSolid                              0
#define Gdiplus_DashStyleDash                               1
#define Gdiplus_DashStyleDot                                2
#define Gdiplus_DashStyleDashDot                            3
#define Gdiplus_DashStyleDashDotDot                         4
#define Gdiplus_DashStyleCustom                             5

// enum DitherType
#define Gdiplus_DitherTypeNone                              0
#define Gdiplus_DitherTypeSolid                             1
#define Gdiplus_DitherTypeOrdered4x4                        2
#define Gdiplus_DitherTypeOrdered8x8                        3
#define Gdiplus_DitherTypeOrdered16x16                      4
#define Gdiplus_DitherTypeOrdered91x91                      5
#define Gdiplus_DitherTypeSpiral4x4                         6
#define Gdiplus_DitherTypeSpiral8x8                         7
#define Gdiplus_DitherTypeDualSpiral4x4                     8
#define Gdiplus_DitherTypeDualSpiral8x8                     9
#define Gdiplus_DitherTypeErrorDiffusion                    10

// enum DriverStringOptions
#define Gdiplus_DriverStringOptionsCmapLookup               1
#define Gdiplus_DriverStringOptionsVertical                 2
#define Gdiplus_DriverStringOptionsRealizedAdvance          4
#define Gdiplus_DriverStringOptionsLimitSubpixel            8

//#define Gdiplus_GDIP_WMF_RECORD_TO_EMFPLUS(meta)          ((meta) | 0x10000)
#define Gdiplus_GDIP_WMF_RECORD_TO_EMFPLUS(meta)            hb_bitor(meta, 0x10000)
#define Gdiplus_GDIP_EMFPLUS_RECORD_BASE                    (0x4000)

// enum EmfPlusRecordType
#define Gdiplus_WmfRecordTypeSetBkColor                     GDIP_WMF_RECORD_TO_EMFPLUS(META_SETBKCOLOR)
#define Gdiplus_WmfRecordTypeSetBkMode                      GDIP_WMF_RECORD_TO_EMFPLUS(META_SETBKMODE)
#define Gdiplus_WmfRecordTypeSetMapMode                     GDIP_WMF_RECORD_TO_EMFPLUS(META_SETMAPMODE)
#define Gdiplus_WmfRecordTypeSetROP2                        GDIP_WMF_RECORD_TO_EMFPLUS(META_SETROP2)
#define Gdiplus_WmfRecordTypeSetRelAbs                      GDIP_WMF_RECORD_TO_EMFPLUS(META_SETRELABS)
#define Gdiplus_WmfRecordTypeSetPolyFillMode                GDIP_WMF_RECORD_TO_EMFPLUS(META_SETPOLYFILLMODE)
#define Gdiplus_WmfRecordTypeSetStretchBltMode              GDIP_WMF_RECORD_TO_EMFPLUS(META_SETSTRETCHBLTMODE)
#define Gdiplus_WmfRecordTypeSetTextCharExtra               GDIP_WMF_RECORD_TO_EMFPLUS(META_SETTEXTCHAREXTRA)
#define Gdiplus_WmfRecordTypeSetTextColor                   GDIP_WMF_RECORD_TO_EMFPLUS(META_SETTEXTCOLOR)
#define Gdiplus_WmfRecordTypeSetTextJustification           GDIP_WMF_RECORD_TO_EMFPLUS(META_SETTEXTJUSTIFICATION)
#define Gdiplus_WmfRecordTypeSetWindowOrg                   GDIP_WMF_RECORD_TO_EMFPLUS(META_SETWINDOWORG)
#define Gdiplus_WmfRecordTypeSetWindowExt                   GDIP_WMF_RECORD_TO_EMFPLUS(META_SETWINDOWEXT)
#define Gdiplus_WmfRecordTypeSetViewportOrg                 GDIP_WMF_RECORD_TO_EMFPLUS(META_SETVIEWPORTORG)
#define Gdiplus_WmfRecordTypeSetViewportExt                 GDIP_WMF_RECORD_TO_EMFPLUS(META_SETVIEWPORTEXT)
#define Gdiplus_WmfRecordTypeOffsetWindowOrg                GDIP_WMF_RECORD_TO_EMFPLUS(META_OFFSETWINDOWORG)
#define Gdiplus_WmfRecordTypeScaleWindowExt                 GDIP_WMF_RECORD_TO_EMFPLUS(META_SCALEWINDOWEXT)
#define Gdiplus_WmfRecordTypeOffsetViewportOrg              GDIP_WMF_RECORD_TO_EMFPLUS(META_OFFSETVIEWPORTORG)
#define Gdiplus_WmfRecordTypeScaleViewportExt               GDIP_WMF_RECORD_TO_EMFPLUS(META_SCALEVIEWPORTEXT)
#define Gdiplus_WmfRecordTypeLineTo                         GDIP_WMF_RECORD_TO_EMFPLUS(META_LINETO)
#define Gdiplus_WmfRecordTypeMoveTo                         GDIP_WMF_RECORD_TO_EMFPLUS(META_MOVETO)
#define Gdiplus_WmfRecordTypeExcludeClipRect                GDIP_WMF_RECORD_TO_EMFPLUS(META_EXCLUDECLIPRECT)
#define Gdiplus_WmfRecordTypeIntersectClipRect              GDIP_WMF_RECORD_TO_EMFPLUS(META_INTERSECTCLIPRECT)
#define Gdiplus_WmfRecordTypeArc                            GDIP_WMF_RECORD_TO_EMFPLUS(META_ARC)
#define Gdiplus_WmfRecordTypeEllipse                        GDIP_WMF_RECORD_TO_EMFPLUS(META_ELLIPSE)
#define Gdiplus_WmfRecordTypeFloodFill                      GDIP_WMF_RECORD_TO_EMFPLUS(META_FLOODFILL)
#define Gdiplus_WmfRecordTypePie                            GDIP_WMF_RECORD_TO_EMFPLUS(META_PIE)
#define Gdiplus_WmfRecordTypeRectangle                      GDIP_WMF_RECORD_TO_EMFPLUS(META_RECTANGLE)
#define Gdiplus_WmfRecordTypeRoundRect                      GDIP_WMF_RECORD_TO_EMFPLUS(META_ROUNDRECT)
#define Gdiplus_WmfRecordTypePatBlt                         GDIP_WMF_RECORD_TO_EMFPLUS(META_PATBLT)
#define Gdiplus_WmfRecordTypeSaveDC                         GDIP_WMF_RECORD_TO_EMFPLUS(META_SAVEDC)
#define Gdiplus_WmfRecordTypeSetPixel                       GDIP_WMF_RECORD_TO_EMFPLUS(META_SETPIXEL)
#define Gdiplus_WmfRecordTypeOffsetClipRgn                  GDIP_WMF_RECORD_TO_EMFPLUS(META_OFFSETCLIPRGN)
#define Gdiplus_WmfRecordTypeTextOut                        GDIP_WMF_RECORD_TO_EMFPLUS(META_TEXTOUT)
#define Gdiplus_WmfRecordTypeBitBlt                         GDIP_WMF_RECORD_TO_EMFPLUS(META_BITBLT)
#define Gdiplus_WmfRecordTypeStretchBlt                     GDIP_WMF_RECORD_TO_EMFPLUS(META_STRETCHBLT)
#define Gdiplus_WmfRecordTypePolygon                        GDIP_WMF_RECORD_TO_EMFPLUS(META_POLYGON)
#define Gdiplus_WmfRecordTypePolyline                       GDIP_WMF_RECORD_TO_EMFPLUS(META_POLYLINE)
#define Gdiplus_WmfRecordTypeEscape                         GDIP_WMF_RECORD_TO_EMFPLUS(META_ESCAPE)
#define Gdiplus_WmfRecordTypeRestoreDC                      GDIP_WMF_RECORD_TO_EMFPLUS(META_RESTOREDC)
#define Gdiplus_WmfRecordTypeFillRegion                     GDIP_WMF_RECORD_TO_EMFPLUS(META_FILLREGION)
#define Gdiplus_WmfRecordTypeFrameRegion                    GDIP_WMF_RECORD_TO_EMFPLUS(META_FRAMEREGION)
#define Gdiplus_WmfRecordTypeInvertRegion                   GDIP_WMF_RECORD_TO_EMFPLUS(META_INVERTREGION)
#define Gdiplus_WmfRecordTypePaintRegion                    GDIP_WMF_RECORD_TO_EMFPLUS(META_PAINTREGION)
#define Gdiplus_WmfRecordTypeSelectClipRegion               GDIP_WMF_RECORD_TO_EMFPLUS(META_SELECTCLIPREGION)
#define Gdiplus_WmfRecordTypeSelectObject                   GDIP_WMF_RECORD_TO_EMFPLUS(META_SELECTOBJECT)
#define Gdiplus_WmfRecordTypeSetTextAlign                   GDIP_WMF_RECORD_TO_EMFPLUS(META_SETTEXTALIGN)
#define Gdiplus_WmfRecordTypeDrawText                       GDIP_WMF_RECORD_TO_EMFPLUS(0x062F)
#define Gdiplus_WmfRecordTypeChord                          GDIP_WMF_RECORD_TO_EMFPLUS(META_CHORD)
#define Gdiplus_WmfRecordTypeSetMapperFlags                 GDIP_WMF_RECORD_TO_EMFPLUS(META_SETMAPPERFLAGS)
#define Gdiplus_WmfRecordTypeExtTextOut                     GDIP_WMF_RECORD_TO_EMFPLUS(META_EXTTEXTOUT)
#define Gdiplus_WmfRecordTypeSetDIBToDev                    GDIP_WMF_RECORD_TO_EMFPLUS(META_SETDIBTODEV)
#define Gdiplus_WmfRecordTypeSelectPalette                  GDIP_WMF_RECORD_TO_EMFPLUS(META_SELECTPALETTE)
#define Gdiplus_WmfRecordTypeRealizePalette                 GDIP_WMF_RECORD_TO_EMFPLUS(META_REALIZEPALETTE)
#define Gdiplus_WmfRecordTypeAnimatePalette                 GDIP_WMF_RECORD_TO_EMFPLUS(META_ANIMATEPALETTE)
#define Gdiplus_WmfRecordTypeSetPalEntries                  GDIP_WMF_RECORD_TO_EMFPLUS(META_SETPALENTRIES)
#define Gdiplus_WmfRecordTypePolyPolygon                    GDIP_WMF_RECORD_TO_EMFPLUS(META_POLYPOLYGON)
#define Gdiplus_WmfRecordTypeResizePalette                  GDIP_WMF_RECORD_TO_EMFPLUS(META_RESIZEPALETTE)
#define Gdiplus_WmfRecordTypeDIBBitBlt                      GDIP_WMF_RECORD_TO_EMFPLUS(META_DIBBITBLT)
#define Gdiplus_WmfRecordTypeDIBStretchBlt                  GDIP_WMF_RECORD_TO_EMFPLUS(META_DIBSTRETCHBLT)
#define Gdiplus_WmfRecordTypeDIBCreatePatternBrush          GDIP_WMF_RECORD_TO_EMFPLUS(META_DIBCREATEPATTERNBRUSH)
#define Gdiplus_WmfRecordTypeStretchDIB                     GDIP_WMF_RECORD_TO_EMFPLUS(META_STRETCHDIB)
#define Gdiplus_WmfRecordTypeExtFloodFill                   GDIP_WMF_RECORD_TO_EMFPLUS(META_EXTFLOODFILL)
#define Gdiplus_WmfRecordTypeSetLayout                      GDIP_WMF_RECORD_TO_EMFPLUS(0x0149)
#define Gdiplus_WmfRecordTypeResetDC                        GDIP_WMF_RECORD_TO_EMFPLUS(0x014C)
#define Gdiplus_WmfRecordTypeStartDoc                       GDIP_WMF_RECORD_TO_EMFPLUS(0x014D)
#define Gdiplus_WmfRecordTypeStartPage                      GDIP_WMF_RECORD_TO_EMFPLUS(0x004F)
#define Gdiplus_WmfRecordTypeEndPage                        GDIP_WMF_RECORD_TO_EMFPLUS(0x0050)
#define Gdiplus_WmfRecordTypeAbortDoc                       GDIP_WMF_RECORD_TO_EMFPLUS(0x0052)
#define Gdiplus_WmfRecordTypeEndDoc                         GDIP_WMF_RECORD_TO_EMFPLUS(0x005E)
#define Gdiplus_WmfRecordTypeDeleteObject                   GDIP_WMF_RECORD_TO_EMFPLUS(META_DELETEOBJECT)
#define Gdiplus_WmfRecordTypeCreatePalette                  GDIP_WMF_RECORD_TO_EMFPLUS(META_CREATEPALETTE)
#define Gdiplus_WmfRecordTypeCreateBrush                    GDIP_WMF_RECORD_TO_EMFPLUS(0x00F8)
#define Gdiplus_WmfRecordTypeCreatePatternBrush             GDIP_WMF_RECORD_TO_EMFPLUS(META_CREATEPATTERNBRUSH)
#define Gdiplus_WmfRecordTypeCreatePenIndirect              GDIP_WMF_RECORD_TO_EMFPLUS(META_CREATEPENINDIRECT)
#define Gdiplus_WmfRecordTypeCreateFontIndirect             GDIP_WMF_RECORD_TO_EMFPLUS(META_CREATEFONTINDIRECT)
#define Gdiplus_WmfRecordTypeCreateBrushIndirect            GDIP_WMF_RECORD_TO_EMFPLUS(META_CREATEBRUSHINDIRECT)
#define Gdiplus_WmfRecordTypeCreateBitmapIndirect           GDIP_WMF_RECORD_TO_EMFPLUS(0x02FD)
#define Gdiplus_WmfRecordTypeCreateBitmap                   GDIP_WMF_RECORD_TO_EMFPLUS(0x06FE)
#define Gdiplus_WmfRecordTypeCreateRegion                   GDIP_WMF_RECORD_TO_EMFPLUS(META_CREATEREGION)
#define Gdiplus_EmfRecordTypeHeader                         EMR_HEADER
#define Gdiplus_EmfRecordTypePolyBezier                     EMR_POLYBEZIER
#define Gdiplus_EmfRecordTypePolygon                        EMR_POLYGON
#define Gdiplus_EmfRecordTypePolyline                       EMR_POLYLINE
#define Gdiplus_EmfRecordTypePolyBezierTo                   EMR_POLYBEZIERTO
#define Gdiplus_EmfRecordTypePolyLineTo                     EMR_POLYLINETO
#define Gdiplus_EmfRecordTypePolyPolyline                   EMR_POLYPOLYLINE
#define Gdiplus_EmfRecordTypePolyPolygon                    EMR_POLYPOLYGON
#define Gdiplus_EmfRecordTypeSetWindowExtEx                 EMR_SETWINDOWEXTEX
#define Gdiplus_EmfRecordTypeSetWindowOrgEx                 EMR_SETWINDOWORGEX
#define Gdiplus_EmfRecordTypeSetViewportExtEx               EMR_SETVIEWPORTEXTEX
#define Gdiplus_EmfRecordTypeSetViewportOrgEx               EMR_SETVIEWPORTORGEX
#define Gdiplus_EmfRecordTypeSetBrushOrgEx                  EMR_SETBRUSHORGEX
#define Gdiplus_EmfRecordTypeEOF                            EMR_EOF
#define Gdiplus_EmfRecordTypeSetPixelV                      EMR_SETPIXELV
#define Gdiplus_EmfRecordTypeSetMapperFlags                 EMR_SETMAPPERFLAGS
#define Gdiplus_EmfRecordTypeSetMapMode                     EMR_SETMAPMODE
#define Gdiplus_EmfRecordTypeSetBkMode                      EMR_SETBKMODE
#define Gdiplus_EmfRecordTypeSetPolyFillMode                EMR_SETPOLYFILLMODE
#define Gdiplus_EmfRecordTypeSetROP2                        EMR_SETROP2
#define Gdiplus_EmfRecordTypeSetStretchBltMode              EMR_SETSTRETCHBLTMODE
#define Gdiplus_EmfRecordTypeSetTextAlign                   EMR_SETTEXTALIGN
#define Gdiplus_EmfRecordTypeSetColorAdjustment             EMR_SETCOLORADJUSTMENT
#define Gdiplus_EmfRecordTypeSetTextColor                   EMR_SETTEXTCOLOR
#define Gdiplus_EmfRecordTypeSetBkColor                     EMR_SETBKCOLOR
#define Gdiplus_EmfRecordTypeOffsetClipRgn                  EMR_OFFSETCLIPRGN
#define Gdiplus_EmfRecordTypeMoveToEx                       EMR_MOVETOEX
#define Gdiplus_EmfRecordTypeSetMetaRgn                     EMR_SETMETARGN
#define Gdiplus_EmfRecordTypeExcludeClipRect                EMR_EXCLUDECLIPRECT
#define Gdiplus_EmfRecordTypeIntersectClipRect              EMR_INTERSECTCLIPRECT
#define Gdiplus_EmfRecordTypeScaleViewportExtEx             EMR_SCALEVIEWPORTEXTEX
#define Gdiplus_EmfRecordTypeScaleWindowExtEx               EMR_SCALEWINDOWEXTEX
#define Gdiplus_EmfRecordTypeSaveDC                         EMR_SAVEDC
#define Gdiplus_EmfRecordTypeRestoreDC                      EMR_RESTOREDC
#define Gdiplus_EmfRecordTypeSetWorldTransform              EMR_SETWORLDTRANSFORM
#define Gdiplus_EmfRecordTypeModifyWorldTransform           EMR_MODIFYWORLDTRANSFORM
#define Gdiplus_EmfRecordTypeSelectObject                   EMR_SELECTOBJECT
#define Gdiplus_EmfRecordTypeCreatePen                      EMR_CREATEPEN
#define Gdiplus_EmfRecordTypeCreateBrushIndirect            EMR_CREATEBRUSHINDIRECT
#define Gdiplus_EmfRecordTypeDeleteObject                   EMR_DELETEOBJECT
#define Gdiplus_EmfRecordTypeAngleArc                       EMR_ANGLEARC
#define Gdiplus_EmfRecordTypeEllipse                        EMR_ELLIPSE
#define Gdiplus_EmfRecordTypeRectangle                      EMR_RECTANGLE
#define Gdiplus_EmfRecordTypeRoundRect                      EMR_ROUNDRECT
#define Gdiplus_EmfRecordTypeArc                            EMR_ARC
#define Gdiplus_EmfRecordTypeChord                          EMR_CHORD
#define Gdiplus_EmfRecordTypePie                            EMR_PIE
#define Gdiplus_EmfRecordTypeSelectPalette                  EMR_SELECTPALETTE
#define Gdiplus_EmfRecordTypeCreatePalette                  EMR_CREATEPALETTE
#define Gdiplus_EmfRecordTypeSetPaletteEntries              EMR_SETPALETTEENTRIES
#define Gdiplus_EmfRecordTypeResizePalette                  EMR_RESIZEPALETTE
#define Gdiplus_EmfRecordTypeRealizePalette                 EMR_REALIZEPALETTE
#define Gdiplus_EmfRecordTypeExtFloodFill                   EMR_EXTFLOODFILL
#define Gdiplus_EmfRecordTypeLineTo                         EMR_LINETO
#define Gdiplus_EmfRecordTypeArcTo                          EMR_ARCTO
#define Gdiplus_EmfRecordTypePolyDraw                       EMR_POLYDRAW
#define Gdiplus_EmfRecordTypeSetArcDirection                EMR_SETARCDIRECTION
#define Gdiplus_EmfRecordTypeSetMiterLimit                  EMR_SETMITERLIMIT
#define Gdiplus_EmfRecordTypeBeginPath                      EMR_BEGINPATH
#define Gdiplus_EmfRecordTypeEndPath                        EMR_ENDPATH
#define Gdiplus_EmfRecordTypeCloseFigure                    EMR_CLOSEFIGURE
#define Gdiplus_EmfRecordTypeFillPath                       EMR_FILLPATH
#define Gdiplus_EmfRecordTypeStrokeAndFillPath              EMR_STROKEANDFILLPATH
#define Gdiplus_EmfRecordTypeStrokePath                     EMR_STROKEPATH
#define Gdiplus_EmfRecordTypeFlattenPath                    EMR_FLATTENPATH
#define Gdiplus_EmfRecordTypeWidenPath                      EMR_WIDENPATH
#define Gdiplus_EmfRecordTypeSelectClipPath                 EMR_SELECTCLIPPATH
#define Gdiplus_EmfRecordTypeAbortPath                      EMR_ABORTPATH
#define Gdiplus_EmfRecordTypeReserved_069                   69
#define Gdiplus_EmfRecordTypeGdiComment                     EMR_GDICOMMENT
#define Gdiplus_EmfRecordTypeFillRgn                        EMR_FILLRGN
#define Gdiplus_EmfRecordTypeFrameRgn                       EMR_FRAMERGN
#define Gdiplus_EmfRecordTypeInvertRgn                      EMR_INVERTRGN
#define Gdiplus_EmfRecordTypePaintRgn                       EMR_PAINTRGN
#define Gdiplus_EmfRecordTypeExtSelectClipRgn               EMR_EXTSELECTCLIPRGN
#define Gdiplus_EmfRecordTypeBitBlt                         EMR_BITBLT
#define Gdiplus_EmfRecordTypeStretchBlt                     EMR_STRETCHBLT
#define Gdiplus_EmfRecordTypeMaskBlt                        EMR_MASKBLT
#define Gdiplus_EmfRecordTypePlgBlt                         EMR_PLGBLT
#define Gdiplus_EmfRecordTypeSetDIBitsToDevice              EMR_SETDIBITSTODEVICE
#define Gdiplus_EmfRecordTypeStretchDIBits                  EMR_STRETCHDIBITS
#define Gdiplus_EmfRecordTypeExtCreateFontIndirect          EMR_EXTCREATEFONTINDIRECTW
#define Gdiplus_EmfRecordTypeExtTextOutA                    EMR_EXTTEXTOUTA
#define Gdiplus_EmfRecordTypeExtTextOutW                    EMR_EXTTEXTOUTW
#define Gdiplus_EmfRecordTypePolyBezier16                   EMR_POLYBEZIER16
#define Gdiplus_EmfRecordTypePolygon16                      EMR_POLYGON16
#define Gdiplus_EmfRecordTypePolyline16                     EMR_POLYLINE16
#define Gdiplus_EmfRecordTypePolyBezierTo16                 EMR_POLYBEZIERTO16
#define Gdiplus_EmfRecordTypePolylineTo16                   EMR_POLYLINETO16
#define Gdiplus_EmfRecordTypePolyPolyline16                 EMR_POLYPOLYLINE16
#define Gdiplus_EmfRecordTypePolyPolygon16                  EMR_POLYPOLYGON16
#define Gdiplus_EmfRecordTypePolyDraw16                     EMR_POLYDRAW16
#define Gdiplus_EmfRecordTypeCreateMonoBrush                EMR_CREATEMONOBRUSH
#define Gdiplus_EmfRecordTypeCreateDIBPatternBrushPt        EMR_CREATEDIBPATTERNBRUSHPT
#define Gdiplus_EmfRecordTypeExtCreatePen                   EMR_EXTCREATEPEN
#define Gdiplus_EmfRecordTypePolyTextOutA                   EMR_POLYTEXTOUTA
#define Gdiplus_EmfRecordTypePolyTextOutW                   EMR_POLYTEXTOUTW
#define Gdiplus_EmfRecordTypeSetICMMode                     98
#define Gdiplus_EmfRecordTypeCreateColorSpace               99
#define Gdiplus_EmfRecordTypeSetColorSpace                  100
#define Gdiplus_EmfRecordTypeDeleteColorSpace               101
#define Gdiplus_EmfRecordTypeGLSRecord                      102
#define Gdiplus_EmfRecordTypeGLSBoundedRecord               103
#define Gdiplus_EmfRecordTypePixelFormat                    104
#define Gdiplus_EmfRecordTypeDrawEscape                     105
#define Gdiplus_EmfRecordTypeExtEscape                      106
#define Gdiplus_EmfRecordTypeStartDoc                       107
#define Gdiplus_EmfRecordTypeSmallTextOut                   108
#define Gdiplus_EmfRecordTypeForceUFIMapping                109
#define Gdiplus_EmfRecordTypeNamedEscape                    110
#define Gdiplus_EmfRecordTypeColorCorrectPalette            111
#define Gdiplus_EmfRecordTypeSetICMProfileA                 112
#define Gdiplus_EmfRecordTypeSetICMProfileW                 113
#define Gdiplus_EmfRecordTypeAlphaBlend                     114
#define Gdiplus_EmfRecordTypeSetLayout                      115
#define Gdiplus_EmfRecordTypeTransparentBlt                 116
#define Gdiplus_EmfRecordTypeReserved_117                   117
#define Gdiplus_EmfRecordTypeGradientFill                   118
#define Gdiplus_EmfRecordTypeSetLinkedUFIs                  119
#define Gdiplus_EmfRecordTypeSetTextJustification           120
#define Gdiplus_EmfRecordTypeColorMatchToTargetW            121
#define Gdiplus_EmfRecordTypeCreateColorSpaceW              122
#define Gdiplus_EmfRecordTypeMax                            122
#define Gdiplus_EmfRecordTypeMin                            1
#define Gdiplus_EmfPlusRecordTypeInvalid                    GDIP_EMFPLUS_RECORD_BASE
#define Gdiplus_EmfPlusRecordTypeHeader                     (GDIP_EMFPLUS_RECORD_BASE + 1)
#define Gdiplus_EmfPlusRecordTypeEndOfFile                  (GDIP_EMFPLUS_RECORD_BASE + 2)
#define Gdiplus_EmfPlusRecordTypeComment                    (GDIP_EMFPLUS_RECORD_BASE + 3)
#define Gdiplus_EmfPlusRecordTypeGetDC                      (GDIP_EMFPLUS_RECORD_BASE + 4)
#define Gdiplus_EmfPlusRecordTypeMultiFormatStart           (GDIP_EMFPLUS_RECORD_BASE + 5)
#define Gdiplus_EmfPlusRecordTypeMultiFormatSection         (GDIP_EMFPLUS_RECORD_BASE + 6)
#define Gdiplus_EmfPlusRecordTypeMultiFormatEnd             (GDIP_EMFPLUS_RECORD_BASE + 7)
#define Gdiplus_EmfPlusRecordTypeObject                     (GDIP_EMFPLUS_RECORD_BASE + 8)
#define Gdiplus_EmfPlusRecordTypeClear                      (GDIP_EMFPLUS_RECORD_BASE + 9)
#define Gdiplus_EmfPlusRecordTypeFillRects                  (GDIP_EMFPLUS_RECORD_BASE + 10)
#define Gdiplus_EmfPlusRecordTypeDrawRects                  (GDIP_EMFPLUS_RECORD_BASE + 11)
#define Gdiplus_EmfPlusRecordTypeFillPolygon                (GDIP_EMFPLUS_RECORD_BASE + 12)
#define Gdiplus_EmfPlusRecordTypeDrawLines                  (GDIP_EMFPLUS_RECORD_BASE + 13)
#define Gdiplus_EmfPlusRecordTypeFillEllipse                (GDIP_EMFPLUS_RECORD_BASE + 14)
#define Gdiplus_EmfPlusRecordTypeDrawEllipse                (GDIP_EMFPLUS_RECORD_BASE + 15)
#define Gdiplus_EmfPlusRecordTypeFillPie                    (GDIP_EMFPLUS_RECORD_BASE + 16)
#define Gdiplus_EmfPlusRecordTypeDrawPie                    (GDIP_EMFPLUS_RECORD_BASE + 17)
#define Gdiplus_EmfPlusRecordTypeDrawArc                    (GDIP_EMFPLUS_RECORD_BASE + 18)
#define Gdiplus_EmfPlusRecordTypeFillRegion                 (GDIP_EMFPLUS_RECORD_BASE + 19)
#define Gdiplus_EmfPlusRecordTypeFillPath                   (GDIP_EMFPLUS_RECORD_BASE + 20)
#define Gdiplus_EmfPlusRecordTypeDrawPath                   (GDIP_EMFPLUS_RECORD_BASE + 21)
#define Gdiplus_EmfPlusRecordTypeFillClosedCurve            (GDIP_EMFPLUS_RECORD_BASE + 22)
#define Gdiplus_EmfPlusRecordTypeDrawClosedCurve            (GDIP_EMFPLUS_RECORD_BASE + 23)
#define Gdiplus_EmfPlusRecordTypeDrawCurve                  (GDIP_EMFPLUS_RECORD_BASE + 24)
#define Gdiplus_EmfPlusRecordTypeDrawBeziers                (GDIP_EMFPLUS_RECORD_BASE + 25)
#define Gdiplus_EmfPlusRecordTypeDrawImage                  (GDIP_EMFPLUS_RECORD_BASE + 26)
#define Gdiplus_EmfPlusRecordTypeDrawImagePoints            (GDIP_EMFPLUS_RECORD_BASE + 27)
#define Gdiplus_EmfPlusRecordTypeDrawString                 (GDIP_EMFPLUS_RECORD_BASE + 28)
#define Gdiplus_EmfPlusRecordTypeSetRenderingOrigin         (GDIP_EMFPLUS_RECORD_BASE + 29)
#define Gdiplus_EmfPlusRecordTypeSetAntiAliasMode           (GDIP_EMFPLUS_RECORD_BASE + 30)
#define Gdiplus_EmfPlusRecordTypeSetTextRenderingHint       (GDIP_EMFPLUS_RECORD_BASE + 31)
#define Gdiplus_EmfPlusRecordTypeSetTextContrast            (GDIP_EMFPLUS_RECORD_BASE + 32)
#define Gdiplus_EmfPlusRecordTypeSetGammaValue              (GDIP_EMFPLUS_RECORD_BASE + 33)
#define Gdiplus_EmfPlusRecordTypeSetInterpolationMode       (GDIP_EMFPLUS_RECORD_BASE + 34)
#define Gdiplus_EmfPlusRecordTypeSetPixelOffsetMode         (GDIP_EMFPLUS_RECORD_BASE + 35)
#define Gdiplus_EmfPlusRecordTypeSetCompositingMode         (GDIP_EMFPLUS_RECORD_BASE + 36)
#define Gdiplus_EmfPlusRecordTypeSetCompositingQuality      (GDIP_EMFPLUS_RECORD_BASE + 37)
#define Gdiplus_EmfPlusRecordTypeSave                       (GDIP_EMFPLUS_RECORD_BASE + 38)
#define Gdiplus_EmfPlusRecordTypeRestore                    (GDIP_EMFPLUS_RECORD_BASE + 39)
#define Gdiplus_EmfPlusRecordTypeBeginContainer             (GDIP_EMFPLUS_RECORD_BASE + 40)
#define Gdiplus_EmfPlusRecordTypeBeginContainerNoParams     (GDIP_EMFPLUS_RECORD_BASE + 41)
#define Gdiplus_EmfPlusRecordTypeEndContainer               (GDIP_EMFPLUS_RECORD_BASE + 42)
#define Gdiplus_EmfPlusRecordTypeSetWorldTransform          (GDIP_EMFPLUS_RECORD_BASE + 43)
#define Gdiplus_EmfPlusRecordTypeResetWorldTransform        (GDIP_EMFPLUS_RECORD_BASE + 44)
#define Gdiplus_EmfPlusRecordTypeMultiplyWorldTransform     (GDIP_EMFPLUS_RECORD_BASE + 45)
#define Gdiplus_EmfPlusRecordTypeTranslateWorldTransform    (GDIP_EMFPLUS_RECORD_BASE + 46)
#define Gdiplus_EmfPlusRecordTypeScaleWorldTransform        (GDIP_EMFPLUS_RECORD_BASE + 47)
#define Gdiplus_EmfPlusRecordTypeRotateWorldTransform       (GDIP_EMFPLUS_RECORD_BASE + 48)
#define Gdiplus_EmfPlusRecordTypeSetPageTransform           (GDIP_EMFPLUS_RECORD_BASE + 49)
#define Gdiplus_EmfPlusRecordTypeResetClip                  (GDIP_EMFPLUS_RECORD_BASE + 50)
#define Gdiplus_EmfPlusRecordTypeSetClipRect                (GDIP_EMFPLUS_RECORD_BASE + 51)
#define Gdiplus_EmfPlusRecordTypeSetClipPath                (GDIP_EMFPLUS_RECORD_BASE + 52)
#define Gdiplus_EmfPlusRecordTypeSetClipRegion              (GDIP_EMFPLUS_RECORD_BASE + 53)
#define Gdiplus_EmfPlusRecordTypeOffsetClip                 (GDIP_EMFPLUS_RECORD_BASE + 54)
#define Gdiplus_EmfPlusRecordTypeDrawDriverString           (GDIP_EMFPLUS_RECORD_BASE + 55)
#define Gdiplus_EmfPlusRecordTypeStrokeFillPath             (GDIP_EMFPLUS_RECORD_BASE + 56)
#define Gdiplus_EmfPlusRecordTypeSerializableObject         (GDIP_EMFPLUS_RECORD_BASE + 57)
#define Gdiplus_EmfPlusRecordTypeSetTSGraphics              (GDIP_EMFPLUS_RECORD_BASE + 58)
#define Gdiplus_EmfPlusRecordTypeSetTSClip                  (GDIP_EMFPLUS_RECORD_BASE + 59)
#define Gdiplus_EmfPlusRecordTotal                          (GDIP_EMFPLUS_RECORD_BASE + 60)
#define Gdiplus_EmfPlusRecordTypeMax                        (EmfPlusRecordTotal - 1)
#define Gdiplus_EmfPlusRecordTypeMin                        EmfPlusRecordTypeHeader

// enum EmfToWmfBitsFlags
#define Gdiplus_EmfToWmfBitsFlagsDefault                    0
#define Gdiplus_EmfToWmfBitsFlagsEmbedEmf                   1
#define Gdiplus_EmfToWmfBitsFlagsIncludePlaceable           2
#define Gdiplus_EmfToWmfBitsFlagsNoXORClip                  4

// enum EmfType
#define Gdiplus_EmfTypeEmfOnly                              3
#define Gdiplus_EmfTypeEmfPlusOnly                          4
#define Gdiplus_EmfTypeEmfPlusDual                          5

// enum EncoderParameterValueType
#define Gdiplus_EncoderParameterValueTypeByte               1
#define Gdiplus_EncoderParameterValueTypeASCII              2
#define Gdiplus_EncoderParameterValueTypeShort              3
#define Gdiplus_EncoderParameterValueTypeLong               4
#define Gdiplus_EncoderParameterValueTypeRational           5
#define Gdiplus_EncoderParameterValueTypeLongRange          6
#define Gdiplus_EncoderParameterValueTypeUndefined          7
#define Gdiplus_EncoderParameterValueTypeRationalRange      8
#define Gdiplus_EncoderParameterValueTypePointer            9

// enum EncoderValue
#define Gdiplus_EncoderValueColorTypeCMYK                   0
#define Gdiplus_EncoderValueColorTypeYCCK                   1
#define Gdiplus_EncoderValueCompressionLZW                  2
#define Gdiplus_EncoderValueCompressionCCITT3               3
#define Gdiplus_EncoderValueCompressionCCITT4               4
#define Gdiplus_EncoderValueCompressionRle                  5
#define Gdiplus_EncoderValueCompressionNone                 6
#define Gdiplus_EncoderValueScanMethodInterlaced            7
#define Gdiplus_EncoderValueScanMethodNonInterlaced         8
#define Gdiplus_EncoderValueVersionGif87                    9
#define Gdiplus_EncoderValueVersionGif89                    10
#define Gdiplus_EncoderValueRenderProgressive               11
#define Gdiplus_EncoderValueRenderNonProgressive            12
#define Gdiplus_EncoderValueTransformRotate90               13
#define Gdiplus_EncoderValueTransformRotate180              14
#define Gdiplus_EncoderValueTransformRotate270              15
#define Gdiplus_EncoderValueTransformFlipHorizontal         16
#define Gdiplus_EncoderValueTransformFlipVertical           17
#define Gdiplus_EncoderValueMultiFrame                      18
#define Gdiplus_EncoderValueLastFrame                       19
#define Gdiplus_EncoderValueFlush                           20
#define Gdiplus_EncoderValueFrameDimensionTime              21
#define Gdiplus_EncoderValueFrameDimensionResolution        22
#define Gdiplus_EncoderValueFrameDimensionPage              23

// enum FillMode
#define Gdiplus_FillModeAlternate                           0
#define Gdiplus_FillModeWinding                             1

// enum FlushIntention
#define Gdiplus_FlushIntentionFlush                         0
#define Gdiplus_FlushIntentionSync                          1

// enum FontStyle
#define Gdiplus_FontStyleRegular                            0
#define Gdiplus_FontStyleBold                               1
#define Gdiplus_FontStyleItalic                             2
#define Gdiplus_FontStyleBoldItalic                         3
#define Gdiplus_FontStyleUnderline                          4
#define Gdiplus_FontStyleStrikeout                          8

// enum HatchStyle
#define Gdiplus_HatchStyleHorizontal                        0
#define Gdiplus_HatchStyleVertical                          1
#define Gdiplus_HatchStyleForwardDiagonal                   2
#define Gdiplus_HatchStyleBackwardDiagonal                  3
#define Gdiplus_HatchStyleCross                             4
#define Gdiplus_HatchStyleLargeGrid                         4
#define Gdiplus_HatchStyleDiagonalCross                     5
#define Gdiplus_HatchStyle05Percent                         6
#define Gdiplus_HatchStyle10Percent                         7
#define Gdiplus_HatchStyle20Percent                         8
#define Gdiplus_HatchStyle25Percent                         9
#define Gdiplus_HatchStyle30Percent                         10
#define Gdiplus_HatchStyle40Percent                         11
#define Gdiplus_HatchStyle50Percent                         12
#define Gdiplus_HatchStyle60Percent                         13
#define Gdiplus_HatchStyle70Percent                         14
#define Gdiplus_HatchStyle75Percent                         15
#define Gdiplus_HatchStyle80Percent                         16
#define Gdiplus_HatchStyle90Percent                         17
#define Gdiplus_HatchStyleLightDownwardDiagonal             18
#define Gdiplus_HatchStyleLightUpwardDiagonal               19
#define Gdiplus_HatchStyleDarkDownwardDiagonal              20
#define Gdiplus_HatchStyleDarkUpwardDiagonal                21
#define Gdiplus_HatchStyleWideDownwardDiagonal              22
#define Gdiplus_HatchStyleWideUpwardDiagonal                23
#define Gdiplus_HatchStyleLightVertical                     24
#define Gdiplus_HatchStyleLightHorizontal                   25
#define Gdiplus_HatchStyleNarrowVertical                    26
#define Gdiplus_HatchStyleNarrowHorizontal                  27
#define Gdiplus_HatchStyleDarkVertical                      28
#define Gdiplus_HatchStyleDarkHorizontal                    29
#define Gdiplus_HatchStyleDashedDownwardDiagonal            30
#define Gdiplus_HatchStyleDashedUpwardDiagonal              31
#define Gdiplus_HatchStyleDashedHorizontal                  32
#define Gdiplus_HatchStyleDashedVertical                    33
#define Gdiplus_HatchStyleSmallConfetti                     34
#define Gdiplus_HatchStyleLargeConfetti                     35
#define Gdiplus_HatchStyleZigZag                            36
#define Gdiplus_HatchStyleWave                              37
#define Gdiplus_HatchStyleDiagonalBrick                     38
#define Gdiplus_HatchStyleHorizontalBrick                   39
#define Gdiplus_HatchStyleWeave                             40
#define Gdiplus_HatchStylePlaid                             41
#define Gdiplus_HatchStyleDivot                             42
#define Gdiplus_HatchStyleDottedGrid                        43
#define Gdiplus_HatchStyleDottedDiamond                     44
#define Gdiplus_HatchStyleShingle                           45
#define Gdiplus_HatchStyleTrellis                           46
#define Gdiplus_HatchStyleSphere                            47
#define Gdiplus_HatchStyleSmallGrid                         48
#define Gdiplus_HatchStyleSmallCheckerBoard                 49
#define Gdiplus_HatchStyleLargeCheckerBoard                 50
#define Gdiplus_HatchStyleOutlinedDiamond                   51
#define Gdiplus_HatchStyleSolidDiamond                      52
#define Gdiplus_HatchStyleTotal                             53
#define Gdiplus_HatchStyleMin                               HatchStyleHorizontal
#define Gdiplus_HatchStyleMax                               (HatchStyleTotal - 1)

// enum HotkeyPrefix
#define Gdiplus_HotkeyPrefixNone                            0
#define Gdiplus_HotkeyPrefixShow                            1
#define Gdiplus_HotkeyPrefixHide                            2

// enum ImageType
#define Gdiplus_ImageTypeUnknown                            0
#define Gdiplus_ImageTypeBitmap                             1
#define Gdiplus_ImageTypeMetafile                           2

// enum InterpolationMode
#define Gdiplus_InterpolationModeInvalid                    -1
#define Gdiplus_InterpolationModeDefault                    0
#define Gdiplus_InterpolationModeLowQuality                 1
#define Gdiplus_InterpolationModeHighQuality                2
#define Gdiplus_InterpolationModeBilinear                   3
#define Gdiplus_InterpolationModeBicubic                    4
#define Gdiplus_InterpolationModeNearestNeighbor            5
#define Gdiplus_InterpolationModeHighQualityBilinear        6
#define Gdiplus_InterpolationModeHighQualityBicubic         7

// enum LinearGradientMode
#define Gdiplus_LinearGradientModeHorizontal                0
#define Gdiplus_LinearGradientModeVertical                  1
#define Gdiplus_LinearGradientModeForwardDiagonal           2
#define Gdiplus_LinearGradientModeBackwardDiagonal          3

// enum LineCap
#define Gdiplus_LineCapFlat                                 0
#define Gdiplus_LineCapSquare                               1
#define Gdiplus_LineCapRound                                2
#define Gdiplus_LineCapTriangle                             3
#define Gdiplus_LineCapNoAnchor                             16
#define Gdiplus_LineCapSquareAnchor                         17
#define Gdiplus_LineCapRoundAnchor                          18
#define Gdiplus_LineCapDiamondAnchor                        19
#define Gdiplus_LineCapArrowAnchor                          20
#define Gdiplus_LineCapCustom                               255

// enum LineJoin
#define Gdiplus_LineJoinMiter                               0
#define Gdiplus_LineJoinBevel                               1
#define Gdiplus_LineJoinRound                               2
#define Gdiplus_LineJoinMiterClipped                        3

// enum MatrixOrder
#define Gdiplus_MatrixOrderPrepend                          0
#define Gdiplus_MatrixOrderAppend                           1

// enum MetafileFrameUnit
#define Gdiplus_MetafileFrameUnitPixel                      2
#define Gdiplus_MetafileFrameUnitPoint                      3
#define Gdiplus_MetafileFrameUnitInch                       4
#define Gdiplus_MetafileFrameUnitDocument                   5
#define Gdiplus_MetafileFrameUnitMillimeter                 6
#define Gdiplus_MetafileFrameUnitGdi                        7

// enum MetafileType
#define Gdiplus_MetafileTypeInvalid                         0
#define Gdiplus_MetafileTypeWmf                             1
#define Gdiplus_MetafileTypeWmfPlaceable                    2
#define Gdiplus_MetafileTypeEmf                             3
#define Gdiplus_MetafileTypeEmfPlusOnly                     4
#define Gdiplus_MetafileTypeEmfPlusDual                     5

// enum ObjectType
#define Gdiplus_ObjectTypeInvalid                           0
#define Gdiplus_ObjectTypeBrush                             1
#define Gdiplus_ObjectTypePen                               2
#define Gdiplus_ObjectTypePath                              3
#define Gdiplus_ObjectTypeRegion                            4
#define Gdiplus_ObjectTypeFont                              5
#define Gdiplus_ObjectTypeStringFormat                      6
#define Gdiplus_ObjectTypeImageAttributes                   7
#define Gdiplus_ObjectTypeCustomLineCap                     8
#define Gdiplus_ObjectTypeGraphics                          9
#define Gdiplus_ObjectTypeMin                               ObjectTypeBrush
#define Gdiplus_ObjectTypeMax                               ObjectTypeGraphics

// enum PathPointType
#define Gdiplus_PathPointTypeStart                          0x00
#define Gdiplus_PathPointTypeLine                           0x01
#define Gdiplus_PathPointTypeBezier                         0x03
#define Gdiplus_PathPointTypeBezier3                        0x03
#define Gdiplus_PathPointTypePathTypeMask                   0x07
#define Gdiplus_PathPointTypePathDashMode                   0x10
#define Gdiplus_PathPointTypePathMarker                     0x20
#define Gdiplus_PathPointTypeCloseSubpath                   0x80

// enum PenAlignment
#define Gdiplus_PenAlignmentCenter                          0
#define Gdiplus_PenAlignmentInset                           1

// enum PenType
#define Gdiplus_PenTypeUnknown                              -1
#define Gdiplus_PenTypeSolidColor                           0
#define Gdiplus_PenTypeHatchFill                            1
#define Gdiplus_PenTypeTextureFill                          2
#define Gdiplus_PenTypePathGradient                         3
#define Gdiplus_PenTypeLinearGradient                       4

// enum PixelOffsetMode
#define Gdiplus_PixelOffsetModeInvalid                      -1
#define Gdiplus_PixelOffsetModeDefault                      0
#define Gdiplus_PixelOffsetModeHighSpeed                    1
#define Gdiplus_PixelOffsetModeHighQuality                  2
#define Gdiplus_PixelOffsetModeNone                         3
#define Gdiplus_PixelOffsetModeHalf                         4

// enum QualityMode
#define Gdiplus_QualityModeInvalid                          -1
#define Gdiplus_QualityModeDefault                          0
#define Gdiplus_QualityModeLow                              1
#define Gdiplus_QualityModeHigh                             2

// enum SmoothingMode
#define Gdiplus_SmoothingModeInvalid                        QualityModeInvalid
#define Gdiplus_SmoothingModeDefault                        0
#define Gdiplus_SmoothingModeHighSpeed                      1
#define Gdiplus_SmoothingModeHighQuality                    2
#define Gdiplus_SmoothingModeNone                           3
#define Gdiplus_SmoothingModeAntiAlias8x4                   4
#define Gdiplus_SmoothingModeAntiAlias                      4
#define Gdiplus_SmoothingModeAntiAlias8x8                   5

// enum StringAlignment
#define Gdiplus_StringAlignmentNear                         0
#define Gdiplus_StringAlignmentCenter                       1
#define Gdiplus_StringAlignmentFar                          2

// enum StringDigitSubstitute
#define Gdiplus_StringDigitSubstituteUser                   0
#define Gdiplus_StringDigitSubstituteNone                   1
#define Gdiplus_StringDigitSubstituteNational               2
#define Gdiplus_StringDigitSubstituteTraditional            3

// enum StringFormatFlags
#define Gdiplus_StringFormatFlagsDirectionRightToLeft       0x00000001
#define Gdiplus_StringFormatFlagsDirectionVertical          0x00000002
#define Gdiplus_StringFormatFlagsNoFitBlackBox              0x00000004
#define Gdiplus_StringFormatFlagsDisplayFormatControl       0x00000020
#define Gdiplus_StringFormatFlagsNoFontFallback             0x00000400
#define Gdiplus_StringFormatFlagsMeasureTrailingSpaces      0x00000800
#define Gdiplus_StringFormatFlagsNoWrap                     0x00001000
#define Gdiplus_StringFormatFlagsLineLimit                  0x00002000
#define Gdiplus_StringFormatFlagsNoClip                     0x00004000

// enum StringTrimming
#define Gdiplus_StringTrimmingNone                          0
#define Gdiplus_StringTrimmingCharacter                     1
#define Gdiplus_StringTrimmingWord                          2
#define Gdiplus_StringTrimmingEllipsisCharacter             3
#define Gdiplus_StringTrimmingEllipsisWord                  4
#define Gdiplus_StringTrimmingEllipsisPath                  5

// enum TextRenderingHint
#define Gdiplus_TextRenderingHintSystemDefault              0
#define Gdiplus_TextRenderingHintSingleBitPerPixelGridFit   1
#define Gdiplus_TextRenderingHintSingleBitPerPixel          2
#define Gdiplus_TextRenderingHintAntiAliasGridFit           3
#define Gdiplus_TextRenderingHintAntiAlias                  4
#define Gdiplus_TextRenderingHintClearTypeGridFit           5

// enum Unit
#define Gdiplus_UnitWorld                                   0
#define Gdiplus_UnitDisplay                                 1
#define Gdiplus_UnitPixel                                   2
#define Gdiplus_UnitPoint                                   3
#define Gdiplus_UnitInch                                    4
#define Gdiplus_UnitDocument                                5
#define Gdiplus_UnitMillimeter                              6

// enum WarpMode
#define Gdiplus_WarpModePerspective                         0
#define Gdiplus_WarpModeBilinear                            1

// enum WrapMode
#define Gdiplus_WrapModeTile                                0
#define Gdiplus_WrapModeTileFlipX                           1
#define Gdiplus_WrapModeTileFlipY                           2
#define Gdiplus_WrapModeTileFlipXY                          3
#define Gdiplus_WrapModeClamp                               4

// enum GpTestControlEnum
#define Gdiplus_TestControlForceBilinear                    0
#define Gdiplus_TestControlForceNoICM                       1
#define Gdiplus_TestControlGetBuildNumber                   2

#endif /* __GDIPLUS_ENUMS_CH */
