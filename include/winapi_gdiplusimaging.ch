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

#ifndef __GDIPLUS_IMAGING_CH
#define __GDIPLUS_IMAGING_CH

// enum ImageCodecFlags
#define Gdiplus_ImageCodecFlagsEncoder                      0x00000001
#define Gdiplus_ImageCodecFlagsDecoder                      0x00000002
#define Gdiplus_ImageCodecFlagsSupportBitmap                0x00000004
#define Gdiplus_ImageCodecFlagsSupportVector                0x00000008
#define Gdiplus_ImageCodecFlagsSeekableEncode               0x00000010
#define Gdiplus_ImageCodecFlagsBlockingDecode               0x00000020
#define Gdiplus_ImageCodecFlagsBuiltin                      0x00010000
#define Gdiplus_ImageCodecFlagsSystem                       0x00020000
#define Gdiplus_ImageCodecFlagsUser                         0x00040000

// enum ImageFlags
#define Gdiplus_ImageFlagsNone                              0
#define Gdiplus_ImageFlagsScalable                          0x00000001
#define Gdiplus_ImageFlagsHasAlpha                          0x00000002
#define Gdiplus_ImageFlagsHasTranslucent                    0x00000004
#define Gdiplus_ImageFlagsPartiallyScalable                 0x00000008
#define Gdiplus_ImageFlagsColorSpaceRGB                     0x00000010
#define Gdiplus_ImageFlagsColorSpaceCMYK                    0x00000020
#define Gdiplus_ImageFlagsColorSpaceGRAY                    0x00000040
#define Gdiplus_ImageFlagsColorSpaceYCBCR                   0x00000080
#define Gdiplus_ImageFlagsColorSpaceYCCK                    0x00000100
#define Gdiplus_ImageFlagsHasRealDPI                        0x00001000
#define Gdiplus_ImageFlagsHasRealPixelSize                  0x00002000
#define Gdiplus_ImageFlagsReadOnly                          0x00010000
#define Gdiplus_ImageFlagsCaching                           0x00020000

// enum ImageLockMode
#define Gdiplus_ImageLockModeRead                           1
#define Gdiplus_ImageLockModeWrite                          2
#define Gdiplus_ImageLockModeUserInputBuf                   4

// enum ItemDataPosition
#define Gdiplus_ItemDataPositionAfterHeader                 0
#define Gdiplus_ItemDataPositionAfterPalette                1
#define Gdiplus_ItemDataPositionAfterBits                   2

// enum RotateFlipType
#define Gdiplus_RotateNoneFlipNone                          0
#define Gdiplus_Rotate90FlipNone                            1
#define Gdiplus_Rotate180FlipNone                           2
#define Gdiplus_Rotate270FlipNone                           3
#define Gdiplus_RotateNoneFlipX                             4
#define Gdiplus_Rotate90FlipX                               5
#define Gdiplus_Rotate180FlipX                              6
#define Gdiplus_Rotate270FlipX                              7
#define Gdiplus_Rotate180FlipXY                             0
#define Gdiplus_Rotate270FlipXY                             1
#define Gdiplus_RotateNoneFlipXY                            2
#define Gdiplus_Rotate90FlipXY                              3
#define Gdiplus_Rotate180FlipY                              4
#define Gdiplus_Rotate270FlipY                              5
#define Gdiplus_RotateNoneFlipY                             6
#define Gdiplus_Rotate90FlipY                               7

#define Gdiplus_PropertyTagGpsVer                           0x0000
#define Gdiplus_PropertyTagGpsLatitudeRef                   0x0001
#define Gdiplus_PropertyTagGpsLatitude                      0x0002
#define Gdiplus_PropertyTagGpsLongitudeRef                  0x0003
#define Gdiplus_PropertyTagGpsLongitude                     0x0004
#define Gdiplus_PropertyTagGpsAltitudeRef                   0x0005
#define Gdiplus_PropertyTagGpsAltitude                      0x0006
#define Gdiplus_PropertyTagGpsGpsTime                       0x0007
#define Gdiplus_PropertyTagGpsGpsSatellites                 0x0008
#define Gdiplus_PropertyTagGpsGpsStatus                     0x0009
#define Gdiplus_PropertyTagGpsGpsMeasureMode                0x000A
#define Gdiplus_PropertyTagGpsGpsDop                        0x000B
#define Gdiplus_PropertyTagGpsSpeedRef                      0x000C
#define Gdiplus_PropertyTagGpsSpeed                         0x000D
#define Gdiplus_PropertyTagGpsTrackRef                      0x000E
#define Gdiplus_PropertyTagGpsTrack                         0x000F
#define Gdiplus_PropertyTagGpsImgDirRef                     0x0010
#define Gdiplus_PropertyTagGpsImgDir                        0x0011
#define Gdiplus_PropertyTagGpsMapDatum                      0x0012
#define Gdiplus_PropertyTagGpsDestLatRef                    0x0013
#define Gdiplus_PropertyTagGpsDestLat                       0x0014
#define Gdiplus_PropertyTagGpsDestLongRef                   0x0015
#define Gdiplus_PropertyTagGpsDestLong                      0x0016
#define Gdiplus_PropertyTagGpsDestBearRef                   0x0017
#define Gdiplus_PropertyTagGpsDestBear                      0x0018
#define Gdiplus_PropertyTagGpsDestDistRef                   0x0019
#define Gdiplus_PropertyTagGpsDestDist                      0x001A
#define Gdiplus_PropertyTagNewSubfileType                   0x00FE
#define Gdiplus_PropertyTagSubfileType                      0x00FF
#define Gdiplus_PropertyTagImageWidth                       0x0100
#define Gdiplus_PropertyTagImageHeight                      0x0101
#define Gdiplus_PropertyTagBitsPerSample                    0x0102
#define Gdiplus_PropertyTagCompression                      0x0103
#define Gdiplus_PropertyTagPhotometricInterp                0x0106
#define Gdiplus_PropertyTagThreshHolding                    0x0107
#define Gdiplus_PropertyTagCellWidth                        0x0108
#define Gdiplus_PropertyTagCellHeight                       0x0109
#define Gdiplus_PropertyTagFillOrder                        0x010A
#define Gdiplus_PropertyTagDocumentName                     0x010D
#define Gdiplus_PropertyTagImageDescription                 0x010E
#define Gdiplus_PropertyTagEquipMake                        0x010F
#define Gdiplus_PropertyTagEquipModel                       0x0110
#define Gdiplus_PropertyTagStripOffsets                     0x0111
#define Gdiplus_PropertyTagOrientation                      0x0112
#define Gdiplus_PropertyTagSamplesPerPixel                  0x0115
#define Gdiplus_PropertyTagRowsPerStrip                     0x0116
#define Gdiplus_PropertyTagStripBytesCount                  0x0117
#define Gdiplus_PropertyTagMinSampleValue                   0x0118
#define Gdiplus_PropertyTagMaxSampleValue                   0x0119
#define Gdiplus_PropertyTagXResolution                      0x011A
#define Gdiplus_PropertyTagYResolution                      0x011B
#define Gdiplus_PropertyTagPlanarConfig                     0x011C
#define Gdiplus_PropertyTagPageName                         0x011D
#define Gdiplus_PropertyTagXPosition                        0x011E
#define Gdiplus_PropertyTagYPosition                        0x011F
#define Gdiplus_PropertyTagFreeOffset                       0x0120
#define Gdiplus_PropertyTagFreeByteCounts                   0x0121
#define Gdiplus_PropertyTagGrayResponseUnit                 0x0122
#define Gdiplus_PropertyTagGrayResponseCurve                0x0123
#define Gdiplus_PropertyTagT4Option                         0x0124
#define Gdiplus_PropertyTagT6Option                         0x0125
#define Gdiplus_PropertyTagResolutionUnit                   0x0128
#define Gdiplus_PropertyTagPageNumber                       0x0129
#define Gdiplus_PropertyTagTransferFunction                 0x012D
#define Gdiplus_PropertyTagSoftwareUsed                     0x0131
#define Gdiplus_PropertyTagDateTime                         0x0132
#define Gdiplus_PropertyTagArtist                           0x013B
#define Gdiplus_PropertyTagHostComputer                     0x013C
#define Gdiplus_PropertyTagPredictor                        0x013D
#define Gdiplus_PropertyTagWhitePoint                       0x013E
#define Gdiplus_PropertyTagPrimaryChromaticities            0x013F
#define Gdiplus_PropertyTagColorMap                         0x0140
#define Gdiplus_PropertyTagHalftoneHints                    0x0141
#define Gdiplus_PropertyTagTileWidth                        0x0142
#define Gdiplus_PropertyTagTileLength                       0x0143
#define Gdiplus_PropertyTagTileOffset                       0x0144
#define Gdiplus_PropertyTagTileByteCounts                   0x0145
#define Gdiplus_PropertyTagInkSet                           0x014C
#define Gdiplus_PropertyTagInkNames                         0x014D
#define Gdiplus_PropertyTagNumberOfInks                     0x014E
#define Gdiplus_PropertyTagDotRange                         0x0150
#define Gdiplus_PropertyTagTargetPrinter                    0x0151
#define Gdiplus_PropertyTagExtraSamples                     0x0152
#define Gdiplus_PropertyTagSampleFormat                     0x0153
#define Gdiplus_PropertyTagSMinSampleValue                  0x0154
#define Gdiplus_PropertyTagSMaxSampleValue                  0x0155
#define Gdiplus_PropertyTagTransferRange                    0x0156
#define Gdiplus_PropertyTagJPEGProc                         0x0200
#define Gdiplus_PropertyTagJPEGInterFormat                  0x0201
#define Gdiplus_PropertyTagJPEGInterLength                  0x0202
#define Gdiplus_PropertyTagJPEGRestartInterval              0x0203
#define Gdiplus_PropertyTagJPEGLosslessPredictors           0x0205
#define Gdiplus_PropertyTagJPEGPointTransforms              0x0206
#define Gdiplus_PropertyTagJPEGQTables                      0x0207
#define Gdiplus_PropertyTagJPEGDCTables                     0x0208
#define Gdiplus_PropertyTagJPEGACTables                     0x0209
#define Gdiplus_PropertyTagYCbCrCoefficients                0x0211
#define Gdiplus_PropertyTagYCbCrSubsampling                 0x0212
#define Gdiplus_PropertyTagYCbCrPositioning                 0x0213
#define Gdiplus_PropertyTagREFBlackWhite                    0x0214
#define Gdiplus_PropertyTagGamma                            0x0301
#define Gdiplus_PropertyTagICCProfileDescriptor             0x0302
#define Gdiplus_PropertyTagSRGBRenderingIntent              0x0303
#define Gdiplus_PropertyTagImageTitle                       0x0320
#define Gdiplus_PropertyTagResolutionXUnit                  0x5001
#define Gdiplus_PropertyTagResolutionYUnit                  0x5002
#define Gdiplus_PropertyTagResolutionXLengthUnit            0x5003
#define Gdiplus_PropertyTagResolutionYLengthUnit            0x5004
#define Gdiplus_PropertyTagPrintFlags                       0x5005
#define Gdiplus_PropertyTagPrintFlagsVersion                0x5006
#define Gdiplus_PropertyTagPrintFlagsCrop                   0x5007
#define Gdiplus_PropertyTagPrintFlagsBleedWidth             0x5008
#define Gdiplus_PropertyTagPrintFlagsBleedWidthScale        0x5009
#define Gdiplus_PropertyTagHalftoneLPI                      0x500A
#define Gdiplus_PropertyTagHalftoneLPIUnit                  0x500B
#define Gdiplus_PropertyTagHalftoneDegree                   0x500C
#define Gdiplus_PropertyTagHalftoneShape                    0x500D
#define Gdiplus_PropertyTagHalftoneMisc                     0x500E
#define Gdiplus_PropertyTagHalftoneScreen                   0x500F
#define Gdiplus_PropertyTagJPEGQuality                      0x5010
#define Gdiplus_PropertyTagGridSize                         0x5011
#define Gdiplus_PropertyTagThumbnailFormat                  0x5012
#define Gdiplus_PropertyTagThumbnailWidth                   0x5013
#define Gdiplus_PropertyTagThumbnailHeight                  0x5014
#define Gdiplus_PropertyTagThumbnailColorDepth              0x5015
#define Gdiplus_PropertyTagThumbnailPlanes                  0x5016
#define Gdiplus_PropertyTagThumbnailRawBytes                0x5017
#define Gdiplus_PropertyTagThumbnailSize                    0x5018
#define Gdiplus_PropertyTagThumbnailCompressedSize          0x5019
#define Gdiplus_PropertyTagColorTransferFunction            0x501A
#define Gdiplus_PropertyTagThumbnailData                    0x501B
#define Gdiplus_PropertyTagThumbnailImageWidth              0x5020
#define Gdiplus_PropertyTagThumbnailImageHeight             0x5021
#define Gdiplus_PropertyTagThumbnailBitsPerSample           0x5022
#define Gdiplus_PropertyTagThumbnailCompression             0x5023
#define Gdiplus_PropertyTagThumbnailPhotometricInterp       0x5024
#define Gdiplus_PropertyTagThumbnailImageDescription        0x5025
#define Gdiplus_PropertyTagThumbnailEquipMake               0x5026
#define Gdiplus_PropertyTagThumbnailEquipModel              0x5027
#define Gdiplus_PropertyTagThumbnailStripOffsets            0x5028
#define Gdiplus_PropertyTagThumbnailOrientation             0x5029
#define Gdiplus_PropertyTagThumbnailSamplesPerPixel         0x502A
#define Gdiplus_PropertyTagThumbnailRowsPerStrip            0x502B
#define Gdiplus_PropertyTagThumbnailStripBytesCount         0x502C
#define Gdiplus_PropertyTagThumbnailResolutionX             0x502D
#define Gdiplus_PropertyTagThumbnailResolutionY             0x502E
#define Gdiplus_PropertyTagThumbnailPlanarConfig            0x502F
#define Gdiplus_PropertyTagThumbnailResolutionUnit          0x5030
#define Gdiplus_PropertyTagThumbnailTransferFunction        0x5031
#define Gdiplus_PropertyTagThumbnailSoftwareUsed            0x5032
#define Gdiplus_PropertyTagThumbnailDateTime                0x5033
#define Gdiplus_PropertyTagThumbnailArtist                  0x5034
#define Gdiplus_PropertyTagThumbnailWhitePoint              0x5035
#define Gdiplus_PropertyTagThumbnailPrimaryChromaticities   0x5036
#define Gdiplus_PropertyTagThumbnailYCbCrCoefficients       0x5037
#define Gdiplus_PropertyTagThumbnailYCbCrSubsampling        0x5038
#define Gdiplus_PropertyTagThumbnailYCbCrPositioning        0x5039
#define Gdiplus_PropertyTagThumbnailRefBlackWhite           0x503A
#define Gdiplus_PropertyTagThumbnailCopyRight               0x503B
#define Gdiplus_PropertyTagLuminanceTable                   0x5090
#define Gdiplus_PropertyTagChrominanceTable                 0x5091
#define Gdiplus_PropertyTagFrameDelay                       0x5100
#define Gdiplus_PropertyTagLoopCount                        0x5101
#define Gdiplus_PropertyTagGlobalPalette                    0x5102
#define Gdiplus_PropertyTagIndexBackground                  0x5103
#define Gdiplus_PropertyTagIndexTransparent                 0x5104
#define Gdiplus_PropertyTagPixelUnit                        0x5110
#define Gdiplus_PropertyTagPixelPerUnitX                    0x5111
#define Gdiplus_PropertyTagPixelPerUnitY                    0x5112
#define Gdiplus_PropertyTagPaletteHistogram                 0x5113
#define Gdiplus_PropertyTagCopyright                        0x8298
#define Gdiplus_PropertyTagExifExposureTime                 0x829A
#define Gdiplus_PropertyTagExifFNumber                      0x829D
#define Gdiplus_PropertyTagExifIFD                          0x8769
#define Gdiplus_PropertyTagICCProfile                       0x8773
#define Gdiplus_PropertyTagExifExposureProg                 0x8822
#define Gdiplus_PropertyTagExifSpectralSense                0x8824
#define Gdiplus_PropertyTagGpsIFD                           0x8825
#define Gdiplus_PropertyTagExifISOSpeed                     0x8827
#define Gdiplus_PropertyTagExifOECF                         0x8828
#define Gdiplus_PropertyTagExifVer                          0x9000
#define Gdiplus_PropertyTagExifDTOrig                       0x9003
#define Gdiplus_PropertyTagExifDTDigitized                  0x9004
#define Gdiplus_PropertyTagExifCompConfig                   0x9101
#define Gdiplus_PropertyTagExifCompBPP                      0x9102
#define Gdiplus_PropertyTagExifShutterSpeed                 0x9201
#define Gdiplus_PropertyTagExifAperture                     0x9202
#define Gdiplus_PropertyTagExifBrightness                   0x9203
#define Gdiplus_PropertyTagExifExposureBias                 0x9204
#define Gdiplus_PropertyTagExifMaxAperture                  0x9205
#define Gdiplus_PropertyTagExifSubjectDist                  0x9206
#define Gdiplus_PropertyTagExifMeteringMode                 0x9207
#define Gdiplus_PropertyTagExifLightSource                  0x9208
#define Gdiplus_PropertyTagExifFlash                        0x9209
#define Gdiplus_PropertyTagExifFocalLength                  0x920A
#define Gdiplus_PropertyTagExifMakerNote                    0x927C
#define Gdiplus_PropertyTagExifUserComment                  0x9286
#define Gdiplus_PropertyTagExifDTSubsec                     0x9290
#define Gdiplus_PropertyTagExifDTOrigSS                     0x9291
#define Gdiplus_PropertyTagExifDTDigSS                      0x9292
#define Gdiplus_PropertyTagExifFPXVer                       0xA000
#define Gdiplus_PropertyTagExifColorSpace                   0xA001
#define Gdiplus_PropertyTagExifPixXDim                      0xA002
#define Gdiplus_PropertyTagExifPixYDim                      0xA003
#define Gdiplus_PropertyTagExifRelatedWav                   0xA004
#define Gdiplus_PropertyTagExifInterop                      0xA005
#define Gdiplus_PropertyTagExifFlashEnergy                  0xA20B
#define Gdiplus_PropertyTagExifSpatialFR                    0xA20C
#define Gdiplus_PropertyTagExifFocalXRes                    0xA20E
#define Gdiplus_PropertyTagExifFocalYRes                    0xA20F
#define Gdiplus_PropertyTagExifFocalResUnit                 0xA210
#define Gdiplus_PropertyTagExifSubjectLoc                   0xA214
#define Gdiplus_PropertyTagExifExposureIndex                0xA215
#define Gdiplus_PropertyTagExifSensingMethod                0xA217
#define Gdiplus_PropertyTagExifFileSource                   0xA300
#define Gdiplus_PropertyTagExifSceneType                    0xA301
#define Gdiplus_PropertyTagExifCfaPattern                   0xA302

#define Gdiplus_PropertyTagTypeByte                         1
#define Gdiplus_PropertyTagTypeASCII                        2
#define Gdiplus_PropertyTagTypeShort                        3
#define Gdiplus_PropertyTagTypeLong                         4
#define Gdiplus_PropertyTagTypeRational                     5
#define Gdiplus_PropertyTagTypeUndefined                    7
#define Gdiplus_PropertyTagTypeSLONG                        9
#define Gdiplus_PropertyTagTypeSRational                    10

#endif /* __GDIPLUS_IMAGING_CH */
