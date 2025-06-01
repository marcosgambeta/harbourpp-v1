// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>

// MIT License
//
// Copyright (c) 2024 Marcos Antonio Gambeta
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#ifndef __GDIPLUS_COLOR_CH
#define __GDIPLUS_COLOR_CH

// enum ColorChannelFlags
#define Gdiplus_ColorChannelFlagsC                          0
#define Gdiplus_ColorChannelFlagsM                          1
#define Gdiplus_ColorChannelFlagsY                          2
#define Gdiplus_ColorChannelFlagsK                          3
#define Gdiplus_ColorChannelFlagsLast                       4

// Color
#define Gdiplus_ColorAlphaMask                              0xFF000000
#define Gdiplus_ColorAliceBlue                              0xFFF0F8FF
#define Gdiplus_ColorAntiqueWhite                           0xFFFAEBD7
#define Gdiplus_ColorAqua                                   0xFF00FFFF
#define Gdiplus_ColorAquamarine                             0xFF7FFFD4
#define Gdiplus_ColorAzure                                  0xFFF0FFFF
#define Gdiplus_ColorBeige                                  0xFFF5F5DC
#define Gdiplus_ColorBisque                                 0xFFFFE4C4
#define Gdiplus_ColorBlack                                  0xFF000000
#define Gdiplus_ColorBlanchedAlmond                         0xFFFFEBCD
#define Gdiplus_ColorBlue                                   0xFF0000FF
#define Gdiplus_ColorBlueViolet                             0xFF8A2BE2
#define Gdiplus_ColorBrown                                  0xFFA52A2A
#define Gdiplus_ColorBurlyWood                              0xFFDEB887
#define Gdiplus_ColorCadetBlue                              0xFF5F9EA0
#define Gdiplus_ColorChartreuse                             0xFF7FFF00
#define Gdiplus_ColorChocolate                              0xFFD2691E
#define Gdiplus_ColorCoral                                  0xFFFF7F50
#define Gdiplus_ColorCornflowerBlue                         0xFF6495ED
#define Gdiplus_ColorCornsilk                               0xFFFFF8DC
#define Gdiplus_ColorCrimson                                0xFFDC143C
#define Gdiplus_ColorCyan                                   0xFF00FFFF
#define Gdiplus_ColorDarkBlue                               0xFF00008B
#define Gdiplus_ColorDarkCyan                               0xFF008B8B
#define Gdiplus_ColorDarkGoldenrod                          0xFFB8860B
#define Gdiplus_ColorDarkGray                               0xFFA9A9A9
#define Gdiplus_ColorDarkGreen                              0xFF006400
#define Gdiplus_ColorDarkKhaki                              0xFFBDB76B
#define Gdiplus_ColorDarkMagenta                            0xFF8B008B
#define Gdiplus_ColorDarkOliveGreen                         0xFF556B2F
#define Gdiplus_ColorDarkOrange                             0xFFFF8C00
#define Gdiplus_ColorDarkOrchid                             0xFF9932CC
#define Gdiplus_ColorDarkRed                                0xFF8B0000
#define Gdiplus_ColorDarkSalmon                             0xFFE9967A
#define Gdiplus_ColorDarkSeaGreen                           0xFF8FBC8F
#define Gdiplus_ColorDarkSlateBlue                          0xFF483D8B
#define Gdiplus_ColorDarkSlateGray                          0xFF2F4F4F
#define Gdiplus_ColorDarkTurquoise                          0xFF00CED1
#define Gdiplus_ColorDarkViolet                             0xFF9400D3
#define Gdiplus_ColorDeepPink                               0xFFFF1493
#define Gdiplus_ColorDeepSkyBlue                            0xFF00BFFF
#define Gdiplus_ColorDimGray                                0xFF696969
#define Gdiplus_ColorDodgerBlue                             0xFF1E90FF
#define Gdiplus_ColorFirebrick                              0xFFB22222
#define Gdiplus_ColorFloralWhite                            0xFFFFFAF0
#define Gdiplus_ColorForestGreen                            0xFF228B22
#define Gdiplus_ColorFuchsia                                0xFFFF00FF
#define Gdiplus_ColorGainsboro                              0xFFDCDCDC
#define Gdiplus_ColorGhostWhite                             0xFFF8F8FF
#define Gdiplus_ColorGold                                   0xFFFFD700
#define Gdiplus_ColorGoldenrod                              0xFFDAA520
#define Gdiplus_ColorGray                                   0xFF808080
#define Gdiplus_ColorGreen                                  0xFF008000
#define Gdiplus_ColorGreenYellow                            0xFFADFF2F
#define Gdiplus_ColorHoneydew                               0xFFF0FFF0
#define Gdiplus_ColorHotPink                                0xFFFF69B4
#define Gdiplus_ColorIndianRed                              0xFFCD5C5C
#define Gdiplus_ColorIndigo                                 0xFF4B0082
#define Gdiplus_ColorIvory                                  0xFFFFFFF0
#define Gdiplus_ColorKhaki                                  0xFFF0E68C
#define Gdiplus_ColorLavender                               0xFFE6E6FA
#define Gdiplus_ColorLavenderBlush                          0xFFFFF0F5
#define Gdiplus_ColorLawnGreen                              0xFF7CFC00
#define Gdiplus_ColorLemonChiffon                           0xFFFFFACD
#define Gdiplus_ColorLightBlue                              0xFFADD8E6
#define Gdiplus_ColorLightCoral                             0xFFF08080
#define Gdiplus_ColorLightCyan                              0xFFE0FFFF
#define Gdiplus_ColorLightGoldenrodYellow                   0xFFFAFAD2
#define Gdiplus_ColorLightGray                              0xFFD3D3D3
#define Gdiplus_ColorLightGreen                             0xFF90EE90
#define Gdiplus_ColorLightPink                              0xFFFFB6C1
#define Gdiplus_ColorLightSalmon                            0xFFFFA07A
#define Gdiplus_ColorLightSeaGreen                          0xFF20B2AA
#define Gdiplus_ColorLightSkyBlue                           0xFF87CEFA
#define Gdiplus_ColorLightSlateGray                         0xFF778899
#define Gdiplus_ColorLightSteelBlue                         0xFFB0C4DE
#define Gdiplus_ColorLightYellow                            0xFFFFFFE0
#define Gdiplus_ColorLime                                   0xFF00FF00
#define Gdiplus_ColorLimeGreen                              0xFF32CD32
#define Gdiplus_ColorLinen                                  0xFFFAF0E6
#define Gdiplus_ColorMagenta                                0xFFFF00FF
#define Gdiplus_ColorMaroon                                 0xFF800000
#define Gdiplus_ColorMediumAquamarine                       0xFF66CDAA
#define Gdiplus_ColorMediumBlue                             0xFF0000CD
#define Gdiplus_ColorMediumOrchid                           0xFFBA55D3
#define Gdiplus_ColorMediumPurple                           0xFF9370DB
#define Gdiplus_ColorMediumSeaGreen                         0xFF3CB371
#define Gdiplus_ColorMediumSlateBlue                        0xFF7B68EE
#define Gdiplus_ColorMediumSpringGreen                      0xFF00FA9A
#define Gdiplus_ColorMediumTurquoise                        0xFF48D1CC
#define Gdiplus_ColorMediumVioletRed                        0xFFC71585
#define Gdiplus_ColorMidnightBlue                           0xFF191970
#define Gdiplus_ColorMintCream                              0xFFF5FFFA
#define Gdiplus_ColorMistyRose                              0xFFFFE4E1
#define Gdiplus_ColorMoccasin                               0xFFFFE4B5
#define Gdiplus_ColorNavajoWhite                            0xFFFFDEAD
#define Gdiplus_ColorNavy                                   0xFF000080
#define Gdiplus_ColorOldLace                                0xFFFDF5E6
#define Gdiplus_ColorOlive                                  0xFF808000
#define Gdiplus_ColorOliveDrab                              0xFF6B8E23
#define Gdiplus_ColorOrange                                 0xFFFFA500
#define Gdiplus_ColorOrangeRed                              0xFFFF4500
#define Gdiplus_ColorOrchid                                 0xFFDA70D6
#define Gdiplus_ColorPaleGoldenrod                          0xFFEEE8AA
#define Gdiplus_ColorPaleGreen                              0xFF98FB98
#define Gdiplus_ColorPaleTurquoise                          0xFFAFEEEE
#define Gdiplus_ColorPaleVioletRed                          0xFFDB7093
#define Gdiplus_ColorPapayaWhip                             0xFFFFEFD5
#define Gdiplus_ColorPeachPuff                              0xFFFFDAB9
#define Gdiplus_ColorPeru                                   0xFFCD853F
#define Gdiplus_ColorPink                                   0xFFFFC0CB
#define Gdiplus_ColorPlum                                   0xFFDDA0DD
#define Gdiplus_ColorPowderBlue                             0xFFB0E0E6
#define Gdiplus_ColorPurple                                 0xFF800080
#define Gdiplus_ColorRed                                    0xFFFF0000
#define Gdiplus_ColorRosyBrown                              0xFFBC8F8F
#define Gdiplus_ColorRoyalBlue                              0xFF4169E1
#define Gdiplus_ColorSaddleBrown                            0xFF8B4513
#define Gdiplus_ColorSalmon                                 0xFFFA8072
#define Gdiplus_ColorSandyBrown                             0xFFF4A460
#define Gdiplus_ColorSeaGreen                               0xFF2E8B57
#define Gdiplus_ColorSeaShell                               0xFFFFF5EE
#define Gdiplus_ColorSienna                                 0xFFA0522D
#define Gdiplus_ColorSilver                                 0xFFC0C0C0
#define Gdiplus_ColorSkyBlue                                0xFF87CEEB
#define Gdiplus_ColorSlateBlue                              0xFF6A5ACD
#define Gdiplus_ColorSlateGray                              0xFF708090
#define Gdiplus_ColorSnow                                   0xFFFFFAFA
#define Gdiplus_ColorSpringGreen                            0xFF00FF7F
#define Gdiplus_ColorSteelBlue                              0xFF4682B4
#define Gdiplus_ColorTan                                    0xFFD2B48C
#define Gdiplus_ColorTeal                                   0xFF008080
#define Gdiplus_ColorThistle                                0xFFD8BFD8
#define Gdiplus_ColorTomato                                 0xFFFF6347
#define Gdiplus_ColorTransparent                            0x00FFFFFF
#define Gdiplus_ColorTurquoise                              0xFF40E0D0
#define Gdiplus_ColorViolet                                 0xFFEE82EE
#define Gdiplus_ColorWheat                                  0xFFF5DEB3
#define Gdiplus_ColorWhite                                  0xFFFFFFFF
#define Gdiplus_ColorWhiteSmoke                             0xFFF5F5F5
#define Gdiplus_ColorYellow                                 0xFFFFFF00
#define Gdiplus_ColorYellowGreen                            0xFF9ACD32

#endif // __GDIPLUS_COLOR_CH
