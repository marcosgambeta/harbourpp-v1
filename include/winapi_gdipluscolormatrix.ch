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

#ifndef __GDIPLUS_COLORMATRIX_CH
#define __GDIPLUS_COLORMATRIX_CH

// enum ColorAdjustType
#define Gdiplus_ColorAdjustTypeDefault                      0
#define Gdiplus_ColorAdjustTypeBitmap                       1
#define Gdiplus_ColorAdjustTypeBrush                        2
#define Gdiplus_ColorAdjustTypePen                          3
#define Gdiplus_ColorAdjustTypeText                         4
#define Gdiplus_ColorAdjustTypeCount                        5
#define Gdiplus_ColorAdjustTypeAny                          6

// enum ColorMatrixFlags
#define Gdiplus_ColorMatrixFlagsDefault                     0
#define Gdiplus_ColorMatrixFlagsSkipGrays                   1
#define Gdiplus_ColorMatrixFlagsAltGray                     2

// enum HistogramFormat
#define Gdiplus_HistogramFormatARGB                         0
#define Gdiplus_HistogramFormatPARGB                        1
#define Gdiplus_HistogramFormatRGB                          2
#define Gdiplus_HistogramFormatGray                         3
#define Gdiplus_HistogramFormatB                            4
#define Gdiplus_HistogramFormatG                            5
#define Gdiplus_HistogramFormatR                            6
#define Gdiplus_HistogramFormatA                            7

#endif /* __GDIPLUS_COLORMATRIX_CH */