//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2025 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// MIT License
//
// Copyright (c) 2025 Marcos Antonio Gambeta
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

// NOTE: source code generated with the help of a code generator

#include <windows.h>
#include <gdiplus.h>
#include "hbapi.hpp"
#include "hbapiitm.hpp"
#include "hbapicls.hpp"
#include "hbwinuni.hpp"
#include "winapi.hpp"

#define wa_ret_GpStatus(x) hb_retni(x)

using namespace Gdiplus;

static ULONG_PTR gdiplusToken{};
static GdiplusStartupInput gdiplusStartupInput{};

// GpStatus WINGDIPAPI GdiplusStartup(ULONG_PTR*,GDIPCONST GdiplusStartupInput*,GdiplusStartupOutput*)
HB_FUNC(WAGDIPLUSSTARTUP) // TODO: parameters not used
{
  wa_ret_GpStatus(GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, nullptr));
}

// VOID WINGDIPAPI GdiplusShutdown(ULONG_PTR)
HB_FUNC(WAGDIPLUSSHUTDOWN) // TODO: parameters not used
{
  GdiplusShutdown(gdiplusToken);
}

// GpStatus WINGDIPAPI GdiplusNotificationHook(ULONG_PTR*)

// VOID WINGDIPAPI GdiplusNotificationUnhook(ULONG_PTR)
