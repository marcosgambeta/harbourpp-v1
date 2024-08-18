//
// WINAPI for Harbour++ - Bindings libraries for Harbour++ and WINAPI
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

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

#include <windows.h>
#include "hbapi.hpp"
#include "winapi.hpp"

HB_FUNC( WAPTON )
{
  hb_retnint(reinterpret_cast<HB_PTRUINT>(hb_parptr(1)));
}

HB_FUNC( WANTOP )
{
  hb_retptr(reinterpret_cast<void*>(hb_parnint(1)));
}

HB_FUNC( WALOWORD ) // macro TODO: move to correct file
{
  wa_ret_int(LOWORD(wa_par_DWORD(1)));
}

HB_FUNC( WAHIWORD ) // macro TODO: move to correct file
{
  wa_ret_int(HIWORD(wa_par_DWORD(1)));
}
