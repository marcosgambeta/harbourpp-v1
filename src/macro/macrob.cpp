// hbexprb.cpp is also included from ../compiler/exproptb.cpp
// However it produces a slightly different code if used in
// macro compiler (there is an additional parameter passed to some functions)

#define HB_MACRO_SUPPORT

#include "hbmacro.hpp"
#include "hbcomp.hpp"

#include "hbexprb.cpp"
