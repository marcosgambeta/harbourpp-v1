# Notes about the code style used in Harbour++

## Formatting

The source code is formated using the clang-format tool:

https://clang.llvm.org/docs/ClangFormat.html

The preset utilized is derived from 'Microsoft' preset:

https://github.com/marcosgambeta/harbourpp-v1/blob/main/.clang-format

Additional changes in the preset:

IndentWidth: 2  
SortIncludes: Never  

Sections are protected from formatting with the lines below:

```
// clang-format off

... code protected

// clang-format on
```

## auto keyword

The `auto` keyword is used to help simplify code writing and maintenance. Your use do not affect the final result
(binaries are the same with or without the auto keyword).

Example:

The return of the function `hb_itemNew` is always a pointer to a HB_ITEM structure.

So, we can write this...

```
auto pItem = hb_itemNew(nullptr);
```

instead of this...

```
PHB_ITEM pItem = hb_itemNew(nullptr);
```

The type is deduced at compile time.

For xBase developers, the auto keyword used in the Harbour++ source code is irrelevant.

## C++ casting

Casting operators are used to convert one data type to another. The Harbour++ source code uses const_cast, static_cast
and reinterpret_cast to show the type of the cast being used in each conversion.

Despite the verbosity, the type of cast used in each conversion is explicit and not hidden.

For xBase developers, the casting operators used in the Harbour++ source code are irrelevant.

## More info

This document is a work in progress. You can use the section below to discuss about the topics above or related topics:

https://github.com/marcosgambeta/harbourpp-v1/discussions