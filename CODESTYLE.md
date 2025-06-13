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

## nullptr keyword

The Harbour++ source code use `nullptr` in place of `NULL`, because `nullptr` is safer while `NULL` is ambiguous
and can result in errors in some contexts.

Info from Google AI:

The nullptr keyword in C++ offers several advantages over using NULL or 0 to represent null pointers:

Type Safety:  
nullptr has its own type (std::nullptr_t) and cannot be implicitly converted to integral types (except for bool). This prevents accidental type mismatches and errors that can occur with NULL or 0, which are often treated as integers.

Improved Readability:  
Code using nullptr is more self-explanatory. It clearly indicates the intent to use a null pointer, making the code easier to understand and maintain.

Function Overload Resolution:  
When passing a null pointer as an argument to overloaded functions, nullptr avoids ambiguity. It ensures the correct overload is called, unlike NULL or 0, which can be interpreted as integers, leading to unexpected behavior. 

Template Specialization:  
nullptr allows for template specialization, enabling the creation of specific implementations for null pointer cases. This is not possible with NULL or 0.

Compiler Support:  
Modern compilers provide better diagnostics and error messages when nullptr is used incorrectly, helping developers catch potential issues early in the development process.

Clarity with Auto Variables:  
nullptr helps avoid ambiguity when using the auto keyword for type deduction. The compiler can correctly deduce the type as a pointer type when nullptr is used.

No Implicit Conversion to Non-Pointer Types:  
nullptr cannot be unintentionally converted to non-pointer types, reducing the risk of errors.

Consistent Behavior:  
nullptr ensures consistent behavior when comparing pointers. Comparisons between two nullptr values always behave as expected.

Managed Code Support:  
nullptr works correctly with both native and managed code, ensuring compatibility in mixed environments.

In summary, nullptr provides a safer, more readable, and less error-prone way to represent null pointers in C++ compared to NULL or 0. It is the recommended approach for modern C++ development.

## Testing pointers

```
if (p == nullptr)
{
  // code
}
```

```
if (p != nullptr)
{
  // code
}
```

## bool type

Whenever possible, the `bool` type (`true` and `false`) is used instead of `HB_BOOL` (`HB TRUE` and `HB_FALSE`).

## \#define vs constexpr

Whenever possible, constexpr is used instead of #define.

Info from Google AI:

Here is a comparison between #define and constexpr:  

\#define:  

    Is a preprocessor directive that performs text substitution before compilation.  
    Does not perform type checking, making it less safe.  
    Can be used for various purposes, including conditional compilation and defining constants.  
    Can lead to unexpected behavior due to its simple text substitution nature.  
    Macros do not have scope, and they can be replaced anywhere in the code.  
    It is generally recommended to avoid #define when modern alternatives are available.  

constexpr:  

    Is a keyword that declares a variable or function whose value can be evaluated at compile time.  
    Provides type safety, as it requires a specific data type.  
    Can be used to create named compile-time constants.  
    Can be used for functions that can be evaluated at compile time.  
    It ensures that the value is determined during the compilation phase.  
    It can be used in contexts where compile-time constants are required, such as array sizes and template arguments.  
    constexpr implies const.  

Key Differences:  

    #define is a preprocessor directive, whereas constexpr is a language feature.  
    #define performs text substitution, while constexpr performs compile-time evaluation with type checking.  
    constexpr is preferred over #define for defining constants due to its safety and type checking.  
    constexpr functions can be recursive while #define macros cannot.  
    #define can be more flexible in certain contexts, but constexpr is safer and more reliable.  

In summary:  
constexpr is a modern C++ feature that provides a safer and more reliable alternative to \#define for defining compile-time constants and performing compile-time computations. It is recommended to use constexpr whenever possible to avoid the potential issues associated with \#define.  

## More info

This document is a work in progress. You can use the section below to discuss about the topics above or related topics:

https://github.com/marcosgambeta/harbourpp-v1/discussions
