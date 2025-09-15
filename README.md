# Harbour++

* [English](#english)
* [Portuguese](#portuguese)

## English

Harbour++ is derived from the Harbour project, having as main goal the transition
from the C language to the C++ language, using C++11 as the minimum standard.

The source codebase was the Harbour 3.2.0dev. Harbour 3.4 features are being reviewed and,
if appropriate, implemented in Harbour++.

To help the migration of the applications, the series 1 of Harbour++ (versions 1.#.#)
will keep the most compatibility possible with CA-Clipper/Harbour commands, classes and functions.

This project is a work in progress.

### Requisites
* C++ compiler
* C++11 or upper (C++14, C++17, C++20, C++23, ...)

### Compilation

The project is being developed and tested with the GCC (MinGW), Microsoft Visual C++ (MSVC),
LLVM/Clang C++ and 'Embarcadero C++ 7.x for Win64' (BCC64) compilers.

#### MinGW
* Set the standard as shown below:  
set HB_USER_CFLAGS=-std=c++11

##### Examples
```Batch
set PATH=C:\MinGW32\bin;%PATH%
set HB_INSTALL_PREFIX=C:\HBPPMINGW32
set HB_USER_CFLAGS=-std=c++11
win-make install 1>log1.log 2>log2.log
```
```Batch
set PATH=C:\MinGW64\bin;%PATH%
set HB_INSTALL_PREFIX=C:\HBPPMINGW64
set HB_USER_CFLAGS=-std=c++11
win-make install 1>log1.log 2>log2.log
```

#### Visual C++
* If necessary, set the standard as per the example below:
set HB_USER_CFLAGS=/std=c++11

##### Examples
```Batch
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars32.bat"
set HB_INSTALL_PREFIX=C:\HBPPMSVC32
set HB_USER_CFLAGS=/std=c++11
win-make install 1>log1.log 2>log2.log
```
```Batch
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars64.bat"
set HB_INSTALL_PREFIX=C:\HBPPMSVC64
set HB_USER_CFLAGS=/std=c++11
win-make install 1>log1.log 2>log2.log
```

#### LLVM/Clang C++
* If necessary, set the standard as per the example below:  
set HB_USER_CFLAGS=-std=c++11

##### Examples
```Batch
set PATH=C:\MinGW32\bin;%PATH%
set HB_INSTALL_PREFIX=C:\HBPPCLANG32
set HB_COMPILER=clang
win-make install 1>log1.log 2>log2.log
```
```Batch
set PATH=C:\MinGW64\bin;%PATH%
set HB_INSTALL_PREFIX=C:\HBPPCLANG64
set HB_COMPILER=clang64
win-make install 1>log1.log 2>log2.log
```

#### Embarcadero C++ 7.x for Win64

* Using Harbour++ in conjunction with this compiler is a work in progress.
* Please follow the examples below as a basis for your testing.
* If you wish, you can use the 'Issues' section to report any problems you encounter.
* Compiling the libraries in the 'contrib' folder has not yet been tested.

##### Examples
```Batch
set PATH=C:\BCC64\bin;%PATH%
set HB_INSTALL_PREFIX=C:\HBPPBCC64
set HB_BUILD_CONTRIB_DYN=no
set HB_BUILD_CONTRIBS=no
set HB_COMPILER=bcc64
set HB_USER_LDFLAGS=-LC:\BCC64\lib;C:\BCC64\lib\psdk
set HB_USER_DFLAGS=-LC:\BCC64\lib;C:\BCC64\lib\psdk
set HB_USER_RESFLAGS=-IC:\BCC64\include\windows\sdk
win-make install 1>log1.log 2>log2.log
```

```Batch
hbmk2 program -ldflag=-LC:\BCC64\lib -ldflag=-LC:\BCC64\lib\psdk
```

#### ZIG toolchain

Work in progress. Can be tested only in Windows and only with the Harbour++ core.

hbmk2 compile only single .prg files.

##### Examples

Download and extract the ZIG toolchain:  
https://ziglang.org/download/0.15.1/zig-x86-windows-0.15.1.zip

```Batch
set PATH=C:\zig-x86-windows-0.15.1;%PATH%
set HB_INSTALL_PREFIX=C:\HBPPZIG32
set HB_BUILD_DYN=no
set HB_BUILD_CONTRIB_DYN=no
set HB_BUILD_CONTRIBS=no
set HB_COMPILER=zig
win-make install 1>log1.log 2>log2.log
```

```Batch
hbmk2 file.prg
```

#### icx - Intel oneAPI compiler

To learn more about the compiler and download binaries:  
https://www.intel.com/content/www/us/en/developer/tools/oneapi/overview.html  

Note: work in progress. Can be tested only in Windows.

##### Examples

```Batch
call "C:\Program Files (x86)\Intel\oneAPI\compiler\2021.2.0\env\vars.bat" ia32
set HB_INSTALL_PREFIX=C:\HBPPICX32
set HB_BUILD_DYN=no
set HB_BUILD_CONTRIB_DYN=no
set HB_COMPILER=icx
win-make install 1>log1.log 2>log2.log
pause
```

```Batch
hbmk2 file.prg -comp=icx
hbmk2 project.hbp -comp=icx
```

```Batch
call "C:\Program Files (x86)\Intel\oneAPI\compiler\2021.2.0\env\vars.bat" intel64
set HB_INSTALL_PREFIX=C:\HBPPICX64
set HB_BUILD_DYN=no
set HB_BUILD_CONTRIB_DYN=no
set HB_COMPILER=icx64
win-make install 1>log1.log 2>log2.log
pause
```

```Batch
hbmk2 file.prg -comp=icx64
hbmk2 project.hbp -comp=icx64
```

### Related projects

https://github.com/marcosgambeta/hwguipp  
HWGUI for Harbour++

https://github.com/marcosgambeta/hmgextpp  
Harbour MiniGUI Extended for Harbour++

https://github.com/marcosgambeta/sqlrddpp  
SQLRDD for Harbour++

https://github.com/marcosgambeta/hblibvlc  
libvlc for Harbour++

https://github.com/marcosgambeta/hbopencv2  
OpenCV2 for Harbour++

### Contact

Send your message using one of the options below:

E-mail:  
marcosgambeta@outlook.com

Telegram:  
https://t.me/marcosgambeta

## Portuguese

Harbour++ é um projeto derivado do projeto Harbour, tendo como objetivo principal
a transição da linguagem C para a linguagem C++, usando o C++11 como padrão mínimo.

Foi utilizado como base o código-fonte do Harbour 3.2.0dev. Recursos do Harbour 3.4
estão sendo revisados e, conforme o caso, implementados no Harbour++.

Para facilitar a adaptação das aplicações, a série 1 do Harbour++ (versões 1.#.#) manterá
o máximo possível de compatibilidade com os comandos, classes e funções do CA-Clipper/Harbour.

Este projeto é um trabalho em progresso.

### Requisitos
* Compilador C++
* Padrão C++11 ou superior (C++14, C++17, C++20, C++23, ...)

### Compilação

O projeto está sendo desenvolvido e testado com os compiladores GCC (MinGW), Microsoft Visual C++ (MSVC),
LLVM/Clang C++ e 'Embarcadero C++ 7.x for Win64' (BCC64).

#### MinGW
* Definir o padrão, conforme exemplo abaixo:  
set HB_USER_CFLAGS=-std=c++11

##### Exemplos
```Batch
set PATH=C:\MinGW32\bin;%PATH%
set HB_INSTALL_PREFIX=C:\HBPPMINGW32
set HB_USER_CFLAGS=-std=c++11
win-make install 1>log1.log 2>log2.log
```
```Batch
set PATH=C:\MinGW64\bin;%PATH%
set HB_INSTALL_PREFIX=C:\HBPPMINGW64
set HB_USER_CFLAGS=-std=c++11
win-make install 1>log1.log 2>log2.log
```

#### Visual C++
* Se necessário, definir o padrão conforme exemplo abaixo:  
set HB_USER_CFLAGS=/std=c++11

##### Exemplos
```Batch
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars32.bat"
set HB_INSTALL_PREFIX=C:\HBPPMSVC32
set HB_USER_CFLAGS=/std=c++11
win-make install 1>log1.log 2>log2.log
```
```Batch
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars64.bat"
set HB_INSTALL_PREFIX=C:\HBPPMSVC64
set HB_USER_CFLAGS=/std=c++11
win-make install 1>log1.log 2>log2.log
```

#### LLVM/Clang C++
* Se necessário, definir o padrão conforme exemplo abaixo:  
set HB_USER_CFLAGS=-std=c++11

##### Exemplos
```Batch
set PATH=C:\MinGW32\bin;%PATH%
set HB_INSTALL_PREFIX=C:\HBPPCLANG32
set HB_COMPILER=clang
win-make install 1>log1.log 2>log2.log
```
```Batch
set PATH=C:\MinGW64\bin;%PATH%
set HB_INSTALL_PREFIX=C:\HBPPCLANG64
set HB_COMPILER=clang64
win-make install 1>log1.log 2>log2.log
```

#### Embarcadero C++ 7.x for Win64

* A utilização do Harbour++ em conjunto com este compilador é um trabalho em progresso.
* Siga os exemplos abaixo como base para seus testes.
* Se desejar, poderá usar a seção 'Issues' para relatar problemas encontrados.
* A compilação das bibliotecas da pasta 'contrib' ainda não foi testada.

##### Exemplos
```Batch
set PATH=C:\BCC64\bin;%PATH%
set HB_INSTALL_PREFIX=C:\HBPPBCC64
set HB_BUILD_CONTRIB_DYN=no
set HB_BUILD_CONTRIBS=no
set HB_COMPILER=bcc64
set HB_USER_LDFLAGS=-LC:\BCC64\lib;C:\BCC64\lib\psdk
set HB_USER_DFLAGS=-LC:\BCC64\lib;C:\BCC64\lib\psdk
set HB_USER_RESFLAGS=-IC:\BCC64\include\windows\sdk
win-make install 1>log1.log 2>log2.log
```

```Batch
hbmk2 program -ldflag=-LC:\BCC64\lib -ldflag=-LC:\BCC64\lib\psdk
```

#### ZIG toolchain

Trabalho em progresso. Pode ser usado somente no Windows e somente com o núcleo do Harbour++.

hbmk2 compila apenas arquivos .prg individuais.

##### Exemplos

Baixe e extraia o ZIG toolchain:  
https://ziglang.org/download/0.15.1/zig-x86-windows-0.15.1.zip

```Batch
set PATH=C:\zig-x86-windows-0.15.1;%PATH%
set HB_INSTALL_PREFIX=C:\HBPPZIG32
set HB_BUILD_DYN=no
set HB_BUILD_CONTRIB_DYN=no
set HB_BUILD_CONTRIBS=no
set HB_COMPILER=zig
win-make install 1>log1.log 2>log2.log
```

```Batch
hbmk2 file.prg
```

#### icx - Intel oneAPI compiler

Para aprender mais sobre o compilador e baixar binários:  
https://www.intel.com/content/www/us/en/developer/tools/oneapi/overview.html  

Nota: trabalho em progresso. Pode ser testado somente no Windows.

##### Exemplos

```Batch
call "C:\Program Files (x86)\Intel\oneAPI\compiler\2021.2.0\env\vars.bat" ia32
set HB_INSTALL_PREFIX=C:\HBPPICX32
set HB_BUILD_DYN=no
set HB_BUILD_CONTRIB_DYN=no
set HB_COMPILER=icx
win-make install 1>log1.log 2>log2.log
pause
```

```Batch
hbmk2 file.prg -comp=icx
hbmk2 project.hbp -comp=icx
```

```Batch
call "C:\Program Files (x86)\Intel\oneAPI\compiler\2021.2.0\env\vars.bat" intel64
set HB_INSTALL_PREFIX=C:\HBPPICX64
set HB_BUILD_DYN=no
set HB_BUILD_CONTRIB_DYN=no
set HB_COMPILER=icx64
win-make install 1>log1.log 2>log2.log
pause
```

```Batch
hbmk2 file.prg -comp=icx64
hbmk2 project.hbp -comp=icx64
```

### Projetos relacionados

https://github.com/marcosgambeta/hwguipp  
HWGUI++ - fork da HWGUI para Harbour++

https://github.com/marcosgambeta/hmgextpp  
HMGEXT++ - fork da Harbour MiniGUI Extended para Harbour++

https://github.com/marcosgambeta/sqlrddpp  
SQLRDD for Harbour++

https://github.com/marcosgambeta/hblibvlc  
libvlc for Harbour++

https://github.com/marcosgambeta/hbopencv2  
OpenCV2 for Harbour++

### Contato

Envie sua mensagem usando uma das opções abaixo:

E-mail:  
marcosgambeta@outlook.com

Telegram:  
https://t.me/marcosgambeta

### Mais informações e conteúdo extra

Siga o blog abaixo para se manter informado:

https://magsoftinfo.com/blog/

### Donativos

Se este projeto for útil para você e desejar apoiar seu desenvolvimento através de donativos,
basta utilizar a chave Pix abaixo:

marcosgambeta@outlook.com
