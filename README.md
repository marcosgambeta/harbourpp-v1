# Harbour++

Harbour++ é um projeto derivado do projeto Harbour, tendo como objetivo principal
a transição da linguagem C para a linguagem C++, usando o C++11 como padrão mínimo.

Para facilitar a adaptação das aplicações, será mantido o máximo de compatibilidade
possível com CA-Clipper/Harbour.

Este projeto é um trabalho em progresso.

## Requisitos
* Compilador C++
* Padrão C++11 ou superior (C++14, C++17, ...)

## Compilação

O projeto está sendo desenvolvido e testado com os compiladores GCC (MinGW) e Microsoft Visual C++ (MSVC).

### MinGW
* Definir o padrão, conforme abaixo:  
set HB_USER_CFLAGS=-std=c++11

#### Exemplo
set PATH=C:\MinGW810\bin;%PATH%
set HB_INSTALL_PREFIX=C:\HarbourPP
set HB_USER_CFLAGS=-std=c++11
win-make install 1>log1.log 2>log2.log

### Visual C++
* Se necessário, definir o padrão conforme abaixo:  
set HB_USER_CFLAGS=/std=c++11

#### Exemplo
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars32.bat"
set HB_INSTALL_PREFIX=C:\HarbourPP
set HB_USER_CFLAGS=/std=c++11
win-make install 1>log1.log 2>log2.log

## Contato

Envie sua mensagem para:

marcosgambeta AT outlook DOT com

Questões sobre o projeto serão respondidas **somente por email**.  

## Mais informações/conteúdo extra

Siga o blog abaixo para se manter informado:

https://magsoftinfo.com/blog/
