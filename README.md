# Harbour++

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
* C++11 or upper (C++14, C++17, C++20, ...)

### Related projects

https://github.com/marcosgambeta/hwguipp  
HWGUI for Harbour++

https://github.com/marcosgambeta/hmgextpp  
Harbour MiniGUI Extended for Harbour++

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
* Padrão C++11 ou superior (C++14, C++17, C++20, ...)

### Compilação

O projeto está sendo desenvolvido e testado com os compiladores GCC (MinGW) e Microsoft Visual C++ (MSVC).

#### MinGW
* Definir o padrão, conforme abaixo:  
set HB_USER_CFLAGS=-std=c++11

##### Exemplo
```Batch
set PATH=C:\MinGW810\bin;%PATH%
set HB_INSTALL_PREFIX=C:\HarbourPP
set HB_USER_CFLAGS=-std=c++11
win-make install 1>log1.log 2>log2.log
```

#### Visual C++
* Se necessário, definir o padrão conforme abaixo:  
set HB_USER_CFLAGS=/std=c++11

##### Exemplo
```Batch
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars32.bat"
set HB_INSTALL_PREFIX=C:\HarbourPP
set HB_USER_CFLAGS=/std=c++11
win-make install 1>log1.log 2>log2.log
```

### Projetos relacionados

https://github.com/marcosgambeta/hwguipp  
HWGUI++ - fork da HWGUI para Harbour++

https://github.com/marcosgambeta/hmgextpp  
HMGEXT++ - fork da Harbour MiniGUI Extended para Harbour++

### Contato

Envie sua mensagem para:

marcosgambeta@outlook.com

Questões sobre o projeto serão respondidas **somente por email**.  

### Mais informações e conteúdo extra

Siga o blog abaixo para se manter informado:

https://magsoftinfo.com/blog/
