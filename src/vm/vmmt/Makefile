ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

vpath %.cpp ../
vpath %.prg ../

ifeq ($(HB_PLATFORM),win)
   ifneq ($(filter $(HB_COMPILER),mingw mingw64),)
      C_MAIN := mainwin.cpp
   else
      C_MAIN := mainstd.cpp mainwin.cpp
   endif
else
   ifeq ($(HB_PLATFORM),cygwin)
      C_MAIN := mainwin.cpp
   else
      ifeq ($(HB_PLATFORM),wce)
         C_MAIN := mainwin.cpp
      else
         ifeq ($(HB_PLATFORM),os2)
            C_MAIN := mainstd.cpp
         else
            C_MAIN := main.cpp
         endif
      endif
   endif
endif

ifeq ($(HB_HVM_ALL),yes)

CPP_SOURCES := \
   hvmall.cpp \

else

CPP_SOURCES := \
   hvm.cpp \
   arrays.cpp \
   classes.cpp \
   dynsym.cpp \
   codebloc.cpp \
   estack.cpp \
   extend.cpp \
   fm.cpp \
   garbage.cpp \
   hashes.cpp \
   itemapi.cpp \
   strapi.cpp \
   macro.cpp \
   memvars.cpp \
   set.cpp \
   thread.cpp \

endif

CPP_SOURCES += \
   arrayshb.cpp \
   asort.cpp \
   break.cpp \
   cmdarg.cpp \
   debug.cpp \
   dynlibhb.cpp \
   eval.cpp \
   evalhb.cpp \
   extrap.cpp \
   hashfunc.cpp \
   initexit.cpp \
   initsymb.cpp \
   memvclip.cpp \
   pbyref.cpp \
   pcount.cpp \
   pvalue.cpp \
   pvaluehb.cpp \
   proc.cpp \
   procaddr.cpp \
   runner.cpp \
   vm.cpp \
   $(C_MAIN) \

PRG_SOURCES := \
   harbinit.prg \

LIBNAME := hbvmmt

HB_CFLAGS += -DHB_MT_VM

include $(TOP)$(ROOT)config/lib.mk
