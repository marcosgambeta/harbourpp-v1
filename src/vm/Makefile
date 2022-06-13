ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

ifeq ($(HB_HVM_ALL),)

   HB_HVM_ALL := yes
   ifeq ($(HB_COMPILER),bcc)
      HB_HVM_ALL := no
   endif
   export HB_HVM_ALL
endif

DIRS :=

ifeq ($(HB_PLATFORM),win)
   ifneq ($(filter $(HB_COMPILER),mingw mingw64),)
      C_MAIN := mainwin.cpp
   else
      C_MAIN := mainstd.cpp mainwin.cpp
   endif
   DIRS := mainstd mainwin maindllh maindllp
else
   ifeq ($(HB_PLATFORM),cygwin)
      C_MAIN := mainstd.cpp
      DIRS := mainstd maindllh maindllp
   else
      ifeq ($(HB_PLATFORM),wce)
         C_MAIN := mainwin.cpp
         DIRS := mainstd mainwin maindllh maindllp
      else
         ifeq ($(HB_PLATFORM),os2)
            C_MAIN := mainstd.cpp
            DIRS := mainstd maindllh
         else
            ifeq ($(HB_PLATFORM)-$(HB_COMPILER),dos-watcom)
               DIRS := mainstd
            endif
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
   short.cpp \
   vm.cpp \
   $(C_MAIN) \

PRG_SOURCES := \
   harbinit.prg \

LIBNAME := hbvm

ifneq ($(HB_MT),yes)
ifeq ($(HB_PLATFORM),dos)
ifneq ($(HB_COMPILER),djgpp)
ifneq ($(HB_COMPILER),watcom)
   HB_MT := no
endif
endif
endif
endif
ifneq ($(HB_MT),no)
   DIRS += vmmt
endif

include $(TOP)$(ROOT)config/lib.mk
ifneq ($(DIRS),)
   include $(TOP)$(ROOT)config/dir.mk
endif
