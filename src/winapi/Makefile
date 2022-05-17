ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),wce)
   ifneq ($(HB_COMPILER),pcc)
      HB_PRGFLAGS += -gc3
   endif
endif

CPP_SOURCES := \
   commctrl.cpp \
   wingdi.cpp \

PRG_SOURCES :=

LIBNAME := hbwinapi

DIRS =

include $(TOP)$(ROOT)config/lib.mk
include $(TOP)$(ROOT)config/dir.mk
