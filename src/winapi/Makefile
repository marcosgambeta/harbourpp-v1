ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),wce)
   ifneq ($(HB_COMPILER),pcc)
      HB_PRGFLAGS += -gc3
   endif
endif

CPP_SOURCES := \
   commctrl.cpp \
   mmsystem.cpp \
   winbase.cpp \
   wingdi.cpp \
   winhttp.cpp \
   winnetwk.cpp \
   winreg.cpp \
   winspool.cpp \
   winuser.cpp \

PRG_SOURCES := \
   struct_point.prg \
   struct_rect.prg \
   struct_size.prg \

LIBNAME := hbwinapi

DIRS =

include $(TOP)$(ROOT)config/lib.mk
include $(TOP)$(ROOT)config/dir.mk
