ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),wce)
   ifneq ($(HB_COMPILER),pcc)
      HB_PRGFLAGS += -gc3
   endif
endif

CPP_SOURCES := \
   bcrypt.cpp \
   commapi.cpp \
   commctrl.cpp \
   mmsystem.cpp \
   utils.cpp \
   winbase.cpp \
   wingdi.cpp \
   winhttp.cpp \
   winnetwk.cpp \
   winreg.cpp \
   winspool.cpp \
   winuser.cpp \

PRG_SOURCES := \
   struct_abc.prg \
   struct_abcfloat.prg \
   struct_bitmapinfoheader.prg \
   struct_iconinfo.prg \
   struct_imagelistdrawparams.prg \
   struct_initcommoncontrolsex.prg \
   struct_logbrush.prg \
   struct_logpen.prg \
   struct_msg.prg \
   struct_pixelformatdescriptor.prg \
   struct_point.prg \
   struct_rect.prg \
   struct_rgbquad.prg \
   struct_size.prg \
   struct_timecaps.prg \

LIBNAME := hbwinapi

DIRS =

include $(TOP)$(ROOT)config/lib.mk
include $(TOP)$(ROOT)config/dir.mk
