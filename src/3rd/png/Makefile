ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := png

HB_BUILD_WARN := no
HB_BUILD_MODE := c
HB_USER_CFLAGS :=

C_SOURCES := \
   png.c \
   pngerror.c \
   pngget.c \
   pngmem.c \
   pngpread.c \
   pngread.c \
   pngrio.c \
   pngrtran.c \
   pngrutil.c \
   pngset.c \
   pngtrans.c \
   pngwio.c \
   pngwrite.c \
   pngwtran.c \
   pngwutil.c \

ifeq ($(filter $(HB_COMPILER),poccarm xcc tcc),)

   _DET_DSP_NAME := libpng
   _DET_VAR_INC_ := HB_INC_PNG
   _DET_VAR_HAS_ := HB_HAS_PNG
   _DET_FLT_PLAT :=
   _DET_FLT_COMP :=
   _DET_INC_DEFP := /usr/include /boot/develop/headers/3rdparty
   _DET_INC_LOCL := src/3rd/png
   _DET_INC_HEAD := /png.h
   include $(TOP)$(ROOT)config/detfun.mk

   ifneq ($(HB_HAS_PNG_LOCAL),)

      ifneq ($(HB_HAS_ZLIB),)

         HB_CFLAGS += $(foreach d,$(HB_HAS_ZLIB),-I$(d))
         HB_CFLAGS_DYN := -DPNG_BUILD_DLL

         HB_CFLAGS += -DPNG_NO_STDIO

         ifneq ($(filter $(HB_COMPILER),pocc pocc64 poccarm),)
            HB_CFLAGS += -DPNG_ALLOCATED
         endif
         HB_CFLAGS += -DPNG_ARM_NEON_OPT=0

         ifeq ($(HB_COMPILER),bcc64)
            HB_CFLAGS += -std=c11 -Wno-visibility -Wno-missing-declarations -Wno-tautological-constant-out-of-range-compare
         endif

         include $(TOP)$(ROOT)config/lib.mk
      else
         HB_SKIP_REASON := $(_DET_RES_TEXT)
         include $(TOP)$(ROOT)config/none.mk
      endif
   else
      HB_SKIP_REASON := unused
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   HB_SKIP_REASON := compiler not supported
   include $(TOP)$(ROOT)config/none.mk
endif

# ORIGIN http://libpng.org/pub/png/libpng.html
# VER    1.6.42
# URL    http://downloads.sourceforge.net/libpng/libpng-1.6.42.tar.gz
# DIFF   png.diff (not used - using original files)
#
# MAP    LICENSE
# MAP    png.c
# MAP    pngconf.h
# MAP    scripts/pnglibconf.h.prebuilt           pnglibconf.h
# MAP    pngerror.c
# MAP    pngget.c
# MAP    png.h
# MAP    pngmem.c
# MAP    pngpread.c
# MAP    pngpriv.h
# MAP    pngread.c
# MAP    pngrio.c
# MAP    pngrtran.c
# MAP    pngrutil.c
# MAP    pngset.c
# MAP    pngtrans.c
# MAP    pngwio.c
# MAP    pngwrite.c
# MAP    pngwtran.c
# MAP    pngwutil.c
# MAP    pnginfo.h
# MAP    pngstruct.h
# MAP    pngdebug.h
