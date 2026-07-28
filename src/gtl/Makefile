ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),wce)
   ifneq ($(HB_COMPILER),pcc)
      HB_PRGFLAGS += -gc3
   endif
endif

HB_PRGFLAGS += -ala

CPP_SOURCES := \
   gt.cpp \
   gtapi.cpp \
   gtchrmap.cpp \
   gtclip.cpp \
   gtfunc.cpp \
   gtkbstat.cpp \
   gtkeycod.cpp \
   gtsys.cpp \
   gttone.cpp \
   gx.cpp \
   hbgtcore.cpp \
   xsavescr.cpp \

PRG_SOURCES := \

LIBNAME := hbgtl

DIRS := \
   gtcgi \
   gtcrs \
   gtgui \
   gtpca \
   gtsln \
   gtstd \
   gttrm \
   gtwin \
   gtwvt \
   gtxwc \

HB_CFLAGS += -D_HB_API_INTERNAL_

ifneq ($(HB_HAS_ZLIB),)

   ifneq ($(HB_HAS_PCRE),)
      HB_CFLAGS += -DHB_HAS_PCRE $(foreach d,$(HB_HAS_PCRE),-I$(d))
      ifneq ($(HB_HAS_PCRE_LOCAL),)
         HB_CFLAGS += -DPCRE_STATIC
      endif
   endif
   ifneq ($(HB_HAS_ZLIB),)
      HB_CFLAGS += -DHB_HAS_ZLIB $(foreach d,$(HB_HAS_ZLIB),-I$(d))
   endif
   ifneq ($(HB_HAS_WATT),)
      HB_CFLAGS += -DHB_HAS_WATT $(foreach d,$(HB_HAS_WATT),-I$(d))
   endif

   include $(TOP)$(ROOT)config/lib.mk
   include $(TOP)$(ROOT)config/dir.mk
else
   $(error ! Error: zlib component required, but not available)
endif
