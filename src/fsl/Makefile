ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

#ifneq ($(HB_PLATFORM),wce) (deprecated)
#   ifneq ($(HB_COMPILER),pcc) (deprecated)
      HB_PRGFLAGS += -gc3
#   endif
#endif

HB_PRGFLAGS += -ala

CPP_SOURCES := \
   copyfile.cpp \
   dirdrive.cpp \
   direct.cpp \
   diskspac.cpp \
   disksphb.cpp \
   file.cpp \
   filebuf.cpp \
   filebufd.cpp \
   filehb.cpp \
   filesys.cpp \
   fnsplit.cpp \
   fscopy.cpp \
   fserr.cpp \
   fslink.cpp \
   fssize.cpp \
   fstemp.cpp \
   hbfeof.cpp \
   hbfile.cpp \
   hbfilehc.cpp \
   iousr.cpp \
   philes.cpp \
   philes53.cpp \
   spfiles.cpp \
   vfile.cpp \

PRG_SOURCES := \
   adir.prg \
   dirscan.prg \
   hbfilehi.prg \
   vfilehi.prg \

LIBNAME := hbfsl

DIRS := \

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
