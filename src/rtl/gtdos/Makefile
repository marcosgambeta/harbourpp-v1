ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtdos

CPP_SOURCES := \
   gtdos.cpp \

ifeq ($(HB_PLATFORM),dos)
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := platform not supported
   include $(TOP)$(ROOT)config/none.mk
endif
