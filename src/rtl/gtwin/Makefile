ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtwin

CPP_SOURCES := \
   gtwin.cpp \

ifeq ($(HB_PLATFORM),win)
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := platform not supported
   include $(TOP)$(ROOT)config/none.mk
endif
