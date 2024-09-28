ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtgui

CPP_SOURCES := \
   gtgui.cpp \

HB_CFLAGS += -D_HB_API_INTERNAL_

ifneq ($(filter $(HB_PLATFORM),win wce),)
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := platform not supported
   include $(TOP)$(ROOT)config/none.mk
endif
