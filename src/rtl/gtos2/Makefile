ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtos2

CPP_SOURCES := \
   gtos2.cpp \

ifeq ($(HB_PLATFORM),os2)
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := platform not supported
   include $(TOP)$(ROOT)config/none.mk
endif
