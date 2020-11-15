ROOT := ../../

CPP_SOURCES := \

C_MAIN := harbour.cpp

ifneq ($(filter $(HB_PLATFORM),win wce),)
RC_SOURCES := harbour.rc
endif

LIBS := \
   hbcplr \
   hbpp \
   hbnortl \
   hbcommon \

include $(TOP)$(ROOT)config/bin.mk
