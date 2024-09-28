ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtcrs

CPP_SOURCES := \
   gtcrs.cpp \

HB_CFLAGS += -D_HB_API_INTERNAL_

ifneq ($(HB_HAS_CURSES),)

   HB_CFLAGS += $(foreach d,$(HB_HAS_CURSES),-I$(d))

   ifneq ($(HB_HAS_GPM),)
      HB_CFLAGS += -DHB_HAS_GPM
   endif

   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := component not found
   include $(TOP)$(ROOT)config/none.mk
endif
