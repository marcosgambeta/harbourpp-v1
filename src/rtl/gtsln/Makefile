ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtsln

CPP_SOURCES := \
   gtsln.cpp \
   kbsln.cpp \
   mousesln.cpp \

HB_CFLAGS += -D_HB_API_INTERNAL_

# TODO: Add these to lib path list
# LDFLAGS := -L/usr/local/lib $(LDFLAGS)
# LDFLAGS := -L/usr/lib/slang -L/usr/local/lib -L/usr/local/lib/slang $(LDFLAGS)

ifneq ($(HB_HAS_SLANG),)

   HB_CFLAGS += $(foreach d,$(HB_HAS_SLANG),-I$(d))

   ifneq ($(HB_HAS_GPM),)
      HB_CFLAGS += -DHB_HAS_GPM
   endif

   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := component not found
   include $(TOP)$(ROOT)config/none.mk
endif
