ROOT := ../../

CPP_SOURCES := \
   pptable.cpp \
   ppcore.cpp \
   pplib.cpp \
   pplib2.cpp \

LIBNAME := hbpp

LIBS := \
   hbnortl \
   hbcommon \

include $(TOP)$(ROOT)config/lib.mk

ifneq ($(HB_PLATFORM),)
ifneq ($(HB_COMPILER),)

HB_VERHEADER := $(TOP)$(ROOT)include/hbverbld.h

BIN_FILE := $(BIN_DIR)/hbpp$(BIN_EXT)

vpath hbpp$(BIN_EXT) $(BIN_DIR)

pptable.cpp : hbpp$(BIN_EXT)
	$(HB_HOST_BIN_DIR)/hbpp$(HB_HOST_BIN_EXT) $(TOP)$(ROOT)include/hbstdgen.ch -opptable.cpp -q -c$(TOP)$(ROOT) -v$(HB_VERHEADER)
	-@$(RM) hbpp$(OBJ_EXT) $(if $(OBJ_DYN_SUFFIX),hbpp$(OBJ_DYN_SUFFIX)$(OBJ_EXT),)

ifneq ($(HB_BUILD_PARTS),lib)
   ifneq ($(HB_INSTALL_BIN),)
      INSTALL_FILES := $(BIN_DIR)/hbpp$(BIN_EXT)
      INSTALL_DIR := $(HB_INSTALL_BIN)
      include $(TOP)$(ROOT)config/install.mk
   endif
endif

clean::
	$(if $(wildcard $(HB_VERHEADER)), -@$(RM) $(subst /,$(DIRSEP),$(HB_VERHEADER)),)

endif
endif
