PRG_C_SOURCES := $(PRG_SOURCES:.prg=.cpp)
PRG_OBJS := $(PRG_SOURCES:.prg=$(OBJ_EXT))
PRG_EXES := $(PRG_SOURCES:.prg=$(BIN_EXT))

PRG_MAIN_OBJ := $(PRG_MAIN:.prg=$(OBJ_EXT))

ALL_PRG_OBJS := $(PRG_OBJS) $(PRG_MAIN_OBJ)

$(PRG_OBJS) : %$(OBJ_EXT) : %.cpp

$(PRG_C_SOURCES) : %.cpp : $(GRANDP)%.prg
