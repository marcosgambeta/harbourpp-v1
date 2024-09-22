ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),wce)
   ifneq ($(HB_COMPILER),pcc)
      HB_PRGFLAGS += -gc3
   endif
endif

CPP_SOURCES := \
   dbcmd.cpp \
   dbcmd53.cpp \
   dbcmdhb.cpp \
   dbdetach.cpp \
   dbdrop.cpp \
   dbexists.cpp \
   dbf1.cpp \
   dbnubs.cpp \
   dbrename.cpp \
   dbsql.cpp \
   delim1.cpp \
   fieldhb.cpp \
   ordcount.cpp \
   ordwldsk.cpp \
   workarea.cpp \
   wacore.cpp \
   wafunc.cpp \
   rddinfo.cpp \
   rddshort.cpp \
   sdf1.cpp \

PRG_SOURCES := \
   dbdelim.prg \
   dbjoin.prg \
   dblist.prg \
   dbsdf.prg \
   dbsort.prg \
   dbstrux.prg \
   dbstruxu.prg \
   dbtotal.prg \
   dbupdat.prg \
   rddord.prg \
   rddordu.prg \
   rddsys.prg \

LIBNAME := hbrdd

DIRS = \
   nulsys \
   dbfntx \
   dbfnsx \
   dbfcdx \
   dbffpt \
   hbsix \
   hsx \
   usrrdd \
   usrrdd/rdds \

HB_CFLAGS += -D_HB_API_INTERNAL_

include $(TOP)$(ROOT)config/lib.mk
include $(TOP)$(ROOT)config/dir.mk
