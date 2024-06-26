all : first

RES_EXT := .res
BIN_EXT := .exe
DYN_EXT := .dll

HB_CFLAGS += -DUNICODE

HB_GT_LIBS += gtwvt gtgui gtwin

# kernel32: needed by some compilers (watcom)
# user32: *Clipboard*(), GetKeyState(), GetKeyboardState(), SetKeyboardState(), gtwvt stuff
# ws2_32/wsock32: hbsocket
# ws2_32: WSAIoctl()
# iphlpapi: hbsocket->GetAdaptersInfo()
# advapi32: GetUserName()
# gdi32: gtwvt

# unicows lib must come after user libs and before Windows system libs
ifneq ($(wildcard $(TOP)$(ROOT)lib/3rd/$(HB_PLATFORM)/$(HB_COMPILER)),)
   3RDLIB_DIR := $(TOP)$(ROOT)lib/3rd/$(HB_PLATFORM)/$(HB_COMPILER)
   3RDLIBS := unicows
endif

ifeq ($(HB_COMPILER),bcc64)
   SYSLIBS += winmm kernel32 user32 ws2_32 iphlpapi advapi32 gdi32 winhttp winspool mpr comctl32 msimg32 opengl32 gdiplus
else
   SYSLIBS += winmm kernel32 user32 ws2_32 iphlpapi advapi32 gdi32 winhttp winspool mpr comctl32 msimg32 opengl32 version gdiplus
endif   
