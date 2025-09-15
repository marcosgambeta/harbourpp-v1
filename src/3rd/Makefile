ROOT := ../../

DIRS := \
   png \
   jpeg \
   pcre \
   zlib \

# TODO: error with tiff and icx64 (Intel oneAPI compiler)
# In file included from ../../../tif_aux.c:33:
# ../../../tiffiop.h(56,14): error: conflicting types for 'lfind'
# extern void *lfind(const void *, const void *, size_t *, size_t,
#              ^
# C:\Program Files (x86)\Windows Kits\10\include\10.0.26100.0\ucrt\corecrt_search.h(195,28): note: previous declaration is here
#     _ACRTIMP void* __cdecl lfind(
#                            ^

# disable libtiff for icx64 (Intel oneAPI compiler)
ifneq ($(HB_COMPILER),icx64)
   DIRS += tiff
endif

include $(TOP)$(ROOT)config/dir.mk
