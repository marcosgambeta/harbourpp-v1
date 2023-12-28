/*
 * WinApi test
 *
 * Copyright (c) 2023 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
 *
 */

// Compile with:
// hbmk2 getdevicecaps -lopengl32

REQUEST HB_GT_WVT_DEFAULT

#include "hbgtinfo.ch"
#include "winapi_wingdi.ch"

PROCEDURE Main()

   LOCAL pWinHandle
   LOCAL pDC

   CLS

   pWinHandle := hb_GtInfo(HB_GTI_WINHANDLE)
   ? "pWinHandle=", pWinHandle

   ?

   pDC := waGetDC(pWinHandle)
   ? "pDC=", pDC

   ?

   ? "DRIVERVERSION..:", waGetDeviceCaps(pDC, DRIVERVERSION)
   ? "TECHNOLOGY.....:", waGetDeviceCaps(pDC, TECHNOLOGY)
   ? "HORZSIZE.......:", waGetDeviceCaps(pDC, HORZSIZE)
   ? "VERTSIZE.......:", waGetDeviceCaps(pDC, VERTSIZE)
   ? "HORZRES........:", waGetDeviceCaps(pDC, HORZRES)
   ? "VERTRES........:", waGetDeviceCaps(pDC, VERTRES)
   ? "BITSPIXEL......:", waGetDeviceCaps(pDC, BITSPIXEL)
   ? "PLANES.........:", waGetDeviceCaps(pDC, PLANES)
   ? "NUMBRUSHES.....:", waGetDeviceCaps(pDC, NUMBRUSHES)
   ? "NUMPENS........:", waGetDeviceCaps(pDC, NUMPENS)
   ? "NUMMARKERS.....:", waGetDeviceCaps(pDC, NUMMARKERS)
   ? "NUMFONTS.......:", waGetDeviceCaps(pDC, NUMFONTS)
   ? "NUMCOLORS......:", waGetDeviceCaps(pDC, NUMCOLORS)
   ? "PDEVICESIZE....:", waGetDeviceCaps(pDC, PDEVICESIZE)
   ? "CURVECAPS......:", waGetDeviceCaps(pDC, CURVECAPS)
   ? "LINECAPS.......:", waGetDeviceCaps(pDC, LINECAPS)
   ? "POLYGONALCAPS..:", waGetDeviceCaps(pDC, POLYGONALCAPS)
   ? "TEXTCAPS.......:", waGetDeviceCaps(pDC, TEXTCAPS)
   ? "CLIPCAPS.......:", waGetDeviceCaps(pDC, CLIPCAPS)
   ? "RASTERCAPS.....:", waGetDeviceCaps(pDC, RASTERCAPS)

   WAIT

   ? "ASPECTX........:", waGetDeviceCaps(pDC, ASPECTX)
   ? "ASPECTY........:", waGetDeviceCaps(pDC, ASPECTY)
   ? "ASPECTXY.......:", waGetDeviceCaps(pDC, ASPECTXY)
   ? "LOGPIXELSX.....:", waGetDeviceCaps(pDC, LOGPIXELSX)
   ? "LOGPIXELSY.....:", waGetDeviceCaps(pDC, LOGPIXELSY)
   ? "SIZEPALETTE....:", waGetDeviceCaps(pDC, SIZEPALETTE)
   ? "NUMRESERVED....:", waGetDeviceCaps(pDC, NUMRESERVED)
   ? "COLORRES.......:", waGetDeviceCaps(pDC, COLORRES)
   ? "PHYSICALWIDTH..:", waGetDeviceCaps(pDC, PHYSICALWIDTH)
   ? "PHYSICALHEIGHT.:", waGetDeviceCaps(pDC, PHYSICALHEIGHT)
   ? "PHYSICALOFFSETX:", waGetDeviceCaps(pDC, PHYSICALOFFSETX)
   ? "PHYSICALOFFSETY:", waGetDeviceCaps(pDC, PHYSICALOFFSETY)
   ? "SCALINGFACTORX.:", waGetDeviceCaps(pDC, SCALINGFACTORX)
   ? "SCALINGFACTORY.:", waGetDeviceCaps(pDC, SCALINGFACTORY)
   ? "VREFRESH.......:", waGetDeviceCaps(pDC, VREFRESH)
   ? "DESKTOPVERTRES.:", waGetDeviceCaps(pDC, DESKTOPVERTRES)
   ? "DESKTOPHORZRES.:", waGetDeviceCaps(pDC, DESKTOPHORZRES)
   ? "BLTALIGNMENT...:", waGetDeviceCaps(pDC, BLTALIGNMENT)
   ? "SHADEBLENDCAPS.:", waGetDeviceCaps(pDC, SHADEBLENDCAPS)
   ? "COLORMGMTCAPS..:", waGetDeviceCaps(pDC, COLORMGMTCAPS)

   WAIT

RETURN
