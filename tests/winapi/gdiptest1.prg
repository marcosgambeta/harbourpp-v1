//
// WinApi test
//
// Copyright (c) 2024 Marcos Antonio Gambeta <marcosgambeta AT outlook DOT com>
//

// Compile with:
// hbmk2 gdiptest1

REQUEST HB_GT_WVT_DEFAULT

PROCEDURE Main()

   LOCAL pImage
   LOCAL nWidth
   LOCAL nHeight

   ? "waGdiplusStartup=", waGdiplusStartup()

   ? "waGdipLoadImageFromFile=", waGdipLoadImageFromFile("harbour.gif", @pImage)
   ? "pImage=", pImage

   ? "waGdipGetImageDimension=", waGdipGetImageDimension(pImage, @nWidth, @nHeight)
   ? "nWidth=", nWidth
   ? "nHeight=", nHeight

   ? "waGdipDisposeImage=", waGdipDisposeImage(pImage)

   ? waGdiplusShutdown()

   WAIT

RETURN
