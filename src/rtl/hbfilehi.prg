//
// High-level portable file functions.
//
// Copyright 2009-2017 Viktor Szakats (vsz.me/hb)
//

// $HB_BEGIN_LICENSE$
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file LICENSE.txt.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
//
// As a special exception, the Harbour Project gives permission for
// additional uses of the text contained in its release of Harbour.
//
// The exception is that, if you link the Harbour libraries with other
// files to produce an executable, this does not by itself cause the
// resulting executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of
// linking the Harbour library code into it.
//
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.
//
// This exception applies only to the code released by the Harbour
// Project under the name Harbour.  If you copy code from other
// Harbour Project or Free Software Foundation releases into a copy of
// Harbour, as the General Public License permits, the exception does
// not apply to the code that you add in this way.  To avoid misleading
// anyone as to the status of such modified files, you must delete
// this exception notice from them.
//
// If you write modifications of your own for Harbour, it is your choice
// whether to permit this exception to apply to your modifications.
// If you do not wish that, delete this exception notice.
// $HB_END_LICENSE$

#define _ISDRIVESPEC(cDir) (!Empty(hb_osDriveSeparator()) .AND. Right(cDir, Len(hb_osDriveSeparator())) == hb_osDriveSeparator())

// NOTE: Can hurt if there are symlinks on the way.
FUNCTION hb_PathNormalize(cPath)

   LOCAL aDir
   LOCAL cDir

   IF !hb_IsString(cPath)
      RETURN ""
   ENDIF

   IF !Empty(cPath)

      aDir := hb_ATokens(cPath, hb_ps())

      FOR EACH cDir IN aDir DESCEND

         IF cDir == "." .OR. ;
            (Empty(cDir) .AND. ;
            !cDir:__enumIsLast() .AND. ;
            (cDir:__enumIndex() > 2 .OR. ;
            (cDir:__enumIndex() == 2 .AND. !Empty(aDir[1]))))

            hb_ADel(aDir, cDir:__enumIndex(), .T.)

         ELSEIF !(cDir == "..") .AND. ;
            !Empty(cDir) .AND. ;
            !_ISDRIVESPEC(cDir)

            IF !cDir:__enumIsLast() .AND. ;
               aDir[cDir:__enumIndex() + 1] == ".."
               hb_ADel(aDir, cDir:__enumIndex() + 1, .T.)
               hb_ADel(aDir, cDir:__enumIndex(), .T.)
            ENDIF
         ENDIF
      NEXT

      cPath := ""
      FOR EACH cDir IN aDir
         cPath += cDir
         IF !cDir:__enumIsLast()
            cPath += hb_ps()
         ENDIF
      NEXT

      IF Empty(cPath)
         cPath := "." + hb_ps()
      ENDIF
   ENDIF

   RETURN cPath

FUNCTION hb_PathJoin(cPathA, cPathR)

   LOCAL cDirA
   LOCAL cDirR
   LOCAL cDriveR
   LOCAL cNameR
   LOCAL cExtR

   IF !hb_IsString(cPathR)
      RETURN ""
   ENDIF

   IF !hb_IsString(cPathA) .OR. Empty(cPathA)
      RETURN cPathR
   ENDIF

   hb_FNameSplit(cPathR, @cDirR, @cNameR, @cExtR, @cDriveR)

   IF !Empty(cDriveR) .OR. (!Empty(cDirR) .AND. Left(cDirR, 1) $ hb_osPathDelimiters())
      RETURN cPathR
   ENDIF

   IF Empty(cDirA := hb_FNameDir(cPathA))
      RETURN cPathR
   ENDIF

   RETURN hb_FNameMerge(cDirA + cDirR, cNameR, cExtR)

FUNCTION hb_PathRelativize(cPathBase, cPathTarget, lForceRelative)

   LOCAL tmp
   LOCAL aPathBase
   LOCAL aPathTarget
   LOCAL cTestBase
   LOCAL cTestTarget
   LOCAL cTargetFileName

   IF !hb_IsString(cPathBase) .OR. !hb_IsString(cPathTarget)
      RETURN ""
   ENDIF

   cPathBase   := hb_PathJoin(hb_DirBase(), hb_DirSepAdd(cPathBase))
   cPathTarget := hb_PathJoin(hb_DirBase(), cPathTarget)

   // TODO: Optimize to operate on strings instead of arrays

   aPathBase   := s_FN_ToArray(cPathBase)
   aPathTarget := s_FN_ToArray(cPathTarget, @cTargetFileName)

   tmp := 1
   cTestBase := ""
   cTestTarget := ""
   DO WHILE tmp <= Len(aPathTarget) .AND. tmp <= Len(aPathBase)
      cTestBase   += aPathBase[tmp]
      cTestTarget += aPathTarget[tmp]
      IF !hb_FileMatch(cTestBase, cTestTarget)
         EXIT
      ENDIF
      ++tmp
   ENDDO

   IF tmp > Len(aPathTarget) .AND. tmp > Len(aPathBase)
      tmp--
   ENDIF

   IF tmp == Len(aPathBase)
      RETURN s_FN_FromArray(aPathTarget, tmp, cTargetFileName, "")
   ENDIF

   // Different drive spec. There is no way to solve that using relative dirs.
   IF !Empty(hb_osDriveSeparator()) .AND. ;
      tmp == 1 .AND. ( ;
      Right(aPathBase[1]  , Len(hb_osDriveSeparator())) == hb_osDriveSeparator() .OR. ;
      Right(aPathTarget[1], Len(hb_osDriveSeparator())) == hb_osDriveSeparator())
      RETURN cPathTarget
   ENDIF

   // Force to return relative paths even when base is different.
   IF hb_defaultValue(lForceRelative, .T.) .AND. ;
      hb_DirExists(cPathBase + (cTestTarget := s_FN_FromArray(aPathTarget, tmp, cTargetFileName, Replicate(".." + hb_ps(), Len(aPathBase) - tmp))))
      RETURN cTestTarget
   ENDIF

   RETURN cPathTarget

STATIC FUNCTION s_FN_ToArray(cPath, /* @ */ cFileName)

   LOCAL cDir
   LOCAL cName
   LOCAL cExt

   hb_FNameSplit(cPath, @cDir, @cName, @cExt)

   IF !Empty(cName) .OR. !Empty(cExt)
      cFileName := cName + cExt
   ENDIF

   RETURN hb_ATokens(cDir, hb_ps())

STATIC FUNCTION s_FN_FromArray(aPath, nFrom, cFileName, cDirPrefix)

   LOCAL nTo := Len(aPath)
   LOCAL cDir
   LOCAL tmp

   IF nFrom > Len(aPath) .OR. nTo < 1
      RETURN ""
   ENDIF

   IF nFrom < 1
      nFrom := 1
   ENDIF

   cDir := ""
   FOR tmp := nFrom TO nTo
      cDir += aPath[tmp]
      IF nFrom < nTo
         cDir += hb_ps()
      ENDIF
   NEXT

   RETURN hb_FNameMerge(hb_DirSepDel(hb_DirSepAdd(cDirPrefix) + cDir), cFileName)

FUNCTION hb_DirSepAdd(cDir)

   IF !hb_IsString(cDir)
      RETURN ""
   ENDIF

   IF !Empty(cDir) .AND. ;
      !_ISDRIVESPEC(cDir) .AND. ;
      !Right(cDir, 1) == hb_ps()

      cDir += hb_ps()
   ENDIF

   RETURN cDir

FUNCTION hb_DirSepDel(cDir)

   IF !hb_IsString(cDir)
      RETURN ""
   ENDIF

   IF Empty(hb_osDriveSeparator())
      DO WHILE Len(cDir) > 1 .AND. Right(cDir, 1) == hb_ps() .AND. ;
         !cDir == hb_ps() + hb_ps()

         cDir := hb_StrShrink(cDir)
      ENDDO
   ELSE
      DO WHILE Len(cDir) > 1 .AND. Right(cDir, 1) == hb_ps() .AND. ;
         !cDir == hb_ps() + hb_ps() .AND. ;
         !Right(cDir, Len(hb_osDriveSeparator()) + 1) == hb_osDriveSeparator() + hb_ps()

         cDir := hb_StrShrink(cDir)
      ENDDO
   ENDIF

   RETURN cDir

FUNCTION hb_DirSepToOS(cFileName)

   IF hb_IsString(cFileName)
      RETURN StrTran(cFileName, IIf(hb_ps() == "\", "/", "\"), hb_ps())
   ENDIF

   RETURN ""

FUNCTION hb_DirBuild(cDir)

   LOCAL cDirTemp
   LOCAL cDirItem
   LOCAL tmp

   IF !hb_IsString(cDir)
      RETURN .F.
   ENDIF

   cDir := hb_PathNormalize(cDir)

   IF !hb_DirExists(cDir)

      cDir := hb_DirSepAdd(cDir)

      IF !hb_osDriveSeparator() == "" .AND. ;
         (tmp := At(hb_osDriveSeparator(), cDir)) > 0
         cDirTemp := Left(cDir, tmp)
         cDir := SubStr(cDir, tmp + 1)
#ifdef __PLATFORM__WINDOWS
      ELSEIF hb_LeftEq(cDir, "\\") // UNC Path, network share
         cDirTemp := Left(cDir, hb_At("\", cDir, 3))
         cDir := SubStr(cDir, Len(cDirTemp) + 1)
#endif
      ELSEIF hb_LeftEq(cDir, hb_ps())
         cDirTemp := Left(cDir, 1)
         cDir := SubStr(cDir, 2)
      ELSE
         cDirTemp := ""
      ENDIF

      FOR EACH cDirItem IN hb_ATokens(cDir, hb_ps())
         IF !Right(cDirTemp, 1) == hb_ps() .AND. !cDirTemp == ""
            cDirTemp += hb_ps()
         ENDIF
         IF !cDirItem == ""  // Skip root path, if any
            cDirTemp += cDirItem
            IF hb_FileExists(cDirTemp)
               RETURN .F.
            ELSEIF !hb_DirExists(cDirTemp)
               IF hb_DirCreate(cDirTemp) != 0
                  RETURN .F.
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN .T.

FUNCTION hb_DirUnbuild(cDir)

   LOCAL tmp

   IF !hb_IsString(cDir)
      RETURN .F.
   ENDIF

   IF hb_DirExists(cDir)

      cDir := hb_DirSepDel(cDir)

      DO WHILE !Empty(cDir)
         IF hb_DirExists(cDir) .AND. ;
            hb_DirDelete(cDir) != 0
            RETURN .F.
         ENDIF
         IF (tmp := RAt(hb_ps(), cDir)) == 0  // FIXME: use hb_URAt() function
            EXIT
         ENDIF
         cDir := Left(cDir, tmp - 1)
         IF !hb_osDriveSeparator() == "" .AND. ;
            Right(cDir, Len(hb_osDriveSeparator())) == hb_osDriveSeparator()
            EXIT
         ENDIF
      ENDDO
   ENDIF

   RETURN .T.
