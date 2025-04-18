//
// Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "hbhpdf.hpp"

/* HPDF_LinkAnnot_SetHighlightMode( hAnnot, nHilightMode ) --> hStatus
       nHilightMode ==
   HPDF_ANNOT_NO_HIGHTLIGHT       1     No highlighting.
   HPDF_ANNOT_INVERT_BOX          2     Invert the contents of the area of annotation.
   HPDF_ANNOT_INVERT_BORDER       3     Invert the annotation's border.
   HPDF_ANNOT_DOWN_APPEARANCE     4     Dent the annotation.
 */
HB_FUNC(HPDF_LINKANNOT_SETHIGHLIGHTMODE)
{
  hb_retnl(static_cast<long>(HPDF_LinkAnnot_SetHighlightMode(static_cast<HPDF_Annotation>(hb_parptr(1)),
                                                             static_cast<HPDF_AnnotHighlightMode>(hb_parni(2)))));
}

/* HPDF_LinkAnnot_SetBorderStyle( hAnnot, nWidth, nDashOn, nDashOff ) --> hStatus */
HB_FUNC(HPDF_LINKANNOT_SETBORDERSTYLE)
{
  hb_retnl(static_cast<long>(
      HPDF_LinkAnnot_SetBorderStyle(static_cast<HPDF_Annotation>(hb_parptr(1)), static_cast<HPDF_REAL>(hb_parnd(2)),
                                    static_cast<HPDF_UINT16>(hb_parni(3)), static_cast<HPDF_UINT16>(hb_parni(4)))));
}

/* HPDF_TextAnnot_SetIcon( hAnnot, nIconID ) --> hStatus
       nIconID
   HPDF_ANNOT_ICON_COMMENT
   HPDF_ANNOT_ICON_KEY
   HPDF_ANNOT_ICON_NOTE
   HPDF_ANNOT_ICON_HELP
   HPDF_ANNOT_ICON_NEW_PARAGRAPH
   HPDF_ANNOT_ICON_PARAGRAPH
   HPDF_ANNOT_ICON_INSERT
 */
HB_FUNC(HPDF_TEXTANNOT_SETICON)
{
  hb_retnl(static_cast<long>(
      HPDF_TextAnnot_SetIcon(static_cast<HPDF_Annotation>(hb_parptr(1)), static_cast<HPDF_AnnotIcon>(hb_parni(2)))));
}

/* HPDF_TextAnnot_SetOpened(hAnnot, lOpened) --> hStatus */
HB_FUNC(HPDF_TEXTANNOT_SETOPENED)
{
  hb_retnl(static_cast<long>(
      HPDF_TextAnnot_SetOpened(static_cast<HPDF_Annotation>(hb_parptr(1)), hb_parl(2) ? HPDF_TRUE : HPDF_FALSE)));
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateFreeTextAnnot  (HPDF_Page       page,
                        HPDF_Rect       rect,
                        const char     *text,
                        HPDF_Encoder    encoder);
 */
HB_FUNC(HPDF_PAGE_CREATEFREETEXTANNOT)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Rect rc;

  rc.left = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  rc.top = static_cast<HPDF_REAL>(hb_parvnd(2, 2));
  rc.right = static_cast<HPDF_REAL>(hb_parvnd(2, 3));
  rc.bottom = static_cast<HPDF_REAL>(hb_parvnd(2, 4));

  hb_retptr(HPDF_Page_CreateFreeTextAnnot(static_cast<HPDF_Page>(hb_parptr(1)), rc, hb_parc(3),
                                          static_cast<HPDF_Encoder>(hb_parptr(4))));
#else
  hb_retptr(nullptr);
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateLineAnnot  (HPDF_Page       page,
                     const char     *text,
                     HPDF_Encoder    encoder);
 */
HB_FUNC(HPDF_PAGE_CREATELINEANNOT)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retptr(HPDF_Page_CreateLineAnnot(static_cast<HPDF_Page>(hb_parptr(1)), hb_parc(2),
                                      static_cast<HPDF_Encoder>(hb_parptr(3))));
#else
  hb_retptr(nullptr);
#endif
}
/*
   HPDF_Annotation
   HPDF_Page_CreateTextMarkupAnnot (HPDF_Page     page,
                        HPDF_Rect      rect,
                        const char     *text,
                        HPDF_Encoder   encoder,
                        HPDF_AnnotType subType);
 */
HB_FUNC(HPDF_PAGE_CREATETEXTMARKUPANNOT)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Rect rc;

  rc.left = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  rc.top = static_cast<HPDF_REAL>(hb_parvnd(2, 2));
  rc.right = static_cast<HPDF_REAL>(hb_parvnd(2, 3));
  rc.bottom = static_cast<HPDF_REAL>(hb_parvnd(2, 4));

  hb_retptr(HPDF_Page_CreateTextMarkupAnnot(static_cast<HPDF_Page>(hb_parptr(1)), rc, hb_parc(3),
                                            static_cast<HPDF_Encoder>(hb_parptr(4)),
                                            static_cast<HPDF_AnnotType>(hb_parni(5))));
#else
  hb_retptr(nullptr);
#endif
}
/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateHighlightAnnot  (HPDF_Page   page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
 */
HB_FUNC(HPDF_PAGE_CREATEHIGHLIGHTANNOT)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Rect rc;

  rc.left = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  rc.top = static_cast<HPDF_REAL>(hb_parvnd(2, 2));
  rc.right = static_cast<HPDF_REAL>(hb_parvnd(2, 3));
  rc.bottom = static_cast<HPDF_REAL>(hb_parvnd(2, 4));

  hb_retptr(HPDF_Page_CreateHighlightAnnot(static_cast<HPDF_Page>(hb_parptr(1)), rc, hb_parc(3),
                                           static_cast<HPDF_Encoder>(hb_parptr(4))));
#else
  hb_retptr(nullptr);
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateUnderlineAnnot (HPDF_Page    page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
 */
HB_FUNC(HPDF_PAGE_CREATEUNDERLINEANNOT)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Rect rc;

  rc.left = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  rc.top = static_cast<HPDF_REAL>(hb_parvnd(2, 2));
  rc.right = static_cast<HPDF_REAL>(hb_parvnd(2, 3));
  rc.bottom = static_cast<HPDF_REAL>(hb_parvnd(2, 4));

  hb_retptr(HPDF_Page_CreateUnderlineAnnot(static_cast<HPDF_Page>(hb_parptr(1)), rc, hb_parc(3),
                                           static_cast<HPDF_Encoder>(hb_parptr(4))));
#else
  hb_retptr(nullptr);
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateSquigglyAnnot  (HPDF_Page    page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
 */
HB_FUNC(HPDF_PAGE_CREATESQUIGGLYANNOT)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Rect rc;

  rc.left = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  rc.top = static_cast<HPDF_REAL>(hb_parvnd(2, 2));
  rc.right = static_cast<HPDF_REAL>(hb_parvnd(2, 3));
  rc.bottom = static_cast<HPDF_REAL>(hb_parvnd(2, 4));

  hb_retptr(HPDF_Page_CreateSquigglyAnnot(static_cast<HPDF_Page>(hb_parptr(1)), rc, hb_parc(3),
                                          static_cast<HPDF_Encoder>(hb_parptr(4))));
#else
  hb_retptr(nullptr);
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateStrikeOutAnnot  (HPDF_Page   page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
 */
HB_FUNC(HPDF_PAGE_CREATESTRIKEOUTANNOT)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Rect rc;

  rc.left = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  rc.top = static_cast<HPDF_REAL>(hb_parvnd(2, 2));
  rc.right = static_cast<HPDF_REAL>(hb_parvnd(2, 3));
  rc.bottom = static_cast<HPDF_REAL>(hb_parvnd(2, 4));

  hb_retptr(HPDF_Page_CreateStrikeOutAnnot(static_cast<HPDF_Page>(hb_parptr(1)), rc, hb_parc(3),
                                           static_cast<HPDF_Encoder>(hb_parptr(4))));
#else
  hb_retptr(nullptr);
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreatePopupAnnot  ( HPDF_Page    page,
                        HPDF_Rect          rect,
                        HPDF_Annotation      parent);
 */
HB_FUNC(HPDF_PAGE_CREATEPOPUPANNOT)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Rect rc;

  rc.left = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  rc.top = static_cast<HPDF_REAL>(hb_parvnd(2, 2));
  rc.right = static_cast<HPDF_REAL>(hb_parvnd(2, 3));
  rc.bottom = static_cast<HPDF_REAL>(hb_parvnd(2, 4));

  hb_retptr(
      HPDF_Page_CreatePopupAnnot(static_cast<HPDF_Page>(hb_parptr(1)), rc, static_cast<HPDF_Annotation>(hb_parptr(3))));
#else
  hb_retptr(nullptr);
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateStampAnnot  (   HPDF_Page           page,
                        HPDF_Rect           rect,
                        HPDF_StampAnnotName name,
                        const char*         text,
                        HPDF_Encoder      encoder);
 */
HB_FUNC(HPDF_PAGE_CREATESTAMPANNOT)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Rect rc;

  rc.left = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  rc.top = static_cast<HPDF_REAL>(hb_parvnd(2, 2));
  rc.right = static_cast<HPDF_REAL>(hb_parvnd(2, 3));
  rc.bottom = static_cast<HPDF_REAL>(hb_parvnd(2, 4));

  hb_retptr(HPDF_Page_CreateStampAnnot(static_cast<HPDF_Page>(hb_parptr(1)), rc,
                                       static_cast<HPDF_StampAnnotName>(hb_parni(3)), hb_parc(4),
                                       static_cast<HPDF_Encoder>(hb_parptr(5))));
#else
  hb_retptr(nullptr);
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateSquareAnnot (HPDF_Page          page,
                      HPDF_Rect          rect,
                      const char         *text,
                      HPDF_Encoder       encoder);
 */
HB_FUNC(HPDF_PAGE_CREATESQUAREANNOT)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Rect rc;

  rc.left = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  rc.top = static_cast<HPDF_REAL>(hb_parvnd(2, 2));
  rc.right = static_cast<HPDF_REAL>(hb_parvnd(2, 3));
  rc.bottom = static_cast<HPDF_REAL>(hb_parvnd(2, 4));

  hb_retptr(HPDF_Page_CreateSquareAnnot(static_cast<HPDF_Page>(hb_parptr(1)), rc, hb_parc(3),
                                        static_cast<HPDF_Encoder>(hb_parptr(4))));
#else
  hb_retptr(nullptr);
#endif
}

/* HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateCircleAnnot (HPDF_Page          page,
                      HPDF_Rect          rect,
                      const char         *text,
                      HPDF_Encoder       encoder);
 */
HB_FUNC(HPDF_PAGE_CREATECIRCLEANNOT)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Rect rc;

  rc.left = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  rc.top = static_cast<HPDF_REAL>(hb_parvnd(2, 2));
  rc.right = static_cast<HPDF_REAL>(hb_parvnd(2, 3));
  rc.bottom = static_cast<HPDF_REAL>(hb_parvnd(2, 4));

  hb_retptr(HPDF_Page_CreateCircleAnnot(static_cast<HPDF_Page>(hb_parptr(1)), rc, hb_parc(3),
                                        static_cast<HPDF_Encoder>(hb_parptr(4))));
#else
  hb_retptr(nullptr);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annot_SetRGBColor (HPDF_Annotation annot, HPDF_RGBColor color);
 */
HB_FUNC(HPDF_ANNOT_SETRGBCOLOR)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_RGBColor rgb;

  rgb.r = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  rgb.g = static_cast<HPDF_REAL>(hb_parvnd(2, 2));
  rgb.b = static_cast<HPDF_REAL>(hb_parvnd(2, 3));

  hb_retnl(static_cast<long>(HPDF_Annot_SetRGBColor(static_cast<HPDF_Annotation>(hb_parptr(1)), rgb)));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annot_SetCMYKColor (HPDF_Annotation annot, HPDF_CMYKColor color);
 */
HB_FUNC(HPDF_ANNOT_SETCMYKCOLOR)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_CMYKColor cmyk;

  cmyk.c = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  cmyk.m = static_cast<HPDF_REAL>(hb_parvnd(2, 2));
  cmyk.y = static_cast<HPDF_REAL>(hb_parvnd(2, 3));
  cmyk.k = static_cast<HPDF_REAL>(hb_parvnd(2, 4));

  hb_retnl(static_cast<long>(HPDF_Annot_SetCMYKColor(static_cast<HPDF_Annotation>(hb_parptr(1)), cmyk)));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annot_SetGrayColor (HPDF_Annotation annot, HPDF_REAL color);
 */
HB_FUNC(HPDF_ANNOT_SETGRAYCOLOR)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(
      HPDF_Annot_SetGrayColor(static_cast<HPDF_Annotation>(hb_parptr(1)), static_cast<HPDF_REAL>(hb_parnd(2)))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annot_SetNoColor (HPDF_Annotation annot);
 */
HB_FUNC(HPDF_ANNOT_SETNOCOLOR)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(HPDF_Annot_SetNoColor(static_cast<HPDF_Annotation>(hb_parptr(1)))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetTitle (HPDF_Annotation annot, const char* name);
 */
HB_FUNC(HPDF_MARKUPANNOT_SETTITLE)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(HPDF_MarkupAnnot_SetTitle(static_cast<HPDF_Annotation>(hb_parptr(1)), hb_parc(2))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetSubject (HPDF_Annotation annot, const char* name);
 */
HB_FUNC(HPDF_MARKUPANNOT_SETSUBJECT)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(HPDF_MarkupAnnot_SetSubject(static_cast<HPDF_Annotation>(hb_parptr(1)), hb_parc(2))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetCreationDate (HPDF_Annotation annot, HPDF_Date value);
 */
HB_FUNC(HPDF_MARKUPANNOT_SETCREATIONDATE)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Date date{};

  date.year = hb_parvni(2, 1);
  date.month = hb_parvni(2, 2);
  date.day = hb_parvni(2, 3);
  date.hour = hb_parvni(2, 4);
  date.minutes = hb_parvni(2, 5);
  date.seconds = hb_parvni(2, 6);
  date.ind = ' ';

  hb_retnl(static_cast<long>(HPDF_MarkupAnnot_SetCreationDate(static_cast<HPDF_Annotation>(hb_parptr(1)), date)));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetTransparency (HPDF_Annotation annot, HPDF_REAL value);
 */
HB_FUNC(HPDF_MARKUPANNOT_SETTRANSPARENCY)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(HPDF_MarkupAnnot_SetTransparency(static_cast<HPDF_Annotation>(hb_parptr(1)),
                                                              static_cast<HPDF_REAL>(hb_parnd(2)))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetIntent (HPDF_Annotation  annot, HPDF_AnnotIntent  intent);
 */
HB_FUNC(HPDF_MARKUPANNOT_SETINTENT)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(HPDF_MarkupAnnot_SetIntent(static_cast<HPDF_Annotation>(hb_parptr(1)),
                                                        static_cast<HPDF_AnnotIntent>(hb_parni(2)))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetPopup (HPDF_Annotation annot, HPDF_Annotation popup);
 */
HB_FUNC(HPDF_MARKUPANNOT_SETPOPUP)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(HPDF_MarkupAnnot_SetPopup(static_cast<HPDF_Annotation>(hb_parptr(1)),
                                                       static_cast<HPDF_Annotation>(hb_parptr(2)))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetRectDiff (HPDF_Annotation  annot, HPDF_Rect  rect);
 */
HB_FUNC(HPDF_MARKUPANNOT_SETRECTDIFF)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Rect rc;

  rc.left = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  rc.top = static_cast<HPDF_REAL>(hb_parvnd(2, 2));
  rc.right = static_cast<HPDF_REAL>(hb_parvnd(2, 3));
  rc.bottom = static_cast<HPDF_REAL>(hb_parvnd(2, 4));

  hb_retnl(static_cast<long>(HPDF_MarkupAnnot_SetRectDiff(static_cast<HPDF_Annotation>(hb_parptr(1)), rc)));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetCloudEffect (HPDF_Annotation  annot, HPDF_INT cloudIntensity);
 */
HB_FUNC(HPDF_MARKUPANNOT_SETCLOUDEFFECT)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(
      HPDF_MarkupAnnot_SetCloudEffect(static_cast<HPDF_Annotation>(hb_parptr(1)), static_cast<HPDF_INT>(hb_parni(2)))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetInteriorRGBColor (HPDF_Annotation  annot, HPDF_RGBColor color);
 */
HB_FUNC(HPDF_MARKUPANNOT_SETINTERIORRGBCOLOR)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_RGBColor rgb;

  rgb.r = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  rgb.g = static_cast<HPDF_REAL>(hb_parvnd(2, 2));
  rgb.b = static_cast<HPDF_REAL>(hb_parvnd(2, 3));

  hb_retnl(static_cast<long>(HPDF_MarkupAnnot_SetInteriorRGBColor(static_cast<HPDF_Annotation>(hb_parptr(1)), rgb)));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetInteriorCMYKColor (HPDF_Annotation  annot, HPDF_CMYKColor color);
 */
HB_FUNC(HPDF_MARKUPANNOT_SETINTERIORCMYKCOLOR)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_CMYKColor cmyk;

  cmyk.c = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  cmyk.m = static_cast<HPDF_REAL>(hb_parvnd(2, 2));
  cmyk.y = static_cast<HPDF_REAL>(hb_parvnd(2, 3));
  cmyk.k = static_cast<HPDF_REAL>(hb_parvnd(2, 4));

  hb_retnl(static_cast<long>(HPDF_MarkupAnnot_SetInteriorCMYKColor(static_cast<HPDF_Annotation>(hb_parptr(1)), cmyk)));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetInteriorGrayColor (HPDF_Annotation  annot, HPDF_REAL color);
 */
HB_FUNC(HPDF_MARKUPANNOT_SETINTERIORGRAYCOLOR)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(HPDF_MarkupAnnot_SetInteriorGrayColor(static_cast<HPDF_Annotation>(hb_parptr(1)),
                                                                   static_cast<HPDF_REAL>(hb_parnd(2)))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetInteriorTransparent (HPDF_Annotation  annot);
 */
HB_FUNC(HPDF_MARKUPANNOT_SETINTERIORTRANSPARENT)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(HPDF_MarkupAnnot_SetInteriorTransparent(static_cast<HPDF_Annotation>(hb_parptr(1)))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_TextMarkupAnnot_SetQuadPoints ( HPDF_Annotation annot, HPDF_Point lb, HPDF_Point rb, HPDF_Point rt, HPDF_Point
   lt);
 */
HB_FUNC(HPDF_TEXTMARKUPANNOT_SETQUADPOINTS)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Point p1;
  HPDF_Point p2;
  HPDF_Point p3;
  HPDF_Point p4;

  p1.x = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  p1.y = static_cast<HPDF_REAL>(hb_parvnd(2, 2));

  p2.x = static_cast<HPDF_REAL>(hb_parvnd(3, 1));
  p2.y = static_cast<HPDF_REAL>(hb_parvnd(3, 2));

  p3.x = static_cast<HPDF_REAL>(hb_parvnd(4, 1));
  p3.y = static_cast<HPDF_REAL>(hb_parvnd(4, 2));

  p4.x = static_cast<HPDF_REAL>(hb_parvnd(5, 1));
  p4.y = static_cast<HPDF_REAL>(hb_parvnd(5, 2));

  hb_retnl(static_cast<long>(
      HPDF_TextMarkupAnnot_SetQuadPoints(static_cast<HPDF_Annotation>(hb_parptr(1)), p1, p2, p3, p4)));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annot_Set3DView  ( HPDF_MMgr mmgr,
                   HPDF_Annotation   annot,
                   HPDF_Annotation   annot3d,
                   HPDF_Dict         view);
 */
HB_FUNC(HPDF_ANNOT_SET3DVIEW)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(
      HPDF_Annot_Set3DView(static_cast<HPDF_MMgr>(hb_parptr(1)), static_cast<HPDF_Annotation>(hb_parptr(2)),
                           static_cast<HPDF_Annotation>(hb_parptr(3)), static_cast<HPDF_Dict>(hb_parptr(4)))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_PopupAnnot_SetOpened  (HPDF_Annotation   annot,
                            HPDF_BOOL         opened);
 */
HB_FUNC(HPDF_POPUPANNOT_SETOPENED)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(
      HPDF_PopupAnnot_SetOpened(static_cast<HPDF_Annotation>(hb_parptr(1)), static_cast<HPDF_BOOL>(hb_parl(2)))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_FreeTextAnnot_SetLineEndingStyle (HPDF_Annotation annot, HPDF_LineAnnotEndingStyle startStyle,
   HPDF_LineAnnotEndingStyle endStyle);
 */
HB_FUNC(HPDF_FREETEXTANNOT_SETLINEENDINGSTYLE)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(HPDF_FreeTextAnnot_SetLineEndingStyle(
      static_cast<HPDF_Annotation>(hb_parptr(1)), static_cast<HPDF_LineAnnotEndingStyle>(hb_parni(2)),
      static_cast<HPDF_LineAnnotEndingStyle>(hb_parni(3)))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_FreeTextAnnot_Set3PointCalloutLine (HPDF_Annotation annot, HPDF_Point startPoint, HPDF_Point kneePoint,
   HPDF_Point endPoint);
 */
HB_FUNC(HPDF_FREETEXTANNOT_SET3POINTCALLOUTLINE)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Point p1;
  HPDF_Point p2;
  HPDF_Point p3;

  p1.x = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  p1.y = static_cast<HPDF_REAL>(hb_parvnd(2, 2));

  p2.x = static_cast<HPDF_REAL>(hb_parvnd(3, 1));
  p2.y = static_cast<HPDF_REAL>(hb_parvnd(3, 2));

  p3.x = static_cast<HPDF_REAL>(hb_parvnd(4, 1));
  p3.y = static_cast<HPDF_REAL>(hb_parvnd(4, 2));

  hb_retnl(static_cast<long>(
      HPDF_FreeTextAnnot_Set3PointCalloutLine(static_cast<HPDF_Annotation>(hb_parptr(1)), p1, p2, p3)));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_FreeTextAnnot_Set2PointCalloutLine (HPDF_Annotation annot, HPDF_Point startPoint, HPDF_Point endPoint);
 */
HB_FUNC(HPDF_FREETEXTANNOT_SET2POINTCALLOUTLINE)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Point p1;
  HPDF_Point p2;

  p1.x = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  p1.y = static_cast<HPDF_REAL>(hb_parvnd(2, 2));

  p2.x = static_cast<HPDF_REAL>(hb_parvnd(3, 1));
  p2.y = static_cast<HPDF_REAL>(hb_parvnd(3, 2));

  hb_retnl(
      static_cast<long>(HPDF_FreeTextAnnot_Set2PointCalloutLine(static_cast<HPDF_Annotation>(hb_parptr(1)), p1, p2)));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_FreeTextAnnot_SetDefaultStyle (HPDF_Annotation  annot, const char* style);
 */
HB_FUNC(HPDF_FREETEXTANNOT_SETDEFAULTSTYLE)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(
      static_cast<long>(HPDF_FreeTextAnnot_SetDefaultStyle(static_cast<HPDF_Annotation>(hb_parptr(1)), hb_parc(2))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_LineAnnot_SetPosition (HPDF_Annotation annot,
                     HPDF_Point startPoint, HPDF_LineAnnotEndingStyle startStyle,
                     HPDF_Point endPoint, HPDF_LineAnnotEndingStyle endStyle);
 */
HB_FUNC(HPDF_LINEANNOT_SETPOSITION)
{
#if HB_HPDF_VERS(2, 2, 0)
  HPDF_Point p1;
  HPDF_Point p2;

  p1.x = static_cast<HPDF_REAL>(hb_parvnd(2, 1));
  p1.y = static_cast<HPDF_REAL>(hb_parvnd(2, 2));

  p2.x = static_cast<HPDF_REAL>(hb_parvnd(4, 1));
  p2.y = static_cast<HPDF_REAL>(hb_parvnd(4, 2));

  hb_retnl(static_cast<long>(HPDF_LineAnnot_SetPosition(static_cast<HPDF_Annotation>(hb_parptr(1)), p1,
                                                        static_cast<HPDF_LineAnnotEndingStyle>(hb_parni(3)), p2,
                                                        static_cast<HPDF_LineAnnotEndingStyle>(hb_parni(5)))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_LineAnnot_SetLeader (HPDF_Annotation annot, HPDF_INT leaderLen, HPDF_INT leaderExtLen, HPDF_INT
   leaderOffsetLen);
 */
HB_FUNC(HPDF_LINEANNOT_SETLEADER)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(
      HPDF_LineAnnot_SetLeader(static_cast<HPDF_Annotation>(hb_parptr(1)), hb_parni(2), hb_parni(3), hb_parni(4))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_LineAnnot_SetCaption (HPDF_Annotation annot, HPDF_BOOL showCaption, HPDF_LineAnnotCapPosition position, HPDF_INT
   horzOffset, HPDF_INT vertOffset);
 */
HB_FUNC(HPDF_LINEANNOT_SETCAPTION)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(HPDF_LineAnnot_SetCaption(static_cast<HPDF_Annotation>(hb_parptr(1)), hb_parl(2),
                                                       static_cast<HPDF_LineAnnotCapPosition>(hb_parni(3)), hb_parni(4),
                                                       hb_parni(5))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}

/* HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annotation_SetBorderStyle  (HPDF_Annotation  annot,
                                 HPDF_BSSubtype   subtype,
                                 HPDF_REAL        width,
                                 HPDF_UINT16      dash_on,
                                 HPDF_UINT16      dash_off,
                                 HPDF_UINT16      dash_phase);
 */
HB_FUNC(HPDF_ANNOTATION_SETBORDERSTYLE)
{
#if HB_HPDF_VERS(2, 2, 0)
  hb_retnl(static_cast<long>(HPDF_Annotation_SetBorderStyle(
      static_cast<HPDF_Annotation>(hb_parptr(1)), static_cast<HPDF_BSSubtype>(hb_parni(2)),
      static_cast<HPDF_REAL>(hb_parnd(3)), static_cast<HPDF_UINT16>(hb_parni(4)), static_cast<HPDF_UINT16>(hb_parni(5)),
      static_cast<HPDF_UINT16>(hb_parni(6)))));
#else
  hb_retnl(HB_HPDF_NOTSUPPORTED);
#endif
}
