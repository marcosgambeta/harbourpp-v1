//
// Xbase++ xbpHtmlViewer compatible Class
//
// Copyright 2008 Andy Wos
// Copyright 2008-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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

//                               EkOnkar
//                         ( The LORD is ONE )

#include <hbclass.ch>
#include <inkey.ch>
#include <hbgtinfo.ch>

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

#define evBeforeNavigate                          100
#define evNavigateComplete                        101
#define evStatusTextChange                        102
#define evDownloadComplete                        104
#define evCommandStateChange                      105
#define evDownloadBegin                           106
#define evProgressChange                          108
#define evTitleChange                             113

#define evPropertyChange                          112
#define evBeforeNavigate2                         250
#define evNavigateComplete2                       252
#define evDocumentComplete                        259
#define evNavigateError                           271

CREATE CLASS WvgHTMLViewer INHERIT WvgActiveXControl

   METHOD new(oParent, oOwner, aPos, aSize, aPresParams, lVisible)
   METHOD create(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   METHOD xBeforeNavigate(cURL, Flags, TFName, PData, Headers)
   METHOD xStatusTextChange(cText)
   METHOD xNavigateComplete(cURL)
   METHOD xProgressChange(nProgress, nProgMax)
   METHOD xTitleChange(cTitle)
   METHOD xDocumentComplete(cURI)
   METHOD setHTML(cHTML)

   METHOD back() INLINE ::goBack()
   METHOD forward() INLINE ::goForward()
   METHOD home() INLINE ::goHome()
   METHOD search() INLINE ::goSearch()
   METHOD isBusy() INLINE ::busy()

   VAR sl_beforeNavigate PROTECTED
   ACCESS beforeNavigate INLINE ::sl_beforeNavigate
   ASSIGN beforeNavigate(bBlock) INLINE ::sl_beforeNavigate := bBlock

   VAR sl_navigateComplete PROTECTED
   ACCESS navigateComplete INLINE ::sl_navigateComplete
   ASSIGN navigateComplete(bBlock) INLINE ::sl_navigateComplete := bBlock

   VAR sl_statusTextChange PROTECTED
   ACCESS statusTextChange INLINE ::sl_statusTextChange
   ASSIGN statusTextChange(bBlock) INLINE ::sl_statusTextChange := bBlock

   VAR sl_progressChange PROTECTED
   ACCESS progressChange INLINE ::sl_progressChange
   ASSIGN progressChange(bBlock) INLINE ::sl_progressChange := bBlock

   VAR sl_titleChange PROTECTED
   ACCESS titleChange INLINE ::sl_titleChange
   ASSIGN titleChange(bBlock) INLINE ::sl_titleChange := bBlock

   VAR sl_documentComplete PROTECTED
   ACCESS documentComplete INLINE ::sl_documentComplete
   ASSIGN documentComplete(bBlock) INLINE ::sl_documentComplete := bBlock

   VAR CLSID PROTECTED INIT "Shell.Explorer"

ENDCLASS

METHOD WvgHTMLViewer:new(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   ::wvgWindow:new(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   ::WvgActiveXControl:new(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   RETURN Self

METHOD WvgHTMLViewer:create(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   ::wvgWindow:create(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   ::mapEvent(evBeforeNavigate,   {|cURL, Flags, TFName, PData, Headers|::xBeforeNavigate(cURL, Flags, TFName, PData, Headers)})

   ::mapEvent(evStatusTextChange, {|cText|::xStatusTextChange(cText)})

   ::mapEvent(evNavigateComplete, {|cURL|::xNavigateComplete(cURL)})

   ::mapEvent(evProgressChange, {|nProgress, nProgMax|::xProgressChange(nProgress, nProgMax)})

   ::mapEvent(evTitleChange, {|cTitle|::xTitleChange(cTitle)})

   ::mapEvent(evDocumentComplete, {|cURI|::xDocumentComplete(cURI)})

   ::WvgActiveXControl:create(oParent, oOwner, aPos, aSize, aPresParams, lVisible)

   RETURN Self

METHOD WvgHTMLViewer:xBeforeNavigate(cURL, Flags, TFName, PData, Headers)

   HB_SYMBOL_UNUSED(Flags)
   HB_SYMBOL_UNUSED(TFName)
   HB_SYMBOL_UNUSED(PData)
   HB_SYMBOL_UNUSED(Headers)

   IF PCount() >= 1 .AND. hb_IsBlock(::sl_beforeNavigate)
      Eval(::sl_beforeNavigate, cURL, , Self)
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgHTMLViewer:xStatusTextChange(cText)

   IF hb_IsBlock(::sl_statusTextChange)
      Eval(::sl_statusTextChange, cText, , Self)
   ENDIF

   RETURN Self

METHOD WvgHTMLViewer:xNavigateComplete(cURL)

   IF hb_IsBlock(::sl_navigateComplete)
      Eval(::sl_navigateComplete, cURL, , Self)
   ENDIF

   RETURN Self

METHOD WvgHTMLViewer:xProgressChange(nProgress, nProgMax)

   IF hb_IsBlock(::sl_progressChange)
      Eval(::sl_progressChange, nProgress, nProgMax, Self)
   ENDIF

   RETURN Self

METHOD WvgHTMLViewer:xTitleChange(cTitle)

   IF hb_IsBlock(::sl_titleChange)
      Eval(::sl_titleChange, cTitle, , Self)
   ENDIF

   RETURN Self

METHOD WvgHTMLViewer:xDocumentComplete(cURI)

   IF hb_IsBlock(::sl_documentComplete)
      Eval(::sl_documentComplete, cURI, , Self)
   ENDIF

   RETURN Self

METHOD WvgHTMLViewer:setHTML(cHTML)

   ::document:innerHTML := cHTML
   ::refresh()

   RETURN Self
