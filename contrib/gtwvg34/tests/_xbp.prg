// Pritpal Bedi <bedipritpal@hotmail.com>

#include <inkey.ch>
#include <hbgtinfo.ch>
#include <hbver.ch>

// A pure Xbase++ implementation

PROCEDURE demoxbp()

   LOCAL oCrt, oTBar, oSBar, oStatic, oCom, oXbp, oTree, oItem1, oItem2
   LOCAL oListBox, oCheck, oRadio, oStatic2, oMLE, oAddr
   LOCAL oPanel, oPanel1, oPanel2, cText, cNavigate, oDA
   LOCAL cVarA  := "Test A", cVarB := "Test B"
   LOCAL aState := { "not selected", "selected", "undefined" }
   LOCAL aParts := {}

   // --- Dialog ---
   oCrt := WvgDialog():new( , , { 30, 30 }, { 900, 600 }, , .T. )
   oCrt:closable := .T.
   oCrt:icon := GetResource( "vr_1.ico" )
   oCrt:create()

   oCrt:setFontCompoundName( "12.Courier italic" )

   oDA := oCrt:drawingArea

   // --- Menu ---
   BuildMenuXbp( oCrt, @oStatic, @oStatic2 )

   // --- ToolBar ---
   oTBar := BuildToolBarXbp( oDA )

   // --- StatusBar ---
   oSBar   := WvgStatusBar():new( oCrt ):create( , , , , , .T. )
   oSBar:panelClick := {| oPanel | wapi_MessageBox( , oPanel:caption ) }
   oPanel  := oSBar:getItem(1)
   oPanel:caption := "My Root Panel"
   oPanel1 := oSBar:addItem()
   oPanel1:caption := "Ready"
   oPanel2 := oSBar:addItem()
   oPanel2:caption := "Click on any part!"

   // --- Static ---
   oStatic := WvgStatic():new( oDA )
   oStatic:type    := WVGSTATIC_TYPE_TEXT
   oStatic:options := WVGSTATIC_TEXT_CENTER
   oStatic:caption := Chr(13) + "Implemented   Xbase++ Parts"

   oStatic:create( , , { 0, oTBar:currentSize()[2] + 3 }, { 120, oCrt:currentSize()[2] - ;
      oTBar:currentSize()[2] - oSBar:currentSize()[2] - 4 }, , .T. )
   oStatic:setColorBG( WIN_RGB(200, 200, 200) )

   // --- ListBox ---
   oListBox := WvgListBox():new()
   oListBox:create( oStatic, , { 5, 55 }, { 107, 380 } )

   oListBox:setColorFG( WIN_RGB(218, 61, 34) )
#if 0
   oListBox:setColorBG( WIN_RGB(250, 244, 182) )
#endif

   AAdd(aParts, "XbpDialog")
   AAdd(aParts, "XbpMenuBar")
   AAdd(aParts, "XbpToolBar")
   AAdd(aParts, "XbpToolBarButton")
   AAdd(aParts, "XbpStatusBar")
   AAdd(aParts, "XbpStatic")
   AAdd(aParts, "XbpTreeView")
   AAdd(aParts, "XbpTreeViewItem")
   AAdd(aParts, "XbpActiveXControl")
   AAdd(aParts, "XbpListBox")
   AAdd(aParts, "XbpPushButton")
   AAdd(aParts, "XbpCheckBox")
   AAdd(aParts, "XbpRadioButton")
   AAdd(aParts, "Xbp3State")
   AAdd(aParts, "XbpSLE")
   AAdd(aParts, "XbpMLE")
   AAdd(aParts, "XbpHTMLViewer")
   AAdd(aParts, "XbpSysWindow")
   AAdd(aParts, "XbpFontDialog")
   AAdd(aParts, "XbpFont")
   AAdd(aParts, "-------------")
   AAdd(aParts, "DataRef")

   AEval(aParts, {| e | oListBox:addItem( e ) })
   oListBox:itemSelected := {|| wapi_MessageBox( , oListBox:getCurItem() ) }
   oListBox:setData(3)    // show selected "XbpToolBar"

   // --- PushButton ---
   oXbp := WvgPushButton():new( oStatic )
   oXbp:caption := "Hide"
   oXbp:create( , , { 20, 440 }, { 80, 30 } )
   oXbp:activate := {|| oStatic:hide(), oCrt:sendMessage( WIN_WM_SIZE, 0, 0 ) }

   // --- TreeView ---

   oTree := WvgTreeView():new( oDA, , { oCrt:currentSize()[1] - 160, oTBar:currentSize()[2] + 3 }, ;
      { 160, oCrt:currentSize()[2] - ;
      oTBar:currentSize()[2] - oSBar:currentSize()[2] - 4 }, , .T. )
   oTree:hasLines   := .T.
   oTree:hasButtons := .T.
   oTree:alwaysShowSelection := .T.
   oTree:create()
   oTree:setColorBG( WIN_RGB(120, 15, 240) )
   oTree:setColorFG( WIN_RGB(15, 240, 120) )
   oTree:itemSelected := {| oItem | IIf(oItem != NIL, wapi_MessageBox( , oItem:caption ), NIL) }

   oItem1 := oTree:rootItem:addItem( "First level A" )

   oTree:rootItem:addItem( "First level B" )

   oItem2 := oItem1:addItem( "Second level A" )
   oItem1:addItem( "Second level B" )

   oItem2:addItem( "Third level A" )
   oItem2:addItem( "Third level B" )
   oItem2:addItem( "Third level C" )

#if 0
   oItem1:expand(.T.)
#else
   oTree:showExpanded(.T., 2)
#endif

   oTree:setData(oItem2)

   hb_gtInfo( HB_GTI_WINTITLE, hb_Version( HB_VERSION_URL_BASE ) )
   // --- ActiveX ---
#if 0
   oCom := WvgActiveXControl():New( oDA, , { 0, 0 }, { 100, 100 }, , .T. )
   oCom:CLSID := "Shell.Explorer.2"
   oCom:mapEvent( 269, {|| QOut( "EXPLORER-269" ) } )
#else
   oCom := WvgHtmlViewer():New( oDA, , { 0, 0 }, { 100, 100 }, , .T. )
   oCom:beforeNavigate := {| cURL, x, oHTML | x := x, oHTML := oHTML, oPanel:caption := cURL }
   oCom:statusTextChange := {| cText | oPanel:caption := cText }
   oCom:mapEvent( 112, {|| oPanel:caption := "EXPLORER-269" } )
#endif
   oCom:create()
   oCom:Navigate( hb_Version( HB_VERSION_URL_BASE ) )

   oAddr := WvgSLE():new()
   oAddr:bufferLength := 500
   oAddr:border       := .T.
   cNavigate          := hb_Version( HB_VERSION_URL_BASE )
   oAddr:dataLink     := {| x | IIf(x == NIL, cNavigate, cNavigate := x) }
   oAddr:setColorFG( WIN_RGB(0, 0, 255) )
   oAddr:setColorBG( WIN_RGB(0, 255, 255) )
   oAddr:create( oDA, , { 120, oTBar:currentSize()[2] }, { 500, 20 }, , .T. )
   oAddr:setData()
   oAddr:killInputFocus := {| m1, m2, oS | m1 := m1, m2 := m2, oS:getData(), oCom:navigate( cNavigate ) }

   // --- Panel : Static + Radio + Checkbox ---
   oStatic2 := WvgStatic():New( oDA, , { 150, 150 }, { 500, 310 }, , .F. )
#if 0
   oStatic2:type    := WVGSTATIC_TYPE_RAISEDBOX //BGNDFRAME
#endif
   oStatic2:exStyle += WIN_WS_EX_WINDOWEDGE
#if 0
   oStatic2:options := WVGSTATIC_FRAMETHICK
#endif
   oStatic2:create()
   oStatic2:setColorBG( WIN_RGB(175, 175, 175) )

   oXbp := WvgPushButton():new( oStatic2 )
   oXbp:caption     := "Hide"
   oXbp:caption     := "Hide"
   oXbp:create( , , { 430, 275 }, { 60, 25 } )
   oXbp:activate    := {|| oStatic2:hide(), oCrt:sendMessage( WIN_WM_SIZE, 0, 0 ) }

   oRadio := WvgRadioButton():new( oStatic2, , { 10, 10 }, { 100, 15 } )
   oRadio:caption   := "Com 1"
   oRadio:selection := .T.
   oRadio:selected  := {| m1, m2, obj | m1 := m1, m2 := m2, wapi_MessageBox( , obj:caption + IIf(obj:selection, "< S >", "< N >") ) }
   oRadio:create()

   oRadio := WvgRadioButton():new( oStatic2, , { 10, 35 }, { 100, 15 } )
   oRadio:caption   := "Com 2"
   oRadio:create()

   oCheck := WvgCheckBox():New( oStatic2, , { 10, 70 }, { 100, 15 }, , .T. )
   oCheck:caption   := "Checkbox A"
   oCheck:create()
   oCheck:selected  := {| m1, m2, o | m1 := m1, m2 := m2, wapi_MessageBox( , IIf(o:getData(), "I am selected", "I am not selected") ) }

   // Create first 3State button, passing the position to :create()
   oXbp  := Wvg3State():new()
   oXbp:caption := "3 State A"
   oXbp:create( oStatic2, , { 10, 100 }, { 100, 15 } )
   // Determine current state using mp1
   oXbp:selected := {| m1, m2, oBtn | m2 := m2, oBtn := oBtn, oPanel1:caption := "3State A [" + aState[m1 + 1] + "]" }

   // Create second 3State Button, passing the position to :new()
   oXbp  := Wvg3State():new( oStatic2, , { 10, 125 }, { 100, 15 } )
   oXbp:caption := "3 State B"
   oXbp:create( oStatic2 )
   // Determine current state using :getData()
   oXbp:selected := {| m1, m2, oBtn | m1 := m1, m2 := m2, wapi_MessageBox( , "3State B", aState[oBtn:getData() + 1] ) }

   // Create first SLE, specify position using :create()
   // On :typeOut set the focus to the second SLE
   oXbp                := WvgSLE():new()
   oXbp:autoTab        := .T.
   oXbp:bufferLength   := 20
   // Data code block containing assignment to LOCAL variable
   oXbp:dataLink       := {| x | IIf(x == NIL, cVarA, cVarA := x) }
   oXbp:create( oStatic2, , { 10, 170 }, { 150, 20 } )
   oXbp:setData()
   // Assign the value of the edit buffer to a LOCAL variable when the input focus is lost
   oXbp:killInputFocus := {| x, y, oSLE | x := x, y := y, oSLE:getData(), oPanel:caption := "cVarA =" + cVarA }

   // Create second SLE, specify position using :new()
   oXbp                := WvgSLE():new( , , { 10, 200 }, { 150, 20 } )
   oXbp:tabStop        := .T.
   oXbp:bufferLength   := 15
   oXbp:dataLink       := {| x | IIf(x == NIL, cVarB, cVarB := x) }
   oXbp:create( oStatic2 )
   oXbp:setData()
   oXbp:killInputFocus := {| x, y, oSLE | x := x, y := y, oSLE:getData(), oPanel:caption := "cVarB =" + cVarB }

   // Read file into LOCAL variable
   cText   := hb_MemoRead(GetResource(__FILE__))
   // Create MLE, specify position using :create() and
   // assign data code block accessing LOCAL variable
   oMLE    := WvgMLE():new()
   oMLE:wordWrap := .F.
   oMLE:border   := .T.
   oMLE:dataLink := {| x | IIf(x == NIL, cText, cText := x) }
   oMLE:create( oStatic2, , { 180, 10 }, { 310, 250 } )
   // Copy text from LOCAL variable into edit buffer via :dataLink
   oMLE:setData()

   // --- Misc Config ---
   oTBar:buttonClick := {| oBtn | ;
      IIf(oBtn:caption == "Hide", oStatic:hide(), NIL), ;
      IIf(oBtn:caption == "Show", oStatic:show(), NIL), ;
      IIf(oBtn:caption == "Tools", oStatic2:show():toFront(), NIL), ;
      IIf(oBtn:caption == "FontDlg", ExeFontDialog( oCrt ), NIL), ;
      IIf(oBtn:caption $ "Hide,Show", oCrt:sendMessage( WIN_WM_SIZE, 0, 0 ), NIL), ;
      oPanel2:caption := "Button [ " + oBtn:caption + " ] clicked!" }

   oCrt:resize := {|| ResizeDialogXbp( oCrt, oTBar, oSBar, oStatic, oCom, oTree, oAddr ) }

   oCrt:sendMessage( WIN_WM_SIZE, 0, 0 )
   oCrt:show()

   DO WHILE hb_keyStd(Inkey(0)) != K_ESC
   ENDDO

   oCrt:Destroy()

   RETURN

STATIC FUNCTION ResizeDialogXbp( oCrt, oTBar, oSBar, oStatic, oCom, oTree, oAddr )

   LOCAL aCrt  := oCrt:currentSize()
   LOCAL aTBar := oTBar:currentSize()
   LOCAL aSBar := oSBar:currentSize()

   LOCAL nT := aTBar[2]
   LOCAL nH := aCrt[2] - aTBar[2] - aSBar[2]

   IF oStatic:isVisible
      oStatic:setPosAndSize( { 0, nT + 3 }, { 120, nH - 4 }, .T. )
      oAddr:setPosAndSize( { 120, nT + 2 }, { aCrt[1] - 120 - 150, 20 }, .T. )
      oCom:setPosAndSize( { 120, nT + 2 + 20 }, { aCrt[1] - 120 - 150, nH - 20 }, .T. )
      oTree:setPosAndSize( { aCrt[1] - 150, nT }, { 150, nH }, .T. )

   ELSE
      oAddr:setPosAndSize( { 0, nT + 2 }, { aCrt[1] - 150, 20 }, .T. )
      oCom:setPosAndSize( { 0, nT + 2 + 20 }, { aCrt[1] - 150, nH - 20 }, .T. )
      oTree:setPosAndSize( { aCrt[1] - 150, nT }, { 150, nH }, .T. )

   ENDIF

   RETURN 1

STATIC PROCEDURE BuildMenuXbp( oCrt, oStatic, oStatic2 )

   LOCAL oMenuBar, oSubMenu

   oMenuBar := WvgMenuBar():new( oCrt ):create()

   // Define submenu in procedural style.
   // The numeric index of the selected menu item
   // is passed to the Callback code block -> mp1

   oSubMenu       := WvgMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Procedural"
   oSubMenu:addItem( { "Play Charge ~1", } )
   oSubMenu:addItem( { "Play Nannyboo ~2", } )
   oSubMenu:itemSelected := {| mp1 | MyFunctionXbp( 100 + mp1 ) }
   oMenuBar:addItem( { oSubMenu, NIL } )

   // Define submenu in the functional style:
   // A menu item executes a code block that
   // calls a function
   oSubMenu       := WvgMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Functional"
   oSubMenu:addItem( { "Play Opening ~1", {|| MyFunctionXbp(1) } } )
   oSubMenu:addItem( { "Play Closing ~2", {|| MyFunctionXbp(2) } } )
   oSubMenu:addItem()
   oSubMenu:addItem( { "~MessageBox"    , {|| MyFunctionXbp(3) } } )
   oMenuBar:addItem( { oSubMenu, NIL } )

   oSubMenu       := WvgMenu():new( oMenuBar ):create()
   oSubMenu:title := "F~eatures"
   oSubMenu:addItem( { "~Hide or Show Left Panel" , {|| IIf(oStatic:isVisible, ;
      oStatic:hide(), oStatic:show()), oCrt:sendMessage( WIN_WM_SIZE, 0, 0 ) } } )
   oSubMenu:addItem( { "~Show My Panel" , {|| oStatic2:show():toFront() } } )
   oSubMenu:addItem()
   oSubMenu:addItem( { "~Font Dialog"   , {|| ExeFontDialog( oCrt ) } } )

   oMenuBar:addItem( { oSubMenu, NIL } )

   RETURN

STATIC FUNCTION BuildToolBarXbp( oCrt )

   LOCAL oTBar := WvgToolBar():new( oCrt, , { 0, 0 }, { 0, 0 }, , .T. )

   oTBar:style        := WVGTOOLBAR_STYLE_FLAT

   oTBar:borderStyle  := WVGFRAME_RECT

   oTBar:buttonWidth  := 28
   oTBar:buttonHeight := 26

   oTBar:imageWidth   := 26
   oTBar:imageHeight  := 24

   oTBar:showToolTips := .T.

   oTBar:create()

   oTBar:addItem( "New"       , hb_DirBase() + "v_new.bmp"    )
   oTBar:addItem( "Select"    , hb_DirBase() + "v_selct1.bmp" )
   oTBar:addItem()
   oTBar:addItem( "FontDlg"   , hb_DirBase() + "v_calend.bmp" )
   oTBar:addItem( "Tools"     , hb_DirBase() + "v_lock.bmp"   )
   oTBar:addItem( "Index"     , hb_DirBase() + "v_index.bmp"  )
   oTBar:addItem()
   oTBar:addItem( "Show"      , hb_DirBase() + "v_clclt.bmp"  )
   oTBar:addItem( "Hide"      , hb_DirBase() + "v_notes1.bmp" )

   RETURN oTBar

STATIC PROCEDURE MyFunctionXbp( nMode )

#define MUSIC_WAITON          { 800, 1600 }

   SWITCH nMode
   CASE 1
      Tone( MUSIC_WAITON[1], 1 )
      Tone( MUSIC_WAITON[2], 1 )
      EXIT

   CASE 2
      Tone( MUSIC_WAITON[2], 1 )
      Tone( MUSIC_WAITON[1], 1 )
      EXIT

   CASE 3
      wapi_MessageBox( , "Button clicked!" )
      EXIT

   CASE 101  // Charge
      Eval({|| Tone( 523, 2 ), Tone( 698, 2 ), Tone( 880, 2 ), Tone( 1046, 4 ), Tone( 880, 2 ), Tone( 1046, 8 ) })
      EXIT

   CASE 102  // NannyBoo
      AEval({ { 196, 2 }, { 196, 2 }, { 164, 2 }, { 220, 2 }, { 196, 4 }, { 164, 4 } }, {| a | Tone( a[1], a[2] ) })
      EXIT

   CASE 103  // BADKEY
      Tone( 480, 0.25 )
      Tone( 240, 0.25 )
      EXIT

   ENDSWITCH

   RETURN

STATIC PROCEDURE ExeFontDialog( oCrt )

   STATIC s_nMode := 0

   LOCAL oFontDlg := WvgFontDialog():new( oCrt )

   oFontDlg:title            := "Select a Screen Font"
   oFontDlg:aPos             := { 150, 150 }
   oFontDlg:buttonApply      := .T.
   oFontDlg:activateApply    := {|| NIL }
   oFontDlg:familyName       := "Courier New"
   oFontDlg:strikeout        := .T.
   oFontDlg:underscore       := .F.
#if 0
   oFontDlg:activateOk       := {|| wapi_MessageBox( , "activateOK Event Handled in Windows!" ) }
#endif
   oFontDlg:nominalPointSize := 12

#if 0
   oFontDlg:size             := .F.
   oFontDlg:style            := .F.
#endif

   oFontDlg:create()

   // Every 2nd FontDialog will be MODAL
   oFontDlg:display( ++s_nMode % 2 )

   oFontDlg:destroy()

   RETURN
