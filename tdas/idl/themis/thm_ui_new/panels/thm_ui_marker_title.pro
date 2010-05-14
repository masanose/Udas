;+
;NAME:
; thm_ui_marker_title
;
;PURPOSE:
; this window is displayed whenever a user has marked an area
; the window handles marker information such as title
;    
;CALLING SEQUENCE:
; thm_ui_marker_title, gui_id
;INPUT:
; gui_id     id of top level base widget from calling program
;
;OUTPUT:
; 
;HISTORY:
;$LastChangedBy: cgoethel_new $
;$LastChangedDate: 2009-07-02 12:55:47 -0700 (Thu, 02 Jul 2009) $
;$LastChangedRevision: 6383 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_marker_title.pro $
;
;--------------------------------------------------------------------------------



PRO thm_ui_marker_title_event, event

  Compile_Opt hidden

  Widget_Control, event.TOP, Get_UValue=state, /No_Copy

    ;Put a catch here to insure that the state remains defined

  err_xxx = 0
  Catch, err_xxx
  If(err_xxx Ne 0) Then Begin
    Catch, /Cancel
    Help, /Last_Message, output = err_msg
    FOR j = 0, N_Elements(err_msg)-1 DO PRINT, err_msg[j]
    Print, 'Error--See history'
    histobj=state.historywin
    x=state.gui_id
    Widget_Control, event.top, Set_UValue=state, /No_Copy
    thm_gui_error,x,histobj
    RETURN
  EndIf

   ; Get the instructions from the widget causing the event and
   ; act on them.

  Widget_Control, event.id, Get_UValue=uval
  CASE uval OF
     'CANC': BEGIN
      Print, 'New File widget canceled' 
      state.markerTitle->SetProperty, Cancelled=1, Name=''
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    END 
    'NAME': BEGIN
      state.markerTitle->GetProperty, UseDefault=useDefault
      Widget_Control, event.id, Get_Value=name
      IF useDefault EQ 1 THEN state.markerTitle->SetProperty, Name=name, DefaultName=name $
        ELSE state.markerTitle->SetProperty, Name=name
      state.statusBar->Update, String('Marker title has been set to '+name)
      state.historyWin->Update, String('Marker title has been set to '+name)
    END
    'MAKE': BEGIN
      result = Widget_Info(event.id, /Button_Set)
      IF result EQ 0 THEN BEGIN
         state.markerTitle->SetProperty, UseDefault=0, DefaultName=''
      ENDIF ELSE BEGIN
         state.markerTitle->GetProperty, Name=name
         state.markerTitle->SetProperty, UseDefault=1, DefaultName=name
         state.statusBar->Update, String('The default marker title has been set to '+name)
         state.historyWin->Update, String('The default marker title has been set to '+name)        
      ENDELSE
    END
;    'DONT': BEGIN
;      result = Widget_Info(event.id, /Button_Set)
;      IF result EQ 0 THEN state.markerTitle->SetProperty, DoNotAsk=0 $
;         ELSE state.markerTitle->SetProperty, DoNotAsk=1
;       IF result EQ 0 THEN state='on' ELSE state='off' 
;       state.statusBar->Update, String('The marker title window has been turned '+state)
;       state.historyWin->Update, String('The marker title window has been turned '+state)        
;    END     
    'OK': BEGIN
      Print, 'New File widget canceled'
      state.markerTitle->SetProperty, Cancelled=0 
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    END
    
    ELSE: Print, 'Not yet implemented'
  ENDCASE

  Widget_Control, event.top, Set_UValue=state, /No_Copy

  RETURN
END ;--------------------------------------------------------------------------------



PRO thm_ui_marker_title, gui_id, markerTitle, historyWin, statusBar

      ;top level base widget
      
  tlb = Widget_Base(/Col, Title='THEMIS: Marker Title', Group_Leader=gui_id, $
                    /Modal, /Floating)
                    
      ;base widgets
                    
  markerTBase = Widget_Base(tlb, /Row) 
  markerBBase = Widget_Base(tlb, /Col, /Nonexclusive, sensitive=0) 
  buttonBase = Widget_Base(tlb, /Row, /Align_Center)
 
      ;get initial values for widgets
      
  markerTitle->GetProperty, Name=name, UseDefault=useDefault, DefaultName=defaultName

      ;widgets
    
  markerTLabel = Widget_Label(markerTBase, Value='Marker Title: ')
  markerTText = Widget_Text(markerTBase, Value=name, /Editable, XSize=30, UValue='NAME', /All_Events)
  makeButton = Widget_Button(markerBBase, $
    Value='Make this the default when creating new markers', UValue='MAKE')
  IF UseDefault EQ 0 THEN Widget_Control, makeButton, set_button=0 $
    ELSE Widget_Control, makeButton, set_button=1
;  dontButton = Widget_Button(markerBBase, Value='Do not ask for title, always use default', $
;    UValue='DONT')
;  IF DoNotAsk EQ 0 THEN Widget_Control, dontButton, set_button=0 $
;    ELSE Widget_Control, dontButton, set_button=1
  okButton = Widget_Button(buttonBase, Value='    OK     ', UValue='OK')
  cancelButton = Widget_Button(buttonBase, Value = '  Cancel   ', UValue='CANC')

  state = {tlb:tlb, gui_id:gui_id, markerTitle:markerTitle, historyWin:historyWin, statusBar:statusBar}

  Widget_control, tlb, Set_UValue=state, /No_Copy
  Widget_control, tlb, /Realize
  XManager, 'thm_ui_marker_title', tlb, /No_Block

  RETURN
END ;--------------------------------------------------------------------------------

