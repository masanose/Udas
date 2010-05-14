;+
;NAME:
; thm_ui_widget_template
;
;PURPOSE:
;  template that contains repeated code in widget creation
;    
;CALLING SEQUENCE:
; thm_ui_widget_template,gui_id
; 
;INPUT:
; gui_id:  id of top level base widget from calling program
;
;OUTPUT:
; 
;HISTORY:
;$LastChangedBy: mfeuerstein $
;$LastChangedDate: 2009-02-04 17:32:09 -0800 (Wed, 04 Feb 2009) $
;$LastChangedRevision: 4788 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_widget_template.pro $
;
;--------------------------------------------------------------------------------

pro thm_ui_widget_template_event,event

  compile_opt hidden,idl2

  Widget_Control, event.TOP, Get_UValue=state, /No_Copy

  ;Put a catch here to insure that the state remains defined

  err_xxx = 0
  Catch, err_xxx
  If(err_xxx Ne 0) Then Begin
    Catch, /Cancel
    Help, /Last_Message, output = err_msg
    FOR j = 0, N_Elements(err_msg)-1 DO PRINT, err_msg[j]
    ;Print, 'Error--See history'
    histobj=state.historywin
    x=state.gui_id
    Widget_Control, event.top, Set_UValue=state, /No_Copy
    thm_gui_error,x,histobj
    RETURN
  EndIf
  
  IF(Tag_Names(event, /Structure_Name) EQ 'WIDGET_KILL_REQUEST') THEN BEGIN  
    Exit_Sequence:
    Print, 'Widget Killed' 
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy    
    Widget_Control, event.top, /Destroy
    RETURN      
  ENDIF

  Widget_Control, event.id, Get_UValue=uval
  CASE uval OF
    'CANC': BEGIN
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    END
    'OK': BEGIN
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    END
    ELSE: Print,'Not yet implemented'
  ENDCASE
    
  Widget_Control, event.top, Set_UValue=state, /No_Copy

  RETURN

end


pro thm_ui_widget_template,gui_id, historywin

  compile_opt idl2
  
  tlb = Widget_Base(/Col, Title='Template', Group_Leader=gui_id, $
    /Modal, /Floating,/tlb_kill_request_events)
    
  button_row = widget_base(tlb,/row)
  status_row = widget_base(tlb,/row)
  
  ok_button = widget_button(button_row,value='OK',uvalue='OK')
  canc_button = widget_button(button_row,value='Cancel',uvalue='CANC')  
  
  statusBar = Obj_New("THM_UI_MESSAGE_BAR", status_row, Xsize=30, $
YSize=1)
  
  state = {tlb:tlb,  $
           gui_id:gui_id, $
           statusBar:statusBar, $
           historywin:historywin}
            
  Widget_Control, tlb, Set_UValue = state, /No_Copy
  Widget_Control, tlb, /Realize
  
  XManager, 'thm_ui_widget_template', tlb, /No_Block

  return
  
end
