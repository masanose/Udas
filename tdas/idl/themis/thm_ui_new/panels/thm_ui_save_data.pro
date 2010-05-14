;+ 
;NAME:
; thm_ui_save_data
;
;PURPOSE:
; user interface panel for user to select whether to save data with the 
; THEMIS GUI Document along with the other settings that are saved in the file
;
;CALLING SEQUENCE:
; result = thm_ui_data(gui_id)   where result 1=save w/data, 0=save settings only
;
;INPUT:
; gui_id    widget id of calling program
;
;OUTPUT:
; 
;HISTORY:
;$LastChangedBy: cgoethel_new $
;$LastChangedDate: 2009-06-08 09:43:53 -0700 (Mon, 08 Jun 2009) $
;$LastChangedRevision: 6062 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_save_data.pro $
;
;---------------------------------------------------------------------------------



Pro thm_ui_save_data_event, event

  Compile_Opt hidden

  Widget_Control, event.TOP, Get_UValue=state, /No_Copy

    ;Put a catch here

  err_xxx = 0
  Catch, err_xxx
  IF (err_xxx NE 0) THEN BEGIN
    Catch, /Cancel
    Help, /Last_Message, Output = err_msg
    if is_struct(state) then begin
      FOR j = 0, N_Elements(err_msg)-1 DO state.historywin->update,err_msg[j]
      x=state.gui_id
      histobj=state.historywin
    endif
    Print, 'Error--See history'
    ok=error_message('An unknown error occured and the window must be restarted. See console for details.',$
       /noname, /center, title='Error in Save Data')
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin 
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF

  Widget_Control, event.id, Get_UValue=uval
  
  CASE uval OF
    'CANC': BEGIN
      Print, 'New File widget canceled' 
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    END    
    'OK': BEGIN
      Print, 'New File widget canceled' 
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    END
    
    Else: Print, 'Not yet implemented'
  EndCase
  
  Widget_Control, event.top, Set_UValue = state, /No_Copy

  RETURN
END ;--------------------------------------------------------------------------------



PRO thm_ui_save_data, gui_id, historywin

      ;top level land main bases
      
  tlb = Widget_Base(/Col,Title='THEMIS: Save with Data ', Group_Leader=gui_id, $
                    /Modal, /Floating)
  topBase = Widget_Base(tlb, /Row, /Align_Top, /Align_Left, YPad=1, XPad=10) 
  radioBase = Widget_Base(tlb, /Col, /Align_Center, YPad=8, /Exclusive) 
  buttonBase = Widget_Base(tlb, /Row, /Align_Center, YPad=8) 

  topLabel = Widget_Label(topBase, Value='Which portion of the data would you like to save?')
  
  fieldsButton = Widget_Button(radioBase, Value='Just those fields used in the plot')
  allDataButton = Widget_Button(radioBase, Value='All data')
  Widget_Control, allDataButton, /Set_Button
  
  okButton = Widget_Button(buttonBase, Value='    OK     ', UValue='OK')
  cancelButton = Widget_Button(buttonBase, Value='  Cancel   ', UValue='CANC')

  state = {tlb:tlb, gui_id:gui_id,historywin:historywin}

  CenterTlb, tlb
  Widget_Control, tlb, Set_UValue=state, /No_Copy
  Widget_Control, tlb, /Realize
  XManager, 'thm_ui_save_data', tlb, /No_Block

  RETURN
END ;--------------------------------------------------------------------------------

