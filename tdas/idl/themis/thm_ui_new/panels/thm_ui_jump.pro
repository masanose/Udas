;+ 
;NAME:
; thm_ui_jump
;
;PURPOSE:
; small window that allows the user to enter a new start time for the x axis
; 
;CALLING SEQUENCE:
; thm_ui_jump
;
;INPUT:
; gui_id    id of the top level base widget that is calling this routine
;
;OUTPUT:
; 
;HISTORY:
;$LastChangedBy: bckerr $
;$LastChangedDate: 2009-04-03 12:39:05 -0700 (Fri, 03 Apr 2009) $
;$LastChangedRevision: 5544 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_jump.pro $
;
;---------------------------------------------------------------------------------



PRO thm_ui_jump_event, event

  COMPILE_OPT hidden

  Widget_Control, event.TOP, Get_UValue=state, /No_Copy

    ;Put a catch here to insure that the state remains defined

  err_xxx = 0
  Catch, err_xxx
  IF (err_xxx NE 0) THEN BEGIN
    Catch, /Cancel
    Help, /Last_Message, Output = err_msg
    if is_struct(state) then begin
      FOR j = 0, N_Elements(err_msg)-1 DO state.info.historywin->update,err_msg[j]
      x=state.gui_id
      histobj=state.info.historywin
    endif
    Print, 'Error--See history'
    ok=error_message('An unknown error occured and the window must be restarted. See console for details.',$
       /noname, /center, title='Error in Jump')
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin 
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF

    ;Which widget caused this event? 
    
  Widget_Control, event.id, Get_UValue=uval
  
    ;Deal with it
    
  CASE uval OF
    'CANC': BEGIN
       PRINT, 'New File widget canceled' 
       Widget_Control, event.TOP, Set_UValue=state, /No_Copy
       Widget_Control, event.top, /Destroy
       RETURN
    END    
    'OK': BEGIN
       PRINT, 'New File widget canceled' 
       Widget_Control, event.TOP, Set_UValue=state, /No_Copy
       Widget_Control, event.top, /Destroy
       RETURN
    END
    'TEXT': BEGIN
       ; do something here with text
    END
    ELSE: Print, 'Not yet implemented'
  ENDCASE
  
  Widget_Control, event.top, Set_UValue=state, /No_Copy

  RETURN
END ;--------------------------------------------------------------------------------



PRO thm_ui_jump, gui_id, historywin

    ;build master widget

  tlb = Widget_Base(/Col, title='THEMIS: Marker Title', Group_Leader = gui_id, $
                    /Modal, /Floating)

    ;widget bases
    
  spaceBase = Widget_Base(tlb, /Row)
  jumptoBase = Widget_Base(tlb, /Row, XPad=10)
  textBase = Widget_Base(tlb, /Row)
  buttonBase = Widget_Base(tlb, /Row, /Align_Center, XPad=10, YPad=10)

    ;widgets
    
  spaceLabel = Widget_Label(spaceBase, Value='  ')  
  jumptoLabel = Widget_Label(jumptoBase, Value='Jump To: ')
  jumptoText = Widget_Text(jumptoBase, Value='  ', /Editable, XSize=30, UValue='TEXT')
  jumptoLabel = Widget_Label(textBase, Value='                    Format:  YYYY/MM/DD-HH:MM:SS.S')  
  okButton = Widget_Button(buttonBase, Value='    OK     ', UValue='OK')
  cancelButton = Widget_Button(buttonBase, Value='  Cancel   ', UValue='CANC')

  state = {tlb:tlb,gui_id:gui_id, historywin:historywin}

  Widget_Control, tlb, Set_UValue=state, /No_Copy
  Widget_Control, tlb, /Realize
  XManager, 'thm_ui_jump', tlb, /No_Block

  RETURN
END ;--------------------------------------------------------------------------------

