;+
;NAME:
;  thm_ui_overplot_key
;
;PURPOSE:
;  Pops up a window that displays the Themis overview plot key (the same one
;  that's on the website).
;
;CALLING SEQUENCE:
;  thm_ui_overplot_key, gui_id, historyWin, modal=modal
;
;INPUT:
;  gui_id:  The id of the main GUI window.
;  historyWin:  The history window object.
;  
;KEYWORDS:
;  modal = Flag to set the modal of the top level base
;
;OUTPUT:
;  none
;-

pro thm_ui_overplot_key_draw, state

  compile_opt idl2, hidden

  getresourcepath,rpath
  key = read_png(rpath + 'overplotkey.png')
 
  keyImageObj = obj_new('IDLgrImage', key, dimen=[1,1])

  model = obj_new('IDLgrModel')
  model->add, keyImageObj
  
  viewRect = [0.0, 0.0, 1.0, 1.0]
  view = Obj_New('IDLgrView', units=3, viewplane_rect=[0,0,1.,1.])
  view->add, model
  
  scene = obj_new('IDLgrScene')
  scene->add, view

  widget_control, state.keyDisplay, get_value=drawWin

  drawWin->draw, scene

end

pro thm_ui_overplot_key_event, event

  compile_opt idl2, hidden

  Widget_Control, event.TOP, Get_UValue=state, /No_Copy

;catch block for future additions
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
                     /noname,/center, title='Error in Overplot Key')
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin 
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF

  ;kill request block
  IF (Tag_Names(event, /Structure_Name) EQ 'WIDGET_KILL_REQUEST') THEN BEGIN  

    Print, 'Overview Plot Key widget killed' 
    state.historyWin->Update,'THM_UI_OVERPLOT_KEY: Widget killed' 
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    Widget_Control, event.top, /Destroy
    RETURN 
  ENDIF

  IF(TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_DRAW') THEN BEGIN

    ;this code redraws the window if manual redraw & expose events are enabled
    if event.type eq 4 then begin
      thm_ui_overplot_key_draw, state
    endif

    Widget_Control, event.top, Set_UValue=state, /No_Copy
    RETURN
  ENDIF

;  what happened?
;  widget_control, event.top, get_uval = state
  widget_control, event.id, get_uval = uval

  ; check for empty event coming from one of the other event handlers
  if size(uval,/type) eq 0 then begin 
    Widget_Control, event.top, Set_UValue = state, /No_Copy
    RETURN
  endif

  Case uval Of
    'EXIT': BEGIN
      Print, 'Overview Plot Key dismissed.'
      Widget_Control, event.top, Set_UValue=state, /No_Copy
      widget_control, event.top, /destroy
      RETURN
    END
    ELSE: print, 'Not yet implemented'
  Endcase
  
  Widget_Control, event.top, Set_UValue=state, /No_Copy
  
  Return
end

pro thm_ui_overplot_key, gui_id, historyWin, modal=modal

  compile_opt idl2, hidden
  
  keyid = widget_base(/col, title='THEMIS: Overview Plot Key', modal=modal, $
                      group_leader=gui_id, /tlb_kill_request_events)
    keyDisplay = widget_draw(keyid, graphics_level=2, renderer=1, retain=0, $
                             XSize=750, YSIZE=900, units=0, x_scroll_size=750, $
                             y_scroll_size=450, /expose_events)
    buttons= widget_base(keyid, /row, /align_center)
    exitButtonBase = widget_base(buttons, /col, /align_center)
    exitButton = widget_button(exitButtonBase, val=' Close ', uval='EXIT', $
                               /align_center)

  state = {gui_id:gui_id, historyWin:historyWin, keyDisplay:keyDisplay}

  Widget_Control, keyid, Set_UValue=state, /No_Copy
  CenterTLB, keyid
  Widget_Control, keyid, /Realize
  
  Widget_Control, keyid, Get_UValue=state, /No_Copy
  
  thm_ui_overplot_key_draw, state
  
  Widget_Control, keyid, Set_UValue=state, /No_Copy
  Widget_Control, keyid, /Realize
  
  xmanager, 'thm_ui_overplot_key', keyid, /no_block
  Return
end