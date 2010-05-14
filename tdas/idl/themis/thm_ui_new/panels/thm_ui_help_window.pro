;+
;NAME:
;thm_ui_help_window
;PURPOSE:
; A widget to display the file 'thm_gui.txt' help
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2010-03-25 10:12:28 -0700 (Thu, 25 Mar 2010) $
;$LastChangedRevision: 7440 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_help_window.pro $
;
;-
Pro thm_ui_help_window_event, event

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
                     /noname,/center, title='Error in Help Window')
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin 
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF

;  what happened?
  widget_control, event.top, get_uval = state
  widget_control, event.id, get_uval = uval
  Case uval Of
    'EXIT': widget_control, event.top, /destroy
  Endcase
  Return
End

Pro thm_ui_help_window, historyWin, gui_id

;catch block for future additions
err=0
catch, err
if err ne 0 then begin
  catch,/cancel
  help,/last_message, output=err_msg
  if obj_valid(historyWin) then $
    for i=0,n_elements(err_msg)-1 do historyWin->update,err_msg[i]
  ok = error_message('Unknown error, the help window will now close',/noname, $
       /center, title='Error in Help Window') 
  widget_control,helpid,/destroy
  thm_gui_error, gui_id, historywin
  return
endif



  help_arr = 'No Help File'
  
 getresourcepath,rpath
 fname = rpath+'thm_users_guide_link.txt'
 if file_test(fname) then begin
   help_arr = strarr(file_lines(fname))
   openr, unit, fname, /get_lun
   readf, unit, help_arr
   free_lun, unit
 endif

;here is the display widget,not editable
  helpid = widget_base(/col, title = 'THEMIS: GUI Help',Group_Leader = gui_id, $
                    /Modal, /Floating)
  helpdisplay = widget_text(helpid, uval = 'HELP_DISPLAY', val = help_arr, $
                            ;xsize = 80, ysize = 40, /scroll, frame = 5)
                            xsize = 140, ysize = 5, /scroll, frame = 5)
;a widget for buttons
  buttons = widget_base(helpid, /row, /align_center)
  exit_button = widget_base(buttons, /col, /align_center)
  exitbut = widget_button(exit_button, val = ' Close ', uval = 'EXIT', $
                        /align_center)
  state = {help:help_arr, historyWin:historyWin, gui_id:gui_id}
  
  CenterTLB, helpid
  widget_control, helpid, set_uval = state, /no_copy
  widget_control, helpid, /realize
  xmanager, 'thm_ui_help_window', helpid, /no_block
  Return
End


