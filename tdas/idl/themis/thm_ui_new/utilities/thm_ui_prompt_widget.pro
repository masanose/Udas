
;+ 
;NAME:
; thm_ui_prompt_widget.pro
;
;PURPOSE:
;  Simple standardized popup, for yes/no,yes_to_all,no_to_all,cancel 
;  Like dialog_message, but allows additional options, and automatically logs/prompts
;
;CALLING SEQUENCE:
;  result = thm_ui_prompt_widget(parent,statusbar,historywin,"Continue?")
;
;INPUT: 
;  parent: Widget Id of parent widget(Note that if parent is invalid, it will block until a response is received. See documentation for XMANAGER:NO_BLOCK keyword)
;  statusBar: The statusbar object to which output should be sent, if unavailable pass null object(obj_new())
;  historywin:  The historywin object to which output should be sent if unavailable pass null object(obj_new())
;  promptText=promptText: The text of the prompt to be displayed to the user.
;  no=no :Include "No" Button
;  yes=yes: Include "Yes" Button
;  allno=allno: Include "No To All" button.
;  allyes=allyes: Include "Yes To All" button.
;  cancel=cancel: Include "Cancel" button.
;  ok=ok: Include "Ok" button.
;  maxwidth=maxwidth: Control the width at which the prompt starts wrapping prompt text.
;  defaultValue=defaultValue: Set to string to return as default. (Occurs during error or close by clicking "X")
;                    Normally default is value of right-most button.
;  title=title:  Set window title to string.
;  
;  traceback=traceback: Do a trace to calling location
;  
;
;OUTPUT:
; Returns an all lower case string with the response text:
; "no","yes","yestoall","notoall","cancel","ok"
; 
; 
;NOTES:
;  1. If no button keywords are set, "ok" is used.
;  2. Based heavily on deprecated gui load subroutine: thm_ui_load_clob_prompt:
;  3. If a parent widget is unavailable, statusbar, or historywin unavailable, you can pass null values
;    result = thm_ui_prompt_widget(0l,obj_new(),obj_new(),prompt="Continue?")
;    This call will interact with other widgets in a way that is similar to a call to error_message 
;  
;
;HISTORY:
;-

pro thm_ui_prompt_widget_event, event

  compile_opt idl2, hidden
  
  Widget_Control, event.TOP, Get_UValue=state, /No_Copy

  ;Put a catch here to insure that the state remains defined

  err_xxx = 0
  Catch, err_xxx
  IF (err_xxx NE 0) THEN BEGIN
    Catch, /Cancel
    Help, /Last_Message, Output = err_msg
    if is_struct(state) && obj_valid(state.historywin) then begin
      FOR i = 0, N_Elements(err_msg)-1 DO state.historywin->update,err_msg[i]
    endif
    Print, 'Error--See history'
    ok=error_message('An unknown error occured and the window must be restarted. See console for details.',$
       /noname, /center, title='Error in thm_ui_prompt_widget.')
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if is_struct(state) && widget_valid(state.parent) && obj_valid(state.historyWin) then begin 
      thm_gui_error,state.parent,state.historyWin
    endif
    RETURN
  ENDIF

  Widget_Control, event.id, Get_UValue=uval
    
  if is_string(uval) then begin
    *state.answer = uval  
    Widget_Control, event.top, Set_UValue=state, /No_Copy
    Widget_Control, event.top, /Destroy
  endif

  RETURN
end

function thm_ui_prompt_widget,$
                       parent,$
                       statusBar,$
                       historyWin,$
                       promptText=promptText,$
                       no=no,$
                       yes=yes,$ 
                       allno=allno,$
                       allyes=allyes,$         
                       cancel=cancel,$
                       ok=ok,$
                       maxwidth=maxwidth,$
                       defaultValue=defaultValue,$
                       title=title,$
                       traceback=traceback
                       
  compile_opt idl2
  
  if ~is_string(title,/blank) then begin
    title = 'Please Respond'
  endif
  
  if ~is_string(promptText,/blank) then begin
    promptText = ' '
  endif 
  
  if ~keyword_set(maxwidth) then begin
    maxwidth = 100
  endif
                       
  if widget_valid(parent) then begin
    tlb = widget_base(/col, title=title, $
                    group_leader=parent, /modal, /base_align_center)
  endif else begin
    tlb = widget_base(/col, title=title, /base_align_center)
    parent = 0
  
;    if obj_valid(statusbar) then begin
;      statusBar->update,"Could not open prompt widget, illegal parent text/title: " + promptText + " / " + title
;    endif 
;   
;    if obj_valid(historywin) then begin
;      historywin->update,"Count not open prompt widget, illegal parent, text/title: " + promptText + " / " + title
;    endif
    
    ;return,"ok"
  endelse                     
 
  ;calculate minimum width required to display text box                               
  width = 0
  ;value are number of character for button labels +2 for button beveling
  width += keyword_set(yes) ? 7 : 0
  width += keyword_set(no) ? 6 : 0
  width += keyword_set(allyes) ? 13 : 0  
  width += keyword_set(allno) ? 12 : 0
  width += keyword_set(cancel) ? 10 : 0
  width += (keyword_set(ok) ||(~keyword_set(allyes) && ~keyword_set(allno) && $
    ~keyword_set(yes) && ~keyword_set(no) && ~keyword_set(cancel))) ? 6 : 0 
                       
  width = max([width,strlen(strsplit(promptText,thm_ui_newline(),/extract))])
  width += 2
  width = width < maxwidth 
  
  height = long(strlen(promptText)+2) / long(width) + 1
      
  textBase = widget_text(tlb, value=promptText,xsize=width,editable=0,/wrap,ysize=height)                  
  buttonBase = widget_base(tlb, /row, /align_center)

  ;default ordering and button ordering are not the same for yestoall & notoall.  If present with yes/no, 
  ; the non-all option will be the default, but the all-button will be place to the right of the all-button
  if keyword_set(allyes) then begin
    default = "yestoall"
  endif

  if keyword_set(yes) then begin
    yesButton = widget_button(buttonBase, value=' Yes ', uvalue='yes')
    default = "yes"
  endif
  
  if keyword_set(allyes) then begin
    yesToAllButton = widget_button(buttonBase, value=' Yes To All ', uvalue='yestoall')
  endif
  
  if keyword_set(allno) then begin
    default = "notoall"
  endif
  
  if keyword_set(no) then begin
    noButton = widget_button(buttonBase, value=' No ', uvalue='no')
    default = "no"
  endif
  
  if keyword_set(allno) then begin
    noToAllButton = widget_button(buttonBase, value=' No To All ', uvalue='notoall')
  endif
  
  if keyword_set(ok) || (~keyword_set(allyes) && ~keyword_set(allno) && $
    ~keyword_set(yes) && ~keyword_set(no) && ~keyword_set(cancel)) then begin
    okButton = widget_button(buttonBase, value=' Ok ', uvalue='ok')
    default = "ok"
  endif
  
  if keyword_set(cancel) then begin
    cancelButton = widget_button(buttonBase, value=' Cancel ', uvalue='cancel')
    default = "cancel"
  endif
  
  ;don't use is_string because it_string doesn't count '' as a string
  if size(defaultValue,/type) eq 7 then begin
    default = defaultValue
  endif
  
  answer = ptr_new(default)
  
  if obj_valid(statusBar) then begin
    statusBar->update,"Popup Requires Response, Prompt Is: " + promptText
  endif
  
  if obj_valid(historyWin) then begin
    historyWin->update,"Opening Popup With Prompt: " + promptText  
  endif
  
  if Keyword_Set(traceback) then begin
  
    Help, Calls=callStack
    callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]
    Help, /Last_Message, Output=idl_traceback
    traceback = scope_traceback()
    Print,''
    Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
    Print, ''
    for j=0,N_Elements(traceback)-1 do Print, "     " + traceback[j]
   
    if keyword_set(idl_traceback[0]) then begin
      print,'Last IDL Error: '
      print,idl_traceback[0]
      if n_elements(idl_traceback) gt 1 then begin
        print,idl_traceback[1]
      endif
    endif
   
  endif
  
  state = {tlb:tlb, parent:parent, statusBar:statusBar,historyWin:historyWin, answer:answer}
 
  centertlb, tlb
  Widget_Control, tlb, Set_UValue=state, /No_Copy
  Widget_Control, tlb, /Realize
  XManager, 'thm_ui_prompt_widget', tlb, No_Block=widget_valid(parent)
 
  if obj_valid(statusBar) then begin
    statusBar->update,"Popup Received Response: " + *answer
  endif
  
  if obj_valid(historyWin) then begin
    historyWin->update,"Closing Popup With Response: " + *answer  
  endif
  
  return, *answer
end