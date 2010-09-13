
;+
; NAME:
;   THM_UI_SPINNER
;
; PURPOSE:
;   A compound 'spinner' widget for editing numerical values.
;   Consists of up and down buttons and a text field for display
;   and direct editing. 
;
; CALLING SEQUENCE:
;   Result = SPINNER(parent)
; 
; KEYWORD PARAMETERS:
;    INCREMENT: Numerical ammount spinner value is incremented/decremented
;    LABEL: Text label for spinner
;    SPIN_TIME: Delay before 'spinning'
;    UNITS: Units (text) to appear to the right of the value
;    VALUE: Initial value
;    XLABELSIZE: Size of the text label
;    TEXT_BOX_SIZE: Size (in char) of the text box
;    TOOLTIP: Tooltip (text)
;
; EVENT STRUCTURE:
;   When the field is modified (either directly) or by the
;   up/down buttons, the following event is returned:
;       {thm_ui_spinner_widget, ID: id, TOP: top, HANDLER: handler, VALUE: value, VALID: valid}
;       
;   VALUE: formatted, double precision number from widget's text field; 
;          value is NaN when the text widget contains an unrecognizable
;          or unconvertable number 
;   VALID: 0 if value is not a recogizable/convertable number, 1 otherwise
;
;
; GETTING/SETTINGS VALUES
;   Using get_value from widget_control will return the same as the VALUE 
;    field in the event structure.
;   The set_value procedure expects numerical input (e.g. doubles, floats, ints).
;   
;-

FUNCTION thm_ui_spinner_button_event, event, down_click=down
  
  compile_opt idl2, hidden
  
    ; Save current select state to later see if the mouse is released.
  widget_control, event.id, SET_UVALUE=event.select
  
  
  IF (event.select eq 0) then begin
    widget_control, event.id, /CLEAR_EVENTS
    RETURN, 0
  ENDIF
  
  
    ; The widget Base and Text Field widget IDs.
  base = widget_info(widget_info(event.id, /PARENT), /PARENT)
  text = widget_info(base, FIND_BY_UNAME='_text')
  handler = base  
    
    ; Find parent's event handler.
  WHILE (widget_info(handler, /VALID)) do begin
    par_event_func = widget_info(handler, /EVENT_FUNC)
    par_event_pro = widget_info(handler, /EVENT_PRO)
    
    IF (par_event_func || par_event_pro) then BREAK
    
    handler = widget_info(handler, /PARENT)
  ENDWHILE


    ; Iterations before starting spin; allows for slow press w/o activating spinner.
  delay = 10              
  result = 0 
  widget_control, text, GET_UVALUE=state
  i = keyword_set(down) ? -1L : 1L
  
  
    ; To stop if the mouse is released.
  WHILE (1) do begin
    ;widget_control, text, GET_VALUE=old
    old = double(state.value)
    oldint = old*(1d/state.increment)
    time = systime(1)
    
    
      ; If rounded to nearest fraction, then increment; otherwise, round off.
      ; Avoid roundoff errors by testing against a tiny #.
    IF ( abs(oldint - round(oldint,/L64)) lt 1d-7 ) then $
      new = old + i*state.increment $ ; + 1d-9*i ;tiny number needed?
    ELSE new = (keyword_set(down) ? floor(oldint,/L64) : ceil(oldint,/L64) )*state.increment
     
     
      ; Update the text widget.        
    new = thm_ui_spinner_num_to_string(new, state.units,state.precision)
    state.value = new
    widget_control, text, SET_VALUE=new, SET_UVALUE=state


      ; Create and feed a new event into any parent's event handler.
    IF (par_event_func || par_event_pro) then begin
      new_event = {thm_ui_spinner_widget, ID: base, TOP: event.top, HANDLER: handler, VALUE: double(new), VALID: 1}
;      if (par_event_func) $
;        then result = call_function(par_event_func, new_event) $
;        else call_procedure, par_event_pro, new_event
    ENDIF

    ; Delay start of spin
    ; ### loop ### 
    repeat begin
  
        ; Check for new event (mouse - unclick)
      newevent = widget_event(event.top, BAD_ID=bad, /NOWAIT)
      
        ; Quit if bad ID occurs.
      IF (bad ne 0L) then RETURN, 0
  
      widget_control, event.id, GET_UVALUE = x

        ; End if mouse was released and
        ; send to parent function/procedure
      IF (x eq 0) then begin
        if (par_event_func) then begin
          return, call_function(par_event_func, new_event)
        endif else if (par_event_pro) then begin
          call_procedure, par_event_pro, new_event
        endif
        return,0
      ENDIF
  
        ; Delay before starting spin.
      IF (delay ne 0) then begin
        WAIT, 0.05d
        delay--
      ENDIF
    
    endrep until delay eq 0

    elapsed = systime(1) - time
    
    IF (elapsed lt state.spinTime) then wait, state.spinTime - elapsed

  ENDWHILE

END ;---------------------------------------------------------------------



FUNCTION thm_ui_spinner_update_event, event

      compile_opt idl2, hidden
      on_ioerror, null
  
  ; Pull new values and save old for reset
  widget_control, event.id, GET_VALUE=new, GET_UVALUE=state
  
    ; For testing/debugging only
  ;if new[0] eq (state.value+state.units) then return,0
  ;if new[0] eq '-*' then return,0
   
  ;carriage return
  if  Tag_Names(event,/Structure_Name) eq 'WIDGET_TEXT_CH' && event.ch eq 10 then begin
    widget_control,event.id,set_value=state.value+state.units
    parent = widget_info(event.id, /PARENT)
    RETURN, {thm_ui_spinner_widget, ID: parent, TOP: event.top, HANDLER: event.handler, VALUE: new[0], VALID:is_numeric(new[0])}
  endif


  ;If there are units on the value, then remove them
  if stregex(new,'.*' + state.units + '.*$',/boolean) then begin
    new = (stregex(new,' *(.*)'+state.units + '.*$',/extract,/subexpr))[1]
  endif
  offset = widget_info(event.id, /text_select)
  widget_control,event.id,set_value=new+state.units,set_text_select=offset

  parent = widget_info(event.id, /PARENT)

  if is_numeric(new) then begin  ;if the input is a correctly formatted numerical value then store it, and generate an event
    state.value = new
    widget_control,event.id,set_uvalue=state
      on_ioerror, fail
    RETURN, {thm_ui_spinner_widget, ID: parent, TOP: event.top, HANDLER: event.handler, VALUE: double(new[0]), VALID: 1}
    fail: RETURN, {thm_ui_spinner_widget, ID: parent, TOP: event.top, HANDLER: event.handler, VALUE: !values.D_NAN, VALID: 0}
  endif else begin
    return, {thm_ui_spinner_widget, ID: parent, TOP: event.top, HANDLER: event.handler, VALUE: !values.D_NAN, VALID: 0}
  endelse
 
  
END ;---------------------------------------------------------------------



FUNCTION thm_ui_spinner_up_click, event

      compile_opt idl2, hidden
  RETURN, thm_ui_spinner_button_event(event)
  
END ;---------------------------------------------------------------------



FUNCTION thm_ui_spinner_down_click, event

      compile_opt idl2, hidden  
  RETURN, thm_ui_spinner_button_event(event, /down_click)
  
END ;---------------------------------------------------------------------



FUNCTION thm_ui_spinner_getvalue, base

      compile_opt idl2, hidden

  widget_control, widget_info(base, FIND_BY_UNAME='_text'), GET_VALUE=value,get_uvalue=state
  
  value = value[0]
  
  ;If there are units on the value, then remove them
  if stregex(value,'.*' + state.units + '.*$',/boolean) then begin
    value = (stregex(value,' *(.*)'+state.units + '.*$',/extract,/subexpr))[1]
  endif
  
  ;Return NaN if value isn't numeric or double() conversion fails
  if is_numeric(value) then begin
      on_ioerror, fail
    return, double(value)
    fail: return, !values.D_NAN
  endif else begin
    return,!values.D_NAN
  endelse

END ;---------------------------------------------------------------------



PRO thm_ui_spinner_setvalue, base, value

      compile_opt idl2, hidden
  
  ; Set text widget value
  text = widget_info(base, FIND_BY_UNAME='_text')
  offset = widget_info(text, /text_select)
  
  ;widget_control, text, SET_VALUE=string(value), SET_TEXT_SELECT = offset
  
  widget_control,text,get_uvalue=state
  state.value = thm_ui_spinner_num_to_string(value,state.units,state.precision)
  widget_control, text, SET_VALUE=state.value,set_text_select=offset
  
  ;Update spinner value with event handler
  set = thm_ui_spinner_update_event( {ID: text, TOP: base, HANDLER: text} )
  
END ;---------------------------------------------------------------------



FUNCTION thm_ui_spinner_num_to_string, num_in, units_in,precision

        compile_opt idl2, hidden
  
   data = {timeaxis:0,$
           formatid:precision,$ ;default 10
           scaling:0,$
           exponent:1}
  
   ; RETURN, string(num_in,FORMAT='(G0)')+  units_in
   RETURN, formatannotation(0,0,num_in,data=data) +  units_in

END ;---------------------------------------------------------------------

PRO thm_ui_spinner_remove_spaces, string_in

        compile_opt idl2, hidden
  
  if ~strmatch(string_in, '*[! ]*') then return
  while strmatch(string_in, '*[ ]*') do begin
      bst = byte(string_in)
      space = byte(' ')
      bst = bst[where(bst ne space[0],count)]
      string_in = string(bst)
  endwhile
  
END ;---------------------------------------------------------------------



FUNCTION thm_ui_spinner, parent,     $
  INCREMENT=inc_set,          $
  LABEL=label_set,            $
  SPIN_TIME=spin_time_set,    $
  UNITS=unit_set,             $
  VALUE=value_set,            $
  XLABELSIZE=label_size_set,  $
  TEXT_BOX_SIZE=text_box_size,$
  getXLabelSize=size_out_var, $
  ALL_EVENTS=all_events,      $
  TOOLTIP=tooltip,            $
  precision=precision,        $
  _EXTRA=_extra
  

      compile_opt idl2, hidden
      
 ; if ~keyword_set(precision) then begin
    precision = 16
  ;'endif

; Initiations & checks
  increment = n_elements(inc_set) ? double(inc_set[0]) : 0.1d
  spinTime = keyword_set(spin_time_set) ? (double(spin_time_set) > 0d) : 0.05d
  units = keyword_set(unit_set) ? unit_set : ''
  value =  thm_ui_spinner_num_to_string( keyword_set(value_set) ? value_set : 0, units,precision)
  tboxsize = keyword_set(text_box_size) ? text_box_size : 8
  state = {VALUE: value, INCREMENT: increment, SPINTIME: spinTime, UNITS: units,precision:precision}
  
  
; General base for each part of the compound widget
  base = widget_base(parent, /ROW,            $
    FUNC_GET_VALUE='thm_ui_spinner_getvalue',        $          
    PRO_SET_VALUE='thm_ui_spinner_setvalue',         $
    XPAD=0, YPAD=0, SPACE=1, _EXTRA=_extra)



; Label
  label = (keyword_set(label_set)) ? $
          widget_label(base, VALUE=label_set, XSIZE=label_size_set) : '' 

  if arg_present(size_out_var) then begin
    geo_info = widget_info(label,/geometry)
    size_out_var = geo_info.scr_xsize
  endif
  
; Text 
  text = widget_text(base, /EDITABLE, /all_events,             $
    EVENT_FUNC='thm_ui_spinner_update_event',                         $
    IGNORE_ACCELERATORS=['Ctrl+C','Ctrl+V','Ctrl+X','Del'],    $
    VALUE=value, UNAME='_text', UVALUE=state, XSIZE=tboxsize)


;Buttons
  button_base = widget_base(base, /ALIGN_CENTER, /COLUMN, /TOOLBAR, XPAD=0, YPAD=0, SPACE=0)

; Extra pixel added to button padding for Windows 
  one = !version.os_family ne 'Windows'

; 'Up' Button
  up_button = widget_button(button_base, EVENT_FUNC='thm_ui_spinner_up_click',    $
    /BITMAP, VALUE= filepath('spinup.bmp', SUBDIR=['resource','bitmaps']), $
    /PUSHBUTTON_EVENTS, UNAME='_up',UVALUE=0, XSIZE=16+one, YSIZE=10+one, $
    tooltip=tooltip)

; 'Down' Button
  down_button = widget_button(button_base, EVENT_FUNC='thm_ui_spinner_down_click', $
    /BITMAP, VALUE=filepath('spindown.bmp', SUBDIR=['resource','bitmaps']), $
    /PUSHBUTTON_EVENTS, UNAME='_down', UVALUE=0, XSIZE=16+one, YSIZE=10+one, $
    tooltip=tooltip)


  RETURN, base
  
END


