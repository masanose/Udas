;+ 
;NAME:
; thm_ui_load_data_file_obs_sel.pro
;
;PURPOSE:
; Controls actions that occur when selecting items in probe/station box.  Called
; by thm_ui_load_data_file event handler.
;
;CALLING SEQUENCE:
; thm_ui_load_data_file_obs_sel, state
;
;INPUT:
; state     State structure
;
;OUTPUT:
; None
;
;HISTORY:
;-
pro thm_ui_load_data_file_obs_sel, state

  Compile_Opt idl2, hidden
  
  pindex = widget_info(state.observList, /list_select)
  if ~array_equal(pindex, -1, /no_typeconv) then begin
  
    all_chosen = where(pindex Eq 0, nall)
    if(nall gt 0) then observ0 = (*state.validobserv)[1:*] $
      else observ0 = (*state.validobserv)[pindex]
    if (ptr_valid(state.observ)) then ptr_free, state.observ
    state.observ = ptr_new(observ0)
    
;    state.statusText->Update, h
;    state.historyWin->Update, 'LOAD DATA: ' + h
    
    instr_in = state.instr
    
    if (instr_in eq 'asi' or instr_in eq 'ask') then begin
      if ptr_valid(state.astation) then ptr_free, state.astation
      state.astation = ptr_new(observ0)
    endif else if (instr_in eq 'gmag') then begin
      if ptr_valid(state.station) then ptr_free, state.station
      state.station = ptr_new(observ0)
    endif else begin
    
      if(nall gt 0) then observ0 = (*state.validobserv)[1:5] $ ; make sure that asterisk doesn't
        else observ0 = (*state.validobserv)[pindex]            ; select flatsat probe
      if (ptr_valid(state.observ)) then ptr_free, state.observ
      state.observ = ptr_new(observ0)
      if ptr_valid(state.probe) then ptr_free, state.probe
      state.probe = ptr_new(observ0)
    end

    message_pre = 'Chosen '+state.observ_label
    h = thm_ui_new_multichoice_history(message_pre, observ0)
 
  endif else begin
  
    If(state.instr Eq 'asi' Or state.instr Eq 'ask') Then Begin
      If(ptr_valid(state.astation)) Then ptr_free, state.astation
      h = 'No Chosen Asi_station'
      state.statusText->Update, h
    Endif Else If(state.instr Eq 'gmag') Then Begin
      If(ptr_valid(state.station)) Then ptr_free, state.station
      h = 'No Chosen Gmag_station'
      state.statusText->Update, h
    Endif Else Begin
      If(ptr_valid(state.probe)) Then ptr_free, state.probe      
      ;h = 'probe = '+''''+''''
      h = 'No Chosen Probe'
      state.statusText->Update, h
    Endelse
    widget_control,state.observlist, set_value=*state.validobservlist
    return
  endelse

  state.statusText->Update, h
  state.historyWin->Update, 'LOAD DATA: ' + h
  
END