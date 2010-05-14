;+ xpad
;NAME:
;  thm_ui_part_getspec_options
; 
;PURPOSE:
;  A interface to THM_PART_GETSPEC.PRO for creating and loading THEMIS energy/
;  angular particle spectra into the GUI.  Intended to be called from
;  THM_UI_INIT_LOAD_WINDOW.PRO.
; 
;CALLING SEQUENCE:
;  thm_ui_part_getspec_options, tab_id, loadedData, historyWin, statusText, $
;                              trObj, timeWidget=timeWidget
;INPUT:
;  tab_id:  The widget id of the tab.
;  loadedData:  The loadedData object.
;  historyWin:  The history window object.
;  statusText:  The status bar object for the main Load window.
;  trObj:  The GUI timerange object.
; 
;KEYWORDS:
;  timeWidget = The time widget object.
; 
;OUTPUT:
; No explicit output, new variables are created. For a given data
; type, one or two spectra are created. One is the angular spectrum
; for the full energy range. If the energy spectrum option is chosen,
; then another spectrum is created which is the energy spectrum for
; the full angular range.
; 
;HISTORY:
; 5-jan-2009, jmm, jimm@ssl.berkeley.edu
; 14-jan-2009, jmm, added statusbar object
; 15-jan-2009, jmm, added external_state, so that the active data
;                   widget on the dproc panel can be updated.
; 23-jan-2009, jmm, deletes tplot variables created during processing,
;                   correctly updates active data
; 13-may-2009, bck, moved code from Data Processing window to Load window
; 
;$LastChangedBy: aaflores $
;$LastChangedDate: 2010-02-08 10:21:08 -0800 (Mon, 08 Feb 2010) $
;$LastChangedRevision: 7223 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_part_getspec_options.pro $
;-

; reset all
pro thm_ui_part_getspec_options_reset, state

  compile_opt idl2, hidden

  state.suffix = ''
  state.def_angle = 'PHI'  
  state.angle_values[*, 0] = ['0', '360']
  state.angle_values[*, 1] = ['-90', '90']
  state.angle_values[*, 2] = ['0', '180']
  state.angle_values[*, 3] = ['0', '360']
  state.start_angle_value = '0'
  state.erange_values = ['0', '1e7']
  state.energy_set = 0b
  state.normalize = 0b
  state.regrid_values = ['16', '8']
  state.datagap_value = '0'
  state.remove_mask = 0b
  state.mask_value = '0.99'
  state.spvalue = 0b
  state.def_sp = 'SPIN_FIT'
  state.limvalue = '2.0'
  state.def_fill = 'INTERPOLATION'
  
  widget_control, state.probeList , set_list_select=-1
  widget_control, state.dataTypeList, set_list_select=-1
  
  id=widget_info(state.tab_id, find_by_uname='SUFFIX')
  widget_control, id, set_value=''
  state.suffix=''
  widget_control, state.angle_combobox, set_combobox_select=0
  
  id=widget_info(state.tab_id, find_by_uname='ANGLE:PHI:MIN')
  widget_control, id, set_value=state.angle_values[0, 0]
  id=widget_info(state.tab_id, find_by_uname='ANGLE:PHI:MAX')
  widget_control, id, set_value=state.angle_values[1, 0]
  id=widget_info(state.tab_id, find_by_uname='ANGLE:THETA:MIN')
  widget_control, id, set_value=state.angle_values[0, 1]
  id=widget_info(state.tab_id, find_by_uname='ANGLE:THETA:MAX')
  widget_control, id, set_value=state.angle_values[1, 1]        
  id=widget_info(state.tab_id, find_by_uname='ANGLE:PA:MIN')
  widget_control, id, set_value=state.angle_values[0, 2]
  id=widget_info(state.tab_id, find_by_uname='ANGLE:PA:MAX')
  widget_control, id, set_value=state.angle_values[1, 2]
  id=widget_info(state.tab_id, find_by_uname='ANGLE:GYRO:MIN')
  widget_control, id, set_value=state.angle_values[0, 3]
  id=widget_info(state.tab_id, find_by_uname='ANGLE:GYRO:MAX')
  widget_control, id, set_value=state.angle_values[1, 3]
  id=widget_info(state.tab_id, find_by_uname='ANGLE:START')
  widget_control, id, set_value=state.start_angle_value

  id=widget_info(state.tab_id, find_by_uname='ENERGY:MIN')
  widget_control, id, set_value=state.erange_values[0]
  id=widget_info(state.tab_id, find_by_uname='ENERGY:MAX')
  widget_control, id, set_value=state.erange_values[1]
  id=widget_info(state.tab_id, find_by_uname='ENERGY:SET')
  widget_control, id, set_button=state.energy_set

  id=widget_info(state.tab_id, find_by_uname='NORM:SET')
  widget_control, id, set_button=state.normalize
  id=widget_info(state.tab_id, find_by_uname='RGBASE')
  widget_control, id, set_button=1
  id=widget_info(state.tab_id, find_by_uname='REGRID:M')
  widget_control, id, set_value=state.regrid_values[0]
  id=widget_info(state.tab_id, find_by_uname='REGRID:N')
  widget_control, id, set_value=state.regrid_values[1]        
  id=widget_info(state.tab_id, find_by_uname='DATAGAP:DG')
  widget_control, id, set_value=state.datagap_value
  id=widget_info(state.tab_id, find_by_uname='SST:MASKSET')
  widget_control, id, set_button=state.remove_mask
  id=widget_info(state.tab_id, find_by_uname='SSTBASE')
  widget_control, id, sensitive=state.remove_mask  
  id=widget_info(state.tab_id, find_by_uname='SST:MASKREMOVE')
  widget_control, id, set_value=state.mask_value
  id=widget_info(state.tab_id, find_by_uname='SP:SET')
  widget_control, id, set_button=state.spvalue
  id=widget_info(state.tab_id, find_by_uname='SPBASE')
  widget_control, id, sensitive=state.spvalue
  widget_control, state.sp_combobox, set_combobox_select=0
  id=widget_info(state.tab_id, find_by_uname='LIM:VALUE')
  widget_control, id, set_value=state.limvalue
  widget_control, state.fill_combobox, set_combobox_select=0

  state.statusBar->update, 'Window reset to default settings.'
  state.historyWin->update, 'Window reset to default settings.'
end

; check angle inputs for correct ranges
function thm_ui_part_getspec_angcheck, state

  compile_opt idl2, hidden

  ; reset angle widgets to remove invalid inputs...has to be a better way
  id=widget_info(state.tab_id, find_by_uname='ANGLE:PHI:MIN')
  widget_control, id, set_value=state.angle_values[0, 0]
  id=widget_info(state.tab_id, find_by_uname='ANGLE:PHI:MAX')
  widget_control, id, set_value=state.angle_values[1, 0]
  id=widget_info(state.tab_id, find_by_uname='ANGLE:THETA:MIN')
  widget_control, id, set_value=state.angle_values[0, 1]
  id=widget_info(state.tab_id, find_by_uname='ANGLE:THETA:MAX')
  widget_control, id, set_value=state.angle_values[1, 1]        
  id=widget_info(state.tab_id, find_by_uname='ANGLE:PA:MIN')
  widget_control, id, set_value=state.angle_values[0, 2]
  id=widget_info(state.tab_id, find_by_uname='ANGLE:PA:MAX')
  widget_control, id, set_value=state.angle_values[1, 2]
  id=widget_info(state.tab_id, find_by_uname='ANGLE:GYRO:MIN')
  widget_control, id, set_value=state.angle_values[0, 3]
  id=widget_info(state.tab_id, find_by_uname='ANGLE:GYRO:MAX')
  widget_control, id, set_value=state.angle_values[1, 3]
  id=widget_info(state.tab_id, find_by_uname='ANGLE:START')
  widget_control, id, set_value=state.start_angle_value
  id=widget_info(state.tab_id, find_by_uname='ENERGY:MIN')
  widget_control, id, set_value=state.erange_values[0]
  id=widget_info(state.tab_id, find_by_uname='ENERGY:MAX')
  widget_control, id, set_value=state.erange_values[1]
  id=widget_info(state.tab_id, find_by_uname='REGRID:M')
  widget_control, id, set_value=state.regrid_values[0]
  id=widget_info(state.tab_id, find_by_uname='REGRID:N')
  widget_control, id, set_value=state.regrid_values[1]        
  id=widget_info(state.tab_id, find_by_uname='DATAGAP:DG')
  widget_control, id, set_value=state.datagap_value
  id=widget_info(state.tab_id, find_by_uname='SST:MASKREMOVE')
  widget_control, id, set_value=state.mask_value
  id=widget_info(state.tab_id, find_by_uname='LIM:VALUE')
  widget_control, id, set_value='2.0'


  phi = float(state.angle_values[*, 0])
  theta = float(state.angle_values[*, 1])
  pitch = float(state.angle_values[*, 2])
  gyro = float(state.angle_values[*, 3])

err_mess = ''

 if phi[0] gt phi[1] then $
   err_mess = 'Error with phi angle limits: Min must be less than Max.  '
 
 if (theta[1] lt theta[0]) OR (theta[0] lt -90) OR (theta[1] gt 90) then begin
 
   if theta[1] lt theta[0] then $
     err_mess = err_mess + 'Error with theta angle limits: Min must be less than Max.  '
   
   if theta[0] lt -90 then $
     err_mess = err_mess + 'Error with theta angle limits: Min must be greater than -90.  '
     
   if theta[1] gt 90 then $
     err_mess = err_mess + 'Error with theta angle limits: Max must be less than 90.  '
 endif


 if (pitch[1] lt pitch[0]) OR (pitch[0] lt 0) OR (pitch[1] gt 180) then begin

   if pitch[1] lt pitch[0] then $
     err_mess = err_mess + 'Error with pitch angle limits: Min must be less than Max.  '
   
   if pitch[0] lt 0 then $
     err_mess = err_mess + 'Error with pitch angle limits: Min must be greater than 0.  '
     
   if pitch[1] gt 180 then $
     err_mess = err_mess + 'Error with pitch angle limits: Max must be less than 180.  '
 endif


 if gyro[0] gt gyro[1] then $
   err_mess = err_mess + 'Error with gyro angle limits: Min must be less than Max.  '

return, err_mess
end


;Procedure to control sensitization for regrid widgets
;Regrid options will only be used for:
;-Pitch Angle/Gyro spectra
;-Energy spectra where PA or Gyro is limited or distribution is single-angle
;
;*no easy way to determine if distribution is single angle, so leave 
; sensitized whenever energy spectra are returned
pro thm_ui_part_getspec_rgcheck, state

    compile_opt idl2, hidden

  if state.def_angle EQ 'PA' or state.def_angle EQ 'GYRO' or state.energy_set eq 1 then sens=1 $
    else sens = 0
  
  id=widget_info(state.tab_id, find_by_uname='RGBASE')
  widget_control, id, sensitive=sens

end


;event handler
Pro thm_ui_part_getspec_options_event, event
;Catch here to insure that the state remains defined
;err_xxx = 0
;catch, err_xxx
;If(err_xxx Ne 0) Then Begin
;    catch, /cancel
;Extra Error message
;    txt = ['Error in THM_PART GETSPEC. It is possible that the proper data has not been loaded.', $
;           'THM_PART_GETSPEC Operates on Level 1 SST and ESA data only. For each datatype, (e.g., ', $
;           'peif, peef, psif, psef, etc...) all of the L1 data must be loaded. For ESA data, this ', $
;           'is done automatically when a datatype is chosen from the Load THEMIS Data widget. For ', $
;           'SST you need to click on all L1 variable choices for each given data type. If creating ',$
;           'pitch angle or gyrovelocity spectra, try increasing the pitch/gyro angle ranges, or ', $
;           'increase M and/or N. Otherwise, check to see if all of the appropriate data is loaded.']
;    ok = dialog_message(txt, title='Error in Get Particle Spectra.', /center);,traceback = 1)
;    If(apply) Then Begin
;        Goto, delete_tplot_vars ;back into the 'apply' block to delete tplot vars
;    Endif Else Begin
;        histobj=state.historywin
;        x=state.tab_id
;        If(is_struct(state)) Then $
;          widget_control, event.top, set_uvalue = state, /no_copy
;        thm_gui_error,x,histobj
;        Return                  ;get out
;    Endelse
;Endif

  err_xxx = 0
  Catch, err_xxx
  IF (err_xxx NE 0) THEN BEGIN
    Catch, /Cancel
    Help, /Last_Message, Output = err_msg
    if is_struct(state) then begin
      FOR j = 0, N_Elements(err_msg)-1 DO state.historywin->update,err_msg[j]
      x=state.tab_id
;      if obj_valid(state.insertTree) then begin
;        *state.treeCopyPtr = state.insertTree->getCopy() 
;      endif 
      histobj=state.historywin
    endif
    Print, 'Error--See history'
    ok=error_message('An unknown error occured and the window must be restarted. Check for proper data.',$
       /noname, /center, title='Error in THM_UI_PART_GETSPEC')
    if widget_valid(stash) then begin
      Widget_Control, stash, Set_UValue=state, /No_Copy
    endif
    
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin 
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF

;widget_control, event.top, get_uvalue = state, /no_copy
base = event.handler
stash = widget_info(base,/child)
widget_control, stash, Get_UValue=state, /no_copy

;Options
apply = 0b                      ;local flag for the 'apply' process.
widget_control, event.id, get_uvalue = uval
state.historyWin->Update,'THM_UI_PART_GETSPEC_OPTIONS: User value: '+uval,/dontshow
Case uval of
    'SUFFIX': Begin
        widget_control, event.id, get_value = temp_string
;anything can be a suffix, as long as there are no spaces
        state.suffix = strcompress(/remove_all, temp_string)
        state.statusbar -> update, uval+' = '+state.suffix
    End
    'ANGLE:COMBOBOX': Begin
        temp = strsplit(widget_info(state.angle_combobox, /combobox_gettext),':',/extract)
        state.def_angle = strupcase(strcompress(/remove_all, temp[0]))
        state.statusbar -> update, 'ANGLE = '+state.def_angle
        thm_ui_part_getspec_rgcheck, state
;        id=widget_info(state.tab_id, find_by_uname='RGBASE')
;        if state.def_angle EQ 'PHI' or state.def_angle EQ 'THETA' then sens=0 else sens=1
;        widget_control, id, sensitive=sens
    End
    'ANGLE:PHI:MIN': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            state.angle_values[0, 0] = temp_string
            state.statusbar -> update, uval+' = '+temp_string
        Endif Else begin
;          state.angle_values[0, 0] = '0'
          state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
                     '.  Will be reset to ' + state.angle_values[0, 0] + ' when you hit Apply.'
        endelse
    End
    'ANGLE:PHI:MAX': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            state.angle_values[1, 0] = temp_string
            state.statusbar -> update, uval+' = '+temp_string
        Endif Else begin
;          state.angle_values[1, 0] = '360'
          state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
                     '.  Will be reset to ' + state.angle_values[1, 0] + ' when you hit Apply.'
        endelse
    End
    'ANGLE:THETA:MIN': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            state.angle_values[0, 1] = temp_string
            state.statusbar -> update, uval+' = '+temp_string
        Endif Else begin
;          state.angle_values[0, 1] = '-90'
          state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
                     '.  Will be reset to ' + state.angle_values[0, 1] + ' when you hit Apply.'
        endelse
    End
    'ANGLE:THETA:MAX': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            state.angle_values[1, 1] = temp_string
            state.statusbar -> update, uval+' = '+temp_string
        Endif Else begin
;          state.angle_values[1, 1] = '90'
          state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
                     '.  Will be reset to ' + state.angle_values[1, 1] + ' when you hit Apply.'
        endelse
    End
    'ANGLE:PA:MIN': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            state.angle_values[0, 2] = temp_string
            state.statusbar -> update, uval+' = '+temp_string
        Endif Else begin
;          state.angle_values[0, 2] = '0'
          state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
                     '.  Will be reset to ' + state.angle_values[0, 2] + ' when you hit Apply.'
        endelse
    End
    'ANGLE:PA:MAX': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            state.angle_values[1, 2] = temp_string
            state.statusbar -> update, uval+' = '+temp_string
        Endif Else begin
;          state.angle_values[1, 2] = '180'
          state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
                     '.  Will be reset to ' + state.angle_values[1, 2] + ' when you hit Apply.'
        endelse
    End
    'ANGLE:GYRO:MIN': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            state.angle_values[0, 3] = temp_string
            state.statusbar -> update, uval+' = '+temp_string
        Endif Else begin
;          state.angle_values[0, 3] = '0'
          state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
                     '.  Will be reset to ' + state.angle_values[0, 3] + ' when you hit Apply.'
        endelse
    End
    'ANGLE:GYRO:MAX': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            state.angle_values[1, 3] = temp_string
            state.statusbar -> update, uval+' = '+temp_string
        Endif Else begin
;          state.angle_values[1, 3] = '360'
          state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
                     '.  Will be reset to ' + state.angle_values[1, 3] + ' when you hit Apply.'
        endelse
    End
    'ANGLE:START': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            state.start_angle_value = temp_string
            state.statusbar -> update, uval+' = '+temp_string
        Endif Else begin
;          state.start_angle_value = '0'
          state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
                     '.  Will be reset to ' + state.start_angle_value + ' when you hit Apply.'
        endelse
    End
    'ENERGY:MIN': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            test_value = float(temp_string)
            If(test_value Ge 0) Then Begin
                state.erange_values[0] = temp_string
                state.statusbar -> update, uval+' = '+temp_string
            Endif Else begin
;              state.erange_values[0] = '0'
              state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
                 '.  Will be reset to ' + state.erange_values[0] + ' when you hit Apply.'
            endelse
        Endif Else begin
;          state.erange_values[0] = '0'
          state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
             '.  Will be reset to ' + state.erange_values[0] + ' when you hit Apply.'
        endelse
    End
    'ENERGY:MAX': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            test_value = float(temp_string)
            If(test_value Ge 0) Then Begin
                state.erange_values[1] = temp_string
                state.statusbar -> update, uval+' = '+temp_string
            Endif Else begin
;              state.erange_values[1] = '1e7'
              state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
                 '.  Will be reset to ' + state.erange_values[1] + ' when you hit Apply.'
            endelse
        Endif Else begin
;          state.erange_values[1] = '1e7'
          state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
             '.  Will be reset to ' + state.erange_values[1] + ' when you hit Apply.'
        endelse
    End
    'ENERGY:SET': Begin
        state.energy_set = event.select
        If(event.select Eq 1) Then state.statusbar -> update, 'Return Energy Spectrum Option Set' $
           Else state.statusbar -> update, 'Return Energy Spectrum Option Not Set'
        thm_ui_part_getspec_rgcheck, state
;        id=widget_info(state.tab_id, find_by_uname='RGBASE')
;        if state.energy_set then sens=0 else sens=1
;        widget_control, id, sensitive=sens        
    End
    'NORM:SET': Begin
        state.normalize = event.select
        If(event.select Eq 1) Then state.statusbar -> update, 'Normalize Option Set' $
        Else state.statusbar -> update, 'Normalize Option Not Set'
    End
    'REGRID:M': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            test_value = float(temp_string)
            If(test_value Gt 0) Then Begin
                state.regrid_values[0] = temp_string
                state.statusbar -> update, uval+' = '+temp_string
            Endif Else begin
;              state.regrid_values[0] = '16'
              state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
                 '.  Will be reset to ' + state.regrid_values[0] + ' when you hit Apply.'
            endelse
        Endif Else begin
;          state.regrid_values[0] = '16'
          state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
             '.  Will be reset to ' + state.regrid_values[0] + ' when you hit Apply.
        endelse
    End
    'REGRID:N': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            test_value = float(temp_string)
            If(test_value Gt 0) Then Begin
                state.regrid_values[1] = temp_string
                state.statusbar -> update, uval+' = '+temp_string
            Endif Else begin
;              state.regrid_values[1] = '8'
              state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
                 '.  Will be reset to ' + state.regrid_values[1] + ' when you hit Apply.'
            endelse
        Endif Else begin
;          state.regrid_values[1] = '8'
          state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
             '.  Will be reset to ' + state.regrid_values[1] + ' when you hit Apply.
        endelse
    End
    'OD:COMBOBOX': Begin
        state.def_other_dim = widget_info(state.od_combobox, /combobox_gettext)
        state.statusbar -> update, 'OTHER_DIM = '+state.def_other_dim
    End
    'DATAGAP:DG': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            test_value = float(temp_string)
            If(test_value Ge 0) Then Begin
                state.datagap_value = temp_string
                state.statusbar -> update, uval+' = '+temp_string
            Endif Else begin
;              state.datagap_value = '0'
              state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
                 '.  Will be reset to ' + state.datagap_value + ' when you hit Apply.'
            endelse
        Endif Else begin
;          state.datagap_value = '0'
          state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
             '.  Will be reset to ' + state.datagap_value + ' when you hit Apply.'
        endelse
    End
    'CLEARDATA': Begin
       widget_control, state.dataTypeList, set_list_select=-1
    End
    'CLEARPROBE': Begin
       widget_control, state.probeList , set_list_select=-1
    End
    'SST:MASKSET': Begin
        state.remove_mask = event.select
        If(event.select Eq 1) Then state.statusbar -> update, 'Remove Mask Option Set' $
        Else state.statusbar -> update, 'Remove Mask Option Not Set'
        id=widget_info(state.tab_id, find_by_uname='SSTBASE')
        widget_control, id, sensitive=state.remove_mask
    End
    'SST:MASKREMOVE': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            test_value = float(temp_string)
            If(test_value Ge 0 And test_value Le 1.0) Then Begin
                state.mask_value = temp_string
                state.statusbar -> update, uval+' = '+temp_string
            Endif Else begin
;              state.mask_value = '0.99'
              state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
                 '.  Will be reset to ' + state.mask_value + ' when you hit Apply.'
            endelse
        Endif Else begin
;          state.mask_value = '0.99'
          state.statusbar -> update, uval+' Invalid Input: '+temp_string+ $
             '.  Will be reset to ' + state.mask_value + ' when you hit Apply.'
        endelse
    End
    'SP:SET': Begin
        state.spvalue = event.select
        If(event.select Eq 1) Then state.statusbar -> update, 'SunPulse Removal Option Set' $
        Else state.statusbar -> update, 'SunPulse Removal Option Not Set'
        id=widget_info(state.tab_id, find_by_uname='SPBASE')
        widget_control, id, sensitive=state.spvalue
        
    End
    'SP:COMBOBOX': Begin
        state.def_sp = widget_info(state.sp_combobox, /combobox_gettext)
        state.statusbar -> update, 'SUNPULSE REMOVAL METHOD= '+state.def_sp
    End
    'LIM:VALUE': Begin
        widget_control, event.id, get_value = temp_string
        If(is_numeric(temp_string)) Then Begin
            test_value = float(temp_string)
            If(test_value Ge 0) Then begin 
              state.limvalue = temp_string
              state.statusbar -> update, uval+' = '+temp_string
            Endif Else begin
;              state.limvalue = '2.0'
              state.statusbar -> update, 'Invalid Input'+ $
                 '.  Will be reset to ' + state.limvalue + ' when you hit Apply.'
            endelse
        Endif Else begin
;          state.limvalue = '2.0'
          state.statusbar -> update, 'Invalid Input'+ $
             '.  Will be reset to ' + state.limvalue + ' when you hit Apply.'
        endelse
    End
    'FILL:COMBOBOX': Begin
        state.def_fill = widget_info(state.fill_combobox, /combobox_gettext)
        state.statusbar -> update, 'FILLIN_METHOD = '+state.def_fill
    End
    'HELP':BEGIN
      gethelppath,path
      xdisplayfile, path+'thm_ui_part_getspec_options.txt', group=state.tab_id, /modal, done_button='Done', $
                    title='HELP: Getspec Options'
    END
    'RESET':BEGIN
      thm_ui_part_getspec_options_reset, state
    END
    'APPLY':Begin

      apply = 1b              ;this is here to be checked if the catch is triggered

      widget_control, /hourglass

;Check angle inputs for any errors
      ang_err_mess = thm_ui_part_getspec_angcheck(state)
      if ~array_equal(ang_err_mess, '') then begin
        state.statusbar->Update, ang_err_mess
        state.historyWin->Update, ang_err_mess
        widget_control, stash, set_uvalue = state, /no_copy
        return
      endif

      probe_idx = widget_info(state.probeList,/list_select)
      
      if probe_idx[0] eq -1 then begin
        state.statusbar->update, 'Please select a probe.'
        break
      endif
      
      if in_set(probe_idx,0) then begin
        probe = state.validProbes[1:n_elements(state.validProbes)-1]
      endif else begin
        probe = state.validProbes[probe_idx]
      endelse
      
      data_idx = widget_info(state.dataTypeList,/list_select)
      
      if data_idx[0] eq -1 then begin
        state.statusbar->update, 'Please select a datatype.'
        break
      endif
      
      if in_set(data_idx,0) then begin
        dtype = state.validDatatypes[1:n_elements(state.validDatatypes)-1]
      endif else begin
        dtype = state.validDatatypes[data_idx]
      endelse
  
; setup array of requested probe + data types
      for i=0,n_elements(probe)-1 do begin
         for j=0,n_elements(dtype)-1 do begin
            IF n_elements(dtype_arr) EQ 0 then dtype_arr = ['th' + probe[i] + '_' + dtype[j]] $
              ELSE dtype_arr = [dtype_arr, 'th' + probe[i] + '_' + dtype[j]]
         endfor
      endfor
      
        state.tr->getproperty, starttime=start_obj
        state.tr->getproperty, endtime=stop_obj
        start_obj->getProperty,tstring=startText
        stop_obj->getProperty,tstring=stopText
        trange = time_double([startText, stopText])
        
;Check that start and end times are not out of order/equal
        if trange[0] ge trange[1] then begin
          state.statusbar->update, 'Error: End time less than/equal to Start time.'
          widget_control, stash, set_uvalue = state, /no_copy
          return
        endif
        
;Check that start and end times are not in the future
        if (trange[0] gt systime(/sec)) AND (trange[1] gt systime(/sec)) then begin
          state.statusbar->Update, "Error: Start and end times are later than today's date. "
          widget_control, stash, set_uvalue = state, /no_copy
          return
        endif
        
        x = state.loadeddata
;        h = state.historywin

;Find active data, get datatypes, run thm_part_getspec separately for each uniq
;probe_datatype combination
        vars=dtype_arr
        sv = strmid(vars, 0, 8)
        usv = uniq(sv)
        sv = sv[usv]
;define these variables outside of the loop, for easy testing, and record all
;choices in the history window.
        history_arr = ['INPUT PARAMETERS FOR PART_GETSPEC:', $
                       'TIME RANGE: '+startText+', '+stopText, $
                       'START ANGLE: '+state.start_angle_value, $
                       'SUFFIX: '+state.suffix, 'ANGLE: '+state.def_angle, $
                       'ENERGY RANGE: '+state.erange_values[0]+', '+state.erange_values[1], $
                       'ENERGY SPECTROGRAM OPTION: '+strcompress(string(state.energy_set)), $
                       'REGRID VALUES: '+state.regrid_values[0]+', '+state.regrid_values[1], $
                       'OTHER_DIM: '+state.def_other_dim, $
                       'NORMALIZE OPTION: '+strcompress(string(state.normalize)), $
                       'DATAGAP VALUE: '+state.datagap_value, $
                       'SST MASK REMOVAL OPTION: '+strcompress(string(state.remove_mask)), $
                       'SST MASK VALUE: '+state.mask_value, $
                       'SST SUNPULSE REMOVAL OPTION: '+strcompress(string(state.spvalue)), $
                       'SST SUNPULSE REMOVAL METHOD: '+state.def_sp, $
                       'SST SUNPULSE REMOVAL FILLIN METHOD: '+state.def_fill, $
                       'SST SUNPULSE REMOVAL TOLERANCE: '+state.limvalue]
        For j = 0, 3 Do history_arr = [history_arr, state.angle_options[j]+' LIMITS:'+$
                                       state.angle_values[0, j]+', '+state.angle_values[1, j]]

        ; reset angle widgets to remove invalid inputs...has to be a better way
        id=widget_info(state.tab_id, find_by_uname='ANGLE:PHI:MIN')
        widget_control, id, set_value=state.angle_values[0, 0]
        id=widget_info(state.tab_id, find_by_uname='ANGLE:PHI:MAX')
        widget_control, id, set_value=state.angle_values[1, 0]
        id=widget_info(state.tab_id, find_by_uname='ANGLE:THETA:MIN')
        widget_control, id, set_value=state.angle_values[0, 1]
        id=widget_info(state.tab_id, find_by_uname='ANGLE:THETA:MAX')
        widget_control, id, set_value=state.angle_values[1, 1]        
        id=widget_info(state.tab_id, find_by_uname='ANGLE:PA:MIN')
        widget_control, id, set_value=state.angle_values[0, 2]
        id=widget_info(state.tab_id, find_by_uname='ANGLE:PA:MAX')
        widget_control, id, set_value=state.angle_values[1, 2]
        id=widget_info(state.tab_id, find_by_uname='ANGLE:GYRO:MIN')
        widget_control, id, set_value=state.angle_values[0, 3]
        id=widget_info(state.tab_id, find_by_uname='ANGLE:GYRO:MAX')
        widget_control, id, set_value=state.angle_values[1, 3]
        id=widget_info(state.tab_id, find_by_uname='ANGLE:START')
        widget_control, id, set_value=state.start_angle_value
        id=widget_info(state.tab_id, find_by_uname='ENERGY:MIN')
        widget_control, id, set_value=state.erange_values[0]
        id=widget_info(state.tab_id, find_by_uname='ENERGY:MAX')
        widget_control, id, set_value=state.erange_values[1]
        id=widget_info(state.tab_id, find_by_uname='REGRID:M')
        widget_control, id, set_value=state.regrid_values[0]
        id=widget_info(state.tab_id, find_by_uname='REGRID:N')
        widget_control, id, set_value=state.regrid_values[1]        
        id=widget_info(state.tab_id, find_by_uname='DATAGAP:DG')
        widget_control, id, set_value=state.datagap_value
        id=widget_info(state.tab_id, find_by_uname='SST:MASKREMOVE')
        widget_control, id, set_value=state.mask_value
        id=widget_info(state.tab_id, find_by_uname='LIM:VALUE')
        widget_control, id, set_value=state.limvalue


        state.historyWin -> update, history_arr
        trange = time_double([startText, stopText])
        start_angle = float(state.start_angle_value)
        suffix = state.suffix
        angle = strlowcase(state.def_angle)
        phi = float(state.angle_values[*, 0])
        theta = float(state.angle_values[*, 1])
        pitch = float(state.angle_values[*, 2])
        gyro = float(state.angle_values[*, 3])
        erange = float(state.erange_values)
        energy = state.energy_set
        regrid = long(state.regrid_values)
        other_dim = strlowcase(state.def_other_dim)
        normalize = state.normalize
        datagap = float(state.datagap_value)
;Keep track of new created variables
        new_vars = ''
;Keep track of tplot data here
        tnames_in = tnames()
        For j = 0, n_elements(sv)-1 Do Begin
;thm_part_getspec accepts 'probe' and 'data_type' keywords,
;assuming that there aren't non-standard variable names
            probe = strmid(sv[j], 2, 1)
            dtype = strmid(sv[j], 4, 4)
            state.historyWin-> update, 'THM_PART_GETSPEC: Processing Probe: '+probe+',  Datatype: '+dtype
            state.statusbar -> update, 'Processing Probe: '+probe+',  Datatype: '+dtype
;SST masking is an issue
            If(strmid(dtype, 1, 1) Eq 's') Then Begin
                If(state.remove_mask Eq 0) Then mask_remove = 0 $
                Else mask_remove = float(state.mask_value)
;Sun pulse cleanup?
                If(state.spvalue Eq 0) Then Begin
                    method_sunpulse_clean = 0
                    limit_sunpulse_clean = 0
                Endif Else Begin
                    method_sunpulse_clean = strlowcase(state.def_sp)
                    limit_sunpulse_clean = float(state.limvalue)
                Endelse
                fillin_method = strlowcase(state.def_fill)
            Endif Else Begin
                mask_remove = 0
                method_sunpulse_clean = 0
                limit_sunpulse_clean = 0
                fillin_method = 0
            Endelse
            tn_before = tnames('*',create_time=cn)
            thm_part_getspec, probe = probe, data_type = dtype, trange = trange, $
              start_angle = start_angle, suffix = suffix, angle = angle, $
              phi = phi, theta = theta, pitch = pitch, gyro = gyro, $
              erange = erange, energy = energy, regrid = regrid, $
              other_dim = other_dim, normalize = normalize, datagap = datagap, $
              mask_remove = mask_remove, method_sunpulse_clean = method_sunpulse_clean, $
              limit_sunpulse_clean = limit_sunpulse_clean, fillin_method = fillin_method, $
              /gui_flag, gui_statusBar=state.statusBar, gui_historyWin=state.historyWin
;Now find the new variables

            thm_ui_cleanup_tplot,tn_before,create_time_before=cn,new_vars=found_vars,del_vars=to_delete

            call_added = 0

            new_spec = ssl_set_intersection([sv[j]+'_an_eflux'+'_'+angle+suffix],[found_vars])
            If(is_string(new_spec) && new_spec[0] ne '') Then Begin
              success = x->add(new_spec)
              If(success) Then Begin
                call_added = 1
                state.callSequence->addGetSpecOp,probe,dtype,trange,$
                  start_angle,suffix,angle,phi,theta,pitch,gyro,erange,$
                  energy,regrid,other_dim,normalize,datagap,mask_remove,method_sunpulse_clean,$
                  limit_sunpulse_clean,fillin_method
                state.statusBar->update, 'Added Variable: '+new_spec
                state.historyWin->update, 'Added Variable: '+new_spec
                new_vars = [new_vars, new_spec]
              Endif Else Begin
                state.statusBar->update, 'Failed to Add Variable: '+new_spec+"  What's the deal with that?"
                state.historyWin->update, 'Failed to Add Variable: '+new_spec+"  What's the deal with that?"
              Endelse
            Endif
            If(energy Gt 0) Then Begin
              new_espec = ssl_set_intersection([sv[j]+'_en_eflux'+suffix],[found_vars])
              If(is_string(new_espec) && new_espec[0] ne '') Then Begin
                success = x->add(new_espec)
                If(success) Then Begin
                  if ~keyword_set(call_added) then begin
                      state.callSequence->addGetSpecOp,probe,dtype,trange,$
                        start_angle,suffix,angle,phi,theta,pitch,gyro,erange,$
                        energy,regrid,other_dim,normalize,datagap,mask_remove,method_sunpulse_clean,$
                        limit_sunpulse_clean,fillin_method
                  endif
                  state.statusBar->update, 'Added Variable: '+new_espec
                  state.historyWin->update, 'Added Variable: '+new_espec
                  new_vars = [new_vars, new_espec]
                Endif Else Begin
                  state.statusBar->update, 'Failed to Add Variable: '+new_espec+"  What's the deal with that?"
                  state.historyWin->update, 'Failed to Add Variable: '+new_espec+"  What's the deal with that?"
                Endelse
              Endif
            Endif
            
            if to_delete[0] ne '' then begin
              store_data,to_delete,/delete
            endif
            
        Endfor

      
        ;update status bar and history window
        state.statusBar->update, 'Getspec load finished.'
        state.historyWin->update, 'Getspec load finished.'
;All done
    End
  ELSE:
Endcase
widget_control, stash, set_uvalue=state, /NO_COPY

Return
End


;widget set up
Pro thm_ui_part_getspec_options, tab_id, loadedData, historyWin, statusBar, $
                                 trObj,callSequence,timeWidget=timeid

widget_control, /hourglass

mainBase = widget_base(tab_id, /col)
  topBase = widget_base(mainBase, /row)
     topCol1Base =  widget_base(topBase, /col)
       instrLabelBase = widget_base(topcol1base, /row)
       instrumentBase = widget_base(topCol1Base, /col, /frame, xpad=5)
         timeBase = widget_base(instrumentBase)
         dataBase = widget_base(instrumentBase, /row)
       suffixLabelBase = widget_base(topCol1Base,/col)
       suffixBase = widget_base(topCol1Base, /row, /frame, ypad=1, xpad=10)
     topCol2Base = widget_base(topBase, /col)
        energyLabelBase = widget_base(topCol2Base, /row, /align_left)
        energy_base = widget_base(topCol2Base, /col, /frame, /align_left, ypad=10, xpad=8, tab_mode=1) ;base widget for energy info  
        angleLabelBase = widget_base(topCol2Base, /row, /align_left)
        angle_Base = widget_base(topCol2Base, /col, /frame, ypad=15, xpad=5, tab_mode=1)
     topCol3Base =  widget_base(topBase, /col)
      advancedLabelBase = widget_base(topCol3Base, /row) 
      advanced_base = widget_base(topCol3Base, /col, /frame, ypad=1, xpad=5, tab_mode=1)    
  buttonBase = widget_base(mainBase, /row, /align_center)
  
  ;data input options
  dataLabel = widget_label(instrLabelBase, value='Data Selection: ', /align_left)
  ; Time widget
  tr = trObj
  timeid = thm_ui_time_widget(timeBase,$
                              statusBar,$
                              historyWin,$
                              timeRangeObj=tr,$
                              uvalue='TIME_WIDGET',$
                              uname='time_widget')

  ; Probe selection
  validProbesVisible = [' * (All)', ' A (P5)', ' B (P1)', ' C (P2)', ' D (P3)', $
                 ' E (P4)']
  validProbes = ['*', 'a', 'b', 'c', 'd', $
                 'e']
  probeBase = Widget_Base(dataBase, /col)
  plBase = Widget_base(probeBase, /row)
  probeListBase = Widget_Base(plBase, /col)
  probeLabel = Widget_Label(probeListBase, Value='Probe: ', /align_left)
  probeList = Widget_List(probeListBase, Value=validProbesVisible, /multiple, uval='PROBE', XSize=16, YSize=11)
  probeButtonBase = Widget_Base(probeListBase, /row, /align_center)
  probeClearButton = Widget_Button(probeButtonBase, value=' Clear Probe ', uvalue='CLEARPROBE', /align_center)

  ; Data type selection
  ; Get valid datatypes, probes, etc for different data types  
  dlist1 = thm_ui_new_valid_datatype('ESA', ilist, ['l1'])
  dlist1 = dlist1[0:5]
  validDataTypes = ['*', dlist1]
  dlist2 = thm_ui_new_valid_datatype('SST', ilist, ['l1'])
  ;validDataTypes = [validDataTypes, dlist2]
  validDataTypes = ['*','peif', 'peir', 'peib', 'peef', 'peer', 'peeb', 'psif', 'psir', 'psib', $
    'psef', 'pser', 'pseb'] 
 ; dtype_ind = make_array(n_elements(validDataTypes)-1, /int)
  
  dataTypeBase = Widget_Base(DataBase, /col)
  dataL1Base = Widget_Base(dataTypeBase, /col)
  dataButtonBase = Widget_Base(dataTypeBase, /col, /align_center)
  dataTypeLabel = Widget_Label(dataL1Base, Value='Data Type:', /align_left)
  dataTypeList = Widget_List(dataL1Base, Value=validDataTypes, uval='DATATYPE', $
                         /Multiple, XSize=16, YSize=11)
  dataClearButton = Widget_Button(dataButtonBase, value=' Clear Data Type ', uvalue='CLEARDATA', /align_center)

  ;suffix
  suffixlabel = widget_label(suffixLabelBase, value = 'Variable Name: ', /align_left)
  suffixlabel = widget_label(suffixBase, value = 'New suffix: ', /align_left)
  suffix = ''
  suffixid = widget_text(suffixBase, value = suffix, xsiz = 22, ysiz = 1, $
                         uvalue = 'SUFFIX', uname='SUFFIX', /editable, /all_events)

  ;Buttons for energy spectra
  erange_values = ['0', '1e7']
  erbase = widget_base(energy_base, /col)
  erlabel = widget_label(energyLabelbase, value = 'Energy Range:')
  erminmaxbase = widget_base(erbase, /row)
  erminbase = widget_base(erminmaxbase, /row)
  erminlabel = widget_label(erminbase, value = 'Min (eV): ')
  ermin = widget_text(erminbase, value = erange_values[0], xsiz = 6, ysiz = 1, $
                      uvalue = 'ENERGY:MIN', uname='ENERGY:MIN', /editable, /all_events)
  ermaxbase = widget_base(erminmaxbase, /row)
  ermaxlabel = widget_label(ermaxbase, value = 'Max (eV):')
  ermax = widget_text(ermaxbase, value = erange_values[1], xsiz = 6, ysiz = 1, $
                      uvalue = 'ENERGY:MAX', uname='ENERGY:MAX', /editable, /all_events)
  energy_set = 0b
  ebuttonbase = widget_base(energy_base, /col, /nonexclusive, xpad=5)
  ebutton = widget_button(ebuttonbase, value = 'Return Energy Spectrum', uvalue = 'ENERGY:SET', $
                          uname = 'ENERGY:SET', $
                          tooltip='Output tplot variables containing the energy spectrum of ' + $ 
                          'the energy/angle ranges input above')

  ;Angle stuff
  alabel = widget_label(angleLabelBase, value = 'Angular distribution:')
  angle_options = ['PHI', 'THETA', 'PA', 'GYRO']
  def_angle = 'PHI'
  angle_description = ['probe-sun dir in spin plane (deg)', $
                       'spin plane/az of pitch ang (deg)', $
                       'pitch angle, magnetic field (deg)', $
                       'gyrovelocity']        ;need to figure out gyrovelocity
  angle_combobox = widget_combobox(angle_base, value = angle_options+': '+$
                                   angle_description, uvalue = 'ANGLE:COMBOBOX')
  
  nangles = n_elements(angle_options)
  angle_values = strarr(2, nangles)
  angle_values[*, 0] = ['0', '360']
  angle_values[*, 1] = ['-90', '90']
  angle_values[*, 2] = ['0', '180']
  angle_values[*, 3] = ['0', '360']
  
  angle_values_base = widget_base(angle_base, /col)
  angle_values_label = widget_label(angle_values_base, value = 'Angular Limits')
  For j = 0, nangles-1 Do Begin
      uvalj = 'ANGLE:'+angle_options[j]
      basej = widget_base(angle_values_base, /row)
      basejlabel = widget_label(basej, value = angle_options[j], xsiz = 60)
      basejminbase = widget_base(basej, /row)
      basejminlabel = widget_label(basejminbase, value = 'Min')
      basejmin = widget_text(basejminbase, value = angle_values[0, j], xsiz = 6, ysiz = 1, $
                             uvalue = uvalj+':MIN', uname = uvalj+':MIN', /editable, /all_events)
      basejmaxbase = widget_base(basej, /row)
      basejmaxlabel = widget_label(basejmaxbase, value = 'Max')
      basejmax = widget_text(basejmaxbase, value = angle_values[1, j], xsiz = 6, ysiz = 1, $
                             uvalue = uvalj+':MAX', uname = uvalj+':MAX', /editable, /all_events)
  Endfor
  
  start_angle_value = '0'
  start_angle_base = widget_base(angle_values_base, /row)
  start_angle_label = widget_label(start_angle_base, value = 'Start Angle (Phi only)', xsiz = 150)
  start_angle_id = widget_text(start_angle_base, value = start_angle_value, xsiz = 6, ysiz = 1, $
                               uvalue = 'ANGLE:START', uname='ANGLE:START', /editable, /all_events)

  ;advanced stuff
  advancedLabel = widget_label(advancedLabelBase, value='Advanced: ')
  
  rgbase = widget_base(advanced_base, /row, frame=3, uname='RGBASE')
  rglabelbase = widget_base(rgbase, /col)
  rglabel1 = widget_label(rglabelbase, /align_left, value = 'Regrid=[m,n] for [phi, theta]')
  regrid_values = ['16', '8']
  rgmnbase = widget_base(rgLabelbase, /row)
  rgmbase = widget_base(rgmnbase, /row)
  rgmlabel = widget_label(rgmbase, value = 'M ')
  rgm = widget_text(rgmbase, value = regrid_values[0], xsiz = 6, ysiz = 1, $
                    uvalue = 'REGRID:M', uname='REGRID:M', /editable, /all_events)
  rgnbase = widget_base(rgmnbase, /row)
  rgnlabel = widget_label(rgnbase, value = 'N ')
  rgn = widget_text(rgnbase, value = regrid_values[1], xsiz = 6, ysiz = 1, $
                    uvalue = 'REGRID:N', uname='REGRID:N', /editable, /all_events)

  normbase = widget_base(advanced_base, /col, /nonexclusive)
  normalize = 0b
  nbutton = widget_button(normbase, value = 'Normalize Spectrum', uvalue = 'NORM:SET', $
                          uname='NORM:SET', $
                          tooltip='Normalize the flux for each time sample to 0-1')

  odbase = widget_base(advanced_base, /row)
  odlabelbase = widget_base(odbase, /col)
  odlabel1 = widget_label(odlabelbase, /align_left, value = 'Conversion to FAC')
  od_options = ['MPHIGEO',  'PHIGEO', 'MPHISM', 'PHISM', 'MRGEO', 'RGEO', 'XGSE', 'YGSM']
  def_other_dim = 'MPHIGEO'
  od_combobox = widget_combobox(odbase, value = od_options, uvalue = 'OD:COMBOBOX')
  
  dgbase = widget_base(advanced_base, /row)
  dglabelbase = widget_base(dgbase, /col)
  dglabel1 = widget_label(dglabelbase, /align_left, value = 'Maximum gap, sec. ')
  datagap_value = '0'
  dgvaluebase = widget_base(dgbase, /row)
  dgvaluelabel = widget_label(dgvaluebase, value = 'GAP')
  dg = widget_text(dgvaluebase, xsiz = 8, ysiz = 1, value = datagap_value, $
                   uvalue = 'DATAGAP:DG', uname = 'DATAGAP:DG', /editable, /all_events)
  
  ;SST stuff
  advanced_base_sst = widget_base(advanced_base, sensitive=1, /col)
  maskbase = widget_base(advanced_base_sst, /col)
  maskbuttonbase = widget_base(maskbase, /col, /nonexclusive)
  remove_mask = 0b
  maskbutton = widget_button(maskbuttonbase, value = 'Remove SST mask', uvalue = 'SST:MASKSET', $
                             uname='SST:MASKSET', tooltip='Remove on-board SST mask')
  maskvaluebase = widget_base(maskbase, /row, sensitive=0, uname='SSTBASE', frame=3)
  maskvaluelabelbase = widget_base(maskvaluebase, /col)
  maskvaluelabel1 = widget_label(maskvaluelabelbase, /align_left, value = 'Mask_remove proportion')
  maskvaluelabel2 = widget_label(maskvaluelabelbase, /align_left, value = 'of values that must be zero')
  mask_value = '0.99'
  maskremove = widget_text(maskvaluebase, value = mask_value, xsiz = 6, ysiz = 1, $
                           uvalue = 'SST:MASKREMOVE', uname = 'SST:MASKREMOVE', /editable, $
                           /all_events)
  
  spbase = widget_base(advanced_base_sst, /col)
  spbuttonbase = widget_base(spbase, /col, /nonexclusive)
  spbutton = widget_button(spbuttonbase, value = 'Remove SST Sun Contamination', uvalue = 'SP:SET', $
                           uname='SP:SET', tooltip='Removes sunpulse contamination of SST data')
  spvalue = 0b
  spbase0 = widget_base(spbase, /col, sensitive=0, uname='SPBASE', frame=3)
  spbase1 = widget_base(spbase0, /row)
  splabelbase = widget_base(spbase1, /col)
  splabel1 = widget_label(splabelbase, value = 'Sunpulse cleaning method ')
  sp_options = ['SPIN_FIT', 'MEDIAN']
  def_sp = 'SPIN_FIT'
  sp_combobox = widget_combobox(spbase1, value = sp_options, uvalue = 'SP:COMBOBOX')
  limbase = widget_base(spbase0, /row)
  limlabelbase = widget_base(limbase, /col)
  limlabel1 = widget_label(limlabelbase, value = 'SDT for sunpulse cleaning')
  limvalue = '2.0'
  limtxt = widget_text(limbase, value = limvalue, xsiz = 6, ysiz = 1, $
                       uvalue = 'LIM:VALUE', uname = 'LIM:VALUE', /editable, /all_events)
  fillbase = widget_base(spbase0, /row)
  filllabelbase = widget_base(fillbase, /col)
  filllabel1 = widget_label(filllabelbase, value = 'Fill method')
  def_fill = 'INTERPOLATION'
  fill_options = ['INTERPOLATION', 'SPIN_FIT']
  fill_combobox = widget_combobox(fillbase, value = fill_options, uvalue = 'FILL:COMBOBOX')
  
  ;buttons
  apply_button = Widget_Button(buttonBase, Value = '    Apply    ', XSize = 70, $
                               UValue = 'APPLY', tooltip='Create particle spectra from active data')
  resetButton = Widget_Button(buttonBase, value='Reset All', uvalue='RESET', $
                              tooltip='Reset all settings to their default value.')
  helpButton = Widget_Button(buttonBase, Value='Help', XSize=70, UValue='HELP', $
                              tooltip='Open Help Window.')

state = {tab_id:tab_id, loadedData:loadedData, historyWin:historyWin, $
         statusBar:statusBar, probeList:probeList, $
         angle_combobox:angle_combobox, dataTypeList:dataTypeList, $
         def_angle:def_angle, angle_options:angle_options, angle_values:angle_values, $
         energy_set:energy_set, erange_values:erange_values, start_angle_value:start_angle_value, $
         tr:tr, probeClearButton:probeClearButton, dataClearButton:dataClearButton, $
         regrid_values:regrid_values, od_combobox:od_combobox, $
         def_other_dim:def_other_dim, normalize:normalize, datagap_value:datagap_value, $
         remove_mask:remove_mask, mask_value:mask_value, spvalue:spvalue, sp_combobox:sp_combobox, $
         def_sp:def_sp, limvalue:limvalue, fill_combobox:fill_combobox, def_fill:def_fill, $
         suffix:suffix, validProbes:validProbes, validDataTypes:validDataTypes, $
         advanced_base_sst:advanced_base_sst, maskValueBase:maskValueBase, maskButton:maskButton, $
         spButton:spButton, spBase0:spBase0,callSequence:callSequence}

thm_ui_part_getspec_rgcheck, state

widget_control, widget_info(tab_id, /child), set_uvalue=state, /no_copy

Return
End
