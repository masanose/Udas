;+ 
;NAME:
; thm_ui_new_dproc
;PURPOSE:
; handles data processing requests
;CALLING SEQUENCE:
; thm_ui_new_dproc, info, uval
;INPUT:
; info = the info structure for the calling widget, the loadeddata
;        object, statusbar object and historywin object need to have
;        been initialized.
; uval = the string value for the data processing task that is to be
;        done. Good values are:  ['subavg', 'submed', 'smooth', 'blkavg',
;        'clip','deflag','degap','spike','deriv','pwrspc','wave',
;        'hpfilt']
;KEYWORDS:
; out_string = an output string for display in history, status
; ext_statusbar = the default is to output messages to the main GUI
;                 statusbar. If ext_statusbar is a valid object, then
;                 updates go here
; group_leader = the calling widget id, the default is to use the main
;                GUI
; ptree = pointer to copy data tree
;
;OUTPUT:
; Returns 1 for successful output, 0 for unsuccessful, otherwise, 
; tasks are preformed, active data are updated, messages are updated.
; 
;NOTES:
;  If you add any operations,  be sure to put code in place
;  so that we can recall the operation when a themis document is loaded without data.
;
;
;HISTORY:
; 20-oct-2008, jmm, jimm@ssl.berkeley.edu
;$LastChangedBy: pcruce $
;$LastChangedDate: 2010-03-03 13:23:25 -0800 (Wed, 03 Mar 2010) $
;$LastChangedRevision: 7395 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_new_dproc.pro $


;This routine simplifies some of the recall code
function call_dproc, dp_task, dp_pars, names_out = names_out, $
                     out_string = out_string, no_setactive = no_setactive, $
                     hwin = hwin, sbar = sbar, call_sequence=call_sequence,$
                     loadedData=loadedData,gui_id = gui_id, dpr_extra = _extra

  compile_opt idl2,hidden

  ;First get the active data names:
  in_vars = loadedData->getactive(/parent) ;all you want is parents
  If(is_string(in_vars) Eq 0) Then Begin
      out_string = 'No active data, Returning'
      Return, otp
  Endif
  
  otp = loadedData->dproc(dp_task, dp_pars, names_out = names_out, in_vars=in_vars, $
                      out_string = out_string, no_setactive = no_setactive, $
                      hwin = hwin, sbar = sbar, gui_id = gui_id)
  
  if otp then begin
    if n_elements(dp_pars) gt 0 then begin
      call_sequence->addDprocOp,dp_task,in_vars,params=dp_pars
    endif else begin
      call_sequence->addDprocOp,dp_task,in_vars
    endelse
  endif
  
  return,otp

end


Function thm_ui_new_dproc, info, uval, out_string = out_string, $
                           group_leader = group_leader, ext_statusbar = ext_statusbar, $
                           ptree = ptree


;Initialize output
  otp = 0b
  out_string = ''

  err0 = 0
  catch, err0
  If(err0 Ne 0) Then Begin
    catch, /cancel
    ok = error_message(traceback = 1, /noname, title = 'Error in Data Processing: ')
    Return, 0b
  Endif

;Fall hard if the info structure doesn't exist here
  If(is_struct(info) Eq 0) Then message, 'Invalid info structure?'

  uv0 = ['subavg', 'submed', 'smooth', 'blkavg', 'clip', 'deflag', $
         'degap', 'spike', 'deriv', 'pwrspc', 'wave', 'hpfilt', $
         'interpol']
  uvlong = ['Average subtraction', 'Median subtraction', $
            'Time smoothing', 'Block average', 'Clip', 'Deflag', $
            'Degap', 'De-spike', 'Time derivative', $
            'Dynamic Power spectrum', 'Wavelet transform', $
            'High-pass filter', 'Interpolate']

  If(obj_valid(ext_statusbar)) Then sbar = ext_statusbar $
  Else sbar = info.statusbar
;This is not likely to happen,
  If(is_string(uval) Eq 0) Then Begin
    out_string = 'THM_UI_NEW_DPROC: No user value.'
    sbar -> update, out_string
    info.historywin -> update, out_string
    Return, otp
  Endif

  uv = strcompress(/remove_all, strlowcase(uval))

;This is not likely to happen, either
  is_possible = where(uv0 Eq uv)
  If(is_possible[0] Eq -1) Then Begin
    out_string = 'THM_UI_NEW_DPROC: Invalid user value: '+uval
    sbar -> update, out_string
    info.historywin -> update, out_string
    Return, otp
  Endif

  info.windowStorage->getProperty,callSequence=call_sequence

  ;Get active data
  active_data = info.loadedData->getactive(/parent)

  if ~is_string(active_data[0]) then begin
    message = 'No active data.  Returning to Data Processing window.'
    sBar->update, message
    info.historyWin->update, 'THM_UI_NEW_PWRSPC: ' + message
    return,''
  endif

  If(keyword_set(group_leader)) Then guiid = group_leader[0] Else guiid = info.master
;Long case statement
  Case uv Of
    'subavg': otp = call_dproc(uv, out_string = out_string,hwin=info.historyWin,sbar=info.statusBar,call_sequence=call_sequence,loadedData=info.loadedData, gui_id=guiid)
    'submed': otp = call_dproc(uv, out_string = out_string,hwin=info.historyWin,sbar=info.statusBar,call_sequence=call_sequence,loadedData=info.loadedData, gui_id=guiid)
    'deriv': begin
      values = thm_ui_time_derivative_options(guiid, sbar, info.historywin)
      if values.ok then begin
        otp = call_dproc(uv, values, out_string = out_string,hwin=info.historyWin,sbar=info.statusBar,call_sequence=call_sequence,loadedData=info.loadedData, gui_id=guiid)
      endif else sbar->update,'Time derivative canceled'
    end
    'spike': begin
      values = thm_ui_clean_spikes_options(guiid, sbar, info.historywin)
      if values.ok then begin
        otp = call_dproc(uv, values, out_string = out_string,hwin=info.historyWin,sbar=info.statusBar,call_sequence=call_sequence,loadedData=info.loadedData, gui_id=guiid)
      endif else sbar->update,'Clean Spikes canceled'
    end
    'pwrspc': Begin
      popt = thm_ui_pwrspc_options(guiid, info.loadtr, info.historywin, sbar)
      if popt.success eq 0 then return,''
    
      ;Get active data
      active_data = info.loadedData->getactive(/parent)
    
      if ~is_string(active_data[0]) then begin
        message = 'No active data.  Returning to Data Processing window.'
        sBar->update, message
        info.historyWin->update, 'THM_UI_NEW_PWRSPC: ' + message
        return,''
      endif

      thm_ui_new_pwrspc, popt,active_data, info.loadedData, info.historywin, sbar, $
                         out_string=out_string,fail=fail
                         
      if ~keyword_set(fail) then begin
        call_sequence->addPwrSpecOp,popt,active_data
      endif
    End
    'smooth': Begin
;      opar = obj_new('thm_ui_dproc_par', dp_string = uv, $
;                     dp_struct = {plabel:'Smoothing Resolution (sec)', $
;                                  pvalue:'61'})
;      smooth_res = thm_ui_dproc_par_choose(opar, gui_id = guiid, $
;                                           title = 'Choose Smoothing Time Resolution')
;      obj_destroy, opar
      values = thm_ui_smooth_data_options(guiid, sbar, info.historywin)
      If ~values.ok Then Begin ;check par values
        out_string = 'Operation Cancelled: '+uvlong[is_possible]
;      Endif Else If(is_numeric(smooth_res) Eq 0) Then Begin
;        out_string = 'Invalid smoothing time: '+smooth_res
      Endif Else Begin
;        smooth_res0 = double(smooth_res[0])
;        If(smooth_res0 Le 0) Then out_string = 'Invalid smoothing time: '+smooth_res $
        otp = call_dproc(uv, values, out_string = out_string,hwin=info.historyWin,sbar=info.statusBar,call_sequence=call_sequence,loadedData=info.loadedData, gui_id=guiid) 
      Endelse
    End
    'blkavg': Begin
;      opar = obj_new('thm_ui_dproc_par', dp_string = uv, $
;                     dp_struct = {plabel:'Time Resolution (sec)', $
;                                  pvalue:'60'})
;      time_res = thm_ui_dproc_par_choose(opar, gui_id = guiid, $
;                                         title = 'Choose Averaging Time Resolution')
;      obj_destroy, opar
;      If(time_res[0] Eq 'Cancelled') Then Begin
;        out_string = 'Operation Cancelled: '+uvlong[is_possible]
;      Endif Else If(is_numeric(time_res) Eq 0) Then Begin
;        out_string = 'Invalid averaging time: '+time_res
;      Endif Else Begin
;        time_res0 = double(time_res[0])
;        If(time_res0 Le 0) Then out_string = 'Invalid averaging time: '+time_res $
;        Else  otp = call_dproc(uv, {dt:time_res0}, out_string = out_string,hwin=info.historyWin,sbar=info.statusBar,call_sequence=call_sequence,loadedData=info.loadedData, gui_id=guiid) 
;      Endelse

      datap = ptr_new(info.loadeddata)
      values = thm_ui_block_ave_options(guiid, sbar, info.historywin, datap)

      if values.ok then begin
        if obj_valid(values.trange) then str_element,values,'trange', $
          [values.trange->getStartTime(),values.trange->getEndTime()],/add_replace
        otp = call_dproc(uv, values, out_string = out_string,hwin=info.historyWin,sbar=info.statusBar,call_sequence=call_sequence,loadedData=info.loadedData, gui_id=guiid) 
      endif else sbar->update,'Block Average Canceled'

    End
    'clip':Begin
;      opar = obj_new('thm_ui_dproc_par', dp_string = uv, $
;                     dp_struct = {pvalue:['20.0', '-20.0'], $
;                                  plabel:['Max', 'Min']+' for clip'})
;      mmm = thm_ui_dproc_par_choose(opar, gui_id = guiid, $
;                                    title = 'Max/Min for Clip')
;      obj_destroy, opar
;      If(mmm[0] Eq 'Cancelled') Then Begin
;        out_string = 'Operation Cancelled: '+uvlong[is_possible]
;      Endif Else If(total(is_numeric(mmm)) Ne n_elements(mmm)) Then Begin
;        out_string = 'Invalid Max/Min values: '+mmm[0]+', '+mmm[1]
;      Endif Else Begin
;        amin = double(mmm[1])
;        amax = double(mmm[0])
;        If(amin Ge amax) Then Begin
;          out_string = 'Min value: '+mmm[1]+' is GE Max value: '+mmm[0]
;        Endif Else Begin
;        otp = call_dproc(uv,{amin:amin, amax:amax} , out_string = out_string,hwin=info.historyWin,sbar=info.statusBar,call_sequence=call_sequence,loadedData=info.loadedData, gui_id=guiid) 
;        Endelse
;      Endelse

      values = thm_ui_clip_data_options(guiid, sbar, info.historywin)
      if values.ok then begin
        otp = call_dproc(uv,values, out_string = out_string,hwin=info.historyWin,sbar=info.statusBar,call_sequence=call_sequence,loadedData=info.loadedData, gui_id=guiid) 
      endif else sbar->update,'Clip Data Canceled'
    End

    'deflag': Begin
;      plabel = 'Choose a Deflag Method:'
;      opar = obj_new('thm_ui_dproc_par', dp_string = uv, $
;                     dp_struct = {plabel:plabel, radio_array:['Repeat last value', $
;                                                              'Interpolate'], $
;                                  radio_value:'Repeat last value'})
;      method = thm_ui_dproc_par_choose(opar, gui_id = guiid)
;      obj_destroy, opar
;      If(method[0] Eq 'Cancelled') Then Begin
;        out_string = 'Operation Cancelled: '+uvlong[is_possible]
;      Endif Else Begin
;        If(method[0] Eq 'Repeat last value') Then dp_pars = {method:'repeat'} $
;        Else dp_pars = {method:'linear'}
;        otp = call_dproc(uv,dp_pars , out_string = out_string,hwin=info.historyWin,sbar=info.statusBar,call_sequence=call_sequence,loadedData=info.loadedData, gui_id=guiid) 
;      Endelse

      values = thm_ui_deflag_options(guiid, sbar, info.historywin)
      if values.ok then begin
        otp = call_dproc(uv, values, out_string = out_string,hwin=info.historyWin,sbar=info.statusBar,call_sequence=call_sequence,loadedData=info.loadedData, gui_id=guiid)
      endif else sbar->update,'Deflag Canceled'
    End

    'degap':Begin
;      opar = obj_new('thm_ui_dproc_par', dp_string = uv, $
;                     dp_struct = {plabel:['Time Interval (sec)', $
;                                          'Margin (sec)', 'max gapsize (sec)'], $
;                                  pvalue:['1.0', '0.25', '10000']})
;      mmm = thm_ui_dproc_par_choose(opar, title = 'Degap Parameters', $
;                                    gui_id = guiid)
;      obj_destroy, opar
;      If(mmm[0] Eq 'Cancelled') Then Begin
;        out_string = 'Operation Cancelled: '+uvlong[is_possible]
;      Endif Else If(total(is_numeric(mmm)) Ne n_elements(mmm)) Then Begin
;        out_string = 'Invalid Degap values: '+mmm[0]+', '+mmm[1]+', '+mmm[2]
;      Endif Else Begin
;        mmm = double(mmm)
;        oops = where(mmm Lt 0.0, noops)
;        If(noops Gt 0) Then Begin
;          out_string = 'Negative Degap values detected: '+strcompress(string(mmm[oops]))
;        Endif Else Begin
;          otp = call_dproc(uv,{degap_dt:mmm[0], margin:mmm[1], maxgap:long(mmm[2])} , out_string = out_string,hwin=info.historyWin,sbar=info.statusBar,call_sequence=call_sequence,loadedData=info.loadedData, gui_id=guiid)  
;     
;        Endelse
;      Endelse

      values = thm_ui_degap_options(guiid, sbar, info.historywin)
      if values.ok then begin
        otp = call_dproc(uv, values,out_string = out_string,hwin=info.historyWin,sbar=info.statusBar,call_sequence=call_sequence,loadedData=info.loadedData, gui_id=guiid)
      endif else sbar->update,'Degap Canceled'
    End
    'wave':Begin
;You need to choose a time range here:
      trange0 = timerange(/current)
      plabel = ['Start Time:', 'Stop Time: ']
      pvalue = time_string(trange0)
      opar = obj_new('thm_ui_dproc_par', dp_string = uv, $
                     dp_struct = {pvalue:pvalue, plabel:plabel})
      trange0 = thm_ui_dproc_par_choose(opar, gui_id = guiid, $
                                        par_pad = 2, label_xsize = 100, $
                                        title = 'Time Range for Wavelet transform', $
                                        bottomlabel = 'Format: yyyy-mm-dd/hh:mm:ss')
      obj_destroy, opar
      If(trange0[0] Eq 'Cancelled') Then Begin
        out_string = 'Operation Cancelled: '+uvlong[is_possible]
      Endif Else Begin
        t00 = thm_ui_timefix(trange0[0])
        If(is_string(t00)) Then Begin
            t00x = time_double(t00)
        Endif Else Begin
            out_string = 'Invalid Start Time: '+trange0[0]
            goto, out_of_case
        Endelse
        t01 = thm_ui_timefix(trange0[1])
        If(is_string(t01)) Then Begin
            t01x = time_double(t01)
        Endif Else Begin
            out_string = 'Invalid End Time: '+trange0[1]
            goto, out_of_case
        Endelse
        trange = [t00x, t01x]
        If(trange[0] Le 0) Then Begin
          out_string = 'Invalid Start Time: '+trange0[0]
        Endif Else If(trange[1] Le 0) Then Begin
          out_string = 'Invalid End Time: '+trange0[1]
        Endif Else If(trange[0] Ge trange[1]) Then Begin
          out_string = 'Start Time: '+trange0[0]+' is GE End Time: '+trange0[1]
        Endif Else Begin
          otp = call_dproc(uv,{trange:trange} , out_string = out_string,hwin=info.historyWin,sbar=info.statusBar,call_sequence=call_sequence,loadedData=info.loadedData, gui_id=guiid)  
        Endelse
      Endelse
    End
    'hpfilt':Begin
;      opar = obj_new('thm_ui_dproc_par', dp_string = uv, $
;                     dp_struct = {plabel:'Averaging Time (sec)', $
;                                  pvalue:'60'})
;      time_res = thm_ui_dproc_par_choose(opar, gui_id = guiid, $
;                                         title = 'High Pass Filter Averaging Time')
;      obj_destroy, opar
;      If(time_res[0] Eq 'Cancelled') Then Begin
;        out_string = 'Operation Cancelled: '+uvlong[is_possible]
;      Endif Else If(is_numeric(time_res) Eq 0) Then Begin
;        out_string = 'Invalid averaging time: '+time_res
;      Endif Else Begin
;        time_res0 = double(time_res[0])
;        If(time_res0 Le 0) Then out_string = 'Invalid averaging time: '+time_res $
      values = thm_ui_high_pass_options(guiid, sbar, info.historywin)
      if values.ok then begin
        otp = call_dproc(uv, values, out_string = out_string,hwin=info.historyWin,sbar=info.statusBar,call_sequence=call_sequence,loadedData=info.loadedData, gui_id=guiid) 
      endif else out_string = [out_string, 'High Pass Filter Canceled']
    End
    'interpol':Begin

      datap = ptr_new(info.loadeddata)
      
      ;get interpolate options
      result = thm_ui_interpol_options(guiid,info.historywin, sbar, datap, ptree = ptree)

      if is_struct(result) then begin
      
        if result.ok then begin
         
          ;Get active data
          active_data = info.loadedData->getactive(/parent)

          if ~is_string(active_data[0]) then begin
          
            out_string = [out_string, 'Interpolate: No active data.  Returned to Data Processing window.']

          endif else begin

            ;easier to serialize array than object
            if obj_valid(result.trange) then str_element,result,'trange', $
              [result.trange->getStartTime(),result.trange->getEndTime()],/add_replace
          
            thm_ui_interpolate, result,active_data, info.loadedData, info.historywin, $
                                              sbar, out_string=out_string,fail=fail,guiid=guidid
          
            if fail eq 0 then begin
              call_sequence->addInterpOp,result,active_data
            endif
          endelse
                                              
        endif
      endif

    End
  Endcase

;Update the draw object to refresh any plots
info.drawobject->update, info.windowstorage, info.loadeddata
info.drawobject->draw
info.scrollbar->update

;Remember to update messages
  out_of_case:
  For j = 0, n_elements(out_string)-1 Do Begin
      if stregex(out_string[j], '^ *$', /bool) then continue
      sbar -> update, out_string[j]
      info.statusbar -> update, out_string[j]
      info.historywin -> update, out_string[j]
  Endfor
  Return, otp
End

