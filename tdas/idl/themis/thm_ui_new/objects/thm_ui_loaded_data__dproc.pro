;+
; estimate of memory used by a wavelet transform. The estimated memory
; use is 36.26*n_elements(transformed_data.y). The factor of 36 comes
; from testing different transforms for different types of data, for
; fgm (FGH and FGS) data, 2009-01-14, for ESA L2 density data
; 2007-07-07, and for GMAG data for both of those days. Note that this
; is currently only useful for default inputs.
; 10-jun-2009, jmm, added jv output to test for a reasonable number of
; wavelets later, jv must be GT 1 for the wavelet2 routine to work.
;-
Function wv_memory_test, t, jv      ;t is the input time array, jv is the number of wavelets used
  n = n_elements(t)
  dt = (t[1:*]-t)
  
  ;dt = mean(t[1:*]-t)
;Hacked from wavelet2.pro -- these are defaults different from wavelet.pro
  ;w0 = 2.*!pi
  ;dj = 1/8.*(2.*!pi/w0)
  ;prange = [2.*dt, 0.05*n*dt] ; default range = nyquist period - 5% of time period
  ;srange = (2.*dt > prange < n*dt) * (w0+sqrt(2+w0^2))/4/!pi
  ;srange = (prange) * (w0+sqrt(2+w0^2))/4/!pi ;srange are the scales of the wavelets
  ;jv = FIX((ALOG(srange[1]/srange[0])/ALOG(2))/dj);jv+1 is the number of wavelets used


;Check for resampling later in wave_data procedure,
;default is to use mean value
  if total(abs(minmax(dt)/mean(dt)-1)) gt .01 then begin
    dprint,'Using resampled estimate'
        
    ;Resampling will occur at intervals of the median period, 
    times = round(dt/median(dt))

    ;Get total number of points in resample
    n = total(times, /preserve) + 1
    
  endif
  ;jv+1 is the number of wavelets used
  jv = fix( 8*( alog(.05*n)/alog(2) -1 ) ) ;simplified calculation
  
;The memory used in bytes is approximately 36 times the number of
;elements in the final product.  Added 16% margin to account for spikes.
  Return, 1.16*36.26*float(n)*float(jv+1)
  
End
;+
;NAME:
; thm_ui_loaded_data::dproc
;PURPOSE:
; extracts tplot variables from active data, performs data processing
; tasks, creates new variables, optionally sets those variables to
; active variables
;CALLING SEQUENCE:
; success = loaded_data_obj -> dproc(dp_task, dp_pars, out_string = $
;           out_string, names_out=names_out, no_setactive=no_setactive)
;INPUT:
; dp_task = a string variable specifying the task to be carried
;           out. The options are ['subavg', 'submed', 'smooth',
;           'blkavg','clip','deflag','degap','spike','deriv',
;           'pwrspc','wave','hpfilt']
; dp_pars = an anonymous structure containing the input parameters for
; the task, this will be unpacked in this routine and the parameters
; are passed through. Note that, since this is only called from the
; thm_GUI_new routine, there is no error checking for
; content, it is expected that the calling routine passes through the
; proper parameters in each case.
;OUTPUT:
; success = a byte, 0b if the process was unsuccessful or cancelled,
;           1b if the process was completed
;KEYWORDS:
; out_string = an output string, contains either an error message,
;           or names of tplot variables that have been creeated
; names_out = the tplot names of the created data variables
; no_setactive = if set, the new variables will no be set to active at
;                the end of the process.
; hwin, sbar = history window and status bar objects for updates
; gui_id = the id of the calling widget - to pass into warning pop-ups
;HISTORY:
; 16-oct-2008, jmm, jimm@ssl.berkeley.edu
; switched output from message to byte, 29-oct-2008,jmm
; 12-Dec-2008,prc Fixed bug where dproc was not reading data stored in
; loaded data,but instead was reading non-gui-data.
; Fixed bug where data produced by dproc was not inheriting any meta-data.
; 23-jan-2009, jmm, deletes any tplot variables that are created
;                   during processing, added catch, so that deletion
;                   of tplot variables is done if an error bonks a
;                   process.
; 10-Feb-2009, jmm, Added hwin, sbar keywords
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2010-03-01 15:26:32 -0800 (Mon, 01 Mar 2010) $
;$LastChangedRevision: 7389 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/objects/thm_ui_loaded_data__dproc.pro $
Function thm_ui_loaded_data::dproc, dp_task, dp_pars,in_vars=in_vars, names_out = names_out, $
                           out_string = out_string, no_setactive = no_setactive, $
                           hwin = hwin, sbar = sbar, gui_id = gui_id, _extra = _extra

err = 0
catch, err
If(err Ne 0) Then Begin
    catch, /cancel
    Help, /Last_Message, Output=error
    if obj_valid(hwin) then hwin->update, error
    out_msg = 'Warning: An error occured durring processing.  Check the history window for details.'
    If(is_string(out_string)) Then out_string = [out_string, out_msg] $
      Else out_string = out_msg
    ok = error_message(traceback = 1, /noname, title = 'Error in Data Processing: ',/center)
    Goto, return_sequence
Endif

;Initialize output
out_string = ''
names_out = ''
otp = 0b

;Other
addmessage = 0b

if ~is_string(in_vars) then begin
  return,0
endif else begin
  active_v = in_vars
endelse

nav = n_elements(active_v)
dpt = strtrim(strlowcase(dp_task), 2)
;keep track of tnames created in process
tnames_in = tnames()

degap_yesnoall = 0 ; indicates whether the yesall(=1) or noall(-1) flags have been set for degap
blkavg_yesnoall = 0 ; indicates whether the yesall(=1) or noall(-1) flags have been set for degap

For j = 0, nav-1 Do Begin
    canceled = 0b  ; set/reset canceled flag
    skipped = 0b  ; set/reset skipped flag
   
    
    varname = active_v[j]
;Get the data -- first check to see if there are more than 2D. Most of
;                these routines will not work for more than 2d data.

    ;This call to getTvarObject is important for proper function of
    ;the gui. Do not remove.
    ;#1 It reads data from loadedData, not from tplot variables that may be deleted.
    ;#2 It stores all the associated metadata with the object, which is needed to properly classify data.
    tvarObject = self->getTvarObject(varname)
    get_data, varname, data = d
    If(is_struct(d)) Then Begin
        sy = size(d.y, /n_dim)
        If(sy Gt 2) And (dpt Ne 'subavg') And (dpt Ne 'submed') Then Begin
            out_string = [out_string, varname+': data has too many dimensions']
            yes_data = 0b
        Endif Else yes_data = 1b
    Endif Else Begin
        out_string = [out_string, varname+': has no data']
        yes_data = 0b
    Endelse

    If(yes_data) Then Begin
        nn0 = varname
;Now process the data
        Case dpt Of
            'subavg': Begin
                tsub_average, varname, nn, new_name = nn0+'-d'
                nn = tnames(nn0+'-d')
            End
            'submed': Begin
                tsub_average, varname, nn, new_name = nn0+'-m', /median
                nn = tnames(nn0+'-m')
            End
            'deriv': Begin
                deriv_data, varname, newname = nn0+dp_pars.suffix[0], $
                            _extra={nsmooth:( dp_pars.setswidth ? dp_pars.swidth:0b)}
                nn = tnames(nn0+dp_pars.suffix)
            End
            'spike': Begin
                clean_spikes, varname, new_name = nn0+dp_pars.suffix[0], $
                              nsmooth = dp_pars.swidth, $
                              thresh = dp_pars.thresh
                nn = tnames(nn0+dp_pars.suffix)
            End
            'smooth': Begin
                dt = dp_pars.dt[0]
                _extra = {forward:dp_pars.dttype[1], $
                          backward:dp_pars.dttype[2], $
                          no_time_interp:dp_pars.opts[0], $
                          true_t_integration:dp_pars.opts[1], $
                          smooth_nans:dp_pars.opts[2]}
                if dp_pars.setICad then str_element, _extra, 'interp_resolution', dp_pars.icad, /add_replace
                If(dt Lt 1.0) Then dtchar = strcompress(/remove_all, dt) $
                Else dtchar = strcompress(/remove_all, fix(dt))
                tsmooth_in_time, varname, dt, newname = nn0+dp_pars.suffix[0], _extra=_extra,interactive_varname=varname,/interactive_warning,warning_result=warning_result
                if n_elements(warning_result) gt 0 && warning_result eq 1 then begin
                  nn = tnames(nn0+dp_pars.suffix)
                endif
            End
            'blkavg': Begin
;need another sanity test
              get_data, nn0, t
              
              if n_elements(t) gt 1 then begin ; operation not valid on single time element
                av_dt = median(t[1:*]-t)
              
                ok = 1
                If(av_dt Gt dp_pars.dt) && blkavg_yesnoall eq 0 Then Begin
                    lbl = ['Note that the median value of the time resolution for '+nn0+' is:'+strcompress(string(av_dt))+' sec.', $
                           'The value that you have chosen for the averaging time resolution is smaller: '+$
                           strcompress(string(dp_pars.dt))+' sec.', $
                           'This will have non-intuitive and possibly non-plottable results. Do you want to continue?']
                    ;ok = yesno_widget_fn('THEMIS GUI BLK_AVG TEST', label = lbl, gui_id = gui_id)
                    out = thm_ui_prompt_widget(gui_id,sbar,hwin,prompt=strjoin(lbl,thm_ui_newline()),title='THEMIS GUI BLK_AVG TEST',/yes,/no,/allyes,/allno) ; replacing yesno_widget with prompt widget, provides better logging and guarantees centering
                    
                    if out eq 'no' then begin
                      ok = 0
                    endif else if out eq 'yestoall' then begin
                      blkavg_yesnoall = 1
                    endif else if out eq 'notoall' then begin
                      blkavg_yesnoall = -1
                    endif
                   
                Endif
                
                If  (ok || blkavg_yesnoall eq 1) && blkavg_yesnoall ne -1 Then Begin
                    dt = dp_pars.dt[0]
;                    If(dt Lt 1.0) Then dtchar = strcompress(/remove_all, dt) $
;                    Else dtchar = strcompress(/remove_all, fix(dt))
                    avg_data, varname, dt, newname = nn0+dp_pars.suffix[0], $
                              _extra = {trange:( dp_pars.limit ? dp_pars.trange:0b)}
                    nn = tnames(nn0+dp_pars.suffix)
                Endif Else Begin
                    out_string = 'Block Average process for: '+nn0+' cancelled. '
                    canceled = 1b ;prevent variable from being added later
                Endelse
              endif else begin
                out_string = 'Unable to block average '+nn0+' not enough elements. '
                canceled = 1b
              endelse
              
            End
            'clip': Begin
                tclip, varname, dp_pars.minc, dp_pars.maxc, newname = nn0+dp_pars.suffix[0], $
                       _extra = {clip_adjacent: dp_pars.opts[0], $
                                 flag: (dp_pars.opts[1] ? dp_pars.flag:0b)}
                nn = tnames(nn0+dp_pars.suffix)
            End
            'deflag': Begin
                tdeflag, varname, (dp_pars.method[0] ? 'repeat':'linear'), $
                         newname = nn0+dp_pars.suffix[0], $
                         _extra = {flag: (dp_pars.opts[0] ? dp_pars.flag:0b), $
                                   maxgap: (dp_pars.opts(1) ? dp_pars.maxgap:0b)}
                nn = tnames(nn0+dp_pars.suffix)
            End
            'degap': Begin
;need another sanity test
              get_data, nn0, t
              
              if n_elements(t) gt 1 then begin ; operation not valid on single time element
                av_dt = median(t[1:*]-t)
                
                ok = 1         
                If(av_dt Gt dp_pars.dt) && degap_yesnoall eq 0 Then Begin
                  lbl = ['Note that the median value of the time resolution for '+nn0+' is:'+strcompress(string(av_dt))+' sec.', $
                         'The value that you have chosen for the degap time resolution is smaller: '+$
                         strcompress(string(dp_pars.dt))+' sec.', $
                         'This will have non-intuitive and possibly non-plottable results. Do you want to continue?']
                  ;ok = yesno_widget_fn('THEMIS GUI DEGAP TEST', label = lbl, gui_id = gui_id)
                  out = thm_ui_prompt_widget(gui_id,sbar,hwin,prompt=strjoin(lbl,thm_ui_newline()),title='THEMIS GUI DEGAP TEST',/yes,/no,/allyes,/allno) ; replacing yesno_widget with prompt widget, provides better logging and guarantees centering
                 
                  if out eq 'no' then begin
                    ok = 0
                  endif else if out eq 'yestoall' then begin
                    degap_yesnoall = 1
                  endif else if out eq 'notoall' then begin
                    degap_yesnoall = -1
                  endif
                
                endif
                
                If (ok || degap_yesnoall eq 1) && degap_yesnoall ne -1 Then Begin
                    _extra = {maxgap: (dp_pars.opts[1] ? dp_pars.maxgap:0b)}
                    if dp_pars.opts[0] then str_element,_extra,'flag',dp_pars.flag, /add_replace
                    tdegap, varname, dt = dp_pars.dt[0], $
                            margin = dp_pars.margin[0], $
                            maxgap = dp_pars.maxgap[0], $
                            newname = nn0+dp_pars.suffix[0], $
                            _extra = _extra
                    nn = tnames(nn0+dp_pars.suffix)
                Endif Else Begin
                    out_string = 'Degap process for: '+nn0+' cancelled. '
                    canceled = 1b ;prevent variable from being added later
                Endelse
              endif else begin
                out_string ='Unable to process '+nn0+' not enough elements'
                canceled = 1b
              endelse
                
            End
            'pwrspc': Begin
;                popt = thm_ui_pwrspc_options(!thm_gui_new.guiid, hwin)
;                if popt.success eq 0 then return,''

;                thm_ui_pwrspc, nn0, nn, popt.trange, dynamic=popt.dynamic, $
;                               nboxpoints=popt.nboxpoints, $
;                               nshiftpoints=popt.nshiftpoints, $ 
;                               ;tbegin=popt.tbegin, tend=popt.tend, $
;                               bins=popt.bins, noline=popt.noline, $
;                               nohanning=popt.nohanning, notperhz=popt.notperhz
;                nn = tnames(nn0+'_?_dpwrspc')
;                store_data, tnames(nn0), /delete
;                store_data, tnames(nn0+'_?'), /delete
            End
            'wave': Begin
                copy_data, varname, nn0
;Test for memory
                get_data, nn0, t
                sstx = where(t Ge dp_pars.trange[0] And $
                             t Lt dp_pars.trange[1], nsstx)
                If(nsstx Eq 0) Then Begin
                  out_string = 'No Data In Time Range'
                  Return, -1
                Endif Else Begin
                  t = t[sstx]
                  memtest = wv_memory_test(temporary(t), jv)/1.0e6
;added error check for jv, wavelet, wave_data crash on too few data
;points if jv LT 2, jmm, 10-jun-2009
                  If(jv LT 2) Then Begin
                      out_string = 'Only'+strcompress(string(nsstx))+' Data Points. '+$
                        'Not Enough Data In Time Range'
                      Return, -1
                  Endif
                  mem_av = get_max_memblock2()

                  if double(memtest)/mem_av gt .01 then begin       
                    lbl = ['Estimated Memory usage for Wavelet for '+nn0+' is: '+string(memtest, '(f10.2)')+' Mbytes, ', $
                         'You have an estimated '+string(mem_av, '(f10.2)')+' Mbytes, Continue?']
                    ;ok = yesno_widget_fn('THEMIS GUI WAVELET MEMORY CHECK', label = lbl, /center, gui_id = gui_id)
                    out = thm_ui_prompt_widget(gui_id,sbar,hwin,prompt=strjoin(lbl,thm_ui_newline()),title='THEMIS GUI WAVELET MEMORY CHECK',/yes,/no) ; replacing yesno_widget with prompt widget, provides better logging and guarantees centering
                    ok = out eq 'yes'?1:0 
                  endif else begin
                    ok = 1
                  endelse
                  
                  If(ok) Then Begin
                    If(obj_valid(hwin)) Then hwin -> update, 'Processing Wavelet for: '+nn0
                    If(obj_valid(sbar)) Then sbar -> update, 'Processing Wavelet for: '+nn0
                    thm_ui_wavelet, nn0, nn, dp_pars.trange,temp_names=temp_names
                   
                   ;globs removed, not identifying all quantities accurately
                   ;  nn = tnames(nn0+'_?_wv_pow')
                   ;   nn = tnames(nn0+'?_wv_pow')
                    if is_string(temp_names) then begin
                      store_data, temp_names, /delete
                    endif
                  Endif Else Begin
                    out_string = 'Wavelet process for: '+nn0+' cancelled. '
                    canceled = 1b ;prevent variable from being added later
                  Endelse
                  store_data, tnames(nn0), /delete
                Endelse
            End
            'hpfilt': Begin
                dt = dp_pars.dt[0]
                thigh_pass_filter, varname, dt, newname = nn0+dp_pars.suffix[0], $
                                /interactive_warning,warning_result=warning_result, $
                                _extra = {interp_resolution: (dp_pars.seticad ? dp_pars.icad:0b)}
                if n_elements(warning_result) gt 0 && warning_result eq 0 then break
                nn = tnames(nn0+dp_pars.suffix)
            End
        Endcase
    Endif Else nn = ''
;varname is no longer needed ?
;    store_data, varname, /delete
    If(is_string(nn)) and (~canceled) Then Begin
;Add the new variables into the loaded_data object, this will create a
;new group
        For k = 0, n_elements(nn)-1 Do Begin
            If(~otp) Then Begin ;only need 1 success for otp to be set to 1
                get_data, nn[k], data = dtest
                If(is_struct(temporary(dtest))) Then otp = 1b
            Endif
            ;The following 3 lines are important for ensuring that metadata
            ;associated with the old variables is also associated with the
            ;new variables.  Do not remove.

            if dpt eq 'wave' || dpt eq 'pwrspc' then begin
              isSpect = 1
            endif else begin
              tvarObject->getProperty,isSpect=isSpect
            endelse

            newObject = tvarObject->copy()
            newObject->setProperty,name=nn[k],isSpect=isSpect, coordsys=''
            if self->addTvarObject(newObject,added_name=added_name) then begin
              out_string = [out_string, 'Added variable: '+nn[k]]
            endif else begin
              out_string = [out_string, 'Failed to add variable: '+nn[k]]
            endelse
        Endfor
        names_out = [names_out, nn]
    Endif Else Begin
        out_string = [out_string, varname+' Not processed']
        skipped = 1b
    Endelse
    if canceled or skipped then addmessage = 1b ; set flag to notify user later
    heap_gc                     ;clean-up memory
Endfor
;Reset active data to new variables, if there are any
If(otp) Then Begin
    If(is_string(names_out, names_out_ok)) Then Begin
        names_out = names_out_ok
        If(~keyword_set(no_setactive)) Then Begin
            self -> clearallactive
            For j = 0, n_elements(names_out)-1 Do Begin
                self -> setactive, names_out[j]
            Endfor
        Endif
    Endif
Endif

return_sequence:
;Notify user if quantities were dropped (this should be the last item added).
if addmessage then out_string = [out_string, 'Finished.  Some quantities not processed.']
;Remove nulls from output string
If(is_string(out_string, out_string_ok)) Then out_string = out_string_ok

;dump any tplot variables that you didn't start with
tnames_out = tnames()
If(is_string(tnames_out)) Then Begin
    If(is_string(tnames_in)) Then Begin
        For j = 0, n_elements(tnames_out)-1 Do Begin
            xx = where(tnames_in Eq tnames_out[j], nxx)
            If(nxx Eq 0) Then store_data, tnames_out[j], /delete
        Endfor
    Endif Else del_data, '*'    ;no variables when we started, so delete all
Endif

Return, otp
End

