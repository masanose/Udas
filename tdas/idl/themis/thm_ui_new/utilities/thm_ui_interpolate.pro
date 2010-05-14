
;+
;NAME:
;  thm_ui_interpolate
;
;PURPOSE:
;  Interpolates over x-axis of active data and adds new data to
;  loaded data object.  Intended to be called from thm_ui_new_dproc.
;
;CALLING SEQUENCE:
;  thm_ui_interpolate, result, loadedData, historywin, statusbar, $
;                      out_string=out_string
;
;INPUT:
;  loadedData: Loaded data object.
;  historywin: History window reference.
;  statusbar: Status bar reference.
;  out_string: Existing string array to dump error messages into.
;  guiid: If set and valid, will make user queries part of GUI widget hierarchy.
;  result: Anonymous structure returned by thm_ui_interpol_options:
;          {
;           num: Number of points for the result when not matching.
;           cad: Cadence of the result when using that option
;           match: Flag indicating to interpolate to another variable's abcissa.
;           matchto: Group name of variable to match to.
;           extrap: Flags for extrapolation type: [none, last value, NaN]
;           suffix: Suffix to create new data group with.
;           limit: Flag indicating time limits.
;           trange: Time range array containing desired time limits.
;           type: Flags for interpolation type: [linear,quadratic,lsquadratic,spline]
;           ntype: Flag to use number or cadence (0,1 respectively)
;           ok: (not used) Indicates thm_ui_interpol_options was successful
;           }
;
;HISTORY:
;
;-

pro thm_ui_interpolate, result,in_vars, loadedData, historywin, statusbar, $
                        out_string=out_string, _extra=_extra,fail=fail,guiid=guidid

    compile_opt idl2

  catch, on_err
  if on_err ne 0 then begin
    catch, /cancel
    help, /last_message, output=msg
    for i=0, n_elements(msg)-1 do historywin->update,msg[i]
    ok = error_message('Error in Interpolate function. Interpolation halted.', $
                       /center,/noname,title='Interpolation Error')
    fail=1
    return
  endif
  
  if ~keyword_set(guiId) then begin
    guiId = 0
  endif


active_data = in_vars


;Initializations
fail=0b
new_active=''
skipped=''
_extra = {QUADRATIC:result.type[1], $
          LSQUADRATIC:result.type[2], $
          SPLINE:result.type[3]}

cadenceyesnoall = 0
overwriteyesnoall = 0

;Loop over active data
for j=0, n_elements(active_data)-1 do begin

  ;Get data elements
  loadedData->getvardata, name=active_data[j], $
                          time = t, $
                          data = d, $
                          yaxis = yd, $
                          dlimit=dl, $
                          limits=l
      
  if ~ptr_valid(t) || ~ptr_valid(d) then begin
    warning_message = active_data[j] + 'is invalid please re-try'
    ok = thm_ui_prompt_widget(guiId,statusbar,historywin,promptText=warning_message,title='Interpolate error')
    ;fail = 1
    continue
  endif 
      
  if ptr_valid(dl) then begin
    dlimit = *dl
  endif 
  
  if ptr_valid(l) then begin
    limit = *l
  endif

  ydata = ptr_valid(yd) ? 1b:0b

  ;Get time restrictions
  if result.limit[0] then begin
    t0 = result.trange[0]
    t1 = result.trange[1]
  endif else begin
    t0 = (*t)[0]
    t1 = (*t)[n_elements(*t)-1]
  endelse

  ;Apply time limits 
  idx = where( ((*t) ge t0) and ((*t) le t1), c)
  if c eq 0 then begin
    warning_message = 'No data in selected range for ' + active_data[j] + ' please re-try'
    ok = thm_ui_prompt_widget(guiId,statusbar,historywin,promptText=warning_message,title='Interpolate error')
    ;fail = 1
    continue
  endif

  if cadenceyesnoall eq -1 then begin
    skipped = [skipped,active_data[j]]
    continue
  endif

  ;Check for matching
  ;----------
    if result.match[0] then begin

;      if result.matchto[0] eq active_data[j] then begin
;        statusbar->update,'Skipping '+active_data[j]+', cannot match to same quantity.'
;        continue
;      endif else 
      
      statusbar->update,'Interpolate: Matching to '+result.matchto[0]+'...'

      ;Get new abscissa for interpol function
      loadedData->getvardata, name=result.matchto[0], time = u,dl=dl2

      ;Apply time limits to result's abcissa
      idx2 = where( ((*u) ge t0) and ((*u) le t1), c)
      if c eq 0 then begin
        ok = error_message('No data to match in selected range, please re-try', $
                     /center,/noname,traceback=0, title='Interpolate error')
;        fail = 1
        skipped = [skipped,active_data[j]]
        continue
      endif

      n_d0 = n_elements((*d)[idx,0])
      n_d1 = n_elements((*d)[0,*])
      n_u = n_elements((*u)[idx2])
      if ydata then n_yd1 = n_elements((*yd)[0,*])

      ;Check for resolution decrease
      if n_d0 gt n_u && cadenceyesnoall ne 1 then begin
        prompt = 'Matching data will decrease time resolution for '+active_data[j]+ $
                            '. Proceed?'
                            
        ok = thm_ui_prompt_widget(guiId,statusbar,historywin,promptText=prompt,$
                                  /yes,/no,/allyes,/allno,title='Decrease time resolution?')
        
        if ok eq 'no' then begin
;          fail = 1
          skipped = [skipped,active_data[j]]
          continue
        endif else if ok eq 'notoall' then begin
          skipped = [skipped,active_data[j]]
          cadenceyesnoall = -1
          continue
        endif else if ok eq 'yestoall' then begin
          cadenceyesnoall = 1
        endif
        
      endif

      ;Do interpolation
      ;-----------
      d_p = dblarr( n_u, n_d1, /nozero)
      for i=0, n_d1-1 do begin
        d_p[0,i] = interpol( (*d)[idx,i], (*t)[idx], (*u)[idx2], _extra=_extra )
      endfor

      ;Interpolate over any yaxis data
      ;-----------
      if ydata then begin
        yd_p = dblarr( n_u, n_yd1, /nozero)
        for i=0, n_yd1-1 do begin
          yd_p[0,i] = interpol( (*yd)[idx,i], (*t)[idx], (*u)[idx2], _extra=_extra )
        endfor
      endif

      ;Apply extrapolation
      if result.extrap[1] || result.extrap[2] then begin
        ;Contraints are equal to bounds in absence of explicit time range
        if ~result.limit[0] then begin
          t0 = (*u)[0]
          t1 = (*u)[n_elements(*u)-1]
        endif
        
        ;Check lower/upper bounds against original data ends and any time range constraints
        bottom_idx = where( ((*u) gt t0) and ((*u) lt (*t)[0]), cb)
        top_idx = where( ((*u) gt (*t)[n_elements(*t)-1]) and ((*u) lt t1), ct)

        ;Extrapolate with NaNs.
        if result.extrap[2] then begin

          ;Bottom
          if cb gt 0 then begin
            d_p = [replicate(!values.d_nan, cb, n_d1),temporary(d_p)]

            if ydata then begin ;extrapolate y-data
              ybottom = dblarr(cb, n_yd1, /nozero)
              for i=0, n_yd1-1 do ybottom[*,i] = yd_p[0,i]
              yd_p = [temporary(ybottom), temporary(yd_p)]
            endif

            idx2 = [bottom_idx, temporary(idx2)]
          endif

          ;Top
          if ct gt 0 then begin
            d_p = [temporary(d_p), replicate(!values.d_nan, ct, n_d1)]

            if ydata then begin ;extrapolate y-data
              ytop = dblarr(ct, n_yd1, /nozero)
              for i=0, n_yd1-1 do ytop[*,i] = yd_p[n_u-1,i]
              yd_p = [temporary(yd_p), temporary(ytop)]
            endif

            idx2 = [temporary(idx2), top_idx]
          endif
        
        ;Extrapolate with last value
        endif else begin

          ;Bottom
          if cb gt 0 then begin
            bottom = dblarr(cb, n_d1, /nozero)
            for i=0, n_d1-1 do bottom[*,i] = d_p[0,i]
            d_p = [temporary(bottom), temporary(d_p)]
  
            if ydata then begin ;extrapolate y-data
              ybottom = dblarr(cb, n_yd1, /nozero)
              for i=0, n_yd1-1 do ybottom[*,i] = yd_p[0,i]
              yd_p = [temporary(ybottom), temporary(yd_p)]
            endif
  
            idx2 = [bottom_idx, temporary(idx2)]
          endif

          ;Top
          if ct gt 0 then begin
            top = dblarr(ct, n_d1, /nozero)
            for i=0, n_d1-1 do top[*,i] = d_p[n_u-1,i]
            d_p = [temporary(d_p), temporary(top)]
  
            if ydata then begin ;extrapolate y-data
              ytop = dblarr(ct, n_yd1, /nozero)
              for i=0, n_yd1-1 do ytop[*,i] = yd_p[n_u-1,i]
              yd_p = [temporary(yd_p), temporary(ytop)]
            endif
  
            idx2 = [temporary(idx2), top_idx]
          endif
        endelse
      endif

      ;New abcissa
      t_p = (*u)[idx2]

  ;Check if using Cadence
  ;---------
    endif else if result.ntype[0] then begin


      statusbar->update,'Interpolate: Using '+strtrim(result.cad[0])+' second cadence...'


      n_d0 = n_elements((*d)[idx,0])
      n_d1 = n_elements((*d)[0,*])
      if ydata then begin
        n_yd1 = n_elements((*yd)[0,*])
      endif

      ;Create new abcissa at desired cadence
      n_u = floor( (t1-t0)/result.cad[0],/l64)

      u = result.cad[0]*dindgen(n_u) + t0

      ;Check requested resolution

      if n_d0 gt n_u && cadenceyesnoall ne 1 then begin
        prompt = 'Using the requested cadence  ('+strtrim(result.cad[0],2)+ $
                            ' sec) will decrease time resolution for '+active_data[j]+ $
                            ' .  Proceed?'
        ok = thm_ui_prompt_widget(guiId,statusbar,historywin,promptText=prompt,$
                                  /yes,/no,/allyes,/allno,title='Decrease time resolution?')
        
        if ok eq 'no' then begin
;          fail = 1
          skipped = [skipped,active_data[j]]
          continue
        endif else if ok eq 'notoall' then begin
          skipped = [skipped,active_data[j]]
          cadenceyesnoall = -1
          continue
        endif else if ok eq 'yestoall' then begin
          cadenceyesnoall = 1
        endif
       
      endif

      ;Do interpolation
      ;-----------
      d_p = dblarr( n_u, n_d1, /nozero)
      for i=0, n_d1-1 do begin
        d_p[0,i] = interpol( (*d)[idx,i], (*t)[idx], (u), _extra=_extra )
      endfor

      ;Interpolate over any yaxis data
      ;-----------
      if ydata then begin
        yd_p = dblarr( n_u, n_yd1, /nozero)
        for i=0, n_yd1-1 do begin
          yd_p[0,i] = interpol( (*yd)[idx,i], (*t)[idx], (u), _extra=_extra )
        endfor
      endif

      t_p = temporary(u)

  ;If not, use default option
  ;---------
    endif else begin

      statusbar->update,'Interpolate: Using '+strtrim(result.num[0])+' points...'

      n_d0 = n_elements((*d)[idx,0])
      n_d1 = n_elements((*d)[0,*])
      if ydata then begin
        n_yd1 = n_elements((*yd)[0,*])
      endif

      

      ;Check requested resolution
      if n_d0 gt result.num[0] && cadenceyesnoall ne 1 then begin
        prompt = 'The requested number of data points is less than '+$
                 'the current data resolution for '+active_data[j]+ $
                 ' ( '+strtrim(n_d0,2)+' points).  Proceed?'
        ok = thm_ui_prompt_widget(guiId,statusbar,historywin,promptText=prompt,$
                                  /yes,/no,/allyes,/allno,title='Decrease time resolution?')
        
        if ok eq 'no' then begin
;          fail = 1
          skipped = [skipped,active_data[j]]
          continue
        endif else if ok eq 'notoall' then begin
          skipped = [skipped,active_data[j]]
          cadenceyesnoall = -1
          continue
        endif else if ok eq 'yestoall' then begin
          cadenceyesnoall = 1
        endif
      endif

      ;Do interpolation
      ;-----------
      d_p = dblarr(result.num[0], n_d1, /nozero)
      for i=0, n_d1-1 do begin
        d_p[0,i] = interpol( (*d)[idx,i], result.num[0], _extra=_extra)
      endfor

      ;Interpolate over any yaxis data
      ;----------- 
      if ydata then begin
        yd_p = dblarr(result.num[0], n_yd1, /nozero)
        for i=0, n_yd1-1 do begin
          yd_p[0,i] = interpol( (*yd)[idx,i], result.num[0], _extra=_extra)
        endfor
      endif

      ;Match number of times using linear default for consistancy
      t_p = interpol( (*t)[idx], result.num[0])

    endelse


  ;Set up data to be added
    name = active_data[j]+result.suffix[0]
    if ptr_valid(l) then limit = *l else limit=0 
    if ptr_valid(dl) then dlimit = *dl else dlimit=0
    if ydata then data = {x:temporary(t_p), y:temporary(d_p), v:temporary(yd_p)} $
      else data = {x:temporary(t_p), y:temporary(d_p)}

  ;Check for overwrite
    names = loadedData->getall()
    rewrite = where(name eq names, nx)
    if nx gt 0 then begin
      ;notoall, then skip quantity before querying
      if overwriteyesnoall eq -1 then begin
        skipped=[skipped,active_data[j]]
        continue
      endif else if overwriteyesnoall ne 1 then begin
        prompt = 'Do you want to overwrite '+names[rewrite[0]]+' with the new interpolated data?'
        answer = thm_ui_prompt_widget(guiId,statusbar,historywin,prompttext=prompt,$
                                      /yes,/allyes,/no,/allno,title='Overwrite Existing Data?')
        
        if answer eq 'no' then begin
          skipped=[skipped,active_data[j]]
          continue
        endif else if answer eq 'yestoall' then begin
          overwriteyesnoall = 1
        endif else if answer eq 'notoall' then begin
          overwriteyesnoall = -1
          skipped=[skipped,active_data[j]]
          continue
        endif
      endif
    endif

  ;Add data
    add = loadedData->addData(name, data, dlimit=dlimit, limit=limit, isspect=ydata)
    if add then new_active = [new_active,name] $
      else skipped = [skipped,active_data[j]]

endfor


;Reset active data quantities
  if n_elements(new_active) gt 1 then begin
    loadedData->clearAllActive
    for i=1, n_elements(new_active)-1 do loadedData->setactive,new_active[i]
  endif

;Return messages for any problems encountered
  if fail then statusbar->update,'Interpolate:  One or more problems occured. See history.' $
    else statusbar->update,'Interpolate: Successful'

  if n_elements(skipped) gt 1 then begin
    for i=1, n_elements(skipped)-1 do begin
      out_string = [out_string, skipped[i]+' not processed.']
    endfor
  endif


end
