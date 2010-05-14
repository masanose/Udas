;+
;NAME:
;  thm_ui_new_pwrspc
;
;PURPOSE:
;  Creates power spectra from GUI variables that are active in the Process Data
;  window and loads the spectra into the loadedData object.  Directly calls
;  PWRSPC and DPWRSPC to create the spectra.  Intended to be called from
;  THM_UI_NEW_DPROC.
;
;CALLING SEQUENCE:
;  thm_ui_new_pwrspc, loadedData, options
;
;INPUT:
;  options: Structure that is output from THM_UI_PWRSPC_OPTIONS containing
;           keyword options for PWRSPC and DPWRSPC.
;  loadedData: The loadedData object.
;  historyWin: The history window object.
;  statusBar: The status bar object for the Data Processing window.
;  
;KEYWORDS:
;  out_string: array of string messages to be passed back out to THM_UI_NEW_DPROC.
;
;OUTPUT:
;  none
;-


function thm_ui_new_pwrspc_check_time, time, tbegin, tend
  ; This code duplicates what's being done in dpwrspc, but we need it here so
  ; we can produce GUI error messages.
  
  compile_opt idl2, hidden

  if keyword_set(tend) then begin
    t2 = tend
  endif else begin
    t2 = time[n_elements(time)-1]
  endelse

  if keyword_set(tbegin) then begin
    t1 = tbegin
  endif else begin
    t1 = time[0]
  endelse

  igood = where((time ge t1) and (time le t2), jgood)
  if (jgood gt 0) then return, 1 else return, 0

end


pro thm_ui_new_pwrspc, options,invars, loadedData, historyWin, statusBar, $
                       out_string=out_string,fail=totalfail

  compile_opt idl2
  
  ; error catch
  err = 0
  catch, err
  If(err Ne 0) Then Begin
      catch, /cancel
      Help, /Last_Message, Output=err_msg
      If(is_string(out_string)) Then out_string = [out_string, err_msg] $
      Else out_string = err_msg
      ok = error_message(traceback = 1, /noname, title = 'Error in THM_UI_NEW_PWRSPC: ')
      totalfail = 1
      return
  Endif
  
  totalfail=1
  fail=0
  
  ;Initialize output
  out_string = ''
  add_names = ''
  
  active_v = invars
  
  nav = n_elements(active_v)
  
  for i=0, nav-1 do begin
    varname = active_v[i]
    loadedData->getvardata, name=varname, time=t, data=d, limits=l, dlimits=dl
    
    sd = size(*d, /n_dim)
    ncomp = n_elements((*d)[0, *]) ; the number of components of variable
    if (sd gt 2) then begin
      mess = varname + ': data has too many dimensions and will not be processed.'
      statusBar->update, mess
      historyWin->update, 'THM_UI_NEW_PWRSPC: ' + mess
      yes_data = 0b
      fail = 1
    endif else yes_data = 1b
    
    if yes_data then begin
         
      ; check that there's data w/in requested time range
      tsuccess = thm_ui_new_pwrspc_check_time(*t, options.tbegin[0], options.tend[0])
      if options.dynamic[0] AND ~tsuccess then begin
        mess = varname + ': No data in time range. Moving on to next active variable.'
        statusBar->update, mess
        historyWin->update, 'THM_UI_NEW_PWRSPC: ' + mess
        fail = 1
        continue
      endif
    
      ; setup suffixes for power spec var names
      case ncomp of
        1: dsuffix = ''
        3: if keyword_set(polar) then dsuffix = ['_mag','_th','_phi'] $
            else dsuffix = ['_x','_y','_z']
        ;6: dsuffix = '_'+strsplit('xx yy zz xy xz yz',/extract)
        else: dsuffix = '_' + strtrim(lindgen(ncomp),2)
      endcase
      
      for j=0, ncomp-1 do begin
      
        limit=*l
        dlimit=*dl
      
        tpts = n_elements(*t)
        nanidx = where(finite((*d)[*,j]),c)
         
        if c lt tpts && ~options.scrubnans then begin
          message = 'NaNs found in ' + varname + ' component ' + strtrim(j,2) + '. May cause gaps in Power Spectrum.  Degap or Scrub to eliminate errors.'
          statusBar->update,message
          historywin->update,message
          warnNaNs = 1
        endif
    
        if options.dynamic[0] then begin
          
          ; check to make sure there are enough points for a calculation
         
          nspectra = long((tpts-options.nboxpoints[0]/2l)/options.nshiftpoints[0])
          if(nspectra le 0) then begin
            mess = 'Window Size too small for ' + varname + dsuffix[j] + ', and it will not be processed.'
            statusBar->update, mess
            historyWin->update, 'THM_UI_NEW_PWRSPC: ' + mess
            fail = 1
            continue
          endif
          
          if c lt tpts && options.scrubnans then begin
            message = 'Scrubbing NaNs from ' + varname + ' component ' + strtrim(j,2) + '.'
            statusBar->update,message
            historywin->update,message
            dpwrspc, (*t)[nanidx], (*d)[nanidx,j], tps, fdps, dps, nboxpoints=options.nboxpoints[0], $
                  nshiftpoints=options.nshiftpoints[0], bin=options.bins[0], $
                  tbegin=options.tbegin[0], tend=options.tend[0], noline=options.noline[0], $
                  nohanning=options.nohanning[0], notperhz=options.notperhz[0],fail=specfail
          endif else begin
            dpwrspc, *t, (*d)[*,j], tps, fdps, dps, nboxpoints=options.nboxpoints[0], $
              nshiftpoints=options.nshiftpoints[0], bin=options.bins[0], $
              tbegin=options.tbegin[0], tend=options.tend[0], noline=options.noline[0], $
              nohanning=options.nohanning[0], notperhz=options.notperhz[0],fail=specfail
          endelse
                  
          if specfail then begin
            fail = 1
            continue
          endif        
                  
          specdata = {x:temporary(tps), y:temporary(dps), v:temporary(fdps)}
  
          isspect=1
          if in_set('xlog', strlowcase(tag_names(dlimit))) then dlimit.xlog = 0 $
            else str_element, dlimit, 'xlog', 0, /add
  
        endif else begin

          if c lt tpts && options.scrubnans then begin
            message = 'Scrubbing NaNs from ' + varname + ' component ' + strtrim(j,2) + '.'
            statusBar->update,message
            historywin->update,message
            pwrspc, (*t)[nanidx], (*d)[nanidx,j], freq, power, noline=options.noline[0], $
                 nohanning=options.nohanning[0], bin=options.bins[0], $
                 notperhz=options.notperhz[0], err_msg=pwrspc_err_msg
          endif else begin
            pwrspc, *t, (*d)[*,j], freq, power, noline=options.noline[0], $
                 nohanning=options.nohanning[0], bin=options.bins[0], $
                 notperhz=options.notperhz[0], err_msg=pwrspc_err_msg
          endelse

          if ~array_equal(pwrspc_err_msg,'', /no_typeconv) then begin
            historyWin->update, ['THM_UI_NEW_PWRSPC: Error with '+varname+': ', $
                                 '     '+pwrspc_err_msg]
            statusBar->update, ['Error with '+varname+': '+pwrspc_err_msg]
            fail=1
          endif

          avg_time = (dblarr(n_elements(freq)) + 1) * mean(*t, /double)
          specdata = {x:avg_time, y:temporary(power)}
          freqdata = {x:avg_time, y:temporary(freq)}
          
          isspect=0
          str_element, dlimit, 'xlog', 1, /add
            
        endelse
        
        ; set general dlimits
        str_element, dlimit, 'ylog', 1, /add
  
        str_element, dlimit, 'zlog', 1, /add
        
        str_element,dlimit,'labels',value=labels,success=s
        if s && n_elements(labels) eq ncomp then begin
          str_element,dlimit,'labels',[dlimit.labels[j]],/add
        endif
        
        str_element,dlimit,'colors',value=colors,success=s
        if s && n_elements(colors) eq ncomp then begin
          str_element,dlimit,'colors',[dlimit.colors[j]],/add
        endif
        
        ; add data to loadedData object
        specname = varname + dsuffix[j] + options.suffix[0]
        freqname = specname + '_freq'
        if options.dynamic[0] then begin
          success = loadedData->addData(specname, specdata, limit=limit, $
                    dlimit=dlimit, isspect=isspect)
          if success then add_names=[add_names, specname]
        endif else begin
          success = loadedData->addData(specname, specdata, limit=limit, $
                    dlimit=dlimit, isspect=isspect, indepName=freqname)
                    
          if success then begin
            add_names=[add_names, specname]
            
            ; add indep data of power spectra as a separate variable
            success2 = loadedData->addData(freqname, freqdata, limit=limit, dlimit=dlimit, isspect=isspect)
            if success2 then add_names=[add_names, freqname]
          endif
        endelse
           
      endfor
  
      ;Reset active data to new variables, if there are any
      if ~array_equal(add_names, '') then begin
        totalfail = 0
        loadedData->clearAllActive
        for j=1, n_elements(add_names)-1 do loadedData->setActive, add_names[j]
      endif
  
    endif
    
  endfor
  
  if fail then begin
    mess = 'Some quantities may not have been processed. Check History window.'
    statusBar->update, mess
  endif else if keyword_set(warnnans) then begin 
    message = 'Processing Successful,  but NaNs in data may cause gaps in output.  Check History for more detail.'
    statusBar->update, message 
  endif else begin
    mess = 'Spectra creation successful.'
    statusBar->update, mess
  endelse
  
end