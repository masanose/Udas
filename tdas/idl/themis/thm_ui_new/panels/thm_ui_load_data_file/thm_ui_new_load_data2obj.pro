;+   
;NAME:
;  thm_ui_new_load_data2obj
;
;PURPOSE:
;  A widget interface to load CDF data for whatever instrument
;
;CALLING SEQUENCE:
;  thm_ui_new_load_data2obj, st_time, en_time, $
;                              dtype = dtype, $
;                              observ = observ, $
;                              scm_cal = scm_cal, $
;                              outcoord = outcoord, $
;                              loadedData = loadedData, $
;                              historyWin=historyWin, $
;                              statusText=statusText, $
;                              state_gui_id=state_gui_id
;INPUT:
;  st_time, en_time = start and end times in seconds from
;                   1-jan-1970 0:00
;KEYWORDS:
;  dtype =  the type of data, a string, of form 'instrument/datatype/datalevel', 
;           the default is 'gmag/mag/l2'
;  observ = the probe or ground station requested (string array) (this replaces
;           the station, astation, and probe keywords).
;  scm_cal = a structure containing calibration parameters
;  outcoord = requested output coordinates as set in thm_ui_new Themis: Load Data
;             panel
;  loadedData = loaded data object
;  historyWin = history window object
;  statusText = status bar object
;  state_gui_id = gui_id of main gui window
;  
;
;OUTPUT:
;  none
;
;
;HISTORY:
;  07-sep-2008, bck  begin modification for use in thm_gui_new from thm_ui_load_data_fn
; 
;$LastChangedBy: pcruce $
;$LastChangedDate: 2010-03-01 16:24:31 -0800 (Mon, 01 Mar 2010) $
;$LastChangedRevision: 7391 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_load_data_file/thm_ui_new_load_data2obj.pro $
;
;-

pro thm_ui_new_del_temp_tvar, pre_names, post_names, del_names=del_names, $
                              pre_times, post_times, $
                              owrite_names=owrite_names, $
                              arr_owrite_names=arr_owrite_names
; Used to delete tplot vars that were created as support data, but are not to be
; loaded into data object.

  compile_opt idl2, hidden

  if array_equal(pre_names,'') then begin
    del_names = post_names
    owrite_names = ''
    arr_owrite_names = ''
    return
  endif
  
  del_names = ''
  
  for i=0,n_elements(post_names)-1 do begin
    
    keep_ind = where(post_names[i] eq pre_names)
    
    if keep_ind eq -1 then begin
    
      if i eq 0 then del_names = post_names[i] $
        else del_names = [del_names, post_names[i]]
    endif else begin
    
      ;if i eq 0 then begin
      if ~keyword_set(owrite_names) then begin
        if post_times[i] gt pre_times[keep_ind] then begin
          owrite_names = pre_names[keep_ind]
          arr_owrite_names = pre_names[keep_ind]
        endif
      endif else begin
        if post_times[i] gt pre_times[keep_ind] then begin
          owrite_names = owrite_names + ', ' + pre_names[keep_ind]
          arr_owrite_names = [arr_owrite_names, pre_names[keep_ind]]
        endif
      endelse
    endelse
  endfor
  
  if ~array_equal(del_names, '') then $
    del_names = del_names[1:n_elements(del_names)-1]
  
  if ~keyword_set(owrite_names) then begin
    owrite_names=''
    arr_owrite_names=''
  endif

end

;NOTE, this routine is called in several places.
;If you change these arguments, make sure to search the distribution
;and update the code in all instances where it is called.
pro thm_ui_new_load_data2obj, st_time, en_time, $
                              dtype = dtype0, $
                              observ=observ, $
                              scm_cal = scm_cal, $    ; not used now but might be later
                              outcoord=outcoord, $
                              loadedData=loadedData, $
                              historyWin=historyWin, $
                              statusText=statusText, $
                              state_gui_id=state_gui_id,$
                              loadedVarList=loadedVarList ; returns a list of the variables that were loaded

  Compile_Opt idl2, hidden
  
  thm_init
 
  ; initialization stuff
  otp = -1
  no_load = 0  ; flag to indicate whether a load routine did not load one of
               ; the data quantities
  load_err = 0  ; flag to indicate whether there was an error trying to load
                ; one of the data quantities
  prev_percent_done = 0. ; initialize for progress counter
  out_coord = outcoord ; create local copy so it's not overwritten
  
  cmd_tnames = tnames() ; get tplot var names that were created outside of gui
  overwrite_vars = '' ; stores tplot var names that have been overwritten by gui
  clobber='' ; stores the answer to the clobber widget
  
  loadedVarList = '' ; init with null value

  ;people are inconsistent in using the trange keyword, so set a
  ;timespan...:
  tt = [st_time, en_time]
  t1 = str2time(strmid(time_string(tt[0]), 0, 10))
  t2 = str2time(strmid(time_string(tt[1]-1.), 0, 10))
  ndays = 1+fix((t2-t1)/(24.*3600.))
  timespan, t1, ndays
  
  If(n_elements(dtype0) Eq 0) Then dtype_all = 'gmag/mag/l2' $    ; necessary?
    Else dtype_all = strlowcase(strcompress(dtype0, /remove_all))
  ndtype = n_elements(dtype_all)
  nobs = n_elements(observ)
 
  ;get the instrument type datatype and dlevel
  instr = strarr(ndtype)
  iname = instr
  dlvl = instr

  ;get the instrument type datatype and dlevel
  for a = 0, ndtype-1 do begin
    ppp = strsplit(dtype_all[a], '/', /extract)
    instr[a] = ppp[0]
    iname[a] = ppp[1]
    dlvl[a] = ppp[2]
  endfor
  
  for i=0L,nobs-1 do begin ; loop over observatories (probes/stations)
  
  init_time = systime(/sec)
  
  ;GMAG
    ss = where(instr Eq 'gmag')
    If(ss[0] Ne -1) Then Begin
      thm_load_gmag, site=observ[i], trange=[st_time, en_time], files=fns4obj
      out_coord=''
    Endif

  ;ASI
    ss = where(instr Eq 'asi')
    If(ss[0] Ne -1) Then Begin
      thm_load_asi, site=observ[i], trange=[st_time, en_time], $
        datatype=iname, files=fns4obj
    Endif

  ;ASK
    ss = where(instr Eq 'ask')
    If(ss[0] Ne -1) Then Begin
      thm_load_ask, site = observ[i], trange=[st_time, en_time], $
        datatype=iname, files=fns4obj
    Endif

  ;EFI
    ss = where(instr Eq 'efi')
    If(ss[0] Ne -1) Then Begin
      pre_tnames_tmp = tnames(create_time=pre_times) ; get tplotvars that are already loaded
      u_lev = uniq(dlvl[ss]) & lvls = dlvl[ss[u_lev]]

      ;load state data, if not there
      if array_equal(observ[i], 'f') then begin ; because 'f' isn't valid for thm_load_state
        no_load = 1
        historyWin->Update, 'LOAD DATA: Probe "f" is not a valid probe for L1 EFI data.' 
        continue
      endif
      thm_ui_check4spin, 'th'+observ[i]+'_efi_dummy', vx1, vx1, h1, $
                          probe_in=observ[i], trange=[st_time, en_time]

      post_tnames_tmp = tnames(create_time=post_times) ;get tplotvars after current load
      
      thm_ui_new_del_temp_tvar, pre_tnames_tmp, post_tnames_tmp, $
                               pre_times, post_times, $
                               del_names=del_names, owrite_names=owrite_names
      if array_equal(overwrite_vars, '') then overwrite_vars=owrite_names $
        else overwrite_vars = overwrite_vars + ', ' + owrite_names
      
      For j = 0, n_elements(lvls)-1 Do Begin ; loop over EFI levels
        ssj = where(dlvl[ss] Eq lvls[j])
        If(ssj[0] Ne -1) Then Begin
          thm_load_efi, probe=observ[i], datatype=iname[ss[ssj]], $
            level=lvls[j], /get_support_data, trange=[st_time, en_time], $
            coord=out_coord, files=fns4obj
        Endif
      Endfor ; loop over EFI levels
      
      if ~array_equal(del_names, '') then store_data, delete=del_names        
    Endif

  ;FBK
    ss = where(instr Eq 'fbk')
    If(ss[0] Ne -1) Then Begin
      u_lev = uniq(dlvl[ss]) & lvls = dlvl[ss[u_lev]]
      For j = 0, n_elements(lvls)-1 Do Begin
        ssj = where(dlvl[ss] Eq lvls[j])
        If(ssj[0] Ne -1) Then Begin
          thm_load_fbk, probe=observ[i], datatype=iname[ss[ssj]], $
            level = lvls[j], /get_support_data, trange=[st_time, en_time], $
            files=fns4obj
        Endif
      Endfor
    Endif

  ;FFT
    ss = where(instr Eq 'fft')
    If(ss[0] Ne -1) Then Begin
      u_lev = uniq(dlvl[ss]) & lvls = dlvl[ss[u_lev]]
      For j = 0, n_elements(lvls)-1 Do Begin
        ssj = where(dlvl[ss] Eq lvls[j])
        If(ssj[0] Ne -1) Then Begin
        
          ;if m eq 0 then begin
          
            dtype_names=strarr(ndtype)
            
            for z=0L,ndtype-1 do begin
              zzz = strsplit(dtype_all[z], '/', /extract)
              dtype_names[z] = zzz[1]
            endfor
            
            dtype_names_copy = dtype_names ; copy needed because thm_load_fft overwrites it
            pre_tnames_tmp = tnames(create_time=pre_times)
            
            thm_load_fft, probe=observ[i], datatype=dtype_names, $
              level=lvls[j], /get_support_data, trange=[st_time, en_time], $
              files=fns4obj
            
          ; code to look for which requested dtypes were not loaded
          ; \/
            post_tnames_tmp = tnames(create_time=post_times)
            thm_ui_new_del_temp_tvar, pre_tnames_tmp, post_tnames_tmp, $
                               pre_times, post_times, $
                               del_names=del_names, arr_owrite_names=arr_owrite_names

            loadedTypes = strmid([del_names, arr_owrite_names], 4)
            noloadTypes = ''
            
            for z=0L,ndtype-1 do begin
              fft_ind = where(dtype_names_copy[z] eq loadedTypes, n)
              if n eq 0 then noloadTypes = [noloadTypes, dtype_names_copy[z]]
            endfor
            
            if n_elements(noloadTypes) gt 1 then begin
              no_load = 1  
              noloadTypes = noloadTypes[1:*]

              mess = 'LOAD DATA: The following FFT data types were not available for Probe ' $
                     + strupcase(observ[i]) + ' during the requested time range: '
              h = thm_ui_new_multichoice_history(mess,noloadTypes)
              historyWin->Update, h
            endif
          ; /\
          ; code to look for which requested dtypes were not loaded
            
            
          ;endif
        Endif
      Endfor
    Endif

  ;FGM
    ss = where(instr Eq 'fgm')
    If(ss[0] Ne -1) Then Begin
      pre_tnames_tmp = tnames(create_time=pre_times) ; get tplotvars that are already loaded
      u_lev = uniq(dlvl[ss])
      lvls = dlvl[ss[u_lev]]
      ;load state data, if not there, if loading level 1 data
      ss1 = where(lvls Eq 'l1', nl1)
      If(nl1 Gt 0) Then Begin
        if array_equal(observ[i], 'f') then begin ; because 'f' isn't valid for thm_load_state
          no_load = 1
          historyWin->Update, 'LOAD DATA: Probe "f" is not a valid probe for L1 FGM data.' 
          continue
        endif
        thm_ui_check4spin, 'th'+observ[i]+'_fgm_dummy', vx1, vx1, h1, $
          probe_in=observ[i], trange=[st_time, en_time]
      Endif
      
      post_tnames_tmp = tnames(create_time=post_times) ;get tplotvars after current load
      
      thm_ui_new_del_temp_tvar, pre_tnames_tmp, post_tnames_tmp, $
                               pre_times, post_times, $
                               del_names=del_names, owrite_names=owrite_names
      if array_equal(overwrite_vars, '') then overwrite_vars=owrite_names $
        else overwrite_vars = overwrite_vars + ', ' + owrite_names
      
      For j = 0, n_elements(lvls)-1 Do Begin
        ssj = where(dlvl[ss] Eq lvls[j])
        If(ssj[0] Ne -1) Then Begin
        
          ;thm_load_fgm needs to be fooled here into reading the appropriate data for L2
          If(lvls[j] Eq 'l2') Then begin
          
            for k=0,n_elements(iname[ss[ssj]])-1 do begin
              ; check for btotal dtype to account for it's different filename
              if array_equal('_btotal', strmid(iname[ss[ssj[k]]], 3, 7)) $
                then iname_tmp = iname[ss[ssj[k]]] else iname_tmp = strmid(iname[ss[ssj[k]]], 0, 3)
              if k eq 0 then iname_mod = iname_tmp else iname_mod = [iname_mod, iname_tmp]
            endfor
          endif else iname_mod = iname[ss[ssj]]
          thm_load_fgm, probe=observ[i], datatype=iname_mod, $
            level=lvls[j], /get_support_data, trange=[st_time, en_time], $
            coord=out_coord, files=fns4obj
        Endif
      Endfor
      
      if ~array_equal(del_names, '') then store_data, delete=del_names  
    Endif

  ;FIT
    ss = where(instr Eq 'fit')
    If(ss[0] Ne -1) Then Begin
      pre_tnames_tmp = tnames(create_time=pre_times) ; get tplotvars that are already loaded
      u_lev = uniq(dlvl[ss]) & lvls = dlvl[ss[u_lev]]
  
    ;load state data, if not there
      if array_equal(observ[i], 'f') then begin ; because 'f' isn't valid for thm_load_state
        no_load = 1
        historyWin->Update, 'LOAD DATA: Probe "f" is not a valid probe for L1 FIT data.' 
        continue
      endif
      thm_ui_check4spin, 'th'+observ[i]+'_scm_dummy', vx1, vx1, h1, $
        probe_in=observ[i], trange=[st_time, en_time]

      post_tnames_tmp = tnames(create_time=post_times) ;get tplotvars after current load
      
      thm_ui_new_del_temp_tvar, pre_tnames_tmp, post_tnames_tmp, $
                               pre_times, post_times, $
                               del_names=del_names, owrite_names=owrite_names
      if array_equal(overwrite_vars, '') then overwrite_vars=owrite_names $
        else overwrite_vars = overwrite_vars + ', ' + owrite_names

      For j = 0, n_elements(lvls)-1 Do Begin
        ssj = where(dlvl[ss] Eq lvls[j])
        If(ssj[0] Ne -1) Then Begin
          out_coord_temp = out_coord ; temp patch to compensate for argument mutation in thm_load_fit

        ; strip off out_coord from end of datatype
          for k=0,n_elements(iname[ss[ssj]])-1 do begin
            c_dtype = iname[ss[ssj[k]]]
            coord_pos_s =  strpos(iname[ss[ssj[k]]], '_'+outcoord, /reverse_search)
            if coord_pos_s ne -1 then begin
              c_dtype = strmid(c_dtype, 0,coord_pos_s)
            endif
            
            if k eq 0 then iname_mod=c_dtype else iname_mod=[iname_mod, c_dtype]
          endfor

        if array_equal(observ[i], 'f') then begin ; because 'f' isn't valid for thm_load_state
          no_load = 1
          historyWin->Update, 'LOAD DATA: Probe "f" is not a valid probe for L1 FIT data.' 
          continue
        endif            
          thm_load_fit, probe=observ[i], datatype=iname_mod, $
            level=lvls[j], /get_support_data, trange=[st_time, en_time], $
            coord=out_coord_temp, files=fns4obj
        Endif
      Endfor
      
      if ~array_equal(del_names, '') then store_data, delete=del_names  
    Endif

  ;SCM
    ss = where(instr Eq 'scm')
    If(ss[0] Ne -1) Then Begin
      pre_tnames_tmp = tnames(create_time=pre_times) ; get tplotvars that are already loaded
      u_lev = uniq(dlvl[ss]) & lvls = dlvl[ss[u_lev]]
  
    ;load state data, if not there
      if array_equal(observ[i], 'f') then begin ; because 'f' isn't valid for thm_load_state
        no_load = 1
        historyWin->Update, 'LOAD DATA: Probe "f" is not a valid probe for L1 SCM data.' 
        continue
      endif
      thm_ui_check4spin, 'th'+observ[i]+'_scm_dummy', vx1, vx1, h1, $
        probe_in=observ[i], trange=[st_time, en_time]

      post_tnames_tmp = tnames(create_time=post_times) ;get tplotvars after current load
      
      thm_ui_new_del_temp_tvar, pre_tnames_tmp, post_tnames_tmp, $
                               pre_times, post_times, $
                               del_names=del_names, owrite_names=owrite_names
      if array_equal(overwrite_vars, '') then overwrite_vars=owrite_names $
        else overwrite_vars = overwrite_vars + ', ' + owrite_names

      For j = 0, n_elements(lvls)-1 Do Begin
        ssj = where(dlvl[ss] Eq lvls[j])
        If(ssj[0] Ne -1) Then Begin
          ; check for keyword
          if keyword_set(scm_cal) then Begin 
            thm_load_scm,probe=observ[i],datatype=iname[ss[ssj]],level=lvls[j], $
               /get_support_data, trange=[st_time,en_time], $
               ;progobj = progobj, coord='dsl', cleanup='full', $
               coord=out_coord, cleanup='full', $
               type='calibrated', scm_cal = scm_cal, files=fns4obj
          endif else begin
            thm_load_scm,probe=observ[i],datatype=iname[ss[ssj]],level=lvls[j], $
               /get_support_data, trange=[st_time,en_time], $
               coord=out_coord, cleanup='full', $
               type='calibrated', files=fns4obj
          endelse
        Endif
      Endfor
      
      if ~array_equal(del_names, '') then store_data, delete=del_names  
    Endif

  ;MOM
    ss = where(instr Eq 'mom')
    If(ss[0] Ne -1) Then Begin
      lvl_1 = where(dlvl[ss] Eq 'l1',  nl1)
      If(nl1 Gt 0) Then Begin
        ssj = where(dlvl[ss] Eq 'l1')
        thm_load_mom, probe=observ[i], datatype=iname[ss[ssj]], /raw, $
          level='l1', trange=[st_time, en_time], files=fns4obj
      Endif
      lvl_2 = where(dlvl[ss] Eq 'l2',  nl2)
      If(nl2 Gt 0) Then Begin
        ssj = where(dlvl[ss] Eq 'l2')
        thm_load_mom, probe=observ[i], datatype=iname[ss[ssj]], $
          level='l2', trange=[st_time, en_time], files=fns4obj
      Endif
  ;    u_lev = uniq(dlvl[ss]) & lvls = dlvl[ss[u_lev]]
  ;    For j = 0, n_elements(lvls)-1 Do Begin
  ;      ssj = where(dlvl[ss] Eq lvls[j])
  ;      If(ssj[0] Ne -1) Then Begin
  ;        thm_load_mom, probe = probe, datatype = iname[ss[ssj]], $
  ;          level = lvls[j], trange = [st_time, en_time], $
  ;          progobj = progobj
  ;      Endif
  ;    Endfor
    Endif

  ;SST
    ss = where(instr Eq 'sst')
    If(ss[0] Ne -1) Then Begin
      u_lev = uniq(dlvl[ss]) & lvls = dlvl[ss[u_lev]]
      For j = 0, n_elements(lvls)-1 Do Begin
        ssj = where(dlvl[ss] Eq lvls[j])
        If(ssj[0] Ne -1) Then Begin
          thm_load_sst, probe=observ[i], datatype=iname[ss], level=lvls[j], $
            trange=[st_time, en_time], files=fns4obj
        Endif
      Endfor
    Endif

  ;ESA
    ss = where(instr Eq 'esa')
    If(ss[0] Ne -1) Then Begin
      lvl_1 = where(dlvl[ss] Eq 'l1',  nl1)
      If(nl1 Gt 0) Then Begin
        app_id = iname[ss[lvl_1]]
        ddd = strcompress(/remove_all, app_id)
        thm_load_esa_pkt, probe=observ[i], datatype=ddd, $
          /get_support_data, trange=[st_time, en_time], $
          suffix='_L1', files=fns4obj
      Endif
      lvl_2 = where(dlvl[ss] Eq 'l2',  nl2)
      If(nl2 Gt 0) Then Begin
        thm_load_esa, probe=observ[i], datatype=iname[ss[lvl_2]], level='l2', $
          trange=[st_time, en_time], coord=out_coord, files=fns4obj
      Endif
    Endif

  ;STATE
    state_ss = where(instr Eq 'state')
    If(state_ss[0] Ne -1) Then Begin
      if array_equal(observ[i], 'f') then begin ; because 'f' isn't valid for thm_load_state
        no_load = 1
        historyWin->Update, 'LOAD DATA: Probe "f" is not a valid probe for STATE data.' 
        continue
      endif
  
      ; account for load spitting out temp vars if coord is set with spin type(s) requested
      spintypes = ['spin_spinper', 'spin_tend', 'spin_c', 'spin_phaserr', $
                   'spin_nspins', 'spin_npts', 'spin_maxgap']
      for j=0,n_elements(spintypes)-1 do begin
        dum_ind = where(spintypes[j] eq iname, nSpin, complement=nonSpinInd, $
                        ncomplement=nNonSpin)
        if nspin gt 0 then begin
          if j eq 0 then spinInd = dum_ind else spinInd = [spinInd, dum_ind]
        endif
      endfor
      
      if keyword_set(spinInd) then begin
        spin_req=iname[spinInd]
        nonSpin_req = ssl_set_complement(spin_req, iname)
      endif else begin
        spin_req=-1L
        nonSpin_req = iname
      endelse
    
      ;velocity constructed from a derivative, rather than loaded directly
      if in_set('vel',iname) then begin  
      
        nonSpin_req = ssl_set_complement(['vel'],ssl_set_union(['vel','pos'],nonSpin_req))
      
      endif
        
      ;check for variables that should not be loaded so that they can be cleaned up later. 
      no_load_vars = 'th'+observ[i]+'_state_' + ['pos_gse','pos_gsm','vel_gse','vel_gsm']
      no_load_vars_exist = bytarr(n_elements(no_load_vars))
      for var_i = 0,n_elements(no_load_vars)-1 do begin
        no_load_vars_exist[var_i] = is_string(tnames(no_load_vars[var_i]))
      endfor  
      
      pos_preexist = is_string(tnames('th'+observ[i]+'_state_pos'))
   
      ; get state data types
      if ~array_equal(nonSpin_req, -1, /no_typeconv) then begin
        thm_load_state, probe=observ[i], datatype=nonSpin_req, $
          /get_support_data, trange=[st_time, en_time], /no_spin, $
          coord='gei', files=fns4obj
      endif
      
      if ~array_equal(spin_req, -1, /no_typeconv) then begin
        ; no out_coord for spin data types
        thm_load_state, probe=observ[i], datatype=spin_req, $
          trange=[st_time, en_time], files=fns4obj
      endif
      
      for var_i = 0,n_elements(no_load_vars)-1 do begin
        if ~no_load_vars_exist[var_i] && is_string(tnames(no_load_vars[var_i])) then begin
          store_data,no_load_vars[var_i],/delete
        endif
      endfor
      
      ;Note that the cotrans operations below will not be harmful because 
      ;#1 cotrans to current coord is identity operation
      ;#2 pos and velocity cannot be requested in different coordinate systems from the same load
      ;transform pos to proper coord if requested
      
      ;this loads any required spin model parameters and stores data to remove unrequested variables.
      if (in_set('pos',iname) || in_set('vel',iname)) && is_string(tnames('th'+observ[i]+'_state_pos')) && $
         thm_ui_req_spin('gei',out_coord,observ[i],[st_time, en_time],loadedData) then begin
         
        req_spin_tvars_cleanup = tnames('*',create_time=cn_before)
        thm_load_state,probe=observ[i],trange=[st_time, en_time],/get_support
        req_spin_tvars_cleanup_flag = 1
      endif else begin
        req_spin_tvars_cleanup_flag = 0
      endelse
      
      if in_set('pos',iname) && is_string(tnames('th'+observ[i]+'_state_pos')) then begin
        thm_cotrans,'th'+observ[i]+'_state_pos','th'+observ[i]+'_state_pos',out_coord=out_coord
        get_data,'th'+observ[i]+'_state_pos',limit=l
        str_element,l,'labels',['x_'+strlowcase(out_coord),'y_'+strlowcase(out_coord),'z_'+strlowcase(out_coord)],/add
        store_data,'th'+observ[i]+'_state_pos',limit=l
      endif
      
      ;create velocity, may require transform pos
      if in_set('vel',iname) && is_string(tnames('th'+observ[i]+'_state_pos')) then begin
        thm_cotrans,'th'+observ[i]+'_state_pos','th'+observ[i]+'_state_pos',out_coord=out_coord
        deriv_data,'th'+observ[i]+'_state_pos',newname='th'+observ[i]+'_state_vel'
        get_data,'th'+observ[i]+'_state_vel',limit=l,dlimit=dl
        str_element,l,'labels',['x_'+strlowcase(out_coord),'y_'+strlowcase(out_coord),'z_'+strlowcase(out_coord)],/add
        str_element,l,'ysubtitle','[km/s]',/add
        str_element,l,'data_att.units','km/s',/add
        str_element,dl,'labels',['x_'+strlowcase(out_coord),'y_'+strlowcase(out_coord),'z_'+strlowcase(out_coord)],/add
        str_element,dl,'ysubtitle','[km/s]',/add
        str_element,dl,'data_att.units','km/s',/add
        store_data,'th'+observ[i]+'_state_vel',limit=l,dlimit=dl
      endif
       
      ;if pos wasn't requested, then get rid of it
      if ~in_set('pos',iname) && is_string(tnames('th'+observ[i]+'_state_pos')) && ~pos_preexist then begin
        store_data,'th'+observ[i]+'_state_pos',/delete
      endif
   
      ;remove any unwanted variables
      if req_spin_tvars_cleanup_flag then begin
        thm_ui_cleanup_tplot,req_spin_tvars_cleanup,create_time_before=cn_before,del_vars=del_vars
        if is_string(del_vars) then begin
        
          if in_set('vel',iname) && in_set(del_vars,'th'+observ[i]+'_state_vel') then begin
            del_vars = ssl_set_complement(['th'+observ[i]+'_state_vel'],del_vars)
          endif
          store_data,del_vars,/delete
        endif
      endif
   
    Endif
    
    ; get tnames created by this routine
    tplotvars = tnames(create_time=create_times)
    new_vars_ind = where(create_times gt init_time, n_new_vars_ind)
    if n_new_vars_ind gt 0 then begin
      otp = tplotvars[new_vars_ind]
      
      ;Check for missing variables
      if n_new_vars_ind lt n_elements(iname) then begin
        no_load=1
        missed=0
        ;Update history window for each missing variable
        for l=0, n_elements(iname)-1 do begin
          check_name_idx = stregex(otp, '.*'+iname[l]+'.*',/bool)
          if ~in_set(check_name_idx,1b) then begin ;check that dataname wasn't found
            missed++
            historyWin->update, 'LOAD DATA: No data loaded for observatory: ' + observ[i] + ', datatype: ' + iname[l]
          endif
        endfor 
        ;Send this message in case the above loop misses something
        if missed lt (n_elements(iname)-n_new_vars_ind) then $
          historyWin->update, 'LOAD DATA: No data loaded for observatory: ' + observ[i] + ', datatype: ???'
      endif
      
    endif else otp = -1

    ; load tplot vars into gui
    if(is_string(otp)) then begin
    
      n_otp = n_elements(otp)
      for k = 0L, n_otp-1 do begin ; loop over number of tplot vars created
        cmd_index = where(otp[k] eq cmd_tnames, n_overwrite) ; index of overwritten tplot vars
        
        ; make sure state data isn't added unless requested by user
        tvar_name_comp = strsplit(otp[k],'_',/extract) ; get tplot var name components
        n_comp = n_elements(tvar_name_comp)
        if n_comp gt 1 then begin
        
          ;doesn't load state data if it was loaded by some other load routine
          if ~array_equal(instr, 'state') then begin
            if (n_overwrite gt 0) AND (tvar_name_comp[1] eq 'state') then continue
          endif else if n_comp ge 3 then begin
           
            if n_comp gt 3 then begin
              tvar_name_type = strjoin(tvar_name_comp[2:*],'_')
            endif else begin
              tvar_name_type = tvar_name_comp[2]
            endelse
           
            ;doesn't load state if available type is not requested type(extending this check to other data types would eliminate the need for a lot of the special rules in this routine.)
            if ~in_set(tvar_name_type,iname) then begin
              continue
            endif
          endif
       
        endif 
        
      ; begin check for and handling of duplicate var names
        groupNames = loadedData->getGroupNames()
        
        if is_string(groupNames) then dupNameInd = where(otp[k] eq groupNames, n) else n=0
              
        if n gt 0 then begin

          if clobber ne 'yestoall' AND clobber ne 'notoall' then begin
            prompttext='The variable ' + strupcase(otp[k]) + ' already exists.  Do you want to ' + $
                  'overwrite it with the new variable?  If you click "No" the new ' + strupcase(otp[k]) + ' will not be loaded.'
          
            clobber = thm_ui_prompt_widget(state_gui_id, statusText,historyWin,promptText= prompttext,defaultValue='',/yes,/no,/allyes,/allno,maxwidth=80,title="Overwrite Data?")
            
            ; loop so that hitting the widget kill button doesn't close the window
            while clobber eq '' do clobber = thm_ui_prompt_widget(state_gui_id, statusText,historyWin,promptText= prompttext,defaultValue='',/yes,/no,/allyes,/allno,maxwidth=80,title="Overwrite Data?")
          endif

          if clobber eq 'yes' OR clobber eq 'yestoall' then begin
            h = 'LOAD DATA: ' + strupcase(otp[k]) + ' will be overwritten.'
            historyWin->Update, h
            statusText->Update, h
          endif
          if clobber eq 'no' OR clobber eq 'notoall' then begin
            h = 'LOAD DATA: ' + strupcase(otp[k]) + $
                ' not loaded into GUI to prevent overwrite of existing data.'
            historyWin->Update, h
            statusText->Update, h
            
            ; compare with owrite_names and don't delete if it exists
            if n_overwrite eq 0 then begin
              store_data, delete=otp[k]
            endif else begin
            
              h = 'LOAD DATA: THEMIS GUI has overwritten tplot variable: ' + otp[k]
              dprint, h
              historyWin->Update, h
              
              ; collect overwritten tplot var names to report to status text
              if array_equal(overwrite_vars, '') then overwrite_vars= otp[k] $
                else overwrite_vars = overwrite_vars + ', ' + otp[k]
            endelse
            
            continue
          endif
          
        endif
      ; end check for and handling of duplicate var names
        
        if ~loadedData->add(otp[k], $
                            ;in the future, file will be able to accept multiple filenames
                            ;and we can remove the [0] index.
                            file=fns4obj[0], $ ; from rel_pathnames_all keyword
                            mission='THEMIS', $
                            observatory=observ[i], $ ;obs4obj, $
                            coordSys=out_coord, $
                            instrument=ppp[0]) then begin
          ok = error_message('Add failed', /traceback, /noname, /center, title='Error in Load Data')
          h = 'LOAD DATA: LOAD FAILURE for ' + otp[k] + '.'
          load_err = 1
        endif else begin
          h = 'LOAD DATA: '+ otp[k] + ' loaded successfully.
          loadedVarList = array_concat(otp[k],loadedVarList)
        endelse
        
        historyWin->Update, h
        
        ;cmd_index = where(otp[k] eq cmd_tnames, n_overwrite)
        if n_overwrite eq 0 then begin
          del_data, otp[k] ; delete tplot var that was just loaded
        endif else begin
          
          h = 'LOAD DATA: THEMIS GUI has overwritten tplot variable: ' + otp[k]
          dprint, h
          historyWin->Update, h
          
          ; collect overwritten tplot var names to report to status text
          if array_equal(overwrite_vars, '') then overwrite_vars= otp[k] $
            else overwrite_vars = overwrite_vars + ', ' + otp[k]
        endelse


      ; progress tracker
        status_inc = 10. ; threshold percentage interval at which status is updated
        ;nloops = float(ndtype * nobs)
        nloops = float(n_otp * nobs)
    ;    if instr eq 'fft' then nloops = nobs ; special case to handle fft loading
        ;loops_sofar =  float(i*ndtype + k+1)
        loops_sofar =  float(i*n_otp + k+1)
        
        percent_done = loops_sofar * 100. / nloops
        
        ; calc when status inc has been passed
        status_switch = floor(percent_done/status_inc) - floor(prev_percent_done/status_inc)
    
        if status_switch gt 0 then begin
          statusText->Update, 'Loading data...' + $
                              strcompress(string(round(percent_done)), /remove_all) + $
                              '% complete.'
        endif
        prev_percent_done = percent_done
      ; end progress tracker

      endfor ; loop over number of tplot vars created
    endif else begin
      no_load = 1
      h = 'LOAD DATA: No data loaded for observatory: ' + observ[i] + ', datatype: ' + iname
      historyWin->Update, h
      
      ; updates for progress tracker when no data for current observatory
      percent_done = (i+1)*100./nobs
      statusText->Update, 'Loading data...' + $
                    strcompress(string(round(percent_done)), /remove_all) + $
                    '% complete.'
      prev_percent_done = percent_done
    endelse
    
  endfor ; loop over observatories (probes/stations)


  if ~array_equal(overwrite_vars, '') then begin
  
    ; Generate list of overwritten tplot vars and output to statusText object
    overwrite_list = string(string(overwrite_vars, format='("", A, ",")'), /print)
    overwrite_list = strmid(overwrite_list, 0, strlen(overwrite_list)-1)
    h_over = 'LOAD DATA: THEMIS GUI has overwritten tplot variable(s): ' + overwrite_list
  endif else h_over = ''
    
  if load_err OR no_load then begin
    h = 'Load completed.  Some quantities did not load.  Please check the History window.  ' + h_over
  endif else begin
    h = 'Load completed.  ' + h_over
  endelse
  
  statusText->Update, h

end
