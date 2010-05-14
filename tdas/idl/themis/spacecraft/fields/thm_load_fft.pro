;+
;Procedure: THM_LOAD_FFT
;
;Purpose:  Loads THEMIS FFT spectra (ParticleBurst and WaveBurst) data
;
;keywords:
;  probe = Probe name. The default is 'all', i.e., load all available probes.
;          This can be an array of strings, e.g., ['a', 'b'] or a
;          single string delimited by spaces, e.g., 'a b'
;  datatype = The type of data to be loaded, can be an array of strings 
;          or single string separate by spaces.  The default is 'all'
;  TRANGE= (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded
;  level = the level of the data, the default is 'l1', or level-1
;          data. A string (e.g., 'l2') or an integer can be used. 'all'
;          can be passed in also, to get all levels.
;  type=   'raw' or 'calibrated'. default is calibrated.
;  suffix= suffix to add to output data quantity (not added to support
;  data)
;  relpathnames_all: named variable in which to return all files that are
;          required for specified timespan, probe, datatype, and level.
;          If present, no files will be downloaded, and no data will be loaded.
;  files   named varible for output of pathnames of local files.
;  CDF_DATA: named variable in which to return cdf data structure: only works
;          for a single spacecraft and datafile name.
;  VARNAMES: names of variables to load from cdf: default is all.
;  /GET_SUPPORT_DATA: load support_data variables as well as data variables 
;                      into tplot variables.
;  /DOWNLOADONLY: download file but don't read it.
;  /VALID_NAMES: if set, then this routine will return the valid
;  probe, datatype and/or level options in named variables supplied as 
;  arguments to the corresponding keywords.
;  /NO_DOWNLOAD: use only files which are online locally.
;  /VERBOSE  set to output some useful info
;Example:
;   thm_load_fft,/get_suppport_data,probe=['a', 'b']
;Notes:
;
; $LastChangedBy: jwl $
; $LastChangedDate: 2010-03-05 15:59:14 -0800 (Fri, 05 Mar 2010) $
; $LastChangedRevision: 7402 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/spacecraft/fields/thm_load_fft.pro $
;-

;this procedure is a callback(higher order) procedure which is passed
;as an argument to thm_load_xxx
;it removes any suffixes from support data,
;sets dlimit.data_att.data_type
;and calls calibration fx
pro thm_load_fft_post, sname=probe, datatype=dt, level=lvl, $
                       tplotnames=tplotnames, $
                       suffix=suffix, proc_type=proc_type, coord=coord, $
                       delete_support_data = delete_support_data, $
                       _extra=_extra


  if not keyword_set(suffix) then suffix = ''
  ;; remove suffix from support data
  ;; and add DLIMIT tags to data quantities
  for l=0, n_elements(tplotnames)-1 do begin    
     tplot_var = tplotnames[l]

     get_data, tplot_var, data=d_str, limit=l_str, dlimit=dl_str
     ;;if we're not dealing with support data
     ;;set the proper dlimit attributes to make pretty plots
     ;;(right now we don't do anything 'cept set the data type)
     if data_type(dl_str) eq 8 && dl_str.cdf.vatt.var_type eq 'data' $
     then begin
       if strmatch(lvl, 'l1') then begin
         data_att = { data_type:'raw'}
         
       end else if strmatch(lvl, 'l2') then begin
         data_att = { data_type:'calibrated'}
       end

       str_element, dl_str, 'data_att', data_att, /add
       store_data, tplot_var, data = d_str, limit = l_str, dlimit = dl_str

     endif else begin
        ;; for support data,
        ;; rename original variable to exclude suffix
        if keyword_set(suffix) then begin
           tplot_var_root = strmid(tplot_var, 0, $
                                   strpos(tplot_var, suffix, /reverse_search))
           store_data, delete=tplot_var
           if tplot_var_root then begin
             store_data, tplot_var_root, data = d_str, limit = l_str, dlimit = dl_str
           endif
           tplot_var = tplot_var_root
         endif
;           ;; save name of support tplot variable for possible deletion
;         if tplot_var && keyword_set(delete_support_data) then begin
;           if size(support_var_list, /type) eq 0 then $
;             support_var_list = [tplot_var] $
;           else $
;             support_var_list = [support_var_list, tplot_var] 
;         endif
     endelse
  endfor

  ;; calibrate, if this is L1
  if strmatch(lvl, 'l1') then begin
     if ~keyword_set(proc_type) || strmatch(proc_type, 'calibrated') then begin
        thm_cal_fft, probe=probe, datatype=dt, $
          in_suffix = suffix, out_suffix = suffix

        ;delete original
;        if size(support_var_list, /type) ne 0 then $
;          del_data, support_var_list
     endif
  endif

  ;; set units and coordinates from CDF attributes for L2
  if strmatch(lvl, 'l2') then begin
      thm_new_units, tplotvars
      thm_new_coords, tplotvars
  endif
end

;the main proc
pro thm_load_fft,probe=probe,$
                 datatype = datatype, $
                 trange = trange, $
                 level=level, $
                 verbose = verbose, $
                 downloadonly = downloadonly, $
                 relpathnames_all = relpathnames_all, $
                 no_download = no_download,  $
                 cdf_data=cdf_data, $
                 get_support_data = get_support_data, $
                 delete_support_data = delete_support_data, $
                 varnames=varnames, $
                 valid_names = valid_names, $
                 files = files, $
                 suffix = suffix, $
                 type = type, $
                 progobj=progobj, $
                 varformat = varformat


   
  if(keyword_set(type)) then $
    message, /info, 'type keyword ignored for fft data'
  
  if not keyword_set(suffix) then suffix = ''
      
  vlevels = 'l1 l2'
  deflevel = 'l1'
  lvl = thm_valid_input(level, 'Level', vinputs = vlevels, definput = deflevel,format="('l', I1)", verbose=0)

  if lvl eq '' then return                            
      
  vsnames = 'a b c d e f'
  
  ;construct valid datatype list
  valid_raw = [ 'fff_16', 'fff_32', 'fff_64', 'ffp_16', 'ffp_32', 'ffp_64', 'ffw_16', 'ffw_32', 'ffw_64']
   
  valid_calibrated = [ 'v1', 'v2', 'v3', 'v4', 'v5', 'v6', $
                       'edc12', 'edc34', 'edc56', $
                       'scm1', 'scm2', 'scm3', $
                       'eac12', 'eac34', 'eac56', $
                       'undef', $
                       'eperp', 'epara', 'dbperp', 'dbpara']

  valid_support = ['adc','src','hed']

  valid_calibrated = array_cross(valid_raw, [valid_calibrated,valid_support])

  valid_calibrated = reform(valid_calibrated[0, *] + '_' + valid_calibrated[1, *])

  valid_datatypes = ssl_set_union(valid_calibrated, valid_raw)


  if(keyword_set(probe)) then $
    p_var = probe

  ;validate inputs
  if not keyword_set(datatype) then datatype = valid_datatypes $
  else datatype = thm_check_valid_name(strlowcase(datatype),valid_datatypes,/include_all)
  
  if(size(datatype,/n_dim) eq 0 && datatype eq '') then return
     
  if not keyword_set(p_var) then p_var = strsplit(vsnames,' ',/extract) $
  else p_var = thm_check_valid_name(strlowcase(p_var),strsplit(vsnames,' ',/extract),/include_all)
  
  if(size(p_var,/n_dim) eq 0 && p_var eq '') then return

  if arg_present(relpathnames_all) then begin
    downloadonly = 1
    no_download = 1
  end

  if lvl eq 'l1' then begin
    ;; default action for loading level 1 is to calibrate, so get support data
    if not keyword_set(get_support_data) then begin 
      get_support_data = 1
      delete_support_data = 1
    endif
endif

  ;if a calibrated datatype is requested the raw data types must be loaded
  if(lvl eq 'l1') then begin

    isect = ssl_set_intersection(datatype, valid_calibrated)

    if(size(isect, /n_dim) ne 0) then dt = ssl_set_union(datatype, valid_raw) $
    else dt = datatype
     
  endif else dt = datatype

  p = vsnames

  l = lvl

  vl = vlevels

    thm_load_xxx, sname = p_var, $
      datatype = dt, $
      trange = trange, $
      level = l, $
      verbose = verbose, $
      downloadonly = downloadonly, $
      cdf_data = cdf_data, $
      get_cdf_data = arg_present(cdf_data), $
      get_support_data = get_support_data, $
      delete_support_data = delete_support_data, $
      varnames = varnames, $
      valid_names = valid_names, $
      files = files, $
      vsnames = p, $
      type_sname = 'probe', $
      vdatatypes = strjoin(valid_datatypes, ' '), $
      file_vdatatypes = strjoin(congrid(valid_raw, n_elements(valid_datatypes), /center), ' '), $
      file_vL2datatypes = 'fft', $
      vlevels = vl     , $
      deflevel = deflevel, $
      version = 'v01', $
      progobj = progobj, $
      post_process_proc = 'thm_load_fft_post', $
      proc_type = type, $
      vtypes='raw calibrated', deftype = 'calibrated', $
      suffix = suffix, $
      relpathnames_all = relpathnames_all, $
      no_download = no_download, $
      varformat = varformat, $
      _extra = _extra

  ;retain datatype if single argument was passed in
  if n_elements(p_var) eq 1 then probe = p_var[0] else $
  probe=p_var

  ;delete unrequested data
  if(lvl eq 'l1' and not keyword_set(valid_names)) then begin
    
    valid_support = array_cross(valid_raw, valid_support)
    valid_support = reform(valid_support[0, *] + '_' + valid_support[1, *])
    valid_raw_support = [valid_raw,valid_support]
    
    list = ssl_set_complement(ssl_set_intersection(datatype, valid_raw_support),valid_raw_support)

    if(size(list, /n_dim) eq 0 && list eq -1L) then return

    var_list = array_cross(p_var, list)

    var_strings = reform('th' + var_list[0, *] + '_' + var_list[1, *] + suffix)
    
    for i = 0, n_elements(var_strings) -1L do begin
      if(tnames(var_strings[i]) ne '') then del_data, var_strings[i]
    endfor

  endif

  ;needed to return valid names correctly
  level = l

  ;retain datatype if single argument was passed in
  ;and no raw quantities were added
  if n_elements(dt) eq 1 then datatype = dt[0] else $
  datatype = dt

end
