
;+
;Procedure: THM_LOAD_FIT
;
;Purpose:  Loads THEMIS FIT data
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
;  CDF_DATA: named variable in which to return cdf data structure: only works
;          for a single spacecraft and datafile name.
;  VARNAMES: names of variables to load from cdf: default is all.
;  /GET_SUPPORT_DATA: load support_data variables as well as data variables
;                      into tplot variables.
;  /DOWNLOADONLY: download file but don't read it.
;  /valid_names, if set, then this routine will return the valid probe, datatype
;          and/or level options in named variables supplied as
;          arguments to the corresponding keywords.
;  files   named varible for output of pathnames of local files.
;  /no_cal if set will not include boom shortening or Ex offset in output
;  /VERBOSE  set to output some useful info
;Example:
;   thg_load_fit,/get_suppport_data,probe=['a', 'b']
;Modifications:
;  J. McFadden passed through the NO_CAL kw to THM_CAL_FIT.PRO, WMF, 6/27/2008.
;Notes:
;
; $LastChangedBy: bckerr $
; $LastChangedDate: 2009-08-18 18:19:09 -0700 (Tue, 18 Aug 2009) $
; $LastChangedRevision: 6604 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/spacecraft/fields/thm_load_fit.pro $
;-




pro thm_load_fit_post, sname=probe, datatype=dt, level=level, $
                       tplotnames=tplotnames, $
                       suffix=suffix, proc_type=proc_type, coord=coord, $
                       delete_support_data=delete_support_data,sigma=sigma,$
                       _extra=_extra,no_cal=no_cal

if not keyword_set(coord) then coord='dsl'
for l=0, n_elements(tplotnames)-1 do begin
     tplot_var = tplotnames[l]
     dtl = strmid(tplot_var, 4, 3)
     get_data, tplot_var, data=d_str, limit=l_str, dlimit=dl_str
     if data_type(dl_str) eq 8 && dl_str.cdf.vatt.var_type eq 'data' $
     then begin
         if strmatch(level, 'l1') then begin
             data_att = { data_type:'raw'}
             str_element, dl_str, 'data_att', data_att, /add
             store_data, tplot_var, data=d_str, limit=l_str, dlimit=dl_str
         endif else if strmatch(level, 'l2') then begin
             data_att = { data_type:'calibrated'}
             str_element, dl_str, 'data_att', data_att, /add
             store_data, tplot_var, data=d_str, limit=l_str, dlimit=dl_str
         endif
     endif
 endfor

;better way to set units and coordinates:
if (level eq 'l2') then begin
    thm_new_units, tplotnames
    thm_new_coords, tplotnames
endif

if (level eq 'l1' ) and (~keyword_set(proc_type) || strmatch(proc_type, 'calibrated')) then begin
  thm_cal_fit,/verbose,probe=probe,datatype=dt,in_suf=suffix,out_suf=suffix,coord=coord,no_cal=no_cal
endif

if keyword_set(delete_support_data) then begin
    if size(dt, /n_dim) eq 0 then dt = strsplit(dt, ' ', /extract)

    for i = 0, n_elements(dt)-1L do begin
        if tnames('th'+probe+'_'+dt[i]+'_hed') ne '' then del_data, 'th'+probe+'_'+dt[i]+'_hed'
    endfor
endif

end



pro thm_load_fit,probe=probe, datatype=datatype, trange=trange, $
                 level=level, verbose=verbose, downloadonly=downloadonly, $
                 cdf_data=cdf_data,get_support_data=get_support_data, $
                 relpathnames_all=relpathnames_all,nodownload=nodownload,$
                 varnames=varnames, valid_names = valid_names, files=files, $
                 progobj=progobj,type=type, suffix=suffix,coord=coord,sigma=sigma,no_cal=no_cal


  if arg_present(relpathnames_all) then begin
     downloadonly=1
     no_download=1
  end


valid_probes = ['a','b','c','d','e']

valid_raw = ['fit','fit_code','fit_npts']

valid_calibrated = ['fgs','efs','fit_efit','fit_bfit','efs_0','efs_dot0','fgs_sigma','efs_sigma']

valid_datatypes = ssl_set_union(valid_raw,valid_calibrated)

;validate inputs
if not keyword_set(datatype) then datatype = valid_datatypes $
else datatype = thm_check_valid_name(strlowcase(datatype),valid_datatypes,/include_all)

if not keyword_set(probe) then probe=valid_probes

vlevels='l1 l2'
;valid data levels
vlevels = strsplit(vlevels, ' ', /extract)
;   deflevel='l2'
deflevel = 'l1'
; parse out data level
if keyword_set(deflevel) then lvl = deflevel else lvl = 'l1'
if n_elements(level) gt 0 then begin
   if size(level, /type) Eq 7 then begin
      If(level[0] Ne '') Then lvl = strcompress(strlowcase(level), /remove_all)
   endif else lvl = 'l'+strcompress(string(fix(level)), /remove_all)
endif
lvls = thm_check_valid_name(strlowcase(lvl), vlevels)
if not keyword_set(lvls) then return
if n_elements(lvls) gt 1 then begin
   message, /info, level = -1, 'only one value may be specified for level'
   return
endif

if lvls eq 'l2' and keyword_set(type) then begin
   print,"Type keyword not valid for level 2 data."
   return
endif
if not keyword_set(suffix) then suffix = ''
if not keyword_set(type) && lvl eq 'l1' then type='calibrated'
if not keyword_set(coord) then coord='dsl'

if lvls eq 'l1'then begin
   ;; default action for loading level 1 is to calibrate
   if ~keyword_set(type) || strmatch(type, 'calibrated') then begin
      ;; we're calibrating, so make sure we get support data
      if not keyword_set(get_support_data) then begin
         get_support_data = 1
         delete_support_data = 1
      endif
   endif
endif


if not keyword_set(datatype) then begin
  if (lvls eq 'l1') && type eq 'calibrated' then begin
     dt = ['fit','fgs','efs','fgs_sigma','efs_sigma']
  endif
  if (lvls eq 'l1') && type eq 'raw' then dt = ['fit']
  if (lvls eq 'l2') then dt = ['fgs','efs*']
endif else begin
  if (lvls eq 'l1') then begin
      if n_elements(datatype) gt 1 then begin
          dt=['fit',datatype]
      endif else if size(/dimensions,datatype) eq 0 then begin
          dt='fit '+datatype
      endif else dt=['fit',datatype]
  endif else dt=datatype
endelse

;If there is 'fit' or 'sigma' in the datatype, add 'none' to the
;coordinate system, if it's not there for level 2 data, jmm,
;15-jul-2008
if (lvls eq 'l2') then begin
    p1 = strmatch(dt, '*fit')
    p2 = strmatch(dt, '*sigma')
    If(total(p1) Gt 0 Or total(p2) Gt 0) Then Begin
        none_test = strmatch(strlowcase(coord), 'none')
        If(total(none_test) Eq 0) Then Begin
            If(n_elements(coord) Eq 1) Then coord=coord+' none' $
            Else coord = [coord, 'none']
        Endif
    Endif
endif

thm_load_xxx,sname=probe, datatype=dt, trange=trange, $
  level=level, verbose=verbose, downloadonly=downloadonly, $
  cdf_data=cdf_data,get_cdf_data=arg_present(cdf_data), $
  get_support_data=get_support_data, $
  varnames=varnames, valid_names = valid_names, files=files, $
  vsnames = 'a b c d e', $
  type_sname = 'probe', no_download=no_download, $
  vdatatypes = 'fit fgs efs fit_efit fit_bfit efs_0 efs_dot0 fgs_sigma efs_sigma', $
  file_vdatatypes='fit fit fit fit fit fit fit fit fit',$
  vlevels = 'l1 l2', $
  vL2coord = 'dsl gse gsm none', $
  deflevel = deflevel,proc_type=type, $
  post_process_proc='thm_load_fit_post',$
  version = 'v01', suffix=suffix,$
  progobj=progobj,delete_support_data=delete_support_data,$
  coord=coord,sigma=sigma,$
  relpathnames_all=relpathnames_all,no_cal=no_cal,$
  _extra = _extra

;fix units
if((tnames('th?_fgs'+suffix))[0] ne '') then begin
    options,'th?_fgs'+suffix,/def,ysubtitle=''
endif

  ;delete unrequested data
  ;the following code is copied over
  ;from thm_load_fft
  ;
  if(lvl eq 'l1' and not keyword_set(valid_names)) then begin


     ;the lines below help deal with variation in datatypes variable
     if size(datatype,/n_dim) eq 0 && datatype[0] eq '' then begin
        list = ssl_set_complement(-1L,valid_datatypes)
     endif else begin
        list = ssl_set_complement(datatype,valid_datatypes)
     endelse

    if(size(list, /n_dim) eq 0 && list eq -1L) then return

    var_list = array_cross(probe, list)

    var_strings = reform('th' + var_list[0, *] + '_' + var_list[1, *] + suffix)

    for i = 0, n_elements(var_strings) -1L do begin
      tvars = tnames(var_strings[i])
      if (n_elements(tvars) eq 1) && array_equal(tvars, '',/no_type) then continue
      del_data, var_strings[i]
    endfor

  endif

end