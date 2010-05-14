;+
;Procedure: THM_LOAD_GMAG,
; thm_load_gmag, site = site, datatype = datatype, trange = trange, $
;                level = level, verbose = verbose, $
;                subtract_average = subtract_average, $
;                subtract_median = subtract_median, $
;                varname_out = varname_out, $
;                subtracted_values = subtracted_values, $
;                downloadonly = downloadonly, $
;                valid_names = valid_names
;keywords:
;  site  = Observatory name, example, thm_load_gmag, site = 'bmls', the
;          default is 'all', i.e., load all available stations . This
;          can be an array of strings, e.g., ['bmls', 'ccmv'] or a
;          single string delimited by spaces, e.g., 'bmls ccnv'
;  datatype = The type of data to be loaded, for this case, there is only
;          one option, the default value of 'mag', so this is a
;          placeholder should there be more that one data type. 'all'
;          can be passed in also, to get all variables.
;  TRANGE= (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded
;  level = the level of the data, the default is 'l2', or level-2
;          data. A string (e.g., 'l2') or an integer can be used. 'all'
;          can be passed in also, to get all levels.
;  /VERBOSE : set to output some useful info
;  /SUBTRACT_AVERAGE, if set, then the average values are subtracted
;                     from the loaded variables,
;  /SUBTRACT_MEDIAN, if set, then the median values are subtracted
;                     from the loaded variables,
;  varname_out= a string array containing the tplot variable names for
;               the loaded data, useful for the following keyword:
;  subtracted_values = returns N_elements(varname_out) by 3 array
;                      containing the average or median (or 0) values
;                      subtracted from the data.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;  no_download: use only files which are online locally.
;  relpathnames_all: named variable in which to return all files that are
;          required for specified timespan, probe, datatype, and level.
;          If present, no files will be downloaded, and no data will be loaded.
;  /valid_names, if set, then this will return the valid site, datatype
;                and/or level options in named variables, for example,
;
;                thm_load_gmag, site = xxx, /valid_names
;
;                will return the array of valid sites in the
;                variable xxx
; get_support_data = does nothing.  present only for consistency with other
;                load routines
;Example:
;   thm_load_gmag, site = 'bmls', trange =
;   ['2007-01-22/00:00:00','2007-01-24/00:00:00']
;
;Written by: Davin Larson,   Dec 2006
; 22-jan-2007, jmm, jimm@ssl.berkeley.edu rewrote argument list, added
; keywords,
; 1-feb-2007, jmm, added subtract_median, subtracted_value keywords
; 19-mar-2007, jmm, fixed the station list...
; 1-may-2009, jmm, removed greenland_data keyword, the greenland
;                  stations are now valid site names
; 3-jun-2009, jmm, added stations cdrt, crvr, gjoa, rbay, pang, tbdl
;                  MACCS data from Augsburg
; $LastChangedBy: pcruce $
; $LastChangedDate: 2010-03-01 16:24:31 -0800 (Mon, 01 Mar 2010) $
; $LastChangedRevision: 7391 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/ground/thm_load_gmag.pro $
;-

; processing for subracting average, median, and returning subracted value.
pro thm_load_gmag_post, sname=sitei, datatype=dtj, $
                        varcount = varcount, verbose = vb, $
                        subtract_average = subavg, $
                        subtract_median = subtract_median, $
                        varname_out = varname_out, $
                        subtracted_values = subtracted_values, $
                        suffix = suffix, _extra = _extra

;    varname = 'thg_'+lvlk+'_'+dtj+'_'+sitei
  If(keyword_set(suffix)) Then varname = 'thg_'+dtj+'_'+sitei+suffix $
  Else varname = 'thg_'+dtj+'_'+sitei

  options, /def, varname, ytitle = sitei,ysubtitle='B (nT)', $
           constant = 0.,labels=['bx','by','bz'],labflag=1
  if varcount Eq 0 then begin
     varname_out = varname
     subtracted_values = dblarr(1, 3) ;3 field components
     varcount = varcount+1
  endif else begin
     varname_out = [varname_out, varname]
     subtracted_values = [subtracted_values, dblarr(1, 3)]
     varcount = varcount+1
  endelse

  if keyword_set(subavg) Or keyword_set(subtract_median) then begin
     get_data, varname, data = d, alim = alim
     if keyword_set(d) then begin
        lng = struct_value(alim, 'cdf.vatt.station_longitude', default = !values.f_nan)
        lat = struct_value(alim, 'cdf.vatt.station_longitude', default = !values.f_nan)
;Note 'lat' and 'lng' could be used to subtract off a model dipole
;field
        svalue = average(d.y, 1, /double, $
                         ret_median = keyword_set(subtract_median))

        d.y -= (replicate(1, n_elements(d.x)) # svalue ) ; subtract the average value
        subtracted_values[varcount-1, *] = transpose(svalue)
        store_data, varname, data = d
     endif
     
  endif
  
  ;add suffient labeling to make identification and transformation of coordinate system possible
  get_data,varname,dlimit=dl
  str_element,dl,'data_att.coord_sys','hdz',/add
  
  str_element,dl,'cdf.vatt.station_latitude',lat,success=s
  if s then begin
    str_element,dl,'data_att.site_latitude',lat,/add
  endif
  
  str_element,dl,'cdf.vatt.station_longitude',lon,success=s
  if s then begin
    str_element,dl,'data_att.site_longitude',lon,/add
  endif
  
  store_data,varname,dlimit=dl

end

Pro thm_load_gmag, site = site, datatype = datatype, trange = trange, $
                   level = level, verbose = verbose, $
                   subtract_average = subavg, $
                   subtract_median = subtract_median, $
                   varname_out = varname_out, $
                   subtracted_values = subtracted_values, $
                   downloadonly = downloadonly, no_download=no_download, $
                   relpathnames_all=relpathnames_all, $
                   valid_names = valid_names, $
                   get_support_data=get_support_data, $
                   progobj = progobj, files=files, $
                   thm_only = thm_only, $
                   greenland_data = greenland_data, $
                   suffix=suffix
;                   _extra = _extra ;krb 5/4


;figure out sites here
  If(keyword_set(thm_only)) Then Begin
    vsnames = 'atha chbg ekat fsim fsmi fykn gako gbay '+$
              'gill inuv kapu kian kuuj mcgr nrsq pgeo '+$
              'pina rank snap snkq tpas whit yknf'
    vsnames_arr = strsplit(vsnames, ' ', /extract)
    vsnames_all = vsnames_arr
    vsnames_g_arr = ''
    If(is_string(site)) Then Begin
      message, 'Use of site keyword is incompatible with /thm_only keyword, setting site to ALL', /info
      site = 'all'
    Endif
  Endif Else Begin
    vsnames = 'arct atha bett bmls ccnv cdrt chbg cigo crvr drby eagl ekat fsim fsmi fykn '+ $
      'fyts gako gbay gill gjoa hlms homr hots iglo inuv kako kapu kian kuuj loys mcgr nain nrsq '+ $
      'pang pgeo pina pine pokr ptrs rank rbay rmus snap snkq swno tbdl tpas trap ukia whit yknf'
    vsnames_arr = strsplit(vsnames, ' ', /extract)
    vsnames_g = 'amk atu dmh dnb gdh kuv naq nrd sco skt stf svs thl umq upn'
    vsnames_g_arr = strsplit(vsnames_g, ' ', /extract)
    vsnames_all = [vsnames_arr, vsnames_g_arr]
  Endelse

  If(keyword_set(site)) Then site_in = site Else site_in = 'all'
  thm_sites = thm_check_valid_name(site_in, vsnames_arr, /ignore_case, /include_all, /no_warning)
  green_sites = thm_check_valid_name(site_in, vsnames_g_arr, /ignore_case, /include_all, /no_warning)

  If(keyword_set(valid_names)) Then Begin ;need to handle valid_names here too, jmm, 4-may-2009
    thm_load_greenland_gmag, site = gsites, datatype = datatype, $
      level = level, suffix=suffix, /valid_names
    thm_load_xxx, sname = tsites, datatype = datatype, $
      level = level, /valid_names, vsnames = vsnames, $
      type_sname = 'site', $
      vdatatypes = 'mag', $
      vlevels = 'l2', $
      suffix=suffix,$
      deflevel = 'l2'
    site = [gsites, tsites]
    Return
  Endif

  If(is_string(green_sites)) Then Begin ;go to greenland_gmag for these
    If(keyword_set(relpathnames_all)) Then Begin ;this is a mess...
      thm_load_greenland_gmag, site = green_sites, datatype = datatype, trange = trange, $
        level = level, verbose = verbose, subtract_average = subavg, $
        subtract_median = subtract_median, varname_out = varname_out, $
        subtracted_values = subtracted_values, downloadonly = downloadonly, $
        no_download = no_download, relpathnames_all = relpathnames_all, $
        valid_names = valid_names, get_support_data = get_support_data, $
        progobj = progobj, files = files, suffix=suffix
    Endif Else Begin
      thm_load_greenland_gmag, site = green_sites, datatype = datatype, trange = trange, $
        level = level, verbose = verbose, subtract_average = subavg, $
        subtract_median = subtract_median, varname_out = varname_out, $
        subtracted_values = subtracted_values, downloadonly = downloadonly, $
        no_download = no_download, valid_names = valid_names, $
        get_support_data = get_support_data, progobj = progobj, files = files, suffix=suffix
    Endelse
  Endif

  If(is_string(thm_sites)) Then Begin
    if arg_present(relpathnames_all) then begin
      downloadonly = 1
      no_download = 1
    end
    varcount = 0
    thm_load_xxx, sname = thm_sites, datatype = datatype, trange = trange, $
      level = level, verbose = verbose, downloadonly = downloadonly, $
      no_download = no_download, relpathnames_all = relpathnames_all, $
      cdf_data = cdf_data, get_cdf_data = arg_present(cdf_data), $
      varnames = varnames, valid_names = valid_names, files = files, $
      vsnames = vsnames, $
      type_sname = 'site', $
      vdatatypes = 'mag', $
      get_support_data = get_support_data, $
      vlevels = 'l2', $
      deflevel = 'l2', $
      version = 'v01', $
      post_process_proc = 'thm_load_gmag_post', $
      subtract_average = subavg, $
      subtract_median = subtract_median, $
      varname_out = varname_out, $
      subtracted_values = subtracted_values, $
      varcount = varcount, $
      progobj = progobj, $
      suffix=suffix,$
      _extra = _extra
  Endif
end

