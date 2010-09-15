;+
;Procedure: IUG_LOAD_GMAG_WDC
; iug_load_gmag_wdc, site=site, $
;                        trange=trange, $
;                        resolution = resolution, $
;                        level=level, $
;                        verbose=verbose, $
;                        addmaster=addmaster, $
;                        downloadonly=downloadonly, $
;                        no_download=no_download
;
;Purpose:
;  Loading geomag data in WDC format from WDC for Geomag Kyoto.
;
;Keywords:
;  site  = Station ABB code or name of geomagnetic index.
;          Ex1) iug_load_gmag_wdc, site = 'kak', ...
;          Ex2) iug_load_gmag_wdc, site = ['dst', 'ae'], ...
;  trange= (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full month, a full
;          month's data is loaded
;  reolution = Time resolution of the data: 'min' or 'hour',
;          default set to 'min' for AE index and geomag data.
;  level = The level of the data, the default is 'final' for geomag data.
;          For AE and Dst index, default is ['final', 'provsional']
;  /verbose : set to output some useful info
;  /addmaster, if set, then times = [!values.d_nan, times]
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;  no_download: use only files which are online locally.
;
;Example:
;   iug_load_gmag_wdc, site = 'kak', resolution = 'min', trange =
;   ['2007-01-22/00:00:00','2007-01-24/00:00:00']
;
;Notes:
;  '/no_download' option is necessary for this procedure.
;  At WDC Kyoto, data service for TDAS will be open in Spring, 2012.
;  Now support only for AE Dst SYM/ASY KAK data.
;
;Written by:  Daiki Yoshida,  Aug 2010
;Last Updated:  Daiki Yoshida,  Sep 10, 2010
; 
;-

pro iug_load_gmag_wdc, site=site, $
                       trange=trange, $
                       resolution = resolution, $
                       level=level, $
                       verbose=verbose, $
                       addmaster=addmaster, $
                       downloadonly=downloadonly, $
                       no_download=no_download


  ; validate site settings
  vsnames = 'kak asy sym ae dst'  ;ToDo: add all WDC stations
  vsnames_all = strsplit(vsnames, ' ', /extract)
  if(keyword_set(site)) then site_in = site else site_in = 'all'
  wdc_sites = thm_check_valid_name(site_in, vsnames_all, $
    /ignore_case, /include_all, /no_warning)
  if wdc_sites[0] eq '' then return
  nsites = n_elements(wdc_sites)


  for i=0, nsites-1 do begin

     if(~keyword_set(level)) then begin
        if strlowcase(wdc_sites[i]) eq 'dst' or $
           strlowcase(wdc_sites[i]) eq 'ae' then begin
           level_in = ['final','provisional']
        endif else begin
           level_in = 'final'
        endelse
     endif else level_in = level

     if strlowcase(wdc_sites[i]) eq 'sym' or $
        strlowcase(wdc_sites[i]) eq 'asy' then begin
        resolution = 'min'
     endif else if strlowcase(wdc_sites[i]) eq 'dst' then begin
        resolution = 'hour'
     endif
     if(~keyword_set(resolution)) then resolution = 'min'


     for j=0, n_elements(level_in)-1 do begin
        if resolution eq 'hour' or $
           resolution eq 'hr' then begin
           iug_load_gmag_wdc_wdchr, $
              site = wdc_sites[i], $
              trange = trange, $
              level = level_in[j], $
              verbose = verbose, $
              addmaster = addmaster, $
              downloadonly = downloadonly, $
              no_download = no_download, $
              _extra = _extra
        endif else if resolution eq 'min' then begin
           iug_load_gmag_wdc_wdcmin, $
              site = wdc_sites[i], $
              trange = trange, $
              level = level_in[j], $
              verbose = verbose, $
              addmaster = addmaster, $
              downloadonly = downloadonly, $
              no_download = no_download, $
              _extra = _extra
        endif
     endfor

  end

end
