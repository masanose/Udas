;+
;Procedure: IUG_LOAD_MF_NIPR,
;  iug_load_mf_nipr, site = site, trange = trange, $
;                    verbose = verbose, $
;Purpose:
;  This procedure allows you to download and plot MF radar data of
;  NIPR on TDAS. This is a sample code for IUGONET analysis software.
;
;Keywords:
;  site  = Observatory name.  For example, iug_load_mf_nipr, site = 'syo'.
;          The default is 'syo'.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /verbose, if set, then output some useful info
;
;Example:
;  iug_load_mf_nipr, site = 'syo', trange = ['2007-01-22/00:00:00','2007-01-24/00:00:00']
;
;Code:
;  Yoshimasa Tanaka
;
;ChangeLog:
;  11-June-2010, ytanaka, test release.
;
;Acknowledgment:
;
;
; $LastChangedBy: $
; $LastChangedDate: $
; $LastChangedRevision: $
; $URL: $
;-

;**************************
;***** Procedure name *****
;**************************
pro iug_load_mf_nipr, site = site, trange = trange, $
                      verbose = verbose

;*************************
;***** Keyword check *****
;*************************
; verbose
if ~keyword_set(verbose) then verbose=0

; site
; list of sites
vsnames = 'syo'
vsnames_all = strsplit(vsnames, ' ', /extract)

; validate sites
if(keyword_set(site)) then site_in = site else site_in = 'syo'
mf_sites = thm_check_valid_name(site_in, vsnames_all, $
                                /ignore_case, /include_all, /no_warning)
if mf_sites[0] eq '' then return

; number of valid sites
nsites = n_elements(mf_sites)

; acknowlegment string (use for creating tplot vars)
acknowledgstring = 'Please contact the Principal Investigator, Dr. Masaki Tsutsumi,'+$
                   'National Institute of Polar Research before using this data at '+$
                   'any publications and/or presentations.'


;*************************************************************************
;***** Download files, read data, and create tplot vars at each site *****
;*************************************************************************
;=================================
;=== Loop on downloading files ===
;=================================
; make remote path, local path, and download files
for i=0, nsites-1 do begin
  ; define file names
  pathformat= strupcase(strmid(mf_sites[i],0,3)) + '/YYYY/' + $
              'YYYYMMDD.txt'
  relpathnames = file_dailynames(file_format=pathformat, $
                                 trange=trange)

  ; define remote and local path information
  source = file_retrieve(/struct)
  source.verbose = verbose
  source.local_data_dir = root_data_dir() + 'iugonet/nipr/mf/'
  source.remote_data_dir = 'http://polaris.nipr.ac.jp/~iugonet/data/mf/'

  ; download data
  local_files = file_retrieve(relpathnames, _extra=source)

  ;=====================================
  ;=== Loop on reading MF radar data ===
  ;=====================================
  ; initialize data and time buffer
  datanum_buf = 0
  zon_vel_buf = 0
  mer_vel_buf = 0
  zon_std_buf = 0
  mer_std_buf = 0
  time_buf = 0

  for j=0,n_elements(local_files)-1 do begin
    file = local_files[j]

    if file_test(/regular,file) then begin
      dprint,'Loading MF radar data file: ', file
      fexist = 1
    endif else begin
      dprint,'MF radar data file ',file,' not found. Skipping'
      continue
    endelse

    ; create base time
    year = (strmid(relpathnames[j],strlen(relpathnames[j])-12,4))
    month = (strmid(relpathnames[j],strlen(relpathnames[j])-8,2))
    day = (strmid(relpathnames[j],strlen(relpathnames[j])-6,2))
    basetime = time_double(year+'-'+month+'-'+day)

    ; open file
    openr, lun, file, /get_lun

    ; read data
    ncol=6 & height_bin=2. & height_l=50. & height_h=100.
    height = height_bin * indgen((height_h - height_l) / height_bin + 1) + $
	     height_l
    rdata = fltarr(ncol)
    dumm = 0.
    datanum = fltarr(n_elements(height), 24)    
    zon_vel = fltarr(n_elements(height), 24)
    mer_vel = fltarr(n_elements(height), 24)
    zon_std = fltarr(n_elements(height), 24)
    mer_std = fltarr(n_elements(height), 24)
    for ihr = 0, 23 do begin
        readf, lun, dumm
        for ihgt = 0, n_elements(height) - 1 do begin
            readf, lun, rdata
            datanum(ihgt, ihr) = rdata(1)
            zon_vel(ihgt, ihr) = rdata(2)
            zon_std(ihgt, ihr) = rdata(3)
            mer_vel(ihgt, ihr) = rdata(4)
            mer_std(ihgt, ihr) = rdata(5)
        endfor
    endfor

    ; close file
    free_lun, lun

    datanum = transpose(datanum)
    zon_vel = transpose(zon_vel)
    zon_std = transpose(zon_std)
    mer_vel = transpose(mer_vel)
    mer_std = transpose(mer_std)

    ; append data and time index
    append_array, datanum_buf, datanum
    append_array, zon_vel_buf, zon_vel
    append_array, mer_vel_buf, mer_vel
    append_array, zon_std_buf, zon_std
    append_array, mer_std_buf, mer_std
    append_array, time_buf, basetime + dindgen(24)*3600d
  endfor

  ;=======================================
  ;=== Loop on creating tplot variable ===
  ;=======================================
  if size(zon_vel_buf,/type) eq 4 then begin
    ; for bad data
    wbad = where(datanum_buf le 0, nbad)
    if nbad gt 0 then begin
      zon_vel_buf[wbad] = !values.f_nan
      mer_vel_buf[wbad] = !values.f_nan
      zon_std_buf[wbad] = !values.f_nan
      mer_std_buf[wbad] = !values.f_nan
    endif

    ; tplot variable name
    prefix = 'mf_'+ strlowcase(strmid(mf_sites[i],0,3))

    ; add options
    options, prefix+'_zonal_vel', $
      ytitle = strupcase(strmid(mf_sites[i],0,3))+'!CHeight', $
      ysubtitle = '[km]', ztitle='Zonal Wind [m/s]', $
      title = 'MF radar'
    options, prefix+'_merid_vel', $
      ytitle = strupcase(strmid(mf_sites[i],0,3))+'!CHeight', $
      ysubtitle = '[km]', ztitle='Meridional Wind [m/s]', $
      title = 'MF radar'
    options, prefix+'_zonal_std', $
      ytitle = strupcase(strmid(mf_sites[i],0,3))+'!CHeight', $
      ysubtitle = '[km]', ztitle='Std of Zonal Wind [m/s]', $
      title = 'MF radar'
    options, prefix+'_merid_std', $
      ytitle = strupcase(strmid(mf_sites[i],0,3))+'!CHeight', $
      ysubtitle = '[km]', ztitle='Std of Meridional Wind [m/s]', $
      title = 'MF radar'

    ; default limit structure
    dlimit=create_struct('data_att',create_struct('acknowledgment', acknowledgstring, $
                                                  'PI_NAME', 'M. Tsutsumi'))

    ; store data to tplot variable
    store_data, prefix+'_zonal_vel', data={x:time_buf, y:zon_vel_buf, v:height}, $
      dlimit=dlimit
    store_data, prefix+'_merid_vel', data={x:time_buf, y:mer_vel_buf, v:height}, $
      dlimit=dlimit
    store_data, prefix+'_zonal_std', data={x:time_buf, y:zon_std_buf, v:height}, $
      dlimit=dlimit
    store_data, prefix+'_merid_std', data={x:time_buf, y:mer_std_buf, v:height}, $
      dlimit=dlimit

    zlim, prefix+'_zonal_vel', -100, 100, 0
    zlim, prefix+'_merid_vel', -80, 80, 0

    options, prefix+'_zonal_vel', 'spec', 1
    options, prefix+'_merid_vel', 'spec', 1
    options, prefix+'_zonal_std', 'spec', 1
    options, prefix+'_merid_std', 'spec', 1

  endif

; go to next site
endfor

end
