;+
;Procedure: TOHOKUU_LOAD_ONW_PC3_SAMPLE,
;  TohokuU_load_onw_pc3_sample, site = site, datatype = datatype, $
;                         trange = trange, verbose = verbose, $
;                         addmaster=addmaster, downloadonly = downloadonly
;Purpose:
;  This procedure allows you to download and plot ONAGAWA Pc3 INDEX data on TDAS.
;  This is a sample code for IUGONET analysis software.
;
;Keywords:
;  site  = Observatory name. Only 'onw' is allowed.
;  datatype = The type of data to be loaded.  In this sample
;             procedure, there is only one option, the default value of 'pc3i'.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /verbose, if set, then output some useful info
;  /addmaster, if set, then times = [!values.d_nan, times]
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;Example:
;  TohokuU_load_onw_pc3_sample,  trange = ['2007-01-22/00:00:00','2007-01-24/00:00:00']
;
;Code:
;  Shuji Abe, revised by M. Kagitani
;
;ChangeLog:
;  7-April-2010, abeshu, test release.
;  8-April-2010, abeshu, minor update.
;  25-April-2010, revised for ONAGAWA Pc3 index by M. Kagitani
;
;Acknowledgment:
;
;
; $LastChangedBy: $
; $LastChangedDate: $
; $LastChangedRevision: $
; $URL: $
;-
;############################################################
;  Procedure:  FILE_MONTHLYNAMES_ONW
;  Author: Davin Larson, revised by M. Kagitani on Apr. 2010
;  GIT TEST

function file_monthlynames_onw,dir,prefix,suffix,trange=trange, $
    hour_res=hour_res,  $
    unique=unique,  $
    file_format=file_format, $
    dir_format=dir_format, $
    times = times, $
    resolution =res, $
    yyyy_mm_dir=yyyy_mm_dir, $
    yeardir=yeardir,$
    addmaster=addmaster


if not keyword_set(dir) then dir=''
if not keyword_set(prefix) then prefix=''
if not keyword_set(suffix) then suffix=''

;sep = path_sep()
sep = '/'       ; '\' is not needed even with WINDOWS!

if keyword_set(yeardir) then      dir_format='YYYY'+sep
if keyword_set(YYYY_MM_DIR) then  dir_format='YYYY'+sep+'MM'+sep

if not keyword_set(res) then res = 24l*3600*31
if keyword_set(hour_res) then begin   ; one hour resolution
   res = 86400l
   if not keyword_set(file_format) then file_format = 'YYYYMMDD'
endif

if not keyword_set(file_format) then file_format = 'YYYYMM'


tr = timerange(trange)

mmtr = floor( tr / res + [0d,.999d] )
n = (mmtr[1]-mmtr[0])  > 1

times = (dindgen(n) + mmtr[0]) * res
if keyword_set(addmaster) then times = [!values.d_nan,times]

dates = time_string( times , tformat=file_format)

if keyword_set(dir_format) then  datedir = time_string(times,tformat=dir_format)  else datedir=''

files = dir + datedir + prefix + dates + suffix

if keyword_set(unique) then begin
   s = sort(files)
   u = uniq(files[s])
   files = files[s[u]]
   times= times[s[u]]
endif


return,files

end
;############################################################


;**************************
;***** Main Procedure *****
;**************************
pro iug_load_gmag_pc3, site=site, datatype = datatype, $
                           trange = trange, verbose = verbose, $
                           addmaster=addmaster, downloadonly = downloadonly

;*************************
;***** Keyword check *****
;*************************
; verbose
if ~keyword_set(verbose) then verbose=0

; data type
if ~keyword_set(datatype) then datatype='pc3'

; validate datatype
vns=['Pc3']
if size(datatype,/type) eq 7 then begin
  datatype=thm_check_valid_name(datatype,vns, $
                                /ignore_case, /include_all, /no_warning)
  if datatype[0] eq '' then return
endif else begin
  message,'DATATYPE must be of string type.',/info
  return
endelse

site = 'onw'
; list of sites
vsnames = 'onw'
vsnames_all = strsplit(vsnames, ' ', /extract)

; validate sites
if(keyword_set(site)) then site_in = site else site_in = 'all'
magdas_sites = thm_check_valid_name(site_in, vsnames_all, $
                                    /ignore_case, /include_all, /no_warning)
if magdas_sites[0] eq '' then return

; number of valid sites
nsites = n_elements(magdas_sites)

; acknowlegment string (use for creating tplot vars)
acknowledgstring = 'Please contact Prof. Shoichi Okano.'


;*************************************************************************
;***** Download files, read data, and create tplot vars at each site *****
;*************************************************************************
;=================================
;=== Loop on downloading files ===
;=================================
; make remote path, local path, and download files

for i=0, nsites-1 do begin
  ; define file names
  pathformat= strupcase(strmid(magdas_sites[i],0,3)) + '/' + $
              strupcase(strmid(magdas_sites[i],0,3)) + '_pc3_YYYYMM.dat'
  ;relpathnames = file_dailynames(file_format=pathformat, $
  ;                               trange=trange, addmaster=addmaster)
  relpathnames = file_monthlynames_onw(file_format=pathformat, $
                                 trange=trange, addmaster=addmaster)

  ; define remote and local path information
  source = file_retrieve(/struct)
  source.verbose = verbose
  source.local_data_dir = root_data_dir() + 'geom_indices/pc3/'
  source.remote_data_dir = 'http://pparc.gp.tohoku.ac.jp/whi/data/'

  ; download data
  local_files = file_retrieve(relpathnames, _extra=source)

  ; if downloadonly set, go to the top of this loop
  if keyword_set(downloadonly) then continue

  ;===================================
  ;=== Loop on reading MAGDAS data ===
  ;===================================
  print,(local_files)


  for j=0,n_elements(local_files)-1 do begin
    file = local_files[j]

    if file_test(/regular,file) then begin
      dprint,'Loading ONAGAWA Pc3 index data file: ', file
      fexist = 1
    endif else begin
      dprint,'ONAGAWA Pc3 index data file ',file,' not found. Skipping'
      continue
    endelse

    ; create base time
    year = (strmid(relpathnames[j],strlen(relpathnames[j])-10,4))
    month = (strmid(relpathnames[j],strlen(relpathnames[j])-6,2))
    ;year = (strmid(relpathnames[j],21,4))
    ;month = (strmid(relpathnames[j],25,2))
    ;day = (strmid(relpathnames[j],27,2))
    day = '01'
    basetime = time_double(year+'-'+month+'-'+day)


    ;===================================
    buf = fltarr(6)
    line=''
    yy=0L & mm=0L & dd=0L & doy=0L & pc3i=0. & pc3p=0.

    openr,lun,file, /get_lun
    readf,lun,line ; file header
    readf,lun,line ; file header
    readf,lun,line ; file header
    readf,lun,yy,mm,dd,doy,pc3i,pc3p
    while not eof(lun) do begin
      readf,lun,buf
      yy=[yy,buf[0]]
      mm=[mm,buf[1]]
      dd=[dd,buf[2]]
      doy=[doy,buf[3]]
      pc3i=[pc3i,buf[4]]
      pc3p=[pc3p,buf[5]]
    endwhile
    free_lun,lun

    append_array,databuf,pc3i
    append_array,timebuf,basetime + dindgen(31)*86400d
    ;===================================
;stop
;    ; open file
;    openr, lun, file, /get_lun
;
;    ; seek delimiter
;    buf = bytarr(1, 1)
;    while (buf ne 26) do begin
;      readu, lun, buf
;    end
;    readu, lun, buf
;
;    ; read data
;    rdata = fltarr(7, 1440)
;    readu, lun, rdata
;    rdata = transpose(rdata)
;
;    ; close file
;    free_lun, lun
;
;    ; append data and time index
;    append_array, databuf, rdata
;    append_array, timebuf, basetime + dindgen(1440)*60d

  endfor

  ;=======================================
  ;=== Loop on creating tplot variable ===
  ;=======================================
  if size(databuf,/type) eq 4 then begin
    ; tplot variable name
    ;tplot_name = 'onw_pc3_' + strlowcase(strmid(magdas_sites[i],0,3)) + '_pc3'
    tplot_name = 'onw_pc3'

    ; for bad data
    wbad = where(finite(databuf) gt 99999, nbad)
    if nbad gt 0 then databuf[wbad] = !values.f_nan

    wbad = where(databuf lt 0, nbad)
    if nbad gt 0 then databuf[wbad] = !values.f_nan

    ; default limit structure
    dlimit=create_struct('data_att',create_struct('acknowledgment', acknowledgstring, $
                                                  'PI_NAME', 'S. Okano'))

    ; store data to tplot variable
    ;databuf=databuf(*,0:3)
    store_data, tplot_name, data={x:timebuf, y:databuf}, dlimit=dlimit

    ; add options
    options, tplot_name, labels=['Pc3i'] , $
                         ytitle = 'Onw', $
                         ysubtitle = '', $
                         title = 'Onagawa Pc3 index'
  endif

  ; clear data and time buffer
  databuf = 0
  timebuf = 0

; go to next site
endfor

end




