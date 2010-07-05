;+
;Procedure: SERC_LOAD_GMAG_SAMPLE,
;  serc_load_gmag_sample, site = site, datatype = datatype, $
;                         trange = trange, verbose = verbose, $
;                         addmaster=addmaster, downloadonly = downloadonly
;Purpose:
;  This procedure allows you to download and plot MAGDAS magnetometer data on TDAS.
;  This is a sample code for IUGONET analysis software.
;
;Keywords:
;  site  = Observatory name.  For example, serc_load_gmag_sample, site = 'kuj'.
;          The default is 'all', i.e., load all available stations.
;  datatype = The type of data to be loaded.  In this sample
;             procedure, there is only one option, the default value of 'mag'.
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
;  serc_load_gmag_sample, site = 'kuj', trange = ['2007-01-22/00:00:00','2007-01-24/00:00:00']
;
;Code:
;  Shuji Abe
;
;ChangeLog:
;  7-April-2010, abeshu, test release.
;  8-April-2010, abeshu, minor update.
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
pro iug_load_gmag_serc, site = site, datatype = datatype, $
                           trange = trange, verbose = verbose, $
                           addmaster=addmaster, downloadonly = downloadonly


;*************************
;***** Keyword check *****
;*************************
; verbose
if ~keyword_set(verbose) then verbose=0

; data type
if ~keyword_set(datatype) then datatype='mag'

; validate datatype
vns=['mag']
if size(datatype,/type) eq 7 then begin
  datatype=thm_check_valid_name(datatype,vns, $
                                /ignore_case, /include_all, /no_warning)
  if datatype[0] eq '' then return
endif else begin
  message,'DATATYPE must be of string type.',/info
  return
endelse

; site
; list of sites
vsnames = 'anc asb cmd cst dav daw dvs eus her hob ilr kuj lmkw cq' + $
          'mgd mlb mnd mut onw prp ptk roc sma tir twv wad yap'
vsnames_all = strsplit(vsnames, ' ', /extract)

; validate sites
if(keyword_set(site)) then site_in = site else site_in = 'all'
magdas_sites = thm_check_valid_name(site_in, vsnames_all, $
                                    /ignore_case, /include_all, /no_warning)
if magdas_sites[0] eq '' then return

; number of valid sites
nsites = n_elements(magdas_sites)

; acknowlegment string (use for creating tplot vars)
acknowledgstring = 'Scientists who want to engage in collaboration with SERC ' + $
                   'should contact the project leader of MAGDAS/CPMN ' + $
                   'observations, Prof. Dr. K. Yumoto, Kyushu Univ., who will ' + $
                   'organize such collaborations.'


;*************************************************************************
;***** Download files, read data, and create tplot vars at each site *****
;*************************************************************************
;=================================
;=== Loop on downloading files ===
;=================================
; make remote path, local path, and download files
for i=0, nsites-1 do begin
  ; define file names
  pathformat= strupcase(strmid(magdas_sites[i],0,3)) + '/Min/YYYY/' + $
              strupcase(strmid(magdas_sites[i],0,3)) + '_MIN_YYYYMMDD0000.mgd'
  relpathnames = file_dailynames(file_format=pathformat, $
                                 trange=trange, addmaster=addmaster)

  ; define remote and local path information
  source = file_retrieve(/struct)
  source.verbose = verbose
  source.local_data_dir = root_data_dir() + 'iugonet/gmag/'
  source.remote_data_dir = 'http://magdas.serc.kyushu-u.ac.jp/whi/data/'

  ; download data
  local_files = file_retrieve(relpathnames, _extra=source)

  ; if downloadonly set, go to the top of this loop
  if keyword_set(downloadonly) then continue

  ;===================================
  ;=== Loop on reading MAGDAS data ===
  ;===================================
  for j=0,n_elements(local_files)-1 do begin
    file = local_files[j]

    if file_test(/regular,file) then begin
      dprint,'Loading MAGDAS data file: ', file
      fexist = 1
    endif else begin
      dprint,'MAGDAS data file ',file,' not found. Skipping'
      continue
    endelse

    ; create base time
    year = (strmid(relpathnames[j],21,4))
    month = (strmid(relpathnames[j],25,2))
    day = (strmid(relpathnames[j],27,2))
    basetime = time_double(year+'-'+month+'-'+day)

    ; open file
    openr, lun, file, /get_lun

    ; seek delimiter
    buf = bytarr(1, 1)
    while (buf ne 26) do begin
      readu, lun, buf
    end
    readu, lun, buf

    ; read data
    rdata = fltarr(7, 1440)
    readu, lun, rdata
    rdata = transpose(rdata)

    ; close file
    free_lun, lun

    ; append data and time index
    append_array, databuf, rdata
    append_array, timebuf, basetime + dindgen(1440)*60d
  endfor
  
  ;=======================================
  ;=== Loop on creating tplot variable ===
  ;=======================================
  if size(databuf,/type) eq 4 then begin
    ; tplot variable name
    tplot_name = 'serc_magdas_' + strlowcase(strmid(magdas_sites[i],0,3)) + '_mag'
  
    ; for bad data
    wbad = where(finite(databuf) gt 99999, nbad)
    if nbad gt 0 then databuf[wbad] = !values.f_nan

    ; default limit structure
    dlimit=create_struct('data_att',create_struct('acknowledgment', acknowledgstring, $
                                                  'PI_NAME', 'K. Yumoto'))

    ; store data to tplot variable
    databuf=databuf(*,0:3)
    store_data, tplot_name, data={x:timebuf, y:databuf}, dlimit=dlimit

    ; add options
    options, tplot_name, labels=['H','D','Z','F'] , $
                         ytitle = strupcase(strmid(magdas_sites[i],0,3)), $
                         ysubtitle = '[nT]', $
                         title = 'SERC MAGDAS magnetometer'
  endif

  ; clear data and time buffer
  databuf = 0
  timebuf = 0

; go to next site
endfor


;******************************************
;***** Display additional information *****
;******************************************

print,' 
print,'@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print,' Multicore CPU on AA
print,'  Pentium4 HT:  (�L�E�ցE�߁E�ցE`)
print,'  Core2Duo:     ( ^��^ )( ^��^ )
print,'  Core2Quad:    ( ^��^ )( ^��^ )�l( ^��^ )( ^��^ )
print,'  Core i7:
print,'          ����i �O�ցO�߁O�ցO�j������i �O�ցO�߁O�ցO�j��
print,'          ����i �O�ցO�߁O�ցO�j������i �O�ցO�߁O�ցO�j��
print,'@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print,' 

end
