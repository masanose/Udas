;+
;
;Name:
;iug_load_mf_pam
;
;Purpose:
;  Queries the Kyoto_RISH renkei2 servers for pameungpeuk data and loads data into
;  tplot format.
;
;Syntax:
; iug_load_mf_pam [ ,DATATYPE = string ]
;                 [ ,PARAMETERS = string]
;                 [ ,TRANGE = [min,max] ]
;                 [ ,FILENAMES = string scalar or array ]
;                 [ ,<and data keywords below> ]
;
;Keywords:
;  DATATYPE (I/O):
;    Set to 'thermosphere_wind'.  If not set, 'thermosphere_wind_pameungpeuk' is
;      assumed.  Returns cleaned input, or shows default.
;  PARAMETERS:
;    Set to the wind component data. If not set, 'zon_wind' is
;      assumed.  Returns cleaned input, or shows default.      
;  TRANGE (In):
;    Pass a time range a la TIME_STRING.PRO.
;  FILENAMES (In):
;    *PRESENTLY DISABLED* Pass user-defined file names (full paths to local data files).  These will
;      be read a la the RISH format, and the RISH server will not be queried.
;  VERBOSE (In): [1,...,5], Get more detailed (higher number) command line output.
;
;Code:
;  A. Shinbori, 12/05/2010.
;
;Modifications:
;  A. Shinbori, 13/05/2010.
;  A. Shinbori, 06/06/2010.
;
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_mf_pam, datatype=datatype, parameters=parameters, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
if ~keyword_set(verbose) then verbose=2

;*****************************************
;Load 'thermosphere_wind' data by default:
;*****************************************
if ~keyword_set(datatype) then datatype='pameungpeuk'

;**********************************
;Load 'parameters' data by default:
;**********************************
if ~keyword_set(parameters) then parameters='zonal_wind_pam'

;*******************
;Validate datatypes:
;*******************
vns = datatype
if size(datatype,/type) eq 7 then begin
  datatype=thm_check_valid_name(datatype,vns,/ignore_case,/include_all,/no_warning)
  if datatype[0] eq '' then return
endif else begin
  message,'DATATYPE must be of string type.',/info
  return
endelse

;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'If you acquire pameungpeuk data, we ask that you' $
+ 'acknowledge us in your use of the data. This may be done by' $
+ 'including text such as pameungpeuk data provided by Research Institute' $
+ 'for Sustainable Humanosphere of Kyoto University. We would also' $
+ 'appreciate receiving a copy of the relevant publications.'

;==================================================================
;Download files, read data, and create tplot vars at each component
;==================================================================
;******************************************************************
;Loop on downloading files
;******************************************************************
;Get timespan, define FILE_NAMES, and load data:
;===============================================

if ~size(fns,/type) then begin

    ;Get files for ith component:
    ;***************************
    file_names = file_dailynames( $
      file_format='YYYY/pameungpeuk.'+$
      'YYYYMMDD',trange=trange,times=times,/unique)+'.sswma'
    ;            
    ;Define FILE_RETRIEVE structure:
    ;===============================
    source = file_retrieve(/struct)
    source.verbose=verbose
    source.local_data_dir =  root_data_dir() + 'iugonet/rish/pameungpeuk/'
    ;source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/ear/data/data/ver02.0212/'
    
    ;Get files and local paths, and concatenate local paths:
    ;=======================================================
    local_paths=file_retrieve(file_names,_extra=source)
    local_paths_all = ~(~size(local_paths_all,/type)) ? $
      [local_paths_all, local_paths] : local_paths
  if ~(~size(local_paths_all,/type)) then local_paths=local_paths_all
endif else file_names=fns

;Read the files:
;===============

altitude = fltarr(36)
height = fltarr(36)
zon_wind_data = fltarr(1,36)
mer_wind_data = fltarr(1,36)
ver_wind_data = fltarr(1,36)
zon_wind = fltarr(1,36)
mer_wind = fltarr(1,36)
ver_wind = fltarr(1,36)
time = dblarr(1)
ear_time = dblarr(1)

;Loop on files: 
;==============
 for j=0,n_elements(local_paths)-1 do begin
    file= local_paths[j]
    if file_test(/regular,file) then  dprint,'Loading pameungpeuk file: ',file $
    else begin
         dprint,'pameungpeuk file ',file,' not found. Skipping'
         continue
    endelse
    openr,lun,file,/get_lun 
    ;
    ;Determine year, month, day:
    ;===========================
    year = strmid(file,42,4)
    month = strmid(file,46,2)
    day = strmid(file,48,2)
    ;
    ;readdata:
    ;=========
      readf,lun
    ;
    ; read file header:
    ;==================
    file_header_bytes = assoc(lun, lonarr(1))
    file_header_names = ["File magic number (0x23110001)", $
                       "No.of SSWMA records in this file (0 or more)", $
                       "Offset to start of first record from start of file", $
                       "Unit ID or serial number"]
    sswma_records = file_header_bytes[1]
    for i = 0, 3 do begin
      print, file_header_names[i]
      print, file_header_bytes[i]
    endfor
    offset = 48L
    ;
    ; read record header:
    ;==================== 
  m=0 
        
  for k = 1L, sswma_records[0] do begin
    ;
    ; read UNIX time:
    ;================
      epoch_time_stamp = assoc(lun, lonarr(1), offset+16)
      millisecond_time_stamp = assoc(lun, lonarr(1), offset+20)
      print, epoch_time_stamp[0], millisecond_time_stamp[0]
    ;
    ; From UNIX time to real time:
    ;============================= 
      epoch_time = float(epoch_time_stamp[0]) + float(millisecond_time_stamp[0])/1000      
      time[0]= epoch_time
     ;
     ;Calcurate time:
     ;===============
     ; time[m] = time_double(year+'-'+month+'-'+day+'/'+string(hour)+':'+string(minute)+':'+string(second))
     ;

      offset += 116
      record_header_bytes3 = assoc(lun, lonarr(1), offset)
      number_of_range_gates_sampled = record_header_bytes3[0]
      offset += 132

      ; Results Header:
      ;================
      n=0
      for l = 1L, number_of_range_gates_sampled[0] do begin
          record_header_bytes10 = assoc(lun, lonarr(1), offset+0)
          altitude[n] =float(record_header_bytes10[0])/1000;Range [km]
          record_header_bytes12 = assoc(lun, fltarr(1), offset+8)
          a = record_header_bytes12[0];zonal_wind
          wbad = where(a eq -9999,nbad)
          if nbad gt 0 then a[wbad] = !values.f_nan
          zon_wind_data[m,n]=a
          record_header_bytes13 = assoc(lun, fltarr(1), offset+12)
          b= record_header_bytes13[0];meridional_wind
          wbad = where(b eq -9999,nbad)
          if nbad gt 0 then b[wbad] = !values.f_nan
          mer_wind_data[m,n] = b
          record_header_bytes14 = assoc(lun, fltarr(1), offset+16)
          c= record_header_bytes14[0];vertical_wind
          wbad = where(c eq -9999,nbad)
          if nbad gt 0 then c[wbad] = !values.f_nan
          ver_wind_data[m,n] = c
          offset += 144
          n=n+1
       endfor
        height = altitude
        ;
        ;Append data of time:
        ;====================
         append_array, ear_time, time
        ;
        ;Append data of wind velocity:
        ;=============================
         append_array, zon_wind, zon_wind_data
         append_array, mer_wind, mer_wind_data
         append_array, ver_wind, ver_wind_data
 endfor
    free_lun,lun
endfor

;Replace data array:
;===================
number = n_elements(ear_time)

for i=0,number-2 do begin
  ear_time[i] = ear_time[i+1]
  zon_wind[i,*] = zon_wind[i+1,*]
  mer_wind[i,*] = mer_wind[i+1,*]
  ver_wind[i,*] = ver_wind[i+1,*]
  for l=0,35 do begin
    a=zon_wind[i,l]
    wbad = where(a eq 0,nbad)
    if nbad gt 0 then begin
      a[wbad] = !values.f_nan
      zon_wind[i,l]=a
    endif
    b=mer_wind[i,l]
    wbad = where(b eq 0,nbad)
    if nbad gt 0 then begin
      b[wbad] = !values.f_nan
      mer_wind[i,l]=b
    endif
    c=ver_wind[i,l]
    wbad = where(c eq 0,nbad)
    if nbad gt 0 then begin
      c[wbad] = !values.f_nan
      ver_wind[i,l]=c
    endif
  endfor
endfor


;******************************
;Store data in TPLOT variables:
;******************************
acknowledgstring = ''
print, parameters
;Store data of zonal and meridional component:
;=============================================
if  parameters eq 'zonal_wind_pam' then begin
    dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
    store_data,'zonal_wind_pam',data={x:ear_time, y:zon_wind, v:height},dlimit=dlimit
endif
if  parameters eq 'meridional_wind_pam' then begin
    dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
    store_data,'meridional_wind_pam',data={x:ear_time, y:mer_wind, v:height},dlimit=dlimit
endif
if  parameters eq 'vertical_wind_pam' then begin
    dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
    store_data,'vertical_wind_pam',data={x:ear_time, y:ver_wind, v:height},dlimit=dlimit
endif

; add options
options, parameters, 'spec', 1

print,'**********************************************************************************
print,'Data loading is successful!!'
print,'**********************************************************************************

end

