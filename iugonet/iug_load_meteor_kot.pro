;+
;
;Name:
;iug_load_meteor_kot
;
;Purpose:
;  Queries the Kyoto_RISH renkei2 servers for EAR data and loads data into
;  tplot format.
;
;Syntax:
; iug_load_meteor_kot [ ,DATATYPE = string ]
;                     [ ,PARAMETERS = string]
;                     [ ,TRANGE = [min,max] ]
;                     [ ,FILENAMES = string scalar or array ]
;                     [ ,<and data keywords below> ]
;
;Keywords:
;  DATATYPE (I/O):
;    Set to 'thermosphere_wind'.  If not set, 'thermo_wind_kototabang' is
;      assumed.  Returns cleaned input, or shows default.
;  PARAMETERS:
;    Set to the wind data at several altitudes=''. If not set, 'zon_wind_kt76' is
;      assumed.  Returns cleaned input, or shows default.      
;  TRANGE (In):
;    Pass a time range a la TIME_STRING.PRO.
;  FILENAMES (In):
;    *PRESENTLY DISABLED* Pass user-defined file names (full paths to local data files).  These will
;      be read a la the RISH format, and the RISH server will not be queried.
;  VERBOSE (In): [1,...,5], Get more detailed (higher number) command line output.
;
;Code:
;  A. Shinbori, 13/05/2010.
;
;Modifications:
;  
;  
;
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_meteor_kot, datatype=datatype, parameters = parameters, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
;verbose
if ~keyword_set(verbose) then verbose=2

;*****************************************
;Load 'thermosphere_wind' data by default:
;*****************************************
if ~keyword_set(datatype) then datatype='kototabang'

;********************************
;Load 'parameters' data by default:
;********************************
if ~keyword_set(parameters) then parameters='zon_wind_kt76'

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
acknowledgstring = 'If you acquire EAR data, we ask that you' $
+ 'acknowledge us in your use of the data. This may be done by' $
+ 'including text such as EAR data provided by Research Institute' $
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
;get_timespan,t    ;<===== manual mode =====;
if ~size(fns,/type) then begin

    ;Get files for ith component:
    ;***************************
    ;=========Manual mode====================================================
    ;file_names = file_dailynames( $
    ;  file_format='YYYY/Wk'+$
    ;  'YYYYMM',trange=t,times=times,/unique)+'.h2t60'
    ;========================================================================
    ;=========GUI mode=======================================================
    file_names = file_dailynames( $
      file_format='YYYY/Wk'+$
      'YYYYMM',trange=trange,times=times,/unique)+'.h2t60'
    ;=========================================================================
    ;        
    ;Define FILE_RETRIEVE structure:
    ;===============================
    source = file_retrieve(/struct)
    source.verbose=verbose
    source.local_data_dir =  root_data_dir() + 'iugonet/rish/kototabang/h2km_t60min00/'
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
s=''
time76=0
time80=0
time84=0
time88=0
time92=0
time96=0
time100=0
time104=0
time108=0
wind_u76=0
wind_u80=0
wind_u84=0
wind_u88=0
wind_u92=0
wind_u96=0
wind_u100=0
wind_u104=0
wind_u108=0
wind_v76=0
wind_v80=0
wind_v84=0
wind_v88=0
wind_v92=0
wind_v96=0
wind_v100=0
wind_v104=0
wind_v108=0
data=fltarr(7)

;Loop on files (zonal component): 
;================================
 for j=0,n_elements(local_paths)-1 do begin
    file= local_paths[j]
    if file_test(/regular,file) then  dprint,'Loading kototabang file: ',file $
    else begin
         dprint,'kototabang file ',file,' not found. Skipping'
         continue
    endelse
    openr,lun,file,/get_lun    
    ;
    ;Loop on readdata:
    ;=================
    while(not eof(lun)) do begin
      readf,lun,s
      ok=1
      if strmid(s,0,1) eq '[' then ok=0
      if ok && keyword_set(s) then begin
         dprint,s ,dlevel=5
              
         ;calcurate time:
         ;===============
         year = fix(strmid(s,0,2))+2000
         day_of_year = fix(strmid(s,2,3))
         doy_to_month_date, year, day_of_year, month, day
         hour = strmid(s,5,2)
         minute = strmid(s,7,2)
         ;get altitude data:
         ;=================
         alt = fix(strmid(s,9,3))
         ;get data of U, V, sigma-u, sigma-v, N-of-m, int1, int2:
         ;=======================================================
         data1 = strmid(s,12,55)
         data = float(strsplit(data1, ' ', /extract))
         ;
         ;Append data of time and U, V components at determined altitude:
         ;===============================================================   
         if (76 eq alt) then begin
            ;====convert time from LT to UT         
            time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+hour+':'+minute) $
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(6)+':'+string(41)+':'+string(12))
            append_array, time76, time
            append_array, wind_u76, data(0)
            append_array, wind_v76, data(1)
         endif
         if (80 eq alt) then begin
            ;====convert time from LT to UT         
            time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+hour+':'+minute) $
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(6)+':'+string(41)+':'+string(12))
            append_array, time80, time
            append_array, wind_u80, data(0)
            append_array, wind_v80, data(1)
         endif
         if (84 eq alt) then begin
            ;====convert time from LT to UT         
            time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+hour+':'+minute) $
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(6)+':'+string(41)+':'+string(12))
            append_array, time84, time
            append_array, wind_u84, data(0)
            append_array, wind_v84, data(1)
         endif
         if (88 eq alt) then begin
            ;====convert time from LT to UT         
            time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+hour+':'+minute) $
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(6)+':'+string(41)+':'+string(12))
            append_array, time88, time
            append_array, wind_u88, data(0)
            append_array, wind_v88, data(1)
         endif   
         if (92 eq alt) then begin
           ;====convert time from LT to UT         
            time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+hour+':'+minute) $
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(6)+':'+string(41)+':'+string(12))
            append_array, time92, time
            append_array, wind_u92, data(0)
            append_array, wind_v92, data(1)
         endif
         if (96 eq alt) then begin
            ;====convert time from LT to UT         
            time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+hour+':'+minute) $
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(6)+':'+string(41)+':'+string(12))
            append_array, time96, time
            append_array, wind_u96, data(0)
            append_array, wind_v96, data(1)
         endif
         if (100 eq alt) then begin
            ;====convert time from LT to UT         
            time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+hour+':'+minute) $
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(6)+':'+string(41)+':'+string(12))
            append_array, time100, time
            append_array, wind_u100, data(0)
            append_array, wind_v100, data(1)
         endif
         if (104 eq alt) then begin
            ;====convert time from LT to UT         
            time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+hour+':'+minute) $
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(6)+':'+string(41)+':'+string(12))
            append_array, time104, time
            append_array, wind_u104, data(0)
            append_array, wind_v104, data(1)
         endif
         if (108 eq alt) then begin
            ;====convert time from LT to UT         
            time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+hour+':'+minute) $
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(6)+':'+string(41)+':'+string(12))
            append_array, time108, time
            append_array, wind_u108, data(0)
            append_array, wind_v108, data(1)
         endif         
         continue
      endif
    endwhile 
    free_lun,lun
endfor

;******************************
;Store data in TPLOT variables:
;******************************
acknowledgstring = ''
print, time76
;Store data of zonal and merdional component at 76 km:
;=====================================================
if keyword_set(wind_u76) then begin
  wbad = where(wind_u76 eq 9999,nbad)
  if nbad gt 0 then wind_u76[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'zon_wind_kt76',data={x:time76, y:wind_u76},dlimit=dlimit 
endif
if keyword_set(wind_v76) then begin
  wbad = where(wind_v76 eq 9999,nbad)
  if nbad gt 0 then wind_v76[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'mer_wind_kt76',data={x:time76, y:wind_v76},dlimit=dlimit
endif

;Store data of zonal and merdional component at 80 km:
;=====================================================
if keyword_set(wind_u80) then begin
  wbad = where(wind_u80 eq 9999,nbad)
  if nbad gt 0 then wind_u80[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'zon_wind_kt80',data={x:time80, y:wind_u80},dlimit=dlimit
endif
if keyword_set(wind_v80) then begin
  wbad = where(wind_v80 eq 9999,nbad)
  if nbad gt 0 then wind_v80[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'mer_wind_kt80',data={x:time80, y:wind_v80},dlimit=dlimit
endif

;Store data of zonal and merdional component at 84 km:
;=====================================================
if keyword_set(wind_u84) then begin
  wbad = where(wind_u84 eq 9999,nbad)
  if nbad gt 0 then wind_u84[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'zon_wind_kt84',data={x:time84, y:wind_u84},dlimit=dlimit
endif
if keyword_set(wind_v84) then begin
  wbad = where(wind_v84 eq 9999,nbad)
  if nbad gt 0 then wind_v84[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'mer_wind_kt84',data={x:time84, y:wind_v84},dlimit=dlimit
endif

;Store data of zonal and merdional component at 88 km:
;=====================================================
if keyword_set(wind_u88) then begin
  wbad = where(wind_u88 eq 9999,nbad)
  if nbad gt 0 then wind_u88[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'zon_wind_kt88',data={x:time88, y:wind_u88},dlimit=dlimit 
endif
if keyword_set(wind_v88) then begin
  wbad = where(wind_v88 eq 9999,nbad)
  if nbad gt 0 then wind_v88[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'mer_wind_kt88',data={x:time88, y:wind_v88},dlimit=dlimit
endif

;Store data of zonal and merdional component at 92 km:
;=====================================================
if keyword_set(wind_u92) then begin
  wbad = where(wind_u92 eq 9999,nbad)
  if nbad gt 0 then wind_u92[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'zon_wind_kt92',data={x:time92, y:wind_u92},dlimit=dlimit 
endif
if keyword_set(wind_v92) then begin
  wbad = where(wind_v92 eq 9999,nbad)
  if nbad gt 0 then wind_v92[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'mer_wind_kt92',data={x:time92, y:wind_v92},dlimit=dlimit
endif

;Store data of zonal and merdional component at 96 km:
;=====================================================
if keyword_set(wind_u96) then begin
  wbad = where(wind_u96 eq 9999,nbad)
  if nbad gt 0 then wind_u96[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'zon_wind_kt96',data={x:time96, y:wind_u96},dlimit=dlimit
endif
if keyword_set(wind_v96) then begin
  wbad = where(wind_v96 eq 9999,nbad)
  if nbad gt 0 then wind_v96[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'mer_wind_kt96',data={x:time96, y:wind_v96},dlimit=dlimit
endif

;Store data of zonal and merdional component at 100 km:
;=====================================================
if keyword_set(wind_u100) then begin
  wbad = where(wind_u100 eq 9999,nbad)
  if nbad gt 0 then wind_u100[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'zon_wind_kt100',data={x:time100, y:wind_u100},dlimit=dlimit
endif
if keyword_set(wind_v100) then begin
  wbad = where(wind_v100 eq 9999,nbad)
  if nbad gt 0 then wind_v100[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'mer_wind_kt100',data={x:time100, y:wind_v100},dlimit=dlimit
endif

;Store data of zonal and merdional component at 104 km:
;=====================================================
if keyword_set(wind_u104) then begin
  wbad = where(wind_u104 eq 9999,nbad)
  if nbad gt 0 then wind_u104[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'zon_wind_kt104',data={x:time104, y:wind_u104},dlimit=dlimit
endif
if keyword_set(wind_v104) then begin
  wbad = where(wind_v104 eq 9999,nbad)
  if nbad gt 0 then wind_v104[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'mer_wind_kt104',data={x:time104, y:wind_v104},dlimit=dlimit
endif

;Store data of zonal and merdional component at 108 km:
;=====================================================
if keyword_set(wind_u108) then begin
  wbad = where(wind_u108 eq 9999,nbad)
  if nbad gt 0 then wind_u108[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'zon_wind_kt108',data={x:time108, y:wind_u108},dlimit=dlimit
endif
if keyword_set(wind_v108) then begin
  wbad = where(wind_v108 eq 9999,nbad)
  if nbad gt 0 then wind_v108[wbad] = !values.f_nan
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
  store_data,'mer_wind_kt108',data={x:time108, y:wind_v108},dlimit=dlimit
endif

print,'**********************************************************************************
print, 'Data loading is successful!!'
print,'**********************************************************************************

end

