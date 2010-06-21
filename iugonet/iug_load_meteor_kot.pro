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
;  A. Shinbori, 06/06/2010.
;  A. Shinbori, 21/06/2010.
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
if ~keyword_set(parameters) then parameters='zon_wind'

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
height = fltarr(20)
zon_wind_data = fltarr(1,20)
mer_wind_data = fltarr(1,20)
zon_wind = fltarr(1,20)
mer_wind = fltarr(1,20)
zon_thermal_data = fltarr(1,20)
mer_thermal_data = fltarr(1,20)
zon_thermal = fltarr(1,20)
mer_thermal = fltarr(1,20)
time = dblarr(1)
time1 = dblarr(2)
kt_time = dblarr(1)
data=fltarr(7)
n=0
 
 for i=0,19 do begin
   height[i]=72+2*i   
 endfor
 
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
         ;====convert time from LT to UT   
         time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+hour+':'+minute) $
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(6)+':'+string(41)+':'+string(12))
         if (n mod 2 eq 0) then time1(0)= time
         if (n mod 2 eq 1) then time1(1)= time        
         if (72 eq alt) then begin
            zon_wind_data(0,0)= data(0)
            mer_wind_data(0,0)= data(1)
            zon_thermal_data(0,0)= data(2)
            mer_thermal_data(0,0)= data(3)   
         endif
         if (74 eq alt) then begin
            zon_wind_data(0,1)= data(0)
            mer_wind_data(0,1)= data(1)
            zon_thermal_data(0,1)= data(2)
            mer_thermal_data(0,1)= data(3) 
         endif 
         if (76 eq alt) then begin
            zon_wind_data(0,2)= data(0)
            mer_wind_data(0,2)= data(1)
            zon_thermal_data(0,2)= data(2)
            mer_thermal_data(0,2)= data(3)               
         endif
         if (78 eq alt) then begin
            zon_wind_data(0,3)= data(0)
            mer_wind_data(0,3)= data(1)
            zon_thermal_data(0,3)= data(2)
            mer_thermal_data(0,3)= data(3)             
         endif 
         if (80 eq alt) then begin
            zon_wind_data(0,4)= data(0)
            mer_wind_data(0,4)= data(1)
            zon_thermal_data(0,4)= data(2)
            mer_thermal_data(0,4)= data(3)               
         endif
         if (82 eq alt) then begin
            zon_wind_data(0,5)= data(0)
            mer_wind_data(0,5)= data(1)
            zon_thermal_data(0,5)= data(2)
            mer_thermal_data(0,5)= data(3) 
         endif 
         if (84 eq alt) then begin
            zon_wind_data(0,6)= data(0)
            mer_wind_data(0,6)= data(1)
            zon_thermal_data(0,6)= data(2)
            mer_thermal_data(0,6)= data(3)   
         endif
         if (86 eq alt) then begin
            zon_wind_data(0,7)= data(0)
            mer_wind_data(0,7)= data(1)
            zon_thermal_data(0,7)= data(2)
            mer_thermal_data(0,7)= data(3) 
         endif 
         if (88 eq alt) then begin
            zon_wind_data(0,8)= data(0)
            mer_wind_data(0,8)= data(1)
            zon_thermal_data(0,8)= data(2)
            mer_thermal_data(0,8)= data(3)   
         endif
         if (90 eq alt) then begin
            zon_wind_data(0,9)= data(0)
            mer_wind_data(0,9)= data(1)
            zon_thermal_data(0,9)= data(2)
            mer_thermal_data(0,9)= data(3) 
         endif
         if (92 eq alt) then begin
            zon_wind_data(0,10)= data(0)
            mer_wind_data(0,10)= data(1)
            zon_thermal_data(0,10)= data(2)
            mer_thermal_data(0,10)= data(3)   
         endif
         if (94 eq alt) then begin
            zon_wind_data(0,11)= data(0)
            mer_wind_data(0,11)= data(1)
            zon_thermal_data(0,11)= data(2)
            mer_thermal_data(0,11)= data(3) 
         endif 
         if (96 eq alt) then begin
            zon_wind_data(0,12)= data(0)
            mer_wind_data(0,12)= data(1)
            zon_thermal_data(0,12)= data(2)
            mer_thermal_data(0,12)= data(3)   
         endif
         if (98 eq alt) then begin
            zon_wind_data(0,13)= data(0)
            mer_wind_data(0,13)= data(1)
            zon_thermal_data(0,13)= data(2)
            mer_thermal_data(0,13)= data(3) 
         endif 
         if (100 eq alt) then begin
            zon_wind_data(0,14)= data(0)
            mer_wind_data(0,14)= data(1)
            zon_thermal_data(0,14)= data(2)
            mer_thermal_data(0,14)= data(3) 
         endif 
         if (102 eq alt) then begin
            zon_wind_data(0,15)= data(0)
            mer_wind_data(0,15)= data(1)
            zon_thermal_data(0,15)= data(2)
            mer_thermal_data(0,15)= data(3)   
         endif
         if (104 eq alt) then begin
            zon_wind_data(0,16)= data(0)
            mer_wind_data(0,16)= data(1)
            zon_thermal_data(0,16)= data(2)
            mer_thermal_data(0,16)= data(3) 
         endif  
         if (106 eq alt) then begin
            zon_wind_data(0,17)= data(0)
            mer_wind_data(0,17)= data(1)
            zon_thermal_data(0,17)= data(2)
            mer_thermal_data(0,17)= data(3) 
         endif 
         if (108 eq alt) then begin
            zon_wind_data(0,18)= data(0)
            mer_wind_data(0,18)= data(1)
            zon_thermal_data(0,18)= data(2)
            mer_thermal_data(0,18)= data(3)   
         endif
         if (110 eq alt) then begin
            zon_wind_data(0,19)= data(0)
            mer_wind_data(0,19)= data(1)
            zon_thermal_data(0,19)= data(2)
            mer_thermal_data(0,19)= data(3) 
         endif
         data(0)=0
         data(1)=0
         data(2)=0
         data(3)=0
         if n ge 1 then begin
         if (time1(1)-time1(0) ne 0) then begin          
            append_array, se_time, time
            append_array, zon_wind, zon_wind_data
            append_array, mer_wind, mer_wind_data
            append_array, zon_thermal, zon_thermal_data
            append_array, mer_thermal, mer_thermal_data
            for i=0, 19 do begin
             zon_wind_data(0,i)=0
             mer_wind_data(0,i)=0
             zon_thermal_data(0,i)=0
             mer_thermal_data(0,i)=0
            endfor
         endif
         endif
         n=n+1        
         continue
      endif
    endwhile 
    free_lun,lun
endfor

;Replace data array:
;===================
number = n_elements(se_time)

for i=0,number-2 do begin
  se_time[i] = se_time[i+1]
  zon_wind[i,*] = zon_wind[i+1,*]
  mer_wind[i,*] = mer_wind[i+1,*]
  zon_thermal[i,*] = zon_thermal[i+1,*]
  mer_thermal[i,*] = mer_thermal[i+1,*]  
  for l=0,19 do begin
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
     c=zon_thermal[i,l]
    wbad = where(c eq 0,nbad)
    if nbad gt 0 then begin
      c[wbad] = !values.f_nan
      zon_thermal[i,l]=c
    endif
    d=mer_thermal[i,l]
    wbad = where(d eq 0,nbad)
    if nbad gt 0 then begin
      d[wbad] = !values.f_nan
      mer_thermal[i,l]=d
    endif
  endfor
endfor
;******************************
;Store data in TPLOT variables:
;******************************
acknowledgstring = ''

;Store data of zonal and meridional component:
;=============================================
if  parameters eq 'zonal_wind_ktb' then begin
    dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
    store_data,'zonal_wind_ktb',data={x:kt_time, y:zon_wind, v:height},dlimit=dlimit
endif
if  parameters eq 'meridional_wind_ktb' then begin
    dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
    store_data,'meridional_wind_ktb',data={x:kt_time, y:mer_wind, v:height},dlimit=dlimit
endif

;Store data of zonal and meridional thermal speed component:
;===========================================================
if  parameters eq 'zonal_thermal_speed_ktb' then begin
    dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
    store_data,'zonal_thermal_speed_ktb',data={x:kt_time, y:zon_thermal, v:height},dlimit=dlimit
endif
if  parameters eq 'meridional_thermal_speed_ktb' then begin
    dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
    store_data,'meridional_thermal_speed_ktb',data={x:kt_time, y:mer_thermal, v:height},dlimit=dlimit
endif

; add options
options, parameters, 'spec', 1

print,'**********************************************************************************
print, 'Data loading is successful!!'
print,'**********************************************************************************

end

