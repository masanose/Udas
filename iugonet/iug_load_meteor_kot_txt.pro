;+
;
;Name:
;iug_load_meteor_kot_txt
;
;Purpose:
;  Queries the Kyoto_RISH renkei2 servers for ACII data of the Kototabang site 
;  and loads data intotplot format.
;
;Syntax:
; iug_load_meteor_kot_txt [ ,DATATYPE = string ]
;                         [ ,PARAMETERS = string]
;                         [ ,TRANGE = [min,max] ]
;                         [ ,FILENAMES = string scalar or array ]
;                         [ ,<and data keywords below> ]
;
;Keywords:
;  DATATYPE (I/O):
;    Set to 'thermosphere_wind'.  If not set, 'kototabang' is
;      assumed.  Returns cleaned input, or shows default.
;  PARAMETERS:
;    Set to the wind data at several altitudes=''. If not set, 'zon_wind' is
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
;  A. Shinbori, 04/07/2010.
;  A. Shinbori, 07/08/2010.
;  
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_meteor_kot_txt, datatype=datatype, parameters = parameters, trange=trange, verbose=verbose

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
<<<<<<< HEAD
if ~keyword_set(parameters) then parameters='zon_wind'
=======
if ~keyword_set(parameters) then parameters='zon_wind_ktb'
>>>>>>> ea6ad748563fc214c86e5d52c951acf9d2b3095c

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
acknowledgstring = 'If you acquire meteor radar data, we ask that you' $
+ 'acknowledge us in your use of the data. This may be done by' $
+ 'including text such as the kototabang MW data provided by Research Institute' $
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
    source.local_data_dir =  root_data_dir() + 'iugonet/rish/mw/kototabang/h2km_t60min00/'
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
height = fltarr(21)
zon_wind_data = fltarr(1,21)
mer_wind_data = fltarr(1,21)
zon_thermal_data = fltarr(1,21)
mer_thermal_data = fltarr(1,21)
meteor_num_data = fltarr(1,21)
time = dblarr(1)
time1 = dblarr(2)
n=0
<<<<<<< HEAD
kt_time=0
=======
kot_time=0
>>>>>>> ea6ad748563fc214c86e5d52c951acf9d2b3095c
zon_wind=0
mer_wind=0
zon_thermal=0
mer_thermal=0
<<<<<<< HEAD
meteor_num=0
=======
meteor_num=0 
>>>>>>> ea6ad748563fc214c86e5d52c951acf9d2b3095c

 
;Loop on files (zonal component): 
;================================
 for j=0,n_elements(local_paths)-1 do begin
    file= local_paths[j]
    if file_test(/regular,file) then  dprint,'Loading Kototabang file: ',file $
    else begin
         dprint,'Kototabang file ',file,' not found. Skipping'
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
         
         for k=0,6 do begin
<<<<<<< HEAD
           a = float(data[k])
           wbad = where(a eq 999,nbad)
=======
           a = float(data[k])
           wbad = where(a gt 100 || a lt -100,nbad)
           wbad = where(a eq 999,nbad)
>>>>>>> ea6ad748563fc214c86e5d52c951acf9d2b3095c
           if nbad gt 0 then a[wbad] = !values.f_nan
           data[k]=a
         endfor
         ;
         ;Append data of time and U, V components at determined altitude:
         ;=============================================================== 
         ;====convert time from LT to UT   
         time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+hour+':'+minute) $
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(7)+':'+string(0)+':'+string(0))
         if (n mod 2 eq 0) then time1(0)= time
         if (n mod 2 eq 1) then time1(1)= time        
       for g=0,20 do begin         
         if (70+g*2 eq alt) then begin
            zon_wind_data(0,g)= data(0)
            mer_wind_data(0,g)= data(1)
            zon_thermal_data(0,g)= data(2)
            mer_thermal_data(0,g)= data(3)
            meteor_num_data(0,g)= data(4)
         endif
       endfor
         data(0)=0
         data(1)=0
         data(2)=0
         data(3)=0
         data(4)=0
         if n ge 1 then begin
         if (time1(1)-time1(0) ne 0) then begin          
<<<<<<< HEAD
            append_array, kt_time, time
=======
            append_array, kot_time, time
>>>>>>> ea6ad748563fc214c86e5d52c951acf9d2b3095c
            append_array, zon_wind, zon_wind_data
            append_array, mer_wind, mer_wind_data
            append_array, zon_thermal, zon_thermal_data
            append_array, mer_thermal, mer_thermal_data
            append_array, meteor_num, meteor_num_data
            for i=0, 20 do begin
             zon_wind_data(0,i)=!values.f_nan
             mer_wind_data(0,i)=!values.f_nan
             zon_thermal_data(0,i)=!values.f_nan
             mer_thermal_data(0,i)=!values.f_nan
             meteor_num_data(0,i)=!values.f_nan
            endfor
         endif
         endif
         n=n+1 
       continue       
      endif
    endwhile 
    free_lun,lun
endfor

for g=0,20 do begin         
  height[g]=70+g*2 
endfor
;******************************
;Store data in TPLOT variables:
;******************************
acknowledgstring = ''

if time ne 0 then begin

<<<<<<< HEAD
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
;Store data of meteor trace number:
;==================================
   if  parameters eq 'meteor_num_ktb' then begin
       dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
       store_data,'meteor_num_ktb',data={x:kt_time, y:meteor_num, v:height},dlimit=dlimit
   endif
   
; add options
   options, parameters, 'spec', 1
endif 

;Clear time and data buffer:
kt_time=0
=======
;Store data of kototabang wind data:
;===================================
  dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'T. Tsuda'))
  store_data,'zonal_wind_ktb',data={x:kot_time, y:zon_wind, v:height},dlimit=dlimit
  options,'zonal_wind_ktb',ytitle='Kototabang!CHeight [km]',ztitle='Zonal wind [m/s]'
  store_data,'meridional_wind_ktb',data={x:kot_time, y:mer_wind, v:height},dlimit=dlimit
  options,'meridional_wind_ktb',ytitle='Kototabang!CHeight [km]',ztitle='Meridional wind [m/s]'
  store_data,'zonal_thermal_speed_ktb',data={x:kot_time, y:zon_thermal, v:height},dlimit=dlimit
  options,'zonal_thermal_speed_ktb',ytitle='Kototabang!CHeight [km]',ztitle='Sigma zonal wind [m/s]'
  store_data,'meridional_thermal_speed_ktb',data={x:kot_time, y:mer_thermal, v:height},dlimit=dlimit
  options,'meridional_thermal_speed_ktb',ytitle='Kototabang!CHeight [km]',ztitle='Sigma meridional wind [m/s]'
  store_data,'meteor_num_ktb',data={x:kot_time, y:meteor_num, v:height},dlimit=dlimit
  options,'meteor_num_ktb',ytitle='Kototabang!CHeight [km]',ztitle='munber'
endif

; add options
   options, ['zonal_wind_ktb','meridional_wind_ktb',$
            'zonal_thermal_speed_ktb','meridional_thermal_speed_ktb','meteor_num_ktb'], 'spec', 1

; add options of setting lanels
  options,'zonal_wind_ktb', labels='MWR ktb'
  options,'meridional_wind_ktb', labels='MWR ktb'
  options,'zonal_thermal_speed_ktb', labels='MWR ktb'
  options,'meridional_thermal_speed_ktb', labels='MWR ktb'
  options,'meteor_num_ktb', labels='MWR ktb'

;Clear time and data buffer:
kot_time=0
>>>>>>> ea6ad748563fc214c86e5d52c951acf9d2b3095c
zon_wind=0
mer_wind=0
zon_thermal=0
mer_thermal=0
meteor_num=0 
<<<<<<< HEAD
          
=======
  
>>>>>>> ea6ad748563fc214c86e5d52c951acf9d2b3095c
print,'**********************************************************************************
print, 'Data loading is successful!!'
print,'**********************************************************************************

end

