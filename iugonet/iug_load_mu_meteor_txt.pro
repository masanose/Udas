;+
;
;Name:
;iug_load_mu_meteor_txt
;
;Purpose:
;  Queries the Kyoto_RISH servers for ACII data of the MU radar 
;  and loads data intotplot format.
;
;Syntax:
; iug_load_mu_meteor_txt, datatype = datatype, parameter=parameter, $
;                          downloadonly=downloadonly, trange=trange, verbose=verbose
;
;Keywords:
;  datatype = Observation data type. For example, iug_load_mu_meteor_txt, datatype = 'mw'.
;            The default is 'mw'. 
;  parameter = parameter name of MU meteor special obervation data.  
;          For example, iug_load_mu_meteor_txt, parameter = 'uwnd'.
;          The default is 'all', i.e., load all available parameters.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;Code:
;  A. Shinbori, 19/09/2010.
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


pro iug_load_mu_meteor_txt, datatype=datatype, parameters = parameters, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
;verbose
if ~keyword_set(verbose) then verbose=2

;**************************
;Load 'mw' data by default:
;**************************
if ~keyword_set(datatype) then datatype='mw'

;********************************
;Load 'parameters' data by default:
;********************************
if ~keyword_set(parameters) then parameters='uwnd'

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
acknowledgstring = 'If you acquire MU data, we ask that you' $
+ 'acknowledge us in your use of the data. This may be done by' $
+ 'including text such as MU data provided by Research Institute' $
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
      file_format='YYYY/Wmur'+$
      'YYYY',trange=trange,times=times,/unique)+'.h1t60'
    ;=========================================================================
    ;        
    ;Define FILE_RETRIEVE structure:
    ;===============================
    source = file_retrieve(/struct)
    source.verbose=verbose
    source.local_data_dir =  root_data_dir() + 'iugonet/rish/misc/sgk/mu/meteor_wind/'
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
height = fltarr(31)
zon_wind_data = fltarr(1,31)
mer_wind_data = fltarr(1,31)
zon_thermal_data = fltarr(1,31)
mer_thermal_data = fltarr(1,31)
meteor_num_data = fltarr(1,31)
time = dblarr(1)
time1 = dblarr(2)

n=0
mu_time=0
zon_wind=0
mer_wind=0
zon_thermal=0
mer_thermal=0
meteor_num=0
 
 for i=0,19 do begin
   height[i]=72+2*i   
 endfor
 
;Loop on files (zonal component): 
;================================
 for j=0,n_elements(local_paths)-1 do begin
    file= local_paths[j]
    if file_test(/regular,file) then  dprint,'Loading MU-mw file: ',file $
    else begin
         dprint,'MU-mw file ',file,' not found. Skipping'
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
         yy = fix(strmid(s,0,2))
         if yy lt 10 then year = fix(strmid(s,0,2))+2000
         if yy gt 10 then year = fix(strmid(s,0,2))+1900
         day_of_year = fix(strmid(s,2,3))
         doy_to_month_date, year, day_of_year, month, day
         hour = strmid(s,5,2)
         minute = strmid(s,7,2)
         ;get altitude data:
         ;=================
         alt = fix(strmid(s,9,3))
         ;get data of U, V, sigma-u, sigma-v, N-of-m:
         ;=======================================================
         data1 = strmid(s,12,55)
         data = float(strsplit(data1, ' ', /extract))
         
         for k=0,4 do begin
           a = float(data[k])
           wbad = where(a eq 999,nbad)
           if nbad gt 0 then a[wbad] = !values.f_nan
           data[k]=a
         endfor
         ;
         ;Append data of time and U, V components at determined altitude:
         ;=============================================================== 
         ;====convert time from LT to UT   
         time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+hour+':'+minute) $
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(9)+':'+string(0)+':'+string(0))
         if (n mod 2 eq 0) then time1(0)= time
         if (n mod 2 eq 1) then time1(1)= time 
       for g=0,30 do begin         
         if (70+g eq alt) then begin
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
            append_array, mu_time, time
            append_array, zon_wind, zon_wind_data
            append_array, mer_wind, mer_wind_data
            append_array, zon_thermal, zon_thermal_data
            append_array, mer_thermal, mer_thermal_data
            append_array, meteor_num, meteor_num_data
            for i=0, 30 do begin
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

for g=0,30 do begin         
  height[g]=70+g 
endfor
;******************************
;Store data in TPLOT variables:
;******************************
acknowledgstring = ''

if mu_time[0] ne 0 then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'M. Yamamoto'))

     store_data,'iug_mu_meteor_uwnd',data={x:mu_time, y:zon_wind, v:height},dlimit=dlimit
     options,'iug_mu_meteor_uwnd',ytitle='MU-MW!CHeight!C[km]',ztitle='uwnd!C[m/s]',psym=-5
     store_data,'iug_mu_meteor_vwnd',data={x:mu_time, y:mer_wind, v:height},dlimit=dlimit
     options,'iug_mu_meteor_vwnd',ytitle='MU-MW!CHeight!C[km]',ztitle='vwnd!C[m/s]'
     store_data,'iug_mu_meteor_uwndsig',data={x:mu_time, y:zon_thermal, v:height},dlimit=dlimit
     options,'iug_mu_meteor_uwndsig',ytitle='MU-MW!CHeight!C[km]',ztitle='uwndsig!C[m/s]'
     store_data,'iug_mu_meteor_vwndsig',data={x:mu_time, y:mer_thermal, v:height},dlimit=dlimit
     options,'iug_mu_meteor_vwndsig',ytitle='MU-MW!CHeight!C[km]',ztitle='vwndsig!C[m/s]'
     store_data,'iug_mu_meteor_mwnum',data={x:mu_time, y:meteor_num, v:height},dlimit=dlimit
     options,'iug_mu_meteor_mwnum',ytitle='MU-MW!CHeight!C[km]',ztitle='mwnum'


   ; add options
     options, ['iug_mu_meteor_uwnd','iug_mu_meteor_vwnd',$
               'iug_mu_meteor_uwndsig','iug_mu_meteor_vwndsig',$
               'iug_mu_meteor_mwnum'], 'spec', 1

   ; add options of setting labels
     options,'iug_mu_meteor_uwnd', labels='MU-MW [km]'
     options,'iug_mu_meteor_vwnd', labels='MU-MW [km]'
     options,'iug_mu_meteor_uwndsig', labels='MU-MW [km]'
     options,'iug_mu_meteor_vwndsig', labels='MU-MW [km]'
     options,'iug_mu_meteor_mwnum', labels='MU-MW [km]'
  
  ;Clear time and data buffer:
  mu_time=0
  zon_wind=0
  mer_wind=0
  zon_thermal=0
  mer_thermal=0
  meteor_num=0
  endif
  ; add tdegap
  tdegap,'iug_mu_meteor_uwnd',/overwrite
  tdegap,'iug_mu_meteor_vwnd',/overwrite
  tdegap,'iug_mu_meteor_uwndsig',/overwrite
  tdegap,'iug_mu_meteor_vwndsig',/overwrite
  tdegap,'iug_mu_meteor_mwnum',/overwrite 
             
print,'**********************************************************************************
print, 'Data loading is successful!!'
print,'**********************************************************************************

end

