
;+
;
;Name:
;iug_load_meteor_rish_txt4km
;
;Purpose:
;  Queries the Kyoto_RISH renkei2 servers for ACII data of the meteor radar 
;  and loads data intotplot format.
;
;Syntax:
; iug_load_meteor_rish_txt4km, site = site, downloadonly = downloadonly, $
;                           trange = trange, verbose=verbose
;
;Keywords:
;  site  = Observatory code name.  For example, iug_load_meteor_rish_txt, site = 'srp'.
;          The default is 'all', i.e., load all available stations.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;Code:
;  A. Shinbori, 28/08/2010.
;  
;Modifications:
;  A. Shinbori, 10/09/2010.
;  
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_meteor_rish_txt4km, site=site, datatype = datatype, $
                              downloadonly=downloadonly, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2
 
;****************************************
;Load 'troposphere_wind' data by default:
;****************************************
if (not keyword_set(datatype)) then datatype='thermosphere'

;***********
;site codes:
;***********
;--- all sites (default)
site_code_all = strsplit('ktb srp',' ', /extract)

;--- check site codes
if(not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)

print, site_code

;***************
;data directory:
;***************
site_data_dir = strsplit('/meteor/h4km_t60min00/ /meteor/winddata/h4km_t60min00/ ',' ', /extract)
site_data_premane = strsplit('Wk jkt',' ', /extract)

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
h=0
jj=0
n=0
for ii=0,n_elements(site_code)-1 do begin

    if ~size(fns,/type) then begin
      if site_code[ii] eq 'ktb' then h=0
      if site_code[ii] eq 'srp' then h=1
    ;Get files for ith component:
    ;***************************       
      file_names = file_dailynames( $
                   file_format='YYYY/'+site_data_premane[h]+$
                   'YYYYMM',trange=trange,times=times,/unique)+'.h4T60'
    ;        
    ;Define FILE_RETRIEVE structure:
    ;===============================
       source = file_retrieve(/struct)
       source.verbose=verbose
       source.local_data_dir =  root_data_dir() + 'iugonet/rish/misc/'+site_code[ii]+site_data_dir[h]

    ;source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/ear/data/data/ver02.0212/'
    
    ;Get files and local paths, and concatenate local paths:
    ;=======================================================
       local_paths=file_retrieve(file_names,_extra=source, /last_version)
       local_paths_all = ~(~size(local_paths_all,/type)) ? $
                        [local_paths_all, local_paths] : local_paths
       if ~(~size(local_paths_all,/type)) then local_paths=local_paths_all
    endif else file_names=fns

    ;--- Load data into tplot variables
    if(not keyword_set(downloadonly)) then downloadonly=0

    if(downloadonly eq 0) then begin

    ;Read the files:
    ;===============
       s=''
       height = fltarr(18)
       zon_wind_data = fltarr(1,18)
       mer_wind_data = fltarr(1,18)
       zon_thermal_data = fltarr(1,18)
       mer_thermal_data = fltarr(1,18)
       meteor_num_data = fltarr(1,18)
       data= fltarr(5)
       time = 0
       time_val = 0
       site_time=0
       zon_wind=0
       mer_wind=0
       zon_thermal=0
       mer_thermal=0
       meteor_num=0 

      ;Loop on files: 
      ;==============
       for j=jj,n_elements(local_paths)-1 do begin
           file= local_paths[j] 
           if file_test(/regular,file) then  dprint,'Loading meteor radar data file: ',file $
           else begin
              dprint,'Meteor radar data file',file,' not found. Skipping'
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
                if fix(strmid(s,0,2)) gt 70 then year = fix(strmid(s,0,2))+1900
                if fix(strmid(s,0,2)) lt 70 then year = fix(strmid(s,0,2))+2000
                day_of_year = fix(strmid(s,2,3))
                doy_to_month_date, year, day_of_year, month, day
                hour = strmid(s,5,2)
                minute = strmid(s,7,2)
           
              ;get altitude data:
              ;=================
                alt = fix(strmid(s,9,3))
                idx = (alt-54)/4
              ;get data of U, V, sigma-u, sigma-v, N-of-m, int1, int2:
              ;=======================================================
                data1 = strmid(s,12,55)
                data2 = strsplit(data1, ' ', /extract)
                data(0) = float(data2[0])
                data(1) = float(data2[1])
                data(2) = float(data2[2])
                data(3) = float(data2[3])
                data(4) = float(data2[4])
              ;====convert time from LT to UT   
                time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+string(hour)+':'+string(minute)) $
                       -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(7)+':'+string(0)+':'+string(0))                                
                
              ;insert data of zonal and meridional winds etc.
                if n eq 0 then begin
                   time_val3 = time
                   zon_wind_data(0,idx)= data(0)
                   mer_wind_data(0,idx)= data(1)
                   zon_thermal_data(0,idx)= data(2)
                   mer_thermal_data(0,idx)= data(3)
                   meteor_num_data(0,idx)= data(4)
                endif
                time_diff=time-time_val
                if n eq 0 then time_diff=3600
                ;appned time and data if time_val is not equal to time
                if time_val ne time then begin
                   time_val=time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+string(hour)+':'+string(minute)) $
                            -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(7)+':'+string(0)+':'+string(0))          
                   time_val2=time_val-time_diff
                   if time_val2 eq 0 then time_val2=time_val+3600
              ;
              ;Append data of time and wind data at determined altitude:
              ;=========================================================
                if n ne 0 then begin
                      append_array, site_time, time_val2
                      append_array, zon_wind, zon_wind_data
                      append_array, mer_wind, mer_wind_data
                      append_array, zon_thermal, zon_thermal_data
                      append_array, mer_thermal, mer_thermal_data
                      append_array, meteor_num, meteor_num_data
                endif
                n=n+1
                  for i=0, 17 do begin
                    zon_wind_data(0,i)=!values.f_nan
                    mer_wind_data(0,i)=!values.f_nan
                    zon_thermal_data(0,i)=!values.f_nan
                    mer_thermal_data(0,i)=!values.f_nan
                    meteor_num_data(0,i)=!values.f_nan 
                  endfor 
                endif                
                zon_wind_data(0,idx)= data(0)
                mer_wind_data(0,idx)= data(1)
                zon_thermal_data(0,idx)= data(2)
                mer_thermal_data(0,idx)= data(3)
                meteor_num_data(0,idx)= data(4)            
             endif
           
           endwhile 
           free_lun,lun
              ;
              ;Append data of time and wind data at the last time in each file:
              ;================================================================
              append_array, site_time, time_val2+3600
              append_array, zon_wind, zon_wind_data
              append_array, mer_wind, mer_wind_data
              append_array, zon_thermal, zon_thermal_data
              append_array, mer_thermal, mer_thermal_data
              append_array, meteor_num, meteor_num_data
       endfor
          
       for g=0,17 do begin         
           height[g]=float(54+g*4) 
       endfor
 ;******************************
 ;Store data in TPLOT variables:
 ;******************************
       acknowledgstring = ''

 ;Store data of meteor wind data:
 ;===============================

       if site_time[0] ne 0 then begin
          dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'T. Tsuda'))
          store_data,'iug_meteor_'+site_code[ii]+'_uwnd4km',data={x:site_time, y:zon_wind, v:height},dlimit=dlimit
          options,'iug_meteor_'+site_code[ii]+'_uwnd4km',ytitle='MW-'+site_code[ii]+'!CHeight!C[km]',ztitle='uwnd!C[m/s]'
          store_data,'iug_meteor_'+site_code[ii]+'_vwnd4km',data={x:site_time, y:mer_wind, v:height},dlimit=dlimit
          options,'iug_meteor_'+site_code[ii]+'_vwnd4km',ytitle='MW-'+site_code[ii]+'!CHeight!C[km]',ztitle='vwnd!C[m/s]'
          store_data,'iug_meteor_'+site_code[ii]+'_uwndsig4km',data={x:site_time, y:zon_thermal, v:height},dlimit=dlimit
          options,'iug_meteor_'+site_code[ii]+'_uwndsig4km',ytitle='MW-'+site_code[ii]+'!CHeight!C[km]',ztitle='uwndsig!C[m/s]'
          store_data,'iug_meteor_'+site_code[ii]+'_vwndsig4km',data={x:site_time, y:mer_thermal, v:height},dlimit=dlimit
          options,'iug_meteor_'+site_code[ii]+'_vwndsig4km',ytitle='MW-'+site_code[ii]+'!CHeight!C[km]',ztitle='vwndsig!C[m/s]'
          store_data,'iug_meteor_'+site_code[ii]+'_mwnum4km',data={x:site_time, y:meteor_num, v:height},dlimit=dlimit
          options,'iug_meteor_'+site_code[ii]+'_mwnum4km',ytitle='MW-'+site_code[ii]+'!CHeight!C[km]',ztitle='mwnum'

          ; add options
          options, ['iug_meteor_'+site_code[ii]+'_uwnd4km','iug_meteor_'+site_code[ii]+'_vwnd4km',$
                    'iug_meteor_'+site_code[ii]+'_uwndsig4km','iug_meteor_'+site_code[ii]+'_vwndsig4km',$
                    'iug_meteor_'+site_code[ii]+'_mwnum4km'], 'spec', 1

          ; add options of setting labels
          options,'iug_meteor_'+site_code[ii]+'_uwnd4km', labels='MW '+site_code[ii]
          options,'iug_meteor_'+site_code[ii]+'_vwnd4km', labels='MW '+site_code[ii]
          options,'iug_meteor_'+site_code[ii]+'_uwndsig4km', labels='MW '+site_code[ii]
          options,'iug_meteor_'+site_code[ii]+'_vwndsig4km', labels='MW '+site_code[ii]
          options,'iug_meteor_'+site_code[ii]+'_mwnum4km', labels='MW '+site_code[ii]
 
       endif 
       ;Clear time and data buffer:
       site_time=0
       zon_wind=0
       mer_wind=0
       zon_thermal=0
       mer_thermal=0
       meteor_num=0
       ; add tdegap
       tdegap, 'iug_meteor_'+site_code[ii]+'_uwnd4km',/overwrite
       tdegap, 'iug_meteor_'+site_code[ii]+'_vwnd4km',/overwrite
       tdegap, 'iug_meteor_'+site_code[ii]+'_uwndsig4km',/overwrite
       tdegap, 'iug_meteor_'+site_code[ii]+'_vwndsig4km',/overwrite
       tdegap, 'iug_meteor_'+site_code[ii]+'_mwnum4km',/overwrite
   endif 
   jj=n_elements(local_paths)
endfor 

print,'**********************************************************************************
print, 'Data loading is successful!!'
print,'**********************************************************************************

end

