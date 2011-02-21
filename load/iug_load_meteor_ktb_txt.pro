;+
;
;Name:
;iug_load_meteor_ktb_txt
;
;Purpose:
;  Queries the Kyoto_RISH data servers for ACII data of the meteor radar 
;  and loads data intotplot format.
;
;Syntax:
; iug_load_meteor_ktb_txt, parameter = parameter, downloadonly = downloadonly, $
;                           trange = trange, verbose=verbose
;
;Keywords:
;  parameter = Data parameter. For example, iug_load_meteor_ktb_txt, parameter = 'h2t60min00'. 
;              A kind of parameters is 4 types of 'h2t60min00', 'h2t60min00', 'h4t60min00', 'h4t60min00'.
;              The default is 'all'.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;Code:
;  A. Shinbori, 02/04/2011.
;  
;Modifications:
;  
;  
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_meteor_ktb_txt, datatype = datatype, parameter = parameter, $
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
site_code_all = strsplit('ktb',' ', /extract)

;--- check site codes
if(not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)

print, site_code

;***********
;parameters:
;***********
;--- all parameters (default)
parameter_all = strsplit('h2t60min00 h2t60min30 h4t60min00 h4t60min30',' ', /extract)

;--- check parameters
if(not keyword_set(parameter)) then parameter='all'
parameters = thm_check_valid_name(parameter, parameter_all, /ignore_case, /include_all)

print, parameters

;***************
;data directory:
;***************
site_data_dir = strsplit('/h2km_t60min00/ /h2km_t60min30/ /h4km_t60min00/ /h4km_t60min30/',' ', /extract)
site_data_lastmane = strsplit('h2t60min00 h2t60min30 h4t60min00 h4t60min30',' ', /extract)
;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'Scientists who want to engage in collaboration with Research Institute for Sustainable Humanosphere (RISH) ' $
+ 'should contact the principal investigator of the meteor wind (MW) radar in Indonesia ' $
+ 'Prof. Toshitaka Tsuda, Kyoto University, who will organize such collaborations. ' $
+ 'There is a possibility that the PI of the MW radar will arrange offers so that there is less overlapping of themes between our research groups' $
+ 'Before you use the MW radar data for your papers, you must agree to the following points;' $
+ '  1. Before you submit your paper, you must contact the PI (Prof. Toshitaka Tsuda: tsuda@rish.kyoto-u.ac.jp) and discuss authorship.' $
+ '  2. When you submit your paper after doing the above item 1, you must mention the source of the data in the acknowledgment section of your paper.' $
+ '  3. In all circumstances, if anything is published you must send a hardcopy to the following address:' $
+ '    Prof. Toshitaka Tsuda' $
+ '    PI of the MW radar in Indonesia' $
+ '    Director of Research Institute for Sustainable Humanosphere,' $
+ '    Kyoto University' $
+ '    Gokasyo, Uji Kyoto 611-0011, Japan' $
+ '    e-mail: tsuda@rish.kyoto-u.ac.jp' 

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
  if n_elements(parameters) eq 4 then begin 
     h_min=0
     h_max=4
     kk=0
  endif
  if n_elements(parameters) eq 1 then begin
     if parameters eq 'h2t60min00' then begin
        h_min=0
        h_max=1
        kk=0 
     endif
     if parameters eq 'h2t60min30' then begin
        h_min=0
        h_max=1
        kk=1 
     endif
     if parameters eq 'h4t60min00' then begin
        h_min=0
        h_max=1
        kk=2 
     endif
     if parameters eq 'h4t60min30' then begin
        h_min=0
        h_max=1
        kk=3 
     endif
  endif
for ii=h_min,h_max-1 do begin

    if ~size(fns,/type) then begin
     
    ;Get files for ith component:
    ;***************************       
      file_names = file_dailynames( $
                   file_format='YYYY/Wk'+$
                   'YYYYMM',trange=trange,times=times,/unique)+'.'+site_data_lastmane[ii]+'.txt'
    ;        
    ;Define FILE_RETRIEVE structure:
    ;===============================
       source = file_retrieve(/struct)
       source.verbose=verbose
       source.local_data_dir =  root_data_dir() + 'iugonet/rish/misc/ktb'+site_data_dir[ii]
       source.remote_data_dir = 'http://database.rish.kyoto-u.ac.jp/arch/iugonet/data/mwr/kototabang/text'+site_data_dir[ii]
    
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
       if site_data_lastmane[ii] eq 'h4t60min00' or 'h4t60min30' then begin
          arr_num=11
          dh=4
       endif    
       if site_data_lastmane[ii] eq 'h2t60min00' or 'h2t60min30' then begin
          arr_num=21
          dh=2
       endif
       ;Definition of array and its number:
       height = fltarr(arr_num)
       zon_wind_data = fltarr(1,arr_num)
       mer_wind_data = fltarr(1,arr_num)
       zon_thermal_data = fltarr(1,arr_num)
       mer_thermal_data = fltarr(1,arr_num)
       meteor_num_data = fltarr(1,arr_num)
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
           n=0
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
                idx = (alt-70)/dh
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
                  for i=0, arr_num-1 do begin
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
          
       for g=0,arr_num-1 do begin         
           height[g]=float(70+g*dh) 
       endfor
 ;******************************
 ;Store data in TPLOT variables:
 ;******************************

 ;Store data of meteor wind data:
 ;===============================

       if site_time[0] ne 0 then begin
          dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'T. Tsuda'))
          store_data,'iug_meteor_ktb_uwnd_'+site_data_lastmane[ii],data={x:site_time, y:zon_wind, v:height},dlimit=dlimit
          options,'iug_meteor_ktb_uwnd_'+site_data_lastmane[ii],ytitle='MW-ktb!CHeight!C[km]',ztitle='uwnd!C[m/s]'
          store_data,'iug_meteor_ktb_vwnd_'+site_data_lastmane[ii],data={x:site_time, y:mer_wind, v:height},dlimit=dlimit
          options,'iug_meteor_ktb_vwnd_'+site_data_lastmane[ii],ytitle='MW-ktb!CHeight!C[km]',ztitle='vwnd!C[m/s]'
          store_data,'iug_meteor_ktb_uwndsig_'+site_data_lastmane[ii],data={x:site_time, y:zon_thermal, v:height},dlimit=dlimit
          options,'iug_meteor_ktb_uwndsig_'+site_data_lastmane[ii],ytitle='MW-ktb!CHeight!C[km]',ztitle='uwndsig!C[m/s]'
          store_data,'iug_meteor_ktb_vwndsig_'+site_data_lastmane[ii],data={x:site_time, y:mer_thermal, v:height},dlimit=dlimit
          options,'iug_meteor_ktb_vwndsig_'+site_data_lastmane[ii],ytitle='MW-ktb!CHeight!C[km]',ztitle='vwndsig!C[m/s]'
          store_data,'iug_meteor_ktb_mwnum_'+site_data_lastmane[ii],data={x:site_time, y:meteor_num, v:height},dlimit=dlimit
          options,'iug_meteor_ktb_mwnum_'+site_data_lastmane[ii],ytitle='MW-ktb!CHeight!C[km]',ztitle='mwnum'

          ; add options
          options, ['iug_meteor_ktb_uwnd_'+site_data_lastmane[ii],'iug_meteor_ktb_vwnd_'+site_data_lastmane[ii],$
                    'iug_meteor_ktb_uwndsig_'+site_data_lastmane[ii],'iug_meteor_ktb_vwndsig_'+site_data_lastmane[ii],$
                    'iug_meteor_ktb_mwnum_'+site_data_lastmane[ii]], 'spec', 1

          ; add options of setting labels
          options,'iug_meteor_ktb_uwnd_'+site_data_lastmane[ii], labels='MW ktb'
          options,'iug_meteor_ktb_vwnd_'+site_data_lastmane[ii], labels='MW ktb'
          options,'iug_meteor_ktb_uwndsig_'+site_data_lastmane[ii], labels='MW ktb'
          options,'iug_meteor_ktb_vwndsig_'+site_data_lastmane[ii], labels='MW ktb'
          options,'iug_meteor_ktb_mwnum_'+site_data_lastmane[ii], labels='MW ktb'
 
       endif 
       ;Clear time and data buffer:
       site_time=0
       zon_wind=0
       mer_wind=0
       zon_thermal=0
       mer_thermal=0
       meteor_num=0
       
       ; add tdegap
       tdegap, 'iug_meteor_ktb_uwnd_'+site_data_lastmane[ii],/overwrite
       tdegap, 'iug_meteor_ktb_vwnd_'+site_data_lastmane[ii],/overwrite
       tdegap, 'iug_meteor_ktb_uwndsig_'+site_data_lastmane[ii],/overwrite
       tdegap, 'iug_meteor_ktb_vwndsig_'+site_data_lastmane[ii],/overwrite
       tdegap, 'iug_meteor_ktb_mwnum_'+site_data_lastmane[ii],/overwrite
       
       ; add tclip
       tclip, 'iug_meteor_ktb_uwnd_'+site_data_lastmane[ii],-200,200,/overwrite
       tclip, 'iug_meteor_ktb_vwnd_'+site_data_lastmane[ii],-200,200,/overwrite
       tclip, 'iug_meteor_ktb_uwndsig_'+site_data_lastmane[ii],0,400,/overwrite
       tclip, 'iug_meteor_ktb_vwndsig_'+site_data_lastmane[ii],0,400,/overwrite
      ; tclip, 'iug_meteor_ktb_mwnum_'+site_data_lastmane[ii],,0,800,/overwrite
       
   endif 
   jj=n_elements(local_paths)
endfor 

print,'******************************
print, 'Data loading is successful!!'
print,'******************************

;******************************
;print of acknowledgement:
;******************************
print, '****************************************************************
print, 'Acknowledgement'
print, '****************************************************************
print, 'Scientists who want to engage in collaboration with Research Institute for Sustainable Humanosphere (RISH) ' 
print, 'should contact the principal investigator of the meteor wind (MW) radar in Indonesia ' 
print, 'Prof. Toshitaka Tsuda, Kyoto University, who will organize such collaborations. ' 
print, 'There is a possibility that the PI of the MW radar will arrange offers so that there is less overlapping of themes between our research groups' 
print, 'Before you use the MW radar data for your papers, you must agree to the following points;' 
print, '  1. Before you submit your paper, you must contact the PI (Prof. Toshitaka Tsuda: tsuda@rish.kyoto-u.ac.jp) and discuss authorship.' 
print, '  2. When you submit your paper after doing the above item 1, you must mention the source of the data in the acknowledgment section of your paper.' 
print, '  3. In all circumstances, if anything is published you must send a hardcopy to the following address:' 
print, '    Prof. Toshitaka Tsuda' 
print, '    PI of the MW radar in Indonesia' 
print, '    Director of Research Institute for Sustainable Humanosphere,' 
print, '    Kyoto University' 
print, '    Gokasyo, Uji Kyoto 611-0011, Japan' 
print, '    e-mail: tsuda@rish.kyoto-u.ac.jp' 

end

