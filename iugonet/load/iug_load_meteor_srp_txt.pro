;+
;
;NAME:
;iug_load_meteor_srp_txt
;
;PURPOSE:
;  Queries the Kyoto_RISH servers for the horizontal wind data (uwnd, vwnd, uwndsig, vwndsig, mwnum)
;  in the text format taken by the meteor wind radar (MWR) at Serpong and loads data into
;  tplot format.
;
;SYNTAX:
; iug_load_meteor_srp_txt, datatype = datatype, parameter = parameter, length=length, downloadonly = downloadonly, $
;                          trange = trange, verbose=verbose
;
;KEYWOARDS:
;  datatype = Observation data type. For example, iug_load_meteor_srp_txt, datatype = 'thermosphere'.
;            The default is 'thermosphere'. 
;  length = Data length '1-day' or '1-month'. For example, iug_load_meteor_srp_txt, length = '1_day'.
;           A kind of parameters is 2 types of '1_day', and '1_month'.
;  parameter = Data parameter. For example, iug_load_meteor_srp_txt, parameter = 'h2t60min00'. 
;              A kind of parameters is 4 types of 'h2t60min00', 'h2t60min00', 'h4t60min00', 'h4t60min00'.
;              The default is 'all'.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;CODE:
; A. Shinbori, 19/09/2010.
;
;MODIFICATIONS:
; A. Shinbori, 24/03/2011.
; A. Shinbori, 11/07/2011.
; A. Shinbori, 06/10/2011.
; A. Shinbori, 27/12/2011.
;
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_meteor_srp_txt, datatype = datatype, parameter = parameter, length=length, $
                              downloadonly=downloadonly, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2
 
;****************************************
;Load 'thermosphere' data by default:
;****************************************
if (not keyword_set(datatype)) then datatype='thermosphere'

;*****************************
;Load '1_day' data by default:
;*****************************
if (not keyword_set(length)) then length='1_day'

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
parameter_all = strsplit('h2t60min00 h2t60min30 h4t60min00 h4t60min30 h4t240min00',' ', /extract)

;--- check parameters
if(not keyword_set(parameter)) then parameter='all'
parameters = thm_check_valid_name(parameter, parameter_all, /ignore_case, /include_all)

print, parameters

;***************
;data directory:
;***************
site_data_dir = strsplit('h2km_t60min00/ h2km_t60min30/ h4km_t60min00/ h4km_t60min30/ h4km_t240min00/',' ', /extract)
site_data_lastmane = strsplit('h2t60min00 h2t60min30 h4t60min00 h4t60min30 h4t240min00',' ', /extract)

;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'Note: If you would like to use following data for scientific purpose, please read and keep the DATA USE POLICY '$
+'(http://database.rish.kyoto-u.ac.jp/arch/iugonet/data_policy/Data_Use_Policy_e.html '$ 
+'The distribution of meteor wind radar data has been partly supported by the IUGONET (Inter-university Upper '$
+ 'atmosphere Global Observation NETwork) project (http://www.iugonet.org/) funded '$
+ 'by the Ministry of Education, Culture, Sports, Science and Technology (MEXT), Japan.' 

;==================================================================
;Download files, read data, and create tplot vars at each component
;==================================================================
;******************************************************************
;Loop on downloading files
;******************************************************************
;Get timespan, define FILE_NAMES, and load data:
;===============================================

;Definition of parameters:
h=0
jj=0
kk=0
  
  for iii=0,n_elements(parameters)-1 do begin
  ;The parameter search:'
    for jjj=0, n_elements(site_data_lastmane)-1 do begin
       if parameters[iii] eq 'h2t60min00' then kk=0
       if parameters[iii] eq 'h2t60min30' then kk=1
       if parameters[iii] eq 'h4t60min00' then kk=2
       if parameters[iii] eq 'h4t60min30' then kk=3
       if parameters[iii] eq 'h4t240min00' then kk=4
    endfor
 
    if ~size(fns,/type) then begin
      if length eq '1_day' then begin 
        ;Get files for ith component:
        ;***************************       
         file_names = file_dailynames( $
                      file_format='YYYY/jkt'+$
                      'YYYYMMDD',trange=trange,times=times,/unique)+'.'+site_data_lastmane[kk]+'.txt'
      endif else if length eq '1_month' then begin
        ;Get files for ith component:
        ;***************************       
         file_names = file_dailynames( $
                      file_format='YYYY/jkt'+$
                      'YYYYMM',trange=trange,times=times,/unique)+'.'+site_data_lastmane[kk]+'.txt'
      endif
    ;        
    ;Define FILE_RETRIEVE structure:
    ;===============================
       source = file_retrieve(/struct)
       source.verbose=verbose
       source.local_data_dir =  root_data_dir() + 'iugonet/rish/misc/srp/meteor/text/ver1_0/'+length+'/'+site_data_dir[kk]
       source.remote_data_dir = 'http://database.rish.kyoto-u.ac.jp/arch/iugonet/data/mwr/serpong/text/ver1_0/'+site_data_dir[kk]

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
       
       ;Definition of parameter:
       s=''
       
       ;Determination of array number, height and time invervals:
       if site_data_lastmane[kk] eq 'h4t240min00' then begin
          arr_num=11
          dh=4
          dt=14400
       endif    
       if site_data_lastmane[kk] eq 'h2t60min00'  then begin
          arr_num=22
          dh=2
          dt=3600
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
           if file_test(/regular,file) then  dprint,'Loading meteor wind radar data file: ',file $
           else begin
              dprint,'Meteor wind radar data file',file,' not found. Skipping'
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
              
             ;Calculate time:
             ;===============
                if fix(strmid(s,0,2)) gt 70 then year = fix(strmid(s,0,2))+1900
                if fix(strmid(s,0,2)) lt 70 then year = fix(strmid(s,0,2))+2000
                day_of_year = fix(strmid(s,2,3))
                doy_to_month_date, year, day_of_year, month, day
                hour = strmid(s,5,2)
                minute = strmid(s,7,2)
           
              ;Get altitude data:
              ;=================
                alt = fix(strmid(s,9,3))
                idx = (alt-70)/dh
                
              ;Get data of U, V, sigma-u, sigma-v, N-of-m, int1, int2:
              ;=======================================================
                data1 = strmid(s,12,55)
                data2 = strsplit(data1, ' ', /extract)
                data(0) = float(data2[0])
                data(1) = float(data2[1])
                data(2) = float(data2[2])
                data(3) = float(data2[3])
                data(4) = float(data2[4])
                
              ;====Convert time from local time to unix time  
                time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+string(hour)+':'+string(minute)) $
                       -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(7)+':'+string(0)+':'+string(0))                                
                
              ;Insert data of zonal and meridional winds etc.
                if n eq 0 then begin
                   time_val3 = time
                   zon_wind_data(0,idx)= data(0)
                   mer_wind_data(0,idx)= data(1)
                   zon_thermal_data(0,idx)= data(2)
                   mer_thermal_data(0,idx)= data(3)
                   meteor_num_data(0,idx)= data(4)
                endif
                time_diff=time-time_val
                if n eq 0 then time_diff=dt
                
                ;Appned time and data if time_val is not equal to time:
                if time_val ne time then begin
                   time_val=time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+string(hour)+':'+string(minute)) $
                            -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(7)+':'+string(0)+':'+string(0))          
                   time_val2=time_val-time_diff
                   if time_val2 eq 0 then time_val2=time_val+dt
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
              append_array, site_time, time_val2+dt
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
          store_data,'iug_meteor_srp_uwnd_'+site_data_lastmane[kk],data={x:site_time, y:zon_wind, v:height},dlimit=dlimit
          options,'iug_meteor_srp_uwnd_'+site_data_lastmane[kk],ytitle='MW-srp!CHeight!C[km]',ztitle='uwnd!C[m/s]'
          store_data,'iug_meteor_srp_vwnd_'+site_data_lastmane[kk],data={x:site_time, y:mer_wind, v:height},dlimit=dlimit
          options,'iug_meteor_srp_vwnd_'+site_data_lastmane[kk],ytitle='MW-srp!CHeight!C[km]',ztitle='vwnd!C[m/s]'
          store_data,'iug_meteor_srp_uwndsig_'+site_data_lastmane[kk],data={x:site_time, y:zon_thermal, v:height},dlimit=dlimit
          options,'iug_meteor_srp_uwndsig_'+site_data_lastmane[kk],ytitle='MW-srp!CHeight!C[km]',ztitle='uwndsig!C[m/s]'
          store_data,'iug_meteor_srp_vwndsig_'+site_data_lastmane[kk],data={x:site_time, y:mer_thermal, v:height},dlimit=dlimit
          options,'iug_meteor_srp_vwndsig_'+site_data_lastmane[kk],ytitle='MW-srp!CHeight!C[km]',ztitle='vwndsig!C[m/s]'
          store_data,'iug_meteor_srp_mwnum_'+site_data_lastmane[kk],data={x:site_time, y:meteor_num, v:height},dlimit=dlimit
          options,'iug_meteor_srp_mwnum_'+site_data_lastmane[kk],ytitle='MW-srp!CHeight!C[km]',ztitle='mwnum'

          ; add options
          options, ['iug_meteor_srp_uwnd_'+site_data_lastmane[kk],'iug_meteor_srp_vwnd_'+site_data_lastmane[kk],$
                    'iug_meteor_srp_uwndsig_'+site_data_lastmane[kk],'iug_meteor_srp_vwndsig_'+site_data_lastmane[kk],$
                    'iug_meteor_srp_mwnum_'+site_data_lastmane[kk]], 'spec', 1

          ; add options of setting labels
          options,'iug_meteor_srp_uwnd_'+site_data_lastmane[kk], labels='MW srp'
          options,'iug_meteor_srp_vwnd_'+site_data_lastmane[kk], labels='MW srp'
          options,'iug_meteor_srp_uwndsig_'+site_data_lastmane[kk], labels='MW srp'
          options,'iug_meteor_srp_vwndsig_'+site_data_lastmane[kk], labels='MW srp'
          options,'iug_meteor_srp_mwnum_'+site_data_lastmane[kk], labels='MW srp'
 
       endif 
       ;Clear time and data buffer:
       site_time=0
       zon_wind=0
       mer_wind=0
       zon_thermal=0
       mer_thermal=0
       meteor_num=0
       
       ; add tdegap
       tdegap, 'iug_meteor_srp_uwnd_'+site_data_lastmane[kk],/overwrite
       tdegap, 'iug_meteor_srp_vwnd_'+site_data_lastmane[kk],/overwrite
       tdegap, 'iug_meteor_srp_uwndsig_'+site_data_lastmane[kk],/overwrite
       tdegap, 'iug_meteor_srp_vwndsig_'+site_data_lastmane[kk],/overwrite
       tdegap, 'iug_meteor_srp_mwnum_'+site_data_lastmane[kk],/overwrite
       
       ; add tclip
       tclip, 'iug_meteor_srp_uwnd_'+site_data_lastmane[kk],-200,200,/overwrite
       tclip, 'iug_meteor_srp_vwnd_'+site_data_lastmane[kk],-200,200,/overwrite
       tclip, 'iug_meteor_srp_uwndsig_'+site_data_lastmane[kk],0,400,/overwrite
       tclip, 'iug_meteor_srp_vwndsig_'+site_data_lastmane[kk],0,400,/overwrite
      ; tclip, 'iug_meteor_srp_mwnum_'+site_data_lastmane[kk],,0,800,/overwrite
       
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
print, 'Note: If you would like to use following data for scientific purpose,
print, 'please read and keep the DATA USE POLICY'
print, '(http://database.rish.kyoto-u.ac.jp/arch/iugonet/data_policy/Data_Use_Policy_e.html' 
print, 'The distribution of meteor wind radar data has been partly supported by the IUGONET'
print, '(Inter-university Upper atmosphere Global Observation NETwork) project'
print, '(http://www.iugonet.org/) funded by the Ministry of Education, Culture, Sports, Science'
print, 'and Technology (MEXT), Japan.'  

end

