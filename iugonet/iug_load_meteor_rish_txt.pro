;+
;
;Name:
;iug_load_meteor_rish_txt
;
;Purpose:
;  Queries the Kyoto_RISH renkei2 servers for ACII data of the meteor radar 
;  and loads data intotplot format.
;
;Syntax:
; iug_load_meteor_rish_txt, site = site, downloadonly = downloadonly, $
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

pro iug_load_meteor_rish_txt, site=site, datatype = datatype, $
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
site_data_dir = strsplit('/meteor/h2km_t60min00/ /meteor/winddata/h2km_t60min00/ ',' ', /extract)
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
for ii=0,n_elements(site_code)-1 do begin

    if ~size(fns,/type) then begin
      if site_code[ii] eq 'ktb' then h=0
      if site_code[ii] eq 'srp' then h=1
    ;Get files for ith component:
    ;***************************       
      file_names = file_dailynames( $
                   file_format='YYYY/'+site_data_premane[h]+$
                   'YYYYMM',trange=trange,times=times,/unique)+'.h2t60'
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
       height = fltarr(21)
       zon_wind_data = fltarr(1,21)
       mer_wind_data = fltarr(1,21)
       zon_thermal_data = fltarr(1,21)
       mer_thermal_data = fltarr(1,21)
       meteor_num_data = fltarr(1,21)
       time = dblarr(1)
       time1 = dblarr(2)
       n=0
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
           
              ;get data of U, V, sigma-u, sigma-v, N-of-m, int1, int2:
              ;=======================================================
                data1 = strmid(s,12,55)
                data = float(strsplit(data1, ' ', /extract))
         
                for k=0,n_elements(data)-1 do begin
                    a = float(data[k])
                    wbad = where(a gt 105 || a lt -105,nbad)
                    ;wbad = where(a eq 999,nbad)
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
                      append_array, site_time, time
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

 ;Store data of meteor wind data:
 ;===============================

       if site_time[0] ne 0 then begin
          dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'T. Tsuda'))

          store_data,'iug_meteor_'+site_code[ii]+'_uwind',data={x:site_time, y:zon_wind, v:height},dlimit=dlimit
          options,'iug_meteor_'+site_code[ii]+'_uwind',ytitle=site_code[ii]+'!CHeight [km]',ztitle='Zonal wind [m/s]'
          store_data,'iug_meteor_'+site_code[ii]+'_vwind',data={x:site_time, y:mer_wind, v:height},dlimit=dlimit
          options,'iug_meteor_'+site_code[ii]+'_vwind',ytitle=site_code[ii]+'!CHeight [km]',ztitle='Meridional wind [m/s]'
          store_data,'iug_meteor_'+site_code[ii]+'_uwindsig',data={x:site_time, y:zon_thermal, v:height},dlimit=dlimit
          options,'iug_meteor_'+site_code[ii]+'_uwindsig',ytitle=site_code[ii]+'!CHeight [km]',ztitle='Sigma zonal wind [m/s]'
          store_data,'iug_meteor_'+site_code[ii]+'_vwindsig',data={x:site_time, y:mer_thermal, v:height},dlimit=dlimit
          options,'iug_meteor_'+site_code[ii]+'_vwindsig',ytitle=site_code[ii]+'!CHeight [km]',ztitle='Sigma meridional wind [m/s]'
          store_data,'iug_meteor_'+site_code[ii]+'_mwnum',data={x:site_time, y:meteor_num, v:height},dlimit=dlimit
          options,'iug_meteor_'+site_code[ii]+'_mwnum',ytitle=site_code[ii]+'!CHeight [km]',ztitle='munber'


          ; add options
          options, ['iug_meteor_'+site_code[ii]+'_uwind','iug_meteor_'+site_code[ii]+'_vwind',$
                    'iug_meteor_'+site_code[ii]+'_uwindsig','iug_meteor_'+site_code[ii]+'_vwindsig',$
                    'iug_meteor_'+site_code[ii]+'_mwnum'], 'spec', 1

          ; add options of setting labels
          options,'iug_meteor_'+site_code[ii]+'_uwind', labels='MWR '+site_code[ii]
          options,'iug_meteor_'+site_code[ii]+'_vwind', labels='MWR '+site_code[ii]
          options,'iug_meteor_'+site_code[ii]+'_uwindsig', labels='MWR '+site_code[ii]
          options,'iug_meteor_'+site_code[ii]+'_vwindsig', labels='MWR '+site_code[ii]
          options,'iug_meteor_'+site_code[ii]+'_mwnum', labels='MWR '+site_code[ii]
 
       endif 
       ;Clear time and data buffer:
       site_time=0
       zon_wind=0
       mer_wind=0
       zon_thermal=0
       mer_thermal=0
       meteor_num=0
       
   endif 
   jj=n_elements(local_paths)
endfor 

print,'**********************************************************************************
print, 'Data loading is successful!!'
print,'**********************************************************************************

end

