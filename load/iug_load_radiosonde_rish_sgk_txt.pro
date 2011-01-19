;+
;
;Name:
;iug_load_radiosonde_rish_sgk_txt
;
;Purpose:
;  Queries the Kyoto RISH data servers for ACII data of the radiosonde radar 
;  and loads data into tplot format.
;
;Syntax:
; iug_load_radiosonde_rish_sgk_txt, datatype = datatype, site = site, datatype = datatype, downloadonly = downloadonly, $
;                                     trange = trange, verbose=verbose
;
;Keywords:
; datatype = Observation data type. For example, iug_load_radiosonde_rish_sgk_txt, datatype = 'troposphere'.
;            The default is 'troposphere'. 
;  site  = Observatory code name.  For example, iug_load_meteor_rish_sgk_txt, site = 'sgk'.
;          The default is 'all', i.e., load all available stations.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;                 
;Data Availability:
;  Please check the following homepage of the time schedule of radiosonde observation 
;  before you analyze these data using this software. 
;  http://www.rish.kyoto-u.ac.jp/radar-group/mu/sondedb/
;
;Code:
;  A. Shinbori, 13/09/2010.
;  
;Modifications:
; A. Shinbori, 25/11/2010.
;  
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_radiosonde_rish_sgk_txt, datatype = datatype, site=site, $
                                      downloadonly=downloadonly, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2
 
;***********************************
;Load 'troposphere' data by default:
;***********************************
if (not keyword_set(datatype)) then datatype='troposhere'

;***********
;site codes:
;***********
;--- all sites (default)
site_code_all = strsplit('sgk',' ', /extract)

;--- check site codes
if(not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)

print, site_code

;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'If you acquire radio sonde data at Shigaraki, we ask that you' $
+ 'acknowledge us in your use of the data. This may be done by' $
+ 'including text such as the radio sonde data provided by Research Institute' $
+ 'for Sustainable Humanosphere of Kyoto University. We would also' $
+ 'appreciate receiving a copy of the relevant publications.'


;Calculation of altitude:
;========================
height = fltarr(1300)
height[0]=500
for i=0, 1298 do begin
    height[i+1] = height[i]+30
endfor

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
       if site_code[ii] eq 'sgk' then h=0
       hour_res = 1
    ;Get files for ith component:
    ;***************************       
      file_names = file_dailynames( $
                   file_format='YYYY/'+$
                   'YYYYMMDDhh',trange=trange,hour_res=hour_res,times=times,/unique)+'.snd'
    ;        
    ;Define FILE_RETRIEVE structure:
    ;===============================
       source = file_retrieve(/struct)
       source.verbose=verbose
       source.local_data_dir =  root_data_dir() + 'iugonet/rish/misc/'+site_code[ii]+'/radiosonde/'
       source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/radar-group/mu/sondedb/'
    
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
       sonde_time = 0
       sonde_press = 0
       sonde_temp = 0
       sonde_rh = 0
       sonde_aspeed = 0
       sonde_vvelo = 0
       
      ;Loop on files: 
      ;==============
       for j=jj,n_elements(local_paths)-1 do begin
           file= local_paths[j] 
           if file_test(/regular,file) then  dprint,'Loading radio sonde data file: ',file $
           else begin
              dprint,'Radio sonde data file',file,'not found. Skipping'
              continue
           endelse
           openr,lun,file,/get_lun 
           
           ;Read time information:
           readf,lun,s
           temp_name = strsplit(s,' ', /extract)
           year = fix(temp_name[1])
           month = fix(temp_name[2])
           day = fix(temp_name[3])
           hour = fix(temp_name[4])
           minute = fix(temp_name[5])
           second = fix(temp_name[6])
            print, year,month,day,hour,minute,second
 
       ;
       ;Loop on readdata:
       ;=================
       press = fltarr(1,1300)
       temp = fltarr(1,1300)
       rh = fltarr(1,1300)
       aspeed = fltarr(1,1300)
       vvelo = fltarr(1,1300)
           i=0
           while(not eof(lun)) do begin
             readf,lun,s
             ok=1
             if strmid(s,0,1) eq '[' then ok=0
             if ok && keyword_set(s) then begin
                dprint,s ,dlevel=5
              
                data_comp = strsplit(s,' ', /extract)
           
              ;get altitude data:
              ;=================

                hh = height[i]            
                wbad = where(hh eq 0,nbad)
                if nbad gt 0 then hh[wbad] = !values.f_nan
                height[i] =hh 
              ;get data of press., temp., rh, dewp, uwind, vwind:
              ;=======================================================
               press[*,i] = float(data_comp[1])
               temp[*,i] = float(data_comp[2])
               rh[*,i] = float(data_comp[3])
               aspeed[*,i] = float(data_comp[4])
               vvelo[*,i] = float(data_comp[5])
 
                i=i+1 
                continue       
             endif
           endwhile 
           free_lun,lun
           for i=0, 1299 do begin
                a = press[*,i]            
                wbad = where(a eq -999 || a eq 0,nbad)
                if nbad gt 0 then a[wbad] = !values.f_nan
                press[*,i] =a 
                b = temp[*,i]            
                wbad = where(b eq -999 || b eq 0,nbad)
                if nbad gt 0 then b[wbad] = !values.f_nan
                temp[*,i] =b 
                c = rh[*,i]            
                wbad = where(c eq -999 || c eq 0,nbad)
                if nbad gt 0 then c[wbad] = !values.f_nan
                rh[*,i] =c 
                d = aspeed[*,i]            
                wbad = where(d eq -999 || d eq 0,nbad)
                if nbad gt 0 then d[wbad] = !values.f_nan
                aspeed[*,i] =d 
                e = vvelo[*,i]            
                wbad = where(e eq -999 || e eq 0,nbad)
                if nbad gt 0 then e[wbad] = !values.f_nan
                vvelo[*,i] =e 
           endfor
           ;==== time from UT to UNIX Time
           time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+string(hour)+':'+string(minute)+':'+string(second))$
                  -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(9))
          ;
          ;Append data :
          ;=============
           append_array, sonde_time, time
           append_array, sonde_press, press
           append_array, sonde_temp, temp
           append_array, sonde_rh, rh
           append_array, sonde_aspeed, aspeed
           append_array, sonde_vvelo, vvelo
       endfor

      
 ;******************************
 ;Store data in TPLOT variables:
 ;******************************

 ;Store data of meteor wind data:
 ;===============================

       if sonde_time[0] ne 0 then begin
          dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'T. Tsuda'))

          store_data,'iug_radiosonde_'+site_code[ii]+'_press',data={x:sonde_time, y:sonde_press, v:height},dlimit=dlimit
          options,'iug_radiosonde_'+site_code[ii]+'_press',ytitle='RSND-'+site_code[ii]+'!CHeight!C[m]',ztitle='Press.!C[hPa]'
          store_data,'iug_radiosonde_'+site_code[ii]+'_temp',data={x:sonde_time, y:sonde_temp, v:height},dlimit=dlimit
          options,'iug_radiosonde_'+site_code[ii]+'_temp',ytitle='RSND-'+site_code[ii]+'!CHeight!C[m]',ztitle='Temp.!C[deg.]'
          store_data,'iug_radiosonde_'+site_code[ii]+'_rh',data={x:sonde_time, y:sonde_rh, v:height},dlimit=dlimit
          options,'iug_radiosonde_'+site_code[ii]+'_rh',ytitle='RSND-'+site_code[ii]+'!CHeight!C[m]',ztitle='RH!C[%]'
          store_data,'iug_radiosonde_'+site_code[ii]+'_aspeed',data={x:sonde_time, y:sonde_aspeed, v:height},dlimit=dlimit
          options,'iug_radiosonde_'+site_code[ii]+'_aspeed',ytitle='RSND-'+site_code[ii]+'!CHeight!C[m]',ztitle='Ascending speed!C[m/s]'
          store_data,'iug_radiosonde_'+site_code[ii]+'_vvelo',data={x:sonde_time, y:sonde_vvelo, v:height},dlimit=dlimit
          options,'iug_radiosonde_'+site_code[ii]+'_vvelo',ytitle='RSND-'+site_code[ii]+'!CHeight!C[m]',ztitle='Vertical velocity!C[m]'

          ; add options
          options, ['iug_radiosonde_'+site_code[ii]+'_press','iug_radiosonde_'+site_code[ii]+'_temp',$
                    'iug_radiosonde_'+site_code[ii]+'_rh',$
                    'iug_radiosonde_'+site_code[ii]+'_aspeed','iug_radiosonde_'+site_code[ii]+'_vvelo'], 'spec', 1

          ; add options of setting labels
          options,'iug_radiosonde_'+site_code[ii]+'_press', labels='RSND'+site_code[ii]
          options,'iug_radiosonde_'+site_code[ii]+'_temp', labels='RSND'+site_code[ii]
          options,'iug_radiosonde_'+site_code[ii]+'_rh', labels='RSND'+site_code[ii]
          options,'iug_radiosonde_'+site_code[ii]+'_aspeed', labels='RSND'+site_code[ii]
          options,'iug_radiosonde_'+site_code[ii]+'_vvelo', labels='RSND'+site_code[ii]
       endif 
       ;Clear time and data buffer:
       sonde_time = 0
       sonde_press = 0
       sonde_temp = 0
       sonde_rh = 0
       sonde_aspeed = 0
       sonde_vvelo = 0
       ; add tdegap
       tdegap, 'iug_radiosonde_'+site_code[ii]+'_press',/overwrite
       tdegap, 'iug_radiosonde_'+site_code[ii]+'_temp',/overwrite
       tdegap, 'iug_radiosonde_'+site_code[ii]+'_rh',/overwrite
       tdegap, 'iug_radiosonde_'+site_code[ii]+'_aspeed',/overwrite
       tdegap, 'iug_radiosonde_'+site_code[ii]+'_vvelo',/overwrite
       
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
print, 'If you acquire radio sonde data at Shigaraki, we ask that you acknowledge us'
print, 'in your use of the data. This may be done by including text' 
print, 'such as the radio sonde data provided by Research Institute for Sustainable' 
print, 'Humanosphere of Kyoto University. We would also appreciate receiving' 
print, 'a copy of the relevant publications.'

end

