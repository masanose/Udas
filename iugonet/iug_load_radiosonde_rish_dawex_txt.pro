;+
;
;Name:
;iug_load_radiosonde_rish_dawex_txt
;
;Purpose:
;  Queries the Kyoto_RISH renkei2 servers for ACII data of the meteor radar 
;  and loads data intotplot format.
;
;Syntax:
; iug_load_radiosonde_rish_dawex_txt, site = site, datatype = datatype, downloadonly = downloadonly, $
;                                     trange = trange, verbose=verbose
;
;Keywords:
;  site  = Observatory code name.  For example, iug_load_radiosonde_rish_dawex_txt, site = 'srp'.
;          The default is 'all', i.e., load all available stations.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;Code:
;  A. Shinbori, 13/09/2010.
;  
;Modifications:
;  A. Shinbori, 19/09/2010.
;  
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_radiosonde_rish_dawex_txt, site=site, datatype = datatype, $
                                        downloadonly=downloadonly, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2
 
;****************************************
;Load 'troposphere_wind' data by default:
;****************************************
if (not keyword_set(datatype)) then datatype='troposhere'

;***********
;site codes:
;***********
;--- all sites (default)
site_code_all = strsplit('daw gdp khc',' ', /extract)

;--- check site codes
if(not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)

print, site_code

;***************
;data premane:
;***************
site_data_premane = strsplit('nD nG nK',' ', /extract)

;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'If you acquire meteor radar data, we ask that you' $
+ 'acknowledge us in your use of the data. This may be done by' $
+ 'including text such as the dawin donde campaine data provided by Research Institute' $
+ 'for Sustainable Humanosphere of Kyoto University. We would also' $
+ 'appreciate receiving a copy of the relevant publications.'


;Calculation of altitude:
;========================
height = fltarr(400)
height[0]=100
for i=0, 398 do begin
    height[i+1] = height[i]+100
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
       if site_code[ii] eq 'daw' then h=0
       if site_code[ii] eq 'gdp' then h=1
       if site_code[ii] eq 'khc' then h=2
       hour_res = 1
    ;Get files for ith component:
    ;***************************       
      file_names = file_dailynames( $
                   file_format='YYYYMM/'+site_data_premane[h]+$
                   'MMDDhh',trange=trange,hour_res=hour_res,times=times,/unique)+'.txt'
    ;        
    ;Define FILE_RETRIEVE structure:
    ;===============================
       source = file_retrieve(/struct)
       source.verbose=verbose
       source.local_data_dir =  root_data_dir() + 'iugonet/rish/DAWEX/'+site_code[ii]+'/sonde/'

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
       sonde_time = 0
       sonde_press = 0
       sonde_temp = 0
       sonde_rh = 0
       sonde_dewp = 0
       sonde_uwind = 0
       sonde_vwind = 0
       
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
           if (temp_name[3] eq 'OCT') || (temp_name[3] eq 'October') then month =10
           if (temp_name[3] eq 'NOV') || (temp_name[3] eq 'Novembre') then month =11
           if (temp_name[3] eq 'DEC') || (temp_name[3] eq 'December') then month =12
           if fix(temp_name[4]) gt 2000 then year = fix(temp_name[4])
           if fix(temp_name[4]) lt 2000 then year = 2000+fix(temp_name[4])
           day = fix(temp_name[2])
           hour = fix(strmid(temp_name[5],0,2))
           minute = fix(strmid(temp_name[5],2,2))
            print, year,month,day,hour,minute
           ;Read header: 
           readf,lun,s
           readf,lun,s  
       ;
       ;Loop on readdata:
       ;=================
       press = fltarr(1,400)
       temp = fltarr(1,400)
       rh = fltarr(1,400)
       dewp = fltarr(1,400)
       uwind = fltarr(1,400)
       vwind = fltarr(1,400)
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
                ;height[i] = float(data_comp[0])
                
                ;print,height[i]
                hh = height[i]            
                wbad = where(hh eq 0,nbad)
                if nbad gt 0 then hh[wbad] = !values.f_nan
                height[i] =hh 
              ;get data of press., temp., rh, dewp, uwind, vwind:
              ;=======================================================
               press[*,i] = float(data_comp[1])
               temp[*,i] = float(data_comp[2])
               rh[*,i] = float(data_comp[3])
               dewp[*,i] = float(data_comp[4])
               uwind[*,i] = float(data_comp[5])
               vwind[*,i] = float(data_comp[6])
 
                i=i+1 
                continue       
             endif
           endwhile 
           free_lun,lun
           for i=0, 399 do begin
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
                d = dewp[*,i]            
                wbad = where(d eq -999 || d eq 0,nbad)
                if nbad gt 0 then d[wbad] = !values.f_nan
                dewp[*,i] =d 
                e = uwind[*,i]            
                wbad = where(e eq -999 || e eq 0,nbad)
                if nbad gt 0 then e[wbad] = !values.f_nan
                uwind[*,i] =e 
                f = vwind[*,i]            
                wbad = where(f eq -999 || f eq 0,nbad)
                if nbad gt 0 then f[wbad] = !values.f_nan
                vwind[*,i] =f 
           endfor
           ;==== time from UT to UNIX Time
           time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+string(hour)+':'+string(minute))
          ;
          ;Append data :
          ;=============
           append_array, sonde_time, time
           append_array, sonde_press, press
           append_array, sonde_temp, temp
           append_array, sonde_rh, rh
           append_array, sonde_dewp, dewp
           append_array, sonde_uwind, uwind
           append_array, sonde_vwind, vwind
       endfor

      
 ;******************************
 ;Store data in TPLOT variables:
 ;******************************
       acknowledgstring = ''

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
          store_data,'iug_radiosonde_'+site_code[ii]+'_dewp',data={x:sonde_time, y:sonde_dewp, v:height},dlimit=dlimit
          options,'iug_radiosonde_'+site_code[ii]+'_dewp',ytitle='RSND-'+site_code[ii]+'!CHeight!C[m]',ztitle='Dewp.!C[deg.]'
          store_data,'iug_radiosonde_'+site_code[ii]+'_uwnd',data={x:sonde_time, y:sonde_uwind, v:height},dlimit=dlimit
          options,'iug_radiosonde_'+site_code[ii]+'_uwnd',ytitle='RSND-'+site_code[ii]+'!CHeight!C[m]',ztitle='uwnd!C[m/s]'
          store_data,'iug_radiosonde_'+site_code[ii]+'_vwnd',data={x:sonde_time, y:sonde_vwind, v:height},dlimit=dlimit
          options,'iug_radiosonde_'+site_code[ii]+'_vwnd',ytitle='RSND-'+site_code[ii]+'!CHeight!C[m]',ztitle='vwnd!C[m/s]'

          ; add options
          options, ['iug_radiosonde_'+site_code[ii]+'_press','iug_radiosonde_'+site_code[ii]+'_temp',$
                    'iug_radiosonde_'+site_code[ii]+'_rh','iug_radiosonde_'+site_code[ii]+'_dewp',$
                    'iug_radiosonde_'+site_code[ii]+'_uwnd','iug_radiosonde_'+site_code[ii]+'_vwnd'], 'spec', 1

          ; add options of setting labels
          options,'iug_radiosonde_'+site_code[ii]+'_press', labels='RSND'+site_code[ii]
          options,'iug_radiosonde_'+site_code[ii]+'_temp', labels='RSND'+site_code[ii]
          options,'iug_radiosonde_'+site_code[ii]+'_rh', labels='RSND'+site_code[ii]
          options,'iug_radiosonde_'+site_code[ii]+'_dewp', labels='RSND'+site_code[ii]
          options,'iug_radiosonde_'+site_code[ii]+'_uwnd', labels='RSND'+site_code[ii]
          options,'iug_radiosonde_'+site_code[ii]+'_vwnd', labels='RSND'+site_code[ii]
       endif 
       ;Clear time and data buffer:
       sonde_time = 0
       sonde_press = 0
       sonde_temp = 0
       sonde_rh = 0
       sonde_dewp = 0
       sonde_uwind = 0
       sonde_vwind = 0
       ; add tdegap
       tdegap, 'iug_radiosonde_'+site_code[ii]+'_press',/overwrite
       tdegap, 'iug_radiosonde_'+site_code[ii]+'_temp',/overwrite
       tdegap, 'iug_radiosonde_'+site_code[ii]+'_rh',/overwrite
       tdegap, 'iug_radiosonde_'+site_code[ii]+'_dewp',/overwrite
       tdegap, 'iug_radiosonde_'+site_code[ii]+'_uwnd',/overwrite
       tdegap, 'iug_radiosonde_'+site_code[ii]+'_vwnd',/overwrite
   endif 
   jj=n_elements(local_paths)
endfor 

print,'**********************************************************************************
print, 'Data loading is successful!!'
print,'**********************************************************************************

end

