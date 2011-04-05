;+
;
;NAME:
;iug_load_ltr_rish_txt
;
;PURPOSE:
;  Queries the Kyoto_RISH servers for the observation data (uwnd, vwnd, wwnd, pwr1-5, wdt1-5)
;  in the CSV format taken by the L-band lower troposphere radar (LTR)and loads data into
;  tplot format.
;
;SYNTAX:
; iug_load_ltr_rish_txt, datatype = datatype, site=site, parameter=parameter, $
;                        downloadonly=downloadonly, trange=trange, verbose=verbose
;
;KEYWOARDS:
;  datatype = Observation data type. For example, iug_load_ltr_rish_txt, datatype = 'troposphere'.
;            The default is 'troposphere'. 
;   site = LTR observation site.  
;          For example, iug_load_ltr_rish_txt, site = 'sgk'.
;          The default is 'all', i.e., load all available observation points.
;  parameter = parameter name of LTR obervation data.  
;          For example, iug_load_ltr_rish_txt, parameter = 'uwnd'.
;          The default is 'all', i.e., load all available parameters.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;CODE:
; A. Shinbori, 09/19/2010.
;
;MODIFICATIONS:
; A. Shinbori, 03/24/2011.
;
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_ltr_rish_txt, datatype = datatype, site=site, parameter=parameter, $
                           downloadonly=downloadonly, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2

;**************
;datatype check:
;**************
if (not keyword_set(datatype)) then datatype= 'troposphere'

;***********
;site codes:
;***********
;--- all sites (default)
site_code_all = strsplit('sgk',' ', /extract)

;--- check site codes
if(not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)

print, site_code
 
;***********
;parameters:
;***********
;--- all parameters (default)
parameter_all = strsplit('uwnd vwnd wwnd pwr1 pwr2 pwr3 pwr4 pwr5 wdt1 wdt2 wdt3 wdt4 wdt5',' ', /extract)

;--- check parameters
if(not keyword_set(parameter)) then parameter='all'
parameters = thm_check_valid_name(parameter, parameter_all, /ignore_case, /include_all)

print, parameters

;*****************
;defition of unit:
;*****************
;--- all parameters (default)
unit_all = strsplit('m/s dB',' ', /extract)

;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'If you acquire the lower troposphere radar (LTR) data, ' $
+ 'we ask that you acknowledge us in your use of the data. This may be done by' $
+ 'including text such as the LTR data provided by Research Institute' $
+ 'for Sustainable Humanosphere of Kyoto University. We would also' $
+ 'appreciate receiving a copy of the relevant publications.'


;******************************************************************
;Loop on downloading files
;******************************************************************
;Get timespan, define FILE_NAMES, and load data:
;===============================================
;
;==================================================================
;Download files, read data, and create tplot vars at each component
;==================================================================
;******************************************************************
;Loop on downloading files
;******************************************************************
;Get timespan, define FILE_NAMES, and load data:
;===============================================
;

jj=0
o=0
for iii=0,n_elements(parameters)-1 do begin
    if ~size(fns,/type) then begin

    ;Get files for ith component:
    ;***************************
         file_names = file_dailynames( $
         file_format='YYYYMM/YYYYMMDD/'+$
                     'YYYYMMDD',trange=trange,times=times,/unique)+'.'+parameters[iii]+'.csv'
    ;
    ;Define FILE_RETRIEVE structure:
    ;===============================
         source = file_retrieve(/struct)
         source.verbose=verbose
         source.local_data_dir = root_data_dir() + 'iugonet/rish/misc/sgk/ltr/csv/'
         source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/radar-group/blr/shigaraki/data/data/ver02.0212/'
    
    ;Get files and local paths, and concatenate local paths:
    ;=======================================================
         local_paths=file_retrieve(file_names,_extra=source)
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
         u=''
         time = dblarr(1)

      ; Initialize data and time buffer
         ltr_data = 0
         ltr_time = 0

      ;Loop on files (zonal component): 
      ;================================
         for h=jj,n_elements(local_paths)-1 do begin
             file= local_paths[h]
             if file_test(/regular,file) then  dprint,'Loading LTR-shigaraki file: ',file $
             else begin
                dprint,'LTR-shigaraki file ',file,' not found. Skipping'
                continue
             endelse
             openr,lun,file,/get_lun    
      ;
      ;Read information of altitude:
      ;=============================
             readf, lun, s
             height = strsplit(s,',',/extract)
             
             ;Definition of altitude and data arraies:
             altitude = fltarr(70)
             data = strarr(70)
             data2 = fltarr(1,70)
             
             for j=0,n_elements(height)-2 do begin
                 altitude[j] = float(height[j+1])
             endfor

             for j=0, n_elements(altitude)-1 do begin
                 b = float(altitude[j])
                 wbad = where(b eq 0,nbad)
                 if nbad gt 0 then b[wbad] = !values.f_nan
                 data[j] = !values.f_nan
                 data2[j] = !values.f_nan
                 altitude[j]=b
             endfor

      ;
      ;Loop on readdata:
      ;=================
             k=0
             while(not eof(lun)) do begin
               readf,lun,s
               ok=1
               if strmid(s,0,1) eq '[' then ok=0
               if ok && keyword_set(s) then begin
                  dprint,s ,dlevel=5
                  data = strsplit(s,',',/extract)
         
            ;Calcurate time:
            ;==============
                  u=data(0)
                  year = strmid(u,0,4)
                  month = strmid(u,5,2)
                  day = strmid(u,8,2)
                  hour = strmid(u,11,2)
                  minute = strmid(u,14,2)  
            ;====convert time from LT to UT 
                 time[k] = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+hour+':'+minute) $
                               -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(9)+':'+string(0)+':'+string(0))
                 if time[k] lt time_double(string(1992)+'-'+string(9)+'-'+string(1)+'/'+string(0)+':'+string(0)+':'+string(0)) then break
            ;
                  for j=0,n_elements(height)-2 do begin
                      a = float(data[j+1])
                      wbad = where(a eq 999, nbad)
                      if nbad gt 0 then a[wbad] = !values.f_nan
                      data2[k,j]=a
                  endfor
            ;
            ;Append data of time:
            ;====================
                  append_array, ltr_time, time
            ;
            ;Append data of wind velocity:
            ;=============================
                  append_array, ltr_data, data2
    
               endif
             endwhile 
             free_lun,lun  
          endfor
   
  ;******************************
  ;Store data in TPLOT variables:
  ;******************************

          if ltr_time[0] ne 0 then begin 
             o=0 
             if parameters[iii] eq 'pwr1' then o=1  
             if parameters[iii] eq 'pwr2' then o=1
             if parameters[iii] eq 'pwr3' then o=1
             if parameters[iii] eq 'pwr4' then o=1
             if parameters[iii] eq 'pwr5' then o=1
 
             dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'H. Hashiguchi'))
             store_data,'iug_ltr_'+site_code[0]+'_'+parameters[iii],data={x:ltr_time, y:ltr_data, v:altitude},dlimit=dlimit
             options,'iug_ltr_'+site_code[0]+'_'+parameters[iii],ytitle='LTR-'+site_code[0]+'!CHeight!C[km]',$
                     ztitle=parameters[iii]+'!C['+unit_all[o]+']'
             options,'iug_ltr_'+site_code[0]+'_'+parameters[iii], labels='LTR-'+site_code[0]+' [km]'
             ; add options
             options, 'iug_ltr_'+site_code[0]+'_'+parameters[iii], 'spec', 1    
          endif

          ;Clear time and data buffer:
          ltr_data = 0
          ltr_time = 0
          
          ; add tdegap
         tdegap, 'iug_ltr_'+site_code[0]+'_'+parameters[iii],/overwrite
   endif
   jj=n_elements(local_paths)
endfor 
   
print,'*****************************
print,'Data loading is successful!!'
print,'*****************************

;******************************
;print of acknowledgement:
;******************************
print, '****************************************************************
print, 'Acknowledgement'
print, '****************************************************************
print, 'If you acquire LTR data, we ask that you acknowledge us'
print, 'in your use of the data. This may be done by including text' 
print, 'such as LTR data provided by Research Institute for Sustainable' 
print, 'Humanosphere of Kyoto University. We would also appreciate receiving' 
print, 'a copy of the relevant publications.'

end

