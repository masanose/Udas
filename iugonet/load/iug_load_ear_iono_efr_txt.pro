;+
;
;NAME:
;iug_load_ear_iono_efr_txt
;
;PURPOSE:
;  Queries the Kyoto_RISH servers for the FAI observation data in the CSV format 
;  taken by the equatorial atmosphere radar (EAR) and loads data into
;  tplot format.
;
;SYNTAX:
; iug_load_ear_iono_efr_txt, datatype = datatype, parameter1=parameter1, parameter2=parameter2 $
;                          downloadonly=downloadonly, trange=trange, verbose=verbose
;
;KEYWOARDS:
;  datatype = Observation data type. For example, iug_load_ear_iono_efr_txt, datatype = 'fai'.
;            The default is 'fai'. 
;  parameter1 = first parameter name of EAR FAI obervation data.  
;          For example, iug_load_ear_iono_efr_txt, parameter1 = 'efb1p16'.
;          The default is 'all', i.e., load all available parameters.
;  parameter2 = second parameter name of EAR FAI obervation data.  
;          For example, iug_load_ear_iono_efr_txt, parameter2 = 'dpl1'.
;          The default is 'all', i.e., load all available parameters.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;DATA AVAILABILITY:
;  Please check the following homepage of the time schedule of field-aligned irregularity (FAI) observation 
;  before you analyze the FAI data using this software. 
;  http://www.rish.kyoto-u.ac.jp/ear/data-fai/index.html#data
;
;CODE:
; A. Shinbori, 19/09/2010.
;
;MODIFICATIONS:
; A. Shinbori, 24/03/2011.
; A. Shinbori, 06/10/2011.
; 
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_ear_iono_efr_txt, datatype = datatype, parameter1=parameter1, parameter2=parameter2,$
                              downloadonly=downloadonly, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2
 
;****************************************
;Load 'ionosphere' data by default:
;****************************************
if (not keyword_set(datatype)) then datatype='fai'

;***********
;parameters1:
;***********
;--- all parameters1 (default)
parameter1_all = strsplit('efb1p16 efb1p16a efb1p16b',' ', /extract)

;--- check site codes
if(not keyword_set(parameter1)) then parameter1='all'
parameters = thm_check_valid_name(parameter1, parameter1_all, /ignore_case, /include_all)

print, parameters


;***********
;parameters2:
;***********
;--- all parameters2 (default)
parameter2_all = strsplit('dpl1 pwr1 wdt1 pn1',' ', /extract)

;--- check parameters
if(not keyword_set(parameter2)) then parameter2='all'
parameters2 = thm_check_valid_name(parameter2, parameter2_all, /ignore_case, /include_all)

print, parameters2

;*****************
;defition of unit:
;*****************
;--- all parameters2 (default)
unit_all = strsplit('m/s dB',' ', /extract)

;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'If you acquire the equatorial atmospheric radar (EAR) data, ' $
+ 'we ask that you acknowledge us in your use of the data. This may be done by' $
+ 'including text such as EAR data provided by Research Institute' $
+ 'for Sustainable Humanosphere of Kyoto University. We would also' $
+ 'appreciate receiving a copy of the relevant publications.'


;******************************************************************
;Loop on downloading files
;******************************************************************
;Get timespan, define FILE_NAMES, and load data:
;===============================================
;

;Definition of parameter:
jj=0

for ii=0,n_elements(parameters)-1 do begin
  for iii=0,n_elements(parameters2)-1 do begin
    if ~size(fns,/type) then begin

    ;Get files for ith component:
    ;***************************
       file_names = file_dailynames( $
       file_format='YYYY/'+$
                   'YYYYMMDD',trange=trange,times=times,/unique)+'.fai'+parameters[ii]+'.'+parameters2[iii]+'.csv'
    ;
    ;Define FILE_RETRIEVE structure:
    ;===============================
       source = file_retrieve(/struct)
       source.verbose=verbose
       source.local_data_dir = root_data_dir() + 'iugonet/rish/misc/ktb/ear/fai/ef_region/csv/'
       source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/ear/data-fai/data/csv/'
    
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

   ;===========================================================
   ;read data, and create tplot vars at each parameter:
   ;===========================================================
   ;Read the files:
   ;===============
   
   ; Definition of parameters:
      s=''
      u=''

    ; Initialize data and time buffer
      ear_time=0
      ear_data=0
      time=0
      
    ;Loop on files (zonal component): 
    ;================================

      for h=jj,n_elements(local_paths)-1 do begin
          file= local_paths[h]
          if file_test(/regular,file) then  dprint,'Loading EAR file: ',file $
          else begin
             dprint,'EAR file ',file,' not found. Skipping'
             continue
          endelse  
          openr,lun,file,/get_lun  
          readf, lun, s  
    ;
    ;Read information of altitude:
    ;=============================
          
          readf, lun, s
          
          ;Definition of altitude and data arraies:
          h_data = strsplit(s,',',/extract)
          altitude = fltarr(n_elements(h_data)-1)
          
          ;Enter the altitude information:
          for j=0,n_elements(h_data)-2 do begin
              altitude[j] = float(h_data[j+1])
          endfor
          
          ;Enter the missing value:
          for j=0,n_elements(altitude)-1 do begin
              b = altitude[j]
              wbad = where(b eq 0,nbad)
              if nbad gt 0 then b[wbad] = !values.f_nan
              altitude[j]=b
          endfor
    ;
    ;Loop on readdata:
    ;=================
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
                time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+hour+':'+minute) $
                          -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(7)+':'+string(0)+':'+string(0))
            ;
            ;Enter the missing value:
                for j=0,n_elements(data)-2 do begin
                    a = float(data[j+1])
                    wbad = where(a eq -999,nbad)
                    if nbad gt 0 then a[wbad] = !values.f_nan
                    data[k,j]=a
                endfor
            ;
            ;Append data of time:
            ;====================
                append_array, ear_time, time
            ;
            ;Append data of wind velocity:
            ;=============================
                append_array, ear_data, data2

             endif
          endwhile 
          free_lun,lun  
      endfor

   ;******************************
   ;Store data in TPLOT variables:
   ;******************************

      if time ne 0 then begin
         if strmid(parameters2[iii],0,2) eq 'dp' then o=0
         if strmid(parameters2[iii],0,2) eq 'wd' then o=0 
         if strmid(parameters2[iii],0,2) eq 'pw' then o=1
         if strmid(parameters2[iii],0,2) eq 'pn' then o=1
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'M. Yamamoto'))
         store_data,'iug_ear_fai_'+parameters[ii]+'_'+parameters2[iii],data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
         options,'iug_ear_fai_'+parameters[ii]+'_'+parameters2[iii],ytitle='EAR-FAI!CHeight!C[km]',ztitle=parameters2[iii]+'!C['+unit_all[o]+']'
         options,'iug_ear_fai_'+parameters[ii]+'_'+parameters2[iii], labels='EAR-FAI E/F-region [km]'
         ; add options
         if strmid(parameters2[iii],0,2) ne 'np' then options, 'iug_ear_fai_'+parameters[ii]+'_'+parameters2[iii], 'spec', 1         
      endif 
       ;Clear time and data buffer:
      ear_time=0
      ear_data=0
         ; add tdegap
         tdegap, 'iug_ear_fai_'+parameters[ii]+'_'+parameters2[iii],/overwrite
   endif
   jj=n_elements(local_paths)
  endfor
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
print, 'If you acquire the equatorial atmospheric radar (EAR) data, '
print, 'we ask that you acknowledge us in your use of the data. ' 
print, 'This may be done by including text such as EAR data provided ' 
print, 'by Research Institute for Sustainable Humanosphere of Kyoto University. ' 
print, 'We would also appreciate receivinga copy of the relevant publications.' 


end

