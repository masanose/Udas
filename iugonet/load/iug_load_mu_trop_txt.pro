;+
;
;NAME:
;iug_load_mu_trop_txt
;
;PURPOSE:
;  Queries the Kyoto_RISH servers for the standard observation data of the 
;  troposphere and lower stratsphere in the CSV format taken by the Middle and 
;  Upper atmosphere (MU) radar at Shigaraki and loads data into tplot format.
;
;SYNTAX:
; iug_load_ear_trop_txt, datatype = datatype, parameter=parameter, $
;                        downloadonly=downloadonly, trange=trange, verbose=verbose
;
;KEYWOARDS:
;  datatype = Observation data type. For example, iug_load_mu_trop_txt, datatype = 'troposphere'.
;            The default is 'troposphere'. 
;  parameter = parameter name of MU troposphere standard obervation data.  
;          For example, iug_load_mu_trop_txt, parameter = 'uwnd'.
;          The default is 'all', i.e., load all available parameters.
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
; A. Shinbori, 13/11/2011.
; A. Shinbori, 26/12/2011.
; A. Shinbori, 31/01/2012.
; A. Shinbori, 19/12/2012.
; 
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_mu_trop_txt, datatype = datatype, $
  parameter=parameter, $
  downloadonly=downloadonly, $
  trange=trange, $
  verbose=verbose

;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2
 
;************************************
;Load 'toroposphere' data by default:
;************************************
if (not keyword_set(datatype)) then datatype='troposphere'

;***********
;parameters:
;***********
;--- all parameters (default)
parameter_all = strsplit('uwnd vwnd wwnd pwr1 pwr2 pwr3 pwr4 pwr5 wdt1 wdt2 wdt3 wdt4 wdt5',' ', /extract)

;--- check site codes
if(not keyword_set(parameter)) then parameter='all'
parameters = thm_check_valid_name(parameter, parameter_all, /ignore_case, /include_all)

print, parameters

;*****************
;defition of unit:
;*****************
;--- all parameters (default)
unit_all = strsplit('m/s dB',' ', /extract)

;******************************************************************
;Loop on downloading files
;******************************************************************
;Get timespan, define FILE_NAMES, and load data:
;===============================================
;
;===================================================================
;Download files, read data, and create tplot vars at each component:
;===================================================================
;Definition of parameter:
jj=0

for ii=0,n_elements(parameters)-1 do begin
   if ~size(fns,/type) then begin

     ;Get files for ith component:
     ;***************************
      file_names = file_dailynames( $
      file_format='YYYYMM/YYYYMMDD/'+$
                   'YYYYMMDD',trange=trange,times=times,/unique)+'.'+parameters[ii]+'.csv'
     ;
     ;Define FILE_RETRIEVE structure:
     ;===============================
      source = file_retrieve(/struct)
      source.verbose=verbose
      source.local_data_dir = root_data_dir() + 'iugonet/rish/misc/mu/troposphere/csv/'
      source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/radar-group/mu/data/data/ver01.0807/'
    
     ;Get files and local paths, and concatenate local paths:
     ;=======================================================
      local_paths=file_retrieve(file_names,_extra=source)
      local_paths_all = ~(~size(local_paths_all,/type)) ? $
                        [local_paths_all, local_paths] : local_paths
      if ~(~size(local_paths_all,/type)) then local_paths=local_paths_all
   endif else file_names=fns

  ;--- Load data into tplot variables
   if (not keyword_set(downloadonly)) then downloadonly=0

   if (downloadonly eq 0) then begin

     ;===========================================================
     ;Loop on read data, and create tplot vars at each parameter
     ;===========================================================
     ;Read the files:
     ;===============
   
     ;Definition of parameters and array:
      s=''
      u=''

     ;Initialize data and time buffer
      mu_time=0
      mu_data=0
     
     ;============= 
     ;Loop on files: 
     ;==============

      for h=jj,n_elements(local_paths)-1 do begin
         file= local_paths[h]
         if file_test(/regular,file) then  dprint,'Loading MU file: ',file $
         else begin
            dprint,'MU file',file,'not found. Skipping'
            continue
         endelse
            
         openr,lun,file,/get_lun    
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
               data2 = fltarr(1,n_elements(data)-1)
               
              ;Calculate time:
              ;==============
               u=data(0)
               year = strmid(u,0,4)
               month = strmid(u,5,2)
               day = strmid(u,8,2)
               hour = strmid(u,11,2)
               minute = strmid(u,14,2)  
                
              ;====Convert time from local time to unix time      
               time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+hour+':'+minute) $
                      -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(9)+':'+string(0)+':'+string(0))
              ;
              ;Enter the missing value:
               for j=0,n_elements(h_data)-2 do begin
                  a = float(data[j+1])
                  wbad = where(a eq 999,nbad)
                  if nbad gt 0 then a[wbad] = !values.f_nan
                  data2[0,j]=a
               endfor
              ;==============================
              ;Append array of time and data:
              ;==============================
               append_array, mu_time, time
               append_array, mu_data, data2
            endif
         endwhile 
         free_lun,lun  
      endfor
      
     ;==============================
     ;Store data in TPLOT variables:
     ;==============================
     ;Acknowlegment string (use for creating tplot vars)
      acknowledgstring = 'If you acquire the middle and upper atmospher (MU) radar data, ' $
                       + 'we ask that you acknowledge us in your use of the data. This may be done by ' $
                       + 'including text such as the MU data provided by Research Institute ' $
                       + 'for Sustainable Humanosphere of Kyoto University. We would also ' $
                       + 'appreciate receiving a copy of the relevant publications. '$
                       + 'The distribution of MU radar data has been partly supported by the IUGONET '$
                       + '(Inter-university Upper atmosphere Global Observation NETwork) project '$
                       + '(http://www.iugonet.org/) funded by the Ministry of Education, Culture, '$
                       + 'Sports, Science and Technology (MEXT), Japan.'
       o=0
       if time ne 0 then begin
          if strmid(parameters[ii],0,2) eq 'pw' then o=1
          dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'M. Yamamoto'))
          store_data,'iug_mu_trop_'+parameters[ii],data={x:mu_time, y:mu_data, v:altitude},dlimit=dlimit
          new_vars=tnames('iug_mu_trop_*')
          if new_vars[0] ne '' then begin          
             options,'iug_mu_trop_'+parameters[ii],ytitle='MU-trop!CHeight!C[km]',ztitle=parameters[ii]+'!C['+unit_all[o]+']'
             options,'iug_mu_trop_'+parameters[ii], labels='MU-trop [km]'
          endif
       endif   
   
       new_vars=tnames('iug_mu_trop_*')
       ; Add options
       if new_vars[0] ne '' then options, 'iug_mu_trop_'+parameters[ii], 'spec', 1
    
      ;Clear time and data buffer:
       mu_time=0
       mu_data=0
       
      ;Add tdegap
       if new_vars[0] ne '' then tdegap, 'iug_mu_trop_'+parameters[ii],/overwrite
   endif
  jj=n_elements(local_paths)
endfor

new_vars=tnames('iug_mu_trop_*')
if new_vars[0] ne '' then begin
   print,'*****************************
   print,'Data loading is successful!!'
   print,'*****************************
endif

;*************************
;print of acknowledgement:
;*************************
print, '****************************************************************
print, 'Acknowledgement'
print, '****************************************************************
print, 'If you acquire the middle and upper atmosphere (MU) radar data,'
print, 'we ask that you acknowledge us in your use of the data.' 
print, 'This may be done by including text such as MU data provided' 
print, 'by Research Institute for Sustainable Humanosphere of Kyoto University.' 
print, 'We would also appreciate receiving a copy of the relevant publications.'
print, 'The distribution of MU radar data has been partly supported by the IUGONET'
print, '(Inter-university Upper atmosphere Global Observation NETwork) project'
print, '(http://www.iugonet.org/) funded by the Ministry of Education, Culture,'
print, 'Sports, Science and Technology (MEXT), Japan.' 

end

