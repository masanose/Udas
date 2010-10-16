;+
;
;Name:
;iug_load_mf_rish_pon_txt
;
;Purpose:
;  Queries the Kyoto_RISH renkei2 servers for pontianak data and loads data into
;  tplot format.
;
;Syntax:
; iug_load_mf_rish_pon_txt, datatype = datatype, site = site, downloadonly = downloadonly, trange = trange, verbose = verbose
;
;Keywords:
;  datatype = Observation data type. For example, iug_load_mf_rish_pon_txt, datatype = 'thermosphere'.
;            The default is 'thermosphere'. 
;   site  = Observatory code name.  For example, iug_load_meteor_rish_txt, site = 'pon'.
;          The default is 'all', i.e., load all available stations.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;
;Code:
;  A. Shinbori, 10/09/2010.
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

pro iug_load_mf_rish_pon_txt, datatype = datatype, site=site, downloadonly=downloadonly, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
;verbose
if ~keyword_set(verbose) then verbose=2

;*****************************************
;dataype check:
;*****************************************
if (not keyword_set(dataype)) then datatype='thermosphere'

;***********
;site codes:
;***********
if (not keyword_set(site)) then site='pon'

;--- all sites (default)
site_code_all = site

;--- check site codes
if(not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)

print, site_code

;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'If you acquire MF radar data, we ask that you' $
+ 'acknowledge us in your use of the data. This may be done by' $
+ 'including text such as the pontianak data provided by Research Institute' $
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
    file_names = file_dailynames( $
      file_format='YYYYMM/'+$
      'YYYYMMDD',trange=trange,times=times,/unique)+'.txt'
    ;        
    ;Define FILE_RETRIEVE structure:
    ;===============================
    source = file_retrieve(/struct)
    source.verbose=verbose
    source.local_data_dir =  root_data_dir() + 'iugonet/rish/misc/'+site_code+'/mf/
    ;source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/ear/data/data/ver02.0212/'
    
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
  height = fltarr(21)
  zon_wind_data = fltarr(1,21)
  mer_wind_data = fltarr(1,21)
  time = dblarr(1)

  ; Initialize data and time buffer
  pon_time=0
  zon_wind=0
  mer_wind=0

  ;Loop on files: 
  ;==============
  for j=0,n_elements(local_paths)-1 do begin
      file= local_paths[j]
      if file_test(/regular,file) then  dprint,'Loading Pontianak file: ',file $
      else begin
         dprint,'Pontianak file ',file,' not found. Skipping'
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
           year = fix(strmid(s,0,4))
           month = fix(strmid(s,4,2))
           day = fix(strmid(s,6,2))
           hour = fix(strmid(s,8,2))
           minute = fix(strmid(s,10,2))
         
         ;Read data set:
         ;==============
           for k=0,6 do begin
               readf,lun,s
               data1 = strsplit(s,' ',/EXTRACT)
               for l=0,2 do begin
                   height(k*3+l) = float(data1(l*3))
                   zon_wind_data(0,k*3+l) = float(data1(l*3+1))
                   mer_wind_data(0,k*3+l) = float(data1(l*3+2))
                   a = zon_wind_data(0,k*3+l)            
                   wbad = where(a eq 0,nbad)
                   if nbad gt 0 then a[wbad] = !values.f_nan
                   zon_wind_data(0,k*3+l)=a
                   b = mer_wind_data(0,k*3+l)
                   wbad = where(b eq 0,nbad)
                   if nbad gt 0 then b[wbad] = !values.f_nan
                   mer_wind_data(0,k*3+l)=b
               endfor      
           endfor

         ;
         ;Append data of time and U, V components at determined altitude:
         ;=============================================================== 
         ;====convert time from LT to UT   
           time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+string(hour)+':'+string(minute))$
                  -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(7)+':'+string(0))
      
           append_array, pon_time, time
           append_array, zon_wind, zon_wind_data
           append_array, mer_wind, mer_wind_data            
           continue       
        endif
      endwhile 
      free_lun,lun
  endfor

;******************************
;Store data in TPLOT variables:
;******************************
  acknowledgstring = ''

  if time ne 0 then begin
;Store data of pontianak wind data:
;==================================

      dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'T. Tsuda'))
      store_data,'iug_mf_'+site_code[0]+'_uwnd',data={x:pon_time, y:zon_wind, v:height},dlimit=dlimit
      options,'iug_mf_'+site_code[0]+'_uwnd',ytitle='MF-pon!Cheight!C[m]',ztitle='uwnd [m/s]'
      store_data,'iug_mf_'+site_code[0]+'_vwnd',data={x:pon_time, y:mer_wind, v:height},dlimit=dlimit
      options,'iug_mf_'+site_code[0]+'_vwnd',ytitle='MF-pon!Cheight!C[m]',ztitle='vwnd [m/s]'
  

    ; add options
      options, ['iug_mf_'+site_code[0]+'_uwnd','iug_mf_'+site_code[0]+'_vwnd'], 'spec', 1
  
    ; add options of setting lanels
      options, 'iug_mf_'+site_code[0]+'_uwnd', labels='MFR-pon [km]'
      options, 'iug_mf_'+site_code[0]+'_vwnd', labels='MFR-pon [km]'
    endif
  ;Clear time and data buffer:
  pon_time=0
  zon_wind=0
  mer_wind=0

endif
          
print,'**********************************************************************************
print, 'Data loading is successful!!'
print,'**********************************************************************************

end

