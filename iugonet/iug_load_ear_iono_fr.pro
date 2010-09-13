;+
;
;Name:
;iug_load_ear_iono_fr
;
;Purpose:
;  Queries the Kyoto_RISH servers for EAR data and loads data into
;  tplot format.
;
;Syntax:
; iug_load_ear_iono_fr [ ,DATATYPE = string ]
;                      [ ,SITE_OR_PARAM = string ]
;                      [ ,PARAMETERS = string ]
;                      [ ,TRANGE = [min,max] ]
;                      [ ,FILENAMES = string scalar or array ]
;                      [ ,<and data keywords below> ]
;
;Keywords:
;  DATATYPE (I/O):
;    Set to datatype.  If not set, 'iono_fr_dpl' is
;      assumed.  Returns cleaned input, or shows default. 
;  SITE_OR_PARAM (I/O):
;    Set to parameters.  If not set, 'fr_dpl_beam1' is
;      assumed.  Returns cleaned input, or shows default.       
;  PARAMETERS (I/O):
;    Set to file name.  If not set, 'faifb1p16a' is
;      assumed.  Returns cleaned input, or shows default.       
;  TRANGE (In):
;    Pass a time range a la TIME_STRING.PRO.
;  FILENAMES (In):
;    *PRESENTLY DISABLED* Pass user-defined file names (full paths to local data files).  These will
;      be read a la the RISH format, and the RISH server will not be queried.
;  VERBOSE (In): [1,...,5], Get more detailed (higher number) command line output.
;
;Code:
; A. Shinbori, 02/07/2010.
;
;Modifications:
;
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_ear_iono_fr, datatype=datatype, site_or_param =site_or_param, parameters=parameters, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
if ~keyword_set(verbose) then verbose=2
 
;****************************************
;Load 'troposphere_wind' data by default:
;****************************************
if ~keyword_set(datatype) then datatype='iono_fr_dpl'

;************************************
;Load wind component data by default:
;************************************
if ~keyword_set(site_or_param) then site_or_param='fr_dpl_beam1'

;************************************
;Selection of file name by default:
;************************************
if ~keyword_set(parameters) then parameters='faifb1p16a'

;*******************
;Validate datatypes:
;*******************
vns =datatype
if size(datatype,/type) eq 7 then begin
  datatype=thm_check_valid_name(datatype,vns,/ignore_case,/include_all,/no_warning)
  if datatype[0] eq '' then return
endif else begin
  message,'DATATYPE must be of string type.',/info
  return
endelse

;Data component
if datatype eq 'iono_fr_dpl' then begin
   vdcname = strmid(site_or_param, 0,12)
   
   if vdcname eq 'fr_dpl_beam1' then vdcnames = '1'
   if vdcname eq 'fr_dpl_beam2' then vdcnames = '2'
   if vdcname eq 'fr_dpl_beam3' then vdcnames = '3'
   if vdcname eq 'fr_dpl_beam4' then vdcnames = '4'
   if vdcname eq 'fr_dpl_beam5' then vdcnames = '5'
endif else if datatype eq 'iono_fr_pwr' then begin
   vdcname = strmid(site_or_param, 0,12)

   if vdcname eq 'fr_pwr_beam1' then vdcnames = '1'
   if vdcname eq 'fr_pwr_beam2' then vdcnames = '2'
   if vdcname eq 'fr_pwr_beam3' then vdcnames = '3'
   if vdcname eq 'fr_pwr_beam4' then vdcnames = '4'
   if vdcname eq 'fr_pwr_beam5' then vdcnames = '5'
endif else if datatype eq 'iono_fr_spec_width' then begin
   vdcname = strmid(site_or_param, 0,11)

   if vdcname eq 'fr_sw_beam1' then vdcnames = '1'
   if vdcname eq 'fr_sw_beam2' then vdcnames = '2'
   if vdcname eq 'fr_sw_beam3' then vdcnames = '3'
   if vdcname eq 'fr_sw_beam4' then vdcnames = '4'
   if vdcname eq 'fr_sw_beam5' then vdcnames = '5'
endif else if datatype eq 'iono_fr_noise_lev' then begin
   vdcname = strmid(site_or_param, 0,11)

   if vdcname eq 'fr_pn_beam1' then vdcnames = '1'
   if vdcname eq 'fr_pn_beam2' then vdcnames = '2'
   if vdcname eq 'fr_pn_beam3' then vdcnames = '3'
   if vdcname eq 'fr_pn_beam4' then vdcnames = '4'
   if vdcname eq 'fr_pn_beam5' then vdcnames = '5'
endif
;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'If you acquire EAR data, we ask that you' $
+ 'acknowledge us in your use of the data. This may be done by' $
+ 'including text such as EAR data provided by Research Institute' $
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
;
if ~size(fns,/type) then begin

    ;Get files for ith component:
    ;***************************
    if datatype eq 'iono_fr_dpl' then begin
      file_names = file_dailynames( $
        file_format='YYYY/'+$
        'YYYYMMDD',trange=trange,times=times,/unique)+'.'+parameters+'.dpl'+vdcnames+'.csv'
    endif else if datatype eq 'iono_fr_pwr' then begin
      file_names = file_dailynames( $
        file_format='YYYY/'+$
        'YYYYMMDD',trange=trange,times=times,/unique)+'.'+parameters+'.pwr'+vdcnames+'.csv'
    endif else if datatype eq 'iono_fr_spec_width' then begin
      file_names = file_dailynames( $
        file_format='YYYY/'+$
        'YYYYMMDD',trange=trange,times=times,/unique)+'.'+parameters+'.wdt'+vdcnames+'.csv'
    endif else if datatype eq 'iono_fr_noise_lev' then begin
      file_names = file_dailynames( $
        file_format='YYYY/'+$
        'YYYYMMDD',trange=trange,times=times,/unique)+'.'+parameters+'.pn'+vdcnames+'.csv'
    endif
    ;
    ;Define FILE_RETRIEVE structure:
    ;===============================
    source = file_retrieve(/struct)
    source.verbose=verbose
    source.local_data_dir = root_data_dir() + 'iugonet/rish/ear/ionosphere/'
    source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/ear/data-fai/data/csv/'
    
    ;Get files and local paths, and concatenate local paths:
    ;=======================================================
    local_paths=file_retrieve(file_names,_extra=source)
    local_paths_all = ~(~size(local_paths_all,/type)) ? $
      [local_paths_all, local_paths] : local_paths
  if ~(~size(local_paths_all,/type)) then local_paths=local_paths_all
endif else file_names=fns

;Read the files:
;===============
s=''
u=''
range = fltarr(257)
altitude = fltarr(256)
height = fltarr(257)
data = strarr(257)
data2 = fltarr(1,256)
time = dblarr(1)

if datatype ne 'iono_fr_noise_lev' then begin

 for i=0,n_elements(local_paths)-1 do begin
    file= local_paths[i]
    if file_test(/regular,file) then  dprint,'Loading EAR-fai file: ',file $
    else begin
         dprint,'EAR-fai file ',file,' not found. Skipping'
         continue
    endelse
    openr,lun,file,/get_lun    
    ;
    ;Read information of range and altitude:
    ;=======================================
    readf, lun, s
    range = float(strsplit(s,',',/extract))
    readf, lun, s
    height = float(strsplit(s,',',/extract))
    height2 = float(strsplit(s,',',/extract))
    number2 = n_elements(height2)
    print, number2
    for j=0,number2-2 do begin
     altitude[j] = height[j+1]
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
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(7)+':'+string(0)+':'+string(0))
                   
         ;
         for j=0,number2-2 do begin
          a = float(data[j+1])
          wbad = where(a eq -999 or 0,nbad)
          if nbad gt 0 then a[wbad] = !values.f_nan
          data2[k,j]=a
         endfor
           ;
           ;Append data of time:
           ;====================
            append_array, ear_time, time
           ;
           ;Append data of wind velocity:
           ;=============================
            append_array, ear_data, data2
    
         continue
      endif
    endwhile 
    free_lun,lun  
endfor

if time ne 0 then begin
   
   if datatype eq 'iono_fr_dpl' then begin 
  ;Store data of dvr_beam1:
  ;============================
      if site_or_param eq 'fr_dpl_beam1' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'fr_dpl_beam1',data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
      endif
  ;Store data of dvr_beam2:
  ;==============================
      if site_or_param eq 'fr_dpl_beam2' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'fr_dpl_beam2',data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
      endif
  ;Store data of dvr_beam3:
  ;============================
      if site_or_param eq 'fr_dpl_beam3' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'fr_dpl_beam3',data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of dvr_beam4:
  ;==============================
      if site_or_param eq 'fr_dpl_beam4' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'fr_dpl_beam4',data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
      endif
  ;Store data of dvr_beam5:
  ;============================
      if site_or_param eq 'fr_dpl_beam5' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'fr_dpl_beam5',data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
      endif
   endif else if datatype eq 'iono_fr_pwr' then begin
    ;Store data of beam1 echo intensity:
    ;===================================
      if site_or_param eq 'fr_pwr_beam1' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'fr_pwr_beam1',data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam2 echo intensity:
    ;===================================
      if site_or_param eq 'fr_pwr_beam2' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'fr_pwr_beam2',data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam3 echo intensity:
    ;===================================
      if site_or_param eq 'fr_pwr_beam3' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'fr_pwr_beam3',data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam4 echo intensity:
    ;===================================
      if site_or_param eq 'fr_beam4' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'fr_pwr_beam4',data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam5 echo intensity:
    ;===================================
      if site_or_param eq 'fr_pwr_beam5' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'fr_pwr_beam5',data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
      endif
   endif else if datatype eq 'iono_fr_spec_width' then begin
    ;Store data of beam1 spectral width:
    ;===================================
      if site_or_param eq 'fr_sw_beam1' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'fr_sw_beam1',data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam2 spectral width:
    ;===================================
      if site_or_param eq 'fr_sw_beam2' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'fr_sw_beam2',data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam3 spectral width:
    ;===================================
      if site_or_param eq 'fr_sw_beam3' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'fr_sw_beam3',data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam4 spectral width:
    ;===================================
      if site_or_param eq 'fr_sw_beam4' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'fr_sw_beam4',data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam5 spectral width:
    ;===================================
      if site_or_param eq 'fr_sw_beam5' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'fr_sw_beam5',data={x:ear_time, y:ear_data, v:altitude},dlimit=dlimit
      endif
   endif 

; add options
   options, site_or_param, 'spec', 1
endif 
endif
 
if datatype eq 'iono_fr_noise_lev' then begin

;Loop on files (noise level): 
;================================
 for i=0,n_elements(local_paths)-1 do begin
    file= local_paths[i]
    if file_test(/regular,file) then  dprint,'Loading EAR-fai file: ',file $
    else begin
         dprint,'EAR-fai file ',file,' not found. Skipping'
         continue
    endelse
    openr,lun,file,/get_lun   

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
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(6)+':'+string(41)+':'+string(12))
                   
         ;
         data2=float(data(1))
           ;
           ;Append data of time:
           ;====================
            append_array, ear_time, time
           ;
           ;Append data of wind velocity:
           ;=============================
            append_array, ear_data, data2
    
         continue
      endif
    endwhile 
    free_lun,lun  
endfor

if time ne 0 then begin
   
 if datatype eq 'iono_fr_noise_lev' then begin
    ;Store data of beam1 noise level:
    ;===================================
   if site_or_param eq 'fr_pn_beam1' then begin
      dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
      store_data,'fr_pn_beam1',data={x:ear_time, y:ear_data},dlimit=dlimit
   endif
    ;Store data of beam2 noise level:
    ;===================================
   if site_or_param eq 'fr_pn_beam2' then begin
      dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
      store_data,'fr_pn_beam2',data={x:ear_time, y:ear_data},dlimit=dlimit
   endif
    ;Store data of beam3 noise level:
    ;===================================
   if site_or_param eq 'fr_pn_beam3' then begin
      dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
      store_data,'fr_pn_beam3',data={x:ear_time, y:ear_data},dlimit=dlimit
   endif
    ;Store data of beam4 noise level:
    ;===================================
   if site_or_param eq 'fr_pn_beam4' then begin
      dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
      store_data,'fr_pn_beam4',data={x:ear_time, y:ear_data},dlimit=dlimit
   endif
    ;Store data of beam5 noise level:
    ;===================================
   if site_or_param eq 'fr_pn_beam5' then begin
      dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
      store_data,'fr_pn_beam5',data={x:ear_time, y:ear_data},dlimit=dlimit
   endif
  endif
 endif 
endif

;Clear time and data buffer:
ear_data = 0
ear_time = 0

print,'**********************************************************************************
print,'Data loading is successful!!'
print,'**********************************************************************************

end