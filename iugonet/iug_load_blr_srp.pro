;+
;
;Name:
;iug_load_blr_spr
;
;Purpose:
;  Queries the Kyoto_RISH servers for blr data and loads data into
;  tplot format.
;
;Syntax:
; iug_load_blr_spr  [ ,DATATYPE = string ]
;                   [ ,PARAMETERS = string ]
;                   [ ,TRANGE = [min,max] ]
;                   [ ,FILENAMES = string scalar or array ]
;                   [ ,<and data keywords below> ]
;
;Keywords:
;  DATATYPE (I/O):
;    Set to 'blr_wind_srp'.  If not set, 'blr_wind_srp' is
;      assumed.  Returns cleaned input, or shows default. 
;  PARAMETERS (I/O):
;    Set to wind parameters.  If not set, 'srp_zonal_wind' is
;      assumed.  Returns cleaned input, or shows default.       
;  TRANGE (In):
;    Pass a time range a la TIME_STRING.PRO.
;  FILENAMES (In):
;    *PRESENTLY DISABLED* Pass user-defined file names (full paths to local data files).  These will
;      be read a la the RISH format, and the RISH server will not be queried.
;  VERBOSE (In): [1,...,5], Get more detailed (higher number) command line output.
;
;Code:
; A. Shinbori, 05/07/2010.
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

pro iug_load_blr_srp, datatype=datatype, parameters=parameters, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
if ~keyword_set(verbose) then verbose=2
 
;****************************************
;Load 'blr_wind_ear' data by default:
;****************************************
if ~keyword_set(datatype) then datatype='blr_wind_srp'

;************************************
;Load wind component data by default:
;************************************
if ~keyword_set(parameters) then parameters='srp_zonal_wind'

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
if datatype eq 'blr_wind_srp' then begin
   vdcname = strmid(parameters, 4,1)
   
   if vdcname eq 'z' then vdcnames = 'u'
   if vdcname eq 'm' then vdcnames = 'v'
   if vdcname eq 'v' then vdcnames = 'w'
endif else if datatype eq 'blr_pwr_srp' then begin
   vdcname = strmid(parameters, 4,13)

   if vdcname eq 'pwr_beam1' then vdcnames = '1'
   if vdcname eq 'pwr_beam2' then vdcnames = '2'
   if vdcname eq 'pwr_beam3' then vdcnames = '3'
   if vdcname eq 'pwr_beam4' then vdcnames = '4'
   if vdcname eq 'pwr_beam5' then vdcnames = '5'
endif else if datatype eq 'blr_spec_width_srp' then begin
   vdcname = strmid(parameters, 4,12)

   if vdcname eq 'sw_beam1' then vdcnames = '1'
   if vdcname eq 'sw_beam2' then vdcnames = '2'
   if vdcname eq 'sw_beam3' then vdcnames = '3'
   if vdcname eq 'sw_beam4' then vdcnames = '4'
   if vdcname eq 'sw_beam5' then vdcnames = '5'
endif

;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'If you acquire BLR data, we ask that you' $
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
    if datatype eq 'blr_wind_srp' then begin
      file_names = file_dailynames( $
        file_format='YYYYMM/YYYYMMDD/'+$
        'YYYYMMDD',trange=trange,times=times,/unique)+'.'+vdcnames+'wnd.csv'
    endif else if datatype eq 'blr_pwr_srp' then begin
      file_names = file_dailynames( $
        file_format='YYYYMM/YYYYMMDD/'+$
        'YYYYMMDD',trange=trange,times=times,/unique)+'.pwr'+vdcnames+'.csv'    
    endif else if datatype eq 'blr_spec_width_srp' then begin
      file_names = file_dailynames( $
        file_format='YYYYMM/YYYYMMDD/'+$
        'YYYYMMDD',trange=trange,times=times,/unique)+'.wdt'+vdcnames+'.csv'
    endif
    ;
    ;Define FILE_RETRIEVE structure:
    ;===============================
    source = file_retrieve(/struct)
    source.verbose=verbose
    source.local_data_dir = root_data_dir() + 'iugonet/rish/blr/serpong/'
    source.remote_data_dir = 'ftp://ftp.rish.kyoto-u.ac.jp/pub/blr/serpong/data/ver02.0212/'
    
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
altitude = fltarr(63)
data = strarr(64)
data2 = fltarr(1,63)
time = dblarr(1)


;Loop on files (zonal component): 
;================================
 for i=0,n_elements(local_paths)-1 do begin
    file= local_paths[i]
    if file_test(/regular,file) then  dprint,'Loading BLR-Serpong file: ',file $
    else begin
         dprint,'BLR-Serpong file ',file,' not found. Skipping'
         continue
    endelse
    openr,lun,file,/get_lun    
    ;
    ;Read information of altitude:
    ;=============================
    readf, lun, s
    height = float(strsplit(s,',',/extract))
    number2 = n_elements(height)
    print, number2
    for j=0,number2-2 do begin
     altitude[j] = height[j+1]
    endfor
    for j=0,62 do begin
     b = float(altitude[j])
     wbad = where(b eq 0,nbad)
     if nbad gt 0 then b[wbad] = !values.f_nan
     print, b
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
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(7)+':'+string(0)+':'+string(0))
         ;
         for j=0,number2-2 do begin
          a = float(data[j+1])
          wbad = where(a eq 999,nbad)
          if nbad gt 0 then a[wbad] = !values.f_nan
          data2[k,j]=a
         endfor
           ;
           ;Append data of time:
           ;====================
            append_array, blr_time, time
           ;
           ;Append data of wind velocity:
           ;=============================
            append_array, blr_data, data2
    
      endif
    endwhile 
    free_lun,lun  
endfor
;******************************
;Store data in TPLOT variables:
;******************************

if time ne 0 then begin
      dprint,'Loading BLR-Serpong file: ',file 
      
      
   if datatype eq 'blr_wind_sgk' then begin 
   
  ;Store data of zonal wind:
  ;============================
      if parameters eq 'sgk_zonal_wind' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'sgk_zonal_wind',data={x:blr_time, y:blr_data, v:altitude},dlimit=dlimit
      endif
  ;Store data of meridional wind:
  ;==============================
      if parameters eq 'sgk_meridional_wind' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'sgk_meridional_wind',data={x:blr_time, y:blr_data, v:altitude},dlimit=dlimit
      endif
  ;Store data of vertical wind:
  ;============================
      if parameters eq 'sgk_vertical_wind' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'sgk_vertical_wind',data={x:blr_time, y:blr_data, v:altitude},dlimit=dlimit
      endif
   endif else if datatype eq 'blr_pwr_sgk' then begin
    ;Store data of beam1 echo intensity:
    ;===================================
      if parameters eq 'sgk_pwr_beam1' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'sgk_pwr_beam1',data={x:blr_time, y:blr_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam2 echo intensity:
    ;===================================
      if parameters eq 'sgk_pwr_beam2' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'sgk_pwr_beam2',data={x:blr_time, y:blr_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam3 echo intensity:
    ;===================================
      if parameters eq 'sgk_pwr_beam3' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'sgk_pwr_beam3',data={x:blr_time, y:blr_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam4 echo intensity:
    ;===================================
      if parameters eq 'sgk_pwr_beam4' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'sgk_pwr_beam4',data={x:blr_time, y:blr_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam5 echo intensity:
    ;===================================
      if parameters eq 'sgk_pwr_beam5' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'sgk_pwr_beam5',data={x:blr_time, y:blr_data, v:altitude},dlimit=dlimit
      endif
   endif else if datatype eq 'blr_spec_width_kot' then begin
    ;Store data of beam1 spectral width:
    ;===================================
      if parameters eq 'sgk_sw_beam1' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'sgk_sw_beam1',data={x:blr_time, y:blr_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam2 spectral width:
    ;===================================
      if parameters eq 'sgk_sw_beam2' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'sgk_sw_beam2',data={x:blr_time, y:blr_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam3 spectral width:
    ;===================================
      if parameters eq 'sgk_sw_beam3' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'sgk_sw_beam3',data={x:blr_time, y:blr_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam4 spectral width:
    ;===================================
      if parameters eq 'sgk_sw_beam4' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'sgk_sw_beam4',data={x:blr_time, y:blr_data, v:altitude},dlimit=dlimit
      endif
    ;Store data of beam5 spectral width:
    ;===================================
      if parameters eq 'sgk_sw_beam5' then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
         store_data,'sgk_sw_beam5',data={x:blr_time, y:blr_data, v:altitude},dlimit=dlimit
      endif
   endif
endif 

; add options
options, parameters, 'spec', 1

;Clear time and data buffer:
blr_data = 0
blr_time = 0
    
print,'**********************************************************************************
print,'Data loading is successful!!'
print,'**********************************************************************************

end

