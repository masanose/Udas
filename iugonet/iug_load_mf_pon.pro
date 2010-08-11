;+
;
;Name:
;iug_load_mf_pon
;
;Purpose:
;  Queries the Kyoto_RISH renkei2 servers for MF radar data of the pontianak station
;  and loads data into tplot format.
;
;Syntax:
; iug_load_mf_pon [ ,DATATYPE = string ]
;                 [ ,PARAMETERS = string]
;                 [ ,TRANGE = [min,max] ]
;                 [ ,FILENAMES = string scalar or array ]
;                 [ ,<and data keywords below> ]
;
;Keywords:
;  DATATYPE (I/O):
;    Set to 'thermosphere_wind'.  If not set, 'pontianak' is
;      assumed.  Returns cleaned input, or shows default.
;  PARAMETERS:
;    Set to the wind data at several altitudes=''. If not set, 'zon_wind' is
;      assumed.  Returns cleaned input, or shows default.      
;  TRANGE (In):
;    Pass a time range a la TIME_STRING.PRO.
;  FILENAMES (In):
;    *PRESENTLY DISABLED* Pass user-defined file names (full paths to local data files).  These will
;      be read a la the RISH format, and the RISH server will not be queried.
;  VERBOSE (In): [1,...,5], Get more detailed (higher number) command line output.
;
;Code:
;  A. Shinbori, 28/07/2010.
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

pro iug_load_mf_pon, datatype=datatype, parameters = parameters, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
;verbose
if ~keyword_set(verbose) then verbose=2

;*****************************************
;Load 'thermosphere_wind' data by default:
;*****************************************
if ~keyword_set(datatype) then datatype='pontianak'

;********************************
;Load 'parameters' data by default:
;********************************
if ~keyword_set(parameters) then parameters='zon_wind'

;*******************
;Validate datatypes:
;*******************
vns = datatype
if size(datatype,/type) eq 7 then begin
  datatype=thm_check_valid_name(datatype,vns,/ignore_case,/include_all,/no_warning)
  if datatype[0] eq '' then return
endif else begin
  message,'DATATYPE must be of string type.',/info
  return
endelse


;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'If you acquire meteor radar data, we ask that you' $
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
    ;=========Manual mode====================================================
    ;file_names = file_dailynames( $
    ;  file_format='YYYY/Wk'+$
    ;  'YYYYMM',trange=t,times=times,/unique)+'.h2t60'
    ;========================================================================
    ;=========GUI mode=======================================================
    file_names = file_dailynames( $
      file_format='YYYYMM/'+$
      'YYYYMMDD',trange=trange,times=times,/unique)+'.txt'
    ;=========================================================================
    ;        
    ;Define FILE_RETRIEVE structure:
    ;===============================
    source = file_retrieve(/struct)
    source.verbose=verbose
    source.local_data_dir =  root_data_dir() + 'iugonet/rish/mf/pontianak/'
    ;source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/ear/data/data/ver02.0212/'
    
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
height = fltarr(21)
zon_wind_data = fltarr(1,21)
mer_wind_data = fltarr(1,21)
time = dblarr(1)
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
           print, data1
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

;Store data of zonal and meridional component:
;=============================================
   if  parameters eq 'zonal_wind_pon' then begin
       dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
       store_data,'zonal_wind_pon',data={x:pon_time, y:zon_wind, v:height},dlimit=dlimit
   endif
   if  parameters eq 'meridional_wind_pon' then begin
       dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
       store_data,'meridional_wind_pon',data={x:pon_time, y:mer_wind, v:height},dlimit=dlimit
   endif
   
; add options
   options, parameters, 'spec', 1
endif 

;Clear time and data buffer:
pon_time=0
zon_wind=0
mer_wind=0
          
print,'**********************************************************************************
print, 'Data loading is successful!!'
print,'**********************************************************************************

end

