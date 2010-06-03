;+
;
;Name:
;iug_load_mu_trop
;
;Purpose:
;  Queries the Kyoto_RISH servers for MU data and loads data into
;  tplot format.
;
;Syntax:
; iug_load_mu_trop  [ ,DATATYPE = string ]
;                   [ ,PARAMETERS = string ]
;                   [ ,TRANGE = [min,max] ]
;                   [ ,FILENAMES = string scalar or array ]
;                   [ ,<and data keywords below> ]
;
;Keywords:
;  DATATYPE (I/O):
;    Set to 'trop_wind'.  If not set, 'trop_wind' is
;      assumed.  Returns cleaned input, or shows default. 
;  PARAMETERS (I/O):
;    Set to wind parameters.  If not set, 'mu_wind_zon' is
;      assumed.  Returns cleaned input, or shows default.       
;  TRANGE (In):
;    Pass a time range a la TIME_STRING.PRO.
;  FILENAMES (In):
;    *PRESENTLY DISABLED* Pass user-defined file names (full paths to local data files).  These will
;      be read a la the RISH format, and the RISH server will not be queried.
;  VERBOSE (In): [1,...,5], Get more detailed (higher number) command line output.
;
;Code:
; A. Shinbori, 03/06/2010.
;
;Modifications:
;
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_mu_trop, datatype=datatype, parameters=parameters, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
if ~keyword_set(verbose) then verbose=2
 
;****************************************
;Load 'troposphere_wind' data by default:
;****************************************
if ~keyword_set(datatype) then datatype='trop_wind'

;************************************
;Load wind component data by default:
;************************************
if ~keyword_set(parameters) then parameters='zonal_wind_ear'

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
if datatype eq 'trop_wind' then begin
   vdcname = strmid(parameters, 0,1)
   
   if vdcname eq 'z' then vdcnames = 'u'
   if vdcname eq 'm' then vdcnames = 'v'
   if vdcname eq 'v' then vdcnames = 'w'
endif else if datatype eq 'trop_pwr' then begin
   vdcname = strmid(parameters, 0,9)

   if vdcname eq 'pwr_beam1' then vdcnames = '1'
   if vdcname eq 'pwr_beam2' then vdcnames = '2'
   if vdcname eq 'pwr_beam3' then vdcnames = '3'
   if vdcname eq 'pwr_beam4' then vdcnames = '4'
   if vdcname eq 'pwr_beam5' then vdcnames = '5'
endif else if datatype eq 'trop_spec_width' then begin
   vdcname = strmid(parameters, 0,8)

   if vdcname eq 'sw_beam1' then vdcnames = '1'
   if vdcname eq 'sw_beam2' then vdcnames = '2'
   if vdcname eq 'sw_beam3' then vdcnames = '3'
   if vdcname eq 'sw_beam4' then vdcnames = '4'
   if vdcname eq 'sw_beam5' then vdcnames = '5'
endif

;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'If you acquire MU data, we ask that you' $
+ 'acknowledge us in your use of the data. This may be done by' $
+ 'including text such as MU data provided by Research Institute' $
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
    if datatype eq 'trop_wind' then begin
      file_names = file_dailynames( $
        file_format='YYYYMM/YYYYMMDD/'+$
        'YYYYMMDD',trange=trange,times=times,/unique)+'.'+vdcnames+'wnd.csv'
    endif else if datatype eq 'trop_pwr' then begin
      file_names = file_dailynames( $
        file_format='YYYYMM/YYYYMMDD/'+$
        'YYYYMMDD',trange=trange,times=times,/unique)+'.pwr'+vdcnames+'.csv'    
    endif else if datatype eq 'trop_spec_width' then begin
      file_names = file_dailynames( $
        file_format='YYYYMM/YYYYMMDD/'+$
        'YYYYMMDD',trange=trange,times=times,/unique)+'.wdt'+vdcnames+'.csv'
    endif
    ;
    ;Define FILE_RETRIEVE structure:
    ;===============================
    source = file_retrieve(/struct)
    source.verbose=verbose
    source.local_data_dir = root_data_dir() + 'iugonet/rish/mu/troposphere_wind/'
    source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/radar-group/mu/data/data/ver01.0807/'
    
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
altitude = fltarr(120)
height = fltarr(121)
data = strarr(121)
data2 = fltarr(1,120)
mu_data = fltarr(1,120)
time = dblarr(1)
mu_time = dblarr(1)

;Loop on files (zonal component): 
;================================
 for i=0,n_elements(local_paths)-1 do begin
    file= local_paths[i]
    if file_test(/regular,file) then  dprint,'Loading MU file: ',file $
    else begin
         dprint,'MU file ',file,' not found. Skipping'
         continue
    endelse
    openr,lun,file,/get_lun    
    ;
    ;Read information of altitude:
    ;=============================
    readf, lun, s
    height = float(strsplit(s,',',/extract))
    
    for j=0,119 do begin
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
                   -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(6)+':'+string(41)+':'+string(12))
         ;
         for j=0,119 do begin
          a = float(data[j+1])
          wbad = where(a eq 999,nbad)
          if nbad gt 0 then a[wbad] = !values.f_nan
          data2[k,j]=a
         endfor
           ;
           ;Append data of time:
           ;====================
            append_array, mu_time, time
           ;
           ;Append data of wind velocity:
           ;=============================
            append_array, mu_data, data2
    
         continue
      endif
    endwhile 
    free_lun,lun  
endfor

;Replace data array:
;===================
number = n_elements(ear_time)

for i=0,number-2 do begin
  mu_time[i] = mu_time[i+1]
  mu_data[i,*] = mu_data[i+1,*]
endfor


if datatype eq 'trop_wind' then begin 
  ;Store data of zonal wind:
  ;============================
  if parameters eq 'zonal_wind_mu' then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
     store_data,'zonal_wind_mu',data={x:mu_time, y:mu_data, v:altitude},dlimit=dlimit
    ; add options
    options, 'zonal_wind_mu', 'spec', 1
  endif
  ;Store data of meridional wind:
  ;==============================
  if parameters eq 'meridional_wind_mu' then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
     store_data,'meridional_wind_mu',data={x:mu_time, y:mu_data, v:altitude},dlimit=dlimit
  endif
  ;Store data of vertical wind:
  ;============================
  if parameters eq 'vertical_wind_mu' then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
     store_data,'vertical_wind_mu',data={x:mu_time, y:mu_data, v:altitude},dlimit=dlimit
  endif
endif else if datatype eq 'trop_pwr' then begin
    ;Store data of beam1 echo intensity:
    ;===================================
  if parameters eq 'pwr_beam1' then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
     store_data,'pwr_beam1',data={x:mu_time, y:mu_data, v:altitude},dlimit=dlimit
  endif
    ;Store data of beam2 echo intensity:
    ;===================================
  if parameters eq 'pwr_beam2' then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
     store_data,'pwr_beam2',data={x:mu_time, y:mu_data, v:altitude},dlimit=dlimit
  endif
    ;Store data of beam3 echo intensity:
    ;===================================
  if parameters eq 'pwr_beam3' then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
     store_data,'pwr_beam3',data={x:mu_time, y:mu_data, v:altitude},dlimit=dlimit
  endif
    ;Store data of beam4 echo intensity:
    ;===================================
  if parameters eq 'beam4' then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
     store_data,'pwr_beam4',data={x:mu_time, y:mu_data, v:altitude},dlimit=dlimit
  endif
    ;Store data of beam5 echo intensity:
    ;===================================
  if parameters eq 'pwr_beam5' then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
     store_data,'pwr_beam5',data={x:mu_time, y:mu_data, v:altitude},dlimit=dlimit
  endif
endif else if datatype eq 'trop_spec_width' then begin
    ;Store data of beam1 spectral width:
    ;===================================
  if parameters eq 'sw_beam1' then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
     store_data,'sw_beam1',data={x:mu_time, y:mu_data, v:altitude},dlimit=dlimit
  endif
    ;Store data of beam2 spectral width:
    ;===================================
  if parameters eq 'sw_beam2' then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
     store_data,'sw_beam2',data={x:mu_time, y:mu_data, v:altitude},dlimit=dlimit
  endif
    ;Store data of beam3 spectral width:
    ;===================================
  if parameters eq 'sw_beam3' then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
     store_data,'sw_beam3',data={x:mu_time, y:mu_data, v:altitude},dlimit=dlimit
  endif
    ;Store data of beam4 spectral width:
    ;===================================
  if parameters eq 'sw_beam4' then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
     store_data,'sw_beam4',data={x:mu_time, y:mu_data, v:altitude},dlimit=dlimit
  endif
    ;Store data of beam5 spectral width:
    ;===================================
  if parameters eq 'sw_beam5' then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
     store_data,'sw_beam5',data={x:mu_time, y:mu_data, v:altitude},dlimit=dlimit
  endif
endif


    
print,'**********************************************************************************
print,'Data loading is successful!!'
print,'**********************************************************************************

end

