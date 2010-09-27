;+
;
;Name:
;iug_load_ear_trop_nc
;
;Purpose:
;  Queries the Kyoto_RISH servers for ACII data of the equatorial atomosphere radar (EAR) 
;  and loads data intotplot format.
;
;Syntax:
; iug_load_ear_trop_nc, datatype = datatype, parameter=parameter, $
;                        downloadonly=downloadonly, trange=trange, verbose=verbose
;
;Keywords:
;  datatype = Observation data type. For example, iug_load_ear_trop_nc, datatype = 'troposphere'.
;            The default is 'troposphere'. 
;  parameter = parameter name of EAR troposphere standard obervation data.  
;          For example, iug_load_ear_trop_nc, parameter = 'uwnd'.
;          The default is 'all', i.e., load all available parameters.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;Code:
;  A. Shinbori, 09/09/2010.
;  
;Modifications:
;  A. Shinbori, 10/09/2010.
;  
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_ear_trop_nc, datatype = datatype, parameter=parameter, $
                           downloadonly=downloadonly, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2
 
;****************************************
;Load 'troposphere_wind' data by default:
;****************************************
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

;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'If you acquire EAR data, we ask that you' $
+ 'acknowledge us in your use of the data. This may be done by' $
+ 'including text such as EAR data provided by Research Institute' $
+ 'for Sustainable Humanosphere of Kyoto University. We would also' $
+ 'appreciate receiving a copy of the relevant publications.'


;******************************************************************
;Loop on downloading files
;******************************************************************
;Get timespan, define FILE_NAMES, and load data:
;===============================================
;


 if ~size(fns,/type) then begin

    ;Get files for ith component:
    ;***************************
       file_names = file_dailynames( $
       file_format='YYYYMM/YYYYMMDD/'+$
                   'YYYYMMDD',trange=trange,times=times,/unique)+'.nc'
    ;
    ;Define FILE_RETRIEVE structure:
    ;===============================
       source = file_retrieve(/struct)
       source.verbose=verbose
       source.local_data_dir = root_data_dir() + 'iugonet/rish/misc/ear/troposphere/'
       source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/ear/data/data/ver02.0212/'
    
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

      altitude = fltarr(150)
      range = fltarr(150)

    ; Initialize data and time buffer
      ear_time=0
      ear_zon=0
      ear_mer=0
      ear_ver=0
    ;Loop on files (zonal component): 
    ;================================
    
for j=0,n_elements(local_paths)-1 do begin
    file= local_paths[j]
    if file_test(/regular,file) then  dprint,'Loading EAR file: ',file $
    else begin
         dprint,'EAR file ',file,' not found. Skipping'
         continue
    endelse
    
    cdfid = ncdf_open(file,/NOWRITE)  ; Open the file
    glob = ncdf_inquire( cdfid )    ; Find out general info

    ; Show user the size of each dimension

    print,'Dimensions', glob.ndims
    for i=0,glob.ndims-1 do begin
        ncdf_diminq, cdfid, i, name,size
        if i EQ glob.recdim then  $
           print,'    ', name, size, '(Unlimited dim)' $
        else      $
           print,'    ', name, size  
    endfor

    ; Now tell user about the variables

    print
    print, 'Variables'
    for m=0,glob.nvars-1 do begin

        ; Get information about the variable
        info = ncdf_varinq(cdfid, m)
        FmtStr = '(A," (",A," ) Dimension Ids = [ ", 10(I0," "),$)'
        print, FORMAT=FmtStr, info.name,info.datatype, info.dim[*]
        print, ']'

        ; Get attributes associated with the variable
        for l=0,info.natts-1 do begin
            attname = ncdf_attname(cdfid,m,l)
            ncdf_attget,cdfid,m,attname,attvalue
            print,' Attribute ', attname, '=', string(attvalue)
        endfor
    endfor

    ; Get the variable
    ncdf_varget, cdfid, 'date', date
    ncdf_varget, cdfid, 'time', time
    ncdf_varget, cdfid, 'height', height
    ncdf_varget, cdfid, 'vwind', wwind
    ncdf_varget, cdfid, 'mwind', vwind
    ncdf_varget, cdfid, 'zwind', uwind
    ncdf_varget, cdfid, 'pwr', pwr
    ncdf_varget, cdfid, 'width', width
    ncdf_varget, cdfid, 'dpl', dpl
    ncdf_varget, cdfid, 'pnoise', pnoise
    print,date
    
    ; Calculation of unix time:
    year = fix(strmid(date,0,4))
    month = fix(strmid(date,4,2))
    day = fix(strmid(date,6,2))
    time2 = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+string(0)+':'+string(0)+':'+string(0))+double(time) $
                               -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(7)+':'+string(0)+':'+string(0))
    for i=0, 149 do begin
      ;range[i]=height[i]
      altitude[i]=float(height[i])
    endfor

    ; Definition of arrary names
    uwind_ear=fltarr(n_elements(time),150)
    vwind_ear=fltarr(n_elements(time),150)
    wwind_ear=fltarr(n_elements(time),150)
    
    for i=0, n_elements(time)-1 do begin
        for k=0, 149 do begin
            uwind_ear[i,k]=float(uwind[k+150*i])
            vwind_ear[i,k]=float(vwind[k+150*i])
            wwind_ear[i,k]=float(wwind[k+150*i])
            if uwind[k+150*i] eq 1.00000e+010 then uwind_ear[i,k]=0
            a = uwind_ear[i,k]            
            wbad = where(a eq 10000000000,nbad)
            if nbad gt 0 then a[wbad] = !values.f_nan
            uwind_ear[i,k] =a
            b = vwind_ear[i,k]            
            wbad = where(b eq 10000000000,nbad)
            if nbad gt 0 then b[wbad] = !values.f_nan
            vwind_ear[i,k] =b
            c = wwind_ear[i,k]            
            wbad = where(c eq 10000000000,nbad)
            if nbad gt 0 then c[wbad] = !values.f_nan
            wwind_ear[i,k] =c
        endfor
    endfor
    print, uwind_ear
   ; print, uwind, n_elements(uwind),n_elements(time)
   ;Append data of time and wind velocity:
   ;======================================
    append_array, ear_time, time2
    append_array, zon_wind, uwind_ear
    append_array, mer_wind, vwind_ear
    append_array, ver_wind, wwind_ear
 
    ncdf_close,cdfid  ; done
    
endfor
   ;******************************
   ;Store data in TPLOT variables:
   ;******************************
      acknowledgstring = ''
      if time2[0] ne 0 then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'H. Hashiguchi'))
         store_data,'iug_ear_uwnd',data={x:ear_time, y:zon_wind, v:altitude},dlimit=dlimit
         options,'iug_ear_uwnd',ytitle='EAR-trop!CHeight!C[km]',ztitle='uwnd!C[km]'
         options,'iug_ear_uwnd', labels='EAR-trop [km]'
         store_data,'iug_ear_vwnd',data={x:ear_time, y:mer_wind, v:altitude},dlimit=dlimit
         options,'iug_ear_vwnd',ytitle='EAR-trop!CHeight!C[km]',ztitle='vwnd!C[km]'
         options,'iug_ear_vwnd', labels='EAR-trop [km]'
         store_data,'iug_ear_wwnd',data={x:ear_time, y:ver_wind, v:altitude},dlimit=dlimit
         options,'iug_ear_wwnd',ytitle='EAR-trop!CHeight!C[km]',ztitle='wwnd!C[km]'
         options,'iug_ear_wwnd', labels='EAR-trop [km]'
       ; add options
         options, ['iug_ear_uwnd','iug_ear_vwnd','iug_ear_wwnd'], 'spec', 1
      endif

    ;Clear time and data buffer:
      ear_time=0
      zon_wind=0
      mer_wind=0
      ver_wind=0
    ; add tdegap
      tdegap, 'iug_ear_uwnd',/overwrite
      tdegap, 'iug_ear_vwnd',/overwrite
      tdegap, 'iug_ear_wwnd',/overwrite
   endif

print,'**********************************************************************************
print,'Data loading is successful!!'
print,'**********************************************************************************

end

