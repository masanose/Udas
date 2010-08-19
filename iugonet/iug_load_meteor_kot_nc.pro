;+
;
;Name:
;iug_load_meteor_kot_nc
;
;Purpose:
;  Queries the Kyoto_RISH renkei2 servers for the NetCDF data of the Kototabang site
;  and loads data into tplot format.
;
;Syntax:
; iug_load_meteor_kot_nc [ ,DATATYPE = string ]
;                        [ ,PARAMETERS = string]
;                        [ ,TRANGE = [min,max] ]
;                        [ ,FILENAMES = string scalar or array ]
;                        [ ,<and data keywords below> ]
;
;Keywords:
;  DATATYPE (I/O):
;    Set to 'thermosphere_wind'.  If not set, 'kototabang' is
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
;  A. Shinbori, 06/08/2010.
;  
;Modifications:
;  
;  
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_meteor_kot_nc, datatype=datatype, parameters = parameters, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
;verbose
if ~keyword_set(verbose) then verbose=2

;*****************************************
;Load 'thermosphere_wind' data by default:
;*****************************************
if ~keyword_set(datatype) then datatype='kototabang'

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
+ 'including text such as the kototabang MW data provided by Research Institute' $
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
      file_format='YYYYMM',trange=trange,times=times,/unique)+'_koto.nc'
    ;=========================================================================
    ;        
    ;Define FILE_RETRIEVE structure:
    ;===============================
    source = file_retrieve(/struct)
    source.verbose=verbose
    source.local_data_dir =  root_data_dir() + 'iugonet/rish/mw/kototabang/koto_h2km_t60min00_netCDF/'
    ;source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/ear/data/data/ver02.0212/'
        print, file_names
    ;Get files and local paths, and concatenate local paths:
    ;=======================================================
    local_paths=file_retrieve(file_names,_extra=source)
    local_paths_all = ~(~size(local_paths_all,/type)) ? $
      [local_paths_all, local_paths] : local_paths
  if ~(~size(local_paths_all,/type)) then local_paths=local_paths_all
endif else file_names=fns

;Loop on files (read the NetCDF files): 
;======================================
 for j=0,n_elements(local_paths)-1 do begin
    file= local_paths[j]
    if file_test(/regular,file) then  dprint,'Loading Kototabang file: ',file $
    else begin
         dprint,'Kototabang file ',file,' not found. Skipping'
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
    ncdf_varget, cdfid, 'time', time
    ncdf_varget, cdfid, 'range', range
    ncdf_varget, cdfid, 'uwind', uwind
    ncdf_varget, cdfid, 'vwind', vwind
    ncdf_varget, cdfid, 'sig_uwind', sig_uwind
    ncdf_varget, cdfid, 'sig_vwind', sig_vwind
    ncdf_varget, cdfid, 'num', num
    
    ; Definition of arrary names
    timeunix = dblarr(n_elements(time))
    height = fltarr(36)
    uwind_kot=fltarr(n_elements(time),36)
    vwind_kot=fltarr(n_elements(time),36)
    sig_uwind_kot=fltarr(n_elements(time),36)
    sig_vwind_kot=fltarr(n_elements(time),36)
    num_kot=fltarr(n_elements(time),36)

    for i=0, n_elements(time)-1 do begin
        ;Change hourtime since 1992-09-27 17:00:00 (Local Time) into unixtime (1970-01-01 00:00:00)
        timeunix[i] = double(time[i])*3600$
                      +time_double(string(1992)+'-'+string(09)+'-'+string(27)+'/'+string(17-7)+':'+string(00))
                      
        for k=0, 35 do begin
            height[k]=range[k]/1000
            uwind_kot[i,k]=uwind[k+36*i]
            vwind_kot[i,k]=vwind[k+36*i]
            sig_uwind_kot[i,k]=sig_uwind[k+36*i]
            sig_vwind_kot[i,k]=sig_vwind[k+36*i]
            num_kot[i,k]=num[k+36*i]
            
            a = uwind_kot[i,k]            
            wbad = where(a eq -9999,nbad)
            if nbad gt 0 then a[wbad] = !values.f_nan
            uwind_kot[i,k] =a
            b = vwind_kot[i,k]            
            wbad = where(b eq -9999,nbad)
            if nbad gt 0 then b[wbad] = !values.f_nan
            vwind_kot[i,k] =b
            c = sig_uwind_kot[i,k]            
            wbad = where(c eq -9999,nbad)
            if nbad gt 0 then c[wbad] = !values.f_nan
            sig_uwind_kot[i,k] =c
            d = sig_vwind_kot[i,k]            
            wbad = where(d eq -9999,nbad)
            if nbad gt 0 then d[wbad] = !values.f_nan
            sig_vwind_kot[i,k] =d
            e = num_kot[i,k]            
            wbad = where(e eq -9999,nbad)
            if nbad gt 0 then e[wbad] = !values.f_nan
            num_kot[i,k] =d
        endfor
    endfor
    
   ;Append data of time and wind velocity:
   ;======================================
    append_array, kot_time, timeunix
    append_array, zon_wind, uwind_kot
    append_array, mer_wind, vwind_kot
    append_array, zon_thermal, sig_uwind_kot
    append_array, mer_thermal, sig_vwind_kot
    append_array, meteor_num, num_kot
    ncdf_close,cdfid  ; done
    
endfor

;******************************
;Store data in TPLOT variables:
;******************************
acknowledgstring = ''

;Store data of zonal and meridional component:
;=============================================
   if  parameters eq 'zonal_wind_ktb' then begin
       dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
       store_data,'zonal_wind_ktb',data={x:kot_time, y:zon_wind, v:height},dlimit=dlimit
   endif
   if  parameters eq 'meridional_wind_ktb' then begin
       dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
       store_data,'meridional_wind_ktb',data={x:kot_time, y:mer_wind, v:height},dlimit=dlimit
   endif

;Store data of zonal and meridional thermal speed component:
;===========================================================
   if  parameters eq 'zonal_thermal_speed_ktb' then begin
       dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
       store_data,'zonal_thermal_speed_ktb',data={x:kot_time, y:zon_thermal, v:height},dlimit=dlimit
   endif
   if  parameters eq 'meridional_thermal_speed_ktb' then begin
       dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
       store_data,'meridional_thermal_speed_ktb',data={x:kot_time, y:mer_thermal, v:height},dlimit=dlimit
   endif
;Store data of meteor trace number:
;==================================
   if  parameters eq 'meteor_num_ktb' then begin
       dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
       store_data,'meteor_num_ktb',data={x:kot_time, y:meteor_num, v:height},dlimit=dlimit
   endif
   
; add options
   options, parameters, 'spec', 1


;Clear time and data buffer:
kot_time=0
zon_wind=0
mer_wind=0
zon_thermal=0
mer_thermal=0
meteor_num=0 
  
print,'**********************************************************************************
print, 'Data loading is successful!!'
print,'**********************************************************************************

end

