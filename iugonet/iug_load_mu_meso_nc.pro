;+
;
;Name:
;iug_load_mu_meso_nc
;
;Purpose:
;  Queries the Kyoto_RISH renkei2 servers for the NetCDF data of the Serpong site
;  and loads data into tplot format.
;
;Syntax:
; iug_load_mu_meso_nc [ ,DATATYPE = string ]
;                     [ ,PARAMETERS = string]
;                     [ ,TRANGE = [min,max] ]
;                     [ ,FILENAMES = string scalar or array ]
;                     [ ,<and data keywords below> ]
;
;Keywords:
;  DATATYPE (I/O):
;    Set to the dayatype.  If not set, 'doppler_velocity' is
;      assumed.  Returns cleaned input, or shows default.
;  PARAMETERS:
;    Set to the observation data at several altitudes=''. If not set, 'beam1' is
;      assumed.  Returns cleaned input, or shows default.      
;  TRANGE (In):
;    Pass a time range a la TIME_STRING.PRO.
;  FILENAMES (In):
;    *PRESENTLY DISABLED* Pass user-defined file names (full paths to local data files).  These will
;      be read a la the RISH format, and the RISH server will not be queried.
;  VERBOSE (In): [1,...,5], Get more detailed (higher number) command line output.
;
;Code:
;  A. Shinbori, 12/08/2010.
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

pro iug_load_mu_meso_nc, datatype=datatype, parameters = parameters, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
;verbose
if ~keyword_set(verbose) then verbose=2

;*****************************************
;Load 'thermosphere_wind' data by default:
;*****************************************
if ~keyword_set(datatype) then datatype='doppler_velocity'

;********************************
;Load 'parameters' data by default:
;********************************
if ~keyword_set(parameters) then parameters='beam1'

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
+ 'including text such as the MU mesosphere data provided by Research Institute' $
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
      file_format='YYYY/'+'mu_meso_'+'YYYYMMDD',trange=trange,times=times,/unique)+'.nc'
    ;=========================================================================
    ;        
    ;Define FILE_RETRIEVE structure:
    ;===============================
    source = file_retrieve(/struct)
    source.verbose=verbose
    source.local_data_dir =  root_data_dir() + 'iugonet/rish/mu/mesosphere/'
    ;source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/ear/data/data/ver02.0212/'

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
    if file_test(/regular,file) then  dprint,'Loading MU mesosphere file: ',file $
    else begin
         dprint,'MU mesosphere file ',file,' not found. Skipping'
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
    ncdf_varget, cdfid, 'lat', lat
    ncdf_varget, cdfid, 'lon', lon
    ncdf_varget, cdfid, 'time', time
    ncdf_varget, cdfid, 'range', range
    ncdf_varget, cdfid, 'beam', beam
    ncdf_varget, cdfid, 'doppler_velocity', doppler_velocity
    ncdf_varget, cdfid, 'power', power
    ncdf_varget, cdfid, 'spectrum', spectrum
    ncdf_varget, cdfid, 'residual', residual
    ncdf_varget, cdfid, 'condition', condition
    ncdf_varget, cdfid, 'noise_level', noise_level
    ncdf_varget, cdfid, 'jamming', jamming

    ;

    ; Definition of arrary names
    timeunix = dblarr(n_elements(time))
    pwr=dblarr(n_elements(time),128)
    

    for i=0, n_elements(time)-1 do begin
        ; Change hourtime since 1992-09-27 17:00:00 (Local Time) into unixtime (1970-01-01 00:00:00)
        timeunix[i] = double(time[i])$
                      +time_double(string(2003)+'-'+string(11)+'-'+string(5)+'/'+string(-9)+':'+string(0))

        for k=0, 4 do begin
            ;pwr[i,k]=noise_level[k+128*5*i]
            nl=noise_level[i*5]
           ; 
            a = nl          
            wbad = where(a eq -999,nbad)
            if nbad gt 0 then a[wbad] = !values.f_nan
            nl =a
        endfor
   ;Append data of time and wind velocity:
   ;======================================
    
    append_array, meso_nl, nl
    endfor
    append_array, mu_time, timeunix
   
    
    ncdf_close,cdfid  ; done
    
endfor
 print,meso_nl
;******************************
;Store data in TPLOT variables:
;******************************
acknowledgstring = ''
print, parameters
;Store data of zonal and meridional component:
;=============================================
   if  parameters eq 'beam1' then begin
       dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring))
       store_data,'pwr_beam1',data={x:mu_time, y:meso_nl},dlimit=dlimit
   endif
   
; add options
  ; options, 'pwr_beam1', 'spec', 1
   tplot, 'pwr_beam1'

;Clear time and data buffer:
mu_time=0
meso_nl=0
  
print,'**********************************************************************************
print, 'Data loading is successful!!'
print,'**********************************************************************************

end

