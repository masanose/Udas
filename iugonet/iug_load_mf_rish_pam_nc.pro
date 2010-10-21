;+
;
;Name:
;iug_load_mf_rish_pam_nc
;
;Purpose:
;  Queries the Kyoto_RISH renkei2 servers for pameungpeuk data and loads data into
;  tplot format.
;
;Syntax:
; iug_load_mf_rish_pam_nc, datatype = datatype, site=site, downloadonly=downloadonly, trange=trange, verbose=verbose
;
;Keywords:
;  datatype = Observation data type. For example, iug_load_mf_rish_pam_nc, datatype = 'thermosphere'.
;            The default is 'thermosphere'. 
;   site  = Observatory code name.  For example, iug_load_mf_rish_pam_nc, site = 'pam'.
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


pro iug_load_mf_rish_pam_nc, datatype = datatype, site=site, downloadonly=downloadonly, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
if ~keyword_set(verbose) then verbose=2

;************************************
;Load 'thermosphere' data by default:
;************************************
if (not keyword_set(datatype)) then datatype='thermosphere'

;***********
;site codes:
;***********
if (not keyword_set(site)) then site='pam'

;--- all sites (default)
site_code_all = site

;--- check site codes
if(not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)

print, site_code

;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'If you acquire pameungpeuk data, we ask that you' $
+ 'acknowledge us in your use of the data. This may be done by' $
+ 'including text such as pameungpeuk data provided by Research Institute' $
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
    

if ~size(fns,/type) then begin

    ;Get files for ith component:
    ;***************************
    file_names = file_dailynames( $
                 file_format='YYYY/'+$
                             'YYYYMMDD',trange=trange,times=times,/unique)+'_pam.nc'
    ;            
    ;Define FILE_RETRIEVE structure:
    ;===============================
    source = file_retrieve(/struct)
    source.verbose=verbose
    source.local_data_dir =  root_data_dir() + 'iugonet/rish/misc/'+site_code+'/mf/NetCDF/'
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
 ; Initialize data and time buffer
  ear_time=0
  zon_wind=0
  mer_wind=0
  ver_wind=0
  height = fltarr(36)
  
  for j=0,n_elements(local_paths)-1 do begin
      file= local_paths[j]
      if file_test(/regular,file) then  dprint,'Loading pameungpeuk file: ',file $
      else begin
           dprint,'pameungpeuk file ',file,' not found. Skipping'
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
      ncdf_varget, cdfid, 'wwind', wwind
    
      ; Definition of arrary names
      uwind_pam=fltarr(n_elements(time),36)
      vwind_pam=fltarr(n_elements(time),36)
      wwind_pam=fltarr(n_elements(time),36)

      for i=0, n_elements(time)-1 do begin
          for k=0, 35 do begin
              height[k]=range[k]/1000
              uwind_pam[i,k]=uwind[k+36*i]
              vwind_pam[i,k]=vwind[k+36*i]
              wwind_pam[i,k]=wwind[k+36*i]
              a = uwind_pam[i,k]            
              wbad = where(a eq -9999,nbad)
              if nbad gt 0 then a[wbad] = !values.f_nan
              uwind_pam[i,k] =a
              b = vwind_pam[i,k]            
              wbad = where(b eq -9999,nbad)
              if nbad gt 0 then b[wbad] = !values.f_nan
              vwind_pam[i,k] =b
              c = wwind_pam[i,k]            
              wbad = where(c eq -9999,nbad)
              if nbad gt 0 then c[wbad] = !values.f_nan
              wwind_pam[i,k] =c
          endfor
      endfor
    
     ;Append data of time and wind velocity:
     ;======================================
      append_array, ear_time, time
      append_array, pam_time, time
      append_array, zon_wind, uwind_pam
      append_array, mer_wind, vwind_pam
      append_array, ver_wind, wwind_pam
 
      ncdf_close,cdfid  ; done
    
  endfor
;******************************
;Store data in TPLOT variables:
;******************************
  acknowledgstring = ''

;Store data of pameungpeuk wind data:
;====================================
    if(ear_time[0] ne 0) then begin
      dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'T. Tsuda'))
      store_data,'iug_mf_'+site_code[0]+'_uwnd',data={x:ear_time, y:zon_wind, v:height},dlimit=dlimit
      options,'iug_mf_'+site_code[0]+'_uwnd',ytitle='MF-pam!Cheight!C[m]',ztitle='uwnd!C[m/s]'
      store_data,'iug_mf_'+site_code[0]+'_vwnd',data={x:ear_time, y:mer_wind, v:height},dlimit=dlimit
      options,'iug_mf_'+site_code[0]+'_vwnd',ytitle='MF-pam!Cheight!C[m]',ztitle='vwind!C[m/s]'
      store_data,'iug_mf_'+site_code[0]+'_wwnd',data={x:ear_time, y:ver_wind, v:height},dlimit=dlimit
      options,'iug_mf_'+site_code[0]+'_wwnd',ytitle='MF-pam!Cheight!C[m]',ztitle='wwnd!C[m/s]'
  

    ; add options
    options, ['iug_mf_'+site_code[0]+'_uwnd','iug_mf_'+site_code[0]+'_vwnd','iug_mf_'+site_code[0]+'_wwnd'], 'spec', 1
  
    ; add options of setting lanels
    options, 'iug_mf_'+site_code[0]+'_uwnd', labels='MFR-pam [km]'
    options, 'iug_mf_'+site_code[0]+'_vwnd', labels='MFR-pam [km]'
    options, 'iug_mf_'+site_code[0]+'_wwnd', labels='MFR-pam [km]'

 ; clear data and time buffer
   ear_time=0
   zon_wind=0
   mer_wind=0
   ver_wind=0
   ; add tdegap
   tdegap, 'iug_mf_'+site_code[0]+'_uwnd',/overwrite
   tdegap, 'iug_mf_'+site_code[0]+'_vwnd',/overwrite
   tdegap, 'iug_mf_'+site_code[0]+'_wwnd',/overwrite
  endif 
endif

end
