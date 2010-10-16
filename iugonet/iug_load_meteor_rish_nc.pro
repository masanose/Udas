;+
;
;Name:
;iug_load_meteor_rish_nc
;
;Purpose:
;  Queries the Kyoto_RISH renkei2 servers for ACII data of the meteor radar 
;  and loads data intotplot format.
;
;Syntax:
; iug_load_meteor_rish_nc, datatype = datatype, site = site, downloadonly = downloadonly, $
;                           trange = trange, verbose=verbose
;
;Keywords:
;  datatype = Observation data type. For example, iug_load_meteor_rish_nc, datatype = 'thermosphere'.
;            The default is 'thermosphere'. 
;  site  = Observatory code name.  For example, iug_load_meteor_rish_txt, site = 'srp'.
;          The default is 'all', i.e., load all available stations.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;Code:
;  A. Shinbori, 28/08/2010.
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


pro iug_load_meteor_rish_nc, datatype = datatype, site = site, $
                             downloadonly = downloadonly, trange = trange, verbose = verbose

;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2
 
;************************************
;Load 'thermosphere' data by default:
;************************************
if (not keyword_set(datatype)) then datatype='thermosphere'

;***********
;site codes:
;***********
;--- all sites (default)
site_code_all = strsplit('ktb srp',' ', /extract)

;--- check site codes
if(not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)

print, site_code

;***************
;data directory:
;***************
site_data_dir = strsplit('/meteor/koto_h2km_t60min00_netCDF/ /meteor/winddata/serp_wind_h2km_t60min00netCDF/',' ', /extract)
site_data_lastmane = strsplit('_koto.nc _serp.nc',' ', /extract)

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
h=0
jj=0
for ii=0,n_elements(site_code)-1 do begin

  if ~size(fns,/type) then begin
      if site_code[ii] eq 'ktb' then h=0
      if site_code[ii] eq 'srp' then h=1
    ;Get files for ith component:
    ;***************************       
      file_names = file_dailynames( $
                   file_format='YYYY/'+$
                   'YYYYMM',trange=trange,times=times,/unique)+site_data_lastmane[h]
    ;        
    ;Define FILE_RETRIEVE structure:
    ;===============================
       source = file_retrieve(/struct)
       source.verbose=verbose
       source.local_data_dir =  root_data_dir() + 'iugonet/rish/misc/'+site_code[ii]+site_data_dir[h]

    ;source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/ear/data/data/ver02.0212/'
    
    ;Get files and local paths, and concatenate local paths:
    ;=======================================================
       local_paths=file_retrieve(file_names,_extra=source, /last_version)
       local_paths_all = ~(~size(local_paths_all,/type)) ? $
                        [local_paths_all, local_paths] : local_paths
       if ~(~size(local_paths_all,/type)) then local_paths=local_paths_all
    endif else file_names=fns

    ;--- Load data into tplot variables
    if(not keyword_set(downloadonly)) then downloadonly=0

    if(downloadonly eq 0) then begin

     ; Initialize data and time buffer
      site_time=0
      zon_wind=0
      mer_wind=0
      zon_thermal=0
      mer_thermal=0
      meteor_num=0 
      height = fltarr(36)
 
 ;Loop on files (read the NetCDF files): 
 ;======================================
      for j=jj,n_elements(local_paths)-1 do begin
          file= local_paths[j]
          if file_test(/regular,file) then  dprint,'Loading meteor radar data file: ',file $
          else begin
             dprint,'Meteor radar data file',file,'not found. Skipping'
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
          uwind_data=fltarr(n_elements(time),36)
          vwind_data=fltarr(n_elements(time),36)
          sig_uwind_data=fltarr(n_elements(time),36)
          sig_vwind_data=fltarr(n_elements(time),36)
          num_data=fltarr(n_elements(time),36)

          for i=0, n_elements(time)-1 do begin
        ;Change hourtime since 1992-10-27 17:00:00 (Local Time) into unixtime (1970-01-01 00:00:00)
              timeunix[i] = double(time[i])*3600-3600*7$
                            +time_double(string(1992)+'-'+string(10)+'-'+string(27)+'/'+string(17)+':'+string(00)+':'+string(00))
                      
              for k=0, 35 do begin
                  height[k]=float(range[k])/1000
                  uwind_data[i,k]=uwind[k+36*i]
                  vwind_data[i,k]=vwind[k+36*i]
                  sig_uwind_data[i,k]=sig_uwind[k+36*i]
                  sig_vwind_data[i,k]=sig_vwind[k+36*i]
                  num_data[i,k]=num[k+36*i]
            
                  a = uwind_data[i,k]            
                  wbad = where(a eq -9999,nbad)
                  if nbad gt 0 then a[wbad] = !values.f_nan
                  uwind_data[i,k] =a
                  b = vwind_data[i,k]            
                  wbad = where(b eq -9999,nbad)
                  if nbad gt 0 then b[wbad] = !values.f_nan
                  vwind_data[i,k] =b
                  c = sig_uwind_data[i,k]            
                  wbad = where(c eq -9999,nbad)
                  if nbad gt 0 then c[wbad] = !values.f_nan
                  sig_uwind_data[i,k] =c
                  d = sig_vwind_data[i,k]            
                  wbad = where(d eq -9999,nbad)
                  if nbad gt 0 then d[wbad] = !values.f_nan
                  sig_vwind_data[i,k] =d
                  e = num_data[i,k]            
                  wbad = where(e eq -9999,nbad)
                  if nbad gt 0 then e[wbad] = !values.f_nan
                  num_data[i,k] =e
              endfor
          endfor
    
   ;Append data of time and wind velocity:
   ;======================================
      append_array, site_time, timeunix
      append_array, zon_wind, uwind_data
      append_array, mer_wind, vwind_data
      append_array, zon_thermal, sig_uwind_data
      append_array, mer_thermal, sig_vwind_data
      append_array, meteor_num, num_data
      ncdf_close,cdfid  ; done
    
  endfor

;******************************
;Store data in TPLOT variables:
;******************************
  acknowledgstring = ''

;Store data of meteor wind data:
;===============================

  if site_time[0] ne 0 then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'T. Tsuda'))
     store_data,'iug_meteor_'+site_code[ii]+'_uwnd',data={x:site_time, y:zon_wind, v:height},dlimit=dlimit
     options,'iug_meteor_'+site_code[ii]+'_uwnd',ytitle='MW-'+site_code[ii]+'!CHeight!C[km]',ztitle='uwnd!C[m/s]'
     store_data,'iug_meteor_'+site_code[ii]+'_vwnd',data={x:site_time, y:mer_wind, v:height},dlimit=dlimit
     options,'iug_meteor_'+site_code[ii]+'_vwnd',ytitle='MW-'+site_code[ii]+'!CHeight!C[km]',ztitle='vwnd!C[m/s]'
     store_data,'iug_meteor_'+site_code[ii]+'_uwndsig',data={x:site_time, y:zon_thermal, v:height},dlimit=dlimit
     options,'iug_meteor_'+site_code[ii]+'_uwndsig',ytitle='MW-'+site_code[ii]+'!CHeight!C[km]',ztitle='uwndsig!C[m/s]'
     store_data,'iug_meteor_'+site_code[ii]+'_vwndsig',data={x:site_time, y:mer_thermal, v:height},dlimit=dlimit
     options,'iug_meteor_'+site_code[ii]+'_vwndsig',ytitle='MW-'+site_code[ii]+'!CHeight!C[km]',ztitle='vwndsig!C[m/s]'
     store_data,'iug_meteor_'+site_code[ii]+'_mwnum',data={x:site_time, y:meteor_num, v:height},dlimit=dlimit
     options,'iug_meteor_'+site_code[ii]+'_mwnum',ytitle='MW-'+site_code[ii]+'!CHeight!C[km]',ztitle='mwnum'


     ; add options
     options, ['iug_meteor_'+site_code[ii]+'_uwnd','iug_meteor_'+site_code[ii]+'_vwnd',$
               'iug_meteor_'+site_code[ii]+'_uwndsig','iug_meteor_'+site_code[ii]+'_vwndsig',$
               'iug_meteor_'+site_code[ii]+'_mwnum'], 'spec', 1

     ; add options of setting labels
     options,'iug_meteor_'+site_code[ii]+'_uwnd', labels='MW '+site_code[ii]+' [km]'
     options,'iug_meteor_'+site_code[ii]+'_vwnd', labels='MW '+site_code[ii]+' [km]'
     options,'iug_meteor_'+site_code[ii]+'_uwndsig', labels='MW '+site_code[ii]+' [km]'
     options,'iug_meteor_'+site_code[ii]+'_vwndsig', labels='MW '+site_code[ii]+' [km]'
     options,'iug_meteor_'+site_code[ii]+'_mwnum', labels='MW '+site_code[ii]+' [km]'
   endif
  
  ;Clear time and data buffer:
   site_time=0
   zon_wind=0
   mer_wind=0
   zon_thermal=0
   mer_thermal=0
   meteor_num=0
   ; add tdegap
   tdegap, 'iug_meteor_'+site_code[ii]+'_uwnd',/overwrite
   tdegap, 'iug_meteor_'+site_code[ii]+'_vwnd',/overwrite
   tdegap, 'iug_meteor_'+site_code[ii]+'_uwndsig',/overwrite
   tdegap, 'iug_meteor_'+site_code[ii]+'_vwndsig',/overwrite
   tdegap, 'iug_meteor_'+site_code[ii]+'_mwnum',/overwrite
   
  endif
  jj=n_elements(local_paths) 
endfor 
print,'**********************************************************************************
print, 'Data loading is successful!!'
print,'**********************************************************************************

end

