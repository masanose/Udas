;+
;
;NAME:
;iug_load_meteor_ktb_nc
;
;PURPOSE:
;  Queries the Kyoto_RISH servers for the horizontal wind data (uwnd, vwnd, uwndsig, vwndsig, mwnum)
;  in the NetCDF format taken by the meteor wind radar (MWR) at Kototabang and loads data into
;  tplot format.
;
;SYNTAX:
; iug_load_meteor_ktb_nc, datatype = datatype, parameter = parameter, length = length, downloadonly = downloadonly, $
;                           trange = trange, verbose=verbose
;
;KEYWOARDS:
;  datatype = Observation data type. For example, iug_load_meteor_ktb_nc, datatype = 'thermosphere'.
;            The default is 'thermosphere'.
;  length = Data length '1-day' or '1-month'. For example, iug_load_meteor_srp_nc, length = '1_day'.
;           A kind of parameters is 2 types of '1_day', and '1_month'.  
; parameters = Data parameter. For example, iug_load_meteor_ktb_nc, parameter = 'h2t60min00'. 
;             A kind of parameters is 4 types of 'h2t60min00', 'h2t60min00', 'h4t60min00', 'h4t60min00'.
;             The default is 'all'.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;CODE:
; A. Shinbori, 19/09/2010.
;
;MODIFICATIONS:
; A. Shinbori, 24/03/2011.
; A. Shinbori, 11/07/2011.
;
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_meteor_ktb_nc, datatype = datatype, parameter = parameter, length=length, $
                             downloadonly = downloadonly, trange = trange, verbose = verbose

;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2
 
;************************************
;Load 'thermosphere' data by default:
;************************************
if (not keyword_set(datatype)) then datatype='thermosphere'

;*****************************
;Load '1_day' data by default:
;*****************************
if (not keyword_set(length)) then length='1_day'

;***********
;parameters:
;***********

;--- all parameters (default)
parameter_all = strsplit('h2t60min00 h2t60min30 h4t60min00 h4t60min30 h4t240min00',' ', /extract)

;--- check parameters
if(not keyword_set(parameter)) then parameter='all'
parameters = thm_check_valid_name(parameter, parameter_all, /ignore_case, /include_all)

print, parameters

;***************
;data directory:
;***************
site_data_dir = strsplit('h2km_t60min00/ h2km_t60min30/ h4km_t60min00/ h4km_t60min30/ h4km_t240min00/',' ', /extract)
site_data_lastmane = strsplit('.h2t60min00.nc .h2t60min30.nc .h4t60min00.nc .h4t60min30.nc .h4t240min00.nc',' ', /extract)

;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'Scientists who want to engage in collaboration with Research Institute for Sustainable Humanosphere (RISH) ' $
+ 'should contact the principal investigator of the meteor wind (MW) radar in Indonesia ' $
+ 'Prof. Toshitaka Tsuda, Kyoto University, who will organize such collaborations. ' $
+ 'There is a possibility that the PI of the MW radar will arrange offers so that there is less overlapping of themes between our research groups' $
+ 'Before you use the MW radar data for your papers, you must agree to the following points;' $
+ '  1. Before you submit your paper, you must contact the PI (Prof. Toshitaka Tsuda: tsuda@rish.kyoto-u.ac.jp) and discuss authorship.' $
+ '  2. When you submit your paper after doing the above item 1, you must mention the source of the data in the acknowledgment section of your paper.' $
+ '  3. In all circumstances, if anything is published you must send a hardcopy to the following address:' $
+ '    Prof. Toshitaka Tsuda' $
+ '    PI of the MW radar in Indonesia' $
+ '    Director of Research Institute for Sustainable Humanosphere,' $
+ '    Kyoto University' $
+ '    Gokasyo, Uji Kyoto 611-0011, Japan' $
+ '    e-mail: tsuda@rish.kyoto-u.ac.jp' 


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
kk=0
  
  for iii=0,n_elements(parameters)-1 do begin
  ;The parameter search:'
    for jjj=0, n_elements(site_data_lastmane)-1 do begin
       if parameters[iii] eq 'h2t60min00' then kk=0
       if parameters[iii] eq 'h2t60min30' then kk=1
       if parameters[iii] eq 'h4t60min00' then kk=2
       if parameters[iii] eq 'h4t60min30' then kk=3
       if parameters[iii] eq 'h4t240min00' then kk=4
    endfor
    
    if ~size(fns,/type) then begin
      if length eq '1_day' then begin 
        ;Get files for ith component:
        ;***************************       
         file_names = file_dailynames( $
                      file_format='YYYY/Wk'+$
                      'YYYYMMDD',trange=trange,times=times,/unique)+site_data_lastmane[kk]
      endif else if length eq '1_month' then begin
        ;Get files for ith component:
        ;***************************       
         file_names = file_dailynames( $
                      file_format='YYYY/Wk'+$
                      'YYYYMM',trange=trange,times=times,/unique)+site_data_lastmane[kk]
      endif
    ;        
    ;Define FILE_RETRIEVE structure:
    ;===============================
       source = file_retrieve(/struct)
       source.verbose=verbose
       source.local_data_dir =  root_data_dir() + 'iugonet/rish/misc/ktb/meteor/nc/ver1_1/'+length+'/'+site_data_dir[kk]
       source.remote_data_dir = 'http://database.rish.kyoto-u.ac.jp/arch/iugonet/data/mwr/kototabang/nc/'+site_data_dir[kk]
    
    ;Get files and local paths, and concatenate local paths:
    ;=======================================================
       local_paths=file_retrieve(file_names,_extra=source, /last_version)
       local_paths_all = ~(~size(local_paths_all,/type)) ? $
                        [local_paths_all, local_paths] : local_paths
       if ~(~size(local_paths_all,/type)) then local_paths=local_paths_all
    endif else file_names=fns

    ;--- Load data into tplot variables
    if (not keyword_set(downloadonly)) then downloadonly=0

    if (downloadonly eq 0) then begin

   ;Definition time and parameters:
   site_time=0
   zon_wind=0
   mer_wind=0
   zon_thermal=0
   mer_thermal=0
   meteor_num=0    
 
 ;Loop on files (read the NetCDF files): 
 ;======================================
      for j=jj,n_elements(local_paths)-1 do begin
          file= local_paths[j]
          if file_test(/regular,file) then  dprint,'Loading the wind data estimated from the MWR at Kototabang: ',file $
          else begin
             dprint,'The wind data estimated from the MWR at Kototabang',file,'not found. Skipping'
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
                  if (info.name eq 'time') and (attname eq 'units') then time_data=string(attvalue)
              endfor
          endfor
          
        ;  Enter the time infomation:
          time_info=strsplit(time_data,' ',/extract)
          syymmdd=time_info[2]
          shhmmss=time_info[3]
          time_diff=strsplit(time_info[4],':',/extract)
          time_diff2=fix(time_diff[0])*3600+fix(time_diff[1])*60   
          
    ; Get the variable
          ncdf_varget, cdfid, 'time', time
          ncdf_varget, cdfid, 'range', range
          ncdf_varget, cdfid, 'uwind', uwind
          ncdf_varget, cdfid, 'vwind', vwind
          ncdf_varget, cdfid, 'sig_uwind', sig_uwind
          ncdf_varget, cdfid, 'sig_vwind', sig_vwind
          ncdf_varget, cdfid, 'num', num

    ; Definition of arrary names
          unix_time = dblarr(n_elements(time))
          height=fltarr(n_elements(range))
          uwind_data=fltarr(n_elements(time),n_elements(range))
          vwind_data=fltarr(n_elements(time),n_elements(range))
          sig_uwind_data=fltarr(n_elements(time),n_elements(range))
          sig_vwind_data=fltarr(n_elements(time),n_elements(range))
          num_data=fltarr(n_elements(time),n_elements(range))

          for i=0, n_elements(time)-1 do begin
          
             ; Change hours since midnight of the first day of every month (Universal Time) into unixtime (1970-01-01 00:00:00)
              unix_time[i] = double(time[i])*3600 +time_double(syymmdd+'/'+shhmmss)-time_diff2
 
              for k=0, n_elements(range)-1 do begin
                  uwind_data[i,k]=uwind[0,k,i]
                  vwind_data[i,k]=vwind[0,k,i]
                  sig_uwind_data[i,k]=sig_uwind[0,k,i]
                  sig_vwind_data[i,k]=sig_vwind[0,k,i]
                  num_data[i,k]=num[0,k,i]
                  height[k]= range[k]/1000
                  
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
      append_array, site_time, unix_time
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

;Store data of meteor wind data:
;===============================

  if site_time[0] ne 0 then begin
     dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'T. Tsuda'))
     store_data,'iug_meteor_ktb_uwnd_'+parameters[kk],data={x:site_time, y:zon_wind, v:height},dlimit=dlimit
     options,'iug_meteor_ktb_uwnd_'+parameters[kk],ytitle='MW-ktb!CHeight!C[km]',ztitle='uwnd!C[m/s]'
     store_data,'iug_meteor_ktb_vwnd_'+parameters[kk],data={x:site_time, y:mer_wind, v:height},dlimit=dlimit
     options,'iug_meteor_ktb_vwnd_'+parameters[kk],ytitle='MW-ktb!CHeight!C[km]',ztitle='vwnd!C[m/s]'
     store_data,'iug_meteor_ktb_uwndsig_'+parameters[kk],data={x:site_time, y:zon_thermal, v:height},dlimit=dlimit
     options,'iug_meteor_ktb_uwndsig_'+parameters[kk],ytitle='MW-ktb!CHeight!C[km]',ztitle='uwndsig!C[m/s]'
     store_data,'iug_meteor_ktb_vwndsig_'+parameters[kk],data={x:site_time, y:mer_thermal, v:height},dlimit=dlimit
     options,'iug_meteor_ktb_vwndsig_'+parameters[kk],ytitle='MW-ktb!CHeight!C[km]',ztitle='vwndsig!C[m/s]'
     store_data,'iug_meteor_ktb_mwnum_'+parameters[kk],data={x:site_time, y:meteor_num, v:height},dlimit=dlimit
     options,'iug_meteor_ktb_mwnum_'+parameters[kk],ytitle='MW-ktb!CHeight!C[km]',ztitle='mwnum'


     ; add options
     options, ['iug_meteor_ktb_uwnd_'+parameters[kk],'iug_meteor_ktb_vwnd_'+parameters[kk],$
               'iug_meteor_ktb_uwndsig_'+parameters[kk],'iug_meteor_ktb_vwndsig_'+parameters[kk],$
               'iug_meteor_ktb_mwnum_'+parameters[kk]], 'spec', 1

     ; add options of setting labels
     options,'iug_meteor_ktb_uwnd_'+parameters[kk], labels='MW ktb'+parameters[kk]+' [km]'
     options,'iug_meteor_ktb_vwnd_'+parameters[kk], labels='MW ktb'+parameters[kk]+' [km]'
     options,'iug_meteor_ktb_uwndsig_'+parameters[kk], labels='MW ktb'+parameters[kk]+' [km]'
     options,'iug_meteor_ktb_vwndsig_'+parameters[kk], labels='MW ktb'+parameters[kk]+' [km]'
     options,'iug_meteor_ktb_mwnum_'+parameters[kk], labels='MW ktb'+parameters[kk]+' [km]'
   endif
  
  ;Clear time and data buffer:
   site_time=0
   zon_wind=0
   mer_wind=0
   zon_thermal=0
   mer_thermal=0
   meteor_num=0
   
   ; add tdegap
   tdegap, 'iug_meteor_ktb_uwnd_'+parameters[kk],dt=86400,/overwrite
   tdegap, 'iug_meteor_ktb_vwnd_'+parameters[kk],dt=86400,/overwrite
   tdegap, 'iug_meteor_ktb_uwndsig_'+parameters[kk],dt=86400,/overwrite
   tdegap, 'iug_meteor_ktb_vwndsig_'+parameters[kk],dt=86400,/overwrite
   tdegap, 'iug_meteor_ktb_mwnum_'+parameters[kk],dt=86400,/overwrite
   
   ; add tclip
   tclip, 'iug_meteor_ktb_uwnd_'+parameters[kk],-200,200,/overwrite
   tclip, 'iug_meteor_ktb_vwnd_'+parameters[kk],-200,200,/overwrite
   tclip, 'iug_meteor_ktb_uwndsig_'+parameters[kk],0,800,/overwrite
   tclip, 'iug_meteor_ktb_vwndsig_'+parameters[kk],0,800,/overwrite
   tclip, 'iug_meteor_ktb_mwnum_'+parameters[kk],0,1200,/overwrite   
   
  endif
  jj=n_elements(local_paths)
endfor
kk=0

print,'******************************
print, 'Data loading is successful!!'
print,'******************************

;******************************
;print of acknowledgement:
;******************************
print, '****************************************************************
print, 'Acknowledgement'
print, '****************************************************************
print, 'Scientists who want to engage in collaboration with Research Institute for Sustainable Humanosphere (RISH) ' 
print, 'should contact the principal investigator of the meteor wind (MW) radar in Indonesia ' 
print, 'Prof. Toshitaka Tsuda, Kyoto University, who will organize such collaborations. ' 
print, 'There is a possibility that the PI of the MW radar will arrange offers so that there is less overlapping of themes between our research groups' 
print, 'Before you use the MW radar data for your papers, you must agree to the following points;' 
print, '  1. Before you submit your paper, you must contact the PI (Prof. Toshitaka Tsuda: tsuda@rish.kyoto-u.ac.jp) and discuss authorship.' 
print, '  2. When you submit your paper after doing the above item 1, you must mention the source of the data in the acknowledgment section of your paper.' 
print, '  3. In all circumstances, if anything is published you must send a hardcopy to the following address:' 
print, '    Prof. Toshitaka Tsuda' 
print, '    PI of the MW radar in Indonesia' 
print, '    Director of Research Institute for Sustainable Humanosphere,' 
print, '    Kyoto University' 
print, '    Gokasyo, Uji Kyoto 611-0011, Japan' 
print, '    e-mail: tsuda@rish.kyoto-u.ac.jp'  

end

