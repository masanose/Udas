;+
;
;NAME:
;iug_load_mu_iono_teti_nc
;
;PURPOSE:
;  Queries the Kyoto_RISH servers for ion and electron temperatures in netCDF format 
;  estimated from the incoherent scatter observation of the MU radar at Shigaraki 
;  and loads data into tplot format.
;
;SYNTAX:
; iug_load_mu_iono_teti_nc, datatype = datatype, downloadonly = downloadonly, $
;                          trange = trange, verbose=verbose
;
;KEYWOARDS:
;  datatype = Observation data type. For example, iug_load_mu_iono_teti_nc, datatype = 'ionosphere'.
;            The default is 'ionosphere'.  
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;CODE:
; A. Shinbori, 02/10/2012.
;
;MODIFICATIONS:
; A. Shinbori, 12/11/2012. 
; A. Shinbori, 24/12/2012.
; 
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_mu_iono_teti_nc, datatype = datatype, $
   downloadonly = downloadonly, $
   trange = trange, $
   verbose = verbose

;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2
 
;************************************
;Load 'thermosphere' data by default:
;************************************
if (not keyword_set(datatype)) then datatype='ionosphere'

;******************************************************************
;Loop on downloading files
;******************************************************************
;Get timespan, define FILE_NAMES, and load data:
;===============================================
;
;===================================================================
;Download files, read data, and create tplot vars at each component:
;===================================================================
h=0
site_time=0 
if ~size(fns,/type) then begin 
  ;
  ;Get files for ith component:
  ;***************************       
   file_names = file_dailynames(file_format='YYYY/YYYYMMDD',trange=trange,times=times,/unique)+'_teti.nc'
    
  ;        
  ;Define FILE_RETRIEVE structure:
  ;===============================
   source = file_retrieve(/struct)
   source.verbose=verbose
   source.local_data_dir =  root_data_dir() + 'iugonet/rish/misc/sgk/mu/ionosphere/teti/nc/'
   source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/mu/isdata/data/teti/netcdf/'
  
  ;  
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
      
  ;Loop on files (read the NetCDF files): 
  ;======================================
   for h=0,n_elements(local_paths)-1 do begin
      file= local_paths[h]
      if file_test(/regular,file) then  dprint,'Loading the ionosphere data estimated from the incoherent scatter observation of the MU radar: ',file $
      else begin
         dprint,'The ionosphere data estimated from the incoherent scatter observation of the MU radar ',file,' not found. Skipping'
         continue
      endelse
    
      cdfid = ncdf_open(file,/NOWRITE)  ; Open the file
      glob = ncdf_inquire( cdfid )    ; Find out general info

     ;Show user the size of each dimension

      print,'Dimensions', glob.ndims
      for i=0,glob.ndims-1 do begin
         ncdf_diminq, cdfid, i, name,size
         if i EQ glob.recdim then  $
            print,'    ', name, size, '(Unlimited dim)' $
         else      $
            print,'    ', name, size  
      endfor

     ;Now tell user about the variables

      print
      print, 'Variables'
      for m=0,glob.nvars-1 do begin

        ;Get information about the variable
         info = ncdf_varinq(cdfid, m)
         FmtStr = '(A," (",A," ) Dimension Ids = [ ", 10(I0," "),$)'
         print, FORMAT=FmtStr, info.name,info.datatype, info.dim[*]
         print, ']'

        ;Get attributes associated with the variable
         for l=0,info.natts-1 do begin
            attname = ncdf_attname(cdfid,m,l)
            ncdf_attget,cdfid,m,attname,attvalue
            print,' Attribute ', attname, '=', string(attvalue)
            if (info.name eq 'stime') and (attname eq 'units') then time_data=string(attvalue)
         endfor
      endfor

     ;Calculation the start time infomation from the attribute data:
      time_info=strsplit(time_data,' ',/extract)
      syymmdd=time_info[2]
      shhmmss=time_info[3]
      time_diff=strsplit(time_info[4],':',/extract)
      time_diff2=fix(time_diff[0])*3600+fix(time_diff[1])*60 

     ;Get the variable
      ncdf_varget, cdfid, 'lat', lat
      ncdf_varget, cdfid, 'lon', lon
      ncdf_varget, cdfid, 'obsdate', obsdate
      ncdf_varget, cdfid, 'stime', stime
      ncdf_varget, cdfid, 'etime', etime
      ncdf_varget, cdfid, 'height', height
      ncdf_varget, cdfid, 'Ti', ti
      ncdf_varget, cdfid, 'Te', te
      ncdf_varget, cdfid, 'icon', icon
      ncdf_varget, cdfid, 'er_ti', er_ti
      ncdf_varget, cdfid, 'er_te', er_te
      ncdf_varget, cdfid, 'er_tr', er_tr
      ncdf_varget, cdfid, 'snr', snr

     ;Definition of arrary names
      unix_time = dblarr(n_elements(stime))
      center_time = (stime+etime)/2.0                         
      for i=0, n_elements(center_time)-1 do begin
        ;Change seconds since the midnight of every day (Local Time) into unix time (1970-01-01 00:00:00)    
         unix_time[i] = double(center_time[i])+time_double(syymmdd+'/'+shhmmss)-time_diff2 
         
        ;Replace the missing value by NAN for drift velocity      
         for k=0, n_elements(height)-1 do begin
            a = ti[k,i]            
            wbad = where(a eq -999.0 or icon[k,i] eq 3,nbad)
            if nbad gt 0 then a[wbad] = !values.f_nan
            ti[k,i] =a
            b = te[k,i]            
            wbad = where(b eq -999.0 or icon[k,i] eq 3,nbad)
            if nbad gt 0 then b[wbad] = !values.f_nan
            te[k,i] =b
            c = er_ti[k,i]            
            wbad = where(c eq -999.0 or icon[k,i] eq 3,nbad)
            if nbad gt 0 then c[wbad] = !values.f_nan
            er_ti[k,i] =c
            d = er_te[k,i]            
            wbad = where(d eq -999.0 or icon[k,i] eq 3,nbad)
            if nbad gt 0 then d[wbad] = !values.f_nan
            er_te[k,i] =d
            e = er_tr[k,i]           
            wbad = where(e eq -999.0 or icon[k,i] eq 3,nbad)
            if nbad gt 0 then e[wbad] = !values.f_nan
            snr[k,i] =e
            f = er_tr[k,i]           
            wbad = where(f eq -999.0 or icon[k,i] eq 3,nbad)
            if nbad gt 0 then f[wbad] = !values.f_nan
            snr[k,i] =f         
         endfor
      endfor
      
      ti = transpose(ti)
      te = transpose(te)
      er_ti = transpose(er_ti)
      er_te = transpose(er_te)
      er_tr = transpose(er_tr)
      snr = transpose(snr)
      
     ;==============================
     ;Append array of time and data:
     ;==============================
      append_array, site_time, unix_time
      append_array, ti_app, ti
      append_array, te_app, te
      append_array, er_ti_app, er_ti
      append_array, er_te_app, er_te
      append_array, er_tr_app, er_tr
      append_array, snr_app, snr
      
      ncdf_close,cdfid  ; done
   endfor

  ;==============================
  ;Store data in TPLOT variables:
  ;==============================
  ;Acknowlegment string (use for creating tplot vars)
   acknowledgstring = 'If you acquire the middle and upper atmospher (MU) radar data, ' $
                    + 'we ask that you acknowledge us in your use of the data. This may be done by ' $
                    + 'including text such as the MU data provided by Research Institute ' $
                    + 'for Sustainable Humanosphere of Kyoto University. We would also' $
                    + 'appreciate receiving a copy of the relevant publications.The distribution of ' $
                    + 'ionogram data has been partly supported by the IUGONET (Inter-university Upper ' $
                    + 'atmosphere Global Observation NETwork) project (http://www.iugonet.org/) funded '$
                    + 'by the Ministry of Education, Culture, Sports, Science and Technology (MEXT), Japan.'

   if size(ti_app,/type) eq 4 then begin
      dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'Y. Yamamoto'))
      store_data,'iug_mu_iono_ti',data={x:site_time, y:ti_app,v:height},dlimit=dlimit
      options,'iug_mu_iono_ti',ytitle='MU-iono!CHeight!C[km]',ztitle='Ion temp.!C[K]'
      options,'iug_mu_iono_ti',spec=1
      store_data,'iug_mu_iono_te',data={x:site_time, y:te_app,v:height},dlimit=dlimit
      options,'iug_mu_iono_te',ytitle='MU-iono!CHeight!C[km]',ztitle='Electron temp.!C[K]'
      options,'iug_mu_iono_te',spec=1
      store_data,'iug_mu_iono_er_ti',data={x:site_time, y:er_ti_app,v:height},dlimit=dlimit
      options,'iug_mu_iono_er_ti',ytitle='MU-iono!CHeight!C[km]',ztitle='Ion temp. Error!C[K]'
      options,'iug_mu_iono_er_ti',spec=1
      store_data,'iug_mu_iono_er_te',data={x:site_time, y:er_te_app,v:height},dlimit=dlimit
      options,'iug_mu_iono_er_te',ytitle='MU-iono!CHeight!C[km]',ztitle='Electron temp. Error!C[K]'
      options,'iug_mu_iono_er_te',spec=1
      store_data,'iug_mu_iono_er_tr',data={x:site_time, y:er_tr_app,v:height},dlimit=dlimit
      options,'iug_mu_iono_er_tr',ytitle='MU-iono!CHeight!C[km]',ztitle='Te/Ti Error!C[K]'
      options,'iug_mu_iono_er_tr',spec=1
      store_data,'iug_mu_iono_snr',data={x:site_time, y:snr_app,v:height},dlimit=dlimit
      options,'iug_mu_iono_snr',ytitle='MU-iono!CHeight!C[km]',ztitle='SNR!C[dB]'
      options,'iug_mu_iono_snr',spec=1
      
     ;Add tdegap
      tdegap, 'iug_mu_iono_ti',dt=3600,/overwrite
      tdegap, 'iug_mu_iono_te',dt=3600,/overwrite
      tdegap, 'iug_mu_iono_er_ti',dt=3600,/overwrite
      tdegap, 'iug_mu_iono_er_te',dt=3600,/overwrite
      tdegap, 'iug_mu_iono_er_tr',dt=3600,/overwrite
      tdegap, 'iug_mu_iono_snr',dt=3600,/overwrite 
   endif
  
  ;Clear time and data buffer:
   site_time=0
   ti_app=0
   te_app=0
   er_ti_app=0
   er_te_app=0
   er_tr_app=0
   snr_app=0
endif

new_vars=tnames('iug_mu_iono_ti')
if new_vars[0] ne '' then begin  
   print,'******************************
   print, 'Data loading is successful!!'
   print,'******************************
endif

;*************************
;print of acknowledgement:
;*************************
print, '****************************************************************
print, 'Acknowledgement'
print, '****************************************************************
print, 'If you acquire the middle and upper atmosphere (MU) radar data, '
print, 'we ask that you acknowledge us in your use of the data. ' 
print, 'This may be done by including text such as MU data provided ' 
print, 'by Research Institute for Sustainable Humanosphere of Kyoto University. ' 
print, 'We would also appreciate receiving a copy of the relevant publications. '
print, 'The distribution of ionogram data has been partly supported by the IUGONET '
print, '(Inter-university Upper atmosphere Global Observation NETwork) project '
print, '(http://www.iugonet.org/) funded by the Ministry of Education, Culture, '
print, 'Sports, Science and Technology (MEXT), Japan.' 

end

