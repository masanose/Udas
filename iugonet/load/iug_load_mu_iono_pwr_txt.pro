;+
;
;NAME:
;iug_load_mu_iono_pwr_txt
;
;PURPOSE:
;  Queries the Kyoto_RISH servers for ion and electron temperatures in text format 
;  estimated from the incoherent scatter observation of the MU radar at Shigaraki 
;  and loads data into tplot format.
;
;SYNTAX:
; iug_load_mu_iono_pwr_txt, datatype = datatype, parameter = parameter, downloadonly = downloadonly, $
;                          trange = trange, verbose=verbose
;
;KEYWOARDS:
;  datatype = Observation data type. For example, iug_load_mu_iono_pwr_txt, datatype = 'ionosphere'.
;            The default is 'ionosphere'.
;  parameter = parameter name of echo power data taken by the MU incherent scatter mode.  
;          For example, iug_load_mu_iono_pwr_txt, parameter = 'pwr1'.
;          The default is 'all', i.e., load all available parameters.  
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;CODE:
; A. Shinbori, 03/10/2012.
;
;MODIFICATIONS:
; 
;
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_mu_iono_pwr_txt, datatype = datatype, $
   parameter = parameter, $
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

;***********
;parameters:
;***********
;--- all parameters (default)
parameter_all = strsplit('pwr1 pwr2 pwr3 pwr4',' ', /extract)

;--- check site codes
if(not keyword_set(parameter)) then parameter='all'
parameters = thm_check_valid_name(parameter, parameter_all, /ignore_case, /include_all)

print, parameters

;==================================================================
;Download files, read data, and create tplot vars at each component
;==================================================================
;******************************************************************
;Loop on downloading files
;******************************************************************
;Get timespan, define FILE_NAMES, and load data:
;===============================================

;Definition of parameter:
h=0
site_time=0
jj=0  

for ii=0,n_elements(parameters)-1 do begin   
   if ~size(fns,/type) then begin 
     ;
     ;Get files for ith component:
     ;***************************       
      file_names = file_dailynames(file_format='YYYY/YYYYMMDD',trange=trange,times=times,/unique)+'_'+parameters[ii]+'.txt'
    
     ;        
     ;Define FILE_RETRIEVE structure:
     ;===============================
      source = file_retrieve(/struct)
      source.verbose=verbose
      source.local_data_dir =  root_data_dir() + 'iugonet/rish/misc/sgk/mu/ionosphere/pwr/text/'
      source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/mu/isdata/data/pwr/text/'
  
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

     ;Definition of parameters:
      s=''
      
     ;Loop on files (read the NetCDF files): 
     ;======================================
      for h=0,n_elements(local_paths)-1 do begin
         file= local_paths[h]
         if file_test(/regular,file) then  dprint,'Loading the ionosphere data estimated from the incoherent scatter observation of the MU radar: ',file $
         else begin
            dprint,'The ionosphere data estimated from the incoherent scatter observation of the MU radar ',file,' not found. Skipping'
            continue
         endelse
    
        ;
        ;Open the read file:
        ;===================
         openr,lun,file,/get_lun

        ;
        ;Read the beam direction:
        ;=====================        
         readf,lun,s
         temp = strsplit(s,",",/extract)
         az = temp[0]
         ze = temp[1]
   
        ;
        ;Read the height data:
        ;=====================        
         readf,lun,s
         height = float(strsplit(s,',',/extract))
      
         while(not eof(lun)) do begin
           ;Read the time data:
            readf,lun,s
            data=strsplit(s,' ',/extract)
            year = strmid(data[0],0,4)
            month = strmid(data[0],5,2)
            day = strmid(data[0],8,2)
            time = data[1]
           ;Start time:
            stime = time_double(year+'-'+month+'-'+day+'/'+time)
            year = strmid(data[3],0,4)
            month = strmid(data[3],5,2)
            day = strmid(data[3],8,2)
            time = data[4]
           ;End time:
            etime = time_double(year+'-'+month+'-'+day+'/'+time)
            mu_time = (stime+etime)/2.0D - time_double('1970-1-1/09:00:00')
         
           ;Definition of temp. arraies: 
            pwr = fltarr(1,n_elements(height))
            
            pwr[0,*]= float(data[5:n_elements(height)-1+5])
            for j=0,n_elements(height)-1 do begin       
               a = float(pwr[0,j])            
               wbad = where(a eq 999.0 ,nbad)
               if nbad gt 0 then a[wbad] = !values.f_nan
               pwr[0,j] =a
            endfor
                               
           ;======================================================
           ;Append data of time and electron and ion temperatures:
           ;======================================================
            append_array, site_time, mu_time
            append_array, pwr_app, pwr
         endwhile
         free_lun,lun
      endfor

     ;******************************
     ;Store data in TPLOT variables:
     ;******************************

     ;Acknowlegment string (use for creating tplot vars)
      acknowledgstring = 'If you acquire the middle and upper atmospher (MU) radar data, ' $
                       + 'we ask that you acknowledge us in your use of the data. This may be done by' $
                       + 'including text such as the MU data provided by Research Institute' $
                       + 'for Sustainable Humanosphere of Kyoto University. We would also' $
                       + 'appreciate receiving a copy of the relevant publications.'

      if size(pwr_app,/type) eq 4 then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'Y. Yamamoto'))
         store_data,'iug_mu_iono_'+parameters[ii],data={x:site_time, y:pwr_app,v:height},dlimit=dlimit
         options,'iug_mu_iono_'+parameters[ii],ytitle='MU-iono!CHeight!C[km]',ztitle= parameters[ii]+'!C[dB]'
         options,'iug_mu_iono_'+parameters[ii],spec=1
      
        ;Add options of setting labels
        ; options,'iug_mu_iono_temp_ti', labels='MU iono Height [km]'
        ; options,'iug_mu_iono_temp_te', labels='MU iono Height [km]'
        ; options,'iug_mu_iono_temp_er_ti', labels='MU iono Height [km]'
        ; options,'iug_mu_iono_temp_er_te', labels='MU iono Height [km]'
        ; options,'iug_mu_iono_temp_er_tr', labels='MU iono Height [km]'
        ; options,'iug_mu_iono_temp_snr,', labels='MU iono Height [km]'
   
     ;Add tdegap
      tdegap, 'iug_mu_iono_'+parameters[ii],dt=3600,/overwrite

   endif
  
  ;Clear time and data buffer:
   site_time=0
   pwr_app=0

   endif
   jj=n_elements(local_paths)
endfor

new_vars=tnames('iug_mu_iono_pwr*')
if new_vars[0] ne '' then begin  
   print,'******************************
   print, 'Data loading is successful!!'
   print,'******************************
endif

;******************************
;print of acknowledgement:
;******************************
print, '****************************************************************
print, 'Acknowledgement'
print, '****************************************************************
print, 'If you acquire the middle and upper atmosphere (MU) radar data, '
print, 'we ask that you acknowledge us in your use of the data. ' 
print, 'This may be done by including text such as MU data provided ' 
print, 'by Research Institute for Sustainable Humanosphere of Kyoto University. ' 
print, 'We would also appreciate receiving a copy of the relevant publications.'

end

