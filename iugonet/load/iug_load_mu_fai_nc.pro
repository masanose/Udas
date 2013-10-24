;+
;
;NAME:
;iug_load_mu_fai_nc
;
;PURPOSE:
;  Queries the Kyoto_RISH servers for the FAI observation data in the netCDF format 
;  taken by the MU radar and loads data into tplot format.
;
;SYNTAX:
; iug_load_mu_fai_nc, datatype = datatype, parameter1=parameter1, parameter2=parameter2 $
;                          downloadonly=downloadonly, trange=trange, verbose=verbose
;
;KEYWOARDS:
;  datatype = Observation data type. For example, iug_load_mu_fai_nc, datatype = 'ionosphere'.
;            The default is 'ionosphere'. 
;  parameter1 = first parameter name of MU FAI obervation data.  
;          For example, iug_load_mu_fai_nc, parameter = 'iemdc3'.
;          The default is 'all', i.e., load all available parameters.
;  parameter2 = second parameter name of MU FAI obervation data.  
;          For example, iug_load_mu_fai_nc, parameter = 'dpl1'.
;          The default is 'all', i.e., load all available parameters.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;
;CODE:
; A. Shinbori, 18/08/2013.
; A. Shinbori, 10/10/2013.
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


pro iug_load_mu_fai_nc, datatype = datatype, $
  parameter1=parameter1, $
  downloadonly=downloadonly, $
  trange=trange, $
  verbose=verbose

;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2
 
;**********************************
;Load 'ionosphere' data by default:
;**********************************
if (not keyword_set(datatype)) then datatype='ionosphere'

;************
;parameters1:
;************
;--- all parameters1 (default)
parameter1_all = strsplit('iecob3 ieewb5 ieimga ieimgb ieimgm ieimgt ieis01 iemb5i iemcb3 iemdb3 iemdb5 '+$
                          'iemdc3 iemy3a iemy3b iemy3c iemyb5 iensb5 iepbr1 iepbr2 iepbr3 iepbr4 iepbr5 '+$
                          'iepbrt ieper1 ieper2 ieper3 ieper4 ieper5 ieper6 ieper7 ieper8 '+$
                          'iepsi5 iepsit iesp01 iess01 iess02 iess03 iess04 iess05 iess2l '+$
                          'iess3l iess4l iess8c iessb5 iesst2 iesst3 iet101 iet102 ieto02 ieto03 '+$
                          'ieto16 ietob3 ietob4 ietob5 iey4ch iey4ct ieyo4a ieyo4b ieyo4c '+$
                          'ieyo4d ieyo4e ieyo4f ieyo4g ieyo5a ieyo5b ieyo5c ieyo5d ieyo5e '+$
                          'ieyo5f ieyo5g ieyo5m ifco02 ifco03 ifco04 ifco16 ifim16 ifmb16 ifmc16 '+$
                          'ifmd16 ifmf16 ifmy01 ifmy02 ifmy03 ifmy04 ifmy05 ifmyc1 ifmyc2 '+$
                          'ifmyc3 ifmyc4 ifmyc5 ifmyc6 ifmyc7 ifmyca ifmycb ifmyt1 ifmyt2 '+$
                          'ifmyt3 ifmyt4 ifmyt5 ifmyu1 ifmyu2 ifmyu3 ifmyu4 ifmyu5 ifmyv1 '+$
                          'ifpsi1 ifpsit ifss02 iftes1 iftes2 iftes3 iftes5 iftes6 iftes7 '+$
                          'iftes8 ifts01 ifts02 ifts03 ifts04 ifts05 ifts06 ifts07',' ', /extract)

;--- check site codes
if(not keyword_set(parameter1)) then parameter1='all'
parameters = thm_check_valid_name(parameter1, parameter1_all, /ignore_case, /include_all)

print, parameters

;************
;parameters2:
;************
;--- all parameters2 (default)
parameter2_all = strsplit('dpl1 dpl2 dpl3 dpl4 dpl5 dpl6 dpl7 dpl8 dpl9 dpl10 dpl11 dpl12 dpl13 dpl14 dpl15 dpl16 '+$
                          'pwr1 pwr2 pwr3 pwr4 pwr5 pwr6 pwr7 pwr8 pwr9 pwr10 pwr11 pwr12 pwr13 pwr14 pwr15 pwr16 '+$
                          'wdt1 wdt2 wdt3 wdt4 wdt5 wdt6 wdt7 wdt8 wdt9 wdt10 wdt11 wdt12 wdt13 wdt14 wdt15 wdt16 '+$
                          'snr1 snr2 snr3 snr4 snr5 snr6 snr7 snr8 snr9 snr10 snr11 snr12 snr13 snr14 snr15 snr16 '+$
                          'pn1 pn2 pn3 pn4 pn5 pn6 pn7 pn8 pn9 pn10 pn11 pn12 pn13 pn14 pn15 pn16',' ', /extract)

;--- check parameters
if(not keyword_set(parameter2)) then parameter2='all'
parameters2 = thm_check_valid_name(parameter2, parameter2_all, /ignore_case, /include_all)

print, parameters2

;*****************
;defition of unit:
;*****************
;--- all parameters2 (default)
unit_all = strsplit('m/s dB',' ', /extract)

;******************************************************************
;Loop on downloading files
;******************************************************************
;Get timespan, define FILE_NAMES, and load data:
;===============================================
;
;===================================================================
;Download files, read data, and create tplot vars at each component:
;===================================================================
jj=0
for ii=0,n_elements(parameters)-1 do begin
   if ~size(fns,/type) then begin

        ;Get files for ith component:
        ;***************************
      file_names = file_dailynames( $
      file_format='YYYY/YYYYMMDD/'+$
                  'YYYYMMDD',trange=trange,times=times,/unique)+'.'+parameters[ii]+'.nc'
        ;
        ;Define FILE_RETRIEVE structure:
        ;===============================
         source = file_retrieve(/struct)
         source.verbose=verbose
         source.local_data_dir = root_data_dir() + 'iugonet/rish/misc/sgk/mu/fai/nc/'
         source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/mu/fai/data/nc/'
    
     ;Get files and local paths, and concatenate local paths:
     ;=======================================================
      local_paths=file_retrieve(file_names,_extra=source)
      local_paths_all = ~(~size(local_paths_all,/type)) ? $
                        [local_paths_all, local_paths] : local_paths
      if ~(~size(local_paths_all,/type)) then local_paths=local_paths_all
   endif else file_names=fns

  ;--- Load data into tplot variables
   if (not keyword_set(downloadonly)) then downloadonly=0

   if (downloadonly eq 0) then begin

     ;===================================================
     ;read data, and create tplot vars at each parameter:
     ;===================================================
     ;Read the files:
     ;===============
   
     ;Definition of time and parameters:
      mu_time=0
      pwr1 = 0
      wdt1 = 0
      dpl1 = 0
      pn1 = 0
  
     ;==============
     ;Loop on files: 
     ;============== 
      for j=jj,n_elements(local_paths)-1 do begin
         file= local_paths[j]
         if file_test(/regular,file) then  dprint,'Loading the FAI observation data taken by the EAR: ',file $
         else begin
            dprint,'The FAI observation data taken by the MU radar ',file,' not found. Skipping'
            continue
         endelse
    
         cdfid = ncdf_open(file,/NOWRITE)  ; Open the file
         glob = ncdf_inquire( cdfid )    ; Find out general info

        ;Show user the size of each dimension
         print,'Dimensions', glob.ndims
         for i=0,glob.ndims-1 do begin
            ncdf_diminq, cdfid, i, name,size
            if i eq glob.recdim then  $
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
               if (info.name eq 'time') and (attname eq 'units') then time_data=string(attvalue)
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
         ncdf_varget, cdfid, 'sealvl', sealvl
         ncdf_varget, cdfid, 'bmwdh', bmwdh
         ncdf_varget, cdfid, 'freq', freq
         ncdf_varget, cdfid, 'ipp', ipp
         ncdf_varget, cdfid, 'ndata', ndata
         ncdf_varget, cdfid, 'ncoh', ncoh
         ncdf_varget, cdfid, 'nicoh', nicoh
         ncdf_varget, cdfid, 'beam', beam
         ncdf_varget, cdfid, 'range', range
         ncdf_varget, cdfid, 'az', az
         ncdf_varget, cdfid, 'ze', ze
         ncdf_varget, cdfid, 'date', date
         ncdf_varget, cdfid, 'time', time
         ncdf_varget, cdfid, 'height', height
         ncdf_varget, cdfid, 'pwr', pwr
         ncdf_varget, cdfid, 'width', width
         ncdf_varget, cdfid, 'dpl', dpl
         ncdf_varget, cdfid, 'pnoise', pnoise
 
        ;Calculation of unix time:
         year = fix(strmid(strtrim(string(date),1),0,4))
         month = fix(strmid(strtrim(string(date),1),4,2))
         day = fix(strmid(strtrim(string(date),1),6,2))
                         
        ;Definition of arrary names
         height2 = fltarr(n_elements(range))
         unix_time = dblarr(n_elements(time))
         pwr1_mu=fltarr(n_elements(time),n_elements(range),n_elements(beam))
         wdt1_mu=fltarr(n_elements(time),n_elements(range),n_elements(beam))
         dpl1_mu=fltarr(n_elements(time),n_elements(range),n_elements(beam))
         snr1_mu=fltarr(n_elements(time),n_elements(range),n_elements(beam))
         pnoise1_mu=fltarr(n_elements(time),n_elements(beam)) 
    
         for i=0, n_elements(time)-1 do begin
           ;Change seconds since the midnight of every day (Local Time) into unix time (1970-01-01 00:00:00)      
            unix_time[i] = double(time[i]) +time_double(string(syymmdd)+'/'+string(shhmmss))-double(time_diff2)            
            for k=0, n_elements(range)-1 do begin
               for l=0, n_elements(beam)-1 do begin           
                  a = pwr[k,i,l]            
                  wbad = where(a eq 10000000000,nbad)
                  if nbad gt 0 then a[wbad] = !values.f_nan
                  pwr[k,i,l] =a
                  b = width[k,i,l]            
                  wbad = where(b eq 10000000000,nbad)
                  if nbad gt 0 then b[wbad] = !values.f_nan
                  width[k,i,l]  =b
                  c = dpl[k,i,l]            
                  wbad = where(c eq 10000000000,nbad)
                  if nbad gt 0 then c[wbad] = !values.f_nan
                  dpl[k,i,l] =c                   
                  pwr1_mu[i,k,l]=pwr[k,i,l]  
                  wdt1_mu[i,k,l]=width[k,i,l]  
                  dpl1_mu[i,k,l]=dpl[k,i,l]
               endfor        
            endfor
            for l=0, n_elements(beam)-1 do begin            
               d = pnoise[i,l]            
               wbad = where(d eq 10000000000,nbad)
               if nbad gt 0 then d[wbad] = !values.f_nan
               pnoise[i,l] =d
               pnoise1_mu[i,l]=pnoise[i,l]            
            endfor
         endfor
         ncdf_close,cdfid  ; done
         
        ;Calculation of SNR
         snr=fltarr(n_elements(time),n_elements(range),n_elements(beam)) 
         for i=0,n_elements(time)-1 do begin
            for l=0,n_elements(beam)-1 do begin
               for k=0,n_elements(range)-1 do begin
                  snr1_mu[i,k,l]=pwr1_mu[i,k,l]-(pnoise1_mu[i,l]+alog10(ndata))
              endfor 
            endfor
         endfor
       
        ;=============================
        ;Append data of time and data:
        ;=============================
         append_array, mu_time, unix_time
         append_array, pwr1, pwr1_mu
         append_array, wdt1, wdt1_mu
         append_array, dpl1, dpl1_mu
         append_array, pn1, pnoise1_mu
         append_array, snr1, snr1_mu         
      endfor

     ;==============================
     ;Store data in TPLOT variables:
     ;==============================
     ;Acknowlegment string (use for creating tplot vars)
      acknowledgstring = 'If you acquire the middle and upper atmospher (MU) radar data, ' $
                       + 'we ask that you acknowledge us in your use of the data. This may be done by ' $
                       + 'including text such as the MU data provided by Research Institute ' $
                       + 'for Sustainable Humanosphere of Kyoto University. We would also ' $
                       + 'appreciate receiving a copy of the relevant publications. '$
                       + 'The distribution of MU radar data has been partly supported by the IUGONET '$
                       + '(Inter-university Upper atmosphere Global Observation NETwork) project '$
                       + '(http://www.iugonet.org/) funded by the Ministry of Education, Culture, '$
                       + 'Sports, Science and Technology (MEXT), Japan.'
                       
      if n_elements(mu_time) gt 1 then begin
         bname2=strarr(n_elements(beam))
         bname=strarr(n_elements(beam))
         pwr2_mu=fltarr(n_elements(mu_time),n_elements(range))
         wdt2_mu=fltarr(n_elements(mu_time),n_elements(range))
         dpl2_mu=fltarr(n_elements(mu_time),n_elements(range))
         snr2_mu=fltarr(n_elements(mu_time),n_elements(range))
         pnoise2_mu=fltarr(n_elements(mu_time))       
         if size(pwr1,/type) eq 4 then begin
            dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'M. Yamamoto'))         
           ;Store data of wind velocity
            for l=0, n_elements(beam)-1 do begin
               bname2[l]=string(beam[l]+1)
               bname[l]=strsplit(bname2[l],' ', /extract)
               for k=0, n_elements(range)-1 do begin
                  height2[k]=height[k,l]
               endfor
               for i=0, n_elements(mu_time)-1 do begin
                  for k=0, n_elements(range)-1 do begin
                     pwr2_mu[i,k]=pwr1[i,k,l]
                  endfor
               endfor
              ;print, pwr2_ear
               store_data,'iug_mu_fai_'+parameters[ii]+'_pwr'+bname[l],data={x:mu_time, y:pwr2_mu, v:height2},dlimit=dlimit
               new_vars=tnames('iug_mu_fai_'+parameters[ii]+'_pwr'+bname[l])
               if new_vars[0] ne '' then begin                 
                  options,'iug_mu_fai_'+parameters[ii]+'_pwr'+bname[l],ytitle='MU-FAI!CHeight!C[km]',ztitle='pwr'+bname[l]+'!C[dB]'
                  options,'iug_mu_fai_'+parameters[ii]+'_pwr'+bname[l],'spec',1
                  tdegap, 'iug_mu_fai_'+parameters[ii]+'_pwr'+bname[l], /overwrite
               endif               
               for i=0, n_elements(mu_time)-1 do begin
                  for k=0, n_elements(range)-1 do begin
                     wdt2_mu[i,k]=wdt1[i,k,l]
                  endfor
               endfor
               store_data,'iug_mu_fai_'+parameters[ii]+'_wdt'+bname[l],data={x:mu_time, y:wdt2_mu, v:height2},dlimit=dlimit
               new_vars=tnames('iug_mu_fai_'+parameters[ii]+'_wdt'+bname[l])
               if new_vars[0] ne '' then begin 
                  options,'iug_mu_fai_'+parameters[ii]+'_wdt'+bname[l],ytitle='MU-FAI!CHeight!C[km]',ztitle='wdt'+bname[l]+'!C[m/s]'
                  options,'iug_mu_fai_'+parameters[ii]+'_wdt'+bname[l],'spec',1
                  tdegap, 'iug_mu_fai_'+parameters[ii]+'_wdt'+bname[l], /overwrite
               endif               
               for i=0, n_elements(mu_time)-1 do begin
                  for k=0, n_elements(range)-1 do begin
                     dpl2_mu[i,k]=dpl1[i,k,l]
                  endfor
               endfor
               store_data,'iug_mu_fai_'+parameters[ii]+'_dpl'+bname[l],data={x:mu_time, y:dpl2_mu, v:height2},dlimit=dlimit
               new_vars=tnames('iug_mu_fai_'+parameters[ii]+'_dpl'+bname[l])
               if new_vars[0] ne '' then begin 
                  options,'iug_mu_fai_'+parameters[ii]+'_dpl'+bname[l],ytitle='MU-FAI!CHeight!C[km]',ztitle='dpl'+bname[l]+'!C[m/s]'
                  options,'iug_mu_fai_'+parameters[ii]+'_dpl'+bname[l],'spec',1
                  tdegap, 'iug_mu_fai_'+parameters[ii]+'_dpl'+bname[l], /overwrite
               endif
               for i=0, n_elements(mu_time)-1 do begin
                  for k=0, n_elements(range)-1 do begin
                     snr2_mu[i,k]=snr1[i,k,l]
                  endfor
               endfor
               store_data,'iug_mu_fai_'+parameters[ii]+'_snr'+bname[l],data={x:mu_time, y:snr2_mu, v:height2},dlimit=dlimit
               new_vars=tnames('iug_mu_fai_'+parameters[ii]+'_snr'+bname[l])
               if new_vars[0] ne '' then begin
                  options,'iug_mu_fai_'+parameters[ii]+'_snr'+bname[l],ytitle='MU-FAI!CHeight!C[km]',ztitle='snr'+bname[l]+'!C[dB]'
                  options,'iug_mu_fai_'+parameters[ii]+'_snr'+bname[l],'spec',1
                  tdegap, 'iug_mu_fai_'+parameters[ii]+'_snr'+bname[l], /overwrite
               endif             
               for i=0, n_elements(mu_time)-1 do begin
                  pnoise2_mu[i]=pn1[i,l]
               end
               store_data,'iug_mu_fai_'+parameters[ii]+'_pn'+bname[l],data={x:mu_time, y:pnoise2_mu},dlimit=dlimit
               new_vars=tnames('iug_mu_fai_'+parameters[ii]+'_pn'+bname[l])
               if new_vars[0] ne '' then begin 
                  options,'iug_mu_fai_'+parameters[ii]+'_pn'+bname[l],ytitle='pn'+bname[l]+'!C[dB]' 
                  tdegap, 'iug_mu_fai_'+parameters[ii]+'_pn'+bname[l], /overwrite    
               endif      
            endfor
         endif
         new_vars=tnames('iug_mu_fai*')
         if new_vars[0] ne '' then begin    
            print,'*****************************
            print,'Data loading is successful!!'
            print,'*****************************
         endif
      endif
   endif
  ;Clear time and data buffer:
   mu_time=0
   pwr1 = 0
   wdt1 = 0
   dpl1 = 0
   snr1 = 0
   pn1 = 0
   
   jj=n_elements(local_paths)
endfor

;*************************
;print of acknowledgement:
;*************************
print, '****************************************************************
print, 'Acknowledgement'
print, '****************************************************************
print, 'If you acquire the middle and upper atmosphere (MU) radar data,'
print, 'we ask that you acknowledge us in your use of the data.' 
print, 'This may be done by including text such as MU data provided' 
print, 'by Research Institute for Sustainable Humanosphere of Kyoto University.' 
print, 'We would also appreciate receiving a copy of the relevant publications.'
print, 'The distribution of MU radar data has been partly supported by the IUGONET'
print, '(Inter-university Upper atmosphere Global Observation NETwork) project'
print, '(http://www.iugonet.org/) funded by the Ministry of Education, Culture,'
print, 'Sports, Science and Technology (MEXT), Japan.'

end
