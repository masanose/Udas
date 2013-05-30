;+
;
;NAME:
;iug_load_radiosonde_sgk_csv
;
;PURPOSE:
;  Queries the Kyoto RISH server for the text data (press, temp, rh, uwnd, vwnd) 
;  of the troposphere in csv format taken by the radiosonde at Shigaraki MU Observatory 
;  and loads data into tplot format.
;
;SYNTAX:
; iug_load_radiosonde_sgk_csv, datatype = datatype, site=site, $
;                        downloadonly=downloadonly, trange=trange, verbose=verbose
;
;KEYWOARDS:
;  datatype = Observation data type. For example, iug_load_radiosonde_sgk_csv, datatype = 'troposphere'.
;            The default is 'troposphere'. 
;   site = Shigaraki MU Observatory.  
;          For example, iug_load_radiosonde_sgk_csv, site = 'sgk'.
;          The default is 'all', i.e., load all available observation points.
;          The default is 'all', i.e., load all available parameters.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;CODE:
;  A. Shinbori, 17/05/2013.
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

pro iug_load_radiosonde_sgk_csv, datatype = datatype, $
  site=site, $
  downloadonly=downloadonly, $
  trange=trange, $
  verbose=verbose

;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2
 
;***********************************
;Load 'troposphere' data by default:
;***********************************
if (not keyword_set(datatype)) then datatype='troposhere'

;***********
;site codes:
;***********
;--- all sites (default)
site_code_all = strsplit('sgk',' ', /extract)

;--- check site codes
if (not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)

if n_elements(site_code) eq 1 then begin
   if site_code eq '' then begin
      print, 'This station code is not valid. Please input the allowed keywords, all, and sgk.'
      return
   endif
endif
print, site_code

;======================
;Calculation of height:
;======================
height = fltarr(1200)
height[0]=350.0
for i=0, n_elements(height)-2 do begin
    height[i+1] = height[i]+30.0
endfor

;==================================================================
;Download files, read data, and create tplot vars at each component
;==================================================================
;******************************************************************
;Loop on downloading files
;******************************************************************
;Define FILE_NAMES, and load data:
;=================================

;Definition of parameter and array:
h=0L
jj=0L
kk=0L
kkk=intarr(3)

;In the case that the parameters are except for all.'
if n_elements(site_code) le 3 then begin
   h_max=n_elements(site_code)
   for i=0,n_elements(site_code)-1 do begin
      if site_code[i] eq 'sgk' then begin
         kkk[i]=0 
      endif
    endfor
  endif

for ii=0,n_elements(site_code)-1 do begin
   kk=kkk[ii]
   if ~size(fns,/type) then begin       
     ;****************************
     ;Get files for ith component:
     ;****************************     
      hour_res = 1  
      file_names = file_dailynames( $
                   file_format='YYYY/'+$
                   'YYYYMMDDhh',trange=trange,hour_res=hour_res,times=times,/unique)+'*.csv'
     ;        
     ;Define FILE_RETRIEVE structure:
     ;===============================
      source = file_retrieve(/struct)
      source.verbose=verbose
      source.local_data_dir =  root_data_dir() + 'iugonet/rish/misc/sgk/radiosonde/csv/'
      source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/radar-group/mu/sondedb/'
    
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
     ;=========================
      s=''
      sonde_time = 0
      sonde_press = 0
      sonde_temp = 0
      sonde_rh = 0
      sonde_dewp = 0
      sonde_uwind = 0
      sonde_vwind = 0
       
     ;Loop on files: 
     ;==============
      for j=jj,n_elements(local_paths)-1 do begin
         file= local_paths[j] 
         if file_test(/regular,file) then  dprint,'Loading Shigaraki sonde data file: ',file $
         else begin
            dprint,'Shigaraki sonde data file',file,'not found. Skipping'
            continue
         endelse
         
        ;Open the text file:
         openr,lun,file,/get_lun 
           
        ;Read time and header information:
         readf,lun,s
         temp_name = strsplit(s,',', /extract)
         year = fix(strmid(temp_name[0],0,4))
         month = fix(strmid(temp_name[0],5,2))
         day = fix(strmid(temp_name[0],8,2))
         hhmmss=temp_name[1] 
        
        ;Loop on read data:
        ;=================
         while(not eof(lun)) do begin
            readf,lun,s
            ok=1
            if strmid(s,0,1) eq '[' then ok=0
            if ok && keyword_set(s) then begin
               dprint,s ,dlevel=5
              
               data_comp = strsplit(s,' ', /extract)
              
              ;===================================================================   
              ;Append array of height, press., temp., rh, dewp, uwind, vwind data:
              ;===================================================================
               append_array,height_data, float(data_comp[1])
               append_array,press, float(data_comp[2])
               append_array,temp, float(data_comp[3])
               append_array,rh, float(data_comp[4])
               append_array,uwind, float(data_comp[5])
               append_array,vwind, float(data_comp[6])
               continue       
            endif
         endwhile 
         free_lun,lun ;Close the file

        ;Definition of parameters and arraies:
         h_num= 0
         press2 = fltarr(1,n_elements(height))+!values.f_nan
         temp2 = fltarr(1,n_elements(height))+!values.f_nan
         rh2 = fltarr(1,n_elements(height))+!values.f_nan
         uwind2 = fltarr(1,n_elements(height))+!values.f_nan
         vwind2 = fltarr(1,n_elements(height))+!values.f_nan
         
        ;Replace missing number by NaN
         for i=0, n_elements(height_data)-1 do begin
            a = press[i]            
            wbad = where(a eq -999.0,nbad)
            if nbad gt 0 then a[wbad] = !values.f_nan
            press[i] =a 
            b = temp[i]            
            wbad = where(b eq -999.0,nbad)
            if nbad gt 0 then b[wbad] = !values.f_nan
            temp[i] =b 
            c = rh[i]            
            wbad = where(c eq -999.0,nbad)
            if nbad gt 0 then c[wbad] = !values.f_nan
            rh[i] =c 
            d = uwind[i]            
            wbad = where(d eq -999.0,nbad)
            if nbad gt 0 then d[wbad] = !values.f_nan
            uwind[i] =d 
            e = vwind[i]            
            wbad = where(e eq -999.0,nbad)
            if nbad gt 0 then e[wbad] = !values.f_nan
            vwind[i] =e 
         endfor

        ;==== time from UT to UNIX Time
         time = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+string(hhmmss)) $
                -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/09:00:00')
        k=0
        for i=0, n_elements(height)-1 do begin
           idx = where((height_data ge height[i]-15) and (height_data lt height[i]+15),cnt)
           if idx[0] ne -1 then begin
              press2[0,i]=mean(press[idx],/NAN)
              temp2[0,i]=mean(temp[idx],/NAN)
              rh2[0,i]=mean(rh[idx],/NAN)
              uwind2[0,i]=mean(uwind[idx],/NAN)
              vwind2[0,i]=mean(vwind[idx],/NAN)
           endif
        endfor
        
        ;=====================
        ;Append time and data:
        ;=====================
         append_array, sonde_time, time
         append_array, sonde_press, press2
         append_array, sonde_temp, temp2
         append_array, sonde_rh, rh2
         append_array, sonde_dewp, dewp2
         append_array, sonde_uwind, uwind2
         append_array, sonde_vwind, vwind2
         
        ;Clear Buffer:
         time=0
         height_data=0
         press=0
         temp=0
         rh=0
         uwind=0
         vwind=0 
      endfor

      
     ;==============================
     ;Store data in TPLOT variables:
     ;==============================
     ;Acknowlegment string (use for creating tplot vars)
      acknowledgstring = 'If you acquire the dawin sonde campaine data, we ask that you' $
                       + 'acknowledge us in your use of the data. This may be done by' $
                       + 'including text such as the dawin donde campaine data provided by Research Institute' $
                       + 'for Sustainable Humanosphere of Kyoto University. We would also' $
                       + 'appreciate receiving a copy of the relevant publications. The distribution of dawin sonde data' $
                       + 'has been partly supported by the IUGONET (Inter-university Upper atmosphere Global' $
                       + 'Observation NETwork) project (http://www.iugonet.org/) funded by the' $
                       + 'Ministry of Education, Culture, Sports, Science and Technology (MEXT), Japan.'
  
      if size(sonde_press,/type) eq 4 then begin 
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'H. Hashiguchi'))

         store_data,'iug_radiosonde_'+site_code[ii]+'_press',data={x:sonde_time, y:sonde_press, v:height/1000.0},dlimit=dlimit
         options,'iug_radiosonde_'+site_code[ii]+'_press',ytitle='RSND-'+site_code[ii]+'!CHeight!C[km]',ztitle='Press.!C[hPa]'
         store_data,'iug_radiosonde_'+site_code[ii]+'_temp',data={x:sonde_time, y:sonde_temp, v:height/1000.0},dlimit=dlimit
         options,'iug_radiosonde_'+site_code[ii]+'_temp',ytitle='RSND-'+site_code[ii]+'!CHeight!C[km]',ztitle='Temp.!C[deg.]'
         store_data,'iug_radiosonde_'+site_code[ii]+'_rh',data={x:sonde_time, y:sonde_rh, v:height/1000.0},dlimit=dlimit
         options,'iug_radiosonde_'+site_code[ii]+'_rh',ytitle='RSND-'+site_code[ii]+'!CHeight!C[km]',ztitle='RH!C[%]'
         store_data,'iug_radiosonde_'+site_code[ii]+'_uwnd',data={x:sonde_time, y:sonde_uwind, v:height/1000.0},dlimit=dlimit
         options,'iug_radiosonde_'+site_code[ii]+'_uwnd',ytitle='RSND-'+site_code[ii]+'!CHeight!C[km]',ztitle='uwnd!C[m/s]'
         store_data,'iug_radiosonde_'+site_code[ii]+'_vwnd',data={x:sonde_time, y:sonde_vwind, v:height/1000.0},dlimit=dlimit
         options,'iug_radiosonde_'+site_code[ii]+'_vwnd',ytitle='RSND-'+site_code[ii]+'!CHeight!C[km]',ztitle='vwnd!C[m/s]'

        ;Add options
         options, ['iug_radiosonde_'+site_code[ii]+'_press','iug_radiosonde_'+site_code[ii]+'_temp',$
                   'iug_radiosonde_'+site_code[ii]+'_rh',$
                   'iug_radiosonde_'+site_code[ii]+'_uwnd','iug_radiosonde_'+site_code[ii]+'_vwnd'], 'spec', 1
      endif 

     ;Clear time and data buffer:
      sonde_time = 0
      sonde_press = 0
      sonde_temp = 0
      sonde_rh = 0
      sonde_dewp = 0
      sonde_uwind = 0
      sonde_vwind = 0
       
     ;Add tdegap
      new_vars=tnames('iug_radiosonde_*')
      if new_vars[0] ne '' then begin  
         tdegap, 'iug_radiosonde_'+site_code[ii]+'_press',/overwrite
         tdegap, 'iug_radiosonde_'+site_code[ii]+'_temp',/overwrite
         tdegap, 'iug_radiosonde_'+site_code[ii]+'_rh',/overwrite
         tdegap, 'iug_radiosonde_'+site_code[ii]+'_uwnd',/overwrite
         tdegap, 'iug_radiosonde_'+site_code[ii]+'_vwnd',/overwrite
      endif
   endif 
   jj=n_elements(local_paths)
endfor 

new_vars=tnames('iug_radiosonde_*')
if new_vars[0] ne '' then begin    
   print,'*****************************
   print,'Data loading is successful!!'
   print,'*****************************
endif

;**************************
;print of acknowledgement:
;**************************
print, '****************************************************************
print, 'Acknowledgement'
print, '****************************************************************
print, 'If you acquire the dawin sonde campaine data, we ask that you acknowledge'
print, 'us in your use of the data. This may be done by including text such as' 
print, 'radiosonde data provided by Research Institute for Sustainable Humanosphere' 
print, 'of Kyoto University. We would also appreciate receiving a copy of the' 
print, 'relevant publications. The distribution of dawin sonde campaine data'
print, 'has been partly supported by the IUGONET (Inter-university Upper atmosphere'
print, 'Global Observation NETwork) project (http://www.iugonet.org/) funded by the'
print, 'Ministry of Education, Culture, Sports, Science and Technology (MEXT), Japan.'

end
