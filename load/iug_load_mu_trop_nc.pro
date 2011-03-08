;+
;
;Name:
;iug_load_mu_trop_nc
;
;PURPOSE:
;  Queries the Kyoto_RISH servers for the NetCDF data of the Middle Upper (MU) atmosphere radar 
;  and loads data into tplot format.
;
;Syntax:
; iug_load_mu_trop_nc, datatype = datatype, downloadonly=downloadonly, trange=trange, verbose=verbose
;
;Keywords:
;  datatype = Observation data type. For example, iug_load_mu_trop_nc, datatype = 'troposphere'.
;            The default is 'troposphere'. 
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;Code:
;  A. Shinbori, 11/01/2010.
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

pro iug_load_mu_trop_nc, datatype = datatype, downloadonly=downloadonly, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2
 
;****************************************
;Load 'troposphere_wind' data by default:
;****************************************
if (not keyword_set(datatype)) then datatype='troposphere'


;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'If you acquire the middle and upper atmospher (MU) radar data, ' $
+ 'we ask that you acknowledge us in your use of the data. This may be done by' $
+ 'including text such as the MU data provided by Research Institute' $
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
       source.local_data_dir = root_data_dir() + 'iugonet/rish/misc/sgk/mu/troposphere/nc/'
       source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/radar-group/mu/data/data/ver01.0807/'
    
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
      
    ; Initialize data and time buffer
      ear_time=0
      ear_zon=0
      ear_mer=0
      ear_ver=0
      pwr1 = 0
      wdt1 = 0
      dpl1 = 0
      pn1 = 0
      
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
    ncdf_varget, cdfid, 'lat', lat
    ncdf_varget, cdfid, 'lon', lon
    ncdf_varget, cdfid, 'sealvl', sealvl
    ncdf_varget, cdfid, 'bmwdh', bmwdh
    ncdf_varget, cdfid, 'beam', beam
    ncdf_varget, cdfid, 'range', range
    ncdf_varget, cdfid, 'az', az
    ncdf_varget, cdfid, 'ze', ze
    ncdf_varget, cdfid, 'date', date
    ncdf_varget, cdfid, 'time', time
    ncdf_varget, cdfid, 'navet', navet
    ncdf_varget, cdfid, 'itdnum', itdnum
    ncdf_varget, cdfid, 'height_vw', height_vw
    ncdf_varget, cdfid, 'height_mwzw', height_mwzw
    ncdf_varget, cdfid, 'height', height
    ncdf_varget, cdfid, 'vwind', wwind
    ncdf_varget, cdfid, 'nvwind', nwwind
    ncdf_varget, cdfid, 'mwind', vwind
    ncdf_varget, cdfid, 'nmwind', nvwind
    ncdf_varget, cdfid, 'zwind', uwind
    ncdf_varget, cdfid, 'nzwind', nuwind
    ncdf_varget, cdfid, 'pwr', pwr
    ncdf_varget, cdfid, 'npwr', npwr
    ncdf_varget, cdfid, 'width', width
    ncdf_varget, cdfid, 'nwidth', nwidth
    ncdf_varget, cdfid, 'dpl', dpl
    ncdf_varget, cdfid, 'ndpl', ndpl
    ncdf_varget, cdfid, 'pnoise', pnoise

    ; Calculation of unix time:
    year = fix(strmid(date,4,4))
    month = fix(strmid(date,8,2))
    day = fix(strmid(date,10,2))
                           
    ; Definition of arrary names
    time2 = dblarr(n_elements(time))
    height2 = fltarr(n_elements(range))
    uwind_mu=fltarr(n_elements(time),n_elements(range))
    vwind_mu=fltarr(n_elements(time),n_elements(range))
    wwind_mu=fltarr(n_elements(time),n_elements(range))
    pwr1_mu=fltarr(n_elements(time),n_elements(range),n_elements(beam))
    wdt1_mu=fltarr(n_elements(time),n_elements(range),n_elements(beam))
    dpl1_mu=fltarr(n_elements(time),n_elements(range),n_elements(beam))
    pnoise1_mu=fltarr(n_elements(time),n_elements(beam)) 
    
    for i=0, n_elements(time)-1 do begin
    
         time2[i] = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+string(0)+':'+string(0)+':'+string(0))+double(time[i]) $
                               -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(9)+':'+string(0)+':'+string(0))
                               
        for k=0, n_elements(range)-1 do begin

            a = uwind[k,i]            
            wbad = where(a eq 10000000000,nbad)
            if nbad gt 0 then a[wbad] = !values.f_nan
            uwind[k,i] =a
            b = vwind[k,i]            
            wbad = where(b eq 10000000000,nbad)
            if nbad gt 0 then b[wbad] = !values.f_nan
            vwind[k,i] =b
            c = wwind[k,i]            
            wbad = where(c eq 10000000000,nbad)
            if nbad gt 0 then c[wbad] = !values.f_nan
            wwind[k,i] =c
                        
            uwind_mu[i,k]=uwind[k,i]
            vwind_mu[i,k]=vwind[k,i]
            wwind_mu[i,k]=wwind[k,i]
                       
            for l=0, n_elements(beam)-1 do begin           
                e = pwr[k,i,l]            
                wbad = where(e eq 10000000000,nbad)
                if nbad gt 0 then e[wbad] = !values.f_nan
                pwr[k,i,l] =e
                f = width[k,i,l]            
                wbad = where(f eq 10000000000,nbad)
                if nbad gt 0 then f[wbad] = !values.f_nan
                width[k,i,l] =f
                g = dpl[k,i,l]            
                wbad = where(g eq 10000000000,nbad)
                if nbad gt 0 then g[wbad] = !values.f_nan
                dpl[k,i,l] =g
                
                d = pnoise[i,l]            
                wbad = where(d eq 10000000000,nbad)
                if nbad gt 0 then d[wbad] = !values.f_nan
                pnoise[i,l] =d
                
                pwr1_mu[i,k,l]=pwr[k,i,l]
                wdt1_mu[i,k,l]=width[k,i,l]
                dpl1_mu[i,k,l]=dpl[k,i,l]
                pnoise1_mu[i,l]=pnoise[i,l]
             endfor 
        endfor

    endfor
    
   ; print, uwind, n_elements(uwind),n_elements(time)
   ;Append data of time and wind velocity:
   ;======================================
    append_array, mu_time, time2
    append_array, zon_wind, uwind_mu
    append_array, mer_wind, vwind_mu
    append_array, ver_wind, wwind_mu
    append_array, pwr1, pwr1_mu
    append_array, wdt1, wdt1_mu
    append_array, dpl1, dpl1_mu
    append_array, pn1, pnoise1_mu

    ncdf_close,cdfid  ; done
    
endfor

  if n_elements(mu_time) gt 1 then begin
   ; Definition of arrary names
     bname2=strarr(n_elements(beam))
     bname=strarr(n_elements(beam))
     pwr2_mu=fltarr(n_elements(mu_time),n_elements(range))
     wdt2_mu=fltarr(n_elements(mu_time),n_elements(range))
     dpl2_mu=fltarr(n_elements(mu_time),n_elements(range))
     pnoise2_mu=fltarr(n_elements(mu_time)) 
   
   ;******************************
   ;Store data in TPLOT variables:
   ;******************************
      if time2[0] ne 0 then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'H. Hashiguchi'))
         
         ;Store data of wind velocity
         store_data,'iug_mu_uwnd',data={x:mu_time, y:zon_wind, v:height_mwzw},dlimit=dlimit
         options,'iug_mu_uwnd',ytitle='MUR-trop!CHeight!C[km]',ztitle='uwnd!C[m/s]'
         options,'iug_mu_uwnd', labels='MUR-trop [km]'
         options, 'iug_mu_uwnd','spec',1
         tdegap, 'iug_mu_uwnd',/overwrite
         
         store_data,'iug_mu_vwnd',data={x:mu_time, y:mer_wind, v:height_mwzw},dlimit=dlimit
         options,'iug_mu_vwnd',ytitle='MUR-trop!CHeight!C[km]',ztitle='vwnd!C[m/s]'
         options,'iug_mu_vwnd', labels='MUR-trop [km]'
         options, 'iug_mu_vwnd','spec',1
         tdegap, 'iug_mu_vwnd',/overwrite
         
         store_data,'iug_mu_wwnd',data={x:mu_time, y:ver_wind, v:height_vw},dlimit=dlimit
         options,'iug_mu_wwnd',ytitle='MUR-trop!CHeight!C[km]',ztitle='wwnd!C[m/s]'
         options,'iug_mu_wwnd', labels='MUR-trop [km]'
         options, 'iug_mu_wwnd','spec',1
         tdegap, 'iug_mu_wwnd',/overwrite
                 
         ;Store data of echo intensity, spectral width, and niose level:
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
             store_data,'iug_mu_pwr'+bname[l],data={x:mu_time, y:pwr2_mu, v:height2},dlimit=dlimit
             options,'iug_mu_pwr'+bname[l],ytitle='MUR-trop!CHeight!C[km]',ztitle='pwr'+bname[l]+'!C[dB]'
             options,'iug_mu_pwr'+bname[l], labels='MUR-trop [km]'
             options, 'iug_mu_pwr'+bname[l],'spec',1
             tdegap, 'iug_mu_pwr'+bname[l],/overwrite
             
             for i=0, n_elements(mu_time)-1 do begin
                 for k=0, n_elements(range)-1 do begin
                     wdt2_mu[i,k]=wdt1[i,k,l]
                 endfor
             endfor
             store_data,'iug_mu_wdt'+bname[l],data={x:mu_time, y:wdt2_mu, v:height2},dlimit=dlimit
             options,'iug_mu_wdt'+bname[l],ytitle='MUR-trop!CHeight!C[km]',ztitle='wdt'+bname[l]+'!C[m/s]'
             options,'iug_mu_wdt'+bname[l], labels='MUR-trop [km]'
             options, 'iug_mu_wdt'+bname[l],'spec',1
             tdegap, 'iug_mu_wdt'+bname[l],/overwrite 
             for i=0, n_elements(mu_time)-1 do begin
                 for k=0, n_elements(range)-1 do begin
                     dpl2_mu[i,k]=dpl1[i,k,l]
                 endfor
             endfor             
             store_data,'iug_mu_dpl'+bname[l],data={x:mu_time, y:dpl2_mu, v:height2},dlimit=dlimit
             options,'iug_mu_dpl'+bname[l],ytitle='MUR-trop!CHeight!C[km]',ztitle='dpl'+bname[l]+'!C[m/s]'
             options,'iug_mu_dpl'+bname[l], labels='MUR-trop [km]'
             options, 'iug_mu_dpl'+bname[l],'spec',1
             tdegap, 'iug_mu_dpl'+bname[l],/overwrite 
             
             for i=0, n_elements(mu_time)-1 do begin
                 pnoise2_mu[i]=pn1[i,l]
             endfor
             store_data,'iug_mu_pn'+bname[l],data={x:mu_time, y:pnoise2_mu},dlimit=dlimit
             options,'iug_mu_pn'+bname[l],ytitle='MUR-trop!Cpn'+bname[l]+'!C[dB]'
             options,'iug_mu_pn'+bname[l], labels='MUR-trop [km]'
             tdegap, 'iug_mu_pn'+bname[l],/overwrite                    
         endfor    
      endif
    
   print,'**********************************************************************************
   print,'Data loading is successful!!'
   print,'**********************************************************************************
   
   endif
 endif

    ;Clear time and data buffer:
      mu_time=0
      zon_wind=0
      mer_wind=0
      ver_wind=0
      pwr1 = 0
      wdt1 = 0
      dpl1 = 0
      pn1 = 0
      
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

