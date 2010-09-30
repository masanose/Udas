;+
;
;Name:
;iug_load_ear_trop_nc
;
;Purpose:
;  Queries the Kyoto_RISH servers for ACII data of the equatorial atomosphere radar (EAR) 
;  and loads data intotplot format.
;
;Syntax:
; iug_load_ear_trop_nc, datatype = datatype, parameter=parameter, $
;                        downloadonly=downloadonly, trange=trange, verbose=verbose
;
;Keywords:
;  datatype = Observation data type. For example, iug_load_ear_trop_nc, datatype = 'troposphere'.
;            The default is 'troposphere'. 
;  parameter = parameter name of EAR troposphere standard obervation data.  
;          For example, iug_load_ear_trop_nc, parameter = 'uwnd'.
;          The default is 'all', i.e., load all available parameters.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;Code:
;  A. Shinbori, 09/09/2010.
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

pro iug_load_ear_trop_nc, datatype = datatype, parameter=parameter, $
                           downloadonly=downloadonly, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2
 
;****************************************
;Load 'troposphere_wind' data by default:
;****************************************
if (not keyword_set(datatype)) then datatype='troposphere'

;***********
;parameters:
;***********
;--- all parameters (default)
parameter_all = strsplit('uwnd vwnd wwnd pwr1 pwr2 pwr3 pwr4 pwr5 wdt1 wdt2 wdt3 wdt4 wdt5' + $
                         'dpl1 dpl2 dpl3 dpl4 dpl5 pn1 pn2 pn3 pn4 pn5' ,' ', /extract)

;--- check site codes
if(not keyword_set(parameter)) then parameter='all'
parameters = thm_check_valid_name(parameter, parameter_all, /ignore_case, /include_all)

print, parameters

;*****************
;defition of unit:
;*****************
;--- all parameters (default)
unit_all = strsplit('m/s dB',' ', /extract)

;Acknowlegment string (use for creating tplot vars)
acknowledgstring = 'If you acquire EAR data, we ask that you' $
+ 'acknowledge us in your use of the data. This may be done by' $
+ 'including text such as EAR data provided by Research Institute' $
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
       source.local_data_dir = root_data_dir() + 'iugonet/rish/misc/ear/troposphere/'
       source.remote_data_dir = 'http://www.rish.kyoto-u.ac.jp/ear/data/data/ver02.0212/'
    
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

      height1 = fltarr(150)
      height2 = fltarr(150)
      height3 = fltarr(150)
      height4 = fltarr(150)
      height5 = fltarr(150)
      time2 = dblarr(144)
      
    ; Initialize data and time buffer
      ear_time=0
      ear_zon=0
      ear_mer=0
      ear_ver=0
      pwr1 = 0
      pwr2 = 0
      pwr3 = 0
      pwr4 = 0
      pwr5 = 0
      wdt1 = 0
      wdt2 = 0
      wdt3 = 0
      wdt4 = 0
      wdt5 = 0
      dpl1 = 0
      dpl2 = 0
      dpl3 = 0
      dpl4 = 0
      dpl5 = 0
      pn1 = 0
      pn2 = 0
      pn3 = 0
      pn4 = 0
      pn5 = 0
      
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
    ncdf_varget, cdfid, 'az', az
    ncdf_varget, cdfid, 'ze', ze
    ncdf_varget, cdfid, 'date', date
    ncdf_varget, cdfid, 'time', time
    ncdf_varget, cdfid, 'height_vw', height_vw
    ncdf_varget, cdfid, 'height_mwzw', height_mwzw
    ncdf_varget, cdfid, 'height', height
    ncdf_varget, cdfid, 'vwind', wwind
    ncdf_varget, cdfid, 'mwind', vwind
    ncdf_varget, cdfid, 'zwind', uwind
    ncdf_varget, cdfid, 'pwr', pwr
    ncdf_varget, cdfid, 'width', width
    ncdf_varget, cdfid, 'dpl', dpl
    ncdf_varget, cdfid, 'pnoise', pnoise
    
    ; Calculation of unix time:
    year = fix(strmid(date,4,4))
    month = fix(strmid(date,8,2))
    day = fix(strmid(date,10,2))
                           
    ; Definition of arrary names
    uwind_ear=dblarr(n_elements(time),150)
    vwind_ear=dblarr(n_elements(time),150)
    wwind_ear=dblarr(n_elements(time),150)
    pwr1_ear=dblarr(n_elements(time),150)
    pwr2_ear=dblarr(n_elements(time),150)
    pwr3_ear=dblarr(n_elements(time),150)
    pwr4_ear=dblarr(n_elements(time),150)
    pwr5_ear=dblarr(n_elements(time),150)
    wdt1_ear=dblarr(n_elements(time),150)
    wdt2_ear=dblarr(n_elements(time),150)
    wdt3_ear=dblarr(n_elements(time),150)
    wdt4_ear=dblarr(n_elements(time),150)
    wdt5_ear=dblarr(n_elements(time),150)
    dpl1_ear=dblarr(n_elements(time),150)
    dpl2_ear=dblarr(n_elements(time),150)
    dpl3_ear=dblarr(n_elements(time),150)
    dpl4_ear=dblarr(n_elements(time),150)
    dpl5_ear=dblarr(n_elements(time),150)
    pnoise1_ear=dblarr(n_elements(time))
    pnoise2_ear=dblarr(n_elements(time))
    pnoise3_ear=dblarr(n_elements(time))
    pnoise4_ear=dblarr(n_elements(time))
    pnoise5_ear=dblarr(n_elements(time))
    
    for i=0, n_elements(height_vw)-1 do begin
      height1[i] = height[i+n_elements(height_vw)*beam[0]]
      height2[i] = height[i+n_elements(height_vw)*beam[1]]
      height3[i] = height[i+n_elements(height_vw)*beam[2]]
      height4[i] = height[i+n_elements(height_vw)*beam[3]]
      height5[i] = height[i+n_elements(height_vw)*beam[4]]        
    endfor
    
    for i=0, n_elements(time)-1 do begin
    
         time2[i] = time_double(string(year)+'-'+string(month)+'-'+string(day)+'/'+string(0)+':'+string(0)+':'+string(0))+double(time[i]) $
                               -time_double(string(1970)+'-'+string(1)+'-'+string(1)+'/'+string(7)+':'+string(0)+':'+string(0))
                               
        for k=0, 149 do begin
            uwind_ear[i,k]=double(uwind[k+150*i])
            vwind_ear[i,k]=double(vwind[k+150*i])
            wwind_ear[i,k]=double(wwind[k+150*i])
            pwr1_ear[i,k]=double(pwr[k+150*i+150*beam[0]])
            pwr2_ear[i,k]=double(pwr[k+150*i+150*beam[1]])
            pwr3_ear[i,k]=double(pwr[k+150*i+150*beam[2]])
            pwr4_ear[i,k]=double(pwr[k+150*i+150*beam[3]])
            pwr5_ear[i,k]=double(pwr[k+150*i+150*beam[4]])
            wdt1_ear[i,k]=double(width[k+150*i+150*beam[0]])
            wdt2_ear[i,k]=double(width[k+150*i+150*beam[1]])
            wdt3_ear[i,k]=double(width[k+150*i+150*beam[2]])
            wdt4_ear[i,k]=double(width[k+150*i+150*beam[3]])
            wdt5_ear[i,k]=double(width[k+150*i+150*beam[4]])
            dpl1_ear[i,k]=double(dpl[k+150*i+150*beam[0]])
            dpl2_ear[i,k]=double(dpl[k+150*i+150*beam[1]])
            dpl3_ear[i,k]=double(dpl[k+150*i+150*beam[2]])
            dpl4_ear[i,k]=double(dpl[k+150*i+150*beam[3]])
            dpl5_ear[i,k]=double(dpl[k+150*i+150*beam[4]])
            
            a = uwind_ear[i,k]            
            wbad = where(a eq 10000000000,nbad)
            if nbad gt 0 then a[wbad] = !values.f_nan
            uwind_ear[i,k] =a
            b = vwind_ear[i,k]            
            wbad = where(b eq 10000000000,nbad)
            if nbad gt 0 then b[wbad] = !values.f_nan
            vwind_ear[i,k] =b
            c = wwind_ear[i,k]            
            wbad = where(c eq 10000000000,nbad)
            if nbad gt 0 then c[wbad] = !values.f_nan
            wwind_ear[i,k] =c
            d = pwr1_ear[i,k]            
            wbad = where(d eq 10000000000,nbad)
            if nbad gt 0 then d[wbad] = !values.f_nan
            pwr1_ear[i,k] =d
            e = pwr2_ear[i,k]            
            wbad = where(e eq 10000000000,nbad)
            if nbad gt 0 then e[wbad] = !values.f_nan
            pwr2_ear[i,k] =e
            f = pwr3_ear[i,k]            
            wbad = where(f eq 10000000000,nbad)
            if nbad gt 0 then f[wbad] = !values.f_nan
            pwr3_ear[i,k] =f
            g = pwr4_ear[i,k]            
            wbad = where(g eq 10000000000,nbad)
            if nbad gt 0 then g[wbad] = !values.f_nan
            pwr4_ear[i,k] =g
            h = pwr5_ear[i,k]            
            wbad = where(h eq 10000000000,nbad)
            if nbad gt 0 then h[wbad] = !values.f_nan
            pwr5_ear[i,k] =h 
            aa = wdt1_ear[i,k]            
            wbad = where(aa eq 10000000000,nbad)
            if nbad gt 0 then aa[wbad] = !values.f_nan
            wdt1_ear[i,k] =aa
            bb = wdt2_ear[i,k]            
            wbad = where(bb eq 10000000000,nbad)
            if nbad gt 0 then bb[wbad] = !values.f_nan
            wdt2_ear[i,k] =bb
            cc = wdt3_ear[i,k]            
            wbad = where(cc eq 10000000000,nbad)
            if nbad gt 0 then cc[wbad] = !values.f_nan
            wdt3_ear[i,k] =cc
            dd = wdt4_ear[i,k]            
            wbad = where(dd eq 10000000000,nbad)
            if nbad gt 0 then dd[wbad] = !values.f_nan
            wdt4_ear[i,k] =dd
            ee = wdt5_ear[i,k]            
            wbad = where(ee eq 10000000000,nbad)
            if nbad gt 0 then ee[wbad] = !values.f_nan
            wdt5_ear[i,k] =ee
            aaa = dpl1_ear[i,k]            
            wbad = where(aaa eq 10000000000,nbad)
            if nbad gt 0 then aaa[wbad] = !values.f_nan
            dpl1_ear[i,k] =aaa
            bbb = dpl2_ear[i,k]            
            wbad = where(bbb eq 10000000000,nbad)
            if nbad gt 0 then bbb[wbad] = !values.f_nan
            dpl2_ear[i,k] =bbb
            ccc = dpl3_ear[i,k]            
            wbad = where(ccc eq 10000000000,nbad)
            if nbad gt 0 then ccc[wbad] = !values.f_nan
            dpl3_ear[i,k] =ccc
            ddd = dpl4_ear[i,k]            
            wbad = where(ddd eq 10000000000,nbad)
            if nbad gt 0 then ddd[wbad] = !values.f_nan
            dpl4_ear[i,k] =ddd
            eee = dpl5_ear[i,k]            
            wbad = where(eee eq 10000000000,nbad)
            if nbad gt 0 then eee[wbad] = !values.f_nan
            dpl5_ear[i,k] =eee              
        endfor
        pnoise1_ear[i] = pnoise[i+beam[0]*n_elements(time)]
        pnoise2_ear[i] = pnoise[i+beam[1]*n_elements(time)]
        pnoise3_ear[i] = pnoise[i+beam[2]*n_elements(time)]
        pnoise4_ear[i] = pnoise[i+beam[3]*n_elements(time)]
        pnoise5_ear[i] = pnoise[i+beam[4]*n_elements(time)]
            
            a1 = pnoise1_ear[i]            
            wbad = where(a1 eq 10000000000,nbad)
            if nbad gt 0 then a1[wbad] = !values.f_nan
            pnoise1_ear[i] =a1
            a2 = pnoise2_ear[i]            
            wbad = where(a2 eq 10000000000,nbad)
            if nbad gt 0 then a2[wbad] = !values.f_nan
            pnoise2_ear[i] =a2
            a3 = pnoise3_ear[i]            
            wbad = where(a3 eq 10000000000,nbad)
            if nbad gt 0 then a3[wbad] = !values.f_nan
            pnoise3_ear[i] =a3
            a4 = pnoise4_ear[i]            
            wbad = where(a4 eq 10000000000,nbad)
            if nbad gt 0 then a4[wbad] = !values.f_nan
            pnoise4_ear[i] =a4
            a5 = pnoise5_ear[i]            
            wbad = where(a5 eq 10000000000,nbad)
            if nbad gt 0 then a5[wbad] = !values.f_nan
            pnoise5_ear[i] =a5
    endfor
    
   ; print, uwind, n_elements(uwind),n_elements(time)
   ;Append data of time and wind velocity:
   ;======================================
    append_array, ear_time, time2
    append_array, zon_wind, uwind_ear
    append_array, mer_wind, vwind_ear
    append_array, ver_wind, wwind_ear
    append_array, pwr1, pwr1_ear
    append_array, pwr2, pwr2_ear 
    append_array, pwr3, pwr3_ear
    append_array, pwr4, pwr4_ear
    append_array, pwr5, pwr5_ear
    append_array, wdt1, wdt1_ear
    append_array, wdt2, wdt2_ear 
    append_array, wdt3, wdt3_ear
    append_array, wdt4, wdt4_ear
    append_array, wdt5, wdt5_ear
    append_array, dpl1, dpl1_ear
    append_array, dpl2, dpl2_ear 
    append_array, dpl3, dpl3_ear
    append_array, dpl4, dpl4_ear
    append_array, dpl5, dpl5_ear
    append_array, pn1, pnoise1_ear
    append_array, pn2, pnoise2_ear 
    append_array, pn3, pnoise3_ear
    append_array, pn4, pnoise4_ear
    append_array, pn5, pnoise5_ear

    ncdf_close,cdfid  ; done
    
endfor
   ;******************************
   ;Store data in TPLOT variables:
   ;******************************
  
      acknowledgstring = ''
      if time2[0] ne 0 then begin
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'H. Hashiguchi'))
         ;Store data of wind velocity
         store_data,'iug_ear_uwnd',data={x:ear_time, y:zon_wind, v:height_mwzw},dlimit=dlimit
         options,'iug_ear_uwnd',ytitle='EAR-trop!CHeight!C[km]',ztitle='uwnd!C[m/s]'
         options,'iug_ear_uwnd', labels='EAR-trop [km]'
         store_data,'iug_ear_vwnd',data={x:ear_time, y:mer_wind, v:height_mwzw},dlimit=dlimit
         options,'iug_ear_vwnd',ytitle='EAR-trop!CHeight!C[km]',ztitle='vwnd!C[m/s]'
         options,'iug_ear_vwnd', labels='EAR-trop [km]'
         store_data,'iug_ear_wwnd',data={x:ear_time, y:ver_wind, v:height_vw},dlimit=dlimit
         options,'iug_ear_wwnd',ytitle='EAR-trop!CHeight!C[km]',ztitle='wwnd!C[m/s]'
         options,'iug_ear_wwnd', labels='EAR-trop [km]'
         
         ;Store data of echo intensity
         store_data,'iug_ear_pwr1',data={x:ear_time, y:pwr1, v:height1},dlimit=dlimit
         options,'iug_ear_pwr1',ytitle='EAR-trop!CHeight!C[km]',ztitle='pwr1!C[dB]'
         options,'iug_ear_pwr1', labels='EAR-trop [km]'
         store_data,'iug_ear_pwr2',data={x:ear_time, y:pwr2, v:height2},dlimit=dlimit
         options,'iug_ear_pwr2',ytitle='EAR-trop!CHeight!C[km]',ztitle='pwr2!C[dB]'
         options,'iug_ear_pwr2', labels='EAR-trop [km]'
         store_data,'iug_ear_pwr3',data={x:ear_time, y:pwr3, v:height3},dlimit=dlimit
         options,'iug_ear_pwr3',ytitle='EAR-trop!CHeight!C[km]',ztitle='pwr3!C[dB]'
         options,'iug_ear_pwr3', labels='EAR-trop [km]'
         store_data,'iug_ear_pwr4',data={x:ear_time, y:pwr4, v:height4},dlimit=dlimit
         options,'iug_ear_pwr4',ytitle='EAR-trop!CHeight!C[km]',ztitle='pwr4!C[dB]'
         options,'iug_ear_pwr4', labels='EAR-trop [km]'
         store_data,'iug_ear_pwr5',data={x:ear_time, y:pwr5, v:height5},dlimit=dlimit
         options,'iug_ear_pwr5',ytitle='EAR-trop!CHeight!C[km]',ztitle='pwr5!C[dB]'
         options,'iug_ear_pwr5', labels='EAR-trop [km]'
         
         ;Store data of spectral width
         store_data,'iug_ear_wdt1',data={x:ear_time, y:wdt1, v:height1},dlimit=dlimit
         options,'iug_ear_wdt1',ytitle='EAR-trop!CHeight!C[km]',ztitle='wdt1!C[m/s]'
         options,'iug_ear_wdt1', labels='EAR-trop [km]'
         store_data,'iug_ear_wdt2',data={x:ear_time, y:wdt2, v:height2},dlimit=dlimit
         options,'iug_ear_wdt2',ytitle='EAR-trop!CHeight!C[km]',ztitle='wdt2!C[m/s]'
         options,'iug_ear_wdt2', labels='EAR-trop [km]'
         store_data,'iug_ear_wdt3',data={x:ear_time, y:wdt3, v:height3},dlimit=dlimit
         options,'iug_ear_wdt3',ytitle='EAR-trop!CHeight!C[km]',ztitle='wdt3!C[m/s]'
         options,'iug_ear_wdt3', labels='EAR-trop [km]'
         store_data,'iug_ear_wdt4',data={x:ear_time, y:wdt4, v:height4},dlimit=dlimit
         options,'iug_ear_wdt4',ytitle='EAR-trop!CHeight!C[km]',ztitle='wdt4!C[m/s]'
         options,'iug_ear_wdt4', labels='EAR-trop [km]'
         store_data,'iug_ear_wdt5',data={x:ear_time, y:wdt5, v:height5},dlimit=dlimit
         options,'iug_ear_wdt5',ytitle='EAR-trop!CHeight!C[km]',ztitle='wdt5!C[m/s]'
         options,'iug_ear_wdt5', labels='EAR-trop [km]'
         
         ;Store data of radial Doppler velocity
         store_data,'iug_ear_dpl1',data={x:ear_time, y:dpl1, v:height1},dlimit=dlimit
         options,'iug_ear_dpl1',ytitle='EAR-trop!CHeight!C[km]',ztitle='dpl1!C[m/s]'
         options,'iug_ear_dpl1', labels='EAR-trop [km]'
         store_data,'iug_ear_dpl2',data={x:ear_time, y:dpl2, v:height2},dlimit=dlimit
         options,'iug_ear_dpl2',ytitle='EAR-trop!CHeight!C[km]',ztitle='dpl2!C[m/s]'
         options,'iug_ear_dpl2', labels='EAR-trop [km]'
         store_data,'iug_ear_dpl3',data={x:ear_time, y:dpl3, v:height3},dlimit=dlimit
         options,'iug_ear_dpl3',ytitle='EAR-trop!CHeight!C[km]',ztitle='dpl3!C[m/s]'
         options,'iug_ear_dpl3', labels='EAR-trop [km]'
         store_data,'iug_ear_dpl4',data={x:ear_time, y:dpl4, v:height4},dlimit=dlimit
         options,'iug_ear_dpl4',ytitle='EAR-trop!CHeight!C[km]',ztitle='dpl4!C[m/s]'
         options,'iug_ear_dpl4', labels='EAR-trop [km]'
         store_data,'iug_ear_dpl5',data={x:ear_time, y:dpl5, v:height5},dlimit=dlimit
         options,'iug_ear_dpl5',ytitle='EAR-trop!CHeight!C[km]',ztitle='dpl5!C[m/s]'
         options,'iug_ear_dpl5', labels='EAR-trop [km]'

         ;Store data of noise level
         store_data,'iug_ear_pn1',data={x:ear_time, y:pn1},dlimit=dlimit
         options,'iug_ear_pn1',ytitle='pn1!C[dB]'
        ; options,'iug_ear_pn1', labels='pn1!C[dB]'
         store_data,'iug_ear_pn2',data={x:ear_time, y:pn2},dlimit=dlimit
         options,'iug_ear_pn2',ytitle='pn2!C[dB]'
        ; options,'iug_ear_pn2', labels='pn2!C[dB]'
         store_data,'iug_ear_pn3',data={x:ear_time, y:pn3},dlimit=dlimit
         options,'iug_ear_pn3',ytitle='pn3!C[dB]'
        ; options,'iug_ear_pn3', labels='pn3!C[dB]'
         store_data,'iug_ear_pn4',data={x:ear_time, y:pn4},dlimit=dlimit
         options,'iug_ear_pn4',ytitle='pn4!C[dB]'
        ; options,'iug_ear_pn4', labels='pn4!C[dB]'
         store_data,'iug_ear_pn5',data={x:ear_time, y:pn5},dlimit=dlimit
         options,'iug_ear_pn5',ytitle='pn5!C[dB]'
        ; options,'iug_ear_pn5', labels='pn5!C[dB]'
                  
       ; add options
         options, ['iug_ear_uwnd','iug_ear_vwnd','iug_ear_wwnd','iug_ear_pwr1','iug_ear_pwr2',$
                   'iug_ear_pwr3','iug_ear_pwr4','iug_ear_pwr5','iug_ear_wdt1','iug_ear_wdt2',$
                   'iug_ear_wdt3','iug_ear_wdt4','iug_ear_wdt5','iug_ear_dpl1','iug_ear_dpl2',$
                   'iug_ear_dpl3','iug_ear_dpl4','iug_ear_dpl5'], 'spec', 1
      endif

    ;Clear time and data buffer:
      ear_time=0
      zon_wind=0
      mer_wind=0
      ver_wind=0
      pwr1 = 0
      pwr2 = 0
      pwr3 = 0
      pwr4 = 0
      pwr5 = 0
      wdt1 = 0
      wdt2 = 0
      wdt3 = 0
      wdt4 = 0
      wdt5 = 0
      dpl1 = 0
      dpl2 = 0
      dpl3 = 0
      dpl4 = 0
      dpl5 = 0
      pn1 = 0
      pn2 = 0
      pn3 = 0
      pn4 = 0
      pn5 = 0
    ; add tdegap
      tdegap, 'iug_ear_uwnd',/overwrite
      tdegap, 'iug_ear_vwnd',/overwrite
      tdegap, 'iug_ear_wwnd',/overwrite
      tdegap, 'iug_ear_pwr1',/overwrite
      tdegap, 'iug_ear_pwr2',/overwrite
      tdegap, 'iug_ear_pwr3',/overwrite
      tdegap, 'iug_ear_pwr4',/overwrite
      tdegap, 'iug_ear_pwr5',/overwrite
      tdegap, 'iug_ear_wdt1',/overwrite
      tdegap, 'iug_ear_wdt2',/overwrite
      tdegap, 'iug_ear_wdt3',/overwrite
      tdegap, 'iug_ear_wdt4',/overwrite
      tdegap, 'iug_ear_wdt5',/overwrite
      tdegap, 'iug_ear_dpl1',/overwrite
      tdegap, 'iug_ear_dpl2',/overwrite
      tdegap, 'iug_ear_dpl3',/overwrite
      tdegap, 'iug_ear_dpl4',/overwrite
      tdegap, 'iug_ear_dpl5',/overwrite
      tdegap, 'iug_ear_pn1',/overwrite
      tdegap, 'iug_ear_pn2',/overwrite
      tdegap, 'iug_ear_pn3',/overwrite
      tdegap, 'iug_ear_pn4',/overwrite
      tdegap, 'iug_ear_pn5',/overwrite
   endif

print,'**********************************************************************************
print,'Data loading is successful!!'
print,'**********************************************************************************

end

