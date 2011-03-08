;+
;
;Name:
;iug_load_mf_rish_pam_bin
;
;PURPOSE:
;  Queries the Kyoto_RISH servers for the binary data of the MF radar at Pameungpeuk data
;  and loads data into tplot format.
;
;Syntax:
; iug_load_mf_rish_pam_bin, site=site,downloadonly=downloadonly, trange=trange, verbose=verbose
;
;Keywords:
;   site  = Observatory code name.  For example, iug_load_mf_rish_pam_bin, site = 'pam'.
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

pro iug_load_mf_rish_pam_bin, site=site, downloadonly=downloadonly, trange=trange, verbose=verbose

;**************
;keyword check:
;**************
if ~keyword_set(verbose) then verbose=2

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
acknowledgstring = 'Scientists who want to engage in collaboration with Research Institute for Sustainable Humanosphere (RISH) ' $
+ 'should contact the principal investigator of the MF radar in Indonesia ' $
+ 'Prof. Toshitaka Tsuda, Kyoto University, who will organize such collaborations. ' $
+ 'There is a possibility that the PI of the MF radar will arrange offers so that there is less overlapping of themes between our research groups' $
+ 'Before you use the MF radar data for your papers, you must agree to the following points;' $
+ '  1. Before you submit your paper, you must contact the PI (Prof. Toshitaka Tsuda: tsuda@rish.kyoto-u.ac.jp) and discuss authorship.' $
+ '  2. When you submit your paper after doing the above item 1, you must mention the source of the data in the acknowledgment section of your paper.' $
+ '  3. In all circumstances, if anything is published you must send a hardcopy to the following address:' $
+ '    Prof. Toshitaka Tsuda' $
+ '    PI of the MF radar in Indonesia' $
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

if ~size(fns,/type) then begin

    ;Get files for ith component:
    ;***************************
    file_names = file_dailynames( $
                 file_format='YYYY/pameungpeuk.'+$
                             'YYYYMMDD',trange=trange,times=times,/unique)+'.sswma'
    ;            
    ;Define FILE_RETRIEVE structure:
    ;===============================
    source = file_retrieve(/struct)
    source.verbose=verbose
    source.local_data_dir =  root_data_dir() + 'iugonet/rish/misc/'+site_code+'/mf/binary/'
    source.remote_data_dir = 'http://database.rish.kyoto-u.ac.jp/arch/iugonet/data/mf/pameungpeuk/binary/'
    
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
   ;Read the files:
   ;===============

   height = fltarr(36)
   zon_wind_data = fltarr(1,36)
   mer_wind_data = fltarr(1,36)
   ver_wind_data = fltarr(1,36)
   time = dblarr(1)
   ear_time=0
   zon_wind=0
   mer_wind=0
   ver_wind=0

;Loop on files: 
;==============
   for j=0,n_elements(local_paths)-1 do begin
       file= local_paths[j]
       if file_test(/regular,file) then  dprint,'Loading pameungpeuk file: ',file $
       else begin
          dprint,'pameungpeuk file ',file,' not found. Skipping'
          continue
       endelse
       openr,lun,file,/get_lun 
    ;
    ;Determine year, month, day:
    ;===========================
       year = strmid(file,42,4)
       month = strmid(file,46,2)
       day = strmid(file,48,2)
    ;
    ;readdata:
    ;=========
       readf,lun
    ;
    ; read file header:
    ;==================
       file_header_bytes = assoc(lun, lonarr(1))
       file_header_names = ["File magic number (0x23110001)", $
                            "No.of SSWMA records in this file (0 or more)", $
                            "Offset to start of first record from start of file", $
                            "Unit ID or serial number"]
       sswma_records = file_header_bytes[1]
       for i = 0, 3 do begin
           print, file_header_names[i]
           print, file_header_bytes[i]
       endfor
       offset = 48L
    ;
    ; read record header:
    ;==================== 
       m=0 
        
       for k = 1L, sswma_records[0] do begin
    ;
    ; read UNIX time:
    ;================
           epoch_time_stamp = assoc(lun, lonarr(1), offset+16)
           millisecond_time_stamp = assoc(lun, lonarr(1), offset+20)
           print, epoch_time_stamp[0], millisecond_time_stamp[0]
    ;
    ; Read UNIX time:
    ;================ 
           epoch_time = float(epoch_time_stamp[0]) + float(millisecond_time_stamp[0])/1000      
           time[0]= epoch_time
    ;
    ; Read the number of range gates:
    ;================================ 
           offset += 116
           record_header_bytes3 = assoc(lun, lonarr(1), offset)
           number_of_range_gates_sampled = record_header_bytes3[0]
           offset += 132

    ; Results Header:
    ;================
           n=0
           for l = 1L, number_of_range_gates_sampled[0] do begin
               record_header_bytes10 = assoc(lun, lonarr(1), offset+0)
               height[n] =float(record_header_bytes10[0])/1000;Range [km]
               record_header_bytes12 = assoc(lun, fltarr(1), offset+8)
               a = record_header_bytes12[0];zonal_wind
               wbad = where(a gt 400 || a lt -400,nbad)
               if nbad gt 0 then a[wbad] = !values.f_nan
               zon_wind_data[m,n]=a
               record_header_bytes13 = assoc(lun, fltarr(1), offset+12)
               b= record_header_bytes13[0];meridional_wind
               wbad = where(a gt 400 || a lt -400,nbad)
               if nbad gt 0 then b[wbad] = !values.f_nan
               mer_wind_data[m,n] = b
               record_header_bytes14 = assoc(lun, fltarr(1), offset+16)
               c= record_header_bytes14[0];vertical_wind
               wbad = where(a gt 40 || a lt -40,nbad)
               if nbad gt 0 then c[wbad] = !values.f_nan
               ver_wind_data[m,n] = c
               d=height[n]
               wbad = where(d eq 0,nbad)
               if nbad gt 0 then d[wbad] = !values.f_nan
               height[n] = d
          
               offset += 144
               n=n+1
           endfor

        ;
        ;Append data of time:
        ;====================
           append_array, ear_time, time
        ;
        ;Append data of wind velocity:
        ;=============================
           append_array, zon_wind, zon_wind_data
           append_array, mer_wind, mer_wind_data
           append_array, ver_wind, ver_wind_data
      endfor
      free_lun,lun
   endfor

;******************************
;Store data in TPLOT variables:
;******************************
   if time ne 0 then begin
   
;Store data of pameungpeuk wind data:
;====================================
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
    options, 'iug_mf_'+site_code[0]+'_uwnd', labels='MFR pam'
    options, 'iug_mf_'+site_code[0]+'_vwnd', labels='MFR pam'
    options, 'iug_mf_'+site_code[0]+'_wwnd', labels='MFR pam'
   endif
   
 ; clear data and time buffer
   ear_time=0
   zon_wind=0
   mer_wind=0
   ver_wind=0
   
  ;add tclip  
   tclip, 'iug_mf_'+site_code[0]+'_uwnd',-200,200,/overwrite
   tclip, 'iug_mf_'+site_code[0]+'_vwnd',-200,200,/overwrite
   tclip, 'iug_mf_'+site_code[0]+'_wwnd',-200,200,/overwrite  
    
   ;add tdegap
   tdegap, 'iug_mf_'+site_code[0]+'_uwnd',/overwrite
   tdegap, 'iug_mf_'+site_code[0]+'_vwnd',/overwrite
   tdegap, 'iug_mf_'+site_code[0]+'_wwnd',/overwrite
   
endif

;******************************
;print of acknowledgement:
;******************************
print, '****************************************************************
print, 'Acknowledgement'
print, '****************************************************************
print, 'Scientists who want to engage in collaboration with Research Institute for Sustainable Humanosphere (RISH) ' 
print, 'should contact the principal investigator of the MF radar in Indonesia ' 
print, 'Prof. Toshitaka Tsuda, Kyoto University, who will organize such collaborations. ' 
print, 'There is a possibility that the PI of the MF radar will arrange offers so that there is less overlapping of themes between our research groups' 
print, 'Before you use the MF radar data for your papers, you must agree to the following points;' 
print, '  1. Before you submit your paper, you must contact the PI (Prof. Toshitaka Tsuda: tsuda@rish.kyoto-u.ac.jp) and discuss authorship.' 
print, '  2. When you submit your paper after doing the above item 1, you must mention the source of the data in the acknowledgment section of your paper.' 
print, '  3. In all circumstances, if anything is published you must send a hardcopy to the following address:' 
print, '    Prof. Toshitaka Tsuda' 
print, '    PI of the MF radar in Indonesia' 
print, '    Director of Research Institute for Sustainable Humanosphere,' 
print, '    Kyoto University' 
print, '    Gokasyo, Uji Kyoto 611-0011, Japan' 
print, '    e-mail: tsuda@rish.kyoto-u.ac.jp'

end
