;+
;
;NAME:
;iug_load_ionosonde_rish
;
;PURPOSE:
;  Queries the RISH server for the ionogram data taken by the ionosonde 
;  at Shigaraki and loads data into tplot format.
;
;SYNTAX:
; iug_load_ionosonde_rish, datatype = datatype, site=site, $
;                    downloadonly=downloadonly, trange=trange, verbose=verbose
;
;KEYWOARDS:
;  datatype = Observation data type. For example, iug_load_ionosonde_rish, datatype = 'ionosphere'.
;            The default is 'ionosphere'. 
;  site = Ionosonde observation site.  
;         For example, iug_load_ionosonde_rish, site = 'sgk'.
;         The default is 'all', i.e., load all available observation points.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;CODE:
;  A. Shinbori, 24/10/2012.
;  
;MODIFICATIONS:
;  A. Shinbori, 12/11/2012.
;  
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_load_ionosonde_rish, datatype = datatype, $
  site=site, $
  downloadonly=downloadonly, $
  trange=trange, verbose=verbose

;**********************
;Verbose keyword check:
;**********************
if (not keyword_set(verbose)) then verbose=2


;***************
;Datatype check:
;***************
;--- all datatype (default)
datatype_all = strsplit('ionosphere',' ', /extract)

if (not keyword_set(datatype)) then datatype= 'ionosphere'
datatype_arr = strlowcase(thm_check_valid_name(datatype, datatype_all, /ignore_case, /include_all))

print, datatype_arr

;****************
;Site code check:
;****************
;--- all sites (default)
site_code_all = strsplit('sgk',' ', /extract)

;--- check site codes
if (not keyword_set(site)) then site='all'
site_code = strlowcase(thm_check_valid_name(site, site_code_all, /ignore_case, /include_all))

print, site_code

if datatype_arr[0] eq '' or site_code[0] eq '' then return ;No valid datatype/site

;Get the time range
get_timespan, tr
tr_str = time_string(tr)
strput, tr_str, "T", 10 ;yyyy-mm-dd/hh:mm:ss --&gt; yyyy-mm-ddThh:mm:ss
  
;=== Loop for loading data for each datatype===
if ~size(fns,/type) then begin

    ;Get files for ith component:
    ;***************************
    file_names = file_dailynames_iug( $
                 file_format='YYYY/YYYYMM/YYYYMMDD/'+$
                             'YYYYMMDDhhmm',trange=trange,times=times,/unique,/minute_res)+'_ionogram.txt'                       
    ;            
    ;Define FILE_RETRIEVE structure:
    ;===============================
    source = file_retrieve(/struct)
    source.verbose=verbose
    source.local_data_dir =  root_data_dir() + 'iugonet/rish/misc/'+site_code+'/ionosonde/text/'
    source.remote_data_dir = 'http://database.rish.kyoto-u.ac.jp/arch/mudb/ionosonde/data/text/'
    
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
      
        ;============= Read data files to generate tplot variables=============
      
        ;print, 'The following text files will be read: '
        ;print, loaded_flist ; for debugging
      
        ;Definition of parameters and array:
         s=''
         header_data = strarr(9)
         
         for j=0,n_elements(local_paths_all)-1 do begin
            file=local_paths_all[j]

            if file_test(/regular,file) then  dprint,'Loading the ionogram data taken by the ionosonde at Shigaraki:',file $
            else begin
               dprint,'The ionogram data taken by the ionosonde at Shigaraki', file,' not found. Skipping'
               continue
            endelse
       
           ;Check file line:   
            lines = file_lines(file)
            
            openr,lun,file,/get_lun    
           ;
           ;Read information of header data:
           ;================================
            for i=0, n_elements(header_data)-1 do begin
               readf, lun, s
               header_data[i]=s
            endfor      

           ;Date and hh:mm data and definition of data array:
            date = strmid(header_data[1],12,10)
            hhmm = strmid(header_data[1],23,5)
            time = time_double(string(date)+'/'+string(hhmm))-3600*9
      
           ;Read the frequency data:
            readf,lun, s
            freq = float(strsplit(s,' ',/extract))
            intensity = fltarr(1,n_elements(freq))
          
           ;Read the data:
            while(not eof(lun)) do begin  
               readf,lun, s
               data = float(strsplit(s,' ',/extract))
               height = data[0]
               intensity [0,*] =data[1:n_elements(data)-1]
         
              ;
              ;Append data of height and intensity:
              ;==========================================
               append_array, height2, height
               append_array, intensity2,intensity
               height3= height2
            endwhile
            free_lun,lun
            intensity3 = transpose(intensity2)
            intensity_all_f = fltarr(1,n_elements(freq),n_elements(height3)) 
            intensity_all_f[0,*,*] = intensity3
            print, time      
            append_array, site_time, time
            append_array, intensity_all_f2,intensity_all_f

        ;   window,1, XSIZE=900,YSIZE=600       
        ;   if j eq 0 then begin 
        ;      plotxyz,freq,height2,intensity3,xrange =[2,18],yrange =[50,700],zrange =[-90,-40],multi='3,2',window=1,/noisotropic,$
        ;              xtitle = 'Frequency [MHz]', ytitle = 'Height [km]', ztitle = 'Echo power [dBV]!C', xticks=8.0, xminor=10,charsize=0.75,$
        ;              title = 'Shigaraki Ionogram ('+date+' '+hhmm+')',/palette_reverse
        ;   endif else begin
        ;      plotxyz,freq,height2,intensity3,xrange =[2,18],yrange =[50,700],zrange =[-90,-40],/noisotropic,window=1,$
        ;              xtitle = 'Frequency [MHz]', ytitle = 'Height [km]', ztitle = 'Echo power [dBV]!C', xticks=8.0, xminor=10,charsize=0.75,$
        ;              title = 'Shigaraki Ionogram ('+date+' '+hhmm+')',/palette_reverse             
        ;   endelse
              
           ;Clear the buffer:
            height2 = 0
            intensity2 = 0
         endfor 

        ;==============================
        ;Store data in TPLOT variables:
        ;==============================
        ;Acknowlegment string (use for creating tplot vars)
         acknowledgstring = 'If you acquire the ionogram data, '+ $
                            'we ask that you acknowledge us in your use of the data. This may be done by'+ $
                            'including text such as the ionogram data provided by Research Institute'+ $
                            'for Sustainable Humanosphere of Kyoto University. We would also'+ $
                            'appreciate receiving a copy of the relevant publications. The distribution of '+ $
                            'ionogram data has been partly supported by the IUGONET (Inter-university Upper '+ $
                            'atmosphere Global Observation NETwork) project (http://www.iugonet.org/) funded '+ $
                            'by the Ministry of Education, Culture, Sports, Science and Technology (MEXT), Japan.'    
                           
         dlimit=create_struct('data_att',create_struct('acknowledgment',acknowledgstring,'PI_NAME', 'M. Yamamoto'))
         data = fltarr(n_elements(site_time),n_elements(height3))
         if size(intensity_all_f2,/type) eq 4 then begin 
            for i=0,n_elements(freq)-1 do begin 
               data[*,*] = intensity_all_f2[*,i,*]
               store_data,'iug_ionosonde_sgk_'+ strmid(STRTRIM(string(freq[i]),2),0,5),data={x:site_time,y:data,v:height3},dlimit=dlimit
               options,'iug_ionosonde_sgk_'+strmid(STRTRIM(string(freq[i]),2),0,5),ytitle = 'Ionosonde-sgk!CHeight [km]',ztitle = 'Echo power!C('+strmid(STRTRIM(string(freq[i]),2),0,5)+' MHz)!C[dBV]'
               options,'iug_ionosonde_sgk_'+strmid(STRTRIM(string(freq[i]),2),0,5),'spec',1
            endfor
         endif
      endif   

     ;Clear time and data buffer:
      site_time = 0
      aintensity_all_f2 = 0
      height3 = 0

new_vars=tnames('iug_ionosonde_sgk_*')
if new_vars[0] ne '' then begin    
   print,'*****************************
   print,'Data loading is successful!!'
   print,'*****************************
endif

;******************************
;print of acknowledgement:
;******************************
print, '****************************************************************
print, 'Acknowledgement'
print, '****************************************************************
print, 'If you acquire the ionogram data, we ask that you acknowledge us '
print, 'in your useof the data. This may be done by including text such as '
print, 'the ionogram data provided by Research Institute for Sustainable '
print, 'Humanosphere of Kyoto University. We would also appreciate receiving '
print, 'a copy of the relevant publications. The distribution of ionogram data '
print, 'has been partly supported by the IUGONET (Inter-university Upper '
print, 'atmosphere Global Observation NETwork) project (http://www.iugonet.org/) '
print, 'funded by the Ministry of Education, Culture, Sports, Science and ' 
print, 'Technology (MEXT), Japan.'
end


