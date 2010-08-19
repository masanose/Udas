;+
; :Author: daiki
;-
;+
;
;Name:
;iug_load_gmag_wdc_wdcmin
;
;-

pro iug_load_gmag_wdc_wdcmin, $
    site=site, $
    trange=trange, $
    verbose=verbose, $
    level=level, $
    addmaster=addmaster, $
    downloadonly=downloadonly, $
    no_download=no_download
    
  if ~keyword_set(verbose) then verbose=2
  if(~keyword_set(level)) then level = 'final'

  if ~keyword_set(datatype) then datatype='gmag'
  vns=['gmag']
  if size(datatype,/type) eq 7 then begin
    datatype=thm_check_valid_name(datatype,vns, $
      /ignore_case, /include_all, /no_warning)
    if datatype[0] eq '' then return
  endif else begin
    message,'DATATYPE kw must be of string type.',/info
    return
  endelse
  
  ;site = 'kak'
  ; list of sites
  vsnames = 'kak asy sym ae'
  vsnames_all = strsplit(vsnames, ' ', /extract)
  
  ; validate sites
  if(keyword_set(site)) then site_in = site else site_in = 'all'
  wdc_sites = thm_check_valid_name(site_in, vsnames_all, $
    /ignore_case, /include_all, /no_warning)
  if wdc_sites[0] eq '' then return
  
  ; number of valid sites
  nsites = n_elements(wdc_sites)
  
    
  ; bad data
  missing_value = 99999
  
  
  for i=0L, nsites-1 do begin
  
    relpathnames = $
       iug_load_gmag_wdc_relpath(sname=wdc_sites[i], $
                                 trange=trange, $
                                 res='min', level=level, $
                                 addmaster=addmaster, /unique)
    ;print,relpathnames

      
    ; define remote and local path information
    source = file_retrieve(/struct)
    source.verbose = verbose
    source.local_data_dir = root_data_dir() + 'iugonet/gmag/wdc/'
    source.remote_data_dir = 'http://localhost/~daiki/test/wdc/data/'
    if(keyword_set(no_download)) then source.no_server = 1
    
    ; download data
    local_files = file_retrieve(relpathnames, _extra=source)
    print,(local_files)
    if keyword_set(downloadonly) then continue
    
    
    ; clear data and time buffer
    elemlist = ''
    elemnum = -1
    elemlength = 0
    
    ; scan data length
    for j=0L,n_elements(local_files)-1 do begin
      file = local_files[j]
      
      if file_test(/regular,file) then begin
        dprint,'Loading data file: ', file
        fexist = 1
      endif else begin
        dprint,'Data file ',file,' not found. Skipping'
        continue
      endelse
      
      ; read data
      ; reference for WDC 1 minute record format:
      ; http://wdc.kugi.kyoto-u.ac.jp/mdplt/format/wdcformat.html
      ; reference for ASY/SYM index 1 minute record format:
      ; http://wdc.kugi.kyoto-u.ac.jp/aeasy/format/asyformat.html
      ; reference for AE index 1 minute record format:
      ; http://wdc.kugi.kyoto-u.ac.jp/aeasy/format/aeformat.html
      openr,lun,file,/get_lun
      while(not eof(lun)) do begin
        line=''
        readf,lun,line
        
        if ~ keyword_set(line) then continue
        dprint,line,dlevel=5
        
        name = strmid(line,21,3)
        if 'ae' eq strlowcase(wdc_sites[i]) then begin
           if 'AEALAOAU' ne strmid(line,0,8) then continue
        endif else begin
           if name ne strupcase(wdc_sites[i]) then continue
        endelse
        ;        year = (strmid(relpathnames[j],strlen(dir),4))
        ;        year_lower = (strmid(line,12,2))
        ;        month = (strmid(line,14,2))
        ;        day = (strmid(line,16,2))
        ;        hour = (strmid(line,19,2))
        ;
        ;        basetime = $
        ;          time_double(year+'-'+month+'-'+day)
        
        element = strmid(line,18,1)
        
        for x=0L,n_elements(elemlist)-1 do begin
          if elemlist[x] eq element then begin
            elemnum = x
            break
          endif else begin
            elemnum = -1
          endelse
        endfor
        if elemnum eq -1 then begin
          ;printdat, elemlist
          ;printdat, elemlength
          append_array, elemlist, element
          append_array, elemlength, 0L
          elemnum = n_elements(elemlist) -1
        endif
        elemlength[elemnum] += 1
        
      endwhile
      free_lun,lun
    endfor
    
    ; if nodata
    ;dprint, size(elemlist,/n_dimensions)
    if size(elemlist,/n_dimensions) eq 0 then continue
    
    ; read data
    dprint, 'Data elements: ', elemlist
    databuf = replicate(!values.f_nan,size(elemlist,/n_elements),max(elemlength)*60)
    timebuf = replicate(!values.d_nan,size(elemlist,/n_elements),max(elemlength)*60)
    bufcount = replicate(0L,size(elemlist,/n_elements))
    elemnum = -1
    ;printdat, databuf
    ;printdat, timebuf
    

    for j=0L,n_elements(local_files)-1 do begin
      file = local_files[j]
      
      if file_test(/regular,file) then begin
         ;dprint,'Scanning data file: ', file
        fexist = 1
      endif else begin
        dprint,'Data file ',file,' not found. Skipping'
        continue
      endelse
      
      ; read data
      openr,lun,file,/get_lun
      while(not eof(lun)) do begin
        line=''
        readf,lun,line
        
        if ~ keyword_set(line) then continue
        dprint,line,dlevel=5
        
        name = strmid(line,21,3)
        if 'ae' eq strlowcase(wdc_sites[i]) then begin
           if 'AEALAOAU' ne strmid(line,0,8) then continue
        endif else begin
           if name ne strupcase(wdc_sites[i]) then continue
        endelse
        year = iug_load_gmag_wdc_relpath_to_year(relpathnames[j],wdc_sites[i])
        year_lower = (strmid(line,12,2))
        month = (strmid(line,14,2))
        day = (strmid(line,16,2))
        hour = (strmid(line,19,2))
        
        if fix(strmid(year,2,2)) ne fix(year_lower) then begin
           dprint, 'invalid year value? (dir:'+year+', data:'+year_lower +')'
           continue
        endif
        basetime = time_double(year+'-'+month+'-'+day) + hour*3600d
        
        element = strmid(line,18,1)
        ;version = strmid(line,13,1) ; 0:realtime, 1:prov., 2+:final
        

        for x=0L,n_elements(elemlist)-1 do begin
          if elemlist[x] eq element then begin
            elemnum = x
            break
          endif else begin
            elemnum = -1
          endelse
        endfor
        
        basevalue = 0l ;for wdc 1-min format data
        variations = long( strmid(line, indgen(60)*6 +34 ,6) )
        
        if strlowcase(wdc_sites[i]) eq 'sym' or $
           strlowcase(wdc_sites[i]) eq 'asy' then begin
           value = float(variations)
        endif else if element eq 'D' or element eq 'I' then begin
           value = float(variations) / 600. + float(basevalue)
        endif else begin
          value = float(variations) + float(basevalue * 100)
        endelse
        wbad = where(variations eq missing_value, nbad)
        if nbad gt 0 then value[wbad] = !values.f_nan
        
        for x=0L,n_elements(value)-1 do begin
          p = bufcount[elemnum]+x
          ;dprint,bufcount[elemnum], p
          databuf[elemnum,p] = value[x]
          timebuf[elemnum,p] = basetime + x*60d
        endfor
        bufcount[elemnum] += n_elements(value)
        
      endwhile
      free_lun,lun
    endfor
    

    ; store data to tplot variables
    for elemnum=0,n_elements(elemlist)-1 do begin

       iug_load_gmag_wdc_create_tplot_vars, $
          sname = wdc_sites[i], $
          element = elemlist[elemnum], $
          res = 'min', level = level, $
          tplot_name, tplot_ytitle, tplot_ysubtitle, dlimit

       store_data,$
          tplot_name, $
          data={x:transpose(timebuf[elemnum,*]),$
                y:transpose(databuf[elemnum,*])},$
          dlimit=dlimit
       options,$
          tplot_name, $
          ytitle = tplot_ytitle, ysubtitle = tplot_ysubtitle

   endfor
    
    print, '**************************************************'
    print, dlimit.data_att.acknowledgment
    print, '**************************************************'
    
    databuf = 0
    timebuf = 0
 endfor
  
end
