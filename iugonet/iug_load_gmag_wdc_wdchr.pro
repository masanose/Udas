;+
; :Author: daiki
;-
;+
;
;Name:
;iug_load_gmag_wdc_wdchr
;
;-

pro iug_load_gmag_wdc_wdchr, $
    site=site, $
    trange=trange, $
    verbose=verbose, $
    addmaster=addmaster, $
    downloadonly=downloadonly
    
  if ~keyword_set(verbose) then verbose=2
  
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
  vsnames = 'kak dst'
  vsnames_all = strsplit(vsnames, ' ', /extract)
  
  ; validate sites
  if(keyword_set(site)) then site_in = site else site_in = 'all'
  wdc_sites = thm_check_valid_name(site_in, vsnames_all, $
    /ignore_case, /include_all, /no_warning)
  if wdc_sites[0] eq '' then return
  
  ; number of valid sites
  nsites = n_elements(wdc_sites)
  
  ; acknowlegment string (use for creating tplot vars)
  acknowledgstring_dst = $
    'The DST data are provided by the World Data Center for Geomagnetism, Kyoto, and'+ $
    ' are not for redistribution (http://wdc.kugi.kyoto-u.ac.jp/). Furthermore, we thank'+ $
    ' the geomagnetic observatories (Kakioka [JMA], Honolulu and San Juan [USGS], Hermanus'+ $
    ' [RSA], Alibag [IIG]), NiCT, INTERMAGNET, and many others for their cooperation to'+ $
    ' make the Dst index available.'
  acknowledgstring = $
    'The rules for the data use and exchange are defined'+ $
    ' by the Guide on the World Data Center System '+ $
    ' (ICSU Panel on World Data Centers, 1996).'+$
    ' Note that information on the appropriate institution(s)'+$
    ' is also supplied with the WDC data sets.'+$
    ' If the data are used in publications and presentations,'+$
    ' the data suppliers and the WDC for Geomagnetism, Kyoto'+$
    ' must properly be acknowledged.'+$
    ' Commercial use and re-distribution of WDC data are, in general, not allowed.'+$
    ' Please ask for the information of each observatory to the WDC.'
    
  ; bad data
  missing_value = 9999
  
  
  for i=0L, nsites-1 do begin
  
    relpathnames = $
       iug_load_gmag_wdc_relpath(sname=wdc_sites[i], res='hour', $
                                 trange=trange, addmaster=addmaster, /unique)

    ;print,relpathnames
      
    ; define remote and local path information
    source = file_retrieve(/struct)
    source.verbose = verbose
    source.local_data_dir = root_data_dir() + 'iugonet/gmag/wdc/'
    source.remote_data_dir = 'http://localhost/~daiki/test/wdc/data/'
    
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
        dprint,'Scanning data file: ', file
        fexist = 1
      endif else begin
        dprint,'Data file ',file,' not found. Skipping'
        continue
      endelse
      
      ; read data
      ; reference for Dst index record format:
      ; http://wdc.kugi.kyoto-u.ac.jp/dstae/format/dstformat.html
      ; reference for WDC hourly means record format:
      ; http://wdc.kugi.kyoto-u.ac.jp/hyplt/format/wdchrformat.html
      openr,lun,file,/get_lun
      while(not eof(lun)) do begin
        line=''
        readf,lun,line
        
        if ~ keyword_set(line) then continue
        dprint,line,dlevel=5
        
        name = strmid(line,0,3)
        if name ne strupcase(wdc_sites[i]) then continue
        ;        year_lower = (strmid(line,3,2))
        ;        year_upper= (strmid(line,14,2))
        ;        month = (strmid(line,5,2))
        ;        day = (strmid(line,8,2))
        ;
        ;        basetime = $
        ;          time_double(year_upper+year_lower+'-'+month+'-'+day)
        
        element = strmid(line,7,1)     ; * for index
        version = strmid(line,13,1)    ; 0:realtime, 1:prov., 2+:final
        
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
    databuf = replicate(!values.f_nan,size(elemlist,/n_elements),max(elemlength)*24)
    timebuf = replicate(!values.d_nan,size(elemlist,/n_elements),max(elemlength)*24)
    bufcount = replicate(0L,size(elemlist,/n_elements))
    elemnum = -1
    ;printdat, databuf
    ;printdat, timebuf
    
    
    
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
      ; reference for Dst index record format:
      ; http://wdc.kugi.kyoto-u.ac.jp/dstae/format/dstformat.html
      ; reference for WDC hourly means record format:
      ; http://wdc.kugi.kyoto-u.ac.jp/hyplt/format/wdchrformat.html
      openr,lun,file,/get_lun
      while(not eof(lun)) do begin
        line=''
        readf,lun,line
        
        if ~ keyword_set(line) then continue
        dprint,line,dlevel=5
        
        name = strmid(line,0,3)
        if name ne strupcase(wdc_sites[i]) then continue
        year_lower = (strmid(line,3,2))
        year_upper= (strmid(line,14,2))
        month = (strmid(line,5,2))
        day = (strmid(line,8,2))
        
        basetime = $
          time_double(year_upper+year_lower+'-'+month+'-'+day)
          
        element = strmid(line,7,1)     ; * for index
        version = strmid(line,13,1)    ; 0:realtime, 1:prov., 2+:final
        
        for x=0L,n_elements(elemlist)-1 do begin
          if elemlist[x] eq element then begin
            elemnum = x
            break
          endif else begin
            elemnum = -1
          endelse
        endfor
        
        basevalue = fix(strmid(line,16,4)) ; unit 100 nT for index
        variations = fix( strmid(line, indgen(24)*4 +20 ,4) )
        
        if element eq 'D' or element eq 'I' then begin
          value = float(variations) / 600. + basevalue
        endif else begin
          value = float(variations) + basevalue * 100
        endelse
        wbad = where(variations eq missing_value, nbad)
        if nbad gt 0 then value[wbad] = !values.f_nan
        
        for x=0L,n_elements(value)-1 do begin
          p = bufcount[elemnum]+x
          ;dprint,bufcount[elemnum], p
          databuf[elemnum,p] = value[x]
          timebuf[elemnum,p] = basetime + x*3600d
        endfor
        bufcount[elemnum] += n_elements(value)
        
      endwhile
      free_lun,lun
    endfor
    
    
    ; store data to tplot variables
    if strlowcase(wdc_sites[i]) eq 'dst' then begin
      tplot_name = 'iug_gmag_index_dst'
      site_title = 'Dst'
      dlimit=$
        create_struct('data_att', $
        create_struct('acknowledgment', acknowledgstring_dst))
    endif else begin
      tplot_name = 'iug_gmag_wdc_hr_' + strlowcase(wdc_sites[i])
      site_title = strupcase(wdc_sites[i])
      dlimit=create_struct('data_att', $
        create_struct('acknowledgment', acknowledgstring))
    endelse
    
    for elemnum=0,n_elements(elemlist)-1 do begin
      if elemlist[elemnum] eq '*' then begin   ; for index
        store_data,$
          tplot_name,$
          data={x:transpose(timebuf[elemnum,*]),$
          y:transpose(databuf[elemnum,*])},$
          dlimit=dlimit
        options, tplot_name, ytitle = site_title, ysubtitle = '[nT]'
      endif else begin
        store_data,$
          tplot_name+'_'+elemlist[elemnum],$
          data={x:transpose(timebuf[elemnum,*]),$
          y:transpose(databuf[elemnum,*])},$
          dlimit=dlimit
        if elemlist[elemnum] eq 'D' or elemlist[elemnum] eq 'I' then begin
          options,$
            tplot_name+'_'+elemlist[elemnum],$
            ytitle = site_title+' '+elemlist[elemnum],$
            ysubtitle = '[Deg]'
        endif else begin
          options,$
            tplot_name+'_'+elemlist[elemnum],$
            ytitle = site_title+' '+elemlist[elemnum],$
            ysubtitle = '[nT]'
        endelse
      endelse
    endfor
    
    print, '**************************************************'
    if strlowcase(wdc_sites[i]) eq 'dst' then begin
      print, acknowledgstring_dst
    endif else begin
      print, acknowledgstring
    endelse
    print, '**************************************************'
    
    databuf = 0
    timebuf = 0
  endfor
  
end
