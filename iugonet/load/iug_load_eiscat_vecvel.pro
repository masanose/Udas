;+
;PROCEDURE: IUG_LOAD_EISCAT_VECVEL,
;  iug_load_eiscat_vecvel, site = site, trange = trange, 
;         verbose = verbose, downloadonly = downloadonly
;
;PURPOSE:
;  Loads 3D velocity data obtained with EISCAT UHF radar/ESR.
;
;KEYWORDS:
;  site  = Observatory name.  For example, serc_load_gmag_sample, site = 'tro'.
;          The default is 'all', i.e., load all available stations.
;  /verbose, if set, then output some useful info
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;EXAMPLE:
;  iug_load_eiscat_vecvel, site = 'tro_uhf'
;
; Written by: Y. Ogawa, Feb. 25, 2012 (yogawa at nipr.ac.jp)
;-

pro iug_load_eiscat_vecvel, site = site, mode=mode, pulse_code=pulse_code, $
       trange = trange, verbose = verbose, downloadonly = downloadonly, $
       no_download = no_download

;===== Keyword check =====
;----- all site codes -----;
site_code_all = strsplit('tro_uhf esr_32m', /extract)

;----- site -----;
if(not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)
if site_code[0] eq '' then return
print, 'site_code = ',site_code

;----- all modes -----;
mode_all = strsplit('mono tri0', /extract)

;----- mode -----;
if(not keyword_set(mode)) then mode='all'
md = thm_check_valid_name(mode, mode_all, /ignore_case, /include_all)
if md[0] eq '' then return
print, 'md = ', md

;----- all pulse codes -----;
pulse_code_all = strsplit('beat ipy0 arc1 stef t2pl tau1', /extract)

;----- pulse_code -----;
if(not keyword_set(pulse_code)) then pulse_code='all'
pc = thm_check_valid_name(pulse_code, pulse_code_all, /ignore_case, /include_all)
if pc[0] eq '' then return
print, 'pc = ', pc

;----- verbose -----;
if(not keyword_set(verbose)) then verbose=0

;----- downloadonly -----;
if(not keyword_set(downloadonly)) then downloadonly=0

;----- no_download -----;
if(not keyword_set(no_download)) then no_download=0

; acknowlegment string (use for creating tplot vars)
acknowledgstring = 'The vecter velocity data are calculated with procedure made by '$
  + 'Y. Ogawa (yogawa at nipr.ac.jp), so ask him if you have any questions on the '$
  + 'data. The distribution of the vecter velocity data has been partly '$
  + 'supported by the IUGONET (Inter-university Upper atmosphere Global Observation '$
  + 'NETwork) project (http://www.iugonet.org/) funded by the Ministry of Education, '$
  + 'Culture, Sports, Science and Technology (MEXT), Japan.'

;----- default limit structure -----;
dlimit=create_struct('data_att',create_struct('acknowledgment', $	
          acknowledgstring, 'PI_NAME', 'Y. Ogawa', 'LINK_TEXT', $
          'http://polaris.nipr.ac.jp/~eiscat/eiscatdata/'))


;===== Download files, read data, and create tplot vars at each site =====
ack_flg=0

for isite=0, n_elements(site_code)-1 do begin
  site1=site_code(isite)
  site1=strsplit(site1, '_', /extract)
  stn=site1(0)
  ant=site1(1)

  for imode=0, n_elements(md)-1 do begin
    md1=md(imode)

    for ipc=0, n_elements(pc)-1 do begin
      pc1=pc(ipc)

      ;----- Set parameters for file_retrieve and download data files -----;
      source = file_retrieve(/struct)
      source.verbose = verbose
      source.local_data_dir = root_data_dir() + 'iugonet/nipr/eiscat/'+stn+'/vecvel/'
      source.remote_data_dir = 'http://polaris.nipr.ac.jp/~eiscat/eiscatdata/vecvel/'
      if keyword_set(no_download) then source.no_download = 1

      pathformat= stn + '/' + ant + '/YYYY/' + $
              'YYYY-MM-DD_'+ ant + '0_' + pc1 + '_' + md1 +'.ext'
      relpathnames = file_dailynames(file_format=pathformat, $
                                 trange=trange)
      files = file_retrieve(relpathnames, _extra=source, /last_version)

      filestest=file_test(files)

      if(total(filestest) ge 1) then begin
        files=files(where(filestest eq 1))
  
        ;==================================
        ;=== Loop on reading radar data ===
        ;==================================
        ;----- Initialize data buffer -----;
        time_buf = 0
        lat_buf = 0
        lon_buf = 0
        alt_buf = 0
        Ve_buf = 0
        Vn_buf = 0
        Vu_buf = 0
        Ve_err_buf = 0
        Vn_err_buf = 0
        Vu_err_buf = 0
        time_exb_buf = 0
        exb_buf = 0

        ntime=4320 ;60*24*3
        nalt = 15

        for j=0, n_elements(files)-1 do begin
          file1 = files[j]

          if file_test(/regular, file1) then begin
            dprint,'Loading EISCAT 3D velocity data file: ', file1
            fexist = 1
          endif else begin
            dprint,'EISCAT 3D velocity data file ',file1,' not found. Skipping'
            continue
          endelse

          ;----- Open file ------;
          openr, lun, file1, /get_lun

          ;--- Read ascii data ---;
          timevec = dblarr(ntime)
          lat = replicate(!values.f_nan, ntime, nalt)
          lon = replicate(!values.f_nan, ntime, nalt)
          alt = replicate(!values.f_nan, ntime, nalt)
          Ve = replicate(!values.f_nan, ntime, nalt)
          Vn = replicate(!values.f_nan, ntime, nalt)
          Vu = replicate(!values.f_nan, ntime, nalt)
          Ve_err = replicate(!values.f_nan, ntime, nalt)
          Vn_err = replicate(!values.f_nan, ntime, nalt)
          Vu_err = replicate(!values.f_nan, ntime, nalt)
          timevec_exb = dblarr(ntime)
          exb = replicate(!values.f_nan, ntime, 3)

          data=''
          itime=-1
          itime_exb=0
          ialt=0
          old_time=-1D
          while (not EOF(lun)) do begin
            readf, lun, data
            if strmid(data,0,1) eq '%' then continue

            stryear=strmid(data,0,4)
            strmon=strmid(data,4,2)
            strday=strmid(data,6,2)
            strhour=strmid(data,9,2)
            strmin=strmid(data,11,2)
            strsec=strmid(data,13,2)
            if strhour eq '  ' then strhour='00'
            if strmin eq '  ' then strmin='00'
            new_time=time_double(stryear+'-'+strmon+'-'+strday+'/'+$
                      strhour+':'+strmin+':'+strsec)
      
            if new_time ne old_time then begin
              alt_tmp=alt(itime, ialt)
;print, 'alt_tmp = ', alt_tmp
              if alt_tmp gt 180. then begin
;print, 'itime_exb = ', itime_exb
                timevec_exb(itime_exb)=new_time
                exb(itime_exb, 0)=Ve(itime, ialt)
                exb(itime_exb, 1)=Vn(itime, ialt)
                exb(itime_exb, 2)=Vu(itime, ialt)
                itime_exb++ 
              endif

              ialt=0
              itime++
            endif else begin
              ialt++
            endelse

            timevec(itime)=new_time
            lat(itime, ialt)=float(strmid(data,15,7))
            lon(itime, ialt)=float(strmid(data,22,7))
            alt(itime, ialt)=float(strmid(data,29,7))
            Ve(itime, ialt)=float(strmid(data,36,8))
            Vn(itime, ialt)=float(strmid(data,44,8))
            Vu(itime, ialt)=float(strmid(data,52,8))
            Ve_err(itime, ialt)=float(strmid(data,60,8))
            Vn_err(itime, ialt)=float(strmid(data,68,8))
            Vu_err(itime, ialt)=float(strmid(data,76,8))

;            print, itime, ialt, Ve(itime, ialt)

            old_time=new_time
          endwhile

          timevec=timevec[0:itime-1]
          lat=lat[0:itime-1, *]
          lon=lon[0:itime-1, *]
          alt=alt[0:itime-1, *]
          Ve=Ve[0:itime-1, *]
          Vn=Vn[0:itime-1, *]
          Vu=Ve[0:itime-1, *]
          Ve_err=Ve_err[0:itime-1, *]
          Vn_err=Vn_err[0:itime-1, *]
          Vu_err=Vu_err[0:itime-1, *]
          timevec_exb=timevec_exb[0:itime_exb-1]
          exb=exb[0:itime_exb-1, *]

          ;----- Append data -----;
          append_array, time_buf, timevec
          append_array, lat_buf, lat
          append_array, lon_buf, lon
          append_array, alt_buf, alt
          append_array, Ve_buf, Ve
          append_array, Vn_buf, Vn
          append_array, Vu_buf, Vu
          append_array, Ve_err_buf, Ve_err
          append_array, Vn_err_buf, Vn_err
          append_array, Vu_err_buf, Vu_err
          append_array, time_exb_buf, timevec_exb
          append_array, exb_buf, exb

          ;----- Close file -----;
          free_lun, lun
        endfor

        ;----- Show data policy -----;
        if(ack_flg eq 0) then begin
          ack_flg=1
          print, '**************************************************************************************'
          print, 'Information about NIPR Meteor Radar data'
          print, 'PI: ', dlimit.data_att.PI_name
          print, ''
          print, 'Rules of the Road for NIPR Meteor Radar Data:'
          print, ''
          print_str_maxlet, dlimit.data_att.acknowledgment
          print, ''
          print, 'URL: ',dlimit.data_att.LINK_TEXT
          print, '**************************************************************************************'
        endif

        ;----- Create tplot variables -----;
        if(downloadonly eq 0) then begin
          ;----- tplot variable name -----;
          prefix = 'eiscat_'+stn+ant+'_vecvel_'+md1+'_'

          ;----- store data to tplot variable -----;
          vdat=alt_buf
          store_data, prefix+'lat', data={x:time_buf, y:lat_buf, $
      	      v:vdat}, dlimit=dlimit
          store_data, prefix+'lon', data={x:time_buf, y:lon_buf, $
	      v:vdat}, dlimit=dlimit
          store_data, prefix+'alt', data={x:time_buf, y:alt_buf, $
 	      v:vdat}, dlimit=dlimit
          store_data, prefix+'Ve', data={x:time_buf, y:Ve_buf, $
              v:vdat}, dlimit=dlimit
          store_data, prefix+'Vn', data={x:time_buf, y:Vn_buf, $
              v:vdat}, dlimit=dlimit
          store_data, prefix+'Vu', data={x:time_buf, y:Vu_buf, $
              v:vdat}, dlimit=dlimit
          store_data, prefix+'Veerr', data={x:time_buf, y:Ve_err_buf, $
              v:vdat}, dlimit=dlimit
          store_data, prefix+'Vnerr', data={x:time_buf, y:Vn_err_buf, $
              v:vdat}, dlimit=dlimit
          store_data, prefix+'Vuerr', data={x:time_buf, y:Vu_err_buf, $
              v:vdat}, dlimit=dlimit
          store_data, prefix+'exb', data={x:time_exb_buf, y:exb_buf, $
              v:vdat}, dlimit=dlimit

          ylim, prefix+'lat', 80, 320, 0
          ylim, prefix+'lon', 80, 320, 0
          ylim, prefix+'alt', 80, 320, 0
          ylim, prefix+'Ve', 80, 320, 0
          ylim, prefix+'Vn', 80, 320, 0
          ylim, prefix+'Vu', 80, 320, 0
          ylim, prefix+'Veerr', 80, 320, 0
          ylim, prefix+'Vnerr', 80, 320, 0
          ylim, prefix+'Vuerr', 80, 320, 0
          ylim, prefix+'exb', -1000, 1000, 0

;          zlim, prefix+'lat', 50, 80, 0
;          zlim, prefix+'lon', 0, 30, 0
;          zlim, prefix+'alt', 80, 320, 0
          zlim, prefix+'Ve', -1000, 1000, 0
          zlim, prefix+'Vn', -1000, 1000, 0
          zlim, prefix+'Vu', -1000, 1000, 0
          zlim, prefix+'Veerr', -1000, 1000, 0
          zlim, prefix+'Vnerr', -1000, 1000, 0
          zlim, prefix+'Vuerr', -1000, 1000, 0

          options, prefix+'lat', 'spec', 1
          options, prefix+'lon', 'spec', 1
          options, prefix+'alt', 'spec', 1
          options, prefix+'Ve', 'spec', 1
          options, prefix+'Vn', 'spec', 1
          options, prefix+'Vu', 'spec', 1
          options, prefix+'Veerr', 'spec', 1
          options, prefix+'Vnerr', 'spec', 1
          options, prefix+'Vuerr', 'spec', 1
          options, prefix+'exb', 'spec', 0

          ;----- add options -----;
          options, prefix+'lat', $
            ytitle = 'EISCAT radar '+strupcase(stn)+'!CHeight', $
            ysubtitle = '[km]', ztitle='Latitude [m/s]'
          options, prefix+'lon', $
            ytitle = 'EISCAT radar '+strupcase(stn)+'!CHeight', $
            ysubtitle = '[km]', ztitle='Longitude [m/s]'
          options, prefix+'alt', $
            ytitle = 'EISCAT radar '+strupcase(stn)+'!CHeight', $
            ysubtitle = '[km]', ztitle='Altitude [m/s]'

          options, prefix+'Ve', $
            ytitle = 'EISCAT radar '+strupcase(stn)+'!CHeight', $
            ysubtitle = '[km]', ztitle='Eastward Vel. [m/s]'
          options, prefix+'Vn', $
            ytitle = 'EISCAT radar '+strupcase(stn)+'!CHeight', $
            ysubtitle = '[km]', ztitle='Northward Vel. [m/s]'
          options, prefix+'Vu', $
            ytitle = 'EISCAT radar '+strupcase(stn)+'!CHeight', $
            ysubtitle = '[km]', ztitle='Upward Vel. [m/s]'
          options, prefix+'Veerr', $
            ytitle = 'EISCAT radar '+strupcase(stn)+'!CHeight', $
            ysubtitle = '[km]', ztitle='Error of Eastward Vel. [m/s]'
          options, prefix+'Vnerr', $
            ytitle = 'EISCAT radar '+strupcase(stn)+'!CHeight', $
            ysubtitle = '[km]', ztitle='Error of Northward Vel. [m/s]'
          options, prefix+'Vuerr', $
            ytitle = 'EISCAT radar '+strupcase(stn)+'!CHeight', $
            ysubtitle = '[km]', ztitle='Error of Upwardward Vel. [m/s]'
          options, prefix+'exb', $
            ytitle = 'EISCAT radar '+strupcase(stn)+'!CHeight', $
            ysubtitle = '[m/s]', labels=['Eastward Vel.', 'Northward Vel.', 'Upward Vel.']

        endif
      endif
    endfor
  endfor
endfor

end

