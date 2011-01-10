;+
;NAME:
;  thm_ui_load_iugonet_data_load_pro
;
;PURPOSE:
;  Modularized gui iugonet data loader
;
;HISTORY:
;$LastChangedBy: Y.Tanaka $
;$LastChangedDate: 2010-04-20 $
; 
;Modifications:
;A. Shinbori, 12/05/2010
;A. Shinbori, 10/07/2010
;A. Shinbori, 25/11/2010
;A. Shinbori, 10/01/2011
;
;--------------------------------------------------------------------------------
pro thm_ui_load_iugonet_data_load_pro,$
                         instrument,$
                         datatype,$
                         site_or_param,$
                         parameters,$
                         timeRange,$
                         loadedData,$
                         statusBar,$
                         historyWin
                         

  compile_opt hidden,idl2

  loaded = 0

  new_vars = ''

  tn_before = [tnames('*',create_time=cn_before)]
  
  ;====================================
  ;=== Load data of the IUGONET data
  ;====================================
  ;load data of Iitate Planetary Radio Telescope
  if instrument eq 'Iitate_Planetary_Radio_Telescope' then begin       
     par_names=parameters
     iug_load_iprt, site =site_or_param, datatype=datatype, trange = timeRange
  endif
  
  ;load data of geomagnetic field index
  if instrument eq 'geomagnetic_field_index' then begin
    if datatype eq 'ASY_index' then begin
      if site_or_param eq 'WDC_kyoto' then begin
         par_names='iug_'+parameters
         iug_load_gmag_wdc, site=parameters, trange=timeRange        
      endif
    endif else if datatype eq 'Dst_index' or datatype eq 'AE_index' then begin
      if site_or_param eq 'WDC_kyoto' then begin
        case datatype of
          'Dst_index': vns='dst'
          'AE_index':  vns='ae'
        endcase
        if parameters[0] eq 'prov' then begin
          par_names='iug_'+vns+'_prov'
          iug_load_gmag_wdc, site=vns, trange=timeRange, level=parameters
        endif else if parameters[0] eq 'final' then begin
          par_names='iug_'+vns
          iug_load_gmag_wdc, site=vns, trange=timeRange, level=parameters
        endif else begin
          iug_load_gmag_wdc, site=vns, trange=timeRange
          par_names=tnames('iug_'+vns+'*')
        endelse
      endif
    endif else if datatype eq 'Pc3_index' then begin            
       if site_or_param eq 'Tohoku_U' then begin
          iug_load_gmag_pc3, site='onw',trange=timeRange 
          if datatype eq 'Pc3_index' then par_names='iug_'+parameters
      endif
    endif
  endif else if instrument eq 'geomagnetic_field_fluxgate' then begin
    if datatype eq 'magdas' then begin
      par_names='magdas_mag_' + site_or_param 
      iug_load_gmag_serc, trange = timeRange, site = site_or_param
    endif
    if datatype eq '210mm*' then begin
      vns=parameters
      if parameters[0] eq '*' then vns=['1min']
      for i=0, n_elements(vns)-1 do begin
          par_names='mm210_mag_' + site_or_param+'_'+vns[i]+'_hdz'  
          erg_load_gmag_mm210, trange = timeRange, site = site_or_param, datatype=vns[i]
      endfor
    endif
    if datatype eq 'WDC_kyoto' then begin
      par_names='iug_mag_'+site_or_param
      iug_load_gmag_wdc, trange=timeRange, site = site_or_param
    endif
    if datatype eq 'NIPR_mag*' then begin
      par_names='iug_mag_'+site_or_param
      iug_load_gmag_nipr, trange=timeRange, site = site_or_param
    endif
  endif 
  
  ;load data of Equatorial Atomosphere Radar
  if instrument eq 'Equatorial_Atomosphere_Radar' then begin
     iug_load_ear, datatype = datatype, parameter1 = site_or_param, parameter2 = parameters, trange = timeRange
     if datatype eq 'troposphere' then par_names='iug_ear_'+parameters
     if datatype eq 'e_region'  then par_names='iug_ear_fai'+site_or_param+'_'+parameters
     if datatype eq 'ef_region'  then par_names='iug_ear_fai'+site_or_param+'_'+parameters
     if datatype eq 'v_region' then par_names='iug_ear_fai'+site_or_param+'_'+parameters
     if datatype eq 'f_region'  then par_names='iug_ear_fai'+site_or_param+'_'+parameters
  endif 
  
  ;load data of Medium Frequency radar
  if instrument eq 'Medium_Frequency_radar' then begin
     iug_load_mf_rish, datatype = datatype, site =site_or_param, trange = timeRange
     if site_or_param eq 'pam' or 'pon' then par_names='iug_mf_'+site_or_param+'_'+parameters 
  endif
   
  ;load data of Meteor Wind radar
  if instrument eq 'Meteor_Wind_radar' then begin
     par_names='iug_meteor_'+site_or_param+'_'+parameters
     iug_load_meteor_rish_nc, datatype =datatype, site=site_or_param, trange = timeRange
  endif 
  
  ;load data of Middle Upper atomosphere radar
  if instrument eq 'Middle_Upper_atomosphere_radar' then begin
     iug_load_mu, datatype =datatype, parameter=parameters, trange = timeRange
     if datatype eq 'troposphere' then par_names='iug_mu_'+parameters
     if datatype eq 'meteor_wind' then par_names='iug_mu_meteor_'+parameters     
  endif
  
  ;load data of Bandary Layer Radar
  if instrument eq 'Boundary_Layer_Radar' then begin       
     par_names='iug_blr_'+site_or_param+'_'+parameters
     iug_load_blr_rish_txt, site =site_or_param, parameter=parameters, trange = timeRange
  endif
  
  ;load data of Radio sonde 
  if instrument eq 'Radio_sonde' then begin
     par_names='iug_radiosonde_'+site_or_param+'_'+parameters
     iug_load_radiosonde_rish_dawex_nc, datatype = datatype, site =site_or_param, trange = timeRange
     if site_or_param eq 'sgk' then begin      
        iug_load_radiosonde_rish_sgk_txt, datatype = datatype, site =site_or_param, trange = timeRange
     endif
  endif
  

  thm_ui_cleanup_tplot,tn_before,create_time_before=cn_before,del_vars=to_delete,new_vars=new_vars

  if new_vars[0] ne '' then begin
    ;only add the requested new parameters
    new_vars = ssl_set_intersection([par_names],[new_vars])
    loaded = 1
    ;loop over loaded data
    for i = 0,n_elements(new_vars)-1 do begin
    
      ;result = loadedData->add(new_vars[i],mission='IUGONET',instrument=instrument,observatory=datatype)
      ;===  instrument=instrumenty->instrument=datatype, observatory=datatype->observatory=instrument  ===
       result = loadedData->add(new_vars[i],mission='IUGONET',instrument=site_or_param,observatory=instrument)
      if ~result then begin
        statusBar->update,'Error loading: ' + new_vars[i]
        historyWin->update,'IUGONET: Error loading: ' + new_vars[i]
        return
      endif
    endfor
  endif
    
  if n_elements(to_delete) gt 0 && is_string(to_delete) then begin
    store_data,to_delete,/delete
  endif
     
  if loaded eq 1 then begin
     statusBar->update,'IUGONET Data Loaded Successfully'
     historyWin->update,'IUGONET Data Loaded Successfully'
  endif else begin
     statusBar->update,'No IUGONET Data Loaded.  Data may not be available during this time interval.'
     historyWin->update,'No IUGONET Data Loaded.  Data may not be available during this time interval.' 
  endelse   
end
