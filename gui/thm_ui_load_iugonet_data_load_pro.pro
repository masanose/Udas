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
;A. Shinbori, 11/10/2011
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
     iug_load_iprt, site =site_or_param, datatype=datatype, trange = timeRange
     if parameters[0] eq '*' then begin
        par_names=tnames('*')
     endif else begin
        par_names=parameters
     endelse
  endif
  
  ;load data of geomagnetic field index
  if instrument eq 'geomagnetic_field_index' then begin
    if datatype eq 'ASY_index' then begin
      if site_or_param eq 'WDC_kyoto' then begin
         par_names='wdc_mag_'+parameters
         iug_load_gmag_wdc, site=parameters, trange=timeRange        
      endif
    endif else if datatype eq 'Dst_index' or datatype eq 'AE_index' then begin
      if site_or_param eq 'WDC_kyoto' then begin
        case datatype of
          'Dst_index': vns='dst'
          'AE_index':  vns='ae'
        endcase
        if vns eq 'dst' then begin 
           if parameters[0] eq 'prov' then begin
              par_names='wdc_mag_'+vns+'_prov'
              iug_load_gmag_wdc, site=vns, trange=timeRange, level=parameters
           endif else if parameters[0] eq 'final' then begin
              par_names='wdc_mag_'+vns
              iug_load_gmag_wdc, site=vns, trange=timeRange, level=parameters
           endif else begin
              iug_load_gmag_wdc, site=vns, trange=timeRange
              par_names=tnames('wdc_mag_'+vns+'*')
           endelse
        endif
        if vns eq 'ae' then begin 
           if parameters[0] eq '*' then begin
              vns2=['min','hour','prov_min','prov_hour']
              vns4=['min','hour','min','hour']
              for i=0, n_elements(vns2)-1 do begin
                if vns2[i] eq ('min' or 'hour') then vns3='final'
                if vns2[i] eq ('prov_min' or 'prov_hour') then vns3='prov'
                iug_load_gmag_wdc, site=vns, trange=timeRange, level=vns3, resolution=vns4[i]
              endfor
              par_names=tnames('wdc_mag_'+vns+'_'+'*')             
           endif else if parameters eq 'prov_min' then begin
              par_names='wdc_mag_'+vns+'_prov_1min'
              iug_load_gmag_wdc, site=vns, trange=timeRange, level='prov', resolution='min'
           endif else if parameters eq 'prov_hour' then begin
              par_names='wdc_mag_'+vns+'_prov_1hr'
              iug_load_gmag_wdc, site=vns, trange=timeRange, level='prov', resolution='hour'
           endif else if parameters eq 'min' then begin
              par_names='wdc_mag_'+vns+'_1min'
              iug_load_gmag_wdc, site=vns, trange=timeRange, level='final', resolution='min'
           endif else if parameters eq 'hour' then begin
              par_names='wdc_mag_'+vns+'_1hr'
              iug_load_gmag_wdc, site=vns, trange=timeRange, level='final', resolution='hour'
           endif 
        endif
      endif
    endif else if datatype eq 'Pc3_index' then begin            
       if site_or_param eq 'Tohoku_U' then begin
          if datatype eq 'Pc3_index' then par_names='iug_'+parameters
          iug_load_gmag_pc3, site='onw',trange=timeRange 
      endif
    endif
  endif else if instrument eq 'geomagnetic_field_fluxgate' then begin
    if datatype eq 'magdas' then begin
      par_names='magdas_mag_' + site_or_param 
      iug_load_gmag_serc, trange = timeRange, site = site_or_param
    endif
    if datatype eq '210mm*' then begin
      vns=parameters
      if parameters[0] eq '*' then vns=['all']
      erg_load_gmag_mm210, trange = timeRange, site = site_or_param, datatype=vns 
      par_names=tnames('mm210_mag_'+site_or_param+'_'+'*'+'_hdz')
    endif
    if datatype eq 'WDC_kyoto' then begin
      vns=parameters
      if parameters[0] eq '*' then vns=['min', 'hour']
      for i=0, n_elements(vns)-1 do begin
         iug_load_gmag_wdc, trange=timeRange, site = site_or_param, resolution=vns[i]
      end
      par_names=tnames('wdc_mag_'+site_or_param+'_'+'*')
    endif
    if datatype eq 'NIPR_mag*' then begin     
      iug_load_gmag_nipr, trange=timeRange, site = site_or_param
      par_names=tnames('nipr_mag_'+site_or_param+'_'+'*')
    endif
  endif 
  
  ;load data of Equatorial Atomosphere Radar
  if instrument eq 'Equatorial_Atomosphere_Radar' then begin
     case datatype of
          'troposphere': par_names='iug_ear_'+parameters
          'e_region':  par_names='iug_ear_fai'+site_or_param+'_'+parameters
          'ef_region': par_names='iug_ear_fai'+site_or_param+'_'+parameters
          'v_region':  par_names='iug_ear_fai'+site_or_param+'_'+parameters
          'f_region':  par_names='iug_ear_fai'+site_or_param+'_'+parameters
     endcase
     iug_load_ear, datatype = datatype, parameter1 = site_or_param, parameter2 = parameters, trange = timeRange

  endif 
  
  ;load data of Medium Frequency radar
  if instrument eq 'Medium_Frequency_radar' then begin
     par_names='iug_mf_'+site_or_param+'_'+parameters
     iug_load_mf_rish, datatype = datatype, site =site_or_param, trange = timeRange 
  endif
   
  ;load data of Meteor Wind radar
  if instrument eq 'Meteor_Wind_radar' then begin
     if parameters[0] ne '*' then para=strsplit(parameters,'_',/extract)
     vns=para[1]
     if parameters[0] eq '*' then vns=['all']
     iug_load_meteor_rish, datatype =datatype, site=site_or_param, parameters = vns, trange = timeRange
     par_names=tnames('iug_meteor_'+site_or_param+'_'+'*')
  endif 
  
  ;load data of Middle Upper atomosphere radar
  if instrument eq 'Middle_Upper_atomosphere_radar' then begin
     case datatype of
          'troposphere': par_names='iug_mu_'+parameters
          'meteor_win':  par_names='iug_mu_meteor_'+parameter
     endcase
     iug_load_mu, datatype =datatype, parameter=parameters, trange = timeRange 
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
  
  ;Definition of answer
  Answer = ''

  if new_vars[0] ne '' then begin
    ;only add the requested new parameters
    new_vars = ssl_set_intersection([par_names],[new_vars])
    loaded = 1
    
    ;output of the acknowledgement message:
    Answer=gui_load_acknowledgement(datatype = datatype, par_names = par_names)

    if Answer ne 'Cancel' then begin
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
  endif 
  
  if n_elements(to_delete) gt 0 && is_string(to_delete) then begin
    store_data,to_delete,/delete
  endif
                                     
  if (loaded eq 1) and (Answer ne 'Cancel') then begin     
     statusBar->update,'IUGONET Data Loaded Successfully'
     historyWin->update,'IUGONET Data Loaded Successfully'
  endif else if (loaded eq 1) and (Answer eq 'Cancel') then begin     
     statusBar->update,'You must accept the rules of the road for IUGONET data ussage before you load and plot the data.'
     historyWin->update,'You must accept the rules of the road for IUGONET data ussage before you load and plot the data.'
  endif else begin
     statusBar->update,'No IUGONET Data Loaded.  Data may not be available during this time interval.'
     historyWin->update,'No IUGONET Data Loaded.  Data may not be available during this time interval.' 
  endelse

end
