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
;A. SHinbori, 12/05/2010
;A. SHinbori, 10/07/2010
;
;  �e�X�g�v���V�W���Bthm_ui_load_wind_data.pro���������B
;  �Ƃ肠�����A�������B
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
  ;=== �����ŁAload�v���V�W����
  ;====================================
  if instrument eq 'gmag_index' then begin
    if datatype eq 'Dst' then begin
      if site_or_param eq 'WDC_kyoto' then begin
         par_names='kyoto_'+parameters
         kyoto_load_dst, trange=timeRange
      endif
    endif else if datatype eq 'AE' then begin
      if site_or_param eq 'WDC_kyoto' then begin
         if (parameters eq 'ae') or (parameters eq 'al') or (parameters eq 'ao') or (parameters eq 'au')  or (parameters eq 'ax') then begin
             par_names='kyoto_'+parameters
             kyoto_load_ae, trange=timeRange, datatype=parameters
         endif
      endif
    endif else if datatype eq 'ASY' then begin
       if site_or_param eq 'WDC_kyoto' then begin
         if (parameters eq 'asy_d') or (parameters eq 'asy_h') or (parameters eq 'sym_d') or (parameters eq 'sym_h')  then begin
             par_names='kyoto_'+parameters
             kyoto_load_asy, trange=timeRange, datatype=parameters
         endif
      endif
    endif else if datatype eq 'other' then begin            
       if site_or_param eq 'Tohoku_U' then begin
          par_names=parameters
          iug_load_gmag_pc3, trange=timeRange, site='onw'
      endif
    endif
  endif else if instrument eq 'gmag_fluxgate' then begin
    if datatype eq 'magdas' then begin
      par_names='serc_magdas_' + site_or_param + '_mag'
      iug_load_gmag_serc, trange = timeRange, site = site_or_param
    endif

  endif else if instrument eq 'superdarn' then begin
    par_names='sd_' + datatype + '_' + parameters +'_0'
    erg_load_sdfit, trange=timeRange, sites=datetype    
  endif else if instrument eq 'EAR' then begin
        if datatype eq 'trop_wind' then begin
           par_names=parameters+'_'+strmid(site_or_param, 0,4)+'_ear'
           iug_load_ear_trop, datatype =datatype, parameters = par_names, trange = timeRange
        endif else if datatype eq 'iono_er_dpl' then begin
           par_names=site_or_param
           par_names2=parameters
           iug_load_ear_iono_er, datatype =datatype, site_or_param=par_names, parameters=par_names2, trange = timeRange
        endif else if datatype eq 'iono_efr_dpl' then begin
           par_names=site_or_param
           par_names2=parameters
           iug_load_ear_iono_efr, datatype =datatype, site_or_param=par_names, parameters=par_names2, trange = timeRange
        endif else if datatype eq 'iono_fr_dpl' then begin 
           par_names=site_or_param
           par_names2=parameters
           iug_load_ear_iono_fr, datatype =datatype, site_or_param=par_names, parameters=par_names2, trange = timeRange
        endif else if datatype eq 'trop_pwr' then begin
           par_names=parameters
           iug_load_ear_trop, datatype =datatype, parameters = parameters, trange = timeRange
        endif else if datatype eq 'iono_er_pwr' then begin
           par_names=site_or_param
           par_names2=parameters
           iug_load_ear_iono_er, datatype =datatype, site_or_param=par_names, parameters=par_names2, trange = timeRange
        endif else if datatype eq 'iono_efr_pwr' then begin
           par_names=site_or_param
           par_names2=parameters
           iug_load_ear_iono_efr, datatype =datatype, site_or_param=par_names, parameters=par_names2, trange = timeRange
        endif else if datatype eq 'iono_fr_pwr' then begin
           par_names=site_or_param
           par_names2=parameters
           iug_load_ear_iono_fr, datatype =datatype, site_or_param=par_names, parameters=par_names2, trange = timeRange
        endif else if datatype eq 'trop_spec_width' then begin
           par_names=parameters
           iug_load_ear_trop, datatype =datatype, parameters = parameters, trange = timeRange
        endif else if datatype eq 'iono_er_spec_width' then begin
           par_names=site_or_param
           par_names2=parameters
           iug_load_ear_iono_er, datatype =datatype, site_or_param=par_names, parameters=par_names2, trange = timeRange
        endif else if datatype eq 'iono_efr_spec_width' then begin
           par_names=site_or_param
           par_names2=parameters
           iug_load_ear_iono_efr, datatype =datatype, site_or_param=par_names, parameters=par_names2, trange = timeRange
        endif else if datatype eq 'iono_fr_spec_width' then begin
           par_names=site_or_param
           par_names2=parameters
           iug_load_ear_iono_fr, datatype =datatype, site_or_param=par_names, parameters=par_names2, trange = timeRange
        endif else if datatype eq 'iono_er_noise_lev' then begin
           par_names=site_or_param
           par_names2=parameters
           iug_load_ear_iono_er, datatype =datatype, site_or_param=par_names, parameters=par_names2, trange = timeRange
        endif else if datatype eq 'iono_efr_noise_lev' then begin
           par_names=site_or_param
           par_names2=parameters
           iug_load_ear_iono_efr, datatype =datatype, site_or_param=par_names, parameters=par_names2, trange = timeRange
        endif else if datatype eq 'iono_fr_noise_lev' then begin
           par_names=site_or_param
           par_names2=parameters 
           iug_load_ear_iono_fr, datatype =datatype, site_or_param=par_names, parameters=par_names2, trange = timeRange
        endif
  endif else if instrument eq 'MF_radar' then begin
        if site_or_param eq 'pameungpeuk' then begin
           par_names=parameters+'_wind_pam'
           iug_load_mf_pam, datatype =datatype, parameters=par_names, trange = timeRange
        endif else if site_or_param eq 'pontianak' then begin
           par_names=parameters+'_wind_pon'
           iug_load_mf_pon, datatype =datatype, parameters=par_names, trange = timeRange
        endif
  endif else if instrument eq 'meteor_radar' then begin
        if site_or_param eq 'kototabang' then begin
           par_names=parameters+'_ktb'
           iug_load_meteor_kot, datatype =datatype, parameters=par_names, trange = timeRange
        endif else if site_or_param eq 'serpong' then begin
           par_names=parameters+'_srp'
           iug_load_meteor_srp, datatype =datatype, parameters=par_names, trange = timeRange
        endif
  endif else if instrument eq 'MU' then begin
     if datatype eq 'trop_std' then begin
        if site_or_param eq 'trop_wind' then par_names=parameters+'_wind_mu'
        if site_or_param eq 'trop_pwr' then par_names='pwr_'+parameters
        if site_or_param eq 'trop_spec_width' then par_names='sw_'+parameters
           iug_load_mu_trop, datatype =site_or_param, parameters=par_names, trange = timeRange
     endif else if datatype eq 'mw_spc' then begin
           par_names=parameters+'_mw'
           iug_load_meteor_mu, datatype =datatype, parameters=par_names, trange = timeRange
     endif
  endif else if instrument eq 'BLR' then begin       
        if site_or_param eq 'kototabang' then begin
           datatype = datatype+'_kot'
           par_names='kot_'+parameters
           iug_load_blr_kot, datatype =datatype, parameters=par_names, trange = timeRange
        endif else if site_or_param eq 'shigaraki' then begin
           datatype = datatype+'_sgk'
           par_names='sgk_'+parameters
           iug_load_blr_sgk, datatype =datatype, parameters=par_names, trange = timeRange
        endif else if site_or_param eq 'serpong' then begin
           datatype = datatype+'_srp'
           par_names='srp_'+parameters
           iug_load_blr_srp, datatype =datatype, parameters=par_names, trange = timeRange
        endif
  endif
  
  ;===========================================================
  ;=== �V����load���ꂽtplot�ϐ���T����loadedData�ɓo�^�H ===
  ;=== �ȉ��́A�C�����K�v�I�I�I ==============================
  ;===========================================================
  thm_ui_cleanup_tplot,tn_before,create_time_before=cn_before,del_vars=to_delete,new_vars=new_vars

  if new_vars[0] ne '' then begin
    ;only add the requested new parameters
    new_vars = ssl_set_intersection([par_names],[new_vars])
    loaded = 1
    ;loop over loaded data
    for i = 0,n_elements(new_vars)-1 do begin
    
      ;===================================
      ;=== loadedData��tplot�ϐ���o�^ ===
      ;=== Tree�\��������Ō��܂�H ======
      ;===================================
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
