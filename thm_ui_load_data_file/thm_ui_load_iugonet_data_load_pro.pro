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
;
;  �e�X�g�v���V�W���Bthm_ui_load_wind_data.pro���������B
;  �Ƃ肠�����A�������B
;--------------------------------------------------------------------------------
pro thm_ui_load_iugonet_data_load_pro,$
                         instrument,$
                         datatype,$
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
  if instrument eq 'gmag' then begin
    if datatype eq 'index' then begin
      if parameters eq 'dst' then begin
         par_names='kyoto_'+parameters
         kyoto_load_dst, trange=timeRange
      endif else if (parameters eq 'ae') or (parameters eq 'al') or (parameters eq 'ao') or (parameters eq 'au')  or (parameters eq 'ax') then begin
        par_names='kyoto_'+parameters
        kyoto_load_ae, trange=timeRange, datatype=parameters
      endif else if parameters eq 'onw_pc3' then begin
        par_names=parameters
        iug_load_gmag_pc3, trange=timeRange, site='onw'
      endif
    endif else if datatype eq 'magdas' then begin
      par_names='serc_magdas_' + parameters + '_mag'
      iug_load_gmag_serc, trange = timeRange, site = parameters
    endif

  endif else if instrument eq 'superdarn' then begin
    par_names='sd_' + datatype + '_' + parameters +'_0'
    erg_load_sdfit, trange=timeRange, sites=datetype    
  endif else if instrument eq 'EAR' then begin
        par_names=parameters
        iug_load_ear_trop, datatype =datatype, parameters = parameters, trange = timeRange
  endif else if instrument eq 'MF_radar' then begin
        par_names=parameters
        iug_load_mf, datatype =datatype, parameters=parameters, trange = timeRange
  endif else if instrument eq 'meteor_radar' then begin
        par_names=parameters
        iug_load_meteor, datatype =datatype, parameters=parameters, trange = timeRange
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
       result = loadedData->add(new_vars[i],mission='IUGONET',instrument=datatype,observatory=instrument)
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
