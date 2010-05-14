;+
;NAME:
;  thm_ui_load_wind_data_load_pro
;
;PURPOSE:
;  Modularized gui wind data loader
;
;
;HISTORY:
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-06-11 12:04:38 -0700 (Thu, 11 Jun 2009) $
;$LastChangedRevision: 6135 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_load_data_file/thm_ui_load_wind_data_load_pro.pro $
;
;--------------------------------------------------------------------------------
pro thm_ui_load_wind_data_load_pro,$
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

  ;select the appropriate wind load routine
  if instrument eq 'or' then begin
    par_names = 'wi_'+datatype+'_or_'+parameters
    wi_or_load,datatype=datatype,trange=timeRange,varformat='*'
  endif else if instrument eq 'mfi' then begin
    par_names = 'wi_'+datatype+'_mfi_'+parameters
    wi_mfi_load,datatype=datatype,trange=timeRange,varformat='*'
  endif else if instrument eq 'swe' then begin
    par_names = 'wi_swe_' + parameters
    wi_swe_load,datatype=datatype,trange=timeRange,varformat='*'
  endif else if instrument eq '3dp' then begin
    ;CDF_LOAD_VARS crashes for some reason if varformat is '*' for datatype ='ELPD'
    if datatype eq 'elpd' then begin
      wi_3dp_load,datatype='elpd_old',trange=timeRange,varformat='TIME FLUX ENERGY PANGLE INTEG_T EDENS TEMP QP QM QT REDF VSW MAGF'
    endif else begin
      wi_3dp_load,datatype=datatype,trange=timeRange,varformat='*'
    endelse
    par_names = 'wi_3dp_'+datatype+'_'+parameters
  
  endif
  
  thm_ui_cleanup_tplot,tn_before,create_time_before=cn_before,del_vars=to_delete,new_vars=new_vars
  
  if new_vars[0] ne '' then begin
    ;only add the requested new parameters
    new_vars = ssl_set_intersection([par_names],[new_vars])
    loaded = 1
    ;loop over loaded data
    for i = 0,n_elements(new_vars)-1 do begin
    
      if stregex(new_vars[i],'gse',/fold_case,/boolean) then begin
        coordSys = 'gse'
      endif else if stregex(new_vars[i],'gsm',/fold_case,/boolean) then begin
        coordSys = 'gsm'
      endif else if stregex(new_vars[i],'GCI',/fold_case,/boolean) then begin
        coordSys = 'gci'
      endif else begin
        coordSys = ''
      endelse
      
      result = loadedData->add(new_vars[i],mission='WIND',observatory='WIND',instrument=instrument,coordSys=coordSys)
      
      if ~result then begin
        statusBar->update,'Error loading: ' + new_vars[i]
        historyWin->update,'WIND: Error loading: ' + new_vars[i]
        return
      endif
    endfor
  endif
    
  if n_elements(to_delete) gt 0 && is_string(to_delete) then begin
    store_data,to_delete,/delete
  endif
     
  if loaded eq 1 then begin
    statusBar->update,'WIND Data Loaded Successfully'
    historyWin->update,'WIND Data Loaded Successfully'
  endif else begin
    statusBar->update,'No WIND Data Loaded.  Data may not be available during this time interval.'
    historyWin->update,'No WIND Data Loaded.  Data may not be available during this time interval.'    
  endelse

end