;+
;NAME:
;  thm_ui_load_ace_data_load_pro
;
;PURPOSE:
;  Modularized gui ace data loader
;
;
;HISTORY:
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-06-11 12:04:38 -0700 (Thu, 11 Jun 2009) $
;$LastChangedRevision: 6135 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_load_data_file/thm_ui_load_ace_data_load_pro.pro $
;
;--------------------------------------------------------------------------------


pro thm_ui_load_ace_data_load_pro,$
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
;  tn_before_time_hash = [tn_before + time_string(double(cn_before),/msec)]

  par_names = 'ace_'+datatype+'_'+instrument+'_'+parameters

  ;select the appropriate ace load routine
  if instrument eq 'mfi' then begin
    ace_mfi_load,datatype=datatype,trange=timeRange,varformat='*'
  endif else if instrument eq 'swe' then begin
    ace_swe_load,datatype=datatype,trange=timeRange
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
      endif else if stregex(new_vars[i],'rtn',/fold_case,/boolean) then begin
        coordSys = 'rtn'
      endif else begin 
        coordSys = ''
      endelse
      
      result = loadedData->add(new_vars[i],mission='ACE',observatory='ACE',instrument=instrument,coordSys=coordSys)
      
      if ~result then begin
        statusBar->update,'Error loading: ' + new_vars[i]
        historyWin->update,'ACE: Error loading: ' + new_vars[i]
        return
      endif
    endfor
  endif
    
  if to_delete[0] ne '' then begin
    store_data,to_delete,/delete
  endif
     
  if loaded eq 1 then begin
    statusBar->update,'ACE Data Loaded Successfully'
    historyWin->update,'ACE Data Loaded Successfully'
  endif else begin
    statusBar->update,'No ACE Data Loaded.  Data may not be available during this time interval.'
    historyWin->update,'No ACE Data Loaded.  Data may not be available during this time interval.'    
  endelse

end
