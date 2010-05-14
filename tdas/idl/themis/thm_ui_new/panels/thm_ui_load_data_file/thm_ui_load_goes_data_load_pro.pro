;+
;NAME:
;  thm_ui_load_goes_data_load_pro
;
;PURPOSE:
;  Modularized gui goes data loader
;
;
;HISTORY:
;$LastChangedBy: pcruce $
;$LastChangedDate: 2010-01-11 10:09:36 -0800 (Mon, 11 Jan 2010) $
;$LastChangedRevision: 7084 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_load_data_file/thm_ui_load_goes_data_load_pro.pro $
;
;--------------------------------------------------------------------------------

pro thm_ui_load_goes_data_load_pro,$
                         probes,$
                         datatype,$
                         timeRange,$
                         loadedData,$
                         statusBar,$
                         historyWin
                         

  compile_opt hidden,idl2

  loaded = 0
  
  goesmintime = '2007-01-01'
  goesmaxtime = '2008-12-31'

  new_vars = ''

  tn_before = [tnames('*',create_time=cn_before)]
  

  thm_load_goesmag,$
               probe=probes,$
               datatype=datatype,$
               trange=timeRange
  
  thm_ui_cleanup_tplot,tn_before,create_time_before=cn_before,del_vars=to_delete,new_vars=new_vars
 
  if new_vars[0] ne '' then begin
    loaded = 1
    ;loop over loaded data
    for i = 0,n_elements(new_vars)-1 do begin
      
      if stregex(new_vars[i],'^g1[012]_dataqual.*',/boolean,/fold_case) || $
        stregex(new_vars[i],'^g1[012]_t[12]_counts.*',/boolean,/fold_case) then begin
        instrument = 'support'
      endif else if stregex(new_vars[i],'^g1[012]_longitude.*',/boolean,/fold_case) || $
                   stregex(new_vars[i],'^g1[012]_mlt.*',/boolean,/fold_case) || $
                   stregex(new_vars[i],'^g1[012]_pos.*',/boolean,/fold_case) || $
                   stregex(new_vars[i],'^g1[012]_vel.*',/boolean,/fold_case) then begin
        instrument = 'ephem'
      endif else begin
        instrument = 'fgm'
      endelse
      result = loadedData->add(new_vars[i],mission='GOES',instrument=instrument)
      if ~result then begin
        statusBar->update,'Error loading: ' + new_vars[i]
        historyWin->update,'GOES: Error loading: ' + new_vars[i]
        return
      endif
    endfor
  endif
    
  if to_delete[0] ne '' then begin
    store_data,to_delete,/delete
  endif
     
  if loaded eq 1 then begin
    statusBar->update,'GOES Data Loaded Successfully'
    historyWin->update,'GOES Data Loaded Successfully'
  endif else begin
  
    if time_double(goesmaxtime) lt time_double(timerange[0]) || $
       time_double(goesmintime) gt time_double(timerange[1]) then begin
       statusBar->update,'No GOES Data Loaded, GOES data is only available between ' + goesmintime + ' and ' + goesmaxtime
       historyWin->update,'No GOES Data Loaded,  GOES data is only available between ' + goesmintime + ' and ' + goesmaxtime
    endif else begin   
       statusBar->update,'No GOES Data Loaded'
       historyWin->update,'No GOES Data Loaded'
    endelse
    
  endelse

end