;+
; Purpose: Wrapper to thm_part_getspec, to be called during replay by thm_ui_call_sequence  Modularizes
; some operations. 
; 
; Inputs:    probe,$ ;standard getspec arguments
;            dtype,$
;            trange, $
;            start_angle,$
;            suffix,$ 
;            angle, $
;            phi,$
;            theta,$
;            pitch,$
;            gyro, $
;            erange,$
;            energy,$
;            regrid, $
;            other_dim,$
;            normalize,$
;            datagap, $
;            mask_remove,$
;            method_sunpulse_clean, $
;            limit_sunpulse_clean, $
;            fillin_method, $
;            statusbar,$
;            historyWin,$
;            loadedData
; 
;
;
;Version:
; $LastChangedBy: pcruce $
; $LastChangedDate: 2009-06-12 15:48:10 -0700 (Fri, 12 Jun 2009) $
; $LastChangedRevision: 6185 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_part_getspec_replay.pro $
;-

pro thm_ui_part_getspec_replay, probe,$
                                dtype,$
                                trange, $
                                start_angle,$
                                suffix,$ 
                                angle, $
                                phi,$
                                theta,$
                                pitch,$
                                gyro, $
                                erange,$
                                energy,$
                                regrid, $
                                other_dim,$
                                normalize,$
                                datagap, $
                                mask_remove,$
                                method_sunpulse_clean, $
                                limit_sunpulse_clean, $
                                fillin_method, $
                                statusbar,$
                                historyWin,$
                                loadedData
 
  compile_opt hidden
 
  ;This craziness with the time hash is to identify new variables, so they can be added
  ;Because some variables may be replaced, we need to incorporate creation times, and
  ;this method, though somewhat opaque, is vectorized
  tn_before = tnames('*',create_time=cn_before)
 
  thm_part_getspec, probe = probe, data_type = dtype, trange = trange, $
              start_angle = start_angle, suffix = suffix, angle = angle, $
              phi = phi, theta = theta, pitch = pitch, gyro = gyro, $
              erange = erange, energy = energy, regrid = regrid, $
              other_dim = other_dim, normalize = normalize, datagap = datagap, $
              mask_remove = mask_remove, method_sunpulse_clean = method_sunpulse_clean, $
              limit_sunpulse_clean = limit_sunpulse_clean, fillin_method = fillin_method, $
              gui_statusBar=statusbar, gui_historyWin=historyWin
              
  thm_ui_cleanup_tplot,tn_before,create_time_before=cn_before,del_vars=to_delete,new_vars=new_vars
  
  if new_vars[0] ne '' then begin
    for i = 0,n_elements(new_vars)-1L do begin
      ;thm_part_getspec creates other unrelated quantities.  This ensures that only final product is loaded.
      if stregex(new_vars[i],'.*'+ '_an_eflux'+'_'+angle+'.*',/boolean,/fold_case) then begin
        if ~loadedData->add(new_vars[i]) then begin
          historywin->update,'Failed to add data: ' + new_vars[i] + ' after getspec replay'
          statusbar->update,'Failed to add data: ' + new_vars[i] + ' after getspec replay'
          return
        endif
      endif else if stregex(new_vars[i],'.*'+'_en_eflux.*',/boolean,/fold_case) then begin
       if ~loadedData->add(new_vars[i]) then begin
          historywin->update,'Failed to add data: ' + new_vars[i] + ' after getspec replay'
          statusbar->update,'Failed to add data: ' + new_vars[i] + ' after getspec replay'
          return
        endif
      endif
    endfor
   endif
   
   if to_delete[0] ne '' then begin
     store_data,to_delete,/delete
   endif
              
end