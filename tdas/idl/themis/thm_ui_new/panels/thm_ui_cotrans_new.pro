;+ 
;NAME:
; thm_ui_cotrans_new
;
;PURPOSE:
; A performs the coordinate transformations
;
;CALLING SEQUENCE:
; thm_ui_cotrans_new, value,info
;
;INPUT:
; value:  a string storing the destination coordinate system
; active: the set of variables to be transformed
; loadedData: the loadedData object
; sobj: the status bar object to which messages should be sent
; silent(optional): set this keyword to suppress popup messages.(Used during replay)
; 
; 
;OUTPUT:
; none
; 
; SIDE EFFECT: New active variable for each prior active stored in loaded data
;   and transformed into the new coordinate system with suffix added/changed
;
;HISTORY:
;$LastChangedBy: pcruce $
;$LastChangedDate: 2010-04-08 10:51:44 -0700 (Thu, 08 Apr 2010) $
;$LastChangedRevision: 7472 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_cotrans_new.pro $
;
;---------------------------------------------------------------------------------

pro thm_ui_cotrans_new,tlb,value,active,loadedData,sobj,historywin,silent=silent

  compile_opt idl2,hidden
  
  all = loadedData->getAll(/parent) ; no children traces will hold support data so we don't bother either
  
  validcoords = ['dsl','ssl','spg','gsm','gse','gei','sm','geo']
  ;remember "Yes to all" and "No to all" decisions for state load queries
  yesall = 0
  noall = 0
   
  if ~keyword_set(active) then begin
    ;info.statusBar->update,'No active data is transformable'
    sobj->update,'No active data is transformable'
    return
  endif else begin
   
    for i = 0,n_elements(active)-1 do begin
      out_var = '' ; reset variable 
    
      tn_before = tnames('*')
    
      var = loadedData->getTvarObject(active[i])
    
      var->GetProperty,name=name,coordSys=coordSys,observatory=probe,mission=mission,timerange=timerange

      startTime = timerange->getStartTime()
      endTime = timerange->getEndTime()
      
      trange = [starttime,endtime]
      
      origname=name
      
      if strlowcase(mission) ne 'themis' then begin
;          result=error_message('No coordinate transformation support for non-THEMIS missions',$
;                               title ='Error in Cotrans: ', /noname ) 
;          sobj->update, 'No coordinate transformation support for non-THEMIS missions'
          
          probe='xxx' ; set a dummy probe so non-THEMIS data can be converted
          
;          continue              ;skip the rest of the loop
      endif
      
      if strlowcase(coordSys) eq 'n/a' then begin
        if ~keyword_set(silent) then begin
          result=error_message('Sorry. '+name+ ' does not have its coordinate system defined. Cannot perform transformation.', $
                              title ='Error in Cotrans: ', /noname, /center,traceback=0)
        endif
        sobj->update, 'Sorry. '+name+ ' does not have its coordinate system defined. Cannot perform transformation.'
        thm_ui_cleanup_tplot,tn_before,del_vars=to_delete
        if to_delete[0] ne '' then begin
          store_data,to_delete,/delete
        endif
        continue              ;skip the rest of the loop
      endif

      if strlowcase(probe) eq 'tha' then probe = 'a'
      if strlowcase(probe) eq 'thb' then probe = 'b'
      if strlowcase(probe) eq 'thc' then probe = 'c'
      if strlowcase(probe) eq 'thd' then probe = 'd'
      if strlowcase(probe) eq 'the' then probe = 'e'
      if strlowcase(probe) eq 'xxx' then probe = 'x' ; dummy probe for non-THEMIS data

      ok_probe = where(['a', 'b', 'c', 'd', 'e', 'x'] Eq probe)
      if ok_probe[0] eq -1 then begin
          if ~keyword_set(silent) then begin
            result=error_message('Sorry. No coordinate transformation support for ground-based data: '+probe,$
                               title ='Error in Cotrans: ', /noname, /center,traceback=0)
          endif
          sobj->update, 'Sorry. No coordinate transformation support for ground-based data: '+probe
          thm_ui_cleanup_tplot,tn_before,del_vars=to_delete
          if to_delete[0] ne '' then begin
            store_data,to_delete,/delete
          endif
          continue              ;skip the rest of the loop
      endif
      
      if strlowcase(coordSys) eq 'spg' || strlowcase(coordSys) eq 'dsl' || strlowcase(coordSys) eq 'ssl' || $
         strlowcase(value) eq 'spg' || strlowcase(value) eq 'dsl' || strlowcase(value) eq 'ssl' then begin

        if probe eq 'x' then begin
        ; make sure non-THEMIS data isn't converted to spg, dsl, or ssl coords
          if ~keyword_set(silent) then begin
            result=error_message('Sorry. '+name+ ' is not THEMIS data. Can not convert to SPG, DSL, or SSL coordinates.',$
                               title ='Error in Cotrans: ', /noname, /center,traceback=0)
          endif 
          sobj->update, 'Sorry. '+name+ ' is not THEMIS data. Can not convert to SPG, DSL, or SSL coordinates.'
          thm_ui_cleanup_tplot,tn_before,del_vars=to_delete
          if to_delete[0] ne '' then begin
            store_data,to_delete,/delete
          endif
          continue              ;skip the rest of the loop
        endif
      endif
      
      ;check for EFI variables and disallow coordinate transforms from SPG to anything else
      efi_vars = ['eff', 'efp', 'efw']
      instr = strmid(name, 4, 3)
      efi_test = where(instr Eq efi_vars)
      if(efi_test[0] ne -1 &&  strlowcase(coordSys) eq 'spg') then begin
        if(~keyword_set(silent)) then begin
          result = error_message('Sorry. '+name+ ' is in SPG coordinates. EFI data in SPG can not be converted to other coordinates. Please load EFI L1 data in DSL to convert.', $
                                 title = 'Error in Cotrans: ', /noname, /center, traceback = 0)
        endif 
        sobj -> update, 'Sorry. '+name+ ' is in SPG coordinates. EFI data in SPG can not be converted to other coordinates. Please load EFI L1 data in DSL to convert.'
        thm_ui_cleanup_tplot, tn_before, del_vars = to_delete
        if to_delete[0] ne '' then begin
          store_data, to_delete, /delete
        endif
        continue                ;skip the rest of the loop
      Endif

      spinras_cor = 'th'+probe+'_state_spinras_corrected'
      spindec_cor = 'th'+probe+'_state_spindec_corrected'
      spinras = 'th'+probe+'_state_spinras'
      spindec = 'th'+probe+'_state_spindec'
      
      ;determine state dependencies for variable, know that the behavior of these
      ;routines is undefined if non-themis observatories request, themis spacecraft transforms
      ;this is intentional, as it should never happen
      if thm_ui_req_spin(coordSys,value,probe,trange,loadedData) then begin
         
         message_stem = 'Required state data not loaded for THEMIS ' + strupcase(probe) + '.'
         skip_message = message_stem + ' skipping transform of ' + active[i] 
         prompt_message = message_stem + ' Would you like to load this data automatically?'
         loading_message = message_stem + ' Attempting to load state data.'
         
         result = 'yes'
         
         if ~keyword_set(yesall) && ~keyword_set(noall) && ~keyword_set(silent) then begin
           result = thm_ui_prompt_widget(tlb,sobj,historyWin,promptText=prompt_message,title="Load state data?",defaultValue="no",/yes,/no,/allyes,/allno)
           if result eq 'notoall' then begin
             noall = 1
           endif else if result eq 'yestoall' then begin
             yesall = 1
           endif
         endif
         
         if keyword_set(noall) || result eq 'no' then begin
           sobj->update,skip_message 
           historywin->update,skip_message
           thm_ui_cleanup_tplot,tn_before,del_vars=to_delete
           if to_delete[0] ne '' then begin
             store_data,to_delete,/delete
           endif
           continue
         endif
         
         thm_load_state,probe=probe,/get_support_data,trange=trange
         
         if is_string(tnames(spinras_cor)) && is_string(tnames(spindec_cor)) then begin
           sobj->update,'Loading : ' + spinras_cor + ' & ' + spindec_cor
           historywin->update,'Loading : ' + spinras_cor + ' & ' + spindec_cor
           if (~loadedData->add(spinras_cor) || ~loadedData->add(spindec_cor)) && ~keyword_set(silent) then begin
             ok = error_message('unexpected error adding data',traceback=0,/center,title='Error in Cotrans New')
           endif
         endif else if is_string(tnames(spinras)) && is_string(tnames(spindec)) then begin
           sobj->update,'Corrected variable not found, loading : ' + spinras + ' & ' + spindec
           historywin->update,'Corrected variable not found, loading : ' + spinras + ' & ' + spindec
           if (~loadedData->add(spinras) || ~loadedData->add(spindec)) && ~keyword_set(silent) then begin
             ok = error_message('unexpected error adding data',traceback=0,/center,title='Error in Cotrans New')
           endif
         endif
               
         if thm_ui_req_spin(coordSys,value,probe,trange,loadedData) then begin
            
            thm_ui_cleanup_tplot,tn_before,del_vars=to_delete
            if to_delete[0] ne '' then begin
              store_data,to_delete,/delete
            endif
            fail_message = "Failed to auto-load state data for THEMIS " + strlowcase(probe) + " to transform " + active[i] + ". Skipping."
            sobj->update,fail_message
            historywin->update,fail_message
            ok = error_message(fail_message,traceback=0,/center,title='Error in Cotrans New')
            continue
          endif
        
      endif
        
      out_suffix = '_'+strlowcase(value)
      in_suffix = ''
      
      ;info.statusBar->update,'Coordinate Transforming: ' + name
      sobj->update,String('Coordinate Transforming: ' + name)
      
      for j = 0,n_elements(validCoords)-1 do begin
      
        if (pos = stregex(name,'_'+validCoords[j]+'$',/fold_case)) ne -1 then begin
          in_suffix = '_'+ validCoords[j]
          name = strmid(name,0,pos)
          break
        endif
      endfor
      
      catch,err
      
      if err ne 0 then begin
      
        catch,/cancel
        if ~keyword_set(silent) then begin
          ok = error_message('caught cotrans error',/traceback,/center,title='Error in Cotrans New')
        endif
        thm_ui_cleanup_tplot,tn_before,del_vars=to_delete
        if to_delete[0] ne '' then begin
          store_data,to_delete,/delete
        endif
        return
        
      endif else begin
      
        thm_cotrans,name,in_suffix=in_suffix,out_suffix=out_suffix,out_vars=out_var,probe=probe,in_coord=coordSys,out_coord=value
      
      endelse
      
      catch,/cancel
      
      if ~keyword_set(out_var) then begin
        ;info.statusbar->update,'Data not transformed: '+name
        sobj->update,String('Data not transformed: '+name)
        thm_ui_cleanup_tplot,tn_before,del_vars=to_delete
        if to_delete[0] ne '' then begin
          store_data,to_delete,/delete
        endif
        continue
      endif else begin
        ;info.statusbar->update,'Successfully transformed variable to: ' + out_var[0]
        sobj->update,String('Successfully transformed variable to: ' + out_var[0])
      endelse
      
      out = var->copy()
      out->setProperty,coordSys = value,name=out_var
      
      if ~loadedData->addTvarObject(out) && ~keyword_set(silent) then begin
        ok = error_message('error adding data',traceback=0,/center,title='Error in Cotrans New')
      endif
      
      ;delete the left over variables from normal memory(they'll still be stored as traces within loaded data)
   ;   store_data,spinras,/delete
   ;   store_data,spindec,/delete
     
      loadedData->clearActive,origname
      loadedData->setActive,out_var
      
      thm_ui_cleanup_tplot,tn_before,del_vars=to_delete
      if to_delete[0] ne '' then begin
        store_data,to_delete,/delete
      endif
      
    endfor
       
  endelse
 
end
