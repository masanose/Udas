;+ 
;NAME:
; thm_ui_run_calc
;
;PURPOSE:
; Function that interprets program for thm_ui_calculate
;
;CALLING SEQUENCE:
; thm_ui_run_calc,programtext,loadeddata,historywin,statusbar,fail=fail
;
;INPUT:
; programText: array of strings, text of program
; loadeddata: the loaded data object
; historywin: the historywin object
; statusbar: the statusbar object
;
;OUTPUT:
; fail=fail: set to named variable, will be 1 if an error occurs during processing.
;
;HISTORY:
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-06-11 13:47:09 -0700 (Thu, 11 Jun 2009) $
;$LastChangedRevision: 6146 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_run_calc.pro $
;
;---------------------------------------------------------------------------------


pro thm_ui_run_calc,programtext,loadeddata,historywin,statusbar,fail=fail

  compile_opt hidden,idl2
  
  pi = !DPI
  e = exp(1)
  
  fail = 0
  
  ;list of names so that we can delete any newly created names
  tn_before = tnames()
  
  for i = 0,n_elements(programtext)-1 do begin
  
    ;widget_control,state.programLabel,set_value="Calculating line: " + strtrim(string(i),2)
    
    statusBar->update,'Calculating line: ' + strtrim(string(i),2)
    historyWin->update,'Calculating line: ' + strtrim(string(i),2)
    
    if keyword_set(programtext[i]) then begin
      calc,programtext[i],gui_data_obj=loadedData,error=fail
    endif
    
    if keyword_set(fail) then begin
    
      break
    
    endif
  
  endfor
  
  ;list of names after processing
  thm_ui_cleanup_tplot,tn_before,del_vars=to_delete
  if to_delete[0] ne '' then begin
    store_data,to_delete,/delete
  endif
  
end
