;+
;
;  Name: THM_UI_SCROLLF
;  
;  Purpose: Scrolls the current window forwards
;  
;  Inputs: The info structure from the main gui
;
;
;$LastChangedBy: aaflores $
;$LastChangedDate: 2009-07-13 18:18:58 -0700 (Mon, 13 Jul 2009) $
;$LastChangedRevision: 6427 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_main_funcs/thm_ui_scrollf.pro $
;-

pro thm_ui_scrollf,info

  compile_opt idl2
  
  dataNames = info.loadedData->GetAll()
  IF Is_Num(dataNames) THEN BEGIN
    info.statusBar->Update, 'The scroll forward function is not available until data has been loaded.'
  ENDIF ELSE BEGIN
    thm_ui_scroll, info.windowStorage, info.drawObject, 0
    info.drawObject->Update, info.windowStorage, info.loadedData
    info.drawObject->Draw
    info.scrollbar->update
    info.statusBar->Update, 'The active window has been scrolled forward.'
    info.historyWin->Update, 'Active window has been scrolled forward.'     
  ENDELSE
  
end