;+
;
;  Name: THM_UI_REFRESH
;  
;  Purpose: Refreshes the draw area of the GUI
;  
;  Inputs: The info structure from the main gui
;   
;
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-02-25 12:22:47 -0800 (Wed, 25 Feb 2009) $
;$LastChangedRevision: 5158 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_main_funcs/thm_ui_refresh.pro $
;-

pro thm_ui_refresh,info

  compile_opt idl2
  
  if is_struct(info) then begin
   info.drawObject->Update, info.windowStorage, info.loadedData
   info.drawObject->Draw
   info.statusBar->Update, 'The active window has been refreshed.'
   info.historyWin->Update, 'Active window has been refreshed.'     
  ENDif
  
end