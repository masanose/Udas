;+
;
;  Name: THM_UI_EXIT
;  
;  Purpose: Exits the GUI
;  
;  Inputs: The info structure from the main gui
;          The event that led to this function call
;
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-03-02 12:51:33 -0800 (Mon, 02 Mar 2009) $
;$LastChangedRevision: 5194 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_main_funcs/thm_ui_exit.pro $
;-
pro thm_ui_exit,event,info=info

  compile_opt idl2
  
 Print, 'Main THEMIS GUI exited'
  IF N_Elements(info) NE 0 THEN BEGIN
  
    if obj_valid(info.historyWin) then begin
      info.historyWin->saveHistoryFile
    endif
  
    obj_destroy,info.historywin
    Obj_Destroy,info.statusBar
    Obj_Destroy,info.pathBar
    obj_destroy,info.drawObject
    obj_destroy,info.loadedData
    obj_destroy,info.windowStorage
  ENDIF
  Widget_Control, event.TOP, /Destroy
 ; if logical_true(!journal) then Journal
 ; WHILE !D.Window NE -1 DO WDelete, !D.Window
  RETURN
  
end