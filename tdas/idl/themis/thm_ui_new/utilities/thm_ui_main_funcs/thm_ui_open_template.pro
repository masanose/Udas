;+
;
;  Name: THM_UI_OPEN_TEMPLATE
;  
;  Purpose: Opens a themis template
;  
;  Inputs: The info structure from the main gui
;
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-09-14 16:51:22 -0700 (Mon, 14 Sep 2009) $
;$LastChangedRevision: 6725 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_main_funcs/thm_ui_open_template.pro $
;-
pro thm_ui_open_template,info

  compile_opt idl2

  if info.marking ne 0 || info.rubberbanding ne 0 then begin
    return
  endif
  
  info.ctrl = 0

  filestring=info.template_filename
  IF is_String(filestring) then begin
    path = file_dirname(filestring)
  endif
 
  fileName = Dialog_Pickfile(Title='Open THEMIS Template:', $
    Filter='*.tgt', Dialog_Parent=info.master,file=filestring,path=path)
  IF(Is_String(fileName)) THEN BEGIN
    open_themis_template,template=template,filename=fileName,$
        statusmsg=statusmsg,statuscode=statuscode
    IF (statuscode LT 0) THEN BEGIN
        dummy=error_message(statusmsg,/ERROR,/CENTER,traceback=0)
    ENDIF ELSE BEGIN
      info.template_filename = filename
      info.template_object = template
      info.windowStorage->setProperty,template=template
    ENDELSE
    info.statusBar->Update, statusmsg
    info.historywin->Update,statusmsg
  ENDIF ELSE BEGIN
    info.statusBar->Update, 'Invalid Filename'
  ENDELSE
    
end
