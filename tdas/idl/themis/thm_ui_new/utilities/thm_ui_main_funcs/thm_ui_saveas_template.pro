;+
;
;  Name: THM_UI_SAVEAS_TEMPLATE
;  
;  Purpose: SAVES a themis template with a new file name
;  
;  Inputs: The info structure from the main gui
;
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-09-14 14:58:32 -0700 (Mon, 14 Sep 2009) $
;$LastChangedRevision: 6722 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_main_funcs/thm_ui_saveas_template.pro $
;-

pro thm_ui_saveas_template,info

  if info.marking ne 0 || info.rubberbanding ne 0 then begin
    return
  endif
  
  info.ctrl = 0

  filestring=info.template_filename
  IF NOT Is_String(filestring) then begin
     xt = Time_String(systime(/sec))
     timeString = Strmid(xt, 0, 4)+Strmid(xt, 5, 2)+Strmid(xt, 8, 2)+$
       '_'+Strmid(xt,11,2)+Strmid(xt,14,2)+Strmid(xt,17,2)
     filestring = 'themis_template_'+timeString+'.tgt'
  ENDIF
  
  path = file_dirname(filestring)
  
  fileName = dialog_pickfile(Title='Save As:', $
       Filter='*.tgt', File = fileString,path=path, /Write, Dialog_Parent=info.master)
  IF(Is_String(fileName)) THEN BEGIN
     save_themis_template,template=info.template_object,filename=fileName,$
         statusmsg=statusmsg,statuscode=statuscode
     IF (statuscode LT 0) THEN BEGIN
       dummy=dialog_message(statusmsg,/ERROR,/CENTER, title='Error in GUI')
     ENDIF ELSE BEGIN
       info.template_filename=filename
     ENDELSE
     info.statusBar->Update, statusmsg
     info.historywin->Update,statusmsg
  ENDIF ELSE BEGIN
    info.statusBar->Update, 'Operation Cancelled'
  ENDELSE

end