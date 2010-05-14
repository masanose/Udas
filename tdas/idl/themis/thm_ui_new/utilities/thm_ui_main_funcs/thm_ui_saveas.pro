;+
;
;  Name: THM_UI_SAVEAS
;  
;  Purpose: SAVES a themis document with a new file name
;  
;  Inputs: The info structure from the main gui
;
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-09-10 18:21:59 -0700 (Thu, 10 Sep 2009) $
;$LastChangedRevision: 6716 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_main_funcs/thm_ui_saveas.pro $
;-

pro thm_ui_saveas,info

  if info.marking ne 0 || info.rubberbanding ne 0 then begin
    return
  endif
  
  info.ctrl = 0

  dataNames = info.loadedData->GetAll()
  activeWindow=info.windowStorage->GetActive()
  activeWindow->GetProperty, Name=name
  filestring=info.mainFileName
  IF NOT Is_String(filestring) then begin
     xt = Time_String(systime(/sec))
     timeString = Strmid(xt, 0, 4)+Strmid(xt, 5, 2)+Strmid(xt, 8, 2)+$
       '_'+Strmid(xt,11,2)+Strmid(xt,14,2)+Strmid(xt,17,2)
     filestring = 'themis_saved_'+timeString+'.tgd'
  ENDIF
  fileName = dialog_pickfile(Title='Save As:', $
       Filter='*.tgd', File = fileString, /Write, Dialog_Parent=info.master)
  IF(Is_String(fileName)) THEN BEGIN
     save_document,windowstorage=info.windowstorage,filename=fileName,$
         statusmsg=statusmsg,statuscode=statuscode
     IF (statuscode LT 0) THEN BEGIN
          dummy=dialog_message(statusmsg,/ERROR,/CENTER, title='Error in GUI')
     ENDIF ELSE BEGIN
          activeWindow->GetProperty, Name=name
          info.mainFileName=filename
          Widget_Control, info.master, Base_Set_Title=filename
     ENDELSE
     info.statusBar->Update, statusmsg
     info.historywin->Update,statusmsg
  ENDIF ELSE BEGIN
    info.statusBar->Update, 'Operation Cancelled'
  ENDELSE

end