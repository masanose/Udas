;+
;
;  Name: THM_UI_SAVE
;  
;  Purpose: SAVES a themis document
;  
;  Inputs: The info structure from the main gui
;
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-09-10 18:21:59 -0700 (Thu, 10 Sep 2009) $
;$LastChangedRevision: 6716 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_main_funcs/thm_ui_save.pro $
;-
pro thm_ui_save,info

  compile_opt idl2
  
  if info.marking ne 0 || info.rubberbanding ne 0 then begin
    return
  endif
  
  info.ctrl = 0
    
  dataNames = info.loadedData->GetAll()
  IF Is_Num(dataNames) THEN BEGIN
     info.statusBar->Update, 'The save operation is not functional until data has been loaded'
  ENDIF ELSE BEGIN   
    activeWindow=info.windowStorage->GetActive()
    activeWindow->GetProperty, Name=name
    filename=info.mainFileName  
    reset_filename_flag = 0
    IF NOT Is_String(filename) then filename=''
    IF filename EQ '' THEN BEGIN 
      xt = Time_String(systime(/sec))
      timeString = Strmid(xt, 0, 4)+Strmid(xt, 5, 2)+Strmid(xt, 8, 2)+$
        '_'+Strmid(xt,11,2)+Strmid(xt,14,2)+Strmid(xt,17,2)
      fileString = 'themis_saved_'+timeString+'.tgd'
      filename = dialog_pickfile(Title='Save THEMIS Document:', $
         Filter='*.tgd', File = fileString, /Write, Dialog_Parent=info.master)
      reset_filename_flag=1
    ENDIF 
    IF(Is_String(filename)) THEN BEGIN
       save_document,windowstorage=info.windowstorage,filename=filename,$
           statusmsg=statusmsg,statuscode=statuscode
       IF (statuscode LT 0) THEN BEGIN
            dummy=dialog_message(statusmsg,/ERROR,/CENTER)
       ENDIF ELSE BEGIN
            if (reset_filename_flag EQ 1) then begin
               activeWindow->GetProperty, Name=name
               info.mainFileName=filename
               Widget_Control, info.master, Base_Set_Title=filename
            endif
       ENDELSE
       info.statusBar->Update, statusmsg
       info.historywin->Update,statusmsg
    ENDIF ELSE BEGIN
      info.statusBar->Update, 'Operation Cancelled'
    ENDELSE
  ENDELSE
  
end