;+
;
;  Name: THM_UI_OPEN
;  
;  Purpose: Opens a themis document
;  
;  Inputs: The info structure from the main gui
;
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-09-10 18:21:59 -0700 (Thu, 10 Sep 2009) $
;$LastChangedRevision: 6716 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_main_funcs/thm_ui_open.pro $
;-
pro thm_ui_open,info

  compile_opt idl2

  if info.marking ne 0 || info.rubberbanding ne 0 then begin
    return
  endif
  
  info.ctrl = 0
    
  ;query if users want to delete data.
  
  del_text=dialog_message('Delete GUI data before loading the THEMIS document?'+thm_ui_newline()+'(Deleting data is strongly recommended.)',/question,/center)
  if strlowcase(del_text) eq 'yes' then begin
    confirm_text = 'Loading a new document will destroy all your GUI pages and loaded data.'
    nodelete = 0
  end else begin
    confirm_text = 'Loading a new document will destroy all your GUI pages.'
    nodelete = 1
  endelse
  
  confirm_text += thm_ui_newline() + 'Do you wish to continue?'
  
  ; Display confirmation dialog
  confirm_text=dialog_message(confirm_text,/CANCEL,/CENTER)
  if (confirm_text EQ 'OK') then begin
     fileName = Dialog_Pickfile(Title='Open THEMIS Document:', $
       Filter='*.tgd', Dialog_Parent=info.master)
     IF(Is_String(fileName)) THEN BEGIN
       open_themis_document,info=info,filename=fileName,$
           statusmsg=statusmsg,statuscode=statuscode,nodelete=nodelete
       IF (statuscode LT 0) THEN BEGIN
            dummy=error_message(statusmsg,/ERROR,/CENTER,traceback=0)
       ENDIF ELSE BEGIN
           ; If successful, sensitize data controls
           FOR i=0, N_Elements(info.dataButtons)-1 DO Widget_Control, info.dataButtons[i], sensitive=1
           ; Put filename in title bar
           activeWindow=info.windowStorage->GetActive()
           activeWindow->GetProperty, Name=name
           ;result=info.windowMenus->SetFilename(name, filename)
           info.mainFileName=filename
           Widget_Control, info.master, Base_Set_Title=filename
       ENDELSE
       info.statusBar->Update, statusmsg
       info.historywin->Update,statusmsg
     ENDIF ELSE BEGIN
       info.statusBar->Update, 'Invalid Filename'
     ENDELSE
  endif
      
end
