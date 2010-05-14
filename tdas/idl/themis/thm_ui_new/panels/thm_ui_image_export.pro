;+
;NAME:
; thm_ui_image_export
;
;PURPOSE:
; This window allows the user to pick the filename/type for image export
;    
;CALLING SEQUENCE:
; thm_ui_image_export,gui_id,draw_save_object
; 
;INPUT:
; gui_id:  id of top level base widget from calling program
; draw_save_object: the object that does the actual output
;
;OUTPUT:
; 
;HISTORY:
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-06-12 16:42:21 -0700 (Fri, 12 Jun 2009) $
;$LastChangedRevision: 6187 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_image_export.pro $
;
;--------------------------------------------------------------------------------


PRO thm_ui_image_export_event, event

  Compile_Opt hidden,idl2

  Widget_Control, event.TOP, Get_UValue=state, /No_Copy

    ;Put a catch here to insure that the state remains defined

  err_xxx = 0
  Catch, err_xxx
  If(err_xxx Ne 0) Then Begin
    Catch, /Cancel
    Help, /Last_Message, output = err_msg
    if is_struct(state) then begin
      FOR j = 0, N_Elements(err_msg)-1 DO state.historywin->update,err_msg[j]
      ;Print, 'Error--See history'
      ok = error_message('Unknown error: widget will now close. See console for details.',traceback=1,$
           /noname, /center, title='Error in Image Export')
      thm_ui_error,state.gui_id
      Widget_Control, event.top, Set_UValue=state, /No_Copy   
    endif else begin
      FOR j = 0, N_Elements(err_msg)-1 DO print,err_msg[j]
    endelse
    
    Widget_Control, event.top,/destroy
    RETURN
  EndIf
     
  IF(Tag_Names(event, /Structure_Name) EQ 'WIDGET_KILL_REQUEST') THEN BEGIN  
    (*state.out).name=''
    state.historyWin->update,'Image export widget killed'
    state.statusBar->update,'Image export widget killed'
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy    
    Widget_Control, event.top, /Destroy
    RETURN      
  ENDIF

   ; Get the instructions from the widget causing the event and
   ; act on them.

  Widget_Control, event.id, Get_UValue=uval

  state.historywin->update,'THM_UI_IMAGE_EXPORT: User value: '+uval  ,/dontshow

  CASE uval OF
    'CANC': BEGIN
     ; print, 'Image Save Cancelled'
      (*state.out).name=''
      state.historyWin->update,'Image export widget canceled'
      state.statusBar->update,'Image export widget canceled'
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    END
    'OPTIONS':BEGIN
      out = thm_ui_image_export_options(state.tlb,state.type,state.drawObj,(*state.out).options,state.historywin)
         
      if ptr_valid(out) then begin
        ptr_free,(*state.out).options
        (*state.out).options = out
      endif
    
    END
    'HELP':BEGIN
      gethelppath,path
      xdisplayfile, path+'thm_ui_image_export.txt', group=state.tlb, /modal, done_button='Done', $
                    title='HELP: Image Options'
    END
    'SAVE': BEGIN
      thm_ui_image_export_save,state,event
      return
    END
    'FILENAME': BEGIN
      thm_ui_image_export_save,state,event
      RETURN
    END
    'DIRNAME': BEGIN
      widget_control,state.wDirName,get_value=newDirName
      if file_test(newDirName,/directory) then begin
        thm_ui_resetDirs,state,newDirName
      endif else begin
        widget_control,state.wDirName,set_value=state.dirName
      endelse
    END
    'DIRLIST': BEGIN
      baseName = (*state.dirList)[event.index]
      newDirName = state.dirName + baseName
      thm_ui_resetDirs,state,newDirName

    END
    'UP':BEGIN

      newDirName = thm_ui_updir(state.dirName)
      thm_ui_resetDirs,state,newDirName
      
    END
    'TYPE':BEGIN
    
       widget_control,state.wFileName,get_value=filename
       pos = stregex(filename,'\'+state.type+'$')
       
       state.type = state.typeList[event.index]
       if ptr_valid(state.out) then ptr_free,(*state.out).options
       
       if pos[0] ne -1 then begin
          filename = strmid(filename,0,pos) + state.type
          widget_control,state.wFileName,set_value=filename
       endif
     
    END
    ELSE: Print, 'Not yet implemented' 
  ENDCASE

  Widget_Control, event.top, Set_UValue=state, /No_Copy

  RETURN
END ;--------------------------------------------------------------------------------


;This routine performs the validation that occurs when 'save' is pressed or the enter key in the filename textbox is pressed
pro thm_ui_image_export_save,state,event
  compile_opt idl2,hidden
  
  widget_control,state.wFileName,get_value=basename
      
  if basename ne '' then begin
    filename = state.dirName + path_sep() + basename
    
    if strlowcase(!version.os_family) eq 'windows' && $
       ~stregex(filename,'\'+state.type+'$',/boolean) then begin
      state.historywin->update,'Windows file: ' + filename + ' has no extension, automatically adding extension' 
      filename = filename+state.type
    endif
         
    (*state.out).name=filename
    (*state.out).type=state.type 
  endif
  
  if file_test(filename,/write) then begin
  
    result = dialog_message('The file you have selected already exists.  Are you sure you want to overwrite it?',/Question,/default_no,/center)
  
    if result eq 'Yes' then begin      
      state.historyWin->update,'Image export save selected(overwrite)'
      state.statusBar->update,'Image export save selected(overwrite)'
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    endif else begin  
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      return
    endelse
  
  endif else if file_test(filename) then begin
    
    result = dialog_message('File is read only.  Please make a different selection.',/information)
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    return
    
  endif
  
  state.historyWin->update,'Image export save selected'
  state.statusBar->update,'Image export save selected'
  
  Widget_Control, event.TOP, Set_UValue=state, /No_Copy
  Widget_Control, event.top, /Destroy
  RETURN
end

function thm_ui_listDirs,basedir

  compile_opt idl2,hidden
  
    dirStructs = file_info(file_search(basedir+path_sep()+'*'))
    
    idx = where(dirStructs.directory eq 1,c)
    
    if ~c then return,['']
    
    return,file_basename(dirStructs[idx].name)+path_sep()

end

function thm_ui_updir,basedir,olddir=olddir

  compile_opt idl2,hidden
  
  partList = strsplit(basedir,path_sep(),/extract)
  if n_elements(partList) eq 1 then begin
    newDirName = ''
  endif else begin
    if strlowcase(!version.os_family) eq 'windows' then begin
      newDirName = strjoin(partList[0:n_elements(partList)-2],path_sep())
    endif else begin
      newDirName = path_sep() + strjoin(partList[0:n_elements(partList)-2],path_sep())
    endelse
    olddir = partList[n_elements(partlist)-1]
  endelse
  newDirName += path_sep()
  
  return,newDirName

end

pro thm_ui_resetDirs,state,newDirName

  newDirName = expand_path(newDirName)
  if newDirName ne path_sep() then newDirName += path_sep()
  newDirList = thm_ui_listdirs(newDirName)
  widget_control,state.wDirList,set_value=newDirList
  widget_control,state.wDirName,set_value=newDirName
  state.dirName=newDirName
  ptr_free,state.dirList
  state.dirList = ptr_new(newDirList)

end

function thm_ui_image_export, gui_id,drawObject,historywin,statusBar,inPtr

  compile_opt idl2
 
      ;top level base widget
      
  if obj_valid(historyWin) then historyWin->update,'Image export widget opened'
  if obj_valid(statusBar) then statusBar->update,'Image export widget opened'
  
  err=0
  catch, err
  if err ne 0 then begin
    catch, /cancel
    help,/last_message, output=err_msg
    for i=0, n_elements(err_msg)-1 do historyWin->update, err_msg[i]
    ok = error_message('An unknown error occured while starting Image Export.  See console for details.',$
        /noname, /center, title='Error in Image Export')
    widget_control, tlb, /destroy
    thm_ui_error, gui_id
    return,-1
  endif
      
  tlb = Widget_Base(/Col, Title='Save Image', Group_Leader=gui_id, $
     /Modal, /Floating,/tlb_kill_request_events)
                    
      ;base widgets
  row1 = widget_base(tlb,/row)
  row2 = widget_base(tlb,/row)
  row3 = widget_base(tlb,/row)
  row4 = widget_base(tlb,/row)
  row5 = widget_base(tlb,/row)
  
  typeList = ['.png','.eps','.bmp','.gif','.jpg','.jp2','.pic','.ppm','.srf']
  
  if strlowcase(!version.os_family) eq 'windows' then begin
    typeList = [typeList,'.emf'] ;emf is windows only
  endif
  
  if ptr_valid(inPtr) then begin
  
    dirname = thm_ui_updir((*inPtr).name,olddir=filename)
    type = (*inPtr).type
    
    typeIdx = where(type eq typeList,c)
    
    if c eq 0 then typeIdx = 0
    
    optionsptr = (*inPtr).options
  
  endif
  
  if ~keyword_set(filename) then begin
  
    cd,current=dirname
    dirname += path_sep()
    filename = 'untitled.png'
    type = '.png'
    typeIdx = 0    
    optionsptr = ptr_new()
  endif
  
;   for i=0, n_elements(typelist)-1 do begin
;     pos = stregex(filename,'\'+typelist[i]+'$')
;     if pos[0] ne -1 then filename = strmid(filename,0,pos)
;   endfor

  dirList = thm_ui_listdirs(dirname)
  
  dirNameText = widget_text(row1,value=dirname,uvalue="DIRNAME",/editable,xsize=30)
  upButton = widget_button(row1,Value=filepath('up1lvl.bmp',Subdir=['resource','bitmaps']),/bitmap,uvalue="UP")
  dirListText = widget_list(row2,value=dirList,uvalue="DIRLIST",ysize=7,xsize=35)
  fileNameText = widget_text(row3,value=filename,uvalue="FILENAME",/editable,xsize=30)
  fileTypeLabel = widget_label(row4,value="Save as: ") 
  fileTypeDrop = widget_combobox(row4,value=typeList,uvalue="TYPE")
  widget_control,fileTypeDrop,set_combobox_select=typeidx
  okButton = Widget_Button(row5, Value='SAVE', UValue='SAVE')
  cancelButton = Widget_Button(row5, Value = 'Cancel', UValue='CANC')
  optionsButton = Widget_Button(row5,value = 'Options...',UValue='OPTIONS')
  help_button = widget_button(row5,value='Help',uvalue='HELP')  
 
  outPtr = ptr_new({name:filename,type:type,options:optionsptr})
 
  state = {tlb:tlb, $
           gui_id:gui_id, $
           dirName:dirName, $
           dirList:ptr_new(dirList), $
           typeList:typeList,$
           type:type,$
           out:outPtr,$
           wDirname:dirNameText, $
           wDirList:dirListText, $
           wFileName:fileNameText,$
           drawObj:drawObject, $
           historywin:historywin,$
           statusBar:statusBar}
           

  Widget_control, tlb, Set_UValue=state, /No_Copy
  
  centerTLB,tlb
  
  Widget_control, tlb, /Realize
  XManager, 'thm_ui_image_export', tlb, /No_Block
  
  

  RETURN,*outPtr
  
END ;--------------------------------------------------------------------------------

