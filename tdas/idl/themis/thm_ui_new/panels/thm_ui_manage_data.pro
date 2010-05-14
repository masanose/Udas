;+ 
;NAME:
; thm_ui_manage_data
;
;PURPOSE:
;  panel which allows user to import and export tplot data
;    
;CALLING SEQUENCE:
; thm_ui_manage_data,gui_id
; 
;INPUT:
; gui_id:  id of top level base widget from calling program
;
;OUTPUT:
; 
;HISTORY:
;$LastChangedBy: pcruce $
;$LastChangedDate: 2010-01-27 10:11:05 -0800 (Wed, 27 Jan 2010) $
;$LastChangedRevision: 7163 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_manage_data.pro $
;
;--------------------------------------------------------------------------------

pro thm_ui_manage_data_import, state, datanames, newnames=newnames, success=success

  compile_opt hidden,idl2
  
  success = 0
  
  ; this code is basically copied from tplot_gui
  ; check type and dimension of input
  dt = size(/type,datanames)
  ndim = size(/n_dimen,datanames)
   
  ; check for valid input
  if dt ne 0 then begin
   if dt ne 7 or ndim ge 1 then dnames = strjoin(tnames(datanames,/all),' ') $
     else dnames=datanames
  endif else begin
    last_tplot = tnames(/tplot)
    dprint,'Recreating the last tplot command with the following tplot variables:'
    dprint, last_tplot
    dnames = strjoin(last_tplot,' ')
  endelse
  
  
  ; make sure indexes are converted array of tplot variable names
  varnames = tnames(dnames,nd,ind=ind,/all)
  valid_names= '' ; same as above but will have pseudovars expanded to component varnames
  
  if nd eq 0 then begin
     dprint,'No valid variable names found to tplot! (use <VARIABLE> = TNAMES() and PRINT,<VARIABLE> to display)'
     return
  endif
  
  ; add tplot variables to loadedData object
  for i=0L,nd-1 do begin
  
    ; check if pseudovariable
    get_data, varnames[i], data=d
    dSize = size(d, /type)
    
    if dSize eq 7 then begin
    ;load tplot pseudovariable
      
      subNames = tnames(d, sub_nd, ind=sub_ind, /all)
      
      rebuild=0
      for j=0L,sub_nd-1 do begin
      ; load each component of pseudovariable
        
        if keyword_set(state.loadedData->add(subNames[j])) then begin
          if keyword_set(valid_names) then begin
            valid_names = [valid_names, subNames[j]]
          endif else begin
            valid_names = [subNames[j]]
          endelse
        endif
        
      endfor

    endif else begin
    ; load standard tplot variable
  
      if state.loadedData->add(varnames[i]) then begin        
        if keyword_set(valid_names) then begin
          valid_names = [valid_names, varnames[i]]
        endif else begin
          valid_names = [varnames[i]]
        endelse
      endif else begin
        state.statusbar->update,'Add failed: ' + varnames[i]
        state.historywin->update,'Add failed: ' + varnames[i]
      endelse
    
    endelse
  endfor
    
  ; verify incoming tplot variables
  
  if keyword_set(valid_names) then begin
    thm_ui_verify_data,state.tlb, valid_names, state.loadedData, state.windowStorage, $
                     state.historywin, newnames=newnames, success=success
  endif else begin
    state.historyWin->update,'No Valid Quantities Found to Load'
    state.statusBar->update,'No Valid Quantities Found to Load'
  endelse
  
end

pro thm_ui_manage_data_event,event

  compile_opt hidden,idl2

  Widget_Control, event.TOP, Get_UValue=state, /No_Copy

  ;Put a catch here to insure that the state remains defined

  err_xxx = 0
  Catch, err_xxx
  IF (err_xxx NE 0) THEN BEGIN
    Catch, /Cancel
    Help, /Last_Message, Output = err_msg
    if is_struct(state) then begin
      FOR j = 0, N_Elements(err_msg)-1 DO state.historywin->update,err_msg[j]
      x=state.gui_id
      histobj=state.historywin
      if obj_valid(state.guiTree) then begin
        *state.treeCopyPtr = state.guiTree->getCopy() 
      endif 
    endif
    Print, 'Error--See history'
    ok=error_message('An unknown error occured and the window must be restarted. See console for details.',$
       /noname, /center, title='Error in Manage Data')
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin 
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF
  
  IF(Tag_Names(event, /Structure_Name) EQ 'WIDGET_KILL_REQUEST') THEN BEGIN  
    if obj_valid(state.guiTree) then begin
      *state.treeCopyPtr = state.guiTree->getCopy() 
    endif 
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy    
    Widget_Control, event.top, /Destroy
    RETURN      
  ENDIF

  Widget_Control, event.id, Get_UValue=uval

  state.historywin->update,'THM_UI_MANAGE_DATA: User value: '+uval  ,/dontshow

  CASE uval OF
    'OK': BEGIN
      if obj_valid(state.guiTree) then begin
        *state.treeCopyPtr = state.guiTree->getCopy() 
      endif 
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    END
    'EDIT': BEGIN
      select = state.guiTree->getValue()
      if ptr_valid(select[0]) then begin
        for i = 0,n_elements(select)-1 do begin
          if keyword_set(names) then begin
            names = [names,(*select[i]).groupname]
          endif else begin
            names =  [(*select[i]).groupname]
          endelse
        endfor
        
         state.windowStorage->getProperty,callSequence=callSequence
        
         thm_ui_verify_data,state.tlb, names, state.loadedData, state.windowStorage, state.historywin, /edit,newnames=newnames,success=success,callSequence=callSequence
         
         if success then begin
           state.guiTree->update,selected=newnames
           state.statusBar->update,'Edit Completed'
           state.historyWin->update,'Edit Completed'
         endif else begin
           state.statusBar->update,'Edit Canceled'
           state.historyWin->update,'Edit Canceled'
         endelse
      ENDIF ELSE BEGIN
         state.statusBar->update,'GUI Data must be loaded and selected'
         state.historyWin->update,'Edit selected with no available data'
      ENDELSE
    END
    'TPLOT': BEGIN
      indices=widget_info(event.id, /list_select)
      if indices[0] eq -1 then begin  ;no valid selection
        state.tplotSelect = ptr_new()
      endif else begin  
        tplotNames = (*state.tplotNames)[indices]
        idx = where(tplotNames ne '',c)
        if c eq 0 then begin    ;no valid names
          state.tplotSelect = ptr_new()
        endif else begin
          tplotNames = tplotNames[idx]
          if Ptr_Valid(state.tplotSelect) THEN Ptr_Free, state.tplotSelect
          state.tplotSelect = Ptr_New(tplotNames)
        endelse
      endelse
    END
    'GUI_TREE':BEGIN
      ;do nothing
    END
    'RIGHT': BEGIN
      if ptr_valid(state.tplotselect) then begin 
        names = *state.tplotSelect
        thm_ui_manage_data_import, state, names, newnames=newnames, success=success
        if success then begin
          state.guiTree->update,selected=newnames
          state.statusBar->update,'Import Completed'
          state.historyWin->update,'Import Completed'
        endif else begin
          ;The verify panel requires data to have been added,
          ;so we just remove added quantities on failure
          ;The downside of this, is that if the operation was a replace
          ;operation, that data will disappear.
          tmp = state.loadedData->remove(newnames)
          state.guiTree->update
          state.statusBar->update,'Import canceled'
          state.historyWin->update,'Import canceled'
        endelse
      endif else begin 
        state.statusBar->update,'No Valid Selection'
        state.historyWin->update,'No valid selection for import'
      endelse
    END
    'LEFT': BEGIN
      select = state.guiTree->getValue()
      IF ptr_valid(select[0]) THEN BEGIN
         success_list = ''
         fail_list = ''
         FOR i=0,n_elements(select)-1 DO BEGIN
            
            if keyword_set(state.loadedData->GetTvarData((*select[i]).groupName)) then begin
              if success_list[0] eq '' then begin
                success_list = [(*select[i]).groupName]
              endif else begin
                success_list = [success_list,(*select[i]).groupName]
              endelse
            endif else begin
              if fail_list[0] eq '' then begin
                fail_list = [(*select[i]).groupName]
              endif else begin
                fail_list = [fail_list,(*select[i]).groupName]
              endelse
            endelse
         ENDFOR
;         tplot_names, names=names
	 names=tnames()        ;Does not have limit on length of name like TPLOT_NAMES.
         if ~keyword_set(names) then begin
           names = ['']
         endif
         if ptr_valid(state.tplotNames) then ptr_free, state.tplotNames
         state.tplotNames = ptr_new(names)
         widget_control,state.tplotList,set_value=names
         ptr_free,state.tplotSelect
         if keyword_set(success_list) then begin
           state.statusBar->update,'Exported Selection: ' + strjoin(success_list,' ')
           state.historyWin->update,'Exported Selection: ' + strjoin(success_list,' ')
         endif
         
         if keyword_set(fail_list) then begin
           state.statusBar->update,'Failed to Export Selection: ' + strjoin(fail_list,' ')
           state.historyWin->update,'Failed to Export Selection: ' + strjoin(fail_list,' ')
         endif
      ENDIF Else begin
         state.statusBar->update,'GUI Data must be loaded and selected'
         state.historyWin->update,'No valid selection for export'
      endelse
    END
    'TRASH': BEGIN
      result=dialog_message('Are you sure you want to delete the selected data from the GUI?', $
                            /question,/center, title='Manage Data: Delete GUI data?')
      if result eq 'Yes' then begin
        select = state.guiTree->getValue()
        if ptr_valid(select[0]) then begin
          for i = 0,n_elements(select)-1 do begin
            if ~state.loadedData->remove((*select[i]).groupname) then begin
              state.statusBar->update,'Error deleting: ' + (*select[i]).groupname
              state.historyWin->update,'Error deleting: ' + (*select[i]).groupname
              return
            endif
          endfor
          heap_gc
          state.guiTree->update
          state.statusBar->update,'Deleted Selection'
          state.historyWin->update,'Deleted Selection'
        endif else begin
          state.statusBar->update,'GUI Data must be loaded and selected'
          state.historyWin->update,'No valid selection for delete'
        endelse
      endif

    END
    ELSE: state.historyWin->update,'Unimplemented UVAL in manage data'
  ENDCASE
    
  Widget_Control, event.top, Set_UValue=state, /No_Copy

  RETURN

END


pro thm_ui_manage_data, gui_id, loadedData, windowStorage, historywin,treeCopyPtr

  compile_opt idl2
  
  xsize = 300
  ysize = 350
  
  err_xxx = 0
  Catch, err_xxx
  If(err_xxx Ne 0) Then Begin
    Catch, /Cancel
    Help, /Last_Message, output = err_msg
    FOR j = 0, N_Elements(err_msg)-1 DO historywin->update,err_msg[j]
    ;Print, 'Error--See history'
    ok = error_message('An unknown error occured starting Manage Data. See console for details.',$
        /noname, /center, title='Error in Manage Data')
    Widget_control,tlb,/destroy
    thm_gui_error,gui_id,historywin
    RETURN
  EndIf
  
  
  tlb = Widget_Base(/col, Title='Manage Data', Group_Leader=gui_id, /Modal, /Floating,/tlb_kill_request_events)
  
  ; Define Base structure for this window
  mainBase = Widget_Base(tlb, /row)
    tplotDataBase = Widget_Base(mainBase, /col, ypad=10)
    arrowBase = Widget_Base(mainBase, /col, ypad=115, xpad=12)
    guiDataBase = Widget_Base(mainBase, /col)
  buttonBase = widget_base(tlb,/row, /align_center)
  statusBase = widget_base(tlb,/row)
  
  ; Create the Widgets
  guiLabel = Widget_Label(guiDataBase, value='GUI Data: ',/align_left)
  guiTree = Obj_New('thm_ui_widget_tree',guiDataBase,'GUI_TREE',loadedData,xsize=xsize, $
                    ysize=ysize,mode=0,uname='GUI_TREE')       
  
  guiTree->update,from_copy=*treeCopyPtr
  
  getresourcepath,rpath
  trashcan = read_bmp(rpath + 'trashcan.bmp',/rgb)
  leftArrow = read_bmp(rpath + 'arrow_180_medium.bmp',/rgb)
  rightArrow = read_bmp(rpath + 'arrow_000_medium.bmp',/rgb)
  zoomIn = read_bmp(rpath + 'magnifier_zoom.bmp',/rgb)
  
  thm_ui_match_background, tlb, trashcan
  thm_ui_match_background, tlb, leftArrow
  thm_ui_match_background, tlb, rightArrow
  thm_ui_match_background, tlb, zoomIn

  rightButton = Widget_Button(arrowBase, Value=rightArrow, /Bitmap, $
                Uvalue='RIGHT', ToolTip='Create a gui variable from tplot data', xsize=27, ysize=27)
  leftButton = Widget_Button(arrowBase, Value=leftArrow, /Bitmap,  UValue='LEFT', $
              ToolTip='Create a tplot variable from gui data', xsize=27, ysize=27)
  trashbutton = Widget_Button(arrowBase, Value=trashcan, /Bitmap,  UValue='TRASH', $
              ToolTip='Delete a Gui Variable', xsize=27, ysize=27) 
  edit_button = widget_button(arrowBase, value=zoomIn, uvalue='EDIT',/Bitmap, $
              Tooltip='Examine a Gui Variable', xsize=27, ysize=27)
  tplotLabel = Widget_Label(tplotDataBase, value='TPLOT Data: ', /align_left)
;  tplot_names, names=tplotListNames
  tplotListNames=tnames()        ;Does not have limit on length of name like TPLOT_NAMES.
  if ~keyword_set(tplotListNames) then begin
    tplotListNames = ['']
  endif
  tplotList = Widget_List(tplotDataBase, value=tplotListNames, xsize=42, $
              ysize=floor(ysize/(!D.Y_CH_SIZE+4)), uvalue='TPLOT', /multiple)
  ok_button = widget_button(buttonBase,value='OK',uvalue='OK', xsize=75)
  
  statusBar = Obj_New("THM_UI_MESSAGE_BAR", statusBase, Xsize=98, YSize=1)
 
  historyWin->update,'Opening manage data panel'
 
  state = {tlb:tlb, gui_id:gui_id, statusBar:statusBar, guiTree:guiTree, tplotSelect:Ptr_New(),$
           tplotList:tplotList,tplotNames:ptr_new(tplotListNames), loadedData:loadedData, $
           windowStorage:windowStorage, historywin:historywin,treeCopyPtr:treeCopyPtr}
            
  Widget_Control, tlb, Set_UValue = state, /No_Copy
  CenterTLB, tlb
  Widget_Control, tlb, /Realize
  
  XManager, 'thm_ui_manage_data', tlb, /No_Block

  historyWin->update,'Closing manage data panel'
  RETURN
  
end
