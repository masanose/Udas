;+ 
;NAME:
;  thm_ui_load_iugonet_data
;
;PURPOSE:
;  Generates the tab that loads iugonet data for the gui.
;
;HISTORY:
;$LastChangedBy: Y.Tanaka $
;$LastChangedDate: 2010-04-20 $
;
;Modifications:
;A. Shinbori, 12/05/2010
;A. Shinbori, 13/05/2010
;
;  �e�X�g�v���V�W���Bthm_ui_load_iugonet_data.pro���������B
;--------------------------------------------------------------------------------
pro thm_ui_load_iugonet_data_event,event

  compile_opt hidden,idl2

  err_xxx = 0
  Catch, err_xxx
  IF (err_xxx NE 0) THEN BEGIN
    Catch, /Cancel
    Help, /Last_Message, Output = err_msg
    if is_struct(state) then begin
      ;send error message
      FOR j = 0, N_Elements(err_msg)-1 DO state.historywin->update,err_msg[j]
      
      if widget_valid(state.baseID) && obj_valid(state.historyWin) then begin 
        thm_gui_error,state.baseid,state.historyWin
      endif
      
      ;update central tree, if possible
      if obj_valid(state.loadTree) then begin
        *state.treeCopyPtr = state.loadTree->getCopy()
      endif  
      
      ;restore state
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      
    endif
    Print, 'Error--See history'
    ok=error_message('An unknown error occured and the window must be restarted. See console for details.',$
       /noname, /center, title='Error in Load Data')

    widget_control, event.top,/destroy
  
    RETURN
  ENDIF

  widget_control, event.handler, Get_UValue=state, /no_copy
  
  ;Options
  widget_control, event.id, get_uvalue = uval
  ;not all widgets are assigned uvalues
  if is_string(uval) then begin
    case uval of
      
      'INSTRUMENT': begin
        typelist = widget_info(event.handler,find_by_uname='typelist')
        widget_control,typelist,set_value=*state.typeArray[event.index],set_list_select=0
        paramList = widget_info(event.handler,find_by_uname='paramlist')
        widget_control,paramList,set_value=*(*state.paramArray[event.index])[0]
      end
      'TYPELIST': begin
        instrument = widget_info(event.handler,find_by_uname='instrument')
        text = widget_info(instrument,/combobox_gettext)
        idx = (where(text eq state.instrumentArray))[0]
        parameter = widget_info(event.handler,find_by_uname='paramlist')
        widget_control,parameter,set_value=*(*state.paramArray[idx])[event.index]
      end
      'CLEARPARAM': begin
        paramlist = widget_info(event.handler,find_by_uname='paramlist')
        widget_control,paramlist,set_list_select=-1
      end
      'CLEARDATA': begin
        ok = dialog_message("This will delete all currently loaded data.  Are you sure you wish to continue?",/question,/default_no,/center)
        
        if strlowcase(ok) eq 'yes' then begin
          datanames = state.loadedData->getAll(/parent)
          if is_string(datanames) then begin
            for i = 0,n_elements(dataNames)-1 do begin
              result = state.loadedData->remove(datanames[i])
              if ~result then begin
                state.statusBar->update,'Unexpected error while removing data.'
                state.historyWin->update,'Unexpected error while removing data.'
              endif
            endfor
          endif
          state.loadTree->update
          state.callSequence->clearCalls  
        endif
        
      end   
      'DEL': begin
        dataNames = state.loadTree->getValue()
        
        if ptr_valid(datanames[0]) then begin
          for i = 0,n_elements(dataNames)-1 do begin
            result = state.loadedData->remove((*datanames[i]).groupname)
            if ~result then begin
              state.statusBar->update,'Unexpected error while removing data.'
              state.historyWin->update,'Unexpected error while removing data.'
            endif
          endfor
        endif
        state.loadTree->update
   
      end
      'ADD': begin
     
        instrument = widget_info(event.handler,find_by_uname='instrument')
        instrumentText = widget_info(instrument,/combobox_gettext)
        instrumentSelect = (where(instrumentText eq state.instrumentArray))[0]   
    
        type = widget_info(event.handler,find_by_uname='typelist')
        typeSelect = widget_info(type,/list_select)
     
        if typeSelect[0] eq -1 then begin
          state.statusBar->update,'You must select one type'
          state.historyWin->update,'IUGONET add attempted without selecting type'
          break
        endif
        
        typeText = (*state.typeArray[instrumentSelect])[typeSelect]
        
        parameter = widget_info(event.handler,find_by_uname='paramlist')
        paramSelect = widget_info(parameter,/list_select)
        
        if paramSelect[0] eq -1 then begin
          state.statusBar->update,'You must select at least one parameter'
          state.historyWin->update,'IUGONET add attempted without selecting parameter'
          break
        endif
        
;======== "*"�̏����̕����B�ʓ|�Ȃ̂ŁA���͊O���B ========;
;        ;handle '*' type, if present, introduce all
;        if in_set(0,paramSelect) then begin
;          paramText = (*(*state.paramArray[instrumentSelect])[typeSelect])
;        endif else begin
          paramText = (*(*state.paramArray[instrumentSelect])[typeSelect])[paramSelect]
;        endelse
;=========================================================;
        timeRangeObj = state.timeRangeObj      
        timeRangeObj->getProperty,startTime=startTimeObj,endTime=endTimeObj
      
        startTimeObj->getProperty,tdouble=startTimeDouble,tstring=startTimeString
        endTimeObj->getProperty,tdouble=endTimeDouble,tstring=endTimeString
        
        if startTimeDouble ge endTimeDouble then begin
          state.statusBar->update,'Cannot add data unless end time is greater than start time.'
          state.historyWin->update,'IUGONET add attempted with start time greater than end time.'
          break
        endif
              
        ;==============================================
        ;=== Call thm_ui_load_iugonet_data_load_pro ===
        ;==============================================
        thm_ui_load_iugonet_data_load_pro, $
                                  instrumentText,$
                                  typeText,$
                                  paramText,$
                                  [startTimeString,endTimeString],$
                                  state.loadedData,$
                                  state.statusBar,$
                                  state.historyWin
                                  
      
      
        state.loadTree->update
        
        ;====================================
        ;=== callSequence->addloadiugonet ===
        ;====================================
        state.callSequence->addloadiugonet,$
                               instrumentText,$
                               typeText,$
                               paramText,$
                               [startTimeString,endTimeString]
      
      end
      else:
    endcase
  endif
  
  Widget_Control, event.handler, Set_UValue=state, /No_Copy
  
  return
  
end


;************************************
;***** thm_ui_load_iugonet_data *****
;************************************
pro thm_ui_load_iugonet_data,tabid,loadedData,historyWin,statusBar,treeCopyPtr,timeRangeObj,callSequence,loadTree=loadTree,timeWidget=timeWidget
  compile_opt idl2,hidden
  
  ;load bitmap resources
  getresourcepath,rpath
  rightArrow = read_bmp(rpath + 'arrow_000_medium.bmp', /rgb)
  leftArrow = read_bmp(rpath + 'arrow_180_medium.bmp', /rgb)
  
  thm_ui_match_background, tabid, rightArrow 
  thm_ui_match_background, tabid, leftArrow
  
  ;===== event��thm_ui_load_iugonet_data_event�� =====;
  topBase = Widget_Base(tabid, /Row, /Align_Top, /Align_Left, YPad=1,event_pro='thm_ui_load_iugonet_data_event') 
  
  leftBase = widget_base(topBase,/col)
  middleBase = widget_base(topBase,/col,/align_center)
  rightBase = widget_base(topBase,/col)
  
  ;===== ���x�� =====;
  leftLabel = widget_label(leftBase,value='IUGONET Data Selection:',/align_left)
  rightLabel = widget_label(rightBase,value='Data Loaded:',/align_left)
  
  selectionBase = widget_base(leftBase,/col,/frame)
  
  treeBase = widget_base(rightBase,/col,/frame)
  
  addButton = Widget_Button(middleBase, Value=rightArrow, /Bitmap,  UValue='ADD', $
              ToolTip='Load data selection')
  minusButton = Widget_Button(middleBase, Value=leftArrow, /Bitmap, $
                Uvalue='DEL', $
                ToolTip='Delete data selected in the list of loaded data')
  
  loadTree = Obj_New('thm_ui_widget_tree', treeBase, 'LOADTREE', loadedData, $
                     XSize=400, YSize=425, mode=0, /multi,/showdatetime)
                     
  loadTree->update,from_copy=*treeCopyPtr
  
  clearDataBase = widget_base(rightBase,/row,/align_center)
  
  clearDataButton = widget_button(clearDataBase,value='Delete All Data',uvalue='CLEARDATA',/align_center,ToolTip='Deletes all loaded data')
  
  
  timeWidget = thm_ui_time_widget(selectionBase,$
                                  statusBar,$
                                  historyWin,$
                                  timeRangeObj=timeRangeObj,$
                                  uvalue='TIME_WIDGET',$
                                  uname='time_widget')
  
  ;================================
  ;========== Instrument ==========
  ;================================
  instrumentBase = widget_base(selectionBase,/row) 
  
  instrumentLabel = widget_label(instrumentBase,value='Instrument Type: ')

  instrumentArray = ['gmag','superdarn','EAR','MF_radar','meteor_radar','MU']
  
  instrumentCombo = widget_combobox(instrumentBase,$
                                       value=instrumentArray,$
                                       uvalue='INSTRUMENT',$
                                       uname='instrument')
                                              
  ;================================
  ;=========== Data Type ==========
  ;================================
  typeArray = ptrarr(6)
  
  typeArray[0] = ptr_new(['index','magdas'])
  typeArray[1] = ptr_new(['hok'])
  typeArray[2] = ptr_new(['trop_wind','trop_pwr','trop_spec_width'])
  typeArray[3] = ptr_new(['pameungpeuk'])
  typeArray[4] = ptr_new(['kototabang'])
  typeArray[5] = ptr_new(['trop_wind','trop_pwr','trop_spec_width'])
                                     
  dataBase = widget_base(selectionBase,/row)
  typeBase = widget_base(dataBase,/col)
  typeLabel = widget_label(typeBase,value='Data Type: ')
  typeList = widget_list(typeBase,$
                          value=*typeArray[0],$
                          uname='typelist',$
                          uvalue='TYPELIST',$
                          xsize=16,$
                          ysize=15)
  
  widget_control,typeList,set_list_select=0
  
  ;================================
  ;========== Parameters ==========
  ;================================
  paramArray = ptrarr(6)
  paramArray[0] = ptr_new(ptrarr(2))
  paramArray[1] = ptr_new(ptrarr(1))
  paramArray[2] = ptr_new(ptrarr(3))
  paramArray[3] = ptr_new(ptrarr(1))
  paramArray[4] = ptr_new(ptrarr(1))
  paramArray[5] = ptr_new(ptrarr(3))
    
  (*paramArray[0])[0] = ptr_new(['dst','ae','al','ao','au','ax','onw_pc3'])
  (*paramArray[0])[1] = ptr_new(['anc','asb','cmd','cst','dav','daw','dvs','eus','her', $
                                 'hob','ilr','kuj','lkw','mcq','mgd','mlb','mnd','mut', $
                                 'onw','prp','ptk','roc','sma','tir','twv','wad','yap'])
  (*paramArray[1])[0] = ptr_new(['azim_no','pwr','pwr_err','spec_width','spec_width_err',$
                                 'vlos','vlos_err','echo_flag','quality','quality_flag','position_tbl'])
  (*paramArray[2])[0] = ptr_new(['zonal_wind_ear','meridional_wind_ear','vertical_wind_ear'])
  (*paramArray[2])[1] = ptr_new(['pwr_beam1','pwr_beam2','pwr_beam3','pwr_beam4','pwr_beam5'])
  (*paramArray[2])[2] = ptr_new(['sw_beam1','sw_beam2','sw_beam3','sw_beam4','sw_beam5'])
  (*paramArray[3])[0] = ptr_new(['zonal_wind_pam','meridional_wind_pam'])                              
  (*paramArray[4])[0] = ptr_new(['zonal_wind_ktb','meridional_wind_ktb'])
  (*paramArray[5])[0] = ptr_new(['zonal_wind_mu','meridional_wind_mu','vertical_wind_mu'])
  (*paramArray[5])[1] = ptr_new(['pwr_beam1','pwr_beam2','pwr_beam3','pwr_beam4','pwr_beam5'])
  (*paramArray[5])[2] = ptr_new(['sw_beam1','sw_beam2','sw_beam3','sw_beam4','sw_beam5'])
                                                                           
  paramBase = widget_base(dataBase,/col)
  paramLabel = widget_label(paramBase,value='Parameter(s):')
  paramList = widget_list(paramBase,$
                         value=*((*paramArray[0])[0]),$
                         /multiple,$
                         uname='paramlist',$
                         xsize=24,$
                         ysize=15)
                         
  clearTypeButton = widget_button(paramBase,value='Clear Parameter',uvalue='CLEARPARAM',ToolTip='Deselect all parameters types')  
  state = {baseid:topBase,$
           loadTree:loadTree,$
           treeCopyPtr:treeCopyPtr,$
           timeRangeObj:timeRangeObj,$
           statusBar:statusBar,$
           historyWin:historyWin,$
           loadedData:loadedData,$
           callSequence:callSequence,$
           instrumentArray:instrumentArray,$
           typeArray:typeArray,$
           paramArray:paramArray}
           
  widget_control,topBase,set_uvalue=state
                                  
  return

end
