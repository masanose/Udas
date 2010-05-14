;+ 
;NAME:
;  thm_ui_load_goes_data
;
;PURPOSE:
;  Generates the tab that loads goes data for the gui.
;
;
;HISTORY:
;$LastChangedBy: aaflores $
;$LastChangedDate: 2010-02-03 11:54:08 -0800 (Wed, 03 Feb 2010) $
;$LastChangedRevision: 7198 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_load_data_file/thm_ui_load_goes_data.pro $
;
;--------------------------------------------------------------------------------
pro thm_ui_load_goes_data_event,event

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
      'CLEARPROBE': begin
        probelist = widget_info(event.handler,find_by_uname='probelist')
        widget_control,probelist,set_list_select=-1
      end
      'CLEARTYPE': begin
        datalist = widget_info(event.handler,find_by_uname='datalist')
        widget_control,datalist,set_list_select=-1
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
      
        datalist = widget_info(event.handler,find_by_uname='datalist')
        typeSelect = widget_info(datalist,/list_select)
        
        if typeSelect[0] eq -1 then begin
          state.statusBar->update,'You must select at least one data type'
          state.historyWin->update,'GOES add attempted without selecting data type'
          break
        endif
        
        types = state.typeArray[typeSelect]
        
        probelist = widget_info(event.handler,find_by_uname='probelist')
        probeSelect = widget_info(probelist,/list_select)
        
        if probeSelect[0] eq -1 then begin
          state.statusBar->update,'You must select at least one probe'
          state.historyWin->update,'GOES add attempted without selecting probe'
          break
        endif
        
        probes = state.probeArray[probeSelect]
        
        timeRangeObj = state.timeRangeObj      
        timeRangeObj->getProperty,startTime=startTimeObj,endTime=endTimeObj
      
        startTimeObj->getProperty,tdouble=startTimeDouble,tstring=startTimeString
        endTimeObj->getProperty,tdouble=endTimeDouble,tstring=endTimeString
        
        if startTimeDouble ge endTimeDouble then begin
          state.statusBar->update,'Cannot add data unless end time is greater than start time.'
          state.historyWin->update,'GOES add attempted with start time greater than end time.'
          break
        endif
           
        thm_ui_load_goes_data_load_pro, $
                                  probes,$
                                  types,$
                                  [startTimeString,endTimeString],$
                                  state.loadedData,$
                                  state.statusBar,$
                                  state.historyWin
                                  
      
      
        state.loadTree->update
        
        state.callSequence->addloadgoes,$
                               probes,$
                               types,$
                               [startTimeString,endTimeString]
      
      end
      else:
    endcase
  endif
  
  Widget_Control, event.handler, Set_UValue=state, /No_Copy
  
  return
  
end


pro thm_ui_load_goes_data,tabid,loadedData,historyWin,statusBar,treeCopyPtr,timeRangeObj,callSequence,loadTree=loadTree,timeWidget=timeWidget
  compile_opt idl2,hidden
  
  ;load bitmap resources
  getresourcepath,rpath
  rightArrow = read_bmp(rpath + 'arrow_000_medium.bmp', /rgb)
  trashcan = read_bmp(rpath + 'trashcan.bmp', /rgb)
  
  thm_ui_match_background, tabid, rightArrow 
  thm_ui_match_background, tabid, trashcan
  
  topBase = Widget_Base(tabid, /Row, /Align_Top, /Align_Left, YPad=1,event_pro='thm_ui_load_goes_data_event') 
  
  leftBase = widget_base(topBase,/col)
  middleBase = widget_base(topBase,/col,/align_center)
  rightBase = widget_base(topBase,/col)
  
  leftLabel = widget_label(leftBase,value='GOES Data Selection:',/align_left)
  rightLabel = widget_label(rightBase,value='Data Loaded:',/align_left)
  
  selectionBase = widget_base(leftBase,/col,/frame)
  treeBase = widget_base(rightBase,/col,/frame)
  
  addButton = Widget_Button(middleBase, Value=rightArrow, /Bitmap,  UValue='ADD', $
              ToolTip='Load data selection')
  minusButton = Widget_Button(middleBase, Value=trashcan, /Bitmap, $
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
    
  probeArrayValues = ['*','g10','g11','g12']
  probeArrayDisplayed = ['*(All)','GOES 10','GOES 11','GOES 12']
  
  typeArray = ['*','b_gsm','b_gei','b_enp','b_total','pos_gsm','pos_gei','vel_gei','t1_counts','t2_counts','dataqual','longitude','mlt']
                                  
  dataBase = widget_base(selectionBase,/row)
  probeBase = widget_base(dataBase,/col)
  probeLabel = widget_label(probeBase,value='Probe: ')
  probeList = widget_list(probeBase,$
                          value=probeArrayDisplayed,$
                          /multiple,$
                          uname='probelist',$
                          xsize=16,$
                          ysize=15)
  clearProbeButton = widget_button(probeBase,value='Clear Probe',uvalue='CLEARPROBE',ToolTip='Deselect all probes/stations')
                          
            
  typeBase = widget_base(dataBase,/col)
  typeLabel = widget_label(typeBase,value='Parameter Type:')
  typeList = widget_list(typeBase,$
                         value=typeArray,$
                         /multiple,$
                         uname='datalist',$
                         xsize=16,$
                         ysize=15)
                         
  clearTypeButton = widget_button(typeBase,value='Clear Parameter',uvalue='CLEARTYPE',ToolTip='Deselect all parameter types')
                                                             
  
  state = {baseid:topBase,$
           loadTree:loadTree,$
           treeCopyPtr:treeCopyPtr,$
           timeRangeObj:timeRangeObj,$
           statusBar:statusBar,$
           historyWin:historyWin,$
           loadedData:loadedData,$
           callSequence:callSequence,$
           probeArray:probeArrayValues,$
           typeArray:typeArray}
           
  widget_control,topBase,set_uvalue=state
                                  
  return

end