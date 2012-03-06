;+
;NAME:
;  thm_ui_init_load_window
;
;PURPOSE:
;  Sets up the window and tab widgets for loading data into the THEMIS GUI.
;
;CALLING SEQUENCE:
;  thm_ui_init_load_window, gui_id, windowStorage, loadedData, historyWin, $
;                           dataFlag, dataButtons, timerange, treeCopyPtr
;
;INPUT:
;  gui_id:  The id of the main GUI window.
;  windowStorage:  The windowStorage object.
;  loadedData:  The loadedData object.
;  historyWin:  The history window object.
;  dataFlag: 
;  dataButtons: 
;  timerange:  The GUI timerange object.
;  treeCopyPtr:  Pointer variable to a copy of the load widget tree.
;  
;KEYWORDS:
;  none
;
;OUTPUT:
;  none
;-

pro thm_ui_init_load_update_tree_copy,state

  Compile_Opt idl2, hidden

  tab = widget_info(state.tabBase,/tab_current)
  
  ;angular spectra panel has no data tree 
  if tab eq 1 then tab-=1
  
  if obj_valid(state.treeArray[tab]) then begin
    *state.treeCopyPtr = state.treeArray[tab]->getCopy()
  endif
end

;restores user selects from previous panel open
pro thm_ui_load_data_set_user_select,state

 widget_control,widget_info(state.tabArray[0],/child),get_uvalue=load_data_state

  if widget_valid(load_data_state.itypeDropList) && (*state.userSelectPtr).inst ne -1 then begin
    widget_control,load_data_state.itypeDroplist,set_combobox_select=(*state.userSelectPtr).inst
  endif
  thm_ui_load_data_file_itype_sel, load_data_state
   
  if widget_valid(load_data_state.coordDropList) && (*state.userSelectPtr).coord ne -1 then begin
    widget_control,load_data_state.coordDropList,set_combobox_select=(*state.userSelectPtr).coord
  endif 
  thm_ui_load_data_file_coord_sel, load_data_state
  
  if widget_valid(load_data_state.observList) && ptr_valid((*state.userSelectPtr).observPtr) && (*(*state.userSelectPtr).observPtr)[0] ne -1 then begin
    widget_control,load_data_state.observList,set_list_select=*(*state.userSelectPtr).observPtr
  endif    
  thm_ui_load_data_file_obs_sel, load_data_state
  
  if widget_valid(load_data_state.level1List) && ptr_valid((*state.userSelectPtr).level1Ptr) && (*(*state.userSelectPtr).level1Ptr)[0] ne -1 then begin
    widget_control,load_data_state.level1List,set_list_select=*(*state.userSelectPtr).level1Ptr
  endif    
  thm_ui_load_data_file_l1_sel, load_data_state
  
  if widget_valid(load_data_state.level2List) && ptr_valid((*state.userSelectPtr).level2Ptr) && (*(*state.userSelectPtr).level2Ptr)[0] ne -1 then begin
    widget_control,load_data_state.level2List,set_list_select=*(*state.userSelectPtr).level2Ptr
  endif    
  thm_ui_load_data_file_l2_sel, load_data_state

  raw_data_widget_id = widget_info(widget_info(state.tabArray[0],/child),find_by_uname='raw_data')
  if widget_valid(raw_data_widget_id) then begin
    widget_control,raw_data_widget_id,set_button=(*state.userSelectPtr).uncalibrated
  endif

  widget_control,widget_info(state.tabArray[0],/child),set_uvalue=load_data_state,/no_copy
end

pro thm_ui_load_data_select_copy,state

  Compile_Opt idl2, hidden

  widget_control,widget_info(state.tabArray[0],/child),get_uvalue=load_data_state

  if ptr_valid(state.userSelectPtr) && is_struct(load_data_state) then begin
     if widget_valid(load_data_state.itypeDroplist) then begin
       (*state.userSelectPtr).inst = where(widget_info(load_data_state.itypeDroplist,/combobox_gettext) eq load_data_state.validItype)
     endif
     
     if widget_valid(load_data_state.coordDropList) then begin
       (*state.userSelectPtr).coord = where(widget_info(load_data_state.coordDropList,/combobox_gettext) eq *load_data_state.validCoords)
     endif
     
     if widget_valid(load_data_state.observList) then begin
       ptr_free,(*state.userSelectPtr).observPtr
       (*state.userSelectPtr).observPtr = ptr_new(widget_info(load_data_state.observList,/list_select))
     endif
     
     if widget_valid(load_data_state.level1List) then begin
       ptr_free,(*state.userSelectPtr).level1Ptr
       (*state.userSelectPtr).level1Ptr = ptr_new(widget_info(load_data_state.level1List,/list_select))
     endif
     
     if widget_valid(load_data_state.level2List) then begin
       ptr_free,(*state.userSelectPtr).level2Ptr
       (*state.userSelectPtr).level2Ptr = ptr_new(widget_info(load_data_state.level2List,/list_select))
     endif
     
     raw_data_widget_id = widget_info(widget_info(state.tabArray[0],/child),find_by_uname='raw_data')
     if widget_valid(raw_data_widget_id) then begin
       (*state.userSelectPtr).uncalibrated = widget_info(raw_data_widget_id,/button_set)
     endif
   endif
       
end

pro thm_ui_init_load_window_event, event

  Compile_Opt idl2, hidden

      ; get the state structure from the widget

  Widget_Control, event.TOP, Get_UValue=state, /No_Copy

      ; put a catch here to insure that the state remains defined

  err_xxx = 0
  Catch, err_xxx
  IF (err_xxx NE 0) THEN BEGIN
    Catch, /Cancel
    Help, /Last_Message, Output = err_msg
    if is_struct(state) then begin
      FOR j = 0, N_Elements(err_msg)-1 DO state.historywin->update,err_msg[j]
      x=state.gui_id
      histobj=state.historywin
      ;update central tree to reflect last expansion of current tree 
      thm_ui_init_load_update_tree_copy,state
    endif
    Print, 'Error--See history'
    ok=error_message('An unknown error occured and the window must be restarted. See console for details.',$
       /noname, /center, title='Error in Load Data')
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin 
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF
  
  ;kill request block

  IF (Tag_Names(event, /Structure_Name) EQ 'WIDGET_KILL_REQUEST') THEN BEGIN  


    exit_sequence:
    dprint,  'Load Themis Data widget killed.' ;<========= Changed from tdas 6.00 to 7.00
    state.historyWin->Update,'THM_UI_INIT_LOAD_WINDOW: Widget closed' 
    ;update central tree to reflect last expansion of current tree 
    thm_ui_init_load_update_tree_copy,state
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    Widget_Control, event.top, /Destroy
    RETURN 

  ENDIF

  ;update widget tree when new tab is selected
  IF (Tag_Names(event, /Structure_Name) EQ 'WIDGET_TAB') THEN BEGIN
    tab = event.tab
    
    thm_ui_time_widget_update,state.timeArray[tab], $
      oneday= thm_ui_time_widget_is_oneday(state.timeArray[state.previoustab])
    
    widget_control,event.top,tlb_set_title=state.tabTitleText[tab]
    
    ;angular spectra panel has no data tree
    if state.previousTab ne 1 then begin
      if obj_valid(state.treeArray[state.previousTab]) then begin
        *state.treeCopyPtr = state.treeArray[state.previousTab]->getCopy()
      endif
    endif
      
    if tab ne 1 then begin
      if obj_valid(state.treeArray[tab]) then begin
        state.treeArray[tab]->update,from_copy=*state.treeCopyPtr
      endif
    endif
  
    state.previousTab = tab
    
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    return
  
  endif  
      ; get the uval from this event so we know which widget was used 

  Widget_Control, event.id, Get_UValue=uval
  
  ; check for empty event coming from one of the other event handlers
  if size(uval,/type) eq 0 then begin 
    Widget_Control, event.top, Set_UValue = state, /No_Copy
    RETURN
  endif
  
  state.historywin->update,'THM_UI_INIT_LOAD_WINDOW: User value: '+uval  ,/dontshow

  CASE uval OF
    'DISMISS':BEGIN
      thm_ui_init_load_update_tree_copy,state
      thm_ui_load_data_select_copy,state
      Widget_Control, event.top, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    END
    ELSE:    
  ENDCASE
  
      ; must ALWAYS reset the state value
      
  Widget_Control, event.top, Set_UValue = state, /No_Copy

  RETURN
end

pro thm_ui_init_load_window, gui_id, windowStorage, loadedData, historyWin, $
                             dataFlag, dataButtons, timerange, treeCopyPtr,userSelectPtr

  compile_opt idl2, hidden
  
  tabNum = 6 ;<========= modified from 5 to 6 by Shinbori
  treeNum = tabNum - 1
  
  tlb = widget_base(/Col, Title = "THEMIS: Load Ground and Probe Data", Group_Leader = gui_id, $
                    /Modal, /Floating, /TLB_KILL_REQUEST_EVENTS)
    tabBase = widget_tab(tlb, location=0)
      loadDataTab = widget_base(tabBase, title="THEMIS Data", $
                                event_pro='thm_ui_load_data_file_event') 
      getspecTab = widget_base(tabBase, title='THEMIS Derived Spectra', $
                               event_pro='thm_ui_part_getspec_options_event')
      goesTab = widget_base(tabBase, title='GOES Data')
      windTab = widget_base(tabBase, title='WIND Data')
      aceTab = widget_base(tabBase,title='ACE Data')
      iugonetTab = widget_base(tabBase,title='IUGONET Data')   ;<===== Added to IUGONET tab =====;
      
    ;statusBase = Widget_Base(tlb, /Row)
    bottomBase = widget_base(tlb, /Col, YPad=6, /Align_Left)

  
  widget_control, tabBase, set_tab_current=0
    
  ; Create Status Bar Object
  okButton = Widget_Button(bottomBase, Value='Done', XSize=75, uValue='DISMISS', $
    ToolTip='Dismiss Load Panel', /align_center)
  statusText = Obj_New('THM_UI_MESSAGE_BAR', $
                       Value='Status information is displayed here.', $
                        bottomBase, XSize=135, YSize=1)

  windowStorage->getProperty, callSequence=callSequence
  
  ;At the moment, this saves user preferences only for the main THEMIS load window.  
  userSelectStruct = $
    {inst:-1,$
     coord:-1,$
     observPtr:ptr_new(-1),$
     level1Ptr:ptr_new(-1),$
     level2Ptr:ptr_new(-1),$
     uncalibrated:0}
   
  if ~ptr_valid(userSelectPtr) then begin
    userSelectPtr = ptr_new(userSelectStruct)
  endif
  
  thm_ui_load_data_file, loadDataTab, gui_id, windowStorage, loadedData, $
                         historyWin, dataFlag, dataButtons, timerange, statusText, $
                         loadTree=themisTree,timeWidget=loadTimeWidget,treeCopyPtr
                         
  thm_ui_part_getspec_options, getspecTab, loadedData, historyWin, statusText, $
                               timerange,callSequence,timeWidget=specTimeWidget

  thm_ui_load_goes_data,goesTab,loadedData,historyWin,statusText,treeCopyPtr,timeRange,$
                        callSequence,loadTree=goesTree,timeWidget=goesTimeWidget
  
  thm_ui_load_wind_data,windTab,loadedData,historyWin,statusText,treeCopyPtr,timeRange,$
                        callSequence,loadTree=windTree,timeWidget=windTimeWidget
  
  thm_ui_load_ace_data,aceTab,loadedData,historyWin,statusText,treeCopyPtr,timeRange,$
                        callSequence,loadTree=aceTree,timeWidget=aceTimeWidget
  ;=========================================================
  ;=== Added to the procedure 'thm_ui_load_iugonet_data' ===
  ;=========================================================
  thm_ui_load_iugonet_data,iugonetTab,loadedData,historyWin,statusText,treeCopyPtr,timeRange,$
                        callSequence,loadTree=iugonetTree,timeWidget=iugonetTimeWidget                          

  treeArray = objarr(treeNum+1)
  timeArray = lonarr(tabNum)
  
  treeArray[0] = themisTree
  treeArray[1] = obj_new() ;filler obj, should not be referenced 
  treeArray[2] = goesTree
  treeArray[3] = windTree
  treeArray[4] = aceTree 
  treeArray[5] = iugonetTree  ;<===== Added to IUGONET Tree =====;
  
  timeArray[0] = loadTimeWidget
  timeArray[1] = specTimeWidget
  timeArray[2] = goesTimeWidget
  timeArray[3] = windTimeWidget
  timeArray[4] = aceTimeWidget
  timeArray[5] = iugonetTimeWidget   ;<===== Added to IUGONET TimeWidget =====;  
  
  tabArray = lonarr(tabNum)
  tabArray[0] = loadDataTab
  tabArray[1] = getSpecTab
  tabArray[2] = goesTab
  tabArray[3] = windTab
  tabArray[4] = aceTab
  tabArray[5] = iugonetTab   ;<===== Added to IUGONET Tab =====; 
  
  tabTitleText = ["Themis: Load Ground and Probe Data",$
                  "Themis: Load Derived Particle Energy and Angular Spectra",$
                  "GOES: Load Data",$
                  "WIND: Load Data",$
                  "ACE: Load Data",$
                  "IUGONET: Load Data"]  ;<===== Added to the comment of IUGONET load Data=====;
                 

  state = {tlb:tlb, gui_id:gui_id,tabBase:tabBase, historyWin:historyWin, statusText:statusText,treeArray:treeArray,timeArray:timeArray,tabArray:tabArray,treeCopyPtr:treeCopyPtr,previousTab:0,tabTitleText:tabTitleText, userSelectPtr:userSelectPtr}

  CenterTLB, tlb
  Widget_Control, tlb, Set_UValue = state, /No_Copy
  Widget_Control, tlb, /Realize
  Widget_Control, tlb, get_UValue = state, /No_Copy
  thm_ui_load_data_set_user_select,state
  Widget_Control, tlb, set_UValue = state, /No_Copy

;==============Changed from tdas 6.00 to 7.00=============
  ;keep windows in X11 from snaping back to 
  ;center during tree widget events 
  if !d.NAME eq 'X' then begin
    widget_control, tlb, xoffset=0, yoffset=0
  endif
  
  XManager, 'thm_ui_init_load_window', tlb, /No_Block
;=========================================================
 
  RETURN
end