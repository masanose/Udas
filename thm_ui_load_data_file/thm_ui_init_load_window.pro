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
;
;    IUGONETタブが入れられるように改良。 by Y.Tanaka  20/04/2010
;-


pro thm_ui_init_load_update_tree_copy,state

  Compile_Opt idl2, hidden

  tab = widget_info(state.tabBase,/tab_current)
  
  ;angular spectra panel has no data tree 
  if tab gt 0 then tab-=1
  
  if obj_valid(state.treeArray[tab]) then begin
    *state.treeCopyPtr = state.treeArray[tab]->getCopy()
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
    Print, 'Load Themis Data widget killed.' 
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
    
    thm_ui_time_widget_update,state.timeArray[tab]
    
    widget_control,event.top,tlb_set_title=state.tabTitleText[tab]
    
    ;angular spectra panel has no data tree
    if tab gt 0 then tab-=1
    
   if obj_valid(state.treeArray[state.previousTab]) then begin
    *state.treeCopyPtr = state.treeArray[state.previousTab]->getCopy()
   endif
    
    if obj_valid(state.treeArray[tab]) then begin
      state.treeArray[tab]->update,from_copy=*state.treeCopyPtr
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
      Print, 'New File widget dismissed' 
      thm_ui_init_load_update_tree_copy,state
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
                             dataFlag, dataButtons, timerange, treeCopyPtr

  compile_opt idl2, hidden
  
  tabNum = 6   ;<===== タブの数 =====;
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
      iugonetTab = widget_base(tabBase,title='IUGONET Data')   ;<===== IUGONETタブ =====;
      
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
  ;=======================================
  ;=== IUGONETタブが選ばれたときの動作 ===
  ;=======================================
  thm_ui_load_iugonet_data,iugonetTab,loadedData,historyWin,statusText,treeCopyPtr,timeRange,$
                        callSequence,loadTree=iugonetTree,timeWidget=iugonetTimeWidget
                     
  treeArray = objarr(treeNum)
  timeArray = lonarr(tabNum)
  
  treeArray[0] = themisTree
  treeArray[1] = goesTree
  treeArray[2] = windTree
  treeArray[3] = aceTree 
  treeArray[4] = iugonetTree  ;<===== IUGONET Tree =====;
  
  timeArray[0] = loadTimeWidget
  timeArray[1] = specTimeWidget
  timeArray[2] = goesTimeWidget
  timeArray[3] = windTimeWidget
  timeArray[4] = aceTimeWidget
  timeArray[5] = iugonetTimeWidget   ;<===== IUGONET TimeWidget =====;
  
  tabTitleText = ["Themis: Load Ground and Probe Data",$
                  "Themis: Load Derived Particle Energy and Angular Spectra",$
                  "GOES: Load Data",$
                  "WIND: Load Data",$
                  "ACE: Load Data",$
                  "IUGONET: Load Data"]   ;<===== IUGONET load ウインドウのタイトル =====;

  state = {tlb:tlb, gui_id:gui_id,tabBase:tabBase, historyWin:historyWin, statusText:statusText,treeArray:treeArray,timeArray:timeArray,treeCopyPtr:treeCopyPtr,previousTab:0,tabTitleText:tabTitleText}

  CenterTLB, tlb
  Widget_Control, tlb, Set_UValue = state, /No_Copy
  Widget_Control, tlb, /Realize
  XManager, 'thm_ui_init_load_window', tlb, /No_Block

  RETURN
end
