;+ 
;NAME: 
; thm_ui_layout_options
;PURPOSE:
; This routine creates and handles the layout widget.
;CALLING SEQUENCE:
; thm_ui_layout_options
;INPUT:
; info:  Info structure from thm_gui_new.
;OUTPUT:
; none
;HISTORY:
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2010-03-03 12:15:58 -0800 (Wed, 03 Mar 2010) $
;$LastChangedRevision: 7394 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_layout_options/thm_ui_layout_options.pro $
;--------------------------------------------------------------------------------


function thm_ui_check_panel_layout, state
  
  compile_opt idl2, hidden
  
  if obj_valid((*state.panelObjs)[0]) then begin
    for i=0,n_elements(*state.panelObjs)-1 do begin
    
      currentPanel = (*state.panelObjs)[i]
      panelLayoutPos = state.cwindow->getPanelPos(currentPanel)
      
      if panelLayoutPos eq -1 then begin
        state.historywin->update,'THM_UI_LAYOUT_OPTIONS: Problem evaluating locked panel layout.'
        state.statusbar->update,'THM_UI_LAYOUT_OPTIONS: Problem evaluating locked panel layout.'
        ok=error_message('Problem panel layout in "Locked Mode," probably from overlapping panels.' + $
                         ' Closing Layout Window and resetting to previous layout.',/noname,/center, $
                         title='Error in Plot/Layout Options', traceback=0)
        return, 0
      endif
    endfor
    return, 1
  endif else return, 1
end


pro thm_ui_layout_set_yselect, state, value
; set state.ySelect

  compile_opt idl2, hidden
  
  value->getProperty, mode=mode

  CASE mode OF
    0: BEGIN                    ; tplot mode
      val = value->getValue()

      if ptr_valid(state.ySelect) then ptr_free, state.ySelect
      
      if size(val, /type) eq 10 then begin
      
        for i=0,n_elements(val)-1 do begin
        
          ; get datanames for each y selection
          ySelect_i = (*val[i]).datanames
          
          ; add to array of y datanames
          if i eq 0 then ySelect = ySelect_i else ySelect = [ySelect, ySelect_i]
        endfor

        state.ySelect = ptr_new(ySelect)
        
        ; get x datanames from timename of y data tree            
        if ~state.compviewmode then begin
        
          if ptr_valid(state.xSelect) then ptr_free, state.xSelect
          
          for i=0,n_elements(val)-1 do begin
          
            ; make sure xSelect has same number of elements as ySelect
            xSelect_i = replicate(((*val[i]).timename), n_elements((*val[i]).datanames))
            
            ; add to array of x datanames
            if i eq 0 then xSelect = xSelect_i else xSelect = [xSelect, xSelect_i]
          endfor
          
          state.xSelect = ptr_new(xSelect)
        endif
      endif
    END
    1: BEGIN
      yNames = value->getValue()
      if ptr_valid(state.ySelect) then ptr_free, state.ySelect
      state.ySelect = Ptr_New(yNames)
    END
    2: BEGIN
      yNames = value->getValue()
      if ptr_valid(state.ySelect) then ptr_free, state.ySelect
      state.ySelect = Ptr_New(yNames)
    END
    3: BEGIN
      yNames = value->getValue()
      
      for i=0,n_elements(yNames)-1 do begin
      
        ; get datanames for each y selection
        if state.loadedData->isparent(yNames[i]) then begin
          group = state.loadedData->getGroup(yNames[i])
          ySelect_i = group->getDataNames()
        endif else ySelect_i=yNames[i]
        
        ; add to array of y datanames
        if i eq 0 then ySelect = ySelect_i else ySelect = [ySelect, ySelect_i]
      endfor
      
      ySelect = ySelect[uniq(strlowcase(ySelect),bsort(strlowcase(ySelect)))]

      if ptr_valid(state.ySelect) then ptr_free, state.ySelect
      state.ySelect = ptr_new(ySelect)
      
      for i=0,n_elements(ySelect)-1 do begin
      
        group = state.loadedData->getobjects(name=ySelect[i])
        group[0]->getproperty, timename=xSelect_i, indepName=indepName
        
        ; use diff quantity for x-axis if specified by indepName 
        if is_string(indepName) then xSelect_i=indepName
        
        ; add to array of x datanames
        if i eq 0 then xSelect = xSelect_i else xSelect = [xSelect, xSelect_i]
      endfor
      
      if ptr_valid(state.xSelect) then ptr_free, state.xSelect
      state.xSelect = ptr_new(xSelect)
    END
  ENDCASE
end


pro thm_ui_layout_draw_update,tlb,state,apply=apply

  compile_opt idl2,hidden
  
  state.info.drawObject->update,state.info.windowStorage,state.info.loadedData 
  state.info.drawObject->draw
  state.info.scrollbar->update
end


pro thm_ui_update_panel_list, state=state, panelNames=panelNames, $
                              panelValue=panelValue, panel_ValueInfo=panel_ValueInfo, $
                              panelLayout=panelLayout, ntr=ntr
; get panel/tracenames and panel layout

  compile_opt idl2, hidden

  if state.npanels LT 1 then begin
    result=dialog_message("There are no panels on this page. Please create a panel before performing operations.", /INFO, /CENTER) 
    return
  endif
  
  panelNames = strarr(state.npanels)
  ntr = intarr(state.npanels)

  for i = 0,state.npanels-1 do begin ; loop over panels
  
    cPanel = (*state.panelObjs)[i]
    
    panelNames[i] = cPanel->constructPanelName()
    state.cwindow->GetProperty, locked=locked
    if i eq locked then panelNames[i] = state.lockPrefix + panelNames[i]
    
    if i eq 0 then panelValue = panelNames[i] $
      else panelValue = [panelValue, panelNames[i]]

    if i eq 0 then panelLayout = cPanel->getLayoutStructure() $
      else panelLayout = [panelLayout, cPanel->getLayoutStructure()]
    
    if i eq 0 then panel_ValueInfo = {panelListInfo, ispanel:1, istrace:0, $
                                     panelid:panelLayout[i].id, traceid:-1} $
      else panel_ValueInfo = [panel_ValueInfo, {panelListInfo, ispanel:1, istrace:0, $
                                              panelid:panelLayout[i].id, traceid:-1}]
    
    cPanel->getProperty,traceSettings=traceSettings
    traces = traceSettings->get(/all)
    
    if obj_valid(traces[0]) then begin
      ntr[i] = n_elements(traces)
      trNames = cPanel->constructTraceNames()
      
      for j = 0,ntr[i]-1 do begin
  
        panelValue = [panelValue, trNames[j]]
        
        panel_ValueInfo = [panel_ValueInfo, {panelListInfo, ispanel:0, istrace:1, $
                                           panelid:panelLayout[i].id, traceid:j}]
        
        traces[j]->getProperty,dataX=dx, dataY=dy
        
        if obj_isa(traces[j],'thm_ui_spectra_settings') then begin
            traces[j]->getProperty,dataZ=dz
        endif else dz = ''
        
      endfor
      
    endif
  endfor
 
end

pro thm_ui_init_tree_widgets, tlb, state=state

compile_opt idl2, hidden

  statedef = ~(~size(state,/type))
  ; 
  if ~statedef then begin
    Widget_Control, tlb, Get_UValue=state, /No_Copy  ;Only get STATE if it is not passed in.
  endif else begin
    tlb = state.tlb
  endelse
  
  ;**********************************************************
  ; Initialize tree widgets
  ;**********************************************************
  
  widget_control,tlb,update=0
  
  ;the index of the tab that should be selected after update
  tab_index = 0
    
  ; destroy tertiary (x-axis) tree widget
  if widget_info(state.terBase, /valid_id) then begin
    
    widget_control, state.terBase, /destroy
    if obj_valid(state.terTree) then obj_destroy, state.terTree
  endif
  
    ; destroy tertiary (y-axis) tree widget
  if widget_info(state.secBase, /valid_id) then begin
    
    widget_control, state.secBase, /destroy
    if obj_valid(state.secTree) then obj_destroy, state.secTree
  endif
  
    ; destroy tertiary (z-axis) tree widget
  if widget_info(state.priBase, /valid_id) then begin
    
    widget_control, state.priBase, /destroy
    if obj_valid(state.priTree) then obj_destroy, state.priTree
  endif
  
  if state.compviewmode then begin
 
  ; create secondary (x-axis) tree widget/object

    if state.xtree_copy gt 0 then begin
      xcopy=state.xtree_copy
    endif else if state.pritree_copy gt 0 then begin
      xcopy=state.pritree_copy
    endif else begin
      xcopy=0
    endelse

    state.terBase = Widget_Base(state.tabBase, Title='X-Var', /Col)
    state.terTree = obj_new('thm_ui_widget_tree', state.terBase,'TERTREE', $
                      state.loadedData, XSize=state.tree_size, YSize=340, $
                      mode=1, multi=1, leafonly=1, uname='tertree', /showdatetime,$
                      from_copy=xcopy)
    
    id = widget_info(state.tlb, find_by_uname='tertree')
    widget_control, id, get_value=val
   
    if state.sectree_copy gt 0 then begin
      ycopy=state.sectree_copy
    endif else if state.pritree_copy gt 0 then begin
      ycopy=state.pritree_copy
    endif else begin
      ycopy=0
    endelse
    
    state.secBase = Widget_Base(state.tabBase, Title='Y-Var', /Col)
    state.secTree = obj_new('thm_ui_widget_tree', state.secBase,'SECTREE', $
                          state.loadedData, XSize=state.tree_size, YSize=340, $
                          mode=3, multi=1,leafonly=1, uname='sectree', /showdatetime,$
                          from_copy=ycopy) 
        
    id = widget_info(state.tlb, find_by_uname='sectree')
    widget_control, id, get_value=val
  
    tab_index = 2 
        
    pri_name = 'Z-Var'
  endif else begin
    pri_name = 'Dependent Variable'
  endelse

  if state.pritree_copy gt 0 then begin
    pricopy=state.pritree_copy
  endif else begin
    pricopy=0
  endelse

  state.priBase = Widget_Base(state.tabBase, Title=pri_name, /Col)
  state.priTree = obj_new('thm_ui_widget_tree', state.priBase,'PRITREE', $
                    state.loadedData, XSize=state.tree_size, YSize=340, $
                    mode=3, multi=1, leafonly=1, uname='pritree', /showdatetime,$
                    from_copy=pricopy)

  
  id = widget_info(state.tlb, find_by_uname='pritree')
  widget_control, id, get_value=val
 
  ;set selected tab
  widget_control,state.tabBase,set_tab_current=tab_index
  
  widget_control,tlb,update=1
  
  if ~statedef then Widget_Control, tlb, Set_UValue=state, /No_Copy   ;Only put STATE if it was not passed in.

end

pro thm_ui_update_var_widget, tlb, state=state

  compile_opt idl2, hidden
  
  statedef = ~(~size(state,/type))
  ; 
  if ~statedef then begin
    Widget_Control, tlb, Get_UValue=state, /No_Copy  ;Only get STATE if it is not passed in.
  endif else begin
    tlb = state.tlb
  endelse
  
  
  ; Initialize Variables widget
  if state.npanels gt 0 then cpanel = (*state.panelObjs)[state.panel_sel]
  IF Obj_Valid(cpanel) THEN cpanel->GetProperty, Variables=variables ELSE variables=-1 
  IF Obj_Valid(variables) THEN varObjs = variables->Get(/all) ELSE varObjs=-1
  IF Size(varObjs, /type) EQ 11 THEN BEGIN
     varNames = make_array(N_Elements(varObjs), /string)
     FOR i=0,N_Elements(varObjs)-1 DO BEGIN
        varObjs[i]->GetProperty, Text=text
        IF Obj_Valid(text) THEN text->GetProperty, Value=value
        varNames[i]=value 
     ENDFOR
  ENDIF
  IF Size(varNames, /Type) NE 0 && N_Elements(varNames) GT 0 THEN BEGIN
    WIDGET_CONTROL, state.variableText, set_value=varnames,set_text_select=[0,0]
  ENDIF ELSE WIDGET_CONTROL, state.variableText, set_value=''
end

;modularizes repeated panel add code
;Adds a panel to the list, updates state representation
pro thm_ui_layout_add_panel,state

  compile_opt idl2,hidden
  
   ; Add panel to panel layout
  thm_ui_make_default_panel, state.info.windowStorage,state.template, outpanel=newpanel
      
  newpanelLayout = newpanel->getlayoutstructure()
  state.nRows = newpanelLayout.row
  state.nCols = newpanelLayout.col
  widget_control, state.rowText, set_value=state.nRows
  widget_control, state.colText, set_value=state.nCols

; begin "in case of cancel of child panel" code
  state.cWindow->GetProperty, Panels=panels, nRows=nrows, nCols=ncols;, locked=locked ;locked necessary?
  if ptr_valid(state.panels) then ptr_free, state.panels
  state.panels = ptr_new(panels)
; end "in case of cancel of child panel" code

  widget_control, state.rowPageText, set_value=nRows
  widget_control, state.colPageText, set_value=ncols
  
  if ptr_valid(state.panelObjs) then ptr_free, state.panelObjs
  state.panelObjs = ptr_new(*state.panels->Get(/All))
  
  state.npanels = n_elements(*state.panelObjs)
  if ptr_valid(state.panelValue) then ptr_free, state.panelValue
  if ptr_valid(state.panel_ValueInfo) then ptr_free, state.panel_ValueInfo
  if ptr_valid(state.panelNames) then ptr_free, state.panelNames
  ;panelNames = strarr(state.npanels)

  ; get panel/tracenames and panel layout
  thm_ui_update_panel_list, state=state, panelNames=panelNames, $
     panelValue=panelValue, panel_ValueInfo=panel_ValueInfo, $
     panelLayout=panelLayout

  if ptr_valid(state.panelLayout) then ptr_free, state.panelLayout
  state.panelLayout = ptr_new(panelLayout)
  state.panelValue = ptr_new(panelValue)
  state.panel_ValueInfo = ptr_new(panel_ValueInfo)
  state.panelNames = ptr_new(panelNames)
  state.panel_sel = state.npanels-1
  state.trace_sel = -1
  

  widget_control, state.rowText, /sensitive
  widget_control, state.colText, /sensitive
  widget_control, state.shiftupButton, /sensitive
  widget_control, state.shiftdownButton, /sensitive
  widget_control, state.shiftleftButton, /sensitive
  widget_control, state.shiftrightButton, /sensitive
  id = widget_info(state.tlb, find_by_uname='rempan')
  widget_control, id, /sensitive
  widget_control, state.editButton, sensitive=1
  
  
  Widget_Control, state.plusButton, /sensitive
  
  widget_control, state.panelList, set_value=*state.panelValue
  widget_control, state.panelList, $
                  set_list_select=where((*state.panelNames)[state.npanels-1] eq *state.panelValue)

  state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: New panel added.'
  state.statusbar->update,'THM_UI_LAYOUT_OPTIONS: New panel added.'

end

;This modularizes duplicated code for adding spectra and lines
;Set keyword spect, to indicate spectrograms should be created
;from current state.
pro thm_ui_layout_add_trace,event,state,spectra=spectra

  compile_opt idl2,hidden
  
    ;Add selected traces to selected panel

  ;IF N_Elements(*state.xSelect) GT 0 && N_Elements(*state.ySelect) GT 0 THEN BEGIN
  ;IF is_string(*state.xSelect) && (state.isspec || is_string(*state.ySelect)) THEN BEGIN
        
  h_msg = 'THM_UI_LAYOUT_OPTIONS: '
  e_msg = ''

  state.historywin->Update, h_msg+'Adding Variables to Panel ' + $
                               strtrim(state.panel_sel + 1,2) + '...'

  state.statusbar->update, 'Adding Variables to Panel ' + $
                               strtrim(state.panel_sel + 1,2) + '...'
        
  thm_ui_layout_get_selection,state,x_select=xnames,y_select=ynames,z_select=znames,spectra=spectra
   
  nnames = n_elements(xnames)

;Loop Over Names
;---------------
  bad=0
  for i=0,nnames-1 do begin
    msg=''

    if ((n_elements(znames) eq 1 && znames[0] eq '') || znames[i] eq '') && keyword_set(spectra) then begin
      ;checking is ordered
      msg = 'Invalid Z variable selection, unable to add "'+xnames[i]+'"/"'+ynames[i]+'" to panel.'
    endif else if ~keyword_set(ynames[i]) then begin
      msg = 'Invalid Y variable selection, unable to add "'+xnames[i]+'" to panel.'
    endif else if ~keyword_set(xnames[i]) then begin
      msg = 'Invalid X variable selection, unable to add "'+ynames[i]+'" to panel.'
    endif

    if keyword_set(msg) then begin ;notify user, then skip variable or return
      bad++
      state.statusbar->update, msg
      state.historywin->update, h_msg+msg
      e_msg = 'At least one variable could not be added. Check history window for details.'
      continue
    endif 
    
    ;first valid component
    if i-bad eq 0 then begin    
      ;*************************
      ;Create a panel if none exist
      ;*************************
      if state.npanels eq 0 then begin
        thm_ui_layout_add_panel,state
      endif

      ;*************************
      ;Add trace/spectra
      ;*************************
      ;widget_control,event.id,get_value=value
      pindex = widget_info(state.panelList, /list_select)
    
      cpanel = (*state.panelObjs)[state.panel_sel]
    endif

    if ~keyword_set(spectra) then begin
      thm_ui_make_default_lineplot, state.info.loadedData, cpanel, $
                                    xnames[i], ynames[i], state.template,$
                                    gui_sbar=state.statusBar
    endif else begin
      
      thm_ui_make_default_specplot, state.info.loadedData, cpanel, $
                                        xnames[i], ynames[i], $
                                        znames[i], state.template,gui_sbar=state.statusBar
    
    endelse
    
  endfor
  ;End loop over names
  ;-------------------
  
  ;all components invalid
  if i-bad eq 0 then return


  ; begin "in case of cancel of child panel" code
    state.cWindow->GetProperty, Panels=panels, nRows=nrows, nCols=ncols;, locked=locked ;locked necessary?
    if ptr_valid(state.panels) then ptr_free, state.panels
    state.panels = ptr_new(panels)

    if ptr_valid(state.panelObjs) then ptr_free, state.panelObjs
    state.panelObjs = ptr_new(*state.panels->Get(/All))
    cpanel = (*state.panelObjs)[state.panel_sel]
  ; end "in case of cancel of child panel" code

    
    cpanel->getProperty, traceSettings=traceSettings
    state.trace_sel = traceSettings->Count() - 1
    
    ; check whether selected trace is line or spectra
    traces = traceSettings->get(/all) ; get traces to check for spectra/line type
    if obj_isa(traces[state.trace_sel], 'thm_ui_spectra_settings') then $
      state.is_trace_spec = 1 else state.is_trace_spec = 0

    state.npanels = n_elements(*state.panelObjs)
    if ptr_valid(state.panelValue) then ptr_free, state.panelValue
    if ptr_valid(state.panel_ValueInfo) then ptr_free, state.panel_ValueInfo
    if ptr_valid(state.panelNames) then ptr_free, state.panelNames

    ; get panel/tracenames and panel layout
    thm_ui_update_panel_list, state=state, panelNames=panelNames, $
       panelValue=panelValue, panel_ValueInfo=panel_ValueInfo, $
       panelLayout=panelLayout, ntr=ntr

    if ptr_valid(state.panelLayout) then ptr_free, state.panelLayout
    state.panelLayout = ptr_new(panelLayout)
    state.panelValue = ptr_new(panelValue)
    state.panel_ValueInfo = ptr_new(panel_ValueInfo)
    state.panelNames = ptr_new(panelNames)
    
        ;state.panel_sel = (*state.panel_ValueInfo)[pindex].panelid
        ;state.trace_sel = (*state.panel_ValueInfo)[pindex].traceid
    
    ; advance selected y-axis dataname to next down the list
    if nnames eq 1 then begin
      ld = state.loadeddata ->getall()
      ldi = where(ld eq (keyword_set(spectra)?znames[0]:ynames[0]),c)
      if c gt 0 then begin
        ;use last index in case parent and child have same name
        if c gt 1 then ldi = ldi[n_elements(ldi)-1]
        next_idx = (long(ldi)+1) mod n_elements(ld)
        next = ld[next_idx]
        if state.loadeddata->ischild(next[0]) && ~state.loadeddata->isparent(next[0]) then begin
          *state.ySelect = next
          state.pritree->clearSelected
          state.pritree->setSelected,next
        endif
      endif
    endif
                   
    widget_control, state.panelList, set_value=*state.panelValue
    ;widget_control, state.panelList, set_list_select=state.panel_sel + ntr
    
    new_trace_ind = where(*state.panelValue eq (*state.panelNames)[state.panel_sel]) $
                      + ntr[state.panel_sel]
    widget_control, state.panelList, set_list_select=new_trace_ind
    widget_control, state.shiftleftButton, sensitive=0
    widget_control, state.shiftrightButton, sensitive=0
    widget_control, state.rowtext, sensitive=0
    widget_control, state.coltext, sensitive=0
    widget_control, state.editButton, sensitive=1
    
    if state.trace_sel eq 0 then widget_control, state.shiftupButton, sensitive=0 $
      else widget_control, state.shiftupButton, sensitive=1
      
    if state.trace_sel eq (ntr[state.panel_sel]-1) then widget_control, state.shiftdownButton, sensitive=0 $
      else widget_control, state.shiftdownButton, sensitive=1
            
    msg = 'Add Finished. '+e_msg
    state.historywin->update, h_msg + msg
    state.statusbar->update, msg

end

;returns selection in x_select,y_select,z_select
;keyword spectra, set to return spectral selection rather than line selection
pro thm_ui_layout_get_selection,state,x_select=x_select,y_select=y_select,z_select=z_select,spectra=spectra

  compile_opt idl2,hidden
  
  x_select = ''
  y_select = ''
  z_select = ''
  failtype = 0

  if state.compviewmode eq 0 then begin
  
    pri_selection = state.priTree->getValue()
  
    if ~keyword_set(pri_selection[0]) then begin
      ok=dialog_message("No variable selected, Unable to add to panel", /information, /center, dialog_parent=state.tlb)
      state.statusBar->update,'Warning: No variable selected'
      return
    endif
  
    ;get line selection when not in component mode
    if ~keyword_set(spectra) then begin
   
      obj_list = 0
      
      for i = 0,n_elements(pri_selection)-1 do begin
        
        if state.loadedData->isParent(pri_selection[i]) then begin
          group = state.loadedData->getGroup(pri_selection[i])
          
          if ~obj_Valid(group[0]) then continue
          
          dataObjects = group->getDataObjects()
          
        endif else begin
        
          dataObjects = state.loadedData->getObjects(name=pri_selection[i])
  
        endelse
         
        if ~obj_valid(dataObjects[0]) then continue
          
        obj_list = array_concat(dataObjects,obj_list)
        
      endfor
  
      for i = 0,n_elements(obj_list)-1 do begin
      
        if ~obj_valid(obj_list[i]) then continue
        
        obj_list[i]->getProperty,name=name,timeName=timeName,indepName=indepName
        
        y_select = array_concat(name,y_select)
        
        if ~keyword_set(indepName) then begin
          x_select = array_concat(timename,x_select)
        endif else begin
          x_select = array_concat(indepName,x_select)
        endelse
        
      endfor
    
    ;get spectra selection when not in component mode
    endif else begin
    
      for i = 0,n_elements(pri_selection)-1 do begin
      
        if state.loadedData->isParent(pri_selection[i]) then begin
          group = state.loadedData->getGroup(pri_selection[i])
          
          if ~obj_Valid(group[0]) then continue
          
          indepName = group->getIndepName()
          timename = group->getTimeName()
          name = group->getName()
          yaxisname = group->getyaxisname()
          
        endif else begin
        
          obj = state.loadedData->getObjects(name=pri_selection[i])
          
          if ~obj_valid(obj[0]) then continue
        
          obj->getProperty,name=name,timename=timename,indepname=indepname,yaxisname=yaxisname
             
        endelse
      
        if ~keyword_set(indepName) then begin
          x_select = array_concat(timename,x_select)
        endif else begin
          x_select = array_concat(indepName,x_select)
        endelse
          
        y_select = array_concat(yaxisname,y_select)
        z_select = array_concat(name,z_select)
      
      endfor
    
    endelse
  endif else begin ;identify selection for component mode
  
    z_selection = state.priTree->getValue()
    y_selection = state.secTree->getValue()
    x_selection = state.terTree->getValue()
    
    if ~keyword_set(x_selection[0]) then begin
       state.statusBar->update,'Warning: No X-variable selected'
       ok=dialog_message("No X-variable selected, Unable to add to panel", /information, /center, dialog_parent=state.tlb)
       return
    endif
    
    if ~keyword_set(y_selection[0]) then begin
      state.statusbar->update,'Warning: No Y-variable selected'
      ok=dialog_message("No Y-variable selected, Unable to add to panel", /information, /center, dialog_parent=state.tlb)
      return
    endif
  
    if ~keyword_set(spectra) then begin
    
      ;allows a one-to-many, many-to-one, or a one-to-one, but not a general m-to-n match
    
      if n_elements(x_selection) ne 1 && $
         n_elements(y_selection) ne 1 && $
         n_elements(x_selection) ne n_elements(y_selection) then begin
         state.statusbar->update,'Warning: Number of X and Y elements do not match'
         ok=dialog_message("Number of X & Y elements do not match, Unable to add to panel", /information, /center, dialog_parent=state.tlb)
         return
       endif 
       
       if n_elements(x_selection) eq 1 && $
          n_elements(y_selection) gt 1 then begin
          
          x_selection = replicate(x_selection,n_elements(y_selection))
          
       endif else if n_elements(y_selection) eq 1 && $
                     n_elements(x_selection) gt 1 then begin
                     
          y_selection = replicate(y_selection,n_elements(y_selection))
         
       endif
          
       for i = 0,n_elements(y_selection)-1 do begin
       
         if state.loadedData->isParent(y_selection[i]) then begin
         
           y_children = state.loadedData->getChildren(y_selection[i])
           x_children = replicate(x_selection[i],n_elements(y_children))
           
         endif else begin
         
           y_children = y_selection[i]
           x_children = x_selection[i]
           
         endelse
       
         x_select = array_concat(x_children,x_select)
         y_select = array_concat(y_children,y_select)
         
       endfor
      
    endif else begin
    
      ;allows a one-to-many, many-to-one, or a one-to-one, but not a general m-to-n match
    
      if ~keyword_set(z_selection[0]) then begin
        state.statusbar->update,'Warning: No Z-variable selected'
        ok=dialog_message("No Z-variable selected, Unable to add to panel", /information, /center, dialog_parent=state.tlb)
        return
      endif
      
      if n_elements(x_selection) ne 1 && $
         n_elements(z_selection) ne 1 && $
         n_elements(x_selection) ne n_elements(z_selection) then begin
        state.statusbar->update,'Warning: Number of X and Z elements do not match'
        ok=dialog_message("Number of X & Z elements do not match, Unable to add to panel", /information, /center, dialog_parent=state.tlb)
        return
      endif 
       
       if n_elements(x_selection) eq 1 && $
          n_elements(z_selection) gt 1 then begin
          
         x_selection = replicate(x_selection,n_elements(z_selection))
          
       endif else if n_elements(z_selection) eq 1 && $
                     n_elements(x_selection) gt 1 then begin
                     
         z_selection = replicate(z_selection,n_elements(x_selection))
         
       endif
           
      if n_elements(y_selection) ne 1 && $
         n_elements(z_selection) ne 1 && $
         n_elements(y_selection) ne n_elements(z_selection) then begin
        state.statusbar->update,'Warning: Number of Y and Z elements do not match'
        ok=dialog_message("Number of Y & Z elements do not match, Unable to add to panel", /information, /center, dialog_parent=state.tlb)
        return
      endif 
       
      if n_elements(y_selection) eq 1 && $
          n_elements(z_selection) gt 1 then begin
          
         y_selection = replicate(y_selection,n_elements(z_selection))
          
       endif else if n_elements(y_selection) eq 1 && $
                     n_elements(z_selection) gt 1 then begin
                     
         z_selection = replicate(z_selection,n_elements(y_selection))
         
       endif  
       
       x_select = x_selection
       y_select = y_selection
       z_select = z_selection
       
       for i = 0,n_elements(z_select)-1 do begin
       
         if (state.loadedData->isParent(z_select[i]) && $
            ~state.loadedData->isParent(y_select[i])) then begin
            
            state.statusbar->update,'WARNING: Z-axis is group-variable, but Y-variable is not. May generate invalid plot'
            
         endif else if (state.loadedData->isParent(y_select[i]) && $
            ~state.loadedData->isParent(z_select[i])) then begin
            
            state.statusbar->update,'WARNING: Y-axis is group-variable, but Z-variable is not. May generate invalid plot'
        
         endif 
       
       endfor
       
    endelse
  
  endelse
  
end

Pro thm_ui_layout_options_event, event

  Compile_Opt idl2, hidden
  
  Widget_Control, event.TOP, Get_UValue=state, /No_Copy

    ;Put a catch here to insure that the state remains defined

  err_xxx = 0
  Catch, err_xxx
  IF (err_xxx NE 0) THEN BEGIN
    Catch, /Cancel
    Help, /Last_Message, Output = err_msg
    if is_struct(state) then begin
      FOR j = 0, N_Elements(err_msg)-1 DO state.historywin->update,err_msg[j]

        value = 'NO UVALUE'
        if widget_valid(event.id) then begin
          widget_control, event.id, get_uvalue=val
          if keyword_set(val) then begin
            value = val
          endif
        endif
          
      ; Store most recent events
       
        state.uvalHist[1:4] = state.uvalHist[0:3]
        state.uvalHist[0] = 'UVALUE: ' + value ; most recent uval
        
        for i=n_elements(state.eventHist)-1,1L,-1 do begin
          *state.eventHist[i] = *state.eventHist[i-1]
        endfor
        *state.eventHist[0] = event
      
      ; Output most recent events
      state.historywin->update, strcompress(string(n_elements(state.uvalHist)), /remove_all) + $
                             ' most recent events in Layout Options (in descending order):'
      for j=0,n_elements(state.uvalHist)-1 do begin
        state.historywin->update, state.uvalHist[j]
        printdat, *state.eventHist[j], output=eventStruct
        for k=0L,n_elements(eventStruct)-1 do state.historywin->update, '   ' + eventStruct[k] 
      endfor
      
      x=state.info.master
      histobj=state.historywin
    endif
    Print, 'Error--See history'
    ok=error_message('An unknown error occured and the window must be restarted. See console for details.',$
                     /noname, /center, title='Error in Plot/Layout Options')
   
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin 
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF


  ; Store most recent events
    widget_control, event.id, get_uvalue=value
    state.uvalHist[1:4] = state.uvalHist[0:3]
    if ~keyword_set(value) then value='NO UVALUE'
    state.uvalHist[0] = 'UVALUE: ' + value ; most recent uval
    
    for i=n_elements(state.eventHist)-1,1L,-1 do begin
      *state.eventHist[i] = *state.eventHist[i-1]
    endfor
    *state.eventHist[0] = event

    ;kill request block

  IF(Tag_Names(event, /Structure_Name) EQ 'WIDGET_KILL_REQUEST') THEN BEGIN  
      state.origWindow->GetProperty, panels=origPanels, nRows=nRows, nCols=nCols, locked=locked
      state.cWindow->SetProperty, panels=origPanels, nRows=nRows, nCols=nCols, locked=locked
       
      thm_ui_layout_draw_update,event.top,state
      
      state.historywin->Update,'THM_UI_LAYOUT_OPTIONS: Active window refreshed.'
      state.statusbar->update,'THM_UI_LAYOUT_OPTIONS: Active window refreshed.'
     
      if obj_valid(state.priTree) then begin
        *state.treeCopyPtr = state.priTree->getCopy()
      endif
     
      Print, 'Layout Options widget cancelled. No changes made.'
      state.historywin->Update, 'Layout Options window cancelled. No changes made.'
      state.statusbar->update, 'Layout Options window cancelled. No changes made.'
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /destroy
      RETURN
  ENDIF

 ;deal with tabs

IF (Tag_Names(event, /Structure_Name) EQ 'WIDGET_TAB') THEN BEGIN  
;  Widget_Control, state.panelDroplists[event.tab], set_droplist_select = state.axispanelselect
  Widget_Control, event.TOP, Set_UValue=state, /No_Copy
  RETURN 
EndIF

   ; Get the instructions from the widget causing the event and
   ; act on them.

  Widget_Control, event.id, Get_UValue=uval

  state.historywin->update,'THM_UI_LAYOUT_OPTIONS: User value: '+uval  ,/dontshow

  CASE uval OF
    'ADDPAN': BEGIN
    
      thm_ui_layout_add_panel,state
     
      ;thm_ui_init_tree_widgets, state=state
      thm_ui_update_var_widget, state=state
      
      ;widget_control, state.editButton, sensitive=0; temp til panel options works
      
    END
    'ADDLINE': BEGIN
      thm_ui_layout_add_trace,event,state 
    END
   'ADDSPEC': BEGIN
      thm_ui_layout_add_trace,event,state,/spectra
    END
    'ADDVAR': BEGIN
      if ~ptr_valid(state.ySelect) then begin
        RETURN
      endif

      if ptr_valid(state.varList) then begin ; Add variables from y-axis list
        varList = *state.varList
        ptr_free, state.varList
        varList = [varList, *state.ySelect]
      endif else varList = *state.ySelect
      state.varList = ptr_new(varList)
      widget_control, state.variableText, set_value=*state.varList
    END
    'APPLY': BEGIN

      ; should apply any changes made to panel layout
      
      ; make sure no panels overlap, reset if so
      if thm_ui_check_overlap(*state.panelobjs, state.cwindow[0]) then break
      state.cWindow->getProperty, locked=locked
      if locked ne -1 then begin
        ok = thm_ui_check_panel_layout(state)
        if ~ok then begin
          ; have to revert to initial state
          ;*state.cWindow->SetProperty, Panels=*state.panelsCopy
          ;ptr_free, state.panels
    
          ;this code should be replaced with a save/reset method
          state.origWindow->GetProperty, panels=origPanels, nRows=nRows, nCols=nCols, locked=locked
          state.cWindow->SetProperty, panels=origPanels, nRows=nRows, nCols=nCols, locked=locked
          state.nRows = nRows
          state.nCols = nCols
           
          state.cWindow->getProperty,settings=page_settings
          
          page_settings->reset
           
          state.info.drawObject->update,state.info.windowStorage,state.info.loadedData
          state.info.drawObject->draw
          state.info.scrollbar->update
    
          if obj_valid(state.priTree) then begin
            *state.treeCopyPtr = state.priTree->getCopy()
          endif
    
          ;Print, 'Layout Options widget cancelled. No changes made.'
          ;state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Layout Options window cancelled. No changes made.'
          Widget_Control, event.TOP, Set_UValue=state, /No_Copy
          Widget_Control, event.top, /destroy
          RETURN
        endif
      endif
      
      ;make sure window shrinks to max rows/cols
      if state.npanels eq 0 then begin
        state.cWindow->SetProperty, nRows=nRows
        state.cWindow->SetProperty, nCols=nCols
      endif else begin
      
;        maxrspan = 1
;        maxcspan = 1
;        
;        for i = 0,state.npanels-1 do begin
;          cpanel = (*state.panelObjs)[i]
;          cpanel->GetProperty, settings=panelSettings
;          panelSettings->GetProperty, rspan=rspan
;          totalspan = rspan + ((*state.panelLayout).row)[i] - 1
;          if totalspan gt maxrspan then maxrspan = totalspan
;        endfor
;        
;        for i = 0,state.npanels-1 do begin
;          cpanel = (*state.panelObjs)[i]
;          cpanel->GetProperty, settings=panelSettings
;          panelSettings->GetProperty, cspan=cspan
;          totalspan = cspan + ((*state.panelLayout).col)[i] - 1
;          if totalspan gt maxcspan then maxcspan = totalspan
;        endfor
;
;        if maxrspan lt state.initNRows then maxrspan = state.initNRows
;        if maxcspan lt state.initNCols then maxcspan = state.initNCols
;        
;        state.cWindow->SetProperty, nRows=maxrspan
;        state.cWindow->SetProperty, nCols=maxcspan

      endelse
      thm_ui_layout_draw_update,event.top,state,/apply
      
      state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Changes applied.'
      state.statusbar->update, 'THM_UI_LAYOUT_OPTIONS: Changes applied.'
    END
    'CANC': BEGIN

      ; have to revert to initial state
;      *state.cWindow->SetProperty, Panels=*state.panelsCopy
;      ptr_free, state.panels

      ;this code should be replaced with a save/reset method
      state.origWindow->GetProperty, panels=origPanels, nRows=nRows, nCols=nCols, locked=locked
      state.cWindow->SetProperty, panels=origPanels, nRows=nRows, nCols=nCols, locked=locked
      state.nRows = nRows
      state.nCols = nCols
       
      state.cWindow->getProperty,settings=page_settings
      
      page_settings->reset
       
      state.info.drawObject->update,state.info.windowStorage,state.info.loadedData
      state.info.drawObject->draw
      state.info.scrollbar->update

      Print, 'Layout Options widget cancelled. No changes made.'
      state.info.statusBar->update,'Plot/Layout Options closed.'
      state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Layout Options window cancelled. No changes made.'
      
      if obj_valid(state.priTree) then begin
        *state.treeCopyPtr = state.priTree->getCopy()
      endif
      
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /destroy
      RETURN
    END
    'EDPAN': BEGIN
      ; edit selected panel
      
      pindex = widget_info(state.panelList, /list_select)
      cpanel_num = ptr_new(state.panel_sel)
      ctr_num = ptr_new(state.trace_sel)
                
      if (*state.panel_ValueInfo)[pindex].ispanel then begin
      ;temporarily comment out until we get panel options working
        state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Calling PANEL OPTIONS panel.'
        state.statusbar->update,'THM_UI_LAYOUT_OPTIONS: Calling PANEL OPTIONS panel.'
        thm_ui_panel_options, state.tlb, state.info.windowStorage, $
                             state.info.loadedData, state.historywin, $
                             state.info.drawObject, state.template,$
                             panel_select=cpanel_num, ctr_num=ctr_num
        state.panel_sel = state.npanels-1
        state.panel_sel = *cpanel_num
        state.trace_sel = -1
        istrace=0
      endif else begin

        if state.is_trace_spec eq 0 then begin
          state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Calling LINE OPTIONS panel.'
          state.statusbar->update, 'THM_UI_LAYOUT_OPTIONS: Calling LINE OPTIONS panel.'
          thm_ui_line_options, state.tlb, state.info.windowStorage, $
                               state.info.loadedData, state.historywin, $
                               state.info.drawObject, state.template,cpanel_num=cpanel_num, $
                               ctr_num=ctr_num

          state.panel_sel = *cpanel_num
          state.trace_sel = *ctr_num
        endif else begin
          state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Calling Z-AXIS OPTIONS panel.'
          state.statusbar->update, 'THM_UI_LAYOUT_OPTIONS: Calling Z-AXIS OPTIONS panel.'
          thm_ui_zaxis_options, state.tlb, state.info.windowStorage, $
                               state.info.zaxisSettings, state.info.drawObject, $
                               state.info.loadedData, state.historywin,state.template;, cpanel_num=cpanel_num
          
;          state.panel_sel = *cpanel_num
;          state.trace_sel = 0
        endelse
        
        istrace=1      
      endelse

      ptr_free, cpanel_num & ptr_free, ctr_num
      
    ; begin "in case of cancel of child panel" code
      state.cWindow->GetProperty, Panels=panels, nRows=nrows, nCols=ncols;, locked=locked ;locked necessary?
      if ptr_valid(state.panels) then ptr_free, state.panels
      state.panels = ptr_new(panels)

      if ptr_valid(state.panelObjs) then ptr_free, state.panelObjs
      state.panelObjs = ptr_new(*state.panels->Get(/All))
    ; end "in case of cancel of child panel" code

    ; update page row/col number in case of panel span change
      state.nrows = nrows
      state.ncols = ncols
      widget_control, state.rowpagetext, set_value=nrows
      widget_control, state.colpagetext, set_value=ncols

      
      state.npanels = n_elements(*state.panelObjs)
      if ptr_valid(state.panelValue) then ptr_free, state.panelValue
      if ptr_valid(state.panel_ValueInfo) then ptr_free, state.panel_ValueInfo
      if ptr_valid(state.panelNames) then ptr_free, state.panelNames
        
      ; get panel/tracenames and panel layout
      thm_ui_update_panel_list, state=state, panelNames=panelNames, $
         panelValue=panelValue, panel_ValueInfo=panel_ValueInfo, $
         panelLayout=panelLayout, ntr=ntr

      if ptr_valid(state.panelLayout) then ptr_free, state.panelLayout
      state.panelLayout = ptr_new(panelLayout)
      state.panelValue = ptr_new(panelValue)
      state.panel_ValueInfo = ptr_new(panel_ValueInfo)
      state.panelNames = ptr_new(panelNames)
      ;state.panel_sel = state.npanels-1

      widget_control, state.panelList, set_value=*state.panelValue
      ;widget_control, state.panelList, set_list_select=state.panel_sel + ntr
      
      new_trace_ind = where(*state.panelValue eq (*state.panelNames)[state.panel_sel])
      widget_control, state.panelList, set_list_select=new_trace_ind

      if istrace then begin
      
        ;make sure correct trace is selected
        new_trace_ind = where(*state.panelValue eq (*state.panelNames)[state.panel_sel]) $
                        + state.trace_sel+1
        widget_control, state.panelList, set_list_select=new_trace_ind
        
        ;make sure widgets are appropriately sensitized
        if state.trace_sel eq 0 then widget_control, state.shiftupButton, sensitive=0 $
          else widget_control, state.shiftupButton, sensitive=1
          
        if state.trace_sel eq (ntr[state.panel_sel]-1) then widget_control, state.shiftdownButton, sensitive=0 $
          else widget_control, state.shiftdownButton, sensitive=1
          
        widget_control, state.rowText, set_value=(*state.panelLayout)[state.panel_sel].row
        widget_control, state.colText, set_value=(*state.panelLayout)[state.panel_sel].col
      
      endif
      ;thm_ui_init_tree_widgets, state=state
      thm_ui_update_var_widget, state=state
    END
    
    'EDVAR': BEGIN
      ; edit selected variable in box     
      pindex = widget_info(state.panelList, /list_select)
      state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Calling VARIABLE OPTIONS panel.'
      state.statusbar->update, 'THM_UI_LAYOUT_OPTIONS: Calling VARIABLE OPTIONS panel.'
      
      guiTree = ptr_new(state.priTree->getCopy()) 
      
      thm_ui_variable_options, state.info.master, state.info.loadeddata, $
                               state.info.windowstorage, state.info.drawObject, $
                               state.historywin, state.template,guiTree,panel_select=state.panel_sel
   
      
      thm_ui_update_var_widget, state=state
    END
    'LOCK_PAN': BEGIN
    
      ;this check prevents an accidental unlock of first panel created that can occur if lock clicked when no panels exist
      if state.npanels gt 0 then begin
        state.cWindow->setProperty, locked=state.panel_sel
      endif

      pindex = widget_info(state.panelList, /list_select)
      
      thm_ui_update_panel_list, state=state, panelNames=panelNames, $
         panelValue=panelValue, panel_ValueInfo=panel_ValueInfo, $
         panelLayout=panelLayout

      widget_control, state.panelList, set_value=panelValue, set_list_select=pindex
      if ptr_valid(state.panelValue) then ptr_free, state.panelValue
      state.panelValue = ptr_new(panelValue)
      if ptr_valid(state.panelNames) then ptr_free, state.panelNames
      state.panelNames = ptr_new(panelNames)
    END
    'UNLOCK_PAN': BEGIN
      state.cWindow->setProperty, locked=-1

      pindex = widget_info(state.panelList, /list_select)
      
      thm_ui_update_panel_list, state=state, panelNames=panelNames, $
         panelValue=panelValue, panel_ValueInfo=panel_ValueInfo, $
         panelLayout=panelLayout

      widget_control, state.panelList, set_value=panelValue, set_list_select=pindex
      if ptr_valid(state.panelValue) then ptr_free, state.panelValue
      state.panelValue = ptr_new(panelValue)
      if ptr_valid(state.panelNames) then ptr_free, state.panelNames
      state.panelNames = ptr_new(panelNames)
    END

    'OK': BEGIN

      ; should apply any changes made and then exit (cancel)

      ; make sure no panels overlap, reset if so
      if thm_ui_check_overlap(*state.panelobjs, state.cwindow[0]) then break
      state.cWindow->getProperty, locked=locked
      if locked ne -1 then begin
        ok = thm_ui_check_panel_layout(state)
        if ~ok then begin
          ; have to revert to initial state
          ;*state.cWindow->SetProperty, Panels=*state.panelsCopy
          ;ptr_free, state.panels
    
          ;this code should be replaced with a save/reset method
          state.origWindow->GetProperty, panels=origPanels, nRows=nRows, nCols=nCols, locked=locked
          state.cWindow->SetProperty, panels=origPanels, nRows=nRows, nCols=nCols, locked=locked
          state.nRows = nRows
          state.nCols = nCols
           
          state.cWindow->getProperty,settings=page_settings
          
          page_settings->reset
           
          state.info.drawObject->update,state.info.windowStorage,state.info.loadedData
          state.info.drawObject->draw
          state.info.scrollbar->update
          
          if obj_valid(state.priTree) then begin
            *state.treeCopyPtr = state.priTree->getCopy()
          endif
    
          ;Print, 'Layout Options widget cancelled. No changes made.'
          ;state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Layout Options window cancelled. No changes made.'
          Widget_Control, event.TOP, Set_UValue=state, /No_Copy
          Widget_Control, event.top, /destroy
          RETURN
        endif
      endif
      
      ;make sure window shrinks to max rows/cols
      if state.npanels eq 0 then begin
        state.cWindow->SetProperty, nRows=nRows
        state.cWindow->SetProperty, nCols=nCols
      endif else begin

      endelse
      
      thm_ui_layout_draw_update,event.top,state
      
      FOR i=0,N_Elements(state.info.dataButtons)-1 DO Widget_Control, state.info.dataButtons[i], /sensitive
      
      Print, 'Layout updated.  Layout Options widget closed.'
      state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Layout updated.  Layout Options widget closed.'
      if obj_valid(state.priTree) then begin
        *state.treeCopyPtr = state.priTree->getCopy()
      endif
      
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    END
    'PAGE_COL': BEGIN
      if event.valid then begin
        
        maxcspan = 1
      
        for i = 0,state.npanels-1 do begin
          cpanel = (*state.panelObjs)[i]
          cpanel->GetProperty, settings=panelSettings
          panelSettings->GetProperty, cspan=cspan
          totalspan = cspan + ((*state.panelLayout).col)[i] - 1
          if totalspan gt maxcspan then maxcspan = totalspan
        endfor
  
        if event.value lt maxcspan then begin
          widget_control, event.id, set_value=maxcspan
          state.cWindow->setProperty, ncols=maxcspan
          state.nCols = maxcspan
        endif else begin
          state.cWindow->setProperty, ncols=event.value
          state.nCols = event.value
        endelse
      endif
      
    END
    'PAGE_ROW': BEGIN
      if event.valid then begin
  
        maxrspan = 1
        
        for i = 0,state.npanels-1 do begin
          cpanel = (*state.panelObjs)[i]
          cpanel->GetProperty, settings=panelSettings
          panelSettings->GetProperty, rspan=rspan
          totalspan = rspan + ((*state.panelLayout).row)[i] - 1
          if totalspan gt maxrspan then maxrspan = totalspan
        endfor
        
        if event.value lt maxrspan then begin
          widget_control, event.id, set_value=maxrspan
          state.cWindow->setProperty, nrows=maxrspan
          state.nRows = maxrspan
        endif else begin
          state.cWindow->setProperty, nrows=event.value
          state.nRows = event.value
        endelse
      endif

    END
    'PANEL': BEGIN
      ; show list of panels
      
      if event.clicks eq 1 then begin
        ;state.panel_sel = widget_info(state.panelList, /list_select)
        pindex = widget_info(state.panelList, /list_select)
        
        if (*state.panel_ValueInfo)[pindex].ispanel then begin
        widget_control, state.editButton, sensitive=1; temp til panel options works
          
          widget_control, state.rowText, /sensitive
          widget_control, state.colText, /sensitive
          widget_control, state.shiftupButton, /sensitive
          widget_control, state.shiftdownButton, /sensitive
          widget_control, state.shiftleftButton, /sensitive
          widget_control, state.shiftrightButton, /sensitive
          state.trace_sel = -1 
        
          state.panel_sel = where((*state.panelNames) eq (*state.panelValue)[pindex])
          widget_control, state.rowText, set_value=(*state.panelLayout)[state.panel_sel].row
          widget_control, state.colText, set_value=(*state.panelLayout)[state.panel_sel].col
          
          
          ; get variables for current panel
          cpanel = (*state.panelObjs)[state.panel_sel]
          cpanel->getProperty, variables=variablescontainer
          variablesobjects=variablescontainer->get(/all)
          if obj_valid(variablesobjects[0]) then begin
          
            variablesobjects[0]->GetProperty,text=textobject
            textobject->GetProperty,value=ctextvalues
            nvo= n_elements(variablesobjects)
            
            if nvo gt 1 then begin
              for j=1,nvo-1 do begin
                variablesobjects[j]->GetProperty,text=textobject
                textobject->GetProperty,value=ctextvalue
                ctextvalues=[ctextvalues, ctextvalue]
              endfor
            endif
          endif else ctextvalues=['']
          
          widget_control, state.variableText, set_value=ctextvalues ;*state.varList
        
        endif else begin
        ;widget_control, state.editButton, sensitive=1; temp til panel options works
          panelids = (*state.panel_ValueInfo).panelid
          panelids = panelids[uniq(panelids)]
          state.panel_sel = where(panelids eq (*state.panel_ValueInfo)[pindex].panelid)

          cpanel = (*state.panelObjs)[state.panel_sel]
          cpanel->GetProperty, tracesettings=tracesettings
          ntraces = tracesettings->Count()
          traces = traceSettings->get(/all) ; get traces to check for spectra/line type
          
          widget_control, state.rowText, sensitive=0
          widget_control, state.colText, sensitive=0
          widget_control, state.shiftleftButton, sensitive=0
          widget_control, state.shiftrightButton, sensitive=0
                    
          state.trace_sel = (*state.panel_ValueInfo)[pindex].traceid
          
          ; check whether selected trace is line or spectra
          if obj_isa(traces[state.trace_sel], 'thm_ui_spectra_settings') then $
            state.is_trace_spec = 1 else state.is_trace_spec = 0

          if state.trace_sel eq 0 then widget_control, state.shiftupButton, sensitive=0 $
            else widget_control, state.shiftupButton, sensitive=1
            
          if state.trace_sel eq (ntraces-1) then widget_control, state.shiftdownButton, sensitive=0 $
            else widget_control, state.shiftdownButton, sensitive=1

          widget_control, state.rowText, set_value=(*state.panelLayout)[state.panel_sel].row
          widget_control, state.colText, set_value=(*state.panelLayout)[state.panel_sel].col

        endelse
      endif
      ;thm_ui_init_tree_widgets, state=state
      thm_ui_update_var_widget, state=state
    END
    'PAN_COL': BEGIN
      if event.valid then begin
        value = event.value
        ;pindex = widget_info(state.panelList, /list_select)

        if value lt 1 then begin ; make sure col lt 1 isn't acceptable
          value = ((*state.panelLayout).col)[state.panel_sel]
          widget_control,event.id,set_value=value
        endif else begin
        
          cpanel = (*state.panelObjs)[state.panel_sel]
          
          cpanel->GetProperty, settings=panelSettings
          panelSettings->GetProperty, cspan=cspan
          
          cpanel->SetLayoutStructure,col=value
  
          if (value + cspan - 1) gt state.nCols then begin
            state.nCols = value + cspan - 1
            state.cWindow->SetProperty, nCols=state.nCols
            id = widget_info(state.tlb, find_by_uname='page_col')
            widget_control, id, set_value=state.nCols
          endif
  
    
          if ptr_valid(state.panelObjs) then ptr_free, state.panelObjs
          state.panelObjs = ptr_new(*state.panels->Get(/All))
          
          state.npanels = n_elements(*state.panelObjs)
          if ptr_valid(state.panelValue) then ptr_free, state.panelValue
          if ptr_valid(state.panel_ValueInfo) then ptr_free, state.panel_ValueInfo
          if ptr_valid(state.panelNames) then ptr_free, state.panelNames
    
          ; get panel/tracenames and panel layout
          thm_ui_update_panel_list, state=state, panelNames=panelNames, $
             panelValue=panelValue, panel_ValueInfo=panel_ValueInfo, $
             panelLayout=panelLayout
  
          if ptr_valid(state.panelLayout) then ptr_free, state.panelLayout
          state.panelLayout = ptr_new(panelLayout)
          state.panelValue = ptr_new(panelValue)
          state.panel_ValueInfo = ptr_new(panel_ValueInfo)
          state.panelNames = ptr_new(panelNames)
          widget_control, state.panelList, set_value=*state.panelValue
          widget_control, state.panelList, $
                          set_list_select=where((*state.panelNames)[state.panel_sel] eq *state.panelValue)
    
                
          state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Column set to ' + $
                                          strcompress(string(uint(value)),/remove_all) + '.'
          state.statusbar->update,'THM_UI_LAYOUT_OPTIONS: Column set to ' + $
                                          strcompress(string(uint(value)),/remove_all) + '.'
        endelse
      endif
    END
    'PAN_DOWN': BEGIN
      
      pindex = state.panel_sel
      
      ; shift selected panel down
      if state.trace_sel eq -1 then begin
          
        rowvalue = ((*state.panelLayout).row)[pindex] + 1
        
        cpanel = (*state.panelObjs)[pindex]
        
        cpanel->GetProperty, settings=panelSettings
        panelSettings->GetProperty, rspan=rspan
              
        cpanel->SetLayoutStructure,row=rowvalue
        
        if (rowvalue + rspan - 1) gt state.nRows then begin
          state.nRows = rowvalue + rspan - 1
          state.cWindow->SetProperty, nRows=state.nRows
          id = widget_info(state.tlb, find_by_uname='page_row')
          widget_control, id, set_value=state.nRows
        endif
        
  
      endif else begin
      ; shift selected trace down
      
        cpanel = (*state.panelObjs)[pindex]
        
        cpanel->GetProperty, tracesettings=traceSettings, xaxis=xaxisSettings, $
                             yaxis=yaxisSettings
        xaxisSettings->GetProperty, labels=xlabels
        yaxisSettings->GetProperty, labels=ylabels
        
        ntraces = tracesettings->count()
        
        if state.trace_sel lt ntraces - 1 then begin
          tracesettings->Move, state.trace_sel, state.trace_sel+1
          xlabels->Move, state.trace_sel, state.trace_sel+1
          ylabels->Move, state.trace_sel, state.trace_sel+1
          
          xaxisSettings->SetProperty, labels=xlabels
          yaxisSettings->SetProperty, labels=ylabels
            
          state.trace_sel = state.trace_sel+1
          cpanel->SetProperty, tracesettings=traceSettings, xaxis=xaxisSettings, $
                               yaxis=yaxisSettings
        endif
      endelse
  
  
      if ptr_valid(state.panelObjs) then ptr_free, state.panelObjs
      state.panelObjs = ptr_new(*state.panels->Get(/All))
      
      state.npanels = n_elements(*state.panelObjs)
      if ptr_valid(state.panelValue) then ptr_free, state.panelValue
      if ptr_valid(state.panel_ValueInfo) then ptr_free, state.panel_ValueInfo
      if ptr_valid(state.panelNames) then ptr_free, state.panelNames

      ; get panel/tracenames and panel layout
      thm_ui_update_panel_list, state=state, panelNames=panelNames, $
         panelValue=panelValue, panel_ValueInfo=panel_ValueInfo, $
         panelLayout=panelLayout, ntr=ntr

      if ptr_valid(state.panelLayout) then ptr_free, state.panelLayout
      state.panelLayout = ptr_new(panelLayout)
      state.panelValue = ptr_new(panelValue)
      state.panel_ValueInfo = ptr_new(panel_ValueInfo)
      state.panelNames = ptr_new(panelNames)
      widget_control, state.rowText, set_value=rowvalue
      widget_control, state.panelList, set_value=*state.panelValue

      if state.trace_sel eq -1 then begin
        widget_control, state.panelList, $
                        set_list_select=where((*state.panelNames)[state.panel_sel] eq *state.panelValue)
        
        state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Column set to ' + $
                                        strcompress(string(uint(rowvalue)),/remove_all) + '.'
        state.statusbar->Update, 'THM_UI_LAYOUT_OPTIONS: Column set to ' + $
                                        strcompress(string(uint(rowvalue)),/remove_all) + '.'              
      endif else begin

        new_trace_ind = where(*state.panelValue eq (*state.panelNames)[state.panel_sel]) $
                          + state.trace_sel + 1
        widget_control, state.panelList, set_list_select=new_trace_ind        
        state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Trace position moved down one position.'
        state.statusbar->Update, 'THM_UI_LAYOUT_OPTIONS: Trace position moved down one position.'
        if state.trace_sel eq 0 then widget_control, state.shiftupButton, sensitive=0 $
          else widget_control, state.shiftupButton, sensitive=1
          
        if state.trace_sel eq (ntr[state.panel_sel]-1) then widget_control, state.shiftdownButton, sensitive=0 $
          else widget_control, state.shiftdownButton, sensitive=1

      endelse

    END
    'PAN_LEFT': BEGIN
      ; shift selected panel left
      
      pindex = state.panel_sel
      
      if ((*state.panelLayout).col)[pindex] eq 1 then begin
      endif else begin
        
        value = ((*state.panelLayout).col)[pindex] - 1
        
        cpanel = (*state.panelObjs)[pindex]
        cpanel->SetLayoutStructure,col=value
  
        if ptr_valid(state.panelObjs) then ptr_free, state.panelObjs
        state.panelObjs = ptr_new(*state.panels->Get(/All))
        
        state.npanels = n_elements(*state.panelObjs)
        if ptr_valid(state.panelValue) then ptr_free, state.panelValue
        if ptr_valid(state.panel_ValueInfo) then ptr_free, state.panel_ValueInfo
        if ptr_valid(state.panelNames) then ptr_free, state.panelNames
  
        ; get panel/tracenames and panel layout
        thm_ui_update_panel_list, state=state, panelNames=panelNames, $
           panelValue=panelValue, panel_ValueInfo=panel_ValueInfo, $
           panelLayout=panelLayout

        if ptr_valid(state.panelLayout) then ptr_free, state.panelLayout
        state.panelLayout = ptr_new(panelLayout)
        state.panelValue = ptr_new(panelValue)
        state.panel_ValueInfo = ptr_new(panel_ValueInfo)
        state.panelNames = ptr_new(panelNames)
        widget_control, state.colText, set_value=value
        widget_control, state.panelList, set_value=*state.panelValue
        widget_control, state.panelList, $
                        set_list_select=where((*state.panelNames)[state.panel_sel] eq *state.panelValue)
              
        state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Column set to ' + $
                                       strcompress(string(uint(value)),/remove_all) + '.'
        state.statusbar->Update, 'THM_UI_LAYOUT_OPTIONS: Column set to ' + $
                                       strcompress(string(uint(value)),/remove_all) + '.'                       
      endelse
    END
    'PAN_RIGHT': BEGIN
      ; shift selected panel right

      pindex = state.panel_sel
 
      value = ((*state.panelLayout).col)[pindex] + 1
      
      cpanel = (*state.panelObjs)[pindex]
      
      cpanel->GetProperty, settings=panelSettings
      panelSettings->GetProperty, cspan=cspan
      
      cpanel->SetLayoutStructure,col=value
      
      if (value + cspan - 1) gt state.nCols then begin
        state.nCols = value + cspan - 1
        state.cWindow->SetProperty, nCols=state.nCols
        id = widget_info(state.tlb, find_by_uname='page_col')
        widget_control, id, set_value=state.nCols
      endif

      if ptr_valid(state.panelObjs) then ptr_free, state.panelObjs
      state.panelObjs = ptr_new(*state.panels->Get(/All))
      
      state.npanels = n_elements(*state.panelObjs)
      if ptr_valid(state.panelValue) then ptr_free, state.panelValue
      if ptr_valid(state.panel_ValueInfo) then ptr_free, state.panel_ValueInfo
      if ptr_valid(state.panelNames) then ptr_free, state.panelNames

      ; get panel/tracenames and panel layout
        thm_ui_update_panel_list, state=state, panelNames=panelNames, $
           panelValue=panelValue, panel_ValueInfo=panel_ValueInfo, $
           panelLayout=panelLayout

      if ptr_valid(state.panelLayout) then ptr_free, state.panelLayout
      state.panelLayout = ptr_new(panelLayout)
      state.panelValue = ptr_new(panelValue)
      state.panel_ValueInfo = ptr_new(panel_ValueInfo)
      state.panelNames = ptr_new(panelNames)
      widget_control, state.colText, set_value=value
      widget_control, state.panelList, set_value=*state.panelValue
      widget_control, state.panelList, $
                      set_list_select=where((*state.panelNames)[state.panel_sel] eq *state.panelValue)
            
      state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Column set to ' + $
                                     strcompress(string(uint(value)),/remove_all) + '.'
      state.statusbar->Update, 'THM_UI_LAYOUT_OPTIONS: Column set to ' + $
                                     strcompress(string(uint(value)),/remove_all) + '.'
    END
    'PAN_ROW': BEGIN
      ; change row of selected panel
      if event.valid then begin
        value = event.value
        ;pindex = widget_info(state.panelList, /list_select)
        
        if value lt 1 then begin ; make sure row lt 1 isn't acceptable
          value = ((*state.panelLayout).row)[state.panel_sel]
          widget_control,event.id,set_value=value
        endif else begin
        
          cpanel = (*state.panelObjs)[state.panel_sel]
          
          cpanel->GetProperty, settings=panelSettings
          panelSettings->GetProperty, rspan=rspan
          
          cpanel->SetLayoutStructure,row=value
  
          if (value + rspan - 1) gt state.nRows then begin
            state.nRows = value + rspan - 1
            state.cWindow->SetProperty, nRows=state.nRows
            id = widget_info(state.tlb, find_by_uname='page_row')
            widget_control, id, set_value=state.nRows
          endif
          
          if ptr_valid(state.panelObjs) then ptr_free, state.panelObjs
          state.panelObjs = ptr_new(*state.panels->Get(/All))
          
          state.npanels = n_elements(*state.panelObjs)
          if ptr_valid(state.panelValue) then ptr_free, state.panelValue
          if ptr_valid(state.panel_ValueInfo) then ptr_free, state.panel_ValueInfo
          if ptr_valid(state.panelNames) then ptr_free, state.panelNames
    
          ; get panel/tracenames and panel layout
          thm_ui_update_panel_list, state=state, panelNames=panelNames, $
             panelValue=panelValue, panel_ValueInfo=panel_ValueInfo, $
             panelLayout=panelLayout
  
          if ptr_valid(state.panelLayout) then ptr_free, state.panelLayout
          state.panelLayout = ptr_new(panelLayout)
          state.panelValue = ptr_new(panelValue)
          state.panel_ValueInfo = ptr_new(panel_ValueInfo)
          state.panelNames = ptr_new(panelNames)
          widget_control, state.panelList, set_value=*state.panelValue
          widget_control, state.panelList, $
                          set_list_select=where((*state.panelNames)[state.panel_sel] eq *state.panelValue)
    
          state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Row set to ' + $
                                         strcompress(string(uint(value)),/remove_all) + '.'
          state.statusbar->Update, 'THM_UI_LAYOUT_OPTIONS: Row set to ' + $
                                         strcompress(string(uint(value)),/remove_all) + '.'
        endelse
      endif     
    END
    'PAN_UP': BEGIN
      ; shift selected panel up
      
      pindex = state.panel_sel
      
      if (((*state.panelLayout).row)[pindex] eq 1) AND (state.trace_sel eq -1) then begin
      endif else begin
        
        ; shift selected panel up
        if state.trace_sel eq -1 then begin
        
          rowvalue = ((*state.panelLayout).row)[pindex] - 1
          
          cpanel = (*state.panelObjs)[pindex]
          cpanel->SetLayoutStructure,row=rowvalue
        
        endif else begin
        ; shift selected trace up
          
          cpanel = (*state.panelObjs)[pindex]
          
          cpanel->GetProperty, tracesettings=traceSettings, xaxis=xaxisSettings, $
                               yaxis=yaxisSettings
          xaxisSettings->GetProperty, labels=xlabels
          yaxisSettings->GetProperty, labels=ylabels
          
          ;ntraces = tracesettings->count()
          
          if state.trace_sel gt 0 then begin
            tracesettings->Move, state.trace_sel, state.trace_sel-1
            xlabels->Move, state.trace_sel, state.trace_sel-1
            ylabels->Move, state.trace_sel, state.trace_sel-1
            
            xaxisSettings->SetProperty, labels=xlabels
            yaxisSettings->SetProperty, labels=ylabels
            
            state.trace_sel = state.trace_sel-1
            cpanel->SetProperty, tracesettings=traceSettings, xaxis=xaxisSettings, $
                               yaxis=yaxisSettings
          endif
        endelse
  
        if ptr_valid(state.panelObjs) then ptr_free, state.panelObjs
        state.panelObjs = ptr_new(*state.panels->Get(/All))
        
        state.npanels = n_elements(*state.panelObjs)
        if ptr_valid(state.panelValue) then ptr_free, state.panelValue
        if ptr_valid(state.panel_ValueInfo) then ptr_free, state.panel_ValueInfo
        if ptr_valid(state.panelNames) then ptr_free, state.panelNames
        
        ; get panel/tracenames and panel layout
        thm_ui_update_panel_list, state=state, panelNames=panelNames, $
           panelValue=panelValue, panel_ValueInfo=panel_ValueInfo, $
           panelLayout=panelLayout, ntr=ntr
  
        if ptr_valid(state.panelLayout) then ptr_free, state.panelLayout
        state.panelLayout = ptr_new(panelLayout)
        state.panelValue = ptr_new(panelValue)
        state.panel_ValueInfo = ptr_new(panel_ValueInfo)
        state.panelNames = ptr_new(panelNames)
        widget_control, state.rowText, set_value=rowvalue
        widget_control, state.panelList, set_value=*state.panelValue
        
        if state.trace_sel eq -1 then begin
          widget_control, state.panelList, $
                          set_list_select=where((*state.panelNames)[state.panel_sel] eq *state.panelValue)
                
          state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Column set to ' + $
                                          strcompress(string(uint(rowvalue)),/remove_all) + '.'
          state.statusbar->Update, 'THM_UI_LAYOUT_OPTIONS: Column set to ' + $
                                          strcompress(string(uint(rowvalue)),/remove_all) + '.'
        endif else begin

          new_trace_ind = where(*state.panelValue eq (*state.panelNames)[state.panel_sel]) $
                            + state.trace_sel + 1
          widget_control, state.panelList, set_list_select=new_trace_ind        
          state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Trace position moved up one position.'
          state.statusbar->Update, 'THM_UI_LAYOUT_OPTIONS: Trace position moved up one position.'
          if state.trace_sel eq 0 then widget_control, state.shiftupButton, sensitive=0 $
            else widget_control, state.shiftupButton, sensitive=1
            
          if state.trace_sel eq (ntr[state.panel_sel]-1) then widget_control, state.shiftdownButton, sensitive=0 $
            else widget_control, state.shiftdownButton, sensitive=1

        endelse
      endelse
    END
    'REMPAN': BEGIN ;This is a misnomer, actually removes panel or trace

      pindex = state.panel_sel
      remlocked=0
      
      ;remove panel
      if state.trace_sel eq -1 then begin
        
        ; detect if panel that was removed is locked
        state.cWindow->GetProperty, locked=locked
        if state.panel_sel eq locked then begin
          remlocked=1
        endif else begin
          if locked gt state.panel_sel then locked=locked-1
        endelse
        
        *state.panels->Remove, (*state.panelObjs)[pindex] ; cpanel
        wastrace=0
        state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Removed Panel ' + $
                                       strcompress(string(state.panel_sel+1),/remove_all)
        state.statusbar->Update, 'THM_UI_LAYOUT_OPTIONS: Removed Panel ' + $
                                       strcompress(string(state.panel_sel+1),/remove_all)
      endif else begin ;Remove trace
          
        cpanel = (*state.panelObjs)[pindex]
        
        cpanel->GetProperty, tracesettings=traceSettings, xaxis=xaxissettings, $
                             yaxis=yaxissettings, zaxis=zaxissettings
        xaxissettings->GetProperty, labels=xlabelsObj
        yaxissettings->GetProperty, labels=ylabelsObj
        
        tracesettings->Remove, position=state.trace_sel
        xlabelsObj->Remove, position=state.trace_sel
        ylabelsObj->Remove, position=state.trace_sel      
        
        cpanel->SetProperty, tracesettings=traceSettings
        orig_trace_sel = state.trace_sel
        
        ntraces = tracesettings->count()
        
        ; check to see if any spectra exist, if not, then reset z-axis
        if ntraces eq 0 then begin
          state.trace_sel=-1
          wastrace = 1
          ;if zaxis exists then get rid of it
          if obj_valid(zaxissettings) then zaxissettings->setProperty,placement=4,touchedPlacement=0
        endif else begin
        
          traces = traceSettings->get(/all)
          isspec = 0
          
          for i=0,ntraces-1 do begin
            if obj_isa(traces[i],'thm_ui_spectra_settings') then isspec=1
          endfor
          
          if ~isspec then begin
            if obj_valid(zaxissettings) then zaxissettings->setProperty,placement=4,touchedPlacement=0
          endif
        endelse
        
        if state.trace_sel gt (ntraces-1) then state.trace_sel = ntraces-1
        
        state.historywin->Update, 'THM_UI_LAYOUT_OPTIONS: Removed trace ' + $
                               strcompress(string(orig_trace_sel+1),/remove_all) $
                               + ' from Panel ' + strcompress(string(state.panel_sel+1),/remove_all)
        state.statusBar->update, 'THM_UI_LAYOUT_OPTIONS: Removed trace ' + $
                               strcompress(string(orig_trace_sel+1),/remove_all) $
                               + ' from Panel ' + strcompress(string(state.panel_sel+1),/remove_all)
      endelse
      
    ; begin "in case of cancel of child panel" code
      state.cWindow->GetProperty, Panels=panels, nRows=nrows, nCols=ncols;, locked=locked ;locked necessary?
      if ptr_valid(state.panels) then ptr_free, state.panels
      state.panels = ptr_new(panels)
    ; end "in case of cancel of child panel" code

      if ptr_valid(state.panelObjs) then ptr_free, state.panelObjs
      state.panelObjs = ptr_new(*state.panels->Get(/All))
      
      if obj_valid((*state.panelObjs)[0]) then begin
      
        state.npanels = n_elements(*state.panelObjs)
        if ptr_valid(state.panelValue) then ptr_free, state.panelValue
        if ptr_valid(state.panel_ValueInfo) then ptr_free, state.panel_ValueInfo
        if ptr_valid(state.panelNames) then ptr_free, state.panelNames
  
        ; if removed panel was locked, then set last panel to be locked
        if remlocked then begin
          state.cwindow->SetProperty, locked=state.npanels-1
        endif else begin
          state.cwindow->SetProperty, locked=locked
        endelse
        
        ; get panel/tracenames and panel layout
        thm_ui_update_panel_list, state=state, panelNames=panelNames, $
           panelValue=panelValue, panel_ValueInfo=panel_ValueInfo, $
           panelLayout=panelLayout, ntr=ntr
        
        if ptr_valid(state.panelLayout) then ptr_free, state.panelLayout
        state.panelLayout = ptr_new(panelLayout)
        state.panelValue = ptr_new(panelValue)
        state.panel_ValueInfo = ptr_new(panel_ValueInfo)
        state.panelNames = ptr_new(panelNames)
        ;state.panel_sel = state.npanels-1
        widget_control, state.panelList, set_value=*state.panelValue

        if state.trace_sel eq -1 then begin
          
          if ~wastrace then state.panel_sel = state.npanels-1
          
          rowvalue = ((*state.panelLayout).row)[state.panel_sel]
          colvalue = ((*state.panelLayout).col)[state.panel_sel]
        
          widget_control, state.panelList, $
                          set_list_select=where((*state.panelNames)[state.panel_sel] eq *state.panelValue)
          widget_control, state.shiftleftButton, sensitive=1
          widget_control, state.shiftrightButton, sensitive=1
          widget_control, state.shiftupButton, sensitive=1
          widget_control, state.shiftdownButton, sensitive=1
          widget_control, state.rowtext, set_value=rowvalue, sensitive=1
          widget_control, state.coltext, set_value=colvalue, sensitive=1

        endif else begin

          rowvalue = ((*state.panelLayout).row)[state.panel_sel]
          colvalue = ((*state.panelLayout).col)[state.panel_sel]
          
          new_trace_ind = where(*state.panelValue eq (*state.panelNames)[state.panel_sel]) $
                            + state.trace_sel + 1
          
          widget_control, state.panelList, set_list_select=new_trace_ind        
  
          if state.trace_sel eq 0 then widget_control, state.shiftupButton, sensitive=0 $
            else widget_control, state.shiftupButton, sensitive=1
            
          if state.trace_sel eq (ntr[state.panel_sel]-1) then widget_control, state.shiftdownButton, sensitive=0 $
            else widget_control, state.shiftdownButton, sensitive=1
        endelse
        
      endif else begin

        if ptr_valid(state.panelLayout) then ptr_free, state.panelLayout
        state.npanels = 0
        state.panelLayout = ptr_new()
        state.panelValue = ptr_new()
        state.panel_ValueInfo = ptr_new()
        state.panelNames = ptr_new()
        state.panel_sel = state.npanels-1
        widget_control, state.panelList, set_value='';*state.panelValue

        widget_control, state.shiftleftButton, sensitive=0
        widget_control, state.shiftrightButton, sensitive=0
        widget_control, state.shiftupButton, sensitive=0
        widget_control, state.shiftdownButton, sensitive=0
        widget_control, state.rowtext, sensitive=0
        widget_control, state.coltext, sensitive=0
        id = widget_info(state.tlb, find_by_uname='rempan')
        widget_control, id, sensitive=0
        widget_control, state.editButton, sensitive=0

      endelse
      thm_ui_update_var_widget, state=state
;      thm_ui_init_tree_widgets, state=state ; no longer needed, was only for updating var widget?
    END
    'REMVAR': BEGIN
      ; remove variables from variable box
    END
    'SHOWCOMP': BEGIN
            
      if widget_info(state.priBase, /valid_id) then begin
        id = widget_info(state.tlb, find_by_uname='pritree')
        widget_control, id, get_value=val
        state.pritree_copy = val->getCopy()
      endif    
      
      ; copy x-axis tree
      if widget_info(state.terBase, /valid_id) then begin
        id = widget_info(state.tlb, find_by_uname='tertree')
        widget_control, id, get_value=val
        state.xtree_copy = val->getCopy()
      endif
      
      ;copy y-axis tree from spec-component mode
      if state.compviewmode then begin
        id = widget_info(state.tlb, find_by_uname='sectree')
        widget_control, id, get_value=val
        state.secTree_copy = val->getCopy()
      endif
       
      state.compviewmode = event.select
      
      thm_ui_init_tree_widgets, state=state
    END
    'VARDOWN': BEGIN
      ; move variable down in box
    END
    'VAR_LIST': BEGIN
      if event.clicks eq 1 then begin
        pindex = widget_info(state.variableText, /list_select)
      endif else begin
        pindex = widget_info(state.variableText, /list_select)
      endelse
    END
    'VARUP': BEGIN
      ; move variable up in box
    END
    Else:
  ENDCase

  Widget_Control, event.TOP, Set_UValue=state, /No_Copy
  
RETURN
END ;--------------------------------------------------------------------------------



PRO thm_ui_layout_options, info

  compile_opt idl2

  windowStorage = info.windowStorage
  zaxisSettings = info.zaxisSettings
  loadedData = info.loadedData
  drawObject = info.drawObject
  
  screen_size = get_screen_size()
  tree_size = min([350,floor((screen_size[0]/4.5))])
  
  def_colors = [[0,0,0],[255,0,0],[0,255,0],[0,0,255],[110,110,110]]
  lockPrefix = '(L)  '
  isspec = 0

    ; top level and base widgets

  ; in case of error
  err_xxx = 0
  Catch, err_xxx
  IF (err_xxx Ne 0) THEN BEGIN
    Catch, /Cancel
    Help, /Last_Message, Output=err_msg
    FOR j = 0, N_Elements(err_msg)-1 DO begin
      Print, err_msg[j]
      info.historywin->update,err_msg[j]
    endfor
    Print, 'Error in Layout Panel--See history'
    
    Widget_Control,tlb,/destroy
    ok = error_message('An unknown error occured while starting Plot/Layout Options', $
                       traceback=0, /center, /noname, title='Error in Plot/Layout Options')
    thm_gui_error, info.master, info.historywin
    RETURN
  ENDIF
    
  tlb = Widget_Base(/Col, Title='THEMIS: Plot/Layout Options', Group_Leader=info.master, $
                    /Modal, /Floating, /tlb_kill_request_events) 

  toprowBase = Widget_Base(tlb, /Row, /Align_Left, space=200)
    traceBase = Widget_Base(toprowBase, /Row, /NonExclusive, /Align_Left)
    titleBase = Widget_Base(toprowBase, /Row, /Align_Center)
  topBase = Widget_Base(tlb, /Row, /Align_Center)
    dataBase = Widget_Base(topBase, /Row, /Align_Center)
      tabBase = Widget_Tab(dataBase, location=0)
        priBase = Widget_Base(tabBase, Title='Dependent Variable', /Col) ; primary base
      plusBase = Widget_Base(topBase, /Col, YPad=125)
    rightBase = Widget_Base(topBase, /Row)
      panelvarBase = Widget_Base(rightbase, /Col, /align_bottom)
        panelBase = Widget_Base(panelvarBase, /Row)
        variableBase=Widget_Base(panelvarBase, /Row)
          varTextBoxBase = Widget_Base(variableBase, /Row)
          varButtonsBase = Widget_Base(variableBase, /Col)
            varLabelBase = Widget_Base(varButtonsBase, /Row)
      topButtonBase = Widget_Base(rightBase, /Col)
        tbuttonBase = Widget_Base(topButtonBase, /Align_Center, /Col)
        arrowBase = Widget_Base(topButtonBase, /Align_Center, /Row)
          leftArrowBase = Widget_Base(arrowBase, /Align_Center)
          updownArrowBase = Widget_Base(arrowBase, /Align_Center, /Col)
          rightArrowBase = Widget_Base(arrowBase, /Align_Center)
        rcTextBase = Widget_Base(topButtonBase, /Align_Center, /Col)
          rowTextBase = Widget_Base(rcTextBase, /Align_Center, /Col)
          colTextBase = Widget_Base(rcTextBase, /Align_Center, /Col)
          rowPageTextBase = Widget_Base(rcTextBase, /Align_Center, /Col)
          colPageTextBase = Widget_Base(rcTextBase, /Align_Center, /Col)
  buttonBase = Widget_Base(tlb, /Row, /Align_Center, YPad=5) 
  statusBase = Widget_Base(tlb, /Row, /Align_Center, YPad=5) 

      ;retrieve data and panel info for display
  ;dataNames = info.loadedData->GetAll()
  dataNames = info.loadedData->GetAll(/child)
  IF is_num(dataNames) THEN dataNames=''
  cWindow = info.windowStorage->GetActive()
  origWindow = cWindow->Copy()
  IF NOT Obj_Valid(cWindow) THEN BEGIN
     panelNames=[''] 
  ENDIF ELSE BEGIN
     cWindow->GetProperty, Panels=panels, nRows=nRows, nCols=nCols, locked=locked
     initNRows = nRows
     initNCols = nCols
;     cwindow->GetProperty, variables=variablescontainer
;     variablesobjects = variablescontainer->get(/all)
     IF NOT Obj_Valid(panels) THEN panelObjs=[''] ELSE panelObjs = panels->Get(/All)
  ENDELSE


  if obj_valid(panelObjs[0]) then begin
  
    npanels = n_elements(panelObjs)
    panelNames = strarr(npanels)
  
    for i = 0,npanels-1 do begin ; loop over panels
    
      cPanel = panelObjs[i]
      
      panelNames[i] = cPanel->constructPanelName()
      
      cwindow->GetProperty, locked=locked
      if i eq locked then panelNames[i] = lockPrefix + panelNames[i]
      
      if i eq 0 then panelValue = panelNames[i] $
        else panelValue = [panelValue, panelNames[i]]
        
      if i eq 0 then panelLayout = cPanel->getLayoutStructure() $
        else panelLayout = [panelLayout, cPanel->getLayoutStructure()]
      
      if i eq 0 then panel_ValueInfo = {panelListInfo, ispanel:1, istrace:0, $
                                       panelid:panelLayout[i].id, traceid:-1} $
        else panel_ValueInfo = [panel_ValueInfo, {panelListInfo, ispanel:1, istrace:0, $
                                                panelid:panelLayout[i].id, traceid:-1}]
      
      cPanel->getProperty,traceSettings=traceSettings
      traces = traceSettings->get(/all)
      
      if obj_valid(traces[0]) then begin
        ntr = n_elements(traces)
        trNames = cPanel->constructTraceNames()
        
        for j = 0,ntr-1 do begin
          panelValue = [panelValue, trNames[j]]
          panel_ValueInfo = [panel_ValueInfo, {panelListInfo, ispanel:0, istrace:1, $
                                             panelid:panelLayout[i].id, traceid:j}]          
          traces[j]->getProperty,dataX=dx, dataY=dy
          if obj_isa(traces[j],'thm_ui_spectra_settings') then begin
              traces[j]->getProperty,dataZ=dz
          endif else dz = ''
        endfor        
      endif
    endfor
  
  endif else begin
    npanels=0
    panelNames=''
  endelse

  ctextvalues=['']

  plotTypeTitle = Widget_Label(titleBase, Value='  - CREATE PLOTS -  ', uname='plottypetitle')
  
  compviewButton = Widget_Button(traceBase, Value='Show Data Components', UValue='SHOWCOMP', $
                              uname='showcomp', sensitive=1, tooltip='Show Components of ' + $
                              'data variables in widget trees')
  compviewmode = 0

  ;Primary Axis Tree
  priTree = obj_new('thm_ui_widget_tree',priBase,'PRITREE',loadedData, XSize=tree_size, $
                    YSize=340,mode=3,multi=1,leafonly=1, uname='pritree',/showdatetime, $
                    from_copy=long(*info.guiTree))

  if ~array_equal(datanames, '', /no_typeconv) then begin
    xSelect = dataNames[0]
    ySelect = dataNames[1]
  endif else begin
    xSelect = ''
    ySelect = ''
  endelse
  
  add_label = widget_label(plusbase,value=' Add: ')

  plusButtonSens = is_string(xSelect) && is_string(ySelect)
  plusButton = Widget_Button(plusBase, Value=' Line -> ', UValue='ADDLINE', $
    Tooltip='Add Lines to the selected panel', sensitive=plusButtonSens)

  if isspec then begin

  endif else begin
    specButton = Widget_Button(plusBase, Value=' Spec -> ', UValue='ADDSPEC', $
                 Tooltip='Add Spectrograms to selected panel', sensitive=1)
  endelse
  
  panelLabel = Widget_Label(tbuttonBase, Value='Panels', /Align_Center)

  panelList = Widget_List(panelBase, Value=panelValue, UValue='PANEL', /Align_Left, $
                          YSize=18, xsize=70)
  if npanels gt 0 then  widget_control, panelList, $
                           set_list_select=where(panelNames[npanels-1] eq panelValue)
  trace_sel=-1

  lockBase = Widget_Base(topButtonBase, /Col, /Align_Left)
  lockAxesButton = Widget_Button(lockBase, Value='Lock To Panel', UValue='LOCK_PAN', $
                                 uname='lock_pan', $
                                 tooltip='Lock panel axes to currently selected panel. ' + $
                                         'Notated by an (L).')
  unlockAxesButton = Widget_Button(lockBase, Value='Unlock Panels', UValue='UNLOCK_PAN', $
                                   uname='unlock_pan', tooltip='Unlock Panel Axes.')
  
  
  addButton = Widget_Button(tbuttonBase, Value=' Add ', UValue='ADDPAN', uname='addpan', $
    Tooltip='Add a new panel')

  removeButton = Widget_Button(tbuttonBase, Value=' Remove ', UValue='REMPAN', uname='rempan', $
    Tooltip='Removes the selected panel/trace', sensitive=(npanels gt 0))

  editButton = Widget_Button(tbuttonBase, Value=' Edit ', UValue='EDPAN', uname='edpan', $
    Tooltip='Edit panel/trace (opens Panel, Line, or Z-Axis Options window)', sensitive=(npanels gt 0))
  
  getresourcepath,rpath
  leftArrow = read_bmp(rpath + 'arrow_180_medium.bmp', /rgb)
  rightArrow = read_bmp(rpath + 'arrow_000_medium.bmp', /rgb)
  upArrow = read_bmp(rpath + 'arrow_090_medium.bmp', /rgb)
  downArrow = read_bmp(rpath + 'arrow_270_medium.bmp', /rgb)
  
  thm_ui_match_background, tlb, leftArrow
  thm_ui_match_background, tlb, rightArrow
  thm_ui_match_background, tlb, upArrow
  thm_ui_match_background, tlb, downArrow
  
  shiftupButton = Widget_Button(updownArrowBase, Value=upArrow, /Bitmap, UValue='PAN_UP', $
    Tooltip='Move this panel/trace up by one', sensitive=(npanels gt 0))
  shiftdownButton = Widget_Button(updownArrowBase, Value=downArrow, /Bitmap, $
    UValue='PAN_DOWN', Tooltip='Move this panel/trace down by one', sensitive=(npanels gt 0))
  shiftleftButton = Widget_Button(leftArrowBase, Value=leftArrow, /Bitmap, $
                                  UValue='PAN_LEFT', Tooltip='Move this panel left by one', $
                                  sensitive=(npanels gt 0))
  shiftrightButton = Widget_Button(rightArrowBase, Value=rightArrow, /Bitmap, $
                                   UValue='PAN_RIGHT', Tooltip='Move this panel right by one', $
                                   sensitive=(npanels gt 0))
  rowLabel = Widget_Label(rowTextBase, Value='Row:')
  
  if npanels gt 0 then $
    rowText = thm_ui_spinner(rowTextBase, Increment=1, Value=panelLayout[npanels-1].row, $
                               UValue='PAN_ROW') $
    else $
    rowText = thm_ui_spinner(rowTextBase, Increment=1, Value=1, UValue='PAN_ROW', sensitive=0)
                               
  colLabel = Widget_Label(colTextBase, Value='Column:')

  if npanels gt 0 then $
    colText = thm_ui_spinner(colTextBase, Increment=1, Value=panelLayout[npanels-1].col, $
                             UValue='PAN_COL') $
    else $
    colText = thm_ui_spinner(colTextBase, Increment=1, Value=1, UValue='PAN_COL', sensitive=0)
  
  rowspace = Widget_Label(rowPageTextBase, Value='')
  rowPageLabel = Widget_Label(rowPageTextBase, Value='Rows Per Page:')
  colPageLabel = Widget_Label(colPageTextBase, Value='Cols Per Page:')
  
  rowPageText = thm_ui_spinner(rowPageTextBase, Increment=1, Value=nrows, $
                                 UValue='PAGE_ROW', uname='page_row')
  colPageText = thm_ui_spinner(colPageTextBase, Increment=1, Value=ncols, $
                                 UValue='PAGE_COL', uname='page_col')


; VARIABLES WIDGETS ------------------------------------------------------------
  variableLabel =  Widget_Label(varLabelBase, Value='Variables: ')
  variableText = WIDGET_TEXT(varTextBoxBase, Value=ctextvalues, XSize=40, YSize=7, $
                             UValue='VAR_LIST')
  varEditButton = Widget_Button(varButtonsBase, Value=' Add/Edit ', UValue='EDVAR', $
                                tooltip='Open Variable Options panel')              


; MAIN BUTTON WIDGETS ----------------------------------------------------------
  okButton = Widget_Button(buttonBase, Value=' OK ', UValue='OK', XSize=80, $
    Tooltip='Applies the changes to the layout and closes the window')
  applyButton = Widget_Button(buttonBase, Value=' Apply ', UValue='APPLY', $
    Tooltip='Applies the changes to the layout', XSize=80)
  cancelButton = Widget_Button(buttonBase, Value=' Cancel ', UValue='CANC', XSize=80, $
    Tooltip='Cancels the operation and closes the window')

  

  cWindow->getProperty,settings=psettings
  psettings->save
  
; SETUP EVENT HISTORY
  numEvents = 5
  uvalHist = strarr(numEvents) ; init array of last 5 uvals
  eventHist = ptrarr(numEvents, /allocate_heap)
  for i=0,numEvents-1 do *eventHist[i]=''
  
; REPORT WINDOW OPENING TO GUI STATUS BAR
  info.statusBar->update,'Plot/Layout Options opened.'
  
  StatusBar = Obj_New('THM_UI_MESSAGE_BAR', $
                       Value='Status information is displayed here.', $
                        statusBase, XSize=150, YSize=1)
  
  
  state = {tlb:tlb, info:info, uvalHist:uvalHist, eventHist:eventHist, $
           tree_size:tree_size, compviewmode:compviewmode, $
           def_colors:def_colors, lockPrefix:lockPrefix, $
           variableText:variableText, panelList:panelList, $
           xselect:Ptr_New(xselect), yselect:Ptr_New(yselect), zselect:Ptr_New(), $
           y_ind:Ptr_New(), z_ind:Ptr_New(), $
           dataNames:dataNames, $
           rowtext:rowtext, coltext:coltext, $
           rowPageText:rowPageText, colPageText:colPageText, $
           editButton:editButton, plusButton:plusButton, $
           shiftleftButton:shiftleftButton, shiftrightButton:shiftrightButton, $
           shiftupButton:shiftupButton, shiftdownButton:shiftdownButton, $
           panels:ptr_new(panels), panelObjs:ptr_new(panelObjs), panelValue:ptr_new(panelValue), $
           nRows:nRows, nCols:nCols, initNRows:initNRows, initNCols:initNCols, $
           varList:Ptr_New(), $
           panelLayout:Ptr_New(panelLayout), npanels:npanels, panel_sel:(npanels-1), $
           panel_ValueInfo:Ptr_New(panel_ValueInfo), panelNames:ptr_new(panelNames), $
           trace_sel:trace_sel, is_trace_spec:0, $
           tabBase:tabBase, priBase:priBase, secBase:0, terBase:0, $
           priTree:priTree, secTree:obj_new(), terTree:obj_new(), $
           xtree_copy:-1l, priTree_copy:long(*info.guiTree), secTree_copy:-1l, $
           specButton:specButton, plusBase:plusBase, $
           cWindow:cWindow, origWindow:origWindow, template:info.template_object,$
           windowStorage:windowStorage, loadedData:loadedData, drawObject:drawObject, $
           zaxisSettings:zaxisSettings,treeCopyPtr:info.guiTree, $
           historywin:info.historywin,statusbar:statusbar}
 
  cWindow->getProperty,locked=locked
  thm_ui_update_var_widget, state=state

  CenterTlb, tlb
  Widget_Control, tlb, Set_UValue=state, /No_Copy
  Widget_Control, tlb, /Realize

 ; thm_ui_init_tree_widgets, tlb
  
  XManager, 'thm_ui_layout_options', tlb, /No_Block
  
RETURN
END
