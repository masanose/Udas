
;+
;  thm_ui_variable_options
;
;W.M.Feuerstein, 10/14/2008.
;Rewritten pcruce@igpp.ucla.edu 9/10/2009
;-

pro thm_ui_add_variable_event, event

Compile_Opt idl2, hidden

Widget_Control, event.TOP, Get_UValue=state

;Put a catch here to insure that the state remains defined

err_xxx = 0
Catch, err_xxx
IF(err_xxx Ne 0) THEN BEGIN
  Catch, /Cancel
  Help, /Last_Message, Output=err_msg
  FOR j = 0, N_Elements(err_msg)-1 DO begin
    Print, err_msg[j]
    state.historywin->update,err_msg[j]
  endfor
  Print, 'Error--See history'
  histobj=state.historywin
  Widget_Control, event.TOP, Set_UValue=state, /No_Copy
  thm_gui_error,state.gui_id,histobj
  Widget_Control, event.top, /Destroy
  RETURN
ENDIF

;kill request block

IF (Tag_Names(event, /Structure_Name) EQ 'WIDGET_KILL_REQUEST') THEN BEGIN
  *state.guiTree = state.treeObj->GetCopy()  
  Widget_Control, event.top, /Destroy
  RETURN
ENDIF

Widget_Control, event.id, Get_UValue=uval

CASE uval OF

  'CANC': BEGIN
   
    state.historyWin->Update,'Exiting Variable Options.
    state.statusbar->Update,'Exiting Variable Options.
    ;
    *state.guiTree = state.treeObj->GetCopy()
    
    Widget_Control, event.top, /destroy
    RETURN
  END
  'OK': begin
    ;Get indices of user selection:
    ;******************************
    id=widget_info(event.top,find_by_uname='addvarwlist')

    widget_control,id,get_value=wlist                            ;tree
    *state.return=wlist->GetValue()                                       ;tree -- may need to reconstruct indicies to NAME.
  
    *state.guiTree = state.treeObj->GetCopy()
    Widget_Control, event.top, /destroy
    RETURN
  end
  else:
ENDCASE

end

pro thm_ui_variable_options_get_varinfo,tlb,panels,statusbar,historywin,operation,variables=variables,varselect=varselect,varlist=varlist,fail=fail

  Compile_Opt idl2, hidden
  
    fail = 1
    variables = obj_new()
    varselect = -1
  
    panelNum = panels->count()
    if panelNum eq 0 then begin
      statusbar->update,'Cannot '+ operation +' variable, no panels'
      historywin->update,'Variable Panel, ' + operation + ' : cannot ' + operation + ', no panels'
      return
    endif
    
    panels = panels->get(/all)
    panelSelect = thm_ui_variable_get_combobox_select(tlb,'panellist')
    panel = panels[panelSelect]
    
    panel->getProperty,variables=variables
    
    variableNum = variables->count()
    if variableNum eq 0 then begin
      statusbar->update,'Cannot ' + operation + ' variable, no variables'
      historywin->update,'Variable Panel, ' + operation + ' : cannot ' + operation + ', no variables'
      return
    endif

    ;get variable list selection
    varlist = widget_info(tlb,find_by_uname='varlisttext')
    varselect = (widget_info(varlist,/list_select))[0]
    
    if varselect[0] eq -1 || varselect gt variableNum then begin
      statusbar->update,'Cannot ' + operation + ' variable, no valid selection'
      historywin->update,'Variable Panel, ' + operation + ' : cannot ' + operation + ', no valid selection'
      return
    endif
        
    fail = 0
            
end

function thm_ui_add_variable, guiId,loadedData,guiTree,historywin,statusBar,multi=multi

Compile_Opt idl2, hidden

  if ~keyword_set(multi) then begin
    multi=0
  endif

  tlb=widget_base(/col,title='Add Variable(s)',group_leader=guiId,/floating,/modal)
;  buttonbase=widget_base(tlb,/row,/align_center)

  ;Widgets:
  ;********

  treeObj=obj_new('thm_ui_widget_tree',tlb,'VARIABLES',loadeddata,xsize=400,ysize=400,uname='addvarwlist',mode=1, $
    multi=multi,/leafonly,/showdatetime,from_copy=*guiTree)  ;tree

  buttonbase=widget_base(tlb,/row,/align_center)
  okbutton=widget_button(buttonbase,value='OK', uval='OK')
  cancelbutton=widget_button(buttonbase,value='CANCEL', uval = 'CANC')

  return_value = ptr_new('')

  state = {treeObj:treeObj,guiTree:guiTree,historywin:historywin,statusbar:statusbar,return:return_value}

;Make sure the window is centered:
;*********************************
CenterTlb, tlb

;Make and store state structure:
;********************************
widget_control,tlb,set_uval=state

;Realize widget:
;***************
widget_control,tlb,/realize
XManager, 'thm_ui_add_variable', tlb, /No_Block

;Return success:
;***************
return,*return_value

end

pro thm_ui_variable_set_value,tlb,panels,statusbar,historywin, previousvar=previousvar

  Compile_Opt hidden,idl2
  
  panelNum = panels->count()
  if panelNum eq 0 then begin
    thm_ui_variable_options_init_novars,tlb
    return
  endif
   
  panelObjs = panels->get(/all)
  
  panelSelect = thm_ui_variable_get_combobox_select(tlb,'panellist')
  panel = panelObjs[panelSelect]

  if ~obj_valid(panel) then return

  ;set label margin value
  labelmarginwidget = widget_info(tlb,find_by_uname='labelmarginwidget')
  widget_control,labelmarginwidget,get_value=labelmargin

  panel->setProperty,labelmargin=labelmargin
  
  thm_ui_variable_options_get_varinfo,tlb,panels,statusbar,historywin,'Set Value',variables=variables,varselect=varselect,varlist=varlist,fail=fail

  if fail then return
  
  if size(previousvar,/type) ne 0 then begin
    if previousvar ge 0 and previousvar ne varselect then begin
      varselect = previousvar
    endif 
  endif
  
  varObj = variables->get(position=varselect)
  varObj->getProperty,text=textObj
  
  controlwidget  = widget_info(tlb,find_by_uname='controlwidget')  
  widget_control,controlwidget,sensitive=1,get_value=controlname

  textwidget  = widget_info(tlb,find_by_uname='textwidget')  
  widget_control,textwidget,sensitive=1,get_value=textString
  
  format = thm_ui_variable_get_combobox_select(tlb,'formatwidget')

  if format eq -1 then begin
    format = 4
  endif

  autowidget  = widget_info(tlb,find_by_uname='aauto') 
  dblwidget  = widget_info(tlb,find_by_uname='adbl') 
  expwidget  = widget_info(tlb,find_by_uname='aexp') 
  
  if widget_info(autowidget,/button_set) then begin
    annoExpo = 0
  endif else if widget_info(dblwidget,/button_set) then begin
    annoExpo = 1
  endif else begin
    annoExpo = 2
  endelse
  
  ;current color display draw widget
  colorwindow = widget_info(tlb,find_by_uname='colorwindow')
  ;get the actual window object
  Widget_Control, colorwindow, Get_Value=colorWin
  ;get the scene being drawn on the object
  ColorWin->getProperty,graphics_tree=scene
  ;get the color from the scene
  scene->getProperty,color=color
  
  includeunitswidget  = widget_info(tlb,find_by_uname='includeunitswidget')  
  includeunits = widget_info(includeunitswidget,/button_set)
  
  showvarwidget  = widget_info(tlb,find_by_uname='showvarwidget')  
  showvar = widget_info(showvarwidget,/button_set)
  
  varObj->setProperty,$
                controlname=controlname,$
                includeunits=includeunits,$
                format=format,$
                annotateExponent=annoExpo
  
  textObj->setProperty,$
          value=textString,$
          color=color,$
          show=showvar
end
;+
;NAME:
; thm_ui_variable_options
;
;PURPOSE:
; This routine creates and handles the layout widget. The layout panel is
; used to create and control a panels settings
;
;CALLING SEQUENCE:
; thm_ui_variable_options, gui_id
;
;INPUT:
; gui_id:  id for the master base widget (tlb)
;
;OUTPUT:
;
;HISTORY:
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2010-02-19 09:46:17 -0800 (Fri, 19 Feb 2010) $
;$LastChangedRevision: 7343 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_variable_options.pro $
;---------------------------------------------------------------------------------

PRO thm_ui_variable_options_event, event

  Compile_Opt hidden,idl2

  if widget_valid(event.top) then begin
    Widget_Control, event.TOP, Get_UValue=state
  endif else begin
    dprint,'IDL error detected, halting execution to prevent unescapable loop'
    stop
  endelse
    
  err_xxx = 0
  Catch, err_xxx
  If(err_xxx Ne 0) Then Begin
    Catch, /Cancel
    Help, /Last_Message, output = err_msg
    FOR j = 0, N_Elements(err_msg)-1 DO PRINT, err_msg[j]
    ;Print, 'Error--See history'
    histobj=state.historywin
    Widget_Control, event.top, Set_UValue=state, /No_Copy
    if is_struct(state) then begin
      thm_gui_error,state.gui_id,histobj
    endif else begin
      dprint,'Handling error for bug with improperly set state struct.  Value of !ERROR_STATE.msg is:  ' + !error_state.msg
    endelse
    Widget_Control, event.top, /Destroy
    RETURN
  EndIf
  
  IF(Tag_Names(event, /Structure_Name) EQ 'WIDGET_KILL_REQUEST') THEN BEGIN  
    cWindow = state.windowStorage->getActive()
    cWindow->reset
    state.drawObject->update,state.windowStorage,state.loadedData
    state.drawObject->draw
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy    
    Widget_Control, event.top, /Destroy
    RETURN      
  ENDIF
  
  Widget_Control, event.id, Get_UValue=uval
  
  ;skip any events returned for widgets without user values
  if ~keyword_set(uval) && widget_valid(event.top) then begin
    Widget_Control, event.top, Set_UValue=state, /No_Copy
    return
  endif
  
  thm_ui_variable_set_value,state.tlb,state.panels,state.statusbar,state.historywin, previousvar=state.previousvar
 
  CASE uval OF
    'ADD':BEGIN
      panelNum = state.panels->count()
      
      if panelNum eq 0 then begin
        state.statusbar->update,'Cannot add variables until panels are present in the layout'
        state.historywin->update,'Variable Add, unable to add variables: No Panels'
      endif else if is_num(state.loadedData->getAll()) then begin
        state.statusbar->update,'Cannot add variables until data is loaded.'
        state.historywin->update,'Variable Add, unable to add variables: No data.'
      endif else begin
        newvars = thm_ui_add_variable(state.gui_id,state.loadedData,state.guiTree,state.historywin,state.statusbar,multi=1)
        if ~keyword_set(newvars[0]) then begin
          state.statusbar->update,'Cannot add variables no selection.'
          state.historywin->update,'Variable Add, unable to add variables: No selection.'
        endif else begin
        
          panels = state.panels->get(/all)
          panelSelect = thm_ui_variable_get_combobox_select(state.tlb,'panellist')
          panel = panels[panelSelect]
        
          panel->getProperty,variables=variables
          if ~obj_valid(variables) || ~obj_isa(variables,'idl_container') then begin
            variables = obj_new('IDL_Container')
            panel->setProperty,variables=variables
          endif
          
          state.template->getProperty,variable=variableTemplate
        
          for i = 0,n_elements(newvars)-1 do begin
          
            dataObj = state.loadedData->getObjects(name=newvars[i])
            dataObj->getProperty,indepname=indepname,timename=timename,isTime=isTime
            if keyword_set(indepname) && state.loadedData->isChild(indepname) then begin
              controlname = indepname
            endif else begin
              controlname = timename
            endelse
           
            state.pageSettings->getProperty,variables=varText
            varText = varText->copy()
            varText->setProperty,value=newvars[i]+' :'
        
            if obj_valid(variableTemplate) then begin
              newvarobj = variableTemplate->copy()
            endif else begin
              newvarobj = obj_new('thm_ui_variable')
            endelse
            
            newvarobj->setProperty,controlname=controlname,fieldname=newvars[i],text=varText,isTime=isTime
           
            variables->add,newvarobj
          
          endfor
                 
        endelse
      endelse
      
      thm_ui_variable_options_init,state
    
    END
    'PICKCONTROL' : begin
      
      controlname = thm_ui_add_variable(state.gui_id,state.loadedData,state.guiTree,state.historywin,state.statusbar,multi=0)
                                          
      if keyword_set(controlname[0]) then begin
        controlwidget  = widget_info(state.tlb,find_by_uname='controlwidget')  
        widget_control,controlwidget,sensitive=1,set_value=controlname        
      endif

    end
    'SUBTRACT': BEGIN
    
      thm_ui_variable_options_get_varinfo,state.tlb, $
                                          state.panels,$
                                          state.statusbar,$
                                          state.historywin,$
                                          'remove',$
                                          variables=variables,$
                                          varselect=varselect,$
                                          varlist=varlist,$
                                          fail=fail
    
      if fail eq 0 then begin
        variables->remove,position=varselect
        widget_control,varlist,set_list_select=varselect-1
      endif

      thm_ui_variable_options_init,state
    end
    'UP':BEGIN
      thm_ui_variable_options_get_varinfo,state.tlb, $
                                      state.panels,$
                                      state.statusbar,$
                                      state.historywin,$
                                      'move up',$
                                      variables=variables,$
                                      varselect=varselect,$
                                      varlist=varlist,$
                                      fail=fail
    
      if fail eq 0 && varselect gt 0 then begin
        variables->move,varselect,varselect-1
        widget_control,varlist,set_list_select=varselect-1
      endif

      thm_ui_variable_options_init,state
    end
    'DOWN':BEGIN
      thm_ui_variable_options_get_varinfo,state.tlb, $
                                      state.panels,$
                                      state.statusbar,$
                                      state.historywin,$
                                      'move down',$
                                      variables=variables,$
                                      varselect=varselect,$
                                      varlist=varlist,$
                                      fail=fail
    
      if fail eq 0 && varselect lt variables->count()-1 then begin
        variables->move,varselect,varselect+1
        widget_control,varlist,set_list_select=varselect+1
      endif

      thm_ui_variable_options_init,state
    end
    'PALETTE': begin
    
      ;current color display draw widget
      colorwindow = widget_info(state.tlb,find_by_uname='colorwindow')
      ;get the actual window object
      Widget_Control, colorwindow, Get_Value=colorWin
      ;get the scene being drawn on the object
      ColorWin->getProperty,graphics_tree=scene
      ;get the color from the scene
      scene->getProperty,color=currentcolor

      color = PickColor(!p.color, Group_Leader=state.tlb, Cancel=cancelled,currentcolor=currentcolor)

      if ~cancelled then begin
        scene->setProperty,color=reform(color)
        colorwin->draw,scene
      endif
      
    end
    'VARIABLES': begin
      thm_ui_variable_options_init,state
    end
    'PANELS': begin
      thm_ui_variable_options_init,state
    end
    'TEMP': begin
      thm_ui_variable_options_get_varinfo,state.tlb, $
                                  state.panels,$
                                  state.statusbar,$
                                  state.historywin,$
                                  'save to temp',$
                                  variables=variables,$
                                  varselect=varselect,$
                                  varlist=varlist,$
                                  fail=fail
      if fail eq 0 then begin
        state.template->setProperty,variable=variables->get(position=varselect)
        state.statusbar->update,'Saved current variable settings to template'
        state.historywin->update,'Saved current variable settings to template'   
      endif else begin
        state.statusbar->update,'Cannot save template. Needs a valid variable to save variable template.'
        state.historywin->update,'Cannot save template. Needs a valid variable to save variable template.'
      endelse
                                  
    end
    'CANC': BEGIN
      cWindow = state.windowStorage->getActive()
      cWindow->reset
      state.drawObject->update,state.windowStorage,state.loadedData
      state.drawObject->draw
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    END
    'APPLY':begin
      state.drawObject->update,state.windowStorage,state.loadedData
      state.drawObject->draw
    end
    'OK': BEGIN
      state.drawObject->update,state.windowStorage,state.loadedData
      state.drawObject->draw
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    END
    ELSE:
  ENDCASE
    
  Widget_Control, event.top, Set_UValue=state, /No_Copy

  RETURN
   
END ;--------------------------------------------------------------------------------

function thm_ui_variable_get_combobox_select,tlb,uname

  compile_opt idl2,hidden

  ;combobox widget index
  combo = widget_info(tlb,find_by_uname=uname)
  ;combobox text
  text = widget_info(combo,/combobox_gettext)
  ;combobox values list
  widget_control,combo,get_value=names
  
  ;combobox index of current text
  return,where(text eq names)

end

;desensitizes fields for common fail case
pro thm_ui_variable_options_init_novars,tlb

  compile_opt idl2,hidden

  varlist = widget_info(tlb,find_by_uname='varlisttext')
  widget_control,varlist,set_value=' '
  
  shiftup = widget_info(tlb,find_by_uname='shiftupbutton')
  widget_control,shiftup,sensitive=0
  
  shiftdown = widget_info(tlb,find_by_uname='shiftdownbutton')
  widget_control,shiftdown,sensitive=0
  
  fieldwidget  = widget_info(tlb,find_by_uname='fieldwidget')  
  widget_control,fieldwidget,sensitive=0,set_value='<none selected>'
  
  controlwidget  = widget_info(tlb,find_by_uname='controlwidget')  
  widget_control,controlwidget,sensitive=0,set_value='<none selected>'

  controlbutton  = widget_info(tlb,find_by_uname='controlbutton')  
  widget_control,controlbutton,sensitive=0
  
  textwidget  = widget_info(tlb,find_by_uname='textwidget')  
  widget_control,fieldwidget,sensitive=0,set_value=' '
  
  symbolwidget  = widget_info(tlb,find_by_uname='symbolwidget')  
  widget_control,symbolwidget,sensitive=0
  
  formatwidget  = widget_info(tlb,find_by_uname='formatwidget')  
  widget_control,formatwidget,sensitive=0
  
  annobase  = widget_info(tlb,find_by_uname='annobase')  
  widget_control,annobase,sensitive=0
  
  palettewidget  = widget_info(tlb,find_by_uname='palettewidget')  
  widget_control,palettewidget,sensitive=0
  
  includeunitswidget  = widget_info(tlb,find_by_uname='includeunitswidget')  
  widget_control,includeunitswidget,sensitive=0
  
  showvarwidget  = widget_info(tlb,find_by_uname='showvarwidget')  
  widget_control,showvarwidget,sensitive=0
  
end

pro thm_ui_variable_options_init,state

  compile_opt idl2,hidden
  
  tlb = state.tlb
  
  shiftup = widget_info(tlb,find_by_uname='shiftupbutton')
  shiftdown = widget_info(tlb,find_by_uname='shiftdownbutton')
  
  panelNum = state.panels->count()
  if panelNum eq 0 then begin
    state.previousvar = -1
    thm_ui_variable_options_init_novars,tlb
    return
  endif
   
  panels = state.panels->get(/all)
  
  panelSelect = thm_ui_variable_get_combobox_select(tlb,'panellist')
  panel = panels[panelSelect]

  if ~obj_valid(panel) then return

  panel->getProperty,labelmargin=labelmargin,variables=variables

  ;set label margin value
  labelmarginwidget = widget_info(tlb,find_by_uname='labelmarginwidget')
  widget_control,labelmarginwidget,set_value=labelmargin

  ;get variable list selection
  varlist = widget_info(tlb,find_by_uname='varlisttext')
  varselect = (widget_info(varlist,/list_select))[0]

  ;no variables, then return
  varnum = variables->count()
  if varnum eq 0 then begin
    state.previousvar = -1
    thm_ui_variable_options_init_novars,tlb
    return
  endif

  ;assemble list of variable names
  varnames = 0
  varObjs = variables->get(/all)
  for i = 0,varnum-1 do begin
    varObj = varObjs[i]
    varObj->getProperty,fieldname=fieldname
    varnames = array_concat(fieldname,varnames)
  endfor
 
  ;set default selection
  if varselect eq -1 then begin
    varselect = 0
  endif
 
  ;set list of variable names
  if keyword_set(varnames) then begin 
    widget_control,varlist,set_value=varnames
    widget_control,varlist,set_list_select=varselect
    state.previousvar = varselect
  endif else begin
    state.previousvar = -1
    thm_ui_variable_options_init_novars,tlb
    return
  endelse
  
  ;set sensitivity values for arrows
  if varselect lt n_elements(varnames)-1 && $
     varselect gt 0 then begin  
    widget_control,shiftup,sensitive=1
    widget_control,shiftdown,sensitive=1  
  endif else if varselect lt n_elements(varnames)-1 then begin
    widget_control,shiftup,sensitive=0
    widget_control,shiftdown,sensitive=1  
  endif else if varselect gt 0 then begin
    widget_control,shiftup,sensitive=1
    widget_control,shiftdown,sensitive=0
  endif else begin
    widget_control,shiftup,sensitive=0
    widget_control,shiftdown,sensitive=0
  endelse
  
  varObj = varObjs[varselect]
 
  varObj->getProperty, $
            fieldname=fieldname,$
            controlname=controlname,$
            includeunits=includeunits,$
            text=text,$
            format=format,$
            istime=istime,$
            annotateExponent=anno
 
  text->getProperty,color=color,value=textString,show=show
 
  fieldwidget  = widget_info(tlb,find_by_uname='fieldwidget')  
  widget_control,fieldwidget,sensitive=1,set_value=fieldname
  
  controlwidget  = widget_info(tlb,find_by_uname='controlwidget')  
  widget_control,controlwidget,sensitive=1,set_value=controlname

  controlbutton  = widget_info(tlb,find_by_uname='controlbutton')  
  widget_control,controlbutton,sensitive=1
  
  textwidget  = widget_info(tlb,find_by_uname='textwidget')  
  widget_control,textwidget,sensitive=1,set_value=textString
  
  symbolwidget  = widget_info(tlb,find_by_uname='symbolwidget')  
  widget_control,symbolwidget,sensitive=0
  
  formatwidget  = widget_info(tlb,find_by_uname='formatwidget')  
  annobase  = widget_info(tlb,find_by_uname='annobase') 
  
  formats = varObj->getFormats(istime=istime)
  
  widget_control,formatwidget,sensitive=1,$
                   set_value=formats,$
                   set_combobox_select=format
  
  if istime then begin
  
    widget_control,annobase,sensitive=0
  
  endif else begin
  
    widget_control,annobase,sensitive=1
    
    if anno eq 0 then begin
      autowidget  = widget_info(tlb,find_by_uname='aauto') 
      widget_control,autowidget,/set_button
    endif else if anno eq 1 then begin
      dblwidget  = widget_info(tlb,find_by_uname='adbl') 
      widget_control,dblwidget,/set_button
    endif else if anno eq 2 then begin
      expwidget  = widget_info(tlb,find_by_uname='aexp') 
      widget_control,expwidget,/set_button
    endif
  
  endelse
  
  palettewidget  = widget_info(tlb,find_by_uname='palettewidget')  
  widget_control,palettewidget,sensitive=1
  
  colorid = widget_info(tlb,find_by_uname='colorwindow')
  widget_control,colorid,get_value=colorwindow
  scene=obj_new('IDLGRSCENE', color=color)
  colorwindow->setProperty,graphics_tree=scene
  colorwindow->draw, scene
  
  ;To re-enable the include units flag, just set sensitive = 1
  includeunitswidget  = widget_info(tlb,find_by_uname='includeunitswidget')  
  widget_control,includeunitswidget,sensitive=0,set_button=includeunits
  
  showvarwidget  = widget_info(tlb,find_by_uname='showvarwidget')  
  widget_control,showvarwidget,sensitive=1,set_button=show
 
end

Pro thm_ui_variable_options, gui_id, loadeddata, windowstorage, drawobject, historywin, template,guiTree,panel_select=panel_select

  ; top level and main base widgets

tlb = Widget_Base(/Col, Title='THEMIS: Variable Options ', Group_Leader=gui_id, /Modal, /Floating, /tlb_kill_request_events)
mainBase = Widget_Base(tlb, /Row)
varlistBase = Widget_Base(mainBase, /Col, YPad=8)

panelBase = Widget_Base(varListBase, /row, ypad=4)
dummybase = Widget_Base(varListBase, /row, ypad=4)
varTextBase = Widget_Base(varListBase, YPad=1)
varButtonBase = Widget_Base(varListBase, /Row, /Align_center, YPad=1)

plusMinusBase = Widget_Base(mainBase, /Col, YPad=135, XPad=4)
attributesBase = Widget_Base(mainBase, /Col)

attLabelBase = Widget_Base(attributesBase)
attListBase = Widget_Base(attributesBase, /Col, Frame=3)
marginBase = Widget_Base(attributesBase, /Row, YPad=1)

buttonBase = Widget_Base(tlb, /Row, /Align_Center)
statusBase = Widget_Base(tlb, /Row, /Align_Center)

; widgets

;Get text values of current VARIABLESOBJECTS (if any) as well PANELOBJS, and PANELNAMES:
;***************************************************************************************
cWindow = windowStorage->GetActive()
cWindow->GetProperty, Panels=panels, locked=locked,settings=pageSettings
panelObjs = panels->Get(/all)

if is_num(panelObjs) then begin
  panelObjs = obj_new()
endif

;Check to see if PANEL_SELECT is set.  If not, then check to see if axes are locked.  If so, then initialize to last panel:
;**************************************************************************************************************************
;
if n_elements(panel_select) eq 0 || panel_select lt 0 || panel_select ge n_elements(panelObjs) then begin
  if locked ge 0 && locked lt n_elements(panelObjs) && obj_valid(panelObjs[0]) then begin
    rownum = 0
    for i=0, n_elements(panelobjs)-1 do begin ;find last panel on the page
      panelobjs[i]->getproperty, settings=psettings
      psettings->getproperty, row=row
      if row gt rownum then begin
        panel_select = i
        rownum = row
      endif 
    endfor
  endif else begin
    panel_select = 0
  endelse
endif

IF ~obj_valid(panelobjs[0]) THEN BEGIN
  panelNames=['No Panels']
  variables = obj_new('IDL_Container')
  variableobjs = obj_new()
ENDIF ELSE BEGIN
  n_panels=n_elements(panelobjs)
  panelObjs[panel_select]->GetProperty,variables=variables      ; *** Retrieve variables from 1st panel.
  panelObjs[panel_select]->GetProperty,labelmargin = labelmargin             ; Get LABELMARGIN from 1st panel.
  variableobjects=variables->get(/all)
;  FOR i=0, N_Elements(panelObjs)-1 DO BEGIN
;    panelObjs[i]->GetProperty, Name=name
;    IF i EQ 0 THEN panelNames=[name] ELSE panelNames=[panelNames, name]
;  ENDFOR
  panelnames=panelobjs[0]->constructpanelname()
  if n_panels gt 1 then begin
    for i=1,n_panels-1 do panelnames=[panelnames,panelobjs[i]->constructpanelname()]
  endif
ENDELSE

IF Is_Num(panelNames) THEN panelNames=['No Panels']
IF N_Elements(panelNames) EQ 1 && panelNames EQ '' THEN panelNames=['No Panels']

cWindow->save

pdLabel = widget_label(panelBase, value = 'Panels: ')
panelDroplist = Widget_combobox(panelBase, Value=panelNames, XSize=160, UValue='PANELS',uname='panellist')
if is_num(panel_select) then widget_control,panelDroplist, set_combobox_select=panel_select
varListLabel = Widget_Label(dummybase, Value='Variables: ')
varlistText=Widget_list(varTextBase, Value=ctextvalues, XSize=37, YSize=15, uname='varlisttext', uval='VARIABLES')
;varlistText=Widget_list(varTextBase, Value=shadowlisttextvalues, XSize=37, YSize=15, uname='varlisttext', uval='VARIABLES')

getresourcepath,rpath
upArrow = read_bmp(rpath + 'arrow_090_medium.bmp', /rgb)
downArrow = read_bmp(rpath + 'arrow_270_medium.bmp', /rgb)
plusbmp = read_bmp(rpath + 'plus.bmp', /rgb)
minusbmp = read_bmp(rpath + 'minus.bmp', /rgb)
palettebmp = read_bmp(rpath + 'color.bmp', /rgb)

thm_ui_match_background, tlb, upArrow
thm_ui_match_background, tlb, downArrow
thm_ui_match_background, tlb, plusbmp
thm_ui_match_background, tlb, minusbmp
thm_ui_match_background, tlb, palettebmp

shiftUpButton = Widget_Button(varButtonBase, Value=upArrow, /Bitmap, UValue='UP', uname = 'shiftupbutton', Tooltip='Move this panel up by one', $
  sensitive = 0)
shiftDownButton = Widget_Button(varButtonBase, Value=downArrow, /Bitmap, UValue='DOWN', uname = 'shiftdownbutton', $
  Tooltip='Move this panel down by one', sensitive = 0)
;getresourcepath,rpath
;plusbmp = rpath + 'plus.bmp'
;minusbmp = rpath + 'minus.bmp'
addButton = Widget_Button(plusMinusBase, Value=plusbmp, /Bitmap, ToolTip='Add selections to the list of data to be loaded', uval='ADD')
minusButton = Widget_Button(plusMinusBase, Value=minusbmp, /Bitmap, ToolTip='Remove data from the list of data to be loaded', $
  uval='SUBTRACT', uname='subtract',sensitive=subtractsensitive)
attLabel = Widget_Label(attLabelBase, Value='Attributes: ')

fieldBase = Widget_Base(attListBase, /row)
fieldLabel = Widget_Label(fieldBase, Value='Field: ', XSize=70, /align_left)
fieldText = Widget_Text(fieldBase, Value='<none selected>', XSize=20, YSize=1, uname='fieldwidget', sensitive=0)

controlBase = Widget_Base(attListBase, /row)
controlLabel = Widget_Label(controlBase, Value='Control: ', Xsize=70, /align_left)
controlText = Widget_Text(controlBase, Value='<none selected>', XSize=20, YSize=1, uname='controlwidget', sensitive=0, uval='PICKCONTROL')
;controltext= Widget_text(attListBase, xsize=200, Title='Control:  ', Value='<none selected>', uname = 'controlwidget', sensitive = 0, uval='PICKCONTROL')
controlButton = Widget_Button(controlBase, Value="Choose...", ToolTip='Pick a control for the variable.', uval='PICKCONTROL',sensitive=0,uname='controlbutton')

textBase = Widget_Base(attListBase, /Row)
textLabel = Widget_Label(textBase, Value='Text: ', XSize=70, /align_left)
textText = Widget_Text(textBase, Value=' ', XSize=20, /Editable, /all_events, YSize=1, uname='textwidget', uval='TEXT', sensitive=0)

if obj_valid(panelobjs[0]) then begin
  panelobjs[panel_select]->GetProperty,xaxis=xaxis
  if obj_valid(xaxis) then formatValues = xaxis->getannotationformats()
  foo=obj_new('thm_ui_variable')
  symbolValues = foo->getsymbols()
  obj_destroy, foo
endif else begin
  symbolvalues='<none selected>'
  formatValues='<none selected>'
endelse

sdBase = widget_base(attListBase, /row)
sdLabel = widget_label(sdBase, value = 'Symbol: ', XSize=70, /align_left)
symbolDroplist = WIDGET_combobox(sdBase, uname='symbolwidget', uval='SYMBOL', sensitive=0, value=symbolValues)

fdBase = widget_base(attListBase, /row)
fdLabel = widget_label(fdBase, value = 'Format: ', XSize=70, /align_left, uname='formatlabel')
formatDroplist = WIDGET_combobox(fdBase, Value=formatValues, uname='formatwidget', uval='FORMAT', sensitive=0)

  anoSOBase = widget_base(attListBase, /row, /exclusive, ypad=2, space=0,uname='annobase',sensitive=0)
  default = widget_button(anoSOBase, value = 'Auto-Notation', uvalue='AAUTO', uname='aauto')
  dbl = widget_button(anoSOBase, value = 'Decimal', uvalue='ADBL', uname='adbl')
  expo = widget_button(anoSOBase, value = 'Sci-Notation', uvalue='AEXP', uname = 'aexp') 
  atype = [default, dbl, expo]

paletteBase = Widget_Base(attListBase, /Row)
colorLabel = Widget_Label(paletteBase, Value='Color: ', xsize=70, /align_left)

paletteButton = Widget_Button(paletteBase, Value=palettebmp, /Bitmap, UValue='PALETTE', ToolTip='Choose color from Palette', uname = 'palettewidget', $
  sensitive=0)
vspaceLabel = Widget_Label(paletteBase, Value=' ')
colorWindow = Widget_Draw(paletteBase, XSize=50, YSize=19,sensitive=0,uname='colorwindow', $
                graphics_level=2,renderer=1,retain=1,units=0,frame=1, /expose_events)

includeBase = Widget_Base(attListBase, /Row, /NonExclusive)
includeButton = Widget_Button(includeBase, Value='Include units in label', uname = 'includeunitswidget', uval='INCLUDE UNITS', sensitive=0)

showVarBase = Widget_Base(attListBase, /Row, /NonExclusive)
showVarButton = Widget_Button(showVarBase , Value='Show Variable', uname = 'showvarwidget', uval='SHOWVAR', sensitive=0)
widget_control,showVarButton,/set_button

margBase = Widget_Base(marginBase, /Row)
if ~size(labelmargin,/type) then labelmargin=0
margIncrement = thm_ui_spinner(margBase, label= 'Label Margin (points): ', Increment=1, Value=labelmargin, uname='labelmarginwidget', $
  uval='LABELMARGIN', /all_events,sensitive=labelmarginsensitive)

okButton = Widget_Button(buttonBase, Value=' OK ', UValue='OK', XSize=80, $
  ToolTip='Applies the changes to the layout and closes the window')
applyButton = Widget_Button(buttonBase, Value=' Apply ', UValue='APPLY', XSize=80, $
  ToolTip='Applies the changes to the layout, leaves window open')
cancelButton = Widget_Button(buttonBase, Value=' Cancel ', UValue='CANC', XSize=80, $
  ToolTip='Cancels the operation and closes the window')
templateButton = Widget_Button(buttonBase, Value='Save to Template', UValue='TEMP',xsize=125)
 
; Create Status Bar Object
statusBar = Obj_New('THM_UI_MESSAGE_BAR', $
		     Value='Status information is displayed here.', $
		     statusBase,Xsize=75, YSize=1)

names=loadeddata->getall(/child)

;LOADEDDATA: contains unique NAMES of the loaded data.
;GUI_ID: Needed for the groud leader for the variable selection widget.
;WINDOWSTORAGE: Contains the current variable objects.

  state = {tlb:tlb, loadeddata:loadeddata, gui_id:gui_id, windowstorage:windowstorage, $
    drawobject:drawobject,panels:panels,pageSettings:pageSettings, previousvar:0L, $
    historywin:historywin, statusbar:statusbar, $
    guiTree:guiTree, treeObj:obj_new(),template:template}

centertlb,tlb
Widget_control, tlb, Set_UValue=state
Widget_control, tlb, /Realize

thm_ui_variable_options_init,state

XManager, 'thm_ui_variable_options', tlb, /No_Block

RETURN
END ;--------------------------------------------------------------------------------
































