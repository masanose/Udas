;NAME:
; thm_ui_zaxis_options
; stop
;PURPOSE:
; A widget interface for modifying line, zaxis and highlight attributes 
;
;CALLING SEQUENCE:
; thm_ui_newfile, gui_id
;
;INPUT:
; gui_id = the id number of the widget that calls this
;
;OUTPUT:
; 
;
;HISTORY:
;$LastChangedBy: aaflores $
;$LastChangedDate: 2010-01-29 11:47:26 -0800 (Fri, 29 Jan 2010) $
;$LastChangedRevision: 7182 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_zaxis_options.pro $
;
;---------------------------------------------------------------------------------

PRO thm_ui_zaxis_init_color, state=state


  compile_opt idl2, hidden

  ; intialize color windows
  
  state.zAxisSettings->GetProperty, LabelTextObject=labelTextObject, AnnotateTextObject=annotateTextObject
  IF Obj_Valid(labelTextObject) THEN labelTextObject->GetProperty, Color=value ELSE value=[0,0,0]
  Widget_Control, state.colorWindow, Get_Value=colorWin
  scene=obj_new('IDLGRSCENE', color=value)
  colorWin->setProperty,graphics_tree=scene
  colorWin->draw

  IF Obj_Valid(annotateTextObject) THEN AnnotateTextObject->GetProperty, Color=value ELSE value = [0,0,0]
  Widget_Control, state.acolorWindow, Get_Value=acolorWin
  scene=obj_new('IDLGRSCENE', color=value)
  acolorwin->setProperty,graphics_tree=scene
  acolorWin->draw
  
  ;get the draw window
  widget_control,state.zaxisarea,get_value=drawWin
  
  ;create the scene
  view = obj_new('IDLgrView',units=3,viewPlane_rect=[0,0,1,1],location=[0.,0.],dimensions=[1.,1.],zclip=[1.,-1],eye=5.,transparent=1,hide=0)
  model = obj_new('IDLgrModel')
  palette = obj_new('IDLgrPalette')
  getctpath,colortablepath
  palette->loadCt,state.zAxisSettings->getColorTableNumber(),file=colortablepath
  cbar = obj_new('IDLgrImage',indgen(1,256),palette=palette,location=[0,0,0],dimensions=[1,1])
  model->add,cbar
  view->add,model
  
  ;add the scene to the window and redraw
  drawWin->setProperty,graphics_tree=view 
  drawWin->draw

 

END ;---------------------------------------------------------------------------------------------

pro thm_ui_zaxis_propagate_settings, state

    compile_opt idl2, hidden

  sbutton = widget_info(state.tlb, find_by_uname='ssetall')
  abutton = widget_info(state.tlb, find_by_uname='asetall')

  if ~widget_info(sbutton,/button_set) and ~widget_info(abutton,/button_set) then return 

  z = state.zaxissettings->getall()

  if widget_info(sbutton,/button_set) then begin
    
    for i=0, n_elements(state.zaxes)-1 do begin
      
      if i eq state.selectedpanel then continue
      
      state.zaxes[i]->setproperty, $
                             minrange=z.minrange, $
                             maxrange=z.maxrange, $
                             colortable=z.colortable, $
                             fixed=z.fixed,$ 
                             scaling=z.scaling, $
                             ticknum=z.ticknum, $
                             minorticknum=z.minorticknum, $
                             autoticks=z.autoticks, $
                             placement=z.placement, $
                             margin=z.margin,$
                             logminorticktype=z.logminorticktype
    endfor
  endif
  
  if widget_info(abutton,/button_set) then begin

    a = z.annotateTextObject->getall()
    l = z.labelTextObject->getall()

    for i=0, n_elements(state.zaxes)-1 do begin
      
      if i eq state.selectedPanel then continue
      
      state.zaxes[i]->setproperty, labelOrientation=z.labelOrientation, $
                                   labelMargin=z.labelMargin, $
                                   annotationOrientation=z.annotationOrientation, $
                                   annotateExponent=z.annotateExponent, $
                                   annotationStyle=z.annotationStyle

      state.zAxes[i]->getproperty, annotateTextObject=annotateTextObject, $
                                   labelTextObject=labelTextObject

      annotateTextObject->setproperty, color=a.color, $
                                       font=a.font, $
                                       format=a.format, $
                                       show=a.show, $
                                       size=a.size, $
                                       thickness=a.thickness

      if obj_valid(labelTextObject) then $
        labelTextObject->setproperty, color=l.color, $
                                      font=l.font, $
                                      format=l.format, $
                                      show=l.show, $
                                      size=l.size, $
                                      thickness=l.thickness
      
    endfor
  endif

end ;---------------------------------------------------------------------------------------------

;This routine will update the zaxis settings using information from
;the draw object.  When automatic settings are used, the draw object 
;will sometimes use different numbers of ticks than the requested 
;number.  This routine ensures the values reported by the panel
;are accurate without requiring the draw object to break abstraction
pro thm_ui_update_zaxis_from_draw,panels,draw,historywin

  compile_opt idl2,hidden
  
  if obj_valid(panels) && panels->count() gt 0 then begin
  
    panel_list = panels->get(/all)
  
    ;loop over panel list
    for i = 0,n_elements(panel_list)-1 do begin
    
      panel_list[i]->getProperty,zaxis=zaxis
      
      if ~obj_valid(zaxis) then continue
      
      info = draw->getPanelInfo(i)
      
      if ~is_struct(info) then continue
      
      if ~info.hasSpec then begin
        historyWin->update,'Possible problem in zaxis Options.  Panel was expected to have z-axis but none is present.'
        continue
      endif
    
      zrange = info.zrange
      
      ;delog range(draw object represents ranges internally with logs already applied
      if info.zscale eq 1 then begin
        zrange = 10D^zrange
      endif else if info.zscale eq 2 then begin
        zrange = exp(zrange)
      endif
      
      zaxis->setProperty, $
            minRange=zrange[0], $
            maxrange=zrange[1], $
            tickNum = info.zmajornum,$
            minorTickNum = info.zminornum
        
    endfor
    
  endif
  
end

pro thm_ui_zaxis_switch_color_table,table,tlb

  compile_opt idl2, hidden
  
  ; set color table button
  
  colorbar = widget_info(tlb,find_by_uname='colorbar')
  
  Widget_Control, colorbar, Get_Value=win
  win->getProperty,graphics_tree=view
  model = (view->get(/all))[0]
  image = (model->get(/all))[0]
  image->getProperty,palette=pal
  getctpath,ctpathname
  pal->loadct,table,file=ctpathname
  win->draw    
  
end

;note the update function is the only remaining function
;in this panel that makes extensive use of the state struct
;to pass around widget ids.  If at possible, try to migrate
;away from this formulation when maintaining the code,
;and instead use the uname/find_by_uname formulation
PRO thm_ui_zaxis_update, state

    state.zAxisSettings->GetProperty, $
      ColorTable=colortable, $
      Fixed=fixed, $
      MinRange=minrange, $
      MaxRange=maxrange, $
      Scaling=scaling, $
      Placement=placement, $
      TickNum=ticknum, $
      minorTickNum=minorTickNum,$
      Margin=margin, $
      LabelMargin=labelmargin, $
      LabelTextObject=labeltextobject, $
      AnnotateTextObject=annotatetextobject, $
      LabelOrientation=labelorientation, $
      AnnotationStyle=annotationstyle, $
      AnnotationOrientation=annotationorientation, $
      annotateExponent=annotateExponent,$
      autoticks=autoticks,$
      logminorticktype=logminorticktype

    IF ~Obj_Valid(labelTextObject) THEN begin
     labelTextObject=Obj_New("THM_UI_TEXT")
     state.zAxisSettings->setProperty,labelTextObject=labelTextObject
    endif
    
    LabelTextObject->getproperty, $
      value=value, $
      font=font, $
      size=size, $
      format=format, $
      color=color
      
    if ~obj_valid(AnnotateTextObject) then begin
      AnnotateTextObject=obj_new('thm_ui_text')
      state.zAxisSettings->setProperty,annotateTextObject=annotateTextObject
    endif
      
    AnnotateTextObject->getproperty, $
      font=afont, $
      size=asize, $
      format=aformat, $
      color=acolor
      
      ; set current panel selection
    panels = widget_info(state.tlb, find_by_uname='sPanels')
    widget_control, panels, set_combobox_select=state.selectedPanel
    panels = widget_info(state.tlb, find_by_uname='tPanels')
    widget_control, panels, set_combobox_select=state.selectedPanel
    
        ; update widgets on the settings panel
    thm_ui_zaxis_switch_color_table,colortable,state.tlb
    
    FOR i = 0, N_Elements(state.colortablebuttons)-1 DO BEGIN
      IF i EQ colortable THEN begin
        state.colortable=colortable
        Widget_Control, state.colorTableButtons[colortable], set_button=1
      endif 
    ENDFOR
      
      ; set range widgets
      IF fixed EQ 0 THEN BEGIN
        Widget_Control, state.fixedButton, Set_Button=0
        Widget_Control, state.rangeMinIncrement, Set_Value=minRange   
        Widget_Control, state.rangeMaxIncrement, Set_Value=maxRange   
        Widget_Control, state.rangeMinIncrement, sensitive=0   
        Widget_Control, state.rangeMaxIncrement, sensitive=0        
      ENDIF ELSE BEGIN
        Widget_Control, state.fixedButton, set_Button=1
        Widget_Control, state.rangeMinIncrement, Set_Value=minRange   
        Widget_Control, state.rangeMaxIncrement, Set_Value=maxRange   
        Widget_Control, state.rangeMinIncrement, sensitive=1   
        Widget_Control, state.rangeMaxIncrement, sensitive=1        
      ENDELSE
      ; set scaling button
      CASE scaling OF
        0: Widget_Control, state.slinearButton, Set_Button=1
        1: Widget_Control, state.slogButton, Set_Button=1
        2: Widget_Control, state.snatButton, Set_Button=1
      ENDCASE 
      ; set placement buttons
      Widget_Control, state.plinearButtons[placement], Set_Button=1
      
      ;set ticks options
      id = widget_info(state.tlb,find_by_uname='autoticks')
      widget_control,id,set_button=autoticks
      id = widget_info(state.tlb,find_by_uname='ticksbase')
      widget_control,id,sensitive=~autoticks
      
      id = widget_info(state.tlb,find_by_uname='nmajorticks')
      widget_control,id,set_value=ticknum
      
      id = widget_info(state.tlb,find_by_uname='nminorticks')
      widget_control,id,set_value=minorTickNum

      id = widget_info(state.tlb,find_by_uname='margin')
      widget_control,id,set_value=margin

        ; set text tab widgets
        
      id = widget_info(state.tlb,find_by_uname='logminorticktypebase')
      widget_control,id,sensitive=scaling ne 0
      
      id = widget_info(state.tlb,find_by_uname='logminorticktype'+strtrim(logminorticktype,2))
      widget_control,id,/set_button
      
      ; Labels
      id = widget_info(state.tlb,find_by_uname='text')
      widget_control,id,set_value=value

      widget_control, state.fontDroplist, set_combobox_select=font
      widget_control, state.fontIncrement, set_value=size
      if format eq -1 then format = n_elements(labeltextobject->getformats())-1
      widget_control, state.formatDroplist, set_combobox_select=format
      widget_control, state.labelMarg, set_value=labelmargin
      if labelorientation eq 0 then widget_control, state.horizontalButton, /set_button $
        else widget_control, state.verticalButton, /set_button
      ; redraw color
      widget_control, state.colorWindow, get_value=colorWin
      colorWin->getProperty,graphics_tree=scene
      scene->setProperty,color=reform(color)
      colorWin->draw
      
      ; Annotations
      widget_control, state.afontDroplist, set_combobox_select=afont
      widget_control, state.afontIncrement, set_value=asize
      if aformat eq -1 then aformat = n_elements(labeltextobject->getformats())-1
      widget_control, state.aformatDroplist, set_combobox_select=aformat
      widget_control, state.annotationDroplist, set_combobox_select=annotationstyle
      widget_control, state.atype[annotateExponent], set_button=1
      if annotationOrientation eq 0 then widget_control, state.ahorizontalButton, /set_button $
        else widget_control, state.averticalButton, /set_button
      ; redraw color
      widget_control, state.acolorWindow, get_value=acolorWin
      acolorWin->getProperty,graphics_tree=scene
      scene->setProperty,color=reform(acolor)
      acolorWin->draw
      
      state.historywin->update,'THM_UI_ZAXIS_OPTIONS: Widget display values updated/redrawn.
      
END ;---------------------------------------------------------------------------------------------

;This routine allows delayed handling of events
;This should simplify code, allow for more reliable error handling, and generate more reliable code
;This style of event handling is being implemented using a gradual approach 
pro thm_ui_zaxis_set_value,state,event

  compile_opt idl2,hidden

  zaxis = state.zAxisSettings
  historyWin = state.historyWin
  statusBar = state.statusBar
  tlb = event.top
  
  ;minor tick num,major tick num, margin size spinners
  thm_ui_zaxis_set_value_ticks,zaxis,tlb,statusBar,historyWin
    
  ;range and scaling settings settings
  thm_ui_zaxis_set_value_range,zaxis,tlb,statusBar,historyWin
    
  ;color table value
  thm_ui_zaxis_set_value_color_table,zaxis,tlb,statusBar,historyWin
    
  ;zaxis placement
  thm_ui_zaxis_set_value_placement,zaxis,tlb,statusBar,historyWin
  
  ;text parameters
  thm_ui_zaxis_set_value_text,zaxis,tlb,statusBar,historyWin
  
  ;annotation parameters
  thm_ui_zaxis_set_value_annotation,zaxis,tlb,statusBar,historyWin
 
  historyWin->update,'Z Axis Update Complete',/dontshow
  
end

pro thm_ui_zaxis_set_value_text_formats,textobj,tlb,statusBar,historywin,uprefix,messagename

  compile_opt idl2,hidden
  
  ;set text font
  font = widget_info(tlb,find_by_uname=uprefix+'font')
  widget_control,font,get_value=fontnames
  currentfont = widget_info(font,/combobox_gettext)
  fontindex = where(currentfont eq fontnames,c)
  
  if c eq 0 then begin
    statusBar->update,'Error: Cannot identify ' +messagename+ ' font index'
    historyWin->update,'Error: Cannot identify ' +messagename+ ' font index'
    textObj->getProperty,font=fontindex
    widget_control,font,set_combobox_select=fontindex
  endif else begin
    textObj->setProperty,font=fontindex
  ;  statusBar->update,'Set ' + messagename + ' Font Value'
    historyWin->update,'Set ' + messagename + ' Font Value',/dontshow
  endelse
 
  ;set text size
  size = widget_info(tlb,find_by_uname=uprefix+'size')
  widget_control,size,get_value=sizevalue
  if ~finite(sizevalue,/nan) then begin
    if sizevalue le 0 then begin
      statusBar->update,'Error: ' + messagename+ ' font size cannot be 0 or less'
      historyWin->update,'Error: ' + messagename + ' font size cannot be 0 or less'
      textObj->getProperty,size=sizevalue
      widget_control,size,set_value=sizevalue
    endif else begin
      textObj->setProperty,size=sizevalue
;      statusBar->update,'Set ' + messagename + ' Font Value'
      historyWin->update,'Set ' + messagename + ' Font Size',/dontshow
    endelse
  endif else begin
    statusBar->update,'Invalid ' + messagename + ' text size, please re-enter.'
    historyWin->update,'Invalid ' + messagename + ' text size, value not applied.',/dontshow
  endelse
    
  ;set text format
  format = widget_info(tlb,find_by_uname=uprefix+'format')
  widget_control,format,get_value=formatnames
  currentformat = widget_info(format,/combobox_gettext)
  formatindex = where(currentformat eq formatnames,c)
  
  if c eq 0 then begin
    statusBar->update,'Error: Cannot identify ' + messagename + ' format index'
    historyWin->update,'Error: Cannot identify ' + messagename + ' format index'
    textObj->getProperty,format=formatindex
    widget_control,format,set_combobox_select=formatindex
  endif else begin
    textObj->setProperty,format=formatindex
   ; statusBar->update,'Set ' + messagename + ' format Value'
    historyWin->update,'Set ' + messagename + ' format Value',/dontshow
  endelse
  
  ;set text color 
  color = widget_info(tlb,find_by_uname=uprefix+'color')
  widget_control,color,get_value=cWin
  cWin->getProperty,graphics_tree=view
  view->getProperty,color=colorvalue
  textObj->setProperty,color=colorvalue
  ;statusBar->update,'Set ' + messagename + ' color Value'
  historyWin->update,'Set ' + messagename + ' color Value',/dontshow

end

pro thm_ui_zaxis_set_value_annotation,zaxis,tlb,statusBar,historyWin

  compile_opt idl2,hidden
  
  zaxis->getProperty,annotateTextObject=textobj
  
  ;verify that the text object actually exists
  if ~obj_valid(textobj) then begin
    textobj = obj_new('thm_ui_text')
    zaxis->setProperty,annotateTextObject=textobj
  endif
  
  ;use this function to handle all the features that are duplicated between text and annotations
  thm_ui_zaxis_set_value_text_formats,textobj,tlb,statusBar,historywin,'a','Annotation'
  
  ;set annotation orientation
  vert = widget_info(tlb,find_by_uname='avert')
  vertvalue = widget_info(vert,/button_set)
  zaxis->setProperty,annotationOrientation=vertvalue
 ; statusBar->update,'Set annotation orientation'
  historyWin->update,'Set annotation orientation',/dontshow
  
  ;set annotation style
  style = widget_info(tlb,find_by_uname='astyle')
  widget_control,style,get_value=stylenames
  currentstyle = widget_info(style,/combobox_gettext)
  styleindex = where(currentstyle eq stylenames,c)
  
  if c eq 0 then begin
    statusBar->update,'Error: Cannot identify style index'
    historyWin->update,'Error: Cannot identify style index'
    zaxis->getProperty,annotationstyle=styleindex
    widget_control,style,set_combobox_select=styleindex
  endif else begin
    zaxis->setProperty,annotationstyle=styleindex
    ;statusBar->update,'Set Style Value'
    historyWin->update,'Set Style Value',/dontshow
  endelse
  
  ;set annotation type restrictions
  annoExp=0
  id = widget_info(tlb, find_by_uname='aauto')
  if widget_info(id, /button_set) then annoExp =0
  id = widget_info(tlb, find_by_uname='adbl')
  if widget_info(id, /button_set) then annoExp =1
  id = widget_info(tlb, find_by_uname='aexp')
  if widget_info(id, /button_set) then annoExp =2
  zaxis->setProperty, annotateExponent=annoExp
  
end

pro thm_ui_zaxis_set_value_text,zaxis,tlb,statusBar,historyWin

  compile_opt idl2,hidden
  
  zaxis->getProperty,labelTextObject=textobj
  
  ;verify that the text object actually exists
  if ~obj_valid(textobj) then begin
    textobj = obj_new('thm_ui_text')
    zaxis->setProperty,labelTextObject=textobj
  endif
  
  ;set text value
  text = widget_info(tlb,find_by_uname='text')
  widget_control,text,get_value=textvalue
  textobj->setProperty,value=textvalue
;  statusBar->update,'Set Text Value'
  historyWin->update,'Set Text Value',/dontshow
  
  ;use this function to handle all the features that are duplicated between text and annotations
  thm_ui_zaxis_set_value_text_formats,textobj,tlb,statusBar,historywin,'t','Text'
  
  ;set label orientation
  vert = widget_info(tlb,find_by_uname='tvert')
  vertvalue = widget_info(vert,/button_set)
  zaxis->setProperty,labelOrientation=vertvalue
 ; statusBar->update,'Set label orientation'
  historyWin->update,'Set label orientation',/dontshow
  
  ;set label margin
  margin = widget_info(tlb,find_by_uname='tmargin')
  widget_control,margin,get_value=marginvalue
  if marginvalue le 0 then begin
    statusBar->update,'Error: label margin cannot be 0 or less'
    historyWin->update,'Error: label margin cannot be 0 or less'
    zaxis->getProperty,labelmargin=marginvalue
    widget_control,margin,set_value=marginvalue
  endif else begin
    if ~finite(marginvalue,/nan) then begin
      zaxis->setProperty,labelmargin=marginvalue
;      statusBar->update,'Set label margin'
      historyWin->update,'Set label margin',/dontshow
    endif else begin
      statusBar->update,'Invalid label margin, please re-enter.'
      historyWin->update,'Invalid label margin, value not set.',/dontshow
    endelse
  endelse
 
  
end

pro thm_ui_zaxis_set_value_color_table,zaxis,tlb,statusBar,historyWin

  compile_opt idl2,hidden

  colorTableNames = zaxis->getColorTables()
  
  for i = 0,n_elements(colorTableNames)-1 do begin
  
    button = widget_info(tlb,find_by_uname=colorTableNames[i])
    if widget_info(button,/button_set) then begin
      zaxis->setProperty,colorTable=i
     ; statusBar->update,'Updating color table'
      historyWin->update,'Updating color table',/dontshow
      break
    endif
    
  endfor

end

pro thm_ui_zaxis_set_value_placement,zaxis,tlb,statusBar,historyWin

  compile_opt idl2,hidden
  
  placementNames = zaxis->getPlacements()
  
  for i = 0,n_elements(placementNames)-1 do begin
  
    button = widget_info(tlb,find_by_uname=placementNames[i])
    if widget_info(button,/button_set) then begin
      zaxis->getProperty,placement=j
      zaxis->setProperty,placement=i
     ;statusBar->update,'Updating placement'
      historyWin->update,'Updating placement',/dontshow
      break
    endif
    
  endfor
  
end

;range and scaling settings
pro thm_ui_zaxis_set_value_range,zaxis,tlb,statusBar,historyWin

  compile_opt idl2,hidden
 
  ;fixed range flag
  fixed = widget_info(tlb,find_by_uname='fixed')
  fixedvalue = widget_info(fixed,/button_set)
  zaxis->getProperty,fixed=oldfixed
  zaxis->setProperty,fixed=fixedvalue
 ; statusBar->update,'Updated fixed value'
  historyWin->update,'Updated fixed value',/dontshow
  
  ;scaling flag
  linear = widget_info(tlb,find_by_uname='linear')
  linearvalue = widget_info(linear,/button_set)
  log10 = widget_info(tlb,find_by_uname='log10')
  log10value = widget_info(log10,/button_set)
  natural = widget_info(tlb,find_by_uname='natural')
  naturalvalue = widget_info(natural,/button_set)
  
  if log10value then begin
    scaling = 1
  endif else if naturalvalue then begin
    scaling = 2
  endif else begin
    scaling = 0
  endelse
  
  zaxis->getProperty,scaling=oldscaling
  zaxis->setProperty,scaling=scaling
 
  ;statusBar->update,'Updated scaling value'
  historyWin->update,'Updated scaling value',/dontshow
  
  ;minimum and maximum values
  
  minimum = widget_info(tlb,find_by_uname='minimum')
  widget_control,minimum,get_value=minimumvalue
  
  maximum = widget_info(tlb,find_by_uname='maximum')
  widget_control,maximum,get_value=maximumvalue

  if finite(maximumvalue,/nan) && finite(minimumvalue,/nan) then begin
    statusBar->update,'Invalid Fixed Maximum & Minimum, please re-enter.'
    historyWin->update,'Invalid Fixed Maximum & Minimum , values not applied.', /dontshow
  endif else if finite(maximumvalue,/nan) then begin
    statusBar->update,'Invalid Fixed Maximum, please re-enter.'
    historyWin->update,'Invalid Fixed Maximum, values not applied.', /dontshow
  endif else if finite(minimumvalue,/nan) then begin
    statusBar->update,'Invalid Fixed Minimum, please re-enter.'
    historyWin->update,'Invalid Fixed Minimum, values not applied.', /dontshow
  endif else begin
    zaxis->getProperty,minrange=oldmin,maxrange=oldmax
    if minimumvalue ge maximumvalue then begin
      statusBar->update,'Cannot have a minimum range that is greater than maximum range. Resetting.'
      historyWin->update,'Cannot have a minimum range that is greater than maximum range. Resetting.'
      zaxis->getProperty,minrange=minimumvalue,maxrange=maximumvalue
      widget_control,minimum,set_value=minimumvalue
      widget_control,maximum,set_value=maximumvalue
     endif else if scaling ne 0 && maximumvalue le 0 then begin
      statusBar->update,'Cannot have a maximum range that is less than 0 on log z-axis. Resetting.'
      historyWin->update,'Cannot have a maximum range that is less than 0 on log z-axis. Resetting.'
      zaxis->getProperty,minrange=minimumvalue,maxrange=maximumvalue
      widget_control,minimum,set_value=minimumvalue
      widget_control,maximum,set_value=maximumvalue
    endif else if scaling ne 0 && minimumvalue lt 0 then begin
      statusBar->update,'Cannot have a minimum range that is less than 0 on log z-axis. Resetting.'
      historyWin->update,'Cannot have a minimum range that is less than 0 on log z-axis. Resetting.'
      zaxis->getProperty,minrange=minimumvalue,maxrange=maximumvalue
      widget_control,minimum,set_value=minimumvalue
      widget_control,maximum,set_value=maximumvalue
    endif else if scaling ne 0 && maximumvalue lt 0 then begin
      statusBar->update,'Cannot have a minimum range that is less than 0 on log z-axis. Resetting.'
      historyWin->update,'Cannot have a minimum range that is less than 0 on log z-axis. Resetting.'
      zaxis->getProperty,minrange=minimumvalue,maxrange=maximumvalue
      widget_control,minimum,set_value=minimumvalue
      widget_control,maximum,set_value=maximumvalue
    endif else begin
      zaxis->setProperty,minRange=minimumvalue,maxrange=maximumvalue
    ;  statusBar->update,'Min & Max range set.'
      historyWin->update,'Min & Max range set.',/dontshow
    endelse
    
  endelse 

end

;This procedure will set values for the tick spinners and the margin spinner
pro thm_ui_zaxis_set_value_ticks,zaxis,tlb,statusBar,historyWin

  compile_opt idl2,hidden
 
  margin = widget_info(tlb,find_by_uname='margin')
  widget_control,margin,get_value=marginnum

  if ~finite(marginnum,/nan) then begin
    if marginnum lt 0 then begin
      statusBar->update,'Cannot have a negative margin'
      historyWin->update,'Cannot have a negative margin'
      ;Update spinner with integerized or reset value
      zaxis->getProperty,margin=marginnum
      widget_control,margin,set_value=marginnum
    endif else begin
      zaxis->setProperty,margin=marginnum
      historyWin->update,'Margin Updated',/dontshow
    endelse
  endif else begin
    statusBar->update,'Invalid margin value, please re-enter.'
    historywin->update,'Invalid margin, value not applied.', /dontshow 
  endelse
  
  zaxis->getProperty,autoticks=oldauto
  autoticks = widget_info(tlb,find_by_uname='autoticks')
  autoval = widget_info(autoticks,/button_set)
  zaxis->setProperty,autoticks=autoval

  major = widget_info(tlb,find_by_uname='nmajorticks')
  widget_control,major,get_value=majornum 
 
  if ~finite(majornum,/nan) then begin
    if majornum lt 0 then begin
      statusBar->update,'Cannot have a negative number of ticks'
      historyWin->update,'Cannot have a negative number of ticks'
      zaxis->getProperty,tickNum=majornum
      widget_control,major,set_value=majornum
    endif else begin
      zaxis->getProperty,ticknum=oldticknum
      zaxis->setProperty,tickNum=majornum
   
      historyWin->update,'major Ticks Updated',/dontshow
    endelse
  endif else begin
    statusBar->update,'Invalid number of major ticks, please re-enter.'
    historywin->update,'Invalid number of major ticks, value not applied.', /dontshow 
  endelse
  
  minor = widget_info(tlb,find_by_uname='nminorticks')
  widget_control,minor,get_value=minornum 
 
  if ~finite(minornum,/nan) then begin
    if minornum lt 0 then begin
      statusBar->update,'Cannot have a negative number of ticks'
      historyWin->update,'Cannot have a negative number of ticks'
      zaxis->getProperty,minorTickNum=minornum
      widget_control,minor,set_value=minornum
    endif else begin
      zaxis->getProperty,ticknum=oldminornum
      zaxis->setProperty,minorTickNum=minornum
   
      historyWin->update,'Minor Ticks Updated',/dontshow
    endelse
  endif else begin
    statusBar->update,'Invalid number of minor ticks, please re-enter.'
    historywin->update,'Invalid number of minor ticks, value not applied.', /dontshow 
  endelse

  for i = 0,3 do begin
    id = widget_info(tlb,find_by_uname='logminorticktype'+strtrim(i,2))
    if widget_info(id,/button_set) then begin
      zaxis->setProperty,logminorticktype=i
    endif
  endfor

end

pro thm_ui_zaxis_color_event,tlb,uname,messagename,historywin,statusbar

  compile_opt idl2,hidden
  
  colorwindow = widget_info(tlb,find_by_uname=uname)
  Widget_Control, colorwindow, Get_Value=colorWin
  ColorWin->getProperty,graphics_tree=scene
  scene->getProperty,color=currcolor
  color = PickColor(!p.color, Group_Leader=tlb, Cancel=cancelled,currentcolor=currcolor)
  if ~cancelled then begin

    scene->setProperty,color=reform(color)
    Colorwin->draw
 
    historyWin->Update,messagename + ' color changed.',/dontshow
    statusbar->Update,messagename + ' color changed.'
  endif

end


PRO thm_ui_zaxis_options_event, event

  Compile_Opt hidden

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
    endif
    Print, 'Error--See history'
    ok=error_message('An unknown error occured and the window must be restarted. See console for details.',$
       /noname, /center, title='Error in Z-Axis Options')
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin 
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF

    ;kill request block

  IF (Tag_Names(event, /Structure_Name) EQ 'WIDGET_KILL_REQUEST') THEN BEGIN  
    ; reset values for each z axis
    IF N_Elements(state.zAxes) GE 1 && ~in_set(obj_valid(state.zAxes),'0') THEN BEGIN
      FOR i=0, N_Elements(state.zAxes)-1 DO BEGIN
        state.spectraPanels[i]->Reset
      ENDFOR
    ENDIF
    state.drawObject->Update, state.windowStorage, state.loadedData
    state.drawObject->Draw
    
    ; exit 
    Print, 'widget killed'
    state.historywin->update,'THM_UI_ZAXIS_OPTIONS: widget killed' 
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy    
    Widget_Control, event.top, /Destroy
    RETURN      
  ENDIF

   ;redraw palettes and write history about swap
  IF (Tag_Names(event, /Structure_Name) EQ 'WIDGET_TAB') THEN BEGIN  
  
 
    Widget_Control, state.zAxisArea, Get_Value=win
    win->Draw    
    widget_control, state.colorWindow, get_value=colorWin
    colorWin->draw
    widget_control, state.acolorWindow, get_value=acolorWin
    acolorWin->draw
   
    state.historywin->update,'THM_UI_ZAXIS_OPTIONS: tab switched to: ' + $
                              strtrim(widget_info(state.tabbase, /tab_current))
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    RETURN 
  ENDIF

   ; Get the instructions from the Widget causing the event and
   ; act on them.

  Widget_Control, event.id, Get_UValue=uval

  IF Size(uval, /Type) NE 0 THEN BEGIN
    state.historywin->update,'THM_UI_ZAXIS_OPTIONS: User value: '+uval, /dontshow
    CASE uval OF
      'CANC': BEGIN
        Print, 'Panel Widget canceled' 
        ; for each panel reset z axis
        IF N_Elements(state.zAxes) GE 1 && ~in_set(obj_valid(state.zAxes),'0') THEN BEGIN
           FOR i=0, N_Elements(state.zAxes)-1 DO BEGIN
              state.spectraPanels[i]->Reset
           ENDFOR
        ENDIF
        state.drawObject->Update, state.windowStorage, state.loadedData
        state.drawObject->Draw
        thm_ui_update_zaxis_from_draw,state.panels,state.drawObject,state.historyWin    
        Widget_Control, event.TOP, Set_UValue=state, /No_Copy
        Widget_Control, event.top, /Destroy
        RETURN
      END
      'APPLY': BEGIN
        IF Obj_Valid(state.zAxisSettings) && ~in_set(obj_valid(state.zAxes),'0') THEN BEGIN
          thm_ui_zaxis_set_value,state,event
          thm_ui_zaxis_propagate_settings, state
          state.zAxes[state.selectedPanel]=state.zAxisSettings
          state.drawObject->Update, state.windowStorage, state.loadedData,error=draw_error
          
          if draw_error then begin
            state.statusBar->update,'Draw Error, Attempting to revert settings'
            state.historyWin->update,'Draw Error, Attempting to revert settings'          
            IF N_Elements(state.zAxes) GE 1 && ~in_set(obj_valid(state.zAxes),'0') THEN BEGIN
              FOR i=0, N_Elements(state.zAxes)-1 DO BEGIN
                state.spectraPanels[i]->Reset
              ENDFOR
            ENDIF
            
            state.drawObject->Update, state.windowStorage, state.loadedData
          endif
          
          thm_ui_update_zaxis_from_draw,state.panels,state.drawObject,state.historyWin
          
          thm_ui_zaxis_update,state
          
          state.drawObject->Draw
        ENDIF ELSE BEGIN
          state.statusBar->Update, 'No panels or axes to apply.'
        ENDELSE
      END
      'OK': BEGIN
        thm_ui_zaxis_set_value,state,event
        thm_ui_zaxis_propagate_settings, state
        state.drawObject->Update, state.windowStorage, state.loadedData,error=draw_error
        
        if draw_error then begin
          state.statusBar->update,'Draw Error, Attempting to revert settings'
          state.historyWin->update,'Draw Error, Attempting to revert settings'          
          IF ~in_set(obj_valid(state.zAxes),'0') THEN BEGIN
            FOR i=0, N_Elements(state.zAxes)-1 DO BEGIN
              state.spectraPanels[i]->Reset
            ENDFOR
          ENDIF
        
          state.drawObject->Update, state.windowStorage, state.loadedData
        endif

        IF N_Elements(state.zAxes) GE 1 && ~in_set(obj_valid(state.zAxes),'0') THEN BEGIN
          FOR i=0, N_Elements(state.zAxes)-1 DO BEGIN
            state.spectraPanels[i]->setTouched
          ENDFOR
        ENDIF
        
        state.drawObject->Draw
        thm_ui_update_zaxis_from_draw,state.panels,state.drawObject,state.historyWin   
        Widget_Control, event.TOP, Set_UValue=state, /No_Copy
        Widget_Control, event.top, /Destroy
        RETURN        
      END
      'TEMP': begin
     
        ;make sure internal state is updated before save
        thm_ui_zaxis_set_value,state,event
        thm_ui_zaxis_propagate_settings, state
        state.zAxes[state.selectedPanel]=state.zAxisSettings
        
        IF N_Elements(state.zAxes) GE 1 && ~in_set(obj_valid(state.zAxes),'0') THEN BEGIN
          FOR i=0, N_Elements(state.zAxes)-1 DO BEGIN
            state.spectraPanels[i]->setTouched
          ENDFOR
        ENDIF
     
        if ~in_set(obj_valid(state.zAxes),'0') then begin
          state.template->setProperty,z_axis=state.zaxisSettings->copy()
          state.historywin->update,'Current Z-axis settings Saved to Template'
          state.statusBar->update,'Current Z-axis settings Saved to Template'
        endif else begin
          state.statusbar->update,'Cannot save template. Needs a valid spectral panel to save z-axis template.'
          state.historywin->update,'Cannot save template. Needs a valid spectral panel to save z-axis template.'
        endelse   
    ;  
      end
      'PANEL': BEGIN
         IF ~in_set(Obj_Valid(state.zAxes),'0') THEN  BEGIN     
           thm_ui_zaxis_set_value,state,event
           thm_ui_zaxis_propagate_settings, state
           state.selectedPanel = event.index 
           state.zAxisSettings = state.zAxes[event.index]
           thm_ui_zaxis_update, state
         ENDIF
      END
      'RAINBOW':BEGIN
        thm_ui_zaxis_switch_color_table,0,event.top
      END
      'COOL':BEGIN     
        thm_ui_zaxis_switch_color_table,1,event.top
      END
      'HOT':BEGIN
        thm_ui_zaxis_switch_color_table,2,event.top
      END
      'COPPER':BEGIN 
        thm_ui_zaxis_switch_color_table,3,event.top
      END
      'EXTREME HOT-COLD':BEGIN  
        thm_ui_zaxis_switch_color_table,4,event.top
      END
      'GRAY':BEGIN   
        thm_ui_zaxis_switch_color_table,5,event.top
      END
      'THEMIS':BEGIN    
        thm_ui_zaxis_switch_color_table,6,event.top
      END
      'FIXED': BEGIN
        ;this code switches sensitivity and fills in the range information
        result = Widget_Info(event.id, /Button_Set) 
        IF result EQ 1 THEN BEGIN
           state.spectrapanels[state.selectedpanel]->getproperty, id=selected
           info = state.drawobject->getpanelinfo(selected)
           case info.zscale of
             '0':begin
               widget_control, state.rangeminincrement, set_value=info.zrange[0]
               widget_control, state.rangemaxincrement, set_value=info.zrange[1]
               state.zaxissettings->setproperty, minrange=info.zrange[0]
               state.zaxissettings->setproperty, maxrange=info.zrange[1]
             end
             '1':begin
               widget_control, state.rangeminincrement, set_value=10^info.zrange[0]
               widget_control, state.rangemaxincrement, set_value=10^info.zrange[1]
               state.zaxissettings->setproperty, minrange=10^info.zrange[0]
               state.zaxissettings->setproperty, maxrange=10^info.zrange[1]
             end
             '2':begin
               widget_control, state.rangeminincrement, set_value=exp(info.zrange[0])
               widget_control, state.rangemaxincrement, set_value=exp(info.zrange[1])
               state.zaxissettings->setproperty, minrange=exp(info.zrange[0])
               state.zaxissettings->setproperty, maxrange=exp(info.zrange[1])
             end
           endcase
           Widget_Control, state.rangeMinIncrement, Sensitive=1
           Widget_Control, state.rangeMaxIncrement, Sensitive=1
        ENDIF ELSE BEGIN
           Widget_Control, state.rangeMinIncrement, Sensitive=0
           Widget_Control, state.rangeMaxIncrement, Sensitive=0
        ENDELSE
      END 
      'PALETTE': BEGIN
        thm_ui_zaxis_color_event,event.top,'tcolor','Text Color',state.historyWin,state.statusBar
      END 
      'APALETTE': BEGIN
        thm_ui_zaxis_color_event,event.top,'acolor','Annotation Color',state.historyWin,state.statusBar
      END 
      'AUTOTICKS':BEGIN
        id = widget_info(event.top,find_by_uname='ticksbase')
        widget_control,id,sensitive=~event.select
      END
      'LINEAR': begin
        id = widget_info(event.top,find_by_uname='logminorticktypebase')
        widget_control,id,sensitive=0
      end
      'LOG10': begin
        id = widget_info(event.top,find_by_uname='logminorticktypebase')
        widget_control,id,sensitive=1
      end
      'NATLOG': begin
        id = widget_info(event.top,find_by_uname='logminorticktypebase')
        widget_control,id,sensitive=1
      end
     ELSE :
    ENDCASE
  ENDIF
  
  Widget_Control, event.TOP, Set_UValue=state, /No_Copy
  
  RETURN
END ;--------------------------------------------------------------------------------



PRO thm_ui_zaxis_options, gui_id, windowStorage, zaxisSettings, drawObject, loadedData,historywin,template,panel_select=panel_select

   ; kill top base in case of init error  
  catch, err
  if err ne 0 then begin
    catch, /cancel
    
    help, /last_message, output=err_msg
    for i = 0, N_Elements(err_msg)-1 do historywin->update,err_msg[i]
    print, 'Error--See history'
    ok = error_message('An error occured while starting Z-Axis Options.',$
         /noname, /center, title='Error in Z-Axis Options')

    widget_control, tlb, /destroy
    
    thm_gui_error, gui_id, historywin
    
    return
  endif
  
    ; build top level and main tab bases
    
  tlb = Widget_Base(/Col, title='THEMIS: Z Axis Options ', Group_Leader=gui_id, $
                   /Modal, /Floating, /TLB_KILL_REQUEST_EVENTS, tab_mode=1)
  
     ; Z Axis Bases
     
  tabBase = Widget_Tab(tlb, Location=location)
  buttonBase = Widget_Base(tlb, /Row, /align_center)
  statusBase = Widget_Base(tlb, /Row, /align_center)
  settingsBase = Widget_Base(tabBase, Title='Settings', /Col)    
  textBase = Widget_Base(tabBase, Title='Text', /Col)

    ; Settings Bases
           
    panelsBase = Widget_Base(settingsBase, /Row)
      middleBase = Widget_Base(settingsBase, /Row)
        col1Base = Widget_Base(middleBase, /Col, ypad=6, space=2)
          colorTableBase = Widget_Base(col1Base, /Col)
          colorFrameBase = Widget_Base(col1Base, /Col, /Exclusive, Frame=3)
          rangeLabelBase = Widget_Base(col1Base, /Col)
          rangeFrameBase = Widget_Base(col1Base, /Col, Frame=3, YPad=5)
        col2Base = Widget_Base(middleBase, /Col, XPad=10, space=2)
          scalingLabelBase = Widget_Base(col2Base, /Col)
          scalingBase = Widget_Base(col2Base, /col, Frame=3, /Exclusive)
          placementLabelBase = Widget_Base(col2Base, /Col)
          placementBase = Widget_Base(col2Base, /Col, Frame=3, /Exclusive)
          autotickBase = widget_base(col2Base,/col,/align_left,/nonexclusive)
          tickBase = Widget_Base(col2Base, /col, /align_right, ypad=4,frame=1,uname='ticksbase')
          ;anoLabelBase = Widget_Base(col2Base, /Col)           
          ;anoBase = Widget_Base(col2Base, /Col, Frame=3)           
          marginBase = Widget_Base(col2Base, /col, /align_right)
        col3Base = Widget_Base(middleBase, /Col, YPad=4, XPad=5)
      bottomBase = widget_base(settingsBase, /col, /align_center)
     
         
    ; Text Bases
           
    tpanelsBase = Widget_Base(textBase, /Row)
    labelBase = Widget_Base(textBase, /Col)
    annotationBase = Widget_base(textBase, /Col)
    tsetallbase = widget_base(textBase, /col, /align_center, /nonexclusive,  ypad=4)
    
    ;retrieve data zaxis, and panel info for display   
  activeWindow = windowStorage->GetActive()
  IF ~Obj_Valid(activeWindow) THEN BEGIN
    panelNames=['No Panels']  
  ENDIF ELSE BEGIN
    activeWindow->GetProperty, Panels=panels
    IF ~Obj_Valid(panels) THEN BEGIN
      panelNames=['No Panels']  
    ENDIF ELSE BEGIN
      panelObjs = panels->Get(/all)
      IF Is_Num(panelObjs) THEN BEGIN
        panelNames=['No Panels'] 
      ENDIF ELSE BEGIN
        FOR i=0, N_Elements(panelObjs)-1 DO BEGIN
          panelObjs[i]->Save
          panelObjs[i]->GetProperty, Name=name, ZAxis=zaxis
          IF Obj_Valid(zAxis) THEN BEGIN
             zAxis->Save
             IF Size(panelNames, /type) EQ 0 THEN BEGIN
                panelNames=[name]
                spectraPanels=[panelObjs[i]]
                zAxes=[zAxis] 
             ENDIF ELSE BEGIN
                panelNames=[panelNames, name]
                spectraPanels=[spectraPanels, panelObjs[i]]
                zAxes=[zAxes, zAxis]
             ENDELSE
             
             if n_elements(panel_select) gt 0 && i eq panel_select then begin
               selected_zpanel = n_elements(panelNames)-1
             endif
             
          ENDIF
        ENDFOR
      ENDELSE
    ENDELSE
    IF Size(panelNames, /type) EQ 0 THEN BEGIN
       panelNames=['No Panels with Z Axes'] 
       zAxes=-1
    ENDIF   
    IF Is_Num(panelNames) THEN BEGIN
       panelNames=['No Panels with Z Axes'] 
       zAxes=-1
    ENDIF 
    IF N_Elements(panelNames) EQ 1 && panelNames EQ '' THEN BEGIN
       panelNames=['No Panels with Z Axes']
       zAxes=-1
    ENDIF
  ENDELSE
  
  if size(selected_zpanel, /type) eq 0 then selected_zpanel = 0
  
  IF Size(spectraPanels, /type) NE 0 && Obj_Valid(spectraPanels[0]) $
    THEN spectraPanels[selected_zpanel]->GetProperty, ZAxis=zAxisSettings $
      ELSE spectraPanels=-1 

  IF ~Obj_Valid(zAxisSettings) THEN zAxisSettings=Obj_New("THM_UI_ZAXIS_SETTINGS")
  
  zAxisSettings->GetProperty, $
      ColorTable=colortable, $
      Fixed=fixed, $
      MinRange=minrange, $
      MaxRange=maxrange, $
      Scaling=scaling, $
      Placement=placement, $
      TickNum=ticknum, $
      minorTickNum=minorTickNum,$
      Margin=margin, $
      LabelTextObject=labeltextobject, $
      LabelMargin=labelmargin, $
      AnnotateTextObject=annotatetextobject, $
      LabelOrientation=labelorientation, $
      AnnotationOrientation=annotationorientation,$
      annotationStyle=annotationStyle, $
      annotateExponent=annotateExponent
 
  IF ~Obj_Valid(labeltextobject) THEN labeltextobject=Obj_New("THM_UI_TEXT")
  IF ~Obj_Valid(annotatetextobject) THEN annotatetextobject=Obj_New("THM_UI_TEXT")
               
                    ;widgets for Settings Tab
                    
  plabel = Widget_Label(panelsBase, value='Panels: ')
  sxaxisDroplist = Widget_combobox(panelsBase, XSize=350, $
    Value=panelNames, UValue='PANEL', uname = 'sPanels')
    
  ;swap the default, if necessary
  if keyword_set(selected_zpanel) then begin
  
    widget_control,sxaxisDroplist,set_combobox_select=selected_zpanel
  
  endif else begin
  
    selected_zpanel = 0
  
  endelse
    
  colorTableLabel = Widget_Label(colorTableBase, Value ='Color Table:')
  colorTableNames = zaxisSettings->GetColorTables()
  colorTableButtons = make_array(N_Elements(colorTableNames), /Long)
  FOR i=0,N_Elements(colorTableNames)-1 DO BEGIN
    colorTableButtons[i]= Widget_Button(colorFrameBase, Value=colorTableNames[i], $
      UValue=StrUpCase(colorTableNames[i]),uname=colorTableNames[i])
  ENDFOR
  Widget_Control, colorTableButtons[colortable], /Set_Button
  rangeLabelLabel =  Widget_Label(rangeLabelBase, Value ='Range:')
  fixedBase =  Widget_Base(rangeFrameBase, /Row, /nonexclusive)
  fixedButton=Widget_Button(fixedBase, Value='Fixed Min/Max', /align_center, UValue='FIXED',uname='fixed')
  IF fixed EQ 1 THEN Widget_Control, fixedButton, /Set_Button
  IF fixed EQ 1 THEN sensitive=1 ELSE sensitive=0
  rangeMinBase = Widget_Base(rangeFrameBase, /Row)
  rangeMinIncrement = thm_ui_spinner(rangeMinBase, label= 'Min: ', Value=minRange, $
    text_box_size=12, xlabelsize=26, UValue='MINR', sensitive=sensitive,uname='minimum')
  rangeMaxBase = Widget_Base(rangeFrameBase, /Row)
  rangeMaxIncrement = thm_ui_spinner(rangeMaxBase, label= 'Max: ', Value=maxRange, $
    text_box_size=12, xlabelsize=26, UValue='MAXR', sensitive=sensitive,uname='maximum')
  
  scalingLabel = Widget_Label(scalingLabelBase, Value='Scaling:')
  slinearButton = Widget_Button(scalingBase, Value= 'Linear', UValue='LINEAR',uname='linear')
  slogButton = Widget_Button(scalingBase, Value= 'Log 10', UValue='LOG10',uname='log10')
  snatButton = Widget_Button(scalingBase, Value= 'Natural Log', UValue='NATLOG',uname='natural')
  IF scaling EQ 0 THEN Widget_Control, slinearButton, /Set_Button
  IF scaling EQ 1 THEN Widget_Control, slogButton, /Set_Button
  IF scaling EQ 2 THEN Widget_Control, snatButton, /Set_Button
  placementLabel = Widget_Label(placementLabelBase, Value='Placement:')
  placeValues = zaxisSettings->GetPlacements()
  plinearButtons = make_array(N_Elements(placeValues), /Long)
  FOR i=0,N_Elements(placeValues)-1 DO BEGIN
    plinearButtons[i]=Widget_Button(placementBase, Value=placeValues[i], UValue=Strupcase(placeValues[i]),uname=placeValues[i])
  ENDFOR
  Widget_Control, plinearButtons[placement], /Set_Button  
  
  autobutton = widget_button(autotickbase,value='Automatic Ticks',uname='autoticks',UVALUE='AUTOTICKS')
  
  ticknum = thm_ui_spinner(tickBase, label= 'Major Ticks(#):  ', Increment=1, Value=ticknum, UValue='NTICKS',uname='nmajorticks')
  minorTickNum = thm_ui_spinner(tickBase, label= 'Minor Ticks(#):  ', Increment=1, Value=minorticknum, UValue='NMINORTICKS',uname='nminorticks')
  margin = thm_ui_spinner(marginBase, label= 'Margin:  ', Increment=1, Value=margin, $
     uval='MARGIN', /all_events,sensitive=barmarginsensitive,uname='margin')

  minorLogTickTypeBase = widget_base(bottomBase,/col,/frame,/base_align_center)
  minorLogTickTypeLabelBase = widget_base(minorLogTickTypeBase,/base_align_center,/row)
  minorLogTickTypeLabel = widget_label(minorLogTickTypeLabelBase,value="Log Minor Tick Type:",/align_center)
  minorLogTickTypeButtonBase = widget_base(minorLogTickTypeBase,/exclusive,/row,uname='logminorticktypebase')
  minorLogTickType0 = widget_button(minorLogTickTypeButtonBase,value='Full Interval',uname='logminorticktype0')
  minorLogTickType1 = widget_button(minorLogTickTypeButtonBase,value='First Magnitude',uname='logminorticktype1')
  minorLogTickType2 = widget_button(minorLogTickTypeButtonBase,value='Last Magnitude',uname='logminorticktype2')
  minorLogTickType3 = widget_button(minorLogTickTypeButtonBase,value='Even Spacing',uname='logminorticktype3')
  
  lowerBase = widget_base(bottomBase, /col, /align_center, ypad=4, /nonexclusive)
  ssetallbutton = widget_button(lowerbase, value='Set All Panels ', uname='ssetall')

  ; Text Tab Widgets

  tplabel = WIDGET_LABEL(tpanelsBase, value = 'Panels: ')
  txaxisDroplist = Widget_combobox(tpanelsBase,XSize=350, $
    Value=panelNames, UValue='PANEL', uname = 'tPanels')
  widget_control,txaxisDroplist,set_combobox_select=selected_zpanel
  textLabel = Widget_Label(labelBase, value = 'Label:', /align_left)
  labelFrame = Widget_Base(labelBase, /col, frame=3)
  styleNames = zaxisSettings->GetAnnotations()  
  textObj = Obj_New("THM_UI_TEXT")
  fontNames = textObj->GetFonts()
  formatNames = textObj->GetFormats()
  Obj_Destroy, textObj  
  fontBase = Widget_Base(labelFrame, /row, space=3)
  fcol1Base = Widget_Base(fontBase, /col, space=14)
  fcol2Base = Widget_Base(fontBase, /col, space=6)
  fcol3Base = Widget_Base(fontBase, /col, space=17)
  fcol4Base = Widget_Base(fontBase, /col, space=4)
  textLabel=Widget_Label(fcol1Base, value='Text:', /align_left)
  zAxisSettings->GetProperty, LabelTextObject=labeltextobject
  IF Obj_Valid(labeltextobject) THEN labeltextobject->GetProperty, Value=labelValue ELSE labelValue=''
;  varNameText = Widget_Text(variableBase, value=metaData[0].name, uValue='NAME', /editable, xsize=20)             
  textLabelBox=Widget_Text(fcol2Base, value=labelValue, /editable, xsize=25, uValue='TEXT', /all_events,uname='text')
  fontLabel=Widget_Label(fcol1Base, value='Font: ', /align_left)
  fontDroplist = Widget_combobox(fcol2Base, Value=fontNames, UValue='FONT',uname='tfont')
  IF ~Obj_Valid(labelTextObject) THEN BEGIN
    font = 0
    format = n_elements(formatnames)-1  ;correct for no formatting
    size=12
    color=[0,0,0]
  ENDIF ELSE BEGIN
     labeltextobject->GetProperty, Font=font, Format=format, Size=size, Color=color
  ENDELSE
  Widget_Control, fontDroplist, Set_combobox_Select=font 
  incLabel=Widget_Label(fcol3Base, value='Size(pts) :', /align_left)
  fontIncrement = thm_ui_spinner(fcol4Base, Increment=1, Value=size, UValue='SIZE',uname='tsize')
  colorBase = Widget_Base(fcol4Base, /row, space=1)
  formatLabel=Widget_Label(fcol1Base, value='Format: ',/align_left)
  formatDroplist = Widget_combobox(fcol2Base, Value=formatNames, UValue='FORMAT',uname='tformat') 
  IF format eq -1 then format=n_elements(formatnames)-1   ; correct for no formating
  Widget_Control, formatDroplist, Set_combobox_Select=format 
  colorLabel = Widget_Label(fcol3Base, Value='Color: ', /align_left)
  
  getresourcepath,rpath
  palettebmp = read_bmp(rpath + 'color.bmp', /rgb)
  thm_ui_match_background, tlb, palettebmp
  
  paletteButton = Widget_Button(colorBase, Value=palettebmp, /bitmap, $
    UValue='PALETTE', Tooltip='Choose color from Palette')
  spaceLabel = Widget_Label(colorBase, Value=' ')    
  colorWindow = WIDGET_DRAW(colorBase, graphics_level=2,renderer=1, retain=1,  XSize=50, YSize=21, units=0, frame=1,uname='tcolor',/expose_events)
  orientationBase = Widget_Base(labelFrame, /row, xpad=5)
  orientationLabel = Widget_Label(orientationBase, value='Orientation: ', /align_left)
  orientBase = Widget_Base(orientationBase, /row, /exclusive, frame=3, ypad=3)
  horizontalButton = Widget_Button(orientBase, value='Horizontal', UValue='HORIZ',uname='thoriz')
  verticalButton = Widget_Button(orientBase, value='Vertical', UValue='VERT',uname='tvert') 
  IF labelorientation EQ 0 THEN Widget_Control, horizontalButton, /Set_Button $
     ELSE Widget_Control, verticalButton, /Set_Button
  margLabel=Widget_Label(fcol3Base, value='Label Margin (pts): ')
  margABase = Widget_Base(fcol4Base, /Row)
  labelmarg = thm_ui_spinner(margABase, Increment=1, Value=labelmargin, $
  uval='LABELMARGIN', /all_events,sensitive=labelmarginsensitive,uname='tmargin')
  
  anoLabel = Widget_Label(annotationBase, value = 'Annotation:', /align_left)
  anoFrameBase = Widget_Base(annotationBase, /col, frame=3)
  afontBase = Widget_Base(anoFrameBase, /row, space=3)
  afcol1Base = Widget_Base(afontBase, /col, space=17)
  afcol2Base = Widget_Base(afontBase, /col, space=6)
  afcol3Base = Widget_Base(afontBase, /col, space=17)
  afcol4Base = Widget_Base(afontBase, /col, space=4)
  afontLabel=Widget_Label(afcol1Base, value='Font: ', /align_left)
  annotatetextobject->GetProperty, Font=font, Format=format, Size=size, Color=acolor
  afontDroplist = Widget_combobox(afcol2Base, Value=fontNames, UValue='AFONT',uname='afont')
  Widget_Control, afontDroplist, Set_combobox_Select=font 
  aincLabel=Widget_Label(afcol3Base, value='Size(pts) : ', /align_left)
  afontIncrement = thm_ui_spinner(afcol4Base, Increment=1, Value=size, UValue='ASIZE',uname='asize')
  acolorBase = Widget_Base(afcol4Base, /row, space=1)
  aformatLabel=Widget_Label(afcol1Base, value='Format: ', /align_left)
  aformatDroplist = Widget_combobox(afcol2Base, Value=formatNames, UValue='AFORMAT',uname='aformat') 
  Widget_Control, aformatDroplist, Set_combobox_Select=format 
  acolorLabel = Widget_Label(afcol3Base, Value='Color: ', /align_left)
  apaletteButton = Widget_Button(acolorBase, Value=palettebmp, /bitmap, $
    UValue='APALETTE', Tooltip='Choose color from Palette')
  spaceLabel = Widget_Label(acolorBase, Value=' ')    
  acolorWindow = WIDGET_DRAW(acolorBase, graphics_level=2,renderer=1, retain=1,  XSize=50, YSize=21, units=0, frame=1,uname='acolor',/expose_events)
  
  anoSOBase = widget_base(anoFrameBase, /row, /exclusive, ypad=2, space=0)
  default = widget_button(anoSOBase, value = 'Auto-Notation', uvalue='AAUTO', uname='aauto')
  dbl = widget_button(anoSOBase, value = 'Decimal Notation', uvalue='ADBL', uname='adbl')
  expo = widget_button(anoSOBase, value = 'Scientific Notation', uvalue='AEXP', uname = 'aexp') 
  
  atype = [default, dbl, expo]
  widget_control, atype[annotateExponent], /set_button
  
  
  aorientationBase = Widget_Base(anoFrameBase, /row, xpad=5)
  aorientationLabel = Widget_Label(aorientationBase, value='Orientation: ', /align_left)
  aorientBase = Widget_Base(aorientationBase, /row, /exclusive, frame=3, ypad=3)
  ahorizontalButton = Widget_Button(aorientBase, value='Horizontal', UValue='AHORIZ',uname='ahoriz')
  averticalButton = Widget_Button(aorientBase, value='Vertical', UValue='AVERT',uname='avert') 
  IF annotationorientation EQ 0 THEN Widget_Control, ahorizontalButton, /Set_Button $
     ELSE Widget_Control, averticalButton, /Set_Button
  annotationsBase=Widget_Base(aorientationBase, /row)
  annotationsLabel=Widget_Label(afcol1Base, value='Annotation Precision: ', /align_left)
  annotations=zAxisSettings->GetAnnotations()
  annotationDroplist=Widget_combobox(afcol2Base, value=annotations, uValue='ANNOTATIONFORMAT',uname='astyle')
  widget_control,annotationDroplist,set_combobox_select=annotationstyle
  
 
  sampleLabel = Widget_Label(col3Base, Value='Sample:')
  specAreaBase = Widget_Base(col3Base, /Row)
  colorBase = widget_base(specAreaBase,frame=1)
  zaxisArea = Widget_Draw(colorBase,uname='colorbar',graphics_level=2,xsize=24,ysize=295,renderer=1,retain=1,/expose_events)
  
 ; colorBar = Obj_New("COLORBAR", Title='Colorbar Values', Vertical=1, Position=[0,0,1,1])

  ssetallbutton = widget_button(tsetallbase, value='Set All Panels ', uname='asetall')
  
  okButton = Widget_Button(buttonBase, Value='OK', XSize=75, UValue='OK')
  applyButton = Widget_Button(buttonBase, Value='Apply', XSize=75, UValue='APPLY')
  cancelButton = Widget_Button(buttonBase, Value='Cancel', UValue='CANC', XSize=75)
  templateButton = Widget_Button(buttonBase, Value='Save to Template', UValue='TEMP',xsize=125)
  
  IF N_Elements(zAxes) GE 1 && ~in_set(obj_valid(zaxes),'0') THEN BEGIN
     FOR i=0, N_Elements(zAxes)-1 DO BEGIN
             zOrig = zAxes[i]->Copy()
             IF i EQ 0 THEN zOriginals=[zOrig] ELSE zOriginals=[zOriginals,zOrig]
     ENDFOR
  ENDIF ELSE BEGIN
     zOriginals=-1
     zAxes=-1
  ENDELSE
  
  statusBar = Obj_New('THM_UI_MESSAGE_BAR', statusBase, XSize=75, YSize=1)
 
  ;NOTE TO FUTURE DEVELOPERS
  ;Any modifications should port over to not including the value in the state
  ;and using a uname/find_by_uname paradigm.
  state = {tlb:tlb, gui_id:gui_id, winID:0, tabBase:tabBase, panelNames:panelNames, zaxisSettings:zaxisSettings, statusBar:statusBar, $
           rangeMinIncrement:rangeMinIncrement, rangeMaxIncrement:rangeMaxIncrement, $
           colorWindow:colorWindow, aColorWindow:aColorWindow, atype:atype, $
           spectraPanels:spectraPanels,panels:panels, colorTableButtons:colorTableButtons, fixedButton:fixedButton, $
           slinearButton:slinearButton, slogButton:slogButton, snatButton:snatButton, $
           plinearButtons:plinearButtons, labelmarg:labelmarg, selectedPanel:selected_zpanel, $
           horizontalButton:horizontalButton, verticalButton:verticalButton, $
           fontDroplist:fontDroplist, fontIncrement:fontincrement, formatDroplist:formatDroplist, $
           afontDroplist:afontDroplist, afontIncrement:afontincrement, aformatDroplist:aformatDroplist, $
           ahorizontalButton:ahorizontalButton, averticalButton:averticalButton, annotationDroplist:annotationDroplist, $
           colorTable:colorTable, zAxisArea:zAxisArea, drawObject:drawObject, loadedData:loadedData, $
           windowStorage:windowStorage, zAxes:zAxes, zOriginals:zOriginals, panelObjs:panelObjs, historywin:historywin,template:template}

  Widget_Control, tlb, Set_UValue=state, /No_Copy
  CenterTLB, tlb 
  Widget_Control, tlb, /Realize

  Widget_Control, tlb, Get_UValue=state, /No_Copy
  
  ;guarantee accurate settings are presented in panel
  state.drawObject->update, state.windowStorage, state.loadedData
  state.drawObject->draw
  thm_ui_update_zaxis_from_draw,state.panels,state.drawObject,state.historyWin
  
  thm_ui_zaxis_init_color, State=state
  thm_ui_zaxis_update,state
  Widget_Control, tlb, Set_UValue=state, /No_Copy
  

  
  historywin->update,'THM_UI_ZAXIS_OPTIONS: Panel started.'
  XManager, 'thm_ui_zaxis_options', tlb, /no_block

  RETURN
END
