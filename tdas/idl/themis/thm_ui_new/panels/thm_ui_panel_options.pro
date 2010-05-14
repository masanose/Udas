;+ 
;NAME:  
; thm_ui_panel_options
;PURPOSE:
; A widget interface for selecting data
;CALLING SEQUENCE:
; thm_ui_newfile, master_widget_id
;INPUT:
; master_widget_id = the id number of the widget that calls this
;OUTPUT:
; none, there are buttons to push for plotting, setting limits, not
; sure what else yet...
;HISTORY:
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-09-14 14:58:32 -0700 (Mon, 14 Sep 2009) $
;$LastChangedRevision: 6722 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_panel_options.pro $
;
;--------------------------------------------------------------------------------


pro thm_ui_panel_update,tlb,state=state

  compile_opt idl2, hidden
  
  statedef = ~(~size(state,/type))
  
  if ~statedef then begin
    Widget_Control, tlb, Get_UValue=state, /No_Copy  ;Only get STATE if it is not passed in.
  endif else begin
    tlb = state.tlb
  endelse
  
  ;make sure settings are copied
  thm_ui_init_panel_options,tlb,state=state
  
  ;now update
  state.drawObject->update,state.windowStorage,state.loadedData
  state.drawObject->draw
  
  ;now update panel coordinates with current info
  for i = 0,n_elements(state.panelObjs)-1 do begin
  
    info = state.drawObject->getPanelInfo(i)
    
    if is_struct(info) && obj_valid(state.panelObjs[i]) then begin
      newsize = state.drawObject->getpanelsize(info.xpos,info.ypos)
      state.panelObjs[i]->setPanelCoordinates,newsize
    endif
  
  endfor
  
  ;Mutate any structures to reflect current display settings
  thm_ui_init_panel_options,tlb,state=state
  
  if ~statedef then Widget_Control, tlb, Set_UValue=state, /No_Copy   ;Only put STATE if it was not passed in.
  
end      

pro thm_ui_init_panel_options, tlb, state=state

compile_opt idl2, hidden

statedef = ~(~size(state,/type))

if ~statedef then begin
  Widget_Control, tlb, Get_UValue=state, /No_Copy  ;Only get STATE if it is not passed in.
endif else begin
  tlb = state.tlb
endelse

; Get currently selected panel object and settings
cpanel = state.panelobjs[*state.panel_select]
IF ~Obj_Valid(cpanel) THEN BEGIN
   traceSettings=Obj_New('THM_UI_LINE_SETTINGS')
   panelSettings=Obj_New('THM_UI_PANEL_SETTINGS')
   panelSettings->GetProperty, titleobj=panelTitle
ENDIF ELSE BEGIN
   cpanel->GetProperty, traceSettings=traceSettings, settings=panelSettings
   panelSettings->GetProperty, titleobj=panelTitle
ENDELSE

; Set panel select
id = widget_info(state.tlb, find_by_uname='layoutpanel')
widget_control, id, set_combobox_select=*state.panel_select

; Get panel title and font options
paneltitle->GetProperty, value=value, font=titlefont, size=titlesize, color=titlecolor
id = widget_info(state.tlb, find_by_uname='paneltitle')
widget_control, id, set_value = value
id = widget_info(state.tlb, find_by_uname='titlecombo')
widget_control, id, set_combobox_select=titlefont
id = widget_info(state.tlb, find_by_uname='titlesize')
widget_control, id, set_value=titlesize

tcolorwindow = widget_info(tlb,find_by_uname='tcolorwindow')
Widget_Control, tcolorwindow, Get_Value=tcolorWin
scene=obj_new('IDLGRSCENE', color=titlecolor)
tcolorWin->setProperty,graphics_tree=scene
tcolorWin->draw

; Get title margin
panelSettings->GetProperty, titleMargin=titlemargin
id = widget_info(state.tlb, find_by_uname='titlemargin')
widget_control, id, set_value = titlemargin

; Get panel row, col, rowspan, colspan
panelSettings->GetProperty, row=row, col=col, rspan=rspan, cspan=cspan
id = widget_info(state.tlb, find_by_uname='row')
widget_control, id, set_value = row
id = widget_info(state.tlb, find_by_uname='col')
widget_control, id, set_value = col
id = widget_info(state.tlb, find_by_uname='rspan')
widget_control, id, set_value = rspan
id = widget_info(state.tlb, find_by_uname='cspan')
widget_control, id, set_value = cspan
  
; Get bottom/left position settings
panelSettings->GetProperty, bottom=bottom, bunit=bunit, left=left, lunit=lunit,lvalue=lvalue,bvalue=bvalue

unitNames=panelSettings->GetUnitNames()
id = widget_info(state.tlb, find_by_uname='botbutton')
widget_control, id, set_button=bottom
id = widget_info(state.tlb, find_by_uname='bvalue')
widget_control, id, set_value=strcompress(string(bvalue), /remove_all), sensitive=bottom
id = widget_info(state.tlb, find_by_uname='bunit')
widget_control, id, set_value=unitNames, set_combobox_select=bunit, sensitive=bottom

id = widget_info(state.tlb, find_by_uname='leftbutton')
widget_control, id, set_button=left
id = widget_info(state.tlb, find_by_uname='lvalue')
widget_control, id, set_value=strcompress(string(lvalue), /remove_all), sensitive=left
id = widget_info(state.tlb, find_by_uname='lunit')
widget_control, id, set_value=unitNames, set_combobox_select=lunit, sensitive=left


; Get width/height position settings
panelSettings->GetProperty, width=width, wunit=wunit, height=height, $
                            hunit=hunit, relvertsize=relvertsize,$
                            hvalue=hvalue,wvalue=wvalue
                                                        
id = widget_info(state.tlb, find_by_uname='widthbutton')
widget_control, id, set_button=width
id = widget_info(state.tlb, find_by_uname='wvalue')
widget_control, id, set_value=strcompress(string(wvalue), /remove_all), sensitive=width
id = widget_info(state.tlb, find_by_uname='wunit')
widget_control, id, set_value=unitNames, set_combobox_select=wunit, sensitive=width

id = widget_info(state.tlb, find_by_uname='heightbutton')
widget_control, id, set_button=height
id = widget_info(state.tlb, find_by_uname='hvalue')
widget_control, id, set_value=strcompress(string(hvalue), /remove_all), sensitive=height
id = widget_info(state.tlb, find_by_uname='hunit')
widget_control, id, set_value=unitNames, set_combobox_select=hunit, sensitive=height

id = widget_info(state.tlb, find_by_uname='relvertsize')
widget_control, id, set_value=relvertsize

; Get background color and initialize background color window
panelSettings->GetProperty, backgroundcolor=value
Widget_Control, state.bgcolorWindow, Get_Value=bgcolorWin
if obj_valid(scene) then scene->remove,/all
scene=obj_new('IDLGRSCENE', color=value)
bgcolorWin->draw, scene

; Get frame color/thickness and initialize frame color window
panelSettings->GetProperty, framecolor=value, framethick=framethick
Widget_Control, state.fcolorWindow, Get_Value=fcolorWin
if obj_valid(scene) then scene->remove,/all
scene=obj_new('IDLGRSCENE', color=value)
fcolorWin->draw, scene

id = widget_info(state.tlb, find_by_uname='framethick')
widget_control, id, set_value=framethick

state.historyWin->update,'THM_UI_PANEL_OPTIONS: Widgets updated.'

id = widget_info(state.tlb, find_by_uname='setallpanel')
setAllFlag = Widget_Info(id, /button_set)
IF setAllFlag THEN BEGIN
  panelSettings->GetProperty, titleobj=titleobj, titlemargin=titlemargin, bottom=bottom, $
    bvalue=bvalue, bunit=bunit, left=left, lvalue=lvalue, $
    lunit=lunit, width=width, wvalue=wvalue, wunit=wunit, $
    height=height, hvalue=hvalue, hunit=hunit, $
    backgroundcolor=backgroundcolor, framecolor=framecolor, $
    framethick=framethick        
  npanels = n_elements(state.panelobjs)        
  for i=0,npanels-1 do begin
    state.panelobjs[i]->GetProperty, settings=panelSettings          
    panelSettings->SetProperty, titleobj=titleobj->copy(), titlemargin=titlemargin, bottom=bottom, $
      bvalue=bvalue, bunit=bunit, left=left, lvalue=lvalue, $
      lunit=lunit, width=width, wvalue=wvalue, wunit=wunit, $
      height=height, hvalue=hvalue, hunit=hunit, $
      backgroundcolor=backgroundcolor, framecolor=framecolor, $
      framethick=framethick         
    state.panelobjs[i]->SetProperty, settings=panelSettings
  endfor
ENDIF

if ~statedef then Widget_Control, tlb, Set_UValue=state, /No_Copy   ;Only put STATE if it was not passed in.

END ;-----------------------------------------------------------------------



pro thm_ui_panel_options_set_dims, origWindow, cWindow, panelObjs

    compile_Opt idl2, hidden

if ~obj_valid(panelObjs[0]) then return

origWindow->getproperty, nrows=o_nrows, ncols=o_ncols

r=0
c=0
for i=0, n_elements(panelObjs)-1 do begin

  panelObjs[i]->getproperty, settings=panelSettings
  panelSettings->getproperty, row=row, col=col, rspan=rspan, cspan=cspan

  r = (row+rspan-1) > r
  c = (col+cspan-1) > c

endfor

r = o_nrows > r
c = o_ncols > c

cWindow->setproperty, nrows=r, ncols=c

end

;function to handle color changing events
;returns chosen color
function thm_ui_panel_options_color_event, tlb, panelsettings, colorwidget

  panelSettings->GetProperty, backgroundColor=currentcolor
  
  color = PickColor(!P.Color, Group_Leader=tlb, Cancel=cancelled, $
                    currentcolor=currentcolor)
  
  if cancelled then color=currentcolor
  
  Widget_Control, colorwidget, Get_Value=colorWin
  if obj_valid(scene) then scene->remove,/all
  scene=obj_new('IDLGRSCENE', color=reform(color))
  colorWin->draw, scene  

  return, color
end ;---------------------------------------


PRO thm_ui_panel_options_event, event

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
       /noname, /center, title='Error in Panel Options')
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin 
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF
  
  cpanel = state.panelobjs[*state.panel_select]
  IF ~Obj_Valid(cpanel) THEN cpanel = Obj_New('THM_UI_PANEL', 1)
  cpanel->GetProperty, tracesettings=tracesettings, settings=panelsettings, YAxis=yaxis
  

    ;kill request block

  IF (Tag_Names(event, /Structure_Name) EQ 'WIDGET_KILL_REQUEST') THEN BEGIN  
       ; reset
    state.origWindow->GetProperty, panels=origPanels, nrows=nrows, ncols=ncols          
    state.cWindow->SetProperty, panels=origPanels, nrows=nrows, ncols=ncols
    state.drawObject->update,state.windowStorage,state.loadedData
    state.drawObject->draw       
    state.historyWin->Update,'THM_UI_PANEL_OPTIONS: Panel Options window killed.'

    exit_sequence:
    Print, 'widget killed' 
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    Widget_Control, event.top, /Destroy
    RETURN      
  ENDIF

   ; Get the instructions from the widget causing the event and
   ; act on them.

  Widget_Control, event.id, Get_UValue=uval

  IF Size(uval, /Type) NE 0 THEN BEGIN

  state.historywin->update,'THM_UI_PANEL_OPTIONS: User value: '+uval  ,/dontshow

    CASE uval OF
      'APPLY': BEGIN
        thm_ui_panel_options_set_dims, state.origWindow, state.cWindow, state.panelObjs
        if thm_ui_check_overlap(state.panelobjs, state.cwindow[0]) then break
        thm_ui_panel_update,event.top, state=state
        state.historyWin->Update, 'Changes applied.'
        state.statusBar->Update, 'Changes applied.'
      END
      'CANC': BEGIN
        state.origWindow->GetProperty, panels=origPanels, nrows=nrows, ncols=ncols          
        state.cWindow->SetProperty, panels=origPanels, nrows=nrows, ncols=ncols
        state.drawObject->update,state.windowStorage,state.loadedData
        state.drawObject->draw       
        Print, 'Panel Options widget cancelled. No changes made.'
        state.historyWin->Update, 'Panel Options window cancelled. No changes made.'
        Widget_Control, event.TOP, Set_UValue=state, /No_Copy
        Widget_Control, event.top, /Destroy
        RETURN
      END
      'OK': BEGIN
        thm_ui_panel_options_set_dims, state.origWindow, state.cWindow, state.panelObjs
        if thm_ui_check_overlap(state.panelobjs, state.cwindow[0]) then break
        thm_ui_panel_update,event.top, state=state
        Print, 'Panel options update. Panel Options widget closed.'
        state.historyWin->Update, 'Panel options update. Panel Options widget closed.'
        Widget_Control, event.TOP, Set_UValue=state, /No_Copy
        Widget_Control, event.top, /destroy
        RETURN
      END
      'TEMP': begin
        
        if obj_valid(state.panelobjs[*state.panel_select]) then begin
          state.template->setProperty,panel=panelSettings->copy()
          state.historywin->update,'Current Panel Settings Saved to Template'
          state.statusBar->update,'Current Panel Settings Saved to Template'
        endif else begin
          state.historywin->update,'Cannot save template. Needs a valid panel to save panel template.'
          state.statusBar->update,'Cannot save template. Needs a valid panel to save panel template.'
        endelse
      
      end
      'LAYOUTPANEL': BEGIN
        *state.panel_select = event.index
        *state.ctr_num = 0
        thm_ui_init_panel_options, state=state
      END
      'PANELTITLE': BEGIN
        widget_control, event.id, get_value=value
        panelsettings->GetProperty, titleobj=panelTitle

        panelTitle->SetProperty, value=value
        ;panelsettings->SetProperty, titleobj=panelTitle
        ;cpanel->SetProperty, settings=panelSettings
      END
      'TFONT': BEGIN
        panelsettings->getproperty, titleobj=title
        title->setproperty, font=event.index
      END
      'TSIZE': BEGIN
        if event.valid then begin
          panelsettings->GetProperty, titleobj=title
          if event.value gt 0 then title->setproperty, size=event.value
        endif
      END
      'TITLEMARGIN': BEGIN
        if event.valid then begin 
          if event.value lt 0 then begin
            panelSettings->SetProperty, titlemargin=0
            widget_control, event.id, set_value=0
          endif else panelSettings->SetProperty, titlemargin=event.value
        endif
      END
      'ROW': BEGIN
        if event.valid then begin 
          if event.value lt 1 then begin
            panelSettings->SetProperty, row=1
            widget_control, event.id, set_value=1
          endif else panelSettings->SetProperty, row=event.value
        endif
      END
      'COL': BEGIN
        if event.valid then begin 
          if event.value lt 1 then begin
            panelSettings->SetProperty, col=1
            widget_control, event.id, set_value=1
          endif else panelSettings->SetProperty, col=event.value
        endif
      END
      'RSPAN': BEGIN
        if event.valid then begin 
          if event.value lt 1 then begin
            panelSettings->SetProperty, rspan=1
            widget_control, event.id, set_value=1
          endif else panelSettings->SetProperty, rspan=event.value
        endif
      END
      'CSPAN': BEGIN
        if event.valid then begin 
          if event.value lt 1 then begin
            panelSettings->SetProperty, cspan=1
            widget_control, event.id, set_value=1
          endif else panelSettings->SetProperty, cspan=event.value
        endif
      END
      'BOTBUTTON': BEGIN
        panelSettings->SetProperty, bottom=event.select
        thm_ui_init_panel_options, state=state
      END
      'LEFTBUTTON': BEGIN
        panelSettings->SetProperty, left=event.select
        thm_ui_init_panel_options, state=state
      END
      'WIDTHBUTTON': BEGIN
        panelSettings->SetProperty, width=event.select
        thm_ui_init_panel_options, state=state
      END
      'HEIGHTBUTTON': BEGIN
        panelSettings->SetProperty, height=event.select
        thm_ui_init_panel_options, state=state
      END
      'BVALUE': BEGIN
        if event.valid then begin 
          if event.value lt 0 then begin
            panelSettings->SetProperty, bvalue=0
            widget_control, event.id, set_value=0
          endif else panelSettings->SetProperty, bvalue=event.value
        endif
      END
      'LVALUE': BEGIN
        if event.valid then begin
          if event.value lt 0 then begin
            panelSettings->SetProperty, lvalue=0
            widget_control, event.id, set_value=0
          endif else panelSettings->SetProperty, lvalue=event.value
        endif
      END
      'WVALUE': BEGIN
        if event.valid then begin
          if event.value lt 0 then begin
;            panelSettings->SetProperty, wvalue=0
;            widget_control, event.id, set_value=0
          endif else panelSettings->SetProperty, wvalue=event.value
        endif
      END
      'HVALUE': BEGIN
        if event.valid then begin
          if event.value lt 0 then begin
;            panelSettings->SetProperty, hvalue=0
;            widget_control, event.id, set_value=0
          endif else panelSettings->SetProperty, hvalue=event.value
        endif
      END
      'BUNIT': BEGIN
        panelSettings->SetProperty, bunit=event.index
        thm_ui_init_panel_options, state=state
      END
      'LUNIT': BEGIN
        panelSettings->SetProperty, lunit=event.index
        thm_ui_init_panel_options, state=state
      END
      'WUNIT': BEGIN
        panelSettings->SetProperty, wunit=event.index
        thm_ui_init_panel_options, state=state
      END
      'HUNIT': BEGIN
        panelSettings->SetProperty, hunit=event.index
        thm_ui_init_panel_options, state=state 
      END
      'RELVERTSIZE': BEGIN
        if event.valid then begin
          if event.value lt 0 then begin
            panelSettings->SetProperty, relvertsize=0
            widget_control, event.id, set_value=0
          endif else panelSettings->SetProperty, relvertsize=event.value
        endif
      END
      'TPALETTE': BEGIN
        color = thm_ui_panel_options_color_event(state.tlb, panelsettings, state.tcolorWindow)
        panelSettings->GetProperty, titleobj=title
        title->SetProperty, color=color
      END
      'BGPALETTE': BEGIN
        color = thm_ui_panel_options_color_event(state.tlb, panelsettings, state.bgcolorWindow)
        panelSettings->SetProperty, backgroundcolor=color
      END   
      'FPALETTE': BEGIN
        color = thm_ui_panel_options_color_event(state.tlb, panelsettings, state.fcolorWindow)
        panelSettings->SetProperty, framecolor=color
      END
      'FRAMETHICK': BEGIN
        if event.valid then panelSettings->SetProperty, framethick=event.value
      END
      'SETALLPANEL': 
      ELSE: Print, ''
    ENDCASE
  ENDIF

      ; ALWAYS reset state
  
  Widget_Control, event.TOP, Set_UValue=state, /No_Copy
  
  RETURN
END ;--------------------------------------------------------------------------------



PRO thm_ui_panel_options, gui_id, windowStorage, loadedData, historyWin, $
                          drawObject, panel_select=panel_select, ctr_num=ctr_num,$
                          template

   ; kill top base in case of init error
  catch, err
  if err ne 0 then begin
    catch, /cancel
    help, /last_message, output=err_msg
    for i = 0, n_elements(err_msg)-1 do historywin->update,err_msg[i]
    print, 'Error--See history'
    widget_control, tlb, /destroy
    ok = error_message('An unknown error occured while starting Panel Options. See console for details.',$
        /noname, /center, title='Error in Panel Options')
    thm_gui_error, gui_id, historywin
    return
  endif

    ;top level and main base widgets
    
  tlb = Widget_Base(/Col, Title='THEMIS: Panel Options ', Group_Leader=gui_id, $
                    /Modal, /Floating, /tlb_kill_request_events)

  mainBase = Widget_Base(tlb, /Col)
  mainButtonBase = Widget_Base(tlb, /Row, /Align_Center)
  
    ;layout panel bases
   
  layoutBase = Widget_Base(mainBase, Title='Layout', /Col, ypad=2)
     panellBase = Widget_Base(layoutBase, /ROW, YPad=2, XPad=2)
       titleBase = Widget_Base(layoutBase, /Row, YPad=1, XPad=2)
       titleFontBase = widget_base(layoutBase,/row, ypad=0, xpad=2)       
       plabelBase = Widget_Base(layoutBase, /Row, YPad = 2, XPad=2)  
     placeBase = Widget_Base(layoutBase, /Col, Frame=3)
;       overlayBase = Widget_Base(placeBase, /Row, YPad=1, XPad=2)
       rcBase = Widget_Base(placeBase, /Row, YPad=2)
         col1 = Widget_Base(rcBase, /Col,  XPad=2, ypad=10, tab_mode=1)
         col2 = Widget_Base(rcBase, /Col, XPad=30)
       topBase = Widget_Base(placeBase, /Row)
         tcol1 = Widget_Base(col2, /Row)
         tcol2 = Widget_Base(col2, /Row)
         tcol3 = Widget_Base(col2, /Row)
         tcol4 = Widget_Base(col2, /Row)
           tButBase = Widget_Base(tcol1, /Col, /NonExclusive, xsize=70)
           tSizeBase = Widget_Base(tcol1, /Col)
           tPullBase = Widget_Base(tcol1, /Col)
           t2ButBase = Widget_Base(tcol2, /Col, /NonExclusive, xsize=70)
           t2SizeBase = Widget_Base(tcol2, /Col)
           t2PullBase = Widget_Base(tcol2, /Col)
           t3ButBase = Widget_Base(tcol3, /Col, /NonExclusive, xsize=70)
           t3SizeBase = Widget_Base(tcol3, /Col)
           t3PullBase = Widget_Base(tcol3, /Col)
           t4ButBase = Widget_Base(tcol4, /Col, /NonExclusive, xsize=70)
           t4SizeBase = Widget_Base(tcol4, /Col)
           t4PullBase = Widget_Base(tcol4, /Col)
       bottomBase = Widget_Base(placeBase, /Row)
       relBase = Widget_Base(placeBase, /Row, /Align_Center)      
     clabelBase = Widget_Base(layoutBase, /Row, YPad=1, XPad=2)
     colorColBase = Widget_Base(layoutBase, /col, Frame=3)
     colorBase = Widget_Base(colorColBase, /Row, YPad=1, XPad=2)
     thicknessBase = Widget_Base(colorColBase, /row)
     setbBase = Widget_Base(layoutBase, /Row, /Align_Center, /NonExclusive, YPad=4, XPad=2)


  if ~ptr_valid(panel_select) then panel_select = ptr_new(0)
  if ~ptr_valid(ctr_num) then ctr_num = ptr_new(0) else *ctr_num = 0

  cWindow = windowStorage->GetActive()
  origWindow = cWindow->Copy()
 
  IF NOT Obj_Valid(cWindow) THEN BEGIN
    panelNames=['No Panels']  
  ENDIF ELSE BEGIN
    cWindow->GetProperty, Panels=panels, nRows=nRows, nCols=nCols
    IF Obj_Valid(panels) THEN BEGIN
      panelObjs = panels->Get(/all)
      IF obj_valid(panelobjs[0]) then begin
        FOR i=0, N_Elements(panelObjs)-1 do panelobjs[i]->save
      endif
    endif
    IF NOT Obj_Valid(panels) THEN BEGIN
      panelNames=['No Panels']  
    ENDIF ELSE BEGIN
      panelObjs = panels->Get(/all)
      IF Is_Num(panelObjs) THEN BEGIN
        panelNames=['No Panels'] 
      ENDIF ELSE BEGIN
        FOR i=0, N_Elements(panelObjs)-1 DO BEGIN
          name = panelObjs[i]->constructPanelName()
          IF i EQ 0 THEN panelNames=[name] ELSE panelNames=[panelNames, name]
        ENDFOR
        panelobjs[0]->getproperty, settings=panelsettings
        panelsettings->getproperty, titleobj=title
      ENDELSE
    ENDELSE
    IF Is_Num(panelNames) THEN panelNames=['No Panels'] 
    IF N_Elements(panelNames) EQ 1 && panelNames EQ '' THEN panelNames=['No Panels']
  ENDELSE
  
  if ~obj_valid(title) then title = obj_new('thm_ui_text')

  ;Get path to bitmap icons
  getresourcepath,rpath
  palettebmp = read_bmp(rpath + 'color.bmp', /rgb)
  thm_ui_match_background, tlb, palettebmp

    ;layout panel widgets
  
  ;pldBase = widget_base(panellbase, /row)
  pldLabel = widget_label(panellBase, value = 'Panel: ', xsize=50)
  panellDroplist = Widget_combobox(panellBase, Value=panelNames, XSize=340, $
                   UValue='LAYOUTPANEL', uname='layoutpanel')
  TitleLabel = Widget_Label(TitleBase, Value='Title: ', xsize=50)                                
  TitleText = Widget_Text(TitleBase, /Editable, /all_events, XSize = 55, ysize=1, $
                          uvalue='PANELTITLE', uname='paneltitle')
  marginBase = Widget_Base(TitleBase, /Row)
  marginIncrement = thm_ui_spinner(marginBase, Label='Margin: ', Increment=1, Value=1, $
                                     UValue='TITLEMARGIN', uname='titlemargin')

  ;Title font and options widgets
  spacelabel = widget_label(titleFontBase, value='', xsize=50)
  titleFontDroplist = Widget_Combobox(titleFontBase,xsize=150, Value=title->getfonts(), uval='TFONT', uname='titlecombo')
  titleFontIncBase = widget_base(titleFontBase, /row, xpad=8, ypad=0, space=0) 
    titleFontIncrement = thm_ui_spinner(titleFontIncBase, incr=1, uval='TSIZE', uname='titlesize')

  titleColorBase = Widget_Base(titleFontBase, /row, xpad=4, ypad=0, space=0)
    paletteButton = Widget_Button(titleColorBase, Value=palettebmp, /Bitmap, UValue='TPALETTE', Tooltip='Choose color from Palette')

  geo_struct = widget_info(paletteButton,/geometry)
  tcolorWindow = Widget_Draw(titleFontBase, XSize=50, YSize=geo_struct.scr_ysize,uname='tcolorwindow', $
                             graphics_level=2,renderer=1,retain=1,units=0,frame=1, /expose_events)
 
  
  placemLabel = Widget_Label(plabelBase, Value='Placement: ' )
;  odLabel = widget_label(overlayBase, value = 'Overlay Panel: ')
;  overlayDroplist = Widget_combobox(overlayBase, Value=['panel 1', 'panel 2'], sensitive=0)
    
  rownBase = Widget_Base(col1, /Row)
;  rownLabel = Widget_Label(rownBase, value='Row: ')
  rownIncrement = thm_ui_spinner(rownBase, xlabelsize=80, label= 'Row: ',Increment=1, Value=1, $
                                   uvalue='ROW', uname='row')
  colnBase = Widget_Base(col1, /Row, /align_left)
;  colnLabel = Widget_Label(colnBase, value='Column:')
  colnIncrement = thm_ui_spinner(colnBase, xlabelsize=80, label='Column: ',Increment=1, Value=1, $
                                   uvalue='COL', uname='col')
  rown2Base = Widget_Base(col1, /Row)
  rown2Increment = thm_ui_spinner(rown2Base, Label='Row Span: ', xlabelsize=80, Increment=1, $
                                    Value=1, uvalue='RSPAN', uname='rspan')
  coln2Base = Widget_Base(col1, /Row)
  coln2Increment = thm_ui_spinner(coln2Base, Label='Column Span: ', xlabelsize=80, Increment=1, $
                                    Value=1, uvalue='CSPAN', uname='cspan')
                                    
  botButton = Widget_Button(tbutBase, Value = 'Bottom:', uval='BOTBUTTON', uname='botbutton')
  leftButton = Widget_Button(t2butBase, Value = 'Left:', uval='LEFTBUTTON', uname='leftbutton')
  botText = thm_ui_spinner(tsizeBase, Increment=1, uval='BVALUE', uname='bvalue')
  leftText = thm_ui_spinner(t2sizeBase, Increment=1, uval='LVALUE', uname='lvalue')
  botDroplist = Widget_combobox(tpullBase, uval='BUNIT', uname='bunit')
  leftDroplist = Widget_combobox(t2pullBase, uval='LUNIT', uname='lunit')
  widthButton = Widget_Button(t3butBase, Value = 'Width:', uval='WIDTHBUTTON', uname='widthbutton')
  heightButton = Widget_Button(t4butBase, Value = 'Height:', uval='HEIGHTBUTTON', uname='heightbutton')
  widthText = thm_ui_spinner(t3sizeBase, Increment=1, uval='WVALUE', uname='wvalue')
  heightText = thm_ui_spinner(t4sizeBase, Increment=1, uval='HVALUE', uname='hvalue')
  widthDroplist = Widget_combobox(t3pullBase, uval='WUNIT', uname='wunit')
  heightDroplist = Widget_combobox(t4pullBase, uval='HUNIT', uname='hunit')
  relLabel = Widget_Label(relBase, Value='Relative Vertical Size (%):  ', /Align_Center, sensitive=0)
  relText = thm_ui_spinner(relBase, /Align_Center, Increment=1, uval='RELVERTSIZE', uname='relvertsize', sensitive=0)
  colorLabel = Widget_Label(clabelBase, Value='Color: ' )
  bgpaletteBase = Widget_Base(colorBase, /Row)
  bgcolorLabel = Widget_Label(bgpaletteBase, Value='Background Color: ')
  geo_struct = widget_info(bgcolorLabel,/geometry)
  labelXSize = geo_struct.scr_xsize
 
  getresourcepath,rpath
  palettebmp = read_bmp(rpath + 'color.bmp', /rgb)
  thm_ui_match_background, tlb, palettebmp
  
  bgpaletteButton = Widget_Button(bgpaletteBase, Value=palettebmp, /Bitmap, $
                                  UValue='BGPALETTE', Tooltip='Choose background color from palette')
  bgspaceLabel = Widget_Label(bgpaletteBase, Value=' ')
  bgcolorWindow = WIDGET_DRAW(bgpaletteBase,graphics_level=2,renderer=1, $
                             retain=1, XSize=50, YSize=20, units=0, frame=1, /expose_events)
  fpaletteBase = Widget_Base(thicknessBase, /Row)
  fcolorLabel = Widget_Label(fpaletteBase, Value='Panel Frame Color: ', xsize=labelXSize)
  fpaletteButton = Widget_Button(fpaletteBase, Value=palettebmp, /Bitmap, $
                                  UValue='FPALETTE', Tooltip='Choose panel frame color from palette')
  fspaceLabel = Widget_Label(fpaletteBase, Value=' ')
  fcolorWindow = WIDGET_DRAW(fpaletteBase,graphics_level=2,renderer=1, $
                             retain=1, XSize=50, YSize=20, units=0, frame=1, /expose_events)
  frametBase = WIDGET_BASE(thicknessBase, /row, xpad = 30)
  linetIncrement = thm_ui_spinner(frametBase, label='Frame Thickness:    ', Increment=1, $
                                    uval='FRAMETHICK', uname='framethick')
  linetLabel = Widget_Label(frametBase, Value=' (pts)')
  setAllPanelButton = Widget_Button(setbBase, Value='Set All Panels', uname='setallpanel', xsize=125, uval='SETALLPANEL')
                                
  okButton = Widget_Button(mainButtonBase, Value='OK', Uvalue='OK', XSize=75)
  applyButton = Widget_Button(mainButtonBase, Value='Apply', Uvalue='APPLY', XSize=75)
  cancelButton = Widget_Button(mainButtonBase, Value='Cancel', UValue='CANC', XSize=75)
  templateButton = Widget_Button(mainButtonBase, Value='Save to Template', UValue='TEMP',xsize=125)

  statusBar = obj_new('thm_ui_message_bar',tlb)

  state = {tlb:tlb, tcolorWindow:tcolorWindow, bgcolorWindow:bgcolorWindow, fcolorWindow:fcolorWindow, loadedData:loadedData, $
           panelObjs:panelObjs, windowStorage:windowStorage, origWindow:origWindow, $
           cWindow:cWindow, drawObject:drawObject, $
           historyWin:historyWin, nRows:nRows, nCols:nCols, $
           panel_select:panel_select, ctr_num:ctr_num, $ 
           gui_id:gui_id, template:template, statusBar:statusBar,$
           panelNames:panelNames,is_trace_spec:0}

  Widget_Control, tlb, Set_UValue=state, /No_Copy
  centertlb, tlb
  widget_control, tlb, /Realize
    
  thm_ui_panel_update,tlb
    
  ;thm_ui_init_panel_options, tlb
  
  historyWin->update,'THM_UI_PANEL_OPTIONS: Widget started'
  statusBar->update,'THM_UI_PANEL_OPTIONS: Widget started'
  XManager, 'thm_ui_panel_options', tlb, /No_Block
  RETURN
END ;--------------------------------------------------------------------------------
