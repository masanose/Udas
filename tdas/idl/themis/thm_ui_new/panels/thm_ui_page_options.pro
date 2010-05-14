
;+ 
;
;Pro HANDLE_PAGETITLE_COMMAND updates the "titletext" widget, and the pagesettings object after a selection on one
;of the subbuttons of the "Token" and "Format" buttons (Graph -> Page Options... Text [tab]).  For the format buttons,
;the FORMAT kw must be set.  This routine reduces duplication in the case statement of THM_UI_PAGE_OPTIONS_EVENT.PRO.
;
;W.M.Feuerstein, September 24, 2008.
;
;
;Function not currently in use.
;
;-  
Compile_opt idl2, hidden

pro handle_pagetitle_command, state, event, ind, format=format

  if event.select then begin

    state.pagesettings->GetProperty,title=title
    title->GetProperty,value=value

    case 1 of
      ~keyword_set(format): begin
	tokenCommand= state.pagesettings->GetTokenCommand(ind)
	value+=tokenCommand
      end
      else: begin
	formatCommand= state.pagesettings->GetFormatCommand(ind)
	value+=formatCommand
      end
    endcase

    title->SetProperty,value=value
    id=widget_info(state.tlb,find_by_uname='titletext')
    widget_control,id,set_value=value

  endif

end ;----------------------------------


PRO thm_ui_page_spacing_set_value, tlb, pagesettings, historywin, statusbar

    compile_opt idl2, hidden

  xid = widget_info(tlb, find_by_uname='xpanelspacing')
  widget_control, xid, get_value = xspacing
  yid = widget_info(tlb, find_by_uname='ypanelspacing')
  widget_control, yid, get_value = yspacing

  
    if (xspacing lt 0) or (yspacing lt 0) then begin
       historywin->Update, "Panel spacing cannot be negative; some values not applied.", /dontshow
       statusBar->Update, "Panel spacing cannot be negative; some values not applied."
    endif else begin
       historywin->Update, "Panel spacing changed.", /dontshow
       statusBar->Update, "Panel spacing changed."
    endelse
    
    if in_set(finite([xspacing, yspacing]),0) then begin
      statusBar->Update, 'Invalid panel spacing, please re-enter.'
      historywin->Update, 'Invalid panel spacing, value not applied.', /dontshow
    endif
    
    pagesettings->getproperty, xpanelspacing=xps, ypanelspacing=yps
    
    if (xspacing ge 0) && finite(xspacing) then pagesettings->SetProperty, xpanelspacing=xspacing $
      else widget_control, xid, set_value=xps
    if (yspacing ge 0) && finite(yspacing) then pagesettings->SetProperty, ypanelspacing=yspacing $
      else widget_control, yid, set_value=yps

END ;----------------------------------


PRO thm_ui_page_canvas_set_value, tlb, pagesettings, historywin, statusbar,drawObject,windowStorage

    compile_opt idl2, hidden
  
  id = widget_info(tlb, find_by_uname='landscape')
  orientation = widget_info(id, /button_set)
  
  pagesettings->GetProperty, orientation = page_orientation
  
  if page_orientation ne orientation then begin
  
    xid = widget_info(tlb, find_by_uname='xsize')
    yid = widget_info(tlb, find_by_uname='ysize')
  
    widget_control, xid, get_value=xsize
    widget_control, yid, get_value=ysize
   
    pageSettings->setProperty,orientation=orientation,canvasSize=[xsize,ysize]  
    
    thm_ui_orientation_update,drawObject,windowStorage
    
;    drawObject->getProperty,destination=dest
;    
;    if obj_valid(dest) then begin
;      cz = drawObject->getZoom()
;      ;note this is done using !D values for consistency with thm_gui_new
;      ;to get reliable behavior, both sections should probably be switched over
;      ;to use the resolution property of the window object
;      size_px = [xsize*2.54*!D.X_PX_CM,ysize*2.54*!D.Y_PX_CM]
;      ;dest->getProperty,resolution=res
;     ; size_px = size_cm/res  
;      drawObject->setZoom,1
;      dest->setProperty,virtual_dimensions=size_px
;      drawObject->setZoom,cz
;    endif
 
  endif
    
;
;  id = widget_info(tlb, find_by_uname='xsize')
;  widget_control, id, get_value=xsize
;  id = widget_info(tlb, find_by_uname='ysize')
;  widget_control, id, get_value=ysize
;
;  if ~finite(xsize,/nan) and ~finite(ysize,/nan) then begin
;    if (xsize lt 1) or (ysize lt 1) then begin
;       historywin->Update, "Canvas size cannot be less than 1. Values less than 1 were not applied", /dontshow
;       statusBar->Update, "Canvas size cannot be less than 1. Values less than 1 were not applied"
;    endif else begin
;       historywin->Update, "Canvas size changed.", /dontshow
;       statusBar->Update, "Canvas size changed."
;    endelse
;    
;    if xsize ge 1 then canvasSize[0]=xsize
;    if ysize ge 1 then canvasSize[1]=ysize
;  endif else begin
;    statusBar->Update, 'Invalid canvas size, value not applied.'
;    historywin->update, 'Invalid canvas size, value not applied.', /dontshow
;  endelse
;
;  pagesettings->SetProperty, CanvasSize=canvassize

END ; --------------------------------------------


PRO thm_ui_page_margins_set_value, tlb, pagesettings,historywin,statusBar

    compile_opt idl2, hidden
  
  tid = widget_info(tlb, find_by_uname='tmargin')
  widget_control, tid, get_value=top
  bid = widget_info(tlb, find_by_uname='bmargin')
  widget_control, bid, get_value=bottom
  lid = widget_info(tlb, find_by_uname='lmargin')
  widget_control, lid, get_value=left
  rid = widget_info(tlb, find_by_uname='rmargin')
  widget_control, rid, get_value=right
  
  if (left lt 0) or (right lt 0) or (top lt 0) or (bottom lt 0) then begin
    statusbar->Update, 'Page margin cannot be negative.  Negative values not applied.'
    historywin->update, 'Page margin cannot be negative.  Negative values not applied.', /dontshow
  endif else begin
    statusbar->Update, 'Page margins set'
    historywin->update, 'Page margins set.', /dontshow
  endelse

  pagesettings->getproperty, leftprintmargin=l, rightprintmargin=r, topprintmargin=t, bottomprintmargin=b
  
  if ~finite(left,/nan) && (left ge 0) then pagesettings->setproperty, leftprintmargin=left $
    else widget_control, lid, set_value=l
  if ~finite(right,/nan) && (right ge 0) then pagesettings->setproperty, rightprintmargin=right $
    else widget_control, rid, set_value=r
  if ~finite(top,/nan) && (top ge 0) then pagesettings->setproperty, topprintmargin=top $
    else widget_control, tid, set_value=t
  if ~finite(bottom,/nan) && (bottom ge 0) then pagesettings->setproperty, bottomprintmargin=bottom $
    else widget_control, bid, set_value=b
  
  if in_set(finite([left,right,top,bottom],/nan),1) then begin
    statusBar->Update, 'Invalid page margin, value not applied.'
    historywin->Update, 'Invalid page margin, value not applied.', /dontshow
  endif
  
  return  

END ;---------------------------------------------

;+
;
;NAME:
;  thm_ui_page_title_set_value
;
;  PURPOSE:
;    Reads the title settings out of the widgets and stores them in the appropriate location in the draw tree
;  
;  INPUTS:
;    tlb:  the top level base for this window
;    pagesettings: the page settings associated with the current window
;     
; NOTES:  
;-

pro thm_ui_page_title_set_value,tlb,pagesettings,historywin,statusBar

  compile_opt idl2,hidden
  
  pageSettings->getProperty,title=title
  
  ;title text
  ;---------
  
  ;get the title text widget
  titletext = widget_info(tlb,find_by_uname='titletext')
  
  ;get the value from the title text widget
  widget_control,titletext,get_value=textvalue
  
  ;set the value
  title->setProperty,value=textvalue
  
  historywin->update,'Set Title Text', /dontshow
  
  ;title font
  ;----------

  ;get font index

  title->setProperty,font=thm_ui_page_font_get_value(tlb,'titlecombo')
  
  historywin->update,'Set Title Font', /dontshow
  
  ;title format
  ;-------------
  
  ;get combobox
  formatcombo = widget_info(tlb,find_by_uname='titleformat')
  
  ;format text
  formattext = widget_info(formatcombo,/combobox_gettext)
  
  ;format list
  widget_control,formatcombo,get_value=formatnames
  idx = where(formattext eq formatnames)
  
  title->setProperty,format=idx
  
  ;show title flag
  ;---------------
  
  ;get the widget
  showbutton = widget_info(tlb,find_by_uname='showtitle')
  
  ;store its value
  title->setProperty,show=widget_info(showbutton,/button_set)
  
  historywin->update,'Set Title Show Flag', /dontshow
  
  ;title size
  ;---------------
  
  ;get the size spinner widget id 
  sizespinner = widget_info(tlb,find_by_uname='titlesize')
  ;get the current spinner value
  widget_control,sizespinner,get_value=val
  ;
  
  if val lt 0 then begin
    historywin->update,'Current font less than 0.  Cannot set font size.', /dontshow
    statusBar->update,'Current font less than 0.  Cannot set font size.'
    title->getProperty,size=size
    widget_control,sizespinner,set_value=size
  endif else begin
    if ~finite(val,/nan) then begin
      title->setProperty,size=val
      historywin->update,'Set Title Size', /dontshow
    endif else begin
      historywin->update, 'Invalid title size, value not set.',/dontshow
      statusbar->update, 'Invalid titse size, please re-eneter.'
    endelse
  endelse 

  ;title color
  ;-----------
  
  ;set the title color
  title->setProperty,color=thm_ui_page_color_get_value(tlb,'tcolorwindow')
  
  historywin->update,'Set Title Color', /dontshow
  
end


;+
;
;NAME:
;  thm_ui_page_footer_set_value
;
;  PURPOSE:
;    Reads the footer settings out of the widgets and stores them in the appropriate location in the draw tree
;  
;  INPUTS:
;    tlb:  the top level base for this window
;    pagesettings: the page settings associated with the current window
;     
; NOTES:  
;-

pro thm_ui_page_footer_set_value,tlb,pagesettings,historywin,statusBar

  compile_opt idl2,hidden
  
  pageSettings->getProperty,footer=footer
  
  ;footer text
  ;---------
  
  ;get the footer text widget
  footertext = widget_info(tlb,find_by_uname='footertext')
  
  ;get the value from the footer text widget
  widget_control,footertext,get_value=textvalue
  
  ;set the value
  footer->setProperty,value=textvalue
  
  historywin->update,'Set Footer Text', /dontshow
  
  ;footer font
  ;----------

  ;set footer font
  footer->setProperty,font=thm_ui_page_font_get_value(tlb,'footercombo')
  
  historywin->update,'Set footer Font', /dontshow
  
  ;footer format
  ;-------------
  
  ;get combobox
  formatcombo = widget_info(tlb,find_by_uname='footerformat')
  
  ;format text
  formattext = widget_info(formatcombo,/combobox_gettext)
  
  ;format list
  widget_control,formatcombo,get_value=formatnames
  idx = where(formattext eq formatnames)
  
  footer->setProperty,format=idx
  
  ;show footer flag
  ;---------------
  
  ;get the widget
  showbutton = widget_info(tlb,find_by_uname='showfooter')
  
  ;store its value
  footer->setProperty,show=widget_info(showbutton,/button_set)
  
  historywin->update,'Set footer Show Flag', /dontshow
  
  ;footer size
  ;---------------
  
  ;get the size spinner widget id 
  sizespinner = widget_info(tlb,find_by_uname='footersize')
  ;get the current spinner value
  widget_control,sizespinner,get_value=val
  ;
  val = double(val)
  
  if val lt 0 then begin
    historywin->update,'Current font less than 0.  Cannot set font size.', /dontshow
    statusBar->update,'Current font less than 0.  Cannot set font size.'
    footer->getProperty,size=size
    widget_control,sizespinner,set_value=size
  endif else begin
    if ~finite(val,/nan) then begin
      footer->setProperty,size=val
      historywin->update,'Set footer Size', /dontshow
    endif else begin
      historywin->update,'Invalid footer size, value not set.', /dontshow
      statusBar->update,'Invalid footer size, please re-enter.'
    endelse
  endelse 

  ;footer color
  ;-----------

  footer->setProperty,color=thm_ui_page_color_get_value(tlb,'fcolorwindow')
    
  historywin->update,'Set footer Color', /dontshow
end

;+
;
;NAME: 
;  thm_ui_page_set_value
;
;PURPOSE:
;  this procedure is here to provide a framework for the incremental implementation
;  of lazy event handling in the page window. Rather than store a value every time an event
;  occurs, this procedure queries widget values, when they are needed('OK' or 'Apply').
;  
;  The advantages of this are:
;    #1 Simplifies/Centralizes event handling code
;    #2 Prevent routines from overwriting invalid user inputs when they are partially complete
;    #3 Allows the window to perform more complex input validation(because it can be assumed that all inputs should be correct)  
;
;INPUTS:
;  The top level base of the page window
;  The state variable for the window(If the state were not taken off the tlb by the time this is called, we could eliminate this parameter)
;  
;NOTES:  
;  This routine is incomplete.  As software maintainence tasks are performed,
;  programmers should move functions into this routine at their perogative.
;  Right now the title and the footer are processed using the lazy event handling. 
;
;-

pro thm_ui_page_set_value,tlb,state

  compile_opt idl2,hidden

  ;Get page settings 
  ;We may not have to store this in the state in future versions
  pagesettings = state.pagesettings
  
  ;referencing into the main info structure is probably a bad way to get to the history window
  ;In a future version, it should be stored directly in the state.
  historywin = state.info.historywin
  
  statusBar = state.statusBar
  
  window = state.cWindow
  
  ;---------------
  ;process title
  ;---------------
  
  thm_ui_page_title_set_value,tlb,pagesettings,historywin,statusBar
  
  ;--------------
  ;process footer
  ;--------------
  
  thm_ui_page_footer_set_value,tlb,pagesettings,historywin,statusBar
  
  ;---------------
  ;process labels
  ;---------------
  
  ;thm_ui_page_label_set_value,tlb,window,historywin,statusBar
  
  ;---------------
  ;process margins
  ;---------------
  thm_ui_page_margins_set_value, tlb, pagesettings,historywin,statusBar
  
  ;---------------
  ;process canvase size (disabled)
  ;---------------
  thm_ui_page_canvas_set_value, tlb, pagesettings, historywin, statusbar,state.drawObject,state.windowStorage
  
  ;---------------
  ;process panel spacing
  ;---------------
  thm_ui_page_spacing_set_value, tlb, pagesettings, historywin, statusbar
  
end

;+
;
;NAME:
;  thm_ui_page_color_event
;
;  PURPOSE:
;    abstracts duplication in handling of color/palette events
;  
;  INPUTS:
;    tlb:  the top level base for this window
;    uname: the user name of the widget in question
;    messagename: the name to be used in the output message
;     
; NOTES:  
;-

pro thm_ui_page_color_event,tlb,uname,messagename,historywin,statusbar

  compile_opt idl2,hidden
  
  colorwindow = widget_info(tlb,find_by_uname=uname)
  Widget_Control, colorwindow, Get_Value=colorWin
  ColorWin->getProperty,graphics_tree=scene
  scene->getProperty,color=currcolor
  color = PickColor(!p.color, Group_Leader=tlb, Cancel=cancelled,currentcolor=currcolor)
  if ~cancelled then begin

    scene->setProperty,color=reform(color)
    Colorwin->draw
 
    historyWin->Update,messagename + ' color changed.'
    statusbar->Update,messagename + ' color changed.'
  endif

end

;+
;
;NAME:
;  thm_ui_page_font_get_value
;
;  PURPOSE:
;    abstracts duplication in acquisition of font values 
;  
;  INPUTS:
;    tlb:  the top level base for this window
;    uname: the user name of the widget in question
;     
; NOTES:  
;-
function thm_ui_page_font_get_value,tlb,uname

  compile_opt idl2,hidden

  ;combobox widget index
  combo = widget_info(tlb,find_by_uname=uname)
  ;combobox text
  text = widget_info(combo,/combobox_gettext)
  ;combobox values list
  widget_control,combo,get_value=fontnames
  
  ;combobox index of current text
  return,where(text eq fontnames)

end

;+
;
;NAME:
;  thm_ui_page_color_get_value
;
;  PURPOSE:
;    abstracts duplication in acquisition of font values 
;  
;  INPUTS:
;    tlb:  the top level base for this window
;    uname: the user name of the widget in question
;     
; NOTES:  
;-
function thm_ui_page_color_get_value,tlb,uname

  compile_opt idl2,hidden

  ;current color display draw widget
  colorwindow = widget_info(tlb,find_by_uname=uname)
  ;get the actual window object
  Widget_Control, colorwindow, Get_Value=colorWin
  ;get the scene being drawn on the object
  ColorWin->getProperty,graphics_tree=scene
  ;get the color from the scene
  scene->getProperty,color=color
 
  return,color

end

;+
;
;NAME:
;  thm_ui_page_update_canvas
;
;  PURPOSE:
;    updates canvas size values when switching between portrait
;    and landscape modes
;  
;  INPUTS:
;    tlb:  the top level base for this window
;    portrait: keyword for portrait option
;    landscape: keyword for landscape option
;     
; NOTES:  
;-
pro thm_ui_page_update_canvas, tlb

    compile_opt idl2, hidden

  xid = widget_info(tlb, find_by_uname='xsize')
  yid = widget_info(tlb, find_by_uname='ysize')
  
  widget_control, xid, get_value=xsize
  widget_control, yid, get_value=ysize
  
  if finite(xsize) && finite(ysize) then begin
    widget_control, xid, set_value=ysize
    widget_control, yid, set_value=xsize
  endif

end

;+ 
;NAME:
; thm_ui_page_options
;
;PURPOSE:
; user interface panel that allows user to change page parameters 
;
;CALLING SEQUENCE:
; thm_ui_page_options, gui_id
;
;INPUT:
; gui_id = the id number of the widget that calls this
;
;OUTPUT:
; 
;HISTORY:
;$LastChangedBy: aaflores $
;$LastChangedDate: 2010-04-14 11:24:35 -0700 (Wed, 14 Apr 2010) $
;$LastChangedRevision: 7508 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_page_options.pro $
;
;---------------------------------------------------------------------------------



PRO thm_ui_page_options_event, event, pagesettings

  Compile_Opt hidden

  Widget_Control, event.TOP, Get_UValue=state, /No_Copy

    ;Put a catch here to insure that the state remains defined

  err_xxx = 0
  Catch, err_xxx
  IF (err_xxx NE 0) THEN BEGIN
    Catch, /Cancel
    Help, /Last_Message, Output = err_msg
    if is_struct(state) then begin
      FOR j = 0, N_Elements(err_msg)-1 DO state.info.historywin->update,err_msg[j]
      x=state.gui_id
      histobj=state.info.historywin
      if obj_valid(state.pagesettings) then state.pagesettings->Reset
    endif
    Print, 'Error--See history'
    ok=error_message('An unknown error occured and the window must be restarted. See console for details.',$
       /noname, /center, title='Error in Page Options')
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin 
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF

      ;kill request block
      
  IF(Tag_Names(event, /Structure_Name) EQ 'WIDGET_KILL_REQUEST') THEN BEGIN  
    ;Reset old settings:
    ;*******************
    ;
    state.pagesettings->Reset

    ;Call reset method on all PANEL objects:
    ;***************************************
    ;
    state.cwindow->GetProperty, Panels=panels
    IF Obj_Valid(panels) THEN BEGIN
      panelObjs = panels->Get(/all)
      IF obj_valid(panelobjs[0]) then begin
	FOR i=0, N_Elements(panelObjs)-1 do panelobjs[i]->reset
      endif

      state.info.historyWin->Update,'Panels reset.'
      state.statusbar->Update,'Panels reset.'
    endif

    ;Redraw:
    ;*******
    ;
    state.info.drawObject->update,state.info.windowStorage,state.info.loadedData
    state.info.drawObject->draw
    state.info.historyWin->Update,'Active window refreshed.'
    state.statusbar->Update,'Active window refreshed.'

    Print, 'widget killed' 
    state.info.historyWin->Update,'THM_UI_PAGE_OPTIONS: Widget killed' 
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    Widget_Control, event.top, /Destroy
    RETURN      
  ENDIF

   ;deal with tabs

  IF (Tag_Names(event, /Structure_Name) EQ 'WIDGET_TAB') THEN BEGIN  
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    RETURN 
  EndIF

   ; Get the instructions from the widget causing the event and
   ; act on them.

  Widget_Control, event.id, Get_UValue=uval
  if size(uval, /type) ne 0 then begin
    if size(uval,/type) eq 8 then uvalname=uval.name else uvalname=uval
  endif else uvalname = strtrim(-1,2) 

  state.info.historyWin->Update,'THM_UI_PAGE_OPTIONS: User value: '+uvalname,/dontshow
 
  CASE uvalname OF
    'CANC': BEGIN
      Print, 'Panel widget canceled' 

      ;Reset old settings:
      ;*******************
      ;
      state.pagesettings->Reset

      ;Call reset method on all PANEL objects:
      ;***************************************
      ;
      state.cwindow->GetProperty, Panels=panels
      IF Obj_Valid(panels) THEN BEGIN
	panelObjs = panels->Get(/all)
	IF obj_valid(panelobjs[0]) then begin
	  FOR i=0, N_Elements(panelObjs)-1 do panelobjs[i]->reset
	endif

        state.info.historyWin->Update,'Panels reset.'
        state.statusbar->Update,'Panels reset.'
      endif

      ;Redraw:
      ;*******
      ;
      state.info.drawObject->update,state.info.windowStorage,state.info.loadedData
      state.info.drawObject->draw
      state.info.historyWin->Update,'Active window refreshed.'
      state.statusbar->Update,'Active window refreshed.'

      ;Exit:
      ;*****
      ;
      state.info.historyWin->Update,'Exiting Page Options.'
      state.statusbar->Update,'Exiting Page Options.'
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    END
    'OK': BEGIN

      ;Update values from widgets  
      ;******
      ;   
      thm_ui_page_set_value,event.top,state

      ;Redraw:
      ;*******
      ;
      state.info.drawObject->update,state.info.windowStorage,state.info.loadedData
      state.info.drawObject->draw
      state.info.historyWin->Update,'Active window refreshed.'
      state.statusbar->Update,'Active window refreshed.'

      ;Exit:
      ;*****
      ;
      state.info.historyWin->Update,'Exiting Page Options.'
      state.statusbar->Update,'Exiting Page Options.'
      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      Print, 'Settings updated.  Panel widget closed.' 
      RETURN
    END
    'TEMP':begin
       ;make sure internal state is updated before save
       thm_ui_page_set_value,event.top,state
       state.template->setProperty,page=state.pagesettings->Copy()
       state.statusbar->update,'Saved Current Page Settings to Template'
       state.historywin->update,'Saved Current Page Settigns to Template'
     end
    'BPALETTE': begin

      state.pagesettings->GetProperty,backgroundcolor=currcolor
      color = PickColor(!p.color, Group_Leader=state.tlb, Cancel=cancelled,currentcolor=currcolor)
      if ~cancelled then begin
      	state.pagesettings->SetProperty,backgroundcolor=color
      	
      	bcolorwindow = widget_info(state.tlb,find_by_uname='bcolorwindow')
      	Widget_Control, bcolorwindow, Get_Value=bcolorWin
      	
      	if obj_valid(scene) then scene->remove,/all
      	scene=obj_new('IDLGRSCENE', color=reform(color))
      	bcolorWin->draw, scene

        state.info.historyWin->Update,'Background color changed.'
        state.statusbar->Update,'Background color changed.'
      endif
    END
    'TPALETTE': begin

      thm_ui_page_color_event,state.tlb,'tcolorwindow','Title',state.info.historyWin,state.statusBar

    END
    'LPALETTE': BEGIN

       state.pagesettings->GetProperty,Labels=labels
      labels->GetProperty,Color=currcolor
      color = reform(PickColor(!p.color, Group_Leader=state.tlb, Cancel=cancelled,currentcolor=currcolor))
      if ~cancelled then begin
        labels->SetProperty,Color=color
        lcolorwindow = widget_info(state.tlb,find_by_uname='lcolorwindow')
        Widget_Control, lcolorwindow, Get_Value=lcolorWin
        lColorWin->getProperty,graphics_tree=scene
        scene->SetProperty,color=color
        lcolorWin->draw
      endif
      ;
      state.cwindow->GetProperty, Panels=panels
      IF Obj_Valid(panels) THEN BEGIN
         panelObjs = panels->Get(/all)
         IF NOT Is_Num(panelObjs) THEN BEGIN
            FOR j=0, N_Elements(panelObjs)-1 DO BEGIN
               panelObjs[j]->GetProperty, XAxis=xaxis, yAxis=yaxis, zAxis=zaxis
               IF Obj_Valid(xaxis) THEN BEGIN
                  xaxis->GetProperty, Labels=labels
                  if obj_valid(labels[0]) then begin
        labelObjs=labels->Get(/all)
        IF Obj_Valid(labelObjs[0]) THEN BEGIN               
           FOR i=0, N_Elements(labelObjs)-1 DO labelObjs[i]->SetProperty, Color=color
        ENDIF
                  endif
               ENDIF
               IF Obj_Valid(yaxis) THEN BEGIN
                  yaxis->GetProperty, Labels=labels
                  if obj_valid(labels[0]) then begin
        labelObjs=labels->Get(/all)
        IF Obj_Valid(labelObjs[0]) THEN BEGIN                  
           FOR i=0, N_Elements(labelObjs)-1 DO labelObjs[i]->SetProperty, Color=color
        ENDIF
                  endif
               ENDIF
               IF Obj_Valid(zaxis) THEN BEGIN
                  zaxis->GetProperty, Labeltextobject=Labeltextobject
                  if obj_valid(Labeltextobject) then labeltextobject->SetProperty, Color=color
               ENDIF
            ENDFOR
         ENDIF

         state.info.historyWin->Update,'Label color changed.
         state.statusbar->Update,'Label color changed.
      ENDIF
    END
    'VPALETTE': BEGIN

      state.pagesettings->GetProperty,Variables=variables
      variables->GetProperty,Color=currcolor
      color = reform(PickColor(!p.color, Group_Leader=state.tlb, Cancel=cancelled,currentcolor=currcolor))
      ;THM_UI_PALETTE_EVENT, state.tlb, state.vcolorWin, color
      ;state.pagesettings->GetProperty,Variables=variables
      if ~cancelled then begin
        variables->SetProperty,Color=Color
        vcolorwindow = widget_info(state.tlb,find_by_uname='vcolorwindow')
        Widget_Control, vcolorwindow, Get_Value=vcolorWin
        vColorWin->GetProperty,graphics_tree=scene
        scene->setProperty,color=color
        vcolorWin->draw
        ;
        state.cwindow->GetProperty, Panels=panels
        IF Obj_Valid(panels) THEN BEGIN
           panelObjs = panels->Get(/all)
           IF obj_valid(panelobjs[0]) then begin
              FOR i=0, N_Elements(panelObjs)-1 DO BEGIN
        	 panelObjs[i]->GetProperty, Variables=variables               
        	 varObjs=variables->Get(/all)
        	 IF Obj_Valid(varObjs[0]) && N_Elements(varObjs) GT 0 THEN BEGIN
        	       FOR j=0, N_Elements(varObjs)-1 DO BEGIN
        		  varObjs[j]->GetProperty, Text=text
        		  text->SetProperty, Color=color
        	       ENDFOR
        	 ENDIF
              ENDFOR
           ENDIF
        ENDIF

        state.info.historyWin->Update,'Variables color changed.'
        state.statusbar->Update,'Variables color changed.'
      endif
    END
    'FPALETTE': BEGIN
    
      thm_ui_page_color_event,state.tlb,'fcolorwindow','Footer',state.info.historyWin,state.statusBar
    
    END
;    'MPALETTE': BEGIN
; 
;      state.pagesettings->GetProperty,marker=marker
;      marker->GetProperty,Color=currcolor
;      color = reform(PickColor(!p.color, Group_Leader=state.tlb, Cancel=cancelled,currentcolor=currcolor))
;      if ~cancelled then begin
;      	marker->SetProperty,Color=color
;      	state.cwindow->GetProperty, Panels=panels
;      	IF N_Elements(panels) GT 0 && Obj_Valid(panels) THEN panelObjs=panels->Get(/all)
;      	IF N_Elements(panelObjs) GT 0 && Obj_Valid(panelObjs[0]) THEN BEGIN
;         	FOR i=0, N_Elements(panelObjs)-1 DO BEGIN
;         	   panelObjs[i]->GetProperty, Markers=markers
;         	   IF N_Elements(markers) GT 0 && Obj_Valid(markers) THEN markerObjs=markers->Get(/all)
;         	   IF N_Elements(markerObjs) GT 0 && Obj_Valid(markerObjs[0]) THEN BEGIN
;         	      FOR j=0,N_Elements(markerObjs)-1 DO BEGIN
;         	         markerObjs[j]->GetProperty, Settings=settings
;         	         IF Obj_Valid(settings) THEN settings->GetProperty, Label=label
;         	         IF Obj_Valid(Label) THEN label->SetProperty, color=color
;         	      ENDFOR
;         	   ENDIF
;         	ENDFOR
;       	ENDIF
;      	mcolorwindow = widget_info(state.tlb,find_by_uname='mcolorwindow')
;      	Widget_Control, mcolorwindow, Get_Value=mcolorWin
;        mColorWin->GetProperty,graphics_tree=scene
;        scene->setProperty,color=color
;      	mcolorWin->draw
;
;        state.info.historyWin->Update,'Marker color changed.'
;        state.statusBar->Update,"Marker color changed."
;      endif
;    END
    'PORTRAIT': begin
      if event.select then begin ;two events generated for each click
        state.info.historyWin->Update,'Orientation: Portrait.'
        state.statusbar->Update,'Orientation: Portrait.'
      
        if state.orient eq 1 then begin
          state.orient = 0
          thm_ui_page_update_canvas, state.tlb
        endif
        
      end
    end
    'LANDSCAPE': begin
      if event.select then begin ;two events generated for each click
        state.info.historyWin->Update,'Orientation: Landscape.'
        state.statusbar->Update,'Orientation: Landscape.'
     
        if state.orient eq 0 then begin
          state.orient = 1
          thm_ui_page_update_canvas, state.tlb
        endif
        
      endif
    end
    'TMARGIN': state.info.historywin->update, 'Top Margin changed.' ; Replaced by thm_ui_page_margins_set_value
    'BMARGIN': state.info.historywin->update, 'Bottom Margin changed.' ; Replaced by thm_ui_page_margins_set_value
    'RMARGIN': state.info.historywin->update, 'Right Margin changed.' ; Replaced by thm_ui_page_margins_set_value
    'LMARGIN': state.info.historywin->update, 'Left Margin changed.'; Replaced by thm_ui_page_margins_set_value
    'XPANELSPACING': state.info.historywin->update, 'Horizontal panel spacing changed.' ; Replaced by thm_ui_page_spacing_set_value
    'YPANELSPACING': state.info.historywin->update, 'Vertical panel spacing changed.' ; Replaced by thm_ui_page_spacing_set_value
    'XSIZE': state.info.historywin->update, 'Canvase width changed.' ; Replaced with thm_ui_page_canvas_set_value
    'YSIZE': state.info.historywin->update, 'Canvase height changed.' ; Replaced with thm_ui_page_canvas_set_value
    'HEIGHTPROP': state.pagesettings->SetProperty, heightprop=event.select
    'GUTTERWIDTH': if event.valid then state.pagesettings->SetProperty, gutterwidth=event.value
    'DISPLAYONSCREEN': state.pagesettings->SetProperty, displayonscreen=event.select
    'ALTTOPBOTTOM': state.pagesettings->SetProperty, alttopbottom=event.select
    'OFFSETFIRSTPAGE': state.pagesettings->SetProperty, offsetfirstpage=event.select
    'TITLE': begin
       ;replaced by delayed event handling in the set value routine
    end
    'LABEL': begin
      state.pagesettings->GetProperty,labels=labels
      labels->SetProperty, font=event.index
      state.cwindow->GetProperty, Panels=panels
      IF Obj_Valid(panels) THEN BEGIN
         panelObjs = panels->Get(/all)
         IF NOT Is_Num(panelObjs) THEN BEGIN
            FOR i=0, N_Elements(panelObjs)-1 DO BEGIN
               panelObjs[i]->GetProperty, XAxis=xaxis, yAxis=yaxis, zAxis=zaxis
               IF Obj_Valid(xaxis) THEN BEGIN
                  xaxis->GetProperty, Labels=labels
                  labelObjs=labels->Get(/all)
                  IF Obj_Valid(labelObjs[0]) && N_Elements(labelObjs) GT 0 THEN BEGIN                  
                     FOR i=0, N_Elements(labelObjs)-1 DO labelObjs[i]->SetProperty, Font=event.index
                  ENDIF
               ENDIF
               IF Obj_Valid(yaxis) THEN BEGIN
                  yaxis->GetProperty, Labels=labels
                  labelObjs=labels->Get(/all)
                  IF Obj_Valid(labelObjs[0]) && N_Elements(labelObjs) GT 0 THEN BEGIN                  
                     FOR i=0, N_Elements(labelObjs)-1 DO labelObjs[i]->SetProperty, Font=event.index
                  ENDIF
               ENDIF
               IF Obj_Valid(zaxis) THEN BEGIN
                  zaxis->GetProperty, labelTextObject=labelTextObject
;                  labelObjs=labels->Get(/all)
                  IF Obj_Valid(labelTextObject) THEN labelTextObject->SetProperty, Font=event.index
               ENDIF

	       state.info.historywin->Update, "Label font changed."
	       state.statusBar->Update, "Label font changed."
            ENDFOR
         ENDIF
      ENDIF
    end
    'VARIABLES': begin
      state.pagesettings->GetProperty,variables=variables
      variables->SetProperty, font=event.index
      state.cwindow->GetProperty, Panels=panels
      IF Obj_Valid(panels) THEN BEGIN
         panelObjs = panels->Get(/all)
         IF obj_valid(panelobjs[0]) THEN BEGIN
            FOR i=0, N_Elements(panelObjs)-1 DO BEGIN
               panelObjs[i]->GetProperty, Variables=variables
               if obj_valid(variables[0]) then begin
		 varObjs=variables->Get(/all)
		 IF Obj_Valid(varObjs[0]) then begin
		   FOR j=0, N_Elements(varObjs)-1 DO BEGIN
		      varObjs[j]->GetProperty, Text=text
		      text->SetProperty, Font=event.index
		   ENDFOR
		 ENDIF
               endif
               ;
	       state.info.historywin->Update, "Variables font changed."
	       state.statusBar->Update, "Variables font changed."
            ENDFOR
         ENDIF
      ENDIF
    end
    'FOOTER': begin
           ;replaced by delayed event handling in the set value routine
    end
;    'MARKER': begin
;      state.pagesettings->GetProperty,marker=marker                     ;Default.
;      marker->SetProperty, font=event.index
;      focus = Widget_Info(state.tlb, /Kbrd_Focus_Events)
;      state.cwindow->GetProperty, Panels=panels                         ;Panel markers.
;      if Obj_Valid(panels) THEN panelObjs=panels->Get(/all)
;      if Obj_Valid(panelObjs[0]) THEN BEGIN
;        FOR i=0, N_Elements(panelObjs)-1 DO BEGIN
;          panelObjs[i]->GetProperty, Markers=markers
;          IF Obj_Valid(markers) THEN markerObjs=markers->Get(/all)
;          IF Obj_Valid(markerObjs[0]) THEN BEGIN
;              FOR j=0,N_Elements(markerObjs)-1 DO BEGIN
;                 markerObjs[j]->GetProperty, Settings=settings
;                 IF Obj_Valid(settings) THEN settings->GetProperty, Label=label
;                 IF Obj_Valid(Label) THEN label->SetProperty, font=event.index
;              ENDFOR
;          ENDIF
;        ENDFOR
;      	;
;      	state.info.historywin->Update, "Marker font changed."
;      	state.statusBar->Update, "Marker font changed."
;      	;
;      ENDIF     
;;      IF focus NE 1 THEN Widget_Control, state.tlb, /Kbrd_Focus_Events
;    end
    'APPLY': begin

      ;Update values from widgets  
      ;******
      ;   
      thm_ui_page_set_value,event.top,state

      ;Redraw:
      ;*******
      ;
      state.info.drawObject->update,state.info.windowStorage,state.info.loadedData
      state.info.drawObject->draw
      ;
      state.info.historyWin->Update,'Active window refreshed.'
     ; state.statusbar->Update,'Active window refreshed.
    END
    'PAGETITLE': begin
      state.info.historywin->update, 'Page Title changed.'
      ;replaced by delayed event handling in the set value routine
    end
    'PAGEFOOTER': begin
      state.info.historywin->update, 'Page Footer changed.'
      ;replaced by delayed event handling in the set value routine
    end
    'TITLESIZE': begin
      if event.valid then state.info.historywin->update, 'Page Title font size changed.' 
      ;replaced by delayed event handling in the set value routine
    end
    'LABELSIZE': begin

      state.pagesettings->GetProperty, Labels=labels
      labels->GetProperty, Size=size         
      IF double(event.value) LT 0 THEN BEGIN
         state.info.historywin->Update, "Font size values cannot be negative."
         state.statusBar->Update, "Font size values cannot be negative."
      ENDIF ELSE BEGIN
         state.info.historywin->Update, "Label font size changed."
         state.statusBar->Update, "Label font size changed."
      ENDELSE
      size=fix(event.value)
      labels->SetProperty, size=size
      state.cwindow->GetProperty, Panels=panels
      IF Obj_Valid(panels) THEN BEGIN
         panelObjs = panels->Get(/all)
         IF NOT Is_Num(panelObjs) THEN BEGIN
            FOR j=0, N_Elements(panelObjs)-1 DO BEGIN
               panelObjs[j]->GetProperty, XAxis=xaxis, yAxis=yaxis, zAxis=zaxis
               IF Obj_Valid(xaxis) THEN BEGIN
                  xaxis->GetProperty, Labels=Labels
                  if obj_valid(labels) then begin
            		    labelObjs=labels->Get(/all)
            		    IF Obj_Valid(labelObjs[0]) && N_Elements(labelObjs) GT 0 THEN BEGIN                  
            		       FOR i=0, N_Elements(labelObjs)-1 DO labelObjs[i]->SetProperty, Size=size
            		    ENDIF
                  endif
               ENDIF
               IF Obj_Valid(yaxis) THEN BEGIN
                  yaxis->GetProperty, Labels=Labels
                  if obj_valid(labels) then begin
            		    labelObjs=labels->Get(/all)
            		    IF Obj_Valid(labelObjs[0]) && N_Elements(labelObjs) GT 0 THEN BEGIN                  
            		       FOR i=0, N_Elements(labelObjs)-1 DO labelObjs[i]->SetProperty, Size=size
            		    ENDIF
                  endif
               ENDIF
               IF Obj_Valid(zaxis) THEN BEGIN
                  zaxis->GetProperty, Labeltextobject=Labeltextobject
                  if obj_valid(Labeltextobject) then labeltextobject->SetProperty, Size=size
               ENDIF
            ENDFOR
         ENDIF
      ENDIF
      Widget_Control, event.id, set_value=size     
    end
    'VARSIZE': begin
      if event.valid then begin
        state.pagesettings->GetProperty, Variables=variables
        variables->GetProperty, Size=size         
        IF event.value LT 0 THEN BEGIN
           state.info.historywin->Update, "Font size values cannot be negative."
           state.statusBar->Update, "Font size values cannot be negative."
        ENDIF ELSE BEGIN
           state.info.historywin->Update, "Variable font size changed."
           state.statusBar->Update, "Variable font size changed."
        ENDELSE
        size=fix(event.value)
        variables->SetProperty, size=size
        state.cwindow->GetProperty, Panels=panels
        IF Obj_Valid(panels) THEN BEGIN
          panelObjs = panels->Get(/all)
          IF obj_valid(panelobjs[0]) then begin
            FOR i=0, N_Elements(panelObjs)-1 DO BEGIN
              panelObjs[i]->GetProperty, Variables=variables
              varObjs=variables->Get(/all)
              IF Obj_Valid(varObjs[0]) then begin
                FOR j=0, N_Elements(varObjs)-1 DO BEGIN
                  varObjs[j]->GetProperty, Text=text
                  text->SetProperty, Size=size
                ENDFOR
              ENDIF
            ENDFOR
          ENDIF
        ENDIF
        ;Widget_Control, event.id, set_value=size
      endif else state.statusBar->Update, 'Invalid variable font size, please re-enter.'
    end
    'FOOTERSIZE': begin
      state.info.historywin->update, 'Page Footer font size changed.'
      ;replaced by delayed event handling in the set value routine  
    end
;    'MARKERSIZE': begin
;;
;      state.pagesettings->GetProperty, Marker=marker
;      marker->GetProperty, Size=size         
;      IF double(event.value) LT 0 THEN BEGIN
;         state.info.historywin->Update, "Font size values cannot be negative."
;         state.statusBar->Update, "Font size values cannot be negative."
;      ENDIF ELSE BEGIN
;         state.info.historywin->Update, "Marker font size changed."
;         state.statusBar->Update, "Marker font size changed."
;      ENDELSE
;      size=fix(event.value)
;      marker->SetProperty, size=size
;      Widget_Control, event.id, set_value=size     
;      state.cwindow->GetProperty, Panels=panels
;      IF N_Elements(panels) GT 0 && Obj_Valid(panels) THEN panelObjs=panels->Get(/all)
;      IF N_Elements(panelObjs) GT 0 && Obj_Valid(panelObjs[0]) THEN BEGIN
;          FOR i=0, N_Elements(panelObjs)-1 DO BEGIN
;             panelObjs[i]->GetProperty, Markers=markers
;             IF N_Elements(markers) GT 0 && Obj_Valid(markers) THEN markerObjs=markers->Get(/all) ELSE markerObjs=-1
;             IF N_Elements(markerObjs) GT 0 && Obj_Valid(markerObjs[0]) THEN BEGIN
;                FOR j=0,N_Elements(markerObjs)-1 DO BEGIN
;                   markerObjs[j]->GetProperty, Settings=settings
;                   IF Obj_Valid(settings) THEN settings->GetProperty, Label=label
;                   IF Obj_Valid(Label) THEN label->SetProperty, size=size
;                ENDFOR
;             ENDIF
;          ENDFOR
;        ENDIF
;    end
    'TOKEN': BEGIN

      state.pageSettings->SetProperty, Token=uval.ind
      token = state.pageSettings->GetTokenCommand(uval.ind)
     
     titletext = widget_info(state.tlb,find_by_uname='titletext')
     widget_control,titletext,get_value=value
     textselect = widget_info(titletext,/text_select)
     offset = (widget_info(titletext,text_offset_to_xy=textselect[0]))[0]
      
     if offset gt strlen(value) then begin
       titleString = value+token
     endif else begin
       titleString = strmid(value,0,offset) + token + strmid(value,offset,strlen(value))
     endelse
      
     ;  title->setProperty,value=titleString
    
      widget_control, titletext, set_value=titlestring
      widget_control, titletext, set_text_select=textselect
      ;
      state.info.historywin->Update, "Token inserted into title."
      state.statusBar->Update, "Token inserted into title."
    END
    'FOOTERTOKEN': BEGIN

      state.pageSettings->SetProperty, ifooterToken=uval.ind
      token = state.pageSettings->GetTokenCommand(uval.ind)
      
      footertext = widget_info(state.tlb,find_by_uname='footertext')
      widget_control,footertext,get_value=value
      textselect = widget_info(footertext,/text_select)
      offset = (widget_info(footertext,text_offset_to_xy=textselect[0]))[0]
      
      if offset gt strlen(value) then begin
        footerString = value+token
      endif else begin
        footerString = strmid(value,0,offset) + token + strmid(value,offset,strlen(value))
      endelse

      Widget_Control, footerText, set_value=footerString
      Widget_control, footertext, set_text_select=textselect
      ;
      state.info.historywin->Update, "Token inserted into footer."
      state.statusBar->Update, "Token inserted into footer."
    END
    'FORMAT': BEGIN
      state.info.historywin->update, 'Page Title format changed.'
      ;replaced by delayed event handling in the set value routine  
    END
    'FOOTERFORMAT': BEGIN
      state.info.historywin->update, 'Page Footer format changed.'
      ;replaced by delayed event handling in the set value routine  
    END
    'SHOWTITLE': begin
      state.info.historywin->update, 'Show Title changed.'
      ;replaced by delayed event handling in the set value routine  
    end
    'SHOWFOOTER': begin
      state.info.historywin->update, 'Show Footer changed.'
      ;replaced by delayed event handling in the set value routine  
    end
;    'MARKERTITLE': begin
;
;      widget_control,event.id,get_value=value
;      state.pageSettings->GetProperty,marker=marker
;      marker->SetProperty,value=value
;      ;
;      state.info.historywin->Update, "Marker title changed."
;      state.statusBar->Update, "Marker title changed."
;    end
    'OVERLAPMAJORTICKS': begin

      widget_control,event.id,get_value=value
      if ~stregex(value,'[^0-9]',/boolean) then begin
        state.pagesettings->SetProperty, overlapmajorticks=fix(value)

;        state.info.historywin->Update, "Marker title changed."
;        state.statusBar->Update, "Marker title changed."
      endif else begin
        state.pagesettings->GetProperty, overlapmajorticks=curr_value
        widget_control,event.id, set_value=strtrim(curr_value,2)
      endelse
    end
    'SHOWVALUES': begin

      state.pagesettings->SetProperty,showvalues=event.select
      id=widget_info(state.tlb,find_by_uname='closertext')
      widget_control,id,sensitive=event.select
      id=widget_info(state.tlb,find_by_uname='closerdroplist')
      widget_control,id,sensitive=event.select
    end
    'CLOSERTHANVALUE': begin

      widget_control,event.id,get_value=value
      if ~stregex(value,'[^0-9]',/boolean) then begin
        state.pagesettings->SetProperty, CloserThanValue=fix(value)
      endif else begin
        state.pagesettings->GetProperty, CloserThanValue=curr_value
        widget_control,event.id, set_value=strtrim(curr_value,2)
      endelse
    end
    'CLOSERTHANUNITS': begin
      state.pagesettings->SetProperty,CloserThanUnits=event.index
    end
    'USESAMEYRANGE': begin
      state.pagesettings->SetProperty,UseSameYRange=event.select
      id=widget_info(state.tlb,find_by_uname='yminorlabel')
      widget_control,id,sensitive=event.select
      id=widget_info(state.tlb,find_by_uname='yminortext')
      widget_control,id,sensitive=event.select
      id=widget_info(state.tlb,find_by_uname='ymajorlabel')
      widget_control,id,sensitive=event.select
      id=widget_info(state.tlb,find_by_uname='ymajortext')
      widget_control,id,sensitive=event.select
    end
    'YMINORTEXT': begin
      widget_control,event.id,get_value=value
      if ~stregex(value,'[^0-9]',/boolean) then begin
        state.pagesettings->SetProperty, numMinorTicks=fix(value)
      endif else begin
        state.pagesettings->GetProperty, numMinorTicks=curr_value
        widget_control,event.id, set_value=strtrim(curr_value,2)
      endelse
    end
    'YMAJORTEXT': begin
      widget_control,event.id,get_value=value
      if ~stregex(value,'[^0-9]',/boolean) then begin
        state.pagesettings->SetProperty, numMajorTicks=fix(value)
      endif else begin
        state.pagesettings->GetProperty, numMinorTicks=curr_value
        widget_control,event.id, set_value=strtrim(curr_value,2)
      endelse
    end
    'SKIPBLANKS': state.pagesettings->SetProperty,skipBlanks = event.select
;    'TITLECOLOR': begin
;        unneeded, colors now handled using the palette events
;    end
    ELSE: ;Print, ''
  ENDCASE
  
      ; must ALWAYS reset state value
      
  Widget_Control, event.TOP, Set_UValue=state, /No_Copy
     
  RETURN
END ;--------------------------------------------------------------------------------



pro thm_ui_page_options, info

  ;Get PAGESETTINGS from active window:
  ;************************************
  ;
  cwindow = info.windowstorage->getactive()
  cwindow->GetProperty,settings = pagesettings

  gui_id=info.master

  pagesettings->save

  err_xxx = 0
  Catch, err_xxx
  IF (err_xxx NE 0) THEN BEGIN
    Catch, /Cancel
    Help, /Last_Message, Output = err_msg
    FOR j = 0, N_Elements(err_msg)-1 DO info.historywin->update,err_msg[j]
    Print, 'Error--See history'
    ok=error_message('Unknown error starting Page Options. See console for details.',$
       /noname, /center, title='Error in Page Options')
    widget_control, tlb,/destroy
    thm_gui_error,gui_id,info.historywin
    RETURN
  ENDIF

      ;top level base
      
  tlb = Widget_Base(/Col, Title='THEMIS: Page Options ', Group_Leader=info.master, /Modal, /Floating,/TLB_KILL_REQUEST_EVENTS, tab_mode=1)
                   
      ;main base widgets

  tabBase = Widget_Tab(tlb, Location=location)
  buttonBase = Widget_Base(tlb, /Row, /Align_Center)
  statusBase = Widget_Base(tlb, /Row, /Align_Center)
  mainBase = Widget_Base(tabBase, Title='Text', /Col, YPad=1)

      ;text panel bases
      
  textBase = Widget_Base(mainBase, Title='Text', /Col)
    titleBase = Widget_Base(textBase, /Row)
    tokenBase = Widget_Base(textBase, /Row, /Align_Center)
    footerBase = Widget_Base(textBase, /Row, YPad=2)
    footertokenBase = Widget_Base(textBase, /Row, /Align_Center)
;    markerBase = Widget_Base(textBase, /Row, ypad=2)
    fontlabelBase=Widget_Base(textBase, /row)
    fontFrameBase=Widget_Base(textBase, /row)
    fontBase = Widget_Base(textBase, /row, /Align_Left, frame=3)
    fontsBase = Widget_Base(fontBase, /col, space=6)
 
      
  layoutBase = Widget_Base(tabBase, title='Layout', /Col, YPad=2)
    row1Base = Widget_Base(layoutBase, /Row, YPad=2)
      panelBase = Widget_Base(row1Base, /Col)
      gutterBase = Widget_Base(row1Base, /Col)
    row2Base = Widget_Base(layoutBase, /Row)
      marginBase = Widget_Base(row2Base, /Col)
      row2Col2Base = Widget_Base(row2Base, /Col)
      canvasSizeBase = Widget_Base(row2Col2Base, /Col)
      backgroundBase = Widget_Base(row2Col2Base, /Col)

      ;data panel bases
      
;  dataBase = Widget_Base(tabBase,title='Data', /Col)
;    overlapBase = Widget_Base(dataBase, /Row, XPad=5, YPad=10)
;    showVarBase = Widget_Base(dataBase, /Col, XPad=5, YPad=5)
;    sameYBase = Widget_Base(dataBase, /Col,XPad=5, YPad=5)
;    skipBase = Widget_Base(dataBase, /Col, /Nonexclusive, XPad=5, YPad=5)
    
      ;text panel widgets
      
  pagesettings->GetProperty, $
    xpanelspacing=xpanelspacing, $
    ypanelspacing=ypanelspacing, $
    heightprop=heightprop, $
    gutterwidth=gutterwidth, $
    displayonscreen=displayonscreen, $
    alttopbottom=alttopbottom, $
    offsetfirstpage= offsetfirstpage, $
    title=title, $
    labels=labels, $
    variables=variables, $
    footer=footer, $
    marker=marker, $
    overlapmajorticks=overlapmajorticks, $
    topprintmargin=topprintmargin, $
    bottomprintmargin = bottomprintmargin, $
    rightprintmargin = rightprintmargin, $
    leftprintmargin = leftprintmargin, $
    ShowValues=ShowValues, $
    CloserThanValue= CloserThanValue, $ 
    CloserThanUnits= CloserThanUnits, $
    useSameYRange= useSameYRange, $
    numMinorTicks= numMinorTicks, $
    numMajorTicks= numMajorTicks, $
    canvasSize=canvassize, $
    orientation=orientation, $
    backgroundcolor=backgroundcolor, $
    skipBlanks= skipBlanks
  title->GetProperty, $
    value=pagetitle, $
    size=titlesize, $
    font=titlefont, $
    format=titleformat,$
    color=titlecolor, $
    show=showtitle
  labels->GetProperty, $
    size=labelsize, $
    color=labelcolor, $
    font=labelfont
  variables->GetProperty, $
    size=varsize, $
    color=variablescolor, $
    font=varsfont
  footer->GetProperty, $
    size=footersize, $
    font=footerfont, $
    format=footerformat,$
    value=pagefooter, $
    color=footercolor, $
    show=showfooter
  marker->GetProperty, $
    size=markersize, $
    font=markerfont, $
    color=markercolor


  ;Call save method on all PANEL objects:
  ;**************************************
  ;
  cwindow->GetProperty, Panels=panels
  IF Obj_Valid(panels) THEN BEGIN
    panelObjs = panels->Get(/all)
    IF obj_valid(panelobjs[0]) then begin
      FOR i=0, N_Elements(panelObjs)-1 do panelobjs[i]->save
    endif
  endif

  getresourcepath,rpath
  palettebmp = read_bmp(rpath + 'color.bmp', /rgb)
  thm_ui_match_background, tlb, palettebmp

  tokennames = pagesettings->GetTokenNames()
  formatnames= pagesettings->GetFormatNames()
  
  ;detect size of largest label, so that we can mandate that each label has the same width.
  ;Space padding is not effective at this task because space sizes are not consistent across platforms
  
;  markerLabel = Widget_Label(markerBase, Value='Default Marker Title:   ',/align_left)
  footerLabel = Widget_Label(footerBase, Value='Page Footer:  ',/align_left)
  
  geo_struct = widget_info(footerLabel,/geometry)
  
  labelXSize = geo_struct.scr_xsize
  
  ;footerLabel = Widget_Label(footerBase, Value='Page Footer:  ',xsize=labelXSize,/align_left)
  titleLabel = Widget_Label(titleBase, Value='Page Title:  ',xsize=labelXSize,/align_left)
  
  titleText = Widget_Text(titleBase,/all_events, /Editable, Value=pagetitle, XSize=35, ysize=1, uval='PAGETITLE', uname='titletext')
  showTitleBase = Widget_Base(titleBase, /Row, /Nonexclusive)
  showTitleButton = Widget_Button(showTitleBase, Value = 'Show Title', uval='SHOWTITLE',uname='showtitle')
  if showtitle then Widget_Control, showtitleButton, /Set_Button

  ;Title buttons (token and format):
  ;
  tokenButton = Widget_Button(tokenBase, Value='  Token...  ', /Menu)
  tokenSubButtons=lonarr(n_elements(tokennames))
  for i = 0,n_elements(tokennames)-1 do tokenSubButtons[i]= Widget_Button(tokenButton, Value=tokennames[i], uval={name:'TOKEN', ind:i, format:0})
  spaceLabel = Widget_Label(tokenBase, Value='  ')  
  ;
  ;
;  formatButton = Widget_Button(tokenBase, Value='  Format...  ', /Menu)
;  formatSubButtons=lonarr(n_elements(formatnames))
;  for i = 0,n_elements(formatnames)-1 do formatSubButtons[i]= Widget_Button(formatButton, Value=formatnames[i], uval={name:'FORMAT', ind:i, format:1})

  noformatidx = where(formatNames eq 'No Format')

  ;title format now a combobox
  formatbox = widget_combobox(tokenBase,value=formatNames,UVALUE='FORMAT',uname='titleformat')
  if titleformat eq -1 then begin
    widget_control,formatbox,set_combobox_select=noformatidx
  endif else begin
    widget_control,formatbox,set_combobox_select=titleformat
  endelse
  

  ;Footer buttons (token and format):
  ;
  footerTokenButton = Widget_Button(footerTokenBase, Value='  Token...  ', /Menu)
  footerTokenSubButtons=lonarr(n_elements(tokennames))
  for i = 0,n_elements(tokennames)-1 do footertokenSubButtons[i]= Widget_Button(footerTokenButton, Value=tokennames[i], uval={name:'FOOTERTOKEN', $
    ind:i, format:0})
  spaceLabel = Widget_Label(footerTokenBase, Value='  ')  
  
  formatbox = widget_combobox(footertokenBase,value=formatNames,UVALUE='FOOTERFORMAT',uname='footerformat')
  if footerformat eq -1 then begin
    widget_control,formatbox,set_combobox_select=noformatidx
  endif else begin
    widget_control,formatbox,set_combobox_select=footerformat
  endelse
  
  ;footer format now a combobox
  ;footerformatButton = Widget_Button(footerTokenBase, Value='  Format...  ', /Menu)
  ;footerformatSubButtons=lonarr(n_elements(formatnames))
  ;for i = 0,n_elements(formatnames)-1 do footerformatSubButtons[i]= Widget_Button(footerformatButton, Value=formatnames[i], uval={name:'FOOTERFORMAT', $
  ;  ind:i, format:1})

  footerText = Widget_Text(footerBase, /all_events , /Editable, Value=pagefooter, XSize=35, YSize=1, uval='PAGEFOOTER',uname='footertext') 
  showFooterBase = Widget_Base(footerBase, /Row, /Nonexclusive)
  showFooterButton =  Widget_Button(showfooterBase, Value = 'Show Footer', uval='SHOWFOOTER',uname='showfooter')
  if showfooter then Widget_Control, showfooterButton, /Set_Button
;  markerText = Widget_Text(markerBase,  /all_events, /Editable, Value = markertitle, XSize=35, uval='MARKERTITLE', sensitive=0) 
  fontLabel = Widget_Label(fontLabelBase, Value = 'Font Styles: ', /Align_Left)
   fontValues = pagesettings->GetFontnames()
   
  fontslabelsBase = widget_base(fontsbase,/row,ypad=0)
  titleFontBase = widget_base(fontsBase,/row,ypad=0)
;  labelFontBase = widget_base(fontsBase,/row,ypad=0)  
  varFontBase = widget_base(fontsBase,/row,ypad=0)
;  markerFontBase = widget_base(fontsBase,/row,ypad=0)
  footerFontBase = widget_base(fontsBase,/row,ypad=0)
    
  varFontLabel = widget_label(varFontBase,value='Variables: ')
  
  geo_struct = widget_info(varFontLabel,/geometry)
  
  label_xsize = geo_struct.scr_xsize
  label_ysize = geo_struct.scr_ysize
  
  fontTitleLabel = widget_label(titleFontBase,value='Title: ',xsize=label_xsize)
;  fontLabelLabel = widget_label(labelFontBase,value='Label: ',xsize=label_xsize)
  
;  fontMarkerLabel = widget_label(markerFontBase,value='Markers: ',xsize=label_xsize)

  fontFooterLabel = widget_label(footerFontBase,value='Footer: ',xsize=label_xsize)

  fontname_xsize = 150

  ;this base allows the droplist to be undersized, if necessary
  combobase = widget_base(titleFontBase,/row,ypad=0,xpad=0)

  fontTitleDroplist = Widget_Combobox(combobase,xsize=fontname_xsize, Value=fontValues, uval='TITLE',uname='titlecombo')
  widget_control,fontTitleDroplist,set_combobox_select=titlefont ;make sure the setting for the current window is the default when opened
 
  
  ;this base allows the droplist to be undersized, if necessary
;  combobase = widget_base(labelFontBase,/row,ypad=0,xpad=0)
  
;  fontLabelDroplist = Widget_Combobox(combobase,  XSize=fontname_xsize, value=fontValues,UValue='LABEL',uname='labelcombo')
;  widget_control,fontLabelDroplist,set_combobox_select=labelfont ;make sure the setting for the current window is the default when opened
 
  
  ;this base allows the droplist to be undersized, if necessary
  combobase = widget_base(varFontBase,/row,ypad=0,xpad=0)
  
  fontVarsDroplist = Widget_Combobox(comboBase,  XSize=fontname_xsize, Value=fontValues, uval='VARIABLES',uname='variablecombo')
  widget_control,fontVarsDroplist,set_combobox_select=varsfont ;make sure the setting for the current window is the default when opened
 
  ;this base allows the droplist to be undersized, if necessary
;  combobase = widget_base(markerFontBase,/row,ypad=0,xpad=0)
  
  ;marker combobox uname and uval are incorrect, rename if code is to be used
;  fontMarkerDroplist = Widget_Combobox(comboBase,  XSize=fontname_xsize, Value=fontValues, uval='VARIABLES',uname='variablecombo')
;  widget_control,fontMarkerDroplist,set_combobox_select=varsfont ;make sure the setting for the current window is the default when opened
  
  
  ;this base allows the droplist to be undersized, if necessary
  combobase = widget_base(footerFontBase,/row,ypad=0,xpad=0)
  
  fontFooterdroplist = Widget_Combobox(comboBase,  XSize=fontname_xsize, Value=fontValues, uval='FOOTER',uname='footercombo')
  widget_control,fontFooterDroplist,set_combobox_select=footerfont ;make sure the setting for the current window is the default when opened 
  
;  ;this base allows the droplist to be undersized, if necessary
;  combobase = widget_base(markerFontBase,/row,ypad=0,xpad=0)
 
 
  widget_control,fontTitleDroplist,set_combobox_select=titlefont
  fontTitleIncrement = thm_ui_spinner(titleFontBase, Increment=1, Value=titlesize, uval='TITLESIZE',/all_events,uname='titlesize')
  cb1Base = Widget_Base(titleFontBase, /row)
;  palettebmp = filepath('palette.bmp', Subdir=['resource', 'bitmaps'])
  tpaletteButton = Widget_Button(cb1Base, Value=palettebmp, /Bitmap, UValue='TPALETTE', Tooltip='Choose color from Palette')
  
  geo_struct = widget_info(tpaletteButton,/geometry)
  rowysize = geo_struct.scr_ysize
  
  tcolorWindow = Widget_Draw(titleFontBase, XSize=50, YSize=rowysize,uname='tcolorwindow', $
                             graphics_level=2,renderer=1,retain=1,units=0,frame=1, /expose_events)
 
;  widget_control,fontLabelDroplist,set_combobox_select=labelfont
;  fontLabelIncrement = thm_ui_spinner(labelFontBase, Increment=1, Value=labelsize, uval='LABELSIZE')
;  cb2Base = Widget_Base(labelFontBase, /row,ypad=pad)
;  palettebmp = filepath('palette.bmp', Subdir=['resource', 'bitmaps'])
;  lpaletteButton = Widget_Button(cb2Base, Value=palettebmp, /Bitmap, UValue='LPALETTE', Tooltip='Choose color from Palette')
;  lcolorWindow = Widget_Draw(labelFontBase, XSize=50, YSize=rowysize,uname='lcolorwindow',graphics_level=2,renderer=1,retain=1,units=0,frame=1)

  widget_control,fontVarsDroplist,set_combobox_select=varsfont
  fontVarsIncrement = thm_ui_spinner(varFontBase, Increment=1, Value=varsize, uval='VARSIZE')
  cb3Base = Widget_Base(varFontBase, /row,ypad=pad)
;  palettebmp = filepath('palette.bmp', Subdir=['resource', 'bitmaps'])
  vpaletteButton = Widget_Button(cb3Base, Value=palettebmp, /Bitmap, $
    UValue='VPALETTE', Tooltip='Choose color from Palette')
  vcolorWindow = Widget_Draw(varFontBase, XSize=50, YSize=rowysize,uname='vcolorwindow',graphics_level=2,renderer=1,retain=1,units=0,frame=1)

  widget_control,fontFooterDroplist,set_combobox_select=footerfont
  fontFooterIncrement=thm_ui_spinner(footerFontBase, Increment=1, Value=footersize, uval='FOOTERSIZE',uname='footersize')
  cb4Base = Widget_Base(footerFontBase, /row,ypad=pad)
;  palettebmp = filepath('palette.bmp', Subdir=['resource', 'bitmaps'])
  fpaletteButton = Widget_Button(cb4Base, Value=palettebmp, /Bitmap, $
    UValue='FPALETTE', Tooltip='Choose color from Palette')
  fcolorWindow = Widget_Draw(footerFontBase, XSize=50, YSize=rowysize,uname='fcolorwindow', $
                             graphics_level=2,renderer=1,retain=2,units=0,frame=1, /expose_events)

;  widget_control,fontMarkerDroplist,set_combobox_select=markerfont
;  fontMarkerIncrement=thm_ui_spinner(markerFontBase, Increment=1, Value=markersize, uval='MARKERSIZE')
;  cb5Base = Widget_Base(markerFontBase, /row,ypad=pad)
;;  palettebmp = filepath('palette.bmp', Subdir=['resource', 'bitmaps'])
;  mpaletteButton = Widget_Button(cb5Base, Value=palettebmp, /Bitmap, $
;    UValue='MPALETTE', Tooltip='Choose color from Palette')
;  mcolorWindow = Widget_Draw(markerFontBase, XSize=50, YSize=rowysize,uname='mcolorwindow',graphics_level=2,renderer=1,retain=1,units=0,frame=1)

;  getresourcepath,rpath
;  palettebmp = rpath + 'color.bmp'
 
 ;labels get created later so we know how large to make them
  spaceLabel = widget_label(fontsLabelsBase,value=' ',/align_center,xsize=label_xsize)
  fontNamelabel = Widget_Label(fontsLabelsBase, value='Font Name', /align_center,xsize=fontname_xsize)
  sizeLabel = Widget_Label(fontsLabelsBase, value='Size (points)', /align_center)
  spaceLabel = Widget_Label(fontsLabelsBase, value=' ', /align_center)
;  colorLabel = Widget_Label(fontsLabelsBase, value='Color', /align_center)

      ;layout panel widgets
  
  arrangeLabel = Widget_Label(panelBase, Value= 'Panel Arrangement: ', /Align_Left)
  arrangeBase = Widget_Base(panelBase, /Col, frame=3, tab_mode=1)
  label1txt = 'Horizontal Panel Spacing (pts):  '
  label2txt = 'Vertical Panel Spacing (pts):  '
  xspacingBase = Widget_Base(arrangeBase, /Row)
  yspacingBase = Widget_Base(arrangeBase, /Row)
  
  if strlen(label1txt) ge strlen(label2txt) then begin
   
    spacingIncrement=thm_ui_spinner(xspacingBase, label= label1txt, uname='xpanelspacing',$
      Increment=1, Value=xpanelspacing, uval='XPANELSPACING',getXLabelSize=xsize)
 
    spacingIncrement=thm_ui_spinner(yspacingBase, label= label2txt, uname='ypanelspacing',$
      Increment=1, Value=ypanelspacing, uval='YPANELSPACING',xlabelsize=xsize)
     
  endif else begin
     
    spacingIncrement=thm_ui_spinner(yspacingBase, label= label2txt, uname='ypanelspacing',$
      Increment=1, Value=ypanelspacing, uval='YPANELSPACING',getXLabelSize=xsize)
   
    spacingIncrement=thm_ui_spinner(xspacingBase, label= label1txt, uname='xpanelspacing',$
      Increment=1, Value=xpanelspacing, uval='XPANELSPACING',xlabelsize=xsize)
 
  endelse
  
  heightBase = Widget_Base(arrangeBase, /Row, /Nonexclusive)
  heightButton = Widget_Button(heightBase, Value = 'Mark Proportional to Range', uval='HEIGHTPROP')
  if heightprop then widget_control, HeightButton, /set_button
  widget_control,heightButton,sensitive=0
  gutterLabel= Widget_Label(gutterBase, Value='Gutter:', /Align_Left, sensitive=0)
  gframeBase = Widget_Base(gutterBase, /Col, Frame=3, YPad=3)
  widthBase = Widget_Base(gframeBase, /Row)
  widthIncrement = thm_ui_spinner(widthBase, Label='Width (pts): ', $
    Increment=1, Value=gutterwidth, uval='GUTTERWIDTH',sensitive=0)
  gbuttonBase = Widget_Base(gframeBase, /Col, /Nonexclusive, YPad=2)
  displayButton = Widget_Button(gbuttonBase, Value = 'Display on screen', uval='DISPLAYONSCREEN', $
    sensitive=0)
  if displayonscreen then widget_control, displayButton, /set_button
  alternateButton = Widget_Button(gbuttonBase, Value = 'Alternate top/bottom when printing', $
    uval='ALTTOPBOTTOM', sensitive=0)
  if alttopbottom then widget_control, alternateButton, /set_button
  firstButton = Widget_Button(gbuttonBase, Value = 'First page has offset at bottom', $
    uval='OFFSETFIRSTPAGE', sensitive=0)
  if offsetfirstpage then widget_control, alternateButton, /set_button

  canvasSizeLabel = Widget_Label(canvasSizeBase, Value='Canvas Size (inches):', /Align_Left)
  xSizeBase = Widget_Base(canvasSizeBase, /col, frame=3, /Align_Center, ypad=5, space=6)
    xSizeBase2 = Widget_Base(xSizeBase, /row, /Align_Center, ypad=0, space=0)
      xSizeIncrement = thm_ui_spinner(xSizeBase2, Label='X:', Value=canvassize[0], uval='XSIZE', uname='xsize', sens=0)
      ySizeIncrement = thm_ui_spinner(xSizeBase2, Label='Y:', Value=canvassize[1], uval='YSIZE', uname='ysize', sens=0)
    orientationBase = widget_base(xsizebase, /row, /exclusive, ypad=0)
      portrait = widget_button(orientationbase, value='Portrait', uval='PORTRAIT', uname='portrait')
      landscape = widget_button(orientationbase, value='Landscape', uval='LANDSCAPE', uname='landscape')
      if orientation then widget_control, landscape, set_button=1 $
        else widget_control, portrait, set_button=1

  backgroundLabel = Widget_Label(backgroundBase, Value='Background:', /Align_Left)
  backFrameBase = Widget_Base(backgroundBase, /Row, Frame=3, XPad=2, YPad=3)
  paletteBase = Widget_Base(backframeBase, /Row, XPad=3, ypad=2, space=3)
  colorLabel = Widget_Label(paletteBase, Value='Color:  ')
;  getresourcepath,rpath
;  palettebmp = rpath + 'color.bmp'
;  palettebmp = FilePath('palette.bmp', Subdir=['resource', 'bitmaps'])
  paletteButton = Widget_Button(paletteBase, Value=palettebmp, /Bitmap, UValue='BPALETTE', Tooltip='Choose color from Palette')
  vspaceLabel = Widget_Label(paletteBase, Value='  ')    
  bcolorWindow = Widget_Draw(paletteBase, XSize=50, YSize=21,uname='bcolorwindow', $
                          graphics_level=2,renderer=1,retain=2,units=0,frame=1, /expose_events)

  spaceLabel = Widget_Label(paletteBase, Value='   ')                              

  marginLabel = Widget_Label(marginBase, Value='Margins: ', /Align_Left)
  mframeBase = Widget_Base(marginBase, /Col, Frame=3, tab_mode=1)
  topMarginBase = Widget_Base(mframeBase, /Row, /Align_Center)
  tmIncrement = thm_ui_spinner(topmarginBase, Label='Top: ', Value=topprintmargin, uval='TMARGIN', uname='tmargin')
  centerMarginBase = Widget_Base(mframeBase, /Row)
  lmIncrement = thm_ui_spinner(centerMarginBase, Label='Left: ', Value=leftprintmargin, uval='LMARGIN', uname='lmargin')
  rmIncrement = thm_ui_spinner(centerMarginBase, Label='   Right: ', Value=rightprintmargin, uval='RMARGIN', uname='rmargin')
  bottomMarginBase = Widget_Base(mframeBase, /Row, /Align_Center)
  bmIncrement = thm_ui_spinner(bottomMarginBase, Label='Bottom: ', Value=bottomprintmargin, uval='BMARGIN', uname='bmargin')
  dimensionLabel = Widget_Label(mframeBase, Value='All dimensions in inches', /Align_Center)  

      ;data panel widgets
      
;  overlapLabel = Widget_Label(overlapBase, Value='Overlap data in each page by ', sensitive=0);

;  overlapText = Widget_Text(overlapBase, /all_events, Value=strtrim(overlapmajorticks,2), XSize=4, /Editable, uval='OVERLAPMAJORTICKS', sensitive=0)
;  majorLabel = Widget_Label(overlapBase, Value=' major ticks.', sensitive=0)
;  showVarNonBase = Widget_Base(showVarBase, /Nonexclusive)
;  showVarButton = Widget_Button(showVarnonBase, Value='Show variable values if nearest points to tick is ', uval='SHOWVALUES', sensitive=0)
;  if ShowValues then Widget_Control, showvarButton, /Set_Button
;  closerBase = Widget_Base(showvarBase, /Row, Frame=3, YPad=4, XPad=4)
;  closerLabel = Widget_Label(closerBase, Value='closer than  ', sensitive=0)
;  closerText= $
;    Widget_Text(closerBase, /all_events, Value=strtrim(CloserThanValue,2), XSize=12, /Editable, sensitive=ShowValues, uval='CLOSERTHANVALUE',uname='closertext')
;  closerDroplist = $
;    Widget_combobox(closerBase,Value=[' hours  ',' minutes ',' seconds ',' days ','<none>'],sensitive=ShowValues, $
;    uval='CLOSERTHANUNITS',uname='closerdroplist')
;  widget_control,closerDroplist,set_combobox_select=CloserThanUnits
;  yAxisBase = Widget_Base(sameyBase, /Nonexclusive)
;  yAxisButton = Widget_Button(yaxisBase, Value='Use same y-axis range for all autoscaled panels ',uval='USESAMEYRANGE', sensitive=0)
;  ;if useSameYRange then widget_control,yAxisButton,/set_button
;  yTextBase = Widget_Base(sameyBase, /Col, Frame=3, YPad=2, XPad=2)  
;  yMinorBase = Widget_Base(ytextBase, /Row)
;  yMinorLabel = Widget_Label(yMinorBase, Value='# minor ticks:  ', Sensitive=0,uname='yminorlabel')
;  yMinorText = Widget_Text(yMinorBase, Value=strtrim(numMinorTicks,2),/all_events,/Editable, Sensitive=0,uname='yminortext',uval='YMINORTEXT')
;  yMajorBase = Widget_Base(yTextBase, /Row)
;  yMajorLabel = Widget_Label(yMajorBase, Value='# major ticks:  ', Sensitive=0,uname='ymajorlabel')
;  yMajorText = Widget_Text(yMajorBase, Value=strtrim(numMajorTicks,2), /all_events, /Editable, Sensitive=0,uname='ymajortext',uval='YMAJORTEXT')
;  skipButton = Widget_Button(skipBase, Value='Skip blank pages',uval='SKIPBLANKS', sensitive=0)
;  if skipBlanks then Widget_Control, skipButton, /Set_Button
                            
  okButton = Widget_Button(buttonBase, Value='OK', XSize = 75, uval='OK')
  applyButton = Widget_Button(buttonBase, Value='Apply', XSize = 75, uval='APPLY')
  cancelButton = Widget_Button(buttonBase, Value='Cancel', UValue='CANC', XSize = 75)
  templateButton = Widget_Button(buttonBase, Value='Save to Template', UValue='TEMP',xsize=125)
;  helpButton = Widget_Button(buttonBase, Value='Help', XSize = 75)
       
  ; Create Status Bar Object
  statusBar = Obj_New('THM_UI_MESSAGE_BAR', Value='Status information is displayed here.', statusBase,Xsize=75, YSize=1)

  pagesettings->getProperty,orientation=orient
  state = {tlb:tlb, gui_id:gui_id, pagesettings:pagesettings, lcolorWin:0, vcolorWin:0, $
           mcolorWin:0, info:info,drawObject:info.drawObject,windowStorage:info.windowStorage,historywin:info.historywin,template:info.template_object,statusBar:statusBar, cwindow:cwindow,orient:orient}

  centerTLB,tlb

  Widget_Control, tlb, Set_UValue=state, /No_Copy
  Widget_Control, tlb, /Realize

 ;No longer uses CT to look up color values
 ; Device, Decomposed=0
 ; LoadCT, 39
 ; red=[255,0,0]  
 ; blue=[0,0,255]
 ; gray=[110,110,110]  
  
  tcolorwindow = widget_info(tlb,find_by_uname='tcolorwindow')
  Widget_Control, tcolorwindow, Get_Value=tcolorWin
  scene=obj_new('IDLGRSCENE', color=titlecolor)
  tcolorWin->setProperty,graphics_tree=scene
  tcolorWin->draw

;  lcolorwindow = widget_info(tlb,find_by_uname='lcolorwindow')
;  Widget_Control, lcolorwindow, Get_Value=lcolorWin
;  scene=obj_new('IDLGRSCENE', color=labelcolor)
;  lcolorWin->setProperty,graphics_tree=scene
;  lcolorWin->draw, scene

  vcolorwindow = widget_info(tlb,find_by_uname='vcolorwindow')
  Widget_Control, vcolorwindow, Get_Value=vcolorWin
  scene=obj_new('IDLGRSCENE', color=variablescolor)
  vcolorWin->setProperty,graphics_tree=scene
  vcolorWin->draw, scene

  fcolorwindow = widget_info(tlb,find_by_uname='fcolorwindow')
  Widget_Control, fcolorwindow, Get_Value=fcolorWin
  scene=obj_new('IDLGRSCENE', color=footercolor)
  fcolorWin->setProperty,graphics_tree=scene
  fcolorWin->draw, scene

;  mcolorwindow = widget_info(tlb,find_by_uname='mcolorwindow')
;  Widget_Control, mcolorwindow, Get_Value=mcolorWin
;  scene=obj_new('IDLGRSCENE', color=markercolor)
;  mcolorWin->setProperty,graphics_tree=scene
;  mcolorWin->draw, scene

  bcolorwindow = widget_info(tlb,find_by_uname='bcolorwindow')
  Widget_Control, bcolorwindow, Get_Value=bcolorWin
  if obj_valid(scene) then scene->remove,/all
  scene=obj_new('IDLGRSCENE', color=backgroundcolor)
  bcolorWin->draw, scene


  XManager, 'thm_ui_page_options', tlb, /No_Block

  RETURN
END
