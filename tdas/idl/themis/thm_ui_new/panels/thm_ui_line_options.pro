 ;+ 
;NAME:
; thm_ui_line_options

;PURPOSE:
; A widget interface for modifying line attributes 
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
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-09-14 09:55:00 -0700 (Mon, 14 Sep 2009) $
;$LastChangedRevision: 6719 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_line_options.pro $
;
;---------------------------------------------------------------------------------

; Get settings of current trace and set widgets to reflect current settings

pro thm_ui_init_line_panel, tlb, state=state

  compile_opt idl2, hidden
  
  statedef = ~(~size(state,/type))
; 
  if ~statedef then begin
    Widget_Control, tlb, Get_UValue=state, /No_Copy  ;Only get STATE if it is not passed in.
  endif else begin
    tlb = state.tlb
  endelse


  ;if selected trace is spectra then desensitize line options
  if ~obj_valid(*state.ctrace) || obj_isa(*state.ctrace, 'thm_ui_spectra_settings') then begin
  
    widget_control, state.subMainLineBase, sensitive=0
;    widget_control, state.subMainLineBaseRight, sensitive=0
    if ~statedef then Widget_Control, tlb, Set_UValue=state, /No_Copy   ;Only put STATE if it was not passed in.
    RETURN
  endif else begin
    widget_control, state.subMainLineBase, sensitive = 1
  endelse


;****** Line Options **********************************************************
  id = widget_info(tlb, find_by_uname = 'showline')
  *state.ctrace->GetProperty, LineStyle=lineStyleObj
  lineStyleObj->GetProperty, Show=value
  widget_control, id, set_button = value
  
  id = widget_info(tlb, find_by_uname = 'mirror')
  *state.ctrace->GetProperty, mirrorline=value 
  widget_control, id, set_button = value

  ; intialize color window
  *state.ctrace->GetProperty, LineStyle=lineStyleObj
  lineStyleObj->GetProperty, color=value
  Widget_Control, state.lcolorWindow, Get_Value=lcolorWin
  if obj_valid(scene) then scene->remove,/all
  scene=obj_new('IDLGRSCENE', color=value)
  lcolorWin->draw, scene
    
  id = widget_info(tlb, find_by_uname = 'linestyle')
  *state.ctrace->GetProperty, LineStyle=lineStyleObj
  lineStyleObj->GetProperty, id=value
  widget_control, id, set_combobox_select = value

  id = widget_info(tlb, find_by_uname = 'thickness')
  *state.ctrace->GetProperty, LineStyle=lineStyleObj
  lineStyleObj->GetProperty, thickness=value
  widget_control, id, set_value = value
  
  
;****** Symbol Options ********************************************************
  id = widget_info(tlb, find_by_uname = 'showsymbol')
  *state.ctrace->GetProperty, symbol=symbolObj
  symbolObj->GetProperty, show=value
  widget_control, id, set_button = value

  id = widget_info(tlb, find_by_uname = 'fillsymbol')
  *state.ctrace->GetProperty, symbol=symbolObj
  symbolObj->GetProperty, fill=value
  widget_control, id, set_button = value
  
  ; intialize color window
  *state.ctrace->GetProperty, symbol=symbolObj
  symbolObj->GetProperty, color=value
  Widget_Control, state.scolorWindow, Get_Value=scolorWin
  if obj_valid(scene) then scene->remove,/all
  scene=obj_new('IDLGRSCENE', color=value)
  scolorWin->draw, scene
  
  id = widget_info(tlb, find_by_uname = 'symbolstyle')
  *state.ctrace->GetProperty, symbol=symbolObj
  symbolObj->GetProperty, id=value
  widget_control, id, set_combobox_select = value-1
  
  id = widget_info(tlb, find_by_uname = 'symbolsize')
  *state.ctrace->GetProperty, symbol=symbolObj
  symbolObj->GetProperty, size=value
  widget_control, id, set_value = value
  
  
;****** Symbol Frequency ******************************************************
  *state.ctrace->GetProperty, PlotPoints=value
  CASE value OF
    0: BEGIN
      id = widget_info(tlb, find_by_uname = 'allpoints')
      Widget_Control, state.everyBase, sensitive=0
    END
    1: BEGIN
      id = widget_info(tlb, find_by_uname = 'firstlast')
      Widget_Control, state.everyBase, sensitive=0
    END
    2: BEGIN
      id = widget_info(tlb, find_by_uname = 'first')
      Widget_Control, state.everyBase, sensitive=0
    END
    3: BEGIN
      id = widget_info(tlb, find_by_uname = 'last')
      Widget_Control, state.everyBase, sensitive=0
    END
    4: BEGIN
      id = widget_info(tlb, find_by_uname = 'majorticks')
      Widget_Control, state.everyBase, sensitive=0
    END
    5: BEGIN
      id = widget_info(tlb, find_by_uname = 'everypoint')
      Widget_Control, state.everyBase, sensitive=1
    END
  ENDCASE
  widget_control, id, /set_button
  
  id = widget_info(tlb, find_by_uname = 'everyinc')
  *state.ctrace->GetProperty, everyother=value
  Widget_Control, id, set_value=value


;****** Lines Between Points **************************************************
  id = widget_info(tlb, find_by_uname = 'drawbetween')
  *state.ctrace->GetProperty, drawBetweenPts=value
  widget_control, id, set_button = value
  
  if value then begin
    Widget_Control, state.drawLabel, /sensitive
    Widget_Control, state.drawText, /sensitive
    Widget_Control, state.drawDroplist, /sensitive
  endif else begin
    Widget_Control, state.drawLabel, sensitive=0
    Widget_Control, state.drawText, sensitive=0
    Widget_Control, state.drawDroplist, sensitive=0
  endelse

  id = widget_info(tlb, find_by_uname = 'drawdroplist')
  *state.ctrace->GetProperty, SeparatedUnits=value
  if obj_valid(*state.cpanel) then *state.cpanel->GetProperty, xAxis=xaxis
  if obj_valid(xaxis) then units = xaxis->GetUnits() else units = ['<none>']
  widget_control, id, set_value=units
  widget_control, id, set_combobox_select = value
  id = widget_info(tlb, find_by_uname = 'drawtext')
  *state.ctrace->GetProperty, SeparatedBy=value
  widget_control, id, set_value = strcompress(string(value), /remove_all)
  
;****** Line Bar **************************************************************
;  id = widget_info(tlb, find_by_uname = 'negative')
;  *state.ctrace->GetProperty, negativeEndPt=value
;  value = where(state.dataNames eq value, n)
;  if n gt 0 then begin
;    widget_control, id, set_combobox_select=value
;  endif else begin
;    widget_control, id, set_combobox_select=0
;  endelse
  
;  id = widget_info(tlb, find_by_uname = 'positive')
;  *state.ctrace->GetProperty, positiveEndPt=value
;  value = where(state.dataNames eq value, n)
;  if n gt 0 then begin
;    widget_control, id, set_combobox_select=value
;  endif else begin
;    widget_control, id, set_combobox_select=0
;  endelse

;  id = widget_info(tlb, find_by_uname = 'relnegative')
;  *state.ctrace->GetProperty, negativeEndRel=value
;  widget_control, id, set_button = value
;  
;  id = widget_info(tlb, find_by_uname = 'relpositive')
;  *state.ctrace->GetProperty, positiveEndRel=value
;  widget_control, id, set_button = value
  
;  id = widget_info(tlb, find_by_uname = 'showbar')
;  *state.ctrace->GetProperty, BarLine=BarLineObj
;  BarLineObj->GetProperty, Show=value
;  widget_control, id, set_button = value

  
  
;****** Line Bar Line Style ***************************************************
;  id = widget_info(tlb, find_by_uname = 'lsthickness')
;  *state.ctrace->GetProperty, BarLine=lineStyleObj
;  lineStyleObj->GetProperty, Thickness=value
;  widget_control, id, set_value = value

  ; intialize color window
;  *state.ctrace->GetProperty, BarLine=lineStyleObj
;  lineStyleObj->GetProperty, Color=value
;  Widget_Control, state.lscolorWindow, Get_Value=lscolorWin
;  if obj_valid(scene) then scene->remove,/all
;  scene=obj_new('IDLGRSCENE', color=value)
;  lscolorWin->draw, scene
  

;****** Line Bar End Mark *****************************************************
;  id = widget_info(tlb, find_by_uname = 'emthickness')
;  *state.ctrace->GetProperty, MarkSymbol=lineStyleObj
;  lineStyleObj->GetProperty, Thickness=value
;  widget_control, id, set_value = value

  ; intialize color window
;  *state.ctrace->GetProperty, MarkSymbol=lineStyleObj
;  lineStyleObj->GetProperty, Color=value
;  Widget_Control, state.mcolorWindow, Get_Value=mcolorWin
;  if obj_valid(scene) then scene->remove,/all
;  scene=obj_new('IDLGRSCENE', color=value)
;  mcolorWin->draw, scene

;  *state.ctrace->GetProperty, BarLine=barline
;  IF Obj_Valid(BarLine) THEN BEGIN
;     barLine->GetProperty, Show=show
;     IF show EQ 1 THEN BEGIN
;       Widget_Control, state.endFrameBase, Sensitive=1
;       Widget_Control, state.styleFrameBase, Sensitive=1
;       Widget_Control, state.endMarkFrameBase, Sensitive=1
;     ENDIF ELSE BEGIN
;       Widget_Control, state.endFrameBase, Sensitive=0
;       Widget_Control, state.styleFrameBase, Sensitive=0
;       Widget_Control, state.endMarkFrameBase, Sensitive=0
;     ENDELSE   
;  ENDIF ELSE BEGIN
;    Widget_Control, state.endFrameBase, Sensitive=0
;    Widget_Control, state.styleFrameBase, Sensitive=0
;    Widget_Control, state.endMarkFrameBase, Sensitive=0
;  ENDELSE
  
  if ~statedef then Widget_Control, tlb, Set_UValue=state, /No_Copy   ;Only put STATE if it was not passed in.
end




PRO thm_ui_line_options_event, event

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
       /noname, /center, title='Error in Line Options')
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin 
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF

    ;kill request block

  IF (Tag_Names(event, /Structure_Name) EQ 'WIDGET_KILL_REQUEST') THEN BEGIN  

    ;Redraw:
    ;*******
    ;
    state.origWindow->GetProperty, panels=origPanels
    state.cWindow->SetProperty, panels=origPanels
    state.drawObject->update,state.windowStorage,state.loadedData
    state.drawObject->draw

    state.historyWin->Update,'THM_UI_LINE_OPTIONS: Active window refreshed.'
    state.statusbar->Update,'Active window refreshed.'

    Print, 'Line Options widget killed' 
    state.historyWin->Update,'THM_UI_LINE_OPTIONS: Widget killed' 
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    Widget_Control, event.top, /Destroy
    RETURN 

  ENDIF

   ;deal with tabs

  IF (Tag_Names(event, /Structure_Name) EQ 'WIDGET_TAB') THEN BEGIN  
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    RETURN 
  ENDIF

   ; Get the instructions from the Widget causing the event and
   ; act on them.

  Widget_Control, event.id, Get_UValue=uval


  IF Size(uval, /Type) NE 0 THEN BEGIN
  
  state.historyWin->Update,'THM_UI_LINE_OPTIONS: User value: '+uval,/dontshow
    
    CASE uval OF

    ;****** Line Bar **********************************************************
      'NEGATIVE': *state.ctrace->SetProperty, NegativeEndPt=state.dataNames[event.index]
      'POSITIVE': *state.ctrace->SetProperty, PositiveEndPt=state.dataNames[event.index]
      'RELNEGATIVE': *state.ctrace->SetProperty, NegativeEndRel=event.select
      'RELPOSITIVE': *state.ctrace->SetProperty, PositiveEndRel=event.select
      'SHOWBAR':  BEGIN
        result = Widget_Info(event.id, /Button_Set) 
        *state.ctrace->GetProperty, BarLine=BarLineObj
        IF result EQ 0 THEN BEGIN
           Widget_Control, state.endFrameBase, Sensitive=0
           Widget_Control, state.styleFrameBase, Sensitive=0
           Widget_Control, state.endMarkFrameBase, Sensitive=0
           BarLineObj->SetProperty, Show=0
        ENDIF ELSE BEGIN
           Widget_Control, state.endFrameBase, Sensitive=1
           Widget_Control, state.styleFrameBase, Sensitive=1
           Widget_Control, state.endMarkFrameBase, Sensitive=1
           BarLineObj->SetProperty, Show=1           
        ENDELSE
      END
    ;**************************************************************************
 
    ;****** Lines Between Points***********************************************
      'DRAWBETWEEN': BEGIN
         result = Widget_Info(event.id, /Button_Set)
         *state.ctrace->SetProperty, drawBetweenPts=result
         Widget_Control, state.drawLabel, sensitive=result
         Widget_Control, state.drawText, sensitive=result
         Widget_Control, state.drawDroplist, sensitive=result
         if result then state.statusBar->Update, 'Draw Between options activated.' $
           else state.statusBar->Update, 'Draw Between options de-activated.'
      END
      'DRAWDROPLIST': BEGIN
        if obj_valid(*state.cpanel) then *state.cpanel->GetProperty, xAxis=xaxis
        if obj_valid(xaxis) then units = xaxis->GetUnits()
        if N_Elements(units) gt 1 then value=event.index else value=4
        *state.ctrace->SetProperty, SeparatedUnits=value
        state.statusBar->Update, "Separated by units updated."
      END
      'DRAWTEXT': BEGIN
        if event.valid then begin
          IF event.value LT 0 THEN BEGIN
             state.statusBar->Update, 'Separated by values must be positive. Please re-enter'
             widget_control,event.id,set_value=0
          ENDIF ELSE BEGIN   
            *state.ctrace->SetProperty, SeparatedBy=event.value
             state.statusBar->Update, 'Separated By value updated.'
          ENDELSE
        endif else state.statusBar->update, 'Invalid Separated By valid, please re-enter.'
      END
      'SETALL': BEGIN
          widget_control, state.drawText, get_value=sepBy
          result = Widget_Info(state.drawButtonButton, /Button_Set)       
          *state.ctrace->GetProperty, SeparatedUnits=sepU
          if obj_valid((*state.ctraces)[0]) then begin
            ntr = n_elements(*state.ctraces)
            for i=0,ntr-1 do begin
              ctrace=(*state.ctraces)[i]
              ctrace->SetProperty, drawBetweenPts=result, SeparatedUnits=sepU, $
                                   SeparatedBy=sepBy
            endfor     
            state.statusBar->Update, 'Draw Between settings set for all traces in panel.'
          endif
      END
    ;**************************************************************************


    ;****** Line Options ******************************************************
      'LINESTYLE': BEGIN
        *state.ctrace->GetProperty, LineStyle=lineStyleObj
        styleNames = lineStyleObj->GetLineStyles()
        lineStyleObj->SetProperty, Id=event.index, Name=styleNames[event.index]
        state.statusBar->Update, 'Line Options: Style updated.'
      END
      'MIRROR': BEGIN
        *state.ctrace->SetProperty, MirrorLine=event.select
        if event.select then state.statusBar->Update, 'Line Options: Mirror turned on.' $
          else state.statusBar->Update, 'Line Options: Mirror turned off.'
      END
      'PALETTE': BEGIN
        *state.ctrace->GetProperty, LineStyle=lineStyleObj
        lineStyleObj->GetProperty, Color=currentcolor
        color = PickColor(!P.Color, Group_Leader=state.tlb, Cancel=cancelled, $
                          currentcolor=currentcolor)       
        if cancelled then begin
          color=currentcolor
          state.statusBar->Update, 'Line Options: Color unchanged.'
        endif else state.statusBar->Update, 'Line Options: Color updated.'        
        lineStyleObj->SetProperty, Color=color
        Widget_Control, state.lcolorWindow, Get_Value=lcolorWin
        if obj_valid(scene) then scene->remove,/all
        scene=obj_new('IDLGRSCENE', color=reform(color))
        lcolorWin->draw, scene  
        *state.cpanel->SyncLabelsToLines
      END
      'SHOWLINE': BEGIN
        *state.ctrace->GetProperty, LineStyle=lineStyleObj
        lineStyleObj->SetProperty, Show=event.select
        if event.select then state.statusBar->Update, 'Line Options: Show Line turned on.' $
          else state.statusBar->Update, 'Line Options: Show Line turned off.'
      END
;      'SYNC': BEGIN
;        *state.cpanel->SetProperty, SyncFlag=event.select
;        if event.select then state.statusBar->Update, 'Line Options: Sync Labels to Lines turned on.' $
;          else state.statusBar->Update, 'Line Options: Sync Labels to Lines turned off.'
;      END
      'THICKNESS': BEGIN
        if event.valid then begin
          IF event.value LT 0 THEN BEGIN
             state.statusBar->Update, 'Line thickness values must be positive. Please re-enter'
          ENDIF ELSE BEGIN   
            *state.ctrace->GetProperty, LineStyle=lineStyleObj
            lineStyleObj->SetProperty, Thickness=event.value
            state.prevThickness=event.value
            state.statusBar->Update, 'Line Options: Thickness updated.'
          ENDELSE
        endif else state.statusBar->update, 'Invalid thickness, please re-enter.'
      END
    ;**************************************************************************
    
    
    ;****** Line Bar End Mark *************************************************
      'EMTHICKNESS': BEGIN
        widget_control,event.id,get_value=value
        IF value LT 0 THEN BEGIN
           state.statusBar->Update, 'Line thickness values must be positive. Please re-enter'
           widget_control,event.id,set_value=''
        ENDIF ELSE BEGIN   
          *state.ctrace->GetProperty, BarLine=barline
          barline->SetProperty, Thickness=value
          state.prevemthick=value
          state.statusBar->Update, 'Line thickness value updated.'
        ENDELSE
      END
      'PALETTE3': BEGIN
        *state.ctrace->GetProperty, MarkSymbol=lineStyleObj
        lineStyleObj->GetProperty, Color=currentcolor
        color = PickColor(!P.Color, Group_Leader=state.tlb, Cancel=cancelled, $
                          currentcolor=currentcolor)        
        if cancelled then color=currentcolor                          
        lineStyleObj->SetProperty, Color=color        
        Widget_Control, state.mcolorWindow, Get_Value=mcolorWin
        if obj_valid(scene) then scene->remove,/all
        scene=obj_new('IDLGRSCENE', color=reform(color))
        mcolorWin->draw, scene
        *state.cpanel->GetProperty, SyncFlag=syncflag
        IF syncflag EQ 1 THEN *state.cpanel->SyncLabelsToLines
      END
    ;**************************************************************************


    ;****** Line Bar Line Style ***********************************************
      'LSTHICKNESS': BEGIN
       IF event.value LT 0 THEN BEGIN
           state.statusBar->Update, 'Line thickness values must be positive. Please re-enter'
           widget_control,event.id,set_value=''
        ENDIF ELSE BEGIN   
          *state.ctrace->GetProperty, BarLine=lineStyleObj
          lineStyleObj->SetProperty, Thickness=event.value
          state.statusBar->Update, 'Line thickness value updated.'
          prevlsthick=event.value 
        ENDELSE
      END
      'PALETTE2': BEGIN
        *state.ctrace->GetProperty, BarLine=lineStyleObj
        lineStyleObj->GetProperty, Color=currentcolor
        color = PickColor(!P.Color, Group_Leader=state.tlb, Cancel=cancelled, $
                          currentcolor=currentcolor)        
        if cancelled then color=currentcolor                          
        lineStyleObj->SetProperty, Color=color        
        Widget_Control, state.lscolorWindow, Get_Value=lscolorWin
        if obj_valid(scene) then scene->remove,/all
        scene=obj_new('IDLGRSCENE', color=reform(color))
        lscolorWin->draw, scene
      END

    ;**************************************************************************


    ;****** Symbol Options ****************************************************
      'FILLSYMBOL': BEGIN
        *state.ctrace->GetProperty, symbol=symbolObj
        symbolObj->SetProperty, fill=event.select
        if event.select then state.statusBar->Update, 'Symbol Options: Filled turned on.' $
          else state.statusBar->Update, 'Symbol Options: Filled turned off.'
      END
      'PALETTE1': BEGIN
        *state.ctrace->GetProperty, Symbol=symbolObj
        symbolObj->GetProperty, Color=currentcolor
        color = PickColor(!P.Color, Group_Leader=state.tlb, Cancel=cancelled, $
                          currentcolor=currentcolor)        
        if cancelled then begin
          color=currentcolor
          state.statusBar->Update, 'Symbol Options: Color unchanged.'
        endif else state.statusBar->Update, 'Symbol Options: Color updated.'
                                  
        symbolObj->SetProperty, Color=color        
        Widget_Control, state.scolorWindow, Get_Value=scolorWin
        if obj_valid(scene) then scene->remove,/all
        scene=obj_new('IDLGRSCENE', color=reform(color))
        scolorWin->draw, scene
      END 
      'SHOWSYMBOL': BEGIN
        *state.ctrace->GetProperty, symbol=symbolObj
        symbolObj->SetProperty, show=event.select
        if event.select then state.statusBar->Update, 'Symbol Options: Show Symbol turned on.' $
          else state.statusBar->Update, 'Symbol Options: Show Symbol turned off.'
      END
      'SYMBOLSIZE': BEGIN
        if event.valid then begin
          IF event.value LT 0 THEN BEGIN
             state.statusBar->Update, 'Size values must be positive. Please re-enter'
             widget_control,event.id,set_value=''
          ENDIF ELSE BEGIN   
            *state.ctrace->GetProperty, symbol=symbolObj
            IF Obj_Valid(symbolObj) THEN symbolObj->SetProperty, Size=event.value
            prevSymbolSize=event.value
            state.statusBar->Update, 'Symbol Options: Size updated.'
          ENDELSE
        endif else state.statusBar->update, 'Invalid symbol size, please re-enter.'     
      END
      'SYMBOLSTYLE': BEGIN
        *state.ctrace->GetProperty, symbol=symbolObj
        styleNames = symbolObj->GetSymbols()
        symbolObj->SetProperty, Id=event.index+1, Name=styleNames[event.index]
        state.statusBar->Update, 'Symbol Options: Style updated.'
      END
    ;**************************************************************************


    ;****** Symbol Frequency **************************************************
      'ALLPOINTS': BEGIN
        *state.ctrace->SetProperty, PlotPoints=0
        Widget_Control, state.everyBase, sensitive=0
        state.statusBar->Update, 'Symbol Frequency: All points selected.'
      END
      'FIRSTLAST': BEGIN
        *state.ctrace->SetProperty, PlotPoints=1
        Widget_Control, state.everyBase, sensitive=0
        state.statusBar->Update, 'Symbol Frequency: First and last point selected.'
      END
      'FIRST': BEGIN
        *state.ctrace->SetProperty, PlotPoints=2
        Widget_Control, state.everyBase, sensitive=0
        state.statusBar->Update, 'Symbol Frequency: First point selected.'
      END
      'LAST': BEGIN
        *state.ctrace->SetProperty, PlotPoints=3
        Widget_Control, state.everyBase, sensitive=0
        state.statusBar->Update, 'Symbol Frequency: Last point selected.'
      END
      'MAJORTICKS': BEGIN
        *state.ctrace->SetProperty, PlotPoints=4
        Widget_Control, state.everyBase, sensitive=0
        state.statusBar->Update, 'Symbol Frequency: Major ticks selected.'
      END
      'EVERYINC': BEGIN
        if event.valid then begin
          IF event.value LT 0 THEN BEGIN
             state.statusBar->Update, 'Symbol Frequency: Every point values must be positive. Please re-enter.'
             widget_control,event.id,set_value='1'
          ENDIF ELSE BEGIN   
            *state.ctrace->SetProperty, EveryOther=event.value
             state.statusBar->Update, 'Symbol Frequency: Every point value updated.'
             prevInc=event.value
          ENDELSE
        endif else state.statusBar->update, 'Invalid symbol frequency, please re-enter.'     
      END
      'EVERYPOINT': BEGIN
        *state.ctrace->SetProperty, PlotPoints=5
        Widget_Control, state.everyBase, sensitive=1
        state.statusBar->Update, 'Symbol Frequency: Every point selected.'
      END
    ;**************************************************************************
    
    
      'APPLY': BEGIN

      	;Sync labels:      	;
;      	*state.cpanel->GetProperty, SyncFlag=syncflag
        if ptr_valid(state.cpanel) && obj_valid(*state.cpanel) then begin
      	  *state.cpanel->SyncLabelsToLines
      	endif
      	
        state.drawObject->update,state.windowStorage,state.loadedData
        state.drawObject->draw
        state.historyWin->Update, 'THM_UI_LINE_OPTIONS: Changes applied.'     
        state.statusBar->Update, 'Changes applied.'
      END
      'CANC': BEGIN
        state.origWindow->GetProperty, panels=origPanels
        state.cWindow->SetProperty, panels=origPanels
        state.drawObject->update,state.windowStorage,state.loadedData
        state.drawObject->draw
        Print, 'Line Options widget cancelled. No changes made.'
        state.historyWin->Update, 'THM_UI_LINE_OPTIONS: Line Options window cancelled. No changes made.'
        Widget_Control, event.TOP, Set_UValue=state, /No_Copy
        Widget_Control, event.top, /destroy
        RETURN
      END
      'OK': BEGIN
        state.drawObject->update,state.windowStorage,state.loadedData
        state.drawObject->draw  
        Print, 'Line options update. Line Options widget closed.'
        state.historyWin->Update, 'THM_UI_LINE_OPTIONS: Line options update. Line Options widget closed.'
        Widget_Control, event.TOP, Set_UValue=state, /No_Copy
        Widget_Control, event.top, /destroy
        RETURN
      END
      'TEMP': begin
         
        if ptr_valid(state.ctrace) && obj_valid(*state.ctrace) then begin    
          lsettings = (*state.ctrace)->copy()
          lsettings->setProperty,dataX='',dataY=''
          state.template->setProperty,line=lsettings
          state.historywin->update,'Current Line Settings Saved to Template'
          state.statusBar->update,'Current Line Settings Saved to Template'   
        endif else begin
          state.historywin->update,'No Traces Available, Could Not be Saved to Template'
          state.statusBar->update,'No Traces Available, Count Not be Saved to Template'   
        endelse
      end 
      'PANELLIST': BEGIN
        pindex = widget_info(state.panelDroplist, /combobox_gettext)
        widget_control, state.panelDroplist, get_value = pindexval
        pindex = where(pindexval eq pindex)
        
        if ptr_valid(state.panelObjs) && n_elements(*state.panelObjs) ge pindex && obj_valid((*state.panelObjs)[pindex]) then begin
          cpanel = (*state.panelObjs)[pindex]
          
          cpanel->getProperty,traceSettings=traceSettings, XAxis=xaxis
          traces = traceSettings->get(/all)
        
          if obj_valid(traces[0]) then begin
            trNames = cPanel->constructTraceNames()
            
            traces[0]->getProperty, dataX=dataX, dataY=dataY
            ctrace = traces[0]
  
            if obj_isa(traces[0],'thm_ui_spectra_settings') then begin
              
              traces[0]->getProperty,dataZ=dataZ
            endif else begin
            
              state.xaxisTree->Update, selected=dataX
              state.yaxisTree->Update, selected=dataY
                    
              dataZ = ''
            endelse
    
          endif else trNames = 'No traces'
  
          
          widget_control, state.traceDroplist, set_value = trNames
          widget_control, state.traceDroplist, set_combobox_select = 0
          
          *state.cpanel_num = pindex
          *state.ctr_num = 0
          
          if ptr_valid(state.cpanel) then ptr_free, state.cpanel
          state.cpanel = ptr_new(cpanel)
          if ptr_valid(state.ctraces) then ptr_free, state.ctraces
          state.ctraces = ptr_new(traces)
          if ptr_valid(state.ctrace) then ptr_free, state.ctrace
          state.ctrace = ptr_new(ctrace)
          
          thm_ui_init_line_panel, state=state

          state.statusBar->Update, 'Panel selection changed.'
        endif
        
      END
      'TRACELIST': BEGIN
        pindex = widget_info(state.traceDropList, /combobox_gettext)
        widget_control, state.tracedroplist, get_value = pindexval
        pindex = where(pindexval eq pindex)

        ;in case combobox has identical entries
        if n_elements(pindex) gt 1 then begin
          pindex = event.index
        endif

        if obj_valid((*state.ctraces)[pindex]) then begin
          
          (*state.ctraces)[pindex]->getProperty, dataX=dataX, dataY=dataY
          ctrace = (*state.ctraces)[pindex]
  
          if obj_isa((*state.ctraces)[pindex],'thm_ui_spectra_settings') then begin
            
            (*state.ctraces)[pindex]->getProperty,dataZ=dataZ
          endif else begin
          
            state.xaxisTree->Update, selected=dataX
            state.yaxisTree->Update, selected=dataY
                  
            dataZ = ''
          endelse
  
        endif
        
        *state.ctr_num = pindex

        if ptr_valid(state.ctrace) then ptr_free, state.ctrace
        state.ctrace = ptr_new(ctrace)
        
        thm_ui_init_line_panel, state=state
        
        state.statusBar->Update, 'Trace selection changed.'
        
      END
     'XAXISTREE': BEGIN

       widget_control, event.id, get_value=value
       xName = value->getValue()
       
       if is_string(xName[0]) then begin
         *state.ctrace->SetProperty, dataX=xName

         ; rebuild tracenames based on droplist selection
         trNames = *state.cPanel->constructTraceNames()
         widget_control, state.traceDroplist, set_value = trNames
         thm_ui_init_line_panel, state=state      

         ;Sync labels:
         *state.cpanel->SyncLabelsToLines
       
         state.statusBar->Update, 'Selection made from X-Axis tree.'
       endif 
     END
     'YAXISTREE': BEGIN

       widget_control, event.id, get_value=value
       yName = value->getValue()
       if is_string(yName[0]) then begin
         ;get old names for comparison
         oldTrNames = *state.cPanel->constructtracenames()

         *state.ctrace->SetProperty, dataY=yName
  
         ; rebuild tracenames based on droplist selection
         trNames = *state.cPanel->constructTraceNames()
         widget_control, state.traceDroplist, set_value = trNames
         idx = where(oldTrNames ne trNames,c)
         if c ne 0 then widget_control, state.traceDroplist, set_combobox_select=idx
         thm_ui_init_line_panel, state=state      
  
         ;Sync labels:
         *state.cpanel->SyncLabelsToLines
         
         state.statusBar->Update, 'Selection made from Y-Axis tree.'
       endif 
     END
      ELSE: print, ''
    ENDCASE
  ENDIF
  
  Widget_Control, event.TOP, Set_UValue=state, /No_Copy
  
  RETURN
END ;--------------------------------------------------------------------------------



PRO thm_ui_line_options, gui_id, windowStorage, loadedData, historyWin, $
                          drawObject, template, cpanel_num=cpanel_num, ctr_num=ctr_num

  err_xxx = 0
  Catch, err_xxx
  IF(err_xxx Ne 0) THEN BEGIN
    Catch, /Cancel
    Help, /Last_Message, Output=err_msg
    FOR j = 0, N_Elements(err_msg)-1 DO Begin
      print, err_msg[j]
      If(obj_valid(historywin)) Then historyWin -> update, err_msg[j]
    Endfor
    Print, 'Error--See history'
    ok = error_message('An unknown error occured starting line options. See console for details.',$
         /noname, /center, title='Error in Line Options')
    thm_gui_error, gui_id, historywin
    RETURN
  ENDIF

    ;build top level and main tab bases
    
  tlb = Widget_Base(/Col, title='THEMIS: Line Options ', Group_Leader=gui_id, $
                    /modal, /Floating, /TLB_KILL_REQUEST_EVENTS)
  mainBase = Widget_Base(tlb, /Col, /Align_Center)
  buttonStatusBase = Widget_Base(tlb, /Col, /Align_center)
  panelBase=Widget_Base(mainBase, /Col)
  lineBase = Widget_Base(mainBase, /Col)
  
    ; Line Tab Bases
    
  mainLineBase = Widget_Base(lineBase, /Col)
    dummy_base = Widget_Base(MainLineBase, /row)
    subMainLineBase = Widget_Base(Dummy_Base, /col, frame=3)
    traceBase = Widget_Base(submainlineBase, /col)
    subMainLineBase = widget_base(submainlinebase, /col, space=0)
;    SubMainLineBaseRight = Widget_Base(Dummy_Base, /col) ;jmm, 25-feb-2009
    axisBase = Widget_Base(subMainLineBase, /Row)
    xaxisBase = Widget_Base(axisBase, /Col)
    yaxisBase = Widget_Base(axisBase, /Col)
;    main1Base = Widget_Base(SubMainLineBase, /Row)
      col1Base1 = Widget_Base(submainlineBase, /Col)   
        optionsBase = Widget_Base(col1Base1, /Row) 
          c1Base = Widget_Base(optionsBase, /col)
            r1c1Base = Widget_Base(c1Base, /row)
            r2c1Base = Widget_Base(c1Base, /row)     
          c2Base = Widget_Base(optionsBase, /Row)       
          lineOptionsBase = Widget_Base(r1c1Base, /Col)
          symbolOptionsBase = Widget_Base(r1c1Base, /Col) 
          frequencyBase = Widget_Base(c2Base, /Col) 
;        plotOptionsBase = Widget_Base(col1Base1, /Col)
        drawBase = Widget_Base(r2c1Base, /Col, frame = 3)                            
        ;setallBase = Widget_Base(mainlinebase, /row, /align_center)
;    main2base = Widget_base(SubMainLineBaseRight, /Col)
;      col1Base2 = Widget_Base(main2Base, /Col)   
;        drawBase = Widget_Base(col1Base2, /Col)                            
;        barsBase = Widget_Base(col1Base2, /Col) 
;          showBarBase = Widget_Base(barsBase, /Row)
;          endBarsBase = Widget_Base(barsBase, /Col) 
;      col2Base2 = Widget_Base(main2Base, /Col)
;        styleLineBase = Widget_Base(col2Base2, /Col)                            
;        endMarkBase = Widget_Base(col2Base2, /Col)
 
  
  if ~ptr_valid(cpanel_num) then cpanel_num = ptr_new(0) 
  if ~ptr_valid(ctr_num) then ctr_num = ptr_new(0)
  
  ;retrieve data and panel info for display
  dataNames = loadedData->GetAll(/child)
  IF is_num(dataNames) THEN dataNames=''
  cWindow = windowStorage->GetActive()
  origWindow = cWindow->Copy()
  IF NOT Obj_Valid(cWindow) THEN BEGIN
     panelNames=[''] 
  ENDIF ELSE BEGIN
     cWindow->GetProperty, Panels=panels, nRows=nRows, nCols=nCols
;     cwindow->GetProperty, variables=variablescontainer
;     variablesobjects = variablescontainer->get(/all)
     IF NOT Obj_Valid(panels) THEN panelObjs=[''] ELSE panelObjs = panels->Get(/All)
  ENDELSE
 
  ; initialize drawUnits to none, if panel and axes are valid objects then units
  ; will be set later
  drawUnits = ['<none>']

  if obj_valid(panelObjs[0]) then begin
  
    npanels = n_elements(panelObjs)
    panelNames = strarr(npanels)
    panelTitles = panelNames
  
    for i = 0,npanels-1 do begin ; loop over panels
    
      cPanel = panelObjs[i]
      panelNames[i] = cPanel->constructPanelName()
      if i eq 0 then panelValue = cPanel->constructPanelName() $
        else panelValue = [panelValue, cPanel->constructPanelName()]
  
      if i eq 0 then panelLayout = cPanel->getLayoutStructure() $
        else panelLayout = [panelLayout, cPanel->getLayoutStructure()]
      
      if i eq 0 then panelValueInfo = {panelListInfo, ispanel:1, istrace:0, $
                                       panelid:panelLayout[i].id, traceid:-1} $
        else panelValueInfo = [panelValueInfo, {panelListInfo, ispanel:1, istrace:0, $
                                                panelid:panelLayout[i].id, traceid:-1}]
      
      cPanel->getProperty,traceSettings=traceSettings
      traces = traceSettings->get(/all)
      
      if i eq *cpanel_num then ctraces = traces
      
      if obj_valid(traces[0]) then begin
        ntr = n_elements(traces)
        trNames = cPanel->constructTraceNames()
            
        for j = 0,ntr-1 do begin
    
          panelValue = [panelValue, trNames[j]]
          
          panelValueInfo = [panelValueInfo, {panelListInfo, ispanel:0, istrace:1, $
                                             panelid:panelLayout[i].id, traceid:j}]
          
          if (*cpanel_num eq i) AND (*ctr_num eq j) then begin
            traces[j]->getProperty,dataX=dataX, dataY=dataY
            ctrace = traces[j]

            if obj_isa(traces[j],'thm_ui_spectra_settings') then begin
              traces[j]->getProperty,dataZ=dataZ
              
              ;if spectra was selected, check for valid traces
              for k = 0, ntr-1 do begin
                if obj_isa(traces[k],'thm_ui_line_settings') then begin
                  traces[j]->getProperty,dataX=dataX, dataY=dataY
                  dataZ = ''
                  ctrace = traces[k]
                  *ctr_num = k
                  break
                endif
              endfor              
               
            endif else dataZ = ''
            
          endif 
        endfor

      endif
    endfor
  
  endif else begin
    npanels=0
    panelNames='No Panels'
  endelse

  if obj_valid(panelObjs[0]) then cpanel = panelObjs[*cpanel_num] 

  ctextvalues=[''] 
      ;widgets for line panel
  
  paneldbase = widget_base(panelBase, /row)
  paneldlabel = widget_label(paneldBase, value = '   Panel: ')
  panelDroplist = Widget_combobox(paneldBase, XSize=400, $
                                  Value=panelNames, UValue='PANELLIST')
  widget_control, panelDroplist, set_combobox_select=*cpanel_num
  
  if is_struct(panelValueInfo) then begin
  
    tr_nums = where((panelValueInfo.panelid eq *cpanel_num) $
                      and (panelValueInfo.istrace eq 1), ntr_nums)
                      
  endif else begin
    tr_nums = 0
    ntr_nums = 0
;    panelObjs = obj_new('thm_ui_panel')
;    cTrace = obj_new('thm_ui_line_settings')
;    cTraces = [cTrace]
  endelse
    
  
  if ntr_nums gt 0 then traceNames=panelValue[tr_nums] else traceNames='No Traces'
  
  tracedbase = widget_base(tracebase, /row)
  tracedlabel = widget_label(tracedBase, value = 'Select Trace: ')
  traceDroplist = Widget_combobox(tracedBase, XSize=400, $
                                  Value=traceNames, UValue='TRACELIST')
  widget_control, traceDroplist, set_combobox_select=*ctr_num
      
  IF Is_Num(dataNames) OR dataNames[0] EQ '' THEN dataNames=['No Loaded Data']
  
  xaxisLabel = Widget_Label(xaxisBase, Value='Edit X-Axis Variable ', /Align_Center)
  xaxisTree = obj_new('thm_ui_widget_tree',xaxisBase,'XAXISTREE',loadedData,XSize=250, $
                      YSize=150,mode=1,multi=0,leafonly=1, uname='xaxistree',/showdatetime)
  xaxisTree->Update, selected=dataX

  yaxisLabel = Widget_Label(yaxisBase, Value='Edit Y-Axis Variable ', /Align_Center)
  yaxisTree = obj_new('thm_ui_widget_tree',yaxisBase,'YAXISTREE',loadedData,XSize=250, $
                      YSize=150,mode=1,multi=0,leafonly=1, uname='yaxistree',/showdatetime)
  yaxisTree->Update, selected=dataY

  
  
;****** Line Options **********************************************************
  lineOptionsLabel = Widget_Label(lineOptionsBase, Value='Line Options: ', /Align_Left)
  lineFrameBase = Widget_Base(lineOptionsBase, /Col, Frame=3)
  lineShowBase = Widget_Base(lineFrameBase, /NonExclusive, /row)
  lineShowButton = Widget_Button(lineShowBase, Value='Show Line', UValue='SHOWLINE', uname='showline')
  lineMirrorButton = Widget_Button(lineShowBase, Value='Mirror', UValue='MIRROR', uname='mirror')
;  lineSyncBase = Widget_Base(lineFrameBase, /nonexclusive, /row)
;  lineSyncButton = Widget_Button(lineSyncBase, Value='Set All Labels Black', UValue='BLACK', uname='black')
  lpaletteBase = Widget_Base(lineFrameBase, /Row, XPad=1)
  lcolorLabel = Widget_Label(lpaletteBase, Value='Color:', xsize = 70, /align_left)
  
  getresourcepath,rpath
  palettebmp = read_bmp(rpath + 'color.bmp', /rgb)
  thm_ui_match_background, tlb, palettebmp
  
  lpaletteButton = Widget_Button(lpaletteBase, Value=palettebmp, /Bitmap, $
    UValue='PALETTE', UName='palette', Tooltip='Choose color from Palette')
  lspaceLabel = Widget_Label(lpaletteBase, Value=' ')
  lcolorbase = widget_base(lpalettebase, /col)
;  ccolorlabel = Widget_Label(lcolorBase, Value = 'Currently')
  lcolorWindow = WIDGET_DRAW(lcolorBase,graphics_level=2,renderer=1, $
                             retain=1, XSize=50, YSize=19, units=0, frame=1, /expose_events)
                             
  if obj_valid(ctrace) && obj_isa(ctrace,'thm_ui_line_style') then begin
    ctrace->GetProperty, LineStyle=linestyleobj
  endif else begin
    linestyleobj = obj_new('thm_ui_line_style')
  endelse
  styleNames=linestyleobj->GetLineStyles()
  lsdBase = widget_base(lineFrameBase, /row)
  lsdLabel = widget_label(lsdBase, value = 'Style:', xsize = 70, /align_left)
  linestyleDroplist = Widget_combobox(lsdBase, $
    Value=styleNames, UValue='LINESTYLE', UName='linestyle')
  lineThickBase = Widget_Base(lineFrameBase, /Row)
  lthicklabel = widget_label(lineThickBase, value = 'Thickness:', xsize = 70, /align_left)
  lineThickIncrement = thm_ui_spinner(lineThickBase, $
    Increment=1, Value=1, UValue='THICKNESS', UName='thickness')
  prevThickness=1
;******************************************************************************


;****** Symbol Options ********************************************************
  symbolOptionsLabel = Widget_Label(symbolOptionsBase, Value='Symbol Options: ', $
    /Align_Left)
  symbolFrameBase = Widget_Base(symbolOptionsBase, /Row, Frame=3)
  symbol1Base = Widget_Base(symbolFrameBase, /Col)
  symbolShowBase = Widget_Base(symbol1Base, /Nonexclusive, /row)
  symbolShowButton = Widget_Button(symbolShowBase, Value='Show Symbol', $
                       UValue='SHOWSYMBOL', UName='showsymbol')
  symbolFilledButton = Widget_Button(symbolShowBase, Value='Filled', $
                         UValue='FILLSYMBOL', UName='fillsymbol', sensitive=0)
  spaletteBase = Widget_Base(symbol1Base, /Row, XPad=1)
  scolorLabel = Widget_Label(spaletteBase, Value='Color:', xsize = 50, /align_left)
;  palettebmp = filepath('palette.bmp', Subdir=['resource', 'bitmaps'])
  spaletteButton = Widget_Button(spaletteBase, Value=palettebmp, /Bitmap, $
    UValue='PALETTE1', Tooltip='Choose color from Palette')
  sspaceLabel = Widget_Label(spaletteBase, Value=' ')
  scolorbase = widget_base(spalettebase, /col)
;  ccolorlabel = Widget_Label(scolorBase, Value = 'Currently')
  scolorWindow = WIDGET_DRAW(scolorBase,graphics_level=2,renderer=1, $
                             retain=1, XSize=50, YSize=19, units=0, frame=1, /expose_events)
  if obj_valid(ctrace) && obj_isa(ctrace,'thm_ui_line_style') then begin
    ctrace->GetProperty, Symbol=symbolobj
  endif else begin
    symbolobj = obj_new('thm_ui_symbol')
  endelse
  
  symbolnames = symbolobj->GetSymbols()
  ssdBase = widget_base(symbol1Base, /row)
  ssdLabel = widget_label(ssdBase, value = 'Style:', xsize = 50, /align_left)
  symbolStyleDroplist = Widget_combobox(ssdBase, $
    Value=symbolNames, UValue='SYMBOLSTYLE', UName='symbolstyle')
  symbolThickBase = Widget_Base(symbol1Base, /Row)
  sthicklabel = widget_label(symbolThickBase, value = 'Size:', xsize = 50, /align_left)
  symbolThickIncrement = thm_ui_spinner(symbolThickBase, $
    Increment=1, Value=2, UValue='SYMBOLSIZE', UName='symbolsize')
  prevSymbolSize=2
;******************************************************************************

    
;****** Symbol Frequency ******************************************************
  plotOptionLabel = Widget_Label(frequencyBase, Value='Symbol Frequency:', /Align_Left)
  plotFrameBase = Widget_Base(frequencyBase, /col, Frame=3, XPad=5, ypad=6)
  plot1Base = Widget_Base(plotFrameBase, /Exclusive, /COL)
  allPtsButton = Widget_Button(plot1Base, Value = 'All', UValue='ALLPOINTS', $
                   UName='allpoints')
  firstLastButton = Widget_Button(plot1Base, Value = 'First, Last', $ 
                      UValue='FIRSTLAST', UName='firstlast')
  firstButton = Widget_Button(plot1Base, Value = 'First', UValue='FIRST', $
                  UName='first')
  lastButton = Widget_Button(plot1Base, Value = 'Last', UValue='LAST', $
                 UName='last')
  majorButton = Widget_Button(plot1Base, Value = 'Major ticks', UValue='MAJORTICKS', $
                  UName='majorticks')
  everyButton = Widget_Button(plot1Base, Value = 'Every', UValue='EVERYPOINT', $
                  UName='everypoint')
  Widget_Control, allPtsButton, /Set_Button
  everyBase = Widget_Base(plotFrameBase, /Row, Sensitive=0)
  everyIncrement = thm_ui_spinner(everyBase, label= '  ',increment=1, Value=1, $
                     UValue='EVERYINC', UName='everyinc')
  prevInc = 1
;******************************************************************************


;****** Lines Between Points **************************************************
  drawButtonBase = Widget_Base(drawBase, /Col, /NonExclusive)
  drawButtonButton = Widget_Button(drawButtonBase, UValue='DRAWBETWEEN', $
    UName='drawbetween', Value='Do not draw lines between points if')
  drawFrameBase = Widget_Base(drawBase, /Row, XPad=1)
  drawLabel = Widget_Label(drawFrameBase, Value='separated by more than ',sensitive=0)
;  drawText = Widget_TEXT(drawFrameBase, Value=' 0', YSize=1, XSize=5, $
;    UVALUE='DRAWTEXT', UName='drawtext', sensitive=0, /editable, /All_events)
  drawText = thm_ui_spinner(drawFrameBase, increment=1, Value=0, $
    sensitive=0, UVALUE='DRAWTEXT', UName='drawtext')
  ; get units for droplist     
  separatedUnits = ['<none>']
  if obj_valid(cpanel) then begin
      cPanel->GetProperty, XAxis=xaxis
      if obj_valid(xAxis) then separatedUnits=xAxis->GetUnits()  
  endif   
  drawDroplist = Widget_combobox(drawFrameBase, sensitive=0, Uvalue='DRAWDROPLIST', $
    UName='drawdroplist', Value=separatedUnits)
;    UName='drawdroplist', Value=[' seconds', ' minutes', ' hours', ' days', '<none>'])
;  spaceLabel = Widget_Label(drawFrameBase, Value=' ')
  drawSetBase = Widget_Base(drawBase, /col)
  drawSetButton = Widget_Button(drawBase, Value='Set All Lines',UValue='SETALL', XSize=95, /align_center)
;******************************************************************************
  
  
;****** Line Bar **************************************************************
; barsLabel = Widget_Label(barsBase, Value='Bar: ', /Align_Left, sensitive=0)
;  barsFrameBase = Widget_Base(barsBase, /Col, Frame=3, XPad=8)
;  showLabelBase = Widget_Base(barsFrameBase, /Row, /NonExclusive)
;  showLabelButton = Widget_Button(showLabelBase, Value='Show bar or line', UValue='SHOWBAR', $
;                                  UName='showbar', sensitive=0)
;  endLabel = Widget_Label(barsFrameBase, Value='End Points', /Align_Left, sensitive=0)
;  endFrameBase =  Widget_Base(barsFrameBase, /Col, Frame=3, XPad=2, sensitive=0)
;  positiveBase = Widget_Base(endFrameBase, /Row)
;  negativeBase = Widget_Base(endFrameBase, /Row)
;  positiveLabel = widget_label(positiveBase, value = 'Positive: ') 
;  positiveDroplist = Widget_combobox(positiveBase, $
;    Value=dataNames, UValue='POSITIVE', UName='positive')
;  posRelBase = Widget_Base(positiveBase, /NonExclusive)
;  posRelButton = Widget_Button(posRelBase, Value='Relative to trace', UValue='RELPOSITIVE', $
;                               UName='relpositive')
;  negativeLabel = widget_label(negativeBase, value = 'Negative: ')
;  negativeDroplist = Widget_combobox(negativeBase, $
;    Value=dataNames, UValue='NEGATIVE', Uname='negative')
;  negRelBase = Widget_Base(negativeBase, /NonExclusive)
;  negRelButton = Widget_Button(negRelBase, Value='Relative to trace', UValue='RELNEGATIVE', $
;                               UName='relnegative')
;;******************************************************************************


;****** Line Bar Line Style ***************************************************
;  styleLineLabel=Widget_Label(styleLineBase, Value='Line Style:', /Align_Left, sensitive=0)
;  styleFrameBase = Widget_Base(styleLineBase, /Col, Frame=3,sensitive=0)
;  lspaletteBase = Widget_Base(styleFrameBase, /Row, XPad=1)
;  lscolorLabel = Widget_Label(lspaletteBase, Value='Color:', /align_left, xsize = 70)
;  palettebmp = filepath('palette.bmp', Subdir=['resource', 'bitmaps'])
;  lspaletteButton = Widget_Button(lspaletteBase, Value=palettebmp, /Bitmap, $
;    UValue='PALETTE2', Tooltip='Choose color from Palette')
;  lsspaceLabel = Widget_Label(lspaletteBase, Value=' ')
;  lscolorbase = widget_base(lspalettebase, /col)
;  lccolorlabel = Widget_Label(lscolorBase, Value = 'Currently')
;  lscolorWindow = WIDGET_DRAW(lscolorBase,graphics_level=2,renderer=1, $
;                              retain=1, XSize=50, YSize=19, units=0, frame=1)
;  styleThickBase = Widget_Base(styleFrameBase, /Row)
;  lthicklabel = widget_label(styleThickBase, value = 'Thickness:', xsize = 70, /align_left)
;  styleThickIncrement = thm_ui_spinner(styleThickBase, $
;    Increment=1, Value=1, UValue='LSTHICKNESS', UName='lsthickness')
;  prevlsthick=1
;******************************************************************************
    

;****** Line Bar End Mark *****************************************************
;  endMarkLabel=Widget_Label(endMarkBase, Value='End Mark:', /Align_Left, sensitive=0)
;  endMarkFrameBase = Widget_Base(endMarkBase, /Col, Frame=3, sensitive=0)
;  mpaletteBase = Widget_Base(endMarkFrameBase, /Row, XPad=1)
;  mcolorLabel = Widget_Label(mpaletteBase, Value='Color:', /align_left, xsize = 70)
;;  palettebmp = filepath('palette.bmp', Subdir=['resource', 'bitmaps'])
;  mpaletteButton = Widget_Button(mpaletteBase, Value=palettebmp, /Bitmap, $
;    UValue='PALETTE3', Tooltip='Choose color from Palette')
;  mspaceLabel = Widget_Label(mpaletteBase, Value=' ')
;  mscolorbase = widget_base(mpalettebase, /col)
;;  mccolorlabel = Widget_Label(mscolorBase, Value = 'Currently')
;  mcolorWindow = WIDGET_DRAW(mscolorBase,graphics_level=2,renderer=1, $
;                             retain=1, XSize=50, YSize=19, units=0, frame=1)
;  endMarkthickBase = Widget_Base(endMarkFrameBase, /Row)
;  mthicklabel = widget_label(endMarkThickBase, value = 'Thickness:', xsize = 70, /align_left)
;  endMarkthickIncrement = thm_ui_spinner(endMarkthickBase, $
;    Increment=1, Value=1, UValue='EMTHICKNESS', UName='emthickness')
;  prevemthick=1
;******************************************************************************

  buttonsBase = Widget_Base(buttonStatusBase, /row, /align_center)
  okButton = Widget_Button(buttonsBase, Value='OK', UValue='OK', XSize=75)
  applyButton = Widget_Button(buttonsBase, Value='Apply', UValue='APPLY', XSize=75)
  cancelButton = Widget_Button(buttonsBase, Value='Cancel', UValue='CANC', XSize=75)
  templateButton = Widget_Button(buttonsBase, Value='Save to Template', UValue='TEMP',xsize=125)
  
  statusBase = Widget_Base(buttonStatusBase, /row)
  statusBar = Obj_New('THM_UI_MESSAGE_BAR', statusBase, XSize=75, YSize=1)
;  blank_label_for_space = widget_label(statusBase, Value = '      ')

   
  state = {tlb:tlb, gui_id:gui_id, winID:0, xSelect:0, ySelect:0, $;endMarkFrameBase:endMarkFrameBase, $
    lpaletteBase:lpaletteBase, statusBar:statusBar, $;$endFrameBase:EndFrameBase, $
    lcolorWindow:lcolorWindow, scolorWindow:scolorWindow, $
    ;lscolorWindow:lscolorWindow, mcolorWindow:mcolorWindow, $
    ;mcolorWindow:mcolorWindow, $
    cpanel_num:cpanel_num, ctr_num:ctr_num, prevSymbolSize:prevSymbolSize, $
    panelDroplist:panelDroplist, traceDroplist:traceDroplist, $
    xaxisTree:xaxisTree, yaxisTree:yaxisTree, $
    drawText:drawText, drawLabel:drawLabel, prevInc:prevInc, $
    drawDroplist:drawDroplist, drawButtonButton:drawButtonButton, $
    everyBase:everyBase, everyIncrement:everyIncrement, $;styleLineBase:styleLineBase, $
    vcolorWin:0, hfcolorWin:0, ciscolorWin:0, dataNames:dataNames, $
    drawObject:drawObject, historyWin:historyWin, $ ;,prevlsthick:prevlsthick, $
    loadedData:loadedData, windowStorage:windowStorage, origWindow:origWindow, $
    cWindow:cWindow, prevThickness:prevThickness, $ ;prevemthick:prevemthick, $
    panelObjs:ptr_new(panelObjs), cpanel:ptr_new(cpanel), ctrace:ptr_new(ctrace), $
    ctraces:ptr_new(ctraces), $;styleFrameBase:styleFrameBase, $
    subMainLineBase:subMainLineBase,template:template}; subMainLineBaseRight:subMainLineBaseRight}

  Widget_Control, tlb, Set_UValue=state, /No_Copy
  CenterTLB, tlb 
  Widget_Control, tlb, /Realize
  
  thm_ui_init_line_panel, tlb
  
  XManager, 'thm_ui_line_options', tlb, /no_block

  RETURN
END
