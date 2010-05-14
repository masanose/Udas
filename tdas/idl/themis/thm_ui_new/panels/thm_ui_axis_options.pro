;+  
; set_minor
;THM_UI_PROPAGATE_AXIS.PRO
;
;Given STATE propagate the current AXISSETTINGS from the current panel to all other panels.
;
;W.M.Feuerstein, 11/10/2008.
;
;-

pro THM_UI_PROPAGATE_AXIS , state

compile_opt idl2, hidden

;Get correct axis:
;*****************
;
panelobjs=state.panelobjs
npanels=N_Elements(panelobjs)
IF npanels GT 0 && Obj_Valid(panelobjs[0]) THEN currpanelobj = panelobjs[state.axispanelselect]
;
IF ~Obj_Valid(currpanelobj) THEN currpanelobj=Obj_New("THM_UI_PANEL", 1)
case state.axisselect of
  0: currpanelobj->GetProperty,xaxis = curraxissettings
  1: currpanelobj->GetProperty,yaxis = curraxissettings
  ;2: currpanelobj->GetProperty,zaxis = curraxissettings ;z axis doesn't work with this panel
endcase

;Copy AXISSETTINGS into the axissettings of all other panels:
;************************************************************
;
rangeId = widget_info(state.tlb,find_by_uname='rangesetall')
rangeFlag = widget_info(rangeId,/button_set)

tickId = widget_info(state.tlb,find_by_uname='tickssetall')
tickFlag = widget_info(tickId,/button_set)

gridId = widget_info(state.tlb,find_by_uname='gridsetall')
gridFlag = widget_info(gridId,/button_set)

annoId = widget_info(state.tlb,find_by_uname='annotationsetall')
annoFlag = widget_info(annoId,/button_set)

labelId = widget_info(state.tlb,find_by_uname='labeltsetall')
labelFlag = widget_info(labelId,/button_set)

IF N_Elements(panelobjs) GT 0 && Obj_Valid(panelobjs[0]) THEN BEGIN
  for i=0,n_elements(panelobjs)-1 do begin
    if i ne state.axispanelselect then begin
      ;axissettings = curraxissettings->Copy()
      temppanelobj = panelobjs[i]
      case state.axisselect of
        0: temppanelobj->getProperty,xaxis = newaxissettings
        1: temppanelobj->getProperty,yaxis = newaxissettings
       ; 2: temppanelobj->SetProperty,zaxis = axissettings ;z axis doesn't work with this panel
      endcase
      
      if rangeFlag then begin
        currAxisSettings->getProperty,$
          rangeOption=rangeOption,$
          scaling=scaling,$
          isTimeAxis=isTimeAxis,$
          rangeMargin=rangeMargin,$
          boundScaling=boundScaling,$
          boundFloating=boundFloating,$
          minFloatRange=minFloatRange,$
          maxFloatRange=maxFloatRange,$
          minBoundRange=minBoundRange,           $ ; range for bounded autoscaling
          maxBoundRange=maxBoundRange,           $ ; max range for bounded autoscaling
          minFixedRange=minFixedRange,           $ ; min range value if fixed scaling
          maxFixedRange=maxFixedRange,           $ ; max range value if fixed scaling
          FloatingSpan=floatingspan,             $ ; value of span if floating
          FloatingCenter=floatingcenter            ; mean, median, apprx. mean, or apprx. median
        
        newAxisSettings->setProperty,$
          rangeOption=rangeOption,$
          scaling=scaling,$
          isTimeAxis=isTimeAxis,$
          rangeMargin=rangeMargin,$
          boundScaling=boundScaling,$
          boundFloating=boundFloating,$
          minFloatRange=minFloatRange,$
          maxFloatRange=maxFloatRange,$
          minBoundRange=minBoundRange,           $ ; range for bounded autoscaling
          maxBoundRange=maxBoundRange,           $ ; max range for bounded autoscaling
          minFixedRange=minFixedRange,           $ ; min range value if fixed scaling
          maxFixedRange=maxFixedRange,           $ ; max range value if fixed scaling
          FloatingSpan=floatingspan,             $ ; value of span if floating
          FloatingCenter=floatingcenter            ; mean, median, apprx. mean, or apprx. median
       endif
       
       if tickFlag then begin
       
         currAxisSettings->getProperty,$
            MajorTickEvery=majortickevery,         $ ; display major ticks every 
            MinorTickEvery=minortickevery,         $ ; display major ticks every 
            MajorTickUnits=majortickunits,         $ ; major tick units (sec, hr, min, day, none)
            MinorTickUnits=minortickunits,         $ ; major tick units (sec, hr, min, day, none)
            MajorTickAuto=majortickauto,           $ ; set to automatically figure out major ticks
            MinorTickAuto=minortickauto,           $ ; set to automatically figure out minor ticks
            FirstTickAuto=firsttickauto,           $ ; obsolete
            NumMajorTicks=nummajorticks,           $ ; number of major ticks
            NumMinorTicks=numminorticks,           $ ; number of major ticks
            FirstTickAt=firsttickat,               $ ; value where first tick should be
            FirstTickUnits=firsttickunits,         $ ; first tick unit (sec, hr, min, day, none)
            TickStyle=tickstyle,                   $ ; style (inside, outside, both)
            BottomPlacement=bottomplacement,       $ ; flag set if ticks should be on bottom axis
            TopPlacement=topplacement,             $ ; flag set if ticks should be on top axis
            MajorLength=majorlength,               $ ; length of major tick
            MinorLength=minorlength,               $ ; length of minor tick
            autoTicks=autoTicks,                   $
            logminorticktype=logminorticktype
            
          newAxisSettings->setProperty,$
            MajorTickEvery=majortickevery,         $ ; display major ticks every 
            MinorTickEvery=minortickevery,         $ ; display major ticks every 
            MajorTickUnits=majortickunits,         $ ; major tick units (sec, hr, min, day, none)
            MinorTickUnits=minortickunits,         $ ; major tick units (sec, hr, min, day, none)
            MajorTickAuto=majortickauto,           $ ; set to automatically figure out major ticks
            MinorTickAuto=minortickauto,           $ ; set to automatically figure out minor ticks
            FirstTickAuto=firsttickauto,           $ ; obsolete
            NumMajorTicks=nummajorticks,           $ ; number of major ticks
            NumMinorTicks=numminorticks,           $ ; number of major ticks
            FirstTickAt=firsttickat,               $ ; value where first tick should be
            FirstTickUnits=firsttickunits,         $ ; first tick unit (sec, hr, min, day, none)
            TickStyle=tickstyle,                   $ ; style (inside, outside, both)
            BottomPlacement=bottomplacement,       $ ; flag set if ticks should be on bottom axis
            TopPlacement=topplacement,             $ ; flag set if ticks should be on top axis
            MajorLength=majorlength,               $ ; length of major tick
            MinorLength=minorlength,               $ ; length of minor tick
            autoTicks=autoTicks,                   $
            logminorticktype=logminorticktype
       
       endif
       
       if gridflag then begin
         currAxisSettings->getProperty,$
           majorGrid=majorgrid,                   $ ; linestyle object of major grid 
           minorGrid=minorgrid                      ; linestyle object of minor grid 
       
         newAxisSettings->setProperty,$
           majorGrid=(majorgrid->copy()),                   $ ; linestyle object of major grid 
           minorGrid=(minorgrid->copy())                      ; linestyle object of minor grid 

         currpanelobj->GetProperty, Settings=panelSettings
         IF obj_valid(panelSettings) THEN panelSettings->GetProperty, framethick=framethick $
           ELSE framethick=1
         
         IF obj_valid(panelobjs[i]) THEN panelobjs[i]->GetProperty, Settings=newSettings
         IF obj_valid(newSettings) THEN newSettings->SetProperty, framethick=framethick
       endif
       
       if annoflag then begin
          currAxisSettings->getProperty,$
            LineAtZero=lineatzero,                 $ ; flag set if line is drawn at zero
            showdate=showdate,                     $ ; flag set if date strings are shown
            DateString1=datestring1,               $ ; string format of date for line 1
            DateString2=datestring2,               $ ; string format of date for line 1                                         
            DateFormat1=dateformat1,               $ ; format of date for line 1 (annotation purposes)
            DateFormat2=dateformat2,               $ ; format of date for line 2                                         
            AnnotateAxis=annotateaxis,             $ ; flag set to annotate along axis                         
            PlaceAnnotation=placeannotation,       $ ; placement of annotation (bottom or top)
            AnnotateMajorTicks=annotatemajorticks, $ ; set flag if major ticks are annotated
            annotateEvery=annotateevery, $ ; value where annotation of major ticks occur
            annotateUnits=annotateunits, $ ; units for major tick value (sec, min, hr, day)
            FirstAnnotation=firstannotation,       $ ; value where annotation of first major tick occurs
            FirstAnnotateUnits=firstannotateunits, $ ; units for major tick value (sec, min, hr, day)
            AnnotateFirstTick=annotatefirsttick,   $ ; set flag to annotate first tick  
            AnnotateLastTick=annotateLasttick,   $ ; set flag to annotate last tick  
            AnnotateStyle=annotatestyle,           $ ; format style of tick (h:m, doy, time, etc....)
            annotateOrientation=annotateOrientation,$ ;orientation of the annotations: 0(horizontal) & 1(vertical)
            annotateTextObject=annotateTextObject, $  ; Text object that represents that textual style of annotations
            annotateExponent=annotateExponent
            
          newAxisSettings->setProperty,$ 
            LineAtZero=lineatzero,                 $ ; flag set if line is drawn at zero
            showdate=showdate,                     $ ; flag set if date strings are shown
            DateString1=datestring1,               $ ; string format of date for line 1
            DateString2=datestring2,               $ ; string format of date for line 1                                         
            DateFormat1=dateformat1,               $ ; format of date for line 1 (annotation purposes)
            DateFormat2=dateformat2,               $ ; format of date for line 2                                         
            AnnotateAxis=annotateaxis,             $ ; flag set to annotate along axis                         
            PlaceAnnotation=placeannotation,       $ ; placement of annotation (bottom or top)
            AnnotateMajorTicks=annotatemajorticks, $ ; set flag if major ticks are annotated
            annotateEvery=annotateevery, $ ; value where annotation of major ticks occur
            annotateUnits=annotateunits, $ ; units for major tick value (sec, min, hr, day)
            FirstAnnotation=firstannotation,       $ ; value where annotation of first major tick occurs
            FirstAnnotateUnits=firstannotateunits, $ ; units for major tick value (sec, min, hr, day)
            AnnotateFirstTick=annotatefirsttick,   $ ; set flag to annotate first tick  
            AnnotateLastTick=annotateLasttick,   $ ; set flag to annotate last tick  
            AnnotateStyle=annotatestyle,           $ ; format style of tick (h:m, doy, time, etc....)
            annotateOrientation=annotateOrientation,$ ;orientation of the annotations: 0(horizontal) & 1(vertical)
            annotateTextObject=(annotateTextObject->copy()),$   ; Text object that represents that textual style of annotations
            annotateExponent=annotateExponent
       endif
       
       if labelFlag then begin
         currAxisSettings->getProperty,$
            Orientation=orientation,               $ ; orientation of labels 0=Horizontal, 1=Vertical
            Margin=margin,                         $ ; number of points for label margins
            showLabels=showLabels,                 $ ; flag for whether or not labels are displayed
            labels=labels,                         $ ; A container object that stores the text objects which represent each label
            stackLabels=stackLabels,               $ ; A flag to determine whether labels should be stacked                           
            blackLabels=blackLabels                  ; A flag to determine whether labels should be stacked                           
         
         newAxisSettings->getProperty, labels=newlabels
         
         ;properly copy labels
         if obj_valid(labels) && obj_isa(labels,'IDL_Container') && $
            obj_valid(newlabels) && obj_isa(newlabels,'IDL_Container') then begin
         
           labellist = labels->get(/all)
           
           newlabellist = newlabels->get(/all)
           
           if obj_valid(labellist[0]) && obj_valid(newlabellist[0]) then begin
             
             for j=0, n_elements(newlabellist)-1 do begin

               if j gt n_elements(labellist)-1 then break

               if obj_valid(labellist[j]) then begin

                 labellist[j]->getProperty, font=lfont, format=lformat, color=lcolor, $
                                    size=lsize, thickness=lthickness, show=lshow

                 newlabellist[j]->setProperty, font=lfont, format=lformat, color=lcolor, $
                                    size=lsize, thickness=lthickness, show=lshow
               endif

             endfor
           
           endif
           
         endif else begin
         
           ;newlabels = obj_new()
           
         endelse
         
         newAxisSettings->setProperty,$ 
           Orientation=orientation,               $ ; orientation of labels 0=Horizontal, 1=Vertical
           Margin=margin,                         $ ; number of points for label margins
           showLabels=showLabels,                 $ ; flag for whether or not labels are displayed
           ;labels=newlabels,                         $ ; A container object that stores the text objects which represent each label
           stackLabels=stackLabels,                $ ; A flag to determine whether labels should be stacked                           
           blackLabels=blackLabels                  ; A flag to determine whether labels should be stacked                           

         if ~blacklabels && showlabels && (state.axisselect eq 1) then temppanelobj->SyncLinesToLabels

       endif
         
;         if state.axisselect eq 1 then begin
;           currpanelObj->GetProperty, blacklabel=blacklabel
;           if blacklabel then 
;             for j=0, n_elements(panelobjs)-1 do begin
;               panelobjs[j]->SetProperty, blacklabel=blackflag
;             endfor
;         endif
        
    endif
  endfor
ENDIF

end

;+
;
;Procedure: thm_ui_update_axis_from_draw
;
;Syntax:
;  THM_UI_INIT_AXIS_WINDOW , [ tlb ] [ , state = state ]
;
;After an update, this routine will update the fixed range of the axis objects
;Using the output of the draw object.  This stops the draw object from having
;to break abstraction.
;
;It also updates the number of ticks.  As the number displayed may vary
;from the number requested.
;
;-
pro thm_ui_update_axis_from_draw,drawObject,panels

  compile_opt idl2,hidden

  ;validate panel idl_container
  if obj_valid(panels) && panels->count() gt 0 then begin
  
    panel_list = panels->get(/all)
  
    ;loop over panel list
    for i = 0,n_elements(panel_list)-1 do begin
    
      panel_list[i]->getProperty,xaxis=xaxis,yaxis=yaxis
      
      ;get info about current panel settings
      info = drawObject->getPanelInfo(i)
      
      ;if there is a problem go to next iteration
      if ~is_struct(info) then continue
      
      ;get x range and delog it
      xrange = info.xrange
      
      if info.xscale eq 1 then begin
        xrange = 10D^xrange
      endif else if info.xscale eq 2 then begin
        xrange = exp(xrange)
      endif
      
      ;store it
      xaxis->setProperty,$
        minFixedRange=xrange[0],$
        maxFixedRange=xrange[1],$
        numMajorTicks=info.xmajorNum,$
        numMinorTicks=info.xminorNum
      
      ;get yrange and delog it
      yrange = info.yrange
      
      if info.yscale eq 1 then begin
;print,yrange
        yrange = 10D^yrange
;        ycenter = yrange[1]-yrange[0]
;        if ycenter gt 0 then yrange[0] = ycenter^2/yrange[1] else stop
      endif else if info.yscale eq 2 then begin
        yrange = exp(yrange)
;        ycenter = yrange[1]-yrange[0]
;        if ycenter gt 0 then yrange[0] = ycenter^2/yrange[1] else stop  ;Update to base e!
      endif
      
;print,yrange
      yaxis->setProperty,$
        minFixedRange=yrange[0],$
        maxFixedRange=yrange[1],$
        numMajorTicks=info.ymajorNum,$
        numMinorTicks=info.yminorNum
    
    endfor
  
  endif

end

function thm_ui_axis_options_convert_units,value,units,fromseconds=fromseconds

  compile_opt idl2, hidden
  
  if units eq 0 then begin
    conversion_factor = 1D
  endif else if units eq 1 then begin
    conversion_factor = 60D
  endif else if units eq 2 then begin
    conversion_factor = 60D*60D
  endif else if units eq 3 then begin
    conversion_factor = 60D*60D*24D
  endif
  
  if keyword_set(fromseconds) then begin
    return,value/conversion_factor
  endif else begin
    return,value*conversion_factor
  endelse
  
end

pro thm_ui_axis_options_init_color, state

  compile_opt idl2, hidden
  
  ; initialize color windows
  
  id = widget_info(state.tlb, find_by_uname='labelcolorwin')
  widget_control, id, get_value=labelcolorwin
  *state.currlabelobj->getproperty, color=color
  
  scene=obj_new('IDLGRSCENE', color=color)
  labelcolorwin->setProperty,graphics_tree=scene
  labelcolorwin->draw, scene
  
end


;+
;
;THM_UI_INIT_AXIS_WINDOW.PRO
;
;Given the top level base when running THM_UI_AXIS_OPTIONS, update all axis attributes from settings.
;
;The widget ID of the top level base should be the first parameter (this is when the program unit calling this routine
;does not have the STATE structure).  Otherwise, STATE should be passed by keyword and the first parameter should not be passed.
;
;Syntax:
;  THM_UI_INIT_AXIS_WINDOW , [ tlb ] [ , state = state ]
;
;W.M.Feuerstein, 11/7/2008.
;
;-

pro THM_UI_INIT_AXIS_WINDOW ,tlb, state=state

compile_opt idl2, hidden

statedef = ~(~size(state,/type))

if ~statedef then begin
  Widget_Control, tlb, Get_UValue=state, /No_Copy  ;Only get STATE if it is not passed in.
endif else begin
  tlb = state.tlb
endelse

;Get correct axis:
;*****************
;
IF N_Elements(state.panelObjs) GT 0 && Obj_Valid(state.panelObjs[0]) THEN BEGIN
  currpanelobj = state.panelobjs[state.axispanelselect]
  case state.axisselect of
    0: currpanelobj->GetProperty,xaxis = axissettings
    1: currpanelobj->GetProperty,yaxis = axissettings
   ; 2: currpanelobj->GetProperty,zaxis = axissettings ;doesn't work with z axis
  endcase
ENDIF ELSE BEGIN
  currpanelobj=Obj_New()
ENDELSE

; put updating of widgets on hold until end of init function
widget_control, state.tabbase, update=0


;*********************
;Initialize Range tab:
;*********************
;
;Get state of istime setting:
;****************************
IF Obj_Valid(axissettings) THEN BEGIN
  axissettings->GetProperty, istimeaxis=istime
ENDIF ELSE BEGIN
  axissettings=Obj_New("THM_UI_AXIS_SETTINGS")
  axissettings->GetProperty, istimeaxis=istime  
ENDELSE
;

id = widget_info(tlb, find_by_uname = 'rangepaneldroplist')
widget_control, id, set_combobox_select = state.axispanelselect

;Set appropriate sensitivity, buttons, and attributes for Range Options, Scaling, Fixed Min/Max, AutoRange, and Floating Center:
;*******************************************************************************************************************************
case istime of

  0: begin
    FOR i=0,N_Elements(state.rangeOptions)-1 DO Widget_Control, state.rangeOptions[i], /Sensitive
    FOR i=0,N_Elements(state.scalingOptions)-1 DO Widget_Control, state.scalingOptions[i], /Sensitive
    ;
    
    axissettings->GetProperty,rangeoption=rangeoption
    WIDGET_CONTROL, state.rangeoptions[rangeoption], /Set_Button
    axissettings->GetProperty,scaling=scaling
    WIDGET_CONTROL, state.scalingoptions[scaling], /Set_Button
    styleValues = axisSettings->GetAnnotationFormats()
    id = widget_info(state.tlb, find_by_uname='annotatestyle')
    widget_control, id, set_value = styleValues
    
    ;sensitize annotation styles
    for j=0, n_elements(state.atype)-1 do widget_control, state.atype[j], sensitive=1
        
    ;get data annotation format
    axissettings->GetProperty, annotateExponent=annotateExponent
    widget_control, state.atype[annotateExponent], set_button=1
    
    ;
    widget_control, state.minIncrement, /destroy
    widget_control, state.maxIncrement, /destroy 
    state.minIncrement=thm_ui_spinner(state.minBase, Increment=1, text_box_size=12, $
                                      uval='MINFIXEDRANGE', uname='minincrement',precision=16)
    state.maxIncrement=thm_ui_spinner(state.maxBase, Increment=1, text_box_size=12, $
                                      uval='MAXFIXEDRANGE', uname='maxincrement',precision=16)
    ;                                   
    case rangeoption of
      0: begin ;Auto-Range
  id = widget_info(state.tlb, find_by_uname = 'aobase')  
  widget_control, id, sensitive = 1 

  id = widget_info(state.tlb, find_by_uname = 'fobase')
  widget_control, id, sensitive = 0
  
  id = widget_info(state.tlb, find_by_uname = 'fltobase')
  widget_control, id, sensitive = 0

	id = widget_info(state.tlb, find_by_uname = 'rmincrement')
	axissettings->GetProperty, rangemargin = rangemargin
	widget_control, id, set_value = 100*rangemargin
	
	;
	id = widget_info(state.tlb, find_by_uname = 'boundscaling')
	axissettings->GetProperty, boundscaling = boundscaling
	widget_control, id, set_button = boundscaling

  id = widget_info(state.tlb,find_by_uname = 'boundbase')
  widget_control,id,sensitive=boundscaling

	id = widget_info(state.tlb, find_by_uname = 'minboundrange')
	axissettings->GetProperty, minboundrange = minboundrange 
	widget_control, id, set_value=minboundrange
	;
	id = widget_info(state.tlb, find_by_uname = 'maxboundrange')
	axissettings->GetProperty, maxboundrange = maxboundrange
	widget_control, id, set_value=maxboundrange
	;
	
	id = widget_info(state.tlb, find_by_uname = 'minincrement')
	axissettings->GetProperty, minfixedrange = value
  widget_control, id, set_value=value
	id = widget_info(state.tlb, find_by_uname = 'maxincrement')
	axissettings->GetProperty, maxfixedrange = value
  widget_control, id, set_value=value

      end
      1: begin ;Floating Center
	id = widget_info(state.tlb, find_by_uname = 'aobase')
	widget_control, id, sensitive = 0
  ;
  id = widget_info(state.tlb, find_by_uname = 'fobase')
  widget_control, id, sensitive = 0
	;
	id = widget_info(state.tlb, find_by_uname = 'fltobase')
	widget_control, id, sensitive = 1
	;
	id = widget_info(state.tlb, find_by_uname = 'floatingspan')
	axissettings->GetProperty, floatingspan = floatingspan
	widget_control, id, set_value=floatingspan
	;
	id = widget_info(state.tlb, find_by_uname = 'floatingcenter')
	axissettings->GetProperty,floatingcenter = floatingcenter
	widget_control, id, set_combobox_select = floatingcenter
	;
	id = widget_info(state.tlb, find_by_uname = 'minincrement')
	axissettings->GetProperty, minfixedrange = value
  widget_control, id, set_value=value
	id = widget_info(state.tlb, find_by_uname = 'maxincrement')
	axissettings->GetProperty, maxfixedrange = value
  widget_control, id, set_value=value
      end
      2: begin ;Fixed Max
  id = widget_info(state.tlb, find_by_uname = 'aobase')  
  widget_control, id, sensitive = 0 

  id = widget_info(state.tlb, find_by_uname = 'fobase')
  widget_control, id, sensitive = 1
  
  id = widget_info(state.tlb, find_by_uname = 'fltobase')
  widget_control, id, sensitive = 0
	
	;
	id = widget_info(state.tlb, find_by_uname = 'minincrement')
	axissettings->GetProperty, minfixedrange = minfixedrange
	widget_control, id, set_value = minfixedrange
	;
	id = widget_info(state.tlb, find_by_uname = 'maxincrement')
	axissettings->GetProperty, maxfixedrange = maxfixedrange
	widget_control, id, set_value = maxfixedrange
      end
    endcase
  end
  1: begin
    FOR i=0,N_Elements(state.rangeOptions)-1 DO Widget_Control, state.rangeOptions[i], Sensitive = 0
    FOR i=0,N_Elements(state.scalingOptions)-1 DO Widget_Control, state.scalingOptions[i], Sensitive = 0
    ;
    widget_control, state.minIncrement, /destroy
    widget_control, state.maxIncrement, /destroy 
    state.minIncrement=widget_text(state.minBase, Sensitive=sensitive, /editable, uval='MINFIXEDRANGE', $
                                   xsize=21, uname='minincrement',/all_events)
    state.maxIncrement=widget_text(state.maxBase, Sensitive=sensitive, /editable, uval='MAXFIXEDRANGE', $
                                   xsize=21, uname='maxincrement',/all_events)
    styleValues = axisSettings->GetAnnotationFormats()
    id = widget_info(state.tlb, find_by_uname='annotatestyle')
    widget_control, id, set_value = styleValues
    
    ;desensitize annotation types
    for j=0, n_elements(state.atype)-1 do widget_control, state.atype[j], sensitive=0
    
    id = widget_info(state.tlb, find_by_uname = 'aobase')  
    widget_control, id, sensitive = 0 
    id = widget_info(state.tlb, find_by_uname = 'fobase')
    widget_control, id, sensitive = 1
    id = widget_info(state.tlb, find_by_uname = 'fltobase')
    widget_control, id, sensitive = 0
    
    ;id = widget_info(state.tlb, find_by_uname = 'rmincrement')
    ;widget_control, id, sensitive = 0
    ;id = widget_info(state.tlb, find_by_uname = 'boundscaling')
    ;widget_control, id, sensitive = 0
    ;id = widget_info(state.tlb, find_by_uname = 'minboundrange')
    ;widget_control, id, sensitive = 0 
    ;id = widget_info(state.tlb, find_by_uname = 'maxboundrange')
    ;widget_control, id, sensitive = 0 
    ;id = widget_info(state.tlb, find_by_uname = 'floatingspan')
    ;widget_control, id, sensitive = 0
    ;id = widget_info(state.tlb, find_by_uname = 'floatingcenter')
    ;widget_control, id, sensitive = 0
  ;id = widget_info(state.tlb, find_by_uname = 'rmlabel')
  ;widget_control, id, sensitive = 0
  ; id = widget_info(state.tlb, find_by_uname = 'minlabel')
  ;widget_control, id, sensitive = 0
  ; id = widget_info(state.tlb, find_by_uname = 'maxlabel')
  ;widget_control, id, sensitive = 0

    ;
    id = widget_info(state.tlb, find_by_uname = 'minincrement')
    axissettings->GetProperty, minfixedrange = minfixedrange
    minfixedrangetime = formatDate(minfixedrange, '%date/%exacttime', 0)
    widget_control, id, set_value=minfixedrangetime, sensitive=1
    ;
    id = widget_info(state.tlb, find_by_uname = 'maxincrement')
    axissettings->GetProperty, maxfixedrange = maxfixedrange
    maxfixedrangetime = formatDate(maxfixedrange, '%date/%exacttime', 0)
    widget_control, id, set_value=maxfixedrangetime, sensitive=1
  end
endcase

id = widget_info(tlb,find_by_uname = 'istime')

widget_control,id,set_button=istime

; from Annotations tab to get ride of flicker in droplist
axissettings->GetProperty, annotatestyle=annotatestyle
id = widget_info(state.tlb, find_by_uname='annotatestyle')
widget_control, id, set_combobox_select=annotatestyle

widget_control, state.tlb,tlb_get_offset=off
widget_control, state.tlb, xoffset=off[0],yoffset=off[1]
widget_control, state.tabbase, update=1
;
;axissettings->GetProperty, rangesetall=rangesetall
;id = widget_info(tlb, find_by_uname = 'rangesetall')
;widget_control, id, set_button=rangesetall
;


;Warn user about x-axis range options and locking
if state.axisselect eq 0 then begin
  id = widget_info(state.tlb, find_by_uname='tabs')
  
  if widget_info(id, /tab_current) eq 0 then begin
    activeWindow = state.windowstorage->getactive()
    activewindow->getproperty, locked=locked
  
    if locked ne -1 then begin 
      state.statusbar->update, $
      '*Panels Are Locked: Changes to range are only displayed for the locked panel ("(L)" prefix).'
    endif 
  endif
endif


;*********************
;Initialize Ticks tab:
;*********************
;

;Set Auto Button

axisSettings->GetProperty,autoticks=autoticks
id = widget_info(tlb,find_by_uname = 'niceticks')
widget_control,id,set_button=autoticks

id = widget_info(tlb, find_by_uname = 'tickpaneldroplist')
widget_control, id, set_combobox_select = state.axispanelselect

axissettings->GetProperty, majortickevery = majortickevery
id = widget_info(tlb, find_by_uname = 'majortickevery')
widget_control, id, set_value = majortickevery

;Gets the list of units, dependent upon whether the axis is a time axis or not
units = axisSettings->getUnits()

axissettings->GetProperty, majortickunits = majortickunits

;because the list of units may change length when other settings
;the previous unit index may not index into the list anymore
if majorTickUnits ge n_elements(units) then begin
  majorTickUnits = 0
  axissettings->setProperty, majortickunits = majortickunits
endif

id = widget_info(tlb, find_by_uname = 'majortickunits')
widget_control, id, set_value = units
widget_control, id, set_combobox_select = majortickunits
state.majorUnits=majorTickUnits

axissettings->GetProperty, nummajorticks = value
id = widget_info(tlb, find_by_uname = 'nummajorticks')
widget_control, id, set_value = value

axissettings->GetProperty, numminorticks = value

;for now use majortickauto option to control by number/by interval widgets
axissettings->GetProperty, majortickauto = majortickauto
  bid = widget_info(tlb, find_by_uname = 'bynumber')
  id = widget_info(tlb, find_by_uname = 'numberbase')
  widget_control, bid, set_button = majortickauto
  widget_control, id, sensitive = majortickauto 
  
if autoticks && majortickauto then widget_control, state.minorTickBase, sensitive=0 else widget_control, state.minorTickBase, sensitive=1 
  
  bid = widget_info(tlb, find_by_uname = 'byinterval')
  id = widget_info(tlb, find_by_uname = 'intervalbase')
  widget_control, bid, set_button = ~majortickauto
  widget_control, id, sensitive = ~majortickauto
  
;if majortickauto then begin
;  id = widget_info(tlb, find_by_uname = 'majortickevery')
;  widget_control, id, sensitive=0
;  id = widget_info(tlb, find_by_uname = 'majortickunits')
;  widget_control, id, sensitive=0
;  id = widget_info(tlb, find_by_uname = 'nummajorticks')
;  widget_control, id, /sensitive 
;endif else begin
;  id = widget_info(tlb, find_by_uname = 'majortickevery')
;  widget_control, id, /sensitive
;  id = widget_info(tlb, find_by_uname = 'majortickunits')
;  if istime then widget_control, id, /sensitive else widget_control, id, sensitive=0
;  id = widget_info(tlb, find_by_uname = 'nummajorticks')
;  widget_control, id, sensitive=0
;endelse


;axissettings->GetProperty, minortickevery = minortickevery
;id = widget_info(tlb, find_by_uname = 'minortickevery')
;widget_control, id, set_value = minortickevery

;axissettings->GetProperty, minortickunits = minortickunits

;because the list of units may change length when other settings
;the previous unit index may not index into the list anymore
;if minorTickUnits ge n_elements(units) then begin
;  minorTickUnits = 0
;  axissettings->setProperty,minortickunits = minortickunits
;endif

;id = widget_info(tlb, find_by_uname = 'minortickunits')
;widget_control, id, set_value = units
;widget_control, id, set_combobox_select = minortickunits
;state.minorUnits = minorTickUnits

;axissettings->GetProperty, minortickauto = minortickauto
;id = widget_info(tlb, find_by_uname = 'minortickauto')
;widget_control, id, set_button = minortickauto

axissettings->GetProperty, numminorticks = value, autoticks=autoticks
if autoticks then begin
  id = widget_info(tlb, find_by_uname = 'numminorticks')
  widget_control, id, set_value = value
endif

;if minortickauto then begin
;  id = widget_info(tlb, find_by_uname = 'minortickevery')
;  widget_control, id, sensitive=0
;  id = widget_info(tlb, find_by_uname = 'minortickunits')
;  widget_control, id, sensitive=0
;  id = widget_info(tlb, find_by_uname = 'numminorticks')
;  widget_control, id, /sensitive 
;endif else begin
;  id = widget_info(tlb, find_by_uname = 'minortickevery')
;  widget_control, id, /sensitive
;  id = widget_info(tlb, find_by_uname = 'minortickunits')
;  if istime then widget_control, id, /sensitive else widget_control, id, sensitive=0
;  id = widget_info(tlb, find_by_uname = 'numminorticks')
;  widget_control, id, sensitive=0
;endelse

axissettings->GetProperty, firsttickat = firsttickat
bid = widget_info(state.tlb, find_by_uname='firsttickatbase')
id = widget_info(tlb, find_by_uname = 'firsttickat')

  ;update first tick at widget, change type if necessary
  if istime then begin
    if widget_info(id,/type) ne 3 then begin
      widget_control, id, /destroy
      id = widget_text(bid, value=time_string(firsttickat), uname='firsttickat',/editable)
    endif else widget_control, id, set_value = time_string(firsttickat)
  endif else begin
    if widget_info(id,/type) ne 0 then begin
      widget_control, id, /destroy
      id = thm_ui_spinner(bid, value=firsttickat, incr=1, uname='firsttickat', $
                          tooltip='Numerical location of first tick.', text_box=12)
    endif else widget_control, id, set_value = firsttickat
  endelse
  
axisSettings->getProperty,logMinorTickType=logMinorTickType

id = widget_info(tlb,find_by_uname='logminorticktype'+strtrim(logMinorTickType,2))
widget_control,id,/set_button

axisSettings->getProperty,scaling=scaling
id = widget_info(tlb,find_by_uname='logminorticktypebase')
if scaling eq 0 then begin
  widget_control,id,sensitive=0
endif else begin
  widget_control,id,sensitive=1
endelse
  
;axissettings->GetProperty, firsttickunits = firsttickunits

;because the list of units may change length when other settings
;the previous unit index may not index into the list anymore
;if firstTickUnits ge n_elements(units) then begin
;  firstTickUnits = 0
;  axissettings->setProperty,firsttickunits=firstTickUnits
;endif

;id = widget_info(tlb, find_by_uname = 'firsttickunits')
;widget_control, id, set_value=units
;widget_control, id, set_combobox_select = firsttickunits
;state.firstUnits = firstTickUnits

;if firsttickauto then begin
;  id = widget_info(tlb, find_by_uname = 'firsttickat')
;  widget_control, id, sensitive=0
;  id = widget_info(tlb, find_by_uname = 'firsttickunits')
;  widget_control, id, sensitive=0 
;endif else begin
;  id = widget_info(tlb, find_by_uname = 'firsttickat')
;  widget_control, id, /sensitive
;  id = widget_info(tlb, find_by_uname = 'firsttickunits')
;  if istime then widget_control, id, /sensitive else widget_control, id, sensitive=0 
;endelse

axissettings->GetProperty, tickstyle = value
id = widget_info(tlb, find_by_uname = 'tickstyle')
widget_control, id, set_combobox_select=value

axissettings->GetProperty, bottomplacement = value
id = widget_info(tlb, find_by_uname = 'bottomplace')
widget_control, id, set_button=value

axissettings->GetProperty, topplacement = value
id = widget_info(tlb, find_by_uname = 'topplace')
widget_control, id, set_button=value

axissettings->GetProperty, majorlength = value
id = widget_info(tlb, find_by_uname = 'majorlength')
widget_control, id, set_value=value

axissettings->GetProperty, minorlength = value
id = widget_info(tlb, find_by_uname = 'minorlength')
widget_control, id, set_value=value

;axissettings->GetProperty, tickssetall=tickssetall
;id = widget_info(tlb, find_by_uname = 'tickssetall')
;widget_control, id, set_button=tickssetall
;


;********************
;Initialize Grid tab:
;********************
;
id = widget_info(tlb, find_by_uname = 'gridpaneldroplist')
widget_control, id, set_combobox_select = state.axispanelselect

id = widget_info(tlb, find_by_uname = 'outlinethick')
IF ~Obj_Valid(currpanelobj) THEN currpanelobj=Obj_New("THM_UI_PANEL", 1)
currpanelobj->GetProperty, settings=panelsettings
panelsettings->GetProperty, framethick=framethick
widget_control, id, set_value = framethick

;intialize color major grid window
;*********************************
;
axissettings->GetProperty, majorgrid=majorgrid
if obj_valid(majorgrid) then begin
  majorgrid->GetProperty,color=color
endif else begin
  color = [0,0,0]
  majorGrid = obj_new('THM_UI_LINE_STYLE',color=color, show=0)
  axissettings->SetProperty, majorgrid=majorgrid
endelse
Widget_Control, state.majorgridcolorWindow, Get_Value=majorgridcolorWin
if obj_valid(scene) then scene->remove,/all
scene=obj_new('IDLGRSCENE', color=color)
majorgridcolorWin->draw, scene

majorgrid->GetProperty, show=showmajor
id = widget_info(state.tlb, find_by_uname = 'majorbase')
widget_control, id, sensitive=showmajor
id = widget_info(state.tlb, find_by_uname = 'majorgrids')
widget_control, id, set_button=showmajor

styleNames = majorgrid->getlinestyles()
majorgrid->GetProperty, id=styleid
id=widget_info(state.tlb, find_by_uname = 'majorgridstyle')
widget_control, id, set_combobox_select=styleid

majorgrid->GetProperty, thickness=thickness
id=widget_info(state.tlb, find_by_uname = 'majorgridthick')
widget_control, id, set_value = thickness


; intialize color minor grid window
  axissettings->GetProperty, minorgrid=minorgrid
  if obj_valid(minorgrid) then begin
    minorgrid->GetProperty,color=color
  endif else begin
    color = [0,0,0]
    minorGrid = obj_new('thm_ui_line_style',color=color, show=0)
    axissettings->SetProperty, minorgrid=minorgrid
  endelse
  Widget_Control, state.minorgridcolorWindow, Get_Value=minorgridcolorWin
  if obj_valid(scene) then scene->remove,/all
  scene=obj_new('IDLGRSCENE', color=color)
  minorgridcolorWin->draw, scene

minorgrid->GetProperty, show=showminor
id = widget_info(state.tlb, find_by_uname = 'minorbase')
widget_control, id, sensitive=showminor
id = widget_info(state.tlb, find_by_uname = 'minorgrids')
widget_control, id, set_button=showminor


styleNames = minorgrid->getlinestyles()
minorgrid->GetProperty, id=styleid
id=widget_info(state.tlb, find_by_uname = 'minorgridstyle')
widget_control, id, set_value=styleNames, set_combobox_select=styleid

minorgrid->GetProperty, thickness=thickness
id=widget_info(state.tlb, find_by_uname = 'minorgridthick')
widget_control, id, set_value = thickness


;axissettings->GetProperty, gridsetall=gridsetall
;id = widget_info(tlb, find_by_uname = 'gridsetall')
;widget_control, id, set_button=gridsetall


;**************************
;Initialize Annotation tab:
;**************************
;
id = widget_info(tlb, find_by_uname = 'annopaneldroplist')
widget_control, id, set_combobox_select = state.axispanelselect

axissettings->GetProperty, lineatzero=lineatzero
id = widget_info(tlb, find_by_uname = 'lineatzero')
widget_control, id, set_button=lineatzero

axissettings->GetProperty, showdate=showdate
id = widget_info(tlb, find_by_uname = 'showdate')
widget_control, id, set_button=showdate
if istime && ~state.axisselect then widget_control, id, /sensitive else widget_control, id, sensitive=0

id = widget_info(state.tlb, find_by_uname='anodatebase')
if istime && ~state.axisselect then widget_control, id, /sensitive else widget_control, id, sensitive=0

axissettings->GetProperty, datestring1=datestring1
id = widget_info(tlb, find_by_uname = 'anodate1')
widget_control, id, set_value=datestring1

axissettings->GetProperty, datestring2=datestring2
id = widget_info(tlb, find_by_uname = 'anodate2')
widget_control, id, set_value=datestring2

axissettings->GetProperty, annotateaxis=annotateaxis
id = widget_info(state.tlb, find_by_uname='annotateaxis')
widget_control, id, set_button=annotateaxis
id = widget_info(state.tlb, find_by_uname='annoaxisbase')
widget_control, id, sensitive=annotateaxis

axissettings->GetProperty, placeannotation=placeannotation
id = widget_info(tlb, find_by_uname = 'placeannotation')
widget_control, id, set_combobox_select=placeannotation


axissettings->GetProperty, annotatemajorticks=annotatemajorticks
id = widget_info(state.tlb, find_by_uname='annotatemajorticks')
widget_control, id, set_button=annotatemajorticks
id = widget_info(state.tlb, find_by_uname='annomajorbase')
widget_control, id, sensitive=~annotatemajorticks

axissettings->GetProperty, annotateevery=annotateevery
id = widget_info(state.tlb, find_by_uname='annotateevery')
widget_control, id, set_value=annotateevery


axissettings->GetProperty, annotateunits=annotateunits

;if isTime switch caused units switch, make sure they don't index out of range
if annotateUnits ge n_elements(units) then begin
  annotateUnits = 0
  axissettings->setProperty, annotateUnits = annotateUnits
endif

id = widget_info(state.tlb, find_by_uname='annotateunits')
widget_control, id, set_value = units
widget_control, id, set_combobox_select=annotateunits

axissettings->GetProperty, firstannotation=firstannotation
id = widget_info(state.tlb, find_by_uname='firstannotation')
bid =  widget_info(state.tlb, find_by_uname='firstannotationbase')

if istime then begin
  if widget_info(id,/type) ne 3 then begin
    widget_control, id, /destroy
    id = widget_text(bid, value=time_string(firstannotation), uname='firstannotation',/editable)
  endif else begin
    widget_control, id, set_value = time_string(firstannotation)
  endelse
endif else begin
  if widget_info(id,/type) ne 0 then begin
    widget_control,id,/destroy
    id = thm_ui_spinner(bid, value=firstannotation, incr=1, uname='firstannotation', $
                          tooltip='Numerical location of first annotation.', text_box=12)
  endif else begin
    widget_control, id, set_value=firstannotation
  endelse
endelse

axissettings->GetProperty, annotatefirsttick=annotatefirsttick
id = widget_info(state.tlb, find_by_uname='annotatefirsttick')
widget_control, id, set_button=annotatefirsttick

; moved to Range tab init section to get ride of flicker in droplist
;axissettings->GetProperty, annotatestyle=annotatestyle
;id = widget_info(state.tlb, find_by_uname='annotatestyle')
;widget_control, id, set_combobox_select=annotatestyle

axissettings->GetProperty, annotatelasttick=annotatelasttick
id = widget_info(state.tlb, find_by_uname='annotatelasttick')
widget_control, id, set_button=annotatelasttick

axissettings->GetProperty, annotateorientation=annotateorientation
;if annotateorientation then begin
id = widget_info(state.tlb, find_by_uname='annohorizontal')
widget_control, id, set_button=~annotateorientation
id = widget_info(state.tlb, find_by_uname='annovertical')
widget_control, id, set_button=annotateorientation

axissettings->GetProperty, annotateTextObject=annotateTextObject
annotateTextObject->GetProperty, font=font
id = widget_info(state.tlb, find_by_uname='anofontlist')
widget_control, id, set_combobox_select=font

annotateTextObject->GetProperty, size=size
id = widget_info(state.tlb, find_by_uname='anofontsize')
widget_control, id, set_value=size

; intialize annotation font color window
  axissettings->GetProperty, annotatetextobject=annotatetextobject
  if obj_valid(annotatetextobject) then begin
    annotatetextobject->GetProperty,color=color
  endif else begin
    color = [0,0,0]
    annotatetextobject = obj_new('thm_ui_text',color=color, show=0)
    axissettings->SetProperty, annotatetextobject=annotatetextobject
  endelse
  Widget_Control, state.anocolorWindow, Get_Value=anocolorWin
  if obj_valid(scene) then scene->remove,/all
  scene=obj_new('IDLGRSCENE', color=color)
  anocolorWin->draw, scene

;axissettings->GetProperty, annotationsetall=annotationsetall
;id = widget_info(tlb, find_by_uname = 'annotationsetall')
;widget_control, id, set_button=annotationsetall


;**********************
;Initialize Labels tab:
;**********************
;
id = widget_info(tlb, find_by_uname = 'labelpaneldroplist')
widget_control, id, set_combobox_select = state.axispanelselect

IF state.axisselect EQ 1 THEN BEGIN
  axisSettings->GetProperty, BLACKLABELS=blacklabels
  id = widget_info(state.tlb, find_by_uname = 'blacklabels')
  widget_control, id, set_button = blacklabels  
ENDIF

axissettings->GetProperty, stacklabels = stacklabels
id = widget_info(state.tlb, find_by_uname = 'stacklabels')
widget_control, id, set_button = stacklabels

axissettings->GetProperty, showlabels = showlabels
id = widget_info(state.tlb, find_by_uname = 'showlabels')
widget_control, id, set_button = showlabels

id = widget_info(state.tlb, find_by_uname = 'labeldroplist')
axissettings->GetProperty, labels=labelsObj

showid = widget_info(state.tlb,find_by_uname = 'showlabel')

if obj_valid(labelsObj) then labels = labelsObj->get(/all) ELSE labels=Obj_New("THM_UI_TEXT", Value=' ')


if obj_valid(labels[0]) then begin

  nlabels = n_elements(labels)
  labeltextlines = strarr(nlabels)
  for i=0,nlabels-1 do begin
    labels[i]->GetProperty, value=labeltext
    if strlen(labeltext) GT 35 then labeltext=strmid(labeltext, 0, 35)
    labeltextlines[i]=labeltext
  endfor
  idx = where(labeltextlines eq '')

  if idx[0] ne -1 then begin
    placeholder = ' '
    ;linux combobox treats identical entries as same index, use different place holders
    for i=0, n_elements(idx)-1 do begin
      labeltextlines[idx[i]] = placeholder
      placeholder += ' '
    endfor
  endif

  ;labelselect = 0
  if ptr_valid(state.currlabelobj) then ptr_free, state.currlabelobj
  state.currlabelobj = ptr_new(labels[state.labelselect])
  *state.currLabelObj->getProperty,show=showlabel
  widget_control, id, set_value=labeltextlines
  widget_control, id, set_combobox_select=state.labelselect
  IF showlabel THEN widget_control,showid,set_button=1 ELSE widget_control,showid,set_button=0 
  *state.currlabelobj->GetProperty, font=font, format=format, size=size, color=color, value=value
  IF showlabels THEN show=1 ELSE show=0
;  for i=0,nlabels-1 do begin
;    IF NOT show THEN labels[i]->SetProperty, Show=0
;  endfor
  widget_control, showid, sensitive=show
  id = widget_info(state.tlb, find_by_uname = 'labeldroplist')
  widget_control, id, sensitive=show
  
endif else begin
  state.labelselect = 0
  labeltextlines=[' ']
  widget_control, id, set_value=' '
  widget_control,showid,sensitive=1
  font=0
  format=3
  size=10
  color=[0,0,0]
endelse
id = widget_info(tlb, find_by_uname='labeltextedit')
widget_control, id, set_value=value
id = widget_info(state.tlb, find_by_uname='labelfont')
widget_control, id, set_combobox_select=font
id = widget_info(state.tlb, find_by_uname='labelformat')
widget_control, id, set_combobox_select=format
id = widget_info(state.tlb, find_by_uname='labelsize')
widget_control, id, set_value=size

; intialize label color window
  Widget_Control, state.labelcolorWindow, Get_Value=labelcolorWin
  if obj_valid(scene) then scene->remove,/all
  scene=obj_new('IDLGRSCENE', color=color)
  labelcolorWin->draw, scene

axissettings->GetProperty, orientation=orientation
id = widget_info(state.tlb, find_by_uname='labelhorizontal')
widget_control, id, set_button=~orientation
id = widget_info(state.tlb, find_by_uname='labelvertical')
widget_control, id, set_button=orientation

axissettings->GetProperty, margin=margin
id = widget_info(state.tlb, find_by_uname='labelmargin')
widget_control, id, set_value=margin

thm_ui_axis_options_init_color, state

;axissettings->GetProperty, labeltsetall=labeltsetall
;id = widget_info(tlb, find_by_uname = 'labeltsetall')
;widget_control, id, set_button=labeltsetall

;Initialize annotation font color:
;*********************************

;widget_control, state.tabbase, update=1 ; update all widgets


if ~statedef then Widget_Control, tlb, Set_UValue=state, /No_Copy   ;Only put STATE if it was not passed in.

END ; -----------------------------------------


;Get and return current axis settings object from state
FUNCTION thm_ui_axis_options_getaxis, state 

  currpanelobj = state.panelobjs[state.axispanelselect]
  
  if ~Obj_Valid(currpanelobj) then currpanelobj=Obj_New("THM_UI_PANEL", 1)
  case state.axisselect of
    0: currpanelobj->GetProperty,xaxis = axissettings
    1: currpanelobj->GetProperty,yaxis = axissettings
  endcase
  if ~obj_valid(axissettings) then axissettings=obj_new('THM_UI_AXIS_SETTINGS')
  
  return, axissettings
  
END ; -------------------------------------------

function thm_ui_axis_get_combobox_select,tlb,uname

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

pro thm_ui_axis_options_set_scaling,tlb,state

  compile_opt idl2,hidden
  
  axissettings = thm_ui_axis_options_getaxis(state)
  
  linid = widget_info(tlb,find_by_uname='linear')
  logid = widget_info(tlb,find_by_uname='log10')
  
  if widget_info(linid,/button_set) then begin
    axissettings->setProperty,scaling=0
  endif else if widget_info(logid,/button_set) then begin
    axissettings->setProperty,scaling=1
  endif else begin
    axissettings->setProperty,scaling=2
  endelse
  
  
end

; Gets range values from widgets and applies them to the corresponding settings
; thm_ui_axis_options_set_scaling should be called first
;
PRO thm_ui_axis_options_set_range, tlb, state
  
    compile_opt idl2, hidden
  
  axissettings = thm_ui_axis_options_getaxis(state)
  
  ; Retrieve scaling for later
  axissettings->getproperty,scaling=scaling, istimeaxis=istimeaxis
  
  ; Fixed Range
  ;
  ID = widget_info(tlb, find_by_uname='fixedrange')
  if widget_info(ID, /button_set) or istimeaxis then begin
    axissettings->SetProperty,rangeoption=2
    maxID = widget_info(tlb, find_by_uname='maxincrement')
    minID = widget_info(tlb, find_by_uname='minincrement')
    widget_control, minid, get_value=minv
    widget_control, maxid, get_value=maxv
    if istimeaxis then begin
      minv = time_double(minv)
      maxv = time_double(maxv)
    endif
    if finite(minv,/nan) && finite(maxv,/nan) then begin
      state.historywin->Update, 'Invalid fixed min and max, no changes made.'
      state.statusBar->update,'Invalid fixed min and max, no changes made.'
    endif else if finite(minv,/nan) then begin
      state.historywin->Update, 'Invalid fixed min, no changes made.'
      state.statusBar->update,'Invalid fixed min, no changes made.'
    endif else if finite(maxv,/nan) then begin
      state.historywin->Update, 'Invalid fixed max, no changes made.'
      state.statusBar->update,'Invalid fixed max, no changes made.'
    endif else begin
      axisSettings->getProperty,minFixedRange=oldmin,maxFixedRange=oldmax
      if minv ge maxv then begin
        state.historywin->update, 'Maximum range value must be greater than minimum. No changes made.' 
        state.statusbar->update,'Maximum range value must be greater than minimum. No changes made.'
      endif else if scaling ne 0 && minv le 0 then begin
        state.historywin->update, 'Range must be greater than zero for logarithmic scaling.' 
        state.statusbar->update, 'Range must be greater than zero for logarithmic scaling.' 
      endif else begin
        axisSettings->setproperty, minfixedrange=minv
        axisSettings->setproperty, maxfixedrange=maxv
      endelse
    endelse
    if isTimeAxis then return ; will always be fixed for time, regardless of widget selections
  endif
  
  ;Floating Span
  ;
  ID = widget_info(tlb, find_by_uname='floatingspan')
  if widget_info(ID, /button_set) then begin
    axissettings->SetProperty,rangeoption=1
    widget_control, id, get_value = floatingspan
    if ~finite(floatingspan,/nan) then begin
      if floatingspan lt 0 then begin
        state.historywin->Update, 'Error - Floating Span must be positive. Pleae re-enter a positive value.'
        axisSettings->getproperty, floatingspan=floatingspan
        Widget_Control, id, set_value = floatingspan
      endif else begin
        state.historywin->Update, 'Floating span changed to '+StrCompress(String(floatingspan))
        axissettings->SetProperty, floatingspan=floatingspan
      endelse
    endif else state.historywin->Update, 'Invalid floating span, no changes made.'
  endif
  
  ;Auto-Range
  ;
  id = widget_info(tlb, find_by_uname='autorange')
  if widget_info(ID, /button_set) then begin
    axissettings->SetProperty,rangeoption=0
    rmarginid = widget_info(tlb,find_by_uname='rmincrement')
    widget_control, rmarginid, get_value = rangemargin
    if ~finite(rangemargin,/nan) then begin
      if rangemargin lt 0 then begin
         state.historywin->Update, 'Range Margin value must be greater than 0; value reset.'
         ok = dialog_message('Range Margin value must be greater than 0%; value reset.',/information,/center)
         axissettings->GetProperty, rangemargin=rangemargin
         Widget_Control, rmarginid, set_value = rangemargin
      endif else begin
        axissettings->SetProperty, rangemargin = rangemargin/100
      endelse
    endif else state.historywin->Update, 'Invalid range margin, no changes made.'
      
    ; Bound Scaling
    ;
    boundID = widget_info(tlb, find_by_uname='boundscaling')
    bound = widget_info(boundID, /button_set)
    
    if bound then begin
      ID = widget_info(tlb, find_by_uname='minboundrange')
      widget_control, ID, get_value = minboundrange
      ID = widget_info(tlb, find_by_uname='maxboundrange')
      widget_control, ID, get_value = maxboundrange
      if ~finite(minboundrange,/nan) && ~finite(maxboundrange,/nan) then begin
        if minboundrange ge maxboundrange then begin
        ;  result=dialog_message('Bound scaling cannot be set if Minumum is greater than or equal to Maximum.', /INFORMATION,/center)
          state.historywin->Update, 'Bound scaling was not set. Minumum value greater than or equal to Maximum.'
          state.statusBar->update,'Bound scaling was not set. Minumum value greater than or equal to Maximum.'
          Widget_Control, boundID, Set_Button=0
          bound = 0
        endif else begin
          axissettings->setproperty, maxboundrange = maxboundrange, minboundrange = minboundrange
        endelse
      endif else state.statusBar->Update, 'Invalid bound range min/max, no changes made.'
    endif
    
    axisSettings->getProperty,boundscaling=currentbound
    
    if currentBound ne bound then begin
      axissettings->SetProperty, boundscaling = bound 
      scaleState = bound ? 'on.':'off.'
      state.historywin->Update, String('Bound scaling turned ',scaleState)
      state.statusBar->update,String('Bound scaling turned ',scaleState)
    endif
    
  endif
END ;------------------------------------------------


;Applies current widget values to selected label
;Label title is applied in main avent handler
PRO thm_ui_axis_options_set_labels, tlb, state

    compile_opt idl2, hidden

;Get current axis settings and current panel
  axissettings = thm_ui_axis_options_getaxis(state)
  currpanelobj = state.panelobjs[state.axispanelselect]

  ; test for validity so OK/APPLY doesn't bomb when no data loaded bck 4/1/09
  if ~obj_valid(currpanelobj) then begin
    state.historywin->update, 'Current panel object is invalid'
    return
  endif

;Get font
  id = widget_info(tlb, find_by_uname='labelfont')
  widget_control, id, get_value =fontlist
  font = widget_info(id, /combobox_gettext)
  font = where(fontlist eq font)
  
;Get format
  id = widget_info(tlb, find_by_uname='labelformat')
  widget_control, id, get_value =formatlist
  format = widget_info(id, /combobox_gettext)
  format = where(formatlist eq format)

;Get size
  id = widget_info(tlb, find_by_uname='labelsize')
  widget_control, id, get_value =size

;Get color
  id = widget_info(tlb, find_by_uname='labelcolorwin')
  widget_control, id, get_value =colorwin
  colorwin->getProperty,graphics_tree=scene
  scene->getProperty,color=color

;Set Values, sync to labels if applicable
  if ptr_valid(state.currlabelObj) then begin
    *state.currlabelobj->SetProperty, font=font, format=format, color=color
    if state.axisselect eq 1 then begin
        axissettings->GetProperty, blacklabels=blacklabels, showlabels=showlabels
        if ~blacklabels and showlabels then currpanelobj->SyncLinesToLabels
    endif 
    if ~finite(size,/nan) then begin
      if size lt 0 then state.historywin->update, 'Label Size must be greater than zero.' $
        else *state.currlabelobj->SetProperty, size = double(size)
    endif else state.historywin->Update, 'Invalid lable size, no changes made.'
  endif $
    else state.historywin->update, 'Invalid pointer to label object'
  
END ;------------------------------------------------

pro thm_ui_axis_options_set_major_ticks,state

  compile_opt idl2,hidden
  
  tlb = state.tlb
  
  axissettings = thm_ui_axis_options_getaxis(state)
  
  ;setting number of major ticks
  id = widget_info(tlb, find_by_uname = 'nummajorticks')
  widget_control,id,get_value=ticknum
  if ~finite(ticknum,/nan) then begin
    if tickNum lt 0 then begin
      state.historywin->update,'Major Tick Number cannot be less than 0'
      state.statusBar->update,'Major Tick Number cannot be less than 0'
      axisSettings->getProperty,numMajorTicks=ticknum
      widget_control,id,set_value=ticknum
    endif else begin
      axissettings->SetProperty, nummajorticks = ticknum
    endelse
  endif else state.historywin->update, 'Invalid number of major ticks, no changes made.'
  
  id = widget_info(tlb, find_by_uname = 'majortickevery')
  widget_control,id,get_value=majortickevery 
  if ~finite(majortickevery,/nan) then begin
    if majortickevery le 0 then begin
      state.statusbar->Update, 'Major tick interval must be greater than zero.'
      state.historywin->Update, 'Major tick interval must be greater than zero.'
      axissettings->GetProperty,majorTickEvery=majorTickEvery
      widget_control,id,set_value=majorTickEvery
    endif else begin
      axisSettings->setProperty,majorTickEvery=majorTickEvery
    endelse
  endif else state.historywin->update, 'Invalid major tick interval, no changes made.'
  
  axisSettings->setProperty,majorTickUnits=thm_ui_axis_get_combobox_select(tlb,'majortickunits')
  
  id = widget_info(tlb,find_by_uname='bynumber')
  axisSettings->setProperty,majorTickAuto=widget_info(id,/button_set)
  
end

pro thm_ui_axis_options_set_minor_ticks,state

  compile_opt idl2,hidden
  
  tlb = state.tlb
  
  axissettings = thm_ui_axis_options_getaxis(state)
  
  ;setting number of minor ticks
  id = widget_info(tlb, find_by_uname = 'numminorticks')
  widget_control,id,get_value=ticknum
  if ~finite(ticknum,/nan) then begin
    if tickNum lt 0 then begin
      state.historywin->update,'minor Tick Number cannot be less than 0'
      state.statusBar->update,'minor Tick Number cannot be less than 0'
      axisSettings->getProperty,numminorTicks=ticknum
      widget_control,id,set_value=ticknum
    endif else begin
      axissettings->SetProperty, numminorticks = ticknum
    endelse
  endif else begin
    state.statusbar->update, 'Invalid number of minor ticks, no changes made.'
    axisSettings->getProperty, numminorticks = ticknum
    widget_control, id, set_value = ticknum
  endelse
  
  for i = 0,3 do begin
    id = widget_info(tlb,find_by_uname='logminorticktype'+strtrim(i,2))
    if widget_info(id,/button_set) then begin
      axisSettings->setProperty,logminorticktype=i
    endif
  endfor
  
;  id = widget_info(tlb, find_by_uname = 'minortickevery')
;  widget_control,id,get_value=minortickevery 
;  if ~finite(minortickevery,/nan) then begin
;    if minortickevery le 0 then begin
;      state.statusbar->Update, 'minor tick interval must be greater than zero.'
;      state.historywin->Update, 'minor tick interval must be greater than zero.'
;      axissettings->GetProperty,minorTickEvery=minorTickEvery
;      widget_control,id,set_value=minorTickEvery
;    endif else begin
;      axisSettings->setProperty,minorTickEvery=minorTickEvery
;    endelse
;  endif else state.historywin->update, 'Invalid minor tick interval, no changes made.'
  
;  axisSettings->setProperty,minorTickUnits=thm_ui_axis_get_combobox_select(tlb,'minortickunits')
  
;  id = widget_info(tlb,find_by_uname='minortickauto')
;  axisSettings->setProperty,minorTickAuto=widget_info(id,/button_set)

  ;ensure that minor ticks are on auto
  axisSettings->setProperty,minorTickAuto=1
  
end

pro thm_ui_axis_options_set_first_ticks,state

  compile_opt idl2,hidden
  
  tlb = state.tlb
  
  axissettings = thm_ui_axis_options_getaxis(state)
  axissettings->GetProperty, istimeaxis=istime, maxfixedrange=maxfixedrange
  
  id = widget_info(tlb, find_by_uname = 'firsttickat')
  widget_control,id,get_value=firsttickat
  
  f = 'Align ticks at'

  ;If dealing with time axis
  if istime then begin
    if ~is_string(thm_ui_timefix(firsttickat)) then begin
      state.statusbar->update,f+': Invalid entry, please enter valid time (yyyy-mm-dd/hh:mm:ss).'
      state.historywin->update,f+': Invalid time.',/dontshow
      axissettings->GetProperty,firsttickat=firsttickat
      widget_control,id,set_value=time_string(firsttickat)
;    endif else if time_double(firsttickat) gt maxfixedrange then begin
;      state.statusbar->update,f+': Invalid entry, value cannot be greater than range. .'
;      state.historywin->update,f+': Invalid entry, cannot be greater than range.',/dontshow
;      axissettings->GetProperty,firsttickat=firsttickat
;      widget_control,id,set_value=time_string(firsttickat)    
    endif else begin
      axisSettings->setProperty,firsttickat=time_double(firsttickat)
    endelse
  ;If dealing with non-time axis
  endif else if finite(firsttickat) then begin
;    if firsttickat gt maxfixedrange then begin
;      state.statusbar->update,f+': Invalid entry, value cannot be greater than range.' 
;      state.historywin->update,f+': Invalid entry, cannot be greater than range.',/dontshow
;    endif else begin
      axisSettings->setProperty,firsttickat=firsttickat
    ;endelse
  endif else begin
    state.statusbar->update, 'Invalid "'+f+'", value not applied.' 
    state.historywin->update, 'Invalid "'+f+'" value, no changes made.',/dontshow
  endelse
  
end

pro thm_ui_axis_options_set_ticks,tlb,state

  compile_opt idl2,hidden

  axissettings = thm_ui_axis_options_getaxis(state)
  
  ;set major tick settings
  thm_ui_axis_options_set_major_ticks,state
  
  ;set minor tick settings
  thm_ui_axis_options_set_minor_ticks,state
  
  ;set first tick settings
  thm_ui_axis_options_set_first_ticks,state

  ;set 'Nice ticks' button
  id = widget_info(tlb,find_by_uname='niceticks')
  ;must be set to zero when not sensitive until new behavior 
  ;is implemented, otherwise will override other settings
;  if widget_info(id, /sensitive) then begin
    axissettings->setproperty, autoticks = widget_info(id,/button_set)
;    widget_control, state.minorTickBase, sensitive=1
;  endif
   
;  else begin
;    axissettings->setproperty, autoticks = 0 ; commented out for Task 1538
;    widget_control, state.minorTickBase, sensitive=0
;  endelse
  
  ;if widget_info(id,/button_set) then widget_control, state.minorTickBase, sensitive=1 else widget_control, state.minorTickBase, sensitive=0
  ;set style button
  axissettings->setProperty,tickStyle=thm_ui_axis_get_combobox_select(tlb,'tickstyle')
  
  ;tick placement options

  id = widget_info(tlb, find_by_uname = 'bottomplace')
  axissettings->SetProperty, bottomplacement = widget_info(id,/button_set)
 
  id = widget_info(tlb, find_by_uname = 'topplace')
  axissettings->SetProperty, topplacement = widget_info(id,/button_set)

  ;tick length options
  
  id = widget_info(tlb, find_by_uname = 'majorlength')
  widget_control, id, get_value=value
  if ~finite(value,/nan) then axissettings->setProperty, majorlength = value 
  
  id = widget_info(tlb, find_by_uname = 'minorlength')
  widget_control, id, get_value=value
  if ~finite(value,/nan) then axissettings->setProperty, minorlength = value

end

;Get annotation settings and apply
;Only does first annotation at the moment
pro  thm_ui_axis_options_set_annotations,tlb,state

  compile_opt idl2, hidden
  
  axissettings = thm_ui_axis_options_getaxis(state)
  
  firstannotationid = widget_info(tlb,find_by_uname='firstannotation')
  widget_control,firstannotationid,get_value=firstannotation
  
  axissettings->GetProperty, istimeaxis=istime, maxfixedrange=maxfixedrange
  
  f = 'Align Annotations at'

  ;If dealing with time axis
  if istime then begin
    if ~is_string(thm_ui_timefix(firstannotation)) then begin
      state.statusbar->update,f+': Invalid entry, please enter valid time (yyyy-mm-dd/hh:mm:ss).'
      state.historywin->update,f+': Invalid time.',/dontshow
      axissettings->GetProperty,firstannotation=firstannotation
      widget_control,firstannotationid,set_value=time_string(firstannotation)
    endif else if time_double(firstannotation) gt maxfixedrange then begin
      state.statusbar->update,f+': Invalid entry, value cannot be greater than range. .'
      state.historywin->update,f+': Invalid entry, cannot be greater than range.',/dontshow
      axissettings->GetProperty,firstannotation=firstannotation
      widget_control,firstannotationid,set_value=time_string(firstannotation)    
    endif else begin
      axisSettings->setProperty,firstannotation=time_double(firstannotation)
    endelse
  ;If dealing with non-time axis
  endif else if finite(firstannotation) then begin
    if firstannotation gt maxfixedrange then begin
      state.statusbar->update,f+': Invalid entry, value cannot be greater than range.' 
      state.historywin->update,f+': Invalid entry, cannot be greater than range.',/dontshow
    endif else begin
      axisSettings->setProperty,firstannotation=firstannotation
    endelse
  endif else begin
    state.statusbar->update, 'Invalid "'+f+'", value not applied.' 
    state.historywin->update, 'Invalid "'+f+'" value, no changes made.',/dontshow
  endelse
 
end

PRO thm_ui_axis_options_setvalues, tlb, state
  
    compile_opt idl2, hidden
    
  ; Get current scaling
  thm_ui_axis_options_set_scaling, tlb, state

  ; Get current range settings and apply
  thm_ui_axis_options_set_range, tlb, state
  
  ;get current tick settings and apply
  thm_ui_axis_options_set_ticks, tlb,state

  ; Get current label settings and apply
  thm_ui_axis_options_set_labels, tlb, state
  
  ;Get current annotation settings and apply
  thm_ui_axis_options_set_annotations,tlb,state
  
  return

END ;--------------------------------------------------------------------------------



PRO thm_ui_axis_options_update_labels, tlb, axissettings, currlabelobj, labelcolorwindow

    compile_opt idl2, hidden
  
axissettings->GetProperty, orientation=orientation, margin = margin
*currlabelobj->GetProperty, font=font, format=format, size=size, color=color, value=value, show=show

;Text Options
  id = widget_info(tlb, find_by_uname='labeltextedit')
  widget_control, id, set_value=value

  id = widget_info(tlb, find_by_uname='labelfont')
  widget_control, id, set_combobox_select=font
  
  id = widget_info(tlb, find_by_uname='labelformat')
  widget_control, id, set_combobox_select=format
  
  id = widget_info(tlb, find_by_uname='labelsize')
  widget_control, id, set_value=size

;Show
  id = widget_info(tlb, find_by_uname='showlabel')
  widget_control, id, set_button=show

;Color window
  Widget_Control, labelcolorWindow, Get_Value=labelcolorWin
  labelcolorwin->getproperty, graphics_tree=scene
  
  if obj_valid(scene) then begin
    scene->remove,/all
    scene->setproperty, color=reform(color)
  endif else begin
    scene=obj_new('IDLGRSCENE', color=reform(color))
    labelcolorwin->setproperty, graphics_tree=scene
  endelse
  
  labelcolorWin->draw, scene
      

;Style
  id = widget_info(tlb, find_by_uname='labelhorizontal')
  widget_control, id, set_button=~orientation
  
  id = widget_info(tlb, find_by_uname='labelvertical')
  widget_control, id, set_button=orientation
  
  id = widget_info(tlb, find_by_uname='labelmargin')
  widget_control, id, set_value=margin

END ;---------------------------------------------------------



PRO thm_ui_axis_options_update_flt, tlb, axisSettings, state

    compile_opt idl2, hidden

  id = widget_info(tlb, find_by_uname='floatrange')
  if ~widget_info(id, /button_set) then return 

  axisSettings->getproperty, maxFixedRange=maxFixedRange, minFixedRange=minFixedRange

  id = widget_info(tlb, find_by_uname='floatingspan')
;  widget_control, id, get_value = floatingspan
  fslabel = widget_info(tlb, find_by_uname='fslabel')
  
  linid = widget_info(tlb,find_by_uname='linear')
  logid = widget_info(tlb,find_by_uname='log10')
  
  ;If center is < or = 0 then do not allow log scaling:
  ;****************************************************
  info = state.drawobject->getPanelInfo(state.axispanelselect)
  case state.axisselect of
    0: center = info.xcenter
    1: center = info.ycenter
  endcase
  if center le 0 && ~widget_info(linid,/button_set) then begin
    axisSettings->setProperty, scaling=0
    id = widget_info(tlb, find_by_uname='linear')
    widget_control, id, /set_button
    state.statusbar->Update, 'When center is < or = 0, log scaling is not allowed with floating center ranging.  Try auto or fixed ranging.'
    state.historyWin->Update, 'When center is < or = 0, log scaling is not allowed with floating center ranging.  Try auto or fixed ranging.'
    return
  endif

  ;Assign span according to scaling:
  ;*********************************
  ;
 
  
  if maxFixedRange ge minFixedRange then begin
     if widget_info(linid,/button_set) then begin
          widget_control, id, set_value = maxFixedRange - center
          widget_control, fslabel, set_value='Span: '
     endif else if widget_info(logid,/button_set) then begin
          widget_control, id, set_value = alog10(maxFixedRange)-alog10(center)
          widget_control, fslabel, set_value='Span (log()): '
     endif else begin
          widget_control, id, set_value = alog(maxFixedRange)-alog(center)
          widget_control, fslabel, set_value='Span (ln()): '
      endelse
  endif

  return
  
END ;------------------------------------------------



;+ 
;NAME:
; thm_ui_axis_options
;PURPOSE:
; User interface for modifying axis settings
;
;CALLING SEQUENCE:
; thm_ui_axis, gui_id
;
;INPUT:
; gui_id = the id number of the widget that calls this
;
;OUTPUT:
; 
;HISTORY:
;$LastChangedBy: aaflores $
;$LastChangedDate: 2010-02-16 14:16:04 -0800 (Tue, 16 Feb 2010) $
;$LastChangedRevision: 7286 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_axis_options.pro $
;
;---------------------------------------------------------------------------------


PRO thm_ui_axis_options_event, event

Compile_Opt idl2,hidden

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
                     /noname, /center, title='Error in Axis Options')
    Widget_Control, event.TOP, Set_UValue=state, /No_Copy
    widget_control, event.top,/destroy
    if widget_valid(x) && obj_valid(histobj) then begin 
      thm_gui_error,x,histobj
    endif
    RETURN
  ENDIF

currpanelobj = state.panelobjs[state.axispanelselect]

  ;kill request block

IF (Tag_Names(event, /Structure_Name) EQ 'WIDGET_KILL_REQUEST') THEN BEGIN  
  state.statusbar->Update, 'Widget kill request received...'
  state.historyWin->Update, 'Widget kill request received...'

  ;Call reset method on the settings object for every panel:
  ;*********************************************************
  ;
  
  (state.windowStorage->getActive())->reset
;  IF Obj_Valid(state.panelObjs[0]) THEN BEGIN
;    for i=0,n_elements(state.panelobjs)-1 do state.panelobjs[i]->reset
;  ENDIF
  ;IF Obj_Valid(state.infoaxissettings) THEN state.infoaxissettings->reset 
  state.statusbar->Update, 'Panels reset.'
  state.historyWin->Update, 'Panels reset.'

  state.drawObject->update, state.windowStorage, state.loadedData
  state.drawObject->draw
  state.scrollbar->update

  state.statusbar->Update, 'Active window refreshed.'
  state.historyWin->Update, 'Active window refreshed.'

  state.statusbar->Update, 'Closing Axis Options widget...
  state.historyWin->Update, 'Closing Axis Options widget...
  Print, 'Closing Axis Options widget...

  Print, 'widget killed' 
  Widget_Control, event.TOP, Set_UValue=state, /No_Copy
  Widget_Control, event.top, /Destroy
  RETURN      
ENDIF

 ;deal with tabs

IF (Tag_Names(event, /Structure_Name) EQ 'WIDGET_TAB') THEN BEGIN  
  Widget_Control, state.panelDroplists[event.tab], set_combobox_select = state.axispanelselect
  Widget_Control, event.TOP, Set_UValue=state, /No_Copy
  RETURN 
EndIF

 ; Get the instructions from the widget causing the event and
 ; act on them.

Widget_Control, event.id, Get_UValue=uval

;Get correct axis (once per event call):
;***************************************
IF ~Obj_Valid(currpanelobj) THEN currpanelobj=Obj_New("THM_UI_PANEL", 1)
case state.axisselect of
  0: currpanelobj->GetProperty,xaxis = axissettings
  1: currpanelobj->GetProperty,yaxis = axissettings
endcase
IF ~Obj_Valid(axissettings) THEN axissettings=Obj_New("THM_UI_AXIS_SETTINGS")
IF Size(uval, /Type) NE 0 THEN BEGIN

  state.historyWin->Update,'THM_UI_AXIS_OPTIONS: User value: '+uval, /dontshow

  CASE uval OF
    'APPLY': BEGIN
      thm_ui_axis_options_setvalues, state.tlb, state
      thm_ui_propagate_axis,state ;favoring a delayed axis propagation over immediate to simplify code
      
      state.drawObject->update, state.windowStorage, state.loadedData, error=draw_error
    
      if draw_error ne 0 then begin
;        if Obj_Valid(state.panelObjs[0]) then begin
;          for i=0,n_elements(state.panelobjs)-1 do state.panelobjs[i]->reset
;        endif
        (state.windowStorage->getActive())->reset
        ;if Obj_Valid(state.infoaxissettings) then state.infoaxissettings->reset
        thm_ui_init_axis_window, state = state
        state.drawObject->update, state.windowStorage, state.loadedData
        
        ;update fixed range to match the auto or floating range value
        state.historyWin->Update, 'Draw object update error, changes reset.'
      endif else begin
        thm_ui_update_axis_from_draw,state.drawObject,state.panels
      endelse
      
      thm_ui_init_axis_window, state = state
   
      state.drawObject->draw
      state.scrollbar->update
    
      state.historyWin->Update, 'Active window refreshed.'
    END
    'CANC': Begin
      ;Call reset method on the settings object for every panel:
      ;*********************************************************
      ;
;      IF Obj_Valid(state.panelObjs[0]) THEN BEGIN
;        for i=0,n_elements(state.panelobjs)-1 do state.panelobjs[i]->reset
;      ENDIF
      (state.windowStorage->getActive())->reset
     ; IF Obj_Valid(state.infoaxissettings) THEN state.infoaxissettings->reset 
      
      state.statusbar->Update, 'Panels reset.'
      state.historyWin->Update, 'Panels reset.'

      state.drawObject->update, state.windowStorage, state.loadedData
      state.drawObject->draw
      state.scrollbar->update
 
      state.statusbar->Update, 'Active window refreshed.'
      state.historyWin->Update, 'Active window refreshed.'

      state.statusbar->Update, 'Closing Axis Options widget...
      state.historyWin->Update, 'Closing Axis Options widget...
      Print, 'Closing Axis Options widget...

      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    END
    'OK': BEGIN
      ;Exit widget:
      ;************
      
      thm_ui_axis_options_setvalues, state.tlb, state
      thm_ui_propagate_axis,state ;favoring a delayed axis propagation over immediate to simplify code
      
      state.drawObject->update, state.windowStorage, state.loadedData, error=draw_error
   
      if draw_error ne 0 then begin
;        if Obj_Valid(state.panelObjs[0]) then begin
;          for i=0,n_elements(state.panelobjs)-1 do state.panelobjs[i]->reset
;        endif
        (state.windowStorage->getActive())->reset
       ; if Obj_Valid(state.infoaxissettings) then state.infoaxissettings->reset
        thm_ui_init_axis_window, state = state
        state.drawObject->update, state.windowStorage, state.loadedData
        ;update fixed range to match the auto or floating range value
        state.historyWin->Update, 'Draw object update error, changes reset.'
      endif else begin
        thm_ui_update_axis_from_draw,state.drawObject,state.panels
      endelse
      
      IF Obj_Valid(state.panelObjs[0]) THEN BEGIN
        for i=0,n_elements(state.panelobjs)-1 do state.panelobjs[i]->setTouched
      ENDIF
           
      state.drawObject->draw
      state.scrollbar->update

      state.statusbar->Update, 'Active window refreshed.'
      state.historyWin->Update, 'Active window refreshed.'
      Print, 'Active window refreshed.'

      state.statusbar->Update, 'Closing Axis Options widget...
      state.historyWin->Update, 'Closing Axis Options widget...
      Print, 'Closing Axis Options widget...

      Widget_Control, event.TOP, Set_UValue=state, /No_Copy
      Widget_Control, event.top, /Destroy
      RETURN
    END
    'TEMP':begin
    
      ;make sure internal state is updated before save
      thm_ui_axis_options_setvalues, state.tlb, state
      thm_ui_propagate_axis,state ;favoring a delayed axis propagation over immediate to simplify code
      IF Obj_Valid(state.panelObjs[0]) THEN BEGIN
        for i=0,n_elements(state.panelobjs)-1 do state.panelobjs[i]->setTouched
      ENDIF
      
       if obj_valid(state.panelobjs[state.axispanelselect]) then begin
    
         if state.axisselect eq 0 then begin
           state.template->setProperty,x_axis=axissettings->copy()
           axis_string = 'X'
         endif else begin
           state.template->setProperty,y_axis=axissettings->copy()
           axis_string = 'Y'
         endelse
         state.statusbar->update,'Saved Current '+axis_string+'-Axis Settings to Template'
         state.historywin->update,'Saved Current '+axis_string+'-Axis Settigns to Template'
       endif else begin
         state.statusbar->update,'Cannot save template. Needs a valid panel to save axis template.'
         state.historywin->update,'Cannot save template. Needs a valid panel to save axis template.'
       endelse
     end
;******************************************************************************
;  Annotation Options
;******************************************************************************
    'ANNOPANELDROPLIST': begin
      thm_ui_axis_options_setvalues, state.tlb, state 
      thm_ui_propagate_axis,state ;favoring a delayed axis propagation over immediate to simplify code
  
      state.axispanelselect = event.index
      currpanelobj = state.panelobjs[state.axispanelselect]
      thm_ui_init_axis_window , state = state
    end
    'AAUTO': if event.select eq 1 then axissettings->setproperty, AnnotateExponent=0 
    'ADBL': if event.select eq 1 then axissettings->setproperty, AnnotateExponent=1
    'AEXP': if event.select eq 1 then axissettings->setproperty, AnnotateExponent=2
    'LINEATZERO': BEGIN
      axissettings->SetProperty, lineatzero=event.select      
    END
    'SHOWDATE': BEGIN
      axissettings->SetProperty, showdate=event.select
      id = widget_info(state.tlb, find_by_uname='anodatebase')    
      if event.select then begin
        widget_control, id, /sensitive
      endif else begin
        widget_control, id, sensitive=0
      endelse    
    END
    'ANODATE1': BEGIN
      widget_control, event.id, get_value=value
      axissettings->SetProperty, datestring1=value 
    END
    'ANODATE2': BEGIN
      widget_control, event.id, get_value=value
      axissettings->SetProperty, datestring2=value   
    END
    'ANODATEPREVIEW': BEGIN
      axissettings->GetProperty, dateString1=datestring1, dateString2=datestring2 
      IF ~in_set(Obj_Valid(state.panelObjs),'0') THEN BEGIN
        info = state.drawObject->GetPanelInfo(state.axispanelselect)
        line1 = formatdate(info.xrange[0], datestring1, info.xscale)
        line2 = formatdate(info.xrange[0], datestring2, info.xscale)    
        preview = line1 + thm_ui_newline() + line2
        id = widget_info(state.tlb, find_by_uname='anodatepreviewtext')
      widget_control, id, set_value=preview
      ENDIF
    END
    'ANNOTATEAXIS': BEGIN
      axissettings->SetProperty, annotateaxis=event.select
      id = widget_info(state.tlb, find_by_uname='annoaxisbase')      
      if event.select then begin
        widget_control, id, /sensitive
      endif else begin
        widget_control, id, sensitive=0
      endelse      
   
    END
    'PLACEANNOTATION':BEGIN
      axissettings->SetProperty, placeannotation=event.index
      

      state.statusbar->Update, 'Annotation placement updated to "'+axissettings->GetPlacement(event.index)+'".'
      state.historyWin->Update, 'Annotation placement updated to "'+axissettings->GetPlacement(event.index)+'".'
    END
    'ANNOTATEMAJORTICKS': BEGIN
      axissettings->SetProperty, annotatemajorticks=event.select     
      id = widget_info(state.tlb, find_by_uname='annomajorbase')
      if event.select then begin
        widget_control, id, sensitive=0
      endif else begin
        widget_control, id, /sensitive
      endelse     
    
    END
    'ANNOTATEEVERY':BEGIN
      if event.valid then begin
        if event.value le 0 then state.statusBar->Update, 'Annotate Every must be greater than zero.'
        axissettings->SetProperty, annotateevery = event.value
        state.statusBar->Update, 'Annotate every value changed.'
      endif else state.statusBar->Update, 'Invalid annotate every value, please re-enter.'
    END
    'ANNOTATEUNITS':BEGIN
      axissettings->GetProperty, $
        annotateevery = annotateevery, $
        annotateUnits = annotateUnits
      
      annotateEvery = thm_ui_axis_options_convert_units(annotateEvery,annotateUnits)
      annotateEvery = thm_ui_axis_options_convert_units(annotateEvery,event.index,/fromSeconds)
      

      axissettings->SetProperty, annotateunits=event.index,annotateEvery=annotateEvery

      thm_ui_init_axis_window , state = state

      state.statusbar->Update, 'Annotation units updated to "'+axissettings->GetUnit(event.index)+'".'
      state.historyWin->Update, 'Annotation units updated to "'+axissettings->GetUnit(event.index)+'".'

    END
    'ANNOTATESTYLE': BEGIN
      axissettings->SetProperty, annotatestyle=event.index      
 
      state.statusbar->Update, 'Annotation style updated to "'+axissettings->GetAnnotationFormat(event.index)+'".'
      state.historyWin->Update, 'Annotation style updated to "'+axissettings->GetAnnotationFormat(event.index)+'".'
    END
    'ANNOTATEFIRSTTICK':BEGIN
      axissettings->SetProperty, annotatefirsttick = event.select      
    END
    'ANNOTATELASTTICK':BEGIN
      axissettings->SetProperty, annotatelasttick = event.select      
    END
    'ANNOHORIZONTAL': BEGIN
      axissettings->SetProperty, annotateorientation=~event.select      
    END
    'ANNOVERTICAL':BEGIN
      axissettings->SetProperty, annotateorientation=event.select     
    END
    'ANOFONTLIST': BEGIN
      axissettings->GetProperty, annotatetextobject = annotatetextobject
      annotatetextobject->SetProperty, font=event.index
      state.statusbar->Update, 'Annotation font updated to "'+annotatetextobject->GetFont(index=event.index)+'".'
      state.historyWin->Update, 'Annotation font updated to "'+annotatetextobject->GetFont(index=event.index)+'".'
    END
    'ANOFONTSIZE': BEGIN
      if event.valid then begin
        axissettings->GetProperty, annotatetextobject = annotatetextobject      
        annotatetextobject->SetProperty, size=event.value
        state.statusBar->Update, 'Annotation font size changed.'
      endif else state.statusBar->Update, 'Invalid annotation font size, please re-enter.'
    END    
    'ANNOTATIONPALETTE': BEGIN
      ;THM_UI_PALETTE_EVENT, state.tlb, state.anocolorWin, color
      axissettings->GetProperty,annotatetextobject=annotatetextobject
      annotatetextobject->GetProperty,color=currentcolor
      color = PickColor(!P.Color, Group_Leader=state.tlb, Cancel=cancelled, $
                          currentcolor=currentcolor)      
      if cancelled then color=currentcolor                          
      if obj_valid(annotatetextobject) then annotatetextobject->SetProperty,color=color else begin
        annotatetextobject = obj_new('thm_ui_text')
        annotatetextobject->SetProperty,color=color
        axissettings->SetProperty,annotatetextobject=annotatetextobject
      endelse      
      Widget_Control, state.anocolorWindow, Get_Value=anocolorWin
      if obj_valid(scene) then scene->remove,/all
      scene=obj_new('IDLGRSCENE', color=reform(color))
      anocolorWin->draw, scene      
    END
    'ANNOTATIONSETALL': 
;******************************************************************************
;  End Annotation Options
;******************************************************************************


;******************************************************************************
;  Grid Panel Options
;******************************************************************************
    'GRIDPANELDROPLIST': begin
      thm_ui_axis_options_setvalues, state.tlb, state
      thm_ui_propagate_axis,state ;favoring a delayed axis propagation over immediate to simplify code
  
      state.axispanelselect = event.index
      currpanelobj = state.panelobjs[state.axispanelselect]
      thm_ui_init_axis_window , state = state
    end
    'OUTLINETHICK': BEGIN
      if event.valid then begin
        currpanelobj->GetProperty, settings = panelsettings
        if event.value lt 0 then state.statusbar->update, 'Frame thickness should be greater than zero.'
        panelsettings->SetProperty, framethick=event.value
        state.statusBar->Update, 'Outline thickness changed.'
      endif else state.statusBar->Update, 'Invalid outline thickness, please re-enter.' 
    END
    'MAJORGRIDPALETTE': begin
      axissettings->GetProperty,majorgrid=majorgrid
      IF ~Obj_Valid(majorgrid) THEN majorgrid=Obj_New("THM_UI_LINE_STYLE")
      majorgrid->GetProperty,color=currentcolor

      color = PickColor(!P.Color, Group_Leader=state.tlb, Cancel=cancelled, $
                          currentcolor=currentcolor)
      
      if cancelled then color=currentcolor
                          
      if obj_valid(majorgrid) then majorgrid->SetProperty,color=color else begin
        majorgrid = obj_new('THM_UI_LINE_STYLE')
        majorgrid->SetProperty,color=color
        axissettings->SetProperty,majorgrid=majorgrid
      endelse
      
      Widget_Control, state.majorgridcolorWindow, Get_Value=majorgridcolorWin
      if obj_valid(scene) then scene->remove,/all
      scene=obj_new('IDLGRSCENE', color=reform(color))
      majorgridcolorWin->draw, scene 

    end
    'MAJORGRIDS': BEGIN
      axissettings->GetProperty, majorgrid=majorgrid
      IF ~Obj_Valid(majorgrid) THEN majorgrid=Obj_New("THM_UI_LINE_STYLE")
      majorgrid->SetProperty, show=event.select     
      if event.select then begin
        id = widget_info(state.tlb, find_by_uname = 'majorbase')
        widget_control, id, /sensitive
      endif else begin
        id = widget_info(state.tlb, find_by_uname = 'majorbase')
        widget_control, id, sensitive=0
      endelse      
 
    END
    'MAJORGRIDSTYLE': BEGIN

      axissettings->GetProperty, majorgrid=majorgrid
      IF ~Obj_Valid(majorgrid) THEN majorgrid=Obj_New("THM_UI_LINE_STYLE")
      styleNames = majorgrid->getlinestyles()     
      majorgrid->SetProperty, Id=event.index, Name=styleNames[event.index]
print, event.index, ' - ', styleNames[event.index]
      state.statusbar->Update, 'Major grid line style updated to "'+majorgrid->GetLineStyleName(linestyleid=event.index)+'".'
      state.historyWin->Update, 'Major grid line style updated to "'+majorgrid->GetLineStyleName(linestyleid=event.index)+'".'
    END
    'MAJORGRIDTHICK': BEGIN
      if event.valid then begin
        axissettings->GetProperty, majorgrid=majorgrid
        IF ~Obj_Valid(majorgrid) THEN majorgrid=Obj_New("THM_UI_LINE_STYLE")
        if event.value le 0 then begin
          state.statusBar->update, 'Grid thickness should be greater than zero.'
        endif else begin 
          majorgrid->SetProperty, thickness=event.value
          state.statusBar->Update, 'Major grid thickness changed.'
        endelse
      endif else state.statusBar->Update, 'Invalid major grid thickness, please re-enter.'
    END
    'MINORGRIDPALETTE': begin
      axissettings->GetProperty,minorgrid=minorgrid
      IF ~Obj_Valid(minorgrid) THEN minorgrid=Obj_New("THM_UI_LINE_STYLE")
      minorgrid->GetProperty,color=currentcolor
      color = PickColor(!P.Color, Group_Leader=state.tlb, Cancel=cancelled, $
                          currentcolor=currentcolor)      
      if cancelled then color=currentcolor                          
      if obj_valid(minorgrid) then minorgrid->SetProperty,color=color else begin
        minorgrid = obj_new('thm_ui_line_style')
        minorgrid->SetProperty,color=color
        axissettings->SetProperty,minorgrid=minorgrid
      endelse      
      Widget_Control, state.minorgridcolorWindow, Get_Value=minorgridcolorWin
      if obj_valid(scene) then scene->remove,/all
      scene=obj_new('IDLGRSCENE', color=reform(color))
      minorgridcolorWin->draw, scene 
    
    end
    'MINORGRIDS': BEGIN
      axissettings->GetProperty, minorgrid=minorgrid
      IF ~Obj_Valid(minorgrid) THEN minorgrid=Obj_New("THM_UI_LINE_STYLE")
      minorgrid->SetProperty, show=event.select 
      if event.select then begin
        id = widget_info(state.tlb, find_by_uname = 'minorbase')
        widget_control, id, /sensitive
      endif else begin
        id = widget_info(state.tlb, find_by_uname = 'minorbase')
        widget_control, id, sensitive=0
      endelse
   
    END
    'MINORGRIDSTYLE': BEGIN
      axissettings->GetProperty, minorgrid=minorgrid
      IF ~Obj_Valid(minorgrid) THEN minorgrid=Obj_New("THM_UI_LINE_STYLE")
      styleNames = minorgrid->getlinestyles()
      minorgrid->SetProperty, Id=event.index, Name=styleNames[event.index]

      state.statusbar->Update, 'Minor grid line style updated to "'+minorgrid->GetLineStyleName(linestyleid=event.index)+'".'
      state.historyWin->Update, 'Minor grid line style updated to "'+minorgrid->GetLineStyleName(linestyleid=event.index)+'".'
    END
    'MINORGRIDTHICK': BEGIN
      if event.valid then begin
        axissettings->GetProperty, minorgrid=minorgrid
        IF ~Obj_Valid(minorgrid) THEN minorgrid=Obj_New("THM_UI_LINE_STYLE")     
        if event.value le 0 then begin
           state.statusBar->update, 'Grid thickness should be greater than zero.'
        endif else begin
          minorgrid->SetProperty, thickness=event.value
          state.statusBar->Update, 'Minor grid thickness changed.'
        endelse
      endif else state.statusBar->Update, 'Invalid minor grid thickness, please re-enter.'
    END
    'GRIDSETALL':

;******************************************************************************
;  End Grid Panel Options
;******************************************************************************



;******************************************************************************
;  Labels Options
;******************************************************************************
    'LABELPANELDROPLIST': begin
     thm_ui_axis_options_setvalues, state.tlb, state
     thm_ui_propagate_axis,state ;favoring a delayed axis propagation over immediate to simplify code
  
      state.axispanelselect = event.index
      currpanelobj = state.panelobjs[state.axispanelselect]
      state.labelselect=0
      thm_ui_init_axis_window , state = state
    end
    'BLACKLABELS': BEGIN
       axissettings->SetProperty, BlackLabels=event.select  
       id = widget_info(state.tlb, find_by_uname='labelpalette')
       if event.select eq 1 then widget_control, id, sensitive=0 $
         else widget_control, id, sensitive=1
     END
     
    'STACKLABELS': axissettings->SetProperty, stacklabels=event.select      

    'SHOWLABELS': BEGIN
      axissettings->SetProperty, showlabels=event.select      
      id = widget_info(state.tlb, find_by_uname = 'showlabel')
      widget_control, id, sensitive=event.select
      id = widget_info(state.tlb, find_by_uname = 'labeldroplist')
      widget_control, id, sensitive=event.select
    END
    'SHOWLABEL': BEGIN
      if ptr_valid(state.currLabelobj) then begin
        *state.currLabelObj->setProperty,show=event.select
      endif
      thm_ui_axis_options_update_labels, state.tlb, axissettings, state.currlabelobj, state.labelcolorwindow
    END
    'LABELDROPLIST': BEGIN     
      thm_ui_axis_options_set_labels, state.tlb, state

      if event.index eq -1 then begin  
        IF ~Obj_Valid(*state.currlabelobj) THEN BEGIN
           currlabelobj=Obj_New("THM_UI_TEXT", Value=' ') 
           state.currlabelObj=Ptr_New(currlabelobj)
        ENDIF            
        *state.currlabelobj->SetProperty, value=event.str
      endif else begin
        state.labelselect = event.index       
        axissettings->GetProperty,labels=labels
        if obj_valid(labels[0]) then begin          
          currlabelobj = labels->get(position = state.labelselect)
          if ptr_valid(state.currlabelobj) then ptr_free, state.currlabelobj
          state.currlabelobj = ptr_new(currlabelobj)          
        endif else begin          
          if ptr_valid(state.currlabelobj) then ptr_free, state.currlabelobj
        endelse
      endelse      
      
      thm_ui_axis_options_update_labels, state.tlb, axissettings, state.currlabelobj, state.labelcolorwindow
    END
    'LABELTEXTEDIT': BEGIN
      widget_control, event.id, get_value = value
    
      if stregex(value[0], '^ *$', /boolean) then begin
        show = 0
        placeholder = ''
        for i=0, state.labelselect do placeholder += ' ' ;equal entries not distinguished on linux
        value[0] = placeholder   ;combobox cannot take empty string
      endif else begin
        show = 1
      endelse

      id = widget_info(state.tlb, find_by_uname='labeldroplist')
      widget_control, id, get_value = labels

      labels[state.labelselect] = value[0]

      widget_control, id, set_value = labels
      widget_control, id, set_combobox_select=state.labelselect
      
      ;reset to empty string before altering label object
      if show eq 0 then value[0] = ''

      *state.currLabelObj->setProperty, show=show, value=value[0]
      
      id = widget_info(state.tlb, find_by_uname='showlabel')
      widget_control, id, set_button=show
    END
    'ADDLABEL': BEGIN
      axissettings->GetProperty, labels=labelsObj      
      if obj_valid(labelsObj) then begin
        labelsObj->add,obj_new('thm_ui_text',value='New Text Label',font=0,format=3, $
                               size=10,color=[0,0,0])
      endif else begin
        labelsObj = obj_new('IDL_Container')
        labelsObj->add,obj_new('thm_ui_text',value='New Text Label',font=0,format=3, $
                               size=10,color=[0,0,0])
      endelse      
      axissettings->SetProperty, labels=labelsObj
      state.labelselect = labelsObj->Count() - 1      
      currlabelobj = labelsObj->get(position = state.labelselect)
      if ptr_valid(state.currlabelobj) then ptr_free, state.currlabelobj
      state.currlabelObj = ptr_new(currlabelobj)
   
      thm_ui_init_axis_window , state = state      
    END
    'LABELFONT': ;Replaced by thm_ui_axis_options_set_labels
    'LABELFORMAT': ;Replaced by thm_ui_axis_options_set_labels
    'LABELSIZE': ;Replaced by thm_ui_axis_options_set_labels
    'LABELPALETTE': BEGIN
      IF Ptr_Valid(state.currlabelobj) THEN *state.currlabelobj->GetProperty,color=currentcolor $ 
         ELSE currentcolor=[0,0,0]
      color = PickColor(!P.Color, Group_Leader=state.tlb, Cancel=cancelled, $
                          currentcolor=currentcolor)     
      if cancelled then color=currentcolor
                     
      Widget_Control, state.labelcolorWindow, Get_Value=labelcolorWin
      labelcolorwin->getproperty, graphics_tree=scene
      if obj_valid(scene) then begin
        scene->remove,/all
        scene->setproperty, color=reform(color)
      endif else begin
        scene=obj_new('IDLGRSCENE', color=reform(color))
        labelcolorwin->setproperty, graphics_tree=scene
      endelse
      labelcolorWin->draw, scene      
    
    END
    'LABELHORIZONTAL': BEGIN
      axissettings->SetProperty, orientation=~event.select     
 
    END
    'LABELVERTICAL': BEGIN
      axissettings->SetProperty, orientation=event.select  
  
    END
    'LABELMARGIN': BEGIN
      if event.valid then begin
        axissettings->SetProperty, margin=event.value
        state.statusBar->Update, 'Label margin changed.'
      endif else state.statusBar->Update, 'Invalid label margin, please re-enter.'
    END
    'LABELTSETALL': ;Replaced by thm_ui_axis_options_set_labels

;******************************************************************************
;  End Labels Options
;******************************************************************************


;******************************************************************************
;  Range Options
;******************************************************************************
    'RANGEPANELDROPLIST': begin
      
      thm_ui_axis_options_setvalues, state.tlb, state
      thm_ui_propagate_axis,state ;favoring a delayed axis propagation over immediate to simplify code
     
      state.axispanelselect = event.index
      currpanelobj = state.panelobjs[state.axispanelselect]

      thm_ui_init_axis_window , state = state
      
    end
    'AUTORANGE': begin

      ;Update AXISSETTINGS:
      ;********************
      ;
  
      ;Sensitize Range Options:
      ;************************
      ;
      id = widget_info(state.tlb, find_by_uname = 'aobase')
      widget_control, id, sensitive = 1 
      ;Desensitize Floating Center and Fixed Min/Max:
      ;**********************************************
      ;
      id = widget_info(state.tlb, find_by_uname = 'fltobase')
      widget_control, id, sensitive = 0
      id = widget_info(state.tlb, find_by_uname = 'fobase')
      widget_control, id, sensitive = 0
   

    end
    'FLOATRANGE': begin

      ;Desensitize Range Options:
      ;**************************
      ;
      id = widget_info(state.tlb, find_by_uname = 'aobase')
      widget_control, id, sensitive = 0 
  
      ;Sensitize Floating Center:
      ;**************************
      ;
      id = widget_info(state.tlb, find_by_uname = 'fltobase')
      widget_control, id, sensitive = 1
    
      thm_ui_axis_options_update_flt, state.tlb, axissettings, state
  
      id = widget_info(state.tlb, find_by_uname = 'fobase')
      widget_control, id, sensitive = 0
     

    end
    'FIXEDRANGE': begin

      ;Desensitize Range Options:
      ;**************************
      ;
      id = widget_info(state.tlb, find_by_uname = 'aobase')
      widget_control, id, sensitive = 0
   
      id = widget_info(state.tlb, find_by_uname = 'fltobase')
      widget_control, id, sensitive = 0
     
      id = widget_info(state.tlb, find_by_uname = 'fobase')
      widget_control, id, sensitive = 1
     

    end
    'BOUNDSCALING': begin 
;       ID1 = widget_info(event.top, find_by_uname='minboundrange')
;       ID2 = widget_info(event.top, find_by_uname='maxboundrange')
;       widget_control,ID1,sensitive=event.select
;       widget_control,ID2,sensitive=event.select
       
       id = widget_info(event.top, find_by_uname='boundbase')
       widget_control,id,sensitive=event.select

    end
    'ISTIME': BEGIN

      ;make sure axis settings settings are updated
      thm_ui_axis_options_setvalues, state.tlb, state

      ;Get state of istime button and store in AXISSETTINGS:
      ;*****************************************************
      ;
        
      istime = Widget_Info(event.id, /Button_Set)
    ;  IF ~Obj_Valid(axissettings) THEN axissettings = Obj_New("THM_UI_AXIS_SETTINGS")
      
      axisSettings->getProperty,$
          istimeaxis=istimeold,$
          annotateUnits=annotateUnits,$
          firstAnnotateUnits=firstAnnotateUnits,$
          majorTickUnits=majorTickUnits,$
          minorTickUnits=minorTickUnits,$
          firstTickUnits=firstTickUnits,$
          annotateEvery=annotateEvery,$
          firstAnnotation=firstAnnotation,$
          majorTickEvery=majorTickEvery,$
          minorTickEvery=minorTickEvery,$
          firsttickAt=firstTickAt
          
      ;default units to hours when axis is time
      ;not entirely necessary, but this prevents users from accidentally creating
      ;plots with thousands of ticks or annotations
      if istime eq 1 && istimeold eq 0 then begin
;        axissettings->setProperty, annotateUnits = 2
;        axissettings->setProperty, majorTickUnits = 2
;        axissettings->setProperty, minortickUnits = 2


               
        annotateEvery = thm_ui_axis_options_convert_units(annotateEvery,annotateUnits,/fromseconds)
        firstAnnotation = thm_ui_axis_options_convert_units(firstAnnotation,firstAnnotateUnits,/fromseconds) 
        majorTickEvery = thm_ui_axis_options_convert_units(majorTickEvery,majorTickUnits,/fromseconds) 
        minorTickEvery = thm_ui_axis_options_convert_units(minorTickEvery,minorTickUnits,/fromseconds)
        firstTickAt = thm_ui_axis_options_convert_units(firstTickAt,firstTickUnits,/fromseconds)
        

      endif else begin
               
        annotateEvery = thm_ui_axis_options_convert_units(annotateEvery,annotateUnits)
        firstAnnotation = thm_ui_axis_options_convert_units(firstAnnotation,firstAnnotateUnits) 
        majorTickEvery = thm_ui_axis_options_convert_units(majorTickEvery,majorTickUnits) 
        minorTickEvery = thm_ui_axis_options_convert_units(minorTickEvery,minorTickUnits)
        firstTickAt = thm_ui_axis_options_convert_units(firstTickAt,firstTickUnits)
        
       endelse
      
      axissettings->SetProperty,$
        istimeaxis=istime,$
        annotateEvery=annotateEvery,$
        firstAnnotation=firstAnnotation,$
        majorTickEvery=majorTickEvery,$
        minorTickEvery=minorTickEvery,$
        firsttickAt=firstTickAt

      thm_ui_init_axis_window,event.top,state=state

    END
    'LINEAR': begin

     ; axissettings->SetProperty,scaling= 0
      
      thm_ui_axis_options_update_flt, state.tlb, axisSettings, state
      id = widget_info(state.tlb,find_by_uname='logminorticktypebase')
      widget_control,id,sensitive=0

    end
    'LOG10': begin

;      axissettings->SetProperty,scaling= 1

      thm_ui_axis_options_update_flt, state.tlb, axisSettings, state
      id = widget_info(state.tlb,find_by_uname='logminorticktypebase')
      widget_control,id,sensitive=1

    end
    'NATURALLOG': begin

 ;     axissettings->SetProperty,scaling= 2
      
      thm_ui_axis_options_update_flt, state.tlb, axisSettings, state
      id = widget_info(state.tlb,find_by_uname='logminorticktypebase')
      widget_control,id,sensitive=1   

    end
     
     ;Range Bounds Replaced with thm_ui_axis_options_set_range
 
    'FLOATINGCENTER': begin

      axissettings->SetProperty, floatingcenter = event.index

      state.statusbar->Update, 'Floating Center updated to "'+axissettings->GetFloatingCenter(event.index)+'".'
      state.historyWin->Update, 'Floating Center updated to "'+axissettings->GetFloatingCenter(event.index)+'".'
    end
    'EQUALXYSCALING': begin

      axissettings->SetProperty, equalxyscaling = widget_info(event.id, /button_set)
    end
;******************************************************************************
;  End Range Options
;******************************************************************************


;******************************************************************************
;  Tick Options
;******************************************************************************
    'TICKPANELDROPLIST': begin
    
      thm_ui_axis_options_setvalues, state.tlb, state
      thm_ui_propagate_axis,state ;favoring a delayed axis propagation over immediate to simplify code
    
      state.axispanelselect = event.index
      currpanelobj = state.panelobjs[state.axispanelselect]

      thm_ui_init_axis_window , state = state
    end

    ;several major tick events replaced by setvalue

    'MAJORTICKUNITS': BEGIN
    
      id = widget_info(state.tlb,find_by_uname='majortickevery')
      widget_control,id,get_value=majorTickEvery
            
      majorTickEvery = thm_ui_axis_options_convert_units(majorTickEvery,state.majorUnits)
      majorTickEvery = thm_ui_axis_options_convert_units(majorTickEvery,event.index,/fromSeconds)
      state.majorUnits = event.index

      widget_control,id,set_value=majorTickEvery
  
      state.statusbar->Update, 'Major tick units updated to "'+axissettings->GetUnit(event.index)+'".'
      state.historyWin->Update, 'Major tick units updated to "'+axissettings->GetUnit(event.index)+'".'
    END

    'BYNUMBER': begin
      bid = widget_info(state.tlb, find_by_uname='byinterval')
      id = widget_info(state.tlb, find_by_uname='intervalbase')
      widget_control, bid, set_button=0
      widget_control, id, sens=0
      
      id = widget_info(state.tlb, find_by_uname='numberbase')
      widget_control, id, sens=1 
      id = widget_info(state.tlb, find_by_uname='niceticks')      
      If widget_info(id, /button_set) then widget_control, state.minorTickBase, sens=0 else widget_control, state.minorTickBase, sens=1 
    end

    'NICETICKS': begin
      id = widget_info(state.tlb, find_by_uname='niceticks')      
      If widget_info(id, /button_set) then widget_control, state.minorTickBase, sens=0 else widget_control, state.minorTickBase, sens=1 
     end

    'BYINTERVAL': begin
      bid = widget_info(state.tlb, find_by_uname='bynumber')
      id = widget_info(state.tlb, find_by_uname='numberbase')
      widget_control, bid, set_button=0
      widget_control, id, sens=0
      
      id = widget_info(state.tlb, find_by_uname='intervalbase')
      widget_control, id, sens=1
      widget_control,state.minorTickBase,sensitive=1
    end

;******************************************************************************
;  End Tick Options
;******************************************************************************

    'TPALETTE': BEGIN

      THM_UI_PALETTE_EVENT, state.tlb, state.tcolorWin, color
      state.axisSettings->GetProperty, Labels=labels
      ; **********   NEED TO FIGURE OUT WHICH LABEL THIS IS ******
      ; **********   Should know by index  ************
      labelObj->SetProperty, Color=color
    END


    ELSE:
  ENDCASE
ENDIF
  
Widget_Control, event.TOP, Set_UValue=state, /No_Copy
  
RETURN
END ;--------------------------------------------------------------------------------



PRO thm_ui_axis_options, gui_id, windowStorage, loadedData, drawObject, historyWin, $
                         windowTitle, axisselect, scrollbar, template,$
                         panel_select=panel_select

compile_opt idl2

err_x=0
catch, err_x
if err_x ne 0 then begin
  help, /Last_Message, Output=err_msg
  for j = 0, N_Elements(err_msg)-1 do historywin->update,err_msg[j]
  ok = error_message('An unknown error occured starting Axis Options.', /noname, $
                     /center, title='Error in Axis Options')
  widget_control, tlb, /destroy
  thm_gui_error, gui_id, historywin
  return
endif
  

  ;top level and main bases
  
tlb = Widget_Base(/Col, Title=windowTitle, Group_Leader=gui_id, /Modal, /Floating,/tlb_kill_request_events)
tabBase = Widget_Tab(tlb, Location=location, uname = 'tabs', tab_mode=1)
buttonBase = Widget_Base(tlb, /Row, /align_center, tab_mode=1)
statusBase = Widget_Base(tlb, /Row, /align_center)
rangeBase = Widget_Base(tabBase, Title='Range', /Col)    
ticksBase = Widget_Base(tabBase, Title='Ticks', /Col)
gridBase = Widget_Base(tabBase, Title='Grid', /Col)
annotationBase = Widget_Base(tabBase, Title='Annotation', /Col)
labelsBase  = Widget_Base(tabBase, Title='Labels', /Col)

    ;range bases
    
panelBase = Widget_Base(rangeBase, /Row)
rangeBase = Widget_Base(rangeBase, /Row)
  range1Base = Widget_Base(rangeBase, /Col)
    rlabelBase = Widget_Base(range1Base, /Col)
    roptionsBase = Widget_Base(range1Base, /Col, Frame=3)
    slabelBase = Widget_Base(range1Base, /Col)
    soptionsBase = Widget_Base(range1Base, /Col, Frame=3)
    flabelBase = Widget_Base(range1Base, /Col)
    foptionsBase = Widget_Base(range1Base, /Col, Frame=3, uname='fobase')
    timeXYBase = Widget_Base(range1Base, /Col, /NonExclusive, YPad=2)
    lockMSGbase = widget_base(range1Base, /row, ypad=1)
  range2Base = Widget_Base(rangeBase, /Col)
    alabelBase = Widget_Base(range2Base, /Col, uname='alabelbase')
    aoptionsBase = Widget_Base(range2Base, /Col, Frame=3, YPad=6, uname='aobase')
    fclabelBase = Widget_Base(range2Base, /Col)
    fcoptionsBase = Widget_Base(range2Base, /Col, Frame=3, YPad=6, uname='fltobase')
setAllBase = Widget_Base(range2Base, /Col, /Align_Center, YPad=15, /nonexclusive)
;setAllBase = Widget_Base(range2Base, /Col, /Align_Center, YPad=6)

    ;ticks bases
  
tpanelBase = Widget_Base(ticksBase, ypad=15, /Row)
ticksMainBase = Widget_Base(ticksBase, /Col)
  ticksTopBase = Widget_Base(ticksMainBase, /row, ypad=5, /align_left)
  ticksMiddleBase = widget_base(ticksMainBase, /col, ypad=1)
  ticksBottomBase = Widget_Base(ticksMainBase, /Row,/align_left)
    plcmntBase = Widget_Base(ticksBottomBase, /Col, /Align_Left, ypad=5);,xpad=3)
    lengthBase = Widget_Base(ticksBottomBase, /Col, /Align_Left, xpad=3, ypad=5)
  ticksButtonBase = Widget_Base(ticksMainBase, /Row, /Align_Center, YPad=10, /nonexclusive)

   ;grid panel bases
   
  gpBase = Widget_Base(gridBase, /Col)
  grid1Base = Widget_Base(gridBase, /Col, XPad=4)
    majorBase = Widget_Base(grid1Base, /Col, YPad=1, xpad=20)
      mgLabelBase = Widget_Base(majorBase, /Row)
      majFrameBase = Widget_Base(majorBase, /Col, Frame=3, XPad=15, YPad=1, /align_center, uname='majorbase')
      mdLabelBase = Widget_Base(majFrameBase, /Row)
      dirPullBase =  Widget_Base(majFrameBase, /Col, YPad=1)
    minorBase = Widget_Base(grid1Base, /Col, YPad=1, XPad=20)
      mgiLabelBase = Widget_Base(minorBase, /Row, YPad=1)
      minFrameBase = Widget_Base(minorBase, /Col, Frame=3, YPad=1, XPad=15, /align_center, uname='minorbase')
      mdiLabelBase = Widget_Base(minFrameBase, /Row)
      diriPullBase =  Widget_Base(minFrameBase, /Col, YPad=1)
  gcBase = Widget_Base(grid1Base, /Col)
  gaBase = Widget_Base(gridBase, /Col, /Align_Center, YPad=1, /nonexclusive)
      
    ;annotation bases  
    
anoPanelBase = Widget_Base(annotationBase, /Row)
anoMainBase = Widget_Base(annotationBase, /Col)
  anoTopBase = Widget_Base(anoMainBase, /row, space=0, ypad=0) 
  anoTopCol1Base = Widget_Base(anoTopBase, space=0, ypad=0, /Col)
  anoTopCol2Base = Widget_Base(anoTopBase, space=0, ypad=0, /Col)
  anoMiddleBase = Widget_Base(anoMainBase, /Col, ypad=0, space=0)
anoButtonBase = Widget_Base(anoMainBase, /Row, /Align_Center, /nonexclusive)

    ;labels bases

lpanelBase = Widget_Base(labelsBase, /col, YPad=3)
;stackBase = Widget_Base(labelsBase, /nonexclusive, /row)
labelMainBase = Widget_Base(labelsBase, /Col) 
  labelTextBase = Widget_Base(labelMainBase, /Col, YPad=1) 
;  labelPositionBase = Widget_Base(labelMainBase, /Col) 
  stackBase = Widget_Base(labelMainBase, /nonexclusive, /row)
  labelButtonBase = Widget_Base(labelMainBase, /Row, /Align_Center, YPad=3, /nonexclusive)
     
    ;axis settings were passed in
    ;save here in case reset must be called
;infoaxissettings->save 
     
    ;retrieve data and panel info for display

activeWindow = windowStorage->GetActive()
IF ~Obj_Valid(activeWindow) THEN BEGIN
  panelNames=['No Panels']  
  windowlocked = -1
ENDIF ELSE BEGIN
  activeWindow->GetProperty, Panels=panels,locked=windowlocked
  IF ~Obj_Valid(panels) THEN BEGIN
    panelNames=['No Panels']  
  ENDIF ELSE BEGIN
    panelObjs = panels->Get(/all)
    IF Is_Num(panelObjs) THEN BEGIN
      panelNames=['No Panels'] 
    ENDIF ELSE BEGIN
      FOR i=0, N_Elements(panelObjs)-1 DO BEGIN
        activeWindow->getproperty, locked = locked
        lockPrefix = i eq locked ? '(L)  ':''
	      name = lockPrefix + panelObjs[i]->constructPanelName()
	      IF i EQ 0 THEN panelNames=[name] ELSE panelNames=[panelNames, name]
      ENDFOR
    ENDELSE
  ENDELSE
  IF Is_Num(panelNames) THEN panelNames=['No Panels'] 
  IF N_Elements(panelNames) EQ 1 && panelNames EQ '' THEN panelNames=['No Panels']
ENDELSE

activeWindow->save

;Save all existing panels in current window:
;*******************************************
;
;if ~(~size(panelobjs,/type)) && obj_valid(panelobjs[0]) then begin
;  for i=0,n_elements(panelobjs)-1 do panelobjs[i]->save
;endif

;If 0th PANELOBJ is valid, then assume the AXISPANELSELECT-th PANELOBJ is valid and get AXISSETTINGS for correct axis:
;*********************************************************************************************************************
;
axispanelselect = 0

if obj_valid(panelobjs[0]) then begin

  if n_elements(panel_select) gt 0 && panel_select ge 0 && panel_select lt n_elements(panelObjs) then begin
    axispanelselect = panel_select
  endif else begin
    if windowlocked ne -1 then begin
      axispanelselect = windowlocked
    endif
  endelse

  case axisselect of
    0: panelobjs[axispanelselect]->GetProperty,xaxis=axissettings
    1: panelobjs[axispanelselect]->GetProperty,yaxis=axissettings
    ;2: panelobjs[axispanelselect]->GetProperty,zaxis=axissettings ;axis options window couldn't possibly work with z axis
  endcase
  IF ~Obj_Valid(axissettings) THEN axissettings = obj_new('thm_ui_axis_settings')
endif else axissettings = obj_new('thm_ui_axis_settings')

axisSettings->GetProperty,       $
  Scaling=scaleIndex,             $
  RangeOption=rangeIndex,         $
  IsTimeAxis=isTimeAxis,          $
  EqualXYScaling=equalXYScaling,  $
  MinFixedRange=minFixedRange,    $
  MaxFixedRange=maxFixedRange,    $
  RangeMargin=rangeMargin,        $
  BoundScaling=boundScaling,      $
  MinBoundRange=minBoundRange,    $
  MaxBoundRange=maxBoundRange,    $
  FloatingSpan=floatingSpan,      $
  FloatingCenter=floatingCenter,  $
  MajorTickEvery=majorTickEvery,  $
  MajorTickUnits=majorTickUnits,  $
  MajorTickAuto=majorTickAuto,    $     
  MinorTickEvery=minorTickEvery,  $
  MinorTickUnits=minorTickUnits,  $
  MinorTickAuto=minorTickAuto,    $
  NumMajorTicks=numMajorTicks,    $
  NumMinorTicks=numMinorTicks,    $
  FirstTickAt=firstTickAt,        $
  FirstTickUnits=firstickUnits,   $     
;  FirstTickAuto=firstTickAuto,    $
  AnnotateExponent=AnnotateExponent
  

;Kludge to make sure that RANGEMARGIN is at 5% if it got initialized to 0 for some reason:
;*****************************************************************************************
;
;if rangemargin eq 0 and ~istimeaxis then begin
;  rangemargin = 0.05d
;  axissettings->SetProperty, rangemargin=rangemargin
;endif
      
    ;range widgets

rpdlabel = Widget_label(panelbase, value = 'Panels: ', /align_center)
rangepanelDroplist = Widget_combobox(panelBase, Value=panelNames, XSize=200, uval='RANGEPANELDROPLIST', uname='rangepaneldroplist')
roptionsLabel = Widget_Label(rlabelBase, Value='Range Options:', /Align_Left)
rbuttonsBase = Widget_Base(roptionsBase, /Col, /Exclusive)
;IF isTimeAxis EQ 1 THEN sensitive=0 ELSE sensitive=1
sensitive = ~istimeaxis
rangeOptions = Make_Array(3, /long)
rangeOptions[0] = Widget_Button(rbuttonsBase, Value='Auto Range', Sensitive=sensitive, UValue='AUTORANGE',uname='autorange')
rangeOptions[1] = Widget_Button(rbuttonsBase, Value='Floating Center', Sensitive=sensitive, UValue='FLOATRANGE', uname ='floatrange')
rangeOptions[2] = Widget_Button(rbuttonsBase, Value='Fixed Min/Max', Sensitive=sensitive, UValue='FIXEDRANGE',uname='fixedrange')
WIDGET_CONTROL, rangeOptions[rangeIndex], /Set_Button
soptionsLabel = Widget_Label(slabelBase, Value='Scaling:', /Align_Left)
sbuttonsBase = Widget_Base(soptionsBase, /Col, /Exclusive)
scalingOptions = Make_Array(3, /long)
scalingOptions[0] = Widget_Button(sbuttonsBase, Value='Linear', Sensitive=sensitive, uval = 'LINEAR', uname='linear')
scalingOptions[1] = Widget_Button(sbuttonsBase, Value='Log 10', Sensitive=sensitive, uval = 'LOG10',uname='log10')
scalingOptions[2] = Widget_Button(sbuttonsBase, Value='Natural Log', Sensitive=sensitive, uval = 'NATURALLOG',uname='logn')
WIDGET_CONTROL, scalingOptions[scaleIndex], /Set_Button
equalXYButton = Widget_Button(timeXYBase, Value='Equal X & Y Axis Scaling', uval = 'EQUALXYSCALING', uname='equalxyscaling')
IF equalXYScaling EQ 1 THEN Widget_Control, equalXYButton, /Set_Button
widget_control,equalxybutton,sensitive=0
isTimeButton = Widget_Button(timeXYBase, Value='Is Time', UValue='ISTIME',uname='istime')
lockMSG = widget_label(lockmsgbase, value='', uvalue = 'lockmsg')
IF isTimeAxis EQ 1 THEN Widget_Control, isTimeButton, /Set_Button
IF rangeIndex EQ 2 THEN sensitive=1 ELSE sensitive=0
foptionsLabel = Widget_Label(flabelBase, Value='Fixed Min/Max:', /Align_Left)
minBase = Widget_Base(foptionsBase, /Row, /align_right)
maxBase = Widget_Base(foptionsBase, /Row, /align_right)
minIncLabel=widget_label(minbase, Value='Min: ')
maxIncLabel=widget_label(maxbase, Value='Max: ')

if istimeaxis then begin
;  minFixedRangeTime = formatDate(minFixedRange, '%date/%time', 0)
  minIncrement=widget_text(minBase, Sensitive=sensitive, /editable, uval='MINFIXEDRANGE', uname='minincrement',/all_events)

;  maxFixedRangeTime = formatDate(maxFixedRange, '%date/%time', 0)
  maxIncrement=widget_text(maxBase, Sensitive=sensitive, /editable, uval='MAXFIXEDRANGE', uname='maxincrement',/all_events)
endif else begin
  minIncrement=thm_ui_spinner(minBase, Increment=1, Sensitive=sensitive, Value=minFixedRange, text_box_size=12, $
                              uval='MINFIXEDRANGE', uname='minincrement',precision=16)
  maxIncrement=thm_ui_spinner(maxBase, Increment=1, Sensitive=sensitive, Value=maxFixedRange, text_box_size=12, $
                              uval='MAXFIXEDRANGE', uname='maxincrement',precision=16)
endelse

aoptionsLabel = Widget_Label(alabelBase, Value='Auto Range:', /Align_Left)
rmBase = Widget_Base(aoptionsBase, /Row)
IF rangeIndex EQ 0 THEN sensitive=1 ELSE sensitive=0
;rmIncrement=thm_ui_spinner(rmBase, Increment=0.1, Label='Range Margin (%): ', Sensitive=sensitive, Value=100*rangeMargin, uval = 'RANGEMARGIN', $
;  uname='rmincrement')
rmLabel=Widget_Label(rmBase, value='Range Margin (%): ', xsize=120, /align_left, uname='rmlabel')
rmIncrement=thm_ui_spinner(rmBase,increment=1, Value=200*rangeMargin, uval = 'RANGEMARGIN', $
                             uname='rmincrement')
autoBase = Widget_Base(aoptionsBase, /NonExclusive);, Sensitive=sensitive) 
autoButton = Widget_Button(autoBase, value = ' Bound autoscaling range', uname='boundscaling', uval='BOUNDSCALING')
IF boundScaling THEN BEGIN 
  Widget_Control, autoButton, /Set_Button
  sensitive=1
ENDIF else sensitive = 0
;Widget_Control, rmLabel, sensitive=sensitive
boundbase = widget_base(aoptionsBase,uname='boundbase',/col)
minaBase = Widget_Base(boundbase, /Row)
minaLabel=Widget_Label(minaBase, value='Minimum: ', /align_left,  uname='minlabel', xsize=96)
minaIncrement=thm_ui_spinner(minaBase, Increment=1, Value=minBoundRange, $
                             text_box_size=12, uval = 'MINBOUNDRANGE', uname='minboundrange')
maxaBase = Widget_Base(boundbase, /Row)
maxaLabel=Widget_Label(maxaBase, value='Maximum: ', /align_left, xsize=96, uname='maxlabel')
mmaxaIncrement=thm_ui_spinner(maxaBase, Increment=1, Value=maxBoundRange, $
                              text_box_size=12, uval = 'MAXBOUNDRANGE', uname='maxboundrange')
minmaxLabel = Widget_Label(boundbase, Value='(Not applied if min/max are equal)')
spaceLabel = Widget_Label(fclabelBase, Value='    ', /Align_Left)
fcoptionsLabel = Widget_Label(fclabelBase, Value='Floating Center:', /Align_Left)
IF rangeIndex EQ 1 THEN sensitive=1 ELSE sensitive=0

spanBase = Widget_Base(fcoptionsBase, /Row)
fslabel=widget_label(spanbase, value = 'Span: ', xsize=76, uname = 'fslabel')
floatingspan=thm_ui_spinner(spanBase, Increment=1, Value=floatingspan, uval='FLOATINGSPAN', $
  uname='floatingspan', tooltip='If logarithmic Max/Min = 10^[log(center) +- span]', $
  text_box_size=12)
cspaceBase = Widget_Base(fcoptionsBase, /Row, YPad=2)
centerOptions = axisSettings->GetFloatingCenters()

flclabel = widget_label(cspacebase, value = 'Center: ', xsize=76)
floatingcenterid = Widget_combobox(cspaceBase, Value=centeroptions, uval='FLOATINGCENTER', $
  uname='floatingcenter')

Widget_Control, floatingcenterid, Set_combobox_Select=floatingCenter
;spaceLabel = Widget_Label(setAllBase, Value='  ')
;spaceLabel = Widget_Label(setAllBase, Value='  ')
setAllButton = Widget_Button(setAllBase, Value='Set All Panels', /Align_Center, XSize = 120, uval='RANGESETALL', uname='rangesetall')

;if windowlocked && axisselect eq 0 then begin
;  widget_control,setAllButton,/set_button
;endif

    ;TICKS WIDGETS 
    ;------------

tpanellabel = widget_label(tpanelbase, value = 'Panels: ', /align_left)
tpanelDroplist = Widget_combobox(tpanelBase, Value=panelNames, XSize=200, $
                                 uval='TICKPANELDROPLIST', uname='tickpaneldroplist')

tickUnitValues = axisSettings->GetUnits()
tickStyleValues = axisSettings->GetStyles()

;Main Bases for Type of ticks
bynumberBase = widget_base(ticksTopBase, /col)
byintervalBase = widget_base(ticksTopBase, /col)


;'By Interval' Widgets

iLabelBase = widget_base(byintervalBase, /row, /exclusive)
  byinterval = widget_button(ilabelbase, value='Major Ticks By Interval', uvalue='BYINTERVAL', $
                             uname='byinterval')

intervalbase = widget_base(byintervalBase, /col, /base_align_left, uname='intervalbase', $
                           frame=1, space=6, ypad=6, xpad=4)

  tickEvery = thm_ui_spinner(intervalBase, label='Major Tick Every:   ', getXLabelSize=xlsize, $
                         text_box_size=12, uname='majortickevery', incr=1, $
                         tooltip='Inverval in Units between major tick marks')

  tickUnitsBase = widget_base(intervalBase, /row, xpad=0, ypad=0, space=1)
    unitslabel = widget_label(tickUnitsBase, value='Units', xsize=xlsize)
    unitslist = widget_combobox(tickUnitsBase, value=tickUnitValues, uval='MAJORTICKUNITS', $
                                uname='majortickunits')

  firsttickBase = widget_base(intervalBase, /row, xpad=0, space=0,uname='firsttickatbase')
    firstticklabel = widget_label(firsttickBase, value='Align ticks at: ', xsize=xlsize+3)
    if istimeaxis then begin
      firsttick = widget_text(firsttickBase, value=time_string(firsttickat), uname='firsttickat',/editable)
    endif else begin
      firsttick = thm_ui_spinner(firsttickBase, value=firsttickat, incr=1, uname='firsttickat', $
                                 tooltip='Numerical location of first tick.', text_box=12)
    endelse


;'By Number' Widgets

numLabelBase = widgeT_base(bynumberBase, /Col, /exclusive)
  bynumber = widget_button(numlabelBase, value='Major Ticks By Number', uvalue='BYNUMBER', $
                           uname='bynumber')

numberbase = widget_base(bynumberBase, /col, /base_align_left, uname='numberbase', $
                         frame=1, space=6, ypad=6, xpad=4)
  MajorTicknum = thm_ui_spinner(numberbase, value=nummajorticks, getXlabelSize=xlsize, $
                                uname='nummajorticks', label='Major Ticks:  ', incr=1)
  niceticksbase = widget_base(numberbase, /row, /nonexclusive, uname='niceticksbase')
    niceticks = widget_button(niceticksbase, value='Aesthetic Ticks', uvalue='NICETICKS', uname='niceticks', $
                              tooltip='Places ticks at easily read values. '+$
                              'May cause desired number to be approximate.')

  ;resize bynumberbase
  widget_control, numberbase, scr_ysize=( widget_info(intervalbase, /geo) ).scr_ysize


;initializations
widget_control, unitslist, Set_combobox_Select=majorTickUnits

widget_control, bynumber, set_button=1
widget_control, numberbase, sens=1

widget_control, byinterval, set_button=0
widget_control, intervalbase, sens=0


minorTickBase = widget_base(ticksMiddleBase, /row, sens=0)
  MinorTicknum = thm_ui_spinner(minorTickBase, value=numminorticks, getXlabelSize=xlsize, $
                              uname='numminorticks', label='# of Minor Ticks:  ', incr=1)

minorLogTickTypeBase = widget_base(ticksMiddleBase,/col,/frame,/base_align_center)
minorLogTickTypeLabelBase = widget_base(minorLogTickTypeBase,/base_align_center,/row)
minorLogTickTypeLabel = widget_label(minorLogTickTypeLabelBase,value="Log Minor Tick Type:",/align_center)
minorLogTickTypeButtonBase = widget_base(minorLogTickTypeBase,/exclusive,/row,uname='logminorticktypebase')
minorLogTickType0 = widget_button(minorLogTickTypeButtonBase,value='Full Interval',uname='logminorticktype0')
minorLogTickType1 = widget_button(minorLogTickTypeButtonBase,value='First Magnitude',uname='logminorticktype1')
minorLogTickType2 = widget_button(minorLogTickTypeButtonBase,value='Last Magnitude',uname='logminorticktype2')
minorLogTickType3 = widget_button(minorLogTickTypeButtonBase,value='Even Spacing',uname='logminorticktype3')

tickstyleBase = Widget_Base(ticksMiddleBase, /row, ypad=3)
  tslabel = widget_label(tickstylebase, value = 'Draw Ticks: ', xsize=xlsize+1)
  tickStyleDroplist = Widget_combobox(tickstyleBase, Value=tickStyleValues, uval='TICKSTYLE',uname='tickstyle')

placementLabel = Widget_Label(plcmntBase, Value = 'Placement', /Align_Left)  
placeFrameBase = Widget_Base(plcmntBase, /Col, /NonExclusive, Frame=3, YPad=11, XPad=10)

case axisselect of
  0:BEGIN
    botbutval = 'Bottom              '
    topbutval = 'Top                 '
  END
  1:BEGIN
    botbutval = 'Left                '
    topbutval = 'Right               '
  END
endcase
  bottomButton = Widget_Button(placeFrameBase, Value=botbutval, uval='BOTTOMPLACE', $
                               uname='bottomplace')
  topButton = Widget_Button(placeFrameBase, Value=topbutval, uval='TOPPLACE', $
                            uname='topplace')
WIDGET_CONTROL, bottomButton, /Set_Button
WIDGET_CONTROL, topButton, /Set_Button 
lengthLabel = Widget_Label(lengthBase, Value = 'Length', /Align_Left)  
lengthFrameBase = Widget_Base(lengthBase, /Col, Frame=3)
lmajorBase = Widget_Base(lengthFrameBase, /Row, YPad=3)
lmajorIncrement = thm_ui_spinner(lmajorBase, label = 'Major : ',Increment=1, uval='MAJORLENGTH', $
                                   uname='majorlength')
lmajorLabel= Widget_Label(lmajorBase, Value=' pts')
lminorBase = Widget_Base(lengthFrameBase, /Row, YPad=3)
lminorIncrement = thm_ui_spinner(lminorBase, label = 'Minor : ',Increment=1, uval='MINORLENGTH', $
                                   uname='minorlength')
lminorLabel = Widget_Label(lminorBase, Value=' pts')  

tsetAllButton = Widget_Button(ticksButtonBase, Value='Set All Panels', /Align_Center, XSize = 120, uval='TICKSSETALL', uname='tickssetall')   
  
if windowlocked ne -1 && axisselect eq 0 then begin
  widget_control,tsetallbutton,/set_button
endif 
  
    ;GRID WIDGETS
    ;------------

gppanelbase = widget_base(gpbase, /row)
gplabel = widget_label(gppanelbase, value='Panels: ')
gpDroplist = Widget_combobox(gppanelBase, Value=panelNames, XSize=200, uval='GRIDPANELDROPLIST', uname='gridpaneldroplist') 
outlineBase = Widget_Base(gpBase, /Row)
outlineIncrement = thm_ui_spinner(outlineBase, Label= 'Panel Frame Thickness: ', Increment=1, Value=1, $
                                    uval='OUTLINETHICK', uname='outlinethick')
gmajorLabel = Widget_Label(mglabelBase, Value='Major Grids: ', /Align_left)
gmajorbuttonbase = Widget_Base(mglabelBase, /NonExclusive)
gmajorbutton = Widget_Button(gmajorbuttonbase, value='', uval='MAJORGRIDS', uname='majorgrids')
currentBase =  Widget_Base(dirPullBase, /Row, XPad=1)
paletteBase = Widget_Base(dirPullBase, /Row, XPad=1)
colorLabel = Widget_Label(paletteBase, Value='Color: ', /align_left, xsize=100, uname='majorcolorlabel')

getresourcepath,rpath
palettebmp = read_bmp(rpath + 'color.bmp', /rgb)
thm_ui_match_background, tlb, palettebmp

majorgridpaletteButton = Widget_Button(paletteBase, Value=palettebmp, /Bitmap, $
                                       UValue='MAJORGRIDPALETTE', uname='majorgridpalette', $ 
                                       Tooltip='Choose color from Palette')
spaceLabel = Widget_Label(paletteBase, Value=' ')    
;majorgridcolorWindow = Widget_Draw(paletteBase, XSize=50, YSize=21, uname='majorgridcolorwindow')
majorgridcolorWindow = WIDGET_DRAW(paletteBase,graphics_level=2,renderer=1, $
                                   retain=1, XSize=50, YSize=20, units=0, frame=1, /expose_events)
cmajorBase = Widget_Base(dirpullBase, /Row)
cmajorlabel=widget_label(cmajorBase, value='Thickness: ', /align_left, uname='majorgridlabel', xsize=100)
cmajorIncrement = thm_ui_spinner(cmajorBase, Increment=1, Value=1,xsize=120, $
                                   uval='MAJORGRIDTHICK', uname='majorgridthick')
smajorbase = widget_base(dirpullbase, /row)
smajorlabel = widget_label(smajorbase, value='Style: ', xsize=100, /align_left, uname='majorstylelabel')
lineObj=Obj_New("THM_UI_LINE_STYLE")
lineStyles=lineObj->GetLineStyles()
Obj_Destroy, lineObj
smajorDroplist = Widget_combobox(smajorbase, XSize=157, $
                                 uval='MAJORGRIDSTYLE', uname='majorgridstyle', $
                                 Value=lineStyles)
gimajorLabel = Widget_Label(mgilabelBase, Value = 'Minor Grids: ', /Align_Left)
gminorbuttonbase = Widget_Base(mgilabelBase, /NonExclusive)
gminorbutton = Widget_Button(gminorbuttonbase, value='', uval='MINORGRIDS', uname='minorgrids')
cminorBase =  Widget_Base(diripullBase, /Row, XPad=1)
;cminorLabel = Widget_Label(cminorBase, Value='                                  Current Color:')
paletteiBase = Widget_Base(diripullBase, /Row, XPad=1)
coloriLabel = Widget_Label(paletteiBase, Value='Color: ' ,/align_left, uname='minorclabolorel', xsize=100)
minorgridpaletteButton = Widget_Button(paletteiBase, Value=palettebmp, /Bitmap, $
                         UValue='MINORGRIDPALETTE', uname='minorgridpalette', $
                         Tooltip='Choose color from Palette')
spaceLabel = Widget_Label(paletteiBase, Value=' ')
;minorgridcolorWindow = WIDGET_DRAW(paletteiBase, XSize=50, YSize=21, uname='minorgridcolorwindow')
minorgridcolorWindow = WIDGET_DRAW(paletteiBase,graphics_level=2,renderer=1, $
                                   retain=1, XSize=50, YSize=20, units=0, frame=1, /expose_events)
cminorBase = Widget_Base(diripullBase, /Row)
cmajorlabel=widget_label(cminorBase, value='Thickness: ', /align_left, uname='minorgridlabel', xsize=100)
cminorIncrement = thm_ui_spinner(cminorBase,Increment=1, Value=1, $
                                    uval='MINORGRIDTHICK', uname='minorgridthick')
sminorbase = widget_base(diripullbase, /row)
sminorlabel=widget_label(sminorBase, value='Style: ', /align_left, uname='minorstylelabel', xsize=100)
sminorDroplist = Widget_combobox(sminorbase, XSize=160, $
                                 uval='MINORGRIDSTYLE', uname='minorgridstyle', $
                                 Value=lineStyles)
;spaceLabel = Widget_Label(gaBase, Value='            ')  ;annoying IDL problem
setAllgButton = Widget_Button(gaBase, Value='Set All Panels', /Align_Center, XSize=125, uval='GRIDSETALL', uname='gridsetall')
	       
if windowlocked ne -1 && axisselect eq 0 then begin
  widget_control,setAllgButton,/set_button
endif
	       
    ;annotation widgets

anoPlabel = widget_label(anopanelbase, value = 'Panels: ')
anoPanelDroplist = Widget_combobox(anopanelBase, Value=panelNames, XSize=200, uval='ANNOPANELDROPLIST', uname='annopaneldroplist')

IF isTimeAxis EQ 1 THEN sensitive=1 ELSE sensitive=0

anoDrawBase = Widget_Base(anoTopCol1Base, /col, /NonExclusive)
anoDrawButton = Widget_Button(anoDrawBase, Value='Draw Line at Zero (1 for log)', uval='LINEATZERO', $
                              uname='lineatzero')
;anoShowButton = Widget_Button(anoShowDateBase, Value='Show Date', Sensitive=sensitive, $
;anoShowBase = Widget_Base(anoTopCol1Base, /row, /nonexclusive)
;anoftBase = Widget_Base(anoTopCol1Base, /col, /NonExclusive)
anoftButton =  Widget_Button(anoDrawBase, Value='Annotate first', uval='ANNOTATEFIRSTTICK', $
                             uname='annotatefirsttick')
WIDGET_CONTROL, anoftButton, /Set_Button
;ano2ftBase = Widget_Base(anoftBase, /Row, /NonExclusive)
ano2ftButton =  Widget_Button(anoDrawBase, Value='Annotate last', uval='ANNOTATELASTTICK', $
                             uname='annotatelasttick')
WIDGET_CONTROL, ano2ftButton, /Set_Button    
anoShowButton = Widget_Button(anoDrawBase, Value='Show Date', Sensitive=sensitive, $
                              uval='SHOWDATE', uname='showdate')
WIDGET_CONTROL, anoShowButton, /Set_Button 

anoShowDateBase = Widget_Base(anoMiddleBase, /Row, /NonExclusive)
anoDateBase = Widget_Base(anoMiddleBase, /Row, frame=1, uname='anodatebase')
  anoDateTextBases = Widget_Base(anoDateBase, /col)
    anoDateFormat1Base = Widget_Base(anoDateTextBases, /row)
    anoDateFormat2Base = Widget_Base(anoDateTextBases, /row)
  anoDatePreviewTextBase = Widget_Base(anoDateBase, /col)
  
anoDateFormat1Label = Widget_Label(anoDateFormat1Base, Value='Line 1: ')
anoDateFormat1 = Widget_Text(anoDateFormat1Base, /editable, uval='ANODATE1', uname='anodate1', /All_Events)
anoDateFormat2Label = Widget_Label(anoDateFormat2Base, Value='Line 2: ')
anoDateFormat2 = Widget_Text(anoDateFormat2Base, /editable, uval='ANODATE2', uname='anodate2', /All_Events)

anoDatePreviewText = Widget_Text(anoDatePreviewTextBase, ysize=2, xsize=32, /wrap, uname='anodatepreviewtext')
anoDatePreviewButt = Widget_Button(anoDatePreviewTextBase, value='Preview', uval='ANODATEPREVIEW')

;anoDateButton = Widget_Button(anoTopCol2Base, Value='Date Format...', Sensitive=sensitive)
anoAxisBase = Widget_Base(anoMiddleBase, /Row, /NonExclusive)
anoAxisButton = Widget_Button(anoAxisBase, Value='Annotate Along Axis:', uval='ANNOTATEAXIS', $
                              uname='annotateaxis')
WIDGET_CONTROL, anoAxisButton, /Set_Button  

anoFrameBase = Widget_Base(anoMiddleBase, /Col, Frame=5, XPad=5, uname='annoaxisbase')
anofpBase = Widget_Base(anoFrameBase, /row)
;annoplacement = axissettings->GetPlacements()
case axisselect of
  0: annoplacement = ['Bottom','Top']
  1: annoplacement = ['Left','Right']
endcase
anoplabel = widget_label(anofpbase, value='Place Annotation on: ', /align_left, xsize=125)
anoPlaceDroplist = Widget_combobox(anofpBase, Value=annoplacement, uval='PLACEANNOTATION', uname='placeannotation')
anoMajorBase = Widget_Base(anofpBase, /Row, /NonExclusive)
anoMajorButton = Widget_Button(anoMajorBase, Value='Annotate Major Ticks', uval='ANNOTATEMAJORTICKS', $
                               uname='annotatemajorticks')
WIDGET_CONTROL, anoMajorButton, /Set_Button 
anomFrameBase = Widget_Base(anoFrameBase, /Col, XPad=4, uname='annomajorbase')
anoEveryBase = Widget_Base(anomFrameBase, /Row)
anoEveryLabel = Widget_Label(anoEveryBase, Value='Annotate Every: ', /align_left, xsize=101)
anoEveryText = thm_ui_spinner(anoEveryBase, Value=majortickevery, increment=1,$
                                text_box_size=12, uval='ANNOTATEEVERY', uname='annotateevery')
spaceLabel = Widget_Label(anoEveryBase, Value = '    ')
IF isTimeAxis EQ 1 THEN Sensitive=1 ELSE Sensitive=0
anoEveryDroplist = Widget_combobox(anoEveryBase, Value=tickUnitValues, xsize=120, uval='ANNOTATEUNITS', uname='annotateunits')
anoFirstBase = Widget_Base(anomFrameBase, /Row,uname='firstannotationbase')
anoFirstLabel = Widget_Label(anoFirstBase, Value='Align Annotations At: ' ,/align_left)
if istimeaxis then begin
  anoFirstText = widget_text(anoFirstBase, value=time_string(0), uname='firstannotation',/editable)
endif else begin
  anoFirstText = thm_ui_spinner(anoFirstBase, value=0, incr=1, uname='firstannotation', $
                             tooltip='Numerical location of first annotation.', text_box=12)
endelse

anoBase = Widget_Base(anoFrameBase, /row)
styleValues = axisSettings->GetAnnotationFormats()
anoSBase = widget_base(anoTopCol2Base, /row, space = 0)
avalue = istimeaxis ? 'Annotation Format:  ':'Annotation Precision:  '
anoSLabel = widget_label(anosbase, value = avalue)
anoStyleDroplist = Widget_combobox(anosbase, Value = styleValues, uval='ANNOTATESTYLE', uname='annotatestyle')

anoSOBase = widget_base(anoTopCol2Base, column=1, /exclusive, ypad=0, space=0)
default = widget_button(anoSOBase, value = 'Auto-Notation', uvalue='AAUTO', uname='aauto', sensitive=~istimeaxis)
dbl = widget_button(anoSOBase, value = 'Decimal Notation', uvalue='ADBL', uname='adbl', sensitive=~istimeaxis)
expo = widget_button(anoSOBase, value = 'Scientific Notation', uvalue='AEXP', uname = 'aexp', sensitive=~istimeaxis) 

atype = [default, dbl, expo]
widget_control, atype[annotateExponent], /set_button

;ano2Base = Widget_Base(anoFrameBase, /row)
;spaceLabel = Widget_Label(anoFrameBase, Value = '')
orientBase = Widget_Base(anoFrameBase, /row)
orientationLabel = Widget_Label(orientBase, Value=' Orientation: ', /Align_Left, xsize=125,sensitive=1)
orientButtonBase = Widget_Base(orientBase, /Exclusive, /row)
landscapeButton = Widget_Button(orientButtonBase, Value='Horizontal      ', UValue='ANNOHORIZONTAL', $
                                uname='annohorizontal');, sensitive=1)
landscapeButton = Widget_Button(orientButtonBase, Value='Vertical   ', UValue='ANNOVERTICAL', $
                                uname='annovertical');, sensitive=1)
anoSetAllButton = Widget_Button(anoButtonBase, Value='Set All Panels', /Align_Center, XSize = 125, uval='ANNOTATIONSETALL', uname='annotationsetall')
;spaceLabel = Widget_Label(anoFrameBase, Value = '')
;
;
;
if windowlocked ne -1 && axisselect eq 0 then begin
  widget_control,anoSetAllButton,/set_button
endif
         
;styleLabel = Widget_Label(anoFrameBase, Value=' Style:', /Align_Left, sensitive=1)
anoStyleBase = Widget_Base(anoFrameBase, /row)
anoFontBase = Widget_Base(anoStyleBase, /col)
textObj = Obj_New("THM_UI_TEXT")
fontNames = textObj->GetFonts()
anoFontLabel = Widget_Label(anoFontBase, value='Font', /align_left)
anoStyleDroplist = Widget_combobox(anoFontbase, value=fontNames, uval='ANOFONTLIST', uname='anofontlist')
anoIncBase = Widget_Base(anoStyleBase, /col)
anoIncLabel = Widget_Label(anoIncBase, value='Size (pts)', /align_left)
fontTitleIncrement = thm_ui_spinner(anoIncBase, Increment=1, uval='ANOFONTSIZE', $
                                      uname='anofontsize')
anoColorBase = Widget_Base(anoStyleBase, /col)
colorLabel = Widget_Label(anoColorbase, value='Color', /align_center)
;getresourcepath,rpath
;palettebmp = rpath + 'color.bmp'
annotationpaletteButton = Widget_Button(anoColorBase, Value=palettebmp, /Bitmap, $
                          UVal='ANNOTATIONPALETTE', uname='annotationpalette', $
                          Tooltip='Choose color from Palette')
anoCurrentBase = Widget_Base(anoStyleBase, /col)
anoCurrentLabel = Widget_Label(anocurrentbase, value='Current Color')
anoColorWindow = WIDGET_DRAW(anoCurrentBase, graphics_level=2, renderer=1, retain=1, XSize=50, $
                             YSize=20, units=0, frame=1, /expose_events)

    ;labels widgets

lpdBase = widget_base(lpanelbase, /row)
panelLabel=Widget_Label(lpdBase, value='Panels: ')
lpanelDroplist = Widget_combobox(lpdBase, Value=panelNames,  XSize=150, uval='LABELPANELDROPLIST', uname='labelpaneldroplist')  
ltextLabel = Widget_Label(labeltextBase, Value='Text: ', /Align_Left)
ltFrameBase = Widget_Base(labeltextBase, /col, Frame=3,XPad=1)
lt1TextBase = Widget_Base(ltFrameBase, /row, xpad=3) 
col1Base = Widget_Base(lt1TextBase, /col, /base_align_Center, ypad=5);, ypad=10)
lt1Text = Widget_Combobox(col1Base, Value = ' ', XSize=260, uval='LABELDROPLIST', uname='labeldroplist')

ltexteditBase = widget_base(col1base, /row, xpad=0, /align_left, space=3)
ltextedit = widget_text(ltexteditBase, value = '', /editable, /all_events, xsize=40, ysize=1, $
                        uval='LABELTEXTEDIT', uname='labeltextedit')

;uptoarrow = read_bmp(rpath + 'up_to_arrow.bmp', /rgb)
;thm_ui_match_background, tlb, uptoarrow
;ltextsync = widget_button(ltexteditBase, value = uptoarrow, /bitmap, uval='LABELTEXTSYNC', uname='labeltextsync')

col2Base = Widget_Base(lt1TextBase, /col, /nonexclusive, ypad=5)
showSingleLabelCheck = widget_button(col2Base,value='Show Label',UVALUE='SHOWLABEL',uname='showlabel')

;lt2Base = Widget_Base(ltFrameBase, /col, xpad=7, /align_left)
;lt3Base = Widget_base(lt2base, /row, /align_center)

ltFrameAttrBase = widget_base(ltFrameBase,/row)
lpcol1Base = Widget_Base(ltFrameAttrBase, /row)
lpcol2Base = Widget_Base(ltFrameAttrBase, /row)

;lt4Base = Widget_base(lt3Base, /row)

;lpFrameBase = Widget_Base(labelTextBase, /row, Frame=3, YPad=2, XPad=2)

;lpcol1Base = Widget_Base(lpFrameBase, /row)
lpcol1aBase = Widget_Base(lpcol1Base, /col, space=17)
lpcol1bBase = Widget_Base(lpcol1Base, /col, space=5)
lpcol2aBase = Widget_Base(lpcol2Base, /col, space=17)
lpcol2bBase = Widget_Base(lpcol2Base, /col, space=5)
;lpcol2Base = Widget_Base(lpFrameBase, /col)

lpstyleLabel = Widget_Label(labelTextBase, Value=' Style:', /Align_Left)

styleFrameBase= Widget_Base(labelTextBase, /col, Frame=3, YPad=2, XPad=2)

showLabelButtonBase = widget_base(styleFrameBase,/row,/nonexclusive,/align_left)
stackButton = Widget_Button(showLabelButtonBase, Value='Stack Labels', uval='STACKLABELS', uname='stacklabels')
showLabelButton = Widget_Button(showLabelButtonBase, Value='Show Labels', uval='SHOWLABELS', uname='showlabels')
stackButtonBase= widget_base(styleFrameBase,/row,/nonexclusive)
IF axisselect EQ 1 THEN blackButton = Widget_Button(stackButtonBase, Value='Set All Labels Black', UValue='BLACKLABELS', uname='blacklabels')
Widget_Control, showlabelButton, /Set_Button

lpGlobalBase = Widget_Base(styleFrameBase, /row)
lpoBase = Widget_Base(lpGlobalBase, /col)
lpoLabel = Widget_Label(lpoBase , Value='Orientation:  ', /align_left)
lpoButBase = Widget_Base(lpoBase , /col, /Exclusive, Frame=3, xpad=18)
lpovHorizButton = Widget_Button(lpoButBase, Value='Horizontal', uval='LABELHORIZONTAL', uname='labelhorizontal')
lpoVertButton = Widget_Button(lpoButBase, Value='Vertical', uval='LABELVERTICAL', uname='labelvertical')  
Widget_Control, lpovHorizButton, /Set_Button

lpoIncrBase = Widget_Base(lpGlobalBase, /Row, YPad=2,/align_center)
lpoIncrIncrement=thm_ui_spinner(lpoIncrBase, label = 'Margin: ',Increment=1, uval='LABELMARGIN', uname='labelmargin')
lpoIncrLabel = Widget_Label(lpoIncrBase, Value=' pts ')  

tlabel = Widget_Label(lpcol1aBase, value = 'Font:', /align_left)
lpofontDroplist = Widget_combobox(lpcol1bBase, XSize=125, Value=fontnames, uval='LABELFONT', uname='labelfont')
tlabel = Widget_Label(lpcol1aBase, value = 'Format:', /align_left)
formattypes=textObj->getformats()
Obj_Destroy, textObj

lpoFormatDroplist = Widget_combobox(lpcol1bBase, XSize=125, Value=formattypes, uval='LABELFORMAT', uname='labelformat')
tlabel= Widget_Label(lpcol2aBase, value='Size (points): ', /align_left)
fontIncrement = thm_ui_spinner(lpcol2bBase, Increment=1,  Value=12, uval='LABELSIZE', uname='labelsize')
cb1Base = Widget_Base(lpcol2bBase, /row, space=3)

tlabel= Widget_Label(lpcol2aBase, value='Color: ', /align_left)

labelpaletteButton = Widget_Button(cb1Base, Value=palettebmp, /Bitmap, UValue='LABELPALETTE', uname='labelpalette', $
                                   Tooltip='Choose color from Palette')
labelColorWindow = WIDGET_DRAW(cb1Base, graphics_level=2, renderer=1, retain=1, XSize=50, YSize=20, units=0, frame=1, $
                               uname = 'labelcolorwin', /expose_events)

labelsSetButton = Widget_Button(labelButtonBase, Value='Set All Panels', /Align_Center, XSize = 125, uval='LABELTSETALL', uname='labeltsetall')

			 
okButton = Widget_Button(buttonBase, Value='OK', XSize=75, uval='OK')
applyButton = Widget_Button(buttonBase, Value='Apply', UValue='APPLY', XSize=75)
cancelButton = Widget_Button(buttonBase, Value='Cancel', UValue='CANC', XSize=75)
templateButton = Widget_Button(buttonBase, Value='Save to Template', UValue='TEMP',xsize=125)
;helpButton = Widget_Button(buttonBase, Value='Help', XSize=75)
    
;Create Status Bar Object:
;*************************
;
panelDroplists=[rangepanelDroplist, tpanelDroplist, gpDroplist, anopanelDroplist, lpanelDroplist]
statusBar = Obj_New('THM_UI_MESSAGE_BAR', statusBase, XSize=68, YSize=1)
state = {tlb:tlb, gui_id:gui_id, tabbase:tabbase, rangeOptions:rangeOptions, $
  scalingOptions:scalingOptions, tcolorWin:0, labelselect:0, atype:atype, $
  axispanelselect:axispanelselect, panels:panels, panelobjs:panelobjs, $
  axisselect:axisselect, currlabelobj:ptr_new(), $ ;anocolorwin:-1, $
  ;labelcolorwin:-1, majorgridcolorwin:-1, minorgridcolorwin:-1, $
  windowStorage:windowStorage, loadedData:loadedData, drawObject:drawObject, $
  historyWin:historyWin, statusBar:statusBar, panelDroplists:panelDroplists, $
  majorgridcolorWindow:majorgridcolorWindow, minorgridcolorWindow:minorgridcolorWindow, $
  anocolorWindow:anocolorWindow, labelcolorWindow:labelcolorWindow, $
  minIncrement:minIncrement, maxIncrement:maxIncrement, minBase:minBase, maxBase:maxBase,$
  majorUnits:0,minorUnits:0,firstUnits:0, minorTickBase:minorTickBase, $
  scrollbar:scrollbar,template:template}

centerTLB, tlb

Widget_Control, tlb, Set_UValue=state, /No_Copy
Widget_Control, tlb, /realize
statusBar->Draw

drawObject->update,windowStorage,loadedData
thm_ui_update_axis_from_draw,drawObject,panels
thm_ui_init_axis_window, tlb

XManager, 'thm_ui_axis_options', tlb, /No_Block

RETURN
END ;--------------------------------------------------------------------------------
