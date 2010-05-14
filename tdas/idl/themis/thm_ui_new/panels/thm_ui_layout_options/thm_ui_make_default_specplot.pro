;+ 
;NAME:  
;  thm_ui_make_default_specplot
;  
;PURPOSE:
;  Routine that creates a default specplot on particular panel
;  
;CALLING SEQUENCE:
;  thm_ui_make_default_specplot, loadedData, panel, xvar, yvar, zvar, 
;                                gui_sbar=gui_sbar
;
;INPUT:
;  loadedData: the loadedData object
;  panel:  the panel on which the plot should be placed.
;  xvar:  the name of the variable storing the x-data.
;  yvar:  the name of the variable storing the y-data.
;  zvar:  the name of the variable storing the z-data.
;  gui_sbar: the main GUI status bar object
;            
;KEYWORDS:
;  none
;        
;OUTPUT:
;  none
;
;--------------------------------------------------------------------------------

;just eliminates some repetition in retrieving a data object
function thm_ui_default_specplot_get_data_object,loadedData,name

  compile_opt idl2, hidden
  
  name = name[0]
  
  if name eq '' then begin
    return,0
  endif else if loadedData->isParent(name) then begin ;sometimes name can be a group and data name, this makes sure we get the data
    group = loadedData->getGroup(name)
    if group->hasChild(name) then begin
      return,group->getObject(name)
    endif else begin
      return,(group->getDataObjects())[0]
    endelse
  endif else begin
    return,loadedData->getObjects(name=name)
  endelse
  
end

pro thm_ui_make_default_specplot, loadedData, panel, xvar, yvar, zvar,template, $
                                  gui_sbar=gui_sbar

compile_opt idl2, hidden

panel->GetProperty, traceSettings=traceSettings, xAxis=xAxis, yAxis=yAxis, $
                    zAxis=zAxis
;ntraces = traceSettings->count()

template->getProperty,z_axis=zAxisTemplate,x_axis=xAxisTemplate,y_axis=yAxisTemplate

; check if x-axis object exists, create if not
if ~obj_valid(xAxis) then xAxis = obj_new('thm_ui_axis_settings', $
                                  isTimeAxis=1, $
                                  orientation=0, $
                                  majorLength=7., $
                                  minorLength=3., $
                                  topPlacement=1, $
                                  bottomPlacement=1, $
                                  tickStyle=0, $
                                  majorTickAuto=1, $
                                  numMajorTicks=5, $
                                  minorTickAuto=1, $
                                  numMinorTicks=5, $
                                 ; majorGrid=majorGrid, $
                                 ; minorGrid=minorGrid, $
                                  dateString1="%date", $
                                  dateString2="%time", $
                                  showdate=1,$
                                  annotateAxis=1,$
                                  placeAnnotation=0,$
                                  AnnotateMajorTicks=1,$
                                  annotatefirstTick=0,$
                                  annotateLastTick=0,$
                                  annotateStyle=5,$
                                  annotateTextObject=obj_new('thm_ui_text',font=2,size=11.,color=[0,0,0]),$
                                  scaling=0,$
                                  rangeMargin=0., $
                                  margin=15., $
                                  annotateOrientation=0, $
                                  autoticks = 1 $
                                  ;labels=labels,$
                                  ;stacklabels=0 $
                                  ;rangeoption=2,$
                                  )

; check if y-axis object exists, create if not
if ~obj_valid(yAxis) then yAxis = obj_new('thm_ui_axis_settings', $
                                   isTimeAxis=0,$
                                   orientation=1,$
                                   majorLength=7.,$
                                   minorLength=3.,$
                                   topPlacement=1,$
                                   bottomPlacement=1,$
                                   tickStyle=0,$
                                   majorTickAuto=1,$
                                   numMajorTicks=5,$
                                   minorTickAuto=1,$
                                   numMinorTicks=8,$
                                   ;majorGrid=majorGrid,$
                                   ;minorGrid=minorGrid,$
                                   ;/lineatzero,$
                                   annotateAxis=1,$
                                   placeAnnotation=0,$
                                   AnnotateMajorTicks=1,$
                                   annotateLastTick=0,$
                                   annotateFirstTick=0,$
                                   annotateStyle=4,$
                                   annotateTextObject=obj_new('thm_ui_text',font=2,size=11.,color=[0,0,0]),$
                                   rangeOption=0,$
                                   rangeMargin=.00, $
                                   scaling=1, $
                                   annotateOrientation=1, $
                                   margin=45., $
                                   autoticks = 1 $
                                   ;labels=labels, $
                                   ;stackLabels=1 $
                                   )

; check if z-axis object exists, create z-axis and make sure y-axis is log
if ~obj_valid(zAxis) then begin
  
  xAxis->setProperty, annotateLastTick=0,/notouched
  
  if obj_valid(zAxisTemplate) then begin
    zAxis = zAxisTemplate->copy()
  endif else begin
  
    zAxis = obj_new('thm_ui_zaxis_settings', $
                     colorTable = 6,$
                     fixed = 0,$
                     tickNum = 5, $
                     minorTickNum = 8, $
                     labelmargin = 40, $
                     scaling = 1, $
                     annotationStyle = 4, $
                     margin = 5, $
                     annotateTextObject = obj_new('thm_ui_text',size=11.,font=2,format=3), $
                     annotationOrientation = 0, $
                     ;labelTextObject = obj_new('thm_ui_text',value='FLUX in eV'),$
                     labelOrientation=1, $
                     placement=3,$
                     autoticks=1 $
                     )
   endelse
endif else begin
  zAxis->getProperty,placement=placement
  if placement eq 4 then begin
    zAxis->setProperty,placement=3,/notouched
  endif
endelse

yAxis->setProperty, rangeOption=0, $
                     rangeMargin=.00, $
                     scaling=0, $
                     lineatzero=0,/notouched

yAxis->getProperty, labels=ylabels
if ~obj_valid(ylabels) then ylabels = obj_new('IDL_Container')
xAxis->getProperty, labels=xlabels
if ~obj_valid(xlabels) then xlabels = obj_new('IDL_Container')
                    
traceSettings->add,obj_new('thm_ui_spectra_settings', dataX=xvar, dataY=yvar, dataZ=zvar)

xobj = thm_ui_default_specplot_get_data_object(loadedData,xvar)
yobj = thm_ui_default_specplot_get_data_object(loadedData,yvar)
zobj = thm_ui_default_specplot_get_data_object(loadedData,zvar)

if obj_valid(xObj) && xvar[0] ne '' then begin
  xObj->getProperty, isTime=xisTime, settings=dataSettings
  
  if obj_valid(dataSettings) then begin
  
  ;if this quantity is a non-time axis, then we want to use the settings from the y-axis in tplot, because those apply to the dependent variable
;  if xIsTime then begin
;    thm_ui_process_axis_tags,xAxis,'x',xdlptr,xlptr

    xAxis->getproperty, minfixedrange=minfixedrange, maxfixedrange=maxfixedrange
  
    ; if fixed-range mode then set rangemargin=0
    if (dataSettings->getxRangeOption()) eq 2 then xAxis->setproperty,rangemargin=0,/notouched
  
    ; make sure xrange=[0,0] isn't used to determine fixed range
    if (minFixedRange eq maxFixedRange) then begin
      minfixedrange = (dataSettings->getxFixedRange())[0]
      maxfixedrange = (dataSettings->getxFixedRange())[1]
    endif else begin
      minfixedrange = min([minfixedrange,(dataSettings->getxFixedRange())[0]],/nan)
      maxfixedrange = max([maxfixedrange,(dataSettings->getxFixedRange())[1]],/nan)
    endelse
  
    xAxis->setproperty, minfixedrange=minfixedrange, maxfixedrange=maxfixedrange, $
                        rangeoption=dataSettings->getxRangeOption(), $
                        scaling=dataSettings->getxScaling(),/notouched
  endif

;  endif else begin
;    thm_ui_process_axis_tags,xAxis,'y',xdlptr,xlptr
;  endelse
endif else begin
  xistime = 0
endelse

if obj_valid(yObj) && yvar[0] ne '' then begin
  
  yObj->getProperty, isTime=yistime, settings=dataSettings
  
  if obj_valid(dataSettings) then begin
    yAxis->getproperty, minfixedrange=minfixedrange, maxfixedrange=maxfixedrange
    
    ; if fixed-range mode then set rangemargin=0
    if (dataSettings->getyRangeOption()) eq 2 then yAxis->setproperty,rangemargin=0,/notouched
  
    ; make sure yrange=[0,0] isn't used to determine fixed range
    if (minFixedRange eq maxFixedRange) then begin
      minfixedrange = (dataSettings->getyFixedRange())[0]
      maxfixedrange = (dataSettings->getyFixedRange())[1]
    endif else begin
      minfixedrange = min([minfixedrange,(dataSettings->getyFixedRange())[0]],/nan)
      maxfixedrange = max([maxfixedrange,(dataSettings->getyFixedRange())[1]],/nan)
    endelse
    
    yAxis->setproperty, minfixedrange=minfixedrange, maxfixedrange=maxfixedrange, $
                        rangeoption=dataSettings->getyRangeOption(), $
                        scaling=dataSettings->getyScaling(),/notouched
  endif
 
;  thm_ui_process_axis_tags,yAxis,'y',ydlptr,ylptr

endif else begin
 yistime = 0
endelse

if obj_valid(zObj) && zvar[0] ne '' then begin

  zObj->getProperty, settings=dataSettings
  
  if obj_valid(dataSettings) then begin
    zAxis->getproperty, minrange=minrange, maxrange=maxrange
    
    ; make sure zrange=[0,0] isn't used to determine fixed range
    if (minrange eq maxrange) then begin
      minrange = (dataSettings->getzRange())[0]
      maxrange = (dataSettings->getzRange())[1]
    endif else begin
      minrange = min([minrange,(dataSettings->getzRange())[0]],/nan)
      maxrange = max([maxrange,(dataSettings->getzRange())[1]],/nan)
    endelse
    
    zAxis->setproperty, minrange=minrange, maxrange=maxrange, $
                        fixed=dataSettings->getzFixed(), $
                        scaling=dataSettings->getzScaling(),/notouched
  endif

;  thm_ui_process_axis_tags,zAxis,'z',zdlptr,zlptr
  
endif
 
if obj_valid(dataSettings) then begin
  xlabeltext = dataSettings->getxlabel()
  ylabeltext = dataSettings->getylabel()
  zlabeltext = dataSettings->getzlabel()
  
  if dataSettings->getUseColor() then begin
    ycolor = dataSettings->getcolor()
  endif
endif else begin
  xlabeltext = ''
  ylabeltext = zvar
  zlabeltext = ''
endelse

yfont = 2
yformat = 3
ysize = 11

if obj_valid(yAxisTemplate) then begin

  yAxisTemplate->getProperty,labels=yTlabels
  
  if obj_valid(yTlabels) && obj_isa(yTlabels,'IDL_Container') then begin
  
    yTlabel = yTlabels->get()
  
    if obj_valid(yTlabel) then begin
  
      yTlabel->getProperty,font=yfont,format=yformat,size=ysize
      
      if ~keyword_set(ycolor) then begin
        yTlabel->getProperty,color=ycolor
      endif
  
    endif
  
  endif

endif

if ~keyword_set(ycolor) then begin
  ycolor = [0,0,0]
endif

ylabels->add,obj_new('thm_ui_text', value=ylabeltext, font=yfont, format=yformat, size=ysize, $
                    color=ycolor,show=1)
     
xfont = 2
xformat = 3
xsize = 11
xcolor = [0,0,0]

if obj_valid(xAxisTemplate) then begin

  xAxisTemplate->getProperty,labels=xTlabels
  
  if obj_valid(xTlabels) && obj_isa(xTlabels,'IDL_Container') then begin
  
    xTlabel = xTlabels->get()
  
    if obj_valid(xTlabel) then begin
  
      xTlabel->getProperty,font=xfont,format=xformat,size=xsize,color=xcolor
  
    endif
  
  endif

endif
                    
xlabels->add,obj_new('thm_ui_text',value=xlabeltext,font=xfont,format=xformat,size=xsize,$
                    color=xcolor,show=keyword_set(xlabeltext))
                    
if keyword_set(zlabeltext) then begin

  zfont = 2
  zformat = 3
  zsize = 11
  zcolor = [0,0,0]
  
  if obj_valid(zAxisTemplate) then begin
  
    zAxisTemplate->getProperty,labeltextobject=zTlabel
    
    if obj_valid(zTlabel) then begin
  
      zTlabel->getProperty,font=zfont,format=zformat,size=zsize,color=zcolor
  
    endif
  
  endif

  zaxis->setProperty,labeltextobject=obj_new('thm_ui_text',value=zlabeltext,font=zfont,format=zformat,size=zsize,$
                    color=zcolor,show=1)
endif

; check if current data is in current y-range (as long as it's not a new panel)
yaxis->getproperty, minfixedrange=minfixedrange, maxfixedrange=maxfixedrange, $
                    rangeOption=rangeOption
if (minfixedrange ne maxfixedrange) and (rangeOption eq 2) then begin
  cminmax = thm_ui_data_minmax(loadedData, yvar)
  if (cminmax[0] lt minfixedrange) OR (cminmax[1] gt maxfixedrange) then begin
    if obj_valid(gui_sbar) then $
      gui_sbar->update, 'Some of the data added may be outside the panel Y-Axis limits.'
  endif
endif

; set x and y axis properties
xAxis->SetProperty, labels=xlabels, isTimeAxis=xistime,/notouched

;Set number of major ticks(xAxis)
if dataSettings->getxNumMajorTicks() eq -1 then begin ;auto mode
end else begin ; use setting from dlimits
  xAxis->setProperty, numMajorTicks=dataSettings->getxNumMajorTicks(), $
                      autoticks=0,/notouched
endelse

;Set number of minor ticks(xAxis)
if dataSettings->getxNumMinorTicks() eq -1 then begin ;auto mode
  if xistime then begin
    xAxis->setProperty,numMinorTicks = 5,/notouched
  endif else begin
    xAxis->getProperty,scaling=xscale
    if xscale eq 1 then begin
      xAxis->setProperty,numMinorTicks = 8,/notouched
    endif else if xScale eq 2 then begin
      xAxis->setProperty,numMinorTicks = 0,/notouched
    endif else begin
      xAxis->setProperty,numMinorTicks = 4,/notouched
    endelse
  endelse
endif else begin ; use setting from dlimits
  xAxis->getProperty, scaling=xscaling
  xAxis->setProperty, numMinorTicks=dataSettings->getxNumMinorTicks(), $
                      minorTickAuto=0, majorTickAuto=1,/notouched;, autoticks=0
  if (dataSettings->getxNumMajorTicks() eq -1) then begin
    if xscaling eq 1 then begin
      xAxis->setProperty, majortickauto=0, majorTickEvery=1, firstTickAt=0., $
                          firstTickUnits=4, majorTickUnits=0,/notouched
    endif else begin
      ; minor ticks won't work when major ticks on auto
    endelse
  endif
endelse

yAxis->SetProperty, labels=ylabels, isTimeAxis=yistime,/notouched

;Set number of major ticks(yAxis)
if dataSettings->getyNumMajorTicks() eq -1 then begin ;auto mode
end else begin ; use setting from dlimits
  yAxis->setProperty, numMajorTicks=dataSettings->getyNumMajorTicks(), $
                      autoticks=0,/notouched
endelse

;Set number of minor ticks(yAxis)
if dataSettings->getyNumMinorTicks() eq -1 then begin ;auto mode
  if yistime then begin
    yAxis->setProperty,numMinorTicks = 5,/notouched
  endif else begin
    yAxis->getProperty,scaling=yscale
    if yscale eq 1 then begin
      yAxis->setProperty,numMinorTicks = 8,/notouched
    endif else if yScale eq 2 then begin
      yAxis->setProperty,numMinorTicks = 0,/notouched
    endif else begin
      yAxis->setProperty,numMinorTicks = 4,/notouched
    endelse
  endelse
endif else begin ; use setting from dlimits
  yAxis->getProperty, scaling=yscaling
  yAxis->setProperty, numMinorTicks=dataSettings->getyNumMinorTicks(), $
                      minorTickAuto=0, majorTickAuto=1,/notouched;, autoticks=0
  if (dataSettings->getyNumMajorTicks() eq -1) then begin
    if yscaling eq 1 then begin
      yAxis->setProperty, majorTickAuto=0, majorTickEvery=1, firstTickAt=0., $
                          firstTickUnits=4, majorTickUnits=0,/notouched
    endif else begin
      ; minor ticks won't work when major ticks on auto
    endelse
  endif
endelse

;Set number of major ticks(zAxis)
if dataSettings->getzNumMajorTicks() eq -1 then begin ;auto mode
end else begin ; use setting from dlimits
  zAxis->setProperty, ticknum=dataSettings->getzNumMajorTicks(), $
                      autoticks=0,/notouched
endelse

;Set number of minor ticks(zAxis)
if dataSettings->getzNumMinorTicks() eq -1 then begin ; auto mode
  zAxis->getProperty,scaling=zscale
  if zscale eq 1 then begin
    zAxis->setProperty,minorTickNum = 8,/notouched
  endif else if zScale eq 2 then begin
    zAxis->setProperty,minorTickNum = 0,/notouched
  endif else begin
    zAxis->setProperty,minorTickNum = 4,/notouched
  endelse
endif else begin ; use setting from dlimits
  zAxis->setProperty, minorTickNum=dataSettings->getzNumMinorTicks(), $
                      autoticks=0,/notouched
endelse

panel->SetProperty, traceSettings=traceSettings, xAxis=xAxis, yAxis=yAxis, $
                    zAxis=zAxis

end
