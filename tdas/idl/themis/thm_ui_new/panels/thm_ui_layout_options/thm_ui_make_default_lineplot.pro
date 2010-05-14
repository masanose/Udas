;+ 
;NAME:  
;  thm_ui_make_default_lineplot
;  
;PURPOSE:
;  Routine that creates a default lineplot on particular panel
;  
;CALLING SEQUENCE:
;  thm_ui_make_default_lineplot, loadedData, panel, xvar, yvar, gui_sbar=gui_sbar
;  
;INPUT:
;  loadedData: the loadedData object
;  panel:  the panel on which the plot should be placed.
;  xvar:  the name of the variable storing the x-data.
;  yvar:  the name of the variable storing the y-data.
;  gui_sbar: the main GUI status bar object
;            
;KEYWORDS:
;  none
;        
;OUTPUT:
;  none
;
;--------------------------------------------------------------------------------

pro thm_ui_make_default_lineplot, loadedData, panel, xvar, yvar,template, gui_sbar=gui_sbar

compile_opt idl2, hidden

panel->GetProperty, traceSettings=traceSettings, xAxis=xAxis, yAxis=yAxis
ntraces = traceSettings->count()

template->getProperty,x_axis=xAxisTemplate,y_axis=yAxisTemplate,line=lineTemplate

; check if x,y-axis objects exist, create if not
if ~obj_valid(xAxis) then xAxis = obj_new('thm_ui_axis_settings', blacklabels=1)
if ~obj_valid(yAxis) then yAxis = obj_new('thm_ui_axis_settings', blacklabels=0)

; get and check if y-axis labels exist, create if not
yAxis->getProperty, labels=ylabels
if ~obj_valid(ylabels) then ylabels = obj_new('IDL_Container')
xAxis->getProperty, labels=xlabels
if ~obj_valid(xlabels) then xlabels = obj_new('IDL_Container')

; setup default color order ['k','m','r','g','c','b','y']
;def_colors = [[0,0,0],[255,0,255],[255,0,0],[0,255,0],[0,255,255],[0,0,255],[255,255,0]]
def_colors = [[0,0,255],[0,255,0],[255,0,0],[0,0,0],[255,255,0],[255,0,255],[0,255,255]]

factors = [[1,0],[1,1],[1,3],[3,1],[1,7],[5,3],[3,5],[7,1]]

color_idx1 = ntraces mod 7
color_idx2 = (ntraces +1) mod 7
factor1 = factors[0,(ntraces/7) mod 8]
factor2 = factors[1,(ntraces/7) mod 8]


color=(factor1*def_colors[*,color_idx1]+factor2*def_colors[*,color_idx2])/(factor1+factor2)

;Note that this code assumes no names that represent quantities with multiple components
;will be passed in.  This is probably a good assumption, since we're dealing with line 
;quantities, but if this problem does occur, the function from the default specplot code
;called: thm_ui_default_specplot_get_data_object
;can probably be used without modification in lieu of the code below 
if loadedData->isParent(xvar) then begin ;sometimes name can be a group and data name, this makes sure we get the data
  group = loadedData->getGroup(xvar)
  xObj = group->getObject(xvar)
endif else begin
  xObj = loadedData->getObjects(name=xvar)
endelse

if loadedData->isParent(yvar) then begin ;sometimes name can be a group and data name, this makes sure we get the data
  group = loadedData->getGroup(yvar)
  yObj = group->getObject(yvar)
endif else begin
  yObj = loadedData->getObjects(name=yvar)
endelse

if obj_valid(yobj) && yvar[0] ne '' then begin

  yObj->getProperty, istime=yIsTime, settings=datasettings
  
  if obj_valid(datasettings) then begin
  
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
  
    if dataSettings->getUseColor() then begin
      color = dataSettings->getColor()
    endif
    
    xlabeltext = dataSettings->getXLabel()
    ylabeltext = dataSettings->getYLabel()

  endif
endif else begin
  yistime = 0
endelse
  
if obj_valid(xObj) && xvar[0] ne '' then begin
  xObj->getProperty, isTime=xIsTime, settings=datasettings
  
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

endif else begin
  xistime = 0
endelse

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

;yobj->getProperty,settings=dataSettings
;
;if obj_valid(dataSettings) then begin
;
;  if dataSettings->getUseColor() then begin
;    color = dataSettings->getColor()
;  endif
;  
;  xlabeltext = dataSettings->getXLabel()
;  ylabeltext = dataSettings->getYLabel()
;
;endif else begin
;  xlabeltext = ''
;  ylabeltext = yvar
;endelse

; add trace to traceSettings object

if obj_valid(lineTemplate) then begin

  line = lineTemplate->copy()
  line->setProperty,dataX=xvar,dataY=yvar
  traceSettings->add,line

endif else begin

  traceSettings->add,obj_new('thm_ui_line_settings',dataX=xvar, dataY=yvar, $
                           linestyle=obj_new('thm_ui_line_style', color=color,thick=1.0),$
                           symbol=obj_new('thm_ui_symbol',color=color,size=10.,id=4,name='Diamond'),$
                           plotpoints=4)

endelse

; add label for trace to labels object
; 

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

yfont = 2
yformat = 3
ysize = 11

if obj_valid(yAxisTemplate) then begin

  yAxisTemplate->getProperty,labels=yTlabels
  
  if obj_valid(yTlabels) && obj_isa(yTlabels,'IDL_Container') then begin
  
    yTlabel = yTlabels->get()
  
    if obj_valid(yTlabel) then begin
  
      yTlabel->getProperty,font=yfont,format=yformat,size=ysize
  
    endif
  
  endif

endif

;ylabels->add,obj_new('thm_ui_text', value=ylabel, font=2, format=3, size=8, $
ylabels->add,obj_new('thm_ui_text', value=ylabeltext, font=yfont, format=yformat, size=ysize, $
                    color=color)
xlabels->add,obj_new('thm_ui_text', value=xlabeltext, font=xfont, format=xformat, size=xsize,$
                    color=xcolor, show=keyword_set(xlabeltext))

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

panel->SetProperty, traceSettings=traceSettings, xAxis=xAxis, yAxis=yAxis

end
