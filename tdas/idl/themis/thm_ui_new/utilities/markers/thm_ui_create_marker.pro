PRO thm_ui_create_marker, info

  compile_opt idl2,hidden
   
  windowStorage = info.windowStorage
  markerTitle = info.markerTitle
  drawObject = info.drawObject
  pageSettings = info.pageSettings
  statusBar = info.statusBar
  historywin = info.historywin
  
  newMarkers = drawObject->GetMarkers()
  
  IF Is_Num(newMarkers) && newMarker EQ 0 THEN BEGIN
    statusBar->update,'Marker was not created'
    historywin->update,'Marker was not created'
    print, 'Marker was not created'
    return
  ENDIF 
  
  activeWindow = windowStorage->GetActive()
  IF NOT Obj_Valid(activeWindow) THEN BEGIN
    statusBar->update,'Marker not created: no active window'
    historywin->update,'Marker not created: no active window'
    print, 'Marker not created: no active window'
    return  
  ENDIF
  
  activeWindow->GetProperty, Panels=panels
  IF ~Obj_Valid(panels) THEN BEGIN
    statusBar->update,'Marker not created: no panels'
    historywin->update,'Marker not created: no panels'
    print, 'Marker not created: no panels'
    return  
  ENDIF
  
  panelObjs = panels->Get(/all)
  IF Is_Num(panelObjs) || ~obj_valid(panelObjs[0]) THEN BEGIN
    statusBar->update,'Marker not created: no panels'
    historywin->update,'Marker not created: no panels'
    print, 'Marker not created: no panels'
    return  
  ENDIF

  invalidList = ''
  windowdims = drawobject->getdim()
    
  markerPanels = panelObjs[newMarkers[*].idx]
  for i = 0,n_elements(markerPanels)-1 do begin
      
    markerPanels[i]->GetProperty, Markers=markers
    newMarkers[i].marker->GetProperty, Settings=markerSettings,range=range
    
    panelInfo = drawObject->getPanelInfo(newMarkers[i].idx)
    if ~is_struct(panelInfo) then begin
      statusBar->update,'Marker not created: panel error'
      historywin->update,'Marker not created: panel error'
      return
    endif

    if panelInfo.xscale eq 1 then begin
      panelxrange=10D^panelinfo.xrange
    endif else if panelinfo.xscale eq 2 then begin
      panelxrange=exp(panelinfo.xrange)
    endif else begin
      panelxrange = panelinfo.xrange
    endelse 

    ;check marker width prior to adding
    if windowdims[0]*(range[1]-range[0])/(panelxrange[1]-panelxrange[0]) ge 2D then begin
      
      markerSettings->GetProperty, Label=label
      markerTitle->GetProperty, name=name, UseDefault=usedefault, DefaultName=defaultname  
      IF UseDefault EQ 1 THEN markerName=defaultname ELSE markerName=name      
      pageSettings->GetProperty, Marker=markerTextObject
      markerTextObject->GetProperty, Size=size, Font=font, Format=format, Color=color, $
        Thickness=thickness, Show=show
      label->SetProperty, Value=markername, Size=size, Font=font, Format=format, Color=color, $
        Thickness=thickness, Show=show
      markerSettings->SetProperty, VertPlacement=0
      newMarkers[i].marker->SetProperty, Name=markername
      markers->Add, newMarkers[i].marker
    endif else begin
      if ~keyword_set(invalidlist) then begin
        invalidlist = markerPanels[i]->constructPanelName()
      endif else begin
        invalidList = [invalidList,markerPanels[i]->constructPanelName()]
      endelse
    endelse
  endfor
 
  if keyword_set(invalidList) then begin
    outlist = strjoin(invalidList,' , ')
    statusBar->update,'Marker not wide enough. Could not be added to panel(s): ' + outlist
    historyWin->update,'Marker not wide enough. Could not be added to panel(s): ' + outlist
  endif else begin
    statusBar->update,'Marker(s) created.'
    historywin->update,'Marker(s) created.'
  endelse

END
