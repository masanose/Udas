;+ 
;NAME:  
;  tplot_gui
;  
;PURPOSE:
;  Imports and creates plot of tplot variable in THM_GUI_NEW.
;  
;CALLING SEQUENCE:
;  tplot_gui, [datanames]
;  
;INPUT:
;  datanames: A string of space separated datanames.  Wildcard expansion is
;             supported.  If datanames is not supplied then the last values are
;             used. Each name should be associated with a data quantity.
;             (see the "STORE_DATA" and "GET_DATA" routines.) Alternatively 
;             datanames can be an array of integers or strings.  Run
;             "TPLOT_NAMES" to show the current numbering.
;          
;            
;            
;KEYWORDS:
;  /NO_VERIFY: Bypasses the Verify window before plotting the data in the GUI.
;              Intended to be used in cases when it is certain that the limits
;              and dlimits of the incoming tplot variables are complete and
;              correct (e.g. overview plot generation).  Use with caution.
;  /NO_DRAW: Data is loaded, but it is not plotted.
;  /no_update: data is loaded and added to plot, but update is not called.
;    (saves runtime when building up a plot from several calls)
;  /RESET: Sets the Reset keyword on THM_GUI_NEW if gui is not already open. If
;          set, it will reset all internal gui settings. Otherwise, it will try
;          to load the state of the previous gui call.
;  /add_panel:  Adds data as a new panel in the current display
;  template_filename : The file name of a previously saved themis template document,
;                   can be used to store user preferences and defaults.
;        
;OUTPUT:
;  none
;
;--------------------------------------------------------------------------------

pro make_plots, newpanel, varname,template

  compile_opt idl2, hidden
    
  ; get group object and x-axis data quantity name
  group = !thm_gui_new.loadedData->getGroup(varname)
  xname = group->getTimeName()
  
  ; get dlimits/limits and merge into superstructure
  get_data, varname, dlimits=dl, limits=l
  extract_tags,dl,l

  if (size(dl, /type) eq 8) then begin
    if in_set('spec', strlowcase(tag_names(dl))) && dl.spec eq 1 then begin  ; create specplot
      
        groupname = group->getName()
        yname = group->getYaxisName()
        
        thm_ui_make_default_specplot, !thm_gui_new.loadedData, newpanel, xname,  $
                                      yname, groupname, template
      
    endif else begin    ; create lineplot
      
        ynames = group->getDataNames()
        
        for j=0, n_elements(ynames)-1 do begin
        
          thm_ui_make_default_lineplot, !thm_gui_new.loadedData, newpanel, xname, $
                                        ynames[j], template
        endfor
    endelse
  endif else begin   ; create lineplot
  
    ynames = group->getDataNames()
    
    for j=0, n_elements(ynames)-1 do begin
    
      thm_ui_make_default_lineplot, !thm_gui_new.loadedData, newpanel, xname, $
                                    ynames[j],template
    endfor
  endelse

end


pro tplot_gui, datanames, no_verify=no_verify, no_draw=no_draw,no_update=no_update, reset=reset,add_panel=add_panel,template_filename=template_filename

  compile_opt idl2
  
  ; check type and dimension of input
  dt = size(/type,datanames)
  ndim = size(/n_dimen,datanames)
  
  ; check for valid input
  if dt ne 0 then begin
   if dt ne 7 or ndim ge 1 then dnames = strjoin(tnames(datanames,/all),' ') $
     else dnames=datanames
  endif else begin
    last_tplot = tnames(/tplot)
    dprint,'Recreating the last tplot command with the following tplot variables:'
    dprint, last_tplot
    dnames = strjoin(last_tplot,' ')
  endelse
  
  ; make sure indexes are converted array of tplot variable names
  varnames = tnames(dnames,nd,ind=ind,/all)
  varnames2 = '' ; same as above but will have pseudovars expanded to component varnames
  varnames3 = '' ; to handle successfully loaded varnames
  temp2del = '' ; array of tplot pseudovariable names to delete
  
  if nd eq 0 then begin
     dprint,'No valid variable names found to tplot! (use <VARIABLE> = TNAMES() and PRINT,<VARIABLE> to display)'
     return
  endif
  
  defsysv,'!THM_GUI_NEW',exists=thm_exists ;check if prexisting gui
  
  if keyword_set(reset) || keyword_set(add_panel) || ~thm_exists then begin
    newwin = 0
  endif else begin
    newwin = 1
  endelse
  
  ; check if gui already running, start if not
  if (xregistered('thm_gui_new') eq 0) then begin
    
    thm_gui_new, reset=reset,template_filename=template_filename
     
  endif else begin
  
    ;read template from file, if requested
    if keyword_set(template_filename) then begin
      open_themis_template,template=template,filename=template_filename,$
        statusmsg=statusmsg,statuscode=statuscode
      if statuscode lt 0 then begin
        ok = dialog_message(statusmsg,/error,/center)
      endif else begin
        !thm_gui_new.windowstorage->setProperty,template=template
      endelse
    endif
    
  endelse
  
  !thm_gui_new.windowStorage->getProperty,template=template
  
  if keyword_set(no_verify) then begin
    clobber = 'yesall'
  endif else begin
    clobber = ''
  endelse
  
  ; add tplot variables to loadedData object
  for i=0L,nd-1 do begin
  
    ; get pre-existing gui variable names
    guiNames = !thm_gui_new.loadedData->GetAll(/Parent)
    if size(guiNames, /type) ne 7 then guiNames=''
  
    ; check if pseudovariable
    get_data, varnames[i], data=d, dlimits=pseudo_dl, limits=pseudo_l
    dSize = size(d, /type)
    
    if dSize eq 7 then begin
    ;load tplot pseudovariable
    
      ;extract_tags, pseudo_dl, pseudo_l
      
      subNames = tnames(d, sub_nd, ind=sub_ind, /all)
      varnames3 = [varnames3, varnames[i]]
      
      rebuild=0
      for j=0L,sub_nd-1 do begin
      ; load each component of pseudovariable
      
      
        ;make sure dlimits from pseudovar are inherited by component variables
        get_data, subNames[j], dlimits=sub_dl, limits=sub_l
        extract_tags, sub_dl, pseudo_dl
        extract_tags, sub_l, pseudo_l
        store_data, subNames[j], dlimits=sub_dl, limits=sub_l
        
        fail=0
        if (clobber ne 'yesall' AND clobber ne 'noall') AND in_set(subNames[j], guiNames) then begin
          clobber=thm_ui_load_clob_prompt(!thm_gui_new.guiId, !thm_gui_new.historyWin, $
                                          subNames[j])
          ; loop so that hitting the widget kill button doesn't close the window
          while clobber eq '' do clobber=thm_ui_load_clob_prompt(!thm_gui_new.guiId, $
                                                                 !thm_gui_new.historyWin, $
                                                                 subNames[j])
        endif 
        
        if clobber eq 'yes' OR clobber eq 'yesall' then begin
          h = 'TPLOT_GUI: ' + strupcase(subNames[j]) + ' will be overwritten.'
          !thm_gui_new.historyWin->Update, h
        endif
        
        if (clobber eq 'no' OR clobber eq 'noall') AND in_set(subNames[j], guiNames) then begin
          h = 'LOAD DATA: ' + strupcase(subNames[j]) + $
              ' not loaded to prevent overwrite of existing data.'
          !thm_gui_new.historyWin->Update, h
          continue
        endif
        
        if ~!thm_gui_new.loadedData->add(subNames[j]) then begin
;          ok = error_message('Add failed: ' + subNames[j] + '. the pseudovariable ' + varnames[i] + $
;               ' has an invalid variable.  Nothing will be plotted, but some data might have' + $
;               ' already been added to the GUI.', /traceback, /center, title='Error in TPLOT_GUI')
          fail=1
          rebuild=1
          ;return
        endif
        
        if ~fail then begin
          varnames2 = [varnames2, subNames[j]]
        endif
      endfor
      
      if rebuild then begin
      ; create a new temp pseudovar that only contains valid sub-variables
        get_data, varnames[i], dlimits=dl, limits=l
        store_data, varnames[i]+'_tmp', data=varnames2, dlimits=dl, limits=l
        varnames[i]=varnames[i]+'_tmp'
        varnames3[i+1] = varnames[i]
        temp2del = [temp2del, varnames[i]]
      endif
  
    endif else begin
    ; load standard tplot variable
  
      fail=0
      
      if (clobber ne 'yesall' AND clobber ne 'noall') AND in_set(varnames[i], guiNames) then begin
        clobber=thm_ui_load_clob_prompt(!thm_gui_new.guiId, !thm_gui_new.historyWin, $
                                        varnames[i])
        ; loop so that hitting the widget kill button doesn't close the window
        while clobber eq '' do clobber=thm_ui_load_clob_prompt(!thm_gui_new.guiId, $
                                                               !thm_gui_new.historyWin, $
                                                               varnames[i])
      endif
      
      if clobber eq 'yes' OR clobber eq 'yesall' then begin
        h = 'TPLOT_GUI: ' + strupcase(varnames[i]) + ' will be overwritten.'
        !thm_gui_new.historyWin->Update, h
      endif
      
      if (clobber eq 'no' OR clobber eq 'noall') AND in_set(varnames[i], guiNames) then begin
        h = 'LOAD DATA: ' + strupcase(varnames[i]) + $
            ' not loaded to prevent overwrite of existing data.'
        !thm_gui_new.historyWin->Update, h
        continue
      endif

      if ~!thm_gui_new.loadedData->add(varnames[i]) then begin
        ok = error_message('Add failed: ' + varnames[i], /traceback, /center, $
               title='Error in TPLOT_GUI')
        fail=1
      endif
      
      if ~fail then begin
        varnames2 = [varnames2, varnames[i]]
        varnames3 = [varnames3, varnames[i]]
      endif
    endelse
  endfor
  
  if n_elements(varnames2) gt 1 then begin
    varnames2 = varnames2[1:*] ; remove empty string at beginning of array
  endif else begin
    if array_equal(varnames2,'', /no_typeconv) then return
  endelse
 
  if n_elements(varnames3) gt 1 then begin
    varnames3 = varnames3[1:*] ; remove empty string at beginning of array
  endif else begin
    if array_equal(varnames3,'', /no_typeconv) then return
  endelse
  
  nd3 = n_elements(varnames3)
  
  ; verify incoming tplot variables
  ;thm_ui_verify_data,!thm_gui_new.guiId, varnames, !thm_gui_new.loadedData, $
  ;                   !thm_gui_new.windowStorage, !thm_gui_new.historywin, $
  ;                   success=success
  if ~keyword_set(no_verify) then begin
    thm_ui_verify_data,!thm_gui_new.guiId, varnames2, !thm_gui_new.loadedData, $
                       !thm_gui_new.windowStorage, !thm_gui_new.historywin, $
                       success=success,newnames=newnames
  
    if ~success then begin
      dprint,'Data verify canceled. No tplot variables were imported.'
      !THM_GUI_NEW.historyWin->update,'Data verify canceled. No tplot variables were imported.'
      return
    endif
  
  endif

  if ~keyword_set(no_draw) then begin

    ; add new window to gui
    if newwin then begin
            
      if ~!thm_gui_new.windowStorage->add(isactive=1) then begin
        ok = error_message('Error initializing new window for TPLOG_GUI.',/traceback, /center, $
               title='Error in TPLOT_GUI')
      endif  
    
    activeWindow = !thm_gui_new.windowStorage->GetActive()
    
    ; add window name to gui window menu
    activeWindow[0]->GetProperty, Name=name
    !thm_gui_new.windowMenus->Add, name
    !thm_gui_new.windowMenus->Update, !thm_gui_new.windowStorage
    
     ; update draw object and draw new window
 ;    !thm_gui_new.drawObject->Update, !thm_gui_new.windowStorage, !thm_gui_new.loadedData
 ;   !thm_gui_new.drawObject->draw
    
    endif
     
    ;get current time range (as set by tlimit)
    ctrange = timerange(/current)
    
    n_sub = 0 ; Because of the way this routine splits up the lists we need to keep a separate counter to synchronize output from newnames in the event of a name change
    
    ; create a separate panel for each data quantity
    for i=0L,nd3-1 do begin
    
      ; create panel
      thm_ui_make_default_panel, !thm_gui_new.windowStorage, template,outpanel=newpanel, $
                                 trange=ctrange
  
      ; check if pseudovariable
      get_data, varnames3[i], data=d
      dSize = size(d, /type)
      
      if dsize eq 7 then begin
      ;plot tplot pseudovariable
      
        subNames = tnames(d, sub_nd, ind=sub_ind, /all)
  
        for k=0L,sub_nd-1 do begin
        ; load each component of pseudovariable
    
          if keyword_set(newnames) then begin
            make_plots, newpanel, newnames[n_sub],template
          endif else begin     
            make_plots, newpanel, subNames[k],template
          endelse
    
          n_sub++
          
        endfor
      endif else begin
      ; plot standard tplot variable
        if keyword_set(newnames) then begin
          make_plots, newpanel, newnames[n_sub],template
        endif else begin   
          make_plots, newpanel, varnames3[i],template
        endelse
        
        n_sub++
      endelse
    endfor
        
    if ~keyword_set(no_update) then begin
      !thm_gui_new.drawObject->update,!thm_gui_new.windowStorage, !thm_gui_new.loadedData
      !thm_gui_new.drawObject->draw
    endif
    
  endif

  store_data, delete=tnames(temp2del)
end
