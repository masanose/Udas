;+ 
;NAME:
; thm_ui_load_data_file_itype_sel.pro
;
;PURPOSE:
; Controls actions that occur when Instrument Type menu is selected.  Called by
; thm_ui_load_data_file event handler.
;
;CALLING SEQUENCE:
; thm_ui_load_data_file_itype_sel, state
;
;INPUT:
; state     State structure
;
;OUTPUT:
; None
;
;HISTORY:
;-
pro thm_ui_load_data_file_itype_sel, state, from_coord_sel=from_coord_sel

  Compile_Opt idl2, hidden
  
;  pindex = widget_info(state.itypeDroplist, /DropList_Select)
;  instr_in = strcompress(strlowcase(state.validIType[pindex]),/remove_all)
  instr_in = widget_info(state.itypeDroplist, /combobox_gettext)
  state.instr = strlowcase(strcompress(instr_in, /remove_all))
  
  pindex = where(instr_in eq state.validIType)
  
  ; handle different coordinate system abilities of load routines 
  CASE 1 OF
    (state.instr eq 'fgm') OR (state.instr eq 'scm'): BEGIN
      if ptr_valid(state.validcoords) then ptr_free, state.validcoords
      validcoords = [ ' DSL ', ' GSM ', ' SSL ',' GSE ']
      state.validcoords = ptr_new(validcoords)
    END
    (state.instr eq 'fit') OR (state.instr eq 'esa'): BEGIN
      if ptr_valid(state.validcoords) then ptr_free, state.validcoords
      validcoords = [ ' DSL ', ' GSM ', ' GSE ']
      state.validcoords = ptr_new(validcoords)
    END
    ELSE: BEGIN
      if ptr_valid(state.validcoords) then ptr_free, state.validcoords
      validCoords = [ ' DSL ', ' GSM ', ' SPG  ', ' SSL ',' GSE ', ' GEI ',' SM ']
      state.validcoords = ptr_new(validcoords)
    END
  ENDCASE
  
  ; reset selected output coordinate when new instrument is selected
  if ~keyword_set(from_coord_sel) then begin
    if state.instr eq 'state' then begin
      ; make sure STATE defaults to GEI coords
      widget_control, state.coordDroplist, set_value=*state.validCoords, $
                      set_combobox_select=5
    endif else begin
      widget_control, state.coordDroplist, set_value=*state.validCoords, $
                      set_combobox_select=0
    endelse
  endif
  outCoord = widget_info(state.coordDroplist, /combobox_gettext)
  state.outCoord = strlowcase(strcompress(outCoord, /remove_all))

  if ~keyword_set(from_coord_sel) then begin
    if(ptr_valid(state.dtyp)) then ptr_free, state.dtyp
    if(ptr_valid(state.dtyp_pre)) then ptr_free, state.dtyp_pre
    state.dtyp_pre = ptr_new('')
  endif

 
; temp code until we get ASI data going again.
;
  if pindex eq 0 then begin
    state.observ_label = ' '
    validobservlist = 'None'
    dlist1_all = 'None'
    dlist2_all = 'None'
    
    h = 'Please select an Instrument Type. '
    state.statusText->Update, h
    state.historyWin->Update, 'LOAD DATA: ' + h
    
    widget_control, state.observBase, sensitive=0

    if(ptr_valid(state.dtyp1)) then ptr_free, state.dtyp1
    state.dtyp1 = ptr_new('')
    if(ptr_valid(state.dtyp2)) then ptr_free, state.dtyp2
    state.dtyp2 = ptr_new('')
    if(ptr_valid(state.observ)) then ptr_free, state.observ
    state.observ = ptr_new('')
    if(ptr_valid(state.dtyp)) then ptr_free, state.dtyp
    if(ptr_valid(state.dtyp_pre)) then ptr_free, state.dtyp_pre
    state.dtyp_pre = ptr_new('')
  endif else widget_control, state.observBase, sensitive=1
;
; end temp code until we get ASI data going again.

  
  dlist = thm_ui_new_valid_datatype(state.instr, ilist, llist)
  
  ; temporarily remove the "all processors" variables until data object can handle 3D data
  if array_equal(state.instr, 'fft') then begin
    dlist = dlist[6:n_elements(dlist)-1]
    llist = llist[6:n_elements(llist)-1]
    ilist = ilist[6:n_elements(ilist)-1]
  endif
  
  dlist2_orig = *state.dlist2
  
;  validcoords = strcompress(strlowcase(state.validcoords), /remove_all)
;  not_val_coord = validcoords(where(state.outcoord ne validcoords))
;  
;  for i=0,n_elements(not_val_coords) - 1 do begin
;    
;  endfor 
  
  ; clear observatories, L1 and L2 datatype selections
  if ~keyword_set(from_coord_sel) then begin
;    if(ptr_valid(state.dtyp2)) then ptr_free, state.dtyp2
;    state.dtyp2 = ptr_new('')
;  endif else begin
    if(ptr_valid(state.dtyp1)) then ptr_free, state.dtyp1
    state.dtyp1 = ptr_new('')
    if(ptr_valid(state.dtyp2)) then ptr_free, state.dtyp2
    state.dtyp2 = ptr_new('')
    if(ptr_valid(state.observ)) then ptr_free, state.observ
    state.observ = ptr_new('')
    state.statusText->Update,'No Chosen data types or observatories'
  endif
  
  
  CASE 1 OF
    (pindex gt 1 AND pindex lt 8) OR (pindex gt 8):BEGIN
      state.observ_label = state.observ_labels[2]+':'
      validobserv = state.probes
      validobservlist = state.validProbes
      xx1 = where(llist Eq 'l1', nxx1)
      If(nxx1 Gt 0) Then Begin
        dlist1_all = ['*', dlist[xx1]] 
        If(state.instr Eq 'fbk') Then Begin
          dlist1_all = ['*', 'fb1', 'fb2', 'fbh']
        Endif
      Endif Else dlist1_all = 'None'
      xx2 = where(llist Eq 'l2', nxx2)
      If(nxx2 Gt 0) Then begin
      
        dlist2_all = ['*', dlist[xx2]]
        
        validcoords = strcompress(strlowcase(*state.validcoords), /remove_all)
        inval_coords = validcoords[where(state.outcoord ne validcoords)]
  
        for i=0,n_elements(inval_coords) - 1 do begin
          f_str = '*_' + inval_coords[i]
          if i gt 0 then invali = [invali, where(strmatch(dlist2_all, f_str) eq 1)] $
            else invali = where(strmatch(dlist2_all, f_str) eq 1)
        endfor
        
        dlist2_all_t = dlist2_all
        invali_i = where(invali gt 0, n_invali)
        
        if n_invali eq 0 then begin
          if array_equal(dlist2_all,'*',/no_typeconv) then dlist2_all = 'None'
        endif else begin
          invali = invali[where(invali gt 0, n_invali_i)]
          dlist2_all_t[invali] = 'invalid'
          dlist2_all = dlist2_all[where(dlist2_all_t ne 'invalid')]
          if array_equal(dlist2_all,'*',/no_typeconv) then dlist2_all = 'None'
        endelse
      
      endif else dlist2_all = 'None'
      

      if (state.instr eq 'fbk') || (state.instr eq 'fft') || $
         (state.instr eq 'mom') || (state.instr eq 'spin') || $
         (state.instr eq 'sst') then begin
           widget_control, state.coordDroplist, Sensitive=0
           state.outCoord = 'N/A'
      endif else begin
        widget_control, state.coordDroplist, /Sensitive
        coord_index = widget_info(state.coordDroplist, /droplist_select)
        outCoord = widget_info(state.coordDroplist, /combobox_gettext)
        state.outCoord = strlowcase(strcompress(outCoord, /remove_all))
      endelse
    END
;    pindex eq 0:BEGIN
;    ; place holder for when we get ASI data going again
;      h = 'Please select an Instrument Type. '
;      state.statusText->Update, h
;      state.historyWin->Update, 'LOAD DATA: ' + h
;      return
;    END
;    pindex eq 0:BEGIN
;      state.observ_label = ' '
;      validobservlist = 'None'
;      dlist1_all = 'None'
;      dlist2_all = 'None'
;      
;      h = 'Please select an Instrument Type. '
;      state.statusText->Update, h
;      state.historyWin->Update, 'LOAD DATA: ' + h
;      temp=1
;    END
    pindex eq 1:BEGIN
      state.observ_label = state.observ_labels[0]+':'
      thm_load_ask, /valid_names, site=asi_stations
      validobserv = ['* (All)', asi_stations]
      validobservlist = validobserv
      validobserv = strlowcase(strcompress(validobserv, /remove_all))
      dlist1_all = ['*', dlist]
      dlist2_all = 'None'
      widget_control, state.coordDroplist, Sensitive=0
      state.outCoord = 'N/A'
    END
    pindex eq 8:BEGIN
      state.observ_label = state.observ_labels[1]+':'
      thm_load_gmag, /valid_names, site = gmag_stations
      validobserv = ['* (All)', gmag_stations]
      validobservlist = validobserv
      validobserv = strlowcase(strcompress(validobserv, /remove_all))
      dlist1_all = 'None'
      dlist2_all = ['*', dlist]
      widget_control, state.coordDroplist, Sensitive=0
      state.outCoord = 'N/A'
    END 
    ELSE: ;print,'DTYPE_DLIST bomb.'
  ENDCASE
  widget_control, state.observLabel, set_value=state.observ_label
  if ~keyword_set(from_coord_sel) then  widget_control,state.observList, $
                                          set_value=validobservlist
  if ptr_valid(state.validobserv) then ptr_free, state.validobserv
  state.validobserv = ptr_new(validobserv)
  if ptr_valid(state.validobservlist) then ptr_free, state.validobservlist
  state.validobservlist = ptr_new(validobservlist)
  
  dlist1 = dlist1_all & dlist2 = dlist2_all
  ;state.dlist1 = dlist1 & state.dlist2 = dlist2

  if ~keyword_set(from_coord_sel) then begin
    widget_control,state.level1List, set_value=dlist1
    if (ptr_valid(state.dlist1)) then ptr_free,state.dlist1
    state.dlist1 = ptr_new(dlist1)
  endif
  
  if keyword_set(from_coord_sel) then begin
    if ~array_equal(dlist2, dlist2_orig, /no_typeconv) then begin
      if(ptr_valid(state.dtyp2)) then ptr_free, state.dtyp2
      state.dtyp2 = ptr_new('')
      widget_control,state.level2List, set_value=dlist2
      if (ptr_valid(state.dlist2)) then ptr_free,state.dlist2
      state.dlist2 = ptr_new(dlist2)
    endif
  endif else begin
    widget_control,state.level2List, set_value=dlist2
    if (ptr_valid(state.dlist2)) then ptr_free,state.dlist2
    state.dlist2 = ptr_new(dlist2)
  endelse

  h = 'Selected Output Coordinates: '+state.outCoord
  state.historyWin->Update, h
  if keyword_set(from_coord_sel) then begin
    h = 'Selected Output Coordinates: '+state.outCoord
  endif else begin
    h = 'Selected Instrument Type: '+state.instr
  endelse
  state.statusText->Update, h
  state.historyWin->Update, 'LOAD DATA: ' + h

  RETURN
END