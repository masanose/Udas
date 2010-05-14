;+ 
;NAME:
; thm_ui_load_data_file_coord_sel.pro
;
;PURPOSE:
; Controls actions that occur when Output Coordinates menu is selected.  Called
; by thm_ui_load_data_file event handler.
;
;CALLING SEQUENCE:
; thm_ui_load_data_file_coord_sel, state
;
;INPUT:
; state     State structure
;
;OUTPUT:
; None
;
;HISTORY:
;-
pro thm_ui_load_data_file_coord_sel, state

  Compile_Opt idl2, hidden
  
  outCoord = widget_info(state.coordDroplist, /combobox_gettext)
  state.outCoord = strlowcase(strcompress(outCoord, /remove_all))
  
  h = 'Selected Output Coordinates: ' + state.outCoord
  state.statusText->Update, h
  state.historyWin->Update, 'LOAD DATA: ' + h
  
  ; reset Level 2 datatype list based on coord type
  thm_ui_load_data_file_itype_sel, state, /from_coord_sel
  
END