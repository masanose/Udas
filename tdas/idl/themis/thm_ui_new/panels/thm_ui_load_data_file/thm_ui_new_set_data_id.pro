;+
;NAME:
; thm_ui_new_set_data_id
;PURPOSE:
; This program sets the data_id values of the pointer in the thm_gui state
; structure, given the tplot common block:
;INPUT:
; state_or_id = thm_gui state structure, or gui_id
;HISTORY:
; 26-feb-2007, jmm, jimm@ssl.berkeley.edu
; 10-may-2007, jmm, allow for input of the gui_id instead of the state
;
;$LastChangedBy: bckerr $
;$LastChangedDate: 2008-10-03 09:14:58 -0700 (Fri, 03 Oct 2008) $
;$LastChangedRevision: 3611 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_load_data_file/thm_ui_new_set_data_id.pro $
;
;-
Pro thm_ui_new_set_data_id, state_or_id
  @tplot_com
  have_widget = 0b
  If(is_struct(state_or_id)) Then state = state_or_id $
  Else If(widget_valid(state_or_id)) Then Begin
    have_widget = 1b
    widget_control, state_or_id, get_uval = state, /no_copy
  Endif Else message, 'Invalid Input'
  If(is_struct(data_quants) And n_elements(data_quants) Gt 1) Then Begin
    tx = time_string(data_quants.trange)
    didx = data_quants.name+':  '+tx[0, *]+' to '+tx[1, *];+']'
    If(ptr_valid(state.data_id)) Then ptr_free, state.data_id
    state.data_id = ptr_new(didx)
  Endif Else Begin
    state.data_id = ptr_new('None')
  Endelse
  If(have_widget) Then Begin
    widget_control, state_or_id, set_uval = state, /no_copy
  Endif Else state_or_id = temporary(state)
  Return
End
  
