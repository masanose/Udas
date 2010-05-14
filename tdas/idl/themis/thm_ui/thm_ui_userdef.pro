;+
;NAME:
; thm_ui_userdef
;PURPOSE:
; A widget that allows the user to define his own operator to be used
; on the data from a tplot variable, e.g., the user types in the
; string '2.0*!pi*q', and a temporary function is created which takes
; the data and does this multiplication. This is pretty experimental.
;CALLING SEQUENCE:
; thm_ui_userdef, gui_id
;INPUT:
; gui_id = the id of the main gui widget
;OUTPUT:
; none explicit, hopefully new tplot variables are created
;HISTORY:
; 7-jun-2007, jmm, jimm@ssl.berkeley.edu
;
;$LastChangedBy: cgoethel $
;$LastChangedDate: 2008-07-08 08:41:22 -0700 (Tue, 08 Jul 2008) $
;$LastChangedRevision: 3261 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui/thm_ui_userdef.pro $
;-
Pro thm_ui_userdef_event, event
  widget_control, event.id, get_uval = uval
  Case uval Of
    'EXIT': widget_control, event.top, /destroy
    'FUNC': Begin
      widget_control, event.id, get_val = func_text
      If(is_string(func_text)) Then Begin
        widget_control, event.top, get_uval = state, /no_copy
        ptr_free, state.func_text
        state.func_text = ptr_new(temporary(func_text))
      Endif
      widget_control, event.top, set_uval = state, /no_copy
    End
    'GO':Begin
      widget_control, event.id, get_val = func_text
      If(is_string(func_text)) Then Begin
        thm_ui_create_userdef, func_text, func_name
        widget_control, event.top, get_uval = state
        widget_control, state.cw, get_uval = wstate
        If(ptr_valid(wstate.active_vnames)) Then Begin
          tvn = *wstate.active_vnames
          cw = state.cw
          widget_control, state.cw, set_uval = wstate
          widget_control, event.top, set_uval = state
          vn_new = call_function(tvn, func_name) ;vn_new are new variable names
          thm_ui_update_data_all, cw, vn_new, /add_active
        Endif Else Begin
          widget_control, state.cw, set_uval = wstate
          thm_ui_update_progress, state.cw, $
            'No Active Dataset, Nothing happened'
          widget_control, event.top, set_uval = state
        Endelse
      Endif
    End
  Endcase

Return
End
Pro thm_ui_userdef

  master = widget_base(/row, title = 'THEMIS User-Defined Operator')

  label = 'Input string for algeraic operation, with the data to be input represented by a ''qq'''

  funcbase = widget_base(master, /col, /align_left, frame = 5)
  funcdisplay = widget_text(funcbase, uval = 'FUNC', val = '', /all_events, $
                            /editable, xsize = 80, ysize = 12, $
                            frame = 5)
  flabel = widget_label(funcbase, val = label)
;a widget for buttons
  buttons = widget_base(master, /col, /align_center, frame = 5)

; go button
  go_button = widget_base(buttons, /col, /align_center)
  gobut = widget_button(go_button, val = ' GO ', uval = 'GO', $
                        /align_center, scr_xsize = 120)
; save button
  save_button = widget_base(buttons, /col, /align_center)
  savebut = widget_button(go_button, val = 'SAVE ', uval = 'SAVE', $
                        /align_center, scr_xsize = 120)
; exit button
  exit_button = widget_base(buttons, /col, /align_center)
  exitbut = widget_button(exit_button, val = ' Close ', uval = 'EXIT', $
                        /align_center, scr_xsize = 120)
  state = {master:master, funcdisplay:funcdisplay, $
           func_text:ptr_new('')}
  widget_control, master, set_uval = state, /no_copy
  widget_control, master, /realize
  xmanager, 'thm_ui_userdef', master, /no_block
  Return
End

