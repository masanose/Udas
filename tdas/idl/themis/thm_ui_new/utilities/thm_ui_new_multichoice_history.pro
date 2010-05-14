;+
; This program creates a string for the history array when multiple
; datatypes, spacecraft or stations are chosen, for use in the
; themis_w_event routine
;$LastChangedBy: bckerr $
;$LastChangedDate: 2008-10-03 11:35:33 -0700 (Fri, 03 Oct 2008) $
;$LastChangedRevision: 3615 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_new_multichoice_history.pro $
;-
Function thm_ui_new_multichoice_history, init_string, multi_string

  n = n_elements(multi_string)
  
  mss = ''''+multi_string+''''
  ext = init_string+'['+mss[0]
  If(n Gt 1) Then Begin
    For j = 1, n-1 Do ext = ext+','+mss[j]
  Endif
  ext = ext+']'
  Return, ext
End

