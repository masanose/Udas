;+
;NAME:
;thm_check_valid_name
;PURPOSE:
;checks a string or array input against another array and 'all' to
;find matches.
;CALLING SEQUENCE:
; ok_names = thm_check_valid_name(names, valid_names)
;INPUT:
; names = a string array of possible names for data types, stations,
; etc...
; valid_names = those names that will be valid
;OUTPUT:
; ok_names = the valid data names, if there are none, then the null
; string is returned
;KEYWORDS:
; include_all = if set, include 'all' in the possible datanames
; ignore_case = if set converts all inputs 
; loose_interpretation = if set, adds wild card '*'s to each end of
;                        the input names -- used in thm_load_mom, and
;                        thm_load_esa, where there are many L2
;                        variables.
; no_warning = if set, do not issue a warning if the input is invalid
;HISTORY:
; 22-jan-2007, jmm, jimm@ssl.berkeley.edu
; 11-feb-2007, jmm, Added loose_interpretation keyword
;
; $LastChangedBy: kenb-mac $
; $LastChangedDate: 2007-01-26 15:52:34 -0800 (Fri, 26 Jan 2007) $
; $LastChangedRevision: 241 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/trunk/idl/themis/common/thm_check_valid_name.pro $
;-
Function thm_check_valid_name, names_in, valid_names, include_all = include_all, $
                               ignore_case = ignore_case, $
                               loose_interpretation = loose_interpretation, $
                               no_warning = no_warning

  if keyword_set(ignore_case) then begin
    names_in = strlowcase(names_in)
    valid_names = strlowcase(valid_names)
  endif

  otp = ''
  If(size(/type, names_in[0]) ne 7  Or $
     size(/type, valid_names[0]) ne 7) Then Return, otp
  If(keyword_set(include_all)) Then vn = ['all', valid_names] $
  Else vn = valid_names
  
;string compression to eliminate space padding errors
  vn = strcompress(vn, /remove_all)

;this line prevents input mutation
  ni = names_in
  
  if(size(names_in, /n_dim) ne 0) then ni = strcompress(ni, /remove_all)
  If(keyword_set(loose_interpretation)) Then ni = '*'+ni+'*'

  otp = strfilter(vn, ni, delimiter = ' ', /string)
  If(not keyword_set(otp)) Then Begin
    If(not keyword_set(no_warning)) Then Begin
      if keyword_set(include_all) then $
        message, /info, 'Input must be one or more of the following strings:' $
      else $
        message, /info, 'Input must be one of the following strings:' 
      print, vn
    Endif
    Return, ''
  Endif
  If(keyword_set(include_all)) Then Begin
    all = where(otp Eq 'all')
    If(all[0] ne -1) Then otp = valid_names
  Endif

  Return, otp
End

