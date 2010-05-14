;+
;NAME:
; tdeflag
;PURPOSE:
; wapper for xdeflag.pro allowing input of tplot variable names
;CALLING SEQUENCE:
; tdeflag, varnames, method, newname=newname, $
;          overwrite = overwrite, _extra=_extra
;INPUT:
; varnames = an array (or scalar) of tplot variable names
; method = set to "repeat", this will repeat the last good value.
;          set to "linear", then linear interpolation is used, but for
;          the edges, the closest value is used, there is no
;          extrapolation
;KEYWORDS:
; flag = all values greater than 0.98 times this value will be deflagged, 
;        the default is 6.8792e28, Nan's, Inf's are also deflagged
; maxgap = the maximum number of rows that can be filled? the default
;           is n_elements(t)
; newname = if set,give these names to the deflagged data, the
;                default is to append '_deflag' to the input names and
;                pass out the names in the newname variables,
;                Unless /overwrite is set
; overwrite = if set, write the new data back to the old tplot
;             variables, do not set this with newname
;HISTORY:
; 2-feb-2007, jmm, jimm.ssl.berkeley.edu
;
;$LastChangedBy$
;$LastChangedDate$
;$LastChangedRevision$
;$URL$
;-
Pro tdeflag, varnames, method, newname = newname, $
             overwrite = overwrite, _extra = _extra

;First extract the data
  n = n_elements(varnames)
  If(keyword_set(newname)) Then begin
    If(keyword_set(overwrite)) Then begin
      message, /info, 'Do not set both the newname and overwrite keywords'
      return
    Endif
    If(n_elements(newname) Ne n) Then Begin
      message, /info, 'Incompatible varnames, newname input'
      Return
    Endif
    nvn = newname
  Endif Else nvn = varnames+'_deflag'
;Now do the deflagging
  For j = 0, n-1 Do Begin
    get_data, varnames[j], data = d, dlim = dlim, lim = lim
    If(is_struct(d)) Then Begin
      y = d.y
      xdeflag, method, d.x, y, _extra = _extra
      d.y = temporary(y)
      If(keyword_set(overwrite)) Then new_name = varnames[j] $
      Else new_name = nvn[j]
      store_data, new_name, data = d, dlim = dlim, lim = lim
    Endif Else Begin
      message, /info, 'No Deflagging of: '+varnames[j]
    Endelse
  Endfor
  newname = nvn
  Return
End

