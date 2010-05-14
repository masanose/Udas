;+
;NAME:
; tclip
;PURPOSE:
; wapper for xclip.pro allowing input of tplot variable names
;CALLING SEQUENCE:
; tclip, varnames, amin, amax, _extra=_extra
;INPUT:
; varnames = an array (or scalar) of tplot variable names
; amin, amax = the minumum and maximum values
;KEYWORDS:
; flag = the value that clipped data are  set to, the default is
;        -0.0/0.0 (NaN)
; newname = if set,give these names to the clipped data, the
;                default is to append '_clip' to the input names and
;                pass out the names in the newname variables,
;                Unless /overwrite is set. This will not work for wild
;                card input.
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
Pro tclip, varnames_in, amin, amax, newname = newname, $
           overwrite = overwrite, _extra = _extra

;First extract the data
  varnames = tnames(varnames_in)
  n = n_elements(varnames)
  nvn = varnames+'_clip'
  If(keyword_set(newname)) Then Begin
    If(keyword_set(overwrite)) Then Begin
      message, /info, 'Do not set both the newname and overwrite keywords. '+$
        'Using Default Values'
    Endif Else If(n_elements(newname) Ne n) Then Begin
      message, /info, 'Incompatible varnames, newname input. '+$
        'Using Default Values'
    Endif Else nvn = newname
  Endif
;Now do the clipping
  For j = 0, n-1 Do Begin
    get_data, varnames[j], data = d, dlim = dlim, lim = lim
    If(is_struct(d)) Then Begin
      y = d.y
      xclip, amin, amax, y, _extra = _extra
      d.y = temporary(y)
      If(keyword_set(overwrite)) Then new_name = varnames[j] $
      Else new_name = nvn[j]
      store_data, new_name, data = d, dlim = dlim, lim = lim
    Endif Else Begin
      message, /info, 'No Clipping of: '+varnames[j]
    Endelse
  Endfor
  newname = nvn
  Return
End

