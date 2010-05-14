;+
;NAME:
; tdegap
;PURPOSE:
; wrapper for xdegap.pro allowing input of tplot variable names
;CALLING SEQUENCE:
; tdegap, varnames, dt=dt, margin=margin, maxgap=maxgap,$
;         newname=newname, overwrite=overwrite
;INPUT:
; varnames = an array (or scalar) of tplot variable names
;KEYWORDS:
; dt = the nominal time resolution of the data that will be inserted,
;      the default is to choose the median of the input time array
; margin = the margin used to determine if a gap is big enough, the
;          default is 0.25 seconds
; maxgap = the maximum gap size that will be allowed to be filled, in
;          units of dt. the default is to set this to the max number
;          of data points
;          (TDEGAP degaps anything that is greater than dt+margin 
;          and less than maxgap*dt)
; newname = if set,give these names to the degapped data, the
;                default is to append '_degap' to the input names and
;                pass out the names in the newname variables,
;                Unless /overwrite is set
; overwrite = if set, write the new data back to the old tplot
;             variables, do not set this with newname
; (Keywords passed to XDEGAP:)
; nowarning = if set, suppresses warnings
; flag = A numeric user-specified value to use for flagging gaps.
;   Defaults to a floating NaN.  If an array is entered, only the
;   first element is considered.If a non-numeric datatype is entered,
;   its value is ignored.
; onenanpergap = Fill gaps with only one NaN -> useful for conserving memory.
;   Also, for reference concerning post-processing, the INTERPOL function
;   propagates a single NaN just as it would many NaNs.
;HISTORY:
; 9-apr-2007, jmm, jimm.ssl.berkeley.edu
; 10-oct-2008, jmm, Degaps v tags if necessary
;
;$LastChangedBy$
;$LastChangedDate$
;$LastChangedRevision$
;$URL$
;-
Pro tdegap, varnames_in, dt = dt, margin = margin, $
            newname = newname, overwrite = overwrite, $
            _extra = _extra

  varnames = tnames(varnames_in)  ;for wild cards, etc....
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
  Endif Else nvn = varnames+'_degap'
;Now do the degapping
  If(keyword_set(margin)) Then mar = margin Else mar = 0.25
  For j = 0, n-1 Do Begin
    get_data, varnames[j], data = d, dlim = dlim, lim = lim
    If(is_struct(d)) Then Begin
      x = d.x
      If(n_elements(x) Le 1) Then Begin
        message, /cont, 'Only one X value, No Degapping of: '+varnames[j]
        nvn[j] = ''
      Endif Else Begin
        y = d.y
        If(keyword_set(dt)) Then dx = dt Else Begin
          dx = median(x[1:*]-x)
        Endelse
;may need to degap v's depending on size
        str_element, d, 'v', v, success = yes_v
        str_element, d, 'v2', v2, success = yes_v2
        If(yes_v) Then Begin
          vsz = size(v, /dim) & ysz = size(y, /dim)
          If(n_elements(vsz) Eq n_elements(ysz) && total(vsz-ysz) Eq 0) Then degap_v = 1b $
          Else degap_v = 0b
        Endif Else degap_v = 0b
        If(yes_v2) Then Begin
          vsz = size(v2, /dim) & ysz = size(y, /dim)
          If(n_elements(vsz) Eq n_elements(ysz) && total(vsz-ysz) Eq 0) Then degap_v2 = 1b $
          Else degap_v2 = 0b
        Endif Else degap_v2 = 0b
        xdegap, dx, mar, temporary(x), temporary(y), $
          x_out, y_out, iindices = ii, _extra = _extra
        If(x_out[0] Ne -1) Then Begin
          If(degap_v) Then Begin
            v_out = make_array(dimension = size(y_out, /dim), type = size(v, /type), $
                               value = 'NaN')
            v_out[ii, *] = temporary(v)
            str_element, d, 'v', temporary(v_out), /add_replace
          Endif
          If(degap_v2) Then Begin
            v_out = make_array(dimension = size(y_out, /dim), type = size(v2, /type), $
                               value = 'NaN')
            v_out[ii, *] = temporary(v2)
            str_element, d, 'v2', temporary(v_out), /add_replace
          Endif
          str_element, d, 'y', temporary(y_out), /add_replace
          str_element, d, 'x', temporary(x_out), /add_replace

          If(keyword_set(overwrite)) Then new_name = varnames[j] $
          Else new_name = nvn[j]
          store_data, new_name, data = d, dlim = dlim, lim = lim
        Endif Else Begin
          message, /info, 'Invalid output, No Degapping of: '+varnames[j]
          nvn[j] = ''
        Endelse
      Endelse
    Endif Else Begin
      message, /info, 'Invalid input, No Degapping of: '+varnames[j]
      nvn[j] = ''
    Endelse
  Endfor
  newname = nvn
  Return
End

