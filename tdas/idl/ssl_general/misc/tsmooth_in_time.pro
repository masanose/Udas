;+
;PROCEDURE: tsmooth_in_time, varname, dt, newname = newname
;PURPOSE:
; Calls smooth_in_time function on a plot variable
;INPUT:
; varname = variable passed to get_data, example - thg_mag_ccnv
; dt = the averaging time (in seconds)
;KEYWORDS:
; newname: set output variable name
; 
;HISTORY:
; 11-apr-2008, jmm, jimm@ssl.berkeley.edu
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2010-01-26 12:04:30 -0800 (Tue, 26 Jan 2010) $
;$LastChangedRevision: 7151 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/misc/tsmooth_in_time.pro $
;-

Pro tsmooth_in_time, varname, dt, newname = newname, _extra = _extra,interactive_warning=interactive_warning,warning_result=warning_result

  get_data, varname, data = data, dlimits = dlimits, limits = limits
  If(is_struct(data) Eq 0) Then Begin
    message, /info, 'No data in '+varname
  Endif Else Begin
    y1 = smooth_in_time(data.y, data.x, dt, _extra = _extra,interactive_warning=keyword_set(interactive_warning),warning_result=warning_result)
    if warning_result eq 0 then return
    str_element, data, 'v', success = ok
    If(ok Eq 0) Then data1 = {x:data.x, y:y1} $
    Else data1 = {x:data.x, y:y1, v:data.v}
    If(keyword_set(newname)) then name2 = newname $
    Else name2 = varname+'_smoothed'
    store_data, name2, data = data1, dlimits = dlimits, limits = limits
  Endelse
End
