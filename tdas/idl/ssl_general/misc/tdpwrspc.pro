;+
;NAME:
; tdpwrspc
;PURPOSE:
; wapper for dpwrspc.pro allowing input of a tplot variable name
;CALLING SEQUENCE:
; tdpwrspc, varname, newname=newname,_extra=_extra
;INPUT:
; varname = one tplot variable name
;KEYWORDS:
; newname = if set,give this name to the new data, the
;           default is to append '_dpwrspc' to the input name and
;           pass out the name in the newname variable,
;           Unless /overwrite is set
; overwrite = if set, write the new data back to the old tplot
;             variable, do not set this with newname
;             
; nboxpoints = the number of points to use for the hanning window, the
;              default is the closest power of 2 less than the number of points divided by 32
; nshiftpoints = the number of points to shift the hanning window per-step, the default in nboxpoints/2
; 
; bin = a binsize for binning of the data along the frequency domain, the default is 3
; tbegin = a start time, the default is time[0] 
; tend = an end time, the default is time[n_elements(time)-1]
; noline = if set, no straight line is subtracted
; nohanning = if set, then no hanning window is applied to the input
; notperhz = if set, the output units are simply the square of the
;            input units 
;HISTORY:
; 27-mar-2007, jmm, jimm.ssl.berkeley.edu
; 10-apr-2007, jmm, fixed 2 bugs wrt structure definition
;
;$LastChangedBy$
;$LastChangedDate$
;$LastChangedRevision$
;$URL$
;-
Pro tdpwrspc, varname, newname = newname, $
              trange = trange, nboxpoints=nboxpoints,$
              nshiftpoints=nshiftpoints,_extra = _extra

;First extract the data
  If(keyword_set(newname)) Then begin
    If(keyword_set(overwrite)) Then begin
      message, /cont, 'Do not set both the newname and overwrite keywords'
      return
    Endif
    nvn = newname
  Endif Else nvn = varname+'_dpwrspc'
;Now do the power spectrum
  get_data, varname, data = d, dlim = dlim, lim = lim
  If(is_struct(d)) Then Begin
    y = d.y
    sizey = size(y)
    If(sizey[0] Ne 1) Then Begin
      message, /cont, 'Inappropriate Data Input: Y must be 1d: '+varname
      return
    Endif
    t = (d.x-d.x[0])
    If(n_elements(trange) Eq 2) Then Begin
      tr = time_double(trange)
      ok = where(d.x Ge tr[0] And d.x Lt tr[1], nok)
      If(nok Eq 0) Then Begin
        message, /cont, 'No data in time range'
        print, time_string(tr)
        message, /cont, 'No Dynamic Power spectrum for: '+varname
        Return
      Endif Else Begin
        t = t[ok] & y = y[ok]
      Endelse
    Endif
;Filter out NaN's
    Ok = where(finite(d.y), nok)
    If(nok Eq 0) Then Begin
      message, /cont, 'No finite data in time range'
      Return
    Endif Else Begin
      t = t[ok] & y = y[ok]
    Endelse
    t00 = d.x[0]
    
    ;Only do this if there are enough data points, default nboxpoints to
    ;64 and nshiftpoints to 32, and use larger values when there are more
    ;points
    if ~keyword_set(nboxpoints) then begin
      nbp = max([2^(floor(alog(nok)/alog(2),/l64)-5),8])
    endif else begin
      nbp = nboxpoints
    endelse
    
    if ~keyword_set(nshiftpoints) then begin
      nsp = nbp/2
    endif else begin
      nsp = nshiftpoints
    endelse

    If(nok Le nbp) Then Begin
      message, /cont, 'Not enough data in time range'
      Return
    Endif

    dpwrspc, t, y, tp, f, p, nboxpoints = nbp, nshiftpoints = nsp, _extra = _extra
    d = {x:temporary(tp+t00), y:temporary(p), v:temporary(f)}
    If(tp[0] Ne -1) Then Begin
      If(keyword_set(overwrite)) Then newname = varname $
      Else newname = nvn
      str_element, dlim, 'data_type', 'dynamic_power_spectrum', /add
      str_element, dlim, 'ytitle', 'Freq (Hz)'
      store_data, newname, data = d, dlim = dlim, lim = lim
;some other options;
      options, newname, spec = 1, ylog = 1, zlog = 1, ystyle = 1
    Endif Else Begin
      message, /cont, 'No Power spectrum for: '+varname
    Endelse
  Endif Else Begin
    message, /cont, 'No Power spectrum for: '+varname
  Endelse
  newname = nvn
  Return
End

