;+
;NAME:
; tpwrspc
;PURPOSE:
; wrapper for pwrspc.pro allowing input of a tplot variable name.
;    A Hanning window is applied to
;    the input data, and its power is divided out of the returned
;    spectrum. A straight line is subtracted from the data to
;    reduce spurius power due to sawtooth behavior of a background.
;    UNITS ARE (UNITS)^2 WHERE UNITS ARE THE UNITS OF quantity. freq
;    is in 1/timeunits.
;    THUS THE OUTPUT REPRESENTS THE MEAN SQUARED AMPLITUDE OF THE SIGNAL
;       AT EACH SPECIFIC FREQUENCY. THE TOTAL (SUM) POWER UNDER THE CURVE IS
;       EQUAL TO THE MEAN (OVER TIME) POWER OF THE OSCILLATION IN TIME DOMAIN.

;CALLING SEQUENCE:
; 
;CALLING SEQUENCE:
; tpwrspc, varnames, newname=newname,_extra=_extra
;INPUT:
; varname = one tplot variable name
;KEYWORDS:
; newname = if set,give this name to the new data, the
;           default is to append '_pwrspc' to the input name and
;           pass out the name in the newname variable,
;           Unless /overwrite is set
; overwrite = if set, write the new data back to the old tplot
;             variable, do not set this with newname
; noline = if set, no straight line is subtracted
; nohanning = if set, then no hanning window is applied to the input
; bin = a binsize for binning of the frequency data, the default is 3
; notperhz = if set, the output units are simply the square of the
;            input units 
; err_msg = named variable that contains any error message that might occur 
; 
; NOTES: 1. IF KEYWORD notperhz IS SET, THEN POWER IS IN UNITS^2. If notset
;           power is (as normal) in UNITS^2/Hz.
;        2. Inputs must be 1-dimensional.   For example, if you try to
;            call this on a 3-d vector like fgs data, it will not work.
;            call 'split_vec' first, to split the quantity into its components.        
;      
;HISTORY:
; 27-mar-2007, jmm, jimm.ssl.berkeley.edu
;
;$LastChangedBy$
;$LastChangedDate$
;$LastChangedRevision$
;$URL$
;-
Pro tpwrspc, varname, newname = newname, $
             trange = trange, _extra = _extra

;First extract the data
  If(keyword_set(newname)) Then begin
    If(keyword_set(overwrite)) Then begin
      message, /info, 'Do not set both the newname and overwrite keywords'
      return
    Endif
    nvn = newname
  Endif Else nvn = varname+'_pwrspc'
;Now do the power spectrum
  get_data, varname, data = d, dlim = dlim, lim = lim
  If(is_struct(d)) Then Begin
    y = d.y
    t = (d.x-d.x[0])
    If(n_elements(trange) Eq 2) Then Begin
      tr = time_double(trange)
      ok = where(d.x Ge tr[0] And d.x Lt tr[1], nok)
      If(nok Eq 0) Then Begin
        message, /info, 'No data in time range'
        print, time_string(tr)
        message, /info, 'No Power spectrum for: '+varname
        Return
      Endif Else Begin
        t = t[ok] & y = y[ok]
      Endelse
    Endif
    t00 = d.x[0]
    pwrspc, t, y, f, p, _extra = _extra
    d = {x:temporary(f), y:temporary(p)}
    If(keyword_set(overwrite)) Then newname = varname $
    Else newname = nvn
    str_element, dlim, 'data_type', 'power_spectrum', /add
    str_element, dlim, 'xtitle', 'Freq (Hz)'
    store_data, newname, data = d, dlim = dlim, lim = lim
    xlim, newname, min(d.x), max(d.x), 0
    options, newname, spec = 0, ylog = 1, ystyle = 1, $
      yrange = [min(d.y), max(d.y)], $
      xrange = [min(d.x), max(d.x)]
   Endif Else Begin
    message, /info, 'No Power spectrum for: '+varname
  Endelse
  newname = nvn
  Return
End

