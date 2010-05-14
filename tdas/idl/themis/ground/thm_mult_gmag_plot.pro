;+
; NAME:
;       thm_mult_gmag_plot.pro
; PURPOSE:
;       Plots all GMAG data, in groups of 3 (or anything set by
;       keyword n_per_page) for quick viewing
; CALLING SEQUENCE:
;       thm_mult_gmag_plot, date, duration
; INPUTS:
;       date: The start of the time interval to be plotted. (Format:
;       'YYYY-MM-DD/hh:mm:ss')
;	duration: The length of the interval being plotted. (Floating
;	point number of days -> 12hr=0.5), default=1
;	no_data_load:  This keyword prevents new data from being
;	loaded; the routine will try to plot existing data if it
;	exists.
;       n_per_page: plots this many stations at a time, default is 3
; OUTPUTS:
;       Plots...
; MODIFICATION HISTORY:
;       jmm, 1-sep-2009, jimm@ssl.berkeley.edu
; $LastChangedBy: jimm $
; $LastChangedDate: 2009-09-02 10:23:46 -0700 (Wed, 02 Sep 2009) $
; $LastChangedRevision: 6672 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/ground/thm_mult_gmag_plot.pro $
;-

Pro thm_mult_gmag_plot, date, duration, no_data_load = no_data_load, $
                        n_per_page = n_per_page, winsize = winsize, $
                        _extra = _extra
;________________________________________________________________________________________________
;find all gmag CDF files on given date and put data into tplot variables
;________________________________________________________________________________________________
  if not keyword_set(date) then begin
    print, 'You must specify a date.  (Format : YYYY-MM-DD/HH:MM:SS)'
    return
  endif

  if not keyword_set(duration) then duration = 1.

  start_time = time_double(date)
  end_time = start_time+86400.*duration

  timespan, date, duration
  if not keyword_set(no_data_load) then begin
    del_data, 'thg_mag_*'
    thm_load_gmag, site = '????', /subtract_median
  endif
;________________________________________________________________________________________________
;check that there is some valid data
;________________________________________________________________________________________________
  tplotvars = tnames('thg_mag_????')
  if tplotvars[0] eq '' then begin
    print, 'Appropriate tplot variables could not be found!'
    print, 'Searched for "thg_mag_????"'
    print, 'plot program was aborted.'
    return
  endif
;sort stations by longitude
  stations = tplotvars
  lats = fltarr(n_elements(stations))
  lons = fltarr(n_elements(stations))
  nstations = n_elements(stations)
  for i = 0, nstations-1 do begin
    get_data, stations[i], dlimits = dl
    lats[i] = float(dl.cdf.vatt.station_latitude)
    lons[i] = float(dl.cdf.vatt.station_longitude)
  endfor
  lat_index = reverse(sort(lats))
  stations = stations[lat_index]
  stations0 = stations
  original_device = !d.name     ;save to reset
  osf = strupcase(!version.os_family)
  If(osf Eq 'WINDOWS') Then set_plot, 'win' Else set_plot, 'x'
  If(n_elements(winsize) Eq 2) Then Begin
    window, 0, xsize = winsize[0], ysize = winsize[1]
  Endif Else Begin
    screen_size = get_screen_size()
    xss = screen_size/3.0
    xss = 100.0*fix(xss[0]/100)
    window, 0, xsize = xss, ysize = screen_size(1)*0.9
  Endelse
  tplot_options, 'region', [0.00, 0.00, 0.95, 1.]
  !p.color = 0
  !p.background = 255
  !p.charsize = 1.0
  time_stamp, /off
  date_compress = strcompress(strmid(date, 0, 4)+strmid(date, 5, 2)+strmid(date, 8, 2), /remove_all)
  timespan, date, duration
;--------------------------
;plot away
  If(keyword_set(n_per_page)) Then npp = n_per_page $
  Else npp = 3
  nx = ceil(nstations/float(npp)) ;this will be the number of plots
  stations0 = stations          ;hold full array here
  for w = 0, nx-1 do begin
    wch = strcompress(/remove_all, string(w))
    x0 = w*npp
    x1 = (x0+npp-1) < (nstations-1)
    stations = stations0[x0:x1]
    thetitle = 'Sample: '+wch
    print, 'PLOTTING:', stations
    tplot, stations,  window = 0, title = thetitle
    wshow, 0
    print, 'Hit Enter to Continue:'
    xxx = strarr(1)
    read, xxx
  endfor                        ; w
  set_plot, original_device

end

