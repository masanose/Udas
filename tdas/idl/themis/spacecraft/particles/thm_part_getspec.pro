;+
;PROCEDURE: thm_part_getspec
;PURPOSE:
;	Generates tplot variables containing energy-time angular particle spectra
;	for any angle range and/or any energy range.
;
;KEYWORDS:
; probe  = Probe name. The default is 'all', i.e., load all available probes.
;          This can be an array of strings, e.g., ['a', 'b'] or a
;          single string delimited by spaces, e.g., 'a b'
; trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
; phi    = Angle range of interest (2 element array) in degrees relative to
;          probe-sun direction in the probe's spin plane. Specify angles in
;          ascending order (e.g. [270, 450]) to specify the 'daylight'
;          hemisphere in DSL coordinates. Default is all 360 degrees (e.g.
;          [0, 360]). If the phi range is greater than 360 degrees, then the phi
;          bins at the beginning of the phi range are added and wrapped around
;          the end of phi range. For example, if phi=[0,420], the phi bins
;          corresponding to 0-60 degrees are appended to the top of the plot.
;          Also controls which bins are selected for pitch/gyrovelocity spectra.
; theta  = Angle range of interest (2 element array) in degrees relative to
;          spin plane, e.g. [-90, 0] or [-45, 45] in the probe's spin plane.
;          Specify in acending order. Default is all (e.g. [-90, 90]). Also
;          controls which bins are selected for pitch/gyrovelocity spectra.
; pitch  = Angle range of interest (2 element array) in degrees relative to
;          the magnetic field. Default is all (e.g. [0, 180]). Has no effect
;          on phi/theta spectra plots.
; gyro   = Gyrovelocity angle range of interest (2 element array) in degrees.
;          Same rules as specifying phi apply to gyrophse. Default is all
;          (e.g. [0, 360]). Has no effect on phi/theta spectra plots.
; erange = Energy range (in eV) of interest (2 element array). Default is all.
; data_type = The type of data to be loaded (e.g. ['peif', 'peef'])
; start_angle = Angle at which to begin plotting the data the bottom of the
;               plot along the y-axis. At this time this works for phi only. If
;               not set, keyword defaults to first element of phi input.
; suffix = Suffix to add to tplot variable names.
; /AUTOPLOT: Set to activiate a simple plotting routine to display tplot
;            variables created by this routine.
; /ENERGY: Set to output tplot variables containing the energy spectrum of the
;          energy/angle ranges input above.
; angle  = Type of angular spectrum tplot variable created and plotted using the
;          energy/angle ranges input above e.g. 'phi'. Default is 'phi'. The
;          type of angular spectra is appended to its corresponding tplot
;          variable name. All possible options are:
;          phi   = phi angular spectrum
;          theta = theta angular spectrum
;          pa    = pitch angular spectrum (only works with full angle ranges)
;          gyro  = gyrovelocity angular spectrum  (only works with full angle
;                  ranges)
;          NOTE: If neither of ENERGY and ANGLE keywords are specified, then
;                then both are turned on with ANGLE defaulting to 'phi'.
; /GET_SUPPORT_DATA: load support_data variables as well as data variables into
;                    tplot variables.
; regrid = [m,n]  Number of angle bins, used to re-grid (2-element array) along
;          phi (gyrovelocity) and theta (pitch), respectively, when calculating
;          pitch/gyrovelocity angle spectrum. For example, e.g. [32,16] creates
;          a regularly-spaced array of 32 phi angles by 16 theta angles that
;          resample the probe(s) native angle bin array. Default is [16,8].
;          Keyword has no effect on phi and theta spectragrams. Accuracy of the
;          spectragrams increases when more angle bins are used to re-grid.
;          Suitable numbers for m and n are numbers like 2^k. So far, k=2-6 has
;          been tested and work. Other numbers will work provided 180/n and
;          360/m are rational.
; other_dim = Keyword passed to THM_FAC_MATRIX_MAKE for conversion to field
;             aligned coordinates to create pitch angle and gyrovelocity
;             spectra. See THM_FAC_MATRIX_MAKE for valid input. Default is
;             'mphigeo'.
; /NORMALIZE: Set to normalize the flux for each time sample to 0-1.
; badbins2mask = A 0-1 array that indicates which SST bins will be masked with
;                NaN to eliminate things like sun contamination. The array
;                should have the same number of elements as the number of angle
;                bins for a given data type. A 0 indicates that will be masked
;                with a NaN. This is basically the output from the bins argument
;                of EDIT3DBINS.
; datagap = Maximum time gap in seconds over which to interpolate the plot.
;           Sets the DATAGAP flag in the dlimits which SPECPLOT uses to
;           interpolate pixels in time gaps less than DATAGAP.  Use this keyword
;           when overlaying spectra plots, allowing the underlying spectra to be
;           shown in the data gaps of the overlying spectra.  Default is zero.
;
;GUI-RELATED KEYWORDS:
; /GUI_FLAG: Flag tells code to recognize status bar and history window objects.
; gui_statusBar = Object reference to status bar object in GUI.
; gui_historyWin = Object reference to history window object in GUI.
; 
;EXAMPLE: thm_part_getspec, probe='d', trange=['07-06-17','07-06-19']
;
;SEE ALSO:
;	THM_CRIB_PART_GETSPEC, THM_PART_MOMENTS2, THM_PART_GETANBINS, THM_LOAD_SST,
;   THM_LOAD_ESA_PKT, THM_FAC_MATRIX_MAKE,THM_REMOVE_SUNPULSE,THM_CRIB_SST_CONTAMINATION
;
;NOTES:  For documentation on sun contamination correction keywords that
;  may be passed in through the _extra keyword please see:
;  thm_sst_remove_sunpulse.pro or thm_crib_sst_contamination.pro
;
;CREATED BY:	Bryan Kerr
;
;  $LastChangedBy: bckerr $
;  $LastChangedDate: 2008-06-13 13:29:12 -0700 (Fri, 13 Jun 2008) $
;  $LastChangedRevision: 3204 $
;  $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/trunk/idl/themis/spacecraft/particles/thm_part_getspec.pro $
;-

pro thm_part_getspec, probe=probe, trange=trange, phi=phi, theta=theta, $
                      pitch=pitch, erange=erange, data_type=data_type, $
                      start_angle=start_angle, suffix=suffix, $
                      autoplot=autoplot, angle=angle, energy=energy, $
                      nomask=nomask, get_support_data=get_support_data, $
                      regrid=regrid, gyro=gyro, other_dim=other_dim, $
                      en_tnames=en_tnames, an_tnames=an_tnames, $
                      normalize=normalize, bins2mask=bins2mask, $
                      badbins2mask=badbins2mask, datagap=datagap, $ 
                      ; gui-related keywords
                      gui_flag=gui_flag, gui_statusBar=gui_statusBar, $
                      gui_historyWin=gui_historyWin, $
                      ; misc keywords
                      _extra=ex, test=test

;******************************************************************************
; This section warns when obsolete keywords are used.
;******************************************************************************
if keyword_set(nomask) then begin
   print,''
   dprint, 'The /NOMASK keyword has been replaced by the /MASK keyword. ', $
           'Please see documentation (thm_crib_part_getspec) for usage info.'
   print,''
   return
endif

if keyword_set(bins2mask) then begin
   print,''
   dprint, 'The BINS2MASK keyword has been replaced by the BADBINS2MASK ', $
            'keyword. Please see documentation (thm_crib_part_getspec) for usage info.'
   print,''
   return
endif

;******************************************************************************
; Set default input
;******************************************************************************
deftheta = [-90, 90]
defphi = [0, 360]
defpitch = [0, 180]
defgyro = defphi
deferange = [0, 1e7]
defdata_type = 'peif'
def_angle = 'phi'
defstart_angle = 0
wrapphi = 0
def_other_dim='mphigeo'

;******************************************************************************
; Check for correct angle input
;******************************************************************************
if keyword_set(theta) then begin
   if theta[1] lt theta[0] OR (theta[0] lt -90 OR theta[1] gt 90) then begin
      print,''
      dprint, 'Error with theta input. Does not satisfy: -90 < theta[0] < theta[1] < 90.'
      return
   endif
endif else theta=deftheta

if keyword_set(phi) then begin
   if phi[0] gt phi[1] then begin
      print,''
      dprint, 'Error with phi input. Phi angles must be specified in ascending order.'
      return
   endif
   if (phi[1] - phi[0]) gt 360 then begin
      ;if angle eq 'phi' || angle eq 'theta' then wrapphi = phi[1] - phi[0] - 360
      wrapphi = phi[1] - phi[0] - 360
      phi[1] = phi[0] + 360
   endif
endif else phi=defphi

if keyword_set(pitch) then begin
   if pitch[1] lt pitch[0] OR (pitch[0] lt 0 OR pitch[1] gt 180) then begin
      print,''
      dprint, 'Error with pitch input. Does not satisfy: 0 < pitch[0] < pitch[1] < 180.'
      return
   endif
endif else pitch=defpitch

if keyword_set(gyro) then begin
   if gyro[0] gt gyro[1] then begin
      print,''
      dprint, 'Error with gyro input. Gyrovelocity angles must be specified in ascending order.'
      return
   endif
   if (gyro[1] - gyro[0]) gt 360 then begin
      wrapphi = gyro[1] - gyro[0] - 360
      gyro[1] = gyro[0] + 360
   endif
endif else begin
   gyro=defgyro
   if keyword_set(angle) then begin
      if angle eq 'gyro' || angle eq 'pa' then wrapphi = 0
   endif
endelse

;******************************************************************************
; Check for type of spectra requested (e.g. angle or energy)
;******************************************************************************
if ~ keyword_set(angle) AND ~ keyword_set(energy) then begin
   angle = def_angle
   dprint, 'setting angle data_type: ', angle
   energy=1
endif else if ~ keyword_set(angle) then angle = 0

if keyword_set(erange) eq 0 then erange=deferange
if keyword_set(data_type) eq 0 then begin
   data_type = defdata_type
   print,''
   dprint, 'setting default data_type: ', data_type
   print,''
endif

theta1 = min(theta)
theta2 = max(theta)
phi1 = phi[0]
phi2 = phi[1]

if keyword_set(trange) then begin
   tr = trange ; copy trange to internal variable
   trd = time_double(tr)
   ndays = (trd[1] - trd[0]) / 86400
   timespan,trd[0],ndays
endif else begin
   tr = time_string(timerange())
endelse



if keyword_set(badbins2mask) then begin
   if array_equal(badbins2mask, -1) then begin
      dprint,''
      dprint,'WARNING: BADBINS2MASK array is empty. Not masking any SST angle bins.'
      dprint,''
   endif else begin
      dprint,'Masking the following SST angle bins: ', fix(where(badbins2mask eq 0))
   endelse
endif


datatype = data_type

;******************************************************************************
; check for sst data_type requests and load sst data
;******************************************************************************
  sst_ind = where(strmid(datatype,1,1) eq 's', sst_count)
  if sst_count ne 0 then begin
      sst_type = datatype[sst_ind]
      thm_load_sst, probe=probe, trange=trd, datatype=sst_type, $
        get_support_data=get_support_data
  endif

;******************************************************************************
; check for esa data_type requests and load esa data
;******************************************************************************
  esa_ind = where(strmid(datatype,1,1) eq 'e', esa_count)
  if esa_count ne 0 then begin
      esa_type = datatype[esa_ind]
      thm_load_esa_pkt, probe=probe, trange=trd, datatype=esa_type, $
        get_support_data=get_support_data
  endif


;******************************************************************************
; Setup for pitch angle/gyrovelocity spectra. Load state and mag data.
;******************************************************************************
if strcmp(angle,'pa') || strcmp(angle,'gyro') || keyword_set(energy) then begin
   ;If the ENERGY keyword is set, this code is needed so that eflux spectra can
   ;be subject to pitch/gyrovelocity angle restrictions
   if ~ keyword_set(regrid) then begin
      dprint, 'REGRID keyword not set. Defaulting to [16 phis x 8 thetas].'
      regrid=[16,8]
   endif
   
   if ~ keyword_set(other_dim) then other_dim=def_other_dim

   ; move phi range to between 0-360
   if ~ ((phi1 le 360 && phi1 ge 0) && (phi2 le 360 && phi2 ge 0)) then begin
      if phi2 lt (phi2 - phi1) then begin
         phi1_1 = 360 + phi1
         phi1_2 = 360
         phi2_1 = 0
         phi2_2 = phi2
      endif
      if phi2 gt (phi2 - phi1) then begin
         phi1_1 = phi1
         phi1_2 = 360
         phi2_1 = 0
         phi2_2 = phi2 - 360
      endif
   endif else begin
      phi1_1 = phi1
      phi1_2 = phi2
      phi2_1 = phi1
      phi2_2 = phi2
   endelse
   phi[0] = min([phi1_1, phi1_2, phi2_1, phi2_2])
   phi[1] = max([phi1_1, phi1_2, phi2_1, phi2_2])

   coord = 'dsl'
   mag_suffix = '_fgs_'+coord;+'_'+angle
;Check first for state and FGM data for each probe, before loading
;similar to thm_ui_check4spin, but without the common block, jmm, 1-9-2009
   p1 = thm_valid_input(probe, vinputs = ['a b c d e'], definput = 'a', /include_all)
   tr0 = time_double(tr)
   For j = 0, n_elements(p1)-1 Do Begin
       load_state = 1b          ;default is to load
       var1 = tnames('th'+p1[j]+'_state_spinper')
       var2 = tnames('th'+p1[j]+'_state_spinphase')
       If(is_string(var1) && is_string(var2)) Then Begin ;if both are present, check timeranges
;To get the full time range is kind of annoying
         get_data, var1, t1
         dt1 = t1[n_elements(t1)-1]-t1[n_elements(t1)-2]
         t1r = minmax(temporary(t1))
         t1r[1] = t1r[1]+dt1
         get_data, var2, t2
         dt2 = t2[n_elements(t2)-1]-t2[n_elements(t2)-2]
         t2r = minmax(temporary(t2))
         t2r[1] = t2r[1]+dt2
         If((tr0[0] Ge t1r[0] && tr0[1] Le t1r[1]) And $
            (tr0[0] Ge t2r[0] && tr0[1] Le t2r[1])) Then load_state = 0b
       Endif
       If(load_state) Then thm_load_state, probe = p1[j], trange = trd, /get_support_data
       load_fgs = 1b            ;default is to load
       var = tnames('th'+p1[j]+'_fgs_dsl') ;loaded from L2 file
       If(is_string(var)) Then Begin ;check timeranges
         get_data, var, t1
         dt1 = t1[n_elements(t1)-1]-t1[n_elements(t1)-2]
         t1r = minmax(temporary(t1))
         t1r[1] = t1r[1]+dt1
         If((tr0[0] Ge t1r[0] && tr0[1] Le t1r[1])) Then load_fgs = 0b
       Endif
       If(load_fgs) Then thm_load_fgm, probe=p1[j], trange=trd, datatype='fgs', $
         coord='dsl', get_support_data=get_support_data, level = 'l2'
   Endfor
endif

; Calc and create spectra tplot variables
thm_part_moments2, probe=probe, instruments_types=datatype, trange=trd, $
                  tplotnames=tn, theta=theta, phi=phi, pitch=pitch, $
                  erange=erange, tplotsuffix=suffix, start_angle=start_angle, $
                  doangle=angle, doenergy=energy, wrapphi=wrapphi, $
                  mag_suffix=mag_suffix, regrid=regrid, $
                  gyro=gyro, other_dim=other_dim, en_tnames=en_tnames, $
                  an_tnames=an_tnames, normalize=normalize, $
                  badbins2mask=badbins2mask, datagap=datagap, _extra=ex, $
                  test=test, gui_flag=gui_flag, gui_statusBar=gui_statusBar, $
                  gui_historyWin=gui_historyWin

;******************************************************************************
; Setup angle/energy spectrum tplot variables calculated by thm_part_moments2
;******************************************************************************
if keyword_set(angle) then begin
   stheta = strcompress(string(theta))
   sphi = strcompress(string(phi))
   serang = strcompress(string(erange))
   spitch = strcompress(string(pitch))
   sgyro = strcompress(string(gyro))
   title = strjoin(['theta=', stheta, ', phi=', sphi, ', erange=', serang])
   if angle eq 'gyro' || angle eq 'pa' then begin
      title = strjoin(['theta=', stheta, ', phi=', sphi, ', pitch=', spitch, $
                    ', gyro=', sgyro, ', erange=', serang])
   endif
   tplot_options,'title',''
   tplot_options,'xmargin',[15,10]
   options,an_tnames,'y_no_interp',1,/default
   options,an_tnames,'x_no_interp',1,/default
   options,an_tnames,'ztitle','Eflux [eV/cm2/s/ster/eV]',/default
   options,an_tnames,minzlog=1,/default
   zlim,an_tnames,1,1,1,/default
;   ylim,'th*an_eflux*',p_start_angle, p_end_angle,log=0,/default
   ;tdegap, 'th*an_eflux', overwrite=1
endif

if keyword_set(energy) then begin
   stheta = strcompress(string(theta))
   sphi = strcompress(string(phi))
   serang = strcompress(string(erange))
   title = strjoin(['theta=', stheta,', phi=',sphi,', erange=', $
                    serang])
   tplot_options,'title',''
   tplot_options,'xmargin',[15,10]
   options,en_tnames,'y_no_interp',1,/default
   options,en_tnames,'x_no_interp',1,/default
   options,en_tnames,'ztitle','Eflux [eV/cm2/s/ster/eV]',/default
   options,en_tnames,minzlog=1,/default
   zlim,en_tnames,1,1,1,/default
   options,en_tnames,ystyle=1,/default
endif

;******************************************************************************
; Autoplot the angle/energy spectra
;******************************************************************************
if keyword_set(autoplot) && keyword_set(angle) then begin
   tplot_options,'title',title
   tplot,an_tnames
endif
if keyword_set(autoplot) && keyword_set(energy) then begin
   if keyword_set(angle) then begin
      tplot_options,'title',title
      tplot, an_tnames

      ; look for & plot nrg spectra vars w/more than 1 nrg channel
      for i=0,size(en_tnames,/n_elements)-1 do begin
         get_data,en_tnames[i],x,y,v
         if size(v,/n_dimensions) le 1 then begin
            dprint, en_tnames[i]," has only 1 energy channel.  Can't plot ", $
                    'spectra unless ERANGE is larger."
            continue
         endif else begin
            tplot, en_tnames[i], /ADD
         endelse
      endfor

   endif else begin
      tplot_options,'title',title

      ; look for & plot nrg spectra vars w/more than 1 nrg channel
      for i=0,size(en_tnames,/n_elements)-1 do begin
         get_data,en_tnames[i],x,y,v
         if size(v,/n_dimensions) le 1 then begin
            dprint, en_tnames[i]," has only 1 energy channel.  Can't plot ", $
                    'spectra unless ERANGE is larger."
            continue
         endif else begin
            tplot, en_tnames[i], /ADD
         endelse
      endfor

   endelse
endif
end
