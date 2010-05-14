;+
;Procedure: thm_part_dist_array
;
;Purpose: Returns and array of particle distributions for use by THM_PART_SLICE2D.
;
;Details: This is basically a wrapper for THM_PART_DIST that will create an
;         array of particle distribution structures sorted by increasing time.
;         
;
;Keywords:
; FORMAT: A string denoting the data that is desired: options are:
;           'tha_peif': Full Esa mode data, ions, probe A
;           'tha_peef': Full Esa mode data, electrons, probe A
;           'tha_peir': Reduced Esa mode data, ions, probe A
;           'tha_peer': Reduced Esa mode data, electrons, probe A
;           'tha_peir': Burst Esa mode data, ions, probe A
;           'tha_peer': Reduced Esa mode data, electrons, probe A
;           'tha_psif': Full Sst mode data, ions, probe A
;           'tha_psef': Full Sst mode data, electrons, probe A
;           'tha_psir': Reduced Sst mode data, ions, probe A
;           'tha_pser': Reduced Sst mode data, electrons, probe A
;         For other probes, just replace 'tha' with the appropriate string,
;         'thb', 'thc', 'thd', 'the'.  If this is not set, then the string is
;         constructed from the type and probe keywords.
; TRANGE: Time range of interest (2 element array).
; TYPE: Four character string denoting the type of data that you need,
;       e.g., 'peif' for full mode esa data.
; PROBE: The THEMIS probe, 'a','b','c','d','e'.
; MAG_DATA: Tplot variable containing magnetic field data that you want added
;           to the dat structures.
; VEL_DATA: Tplot variable containing velocity data that you want added to dat
;           structure under the tag name 'velocity'.  Overrides VEL_AUTO keyword.
;           The velocity data will be intpolated to the cadence of the particle
;           distributions.
; VEL_AUTO: Automatically calculates velocity at each time sample using V_3D.PRO
;           and adds it to the dat structure with the tag name 'velocity'.
;
;See Also: THM_PART_SLICD2D, THM_CRIB_PART_SLICE2D, THM_PART_MAKE_DIST,
;          THM_PART_DIST_VELS, TINTERPOL_MXN, ADD_MAGF, V_3D
;
;Created by Bryan Kerr
;-

function thm_part_dist_array, format=format, trange=trange, type=type, $
                              probe=probe, mag_data=mag_data, vel_data=vel_data, $
                              vel_auto=vel_auto

if keyword_set(format) then begin
   probe = strmid(format,2,1)
   type  = strmid(format,4,4)
endif else format = 'th'+probe+'_'+type

probe=strlowcase(probe)
inst = strmid(type,1,1)
species = strmid(type,2,1)

; check requested probe
dummy = where(probe eq ['a','b','c','d','e'], yes_probe)
if yes_probe lt 1 then begin
  dprint, 'Invalid proble: ' + probe
  return, -1
endif

; check requested instrument type
if inst ne 'e' && inst ne 's' then begin
  dprint, 'Invalid instrument type: ' + inst
  return, -1
endif

; check requested species
if species ne 'e' && species ne 'i' then begin
  dprint, 'Invalid species: ',species
  return, -1
endif

; make sure timespan gets set
if keyword_set(trange) then begin
   tr = trange ; copy trange to internal variable
   trd = time_double(tr)
   ndays = (trd[1] - trd[0]) / 86400
   timespan,trd[0],ndays
endif else begin
   tr = time_string(timerange())
endelse

; load L1 data
if inst eq 'e' then begin
  thm_load_esa_pkt, probe=probe, trange=tr, datatype=type
endif else begin
  thm_load_sst, probe=probe, trange=tr, datatype=type
endelse


;;;;;  begin get time indexes of data in requested time range  ;;;;;;;;;;;;;;;;;
times = thm_part_dist(format,/times)
if size(times,/type) ne 5 then begin
  dprint, 'No th',probe,'_',inst,' data for time range ',time_string(trange[0]), $
           ' to ',time_string(trange[1]),'.'
  return, -1
endif 

;time correction to point at bin center is applied for ESA, but not for SST
if strmid(inst,1,1) eq 's' then begin
  times += 1.5
endif

if keyword_set(trange) then tr = minmax(time_double(trange)) else tr=[1,1e20]
time_ind = where(times ge tr[0] and times le tr[1], ns)
;;;;;  end get time indexes of data in requested time range  ;;;;;;;;;;;;;;;;;;;

if keyword_set(vel_data) then begin

  ;TODO: check that tplot var has valid data (tinterpol_mxn should take care of this initially)
  ;TODO: check that tplot data is in time window (tinterpol_mxn should take care of this initially)

  ; interpolate velocity data
  tinterpol_mxn, vel_data, times[time_ind], /nan_extrapolate, error=success

  if success then begin
    get_data, vel_data+'_interp', data=d
    vel = d.y
  endif else begin
    err_mess = 'Error interpolating velocity data. Not adding velocity data to dat structure.'
    dprint, err_mess
    vel_data=0
  endelse
endif


for i=0L,ns-1 do begin

  dat = thm_part_dist(format, index=time_ind[i])

  ; add mag data to dat structure
  if keyword_set(mag_data) then add_magf, dat, mag_data
  
  if keyword_set(vel_data) then begin
    ; add user-input velocity data to data structure
    add_str_element, dat, 'velocity', vel[i]    
  endif else begin
    if keyword_set(vel_auto) then begin
      ; calculate velocity in m/s and add to dat stucture
      vel=v_3d(dat)*1000.
      add_str_element, dat, 'velocity', vel
    endif
  endelse
  
  ; append current dat structure to array of dat structures
  if i eq 0 then dist_arr=dat else dist_arr=[dist_arr, dat]

endfor

return, dist_arr
end