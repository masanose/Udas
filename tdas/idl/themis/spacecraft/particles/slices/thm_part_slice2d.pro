;+
;Procedure: thm_part_slice2d
;
;Purpose: Returns a 2-D slice of the 3-D THEMIS ESA/SST ion or electron distribution
;         function.  The 2-D slice is returned via the VPARA, VPERP, and ZDATA
;         keywords.
;
;Arguments:
; datArray: An array of data structures as returned by one of the get_th?_p???
;            routines.  The structure needs to contain a magf field containing
;            a three-component magnetic field vector.
;
;Input Keywords:
; SLICE_TIME: Beginning of time window in seconds since Jan. 1, 1970.  If
;             CENTER_TIME keyword set, then TIME is the center of the time widow
;             specified by the TIMEWIN keyword.
; CENTER_TIME: Flag that, when set, centers the time window set in TIMEWIN
;              around the time specified with the TIME keyword.
; TIMEWIN: Length in seconds over which to compute the slice.
; SLICE_ORIENT: [x, y, z, vel] A four-element array that specifies the
;               orientation of the slice plane.  The first three elements
;               specify the direction of the plane's normal vector.  The fourth
;               element is the distance of the slice plane's center from the
;               origin along the plane's normal vector in units of meters/sec. 
; RESOLUTION: A single integer specfying the length, width, and height of the
;             cube grid used to interpolate the particle distribution.
; ROTATION: suggesting the x and y axis, which can be specified as the following:
;   'BV': the x axis is V_para (to the magnetic field) and the bulk velocity is in the x-y plane. (DEFAULT)
;   'BE': the x axis is V_para (to the magnetic field) and the VxB direction is in the x-y plane.
;   'xy': the x axis is V_x and the y axis is V_y.
;   'xz': the x axis is V_x and the y axis is V_z.
;   'yz': the x axis is V_y and the y axis is V_z.
;   'perp': the x-y plane is perpendicular to the B field, while the x axis is the velocity projection on the plane.
;   'perp_xy': the x-y plane is perpendicular to the B field, while the x axis is the x projection on the plane.
;   'perp_xz': the x-y plane is perpendicular to the B field, while the x axis is the x projection on the plane.
;   'perp_yz': the x-y plane is perpendicular to the B field, while the x axis is the y projection on the plane.
; ERANGE: specifies the energy range to be used
; REMOVEZERO: removes the data with zero counts for plotting
; SUBTRACT: subtract the bulk velocity before plot
;
;Output Keywords for 2-D plotting:
; PART_SLICE: The 2-D array of flux (the slice that you want to plot).
; XGRID: Array of x-locations for the slice grid.
; YGRID: Array of y-locations for the slice grid.
; ZGRID: Array of z-locations for the slice grid.
; 
;Output Keywords for 3-D plotting:
; VPARA: Variable to return the x-axis location of the zdata.
; VPERP1: Variable to return the y-axis location of the zdata.
; VPERP2: Variable to return the z-axis location of the zdata.
; ZDATA: Variable to return the z data (the flux).
;
; 
; The following keywords are used to pass variables back to the calling that are
; useful for plotting.
; ROT_MATRIX: The rotation matrix created by setting the ROTATION keyword
; 
;CREATED BY: Bryan Kerr based on Arjun Raj original thm_esa_slice2d.pro
;EXAMPLES:  see the crib file: thm_crib_part_slice2d.pro
;-

pro thm_part_slice2d, datArray, vpara=vpara, vperp1=vperp, vperp2=vperp2, $
                      zdata=zdata, slice_time=slice_time, $
                      center_time=center_time, timewin=timewin, $
                      slice_orient=slice_orient, resolution=resolution, $
                      part_slice=part_slice, xgrid=xgrid, ygrid=ygrid, zgrid=zgrid, $
                      rotation=rotation, erange=erange, removezero=removezero, $
                      subtract=subtract, $
                      ; passing back to calling program for plotting purposes
                      rot_matrix=rot_matrix, $
                      _EXTRA = e

if keyword_set(removezero) then leavezero=0 else leavezero=1

if not keyword_set(subtract) then nosubtract = 1

if not keyword_set(rotation) then rotation='BV'

if ~keyword_set(slice_time) then begin
  dprint, 'Please specifiy a time at which to compute the slice.'
  return
endif

if ~keyword_set(timewin) then begin
  dprint, 'Please specifiy a time window for the slice."
  return
endif

; get center times of each dat structure
times = datArray.time + (datArray.end_time - datArray.time)/2

; get the boundaries of the time window
if keyword_set(center_time) then begin
  time_min = slice_time - timewin/2
  time_max = slice_time + timewin/2
endif else begin
  time_min = slice_time
  time_max = slice_time + timewin
endelse

; get indexes of dat structures in reqested time window
times_ind = where(times ge time_min AND times le time_max, ndat)

if ndat gt 0 then begin
  datStructs = datArray[times_ind]
endif else begin
  dprint, 'No particle distributions in datArray that are within the requested time window.'
  return
endelse


; %%% LOOP START HERE  %%%

for in=0,ndat-1 do begin
  thedata=datStructs[in]
  
  bins_2d=fltarr(thedata.nenergy,thedata.nbins)
  for i=0,thedata.nbins-1 do begin
  ;    bins_2d[*,i]=thedata.bins[i]
      bins_2d[*,i]=thedata.bins[*,i]
  endfor
 
  if thedata.valid ne 1 then begin
    print,'Not valid data'
    verp=0 & vpara=0 & zdata=0
;    return, [0,0,0]
  endif
  

  ;*****************************
  
  ;In order to find out how many particles there are at all the different locations,
  ;we must transform the data into cartesian coordinates.
    
  totalx = fltarr(1) & totaly = fltarr(1) & totalz = fltarr(1)
  ncounts = fltarr(1)
  
  ; modify requested erange to values of bins in that range
  if not keyword_set(erange) then begin
    ;erange = [thedata.energy[thedata.nenergy-1,0],thedata.energy[0,0]]
    erange = [min(thedata.energy), max(thedata.energy)]
    eindex = indgen(thedata.nenergy)
  endif else begin
    eindex = where(thedata.energy[*,0] ge erange[0] and thedata.energy[*,0] le erange[1])
    erange = [min(thedata.energy[eindex,0]),max(thedata.energy[eindex,0])]
  endelse
  
  mass = thedata.mass / 6.2508206e24
  
  for i = 0, thedata.nenergy-1 do begin
    ; active and finite bins in specified erange
    currbins = where(bins_2d[i,*] ne 0 and thedata.energy[i,*] le erange[1] and thedata.energy[i,*] ge erange[0] and finite(thedata.data[i,*]) eq 1,nbins)

    if nbins ne 0 then begin
      ;print, i
      x = fltarr(nbins) & y = fltarr(nbins) & z = fltarr(nbins)
      
      ; convert sphere coord of each currbin to cartesian coord
      sphere_to_cart,1,reform(thedata.theta[i,currbins]),reform(thedata.phi[i,currbins]),x,y,z

      ; x,y,z components of velocity of each currbin for current nrg channel
      ; (for full nrg range, x,y, and z will be array sized nenergy*n_angbins (e.g. 32*88))
      totalx = [totalx, x * reform(sqrt(2*1.6e-19*thedata.energy[i,currbins]/mass))]
      totaly = [totaly, y * reform(sqrt(2*1.6e-19*thedata.energy[i,currbins]/mass))]
      totalz = [totalz, z * reform(sqrt(2*1.6e-19*thedata.energy[i,currbins]/mass))]
  
      ; array of counts for each nrg/angle bin...same size as x
      ncounts = [ncounts,reform(thedata.data[i, currbins])]
    endif
  endfor
  
  ; remove the first element, which is zero, created by array initialization
  totalx = totalx[1:*]
  totaly = totaly[1:*]
  totalz = totalz[1:*]
  ncounts = ncounts[1:*]
  
  ; sum of counts at each time sample
  if in eq 0 then begin
    ncounts_t = ncounts
  endif else begin
    ncounts_t = ncounts_t+ncounts
  endelse

endfor
ncounts=ncounts_t/(ndat+1) ; average counts over number of time samples
;  %%%  LOOP ENDS HERE  %%%


;*****HERES SOMETHING NEW (Arjun Raj)

; create stuct containing x,y,z, and ncounts arrays
newdata = {dir:fltarr(n_elements(totalx),3), n:fltarr(n_elements(totalx))}

newdata.dir[*,0] = totalx
newdata.dir[*,1] = totaly
newdata.dir[*,2] = totalz
newdata.n = ncounts


  ;**********************************************



;************EXPERIMENTAL INTERPOLATION FIX************ (Arjun Raj)
;get_data,magdata,data = bdata
;index = where(bdata.x le thedata.time + 600 and bdata.x ge thedata.time - 600)
;store_data,magdata+'cut',data={x:bdata.x[index],y:bdata.y[index,*]}
;********

;store_data,'time',data = {x:thedata.time+thedata.integ_t*.5}
;print, thedata.integ_t, ' Thedata.integ_t'
;interpolate,'time',magdata+'cut','Bfield'
;get_data,'Bfield',data = mgf
;bfield = fltarr(3)
;bfield[0] = mgf.y[0,0]
;bfield[1] = mgf.y[0,1]
;bfield[2] = mgf.y[0,2]

if keyword_set(magdata) then begin
  ;get the magnetic field into a variable
  get_data, magdata, data=mgf
  
  ; average mag data over time window
  bfield = dat_avg(magdata, datStructs[0].time, thedata.end_time)
endif else begin
  ; average mag data over time window
  bfield = average(datStructs.magf, 2)
endelse

if keyword_set(nosubtract) then print,'No velocity transform' else begin
  if keyword_set(vel) then print,'Velocity used for subtraction is '+vel else print, 'Velocity used for subtraction is V_3D'
endelse

; calculate bulk velocity from dat structure
dprint, 'Using velocity tag from dat structure.'
thevel = average(datStructs.velocity, 2)


; subtract bulk velocity vector
if not keyword_set(nosubtract) then begin
  newdata.dir[*,0] = newdata.dir[*,0] - thevel[0]*factor
  newdata.dir[*,1] = newdata.dir[*,1] - thevel[1]*factor
  newdata.dir[*,2] = newdata.dir[*,2] - thevel[2]*factor
endif

;**************NOW CONVERT TO THE DATA SET REQUIRED*****************

; create rotation matrix
if rotation eq 'BV' then rot=cal_rot(bfield,thevel)
if rotation eq 'BE' then rot=cal_rot(bfield,crossp(bfield,thevel))
if rotation eq 'xy' then rot=cal_rot([1,0,0],[0,1,0])
if rotation eq 'xz' then rot=cal_rot([1,0,0],[0,0,1])
if rotation eq 'yz' then rot=cal_rot([0,1,0],[0,0,1])
if rotation eq 'xvel' then rot=cal_rot([1,0,0],thevel)
if rotation eq 'perp' then begin
  rot=cal_rot(crossp(bfield,crossp(bfield,thevel)),crossp(bfield,thevel))
endif
if rotation eq 'perp_yz' then begin
  rot=cal_rot(CROSSP(CROSSP(bfield,[0,1,0]),bfield),CROSSP(CROSSP(bfield,[0,0,1]),bfield))
endif
if rotation eq 'perp_xy' then begin
  rot=cal_rot(CROSSP(CROSSP(bfield,[1,0,0]),bfield),CROSSP(CROSSP(bfield,[0,1,0]),bfield))
endif
if rotation eq 'perp_xz' then begin
  rot=cal_rot(CROSSP(CROSSP(bfield,[1,0,0]),bfield),CROSSP(CROSSP(bfield,[0,0,1]),bfield))
endif

; pass rotation matrix back out to calling routine
rot_matrix=rot

; rotate data
newdata.dir = newdata.dir#rot

factor = 1000.
vperp = newdata.dir[*,1]/factor
vpara = newdata.dir[*,0]/factor
vperp2= newdata.dir[*,2]/factor
zdata = newdata.n

;**********************

; TODO: move this code outside of file
;if not keyword_set(vel) then veldir = v_3d(thedata) else veldir = thevel/1000.
;veldir = veldir#rot
;velocity_dir = veldir

;EXPERIMENTAL GET RID OF 0 THING************* (Arjun Raj)
if not keyword_set(leavezero) then begin
  index = where(zdata ne 0)
  vperp = vperp[index]
  vpara = vpara[index]
  vperp2=vperp2[index]
  zdata = zdata[index]
endif else print, 'Zeros left in plot'

;MAKE SURE THERE ARE NO NEGATIVE VALUES!! *********** (Arjun Raj)
index2 = where(zdata lt 0., count)
if count ne 0 then print,'THERE ARE NEGATIVE DATA VALUES'

index = where(zdata ge 0,count)
if count ne 0 then begin
  vperp = vperp[index]
  vperp2= vperp2[index]
  vpara = vpara[index]
  zdata = zdata[index]
endif


vperp=vperp[sort(vpara)]
zdata=zdata[sort(vpara)]
vperp2=vperp2[sort(vpara)]
vpara=vpara[sort(vpara)]


;;;;;;  new slice code here  ;;;;;;;
qhull, vpara, vperp, vperp2, tetrahedra, /DELAUNAY

; create unit vector
slice_norm = float(slice_orient[0:2])
slice_loc = float(slice_orient[3])
unit_norm = slice_norm / sqrt(total(slice_norm^2)) ; convert slice normal to a unit vec

xdimSize = resolution
ydimSize = resolution
zdimSize = resolution

cubeSize = [xdimSize, ydimSize, zdimSize]

velmm = minmax([vpara, vperp, vperp2])

xgrid = interpol(velmm, xdimSize)
ygrid = interpol(velmm, ydimSize)
zgrid = interpol(velmm, zdimSize)

x_center = (unit_norm[0] * slice_loc - velmm[0]) /(velmm[1]-velmm[0]) * resolution
y_center = (unit_norm[1] * slice_loc - velmm[0]) /(velmm[1]-velmm[0]) * resolution
z_center = (unit_norm[2] * slice_loc - velmm[0]) /(velmm[1]-velmm[0]) * resolution


vol=qgrid3(vpara, vperp, vperp2, zdata, tetrahedra, dimension=cubeSize)

a = crossp([1,0,0], unit_norm)
if total(abs(a)) eq 0. then begin
  if unit_norm[0] lt 0 then xvec=[0, 0, 1] else xvec=[0, 0, -1]
endif else begin
  ; normalize a
  a = a / sqrt(total(a^2))
  b = crossp(a, unit_norm)
  xvec = -b
endelse

part_slice=extract_slice(vol, xdimSize, ydimSize, x_center, y_center, z_center, $
                         unit_norm, xvec)

end


