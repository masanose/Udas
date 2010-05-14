;+
;Purpose: A crib showing how to create 2-D slices through Themis particle
;         distributions using THM_PART_SLICE2D.PRO.
; 
; This is a cut and past crib.
;-


;===============================================================================
; Create the slice

; Set time range of interest to create array of particle distributions
start_time='2008-02-26/04:50:00'
start_time=time_double(start_time)
;timespan, start_time, 0.05
end_time = '2008-02-26/04:55:00'
end_time = time_double(end_time)
trange=[start_time, end_time]

; Load magnetic field data
thm_load_fgm, level=2, coord='dsl', probe='b', trange=trange

; Create array of ESA ion particle distributions from which we will get particle velocities
peif_dist_arr = thm_part_dist_array(format='thb_peif', trange=trange, $
                                    mag_data='thb_fgh_dsl')

; Calculate particle velocities from SST particle distributions
thm_part_dist_vels, peif_dist_arr, 'thb_peif_vels'

; Create array of ESA particle distributions using the SST velocities
peib_dist_arr = thm_part_dist_array(format='thb_peib',  $
                                    ;or you could use the line below instead of the FORMAT keyword
                                    ;type='peib', probe='b', $
                                    trange=trange, mag_data='thb_fgh_dsl', $
                                    vel_data='thb_peif_vels')


slice_time=time_double('2008-02-26/04:52:00') ; time at which to calc slice
timewin=30.    ; set the time window to 30 seconds
center_time=1  ; make SLICE_TIME the center of TIMEWIN

rotation='BV' ; The x axis is V_para (to the magnetic field) and the bulk velocity
              ; is in the x-y plane.  See documentation in thm_part_slice2d.pro
              ; for more options.

slice_orient=[0, 0, 1, 0] ; slice plane is the x-y axis
;slice_orient=[0, 0, 1, 400] ; slice plane is the x-y axis translated 400 m/s from the origin

resolution=151 ; set the number of boxels in each dimension

thm_part_slice2d, peib_dist_arr, slice_time=slice_time, timewin=timewin, $
                  center_time=center_time, slice_orient=slice_orient, $
                  resolution=resolution, part_slice=part_slice, xgrid=xgrid, $
                  ygrid=ygrid, zgrid=zgrid, vpara=vpara, vperp1=vperp1, $
                  vperp2=vperp2, zdata=zdata, rotation=rotation



;===============================================================================
; Plot 2D Slice

; Set keywords for plotting
;range=[1.E-14,1.E-6]
olines=20
nlines=60
fill=1
plot_part_slice2d, part_slice, xgrid=xgrid, ygrid=ygrid, $
                   range=range, olines=olines, nlines=nlines, fill=fill



;===============================================================================
; Use slicer3 gui to visualize the distribution

; create Delaunay triangulations of distribution
qhull, vpara, vperp1, vperp2, tetrahedra, /DELAUNAY

; create a regular 3D grid
cubeSize = [51, 51, 51]

; interpolate distribution onto a regular grid
vol=qgrid3(vpara, vperp1, vperp2, zdata, tetrahedra, dimension=cubesize)

;mm = minmax(zdata, min_value=0)
mm = minmax(vol, min_value=0)
vol= temporary(vol)/mm[0]
vol = alog10(temporary(vol))
mm = minmax(vol, min_value=0)
vol = BYTSCL(temporary(vol), min=mm[0], max=mm[1])
hData = PTR_NEW(vol) ; slicer3 needs a pointer to the volume data 
slicer3, hData

; returns color table back to Themis color table
thm_config
