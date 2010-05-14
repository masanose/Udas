


function thm_sst_dist3d_16x1,ion=ion,elec=elec,time,probe=prb,index=index

dat = {thm_sst_dist3d_16x1}

spin_period = 3.

dim = size(/dimension,dat.data)
nenergy = dim[0]
nbins   = 1  ; dim[1]

one = replicate(1.,16)
dat.dphi   = 360  ;22.5

if keyword_set(ion) then begin
    dat.theta = 0 ;replicate(1,16) # [52*one,-52*one,25*one,-25*one]
    dat.dtheta = 90

;    phi16 = (findgen(16)+.5) * 22.5
;    sphi16 = shift(phi16,8)
    dat.phi   = 0
    dat.dphi   = 360
endif

if keyword_set(elec) then begin
    dat.theta = 0 ;replicate(1,16) # [-52*one,+52*one,-25*one,+25*one]
    dat.dtheta = 90

;    phi16 = (findgen(16)+.5) * 22.5
;    sphi16 = shift(phi16,8)
    dat.phi   = 0 ;replicate(1,16) # [sphi16,phi16,phi16,sphi16]
    dat.dphi   = 360  ;22.5
endif

dat.integ_t = dat.dphi / 360 * spin_period

; Warning: not final cals!
idap_start = [12,19,26,34,44,69,103,150,215,306,506,906,2000,3000,4000,5000]  * 1.5 * 1000
idap_width = [ 7, 7, 8,10,25,34, 47, 65, 91,200,400,3060,1000,1000,1000,1000] * 1.5 * 1000

edap_start = [12,19,26,34,44,69,103,150,215,306,506,906,2000,3000,4000,5000]  * 1.5 * 1000
edap_width = [ 7, 7, 8,10,25,34, 47, 65, 91,200,400,2000,1000,1000,1000,1000] * 1.5 * 1000

energy = (2*idap_start + idap_width)/2   + 5000.       ; midpoint energy
dat.energy = energy # replicate(1,nbins)       ; total energy width
denergy = (idap_width)
dat.denergy = denergy # replicate(1,nbins)

;weights = calc_omega_flt2(dat.theta,dat.phi,dat.dtheta,dat.dphi, 1.)
;dat.domega = weights[0,*,*]

dat.nenergy = nenergy
dat.nbins   = nbins
dat.bins = 1
dat.gf = 1

dat.units_procedure = 'thm_sst_convert_units'

dat.geom_factor = .1

return,dat
end



