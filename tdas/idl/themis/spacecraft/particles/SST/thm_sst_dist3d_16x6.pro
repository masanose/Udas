


function thm_sst_dist3d_16x6,ion=ion,elec=elec,time,probe=prb,index=index

dat = {thm_sst_dist3d_16x6}

spin_period = 3.

dim = size(/dimension,dat.data)
nenergy = dim[0]
nbins   = dim[1]

;one = replicate(1.,16)
if keyword_set(ion) then begin
    dat.theta = replicate(1,16) # [67.5,-67.5,0,0,0,0.]
    dat.dtheta = replicate(1,16) # [45.,45,90,90,90,90.]

    dat.phi   = replicate(1,16) # [0,0,45,135.,225,315]   ; rotate by 45?
    dat.dphi  = replicate(1,16) # [360.,360.,90.,90.,90.,90]
endif

if keyword_set(elec) then begin
    dat.theta = replicate(1,16) # [67.5,-67.5,0,0,0,0.]
    dat.dtheta = replicate(1,16) # [45.,45,90,90,90,90.]

    dat.phi   = replicate(1,16) # [0,0,45,135.,225,315]
    dat.dphi  = replicate(1,16) # [360.,360.,90.,90.,90.,90]

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



