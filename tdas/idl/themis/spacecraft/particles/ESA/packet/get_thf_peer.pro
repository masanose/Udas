;+
;PROCEDURE:	get_thf_peer
;PURPOSE:	
;	Returns peer data structure at a single time from common generated by thm_load_esa_pkt.pro
;INPUT:		
;	time:		dbl		time of data to be returned
;
;KEYWORDS:
;	start:		0,1		if set, gets first time in common block
;	en:		0,1		if set, gets last time in common block
;	advance		0,1		if set, gets next time in common block
;	retreat		0,1		if set, gets previous time in common block
;	index		long		gets data at the index value "ind" in common block
;	calib:		0,1		not working yet, allows alternate calibration
;	times		0,1		returns an array of times for all the data, returns 0 if no data
;
;
;CREATED BY:	J. McFadden
;VERSION:	1
;LAST MODIFICATION:  07/03/22
;MOD HISTORY:
;
;NOTES:	  
;	Data structures can be used as inputs to functions such as n_3d.pro, v_3d.pro
;	Or used in conjunction with iterative programs such as get_3dt.pro, get_en_spec.pro
;-
FUNCTION get_thf_peer,time,START=st,EN=en,ADVANCE=adv,RETREAT=ret,index=ind,calib=calib,times=times

common thf_458,get_ind,all_dat

if n_elements(get_ind) eq 0 then begin
	dat = 	{project_name:'THEMIS',valid:0}
	print,' ERROR - thf peer data not loaded'
endif else if get_ind eq -1 then begin
	dat = 	{project_name:'THEMIS',valid:0}
	print,' ERROR - thf peer data not loaded'
endif else if keyword_set(times) then begin
	dat=(all_dat.time+all_dat.end_time)/2.
endif else begin

decompress=1.*[indgen(16),16+2*indgen(8),32+4*indgen(24),128+8*indgen(16),256+16*indgen(16),$
	512+32*indgen(16),1024+64*indgen(16),2048+128*indgen(48),8192+256*indgen(32),$
	16384+512*indgen(32),32768+1024*indgen(32)]

if (n_elements(time) eq 0) and (not keyword_set(st)) and (not keyword_set(en)) $
        and (not keyword_set(adv)) and (not keyword_set(ret)) and (n_elements(ind) eq 0) $
	then ctime,time,npoints=1

if keyword_set(st) then ind=0l 						$
	else if keyword_set(en) then ind=n_elements(all_dat.time)-1 	$
	else if keyword_set(adv) then ind=get_ind+1 			$
	else if keyword_set(ret) then ind=get_ind-1 			$
	else if n_elements(ind) ne 0 then ind=ind 			$
	else tmpmin = min(abs(all_dat.time-time),ind)

if ind lt 0 or ind ge n_elements(all_dat.time) then begin

dat = 		{project_name:		all_dat.project_name,		$
		spacecraft:		all_dat.spacecraft, 		$
		data_name:		all_dat.data_name, 		$
		apid:			all_dat.apid,			$
		valid: 			0}

endif else begin

	while (all_dat.valid[ind] eq 0 and ind+1 lt n_elements(all_dat.time)) do ind=ind+1

	config1=all_dat.config1[ind]
	config2=all_dat.config2[ind]
	an_ind=all_dat.an_ind[ind]
	en_ind=all_dat.en_ind[ind]
	mode=all_dat.md_ind[ind]
	cs_ptr=all_dat.cs_ptr[ind]

	nenergy=all_dat.nenergy[en_ind]
	nbins=all_dat.nbins[an_ind]
	bins=intarr(nenergy,nbins) & bins(1:nenergy-1,*)=1

	energy =reform( all_dat.energy[0:nenergy-1,en_ind])#replicate(1.,nbins)
	denergy=reform(all_dat.denergy[0:nenergy-1,en_ind])#replicate(1.,nbins)
	an_eff =total(all_dat.an_eff#replicate(1.,nbins)*all_dat.an_map[*,0:nbins-1,an_ind],1)
	eff    =reform( all_dat.en_eff[0:nenergy-1,en_ind])#an_eff*all_dat.rel_gf[ind]

	theta =reform(all_dat.theta[0:nenergy-1,0:nbins-1,an_ind])
	dtheta=reform(all_dat.dtheta[0:nenergy-1,0:nbins-1,an_ind])
	phi   =reform(all_dat.phi[0:nenergy-1,0:nbins-1,an_ind])+all_dat.phi_offset(ind)
	dphi  =reform(all_dat.dphi[0:nenergy-1,0:nbins-1,an_ind])
	domega=reform(all_dat.domega[0:nenergy-1,0:nbins-1,an_ind])
	gf    =reform(all_dat.gf[0:nenergy-1,0:nbins-1,an_ind])
	dt_arr=reform(all_dat.dt_arr[0:nenergy-1,0:nbins-1,an_ind])
	bkg   =reform(all_dat.bkg_arr[0:nenergy-1,0:nbins-1,an_ind])*all_dat.bkg[ind]

;	print,nbins,nenergy,an_ind,en_ind,mode,config1,config2

	case all_dat.cs_ind[ind] of
		0: dat=transpose(reform(decompress(all_dat.dat0[cs_ptr,*]),nbins,nenergy))
		1: dat=transpose(reform(decompress(all_dat.dat1[cs_ptr,*]),nbins,nenergy))
		2: dat=transpose(reform(decompress(all_dat.dat2[cs_ptr,*]),nbins,nenergy))
		3: dat=transpose(reform(decompress(all_dat.dat3[cs_ptr,*]),nbins,nenergy))


	endcase







	valid=all_dat.valid[ind]
;	if dimen1(counts) ne nenergy or dimen2(counts) ne nbins then begin
;		nd1=dimen1(counts) & nd2=dimen2(counts)
;		if nd1*nd2 eq nbins*nenergy then counts=reform(counts,nenergy,nbins)
;		if nd1*nd2 gt nbins*nenergy then counts=reform(counts(0:nbins*nenergy),nenergy,nbins)
;		if nd1*nd2 lt nbins*nenergy then valid=0
;		if nd1*nd2 lt nbins*nenergy then print,'ERROR: nbins, nenergy inconsistent with counts array'
;	endif

dat = 		{project_name:		all_dat.project_name,		$
		spacecraft:		all_dat.spacecraft, 		$
		data_name:		all_dat.data_name, 		$
		apid:			all_dat.apid,			$
		units_name: 		'counts', 			$
		units_procedure: 	all_dat.units_procedure, 	$
		valid: 			valid, 				$

		time: 			all_dat.time[ind], 		$
		end_time: 		all_dat.end_time[ind], 		$
		delta_t: 		all_dat.delta_t[ind],		$
		integ_t: 		all_dat.integ_t[ind],		$
		dt_arr: 		dt_arr,				$

		config1:		config1,			$
		config2:		config2,			$
		an_ind:			an_ind,				$
		en_ind:			en_ind,				$
		mode:			mode,				$

		nenergy: 		nenergy, 			$
		energy: 		energy, 			$
		denergy: 		denergy,     		 	$
		eff: 			eff,	 			$
		bins: 			bins, 				$

		nbins: 			nbins,	 			$
		theta: 			theta,  			$
		dtheta: 		dtheta,  			$
		phi: 			phi,  				$
		dphi: 			dphi,	  			$
		domega: 		domega,  			$
		gf: 			gf, 				$

		geom_factor: 		all_dat.geom_factor, 		$
		dead: 			all_dat.dead,			$
		mass: 			all_dat.mass, 			$
		charge: 		all_dat.charge, 		$
		sc_pot: 		all_dat.sc_pot[ind], 		$

		magf:	 		reform(all_dat.magf[ind,*]), 	$

		bkg:	 		bkg,		 		$

		data: 			dat}
get_ind=ind

endelse
endelse

return,dat

end
