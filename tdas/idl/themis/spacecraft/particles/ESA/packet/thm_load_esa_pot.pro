;+
;PROCEDURE:	thm_load_esa_pot
;PURPOSE:	
;	Add spacecraft potential data to the ESA structures
;INPUT:		
;
;KEYWORDS:
;	probe:		string		themis spacecraft - "a", "b", "c", "d", "e"
;					if not set defaults to "a"
;	sc:		string		themis spacecraft - "a", "b", "c", "d", "e"
;					if not set defaults to "a"				
;	themishome:	string		path to data dir, where data dir contains the th* dir, where *=a,b,c,d,e
;	datatype	string or 0/1	if not set, uses a combination of spin averaged moment data and v1234 spin averaged data
;					if set to 1, uses vaf v12 data
;					if string, uses string data type - mom or v12, v34, v1234 where the latter are vaf data
;					vaf data is averaged by "time_average" with resolution=3s before interpolation 
;					s/c potential data are interpolated to center of esa data collection
;	pot_scale:	real		scale factor applied to measured EFI sensor potential, default=1.15
;	offset:		real		EFI sensor to plasma potential offset, default=1.0 V
;	min_pot:	real		minimum potential allowed, default=2.0
;	make_plot:	0/1		if set, makes tplot structure of potential 
;	trange:		str/dbl		2 element array of times to fix s/c pot at min_pot
;
;CREATED BY:	J. McFadden	  07-07-23
;VERSION:	1
;LAST MODIFICATION:  08-01-02
;MOD HISTORY:
;			07-12-27	corrected mom packet timing and potential
;			08-01-02	added trange keyword
;			08-04-16	default datatype changed to optimize between mom-pot and vaf-v1234-pot
;			09-06-10	expanded mom_pot_adjust for multiple changes in scale/offset algorithm
;			09-06-24	force use of mom pot during sphere shadow season
;			09-06-25	better sphere shadow season bad point removal algorithm, no need to force mom pot during sphere shadow season
;
;		TBDs
;			09-06-25	need to include 1.03 mom_pot correction factor for (v1234_avg)/(V3_snapshot) difference that changes with time
;			09-06-25	need to identify why (v1234_avg)/V3_snapshot varies in time 
;			09-06-25	scale factor for V1 to Vsc seems to require 1.04 wen in solar wind 
;			10-01-12	need to change change default scale to 1.04 and offset to 0.2 when in solar wind, sheath or plasmasphere
;			10-01-12	may need a braid mode dependent value for scale and offsets
;NOTES:	  
;	
;-

pro thm_load_esa_pot,sc=sc,probe=probe,themishome=themishome,datatype=datatype_set,pot_scale=pot_scale,offset=offset,min_pot=min_pot,make_plot=make_plot,trange=trange

; booms are not fully deployed before the following dates, set pot to zero
;	TH-C	07-05-15
;	TH-D	07-06-06
;	TH-E	07-06-06
;	TH-B	07-11-21
;	TH-A	08-01-13
;
; moment packet potential must be time shifted, onboard timing changed at mom_tim_adjust
; 1.6028 = 1 + 217/360 times spin_period is the spin offset time between s/c pot in moments packet and actual time s/c potential calculated
; after mom_tim_adjust[], 0.625 times spin_period is the spin offset time between s/c pot in moments packet and actual time s/c potential calculated
; after mom_tim_adjust[], the measured moment potential should be increased by 1.03 to account for differences in V1234 and the snapshot of V3
; changes to timing for s/c potential in moments packets occurred at
; 	THEMIS A: 07-333-20:51:26
; 	THEMIS B: 07-337-18:43:24
; 	THEMIS C: 07-337-18:23:03
; 	THEMIS D: 07-331-18:34:23
; 	THEMIS E: 07-333-17:49:10
;
; moment packet potential must be rescaled after below dates (mom_pot_adjust) to compensate for onboard scaling
; ICCR_MML28 - Adjust spacecraft potential scale & offset for pot in moments packets
;	THEMIS A 07-321-19:03:49
;	THEMIS B 07-322-03:12:17
;	THEMIS C 07-322-07:12:01
;	THEMIS D 07-322-00:02:15
;	THEMIS E 07-322-01:55:32
;

boom_deploy_time=time_double(['08-01-13','07-11-21','07-05-15','07-06-06','07-06-06'])

mom_pot_adjust=dblarr(5,3)
mom_pot_adjust[*,0]=time_double(['07-11-17/19:03:49','07-11-18/03:12:17','07-11-18/07:12:01','07-11-18/00:02:15','07-11-18/01:55:32'])
mom_pot_adjust[*,1]=time_double(['08-09-10/16:00:00','08-10-10/23:00:00','08-02-05/06:00:00','08-09-06/17:00:00','08-04-04/23:00:00'])
mom_pot_adjust[*,2]=time_double(['09-06-04/20:17:45','09-06-08/21:51:00','09-06-09/22:48:00','09-06-08/21:25:00','09-06-08/22:57:00'])

mom_tim_adjust=time_double(['07-11-29/20:51:26','07-12-03/18:43:24','07-12-03/18:23:03','07-11-27/18:34:23','07-11-29/17:49:10'])
tshft_mom=[1.6028,0.625]
probe_order=['a','b','c','d','e','f']

; sc default
	if keyword_set(probe) then sc=probe
	if not keyword_set(sc) then begin
		print,'S/C number not set, default = all probes'
		sc=['a','b','c','d','e']
	endif

	if not keyword_set(themishome) then themishome=!themis.local_data_dir

nsc = n_elements(sc)
probes=strarr(1)
if nsc eq 1 then probes[0]=sc
if nsc ne 1 then probes=sc
isc = intarr(nsc)
for i=0,nsc-1 do isc[i]=where(probes(i) eq probe_order)

;***********************************************************************************
; Set scale, offset, and min_pot if not set by keywords
; TBD - In the future these scale and offset values will have to be determined on the fly

def_scale = 1.15
def_offset = 1.0
def_min_pot = 1.0

if not keyword_set(min_pot) then min_pot=def_min_pot
if not keyword_set(pot_scale) then scale=def_scale else scale=pot_scale
if not keyword_set(offset) then offset=def_offset

;***********************************************************************************
; if keyword "trange" is set, then set sc_pot to min_pot

if keyword_set(trange) then begin
	if n_elements(trange) ne 2 then begin
		print,'trange keyword must be 2 element array'
		return
	endif
	tr2=time_double(trange)
	if tr2[0] gt tr2[1] then tr2=reverse(tr2)
	
  for i=0,nsc-1 do begin
	if probes[i] eq 'a' then begin
		common tha_454,tha_454_ind,tha_454_dat 
		if n_elements(tha_454_dat) ge 1 then begin
			ind=where(tha_454_dat.time ge tr2[0] and tha_454_dat.time le tr2[1])
			if ind[0] ne -1 then tha_454_dat.sc_pot[ind]=min_pot
			store_data,'tha_peif_sc_pot',data={x:(tha_454_dat.time+tha_454_dat.end_time)/2.,y:tha_454_dat.sc_pot}
		endif
		common tha_455,tha_455_ind,tha_455_dat 
		if n_elements(tha_455_dat) ge 1 then begin
			ind=where(tha_455_dat.time ge tr2[0] and tha_455_dat.time le tr2[1])
			if ind[0] ne -1 then tha_455_dat.sc_pot[ind]=min_pot
			store_data,'tha_peir_sc_pot',data={x:(tha_455_dat.time+tha_455_dat.end_time)/2.,y:tha_455_dat.sc_pot}
		endif
		common tha_456,tha_456_ind,tha_456_dat 
		if n_elements(tha_456_dat) ge 1 then begin
			ind=where(tha_456_dat.time ge tr2[0] and tha_456_dat.time le tr2[1])
			if ind[0] ne -1 then tha_456_dat.sc_pot[ind]=min_pot
			store_data,'tha_peib_sc_pot',data={x:(tha_456_dat.time+tha_456_dat.end_time)/2.,y:tha_456_dat.sc_pot}
		endif
		common tha_457,tha_457_ind,tha_457_dat 
		if n_elements(tha_457_dat) ge 1 then begin
			ind=where(tha_457_dat.time ge tr2[0] and tha_457_dat.time le tr2[1])
			if ind[0] ne -1 then tha_457_dat.sc_pot[ind]=min_pot
			store_data,'tha_peef_sc_pot',data={x:(tha_457_dat.time+tha_457_dat.end_time)/2.,y:tha_457_dat.sc_pot}
		endif
		common tha_458,tha_458_ind,tha_458_dat 
		if n_elements(tha_458_dat) ge 1 then begin
			ind=where(tha_458_dat.time ge tr2[0] and tha_458_dat.time le tr2[1])
			if ind[0] ne -1 then tha_458_dat.sc_pot[ind]=min_pot
			store_data,'tha_peer_sc_pot',data={x:(tha_458_dat.time+tha_458_dat.end_time)/2.,y:tha_458_dat.sc_pot}
		endif
		common tha_459,tha_459_ind,tha_459_dat 
		if n_elements(tha_459_dat) ge 1 then begin
			ind=where(tha_459_dat.time ge tr2[0] and tha_459_dat.time le tr2[1])
			if ind[0] ne -1 then tha_459_dat.sc_pot[ind]=min_pot
			store_data,'tha_peeb_sc_pot',data={x:(tha_459_dat.time+tha_459_dat.end_time)/2.,y:tha_459_dat.sc_pot}
		endif
	endif else if probes[i] eq 'b' then begin
		common thb_454,thb_454_ind,thb_454_dat 
		if n_elements(thb_454_dat) ge 1 then begin
			ind=where(thb_454_dat.time ge tr2[0] and thb_454_dat.time le tr2[1])
			if ind[0] ne -1 then thb_454_dat.sc_pot[ind]=min_pot
			store_data,'thb_peif_sc_pot',data={x:(thb_454_dat.time+thb_454_dat.end_time)/2.,y:thb_454_dat.sc_pot}
		endif
		common thb_455,thb_455_ind,thb_455_dat 
		if n_elements(thb_455_dat) ge 1 then begin
			ind=where(thb_455_dat.time ge tr2[0] and thb_455_dat.time le tr2[1])
			if ind[0] ne -1 then thb_455_dat.sc_pot[ind]=min_pot
			store_data,'thb_peir_sc_pot',data={x:(thb_455_dat.time+thb_455_dat.end_time)/2.,y:thb_455_dat.sc_pot}
		endif
		common thb_456,thb_456_ind,thb_456_dat 
		if n_elements(thb_456_dat) ge 1 then begin
			ind=where(thb_456_dat.time ge tr2[0] and thb_456_dat.time le tr2[1])
			if ind[0] ne -1 then thb_456_dat.sc_pot[ind]=min_pot
			store_data,'thb_peib_sc_pot',data={x:(thb_456_dat.time+thb_456_dat.end_time)/2.,y:thb_456_dat.sc_pot}
		endif
		common thb_457,thb_457_ind,thb_457_dat 
		if n_elements(thb_457_dat) ge 1 then begin
			ind=where(thb_457_dat.time ge tr2[0] and thb_457_dat.time le tr2[1])
			if ind[0] ne -1 then thb_457_dat.sc_pot[ind]=min_pot
			store_data,'thb_peef_sc_pot',data={x:(thb_457_dat.time+thb_457_dat.end_time)/2.,y:thb_457_dat.sc_pot}
		endif
		common thb_458,thb_458_ind,thb_458_dat 
		if n_elements(thb_458_dat) ge 1 then begin
			ind=where(thb_458_dat.time ge tr2[0] and thb_458_dat.time le tr2[1])
			if ind[0] ne -1 then thb_458_dat.sc_pot[ind]=min_pot
			store_data,'thb_peer_sc_pot',data={x:(thb_458_dat.time+thb_458_dat.end_time)/2.,y:thb_458_dat.sc_pot}
		endif
		common thb_459,thb_459_ind,thb_459_dat 
		if n_elements(thb_459_dat) ge 1 then begin
			ind=where(thb_459_dat.time ge tr2[0] and thb_459_dat.time le tr2[1])
			if ind[0] ne -1 then thb_459_dat.sc_pot[ind]=min_pot
			store_data,'thb_peeb_sc_pot',data={x:(thb_459_dat.time+thb_459_dat.end_time)/2.,y:thb_459_dat.sc_pot}
		endif
	endif else if probes[i] eq 'c' then begin
		common thc_454,thc_454_ind,thc_454_dat 
		if n_elements(thc_454_dat) ge 1 then begin
			ind=where(thc_454_dat.time ge tr2[0] and thc_454_dat.time le tr2[1])
			if ind[0] ne -1 then thc_454_dat.sc_pot[ind]=min_pot
			store_data,'thc_peif_sc_pot',data={x:(thc_454_dat.time+thc_454_dat.end_time)/2.,y:thc_454_dat.sc_pot}
		endif
		common thc_455,thc_455_ind,thc_455_dat 
		if n_elements(thc_455_dat) ge 1 then begin
			ind=where(thc_455_dat.time ge tr2[0] and thc_455_dat.time le tr2[1])
			if ind[0] ne -1 then thc_455_dat.sc_pot[ind]=min_pot
			store_data,'thc_peir_sc_pot',data={x:(thc_455_dat.time+thc_455_dat.end_time)/2.,y:thc_455_dat.sc_pot}
		endif
		common thc_456,thc_456_ind,thc_456_dat 
		if n_elements(thc_456_dat) ge 1 then begin
			ind=where(thc_456_dat.time ge tr2[0] and thc_456_dat.time le tr2[1])
			if ind[0] ne -1 then thc_456_dat.sc_pot[ind]=min_pot
			store_data,'thc_peib_sc_pot',data={x:(thc_456_dat.time+thc_456_dat.end_time)/2.,y:thc_456_dat.sc_pot}
		endif
		common thc_457,thc_457_ind,thc_457_dat 
		if n_elements(thc_457_dat) ge 1 then begin
			ind=where(thc_457_dat.time ge tr2[0] and thc_457_dat.time le tr2[1])
			if ind[0] ne -1 then thc_457_dat.sc_pot[ind]=min_pot
			store_data,'thc_peef_sc_pot',data={x:(thc_457_dat.time+thc_457_dat.end_time)/2.,y:thc_457_dat.sc_pot}
		endif
		common thc_458,thc_458_ind,thc_458_dat 
		if n_elements(thc_458_dat) ge 1 then begin
			ind=where(thc_458_dat.time ge tr2[0] and thc_458_dat.time le tr2[1])
			if ind[0] ne -1 then thc_458_dat.sc_pot[ind]=min_pot
			store_data,'thc_peer_sc_pot',data={x:(thc_458_dat.time+thc_458_dat.end_time)/2.,y:thc_458_dat.sc_pot}
		endif
		common thc_459,thc_459_ind,thc_459_dat 
		if n_elements(thc_459_dat) ge 1 then begin
			ind=where(thc_459_dat.time ge tr2[0] and thc_459_dat.time le tr2[1])
			if ind[0] ne -1 then thc_459_dat.sc_pot[ind]=min_pot
			store_data,'thc_peeb_sc_pot',data={x:(thc_459_dat.time+thc_459_dat.end_time)/2.,y:thc_459_dat.sc_pot}
		endif
	endif else if probes[i] eq 'd' then begin
		common thd_454,thd_454_ind,thd_454_dat 
		if n_elements(thd_454_dat) ge 1 then begin
			ind=where(thd_454_dat.time ge tr2[0] and thd_454_dat.time le tr2[1])
			if ind[0] ne -1 then thd_454_dat.sc_pot[ind]=min_pot
			store_data,'thd_peif_sc_pot',data={x:(thd_454_dat.time+thd_454_dat.end_time)/2.,y:thd_454_dat.sc_pot}
		endif
		common thd_455,thd_455_ind,thd_455_dat 
		if n_elements(thd_455_dat) ge 1 then begin
			ind=where(thd_455_dat.time ge tr2[0] and thd_455_dat.time le tr2[1])
			if ind[0] ne -1 then thd_455_dat.sc_pot[ind]=min_pot
			store_data,'thd_peir_sc_pot',data={x:(thd_455_dat.time+thd_455_dat.end_time)/2.,y:thd_455_dat.sc_pot}
		endif
		common thd_456,thd_456_ind,thd_456_dat 
		if n_elements(thd_456_dat) ge 1 then begin
			ind=where(thd_456_dat.time ge tr2[0] and thd_456_dat.time le tr2[1])
			if ind[0] ne -1 then thd_456_dat.sc_pot[ind]=min_pot
			store_data,'thd_peib_sc_pot',data={x:(thd_456_dat.time+thd_456_dat.end_time)/2.,y:thd_456_dat.sc_pot}
		endif
		common thd_457,thd_457_ind,thd_457_dat 
		if n_elements(thd_457_dat) ge 1 then begin
			ind=where(thd_457_dat.time ge tr2[0] and thd_457_dat.time le tr2[1])
			if ind[0] ne -1 then thd_457_dat.sc_pot[ind]=min_pot
			store_data,'thd_peef_sc_pot',data={x:(thd_457_dat.time+thd_457_dat.end_time)/2.,y:thd_457_dat.sc_pot}
		endif
		common thd_458,thd_458_ind,thd_458_dat 
		if n_elements(thd_458_dat) ge 1 then begin
			ind=where(thd_458_dat.time ge tr2[0] and thd_458_dat.time le tr2[1])
			if ind[0] ne -1 then thd_458_dat.sc_pot[ind]=min_pot
			store_data,'thd_peer_sc_pot',data={x:(thd_458_dat.time+thd_458_dat.end_time)/2.,y:thd_458_dat.sc_pot}
		endif
		common thd_459,thd_459_ind,thd_459_dat 
		if n_elements(thd_459_dat) ge 1 then begin
			ind=where(thd_459_dat.time ge tr2[0] and thd_459_dat.time le tr2[1])
			if ind[0] ne -1 then thd_459_dat.sc_pot[ind]=min_pot
			store_data,'thd_peeb_sc_pot',data={x:(thd_459_dat.time+thd_459_dat.end_time)/2.,y:thd_459_dat.sc_pot}
		endif
	endif else if probes[i] eq 'e' then begin
		common the_454,the_454_ind,the_454_dat 
		if n_elements(the_454_dat) ge 1 then begin
			ind=where(the_454_dat.time ge tr2[0] and the_454_dat.time le tr2[1])
			if ind[0] ne -1 then the_454_dat.sc_pot[ind]=min_pot
			store_data,'the_peif_sc_pot',data={x:(the_454_dat.time+the_454_dat.end_time)/2.,y:the_454_dat.sc_pot}
		endif
		common the_455,the_455_ind,the_455_dat 
		if n_elements(the_455_dat) ge 1 then begin
			ind=where(the_455_dat.time ge tr2[0] and the_455_dat.time le tr2[1])
			if ind[0] ne -1 then the_455_dat.sc_pot[ind]=min_pot
			store_data,'the_peir_sc_pot',data={x:(the_455_dat.time+the_455_dat.end_time)/2.,y:the_455_dat.sc_pot}
		endif
		common the_456,the_456_ind,the_456_dat 
		if n_elements(the_456_dat) ge 1 then begin
			ind=where(the_456_dat.time ge tr2[0] and the_456_dat.time le tr2[1])
			if ind[0] ne -1 then the_456_dat.sc_pot[ind]=min_pot
			store_data,'the_peib_sc_pot',data={x:(the_456_dat.time+the_456_dat.end_time)/2.,y:the_456_dat.sc_pot}
		endif
		common the_457,the_457_ind,the_457_dat 
		if n_elements(the_457_dat) ge 1 then begin
			ind=where(the_457_dat.time ge tr2[0] and the_457_dat.time le tr2[1])
			if ind[0] ne -1 then the_457_dat.sc_pot[ind]=min_pot
			store_data,'the_peef_sc_pot',data={x:(the_457_dat.time+the_457_dat.end_time)/2.,y:the_457_dat.sc_pot}
		endif
		common the_458,the_458_ind,the_458_dat 
		if n_elements(the_458_dat) ge 1 then begin
			ind=where(the_458_dat.time ge tr2[0] and the_458_dat.time le tr2[1])
			if ind[0] ne -1 then the_458_dat.sc_pot[ind]=min_pot
			store_data,'the_peer_sc_pot',data={x:(the_458_dat.time+the_458_dat.end_time)/2.,y:the_458_dat.sc_pot}
		endif
		common the_459,the_459_ind,the_459_dat 
		if n_elements(the_459_dat) ge 1 then begin
			ind=where(the_459_dat.time ge tr2[0] and the_459_dat.time le tr2[1])
			if ind[0] ne -1 then the_459_dat.sc_pot[ind]=min_pot
			store_data,'the_peeb_sc_pot',data={x:(the_459_dat.time+the_459_dat.end_time)/2.,y:the_459_dat.sc_pot}
		endif
	endif else if probes[i] eq 'f' then begin
		common thf_454,thf_454_ind,thf_454_dat 
		if n_elements(thf_454_dat) ge 1 then begin
			ind=where(thf_454_dat.time ge tr2[0] and thf_454_dat.time le tr2[1])
			if ind[0] ne -1 then thf_454_dat.sc_pot[ind]=min_pot
			store_data,'thf_peif_sc_pot',data={x:(thf_454_dat.time+thf_454_dat.end_time)/2.,y:thf_454_dat.sc_pot}
		endif
		common thf_455,thf_455_ind,thf_455_dat 
		if n_elements(thf_455_dat) ge 1 then begin
			ind=where(thf_455_dat.time ge tr2[0] and thf_455_dat.time le tr2[1])
			if ind[0] ne -1 then thf_455_dat.sc_pot[ind]=min_pot
			store_data,'thf_peir_sc_pot',data={x:(thf_455_dat.time+thf_455_dat.end_time)/2.,y:thf_455_dat.sc_pot}
		endif
		common thf_456,thf_456_ind,thf_456_dat 
		if n_elements(thf_456_dat) ge 1 then begin
			ind=where(thf_456_dat.time ge tr2[0] and thf_456_dat.time le tr2[1])
			if ind[0] ne -1 then thf_456_dat.sc_pot[ind]=min_pot
			store_data,'thf_peib_sc_pot',data={x:(thf_456_dat.time+thf_456_dat.end_time)/2.,y:thf_456_dat.sc_pot}
		endif
		common thf_457,thf_457_ind,thf_457_dat 
		if n_elements(thf_457_dat) ge 1 then begin
			ind=where(thf_457_dat.time ge tr2[0] and thf_457_dat.time le tr2[1])
			if ind[0] ne -1 then thf_457_dat.sc_pot[ind]=min_pot
			store_data,'thf_peef_sc_pot',data={x:(thf_457_dat.time+thf_457_dat.end_time)/2.,y:thf_457_dat.sc_pot}
		endif
		common thf_458,thf_458_ind,thf_458_dat 
		if n_elements(thf_458_dat) ge 1 then begin
			ind=where(thf_458_dat.time ge tr2[0] and thf_458_dat.time le tr2[1])
			if ind[0] ne -1 then thf_458_dat.sc_pot[ind]=min_pot
			store_data,'thf_peer_sc_pot',data={x:(thf_458_dat.time+thf_458_dat.end_time)/2.,y:thf_458_dat.sc_pot}
		endif
		common thf_459,thf_459_ind,thf_459_dat 
		if n_elements(thf_459_dat) ge 1 then begin
			ind=where(thf_459_dat.time ge tr2[0] and thf_459_dat.time le tr2[1])
			if ind[0] ne -1 then thf_459_dat.sc_pot[ind]=min_pot
			store_data,'thf_peeb_sc_pot',data={x:(thf_459_dat.time+thf_459_dat.end_time)/2.,y:thf_459_dat.sc_pot}
		endif
	endif

  endfor

;***********************************************************************************
; if keyword "trange" is not set, then set sc_pot with vaf (or mom_pot if vaf data unavailable)

endif else begin

  for i=0,nsc-1 do begin

	if probes[i] ne 'f' then begin
;	default
		scpot=[-offset,-offset] & time=timerange()
	if time[1] le boom_deploy_time[isc[i]] then begin
		if keyword_set(min_pot) then min_pot2=min_pot else min_pot2=0.
		scpot=[min_pot2,min_pot2] & time=timerange()
		store_data,'th'+probes[i]+'_esa_pot',data={x:time,y:scpot}
		print, 'Booms not deployed, loading sc_pot=min_pot'
	endif else if not keyword_set(datatype) then begin

; this section loads a combination of moment s/c potential and v1234

		thm_load_mom,probe=probes[i]
		get_data,'th'+probes[i]+'_pxxm_pot',data=tmp,index=index
		if index ne 0 then begin
			npts=n_elements(tmp.x)

; the following uncorrects onboard mom packet sc_pot after the date where corrections were implemented
; note that the onboard offset was set to 311, where 311/256=1.215 V

			if tmp.x[npts-1] lt mom_pot_adjust[isc[i],0] then begin
					; do nothing
			endif else if tmp.x[npts-1] lt mom_pot_adjust[isc[i],1] then begin 
				ind=where(tmp.x gt mom_pot_adjust[isc[i],0],count)
				if count gt 0 then tmp.y[ind]=(tmp.y[ind]-1.215)/1.15
			endif else if tmp.x[npts-1] lt mom_pot_adjust[isc[i],2] then begin 
				ind=where(tmp.x lt mom_pot_adjust[isc[i],1],count)
				if count gt 0 then tmp.y[ind]=(tmp.y[ind]-1.215)/1.15
				ind=where(tmp.x gt mom_pot_adjust[isc[i],1],count)
				if count gt 0 then tmp.y[ind]=(tmp.y[ind]-1.215)
			endif else begin
				ind=where(tmp.x lt mom_pot_adjust[isc[i],2],count)
				if count gt 0 then tmp.y[ind]=(tmp.y[ind]-1.215)
				ind=where(tmp.x gt mom_pot_adjust[isc[i],2],count)
				if count gt 0 then tmp.y[ind]=(tmp.y[ind]-1.215)/1.15
			endelse

; the following gets the spin period so that proper corrections to the mom packet sc_pot timing can be implemented 
; mom packet sc_pot measurement time, or average time, depends on the onboard alogrithm used 

		get_data,'th'+probes[i]+'_state_spinper',data=spinper
		if not keyword_set(spinper) then begin
			thm_load_state,/get_support_data,probe=probes[i]
			get_data,'th'+probes[i]+'_state_spinper',data=spinper
			if not keyword_set(spinper) then begin
				print,'No state data available for probe ',probes[i]
				print,'Using default 3 sec spin period'
				spin_period=replicate(3.,n_elements(tmp.x))
			endif else spin_period = interp(spinper.y,spinper.x,tmp.x)
		endif else spin_period = interp(spinper.y,spinper.x,tmp.x)

		if tmp.x[0] ge mom_tim_adjust[isc[i]] then time=tmp.x-tshft_mom[1]*spin_period else $
		if tmp.x[npts-1] le mom_tim_adjust[isc[i]] then time=tmp.x-tshft_mom[0]*spin_period else begin
			min_tim=min(abs(tmp.x-mom_tim_adjust[isc[i]]),ind)
			if tmp.x[ind] gt mom_tim_adjust[isc[i]] then ind=ind-1
			time=tmp.x
			time[0:ind]=time[0:ind]-tshft_mom[0]*spin_period[0:ind]
			time[ind+1:npts-1]=time[ind+1:npts-1]-tshft_mom[1]*spin_period[ind+1:npts-1]
		endelse	
		scpot=tmp.y
		endif else begin
			print,'No moment data available for probe ',probes[i]
		endelse

; if no mom data (index=0) we still need to get the average spin period

		if index eq 0 then begin
			get_data,'th'+probes[i]+'_state_spinper',data=spinper
			if not keyword_set(spinper) then begin
				thm_load_state,/get_support_data,probe=probes[i]
				get_data,'th'+probes[i]+'_state_spinper',data=spinper
				if not keyword_set(spinper) then begin
					print,'No state data available for probe ',probes[i]
					print,'Using default 3 sec spin period'
					spin_period=3.
					npts=1
				endif else begin
					spin_period = spinper.y 
					npts=n_elements(spinper.x)
				endelse
			endif else begin
				spin_period = spinper.y 
				npts=n_elements(spinper.x)
			endelse
		endif

		avg_spin_period=total(spin_period)/npts

; Calculate sc_pot based on efi vaf data if available

		thm_load_efi,probe=probes[i],datatype='vaf',level=1
		get_data,'th'+probes[i]+'_vaf',data=tmp1,index=index2

		if index2 ne 0 then begin
;			get rid of bad points
			bad_frac=1.05 
			vaf1 = -1.*reform(tmp1.y[*,0])
			vaf2 = -1.*reform(tmp1.y[*,1])
			vaf3 = -1.*reform(tmp1.y[*,2])
			vaf4 = -1.*reform(tmp1.y[*,3])
			ind1 = where(bad_frac lt 3.*vaf1/(vaf2+vaf3+vaf4),cnt1)
			ind2 = where(bad_frac lt 3.*vaf2/(vaf1+vaf3+vaf4),cnt2)
			ind3 = where(bad_frac lt 3.*vaf3/(vaf4+vaf1+vaf2),cnt3)
			ind4 = where(bad_frac lt 3.*vaf4/(vaf3+vaf1+vaf2),cnt4)
			if cnt1 gt 0 then vaf1[ind1]=vaf2[ind1]
			if cnt2 gt 0 then vaf2[ind2]=vaf1[ind2]
			if cnt3 gt 0 then vaf3[ind3]=vaf4[ind3]
			if cnt4 gt 0 then vaf4[ind4]=vaf3[ind4]
			vaf12 = (vaf1+vaf2)/2.
			vaf34 = (vaf3+vaf4)/2.
			ind5 = where(vaf12/vaf34 gt bad_frac and vaf34 gt 1.,cnt5) 
			if cnt5 gt 0 then vaf12[ind5]=vaf34[ind5]		
			ind6 = where(vaf34/vaf12 gt bad_frac and vaf12 gt 1.,cnt6) 
			if cnt6 gt 0 then vaf34[ind6]=vaf12[ind6]		
			vaf1234 = (vaf12+vaf34)/2.
			print,'Bad point counts=',cnt1,cnt2,cnt3,cnt4,cnt5,cnt6

;			vaf1234_3a=time_average(tmp1.x,vaf1234,resolution=avg_spin_period,newtime=newtime)
;			ind = where(finite(vaf1234_3a))
;			newtime=newtime(ind)
;			vaf1234_3a=vaf1234_3a(ind)
;			if keyword_set(make_plot) then store_data,'th'+sc+'_vaf1234_3a_pot',data={x:newtime,y:vaf1234_3a}


			vaf1234_3s=smooth_in_time(vaf1234,tmp1.x,avg_spin_period)
			if keyword_set(make_plot) then store_data,'th'+sc+'_vaf1234_3s_pot',data={x:tmp1.x,y:vaf1234_3s}

			if keyword_set(make_plot) then store_data,'th'+sc+'_mom_pot',data={x:time,y:scpot}

			if index ne 0 then begin
;				t3 = [time,newtime]
;				d3 = [scpot,vaf1234_3a]
				t3 = [time,tmp1.x]
				d3 = [scpot,vaf1234_3s]
				s = sort(t3)
				time=t3[s]
				scpot=d3[s]
			endif else begin
; bug, newtime no longer defined
;				time=newtime
				time = tmp1.x  		; bug fix by Jim McTiernan, failed when no mom data existed
				scpot=vaf1234_3s
			endelse
		endif

		scpot=(scale*(scpot+offset)) > min_pot
		store_data,'th'+probes[i]+'_esa_pot',data={x:time,y:scpot}

	endif else if string(datatype) eq 'mom' then begin
		thm_load_mom,probe=probes[i]
		get_data,'th'+probes[i]+'_pxxm_pot',data=tmp,index=index
		if index ne 0 then begin
			npts=n_elements(tmp.x)
			if tmp.x[npts-1] lt mom_pot_adjust[isc[i],0] then begin
					; do nothing
			endif else if tmp.x[npts-1] lt mom_pot_adjust[isc[i],1] then begin 
				ind=where(tmp.x gt mom_pot_adjust[isc[i],0],count)
				if count gt 0 then tmp.y[ind]=(tmp.y[ind]-1.215)/1.15
			endif else if tmp.x[npts-1] lt mom_pot_adjust[isc[i],2] then begin 
				ind=where(tmp.x lt mom_pot_adjust[isc[i],1],count)
				if count gt 0 then tmp.y[ind]=(tmp.y[ind]-1.215)/1.15
				ind=where(tmp.x gt mom_pot_adjust[isc[i],1],count)
				if count gt 0 then tmp.y[ind]=(tmp.y[ind]-1.215)
			endif else begin
				ind=where(tmp.x lt mom_pot_adjust[isc[i],2],count)
				if count gt 0 then tmp.y[ind]=(tmp.y[ind]-1.215)
				ind=where(tmp.x gt mom_pot_adjust[isc[i],2],count)
				if count gt 0 then tmp.y[ind]=(tmp.y[ind]-1.215)/1.15
			endelse

		get_data,'th'+probes[i]+'_state_spinper',data=spinper
		if not keyword_set(spinper) then begin
			thm_load_state,/get_support_data,probe=probes[i]
			get_data,'th'+probes[i]+'_state_spinper',data=spinper
			if not keyword_set(spinper) then begin
				print,'No state data available for probe ',probes[i]
				print,'Using default 3 sec spin period'
				spin_period=replicate(3.,n_elements(tmp.x))
			endif else spin_period = interp(spinper.y,spinper.x,tmp.x)
		endif else spin_period = interp(spinper.y,spinper.x,tmp.x)

		if tmp.x[0] ge mom_tim_adjust[isc[i]] then time=tmp.x-tshft_mom[1]*spin_period else $
		if tmp.x[npts-1] le mom_tim_adjust[isc[i]] then time=tmp.x-tshft_mom[0]*spin_period else begin
			min_tim=min(abs(tmp.x-mom_tim_adjust[isc[i]]),ind)
			if tmp.x[ind] gt mom_tim_adjust[isc[i]] then ind=ind-1
			time=tmp.x
			time[0:ind]=time[0:ind]-tshft_mom[0]*spin_period[0:ind]
			time[ind+1:npts-1]=time[ind+1:npts-1]-tshft_mom[1]*spin_period[ind+1:npts-1]
		endelse	
		scpot=tmp.y
		endif else begin
			print,'No moment data available for probe ',probes[i]
		endelse

			if keyword_set(make_plot) then store_data,'th'+sc+'_mom_pot',data={x:time,y:scpot}

		scpot=(scale*(tmp.y+offset)) > min_pot
		store_data,'th'+probes[i]+'_esa_pot',data={x:time,y:scpot}

	endif else begin

		get_data,'th'+probes[i]+'_state_spinper',data=spinper
		if not keyword_set(spinper) then begin
			thm_load_state,/get_support_data,probe=probes[i]
			get_data,'th'+probes[i]+'_state_spinper',data=spinper
			if not keyword_set(spinper) then begin
				print,'No state data available for probe ',probes[i]
				print,'Using default 3 sec spin period'
				spin_period=3.
				npts=1
			endif else begin
				spin_period = spinper.y 
				npts=n_elements(spinper.x)
			endelse
		endif else begin
			spin_period = spinper.y 
			npts=n_elements(spinper.x)
		endelse

		avg_spin_period=total(spin_period)/npts

		thm_load_efi,probe=probes[i],datatype='vaf',level=1
		get_data,'th'+probes[i]+'_vaf',data=tmp1,index=index2
		if index2 ne 0 then begin
;			get rid of bad points
			bad_frac=1.05 
			vaf1 = -1.*reform(tmp1.y[*,0])
			vaf2 = -1.*reform(tmp1.y[*,1])
			vaf3 = -1.*reform(tmp1.y[*,2])
			vaf4 = -1.*reform(tmp1.y[*,3])
			ind1 = where(bad_frac lt 3.*vaf1/(vaf2+vaf3+vaf4),cnt1)
			ind2 = where(bad_frac lt 3.*vaf2/(vaf1+vaf3+vaf4),cnt2)
			ind3 = where(bad_frac lt 3.*vaf3/(vaf4+vaf1+vaf2),cnt3)
			ind4 = where(bad_frac lt 3.*vaf4/(vaf3+vaf1+vaf2),cnt4)
			if cnt1 gt 0 then vaf1[ind1]=vaf2[ind1]
			if cnt2 gt 0 then vaf2[ind2]=vaf1[ind2]
			if cnt3 gt 0 then vaf3[ind3]=vaf4[ind3]
			if cnt4 gt 0 then vaf4[ind4]=vaf3[ind4]
			vaf12 = (vaf1+vaf2)/2.
			vaf34 = (vaf3+vaf4)/2.
			ind5 = where(vaf12/vaf34 gt bad_frac and vaf34 gt 1.,cnt5) 
			if cnt5 gt 0 then vaf12[ind5]=vaf34[ind5]		
			ind6 = where(vaf34/vaf12 gt bad_frac and vaf12 gt 1.,cnt6) 
			if cnt6 gt 0 then vaf34[ind6]=vaf12[ind6]		
			vaf1234 = (vaf12+vaf34)/2.
			print,'Bad point counts=',cnt1,cnt2,cnt3,cnt4,cnt5,cnt6

;			vaf1234_3s=time_average(tmp1.x,vaf1234,resolution=avg_spin_period,newtime=newtime)
			vaf1234_3s=smooth_in_time(vaf1234,tmp1.x,avg_spin_period)

			if keyword_set(make_plot) then store_data,'th'+sc+'_vaf1234_3s_pot',data={x:tmp1.x,y:vaf1234_3s}
;			if keyword_set(make_plot) then store_data,'th'+probes[i]+'_v1234_3s',data={x:newtime,y:vaf1234_3s}
			vaf3s = vaf1234_3s
			vaf12_3s=time_average(tmp1.x,vaf12,resolution=avg_spin_period,newtime=newtime)
			if keyword_set(make_plot) then store_data,'th'+probes[i]+'_v12_3s',data={x:newtime,y:vaf12_3s}
			if string(datatype) eq 'v12' then vaf3s = vaf12_3s
			vaf34_3s=time_average(tmp1.x,vaf34,resolution=avg_spin_period,newtime=newtime)
			if keyword_set(make_plot) then store_data,'th'+probes[i]+'_v34_3s',data={x:newtime,y:vaf34_3s}
			if string(datatype) eq 'v34' then vaf3s = vaf34_3s
			if keyword_set(make_plot) then store_data,'th'+probes[i]+'_vaf_3s',data={x:newtime,y:vaf3s}
			time=newtime
			scpot=vaf3s
		endif

		scpot=(scale*(scpot+offset)) > min_pot
		store_data,'th'+probes[i]+'_esa_pot',data={x:time,y:scpot}
		
	endelse
	endif

	if probes[i] eq 'a' then begin
		common tha_454,tha_454_ind,tha_454_dat 
		if n_elements(tha_454_dat) ne 0 then begin
		  if tha_454_ind ne -1 then begin
			tt=(tha_454_dat.time+tha_454_dat.end_time)/2. 
			tha_454_dat.sc_pot=interp(scpot,time,tt)
			store_data,'tha_peif_sc_pot',data={x:tt,y:tha_454_dat.sc_pot}
		  endif
		endif
		common tha_455,tha_455_ind,tha_455_dat 
		if n_elements(tha_455_dat) ne 0 then begin
		  if tha_455_ind ne -1 then begin
			tt=(tha_455_dat.time+tha_455_dat.end_time)/2.
			tha_455_dat.sc_pot=interp(scpot,time,tt)
			store_data,'tha_peir_sc_pot',data={x:tt,y:tha_455_dat.sc_pot}
		  endif
		endif
		common tha_456,tha_456_ind,tha_456_dat 
		if n_elements(tha_456_dat) ne 0 then begin
		  if tha_456_ind ne -1 then begin
			tt=(tha_456_dat.time+tha_456_dat.end_time)/2.
			tha_456_dat.sc_pot=interp(scpot,time,tt)
			store_data,'tha_peib_sc_pot',data={x:tt,y:tha_456_dat.sc_pot}
		  endif
		endif
		common tha_457,tha_457_ind,tha_457_dat 
		if n_elements(tha_457_dat) ne 0 then begin
		  if tha_457_ind ne -1 then begin
			tt=(tha_457_dat.time+tha_457_dat.end_time)/2.
			tha_457_dat.sc_pot=interp(scpot,time,tt)
			store_data,'tha_peef_sc_pot',data={x:tt,y:tha_457_dat.sc_pot}
		  endif
		endif
		common tha_458,tha_458_ind,tha_458_dat 
		if n_elements(tha_458_dat) ne 0 then begin
		  if tha_458_ind ne -1 then begin
			tt=(tha_458_dat.time+tha_458_dat.end_time)/2.
			tha_458_dat.sc_pot=interp(scpot,time,tt)
			store_data,'tha_peer_sc_pot',data={x:tt,y:tha_458_dat.sc_pot}
		  endif
		endif
		common tha_459,tha_459_ind,tha_459_dat 
		if n_elements(tha_459_dat) ne 0 then begin
		  if tha_459_ind ne -1 then begin
			tt=(tha_459_dat.time+tha_459_dat.end_time)/2.
			tha_459_dat.sc_pot=interp(scpot,time,tt)
			store_data,'tha_peeb_sc_pot',data={x:tt,y:tha_459_dat.sc_pot}
		  endif
		endif
	endif else if probes[i] eq 'b' then begin
		common thb_454,thb_454_ind,thb_454_dat 
		if n_elements(thb_454_dat) ne 0 then begin
		  if thb_454_ind ne -1 then begin
			tt=(thb_454_dat.time+thb_454_dat.end_time)/2.
			thb_454_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thb_peif_sc_pot',data={x:tt,y:thb_454_dat.sc_pot}
		  endif
		endif
		common thb_455,thb_455_ind,thb_455_dat 
		if n_elements(thb_455_dat) ne 0 then begin
		  if thb_455_ind ne -1 then begin
			tt=(thb_455_dat.time+thb_455_dat.end_time)/2.
			thb_455_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thb_peir_sc_pot',data={x:tt,y:thb_455_dat.sc_pot}
		  endif
		endif
		common thb_456,thb_456_ind,thb_456_dat 
		if n_elements(thb_456_dat) ne 0 then begin
		  if thb_456_ind ne -1 then begin
			tt=(thb_456_dat.time+thb_456_dat.end_time)/2.
			thb_456_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thb_peib_sc_pot',data={x:tt,y:thb_456_dat.sc_pot}
		  endif
		endif
		common thb_457,thb_457_ind,thb_457_dat 
		if n_elements(thb_457_dat) ne 0 then begin
		  if thb_457_ind ne -1 then begin
			tt=(thb_457_dat.time+thb_457_dat.end_time)/2.
			thb_457_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thb_peef_sc_pot',data={x:tt,y:thb_457_dat.sc_pot}
		  endif
		endif
		common thb_458,thb_458_ind,thb_458_dat 
		if n_elements(thb_458_dat) ne 0 then begin
		  if thb_458_ind ne -1 then begin
			tt=(thb_458_dat.time+thb_458_dat.end_time)/2.
			thb_458_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thb_peer_sc_pot',data={x:tt,y:thb_458_dat.sc_pot}
		  endif
		endif
		common thb_459,thb_459_ind,thb_459_dat 
		if n_elements(thb_459_dat) ne 0 then begin
		  if thb_459_ind ne -1 then begin
			tt=(thb_459_dat.time+thb_459_dat.end_time)/2.
			thb_459_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thb_peeb_sc_pot',data={x:tt,y:thb_459_dat.sc_pot}
		  endif
		endif
	endif else if probes[i] eq 'c' then begin
		common thc_454,thc_454_ind,thc_454_dat 
		if n_elements(thc_454_dat) ne 0 then begin
		  if thc_454_ind ne -1 then begin
			tt=(thc_454_dat.time+thc_454_dat.end_time)/2.
			thc_454_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thc_peif_sc_pot',data={x:tt,y:thc_454_dat.sc_pot}
		  endif
		endif
		common thc_455,thc_455_ind,thc_455_dat 
		if n_elements(thc_455_dat) ne 0 then begin
		  if thc_455_ind ne -1 then begin
			tt=(thc_455_dat.time+thc_455_dat.end_time)/2.
			thc_455_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thc_peir_sc_pot',data={x:tt,y:thc_455_dat.sc_pot}
		  endif
		endif
		common thc_456,thc_456_ind,thc_456_dat 
		if n_elements(thc_456_dat) ne 0 then begin
		  if thc_456_ind ne -1 then begin
			tt=(thc_456_dat.time+thc_456_dat.end_time)/2.
			thc_456_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thc_peib_sc_pot',data={x:tt,y:thc_456_dat.sc_pot}
		  endif
		endif
		common thc_457,thc_457_ind,thc_457_dat 
		if n_elements(thc_457_dat) ne 0 then begin
		  if thc_457_ind ne -1 then begin
			tt=(thc_457_dat.time+thc_457_dat.end_time)/2.
			thc_457_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thc_peef_sc_pot',data={x:tt,y:thc_457_dat.sc_pot}
		  endif
		endif
		common thc_458,thc_458_ind,thc_458_dat 
		if n_elements(thc_458_dat) ne 0 then begin
		  if thc_458_ind ne -1 then begin
			tt=(thc_458_dat.time+thc_458_dat.end_time)/2.
			thc_458_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thc_peer_sc_pot',data={x:tt,y:thc_458_dat.sc_pot}
		  endif
		endif
		common thc_459,thc_459_ind,thc_459_dat 
		if n_elements(thc_459_dat) ne 0 then begin
		  if thc_459_ind ne -1 then begin
			tt=(thc_459_dat.time+thc_459_dat.end_time)/2.
			thc_459_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thc_peeb_sc_pot',data={x:tt,y:thc_459_dat.sc_pot}
		  endif
		endif
	endif else if probes[i] eq 'd' then begin
		common thd_454,thd_454_ind,thd_454_dat 
		if n_elements(thd_454_dat) ne 0 then begin
		  if thd_454_ind ne -1 then begin
			tt=(thd_454_dat.time+thd_454_dat.end_time)/2.
			thd_454_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thd_peif_sc_pot',data={x:tt,y:thd_454_dat.sc_pot}
		  endif
		endif
		common thd_455,thd_455_ind,thd_455_dat 
		if n_elements(thd_455_dat) ne 0 then begin
		  if thd_455_ind ne -1 then begin
			tt=(thd_455_dat.time+thd_455_dat.end_time)/2.
			thd_455_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thd_peir_sc_pot',data={x:tt,y:thd_455_dat.sc_pot}
		  endif
		endif
		common thd_456,thd_456_ind,thd_456_dat 
		if n_elements(thd_456_dat) ne 0 then begin
		  if thd_456_ind ne -1 then begin
			tt=(thd_456_dat.time+thd_456_dat.end_time)/2.
			thd_456_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thd_peib_sc_pot',data={x:tt,y:thd_456_dat.sc_pot}
		  endif
		endif
		common thd_457,thd_457_ind,thd_457_dat 
		if n_elements(thd_457_dat) ne 0 then begin
		  if thd_457_ind ne -1 then begin
			tt=(thd_457_dat.time+thd_457_dat.end_time)/2.
			thd_457_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thd_peef_sc_pot',data={x:tt,y:thd_457_dat.sc_pot}
		  endif
		endif
		common thd_458,thd_458_ind,thd_458_dat 
		if n_elements(thd_458_dat) ne 0 then begin
		  if thd_458_ind ne -1 then begin
			tt=(thd_458_dat.time+thd_458_dat.end_time)/2.
			thd_458_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thd_peer_sc_pot',data={x:tt,y:thd_458_dat.sc_pot}
		  endif
		endif
		common thd_459,thd_459_ind,thd_459_dat 
		if n_elements(thd_459_dat) ne 0 then begin
		  if thd_459_ind ne -1 then begin
			tt=(thd_459_dat.time+thd_459_dat.end_time)/2.
			thd_459_dat.sc_pot=interp(scpot,time,tt)
			store_data,'thd_peeb_sc_pot',data={x:tt,y:thd_459_dat.sc_pot}
		  endif
		endif
	endif else if probes[i] eq 'e' then begin
		common the_454,the_454_ind,the_454_dat 
		if n_elements(the_454_dat) ne 0 then begin
		  if the_454_ind ne -1 then begin
			tt=(the_454_dat.time+the_454_dat.end_time)/2.
			the_454_dat.sc_pot=interp(scpot,time,tt)
			store_data,'the_peif_sc_pot',data={x:tt,y:the_454_dat.sc_pot}
		  endif
		endif
		common the_455,the_455_ind,the_455_dat 
		if n_elements(the_455_dat) ne 0 then begin
		  if the_455_ind ne -1 then begin
			tt=(the_455_dat.time+the_455_dat.end_time)/2.
			the_455_dat.sc_pot=interp(scpot,time,tt)
			store_data,'the_peir_sc_pot',data={x:tt,y:the_455_dat.sc_pot}
		  endif
		endif
		common the_456,the_456_ind,the_456_dat 
		if n_elements(the_456_dat) ne 0 then begin
		  if the_456_ind ne -1 then begin
			tt=(the_456_dat.time+the_456_dat.end_time)/2.
			the_456_dat.sc_pot=interp(scpot,time,tt)
			store_data,'the_peib_sc_pot',data={x:tt,y:the_456_dat.sc_pot}
		  endif
		endif
		common the_457,the_457_ind,the_457_dat 
		if n_elements(the_457_dat) ne 0 then begin
		  if the_457_ind ne -1 then begin
			tt=(the_457_dat.time+the_457_dat.end_time)/2.
			the_457_dat.sc_pot=interp(scpot,time,tt)
			store_data,'the_peef_sc_pot',data={x:tt,y:the_457_dat.sc_pot}
		  endif
		endif
		common the_458,the_458_ind,the_458_dat 
		if n_elements(the_458_dat) ne 0 then begin
		  if the_458_ind ne -1 then begin
			tt=(the_458_dat.time+the_458_dat.end_time)/2.
			the_458_dat.sc_pot=interp(scpot,time,tt)
			store_data,'the_peer_sc_pot',data={x:tt,y:the_458_dat.sc_pot}
		  endif
		endif
		common the_459,the_459_ind,the_459_dat 
		if n_elements(the_459_dat) ne 0 then begin
		  if the_459_ind ne -1 then begin
			tt=(the_459_dat.time+the_459_dat.end_time)/2.
			the_459_dat.sc_pot=interp(scpot,time,tt)
			store_data,'the_peeb_sc_pot',data={x:tt,y:the_459_dat.sc_pot}
		  endif
		endif
	endif

  endfor
endelse

end