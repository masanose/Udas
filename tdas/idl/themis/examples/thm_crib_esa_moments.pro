;***********************************************************************************
; Calculates ion and electron moments from full distribution
; this is a cut and paste crib, not to be run as program

; Cavets
; works best with data type - full, 
; moments don't work well with data type - reduced 
; data prior to 2007-03-30 may have errors in ETC table maps that produce errors in moments and distributions

; Note on variable/routine names: Particle variables/routines follow the naming convention th[a/b/c/d/e]_p[e/s][i/e][f/r/b]
;  where th=themis, [a/b/c/d/e]=spacecraft, p=particle, [e/s]=ESA or SST instrument, [i/e]=ion/electron, [f/r/b]=full/reduced/burst distribution


;-----------------------------------------------------------------------------------
; This crib is divided into three sections:
;   the first section demonstrates a wrapper routine (thm_esa_specmom_calc.pro) that gets L0 quantities
;     from L0/packet files and calculates L2 quantities
;   the second section demonstrates how to get the quantities without the wrapper routine
;   the third section demonstrates how to get l2 quantities from L2 files


;-----------------------------------------------------------------------------------
;#1:  Load ESA spectragram and moment data (N, V, T) wrapper
;routine(easy way)

;load 2 day of spacecraft B full ion distribution quantities
thm_esa_specmom_calc,date='2007-06-17',dur=2,probe='b',mtypes='spectrogram velocity density temperature', $
                     distribution_type='full',species='ion'

tplot_names    ; show existing tplot quantities
tplot,['thb_peif_en_eflux','thb_peif_density','thb_peif_velocity_dsl','thb_peif_T_dsl']


;load 2 day of spacecraft C burst electron distribution quantities
thm_esa_specmom_calc,date='2007-06-17',dur=2,probe='c',mtypes='spectrogram velocity density temperature', $
                     distribution_type='burst',species='electron'

tplot_names    ; show existing tplot quantities
tplot,['thc_peeb_en_eflux','thc_peeb_density','thc_peeb_velocity_dsl','thc_peeb_T_dsl']


;load 2 day of spacecraft A full ion distribution quantities (illustrating that some keywords can be abbr.)
thm_esa_specmom_calc,date='2007-06-17',dur=2,probe='a',mtypes='spec vel den temp',dist='f',sp='i'

tplot_names    ; show existing tplot quantities
tplot,['tha_peif_en_eflux','tha_peif_density','tha_peif_velocity_dsl','tha_peif_T_dsl']


;-----------------------------------------------------------------------------------
;#2:  Load ESA spectragram and moment data (N, V, T) (hard way)

; select day for event

	startdate = '2007-06-17/0:00'
	t1=time_double(startdate)
	ndays=2
	t2=t1+ndays*24.*3600.
	timespan,startdate,ndays

; select a probe
	sc='a'
	scc=sc				; this can be eliminated on the next version and just use sc
	thm_load_state,probe=scc
	thm_load_esa_pkt,probe=scc

; choose type of data - f-full, r-reduced, b-burst
;gap time for full distribution is greater because data rates are higher per sample
;So we have less frequent, larger samples
	typ='f'
	if typ eq 'f' then gap_time=1000. else gap_time=10.

; decide if you want magnetosheath (sheath=1) or plasmasheet (sheath=0) limits, default is sheath=1
;	sheath=0
;	if sheath then nmax=100. else nmax=10.
	sheath=1
	nmax=100.
	emin=15

; Get position info


	get_data,strjoin('th'+sc+'_state_pos_gse'),data=tmp
	store_data,'th'+sc+'_state_pos_gse_x',data={x:tmp.x,y:tmp.y(*,0)/6370.}
		options,'th'+sc+'_state_gse_pos_x','ytitle','th'+sc+'_X-GSE'
	store_data,'th'+sc+'_state_pos_gse_y',data={x:tmp.x,y:tmp.y(*,1)/6370.}
		options,'th'+sc+'_state_pos_gse_y','ytitle','th'+sc+'_Y-GSE'
	store_data,'th'+sc+'_state_pos_gse_z',data={x:tmp.x,y:tmp.y(*,2)/6370.}
		options,'th'+sc+'_state_pos_gse_z','ytitle','th'+sc+'_Z-GSE'

; ion plots


	get_dat='th'+sc+'_pei'+typ
	name1='th'+sc+'_pei'+typ+'_en_eflux'
	;call get_en_spec to make sure units are properly set before moment calculations
	get_en_spec,get_dat,units='eflux',retrace=1,name=name1,gap_time=gap_time,t1=t1,t2=t2
	
	;make spectrogram plot look pretty
	zlim,name1,1.e3,1.e7,1
	ylim,name1,3.,40000.,1
	options,name1,'ztitle','Eflux !C!C eV/cm!U2!N-s-sr-eV'
	options,name1,'ytitle','i+ th'+sc+'!C!C eV'
	options,name1,'spec',1
	options,name1,'x_no_interp',1
	options,name1,'y_no_interp',1




	name1='th'+sc+'_pei'+typ+'_density'
	get_2dt,'n_3d_new',get_dat,name=name1,gap_time=gap_time,t1=t1,t2=t2,energy=[20.,21000.]
	;set metadata
	ylim,name1,.1,nmax,1
	options,name1,'ytitle','Ni th'+sc+'!C!C1/cm!U3'
	
	
	name1='th'+sc+'_pei'+typ+'_velocity_dsl'
	;v_3d_new is proc that calculates velocity moments
	get_2dt,'v_3d_new',get_dat,name=name1,gap_time=gap_time,t1=t1,t2=t2,energy=[20.,21000.]
	;set metadata
	get_data,name1,data=d,dlimits=a
	cotrans_set_coord,a,'dsl'	; add coord system label for cotrans routines
	store_data,name1,data=d,dlimits=a
	ylim,name1,-500,500.,0
	ylim,name1,-200,200.,0
	if sheath then ylim,name1,-500,200.,0
	options,name1,'ytitle','Vi th'+sc+'!C!Ckm/s'
	;options,name1,'colors',[cols.blue,cols.green,cols.red]
	options,name1,labels=['Vi!dx!n', 'Vi!dy!n', 'Vi!dz!n'],constant=0.

	name1='th'+sc+'_pei'+typ+'_T_dsl'
	;t_3d_new calculates temperature moments
	get_2dt,'t_3d_new',get_dat,name=name1,gap_time=gap_time,t1=t1,t2=t2,energy=[20.,21000.]
	;set metadata
	get_data,name1,data=d,dlimits=a
	cotrans_set_coord,a,'dsl'	; add coord system label for cotrans routines
	store_data,name1,data=d,dlimits=a
	ylim,name1,100,10000.,1
	if sheath then ylim,name1,10,10000.,1
	options,name1,'ytitle','Ti th'+sc+'!C!CeV'

; plot the ion data

	tplot,['th'+sc+'_pei'+typ+'_en_eflux','th'+sc+'_pei'+typ+'_density','th'+sc+'_pei'+typ+'_velocity_dsl','th'+sc+'_pei'+typ+'_T_dsl'],$
	title='Themis',var_label=['th'+sc+'_state_pos_x','th'+sc+'_state_pos_y','th'+sc+'_state_pos_z']

; electron plots

	get_dat='th'+sc+'_pee'+typ
	name1='th'+sc+'_pee'+typ+'_en_eflux'
	;get_en_spec sets proper units so it should be called prior to moments calculations
	get_en_spec,get_dat,units='eflux',retrace=1,name=name1,gap_time=gap_time,t1=t1,t2=t2
	;set metadata
	zlim,name1,1.e5,1.e9,1
	ylim,name1,3.,40000.,1
	options,name1,'ztitle','Eflux !C!C eV/cm!U2!N-s-sr-eV'
	options,name1,'ytitle','e- th'+sc+'!C!C eV'
	options,name1,'spec',1
	options,name1,'x_no_interp',1
	options,name1,'y_no_interp',1

	name1='th'+sc+'_pee'+typ+'_density'
	get_2dt,'n_3d_new',get_dat,name=name1,gap_time=gap_time,t1=t1,t2=t2,energy=[emin,27000.]
	;set metadata
	ylim,name1,.1,nmax,1
	options,name1,'ytitle','Ne th'+sc+'!C!C1/cm!U3'
	
	name1='th'+sc+'_pee'+typ+'_velocity_dsl'
	get_2dt,'v_3d_new',get_dat,name=name1,gap_time=gap_time,t1=t1,t2=t2,energy=[emin,27000.]
	;set metadata
	get_data,name1,data=d,dlimits=a
	cotrans_set_coord,a,'dsl'	; add coord system label for cotrans routines
	store_data,name1,data=d,dlimits=a
	ylim,name1,-500,500.,0
	ylim,name1,-200,200.,0
	if sheath then ylim,name1,-500,200.,0
	options,name1,'ytitle','Ve th'+sc+'!C!Ckm/s'
	;options,name1,'colors',[cols.blue,cols.green,cols.red]
	options,name1,labels=['V!dex!n', 'V!dey!n', 'V!dez!n'],constant=0.

	name1='th'+sc+'_pee'+typ+'_T_dsl'
	get_2dt,'t_3d_new',get_dat,name=name1,gap_time=gap_time,t1=t1,t2=t2,energy=[emin,27000.]
	;set metadata
	get_data,name1,data=d,dlimits=a
	cotrans_set_coord,a,'dsl'	; add coord system label for cotrans routines
	store_data,name1,data=d,dlimits=a
	ylim,name1,100,10000.,1
	if sheath then ylim,name1,10,10000.,1
	options,name1,'ytitle','Te th'+sc+'!C!CeV'


; plot the electron data

	tplot,['th'+sc+'_pee'+typ+'_en_eflux','th'+sc+'_pee'+typ+'_density','th'+sc+'_pee'+typ+'_velocity_dsl','th'+sc+'_pee'+typ+'_T_dsl'],$
	title='Themis',var_label=['th'+sc+'_state_pos_gse_x','th'+sc+'_state_pos_gse_y','th'+sc+'_state_pos_gse_z']



;-----------------------------------------------------------------------------------
;#3:  Load ESA spectragram and moment data (N, V, T) from L2 files

timespan,'2007-07-01'

;load all L2 data for spacecraft B
thm_load_esa,probe='b'

;load all coordinate independent and GSE coordinate data for spacecrafts C and D
thm_load_esa,probe=['c','d'],coord='gse'

;load all variables that look like '*density*' for spacecraft A
thm_load_esa,probe='a',varformat='*density*'

;load all coordinate independent and GSE and DSL coordinate data for spacecrafts A, B and D
thm_load_esa,probe='a b d',coord='gse dsl'


end