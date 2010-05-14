;+
;pro thm_crib_mom
; This is an example crib sheet that will load onboard MOMent data.
; It also shows how to compute moments from the full distributions.
; Data is corrected for spacecraft potential.
;
; Open this file in a text editor and then use copy and paste to copy
; selected lines into an idl window. Or alternatively compile and run
; using the command:
; .RUN THM_CRIB_MOM
;-

if not keyword_set(sc) then sc = 'c'

;------------ On Board moments: ----------------

; load onboard moments
thm_load_mom,probe=sc

; load magnetic field data:
thm_load_fit,probe=sc


; ------------- Ground processed moments: ----------------


; load ESA distribution data:
thm_load_esa_pkt,probe=sc

; load Spacecraft Potential, this results in a tplot variable with the
; spacecraft potential for all ESA modes. Note that setting the
; datatype keyword equal to anything that is not 'mom' will result in
; the direct use of EFI data for the potential. If datatype is not
; set, or is set to 'mom', then th?_pxxm_pot is used  
thm_load_esa_pot, sc = sc

; calculate esa electron and ion parameters  (moments and spectra) :
thm_part_moments, probe = sc, instrum = 'pe?f', scpot_suffix = '_esa_pot', mag_suffix = '_fgs', tplotnames = tn, verbose = 2 ; names are output into variable tn

; load SST data
thm_load_sst,probe=sc

; calculate SST parameters:
thm_part_moments,probe=sc,instrum='ps?f',mag_suffix='_fgs' ,tplotnames=tn, verbose=2    ; names are output into variable tn
thm_part_moments,probe=sc,instrum='ps?r',mag_suffix='_fgs' ,tplotnames=tn, verbose=2    ; names are output into variable tn

; get eflux spectra of reduced distributions  (but not moments)
thm_part_moments, probe = sc, instrum = 'pe?r', moments = ''

; Create overview variables

store_data,'Th'+sc+'_pXiX_en_eflux',data='th'+sc+['_peif_en_eflux','_peir_en_eflux','_psif_en_eflux','_psir_en_eflux'], $
    dlimit={yrange:[1,1e6],ylog:1,panel_size:1.5,ztitle:'Eflux [eV/cm2/s/ster/eV]',zrange:[1e3,1e9],zlog:1}

store_data,'Th'+sc+'_pXeX_en_eflux',data='th'+sc+['_peef_en_eflux','_peer_en_eflux','_psef_en_eflux', '_pxxm_pot'], $
    dlimit={yrange:[1,1e6],ylog:1,panel_size:1.5,ztitle:'Eflux [eV/cm2/s/ster/eV]',zrange:[1e3,1e9],zlog:1}

options,'th?_p?if_density',colors='b'
options,'th?_p?ef_density',colors='r'
options,'th?_p?im_density',colors='c'
options,'th?_p?em_density',colors='m'

store_data,'Th'+sc+'_peXf_density',data='th'+sc+['_peef_density','_peif_density']   ;, '_pxxm_pot'
store_data,'Th'+sc+'_peXm_density',data='th'+sc+['_peem_density','_peim_density']   ;, '_pxxm_pot'

store_data,'Th'+sc+'_peiX_density',data='th'+sc+['_peim_density','_peif_density']   ;, '_pxxm_pot'
store_data,'Th'+sc+'_peeX_density',data='th'+sc+['_peem_density','_peef_density']   ;, '_pxxm_pot'

ylim,'*density',.1,400,1


tplot,'T* '


end

