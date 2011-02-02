;+
;
;Name:
;iug_load_crib_mu.pro
;
;Purpose:
;Demonstrate the RISH MU radar data loader.
;
;Code:
;A. Shinbori, 02/01/2011.
;
;Modifications:
; 
;
;-


;Specify timespan:
;=================
timespan,'1994-09-29',1

;Load all the standard observation data of the troposphere and stratosphere 
;taken by the MU radar in timespan:
;Tplot variables are 'iug_mu_uwnd', 'iug_mu_vwnd', 'iug_mu_wwnd', 'iug_mu_pwr1', 
;'iug_mu_pwr2', 'iug_mu_pwr3', 'iug_mu_pwr4', 'iug_mu_pwr5', 'iug_mu_wdt1',
;'iug_mu_wdt2', 'iug_mu_wdt3', 'iug_mu_wdt4', 'iug_mu_wdt5', 'iug_mu_dpl1',
;'iug_mu_dpl2', 'iug_mu_dpl3', 'iug_mu_dpl4', 'iug_mu_dpl5', 'iug_mu_pn1',
;'iug_mu_pn2, 'iug_mu_pn3,'iug_mu_pn4, 'iug_mu_pn5:
;  uwnd = zonal wind:
;  vwnd = meridional wind
;  wwnd = vertical wind
;  pwr = echo intensity, wdt = spectral width, dpl = radial Doppler velocity,
;  pn = noise level
;===============================================================================
iug_load_mu, datatype = 'troposphere'

;Plot time-height distribution of zonal wind, and echo intensity, spectral width,
;radial Doppler velocity, and noise level for beam number 1 in the troposphere:
;===============================================================================
tplot,['iug_mu_uwnd','iug_mu_pwr1','iug_mu_wdt1','iug_mu_dpl1', 'iug_mu_pn1']


end


