;+
;
;Name:
;iug_load_clib_radiosonde_rish_sgk_txt.pro
;
;Purpose:
;Demonstrate the RISH radiosonde data loader.
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
timespan,'2002-09-23',4,/day

;Load all the radiosonde observation data at Shigaraki in timespan:
;We can select the parameters as 'uwnd', 'vwnd', 'wwnd', 'pwr1', 'pwr2', 'pwr3',
;Tplot variables are 'iug_radiosonde_sgk_press', 'iug_radiosonde_sgk_temp', 
;'iug_radiosonde_sgk_rh', 'iug_radiosonde_sgk_aspeed', 'iug_radiosonde_sgk_vvelo':
;  press = pressure:
;  temp = temperature
;  rh = relative humidity
;===============================================================================
iug_load_radiosonde_rish_sgk_txt, site = 'sgk'


;Plot time-height distribution of pressure, temperature and relative humidity:
;=============================================================================
tplot,['iug_radiosonde_sgk_press', 'iug_radiosonde_sgk_temp', 'iug_radiosonde_sgk_rh']


end


