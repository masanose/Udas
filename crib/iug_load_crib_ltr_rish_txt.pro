;+
;
;Name:
;iug_load_crib_ltr_rish_txt.pro
;
;Purpose:
;Demonstrate the RISH LTR data loader.
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
timespan,'2005-12-01',31


;Load the zonal wind data taken by the LTR at Shigaraki in timespan:
;We can select the parameters as 'uwnd', 'vwnd', 'wwnd', 'pwr1', 'pwr2', 'pwr3',
;  'pwr4', 'pwr5', 'wdt1', 'wdt2', 'wdt3', 'wdt4', 'wdt5':
;  uwnd = zonal wind:
;  vwnd = meridional wind
;  wwnd = vertical wind
;===============================================================================
iug_load_ltr_rish_txt, site = 'sgk', parameter = 'uwnd'


;Plot time-height distribution of zonal wind:
;============================================
tplot,['iug_ltr_sgk_uwnd']


;Load the meridional wind data taken by the LTR at Shigaraki in timespan:
;========================================================================
iug_load_ltr_rish_txt, site = 'sgk', parameter = 'vwnd'


;Plot time-height distribution of zonal and meridional winds:
;============================================================
tplot,['iug_ltr_sgk_uwnd','iug_ltr_sgk_vwnd']


end


