;+
;
;Name:
;iug_load_crib_blr_rish_txt.pro
;
;Purpose:
;Demonstrate the RISH BLR data loader.
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
timespan,'2007-08-01',31,/day


;Load zonal wind data at Kototabang in timespan:
;We can select the parameters as 'uwnd', 'vwnd', 'wwnd', 'pwr1', 'pwr2', 'pwr3',
;  'pwr4', 'pwr5', 'wdt1', 'wdt2', 'wdt3', 'wdt4', 'wdt5':
;  uwnd = zonal wind:
;  vwnd = meridional wind
;  wwnd = vertical wind
;===============================================================================
iug_load_blr_rish_txt, site = 'ktb', parameter = 'uwnd'


;Plot time-height distribution of zonal wind:
;============================================
tplot,['iug_blr_ktb_uwnd']

stop

;Load meridional wind data at Kototabang in timespan:
;====================================================
iug_load_blr_rish_txt, site = 'ktb', parameter = 'vwnd'


;Plot time-height distribution of zonal and meridional winds:
;============================================================
tplot,['iug_blr_ktb_uwnd','iug_blr_ktb_vwnd']

stop

; Set up the plot time range of zonal and meridional winds in the troposphere:
;===============================================================================
tlimit, '2007-08-01 00:00:00', '2007-08-05 00:00:00'
tplot

end


