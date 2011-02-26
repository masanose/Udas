;+
;
;Name:
;iug_load_crib_blr_rish_txt.pro
;
;Purpose:
;Demonstrate the RISH BLR data loader.
;
;Code:
;A. Shinbori, 02/18/2011.
;
;Modifications:
; 
;
;-


;Specify timespan:
;=================
timespan,'2007-08-01',5,/day


;Load zonal, meridional and vertical winds at Kototabang in timespan:
;We can select the parameters as 'uwnd', 'vwnd', 'wwnd', 'pwr1', 'pwr2', 'pwr3',
;  'pwr4', 'pwr5', 'wdt1', 'wdt2', 'wdt3', 'wdt4', 'wdt5':
;  uwnd = zonal wind:
;  vwnd = meridional wind
;  wwnd = vertical wind
;===============================================================================
iug_load_blr_rish_txt, site = 'ktb', parameter = ['uwnd','vwnd','wwnd']


;Plot time-height distribution of zonal wind:
;============================================
tplot,['iug_blr_ktb_uwnd','iug_blr_ktb_vwnd','iug_blr_ktb_wwnd']

stop

;Substract the average data of zonal, meridional and vertical winds:
;===================================================================
tsub_average, 'iug_blr_ktb_uwnd'
tsub_average, 'iug_blr_ktb_vwnd'
tsub_average, 'iug_blr_ktb_wwnd'
tplot,['iug_blr_ktb_uwnd-d','iug_blr_ktb_vwnd-d','iug_blr_ktb_wwnd-d']

stop

;1-hour running average of zonal, meridional and vertical winds:
;==============================================================
tsmooth_in_time, 'iug_blr_ktb_uwnd', 3600
tsmooth_in_time, 'iug_blr_ktb_vwnd', 3600
tsmooth_in_time, 'iug_blr_ktb_wwnd', 3600

tplot, ['iug_blr_ktb_uwnd_smoothed','iug_blr_ktb_vwnd_smoothed','iug_blr_ktb_wwnd_smoothed']

stop

; Set up the plot time range of zonal, meridional and vertical winds in the troposphere:
;=======================================================================================
tlimit, '2007-08-01 00:00:00', '2007-08-02 00:00:00'
tplot

end


