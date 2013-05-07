;+
;PROCEDURE: IUG_CRIB_RADIOSONDE_RISH.PRO
;    A sample crib sheet that explains how to use the "iug_load_radiosonde_rish.pro" 
;    procedure. You can run this crib sheet by copying & pasting each 
;    command below (except for stop and end) into the IDL command line. 
;    Or alternatively compile and run using the command:
;        .run iug_crib_ltr_rish
;
;Written by: A. Shinbori,  Apr 12, 2013
;Last Updated:  A. Shinbori,  Apr 15, 2013
;-

;Initializes system variables for themis:
;=========================================
thm_init

;Specify timespan:
;=================
timespan,'2001-10-13',5,/day


;Load preessure, temperature, dew point temperature, relative humidity, 
;zonal, and meridional winds at Darwin in timespan:
;We can select the parameters as 'press', 'temp', 'dewp','rh','uwnd', and 'vwnd':
;  press = pressure
;  temp = temperature
;  dewp = dew point temperature
;  rh = relative humidity
;  uwnd = zonal wind:
;  vwnd = meridional wind
;===============================================================================
iug_load_radiosonde_rish, site = 'drw'


;Plot time-height distribution of temperature, relative humidity, zonal and meridional winds:
;============================================================================================
tplot,['iug_radiosonde_drw_temp','iug_radiosonde_drw_dewp','iug_radiosonde_drw_rh',$
       'iug_radiosonde_drw_uwnd','iug_radiosonde_drw_vwnd']

stop

;Substract the average data of zonal and meridional winds:
;=========================================================
tsub_average, 'iug_radiosonde_drw_uwnd'
tsub_average, 'iug_radiosonde_drw_vwnd'
tplot, ['iug_radiosonde_drw_uwnd-d','iug_radiosonde_drw_vwnd-d']

stop

; Set up the plot time range of zonal, meridional and vertical winds in the troposphere:
;=======================================================================================
tlimit, '2001-10-15 00:00:00', '2001-10-16 00:00:00'
tplot

end