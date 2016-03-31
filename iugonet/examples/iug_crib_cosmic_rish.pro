;+
;PROCEDURE: IUG_CRIB_COSMIC_RISH.PRO
;    A sample crib sheet that explains how to use the "iug_load_cosmic_rish.pro"
;    procedure. You can run this crib sheet by copying & pasting each
;    command below (except for stop and end) into the IDL command line.
;    Or alternatively compile and run using the command:
;        .run iug_crib_cosmic_rish
;
;Written by: A. Shinbori,  Mar 22, 2016
;Last Updated:  A. Shinbori,  Mar 31, 2016
;-

;Initializes system variables for themis:
;=========================================
thm_init

;Specify timespan:
;=================
timespan,'2009-01-01',3,/day


;Load the GPS RO COSMIC FSI data in timespan:
;We can specify the latitudinal range to load the data:
;======================================================
iug_load_cosmic_rish, region = 'Low_latitude'


;Plot time-height distribution of refractivity, dry air pressure, and temperature:
;=================================================================================
tplot,['gps_ro_cosmic_fsi_ref','gps_ro_cosmic_fsi_pres','gps_ro_cosmic_fsi_temp']

stop

;Substract the average data of refractivity, dry air pressure, and temperature:
;==============================================================================
tsub_average, 'gps_ro_cosmic_fsi_ref'
tsub_average, 'gps_ro_cosmic_fsi_pres'
tsub_average, 'gps_ro_cosmic_fsi_temp'
tplot, ['gps_ro_cosmic_fsi_ref-d','gps_ro_cosmic_fsi_pres-d','gps_ro_cosmic_fsi_temp-d']

stop

;1-hour running average of refractivity, dry air pressure, and temperature:
;==========================================================================
tsmooth_in_time, 'gps_ro_cosmic_fsi_ref', 3600
tsmooth_in_time, 'gps_ro_cosmic_fsi_pres', 3600
tsmooth_in_time, 'gps_ro_cosmic_fsi_temp', 3600

tplot, ['gps_ro_cosmic_fsi_ref_smoothed','gps_ro_cosmic_fsi_pres_smoothed','gps_ro_cosmic_fsi_temp_smoothed']

stop

; Set up the plot time range of refractivity, dry air pressure, and temperature in the troposphere and stratosphere:
;===================================================================================================================
tlimit, '2009-01-02 00:00:00', '2009-01-03 00:00:00'
tplot

end