;+
;PROCEDURE: IUG_CRIB_METEOR_RISH_SRP_NC.PRO
;    A sample crib sheet that explains how to use the "iug_crib_meteor_rish_srp_nc.pro" 
;    procedure. You can run this crib sheet by copying & pasting each 
;    command below (except for stop and end) into the IDL command line. 
;    Or alternatively compile and run using the command:
;        .run iug_crib_meteor_rish_srp_nc
;
;Written by: A. Shinbori,  Feb 18, 2011
;Last Updated:  A. Shinbori,  DEC 03, 2011
;-

;Initializes system variables for themis:
;=========================================
thm_init

;Specify timespan:
;=================
timespan,'1993-01-01',90,/day


;Load all the data of zonal and meridional wind velocities
;and their standard deviations and the number of meteor traces
;at Serpong for the selected parameter in timespan:
;Tplot parameters are 'iug_meteor_srp_uwnd_h2t60min00', 'iug_meteor_srp_vwnd_h2t60min00',
; 'iug_meteor_srp_uwndsig_h2t60min00', 'iug_meteor_srp_vwndsig_h2t60min00',
;  'iug_meteor_srp_mwnum_h2t60min00':
;  
;  uwnd = zonal wind:
;  vwnd = meridional wind
;  
;===============================================================================
iug_load_meteor_srp_nc, parameter = 'h2t60min00',length='1_month'


;Plot time-height distribution of zonal and merdional wind:
;==========================================================
tplot,['iug_meteor_srp_uwnd_h2t60min00','iug_meteor_srp_vwnd_h2t60min00']

;Change in the y-range (altitude):
;=================================
ylim, 'iug_meteor_srp_uwnd_h2t60min00', 70, 110
ylim, 'iug_meteor_srp_vwnd_h2t60min00', 70, 110

;Change in the z-range (color bar scale):
;========================================
zlim, 'iug_meteor_srp_uwnd_h2t60min00', -100, 100
zlim, 'iug_meteor_srp_vwnd_h2t60min00', -100, 100

tplot

stop


; Set up the plot time range of zonal and meridional winds in the thermosphere:
;===============================================================================
tlimit, '1993-01-01 00:00:00', '1993-01-05 00:00:00'
tplot

end


