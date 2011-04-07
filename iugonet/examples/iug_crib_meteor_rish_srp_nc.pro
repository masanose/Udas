;+
;
;Name:
;iug_crib_meteor_rish_srp_nc.pro
;
;Purpose:
;Demonstrate the Serpong meteor wind radar data loader.
;
;Code:
;A. Shinbori, 03/07/2011.
;
;Modifications:
; 
;
;-


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
iug_load_meteor_srp_nc, parameter = 'h2t60min00'


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

;stop


; Set up the plot time range of zonal and meridional winds in the thermosphere:
;===============================================================================
;tlimit, '2008-03-01 00:00:00', '2008-03-05 00:00:00'
;tplot

end


