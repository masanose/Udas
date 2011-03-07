;+
;
;Name:
;iug_crib_mf_rish_pam_nc.pro
;
;Purpose:
;Demonstrate the Pameungpeuk MF radar data loader.
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
timespan,'2008-03-01',31,/day


;Load all the data of zonal, meridional and vertical wind velocities
;at Pameungpeuk for the selected parameter in timespan:
;Tplot parameters are 'iug_mf_pam_uwnd', 'iug_mf_pam_vwnd',
; 'iug_mf_pam_wwnd':
;  
;  uwnd = zonal wind:
;  vwnd = meridional wind
;  wwnd = vertical wind
;  
;===============================================================================
iug_load_mf_rish_pam_nc


;Plot time-height distribution of zonal, merdional and vertical wind:
;====================================================================
tplot,['iug_mf_pam_uwnd','iug_mf_pam_vwnd','iug_mf_pam_wwnd']

;Change in the y-range (altitude):
;=================================
ylim, 'iug_mf_pam_uwnd', 50, 100
ylim, 'iug_mf_pam_vwnd', 50, 100
ylim, 'iug_mf_pam_wwnd', 50, 100

;Change in the z-range (color bar scale):
;========================================
zlim, 'iug_mf_pam_uwnd', -100, 100
zlim, 'iug_mf_pam_vwnd', -100, 100
zlim, 'iug_mf_pam_wwnd', -10, 10

tplot

;stop


; Set up the plot time range of zonal and meridional winds in the thermosphere:
;===============================================================================
;tlimit, '2008-03-01 00:00:00', '2008-03-05 00:00:00'
;tplot

end


