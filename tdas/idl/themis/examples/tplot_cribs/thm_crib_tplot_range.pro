;+
; NAME: thm_crib_tplot_range
; 
; PURPOSE:  Crib to demonstrate tplot range commands  
;           You can run this crib by typing:
;           IDL>.compile thm_crib_tplot_range
;           IDL>.go
;           
;           When you reach a stop, press
;           IDL>.c
;           to continue
;           
;           Or you can copy and paste commands directly onto the command line
;
; SEE ALSO: thm_crib_tplot.pro  (basic tplot commands)
;           thm_crib_tplot_layout.pro  (how to arrange plots within a window, and data within a plot)
;           thm_crib_tplot_annotation.pro  (how to control labels, titles, and colors of plots)
;           thm_crib_tplot_export_print.pro (how to export images of plots into pngs and postscripts)
;
; NOTES:
;   If you see any useful commands missing from these cribs, please let us know.
;   these cribs can help double as documentation for tplot.
;
;
; $LastChangedBy: jimm $
; $LastChangedDate: 2010-04-07 14:48:41 -0700 (Wed, 07 Apr 2010) $
; $LastChangedRevision: 7469 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/examples/tplot_cribs/thm_crib_tplot_range.pro $
;-



;this line deletes data so we start the crib fresh
store_data,'*',/delete

;first we set a time and load some data.
timespan,'2008-03-23'

;loading FGM line data in GSM coordinates
thm_load_fgm,probe='a',coord='gsm', level = 'l2'

;loading ESA for spectral data
thm_load_esa,probe='a'

;you can control x-range using the tlimit routine
;This will set the x-range for all plots
tplot,'tha_fgs_gsm'
tlimit,'2008-03-23/04:00:00','2008-03-23/16:00:00'

print,'Set x-range using tlimit'
print,'Type ".c" to continue'
stop

;you can reset x-range to the current timespan using the /full argument
tplot
tlimit,/full
print,'Set x-range using "tlimit,/full"'
print,'Type ".c" to continue'
stop

;you can set the yrange using the options routine

options,'tha_fgs_gsm',yrange=[-50,200] ;Set a minimum range of -50 and a maximum range of 200
tplot,'tha_fgs_gsm'

print,'Control yrange of a plot using "options"'
print,'Type ".c" to continue'
stop

;reset the yrange by setting the min equal to the max

options,'tha_fgs_gsm',yrange=[0,0]
tplot,'tha_fgs_gsm'
print,'Reset yrange of a plot using "options"'
print,'Type ".c" to continue'
stop

;Control the yrange of multiple panels

options,'tha_fgs_gsm',yrange=[0,100]
options,'tha_peif_en_eflux',yrange=[10,1e4] 

tplot,['tha_peif_en_eflux','tha_fgs_gsm']

print,'Control the yrange of multiple panels'
print,'Type ".c" to continue'
stop

;Turn logarithmic scaling on / off

options,'tha_fgs_gsm',ylog=1,yrange=[1e-2,1e4]  ;also reset range here so the plot centers better
options,'tha_peif_en_eflux',ylog=0
tplot,['tha_peif_en_eflux','tha_fgs_gsm']

print,'Turn logarithmic scaling on / off'
print,'Type ".c" to continue'
stop

;You can control the z-axis using options as well
options,'tha_peif_en_eflux',zrange=[1e2,1e7],zlog=0
tplot,'tha_peif_en_eflux'

print,'Control range/scaling of z-axis'
print,'Type ".c" to continue'
stop

print,'Crib done, resetting limits'

store_data,'tha_peif_en_eflux',limits=0
store_data,'tha_fgs_gsm',limits=0

end
