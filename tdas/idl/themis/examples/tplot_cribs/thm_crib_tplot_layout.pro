;+
; NAME: thm_crib_tplot_layout
; 
; PURPOSE:  Crib to demonstrate tplot layout commands  
;           You can run this crib by typing:
;           IDL>.compile thm_crib_tplot_layout
;           IDL>.go
;           
;           When you reach a stop, press
;           IDL>.c
;           to continue
;           
;           Or you can copy and paste commands directly onto the command line
;
; SEE ALSO: thm_crib_tplot.pro  (basic tplot commands)
;           thm_crib_tplot_range.pro   (how to control the range and scaling of plots)
;           thm_crib_tplot_annotation.pro  (how to control labels, titles, and colors of plots)
;           thm_crib_tplot_export_print.pro (how to export images of plots into pngs and postscripts)
;           thm_crib_tplot_overlay.pro(how to overlay spectral plots)
;
; NOTES:
;   If you see any useful commands missing from these cribs, please let us know.
;   these cribs can help double as documentation for tplot.
;
; $LastChangedBy: jimm $
; $LastChangedDate: 2010-04-07 14:48:41 -0700 (Wed, 07 Apr 2010) $
; $LastChangedRevision: 7469 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/examples/tplot_cribs/thm_crib_tplot_layout.pro $
;-

;this line deletes data so we start the crib fresh
store_data,'*',/delete

;first we set a time and load some data.
timespan,'2008-03-23'

;loading FGM line data in GSM coordinates
thm_load_fgm,probe='a',coord='gsm', level = 'l2'

;loading ESA for spectral data
thm_load_esa,probe='a'

;To plot multiple quantities in separate panels, use an array with the names

tplot,['tha_fgs_gsm','tha_peif_en_eflux']

print,'Plot multiple quantities in separate panels using an array'
print,'Type ".c" to continue'
stop 

;to plot multiple quantities in separate windows use the "window" keyword

window,0
window,1

tplot,'tha_fgs_gsm',window=1
tplot,'tha_peif_en_eflux',window=0

print,'Plot multiple quantities in separate windows using the "window" keyword'
print,'Type ".c" to continue'
stop

;to plot multiple quantities in the same plot use a pseudo-variable

;Use store_data to create a pseudo_variable
store_data,'fgm_pseudo_var',data=['tha_fgl_gsm','tha_fgh_gsm']

;change colors, so we can tell them apart
options,'tha_fgh_gsm',colors=['c','m','y']

;Plot it
tplot,'fgm_pseudo_var'
;zoom in so that you can see the two quantities
tlimit,'2008-03-23/11:00:00','2008-03-23/15:00:00' 

print,'Plot multiple quantities in the same plot using a pseudo-variable, created with "store_data"'
print,'Type ".c" to continue'
stop

;You can put pseudo-variables in multiple panels

;change colors, so we can tell them apart
options,'tha_peif_velocity_gse',colors=['c','m','y']

store_data,'velocity_pvar',data=['tha_peif_velocity_dsl','tha_peif_velocity_gse']

tplot,['fgm_pseudo_var','velocity_pvar']

print,'Plot multiple pseudo variables in separate panels'
print,'Type ".c" to continue'
stop

;Use tplot with no arguments to replot the last plot you plotted with tplot

tplot

print,'Use tplot with no arguments to replot the last plot you plotted with tplot'
print,'Type ".c" to continue'
stop

;To control the range on a pseudo variable, set the range on it
;not its component data
options,'fgm_pseudo_var',yrange=[25,75]

tplot

print,'Control the yrange of pseudo variable, using options on pseudo-var'

stop


;Use the 'xmargin' option to control the size of the left/right plot margin
;Margin size units measured in characters

tplot_options,'xmargin',[20,40] ;This command sets a left margin of 20 characters and a right margin of 40 characters
tplot

print,"Use the 'xmargin' option to control the size of the left/right plot margin"
print,'Type ".c" to continue'
stop

;Use the 'ymargin' option to control the size of the bottom/top plot margin
;Margin size units measured in characters

tplot_options,'ymargin',[5,10] ;This command sets a bottom margin of 5 characters and a top margin of 10 characters
tplot

print,"Use the 'ymargin' option to control the size of the bottom/top plot margin"
print,'Type ".c" to continue'
stop

print,'Resetting margins to something more reasonable'
tplot_options,'xmargin',[15,15]
tplot_options,'ymargin',[5,5]

;Use ygap to control the vertical space between plots
;You can also eliminate the gap by setting ygap=0

tplot_options,'ygap',10
tplot
print,'Change gap between panels using y-gap
print,'Type ".c" to continue'
stop

print,'Crib done, resetting ygap to something more reasonable'
tplot_options,'ygap',1


end
