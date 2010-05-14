;+
; NAME: thm_crib_tplot_export_print
; 
; PURPOSE:  Crib to demonstrate tplot export commands
;           You can run this crib by typing:
;           IDL>.compile thm_crib_tplot_export
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
;           thm_crib_tplot_range.pro   (how to control the range and scaling of plots)
;           thm_crib_tplot_annotation.pro  (how to control labels, titles, and colors of plots)
;
; NOTES:
;   If you see any useful commands missing from these cribs, please let us know.
;   these cribs can help double as documentation for tplot.
;
; $LastChangedBy: jimm $
; $LastChangedDate: 2010-04-07 14:48:41 -0700 (Wed, 07 Apr 2010) $
; $LastChangedRevision: 7469 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/examples/tplot_cribs/thm_crib_tplot_export_print.pro $
;-

;this line deletes data so we start the crib fresh
store_data,'*',/delete

;first we set a time and load some data.
timespan,'2008-03-23'

;loading FGM line data in GSM coordinates
thm_load_fgm,probe='a',coord='gsm', level = 'l2'

;loading ESA for spectral data
thm_load_esa,probe='a'

;increasing the xmargin so it is easier to see the labels
tplot_options,'xmargin',[15,15] ;15 characters on each side

;use this command to get the current directory
cd,current=c

;Image export options.

;Export to PNG

tplot,['tha_fgs_gsm','tha_peif_en_eflux']

;makepng will export your most recent plot to a png file
makepng,'example'  ;extension appended automatically

print,'  Just exported "example.png" to : ' + c
print,'Type ".c" to continue crib examples.'
stop

;Export to GIF

tplot,['tha_fgs_gsm','tha_peif_en_eflux']

;makegif will export your most recent plot to a png file
makegif,'example' ;extension appended automatically

print,'  Just exported "example.gif" to : ' + c
print,'Type ".c" to continue crib examples.'
stop

;Export to Postscript(PS)

;First create your plot
tplot,['tha_fgs_gsm','tha_peif_en_eflux']

;Next open a postscript with popen
popen,'example'  ;note /land option will output in landscape mode
tplot; use tplot with no arguments to redraw your plot to the postscript file
pclose ; close the postscript

print,'  Just exported "example.ps" to : ' + c
print,'Type ".c" to continue crib examples.'
stop

;Export to Encapsulated Postscript(EPS)

;First create your plot
tplot,['tha_fgs_gsm','tha_peif_en_eflux']

;Next open a postscript with popen,/encapsulated
popen,'example',/encapsulated  ;note /land option will output in landscape mode
tplot; use tplot with no arguments to redraw your plot to the postscript file
pclose ; close the postscript

print,'  Just exported "example.eps" to : ' + c
print,'Type ".c" to continue crib examples.'
stop

;Data export options

;Export to ASCII

tplot_ascii,'tha_fgs_gsm'

print,'  Just exported "tha_fgs_gsm" to "tha_fgs_gsm.txt" in directory : ' + c
print,'Type ".c" to continue crib examples.'
stop

;Export Line to IDL array

get_data,'tha_fgs_gsm',data=d

times = d.x
data = d.y
data_x = d.y[*,0]
data_y = d.y[*,1]
data_z = d.y[*,2]

print,'  Just exported "tha_fgs_gsm" to IDL arrays'
print,'Type ".c" to continue crib examples.'
stop

;Export Spectra to IDL array

get_data,'tha_peif_en_eflux',data=d

times = d.x ;x-position(sample time) of each point in plane
zdata = d.y ;height at each point in plane
ydata = d.v ;y-position(energy in this case) of each point in the plane.  If this component is 2-d then there is a different set of y-positions at each point in time.  Otherwise, y-scaling is constant across time.

print,' Just exported "tha_peif_en_eflux" to IDL arrays'
print,'Crib is done!


end
