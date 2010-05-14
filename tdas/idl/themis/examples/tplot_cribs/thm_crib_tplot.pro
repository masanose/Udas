;+
; NAME: thm_crib_tplot
; 
; PURPOSE:  Updated crib to demonstrate tplot basics. 
;           You can run this crib by typing:
;           IDL>.compile thm_crib_tplot
;           IDL>.go
;           
;           When you reach a stop, press
;           IDL>.c
;           to continue
;           
;           Or you can copy and paste commands directly onto the command line
;           
;           
;
;
; SEE ALSO: thm_crib_tplot_layout.pro  (how to arrange plots within a window, and data within a plot)
;           thm_crib_tplot_range.pro   (how to control the range and scaling of plots)
;           thm_crib_tplot_annotation.pro  (how to control labels, titles, and colors of plots)
;           thm_crib_tplot_export_print.pro (how to export images of plots into pngs and postscripts)
;
; NOTES:
;   If you see any useful commands missing from these cribs, please let us know.
;   these cribs can help double as documentation for tplot.
;
; $LastChangedBy: jimm $
; $LastChangedDate: 2010-04-07 14:48:41 -0700 (Wed, 07 Apr 2010) $
; $LastChangedRevision: 7469 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/examples/tplot_cribs/thm_crib_tplot.pro $
;-

;this line deletes data so we start the crib fresh
store_data,'*',/delete

;first we set a time and load some data.
timespan,'2008-03-23'

;loading FGM line data in GSM coordinates
thm_load_fgm,probe='a',coord='gsm', level = 'l2'

;loading ESA for spectral data
thm_load_esa,probe='a'

;Use 'tplot_names' to see the names of the loaded quantities and their indexes
tplot_names

print,"Use 'tplot_names' to see what quantities are loaded"
print,'Type ".c" to continue'
stop

;use 'tplot' to plot some fgm line data
tplot,'tha_fgs_gsm'


print,"Use 'tplot,name' plot line data."
print,'Type ".c" to continue'
stop

;use tplot to plot spectral data
tplot,'tha_peif_en_eflux'

print,"Or Use 'tplot,name' to plot spectal data."
print,'Type ".c" to continue'
stop

;you can also plot quantities by number
tplot,2

print,"Use 'tplot,number' to select a quantity to plot by number."
print,'Type ".c" to continue'
stop

;you can plot multiple quantities in the same window by grouping with brackets.
tplot,['tha_fgs_gsm','tha_peif_en_eflux']

print,"Use 'tplot,[name,name,...]' to put multiple plots in the same window."
print,'Type ".c" to continue'
stop

;you can also select multiple quantities using "globbing" characters('*' & '?')
;Use '?' to select any tplot names that match all characters except
;a single character at '?'
;Use '*' to match multiple characters at the '*'

;this example matches tha_fgs_gsm,tha_fge_gsm,tha_fgh_gsm,tha_fgl_gsm with '?'
tplot,'tha_fg?_gsm'
print,'Use "?" to match multiple quantities with a single character'
print,'Type ".c" to continue'
stop

;this example matches tha_fgs_gsm, tha_fgs_btotal
tplot,'tha_fgs_*'
print,'Use "*" to match multiple quantities with multiple characters'
print,'Type ".c" to continue'
stop

;another common use of globbing is to match multiple probes with a single command
;for example, 'th?_fgs_gsm',
;
;you can also combine globbing with explicit names,
;for example

tplot,['tha_fgs_*','tha_peif_en_eflux']
print,'Example combining globbing and explicit names'
print,'Type ".c" to continue'
stop

;You can zoom in on a specific time range interactively, using tlimit
print,"Use 'tlimit' to select a specific range interactively"
print,"Click two locations on the screen to zoom in"
print,'Type ".c" to continue'

tplot,['tha_fgs_gsm','tha_peif_en_eflux']
tlimit

stop

;you can zoom in without clicking using this command
tlimit,'2008-03-23/12:00:00','2008-03-23/20:00:00'

print,"Use 'tlimit,starttime,stoptime' to select a specific range without clicking"
print,'Type ".c" to continue'
stop

;you can use this command to get the array data that is stored in a tplot variable
;as well as the plot settings associated with that tplot variable

get_data,'tha_peif_en_eflux',data=d
;'d' contains the array data from the tplot variable
print,'contents of d:'
help,d,/str
print,'d.x:'
help,d.x ;d.x is the time array for the tplot variable
print,'d.y:'
help,d.y ;d.y is the data array for the tplot variable
print,'d.v:'
help,d.v ;d.v is the y-axis scaling data for the tplot variable(not all quantities have this component

print,"Use 'get_data,name,data=d' to get the array data from a tplot variable"
print,'Type ".c" to continue'
stop

get_data,'tha_peif_en_eflux',dlimit=dl

print,'contents of dl:'
help,/str,dl ;This struct contains default plot settings like color and labels
print,'contents of dl.cdf:'
help,/str,dl.cdf ;This component contains the CDF meta data that was read on ingestion
print,'contents of dl.data_att:' 
help,/str,dl.data_att ; This component contains commonly used meta data like units, coordinate system, and calibration level 

print,"Use 'get_data,name,dlimit=dl' to get the default plot settings from a tplot variable"
print,'Type ".c" to continue'
stop 

get_data,'tha_peif_en_eflux',limit=l

print,'limits:'
help,/str,l ;This struct contains user defined plot settings which override settings, if unset it will be a 0 

print,"Use 'get_data,name,limit=l' to get user settings from a tplot variable"
print,'Type ".c" to continue'
stop

;you can use store_data to replace the data,limits,& dlimits after modifying them

get_data,'tha_fgs_gsm',data=d,limit=l,dlimit=dl

d.y = d.y*10  ;modify data

store_data,'tha_fgs_gsm_new',data=d,limit=l,dlimit=dl ;store it in a new variable

print,"you can use 'store_data' to replace the data,limits,& dlimits after modifying them"
;replot it
tplot,['tha_fgs_gsm','tha_fgs_gsm_new']

end
