;+
; PROCEDURE: IUG_CRIB_GMAG_NIPR
;    A sample crib sheet that explains how to use the "iug_load_gmag_nipr" 
;    procedure. You can run this crib sheet by copying & pasting each 
;    command below (except for stop and end) into the IDL command line. 
;    Or alternatively compile and run using the command:
;        .run iug_crib_gmag_nipr
;
; NOTE: See the rules of the road.
;       For more information, see:
;       http://polaris.nipr.ac.jp/~dbase/e/100/e/100_03_UAPM_at_Syowa_e.htm
;    &  http://polaris.nipr.ac.jp/~dbase/e/100/e/100_15_Iceland_e.htm
; Written by: Y.-M. Tanaka, May 2, 2011
;             National Institute of Polar Research, Japan.
;             ytanaka at nipr.ac.jp
;-

; Initialize
thm_init

; Set the date and duration (in days)
timespan, '2003-10-29'

; Load NIPR data
erg_load_gmag_nipr,site=['syo','hus','tjo']

; View the loaded data names
tplot_names

; Plot the loaded data
tplot,'nipr_mag_*'

; Stop
print,'Enter ".c" to continue.'
stop

; Set new timespan
timespan,'2003-10-29/06:00:00',4,/hours

; Set y-axis
ylim,'nipr_mag_*',-4000,2000

; Set title
; tplot_options, 'title', 'Sample plot of NIPR magnetometer data'

; Plot
tplot

end
