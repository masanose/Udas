;+
; PROGRAM: erg_crib_gmag_nipr
;   This is an example crib sheet that will load the magnetometer data 
;   otained by NIPR, Japan. Open this file in a text editor and then 
;   use copy and paste to copy selected lines into an idl window.
;   Or alternatively compile and run using the command:
;     .run erg_crib_gmag_nipr
;
; NOTE: See the rules of the road.
;       For more information, see:
;       http://polaris.nipr.ac.jp/~dbase/e/100/e/100_03_UAPM_at_Syowa_e.htm
;    &  http://polaris.nipr.ac.jp/~dbase/e/100/e/100_15_Iceland_e.htm
; Written by: Y.-M. Tanaka, Feb. 18, 2011
;             National Institute of Polar Research, Japan.
;             ytanaka at nipr.ac.jp
;-

; initialize
thm_init

; set the date and duration (in days)
timespan, '2003-10-29'

; load NIPR data
erg_load_gmag_nipr,site=['syo','hus','tjo']

; view the loaded data names
tplot_names

; plot the loaded data
tplot,'nipr_mag_*'

stop

; set new timespan
timespan,'2003-10-29/06:00:00',4,/hours

; set y-axis
ylim,'nipr_mag_*',-4000,2000

; plot
tplot

end
