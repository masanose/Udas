;+
; PROCEDURE: IUG_CRIB_EISCAT
;    A sample crib sheet that explains how to use the "iug_load_eiscat" 
;    procedure. You can run this crib sheet by copying & pasting each 
;    command below (except for stop and end) into the IDL command line. 
;    Or alternatively compile and run using the command:
;        .run iug_crib_eiscat
;
; NOTE: See the rules of the road.
;       For more information, see:
;           http://polaris.nipr.ac.jp/~eiscat/eiscatdata/
; Written by: Y.-M. Tanaka, July 26, 2011
;             National Institute of Polar Research, Japan.
;             ytanaka at nipr.ac.jp
; Modified by: Y.-M. Tanaka, December 1, 2011
;-

; Initialize
thm_init

; Set the date and duration (in days)
timespan, '2011-2-3'

; Load the Tromso UHF radar data
iug_load_eiscat, site='tro_uhf'

; View the loaded data
tplot_names

; Plot Ne, Te, Ti, Vi for Tromso-UHF radar
tplot,['eiscat_trouhf_all_ne','eiscat_trouhf_all_te',$
       'eiscat_trouhf_all_ti','eiscat_trouhf_all_vi']


; Stop
print,'Enter ".c" to continue.'
stop

; Load data observed by ESR-32m radar
iug_load_eiscat, site='esr_32m'

; Load data observed by ESR-42m radar
iug_load_eiscat, site='esr_42m'


; View the loaded data
tplot_names

; Set title
; tplot_options, 'title', 'Sample plot of EISCAT radar data'

; Plot Ne for all stations
tplot, 'eiscat_*_all_ne'


end
