;+
; PROGRAM: iug_crib_eiscat
;   This is an example crib sheet that will load the EISCAT radar data 
;   distributed in CDF files by NIPR, Japan. Open this file in a text 
;   editor and then use copy and paste to copy selected lines into an 
;   idl window. Or alternatively compile and run using the command:
;     .run iug_crib_eiscat
;
; NOTE: See the rules of the road.
;       For more information, see:
;           http://polaris.nipr.ac.jp/~eiscat/eiscatdata/
; Written by: Y.-M. Tanaka, July 26, 2011
;             National Institute of Polar Research, Japan.
;             ytanaka at nipr.ac.jp
;-

; initialize
thm_init

; set the date and duration (in days)
timespan, '2000-11-27'

; load the ESR-42m data
iug_load_eiscat, site='esr_42m'

; view the loaded data names
tplot_names

; plot the loaded data
tplot,['eiscat_esr42m_tau0_ne','eiscat_esr42m_tau0_te','eiscat_esr42m_tau0_ti','eiscat_esr42m_tau0_vi']

stop

; load all EISCAT radar data
iug_load_eiscat

; plot Ne data from all sites and pulse codes
tplot, 'eiscat_*_ne'

end
