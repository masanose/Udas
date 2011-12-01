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
; Modified by: Y.-M. Tanaka, December 1, 2011
;-

; Initialize
thm_init

; Set the date and duration (in days)
timespan, '2010-1-18',3

; Load the Tromso UHF radar data
iug_load_eiscat, site='tro_uhf'

; View the loaded data
tplot_names

; Plot Ne for the integration times of 60s and 0s, and all.
tplot,['eiscat_trouhf_beat_0060_ne','eiscat_trouhf_beat_0000_ne',$
       'eiscat_trouhf_beat_all_ne']

; Stop
print,'Enter ".c" to continue.'
stop

; Load data observed with the ipy0 pulse-code by all EISCAT radars
iug_load_eiscat, site='*', pulse_code='ipy0'

; View the loaded data
tplot_names

; Plot the loaded Ne data
tplot, 'eiscat_*_ipy0_all_ne'

; Stop
print,'Enter ".c" to continue.'
stop

; Set title
tplot_options, 'title', 'Sample plot of EISCAT radar data'

; Plot Ne, Te, Ti, Vi for ESR-42m radar
tplot,['eiscat_esr42m_ipy0_all_ne','eiscat_esr42m_ipy0_all_te',$
       'eiscat_esr42m_ipy0_all_ti','eiscat_esr42m_ipy0_all_vi']

end
