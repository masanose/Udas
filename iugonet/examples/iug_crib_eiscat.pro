;+
; PROGRAM: iug_crib_eiscat
;   This is an example crib sheet that will load the EISCAT radar data.
;   Open this file in a text editor and then use copy and paste to copy 
;   selected lines into an idl window. Or alternatively compile and run 
;   using the command: .run erg_crib_gmag_nipr
;
; NOTE: See the rules of the road.
;       For more information, see:
;       http://polaris.nipr.ac.jp/~dbase/e/100/e/100_03_UAPM_at_Syowa_e.htm
;    &  http://polaris.nipr.ac.jp/~dbase/e/100/e/100_15_Iceland_e.htm
; Written by: Y.-M. Tanaka, Mar. 22, 2011
;             National Institute of Polar Research, Japan.
;             ytanaka at nipr.ac.jp
;-

; initialize
thm_init

; set the date and duration (in days)
timespan, '2011-2-1',7

; load EISCAT data
iug_load_eiscat, site='esr_42m'

; view the loaded data names
tplot_names

; plot the electron density
tplot,'eiscat_*_ne'

stop

; set new timespan
timespan,'2011-2-3',1

; plot the loaded data
tplot,[4,6,8,10]

; change zlim
zlim, 'eiscat_esr42m_ipy0_te', 0, 4000 
zlim, 'eiscat_esr42m_ipy0_ti', 0, 3000 
zlim, 'eiscat_esr42m_ipy0_vi', -200, 200

; plot
tplot

stop

; show height profiles & stop by right click
window,1,xsize=256,ysize=256
ctime,/cut

end
