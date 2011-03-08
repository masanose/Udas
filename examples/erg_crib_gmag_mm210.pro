;+
; PROGRAM: erg_crib_gmag_mm210
;   This is an example crib sheet that will load 210 MM magnetometer data.
;   Open this file in a text editor and then use copy and paste to copy
;   selected lines into an idl window.
;   Or alternatively compile and run using the command:
;     .run erg_crib_gmag_mm210
;
; NOTE: See the rules of the road.
;       For more information, see http://stdb2.stelab.nagoya-u.ac.jp/mm210/
;
; Written by: Y. Miyashita, Jun 16, 2010
;             ERG-Science Center, STEL, Nagoya Univ.
;             erg-sc-core at st4a.stelab.nagoya-u.ac.jp
;
;   $LastChangedBy: miyasita $
;   $LastChangedDate: 2010-11-29 17:42:55 +0900 (Mon, 29 Nov 2010) $
;   $LastChangedRevision: 67 $
;   $URL: http://gemsissc.stelab.nagoya-u.ac.jp/svn/ergsc/trunk/erg/examples/erg_crib_gmag_mm210.pro $
;-

; initialize
thm_init

; set the date and duration (in days)
timespan, '2006-11-20'

; load the data
erg_load_gmag_mm210, site='msr rik kag ktb'

; view the loaded data names
tplot_names

; plot the H, D, and Z components
tplot, ['mm210_mag_*_1min_hdz']

end
