; PROGRAM: iug_crib_gmag_serc
;   This is an example crib sheet that will load the MAGDAS magnetometer data
;   released by Space Environment Research Center(SERC), Kyushu University, Japan. 
;   Open this file in a text editor and then use copy and paste to copy
;   selected lines into an idl window. Or alternatively compile and run
;   using the command:
;     .RUN IUG_CRIB_GMAG_SERC
;-
;
; NOTE: For more information about MAGDAS and its rules of the road, see:
;       http://magdas.serc.kyushu-u.ac.jp/
; Written by: Shuji Abe,  May 01, 2011
;             Space Environment Research Center, Kyushu University, Japan 
;             abeshu _at_ serc.kyushu-u.ac.jp
;
;
; initialize
thm_init

; set the date and duration (in days)
timespan, '2008-03-28'

; load MAGDAS data
iug_load_gmag_serc,site=['ONW','CMD','ANC']

; view the loaded data names
tplot_names
stop

; plot the loaded data
tplot,'serc_mag_*'
stop

; set new timespan
timespan,'2008-03-28/02:00:00',4,/hours

; set y-axis
ylim,'serc_mag_*',-4000,2000

; replot
tplot

end
