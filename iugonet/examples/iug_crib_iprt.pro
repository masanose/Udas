;+
; :DESCRIPTION:
;    A crib sheet to demonstrate how to deal with data from Iitate planetary radio observatory using udas.
;
;    You can run this crib sheet by copying&pasting each command 
;    below into the IDL command line. 
;
; AUTHOR: 
;   Mizuki YONEDA (E-mail: yoneda@pparc.gp.tohoku.ac.jp)
;   
; :HISTORY:
;    2011/11/21: Created
;
;;;;;;;;;;;;;CAUTION;;;;;;;;;;;;;;
;
; To plot data from Iitate planetary radio observatory, libraries for fits 
; files must be installed into your computer (fits_read, sxpar, fits_open, 
; fits_close, gettok, sxdelpar, sxaddpar, and valid_num) in addition to  the 
; udas and tdas.  They are available at http://idlastro.gsfc.nasa.gov/fitsio.html.
;
;-


;Initialize
 
thm_init 

;Specify the time span.
timespan, '2010-11-01',10,/min

;Load the data to plot.
iug_load_iprt
zlim,'iprt_sun_L',20,100
zlim,'iprt_sun_R',20,100
tplot,['iprt_sun_L','iprt_sun_R']


end
