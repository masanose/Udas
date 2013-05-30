;+
;PROCEDURE: IUG_CRIB_METEOR_NIPR.PRO
;    A sample crib sheet that explains how to use the "iug_crib_meteor_nipr.pro" 
;    procedure. You can run this crib sheet by copying & pasting each 
;    command below (except for stop and end) into the IDL command line. 
;    Or alternatively compile and run using the command:
;        .run iug_crib_meteor_nipr
;
;Written by: Y.-M. Tanaka, Aug. 2, 2012
;-

; Initializes system variables for themis:
thm_init

; Specify timespan:
timespan, '2009-1-14',10 

; Load data
iug_load_eiscat_vecvel

; Plot all velocities
tplot, 'eiscat_trouhf_vecvel_mono_V?'

stop


; Line Plot
options, 'eiscat_trouhf_vecvel_mono_V?', 'spec', 0

tvar='eiscat_trouhf_vecvel_mono_Ve'
options, tvar, ytitle='Eastward Vel.', ysubtitle = '[m/s]'
ylim, tvar, -600, 600, 0

tvar='eiscat_trouhf_vecvel_mono_Vn'
options, tvar, ytitle='Northward Vel.', ysubtitle = '[m/s]'
ylim, tvar, -600, 600, 0

tvar='eiscat_trouhf_vecvel_mono_Vu'
options, tvar, ytitle='Upward Vel.', ysubtitle = '[m/s]'
ylim, tvar, -600, 600, 0

tlimit, '2009-1-19/16:00', '2009-1-20/6:00'
tplot


end
