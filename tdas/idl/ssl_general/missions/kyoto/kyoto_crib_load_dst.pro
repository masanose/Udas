
;+
;
;Crib sheet for KYOTO_LOAD_DST.PRO.  Modified from KYOTO_CRIB_LOAD_AE.PRO.
;
;Modifications:
;
;WMFeuerstein, 8/21/2008.
;
;-

print,'Enter ".c" to set the timespan to a date known to contain Kyoto DST data'
print,'(2007-03-23) (if this step is skipped, the user will be queried for the date).
stop
timespan,'7-3-23',2

print,'Enter ".c" to call KYOTO_LOAD_DST 
stop
kyoto_load_dst

print,'Enter ".c" to plot DST data.'
stop
tplot,'kyoto_dst'


end

