;
;   Written by Larry Kepko (larry.kepko@unh.edu)
;

;
;	This command gets the onboard moments, which are wrong because of an
;	error in uploading the onboard tables

;  Correlation likely depends on temperature, so the same coefficients
;   may not be applicable to the entire day.  In these cases it may be
;   necessary to narrow the interval (intervals of 1/3 day is probably
;   about right).  The variables ii and nn mark the start and stop of
;   the interval calculated, so adjust those to find appropriate time
;   periods.  Adjust the sc and timespan lines to change spacecraft and
;   days. Also, the moment data end up being a bit noisy, so you may
;   want to run 'avg_data', with a small timestep (maybe 10 seconds or so).
;   th?_v?_corr contains the high resolution velocity, and th?_v?_dec
;   contains the data decimated to match the low resolution data, for
;   comparison.

sc = 'c'

timespan,'2007-04-11/00:00', 1, /days

thm_load_mom, probe=sc
thm_load_esa, probe=sc	;	Loads moments calculated on the ground

;
;	ca = low-res ground (moments calculated from distribution functions)
;	ob = time-averaged high-res onboard moments
;	ra = highest resolution onboard moments
;
;	the -x component of 'tha_peim_velocity' is the equivalent of Vx
;
;	Stores the following tplot variables
;		th?_onboard_corr = highest resolution velocity
;		th?_onboard_dec = scaled hi-res, decimated to low-res data
;

;	tha  start index = 290
;		 end index = n_elements(ca.x)-1
;		 tc = indices + 1
;
;		 vx : rac = 0, cac = 0
;		 result = 2.3410312, -57908.268
;		 sigma = 0.95144896
;
;		 vz : rac = 1, cac = 2 (onboard 'vy' = vz)
;		 result = -.065602283, 55777.142
;		 sigma = 0.94713645
;


rac = 0		; 	component for ra data
cac = 0		;	component for ca data

get_data, 'th' + sc + '_peim_velocity', data = ra		;	This is the highest res data
get_data, 'th' + sc + '_peif_velocity_dsl', data = ca	;	This is the low-res ground data

nn = n_elements(ca.x)-1

ii = 0
if sc eq 'e' then ii1 = 150

wioff = 1
tc = whatindices(ra.x, ca.x[ii:nn])+wioff					;	Find the times that high-res = low-res
tc2 = whatindices(ra.x, ca.x)+wioff					;	Find the times that high-res = low-res
result = linfit(ra.y[tc,rac], ca.y[ii:nn,cac])			;	The linear fit


ra.y[*,rac] = result[0] + result[1] * ra.y[*,rac]	;	Scale the high-res data
ra2 = {x:ra.x, y:ra.y[*,rac]}
store_data, 'th' + sc + '_vx_corr', data = ra2		;	Store the high-res data

print, 'Vx fitting from ' + time_string(ra.x[tc[0]]) + $
 			    ' to ' + time_string(ra.x[tc[n_elements(tc)-1]])
print, result
print, correlate(ra.y[tc, rac], ca.y[ii:nn, cac])

ra2 = {x:dblarr(n_elements(tc2),1), y:dblarr(n_elements(tc2),2)}
ra2.x = ra.x[tc2]
ra2.y[*,0] = ra.y[tc2, rac]
ra2.y[*,1] = ca.y[*, cac]
store_data, 'th' + sc + '_vx_dec', data = ra2		;	Store the high-res data


;
;	Calculate Vz
;

rac = 1		; 	component for ra data
cac = 2		;	component for ca data

get_data, 'th' + sc + '_peim_velocity', data = ra		;	This is the highest res data
get_data, 'th' + sc + '_peif_velocity_dsl', data = ca	;	This is the low-res ground data

nn = n_elements(ca.x)-1

ii = 1
if sc eq 'e' then ii1 = 150

wioff = 1
tc = whatindices(ra.x, ca.x[ii:nn])+wioff					;	Find the times that high-res = low-res
tc2 = whatindices(ra.x, ca.x)+wioff					;	Find the times that high-res = low-res
result = linfit(ra.y[tc,rac], ca.y[ii:nn,cac])			;	The linear fit


ra.y[*,rac] = result[0] + result[1] * ra.y[*,rac]	;	Scale the high-res data
ra2 = {x:ra.x, y:ra.y[*,rac]}
store_data, 'th' + sc + '_vz_corr', data = ra2		;	Store the high-res data

print, 'Vz fitting from ' + time_string(ra.x[tc[0]]) + $
 			    ' to ' + time_string(ra.x[tc[n_elements(tc)-1]])
print, result
print, correlate(ra.y[tc, rac], ca.y[ii:nn, cac])

ra2 = {x:dblarr(n_elements(tc2),1), y:dblarr(n_elements(tc2),2)}
ra2.x = ra.x[tc2]
ra2.y[*,0] = ra.y[tc2, rac]
ra2.y[*,1] = ca.y[*, cac]
store_data, 'th' + sc + '_vz_dec', data = ra2		;	Store the high-res data


del_data, '*p??m* *p??f*'


tplot,'th'+sc+'_v?_dec'

end
