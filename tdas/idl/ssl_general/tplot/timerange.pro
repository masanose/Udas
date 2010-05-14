;+
;FUNCTION:  timerange
;PURPOSE:	To get timespan from tplot_com or by using timespan, if
;		tplot time range not set.
;INPUT:
;	tr (optional)
;KEYWORDS:
;	CURRENT  Set to 1 to get the current time range as set by tlimit.
;RETURNS:
;    two element time range vector.  (double)
;
;SEE ALSO:	"timespan"
;REPLACES:  "get_timespan"
;
;CREATED BY:	Davin Larson
; $LastChangedBy: kenb-mac $
; $LastChangedDate: 2007-06-05 08:30:27 -0700 (Tue, 05 Jun 2007) $
; $LastChangedRevision: 731 $
; $URL: svn+ssh:$
;
;-



function timerange,trange,current=current
@tplot_com.pro
if keyword_set(trange) then return,minmax(time_double(trange))
str_element,tplot_vars,'options.trange_full',trange_full
if n_elements(trange_full) ne 2 then timespan
if tplot_vars.options.trange_full[0] ge tplot_vars.options.trange_full[1] then $
	timespan
t = tplot_vars.options.trange_full
if keyword_set(current) then t = tplot_vars.options.trange
return,t
end

