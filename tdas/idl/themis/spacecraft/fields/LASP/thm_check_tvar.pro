;+
; HEAD:
;     function thm_check_tvar, tvar
; 
; PURPOSE:
;     This routine check whether the given tplot variable TVAR exists in the
;     memory. If not, return 0. Otherwise, then check whether it contains data of
;     the current date. If yes, return 1. If not, return 0.
;
; ARGUMENTS:
;     tvar: The tplot variable to be checked.
; 
; SEE ALSO:
; 
; HISTORY:
;     2009-05-10: written by Jianbao Tao, in CU/LASP.
;
;-

function thm_check_tvar, tvar

; check data type of tvar
if size(tvar,/type) ne 7 then begin
   print, 'THM_CHECK_TVAR: ' + $
      'Argument TVAR must be a string of the name of a tplot varialbe.'
   return, 0
endif

tvar = strlowcase(tvar)

; get current date
get_timespan, t
tmp = (time_string(t))[0]
cur_date = strmid(tmp,0,10)

if not strcmp(tvar, tnames(tvar)) then begin
   return, 0
endif else begin
   get_data, tvar, data=data
   tmp = time_string(data.x[0])
   date = strmid(tmp,0,10)
   if strcmp(cur_date,date) then return, 1 else return, 0
endelse

end


