;+
;HEAD:
;    function thm_jbt_get_btrange, tplotname, nbursts=nbursts, tind=tind
;
;PURPOSE:
;    This routine take a efp or efw tplot variable, and find the beginning and
;    the ending moments of each continuous burst.
;    If the routine exit unsuccessfully, it will return -1. Otherwise, it will
;    return a 2D array as [2,number_of_total_bursts] which stores the
;    time-ranges of each continuous burst.
;
;    "IF YOU DON'T LIKE WHAT YOU GET, DO IT YOURSEFT."
;                                     --REE
;
;ARGUMENTS AND KEYWORDS:
;    tplotname: NEEDED. The name of the input tplot variable.
;    nbursts: OPTIONAL. A named variable to return the number of bursts.
;    tind: OPTIONAL. A named variable to return a 2D array of the index of
;          starting and ending time points with structure [[starting],[ending]]
;
;HISTORY:
;    2009-05-04, written by Jianbao Tao, in CU/LASP.
;
;-

function thm_jbt_get_btrange, tplotname, nbursts=nbursts, tind=tind

;check if tplotname valid
con1 = n_elements(tplotname) eq 0
con2 = size(tplotname, /type) ne 7
con = con1 + con2
if con gt 0 then begin
  print, 'THM_EFI_GET_BTRANGE: ' + $
         'A valid efp or efw tplot variable name must be provided. Returning...'
  return, -1
endif

get_data, tplotname, data=data
if size(data,/type) ne 8 then begin
  print, 'THM_EFI_GET_BTRANGE: ' + $
         'The given tplot name does not contain valid data. Returning...'
  return, -1
endif

;set default gap = 1 second
if n_elements(gap) eq 0 then gap = 1.d

tarr = data.x
nt = n_elements(tarr)

dtarr = tarr[1:nt-1] - tarr[0:nt-2]
dt = median(dtarr)
gap = 2.0 * dt
bind = where(dtarr gt gap, nbursts)
nbursts = (nbursts+1)
IF nbursts GT 1 then BEGIN
  bstart = [0, bind+1]                  ; STARTING INDEX
  bend   = [bind, n_elements(dtarr)]       ; ENDING INDEX
ENDIF ELSE BEGIN
  bstart = [0]
  bend   = [n_elements(dtarr)]
ENDELSE

btrange = dblarr(2,nbursts)
btrange[0,*] = tarr[bstart]
btrange[1,*] = tarr[bend]

tind = [[bstart],[bend]]

return, btrange

end
