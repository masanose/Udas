;+
; PROCEEDURE: THM_EFI_REMOVE_SPINTONE, name, trange, new_name=new_name, per=per
;
; PURPOSE: GETS RID OF SPIN TONE AND OFFSET ON MEDIUM SIZE STRETCHES
;
; INPUT: 
;
; KEYWORDS: 
;    name -         TPLOT NAME
;
; OUTPUT: TPLOT STORE
;
; INITIAL VERSION: REE 99-03-25 (ff_remove_spintone)
; Space Scienes Lab, UCBerkeley
; MODIFICATION HISTORY: 
; 08-10-31 Modified for THEMIS by REE
; University of Colorado
;
;-
pro thm_efi_remove_spintone, name, trange, new_name=new_name, per=per

IF not keyword_set(per) then BEGIN
  sc = strmid(name,2,1)
  get_data, 'th' + sc + '_state_spinper', data=p
  IF data_type(p) NE 8 THEN BEGIN
    print, 'THM_EFI_REMOVE_SPINTONE: Cannot get spin period.'
    return
  ENDIF
  per = median(p.y)
ENDIF

get_data, name, data=data, dlim=dlim, lim=lim


; CHECK TIMERANGE
npts = n_elements(data.x)
if not keyword_set(trange) then trange = [data.x(0), data.x(npts-1)]
tind = where( (data.x GE trange[0]) AND (data.x LE trange[1]), nind)
IF nind EQ 0 then BEGIN
    print, 'TIME RANGE INVALID'
    return
ENDIF

; ISOLATE DATA QUANTITIES
time = (data.x(tind) - data.x(tind(0)))
ex   = data.y(tind,0)
ey   = data.y(tind,1)
ez   = data.y(tind,2)


; DETERMINE PERIOD USING DATA FROM AXIAL (BIGGEST SIGNAL)
thm_efi_sin_fit, ez, time, es=es, ec=ec, per=per
ezf = ez - es*sin(time*2d*!dpi/per) - ec*cos(time*2d*!dpi/per)
thm_efi_sin_fit, ex, time, es=es, ec=ec, per=per
exf = ex - es*sin(time*2d*!dpi/per) - ec*cos(time*2d*!dpi/per)
thm_efi_sin_fit, ey, time, es=es, ec=ec, per=per
eyf = ey - es*sin(time*2d*!dpi/per) - ec*cos(time*2d*!dpi/per)

; RRETURN TO DATA ARRAY
data.y(tind,0) = exf
data.y(tind,1) = eyf
data.y(tind,2) = ezf

; MAY BE USEFUL SOMEDAY
;thm_qfit, ez, time * !dpi * 2.0 / per, ec=ec, es=es, $
;    /do_sigma, sigma=sigma
;plot, es, yran=[-2.5, 2.5]
;oplot, ec, col=2
;w     = 1.d/sigma
;w     = w*w

if not keyword_set(new_name) then new_name=name
store_data,new_name, data=data, dlim=dlim, lim=lim
print, name,' MODIFIED AND STORED AS ', new_name
return
end
