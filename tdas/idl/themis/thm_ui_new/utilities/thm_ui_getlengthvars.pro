;+
;FUNCTION:
;  thm_ui_getlengthvars
;
;PURPOSE:
;
;  helper function for formatannotation
;   
;Inputs:
;
;Example:
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-04-13 14:51:02 -0700 (Mon, 13 Apr 2009) $
;$LastChangedRevision: 5609 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_getlengthvars.pro $
;-


pro thm_ui_getlengthvars, val, dec, neg

    compile_opt idl2, hidden

  ;extra length for '-' sign 
    neg = val lt 0 
  ;number of digits left of the decimal
    if val eq 0 then begin
      dec = 1
    endif else begin
      dec = floor(alog10(abs(val)) > 0)+1
    endelse
  ;if the size of the exponent is greater than 99, both OS's
    ;will use three digit output
    ;if floor(abs(alog10(abs(val))))+1  gt 99 then os = 1

end