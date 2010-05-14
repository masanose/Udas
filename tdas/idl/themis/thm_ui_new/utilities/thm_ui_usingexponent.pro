;+
;FUNCTION:
;  thm_ui_usingexponent
;
;PURPOSE:
;
;  Used to determine if any of a set of inputs will be put into exponential notation
;   
;Inputs:
;
;Example:
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-08-06 14:15:28 -0700 (Thu, 06 Aug 2009) $
;$LastChangedRevision: 6537 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_usingexponent.pro $
;-

pro thm_ui_usingexponent,val,data,type=type,expsign=expsign

  compile_opt idl2,hidden

  ;lengthLimit = 15
  lengthLimit=17
  neg=0
  dec=0
  precision = data.formatid-1 > 0  ;desired decimal precision

  if ~in_set('exponent',strlowcase(tag_names(data))) then $
    data = create_struct(data,'exponent',0b)

;Auto-Format
  if data.exponent eq 0 then begin

    thm_ui_getlengthvars, val, dec, neg
    check_dround, val, neg, dec, precision

    ;Check large values against precision
    if abs(val) ge 1 or val eq 0 then begin
      if dec gt (precision+1) then begin
        if data.scaling eq 1 then type = 3 else begin
          type = 1
          expsign = 1
        endelse
      endif else type = 0
    ;Check small values against precision
    endif else if val ne 0 then begin
      if ceil(abs(alog10(abs(val)))) gt (precision+1) then begin
        if data.scaling eq 1 then type = 3 else begin
          type = 1
          expsign = -1
        endelse
      endif else type = 0
    endif else begin
      print, 'Uncertain auto-format'
      type = -1
      return
    endelse
    
    ;Handle log scaling and integer type
    if data.scaling eq 2 then type = 2
;    if data.scaling eq 1 then type = 3
    if data.formatid eq 0 then type = 0
;Fix to double format
  endif else if data.exponent eq 1 then begin
    type = 0
;Fix to exponential format
  endif else if data.exponent eq 2 then begin
    type = 1
    if abs(val) ge 1 then begin
      expsign = 1
    endif else if val eq 0 then expsign = 0 $ 
      else expsign = -1
  endif else if data.exponent eq 3 then type = 3
  
 ;If double format will exceed length limit then switch to exponential
  if type eq 0 then begin
    if ((neg+dec) gt lengthLimit and data.formatid eq 0) $
      or ((neg+1+(precision+1)) gt lengthLimit) $
        then begin 
          type = 1
          if abs(val) ge 1 then begin
            expsign = 1
          endif else if val eq 0 then expsign = 0 $ 
            else expsign = -1
    endif
  endif 

end